{-# LANGUAGE OverloadedStrings #-}
module Data.JSON.Stream
  ( parseValue
  , decode
  , decodeValue
  , runParser
  , result
  , Result(..)
  ) where

import           Control.Applicative
import           Numeric (readHex)
import           Data.Word (Word8)
import qualified Data.Char as Char
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8')
import           Data.Aeson (Value(..), toJSON, object, (.=))
import           Data.Aeson.Types (Pair)

data Result a = OK !a ByteString
              | Failed
  deriving (Eq, Show)

result :: b -> (a -> ByteString -> b) -> Result a -> b
result def _ Failed   = def
result _   f (OK x s) = f x s

newtype Parser a = Parser
  { runParser :: ByteString -> Result a }

failP :: Parser a
failP = Parser $ \_ -> Failed

decodeValue :: ByteString -> Maybe Value
decodeValue = decode parseValue

decode :: Parser a -> ByteString -> Maybe a
decode p = result Nothing (\x _ -> Just x)
         . runParser p

instance Functor Parser where
  fmap f p = Parser $ \str ->
    case runParser p str of
      Failed    -> Failed
      OK a str' -> OK (f a) str'

instance Applicative Parser where
  pure x = Parser $ OK x

  f <*> x = Parser $ \str ->
    case runParser f str of -- we run the parser for f
      Failed    -> Failed
      OK g str' ->
        case runParser x str' of -- we run the parser for x
          Failed     -> Failed
          OK a str'' -> OK (g a) str'' -- and apply the function

instance Monad Parser where
  return = pure

  x >>= f = Parser $ \str ->
    case runParser x str of
      Failed    -> Failed
      OK a str' -> runParser (f a) str'

  fail _ = failP

nextWord :: Parser Word8
nextWord = Parser $ \str ->
  case B.uncons str of
    Nothing        -> Failed
    Just (c, str') -> OK c str'

notRelevant :: Word8 -> Bool
notRelevant w
  | w == ord ' '  = True
  | w == ord '\t' = True
  | w == ord '\n' = True
  | otherwise     = False

nextRelevantWord :: Parser Word8
nextRelevantWord = Parser $ \str ->
  case B.uncons $ B.dropWhile notRelevant str of
    Nothing        -> Failed
    Just (w, str') -> OK w str'

splitAtP :: Int -> Parser ByteString
splitAtP n = Parser $ \str ->
  case B.splitAt n str of
    (s, str') -> OK s str'

breakP :: (Word8 -> Bool) -> Parser ByteString
breakP p = Parser $ \input ->
  case B.break p input of
    (str, rest) -> OK str rest

spanP :: (Word8 -> Bool) -> Parser ByteString
spanP p = Parser $ \input ->
  case B.span p input of
    (str, rest) -> OK str rest

parseValue :: Parser Value
parseValue = Parser $ \str ->
  case runParser nextRelevantWord str of
    OK x str'
        | x == ord '{' -> runParser parseObject str'
        | x == ord '[' -> runParser parseArray str'
        | x == ord '"' -> runParser parseString str'
        | x == ord 't' -> runParser parseTrue str'
        | x == ord 'f' -> runParser parseFalse str'
        | x == ord 'n' -> runParser parseNull str'
        -- here we run the parser on the original bs we got
        -- because we don't want to miss the first digit
        | otherwise    -> runParser parseNumber (B.cons x str')
    Failed -> Failed

parseNull :: Parser Value
parseNull = do
  s <- splitAtP 3
  case s of
    "ull" -> return Null
    _     -> failP

parseTrue :: Parser Value
parseTrue = do
  s <- splitAtP 3
  case s of
    "rue" -> return (Bool True)
    _     -> failP

parseFalse :: Parser Value
parseFalse = do
  s <- splitAtP 4
  case s of
    "alse" -> return (Bool False)
    _      -> failP

parseString :: Parser Value
parseString = fmap String parseStringLit

parseStringLit :: Parser Text
parseStringLit = parseStringLit_ >>= convertToText
  where
    convertToText :: [ByteString] -> Parser Text
    convertToText =
      -- if bytestring -> utf8 fails, the parser fails
        either (const failP) return
      . decodeUtf8'
      . B.concat

parseStringLit_ :: Parser [ByteString]
parseStringLit_ = go
  where
    go :: Parser [ByteString]
    go = do
      xs <- breakP (\w -> w == ord '"' || w == ord '\\')
      z  <- nextWord
      case z of
        w | w == ord '"'  -> return [xs]
          | w == ord '\\' -> xs <:> unescape

    unescape :: Parser [ByteString]
    unescape = do
      x <- nextWord
      case x of
        w | w == ord 'u' -> unescapeUnicode
          | w == ord 'b' -> "\b" <:> go
          | w == ord 'f' -> "\f" <:> go
          | w == ord 'n' -> "\n" <:> go
          | w == ord 'r' -> "\r" <:> go
          | w == ord 't' -> "\t" <:> go
          | otherwise    -> B.singleton x <:> go

    unescapeUnicode :: Parser [ByteString]
    unescapeUnicode = do
      -- FIXME: Make tihs more efficient
      xs <- splitAtP 4
      case readHex (B8.unpack xs) of
        [(n, "")] -> (encodeUtf8 $ T.pack [Char.chr n]) <:> go
        _         -> failP

-- FIXME: This is not correct yet..
parseNumber :: Parser Value
parseNumber = do
  xs <- spanP $ \x -> ord '0' <= x && x <= ord '9'
  case B.null xs of
    True  -> failP
    False -> return . Number . read $ B8.unpack xs

-- maybe there's a more efficient way to go than toJSON,
-- for the [Value] -> Value phase
-- ?
parseArray :: Parser Value
parseArray = fmap toJSON go
  where
    go :: Parser [Value]
    go = do
      value <- parseValue
      x <- nextRelevantWord
      case x of
        w | w == ord ']' -> return [value]
          | w == ord ',' -> fmap (value:) go
          | otherwise    -> failP

parseObject :: Parser Value
parseObject = fmap object go
  where
    go :: Parser [Pair]
    go = do
      name <- parseName
      x <- nextRelevantWord
      case x of
        w | w == ord ':' -> do
              value <- parseValue
              z <- nextRelevantWord
              case z of
                z' | z' == ord ',' -> (name .= value) <:> go
                   | z' == ord '}' -> return [name .= value]
                   | otherwise     -> failP

          | otherwise -> failP

    parseName :: Parser Text
    parseName = do
      x <- nextRelevantWord
      if x == ord '"'
        then parseStringLit
        else failP

ord :: Char -> Word8
ord = fromIntegral . Char.ord

-- b <:> p = fmap (b:) p
(<:>) :: a -> Parser [a] -> Parser [a]
b <:> p = fmap (b:) p
