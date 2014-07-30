{-# LANGUAGE OverloadedStrings #-}
module Data.JSON.Stream where

import           Control.Applicative
import qualified Data.Char as Char
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

data Event
  = ObjectBegin
  | ObjectEnd
  | ArrayBegin
  | ArrayEnd
  | Name ByteString
  | ValueString ByteString
  | ValueNumber ByteString
  | ValueNull
  | ValueTrue
  | ValueFalse
  deriving (Show, Eq)

parseValue :: Parser
parseValue = go
  where
    go input = do
      (x, xs) <- B.uncons input
      let cont
            | x == ord '{' = parseObject xs
            | x == ord '[' = parseArray xs
            | x == ord '"' = parseString xs
            | x == ord 't' = parseTrue xs
            | x == ord 'f' = parseFalse xs
            | x == ord 'n' = parseNull xs
            | otherwise = parseNumber input
      cont

type Parser = ByteString -> Maybe ([Event], ByteString)
type ValueParser = ByteString -> Maybe (Event, ByteString)

parseNull :: Parser
parseNull input = case B.splitAt 3 input of
  ("ull", rest) -> Just ([ValueNull], rest)
  _ -> Nothing

parseTrue :: Parser
parseTrue input = case B.splitAt 3 input of
  ("rue", rest) -> Just ([ValueTrue], rest)
  _ -> Nothing

parseFalse :: Parser
parseFalse input = case B.splitAt 4 input of
  ("alse", rest) -> Just ([ValueFalse], rest)
  _ -> Nothing

parseString :: Parser
parseString = fmap (mapFst (return . ValueString)) . parseStringLit

-- FIXME: This is not correct yet..
parseStringLit :: ByteString -> Maybe (ByteString, ByteString)
parseStringLit input = case B.break (== ord '"') input of
  (s, rest) -> Just (s, B.drop 1 rest)

-- FIXME: This is not correct yet..
parseNumber :: Parser
parseNumber input = case B.span (\x -> ord '0' <= x && x <= ord '9') input of
  (xs, rest) | (not . B.null) xs -> Just ([ValueNumber xs], rest)
  _ -> Nothing

parseArray :: Parser
parseArray = fmap (mapFst (ArrayBegin :)) . go
  where
    go input = do
      (xs, rest) <- parseValue input
      case B.uncons rest of
        Just (y, ys) -> do
          let cont
                | y == ord ']' = return (xs ++ [ArrayEnd], ys)
                | y == ord ',' = do
                    (zs, rest_) <- go ys
                    return (xs ++ zs, rest_)
                | otherwise = Nothing
          cont
        Nothing -> Nothing

parseObject :: Parser
parseObject = fmap (mapFst (ObjectBegin :)) . go
  where
    go input = do
      (name, rest) <- parseName input
      (x, xs) <- B.uncons rest
      let cont
            | x == ord ':' = do
                (value, r) <- parseValue xs
                (z, zs) <- B.uncons r
                let cont_
                      | z == ord ',' = mapFst ((name : value) ++) <$> go zs
                      | z == ord '}' = return (name : value ++ [ObjectEnd], zs)
                      | otherwise = Nothing
                cont_
            | otherwise = Nothing
      cont

    parseName :: ValueParser
    parseName input = do
      (x, xs) <- B.uncons input
      let cont
            | x == ord '"' = mapFst Name <$> parseStringLit xs
            | otherwise = Nothing
      cont

ord :: Integral a => Char -> a
ord = fromIntegral . Char.ord

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)
