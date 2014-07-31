{-# LANGUAGE OverloadedStrings #-}
module Data.JSON.Stream (parseValue) where

import           Control.Applicative
import qualified Data.Char as Char
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8')
import           Data.Aeson (Value(..), toJSON, object, (.=))
import           Data.Aeson.Types (Pair)

type Parser = ByteString -> Maybe (Value, ByteString)

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

parseNull :: Parser
parseNull input = case B.splitAt 3 input of
  ("ull", rest) -> Just (Null, rest)
  _ -> Nothing

parseTrue :: Parser
parseTrue input = case B.splitAt 3 input of
  ("rue", rest) -> Just (Bool True, rest)
  _ -> Nothing

parseFalse :: Parser
parseFalse input = case B.splitAt 4 input of
  ("alse", rest) -> Just (Bool False, rest)
  _ -> Nothing

parseString :: Parser
parseString = fmap (mapFst String) . parseStringLit

-- FIXME: This is not correct yet..
parseStringLit :: ByteString -> Maybe (Text, ByteString)
parseStringLit input = case B.break (== ord '"') input of
  (s, rest) -> do
    s_ <- either (const Nothing) Just (decodeUtf8' s)
    return (s_, B.drop 1 rest)

-- FIXME: This is not correct yet..
parseNumber :: Parser
parseNumber input = case B.span (\x -> ord '0' <= x && x <= ord '9') input of
  (xs, rest) | (not . B.null) xs -> Just ((Number . read . B8.unpack) xs, rest)
  _ -> Nothing

parseArray :: Parser
parseArray = fmap (mapFst toJSON) . go
  where
    go :: ByteString -> Maybe ([Value], ByteString)
    go input = do
      (xs, rest) <- parseValue input
      case B.uncons rest of
        Just (y, ys) -> do
          let cont
                | y == ord ']' = return ([xs], ys)
                | y == ord ',' = do
                    (zs, rest_) <- go ys
                    return (xs : zs, rest_)
                | otherwise = Nothing
          cont
        Nothing -> Nothing

parseObject :: Parser
parseObject = fmap (mapFst object) . go
  where
    go :: ByteString -> Maybe ([Pair], ByteString)
    go input = do
      (name, rest) <- parseName input
      (x, xs) <- B.uncons rest
      let cont
            | x == ord ':' = do
                (value, r) <- parseValue xs
                (z, zs) <- B.uncons r
                let cont_
                      | z == ord ',' = mapFst ((name .= value) :) <$> go zs
                      | z == ord '}' = return ([name .= value], zs)
                      | otherwise = Nothing
                cont_
            | otherwise = Nothing
      cont

    parseName :: ByteString -> Maybe (Text, ByteString)
    parseName input = do
      (x, xs) <- B.uncons input
      let cont
            | x == ord '"' = parseStringLit xs
            | otherwise = Nothing
      cont

ord :: Integral a => Char -> a
ord = fromIntegral . Char.ord

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)
