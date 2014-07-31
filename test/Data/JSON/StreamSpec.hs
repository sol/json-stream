{-# LANGUAGE OverloadedStrings #-}
module Data.JSON.StreamSpec (main, spec) where

import           Test.Hspec

import           Data.JSON.Stream
import           Data.Aeson (Value(..), toJSON, object, (.=))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseValue" $ do
    it "parses true" $ do
      parseValue "true" `shouldBe` Just (toJSON True, "")

    it "parses false" $ do
      parseValue "false" `shouldBe` Just (toJSON False, "")

    it "parses null" $ do
      parseValue "null" `shouldBe` Just (Null, "")

    it "parses number" $ do
      parseValue "23" `shouldBe` Just (Number 23, "")

    it "parses strings" $ do
      parseValue "\"foo\"" `shouldBe` Just ("foo", "")

    it "parses arrays" $ do
      parseValue "[true,false,null]" `shouldBe` Just (toJSON [toJSON True, toJSON False, Null], "")

    it "parses objects" $ do
      parseValue "{\"foo\":23,\"bar\":42}" `shouldBe` Just (object ["foo" .= Number 23, "bar" .= Number 42], "")

    context "when given invalid input" $ do
      it "returns an error" $ do
        parseValue "baz" `shouldBe` Nothing
