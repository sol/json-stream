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

    it "parses arrays" $ do
      parseValue "[true,false,null]" `shouldBe` Just (toJSON [toJSON True, toJSON False, Null], "")

    it "parses objects" $ do
      parseValue "{\"foo\":23,\"bar\":42}" `shouldBe` Just (object ["foo" .= Number 23, "bar" .= Number 42], "")

    context "when parsing strings" $ do
      it "parses simple strings" $ do
        parseValue "\"foo\"" `shouldBe` Just ("foo", "")

      it "parses strings with Unicode escapes" $ do
        parseValue "\"foo-\\u03BB-bar\"" `shouldBe` Just ("foo-\955-bar", "")

      it "allows to escape double quotes " $ do
        parseValue "\"foo-\\\"-bar\"" `shouldBe` Just ("foo-\"-bar", "")

      it "allows to escape backslashes" $ do
        parseValue "\"foo-\\\\-bar\"" `shouldBe` Just ("foo-\\-bar", "")

      it "accepts backspace escape sequence" $ do
        parseValue "\"foo-\\b-bar\"" `shouldBe` Just ("foo-\b-bar", "")

      it "accepts formfeed escape sequence" $ do
        parseValue "\"foo-\\f-bar\"" `shouldBe` Just ("foo-\f-bar", "")

      it "accepts newline escape sequence" $ do
        parseValue "\"foo-\\n-bar\"" `shouldBe` Just ("foo-\n-bar", "")

      it "accepts carriage return escape sequence" $ do
        parseValue "\"foo-\\r-bar\"" `shouldBe` Just ("foo-\r-bar", "")

      it "accepts horizontal tab escape sequence" $ do
        parseValue "\"foo-\\t-bar\"" `shouldBe` Just ("foo-\t-bar", "")

    context "when given invalid input" $ do
      it "returns an error" $ do
        parseValue "baz" `shouldBe` Nothing
