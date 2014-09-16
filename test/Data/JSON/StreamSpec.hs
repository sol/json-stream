{-# LANGUAGE OverloadedStrings #-}
module Data.JSON.StreamSpec (main, spec) where

import           Test.Hspec

import           Data.JSON.Stream
import           Data.Aeson (Value(..), toJSON, object, (.=))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "decodeValue" $ do
    it "parses true" $ do
      decodeValue "true" `shouldBe` Just (toJSON True)

    it "parses false" $ do
      decodeValue "false" `shouldBe` Just (toJSON False)

    it "parses null" $ do
      decodeValue "null" `shouldBe` Just Null

    it "parses number" $ do
      decodeValue "23" `shouldBe` Just (Number 23)

    it "parses a number surrounded by whitespaces" $
      decodeValue "   23   " `shouldBe` Just (Number 23)

    it "parses arrays" $ do
      decodeValue "[true,false,null]" `shouldBe`
        Just (toJSON [toJSON True, toJSON False, Null])

    it "parses objects" $ do
      decodeValue "{\"foo\":23,\"bar\":42}" `shouldBe`
        Just (object ["foo" .= Number 23, "bar" .= Number 42])

    it "parses objects with whitespaces/newlines sprinkled all around" $
      decodeValue "{\n \"x\" : 23, \n \"y\" : 11, \n \"name\" :\t \"Bob\" \n } " `shouldBe`
        Just (object ["x" .= Number 23, "y" .= Number 11, "name" .= String "Bob"])

    context "when parsing strings" $ do
      it "parses simple strings" $ do
        decodeValue "\"foo\"" `shouldBe` Just "foo"

      it "parses strings with Unicode escapes" $ do
        decodeValue "\"foo-\\u03BB-bar\"" `shouldBe` Just "foo-\955-bar"

      it "allows to escape double quotes " $ do
        decodeValue "\"foo-\\\"-bar\"" `shouldBe` Just "foo-\"-bar"

      it "allows to escape backslashes" $ do
        decodeValue "\"foo-\\\\-bar\"" `shouldBe` Just "foo-\\-bar"

      it "accepts backspace escape sequence" $ do
        decodeValue "\"foo-\\b-bar\"" `shouldBe` Just "foo-\b-bar"

      it "accepts formfeed escape sequence" $ do
        decodeValue "\"foo-\\f-bar\"" `shouldBe` Just "foo-\f-bar"

      it "accepts newline escape sequence" $ do
        decodeValue "\"foo-\\n-bar\"" `shouldBe` Just "foo-\n-bar"

      it "accepts carriage return escape sequence" $ do
        decodeValue "\"foo-\\r-bar\"" `shouldBe` Just "foo-\r-bar"

      it "accepts horizontal tab escape sequence" $ do
        decodeValue "\"foo-\\t-bar\"" `shouldBe` Just "foo-\t-bar"

    context "when given invalid input" $ do
      it "returns an error" $ do
        decodeValue "baz" `shouldBe` Nothing
