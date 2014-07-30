{-# LANGUAGE OverloadedStrings #-}
module Data.JSON.StreamSpec (main, spec) where

import           Test.Hspec

import           Data.JSON.Stream

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseValue" $ do
    it "parses true" $ do
      parseValue "true" `shouldBe` Just ([ValueTrue], "")

    it "parses false" $ do
      parseValue "false" `shouldBe` Just ([ValueFalse], "")

    it "parses null" $ do
      parseValue "null" `shouldBe` Just ([ValueNull], "")

    it "parses number" $ do
      parseValue "23" `shouldBe` Just ([ValueNumber "23"], "")

    it "parses strings" $ do
      parseValue "\"foo\"" `shouldBe` Just ([ValueString "foo"], "")

    it "parses arrays" $ do
      parseValue "[true,false,null]" `shouldBe` Just ([ArrayBegin, ValueTrue, ValueFalse, ValueNull, ArrayEnd], "")

    it "parses objects" $ do
      parseValue "{\"foo\":23,\"bar\":42}" `shouldBe` Just ([ObjectBegin, Name "foo", ValueNumber "23", Name "bar", ValueNumber "42", ObjectEnd], "")

    context "when given invalid input" $ do
      it "returns an error" $ do
        parseValue "baz" `shouldBe` Nothing
