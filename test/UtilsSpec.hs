module UtilsSpec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec (Spec, describe, it, shouldBe)

import Utils (parseMessage, Message(..), ServerID)

-- | Required for auto-discovery.
spec :: Spec
spec =
    describe "Utils" $ do
        describe "parseMessage" $ do
            it "parses a message" $ do
                messageType (parseMessage "x" "5:2") `shouldBe` "5"
        describe "parseHostPort" $ do
            it "parses Host:Port" $ property $
                \x -> x == (x :: Int)
        describe "parseMessage" $ do
            it "parses message" $ property $
                \x -> x == (x :: Int)
        describe "saveServer" $ do
            it "saves server" $ property $
                \x -> x == (x :: Int)
