module AcceptorSpec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec (Spec, describe, it, shouldBe)

import Acceptor (checkProposal, checkAccept, valueDecided)

-- | Required for auto-discovery.
spec :: Spec
spec =
    describe "Acceptor" $ do
        describe "checkProposal" $ do
            it "checkes proposal" $ property $
                \x -> x == (x :: Int)        
        describe "checkAccept" $ do
            it "checks accept" $ property $
                \x -> x == (x :: Int)        
        describe "valueDecided" $ do
            it "value decided" $ property $
                \x -> x == (x :: Int)