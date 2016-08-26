module ProposerSpec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec (Spec, describe, it, shouldBe)

import Proposer (prepareRequest, prepareAccepted, acceptAccepted) 

-- | Required for auto-discovery.
spec :: Spec
spec =
    describe "Proposer" $ do
        describe "prepareRequest" $ do
            it "prepares request" $ property $
                \x -> x == (x :: Int)        
        describe "prepareAccepted" $ do
            it "prepares accepted" $ property $
                \x -> x == (x :: Int)        
        describe "acceptAccepted" $ do
            it "accepts accepted" $ property $
                \x -> x == (x :: Int)
