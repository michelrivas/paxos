module ProposerSpec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec (Spec, describe, it, shouldBe)

import Proposer ()

-- | Required for auto-discovery.
spec :: Spec
spec =
    describe "Proposer" $ do
        describe "newGUID" $ do
            it "creates new GUID" $ property $
                \x -> (read . show) x == (x :: Int)