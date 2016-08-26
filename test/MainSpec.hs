module MainSpec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec (Spec, describe, it, shouldBe)

import Main ()

-- | Required for auto-discovery.
spec :: Spec
spec =
    describe "Main" $ do
        describe "newGUID" $ do
            it "creates new GUID" $ property $
                \x -> (read . show) x == (x :: Int)

