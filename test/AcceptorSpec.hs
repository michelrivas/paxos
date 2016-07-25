module AcceptorSpec where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec (Spec, describe, it, shouldBe)

import Acceptor ()

-- | Required for auto-discovery.
spec :: Spec
spec =
    describe "Acceptor" $ do
        describe "newGUID" $ do
            it "creates new GUID" $ property $
                \x -> (read . show) x == (x :: Int)