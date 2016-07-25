--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Spec where

import Test.Hspec

import qualified MainSpec
import qualified ProposerSpec
import qualified AcceptorSpec
import qualified UtilsSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "MainSpec"     MainSpec.spec
    describe "ProposerSpec" ProposerSpec.spec
    describe "AcceptorSpec"     AcceptorSpec.spec
    describe "UtilsSpec"     UtilsSpec.spec