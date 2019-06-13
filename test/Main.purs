module Test.Main where

import Prelude

import Data.TwoSet (TwoSet(..))
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  describe "Eq" do
    it "works for examples" do
      MkTwoSet 1 2 `shouldEqual` MkTwoSet 1 2
      MkTwoSet 1 2 `shouldEqual` MkTwoSet 2 1
      MkTwoSet 1 2 `shouldNotEqual` MkTwoSet 1 3
      MkTwoSet 1 2 `shouldNotEqual` MkTwoSet 3 1
  describe "Ord" do
    it "works for equal tuples" do
      compare (MkTwoSet 1 4) (MkTwoSet 1 4) `shouldEqual` EQ
      compare (MkTwoSet 1 4) (MkTwoSet 4 1) `shouldEqual` EQ
    it "works when the smaller element is non-equal" do
      compare (MkTwoSet 1 5) (MkTwoSet 2 4) `shouldEqual` LT
      compare (MkTwoSet 5 1) (MkTwoSet 2 4) `shouldEqual` LT
      compare (MkTwoSet 2 5) (MkTwoSet 1 4) `shouldEqual` GT
      compare (MkTwoSet 5 2) (MkTwoSet 1 4) `shouldEqual` GT
    it "works when the smaller element is equal" do
      compare (MkTwoSet 1 5) (MkTwoSet 1 4) `shouldEqual` GT
      compare (MkTwoSet 5 1) (MkTwoSet 1 4) `shouldEqual` GT
      compare (MkTwoSet 1 3) (MkTwoSet 1 4) `shouldEqual` LT
      compare (MkTwoSet 3 1) (MkTwoSet 1 4) `shouldEqual` LT
