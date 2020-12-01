module AdventOfCode.Day01Spec (main, spec) where

import Test.Hspec ( hspec, describe, it, shouldBe, Spec )

import AdventOfCode.Day01 ( day01, combinationsOf, solutionOne, solutionTwo )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "combinationsOf" $ do
    it "should return combinations without replacement from the list" $ do
      combinationsOf 0 [1,2,3] `shouldBe` [[]]
      combinationsOf 1 [1,2,3] `shouldBe` [[1],[2],[3]]
      combinationsOf 2 [1,2,3] `shouldBe` [[1,2],[1,3],[2,3]]
      combinationsOf 3 [1,2,3] `shouldBe` [[1,2,3]]
  describe "solutionOne" $ do
    it "should return expected responses for example data" $ do
      solutionOne exampleData `shouldBe` 514579
  describe "solutionTwo" $ do
    it "should return expected responses for example data" $ do
      solutionTwo exampleData `shouldBe` 241861950
  where
    exampleData = "1721\n979\n366\n299\n675\n1456"