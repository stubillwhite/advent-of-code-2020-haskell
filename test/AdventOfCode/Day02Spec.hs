module AdventOfCode.Day02Spec (main, spec) where

import Test.Hspec ( hspec, describe, it, shouldBe, Spec )

import AdventOfCode.Day02 ( day02, solutionOne )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solutionOne" $ do
    it "should return expected responses for example data" $ do
      solutionOne exampleData `shouldBe` Right 2
  where
    exampleData = unlines ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]