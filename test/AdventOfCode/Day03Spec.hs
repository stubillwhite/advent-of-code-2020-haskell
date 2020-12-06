module AdventOfCode.Day03Spec (main, spec) where

import Test.Hspec ( hspec, describe, it, shouldBe, Spec )

import AdventOfCode.Day03 ( day03, solutionOne, solutionTwo)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solutionOne" $ do
    it "should return expected responses for example data" $ do
      solutionOne exampleData `shouldBe` 7
  describe "solutionTwo" $ do
    it "should return expected responses for example data" $ do
      solutionTwo exampleData `shouldBe` 336
  where
    exampleData = 
      unlines 
        [ "..##......."
        , "#...#...#.."
        , ".#....#..#."
        , "..#.#...#.#"
        , ".#...##..#."
        , "..#.##....."
        , ".#.#.#....#"
        , ".#........#"
        , "#.##...#..."
        , "#...##....#"
        , ".#..#...#.#"
        ]