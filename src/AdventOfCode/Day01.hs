module AdventOfCode.Day01
  ( day01, combinationsOf, solutionOne, solutionTwo
  ) where 

import Data.List ( find )
import Data.Maybe ( fromJust )

parseInput :: String -> Int
parseInput s = read s :: Int

-- >>> combinationsOf 0 [1,2,3]
-- [[]]
-- >>> combinationsOf 1 [1,2,3]
-- [[1],[2],[3]]
-- >>> combinationsOf 2 [1,2,3]
-- [[1,2],[1,3],[2,3]]
combinationsOf :: Int -> [a] -> [[a]]
combinationsOf 0 _ = [[]]
combinationsOf _ [] = []
combinationsOf n items = 
  if n == length items then [items] 
  else 
    includingHead ++ notIncludingHead
    where
      (x:xs) = items
      includingHead = map (x :)(combinationsOf (n - 1) xs)
      notIncludingHead = combinationsOf n xs

solutionOne :: String -> Int
solutionOne input = 
    (product . fromJust)(find (\xs -> sum xs == 2020) combinations)
  where 
    parsedInput = map parseInput (lines input)
    combinations = combinationsOf 2 parsedInput

-- Part two

solutionTwo :: String -> Int
solutionTwo input = 
    (product . fromJust)(find (\xs -> sum xs == 2020) combinations)
  where 
    parsedInput = map parseInput (lines input)
    combinations = combinationsOf 3 parsedInput

-- >>> day01
day01 :: IO ()
day01 = do  
    problemInput <- readFile "input/day-01.txt"
    putStrLn "Day 1"
    putStrLn $ "  Part one: " ++ show (solutionOne problemInput)
    putStrLn $ "  Part one: " ++ show (solutionTwo problemInput)
