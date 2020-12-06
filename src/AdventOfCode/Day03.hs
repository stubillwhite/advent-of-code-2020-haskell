module AdventOfCode.Day03
  ( day03, solutionOne
  ) where 

data State = State
  { forest :: [String]
  , width :: Int
  , height :: Int
  , trees :: Int
  , x :: Int
  , y :: Int
  , dx :: Int
  , dy :: Int
  } deriving (Show, Eq, Ord)

initialState :: [String] -> Int -> Int -> State
initialState forest dx dy =
  State forest width height 0 0 0 dx dy
  where
    height = length forest
    width = (length . head) forest

hitTree :: State -> Bool
hitTree State{forest, x, y} =
  forest !! y !! x == '#'

handleCollisions :: State -> State
handleCollisions state@State{height, y, trees} =
  if (y < height) && hitTree state then state{trees = trees + 1}
  else state

nextState :: State -> State
nextState state@State{width, x, y, dx, dy} =
  handleCollisions state{x = newX, y = newY}
  where
    newX = mod (x + dx) width
    newY = y + dy

totalTreesHit :: State -> Int
totalTreesHit state@State{height, y, trees} =
  if (y < height) then (totalTreesHit . nextState) state
  else trees

solutionOne :: String -> Int
solutionOne input = 
    totalTreesHit state
    where
      forest = lines input
      state = initialState forest 3 1

day03 :: IO ()
day03 = do  
    problemInput <- readFile "input/day-03.txt"
    putStrLn "Day 3"
    putStrLn $ "  Part one: " ++ show (solutionOne problemInput)