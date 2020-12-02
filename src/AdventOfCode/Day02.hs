module AdventOfCode.Day02
  ( day02, solutionOne, solutionTwo
  ) where 

import Text.ParserCombinators.Parsec

data PasswordEntry = PasswordEntry
  { a :: Int
  , b :: Int
  , ch :: Char
  , password :: String
  }
  deriving (Show, Eq)

-- >>> parse parser "Failed to parse" "1-2 c: abcd\n"
-- Right [PasswordEntry {a = 1, b = 2, ch = 'c', password = "abcd"}]
parser :: Parser [PasswordEntry]
parser = many1 $ passwordEntry <* newline
  where
    passwordEntry :: Parser PasswordEntry
    passwordEntry = PasswordEntry <$> (number <* char '-') <*> (number <* char ' ') <*> (anyChar <* string ": ") <*> many1 alphaNum

    number :: Parser Int
    number = read <$> many1 digit

parseInput :: String -> Either ParseError [PasswordEntry]
parseInput input = parse parser "Failed to parse" input

-- >>> isValidSledRentalPassword (PasswordEntry 1 2 'c' "abc")
-- True
-- >>> isValidSledRentalPassword (PasswordEntry 2 3 'c' "abc")
-- False
isValidSledRentalPassword :: PasswordEntry -> Bool
isValidSledRentalPassword PasswordEntry{a,b,ch,password} = 
    a <= count && count <= b
  where
    count = (length . filter (== ch)) password

solutionOne :: String -> Either ParseError Int
solutionOne input = 
    fmap (length . filter isValidSledRentalPassword) (parseInput input)

-- Part two

-- >>> isValidTobogganRentalPassword (PasswordEntry 1 2 'c' "abc")
-- False
-- >>> isValidTobogganRentalPassword (PasswordEntry 2 3 'c' "abc")
-- True
isValidTobogganRentalPassword :: PasswordEntry -> Bool
isValidTobogganRentalPassword PasswordEntry{a,b,ch,password} = 
    (length . filter id) [charMatchesAt a, charMatchesAt b] == 1
  where
    charMatchesAt = \x -> ch == password !! (x - 1)

solutionTwo :: String -> Either ParseError Int
solutionTwo input = 
    validPasswords
  where
    validPasswords = fmap (length . filter isValidTobogganRentalPassword) (parseInput input)

day02 :: IO ()
day02 = do  
    problemInput <- readFile "input/day-02.txt"
    putStrLn "Day 2"
    putStrLn $ "  Part one: " ++ show (solutionOne problemInput)
    putStrLn $ "  Part two: " ++ show (solutionTwo problemInput)
