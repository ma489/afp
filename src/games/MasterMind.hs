module MasterMind where

import Data.List
import System.Exit
import System.Random  -- for randoms
import System.IO      -- for hFlush

type Row = [Int]
type Guess = Row
type Solution = Row

colors :: Int
colors = 6

width :: Int
width  = 4

-- This is the complete main function. It just initializes the
-- game by generating a solution and then calls the game loop.
main :: IO ()
main =
  do
    putStrLn "Mastermind.\nEnter guesses in format: w x y z.\nEnter 'quit' to exit"
    s <- generateSolution -- initialization
    loop s 1               -- game loop

-- The following function is given. It generates a random solution of the
-- given width, and using the given number of colors.
generateSolution :: IO Solution
generateSolution =
  do
    g <- getStdGen
    let rs = take width (randoms g)
    return (map ((+1) . (`mod` colors)) rs)

-- The loop function is supposed to perform a single interaction. It
-- reads an input, compares it with the solution, produces output to
-- the user, and if the guess of the player was incorrect, loops.
loop :: Solution -> Integer -> IO ()
loop s turns =
  do
    i <- input s           -- read (and parse) the user input
    let (b, w, correctSolution) = check s i
    putStrLn $ report (b, w, correctSolution)
    if correctSolution then do
      putStrLn ("Took " ++ show turns ++ " turns")
      exitSuccess
    else loop s (turns + 1)

--div rounds down. odd number indicates the number exists more times in one list than the other
black :: Solution -> Guess -> Int
black solution guess = count $ zipWith (==) solution guess
                        where count = length . filter id

white :: Solution -> Guess -> Int
white solution guess = sum $ map (count . symmetricDifference) positions
                        where count = (`div` 2) . length
                              positions = keepNonMatchingPositions $ map getPositions guess
                              keepNonMatchingPositions = nub . filter (\(g,s) -> g /= s && s /= [])
                              getPositions x = (elemIndices x guess, elemIndices x solution)

-- aka disjunctive union
symmetricDifference :: Eq a => ([a],[a]) -> [a]
symmetricDifference (a,b) = (a \\ b) `union` (b \\ a)

check :: Solution -> Guess -> (Int,   -- number of black points,
                               Int,   -- number of white points
                               Bool)  -- all-correct guess?
check solution guess = let blackScore = black solution guess in
                          (blackScore,
                           white solution guess,
                           blackScore == width)

-- report is supposed to take the result of calling check, and
-- produces a descriptive string to report to the player.
report :: (Int, Int, Bool) -> String
report (blackScore, whiteScore, correct) =
  let message = "> " ++ show blackScore ++ " black, " ++ show whiteScore ++ " white" in
    if correct then message ++ "\nCongratulations" else message

-- The function input is supposed to read a single guess from the player.
input :: Solution -> IO Guess
input s =
  do
    putStr "? "
    hFlush stdout -- ensure that the prompt is printed
    l <- getLine
    if l == "quit" then do
      putStrLn ("Solution: " ++ show s)
      exitSuccess
    else do
      let inputGuess = map readInt (words l)
      if not (valid inputGuess) then input s else return inputGuess

-- The following function |readInt| is given and parses a single
-- integer from a string. It produces |-1| if the parse fails.
readInt :: String -> Int
readInt x =
  case reads x of
    [(n, "")] -> n
    _         -> -1

-- The function valid tests a guess for validity.
valid :: Guess -> Bool
valid guess = length guess == width && inRange guess
                where inRange = foldl (\a g -> g >= 1 && g <= colors && a) True