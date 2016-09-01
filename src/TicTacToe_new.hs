{- Tic-Tac-Toe https://en.wikipedia.org/wiki/Tic-tac-toe -}

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

{-# LANGUAGE TupleSections #-} {- A handy syntax extension. See:

    http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/syntax-extns.html#tuple-sections

-}
{-# LANGUAGE DeriveFoldable #-}

module Assignment2 where -- Rename to "Main" if you want to compile the game.
                         -- Don't forget to rename it back when submitting!

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Tree as T

import System.IO

-- | Rose trees

data Rose a = a :> [Rose a]
    deriving (Eq, Show, Foldable)

root :: Rose a -> a
root (a :> _) = a

children :: Rose a -> [Rose a]
children (_ :> childTrees) = childTrees

size :: Rose a -> Int
size (_ :> childTrees) = 1 + sum (map size childTrees)

leaves :: Rose a -> Int
leaves (_ :> []) = 1
leaves (_ :> childTrees) = sum (map leaves childTrees)

drawTree :: (Show a) => Rose a -> String
drawTree tree = T.drawTree $ transformTree tree

transformTree :: (Show a) => Rose a -> T.Tree String
transformTree (x :> []) = T.Node (show x) []
transformTree (x :> xs) = T.Node (show x) (map transformTree xs)

-- | State representation

-- * Players

data Player = P1 | P2
    deriving (Eq, Ord)

instance Show Player where
    show P1 = "Player 1"
    show P2 = "Player 2"

nextPlayer :: Player -> Player
nextPlayer currentPlayer = case currentPlayer of P1 -> P2
                                                 P2 -> P1

-- * Board

data Field = X | O | B
    deriving (Eq, Ord)

instance Show Field where
    show X = "X"
    show O = "O"
    show B = " "

symbol :: Player -> Field
symbol player = if player == P1 then X else O

type Row   = (Field, Field, Field)
type Board = (Row, Row, Row)

verticals :: Board -> (Row, Row, Row)
verticals ((a1, a2, a3), (b1,b2,b3), (c1,c2,c3)) = ((a1,b1,c1), (a2,b2,c2), (a3,b3,c3))

diagonals :: Board -> (Row, Row)
diagonals ((a1, _, a3), (_,b2,_), (c1,_,c3)) = ((a1,b2,c3),(a3,b2,c1))

emptyBoard :: Board
emptyBoard = (blankRow, blankRow, blankRow)
                where blankRow = (B,B,B)

printBoard :: Board -> String
printBoard (row1, row2, row3) = concat [rowSeparator, rowToString row1, rowToString row2, rowToString row3]
                                    where rowToString (c1,c2,c3) = concat [col c1, col c2, col c3, "|", rowSeparator]
                                          col = ("|" ++) . show
                                          rowSeparator = "\n+-+-+-+\n"

-- | Move generation

moves :: Player -> Board -> [Board]
moves player board = [replaceCell blankCellPosition (symbol player) board | blankCellPosition <- findBlankCellPositions board]

findBlankCellPositions :: Board -> [(Int, Int)]
findBlankCellPositions (r1, r2, r3) = findBlankCellPositions' r1 0 ++ findBlankCellPositions' r2 1 ++ findBlankCellPositions' r3 2

findBlankCellPositions' :: Row -> Int -> [(Int, Int)]
findBlankCellPositions' (c1, c2, c3) rn = [(rn, i) | (c, i) <- [(c1, 0), (c2, 1), (c3, 2)], c == B]

replaceCell :: (Int, Int) -> Field -> Board -> Board
replaceCell (x,y) symbol' (r1, r2, r3)
     | x == 0 = (modify r1 y symbol', r2, r3)
     | x == 1 = (r1, modify r2 y symbol', r3)
     | otherwise = (r1, r2, modify r3 y symbol')

modify :: Row -> Int -> Field -> Row
modify (c1, c2, c3) columnNumber symbol'
    | columnNumber == 0 = (symbol', c2, c3)
    | columnNumber == 1 = (c1, symbol', c3)
    | otherwise = (c1, c2, symbol')

-- | Gametree generation

hasWinner :: Board -> Maybe Player
hasWinner board = do
    let (col1, col2, col3) = verticals board
    let (row1, row2, row3) = board
    let (diag1, diag2) = diagonals board
    let allRows = [col1, col2, col3, row1, row2, row3, diag1, diag2]
    let winner = checkForWinningRow allRows (winningBoardForPlayer P1)
    if
        winner
    then
        Just P1
    else
        if
            checkForWinningRow allRows (winningBoardForPlayer P2)
        then
            Just P2
        else
            Nothing

winningBoardForPlayer :: Player -> Row
winningBoardForPlayer player = let symbol' = symbol player in
                                (symbol',symbol',symbol')

checkForWinningRow :: [Row] -> Row -> Bool
checkForWinningRow [] _ = False
checkForWinningRow (row:rows) winningRow = row == winningRow || checkForWinningRow rows winningRow

gameTree :: Player -> Board -> Rose Board
gameTree player board = board :> map nextMove (moves player board)
                        where nextMove b = case hasWinner b of Nothing -> gameTree (otherPlayer player) b
                                                               Just _ -> b :> []
fullGameTree :: Rose Board
fullGameTree = gameTree P1 emptyBoard

-- | Game complexity

gameTreeComplexity :: Int
gameTreeComplexity = leaves fullGameTree -- should be 255,168 (naive is 9!)

-- | Minimax
-- https://en.wikipedia.org/wiki/Minimax#Combinatorial_game_theory

-- Exercise 12

minimax :: Player -> Rose Board -> Rose Int
minimax player board = minimax' player player board

minimax' :: Player -> Player -> Rose Board -> Rose Int
minimax' _ maximisingPlayer (a :> []) = calculateValueFunction maximisingPlayer a :> []
minimax' player maximisingPlayer (a :> as) = (minOrMax $ map root children) :> children
    where minOrMax = if player == maximisingPlayer then maximum else minimum
          children = map (minimax' player (otherPlayer maximisingPlayer)) as
-- minBound :: Int?

calculateValueFunction :: Player -> Board -> Int
calculateValueFunction maximisingPlayer x = case hasWinner x of Just p -> if p == maximisingPlayer then 1 else -1
                                                                Nothing -> 0

otherPlayer :: Player -> Player
otherPlayer player = if player == P1 then P2 else P1

-- * Lazier minimum and maximums

-- Exercise 13

minimum' :: [Int] -> Int
minimum' = undefined

maximum' :: [Int] -> Int
maximum' = undefined

-- | Gameplay

-- Exercise 14

makeMove :: Player -> Board -> Maybe Board
makeMove player board = do
    let gameTree' = gameTree player board
    let minimaxTree = minimax player gameTree'
    let possibleMoves = zip [0..] $ map root $ children minimaxTree
    let (bestMove, score) = maximumBy (\(a1,a2) (b1,b2) -> if a2 > b2 then GT else if a2 < b2 then LT else EQ) possibleMoves
    let move = root $ (children gameTree') !! bestMove
    Just move

-- | Main

data PlayerType = Human | Computer

instance Show PlayerType where
    show Human    = "H"
    show Computer = "C"

main :: IO ()
main = do
    typeOfP1 <- askFor "Should Player 1 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]
    typeOfP2 <- askFor "Should Player 2 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]

    let playerType :: Player -> PlayerType
        playerType P1 = typeOfP1
        playerType P2 = typeOfP2

        gameLoop :: Player -> Board -> IO ()
        gameLoop p b = do
            putStrLn ("\n" ++ printBoard b)
            case hasWinner b of
                Just pl  -> putStrLn (show pl ++ " has won!")
                Nothing -> do
                    putStr   ("It's " ++ show p ++ "'s turn. ")
                    mb' <- case playerType p of
                        Human    -> humanMove    p b
                        Computer -> computerMove p b
                    case mb' of
                        Nothing -> do putStr   "No more moves are possible. "
                                      putStrLn "It's a draw."
                        Just b' -> gameLoop (nextPlayer p) b'

        humanMove :: Player -> Board -> IO (Maybe Board)
        humanMove p b = do
            let possibleMoves = moves p b
            if null possibleMoves then
                return Nothing
            else do
                putStrLn "Possible moves are:"
                putStrLn (listMoves possibleMoves)
                i <- askFor "Make your choice:" [1..length possibleMoves]
                return (Just (possibleMoves !! (i-1)))

        computerMove :: Player -> Board -> IO (Maybe Board)
        computerMove p b = do
            putStrLn "Thinking..."
            return (makeMove p b)

        listMoves :: [Board] -> String
        listMoves = intercalate "\n"
                    . map (intercalate "    ")
                    . transpose
                    . map (lines . (\(i,b) -> "  (" ++ show i ++ "): \n" ++ printBoard b))
                    . zip [1..]

    gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
    putStr (m ++ " ")
    hFlush stdout
    i <- getLine
    case find ((map toLower i ==) . map toLower . show) xs of
        Nothing -> do putStrLn $ "I didn't understand you. Enter one of: "
                                 ++ intercalate ", " (map show xs) ++ "."
                      askFor m xs
        Just y  -> return y
