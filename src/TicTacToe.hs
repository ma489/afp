{-# LANGUAGE DeriveFoldable #-}
{- Tic-Tac-Toe
    https://en.wikipedia.org/wiki/Tic-tac-toe -}

module TicTacToe where

{- Rose Trees -}

-- aka muti-way tree. Will be used to represent the game tree
data RoseTree a = a :> [RoseTree a] deriving (Show, Foldable)

-- return the value stored at the root of a rose tree
root :: RoseTree a -> a
root (a :> _) = a

-- return the children of the root of a rose tree
children :: RoseTree a -> [RoseTree a]
children (_ :> childTrees) = childTrees

--count the number of nodes in a rose tree
size :: RoseTree a -> Int
size (_ :> childTrees) = 1 + sum (map size childTrees)

-- count the number of leaves (nodes without any children)
leaves :: RoseTree a -> Int
leaves (_ :> []) = 1
leaves (_ :> childTrees) = sum (map leaves childTrees)

-- example
exampleTree :: RoseTree Int
exampleTree = 1 :> [2 :> [], 3 :> []]

{- Game State -}

data Player = P1 | P2 deriving Eq

-- given the player whose move it is currently, will return the player who will make a move during the next turn
nextPlayer :: Player -> Player
nextPlayer currentPlayer = case currentPlayer of P1 -> P2
                                                 P2 -> P1

-- the board
data Field = X | O | B deriving Eq
type Row = (Field, Field, Field)
type Board = (Row, Row, Row) -- gives easy access to horizontal rows

instance Show Field where
    show X = "X"
    show O = "O"
    show B = " "

-- gives the symbol a particular player uses (by convention, first player uses X)
symbol :: Player -> Field
symbol player = if player == P1 then X else O

-- gives access to the columns
verticals :: Board -> (Row, Row, Row)
verticals ((a1, a2, a3), (b1,b2,b3), (c1,c2,c3)) = ((a1,b1,c1), (a2,b2,c2), (a3,b3,c3))

-- gives access to the diagonal rows
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

{- Game trees -}

-- returns all possible moves that player can make
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

-- returns which player has won or Nothing if none of the players has won
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

-- A game tree is a rose tree where all the nodes represent game states and all the children
-- of a node represent the valid moves than can be made from the state in the parent node
gameTree :: Player -> Board -> RoseTree Board
gameTree player board = board :> map nextMove (moves player board)
                        where nextMove b = case hasWinner b of Nothing -> gameTree player b
                                                               Just _ -> b :> []

{- Game complexity -}

-- number of leaves in the game tree, a measure of game complexity - (should be 9! ?)
gameTreeComplexity :: Int
gameTreeComplexity = leaves (gameTree P1 emptyBoard)


{- Minimax -}

minimax :: Player -> RoseTree Board -> RoseTree Int
minimax player gamTree = undefined

-- minimax' = undefined

-- minimum' :: (Ord a, Foldable t) => t a -> a
-- minimum' elements = foldl (\x acc -> if x < acc then x else acc) maxBound elements

-- maximum' :: (Ord a, Foldable t) => t a -> a
-- maximum' _ = undefined

makeMove :: Player -> Board -> Maybe Board
makeMove player board = undefined

main :: IO ()
main = do
        putStrLn "-- Tic-Tac-Toe --"
        putStrLn $ printBoard emptyBoard
        sequence_ $ map (putStrLn . printBoard) (moves P1 emptyBoard)
        return ()
