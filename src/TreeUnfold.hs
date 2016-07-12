{- Tree unfold (2.2.2) -}

-- tree type
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

-- useful functions
unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr next x = case next x of
                    Nothing -> []
                    Just (y, r) -> y:unfoldr next r

unfoldTree :: (s -> Either a (s, s)) -> s -> Tree a
unfoldTree next x = case next x of
                        Left y -> Leaf y
                        Right (l, r) -> Node (unfoldTree next l) (unfoldTree next r)

-- iterate f x should generate an infinite list  [x, f x, f (f x),...]
iterate' :: (a -> a) -> a -> [a]
iterate' f x = let rest = iterate' f (f x) in
                x : rest

-- as defined in prelude
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- define using unfold or unfold tree
-- generates a balanced tree of a given height
balanced :: Int -> Tree ()
balanced height = undefined
-- nodes = 2 ^ height

-- define using unfold or unfold tree
-- generates any tree with a given number of nodes, with each leaf having a unique label
sized :: Int -> Tree Int
sized = unfoldTree nextNode

nextNode :: Int -> Either Int (Int, Int)
nextNode n = case n of 1 -> Left 1
                       _ -> Right (n,n)

-- examples
exampleTree1 :: Tree Int
exampleTree1 = Leaf 1
exampleTree2 :: Tree Int
exampleTree2 = Node (Leaf 1) (Leaf 2)
exampleTree3 :: Tree Int
exampleTree3 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

powersOfTwo :: [Int]
powersOfTwo = iterate' (*2) 1

powersOfTwoMinusOne :: [Int]
powersOfTwoMinusOne = map' (subtract 1) powersOfTwo
