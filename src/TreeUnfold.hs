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

-- more useful functions defined in terms of unfoldr and unfoldTree

-- iterate f x should generate an infinite list  [x, f x, f (f x),...]
iterate' :: (a -> a) -> a -> [a]
iterate' f = unfoldr next
                where next s = Just (f s, f s)
-- iterate'' :: (a -> a) -> a -> [a]
-- iterate'' f x = let rest = iterate' f (f x) in
--                 x : rest

-- as defined in prelude
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f xs = unfoldr next xs
                where next list = case list of [] -> Nothing
                                               (y:ys) -> Just (f y, ys)
-- map'' _ [] = []
-- map'' f (x:xs) = f x : map' f xs

-- generates a balanced tree of a given height
balanced :: Int -> Tree ()
balanced = unfoldTree next
            where next x = if x == 0 then Left () else Right (x-1,x-1)

-- define using unfold or unfold tree
-- generates any tree with a given number of nodes, with each leaf having a unique label
sized :: Int -> Tree Int
sized numberOfNodes = unfoldTree next (numberOfNodes,[0..])
                        where next x = case x of
                                            (0,labels) -> Left (head labels)
                                            (1,labels) -> Right ((0,tail labels),(0,[head labels]))
                                            (y,labels) -> Right ((y-1, tail labels),(0, [head labels]))

-- examples (TODO: make module and move examples to tests)
exampleTree1 :: Tree Int
exampleTree1 = Leaf 1
exampleTree2 :: Tree Int
exampleTree2 = Node (Leaf 1) (Leaf 2)
exampleTree3 :: Tree Int
exampleTree3 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

powersOfTwo :: [Int]
powersOfTwo = iterate' (*2) 1 -- will work up to the first 64 terms (2^64), after that it overflows (results in zeros)

powersOfTwoMinusOne :: [Int]
powersOfTwoMinusOne = map' (subtract 1) powersOfTwo -- similarly, works up to first 64 terms

balancedTree5 :: Tree ()
balancedTree5 = balanced 5
