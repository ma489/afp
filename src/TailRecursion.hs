{- Tail recursion (2.2.1) -}

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

splitleft :: Tree a -> (a, Maybe (Tree a))
splitleft (Leaf a) = (a, Nothing)
splitleft (Node l r) = case splitleft l of
                        (a, Nothing) -> (a, Just r)
                        (a, Just l') -> (a, Just (Node l' r))

-- tail recursive version of splitLeft
splitleft'' :: Tree a -> (a, Maybe (Tree a))
splitleft'' (Leaf a) = (a, Nothing)
splitleft'' (Node l r) = let (v, m) = splitleft' l (flip Node r)
                            in (v, Just m)

splitleft' :: Tree a -> (Tree a -> Tree a) -> (a, Tree a)
splitleft' (Leaf a) f = let (Node _ r) = f (Leaf a) in
                            (a, r)
splitleft' (Node (Leaf a) r) f = (a, f r)
splitleft' (Node l r) f = splitleft' l (\l -> f (Node l r))

--examples (call splitleft'' x_)
-- TODO: move to tests

x1 = Leaf 1 -- (1, Nothing)
x2 = Node (Leaf 1) (Leaf 2) -- (1, Just (Leaf 2))
x3 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4)) -- (1, Just (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
x4 = Node (Leaf 1) (Node (Leaf 2) (Leaf 3)) -- (1, Just (Node (Leaf 2) (Leaf 3)))
x5 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3) -- (1, Just (Node (Leaf 2) (Leaf 3)))
x6 = Node l r
        where l = Node ll lr
              ll = Node (Leaf 1) (Leaf 2)
              lr = Node (Leaf 3) (Leaf 4)
              r = Node rl rr
              rl = Node (Leaf 5) (Leaf 6)
              rr = Node (Leaf 7) (Leaf 8)

--x6: 1, and ...
--Node
    --(Node
        --(Leaf 2)
        --(Node
            --(Leaf 3)
            --(Leaf 4)))
    --(Node
        --(Node
            --(Leaf 5)
            --(Leaf 6))
        --(Node
            --(Leaf 7)
            --(Leaf 8))))
