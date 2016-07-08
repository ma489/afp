-- Tree labeling question: Re-label tree node values so that when reading node values row by row, you get numbers 1,2,3...
-- i.e. breadth-first labelling 1,2,3...
-- we don't care about care about the input type

{-# LANGUAGE DeriveFunctor #-}

module TreeLabeling where

import Data.List
import Control.Monad.State

data Tree a = Leaf | BinTree (Tree a) a (Tree a) deriving (Show, Eq, Functor)

labelTree :: Tree a -> Tree Int
labelTree tree = let (tree', context) = treeInfo tree "" 0 in
                    fmap (lookUpIndex (sortTreeInfo context)) tree'

lookUpIndex :: [(String, Int)] -> String -> Int
lookUpIndex context key = let value = find (\((k,_),_) -> k == key) (zip context [1..]) in
                            case value of Nothing -> -1
                                          Just (_,i) -> i

treeInfo :: Tree a -> String -> Int -> (Tree String, [(String, Int)])
treeInfo Leaf _ _ = (Leaf, [])
treeInfo (BinTree leftTree _ rightTree) key depth =
    let (l', l'') = treeInfo leftTree (key ++ "l") (depth+1)
        (r', r'') = treeInfo rightTree (key ++ "r") (depth+1)
    in (BinTree l' key r', (key, depth) : l'' ++ r'')

sortTreeInfo :: [(String, Int)] -> [(String, Int)]
sortTreeInfo = sortBy (\(_, d1) (_, d2) -> if d1 > d2 then GT else if d1 < d2 then LT else EQ)

-- extension: (for comparison) in-order, depth-first labelling

labelTreeInOrder :: Tree a -> State Int (Tree Int)
labelTreeInOrder Leaf = return Leaf
labelTreeInOrder (BinTree lt v rt) = do
                                        left <- (labelTreeInOrder lt)
                                        n <- get
                                        put (n+1)
                                        right <- (labelTreeInOrder rt)
                                        return (BinTree left n right)

t1 = BinTree (BinTree (BinTree Leaf (-3) Leaf) (-2) Leaf) (-1) (BinTree Leaf (-4) Leaf)
t2 = BinTree
        (BinTree (BinTree Leaf (-3) Leaf) (-2) Leaf)
        (-1)
        (BinTree (BinTree (BinTree Leaf (-3) Leaf) (-2) Leaf) (-1) (BinTree Leaf (-4) Leaf))

doLabeling tree = fst $ runState (labelTreeInOrder tree) 1 --try with t1, or t2
