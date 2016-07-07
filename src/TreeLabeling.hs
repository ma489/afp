-- Tree labeling question: Re-label tree node values so that when reading node values row by row, you get numbers 1,2,3...

{-# LANGUAGE DeriveFunctor #-}

module TreeLabeling where

import Data.List

data Tree a = Leaf | BinTree (Tree a) a (Tree a) deriving (Show, Eq, Functor)

labelTree :: Tree a -> Tree Int
labelTree tree = let (tree', context) = treeInfo tree "" 0 in
                    fmap (lookUpIndex context) tree'

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

sortTreeInfo :: [(String, a, Int)] -> [(String, a, Int)]
sortTreeInfo = sortBy (\(_, _, d1) (_, _, d2) -> if d1 > d2 then GT else if d1 < d2 then LT else EQ)
