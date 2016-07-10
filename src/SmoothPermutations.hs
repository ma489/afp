{- smooth_perms: returns all permutations of its second argument for which the
   distance between each two successive elements is at most the first argument -}

import Data.List

{- naive; expensive -}

split :: [t] -> [(t, [t])]
split [] = []
split (x:xs) = (x, xs):[(y, x:ys) | (y, ys) <- split xs]

perms :: [t] -> [[t]]
perms [] = [[]]
perms xs = [v:p | (v, vs) <- split xs, p <- perms vs]

smooth :: (Num a, Ord a) => a -> [a] -> Bool
smooth n (x:y:ys) = abs (y - x) <= n && smooth n (y:ys)
smooth _ _ = True

smoothPerms :: Int -> [Int] -> [[Int]]
smoothPerms n xs = filter (smooth n) (perms xs)

{- better -}

-- a tree where each route from root to leaf corresponds to a permutation
data PermutationTree = Node Int [PermutationTree] deriving Show

-- maps a given list onto this tree structure (root node is some sentinel with value maxBound)
listToPermutationTree :: [Int] -> PermutationTree
listToPermutationTree xs = Node (maxBound::Int) $ listToPermutationTree' xs

listToPermutationTree' :: [Int] -> [PermutationTree]
listToPermutationTree' xs = [ Node x (listToPermutationTree' (xs \\ [x])) | x <- xs]

-- generate all permutations from tree
generatePermutations :: Int -> PermutationTree -> [[Int]]
generatePermutations n (Node _ startingPoints) = concatMap (generatePermutations' n) startingPoints

generatePermutations' :: Int -> PermutationTree -> [[Int]] -- list of sequences starting with v
generatePermutations' _ (Node v []) = [[v]]
generatePermutations' n (Node v children) = [ v:p | p <- concatMap (generatePermutations' n) $ filter (smoothBranch v) children ]
                                                where smoothBranch v1 (Node v2 _) = abs (v1 - v2) <= n

smoothPermsImproved :: Int -> [Int] -> [[Int]]
smoothPermsImproved n xs = generatePermutations n $ listToPermutationTree xs

{- examples -}

--naive
example1Naive :: [[Int]]
example1Naive = smoothPerms 1 [1,2] -- expect: [[1,2],[2,1]]
example2Naive :: [[Int]]
example2Naive = smoothPerms 1 [1,2,3] -- expect: [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
example3Naive :: [[Int]]
example3Naive = smoothPerms 1 [1,2,3,4,5] -- expect: [[1,2,3,4,5],[5,4,3,2,1]]

--better
example1Better :: [[Int]]
example1Better = smoothPermsImproved 1 [1,2] -- expect: [[1,2],[2,1]]
example2Better :: [[Int]]
example2Better = smoothPermsImproved 1 [1,2,3] -- expect: [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
example3Better :: [[Int]]
example3Better = smoothPermsImproved 1 [1,2,3,4,5] -- expect: [[1,2,3,4,5],[5,4,3,2,1]]
