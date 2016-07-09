{- smooth_perms: returns all permutations of its second argument for which the
   distance between each two successive elements is at most the first argument -}

import Data.List

{- naive; expensive -}

split :: [t] -> [(t, [t])]
split [] = []
split (x:xs) = (x, xs):[(y, x:ys) | (y, ys) <- split xs]

perms :: [t] -> [[t]]
perms [] = [[]]
perms xs = [(v:p) | (v, vs) <- split xs, p <- perms vs]

smooth :: (Num a, Ord a) => a -> [a] -> Bool
smooth n (x:y:ys) = abs (y - x) <= n && smooth n (y:ys)
smooth _ _ = True

smooth_perms :: Int -> [Int] -> [[Int]]
smooth_perms n xs = filter (smooth n) (perms xs)

{- better (? or equivalent, really?) -}

-- a tree where each route from root to leaf corresponds to a permutation
data PermutationTree = Node Int [PermutationTree] deriving Show

-- maps a given list onto this tree structure (root node is some sentinel with value maxBound)
listToPermutationTree :: [Int] -> PermutationTree
listToPermutationTree xs = Node (maxBound::Int) $ listToPermutationTree' xs

listToPermutationTree' :: [Int] -> [PermutationTree]
listToPermutationTree' xs = [ Node x (listToPermutationTree' (xs \\ [x])) | x <- xs]

-- generate all permutations from tree
generatePermutations :: Int -> PermutationTree -> [[Int]]
generatePermutations n (Node _ startingPoints) = concat $ map (generatePermutations' n) startingPoints

generatePermutations' :: Int -> PermutationTree -> [[Int]] -- list of sequences starting with v
generatePermutations' n (Node v []) = [[v]]
generatePermutations' n (Node v children) = [ (v:p) | p <- concat $ map (generatePermutations' n) $ filter (\(Node v2 _) -> abs (v - v2) <= n) children ]

smooth_perms_improved :: Int -> [Int] -> [[Int]]
smooth_perms_improved n xs = generatePermutations n $ listToPermutationTree xs

{- examples -}
--naive
example1Naive = smooth_perms 1 [1,2] -- expect: [[1,2],[2,1]]
example2Naive = smooth_perms 1 [1,2,3] -- expect: [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
example3Naive = smooth_perms 1 [1,2,3,4,5] -- expect: [[1,2,3,4,5],[5,4,3,2,1]]

--better
example1Better = smooth_perms_improved 1 [1,2] -- expect: [[1,2],[2,1]]
example2Better = smooth_perms_improved 1 [1,2,3] -- expect: [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
example3Better = smooth_perms_improved 1 [1,2,3,4,5] -- expect: [[1,2,3,4,5],[5,4,3,2,1]]
