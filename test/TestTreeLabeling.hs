module TestTreeLabeling where

import Test.HUnit
import TreeLabeling

tests = TestList [testLabeling1, testLabeling2, testLabeling3, testLabeling4, testLabeling5, testLabeling6]

testLabeling1 = testCase Leaf Leaf
testLabeling2 = testCase (BinTree Leaf (-1) Leaf) (BinTree Leaf 1 Leaf)
testLabeling3 = testCase (BinTree Leaf (-1) (BinTree Leaf (-1) Leaf)) (BinTree Leaf 1 (BinTree Leaf (2) Leaf))
testLabeling4 = testCase (BinTree (BinTree Leaf (-1) Leaf) (-1) Leaf) (BinTree (BinTree Leaf (2) Leaf) 1 Leaf)
testLabeling5 = testCase (BinTree (BinTree (BinTree Leaf (-3) Leaf) (-2) Leaf) (-1) Leaf) (BinTree (BinTree (BinTree Leaf (3) Leaf) (2) Leaf) (1) Leaf)
testLabeling6 = testCase (BinTree (BinTree (BinTree Leaf (-3) Leaf) (-2) Leaf) (-1) (BinTree Leaf (-4) Leaf)) (BinTree (BinTree (BinTree Leaf (4) Leaf) (2) Leaf) (1) (BinTree Leaf (3) Leaf))

testCase inputTree outputTree = TestLabel ("Test TreeLabeling " ++ show inputTree) thisTestCase
                                    where thisTestCase = TestCase (assertEqual message outputTree actual)
                                          actual = labelTree inputTree
                                          message = "for labeling " ++ show inputTree ++ " " ++ show outputTree

main = do
    putStrLn "> Testing TreeLabeling..."
    runTestTT tests
