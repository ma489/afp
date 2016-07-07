module TestMasterMind where

import Test.HUnit
import qualified MasterMind as MasterMind

tests = TestList [testCheck1, testCheck2, testCheck3, testCheck4,
                  testCheck5, testCheck6, testCheck7, testCheck8,
                  testCheck9, testCheck10]

testCheck1 = testCase [3,4,6,6] [1,1,2,2] (0,0,False)
testCheck2 = testCase [3,4,6,6] [3,3,4,4] (1,1,False)
testCheck3 = testCase [3,4,6,6] [3,5,3,6] (2,0,False)
testCheck4 = testCase [3,4,6,6] [3,4,6,6] (4,0,True)

testCheck5 = testCase [5,1,1,4] [1,2,3,4] (1,1,False)
testCheck6 = testCase [5,1,1,4] [1,3,5,6] (0,2,False)
testCheck7 = testCase [5,1,1,4] [5,2,1,5] (2,0,False)
testCheck8 = testCase [5,1,1,4] [5,2,4,1] (1,2,False)
testCheck9 = testCase [5,1,1,4] [5,4,1,1] (2,2,False)
testCheck10 = testCase [5,1,1,4] [5,1,1,4] (4,0,True)

testCase solution guess output = TestLabel ("Test Check " ++ solutionAndGuess) thisTestCase
                                    where thisTestCase = TestCase (assertEqual message output actual)
                                          actual = MasterMind.check solution guess
                                          message = "for check " ++ solutionAndGuess
                                          solutionAndGuess = show solution ++ " " ++ show guess

main = do
    putStrLn "> Testing MasterMind..."
    runTestTT tests
