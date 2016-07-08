{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- (non-exhaustive) testing cannot prove the absence of bugs.

module QuickCheckMasterMind where

import Test.QuickCheck
import qualified MasterMind as MasterMind

masterMindProperties = [prop_sumOfBlackScoreAndWhiteScoreIsNeverGreaterThanFour, prop_onceBlackScoreIsFourThenCorrectIsTrue]

prop_sumOfBlackScoreAndWhiteScoreIsNeverGreaterThanFour :: MasterMind.Solution -> MasterMind.Guess -> Bool
prop_sumOfBlackScoreAndWhiteScoreIsNeverGreaterThanFour (MasterMind.Solution solution) (MasterMind.Guess guess) =
    let (blackScore, whiteScore, correct) = MasterMind.check solution guess in
        blackScore + whiteScore <= 4

prop_onceBlackScoreIsFourThenCorrectIsTrue :: MasterMind.Solution -> MasterMind.Guess -> Bool
prop_onceBlackScoreIsFourThenCorrectIsTrue (MasterMind.Solution solution) (MasterMind.Guess guess) =
    let (blackScore, whiteScore, correct) = MasterMind.check solution guess in
        if blackScore == 4 then correct == True else correct == False

instance Arbitrary (MasterMind.Solution) where
  arbitrary = fmap (\r -> MasterMind.Solution r) $ randomValidRow

instance Arbitrary MasterMind.Guess where
  arbitrary = fmap (\r -> MasterMind.Guess r) $ randomValidRow

randomValidRow = vectorOf MasterMind.width (choose (1,MasterMind.colors))

main = do
        putStrLn $ "> QuickChecking " ++ (show $ length masterMindProperties) ++ " MasterMind properties"
        mapM_ quickCheck masterMindProperties
        putStrLn "QuickCheck done."
