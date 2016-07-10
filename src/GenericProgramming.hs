
{- Generics in Haskell (datatype-generic programming) -}

{-# LANGUAGE DeriveDataTypeable #-} -- required for SYB
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GenericProgramming where

import Data.Generics (Data, gshows) -- SYB
import GHC.Generics -- this and the below are used by Generic Deriving
import qualified Generics.Deriving.Show as GenDer

-- custom data type
data MyType = ThisGuy { someField ::  Bool }
                -- deriving Show  [want to avoid using the 'default' Show class]
                deriving (Data,   -- required for SYB
                          Generic,
                          GenDer.GShow) -- required for generic-deriving

-- some value to use in examples
x :: MyType
x = ThisGuy True

{- 1 - manual approach
    Defining my own type class for show-able things.
    It would be tedious to do so for all types -}

class ShowG a where
    showMyGeneric :: a -> String

instance ShowG MyType where
    showMyGeneric (ThisGuy innerBool) = "ThisGuy (" ++ showMyGeneric innerBool ++ ")"

instance ShowG Bool where
    showMyGeneric True = "True"
    showMyGeneric False = "False"

{- 2 - using SYB
    simply derive Data -}

showSYB :: Data a => a -> String
showSYB thingToShow = gshows thingToShow ""

{- 3 - Using generic-deriving -}

showGenericDeriving :: (GenDer.GShow a) => a -> String
showGenericDeriving thingToShow = GenDer.gshow thingToShow

{- examples -}

main :: IO ()
main = do
        putStr "1 > Showing using my ShowG typeclass: "
        putStrLn $ showMyGeneric x
        putStr "2 > Showing using SYB: "
        putStrLn $ showSYB x
        putStr "3 > Showing using Generic Deriving: "
        putStrLn $ showGenericDeriving x
