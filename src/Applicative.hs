{- Applicative (Haskell book) -}

import Control.Applicative
import Data.Monoid

f :: Num a => [a -> a]
f = [\x -> x*x, \x -> x+x]

list :: [Int]
list = [1,2,3,4,5]

ex1 = f <*> list
ex2 = (\x -> x*x*x) <$> list
ex3 = Just (*2) <*> Just 3
ex4 = Nothing <*> Just 2
ex5 = ("Woo", (+1)) <*> (" Hoo!",0)
ex6 = ((Sum 2), (+1)) <*> ((Sum 3), 0) -- mappends a1 to a2 and applies b1 to b2
                                        -- two-tuple is a functor, and its a value is a monoid
ex7 = max <$> [1,2] <*> [1,4]
ex8 = pure max <*> [1,2] <*> [1,4]
ex9 = (\a b -> True) <$> [1,2] <*> [3,4]
