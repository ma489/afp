{- Fix (2.2.3) -}

module Fix where

-- Given:

fix :: (a -> a) -> a
fix f = f (fix f)

-- Define the function foldr as an application of fix to a term that is not recursive.

--generally: foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
--here constrained to lists: foldr' :: (a -> b -> b) -> b -> [a] -> b
-- but idea is the same

-- recursive definition
foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' _ initial [] = initial
foldr'' f initial (a:as) = foldr'' f (f a initial) as

-- fix-ified
foldr''' :: (a -> b -> b) -> b -> [a] -> b
foldr''' _ initial [] = initial
foldr''' f initial (a:as) = fix func f initial (a:as) -- func (fix func) f initial (a:as)...
                                where func _ _ i [] = i
                                      func h g i l = h g (g (head l) i) (tail l)
                                -- using parameter h to feed the function into itself
                                    -- making the recursion explicit
                                    -- (fix magic!)

--example
exampleFold :: Int
exampleFold = foldr''' (+) 0 [1,2,3,4,5] --15
