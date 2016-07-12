{- Fix (2.2.3) -}

-- Given:

fix :: (a -> a) -> a
fix f = f (fix f)

-- Define the function foldr as an application of fix to a term that is not recursive.

foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr' f initial foldable = undefined
