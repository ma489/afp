{- Type classes and containers -}

module TypeClassesAndContainers where

-- rose tree: an example container-like data structure
data Rose a = a :> [Rose a] deriving Show

exampleRoseTree :: Rose Int
exampleRoseTree = 1 :> [2 :> [], 3 :> [4 :> []]]

{- Functor -}

-- Define a functor instance for this rose tree, with its corresponding fmap,
    --  so that we can uniformly apply a function to all elements of the tree (like map for lists, but generalised)
instance Functor Rose where
    fmap f (a :> children) = f a :> map (fmap f) children

{- Monoids -}

-- Monoids: algebraic data structure over a type, m,
    -- with a single associative binary operator, mappend :: m -> m -> m
    -- and an identity element, mempty :: m

-- Numbers form a monoid both under addition with 0 as id element, and under multiplication with 1 as id element
-- However, Haskell only allows one instance per combination of type and type class, so a workaround is to create
    -- some newtype wrappers
newtype Sum a = Sum { unSum :: a }
newtype Product a = Product { unProduct :: a }

-- addition
instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum n1 `mappend` Sum n2 = Sum (n1 + n2)

-- multiplication
instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product n1 `mappend` Product n2 = Product (n1 * n2)

exampleSum :: Int
exampleSum = unSum $ Sum 1 `mappend` Sum 2

exampleProduct :: Int
exampleProduct = unProduct $ Product 2 `mappend` Product 3

{- Foldable -}

-- Define an instance of Foldable so that we can fold all the elements of our container-like data structure
    -- where the elements are of some type that forms a Monoid
    -- into a single element of that Monoidal type

instance Foldable Rose where
    foldMap f (a :> []) = f a
    foldMap f (a :> children) = f a `mappend` foldr mappend mempty childrenAsListOfMonoids
                                    where childrenAsListOfMonoids = map (foldMap f) children

fsum :: (Foldable f, Num a) => f a -> a
fsum container = unSum $ foldMap Sum container

fproduct :: (Foldable f, Num a) => f a -> a
fproduct container = unProduct $ foldMap Product container

--examples
main :: IO ()
main = do
        putStrLn $ "Example rose tree: " ++ show exampleRoseTree
        putStrLn $ "Sum: " ++ show (fsum exampleRoseTree)
        putStrLn $ "Product: " ++ show (fproduct exampleRoseTree)
