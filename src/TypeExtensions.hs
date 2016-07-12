{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

{- Type extensions: GADT (Generalized Abstract Datatype) (2.6.1) -}

-- GADTs allow us to describe more precise types
    -- thus, the below constructors can target more specific 'subtypes'
    -- e.g. not just Contract a, but also Contract (a -> b)
data Contract :: * -> * where -- a GADT of contracts
    Pred :: (a -> Bool) -> Contract a -- a predicate for a value of arbitrary type
    Fun :: Contract a -> Contract b -> Contract (a -> b) -- contract functions (pre-condition on argument, post-condition on results)
    DFun :: Contract a -> (a -> Contract b) -> Contract (a -> b) -- generalised Fun (where we care about the argument in the post-condition)

assert :: Contract a -> a -> a
assert (Pred p) x = if p x then x else error "contract violation"
assert (Fun pre post) f = assert post . f . assert pre -- we know f is a function from the Fun constructor
assert (DFun pre post) f = \x -> assert (post x) . f $ assert pre x

(~>) :: Contract a -> Contract b -> Contract (a -> b)
(~>) (Pred p) (Pred q) = DFun (Pred p) (\x -> Pred q) -- express Fun in terms of DFun

--example contract

pos :: (Num a, Ord a) => Contract a
pos = Pred (>0)

examplePosValid :: Int
examplePosValid = assert pos 2

examplePosInvalid :: Int
examplePosInvalid = assert pos (-1)

-- some more contracts

true :: Contract a
true = Pred (const True)

nonEmptyList :: Contract [a]
nonEmptyList = Pred ((>0) . length)

nonNegative :: Contract Int
nonNegative = Pred (> (-1))

resultIsNonNegative :: Contract ([a] -> Int)
resultIsNonNegative = Fun true nonNegative

elementOfOriginalList :: Eq a => [a] -> Contract a
elementOfOriginalList list = Pred (flip elem list)

indexLessThanLengthAndResultIsElementOfOriginalList :: Eq a => [a] -> Int -> Contract a
indexLessThanLengthAndResultIsElementOfOriginalList list index = Pred (flip elem list)

lengthOfValidList :: Int
lengthOfValidList = assert resultIsNonNegative length ['a','b'] -- obvious; if result of length is non-negative, return length

--checks first argument (list) and result (char)
listIndexContract :: Eq a => Contract ([a] -> a)
listIndexContract = DFun nonEmptyList elementOfOriginalList

failableListIndex :: Eq a => [a] -> Int -> a
failableListIndex list index = assert listIndexContract (!! index) list

-- checks first argument (list) and second argument (index) and result (char)
listIndexContractStricter :: Eq a => Contract ([a] -> (Int -> a))
listIndexContractStricter = DFun nonEmptyList (\list -> DFun nonNegative (\index -> indexLessThanLengthAndResultIsElementOfOriginalList list index))

failableListIndexStricter :: Eq a => [a] -> Int -> a
failableListIndexStricter list index = assert listIndexContractStricter (!!) list index

--examples
validIndex :: Char
validIndex = failableListIndexStricter ['a'] 0 -- 'a'

errorNegativeIndex :: Char
errorNegativeIndex = failableListIndexStricter ['a'] (-1) --error

errorEmptyList :: Char
errorEmptyList = failableListIndexStricter [] 0 --error

errorIndexTooLarge :: Char
errorIndexTooLarge = failableListIndexStricter [] 1 --error

-- and another contract

preserves :: (Eq a, Eq b) => (a -> b) -> Contract (a -> a)
preserves p = DFun true (\x -> Pred (\fx -> (p fx) == (p x)))

examplePreserves1 :: String
examplePreserves1 = assert (preserves length) reverse "Hello" -- "olleH"

examplePreserves2 :: String
examplePreserves2 = assert (preserves length) (take 5) "Hello" -- "Hello"

examplePreservesInvalid :: String
examplePreservesInvalid = assert (preserves length) (take 5) "Hello world" -- error
