{- Church Encondings -}

--required for higher-rank polymorphism (see use of 'forall' in definition of ChurchNat)
{-# LANGUAGE RankNTypes #-}

-- Church numerals: representation of natural numbers under Church encoding
-- rank-2 type because the function argument is polymorphic
data ChurchNat = ChurchNat (forall a. (a -> a) -> a -> a)

instance Show ChurchNat where
    show n = "{ " ++ (show $ churchToInt n) ++ " , " ++ (show $ churchToString n) ++ " }"

-- operations
churchToInt :: ChurchNat -> Int
churchToInt (ChurchNat f) = f (+1) 0

churchToString :: ChurchNat -> String
churchToString (ChurchNat f) = f ("f("++) "x"

succ_ :: ChurchNat -> ChurchNat
succ_ (ChurchNat n) = ChurchNat (\f x -> f (n f x))

addition :: ChurchNat -> ChurchNat -> ChurchNat
addition (ChurchNat g) (ChurchNat h) = ChurchNat (\f x -> g f (h f x))

multiplication :: ChurchNat -> ChurchNat -> ChurchNat
multiplication (ChurchNat g) (ChurchNat h) = ChurchNat (\f x -> g (h f) x)

exponentiation :: ChurchNat -> ChurchNat -> ChurchNat
exponentiation (ChurchNat g) (ChurchNat h) = ChurchNat (\f x -> (h g) f x)

--examples
zero :: ChurchNat
zero = ChurchNat (\f x -> x)

one :: ChurchNat
one = ChurchNat (\f x -> f x)

two :: ChurchNat
two =  ChurchNat (\f x -> f (f x))

three :: ChurchNat
three = succ_ two

four :: ChurchNat
four = multiplication two two

five :: ChurchNat
five = addition two three

eight :: ChurchNat
eight = exponentiation two three
