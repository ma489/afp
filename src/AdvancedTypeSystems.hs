{-# LANGUAGE RankNTypes #-} -- used later in the inline example
{-# LANGUAGE ScopedTypeVariables #-}

{- Higher-rank polymorphism -}

-- In standard Haskell, let-bound variables can be polymorphic, where as lambda-bound variables cannot (always monomorphic)
-- So, Haskell only allows you to use polymorphic functions if defined in let's, where's, or in a top-level definition
        -- and not if defined they were passed in through the argument of a surrounding lambda
-- Intuitively: you cannot overload a function unless you provide a explicit type signature, or it can be inferred
-- works fine in System F, but not in Haskell

-- let example: f is polymorphic (here operating on an Integer and a Char)
polymorphicLet :: (Int, Char)
polymorphicLet = let f = id in (f 3, f 'a')

-- where example: f is polymorphic (here operating on an Integer and a Char)
polymorphicWhere :: (Int, Char)
polymorphicWhere = (f 3, f 'a') where f = id

-- top-level example: f is polymorphic (here operating on an Integer and a Char)
f :: a -> a
f = \x -> x
polymorphicTopLevel :: (Int, Char)
polymorphicTopLevel = (f 3, f 'a')

-- inline lambda: negative example
-- the following would not work, because the type of f is undecidable, we need to give the compiler more type info (see positive example)
-- polymorphicInline :: (Int, Char)
-- polymorphicInline = (\f -> (f 3, f 'a')) id
-- note, this is not the same as: polymorphicInline = (\(a,b) -> (id a, id b)) (3,'a')

-- inline lambda: positive example
-- we provide more type info, by explicitly stating that f is of type a -> a, and indeed polymorphic (for any a)
-- this inline lambda is of type: (∀a.a → a) → (Int, Char)
    -- this is called a rank-2 type because the function argument is polymorphic
    -- if function argument of a function argument is poylmorphic then it is called a rank-3 type, and so forth
-- however, In Haskell, to allow a higher-order function (in this case our inline lambda) to accept a polymorphic function as an argument, then
    -- we need to use the RankNTypes and ScopedTypeVariables pragmas
    -- indeed, to allow arbitary rank types
polymorphicInline :: (Int, Char)
polymorphicInline = (\(f :: forall a. a -> a) -> (f 3, f 'a')) id
