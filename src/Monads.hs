{- An extension of the Control.Monad.State monad -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Control.Monad.State

--Definition
data StateMonadPlus s a = StateMonadPlus {runState :: s -> (s, a)}

instance Functor (StateMonadPlus s) where
    fmap f (StateMonadPlus m) = StateMonadPlus (innerFunction m f)
                                    where innerFunction m f = \s -> let (s',a) = m s
                                                                        b = f a
                                                                    in (s',b)

instance Monad (StateMonadPlus s) where
    (>>=) (StateMonadPlus f) g = StateMonadPlus (innerFunction f g)
                                    where innerFunction f g = \s -> let (s',a) = f s
                                                                        (StateMonadPlus h) = g a
                                                                    in h s'
    return a = StateMonadPlus (\s -> (s,a))
    fail = undefined --Feature 2: Failure

instance Applicative (StateMonadPlus s) where
    (<*>) = ap
    pure = return
    -- f (a -> b) -> f a -> f b
    -- (s -> (s,a -> b)) <*> (s -> (s,a)) = s -> (s,b)

instance MonadState s (StateMonadPlus s) where
    get = StateMonadPlus (\s -> (s,s))
    put s = StateMonadPlus (\_ -> (s,()))

runStateMonadPlus :: StateMonadPlus s a -> s -> Either String (a, s)
runStateMonadPlus = undefined --for running the monad

--Feature 1: Diagnostics
diagnostics :: StateMonadPlus s String
diagnostics = undefined

annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate = undefined

diagnosticsTest :: StateMonadPlus s String
diagnosticsTest = do return 3 >> return 4
                     return 5
                     diagnostics

annotateTest :: StateMonadPlus s String
annotateTest = do annotate "A" (return 3 >> return 4)
                  return 5
                  diagnostics


--Feature 3: History of states
class MonadState s m => StoreState s m | m -> s where
saveState :: m ()
saveState = undefined
loadState :: m ()
loadState = undefined
