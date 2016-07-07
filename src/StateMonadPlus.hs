{- An extension of the Control.Monad.State monad -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module StateMonadPlus (
    StateMonadPlus(..),
    put,
    get,
    diagnostics,
    annotate,
    runStateMonadPlus
  ) where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Maybe

--Definition

--using newtype rather than data since there is only one constructor
type CommandStore = Map.Map String Int
newtype StateMonadPlus s a = StateMonadPlus {runState :: (s, CommandStore) -> (s, a, CommandStore)}

instance Functor (StateMonadPlus s) where
    fmap f (StateMonadPlus m) = StateMonadPlus (\(s,c) -> let (s',a,c) = m (s,c)
                                                              b = f a
                                                          in (s',b,c))

instance Monad (StateMonadPlus s) where
    (>>=) (StateMonadPlus f) g = StateMonadPlus (\(s,c) -> let (s',a,c') = f (s,c)
                                                               (StateMonadPlus h) = g a
                                                               c'' = Map.insertWith (+) "bind" 1 c'
                                                           in h (s',c''))
    -- could implement >> in terms of >>= ?
    (>>) (StateMonadPlus f) (StateMonadPlus g) = StateMonadPlus (\(s,c) -> let (s',a,c') = f (s,c)
                                                                               (s'',a',c'') = g (s',c')
                                                                               c''' = Map.insertWith (+) "then" 1 c''
                                                                           in (s', a', c'''))
    return a = StateMonadPlus (\(s,c) -> (s, a, (Map.insertWith (+) "return" 1 c)))
    fail message = undefined --TODO

instance Applicative (StateMonadPlus s) where
    (<*>) = ap
    pure = return

instance MonadState s (StateMonadPlus s) where
    get = StateMonadPlus (\(s,c) -> (s,s,(Map.insertWith (+) "get" 1 c)))
    put s = StateMonadPlus (\(_,c) -> (s,(),(Map.insertWith (+) "put" 1 c)))

runStateMonadPlus :: StateMonadPlus s a -> s -> Either String (a, s) --for running the monad
runStateMonadPlus computationInStateMonad initialState = let
                                                            (initial, commands) = (initialState, Map.empty)
                                                            (s,a,c) = StateMonadPlus.runState computationInStateMonad (initial, commands)
                                                         in
                                                            Right (a,s)

--diagnostics
diagnostics :: StateMonadPlus s String
diagnostics = StateMonadPlus (\(s,c) -> (s, diagnosticsString c, (Map.insertWith (+) "diagnostics" 1 c)))
                --could do better tbh
                where diagnosticsString c = "[" ++ (commandsList c) ++ "diagnostics=" ++ (diagnosticsCount c) ++ "]"
                      commandsList c = foldl (\b (a,d) -> a++"="++(show d)++","++b) "" (Map.toList c)
                      diagnosticsCount c = show $ (+1) $ fromMaybe 0 $ Map.lookup "diagnostics" c

annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate annotation (StateMonadPlus f) = StateMonadPlus (\(s,c) -> let (s', a, c') = f (s,c) in
                                                                      (s', a, (Map.insertWith (+) annotation 1 c')))

--History of states
class MonadState s m => StoreState s m | m -> s where
saveState :: m ()
saveState = undefined --TODO
loadState :: m ()
loadState = undefined --TODO


--examples

--get : set the result value to the state, and leave the state unchanged
getState :: Integer -> (Integer, Integer, CommandStore)
getState initialValue = StateMonadPlus.runState get (initialValue, Map.empty)

--put: set the result value to () and set the state value
putInState :: Integer -> (Integer, (), CommandStore)
putInState x = StateMonadPlus.runState (put x) (0, Map.empty)

trackCommandCall :: (Integer, Integer, CommandStore)
trackCommandCall = StateMonadPlus.runState (return 1) (0,Map.empty)

updateAndCheckState :: (Integer, Integer, CommandStore)
updateAndCheckState = StateMonadPlus.runState (get >>= \s -> put (s+2) >> get) (0, Map.empty)

trackMultipleCommandCalls :: (Integer, Integer, CommandStore)
trackMultipleCommandCalls = StateMonadPlus.runState (get >> return 1 >> return 2 >> get) (0,Map.empty)

runDiagnosticsTest :: (Integer, String, CommandStore)
runDiagnosticsTest = StateMonadPlus.runState diagnosticsTest (0, Map.empty)

diagnosticsTest :: StateMonadPlus Integer String
diagnosticsTest = do return 3 >> return 4
                     return 5
                     diagnostics

runAnnotationTest :: (Integer, String, CommandStore)
runAnnotationTest = StateMonadPlus.runState annotateTest (0, Map.empty)

annotateTest :: StateMonadPlus Integer String
annotateTest = do annotate "A" (return 3 >> return 4)
                  return 5
                  diagnostics

doSomeStuff :: Either String (Integer, Integer)
doSomeStuff = runStateMonadPlus (get >>= \x -> return (3+x) >>= \s -> put (s+4) >> get) 1
