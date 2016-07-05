{-
    The λ-calculus, Church, 1932
    - Rewriting system and simplistic programming language
    - Supports higher-order functions naturally
    - Turing complete
-}

module UntypedLambdaCalculus where

import Data.List
import qualified Data.Map.Strict as Map

{- Grammar -}

type VariableName = String

data Expression = Variable VariableName
                  | Constant Int
                  | Application Expression Expression
                  | Abstraction VariableName Expression
                  -- deriving (Eq)

--for pretty-printing
instance Show Expression where
    show (Variable v) = filter (/='"') $ show v
    show (Constant v) = show v
    show (Application e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    show (Abstraction v e) = filter (/='"') ("λ" ++ show v ++ "." ++ show e)

type Context = Map.Map VariableName Expression

{- Operations on these expression -}

freeVariables :: Expression -> [VariableName]
freeVariables (Variable v) = [v]
freeVariables (Application e1 e2) = freeVariables e1 `union` freeVariables e2
freeVariables (Abstraction v e) = freeVariables e \\ [v]

allVariables :: Expression -> [VariableName]
allVariables (Variable v) = [v]
allVariables (Application e1 e2) = allVariables e1 `union` allVariables e2
allVariables (Abstraction v e) = v : allVariables e

normalForm :: Expression -> Expression
normalForm e = case betaReduce e of Nothing -> e
                                    Just reducedE -> normalForm reducedE

--β-reduction is the central rewrite rule
betaReduce :: Expression -> Maybe Expression
betaReduce (Variable _) = Nothing
betaReduce (Abstraction _ _) = Nothing
betaReduce (Application t1 t2) = Just (substitute t1 t2)

substitute :: Expression -> Expression -> Expression
substitute = undefined --TODO

evaluate :: Expression -> Context -> Expression
evaluate (Variable v) context = context Map.! v
evaluate (Abstraction v e) _ = Abstraction v e --evaluate in context?
evaluate (Application e1 e2) context = substitute (evaluate (normalForm e2) context) (evaluate (normalForm e1) context)

{- Examples -}

variable :: Expression
variable = Variable "x"

sampleContext :: Context
sampleContext = Map.fromList [("x", Constant 1)]

abstraction :: Expression
abstraction = Abstraction "x" (Variable "x") -- identity

application :: Expression
application = Application (Abstraction "a" (Variable "a")) (Variable "x")

-- λx.λy.x
true :: Expression
true = Abstraction "t" (Abstraction "f" (Variable "t"))

-- λx.λy.y
false :: Expression
false = Abstraction "t" (Abstraction "f" (Variable "f"))