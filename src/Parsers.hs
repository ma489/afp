{- Parser (Hutton) -}

import Data.Char

-- represent a parser as a function from a String to a list of tuples of some parsed-value and
    -- the remaining unconsumed part of the input String
type Parser a = String -> [(a, String)]

-- some basic parsers

-- 'return' v' always succeeds, and returns the value v without consuming the inputString
-- deliberately moved the inputString parameter into the function body, to make it explicit that these are functions
return' :: a -> Parser a
return' v = \input -> [(v,input)] --output is input

-- 'failure' always fails; dual to return'
fail' :: Parser a
fail' = \input -> []

-- item (fails on empty string, returns first character otherwise)
item :: Parser Char
item = \input -> case input of
                   [] -> []
                   (x:xs) -> [(x,xs)] --output is remainder

--apply a parser to some input
parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp

--examples
ex1 = parse (return' 1) "abc" -- [(1,"abc")]
ex2 = parse fail' "abc" -- []
ex3 = parse item "abc" -- ['a',"bc"]

-- adding sequencing operation (a combinator!)
then' :: Parser a -> (a -> Parser b) -> Parser b
p `then'` f = \inp -> case parse p inp of
                        [] -> []
                        [(result, out)] -> parse (f result) out

-- another example
chainedParser :: Parser (Char, Char)
chainedParser = item `then'` (\s -> item `then'` \q -> item `then'` \r -> return' (s,r))
ex4 = parse chainedParser "abcdef" -- [( ('a','c'), "def" )]

-- another combinator, choice
orElse :: Parser a -> Parser a -> Parser a
p `orElse` q = \inp -> case parse p inp of
                        [] -> parse q inp -- run q if p fails
                        [(result, out)] -> [(result, out)] -- if p succeeds, use it

-- more examples
orElseParserFirstSucceeds = item `orElse` (return' 'd')
ex5 = parse orElseParserFirstSucceeds "abcdef"

orElseParserFirstFailsUseSecond = fail' `orElse` (return' 'd')
ex6 = parse orElseParserFirstFailsUseSecond "abcdef"

orElseParserBothFail = fail' `orElse` fail'
ex7 = parse orElseParserBothFail "abcdef"

-- derived primitives
sat :: (Char -> Bool) -> Parser Char
sat predicate = item `then'` (\x -> if predicate x then return' x else fail')

-- using some predicates from Data.Char...
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

--more examples
ex8 = parse digit "123" -- [('1', "23")]
ex9 = parse digit "abc" -- []
ex10 = parse (char 'a') "abc" -- [('a', "bc")]
ex11 = parse (char 'a') "bbc" -- []

-- a string parser
string :: String -> Parser String
string [] = return' []
string (x:xs) = (char x) `then'` \s -> string xs `then'` \t -> (return' (x:xs))

--example
ex12 = parse (string "abc") "abcdef" -- [("abc", "def")]
ex13 = parse (string "xyz") "abcdef" -- []

many :: Parser a -> Parser [a]
many p = many1 p `orElse` (return' [])

many1 :: Parser a -> Parser [a]
many1 p = p `then'` \q -> many p `then'` \r -> return' (q:r)

ex14 = parse (many digit) "123abc" -- [("123", "abc")]  -- String is [Char]
ex15 = parse (many digit) "abc" -- [("", "abc")]
ex16 = parse (many1 digit) "abcdef" -- []

ident :: Parser String
ident = lower `then'` \x -> many alphanum `then'` \xs -> return' (x:xs)

nat :: Parser Int
nat = many1 digit `then'` \xs -> return' (read xs)

space :: Parser ()
space = many (sat isSpace) `then'` \_ -> return' ()

ex17 = parse ident "abc def" -- [("abc", "def")]
ex18 = parse nat "123 abc" -- [(123, "abc")]
ex19 = parse space "      abc" -- [((), "abc")]

token :: Parser a -> Parser a
token p = space `then'` \_ -> p `then'` \v -> space `then'` \_ -> return' v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

p :: Parser [Int]
p = (symbol "[") `then'` \_  -> natural `then'` \n -> (many (symbol "," `then'` \_ -> natural)) `then'` \ns -> (symbol "]") `then'` \_ -> (return' (n : ns))

ex20 = parse p "[1,2,3,4]"
ex21 = parse p "    [  1 , 2 , 3] "
ex22 = parse p ""
ex23 = parse p "[]"
