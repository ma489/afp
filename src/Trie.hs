{- Trie (2.2.4) -}

module Trie where

import Data.Map
import Data.Maybe

-- trie datatype, aka prefix tree
data Trie a = Node (Maybe a) (Map Char (Trie a)) deriving (Eq, Show)

-- Invariant: if a trie (or subtrie) is empty, i.e., if it contains no values, it should always be represented by:
empty' :: Trie a
empty' = Node Nothing Data.Map.empty

-- these other functions maintain the above invariant

valid' :: Eq a => Trie a -> Bool
valid' trie = not (null' trie && notRepresentedByEmpty)
                where notRepresentedByEmpty = trie /= empty'

-- if contains no values (i.e. all Nothing's and no Just's)
null' :: Trie a -> Bool
null' (Node (Just x) children) = False
null' (Node Nothing children) = if (Data.Map.null children) then True else any (==True) $ Prelude.map null' $ Prelude.map (\(a,b) -> b) $ toList children

insert' :: String -> a -> Trie a -> Trie a
insert' [] _ trie = trie
insert' [k] v (Node x children) = Node x (Data.Map.insert k (Node (Just v) Data.Map.empty) children)
insert' (k:ey) value (Node x children) = case Data.Map.lookup k children of Nothing -> (Node x updatedChildren)
                                                                                where updatedChildren = Data.Map.insert k nt children
                                                                                      nt = insert' ey value empty'
                                                                            (Just childTrie) -> Node x (Data.Map.insert k newTree children)
                                                                                                    where newTree = insert' ey value childTrie

lookup' :: String -> Trie a -> Maybe a
lookup' [] (Node Nothing _) = Nothing
lookup' [] (Node (Just v) _) = Just v
lookup' (k:ey) (Node x children) = case Data.Map.lookup k children of Nothing -> x
                                                                      (Just childTrie) -> lookup' ey childTrie

--maintain the invariant
delete' :: String -> Trie a -> Trie a
delete' [] trie = if null' trie then empty' else trie
delete' [k] (Node v children) = let prunedTree = (Node v (Data.Map.delete k children)) in
                                    if null' prunedTree then empty' else prunedTree
delete' (k:ey) (Node v children) = case Data.Map.lookup k children of Nothing -> let t = (Node v children) in
                                                                                    if null' t then empty' else t
                                                                      (Just child) -> let t = (Node v (Data.Map.insert k (delete' ey child) children)) in
                                                                                        if null' t then empty' else t


-- examples
exampleTrieEmpty :: Trie Int
exampleTrieEmpty = empty'

exampleTrie1Valid :: Trie Int
exampleTrie1Valid = Node (Just 1) (fromList [('a', empty')])

exampleTrie2Valid :: Trie Int
exampleTrie2Valid = Node (Just 1) (fromList [])

exampleTrie3Valid :: Trie Int
exampleTrie3Valid = Node (Just 1) Data.Map.empty

exampleTrie4Valid :: Trie Int
exampleTrie4Valid = Node Nothing (fromList [('b', node2), ('f', node3)])
                        where node2 = Node Nothing (fromList [('a', node4)])
                              node4 = Node Nothing (fromList [('r', node5), ('z', node6)])
                              node5 = Node (Just 2) Data.Map.empty
                              node6 = Node (Just 3) Data.Map.empty
                              node3 = Node (Just 0) (fromList [('o', node7)])
                              node7 = Node Nothing (fromList [('o', node8)])
                              node8 = Node (Just 1) Data.Map.empty

exampleTrie5Invalid :: Trie Int
exampleTrie5Invalid = Node Nothing (fromList [('a', node2), ('b', node3)])
                        where node2 = Node Nothing Data.Map.empty
                              node3 = Node Nothing Data.Map.empty

-- TODO: move to tests

exampleTrie :: Trie Int
exampleTrie = insert' "foo" 1 $ insert' "bar" 2 $ insert' "baz" 3 empty'

lookupFoo :: Maybe Int
lookupFoo = lookup' "foo" exampleTrie -- 1

lookupBar :: Maybe Int
lookupBar = lookup' "bar" exampleTrie -- 2

lookupBaz :: Maybe Int
lookupBaz = lookup' "baz" exampleTrie -- 3

deleteThenLookupBaz :: Maybe Int
deleteThenLookupBaz = lookup' "baz" $ delete' "baz" exampleTrie -- Nothing

deleteNonExistingKey = exampleTrie == (delete' "nonexistent" exampleTrie) -- True

deleteAndKeepInvariant = (delete' "foo" $ delete' "bar" $ delete' "baz" exampleTrie) == empty' -- True
