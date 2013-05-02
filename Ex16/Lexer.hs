{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Ex16.Lexer (
    Tokenizer(..), Lexer, empty, null,
    insert, insertWith, delete, update,
    lookup_custom, insert_custom, delete_custom, adjust_custom,
) where

import qualified Data.Map as M
import qualified Ex16.Trie as T
import Prelude hiding (null, lex, read)
import Control.Monad
import Debug.Trace

class CustomToken c where
    custom_read :: c -> String -> Maybe (String, String)

data Lexer l c = Lexer (T.Trie Char l) [(String, c)]
               deriving (Show)

empty = Lexer T.empty []
null (Lexer trie []) = T.null trie
null _ = False

insert :: String -> l -> Lexer l c -> Lexer l c
insert name dat (Lexer trie cust) = Lexer (T.insert name dat trie) cust

insertWith :: (l -> l -> l) -> String -> l -> Lexer l c -> Lexer l c
insertWith f name dat (Lexer trie cust) = Lexer (T.insertWith f name dat trie) cust

delete :: String -> Lexer l c -> Lexer l c
delete name (Lexer trie cust) = Lexer (T.delete name trie) cust

update :: (l -> Maybe l) -> String -> Lexer l c -> Lexer l c
update f name (Lexer trie cust) = Lexer (T.update f name trie) cust

lookup_custom :: String -> Lexer l c -> Maybe c
lookup_custom name (Lexer trie cust) = lookup name cust

insert_custom :: String -> c -> Lexer l c -> Lexer c
insert_custom name dat (Lexer trie cust) = Lexer trie ((name, dat) : cust)

delete_custom :: String -> Lexer l c -> Lexer l c
delete_custom name (Lexer trie cust) = Lexer trie (filter ((/= name) . fst) cust)

adjust_custom :: (c -> c) -> String -> Lexer l c -> Lexer l c
adjust_custom f name (Lexer trie cust) = Lexer trie (map g cust) where
    g c = if fst c == name then (fst c, f (snd c)) else c

read :: CustomToken c => Lexer l c -> Maybe (Either l c, String, String)
read (Lexer trie cust) str = literal `mplus` custom where
    literal = do
        (res, got, rest) <- T.read trie str of
        return (Left res, got, rest)
    custom = msum (map try cust)
    try (name, tok) = do
        (got, rest) <- custom_read tok str
        return (Right tok, got, rest)
