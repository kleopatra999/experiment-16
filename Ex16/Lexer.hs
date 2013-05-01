{-# LANGUAGE FlexibleInstances #-}
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

class Tokenizer c where
    read :: c t -> String -> Maybe (t, String, String)

instance Tokenizer (T.Trie Char) where
    read = T.read

data Lexer c t = Lexer (T.Trie Char t) [(String, c t)]
               deriving (Show)

empty = Lexer T.empty []
null (Lexer t []) = T.null t
null _ = False

insert :: String -> t -> Lexer c t -> Lexer c t
insert name dat (Lexer trie cust) = Lexer (T.insert name dat trie) cust

insertWith :: (t -> t -> t) -> String -> t -> Lexer c t -> Lexer c t
insertWith f name dat (Lexer trie cust) = Lexer (T.insertWith f name dat trie) cust

delete :: String -> Lexer c t -> Lexer c t
delete name (Lexer trie cust) = Lexer (T.delete name trie) cust

update :: (t -> Maybe t) -> String -> Lexer c t -> Lexer c t
update f name (Lexer trie cust) = Lexer (T.update f name trie) cust

lookup_custom :: String -> Lexer c t -> Maybe (c t)
lookup_custom name (Lexer trie cust) = lookup name cust

insert_custom :: String -> c t -> Lexer c t -> Lexer c t
insert_custom name dat (Lexer trie cust) = Lexer trie ((name, dat) : cust)

delete_custom :: String -> Lexer c t -> Lexer c t
delete_custom name (Lexer trie cust) = Lexer trie (filter ((/= name) . fst) cust)

adjust_custom :: (c t -> c t) -> String -> Lexer c t -> Lexer c t
adjust_custom f name (Lexer trie cust) = Lexer trie (map g cust) where
    g c = if fst c == name then (fst c, f (snd c)) else c

instance Tokenizer c => Tokenizer (Lexer c) where
    read (Lexer trie cust) str =
        T.read trie str `mplus`
        foldl ((. (flip read str) . snd) . mplus) Nothing cust
