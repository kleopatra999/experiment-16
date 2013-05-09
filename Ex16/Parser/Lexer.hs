module Ex16.Parser.Lexer (
    Reader, Token(..), Lexer, empty, null, lits, customs, custom, alter, lex
) where

import qualified Ex16.Parser.Trie as T
import Prelude hiding (lex, null)
import qualified Data.List as L
import Control.Monad

type Reader = String -> Maybe Int  -- Just return length parsed
data Token t = Token Reader (Maybe t)
instance Show t => Show (Token t) where
    show (Token _ t) = "<Token " ++ showsPrec 11 t ">"

data Lexer t a = Lexer (T.Trie Char a) [(String, (Token t, a))]
               deriving (Show)
lits    (Lexer x _) = x
customs (Lexer _ x) = x

empty :: Lexer t a
empty = Lexer T.empty []
null :: Lexer t a -> Bool
null (Lexer lits cust) = T.null lits && L.null cust

custom :: String -> Token t -> a -> Lexer t a -> Lexer t a
custom name tok base (Lexer lits cust) = Lexer lits ((name, (tok, base)) : cust)

list_update :: (a -> Maybe a) -> String -> [(String, a)] -> [(String, a)]
list_update f name [] = []
list_update f name ((xn, xv) : xs) = if name == xn
    then case f xv of
        Just xv -> (xn, xv) : xs
        Nothing -> xs
    else (xn, xv) : list_update f name xs

alter :: (Maybe a -> Maybe a) -> String -> Lexer t a -> Lexer t a
alter f name (Lexer lits cust) = let
    g (xn, xv) = f (Just xv) >>= \xv -> Just (xn, xv)
    in case lookup name cust of
        Just _ -> Lexer lits (list_update g name cust)
        Nothing -> Lexer (T.alter f name lits) cust

lex :: Lexer t a -> String -> Maybe (Maybe t, a, Int)
lex (Lexer lits cust) str = lex_lits `mplus` lex_cust where
    lex_lits = do
        (dat, len) <- T.read lits str
        guard (len > 0)
        return (Nothing, dat, len)
    lex_cust = let
        one (name, (Token reader tdat, dat)) = do
            len <- reader str
            return (tdat, dat, len)
        in msum (map one cust)

