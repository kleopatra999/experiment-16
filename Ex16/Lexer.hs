module Ex16.Lexer (
    LC, TokenParser(..), tp_ics, tp_parse, ws_tp,
    Lexer(..), build_lexer, lexer_lookup,
    Token(..), token_lc, token_src, token_def, LexError(..),
    parse_lex
) where

import qualified Data.Map as M

type LC = (Int, Int)

 -- The lexer looks up tokens by their first character.
data TokenParser = TPLit String
                 | TPCustom String (String -> Maybe (String, String))
tp_ics (TPLit lit) = [head lit]
tp_ics (TPCustom x _) = x
tp_parse (TPLit lit) = scan lit where
    scan [] rest = Just (lit, rest)
    scan (l:ls) (c:cs) | l == c = scan ls cs
    scan _ _ = Nothing
tp_parse (TPCustom _ x) = x

ws_tp = TPCustom ws span_ws where
    ws = " \t\n"
    span_ws str = case span (flip elem ws) str of
        ([], _) -> Nothing
        ret -> Just ret

 -- TODO: allow adding tokens while parsing
data Lexer a = Lexer (M.Map Char [(TokenParser, a)])
build_lexer :: [(TokenParser, a)] -> Lexer a
build_lexer tokendefs = let
    with_ics (tp, td) = map (\c -> (c, [(tp, td)])) (tp_ics tp)
    in Lexer (M.fromListWith (++) (concatMap with_ics tokendefs))
lexer_lookup c (Lexer map) = M.lookup c map

data Token a = Token LC String a deriving (Show)
token_lc (Token x _ _) = x
token_src (Token _ x _) = x
token_def (Token _ _ x) = x
data LexError = LexError LC deriving (Show)

parse_lex :: Lexer a -> String -> Either LexError [Token a]
parse_lex lexer str = l [] (1, 1) str where
    l acc lc [] = Right (reverse acc)
    l acc lc str@(c:_) = case lexer_lookup c lexer of
        Just bits -> try bits where
            try [] = Left $ LexError lc
            try ((tp, bit):bits) = case tp_parse tp str of
                Just (got, rest) -> let
                    newlc = scan_lc lc got
                    in l (Token lc got bit : acc) newlc rest
                Nothing -> try bits
        Nothing -> Left $ LexError lc
    scan_lc (line, col) [] = (line, col)
    scan_lc (line, col) ('\n':cs) = (succ line, 1)
    scan_lc (line, col) (_:cs) = (line, succ col)

