module Ex16.Syntax (
    parser, parse
) where
import Ex16.TypeTypes
import qualified Ex16.Parser as P
import qualified Ex16.L1 as L1
import Data.Char
import Data.Maybe

parse_while :: (Char -> Bool) -> String -> Maybe (String, String)
parse_while test = f [] where
    finish acc s = if null acc then Nothing else Just (reverse acc, s)
    f acc [] = finish acc []
    f acc (c:cs) = if test c
        then f (c:acc) cs
        else finish acc (c:cs)

name_token :: String -> Maybe (String, String)
name_token [] = Nothing
name_token (c:cs) = if isAlpha c || c == '_'
    then parse_while isAlphaNum (c:cs)
    else Nothing

str_token :: String -> Maybe (String, String)
str_token ('\"':cs) = char "\"" cs where
    char acc [] = Nothing
    char acc ('\\':[]) = Nothing
    char acc ('\\':c:cs) = char (c:'\\':acc) cs
    char acc ('"':cs) = Just ('"' : reverse acc, cs)
    char acc (c:cs) = char (c:acc) cs
str_token _ = Nothing

ws_token = parse_while isSpace

eof_token :: String -> Maybe (String, String)
eof_token [] = Just ([], [])
eof_token _ = Nothing

parser :: P.Parser () L1.Transformer ()
parser = foldl (flip ($)) (P.empty ()) [
    P.insert_token "_eof" eof_token Nothing,
    P.insert_token "_name" name_token (Just (const ())),
    P.insert_token "_str" str_token (Just (const ())),
    P.insert_token "_ws" ws_token Nothing,
    P.insert_ignore "_ws" 0.0 P.ANon,
    P.insert "_ @ _" 10.0 P.ARight (const L1.tr_call),
    P.insert "( _ )" 0.0 P.ANon (const L1.tr_parens),
    P.insert "{ _ }" 0.0 P.ANon (const L1.tr_block),
    P.insert "\\ _ -> _" 12.0 P.ARight (const L1.tr_lambda),
    P.insert "_ : _" 11.0 P.ARight (const L1.tr_constraint),
    P.insert "_ . _" 1.0 P.ALeft (const L1.tr_method),
    P.insert "_ = _" 13.0 P.ARight (const L1.tr_bind),
    P.insert "_str" 0.0 P.ANon (const L1.tr_str),
    P.insert "_name" 0.0 P.ANon (const L1.tr_name),
    P.insert "_ _eof" (-1.0/0.0) P.ANon (const L1.tr_block),
    id ]

instance Show (a -> b) where
    show _ = "<function>"

parse :: String -> Either (P.Error () L1.Transformer ()) L1.L1
parse str = do
    (tree, []) <- P.parse parser (fromJust (P.lookup "_ _eof" parser)) str
    return $ L1.trans tree
