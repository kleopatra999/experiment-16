module Syntax (
) where
import Ex16.TypeTypes
import Ex16.Types
import Ex16.Parser
import Ex16.L1
import Data.Char

parse_while :: (Char -> Bool) -> String -> Maybe (String, String)
parse_while test = f [] where
    f acc [] = Just (reverse acc, [])
    f acc (c:cs) = if test c
        then f (c:acc) cs
        else Just (reverse acc, c:cs)

syntax_tokens :: [(String, TokenParser)]
syntax_tokens = [
    ("_int", TokenParser "0123456789" (parse_while isDigit)),
    ("_name", TokenParser (['a'..'z'] ++ ['A'..'Z'] ++ "_") (parse_while isAlNum)),
    ("_ws", TokenParser " \t\n" (parse_while isSpace))
    ]

pat = Pat . words
spat w tr = pat w undefined undefined tr

syntax_patterns :: [Pat PT_To_L1]
syntax_patterns = [
    spat "( _ )" (p1_to_l1 L1Parens),
    spat "{ _ }" (p1_to_l1 L1Block),
    pat "_ , _" 1.0 AList (p1_to_l1 L1Group),
    pat "_ = _" 2.0 ANon (p1_to_l1 L1Bind),
    pat "_ . _" 10.0 ALeft (p1_to_l1 L1Method),
    spat "_int" (L1Lit . Dynamic tInt . (\s chil -> cast (read s :: Int))),
    spat "_name" (const . L1Name)
    ]
