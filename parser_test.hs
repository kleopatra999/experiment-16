
import qualified Ex16.Parser as P
import qualified Ex16.Lexer as L
import Data.Char
import Data.Maybe

int_token :: String -> Maybe (String, String)
int_token = f [] where
    finish acc s = if null acc then Nothing else Just (reverse acc, s)
    f acc [] = finish acc []
    f acc (c:cs) = if isDigit c
        then f (c:acc) cs
        else finish acc (c:cs)

eof_token :: String -> Maybe (String, String)
eof_token [] = Just ([], [])
eof_token _ = Nothing

ws_token :: String -> Maybe (String, String)
ws_token = f [] where
    finish acc s = if null acc then Nothing else Just (reverse acc, s)
    f acc [] = finish acc []
    f acc (c:cs) = if isSpace c
        then f (c:acc) cs
        else finish acc (c:cs)

curry3 f (a, b, c) = f a b c
curry4 f (a, b, c, d) = f a b c d

type MyParser = P.Parser () String ()
parser_with_tokens :: MyParser
parser_with_tokens = foldl (flip (curry3 P.insert_token)) (P.empty ()) [
    ("_eof", eof_token, Nothing),
    ("_int", int_token, Just ()),
    ("_ws", ws_token, Nothing)
    ]
parser_with_ws :: MyParser
parser_with_ws = P.insert_ignore "_ws" 0.0 P.ANon parser_with_tokens
parser :: MyParser
parser = foldl (flip (curry4 P.insert)) parser_with_ws [
    ("( _ )", 1.0, P.ANon, "( _ )"),
    ("_ + _", 2.0, P.ALeft, "_ + _"),
    ("_ - _", 2.0, P.ALeft, "_ - _"),
    ("- _", 4.0, P.ALeft, "- _"),
    ("_ * _", 3.0, P.ALeft, "_ * _"),
    ("_int", 0.0, P.ANon, "_int"),
    ("_ _eof", 1.0/0.0, P.ANon, "_ _eof")
    ]

main = do
    print $ parser_with_tokens
    print $ parser
    print $ P.parse parser (fromJust (P.lookup "_ _eof" parser)) " (2 + -3 + 4*5 + -(6+7) * -8 ) "
