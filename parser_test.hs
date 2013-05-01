
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

parser :: P.Parser Int String ()
parser = foldl (flip ($)) (P.empty 0) [
    P.insert_token "_eof" eof_token Nothing,
    P.insert_token "_int" int_token (Just (const ())),
    P.insert_token "_ws" ws_token Nothing,
    P.insert_ignore "_ws" 0.0 P.ANon,
    P.insert "( _ )" 0.0 P.ANon (const "( _ )"),
    P.insert "_ + _" 2.0 P.ALeft (const "_ + _"),
    P.insert "_ - _" 2.0 P.ALeft (const "_ - _"),
    P.insert "- _" 4.0 P.ALeft (const "- _"),
    P.insert "_ * _" 3.0 P.ALeft (const "_ * _"),
    P.insert "_int" 0.0 P.ANon (const "_int"),
    P.insert "_ _eof" (-1.0/0.0) P.ANon (const "_ _eof"),
    P.insert_mutator "%unique" 0.0 P.ANon
        (\i parser -> P.set_dat (succ (P.parser_dat parser)) parser)
        show
    ]

main = do
    print $ parser
    print $ P.parse parser (fromJust (P.lookup "_ _eof" parser)) " (2 + -3 + 4*5 + -(6+7) * -8 ) "
    print $ P.parse parser (fromJust (P.lookup "_ _eof" parser)) "%unique + %unique + %unique"
