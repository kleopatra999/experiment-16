{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import qualified Ex16.Parser as P
import Data.Char
import Data.Maybe
import Debug.Trace
import Ex16.Tap

data Expr = Parens Expr
          | Plus Expr Expr
          | Minus Expr Expr
          | Negative Expr
          | Times Expr Expr
          | Literal Int
          | Document Expr
          deriving (Eq, Show)

instance Show (a -> b) where show _ = "<function>"

instance P.ParserData Int (Maybe ([Expr] -> Expr)) () Expr where
    mutate_parser parser curunique Nothing _ _ = (parser, succ curunique)
    mutate_parser p c _ _ _ = (p, c)

    process_token curunique tdat seg str = Literal (read str)

    process_pattern _ (Just trans) _ chil = trans chil
    process_pattern curunique Nothing _ _ = Literal curunique

parser :: P.Parser (Maybe ([Expr] -> Expr)) () Expr
parser = foldl (flip ($)) P.empty [
    P.token "_eof" eof_token Nothing,
    P.token "_int" int_token (Just ()),
    P.token "_ws" ws_token Nothing,
    P.ignore "_ws" (Just undefined),
    P.term "( _ )" (Just (Parens . head)),
    P.term "_int" (Just head),
    P.term "%unique" Nothing,
    P.pattern "_ + _" 2.0 P.ALeft (Just (curryL Plus)),
    P.pattern "_ - _" 2.0 P.ALeft (Just (curryL Minus)),
    P.pattern "- _" 4.0 P.ALeft (Just (Negative . head)),
    P.pattern "_ * _" 3.0 P.ALeft (Just (curryL Times)),
    P.pattern "_ _eof" (-1.0/0.0) P.ANon (Just (Document . head))
    ]

main = run_test $ do
    plan 3
    diag $ P.dump_parser parser
    is (P.parse parser 0 "_ _eof" "3")
       (Right (Document (Literal 3), []))
       "Parsing a custom token and one-token pattern works"
    is (P.parse parser 0 "_ _eof" " (2 + -3 + 4*5 + -(6+7) * -8 ) ")
       (Right (Document (Parens (Plus (Plus (Plus (Literal 2) (Negative (Literal 3))) (Times (Literal 4) (Literal 5))) (Times (Negative (Parens (Plus (Literal 6) (Literal 7)))) (Negative (Literal 8))))), []))
       "Parsing arithmetic works"
    is (P.parse parser 0 "_ _eof" "%unique + %unique + %unique")
       (Right (Document (Plus (Plus (Literal 1) (Literal 2)) (Literal 3)), []))
       "Unique value generation works"

read_while :: (Char -> Bool) -> String -> Maybe (String, String)
read_while test = f [] where
    finish acc s = if null acc then Nothing else Just (reverse acc, s)
    f acc [] = finish acc []
    f acc (c:cs) = if test c
        then f (c:acc) cs
        else finish acc (c:cs)

int_token = read_while isDigit
ws_token = read_while isSpace

eof_token :: String -> Maybe (String, String)
eof_token [] = Just ([], [])
eof_token _ = Nothing

curryL f [a, b] = f a b
curryL _ _ = error "curryL not given list of two elements"
