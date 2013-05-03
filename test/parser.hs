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

data PatDat = Ignore
            | Trans ([Expr] -> Expr)
            | Unique
            deriving (Show)

instance Show (a -> b) where show _ = "<function>"

instance P.ParserData Int PatDat () Expr where

    process_token parser cu (P.ParsedToken pdat tdat index seg src) =
        (parser, cu, tdat >> Just (Literal (read src)))

    process_pattern parser cu (P.ParsedPattern pdat seg chil) =
        case pdat of
            Ignore -> (parser, cu, Literal (-99))
            Trans trans -> (parser, cu, trans chil)
            Unique -> (parser, succ cu, Literal cu)

parser :: P.Parser PatDat () Expr
parser = foldl (flip ($)) P.empty [
    P.token "_eof" eof_token Nothing,
    P.token "_int" int_token (Just ()),
    P.token "_ws" ws_token Nothing,
    P.ignore "_ws" Ignore,
    P.term "( _ )" (Trans (Parens . head)),
    P.term "_int" (Trans head),
    P.term "%unique" Unique,
    P.pattern "_ + _" 2.0 P.ALeft (Trans (curryL Plus)),
    P.pattern "_ - _" 2.0 P.ALeft (Trans (curryL Minus)),
    P.pattern "- _" 4.0 P.ALeft (Trans (Negative . head)),
    P.pattern "_ * _" 3.0 P.ALeft (Trans (curryL Times)),
    P.pattern "_ _eof" (-1.0/0.0) P.ANon (Trans (Document . head))
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
       (Right (Document (Plus (Plus (Literal 0) (Literal 1)) (Literal 2)), []))
       "Unique value generation works"

unempty 0 = Nothing
unempty i = Just i

read_while :: (Char -> Bool) -> String -> Maybe Int
read_while test = unempty . length . takeWhile test

int_token = read_while isDigit
ws_token = read_while isSpace

eof_token :: String -> Maybe Int
eof_token [] = Just 0
eof_token _ = Nothing

curryL f [a, b] = f a b
curryL _ _ = error "curryL not given list of two elements"
