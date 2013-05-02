{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import qualified Ex16.Parser as P
import Data.Char
import Data.Maybe
import Debug.Trace

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
    mutate_parser curunique Nothing _ _ = P.set_dat (succ curunique)
    mutate_parser _ _ _ _ = id

    process_token curunique tdat seg str = Literal (read str)

    process_pattern _ (Just trans) _ chil = trans chil
    process_pattern curunique Nothing _ _ = Literal curunique

parser :: P.Parser Int (Maybe ([Expr] -> Expr)) () Expr
parser = foldl (flip ($)) (P.empty 0) [
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

main = do
    putStr $ P.dump_parser parser
    print $ P.parse parser "_ _eof" "3"
    print $ P.parse parser "_ _eof" " (2 + -3 + 4*5 + -(6+7) * -8 ) "
    print $ P.parse parser "_ _eof" "%unique + %unique + %unique"

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
