module Ex16.Parser (
    Assoc(..), Pat(..), pat_parts, pat_prec, pat_assoc, pat_trans,
    PP(..), pp_pat, pp_left, pp_right,
    PPLink(..), compare_pp, ParseTree(..), pt_token, pt_children,
    Parser(..), parser_lexer, build_parser,
    ParseError(..), parse_lexed, parse, dump_pt
) where

import Ex16.Lexer
import Data.List

data Assoc = ALeft
           | ARight
           | AList
           | ANon
           deriving (Show, Eq)

 -- A mixfix op composed of tokens and open slots
data Pat a = Pat [String] Float Assoc a
pat_parts (Pat x _ _ _) = x
pat_prec (Pat _ x _ _) = x
pat_assoc (Pat _ _ x _) = x
pat_trans (Pat _ _ _ x) = x

 -- pats are broken up into these.
data PP a = PP (Pat a) PPLink PPLink
pp_pat   (PP x _ _) = x
pp_left  (PP _ x _) = x
pp_right (PP _ _ x) = x
instance Show (PP a) where
    show (PP pat left right) = ("<PP of (" ++ unwords (pat_parts pat) ++ ")>")

split_pattern :: Int -> [(String, TokenParser)] -> Pat a -> (Int, [(TokenParser, PP a)])
split_pattern ppid toks pat@(Pat parts prec assoc trans) =
    f "" parts 0 [] where
        n_parts = length parts
        f _ [] i acc = (ppid + i, acc)
        f prev ("_":ps) i acc = f prev ps (succ i) acc
        f prev (part:ps) i acc = f part ps (succ i) ((tp, pp) : acc) where
            tp = case lookup part toks of
                Just tp -> tp
                Nothing -> TPLit part
            pp = PP pat left right
            left = case i of
                0 -> PPLClosed
                1 -> PPLPrec prec assoc
                _ -> PPLMatch (ppid + i - 2)
            right = case n_parts - i of
                1 -> PPLClosed
                2 -> PPLPrec prec assoc
                _ -> PPLMatch (ppid + i)

 -- The left or right side of a PP
data PPLink = PPLClosed
            | PPLPrec Float Assoc
            | PPLMatch Int
            deriving (Show)

compare_pp :: PP a -> PP a -> Maybe Ordering
compare_pp l r = f (pp_right l) (pp_left r) where
    f (PPLPrec _ _) (PPLMatch _) = Just GT
    f (PPLMatch _) (PPLPrec _ _) = Just LT
    f (PPLPrec lp la) (PPLPrec rp ra) = case compare lp rp of
        LT -> Just LT
        GT -> Just GT
        EQ -> g la ra where
            g ALeft ALeft = Just GT
            g AList AList = Just EQ
            g ARight ARight = Just LT
            g _ _ = Nothing
    f (PPLMatch lm) (PPLMatch rm) = if lm == rm
        then Just EQ
        else Nothing
    f _ _ = Nothing

 -- The result of parsing with a Pat
 -- The children are going to be in reverse order (right-to-left)
data ParseTree a = ParseTree (Token (PP a)) [ParseTree a] deriving (Show)
pt_token (ParseTree x _) = x
pt_children (ParseTree _ x) = x

data Parser a = Parser (Lexer (PP a))
parser_lexer (Parser x) = x

build_parser :: [(String, TokenParser)] -> [Pat a] -> Parser a
build_parser toks pats = let
    gen_pps i [] acc = (i, acc)
    gen_pps i (pat:pats) acc = let
        (newi, pps) = split_pattern i toks pat
        in gen_pps newi pats (pps ++ acc)
    (_, pps) = gen_pps 0 pats []
    in Parser (build_lexer pps)

data ParseError a = ParseStackMiss Bool
                  | ParseMismatch (Token (PP a)) (Token (PP a))
                  | ParseMissedMatcher (Token (PP a))
                  | ParseLexError LexError
                  deriving (Show)

parse_lexed :: Parser a -> [Token (PP a)] -> Either (ParseError a) (ParseTree a)
parse_lexed parser toks = p toks [] where
    p :: [Token (PP a)] -> [(ParseTree a)] -> Either (ParseError a) (ParseTree a)
    p [] [tree] = Right tree
    p [] _ = Left $ ParseStackMiss False
    p (tok : toks) [] = p toks [ParseTree tok []]
    p (rtok : toks) stack =
        case pp_left (token_def rtok) of
             -- Push one token
            PPLClosed -> p toks (ParseTree rtok [] : stack)
             -- This token takes stuff from the stack
            _ -> reduce (rtok : toks) stack
    reduce (rtok:toks) (st1 : st2@(ParseTree ltok lchil) : sttail) =
        case compare_pp (token_def ltok) (token_def rtok) of
            Nothing -> Left $ ParseMismatch ltok rtok
             -- (a~b)~c: Give the left pattern to the right pattern
            Just GT -> reduce (rtok : toks) (ParseTree ltok (st1 : lchil) : sttail)
             -- (a~b~c): Merge left and right patterns
            Just EQ -> p toks (ParseTree rtok (st1 : lchil) : sttail)
             -- a~(b~c): Take just st1 for the right pattern
            Just LT -> p toks (ParseTree rtok [st1] : st2 : sttail)
    reduce _ _ = Left $ ParseStackMiss True

parse :: Parser a -> String -> Either (ParseError a) (ParseTree a)
parse parser str = case parse_lex (parser_lexer parser) str of
    Left err -> Left $ ParseLexError err
    Right toks -> parse_lexed parser toks

dump_pt :: ParseTree a -> String
dump_pt (ParseTree tok chil) = "<"
    ++ unwords (pat_parts (pp_pat (token_def tok)))
    ++ ">(" ++ args ++ ")" where
        args = case chil of
            [] -> "\"" ++ token_src tok ++ "\""
            chil -> intercalate ", " (map dump_pt chil) ++ ")"
