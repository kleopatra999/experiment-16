{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Ex16.Parser (
    LC, Segment, Reader, ParserData(..),
    Parser, empty, null, dump_parser,
    ParsedToken(..), pt_pdat, pt_tdat, pt_index, pt_segment, pt_src, pt_start, pt_end,
    ParsedPattern(..), pp_pdat, pp_segment, pp_children,
    Token, token_dat, token, lookup_token, delete_token,
    Pattern, pattern_id, pattern_name, pattern_prec, pattern_assoc, pattern_dat,
    pattern_ignore, pattern, term, ignore, lookup_pattern, --delete_pattern,
    Assoc(..), parse
) where

import qualified Ex16.Trie as T
import qualified Data.List as List
import qualified Data.Map as M
import Prelude hiding (null, lookup, lex)
import Data.Maybe
import Data.Either
import Data.Function
import Control.Monad
import Debug.Trace

 -- Utilitous type aliases

type LC = (Int, Int)
type Segment = (LC, LC)
type Reader = String -> Maybe Int  -- Just return length parsed

 -- Types given to the parser must satisfy this interface.
 --  A g is stored in the parser ('g'lobal).
 --  A p is stored in each pattern.
 --  A t is stored in each custom token.
 --  An o is produced as the output of parsing
class ParserData g p t o | o -> g p t where
    process_token :: Parser p t o -> g -> ParsedToken p t -> (Parser p t o, g, Maybe o)
    process_pattern :: Parser p t o -> g -> ParsedPattern p o -> (Parser p t o, g, o)
data ParsedToken p t = ParsedToken p (Maybe t) Int Segment String
pt_pdat    (ParsedToken x _ _ _ _) = x
pt_tdat    (ParsedToken _ x _ _ _) = x
pt_index   (ParsedToken _ _ x _ _) = x
pt_segment (ParsedToken _ _ _ x _) = x
pt_src     (ParsedToken _ _ _ _ x) = x
pt_start = fst . pt_segment
pt_end = snd . pt_segment
data ParsedPattern p o = ParsedPattern p Segment [o]
pp_pdat     (ParsedPattern x _ _) = x
pp_segment  (ParsedPattern _ x _) = x
pp_children (ParsedPattern _ _ x) = x

 -- PARSER (structure)
data Parser p t o = Parser (T.Trie Char [Tok p])
                           (M.Map String (Token t, [Tok p]))
                           (M.Map String (Pattern p))
                           Int
                    deriving (Show)
lits   (Parser x _ _ _) = x
tokens (Parser _ x _ _) = x
pats   (Parser _ _ x _) = x
curid  (Parser _ _ _ x) = x
set_lits    a (Parser _ b c d) = Parser a b c d
set_tokens  b (Parser a _ c d) = Parser a b c d
set_pats    c (Parser a b _ d) = Parser a b c d
set_curid   d (Parser a b c _) = Parser a b c d

empty = Parser T.empty M.empty M.empty 0
null parser = M.null (tokens parser) && M.null (pats parser)

dump_parser :: (Show p, Show t) => Parser p t o -> String
dump_parser parser = "lits: [\n"
                  ++ concatMap (\(s, l) -> "\t" ++ show s ++ " " ++ show l ++ "\n") (T.assocs (lits parser))
                  ++ "]\ntokens: [\n"
                  ++ concatMap (\t -> "\t" ++ dump_token t ++ "\n") (M.assocs (tokens parser))
                  ++ "]\npatterns: [\n"
                  ++ concatMap (\p -> "\t" ++ show p ++ "\n") (M.elems (pats parser))
                  ++ "]\n"
dump_token (name, (Token _ dat, _)) = show name ++ " " ++ showsPrec 11 dat ""

 -- TOKENS
data Token t = Token Reader (Maybe t)
token_reader (Token x _) = x
token_dat    (Token _ x) = x
instance Show t => Show (Token t) where
    show (Token _ t) = "<Token " ++ showsPrec 11 t ">"

token :: String -> Reader -> Maybe t -> Parser p t o -> Parser p t o
token name reader tdat parser =
    set_tokens (M.insert name (Token reader tdat, []) (tokens parser)) parser

lookup_token :: String -> Parser p t o -> Maybe (Token t)
lookup_token name parser = M.lookup name (tokens parser) >>= return . fst

delete_token :: String -> Parser p t o -> Parser p t o
delete_token name parser =
    set_tokens (M.delete name (tokens parser)) parser

 -- PATTERNS
data Pattern p = Pattern Int String Double Assoc Bool p deriving (Show)
pattern_id     (Pattern x _ _ _ _ _) = x  -- id is unique within one parser
pattern_name   (Pattern _ x _ _ _ _) = x
pattern_prec   (Pattern _ _ x _ _ _) = x
pattern_assoc  (Pattern _ _ _ x _ _) = x
pattern_ignore (Pattern _ _ _ _ x _) = x
pattern_dat    (Pattern _ _ _ _ _ x) = x
data Assoc = ALeft | ARight | AList | ANon deriving (Show, Eq)
instance Eq (Pattern p) where (==) = (==) `on` pattern_id

pattern :: String -> Double -> Assoc -> p -> Parser p t o -> Parser p t o
pattern name prec assoc dat = insert_pattern name prec assoc False dat

term :: String -> p -> Parser p t o -> Parser p t o
term name dat = insert_pattern name 0.0 ANon False dat

ignore :: String -> p -> Parser p t o -> Parser p t o
ignore name dat = insert_pattern name 0.0 ANon True dat

lookup_pattern :: String -> Parser p t o -> Maybe (Pattern p)
lookup_pattern name = M.lookup name . pats

insert_pattern :: String -> Double -> Assoc -> Bool -> p -> Parser p t o -> Parser p t o
insert_pattern name prec assoc ignore pdat parser = let
    pat = Pattern (curid parser) name prec assoc ignore pdat
    parts = words name
    scan left i parts (lits, tokens) = decide where
        part = head parts
        decide = case parts of
            [] -> error $ "Cannot make an empty pattern"
            "_":[] -> error $ "Cannot make a pattern of a single _"
            "_":"_":_ -> openleft_cat
            "_":_ -> openleft
            _:[] -> closed
            _:"_":[] -> open
            _:"_":"_":_ -> match2_cat
            _:"_":_ -> match2
            _:_:_ -> match1
        openleft_cat = scan Open i ("" : tail parts) (lits, tokens)
        openleft = scan Open i (tail parts) (lits, tokens)
        closed = one Closed
        open = one Open
        match2_cat = scan Match2 (succ i) ("" : tail (tail parts)) (one Match2)
        match2 = scan Match2 (succ i) (tail (tail parts)) (one Match2)
        match1 = scan Match1 (succ i) (tail parts) (one Match1)
        tok right = Tok pat i left right
        one right = case M.lookup part tokens of
            Just (token, old) -> (lits, M.adjust af part tokens) where
                af = const (token, List.insert (tok right) old)
            Nothing -> (T.insertWith (List.insert . head) part [tok right] lits, tokens)
    (_lits, _tokens) = scan Closed 0 parts (lits parser, tokens parser)
    in Parser _lits _tokens (M.insert name pat (pats parser)) (succ (curid parser))

 -- TOKEN MEANINGS
data Tok p = Tok (Pattern p) Int Link Link deriving (Show)
tok_pat    (Tok x _ _ _) = x
tok_index  (Tok _ x _ _) = x
tok_left   (Tok _ _ x _) = x
tok_right  (Tok _ _ _ x) = x

instance Eq (Tok p) where
    a == b = tok_pat a == tok_pat b && tok_index a == tok_index b

 -- First prioritize left, then right.
instance Ord (Tok p) where
    compare = compare `on` \x -> (tok_left x, tok_right x)

 -- The left or right side of a Tok, in priority order
data Link = Match1 | Match2 | Open | Closed deriving (Show, Eq, Ord)

 -- Which of two concatenated tokens owns the other (or both)
compete :: Tok p -> Tok p -> Assoc
compete left right = f (tok_right left) (tok_left right) where
    f Open Closed = ARight
    f Match2 Closed = ARight
    f Closed Open = ALeft
    f Closed Match2 = ALeft
    f Match1 Match1 = AList
    f _ _ = ANon

 -- In "left mid right", whether left or right takes the mid (or both)
associate :: Tok p -> Tok p -> Assoc
associate l r = f (tok_right l) (tok_left r) where
    f Open Match2 = ALeft
    f Match2 Open = ARight
    f Open Open = case (compare `on` pattern_prec . tok_pat) l r of
        GT -> ALeft
        LT -> ARight
        EQ -> (g `on` pattern_assoc . tok_pat) l r where
            g ALeft ALeft = ALeft
            g AList AList = AList
            g ARight ARight = ARight
            g _ _ = ANon
    f Match2 Match2 = match
    f Match1 Match1 = match
    f _ _ = ANon
    match = if tok_pat l == tok_pat r && tok_index l + 1 == tok_index r
        then AList
        else ANon

 -- ERRORS
data Error g p t o = NoTokenMatch LC
                   | BadBeginning LC (Tok p)
                   | Mismatch Segment (Tok p) (Tok p)
                   | NoMatch [Error g p t o]
                   | TopNotFound String
                   | NoCatPat LC
                   | InternalStackMiss [Tree p o]
                   | InternalError String
                   deriving (Eq, Show)

 -- PARSE TREES
 --  These are temporary and not part of the parser's API
data Tree p o = Tree Segment (Tok p) [o] deriving (Show)
instance Eq (Tree p o) where a == b = False  -- Don't actually do this
tree_segment (Tree x _ _) = x
tree_tok     (Tree _ x _) = x
tree_chil    (Tree _ _ x) = x  -- children are stored in reverse order
tree_start = fst . tree_segment
tree_end = snd . tree_segment
tree_pat = tok_pat . tree_tok
tree_left = tok_left . tree_tok
tree_right = tok_right . tree_tok
tree_ignore = pattern_ignore . tree_pat
tree_closed = (== Closed) . tree_right
 -- Tree transformations
merge_right l m (Tree (_, e) t c) = Tree (tree_start l, e) t (c ++ [m])
merge_left  (Tree (s, _) t c) m r = Tree (s, tree_end r)   t (m : c)
match1   (Tree (ls, le) lt lc)   (Tree (rs, re) rt rc) = Tree (ls, re) rt (rc ++ lc)
match2   (Tree (ls, le) lt lc) m (Tree (rs, re) rt rc) = Tree (ls, re) rt (rc ++ m : lc)
mismatch (Tree (ls, le) lt lc)   (Tree (rs, re) rt rc) = Left $ Mismatch (le, rs) lt rt
add_child new (Tree seg t c) = Tree seg t (new : c)

tree_to_pt (Tree seg t c) tdat str =
    ParsedToken (pattern_dat (tok_pat t)) tdat (tok_index t) seg str
tree_to_pp (Tree seg t c) =
    ParsedPattern (pattern_dat (tok_pat t)) seg (reverse c)

 -- PARSING

type Lexer p t o = Parser p t o -> String -> Maybe (Maybe t, [Tok p], Int)

lex :: Lexer p t o
lex parser str = lex_lit parser str `mplus` lex_tokens parser str

lex_lit :: Lexer p t o
lex_lit parser str = do
    (meanings, len) <- T.read (lits parser) str
    guard (len > 0)
    return (Nothing, meanings, len)

lex_tokens :: Lexer p t o
lex_tokens parser str = let
    lex_token (Token reader tdat, meanings) = do
        len <- reader str
        return (tdat, meanings, len)
    in msum (map lex_token (M.elems (tokens parser))) where

parse :: (ParserData g p t o, Show p, Show o)
      => Parser p t o
      -> g
      -> String  -- The top-level pattern (e.g. "_ _eof")
      -> String
      -> Either (Error g p t o) (o, String)
parse parser gdat top_name str = do
    top <- require (lookup_pattern top_name parser)
                   (TopNotFound top_name)
    let start = parse' parser gdat (1, 1) str (error "Oops") []
        parse' parser gdat start str prev stack = do
             -- Note: prev is only defined if the last pattern was closed.
             -- Read a single token
            (tdat, meanings, len) <- require (lex parser str) $ NoTokenMatch start
             -- Calculate strings and offsets and stuff
            let (got, rest) = splitAt len str
            let end = inc_lc start got
            let segment = (start, end)
             -- Try meanings until one works
            let biguate meanings stack segment = alternate tried $ nomatch tried
                    where tried = map (try stack) meanings
                nomatch [one] = head (lefts [one])
                nomatch errs = NoMatch (lefts errs)
                try stack tok = apply parser gdat stack where
                    st0 = Tree segment tok []
                     -- Compare tokens 1 apart
                    apply parser gdat [] = if tok_left tok == Closed
                        then return (parser, gdat, st0, [])
                        else Left $ BadBeginning (fst segment) tok
                    apply parser gdat (st1:stack) = if tree_ignore st0 && tok_left tok == Closed
                         -- Ignored patterns are allowed to cheat.
                        then return (parser, gdat, st0, st1:stack)
                        else if tree_right st1 == Closed && tok_left tok == Closed
                            then do  -- Insert concatenation token
                                (meanings, len) <- require (T.read (lits parser) "") $ NoCatPat start
                                (parser, gdat, st1, stack) <- biguate meanings (st1:stack) (fst segment, fst segment)
                                apply parser gdat (st1:stack)
                            else case compete (tree_tok st1) (tree_tok st0) of
                                ALeft -> reduce parser gdat st1 prev stack
                                ARight -> return (parser, gdat, st0, st1:stack)
                                AList -> return (parser, gdat, match1 st1 st0, stack)
                                ANon -> mismatch st1 st0
                     -- When the left token was closed, we start crawling up the stack,
                     --  comparing tokens 2 apart
                    reduce parser gdat st1 prev [] = if tok_left tok == Open
                        then return (parser, gdat, merge_right st1 prev st0, [])
                        else Left $ BadBeginning (fst segment) tok
                    reduce parser gdat st1 prev (st2:stack) =
                        case associate (tree_tok st2) (tree_tok st0) of
                            ALeft  -> do
                                st1 <- return $ merge_left st2 prev st1
                                 -- Mutate parser according to the disputed pattern
                                (parser, gdat, prev) <- return $
                                    process_pattern parser gdat (tree_to_pp st1)
                                reduce parser gdat st1 prev stack
                            ARight -> return (parser, gdat, merge_right st1 prev st0, st2:stack)
                            AList  -> return (parser, gdat, match2 st2 prev st0, stack)
                            ANon   -> mismatch st1 st0
            (parser, gdat, st0, stack) <- biguate meanings stack segment
             -- Mutate parser according to the token read
            (parser, gdat, tokenchild) <- return $
                process_token parser gdat (tree_to_pt st0 tdat got)
            st0 <- return $ case tokenchild of
                Nothing -> st0
                Just c -> add_child c st0
             -- Finish or recurse
            if tree_right st0 == Closed
                then do
                     -- Mutate according to just-closed pattern
                    (parser, gdat, next) <- return $
                        process_pattern parser gdat (tree_to_pp st0)
                    if tree_pat st0 == top
                        then if List.null stack
                            then return (next, rest)
                            else Left $ InternalStackMiss stack
                        else if pattern_ignore (tree_pat st0)
                            then parse' parser gdat end rest prev stack
                            else parse' parser gdat end rest next (st0:stack)
                else parse' parser gdat end rest (error "oops") (st0:stack)
    start

 -- Various utility functions

 -- Like msum but not treading on anyone's toes
alternate list err = foldl or (Left err) list where
    Right x `or` _ = Right x
    Left _ `or` x = x

require Nothing err = Left err
require (Just x) err = Right x

inc_lc :: LC -> String -> LC
inc_lc lc [] = lc
inc_lc (l, c) ('\n':cs) = inc_lc (succ l, 1) cs
inc_lc (l, c) (_:cs) = inc_lc (l, succ c) cs
