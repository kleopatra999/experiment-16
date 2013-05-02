{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Ex16.Parser (
    LC, Segment, Reader, ParserData(..),
    Parser, empty, null, dump_parser,
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
 --  A p is stored in each pattern, unless the pattern is ignored.
 --  A t is stored in each custom token.
 --  An o is produced as the output of parsing
class ParserData g p t o | o -> g p t where
     -- After each token, the parser can be modified.
     -- The Int parameter is index of the token in the pattern,
     --  ignoring "_" parts, e.g. in "\ _ -> _", the "\" has an index
     --  of 0 and the -> has an index of 1.
     -- The Maybe t parameter is Nothing if the token is not a custom token
    mutate_parser :: Parser p t o -> g -> p -> Maybe t -> Int -> (Parser p t o, g)
    mutate_parser p d _ _ _ = (p, d)
     -- The String parameter is the exact string matched by the token.
     -- This will only be called on custom tokens.
    process_token :: g -> t -> Segment -> String -> o
     -- The contents of the list parameter are the processed contents
     --  of "_" parts and special tokens that return something,
     -- e.g. in "%macro ( _str , _ = _ )", it will contain three objects.
     -- process_pattern will not be called on ignored patterns.
    process_pattern :: g -> p -> Segment -> [o] -> o

no_mutation :: Parser p t o -> g -> p -> Maybe t -> Int -> (Parser p t o, g)
no_mutation p d _ _ _ = (p, d)

 -- PARSER (structure)
data Parser p t o = Parser Int
                           (M.Map String (Pattern p))
                           (T.Trie Char [Tok p])
                           (M.Map String (Token t, [Tok p]))
                    deriving (Show)
curid  (Parser x _ _ _) = x
pats   (Parser _ x _ _) = x
lits   (Parser _ _ x _) = x
tokens (Parser _ _ _ x) = x
set_curid   a (Parser _ b c d) = Parser a b c d
set_pats    b (Parser a _ c d) = Parser a b c d
set_lits    c (Parser a b _ d) = Parser a b c d
set_tokens  d (Parser a b c _) = Parser a b c d

empty = Parser 0 M.empty T.empty M.empty
null parser = M.null (pats parser) && M.null (tokens parser)

dump_parser :: (Show p, Show t) => Parser p t o -> String
dump_parser parser = "tokens: [\n"
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
            "_":"_":[] -> error $ "Catpat NYI"
            "_":"_":_ -> error $ "Too many _s in a row"
            "_":ps -> openleft
            p:[] -> closed
            p:"_":[] -> open
            _:"_":"_":_ -> error $ "Too many _s in a row"
            p:"_":ps -> match2
            p:q:ps -> match1
        openleft = scan Open i (tail parts) (lits, tokens)
        closed = one Closed
        open = one Open
        match2 = scan Match2 (succ i) (tail (tail parts)) (one Match2)
        match1 = scan Match1 (succ i) (tail parts) (one Match1)
        tok right = Tok pat i left right
        one right = case M.lookup part tokens of
            Just (token, old) -> (lits, M.adjust af part tokens) where
                af = const (token, List.insert (tok right) old)
            Nothing -> (T.insertWith (List.insert . head) part [tok right] lits, tokens)
    (_lits, _tokens) = scan Closed 0 parts (lits parser, tokens parser)
    in Parser (succ (curid parser)) (M.insert name pat (pats parser)) _lits _tokens

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

 -- Determine which of two non-closed tokens takes precedence
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
                   | CatPatternNYI LC
                   | InternalError String
                   deriving (Eq, Show)

 -- PARSE TREES
 --  These are temporary and not part of the parser's API
data Tree p o = Tree Segment (Tok p) [o] deriving (Show)
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
 -- Deliver the payload
tree_finish :: ParserData g p t o => Parser p t o -> g -> Tree p o -> o
tree_finish parser dat (Tree seg tok chil) =
    process_pattern dat (pattern_dat (tok_pat tok)) seg (reverse chil)
 -- Do something with a tree and a stack
 -- The parser must be fed through so that the g -> p owned
 --  by patterns can be executed
tree_apply :: ParserData g p t o
           => Parser p t o -> g -> Tree p o -> [Tree p o]
           -> Either (Error g p t o) (Tree p o, [Tree p o])
tree_apply parser dat right [] = case tree_left right of
    Closed -> Right $ (right, [])
    _      -> Left $ BadBeginning (tree_start right) (tree_tok right)
tree_apply parser dat right (left:stack) =
    decide (tree_right left) (tree_left right) where
         -- Decide action based on the token links
        decide _      Closed
                 | tree_ignore right = Right $ (right, left:stack)
        decide Closed Closed = Left $ CatPatternNYI (tree_start right)
        decide Closed Open   = reduce left stack
        decide Closed Match2 = reduce left stack
        decide Open   Closed = Right $ (right, left:stack)
        decide Match2 Closed = Right $ (right, left:stack)
        decide Match1 Match1 = Right $ (merge_2 left right, stack)
        decide _      _      = Left $ merge_mismatch left right
        reduce mid [] = case tree_left right of
            Open   -> Right $ (merge_right mid right, [])
            _      -> Left $ BadBeginning (tree_start right) (tree_tok right)
        reduce mid (left:stack) = case associate (tree_tok left) (tree_tok right) of
            ANon   -> Left $ merge_mismatch left right
            ALeft  -> reduce (merge_left left mid) stack
            AList  -> Right $ (merge_3 left mid right, stack)
            ARight -> Right $ (merge_right mid right, left : stack)
         -- Actions
        merge_left left mid =  -- left adopts middle
            Tree (tree_start left, tree_end mid)
                 (tree_tok left)
                 (tree_finish parser dat mid : tree_chil left)
        merge_right mid right =  -- right adopts middle
            Tree (tree_start mid, tree_end right)
                 (tree_tok right)
                 (tree_chil right ++ [tree_finish parser dat mid])
        merge_2 left right =  -- left and right join
            Tree (tree_start left, tree_end right)
                 (tree_tok right)
                 (tree_chil right ++ tree_chil left)
        merge_3 left mid right =  -- left and right join and adopt middle
            Tree (tree_start left, tree_end right)
                 (tree_tok right)
                 (tree_chil right ++ (tree_finish parser dat mid : tree_chil left))
        merge_mismatch left right =
            Mismatch (tree_end left, tree_start right) (tree_tok left) (tree_tok right)

 -- PARSING

type Lexer p t o = Parser p t o -> String -> Maybe (Maybe t, [Tok p], Int)

lex :: Lexer p t o
lex parser str = lex_lit parser str `mplus` lex_tokens parser str

lex_lit :: Lexer p t o
lex_lit parser str = do
    (meanings, len) <- T.read (lits parser) str
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
    let start = parse' parser gdat (1, 1) str [] where
        parse' parser gdat start str stack = do
             -- Read a single token
            (tdatish, meanings, len) <-
                require (lex parser str) $ NoTokenMatch start
             -- Do a little processing
            let (got, rest) = splitAt len str
                end = inc_lc start got
                segment = (start, end)
                process = \tdat -> [process_token gdat tdat segment got]
                token_output = maybe [] process tdatish where
             -- Try meanings until one works
            let try tok = tree_apply parser gdat (Tree segment tok token_output) stack
                tried = map try meanings
                nomatch [one] = head (lefts [one])
                nomatch errs = NoMatch (lefts errs)
            (tree, stack) <- alternate tried $ nomatch tried
             -- Mutate parser
            let pdat = pattern_dat (tok_pat (tree_tok tree))
                index = tok_index (tree_tok tree)
                (newparser, newgdat) = mutate_parser parser gdat pdat tdatish index
             -- Finish or recurse
            if tree_closed tree && tree_pat tree == top
                then if List.null stack
                    then return (tree_finish newparser newgdat tree, rest)
                    else Left $ InternalError "Had some forgotten stack left over"
                else if tree_closed tree && pattern_ignore (tree_pat tree)
                    then parse' newparser newgdat end rest stack
                    else parse' newparser newgdat end rest (tree:stack)
    start

 -- Various utility functions

 -- Like msum but not treading on anyone's toes
alternate list err = foldl (flip or) (Left err) list where
    Right x `or` _ = Right x
    Left _ `or` x = x
require Nothing err = Left err
require (Just x) err = Right x

inc_lc :: LC -> String -> LC
inc_lc lc [] = lc
inc_lc (l, c) ('\n':cs) = (succ l, 1)
inc_lc (l, c) (_:cs) = (l, succ c)
