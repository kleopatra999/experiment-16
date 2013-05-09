{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Ex16.Parser (
    LC, Segment, L.Reader, ParserData(..),
    Parser, empty, null, dump_parser,
    ParsedToken(..), pt_pdat, pt_tdat, pt_index, pt_segment, pt_src, pt_start, pt_end,
    ParsedPattern(..), pp_pdat, pp_segment, pp_children,
    token, pattern, term, ignore, lookup_pattern,
    P.Assoc(..), parse
) where

import qualified Ex16.Parser.Trie as T
import qualified Ex16.Parser.Lexer as L
import qualified Ex16.Parser.Patterns as P
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

 -- Types given to the parser must satisfy this interface.
 --  A g is stored in the parser ('g'lobal).
 --  A p is stored in each pattern.
 --  A t is stored in each custom token.
 --  An o is produced as the output of parsing
class ParserData g p t o | o -> g p t where
    process_token :: Parser p t o -> g -> ParsedToken p t -> (Parser p t o, g, Maybe o)
    process_pattern :: Parser p t o -> g -> ParsedPattern p o -> (Parser p t o, g, o)
data ParsedToken p t = ParsedToken (P.Morpheme p) (Maybe t) Int Segment String deriving (Eq, Show)
pt_mo      (ParsedToken x _ _ _ _) = x
pt_tdat    (ParsedToken _ x _ _ _) = x
pt_index   (ParsedToken _ _ x _ _) = x
pt_segment (ParsedToken _ _ _ x _) = x
pt_src     (ParsedToken _ _ _ _ x) = x
pt_pdat = P.pdat . P.pattern . pt_mo
pt_start = fst . pt_segment
pt_end = snd . pt_segment
data ParsedPattern p o = ParsedPattern (P.Morpheme p) Segment [o] deriving (Eq, Show)
pp_mo       (ParsedPattern x _ _) = x
pp_segment  (ParsedPattern _ x _) = x
pp_children (ParsedPattern _ _ x) = x
pp_pat = P.pattern . pp_mo
pp_left = P.left . pp_mo
pp_right = P.right . pp_mo
pp_pdat = P.pdat . pp_pat
pp_ignore = P.ignore . pp_pat
pp_start = fst . pp_segment
pp_end = snd . pp_segment

 -- PARSER (structure)
data Parser p t o = Parser (L.Lexer t [P.Morpheme p])
                           (M.Map String (P.Pattern p))
                           Int
                    deriving (Show)
lexer  (Parser x _ _) = x
pats   (Parser _ x _) = x
curid  (Parser _ _ x) = x

empty = Parser L.empty M.empty 0
null parser = L.null (lexer parser) && M.null (pats parser)

dump_parser :: (Show p, Show t) => Parser p t o -> String
dump_parser parser = "lits: [\n"
                  ++ concatMap (\(s, l) -> "\t" ++ show s ++ " " ++ show l ++ "\n") (T.assocs (L.lits (lexer parser)))
                  ++ "]\ntokens: [\n"
                  ++ concatMap (\t -> "\t" ++ dump_token t ++ "\n") (L.customs (lexer parser))
                  ++ "]\npatterns: [\n"
                  ++ concatMap (\p -> "\t" ++ show p ++ "\n") (M.elems (pats parser))
                  ++ "]\n"
dump_token (name, (L.Token _ dat, _)) = show name ++ " " ++ showsPrec 11 dat ""

token :: String -> L.Reader -> Maybe t -> Parser p t o -> Parser p t o
token name reader tdat (Parser lexer pats curid) =
    Parser (L.custom name (L.Token reader tdat) [] lexer) pats curid

 -- PATTERNS

pattern :: String -> Double -> P.Assoc -> p -> Parser p t o -> Parser p t o
pattern name prec assoc dat = insert_pattern name prec assoc False dat

term :: String -> p -> Parser p t o -> Parser p t o
term name dat = insert_pattern name 0.0 P.Non False dat

ignore :: String -> p -> Parser p t o -> Parser p t o
ignore name dat = insert_pattern name 0.0 P.Non True dat

lookup_pattern :: String -> Parser p t o -> Maybe (P.Pattern p)
lookup_pattern name = M.lookup name . pats

insert_pattern :: String -> Double -> P.Assoc -> Bool -> p -> Parser p t o -> Parser p t o
insert_pattern name prec assoc ignore pdat (Parser lexer pats curid) = let
    pat = P.build curid name prec assoc ignore pdat
    ins mo lexer = L.alter ((>>= Just . (mo :)) . (`mplus` Just [])) (P.name mo) lexer
    in Parser (foldl (flip ins) lexer (P.mos pat)) (M.insert name pat pats) (succ curid)

 -- ERRORS
data Error g p t o = NoTokenMatch LC
                   | BadBeginning LC (P.Morpheme p)
                   | Mismatch Segment (P.Morpheme p) (P.Morpheme p)
                   | NoMatch [Error g p t o]
                   | TopNotFound String
                   | NoCatPat LC
                   | InternalStackMiss [ParsedPattern p o]
                   | InternalError String
                   deriving (Eq, Show)

 -- PARSE TREE TRANSFORMATIONS
merge_right l m (ParsedPattern t (_, e) c) = ParsedPattern t (pp_start l, e) (c ++ [m])
merge_left  (ParsedPattern t (s, _) c) m r = ParsedPattern t (s, pp_end r)   (m : c)
match1   (ParsedPattern lt (ls, le) lc)   (ParsedPattern rt (rs, re) rc) = ParsedPattern rt (ls, re) (rc ++ lc)
match2   (ParsedPattern lt (ls, le) lc) m (ParsedPattern rt (rs, re) rc) = ParsedPattern rt (ls, re) (rc ++ m : lc)
mismatch (ParsedPattern lt (ls, le) lc)   (ParsedPattern rt (rs, re) rc) = Left $ Mismatch (le, rs) lt rt
add_child new (ParsedPattern t seg c) = ParsedPattern t seg (new : c)

pp_to_pt (ParsedPattern t seg c) tdat str =
    ParsedToken t tdat (P.index t) seg str

 -- PARSING

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
            (tdat, meanings, len) <- require (L.lex (lexer parser) str) $ NoTokenMatch start
             -- Calculate strings and offsets and stuff
            let (got, rest) = splitAt len str
            let end = inc_lc start got
            let segment = (start, end)
             -- Try meanings until one works
            let biguate meanings stack segment = alternate tried $ nomatch tried
                    where tried = map try meanings
                nomatch [one] = head (lefts [one])
                nomatch errs = NoMatch (lefts errs)
                try mo = apply stack where
                    st0 = ParsedPattern mo segment []
                     -- Compare tokens 1 apart
                    apply [] = if P.left mo == P.Closed
                        then return (parser, gdat, st0, [])
                        else Left $ BadBeginning (fst segment) mo
                    apply (st1:stack) = if pp_ignore st0 && P.left mo == P.Closed
                         -- Ignored patterns are allowed to cheat.
                        then return (parser, gdat, st0, st1:stack)
                        else if pp_right st1 == P.Closed && P.left mo == P.Closed
                            then do  -- Insert concatenation token
                                (meanings, len) <- require (T.read (L.lits (lexer parser)) "") $ NoCatPat start
                                (parser, gdat, st1, stack) <- biguate meanings (st1:stack) (fst segment, fst segment)
                                return (parser, gdat, st0, st1:stack)
                            else case P.compete (pp_mo st1) (pp_mo st0) of
                                P.Left -> reduce parser gdat st1 prev stack
                                P.Right -> return (parser, gdat, st0, st1:stack)
                                P.List -> return (parser, gdat, match1 st1 st0, stack)
                                P.Non -> mismatch st1 st0
                     -- When the left token was closed, we start crawling up the stack,
                     --  comparing tokens 2 apart
                    reduce parser gdat st1 prev [] = if P.left mo == P.Open
                        then return (parser, gdat, merge_right st1 prev st0, [])
                        else Left $ BadBeginning (fst segment) mo
                    reduce parser gdat st1 prev (st2:stack) =
                        case P.associate (pp_mo st2) (pp_mo st0) of
                            P.Left  -> do
                                st1 <- return $ merge_left st2 prev st1
                                 -- Mutate parser according to the disputed pattern
                                (parser, gdat, prev) <- return $
                                    process_pattern parser gdat st1
                                reduce parser gdat st1 prev stack
                            P.Right -> return (parser, gdat, merge_right st1 prev st0, st2:stack)
                            P.List  -> return (parser, gdat, match2 st2 prev st0, stack)
                            P.Non   -> mismatch st1 st0
            (parser, gdat, st0, stack) <- biguate meanings stack segment
             -- Mutate parser according to the token read
            (parser, gdat, tokenchild) <- return $
                process_token parser gdat (pp_to_pt st0 tdat got)
            st0 <- return $ case tokenchild of
                Nothing -> st0
                Just c -> add_child c st0
             -- Finish or recurse
            if pp_right st0 == P.Closed
                then do
                     -- Mutate according to just-closed pattern
                    (parser, gdat, next) <- return $
                        process_pattern parser gdat st0
                    if pp_pat st0 == top
                        then if List.null stack
                            then return (next, rest)
                            else Left $ InternalStackMiss stack
                        else if pp_ignore st0
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
