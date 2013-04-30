module Ex16.Parser (
    Assoc(..), Pat,
    Parser, pats, lexer, parser_dat, empty, null, set_dat, insert_token,
    lookup, insert, insert_ignore, insert_mutator, insert_mutator_ignore, delete,
    LC, Segment, Tree(..), Error(..), parse
) where

import qualified Ex16.Lexer as L
import qualified Data.List as List
import qualified Data.Map as M
import Prelude hiding (null, lookup)
import Data.Maybe
import Data.Function
import Control.Monad
import Debug.Trace

data Assoc = ALeft
           | ARight
           | AList
           | ANon
           deriving (Show, Eq)

type Mutator g p t = Parser g p t -> Parser g p t

 -- A mixfix op composed of tokens and open slots
data Pat g p t = Pat Int String Double Assoc (Mutator g p t) (Maybe p)
pat_id     (Pat x _ _ _ _ _) = x
pat_name   (Pat _ x _ _ _ _) = x
pat_prec   (Pat _ _ x _ _ _) = x
pat_assoc  (Pat _ _ _ x _ _) = x
pat_mutate (Pat _ _ _ _ x _) = x
pat_dat    (Pat _ _ _ _ _ x) = x
instance Eq (Pat g p t) where (==) = (==) `on` pat_id
instance Show p => Show (Pat g p t) where
    show pat = "<Pat " ++ show (pat_name pat) ++ " " ++ showsPrec 11 (pat_dat pat) ">"

 -- pats are broken up into these.
data PP g p t = PP (Pat g p t) Int PPLink PPLink (Maybe t) deriving (Show)
pp_pat   (PP x _ _ _ _) = x
pp_index (PP _ x _ _ _) = x
pp_left  (PP _ _ x _ _) = x
pp_right (PP _ _ _ x _) = x
pp_dat   (PP _ _ _ _ x) = x
instance Eq (PP g p t) where
    a == b = pp_pat a == pp_pat b && pp_index a == pp_index b
--instance Show (PP g p t) where
--    show pp = "<PP " ++ show (pat_name (pp_pat pp)) ++ " " ++ show (pp_index pp) ++ ">"

 -- The left or right side of a PP, in preference order
data PPLink = PPLMatch1
            | PPLMatch2
            | PPLOpen
            | PPLClosed
            deriving (Show, Eq, Ord)
 -- First prioritize left, then right.
instance Ord (PP g p t) where
    compare = compare `on` \x -> (pp_left x, pp_right x)

compare_pp :: PP g p t -> PP g p t -> Maybe Ordering
compare_pp l r = f (pp_right l) (pp_left r) where
    f PPLOpen PPLMatch2 = Just GT
    f PPLMatch2 PPLOpen = Just LT
    f PPLOpen PPLOpen = case (compare `on` pat_prec . pp_pat) l r of
        LT -> Just LT
        GT -> Just GT
        EQ -> (g `on` pat_assoc . pp_pat) l r where
            g ALeft ALeft = Just GT
            g AList AList = Just EQ
            g ARight ARight = Just LT
            g _ _ = Nothing
    f PPLMatch2 PPLMatch2 = if
        pp_pat l == pp_pat r && pp_index l + 2 == pp_index r
            then Just EQ
            else Nothing
    f PPLMatch1 PPLMatch1 = if
        pp_pat l == pp_pat r && pp_index l + 1 == pp_index r
            then Just EQ
            else Nothing
    f _ _ = Nothing

 -- This mediates between insert_token and the lexer's custom tokens
data Token t a = Token (Maybe a) (String -> Maybe (String, String)) (Maybe t)
instance L.Tokenizer (Token t) where
    read (Token maybe_res reader _) str = do
        res <- maybe_res
        (got, rest) <- reader str
        return (res, got, rest)
instance (Show t, Show a) => Show (Token t a) where
    show (Token adat reader tdat) = "<Token " ++ showsPrec 11 adat (" " ++ showsPrec 11 tdat ">")

 -- The actual parser object
data Parser g p t = Parser Int (M.Map String (Pat g p t)) (L.Lexer (Token t) [PP g p t]) g
                  deriving (Show)

pats       (Parser _ x _ _) = x
lexer      (Parser _ _ x _) = x
parser_dat (Parser _ _ _ x) = x

empty dat = Parser 0 M.empty L.empty dat
null (Parser _ pats lexer _) = M.null pats && L.null lexer

set_dat dat (Parser curid pats lexer _) = Parser curid pats lexer dat

insert_token :: String -> (String -> Maybe (String, String)) -> Maybe t -> Mutator g p t
insert_token name reader tdat (Parser curid pats lexer gdat) =
    Parser curid pats (L.insert_custom name (Token Nothing reader tdat) lexer) gdat

lookup :: String -> Parser g p t -> Maybe (Pat g p t)
lookup name = M.lookup name . pats

insert :: String -> Double -> Assoc -> p -> Mutator g p t
insert name prec assoc dat = insert_internal name prec assoc id (Just dat)

insert_ignore :: String -> Double -> Assoc -> Mutator g p t
insert_ignore name prec assoc = insert_internal name prec assoc id Nothing

insert_mutator :: String -> Double -> Assoc -> Mutator g p t -> p -> Mutator g p t
insert_mutator name prec assoc f dat = insert_internal name prec assoc f (Just dat)

insert_mutator_ignore :: String -> Double -> Assoc -> Mutator g p t -> Mutator g p t
insert_mutator_ignore name prec assoc f = insert_internal name prec assoc f Nothing

insert_internal :: String -> Double -> Assoc -> Mutator g p t -> Maybe p -> Mutator g p t
insert_internal name prec assoc mut pdat (Parser curid pats lexer gdat) =
    guard (Parser (succ curid) (M.insert name pat pats) (scan PPLClosed 0 parts lexer) gdat) where
        pat = Pat curid name prec assoc mut pdat
        parts = words name
        guard = if isJust pdat || (parts /= [] && head parts /= "_" && last parts /= "_")
            then id
            else error "Cannot specify an ignored pattern with an open end"
        scan left i parts l = let
            part = head parts
            pp right = PP pat i left right
            one right = case L.lookup_custom part lexer of
                Just (Token (Just old) reader tdat) ->
                    L.adjust_custom (const (Token (Just new) reader tdat)) part l where
                        new = List.insert (pp right tdat) old
                Just (Token Nothing reader tdat) ->
                    L.adjust_custom (const (Token (Just [pp right tdat]) reader tdat)) part l
                Nothing -> L.insertWith (List.insert . head) part [pp right Nothing] l
            openleft = scan PPLOpen (i + 1) (tail parts) l
            closed = one PPLClosed
            open = one PPLOpen
            match2 = scan PPLMatch2 (i + 2) (tail (tail parts)) (one PPLMatch2)
            match1 = scan PPLMatch1 (i + 1) (tail parts) (one PPLMatch1)
            in case parts of
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

delete :: String -> Mutator g p t
delete name parser@(Parser curid pats lexer gdat) = case lookup name parser of
    Nothing -> parser
    Just pat -> let
        parts = filter (/= "_") (words name)
        df lexer part = case L.lookup_custom part lexer of
            Just _ -> L.adjust_custom af part lexer
            Nothing -> L.delete part lexer
        af (Token (Just [old]) reader tdat) = Token Nothing reader tdat
        af (Token (Just old) reader tdat) = Token (Just new) reader tdat where
            new = List.filter ((pat /=) . pp_pat) old
        in Parser curid (M.delete name pats) (foldl df lexer parts) gdat

type LC = (Int, Int)
inc_lc :: LC -> String -> LC
inc_lc lc [] = lc
inc_lc (l, c) ('\n':cs) = (succ l, 1)
inc_lc (l, c) (_:cs) = (l, succ c)

type Segment = (LC, LC)

 -- The result of parsing with a Pat
 -- The children are going to be in reverse order (right-to-left)
data Tree p t = Branch Segment p [Tree p t]
              | Leaf Segment t String
instance (Show p, Show t) => Show (Tree p t) where
    show (Branch seg dat chil) = show dat ++ "[" ++ List.intercalate ", " (map show chil) ++ "]"
    show (Leaf seg dat src) = show dat ++ " " ++ show src

 -- This is a structure internal to the parser, representing an incomplete tree
data PreTree g p t = PreTree Segment (PP g p t) [Tree p t]
finish (PreTree seg left chil) = case pat_dat (pp_pat left) of
    Just dat -> Branch seg dat (reverse chil)
    Nothing -> error $ "Internal parser error: A PreTree that should have been ignored wasn't."
add_child c (PreTree seg left chil) = PreTree seg left (c : chil)

 -- Various ways a parse can fail
data Error g p t = NoTokenMatch LC
                 | BadBeginning LC (PP g p t)
                 | Mismatch Segment (PP g p t) (PP g p t)
                 | NoMatch [Error g p t]
                 | CatPatNYI LC
                 deriving (Show)

parse :: (Show g, Show p, Show t) => Parser g p t
      -> Pat g p t
      -> String
      -> Either (Error g p t) (Tree p t, String)
parse = p [] (1, 1) where
    p :: (Show g, Show p, Show t) => [PreTree g p t]
      -> LC
      -> Parser g p t
      -> Pat g p t
      -> String
      -> Either (Error g p t) (Tree p t, String)
    p stack right_start parser top str = case L.read (lexer parser) str of
        Nothing -> Left $ NoTokenMatch right_start
        Just (rights, got, rest) -> single (map try rights) where
            single meanings = case foldl mplus (Left undefined) meanings of
                Left _ -> Left $ NoMatch (map unleft meanings) where
                    unleft (Left x) = x
                Right res -> res
            try right = decide' should_ignore left_link right_link where
                right_end = inc_lc right_start got
                 -- These must only be accessed if the stack isn't empty
                PreTree (pat_start, left_end) left chil = case stack of
                    [] -> error "Internal parser error: tried to reference top of empty stack"
                    (pt:_) -> pt
                make_leaf chil = case pp_dat right of
                    Just dat -> Leaf (right_start, right_end) dat got : chil
                    Nothing -> chil
                 -- Use these to decide what to do
                should_ignore = isNothing (pat_dat (pp_pat right))
                left_link = if List.null stack then Nothing else Just (pp_right left)
                right_link = pp_left right
                 -- The decision function
                decide' a b c = trace ("decide: " ++ show (right, got)) $ decide a b c
                decide True _ PPLClosed = Right push
                decide _ Nothing PPLClosed = Right push
                decide _ Nothing _ = Left $ BadBeginning right_start right
                decide _ (Just PPLClosed) PPLClosed = Left $ CatPatNYI right_start
                decide _ (Just PPLClosed) PPLOpen = Right reduce
                decide _ (Just PPLClosed) PPLMatch2 = Right reduce
                decide _ (Just PPLOpen) PPLClosed = Right push
                decide _ (Just PPLMatch1) PPLMatch1 = match1
                decide _ (Just PPLMatch2) PPLClosed = Right push
                decide _ _ _ = Left mismatch
                 -- Decision results
                mismatch = Mismatch (left_end, right_start) left right
                push = continue $ PreTree (right_start, right_end) right (make_leaf []) : stack
                merge = continue $ PreTree (pat_start, right_end) right (make_leaf chil) : tail stack
                match1 = case compare_pp left right of
                    Just EQ -> Right merge
                    _ -> Left mismatch
                reduce = reduce_stack stack
                 -- The stack reduction routine
                reduce_stack (st1@(PreTree (mid_start,_) _ _) : st2@(PreTree (pat_start, left_end) left chil) : sttail) =
                    case compare_pp left right of
                        Nothing -> Left $ Mismatch (left_end, right_start) left right
                         -- (a~b)~c: Give the left pattern to the right pattern
                        Just GT -> reduce_stack (add_child (finish st1) st2 : sttail)
                         -- (a~b~c): Merge left and right patterns
                        Just EQ -> continue (PreTree (pat_start, right_end) right (make_leaf (finish st1 : chil)) : sttail)
                         -- a~(b~c): Take just st1 for the right pattern
                        Just LT -> continue (PreTree (mid_start, right_end) right (make_leaf [finish st1]) : st2 : sttail)
                reduce_stack [st1@(PreTree (mid_start,_) _ _)] = case pp_left right of
                    PPLOpen -> continue [PreTree (mid_start, right_end) right [finish st1]]
                    _ -> Left $ BadBeginning right_start right
                reduce_stack _ = error "Internal error in the parser: ran out of stack while reducing"
                 -- After we're done with this token, decide how to continue
                plain newstack = p newstack right_end parser top rest
                mutate newstack pp = p newstack right_end (pat_mutate (pp_pat pp) parser) top rest
                continue newstack = case newstack of
                    [] -> plain newstack
                    res@(PreTree seg last chil) : sttail -> case pp_right last of
                        PPLClosed -> if pp_pat last == top
                            then case sttail of
                                [] -> Right (finish res, rest)  -- Finally return
                                _ -> error "Internal error in the parser: found some forgotten stack"
                            else case pat_dat (pp_pat last) of
                                Just _ -> mutate newstack last
                                Nothing -> mutate sttail last -- discard ignored pattern
                        PPLOpen -> mutate newstack last
                        _ -> plain newstack

 -- The parser uses this
instance MonadPlus (Either a) where
    mplus (Right x) _ = Right x
    mplus (Left _) x = x
    mzero = Left undefined
