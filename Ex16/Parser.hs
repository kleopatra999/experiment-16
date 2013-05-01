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
import Data.Either
import Data.Function
import Control.Monad
import Debug.Trace

data Assoc = ALeft | ARight | AList | ANon
           deriving (Show, Eq)

type Mutator g p t = Parser g p t -> Parser g p t

 -- A mixfix op composed of tokens and open slots
data Pat g p t = Pat Int String Double Assoc (Maybe (g -> p))
pat_id    (Pat x _ _ _ _) = x
pat_name  (Pat _ x _ _ _) = x
pat_prec  (Pat _ _ x _ _) = x
pat_assoc (Pat _ _ _ x _) = x
pat_dat   (Pat _ _ _ _ x) = x
pat_ignore = isNothing . pat_dat
instance Eq (Pat g p t) where (==) = (==) `on` pat_id
instance Show p => Show (Pat g p t) where
    show pat = "<Pat " ++ show (pat_name pat) ++ " >"

 -- pats are broken up into these.
data PP g p t = PP (Pat g p t) Int PPLink PPLink (Mutator g p t) (Maybe (g -> t))
pp_pat    (PP x _ _ _ _ _) = x
pp_index  (PP _ x _ _ _ _) = x
pp_left   (PP _ _ x _ _ _) = x
pp_right  (PP _ _ _ x _ _) = x
pp_mutate (PP _ _ _ _ x _) = x
pp_dat    (PP _ _ _ _ _ x) = x
instance Eq (PP g p t) where
    a == b = pp_pat a == pp_pat b && pp_index a == pp_index b
instance Show (PP g p t) where
    show pp = "<PP " ++ show (pat_name (pp_pat pp)) ++ " " ++ show (pp_index pp) ++ ">"

 -- The left or right side of a PP, in preference order
data PPLink = PPLMatch1 | PPLMatch2 | PPLOpen | PPLClosed
            deriving (Show, Eq, Ord)
 -- First prioritize left, then right.
instance Ord (PP g p t) where
    compare = compare `on` \x -> (pp_left x, pp_right x)

associate :: PP g p t -> PP g p t -> Assoc
associate l r = f (pp_right l) (pp_left r) where
    f PPLOpen PPLMatch2 = ALeft
    f PPLMatch2 PPLOpen = ARight
    f PPLOpen PPLOpen = case (compare `on` pat_prec . pp_pat) l r of
        GT -> ALeft
        LT -> ARight
        EQ -> (g `on` pat_assoc . pp_pat) l r where
            g ALeft ALeft = ALeft
            g AList AList = AList
            g ARight ARight = ARight
            g _ _ = ANon
    f PPLMatch2 PPLMatch2 = match
    f PPLMatch1 PPLMatch1 = match
    f _ _ = ANon
    match = if pp_pat l == pp_pat r && pp_index l + 1 == pp_index r
        then AList
        else ANon

 -- This mediates between insert_token and the lexer's custom tokens
data Token g t a = Token (Maybe a) (String -> Maybe (String, String)) (Maybe (g -> t))
instance L.Tokenizer (Token g t) where
    read (Token maybe_res reader _) str = do
        res <- maybe_res
        (got, rest) <- reader str
        return (res, got, rest)
instance (Show a) => Show (Token g t a) where
    show (Token adat reader tdat) = "<Token " ++ showsPrec 11 adat ">"

 -- The actual parser object
data Parser g p t = Parser Int (M.Map String (Pat g p t)) (L.Lexer (Token g t) [PP g p t]) g
                  deriving (Show)

pats       (Parser _ x _ _) = x
lexer      (Parser _ _ x _) = x
parser_dat (Parser _ _ _ x) = x

empty dat = Parser 0 M.empty L.empty dat
null (Parser _ pats lexer _) = M.null pats && L.null lexer

set_dat dat (Parser curid pats lexer _) = Parser curid pats lexer dat

insert_token :: String -> (String -> Maybe (String, String)) -> Maybe (g -> t) -> Mutator g p t
insert_token name reader tdat (Parser curid pats lexer gdat) =
    Parser curid pats (L.insert_custom name (Token Nothing reader tdat) lexer) gdat

lookup :: String -> Parser g p t -> Maybe (Pat g p t)
lookup name = M.lookup name . pats

insert :: String -> Double -> Assoc -> (g -> p) -> Mutator g p t
insert name prec assoc dat = insert_internal name prec assoc (const id) (Just dat)

insert_ignore :: String -> Double -> Assoc -> Mutator g p t
insert_ignore name prec assoc = insert_internal name prec assoc (const id) Nothing

insert_mutator :: String -> Double -> Assoc -> (Int -> Mutator g p t) -> (g -> p) -> Mutator g p t
insert_mutator name prec assoc f dat = insert_internal name prec assoc f (Just dat)

insert_mutator_ignore :: String -> Double -> Assoc -> (Int -> Mutator g p t) -> Mutator g p t
insert_mutator_ignore name prec assoc f = insert_internal name prec assoc f Nothing

insert_internal :: String -> Double -> Assoc -> (Int -> Mutator g p t) -> Maybe (g -> p) -> Mutator g p t
insert_internal name prec assoc mut pdat (Parser curid pats lexer gdat) =
    guard (Parser (succ curid) (M.insert name pat pats) (scan PPLClosed 0 parts lexer) gdat) where
        pat = Pat curid name prec assoc pdat
        parts = words name
        guard = if isJust pdat || (parts /= [] && head parts /= "_" && last parts /= "_")
            then id
            else error "Cannot specify an ignored pattern with an open end"
        scan left i parts l = let
            part = head parts
            pp right = PP pat i left right (mut i)
            one right = case L.lookup_custom part lexer of
                Just (Token (Just old) reader tdat) ->
                    L.adjust_custom (const (Token (Just new) reader tdat)) part l where
                        new = List.insert (pp right tdat) old
                Just (Token Nothing reader tdat) ->
                    L.adjust_custom (const (Token (Just [pp right tdat]) reader tdat)) part l
                Nothing -> L.insertWith (List.insert . head) part [pp right Nothing] l
            openleft = scan PPLOpen i (tail parts) l
            closed = one PPLClosed
            open = one PPLOpen
            match2 = scan PPLMatch2 (succ i) (tail (tail parts)) (one PPLMatch2)
            match1 = scan PPLMatch1 (succ i) (tail parts) (one PPLMatch1)
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

 -- Various ways a parse can fail
data Error g p t = NoTokenMatch LC
                 | BadBeginning LC (PP g p t)
                 | Mismatch Segment (PP g p t) (PP g p t)
                 | NoMatch [Error g p t]
                 | CatPatNYI LC
                 deriving (Show)

 -- Line and column
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

 -- This is a structure internal to the parser, representing an incomplete parse tree
data PreTree g p t = PreTree Segment (PP g p t) [Tree p t] deriving (Show)
pt_start (PreTree (x, _) _ _) = x
pt_end   (PreTree (_, x) _ _) = x
pt_pp    (PreTree (_, _) x _) = x
pt_chil  (PreTree (_, _) _ x) = x  -- children are stored in reverse order
pt_pat = pp_pat . pt_pp
pt_left = pp_left . pt_pp
pt_right = pp_right . pt_pp
pt_ignore = pat_ignore . pp_pat . pt_pp
 -- Do something with a pretree and a stack
 -- The parser must be fed through so that the g -> p owned
 --  by patterns can be executed
pt_apply :: Parser g p t -> PreTree g p t -> [PreTree g p t]
         -> Either (Error g p t) (PreTree g p t, [PreTree g p t])
pt_apply parser right [] = case pt_left right of
    PPLClosed -> Right $ (right, [])
    _         -> Left $ merge_badbeginning right
pt_apply parser right (left:stack) =
    decide (pt_right left) (pt_left right) where
         -- Decide action based on the token links
        decide _         PPLClosed
                 | pt_ignore right = Right $ (right, left:stack)
        decide PPLClosed PPLClosed = Left $ CatPatNYI (pt_start right)
        decide PPLClosed PPLOpen   = reduce left stack
        decide PPLClosed PPLMatch2 = reduce left stack
        decide PPLOpen   PPLClosed = Right $ (right, left:stack)
        decide PPLMatch2 PPLClosed = Right $ (right, left:stack)
        decide PPLMatch1 PPLMatch1 = Right $ (merge_2 left right, stack)
        decide _         _         = Left $ merge_mismatch left right
        reduce mid [] = case pt_left right of
            PPLOpen -> Right $ (merge_right mid right, [])
            _       -> Left $ merge_badbeginning right
        reduce mid (left:stack) = case associate (pt_pp left) (pt_pp right) of
            ANon    -> Left $ merge_mismatch left right
            ALeft   -> reduce (merge_left left mid) stack
            AList   -> Right $ (merge_3 left mid right, stack)
            ARight  -> Right $ (merge_right mid right, left : stack)
         -- Actions
        merge_left left mid =  -- left adopts middle
            PreTree (pt_start left, pt_end mid)
                    (pt_pp left)
                    (pt_finish parser mid : pt_chil left)
        merge_right mid right =  -- right adopts middle
            PreTree (pt_start mid, pt_end right)
                    (pt_pp right)
                    (pt_chil right ++ [pt_finish parser mid])
        merge_2 left right =  -- left and right join
            PreTree (pt_start left, pt_end right)
                    (pt_pp right)
                    (pt_chil right ++ pt_chil left)
        merge_3 left mid right =  -- left and right join and adopt middle
            PreTree (pt_start left, pt_end right)
                    (pt_pp right)
                    (pt_chil right ++ (pt_finish parser mid : pt_chil left))
 -- For converting a PreTree into a Tree
pt_finish parser (PreTree seg left chil) = case pat_dat (pp_pat left) of
    Just f -> Branch seg (f (parser_dat parser)) (reverse chil)
    Nothing -> error $ "Internal parser error: A PreTree that should have been ignored wasn't."
 -- Bad scenarios
merge_mismatch left right =
    Mismatch (pt_end left, pt_start right) (pt_pp left) (pt_pp right)
merge_badbeginning right = BadBeginning (pt_start right) (pt_pp right)

parse :: (Show g, Show p, Show t)
      => Parser g p t
      -> Pat g p t
      -> String
      -> Either (Error g p t) (Tree p t, String)
parse parser top str = parse' parser top (1, 1) str [] where
    parse' :: (Show g, Show p, Show t)
           => Parser g p t
           -> Pat g p t
           -> LC
           -> String
           -> [PreTree g p t]
           -> Either (Error g p t) (Tree p t, String)
    parse' parser top start str stack = case L.read (lexer parser) str of
        Nothing -> Left $ NoTokenMatch start
        Just (meanings, got, rest) -> single (map try meanings) where
            single results = foldl mplus (Left (NoMatch (lefts results))) results
            try part = do
                let end = inc_lc start got
                    segment = (start, end)
                    leaf = liftMaybe (flip (Leaf segment) got . ($ parser_dat parser)) (pp_dat part)
                    preapplied = PreTree segment part leaf
                (applied, newstack) <- pt_apply parser preapplied stack
                let newparser = pp_mutate (pt_pp applied) parser
                    continue = parse' newparser top end rest
                    closed = pt_right applied == PPLClosed
                    ignore = closed && pat_ignore (pt_pat applied)
                    finished = closed && pt_pat applied == top
                if finished
                    then if List.null newstack
                        then return (pt_finish newparser applied, rest)
                        else error "Internal parser error: had some forgotten stack left over"
                    else if ignore
                        then continue newstack
                        else continue (applied : newstack)

 -- Some nice functions for Eithers and stuff, for the parser
instance MonadPlus (Either a) where
    mplus (Right x) _ = Right x
    mplus (Left _) x = x
    mzero = Left undefined

liftMaybe :: MonadPlus m => (a -> b) -> Maybe a -> m b
liftMaybe f Nothing = mzero
liftMaybe f (Just x) = return (f x)
