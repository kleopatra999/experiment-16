module Ex16.Parser.Patterns (
    Pattern(..), id, mos, prec, assoc, ignore, pdat,
    Morpheme(..), pattern, name, index, left, right,
    Assoc(..), Link(..), build, compete, associate
) where

import Prelude hiding (id, Right, Left)
import Data.Function hiding (id)
import Debug.Trace

data Pattern p = Pattern Int [Morpheme p] Double Assoc Bool p deriving (Show)
id     (Pattern x _ _ _ _ _) = x  -- id is unique within one parser
mos    (Pattern _ x _ _ _ _) = x
prec   (Pattern _ _ x _ _ _) = x
assoc  (Pattern _ _ _ x _ _) = x
ignore (Pattern _ _ _ _ x _) = x
pdat   (Pattern _ _ _ _ _ x) = x
instance Eq (Pattern p) where (==) = (==) `on` id

data Assoc = Left | Right | List | Non deriving (Show, Eq)

data Morpheme p = Morpheme (Pattern p) String Int Link Link
pattern (Morpheme x _ _ _ _) = x
name    (Morpheme _ x _ _ _) = x
index   (Morpheme _ _ x _ _) = x
left    (Morpheme _ _ _ x _) = x
right   (Morpheme _ _ _ _ x) = x
instance Show (Morpheme p) where
    show = show . name
instance Eq (Morpheme p) where
    a == b = pattern a == pattern b && index a == index b
instance Ord (Morpheme p) where
    compare = compare `on` \x -> (left x, right x)

data Link = Match1 | Match2 | Open | Closed deriving (Show, Eq, Ord)

build :: Int -> String -> Double -> Assoc -> Bool -> p -> Pattern p
build id name prec assoc ignore pdat = pat where
    pat = Pattern id mos prec assoc ignore pdat
    mos = reverse (scan Closed 0 (words name) [])
    scan left i parts acc = case parts of
        [] -> error $ "Cannot make an empty patttern"
        "_":[] -> error $ "Cannot make a pattern of a single _"
        "_":"_":ps -> scan Open i ("":"_":ps) acc
        "_":ps -> scan Open i ps acc
        p:[] -> Morpheme pat p i left Closed : acc
        p:"_":[] -> Morpheme pat p i left Open : acc
        p:"_":"_":ps -> scan Match2 (succ i) ("":"_":ps) (Morpheme pat p i left Match2 : acc)
        p:"_":ps -> scan Match2 (succ i) ps (Morpheme pat p i left Match2 : acc)
        p:q:ps -> scan Match1 (succ i) (q:ps) (Morpheme pat p i left Match1 : acc)
        
 -- Which of two adjacent morphemes owns the other (or both)
compete :: Morpheme p -> Morpheme p -> Assoc
compete l r = f (right l) (left r) where
    f Open Closed = Right
    f Match2 Closed = Right
    f Closed Open = Left
    f Closed Match2 = Left
    f Match1 Match1 = List
    f _ _ = Non

 -- In "left mid right", whether left or right takes the mid (or both)
associate :: Morpheme p -> Morpheme p -> Assoc
associate l r = f (right l) (left r) where
    f Open Match2 = Left
    f Match2 Open = Right
    f Open Open = case (compare `on` prec . pattern) l r of
        GT -> Left
        LT -> Right
        EQ -> (g `on` assoc . pattern) l r where
            g Left Left = Left
            g List List = List
            g Right Right = Right
            g _ _ = Non
    f Match2 Match2 = match
    f Match1 Match1 = match
    f _ _ = Non
    match = if pattern l == pattern r && index l + 1 == index r
        then List
        else Non

