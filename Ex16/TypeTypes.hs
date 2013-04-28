module Ex16.TypeTypes (
    ClassID, Member, Namespace, Class(..), Dynamic(..), Type(..), BoxType(..), Group(..),
    convert_bt, convert_t
) where
import Control.Arrow
import Unsafe.Coerce
import Data.Array
import Data.Unique

 -- fundamental types

data Unknown

type ClassID = Either Int Unique

type Member = (String, (BoxType, Unknown -> Unknown))
type Namespace = [Member]
data Class = Class ClassID Namespace

data Dynamic = Dynamic Type Unknown

data Type = TGroup [BoxType]  -- TODO: named parameters
          | TClass Class
          | TNil

data BoxType = BTDynamic
             | BTHaskell Type
             | BTConst Dynamic

type Group = Array Int Unknown
group_get :: Group -> Int -> Unknown
group_get = (!)
group1 :: a -> Group
group1 x = listArray (0, 1) [cast x]

convert_bt :: BoxType -> BoxType -> Maybe (Unknown -> Maybe Unknown)
convert_bt BTDynamic BTDynamic = Just $ Just . id
convert_bt BTDynamic (BTHaskell ttyp) = Just (cast f) where
    f :: Dynamic -> Maybe Unknown
    f (Dynamic ftyp dat) = do
        conv <- convert_bt (BTHaskell ftyp) (BTHaskell ttyp)
        conv dat
convert_bt BTDynamic (BTConst lit) = Nothing  -- TODO: make this check for equality
convert_bt (BTHaskell ftyp) BTDynamic = Just $ Just . cast (Dynamic ftyp)
convert_bt (BTHaskell ftyp) (BTHaskell ttyp) = convert_t ftyp ttyp
convert_bt (BTHaskell ftyp) (BTConst lit) = Nothing  -- TODO: make this check for equality
convert_bt (BTConst lit) BTDynamic = Just $ Just . cast (const lit)
convert_bt (BTConst (Dynamic ftyp dat)) (BTHaskell ttyp) = do
    conv <- convert_t ftyp ttyp
    res <- conv dat
    return $ const (Just res)
convert_bt (BTConst flit) (BTConst tlit) = Nothing  -- TODO: make this check for equality

convert_t :: Type -> Type -> Maybe (Unknown -> Maybe Unknown)
convert_t TNil TNil = Just $ Just . id
convert_t TNil (TGroup []) = Just $ Just . const (cast (listArray (0, 0) [] :: Group))
convert_t TNil (TClass _) = Nothing
convert_t (TGroup []) TNil = Just $ Just . const (cast ())
convert_t (TGroup fs) (TGroup ts) = do
    pairs <- zipeq fs ts
    convs <- mapM (uncurry convert_bt) pairs
    return $ cast (convert_group convs)
convert_t (TGroup [from_bt]) to_t@(TClass _) = do
    conv_bt <- convert_bt from_bt (BTHaskell to_t)
    return $ conv_bt . cast ((! 0) :: Group -> Unknown)
convert_t (TGroup _) (TClass _) = Nothing
convert_t (TClass _) TNil = Nothing
convert_t from_t@(TClass _) (TGroup [to_bt]) = do
    conv_bt <- convert_bt (BTHaskell from_t) to_bt
    return $ cast ((>>= Just . group1) . conv_bt)
convert_t (TClass _) (TGroup _) = Nothing
 -- TODO: class-defined conversions
convert_t (TClass (Class aid _)) (TClass (Class bid _)) = if aid == bid then Just (Just . id) else Nothing

convert_group :: [Unknown -> Maybe Unknown] -> Group -> Maybe Group
convert_group convs = \input -> do
    results <- sequence (map (uncurry ($)) (zip convs (elems input)))
    return $ listArray (bounds input) results

cast = unsafeCoerce

zipeq [] [] = Just []
zipeq (x:xs) (y:ys) = fmap ((x, y) :) (zipeq xs ys)
zipeq _ _ = Nothing

kvs :: [a] -> [(Int, a)]
kvs fs = zip (iterate succ 0) fs
