module Ex16.Trie (
    Trie, empty, null,
    lookup, read, insert, insertWith, delete, alter,
    assocs, keys, elems
) where

import Prelude hiding (lookup, read, null)
import qualified Data.Map as M

data Trie k a = Trie (M.Map k (Trie k a)) (Maybe a)

empty = Trie M.empty Nothing
null (Trie map Nothing) = M.null map
null _ = False

lookup :: Ord k => [k] -> Trie k a -> Maybe a
lookup [] (Trie _ end) = end
lookup (k:ks) (Trie map _) = case M.lookup k map of
    Just next -> lookup ks next
    Nothing -> Nothing

read :: Ord k => Trie k a -> [k] -> Maybe (a, Int)
read trie str = f 0 trie str where
    f i (Trie _ end) [] = fmap (\e -> (e, i)) end
    f i (Trie map end) (k:ks) = case M.lookup k map of
        Just next -> f (succ i) next ks
        Nothing -> fmap (\e -> (e, i)) end

insert :: Ord k => [k] -> a -> Trie k a -> Trie k a
insert = insertWith const

insertWith :: Ord k => (a -> a -> a) -> [k] -> a -> Trie k a -> Trie k a
insertWith f [] val (Trie map (Just end)) = Trie map (Just (f val end))
insertWith f [] val (Trie map Nothing) = Trie map (Just val)
insertWith f (k:ks) val (Trie map end) = let
    alterfunc Nothing = Just (insert ks val empty)
    alterfunc (Just t) = Just (insertWith f ks val t)
    in Trie (M.alter alterfunc k map) end

delete :: Ord k => [k] -> Trie k a -> Trie k a
delete [] (Trie map end) = Trie map Nothing
delete (k:ks) (Trie map end) =
    Trie (M.update (nothing_if_empty . delete ks) k map) end

alter :: Ord k => (Maybe a -> Maybe a) -> [k] -> Trie k a -> Trie k a
alter f [] (Trie map old) = Trie map (f old)
alter f (k:ks) (Trie map end) =
    Trie (M.alter af k map) end where
        af Nothing = f Nothing >>= Just . flip (insert ks) empty
        af (Just t) = nothing_if_empty (alter f ks t)

nothing_if_empty trie = if null trie then Nothing else Just trie

assocs :: Trie k a -> [([k], a)]
assocs (Trie m end) = let
    rest = concatMap f (M.assocs m) where
        f (k, t) = map g (assocs t) where
            g (ks, val) = (k:ks, val)
    in case end of
        Just val -> ([], val) : rest
        Nothing -> rest

keys :: Trie k a -> [[k]]
keys = map fst . assocs

elems :: Trie k a -> [a]
elems (Trie m end) = let
    rest = concatMap elems (M.elems m)
    in case end of
        Just val -> val : rest
        Nothing -> rest

instance (Show k, Show a) => Show (Trie k a) where
    show t = "<Trie " ++ showsPrec 11 (assocs t) ">"
