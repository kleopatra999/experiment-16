module Ex16.Patterns (
    Pattern(..), pattern_bt, pattern_ns
) where
import Ex16.TypeTypes

data Pattern = PGroup [Pattern]
             | PVar String BoxType
             | PAny String
             | PLit Dynamic

pattern_bt :: Pattern -> BoxType
pattern_bt p = case p of
    PGroup ps -> BTHaskell (TGroup (map pattern_bt ps))
    PVar name bt -> bt
    PAny name -> BTDynamic
    PLit thing -> BTConst thing

pattern_ns :: Pattern -> Namespace
pattern_ns p = case p of
    PGroup ps -> concatMap (\(i, p) -> map (f i) (pattern_ns p)) (kvs ps) where
        f index (name, (bt, get)) = (name, (bt, get . cast (flip group_get index)))
    PVar name bt -> [(name, (bt, id))]
    PAny name -> [(name, (BTDynamic, id))]
    PLit _ -> []
