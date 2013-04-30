module Ex16.L1 (
    L1(..), Tree(..), Transformer,
) where
import Ex16.TypeTypes
import qualified Ex16.Parser as P

data L1 = L1 LC Tree deriving (Show)
data Tree = Call L1 L1
          | Parens L1
          | Group [L1]
          | Block L1
          | Lambda L1 L1
          | Constraint L1 L1
          | Method L1 L1
          | Bind L1 L1
          | Lit Dynamic
          | Name String
--          | L1Public L1
          deriving (Show)

type Transformer = String -> [L1] -> Tree

class Trans a where
    trans :: a -> Transformer

wrong_number x y = "Wrong number of arguments in pt_to_l1 process (" ++ x ++ " /= " ++ y ++ ")"

instance Trans (L1 -> Tree) where
    trans f _ [a] = f a
    trans f _ bad = error $ wrong_number (length bad) 1
instance Trans (L1 -> L1 -> Tree) where
    trans f _ [a, b] = f a b
    trans f _ bad = error $ wrong_number (length bad) 2
instance Trans ([L1] -> Tree) where
    trans f _ args = f args
instance Trans (String -> Tree) where
    trans f src _ = f src

from_ParseTree :: P.Tree -> L1
from_ParseTree (P.Tree (Token lc src tr) chil) =
    L1 lc (tr src (map from_ParseTree chil))
