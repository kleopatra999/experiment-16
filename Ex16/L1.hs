module Ex16.L1 (
    L1(..)
) where
import Ex16.TypeTypes
import Ex16.Parser

data L1 = L1 LC L1Tree deriving (Show)
data L1Tree = L1Call L1 L1
            | L1Parens L1
            | L1Group [L1]
            | L1Block L1
            | L1Lambda L1 L1
            | L1Constraint L1 L1
            | L1Method L1 L1
            | L1Bind L1 L1
            | L1Lit Dynamic
            | L1Name String
--            | L1Public L1
            deriving (Show)

type PT_To_L1 = String -> [L1] -> L1
type L1_ParseTree = ParseTree PT_To_L1

class L1Trans a where
    pt_to_l1 :: a -> PT_To_L1

wrong_number x y = "Wrong number of arguments in pt_to_l1 process (" ++ x ++ " /= " ++ y ++ ")"

instance L1Trans (L1 -> L1) where
    pt_to_l1 f _ [a] = f a
    pt_to_l1 f _ bad = error $ wrong_number (length bad) 1
instance L1Trans (L1 -> L1 -> L1) where
    pt_to_l1 f _ [a, b] = f a b
    pt_to_l1 f _ bad = error $ wrong_number (length bad) 2
instance L1Trans ([L1] -> L1) where
    pt_to_l1 f _ args = f args
instance L1Trans (String -> L1) where
    pt_to_l1 f src _ = f src

