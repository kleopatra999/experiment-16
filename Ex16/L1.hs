module Ex16.L1 (
    L1(..), Tree(..), Transformer, trans', trans,
    tr_call, tr_group, tr_parens, tr_block, tr_lambda,
    tr_constraint, tr_method, tr_bind, tr_str, tr_name
) where
import Ex16.TypeTypes
import qualified Ex16.Parser as P

data L1 = L1 P.Segment Tree deriving (Show)
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

type Transformer = [Either String L1] -> Tree

trans' :: P.Tree Transformer () -> Either String L1
trans' (P.Branch seg tr chil) = Right $ L1 seg (tr (map trans' chil))
trans' (P.Leaf seg () src) = Left $ src
trans :: P.Tree Transformer () -> L1
trans t = case trans' t of
    Right x -> x
    _ -> error "Tried to call trans on a Leaf"

tr_call :: Transformer
tr_call [Right a, Right b] = Call a b

tr_parens :: Transformer
tr_parens [Right a] = Parens a

tr_group :: Transformer
tr_group as = Group (map unRight as) where
    unRight (Right x) = x

tr_block :: Transformer
tr_block [Right a] = Block a

tr_lambda :: Transformer
tr_lambda [Right a, Right b] = Lambda a b

tr_constraint :: Transformer
tr_constraint [Right a, Right b] = Constraint a b

tr_method :: Transformer
tr_method [Right a, Right b] = Method a b

tr_bind :: Transformer
tr_bind [Right a, Right b] = Bind a b

tr_str :: Transformer
tr_str [Left str] = Name str  -- TODO make this actually useful

tr_name :: Transformer
tr_name [Left str] = Name str
