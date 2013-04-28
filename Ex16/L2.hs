module Ex16.L2 (
    L2(..)
) where
import Ex16.TypeTypes
import Ex16.L1

data L2 = L2Call L2 L2
        | L2Group [L2]
        | L2Block [L2]
        | L2Lambda L2 L2
        | L2Constraint L2 L2
        | L2Method L2 String
        | L2Bind Bool String L2
        | L2Lit Dynamic
        | L2Name String

L2Error = L2BadMethod LC

l1_to_l2 :: L1 -> Either L2Error L2
l1_to_l2 (L1 lc x) = let
    rec1 = fmap
    rec2 wrap a b = do
        a' <- l1_to_l2 a
        b' <- l1_to_l2 b
        return $ wrap a' b'
    reclist wrap as = fmap wrap (mapM l1_to_l2 as)
    in case x of
        L1Call f args -> rec2 L2Call f args
        L1Parens inner -> l1_to_l2 inner
        L1Group elems -> reclist L2Group elems
        L1Block (L1Group elems) -> reclist L2Block elems
        L1Block elem -> rec1 (L2Block . (:[])) elem
        L1Lambda params body -> rec2 L2Lambda params body
        L1Constraint subj typ -> rec2 L2Constraint subj typ
        L1Method subj (L1Name name) -> rec1 (flip L2Method name) subj
        L1Method subj _ -> Left $ L2BadMethod lc
        L1Bind (L1Name name) rhs -> rec1 (L2Bind False name) rhs
        L1Lit dyn -> Right $ L2Lit dyn
        L1Name name -> Right $ L2Name name

