

import Data.Char
import Data.List
import Data.Maybe
import Data.Array
import GHC.Prim
import qualified Data.Map as Map

 -- utility

(f `on` g) a b = f (g a) (g b)

sp x y z = (x z) (y z)

annotate f x = (f x, x)

kvmap :: (Int -> a -> b) -> [a] -> [b]
kvmap f l = kvmap' f l 0 where
    kvmap' f [] i = []
    kvmap' f (x:xs) i = f i x : kvmap' f xs (i+1)

max_accum :: Ord b => (b, [a]) -> (b, a) -> (b, [a])
max_accum (m, acc) (xv, x) = case compare xv m of
    GT -> (xv, [x])
    EQ -> (m, x : acc)
    LT -> (m, acc)

maxes :: Ord b => (a -> b) -> [a] -> (b, [a])
maxes f (x:xs) = foldl max_accum (f x, [x]) (map (annotate f) xs)

 -- ops

data Op = Op [String] Float Assoc (PT -> AST)
op_parts (Op x _ _ _) = x
op_prec  (Op _ x _ _) = x
op_assoc (Op _ _ x _) = x
op_trans (Op _ _ _ x) = x
instance Show Op where show (Op parts _ _ _) = "«" ++ unwords parts ++ "»"
data Assoc = ALeft
           | ARight
           | ANon
           | AList
           deriving (Show)

op_FILE = Op ["_FILE"] 0 ANon astt_wrap

data TP = TP_Str String
        | TP_ID
        | TP_Num
        | TP_BOF
        | TP_EOF
        deriving (Show, Eq)

initial_chars :: TP -> String
initial_chars TP_Num = "0123456789"
initial_chars TP_ID = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
initial_chars (TP_Str s) = case s of [] -> []; c:cs -> [c]
initial_chars TP_BOF = ""
initial_chars TP_EOF = ""


plen :: TP -> String -> Int
plen (TP_Str ts) str = if ts `isPrefixOf` str then length ts else 0
plen TP_Num str = length (takeWhile isDigit str)
plen TP_ID str = length (takeWhile (\c -> isAlphaNum c || c == '_') str)
plen TP_BOF str = 0
plen TP_EOF str = 0

data TL = TL_None
        | TL_Prec Float Assoc
        | TL_Match TP
        deriving (Show)

data TD = TD Op TP TL TL deriving (Show)
td_op    (TD x _ _ _) = x
td_tp    (TD _ x _ _) = x
td_left  (TD _ _ x _) = x
td_right (TD _ _ _ x) = x

tl_is_none TL_None = True
tl_is_none _ = False

compatible_td :: TD -> TD -> Bool
compatible_td l r = tl_is_none (td_right l) /= tl_is_none (td_left r)

compare_td :: TD -> TD -> Maybe Ordering
compare_td l r = case (td_right l, td_left r) of
    (TL_Prec _ _, TL_Match _) -> Just GT
    (TL_Match _, TL_Prec _ _) -> Just LT
    (TL_Match ltp, TL_Match rtp) -> if ltp == td_tp r && rtp == td_tp l
        then Just EQ
        else Nothing
    (TL_Prec lp la, TL_Prec rp ra) -> case compare lp rp of
        GT -> Just GT
        EQ -> case (la, ra) of
            (ALeft, ALeft) -> Just GT
            (AList, AList) -> Just EQ
            (ARight, ARight) -> Just LT
            (_, _) -> Nothing
        LT -> Just LT
    (_, _) -> Nothing

op_tds :: Op -> [TD]
op_tds op@(Op parts prec assoc trans) = let
    len = length parts
    tps = map (\s -> case s of "_num" -> TP_Num; "_id" -> TP_ID; _ -> TP_Str s) parts
    part_td i "_" = []
    part_td i s = [TD
        op
        (tps !! i)
        (if i == 0 then TL_None else
         if i == 1 then TL_Prec prec assoc else
                        TL_Match (tps !! (i - 2)))
        (if i == len-1 then TL_None else
         if i == len-2 then TL_Prec prec assoc else
                            TL_Match (tps !! (i + 2)))]
    in foldl (++) [] (kvmap part_td parts)

type Token_Map = Map.Map Char [TD]

data Token = Token String [TD] Bool Bool Int Int
token_src      (Token x _ _ _ _ _) = x
token_tds      (Token _ x _ _ _ _) = x
token_ws_left  (Token _ _ x _ _ _) = x
token_ws_right (Token _ _ _ x _ _) = x
token_line     (Token _ _ _ _ x _) = x
token_col      (Token _ _ _ _ _ x) = x
instance Show Token where show = show . token_src

token_BOF = Token "<beginning of file>" [TD op_FILE TP_EOF TL_None (TL_Match TP_BOF)] False False 1 1
token_EOF l c = Token "<end of file>" [TD op_FILE TP_BOF (TL_Match TP_EOF) TL_None] False False l c

tok_with_td :: Token -> TD -> Token
tok_with_td (Token src _ ws_left ws_right line col) td = Token src [td] ws_left ws_right line col

data Token_Error = Token_Unknown_Char Char Int Int
                 | Token_No_Match Char Int Int
                 deriving (Show)

tokenize :: Token_Map -> String -> Either Token_Error [Token] 
tokenize table str = tokenize' table str [token_BOF] True 1 1 where
    tokenize' table [] toks had_ws line col = Right$ reverse (token_EOF line col : toks)
    tokenize' table str@(c:cs) toks had_ws line col =
        if isSpace c
            then if c == '\n'
                then tokenize' table cs toks True (line+1) 1
                else tokenize' table cs toks True line (col+1)
            else case Map.lookup c table of
                Nothing -> Left$ Token_Unknown_Char c line col
                Just [] -> Left$ Token_Unknown_Char c line col
                Just tds -> let
                    (len, cands) = maxes (flip plen str . td_tp) tds
                    (src, rest) = splitAt len str
                    in if len > 0
                        then tokenize' table rest (Token
                            src cands
                            had_ws (null rest || isSpace (head rest))
                            line col : toks) False line (col+len)
                        else Left$ Token_No_Match c line col

data PT = PT Token [PT]  -- Children will end up in reverse order
pt_token    (PT x _) = x
pt_children (PT _ x) = x
instance Show PT where
    show (PT tok chil) = let tp = td_tp (head (token_tds tok)) in if tp == TP_Num || tp == TP_ID
        then show (td_op (head (token_tds tok))) ++ "('" ++ (token_src tok) ++ "')"
        else show (td_op (head (token_tds tok)))
            ++ "("
            ++ intercalate ", " (map show chil)
            ++ ")"

data PT_Error = PT_Misc_Error String
              | PT_Stack_Miss Bool
              | PT_Oops
              | PT_Ambiguity_NYI [TD]
              | PT_Immediate_Mismatch Token Token
              | PT_Far_Mismatch Token Token
              | PT_Precedence_Conflict Token Token
              deriving (Show)


treeize :: [Token] -> Either PT_Error PT
treeize toks = treeize' toks [] where
    ifsingle [] e f = Left$ e
    ifsingle [x] e f = f x
    ifsingle l e f = Left$ PT_Ambiguity_NYI l
    search_for_match [] = error$ "Internal oops: ran out of stack looking for a right matcher."
    search_for_match (PT tok chil:pts) = case td_right (head (token_tds tok)) of
        TL_Match s -> head (token_tds tok)
        _ -> search_for_match pts
    treeize' [] [filept] = Right$ filept
    treeize' [] _ = Left$ PT_Stack_Miss True
    treeize' (tok:toks) [] = treeize' toks [PT tok []]
    treeize' (tok:toks) ptstack@(PT cur_tok chil : ptstacktail) = let
        continue td = if tl_is_none (td_left td)
            then treeize' toks (PT (tok_with_td tok td) [] : ptstack)
            else collapse (tok_with_td tok td : toks) ptstack
        in ifsingle (token_tds cur_tok) PT_Oops
            (\cur_td -> case (filter (compatible_td cur_td) (token_tds tok)) of
                [] -> Left$ PT_Immediate_Mismatch cur_tok tok
                [one] -> continue one
                multiple -> ifsingle
                    (filter (((==) `on` (op_parts . td_op)) (search_for_match ptstack)) multiple)
                    (PT_Far_Mismatch cur_tok tok)
                    continue
            )
    collapse (tok:toks) (st1 : st2@(PT st2tok st2chil) : sttail) =
        case compare_td (head (token_tds st2tok)) (head (token_tds tok)) of
            Nothing -> Left$ PT_Far_Mismatch st2tok tok
            Just GT -> collapse (tok:toks) (PT st2tok (st1 : st2chil) : sttail)
            Just EQ -> treeize' toks (PT tok (st1 : st2chil) : sttail)
            Just LT -> treeize' toks (PT tok [st1] : st2 : sttail)
    collapse _ _ = Left$ PT_Stack_Miss False


 -- Interpretation

data AST = AST_Call AST AST  -- func, group
         | AST_Group [AST]  -- raw or bindings
         | AST_Object [AST]  -- Only bindings allowed
         | AST_Lambda AST AST
--         | AST_Constraint AST AST
         | AST_Method AST String
         | AST_Bind String AST
         | AST_Lit Thing
         | AST_ID String
         | AST_Error ASTE Int Int
         deriving (Show)

data ASTE = AST_Not_A_Name AST deriving (Show)

astize pt@(PT tok chil) = op_trans (td_op (head (token_tds tok))) pt

astt_id (PT id []) = AST_ID$ token_src id
astt_id bad = error$ "astt_id called on weird PT: " ++ show bad
astt_num (PT num []) = AST_Lit (Thing Type_Int (unsafeCoerce# (read (token_src num) :: Int)))
astt_num bad = error$ "astt_num called on weird PT: " ++ show bad
astt_bind (PT tok [def, name]) = case astize name of
    AST_ID new_id -> AST_Bind new_id (astize def)
    bad -> AST_Error (AST_Not_A_Name bad) (token_line tok) (token_col tok)
astt_bind bad = error$ "astt_bind called on weird PT: " ++ show bad
astt_method (PT tok [name, subject]) = case astize name of
    AST_ID method -> AST_Method (astize subject) method
    bad -> AST_Error (AST_Not_A_Name bad) (token_line tok) (token_col tok)
astt_group (PT tok chil) = AST_Group$ map astize (reverse chil)
--astt_object (PT tok chil) = AST_Object$ map astize_binding (reverse chil) where
  --  astize_binding c = case astize c of
    --    AST_Bind _ _ -> c
      --  _ -> error$ "Object may only contain bindings."
astt_func name pt = AST_Call (AST_ID name) (astt_group pt)
astt_wrap (PT tok [child]) = case astize child of
    bind@(AST_Bind _ _) -> AST_Group [bind]
    other -> other
astt_wrap bad = error$ "astt_wrap called on weird PT: " ++ show bad
--astt_constraint (PT tok [typ, name]) = case astize name of
--    AST_ID new_id -> AST_constraint (astize typ)
--    bad -> AST_Error (AST_Not_A_Name bad) (token_line tok) (token_col tok)
--astt_constraint bad = error$ "astt_constraint called on weird PT: " ++ show bad
astt_lambda (PT tok [body, pattern]) = case astize pattern of
    group@(AST_Group _) -> AST_Lambda group (astize body)
    other -> AST_Lambda (AST_Group [other]) (astize body)
astt_lambda bad = error$ "astt_lambda called on weird PT: " ++ show bad
astt_call (PT tok [arg, f]) = case astize arg of
    group@(AST_Group _) -> AST_Call (astize f) group
    other -> AST_Call (astize f) (AST_Group [other])
astt_call bad = error$ "astt_call called on weird PT: " ++ show bad

{-
ast_find_id :: [Namespace] -> String -> AST
ast_find_id [] name subj = error$ "No such name " ++ name
ast_find_id (ns:nses) name subj = case Map.lookup name ns of
    Just _ -> AST_Method subj name
    Nothing -> ast_find_id nses name (AST_Method subj "^^")
ast_resolve nses (AST_Bind name val) = AST_Bind name (ast_resolve nses val)
ast_resolve nses other = other
-}

 -- Types

data Unknown
data Thing = Thing Type Unknown
unk = unsafeCoerce# 0 :: Unknown
instance Show Thing where
    show (Thing (Type_Only val) _) = show val
    show (Thing Type_Int i) = show (unsafeCoerce# i :: Int)
    show (Thing (Type_Func f t) _) = "<" ++ show (Type_Func f t) ++ ">"
    show (Thing (Type_Group ns) group) = case Map.toList ns of
        [] -> "()"
        (h:t) -> "(" ++ pair h ++ concatMap ((", " ++) . pair) t ++ ")" where
            pair (name, Method typ method) =
                let val = show (Thing typ (unsafeCoerce# method group))
                in case name of
                    ('_':num) | all isDigit num -> val
                    _ -> name ++ "=" ++ val
instance Eq Thing where
    Thing at av == Thing bt bv = case (at, bt) of
        (Type_Int, Type_Int) -> (unsafeCoerce# at :: Int) == (unsafeCoerce# bt :: Int)
        (Type_Group ans, Type_Group bns) -> ans == bns
        (Type_Only a, Type_Only b) -> a == b
        (Type_Only a, _) -> a == Thing bt bv
        (_, Type_Only b) -> Thing at av == b
        (_, _) -> False


data Method = Method Type (Unknown -> Unknown)
method_type (Method x _) = x
method_code (Method _ x) = x
method_comp (Method typ f) (Method _ g) = Method typ (f . g)
unkm = unsafeCoerce# 0 :: Unknown -> Unknown
instance Eq Method where Method ta _ == Method tb _ = ta == tb  -- HAAACK
type Namespace = Map.Map String Method
data Type = Type_Group Namespace
          | Type_Func Type Type
          | Type_Int
          | Type_Only Thing
          deriving Eq
instance Show Type where
    show (Type_Only val) = "Only(" ++ show val ++ ")"
    show Type_Int = "Int"
    show (Type_Func from to) = "Func(" ++ show from ++ ", " ++ show to ++ ")"
    show (Type_Group ns) = "(" ++ intercalate ", " (map pair (Map.toList ns)) ++ ")" where
            pair (name, Method typ _) = case name of
                ('_':num) | all isDigit num -> show typ
                _ -> name ++ "=" ++ show typ

convert :: Type -> Type -> Maybe (Unknown -> Unknown)

convert (Type_Only (Thing at av)) bt = convert at bt >>= return . const . ($ av)
 -- Currently this throws away extra positional members; we should fail if there are extras.
convert (Type_Group fromns) (Type_Group tons) = do
    generators <- sequence$ map
        (\(name, Method totype tocode) -> Map.lookup name fromns >>=
            (\(Method fromtype fromcode) -> convert fromtype totype >>= return . (. fromcode)))
        (Map.toList tons)
    return (\bv -> unsafeCoerce#$ listArray (0, length generators - 1) (map ($ bv) generators))
convert a b = if a == b then Just id else Nothing

get_method :: String -> Type -> Method
get_method name (Type_Group ns) = fromJust (Map.lookup name ns)

group_type :: [(String, Type)] -> Type
group_type members = Type_Group$ Map.fromList$
    kvmap (\offset (name, typ) -> (name, Method typ (unsafeCoerce# (! offset)))) members

group_type_map :: (Method -> Method) -> Type -> Type
group_type_map f (Type_Group ns) = Type_Group$ Map.map f ns
group_type_add_ctx :: Type -> Type -> Type
group_type_add_ctx ctxt (Type_Group ns) =
    Type_Group$ Map.insert "^^" (Method ctxt (unsafeCoerce# fst)) $
        Map.map (\(Method typ code) -> Method typ (code . unsafeCoerce# snd)) ns

bindize_members mems = bindize mems 0 where
    bindize :: [AST] -> Int -> [AST]
    bindize [] _ = []
    bindize (bind@(AST_Bind name ast) : mems) i = bind : bindize mems i
    bindize (ast:mems) i = AST_Bind ("_" ++ show i) ast : bindize mems (i+1)

ast_compile :: Type -> AST -> Method
 -- Lit: Simply return the thing
ast_compile ctxt (AST_Lit lit@(Thing typ val)) = Method (Type_Only lit) (const unk)
 -- Call: Calculate type of result, compose code
ast_compile ctxt (AST_Call f args) = case (ast_compile ctxt f, ast_compile ctxt args) of
    (Method (Type_Func from to) f_code, Method argt args_code) -> case convert argt from of
        Just conv ->  Method to (sp (unsafeCoerce# f_code) (conv . args_code))
        Nothing -> error$ "Type error: cannot convert " ++ show argt ++ " to " ++ show from
    (Method othertype _, _) -> error$ "Type error: cannot call non-function type " ++ show othertype
 -- Method: Get type of method in subject ns, compose method with code
ast_compile ctxt (AST_Method subject name) = case ast_compile ctxt subject of
    Method (Type_Group ns) code -> case Map.lookup name ns of
        Nothing -> error$ "Type error: " ++ show (Type_Group ns) ++ " has no member " ++ name
        Just (Method typ method) -> Method typ (method . code)
    Method othertype _ -> error$ "Type error: cannot call method " ++ name ++ " on non-object type " ++ show othertype
 -- Group: Create group type, code creates array matching the type
 -- Haskell's laziness will probably show through.
ast_compile ctxt (AST_Group mems) = let
    (names, methods) = unzip (map (\(AST_Bind n a) -> (n, ast_compile ctxt a)) (bindize_members mems))
    in Method (group_type (zip names (map method_type methods)))
        (unsafeCoerce# (\ctx -> listArray (0, length names - 1) (map (($ ctx) . method_code) methods)))
 -- ID: compile code that calls "^^" on the context until the ID is there
ast_compile (Type_Group ns) (AST_ID name) = case Map.lookup name ns of
    Just method -> method
    Nothing -> case Map.lookup "^^" ns of
        Just (Method ctxt ctxm) -> let
            Method typ code = ast_compile ctxt (AST_ID name)
            in Method typ (code . ctxm)
        Nothing -> error$ "Name error: unknown name " ++ name ++ " in " ++  show (Type_Group ns)
 -- Pattern gives us arg_type and inner_type.
 -- The inner type's repr is the haskell tuple (ctxt, arg_type)
ast_compile ctxt (AST_Lambda pattern body) = let
    (arg_type, pattern_type) = pattern_compile ctxt pattern
    inner_type = group_type_add_ctx ctxt pattern_type
    Method result_type code = ast_compile inner_type body
    in Method (Type_Func arg_type result_type) (unsafeCoerce# (\ctx arg -> code (unsafeCoerce# (ctx, arg))))

ast_compile _ other = error$ "AST error: cannot compile AST by itself: " ++ show other

--pattern_compile ctxt (AST_Group members) = let
--    (arg_types, inner_types) = unzip (map pattern_compile members)


 -- \a=x:Int, b=y:Int -> ...
 -- arg_type should end up (a=Int, b=Int)
 -- inner_type should end up that of (x=ARG.a, y=ARG.b)
 -- types of first binding: (Int, that of (x=ARG.a))
pattern_bind ctxt (AST_Bind name inner) = (name, pattern_compile ctxt inner)
pattern_compile ctxt (AST_Group members) = let
    (arg_names, mem_comps) = unzip (map (pattern_bind ctxt) (bindize_members members))
    (arg_types, inner_types) = unzip mem_comps
    arg_type = group_type (zip arg_names arg_types)
    mem_types_aug = zipWith
        (\arg_name -> group_type_map (flip method_comp (get_method arg_name arg_type)))
        arg_names inner_types
    inner_type = Type_Group$ Map.unions (map (\(Type_Group ns) -> ns) mem_types_aug)
    in (arg_type, inner_type)

pattern_compile ctxt (AST_ID name) =
    (Type_Int,
     Type_Group (Map.singleton name (Method Type_Int id)))
pattern_compile _ _ = error$ "Pattern error: Non-trivial pattern not yet supported."




 -- Builtins


swop = Op . words
swopf name prec assoc = Op (words name) prec assoc (astt_func name)

global_ops = [
    swop "_ . _" 12 ALeft astt_method,
    swop "_ ( _ )" 11 ALeft astt_call,
    swopf "_ ** _" 10 ARight,
    swopf "+ _" 9 ANon,
    swopf "- _" 9 ANon,
    swopf "_ * _" 8 ALeft,
--    swop "_ / _" 8 ALeft,
    swopf "_ + _" 7 ALeft,
    swopf "_ - _" 7 ALeft,
--    swop "_ ?? _ !! _" 6 ARight,
    swop "_ = _" 5 ARight astt_bind,
--    swop "_ :: _" 4 ANon astt_constraint,
    swop "\\ _ -> _" 3 ANon astt_lambda,
    swop "_ , _" 1 AList astt_group,
    swop "( _ )" 0 ANon astt_wrap,
--    swop "{ _ }" 0 ANon astt_object,
    swop "_num" 0 ANon astt_num,
    swop "_id" 0 ANon astt_id]

global_op_table = Map.fromListWith (++)
                $ concatMap (\td -> map (\c -> (c,[td])) (initial_chars (td_tp td)))
                $ concatMap op_tds global_ops


func_int_int_int = Type_Func (Type_Group$ Map.fromList [("_0", Method Type_Int unkm), ("_1", Method Type_Int unkm)]) Type_Int
func_int_int = Type_Func (Type_Group$ Map.fromList [("_0", Method Type_Int unkm)]) Type_Int

add :: Array Int Int -> Int
add x = (x ! 0) + (x ! 1)
sub :: Array Int Int -> Int
sub x = (x ! 0) - (x ! 1)
mul :: Array Int Int -> Int
mul x = (x ! 0) * (x ! 1)
exp0 :: Array Int Int -> Int
exp0 x = (x ! 0) ^ (x ! 1)
plus :: Array Int Int -> Int
plus = (! 0)
minus :: Array Int Int -> Int
minus = negate . (! 0)

ign :: (a -> b) -> (Unknown -> a -> b)
ign f unk = f
cm :: (a -> b) -> (Unknown -> Unknown)
cm = unsafeCoerce# . ign

global_ns :: Namespace
global_ns = Map.fromList [
    ("_ ** _", Method func_int_int_int (cm exp0)),
    ("_ * _", Method func_int_int_int (cm mul)),
    ("_ + _", Method func_int_int_int (cm add)),
    ("_ - _", Method func_int_int_int (cm sub)),
    ("+ _", Method func_int_int (cm plus)),
    ("- _", Method func_int_int (cm minus))]

 -- Main

interp src = case tokenize global_op_table src of
    Left err -> Left$ "Tokenizer error: " ++ show err
    Right toks -> case treeize toks of
        Left err -> Left$ "Treeizer error: " ++ show err
        Right tree -> case ast_compile (Type_Group global_ns) (astize tree) of
            Method typ m -> Right$ Thing typ (unsafeCoerce# m unk)

main = do
    src <- getContents
    putStrLn$ case interp src of
        Left err -> err
        Right result -> show result




