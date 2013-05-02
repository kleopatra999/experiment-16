module Ex16.Tap (
    Test(..), plan, ok, is, isn't, diag, test_to_string, run_test
) where

data Test a = Test (Int -> (Int, [String], a))
test_f (Test f) = f
instance Monad Test where
    Test f >>= g = Test h where
        h start = let
            (mid, left, x) = f start
            (end, right, y) = test_f (g x) mid
            in (end, left ++ right, y)
    return x = Test (\s -> (s, [], x))

plan :: Int -> Test ()
plan i = Test f where f s = (s, ["1.." ++ show i], ())

ok :: Bool -> String -> Test Bool
ok b m = Test f where
    f s = (succ s, [res ++ show s ++ "  # " ++ m], b)
    res = if b then "ok " else "not ok "

is :: (Eq a, Show a) => a -> a -> String -> Test Bool
is l r m = Test f where
    b = l == r
    f s = (succ s, res s, b)
    res s = if b
        then ["ok " ++ show s ++ "  # " ++ m]
        else ["not ok " ++ show s ++ "  # " ++ m, " # expected " ++ show r, " #  but got " ++ show l]

isn't :: Eq a => a -> a -> String -> Test Bool
isn't l r m = ok (l /= r) m

diag :: String -> Test ()
diag m = Test f where f s = (s, map (" # " ++) (lines m), ())

test_to_string :: Test a -> String
test_to_string (Test f) = let
    (_, l, _) = f 0
    in unlines l

run_test :: Test a -> IO a
run_test (Test f) = do
    let (end, l, r) = f 0
    putStr (unlines l)
    return r

