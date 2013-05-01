module Ex16.Types (
    tInt
) where
import Ex16.TypeTypes

tInt :: Class
tInt = Class 1 [
    ("sign", (tInt, cast (signum::Int->Int))),
    ("abs", (tInt, cast (abs::Int->Int))),
    ]
