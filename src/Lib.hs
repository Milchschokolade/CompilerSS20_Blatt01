-- / A Lib module.
module Lib
    ( someFunc
    , square
    , ggT
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- / Calculate the square of a number
square :: Num a => a -- ^ the number
    -> a -- ^ the square
square a = a * a

-- / Calculates the greatest common divisor of two Integers using modern eukledian algorithm with recursion
ggT ::
    Integer -- ^ the first Integer 
    -> Integer -- ^ the second Integer
    -> Integer -- ^ the greatest common divisor
ggT a b = do 
    if b == 0
        then abs a
    else do
        let d = abs b
        let c = (abs a `mod` d)
        ggT d c