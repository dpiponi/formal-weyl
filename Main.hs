{-

1  2  1
1  2  1
0! 1! 2!

1*1/1                                    1 

(1*2)*(1*2)/(1)                *2*2/1    = 4

(1*2*1)*(1*2*1)/(1*2)          *1*1/2    = 2

(2 0) (2 0) 0! = 1
(2 1) (2 1) 1! = 4
(2 2) (2 2) 2! = 2

DDXX = D(DXX) = D(2X+XXD) = 2(1+XD)+(DXX)D
= 2+2XD+(2X+XXD)D = 2+2XD+2XD+XXDD = 2+4XD+XXDD

(3 0) (3 0) 0! = 1
(3 1) (3 1) 1! = 9
(3 2) (3 2) 2! = 18
(3 3) (3 3) 3! = 6

(2 0) (3 0) 0! = 1
(2 1) (3 1) 1! = 6
(2 2) (3 2) 2! = 6


 -}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List
import Data.Array
import Data.Ratio
import Prelude hiding (exp)

-- These are integer divisions. Shouldn't need Fractional.
monomialsFromProduct :: (Integral t, Eq t, Fractional s) => s -> t -> t -> t -> t -> [((t, t), s)]
monomialsFromProduct x r s a b =
    monomialsFromProduct' x s a 1 (r+a) (s+b) where
    monomialsFromProduct' :: (Integral n, Eq n, Fractional f) => f -> n -> n -> n -> n -> n -> [((n, n), f)]
    monomialsFromProduct' x 0 _ _ p q = [((p, q), x)]
    monomialsFromProduct' x _ 0 _ p q = [((p, q), x)]
    monomialsFromProduct' x _ _ _ 0 q = [((0, q), x)]
    monomialsFromProduct' x _ _ _ p 0 = [((p, 0), x)]
    monomialsFromProduct' x s' a' t p q = ((p, q), x) :
            monomialsFromProduct' (x*fromIntegral s'*fromIntegral a'/fromIntegral t) (s'-1) (a'-1) (t+1) (p-1) (q-1)

data Weyl a = W (Array (Integer, Integer) a)

weylPlus :: Num a => Weyl a -> Weyl a -> Weyl a
weylPlus (W x) (W y) =
    let bx@((_, _), (mx, nx)) = bounds x
        by@((_, _), (my, ny)) = bounds y
        fx i = if inRange bx i then x!i else 0
        fy i = if inRange by i then y!i else 0
        bz = ((0,0), (max mx my, max nx ny))
    in W $ accumArray (+) 0 bz [(i, fx i+fy i) | i <- range bz]

weylTimes :: (Num a, Fractional a) => Weyl a -> Weyl a -> Weyl a
weylTimes (W x) (W y) =
    let bx@((_, _), (mx, nx)) = bounds x
        by@((_, _), (my, ny)) = bounds y
        bz = ((0, 0), (mx+my, nx+ny))
    in W $ accumArray (+) 0 bz $ 
        [p | ((r, s), u) <- assocs x,
             ((a, b), v) <- assocs y,
             p <- monomialsFromProduct (u*v) r s a b]

weylEq :: (Num a, Eq a) => Weyl a -> Weyl a -> Bool
weylEq (W x) (W y) =
    let bx@((_, _), (mx, nx)) = bounds x
        by@((_, _), (my, ny)) = bounds y
        fx i = if inRange bx i then x!i else 0
        fy i = if inRange by i then y!i else 0
        bz = ((0, 0), (max mx my, max nx ny))
    in and $ [fx i == fy i | i <- range bz]

instance (Eq a, Num a) => Eq (Weyl a) where
    (==) = weylEq

instance (Show a, Eq a, Num a, Fractional a) => Show (Weyl a) where
    show = weylShow

monoPower :: Integer -> String -> String
monoPower 0 s = "1"
monoPower 1 s = s
monoPower n s = s ++ superscript n

weylShow :: (Eq a, Num a, Show a, Fractional a) => Weyl a -> String
weylShow x | x == 0 = "0"
weylShow (W x) = intercalate "+" [show w ++ "*" ++monoPower i "X" ++ "*" ++ monoPower j "D"|((i, j), w) <- assocs x,
                                                             w /= 0]

--class ScalarTimes a b where
--    scalarTimes :: a -> b -> b

--instance Num a => ScalarTimes a (Weyl a) where
scalarTimes :: Num a => a -> Weyl a -> Weyl a
scalarTimes s (W t) = W $ fmap (s *) t

one, d, x :: Num a => Weyl a
one = W $ listArray ((0, 0), (0, 0)) [1]
d = W $ listArray ((0, 0), (0, 1)) [0, 1]
x = W $ listArray ((0, 0), (1, 0)) [0, 1]

instance (Fractional a, Num a) => Num (Weyl a) where
    fromInteger x = W $ listArray ((0, 0), (0, 0)) [fromInteger x]
    (+) = weylPlus
    (*) = weylTimes
    negate = scalarTimes (-1)
    abs = error "No abs"
    signum = error "No signum"

instance (Fractional a) => Fractional (Weyl a) where
    fromRational x = W $ listArray ((0, 0), (0, 0)) [fromRational x]
    recip (W x) =
        case assocs x of
            [((0, 0), a)] -> W $ array (bounds x) [((0, 0), recip a)]

-- Power series
~(a:as) `convolve` (b:bs) = (a *! b):
    ((map (a !*) bs) ^+ (as `convolve` (b:bs)))

(*!) _ 0 = 0
(*!) a b = a*b

(!*) 0 _ = 0
(!*) a b = a*b

(^+) a b = zipWith (+) a b
(^-) a b = zipWith (-) a b

newtype Formal a = F [a]

instance (Num r, Eq r) => Num (Formal r) where
    F x+F y  = F $ zipWith (+) x y
    F x-F y  = F $ zipWith (-) x y
    ~(F x)*F y = F $ x `convolve` y -- ~
    fromInteger x = F $ fromInteger x:repeat 0
    negate (F x)     = F $ map negate x
    signum (F (x:_)) = F $ signum x:repeat 0
    abs (F (x:xs))   = error "Can't form abs of a power series"

instance (Eq r, Fractional r) => Fractional (Formal r) where
    (/) = error "No (/)"
    fromRational x = F $ fromRational x:repeat 0

z :: Num a => Formal a
z = F $ [0, 1] ++ repeat 0

deriv :: Num a => Formal a -> Formal a
deriv (F (_ : x)) = F $ zipWith (*) (map fromInteger [1..]) x

integrate :: Fractional a => Formal a -> Formal a
integrate (F x) = F $ 0 : zipWith (*) (map ((1 /) . fromInteger) [1..]) x

exp :: (Eq a, Num a, Fractional a) => Formal a -> Formal a
exp x = e where e = 1+integrate (e * deriv x)

star :: (Eq a, Num a, Fractional a) => Formal a -> Formal a
star x = e where e = 1+e*x

superDigits = "⁰¹²³⁴⁵⁶⁷⁸⁹"
subDigits = "₀₁₂₃₄₅₆₇₈₉"

superscript :: Integer -> String
superscript 0 = "⁰"
superscript n = script superDigits n

subscript :: Integer -> String
subscript 0 = "⁰"
subscript n = script subDigits n

script :: String -> Integer -> String
script digits 0 = ""
script digits n = script digits (n `div` 10) ++ [digits!!(fromIntegral (n `mod` 10))]

sample n (F xs) = take n xs

main ::  IO ()
main = do
--     print $ monomialsFromProduct 1 0 3 2 0
--     print $ weylPlus d x
--     print $ weylTimes d d
    print $ (x, x*x, x*x*x)
    print $ (d, d*d, d*d*d)
    let ix = F $ ([x] ++ repeat 0) :: Formal (Weyl (Ratio Integer))
    let id = F $ ([d] ++ repeat 0) :: Formal (Weyl (Ratio Integer))
--     print $ take 10 $ exp (z*(ix+id))
    let a = exp (fromRational (1%2)*z*z)*exp(z*ix)*exp(z*id)
    let b = exp (z*(ix+id))
--     print $ take 10 (a-b)
--     print $ take 4 a
--     let c = exp (z*(ix*id*id+ix))
--     print $ take 5 c
--    print $ (exp ([0, 1] ++ repeat 0) :: [Float])

    let q = star (z*(ix+id))
    print $ sample 15 q

