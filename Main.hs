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
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List
import Data.Array
import Data.Ratio
import Prelude hiding (exp)

-- These are integer divisions. Shouldn't need Fractional.
monomialsFromProduct :: (Integral t, Eq t, Fractional s) => s -> t -> t -> t -> t -> [((t, t), s)]
monomialsFromProduct u r s a b =
    monomialsFromProduct' u s a 1 (r+a) (s+b) where
    monomialsFromProduct' :: (Integral n, Eq n, Fractional f) => f -> n -> n -> n -> n -> n -> [((n, n), f)]
    monomialsFromProduct' v 0 _ _ p q = [((p, q), v)]
    monomialsFromProduct' v _ 0 _ p q = [((p, q), v)]
    monomialsFromProduct' v _ _ _ 0 q = [((0, q), v)]
    monomialsFromProduct' v _ _ _ p 0 = [((p, 0), v)]
    monomialsFromProduct' v s' a' t p q = ((p, q), v) :
            monomialsFromProduct' (v*fromIntegral s'*fromIntegral a'/fromIntegral t) (s'-1) (a'-1) (t+1) (p-1) (q-1)

data Weyl a = W (Array (Integer, Integer) a)

fn :: (Ix i, Num e) => Array i e -> i -> e
fn u i = if inRange (bounds u) i then u!i else 0

unionBounds :: (Ix t3, Ix t2, Num t1, Num t) => Array (t2, t3) e1 -> Array (t2, t3) e -> ((t, t1), (t2, t3))
unionBounds u v =
    let ((_, _), (mx, nx)) = bounds u
        ((_, _), (my, ny)) = bounds v
        in ((0, 0), (max mx my, max nx ny))

weylPlus :: Num a => Weyl a -> Weyl a -> Weyl a
weylPlus (W u) (W v) =
    let bz = unionBounds u v
    in W $ accumArray (+) 0 bz [(i, fn u i+fn v i) | i <- range bz]

weylEq :: (Num a, Eq a) => Weyl a -> Weyl a -> Bool
weylEq (W u) (W v) =
    let bz = unionBounds u v
    in and [fn u i == fn v i | i <- range bz]

weylTimes :: (Num a, Fractional a) => Weyl a -> Weyl a -> Weyl a
weylTimes (W u) (W v) =
    let ((_, _), (mx, nx)) = bounds u
        ((_, _), (my, ny)) = bounds v
        bz = ((0, 0), (mx+my, nx+ny))
    in W $ accumArray (+) 0 bz 
        [e | ((r, s), p) <- assocs u,
             ((a, b), q) <- assocs v,
             e <- monomialsFromProduct (p*q) r s a b]

instance (Eq a, Num a) => Eq (Weyl a) where
    (==) = weylEq

instance (Show a, Eq a, Num a, Fractional a) => Show (Weyl a) where
    show = weylShow

monoPower :: Integer -> String -> String
monoPower 0 _ = "1"
monoPower 1 s = s
monoPower n s = s ++ superscript n

weylShow :: (Eq a, Num a, Show a, Fractional a) => Weyl a -> String
weylShow u | u == 0 = "0"
weylShow (W u) = intercalate "+" [show w ++ monoPower i "X" ++ monoPower j "D"|((i, j), w) <- assocs u, w /= 0]

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
    fromInteger u = W $ listArray ((0, 0), (0, 0)) [fromInteger u]
    (+) = weylPlus
    (*) = weylTimes
    negate = scalarTimes (-1)
    abs = error "No abs"
    signum = error "No signum"

instance (Fractional a) => Fractional (Weyl a) where
    fromRational u = W $ listArray ((0, 0), (0, 0)) [fromRational u]
    recip (W u) =
        case assocs u of
            [((0, 0), a)] -> W $ array (bounds u) [((0, 0), recip a)]
            _ -> error "Impossible recip"

-- Power series
convolve :: (Num a, Eq a) => [a] -> [a] -> [a]
~(a:as) `convolve` (b:bs) = (a *! b) :
    map (a !*) bs ^+ (as `convolve` (b:bs))
_ `convolve` _ = error "Impossible convolution"

(*!) :: (Num a, Eq a) => a -> a -> a
(*!) _ 0 = 0
(*!) a b = a*b

(!*) :: (Num a, Eq a) => a -> a -> a
(!*) 0 _ = 0
(!*) a b = a*b

(^+) :: Num c => [c] -> [c] -> [c]
(^+) = zipWith (+)
(^-) :: Num c => [c] -> [c] -> [c]
(^-) = zipWith (-)

newtype Formal a = F [a]

instance (Num r, Eq r) => Num (Formal r) where
    F u+F v  = F $ zipWith (+) u v
    F u-F v  = F $ zipWith (-) u v
    ~(F u)*F v = F $ u `convolve` v -- ~
    fromInteger u = F $ fromInteger u:repeat 0
    negate (F u) = F $ map negate u
    signum (F []) = error "Impossible!"
    signum (F (u:_)) = F $ signum u:repeat 0
    abs _   = error "Can't form abs of a power series"

instance (Eq r, Fractional r) => Fractional (Formal r) where
    (/) = error "No (/)"
    fromRational u = F $ fromRational u:repeat 0

z :: Num a => Formal a
z = F $ [0, 1] ++ repeat 0

deriv :: Num a => Formal a -> Formal a
deriv (F (_ : u)) = F $ zipWith (*) (map fromInteger [1..]) u
deriv _ = error "Impossible deriv"

integrate :: Fractional a => Formal a -> Formal a
integrate (F u) = F $ 0 : zipWith (*) (map ((1 /) . fromInteger) [1..]) u

exp :: (Eq a, Num a, Fractional a) => Formal a -> Formal a
exp u = e where e = 1+integrate (e * deriv u)

star :: (Eq a, Num a, Fractional a) => Formal a -> Formal a
star u = e where e = 1+e*u

superDigits :: String
superDigits = "⁰¹²³⁴⁵⁶⁷⁸⁹"
subDigits :: String
subDigits = "₀₁₂₃₄₅₆₇₈₉"

superscript :: Integer -> String
superscript 0 = "⁰"
superscript n = script superDigits n

subscript :: Integer -> String
subscript 0 = "⁰"
subscript n = script subDigits n

script :: String -> Integer -> String
script _ 0 = ""
script digits n = script digits (n `div` 10) ++ [digits!!fromIntegral (n `mod` 10)]

sample :: Int -> Formal a -> [a]
sample n (F xs) = take n xs

main ::  IO ()
main = do
--     print $ monomialsFromProduct 1 0 3 2 0
--     print $ weylPlus d x
--     print $ weylTimes d d
--     print $ (x, x*x, x*x*x)
--     print $ (d, d*d, d*d*d)
    let xx = F (x : repeat 0) :: Formal (Weyl Rational)
    let dd = F (d : repeat 0) :: Formal (Weyl Rational)
--     print $ take 10 $ exp (z*(ix+id))
    let a = exp (fromRational (1%2)*z*z)*exp(z*xx)*exp(z*dd)
    let b = exp (z*(xx+dd))
    print $ sample 10 (a-b)
--     print $ take 4 a
--     let c = exp (z*(ix*id*id+ix))
--     print $ take 5 c
--    print $ (exp ([0, 1] ++ repeat 0) :: [Float])

    let q = star (z*(xx+dd))
    print $ sample 15 q

