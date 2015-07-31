-- Formal power series.
-- Note the big caveat for division.
-- Probably best to use division only when constant term
-- of divisor is non-zero.

{-# LANGUAGE FlexibleInstances #-}

module Poly where

import Data.Array
import Data.List
import Data.Ratio
import Data.Tuple
import Debug.Trace
import Control.Applicative
import LeftDivide
import Padded
import Over

import MShow
import ISqrt2

data Poly a = P (Array Integer a)

{-
monomialTimes :: (Fractional f) =>
    f -> Integer -> Integer -> Integer -> Integer -> [((Integer, Integer), f)]
monomialTimes u r s a b =
    monomialTimes' u s a 1 (r+a) (s+b) where
    monomialTimes' :: (Fractional f) =>
        f -> Integer -> Integer -> Integer -> Integer -> Integer -> [((Integer, Integer), f)]
    monomialTimes' v 0 _ _ p q = [((p, q), v)]
    monomialTimes' v _ 0 _ p q = [((p, q), v)]
    monomialTimes' v _ _ _ 0 q = [((0, q), v)]
    monomialTimes' v _ _ _ p 0 = [((p, 0), v)]
    monomialTimes' v s' a' t p q = ((p, q), v) :
        monomialTimes' (v*fromIntegral s'*fromIntegral a'/fromIntegral t)
                       (s'-1) (a'-1) (t+1) (p-1) (q-1)
-}

combineBounds :: (Integer -> Integer -> Integer) ->
                 Array Integer e1 -> Array Integer e ->
                 (Integer, Integer)
combineBounds f u v =
    let (_, nx) = bounds u
        (_, ny) = bounds v
        in (0, f nx ny)

{-
fn :: (Ix i, Num e) => Array i e -> i -> e
fn u i = if inRange (bounds u) i then u!i else 0
-}

polyPlus :: Num a => Poly a -> Poly a -> Poly a
polyPlus (P u) (P v) =
    let bz = combineBounds max u v
    in P $ accumArray (+) 0 bz $ assocs u ++ assocs v

polyEq :: (Show a, Num a, Eq a) => Poly a -> Poly a -> Bool
polyEq (P u) (P v) = 
    let bz = combineBounds max u v
--     in and [fn u i == fn v i | i <- range bz]
    in and $ map snd $ padded $ (==) <$> (assocs u :- 0) <*> (assocs v :- 0)

polyTimes :: (Num a, Fractional a) => Poly a -> Poly a -> Poly a
polyTimes (P u) (P v) =
    let bz = combineBounds (+) u v
    in P $ accumArray (+) 0 bz
        [e | (r, p) <- assocs u,
             (a, q) <- assocs v,
             let e = (r+a, p*q)]

-- in (i, j): i labels row, j column
-- XXX What if list is empty?
startsWithZero :: (Num a, Eq a) => Poly a -> Bool
startsWithZero (P ws) = ws!0 == 0

removeFirst :: (Num a) => Poly a -> Poly a
removeFirst (P ws) =
    let (_, n) = bounds ws
    in P $ ixmap (0, n-1) (\i -> i+1) ws

instance (Show a, Eq a, Num a) => Eq (Poly a) where
    (==) = polyEq

instance (Show a, Eq a, Fractional a, Num a, MShow a) => MShow (Poly a) where
    mshow' n x = parens 6 n (mshow x) 
    mshow = polyShow

instance (Show a, Eq a, Fractional a, Num a, MShow a) => Show (Poly a) where
    show = mshow

monoPower :: Integer -> String -> String
monoPower 0 _ = ""
monoPower 1 s = s
monoPower n s = s ++ superscript n

polyRational :: Rational -> String
polyRational = mshow

polyRational' 1 = ""
polyRational' u = polyRational u

data Monomial a = M Integer a

instance (Eq a, MShow a, Num a) => MShow (Monomial a) where
    mshow' n w = parens 7 n (mshow w)
    mshow (M i w) = atLeast "1" ((if w == 1 then "" else mshow' 7 w) ++
                                      monoPower i "X")

-- Can use mshow' 6 for terms with i == j == 0
polyShow :: (Show a, Fractional a, MShow a, Num a, Eq a) => Poly a -> String
polyShow u | u == 0 = "0"
polyShow (P u) = sumMShow 0 -- ??
                    [M i w |
                     (i, w) <- assocs u,
                     w /= 0]

polyShow' :: (Show a, Fractional a, Eq a, MShow a, Num a) => Poly a -> String
polyShow' u = atLeast "0" $ polyShow u

scalarTimes :: Num a => a -> Poly a -> Poly a
scalarTimes s (P t) = P $ fmap (s *) t

one, x :: Num a => Poly a
one = P $ listArray (0, 0) [1]
x = P $ listArray (0, 1) [0, 1]

injectP :: Num a => a -> Poly a
injectP u = P $ listArray ((0, 0)) [u]

instance Over Poly where
    ι = injectP

instance (Fractional a, Num a) => Num (Poly a) where
    fromInteger = injectP . fromInteger
    (+) = polyPlus
    (*) = polyTimes
    negate = scalarTimes (-1)
    abs = error "I don't know what abs would mean here."
    signum = error "Pretty sure signum makes no sense here."
    
-- a `wLeftDivide` b = divide a by b on left, ie. b\a
instance (MShow a, Eq a, Show a, Fractional a) => LeftDivide (Poly a) where
    
    a `wLeftDivide` b | startsWithZero a && startsWithZero b
                        = removeFirst a `wLeftDivide` removeFirst b
                      | otherwise = recip b*a

instance (MShow a, Eq a, Show a, Fractional a) => Fractional (Poly a) where
    fromRational = injectP . fromRational

    a/b | startsWithZero a && startsWithZero b
          = removeFirst a/removeFirst b
        | otherwise = a*recip b
            
    recip (P u) = 
        let vs = filter ((/= 0) . snd) $ assocs u
        in case vs of
            [(0, a)] -> injectP $ recip a
            a -> error $ "Can't compute recip: " ++ show a

-- instance (Floating a) => Floating (Poly a)

realP :: Num a => Poly a -> a
realP (P w) = w!0

substitute :: (Num a, Num b) => (a -> Integer -> b) -> Poly a -> b
substitute f (P ws) =
    sum [f w n | (n, w) <- assocs ws]
