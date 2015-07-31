-- Formal power series.
-- Note the big caveat for division.
-- Probably best to use division only when constant term
-- of divisor is non-zero.

{-# LANGUAGE FlexibleInstances #-}

module Weyl where

import Data.Array
import Data.List
import Data.Ratio
import Data.Tuple
import Debug.Trace
import Control.Applicative
import LeftDivide
import Padded
import qualified Poly as P

import MShow
import ISqrt2

-- These are integer divisions. Shouldn't need Fractional.
monomialTimes :: (Fractional f) =>
    f -> Integer -> Integer -> Integer -> Integer -> [((Integer, Integer), f)]
monomialTimes u r s a b =
    monomialTimes' u s a 1 (r+a) (s+b) where
    monomialTimes' :: (Fractional f) =>
        f -> Integer -> Integer -> Integer -> Integer -> Integer ->
        [((Integer, Integer), f)]
    monomialTimes' v 0 _ _ p q = [((p, q), v)]
    monomialTimes' v _ 0 _ p q = [((p, q), v)]
    monomialTimes' v _ _ _ 0 q = [((0, q), v)]
    monomialTimes' v _ _ _ p 0 = [((p, 0), v)]
    monomialTimes' v s' a' t p q = ((p, q), v) :
        monomialTimes' (v*fromIntegral s'*fromIntegral a'/fromIntegral t)
                       (s'-1) (a'-1) (t+1) (p-1) (q-1)

data Weyl a = W (Array (Integer, Integer) a)

adj :: (Conjugatable a, Show a) => Weyl a -> Weyl a
adj (W ws) = W $ conj <$> ixmap (let (x, y) = bounds ws in (swap x, swap y)) swap ws

combineBounds :: (Integer -> Integer -> Integer) ->
                 Array (Integer, Integer) e1 -> Array (Integer, Integer) e ->
                 ((Integer, Integer), (Integer, Integer))
combineBounds f u v =
    let (_, (mx, nx)) = bounds u
        (_, (my, ny)) = bounds v
        in ((0, 0), (f mx my, f nx ny))

{-
fn :: (Ix i, Num e) => Array i e -> i -> e
fn u i = if inRange (bounds u) i then u!i else 0
-}

weylPlus :: Num a => Weyl a -> Weyl a -> Weyl a
weylPlus (W u) (W v) =
    let bz = combineBounds max u v
--    in W $ accumArray bz $ padded $ (+) <$> (assocs u :- 0) <*> (assocs v :- 0)
    in W $ accumArray (+) 0 bz $ assocs u ++ assocs v

weylEq :: (Show a, Num a, Eq a) => Weyl a -> Weyl a -> Bool
weylEq (W u) (W v) = 
    let bz = combineBounds max u v
--    in and [fn u i == fn v i | i <- range bz]
    in and $ map snd $ padded $ (==) <$> (assocs u :- 0) <*> (assocs v :- 0)

weylTimes :: (Num a, Fractional a) => Weyl a -> Weyl a -> Weyl a
weylTimes (W u) (W v) =
    let bz = combineBounds (+) u v
    in W $ accumArray (+) 0 bz
        [e | ((r, s), p) <- assocs u,
             ((a, b), q) <- assocs v,
             e <- monomialTimes (p*q) r s a b]

-- in (i, j): i labels row, j column
startsWithZeroRow :: (Num a, Eq a) => Weyl a -> Bool
startsWithZeroRow (W ws) =
    let (_, (_, maxcol)) = bounds ws
    in and [ws!(0, i) == 0 | i <- [0..maxcol]]

startsWithZeroCol :: (Num a, Eq a) => Weyl a -> Bool
startsWithZeroCol (W ws) =
    let (_, (maxrow, _)) = bounds ws
    in and [ws!(i, 0) == 0 | i <- [0..maxrow]]

removeFirstCol :: (Num a) => Weyl a -> Weyl a
removeFirstCol (W ws) =
    let (_, (rows, maxcol)) = bounds ws
    in W $ ixmap ((0, 0), (rows, maxcol-1)) (\(i,j) -> (i, j+1)) ws

removeFirstRow :: (Num a) => Weyl a -> Weyl a
removeFirstRow (W ws) =
    let (_, (maxrow, cols)) = bounds ws
    in W $ ixmap ((0, 0), (maxrow-1, cols)) (\(i,j) -> (i+1, j)) ws

instance (Show a, Eq a, Num a) => Eq (Weyl a) where
    (==) = weylEq

instance (Show a, Eq a, Fractional a, Num a, MShow a) => MShow (Weyl a) where
    mshow' n x = parens 6 n (mshow x) 
    mshow = weylShow

instance (Show a, Eq a, Fractional a, Num a, MShow a) => Show (Weyl a) where
    show = mshow

monoPower :: Integer -> String -> String
monoPower 0 _ = ""
monoPower 1 s = s
monoPower n s = s ++ superscript n

weylRational :: Rational -> String
weylRational = mshow

weylRational' 1 = ""
weylRational' u = weylRational u

-- nonNull :: String -> String
-- nonNull "" = "1"
-- nonNull u = u

data WMonomial a = WM Integer Integer a

instance (Eq a, MShow a, Num a) => MShow (WMonomial a) where
    mshow' n w = parens 7 n (mshow w)
    mshow (WM i j w) = 
                    atLeast "1" ((if w == 1 then "" else mshow' 7 w) ++
                                      monoPower i "X" ++
                                      monoPower j "D")

-- Can use mshow' 6 for terms with i == j == 0
weylShow :: (Show a, Fractional a, MShow a, Num a, Eq a) => Weyl a -> String
weylShow u | u == 0 = "0"
weylShow (W u) = sumMShow 0 -- ??
                    [WM i j w |
                     ((i, j), w) <- assocs u,
                     w /= 0]

weylShow' :: (Show a, Fractional a, Eq a, MShow a, Num a) => Weyl a -> String
weylShow' u = atLeast "0" $ weylShow u

scalarTimes :: Num a => a -> Weyl a -> Weyl a
scalarTimes s (W t) = W $ fmap (s *) t

one, d, x :: Num a => Weyl a
one = W $ listArray ((0, 0), (0, 0)) [1]
d = W $ listArray ((0, 0), (0, 1)) [0, 1]
x = W $ listArray ((0, 0), (1, 0)) [0, 1]

injectW :: Num a => a -> Weyl a
injectW u = W $ listArray ((0, 0), (0, 0)) [u]

instance (Fractional a, Num a) => Num (Weyl a) where
    fromInteger = injectW . fromInteger
    (+) = weylPlus
    (*) = weylTimes
    negate = scalarTimes (-1)
    abs = error "I don't know what abs would mean here."
    signum = error "Pretty sure signum makes no sense here."
    
-- Only support trivial divisions.
-- a `wLeftDivide` b = divide a by b on left, ie. b\a
instance (MShow a, Eq a, Show a, Fractional a) => LeftDivide (Weyl a) where
    a `wLeftDivide` b = recip b*a

instance (MShow a, Eq a, Show a, Fractional a) => Fractional (Weyl a) where
    fromRational u = W $ listArray ((0, 0), (0, 0)) [fromRational u]
    a/b = a*recip b
    recip (W u) = 
        let vs = filter ((/= 0) . snd) $ assocs u
        in case vs of
            [((0, 0), a)] -> W $ array ((0,0),(0,0)) [((0, 0), recip a)]
            a -> error $ "Can't compute recip: " ++ show a

-- instance (Floating a) => Floating (Weyl a)

realW :: Num a => Weyl a -> a
realW (W w) = w!(0,0)

polyPart :: Num a => Weyl a -> P.Poly a
polyPart (W ws) = let ((0, 0), (rows, cols)) = bounds ws
                  in P.P $ array (0, rows) [(i, ws!(i, 0)) | i <- [0..rows]]

substitute :: (Num a, Num b) => (a -> Integer -> Integer -> b) -> Weyl a -> b
substitute f (W ws) =
    sum [f w m n | ((m, n), w) <- assocs ws]

-- Fractional shouldn't be needed here XXX
inject_x :: (Fractional a, Num a) => P.Poly a -> Weyl a
inject_x = P.substitute (\w n -> injectW w*x^n)

inject_d :: (Fractional a, Num a) => P.Poly a -> Weyl a
inject_d = P.substitute (\w n -> injectW w*d^n)
