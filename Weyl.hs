{-# LANGUAGE FlexibleInstances #-}

module Weyl where

import Data.Array
import Data.List
import Data.Ratio

data ScriptType = ScriptType { digits :: String,
                               minus :: Char
                             }

super, sub :: ScriptType
super = ScriptType "⁰¹²³⁴⁵⁶⁷⁸⁹" '⁻'
sub = ScriptType "₀₁₂₃₄₅₆₇₈₉" '₋'

script :: ScriptType -> Integer -> String
script _ 0 = ""
script s n | n < 0 = minus s : script s (-n)
script s n = script s (n `div` 10) ++ [digits s!!fromIntegral (n `mod` 10)]

subscript, superscript :: Integer -> String
subscript = script sub
superscript = script super

-- These are integer divisions. Shouldn't need Fractional.
monomialTimes :: (Integral n, Eq n, Fractional f) =>
    f -> n -> n -> n -> n -> [((n, n), f)]
monomialTimes u r s a b =
    monomialTimes' u s a 1 (r+a) (s+b) where
    monomialTimes' v 0 _ _ p q = [((p, q), v)]
    monomialTimes' v _ 0 _ p q = [((p, q), v)]
    monomialTimes' v _ _ _ 0 q = [((0, q), v)]
    monomialTimes' v _ _ _ p 0 = [((p, 0), v)]
    monomialTimes' v s' a' t p q = ((p, q), v) :
        monomialTimes' (v*fromIntegral s'*fromIntegral a'/fromIntegral t)
                       (s'-1) (a'-1) (t+1) (p-1) (q-1)

data Weyl a = W (Array (Integer, Integer) a)

fn :: (Ix i, Num e) => Array i e -> i -> e
fn u i = if inRange (bounds u) i then u!i else 0

combineBounds :: (Num t, Ix t) =>
                 (t -> t -> t) -> Array (t, t) e1 -> Array (t, t) e -> ((t, t), (t, t))
combineBounds f u v =
    let (_, (mx, nx)) = bounds u
        (_, (my, ny)) = bounds v
        in ((0, 0), (f mx my, f nx ny))

weylPlus :: Num a => Weyl a -> Weyl a -> Weyl a
weylPlus (W u) (W v) =
    let bz = combineBounds max u v
    in W $ accumArray (+) 0 bz [(i, fn u i+fn v i) | i <- range bz]

weylEq :: (Num a, Eq a) => Weyl a -> Weyl a -> Bool
weylEq (W u) (W v) =
    let bz = combineBounds max u v
    in and [fn u i == fn v i | i <- range bz]

weylTimes :: (Num a, Fractional a) => Weyl a -> Weyl a -> Weyl a
weylTimes (W u) (W v) =
    let bz = combineBounds (+) u v
    in W $ accumArray (+) 0 bz
        [e | ((r, s), p) <- assocs u,
             ((a, b), q) <- assocs v,
             e <- monomialsFromProduct (p*q) r s a b]

instance (Eq a, Num a) => Eq (Weyl a) where
    (==) = weylEq

instance Show (Weyl Rational) where
    show = weylShow

monoPower :: Integer -> String -> String
monoPower 0 _ = ""
monoPower 1 s = s
monoPower n s = s ++ superscript n

weylRational :: Rational -> String
weylRational r =
    let nm = numerator r
        dn = denominator r
    in if dn == 1
        then show nm
        else superscript nm ++ "/" ++ subscript dn
weylRational' :: Rational -> String

weylRational' 1 = ""
weylRational' u = weylRational u

nonNull :: String -> String
nonNull "" = "1"
nonNull u = u

weylShow :: Weyl Rational -> String
weylShow u | u == 0 = "0"
weylShow (W u) = intercalate "+"
                    [nonNull (weylRational' w ++
                              monoPower i "X" ++
                              monoPower j "D") |
                     ((i, j), w) <- assocs u,
                     w /= 0]

weylShow' :: (Show r, Floating r, Eq r) => Weyl r -> String
weylShow' u | u == 0 = "0"
weylShow' (W u) = intercalate "+"
                    [nonNull (show w ++ monoPower i "X" ++ monoPower j "D") |
                     ((i, j), w) <- assocs u,
                     w /= 0]

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
    abs = error "No abs"
    signum = error "No signum"

instance (Fractional a) => Fractional (Weyl a) where
    fromRational u = W $ listArray ((0, 0), (0, 0)) [fromRational u]
    recip (W u) =
        case assocs u of
            [((0, 0), a)] -> W $ array (bounds u) [((0, 0), recip a)]
            _ -> error "Impossible recip"

-- instance (Floating a) => Floating (Weyl a)
