-- {-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

-- import Test.Framework

import Weyl
import Formal
import ISqrt2
import Data.Array
import MShow

injectFW = injectF . injectW

xx = injectF x :: Formal (Weyl ISqrt2)
dd = injectF d :: Formal (Weyl ISqrt2)
-- xx = injectF x :: Formal (Weyl Rational)
-- dd = injectF d :: Formal (Weyl Rational)

-- test_1 =
--     assertEqual (show (Formal.sample 10 xx)) "[X,0,0,0,0,0,0,0,0,0]"

-- main = htfMain htf_thisModulesTests

real :: Num a => Weyl a -> a
real (W w) = w!(0,0)

realF :: Num a => Formal (Weyl a) -> Formal a
realF (F ws) = F (map real ws)

main = do
--     print $ real x
--     print $ real d
--     print $ sample 10 $ realF xx
--     print $ exp z
--     print $ sample 10 $ exp(z*xx)
--     print $ sample 10 $ exp(z*(xx+dd))
    let q = injectFW (1/sqrt2)*(xx+dd)
    let p = injectFW (i/sqrt2)*(xx-dd)
    let h = injectFW (1/2)*(p*p+q*q)
--     print $ (xx-dd)*(xx-dd)
--     print $ sample 4 $ realF h
--     let q = injectFW (injectI (1/2))*injectFW sqrt2*(xx+dd)
--     print $ sample 10 $ realF $ q*q
--     print $ sample 4 $ realF xx
    let state = exp (injectFW i*z*p)
--     print $ sample 4 state
--     print $ sample 4 $ liftF adj state
--     print $ sample 6 $ liftF adj state*state
--     print $ sample 8 $ liftF adj state*q*state
--     print $ sample 8 $ realF $ liftF adj state*q*state
    let a = (z*dd :: Formal (Weyl ISqrt2))
    let b = (exp(z*dd)-1 :: Formal (Weyl ISqrt2))
    let F xs = a
    let F ys = b
    let d' = d :: Weyl ISqrt2
    print $ d'/d'
    print $ head xs==0
    print $ head ys==0
    print $ sample 4 a
    print $ sample 4 b
    print $ sample 10 $ a/b
--     print $ let F ws = a/b in head ws
--     print $ sample 4 $ p
--     print $ sample 4 $ liftF adj p
--     print $ sample 4 $ q
--     print $ sample 4 $ liftF adj q
    return ()
