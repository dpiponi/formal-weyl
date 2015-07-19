{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List
import Data.Ratio
import Data.Complex
import Prelude hiding (exp)

import Weyl
import Formal

injectFW = injectF . injectW

i :: Num a => Complex a
i = 0 :+ 1

main :: IO ()
main = do
--     print $ monomialsFromProduct 1 0 3 2 0
--     print $ weylPlus d x
--     print $ weylTimes d d
--     print $ (x, x*x, x*x*x)
--     print $ (d, d*d, d*d*d)
--     let xx = F (x : repeat 0) :: Formal (Weyl Rational)
--     let dd = F (d : repeat 0) :: Formal (Weyl Rational)
--     print $ take 10 $ exp (z*(ix+id))
--     let a = exp (fromRational (1%2)*z*z)*exp(z*xx)*exp(z*dd)
--     let b = exp (z*(xx+dd))
--     print $ sample 10 (a-b)
--     print $ take 4 a
--     let c = exp (z*(ix*id*id+ix))
--     print $ take 5 c
--    print $ (exp ([0, 1] ++ repeat 0) :: [Float])

--     let q = exp (z*(xx*xx*dd))
--     print $ sample 6 q
--     print $ sample 6 (egfToOgf q)

--     let xx = F (x : repeat 0) :: Formal (Weyl (Complex Double))
--     let dd = F (d : repeat 0) :: Formal (Weyl (Complex Double))
--     let q = injectFW (1/sqrt 2)*(xx+dd)
--     let p = injectFW (-i/sqrt 2)*(dd-xx)
--     print $ sample 3 (q*q)
--     print $ sample 3 (q*p-p*q)
--     print $ sample 3 (q*q*q*q)
--     print $ sample 3 (q*q*q*q*q*q)
--     let h = injectFW (1/2)*(p*p+q*q)
--     print $ sample 3 $ exp (injectFW i*z*h)*(q*q)*exp (-injectFW i*z*h)

--     let xx = F (x : repeat 0) :: Formal (Weyl Rational)
--     let dd = F (d : repeat 0) :: Formal (Weyl Rational)
--     print $ sample 6 $ dd*xx*dd*xx*xx*xx*dd*xx*dd
--     print $ sample 6 $ dd*dd*dd*xx*xx*xx

    let xx = F (x : repeat 0) :: Formal (Weyl Rational)
    let dd = F (d : repeat 0) :: Formal (Weyl Rational)
    let q = (xx*xx*dd*dd)*(xx*xx*dd*dd)*(xx*xx*dd*dd)
    print q
