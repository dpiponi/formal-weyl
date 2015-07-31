Messing with polynomials.

> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}

> module Main where

> import qualified Weyl as W
> import Formal
> import Data.Array
> import MShow

> k = injectF

> x = k W.x :: Formal (W.Weyl Rational)
> d = k W.d :: Formal (W.Weyl Rational)

> main = do

Compare the result of computing the 10th Hermite polynomial in two different ways.

>   let hermite10 = sum $ sample 11 $ liftF W.polyPart $ exp(z*x^2)*d^10*exp(-z*x^2)
>   let hermite10' = sum $ sample 11 $ liftF W.polyPart $ exp(z*x^2/2)*(x-d)^10*exp(-z*x^2/2)
>   print hermite10
>   print hermite10'

We can test the well known shift formula: exp(αD)f(x+α)

>   let poly = sum $ sample 11 $ liftF W.polyPart $ exp (z*d)*x^10
>   let poly' = sum $ sample 11 $ liftF W.polyPart $ (x+1)^10
>   print poly
>   print poly'

Consider the Laguerre polynomials: https://en.wikipedia.org/wiki/Laguerre_polynomials

They form a Sheffer sequence in the sense that

D/(D-1) L_n(x) = L_{n-1)(x)

Need to make this prettier somehow. In mathematics you can skip
explicitly mentioning injections everywhere but in Haskell
we need to be explicit.

>   let laguerre n = let n0 = fromInteger n
>                    in sum $ sample (n0+1) $ liftF W.polyPart $
>                       exp (z*x)*d^n0*exp (-z*x)*x^n0

>   let laguerre5 = laguerre 5
>   let laguerre4 = 5*laguerre 4
>   let laguerre4' = W.polyPart $ sum $ sample 10 $ z*d/(z*d-1)*injectF (W.inject_x laguerre5)

>   print laguerre4
>   print laguerre4'

You can explore a lot of umbral calculus this way:
https://en.wikipedia.org/wiki/Umbral_calculus
