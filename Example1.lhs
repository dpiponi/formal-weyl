The Euler-Maclaurin summation formula.

> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}

> module Main where

> import qualified Weyl as W
> import Formal
> import ISqrt2
> import Data.Array
> import MShow

> x = injectF W.x :: Formal (W.Weyl Rational)
> d = injectF W.d :: Formal (W.Weyl Rational)

> main = do
>   let b = (z*d/(exp(z*d)-1) :: Formal (W.Weyl Rational))

The Bernoulli numbers, scaled by n!:

>   print $ sample 10 $ b

Note how I use z*D instead of D. This allows me to form the infinite
sum as a formal power series in z. The sum function below effectively
evaluates the result at z=1.

The differential operator D/(exp D-1) converts an integral to a sum
using the Euler-Maclaurin summation formula.
So, for example, it converts exp x to the sum: exp 0+exp (-1)+exp (-2)+...

>   let s = sample 30 $ liftF W.realW $ b*exp(-z*x)
>   print $ s
>   print $ (fromRational (sum s) :: Double)
>   print $ sum [exp (-n) | n <- [0..18]]
>   return ()
