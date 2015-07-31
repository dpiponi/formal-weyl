OEIS explorations.

> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}

> module Main where

> import Control.Monad

> import qualified Weyl as W
> import Formal
> import Data.Array
> import MShow
> import Over

> x = ι W.x :: Formal (W.Weyl Rational)
> d = ι W.d :: Formal (W.Weyl Rational)

> realF = liftF W.realW

> op = 1/(1-(z*(d^2-x^2)))/2

> main = do

Some experimental mathematics. Make up random expressions involving
creation and annihilation operators and look them up at OEIS
https://oeis.org/

OEIS A053871. Take 2n people who are paired.
Count alternative ways to pair them off that include none of
the original pairings.

>   print $ sample 16 $ realF $ 1/(1-z*(x^2+d^2+2*x*d))

OEIS A052502. Permutations without fixed point whose cubes are the identity.

>   print $ sample 20 $ realF $ 1/(1-z*(x^2+d))

OEIS A001470. Number of degree n permutations of order dividing 3.

>   print $ sample 20 $ realF $ 1/(1-z*(x^2+d+1))

OEIS A118934.

>   print $ sample 20 $ liftF W.realW $ 1/(1-z*(1+x+d^3))
