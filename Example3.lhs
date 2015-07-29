Some physics.

> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}

> module Main where

Squeezed light examples.
See, for example: http://arxiv.org/pdf/1401.4118v1.pdf

> import qualified Weyl as W
> import Formal
> import ISqrt2

First some useful inclusions:

  j      k
ℝ → W(ℝ) → W(ℝ)⟦z⟧

> j = W.injectW
> k = injectF
> kj = k . j

> a' = k W.x :: Formal (W.Weyl ISqrt2)
> a = k W.d :: Formal (W.Weyl ISqrt2)

The adjoint.

> adj = liftF W.adj

Note that the formal power series we use are implicitly power series
in a real variable z. If you want to think of z as complex,
and have the usual adjoint operator, then you may need need power
series in two variables: z and z*.

> main = do

n is the number operator

>   let n = a'*a

Just for fun I want to demonstrate the double exponential identity that
appears in some quantum optics books:

exp (-γn) = :exp ((exp(-γ)-1)*n):

We have to deal with the fact that the normal ordering operator :·:
is not a well-defined function on the Weyl algebra.
It's well defined on the free non-commutative algebra ℝ⟦a,a'⟧,
ie. "unsimplified" expressions containing a and a'.
Instead I cheat. A normal ordered function of a'*a is
just the "same" function of a commuting variable, x, where we substitute
(a')^n*a^n for x^n at the end.

(See section 3 of http://cds.cern.ch/record/794566/files/0409152.pdf)

>   let u = sample 10 $ exp (-z*n)
>   let v = sample 10 $ exp ((exp (-z)-1)*k W.x)
>   print u
>   print $ map (W.substitute (\w m 0 -> W.injectW w*W.x^m*W.d^m)) $ v

Now testing some computations relating to squeezed light.

The position operator:

>   let q = kj (1/sqrt2)*(a'+a)

The momentum operator:

>   let p = kj (-i/sqrt2)*(a-a')

This operator "squeezes" a state:

>   let squeeze = exp (z*(a^2-a'^2)/2)

The squeezing scales position by exp(-z) and momentum by exp(z):

>   print $ sample 6 $ adj squeeze*q*squeeze
>   print $ sample 6 $ q*exp (-z)
>   print $ sample 6 $ adj squeeze*p*squeeze
>   print $ sample 6 $ p*exp z

The result being that the Heisenberg uncertainty principle still holds because
squeezing position stretches momentum uncertainty inversely.

The squeeze operator actually applies a Bobgolyubov transformation:

>   print $ sample 6 $ adj squeeze*a'*squeeze
>   print $ sample 6 $ a'*cosh z-a*sinh z
>   print $ sample 6 $ adj squeeze*a*squeeze
>   print $ sample 6 $ a*cosh z-a'*sinh z
