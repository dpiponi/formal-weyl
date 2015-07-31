Some basic Weyl algebra combinatorics.

> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}

> module Main where

> import Weyl
> import Formal
> import ISqrt2
> import Data.Array
> import MShow
> import Over

> a' = x :: Weyl Rational
> a = d :: Weyl Rational

> main = do

Here's the most basic thing you can do with the Weyl algebra:

Consider one device with m output ports and another with n
input ports. How many ways can you wire them together?

  a²        a'⁴   <-- label outputs with annihilation operators
+----+ 1a +----+      and inputs with creation operators.
|    | .--|○  a|
| 1 ●|/   |○  b| 
| 2 ●|----|○  c|
|    | 2c |○  d|
+----+    +----+

There's one way using no cables.
Using one cable there's: 1a, 1b, 1c, 1d, 2a, 2b, 2c, 2d: that's 8 ways.
Using two cables there's: 1a 2b, 1a 2c, 1a 2d, 1b 2a, ...: that's 12 ways

Compare with the coefficients (1, 8, 12) in the following expression
constructed from multiplying the labels above the "modules":

>   print $ a^2*a'^4

Note how the powers of X and D respectively give the number of open input
and output ports respectively, eg. there are 8 wirings that leave open
three input ports.

Here's a more complex examples. You have three "modules" in a row.
The first has two outputs, the second has an input and two outputs and
the third has two inputs.

Consider all possible wirings so that the data flow is always from
left to right.

 a²   a' a²   a'²
+--+ +-----+ +---+
|1●| |○a 3●| | ○c|
|2●| |   4●| | ○d|
+--+ +-----+ +---+

Here are the twelve possible wirings using three patch cables:
1a 2c 3d, 1a 2c 4d, 1a 2d 3c, 1a 2d 4c, 1a 3c 4d, 1a 3d 4c,
1c 2a 4d, 1c 2c 3d, 1d 2a 3c, 1d 2a 4c, 2a 3c 4d, 2a 3d 4c

This and the "patches" with different numbers of cables are
enumerated here:

>   print $ a^2 * a'*a^2 * a'^2

This is essentally Wick's theorem although I think of it as the
"Modular Synth Theorem": https://en.wikipedia.org/wiki/Wick's_theorem

Here's the other basic thing you can do with the Weyl algebra:
Consider non-attacking rook placements in a Ferrers board.
Here's one:

    ♖               *a*a' (up, right)
  ♖..       *a*a'*a'      (up, right, right)
 ..♖.   a*a'              (up, right)

With no rooks: one arrangement.
You can place one rook anywhere: so that's 8.
With two rooks you can easily count 4+3+3+2+1+1 = 14.
With three rooks there are only 4 arrangements.

Walk along the staircase from bottom left to top right.
Accumulate an a for going up and a' for going right

Note how the coefficients in the normal form are (1, 8, 14, 4):

>   print $ a*a'*a*a'*a'*a*a'

See section 8.1 of http://arxiv.org/pdf/1010.0354.pdf

>   return ()
