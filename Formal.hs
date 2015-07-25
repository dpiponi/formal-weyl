module Formal where

import Debug.Trace
import LeftDivide

(*!) :: (Num a, Eq a) => a -> a -> a
(*!) _ 0 = 0
(*!) a b = a*b

(!*) :: (Num a, Eq a) => a -> a -> a
(!*) 0 _ = 0
(!*) a b = a*b

(^+) :: Num c => [c] -> [c] -> [c]
(^+) = zipWith (+)
(^-) :: Num c => [c] -> [c] -> [c]
(^-) = zipWith (-)

convolve :: (Num a, Eq a) => [a] -> [a] -> [a]
~(a:as) `convolve` (b:bs) = (a *! b) :
    map (a !*) bs ^+ (as `convolve` (b:bs))
_ `convolve` _ = error "Impossible convolution"

rconvolve :: (Num a, Eq a) => [a] -> [a] -> [a]
(a:as) `rconvolve` ~(b:bs) = (a !* b) :
    map (*! b) as ^+ ((a:as) `rconvolve` bs)
_ `rconvolve` _ = error "Impossible convolution"

newtype Formal a = F [a] deriving Show

liftF :: (a -> b) -> Formal a -> Formal b
liftF f (F xs) = F $ map f xs

instance (Num r, Eq r) => Num (Formal r) where
    F u+F v  = F $ zipWith (+) u v
    F u-F v  = F $ zipWith (-) u v
    ~(F u)*F v = F $ u `convolve` v
    fromInteger = injectF . fromInteger
    negate (F u) = F $ map negate u
    signum (F []) = error "Can't compute signum for non-constant"
    signum (F (u : _)) = F $ signum u:repeat 0
    abs _  = error "Can't form abs of a power series"

egfToOgf :: Num a => Formal a -> Formal a
egfToOgf (F a) = F $ egfToOgf' 0 1 a where
    egfToOgf' r s (a : as) = (s*a) : egfToOgf' (r+1) ((r+1)*s) as
    egfToOgf' _ _ [] = []

injectF :: Num a => a -> Formal a
injectF u = F $ u : repeat 0

-- leftInvert (x0:xs) = trace ("> " ++ show x0) $ r where r = map (*recip x0) (1:repeat 0) ^- (r `convolve` (0:xs))
-- 
rightDivide y (x0:xs) = trace ("> " ++ show x0) $ r
                        where r = map (`wLeftDivide` x0) (y ^- ((0:xs) `rconvolve` r))

instance (Show r, Eq r, Fractional r, LeftDivide r) => Fractional (Formal r) where
    F (x:xs)/F (y:ys) = 
        if x==0 && y==0
            then F xs/F ys
            else F $ rightDivide (x:xs) (y:ys)
    --recip (F xs) = F (leftInvert xs)
    fromRational u = F $ fromRational u:repeat 0

z :: Num a => Formal a
z = F $ [0, 1] ++ repeat 0

deriv :: Num a => Formal a -> Formal a
deriv (F (_ : u)) = F $ zipWith (*) (map fromInteger [1..]) u
deriv _ = error "Impossible deriv"

integrate :: Fractional a => Formal a -> Formal a
integrate (F u) = F $ 0 : zipWith (*) (map ((1 /) . fromInteger) [1..]) u

-- exp is assuming 0 for first term XXX
instance (Show a, Eq a, Num a, Fractional a, LeftDivide a) => Floating (Formal a) where
    exp u = e where e = 1+integrate (e*deriv u)

-- star :: (Eq a, Num a, Fractional a) => Formal a -> Formal a
-- star u = e where e = 1+e*u

sample :: Int -> Formal a -> [a]
sample n (F xs) = take n xs

-- assume x0 = 1 -- seems not to work???
{-
leftInvert (F x) = F r where r = map (/x0) (1:repeat 0) ^- (r `convolve` (0:xs))
                             x0:xs = x 
-}

{-
rightInvert (F x) = F r where r = map (/x0) (1:repeat 0) ^- ((0:xs) `rconvolve` r)
                              x0:xs = x 
-}

{-
 - 1/(a+b+c+d+...)
 - = (1/a) (1/(1+b/c+c/a+d/a+...)

p... is right inverse of a

(a+bx+cx^2+...)(p+qx+rx^2+...) = 1
ap = 1

p is right inverse of a

Want q now...
aq+bp = 0

commutative case: q = -bp/a = -bp^2
non-commutative q = (left inverse a)bp
 
 -}
