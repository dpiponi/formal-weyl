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

rightDivide y (x0:xs) = r
                        where r = map (`wLeftDivide` x0)
                                      (y ^- ((0:xs) `rconvolve` r))

instance (Show r, Eq r, Fractional r, LeftDivide r) =>
        Fractional (Formal r) where
    F (x:xs)/F (y:ys) = 
        if x==0 && y==0
            then F xs/F ys
            else F $ rightDivide (x:xs) (y:ys)
    fromRational u = F $ fromRational u:repeat 0


z :: Num a => Formal a
z = F $ [0, 1] ++ repeat 0

deriv :: Num a => Formal a -> Formal a
deriv (F (_ : u)) = F $ zipWith (*) (map fromInteger [1..]) u
deriv _ = error "Impossible deriv"

integrate :: Fractional a => Formal a -> Formal a
integrate (F u) = F $ 0 : zipWith (*) (map ((1 /) . fromInteger) [1..]) u

lead :: [a] -> Formal a -> Formal a
lead [] (F x) = F x
lead (a:as) (F x) = F $ a : y where F y = lead as (F $ tail x)

(...) :: [a] -> Formal a -> Formal a
a ... x = lead a x

-- exp is assuming 0 for first term XXX
-- Maybe for other fns too.
instance (Show a, Eq a, Num a, Fractional a, LeftDivide a) => Floating (Formal a) where
    exp u@(F (0:_)) = e where e = [1] ... integrate (e*deriv u)
    sinh u@(F (0:_)) = integrate (cosh u*deriv u)
    cosh u@(F (0:_)) = [1] ... integrate (sinh u*deriv u)
    sin u = integrate (cos u*deriv u)
    cos u@(F (0:_)) = [1] ... negate (integrate (sin u*deriv u))
    -- I think this approach is fine for non-commutative case as long
    -- as first element of series is central.
    -- This version needs first element of series to be 1.
    sqrt (F x@(1 : _)) = F (1:rs) where rs = map (/2) (xs ^- (rs `convolve` (0:rs)))
                                        _:xs = x

    log = error "log not implemented for formal power series yet"
    pi = error "pi not implemented for formal power series yet"
    tan = error "tan not implemented for formal power series yet"
    tanh = error "tanh not implemented for formal power series yet"
    asin = error "asin not implemented for formal power series yet"
    acos = error "acos not implemented for formal power series yet"
    atan = error "atan not implemented for formal power series yet"
    asinh = error "asinh not implemented for formal power series yet"
    acosh = error "acosh not implemented for formal power series yet"
    atanh = error "atanh not implemented for formal power series yet"

-- star :: (Eq a, Num a, Fractional a) => Formal a -> Formal a
-- star u = e where e = 1+e*u

sample :: Int -> Formal a -> [a]
sample n (F xs) = take n xs
