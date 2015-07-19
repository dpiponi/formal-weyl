module Formal where

convolve :: (Num a, Eq a) => [a] -> [a] -> [a]
~(a:as) `convolve` (b:bs) = (a *! b) :
    map (a !*) bs ^+ (as `convolve` (b:bs))
_ `convolve` _ = error "Impossible convolution"

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

newtype Formal a = F [a] deriving Show

instance (Num r, Eq r) => Num (Formal r) where
    F u+F v  = F $ zipWith (+) u v
    F u-F v  = F $ zipWith (-) u v
    ~(F u)*F v = F $ u `convolve` v -- ~
    fromInteger u = F $ fromInteger u:repeat 0
    negate (F u) = F $ map negate u
    signum (F []) = error "Impossible!"
    signum (F (u:_)) = F $ signum u:repeat 0
    abs _   = error "Can't form abs of a power series"

egfToOgf :: Num a => Formal a -> Formal a
egfToOgf (F a) = F $ egfToOgf' 0 1 a

egfToOgf' :: Num a => a -> a -> [a] -> [a]
egfToOgf' r s (a : as) = (s*a) : egfToOgf' (r+1) ((r+1)*s) as
egfToOgf' _ _ [] = error "No egfToOgf"

injectF :: Num a => a -> Formal a
injectF u = F $ (u : repeat 0)

instance (Eq r, Fractional r) => Fractional (Formal r) where
    (/) = error "No (/)"
    fromRational u = F $ fromRational u:repeat 0

-- instance (Eq r, Floating r) => Floating (Formal r)

z :: Num a => Formal a
z = F $ [0, 1] ++ repeat 0

deriv :: Num a => Formal a -> Formal a
deriv (F (_ : u)) = F $ zipWith (*) (map fromInteger [1..]) u
deriv _ = error "Impossible deriv"

integrate :: Fractional a => Formal a -> Formal a
integrate (F u) = F $ 0 : zipWith (*) (map ((1 /) . fromInteger) [1..]) u

exp :: (Eq a, Num a, Fractional a) => Formal a -> Formal a
exp u = e where e = 1+integrate (e*deriv u)

star :: (Eq a, Num a, Fractional a) => Formal a -> Formal a
star u = e where e = 1+e*u

sample :: Int -> Formal a -> [a]
sample n (F xs) = take n xs
