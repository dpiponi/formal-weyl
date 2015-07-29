module ISqrt2 where

import Data.Ratio
import Data.List
import Data.Complex

import MShow

-- Implementation of the field ℚ[i, √2]

data ISqrt2 = I Rational Rational Rational Rational deriving Eq

injectI :: Rational -> ISqrt2
injectI a = I a 0 0 0

instance MShow ISqrt2 where
    mshow = mshow' 0
    mshow' n (I a b c d) = 
        let xs = concat [
                    if a == 0 then [] else [mshow a],
                    if b == 0 then [] else [mshow b ++ "√2"],
                    if c == 0 then [] else [mshow c ++ "i"],
                    if d == 0 then [] else [mshow d ++ "i√2"]]
        in case length xs of
            0 -> "0"
            1 -> parens 7 n (head xs)
            otherwise -> parens 6 n (intercalate "+" xs)

instance Num ISqrt2 where
    fromInteger n = I (fromInteger n) 0 0 0
    I a b c d+I a' b' c' d' = I (a+a') (b+b') (c+c') (d+d')
    I a b c d*I a' b' c' d' = I (a*a'+2*b*b'-c*c'-2*d*d')
                                (a*b'+a'*b-c*d'-c'*d)
                                (a*c'+c*a'+2*b*d'+2*d*b')
                                (a*d'+d*a'+b*c'+c*b')
    negate (I a b c d) = I (-a) (-b) (-c) (-d)
    abs = error "abs could be implemented, but I haven't done so yet"
    signum = error "signum could be implemented, but I haven't done so yet"

instance Fractional ISqrt2 where
    fromRational a = injectI a
    recip (I a b c d) =
        let r = a^4-4*a^2*b^2+4*b^4+2*a^2*c^2+4*b^2*c^2+c^4
               -16*a*b*c*d+4*a^2*d^2+8*b^2*d^2-4*c^2*d^2+4*d^4
        in I ((a^3-2*a*b^2+a*c^2-4*b*c*d+2*a*d^2)/r)
             ((-a^2*b+2*b^3+b*c^2-2*a*c*d+2*b*d^2)/r)
             ((-a^2*c-2*b^2*c-c^3+4*a*b*d+2*c*d^2)/r)
             ((2*a*b*c-a^2*d-2*b^2*d+c^2*d-2*d^3)/r)

instance Show ISqrt2 where
    show = mshow

sqrt2, i :: ISqrt2
sqrt2 = I 0 1 0 0
i = I 0 0 1 0

-- Move into own module?
class Conjugatable a where
    conj :: a -> a

instance RealFloat a => Conjugatable (Complex a) where
    conj = conjugate

instance Conjugatable ISqrt2 where
    conj (I a b c d) = I a b (-c) (-d)
