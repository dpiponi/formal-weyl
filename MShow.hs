{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- Mathematical Show
-- This code is incomplete and doesn't display
-- subtractions nicely.
module MShow where

import Data.Ratio
import Data.List

class MShow a where
    mshow :: a -> String
    mshow' :: Int -> a -> String    

instance MShow Integer where
    mshow = show
    mshow' _ = show

parens :: Int -> Int -> String -> String
parens m n s = if n > m then '(' : (s ++ ")") else s

data ScriptType = ScriptType { digits :: String,
                               minus :: Char
                             }

super, sub :: ScriptType
super = ScriptType "⁰¹²³⁴⁵⁶⁷⁸⁹" '⁻'
sub = ScriptType "₀₁₂₃₄₅₆₇₈₉" '₋'

script :: ScriptType -> Integer -> String
script _ 0 = ""
script s n | n < 0 = minus s : script s (-n)
script s n = script s (n `div` 10) ++ [digits s!!fromIntegral (n `mod` 10)]

subscript, superscript :: Integer -> String
subscript = script sub
superscript = script super

instance MShow Rational where
    mshow' n r = parens 7 n (mshow r)
    mshow r =
        let nm = numerator r
            dn = denominator r
        in if dn == 1
            then show nm
            else superscript nm ++ "/" ++ subscript dn

atLeast :: String -> String -> String
atLeast a "" = a
atLeast _ b = b

sumMShow :: (MShow a) => Int -> [a] -> String
sumMShow n xs | null xs = "0"
              | length xs == 1 = mshow' n (head xs)
              | otherwise = let ss = map (mshow' 6) xs
                                s = intercalate "+" ss
                            in parens 6 n s
