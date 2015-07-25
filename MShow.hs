{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module MShow where

import Data.Ratio
import Data.List

{-
 - infixr 9  .  
 - infixr 8  ^, ^^, ⋆⋆  
 - infixl 7  ⋆, /, ‘quot‘, ‘rem‘, ‘div‘, ‘mod‘  
 - infixl 6  +, -
 - -- The (:) operator is built-in syntax, and cannot legally be given  
 - -- a fixity declaration; but its fixity is given by:  
 - --   infixr 5  :  
 -  
 -  infix  4  ==, /=, <, <=, >=, >  
 -  infixr 3  &&  
 -  infixr 2  ||  
 -  infixl 1  >>, >>=  
 -  infixr 1  =<<  
 -  infixr 0  $, $!, ‘seq‘
 -}

{-
 mshow' n x
 means display x in a context where we've just had an operator
 of level n.
 So show' 6 (a+b) doesn't do parentheses but
    show' 7 does
 -}
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

-- Get rid of zero terms before calling
sumMShow :: (MShow a) => Int -> [a] -> String
sumMShow n xs =
--     let xs' = filter (/= 0) xs
    if null xs
        then "0"
        else if length xs == 1
            then mshow' n (head xs)
            else let ss = map (mshow' 6) xs
                     s = intercalate "+" ss
                 in parens 6 n s

-- mprint :: MShow a => IO ()
-- mprint x = putStrLn (mshow x)
