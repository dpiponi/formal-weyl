{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Test.Framework

import Weyl
import Formal

injectFW = injectF . injectW

xx = injectF x :: Formal (Weyl Rational)
dd = injectF d :: Formal (Weyl Rational)

test_1 =
    assertEqual (show (Formal.sample 10 xx)) "[X,0,0,0,0,0,0,0,0,0]"

test_2 =
    assertEqual (show (Formal.sample 10 dd)) "[D,0,0,0,0,0,0,0,0,0]"

test_3 =
    assertEqual (show (Formal.sample 10 (xx+dd))) "[D+X,0,0,0,0,0,0,0,0,0]"

test_4 =
    assertEqual (show (Formal.sample 10 ((xx+dd)^2))) "[1+D²+2XD+X²,0,0,0,0,0,0,0,0,0]"

test_5 =
    assertEqual (show (Formal.sample 6 (exp (z*(xx+dd)))))
        "[1,D+X,¹/₂+¹/₂D²+XD+¹/₂X²,¹/₂D+¹/₆D³+¹/₂X+¹/₂XD²+¹/₂X²D+¹/₆X³,\
        \¹/₈+¹/₄D²+¹/₂₄D⁴+¹/₂XD+¹/₆XD³+¹/₄X²+¹/₄X²D²+¹/₆X³D+¹/₂₄X⁴,\
        \¹/₈D+¹/₁₂D³+¹/₁₂₀D⁵+¹/₈X+¹/₄XD²+¹/₂₄XD⁴+¹/₄X²D+¹/₁₂X²D³+¹/₁₂X³+\
        \¹/₁₂X³D²+¹/₂₄X⁴D+¹/₁₂₀X⁵]"

test_6 = do
    let xx = injectF x :: Formal (Weyl Rational)
    let dd = injectF d :: Formal (Weyl Rational)
    let a = exp (z*(xx+dd))
    let b = exp (-z*(xx+dd))
    assertEqual (show (Formal.sample 6 (a*b))) "[1,0,0,0,0,0]"

main = htfMain htf_thisModulesTests
