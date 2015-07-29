{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Module to provide division on left.
-- Intended for non-commutative algebraic structures where
-- right-division, ie. (/), is distinct from left-division.
module LeftDivide where

-- | Numbers that support division on the left
class LeftDivide a where
    -- | @a `wLeftDivide` b@ is @a@ divided on the left
    -- by @b@. Sometimes written as b\\a in mathematics
    -- literature.
    -- If the instance is commutative then
    --
    -- prop> a `wLeftDivide b` = a/b
    --
    -- If @recip b@ is defined then
    --
    -- prop> a `wLeftDivide` b == recip b * a
    --
    -- @wLeftDivide@ is useful for times when division is meaningful
    -- but @recip@ isn't. For example if @a@ and @b@ don't commute
    -- we'd like @A\\A²B@ to be @AB@ even if @recip a@ isn't defined.
    wLeftDivide :: a -> a -> a

instance LeftDivide Rational where
    a `wLeftDivide` b = a/b
