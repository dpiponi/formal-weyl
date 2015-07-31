-- The Over type class represents the situation where some algebraic
-- structure is defined over some base structure in such a way
-- that there is an obvious inclusion. For example there is
-- an obvious injection from the reals to the polynomials in
-- X over the reals.

module Over where

class Over a where
    ι :: Num x => x -> a x 

