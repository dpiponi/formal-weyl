-- Based on http://stackoverflow.com/a/21350096

module Padded where

import Control.Applicative
--import Data.Traversable
--import Data.List
import Control.Arrow

data Padded i m = (:-) {padded :: [(i, m)], padder :: m} deriving (Show, Eq)

instance Ord i => Applicative (Padded i) where
    pure = ([] :-)
    (fs :- f) <*> (ss :- s) = zapp fs ss :- f s where
        zapp [] ss = map (second f) ss
        zapp fs [] = map (second ($ s)) fs
        zapp fs0@((i, fi) : fs) ss0@((j, si) : ss)
            | i == j = (i, fi si) : zapp fs ss
            | i < j = (i, fi s) : zapp fs ss0
            | i > j = (j, f si) : zapp fs0 ss

instance Ord i => Functor (Padded i) where
    fmap = (<*>) . pure

-- a = [(1, 10), (3, 30)]
-- b = [(2, 20), (4, 40)]

--main = do
--    print $ (+) <$> (a :- 0) <*> (b :- 0)
