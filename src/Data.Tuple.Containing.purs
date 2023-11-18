module Data.Tuple.Containing where

import Prelude

import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

-- | given a tuple of any size with at least 1 value
-- | of type `a`, `extract` the first occurence of `a`
-- | from the tuple
class TupleContaining @a tup where
  extract :: tup -> a

instance TupleContaining a a where
  extract = identity
else instance TupleContaining a (a /\ b) where
  extract = fst
else instance TupleContaining b (a /\ b) where
  extract = snd
else instance TupleContaining b (a /\ b /\ Unit) where
  extract (_ /\ b /\ _) = b
else instance TupleContaining a tail => TupleContaining a (Tuple head tail) where
  extract (_ /\ tail) = extract tail

