module Data.TwoSet where

import Prelude

import Data.Eq (class Eq1)
import Data.Function (on)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Ord (class Ord1)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple

-- Can also think of it as an unordered tuple. But `TwoSet` is shorter.
data TwoSet a = MkTwoSet a a

derive instance genericTwoSet :: Generic (TwoSet a) _

instance showTwoSet :: (Show a) => Show (TwoSet a) where
  show = genericShow

instance eqTwoSet :: Eq a => Eq (TwoSet a) where
  eq l r = toTuple l == toTuple r || toTuple l == Tuple.swap (toTuple r)
instance eq1TwoSet :: Eq1 TwoSet where
  eq1 = eq

instance ordTwoSet :: Ord a => Ord (TwoSet a) where
  compare = compare `on` sort
    where
      sort (MkTwoSet a b) = Tuple (min a b) (max a b)
instance ord1 :: Ord1 TwoSet where
  compare1 = compare

instance boundedTwoSet :: (Bounded a) => Bounded (TwoSet a) where
  top = MkTwoSet top top
  bottom = MkTwoSet bottom bottom

derive instance functorTwoSet :: Functor TwoSet
instance invariantTwoSet :: Invariant TwoSet where
  imap = imapF

-- We could define more instances but I'll stop for now.
-- Most of the remaining instances would require us to define a conventional order
-- (like `Set`'s folds working in ascending order).

fromTuple :: forall a. Tuple a a -> TwoSet a
fromTuple (Tuple a b) = MkTwoSet a b

toTuple :: forall a. TwoSet a -> Tuple a a
toTuple (MkTwoSet a b) = Tuple a b
