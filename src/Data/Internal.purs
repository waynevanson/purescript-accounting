module Data.Accounting.Internal (nonZero, NonZero(..), toTuple) where

import Data.Boolean (otherwise)
import Data.Eq (class Eq, (/=))
import Data.Function (($))
import Data.List.Types (List, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

newtype NonZero
  = NonZero Int

derive instance eqNonZero :: Eq NonZero

toTuple :: forall a. List a -> Maybe (Tuple a a)
toTuple = case _ of
  x : y : _ -> Just $ Tuple x y
  otherwise -> Nothing

nonZero :: Int -> Maybe NonZero
nonZero int
  | int /= 0 = Just $ NonZero int
  | otherwise = Nothing
