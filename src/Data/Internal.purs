module Data.Accounting.Internal (nonZero, NonZero, toTuple) where

import Prelude
import Data.List.Types (List, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- Use the NonZero from Types
newtype NonZero
  = NonZero Int

toTuple :: forall a. List a -> Maybe (Tuple a a)
toTuple = case _ of
  x : y : _ -> Just $ Tuple x y
  otherwise -> Nothing

nonZero :: Int -> Maybe NonZero
nonZero int
  | int /= 0 = Just $ NonZero int
  | otherwise = Nothing
