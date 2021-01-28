module Data.List (toTuple) where

import Prelude (($))
import Data.List.Types (List, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

toTuple :: forall a. List a -> Maybe (Tuple a a)
toTuple (x : y : _) = Just $ Tuple x y

toTuple otherwise = Nothing
