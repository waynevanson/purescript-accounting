module Accounting (Value, Transaction, credit, debit, transaction, Entry) where

import Prelude
import Data.Accounting.Internal (NonZero, nonZero)
import Data.Bifunctor (bimap)
import Data.Date (Date)
import Data.Either (Either(..))
import Data.Filterable (partitionMap)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList, fromList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- Data
data Value
  = Credit NonZero
  | Debit NonZero

-- The sum of all credits and debits Value are equal
data Transaction
  = Transaction
    { credits :: NonEmptyList Value
    , date :: Date
    , debits :: NonEmptyList Value
    }

data Entry
  = Entry
    { account :: Account
    , description :: String
    , value :: Value
    }

data Account
  = Asset
  | Liability
  | Expense
  | Revenue
  | Equity

-- Constructors
--
-- | Create a `Credit` from an `Int`.
credit :: Int -> Maybe Value
credit int = nonZero >>> map Credit $ int

-- | Create a `Debit` from an `Int`.
debit :: Int -> Maybe Value
debit int = nonZero >>> map Debit $ int

-- | Create an abstraction
transaction :: Date -> List Entry -> Maybe Transaction
transaction date entries =
  partitionMap
    ( \(Entry { value }) -> case value of
        Credit _ -> Left value
        Debit _ -> Right value
    )
    >>> (\{ left, right } -> Tuple left right)
    >>> bimap fromList fromList
    >>> case _ of
        Tuple (Just credits) (Just debits) -> Just $ Transaction { date, credits, debits }
        otherwise -> Nothing
    $ entries
