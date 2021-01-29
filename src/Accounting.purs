module Accounting (Value, Transaction, credit, debit, transaction, Entry, Credit, Debit) where

import Prelude
import Data.Accounting.Internal (NonZero(..), nonZero)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bifoldl, bitraverse)
import Data.Date (Date)
import Data.Either (Either)
import Data.Filterable (partitionMap)
import Data.Foldable (foldl)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList, fromList)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

newtype Credit
  = CR NonZero

derive instance eqCredit :: Eq Credit

newtype Debit
  = DR NonZero

derive instance eqDebit :: Eq Debit

type Value
  = Either Credit Debit

data Entry
  = Entry
    { account :: Account
    , description :: String
    , value :: Value
    }

-- The sum of all credits and debits Value are equal
data Transaction
  = Transaction
    { credits :: NonEmptyList Credit
    , date :: Date
    , debits :: NonEmptyList Debit
    }

data Account
  = Asset
  | Liability
  | Expense
  | Revenue
  | Equity

-- | Create a `Credit` from an `Int`.
credit :: Int -> Maybe Credit
credit int = nonZero >>> map CR $ int

-- | Create a `Debit` from an `Int`.
debit :: Int -> Maybe Debit
debit int = nonZero >>> map DR $ int

-- | Create a `Transaction` using a `Date` and a list of `Entry`'s.
transaction :: Date -> List Entry -> Maybe Transaction
transaction date entries = do
  tcrsdrs <-
    partitionMap (\(Entry { value }) -> value)
      >>> (\{ left, right } -> Tuple left right)
      >>> bitraverse fromList fromList
      $ entries
  -- ensure the difference between both sides is not zero
  _ <-
    pure
      $ bimap (map \(CR (NonZero i)) -> i) (map \(DR (NonZero i)) -> i)
      >>> bimap (foldl (+) 0) (foldl (+) 0)
      >>> bifoldl (-) (+) 0
      $ tcrsdrs
  -- deconstruct args
  Tuple credits debits <- pure tcrsdrs
  pure $ Transaction { date, credits, debits }
