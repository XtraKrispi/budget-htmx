{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Int (Int64)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Internal.Lazy qualified as Lazy
import Data.Time (Day)
import Database.Persist.Postgresql (ConnectionString)
import Database.Persist.TH (derivePersistField)
import Web.Scotty (Parsable)
import Web.Scotty.Trans (Parsable (..))

class HasConnectionString m where
  getConnectionString :: m -> ConnectionString

newtype Env = Env {connectionString :: ConnectionString}
  deriving (Show)

instance HasConnectionString Env where
  getConnectionString :: Env -> ConnectionString
  getConnectionString = connectionString

data Frequency = OneTime | BiWeekly | Monthly
  deriving
    ( Show
    , Read
    , Eq
    , Enum
    , Bounded
    )

derivePersistField "Frequency"

instance Parsable Frequency where
  parseParam :: Lazy.Text -> Either Lazy.Text Frequency
  parseParam "OneTime" = pure OneTime
  parseParam "BiWeekly" = pure BiWeekly
  parseParam "Monthly" = pure Monthly
  parseParam p = Left $ "Invalid Frequency Parameter: " <> p

frequencyDisplay :: (IsString a) => Frequency -> a
frequencyDisplay OneTime = "One Time"
frequencyDisplay BiWeekly = "Bi-Weekly"
frequencyDisplay Monthly = "Monthly"

data EntryType = Payday | Expense
  deriving (Show, Read, Eq)

derivePersistField "EntryType"

instance Parsable EntryType where
  parseParam :: Lazy.Text -> Either Lazy.Text EntryType
  parseParam "Payday" = pure Payday
  parseParam "Expense" = pure Expense
  parseParam p = Left $ "Invalid EntryType Parameter: " <> p

entryTypeDisplay :: (IsString a) => EntryType -> a
entryTypeDisplay Payday = "Payday"
entryTypeDisplay Expense = "Expense"

data ArchiveAction = Paid | Skipped
  deriving (Show, Read)

derivePersistField "ArchiveAction"

data Instance = Instance
  { instanceDescription :: Text
  , instanceAmount :: Double
  , instanceDate :: Day
  , instanceFrequency :: Frequency
  , instanceEntryType :: EntryType
  , instanceEntryId :: Int64
  }
  deriving (Show)