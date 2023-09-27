{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Text (Text)
import Data.Time (Day)
import Database.Persist.Postgresql (ConnectionString)
import Database.Persist.TH (derivePersistField)

class HasConnectionString m where
  getConnectionString :: m -> ConnectionString

newtype Env = Env {connectionString :: ConnectionString}
  deriving (Show)

instance HasConnectionString Env where
  getConnectionString :: Env -> ConnectionString
  getConnectionString = connectionString

data Frequency = BiWeekly | Monthly | OneTime
  deriving (Show, Read)

derivePersistField "Frequency"

data EntryType = Payday | Expense
  deriving (Show, Read)

derivePersistField "EntryType"

data ArchiveAction = Completed | Skipped
  deriving (Show, Read)

derivePersistField "ArchiveAction"

data Instance = Instance
  { instanceDescription :: Text
  , instanceAmount :: Double
  , instanceDate :: Day
  , instanceFrequency :: Frequency
  , instanceEntryType :: EntryType
  }
  deriving (Show)