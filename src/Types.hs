{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.List qualified as List
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (Day)
import Database.SQLite.Simple (FromRow, SQLData (SQLText))
import Database.SQLite.Simple.FromField (FieldParser, FromField, fieldData, fromField)
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField, toField)
import GHC.Generics (Generic)
import Web.Scotty (Parsable, parseParam)

assocs :: IsString s => [(Frequency, s)]
assocs =
  [ (OneTime, "One Time")
  , (Monthly, "Monthly")
  , (BiWeekly, "Bi-Weekly")
  ]

frequencyToText :: Frequency -> Text
frequencyToText frequency = snd $ head $ filter (\(f, _) -> f == frequency) assocs

parseFrequency :: (IsString s, Eq s) => s -> Maybe Frequency
parseFrequency t = fst <$> List.find (\(_, f) -> t == f) assocs

data Frequency
  = OneTime
  | Monthly
  | BiWeekly
  deriving (Eq, Generic, Enum, Bounded)

instance Parsable Frequency where
  parseParam p =
    case parseFrequency p of
      Just f -> Right f
      Nothing -> Left "Invalid frequency"

instance ToField Frequency where
  toField :: Frequency -> SQLData
  toField = SQLText . frequencyToText

instance FromField Frequency where
  fromField :: FieldParser Frequency
  fromField f = case fieldData f of
    SQLText t -> case parseFrequency t of
      Just frequency -> Ok frequency
      Nothing -> Errors []
    _ -> Errors []

data Definition = Definition
  { id :: Int
  , description :: Text
  , amount :: Double
  , startDate :: Day
  , frequency :: Frequency
  }
  deriving (Generic, FromRow)

data Item = Item
  { definitionId :: Int
  , description :: Text
  , amount :: Double
  , date :: Day
  }

data ArchiveType = Paid | Skipped

instance FromField ArchiveType where
  fromField :: FieldParser ArchiveType
  fromField f = case fieldData f of
    SQLText "paid" -> Ok Paid
    SQLText "skipped" -> Ok Skipped
    _ -> Errors []

data Archive = Archive
  { id :: Int
  , definitionId :: Int
  , description :: Text
  , amount :: Double
  , date :: Day
  , archivedDate :: Day
  , archiveType :: ArchiveType
  }
  deriving (Generic, FromRow)