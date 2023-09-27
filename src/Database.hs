{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import Data.Text (Text)
import Data.Time (Day)
import Database.Persist.TH (
  mkMigrate,
  mkPersist,
  persistLowerCase,
  share,
  sqlSettings,
 )
import Types (ArchiveAction, EntryType, Frequency)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Entry
  description Text
  amount Double
  startDate Day
  frequency Frequency
  entryType EntryType sql=type
  isDeleted Bool
  deriving Show

Archive
  originalEntryId EntryId
  description Text
  amount Double
  date Day
  frequency Frequency
  entryType EntryType sql=type
  action ArchiveAction
  deriving Show
|]