module Capability where

import Data.Time (Day)
import Database (Archive, ArchiveId, Entry, EntryId)
import Database.Persist (Entity)

class (Monad m) => GetEntry m where
  getEntries :: m [Entity Entry]
  getEntry :: EntryId -> m (Maybe (Entity Entry))

class (Monad m) => WriteEntry m where
  newEntry :: Entry -> m EntryId
  updateEntry :: EntryId -> Entry -> m ()

class (Monad m) => GetArchive m where
  getArchive :: m [Entity Archive]

class (Monad m) => WriteArchive m where
  newArchive :: Archive -> m ArchiveId

class (Monad m) => Calendar m where
  today :: m Day