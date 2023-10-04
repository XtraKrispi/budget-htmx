module AppM where

import Capability (Calendar (today), GetEntry (..), WriteEntry (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Time (Day, UTCTime (utctDay), getCurrentTime)
import DataAccess (runDbAction)
import Database (EntityField (EntryDescription), Entry, EntryId, Key)
import Database.Persist (Entity, SelectOpt (Asc), getEntity, insert, selectList)
import Types (Env)

newtype AppM a = App {unApp :: ReaderT Env IO a}
  deriving
    ( Monad
    , Applicative
    , Functor
    , MonadReader Env
    , MonadIO
    , MonadUnliftIO
    )

instance Calendar AppM where
  today :: AppM Day
  today = utctDay <$> liftIO getCurrentTime

instance GetEntry AppM where
  getEntries :: AppM [Entity Entry]
  getEntries = runDbAction $ selectList [] [Asc EntryDescription]

  getEntry :: Key Entry -> AppM (Maybe (Entity Entry))
  getEntry = runDbAction . getEntity

instance WriteEntry AppM where
  newEntry :: Entry -> AppM EntryId
  newEntry = runDbAction . insert