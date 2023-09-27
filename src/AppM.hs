module AppM where

import Capability (Calendar (today), GetEntry (getEntries))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Time (Day, UTCTime (utctDay), getCurrentTime)
import DataAccess (runDbAction)
import Database (EntityField (EntryDescription))
import Database.Persist (SelectOpt (Asc), selectList)
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
  getEntries = do
    runDbAction $ selectList [] [Asc EntryDescription]