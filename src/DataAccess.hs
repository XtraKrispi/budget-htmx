module DataAccess where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Database.Persist.Postgresql (SqlBackend, withPostgresqlConn)
import Types (HasConnectionString (getConnectionString))

runDbAction ::
  ( MonadUnliftIO m
  , MonadReader env m
  , HasConnectionString env
  ) =>
  ReaderT SqlBackend (LoggingT m) a ->
  m a
runDbAction action = do
  connectionString <- asks getConnectionString
  runStdoutLoggingT
    $ withPostgresqlConn connectionString
    $ \backend ->
      runReaderT action backend