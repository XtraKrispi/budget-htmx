module Main (main) where

import AppM (AppM (unApp))
import Capability (GetEntry (getEntries))
import Control.Monad.Trans (MonadIO (..), lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.ByteString.Char8 qualified as BS
import Data.Text.Internal.Lazy (Text)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import DataAccess (runDbAction)
import Database (Entry (..), migrateAll)
import Database.Persist.Postgresql (PersistStoreWrite (insert), runMigration)
import Html.FullPage.Archive qualified as Archive
import Html.FullPage.Entries qualified as Entries
import Html.FullPage.Home qualified as Home
import Html.Partials.Entries qualified as Partial.Entries
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import System.Environment (lookupEnv)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Types (Env (..))
import Web.Scotty.Trans (
  ScottyT,
  get,
  html,
  middleware,
  param,
  post,
  scottyT,
 )

webApp :: ScottyT Text AppM ()
webApp = fullPages <> partials

fullPages :: ScottyT Text AppM ()
fullPages = do
  get "/" do
    html $ renderHtml Home.render
  get "/entries" do
    html $ renderHtml Entries.render
  get "/archive" do
    html $ renderHtml Archive.render

partials :: ScottyT Text AppM ()
partials = do
  get "/partial/entries" do
    entries <- lift getEntries
    html $ renderHtml $ Partial.Entries.render entries
  post "/partial/entries" do
    liftIO $ putStrLn "Here!"
    description <- param "new-description"
    amount <- param "new-amount"
    frequency <- param "new-frequency"
    entryType <- param "new-entry-type"
    startDate :: Maybe Day <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d" <$> param "new-start-date"
    -- Need to do validation for all params
    case startDate of
      Nothing -> pure ()
      Just d -> do
        let newEntry = Entry description amount d frequency entryType False
        _ <- lift $ runDbAction $ insert newEntry
        pure ()
    entries <- lift getEntries
    -- Want to do an out of band swap here for the entries list
    html $ renderHtml Partial.Entries.newEntryForm <> renderHtml (Partial.Entries.render entries)

main :: IO ()
main = do
  connectionString <- maybe "host=localhost port=5432 dbname=budget user=postgres" BS.pack <$> lookupEnv "CONNECTION_STRING"
  let env = Env connectionString
  runReaderT
    ( runDbAction
        $ runMigration migrateAll
    )
    env
  scottyT 8080 (flip runReaderT env . unApp) do
    middleware $ staticPolicy (addBase "static")
    webApp
