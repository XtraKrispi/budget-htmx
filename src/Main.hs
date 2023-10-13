module Main (main) where

import AppM (AppM (unApp))
import BusinessLogic (getApplicableInstances)
import Capability (Calendar (today), GetArchive (getArchive), GetEntry (..), WriteEntry (..), newArchive)
import Control.Applicative (Applicative (liftA2))
import Control.Monad.Trans (MonadIO (..), lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.ByteString.Char8 qualified as BS
import Data.Text.Internal.Lazy (Text)
import DataAccess (runDbAction)
import Database (Archive (..), Entry (..), migrateAll)
import Database.Persist.Postgresql (Entity (..), runMigration, toSqlKey)
import Html.FullPage.Archive qualified as Archive
import Html.FullPage.Entries qualified as Entries
import Html.FullPage.Home qualified as Home
import Html.Partials.Archive qualified as Partials.Archive
import Html.Partials.Entries qualified as Partials.Entries
import Html.Partials.Instances qualified as Partials.Instances
import Html.Partials.Scratch qualified as Partials.Scratch
import Network.HTTP.Types (status400, status404)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import System.Environment (lookupEnv)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Types (Env (..), Instance (..))
import Utils qualified
import Web.Scotty.Trans (
  ScottyT,
  delete,
  get,
  html,
  middleware,
  param,
  params,
  post,
  put,
  scottyT,
  setHeader,
  status,
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
  get "/partial/instances" do
    date <- lift today
    entries <- lift getEntries
    archive <- lift getArchive
    let instances = getApplicableInstances date entries archive
    html $ renderHtml $ Partials.Instances.render instances
  get "/partial/scratch" do
    date <- lift today
    entries <- lift getEntries
    archive <- lift getArchive
    let instances = getApplicableInstances date entries archive
    let amt = sum (((.instanceAmount)) <$> instances)
    html $ renderHtml $ Partials.Scratch.render amt
  get "/partial/entries" do
    entries <- lift getEntries
    html $ renderHtml $ Partials.Entries.render entries
  get "/partial/entries/new-form" do
    html $ renderHtml $ Partials.Entries.entryForm Nothing
  get "/partial/entries/:id" do
    entryId <- param "id"
    mEntry <- lift $ getEntry $ toSqlKey entryId
    maybe (status status404) (html . renderHtml . Partials.Entries.entryForm . Just) mEntry
  delete "/entries/:id" do
    entries <- lift getEntries
    html $ renderHtml (Partials.Entries.render entries)
  post "/entries" do
    description <- param "new-description"
    amount <- param "new-amount"
    frequency <- param "new-frequency"
    entryType <- param "new-entry-type"
    startDate <- Utils.parseDate <$> param "new-start-date"
    -- Need to do validation for all params
    case startDate of
      Nothing -> pure ()
      Just d -> do
        let entry = Entry description amount d frequency entryType False
        _ <- lift $ newEntry entry
        pure ()
    setHeader "HX-Trigger" "refreshEntries"
    html $ renderHtml (Partials.Entries.entryForm Nothing)
  put "/entries/:id" do
    entryId <- toSqlKey <$> param "id"
    existingEntry <- lift $ getEntry $ entryId
    case existingEntry of
      Nothing -> status status404
      Just _ -> do
        description <- param "new-description"
        amount <- param "new-amount"
        frequency <- param "new-frequency"
        entryType <- param "new-entry-type"
        startDate <- Utils.parseDate <$> param "new-start-date"
        -- Need to do validation for all params
        case startDate of
          Nothing -> pure ()
          Just d -> do
            let entry = Entry description amount d frequency entryType False
            _ <- lift $ updateEntry entryId entry
            pure ()
        setHeader "HX-Trigger" "refreshEntries"
        html $ renderHtml (Partials.Entries.entryForm Nothing)
  get "/partial/archive" do
    archive <- lift getArchive
    html $ renderHtml $ Partials.Archive.render archive
  post "/archive" do
    params >>= liftIO . print
    entryId <- toSqlKey <$> param "entryId"
    mEntryAndDate <- liftA2 (,) <$> lift (getEntry entryId) <*> (Utils.parseDate <$> param "date")
    action <- read <$> param "action"
    maybe
      (status status400)
      ( \(Entity _ entry, d) -> do
          _ <-
            lift
              $ newArchive
                ( Archive
                    { archiveOriginalEntryId = entryId
                    , archiveAction = action
                    , archiveAmount = entry.entryAmount
                    , archiveDescription = entry.entryDescription
                    , archiveDate = d
                    , archiveFrequency = entry.entryFrequency
                    , archiveEntryType = entry.entryEntryType
                    }
                )
          setHeader "HX-Trigger" "refreshInstances, refreshScratch"
      )
      mEntryAndDate

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
