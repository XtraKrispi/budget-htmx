module Main (main) where

import AppM (AppM (unApp))
import Capability (GetEntry (getEntries))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.ByteString.Char8 qualified as BS
import Data.Text.Internal.Lazy (Text)
import DataAccess (runDbAction)
import Database (migrateAll)
import Database.Persist.Postgresql (runMigration)
import Html.FullPage.Archive qualified as Archive
import Html.FullPage.Entries qualified as Entries
import Html.FullPage.Home qualified as Home
import Html.Partials.Entries qualified as Partial.Entries
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import System.Environment (lookupEnv)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Types (Env (..))
import Web.Scotty.Trans (ScottyT, get, html, middleware, scottyT)

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
