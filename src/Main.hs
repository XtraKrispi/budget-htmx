{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Admin
import Admin.Definitions
import Archive (renderArchive)
import Archive.Items qualified as Archive
import BusinessLogic
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Time (Day, LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), addDays, defaultTimeLocale, getZonedTime, parseTimeM)
import Db (getAllDefinitions)
import Db qualified
import Home (renderHome)
import Home.Items (renderItems)
import Htmx (hxBoost)
import Htmx.ResponseHeaders (hxTriggerResponse)
import Lucid
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (hasPrefix, staticPolicy)
import Types
import Web.Scotty (Parsable, delete, get, html, middleware, param, parseParam, post, put, scotty)

newtype UrlDay = UrlDay {getDay :: Day}

instance Parsable UrlDay where
  parseParam :: TL.Text -> Either TL.Text UrlDay
  parseParam t =
    case parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (TL.unpack t) of
      Just day -> Right (UrlDay day)
      Nothing -> Left "Invalid date"

data View = HomeV Day | AdminV | ArchiveV
  deriving (Eq)

tabs :: View -> Html ()
tabs view =
  let inactiveClass = "inline-block p-4 border-b-2 border-transparent rounded-t-lg hover:text-gray-600 hover:border-gray-300"
      activeClass = "inline-block p-4 text-blue-600 border-b-2 border-blue-600 rounded-t-lg active"
      li :: (Text, Bool, Text) -> Html ()
      li (txt, v, rt) =
        li_ [class_ "mr-2"] $
          a_
            [ hxBoost
            , href_ rt
            , class_ (if v then activeClass else inactiveClass)
            ]
            (toHtml txt)
   in div_ [class_ "text-sm font-medium text-center text-gray-500 border-b border-gray-200"] do
        ul_ [class_ "flex flex-wrap -mb-px"] do
          mapM_
            li
            [
              ( "Home"
              , case view of
                  HomeV _ -> True
                  _ -> False
              , "/"
              )
            ,
              ( "Admin"
              , case view of
                  AdminV -> True
                  _ -> False
              , "/admin"
              )
            ,
              ( "Archive"
              , case view of
                  ArchiveV -> True
                  _ -> False
              , "/archive"
              )
            ]

mainPage :: View -> Html ()
mainPage view = do
  doctypehtml_ do
    head_ do
      title_ "Budget"
      link_ [rel_ "stylesheet", href_ "/public/output.css"]
      script_ [src_ "https://unpkg.com/htmx.org@1.8.6"] ("" :: Text)
    body_ [class_ "h-screen mx-96"] do
      tabs view
      case view of
        HomeV day -> renderHome day
        AdminV -> renderAdmin
        ArchiveV -> renderArchive

main :: IO ()
main = do
  Db.scaffoldDb
  scotty 8080 do
    middleware logStdoutDev
    middleware (staticPolicy $ hasPrefix "public/")
    get "/" do
      todaysDate <- localDay . zonedTimeToLocalTime <$> liftIO getZonedTime
      html $ renderText (mainPage $ HomeV (addDays 20 todaysDate))
    get "/admin" do
      html $ renderText (mainPage AdminV)
    get "/archive" do
      html $ renderText (mainPage ArchiveV)
    get "/api/definitions" do
      defs <- liftIO getAllDefinitions
      html $ renderText do
        div_ [class_ "grid grid-cols-4 gap-4"] do
          traverse_ renderDefinition defs
    get "/api/definitions/:id" do
      i :: Int <- param "id"
      def <- liftIO $ Db.getDefinitionById i
      html $ renderText (editCreatePanel def)
    get "/api/definitions/clearform" do
      html $ renderText (editCreatePanel Nothing)
    post "/api/definitions" do
      description :: Text <- param "description"
      amount :: Double <- param "amount"
      startDate :: Day <- getDay <$> param "startDate"
      frequency :: Frequency <- param "frequency"
      liftIO $ Db.createDefinition description amount startDate frequency
      hxTriggerResponse "reload-definitions"
      html $ renderText (editCreatePanel Nothing)
    put "/api/definitions/:id" do
      i :: Int <- param "id"
      description :: Text <- param "description"
      amount :: Double <- param "amount"
      startDate :: Day <- getDay <$> param "startDate"
      frequency :: Frequency <- param "frequency"
      liftIO $ Db.updateDefinition i description amount startDate frequency
      hxTriggerResponse "reload-definitions"
      html $ renderText (editCreatePanel Nothing)
    delete "/api/definitions/:id" do
      i :: Int <- param "id"
      liftIO $ Db.deleteDefinition i
      hxTriggerResponse "reload-definitions"
      pure ()
    get "/api/items/" do
      endDate <- getDay <$> param "endDate"

      items <- getItems endDate <$> liftIO Db.getAllDefinitions
      archive <- liftIO Db.getArchive

      html $ renderText $ renderItems (trimItems archive items)
    put "/api/items/:definitionId/:date/pay" do
      definitionId :: Int <- param "definitionId"
      date <- getDay <$> param "date"

      liftIO $ Db.payForItem definitionId date

      hxTriggerResponse "load-items"

    put "/api/items/:definitionId/:date/skip" do
      definitionId :: Int <- param "definitionId"
      date <- getDay <$> param "date"

      liftIO $ Db.skipItem definitionId date

      hxTriggerResponse "load-items"
    get "/api/archive" do
      archive <- liftIO Db.getArchive
      html $ renderText $ Archive.renderItems archive