{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Db where

import Control.Exception (bracket)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Time (Day, LocalTime (localDay), getZonedTime, zonedTimeToLocalTime)
import Database.SQLite.Simple (Connection, Only (Only), close, execute, execute_, open, query, query_, setTrace)
import Types

db :: String
db = "budget.db"

scaffoldDb :: IO ()
scaffoldDb = do
  conn <- open db

  execute_
    conn
    "CREATE TABLE IF NOT EXISTS definitions ( \
    \ id INTEGER PRIMARY KEY AUTOINCREMENT \
    \ ,description TEXT \
    \ ,amount REAL\
    \ ,startDate TEXT\
    \ ,frequency TEXT\
    \);"

  execute_
    conn
    "CREATE TABLE IF NOT EXISTS archive ( \
    \id INTEGER PRIMARY KEY AUTOINCREMENT \
    \,definitionId INTEGER\
    \,description TEXT\
    \,amount REAL\
    \,date TEXT\
    \,archivedDate TEXT\
    \,type TEXT\
    \);"

  close conn

withTraceConnection :: String -> (Connection -> IO a) -> IO a
withTraceConnection d = do
  bracket
    ( do
        conn <- open d
        setTrace conn (Just TIO.putStrLn)
        pure conn
    )
    close

getAllDefinitions :: IO [Definition]
getAllDefinitions =
  withTraceConnection
    db
    (`query_` "SELECT id, description, amount, startDate, frequency FROM definitions ORDER BY description")

getDefinitionById :: Int -> IO (Maybe Definition)
getDefinitionById i =
  listToMaybe <$> withTraceConnection db (\conn -> query conn "SELECT id, description, amount, startDate, frequency FROM definitions WHERE id = ?" (Only i))

createDefinition :: Text -> Double -> Day -> Frequency -> IO ()
createDefinition description amount startDate frequency = withTraceConnection db (\conn -> execute conn "INSERT INTO definitions(description, amount, startDate, frequency) VALUES(?, ?, ?, ?)" (description, amount, startDate, frequency))

updateDefinition :: Int -> Text -> Double -> Day -> Frequency -> IO ()
updateDefinition i description amount startDate frequency = do
  let sql = "UPDATE definitions SET description = ?, amount = ?, startDate = ?, frequency = ? WHERE id = ?"
  withTraceConnection db (\conn -> execute conn sql (description, amount, startDate, frequency, i))

deleteDefinition :: Int -> IO ()
deleteDefinition i =
  withTraceConnection db (\conn -> execute conn "DELETE FROM definitions WHERE id = ?" (Only i))

getArchive :: IO [Archive]
getArchive =
  withTraceConnection
    db
    ( `query_`
        "SELECT id, definitionId, description, amount, date, archivedDate, type FROM archive"
    )

payForItem :: Int -> Day -> IO ()
payForItem definitionId date =
  withTraceConnection
    db
    ( \conn -> do
        def :: Maybe Definition <- listToMaybe <$> query conn "SELECT id, description, amount, startDate, frequency FROM definitions WHERE id = ?" (Only definitionId)
        case def of
          Just d -> do
            now <- localDay . zonedTimeToLocalTime <$> getZonedTime
            execute conn "INSERT INTO archive (definitionId, description, amount, date, archivedDate, type) VALUES (?, ?, ?, ?, ?, 'paid')" (definitionId, d.description, d.amount, date, now)
          Nothing -> pure ()
    )

skipItem :: Int -> Day -> IO ()
skipItem definitionId date =
  withTraceConnection
    db
    ( \conn -> do
        def :: Maybe Definition <- listToMaybe <$> query conn "SELECT id, description, amount, startDate, frequency FROM definitions WHERE id = ?" (Only definitionId)
        case def of
          Just d -> do
            now <- localDay . zonedTimeToLocalTime <$> getZonedTime
            execute conn "INSERT INTO archive (definitionId, description, amount, date, archivedDate, type) VALUES (?, ?, ?, ?, ?, 'skipped')" (definitionId, d.description, d.amount, date, now)
          Nothing -> pure ()
    )