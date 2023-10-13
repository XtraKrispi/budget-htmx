module BusinessLogic where

import Data.Function (on)
import Data.List (sortBy)
import Data.Time (Day, addGregorianMonthsClip)
import Data.Time.Calendar (addDays)
import Database (Archive (..), Entry (..))
import Database.Persist.Postgresql (Entity (..), fromSqlKey, toSqlKey)
import Types (EntryType (..), Frequency (..), Instance (..))
import Utils qualified

getApplicableInstances :: Day -> [Entity Entry] -> [Entity Archive] -> [Instance]
getApplicableInstances today entries archive = do
  let
    instances =
      sortBy (compare `on` (.instanceDate))
        $ concatMap (takeWhile ((<= addDays 28 today) . (.instanceDate)) . extractInstances) entries
   in
    filter (\i -> all (\(Entity _ a) -> toSqlKey i.instanceEntryId /= a.archiveOriginalEntryId && i.instanceDate /= a.archiveDate) archive) $ Utils.takeWhileInclusive ((/= Payday) . (.instanceEntryType)) instances

extractInstances :: Entity Entry -> [Instance]
extractInstances (Entity key entry) =
  ( \d ->
      Instance
        { instanceDescription = entry.entryDescription
        , instanceAmount = entry.entryAmount
        , instanceDate = d
        , instanceFrequency = entry.entryFrequency
        , instanceEntryType = entry.entryEntryType
        , instanceEntryId = fromSqlKey key
        }
  )
    <$> getDates entry.entryStartDate entry.entryFrequency

getDates :: Day -> Frequency -> [Day]
getDates startDate OneTime = [startDate]
getDates startDate BiWeekly = startDate : getDates (addDays 14 startDate) BiWeekly
getDates startDate Monthly = startDate : getDates (addGregorianMonthsClip 1 startDate) Monthly