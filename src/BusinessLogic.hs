module BusinessLogic where

import Data.Time (Day, addGregorianMonthsClip)
import Data.Time.Calendar (addDays)
import Database (Entry (..))
import Types (Frequency (..), Instance (..))

extractInstances :: Entry -> [Instance]
extractInstances entry =
  ( \d ->
      Instance
        { instanceDescription = entry.entryDescription
        , instanceAmount = entry.entryAmount
        , instanceDate = d
        , instanceFrequency = entry.entryFrequency
        , instanceEntryType = entry.entryEntryType
        }
  )
    <$> getDates entry.entryStartDate entry.entryFrequency

getDates :: Day -> Frequency -> [Day]
getDates startDate OneTime = [startDate]
getDates startDate BiWeekly = startDate : getDates (addDays 14 startDate) BiWeekly
getDates startDate Monthly = startDate : getDates (addGregorianMonthsClip 1 startDate) Monthly