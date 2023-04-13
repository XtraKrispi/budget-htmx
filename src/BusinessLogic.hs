{-# LANGUAGE OverloadedRecordDot #-}

module BusinessLogic where

import Data.List qualified as L
import Data.Time (Day, addDays, addGregorianMonthsClip)
import Types

getDates :: Day -> Frequency -> [Day]
getDates startDate OneTime = [startDate]
getDates startDate Monthly =
  startDate : getDates (addGregorianMonthsClip 1 startDate) Monthly
getDates startDate BiWeekly =
  startDate : getDates (addDays 14 startDate) BiWeekly

getItems :: Day -> [Definition] -> [Item]
getItems endDate defs =
  let getItem def =
        let dates = takeWhile (<= endDate) $ getDates def.startDate def.frequency
         in (Item def.id def.description def.amount) <$> dates
   in defs >>= getItem

trimItems :: [Archive] -> [Item] -> [Item]
trimItems archive =
  L.filter (\item -> all (\a -> item.date /= a.date && item.definitionId == a.definitionId) archive)