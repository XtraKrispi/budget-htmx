module Utils where

import Data.Text (Text, pack)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)

formatDate :: Day -> String
formatDate = formatShow iso8601Format

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%-m-%-d"

tShow :: (Show a) => a -> Text
tShow = pack . show

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x : xs) =
  x
    : if p x
      then takeWhileInclusive p xs
      else []