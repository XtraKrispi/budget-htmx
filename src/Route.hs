module Route where

import Data.String (IsString)

data Route = Home | Entries | Archive
  deriving (Show, Eq, Enum, Bounded)

routeToPath :: (IsString s) => Route -> s
routeToPath Home = "/"
routeToPath Entries = "/entries"
routeToPath Archive = "/archive"