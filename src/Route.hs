module Route (Route (..), PartialRoute (..), partialRouteToPath, routeToPath) where

import Data.String (IsString)

data PartialRoute = EntriesP | InstancesP | ScratchP | ArchiveP
  deriving (Eq, Enum, Bounded)

data Route = HomeR | EntriesR | ArchiveR
  deriving (Eq, Enum, Bounded)

instance Show Route where
  show HomeR = "Home"
  show EntriesR = "Entries"
  show ArchiveR = "Archive"

routeToPath :: (IsString s) => Route -> s
routeToPath HomeR = "/"
routeToPath EntriesR = "/entries"
routeToPath ArchiveR = "/archive"

partialRouteToPath :: (IsString s) => PartialRoute -> s
partialRouteToPath EntriesP = "/partial/entries"
partialRouteToPath InstancesP = "/partial/instances"
partialRouteToPath ScratchP = "/partial/scratch"
partialRouteToPath ArchiveP = "/partial/archive"