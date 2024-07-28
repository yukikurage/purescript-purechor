module Choreography.Network.Worker.Config where

import Choreography (LocTm)
import Data.Map (Map)

type Config =
  { mainLocation :: LocTm
  , workerPaths :: Map LocTm String
  }
