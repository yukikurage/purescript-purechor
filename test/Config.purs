module Test.Config where

import Choreography.Location (toLocTm)
import Choreography.Network.Worker.Config (Config)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Test.Program (A, B, Main)

config :: Config
config =
  { mainLocation: toLocTm @Main
  , workerPaths: Map.fromFoldable [ toLocTm @A /\ "workerA.js", toLocTm @B /\ "workerB.js" ]
  }
