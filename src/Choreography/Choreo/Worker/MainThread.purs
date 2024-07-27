module Choreography.Choreo.Worker.MainThread where

import Prelude

import Choreography.Choreo (Choreo, epp)
import Choreography.Location (LocTm)
import Choreography.Network.Worker.MainThread (runNetworkMain)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Map (Map)
import Effect.Aff.Class (class MonadAff)

runChoreoMain
  :: forall m a
   . MonadAff m
  => MonadRec m
  => LocTm
  -> Map LocTm String
  -> Choreo m a
  -> m a
runChoreoMain mainLocTm workerPaths choreo = runNetworkMain mainLocTm workerPaths $ epp mainLocTm choreo
