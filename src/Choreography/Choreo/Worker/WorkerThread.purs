module Choreography.Choreo.Worker.WorkerThread where

import Prelude

import Choreography.Choreo (Choreo, epp)
import Choreography.Location (LocTm)
import Choreography.Network.Worker.WorkerThread (runNetworkWorker)
import Control.Monad.Rec.Class (class MonadRec)
import Effect.Aff.Class (class MonadAff)

runChoreoWorker
  :: forall m a
   . MonadAff m
  => MonadRec m
  => LocTm
  -> Choreo m a
  -> m a
runChoreoWorker workerLocTm choreo = runNetworkWorker $ epp workerLocTm choreo
