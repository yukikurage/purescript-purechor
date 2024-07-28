module Choreography
  ( module Choreography.Location
  , module Choreo
  , runChoreography
  ) where

import Choreography.Network

import Choreography.Choreo (Choreo, locally, cond, cond', runChoreo) as Choreo
import Choreography.Choreo (epp)
import Choreography.Location (type (#), LocTm, LocTy, toLocTm)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Symbol (class IsSymbol)
import Effect.Aff.Class (class MonadAff)

-- | Run a choreography with a message transport backend.
runChoreography
  :: forall @l config m a
   . IsSymbol l
  => Backend config
  => MonadAff m
  => MonadRec m
  => config
  -> Choreo.Choreo m a
  -> m a
runChoreography cfg choreo = runNetwork cfg (toLocTm @l) (epp (toLocTm @l) choreo)
