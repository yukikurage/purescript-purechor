module Choreography.Network.Worker.WorkerThread where

import Prelude

import Choreography.Network (class Backend, Network, NetworkSig(..))
import Choreography.Network.Worker.Config (Config)
import Choreography.Structured (fromForeign, toForeign)
import Choreography.WorkerManager.WorkerThread as WM
import Control.Monad.Free (foldFree)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe(..))
import Effect.Aff (error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)

handleNetworkMain :: forall m a. MonadAff m => WM.WorkerManager -> NetworkSig m a -> m a
handleNetworkMain wm = case _ of
  Run m -> m
  Send f l a -> do
    WM.putMessage wm l $ f toForeign
    pure a
  Recv l f -> do
    msg <- liftAff $ WM.takeMessage wm l
    f \xToA -> do
      strct <- case fromForeign msg of
        Just x -> pure x
        Nothing -> liftEffect $ throwException $ error "Invalid message"
      pure $ xToA strct
  BCast f a -> do
    WM.broadcastMessage wm $ f toForeign
    pure a

runNetworkMain :: forall m a. MonadAff m => MonadRec m => Config -> Network m a -> m a
runNetworkMain { mainLocation, workerPaths } network = do
  wm <- WM.newWorkerManager mainLocation workerPaths
  foldFree (handleNetworkMain wm) network

newtype WorkerConfig = WorkerConfig Config

instance Backend WorkerConfig where
  runNetwork (WorkerConfig config) _ network = runNetworkMain config network
