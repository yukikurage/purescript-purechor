module Choreography.Network.Worker.MainThread where

import Prelude

import Choreography.Location (LocTm)
import Choreography.Network (NetworkSig(..), Network)
import Choreography.Structured (class Structured)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (foldFree)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map, lookup, values)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Aff.AVar (empty, put, take)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Worker :: Type

foreign import data Data :: Type

instance Structured Data

foreign import isWorkerAvailable :: Effect Boolean

foreign import newWorker :: String -> Effect Worker

foreign import postMessage :: Worker -> Data -> Effect Unit

foreign import terminateWorker :: Worker -> Effect Unit

foreign import onMessage :: Worker -> (Data -> Effect Unit) -> Effect Unit

-- | Choreo が正しく動いている限り unsafe ではない
unsafeDecodeData :: forall a. Structured a => Data -> a
unsafeDecodeData = unsafeCoerce

encodeData :: forall a. Structured a => a -> Data
encodeData = unsafeCoerce

handleNetworkMain :: forall m a. MonadAff m => Map LocTm (Worker /\ AVar Data) -> NetworkSig m a -> m a
handleNetworkMain workerAndBoxes = case _ of
  Run m -> m
  Send f l a -> do
    log "Main:Send"
    worker <- case lookup l workerAndBoxes of
      Just (w /\ _) -> pure w
      Nothing -> liftEffect $ throwError $ error $ "Worker not found: " <> l
    liftEffect $ postMessage worker (f encodeData) *> pure a
  Recv l f -> do
    log "Main:Recv"
    box <- case lookup l workerAndBoxes of
      Just (_ /\ b) -> pure b
      Nothing -> liftEffect $ throwError $ error $ "Box not found: " <> l
    d <- liftAff $ take box
    pure $ f \xToA -> xToA (unsafeDecodeData d)
  BCast f a -> do
    log "Main:BCast"
    let allWorkers = map fst $ values workerAndBoxes
    for_ allWorkers \worker -> liftEffect $ postMessage worker (f encodeData)
    pure a

runNetworkMain :: forall m a. MonadAff m => MonadRec m => LocTm -> Map LocTm String -> Network m a -> m a
runNetworkMain mainLocTm workerPaths network = do
  liftEffect $ isWorkerAvailable >>= case _ of
    false -> throwError $ error "Worker is not available"
    true -> pure unit

  workerAndBoxes <- for workerPaths \path -> do
    worker <- liftEffect $ newWorker path
    box <- liftAff $ empty
    pure (worker /\ box)

  forWithIndex_ workerAndBoxes \locTm (worker /\ box) -> liftEffect $ onMessage worker \dataAndType -> do
    let { messageType, messageData, sendLocation } = unsafeDecodeData dataAndType :: { messageType :: String, messageData :: Data, sendLocation :: LocTm }
    case messageType of
      "terminate" -> terminateWorker worker
      "send" ->
        if sendLocation == mainLocTm then launchAff_ $ put messageData box
        else do
          worker' <- case lookup sendLocation workerAndBoxes of
            Just (w /\ _) -> pure w
            Nothing -> throwError $ error $ "Worker not found: " <> sendLocation
          postMessage worker' messageData
      "bcast" -> do
        launchAff_ $ put messageData box
        forWithIndex_ workerAndBoxes \locTm' (worker' /\ _) -> do
          if locTm /= locTm' then postMessage worker' messageData
          else pure unit
      _ -> throwError $ error $ "Unknown message type: " <> messageType

  foldFree (handleNetworkMain workerAndBoxes) network
