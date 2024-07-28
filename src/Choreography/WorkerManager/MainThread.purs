module Choreography.WorkerManager.MainThread where

import Prelude

import Choreography.Location (LocTm)
import Choreography.Structured (fromForeign, toForeign)
import Choreography.WorkerManager.Message (MessageToMain, MessageToWorker)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (error, launchAff_)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Foreign (Foreign)

foreign import data Worker :: Type

foreign import isWorkerAvailable :: Effect Boolean

foreign import newWorker :: String -> Effect Worker

foreign import postMessage :: Worker -> Foreign -> Effect Unit

foreign import terminateWorker :: Worker -> Effect Unit

foreign import onMessage :: Worker -> (Foreign -> Effect Unit) -> Effect Unit

type WorkerManager = { mainLocation :: LocTm, workers :: Map LocTm { worker :: Worker, box :: AVar Foreign } }

-- | WorkerManager を作成
newWorkerManager :: forall m. MonadAff m => LocTm -> Map LocTm String -> m WorkerManager
newWorkerManager mainLocation locPath = do
  liftEffect $ isWorkerAvailable >>= case _ of
    false -> throwException $ error "Worker is not available"
    true -> pure unit

  wks <- for locPath \path -> do
    worker <- liftEffect $ newWorker path
    box <- liftAff Avar.empty
    pure { worker, box }

  -- | box 及び Proxy の設定
  forWithIndex_ wks \originLocation { worker: originWorker, box } -> do
    liftEffect $ onMessage (originWorker) \(msg :: Foreign) -> do
      { messageType, messageData, targetLocation } <- case fromForeign @MessageToMain msg of
        Just x -> pure x
        Nothing -> liftEffect $ throwException $ error $ "Invalid message"
      case messageType of
        "send" -> do
          case Map.lookup targetLocation wks of
            Just { worker: targetWorker } -> do
              -- 転送
              postMessage targetWorker (toForeign @MessageToWorker { messageData, originLocation })
            Nothing | mainLocation == targetLocation -> do
              -- Main にためる
              launchAff_ $ Avar.put messageData box
            _ -> do
              liftEffect $ throwException $ error $ "Invalid target location: " <> targetLocation
        "bcast" -> do
          launchAff_ $ Avar.put messageData box
          forWithIndex_ wks \eachLocation { worker: targetWorker } -> do
            when (eachLocation /= originLocation) do
              postMessage targetWorker (toForeign @MessageToWorker { messageData, originLocation })
        "terminate" -> do
          terminateWorker originWorker
          pure unit
        _ -> do
          liftEffect $ throwException $ error $ "Invalid message type: " <> messageType

  pure { mainLocation, workers: wks }

-- | Main から Worker にメッセージを送信
putMessage :: forall m. MonadAff m => WorkerManager -> LocTm -> Foreign -> m Unit
putMessage { mainLocation, workers } locTm msg = do
  case Map.lookup locTm workers of
    Just { worker } -> do
      liftEffect $ postMessage worker $ toForeign @MessageToWorker
        { messageData: msg
        , originLocation: mainLocation
        }
    Nothing -> do
      liftEffect $ throwException $ error $ "Worker not found: " <> locTm

-- | 特定の Worker からのメッセージを受信
takeMessage :: forall m. MonadAff m => WorkerManager -> LocTm -> m Foreign
takeMessage { workers } locTm = do
  case Map.lookup locTm workers of
    Just { box } -> do
      liftAff $ Avar.take box
    Nothing -> do
      liftEffect $ throwException $ error $ "Worker not found: " <> locTm

broadcastMessage :: forall m. MonadAff m => WorkerManager -> Foreign -> m Unit
broadcastMessage { workers } msg = do
  for_ workers \{ worker } -> do
    liftEffect $ postMessage worker $ toForeign @MessageToWorker
      { messageData: msg
      , originLocation: "main"
      }
