module Choreography.WorkerManager.WorkerThread where

import Prelude

import Choreography.Location (LocTm)
import Choreography.Structured (fromForeign, toForeign)
import Choreography.WorkerManager.Message (MessageToWorker, MessageToMain)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (launchAff_)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Foreign (Foreign)

foreign import postMessageToMain :: Foreign -> Effect Unit
foreign import onMessageFromMain :: (Foreign -> Effect Unit) -> Effect Unit

-- | Location 毎に box が用意されている
type WorkerManager = Map LocTm { box :: AVar Foreign }

-- | WorkerManager を作成
newWorkerManager :: forall m. MonadAff m => LocTm -> Map LocTm String -> m WorkerManager
newWorkerManager mainLoc locPath = do
  mainBox <- liftAff Avar.empty
  mgr <- Map.insert mainLoc { box: mainBox } <$> for locPath \_ -> do
    box <- liftAff Avar.empty
    pure { box }

  -- | box の設定
  liftEffect $ onMessageFromMain \(msg :: Foreign) -> do
    { messageData, originLocation } <- case fromForeign @MessageToWorker msg of
      Just x -> pure x
      Nothing -> throwException $ error $ "Invalid message"

    case Map.lookup originLocation mgr of
      Just { box } -> do
        launchAff_ $ Avar.put messageData box
      Nothing -> throwException $ error $ "Invalid origin location: " <> originLocation

  pure mgr

putMessage :: forall m. MonadAff m => WorkerManager -> LocTm -> Foreign -> m Unit
putMessage _ loc msg = liftEffect $ postMessageToMain $ toForeign @MessageToMain
  { messageType: "send"
  , messageData: msg
  , targetLocation: loc
  }

takeMessage :: forall m. MonadAff m => WorkerManager -> LocTm -> m Foreign
takeMessage mgr loc = do
  case Map.lookup loc mgr of
    Just { box } -> liftAff $ Avar.take box
    Nothing -> liftEffect $ throwException $ error $ "Invalid location: " <> loc

broadcastMessage :: forall m. MonadAff m => WorkerManager -> Foreign -> m Unit
broadcastMessage _ msg = liftEffect $ postMessageToMain $ toForeign @MessageToMain
  { messageType: "bcast"
  , messageData: msg
  , targetLocation: ""
  }

terminateSelf :: forall m. MonadAff m => WorkerManager -> m Unit
terminateSelf _ = liftEffect $ postMessageToMain $ toForeign @MessageToMain
  { messageType: "terminate"
  , messageData: toForeign ""
  , targetLocation: ""
  }
