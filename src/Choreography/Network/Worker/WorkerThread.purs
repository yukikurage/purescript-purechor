module Choreography.Network.Worker.WorkerThread where

import Prelude

import Choreography.Network (NetworkSig(..), Network)
import Choreography.Structured (class Structured)
import Control.Monad.Free (foldFree)
import Control.Monad.Rec.Class (class MonadRec)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (launchAff_)
import Effect.Aff.AVar (empty, put, take)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Data :: Type

instance Structured Data

foreign import postMessageToMain :: Data -> Effect Unit
foreign import onMessage :: (Data -> Effect Unit) -> Effect Unit

-- | Choreo が正しく動いている限り unsafe ではない
unsafeDecodeData :: forall a. Structured a => Data -> a
unsafeDecodeData = unsafeCoerce

encodeData :: forall a. Structured a => a -> Data
encodeData = unsafeCoerce

handleNetworkWorker :: forall m a. MonadAff m => AVar Data -> NetworkSig m a -> m a
handleNetworkWorker box = case _ of
  Run m -> m
  Send f l a -> do
    log "Worker:Send"
    liftEffect $ postMessageToMain (encodeData { messageType: "send", messageData: f encodeData, sendLocation: l }) *> pure a
  -- Recv の順番によってはバグりそう
  -- Worker が 2 つ以上起動しているとき、他の Worker からのメッセージが先に来るとバグる
  Recv _ f -> do
    log "Woker:Recv"
    d <- liftAff $ take box
    pure $ f \xToA -> xToA (unsafeDecodeData d)
  BCast f a -> do
    log "Worker:BCast"
    liftEffect $ postMessageToMain (encodeData { messageType: "bcast", messageData: f encodeData, sendLocation: "" }) *> pure a

runNetworkWorker :: forall m a. MonadRec m => MonadAff m => Network m a -> m a
runNetworkWorker network = do
  box <- liftAff $ empty
  liftEffect $ onMessage \d -> launchAff_ $ put d box
  a <- foldFree (handleNetworkWorker box) network

  liftEffect $ postMessageToMain (encodeData { messageType: "terminate", messageData: "", sendLocation: "" })

  pure a
