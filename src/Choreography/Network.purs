module Choreography.Network where

import Prelude

import Choreography.Location (LocTm)
import Choreography.Structured (class Structured)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Rec.Class (class MonadRec)
import Effect.Aff.Class (class MonadAff)

data NetworkSig m a
  = Run (m a)
  | Send (forall r. (forall x. Structured x => x -> r) -> r) LocTm a
  | Recv LocTm (forall r. (forall x. Structured x => (x -> a) -> r) -> r)
  | BCast (forall r. (forall x. Structured x => x -> r) -> r) a

type Network m = Free (NetworkSig m)

run :: forall m a. m a -> Network m a
run = liftF <<< Run

send :: forall m a. Structured a => a -> LocTm -> Network m Unit
send a l = liftF $ Send (\f -> f a) l unit

recv :: forall m a. Structured a => LocTm -> Network m a
recv l = liftF $ Recv l (\f -> f identity)

broadcast :: forall m a. Structured a => a -> Network m Unit
broadcast a = liftF $ BCast (\f -> f a) unit

class Backend c where
  runNetwork :: forall m a. MonadAff m => MonadRec m => c -> LocTm -> Network m a -> m a
