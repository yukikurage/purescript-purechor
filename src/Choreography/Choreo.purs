module Choreography.Choreo where

import Prelude

import Choreography.Location (type (#), LocTm, Located(..), toLocTm, unsafeUnwrap, wrap)
import Choreography.Network (Network, broadcast, recv, run, send)
import Choreography.Structured (class Structured)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Symbol (class IsSymbol)

type Unwrap l = forall a. a # l -> a

-- | めっちゃ unsafe な実装
-- | 下の命令を実行する用の関数を使えば安全
data ChoreoSig m a
  = Local (forall r. (forall @l x. IsSymbol l => (Unwrap l -> m x) -> (x # l -> a) -> r) -> r)
  | Comm (forall r. (forall @l @l' x. IsSymbol l => IsSymbol l' => Structured x => x # l -> (x # l' -> a) -> r) -> r)
  | Cond (forall r. (forall @l x. IsSymbol l => Structured x => x # l -> (x -> Choreo m a) -> r) -> r)

type Choreo m = Free (ChoreoSig m)

-- | Run Choreo sequentially
runChoreo :: forall m. MonadRec m => Choreo m ~> m
runChoreo = foldFree handler
  where
  handler :: ChoreoSig m ~> m
  handler = case _ of
    Local f -> f \m k -> (k <<< wrap) <$> m unsafeUnwrap
    Comm f -> f \x k -> k <$> (pure $ wrap $ unsafeUnwrap x)
    Cond f -> f \x c -> runChoreo $ c $ unsafeUnwrap x

-- | Endpoint projection
epp :: forall m. LocTm -> Choreo m ~> Network m
epp l' = foldFree handler
  where
  handler :: forall a. ChoreoSig m a -> Network m a
  handler = case _ of
    Local f -> f handleLocal
    Comm f -> f handleComm
    Cond f -> f handleCond

    where
    handleLocal :: forall @l x. IsSymbol l => (Unwrap l -> m x) -> (x # l -> a) -> Network m a
    handleLocal m k
      | toLocTm @l == l' = (k <<< wrap) <$> run (m unsafeUnwrap)
      | otherwise = k <$> pure Empty

    handleComm :: forall @s @r x. IsSymbol s => IsSymbol r => Structured x => x # s -> (x # r -> a) -> Network m a
    handleComm x k
      | toLocTm @s == toLocTm @r = k <$> (pure $ wrap $ unsafeUnwrap x)
      | toLocTm @s == l' = k <$> (send (unsafeUnwrap x) (toLocTm @r) *> pure Empty)
      | toLocTm @r == l' = (k <<< wrap) <$> recv (toLocTm @s)
      | otherwise = k <$> pure Empty

    handleCond :: forall @l x. IsSymbol l => Structured x => x # l -> (x -> Choreo m a) -> Network m a
    handleCond x c
      | toLocTm @l == l' = broadcast (unsafeUnwrap x) *> epp l' (c (unsafeUnwrap x))
      | otherwise = recv (toLocTm @l) >>= \y -> epp l' (c y)

locally :: forall @l m x. IsSymbol l => (Unwrap l -> m x) -> Choreo m (x # l)
locally m = liftF $ Local \f -> f m identity

comm :: forall @l m x. IsSymbol l => Structured x => x # l -> forall @l'. IsSymbol l' => Choreo m (x # l')
comm x = liftF $ Comm \f -> f x identity

cond :: forall @l m x a. IsSymbol l => Structured x => x # l -> (x -> Choreo m a) -> Choreo m a
cond x c = liftF $ Cond \f -> f x c

comm' :: forall @l m x. IsSymbol l => Structured x => (Unwrap l -> m x) -> forall @l'. IsSymbol l' => Choreo m (x # l')
comm' m = locally m >>= \x -> comm x

cond' :: forall @l m x a. IsSymbol l => Structured x => (Unwrap l -> m x) -> (x -> Choreo m a) -> Choreo m a
cond' m c = locally m >>= \x -> cond x c
