module Test.Program where

import Prelude

import Choreography (Choreo, LocTy, cond, locally)
import Choreography.Choreo (comm')
import Choreography.Location (wrap)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

type Main :: LocTy
type Main = "main"

type A :: LocTy
type A = "a"

type B :: LocTy
type B = "b"

-- Main -> A, A -> B, B -> Main という通信を行う
program1 :: forall m. MonadEffect m => Choreo m Unit
program1 = do
  let mainData = wrap @Main 0
  aData <- comm' (\ur -> pure $ ur mainData + 1) @A -- 1
  bData <- comm' (\ur -> pure $ ur aData + 1) @B -- 2
  cond bData \x ->
    if x == 2 then do
      lastData <- comm' (\ur -> pure $ ur bData + 1) @Main -- 3
      void $ locally \ur -> log $ "Last data: " <> show (ur lastData)
    else do
      void $ locally \ur -> log $ "Unexpected data: " <> show (ur bData)
