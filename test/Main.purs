module Test.Main where

import Prelude

import Choreography.Choreo.Worker.MainThread (runChoreoMain)
import Choreography.Location (toLocTm)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Program (A, B, Main, program1)

main :: Effect Unit
main = launchAff_ $ runChoreoMain (toLocTm @Main)
  ( Map.fromFoldable
      [ toLocTm @A /\ "workerA.js", toLocTm @B /\ "workerB.js" ]
  )
  program1
