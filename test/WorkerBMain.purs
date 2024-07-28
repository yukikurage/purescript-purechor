module Test.WorkerBMain where

import Prelude

import Choreography (runChoreography)
import Choreography.Network.Worker.WorkerThread (WorkerConfig(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Config (config)
import Test.Program (B, program1)

main :: Effect Unit
main = launchAff_ $ runChoreography @B (WorkerConfig config) program1
