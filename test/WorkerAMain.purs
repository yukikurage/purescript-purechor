module Test.WorkerAMain where

import Prelude

import Choreography (runChoreography)
import Choreography.Network.Worker.WorkerThread (WorkerConfig(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Config (config)
import Test.Program (A, program1)

main :: Effect Unit
main = launchAff_ $ runChoreography @A (WorkerConfig config) program1
