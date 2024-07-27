module Test.WorkerBMain where

import Prelude

import Choreography.Choreo.Worker.WorkerThread (runChoreoWorker)
import Choreography.Location (toLocTm)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Program (B, program1)

main :: Effect Unit
main = launchAff_ $ runChoreoWorker (toLocTm @B) program1
