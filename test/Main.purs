module Test.Main where

import Prelude

import Choreography (runChoreography)
import Choreography.Network.Worker.MainThread (MainConfig(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Config (config)
import Test.Program (Main, program1)

main :: Effect Unit
main = launchAff_ $ runChoreography @Main (MainConfig config) program1
