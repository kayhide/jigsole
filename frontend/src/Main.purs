module Main where

import Prelude

import Component (ui)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Eff (HA.HalogenEffects (random :: RANDOM)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
