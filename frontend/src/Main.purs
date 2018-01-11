module Main where

import Prelude

import Component.PlayboardUI as PlayboardUI
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)

main :: Eff (HA.HalogenEffects (ajax :: AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI PlayboardUI.ui unit body
