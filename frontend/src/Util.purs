module Util where

import Prelude

import Control.Monad.Eff (Eff)
import DOM.Event.MouseEvent (MouseEvent)
import Data.Geometry (Point)

foreign import localPoint :: MouseEvent -> Point

foreign import loadImage :: forall eff. String -> String -> Eff eff Unit
