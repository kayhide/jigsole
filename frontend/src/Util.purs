module Util where

import Prelude

import Control.Monad.Eff (Eff)
import DOM.Event.MouseEvent (MouseEvent)
import DOM.Event.WheelEvent (WheelEvent)
import DOM.Event.WheelEvent as WheelEvent
import Data.Geometry (Point)

foreign import localPoint :: MouseEvent -> Point

foreign import loadImage :: forall eff. String -> String -> Eff eff Unit

delta :: WheelEvent -> Number
delta = WheelEvent.deltaX - WheelEvent.deltaY
