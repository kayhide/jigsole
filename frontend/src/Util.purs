module Util where

import Prelude

import Control.Monad.Eff (Eff)
import DOM.Event.MouseEvent (MouseEvent)
import DOM.Event.WheelEvent (WheelEvent)
import DOM.Event.WheelEvent as WheelEvent
import Data.Geometry (Point)
import Math as Math

foreign import localPoint :: MouseEvent -> Point

foreign import loadImage :: forall eff. String -> String -> Eff eff Unit

delta :: WheelEvent -> Number
delta = WheelEvent.deltaX - WheelEvent.deltaY

type PointDiff =
  { dx :: Number
  , dy :: Number
  }

diff :: Point -> Point -> PointDiff
diff pt1 pt0 =
  { dx: pt1.x - pt0.x
  , dy: pt1.y - pt0.y
  }

distance :: Point -> Point -> Number
distance pt1 pt0 = Math.sqrt $ (dx * dx) + (dy * dy)
  where
    { dx, dy } = diff pt1 pt0
