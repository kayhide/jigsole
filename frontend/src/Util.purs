module Util where

import Prelude

import Control.Monad.Eff (Eff)
import DOM.Event.MouseEvent (MouseEvent)
import DOM.Event.WheelEvent (WheelEvent)
import DOM.Event.WheelEvent as WheelEvent
import Data.Array as Array
import Data.Geometry (Point)
import Math as Math
import Svg.Attributes (Transform(..))

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
distance pt1 pt0 = Math.sqrt $ distanceSquared pt1 pt0

distanceSquared :: Point -> Point -> Number
distanceSquared pt1 pt0 = (dx * dx) + (dy * dy)
  where
    { dx, dy } = diff pt1 pt0


translate :: Number -> Number -> Point -> Point
translate dx dy pt = { x: pt.x + dx, y: pt.y + dy }

rotate :: Number -> Point -> Point
rotate a { x, y } = { x: x * Math.cos theta - y * Math.sin theta
                    , y: x * Math.sin theta + y * Math.cos theta
                    }
  where
    theta = a * 2.0 * Math.pi / 360.0

apply :: Point -> Array Transform -> Point
apply = Array.foldr apply'
  where
    apply' t pt = case t of
      Translate dx dy -> translate dx dy pt
      Rotate a x y -> pt # translate (-x) (-y) >>> rotate a >>> translate x y
      _ -> pt

inverse :: Point -> Array Transform -> Point
inverse = Array.foldl inverse'
  where
    inverse' pt t = case t of
      Translate dx dy -> translate (-dx) (-dy) pt
      Rotate a x y -> pt # translate (-x) (-y) >>> rotate (-a) >>> translate x y
      _ -> pt

angle :: Array Transform -> Number
angle = regularize <<< Array.foldl angle' 0.0
  where
    angle' a t = case t of
      Rotate a' _ _ -> a + a'
      _ -> a

    regularize x = x - 360.0 * Math.floor (x / 360.0)
