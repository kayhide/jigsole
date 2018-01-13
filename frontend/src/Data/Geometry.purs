module Data.Geometry where

import Data.Maybe (Maybe)

type Point =
  { x :: Number
  , y :: Number
  }

type Size =
  { w :: Number
  , h :: Number
  }

type Box =
  { point :: Point
  , size :: Size
  }


type CircleRec =
  { point :: Point
  , r :: Number
  }


type ControlPoint = Maybe Point

type CurvedRec =
  { points :: Array ControlPoint
  }

data Face
  = Circle CircleRec
  | Curved CurvedRec
  | Merged (Array Face)
