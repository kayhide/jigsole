module Data.Geometry where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.Record.ShowRecord (showRecord)

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

derive instance genericFace :: Generic Face _
instance showFace :: Show Face where
  show (Circle r) = "Circle"
  show (Curved _) = "Curved"
  show (Merged fs) = "Merged: [" <> Array.intercalate ", " (show <$> fs) <> "]"
