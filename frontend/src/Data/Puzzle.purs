module Data.Puzzle where

import Prelude

import Data.Geometry (Size, Point)
import Data.Maybe (Maybe)

type ControlPoint = Maybe Point

type Piece =
  { id :: Int
  , points :: Array ControlPoint
  }

type Puzzle =
  { count :: Int
  , size :: Size
  , linear_measure :: Number
  , pieces :: Array Piece
  }
