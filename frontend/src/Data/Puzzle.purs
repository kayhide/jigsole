module Data.Puzzle where

import Prelude

import Data.Foreign (F, Foreign, readArray, readNullOrUndefined)
import Data.Foreign.Class (decode)
import Data.Foreign.Index ((!))
import Data.Geometry (Size, Point)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)

type ControlPoint = Maybe Point

type Piece =
  { id :: Int
  , points :: Array ControlPoint
  , neighbor_ids :: Array Int
  }

type Puzzle =
  { count :: Int
  , size :: Size
  , linear_measure :: Number
  , pieces :: Array Piece
  }


readPuzzle :: Foreign -> F Puzzle
readPuzzle value = do
  count <- decode =<< value ! "count"
  width <- decode =<< value ! "width"
  height <- decode =<< value ! "height"
  linear_measure <- decode =<< value ! "linear_measure"
  pieces <- traverse readPiece =<< readArray =<< value ! "pieces"
  pure { count
       , size: { w: width, h: height }
       , linear_measure
       , pieces
       }

readPiece :: Foreign -> F Piece
readPiece value = do
  id <- decode =<< value ! "id"
  points <- traverse readControlPoint =<< readArray =<< value ! "points"
  neighbor_ids <- traverse decode =<< readArray =<< value ! "neighbor_ids"
  pure { id, points, neighbor_ids }

readControlPoint :: Foreign -> F ControlPoint
readControlPoint value =
  traverse readPoint =<< readNullOrUndefined value
  where
    readPoint v = do
      x <- decode =<< v ! 0
      y <- decode =<< v ! 1
      pure { x, y }
