module Sample where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (fromRight)
import Data.Foreign (F, Foreign, readArray, readNullOrUndefined)
import Data.Foreign.Class (decode)
import Data.Foreign.Index ((!))
import Data.Puzzle (ControlPoint, Puzzle, Piece)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)

foreign import _puzzle_400_300_6 :: Foreign

puzzle_400_300_6 :: Puzzle
puzzle_400_300_6 = unsafePartial $ fromRight puzzle_
  where
    puzzle_ = runExcept $ readPuzzle _puzzle_400_300_6

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
  pure { id
       , points
       }

readControlPoint :: Foreign -> F ControlPoint
readControlPoint value =
  traverse readPoint =<< readNullOrUndefined value
  where
    readPoint v = do
      x <- decode =<< v ! 0
      y <- decode =<< v ! 1
      pure { x, y }
