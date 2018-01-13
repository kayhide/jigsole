module Component.PlayboardUI.Element where

import Prelude

import Data.Array as Array
import Data.Geometry (Face(..))
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Svg.Attributes (Command(Z, C, L, M), D(Abs))
import Svg.Attributes as SA
import Svg.Elements as SE

renderFace :: forall p i. Face -> HH.HTML p i
renderFace (Circle { point, r }) =
  SE.circle
  $ [ SA.cx point.x
    , SA.cy point.y
    , SA.r r
    ]

renderFace (Curved { points }) =
  SE.path
  $ [ SA.d ds
    ]
  where
    toM (Just { x, y }) = M x y
    toM _ = Z
    toC [ Nothing, Nothing, Just { x, y } ] = L x y
    toC [ Just pt1, Just pt2, Just pt3 ] = C pt1.x pt1.y pt2.x pt2.y pt3.x pt3.y
    toC _ = Z
    ds =
      Array.concat
      [ (Abs <<< toM) <$> Array.take 1 points
      , (Abs <<< toC) <$> eachSlice 3 (Array.drop 1 points)
      , [ Abs Z ]
      ]

    eachSlice i [] = []
    eachSlice i xs = [ Array.take i xs ] <> eachSlice i (Array.drop i xs)

renderFace (Merged faces) =
  SE.g [] $ renderFace <$> faces
