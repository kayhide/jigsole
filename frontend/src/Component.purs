module Component where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomRange)
import DOM.Event.MouseEvent (MouseEvent)
import Data.Array as Array
import Data.Geometry (Point, Box)
import Data.Lens (Lens, _Just, assign, modifying)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Ext.Svg.Attributes as ESA
import Ext.Svg.Elements as ESE
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HA
import Svg.Attributes as SA
import Svg.Elements as SE
import Util as Util

type PieceId = Int
type PieceMap = Map PieceId Piece

type Piece =
  { id :: PieceId
  , circle :: Circle
  }

_circle :: forall a b r. Lens { circle :: a | r } { circle :: b | r } a b
_circle = prop (SProxy :: SProxy "circle")

data Query a
  = Initialize a
  | Capture PieceId MouseEvent a
  | Drag MouseEvent a
  | Release MouseEvent a

type Input = Unit

type Circle =
  { point :: Point
  , r :: Number
  }

_point :: forall a b r. Lens { point :: a | r } { point :: b | r } a b
_point = prop (SProxy :: SProxy "point")

type Selection =
  { pieceId :: PieceId
  , lastPoint :: Point
  }

_pieceId :: forall a b r. Lens { pieceId :: a | r } { pieceId :: b | r } a b
_pieceId = prop (SProxy :: SProxy "pieceId")

_lastPoint :: forall a b r. Lens { lastPoint :: a | r } { lastPoint :: b | r } a b
_lastPoint = prop (SProxy :: SProxy "lastPoint")

type State =
  { pieces :: PieceMap
  , viewBox :: Box
  , selection :: Maybe Selection
  }

_pieces :: forall a b r. Lens { pieces :: a | r } { pieces :: b | r } a b
_pieces = prop (SProxy :: SProxy "pieces")

_viewBox :: forall a b r. Lens { viewBox :: a | r } { viewBox :: b | r } a b
_viewBox = prop (SProxy :: SProxy "viewBox")

_selection :: forall a b r. Lens { selection :: a | r } { selection :: b | r } a b
_selection = prop (SProxy :: SProxy "selection")


type Eff_ eff = Aff (random :: RANDOM | eff)

ui :: forall eff. H.Component HH.HTML Query Input Void (Eff_ eff)
ui =
  H.lifecycleComponent
    { initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

initialState :: Input -> State
initialState _ =
  { pieces: Map.empty
  , viewBox: { point, size }
  , selection: Nothing
  }
  where
    size = { h: 600.0, w: 400.0 }
    point = { x: 0.0, y: 0.0 }

render :: State -> H.ComponentHTML Query
render state =
  SE.svg
  [ SA.viewBox viewBox.point.x viewBox.point.y viewBox.size.w viewBox.size.h
  , HE.onMouseMove $ HE.input Drag
  , HE.onMouseUp $ HE.input Release
  ]
  [
    ESE.defs [] [ renderPattern ]
  , SE.g
    []
    $ Array.fromFoldable $ renderPiece <$> state.pieces
  ]

  where
    viewBox = state.viewBox
    selection = state.selection

    renderPattern =
      ESE.pattern
      [ HA.id_ "img1"
      , ESA.patternUnits "userSpaceOnUse"
      , SA.width 400.0
      , SA.height 300.0
      ]
      [
        ESE.image
        [ HA.id_ "image"
        , SA.x 0.0
        , SA.y 0.0
        , SA.width 400.0
        , SA.height 300.0
        ]
      ]

    renderPiece { id, circle: { point, r } } =
      SE.circle
      [ SA.cx point.x
      , SA.cy point.y
      , SA.r r
      , ESA.fill "url(#img1)"
      , HE.onMouseDown $ HE.input $ Capture id
      ]

    color = case _ of
      true -> SA.RGB 0 100 100
      false -> SA.RGB 0 0 100

eval :: forall eff. Query ~> H.ComponentDSL State Query Void (Eff_ eff)
eval (Initialize next) = do
  viewBox <- H.gets _.viewBox
  circles <- H.liftEff $ replicateA 1000 $ randomCircle viewBox
  let pieces = Map.fromFoldable $ Array.mapWithIndex (\id circle -> Tuple id { id, circle }) circles
  assign _pieces pieces
  H.liftEff $ Util.loadImage "image" "samples/IMG_2062.jpg"
  pure next

eval (Capture pieceId event next) = do
  let lastPoint = Util.localPoint event
  assign _selection $ Just { pieceId, lastPoint }
  pure next

eval (Drag event next) = do
  { selection } <- H.get
  case selection of
    Just { pieceId, lastPoint } -> do
      let updater circle@{ point } = circle { point = { x: point.x + dx, y: point.y + dy } }
          currentPoint = Util.localPoint event
          dx = currentPoint.x - lastPoint.x
          dy = currentPoint.y - lastPoint.y
      modifying (_pieces <<< ix pieceId <<< _circle) updater
      assign (_selection <<< _Just <<< _lastPoint) currentPoint

    Nothing -> pure unit
  pure next

eval (Release event next) = do
  assign _selection Nothing
  pure next


randomCircle :: forall eff. Box -> Eff (random :: RANDOM | eff) Circle
randomCircle box = do
  point <- randomPoint box
  r <- ((min box.size.w box.size.h) * _) <$> randomRange 0.01 0.02
  pure { point, r }

randomPoint :: forall eff. Box -> Eff (random :: RANDOM | eff) Point
randomPoint { point, size } = do
  x <- randomRange point.x $ point.x + size.w
  y <- randomRange point.y $ point.y + size.h
  pure { x, y }
