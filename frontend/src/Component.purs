module Component where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomRange)
import DOM.Event.MouseEvent (MouseEvent)
import Data.Array as Array
import Data.Geometry (Point, Box)
import Data.Int (toNumber)
import Data.Lens (Lens, _Just, assign, modifying)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Ext.Svg.Attributes as ESA
import Ext.Svg.Elements as ESE
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HA
import Halogen.HTML.Properties as HP
import Math as Math
import Svg.Attributes (Transform(..))
import Svg.Attributes as SA
import Svg.Elements as SE
import Util as Util

type PieceId = Int
type PieceMap = Map PieceId Piece

type Piece =
  { id :: PieceId
  , circle :: Circle
  , transform :: Array Transform
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
  , down :: Boolean
  }

_pieces :: forall a b r. Lens { pieces :: a | r } { pieces :: b | r } a b
_pieces = prop (SProxy :: SProxy "pieces")

_viewBox :: forall a b r. Lens { viewBox :: a | r } { viewBox :: b | r } a b
_viewBox = prop (SProxy :: SProxy "viewBox")

_selection :: forall a b r. Lens { selection :: a | r } { selection :: b | r } a b
_selection = prop (SProxy :: SProxy "selection")

count :: Int
count = 200

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
  , down: false
  }
  where
    size = { w: 400.0, h: 300.0 }
    point = { x: 0.0, y: 0.0 }

render :: State -> H.ComponentHTML Query
render state =
  HH.div
  [ HP.class_ $ H.ClassName "playboard"
  , HE.onMouseMove $ HE.input Drag
  , HE.onMouseUp $ HE.input Release
  ]
  [
    SE.svg
    [ SA.viewBox viewBox.point.x viewBox.point.y viewBox.size.w viewBox.size.h
    ]
    [
      ESE.defs [] [ renderPattern ]
    , SE.g
      []
      $ Array.fromFoldable $ renderPiece <$> pieces
    ]
  , SE.svg
    [ SA.viewBox viewBox.point.x viewBox.point.y viewBox.size.w viewBox.size.h
    , HA.id_ "selected-layer"
    ]
    [
      SE.g
      []
      $ Array.fromFoldable $ renderPiece <$> (flip Map.lookup pieces =<< _.pieceId <$> selection)
    ]
  ]

  where
    viewBox = state.viewBox
    pieces = state.pieces
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

    renderPiece { id, circle: { point, r }, transform } =
      SE.g
      [
       HE.onMouseDown $ HE.input $ Capture id
      , SA.transform transform
      ]
      [
        SE.circle
        [ SA.cx point.x
        , SA.cy point.y
        , SA.r r
        , ESA.fill "url(#img1)"
        ]
      ]

eval :: forall eff. Query ~> H.ComponentDSL State Query Void (Eff_ eff)
eval (Initialize next) = do
  viewBox <- H.gets _.viewBox
  circles <- H.liftEff $ replicateA count $ randomCircle viewBox
  let pieces = Map.fromFoldable $ Array.mapWithIndex (\id circle -> Tuple id { id, circle, transform }) circles
      transform = []
  assign _pieces pieces
  H.liftEff $ Util.loadImage "image" "samples/IMG_2062.jpg"
  pure next

eval (Capture pieceId event next) = do
  let lastPoint = Util.localPoint event
  assign _selection $ Just { pieceId, lastPoint }
  H.modify _{ down = true }
  pure next

eval (Drag event next) = do
  { selection, down } <- H.get
  when down $ case selection of
    Just { pieceId, lastPoint } -> do
      let updater piece@{ transform } = piece { transform = push transform $ Translate dx dy }
          currentPoint = Util.localPoint event
          dx = currentPoint.x - lastPoint.x
          dy = currentPoint.y - lastPoint.y
      modifying (_pieces <<< ix pieceId) updater
      assign (_selection <<< _Just <<< _lastPoint) currentPoint

    Nothing -> pure unit
  pure next

eval (Release event next) = do
  { down } <- H.get
  H.modify _{ down = false }
  pure next


push :: Array Transform -> Transform -> Array Transform
push ts t@(Translate dx1 dy1) = case Array.unsnoc ts of
  Just { init, last: (Translate dx0 dy0) } -> Array.snoc init $ Translate (dx0 + dx1) (dy0 + dy1)
  _ -> Array.snoc ts t
push ts t = Array.snoc ts t

randomCircle :: forall eff. Box -> Eff (random :: RANDOM | eff) Circle
randomCircle box = do
  point <- randomPoint box
  let r0 = 0.4 / Math.sqrt (toNumber count)
  r <- ((min box.size.w box.size.h) * _) <$> randomRange r0 (2.0 * r0)
  pure { point, r }

randomPoint :: forall eff. Box -> Eff (random :: RANDOM | eff) Point
randomPoint { point, size } = do
  x <- randomRange point.x $ point.x + size.w
  y <- randomRange point.y $ point.y + size.h
  pure { x, y }
