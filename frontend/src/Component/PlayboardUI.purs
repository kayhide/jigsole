module Component.PlayboardUI where

import Prelude

import Component.PlayboardUI.BaseUI (_transform)
import Component.PlayboardUI.BaseUI as BaseUI
import Component.PlayboardUI.Element as Element
import Control.Monad.Aff (Aff)
import Control.MonadZero (guard)
import DOM.Event.MouseEvent (MouseEvent)
import DOM.Event.WheelEvent (WheelEvent)
import Data.Array as Array
import Data.Const (Const)
import Data.Geometry (Point)
import Data.Lens (Lens, _Just, assign, modifying, use)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Puzzle (Puzzle)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Data.Prism (type (<\/>), type (\/))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HA
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Sample as Sample
import Svg.Attributes (Transform(..))
import Svg.Attributes as SA
import Svg.Elements as SE
import Util as Util


data Query a
  = Initialize a
  | MouseDown MouseEvent a
  | MouseUp MouseEvent a
  | Drag MouseEvent a
  | Release MouseEvent a
  | Wheel WheelEvent a
  | HandleBaseUI BaseUI.Message a

type Input = Unit

type Message = Void

type State =
  { puzzle :: Maybe Puzzle
  , selection :: Maybe Selection
  , down :: Boolean
  }

_puzzle :: forall a b r. Lens { puzzle :: a | r } { puzzle :: b | r } a b
_puzzle = prop (SProxy :: SProxy "puzzle")

_selection :: forall a b r. Lens { selection :: a | r } { selection :: b | r } a b
_selection = prop (SProxy :: SProxy "selection")

_down :: forall a b r. Lens { down :: a | r } { down :: b | r } a b
_down = prop (SProxy :: SProxy "down")

type Selection =
  { piece :: BaseUI.Piece
  , lastPoint :: Point
  }

_piece :: forall a b r. Lens { piece :: a | r } { piece :: b | r } a b
_piece = prop (SProxy :: SProxy "piece")

_lastPoint :: forall a b r. Lens { lastPoint :: a | r } { lastPoint :: b | r } a b
_lastPoint = prop (SProxy :: SProxy "lastPoint")


type ChildQuery
  = BaseUI.Query
    <\/> Const Void

type ChildSlot
  = BaseUI.Slot
    \/ Void

cpBaseUI :: CP.ChildPath BaseUI.Query ChildQuery BaseUI.Slot ChildSlot
cpBaseUI = CP.cp1

type Eff_ eff = Aff (ajax :: AJAX | eff)

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
ui =
  H.lifecycleParentComponent
    { initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }

initialState :: Input -> State
initialState _ =
  { puzzle: Nothing
  , selection: Nothing
  , down: false
  }

render :: forall eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Eff_ eff)
render state =
  HH.div
  [ HP.class_ $ H.ClassName "playboard fill-image"
  , HE.onMouseDown $ HE.input MouseDown
  , HE.onMouseUp $ HE.input MouseUp
  , HE.onMouseMove $ HE.input Drag
  , HE.onWheel $ HE.input Wheel
  ]
  $ Array.fromFoldable (renderPuzzle <$> state.puzzle)
  <> Array.fromFoldable (renderSelection <$> state.puzzle)

  where
    renderPuzzle puzzle =
      HH.slot' cpBaseUI BaseUI.Slot BaseUI.ui puzzle $ HE.input HandleBaseUI

    renderSelection puzzle =
      SE.svg
      [ SA.viewBox (- size.w * 0.5) (- size.h * 0.5) (size.w * 2.0) (size.h * 2.0)
      , HA.id_ "selected-layer"
      , HE.onMouseDown $ HE.input MouseDown
      ]
      [
        SE.g
        []
        $ Array.fromFoldable (renderPiece <<< _.piece <$> state.selection)
      ]
      where
        size = puzzle.size

    renderPiece { face, transform } =
      SE.g
      [ SA.transform transform
      ]
      [
        Element.renderFace face
      ]

eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff)
eval (Initialize next) = do
  -- puzzle <- H.liftAff $ Sample.loadPuzzle "puzzle_400x300_6"
  puzzle <- H.liftAff $ Sample.loadPuzzle "puzzle_400x300_88"
  -- puzzle <- H.liftAff $ Sample.loadPuzzle "puzzle_400x300_972"
  assign _puzzle $ Just puzzle
  H.liftEff $ Util.loadImage "image" "samples/IMG_2062.jpg"
  pure next

eval (MouseDown event next) = do
  assign _down true
  assign (_selection <<< _Just <<< _lastPoint) $ Util.localPoint event
  pure next

eval (MouseUp event next) = do
  assign _down false
  pure next

eval (Drag event next) = do
  let currentPoint = Util.localPoint event
  { selection, down } <- H.get
  when down $ case selection of
    Just { piece, lastPoint } -> do
      let updater transform = push transform $ SA.Translate dx dy
          dx = currentPoint.x - lastPoint.x
          dy = currentPoint.y - lastPoint.y
      modifying (_selection <<< _Just <<< _piece <<< _transform) updater
      assign (_selection <<< _Just <<< _lastPoint) $ currentPoint
    Nothing -> pure unit
  pure next

eval (Wheel event next) = do
  H.gets _.selection >>= case _ of
    Just { piece, lastPoint } -> do
      let updater transform = push transform $ SA.Rotate delta lastPoint.x lastPoint.y
          delta = Util.delta event
      modifying (_selection <<< _Just <<< _piece <<< _transform) updater
    Nothing -> pure unit
  pure next

eval (Release event next) = do
  use _selection >>=
    maybe (pure unit)
    (void <<< H.query' cpBaseUI BaseUI.Slot <<< H.action <<< BaseUI.Push <<< _.piece)
  assign _selection Nothing
  pure next

eval (HandleBaseUI (BaseUI.Picked piece lastPoint) next) = do
  use _selection >>=
    maybe (pure unit)
    (void <<< H.query' cpBaseUI BaseUI.Slot <<< H.action <<< BaseUI.Push <<< _.piece)
  assign _selection $ Just { piece, lastPoint }
  pure next

push :: Array Transform -> Transform -> Array Transform
push ts t1 = fromMaybe (Array.cons t1 ts) do
  { head: t0, tail } <- Array.uncons ts
  case { t0, t1 } of
    { t0: Translate dx0 dy0, t1: Translate dx1 dy1 } ->
      pure $ Array.cons (Translate (dx0 + dx1) (dy0 + dy1)) tail

    { t0: Rotate da0 x0 y0, t1: Rotate da1 x1 y1 } -> do
      guard $ x0 == x1 && y0 == y1
      pure $ Array.cons (Rotate (da0 + da1) x0 y0) tail

    _ ->
      Nothing
