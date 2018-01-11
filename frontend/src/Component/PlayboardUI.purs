module Component.PlayboardUI where

import Prelude

import Component.PlayboardUI.BaseUI (_transform)
import Component.PlayboardUI.BaseUI as BaseUI
import Component.PlayboardUI.Element as Element
import Control.Monad.Aff (Aff)
import DOM.Event.MouseEvent (MouseEvent)
import Data.Array as Array
import Data.Const (Const)
import Data.Geometry (Point)
import Data.Lens (Lens, _Just, assign, modifying, use)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just, Nothing), maybe)
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
  [ HP.class_ $ H.ClassName "playboard"
  , HE.onMouseUp $ HE.input MouseUp
  , HE.onMouseMove $ HE.input Drag
  ]
  $ Array.fromFoldable (renderPuzzle <$> state.puzzle)
  <> Array.fromFoldable (renderSelection <$> state.puzzle)

  where
    selection = state.selection

    renderPuzzle puzzle =
      HH.slot' cpBaseUI BaseUI.Slot BaseUI.ui puzzle $ HE.input HandleBaseUI

    renderSelection puzzle =
      SE.svg
      [ SA.viewBox 0.0 0.0 puzzle.size.w puzzle.size.h
      , HA.id_ "selected-layer"
      , HE.onMouseDown $ HE.input MouseDown
      ]
      [
        SE.g
        []
        $ Array.fromFoldable (renderPiece <<< _.piece <$> state.selection)
      ]

    renderPiece { face, transform } =
      SE.g
      [ SA.transform transform
      ]
      [
        Element.renderFace face $ Just "url(#img1)"
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
  let currentPoint = Util.localPoint event
  assign _down true
  assign (_selection <<< _Just <<< _lastPoint) currentPoint
  pure next

eval (MouseUp event next) = do
  assign _down false
  pure next

eval (Drag event next) = do
  { selection, down } <- H.get
  when down $ case selection of
    Just { piece, lastPoint } -> do
      let updater transform = push transform $ Translate dx dy
          currentPoint = Util.localPoint event
          dx = currentPoint.x - lastPoint.x
          dy = currentPoint.y - lastPoint.y
      modifying (_selection <<< _Just <<< _piece <<< _transform) updater
      assign (_selection <<< _Just <<< _lastPoint) currentPoint

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
  assign _down true
  pure next

push :: Array Transform -> Transform -> Array Transform
push ts t@(Translate dx1 dy1) = case Array.unsnoc ts of
  Just { init, last: (Translate dx0 dy0) } ->
    Array.snoc init $ Translate (dx0 + dx1) (dy0 + dy1)
  _ ->
    Array.snoc ts t
push ts t = Array.snoc ts t
