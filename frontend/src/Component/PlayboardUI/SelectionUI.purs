module Component.PlayboardUI.SelectionUI where

import Prelude

import Component.PlayboardUI.BaseUI (Piece, _transform)
import Component.PlayboardUI.Element as Element
import Control.Monad.Aff (Aff)
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Geometry (Point)
import Data.Lens (Lens, assign, modifying, traversed, use)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Puzzle (Puzzle)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HA
import Svg.Attributes (Transform(..))
import Svg.Attributes as SA
import Svg.Elements as SE


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type MoveArgs =
  { dx :: Number
  , dy :: Number
  }

type SpinArgs =
  { delta :: Number
  , center :: Point
  }

data Query a
  = Initialize a
  | Capture Piece a
  | Release a
  | Move MoveArgs a
  | Spin SpinArgs a

type Input = Puzzle

data Message
  = Released Piece

type State =
  { puzzle :: Puzzle
  , pieces :: Array Piece
  }

_puzzle :: forall a b r. Lens { puzzle :: a | r } { puzzle :: b | r } a b
_puzzle = prop (SProxy :: SProxy "puzzle")

_pieces :: forall a b r. Lens { pieces :: a | r } { pieces :: b | r } a b
_pieces = prop (SProxy :: SProxy "pieces")

_down :: forall a b r. Lens { down :: a | r } { down :: b | r } a b
_down = prop (SProxy :: SProxy "down")

_lastPoint :: forall a b r. Lens { lastPoint :: a | r } { lastPoint :: b | r } a b
_lastPoint = prop (SProxy :: SProxy "lastPoint")


type Eff_ eff = Aff eff

ui :: forall eff. H.Component HH.HTML Query Input Message (Eff_ eff)
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
initialState puzzle =
  { puzzle
  , pieces: []
  }

render :: State -> H.ComponentHTML Query
render state =
  SE.svg
  [ SA.viewBox (- size.w * 0.5) (- size.h * 0.5) (size.w * 2.0) (size.h * 2.0)
  , HA.id_ "selection-layer"
  ]
  [
    SE.g
    []
    $ (renderPiece <$> state.pieces)
  ]

  where
    size = state.puzzle.size

    renderPiece { face, transform } =
      SE.g
      [ SA.transform transform
      ]
      [
        Element.renderFace face
      ]

eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval (Initialize next) = do
  pure next

eval (Capture piece next) = do
  void $ use _pieces >>= (traverse (H.raise <<< Released))
  assign _pieces [ piece ]
  pure next

eval (Release next) = do
  void $ use _pieces >>= (traverse (H.raise <<< Released))
  assign _pieces []
  pure next

eval (Move { dx, dy } next) = do
  let updater transform = push transform $ SA.Translate dx dy
  modifying (_pieces <<< traversed <<< _transform) updater
  pure next

eval (Spin { delta, center } next) = do
  let updater transform = push transform $ SA.Rotate delta center.x center.y
  modifying (_pieces <<< traversed <<< _transform) updater
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
