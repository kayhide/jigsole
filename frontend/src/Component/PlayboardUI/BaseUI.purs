module Component.PlayboardUI.BaseUI where

import Prelude

import Component.PlayboardUI.Element as Element
import Control.Monad.Aff (Aff)
import Control.MonadZero (guard)
import DOM.Event.MouseEvent (MouseEvent)
import Data.Array as Array
import Data.Function (on)
import Data.Geometry (Face(..), Point)
import Data.Lens (Lens, modifying, use)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Puzzle (Puzzle)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Svg.Attributes (Transform)
import Svg.Attributes as SA
import Svg.Elements as SE
import Util as Util


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


type PieceId = Int

type Chunk =
  { ids :: Array PieceId
  , neighbor_ids :: Array PieceId
  , face :: Face
  , transform :: Array Transform
  }

_transform :: forall a b r. Lens { transform :: a | r } { transform :: b | r } a b
_transform = prop (SProxy :: SProxy "transform")


data Query a
  = Initialize a
  | Pick Chunk MouseEvent a
  | Put Chunk a
  | Remove Chunk a
  | Try Chunk Point (Maybe Chunk -> a)

type Input = Puzzle

data Message
  = Picked Chunk

type State =
  { puzzle :: Puzzle
  , chunks :: Array Chunk
  }

_puzzle :: forall a b r. Lens { puzzle :: a | r } { puzzle :: b | r } a b
_puzzle = prop (SProxy :: SProxy "puzzle")

_chunks :: forall a b r. Lens { chunks :: a | r } { chunks :: b | r } a b
_chunks = prop (SProxy :: SProxy "chunks")


type Eff_ eff = Aff (ajax :: AJAX | eff)

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
  , chunks
  }
  where
    transform = []
    toChunk { id, points, neighbor_ids } =
      { ids: [ id ]
      , neighbor_ids
      , face: Curved { points }
      , transform
      }
    chunks = toChunk <$> puzzle.pieces

render :: State -> H.ComponentHTML Query
render state =
  SE.svg
  [ SA.viewBox (- size.w * 0.5) (- size.h * 0.5) (size.w * 2.0) (size.h * 2.0)
  , HP.id_ "base-layer"
  ]
  [
    SE.g [] $
    renderChunk <$> chunks
  ]

  where
    chunks = state.chunks
    size = state.puzzle.size

    renderChunk chunk@{ face, transform } =
      SE.g
      [
        HE.onMouseDown $ HE.input $ Pick chunk
      , SA.transform transform
      ]
      [
        Element.renderFace face
      ]


eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval (Initialize next) = do
  pure next

eval (Pick chunk event next) = do
  modifying _chunks $ Array.deleteBy ((==) `on` _.ids) chunk
  H.raise $ Picked chunk
  pure next

eval (Put chunk next) = do
  modifying _chunks $ flip Array.snoc chunk
  pure next

eval (Remove chunk next) = do
  modifying _chunks $ Array.deleteBy ((==) `on` _.ids) chunk
  pure next

eval (Try chunk point reply) = do
  x <- _.linear_measure <$> use _puzzle
  let tolerance = { distanceSquared: x * x / 128.0, angle: 7.0 }
  reply <$> findMergeableChunk chunk point tolerance <$> use _chunks


type Tolerance =
  { distanceSquared :: Number
  , angle :: Number
  }

findMergeableChunk :: Chunk -> Point -> Tolerance -> Array Chunk -> Maybe Chunk
findMergeableChunk chunk point tolerance chunks = Array.head $ do
  merger <- chunks
  guard $ not $ Array.null $ Array.intersect merger.ids chunk.neighbor_ids
  let a1 = Util.angle merger.transform
  let pt1 = Util.inverse point merger.transform
  let _ = Util.trace
          $ "{ angle: " <> show (Util.angleDiff a1 a0) <>
          ", distance: " <> show (Util.distanceSquared pt0 pt1) <> " }"
  guard $ Util.angleDiff a1 a0 < tolerance.angle
  guard $ Util.distanceSquared pt0 pt1 < tolerance.distanceSquared

  pure merger
  where
    a0 = Util.angle chunk.transform
    pt0 = Util.inverse point chunk.transform
