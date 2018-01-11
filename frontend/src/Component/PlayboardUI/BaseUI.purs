module Component.PlayboardUI.BaseUI where

import Prelude

import Component.PlayboardUI.Element as Element
import Control.Monad.Aff (Aff)
import DOM.Event.MouseEvent (MouseEvent)
import Data.Array as Array
import Data.Geometry (Face(Curved), Point)
import Data.Lens (Lens, modifying)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Puzzle (Puzzle)
import Data.Symbol (SProxy(..))
import Ext.Svg.Attributes as ESA
import Ext.Svg.Elements as ESE
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HA
import Network.HTTP.Affjax (AJAX)
import Svg.Attributes (Transform)
import Svg.Attributes as SA
import Svg.Elements as SE
import Util as Util


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


type PieceId = Int

type Piece =
  { id :: PieceId
  , face :: Face
  , transform :: Array Transform
  }

_transform :: forall a b r. Lens { transform :: a | r } { transform :: b | r } a b
_transform = prop (SProxy :: SProxy "transform")


data Query a
  = Initialize a
  | Pick Piece MouseEvent a
  | Push Piece a

type Input = Puzzle

data Message
  = Picked Piece Point

type State =
  { puzzle :: Puzzle
  , pieces :: Array Piece
  }

_pieces :: forall a b r. Lens { pieces :: a | r } { pieces :: b | r } a b
_pieces = prop (SProxy :: SProxy "pieces")


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
  , pieces
  }
  where
    transform = []
    toPiece { id, points } = { id, face: Curved { points }, transform }
    pieces = toPiece <$> puzzle.pieces

render :: State -> H.ComponentHTML Query
render state =
  SE.svg
  [ SA.viewBox 0.0 0.0 puzzleSize.w puzzleSize.h
  ]
  [
    ESE.defs [] [ renderPattern ]
  , SE.g
    []
    $ renderPiece <$> pieces
  ]

  where
    pieces = state.pieces
    puzzleSize = state.puzzle.size

    renderPattern =
      ESE.pattern
      [ HA.id_ "img1"
      , ESA.patternUnits "userSpaceOnUse"
      , SA.width puzzleSize.w
      , SA.height puzzleSize.h
      ]
      [
        ESE.image
        [ HA.id_ "image"
        , SA.x 0.0
        , SA.y 0.0
        , SA.width puzzleSize.w
        , SA.height puzzleSize.h
        ]
      ]

    renderPiece piece@{ face, transform } =
      SE.g
      [
        HE.onMouseDown $ HE.input $ Pick piece
      , SA.transform transform
      ]
      [
        Element.renderFace face $ Just "url(#img1)"
      ]


eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Eff_ eff)
eval (Initialize next) = do
  pure next

eval (Pick piece event next) = do
  let point = Util.localPoint event
  H.raise $ Picked piece point
  modifying _pieces $ Array.filter (notEq piece.id <<< _.id)
  pure next

eval (Push piece next) = do
  modifying _pieces $ flip Array.snoc piece
  pure next
