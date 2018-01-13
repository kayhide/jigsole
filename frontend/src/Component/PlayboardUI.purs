module Component.PlayboardUI where

import Prelude

import Component.PlayboardUI.BaseUI (Chunk)
import Component.PlayboardUI.BaseUI as BaseUI
import Component.PlayboardUI.SelectionUI as SelectionUI
import Control.Monad.Aff (Aff)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import DOM.Event.MouseEvent (MouseEvent)
import DOM.Event.WheelEvent (WheelEvent)
import Data.Array as Array
import Data.Const (Const)
import Data.Geometry (Face(..), Point)
import Data.Lens (Lens, assign, use)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, isJust, maybe)
import Data.Puzzle (Puzzle)
import Data.Symbol (SProxy(..))
import Ext.Svg.Attributes as ESA
import Ext.Svg.Elements as ESE
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Data.Prism (type (<\/>), type (\/))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HA
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (AJAX)
import Sample as Sample
import Svg.Attributes (Transform)
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
  | HandleSelectionUI SelectionUI.Message a

type Input = Unit

type Message = Void

type State =
  { puzzle :: Maybe Puzzle
  , point :: Point
  , upPoint :: Maybe Point
  , down :: Boolean
  , merging :: Maybe { merger :: Chunk, mergee :: Chunk }

  , transform :: Array Transform
  }

_puzzle :: forall a b r. Lens { puzzle :: a | r } { puzzle :: b | r } a b
_puzzle = prop (SProxy :: SProxy "puzzle")

_point :: forall a b r. Lens { point :: a | r } { point :: b | r } a b
_point = prop (SProxy :: SProxy "point")

_upPoint :: forall a b r. Lens { upPoint :: a | r } { upPoint :: b | r } a b
_upPoint = prop (SProxy :: SProxy "upPoint")

_down :: forall a b r. Lens { down :: a | r } { down :: b | r } a b
_down = prop (SProxy :: SProxy "down")

_merging :: forall a b r. Lens { merging :: a | r } { merging :: b | r } a b
_merging = prop (SProxy :: SProxy "merging")

_transform :: forall a b r. Lens { transform :: a | r } { transform :: b | r } a b
_transform = prop (SProxy :: SProxy "transform")


type ChildQuery
  = BaseUI.Query
    <\/> SelectionUI.Query
    <\/> Const Void

type ChildSlot
  = BaseUI.Slot
    \/ SelectionUI.Slot
    \/ Void

cpBaseUI :: CP.ChildPath BaseUI.Query ChildQuery BaseUI.Slot ChildSlot
cpBaseUI = CP.cp1

cpSelectionUI :: CP.ChildPath SelectionUI.Query ChildQuery SelectionUI.Slot ChildSlot
cpSelectionUI = CP.cp2


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
  , point: { x: 0.0, y: 0.0 }
  , upPoint: Nothing
  , down: false
  , merging: Nothing

  , transform: []
  }

render :: forall eff. State -> H.ParentHTML Query ChildQuery ChildSlot (Eff_ eff)
render state =
  HH.div
  [ HP.class_ $ H.ClassName "playboard fill-image"
  , HE.onMouseDown $ HE.input MouseDown
  , HE.onMouseUp $ HE.input MouseUp
  , HE.onMouseMove $ HE.input Drag
  , HE.onWheel $ HE.input Wheel
  ] $
  [
    renderPattern
  ]
  -- <> Array.fromFoldable (renderPoint state.point <$> state.puzzle)
  -- <> Array.fromFoldable (renderOriginal <$> state.puzzle)
  <> Array.fromFoldable (renderPuzzle <$> state.puzzle)
  <> Array.fromFoldable (renderSelection <$> state.puzzle)

  where
    renderPattern =
      SE.svg
      []
      [
        ESE.defs
        []
        [
          ESE.pattern
          [ HA.id_ "img1"
          , ESA.patternUnits "userSpaceOnUse"
          , SA.width size.w
          , SA.height size.h
          ]
          [
            ESE.image
            [ HA.id_ "image"
            , SA.x 0.0
            , SA.y 0.0
            , SA.width size.w
            , SA.height size.h
            ]
          ]
        ]
      ]
      where
        size =
          { w: fromMaybe 1.0 $ (_.w <<< _.size) <$> state.puzzle
          , h: fromMaybe 1.0 $ (_.h <<< _.size) <$> state.puzzle
          }

    renderPuzzle puzzle =
      HH.slot' cpBaseUI BaseUI.Slot BaseUI.ui puzzle $ HE.input HandleBaseUI

    renderSelection puzzle =
      HH.slot' cpSelectionUI SelectionUI.Slot SelectionUI.ui puzzle $ HE.input HandleSelectionUI

    renderPoint pt { size } =
      SE.svg
      [ SA.viewBox (- size.w * 0.5) (- size.h * 0.5) (size.w * 2.0) (size.h * 2.0)
      , HA.id_ "point-layer"
      ]
      [
        SE.circle [ SA.cx x, SA.cy y, SA.r 20.0 ]
      , SE.rect [ SA.x (pt.x - 30.0), SA.y pt.y, SA.width 60.0, SA.height 2.0 ]
      , SE.rect [ SA.x pt.x, SA.y (pt.y - 30.0), SA.width 2.0, SA.height 60.0 ]
      ]
      where
        { x, y } = Util.inverse pt state.transform

    renderOriginal puzzle@{ size } =
      SE.svg
      [ SA.viewBox (- size.w * 0.5) (- size.h * 0.5) (size.w * 2.0) (size.h * 2.0)
      ]
      [
        SE.rect [ SA.x 0.0, SA.y 0.0, SA.width size.w, SA.height size.h ]
      ]




eval :: forall eff. Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Eff_ eff)
eval (Initialize next) = do
  H.liftEff $ Util.loadImage "image" "samples/IMG_2062.jpg"
  puzzle <- H.liftAff $ Sample.loadPuzzle "puzzle_400x300_6"
  -- puzzle <- H.liftAff $ Sample.loadPuzzle "puzzle_400x300_88"
  -- puzzle <- H.liftAff $ Sample.loadPuzzle "puzzle_400x300_972"
  assign _puzzle $ Just puzzle
  pure next

eval (MouseDown event next) = do
  assign _down true
  pure next

eval (MouseUp event next) = do
  let point = Util.localPoint event
  assign _upPoint $ Just point
  assign _down false

  merging <- map join $ runMaybeT $ runMaybeT $ do
    mergee <-
      MaybeT $ MaybeT $
      H.query' cpSelectionUI SelectionUI.Slot (H.request SelectionUI.AskChunk)
    merger <-
      MaybeT $ MaybeT $
      H.query' cpBaseUI BaseUI.Slot (H.request $ BaseUI.Try mergee point)
    pure { merger, mergee }

  assign _merging merging
  when (isJust merging) $ do
    void $ H.query' cpSelectionUI SelectionUI.Slot $ H.action SelectionUI.Release

  pure next

eval (Drag event next) = do
  let pt1 = Util.localPoint event
  use _down >>= case _ of
    true -> do
      pt0 <- use _point
      let args = Util.diff pt1 pt0
      void $ H.query' cpSelectionUI SelectionUI.Slot $ H.action $ SelectionUI.Move args

    false -> do
      d0 <- (map ((1.5 * _) <<< _.linear_measure)) <$> H.gets _.puzzle
      d1 <- (map $ Util.distance pt1) <$> use _upPoint
      let b = (<) <$> d0 <*> d1
      when (b == Just true) $ do
        void $ H.query' cpSelectionUI SelectionUI.Slot $ H.action SelectionUI.Release
        assign _upPoint Nothing

      pure unit

  chunk <- H.query' cpSelectionUI SelectionUI.Slot (H.request SelectionUI.AskChunk)
  assign _transform $ maybe [] _.transform $ join chunk
  assign _point $ pt1
  pure next

eval (Wheel event next) = do
  center <- H.gets _.point
  let delta = Util.delta event
      args = { delta, center }
  void $ H.query' cpSelectionUI SelectionUI.Slot $ H.action $ SelectionUI.Spin args
  pure next

eval (Release event next) = do
  void $ H.query' cpSelectionUI SelectionUI.Slot $ H.action SelectionUI.Release
  pure next

eval (HandleBaseUI (BaseUI.Picked chunk) next) = do
  void $ H.query' cpSelectionUI SelectionUI.Slot $ H.action $ SelectionUI.Capture chunk
  pure next

eval (HandleSelectionUI (SelectionUI.Released chunk) next) = do
  use _merging >>= case _ of
    Just { merger, mergee } -> do
      let merged =
            { ids: merger.ids <> mergee.ids
            , neighbor_ids: Array.nub $ merger.neighbor_ids <> mergee.neighbor_ids
            , face: Merged $ [ merger.face, mergee.face ]
            , transform: merger.transform
            }
      void $ H.query' cpBaseUI BaseUI.Slot $ H.action $ BaseUI.Remove merger
      void $ H.query' cpBaseUI BaseUI.Slot $ H.action $ BaseUI.Put merged

    Nothing -> do
      void $ H.query' cpBaseUI BaseUI.Slot $ H.action $ BaseUI.Put chunk

  pure next
