module Component where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomRange)
import Data.Lens (Lens, assign, modifying)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Unfoldable (replicateA)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Svg.Attributes as SA
import Svg.Elements as SE

data Query a = ToggleState a

type Input = Unit

type Point =
  { x :: Number
  , y :: Number
  }

type Size =
  { w :: Number
  , h :: Number
  }

type Box =
  { point :: Point
  , size :: Size
  }

type Circle =
  { point :: Point
  , r :: Number
  }

type State =
  { circles :: Array Circle
  , viewBox :: Box
  , on :: Boolean
  }

_circles :: forall a b r. Lens { circles :: a | r } { circles :: b | r } a b
_circles = prop (SProxy :: SProxy "circles")

_viewBox :: forall a b r. Lens { viewBox :: a | r } { viewBox :: b | r } a b
_viewBox = prop (SProxy :: SProxy "viewBox")

_on :: forall a b r. Lens { on :: a | r } { on :: b | r } a b
_on = prop (SProxy :: SProxy "on")


type Eff_ eff = Aff (random :: RANDOM | eff)

ui :: forall eff. H.Component HH.HTML Query Input Void (Eff_ eff)
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: Input -> State
initialState _ =
  { circles: [{ point: { x: 0.0, y: 0.0 }, r: 150.0 / 6.0 }]
  , on: false
  , viewBox: { point, size }
  }
  where
    size = { h: 150.0, w: 150.0 }
    point = { x: -(size.w / 2.0), y: -(size.h / 2.0) }

render :: State -> H.ComponentHTML Query
render state =
  SE.svg
  [ SA.viewBox viewBox.point.x viewBox.point.y viewBox.size.w viewBox.size.h ]
  $ renderCircle <$> state.circles

  where
    viewBox = state.viewBox

    renderCircle { point, r } =
      SE.circle
      [ SA.cx point.x
      , SA.cy point.y
      , SA.r r
      , SA.fill $ Just (SA.RGB 0 (if state.on then 100 else 0) 100)
      , HE.onClick (HE.input_ ToggleState)
      ]

eval :: forall eff. Query ~> H.ComponentDSL State Query Void (Eff_ eff)
eval (ToggleState next) = do
  modifying _on not
  viewBox <- H.gets _.viewBox
  circles <- H.liftEff $ replicateA 1000 $ randomCircle viewBox
  assign _circles circles

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
