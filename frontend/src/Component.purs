module Component where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Svg.Attributes as SA
import Svg.Elements as SE

data Query a = ToggleState a

type Input = Unit

type Circle =
  { x :: Number
  , y :: Number
  , r :: Number
  }

type Box =
  { x :: Number
  , y :: Number
  , w :: Number
  , h :: Number
  }

type State =
  { circles :: Array Circle
  , on :: Boolean
  , viewBox :: Box
  }

initialState :: Input -> State
initialState _ =
  { circles: [{ x: 0.0, y: 0.0, r: 150.0 / 6.0 }]
  , on: false
  , viewBox: { x, y, w, h }
  }
  where
    h = 150.0
    w = 150.0
    x = -(w / 2.0)
    y = -(h / 2.0)

ui :: forall eff. H.Component HH.HTML Query Input Void eff
ui =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

render :: State -> H.ComponentHTML Query
render state =
  SE.svg
  [ SA.viewBox viewBox.x viewBox.y viewBox.w viewBox.h ]
  $ renderCircle <$> state.circles

  where
    viewBox = state.viewBox

    renderCircle { x, y, r } =
      SE.circle
      [ SA.r r
      , SA.fill $ Just (SA.RGB 0 (if state.on then 100 else 0) 100)
      , HE.onClick (HE.input_ ToggleState)
      ]

eval :: forall eff. Query ~> H.ComponentDSL State Query Void eff
eval (ToggleState next) = do
  on <- H.gets _.on
  H.modify _{ on = not on }
  let circles = [{ x: 0.0, y: 0.0, r: 150.0 / 6.0 }]
  H.modify _{ circles = circles }

  pure next
