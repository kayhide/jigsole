module Ext.Svg.Attributes where

import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Properties (IProp)
import Svg.Attributes (attr)

xmlns :: forall r i. String -> IProp (xmlns :: String | r) i
xmlns = attr (AttrName "xmlns")

xmlns_xlink :: forall r i. String -> IProp (xmlns_xlink :: String | r) i
xmlns_xlink = attr (AttrName "xmlns:xlink")

xlink_href :: forall r i. String -> IProp (xlink_href :: String | r) i
xlink_href = attr (AttrName "xlink:href")

fill :: forall r i. String -> IProp (fill :: String | r) i
fill = attr (AttrName "fill")

patternUnits :: forall r i. String -> IProp (patternUnits :: String | r) i
patternUnits = attr (AttrName "patternUnits")
