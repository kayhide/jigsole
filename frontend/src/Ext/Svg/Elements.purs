module Ext.Svg.Elements where

import Prelude

import Ext.Svg.Indexed as I
import Halogen.HTML.Core (ElemName(..))
import Halogen.HTML.Elements (Node, Leaf)
import Svg.Elements (element)


svg :: forall p i. Node I.SVGsvg p i
svg = element $ ElemName "svg"

clipPath :: forall p i. Node I.SVGclipPath p i
clipPath = element $ ElemName "clipPath"

pattern :: forall p i. Node I.SVGpattern p i
pattern = element $ ElemName "pattern"

image :: forall p i. Leaf I.SVGimage p i
image props = element (ElemName "image") props []

defs :: forall p i. Node I.SVGdefs p i
defs = element $ ElemName "defs"
