module Ext.Svg.Indexed where

import DOM.HTML.Indexed (Interactive)

type SVGsvg = Interactive
  ( xmlns :: String
  , xmlns_xlink :: String
  , viewBox :: String
  , width :: Number
  , height :: Number
  , transform :: String
  )

type SVGclipPath = Interactive
  ( clipPathUnits :: String
  , transform :: String
  )

type SVGpattern = Interactive
  ( patternUnits :: String
  , patternContentUnits :: String
  , patternTransform :: String
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , xlink_href :: String
  , preserveAspectRatio :: String
  )

type SVGimage = Interactive
  ( x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , xlink_href :: String
  , preserveAspectRatio :: String
  )

type SVGdefs = Interactive
  ( transform :: String
  )
