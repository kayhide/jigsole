module Data.Geometry where

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
