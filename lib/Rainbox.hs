module Rainbox
  ( -- * Alignment
    Align
  , Vert
  , Horiz
  , center
  , left
  , right
  , top
  , bottom
  , Alignment
    ( BuiltBox
    , Opposite
    , convert
    , wrap
    , spreader
    , spacer )

  -- * Boxes
  , Roddable
  , BoxH
  , BoxV

  -- * Box and block construction
  , fromChunk
  , blank

  -- * Rendering
  , render

  -- * Tables
  , Cell(..)
  , tableByRows
  , tableByColumns

  -- * Utilities
  , intersperse
  ) where

import Rainbox.Core
