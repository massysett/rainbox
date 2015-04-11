module Rainbox
  ( -- * Alignment and Boxes
    Alignment
  , Horizontal
  , Vertical
  , center
  , left
  , right
  , top
  , bottom
  , Box
  , Orientation ( spacer, spreader )

  -- * Box construction
  , fromChunk
  , blank
  , wrap

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
