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
  , Alignment(BuiltBox, Opposite, segment)

  -- * Boxes
  , Box
  , BoxH
  , BoxV

  -- * Box and block construction
  , blockFromChunk
  , blankBlock
  , convertBox

  -- * Rendering
  , render

  -- * Tables
  , CellRow(..)
  , Cell(..)
  , RowCol(..)
  , Row
  , Column
  , RowsCols(..)
  , Rows
  , Columns
  , table
  , tableByRows
  , tableByColumns

  -- * Utilities
  , intersperse
  ) where

import Rainbox.Core
