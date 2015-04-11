-- | Typically to use Rainbox you will want these @import@s:
--
-- @
-- import qualified Data.Sequence as Seq
-- import Rainbow
-- import Rainbox
--
-- -- and, for GHC before 7.10:
-- import Data.Monoid
-- @
--
-- Rainbox does not re-export anything from "Data.Sequence" or
-- "Rainbow" because I don't know if you want all those things dumped
-- into the same namespace.
--
-- "Rainbox.Tutorial" wil get you started.  "Rainbox.Core" contains
-- the implementation details, which you should not need to pay
-- attention to (if you do need to use "Rainbox.Core" for ordinary
-- usage of the library, that's a bug; please report it.)
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
  , centerH
  , centerV
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
  , separator

  -- * Utilities
  , intersperse
  ) where

import Rainbox.Core
