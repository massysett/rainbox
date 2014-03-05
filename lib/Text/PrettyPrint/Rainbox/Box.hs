-- | Box primitives.
--
-- This module provides all functions that have access to the
-- internals of a 'Box'.  There are only five functions that make a
-- 'Box':
--
-- * 'blank' - formats a blank box with nothing but a (possibly)
-- colorful background.  Useful to paste to other 'Box' to provide
-- white space.
--
-- * 'chunks' - Makes a box out of Rainbow 'Chunk'.
--
-- * 'hcat' - paste 'Box' together horizontally
--
-- * 'vcat' - paste 'Box' together vertically
--
-- * 'view' - take a portion of an already-existing 'Box'.
--
-- There are many crude diagrams in the Haddock documentation.  A
-- dash means a character with data; a period means a blank
-- character.  When you print your 'Box', the blank characters will
-- have the appropriate background color.
module Text.PrettyPrint.Rainbox.Box
  ( -- * Background
    Background(..)
  , defaultBackground

  -- * Box
  , Row(..)
  , Box
  , unBox

  -- * Rows and Cols
  , Rows(..)
  , rows
  , Cols(..)
  , HasCols(..)

  -- * Alignment
  , Align
  , Vert
  , Horiz
  , center
  , top
  , bottom
  , left
  , right

  -- * Making Boxes
  , blank
  , chunks
  , hcat
  , vcat
  , view

  ) where

import Text.PrettyPrint.Rainbox.Box.Internal

