-- | Create grids of (possibly) colorful boxes.
--
-- For an introduction, see "Rainbox.Tutorial".  That file is
-- written in literate Haskell, so you will want to look at the
-- source itself.  HsColour does not do very well with literate
-- Haskell, so you will want to view the file in your text editor or
-- on Github:
--
-- <https://github.com/massysett/rainbox/blob/master/lib/Rainbox/Tutorial.lhs>
--
-- This module only helps you create simple grids of cells, rather
-- like a spreadsheet that does not allow you to merge or split
-- cells.  If your needs are more complicated, use "Rainbox.Box",
-- which allows you to build 'Box'es of arbitrary complexity by
-- pasting simpler 'Box'es together.  (You can of course use this
-- module together with "Rainbox.Box" to create very complex
-- layouts.)
module Rainbox
  (
    -- * Alignment
    Align
  , Horiz
  , Vert
  , top
  , bottom
  , left
  , right
  , center

  -- * Bar
  , Bar(..)

  -- * Cell and Box
  , Cell(..)
  , Box

  -- * Creating Box and gluing them together

  -- | For simple needs you will only need 'gridByRows' or
  -- 'gridByCols'; 'boxCells' and 'glueBoxes' are provided for more
  -- complex needs.
  , gridByRows
  , gridByCols
  , boxCells
  , glueBoxes

  -- * Rendering
  , render
  , printBox
  ) where

import Rainbow.Colors
import Rainbox.Box
import Rainbox.Array2d
import Data.Array
import Data.String

-- | A 'Cell' consists of multiple screen lines; each screen line is
-- a 'Bar'.
data Cell = Cell
  { bars :: [Bar]
  -- ^ Each Bar is one line on the screen.

  , horiz :: Align Horiz
  -- ^ How this Cell aligns compared to the other Cell in its
  -- column; use 'left', 'center', or 'right'.

  , vert :: Align Vert
  -- ^ How this Cell aligns compared to other Cell in its row; use
  -- 'top', 'center', or 'bottom'.

  , background :: Radiant
  -- ^ Background color for necessary padding that is added to the
  -- Cell to make it the correct width and height.  Does not affect
  -- the 'Chunk' contained in the 'bars'; these will use the colors
  -- that are designated in the 'Chunk' itself.
  } deriving (Eq, Show)

-- | Creates a Cell with a 'left' horizontal alignment, a 'top'
-- vertical alignment, and a background of 'noColorRadianat'.  The
-- cell will be one 'Bar' tall and contain the text given in the
-- string.
instance IsString Cell where
  fromString s = Cell [(fromString s)] left top noColorRadiant

-- | Returns the width of each 'Bar' in the 'Cell'.
cellWidths :: Cell -> [Int]
cellWidths = map width . bars

-- | Transforms a grid of 'Cell' to a grid of 'Box' by adding
-- necessary padding to each 'Cell'.  In every row of the array, all
-- the 'Box' will have equal height; in every column of the array,
-- all the 'Box' will have equal width.
boxCells
  :: (Ix col, Ix row)
  => Array (col, row) Cell
  -> Array (col, row) Box
boxCells ay = cells $ mapTable conv tbl
  where
    tbl = table getWidth getHeight ay
      where
        getWidth _ = maximum . (0:) . concat . map cellWidths . map snd
        getHeight _ = maximum . (0:) . map (length . bars . snd)
    conv lCol lRow _ _ c = grow bk (Height lRow) (Width lCol) av ah bx
      where
        Cell bs ah av bk = c
        bx = barsToBox bk ah bs

-- | Use 'catH' and 'catV' to fuse an array of 'Box' into a single
-- 'Box'.  For example, if the 'bounds' of the array are
-- @((0,0),(3,5))@, then the array has the number of cells given by
-- @rangeSize ((0,0), (3,5))@ (that is, 24).  The upper left corner
-- is @(0,0)@ and the lower right corner is @(3,5)@; the upper right
-- and lower left corners are @(3,0)@ and @(0,5)@, respectively.
glueBoxes
  :: (Ix col, Ix row)
  => Array (col, row) Box
  -> Box
glueBoxes
  = catH noColorRadiant top
  . map (catV noColorRadiant left)
  . cols

-- | Creates a single 'Box' from a list of rows of 'Cell'.  Each list
-- is a row of 'Cell'.  The list of rows is from top to bottom; within
-- each row, the cells are given from left to right.  All rows will be
-- the same length as the first row.  Any row that is longer than the
-- first row will have cells lopped off of the end, and any row that
-- is shorter than the first row will be padded with empty cells on
-- the end.

gridByRows :: [[Cell]] -> Box
gridByRows = glueBoxes . boxCells . arrayByRows padCell

-- | Creates a single 'Box' from a list of columns of 'Cell'.  Each
-- list is a column of 'Cell'.  The list of columns is from left to
-- right; within each column, the cells are given from top to bottom.
-- All columns will be the same height as the first column.  Any
-- column that is longer than the first column will have cells lopped
-- off the bottom, and any column that is shorter than the first
-- column will be padded on the bottom with blank cells.

gridByCols :: [[Cell]] -> Box
gridByCols = glueBoxes . boxCells . arrayByCols padCell

padCell :: Cell
padCell = Cell [] left top noColorRadiant
