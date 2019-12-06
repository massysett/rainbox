{-# LANGUAGE TemplateHaskell #-}
module Rainbox.BicolorTable where

import Control.Lens
import Control.Monad.Except
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Rainbow
import qualified Rainbow.Types as R
import Rainbox.Core
import System.IO

-- | A single line within a cell in a 'BicolorTable'.  For each
-- 'Chunk', leave the 'back' as the default if you want the 'Chunk'
-- background to match '_bctEvenBackground' or '_bctOddBackground'.
-- If you specify a background color for any 'Chunk', it will for
-- that 'Chunk' override the table's background color.
type BicolorTableCellLine = Seq Chunk

-- | The set of all lines within a cell in a 'BicolorTable'.
type BicolorTableCell = Seq BicolorTableCellLine

-- | The set of all columns in a single row.  The length of each
-- 'BicolorTableRow' must be equal to '_bctColumnCount'; otherwise,
-- 'bicolorTable' will fail with an error message.
type BicolorTableRow = Seq BicolorTableCell

-- | Description for a table with rows of alternating background colors.  For
-- instance, if designed for a terminal with a white background, the
-- row backgrounds might alternate between white and light grey.
-- The different backgrounds help with readability.
--
-- For the 'Chunk' that are in the table, simply leave the 'back'
-- color blank if you wish to use the row's background color.  Upon
-- rendering, 'bicolorTable' will render the 'Chunk' with a
-- background color that matches that of the row.  If you specify a
-- background color for a 'Chunk', it will override the background
-- color for the row.
--
-- Note that a row may contain more than one line of text.
--
-- Unlike tables built with 'tableByRows' or 'tableByColums', all
-- tables built with 'bicolorTable' will have separator colums one
-- space wide between each column.
data BicolorTable = BicolorTable
  { _bctEvenBackground :: Radiant
    -- ^ Background color for all even-numbered rows.  Row numbering
    -- starts with zero.

  , _bctOddBackground :: Radiant
    -- ^ Background color for all odd-colored rows.  Row numbering
    -- starts with zero.

  , _bctColumnCount :: Int
  -- ^ How many columns are in this table.  Do not include the
  -- one-space-wide separator columns, which will be added
  -- automatically.

  , _bctAlignments :: Seq (Alignment Vertical)
  -- ^ Specifies the alignment for each column in the table.  The
  -- length of this 'Seq' must be equal to '_bctColumnCount';
  -- otherwise 'bicolorTable' will fail with an error message.

  , _bctRows :: Seq BicolorTableRow
  -- ^ Specifies all the textual and color data for the
  -- BicolorTable.  The outermost 'Seq' is the set of all rows.
  -- These will alternate in background color bewteen
  -- '_bctEvenBackground' and '_bctOddBackground'.  This is a
  -- Russian doll of nested 'Seq'; the type synonyms help explain
  -- the types.
  } deriving Show

makeLenses ''BicolorTable

-- | Creates a bi-color table.  If the number of columns in each
-- 'BicolorTableRow' is not equal to '_bctColumnCount' or if the
-- length of '_bctAlignments' is not equal to '_bctColumnCount',
-- this will return 'Left'; otherwise, returns 'Right' with a 'Box'
-- 'Vertical' that can then be rendered.
bicolorTable :: BicolorTable -> Either String (Box Vertical)
bicolorTable = undefined

-- | Creates a bi-color table and renders it to the given 'Handle'
-- using 'bicolorTable' and 'hPutBox'.  Any errors from
-- 'bicolorTable' are repored with 'fail'.
hPutBicolorTable :: Handle -> BicolorTable -> IO ()
hPutBicolorTable = undefined

-- | Creates a bi-color table and renders it to standard output
-- using 'hPutBicolorTable'.
putBicolorTable :: BicolorTable -> IO ()
putBicolorTable = undefined

-- | Number each row of a 'Seq' with its row number.
numberSeq :: Seq a -> Seq (Int, a)
numberSeq ls = Seq.zip nums ls
  where
    nums = Seq.iterateN (Seq.length ls) succ 0

-- | Convert a 'Chunk' for rendering by substituting the table's
-- row background for the chunk's row background if applicable.
convertChunkForRendering
  :: Radiant
  -- ^ Background for this row
  -> Chunk
  -> Chunk
convertChunkForRendering rad chk
  = chk
  & over (R.scheme . R.style8 . R.back) newBack8
  & over (R.scheme . R.style256 . R.back) newBack256
  where
    newBack8 (R.Color Nothing) = R._color8 rad
    newBack8 x = x
    newBack256 (R.Color Nothing) = R._color256 rad
    newBack256 x = x

-- | Converts a 'BicolorTableCellLine' for rendering.
convertBicolorTableCellLineForRendering
  :: Radiant
  -> BicolorTableCellLine
  -> BicolorTableCellLine
convertBicolorTableCellLineForRendering rad = fmap (convertChunkForRendering rad)

-- | Converts a 'BicolorTableCell' for rendering.
convertBicolorTableCellForRendering
  :: Radiant
  -> BicolorTableCell
  -> BicolorTableCell
convertBicolorTableCellForRendering rad
  = fmap (convertBicolorTableCellLineForRendering rad)
