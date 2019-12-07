{-# LANGUAGE TemplateHaskell #-}
-- | Functions and types to build 'BicolorTable's.  Everything you should
-- typically need is exported from "Rainbox".
module Rainbox.BicolorTable where

import Control.Lens
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
-- Unlike tables built with 'tableByRows' or 'tableByColumns', all
-- tables built with 'bicolorTable' will have separator colums
-- between each column.
data BicolorTable = BicolorTable
  { _bctEvenBackground :: Radiant
  -- ^ Background color for all even-numbered rows.  Row numbering
  -- starts with zero.  To use the terminal's default background color, use
  -- 'mempty'.

  , _bctOddBackground :: Radiant
  -- ^ Background color for all odd-colored rows.  Row numbering
  -- starts with zero.  To use the terminal's default background color, use
  -- 'mempty'.

  , _bctSpacerWidth :: Int
  -- ^ The width of each column of spacer cells.

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
bicolorTable = fmap tableByRows . bicolorToPlainTable

-- | Creates a bi-color table and renders it to the given 'Handle'
-- using 'bicolorTable' and 'hPutBox'.  Any errors from
-- 'bicolorTable' are repored with 'fail'.
hPutBicolorTable :: Handle -> BicolorTable -> IO ()
hPutBicolorTable h tbl = do
  box <- either fail return . bicolorTable $ tbl
  hPutBox h box

-- | Creates a bi-color table and renders it to standard output
-- using 'hPutBicolorTable'.
putBicolorTable :: BicolorTable -> IO ()
putBicolorTable = hPutBicolorTable stdout

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


-- | Convert a BicolorTable cell to a plain Cell.  Does all necessary Chunk
-- conversions.
bicolorToPlainCell
  :: Radiant
  -- ^ Appropriate background color
  -> Alignment Vertical
  -- ^ Column alignment
  -> BicolorTableCell
  -> Cell
bicolorToPlainCell rad align bic = Cell rws top align rad
  where
    rws = convertBicolorTableCellForRendering rad bic

-- | Convert a BicolorTable row to a plain Row.  Does all necessary Chunk conversions.
-- Includes spacer cells.
bicolorToPlainRow
  :: Radiant
  -- ^ Background color for even rows
  -> Radiant
  -- ^ Background color for odd rows
  -> Int
  -- ^ Column count, total
  -> Int
  -- ^ Width of spacer cells
  -> Int
  -- ^ Number for this row
  -> Seq (Alignment Vertical)
  -- ^ Column alignments
  -> BicolorTableRow
  -> Either String (Seq Cell)
bicolorToPlainRow bkgdEven bkgdOdd colsTot sepWidth colNum aligns columns
  | Seq.length columns /= colsTot = Left $ "length of row number " ++ show colNum
      ++ ": " ++ show (Seq.length columns) ++ " (should be: " ++ show colsTot ++ ")"
  | otherwise = Right row'
  where
    row' = Seq.intersperse spcr
      $ Seq.zipWith (bicolorToPlainCell bkgd) aligns columns
      where
        bkgd | even colNum = bkgdEven
             | otherwise = bkgdOdd
        spcr = separator bkgd sepWidth

-- | Converts a BicolorTable table to a plain table with 'Cell'.  Does all
-- necessary Chunk conversions, and includes spacer cells.
bicolorToPlainTable
  :: BicolorTable
  -> Either String (Seq (Seq Cell))
bicolorToPlainTable (BicolorTable bkgdEven bkgdOdd sepWidth colsTot aligns rws)
  | lenAligns /= colsTot = Left $ "length of alignments Seq: " ++ show lenAligns
      ++ " (should be: " ++ show colsTot ++ ")"
  | otherwise = Seq.traverseWithIndex f rws
  where
    lenAligns = Seq.length aligns
    f rowIdx = bicolorToPlainRow bkgdEven bkgdOdd colsTot sepWidth rowIdx aligns
