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

  -- * Sequence utilities
  , intersperse

  -- * Rendering
  , render
  , printBox
  ) where

import Rainbow.Colors
import Rainbox.Box
import Data.Monoid
import Data.String
import Data.Sequence (Seq, viewl, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Foldable as F

-- | A 'Cell' consists of multiple screen lines; each screen line is
-- a 'Bar'.
data Cell = Cell
  { bars :: Seq Bar
  -- ^ Each Bar is one line on the screen.

  , vert :: Align Vert
  -- ^ How this Cell aligns compared to other Cell in its row; use
  -- 'top', 'center', or 'bottom'.

  , horiz :: Align Horiz
  -- ^ How this Cell aligns compared to the other Cell in its
  -- column; use 'left', 'center', or 'right'.

  , background :: Radiant
  -- ^ Background color for necessary padding that is added to the
  -- Cell to make it the correct width and height.  Does not affect
  -- the 'Chunk' contained in the 'bars'; these will use the colors
  -- that are designated in the 'Chunk' itself.
  } deriving (Eq, Show)

instance HasWidth Cell where
  width cl
    | Seq.null (bars cl) = 0
    | otherwise = F.maximum . fmap width . bars $ cl

-- | Creates a Cell with a 'left' horizontal alignment, a 'top'
-- vertical alignment, and a background of 'noColorRadianat'.  The
-- cell will be one 'Bar' tall and contain the text given in the
-- string.
instance IsString Cell where
  fromString s = Cell (Seq.singleton (fromString s)) top left noColorRadiant

-- | Returns the width of each 'Bar' in the 'Cell'.
cellWidths :: Cell -> Seq Int
cellWidths = fmap width . bars

-- | Creates a single 'Box' from a list of rows of 'Cell'.  Each list
-- is a row of 'Cell'.  The list of rows is from top to bottom; within
-- each row, the cells are given from left to right.  All rows will be
-- the same length as the first row.  Any row that is longer than the
-- first row will have cells lopped off of the end, and any row that
-- is shorter than the first row will be padded with empty cells on
-- the end.

-- TODO equalize
gridByRows :: Seq (Seq Cell) -> Box
gridByRows sqnce = glue . fmap mkRow $ sqnce
  where
    glue = catV noColorRadiant left . fmap (catH noColorRadiant top)

    mkRow = applyToLargestValues getHeight widthMap cellToBox
      where
        getHeight = Height . Seq.length . bars

    widthMap = largestByIndex (Width . width) sqnce


-- | Creates a single 'Box' from a list of columns of 'Cell'.  Each
-- list is a column of 'Cell'.  The list of columns is from left to
-- right; within each column, the cells are given from top to bottom.
-- All columns will be the same height as the first column.  Any
-- column that is longer than the first column will have cells lopped
-- off the bottom, and any column that is shorter than the first
-- column will be padded on the bottom with blank cells.

gridByCols :: Seq (Seq Cell) -> Box
gridByCols = undefined
--gridByCols = glueBoxes . boxCells . arrayByCols padCell

-- | Examines a sequence to determine the length of the first
-- sequence.  Fails if the seuence is empty.
lengthOfFirst :: Seq (Seq a) -> Maybe Int
lengthOfFirst sq = case viewl sq of
  EmptyL -> Nothing
  x :< _ -> Just $ Seq.length x

emptyBox :: Box
emptyBox = blank noColorRadiant (Height 0) (Width 0)

-- | Make every 'Seq' as long as the given length, using the given
-- element as padding.
equalizeLengths :: Int -> a -> Seq (Seq a) -> Seq (Seq a)
equalizeLengths tgt pad = fmap padder
  where
    padder sq
      | len > tgt = Seq.take tgt sq
      | otherwise = sq <> Seq.replicate (tgt - len) pad
      where
        len = Seq.length sq

-- | Given a map of largest values for each index, a Seq of values,
-- and a function to calculate the largest value for that Seq, and a
-- function that takes those values and computes a new value, run the
-- computation.

applyToLargestValues
  :: (Ord b, Ord c)
  => (a -> b)
  -- ^ Find the value for item in this Seq
  -> Map Int c
  -- ^ Largest values by index
  -> (b -> c -> a -> d)
  -- ^ Processor
  -> Seq a
  -> Seq d
applyToLargestValues getVal mpLargest f sq = Seq.mapWithIndex k sq
  where
    thisLargest = F.maximum . fmap getVal $ sq
    k idx = f thisLargest (mpLargest M.! idx)


cellToBox
  :: Height
  -> Width
  -> Cell
  -> Box
cellToBox h w c = grow (background c) h w (vert c) (horiz c)
  $ barsToBox (background c) (horiz c) (bars c)
  


-- | Creates a map where every key has the largest value for the given
-- index.
largestByIndex
  :: Ord b
  => (a -> b)
  -- ^ Calculates the value we're interested in
  -> Seq (Seq a)
  -> Map Int b
largestByIndex get = F.foldl' procOuterSeq M.empty
  where
    procOuterSeq outerMap = Seq.foldlWithIndex procItem outerMap
      where
        procItem mp idx item = case M.lookup idx mp of
          Nothing -> update
          Just oldVal -> if val > oldVal then update else mp
          where
            val = get item
            update = M.insert idx val mp

{-
makeColsFromRows
  :: Int
  -- ^ Number of columns
  -> Seq (Seq a)
  -> Seq (Seq a)
makeColsFromRows nCols sq = fmap mkCol . Seq.fromList $ [0 .. nCols - 1]
  where
    mkCol idx = fmap (\rw -> rw `Seq.index` idx) sq
-}


padCell :: Cell
padCell = Cell Seq.empty top left noColorRadiant
