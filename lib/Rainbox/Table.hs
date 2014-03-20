module Rainbox.Table
  ( Bar(..)
  , Cell(..)
  , Record(..)
  , Records
  , unRecords
  , Column(..)
  , MultiWidth(..)
  , maxWidth
  , Columns
  , unColumns
  , Crate(..)
  , toRecords
  , toColumns
  , justifyRecords
  , Carton(..)
  , opaque
  , clear
  , mappendRecord
  , mappendColumn
  , formatRecords
  , formatTableTextSpec
  , fancyTable
  ) where


import Data.List (transpose)
import Rainbox
import qualified Rainbox as B
import Data.Monoid
import qualified Data.Text as X
import System.Console.Rainbow
import System.Console.Rainbow.Types

-- # Tables

-- | Forms the basis of a 'Cell'.  A single screen line of text
-- within a single 'Cell'.
newtype Bar a = Bar { unBar :: [a] }
  deriving (Eq, Ord, Show)

instance HasWidth a => HasWidth (Bar a) where
  width = sum . map width . unBar

instance Functor Bar where
  fmap f = Bar . map f . unBar

-- | A 'Cell' consists of multiple screen lines; each screen line is
-- a 'Bar'.
newtype Cell a = Cell { unCell :: [Bar a] }
  deriving (Eq, Ord, Show)

instance Functor Cell where
  fmap f = Cell . map (fmap f) . unCell

instance HasWidth a => MultiWidth (Cell a) where
  multiWidth = map width . unCell

-- | A 'Record' consists of multple 'Cell' which appear on screen
-- from left to right.
newtype Record a = Record { unRecord :: [Cell a] }
  deriving (Eq, Ord, Show)

instance Functor Record where
  fmap f = Record . map (fmap f) . unRecord

-- | Several 'Record' grouped together.  For a 'Records'  to be
-- valid, every 'Record' must contain an equal number of 'Cell'.  A
-- valid 'Records' might not appear justified, because the cells
-- might not be of uniform width.
newtype Records a = Records { unRecords :: [Record a] }
  deriving (Eq, Ord, Show)

instance Functor Records where
  fmap f = Records . map (fmap f) . unRecords

-- | Several 'Cell' that are in one tabular column.
newtype Column a = Column { unColumn :: [Cell a] }
  deriving (Eq, Ord, Show)

instance Functor Column where
  fmap f = Column . map (fmap f) . unColumn

instance HasWidth a => MultiWidth (Column a) where
  multiWidth = concat . map multiWidth . unColumn

-- | Something that has multiple possible widths at different
-- points.

class MultiWidth a where
  multiWidth :: a -> [Int]

maxWidth :: MultiWidth a => a -> Int
maxWidth = maximum . (0:) . multiWidth

-- | Several 'Column' grouped together.  For a 'Columns' to be
-- valid, each 'Column' must have an equal number of 'Cell'.
newtype Columns a = Columns { unColumns :: [Column a] }
  deriving (Eq, Ord, Show)

instance Functor Columns where
  fmap f = Columns . map (fmap f) . unColumns

data Crate = Crate
  { crateText :: X.Text
  , crateFormat :: TextSpec -> TextSpec
  }

instance Show Crate where
  show = X.unpack . crateText

toColumns :: Records a -> Columns a
toColumns = Columns . map Column . transpose
  . map unRecord . unRecords

toRecords :: Columns a -> Records a
toRecords = Records . map Record . transpose
  . map unColumn . unColumns

columnWidths :: HasWidth a => Columns a -> [Int]
columnWidths = map maxWidth . unColumns


-- | Equalize the cells in each Record and justify them.
justifyRecords
  :: [Align Horiz]
  -- ^ Specifies the alignment for each column in the table.  This
  -- list can be infinite.  If the table has more columns than are
  -- in this list, the extra columns will be formatted with a 'left'
  -- justification.

  -> [Record Chunk]
  -- ^ Each input Record.

  -> [[Background -> Box]]
justifyRecords as rs = map justify' . unRecords $ equalLengthRows
  where
    equalLengthRows = equalizeRecordLengths rs

    justify' = zipWith3 makeCell aligns widths . unRecord
      where
        aligns = as ++ repeat B.left
        widths = columnWidths . toColumns $ equalLengthRows


-- | Takes list of input Record and returns a list of Record where
-- each Record has the same number of cells.
equalizeRecordLengths :: [Record a] -> Records a
equalizeRecordLengths rs = Records $ map equalize rs
  where
    equalize (Record rec) = Record . take maxLen
      $ rec ++ repeat (Cell [])
    maxLen = maximum . (0:) . map (length . unRecord) $ rs

-- | Creates a cell.
makeCell
  :: Align Horiz
  -> Int
  -- ^ Padded final width of cell
  -> Cell Chunk
  -> Background
  -> Box
makeCell align wdth cell bk
  = growH bk wdth align
  . B.catV bk align
  . map B.chunks
  . map unBar
  . unCell
  $ cell

-- | Format the rows and columns of a table with TextSpec.
formatTableTextSpec
  :: [(Align Horiz, TextSpec)]
  -- ^ One element in the list for each column in the table.  If
  -- there are extra columns, they are formatted with the default
  -- 'TextSpec' and with left alignment.

  -> [(TextSpec, Record Crate)]
  -- ^ Each row of the table is represented with a pair.  The
  -- 'TextSpec' for each Record is combined with the 'TextSpec' for
  -- each respective column to format necessary padding.

  -> [[Box]]
formatTableTextSpec formats rws = map toBoxes rws
  where
    fmts = formats ++ repeat (B.left, mempty)
    toBoxes (tsRec, Record cs) = zipWith mkBox fmts cs
      where
        mkBox (align, tsCol) crate = boxFromCrates tsRec align tsCol crate

boxFromCrates
  :: TextSpec
  -- ^ TextSpec for the 'Record'
  -> Align Horiz
  -- ^ Alignment for the column
  -> TextSpec
  -- ^ TextSpec for the 'Column'
  -> Cell Crate
  -> Box
boxFromCrates tsRec ah tsCol cell
  = B.catV bk ah
  . map B.chunks
  . map barToChunks
  . unCell
  $ cell
  where
    barToChunks = map crateToChunk . unBar
    crateToChunk c = Chunk (crateFormat c tsCol) (crateText c)
    bk = backgroundFromTextSpec $ tsCol <> tsRec

--
-- # Fancy table
--

data Carton = Carton
  { cartonText :: X.Text
  , cartonFormat :: TextSpec -> TextSpec -> TextSpec
  -- ^ This function will be applied to the TextSpec for the Record and
  -- the TextSpec for the column (in that order) to get the final
  -- TextSpec for the text in the Carton.
  }

instance Show Carton where
  show c = "carton: " ++ show (cartonText c)

-- | Use only the TextSpec given in the Chunk.  Ignore the TextSpec
-- from the Column and from the Record.
opaque :: Chunk -> Carton
opaque c = Carton (text c) (\_ _ -> textSpec c)

-- | To get the final TextSpec, start with the TextSpec for the
-- 'Column', 'mappend' the TextSpec for the 'Record', and then
-- 'mappend' the TextSpec for the 'Chunk' given here.
clear :: Chunk -> Carton
clear c = Carton (text c) (\col rec -> col <> rec <> textSpec c)

-- | To get the final TextSpec, start with the TextSpec for the
-- 'Record' and then 'mappend' the TextSpec for the 'Chunk' given
-- here.
mappendRecord :: Chunk -> Carton
mappendRecord c = Carton (text c) (\_ rec -> rec <> textSpec c)

-- | To get the final TextSpec, start with the TextSpec for the
-- 'Column' and then 'mappend' the TextSpec for the 'Chunk' given
-- here.
mappendColumn :: Chunk -> Carton
mappendColumn c = Carton (text c) (\col _ -> col <> textSpec c)

formatRecords
  :: [(TextSpec, Record Carton)]
  -- ^ Each record in the table.  The TextSpec is the default
  -- formatting for the row.  It is combined with the Carton and,
  -- later, with column-specific formatting.
  -> [(TextSpec, Record Crate)]
  -- ^ Suitable for use with formatTableTextSpec.
formatRecords = map f
  where
    f (ts, rc) = (ts, fmap (cartonToCrate ts) rc)

-- | With information about the TextSpec for a particular Record,
-- convert a Carton to a Crate.
cartonToCrate
  :: TextSpec
  -> Carton
  -> Crate
cartonToCrate tsRec ctn =
  Crate (cartonText ctn) (cartonFormat ctn tsRec)

fancyTable
  :: [(Align Horiz, TextSpec)]
  -> [(TextSpec, Record Carton)]
  -> [[Box]]
fancyTable cols recs = formatTableTextSpec cols cartons
  where
    cartons = formatRecords recs


