{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedLists #-}
module Rainbox.Core where

import Rainbow
import Control.Monad (join)
import Data.Monoid
import Data.Sums
import Rainbow.Types (Chunk(..))
import Data.Sequence (Seq, ViewL(..), viewl, (|>), (<|))
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Text as X
import GHC.Exts (IsList(..))

-- # Alignment

-- | Alignment.
data Align a = Center | NonCenter a
  deriving (Eq, Show)

-- | Horizontal alignment.
data Horiz = ATop | ABottom
  deriving (Eq, Show)

-- | Vertical alignment.
data Vert = ALeft | ARight
  deriving (Eq, Show)

-- | Place this block so that it is centered on the vertical axis or
-- horizontal axis.
center :: Align a
center = Center

-- | Place this block's left edge on the vertical axis.
left :: Align Vert
left = NonCenter ALeft

-- | Place this block's right edge on the vertical axis.
right :: Align Vert
right = NonCenter ARight

-- | Place this box's top edge on the horizontal axis.
top :: Align Horiz
top = NonCenter ATop

-- | Place this box's bottom edge on the horizontal axis.
bottom :: Align Horiz
bottom = NonCenter ABottom

-- | Class for alignments; both 'Align' 'Vert' and 'Align' 'Horiz' are
-- instances of this class.
class Alignment a where
  type BuiltBox a
  -- ^ The type of box that is constructed from values of this
  -- alignment.
  type Opposite a
  -- ^ The opposite type of 'BuiltBox'.
  convert :: Align a -> Radiant -> Opposite a -> BuiltBox a
  wrap :: Align a -> Radiant -> BuiltBox a -> BuiltBox a
  fromCore :: Align a -> Radiant -> Core -> BuiltBox a
  segment :: Radiant -> Int -> BuiltBox a
  -- ^ Builds a line segment; that is, a one-dimensional box that has
  -- the given height or width.  This is often all you need if you are
  -- making a box solely to separate other boxes.  To build
  -- two-dimensional blank boxes, see 'blank'.

-- | Things that are oriented around a vertical axis.
class LeftRight a where
  -- | Length to the left of the vertical axis.
  port :: a -> Int

  -- | Length to the right of the vertical axis.
  starboard :: a -> Int

-- | Things that are oriented around a horizontal axis.
class UpDown a where
  -- | Number of lines above the horizontal axis.
  above :: a -> Int
  -- | Number of lines below the horizontal axis.
  below :: a -> Int


-- # Height and Width

-- | A count of rows
newtype Height = Height Int
  deriving (Eq, Ord, Show)

class HasHeight a where
  height :: a -> Int

instance (HasHeight a, HasHeight b, HasHeight c)
  => HasHeight (S3 a b c) where
  height = caseS3 height height height

-- | A count of columns
newtype Width = Width Int
  deriving (Eq, Ord, Show)

instance (HasWidth a, HasWidth b, HasWidth c)
  => HasWidth (S3 a b c) where
  width = caseS3 width width width

class HasWidth a where
  width :: a -> Int

-- # Box parts

-- | An intermediate type used in rendering; it consists either of
-- text 'Chunk' or of a number of spaces.
newtype Rod = Rod (Either (Int, Radiant) Chunk)

-- | The core of a block is either a text 'Chunk' or, if the box is
-- blank, is merely a height and a width.
newtype Core = Core (Either Chunk (Height, Width))

instance HasWidth Core where
  width (Core ei) = case ei of
    Left (Chunk _ t) -> F.sum . fmap X.length $ t
    Right (_, Width w) -> max 0 w

instance HasHeight Core where
  height (Core ei) = case ei of
    Left _ -> 1
    Right (Height h, _) -> max 0 h

-- | Convert a 'Core' to a 'Seq' of 'Rod' for rendering.
rodsFromCore :: Radiant -> Core -> Seq Rod
rodsFromCore rd (Core ei) = case ei of
  Left ck -> Seq.singleton . Rod . Right $ ck
  Right (Height h, Width w) -> Seq.replicate h . Rod . Left $ (w, rd)

-- | How many screen columns does this 'Seq' of 'Rod' occupy?
rodsLength :: Seq Rod -> Int
rodsLength = F.sum . fmap toLen
  where
    toLen (Rod ei) = case ei of
      Left (i, _) -> i
      Right (Chunk _ xs) -> F.sum . fmap X.length $ xs

-- | Converts a nested 'Seq' of 'Rod' to a nested 'Seq' of 'Chunk' in
-- preparation for rendering.  Newlines are added to the end of each
-- line.
chunksFromRods :: Seq (Seq Rod) -> Seq (Seq Chunk)
chunksFromRods = fmap (|> "\n") . fmap (fmap chunkFromRod)
  where
    chunkFromRod (Rod ei) = case ei of
      Left (i, r) -> (chunkFromText . X.replicate i $ " ") <> back r
      Right c -> c

-- | Both 'BoxH' and 'BoxV' are members of this class, which allow a
-- box to be converted into 'Chunk'.
class Box a where
  -- | Convert a 'Box' to a nested 'Seq' of 'Rod'.
  makeBox :: a -> Seq (Seq Rod)

-- | Payload for a box that is oriented around a vertical axis.
data PayloadV = PayloadV (Align Vert) Radiant (S3 BoxV BoxH Core)

instance HasHeight PayloadV where
  height (PayloadV _ _ s3) = height s3

instance HasWidth PayloadV where
  width (PayloadV _ _ s3) = width s3

instance LeftRight PayloadV where
  port (PayloadV a _ s3) = case a of
    NonCenter ALeft -> 0
    NonCenter ARight -> width s3
    Center -> fst . split . width $ s3

  starboard (PayloadV a _ s3) = case a of
    NonCenter ALeft -> width s3
    NonCenter ARight -> 0
    Center -> snd . split . width $ s3

-- | Payload for a box that is oriented around a horizontal axis.
data PayloadH = PayloadH (Align Horiz) Radiant (S3 BoxV BoxH Core)

instance HasHeight PayloadH where
  height (PayloadH _ _ s3) = height s3

instance HasWidth PayloadH where
  width (PayloadH _ _ s3) = width s3

instance UpDown PayloadH where
  above (PayloadH a _ s3) = case a of
    NonCenter ATop -> 0
    NonCenter ABottom -> height s3
    Center -> fst . split . height $ s3

  below (PayloadH a _ s3) = case a of
    NonCenter ATop -> height s3
    NonCenter ABottom -> 0
    Center -> snd . split . height $ s3

-- | A 'BoxV' box contains zero or more blocks.  Each block is
-- oriented relative to a single vertical axis.  Blocks are aligned
-- verically along this axis, rather like a flagpole.  'BoxV' can be
-- combined using the 'Monoid' functions.  When you are done adding
-- blocks to the 'BoxV' by combining separate 'BoxV', you can convert
-- it to a 'BoxH' using 'convert'.
newtype BoxV = BoxV (Seq PayloadV)

instance Monoid BoxV where
  mempty = BoxV mempty
  mappend (BoxV x) (BoxV y) = BoxV $ x <> y

instance LeftRight BoxV where
  port (BoxV sq) = F.foldl' max 0 . fmap port $ sq
  starboard (BoxV sq) = F.foldl' max 0 . fmap starboard $ sq

instance HasWidth BoxV where
  width b = port b + starboard b

instance HasHeight BoxV where
  height (BoxV sq) = F.sum . fmap height $ sq

-- | A 'BoxH' box contains zero or more blocks.  Each block is
-- oriented relative to a single horizontal axis.  Blocks are aligned
-- horizontally along this axis, rather like railroad cars on a track.
-- 'BoxH' can be combined using the 'Monoid' functions.  When you are
-- done adding blocks to the 'BoxH' by combining separate 'BoxH', you
-- can convert it to a single 'BoxV' using 'convert'.
newtype BoxH = BoxH (Seq PayloadH)

instance Monoid BoxH where
  mempty = BoxH mempty
  mappend (BoxH x) (BoxH y) = BoxH $ x <> y

instance Box BoxH where
  makeBox bx@(BoxH sqnce) = mergeVert $ fmap equalize sqnce
    where
      maxTop = above bx
      maxBot = below bx
      w = width bx
      mergeVert sqn = case viewl sqn of
        EmptyL -> Seq.empty
        x :< xs -> F.foldl' comb x xs
        where
          comb acc sq = Seq.zipWith (<>) acc sq
      equalize bhp@(PayloadH _ rd s3) = tp <> this <> bot
        where
          this = caseS3 makeBox makeBox
            (fmap Seq.singleton $ rodsFromCore rd) s3
          tp = Seq.replicate (max 0 (maxTop - above bhp)) pad
          bot = Seq.replicate (max 0 (maxBot - below bhp)) pad
          pad = Seq.singleton . Rod . Left $ (w, rd)

instance UpDown BoxH where
  above (BoxH sq) = F.foldl' max 0 . fmap above $ sq
  below (BoxH sq) = F.foldl' max 0 . fmap below $ sq

instance HasHeight BoxH where
  height b = above b + below b

instance HasWidth BoxH where
  width (BoxH sq) = F.sum . fmap width $ sq

instance Box BoxV where
  makeBox bx@(BoxV sqnce) = mergeHoriz $ fmap equalize sqnce
    where
      mergeHoriz = F.foldl' (<>) Seq.empty
      equalize (PayloadV a rd s3) = fmap addLeftRight this
        where
          this = caseS3 makeBox makeBox
            (fmap Seq.singleton $ rodsFromCore rd) s3
          addLeftRight lin = padder lenLft <> lin <> padder lenRgt
            where
              lenLin = rodsLength lin
              lenLft = case a of
                Center -> port bx - (fst . split $ lenLin)
                NonCenter ALeft -> 0
                NonCenter ARight -> port bx - lenLin
              lenRgt = case a of
                Center -> starboard bx - (snd . split $ lenLin)
                NonCenter ALeft -> starboard bx - lenLin
                NonCenter ARight -> 0
              padder len
                | len < 1 = Seq.empty
                | otherwise = Seq.singleton . Rod . Left
                      $ (len, rd)

instance Alignment Horiz where
  type BuiltBox Horiz = BoxH
  type Opposite Horiz = BoxV
  convert a r b = BoxH . Seq.singleton $
    PayloadH a r (S3a b)
  wrap a r b = BoxH . Seq.singleton $
    PayloadH a r (S3b b)
  fromCore a r c = BoxH . Seq.singleton $
    PayloadH a r (S3c c)
  segment r i = BoxH . Seq.singleton $
    PayloadH (NonCenter ATop) r (S3c . Core . Right $
      (Height 0, Width i))

instance Alignment Vert where
  type BuiltBox Vert = BoxV
  type Opposite Vert = BoxH
  convert a r b = BoxV . Seq.singleton $
    PayloadV a r (S3b b)
  wrap a r b = BoxV . Seq.singleton $
    PayloadV a r (S3a b)
  fromCore a r c = BoxV . Seq.singleton $
    PayloadV a r (S3c c)
  segment r i = BoxV . Seq.singleton $
    PayloadV (NonCenter ALeft) r (S3c . Core . Right $
      (Height i, Width 0))

-- | Construct a box from a single 'Chunk'.  Either a 'BoxH' or a
-- 'BoxV' will be built depending on the return type of the function.
fromChunk
  :: Alignment a
  => Align a
  -> Radiant
  -> Chunk
  -> BuiltBox a
fromChunk a r c = fromCore a r (Core (Left c))

-- | Construct a blank box.  Useful for adding in background spacers.
-- For a function that builds one-dimensional boxes, see 'segment',
-- which often is all you need if you are making a blank box to
-- separate other boxes.
blank
  :: Alignment a
  => Align a
  -> Radiant
  -> Height
  -> Width
  -> BuiltBox a
blank a r h w = fromCore a r (Core (Right (h, w)))

-- | Convert a box to a 'Seq' of 'Chunk' in preparation for rendering.
-- Use 'F.toList' to convert the 'Seq' of 'Chunk' to a list so that
-- you can print it using the functions in "Rainbow".
render :: Box a => a -> Seq Chunk
render = join . chunksFromRods . makeBox

-- # Tables

-- | A row of text in a single cell.
newtype CellRow = CellRow (Seq Chunk)
  deriving (Eq, Ord, Show)

instance IsList CellRow where
  type Item CellRow = Chunk
  fromList = CellRow . Seq.fromList
  toList (CellRow sq) = F.toList sq

-- | A single cell; resembles a spreasheet cell.  It can have multiple
-- rows of text.
newtype Cell = Cell (Seq CellRow)
  deriving (Eq, Ord, Show)

instance IsList Cell where
  type Item Cell = CellRow
  fromList = Cell . Seq.fromList
  toList (Cell sq) = F.toList sq

-- | Either a row or a column.  If it's a row, then each
-- cell must appear in the row in left to right order; if it's a
-- column, each cell must appear in the column in top to bottom order.
newtype RowCol a = RowCol (Seq (a, Radiant, Cell))
  deriving (Eq, Ord, Show)

instance IsList (RowCol a) where
  type Item (RowCol a) = (a, Radiant, Cell)
  fromList = RowCol . Seq.fromList
  toList (RowCol sq) = F.toList sq

-- | A row; each value must appear in the row in
-- left-to-right order.
type Row = RowCol

-- | A column; each value must appear in the column in
-- top-to-bottom order.
type Column = RowCol

-- | Either a set of rows or a set of columns.  If it's a
-- set of rows, each row must appear in top to bottom order; if it's a
-- set of columns, each column must appear in left-to-right order.
newtype RowsCols a = RowsCols (Seq (RowCol a))
  deriving (Eq, Ord, Show)

instance IsList (RowsCols a) where
  type Item (RowsCols a) = RowCol a
  fromList = RowsCols . Seq.fromList
  toList (RowsCols sq) = F.toList sq

-- | A set of rows; each row must appear in top-to-bottom
-- order.
type Rows = RowsCols Horiz

-- | A set of columns; each column must appear in
-- left-to-right order.
type Columns = RowsCols Vert

-- | Create a table for a set of either rows or columns.
table
  :: Alignment a
  => RowsCols a
  -> BuiltBox a
table = undefined

-- | Create a table from a set of rows.
tableByRows
  :: Rows
  -> BoxH
tableByRows = table

-- | Create a table from a set of columns.
tableByColumns
  :: Columns
  -> BoxV
tableByColumns = table

-- # Utilities

-- | Like 'Data.List.intersperse' in "Data.List", but works on 'Seq'.
intersperse :: a -> Seq a -> Seq a
intersperse new sq = case viewl sq of
  EmptyL -> Seq.empty
  x :< xs -> x <| go xs
    where
      go sqnce = case viewl sqnce of
        EmptyL -> Seq.empty
        a :< as -> new <| a <| go as

-- | Split a number into two parts, so that the sum of the two parts
-- is equal to the original number.
split :: Int -> (Int, Int)
split i = (r, r + rm)
  where
    (r, rm) = i `quotRem` 2
