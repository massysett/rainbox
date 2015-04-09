{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedLists #-}
module Rainbox.Core where

import Rainbow
import Control.Monad (join)
import Data.Monoid
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

-- | Vertical alignment.
data Vert = ATop | ABottom
  deriving (Eq, Show)

-- | Horizontal alignment.
data Horiz = ALeft | ARight
  deriving (Eq, Show)

-- | Place this block so that it is centered on the vertical axis or
-- horizontal axis.
center :: Align a
center = Center

-- | Place this block's left edge on the vertical axis.
left :: Align Horiz
left = NonCenter ALeft

-- | Place this block's right edge on the vertical axis.
right :: Align Horiz
right = NonCenter ARight

-- | Place this box's top edge on the horizontal axis.
top :: Align Vert
top = NonCenter ATop

-- | Place this box's bottom edge on the horizontal axis.
bottom :: Align Vert
bottom = NonCenter ABottom

-- | Class for alignments; both 'Align' 'Horiz' and 'Align' 'Vert' are
-- instances of this class.
class Alignment a where
  type BuiltBox a
  -- ^ The type of box that is constructed from values of this
  -- alignment.
  type Opposite a
  -- ^ The opposite type of 'BuiltBox'.
  buildBox :: a -> Radiant -> Either (Opposite a) Core -> BuiltBox a
  -- ^ Builds boxes of type 'BuiltBox'.
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

-- | A count of columns
newtype Width = Width Int
  deriving (Eq, Ord, Show)

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
    Right (_, Width w) -> w

instance HasHeight Core where
  height (Core ei) = case ei of
    Left _ -> 1
    Right (Height h, _) -> h

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
data BoxVP = BoxVP (Align Horiz) Radiant (Either BoxH Core)

instance HasHeight BoxVP where
  height (BoxVP _ _ ei) = either height height ei

instance HasWidth BoxVP where
  width (BoxVP _ _ ei) = either width width ei

instance LeftRight BoxVP where
  port (BoxVP a _ ei) = case a of
    NonCenter ALeft -> 0
    NonCenter ARight -> either width width ei
    Center -> fst . split $ either width width ei

  starboard (BoxVP a _ ei) = case a of
    NonCenter ALeft -> either width width ei
    NonCenter ARight -> 0
    Center -> snd . split $ either width width ei

-- | Payload for a box that is oriented around a horizontal axis.
data BoxHP = BoxHP (Align Vert) Radiant (Either BoxV Core)

instance HasHeight BoxHP where
  height (BoxHP _ _ ei) = either height height ei

instance HasWidth BoxHP where
  width (BoxHP _ _ ei) = either width width ei

instance UpDown BoxHP where
  above (BoxHP a _ ei) = case a of
    NonCenter ATop -> 0
    NonCenter ABottom -> either height height ei
    Center -> fst . split $ either height height ei

  below (BoxHP a _ ei) = case a of
    NonCenter ATop -> either height height ei
    NonCenter ABottom -> 0
    Center -> snd . split $ either height height ei

-- | A 'BoxV' box contains zero or more blocks.  Each block is
-- oriented relative to a single vertical axis.  Blocks are aligned
-- verically along this axis, rather like a flagpole.  'BoxV' can be
-- combined using the 'Monoid' functions.  When you are done adding
-- blocks to the 'BoxV' by combining separate 'BoxV', you can convert
-- it to a 'BoxH' using 'convertBox'.
newtype BoxV = BoxV (Seq BoxVP)

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
-- can convert it to a single 'BoxV' using 'convertBox'.
newtype BoxH = BoxH (Seq BoxHP)

instance Monoid BoxH where
  mempty = BoxH mempty
  mappend (BoxH x) (BoxH y) = BoxH $ x <> y

instance Box BoxH where
  makeBox bx@(BoxH sqnce) = mergeHoriz $ fmap equalize sqnce
    where
      maxTop = above bx
      maxBot = below bx
      w = width bx
      mergeHoriz sqn = case viewl sqn of
        EmptyL -> Seq.empty
        x :< xs -> F.foldl' comb x xs
        where
          comb acc sq = Seq.zipWith (<>) acc sq
      equalize bhp@(BoxHP _ rd ei) = tp <> this <> bot
        where
          this = either makeBox (fmap Seq.singleton $ rodsFromCore rd) ei
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
  makeBox bx@(BoxV sqnce) = mergeVert $ fmap equalize sqnce
    where
      mergeVert = F.foldl' (<>) Seq.empty
      equalize (BoxVP a rd ei) = fmap addLeftRight this
        where
          this = either makeBox (fmap Seq.singleton $ rodsFromCore rd) ei
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

instance Alignment (Align Vert) where
  type BuiltBox (Align Vert) = BoxH
  type Opposite (Align Vert) = BoxV
  buildBox a r ei = BoxH . Seq.singleton $
    BoxHP a r ei
  segment r i = BoxH . Seq.singleton $
    BoxHP (NonCenter ATop) r (Right . Core . Right $
      (Height 0, Width i))

instance Alignment (Align Horiz) where
  type BuiltBox (Align Horiz) = BoxV
  type Opposite (Align Horiz) = BoxH
  buildBox a r ei = BoxV . Seq.singleton $
    BoxVP a r ei
  segment r i = BoxV . Seq.singleton $
    BoxVP (NonCenter ALeft) r (Right . Core . Right $
      (Height i, Width 0))

-- | Construct a box from a single 'Chunk'.  Either a 'BoxH' or a
-- 'BoxV' will be built depending on the return type of the function.
blockFromChunk
  :: Alignment a
  => a
  -> Radiant
  -> Chunk
  -> BuiltBox a
blockFromChunk a r c = buildBox a r (Right (Core (Left c)))

-- | Construct a blank box.  Useful for adding in background spacers.
-- For a function that builds one-dimensional boxes, see 'segment',
-- which often is all you need if you are making a blank box to
-- separate other boxes.
blankBlock
  :: Alignment a
  => a
  -> Radiant
  -> Height
  -> Width
  -> BuiltBox a
blankBlock a r h w = buildBox a r (Right (Core (Right (h, w))))

-- | Converts a 'BoxH' to a 'BoxV', and a 'BoxV' to a 'BoxH'.  Useful
-- when combining boxes into larger boxes.
convertBox
  :: Alignment a
  => a
  -> Radiant
  -> Opposite a
  -> BuiltBox a
convertBox a r o = buildBox a r (Left o)

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
type Rows = RowsCols (Align Vert)

-- | A set of columns; each column must appear in
-- left-to-right order.
type Columns = RowsCols (Align Horiz)

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
