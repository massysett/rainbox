{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Rainbox.Core where

import Rainbow
import Control.Monad (join)
import Data.Monoid
import Rainbow.Types (Chunk(..))
import Data.Sequence (Seq, ViewL(..), viewl, (|>), (<|))
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Text as X
import qualified Data.Map as M

-- # Alignment

-- | Alignment.
data Alignment a = Center | NonCenter a
  deriving (Eq, Show)

-- # Width and height

-- | A count of rows
newtype Height = Height Int
  deriving (Eq, Ord, Show)

-- | A count of columns
newtype Width = Width Int
  deriving (Eq, Ord, Show)

class HasHeight a where
  height :: a -> Int

instance (HasHeight a, HasHeight b) => HasHeight (Either a b) where
  height = either height height

class HasWidth a where
  width :: a -> Int

instance HasWidth Chunk where
  width (Chunk _ ts) = F.sum . fmap X.length $ ts

instance (HasWidth a, HasWidth b) => HasWidth (Either a b) where
  width = either width width

-- # Core

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

-- # Rods

-- | An intermediate type used in rendering; it consists either of
-- text 'Chunk' or of a number of spaces.
newtype Rod = Rod (Either (Int, Radiant) Chunk)

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

instance HasWidth Rod where
  width (Rod ei) = case ei of
    Left (i, _) -> max 0 i
    Right c -> width c

-- # RodRows

newtype RodRows = RodRows (Seq (Seq Rod))

instance HasHeight RodRows where
  height (RodRows sq) = Seq.length sq

instance HasWidth RodRows where
  width (RodRows sq) = F.foldl' max 0 . fmap (F.sum . fmap width) $ sq

-- # Payload

data Payload a = Payload (Alignment a) Radiant (Either RodRows Core)

instance HasWidth (Payload a) where
  width (Payload _ _ ei) = width ei

instance HasHeight (Payload a) where
  height (Payload _ _ ei) = height ei

-- # Box

newtype Box a = Box (Seq (Payload a))

instance Monoid (Box a) where
  mempty = Box Seq.empty
  mappend (Box x) (Box y) = Box (x <> y)

-- # Horizontal and vertical

data Horizontal = ATop | ABottom
  deriving (Eq, Show)

data Vertical = ALeft | ARight
  deriving (Eq, Show)

-- | Place this block so that it is centered on the vertical axis or
-- horizontal axis.
center :: Alignment a
center = Center

-- | Place this block's left edge on the vertical axis.
left :: Alignment Vertical
left = NonCenter ALeft

-- | Place this block's right edge on the vertical axis.
right :: Alignment Vertical
right = NonCenter ARight

-- | Place this box's top edge on the horizontal axis.
top :: Alignment Horizontal
top = NonCenter ATop

-- | Place this box's bottom edge on the horizontal axis.
bottom :: Alignment Horizontal
bottom = NonCenter ABottom


-- # Orientation

class Orientation a where
  rods :: Box a -> Seq (Seq Rod)

  spacer :: Radiant -> Int -> Box a
  -- ^ Builds a one-dimensional box of the given size; its single
  -- dimension is on the same alignment as the axis.  When added to a
  -- box, it will insert blank space of the given length.  For a 'Box'
  -- 'Horizontal', this produces a horizontal line; for a 'Box'
  -- 'Vertical', a vertical line.

  spreader :: Alignment a -> Int -> Box a
  -- ^ Builds a one-dimensional box of the given size; its single
  -- dimension is perpendicular to the axis.  This can be used to make
  -- a 'Box' 'Vertical' wider or a 'Box' 'Horizontal' taller.

instance Orientation Vertical where
  rods bx@(Box sqnce) = mergeHoriz $ fmap eqlize sqnce
    where
      mergeHoriz = F.foldl' (<>) Seq.empty
      eqlize (Payload a rd ei) = fmap addLeftRight this
        where
          this = either (\(RodRows sq) -> sq)
            (fmap Seq.singleton $ rodsFromCore rd) ei
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

  spacer r i = Box . Seq.singleton $
    Payload (NonCenter ALeft) r (Right . Core . Right $
      (Height (max 0 i), Width 0))
  spreader a i = Box . Seq.singleton $
    Payload a noColorRadiant (Right . Core . Right $
      (Height 0, Width (max 0 i)))

instance Orientation Horizontal where
  rods bx@(Box sqnce) = mergeVert $ fmap eqlize sqnce
    where
      maxTop = above bx
      maxBot = below bx
      w = width bx
      mergeVert sqn = case viewl sqn of
        EmptyL -> Seq.empty
        x :< xs -> F.foldl' comb x xs
        where
          comb acc sq = Seq.zipWith (<>) acc sq
      eqlize bhp@(Payload _ rd ei) = tp <> this <> bot
        where
          this = either (\(RodRows sq) -> sq)
            (fmap Seq.singleton $ rodsFromCore rd) ei
          tp = Seq.replicate (max 0 (maxTop - above bhp)) pad
          bot = Seq.replicate (max 0 (maxBot - below bhp)) pad
          pad = Seq.singleton . Rod . Left $ (w, rd)

  spacer r i = Box . Seq.singleton $
    Payload (NonCenter ATop) r (Right . Core . Right $
      (Height 0, Width (max 0 i)))
  spreader a i = Box . Seq.singleton $
    Payload a noColorRadiant (Right . Core . Right $
      (Height (max 0 i), Width 0))

-- # port, starboard, above, below


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


instance LeftRight (Payload Vertical) where
  port (Payload a _ ei) = case a of
    NonCenter ALeft -> 0
    NonCenter ARight -> width ei
    Center -> fst . split . width $ ei

  starboard (Payload a _ s3) = case a of
    NonCenter ALeft -> width s3
    NonCenter ARight -> 0
    Center -> snd . split . width $ s3

instance UpDown (Payload Horizontal) where
  above (Payload a _ s3) = case a of
    NonCenter ATop -> 0
    NonCenter ABottom -> height s3
    Center -> fst . split . height $ s3

  below (Payload a _ s3) = case a of
    NonCenter ATop -> height s3
    NonCenter ABottom -> 0
    Center -> snd . split . height $ s3

instance LeftRight (Box Vertical) where
  port (Box sq) = F.foldl' max 0 . fmap port $ sq
  starboard (Box sq) = F.foldl' max 0 . fmap starboard $ sq

instance HasWidth (Box Vertical) where
  width b = port b + starboard b

instance HasHeight (Box Vertical) where
  height (Box sq) = F.sum . fmap height $ sq

instance UpDown (Box Horizontal) where
  above (Box sq) = F.foldl' max 0 . fmap above $ sq
  below (Box sq) = F.foldl' max 0 . fmap below $ sq

instance HasHeight (Box Horizontal) where
  height b = above b + below b

instance HasWidth (Box Horizontal) where
  width (Box sq) = F.sum . fmap width $ sq

-- # Box construction

-- | Construct a box from a single 'Chunk'.
fromChunk
  :: Alignment a
  -> Radiant
  -- ^ Background color.  The background color in the 'Chunk' is not
  -- changed; this background is used if the 'Payload' must be padded
  -- later on.
  -> Chunk
  -> Box a
fromChunk a r = Box . Seq.singleton . Payload a r  . Right . Core . Left

-- | Construct a blank box.  Useful for adding in background spacers.
-- For functions that build one-dimensional boxes, see 'spacer' and
-- 'spreader'.
blank
  :: Alignment a
  -> Radiant
  -- ^ Color for the blank area.
  -> Height
  -> Width
  -> Box a
blank a r h w =
  Box . Seq.singleton . Payload a r . Right . Core . Right $ (h, w)

-- # Box rendering

-- | Convert a box to a 'Seq' of 'Chunk' in preparation for rendering.
-- Use 'F.toList' to convert the 'Seq' of 'Chunk' to a list so that
-- you can print it using the functions in "Rainbow".
render :: Orientation a => Box a -> Seq Chunk
render = join . chunksFromRods . rods


-- # Tables

data Cell = Cell
  { cellRows :: Seq (Seq Chunk)
  , cellHoriz :: Alignment Horizontal
  , cellVert :: Alignment Vertical
  , cellBackground :: Radiant
  }

emptyCell :: Cell
emptyCell = Cell Seq.empty center center noColorRadiant

{-
-- Cells by row:
-- 0. Ensure each row is equal length
-- 1. Create one BoxV for each cell
-- 2. Create widest cell map
-- 3. Pad each BoxV to appropriate width, using cellVert alignment
-- 4. Convert each BoxV to BoxH, using cellHoriz and cellBackground
-- 5. mconcatSeq each row
-- 6. Convert each row to BoxV; use default background
--    and center alignment
-- 7. mconcatSeq the rows

tableByRows :: Seq (Seq Cell) -> BoxV
tableByRows
  = mconcatSeq
  . fmap rowToBoxV
  . fmap mconcatSeq
  . fmap (fmap toBoxH)
  . uncurry padBoxV
  . addWidthMap
  . fmap (fmap cellToBoxV)
  . equalize emptyCell

rowToBoxV :: BoxH -> BoxV
rowToBoxV bv = convert left noColorRadiant bv

cellToBoxV :: Cell -> (BoxV, Align Horiz, Radiant)
cellToBoxV (Cell rs ah av rd) = (bx, ah, rd)
  where
    bx = mconcatSeq
       . fmap (convert av rd)
       . fmap (mconcatSeq . fmap (fromChunk top rd))
       $ rs

toBoxH
  :: (BoxV, Align Horiz, Radiant)
  -> BoxH
toBoxH (bv, ah, rd) = convert ah rd bv

addWidthMap
  :: Seq (Seq (BoxV, b, c))
  -> (M.Map Int (Int, Int), Seq (Seq (BoxV, b, c)))
addWidthMap sqnce = (m, sqnce)
  where
    m = widestCellMap . fmap (fmap (\(a, _, _) -> a)) $ sqnce

padBoxV
  :: M.Map Int (Int, Int)
  -> Seq (Seq (BoxV, a, b))
  -> Seq (Seq (BoxV, a, b))
padBoxV mp = fmap (Seq.mapWithIndex f)
  where
    f idx (bx, a, b) = (bx <> padLeft <> padRight, a, b)
      where
        (lenL, lenR) = mp M.! idx
        padLeft = spreader right lenL
        padRight = spreader left lenR


widestCellMap :: Seq (Seq BoxV) -> M.Map Int (Int, Int)
widestCellMap = F.foldl' outer M.empty
  where
    outer mpOuter = Seq.foldlWithIndex inner mpOuter
      where
        inner mpInner idx bx = case M.lookup idx mpInner of
          Nothing -> M.insert idx (port bx, starboard bx) mpInner
          Just (pOld, sOld) -> M.insert idx
            (max pOld (port bx), max sOld (starboard bx)) mpInner

-- Table by columns:
--
-- 0.  Equalize columns
-- 1.  Create one BoxH for each cell
-- 2.  Create tallest cell map
-- 3.  Pad each BoxH to appropriate height, using cellHeight alignment
-- 4.  Convert each BoxH to BoxV, using cellVert and cellBackground
-- 5.  mconcatSeq each column
-- 6.  Convert each column to BoxH
-- 7.  mconcatSeq the columns

tableByColumns :: Seq (Seq Cell) -> BoxH
tableByColumns
  = mconcatSeq
  . fmap rowToBoxH
  . fmap mconcatSeq
  . fmap (fmap toBoxV)
  . uncurry padBoxH
  . addHeightMap
  . fmap (fmap cellToBoxH)
  . equalize emptyCell


rowToBoxH :: BoxV -> BoxH
rowToBoxH bv = convert top noColorRadiant bv


cellToBoxH :: Cell -> (BoxH, Align Vert, Radiant)
cellToBoxH (Cell rs ah av rd) = (bx, av, rd)
  where
    bx = convert ah rd
       . mconcatSeq
       . fmap (convert av rd)
       . fmap (mconcatSeq . fmap (fromChunk top rd))
       $ rs

addHeightMap
  :: Seq (Seq (BoxH, b, c))
  -> (M.Map Int (Int, Int), Seq (Seq (BoxH, b, c)))
addHeightMap sqnce = (m, sqnce)
  where
    m = tallestCellMap . fmap (fmap (\(a, _, _) -> a)) $ sqnce

tallestCellMap :: Seq (Seq BoxH) -> M.Map Int (Int, Int)
tallestCellMap = F.foldl' outer M.empty
  where
    outer mpOuter = Seq.foldlWithIndex inner mpOuter
      where
        inner mpInner idx bx = case M.lookup idx mpInner of
          Nothing -> M.insert idx (above bx, below bx) mpInner
          Just (aOld, bOld) -> M.insert idx
            (max aOld (above bx), max bOld (below bx)) mpInner


padBoxH
  :: M.Map Int (Int, Int)
  -> Seq (Seq (BoxH, a, b))
  -> Seq (Seq (BoxH, a, b))
padBoxH mp = fmap (Seq.mapWithIndex f)
  where
    f idx (bx, a, b) = (bx <> padTop <> padBot, a, b)
      where
        (lenT, lenB) = mp M.! idx
        padTop = spreader bottom lenT
        padBot = spreader top lenB


toBoxV
  :: (BoxH, Align Vert, Radiant)
  -> BoxV
toBoxV (bh, av, rd) = convert av rd bh


-- | Ensures that each inner 'Seq' is the same length by adding the
-- given empty element where needed.
equalize :: a -> Seq (Seq a) -> Seq (Seq a)
equalize emp sqnce = fmap adder sqnce
  where
    maxLen = F.foldl' max 0 . fmap Seq.length $ sqnce
    adder sq = sq <> pad
      where
        pad = Seq.replicate (max 0 (maxLen - Seq.length sq)) emp

mconcatSeq :: Monoid a => Seq a -> a
mconcatSeq = F.foldl' (<>) mempty
-}

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

