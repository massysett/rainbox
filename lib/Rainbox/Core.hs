{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Map as M

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

-- | Class for alignments; both 'Vert' and 'Horiz' are
-- instances of this class.
class Alignment a where
  type BuiltBox a
  -- ^ The type of box that is constructed from values of this
  -- alignment.
  type Opposite a
  -- ^ The opposite type of 'BuiltBox'.
  convert :: Align a -> Radiant -> Opposite a -> BuiltBox a
  -- ^ Wraps a 'BoxH' and places it in a 'BoxV', and vice-versa.

  wrap :: Align a -> Radiant -> BuiltBox a -> BuiltBox a
  -- ^ Wraps a 'BoxV' inside of a new 'BoxV', or wraps a 'BoxH' inside
  -- of a new 'BoxH'.

  fromCore :: Align a -> Radiant -> Core -> BuiltBox a
  -- ^ Creates a 'BoxV' or a 'BoxH' from a 'Core'.

  spacer :: Radiant -> Int -> BuiltBox a
  -- ^ Builds a one-dimensional box of the given size; its single
  -- dimension is on the same alignment as the axis.  When added to a
  -- box, it will insert blank space of the given length.  For a
  -- 'BoxH', this produces a horizontal line; for a 'BoxV', a vertical
  -- line.

  spreader :: Align a -> Int -> BuiltBox a
  -- ^ Builds a one-dimensional box of the given size; its single
  -- dimension is perpendicular to the axis.  This can be used to make
  -- a 'BoxV' wider or a 'BoxH' taller.

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
-- verically along this axis.  'BoxV' can be combined using the
-- 'Monoid' functions.  When you are done adding blocks to the 'BoxV'
-- by combining separate 'BoxV', you can convert it to a 'BoxH' using
-- 'convert'.
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
-- horizontally along this axis.  'BoxH' can be combined using the
-- 'Monoid' functions.  When you are done adding blocks to the 'BoxH'
-- by combining separate 'BoxH', you can convert it to a single 'BoxV'
-- using 'convert'.
newtype BoxH = BoxH (Seq PayloadH)

instance Monoid BoxH where
  mempty = BoxH mempty
  mappend (BoxH x) (BoxH y) = BoxH $ x <> y

instance Box BoxH where
  makeBox bx@(BoxH sqnce) = mergeVert $ fmap eqlize sqnce
    where
      maxTop = above bx
      maxBot = below bx
      w = width bx
      mergeVert sqn = case viewl sqn of
        EmptyL -> Seq.empty
        x :< xs -> F.foldl' comb x xs
        where
          comb acc sq = Seq.zipWith (<>) acc sq
      eqlize bhp@(PayloadH _ rd s3) = tp <> this <> bot
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
  makeBox bx@(BoxV sqnce) = mergeHoriz $ fmap eqlize sqnce
    where
      mergeHoriz = F.foldl' (<>) Seq.empty
      eqlize (PayloadV a rd s3) = fmap addLeftRight this
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
  spacer r i = BoxH . Seq.singleton $
    PayloadH (NonCenter ATop) r (S3c . Core . Right $
      (Height 0, Width (max 0 i)))
  spreader a i = BoxH . Seq.singleton $
    PayloadH a noColorRadiant (S3c . Core . Right $
      (Height (max 0 i), Width 0))

instance Alignment Vert where
  type BuiltBox Vert = BoxV
  type Opposite Vert = BoxH
  convert a r b = BoxV . Seq.singleton $
    PayloadV a r (S3b b)
  wrap a r b = BoxV . Seq.singleton $
    PayloadV a r (S3a b)
  fromCore a r c = BoxV . Seq.singleton $
    PayloadV a r (S3c c)
  spacer r i = BoxV . Seq.singleton $
    PayloadV (NonCenter ALeft) r (S3c . Core . Right $
      (Height (max 0 i), Width 0))
  spreader a i = BoxV . Seq.singleton $
    PayloadV a noColorRadiant (S3c . Core . Right $
      (Height 0, Width (max 0 i)))

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
-- For a function that build one-dimensional boxes, see 'spacer' and
-- 'spreader'.
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

data Cell = Cell
  { cellRows :: Seq (Seq Chunk)
  , cellHoriz :: Align Horiz
  , cellVert :: Align Vert
  , cellBackground :: Radiant
  }

emptyCell :: Cell
emptyCell = Cell Seq.empty center center noColorRadiant

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
