{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedLists #-}
module Rainbox.Category where

import Rainbow
import Control.Monad (join)
import Data.Monoid
import Rainbow.Types (Chunk(..))
import Data.Sequence (Seq, ViewL(..), viewl, (|>))
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Text as X
import GHC.Exts (IsList(..))

-- | Alignment.
data Align a = Center | NonCenter a
  deriving (Eq, Show)

-- | Vertical alignment.
data Vert = ATop | ABottom
  deriving (Eq, Show)

-- | Horizontal alignment.
data Horiz = ALeft | ARight
  deriving (Eq, Show)

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

class Box a where
  makeBox :: a -> Seq (Seq Rod)

newtype Rod = Rod (Either (Int, Radiant) Chunk)

newtype Core = Core (Either Chunk (Height, Width))

rodsFromCore :: Radiant -> Core -> Seq Rod
rodsFromCore rd (Core ei) = case ei of
  Left ck -> Seq.singleton . Rod . Right $ ck
  Right (Height h, Width w) -> Seq.replicate h . Rod . Left $ (w, rd)

rodsLength :: Seq Rod -> Int
rodsLength = F.sum . fmap toLen
  where
    toLen (Rod ei) = case ei of
      Left (i, _) -> i
      Right (Chunk _ xs) -> F.sum . fmap X.length $ xs

chunksFromRods :: Seq (Seq Rod) -> Seq (Seq Chunk)
chunksFromRods = fmap (|> "\n") . fmap (fmap chunkFromRod)
  where
    chunkFromRod (Rod ei) = case ei of
      Left (i, r) -> (chunkFromText . X.replicate i $ " ") <> back r
      Right c -> c

render :: Box a => a -> [Chunk]
render = F.toList . join . chunksFromRods . makeBox

instance HasWidth Core where
  width (Core ei) = case ei of
    Left (Chunk _ t) -> F.sum . fmap X.length $ t
    Right (_, Width w) -> w

instance HasHeight Core where
  height (Core ei) = case ei of
    Left _ -> 1
    Right (Height h, _) -> h

class LeftRight a where
  port :: a -> Int
  starboard :: a -> Int

class UpDown a where
  above :: a -> Int
  below :: a -> Int


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
      equalize bhp@(BoxHP _ rd ei) = top <> this <> bot
        where
          this = either makeBox (fmap Seq.singleton $ rodsFromCore rd) ei
          top = Seq.replicate (max 0 (maxTop - above bhp)) pad
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

class Alignment a where
  type BuiltBox a
  type Opposite a
  buildBox :: a -> Radiant -> Either (Opposite a) Core -> BuiltBox a
  segment :: Radiant -> Int -> BuiltBox a

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

fromChunk
  :: Alignment a
  => a
  -> Radiant
  -> Chunk
  -> BuiltBox a
fromChunk a r c = buildBox a r (Right (Core (Left c)))

blank
  :: Alignment a
  => a
  -> Radiant
  -> Height
  -> Width
  -> BuiltBox a
blank a r h w = buildBox a r (Right (Core (Right (h, w))))

convert
  :: Alignment a
  => a
  -> Radiant
  -> Opposite a
  -> BuiltBox a
convert a r o = buildBox a r (Left o)

newtype CellRow = Row (Seq Chunk)
  deriving (Eq, Ord, Show)

instance IsList CellRow where
  type Item CellRow = Chunk
  fromList = Row . Seq.fromList
  toList (Row sq) = F.toList sq

newtype Cell = Cell (Seq CellRow)
  deriving (Eq, Ord, Show)

instance IsList Cell where
  type Item Cell = CellRow
  fromList = Cell . Seq.fromList
  toList (Cell sq) = F.toList sq

newtype RowCol a = RowCol (Seq (a, Radiant, Cell))
  deriving (Eq, Ord, Show)

instance IsList (RowCol a) where
  type Item (RowCol a) = (a, Radiant, Cell)
  fromList = RowCol . Seq.fromList
  toList (RowCol sq) = F.toList sq

type Row = RowCol
type Column = RowCol

newtype RowsCols a = RowsCols (Seq (RowCol a))
  deriving (Eq, Ord, Show)

instance IsList (RowsCols a) where
  type Item (RowsCols a) = RowCol a
  fromList = RowsCols . Seq.fromList
  toList (RowsCols sq) = F.toList sq

type Rows = RowsCols (Align Vert)
type Columns = RowsCols (Align Horiz)

table
  :: Alignment a
  => RowsCols a
  -> BuiltBox a
table = undefined

tableByRows
  :: Rows
  -> BoxH
tableByRows = table

tableByColumns
  :: Columns
  -> BoxV
tableByColumns = table


-- | Split a number into two parts, so that the sum of the two parts
-- is equal to the original number.
split :: Int -> (Int, Int)
split i = (r, r + rm)
  where
    (r, rm) = i `quotRem` 2
