module Rainbox.Category where

import Control.Applicative
import Rainbow
import Rainbow.Types (Chunk(..))
import Data.Sequence (Seq, ViewL(..), viewl, (<|), (|>))
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Text as X

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

-- | A count of columns
newtype Width = Width Int
  deriving (Eq, Ord, Show)

data Payload
  = Filled Chunk
  | Blank Height Width
  | CatH Box Box (Align Vert)
  | CatV Box Box (Align Horiz)
  | ViewV Box (Align Vert)
  | ViewH Box (Align Horiz)

data Box = Box (Maybe Radiant) Payload

newtype Rod = Rod (Either (Radiant, Int) Chunk)

calcBox :: Maybe Radiant -> Box -> Seq (Seq Rod)
calcBox ctxtBack (Box thisBack pld) = case pld of
  Filled ck -> Seq.singleton (Seq.singleton . Rod . Right $ ck)
  Blank (Height h) (Width w) -> Seq.replicate (max 0 h)
    (Seq.singleton . Rod . Left $ (bck, w))

  CatH l r a -> mergeHoriz l' r'
    where
      lBox = calcBox mayBack l
      rBox = calcBox mayBack r
      (l', r')
        | diff == 0 = (lBox, rBox)
        | diff > 0 = (lBox, padHoriz bck a diff rBox)
        | otherwise = (padHoriz bck a (negate diff) lBox, rBox)
      diff = Seq.length lBox - Seq.length rBox

  CatV t b a -> t' <> b'
    where
      tBox = calcBox mayBack t
      bBox = calcBox mayBack b
      (t', b')
        | diff == 0 = (tBox, bBox)
        | diff > 0 = (tBox, padVert bck a diff bBox)
        | otherwise = (padVert bck a (negate diff) tBox, bBox)
      diff = Seq.length tBox - Seq.length bBox

  where
    mayBack = thisBack <|> ctxtBack
    bck = maybe noColorRadiant id mayBack

mergeHoriz :: Seq (Seq Rod) -> Seq (Seq Rod) -> Seq (Seq Rod)
mergeHoriz x y = Seq.zipWith (<>) x y

width :: Seq (Seq Rod) -> Int
width sq = case viewl sq of
  EmptyL -> 0
  x :< _ -> F.sum . fmap toWidth $ x
    where
      toWidth (Rod ei) = case ei of
        Left (_, i) -> i
        Right (Chunk _ ts) -> F.sum . fmap X.length $ ts

padHoriz
  :: Radiant
  -> Align Vert
  -> Int
  -> Seq (Seq Rod)
  -> Seq (Seq Rod)
padHoriz bk a hght sq = (padTop <| sq) |> padBot
  where
    (nTop, nBot) = case a of
      Center -> split hght
      NonCenter ATop -> (0, hght)
      NonCenter ABottom -> (hght, 0)
    padTop = Seq.replicate nTop . Rod . Left $ (bk, w)
    padBot = Seq.replicate nBot . Rod . Left $ (bk, w)
    w = width sq

padVert
  :: Radiant
  -> Align Horiz
  -> Int
  -> Seq (Seq Rod)
  -> Seq (Seq Rod)
padVert bk a wdth sqnce = fmap padder sqnce
  where
    (nLeft, nRight)
      | wdth < 1 = (0,0)
      | otherwise = case a of
          Center -> split wdth
          NonCenter ALeft -> (0, wdth)
          NonCenter ARight -> (wdth, 0)
    padder = addPad (<|) nLeft . addPad (flip (|>)) nRight
    addPad adder i
      | i < 1 = id
      | otherwise = adder (Rod . Left $ (bk, i))
    


-- | Split a number into two parts, so that the sum of the two parts
-- is equal to the original number.
split :: Int -> (Int, Int)
split i = (r, r + rm)
  where
    (r, rm) = i `quotRem` 2
