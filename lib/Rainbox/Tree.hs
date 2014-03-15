module Rainbox.Tree where

import System.Console.Rainbow
import System.Console.Rainbow.Colors
import Data.Monoid
import qualified Data.Text as X

class HasHeight a where
  height :: a -> Int

class HasWidth a where
  width :: a -> Int

instance HasWidth Chunk where
  width = X.length . text

instance HasHeight Chunk where
  height = const 1


-- | Background colors to use when inserting necessary padding.
data Background = Background
  { background8 :: Color8
  , background256 :: Color256
  } deriving (Eq, Show)

data Blob = Blob
  { opaque :: Bool
  , blob :: Chunk
  } deriving (Eq, Show)

instance HasWidth Blob where
  width = width . blob

instance HasHeight Blob where
  height = height . blob

data Bar = Bar { unBar :: [Blob] } deriving (Eq, Show)

instance HasWidth Bar where
  width = sum . map width . unBar

instance HasHeight Bar where
  height = const 1

data Word = Word { unWord :: [Blob] } deriving (Eq, Show)

instance HasWidth Word where
  width = sum . map width . unWord

instance HasHeight Word where
  height = const 1

-- | Alignment.
data Align a = Center | NonCenter a
  deriving (Eq, Show)

-- | Vertical alignment.
data Vert = ATop | ABottom
  deriving (Eq, Show)

-- | Horizontal alignment.
data Horiz = ALeft | ARight
  deriving (Eq, Show)

center :: Align a
center = Center

top :: Align Vert
top = NonCenter ATop

bottom :: Align Vert
bottom = NonCenter ABottom

left :: Align Horiz
left = NonCenter ALeft

right :: Align Horiz
right = NonCenter ARight

data Size = Auto | Manual Int deriving (Eq, Show)

newtype Bars = Bars { unBars :: [Bar] } deriving (Eq, Show)

instance HasWidth Bars where
  width = maximum . (0:) . map width . unBars

instance HasHeight Bars where
  height = length . unBars

data Payload
  = PBars Bars
  | Words [Word]
  deriving (Eq, Show)

data Cell = Cell
  { payload :: Payload
  , sizeH :: Size
  , sizeV :: Size
  } deriving (Eq, Show)

instance HasWidth Cell where
  width c = case sizeH c of
    Manual i -> max 0 i
    Auto -> case payload c of
      PBars bs -> width bs
      Words ws -> width . wrapWords (sizeH c) (sizeV c) $ ws

instance HasHeight Cell where
  height c = case sizeV c of
    Manual i -> max 0 i
    Auto -> case payload c of
      PBars bs -> height bs
      Words ws -> height . wrapWords (sizeH c) (sizeV c) $ ws

wrapWords
  :: Size
  -- ^ Horizontal size
  -> Size
  -- ^ Vertical size
  -> [Word]
  -> Bars
wrapWords = undefined

-- | The basic building block.  A 'Box' contains either a 'Cell',
-- which contains textual data, or a 'Container', which contains
-- other 'Cell'.  A 'Box' is always rectangular.  Its dimensions are
-- determined by its contents.
--
-- If the 'Box' contains a 'Cell', its dimensions are determined by
-- reference to the 'sizeH' and 'sizeV' if the 'Cell'.
--
-- If the 'Box' contains a horizontal 'Container', its width is the
-- sum of the widths of its children and its height is the height of
-- the tallest child.  If the 'Box' contains a vertical 'Container',
-- its width is the width of the widest child and its height is the
-- sum of the heights of all children.
data Box a = Box
  { contents :: Either Cell (Container a)
  , attributes :: Attributes a
  } deriving (Eq, Show)

instance HasWidth (Box a) where
  width = either width width . contents

instance HasHeight (Box a) where
  height = either height height . contents

data Attributes a = Attributes
  { alignH :: Align Horiz
  , alignV :: Align Vert
  , background :: Background
  , meta :: a
  } deriving (Eq, Show)

data Orientation = OHoriz | OVert deriving (Eq, Show)

type Forest a = [Box a]

data Container a = Container
  { orientation :: Orientation
  , children :: Forest a
  } deriving (Eq, Show)

instance HasWidth (Container a) where
  width c = case orientation c of
    OHoriz -> sum . map width . children $ c
    OVert -> maximum . (0:) . map width . children $ c

instance HasHeight (Container a) where
  height c = case orientation c of
    OHoriz -> maximum . (0:) . map height . children $ c
    OVert -> sum . map height . children $ c

--
-- Rendering
--

blobToChunk :: Background -> Blob -> Chunk
blobToChunk bk bl
  | opaque bl = blob bl
  | otherwise = blob bl <> bc8 (background8 bk)
                        <> bc256 (background256 bk)

-- | A 'Rod' is a 'Bar' after necessary background coloring is
-- performed.  It still is not padded.
newtype Rod = Rod { unRod :: [Chunk] }
  deriving (Eq, Show)

barToRod :: Background -> Bar -> Rod
barToRod bk = Rod . map (blobToChunk bk) . unBar

data Pad = Pad
  { padSize :: Int
  , padBackground :: Background
  } deriving (Eq, Show)

instance HasWidth Pad where
  width = max 0 . padSize

instance HasHeight Pad where
  height = const 1

-- | A 'Clatch' is a 'Rod' with necessary padding.
data Clatch = Clatch
  { leftPad :: [Pad]
  , clatchChunks :: [Chunk]
  , rightPad :: [Pad]
  } deriving (Eq, Show)

instance HasWidth Clatch where
  width c =
    sum (map width . leftPad $ c)
    + sum (map width . clatchChunks $ c)
    + sum (map width . rightPad $ c)

instance HasHeight Clatch where
  height = const 1

-- | A set of 'Clatch'es.  Every 'Clatch' inside will have the same
-- number of characters.

data Clatches = Clatches
  { ctsClatches :: [Clatch]
  , ctsWidth :: Int
  } deriving (Eq, Show)

instance HasWidth Clatches where
  width = sum . map width . ctsClatches

instance HasHeight Clatches where
  height = length . ctsClatches

cellToClatches :: Cell -> Attributes a -> Clatches
cellToClatches = undefined

containerToClatches :: Container a -> Attributes a -> Clatches
containerToClatches c a = case orientation c of
  OHoriz -> mergeHoriz
    . map (padVert (background a) h' (alignV a))
    . map boxToClatches
    . children $ c
    where
      h' = maximum . (0:) . map height . children $ c
  OVert -> mergeVert
    . map (padHoriz (background a) w' (alignH a))
    . map boxToClatches
    . children $ c
    where
      w' = maximum . (0:) . map width . children $ c

mergeVert :: [Clatches] -> Clatches
mergeVert cs = Clatches ls' w
  where
    ls' = concat . map ctsClatches $ cs
    w = case cs of
      [] -> 0
      x:_ -> ctsWidth x

mergeHoriz :: [Clatches] -> Clatches
mergeHoriz cs = Clatches ls' w
  where
    w = sum . map ctsWidth $ cs
    ls' = foldr (zipWith merge) (repeat (Clatch [] [] []))
            (map ctsClatches cs)
    merge c1 c2 = Clatch
      { leftPad = leftPad c1 ++ leftPad c2
      , clatchChunks = clatchChunks c1 ++ clatchChunks c2
      , rightPad = rightPad c1 ++ rightPad c2
      }

padVert :: Background -> Int -> Align Vert -> Clatches -> Clatches
padVert b i v (Clatches cs w) = Clatches (tp ++ cs ++ bt) w
  where
    pad = Clatch [Pad w b] [] []
    tp = replicate nTop pad
    bt = replicate nBot pad
    diff = max 0 $ i - length cs
    (nTop, nBot)
      | diff <= 0 = (0, 0)
      | v == top = (0, diff)
      | v == bottom = (diff, 0)
      | otherwise = split diff

padHoriz :: Background -> Int -> Align Horiz -> Clatches -> Clatches
padHoriz b i h (Clatches cs w) = Clatches cs' w'
  where
    (w', cs')
      | diff <= 0 = (w, cs)
      | otherwise = (i, map doPad cs)
    diff = max 0 $ i - w
    doPad cltch = cltch { leftPad = lpad ++ leftPad cltch
                        , rightPad = rightPad cltch ++ rpad
                        }
    (lpad, rpad)
      | h == left = ([], [Pad diff b])
      | h == right = ([Pad diff b], [])
      | otherwise = ([Pad l b], [Pad r b])
      where
        (l, r) = split diff

boxToClatches :: Box a -> Clatches
boxToClatches b = case contents b of
  Left c -> cellToClatches c (attributes b)
  Right c -> containerToClatches c (attributes b)

-- | Split a number into two parts, so that the sum of the two parts
-- is equal to the original number.
split :: Int -> (Int, Int)
split i = (r, r + rm)
  where
    (r, rm) = i `quotRem` 2

