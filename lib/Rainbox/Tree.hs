module Rainbox.Tree where

import System.Console.Rainbow
import System.Console.Rainbow.Colors
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
  { boxBackground8 :: Color8
  , boxBackground256 :: Color256
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

