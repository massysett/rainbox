module Text.PrettyPrint.Rainbox.Box where

import System.Console.Rainbow
import Data.Monoid
import qualified Data.Text as X

data Background = Background
  { boxBackground8 :: Background8
  , boxBackground256 :: Background256
  } deriving Show

defaultBackground :: Background
defaultBackground = Background (Last Nothing) (Last Nothing)

-- | Data type for specifying the alignment of boxes.
data Alignment
  = AlignFirst    -- ^ Align at the top/left.
  | AlignCenter   -- ^ Centered, biased to the top/left.
  | AlignLast     -- ^ Align at the bottom/right.
  deriving (Eq, Ord, Show)

-- | Align boxes along their tops.
top :: Alignment
top        = AlignFirst

-- | Align boxes along their bottoms.
bottom :: Alignment
bottom     = AlignLast

-- | Align boxes to the left.
left :: Alignment
left       = AlignFirst

-- | Align boxes to the right.
right :: Alignment
right      = AlignLast

center :: Alignment
center = AlignCenter

-- | The basic data type.  A box has a specified size and some sort of
--   contents.
data Box = Box
  { rows :: Int
  , cols :: Int
  , background :: Background
  , content :: Content
  } deriving Show

-- | Contents of a box.
data Content
  = Blank
  -- ^ No content.

  | Chunks [Chunk]
  -- ^ A raw string.

  | Row [Box]
  -- ^ A row of sub-boxes.

  | Col [Box]
  -- ^ A column of sub-boxes.

  | SubBox Alignment Alignment Box
  -- ^ A sub-box with a specified alignment.  Alignment is
  -- horizontal alignment first, vertical alignment second.
  deriving Show

-- | The null box, which has no content and no size.  It is quite
--   useless.
nullBox :: Background -> Box
nullBox bk = emptyBox bk 0 0

-- | @emptyBox r c@ is an empty box with @r@ rows and @c@ columns.
--   Useful for effecting more fine-grained positioning of other
--   boxes, by inserting empty boxes of the desired size in between
--   them.
emptyBox :: Background -> Int -> Int -> Box
emptyBox b r c = Box r c b Blank

-- | Contains chunks; the box is 1 tall and its length is the sum of
-- the lengths of the chunks.
chunks :: Background -> [Chunk] -> Box
chunks b cs = Box 1 (sum . map X.length . map text $ cs) b (Chunks cs)

--
-- # Gluing
--

-- | Glue a list of boxes together horizontally, with the given alignment.
hcat :: Background -> Alignment -> [Box] -> Box
hcat b a bs = Box h w b (Row $ map (alignVert b a h) bs)
  where h = maximum . (0:) . map rows $ bs
        w = sum . map cols $ bs

instance Monoid Box where
  mappend l r = hcat defaultBackground top [l, r]
  mempty = nullBox defaultBackground



--
-- # Alignment
--

-- | @alignHoriz algn n bx@ creates a box of width @n@, with the
--   contents and height of @bx@, horizontally aligned according to
--   @algn@.
alignHoriz :: Background -> Alignment -> Int -> Box -> Box
alignHoriz bk a c b = Box (rows b) c bk $ SubBox a AlignFirst b

-- | @alignVert algn n bx@ creates a box of height @n@, with the
--   contents and width of @bx@, vertically aligned according to
--   @algn@.
alignVert :: Background -> Alignment -> Int -> Box -> Box
alignVert bk a r b = Box r (cols b) bk $ SubBox AlignFirst a b

-- | @align ah av r c bx@ creates an @r@ x @c@ box with the contents
--   of @bx@, aligned horizontally according to @ah@ and vertically
--   according to @av@.
align :: Background -> Alignment -> Alignment -> Int -> Int -> Box -> Box
align bk ah av r c = Box r c bk . SubBox ah av

