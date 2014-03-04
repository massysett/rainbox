module Text.PrettyPrint.Rainbox.Box where

import qualified Data.Foldable as F
import System.Console.Rainbow
import Data.Monoid
import qualified Data.Text as X

data Background = Background
  { boxBackground8 :: Background8
  , boxBackground256 :: Background256
  } deriving Show

defaultBackground :: Background
defaultBackground = Background (Last Nothing) (Last Nothing)

newtype Row = Row { unRow :: [Chunk] }
  deriving Show

newtype Box = Box { unBox :: [Row] }
  deriving Show

rows :: Box -> Rows
rows = Rows . length . unBox

newtype Rows = Rows { unRows :: Int }
  deriving (Eq, Ord, Show)

newtype Cols = Cols { unCols :: Int }
  deriving (Eq, Ord, Show)

class HasCols a where
  cols :: a -> Cols

instance HasCols Row where
  cols = Cols . sum . map (X.length . text) . unRow

instance HasCols Box where
  cols b = case unBox b of
    [] -> Cols 0
    x:_ -> cols x

blank
  :: Background
  -> Rows
  -> Cols
  -> Box
blank bk r c = Box $ replicate (unRows r) row
  where
    row = Row $ [ blanks bk c ]

chunkBox :: [Chunk] -> Box
chunkBox = Box . (:[]) . Row

data Alignment a = Center | NonCenter a
  deriving (Eq, Show)

data Horizontal = Top | Bottom
  deriving (Eq, Show)

data Vertical = Left | Right
  deriving (Eq, Show)

hcat :: Background -> Alignment Horizontal -> [Box] -> Box
hcat bk al bs = Box . mergeHoriz . map (pad . unBox) $ bs
  where
    pad = padHoriz al height
    height = F.maximum . (Rows 0:) . map rows $ bs

-- | Given the resulting height, pad a list of Rows.  So, when given
-- a height of 3 and an alignment of Top,
--
-- > --------
-- > --------
--
-- becomes
--
-- > --------
-- > --------
-- > ........
--
-- where dashes is a Row with data, and dots is a blank Row.

padHoriz :: Alignment Horizontal -> Rows -> [Row] -> [Row]
padHoriz = undefined


-- | Merge several horizontal Rows into one set of horizontal Row.
-- That is:
--
-- > ----- ----- -----
-- > ----- ----- -----
-- > ----- ----- -----
--
-- into
--
-- > ---------------
-- > ---------------
-- > ---------------
--
-- Strange behavior will result if each input list is not exactly
-- the same length.

mergeHoriz :: [[Row]] -> [Row]
mergeHoriz = foldr (zipWith merge) (repeat (Row []))
  where
    merge (Row r1) (Row r2) = Row $ r1 ++ r2




--
-- # Helpers
--

-- | Generate spaces.
blanks
  :: Background
  -- ^ Background colors
  -> Cols
  -- ^ Number of blanks
  -> Chunk
blanks (Background b8 b256) c = Chunk ts t
  where
    t = X.replicate (unCols c) (X.singleton ' ')
    ts = mempty { style8 = mempty { background8 = b8 }
                , style256 = mempty { background256 = b256 }
                }

-- | Split a number into two parts, so that the sum of the two parts
-- is equal to the original number.
split :: Int -> (Int, Int)
split i = (r, r + rm)
  where
    (r, rm) = i `quotRem` 2

{-

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
  = Blank Background Int Int
  -- ^ No content.  Has given number of rows and columns.

  | Chunks [Chunk]
  -- ^ A raw string.  Has one row and a number of columns equal to
  -- the number of characters in the component Chunks.

  | Row Background [Box]
  -- ^ A row of sub-boxes.  Number of rows is equal to the number of
  -- rows in the tallest Box.  Number of columns is equal to the sum
  -- of the number of columns in component Box.

  | Col Background [Box]
  -- ^ A column of sub-boxes.  Number of columns is equal to the
  -- number of columns 
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
-}
