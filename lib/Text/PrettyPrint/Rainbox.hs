module Text.PrettyPrint.Rainbox where

import Control.Arrow ((***), first)
import Data.Monoid
import Data.List (intersperse)
import Data.String (fromString)
import System.Console.Rainbow
import qualified Data.Text as X

-- | The basic data type.  A box has a specified size and some sort of
--   contents.
data Box = Box
  { rows :: Int
  , cols :: Int
  , content :: Content
  } deriving Show


-- | Data type for specifying the alignment of boxes.
data Alignment
  = AlignFirst    -- ^ Align at the top/left.
  | AlignCenter1  -- ^ Centered, biased to the top/left.
  | AlignCenter2  -- ^ Centered, biased to the bottom/right.
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

-- | Align boxes centered, but biased to the left/top in case of
--   unequal parities.
center1 :: Alignment
center1    = AlignCenter1

-- | Align boxes centered, but biased to the right/bottom in case of
--   unequal parities.
center2 :: Alignment
center2    = AlignCenter2

data Background = Background
  { boxBackground8 :: Background8
  , boxBackground256 :: Background256
  } deriving Show

defaultBackground :: Background
defaultBackground = Background (Last Nothing) (Last Nothing)

-- | Contents of a box.
data Content
  = Blank
  -- ^ No content.

  | Chunks [Chunk]
  -- ^ A raw string.

  | Row Background [Box]
  -- ^ A row of sub-boxes.

  | Col Background [Box]
  -- ^ A column of sub-boxes.

  | SubBox Background Alignment Alignment Box
  -- ^ A sub-box with a specified alignment.  Alignment is
  -- horizontal alignment first, vertical alignment second.
  deriving Show


-- | The null box, which has no content and no size.  It is quite
--   useless.
nullBox :: Box
nullBox = emptyBox 0 0

-- | @emptyBox r c@ is an empty box with @r@ rows and @c@ columns.
--   Useful for effecting more fine-grained positioning of other
--   boxes, by inserting empty boxes of the desired size in between
--   them.
emptyBox :: Int -> Int -> Box
emptyBox r c = Box r c Blank

-- | Contains chunks; the box is 1 tall and its length is the sum of
-- the lengths of the chunks.
chunks :: [Chunk] -> Box
chunks cs = Box 1 (sum . map X.length . map text $ cs) (Chunks cs)

--
-- # Alignment
--

-- | @alignHoriz algn n bx@ creates a box of width @n@, with the
--   contents and height of @bx@, horizontally aligned according to
--   @algn@.
alignHoriz :: Background -> Alignment -> Int -> Box -> Box
alignHoriz bk a c b = Box (rows b) c $ SubBox bk a AlignFirst b

-- | @alignVert algn n bx@ creates a box of height @n@, with the
--   contents and width of @bx@, vertically aligned according to
--   @algn@.
alignVert :: Background -> Alignment -> Int -> Box -> Box
alignVert bk a r b = Box r (cols b) $ SubBox bk AlignFirst a b

-- | @align ah av r c bx@ creates an @r@ x @c@ box with the contents
--   of @bx@, aligned horizontally according to @ah@ and vertically
--   according to @av@.
align :: Background -> Alignment -> Alignment -> Int -> Int -> Box -> Box
align bk ah av r c = Box r c . SubBox bk ah av

-- | Move a box \"up\" by putting it in a larger box with extra rows,
--   aligned to the top.  See the disclaimer for 'moveLeft'.
moveUp :: Background -> Int -> Box -> Box
moveUp bk n b = alignVert bk top (rows b + n) b

-- | Move a box down by putting it in a larger box with extra rows,
--   aligned to the bottom.  See the disclaimer for 'moveLeft'.
moveDown :: Background -> Int -> Box -> Box
moveDown bk n b = alignVert bk bottom (rows b + n) b

-- | Move a box left by putting it in a larger box with extra columns,
--   aligned left.  Note that the name of this function is
--   something of a white lie, as this will only result in the box
--   being moved left by the specified amount if it is already in a
--   larger right-aligned context.
moveLeft :: Background -> Int -> Box -> Box
moveLeft bk n b = alignHoriz bk left (cols b + n) b

-- | Move a box right by putting it in a larger box with extra
--   columns, aligned right.  See the disclaimer for 'moveLeft'.
moveRight :: Background -> Int -> Box -> Box
moveRight bk n b = alignHoriz bk right (cols b + n) b

--
-- # Glueing
--

-- | Glue a list of boxes together horizontally, with the given alignment.
hcat :: Background -> Alignment -> [Box] -> Box
hcat b a bs = Box h w (Row b $ map (alignVert b a h) bs)
  where h = maximum . (0:) . map rows $ bs
        w = sum . map cols $ bs

-- | @hsep sep a bs@ lays out @bs@ horizontally with alignment @a@,
--   with @sep@ amount of space in between each.
hsep :: Background -> Int -> Alignment -> [Box] -> Box
hsep bk sep a bs = punctuateH bk a (emptyBox 0 sep) bs

-- | Glue a list of boxes together vertically, with the given alignment.
vcat :: Background -> Alignment -> [Box] -> Box
vcat b a bs = Box h w (Col b $ map (alignHoriz b a w) bs)
  where h = sum . map rows $ bs
        w = maximum . (0:) . map cols $ bs

-- | @vsep sep a bs@ lays out @bs@ vertically with alignment @a@,
--   with @sep@ amount of space in between each.
vsep :: Int -> Background -> Alignment -> [Box] -> Box
vsep sep b a bs = punctuateV b a (emptyBox sep 0) bs

-- | @punctuateH a p bs@ horizontally lays out the boxes @bs@ with a
--   copy of @p@ interspersed between each.
punctuateH :: Background -> Alignment -> Box -> [Box] -> Box
punctuateH b a p bs = hcat b a (intersperse p bs)

-- | A vertical version of 'punctuateH'.
punctuateV :: Background -> Alignment -> Box -> [Box] -> Box
punctuateV b a p bs = vcat b a (intersperse p bs)

instance Monoid Box where
  mappend l r = hcat defaultBackground top [l, r]
  mempty = nullBox

-- | Paste two boxes together horizontally with a single intervening
--   column of space, using a default (top) alignment.
(<+>) :: Box -> Box -> Box
l <+> r = hcat defaultBackground top [l, emptyBox 0 1, r]

-- | Paste two boxes together vertically, using a default (left)
--   alignment.
(//) :: Box -> Box -> Box
t // b = vcat defaultBackground left [t,b]

-- | Paste two boxes together vertically with a single intervening row
--   of space, using a default (left) alignment.
(/+/) :: Box -> Box -> Box
t /+/ b = vcat defaultBackground left [t, emptyBox 1 0, b]

--
-- # Implementation
--

-- | \"Padded take\": @takeP a n xs@ is the same as @take n xs@, if @n
--   <= length xs@; otherwise it is @xs@ followed by enough copies of
--   @a@ to make the length equal to @n@.
takeP :: a -> Int -> [a] -> [a]
takeP _ n _      | n <= 0 = []
takeP b n []              = replicate n b
takeP b n (x:xs)          = x : takeP b (n-1) xs

takeChunksLen
  :: Background
  -> Int
  -- ^ Length
  -> [Chunk]
  -> [Chunk]
takeChunksLen b len = go len
  where
    go l cs
      | l <= 0 = []
      | otherwise = case cs of
          [] -> [blanks b l]
          (x:xs)
            | lenX >= l -> [x { text = X.take l  . text $ x }]
            | otherwise -> x : go (l - lenX) xs
            where
              lenX = X.length . text $ x

-- | @takePA @ is like 'takeP', but with alignment.  That is, we
--   imagine a copy of @xs@ extended infinitely on both sides with
--   copies of @a@, and a window of size @n@ placed so that @xs@ has
--   the specified alignment within the window; @takePA algn a n xs@
--   returns the contents of this window.
takePA :: Alignment -> a -> Int -> [a] -> [a]
takePA c b n = glue . (takeP b (numRev c n) *** takeP b (numFwd c n)) . split
  where split t = first reverse . splitAt (numRev c (length t)) $ t
        glue    = uncurry (++) . first reverse
        numFwd AlignFirst    n = n
        numFwd AlignLast     _ = 0
        numFwd AlignCenter1  n = n `div` 2
        numFwd AlignCenter2  n = (n+1) `div` 2
        numRev AlignFirst    _ = 0
        numRev AlignLast     n = n
        numRev AlignCenter1  n = (n+1) `div` 2
        numRev AlignCenter2  n = n `div` 2

takePAChunks
  :: (Background8, Background256)
  -> Alignment
  -> Int
  -> [Chunk]
  -> [Chunk]
takePAChunks = undefined

-- | Resize a rendered list of lines.
resizeBox
  :: Background
  -- ^ Background
  -> Int
  -- ^ Rows
  -> Int
  -- ^ Columns
  -> [[Chunk]]
  -- ^ Text of each row
  -> [[Chunk]]
resizeBox b r c
  = takeP [(blanks b c)] r
  . map (takeChunksLen b c)


-- | Generate spaces.
blanks
  :: Background
  -- ^ Background colors
  -> Int
  -- ^ Number of blanks
  -> Chunk
blanks (Background b8 b256) c = Chunk ts t
  where
    t = X.replicate c (X.singleton ' ')
    ts = mempty { style8 = mempty { background8 = b8 }
                , style256 = mempty { background256 = b256 }
                }

renderBox :: Box -> [Chunk]
renderBox (Box r c o) = case o of
  Blank -> undefined
