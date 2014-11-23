-- | Box primitives.
--
-- This module provides all functions that have access to the
-- internals of a 'Box'.  There are only six functions that make a
-- 'Box':
--
-- * 'blank' - formats a blank box with nothing but a (possibly)
-- colorful background.  Useful to paste to other 'Box' to provide
-- white space.
--
-- * 'chunks' - Makes a box out of Rainbow 'Chunk'.
--
-- * 'catH' - paste 'Box' together horizontally
--
-- * 'catV' - paste 'Box' together vertically
--
-- * 'viewH' - view a 'Box', keeping the same height but possibly
-- trimming the width
--
-- * 'viewV' - view a 'Box', keeping the same width but possibly
-- trimming the height
--
-- There are many crude diagrams in the Haddock documentation.  A
-- dash means a character with data; a period means a blank
-- character.  When you print your 'Box', the blank characters will
-- have the appropriate background color.
module Rainbox.Box.Primitives
  ( -- * Background
    Background(..)

  -- * Alignment
  , Align
  , Vert
  , Horiz
  , center
  , top
  , bottom
  , left
  , right

  -- * Box
  , Bar(..)
  , Rod(..)
  , barToBox
  , barsToBox
  , Nibble
  , unNibble
  , Spaces
  , numSpaces
  , spcBackground
  , BoxP(..)
  , Box
  , unBox

  -- * Height and Width
  , Height(..)
  , height
  , Width(..)
  , HasWidth(..)

  -- * Making Boxes
  , blank
  , chunks
  , catH
  , catV
  , viewH
  , viewV

  -- * Helpers
  , split

  ) where

import qualified Data.Foldable as F
import Rainbow
import Data.Monoid
import qualified Data.Text as X
import Data.String

-- # Background

-- | Background colors to use when inserting necessary padding.
newtype Background = Background Radiant
  deriving (Eq, Ord, Show)

instance Color Background where
  back (Background b) = back b
  fore (Background b) = fore b

-- # Box

data Spaces = Spaces
  { numSpaces :: Int
  , spcBackground :: Background
  } deriving (Eq, Show)

instance HasWidth Spaces where
  width = numSpaces

newtype Nibble = Nibble { unNibble :: Either Spaces Chunk }
  deriving (Eq, Show)

instance IsString Nibble where
  fromString = Nibble . Right . fromString

instance HasWidth Nibble where
  width = either width width . unNibble

-- | Occupies a single row on screen.  The 'Chunk's you place in a
-- 'Bar' should not have any control characters such as newlines or
-- tabs, as rainbox assumes that each character in a 'Bar' takes up
-- one screen column and that each character does not create
-- newlines.  Leave newline handling up to rainbox.  However,
-- rainbox will /not/ check to make sure that your inputs do not
-- contain newlines, tabs, or other spurious characters.  Similarly, use of
-- combining characters will create unexpected results, as Rainbox
-- will see something that takes up (for instance) two characters
-- and think it takes up two screen columns, when in reality it will
-- take up only one screen column.  So, if you need accented
-- characters, use a single Unicode code point, not two code points.
-- For example, for é, use U+00E9, not U+0065 and U+0301.
newtype Bar = Bar { unBar :: [Chunk] }
  deriving (Eq, Show)

barToBox :: Bar -> Box
barToBox = chunks . unBar

barsToBox :: Background -> Align Horiz -> [Bar] -> Box
barsToBox bk ah = catV bk ah . map barToBox

instance IsString Bar where
  fromString = Bar . (:[]) . fromString

instance Monoid Bar where
  mempty = Bar []
  mappend (Bar l) (Bar r) = Bar $ l ++ r

-- | A 'Box' has a width in columns and a height in rows.  Its
-- height and width both are always at least zero.  It can have
-- positive height even if its width is zero, and it can have
-- positive width even if its height is zero.
--
-- Each row in a 'Box' always has the same number of characters; a
-- 'Box' with zero height has no characters but still has a certain
-- width.

newtype Box = Box { unBox :: BoxP }
  deriving (Eq, Show)

newtype Rod = Rod { unRod :: [Nibble] }
  deriving (Eq, Show)

instance IsString Rod where
  fromString = Rod . (:[]) . fromString

instance HasWidth Rod where
  width = sum . map width . unRod

-- | Box payload.  Has the data of the box.
data BoxP
  = NoHeight Int
  -- ^ A Box with width but no height.  The Int must be at least
  -- zero.  If it is zero, the Box has no height and no width.
  | WithHeight [Rod]
  -- ^ A Box that has height of at least one.  It must have at least
  -- one component Bar.
  deriving (Eq, Show)

instance HasWidth BoxP where
  width b = case b of
    NoHeight w -> w
    WithHeight ns -> sum . map width $ ns

instance IsString Box where
  fromString = Box . WithHeight . (:[]) . fromString

-- # Height and Width

-- | A count of rows
newtype Height = Height { unHeight :: Int }
  deriving (Eq, Ord, Show)

-- | How many 'Rod' are in this 'Box'?
height :: Box -> Int
height b = case unBox b of
  NoHeight _ -> 0
  WithHeight rs -> length rs

-- | A count of columns
newtype Width = Width { unWidth :: Int }
  deriving (Eq, Ord, Show)

-- | How many columns are in this thing? A column is one character
-- wide.  Every 'Bar' in a 'Box' always has the same number of
-- columns.
--
-- This is for things that have a single, solitary width, not things
-- like columns that might have different widths at different
-- points.
class HasWidth a where
  width :: a -> Int

instance HasWidth Bar where
  width = sum . map (sum . map X.length . text) . unBar

instance HasWidth Box where
  width b = case unBox b of
    NoHeight i -> i
    WithHeight rs -> case rs of
      [] -> error "cols: error"
      x:_ -> width x

instance HasWidth Chunk where
  width = sum . map X.length . text

-- # Making Boxes

-- | A blank 'Box'.  Useful for aligning other 'Box'.
blank
  :: Background
  -> Height
  -> Width
  -> Box
blank bk r c
  | unHeight r < 1 = Box $ NoHeight (max 0 (unWidth c))
  | otherwise = Box . WithHeight $ replicate (unHeight r) row
  where
    row | unWidth c < 1 = Rod []
        | otherwise = Rod [ blanks bk (unWidth c) ]

-- | A 'Box' made of 'Chunk'.  Always one Bar tall, and has as many
-- columns as there are characters in the 'Chunk'.
chunks :: [Chunk] -> Box
chunks = Box . WithHeight . (:[]) . Rod . map (Nibble . Right)

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

-- | Merge several Box horizontally into one Box.  That is, with
-- alignment set to ATop:
--
-- > --- ------- ----
-- > --- -------
-- > ---
--
-- becomes
--
-- > --------------
-- > ----------....
-- > ---...........
--
-- With alignment set to ABottom, becomes
--
-- > ---...........
-- > ----------....
-- > --------------

catH :: Background -> Align Vert -> [Box] -> Box
catH bk al bs
  | null bs = Box $ NoHeight 0
  | hght == 0 = Box . NoHeight . sum . map width $ bs
  | otherwise = Box . WithHeight . mergeHoriz . map (pad . unBox) $ bs
  where
    pad = padHoriz bk al hght
    hght = F.maximum . (0:) . map height $ bs

-- | Merge several Box vertically into one Box.  That is, with
-- alignment set to 'left':
--
-- > -------
-- > -------
-- >
-- > ---
-- > ---
-- >
-- > ----
-- > ----
--
-- becomes
--
-- > -------
-- > -------
-- > ---....
-- > ---....
-- > ---....
-- > ----...
-- > ----...
--
-- With alignment set to 'right', becomes
--
-- > -------
-- > -------
-- > ....---
-- > ....---
-- > ...----
-- > ...----

catV :: Background -> Align Horiz -> [Box] -> Box
catV bk al bs
  | null bs = Box $ NoHeight 0
  | otherwise = Box . foldr f (NoHeight w)
      . concat . map (flatten . unBox) $ bs
  where
    w = F.maximum . (0:) . map width $ bs
    f mayR bp = case mayR of
      Nothing -> bp
      Just rw -> case bp of
        WithHeight wh -> WithHeight $ padded : wh
        _ -> WithHeight [padded]
        where
          padded = padVert bk al w rw
    flatten bp = case bp of
      NoHeight _ -> [Nothing]
      WithHeight rs -> map Just rs


-- | Given the resulting height, pad a list of Height.  So, when given
-- a height of 3 and an alignment of 'top':
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
-- where dashes is a 'Bar' with data, and dots is a blank 'Bar'.

padHoriz :: Background -> Align Vert -> Int -> BoxP -> [Rod]
padHoriz bk a hght bp = case bp of
  NoHeight w -> map (Rod . (:[])) . replicate h $ blanks bk w
  WithHeight rs -> concat [tp, rs, bot]
    where
      nPad = max 0 $ h - length rs
      (nATop, nBot) = case a of
        Center -> split nPad
        NonCenter ATop -> (0, nPad)
        NonCenter ABottom -> (nPad, 0)
      pad = Rod [blanks bk len]
        where
          len = case rs of
            [] -> 0
            x:_ -> width x
      (tp, bot) = (replicate nATop pad, replicate nBot pad)
  where
    h = max 0 hght

-- | Given the resulting width, pad a 'Bar'.  So, when given
-- a width of 10 and an alignment of 'right',
--
-- > -------
--
-- becomes
--
-- > ...-------

padVert
  :: Background
  -> Align Horiz
  -> Int
  -> Rod
  -> Rod
padVert bk a wdth rw@(Rod cs) = Rod . concat $ [lft, cs, rght]
  where
    nPad = max 0 $ w - width rw
    (nLeft, nRight) = case a of
      Center -> split nPad
      NonCenter ALeft -> (0, nPad)
      NonCenter ARight -> (nPad, 0)
    (lft, rght) = (mkPad nLeft, mkPad nRight)
    mkPad n
      | n == 0 = []
      | otherwise = [blanks bk n]
    w = max 0 wdth


-- | Merge several horizontal Height into one set of horizontal 'Bar'.
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

mergeHoriz :: [[Rod]] -> [Rod]
mergeHoriz = foldr (zipWith merge) (repeat (Rod []))
  where
    merge (Rod r1) (Rod r2) = Rod $ r1 ++ r2

-- # Viewing

-- | View a 'Box', possibly shrinking it.  You set the size of your
-- viewport and how it is oriented relative to the 'Box' as a whole.
-- The 'Box' returned may be smaller than the argument 'Box', but it
-- will never be bigger.
--
-- Examples:
--
-- >>> :set -XOverloadedStrings
-- >>> let box = catV defaultBackground top [ "ab", "cd" ]
-- >>> printBox . view (Height 1) (Width 1) left top $ box
-- a
--
-- >>> printBox . view (Height 1) (Width 1) right bottom $ box
-- d

viewV :: Int -> Align Vert -> Box -> Box
viewV hght a (Box b) = Box $ case b of
  WithHeight rs
    | h == 0 -> NoHeight . width . head $ rs
    | otherwise -> WithHeight $ case a of
        NonCenter ATop -> take h rs
        NonCenter ABottom -> drop extra rs
        Center -> drop nDrop . take nTake $ rs
          where
            (trimL, trimR) = split extra
            nTake = length rs - trimR
            nDrop = trimL
        where
          extra = max 0 $ length rs - h
  x -> x
  where
    h = max 0 hght

viewH :: Int -> Align Horiz -> Box -> Box
viewH wdth a (Box b) = Box $ case b of
  NoHeight nh -> NoHeight (min w nh)
  WithHeight rs -> WithHeight $ map f rs
    where
      f rw = case a of
        NonCenter ALeft -> takeChars w rw
        NonCenter ARight -> dropChars extra rw
        Center -> dropChars nDrop . takeChars nTake $ rw
          where
            (trimL, trimR) = split extra
            nTake = max 0 $ width rw - trimR
            nDrop = trimL
        where
          extra = max 0 $ width rw - w
  where
    w = max 0 wdth


dropChars :: Int -> Rod -> Rod
dropChars colsIn = Rod . go colsIn . unRod
  where
    go n cs
      | n <= 0 = cs
      | otherwise = case cs of
         [] -> []
         x:xs
           | lenX <= n -> go (n - lenX) xs
           | otherwise -> x' : xs
           where
             lenX = case unNibble x of
              Left blnk -> numSpaces blnk
              Right chk -> width chk
             x' = case unNibble x of
              Left blnk -> Nibble . Left $
                blnk { numSpaces = numSpaces blnk - n }
              Right chk -> Nibble . Right . dropChunkChars n $ chk

-- | Drops the given number of characters from a Chunk.
dropChunkChars :: Int -> Chunk -> Chunk
dropChunkChars n c = c { text = go n (text c) }
  where
    go nLeft ls = case ls of
      [] -> []
      t:ts
        | len < nLeft -> go (nLeft - len) ts
        | len == nLeft -> ts
        | otherwise -> X.drop nLeft t : ts
        where
          len = X.length t

takeChars :: Int -> Rod -> Rod
takeChars colsIn = Rod . go colsIn . unRod
  where
    go n cs
      | n <= 0 = []
      | otherwise = case cs of
          [] -> []
          x:xs
            | lenX <= n -> x : go (n - lenX) xs
            | otherwise -> [x']
            where
              (lenX, x') = case unNibble x of
                Left blnk ->
                  ( numSpaces blnk,
                    Nibble . Left $ blnk { numSpaces = n } )
                Right chk ->
                  ( width chk,
                    Nibble . Right . takeChunkChars n $ chk)

takeChunkChars :: Int -> Chunk -> Chunk
takeChunkChars n c = c { text = go n (text c) }
  where
    go nLeft ls = case ls of
      [] -> []
      t:ts
        | len < nLeft -> t : go (nLeft - len) ts
        | len == nLeft -> [t]
        | otherwise -> [X.take nLeft t]
        where
          len = X.length t

--
-- # Helpers
--

-- | Generate spaces.
blanks
  :: Background
  -- ^ Background colors
  -> Int
  -- ^ Number of blanks
  -> Nibble
blanks bk c = Nibble (Left (Spaces c bk))

-- | Split a number into two parts, so that the sum of the two parts
-- is equal to the original number.
split :: Int -> (Int, Int)
split i = (r, r + rm)
  where
    (r, rm) = i `quotRem` 2

