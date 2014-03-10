-- | Box primitives.
--
-- This module provides all functions that have access to the
-- internals of a 'Box'.  There are only five functions that make a
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
-- * 'view' - take a portion of an already-existing 'Box'.
--
-- There are many crude diagrams in the Haddock documentation.  A
-- dash means a character with data; a period means a blank
-- character.  When you print your 'Box', the blank characters will
-- have the appropriate background color.
module Rainbox.Box
  ( -- * Background
    Background(..)
  , defaultBackground

  -- * Box
  , Row(..)
  , BoxP(..)
  , Box
  , unBox

  -- * Rows and Cols
  , Rows(..)
  , rows
  , Cols(..)
  , HasCols(..)

  -- * Alignment
  , Align
  , Vert
  , Horiz
  , center
  , top
  , bottom
  , left
  , right

  -- * Making Boxes
  , blank
  , chunks
  , catH
  , catV
  , view

  ) where

import qualified Data.Foldable as F
import System.Console.Rainbow.Types
import Data.Monoid
import qualified Data.Text as X
import Data.String
import System.Console.Rainbow.Colors

-- # Background

-- | Background colors to use when inserting necessary padding.
data Background = Background
  { boxBackground8 :: Color8
  , boxBackground256 :: Color256
  } deriving (Eq, Show)

-- | Use the default background colors of the current terminal.
defaultBackground :: Background
defaultBackground = Background c8_default c256_default

-- # Box

-- | Occupies a single row on screen.  The 'Chunk' you place in a
-- 'Row' should not have any control characters such as newlines or
-- tabs, as rainbox assumes that each character in a 'Row' takes up
-- one screen column and that each character does not create
-- newlines.  Leave newline handling up to rainbox.  However,
-- rainbox does nothing to enforce this practice.  Similarly, use of
-- combining characters will create unexpected result, as rainbow
-- will see something that takes up (for instance) two characters
-- and think it takes up two screen columns, when in reality it will
-- take up only one screen column.  So, if you need accented
-- characters, use a single Unicode code point, not two code points.
newtype Row = Row { unRow :: [Chunk] }
  deriving (Eq, Show)

instance IsString Row where
  fromString = Row . (:[]) . fromString

instance Monoid Row where
  mempty = Row []
  mappend (Row l) (Row r) = Row $ l ++ r

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

-- | Box payload.  Has the data of the box.
data BoxP
  = NoHeight Int
  -- ^ A Box with width but no height.  The Int must be at least
  -- zero.  If it is zero, the Box has no height and no width.
  | WithHeight [Row]
  -- ^ A Box that has height of at least one.  It must have at least
  -- one component Row.
  deriving (Eq, Show)

instance IsString Box where
  fromString = Box . WithHeight . (:[]) . fromString

-- # Rows and Cols

-- | A count of rows
newtype Rows = Rows { unRows :: Int }
  deriving (Eq, Ord, Show)

-- | How many 'Row' are in this 'Box'?
rows :: Box -> Rows
rows b = case unBox b of
  NoHeight _ -> Rows 0
  WithHeight rs -> Rows . length $ rs

-- | A count of columns
newtype Cols = Cols { unCols :: Int }
  deriving (Eq, Ord, Show)

-- | How many columns are in this thing? A column is one character
-- wide.  Every 'Row' in a 'Box' always has the same number of
-- columns.
class HasCols a where
  cols :: a -> Cols

instance HasCols Row where
  cols = Cols . sum . map (X.length . text) . unRow

instance HasCols Box where
  cols b = case unBox b of
    NoHeight i -> Cols i
    WithHeight rs -> case rs of
      [] -> error "cols: error"
      x:_ -> cols x

-- # Making Boxes

-- | A blank 'Box'.  Useful for aligning other 'Box'.
blank
  :: Background
  -> Rows
  -> Cols
  -> Box
blank bk r c
  | unRows r < 1 = Box $ NoHeight (max 0 (unCols c))
  | otherwise = Box . WithHeight $ replicate (unRows r) row
  where
    row = Row $ [ blanks bk c ]

-- | A 'Box' made of 'Chunk'.  Always one Row tall, and has as many
-- columns as there are characters in the 'Chunk'.
chunks :: [Chunk] -> Box
chunks = Box . WithHeight . (:[]) . Row

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
  | otherwise = Box . mergeHoriz . map (pad . unBox) $ bs
  where
    pad = padHoriz bk al height
    height = F.maximum . (Rows 0:) . map rows $ bs

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
-- > With alignment set to 'right', becomes
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
  | otherwise = Box . foldr f (NoHeight (unCols w))
      . concat . map (flatten . unBox) $ bs
  where
    w = F.maximum . (Cols 0:) . map cols $ bs
    f ei bp = case padVert bk al w ei of
      Left nh -> case bp of
        NoHeight wOld -> NoHeight (max (unCols w) wOld)
        x -> x
      Right wh -> case bp of
        WithHeight rw -> WithHeight $ wh : rw
        _ -> WithHeight [wh]
    flatten bp = case bp of
      NoHeight i -> [Left i]
      WithHeight rs -> map Right rs


-- | Given the resulting height, pad a list of Rows.  So, when given
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
-- where dashes is a 'Row' with data, and dots is a blank 'Row'.

padHoriz :: Background -> Align Vert -> Rows -> BoxP -> BoxP
padHoriz = undefined
--padHoriz :: Background -> Align Vert -> Rows -> [Row] -> [Row]
{-
padHoriz bk a (Rows tgt) rs = concat [tp, rs, bot]
  where
    nPad = max 0 $ tgt - length rs
    (nATop, nBot) = case a of
      Center -> split nPad
      NonCenter ATop -> (0, nPad)
      NonCenter ABottom -> (nPad, 0)
    pad = Row [blanks bk len]
      where
        len = case rs of
          [] -> Cols 0
          x:_ -> cols x
    (tp, bot) = (replicate nATop pad, replicate nBot pad)
-}

-- | Given the resulting width, pad a 'Row'.  So, when given
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
  -> Cols
  -> Either Int Row
  -> Either Int Row
padVert = undefined
{-
padVert :: Background -> Align Horiz -> Cols -> Row -> Row
padVert bk a (Cols tgt) rw@(Row cs) = Row . concat $ [lft, cs, rght]
  where
    nPad = max 0 $ tgt - (unCols . cols $ rw)
    (nLeft, nRight) = case a of
      Center -> split nPad
      NonCenter ALeft -> (0, nPad)
      NonCenter ARight -> (nPad, 0)
    (lft, rght) = (mkPad nLeft, mkPad nRight)
    mkPad n
      | n == 0 = []
      | otherwise = [blanks bk (Cols n)]
-}

-- | Merge several horizontal Rows into one set of horizontal 'Row'.
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

{-
mergeHoriz :: [[Row]] -> [Row]
mergeHoriz = foldr (zipWith merge) (repeat (Row []))
  where
    merge (Row r1) (Row r2) = Row $ r1 ++ r2
-}
mergeHoriz :: [BoxP] -> BoxP
mergeHoriz = undefined

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
-- >>> printBox . view (Rows 1) (Cols 1) left top $ box
-- a
--
-- >>> printBox . view (Rows 1) (Cols 1) right bottom $ box
-- d

view
  :: Rows
  -> Cols
  -> Align Horiz
  -> Align Vert
  -> Box
  -> Box
view = undefined
{-
view r c ah av (Box inRows) = Box . map htrim . vtrim $ inRows
  where
    vtrim = case av of
      NonCenter ATop -> take . unRows $ r
      NonCenter ABottom -> drop extra
      Center -> drop nDrop . take nTake
        where
          (trimL, trimR) = split extra
          nTake = length inRows - trimR
          nDrop = trimL
      where
        extra = length inRows - unRows r

    htrim rw = case ah of
      NonCenter ALeft -> takeChars c rw
      NonCenter ARight -> dropChars (Cols extra) rw
      Center -> dropChars nDrop . takeChars nTake $ rw
        where
          (trimL, trimR) = split extra
          nTake = Cols $ (unCols . cols $ rw) - trimR
          nDrop = Cols trimL
      where
        extra = (unCols . cols $ rw) - unCols c
-}

dropChars :: Cols -> Row -> Row
dropChars colsIn = Row . go (unCols colsIn) . unRow
  where
    go n cs
      | n <= 0 = cs
      | otherwise = case cs of
         [] -> []
         x:xs
           | lenX <= n -> go (n - lenX) xs
           | otherwise -> x' : xs
           where
             lenX = X.length . text $ x
             x' = x { text = X.drop n . text $ x }

takeChars :: Cols -> Row -> Row
takeChars colsIn = Row . go (unCols colsIn) . unRow
  where
    go n cs
      | n <= 0 = []
      | otherwise = case cs of
          [] -> []
          x:xs
            | lenX <= n -> x : go (n - lenX) xs
            | otherwise -> [x']
            where
              lenX = X.length . text $ x
              x' = x { text = X.take n . text $ x }

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
    ts = mempty { style8 = mempty { background8 = Last . Just $ b8 }
                , style256 = mempty { background256 = Last . Just $ b256 }
                }

-- | Split a number into two parts, so that the sum of the two parts
-- is equal to the original number.
split :: Int -> (Int, Int)
split i = (r, r + rm)
  where
    (r, rm) = i `quotRem` 2

