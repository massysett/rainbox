{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  (
  -- * Alignment
    Align
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
  , Box

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

  -- * Rendering Boxes
  , render

  -- * Helpers
  , split

  ) where

import Control.Monad (join)
import qualified Data.Foldable as F
import Rainbow
import Rainbow.Types
import Data.Monoid
import qualified Data.Text as X
import Data.String
import Data.Sequence (Seq, (<|), ViewL(..), viewl)
import qualified Data.Sequence as Seq

-- # Classes

-- | How many columns are in this thing? A column is one character
-- wide.
class HasWidth a where
  width :: a -> Int

-- | How tall is this thing?  A single screen row has height of 1.
class HasHeight a where
  height :: a -> Int

-- # Box

data Spaces = Spaces
  { numSpaces :: Int
  , spcBackground :: Radiant
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
newtype Bar = Bar (Seq Chunk)
  deriving (Eq, Show)

-- | Creates a 'Box' from a single 'Bar'.  The 'Box' always has height
-- 1 and is wide as the number of characters in the 'Bar'.
barToBox :: Bar -> Box
barToBox (Bar cs) = chunks cs

-- | Creates a 'Box' from several 'Bar'.  The 'Box' has a height equal
-- to the number of 'Bar' in the 'Seq'.  The width of the 'Box' is as
-- wide as the number of characters in the widest 'Bar'.  The
-- 'Radiant' supplies the background color that will be used when
-- padding 'Bar' that are not as wide as the widest 'Bar'.
barsToBox
  :: Radiant
  -- ^ Background colors
  -> Align Horiz
  -> Seq Bar
  -> Box
barsToBox bk ah = catV bk ah . fmap barToBox

instance IsString Bar where
  fromString = Bar . Seq.singleton . fromString

instance Monoid Bar where
  mempty = Bar Seq.empty
  mappend (Bar l) (Bar r) = Bar $ l <> r

-- | A 'Box' has a width in columns and a height in rows.  Its
-- height and width both are always at least zero.  It can have
-- positive height even if its width is zero, and it can have
-- positive width even if its height is zero.
--
-- Each row in a 'Box' always has the same number of characters; a
-- 'Box' with zero height has no characters but still has a certain
-- width.

newtype Box = Box BoxP
  deriving (Eq, Show)

instance HasHeight Box where
  height (Box b) = height b

newtype Rod = Rod (Seq Nibble)
  deriving (Eq, Show, Monoid)

instance IsString Rod where
  fromString = Rod . Seq.singleton . fromString

instance HasWidth Rod where
  width (Rod ns) = F.sum . fmap width $ ns

-- | Box payload.  Has the data of the box.
data BoxP
  = NoHeight Int
  -- ^ A Box with width but no height.  The Int must be at least
  -- zero.  If it is zero, the Box has no height and no width.
  | WithHeight (Seq Rod)
  -- ^ A Box that has height of at least one.  It must have at least
  -- one component Bar.
  deriving (Eq, Show)

instance HasHeight BoxP where
  height bp = case bp of
    NoHeight _ -> 0
    WithHeight sq -> Seq.length sq

instance HasWidth BoxP where
  width b = case b of
    NoHeight w -> w
    WithHeight ns -> F.sum . fmap width $ ns

instance IsString Box where
  fromString = Box . WithHeight . Seq.singleton . fromString

-- # Height and Width

-- | A count of rows
newtype Height = Height Int
  deriving (Eq, Ord, Show)

-- | A count of columns
newtype Width = Width Int
  deriving (Eq, Ord, Show)

instance HasWidth Bar where
  width (Bar b) = F.sum . fmap (F.sum . fmap X.length . chunkTexts) $ b

instance HasWidth Box where
  width (Box b) = case b of
    NoHeight i -> i
    WithHeight rs -> case viewl rs of
      EmptyL -> error "cols: error"
      x :< _ -> width x

instance HasWidth Chunk where
  width = sum . map X.length . chunkTexts

-- # Making Boxes

-- | A blank 'Box'.  Useful for aligning other 'Box'.
blank
  :: Radiant
  -- ^ Background colors
  -> Height
  -> Width
  -> Box
blank bk (Height r) (Width c)
  | r < 1 = Box $ NoHeight (max 0 c)
  | otherwise = Box . WithHeight $ Seq.replicate r row
  where
    row | c < 1 = Rod Seq.empty
        | otherwise = Rod . Seq.singleton $ blanks bk c

-- | A 'Box' made of 'Chunk'.  Always one Bar tall, and has as many
-- columns as there are characters in the 'Chunk'.
chunks :: Seq Chunk -> Box
chunks = Box . WithHeight . Seq.singleton . Rod . fmap (Nibble . Right)

-- | Alignment.
data Align a = Center | NonCenter a
  deriving (Eq, Show)

-- | Vertical alignment.
data Vert = ATop | ABottom
  deriving (Eq, Show)

-- | Horizontal alignment.
data Horiz = ALeft | ARight
  deriving (Eq, Show)

-- | Align in the center.  Works either for horizontal or vertical
-- alignment, depending on the context.  Leaves both margins ragged.
center :: Align a
center = Center

-- | Align items so they are flush with the top; leaves the bottom
-- ragged.
top :: Align Vert
top = NonCenter ATop

-- | Align items so they are flush with the bottom; leaves the top
-- ragged.
bottom :: Align Vert
bottom = NonCenter ABottom

-- | Align items so they are flush with the left; leaves the right
-- ragged.
left :: Align Horiz
left = NonCenter ALeft

-- | Align items so they are flush with the right; leaves the left
-- ragged.
right :: Align Horiz
right = NonCenter ARight

-- | Merge several Box horizontally into one Box.  That is, with
-- alignment set to 'top':
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
-- With alignment set to 'bottom', becomes
--
-- > ---...........
-- > ----------....
-- > --------------

catH
  :: Radiant
  -- ^ Background colors
  -> Align Vert
  -> Seq Box
  -> Box
catH bk al bs
  | Seq.null bs = Box $ NoHeight 0
  | hght == 0 = Box . NoHeight . F.sum . fmap width $ bs
  | otherwise = Box . WithHeight . mergeHoriz . fmap (pad . (\(Box b) -> b)) $ bs
  where
    pad = padHoriz bk al hght
    hght = F.maximum . (0 <|) . fmap height $ bs

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

catV
  :: Radiant
  -- ^ Background colors
  -> Align Horiz
  -> Seq Box
  -> Box
catV bk al bs
  | Seq.null bs = Box $ NoHeight 0
  | otherwise = Box . F.foldr f (NoHeight w)
      . join . fmap (flatten . (\(Box b) -> b)) $ bs
  where
    w = F.maximum . (0 <|) . fmap width $ bs
    f mayR bp = case mayR of
      Nothing -> bp
      Just rw -> case bp of
        WithHeight wh -> WithHeight $ padded <| wh
        _ -> WithHeight . Seq.singleton $ padded
        where
          padded = padVert bk al w rw
    flatten bp = case bp of
      NoHeight _ -> Seq.singleton Nothing
      WithHeight rs -> fmap Just rs


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

padHoriz
  :: Radiant
  -- ^ Background colors
  -> Align Vert
  -> Int
  -> BoxP
  -> Seq Rod
padHoriz bk a hght bp = case bp of
  NoHeight w -> fmap (Rod . Seq.singleton) . Seq.replicate h $ blanks bk w
  WithHeight rs -> join . Seq.fromList $ [tp, rs, bot]
    where
      nPad = max 0 $ h - Seq.length rs
      (nATop, nBot) = case a of
        Center -> split nPad
        NonCenter ATop -> (0, nPad)
        NonCenter ABottom -> (nPad, 0)
      pad = Rod . Seq.singleton $ blanks bk len
        where
          len = case viewl rs of
            EmptyL -> 0
            x :< _ -> width x
      (tp, bot) = (Seq.replicate nATop pad, Seq.replicate nBot pad)
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
  :: Radiant
  -- ^ Background colors
  -> Align Horiz
  -> Int
  -> Rod
  -> Rod
padVert bk a wdth rw@(Rod cs) = Rod . join . Seq.fromList $ [lft, cs, rght]
  where
    nPad = max 0 $ w - width rw
    (nLeft, nRight) = case a of
      Center -> split nPad
      NonCenter ALeft -> (0, nPad)
      NonCenter ARight -> (nPad, 0)
    (lft, rght) = (mkPad nLeft, mkPad nRight)
    mkPad n
      | n == 0 = Seq.empty
      | otherwise = Seq.singleton $ blanks bk n
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

mergeHoriz :: Seq (Seq Rod) -> Seq Rod
mergeHoriz sq = case viewl sq of
  EmptyL -> Seq.empty
  lead :< rest -> go lead rest
  where
    go x xs = case viewl xs of
      EmptyL -> x
      y :< ys -> go (Seq.zipWith (<>) x y) ys


-- # Viewing

-- | View a 'Box' vertically, possibly shrinking it.  The returned
-- 'Box' is the same width as the argument 'Box'.  If the alignment is
-- 'top', excess lines from the bottom are removed; if the alignment
-- is 'bottom', excess lines from the top are removed; if 'center', a
-- roughly equal number of lines are removed from top and bottom.
viewV :: Int -> Align Vert -> Box -> Box
viewV hght a (Box b) = Box $ case b of
  WithHeight rs
    | h == 0 -> case viewl rs of
        EmptyL -> error "viewV: error"
        x :< _ -> NoHeight . width $ x
    | otherwise -> WithHeight $ case a of
        NonCenter ATop -> Seq.take h rs
        NonCenter ABottom -> Seq.drop extra rs
        Center -> Seq.drop nDrop . Seq.take nTake $ rs
          where
            (trimL, trimR) = split extra
            nTake = Seq.length rs - trimR
            nDrop = trimL
        where
          extra = max 0 $ Seq.length rs - h
  x -> x
  where
    h = max 0 hght

-- | View a 'Box' horizontally, possibly shrinking it.  The returned
-- 'Box' is the same height as the argument 'Box'.  If the alignment
-- is 'left', excess columns from the right are removed; if the
-- alignment is 'right', excess columns from the left are removed; if
-- 'center', a roughly equal number of columns are removed from the
-- left and right.
viewH :: Int -> Align Horiz -> Box -> Box
viewH wdth a (Box b) = Box $ case b of
  NoHeight nh -> NoHeight (min w nh)
  WithHeight rs -> WithHeight $ fmap f rs
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
dropChars colsIn (Rod rd) = Rod . go colsIn $ rd
  where
    go n cs
      | n <= 0 = cs
      | otherwise = case viewl cs of
         EmptyL -> Seq.empty
         x :< xs
           | lenX <= n -> go (n - lenX) xs
           | otherwise -> x' <| xs
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
dropChunkChars n c = c { chunkTexts = go n (chunkTexts c) }
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
takeChars colsIn (Rod rd) = Rod . go colsIn $ rd
  where
    go n cs
      | n <= 0 = Seq.empty
      | otherwise = case viewl cs of
          EmptyL -> Seq.empty
          x :< xs
            | lenX <= n -> x <| go (n - lenX) xs
            | otherwise -> Seq.singleton x'
            where
              (lenX, x') = case unNibble x of
                Left blnk ->
                  ( numSpaces blnk,
                    Nibble . Left $ blnk { numSpaces = n } )
                Right chk ->
                  ( width chk,
                    Nibble . Right . takeChunkChars n $ chk)

takeChunkChars :: Int -> Chunk -> Chunk
takeChunkChars n c = c { chunkTexts = go n (chunkTexts c) }
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
  :: Radiant
  -- ^ Background colors
  -> Int
  -- ^ Number of blanks
  -> Nibble
blanks bk c = Nibble (Left (Spaces c bk))

-- # Rendering

-- | Convert a 'Box' to Rainbow 'Chunk's.  You can then print it, as
-- described in "Rainbow".
render :: Box -> Seq Chunk
render (Box bx) = case bx of
  NoHeight _ -> Seq.empty
  WithHeight rw ->
    join . join . fmap ( <| Seq.singleton (Seq.singleton "\n"))
    . fmap renderRod $ rw

renderRod :: Rod -> Seq Chunk
renderRod (Rod rd) = fmap toChunk rd
  where
    toChunk = either spcToChunk id . unNibble
    spcToChunk ss =
      chunkFromText (X.replicate (numSpaces ss) (X.singleton ' '))
      <> back (spcBackground ss)

-- # Helpers

-- | Split a number into two parts, so that the sum of the two parts
-- is equal to the original number.
split :: Int -> (Int, Int)
split i = (r, r + rm)
  where
    (r, rm) = i `quotRem` 2

