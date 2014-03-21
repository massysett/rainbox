{-# LANGUAGE OverloadedStrings #-}
module Rainbox.Box
  ( -- * Backgrounds
    Background(..)
  , defaultBackground
  , backgroundFromTextSpec
  , same
  
  -- * Height and columns
  , Height(..)
  , B.height
  , Width(..)
  , B.HasWidth(..)

  -- * Alignment
  , Align
  , Vert
  , Horiz
  , B.center
  , B.top
  , B.bottom
  , B.left
  , B.right

  -- * Box properties
  , B.Bar(..)
  , B.barToBox
  , B.barsToBox
  , B.Box
  , B.unBox

  -- * Making Boxes
  , B.blank
  , blankH
  , blankV
  , B.chunks
  , chunk

  -- * Pasting Boxes together
  , B.catH
  , B.catV
  , sepH
  , sepV
  , punctuateH
  , punctuateV

  -- * Viewing Boxes
  , view
  , B.viewH
  , B.viewV

  -- * Growing Boxes
  , grow
  , growH
  , growV
  , column

  -- * Resizing
  , resize
  , resizeH
  , resizeV

  -- * Printing Boxes
  , render
  , printBox
  ) where

import Data.Monoid
import Data.List (intersperse)
import qualified Data.Text as X
import System.Console.Rainbow
import System.Console.Rainbow.Types
import System.Console.Rainbow.Colors
import qualified Rainbox.Box.Primitives as B
import Rainbox.Box.Primitives
  ( Box
  , Align
  , Horiz
  , Vert
  , Height(..)
  , Width(..)
  , Background
  , unBox
  )
import qualified System.IO as IO

backgroundFromTextSpec :: TextSpec -> B.Background
backgroundFromTextSpec ts = B.Background bk8 bk256
  where
    bk8 = case getLast . background8 . style8 $ ts of
      Nothing -> c8_default
      Just c -> c
    bk256 = case getLast . background256 . style256 $ ts of
      Nothing -> c256_default
      Just c -> c

backgroundToTextSpec :: B.Background -> TextSpec
backgroundToTextSpec (B.Background bk8 bk256) = TextSpec
  { style8 = mempty { background8 = Last . Just $ bk8 }
  , style256 = mempty { background256 = Last . Just $ bk256 } }

-- | Use the default background colors of the current terminal.
defaultBackground :: B.Background
defaultBackground = B.Background c8_default c256_default

-- | Use the same color for 8 and 256-color backgrounds.
same :: Color8 -> B.Background
same c = B.Background c (to256 c)

--
-- # Box making
--

-- | A blank horizontal box with a given width and no height.
blankH :: Background -> Int -> Box
blankH bk i = B.blank bk (Height 0) (Width i)

-- | A blank vertical box with a given length.
blankV :: Background -> Int -> Box
blankV bk i = B.blank bk (Height i) (Width 0)

-- | A Box made of a single 'Chunk'.
chunk :: Chunk -> Box
chunk = B.chunks . (:[])

-- | Grow a box.  Each dimension of the result 'Box' is never smaller
-- than the corresponding dimension of the input 'Box'.  Analogous to
-- 'view', so you give the resulting dimensions that you want.  The
-- alignment is analogous to 'view'; for instance, if you specify
-- that the alignment is 'top' and 'left', the extra padding is
-- added to the right and bottom sides of the resulting 'Box'.

grow
  :: Background
  -> Height
  -> Width
  -> Align Vert
  -> Align Horiz
  -> Box
  -> Box
grow bk (B.Height h) (B.Width w) av ah
  = growH bk w ah
  . growV bk h av

-- | Grow a 'Box' horizontally.

growH
  :: Background
  -> Int
  -- ^ Resulting width
  -> Align Horiz
  -> Box
  -> Box
growH bk tgtW a b
  | tgtW < w = b
  | otherwise = B.catH bk B.top [lft, b, rt]
  where
    w = B.width b
    diff = tgtW - w
    (lft, rt) = (blankH bk wl, blankH bk wr)
    (wl, wr)
      | a == B.center = B.split diff
      | a == B.left = (0, diff)
      | otherwise = (diff, 0)

-- | Grow a 'Box' vertically.
growV
  :: Background
  -> Int
  -- ^ Resulting height
  -> Align Vert
  -> Box
  -> Box
growV bk tgtH a b
  | tgtH < h = b
  | otherwise = B.catV bk B.left [tp, b, bt]
  where
    h = B.height b
    diff = tgtH - h
    (tp, bt) = (blankV bk ht, blankV bk hb)
    (ht, hb)
      | a == B.center = B.split diff
      | a == B.top = (0, diff)
      | otherwise = (diff, 0)

-- | Returns a list of 'Box', each being exactly as wide as the
-- widest 'Box' in the input list.
column
  :: Background
  -> Align Horiz
  -> [Box]
  -> [Box]
column bk ah bs = map (growH bk w ah) bs
  where
    w = maximum . (0:) . map B.width $ bs

view
  :: Height
  -> Width
  -> Align Vert
  -> Align Horiz
  -> Box
  -> Box
view h w av ah
  = B.viewH (B.unWidth w) ah
  . B.viewV (B.unHeight h) av

--
-- # Resizing
--

-- | Resize a 'Box'.  Will grow or trim it as necessary in order to
-- reach the resulting size.  Returns an empty 'Box' if either
-- 'Height' or 'Width' is less than 1.

resize
  :: Background
  -> Height
  -> Width
  -> Align Horiz
  -> Align Vert
  -> Box
  -> Box
resize bk h w ah av
  = resizeH bk (unWidth w) ah
  . resizeV bk (unHeight h) av

-- | Resize horizontally.
resizeH
  :: Background
  -> Int
  -- ^ Resulting width
  -> Align Horiz
  -> Box
  -> Box
resizeH bk w a b
  | bw < w = growH bk w a b
  | bw > w = B.viewH w a b
  | otherwise = b
  where
    bw = B.width b

-- | Resize vertically.
resizeV
  :: Background
  -> Int
  -- ^ Resulting height
  -> Align Vert
  -> Box
  -> Box
resizeV bk h a b
  | bh < h = growV bk h a b
  | bh > h = B.viewV h a b
  | otherwise = b
  where
    bh = B.height b

--
-- # Glueing
--

-- | @sepH sep a bs@ lays out @bs@ horizontally with alignment @a@,
--   with @sep@ amount of space in between each.
sepH :: Background -> Int -> Align Vert -> [Box] -> Box
sepH bk sep a = punctuateH bk a bl
  where
    bl = blankH bk sep

-- | @sepV sep a bs@ lays out @bs@ vertically with alignment @a@,
--   with @sep@ amount of space in between each.
sepV :: Background -> Int -> Align Horiz -> [Box] -> Box
sepV bk sep a = punctuateV bk a bl
  where
    bl = blankV bk sep

-- | @punctuateH a p bs@ horizontally lays out the boxes @bs@ with a
--   copy of @p@ interspersed between each.
punctuateH :: Background -> Align Vert -> Box -> [Box] -> Box
punctuateH bk a sep = B.catH bk a . intersperse sep

-- | A vertical version of 'punctuateH'.
punctuateV :: Background -> Align Horiz -> Box -> [Box] -> Box
punctuateV bk a sep = B.catV bk a . intersperse sep

render :: Box -> [Chunk]
render bx = case unBox bx of
  B.NoHeight _ -> []
  B.WithHeight rw ->
    concat . concat . map (: [["\n"]])
    . map renderRod $ rw

renderRod :: B.Rod -> [Chunk]
renderRod = map toChunk . B.unRod
  where
    toChunk = either spcToChunk id . B.unNibble
    spcToChunk ss =
      Chunk (backgroundToTextSpec (B.spcBackground ss))
            (X.replicate (B.numSpaces ss) (X.singleton ' '))

-- | Prints a Box to standard output.  If standard output is not a
-- terminal, no colors are used.  Otherwise, colors are used if your
-- TERM environment variable suggests they are available.
printBox :: Box -> IO ()
printBox b = do
  t <- smartTermFromEnv IO.stdout
  hPutChunks IO.stdout t . render $ b

