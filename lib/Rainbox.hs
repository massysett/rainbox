{-# LANGUAGE OverloadedStrings #-}
module Rainbox where

import System.Console.Rainbow
import qualified Rainbox.Box as B
import Rainbox.Box
  ( Box
  , Align
  , Horiz
  , Vert
  , Rows
  , Cols
  , Background
  , unRow
  , unBox
  )
import qualified System.IO as IO

--
-- # Box making
--

-- | A blank horizontal box with a given length.
blankH :: Background -> Int -> Box
blankH = undefined

-- | A blank vertical box with a given length.
blankV :: Background -> Int -> Box
blankV = undefined

-- | Grow a box.  Each dimension of the result 'Box' is never smaller
-- than the corresponding dimension of the input 'Box'.  Analogous to
-- 'view', so you give the resulting dimensions that you want.  The
-- alignment is analogous to 'view'; for instance, if you specify
-- that the alignment is 'top' and 'left', the extra padding is
-- added to the right and bottom sides of the resulting 'Box'.

grow
  :: Background
  -> Rows
  -> Cols
  -> Align Horiz
  -> Align Vert
  -> Box
  -> Box
grow = undefined

-- | Grow a 'Box' horizontally.

growH
  :: Background
  -> Int
  -- ^ Resulting width
  -> Align Horiz
  -> Box
  -> Box
growH = undefined

-- | Grow a 'Box' vertically.
growV
  :: Background
  -> Int
  -- ^ Resulting height
  -> Align Vert
  -> Box
  -> Box
growV = undefined

-- | View a 'Box', trimming it horizontally.
viewH
  :: Int
  -- ^ Resulting width
  -> Align Horiz
  -> Box
  -> Box
viewH = undefined

-- | View a 'Box', trimming it vertically.
viewV
  :: Int
  -- ^ Resulting height
  -> Align Horiz
  -> Box
  -> Box
viewV = undefined

--
-- # Resizing
--

-- | Resize a 'Box'.  Will grow or trim it as necessary in order to
-- reach the resulting size.

resize
  :: Background
  -> Rows
  -> Cols
  -> Align Horiz
  -> Align Vert
  -> Box
  -> Box
resize = undefined

-- | Resize horizontally.
resizeH
  :: Background
  -> Int
  -- ^ Resulting width
  -> Align Horiz
  -> Box
  -> Box
resizeH = undefined

-- | Resize vertically.
resizeV
  :: Background
  -> Int
  -- ^ Resulting height
  -> Align Vert
  -> Box
  -> Box
resizeV = undefined

--
-- # Glueing
--

-- | @hsep sep a bs@ lays out @bs@ horizontally with alignment @a@,
--   with @sep@ amount of space in between each.
hsep :: Background -> Int -> Align Vert -> [Box] -> Box
hsep bk sep a bs = undefined

-- | @vsep sep a bs@ lays out @bs@ vertically with alignment @a@,
--   with @sep@ amount of space in between each.
vsep :: Background -> Int -> Align Horiz -> [Box] -> Box
vsep sep b a bs = undefined

-- | @punctuateH a p bs@ horizontally lays out the boxes @bs@ with a
--   copy of @p@ interspersed between each.
punctuateH :: Background -> Align Vert -> Box -> [Box] -> Box
punctuateH b a p bs = undefined

-- | A vertical version of 'punctuateH'.
punctuateV :: Background -> Align Horiz -> Box -> [Box] -> Box
punctuateV b a p bs = undefined

render :: Box -> [Chunk]
render = concat . concat . map (: [["\n"]]) . map unRow . unBox

-- | Prints a Box to standard output.  If standard output is not a
-- terminal, no colors are used.  Otherwise, colors are used if your
-- TERM environment variable suggests they are available.
printBox :: Box -> IO ()
printBox b = do
  t <- smartTermFromEnv IO.stdout
  hPutChunks IO.stdout t . render $ b
