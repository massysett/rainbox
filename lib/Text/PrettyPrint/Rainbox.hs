{-# LANGUAGE OverloadedStrings #-}
module Text.PrettyPrint.Rainbox where

import System.Console.Rainbow
import Text.PrettyPrint.Rainbox.Box


--
-- # Glueing
--

-- | @hsep sep a bs@ lays out @bs@ horizontally with alignment @a@,
--   with @sep@ amount of space in between each.
hsep :: Background -> Int -> Align Vert -> [Box] -> Box
hsep bk sep a bs = undefined

-- | @vsep sep a bs@ lays out @bs@ vertically with alignment @a@,
--   with @sep@ amount of space in between each.
vsep :: Int -> Background -> Align Horiz -> [Box] -> Box
vsep sep b a bs = undefined

-- | @punctuateH a p bs@ horizontally lays out the boxes @bs@ with a
--   copy of @p@ interspersed between each.
punctuateH :: Background -> Align Vert -> Box -> [Box] -> Box
punctuateH b a p bs = undefined

-- | A vertical version of 'punctuateH'.
punctuateV :: Background -> Align Horiz -> Box -> [Box] -> Box
punctuateV b a p bs = undefined

-- | Paste two boxes together horizontally with a single intervening
--   column of space, using a default (top) alignment.
(<+>) :: Box -> Box -> Box
l <+> r = undefined

-- | Paste two boxes together vertically, using a default (left)
--   alignment.
(//) :: Box -> Box -> Box
t // b = undefined

-- | Paste two boxes together vertically with a single intervening row
--   of space, using a default (left) alignment.
(/+/) :: Box -> Box -> Box
t /+/ b = undefined

render :: Box -> [Chunk]
render = concat . concat . map (: [["\n"]]) . map unRow . unBox

printBox :: Box -> IO ()
printBox = putChunks . render
