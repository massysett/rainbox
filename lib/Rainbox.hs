module Rainbox
  ( -- * Backgrounds
    Background(..)
  , defaultBackground
  , backgroundFromChunk
  , same

  -- * Alignment
  , Align
  , Horiz
  , Vert
  , top
  , bottom
  , left
  , right
  , center

  -- * Bar
  , Bar(..)

  -- * Cell
  , Cell(..)

  -- * Creating Box and gluing them together
  , gridByRows
  , gridByCols
  , boxCells
  , glueBoxes

  -- * Rendering
  , render
  , printBox
  ) where

import Rainbox.Box
import Rainbox.Array2d
import Data.Array
import Data.String

-- | A 'Cell' consists of multiple screen lines; each screen line is
-- a 'Bar'.
data Cell = Cell
  { bars :: [Bar]
  , horiz :: Align Horiz
  , vert :: Align Vert
  , background :: Background
  } deriving (Eq, Show)

instance IsString Cell where
  fromString s = Cell [(fromString s)] left top defaultBackground

cellWidths :: Cell -> [Int]
cellWidths = map width . bars

boxCells
  :: (Ix col, Ix row)
  => Array (col, row) Cell
  -> Array (col, row) Box
boxCells ay = cells $ mapTable conv tbl
  where
    tbl = table getWidth getHeight ay
      where
        getWidth _ = maximum . (0:) . concat . map cellWidths . map snd
        getHeight _ = maximum . (0:) . map (length . bars . snd)
    conv lCol lRow _ _ c = grow bk (Height lRow) (Width lCol) av ah bx
      where
        Cell bs ah av bk = c
        bx = barsToBox bk ah bs

glueBoxes
  :: (Ix col, Ix row)
  => Array (col, row) Box
  -> Box
glueBoxes
  = catH defaultBackground top
  . map (catV defaultBackground left)
  . cols

gridByRows :: [[Cell]] -> Box
gridByRows = glueBoxes . boxCells . arrayByRows

gridByCols :: [[Cell]] -> Box
gridByCols = glueBoxes . boxCells . arrayByCols
