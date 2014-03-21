module Rainbox.Table where

import Rainbox
import Rainbox.Array2d
import Data.Array
import System.Console.Rainbow

class MultiWidth a where
  multiWidth :: a -> [Int]

maxWidth :: MultiWidth a => a -> Int
maxWidth = maximum . (0:) . multiWidth

-- | Forms the basis of a 'Cell'.  A single screen line of text
-- within a single 'Cell'.
newtype Bar = Bar { unBar :: [Chunk] }
  deriving (Eq, Ord, Show)

barToBox :: Bar -> Box
barToBox = chunks . unBar

barsToBox :: Background -> Align Horiz -> [Bar] -> Box
barsToBox bk ah = catV bk ah . map barToBox

instance HasWidth Bar where
  width = sum . map width . unBar

-- | A 'Cell' consists of multiple screen lines; each screen line is
-- a 'Bar'.
data Cell = Cell
  { bars :: [Bar]
  , horiz :: Align Horiz
  , vert :: Align Vert
  , background :: Background
  } deriving (Eq, Show)

instance MultiWidth Cell where
  multiWidth = map width . bars

boxCells
  :: (Ix col, Ix row)
  => Array (col, row) Cell
  -> Array (col, row) Box
boxCells ay = cells $ mapTable conv tbl
  where
    tbl = Table (labelCols getWidth ay) (labelRows getHeight ay) ay
      where
        getWidth _ = maximum . (0:) . concat . map multiWidth . map snd
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
  . columns

grid
  :: (Ix col, Ix row)
  => Array (col, row) Cell
  -> Box
grid = glueBoxes . boxCells


