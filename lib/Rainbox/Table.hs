module Rainbox.Table where

import Rainbox
import Data.Tuple
import Data.Array
import Data.List (transpose)
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

data Table lCol lRow col row a = Table
  { lCols :: Array col lCol
  , lRows :: Array row lRow
  , cells :: Array (col, row) a
  } deriving Show

instance (Ix col, Ix row) => Functor (Table lCol lRow col row) where
  fmap f t =  t { cells = fmap f . cells $ t }

mapTable
  :: (Ix col, Ix row)
  => (lCol -> lRow -> col -> row -> a -> b)
  -> Table lCol lRow col row a
  -> Table lCol lRow col row b
mapTable f (Table cs rs ls) = Table cs rs ls'
  where
    ls' = listArray (bounds ls) . map g . assocs $ ls
      where
        g ((col, row), e) = f (cs ! col) (rs ! row) col row e

mapColLabels
  :: (Ix col, Ix row)
  => (lCol -> col -> [(lRow, row, a)] -> lCol')
  -> Table lCol lRow col row a
  -> Table lCol' lRow col row a
mapColLabels f (Table cs rs ls) = Table cs' rs ls
  where
    ((colMin, rowMin), (colMax, rowMax)) = bounds ls
    cs' = listArray (colMin, colMax) es
      where
        es = zipWith3 f (elems cs) (indices cs) rws
          where
            rws = map mkRow . indices $ cs
              where
                mkRow idx = zipWith3 (,,) (elems rs)
                  (indices rs)
                  (map (ls !) (range ((idx, rowMin), (idx, rowMax))))

mapRowLabels
  :: (Ix col, Ix row)
  => (lRow -> row -> [(lCol, col, a)] -> lRow')
  -> Table lCol lRow col row a
  -> Table lCol lRow' col row a
mapRowLabels f (Table cs rs ls) = Table cs rs' ls
  where
    ((colMin, rowMin), (colMax, rowMax)) = bounds ls
    rs' = listArray (rowMin, rowMax) es
      where
        es = zipWith3 f (elems rs) (indices rs) cols
          where
            cols = map mkCol . indices $ rs
              where
                mkCol idx = zipWith3 (,,) (elems cs)
                  (indices cs)
                  (map (ls !) (range ((colMin, idx), (colMax, idx))))

tableByRow
  :: (Ix col, Ix row)
  => ((row, col), (row, col))
  -> [lCol]
  -> [(lRow, [a])]
  -> Table lCol lRow col row a
tableByRow (aMin, aMax) cols rws = Table cs rs ls
  where
    cs = listArray (snd aMin, snd aMax) cols
    rs = listArray (fst aMin, fst aMax) (map fst rws)
    ls = listArray (swap aMin, swap aMax)
      . concat . transpose . map snd $ rws

tableByColumn
  :: (Ix col, Ix row)
  => ((col, row), (col, row))
  -> [lRow]
  -> [(lCol, [a])]
  -> Table lCol lRow col row a
tableByColumn (aMin, aMax) rws cols = Table cs rs ls
  where
    rs = listArray (snd aMin, snd aMax) rws
    cs = listArray (fst aMin, fst aMax) (map fst cols)
    ls = listArray (aMin, aMax) . concat . map snd $ cols

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

columns
  :: (Ix col, Ix row)
  => Array (col, row) a
  -> [[a]]
columns ay = map getCol $ range (minCol, maxCol)
  where
    ((minCol, minRow), (maxCol, maxRow)) = bounds ay
    ixsRows = range (minRow, maxRow)
    getCol ixCol = map (\rw -> ay ! (ixCol, rw)) ixsRows

rows
  :: (Ix col, Ix row)
  => Array (col, row) a
  -> [[a]]
rows ay = map getRow $ range (minRow, maxRow)
  where
    ((minCol, minRow), (maxCol, maxRow)) = bounds ay
    ixsCols = range (minCol, maxCol)
    getRow ixRow = map (\cl -> ay ! (cl, ixRow)) ixsCols

labelCols
  :: (Ix col, Ix row)
  => (col -> [(row, a)] -> lCol)
  -> Array (col, row) a
  -> Array col lCol
labelCols f a = listArray (minCol, maxCol) es
  where
    ((minCol, minRow), (maxCol, maxRow)) = bounds a
    es = zipWith f ixsCols . map mkRow $ ixsCols
      where
        ixsCols = range (minCol, maxCol)
        mkRow col = zip ixsRows (map (\rw -> a ! (col, rw)) ixsRows)
          where
            ixsRows = range (minRow, maxRow)

labelRows
  :: (Ix col, Ix row)
  => (row -> [(col, a)] -> lRow)
  -> Array (col, row) a
  -> Array row lRow
labelRows f a = listArray (minRow, maxRow) es
  where
    ((minCol, minRow), (maxCol, maxRow)) = bounds a
    es = zipWith f ixsRows . map mkCol $ ixsRows
      where
        ixsRows = range (minRow, maxRow)
        mkCol row = zip ixsCols (map (\cl -> a ! (cl, row)) ixsCols)
          where
            ixsCols = range (minCol, maxCol)
