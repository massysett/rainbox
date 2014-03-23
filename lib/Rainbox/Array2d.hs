-- | Helpers for two-dimensional arrays.
module Rainbox.Array2d
  (
  -- * Tables
    Table
  , lCols
  , lRows
  , cells
  , table
  , labelCols
  , labelRows
  , mapTable
  , mapColLabels
  , mapRowLabels

  -- * Two-dimensional arrays
  , cols
  , rows
  , arrayByRows
  , arrayByCols
  ) where

import Data.Array

-- * Tables

-- | A Table is a two-dimensional array with two associated
-- one-dimensional arrays: an array of labels for each column, and
-- an array of labels for each row.
data Table lCol lRow col row a = Table
  { lCols :: Array col lCol
  -- ^ One label for each column
  , lRows :: Array row lRow
  -- ^ One label for each row
  , cells :: Array (col, row) a
  -- ^ Two-dimensional array of cells
  } deriving (Eq, Show)

instance (Ix col, Ix row) => Functor (Table lCol lRow col row) where
  fmap f t =  t { cells = fmap f . cells $ t }

-- | Make a new Table.
table
  :: (Ix col, Ix row)
  => (col -> [(row, a)] -> lCol)
  -- ^ Function to generate the column labels.  It is applied to the
  -- column index and the full contents of the column.
 
  -> (row -> [(col, a)] -> lRow)
  -- ^ Function to generate the row labels.  It is applied to the
  -- row index and the full contents of the row.

  -> Array (col, row) a
  -- ^ Cells of the table

  -> Table lCol lRow col row a
table fCol fRow ay = Table ayc ayr ay
  where
    ayc = labelCols fCol ay
    ayr = labelRows fRow ay

-- | Given a two-dimensional array and a function that generates
-- labels, return an array of column labels.
labelCols
  :: (Ix col, Ix row)
  => (col -> [(row, a)] -> lCol)
  -- ^ Function to generate the column labels.  It is applied to the
  -- column index and the full contents of the column.
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

-- | Given a two-dimensional array and a function that generates
-- labels, return an array of row labels.
labelRows
  :: (Ix col, Ix row)
  => (row -> [(col, a)] -> lRow)
  -- ^ Function to generate the row labels.  It is applied to the
  -- row index and the full contents of the row.
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

-- | Transform the cells of the table.  Similar to the Functor
-- instance, but the mapping function has access to the label and
-- index of each cell in the 'Table'.
mapTable
  :: (Ix col, Ix row)
  => (lCol -> lRow -> col -> row -> a -> b)
  -- ^ Function is passed the label for the column, the label for
  -- the row, the column index, the row index, and the contents of
  -- the cell.  It returns a new cell.
  -> Table lCol lRow col row a
  -> Table lCol lRow col row b
mapTable f (Table cs rs ls) = Table cs rs ls'
  where
    ls' = listArray (bounds ls) . map g . assocs $ ls
      where
        g ((col, row), e) = f (cs ! col) (rs ! row) col row e

-- | Transform the column labels.
mapColLabels
  :: (Ix col, Ix row)
  => (lCol -> col -> [(lRow, row, a)] -> lCol')
  -- ^ The function is passed the column label, column index, and
  -- the full contents of the column.
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

-- | Transform the row labels.
mapRowLabels
  :: (Ix col, Ix row)
  => (lRow -> row -> [(lCol, col, a)] -> lRow')
  -- ^ The function is passed the row label, the row index, and the
  -- full contents of the row.
  -> Table lCol lRow col row a
  -> Table lCol lRow' col row a
mapRowLabels f (Table cs rs ls) = Table cs rs' ls
  where
    ((colMin, rowMin), (colMax, rowMax)) = bounds ls
    rs' = listArray (rowMin, rowMax) es
      where
        es = zipWith3 f (elems rs) (indices rs) cls
          where
            cls = map mkCol . indices $ rs
              where
                mkCol idx = zipWith3 (,,) (elems cs)
                  (indices cs)
                  (map (ls !) (range ((colMin, idx), (colMax, idx))))

-- * Two-dimensional arrays

-- | Given a two-dimensional array, return a list of columns in
-- order.
cols
  :: (Ix col, Ix row)
  => Array (col, row) a
  -> [[a]]
cols ay = map getCol $ range (minCol, maxCol)
  where
    ((minCol, minRow), (maxCol, maxRow)) = bounds ay
    ixsRows = range (minRow, maxRow)
    getCol ixCol = map (\rw -> ay ! (ixCol, rw)) ixsRows

-- | Given a two-dimensional array, return a list of rows in order.
rows
  :: (Ix col, Ix row)
  => Array (col, row) a
  -> [[a]]
rows ay = map getRow $ range (minRow, maxRow)
  where
    ((minCol, minRow), (maxCol, maxRow)) = bounds ay
    ixsCols = range (minCol, maxCol)
    getRow ixRow = map (\cl -> ay ! (cl, ixRow)) ixsCols

-- | Generate a two-dimensional array from a list of rows.  Each row
-- must be of equal length; otherwise, the generated array will have
-- undefined elements.
arrayByRows
  :: [[a]]
  -> Array (Int, Int) a
arrayByRows ls = array ((0,0), (colMax, rowMax)) $ indexRows ls
  where
    rowMax = length ls - 1
    colMax = case ls of
      [] -> -1
      x:_ -> length x - 1

indexRows :: [[a]] -> [((Int, Int),a)]
indexRows = concat . map f . zip [0 ..]
  where
    f (rw, ls) = map g $ zip [0 ..] ls
      where
        g (cl, a) = ((cl, rw), a)

-- | Generate a two-dimensional array from a list of columns.  Each
-- column must be of equal length; otherwise, the generated array
-- will have undefined elements.
arrayByCols
  :: [[a]]
  -> Array (Int, Int) a
arrayByCols ls = listArray ((0,0), (colMax, rowMax)) . concat $ ls
  where
    colMax = length ls - 1
    rowMax = case ls of
      [] -> -1
      x:_ -> length x - 1

