module Rainbox.Array2dTests where

import Test.Tasty
import Test.QuickCheck
import Data.Array
import Rainbox.Array2d

-- | Generates a two-dimensional array of Int.  The size of the
-- array depends on the size parameter.
genArray :: Gen (Array (Int, Int) Int)
genArray = do
  bnds <- genBounds
  let nElems = rangeSize bnds
  es <- vectorOf nElems arbitraryBoundedIntegral
  return $ listArray bnds es

-- | Generates array bounds.  The size of the bounds depends on the
-- size parameter.
genBounds :: Gen ((Int, Int), (Int, Int))
genBounds = do
  w:x:y:z:[] <- vectorOf 4 arbitrarySizedIntegral
  let (minC, maxC) | w < x = (w, x)
                   | otherwise = (x, w)
      (minR, maxR) | y < z = (y, z)
                   | otherwise = (z, y)
  return ((minC, minR), (maxC, maxR))

-- # Properties

-- | Bounds of columns in a Table matches those of the cells
propTableColsBounds
  :: (Ix col, Ix row)
  => Table lCol lRow col row a
  -> Bool
propTableColsBounds tbl = bounds cls == tgtBounds
  where
    cls = lCols tbl
    ((minC, _), (maxC, _)) = bounds . cells $ tbl
    tgtBounds = (minC, maxC)

-- | Bounds of rows in a Table matches those of the cells
propTableRowsBounds
  :: (Ix col, Ix row)
  => Table lCol lRow col row a
  -> Bool
propTableRowsBounds tbl = bounds rws == tgtBounds
  where
    rws = lRows tbl
    ((_, minR), (_, maxR)) = bounds . cells $ tbl
    tgtBounds = (minR, maxR)

-- | Generating a table using the contents of the rows as labels
-- allows reconstruction of the original array

propGenRebuildByRow
  :: (Ix col, Ix row, Eq a)
  => Array (col, row) a
  -> Bool
propGenRebuildByRow ay = ay == ay'
  where
    ay' = array (bounds ay) . concat . elems . lRows
      . table (\_ _ -> ()) fRow $ ay
    fRow rw ls = map g ls
      where
        g (col, a) = ((col, rw), a)

-- | Generating a table using the contents of the columns as labels
-- allows reconstruction of the original array

propGenRebuildByCol
  :: (Ix col, Ix row, Eq a)
  => Array (col, row) a
  -> Bool
propGenRebuildByCol ay = ay == ay'
  where
    ay' = array (bounds ay) . concat . elems . lCols
      . table fCol (\_ _ -> ()) $ ay
    fCol cl ls = map g ls
      where
        g (rw, a) = ((cl, rw), a)

-- | Round-tripping through rows and arrayByRows
propRoundTripRows
  :: Eq a
  => Array (Int, Int) a
  -> Bool
propRoundTripRows ay = ay' == ay
  where
    ay' = arrayByRows . rows $ ay

-- | Round-tripping through columns and arrayByCols
propRoundTripCols
  :: Eq a
  => Array (Int, Int) a
  -> Bool
propRoundTripCols ay = ay' == ay
  where
    ay' = arrayByCols . cols $ ay

-- | mapRowLabels does not change column labels
propMapRowLabelsCols
  :: (Ix col, Ix row, Eq lCol)
  => (lRow -> row -> [(lCol, col, a)] -> lRow')
  -> Table lCol lRow col row a
  -> Bool
propMapRowLabelsCols f tb = lbls == lbls'
  where
    lbls = lCols tb
    tb' = mapRowLabels f tb
    lbls' = lCols tb'

-- | mapRowLabels does not change cells
propMapRowLabelsCells
  :: (Ix col, Ix row, Eq a, Eq lCol)
  => (lRow -> row -> [(lCol, col, a)] -> lRow')
  -> Table lCol lRow col row a
  -> Bool
propMapRowLabelsCells f tb = ay == ay'
  where
    ay = cells tb
    tb' = mapRowLabels f tb
    ay' = cells tb'

-- | mapRowLabels permits reconstruction of original array
propMapRowLabelsRebuild
  :: (Ix col, Ix row, Eq a)
  => Table lCol lRow col row a
  -> Bool
propMapRowLabelsRebuild t = ay == ay'
  where
    ay = cells t
    ay' = array (bounds ay) . concat . elems
      . lRows . mapRowLabels f $ t
    f _ rw ls = map g ls
      where
        g (_, cl, a) = ((cl, rw), a)

-- | mapRowLabels gives the original row labels
propMapRowLabelsRelabel
  :: (Ix col, Ix row, Eq a, Eq lRow, Eq lCol)
  => Table lCol lRow col row a
  -> Bool
propMapRowLabelsRelabel t = t == t'
  where
    t' = mapRowLabels (\r _ _ -> r) t

tests :: TestTree
tests = testGroup "Array2d" []

