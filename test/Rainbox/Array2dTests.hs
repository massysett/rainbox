module Rainbox.Array2dTests where

import Test.Tasty
import Test.QuickCheck
import Test.Tasty.QuickCheck (testProperty)
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

genTable :: Gen (Table (Int, [(Int, Int)]) (Int, [(Int, Int)]) Int Int Int)
genTable = do
  ay <- genArray
  return $ table (,) (,) ay

type LabelF
  = (Int, [(Int, Int)])
  -> (Int, [(Int, Int)])
  -> Int -> Int -> Int -> Int

type ChangeLabelF
  = (Int, [(Int, Int)])
  -> Int
  -> [((Int, [(Int, Int)]), Int, Int)]
  -> Int

genLabelF :: Gen LabelF
genLabelF = arbitrary

genChangeLabelF :: Gen ChangeLabelF
genChangeLabelF = arbitrary

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
propRoundTripRows ay = sameShape ay ay'
  where
    ay' = arrayByRows . rows $ ay

-- | Round-tripping through columns and arrayByCols
propRoundTripCols
  :: (Eq a, Show a)
  => Array (Int, Int) a
  -> Bool
propRoundTripCols ay = sameShape ay ay'
  where
    ay' = arrayByCols . cols $ ay

-- | True if both arrays have the same shape; that is, the same
-- number of rows and the same number of columns and the same
-- elements.

sameShape
  :: (Ix col, Ix row, Eq a)
  => Array (col, row) a
  -> Array (col, row) a
  -> Bool
sameShape x y = rx == ry && cx == cy && ex == ey
  where
    ((minCx, minRx), (maxCx, maxRx)) = bounds x
    ((minCy, minRy), (maxCy, maxRy)) = bounds y
    rx = rangeSize (minRx, maxRx)
    ry = rangeSize (minRy, maxRy)
    cx = rangeSize (minCx, maxCx)
    cy = rangeSize (minCy, maxCy)
    ex = elems x
    ey = elems y

-- # mapTable properties

-- | mapTable does not change lCols
mapTableNoChangeCols
  :: (Ix col, Ix row, Eq lCol)
  => (lCol -> lRow -> col -> row -> a -> b)
  -> Table lCol lRow col row a
  -> Bool
mapTableNoChangeCols f t = lCols t == lCols t'
  where
    t' = mapTable f t

-- | mapTable does not change lRows
mapTableNoChangeRows
  :: (Ix col, Ix row, Eq lRow)
  => (lCol -> lRow -> col -> row -> a -> b)
  -> Table lCol lRow col row a
  -> Bool
mapTableNoChangeRows f t = lRows t == lRows t'
  where
    t' = mapTable f t

-- | mapTable allows rebuild of original array
mapTableRebuildNoIndices
  :: (Ix col, Ix row, Eq a)
  => Table lCol lRow col row a
  -> Bool
mapTableRebuildNoIndices tbl = cells tbl == ay'
  where
    ay' = listArray (bounds . cells $ tbl) . elems . cells
        . mapTable f $ tbl
    f _ _ _ _ a = a

mapTableRebuildWithIndices
  :: (Ix col, Ix row, Eq a)
  => Table lCol lRow col row a
  -> Bool
mapTableRebuildWithIndices tbl = cells tbl == ay'
  where
    ay' = array (bounds . cells $ tbl) . elems . cells
      . mapTable f $ tbl
    f _ _ cl rw a = ((cl, rw), a)

-- # labelRows and labelCols properties

-- | labelCols allows rebuild of original array
propLabelColsRebuild
  :: (Ix col, Ix row, Eq a)
  => Array (col, row) a
  -> Bool
propLabelColsRebuild ay = ay == ay'
  where
    ay' = array (bounds ay) . concat . elems
      . labelCols f $ ay
    f cl ls = map g ls
      where
        g (rw, a) = ((cl, rw), a)

-- | labelRows allows rebuild of original array
propLabelRowsRebuild
  :: (Ix col, Ix row, Eq a)
  => Array (col, row) a
  -> Bool
propLabelRowsRebuild ay = ay == ay'
  where
    ay' = array (bounds ay) . concat . elems
      . labelRows f $ ay
    f rw ls = map g ls
      where
        g (cl, a) = ((cl, rw), a)

-- # mapRowLabels properties

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

-- # mapColLabels properties

-- | mapColLabels does not change row labels
propMapColLabelsCols
  :: (Ix col, Ix row, Eq lRow)
  => (lCol -> col -> [(lRow, row, a)] -> lCol')
  -> Table lCol lRow col row a
  -> Bool
propMapColLabelsCols f tb = lbls == lbls'
  where
    lbls = lRows tb
    tb' = mapColLabels f tb
    lbls' = lRows tb'

-- | mapColLabels does not change cells
propMapColLabelsCells
  :: (Ix col, Ix row, Eq a, Eq lCol)
  => (lCol -> col -> [(lRow, row, a)] -> lCol')
  -> Table lCol lRow col row a
  -> Bool
propMapColLabelsCells f tb = ay == ay'
  where
    ay = cells tb
    tb' = mapColLabels f tb
    ay' = cells tb'

-- | mapColLabels permits reconstruction of original array
propMapColLabelsRebuild
  :: (Ix col, Ix row, Eq a)
  => Table lCol lRow col row a
  -> Bool
propMapColLabelsRebuild t = ay == ay'
  where
    ay = cells t
    ay' = array (bounds ay) . concat . elems
      . lCols . mapColLabels f $ t
    f _ cl ls = map g ls
      where
        g (_, rw, a) = ((cl, rw), a)

-- | mapColLabels gives the original row labels
propMapColLabelsRelabel
  :: (Ix col, Ix row, Eq a, Eq lRow, Eq lCol)
  => Table lCol lRow col row a
  -> Bool
propMapColLabelsRelabel t = t == t'
  where
    t' = mapColLabels (\r _ _ -> r) t

tests :: TestTree
tests = testGroup "Array2d"
  [ testProperty "bounds of columns in Table matches those of cells" $
    forAll genTable $
    propTableColsBounds

  , testProperty "bounds of rows in Table matches those of cells" $
    forAll genTable $
    propTableRowsBounds

  , testProperty "propGenRebuildByRow" $
    forAll genArray propGenRebuildByRow

  , testProperty "propGenRebuildByCol" $
    forAll genArray propGenRebuildByCol

  , testProperty "propRoundTripRows" $
    forAll genArray propRoundTripRows

  , testProperty "propRoundTripCols" $
    forAll genArray propRoundTripCols

  , testProperty "mapTableNoChangeCols" $
    forAll genLabelF $ \f ->
    forAll genTable $ \t ->
    mapTableNoChangeCols f t

  , testProperty "mapTableNoChangeRows" $
    forAll genLabelF $ \f ->
    forAll genTable $ \t ->
    mapTableNoChangeRows f t

  , testProperty "mapTableRebuildNoIndices" $
    forAll genTable mapTableRebuildNoIndices

  , testProperty "mapTableRebuildWithIndices" $
    forAll genTable mapTableRebuildWithIndices

  , testProperty "propLabelColsRebuild" $
    forAll genArray propLabelColsRebuild

  , testProperty "propLabelRowsRebuild" $
    forAll genArray propLabelRowsRebuild

  , testProperty "propMapRowLabelsCols" $
    forAll genChangeLabelF $ \f ->
    forAll genTable $ \t ->
    propMapRowLabelsCols f t

  , testProperty "propMapRowLabelsCells" $
    forAll genChangeLabelF $ \f ->
    forAll genTable $ \t ->
    propMapRowLabelsCells f t

  , testProperty "propMapRowLabelsRebuild" $
    forAll genTable propMapRowLabelsRebuild

  , testProperty "propMapRowLabelsRelabel" $
    forAll genTable propMapRowLabelsRelabel

  , testProperty "propMapColLabelsCols" $
    forAll genChangeLabelF $ \f ->
    forAll genTable $ \t ->
    propMapColLabelsCols f t

  , testProperty "propMapColLabelsCells" $
    forAll genChangeLabelF $ \f ->
    forAll genTable $ \t ->
    propMapColLabelsCells f t

  , testProperty "propMapColLabelsRebuild" $
    forAll genTable propMapColLabelsRebuild

  , testProperty "propMapColLabelsRelabel" $
    forAll genTable propMapColLabelsRelabel

  ]

