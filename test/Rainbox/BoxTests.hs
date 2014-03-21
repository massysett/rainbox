module Rainbox.BoxTests where

import Rainbox.Box
import Rainbox.Box.PrimitivesTests
import qualified Data.Text as X
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty
import System.Console.Rainbow
import Test.QuickCheck hiding (maxSize, resize)
import qualified Test.QuickCheck as Q

maxSize :: Int -> Q.Gen a -> Q.Gen a
maxSize s g = Q.sized $ \i -> Q.resize (min s i) g

tests :: TestTree
tests = testGroup "RainboxTests"
  [ testGroup "blankH"
    [ testProperty "makes Box with no height" $ \i ->
      (== 0) . height $ blankH (iBackground i) (unWidth . iWidth $ i)

    , testProperty "makes Box with correct width" $ \i ->
      let w = unWidth . iWidth $ i
          tgt = max 0 w
      in (== tgt) . width $ blankH (iBackground i) w
    ]

  , testGroup "blankV"
    [ testProperty "makes Box with no width" $ \i ->
      (== 0) . width $ blankV (iBackground i) (unHeight . iHeight $ i)

    , testProperty "makes Box with correct height" $ \i ->
      let h = unHeight . iHeight $ i
          tgt = max 0 h
      in (== tgt) . height $ blankV (iBackground i) h
    ]

  , testGroup "chunk"
    [ testProperty "makes Box one high" $
      (== 1) . height . chunk . iChunk

    , testProperty "makes Box as wide as characters in chunk" $ \i ->
      let cs = X.length . text . iChunk $ i
      in (== cs) . width . chunk . iChunk $ i
    ]

  , testGroup "growH"
    [ testProperty "does not change height" $ \i ->
      let bx = iBox i
      in (== height bx) . height $ growH (iBackground i)
            (unWidth . iWidth $ i) (iHoriz i) bx

    , testProperty "new Box is of correct width" $ \i ->
      let bx = iBox i
          tgt = max wdth (width bx)
          wdth = unWidth . iWidth $ i
      in (== tgt) . width $
            growH (iBackground i) wdth (iHoriz i) bx

    , testProperty "new Box is at least as wide as old Box" $ \i ->
      let bx = iBox i
      in (>= width bx) . width $ growH (iBackground i)
            (unWidth . iWidth $ i) (iHoriz i) bx
    ]

  , testGroup "growV"
    [ testProperty "does not change width" $ \i ->
      let bx = iBox i
      in (== width bx) . width $ growV (iBackground i)
            (unWidth . iWidth $ i) (iVert i) bx

    , testProperty "new Box is of correct height" $ \i ->
      let bx = iBox i
          tgt = max (height bx) hght
          hght = unHeight . iHeight $ i
      in (== tgt) . height $
            growV (iBackground i) hght (iVert i) bx

    , testProperty "new Box is at least as tall as old Box" $ \i ->
      let bx = iBox i
      in (>= height bx) . height $ growV (iBackground i)
            (unWidth . iWidth $ i) (iVert i) bx
    ]

  , testGroup "grow"
    [  testProperty "new Box is of correct width" $ \i ->
      let bx = iBox i
          tgt = unWidth . iWidth $ i
      in (\w -> w == width bx || w == tgt) . width $
            growH (iBackground i) tgt (iHoriz i) bx

    , testProperty "new Box is at least as wide as old Box" $ \i ->
      let bx = iBox i
      in (>= width bx) . width $ grow (iBackground i) (iHeight i)
            (iWidth i) (iVert i) (iHoriz i) (iBox i)

    , testProperty "new Box is of correct height" $ \i ->
      let bx = iBox i
          tgt = unHeight . iHeight $ i
      in (\h -> h == height bx || h == tgt) . height $
            grow (iBackground i) (iHeight i) (iWidth i)
                 (iVert i) (iHoriz i) (iBox i)

    , testProperty "new Box is at least as tall as old Box" $ \i ->
      let bx = iBox i
      in (>= height bx) . height $ grow (iBackground i)
            (iHeight i) (iWidth i) (iVert i) (iHoriz i) (iBox i)
    ]

  , testGroup "column"
    [ testProperty "number of inputs == number of outputs" $ \i ->
      let bs = iBoxes i
      in (== length bs) . length $ column (iBackground i) (iHoriz i) bs

    , testProperty "width of outputs is identical" $ \i ->
      case column (iBackground i) (iHoriz i) (iBoxes i) of
        [] -> True
        x:xs -> all (== width x) . map width $ xs

    , testProperty "width of output is as wide as widest input" $ \i ->
      let r = column (iBackground i) (iHoriz i) (iBoxes i)
      in case iBoxes i of
        [] -> null r
        xs -> width (head r) == (maximum . map width $ xs)
    ]

  , testGroup "resizeH"
    [ testProperty "height of resulting Box unchanged" $ \i ->
      let bx = iBox i
      in (== height bx) . height $ resizeH (iBackground i)
            (unWidth . iWidth $ i) (iHoriz i) bx

    , testProperty "result has desired width" $ \i ->
      let tgt = max 0 . unWidth . iWidth $ i
      in (== tgt) . width $ resizeH (iBackground i)
            (unWidth . iWidth $ i) (iHoriz i) (iBox i)
    ]

  , testGroup "resizeV"
    [ testProperty "width of resulting Box unchanged" $ \i ->
      let bx = iBox i
      in (== width bx) . width $ resizeV (iBackground i)
            (unHeight . iHeight $ i) (iVert i) bx

    , testProperty "result has desired height" $ \i ->
      let tgt = max 0 . unHeight . iHeight $ i
      in (== tgt) . height $ resizeV (iBackground i)
            (unHeight . iHeight $ i) (iVert i) (iBox i)
    ]

  , testGroup "resize"
    [ testProperty "result has desired height" $ \i ->
      let tgt = max 0 . unHeight . iHeight $ i
      in (== tgt) . height $ resize (iBackground i)
            (iHeight i) (iWidth i)
            (iHoriz i) (iVert i) (iBox i)

    , testProperty "result has desired width" $ \i ->
      let tgt = max 0 . unWidth . iWidth $ i
      in (== tgt) . width $ resize (iBackground i)
            (iHeight i) (iWidth i) (iHoriz i) (iVert i)
            (iBox i)
    ]

  , testGroup "punctuateH"
    [ testProperty "result has desired width" $ \i ->
      let tgt = (sum . map width $ bs)
            + width bx * (max 0 $ len - 1)
          len = length bs
          bs = iBoxes i
          bx = iBox i
      in (== tgt) . width $ punctuateH (iBackground i)
          (iVert i) bx bs
    ]

  , testGroup "punctuateV"
    [ testProperty "result has desired height" $ \i ->
      let tgt = (sum . map height $ bs)
            + height bx * (max 0 $ len - 1)
          len = length bs
          bs = iBoxes i
          bx = iBox i
      in (== tgt) . height $ punctuateV (iBackground i)
          (iHoriz i) bx bs
    ]

  -- Have to cap size on this one, which is not satisfying.  There
  -- are no apparent bugs.  Apparently what is taking so long is the
  -- Text.replicate in Box.blanks, which is applied from
  -- Box.padHoriz.
  , testGroup "sepH"
    [ testProperty "result has correct width" $
      maxSize 50 $
      forAll arbitrarySizedIntegral $ \len ->
      forAll arbitrary $ \i ->
      let tgt = (sum . map width $ bs)
            + max 0 len * max 0 (length bs - 1)
          bs = iBoxes i
      in (== tgt) . width $ sepH (iBackground i) len
          (iVert i) (iBoxes i)
    ]

  , testGroup "sepV"
    [ testProperty "result has correct height" $
      maxSize 50 $
      forAll arbitrarySizedIntegral $ \len ->
      forAll arbitrary $ \i ->
      let tgt = (sum . map height $ bs)
            + max 0 len * max 0 (length bs - 1)
          bs = iBoxes i
      in (== tgt) . height $ sepV (iBackground i) len
          (iHoriz i) (iBoxes i)
    ]
  ]
