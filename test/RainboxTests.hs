module RainboxTests where

import Rainbox
import Rainbox.BoxTests
import qualified Data.Text as X
import Control.Applicative
import Control.Monad
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty
import Test.QuickCheck
import System.Console.Rainbow

tests :: TestTree
tests = testGroup "RainboxTests"
  [ testGroup "blankH"
    [ testProperty "makes Box with no height" $ \i ->
      (== 0) . height $ blankH (iBackground i) (unWidth . iWidth $ i)

    , testProperty "makes Box with correct width" $ \i ->
      let w = unWidth . iWidth $ i
      in (== w) . width $ blankH (iBackground i) w
    ]

  , testGroup "blankV"
    [ testProperty "makes Box with no width" $ \i ->
      (== 0) . width $ blankV (iBackground i) (unHeight . iHeight $ i)

    , testProperty "makes Box with correct height" $ \i ->
      let h = unHeight . iHeight $ i
      in (== h) . height $ blankV (iBackground i) h
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
          tgt = unWidth . iWidth $ i
      in (\w -> w == width bx || w == tgt) . width $
            growH (iBackground i) tgt (iHoriz i) bx

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
          tgt = unHeight . iHeight $ i
      in (\h -> h == height bx || h == tgt) . height $
            growV (iBackground i) tgt (iVert i) bx

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
  ]
