module Rainbox.BoxTests where

import Control.Monad
import Control.Applicative
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import System.Console.Rainbow
import qualified Data.Text as X
import qualified Test.Rainbow.Generators as G
import Rainbox.Box

genText :: Gen X.Text
genText = fmap X.pack $ listOf c
  where
    c = elements ['0'..'Z']

genChunk :: Gen Chunk
genChunk = genText >>= G.chunk

genHeight :: Gen Height
genHeight = fmap Height $ frequency [(3, nonNeg), (1, neg)]
  where
    nonNeg = fmap getNonNegative arbitrarySizedIntegral
    neg = fmap (negate . getPositive) arbitrarySizedIntegral

genWidth :: Gen Width
genWidth = fmap Width $ frequency [(3, nonNeg), (1, neg)]
  where
    nonNeg = fmap getNonNegative arbitrarySizedIntegral
    neg = fmap (negate . getPositive) arbitrarySizedIntegral

genBackground :: Gen Background
genBackground = liftM2 Background G.colors8 G.colors256

-- | Generates blank Box.
genBox :: Gen Box
genBox = liftM3 blank genBackground rw cl
  where
    rw = fmap (Height . getNonNegative) arbitrarySizedIntegral
    cl = fmap (Width . getNonNegative) arbitrarySizedIntegral

-- # Alignment

genAlignVert :: Gen (Align Vert)
genAlignVert = elements
  [ center, top, bottom ]

genAlignHoriz :: Gen (Align Horiz)
genAlignHoriz = elements [ center, left, right ]

validBox :: Box -> Bool
validBox box = case unBox box of
  NoHeight i -> i > -1
  WithHeight rw -> case rw of
    [] -> False
    x:xs -> all (== width x) . map width $ xs

biggest :: Int -> Gen a -> Gen a
biggest m g = sized $ \s -> resize (min s m) g

data Inputs = Inputs
  { iChunks :: [Chunk]
  , iBackground :: Background
  , iHeight :: Height
  , iWidth :: Width
  , iVert :: Align Vert
  , iHoriz :: Align Horiz
  , iBoxes :: [Box]
  , iBox :: Box
  , iChunk :: Chunk
  } deriving Show

instance Arbitrary Inputs where
  arbitrary = Inputs
    <$> listOf genChunk
    <*> genBackground
    <*> genHeight
    <*> genWidth
    <*> genAlignVert
    <*> genAlignHoriz
    <*> listOf genBox
    <*> genBox
    <*> genChunk

tests :: TestTree
tests = testGroup "BoxTests"
  [ testGroup "blank"
    [ testProperty "makes valid Box" $ \i ->
      validBox $ blank (iBackground i) (iHeight i)
        (iWidth i)

    , testProperty "has right number of rows" $ \i ->
      let ht = unHeight . iHeight $ i
      in (== max 0 ht) . height $ blank (iBackground i)
            (iHeight i) (iWidth i)

    , testProperty "has right number of columns" $ \i ->
      let wt = unWidth . iWidth $ i
      in (== max 0 wt) . width $ blank (iBackground i)
            (iHeight i) (iWidth i)
    ]

  , testGroup "chunks"
    [ testProperty "makes valid Box" $
      validBox . chunks . iChunks

    , testProperty "makes Box whose height is 1" $
      (== 1) . height . chunks . iChunks

    , testProperty "makes Box with cols == number of characters" $ \i ->
      let cks = iChunks i
          nChars = sum . map X.length . map text $ cks
      in (== nChars) . width $ chunks cks
    ]

  , testGroup "catH"
    [ testProperty "makes valid Box" $ \i ->
      validBox $ catH (iBackground i) (iVert i) (iBoxes i)

    , testProperty "is as tall as tallest box" $ \i ->
      let h = maximum . (0 :) . map height $ bs
          bs = iBoxes i
      in (== h) . height $ catH (iBackground i) (iVert i) bs

    , testProperty "is as wide as sum of all widths" $ \i ->
      let s = sum . map width $ bs
          bs = iBoxes i
      in (== s) . width $ catH (iBackground i) (iVert i) bs
    ]

  , testGroup "catV"
    [ testProperty "makes a valid Box" $ \i ->
      validBox $ catV (iBackground i) (iHoriz i) (iBoxes i)

    , testProperty "is as tall as the sum of all heights" $ \i ->
      let h = sum . map height $ bs
          bs = iBoxes i
      in (== h) . height $ catV (iBackground i) (iHoriz i) bs

    , testProperty "is as wide as the widest box" $ \i ->
      let w = maximum . (0:) . map width $ bs
          bs = iBoxes i
      in (== w) . width $ catV (iBackground i) (iHoriz i) bs
    ]

  , testGroup "viewH"
    [ testProperty "makes a valid Box" $ \i ->
      validBox $ viewH (unWidth . iWidth $ i) (iHoriz i) (iBox i)

    , testProperty "number of rows does not change" $ \i ->
      let b = iBox i
      in (== height b) . height $ viewH (unWidth . iWidth $ i)
                                    (iHoriz i) b

    , testProperty "number of columns <= number requested" $ \i ->
      let c = unWidth . iWidth $ i
          tgt = max 0 c
      in (<= tgt) . width $ viewH c (iHoriz i) (iBox i)
    ]

  , testGroup "viewV"
    [ testProperty "makes a valid Box" $ \i ->
      validBox $ viewV (unHeight . iHeight $ i) (iVert i) (iBox i)

    , testProperty "width does not change" $ \i ->
      let b = iBox i
      in (== width b) . width $ viewV (unHeight . iHeight $ i)
                                    (iVert i) b

    , testProperty "number of rows <= number requested" $ \i ->
      let r = unHeight . iHeight $ i
          tgt = max 0 r
      in (<= tgt) . height $ viewV r (iVert i) (iBox i)
    ]
  ]

