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

genRows :: Gen Rows
genRows = fmap Rows $ frequency [(3, nonNeg), (1, neg)]
  where
    nonNeg = fmap getNonNegative arbitrarySizedIntegral
    neg = fmap (negate . getPositive) arbitrarySizedIntegral

genCols :: Gen Cols
genCols = fmap Cols $ frequency [(3, nonNeg), (1, neg)]
  where
    nonNeg = fmap getNonNegative arbitrarySizedIntegral
    neg = fmap (negate . getPositive) arbitrarySizedIntegral

genBackground :: Gen Background
genBackground = liftM2 Background G.colors8 G.colors256

-- | Generates blank Box.
genBox :: Gen Box
genBox = liftM3 blank genBackground rw cl
  where
    rw = fmap (Rows . getNonNegative) arbitrarySizedIntegral
    cl = fmap (Cols . getNonNegative) arbitrarySizedIntegral

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
    x:xs -> all (== cols x) . map cols $ xs

biggest :: Int -> Gen a -> Gen a
biggest m g = sized $ \s -> resize (min s m) g

data BlankInputs = BlankInputs
  { biBackground :: Background
  , biRows :: Rows
  , biCols :: Cols
  } deriving Show

instance Arbitrary BlankInputs where
  arbitrary = BlankInputs <$> genBackground <*> genRows <*> genCols

data ChunksInputs = ChunksInputs
  { ciChunks :: [Chunk] }
  deriving Show

instance Arbitrary ChunksInputs where
  arbitrary = ChunksInputs <$> listOf genChunk

data CatHInputs = CatHInputs
  { hcBackground :: Background
  , hcAlign :: Align Vert
  , hcBoxes :: [Box]
  } deriving Show

instance Arbitrary CatHInputs where
  arbitrary = liftM3 CatHInputs genBackground genAlignVert
    (listOf genBox)

data CatVInputs = CatVInputs
  { vcBackground :: Background
  , vcAlign :: Align Horiz
  , vcBoxes :: [Box]
  } deriving Show

instance Arbitrary CatVInputs where
  arbitrary = liftM3 CatVInputs genBackground genAlignHoriz
    (listOf genBox)

data ViewInputs = ViewInputs
  { viRows :: Rows
  , viCols :: Cols
  , viHoriz :: Align Horiz
  , viVert :: Align Vert
  , viBox :: Box
  } deriving Show

instance Arbitrary ViewInputs where
  arbitrary = liftM5 ViewInputs genRows genCols genAlignHoriz
    genAlignVert genBox

tests :: TestTree
tests = testGroup "BoxTests"
  [ testGroup "blank"
    [ testProperty "makes valid Box" $
      \(BlankInputs bk rw cl) ->
      validBox (blank bk rw cl)

    , testProperty "has right number of rows" $
      \(BlankInputs bk rw@(Rows n) cl) ->
      let numRows | n <= 0 = Rows 0
                  | otherwise = rw
      in (== numRows) . rows $ blank bk rw cl

    , testProperty "has right number of columns" $
      \(BlankInputs bk rw@(Rows nr) cl@(Cols nc)) ->
      let numCols | nr <= 0 = Cols 0
                  | nc <= 0 = Cols 0
                  | otherwise = cl
      in (== numCols) . cols $ blank bk rw cl
    ]

  , testGroup "chunk"
    [ testProperty "makes valid Box" $
      validBox . chunks . ciChunks

    , testProperty "makes Box whose height is 1" $
      (== Rows 1) . rows . chunks . ciChunks

    , testProperty "makes Box with cols == number of characters" $
      \(ChunksInputs ci) ->
      let nChars = sum . map X.length . map text $ ci
      in (== Cols nChars) . cols . chunks $ ci
    ]

  , testGroup "catH"
    [ testProperty "makes valid Box" $
      \(CatHInputs bk a bs) -> validBox $ catH bk a bs

    , testProperty "is as tall as tallest box" $
      \(CatHInputs bk a bs) ->
      let h = maximum . (Rows 0 :) . map rows $ bs
      in (== h) . rows $ catH bk a bs

    , testProperty "is as wide as sum of all widths" $
      \(CatHInputs bk a bs) ->
      let s = sum . map (unCols . cols) $ bs
      in (== s) . unCols . cols $ catH bk a bs
    ]

  , testGroup "catV"
    [ testProperty "makes a valid Box" $
      \(CatVInputs bk a bs) ->
      validBox $ catV bk a bs

    , testProperty "is as tall as the sum of all heights" $
      \(CatVInputs bk a bs) ->
      let h = sum . map (unRows . rows) $ bs
      in (== h) . unRows . rows $ catV bk a bs

    , testProperty "is as wide as the widest box" $
      \(CatVInputs bk a bs) ->
      let w = maximum . (Cols 0:) . map cols $ bs
      in (== w) . cols $ catV bk a bs
    ]

  , testGroup "view"
    [ testProperty "makes a valid Box" $
      \(ViewInputs r c h v b) -> validBox $ view r c h v b

    , testProperty "number of rows does not increase" $
      \(ViewInputs r c h v b) ->
      (<= rows b) . rows $ view r c h v b

    , testProperty "number of columns does not increase" $
      \(ViewInputs r c h v b) ->
      (<= cols b) . cols $ view r c h v b

    , testProperty "number of rows <= number requested" $
      \(ViewInputs r c h v b) ->
      (<= max (Rows 0) r) . rows $ view r c h v b

    , testProperty "number of columns <= number requested" $
      \(ViewInputs r c h v b) ->
      (<= max (Cols 0) c) . cols $ view r c h v b
    ]
  ]

