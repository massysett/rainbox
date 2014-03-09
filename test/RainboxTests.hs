module RainboxTests where

import Rainbox
import qualified Data.Text as X
import Control.Applicative
import Control.Monad
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty
import qualified Rainbox.BoxTests as BT
import Test.QuickCheck
import System.Console.Rainbow

data BlankInputs = BlankInputs
  { biBackground :: Background
  , biSize :: Int
  } deriving Show

instance Arbitrary BlankInputs where
  arbitrary = liftM2 BlankInputs BT.genBackground arbitrarySizedIntegral

data GrowInputs = GrowInputs
  { giBackground :: Background
  , giRows :: Rows
  , giCols :: Cols
  , giHoriz :: Align Horiz
  , giVert :: Align Vert
  , giBox :: Box
  } deriving Show

instance Arbitrary GrowInputs where
  arbitrary =
    GrowInputs
    <$> BT.genBackground
    <*> BT.genRows
    <*> BT.genCols
    <*> BT.genAlignHoriz
    <*> BT.genAlignVert
    <*> BT.genBox

tests :: TestTree
tests = testGroup "RainboxTests"
  [ testGroup "blankH"
    [ testProperty "makes Box of correct length" $
      \(BlankInputs bk sz) ->
      let f | sz < 0 = cs == 0
            | otherwise = cs == sz
          cs = unCols . cols $ bx
          bx = blankH bk sz
      in f

    , testProperty "makes Box of correct height" $
      \(BlankInputs bk sz) ->
      let f | sz < 1 = rs == 0
            | otherwise = rs == 1
          rs = unRows . rows $ bx
          bx = blankH bk sz
      in f
    ]

  , testGroup "blankV"
    [ testProperty "makes Box of correct length" $
      \(BlankInputs bk sz) ->
      let f | sz < 1 = cs == 0
            | otherwise = cs == 1
          cs = unCols . cols $ bx
          bx = blankV bk sz
      in f

    , testProperty "makes Box of correct height" $
      \(BlankInputs bk sz) ->
      let f | sz < 1 = rs == 0
            | otherwise = rs == sz
          rs = unRows . rows $ bx
          bx = blankV bk sz
      in f
    ]

  , testGroup "chunk"
    [ testProperty "makes Box of correct height" $
      forAll BT.genChunk $
      (== 1) . unRows . rows . chunk

    , testProperty "makes Box of correct length" $
      forAll BT.genChunk $ \c ->
      let len = X.length . text $ c
      in (== len) . unCols . cols . chunk $ c
    ]

  , testGroup "grow"
    [ testProperty "makes Box of correct height" $
      \(GrowInputs bk rw cl ah av b) -> case () of
      _ | rw < rows b -> rows r == rows b
        | otherwise -> rows r == rw
        where
          r = grow bk rw cl ah av b

    , testProperty "makes Box of correct width" $
      \(GrowInputs bk rw cl ah av b) -> case () of
      _ | cl < cols b -> cols r == cols b
        | otherwise -> cols r == cl
        where
          r = grow bk rw cl ah av b
    ]

  , testGroup "growH"
    [ testProperty "makes Box of correct width" $
      \(GrowInputs bk _ cl ah _ b) -> case () of
      _ | cl < cols b -> cols r == cols b
        | otherwise -> cols r == cl
        where
          r = growH bk (unCols cl) ah b

    , testProperty "does not change Box height" $
      \(GrowInputs bk _ cl ah _ b) ->
      (== cl) . cols $ growH bk (unCols cl) ah b
    ]

  , testGroup "growV"
    [ testProperty "makes Box of correct height" $
      \(GrowInputs bk rw _ _ av b) -> case () of
      _ | rw < rows b -> rows r == rows b
        | otherwise -> rows r == rw
        where
          r = growV bk (unRows rw) av b

    , testProperty "does not change Box width" $
      \(GrowInputs bk rw _ _  av b) ->
      (== rw) . rows $ growV bk (unRows rw) av b
    ]

  , testGroup "viewH"
    [ testProperty "makes Box of correct width" $
      \(GrowInputs _ _ cl ah _ b) -> case () of
      _ | cl < Cols 1 -> cols r == Cols 0
        | otherwise -> cols r == cl
        where
          r = viewH (unCols cl) ah b
    ]
  ]
