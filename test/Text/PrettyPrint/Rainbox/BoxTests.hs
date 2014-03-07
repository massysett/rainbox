module Text.PrettyPrint.Rainbox.BoxTests where

import Control.Monad
import Control.Applicative
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import System.Console.Rainbow
import qualified Data.Text as X
import qualified Test.Rainbow.Generators as G
import Text.PrettyPrint.Rainbox.Box

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

validBox :: Box -> Bool
validBox box = case unBox box of
  [] -> True
  x:xs -> all (== cols x) . map cols $ xs

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

  ]

