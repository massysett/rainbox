module Rainbox.ReaderTests where

import qualified Rainbox as R
import Rainbox.Reader
import Rainbox.BoxTests
import Test.QuickCheck
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty
import Data.Functor.Identity

tests :: TestTree
tests = testGroup "ReaderTests"
  [ testProperty "blankH" $ \(SpecPair i s) ->
    let p = R.blankH (iBackground i) (unWidth . iWidth $ i)
    in testEq s (blankH (unWidth . iWidth $ i)) p

  , testProperty "blankV" $ \(SpecPair i s) ->
    let p = R.blankV (iBackground i) (unHeight . iHeight $ i)
    in testEq s (blankV (unHeight . iHeight $ i)) p

  , testProperty "catH" $ \(SpecPair i s) ->
    let p = R.catH (iBackground i) (iVert i) (iBoxes i)
    in testEq s (catH (iBoxes i)) p

  , testProperty "catV" $ \(SpecPair i s) ->
    let p = R.catV (iBackground i) (iHoriz i) (iBoxes i)
    in testEq s (catV (iBoxes i)) p

  , testProperty "grow" $ \(SpecPair i s) ->
    let p = R.grow (iBackground i) (iHeight i) (iWidth i)
          (iVert i) (iHoriz i) (iBox i)
    in testEq s (grow (iHeight i) (iWidth i) (iBox i)) p
  ]

testEq :: Eq a => Specs -> Env Identity a -> a -> Bool
testEq s e a = r == a
  where
    r = runEnv s e

specs
  :: Int
  -- ^ Space for horizontal joins
  -> Int
  -- ^ Space for vertical joins
  -> Inputs
  -> Specs
specs h v i = Specs
  { background = iBackground i
  , alignH = iHoriz i
  , alignV = iVert i
  , spaceH = h
  , spaceV = v
  }

genSpecs :: Gen (Inputs, Specs)
genSpecs = do
  h <- frequency [(3, fmap getPositive arbitrarySizedIntegral),
                  (1, arbitrarySizedIntegral)]
  v <- frequency [(3, fmap getPositive arbitrarySizedIntegral),
                  (1, arbitrarySizedIntegral)]
  i <- arbitrary
  let ss = specs h v i
  return (i, ss)

data SpecPair = SpecPair
  { spInputs :: Inputs
  , spSpecs :: Specs
  } deriving Show

instance Arbitrary SpecPair where
  arbitrary = do
    (i, s) <- genSpecs
    return $ SpecPair i s
