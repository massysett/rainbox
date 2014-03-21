module Rainbox.ReaderTests where

import qualified Rainbox.Box as R
import Rainbox.Reader
import Rainbox.Box.PrimitivesTests
import Test.QuickCheck hiding (resize)
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

  , testProperty "growH" $ \(SpecPair i s) ->
    let p = R.growH (iBackground i) (unWidth . iWidth $ i)
          (iHoriz i) (iBox i)
    in testEq s (growH (unWidth . iWidth $ i)
        (iBox i)) p

  , testProperty "growV" $ \(SpecPair i s) ->
    let p = R.growV (iBackground i) (unHeight . iHeight $ i)
          (iVert i) (iBox i)
    in testEq s (growV (unHeight . iHeight $ i)
        (iBox i)) p

  , testProperty "column" $ \(SpecPair i s) ->
    let p = R.column (iBackground i) (iHoriz i) (iBoxes i)
    in testEq s (column (iBoxes i)) p

  , testProperty "resize" $ \(SpecPair i s) ->
    let p = R.resize (iBackground i) (iHeight i) (iWidth i)
          (iHoriz i) (iVert i) (iBox i)
    in testEq s (resize (iHeight i) (iWidth i) (iBox i)) p

  , testProperty "resizeH" $ \(SpecPair i s) ->
    let p = R.resizeH (iBackground i) (unWidth . iWidth $ i)
          (iHoriz i) (iBox i)
    in testEq s (resizeH (unWidth . iWidth $ i) (iBox i)) p

  , testProperty "resizeV" $ \(SpecPair i s) ->
    let p = R.resizeV (iBackground i) (unHeight . iHeight $ i)
          (iVert i) (iBox i)
    in testEq s (resizeV (unHeight . iHeight $ i) (iBox i)) p

  , testProperty "sepH" $ \(SpecPair i s) ->
    let p = R.sepH (iBackground i) (spaceH s) (iVert i) (iBoxes i)
    in testEq s (sepH (spaceH s) (iBoxes i)) p

  , testProperty "sepV" $ \(SpecPair i s) ->
    let p = R.sepV (iBackground i) (spaceV s) (iHoriz i) (iBoxes i)
    in testEq s (sepV (spaceV s) (iBoxes i)) p

  , testProperty "punctuateH" $ \(SpecPair i s) ->
    let p = R.punctuateH (iBackground i) (iVert i) (iBox i) (iBoxes i)
    in testEq s (punctuateH (iBox i) (iBoxes i)) p

  , testProperty "punctuateV" $ \(SpecPair i s) ->
    let p = R.punctuateV (iBackground i) (iHoriz i) (iBox i) (iBoxes i)
    in testEq s (punctuateV (iBox i) (iBoxes i)) p

  , testProperty "viewH" $ \(SpecPair i s) ->
    let p = R.viewH (unWidth . iWidth $ i) (iHoriz i) (iBox i)
    in testEq s (viewH (unWidth . iWidth $ i) (iBox i)) p

  , testProperty "viewV" $ \(SpecPair i s) ->
    let p = R.viewV (unHeight . iHeight $ i) (iVert i) (iBox i)
    in testEq s (viewV (unHeight . iHeight $ i) (iBox i)) p

  , testProperty "view" $ \(SpecPair i s) ->
    let p = R.view (iHeight i) (iWidth i) (iVert i) (iHoriz i)
          (iBox i)
    in testEq s (view (iHeight i) (iWidth i) (iBox i)) p
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
