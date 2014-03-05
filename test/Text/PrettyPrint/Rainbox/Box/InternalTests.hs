module Text.PrettyPrint.Rainbox.Box.InternalTests where

import Control.Applicative
import Test.Tasty
import Test.QuickCheck
import Data.Monoid
import System.Console.Rainbow

tests :: TestTree
tests = testGroup "InternalTests" []

genLast :: Gen a -> Gen (Last a)
genLast g = Last
  <$> frequency [(3, Just <$> g), (1, return Nothing)]

genColor8 :: Gen Color
genColor8 = elements
  [ Black, Red, Green, Yellow, Blue, Magenta, Cyan, White ]

genBackground8 :: Gen Background8
genBackground8 = genLast genColor8
