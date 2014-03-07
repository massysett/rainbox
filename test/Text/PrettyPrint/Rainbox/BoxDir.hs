module Text.PrettyPrint.Rainbox.BoxDir where

import Test.Tasty
import qualified Text.PrettyPrint.Rainbox.BoxTests

tests :: TestTree
tests = testGroup "BoxDir"
  [Text.PrettyPrint.Rainbox.BoxTests.tests ]
