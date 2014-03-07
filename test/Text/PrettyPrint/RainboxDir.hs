module Text.PrettyPrint.RainboxDir where

import Test.Tasty
import qualified Text.PrettyPrint.Rainbox.BoxTests

tests :: TestTree
tests = testGroup "RainboxDir" [Text.PrettyPrint.Rainbox.BoxTests.tests]
