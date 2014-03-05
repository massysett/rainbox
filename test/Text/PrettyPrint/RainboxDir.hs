module Text.PrettyPrint.RainboxDir where

import Test.Tasty
import qualified Text.PrettyPrint.Rainbox.BoxDir

tests :: TestTree
tests = testGroup "RainboxDir" [Text.PrettyPrint.Rainbox.BoxDir.tests]
