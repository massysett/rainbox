module Text.PrettyPrintDir where

import Test.Tasty
import qualified Text.PrettyPrint.RainboxDir

tests :: TestTree
tests = testGroup "PrettyPrintDir" [Text.PrettyPrint.RainboxDir.tests]
