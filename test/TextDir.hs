module TextDir where

import Test.Tasty
import qualified Text.PrettyPrintDir

tests :: TestTree
tests = testGroup "TextDir" [Text.PrettyPrintDir.tests]
