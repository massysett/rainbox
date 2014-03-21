module RainboxDir where

import qualified Rainbox.BoxTests
import qualified Rainbox.BoxDir
import qualified Rainbox.ReaderTests
import Test.Tasty

tests :: TestTree
tests = testGroup "RainboxDir" [ Rainbox.BoxTests.tests
                               , Rainbox.BoxDir.tests
                               , Rainbox.ReaderTests.tests ]
