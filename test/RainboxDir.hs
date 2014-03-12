module RainboxDir where

import qualified Rainbox.BoxTests
import qualified Rainbox.ReaderTests
import Test.Tasty

tests :: TestTree
tests = testGroup "RainboxDir" [ Rainbox.BoxTests.tests
                               , Rainbox.ReaderTests.tests ]
