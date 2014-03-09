module RainboxDir where

import qualified Rainbox.BoxTests
import Test.Tasty

tests :: TestTree
tests = testGroup "RainboxDir" [ Rainbox.BoxTests.tests ]
