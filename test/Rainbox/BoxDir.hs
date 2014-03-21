module Rainbox.BoxDir where

import qualified Rainbox.Box.PrimitivesTests
import Test.Tasty

tests :: TestTree
tests = testGroup "Box" [ Rainbox.Box.PrimitivesTests.tests ]
