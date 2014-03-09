module Main where

import qualified RainboxDir
import qualified RainboxTests
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "rainbox"
  [ RainboxDir.tests
  , RainboxTests.tests
  ]
