module Main where

import qualified RainboxDir
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "rainbox"
  [ RainboxDir.tests
  ]
