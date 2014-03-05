module Main where

import qualified TextDir
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "rainbox" [TextDir.tests]
