-- | Prints a random grid using the main Rainbox module.  Ignores all
-- command line arguments.

module Main where

import Test.QuickCheck
import Rainbox.Instances ()
import Rainbox

main :: IO ()
main = do
  rows <- generate (resize 5 arbitrary)
  putStrLn "Grid by rows:"
  printBox $ gridByRows rows
  putStrLn
  putStrLn "Grid by columns:"
  cols <- generate (resize 5 arbitrary)
  printBox $ gridByCols cols
