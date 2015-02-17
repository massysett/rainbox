-- | Usage:
--
-- Input the size parameter as $1.  Will generate a random box and print
-- it out. Always uses colors.
module Main where

import Test.QuickCheck
import Rainbox.Box.PrimitivesTests
import System.Environment
import Rainbox.Box
import Rainbow

main :: IO ()
main = do
  s:[] <- getArgs
  bx <- generate (Test.QuickCheck.resize (read s) genBox)
  e <- termFromEnv
  putChunks e . render $ bx
