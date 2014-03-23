-- | Usage:
--
-- Input the size parameter as $1.  Will generate a random box and print
-- it out. Always uses colors.
module Main where

import Test.QuickCheck.Gen
import Rainbox.Box.PrimitivesTests
import System.Environment
import System.Random
import Rainbox.Box
import System.Console.Rainbow

main :: IO ()
main = do
  g <- newStdGen
  s:[] <- getArgs
  let bx = unGen genBox g (read s)
  e <- termFromEnv
  putChunks e . render $ bx
