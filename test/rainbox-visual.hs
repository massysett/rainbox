-- | Prints all the boxes in the tutorial.  The output must be
-- visually inspected.

module Main where

import Rainbox.Tutorial

printBox :: String -> IO () -> IO ()
printBox lbl act = do
  putStrLn $ replicate 50 '='
  putStrLn ""
  putStrLn $ lbl ++ ":"
  putStrLn ""
  act
  putStrLn ""

main :: IO ()
main = do
  printBox "box1" renderBox1
  printBox "box2" renderBox2
  printBox "box3" renderBox3
  printBox "box4" renderBox4
  printBox "box5" renderBox5
  printBox "verticalStationTable" renderVerticalStationTable
  printBox "horizontalStationTable" renderHorizontalStationTable
