{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Visual where

import Rainbox
import System.Console.Rainbow
import System.Console.Rainbow.Colors
import Data.Monoid

colors = f_yellow <> b_blue

narrow = "narrow box" <> colors

midwidth = "medium width box" <> colors

wide = "a wide box, see how wide I am?" <> colors

greenBack = same c8_green

yellowBack = same c8_yellow

all3 = [narrow, midwidth, wide]

short = chunk narrow

midheight = catV greenBack left . map chunk $ [narrow, midwidth]

tall = catV greenBack left . map chunk $ [narrow, midwidth, wide]

tests :: IO ()
tests = do
  term <- termFromEnv
  let putBox = putChunks term . render
      describe s b = putStrLn (s ++ ":") >> putBox b >> putStrLn ""
  describe "narrow box" . chunk $ narrow
  describe "medium box" . chunk $ midwidth
  describe "wide box" . chunk $ wide

  describe "catV, right justify"
    . catV greenBack right
    . map chunk
    $ all3

  describe "catV, left justify"
    . catV greenBack left
    . map chunk
    $ all3

  describe "catV, center justify"
    . catV greenBack center
    . map chunk
    $ all3

  describe "catH, top justify"
    . catH yellowBack top
    $ [short, midheight, tall]

  describe "catH, bottom justify"
    . catH yellowBack bottom
    $ [short, midheight, tall]

  describe "catH, center justify"
    . catH yellowBack center
    $ [short, midheight, tall]

  describe "sepH, top justify"
    . sepH yellowBack 1 top
    $ [short, midheight, tall]

