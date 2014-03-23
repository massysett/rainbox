{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Visual where

import Control.Monad
import Rainbox.Box
import System.Console.Rainbow
import System.Console.Rainbow.Colors
import Data.Monoid
import System.Random
import Test.QuickCheck.Gen
import Rainbox.Box.PrimitivesTests

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

sizeParam = 7

putBox b = do
  term <- termFromEnv
  putChunks term . render $ b

describe s b = do
  putStrLn (s ++ ":")
  putBox b
  putStrLn ""

testCompound :: String -> (Background -> [Box] -> Box) -> IO ()
testCompound d f = do
  g <- newStdGen
  let bxs = unGen (replicateM 5 genTextBox) g sizeParam 
      bk = unGen genBackground g sizeParam
  describe d $ f bk bxs

testVert
  :: String
  -> (Background -> Align Vert -> [Box] -> Box)
  -> IO ()
testVert d f = do
  testCompound (d ++ ", top align") (\bk bxs -> f bk top bxs)
  testCompound (d ++ ", center align") (\bk bxs -> f bk center bxs)
  testCompound (d ++ ", bottom align") (\bk bxs -> f bk bottom bxs)

testHoriz
  :: String
  -> (Background -> Align Horiz -> [Box] -> Box)
  -> IO ()
testHoriz d f = do
  testCompound (d ++ ", left align") (\bk bxs -> f bk left bxs)
  testCompound (d ++ ", center align") (\bk bxs -> f bk center bxs)
  testCompound (d ++ ", right align") (\bk bxs -> f bk right bxs)



tests :: IO ()
tests = do
  describe "narrow box" . chunk $ narrow
  describe "medium box" . chunk $ midwidth
  describe "wide box" . chunk $ wide

  testHoriz "catV" catV
  testVert "catH" catH

  testVert "sepH" (\bk av bxs -> sepH bk 1 av bxs)
  testHoriz "sepV" (\bk ah bxs -> sepV bk 1 ah bxs)

  testVert "punctuateH" (\bk av bxs -> punctuateH bk av " " bxs)
  testHoriz "punctuateV" (\bk ah bxs -> punctuateV bk ah " " bxs)

