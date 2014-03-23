{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Visual where

import Control.Monad
import Rainbox.Box
import System.Console.Rainbow
import System.Console.Rainbow.Colors
import Data.Monoid
import System.Random
import Test.QuickCheck.Gen hiding (resize)
import Rainbox.Box.PrimitivesTests
import Data.Maybe (fromJust)
import Data.String

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

-- | Makes a 10x10 test box.
testBox :: Box
testBox = catV defaultBackground left . map mkLine $ clrs
  where
    mkLine clr = chunk $ txt <> clr
    txt = fromString ['0'..'9']
    lkp k = bc256 . fromJust . lookup k $ c256_all
    clrs = map lkp . take 10 . iterate (+6) $ 160

singleH
  :: String
  -> (Align Horiz -> Box)
  -> IO ()
singleH desc f = do
  describe (desc ++ ", left") (f left)
  describe (desc ++ ", center") (f center)
  describe (desc ++ ", right") (f right)

singleV
  :: String
  -> (Align Vert -> Box)
  -> IO ()
singleV desc f = do
  describe (desc ++ ", top") (f top)
  describe (desc ++ ", center") (f center)
  describe (desc ++ ", bottom") (f bottom)

single
  :: String
  -> (Align Vert -> Align Horiz -> Box)
  -> IO ()
single desc f = do
  singleV (desc ++ ", left") (\av -> f av left)
  singleV (desc ++ ", center") (\av -> f av center)
  singleV (desc ++ ", right") (\av -> f av right)

  singleH (desc ++ ", top") (f top)
  singleH (desc ++ ", center") (f center)
  singleH (desc ++ ", bottom") (f bottom)


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

  let green = backgroundFromChunk b_green

  testHoriz "column" (\bk ah bxs -> catV defaultBackground left
                        (column bk ah bxs))

  describe "original box for following tests, 10x10" testBox

  single "view, 3x3"
    (\av ah -> view (Height 3) (Width 3) av ah testBox)
  singleH "viewH, 3" (\ah -> viewH 3 ah testBox)
  singleV "viewV, 3" (\av -> viewV 3 av testBox)

  single "grow, 13x13"
    (\av ah -> grow green (Height 13) (Width 13) av ah testBox)
  singleH "growH, 13" (\ah -> growH green 13 ah testBox)
  singleV "growV, 13" (\av -> growV green 13 av testBox)

  single "resize, 13x13"
    (\av ah -> resize green (Height 13) (Width 13) av ah testBox)
  singleH "resizeH, 13" (\ah -> resizeH green 13 ah testBox)
  singleV "resizeV, 13" (\av -> resizeV green 13 av testBox)

  single "resize, 7x7"
    (\av ah -> resize green (Height 7) (Width 7) av ah testBox)
  singleH "resizeH, 7" (\ah -> resizeH green 7 ah testBox)
  singleV "resizeV, 7" (\av -> resizeV green 7 av testBox)

