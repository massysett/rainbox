module Text.PrettyPrint.Rainbox.Box.InternalTests where

import Control.Monad
import Control.Applicative
import Test.Tasty
import Test.QuickCheck
import Data.Monoid
import System.Console.Rainbow
import qualified Data.Text as X
import qualified Test.Rainbow.Generators as G
import Text.PrettyPrint.Rainbox.Box.Internal

tests :: TestTree
tests = testGroup "InternalTests" []

genText :: Gen X.Text
genText = fmap X.pack $ listOf c
  where
    c = elements ['0'..'Z']

genChunk :: Gen Chunk
genChunk = genText >>= G.chunk

genBackground :: Gen Background
genBackground = liftM2 Background G.colors8 G.colors256
