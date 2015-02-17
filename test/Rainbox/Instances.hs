{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | QuickCheck instances for all Rainbox modules.
module Rainbox.Instances where

import Control.Monad
import Test.QuickCheck
import Rainbow.QuickCheck ()
import Rainbox
import Rainbox.Box

instance Arbitrary Height where
  arbitrary = fmap Height arbitrary

instance Arbitrary Width where
  arbitrary = fmap Width arbitrary

instance Arbitrary (Align Vert) where
  arbitrary = elements [center, top, bottom]

instance Arbitrary (Align Horiz) where
  arbitrary = elements [center, left, right]

instance Arbitrary Bar where
  arbitrary = fmap Bar arbitrary

-- | Creates a non-nested Box.
instance Arbitrary Box where
  arbitrary = liftM3 barsToBox arbitrary arbitrary arbitrary

instance Arbitrary Cell where
  arbitrary = liftM4 Cell arbitrary arbitrary arbitrary arbitrary
