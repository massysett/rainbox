{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rainbox.Instances where

import Control.Monad
import Test.QuickCheck
import Rainbox.Core
import Rainbow.Instances ()
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

instance Arbitrary a => Arbitrary (Alignment a) where
  arbitrary = oneof [ return Center, fmap NonCenter arbitrary ]

instance Arbitrary Horizontal where
  arbitrary = elements [ ATop, ABottom ]

instance Arbitrary Vertical where
  arbitrary = elements [ ALeft, ARight ]

instance Arbitrary Height where
  arbitrary = fmap Height $ frequency
    [ (3, fmap getNonNegative arbitrary)
    , (1, arbitrary)
    ]

instance Arbitrary Width where
  arbitrary = fmap Width $ frequency
    [ (3, fmap getNonNegative arbitrary)
    , (1, arbitrary)
    ]

instance Arbitrary Core where
  arbitrary = fmap Core arbitrary

instance Arbitrary Rod where
  arbitrary = fmap Rod arbitrary

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = fmap Seq.fromList arbitrary

instance Arbitrary RodRows where
  arbitrary = sized $ \s -> resize (s `div` 10) $ oneof
    [ fmap RodRowsWithHeight arbitrary
    , fmap RodRowsNoHeight arbitrary
    ]

instance Arbitrary a => Arbitrary (Payload a) where
  arbitrary = liftM3 Payload arbitrary arbitrary arbitrary

instance Arbitrary a => Arbitrary (Box a) where
  arbitrary = fmap Box arbitrary

instance Arbitrary Cell where
  arbitrary = liftM4 Cell arbitrary arbitrary arbitrary arbitrary
