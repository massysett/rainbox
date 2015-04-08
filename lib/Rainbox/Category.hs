{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Rainbox.Category where

import Rainbow
import Rainbow.Types (Chunk(..))
import Data.Sequence (Seq) -- , ViewL(..)), viewl, (<|), (|>))
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Text as X

-- | Alignment.
data Align a = Center | NonCenter a
  deriving (Eq, Show)

-- | Vertical alignment.
data Vert = ATop | ABottom
  deriving (Eq, Show)

-- | Horizontal alignment.
data Horiz = ALeft | ARight
  deriving (Eq, Show)

-- # Height and Width

-- | A count of rows
newtype Height = Height Int
  deriving (Eq, Ord, Show)

class HasHeight a where
  height :: a -> Int

-- | A count of columns
newtype Width = Width Int
  deriving (Eq, Ord, Show)

class HasWidth a where
  width :: a -> Int

newtype Core = Core (Either Chunk (Height, Width))

instance HasWidth Core where
  width (Core ei) = case ei of
    Left (Chunk _ t) -> F.sum . fmap X.length $ t
    Right (_, Width w) -> w

instance HasHeight Core where
  height (Core ei) = case ei of
    Left _ -> 1
    Right (Height h, _) -> h

{-

Need one class for Port and Starboard, one for Above and Below

class Port a where
  port :: a -> Int

class Starboard a where
  starboard :: a -> Int

class Above a where
  above :: a -> Int

class Below a where
  below :: a -> Int
-}

data BoxVP = BoxVP (Align Horiz) Radiant (Either BoxH Core)

instance HasWidth BoxVP where
  width (BoxVP _ _ ei) = case ei of
    Left bh -> width bh
    Right c -> width c

instance HasHeight BoxVP where
  height (BoxVP _ _ ei) = case ei of
    Left bh -> height bh
    Right c -> height c

data BoxHP = BoxHP (Align Vert) Radiant (Either BoxV Core)

instance HasWidth BoxHP where
  width (BoxHP _ _ ei) = case ei of
    Left bv -> width bv
    Right c -> height c

instance HasHeight BoxHP where
  height (BoxHP _ _ ei) = case ei of
    Left bv -> height bv
    Right c -> height c

newtype BoxV = BoxV (Seq BoxVP)
  deriving Monoid

instance HasWidth BoxV where
  width = undefined

instance HasHeight BoxV where
  height = undefined

newtype BoxH = BoxH (Seq BoxHP)
  deriving Monoid

instance HasHeight BoxH where
  height = undefined

instance HasWidth BoxH where
  width = undefined

class Box a where
  printBox :: a -> Seq (Seq Chunk)

instance Box BoxV where
  printBox = undefined

instance Box BoxH where
  printBox = undefined

class Alignment a where
  type BuiltBox a
  type Opposite a
  buildBox :: a -> Radiant -> Either (Opposite a) Core -> BuiltBox a

instance Alignment (Align Vert) where
  type BuiltBox (Align Vert) = BoxH
  type Opposite (Align Vert) = BoxV
  buildBox a r ei = BoxH . Seq.singleton $
    BoxHP a r ei

instance Alignment (Align Horiz) where
  type BuiltBox (Align Horiz) = BoxV
  type Opposite (Align Horiz) = BoxH
  buildBox a r ei = BoxV . Seq.singleton $
    BoxVP a r ei

fromChunk
  :: Alignment a
  => a
  -> Radiant
  -> Chunk
  -> BuiltBox a
fromChunk a r c = buildBox a r (Right (Core (Left c)))

blank
  :: Alignment a
  => a
  -> Radiant
  -> Height
  -> Width
  -> BuiltBox a
blank a r h w = buildBox a r (Right (Core (Right (h, w))))

convert
  :: Alignment a
  => a
  -> Radiant
  -> Opposite a
  -> BuiltBox a
convert a r o = buildBox a r (Left o)
