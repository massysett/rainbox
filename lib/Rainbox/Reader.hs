-- | 'Box' with many functions in a 'Reader' monad.
--
-- The advantage of this module over "Rainbox" is that many of the
-- functions have fewer arguments because they are instead carried
-- in the 'Reader' monad.  This also allows you to use four infix
-- operators to easily join up 'Box'.  The disadvantage is that
-- using the 'Reader' monad adds a layer of indirection.
module Rainbox.Reader
  ( -- * Backgrounds
    B.Background(..)
  , B.defaultBackground

  -- * Box properties
  , B.Row(..)
  , B.Box
  , B.unBox

  -- * Rows and columns
  , B.Rows(..)
  , B.rows
  , B.Cols(..)
  , B.HasCols(..)

  -- * Alignment
  , B.Align
  , B.Vert
  , B.Horiz
  , B.center
  , B.top
  , B.bottom
  , B.left
  , B.right

  -- * Reader monad
  , Specs(..)
  , Env

  -- * Making Boxes
  , B.blank
  , blankH
  , blankV
  , B.chunks
  , R.chunk

  -- * Pasting Boxes together
  , hcat
  , vcat
  , hsep
  , vsep
  , punctuateH -- # FIXME inconsistent naming - sepH and sepV then.
  , punctuateV
  , (<->)
  , (<+>)
  , (/-/)
  , (/+/)

  -- * Viewing Boxes
  , view
  , viewH
  , viewV

  -- * Growing Boxes
  , grow
  , growH
  , growV

  -- * Resizing
  , resize
  , resizeH
  , resizeV

  -- * Printing Boxes
  , R.render
  , R.printBox
  ) where


import Control.Monad.Trans.Reader
import qualified Rainbox.Box as B
import qualified Rainbox as R
import Rainbox.Box
  ( Box
  , Rows
  , Cols
  , Align
  , Horiz
  , Vert
  )

data Specs = Specs
  { background :: B.Background
  , alignH :: Align Horiz
  , alignV :: Align Vert
  , spaceH :: Int
  -- ^ Amount of intervening space for horizontal joins
  , spaceV :: Int
  --  ^ Amount of intervening space for vertical joins
  } deriving (Eq, Show)

type Env = Reader Specs

blankH :: Int -> Env Box
blankH i = do
  b <- asks background
  return $ R.blankH b i

blankV :: Int -> Env Box
blankV i = do
  b <- asks background
  return $ R.blankV b i

hcat :: [Box] -> Env Box
hcat bxs = do
  bk <- asks background
  al <- asks alignV
  return $ B.hcat bk al bxs

vcat :: [Box] -> Env Box
vcat bxs = do
  bk <- asks background
  al <- asks alignH
  return $ B.vcat bk al bxs

grow :: Rows -> Cols -> Box -> Env Box
grow r c bx = do
  b <- asks background
  h <- asks alignH
  v <- asks alignV
  return $ R.grow b r c h v bx

growH :: Int -> Box -> Env Box
growH i bx = do
  b <- asks background
  h <- asks alignH
  return $ R.growH b i h bx

growV :: Int -> Box -> Env Box
growV i bx = do
  b <- asks background
  v <- asks alignV
  return $ R.growV b i v bx

resize
  :: Rows
  -> Cols
  -> Box
  -> Env Box
resize r c bx = do
  b <- asks background
  h <- asks alignH
  v <- asks alignV
  return $ R.resize b r c h v bx

resizeH
  :: Int
  -> Box
  -> Env Box
resizeH i bx = do
  b <- asks background
  h <- asks alignH
  return $ R.resizeH b i h bx

resizeV
  :: Int
  -> Box
  -> Env Box
resizeV i bx = do
  b <- asks background
  v <- asks alignV
  return $ R.resizeV b i v bx

hsep
  :: Int
  -> [Box]
  -> Env Box
hsep i bx = do
  b <- asks background
  v <- asks alignV
  return $ R.hsep b i v bx

vsep
  :: Int
  -> [Box]
  -> Env Box
vsep i bx = do
  b <- asks background
  h <- asks alignH
  return $ R.vsep b i h bx

punctuateH
  :: Box
  -> [Box]
  -> Env Box
punctuateH bx bxs = do
  b <- asks background
  v <- asks alignV
  return $ R.punctuateH b v bx bxs

punctuateV
  :: Box
  -> [Box]
  -> Env Box
punctuateV bx bxs = do
  b <- asks background
  h <- asks alignH
  return $ R.punctuateV b h bx bxs

view
  :: Rows
  -> Cols
  -> Box
  -> Env Box
view = undefined

viewH
  :: Int
  -> Box
  -> Env Box
viewH = undefined

viewV
  :: Int
  -> Box
  -> Env Box
viewV = undefined

-- | Paste two 'Box' together horizontally with no intervening
-- space.
(<->) :: Box -> Box -> Env Box
(<->) l r = do
  b <- asks background
  a <- asks alignV
  return $ B.hcat b a [l, r]

-- | Paste two 'Box' together.  Intervening space is determined by
-- 'spaceH'.
(<+>) :: Box -> Box -> Env Box
(<+>) l r = do
  bk <- asks background
  a <- asks alignV
  sp <- asks spaceH
  bx <- blankH sp
  return $ B.hcat bk a [ l, bx, r ]

-- | Paste two 'Box' together vertically with no intervening space.
(/-/) :: Box -> Box -> Env Box
(/-/) h l = do
  b <- asks background
  a <- asks alignH
  return $ B.vcat b a [ h, l ]

-- | Paste two 'Box' together vertically.  Intervening space is
-- determined by 'spaceV'.
(/+/) :: Box -> Box -> Env Box
(/+/) h l = do
  bk <- asks background
  a <- asks alignH
  sp <- asks spaceV
  bx <- blankV sp
  return $ B.vcat bk a [ h, bx, l ]
