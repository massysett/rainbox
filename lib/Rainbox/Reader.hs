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
  , R.defaultBackground
  , R.same

  -- * Box properties
  , B.Row(..)
  , B.Box
  , B.unBox

  -- * Height and columns
  , B.Height(..)
  , B.height
  , B.Width(..)
  , B.HasWidth(..)

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
  , runEnv

  -- * Making Boxes
  , B.blank
  , blankH
  , blankV
  , B.chunks
  , R.chunk

  -- * Pasting Boxes together
  , catH
  , catV
  , sepH
  , sepV
  , punctuateH
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
  , column

  -- * Resizing
  , resize
  , resizeH
  , resizeV

  -- * Printing Boxes
  , R.render
  , R.printBox
  ) where


import Control.Monad.Trans.Reader
import Data.Functor.Identity
import qualified Rainbox.Box.Primitives as B
import qualified Rainbox.Box as R
import Rainbox.Box.Primitives
  ( Box
  , Height
  , Width
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

type Env = ReaderT Specs

runEnv :: Specs -> Env Identity a -> a
runEnv s = runIdentity . ($ s) . runReaderT

blankH :: Monad m => Int -> Env m Box
blankH i = do
  b <- asks background
  return $ R.blankH b i

blankV :: Monad m => Int -> Env m Box
blankV i = do
  b <- asks background
  return $ R.blankV b i

catH :: Monad m => [Box] -> Env m Box
catH bxs = do
  bk <- asks background
  al <- asks alignV
  return $ B.catH bk al bxs

catV :: Monad m => [Box] -> Env m Box
catV bxs = do
  bk <- asks background
  al <- asks alignH
  return $ B.catV bk al bxs

grow :: Monad m => Height -> Width -> Box -> Env m Box
grow r c bx = do
  b <- asks background
  h <- asks alignH
  v <- asks alignV
  return $ R.grow b r c v h bx

growH :: Monad m => Int -> Box -> Env m Box
growH i bx = do
  b <- asks background
  h <- asks alignH
  return $ R.growH b i h bx

growV :: Monad m => Int -> Box -> Env m Box
growV i bx = do
  b <- asks background
  v <- asks alignV
  return $ R.growV b i v bx

column :: Monad m => [Box] -> Env m [Box]
column bs = do
  bk <- asks background
  ah <- asks alignH
  return $ R.column bk ah bs

resize
  :: Monad m => Height
  -> Width
  -> Box
  -> Env m Box
resize r c bx = do
  b <- asks background
  h <- asks alignH
  v <- asks alignV
  return $ R.resize b r c h v bx

resizeH
  :: Monad m => Int
  -> Box
  -> Env m Box
resizeH i bx = do
  b <- asks background
  h <- asks alignH
  return $ R.resizeH b i h bx

resizeV
  :: Monad m => Int
  -> Box
  -> Env m Box
resizeV i bx = do
  b <- asks background
  v <- asks alignV
  return $ R.resizeV b i v bx

sepH
  :: Monad m => Int
  -> [Box]
  -> Env m Box
sepH i bx = do
  b <- asks background
  v <- asks alignV
  return $ R.sepH b i v bx

sepV
  :: Monad m => Int
  -> [Box]
  -> Env m Box
sepV i bx = do
  b <- asks background
  h <- asks alignH
  return $ R.sepV b i h bx

punctuateH
  :: Monad m => Box
  -> [Box]
  -> Env m Box
punctuateH bx bxs = do
  b <- asks background
  v <- asks alignV
  return $ R.punctuateH b v bx bxs

punctuateV
  :: Monad m => Box
  -> [Box]
  -> Env m Box
punctuateV bx bxs = do
  b <- asks background
  h <- asks alignH
  return $ R.punctuateV b h bx bxs

view
  :: Monad m
  => Height
  -> Width
  -> Box
  -> Env m Box
view h w b = do
  av <- asks alignV
  ah <- asks alignH
  return $ R.view h w av ah b

viewH
  :: Monad m
  => Int
  -> Box
  -> Env m Box
viewH h b = do
  ah <- asks alignH
  return $ B.viewH h ah b

viewV
  :: Monad m
  => Int
  -> Box
  -> Env m Box
viewV h b = do
  av <- asks alignV
  return $ B.viewV h av b

-- | Paste two 'Box' together horizontally with no intervening
-- space.  Left fixity, precedence 5.
(<->) :: Monad m => Box -> Box -> Env m Box
(<->) l r = do
  b <- asks background
  a <- asks alignV
  return $ B.catH b a [l, r]

infixl 5 <->

-- | Paste two 'Box' together horizontally.  Intervening space is
-- determined by 'spaceH'.
-- Left fixity, precedence 5.
(<+>) :: Monad m => Box -> Box -> Env m Box
(<+>) l r = do
  bk <- asks background
  a <- asks alignV
  sp <- asks spaceH
  bx <- blankH sp
  return $ B.catH bk a [ l, bx, r ]

infixl 5 <+>

-- | Paste two 'Box' together vertically with no intervening space.
-- Left fixity, precedence 6.
(/-/) :: Monad m => Box -> Box -> Env m Box
(/-/) h l = do
  b <- asks background
  a <- asks alignH
  return $ B.catV b a [ h, l ]

infixl 6 /-/

-- | Paste two 'Box' together vertically.  Intervening space is
-- determined by 'spaceV'.  Left fixity, precedence 6.
(/+/) :: Monad m => Box -> Box -> Env m Box
(/+/) h l = do
  bk <- asks background
  a <- asks alignH
  sp <- asks spaceV
  bx <- blankV sp
  return $ B.catV bk a [ h, bx, l ]

infixl 6 /+/
