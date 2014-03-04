module Text.PrettyPrint.Rainbox.Box where

import qualified Data.Foldable as F
import System.Console.Rainbow
import Data.Monoid
import qualified Data.Text as X

data Background = Background
  { boxBackground8 :: Background8
  , boxBackground256 :: Background256
  } deriving Show

defaultBackground :: Background
defaultBackground = Background (Last Nothing) (Last Nothing)

newtype Row = Row { unRow :: [Chunk] }
  deriving Show

newtype Box = Box { unBox :: [Row] }
  deriving Show

rows :: Box -> Rows
rows = Rows . length . unBox

newtype Rows = Rows { unRows :: Int }
  deriving (Eq, Ord, Show)

newtype Cols = Cols { unCols :: Int }
  deriving (Eq, Ord, Show)

class HasCols a where
  cols :: a -> Cols

instance HasCols Row where
  cols = Cols . sum . map (X.length . text) . unRow

instance HasCols Box where
  cols b = case unBox b of
    [] -> Cols 0
    x:_ -> cols x

blank
  :: Background
  -> Rows
  -> Cols
  -> Box
blank bk r c = Box $ replicate (unRows r) row
  where
    row = Row $ [ blanks bk c ]

chunkBox :: [Chunk] -> Box
chunkBox = Box . (:[]) . Row

data Alignment a = Center | NonCenter a
  deriving (Eq, Show)

data Vertical = ATop | ABottom
  deriving (Eq, Show)

data Horizontal = ALeft | ARight
  deriving (Eq, Show)

-- | Merge several Box horizontally into one Box.  That is, with
-- alignment set to ATop:
--
-- > --- ------- ----
-- > --- -------
-- > ---
--
-- becomes
--
-- > --------------
-- > ----------....
-- > ---...........

hcat :: Background -> Alignment Vertical -> [Box] -> Box
hcat bk al bs = Box . mergeHoriz . map (pad . unBox) $ bs
  where
    pad = padHoriz bk al height
    height = F.maximum . (Rows 0:) . map rows $ bs

-- | Merge several Box vertically into one Box.  That is, with
-- alignment set to ALeft:
--
-- > -------
-- > -------
-- >
-- > ---
-- > ---
--
-- > ----
-- > ----
--
-- becomes
--
-- > -------
-- > -------
-- > ---....
-- > ---....
-- > ---....
-- > ----...
-- > ----...

vcat :: Background -> Alignment Horizontal -> [Box] -> Box
vcat bk al bs = Box . map (padVert bk al w) . concat . map unBox $ bs
  where
    w = F.maximum . (Cols 0:) . map cols $ bs


-- | Given the resulting height, pad a list of Rows.  So, when given
-- a height of 3 and an alignment of ATop,
--
-- > --------
-- > --------
--
-- becomes
--
-- > --------
-- > --------
-- > ........
--
-- where dashes is a Row with data, and dots is a blank Row.

padHoriz :: Background -> Alignment Vertical -> Rows -> [Row] -> [Row]
padHoriz bk a (Rows tgt) rs = concat [top, rs, bot]
  where
    nPad = max 0 $ tgt - length rs
    (nATop, nBot) = case a of
      Center -> split nPad
      NonCenter ATop -> (0, nPad)
      NonCenter ABottom -> (nPad, 0)
    pad = Row [blanks bk len]
      where
        len = case rs of
          [] -> Cols 0
          x:_ -> cols x
    (top, bot) = (replicate nATop pad, replicate nBot pad)

-- | Given the resulting width, pad a Row.  So, when given
-- a width of 10 and an alignment of ARight,
--
-- > -------
--
-- becomes
--
-- > ...-------

padVert :: Background -> Alignment Horizontal -> Cols -> Row -> Row
padVert bk a (Cols tgt) (Row cs) = Row . concat $ [left, cs, right]
  where
    nPad = max 0 $ tgt - length cs
    (nLeft, nRight) = case a of
      Center -> split nPad
      NonCenter ALeft -> (0, nPad)
      NonCenter ARight -> (nPad, 0)
    (left, right) = (mkPad nLeft, mkPad nRight)
    mkPad n
      | n == 0 = []
      | otherwise = [blanks bk (Cols n)]
        

-- | Merge several horizontal Rows into one set of horizontal Row.
-- That is:
--
-- > ----- ----- -----
-- > ----- ----- -----
-- > ----- ----- -----
--
-- into
--
-- > ---------------
-- > ---------------
-- > ---------------
--
-- Strange behavior will result if each input list is not exactly
-- the same length.

mergeHoriz :: [[Row]] -> [Row]
mergeHoriz = foldr (zipWith merge) (repeat (Row []))
  where
    merge (Row r1) (Row r2) = Row $ r1 ++ r2

-- # Viewing

-- | View a Box, possibly shrinking it.  You set the size of your
-- viewport and how it is oriented relative to the Box as a whole.
-- The Box returned may be smaller than the argument Box, but it
-- will never be bigger.

view
  :: Rows
  -> Cols
  -> Alignment Horizontal
  -> Alignment Vertical
  -> Box
  -> Box
view r c ah av (Box inRows) = Box . map htrim . vtrim $ inRows
  where
    vtrim = case av of
      NonCenter ATop -> take . unRows $ r
      NonCenter ABottom -> drop extra
      Center -> drop nDrop . take nTake
        where
          (trimL, trimR) = split extra
          nTake = length inRows - trimR
          nDrop = trimL
      where
        extra = length inRows - unRows r
    htrim rw = undefined

dropChars :: Int -> Row -> Row
dropChars n (Row cs)
  | n <= 0 = Row cs
  | otherwise = case cs of
      [] -> Row []
      x:xs
        | lenX <= n -> dropChars (n - lenX) (Row xs)
        | otherwise -> Row $ x' : xs
        where
          lenX = X.length . text $ x
          x' = x { text = X.drop n . text $ x }

takeChars :: Int -> Row -> Row
takeChars n (Row cs)
  | n <= 0 = Row []
  | otherwise = case cs of
      [] -> Row []
      x:xs
        | lenX <= n -> Row $ x : unRow (takeChars (n - lenX) (Row xs))
        | otherwise -> Row [x']
        where
          lenX = X.length . text $ x
          x' = x { text = X.take n . text $ x }

--
-- # Helpers
--

-- | Generate spaces.
blanks
  :: Background
  -- ^ Background colors
  -> Cols
  -- ^ Number of blanks
  -> Chunk
blanks (Background b8 b256) c = Chunk ts t
  where
    t = X.replicate (unCols c) (X.singleton ' ')
    ts = mempty { style8 = mempty { background8 = b8 }
                , style256 = mempty { background256 = b256 }
                }

-- | Split a number into two parts, so that the sum of the two parts
-- is equal to the original number.
split :: Int -> (Int, Int)
split i = (r, r + rm)
  where
    (r, rm) = i `quotRem` 2

