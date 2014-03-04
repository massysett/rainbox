module Text.PrettyPrint.Rainbox where

import Control.Arrow ((***), first)
import Data.Monoid
import Data.List (intersperse)
import System.Console.Rainbow
import qualified Data.Text as X
import Text.PrettyPrint.Rainbox.Box



--
-- # Alignment
--

-- | Move a box \"up\" by putting it in a larger box with extra rows,
--   aligned to the top.  See the disclaimer for 'moveLeft'.
moveUp :: Background -> Int -> Box -> Box
moveUp bk n b = alignVert bk top (rows b + n) b

-- | Move a box down by putting it in a larger box with extra rows,
--   aligned to the bottom.  See the disclaimer for 'moveLeft'.
moveDown :: Background -> Int -> Box -> Box
moveDown bk n b = alignVert bk bottom (rows b + n) b

-- | Move a box left by putting it in a larger box with extra columns,
--   aligned left.  Note that the name of this function is
--   something of a white lie, as this will only result in the box
--   being moved left by the specified amount if it is already in a
--   larger right-aligned context.
moveLeft :: Background -> Int -> Box -> Box
moveLeft bk n b = alignHoriz bk left (cols b + n) b

-- | Move a box right by putting it in a larger box with extra
--   columns, aligned right.  See the disclaimer for 'moveLeft'.
moveRight :: Background -> Int -> Box -> Box
moveRight bk n b = alignHoriz bk right (cols b + n) b

--
-- # Glueing
--

-- | @hsep sep a bs@ lays out @bs@ horizontally with alignment @a@,
--   with @sep@ amount of space in between each.
hsep :: Background -> Int -> Alignment -> [Box] -> Box
hsep bk sep a bs = punctuateH bk a (emptyBox bk 0 sep) bs

-- | Glue a list of boxes together vertically, with the given alignment.
vcat :: Background -> Alignment -> [Box] -> Box
vcat b a bs = Box h w b (Col $ map (alignHoriz b a w) bs)
  where h = sum . map rows $ bs
        w = maximum . (0:) . map cols $ bs

-- | @vsep sep a bs@ lays out @bs@ vertically with alignment @a@,
--   with @sep@ amount of space in between each.
vsep :: Int -> Background -> Alignment -> [Box] -> Box
vsep sep b a bs = punctuateV b a (emptyBox b sep 0) bs

-- | @punctuateH a p bs@ horizontally lays out the boxes @bs@ with a
--   copy of @p@ interspersed between each.
punctuateH :: Background -> Alignment -> Box -> [Box] -> Box
punctuateH b a p bs = hcat b a (intersperse p bs)

-- | A vertical version of 'punctuateH'.
punctuateV :: Background -> Alignment -> Box -> [Box] -> Box
punctuateV b a p bs = vcat b a (intersperse p bs)

-- | Paste two boxes together horizontally with a single intervening
--   column of space, using a default (top) alignment.
(<+>) :: Box -> Box -> Box
l <+> r = hcat d top [l, emptyBox d 0 1, r]
  where
    d = defaultBackground

-- | Paste two boxes together vertically, using a default (left)
--   alignment.
(//) :: Box -> Box -> Box
t // b = vcat defaultBackground left [t,b]

-- | Paste two boxes together vertically with a single intervening row
--   of space, using a default (left) alignment.
(/+/) :: Box -> Box -> Box
t /+/ b = vcat d left [t, emptyBox d 1 0, b]
  where
    d = defaultBackground

--
-- # Implementation
--

-- | \"Padded take\": @takeP a n xs@ is the same as @take n xs@, if @n
--   <= length xs@; otherwise it is @xs@ followed by enough copies of
--   @a@ to make the length equal to @n@.
takeP :: a -> Int -> [a] -> [a]
takeP _ n _      | n <= 0 = []
takeP b n []              = replicate n b
takeP b n (x:xs)          = x : takeP b (n-1) xs

takeChunksPad
  :: Background
  -> Int
  -- ^ Length
  -> [Chunk]
  -> [Chunk]
takeChunksPad b len = go len
  where
    go l cs
      | l <= 0 = []
      | otherwise = case cs of
          [] -> [blanks b l]
          (x:xs)
            | lenX >= l -> [x { text = X.take l  . text $ x }]
            | otherwise -> x : go (l - lenX) xs
            where
              lenX = X.length . text $ x

-- | @takePA @ is like 'takeP', but with alignment.  That is, we
--   imagine a copy of @xs@ extended infinitely on both sides with
--   copies of @a@, and a window of size @n@ placed so that @xs@ has
--   the specified alignment within the window; @takePA algn a n xs@
--   returns the contents of this window.
--
-- Used for vertical padding only.  For horizontal padding see
-- 'takePAChunks'.
takePA :: Alignment -> [Chunk] -> Int -> [[Chunk]] -> [[Chunk]]
takePA c b n = glue . (takeP b (numRev c n) *** takeP b (numFwd c n)) . split
  where split t = first reverse . splitAt (numRev c (length t)) $ t
        glue    = uncurry (++) . first reverse
        numFwd AlignFirst    x = x
        numFwd AlignLast     _ = 0
        numFwd AlignCenter1  x = x `div` 2
        numFwd AlignCenter2  x = (x+1) `div` 2
        numRev AlignFirst    _ = 0
        numRev AlignLast     x = x
        numRev AlignCenter1  x = (x+1) `div` 2
        numRev AlignCenter2  x = x `div` 2

takePAChunks
  :: Background
  -> Alignment
  -> Int
  -- ^ Number of columns
  -> [Chunk]
  -> [Chunk]
takePAChunks bkgn algn ncols cs = concat [beg, cs', end]
  where
    (beg, cs', end) = case algn of

      AlignFirst
        | cksShort -> ([], cs, pad)
        | otherwise -> ([], takeChunkLen ncols cs, [])

    lenCks = sum . map X.length . map text $ cs
    cksShort = lenCks < ncols
    pad = [blanks bkgn (ncols - lenCks)]

takeChunkLen :: Int -> [Chunk] -> [Chunk]
takeChunkLen l cs
  | l <= 0 = []
  | otherwise = case cs of
      [] -> []
      x:xs
        | len >= l -> [x { text = X.take (len - l) . text $ x } ]
        | otherwise -> x : takeChunkLen (l - len) xs
        where
          len = X.length . text $ x

-- | Resize a rendered list of lines.
resizeBox
  :: Background
  -- ^ Background
  -> Int
  -- ^ Rows
  -> Int
  -- ^ Columns
  -> [[Chunk]]
  -- ^ Text of each row
  -> [[Chunk]]
resizeBox b r c
  = takeP [(blanks b c)] r
  . map (takeChunksPad b c)


-- | Generate spaces.
blanks
  :: Background
  -- ^ Background colors
  -> Int
  -- ^ Number of blanks
  -> Chunk
blanks (Background b8 b256) c = Chunk ts t
  where
    t = X.replicate c (X.singleton ' ')
    ts = mempty { style8 = mempty { background8 = b8 }
                , style256 = mempty { background256 = b256 }
                }

render :: Box -> [Chunk]
render = concat . unl . renderBox
  where
    unl ls = case ls of
      [] -> []
      x:xs -> x : nl : unl xs
    nl = [fromString "\n"]

renderBox :: Box -> [[Chunk]]
renderBox (Box r c b o) = case o of
  Blank -> resizeBox b r c [[]]
  Chunks ts -> resizeBox b r c [ts]

  Row bs -> resizeBox b r c
    . merge
    . map (renderBoxWithRows r)
    $ bs
    where
      merge = foldr (zipWith (++)) (repeat [])

  Col bs -> resizeBox b r c
    . concatMap (renderBoxWithCols c)
    $ bs

  SubBox ha va sb -> resizeBoxAligned b r c ha va
    . renderBox $ sb

renderBoxWithRows :: Int -> Box -> [[Chunk]]
renderBoxWithRows r b = renderBox b { rows = r }

renderBoxWithCols :: Int -> Box -> [[Chunk]]
renderBoxWithCols c b = renderBox b { cols = c }

resizeBoxAligned
  :: Background
  -> Int
  -> Int
  -> Alignment
  -> Alignment
  -> [[Chunk]]
  -> [[Chunk]]

resizeBoxAligned bk r c ha va =
  takePA va [(blanks bk c)] r
  . map (takePAChunks bk ha c)
