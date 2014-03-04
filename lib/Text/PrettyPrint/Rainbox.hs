module Text.PrettyPrint.Rainbox where

{-

import Control.Arrow ((***), first)
import Data.Monoid
import Data.List (intersperse)
import System.Console.Rainbow
import qualified Data.Text as X
import Text.PrettyPrint.Rainbox.Box



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
renderRows
  :: Background
  -- ^ Background
  -> Int
  -- ^ Rows
  -> Int
  -- ^ Columns
  -> [[Chunk]]
  -- ^ Text of each row
  -> [[Chunk]]
renderRows b r c
  = takeP [(blanks b c)] r
  . map (takeChunksPad b c)


render :: Box -> [Chunk]
render = concat . unl . renderBox
  where
    unl ls = case ls of
      [] -> []
      x:xs -> x : nl : unl xs
    nl = [fromString "\n"]

renderBox :: Box -> [[Chunk]]
renderBox (Box r c b o) = case o of
  Blank -> renderRows b r c [[]]
  Chunks ts -> renderRows b r c [ts]

  Row bs -> renderRows b r c
    . merge
    . map (renderBoxWithRows r)
    $ bs
    where
      merge = foldr (zipWith (++)) (repeat [])

  Col bs -> renderRows b r c
    . concatMap (renderBoxWithCols c)
    $ bs

  SubBox ha va sb -> renderRowsAligned b r c ha va
    . renderBox $ sb

renderBoxWithRows :: Int -> Box -> [[Chunk]]
renderBoxWithRows r b = renderBox b { rows = r }

renderBoxWithCols :: Int -> Box -> [[Chunk]]
renderBoxWithCols c b = renderBox b { cols = c }

renderRowsAligned
  :: Background
  -> Int
  -> Int
  -> Alignment
  -> Alignment
  -> [[Chunk]]
  -> [[Chunk]]

renderRowsAligned bk r c ha va =
  takePA va [(blanks bk c)] r
  . map (takePAChunks bk ha c)

-}
