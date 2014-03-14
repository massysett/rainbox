{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Rainbox.Table where

import Rainbox hiding (Row)
import System.Console.Rainbow

data Crate = Crate
  { protect :: Bool
  , crateChunk :: Chunk
  } deriving Show

data HardLine = HardLine { unHardLine :: [Crate] }
  deriving Show

data Word = Word { unWord :: [Crate] }
  deriving Show

data Tagged p t = Tagged
  { payload :: p
  , tag :: t
  } deriving Show

data CellPayload
  = Hard [HardLine]
  | Soft [Word]
  deriving Show

type Cell = Tagged CellPayload

data AllocSpec = AllocSpec
  { asMinimum :: Int
  -- ^ Column will always be at least this wide
  , asRatio :: Int
  -- ^ Column consumes this much of the table's leftover space
  } deriving Show

data WidthSpec
  = Expand
  | Fixed Int
  | Alloc AllocSpec
  deriving Show

data Column cm = Column
  { colSpec :: WidthSpec
  , colCells :: [Cell cm]
  } deriving Show

widths
  :: Int
  -- ^ Total desired width of table
  -> [Column cm]
  -> [Int]
  -- ^ Width of each column
widths = undefined

data Row cm = Row
  { rowCells :: [Cell cm]
  } deriving Show

table
  :: Int
  -- ^ Total desired width of table
  -> [WidthSpec]
  -> [Row cm]
  -> [Int]
  -- ^ Width of each column
table = undefined

formatTable
  :: [Tagged (Row cm) rm]
  -> [Tagged Int clm]
  -- ^ One for each column in the table.  If there are any rows with
  -- extra columns, they will be defaulted to Expand, aligned left.
  -> (rm -> cm -> clm -> (Background, Align Horiz))
  -- ^ Applied to each cell in table
  -> [[Box]]
formatTable = undefined

