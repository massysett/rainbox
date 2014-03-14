module Rainbox.Table where

import Rainbox
import System.Console.Rainbow

data Cell a = Cell { unCell :: [a] }
  deriving (Eq, Show)

data Record col a = Record { unRecord :: [(col, Cell a)] }
  deriving (Eq, Show)

data Table row col a = Table { unTable :: [(row, Record col a)] }
  deriving (Eq, Show)

formatTable
  :: (row -> col -> (Background, Align Vert, Align Horiz))
  -> Table row col Chunk
  -> Box
formatTable = undefined
