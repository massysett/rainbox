Rainbox tutorial - introduction
===============================

Rainbox (that is, *Rain*bow and *box*) helps you create colorful,
nicely formatted text boxes.  It is based on the `rainbow` package,
which provides all the color support, so read the [documentation for
that](http://hackage.haskell.org/package/rainbow) before continuing.

[boxes](http://hackage.haskell.org/package/boxes) is a similar
package but without color support.

This file is written in literate Haskell, so it compile and run for
you.  It also means that the compiler checks the examples, which
keeps them accurate.  However, HsColour does not fare so well with
literate Haskell, so this file will not look good from the
hyperlinked source in Haddock.  You're better off viewing it from a
text editor or through [the Gihub
website](http://www.github.com/massysett/rainbox).

A grid of boxes
===============

`Rainbox` is built on `Rainbox.Box`, which contains the building
blocks to create rectangular boxes of text.  Each box is justified
as appropriate and is filled in with a background color to make it
rectangular.

If your needs are complex, use `Rainbox.Box`.  Using it you can
patch boxes together into sort of a crazy quilt.  For simpler needs
you can use `Rainbox`, which only allows you to create grids of
boxes, like a spreadsheet.  This tutorial will show you how to use
the `Rainbox` module.  Everything you need from this package will be
available from the `Rainbox` module.  You will also need to import
packages from `rainbow`:

> {-# LANGUAGE OverloadedStrings #-}
> -- | If you are viewing this module in Haddock, note
> -- that the tutorial is contained in the source code of the
> -- module, which is written in literate Haskell.
> -- It is best viewed in your text editor or through
> -- Github at
> --
> -- <https://github.com/massysett/rainbox/blob/master/lib/Rainbox/Tutorial.lhs>

> module Rainbox.Tutorial where
>
> import System.Console.Rainbow
> import Data.Monoid
> import Data.String
> import Rainbox


Making a table of name data
===========================

For this example, we'll print a table of names, addresses, and
account balances.  This type holds the data we're interested in:

> data Record = Record
>   { firstName :: String
>   , lastName :: String
>   , address :: [String]
>   , phone :: String
>   , email :: String
>   , balance :: String
>   } deriving Show

And let's make a list of some sample data:

> records :: [Record]
> records =
>   [ Record
>       { firstName = "Nell"
>       , lastName = "Langston"
>       , address = [ "Owings Mills, MD 21117" ]
>       , phone = "800-588-2300"
>       , email = "NellJLangston@dayrep.com"
>       , balance = "0"
>       }
> 
>   , Record
>       { firstName = "Sharon"
>       , lastName = "Sutton"
>       , address = [ "37 Church Street", "Flushing, NY 11354" ]
>       , phone = "312-555-8100"
>       , email = "SharonJSutton@teleworm.us"
>       , balance = "1033.54"
>       }
> 
>   , Record
>       { firstName = "Barack"
>       , lastName = "Obama"
>       , address = [ "1600 Pennsylvania Ave NW", "Washington, DC" ]
>       , phone = "877-CASH-NOW"
>       , email = "president@whitehouse.gov"
>       , balance = "23562.00"
>       }
> 
>   , Record
>       { firstName = "Bert and Ernie"
>       , lastName = "Sesame"
>       , address = [ "123 Sesame Street", "Lower Level",
>                     "Sesame, WN V6B432" ]
>       , phone = "+45-123-4567"
>       , email = "lower@rhyta.com"
>       , balance = "100,451.05"
>       }
>
>   , Record
>       { firstName = "Vip"
>       , lastName = "Vipperman"
>       , address = [ "10000 Smiley Lane", "Denver, CO 80266" ]
>       , phone = "303-555-1212"
>       , email = "vipperman@rhyta.com"
>       , balance = "301.00"
>       }
>   ]

Building each row of cells
==========================

`Rainbox` works with rows of `Cell`s.  You build one list of `Cell`
for each row in your grid.  Here we will make the last name bold,
and the rest of the text will be plain.  We will also alternate each
row--every even row (starting with the first row) will be the
default color, and the odd rows will be yellow.  We'll make a
function that takes a `Record` and returns another function that,
when applied to a `Chunk` that contains the color for the row,
returns a list of `Cell`.  First let's make a small function that
will make a function that returns a `Cell` with our desired
defaults:

> cell :: [Chunk] -> Chunk -> Cell
> cell cks back = Cell brs left top (backgroundFromChunk back)
>   where
>     brs = map Bar . map ((:[]) . (<> back)) $ cks


> recordToCells :: Record -> Chunk -> [Cell]
> recordToCells r ck = map ($ ck) $
>   [ cell . (:[]) . fromString . firstName $ r
>   , cell . (:[]) $ (fromString (lastName r) <> bold)
>   , cell . map fromString . address $ r
>   , cell . (:[]) . fromString . phone $ r
>   , cell . (:[]) . fromString . email $ r
>   , cell . (:[]) . fromString . balance $ r
>   ]

Zipping to get rows of cells
============================

> cellRows :: [[Cell]]
> cellRows = zipWith recordToCells records (cycle [mempty, f_yellow])

Printing the cells
==================

To see the result, run this function in ghci:

> printSampleBox :: IO ()
> printSampleBox = printBox . gridByRows $ cellRows
