Rainbox tutorial - introduction
===============================

Rainbox (that is, *Rain*bow and *box*) helps you create colorful,
nicely formatted text boxes.  It is based on the `rainbow` package,
which provides all the color support, so read the [documentation for
that](http://hackage.haskell.org/package/rainbow) before continuing.

[boxes](http://hackage.haskell.org/package/boxes) is a similar
package but without color support.

This file is written in literate Haskell, so you can compile and run
it.  It also means that the compiler checks the examples, which
keeps them accurate.  However, HsColour does not fare so well with
literate Haskell, so this file will not look good from the hyperlinked
source in Haddock.  You're better off viewing it from a text editor or
through [the Gihub website](http://www.github.com/massysett/rainbox).

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
modules from `rainbow`:

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
> import Data.Monoid
> import Data.Sequence (Seq)
> import qualified Data.Sequence as Seq
> import Data.String
> import Rainbow
> import Rainbox


Making a table of name data
===========================

For this example, we'll print a table of names, addresses, and
account balances.  This type holds the data we're interested in:

> data Record = Record
>   { firstName :: String
>   , lastName :: String
>   , address :: Seq String
>   , phone :: String
>   , email :: String
>   , balance :: String
>   } deriving Show

And let's make a list of some sample data:

> records :: Seq Record
> records = Seq.fromList
>   [ Record
>       { firstName = "Nell"
>       , lastName = "Langston"
>       , address = Seq.singleton "Owings Mills, MD 21117"
>       , phone = "800-588-2300"
>       , email = "NellJLangston@dayrep.com"
>       , balance = "0"
>       }
> 
>   , Record
>       { firstName = "Sharon"
>       , lastName = "Sutton"
>       , address = Seq.fromList [ "37 Church Street", "Flushing, NY 11354" ]
>       , phone = "312-555-8100"
>       , email = "SharonJSutton@teleworm.us"
>       , balance = "1033.54"
>       }
> 
>   , Record
>       { firstName = "Barack"
>       , lastName = "Obama"
>       , address = Seq.fromList
>           [ "1600 Pennsylvania Ave NW", "Washington, DC" ]
>       , phone = "877-CASH-NOW"
>       , email = "president@whitehouse.gov"
>       , balance = "23562.00"
>       }
> 
>   , Record
>       { firstName = "Bert and Ernie"
>       , lastName = "Sesame"
>       , address = Seq.fromList [ "123 Sesame Street", "Lower Level",
>                     "Sesame, WN V6B432" ]
>       , phone = "+45-123-4567"
>       , email = "lower@rhyta.com"
>       , balance = "100,451.05"
>       }
>
>   , Record
>       { firstName = "Vip"
>       , lastName = "Vipperman"
>       , address = Seq.fromList
>            [ "10000 Smiley Lane", "Denver, CO 80266" ]
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

> cell :: Seq Chunk -> Radiant -> Cell
> cell cks bck = Cell brs top left bck
>   where
>     brs = fmap Bar . fmap (Seq.singleton . (<> back bck)) $ cks


> recordToCells :: Record -> Radiant -> Seq Cell
> recordToCells r rad = fmap ($ rad) . Seq.fromList $
>   [ cell . Seq.singleton . fromString . firstName $ r
>   , cell . Seq.singleton $ (fromString (lastName r) <> bold)
>   , cell . fmap fromString . address $ r
>   , cell . Seq.singleton . fromString . phone $ r
>   , cell . Seq.singleton . fromString . email $ r
>   , cell . Seq.singleton . fromString . balance $ r
>   ]

Zipping to get rows of cells
============================

> cellRows :: Seq (Seq Cell)
> cellRows = Seq.mapWithIndex makeColumn records
>   where
>     makeColumn index record = recordToCells record color
>       where
>         color = if even index then noColorRadiant else yellow

Adding white space between columns
==================================

If we print the table like it is now, there will be no whitespace,
as `Rainbox` does not add whitespace for you.  Fortunately this is
easy to add.  The string literal, " ", becomes a Cell due to the use
of the Overloaded Strings extension; the cell will have the default
background color.

> spacedOutCells :: Seq (Seq Cell)
> spacedOutCells = fmap (intersperse " ") cellRows

Printing the cells
==================

To see the result, run this function in ghci:

> printSampleBox :: IO ()
> printSampleBox = printBox . gridByRows $ spacedOutCells
