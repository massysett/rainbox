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

The Rainbox model
=================

Rainbox gives you functions to create `Box`es and glue them
together.  All `Box`es are rectangular on your screen.  When you
create many `Box`es, you must specify a background color; this will
be used to insert necessary white space to make your text fill up a
`Box`.  When gluing `Box`es together you can specify how they are
justified.

For maximum flexibility, use the `Rainbox` module or the
`Rainbox.Reader` module, which is similar to `Rainbox` but wraps
many of its functions in a `Reader` monad.  

Everything you need from the `rainbox` package is in the `Rainbox`
module.  Alternatively, you can use the `Rainbox.Reader` module,
which has the same purpose as `Rainbox` but wraps many of its
functions so that they are used from a `Reader` monad.  In this file
we'll only discuss the `Rainbox` module.

For the examples, let's get the module and imports started:

> {-# LANGUAGE OverloadedStrings #-}
> -- | If you are viewing this module in Haddock, note
> -- that the tutorial is contained in the source code of the
> -- module, which is written in literate Haskell.
> -- It is best viewed in your text editor or through
> -- Github at
>
> -- <https://github.com/massysett/rainbox/blob/master/lib/Rainbox/Tutorial.lhs>

> module Rainbox.Tutorial where
>
> import System.Console.Rainbow
> import qualified Data.Text as X
> import Data.Monoid
> import Rainbox


Making a table of name data
===========================

For this example, we'll print a table of names, addresses, and
account balances.  This type holds the data we're interested in:

> data Record a = Record
>   { firstName :: a
>   , lastName :: a
>   , address :: [a]
>   , phone :: a
>   , email :: a
>   , balance :: a
>   } deriving Show

To make it easy to work with a `Record`, let's make it a `Functor`:

> instance Functor Record where
>   fmap f r = Record
>     { firstName = f (firstName r)
>     , lastName = f (lastName r)
>     , address = map f . address $ r
>     , phone = f (phone r)
>     , email = f (email r)
>     , balance = f (balance r)
>     }
> 

And let's make a list of some sample data:

> records :: [Record String]
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
>   ]

Apply foreground colors
=======================

First, we'll apply some colors to the text and change it to Rainbow
`Chunk`s.  For foreground colors, the last name will be green and
the account balance will be yellow; we'll leave everything else as
the default foreground color.

> coloredForeground :: [Record Chunk]
> coloredForeground
>   = map colorForeground
>   . map (fmap (fromText . X.pack))
>   $ records
>   where
>     colorForeground r = r { lastName = lastName r <> f_green
>                           , balance = balance r <> f_yellow
>                           }

Apply background colors
=======================

The `rainbow` library allows you to set the background color of each
`Chunk`.  `rainbox` does not give you any way to override what
`rainbow` does.  Any `Box` you create with `rainbox` envelops the
`Chunk` and whatever colors the `Chunk` came with, although the
`Box` must have its own background color for any necessary fill
text.  So, we have to decide what background colors to use for the
`Chunk`s before putting them into a `Box`.

We will alternate each record in the list--odd rows will have a
magenta background, and even rows, a default background.

> coloredForeAndBack :: [Record Chunk]
> coloredForeAndBack
>  = map colorBackground . zip [ (0 :: Int) ..] $ coloredForeground
>  where
>    colorBackground (i, r)
>      | odd i = fmap (<> b_magenta) r
>      | otherwise = fmap (<> b_default) r

Create `Box`es
==============

To put text into a `Box`, just use the `chunk` function:

> recordBoxes :: [Record Box]
> recordBoxes = map (fmap chunk) coloredForeAndBack

Create columns
==============

To format each column, you have to calculate which row in each
column is the widest.  Then you can size each box accordingly.
First let's make a `Record` where each member is a list of all the
`Box` for that column of data.

> unformattedColumns :: Record [Box]
> unformattedColumns = foldr f epty recordBoxes
>   where
>     epty = Record [] [] [] [] [] []
>     f r acc = Record
>       { firstName = firstName r : firstName acc
>       , lastName = lastName r : lastName acc
>       , address = address r : address acc
>       , phone = phone r : phone acc
>       , email = email r : email acc
>       , balance = balance r : balance acc
>       }


