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

Everything you need from the `rainbox` package is in the `Rainbox`
module.  Alternatively, you can use the `Rainbox.Reader` module,
which has the same purpose as `Rainbox` but wraps many of its
functions so that they are used from a `Reader` monad.  In this file
we'll only discuss the `Rainbox` module.

> module Rainbox.Tutorial where {
> }
