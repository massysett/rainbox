Rainbox
=======

Provides pretty printing of boxes in two dimensions.  Rainbox is
useful for console programs that need to format tabular data.

Documentation
=============

In addition to the Haddock documentation, a tutorial is available in
[the Rainbox.Tutorial module](lib/Rainbox/Tutorial.lhs).  This
module is best read in your text editor or through the Github web
interface, as it is written in literate Haskell, which HsColour does
not fare so well with.

Portability
===========

There's nothing unportable in Rainbox; however, it does use
[Rainbow](http://hackage.haskell.org/package/rainbow) which works
only on UNIX-like systems because it uses the UNIX terminfo library.
I only develop for UNIX-like systems because they are the only ones
I use.

Tests
=====

You can simply use "cabal test".  However, I recommend that you do:

    cabal configure --enable-tests
    cabal build
    dist/build/rainbox-test/rainbox-test
    dist/build/rainbox-visual/rainbox-visual

The last test, `rainbox-visual`, relies on you to examine the output
and make sure it looks correct.

Tests are also run on Travis:

[![Build Status](https://travis-ci.org/massysett/rainbox.svg?branch=master)](https://travis-ci.org/massysett/rainbox)

and although you can see the output of `rainbox-visual` there, it's
not formatted quite right on Travis.

At this time, Rainbox is verified to work with GHC versions 7.4.1,
7.6.3, and 7.8.2.

License
=======

Rainbox is licensed under the BSD license; please see the LICENSE
file.
