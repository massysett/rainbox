Rainbox
=======

Provides pretty printing of boxes in two dimensions.  Rainbox is
useful for console programs that need to format tabular data.

On Hackage
==========

https://hackage.haskell.org/package/rainbox

Documentation
=============

In addition to the Haddock documentation, a tutorial is available in
[the Rainbox.Tutorial module](lib/Rainbox/Tutorial.hs).

Portability
===========

There's nothing unportable in Rainbox; however, it does use
[Rainbow](http://hackage.haskell.org/package/rainbow) which is only
tested on UNIX-like systems.

Tests
=====

You can simply use "cabal test".  However, I recommend that you do:

    cabal configure --enable-tests
    cabal build
    dist/build/rainbox-properties/rainbox-properties
    dist/build/rainbox-visual/rainbox-visual

The last test, `rainbox-visual`, relies on you to examine the output
and make sure it looks correct.

Tests are also run on Travis:

[![Build Status](https://travis-ci.org/massysett/rainbox.svg?branch=master)](https://travis-ci.org/massysett/rainbox)

and although you can see the output of `rainbox-visual` there, it's
not formatted quite right on Travis.

At this time, Rainbox is verified to work with GHC versions in the 7.8 series
and the 7.10 series.

License
=======

Rainbox is licensed under the BSD license; please see the LICENSE
file.
