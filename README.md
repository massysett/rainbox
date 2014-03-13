Rainbox
=======

Provides pretty printing of boxes in two dimensions.  Rainbox is
useful for console programs that need to format tabular data.

Current development status
==========================

Right now the library is done and all the tests pass; I'm working
on documentation.

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

License
=======

Rainbox is licensed under the BSD license; please see the LICENSE
file.
