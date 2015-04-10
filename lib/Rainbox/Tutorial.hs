{-| The Rainbox tutorial

Rainbox helps you create arrangements of (possibly) colorful text
boxes. This module contains a tutorial.  Typically the "Rainbox"
module contains all you need.  There is also a "Rainbox.Core" module
which contains all the innards of Rainbox, but ordinarily you won't
need it.

The tutorial contains live code, and you're best off viewing the
source code.  Viewing the Haddocks alone won't help you as much.

The basic building block of Rainbox is known as a @core@.  A core is
either a single 'Chunk' or a blank box of arbitrary size.  A core
made of a single 'Chunk' should not contain any newlines.  Leave
newline handling up to Rainbox.  However, Rainbox will not check to
ensure that your 'Chunk' does not contain any newline characters.  If
it does contain newlines, your boxes will not look right.

Many things in Rainbox have height and width.  Both the height and
width of an object can be zero, but never less than zero.  A @core@
made from a 'Chunk' always has a height of 1, and its width is equal
to the number of characters in the 'Text's that make up the 'Chunk'.
A @core@ made from a blank box has the height and width that you give
it, though neither its height nor its width is ever smaller than zero.

The next biggest building block after the @core@ is @payload@.  Every
box has a payload.

A blank box of arbitrary size can be useful to help your other boxes
line up how you like them.

Though the @core@ is the basic building core of Rainbox, there is no
type that directly represents cores.  Instead, the core is the basic
component of the 'BoxH' and the 'BoxV'.  A 'BoxH' consists of zero or
more cores.  Each of the cores in the 'BoxH' is lined up along a
horizontal axis, rather resembling a train of railroad cars.`

There is
no type that directly represents cores.  Instead, there are two key
types, 'BoxH' and 'BoxV', each of which contains 

-}
module Rainbox.Tutorial where
