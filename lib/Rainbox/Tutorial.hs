{-| The Rainbox tutorial

Rainbox helps you create arrangements of (possibly) colorful text
boxes. This module contains a tutorial.  Typically the "Rainbox"
module contains all you need.  There is also a "Rainbox.Core" module
which contains all the innards of Rainbox, but ordinarily you won't
need it.

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

The next biggest building block after the @core@ is @payload@.  There
are two different types of payloads: vertical payloads and horizontal
ones.  A vertical payload aligns itself next to other vertical
payloads on a vertical axis, creating a chain of payloads.  The
vertical payload holds either an entire 'BoxH' (more on that later) or
a 'Core'.  The vertical payload also has an alignment.  The alignment
determines whether the payload lines up along the axis on its left
side, right side, or in the center of the payload.

The vertical payload also has a background color, which as type
'Radiant'.  Think of the background color as extending infinitely from
both the left and right side of the vertical payload.  When the
vertical payload is combined with other vertical payloads into a
'BoxV', this background color is used as necessary so that the 'BoxV'
forms a rectangle.

The horizontal payload is similar to the vertical payload, but the
axis is horizontal rather than vertical.  The alignment determines
whether the payload aligns the axis along the top, center, or bottom
of the payload.  A horizontal payload also contains a background
color; it extends infinitely from both the top and bottom of the
horizontal payload.  A horizontal payload also contains either an
entire 'BoxV' or a 'Core'.

Finally, the biggest building block of Rainbox is the box.  There are
two types of boxes: a 'BoxH', which holds zero or more horizontal
payloads, and a 'BoxV', which holds zero or more vertical payloads.
Each kind of box is a 'Monoid', so you can combine it using the usual
monoid functions.  So, to give a visual, a 'BoxV' with five payloads
might look like this:

@
           V Vertical axis


           +----+
           | v1 |
           |    |
           +----+--------+
           |     v2      |
           |             |
  +--------+-------------+
  |        |
  |  v3    |
  |        |
  |        |
  +--------+----------+
           |    v4    |
      +----+----+-----+
      | v5      |
      |         |
      +---------+

@

Each payload is marked in the middle with @v1@, @v2@, etc.  Note how
each payload has a different size.  @v1@ and @v2@, and @v4@ have a 'left'
alignment, as their left edge is lined up with the vertical axis.
@v3@ has a 'right' alignment.  @v5@ has a 'center' alignment.  Think
of each payload has having a background color extending infinitely off
of its left and right sides.  These five payloads put together make a
'BoxV'.  Since 'BoxV' is a monoid, you can combine various 'BoxV'.
Indeed, the pictured 'BoxV' can be built only by combining smaller
'BoxV', as when you create payloads they are always given to you as a
single payload wrapped in a 'BoxV'.

Now, you want to render your 'BoxV'.  You use the 'render' function,
which makes a sequence of Rainbow 'Chunk'.  This turns your 'BoxV'
into a nice rectangle for on-screen rendering:

@


  +--------+----+--------+
  |        | v1 |        |
  |        |    |        |
  +--------+----+--------+
  |        |     v2      |
  |        |             |
  +--------+-------------+
  |        |             |
  |  v3    |             |
  |        |             |
  |        |             |
  +--------+----------+--+
  |        |    v4    |  |
  +---+----+----+-----+--+
  |   | v5      |        |
  |   |         |        |
  +---+---------+--------+

@

The spaces to the left and right of each payload are filled in with
the appropriate background color, which is the background
color of the adjoining payload.


What if you want to place the 'BoxV' alongside another box?  If you
want to put it next to another 'BoxV', just use 'mappend' or '<>'. But
what if you want to put it next to a 'BoxH'?  Let's suppose you have a
'BoxH' that looks like this:

@




                        +----+
                        | h1 |
                        |    |
Horizontal Axis >       +----+----------+
                             |          |
                             |   h2     |
                             |          |
                             +----------+

@

The @h1@ payload has alignment 'bottom', because its bottom edge is
lined up along the horizontal axis.  The @h2@ payload has alignment
'top'.  You want to connect this 'BoxH' with the 'BoxV' made above.
You can't connect them directly because they are different types.  You
can, however, take a complete 'BoxH' and convert it to a 'BoxV', and
vice-versa.  You do this with the 'convert' function, which is
applied to the alignment for the new box, its background color, and
the box you want to convert.  So, let's say you take the 'BoxV'
created above and convert it to a 'BoxH' with 'top' alignment and a
background color, and then you combine it with the 'BoxH' created
above.  The result:

@



                         +----+
                         | h1 |
                         |    |
  +--------+----+--------+----+----------+
  |        | v1 |        |    |          |
  |        |    |        |    |    h2    |
  +--------+----+--------+    |          |
  |        |     v2      |    +----------+
  |        |             |
  +--------+-------------+
  |        |             |
  |  v3    |             |
  |        |             |
  |        |             |
  +--------+----------+--+
  |        |    v4    |  |
  +---+----+----+-----+--+
  |   | v5      |        |
  |   |         |        |
  +---+---------+--------+

@

The old 'BoxV', which is now wrapped in a 'BoxH', now has a background
color which extends infinitely from the top and bottom of the box.  It
is now just a payload inside of the 'BoxH'.  The other two payloads in
the 'BoxH', @h1@ and @h2@, also have background colors extending from
their tops and bottoms.

So, when you render this 'BoxH' with 'render', you get this:


@



  +----------------------+----+----------+
  |                      | h1 |          |
  |                      |    |          |
  +--------+----+--------+----+----------+
  |        | v1 |        |    |          |
  |        |    |        |    |    h2    |
  +--------+----+--------+    |          |
  |        |     v2      |    +----------+
  |        |             |    |          |
  +--------+-------------+    |          |
  |        |             |    |          |
  |  v3    |             |    |          |
  |        |             |    |          |
  |        |             |    |          |
  +--------+----------+--+    |          |
  |        |    v4    |  |    |          |
  +---+----+----+-----+--+    |          |
  |   | v5      |        |    |          |
  |   |         |        |    |          |
  +---+---------+--------+----+----------+

@

The area above the old 'BoxV' has the background color that we used in
the application of 'convert'.  The area below the @h1@ payload has its
background color, and the area above and below the @h2@ payload has
its background color.

What if you just want to create an empty space?  You can create entire
blank boxes with 'blank', but often it is enough to use 'segment,
which gives you a one-dimensional 'BoxH' or 'BoxV'.  If you are
creating a 'BoxH', 'segment' gives you a box with width, but no
height; for a 'BoxV', you get a box with height, but no width.  So, to
return to the example 'BoxH', let's say you want to add a blank space
a few columns wide on the right side.  You 'mappend' a 'BoxH' created
with 'segment' to get this:

@

                        +----+
                        | h1 |
                        |    |
Horizontal Axis >       +----+----------+--+
                             |          |
                             |   h2     |
                             |          |
                             +----------+
@

On the right side you now have a payload with width, but no height.
But it does have a background color.  So when you 'render' the box, you
get this:


@

                        +----+----------+--+
                        | h1 |          |  |
                        |    |          |  |
                        +----+----------+  |
                        |    |          |  |
                        |    |   h2     |  |
                        |    |          |  |
                        +----+----------+--+
@

Those are the basics of the Rainbox model, which should be enough to
get you started.  Also helpful is the 'table' function, which will
help you build a simple grid that resembles a spreadsheet; see its
documentation for hints to get started with that.

-}
module Rainbox.Tutorial where
