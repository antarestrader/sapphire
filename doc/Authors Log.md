# Authors Log for the Sapphire Programming Language

## About this document

Sapphire is (during its design) an experiment.  As such, the process of design
is interesting enough to warrant recording.  This is not a change log for a
stable program release, but rather a record of thoughts and ideas recorded as
Sapphire is constructed.

## December 2, 2013 8:20am

Last night I achieved the basics of function application.  This morning I am
looking to achieve function construction. This is the final step necessary before
I dive into building the Object system which will make or break Sapphire.

There are three (or four) ways to create a function in Sapphire: anonymous
functions using the `lambda` keyword, `do` notation, or labeled function blocks;
named functions using `define` which assign a function to a local variable; and
methods which become part of a class and include as their first (hidden)
argument the reference to `self`.

A function value is being implemented as a (haskell) function from the arguments
to a returned value in the EvalM monad.  This allows built-in functions to be
made indistinguishable from the constructed ones in the interpreter, but will
present a problem when it comes time to compile functions.  I will cross that
bridge when I get to it.

A second lesser concern is upgrading the REPL in Main.hs.  Because of the heavy
use of blocks, I will appreciate having a multi-line input method soon.  I will
search for a pre-built library for this, but I don't want to get side tracked.

## December 2, 2013 at 4:25pm

I have struggled with scope. As implemented, a function inherits its context
from the point where it was defined. The problem is that even if a local
variable is altered it is stuck in the state it was in when the function was
defined.  I think that as the object model becomes more sophisticated this will
work itself out.

For the time being however, I can now define functions using lambda. So long as
the are pure with respect to their context, the work as expected.

My idea is to have all contexts be objects with pure functions given new
anonymous objects while methods get self as the object.  There is still the
issue of formal parameters however. My hope had been to merge Ruby's local and
instance scopes but function parameters make this difficult.

It is now clearly time to work out the object model.  But before this can happen
a little time needs to be spent on parsing blocks and the REPL.  Hopefully not
too much.
