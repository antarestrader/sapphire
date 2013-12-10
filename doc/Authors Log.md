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

# December 4, 2013 at 10:02am

Yesterday I implemented the `If` statement. I want it to be intuitive,
but also work with the LineParser model.  This has made it the most complex item
to parse so far. I have had to add a new function to Parsec to make it and
`block` report errors correctly. The parser has definitely left the realm of
LALR(1) and likely context free, but them so has C11.  With the if statement
intact, I can now say with certainty that Sapphire is Turing complete.

It is now time to work on Objects. I am going to have to feel my way through
some of this, and I may have to rewrite the object model after I give it a first
try. "Once you build a house you will know how one should be built"

My biggest concern is the process of finding and calling method.  Unlike Ruby,
functions are truly first class values in Sapphire. One struggle then is how to
keep closures from ruining the concurrency model. Another is how to effectively
find and call methods.

In particular should methods be separated into their own category as a value, or
should they just be functions whose first parameter must be self?

This is all tied in with the question of how Context will work as well. The
concurrency model depends on the fact that every value in Sapphire belongs to an
Object. (Or is in a local scope that may only leave the local scope by being
made part of an Object.) Values may only be manipulated (read and written) by
the Object that contains them. If a function the closes over Object `A` is called in
the context of Object `B` *and* that function is not Value Pure it must run in
the context of Object `A`.  How do I make that context switch happen?

Note on "Value Pure." Sapphire is not a purely functional language. Functions
can at any time preform IO actions and every function is assumed to be able to
return different values each invocation.  A Value Pure function is one that does
not read or write any Value other then 1) those passed in as arguments or 2)
those that exist outside the control of the Sapphire environment (e.g. read a
file). It is even possible to stretch this a little and allow Value Pure
function to close over local variables from the context in which they were
created. What they may not do is touch `self`.

## December 7, 2013 at 12:09 am

I began implementing Objects today (yesterday by 9 minutes).  The issue I have
run into is how to handle the "global" namespace for classes. When a class is
created by say `class Foo` the expectation is that we can then ask for a new
instance with `Foo.new` and it will be found in most contexts.

In ruby, this is handled by making the class namespace global.  Global names
spaces are more difficult in Sapphire because it destroys threading.  My thought
at the moment is to leverage the RTS by having it call a method in the current
context something like `install_class "Foo" <class Foo>`.  This may also be the
way to deal with classes with no default super-class -- ask for "Object" in the
current context or possibly `get_default_base_class`.

The next hard step is to define functions to walk through the object graph and
find values by name.

## December 9, 2013 at 9:52am

### The Object Graph

The object graph consists of three types of elements: Simple objects which are
leaves on the graph, with no incoming connections and only an outgoing
connection to a single class, Class objects which have a super-class edge as
well as in instance edge (nominally to the class `Class`) and my have many
incoming instance nodes and many included modules, and modules which may have a
super class and be included in other objects.

There is also a single ROOT object which is the superclass of exactly one Class
(nominally `Object`).

An un-scoped identifier is found by a search up the object graph in the following
order:

  1 The local scope which shadows everything.
  2 The instance variables of `self` in the context
  3 The modules included in `self` (elided in initial implementation)
  4 The class variables of `self`s class
  5 The class variables of the super-classes of the above class
  6 When ROOT is encountered in the search the identifier is not in scope

One issue is that it may take many called and context switches to find a single
function or variable.  A process of freezing frequently used, but not typically
altered classes so that their methods can be cached is a planned optimization.

It is still unclear to me if distinguishing `ivars` form `cvars` is the right
choice. My gut instinct is that it is, so my initial attempt will include the
separation, but I may find I like a more JavaScript like unification better.

Also not yet included in this model are visibility rules.

### Proposed default graph

`Object`:  The base object form which all other are derived
           also the default super class
  klass:   Class
  super:   ROOT  (later implementations may acquire Ruby's BasicObject.)
  modules: Kernel

`Class`: The class of which all classes are instances
  klass: Class
  super: Object

`main`: the initial context
  klass: Object

`Kernel`: The module that holds all the built-in functionality of Sapphire
  klass: Module

Some example:

`foo.bar` where `foo` is an instance of `FooClass<-Object`:

  * send foo "bar" -- foo becomes the context
  * check ivars of foo for "bar" (2)
  * move to foo's class FooClass and check cvars for bar (4)
  * move to FooClass's super class Object and check cvars for "bar" (5)
  * find that Object's super class is ROOT and abandon search.

`puts "hello world"` in the `main` context

  * check local variables for "puts" (1)
  * check main's ivars for "puts" (2)
  * move to main's class Object and look for puts in cvars (3)
    (success)

`FooClass.new` in `main context

  * Find FooClass
    * check local variables for "FooClass" (1)
    * check ivars of main for "FooClass" (2)
    * move to main's class Object and look for puts in cvars (3)
      success
  * send "new" to FooClass -- "FooClass" becomes context
    * check ivars of FooClass for new (2)
    * move to FooClass's class Class and check cvars for "New"
      success

## December 9, 2013 at 3:37pm
## Thoughts on PID in Object Data Structure

I've put PID's into the Object Data Structure. Almost everywhere that I wanted
an Object I actually wanted Either Pid Object. I am pretty certain that there
will be a few corner cases where I will want to have a real object and not just
a reference, but I think I can code around those.
