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

## December 10, 2013 at 9:04

Last night I encountered what I believe will be the first of many dead-lock and
race condition errors. Fortunately, this particular deadlock was extremely
consistent, and I have already identified the source of the problem.

When a Pid receives a call message, it must first find the method by searching
the object graph.  Because the top of the object graph is designed to loop back
through itself, it is possible for Object to be blocked waiting for a method
that is currently held in its cvars.  There are a number of ways to work around
the immediate problem.  One is to add a `Modify` message that solves allows me
to do what I need without using a call, another is to move the `setCVar` method to
a place before the loop occurs.  However the underlying problem of Pids looping
back on themselves is a fundamental problem that need to be solved.

I believe the solution is to respawn a new message response queue with its own
Chan, but the same tid and pass this along with any any time a Pid will block
waiting for a response (see function cps in object for prime example). A Pid
receiving such a message is then obligated to use this new channel in preference
to any others with the same tid, as they will be blocked waiting for a response.

In fact sense many Pids could block on the same call chain, this will need to be
some kind of list of shadowed Pids. This 
could be a performance bottleneck.

There is also now the issue of needing the response and the message queue to
both wake up the thread. Some possibilities here are a modified type of Chan, or
perhaps more likely the `orElse` combinator of the STM library. The need to
constantly use `atomically` was a good reason to try to avoid the added
complexity of STM, but faced with the present reality, it may add less
complexity, and be cleaner then a solution that only uses MVars.  Some
experimentation will be necessary.

## December 13, 2013 at 11:20pm
## Continuations

In my bid to implement STM I have re factored the threaded code into a module
called `Continuation`.  The module is named for the traditional programing
concept of the place a function call returns to once complete and the roles are
somewhat similar.  My continuation is a data structure rather then a stack frame
however.

Rather then placing the response on the top of the stack an returning (or in the
case of Haskell generation a result value in a function) responses are
accommodated by means of a (T)Mvar.  This structure is either empty or holds a
value.  Writing to a full MVar or reading from an empty one cause the thread to
block (or in the STM case the transaction to retry) until the condition changes.
In the naïve approach implemented in Master, a thread would place a call
including this MVar into the message queue of another thread and then try to
read the MVar -- blocking until the other thread filled it with the result.

In order not to deadlock when one process calls another and then receives a call
in return, any process waiting for a response must still respond to new
messages. (This is the problem that cause the deadlock the STM branch is trying
to address) However, data integrity requires that *only* calls from "downstream"
of the dispatch point be answered.

The solution is to add a second data structure to the Continuation: an
associative list of ThreadId to message queues. When ever new message is sent
this list is consulted and if the thread has an entry then that message queue
shadows the one in the Pid reference.

Further, before a thread may block waiting for a response it must create a new
message queue and place it and its own threadId at the head of the list before
sending the message. It then processes the new message queue which only holds
messages from the downstream call sequence. These messages are processes prior
to retrieving the response in the same way the primary (but now bocked) queue
would. The STM `orElse` combinator is used to allow this multiplexing.  The
relevant line is Continuation.hs:79,73.

In order to respond to messages, Objects wishing to block waiting for return
values must provide enough information to the dispatch method to create the
response loop. This is going to require a significant rewrite of Spawn.

One of the new complexities is keeping track of the current continuation.  A
copy lives in the Context structure used by the EvalM monad.  Because
Continuations work in the STM monad and EvalM must be a MonadIO the two cannot
be merged. There will be a lot of glue code needed to get relevant data to move
back and forth between the two areas.

Proper tail calls are implemented by sending the TMVar hole for the result to
the next function in line instead of creating a new one and waiting for it to
fill.

This structure allows for a unique feature of Sapphire.  Values can be returned
before the calculations are complete if the response is available.  The `return`
statement puts the value into the Continuation but does not necessarily end
computation in the function.  Imagine a function that took a long time to
process. It could be kicked of by a function that returns as soon as the
computation is set to start but then allows the initial thread to resume while
the computation is on going. One could also imagine passing a call-back that
could be triggered when the long function was complete.

Asynchronous sends, proper tail calls, and returns do not need to perform
special message queue functions because these will all complete and the primary
queue will then unblock to handle further messages.
