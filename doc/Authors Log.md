# Authors Log for the Sapphire Programming Language

## About this document

Sapphire is (during its design) an experiment.  As such, the process of design
is interesting enough to warrant recording.  This is not a change log for a
stable program release, but rather a record of thoughts and ideas recorded as
Sapphire is constructed.

This file is **not** covered under the code or documentation licences.  This
file, Copyright &copy; 2013, 2014, 2015 by John F. Miller, is licenced under 
a [Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License][licence].

<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/deed.en_US"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png" /></a>

[licence]: http://creativecommons.org/licenses/by-nc-nd/4.0/deed.en_US

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
the are pure with respect to their context, they work as expected.

My idea is to have all contexts be objects with pure functions given new
anonymous objects while methods get self as the object.  There is still the
issue of formal parameters however. My hope had been to merge Ruby's local and
instance scopes but function parameters make this difficult.

It is now clearly time to work out the object model.  But before this can happen
a little time needs to be spent on parsing blocks and the REPL.  Hopefully not
too much.

## December 4, 2013 at 10:02am

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
the Object that contains them. If a function that closes over Object `A` is called in
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

In my bid to implement STM I have refactored the threaded code into a module
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

## January 3, 2014 10:50pm
## Continuations Continued

I'm back from Christmas Break.

I have worked through some basic benchmarking with the continuation code.  It
is not supremely fast, but it works at a rate that should allow it to present
only a moderate bottleneck.

The code is not elegant and needs some refactoring.  The ContM monad turns out
to be overkill and will be removed.  MessageQueues need to become internal again
but it is making testing easier that they are not.

The most vexing problem is that `dispatch` is so brittle.  The need to be able
to call back into an object (or in this case a ProcessId as Objects are abstract
with respect to continuations) means that I have to generate a new message queue
before I dispatch a message so that I can properly shadow the queue that I'm
going to block once I try to read the reply.  It bothers me that I need to know
how I will receive a message before I send it.

One way I think I can potentially avoid this is to have central dispatch
mechanism. My understanding is that this is how Erlang's run time works.  There
is a central dispatch process that receives messages to all threads then places
the message the proper queue.  By centralizing this, I could relieve the Object
of any responsibility for tracking it's own queue.  It would only need to supply
a responder and an initial value and the central dispatcher would take care of
the rest.

On the other hand this central process would become a congestion point in and
of itself.

For the moment I will press ahead with the current system, but I wanted to write
this down as something to possibly come back to.  Particularly if the present
system proves too inefficient.

## January 6, 2014 5:34pm
## Epiphanies for Epiphany

I need to walk out the door shortly, but I have a few notes I need to make

1) There is too much in Eval.hs and some of the plumbing needs to be more
generic and move elsewhere.  In particular dispatchM need to go in context with
a `StateMonad Context => m, MonadIO => m` type restriction.

2) Deferred evaluation *is* possible without creating a new new MessageQueue.
By using deferred evaluation the programmer has told us it is ok to accept
messages from other processes, including the one the message was sent to(!),
using the current message queue.  All we must do is send a message without any
shadow on our own queue.  Then use the same STM transaction used in
`Continuation.hs:dispatch` with the MessageQueue in our current Continuation.
Because we use this 'public' message queue, there is no chicken and egg problem
about when to create a new MessageQueue and how to let the called function know
to use it.  Because we will never block that queue waiting for its response, we
will not deadlock.

This in tern allows us to kick a few different methods concurrently and merge
them back together for the final answer (an important form of concurrency for
Sapphire)

To impliment this we will need a new Value which holds a `TMVar Value` (a.k.a) that can be both
read and checked for completeness etc.  Trying to make an object out of this
value is a matter of waiting for it to return while continuing to answer the
existing message queue.

Quick thought here.  What happens when one of these is passed into a function
that needs to complete before answering outside messages, but must not block
it's queue while while waiting for a value.  Possible solution is to form a TVar
that contains the current MessageQueue and replace it, but there are race
conditions here.  Another is to not pass a Future to another function (perhaps
unless it knows it's coming.

All thing to work out.  For now, I continue to work on pushing Continuation into
the rest of the system.  Currently I'm working on Contex and Graph which is good
because that is where the deadlock that started this mess lived.

## January 27, 2014 10:14am

### Update on the last three weeks

I have made much progress. Continuations now work as expected for the most part.
I have split the continuation coded from the sapphire spesific stuff.  The later
now lives in `Context.hs`.

Much of what I have done in the last three weeks has been code clean up.  By the
time I got continuations working the there was a lot of cruft left over, names
were inconsistent, and things were in the wrong files.  I have made a little
forward progress however.

Most significantly, primitive values can now find their classes.  This was a
necessary step before the next big project:

### Strings

While I envision LLVM compilation speeding up numerical calculations to
acceptable levels for an interpreted language, much of what I see sapphire
actually being used for is text manipulation.  To that end, I want a String
implementation that is more sophisticated then just a list/array of bytes. For
now I am content to use the Data.Text.Lazy package which works in a rope like
manner. This allows O(1) concatenation and shared substructures which are
important for a share nothing concurrency model.  What it does not do is allow
for O(1) shared memory slices.  This may be a reason to move to ByteStrings.

Today, I want to work through how to parse an interpreted string.  These
strings are one of Ruby's best innovations. Double quoted strings can have all
kinds of interpreted code in them.  For example the structure #{ ... } within a
double quoted string will evaluate the code between the brackets and insert the
result (perhaps with a `.to_s` call) into the string.  Like most languages, Ruby
uses the convention of the backslash ( \ ) as an escape character.

Implementing requires that I create some kind of structure that the evaluator
can interpret.  One possibility is that the string parser just spits out AST
code calling `concat` on the various chunks of text.  Another is to create a new
type of AST node.  My intent is to create a separate scanner just for strings.

### Lexing with Monads

This also brings up another long standing issue.  The current Tokens.x lexer
uses the `pson` wrapper in alex.  While this is a nice simple wrapper to get
everything started, it is starting to show its limitations.  Most significantly,
when it fails it simply dumps an error and crashes.  What is needed is to run
the lexer in and Error Monad.  This then requires a different wrapper.

The Monad and Monad User wrappers are a large jump in complexity. I have been
doing a close re-reading of both the [Alex basic interface][alex-basic] and the
[Alex monad wrapper][alex-wrapper] (code found [here][alex-code]). There is also
a nice example of all the possible features found in in [language-lua][lua] on
github.

One gottcha with the Monadic Lexer is that is does not provide a complete
scanner like the other.  The function `alexMonadScan` is retrieves only a single
token. The logic for repeatedly applying the scanner until the end of the file is
reached is left to the user.

At this point I think the best action is to get my feet wet with the string
lexer before moving on to the more complicated problem of the main lexer.

### Array and Hash

Even though strings do not depend on the underlying array implementation, the UI
needed for the string class will need arrays and hashes will be needed soon as
well.  The issue here is what to use as the underlying implementation.

With arrays the expectation is O(1) access to the members.  Haskell's lists are
O(n).  Haskell comes with an Array module.  However, I want to support both
dynamic length and sparse arrays, while still maintaining good performance in
the case of short fixed length arrays that are the common result of literal
arrays.  I am inclined to use a hybrid approach with fixed length arrays being
promoted to a more dynamic structure on first use.

Hashes, seem easier because there need not be any performance guarantees like
with arrays.  Obviously the desire is for the best performance possible with
lookups certainly in the O(log n) range.  I think a simple map or slightly more
complex HashMap would work. I will need to make Value a good instance of EQ and
possibly an instance of Hashable as well.  This could be tricky sense I cannot
use the IO monad.


## May 15, 2015

### Scope of Classes and Modules

What is the expecte behavior when the interpreter encounters a `class` or 
`module` statement? Certianlly it should create a new class or reopen an
existing one.  But where does it look?  The current comprimise is to put
everything in `Object` but this is not the expected behavior.  Modules and
Classes should create scope for thier nested children.  How do we impliment
this? My first thought is to add a value to context pointing to the current
scope.  But then how does `::Foo` work? I think it should still point to
`Object`.

Perhaps what is needed is a scope that can track both current and top scopes.

Then there is the question of what scope is current in the middle of a method
call.  It is fairly obvious how and where to switch scopes when opening classes
and modules, but what about on calls into methods defined in those modules.
Should this even be possible?

## May 16, 2015

### Explain the Code

At some point it will become necessary for someone other thne me to understand
how this code base works.  I have though about a set or articles of perhaps
videos that would take a prospective coder through the code explaining some of
the decissions that lead to its current form.

My idea to use the metaphor of exploring a cave system.  Once can start at the
one of the natural enterences: 1) following the running process through starting
from `Main.main` or 2) begining with a source file and following it through
exicution. One could also take the elevator down to the most interesing parts
and work back to the surface. That is start with `Object` or `Concurrent` and work
outword.

## May 27, 2015

I have gotten stuck trying to impliment `super`.  The issues is how to keep
track of why and how a method was called. At the point that a method's code is
invoked, it is just a function excicuted with a context containint the correct
version of `self`.  The function is actually created with the same eval path as
an anonomous function.  In particular, there is no record of what method name
was used or what class / module that function lived in.

The goal in the next set of changes is add enough information to `Context` (the
state in the EvalM monad) that the interpreter can figure out where to go next
to look for `super`.

There are two challenges here, neither of which is faced by Ruby. First
Sapphire admits proper tail calls which makes it much more difficult to undo
changes to the context at the end of the function.  We cannot simply pop a
pointer off the stack at the end of the function because the function may not
actually ever reach the end.  It may instead continue in some other function.

The second issue is how to find the "next higher" version of the function.  It
is very difficult to mark our spot in the Object Graph and even harder to return
to it.  In a more conventional implimentation in an single-threaded imparitive
language, we would jsut store a pointer to the right module/class.  We do not
have the luxury of pointers, and we cannot describer where we are becasue the
shape of the tree might change while our function is executing.  It might even
change because our function is executing.

My current track to solving this problem is to give every Class (as in the
Haskell Constructor for Object) a unique ID. This of course requires some global
source of ID's (random number, monotonically increasing integer, UUIDs) which
in turn implies some use of the IO monad. One thing to realize is that PIDs are
unique are instances of `Eq` so they can be used to identify a named class or
module.  However, we have just contructed anonimous modules which do not have in
independent PID nor even a unique name.

The thought then occurs to change the process field to `Either Int PID` and
perhaps rename it to something like identity.  Then there is the simple matter
of grabbing some form of uninque Int (or to prevent overflow Integer). This
value could then be stuck into Context and on super eval could run up the Object
Graph looking for a module or class with that Identity and move on to the next
occurance.

This solves the question of where to look but not the question of
what to look for.  The name of the current method also need to be in the
Context. Setting these correctly and not having them bleed over is going to
require an intense review of where contexts are created and how they persist
accross method calls.  It may be necessary toi ensure that a new Context is
derived for each funcion call and that it is discarded at the end of the call.
This *may* be how things work already, but I will need to carefully read my code
to find out.

## February 27, 2017

Wow, has it really been more than a year since I worked on this project? I
guess a lot has happened in 2016 including a cancer diagnosis.

I come back to Sapphire with the thought that everything has gotten a little
complex and that there are not clear deliniations between various aspects of
the system.  In particular, I would like to seperate the runtime from the more
primitive evaluation so that it can be reasoned about and tested without code.

In particular I would like a monad with a clear and minimal set of "external"
functions.  This is my working set:

  * `call`      --  send a message wait ro the response
  * `spawn`     --  start a new object thread
  * `getMethod` --  retrieve a method from the class in scope
  * `error`     --  send an error up the call stack
  * `return`    --  send a value back to the caller

I would like the runtime system to handle scope, including class, local
variables, responses, errors and stack mamagement.  In particular, the question
of defining were a variable or a class or a super class is should be handled
outside the exicuting code.  Keep `eval` simple and straight forward, leave the
messy parts to the runtime where it can be more easily tested and resaoned
about.

This though came up when thinking about how garbage collection might work, and
the desire to impliment a tow garbage collector on a minimal runtime. As thing
look now it would be hard seperate Eval from Continuation.  and much of the run
time is scattered all over the code.

I want to rewrite with some very distinct pieces that interact accros strict
APIs:

  * Parsing    -- source code to AST
  * Evaluation -- exicuting the AST for values and effects
  * Runtime    -- Orginizing object and delivering messages.

[alex-basic]: http://www.haskell.org/alex/doc/html/basic-api.html
[alex-wrapper]: http://www.haskell.org/alex/doc/html/wrappers.html
[alex-code]: https://github.com/simonmar/alex/blob/master/templates/wrappers.hs
[lua]: https://github.com/osa1/language-lua/blob/master/src/Language/Lua/Lexer.x
