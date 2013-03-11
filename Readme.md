# Saphire - A High Level Concurent Dynamic Language

## About
Sapphire (a kind of Ruby, pun intended) is an experimental new programming
language that aims to meld the expressive syntax of Ruby with the parallel
process model of Erlang.  Best of class concepts from other languages are also
mixed in.

Sapphire is currently in the development stages for a proof-of-concept program.
There are no firm plans to take this language to a production state. It should
instead be understood as a cool experimental toy. (Don't bet your start-up on
me!)

## Main Idea

The driving concept behind Sapphire is to take Ruby's everything-is-an-object
paradigm and make every object a process with a message queue in the style of
Erlang. In reality one process per object is too many, so the engineering
compromise is to say every object *can be* a process, and every process is an
object.

To make this work, we require (semantically[^practice]) that processes have 
fully independent data. If two object want to share data with each other they
have two choices. One is to copy the data from one to the other.  This lets one
object know something about the state of another object **when the request was
made**. If two objects which to share and update the same piece of data they
must first turn that data into a process that they both read form and write to.
In effect the shared process-object works as a semaphore for the shared data.

## Concurrent Parsing

Sapphire aims to make extensive use of concurrent computations.  To that end 
the parser is designed to be lazy and concurrent.  Each module and class is 
interpreted concurrently. 

The indentation rules are run by a line scanner that is fast, brutish and dumb.
Its world consists of spaces (U+0020) new lines (U+000a) and everything else.
It produces `blocks` and `lines`.  A block is a list of lines at the same level
of indentation. A line is all characters between the first non-space character
and a new line character plus a block of all lines following whose indentation
is strictly greater then the leading line. Lines consisting only of spaces and
a new line character are ignored. All Sapphire statements must be on one line
(which includes the block that follows it).

The line scanner knows nothing of the grammar it scans, nothing can break the
rules. Comments, string literals and the like all must obey the rules. The 
benefit of this thuggish line scanner is that we can quickly determine the full
extent of a class, module, or function.  All fit on a single line and the block
which follows.

This also makes it very simple to embed independent syntactical structures into
Sapphire. The line scanner pulls off the demarcating spaces and returns rest of
the syntax intact. There is no possibility of a end tag collision

We use `parsec`, a monadic parser combinator to parse Sapphire. The concurrent
parsing helps to offset the penalty in raw speed, while the flexibility allows
for parsing to be a built in language feature.

## Implementation

Sapphire is written in Haskell.  The goal is to create an Interpreted language
backed up with JIT LLVM compilation for common functions.  A stretch goal is to
make a broad subset of Sapphire compile to stand-alone executables and then to
become self-hosting

## Syntax

*this is a work in progress*

Blocks and Lines:

A `block` is a list of `line`s at the same indentation level. A `line` is the text
from the first non-space character to the end of the line *plus* the `block` of
lines the follow immediately and are more indented then the line.

Comments and Processor controls:

The character `#` is the process control character. It introduces syntax which
is intended for the interpreter/compiler is not part of the semantics of the
program. If the # is followed by a space or any character other then the ones
listed below then it is introducing a standard comment. All text to the next
new line is ignored unless the `#` is the first character of a `line`[^line]
then the entire `line`, including any block which follows, is ignored.

The symbol `#@` introduces a documentation comment. These comments belong to
the context of the `line` to which their block belongs (or the file if at top
level) and are parsed by a special documentation parser.  The text is stored
with the class, module or function and can be accessed at runtime unless it
has been stripped.  This is specifically intended to allow libraries to be
discovered from a REPL.

The symbol `#$` introduces instructions for the compiler or interpreter. For
example, these could enable non-standard language features similar to the
function of Haskell's Pragmas construct `{-# ... #-}`, request or inhibit the
JITing if a function or class, or some aspect of the process model.

Classes:

Like in Ruby, everything in Sapphire is an object, and every object is an
instance of a class.  The keyword `class` introduces code for a class.  Classes
are a special case of objects which *must* be an independent process. Classes
may have a name which by convention should have initial caps. Unlike Ruby this
is a convention only.  Classes are first class values and as such my be placed
in any legal variable in any context. If a name is given after the `class`
declaration it is assumed to be in the context of the containing module, or at
global scope if at the top level.

The block which follows is executed in the classes context. If the class does
not exist it is created. If it does it is reopened.

Every class has a super class.  If none is specified `Object` a built-in super
class is used. Super classes are specified with the reserve operator `<-` after
the class name. If the object after the `<-` is not an ancestor of `Class` then
a new class is created with the object's original class as a super class, and
the object becomes an instance of the new class. This last form allows the
programmer to give special functionality to a single instance.

Modules:

Modules, introduced with the key word `module` form separate name spaces. 
Modules are hierarchical, and by default a new module will be place in the
name space of the enclosing module.  Per our rules, because each module is
shared by all the object in it, each module is an independent process.

Scope Operator:

The scope operator is `::`.  It allows access to scopes other then the current
one. Contexts are named from the outside in. If the first named context is
found in the current scope then it is used.  If it is not, successively higher
contexts are used until one is found.  Begining a variable name with `::` 
begins immediately searching the highest available (Global) scope.

## Semantics

Before executing the interpreter or runtime sets up a standard configuration of
classes including `Object`, `Class`, and `Module` as well as the classes for
all the literals found in Sapphire: `String`, `Number`, `Symbol`, etc

## Freezing

When a class or module is frozen it is a binding promise that it will not be
modified for the remainder of the run. It allows optimizations that make
method calls in particular much faster because they can be cached at the
object level.

This means that classes and modules can no longer be reopened (though they may
be instantiated and sub classed).  Any object may be frozen by calling 
`Object::freeze`, a special function intended for the purpose.

A class or module may also invoke the `freezable` method which simply marks the
object complete according to its original designer, but still allows set-up
code to make modifications (monkey patching) before the final freeze. The 
`compile` function then freezes all classes and modules marked as freezable.

There is no thaw.  Other objects will immediately begin to depend on the
immutability of frozen object.

We believe that any completely frozen module should be fully compiliable to
LLVM. If the executed file can be frozen as well then it should be possible
to create a stand alone executable.

[^practice]: As implemented, we use immutable memory and a copy-on-write scheme
which we get for free with Haskell

[^line]: Remember a `line` begins at the first non-space character
