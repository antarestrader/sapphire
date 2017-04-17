Sapphire Syntax
===============

Sapphire syntax is mostly inspiried by Ruby with the obvious exception of
having indentation sensitive syntax rather then Ruby's `end` expressions.
There is some addational syntactic structure that aids in the use of a more
functional style of programming.

Lines and Blocks
----------------

### Line ###

A **`Line`** is a string of text begining at the first *non-space* charactor
and ending at a new line, *plus* any block of lines of hight indentation level
which follow.  A logical `line` may span several actuall file lines because it
included all lines more indented which follow. All expression in Sapphire must
be in one `line`.

### Block ###

A **`Block`** is a series of consecutive lines at the same level of
indentation.  A file is considered a block with implied indentation of zero.

### Negative Indentation ###

Negative indentation is the name for the situation when a line is less
indented then the one above it, but more indented then the line which contains
the block.

    This line contains all the lines which follow as a block:
        This line starts a block.
        This block has indent level 4.
      This line starts negative indentation.
      It is more indented then the first line,
      but not as much as the line above it

When this occures, the original (more indented) block becomes the child block
of an empty logical line with an indentation that is the same as the
negatively indented line.

In the example above their is one line with a block of four lines connected to
it.  The first line of that block is an empty line with a block of the two
lines indented at four spaces.

### Indentation Syntax ###

Indentation must be done with spaces (U+0020).  Encountering a TAB charactor
(U+0009) before the first non-space charactor is an error. Once lines and
blocks have been created, all leading whitespace is stripped out.

### Rational ###

The use of [indentation sensitive syntax][1] is not an arbitrary or even an
aesthetic choice.  Sapphire parses and executes programs in a concurrent
manner.  The use of symbols or key words would require that the parser read
the entire program to discover its structure.  By using indentation, the
program structure can be read by tracking nothing but the leading spaces on a
line.  Blocks can then be parsed concurrently and with the context of their
enclosing line.  In addition to concurrent parsing this also allows for easy
and completely unambiguous embedding of other syntax within Sapphire source
code.


Lexical Structure and Literals
------------------------------

The charactor set for Sapphire is unicode.  Files are assumed to be in UTF-8
encoding.  Files begining with a Byte Order Mark will instead be treated as
UTF-16 encoded. Note, ASCII (but not Latin-1) is a subset of UTF-8.  ASCII
files are therefore valid source files.

Reserved words, identifiers and literals may be seperated with
any amount of white space.  **Exceptions**: white space at the start of a line
must be per the rules laid out in the "Lines and Block" section above.  A new
line may only occure within a list expression enclosed in parenthese or at the
start of an expected block structure.

### Reserved Words ##

The following words are reserved and by not be used as identifiers:

    self, if, then, else, elsif, class, module, do, while, until, case, when,
    end, nil, false, true, def, define, lambda, super

### Identifiers ###

Identifiers name some element of the program and are chosen by the author.
Valid identifiers start with a latin letter or and underscore (`[a-zA-Z_]`)
and continue with zero or more of those characters plus numbers, `!`, and `?`.
(`[a-zA-Z0-9\?\!]*`).  Reserved words cannot be used as identifiers.

Sapphire uses identifiers begining with two underscores internally.  The names
of such identifiers are not all documented and are subject to change without
depriciation warnings. Programmers should not use identifiers begining with
two underscores to avoid conflict with this internal data.

### Decorated Identifiers ###

Identifiers may be decorated with special charactors at the begining and
possibally ending.  These decorations change the meaning of the identifier as
described, but the rules for forming them remain the same.

Identifiers begining with a capital letter are reserved for Classes
Modules and Constants.

### Operators ###

Operators are symbols used in infix formulas.  Operators can be defined with a
Sapphire program as described elsewhere.  operators consist of one or more of
the following symbols `+ - \ < > % $ ^ ~ = * ~ & |`. Operators ending with an
equal sign (`=`) are treated as special assignment operators except in the
case of the reserved equality opperators below.

### Reserved Operators ##

  * `=`  assignment opperator.  May not be redefined
  * Equality tests: `==`, `<=`, `>=`, `!=`.  May be redefined but are NOT
    assignment opperators even though they end with an equal sign.
  * `===` the equivelence operator.  May be redefined but is not an assignment
    operator.
  * Arrows `<-` and `->` are reserved operators and may not be defined.

### Numbers ###

Sapphire has two basic types of numbers a floating point value (represented as
a double) and an unbounded integer. Integers are one or more digits. Floating
point literals must start with a digit and include a decmal point with at
least one digit following. They may optionally include an exponent with is an
`e` or `E` optionally followed by a `+` or `-` followed by an integer
exponent.

Examples:
    1               # Integer
    346322200010991 # Integer
    0.5 # Float
    1.76e-21 # Float with exponent
    97.0E5   # Float with exponent

### Atoms ###

Atoms are string lables with no other value.  Atoms are formed by decorating
an identifier with a prefix colon `:`.

### String Literals ###

String literal may be formed in three ways: single quoted (non-interpreted)
strings, double quoted (interpereted) strings and blocks.

The creation and manipluation of marked-up natural-language text is one of the
motivation design goals of Sapphire.  As such its strings are more
sophisticated internally than other languages.  Using Sapphire to manipulate
strings is beyond the scope of this syntax reference, but should be examined
by any prospective programmer.

#### Single Quoted String Literals ####

As their name implies single quoted string literals begin with a `'` single
quote and extend until the next one is reached.  All characters between the
quote marks are taken at face value except the escape sequences `\'` a literal
single quote mark and `\\` a single back-slash. New lines may not appear in a
single quoted string.

#### Double Quoted String Literals ####

By starting a string with a double-quote (`"`) the programmer begins an
interpeted string. Within these strings the standard set of back-slash marked
escape sequinces are available.

The form #{ ... } allows sapphire code to be embeded in a string.  The string
value of the result is inserted into the code.

### Array Literals ###

Array literals are formed with an opening square bracket, `[`, followed by a
comma seperated list of expressions and a closing square bracket, `]`.  If a
new line is encountered before the closing bracket, the block following will
be assumed to have the rest of the array and will be parsed as code.

### Hash Literals ###

Hash Literals begin and end with a curlie brace, `{  }`, between the braces
is a comma seperated list of key value pairs.  Key value pairs are two
expressions seperated by a `=>` operator.  In the case where the key is an
atom the shortend form `key: <value>` may be used which will associate the
atom `:key` with `<value>`.

Meta Constructs
---------------

Instructions which change the way the program is exicuted without changing the
semantics are `Meta` constructs.  Such constructions my alter when and how a
function is compiled to machine instructions, influence specializaion of
functions, effect caching, and other exicution deatails.  Meta constructs also
form documentation.

Meta Constructions are formed by placing the `#` as the first charactor in the
line.  (Note: comments, but no other type of meta constructor, can start
anywhere a space is valid and will run to the end of the physical line).

### Comments ###


Comments for human readable code documenation.  Comments in some locations are
assumed to describe formally certain types of expressions and should be
written so as to be parsed by the code documentation program (see below)

Comments begin with a `#` followed by a space (or any other unrecognised
character, but a space is encouraged to avoide conflict with future meta
constructs.) A comment runs for the entire *logical* line including any
indented block.  Comments are ignored by the parser, but *are* part of the
line and block structure of the program.

**Warning**: A comment can end a block.

A Sapphire Program
------------------



Expressions
-----------



[1]: https://en.wikipedia.org/wiki/Off-side_rule "Off-Side Rule"
