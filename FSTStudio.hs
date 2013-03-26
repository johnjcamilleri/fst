{-# LANGUAGE DoAndIfThenElse, FlexibleContexts, GeneralizedNewtypeDeriving #-}

{- |
fstStudio takes a program consisting of regular relations that denotes
the relation between two regular languages and constructs a
transducer. If a regular expression, not a relation, is given, then it
is interpreted as the identity relation. The syntax is very similar to
Xerox's finite state transducer syntax with two fundamental
differences: a distinction is made between functions (definitions) and
strings, and fststudio allows functional definitions.

[@\"a\"@] A symbol. Example: @[\"b\"]@ denotes the language @{\"b\"}@.

[@a@] A variable. A symbol without quotes is a variable.

[@\"a\":\"b\"@] Describes a relation between the symbol @a@ and @b@.
This relation is ordered and @a@ is said to be a part of the /upper
language/ and @b@ is said to be part of the /lower language/.
Example: @[\"a\":\"b\"]@ denotes the relation @{(\"a\",\"b\")}@.

[@0@] Epsilon symbol. The epsilon symbol denotes the string with no
symbols.  Example: @[0]@ denotes the language @{\"\"}@.

[@?@] All symbol. The all symbol denotes the union of all symbols in
the alphabet. Example: @[?]@ and an alphabet @{a,b,c}@ denotes the
language @{\"a\",\"b\",\"c\"}@.

[@\"\"@] quotes cancel every special meaning of the symbols. Example:
@[\"? 0\"]@ denotes the language @{\"? 0\"}@.

[@\[A\]@] brackets are used to change the precedence of a regular
relation.

[@(A)@] parenthesis expresses optionality, and has the same meaning as
@[A|0]@.

[@A B@] Concatenation of the expressions or relations A and
B. Example: @[[a b] [c d]]@ denotes the language @{\"ac\", \"ad\", \"bc\",
\"bd\"}@

[@A^n@] Concatenation of @A@ /n/ times.  @A^0@ is defined as the empty
string. Example: @[a]^3@ describes the language @{\"aaa\"}@.

[@A|B@] Union of the languages or relations @A@ and @B@. Example: @[a|b]@
describes the language @{\"a\",\"b\"}@.

[@A & B@] Intersection of the languages @A@ and @B@.  Example: @[a b]
& [a]@ describes the language @{\"a\"}@.

[@A - B@] Minus of the languages @A@ and @B@, and has the same meaning as
@[A & B]@.  Example: @[a b] - [a]@ describes the language @{\"b\"}@.

[@~A@] Describes the complement of an expression, and has the same
meaning as @[?* - A]@.  Note that complement is always defined over
an alphabet. The expression @[A]@ is only unambiguous with respect to
an alphabet. Example: @[a]@ denotes the language that doesn't contain
the string @\"a\"@. If the alphabet is @{\"a\",\"b\"}@ then @[a]@
denotes the language @{\"\",\"b\",\"aa\",\"ba\",...}@.

[@A+@] Repetition (Kleenes plus).  A concatenated with itself an
arbitrary number of times, including zero times. Example: @[a]+@ denotes
the infinite language @{\"a\",\"aa\",\"aaa\",...}@

[@A*@] Kleene’s star: @[A+ | 0]@.  Example: @[a]*@ denotes the infinite
language @{\"\",\"a\",\"aa\",...}@

[@$A@] Containment.  The set of strings where @A@ appear at least once
as a substring. Containment is the same thing as @[?* A ?*]@.

[@A .x. B@] Cross product of the languages @A@ and @B@.  Example: @[[a b]
.x. c]@ describes the relations @{(\"a\",\"c\"), (\"b\",\"c\")}@.

[@A .o. B@] Composition of the relations @A@ and @B@.  Example: @[a:b c:d]
.o. [d:e]@ describes the relation @{(\"c\",\"e\")}@.

The precedence of the operators is as follows, where 4 is the highest
precedence:

  1. @.x.@ @.o.@

  2. @&@ @-@

  3. /Concatenation/

  4. @~@ @^@ @*@ @+@ @$@

A file containing a program must end with @.fst@, and an input file
mustend with @.dat@.  A program is a collection of functions defining
regular relations. A function with zero arguments is called a
definition or a macro.  A definition, or a macro, can for example look
like this:

> <digits> ::= "1" | "2" | "3" | "4" | "5" |
>              "6" | "7" | "8" | "9" | "0" ;

and a function can look like this:

> <swap,a,b> ::= b a ;

Note that strings are marked with quotes, and variables have no
quotes. Every program must contain a @\<main\>@ definition (a program
without one will result in a parse error).

> <main> ::= ... ;

The alphabet of a program is the symbols in the regular relation
defined in the program.

/Example program/

> <nickel>  ::= ["n" .x. "c"^5];
> <dime>    ::= ["d" .x. "c"^10];
> <quarter> ::= ["q" .x. "c"^25];
> <cent>    ::= ["c" .x. "c"];
> <money>   ::= [ <nickel> | <dime> | <quarter> | <cent>]*;
> <drink>   ::= ["c"^65 .x. "PLONK"];
> <main>    ::= [ <money> .o. <drink> ];

/Batch mode/

Usage: @fst FILE [Options]@.  FILE must end with @.fst@, which defines
an FstStudio program, or @.net@, which defines a saved transducer. If
no options are given, then input is taken from standard input, the
transducer is applied down, and the output, if any, is produced on
standard output.

[@-u@] Apply the transducer up

[@-d@] Apply the transducer down

[@-i FILE@] Take input from FILE

[@-o FILE@] Write output to FILE

/Interactive mode - list of commands/

[@r REG@] Read a regular relation from standard input. If a regular
expression is typed, then it is interpreted as the identity relation.

[@b@] Build an epsilon-free, deterministic, minimal transducer from a
loaded/typed regular relation.

[@bn@] Build an epsilon-free, possibly non-deterministic, non-minimal
transducer from a load/typed regular relation.

[@m@] Minimize a built transducer.

[@det@] Determinize a built transducer.

[@s FILE@] Save to @FILE@. If @FILE@ ends with @.net@, then the built
transducer is saved. Any other suffix saves the produced output in the
system to @FILE@, if any.

[@l FILE@] Load from @FILE@. @FILE@ must end with @.fst@, @.net@ or
@.dat@. If @FILE@ ends with @.fst@, then a FstStudio program is loaded
into FstStudio. If @FILE@ ends with @.net@, then a transducer is loaded
into FstStudio. If @FILE@ ends with @.dat@, then input is loaded into
FstStudio.

[@l a | b@] Load and union two transducers. a and b must either be a
file ending with @.net@ or the symbol @*@, which refers to the interior
transducer. The produced transducer is possibly non-deterministic and
non-minimal.

[@l a b@] Load and concatenate two transducers. a and b must either be
ale ending with @.net@ or the symbol @*@, which refers to the interior
transducer. The produced transducer is possibly non-deterministicand
non-minimal.

[@l a*@] Load and apply Kleene’s star on a transducer. a must either
be a file ending with @.net@ or the symbol @*@, which refers to the
interior transducer. The produced transducer is possibly
non-deterministicand non-minimal.

[@l a .o. b@] Load and compose two transducers. a and b must either be
a file ending with @.net@ or the symbol @*@, which refers to the
interior transducer. The produced transducer is possibly
non-deterministic andnon-minimal.

[@vt@] View loaded/built transducer.

[@vr@] View loaded/typed regular relation.

[@vi@] View loaded input.

[@vo@] View produced output.

[@d@] Apply transducer down with loaded input.

[@u@] Apply transducer up with loaded input.

[@d SYMBOLS@] Apply tranducer down with @SYMBOLS@.

[@u SYMBOLS@] Apply transducer up with @SYMBOLS@.

[@c@] Clear memory.

[@h@] List commands.

[@q@] End session.

-}
module FSTStudio where

-- This is a dummy module for the sake of documenation

