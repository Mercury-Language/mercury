% vim: ts=4 sw=4 et ff=unix

Copyright (C) 2002 The University of Melbourne

THE LEX MODULE

The lex module provides tools for writing lexical analyzers.
A lexical analyzer parses a stream of chars (e.g. from a string or the
standard input stream) against a list of regular expressions,
returning the first, longest match along with an indication of which
regular expression was matched.

QUICK START GUIDE

A lexer is compiled from a list of lexemes and a predicate that will
read the next char from the input stream.

A lexeme is a pair consisting of a regular expression and a function
that will convert a string matched by the regular expression into a
token, which may be returned as a result by the lexical analyzer
(hereafter referred to as a `lexer'.)

The lex module provides a language for composing regular expressions
including literal strings, alternation, Kleene closure, grouping and
various other useful combinators, as well as a rich set of pre-defined
regular expressions such as identifier, signed_int, real and so forth.
(Also, consider the regexp/1 function defined in the regex module,
which supports the construction of regular expressions from strings
similar to those recognised by tools such as grep and sed.)

A lexer may be created as in the following example (this lexer works
over the standard input stream):

:- type token
    --->    id(string)
    ;       int(int)
    ;       float(float)
    ;       lpar
    ;       rpar
    ;       comment.

Lexer = lex.init([
    ( identifier  ->  func(Id)    = id(Id)),
    ( signed_int  ->  func(Int)   = int(Int)),
    ( real        ->  func(Float) = float(Float)),
    ( "("         ->  return(lpar)),
    ( ")"         ->  return(rpar)),
    ( "%" ++ junk ->  return(comment))
    ], read_from_stdin).

The combinator return/2 is defined s.t. return(X) = (func(_) = X),
that is, it simply discards the matched string and returns X.

(There is also lex.init/3 which takes an extra argument, namely a predicate
which is used to silently ignore certain tokens such as whitespace, say.)

A lexer is activated by calling lex.start/2, which returns a (unique)
lexer state:

    !:LexerState = lex.start(Lexer, !.IO)

The lex.read/3 predicate searches for the next, longest match in the
input stream and returns the corresponding token (or an error message
if there is no immediate match in the input stream):

    lex.read(Result, !LexerState),
    (
        Result = eof,
        ...
    ;
        Result = ok(Token),
        ...
    ;
        Result = error(Message, Offset),
        ...
    )

When lexical analysis is complete, the input source may be obtained 
by calling lex.stop/1:

    !:IO = lex.stop(!.LexerState)
