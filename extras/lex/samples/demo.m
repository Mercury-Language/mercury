%----------------------------------------------------------------------------- %
% demo.m
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Sun Aug 20 18:11:42 BST 2000
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%
%   THIS FILE IS HEREBY CONTRIBUTED TO THE MERCURY PROJECT TO
%   BE RELEASED UNDER WHATEVER LICENCE IS DEEMED APPROPRIATE
%   BY THE ADMINISTRATORS OF THE MERCURY PROJECT.
%
%----------------------------------------------------------------------------- %

:- module demo.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%----------------------------------------------------------------------------- %
%----------------------------------------------------------------------------- %

:- implementation.

:- import_module char, string, exception, array, list.
:- import_module lex.

%----------------------------------------------------------------------------- %

main -->

    io__print("\
I recognise Mercury style comments, integers and reals, punctuation,
the nouns `cat', `dog', `rat' and `mat', the verbs `sat', `caught' and
`chased', conjunctions `and' and `then', and the prepositions and
determiners `the', `it', `them', `to' and `on'.

Try me...

"),

    { Lexer  = lex__init(lexemes, lex__read_from_stdin) },
    call((pred(IO0::di, IO::uo) is det :-
            State0 = lex__start(Lexer, IO0),
            tokenise_stdin(State0, State),
            IO     = lex__stop(State)
    )).

%----------------------------------------------------------------------------- %

:- pred tokenise_stdin(lexer_state(token, io__state),
                lexer_state(token, io__state)).
:- mode tokenise_stdin(di, uo) is det.

tokenise_stdin -->
    lex__read(Result),
    lex__manipulate_source(io__print(Result)),
    lex__manipulate_source(io__nl),
    ( if { Result \= eof } then
        tokenise_stdin
      else
        []
    ).

%----------------------------------------------------------------------------- %

:- type token
    --->    noun
    ;       comment
    ;       integer
    ;       real
    ;       verb
    ;       conj
    ;       prep
    ;       punc
    .

:- func lexemes = list(annotated_lexeme(token)).

lexemes = [
    lexeme(value(comment),
			(atom('%') >> junk)),

    lexeme(value(integer),
			(signed_int)),

    lexeme(value(real),
            (real)),

    lexeme(value(noun), str("cat")),
    lexeme(value(noun), str("dog")),
    lexeme(value(noun), str("rat")),
    lexeme(value(noun), str("mat")),

    lexeme(value(verb),
			(str("sat") \/ str("caught") \/ str("chased"))),

    lexeme(value(conj),
			(str("and") \/ str("then"))),

    lexeme(value(prep),
			(str("the") \/ str("it") \/ str("them") \/ str("to") \/ str("on"))),

    lexeme(noval(punc),
			(any("~!@#$%^&*()_+`-={}|[]\\:"";'<>?,./"))),

    lexeme(ignore,
			whitespace)
].

%----------------------------------------------------------------------------- %
%----------------------------------------------------------------------------- %
