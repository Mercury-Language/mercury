%----------------------------------------------------------------------------- %
% demo.m
% Sun Aug 20 18:11:42 BST 2000
%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
%   THIS FILE IS HEREBY CONTRIBUTED TO THE MERCURY PROJECT TO
%   BE RELEASED UNDER WHATEVER LICENCE IS DEEMED APPROPRIATE
%   BY THE ADMINISTRATORS OF THE MERCURY PROJECT.
% Thu Jul 26 07:45:47 UTC 2001
% Copyright (C) 2001 The Rationalizer Intelligent Software AG
%   The changes made by Rationalizer are contributed under the terms 
%   of the GNU General Public License - see the file COPYING in the
%   Mercury Distribution.
%
%----------------------------------------------------------------------------- %

:- module demo.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%----------------------------------------------------------------------------- %
%----------------------------------------------------------------------------- %

:- implementation.

:- import_module string, int, float, exception, list.
:- import_module lex.

%----------------------------------------------------------------------------- %

main(IO0, IO) :-

    io__print("\

I recognise the following words:
""cat"", ""dog"", ""rat"", ""mat"", ""sat"", ""caught"", ""chased"",
""and"", ""then"", ""the"", ""it"", ""them"", ""to"", ""on"".
I also recognise Mercury-style comments, integers and floating point
numbers, and a variety of punctuation symbols.

Try me...

", IO0, IO1),

    Lexer  = lex__init(lexemes, lex__read_from_stdin, ignore(space)),
    State0 = lex__start(Lexer, IO1),
    tokenise_stdin(State0, State),
    IO     = lex__stop(State).

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
    --->    noun(string)
    ;       comment(string)
    ;       integer(int)
    ;       real(float)
    ;       verb(string)
    ;       conj(string)
    ;       prep(string)
    ;       punc
    ;       space
    .

:- func lexemes = list(lexeme(token)).

lexemes = [

    ( "%" ++ junk      -> (func(Match) = comment(Match)) ),
    ( signed_int       -> (func(Match) = integer(string__det_to_int(Match))) ),
    ( real             -> (func(Match) = real(det_string_to_float(Match))) ),

        % Multiple regexps can match the same token constructor.
        %
    ( "cat"            -> (func(Match) = noun(Match)) ),
    ( "dog"            -> (func(Match) = noun(Match)) ),
    ( "rat"            -> (func(Match) = noun(Match)) ),
    ( "mat"            -> (func(Match) = noun(Match)) ),

        % Here we use `or', rather than multiple lexemes.
        %
    ( "sat" or
      "caught" or
      "chased"         -> (func(Match) = verb(Match)) ),

    ( "and" or
      "then"           -> (func(Match) = conj(Match)) ),

        % `\/' is a synonym for `or'.  Tell us which you prefer...
        % 
    ( "the" \/
      "it" \/
      "them" \/
      "to" \/
      "on"             -> (func(Match) = prep(Match)) ),

        % return/1 can be used when you don't care what string was matched.
        %
    ( any("~!@#$%^&*()_+`-={}|[]\\:"";'<>?,./")
                       -> return(punc) ),
    ( whitespace       -> return(space) )
].



:- func det_string_to_float(string) = float.

det_string_to_float(String) =
    ( if   string__to_float(String, Float)
      then Float
      else throw("error in float conversion")
    ).

%----------------------------------------------------------------------------- %
%----------------------------------------------------------------------------- %
