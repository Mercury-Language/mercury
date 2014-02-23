%-----------------------------------------------------------------------------%
% lex_demo.m
% Sun Aug 20 18:11:42 BST 2000
%
% vim: ts=4 sw=4 et tw=0 wm=0 ft=mercury
%
% Copyright (C) 2001-2002 The University of Melbourne
% Copyright (C) 2001 The Rationalizer Intelligent Software AG
%   The changes made by Rationalizer are contributed under the terms 
%   of the GNU General Public License - see the file COPYING in the
%   Mercury Distribution.
%
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%
%-----------------------------------------------------------------------------%

:- module lex_demo.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module lex.

:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-

    io.print("\

I recognise the following words:
""cat"", ""dog"", ""rat"", ""mat"", ""sat"", ""caught"", ""chased"",
""and"", ""then"", ""the"", ""it"", ""them"", ""to"", ""on"".
I also recognise Unicode characters:
""我"", ""会"", ""说"", ""中文""
I also recognise Mercury-style comments, integers and floating point
numbers, and a variety of punctuation symbols.

Try me...

", !IO),

    Lexer  = lex.init(lexemes, lex.read_from_stdin, ignore(space)),
    State0 = lex.start(Lexer, !.IO),
    tokenise_stdin(State0, State),
    !:IO = lex.stop(State).

%-----------------------------------------------------------------------------%

:- pred tokenise_stdin(lexer_state(token, io)::di, lexer_state(token, io)::uo)
    is det.

tokenise_stdin(!LS) :-
    lex.read(Result, !LS),
    lex.manipulate_source(io.print(Result), !LS),
    lex.manipulate_source(io.nl, !LS),
    (
        Result = ok(_),
        tokenise_stdin(!LS)
    ;
        ( Result = eof
        ; Result = error(_, _)
        )
    ).

%-----------------------------------------------------------------------------%

:- type token
    --->    noun(string)
    ;       comment(string)
    ;       integer(int)
    ;       real(float)
    ;       verb(string)
    ;       conj(string)
    ;       prep(string)
    ;       adverb(string)
    ;       punc
    ;       space
    ;       word(string)
    ;       unrecognised(string).

:- func lexemes = list(lexeme(token)).

lexemes = [

    ( "%" ++ junk       -> (func(Match) = comment(Match)) ),
    ( signed_int        -> (func(Match) = integer(string.det_to_int(Match))) ),
    ( real              -> (func(Match) = real(string.det_to_float(Match))) ),

        % Multiple regexps can match the same token constructor.
        %
    ( "cat"             -> (func(Match) = noun(Match)) ),
    ( "dog"             -> (func(Match) = noun(Match)) ),
    ( "rat"             -> (func(Match) = noun(Match)) ),
    ( "mat"             -> (func(Match) = noun(Match)) ),

        % Here we use `or', rather than multiple lexemes.
        %
    ( "sat" or
      "caught" or
      "chased"          -> (func(Match) = verb(Match)) ),

    ( "and" or
      "then"            -> (func(Match) = conj(Match)) ),

        % `\/' is a synonym for `or'.  Tell us which you prefer...
        % 
    ( "the" \/
      "it" \/
      "them" \/
      "to" \/
      "on"              -> (func(Match) = prep(Match)) ),

    ( "我" or "中文"    -> func(Match) = noun(Match) ),
    ( "会"              -> func(Match) = adverb(Match) ),
    ( "说"              -> func(Match) = verb(Match) ),

        % return/1 can be used when you don't care what string was matched.
        %
    ( any("~!@#$%^&*()_+`-={}|[]\\:"";'<>?,./")
                        -> return(punc) ),
    ( whitespace        -> return(space) ),
    ( +(range('a', 'z') or
        range('A', 'Z')
       )                -> func(Match) = word(Match) ),
    ( dot               -> func(Match) = unrecognised(Match) )
].

%-----------------------------------------------------------------------------%
:- end_module lex_demo.
%-----------------------------------------------------------------------------%
