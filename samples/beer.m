%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% file: beer.m
% author:
%   Fergus Henderson <fjh@cs.mu.oz.au> Thursday 9th November 1995
% Re-written with new syntax standard library calls:
%   Paul Bone <paul@mercurylang.org> 2015-11-20
%
% This beer song is more idiomatic Mercury than the original, I feel bad
% saying that since Fergus is a founder of the language.

:- module beer.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    beer(99, !IO).

:- pred beer(int::in, io::di, io::uo) is det.

beer(N, !IO) :-
    io.write_string(beer_stanza(N), !IO),
    ( if N > 0 then
        io.nl(!IO),
        beer(N - 1, !IO)
    else
        true
    ).

:- func beer_stanza(int) = string.

beer_stanza(N) = Stanza :-
    ( if N = 0 then
        Stanza = "Go to the store and buy some more!\n"
    else
        NBottles = bottles_line(N),
        N1Bottles = bottles_line(N - 1),
        Stanza =
            NBottles ++ " on the wall.\n" ++
            NBottles ++ ".\n" ++
            "Take one down, pass it around,\n" ++
            N1Bottles ++ " on the wall.\n"
    ).

:- func bottles_line(int) = string.

bottles_line(N) =
    ( if N = 0 then
        "No more bottles of beer"
    else if N = 1 then
        "1 bottle of beer"
    else
        string.format("%d bottles of beer", [i(N)])
    ).

