%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% regex_demo.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Copyright (C) 2002 The University of Melbourne
%
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%
% Sun Nov 24 11:44:45 EST 2002
%
%-----------------------------------------------------------------------------%

:- module regex_demo.

:- interface.

:- import_module io.



:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string, list, exception.
:- import_module lex, regex.

%-----------------------------------------------------------------------------%

main(!IO) :-
    S = "([Ff][Oo][Oo])+",
    M = change_all(regex(S), func(_) = "bar"),
    io__format("Replacing multiple \"foo\"s with a single \"bar\"...",
        [], !IO),
    loop(M, !IO).

%-----------------------------------------------------------------------------%

:- pred loop(func(string) = string, io, io).
:- mode loop(func(in) = out is det, di, uo) is det.

loop(M, !IO) :-
    io__format("\n> ", [], !IO),
    io__read_line_as_string(Res, !IO),
    (
        Res = eof
    ;
        Res = error(_),
        throw(Res)
    ;
        Res = ok(S),
        io__format("  %s", [s(M(S))], !IO),
        loop(M, !IO)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
