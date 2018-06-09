%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% regex_demo.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Copyright (C) 2002 The University of Melbourne
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
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

:- import_module lex.
:- import_module regex.

:- import_module exception.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    S = "([Ff][Oo][Oo])+",
    M = change_all(regex(S), func(_) = "bar"),
    io.format("Replacing multiple \"foo\"s with a single \"bar\"...",
        [], !IO),
    loop(M, !IO).

%-----------------------------------------------------------------------------%

:- pred loop(func(string) = string, io, io).
:- mode loop(func(in) = out is det, di, uo) is det.

loop(M, !IO) :-
    io.format("\n> ", [], !IO),
    io.read_line_as_string(Res, !IO),
    (
        Res = eof
    ;
        Res = error(_),
        throw(Res)
    ;
        Res = ok(S),
        io.format("  %s", [s(M(S))], !IO),
        loop(M, !IO)
    ).

%-----------------------------------------------------------------------------%
:- end_module regex_demo.
%-----------------------------------------------------------------------------%
