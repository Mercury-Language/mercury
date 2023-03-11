%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% test_regex.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Copyright (C) 2002 The University of Melbourne
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%
% Thu Nov 21 15:33:48 EST 2002
%
%-----------------------------------------------------------------------------%

:- module test_regex.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module lex.
:- import_module regex.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type op
    --->    set_regex(string)
    ;       try_match(string).

%-----------------------------------------------------------------------------%

main(!IO) :-
    loop(regex("<don't go here>"), !IO).

:- pred loop(regex, io, io).
:- mode loop(in(regex), di, uo) is det.

loop(R, !IO) :-
    io.read(Res, !IO),
    (
        Res = eof
    ;
        Res = error(_, _),
        throw(Res)
    ;
        Res = ok(Op),
        (
            Op = set_regex(S),
            io.format("\n\n* Matching against \"%s\"\n", [s(S)], !IO),
            loop(regex(S), !IO)
        ;
            Op = try_match(S),
            io.format("\n> \"%s\"\n", [s(S)], !IO),
            M = matches(R, S),

            io.format("all matches             : ", [], !IO),
            io.print_line(matches(R, S), !IO),

            (
                M =  [_ | _],

                io.format("replace_first with `<>' : \"%s\"\n",
                    [s(replace_first(R, "<>", S))], !IO),

                io.format("replace_all with `<>'   : \"%s\"\n",
                    [s(replace_all(R, "<>", S))], !IO),

                ChgFn = (func(Str) = append_list(["<", Str, ">"])),

                io.format("change_first to `<&>'   : \"%s\"\n",
                    [s(change_first(R, ChgFn, S))], !IO),

                io.format("change_all to `<&>'     : \"%s\"\n",
                    [s(change_all(R, ChgFn, S))], !IO)
            ;
                M = []
            ),
            ( if exact_match(R, S) then
                io.format("exact_match\n", [], !IO)
            else
                true
            ),
            ( if left_match(R, S, LSub, LS, LC) then
                io.format("left_match              : {\"%s\", %d, %d}\n",
                        [s(LSub), i(LS), i(LC)], !IO)
            else
                true
            ),
            ( if right_match(R, S, RSub, RS, RC) then
                io.format("right_match             : {\"%s\", %d, %d}\n",
                        [s(RSub), i(RS), i(RC)], !IO)
            else
                true
            ),
            ( if first_match(R, S, FSub, FS, FC) then
                io.format("first_match             : {\"%s\", %d, %d}\n",
                        [s(FSub), i(FS), i(FC)], !IO)
            else
                true
            ),
            loop(R, !IO)
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
