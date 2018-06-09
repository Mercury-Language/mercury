%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% Copyright (C) 2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: diff.m.
%
% This module computes diffs between terms.
%
%---------------------------------------------------------------------------%

:- module mdb.diff.
:- interface.

:- import_module io.
:- import_module univ.

:- pred report_diffs(int::in, int::in, univ::in, univ::in, io::di, io::uo)
    is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.

:- import_module deconstruct.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- pragma foreign_export("C", report_diffs(in, in, in, in, di, uo),
    "ML_report_diffs").

report_diffs(Drop, Max, Univ1, Univ2, !IO) :-
    ( if
        Type1 = univ_type(Univ1),
        Type2 = univ_type(Univ2),
        Type1 = Type2
    then
        compute_diffs(Univ1, Univ2, [], [], RevDiffs),
        list.reverse(RevDiffs, AllDiffs),
        list.length(AllDiffs, NumAllDiffs),
        ( if
            list.drop(Drop, AllDiffs, Diffs),
            Diffs = [_ | _]
        then
            FirstShown = Drop + 1,
            LastShown = min(Drop + Max, NumAllDiffs),
            ( if FirstShown = LastShown then
                io.format("There are %d diffs, showing diff %d:\n",
                    [i(NumAllDiffs), i(FirstShown)], !IO)
            else
                io.format("There are %d diffs, showing diffs %d-%d:\n",
                    [i(NumAllDiffs), i(FirstShown), i(LastShown)], !IO)
            ),
            list.take_upto(Max, Diffs, ShowDiffs),
            list.foldl2(show_diff, ShowDiffs, Drop, _, !IO)
        else
            ( if NumAllDiffs = 0 then
                io.write_string("There are no diffs.\n", !IO)
            else if NumAllDiffs = 1 then
                io.write_string("There is only one diff.\n", !IO)
            else
                io.format("There are only %d diffs.\n", [i(NumAllDiffs)], !IO)
            )
        )
    else
        io.write_string("The two values are of different types.\n", !IO)
    ).

:- type term_path_diff
    --->    term_path_diff(term_path, univ, univ).

:- pred compute_diffs(univ::in, univ::in, term_path::in,
    list(term_path_diff)::in, list(term_path_diff)::out) is cc_multi.

compute_diffs(Univ1, Univ2, !.RevPath, !RevDiffs) :-
    deconstruct(univ_value(Univ1), include_details_cc, Functor1, _, Args1),
    deconstruct(univ_value(Univ2), include_details_cc, Functor2, _, Args2),
    ( if Functor1 = Functor2 then
        compute_arg_diffs(Args1, Args2, !.RevPath, 1, !RevDiffs)
    else
        list.reverse(!.RevPath, Path),
        !:RevDiffs = [term_path_diff(Path, Univ1, Univ2) | !.RevDiffs]
    ).

:- pred compute_arg_diffs(list(univ)::in, list(univ)::in, term_path::in,
    int::in, list(term_path_diff)::in, list(term_path_diff)::out) is cc_multi.

compute_arg_diffs([], [], _, _, !RevDiffs).
compute_arg_diffs([], [_ | _], _, _, !RevDiffs) :-
    error("compute_arg_diffs: argument list mismatch").
compute_arg_diffs([_ | _], [], _, _, !RevDiffs) :-
    error("compute_arg_diffs: argument list mismatch").
compute_arg_diffs([Arg1 | Args1], [Arg2 | Args2], !.RevPath, ArgNum,
        !RevDiffs) :-
    compute_diffs(Arg1, Arg2, [ArgNum | !.RevPath], !RevDiffs),
    compute_arg_diffs(Args1, Args2, !.RevPath, ArgNum + 1, !RevDiffs).

:- pred show_diff(term_path_diff::in, int::in, int::out, io::di, io::uo)
    is cc_multi.

show_diff(Diff, !DiffNum, !IO) :-
    !:DiffNum = !.DiffNum + 1,
    io.format("%d: ", [i(!.DiffNum)], !IO),
    Diff = term_path_diff(Path, Univ1, Univ2),
    (
        Path = [],
        io.write_string("mismatch at root", !IO)
    ;
        Path = [Posn | Posns],
        io.write_int(Posn, !IO),
        show_path_rest(Posns, !IO)
    ),
    io.write_string(": ", !IO),
    functor(univ_value(Univ1), include_details_cc, Functor1, Arity1),
    functor(univ_value(Univ2), include_details_cc, Functor2, Arity2),
    io.format("%s/%d vs %s/%d\n",
        [s(Functor1), i(Arity1), s(Functor2), i(Arity2)], !IO).

:- pred show_path_rest(list(int)::in, io::di, io::uo) is det.

show_path_rest([], !IO).
show_path_rest([Posn | Posns], !IO) :-
    io.write_string("/", !IO),
    io.write_int(Posn, !IO),
    show_path_rest(Posns, !IO).
