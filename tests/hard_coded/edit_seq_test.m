%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This module tests the diff module.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module edit_seq_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module edit_seq.

:- import_module assoc_list.
:- import_module list.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module string.

main(!IO) :-
    Params1 = edit_params(1, 1, 1), % replace cost same as insert OR delete
    Params2 = edit_params(1, 1, 2), % replace cost same as insert PLUS delete
    ParamsList = ["replace" - Params1, "delete/insert" - Params2],
    Lists = [
        [],
        [3],
        [42],
        [3, 4, 5, 12, 13],
        [1, 2, 3, 4, 11, 12, 13, 14],
        [1, 19, 2, 29, 3, 38, 39, 4, 12, 11, 13, 14]
    ],
    test_list_list(Lists, Lists, ParamsList, !IO).

    % test_list_list(SeqsA, SeqsB, ParamsList, !IO):
    %
    % Test every sequence in SeqsA against every sequence in SeqsB,
    % using every parameter set in ParamsList.
    %
:- pred test_list_list(list(list(int))::in, list(list(int))::in,
    assoc_list(string, edit_params)::in, io::di, io::uo) is det.

test_list_list([], _SeqsB, _ParamsList, !IO).
test_list_list([SeqA | SeqsA], SeqsB, ParamsList, !IO) :-
    test_list(SeqA, SeqsB, ParamsList, !IO),
    test_list_list(SeqsA, SeqsB, ParamsList, !IO).

    % test_list(SeqA, SeqsB, ParamsList, !IO):
    %
    % Test SeqA against every sequence in SeqsB
    % using every parameter set in ParamsList.
    %
:- pred test_list(list(int)::in, list(list(int))::in,
    assoc_list(string, edit_params)::in, io::di, io::uo) is det.

test_list(_SeqA, [], _ParamsList, !IO).
test_list(SeqA, [SeqB | SeqsB], ParamsList, !IO) :-
    test_params(SeqA, SeqB, ParamsList, !IO),
    test_list(SeqA, SeqsB, ParamsList, !IO).

    % test(SeqA, SeqB, ParamsList, !IO):
    %
    % Test SeqA against SeqB
    % using every parameter set in ParamsList.
    %
:- pred test_params(list(int)::in, list(int)::in,
    assoc_list(string, edit_params)::in, io::di, io::uo) is det.

test_params(_SeqA, _SeqB, [], !IO).
test_params(SeqA, SeqB, [Params | ParamsList], !IO) :-
    test(SeqA, SeqB, Params, !IO),
    test_params(SeqA, SeqB, ParamsList, !IO).

    % test(SeqA, SeqB, Params, !IO):
    %
    % Test SeqA against SeqB.
    %
:- pred test(list(int)::in, list(int)::in, pair(string, edit_params)::in,
    io::di, io::uo) is det.

test(SeqA, SeqB, ParamsName - Params, !IO) :-
    io.write_string("\n------------------\n\n", !IO),
    io.write_string("SeqA:   ", !IO),
    io.write_line(SeqA, !IO),
    io.write_string("SeqB:   ", !IO),
    io.write_line(SeqB, !IO),
    io.write_string("Params: ", !IO),
    io.write_string(ParamsName, !IO),
    io.nl(!IO),
    find_shortest_edit_seq(Params, SeqA, SeqB, Edits),
    io.write_string("Edits:  ", !IO),
    io.write_line(Edits, !IO),
    find_diff_seq(SeqA, Edits, DiffSeq),
    io.write_string("Diff:\n", !IO),
    write_diff_seq(DiffSeq, !IO).

:- pred write_diff_seq(diff_seq(int)::in, io::di, io::uo) is det.

write_diff_seq([], !IO).
write_diff_seq([Diff | Diffs], !IO) :-
    (
        Diff = unchanged(N),
        UDI = " "
    ;
        Diff = deleted(N),
        UDI = "-"
    ;
        Diff = inserted(N),
        UDI = "+"
    ),
    io.format("%s %d\n", [s(UDI), i(N)], !IO),
    write_diff_seq(Diffs, !IO).

%---------------------------------------------------------------------------%
:- end_module edit_seq_test.
%---------------------------------------------------------------------------%
