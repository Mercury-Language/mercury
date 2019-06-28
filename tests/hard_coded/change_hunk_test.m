%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This tests the change hunk functionality of the edit_seq library module.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module change_hunk_test.
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
    % Params1 = edit_params(1, 1, 1), % replace cost same as insert OR delete
    % Params2 = edit_params(1, 1, 2), % replace cost same as insert PLUS delete
    % ParamsList = ["replace" - Params1, "delete/insert" - Params2],
    Params1 = edit_params(1, 1, 1), % replace cost same as insert OR delete
    ParamsList = ["replace" - Params1],
    ListA = [
        "Washington",
        "Adams",
        "Jefferson",
        "Madison",
        "Monroe",
        "Quincy Adams",
        "Jackson",
        "Van Buren",
        "Harrison",
        "Tyler",
        "Polk",
        "Taylor",
        "Fillmore",
        "Pierce",
        "Buchanan",
        "Lincoln",
        "Johnson",
        "Grant",
        "Hayes",
        "Garfield",
        "Arthur",
        "Cleveland",
        "Harrison",
        "McKinley",
        "Roosevelt",
        "Taft",
        "Wilson",
        "Harding",
        "Coolidge",
        "Hoover",
        "Roosevelt",
        "Truman",
        "Eisenhower",
        "Kennedy",
        "Johnson",
        "Nixon",
        "Ford",
        "Carter",
        "Reagan",
        "Bush",
        "Clinton",
        "Bush",
        "Obama",
        "Trump"
    ],
    ListB = [
        "Washington",
        "Adams",
        "Jefferson",
        "Madison",
        "Monroe",
        "Quincy Adams",
        "Jackson",
        "Van Buren",
        "Harrison",
        "Tyler",
        "Polk",
        "Taylor",
        "Fillmore",
        "Pierce",
        "Buchanan",
        "Lincoln",
        "Grant",
        "Sherman",
        "Hayes",
        "Garfield",
        "Arthur",
        "Cleveland",
        "Harrison",
        "McKinley",
        "Roosevelt",
        "Taft",
        "Wilson",
        "Harding",
        "Coolidge",
        "Hoover",
        "Roosevelt",
        "Truman",
        "Eisenhower",
        "Nixon",
        "Kennedy",
        "Johnson",
        "Ford",
        "Carter",
        "Reagan",
        "Bush",
        "Clinton",
        "Bush",
        "Obama",
        "Clinton"
    ],
    ListC = [
        "George III",
        "Adams",
        "Jefferson",
        "Madison",
        "Monroe",
        "Quincy Adams",
        "Jackson",
        "Van Buren",
        "Harrison",
        "Tyler",
        "Polk",
        "Taylor",
        "Fillmore",
        "Pierce",
        "Buchanan",
        "Douglas",
        "Johnson",
        "Grant",
        "Hayes",
        "Garfield",
        "Arthur",
        "Cleveland",
        "Harrison",
        "McKinley",
        "Roosevelt",
        "Taft",
        "Wilson",
        "Pershing",
        "Harding",
        "Coolidge",
        "Hoover",
        "Roosevelt",
        "Truman",
        "Eisenhower",
        "Kennedy",
        "Johnson",
        "Nixon",
        "Ford",
        "Carter",
        "Mondale",
        "Perot",
        "Clinton",
        "Bush",
        "Obama"
    ],
    ListD = [
        "Washington",
        "Monroe",
        "Quincy Adams",
        "Jackson",
        "Van Buren",
        "Harrison",
        "Tyler",
        "Polk",
        "Taylor",
        "Fillmore",
        "Pierce",
        "Buchanan",
        "Lincoln",
        "Johnson",
        "Grant",
        "Sherman",
        "Sheridan",
        "Hayes",
        "Garfield",
        "Arthur",
        "Cleveland",
        "Harrison",
        "McKinley",
        "Roosevelt",
        "Taft",
        "Wilson",
        "Harding",
        "Coolidge",
        "Hoover",
        "Roosevelt",
        "Garner",
        "Wallace",
        "Truman",
        "Eisenhower",
        "Kennedy",
        "Johnson",
        "Nixon",
        "Ford",
        "Carter",
        "Reagan",
        "Bush",
        "Clinton",
        "Bush",
        "Obama",
        "Trump"
    ],
    Lists = [ListA, ListB, ListC, ListD],
    test_list_list(Lists, Lists, ParamsList, !IO).

    % test_list_list(SeqsA, SeqsB, ParamsList, !IO):
    %
    % Test every sequence in SeqsA against every sequence in SeqsB,
    % using every parameter set in ParamsList.
    %
:- pred test_list_list(list(list(string))::in, list(list(string))::in,
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
:- pred test_list(list(string)::in, list(list(string))::in,
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
:- pred test_params(list(string)::in, list(string)::in,
    assoc_list(string, edit_params)::in, io::di, io::uo) is det.

test_params(_SeqA, _SeqB, [], !IO).
test_params(SeqA, SeqB, [Params | ParamsList], !IO) :-
    test(SeqA, SeqB, Params, !IO),
    test_params(SeqA, SeqB, ParamsList, !IO).

    % test(SeqA, SeqB, Params, !IO):
    %
    % Test SeqA against SeqB.
    %
:- pred test(list(string)::in, list(string)::in, pair(string, edit_params)::in,
    io::di, io::uo) is det.

test(SeqA, SeqB, ParamsName - Params, !IO) :-
    ( if SeqA = SeqB then
        true
    else
        io.write_string("\n------------------\n\n", !IO),
        io.write_string("SeqA:   ", !IO),
        io.write_line(SeqA, !IO),
        io.write_string("SeqB:   ", !IO),
        io.write_line(SeqB, !IO),
        io.write_string("Params: ", !IO),
        io.write_string(ParamsName, !IO),
        io.nl(!IO),
        find_shortest_edit_seq(Params, SeqA, SeqB, Edits),
        io.write_string("\nEdits:  ", !IO),
        io.write_line(Edits, !IO),
        find_diff_seq(SeqA, Edits, DiffSeq),
        io.write_string("\nDiff:\n", !IO),
        write_diff_seq(DiffSeq, !IO),
        find_change_hunks(3, DiffSeq, CHunks),
        io.write_string("\nChange hunks:\n", !IO),
        write_chunk_seq(CHunks, !IO)
    ).

:- pred write_diff_seq(diff_seq(string)::in, io::di, io::uo) is det.

write_diff_seq([], !IO).
write_diff_seq([Diff | Diffs], !IO) :-
    (
        Diff = unchanged(Str),
        UDI = " "
    ;
        Diff = deleted(Str),
        UDI = "-"
    ;
        Diff = inserted(Str),
        UDI = "+"
    ),
    io.format("%s%s\n", [s(UDI), s(Str)], !IO),
    write_diff_seq(Diffs, !IO).

:- pred write_chunk_seq(list(change_hunk(string))::in, io::di, io::uo) is det.

write_chunk_seq([], !IO).
write_chunk_seq([CHunk | CHunks], !IO) :-
    CHunk = change_hunk(StartA, LenA, StartB, LenB, Diffs),
    io.format("@@ -%d,%d +%d,%d @@\n",
        [i(StartA), i(LenA), i(StartB), i(LenB)], !IO),
    write_diff_seq(Diffs, !IO),
    write_chunk_seq(CHunks, !IO).

%---------------------------------------------------------------------------%
:- end_module change_hunk_test.
%---------------------------------------------------------------------------%
