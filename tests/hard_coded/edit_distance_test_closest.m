%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This tests the functionality of find_closest_seqs in library/edit_seq.m.
% The test data are from the self test code in gcc.
%
%---------------------------------------------------------------------------%

:- module edit_distance_test_closest.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module edit_distance.

:- import_module char.
:- import_module list.
:- import_module int.
:- import_module string.

main(!IO) :-
    % Treat case changes as half the cost of any other replacement.
    Params = edit_params(2u, 2u, case_sensitive_replacement_cost, 2u),

    FruitCandidates = ["apple", "banana", "cherry"],
    FruitQueries = ["app", "banyan", "berry"],

    % This tests yields a unique closest match ONLY with parameters
    % that treat a transposition of two characters as cheaper than
    % the total cost of an insertion and a deletion.
    CoordCandidates = ["coordx", "coordy", "coordz",
        "coordx1", "coordy1", "coordz1"],
    CoordQueries = ["coorzd1"],

    DwarfCandidates = ["DWARF_GNAT_ENCODINGS_GDB",
        "DWARF_GNAT_ENCODINGS_ALL", "DWARF_GNAT_ENCODINGS_MINIMAL"],
    DwarfQueries = ["DWARF_GNAT_ENCODINGS_all"],

    OptionCandidates = ["-Wtrivial-auto-var-init", "-ftrivial-auto-var-init="],
    OptionQueries = ["-ftrivial-auto-var-init"],

    test_queries_with_candidates(Params, FruitCandidates, FruitQueries, !IO),
    test_queries_with_candidates(Params, CoordCandidates, CoordQueries, !IO),
    test_queries_with_candidates(Params, DwarfCandidates, DwarfQueries, !IO),
    test_queries_with_candidates(Params, OptionCandidates, OptionQueries, !IO).

:- func case_sensitive_replacement_cost(char, char) = uint.

case_sensitive_replacement_cost(CharA, CharB) = ReplacementCost :-
    char.to_lower(CharA, LowerCharA),
    char.to_lower(CharB, LowerCharB),
    ( if LowerCharA = LowerCharB then
        % CharA and CharB differ only in case.
        ReplacementCost = 1u
    else
        ReplacementCost = 2u
    ).

%---------------------------------------------------------------------------%

:- pred test_queries_with_candidates(edit_params(char)::in,
    list(string)::in, list(string)::in, io::di, io::uo) is det.

test_queries_with_candidates(_, _, [], !IO).
test_queries_with_candidates(Params, CandidateStrs,
        [QueryStr | QueryStrs], !IO) :-
    test_query_with_candidates(Params, CandidateStrs, QueryStr, !IO),
    test_queries_with_candidates(Params, CandidateStrs, QueryStrs, !IO).

:- pred test_query_with_candidates(edit_params(char)::in,
    list(string)::in, string::in, io::di, io::uo) is det.

test_query_with_candidates(Params, CandidateStrs, QueryStr, !IO) :-
    list.map(string.to_char_list, CandidateStrs, CandidateCharLists),
    string.to_char_list(QueryStr, QueryCharList),

    find_closest_seqs(Params, QueryCharList, CandidateCharLists,
        Cost, HeadBestChars, TailBestChars),
    list.map(string.from_char_list, [HeadBestChars | TailBestChars],
        BestStrs),

    list.reverse(CandidateCharLists, RevCandidateCharLists),
    find_closest_seqs(Params, QueryCharList, RevCandidateCharLists,
        RevCost, HeadRevBestChars, TailRevBestChars),
    list.map(string.from_char_list, [HeadRevBestChars | TailRevBestChars],
        BestRevStrs),
    list.reverse(BestRevStrs, RevBestRevStrs),

    DisplayStr = (func(S) = "    <" ++ S ++ ">\n"),

    CandidateDisplayStrs = list.map(DisplayStr, CandidateStrs),
    BestDisplayStrs = list.map(DisplayStr, BestStrs),
    io.write_string("among candidates\n", !IO),
    list.foldl(io.write_string, CandidateDisplayStrs, !IO),
    io.format("query <%s> yields cost %u matches\n",
        [s(QueryStr), u(Cost)], !IO),
    list.foldl(io.write_string, BestDisplayStrs, !IO),

    ( if BestStrs = RevBestRevStrs then
        true
    else
        BestRevDisplayStrs = list.map(DisplayStr, BestRevStrs),
        io.write_string("but with the reversed candidate list\n", !IO),
        list.foldl(io.write_string, BestRevDisplayStrs, !IO)
    ),

    ( if Cost = RevCost then
        true
    else
        io.write_string("but with the reversed candidate list\n", !IO),
        io.format("     cost differs: %u vs %u\n", [u(Cost), u(RevCost)], !IO)
    ),

    io.nl(!IO).

%---------------------------------------------------------------------------%
