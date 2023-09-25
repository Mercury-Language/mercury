%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% This tests whether case sensitive replacement costs work.
% The test data are from the self test code in gcc.
%
%---------------------------------------------------------------------------%

:- module edit_distance_test_cost.
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
:- import_module uint.

main(!IO) :-
    % Treat case changes as we treat any other replacement.
    % Treat transposition as a delete AND an insert.
    ParamsNN = edit_params(2u, 2u, case_insensitive_replacement_cost, 4u),
    % Treat case changes as we treat any other replacement.
    % Treat transposition as a delete OR an insert.
    ParamsNY = edit_params(2u, 2u, case_insensitive_replacement_cost, 2u),
    % Treat case changes as HALF THE COST of any other replacement.
    % Treat transposition as a delete AND an insert.
    ParamsYN = edit_params(2u, 2u, case_sensitive_replacement_cost, 4u),
    % Treat case changes as HALF THE COST of any other replacement.
    % Treat transposition as a delete OR an insert.
    ParamsYY = edit_params(2u, 2u, case_sensitive_replacement_cost, 2u),

    TestCases =
        [test_case("", "nonempty", 8, 0, 0),
        test_case("saturday", "sunday", 3, 0, 0),
        test_case("foo", "m_foo", 2, 0, 0),
        test_case("hello_world", "HelloWorld", 1, 2, 0),
        test_case("the quick brown fox jumps over the lazy dog",
            "dog", 40, 0, 0),
        test_case("the quick brown fox jumps over the lazy dog",
            "the quick brown dog jumps over the lazy fox", 4, 0, 0),
        test_case("Lorem ipsum dolor sit amet, consectetur adipiscing elit,",
            "All your base are belong to us", 44, 0, 0),
        test_case("foo", "FOO", 0, 3, 0),
        test_case("fee", "deed", 2, 0, 0),
        test_case("coorzd1", "coordx1", 2, 0, 0),
        test_case("assert", "sqrt", 3, 0, 0),
        test_case("PATH_MAX", "INT8_MAX", 3, 0, 0),
        test_case("time", "nice", 2, 0, 0),
        test_case("bar", "carg", 2, 0, 0),
        test_case("gtk_widget_show_all", "GtkWidgetShowAll", 3, 4, 0),
        test_case("m_bar", "bar", 2, 0, 0),
        test_case("MACRO", "MACRAME", 3, 0, 0),
        test_case("ab", "ac", 1, 0, 0),
        test_case("ab", "a", 1, 0, 0),
        test_case("a", "b", 1, 0, 0),
        test_case("nanl", "name", 2, 0, 0),
        test_case("char", "bar", 2, 0, 0),
        test_case("-optimize", "fsanitize", 5, 0, 0),
        test_case("__DATE__", "__i386__", 4, 0, 0),
        test_case("ab", "ba", 0, 0, 1),             % s/ab/ba/
        test_case("ba", "abc", 2, 0, 0),
        % The obvious edit sequence here is s/ba/ab/, s//c/,
        % but another possible edit sequence to transform "ba" into "abc"
        % is s//a/, s/a/c/, i.e. adding "a" at the front of "ba",
        % transforming it into "aba", and then replacing the final "a"
        % with a "c".
        %
        % The s/ba/ab/, s//c/ sequence has one transposition and one insert.
        % The s//a/, s/a/c/ sequence has one insert and one replacement.
        %
        % With parameters that set the cost of a transposition the same as
        % the cost of a replacement, i.e. Params?Y, the costs are the same.
        % With parameters that set the cost of a transposition to be different
        % than cost of a replacement, i.e. Params?N, the costs are different.
        %
        % Since the cost of the second edit sequence is never more expensive
        % than the first, that is what we expect, as shown by the <2, 0, 0>
        % cost vector above.
        test_case("coorzd1", "coordz1", 0, 0, 1),   % s/zd/dz/
        test_case("abcdefghijklmnopqrstuvwxyz", "bacdefghijklmnopqrstuvwxzy",
            0, 0, 2),                               % s/ab/ba/, s/yz/zy/
        test_case("saturday", "sundya", 3, 0, 1),   % s/at/, s/r/n/, s/ay/ya/
        test_case("signed", "singed", 0, 0, 1)],

    test_costs(ParamsNN, ParamsNY, ParamsYN, ParamsYY, TestCases, !IO).

:- func case_insensitive_replacement_cost(char, char) = uint.

case_insensitive_replacement_cost(_CharA, _CharB) = 2u.

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

:- type test_case
    --->    test_case(
                % The two strings to be compared in both directions.
                string,
                string,

                % The total number insertions, deletions, and general
                % replacements between the two strings, *other than*
                % the changes covered by the next two fields.
                int,

                % The number of case changing replacements.
                int,

                % The number of transpositions.
                int
            ).

:- pred test_costs(edit_params(char)::in, edit_params(char)::in,
    edit_params(char)::in, edit_params(char)::in,
    list(test_case)::in, io::di, io::uo) is det.

test_costs(_, _, _, _, [], !IO).
test_costs(ParamsNN, ParamsNY, ParamsYN, ParamsYY,
        [TestCost | TestCosts], !IO) :-
    TestCost = test_case(StrA, StrB, Gen, Case, Transpose),
    io.format("\ntesting <%s> vs <%s>\n", [s(StrA), s(StrB)], !IO),
    ExpectedCostNN = (Gen * 2) + (Case * 2) + (Transpose * 4),
    ExpectedCostNY = (Gen * 2) + (Case * 2) + (Transpose * 2),
    ExpectedCostYN = (Gen * 2) + (Case * 1) + (Transpose * 4),
    ExpectedCostYY = (Gen * 2) + (Case * 1) + (Transpose * 2),
    test_cost(ParamsNN, "smash_no_transpose_no   ",
        StrA, StrB, ExpectedCostNN, !IO),
    test_cost(ParamsNY, "smash_no_transpose_yes  ",
        StrA, StrB, ExpectedCostNY, !IO),
    test_cost(ParamsYN, "smash_yes_transpose_no  ",
        StrA, StrB, ExpectedCostYN, !IO),
    test_cost(ParamsYY, "smash_yes_transpose_yes ",
        StrA, StrB, ExpectedCostYY, !IO),
    test_costs(ParamsNN, ParamsNY, ParamsYN, ParamsYY, TestCosts, !IO).

:- pred test_cost(edit_params(char)::in, string::in,
    string::in, string::in, int::in, io::di, io::uo) is det.

test_cost(Params, CaseDesc, StrA, StrB, ExpectedCost, !IO) :-
    string.to_char_list(StrA, CharsA),
    string.to_char_list(StrB, CharsB),
    find_edit_distance(Params, CharsA, CharsB, UCostAB),
    find_edit_distance(Params, CharsB, CharsA, UCostBA),
    CostAB = uint.cast_to_int(UCostAB),
    CostBA = uint.cast_to_int(UCostBA),
    ( if
        CostAB = ExpectedCost,
        CostBA = ExpectedCost
    then
        io.format("%s match\n", [s(CaseDesc)], !IO)
    else
        io.format("%s mismatch: expected %d, got %d and %d\n",
            [s(CaseDesc), i(ExpectedCost), i(CostAB), i(CostBA)], !IO)
    ).

%---------------------------------------------------------------------------%
