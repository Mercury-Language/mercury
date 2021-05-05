%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the behavior of fact tables with
%
% - all-int, all-float and all-string arguments,
% - of which exactly one is input.
%
% The "test_a_*" tests call fact table table_a with all int arguments;
% the "test_b_*" tests call fact table table_b with all float arguments;
% the "test_c_*" tests call fact table table_c with all string arguments.
% The test_b and test_c fact tables are all based on test_a's fact table,
% with integers such as 1 replaced by 1.0 and " 1" respectively.
%
% Each test "test_x_y" calls fact table table_x with the y'th argument input.
% For y=1 and y=2, the called mode is semidet; for y=3, it is nondet.
%
%---------------------------------------------------------------------------%

:- module fact_table_test_1.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module float.
:- import_module list.
:- import_module string.
:- import_module solutions.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("\ntest_a_1\n", !IO),
    InsA1 = 0 .. 20,
    test_fact_table(InsA1, test_a_1, !IO),

    io.write_string("\ntest_b_1\n", !IO),
    InsB1 = list.map(float.float, InsA1),
    test_fact_table(InsB1, test_b_1, !IO),

    io.write_string("\ntest_c_1\n", !IO),
    InsC1 = list.map(int_to_2_digit_string, InsA1),
    test_fact_table(InsC1, test_c_1, !IO),

    %-----------------------------------%

    io.write_string("\ntest_a_2\n", !IO),
    InsA2 = 0 .. 24,
    test_fact_table(InsA2, test_a_2, !IO),

    io.write_string("\ntest_b_2\n", !IO),
    InsB2 = list.map(float.float, InsA2),
    test_fact_table(InsB2, test_b_2, !IO),

    io.write_string("\ntest_c_2\n", !IO),
    InsC2 = list.map(int_to_2_digit_string, InsA2),
    test_fact_table(InsC2, test_c_2, !IO),

    %-----------------------------------%

    io.write_string("\ntest_a_3\n", !IO),
    InsA3 = -4 .. 4,
    test_fact_table(InsA3, test_a_3, !IO),

    io.write_string("\ntest_b_3\n", !IO),
    InsB3 = list.map(float.float, InsA3),
    test_fact_table(InsB3, test_b_3, !IO),

    io.write_string("\ntest_c_3\n", !IO),
    InsC3 = list.map(int_to_2_digit_string, InsA3),
    test_fact_table(InsC3, test_c_3, !IO).

:- pred test_fact_table(list(T)::in, pred(T, string)::in(pred(in, out) is det),
    io::di, io::uo) is det.

test_fact_table([], _CallFactTable, !IO).
test_fact_table([In | Ins], CallFactTable, !IO) :-
    CallFactTable(In, Str),
    io.write_string(Str, !IO),
    test_fact_table(Ins, CallFactTable, !IO).

:- func int_to_2_digit_string(int) = string.

int_to_2_digit_string(N) = string.format("%2d", [i(N)]).

%---------------------------------------------------------------------------%

:- pred test_a_1(int::in, string::out) is det.

test_a_1(A, Str) :-
    ( if table_a(A, B, C) then
        Str = string.format("%2d %2d %2d\n", [i(A), i(B), i(C)])
    else
        Str = string.format("%2d %2s %2s\n", [i(A), s("_"), s("_")])
    ).

:- pred test_a_2(int::in, string::out) is det.

test_a_2(B, Str) :-
    ( if table_a(A, B, C) then
        Str = string.format("%2d %2d %2d\n", [i(A), i(B), i(C)])
    else
        Str = string.format("%2s %2d %2s\n", [s("_"), i(B), s("_")])
    ).

:- pred test_a_3(int::in, string::out) is det.

test_a_3(C, Str) :-
    solutions(test_a_3x(C), Strs),
    string.append_list(Strs, Str).

:- pred test_a_3x(int::in, string::out) is multi.

test_a_3x(C, Str) :-
    ( if table_a(A, B, C) then
        Str = string.format("%2d %2d %2d\n", [i(A), i(B), i(C)])
    else
        Str = string.format("%2s %2s %2d\n", [s("_"), s("_"), i(C)])
    ).

:- pred table_a(int, int, int).
:- mode table_a(in, out, out) is semidet.
:- mode table_a(out, in, out) is semidet.
:- mode table_a(out, out, in) is nondet.
:- pragma fact_table(pred(table_a/3), "fact_table_test_1_table_a").

%---------------------------------------------------------------------------%

:- pred test_b_1(float::in, string::out) is det.

test_b_1(A, Str) :-
    ( if table_b(A, B, C) then
        Str = string.format("%5.1f %5.1f %5.1f\n", [f(A), f(B), f(C)])
    else
        Str = string.format("%5.1f %5s %5s\n", [f(A), s("_"), s("_")])
    ).

:- pred test_b_2(float::in, string::out) is det.

test_b_2(B, Str) :-
    ( if table_b(A, B, C) then
        Str = string.format("%5.1f %5.1f %5.1f\n", [f(A), f(B), f(C)])
    else
        Str = string.format("%5s %5.1f %5s\n", [s("_"), f(B), s("_")])
    ).

:- pred test_b_3(float::in, string::out) is det.

test_b_3(C, Str) :-
    solutions(test_b_3x(C), Strs),
    string.append_list(Strs, Str).

:- pred test_b_3x(float::in, string::out) is multi.

test_b_3x(C, Str) :-
    ( if table_b(A, B, C) then
        Str = string.format("%5.1f %5.1f %5.1f\n", [f(A), f(B), f(C)])
    else
        Str = string.format("%5s %5s %5.1f\n", [s("_"), s("_"), f(C)])
    ).

:- pred table_b(float, float, float).
:- mode table_b(in, out, out) is semidet.
:- mode table_b(out, in, out) is semidet.
:- mode table_b(out, out, in) is nondet.
:- pragma fact_table(table_b/3, "fact_table_test_1_table_b").

%---------------------------------------------------------------------------%

:- pred test_c_1(string::in, string::out) is det.

test_c_1(A, Str) :-
    ( if table_c(A, B, C) then
        Str = string.format("%2s %2s %2s\n", [s(A), s(B), s(C)])
    else
        Str = string.format("%2s %2s %2s\n", [s(A), s("_"), s("_")])
    ).

:- pred test_c_2(string::in, string::out) is det.

test_c_2(B, Str) :-
    ( if table_c(A, B, C) then
        Str = string.format("%2s %2s %2s\n", [s(A), s(B), s(C)])
    else
        Str = string.format("%2s %2s %2s\n", [s("_"), s(B), s("_")])
    ).

:- pred test_c_3(string::in, string::out) is det.

test_c_3(C, Str) :-
    solutions(test_c_3x(C), Strs),
    string.append_list(Strs, Str).

:- pred test_c_3x(string::in, string::out) is multi.

test_c_3x(C, Str) :-
    ( if table_c(A, B, C) then
        Str = string.format("%2s %2s %2s\n", [s(A), s(B), s(C)])
    else
        Str = string.format("%2s %2s %2s\n", [s("_"), s("_"), s(C)])
    ).

:- pred table_c(string, string, string).
:- mode table_c(in, out, out) is semidet.
:- mode table_c(out, in, out) is semidet.
:- mode table_c(out, out, in) is nondet.
:- pragma fact_table(table_c/3, "fact_table_test_1_table_c").

%---------------------------------------------------------------------------%
