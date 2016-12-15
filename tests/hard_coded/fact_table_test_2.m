%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the behavior of fact tables with
%
% - arguments of different types,
% - of which more than one is input.
%
%---------------------------------------------------------------------------%

:- module fact_table_test_2.
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
    InsI = 1 .. 9,
    InsF = list.map(float.float, InsI),
    InsS = list.map(string.int_to_string, InsI),

    io.write_string("\ntest_a_1\n", !IO),
    test_fact_table(InsI, test_a_1, !IO),

    io.write_string("\ntest_a_2\n", !IO),
    test_fact_table(InsF, test_a_2, !IO),

    io.write_string("\ntest_a_3\n", !IO),
    test_fact_table(InsS, test_a_3, !IO),

    io.write_string("\ntest_a_1_2\n", !IO),
    test_fact_table(make_doubles(InsI, InsF), test_a_1_2, !IO),

    io.write_string("\ntest_a_1_3\n", !IO),
    test_fact_table(make_doubles(InsI, InsS), test_a_1_3, !IO),

    io.write_string("\ntest_a_2_3\n", !IO),
    test_fact_table(make_doubles(InsF, InsS), test_a_2_3, !IO),

    io.write_string("\ntest_a_1_2_3\n", !IO),
    test_fact_table(make_triples(InsI, InsF, InsS), test_a_1_2_3, !IO).

:- pred test_fact_table(list(T)::in, pred(T, string)::in(pred(in, out) is det),
    io::di, io::uo) is det.

test_fact_table([], _CallFactTable, !IO).
test_fact_table([In | Ins], CallFactTable, !IO) :-
    CallFactTable(In, Str),
    io.write_string(Str, !IO),
    test_fact_table(Ins, CallFactTable, !IO).

%---------------------------------------------------------------------------%

:- func make_doubles(list(T1), list(T2)) = list({T1, T2}).

make_doubles([], _) = [].
make_doubles([A | As], Bs) =
    list.map(double_with(A), Bs) ++
    make_doubles(As, Bs).

:- func double_with(T1, T2) = {T1, T2}.

double_with(A, B) = {A, B}.

:- func make_triples(list(T1), list(T2), list(T3)) = list({T1, T2, T3}).

make_triples([], _, _) = [].
make_triples([A | As], Bs, Cs) =
    list.map(triple_with(A), make_doubles(Bs, Cs)) ++
    make_triples(As, Bs, Cs).

:- func triple_with(T1, {T2, T3}) = {T1, T2, T3}.

triple_with(A, {B, C}) = {A, B, C}.

%---------------------------------------------------------------------------%

:- pred test_a_1(int::in, string::out) is det.

test_a_1(A, Str) :-
    solutions(test_a_1x(A), Strs),
    string.append_list(Strs, Str).

:- pred test_a_1x(int::in, string::out) is multi.

test_a_1x(A, Str) :-
    ( if table_a(A, B, C) then
        Str = string.format("%2d %4.1f %2s\n", [i(A), f(B), s(C)])
    else
        Str = string.format("%2d %4s %2s\n", [i(A), s("_"), s("_")])
    ).

:- pred test_a_2(float::in, string::out) is det.

test_a_2(B, Str) :-
    ( if table_a(A, B, C) then
        Str = string.format("%2d %4.1f %2s\n", [i(A), f(B), s(C)])
    else
        Str = string.format("%2s %4.1f %2s\n", [s("_"), f(B), s("_")])
    ).

:- pred test_a_3(string::in, string::out) is det.

test_a_3(C, Str) :-
    solutions(test_a_3x(C), Strs),
    string.append_list(Strs, Str).

:- pred test_a_3x(string::in, string::out) is multi.

test_a_3x(C, Str) :-
    ( if table_a(A, B, C) then
        Str = string.format("%2d %4.1f %2s\n", [i(A), f(B), s(C)])
    else
        Str = string.format("%2s %4s %2s\n", [s("_"), s("_"), s(C)])
    ).

%---------------------------------------------------------------------------%

:- pred test_a_1_2({int, float}::in, string::out) is det.

test_a_1_2({A, B}, Str) :-
    ( if table_a(A, B, C) then
        Str = string.format("%2d %4.1f %2s\n", [i(A), f(B), s(C)])
    else
        Str = string.format("%2d %4.1f %2s\n", [i(A), f(B), s("_")])
    ).

:- pred test_a_1_3({int, string}::in, string::out) is det.

test_a_1_3({A, C}, Str) :-
    solutions(test_a_1_3x({A, C}), Strs),
    string.append_list(Strs, Str).

:- pred test_a_1_3x({int, string}::in, string::out) is multi.

test_a_1_3x({A, C}, Str) :-
    ( if table_a(A, B, C) then
        Str = string.format("%2d %4.1f %2s\n", [i(A), f(B), s(C)])
    else
        Str = string.format("%2d %4s %2s\n", [i(A), s("_"), s(C)])
    ).

:- pred test_a_2_3({float, string}::in, string::out) is det.

test_a_2_3({B, C}, Str) :-
    solutions(test_a_2_3x({B, C}), Strs),
    string.append_list(Strs, Str).

:- pred test_a_2_3x({float, string}::in, string::out) is multi.

test_a_2_3x({B, C}, Str) :-
    ( if table_a(A, B, C) then
        Str = string.format("%2d %4.1f %2s\n", [i(A), f(B), s(C)])
    else
        Str = string.format("%2s %4.1f %2s\n", [s("_"), f(B), s(C)])
    ).

%---------------------------------------------------------------------------%

:- pred test_a_1_2_3({int, float, string}::in, string::out) is det.

test_a_1_2_3({A, B, C}, Str) :-
    ( if table_a(A, B, C) then
        Str = string.format("%2d %4.1f %2s\n", [i(A), f(B), s(C)])
    else
        Str = ""
    ).

%---------------------------------------------------------------------------%

:- pred table_a(int, float, string).
:- mode table_a(in, out, out) is nondet.
:- mode table_a(out, in, out) is semidet.
:- mode table_a(out, out, in) is nondet.
:- mode table_a(in, in, out) is semidet.
:- mode table_a(in, out, in) is nondet.
:- mode table_a(out, in, in) is nondet.
:- mode table_a(in, in, in) is semidet.
:- pragma fact_table(table_a/3, "fact_table_test_2_table_a").

%---------------------------------------------------------------------------%
