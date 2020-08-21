%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests that disjunctions gets handled properly.
%

:- module disj.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

:- type wrapper
    --->    w(int, int, int).

main(!IO) :-
    solutions(p([1, 7, 4]), SumList),
    io.write_string("p: ", !IO),
    io.write_line(SumList, !IO),
    pa([1, 7, 4], FirstSoln),
    io.write_string("First soln p: ", !IO),
    io.write_line(FirstSoln, !IO),
    solutions(p2([1, 7, 4]), SumList2),
    io.write_string("p2: ", !IO),
    io.write_line(SumList2, !IO),
    solutions(p3([1, 7, 4]), SumList3),
    io.write_string("p3: ", !IO),
    io.write_line(SumList3, !IO),
    solutions(p4a([1, 7, 4]), SumList4),
    io.write_string("p4: ", !IO),
    io.write_line(SumList4, !IO).

:- pred pa(list(int)::in, int::out) is cc_multi.

pa(X, Y) :-
    p(X, Y).

:- pred p(list(int)::in, int::out) is multi.

    % Introduce accumulators because each arm of the disjunction
    % will always produce the same value.
    %
p([], 0).
p([H | T], Sum) :-
    p(T, Sum0),
    (
        Tmp = 2 * H
    ;
        Tmp = 3 * H
    ),
    Sum = Sum0 + Tmp.

    % In the second arm of the disjunction, the call
    % (Sum = Sum0 + Tmp) contains 2 dynamic vars so we should fail.
    %
:- pred p2(list(int)::in, int::out) is nondet.

p2([], 0).
p2([H | T], Sum) :-
    p2(T, Sum0),
    (
        Tmp = 2 * H
    ;
        Tmp = H * Sum0
    ),
    Sum = Sum0 + Tmp.

:- pred p3(list(int)::in, int::out) is nondet.

p3([], 0).
p3([H | T], Sum) :-
    p3(T, Sum0),
    (
        Tmp = 0
    ;
        Tmp = Sum0
    ),
    Sum = H + Tmp.

:- pred p4a(list(int)::in, wrapper::out) is nondet.

p4a(X, Y) :-
    p4(X, S, L, NDS),
    Y = w(S, L, NDS).

:- pred p4(list(int)::in, int::out, int::out, int::out) is nondet.

p4([], 0, 0, 0).
p4([H | T], Sum, Length, NonDetSum) :-
    p4(T, Sum0, Length0, NonDetSum0),
    Length = Length0 + 1,
    Sum = H + Sum0,
    (
        Tmp = Length0
    ;
        Tmp = Sum0
    ;
        Tmp = NonDetSum0
    ),
    NonDetSum = H + Tmp.
