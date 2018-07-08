%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case checks the correctness of the code that saves and restores
% answers for a predicate with multiple arguments (in this case, do_rotate).

:- module rotate2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module set.
:- import_module solutions.

:- pragma require_feature_set([memo]).

main(!IO) :-
    testgroup(3, 2, !IO),
    testgroup(3, 1, !IO),
    testgroup(2, 1, !IO),
    testgroup(2, 0, !IO).

:- pred testgroup(int::in, int::in, io::di, io::uo) is det.

testgroup(E, R, !IO) :-
    test(E, R, [10, 20, 30, 40, 50], !IO),
    test(E, R, [10, 20, 30, 40, 50], !IO),
    test(E, R, ["ten", "twenty", "thirty", "forty", "fifty"], !IO),
    test(E, R, [[1, 0], [2, 0], [3, 0], [4, 0], [5, 0]], !IO),
    test(E, R, [set_func([1]), set_func([1, 2]),
        set_func([1, 2, 3]), set_func([1, 2, 3, 4])], !IO).

:- pred test(int::in, int::in, list(T)::in, io::di, io::uo) is det.

test(NumElements, NumRotations, BaseList, !IO) :-
    solutions(rotations(NumElements, NumRotations, BaseList), Solns),
    io.write_string("rotations(", !IO),
    io.write_int(NumElements, !IO),
    io.write_string(", ", !IO),
    io.write_int(NumRotations, !IO),
    io.write_string(", ", !IO),
    io.write(BaseList, !IO),
    io.write_string(") =\n", !IO),
    write_solns(Solns, 0, !IO),
    io.write_string("\n", !IO).

:- pred rotations(int::in, int::in, list(T)::in, list(T)::out) is nondet.
:- pragma minimal_model(rotations/4).

rotations(NumElements, NumRotations, In, Out) :-
    NumRotations > 0,
    (
        test_rotate(NumElements, In, Out)
    ;
        rotations(NumElements - 1, NumRotations - 1, In, Out)
    ).

:- pred test_rotate(int::in, list(T)::in, list(T)::out) is semidet.

test_rotate(N, In, Out) :-
    do_rotate(N, In, M1, M2, M3, Rest),
    (
        M3 = yes(E3),
        L3 = [E3 | Rest]
    ;
        M3 = no,
        L3 = Rest
    ),
    (
        M2 = yes(E2),
        L2 = [E2 | L3]
    ;
        M2 = no,
        L2 = L3
    ),
    (
        M1 = yes(E1),
        L1 = [E1 | L2]
    ;
        M1 = no,
        L1 = L2
    ),
    Out = L1.

:- pred do_rotate(int::in, list(T)::in,
    maybe(T)::out, maybe(T)::out, maybe(T)::out, list(T)::out) is semidet.

do_rotate(N, In, M1, M2, M3, Rest) :-
    rotate(N, In, Out),
    (
        Out = [],
        M1 = no, M2 = no, M3 = no, Rest = []
    ;
        Out = [E1],
        M1 = yes(E1), M2 = no, M3 = no, Rest = []
    ;
        Out = [E1, E2],
        M1 = yes(E1), M2 = yes(E2), M3 = no, Rest = []
    ;
        Out = [E1, E2, E3 | Rest],
        M1 = yes(E1), M2 = yes(E2), M3 = yes(E3)
    ).

:- pred rotate(int::in, list(T)::in, list(T)::out) is semidet.

rotate(N, In, Out) :-
    list.split_list(N, In, Start, End),
    list.append(End, Start, Out).

:- func set_func(list(T)) = set(T).

set_func(List) = set.list_to_set(List).

:- pred write_solns(list(list(T))::in, int::in, io::di, io::uo) is det.

write_solns([], _, !IO).
write_solns([Soln | Solns], N, !IO) :-
    write_soln(Soln, N, !IO),
    write_solns(Solns, N + 1, !IO).

:- pred write_soln(list(T)::in, int::in, io::di, io::uo) is det.

write_soln(Soln, Seq, !IO) :-
    io.write_string("\tsoln ", !IO),
    io.write_int(Seq, !IO),
    io.write_string(": ", !IO),
    io.write(Soln, !IO),
    io.write_string("\n", !IO).
