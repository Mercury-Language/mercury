% vim: sw=4 ts=4 et
%
% This test case checks the correctness of the code that saves and restores
% answers of different types, including both builtin and user-defined types.

:- module rotate.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module solutions, int, list, set.

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
        rotate(NumElements, In, Out)
    ;
        rotations(NumElements - 1, NumRotations - 1, In, Out)
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
