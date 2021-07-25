%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This tests the case of committing across a nondet goal in a nondet
% context. There was a bug in this, which this test case exercised.

:- module commit_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

main(!IO) :-
    solutions(test, List),
    print_intlist(List, !IO).

:- pred test(int::out) is multi.

test(Val) :-
    ( if some [X]
        list.member(X, [1, 2, 3, 4, 5])
    then
        ( if some [Z] (
            some [Y] foo(X, Y),
            foo(X, Z)
            )
        then
            Val = Z
        else
            Val = -1
        )
    else
        Val = -2
    ).

:- pred foo(int::in, int::out) is nondet.

foo(X, X).
foo(_, 7).

:- pred print_intlist(list(int)::in, io::di, io::uo) is det.

print_intlist([], !IO).
print_intlist([X | L], !IO) :-
    io.write_int(X, !IO),
    io.nl(!IO),
    print_intlist(L, !IO).
