%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This tests the case of committing across a nondet goal in a nondet
% context. There was a bug in this, which this test case exercised.

:- module commit_bug_2.

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

:- pred print_intlist(list(int)::in, io::di, io::uo) is det.

print_intlist([], !IO).
print_intlist([X | L], !IO) :-
    io.write_int(X, !IO),
    io.write_string("\n", !IO),
    print_intlist(L, !IO).

:- pred test(int::out) is nondet.

test(A) :-
    p(A),
    some [B] (
        q(A, B),
        C = B + 1,
        some [D] (
            r(C, D),
            D > 25
        )
    ).

:- pred p(int::out) is multi.

p(0).
p(1).
p(2).
p(3).

:- pred q(int::in, int::out) is nondet.

q(0, 1).
q(1, 2).
q(1, 3).
q(2, 4).
q(3, 5).

:- pred r(int::in, int::out) is nondet.

r(2, 10).
r(3, 20).
r(3, 20).
r(4, 20).
r(4, 20).
r(5, 40).
r(6, 50).
