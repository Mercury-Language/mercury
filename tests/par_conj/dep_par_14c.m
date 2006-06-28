% This is the same as dep_par_14 but uses a switch instead
% of if-then-else.

:- module dep_par_14c.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module bool.
:- import_module list.

main(!IX) :-
    R = [1, 5, 3, 4, 7, 8, 6, 9, 2, 0],
    p(R, 1, S),
    io.print(S, !IX),
    io.nl(!IX).

:- pred p(list(int)::in, int::in, int::out) is cc_multi.

p([], A, A).
p([H|T], A0, A) :-
    eq(H, A0, Eq),
    (
	Eq = yes,
        ( p(T, A0, A1)
        & p(T, A1, A)
        )
    ;
	Eq = no,
        A = A0
    ).

:- pred eq(int::in, int::in, bool::out) is det.
:- pragma no_inline(eq/3).
eq(A, B, (if A = B then yes else no)).
