%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module binary_search.

:- interface.

:- import_module bool.
:- import_module int.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- pred silly_even(int::in, bool::out) is det.

:- implementation.

:- import_module binary_search_1.
:- import_module list.

main(!IO) :-
    a(A),
    b(B),
    add1s([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], L),
    write(A, !IO),
    write(B, !IO),
    write(L, !IO),
    nl(!IO).

:- pred a(bool::out) is det.

a(X) :-
    binary_search.silly_even(1001, X).

:- pred b(bool::out) is det.

b(X) :-
    binary_search.silly_even(1003, X).

silly_even(N, R) :-
    ( if N > 0 then
        ( if
            (
                N =< 600,
                N >= 405
            ;
                N mod 3 = 0
            )
        then
            binary_search_1.sillier_even(N, R)
        else
            ( if N = 619 then
                binary_search.silly_even(N - 1, R)
            else
                binary_search.silly_even(N - 2, R)
            )
        )
    else
        ( if N = 0 then
            R = yes
        else
            R = no
        )
    ).

:- pred add1s(list(int)::in, list(int)::out) is det.

add1s([], []).
add1s([H0 | T0], [H0+1 | T]) :-
    add1s(T0, T).
