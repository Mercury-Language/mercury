%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module max_length_impl.

:- interface.

:- import_module list.

:- pred max_length(list(int)::in, int::out, int::out) is det.

:- implementation.

:- import_module int.

max_length(Ls, M, Len) :-
    max(Ls, M),
    my_length(Ls, Len).

:- pred my_length(list(T)::in, int::out) is det.

my_length([], 0).
my_length([_ | T], L) :-
    my_length(T, LT),
    L = LT + 1.

:- pred max(list(int)::in, int::out) is det.

max(X, M) :-
    max1(X, 0, M).

:- pred max1(list(int)::in, int::in, int::out) is det.

max1([], M, M).
max1([H | T], N, M) :-
    ( if H > N then
        M1 = H
    else
        M1 = N
    ),
    max1(T, M1, M).
