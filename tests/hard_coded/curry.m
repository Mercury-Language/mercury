%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test curried functions.
% This is a regression test: mercury-0.6 failed this test.
%

:- module curry.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    io.write_string("Hello, world\n", !IO),
    _ = my_map(curry2(my_append), [[1], [2], [3]]),
    _ = my_map(curry2(my_plus), [1, 2, 3]).

:- func my_append(list(T), list(T)) = list(T).

my_append(A, B) = C :-
    list.append(A, B, C).

:- inst func1 == (func(in) = out is det).

:- func curry2(func(T1, T2) = T3) = (func(T1) = (func(T2) = T3)).
:- mode curry2(func(in, in) = out is det) =
    out(func(in) = out(func(in) = out is det) is det).

curry2(F) =
    ( (func(X::in) = (F1::out((func(in) = out is det))) is det) :-
        F1 = (func(Y) = apply(F, X, Y))
    ).

:- func my_plus(int, int) = int.

my_plus(A, B) = A + B.

:- func my_map(func(T1) = T2, list(T1)) = list(T2).
% :- mode my_map(func(in) = out is det, in) = out is det.
:- mode my_map(func(in) = out(func(in) = out is det) is det, in) = out is det.

my_map(_F, []) = [].
my_map(F, [X | Xs]) = [apply(F, X) | my_map(F, Xs)].
