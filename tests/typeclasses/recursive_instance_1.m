%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module recursive_instance_1.
:- interface.

:- import_module io.

:- typeclass foo(T, U) where [
    func bar(T) = U
].

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module std_util.

:- instance foo(int, list(T)) <= foo(list(int), T) where [
    bar(N) = ( N < 0 -> [bar([N+1])] ; [] )
].

:- instance foo(list(T), int) <= foo(T, list(int)) where [
    ( bar([X | Xs]) = N + bar(Xs) :-
        bar(X) = B,
        (
            B = [N | _]
        ;
            B = [],
            N = 1
        )
    ),
    ( bar([]) = 0 )
].

main(!IO) :-
    X = bar([0, 1, 2]),
    io.write_int(X, !IO),
    io.nl(!IO).
