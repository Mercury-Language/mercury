%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module inference_test_2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

:- typeclass foo(T) where [
    pred p(T::in, int::out) is det
].

:- instance foo(int) where [
    pred(p/2) is forty_two
].

main(!IO) :-
    main2(!IO).

main2(!IO) :-
    ( if r(X), q(X) then
        print("yes\n", !IO)
    else
        print("no\n", !IO)
    ).

% :- pred forty_two(int, int) is det.
:- mode forty_two(in, out) is det.

forty_two(X, Y) :-
    Y = X + 42.

% :- pred q(T) <= foo(T).
q(X) :-
    p(X, 42).

% :- pred r(int).
r(0).
