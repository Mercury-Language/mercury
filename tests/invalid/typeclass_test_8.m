%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module typeclass_test_8.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- typeclass fooable(T) where [
    pred foo(T::out) is det,
    pred bar(T::in) is det
].

:- implementation.

main(!IO) :-
    foo(X),
    bar(X).
