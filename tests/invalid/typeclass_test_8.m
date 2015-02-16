%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module typeclass_test_8.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- typeclass fooable(T) where [
    pred foo(T::out) is det,
    pred bar(T::in) is det
].

:- implementation.

main -->
    { foo(X) },
    { bar(X) }.
