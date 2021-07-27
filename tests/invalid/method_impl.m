%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module method_impl.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- typeclass c(T) where [
    pred m1(T::in, int::out) is det
].

:- type foo ---> foo.
:- type bar ---> bar.

:- pred foo_m1(foo::in, string::out) is det.

:- pred foo_m2(foo::in, int::in) is det.

:- implementation.

:- instance c(foo) where [
    pred(m1/2) is foo_m1
].
:- instance c(bar) where [
    pred(m1/2) is bar_m1
].

main(!IO).

:- pragma foreign_code("C", "int foo_counter = 0;").

foo_m1(_, "forty two").

foo_m2(_, _).
