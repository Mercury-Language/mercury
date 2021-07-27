%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module impure_method_impl.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- typeclass c(T) where [
    pred m1(T::in, int::out) is det,
    semipure pred m2(T::in, int::out) is det
].

:- type foo
    --->    foo.

:- semipure pred foo_m1(foo::in, int::out) is det.
:- impure pred foo_m2(foo::in, int::out) is det.

:- implementation.

:- instance c(foo) where [
    pred(m1/2) is foo_m1,
    pred(m2/2) is foo_m2
].

main(!IO).

:- pragma foreign_decl("C", "extern int foo_counter;").
:- pragma foreign_code("C", "int foo_counter = 0;").

:- pragma foreign_proc("C",
    foo_m1(_F::in, Val::out),
    [will_not_call_mercury, promise_semipure],
"
    Val = foo_counter;
").
:- pragma foreign_proc("C",
    foo_m2(_F::in, Val::out),
    [will_not_call_mercury],
"
    Val = foo_counter++;"
).
foo_m1(_, 0).
foo_m2(_, 0).
