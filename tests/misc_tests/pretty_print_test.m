%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% XXX we should also test use of operators

:- module pretty_print_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type foobar
    --->    foo
    ;       bar(int)
    ;       baz(int, int).

:- type cont(T)
    --->    foo
    ;       cont(T, cont(T)).

:- type eq(T1, T2) == foobar.

main -->
    io__write_int(type_num(42)),
    io__nl.

:- typeclass numbered_type(T) where [
    func type_num(T) = int,
    func type_num_2(T) = int,
    pred type_num_3(T, int),
    mode type_num_3(in, out),
    mode type_num_3(out, in),
    pred type_num_4(T::in, int::out)
].

:- instance numbered_type(int) where [
    func(type_num/1) is foo_type_num,
    func(type_num_2/1) is foo_type_num,
    pred(type_num_3/2) is foo_type_num_p,
    pred(type_num_4/2) is foo_type_num_p
].

:- func foo_type_num(int) = int.

foo_type_num(_) = 42.

:- pred foo_type_num_p(int, int).
:- mode foo_type_num_p(in, out) is det.
:- mode foo_type_num_p(out, in) is det.

foo_type_num_p(_, 42).
