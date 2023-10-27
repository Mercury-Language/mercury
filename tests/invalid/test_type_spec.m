%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_type_spec.

:- interface.

:- import_module list.

:- pred type_spec1(list(T)::in) is semidet.

:- pragma type_spec(type_spec1/1, U = int).
:- pragma type_spec(type_spec1(out), T = int).
:- pragma type_spec(type_spec1/1, T = list(U)).
:- pragma type_spec(type_spec1/2, T = int).

:- typeclass fooable(T) where [
    pred foo(T),
    mode foo(in) is semidet
].

:- type the_type(T, U).
:- some [U] pred type_spec2(the_type(T, U)::in) is semidet => fooable(U).

:- pragma type_spec(type_spec2/1, U = int).

:- pragma type_spec(type_spec2/1, U = list(U)).

:- pragma type_spec(type_spec2/1, (U = int, U = list(int))).

:- implementation.

:- type the_type(T, U)
    --->    type_type(T, U).
