%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_existential_data_type.
:- interface.
:- import_module io.

:- pred test(int::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module std_util.
:- import_module string.

:- typeclass fooable(T) where [
    pred foo(T::in, int::out) is det
].

:- instance fooable(int) where [
    pred(foo/2) is int_foo
].

:- type bad_quant_1
    --->    some [T] f1 => fooable(T).

:- type bad_quant_2
    --->    some [T, U, V] f2(T) => (fooable(T), fooable(U)).

:- type bad_quant_3
    --->    some [T, U, V] f3(T) => fooable(T).

:- type bad_quant_4(T)
    --->    some [T, U, V] f4(T) => fooable(T).

:- type bad_quant_5(T, U)
    --->    some [T, U, V] f5(T) => fooable(T).

:- type bad_quant_6
    --->    some [T] f6(T) => fooable(T, U).

:- type bad_quant_7
    --->    some [T] f7(T) => fooable(T, U, V).

:- type bad_quant_8
    --->    some [T, U] f8(T) => (fooable(T), fooable(U)).

:- type bad_constraint_9
    --->    some [T] f9(T) => fooable(list(T)).

:- typeclass barable(T, U) <= (T -> U) where [
    pred bar(T::in, U::in, int::out) is det
].

:- instance barable(int, int) where [
    pred(bar/3) is int_bar
].

:- type ok_10
    --->    some [T, U] f10(T) => barable(T, U).

:- pred int_foo(int::in, int::out) is det.

int_foo(X, 2*X).

:- pred int_bar(int::in, int::in, int::out) is det.

int_bar(X, Y, X*Y).

%---------------------------------------------------------------------------%

test(N, !IO) :-
    foo(N, X),
    io.write_int(X, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%
