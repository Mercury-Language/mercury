%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module undetermined_existential_data_type.
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

:- type bad_quant
    --->    some [T, U] f(T) => (fooable(T), fooable(U)).

%---------------------------------------------------------------------------%

test(_N, !IO).

%---------------------------------------------------------------------------%
