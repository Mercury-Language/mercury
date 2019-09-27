%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unqualified_method2.

:- interface.

:- import_module io.

:- typeclass class(T) where [
    pred print_modified(T::in, io::di, io::uo) is det
].

:- instance class(int).

:- implementation.
:- import_module unqualified_method3.

:- instance class(int) where [
    pred(print_modified/3) is print_modified_int
].

