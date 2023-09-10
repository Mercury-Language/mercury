%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test was originally called intermod_pragma_import.
%

:- module foreign_proc_import.

:- interface.

:- pred q(T::in, int::out) is det.

:- implementation.

:- import_module foreign_proc_import_helper_1.

q(A, B) :-
    implemented_as_pragma_import(A, B).
