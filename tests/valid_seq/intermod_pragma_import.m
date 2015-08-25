%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_pragma_import.

:- interface.

:- pred q(T::in, int::out) is det.

:- implementation.

:- import_module intermod_pragma_import2.

q(A, B) :-
    implemented_as_pragma_import(A, B).
