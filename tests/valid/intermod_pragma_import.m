:- module intermod_pragma_import.

:- interface.

:- pred q(T::in, int::out) is det.

:- implementation.

:- import_module intermod_pragma_import2.

q(A, B) :-
        implemented_as_pragma_import(A, B).

