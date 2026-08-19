%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug584.
:- interface.

:- import_module bug584_helper_1.
:- import_module bug584_helper_1.sub1.

:- pred get_field1(struct::in, int::out) is det.

:- implementation.

get_field1(Struct, F1) :-
    F1 = Struct ^ f1.

:- end_module bug584.
