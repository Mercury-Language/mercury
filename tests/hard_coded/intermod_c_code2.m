%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_c_code2.

:- interface.

:- some [U] pred c_code(T::in, U::out) is det.

:- implementation.

c_code(T, U) :-
    c_code_2(T, U).

:- some [U] pred c_code_2(T::in, U::out) is det.

:- pragma foreign_proc("C",
    c_code_2(T::in, U::out),
    [will_not_call_mercury, promise_pure],
"{
    U = T;
    TypeInfo_for_U = TypeInfo_for_T;
}").
:- pragma foreign_proc("C#",
    c_code_2(T::in, U::out),
    [promise_pure],
"{
    U = T;
    TypeInfo_for_U = TypeInfo_for_T;
}").
:- pragma foreign_proc("Java",
    c_code_2(T::in, U::out),
    [promise_pure],
"{
    U = T;
    TypeInfo_for_U = TypeInfo_for_T;
}").
