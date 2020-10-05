%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_poly_mode_2.

:- interface.

:- func new(T::in(I)) = (T::out(I)) is det.

:- implementation.

:- pragma foreign_proc("C",
    new(X::in(I)) = (R::out(I)),
    [promise_pure, will_not_call_mercury],
"
    R = X;
").
:- pragma foreign_proc("C#",
    new(X::in(I)) = (R::out(I)),
    [promise_pure, will_not_call_mercury],
"
    R = X;
").
:- pragma foreign_proc("Java",
    new(X::in(I)) = (R::out(I)),
    [promise_pure, will_not_call_mercury],
"
    R = X;
").
:- pragma foreign_proc("Erlang",
    new(X::in(I)) = (R::out(I)),
    [promise_pure, will_not_call_mercury],
"
    R = X
").
