%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A development version of the Mercury compiler generated invalid C code
% for this example, because it didn't properly escape special characters
% such as \ in some of the C strings that it generated.

:- module unescaped_special_char.

:- interface.

:- type myint64.

:- func myint64 /\ myint64 = myint64.

:- implementation.

:- type myint64 == int.

:- pragma foreign_proc("C",
    (A::in) /\ (B::in) = (C::out),
    [promise_pure, will_not_call_mercury],
"
    C = A & B;
").

% implementation for the other backends.
A /\ _B = A.
