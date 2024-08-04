%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module shallow_helper_2.
:- interface.

:- pred a(string::in, int::in, int::out) is multi.
:- pred b(string::in, int::in, int::out) is det.

:- implementation.

a(_, X, X).
a(_, _, 0).

b(_, _, 5).
