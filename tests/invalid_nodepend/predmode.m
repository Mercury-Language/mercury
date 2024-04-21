%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module predmode.

:- interface.

:- pred p(int::in, int).

:- pred q(int::out) is det.

:- implementation.

q(1).

:- pred r(1::2, int:in, "bad"::"worse") is det.
