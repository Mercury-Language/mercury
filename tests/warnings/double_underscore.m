%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module double_underscore.

:- interface.

:- pred p(int::in, int::out) is det.

:- implementation.

p(_X, _X).
