%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module undef_inst.
:- interface.

:- pred x(int).
:- mode x(foo >> bar) is det.

:- implementation.

x(1).
