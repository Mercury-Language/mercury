%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module completion.completion_helper_1.

:- interface.

:- func z1 = int.

:- pred zp(int::out) is det.

:- implementation.

z1 = 1.

zp(1).
