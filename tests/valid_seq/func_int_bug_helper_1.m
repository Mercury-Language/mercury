%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module func_int_bug_helper_1.
:- interface.

:- func foo = (int::out) is det.

:- implementation.

foo = 6.
