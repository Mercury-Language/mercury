%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug100_helper_1.
:- interface.

:- import_module bug100_helper_2.
:- import_module unit.

:- instance tc2(unit).

:- implementation.

:- instance tc2(unit) where [].
