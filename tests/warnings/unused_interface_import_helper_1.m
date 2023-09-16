%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unused_interface_import_helper_1.
:- interface.

:- import_module unit.

:- import_module unused_interface_import_helper_2.

:- instance tc2(unit).

:- implementation.

:- instance tc2(unit) where [].
