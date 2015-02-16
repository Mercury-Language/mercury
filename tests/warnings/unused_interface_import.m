%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module unused_interface_import.
:- interface.

:- import_module unused_interface_import3.
:- import_module unused_interface_import2.  % Should warn about this.

:- typeclass tc1(T) <= tc2(T) where [].

:- implementation.

:- import_module unit.

:- instance tc1(unit) where [].
