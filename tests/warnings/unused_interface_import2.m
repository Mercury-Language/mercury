:- module unused_interface_import2.
:- interface.

:- import_module unit.

:- import_module unused_interface_import3.

:- instance tc2(unit).

:- implementation.

:- instance tc2(unit) where [].
