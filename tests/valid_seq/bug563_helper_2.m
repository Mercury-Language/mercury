:- module bug563_helper_2.
:- interface.

:- type quirks.

:- implementation.

:- import_module set.

:- type quirks == set(string).
