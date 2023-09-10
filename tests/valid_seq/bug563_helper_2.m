%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug563_helper_2.
:- interface.

:- type quirks.

:- implementation.

:- import_module set.

:- type quirks == set(string).
