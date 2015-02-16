%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% There should be no determinism clause on a pred declaration without modes.

:- module bad_detism.

:- interface.

:- pred p(string) is det.
:- mode p(out) is det.

:- implementation.

p("xyzzy").
