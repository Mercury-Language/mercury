:- module bad_detism.

:- interface.

% There should be no determinism clause on a pred declaration without modes.

:- pred p(string) is det.
:- mode p(out) is det.

:- implementation.

p("xyzzy").
