% This module tests the error message for malformed external declarations.

:- module external.

:- interface.

:- pred p(int, int).
:- mode p(in, out) is nondet.

:- implementation.

:- external(p).
