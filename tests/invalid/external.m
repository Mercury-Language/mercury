%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests the error message for malformed external declarations.

:- module external.

:- interface.

:- pred p(int, int).
:- mode p(in, out) is nondet.

:- implementation.

:- pragma external_pred(p).
