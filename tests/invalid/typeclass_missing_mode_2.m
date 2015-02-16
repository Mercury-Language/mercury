%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case is similar to typeclass_missing_mode,
% except that it also includes an instance declaration.
%
% (XXX Currently mmc reports a spurious flow-on error for the
% instance declaration.  It would be nice to fix that someday.)

:- module typeclass_missing_mode_2.
:- interface.

:- typeclass c(T) where [
    pred p(T)       % error -- missing mode declaration for p/1
].

:- instance c(int).

:- implementation.

:- instance c(int) where [
    p(_) :- true
].
