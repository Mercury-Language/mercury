%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the case of a type class containing a predicate method with no modes.
%

:- module typeclass_missing_mode_1.
:- interface.

:- typeclass c(T) where [
    pred p(T)       % error -- missing mode declaration for p/1
].
