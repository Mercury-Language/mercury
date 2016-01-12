%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module typeclass_bad_method_mode.
:- interface.

:- typeclass c(T) where [
    pred p(T::in) is semidet,
    mode p(out) is det
].
