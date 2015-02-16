%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module typeclass_dup_method_mode.
:- interface.

:- typeclass c(T) where [
    pred p(T::in) is det,
    mode p(in) is semidet
].
