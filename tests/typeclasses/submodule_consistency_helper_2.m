%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module submodule_consistency_helper_2.
:- interface.

:- import_module io.

:- typeclass tc(A, B) <= ((A -> B)) where [
    pred atob(A::in, B::out, io::di, io::uo) is det
].
