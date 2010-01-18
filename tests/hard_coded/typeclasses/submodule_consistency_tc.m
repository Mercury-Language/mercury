:- module submodule_consistency_tc.
:- interface.

:- import_module io.

:- typeclass tc(A, B) <= ((A -> B)) where 
[
    pred atob(A::in, B::out, io::di, io::uo) is det
].
