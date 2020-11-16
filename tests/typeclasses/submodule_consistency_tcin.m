%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module submodule_consistency_tcin.

:- interface.

:- import_module submodule_consistency_tc.

:- type b(B)
    --->    b(B).
:- type a(A)
    --->    a(A).

% The following instance caused problems because the abstract instance
% declaration was included twice in the private interface file.
:- instance tc(a(A), b(B)) <= tc(A, B).

:- implementation.

:- include_module submodule_consistency_tcin.sub.
:- import_module submodule_consistency_tcin.sub.

:- instance tc(a(A), b(B)) <= tc(A, B) where [
    ( atob(a(A), b(B), !IO) :-
        atob(A, B, !IO)
    )
].

