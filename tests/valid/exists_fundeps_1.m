%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module exists_fundeps_1.
:- interface.
:- import_module list.

:- typeclass solver_var(V) where [
    some [P, W] func var_propagators(V::ia) = (list(P)::oa) is det
        => propagator_info(P, W)
].

:- typeclass propagator_info(P, V) <= ((P -> V), solver_var(V)) where [].

:- type no_info
    --->    no_info.

:- instance solver_var(no_info).
:- instance propagator_info(no_info, no_info).

:- implementation.

:- instance solver_var(no_info) where [
    (var_propagators(_) = [] `with_type` list(no_info))
].

:- instance propagator_info(no_info, no_info) where [].
