:- module exists_fundeps_3.
:- interface.

:- import_module list.

:- typeclass solver_var(V) where [
        some [P, W] func var_propagators(V::ia) = (list(P)::oa) is det
            => propagator_info(P, W)
    ].

:- typeclass propagator_info(P, V) <= ((P -> V), solver_var(V)) where [].

:- type gen_solver_var
    --->    some [V] gen_solver_var(V) => solver_var(V).

:- instance solver_var(gen_solver_var).

:- implementation.

:- instance solver_var(gen_solver_var) where [
        (var_propagators(gen_solver_var(V)) = var_propagators(V))
    ].

