%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module flatten_conj_bug.
:- interface.
:- import_module list.

    % A type
:- type cvar(T)
    --->    cvar(v :: T, x :: list(int)).
:- inst cvar
    --->    cvar(ground, non_empty_list).

    % A typeclass.
:- typeclass cvar_type(T) where [].

:- type cvar_wrapper
    --->    some [T] (cvar_wrapper(cvar :: cvar(T)) => cvar_type(T)).

:- some [T] func unwrap_cvar(cvar_wrapper::in) = (cvar(T)::out(cvar)) is det
    => (cvar_type(T)).

:- implementation.

    % The transformed version of this function was not able to be
    % mode checked.  The reason for this is that polymorphism was not
    % flattening conjunctions, and the conjuncts were therefore not
    % able to be scheduled in any order that would work.
    %
unwrap_cvar(cvar_wrapper(V)) = unsafe_any_to_cvar(V).

:- func unsafe_any_to_cvar(cvar(T)::in) = (cvar(T)::out(cvar)) is det.

:- pragma foreign_proc("C",
    unsafe_any_to_cvar(X::in) = (Y::out(cvar)),
    [will_not_call_mercury, promise_pure],
"
    Y = X;
").
:- pragma foreign_proc("Java",
    unsafe_any_to_cvar(X::in) = (Y::out(cvar)),
    [will_not_call_mercury, promise_pure],
"
    Y = X;
").
