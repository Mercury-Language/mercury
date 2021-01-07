%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for term_norm.m
% Symptom: "Software Error: Unmatched lists in functor_norm_filter_args."
% This was caused by the list of counted arguments in the weight table differing
% from the list of arguments the termination analyser provided when it called
% functor norm.  The code that constructed the weight table was ignoring
% type_infos when constructing the list of counted arguments.

:- module existential_error1.

:- interface.

:- type univ
    --->    some [T] univ_cons(T).

:- pred deconstruct_univ(univ::in, T::out) is semidet.

:- implementation.

deconstruct_univ(Univ, T) :-
    Univ = univ_cons(T0),
    private_builtin.typed_unify(T0, T).

:- end_module existential_error1.
