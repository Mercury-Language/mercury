%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module higher_order5.

:- interface.

:- type nat
    --->    zero
    ;       s(nat).

:- pred definite_vars(nat::in, robdd(T)::in, vars_entailed_result(T)::out,
    vars_entailed_result(T)::out) is det.

:- type robdd(T).

:- type entailment_result(T)
    --->    all_vars
    ;       some_vars(vars :: T).

:- type vars_entailed_result(T) == entailment_result(sparse_bitset(T)).

:- type pair(T, U)
    --->    pair(T, U).

:- type sparse_bitset(T)
    --->    sparse_bitset(T).

:- type equivalent_vars_map(T)
    --->    equivalent_vars_map(pair(T, T)).

:- type equivalent_result(T) == entailment_result(equivalent_vars_map(T)).

%---------------------------------------------------------------------------%

:- implementation.

:- type robdd(T)
    --->    robdd(int).

definite_vars(zero, _, all_vars, all_vars).
definite_vars(s(N), R, T, F) :-
    definite_vars(N, id(R), T_tr, F_tr),
    T = T_tr `intersection` T_tr,
    F = F_tr `intersection` F_tr.

:- func id(T) = T.
id(T) = T.

:- type imp_res_2(T)
    --->    imps(pair(T, vars_entailed_result(T))).

:- typeclass intersectable(T) where [
    func T `intersection` T = T
].

:- instance intersectable(sparse_bitset(T)) where [
    intersection(sparse_bitset(A), _) = sparse_bitset(A)
].

:- instance intersectable(entailment_result(T)) <= intersectable(T) where [
    ( all_vars `intersection` R = R ),
    ( some_vars(Vs) `intersection` all_vars = some_vars(Vs) ),
    ( some_vars(Vs0) `intersection` some_vars(Vs1) =
        some_vars(Vs0 `intersection` Vs1) )
].

:- instance intersectable(imp_res_2(T)) where [
    imps(pair(A, ResA)) `intersection` imps(pair(_, ResB)) =
        imps(pair(A, intersection(ResA, ResB)))
].

%---------------------------------------------------------------------------%
