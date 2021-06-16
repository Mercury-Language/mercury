%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2006, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mode_robdd.equiv_vars.m.
% Main author: dmo
%
%---------------------------------------------------------------------------%

:- module mode_robdd.equiv_vars.
:- interface.

:- import_module check_hlds.
:- import_module check_hlds.mode_constraint_robdd.

:- import_module bool.
:- import_module robdd.
:- import_module term.

%---------------------------------------------------------------------------%

:- func init_equiv_vars = equiv_vars(T).

:- func add_equality(var(T), var(T), equiv_vars(T)) = equiv_vars(T).

:- func add_equalities(vars(T), equiv_vars(T)) = equiv_vars(T).

:- pred vars_are_equivalent(equiv_vars(T)::in, var(T)::in, var(T)::in)
    is semidet.

:- pred vars_are_not_equivalent(equiv_vars(T)::in, var(T)::in, var(T)::in)
    is semidet.

:- func leader(var(T), equiv_vars(T)) = var(T) is semidet.
:- pragma type_spec(func(leader/2), T = mc_type).

:- func det_leader(var(T), equiv_vars(T)) = var(T).

:- pred empty(equiv_vars(T)::in) is semidet.

:- func equiv_vars(T) * equiv_vars(T) = equiv_vars(T).

:- func equiv_vars(T) + equiv_vars(T) = equiv_vars(T).

:- func equiv_vars(T) `difference` equiv_vars(T) = equiv_vars(T).

:- func delete(equiv_vars(T), var(T)) = equiv_vars(T).

:- func restrict_threshold(var(T), equiv_vars(T)) = equiv_vars(T).

:- func filter(pred(var(T)), equiv_vars(T)) = equiv_vars(T).
:- mode filter(pred(in) is semidet, in) = out is det.

:- pred normalise_known_equivalent_vars(bool::out, vars(T)::in, vars(T)::out,
    equiv_vars(T)::in, equiv_vars(T)::out) is det.
:- pragma type_spec(pred(normalise_known_equivalent_vars/5), T = mc_type).

:- pred label(equiv_vars(T)::in, vars(T)::in, vars(T)::out, vars(T)::in,
    vars(T)::out) is det.

:- func equivalent_vars_in_robdd(robdd(T)) = equiv_vars(T) is semidet.

:- func remove_equiv(equiv_vars(T), robdd(T)) = robdd(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module sparse_bitset.

%---------------------------------------------------------------------------%

init_equiv_vars = equiv_vars(map.init).

add_equality(VarA, VarB, EQVars0) = EQVars :-
    ( LeaderA = EQVars0 ^ leader(VarA) ->
        ( LeaderB = EQVars0 ^ leader(VarB) ->
            compare(R, LeaderA, LeaderB),
            (
                R = (=),
                EQVars = EQVars0
            ;
                R = (<),
                EQVars = add_equality_2(LeaderA, LeaderB,
                    EQVars0)
            ;
                R = (>),
                EQVars = add_equality_2(LeaderB, LeaderA,
                    EQVars0)
            )
        ;
            compare(R, LeaderA, VarB),
            (
                R = (=),
                EQVars = EQVars0
            ;
                R = (<),
                EQVars = EQVars0 ^ leader(VarB) := LeaderA
            ;
                R = (>),
                EQVars = add_equality_2(VarB, LeaderA, EQVars0)
            )
        )
    ;
        ( LeaderB = EQVars0 ^ leader(VarB) ->
            compare(R, LeaderB, VarA),
            (
                R = (=),
                EQVars = EQVars0
            ;
                R = (<),
                EQVars = EQVars0 ^ leader(VarA) := LeaderB
            ;
                R = (>),
                EQVars = add_equality_2(VarA, LeaderB, EQVars0)
            )
        ;
            compare(R, VarA, VarB),
            (
                R = (=),
                EQVars = EQVars0
            ;
                R = (<),
                EQVars = (EQVars0 ^ leader(VarB) := VarA)
                        ^ leader(VarA) := VarA
            ;
                R = (>),
                EQVars = (EQVars0 ^ leader(VarB) := VarB)
                        ^ leader(VarA) := VarB
            )
        )
    ).

:- func add_equality_2(var(T), var(T), equiv_vars(T)) = equiv_vars(T).

add_equality_2(A, B, EQVars) =
    EQVars ^ leader_map :=
        normalise_leader_map((EQVars ^ leader(B) := A) ^ leader_map).

add_equalities(Vars0, EQVars) =
    ( remove_least(Var, Vars0, Vars) ->
        foldl(add_equality(Var), Vars, EQVars)
    ;
        EQVars
    ).

vars_are_equivalent(_, V, V).
vars_are_equivalent(EQVars, VA, VB) :-
    EQVars ^ leader(VA) = EQVars ^ leader(VB).

vars_are_not_equivalent(EQVars, VA, VB) :-
    not vars_are_equivalent(EQVars, VA, VB).

leader(Var, EQVars) = EQVars ^ leader_map ^ elem(Var).

det_leader(Var, EQVars) = ( L = EQVars ^ leader(Var) -> L ; Var).

:- func 'leader :='(var(T), equiv_vars(T), var(T)) = equiv_vars(T).

'leader :='(Var0, EQVars0, Var) =
    EQVars0 ^ leader_map ^ elem(Var0) := Var.

empty(equiv_vars(LM)) :- is_empty(LM).

equiv_vars(MA) * equiv_vars(MB) = equiv_vars(M) :-
    M1 = map.foldl(func(Var, LeaderA, M0) =
        ( LeaderB = M0 ^ elem(Var) ->
            ( compare(<, LeaderA, LeaderB) ->
                M0 ^ elem(LeaderB) := LeaderA
            ; compare(<, LeaderB, LeaderA) ->
                M0 ^ elem(LeaderA) := LeaderB
            ;
                M0
            )
        ;
            M0 ^ elem(Var) := LeaderA
        ),
        MA, MB),
    M = normalise_leader_map(M1).

:- func normalise_leader_map(leader_map(T)) = leader_map(T).

normalise_leader_map(Map) =
    map.foldl(func(Var, Leader, M0) =
        ( Leader = Var ->
            M0
        ; LeaderLeader = M0 ^ elem(Leader) ->
            M0 ^ elem(Var) := LeaderLeader
        ;
            ( M0 ^ elem(Var) := Leader ) ^ elem(Leader) := Leader
        ),
        Map, map.init).

EA + EB = E :-
    VarsA = map.keys_as_set(EA ^ leader_map),
    VarsB = map.keys_as_set(EB ^ leader_map),
    Vars = set.to_sorted_list(VarsA `intersect` VarsB),
    disj_2(Vars, EA, EB, init_equiv_vars, E).

:- pred disj_2(list(var(T))::in, equiv_vars(T)::in, equiv_vars(T)::in,
    equiv_vars(T)::in, equiv_vars(T)::out) is det.

disj_2([], _, _) --> [].
disj_2([V | Vs0], EA, EB) -->
    { Match = no },
    disj_3(Vs0, Vs, V, EA^det_leader(V), EB^det_leader(V), Match, EA, EB),
    disj_2(Vs, EA, EB).

:- pred disj_3(list(var(T))::in, list(var(T))::out, var(T)::in, var(T)::in,
    var(T)::in, bool::in, equiv_vars(T)::in, equiv_vars(T)::in,
    equiv_vars(T)::in, equiv_vars(T)::out) is det.

disj_3([], [], _, _, _, _, _, _) --> [].
disj_3([V | Vs0], Vs, L, LA, LB, Match, EA, EB) -->
    (
        { EA ^ leader(V) = LA },
        { EB ^ leader(V) = LB }
    ->
        ^ leader(V) := L,
        (
            { Match = yes }
        ;
            { Match = no },
            ^ leader(L) := L
        ),
        disj_3(Vs0, Vs, L, LA, LB, yes, EA, EB)
    ;
        disj_3(Vs0, Vs1, L, LA, LB, Match, EA, EB),
        { Vs = [V | Vs1] }
    ).

% XXX I don't think this implementation of difference is correct, but it should
% work for the purpose we use it for in 'mode_robdd:+' since it never removes
% any equivalences which it shouldn't.
EA `difference` EB = E :-
    Vars = map.sorted_keys(EA ^ leader_map),
    diff_2(Vars, EA, EB, init_equiv_vars, E).

:- pred diff_2(list(var(T))::in, equiv_vars(T)::in, equiv_vars(T)::in,
    equiv_vars(T)::in, equiv_vars(T)::out) is det.

diff_2([], _, _) --> [].
diff_2([V | Vs0], EA, EB) -->
    { Match = no },
    diff_3(Vs0, Vs, V, EA^det_leader(V), EB^det_leader(V), Match, EA, EB),
    diff_2(Vs, EA, EB).

:- pred diff_3(list(var(T))::in, list(var(T))::out, var(T)::in, var(T)::in,
    var(T)::in, bool::in, equiv_vars(T)::in, equiv_vars(T)::in,
    equiv_vars(T)::in, equiv_vars(T)::out) is det.

diff_3([], [], _, _, _, _, _, _) --> [].
diff_3([V | Vs0], Vs, L, LA, LB, Match, EA, EB) -->
    (
        { EA ^ leader(V) = LA },
        { \+ EB ^ leader(V) = LB }
    ->
        ^ leader(V) := L,
        (
            { Match = yes }
        ;
            { Match = no },
            ^ leader(L) := L
        ),
        diff_3(Vs0, Vs, L, LA, LB, yes, EA, EB)
    ;
        diff_3(Vs0, Vs1, L, LA, LB, Match, EA, EB),
        { Vs = [V | Vs1] }
    ).

delete(E0, V) = E :-
    ( L = E0 ^ leader(V) ->
        ( L = V ->
            M0 = map.delete(E0 ^ leader_map, V),
            Vars = solutions.solutions(map.inverse_search(M0, V)),
            ( Vars = [NewLeader | _] ->
                M = list.foldl(
                    func(V1, M1) =
                        M1 ^ elem(V1) := NewLeader,
                    Vars, M0),
                E = equiv_vars(M)
            ;
                error("mode_robdd:equiv_vars:delete: malformed leader map")
            )
        ;
            E = equiv_vars(map.delete(E0 ^ leader_map, V))
        )
    ;
        E = E0
    ).

restrict_threshold(Th, E) = equiv_vars(normalise_leader_map(LM)) :-
    LL0 = map.to_assoc_list(E ^ leader_map),
    list.take_while((pred((V - _)::in) is semidet :-
        \+ compare(>, V, Th)
        ), LL0, LL),
    LM = map.from_assoc_list(LL).

% XXX not terribly efficient.
filter(P, equiv_vars(LM0)) = equiv_vars(LM) :-
    list.filter(P, map.keys(LM0), _, Vars),
    LM = list.foldl(func(V, M) = delete(M, V), Vars, LM0).

normalise_known_equivalent_vars(Changed, Vars0, Vars, EQVars0, EQVars) :-
    ( ( is_empty(Vars0) ; empty(EQVars0) ) ->
        Vars = Vars0,
        EQVars = EQVars0,
        Changed = no
    ;
        Vars1 = foldl(normalise_known_equivalent_vars_1(EQVars0),
            Vars0, Vars0),
        EQVars0 = equiv_vars(LeaderMap0),
        foldl(normalise_known_equivalent_vars_2, LeaderMap0,
            {no, Vars1, LeaderMap0}, {Changed, Vars, LeaderMap}),
        EQVars = equiv_vars(LeaderMap)
    ).

:- func normalise_known_equivalent_vars_1(equiv_vars(T), var(T),
    vars(T)) = vars(T).
:- pragma type_spec(func(normalise_known_equivalent_vars_1/3), T = mc_type).

normalise_known_equivalent_vars_1(EQVars, V, Vs) =
    ( L = EQVars ^ leader(V) ->
        Vs `insert` L
    ;
        Vs
    ).

:- pred normalise_known_equivalent_vars_2(var(T)::in, var(T)::in,
    {bool, vars(T), leader_map(T)}::in,
    {bool, vars(T), leader_map(T)}::out) is det.

normalise_known_equivalent_vars_2(Var, Leader, {Changed0, Vars0, LM0},
        {Changed, Vars, LM}) :-
    ( Vars0 `contains` Leader ->
        Vars = Vars0 `insert` Var,
        LM = LM0 `delete` Var,
        Changed = yes
    ;
        Vars = Vars0,
        LM = LM0,
        Changed = Changed0
    ).

label(E, True0, True, False0, False) :-
    map.foldl2(
        ( pred(V::in, L::in, T0::in, T::out, F0::in, F::out) is det :-
            ( T0 `contains` L ->
                T = T0 `sparse_bitset.insert` V,
                F = F0
            ; F0 `contains` L ->
                T = T0,
                F = F0 `sparse_bitset.insert` V
            ;
                T = T0,
                F = F0
            )
        ), E ^ leader_map, True0, True, False0, False).

equivalent_vars_in_robdd(Robdd) = LeaderMap :-
    some_vars(LeaderMap) = robdd.equivalent_vars(Robdd).

remove_equiv(EQVars, Robdd) =
    ( is_empty(EQVars ^ leader_map) ->
        Robdd
    ;
        rename_vars(func(V) = EQVars ^ det_leader(V), Robdd)
    ).

%---------------------------------------------------------------------------%
:- end_module mode_robdd.equiv_vars.
%---------------------------------------------------------------------------%
