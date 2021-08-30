%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2006, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mode_robdd.implications.m.
% Main author: dmo.
%
%---------------------------------------------------------------------------%

:- module mode_robdd.implications.
:- interface.

:- import_module bool.
:- import_module robdd.
:- import_module term.

%---------------------------------------------------------------------------%

:- func init_imp_vars = imp_vars(T).

:- func imp_vars(T) * imp_vars(T) = imp_vars(T).

:- func imp_vars(T) + imp_vars(T) = imp_vars(T).

:- func imp_vars(T) `difference` imp_vars(T) = imp_vars(T).

:- func imp_vars(T) `delete` var(T) = imp_vars(T).

:- func restrict_threshold(var(T), imp_vars(T)) = imp_vars(T).

:- func filter(pred(var(T)), imp_vars(T)) = imp_vars(T).
:- mode filter(pred(in) is semidet, in) = out is det.

:- func neq_vars(var(T), var(T), imp_vars(T)) = imp_vars(T).

:- func imp_vars(var(T), var(T), imp_vars(T)) = imp_vars(T).

:- func at_most_one_of(vars(T), imp_vars(T)) = imp_vars(T).

:- func not_both(var(T), var(T), imp_vars(T)) = imp_vars(T).

:- func either(var(T), var(T), imp_vars(T)) = imp_vars(T).

:- pred normalise_true_false_implication_vars(bool::out, vars(T)::in,
    vars(T)::out, vars(T)::in, vars(T)::out,
    imp_vars(T)::in, imp_vars(T)::out) is det.

:- pred propagate_equivalences_into_implications(equiv_vars(T)::in, bool::out,
    imp_vars(T)::in, imp_vars(T)::out) is semidet.

:- pred propagate_implications_into_equivalences(bool::out, equiv_vars(T)::in,
    equiv_vars(T)::out, imp_vars(T)::in, imp_vars(T)::out) is det.

:- pred extract_implication_vars_from_robdd(bool::out, robdd(T)::in,
    robdd(T)::out, imp_vars(T)::in, imp_vars(T)::out) is det.

:- func add_equalities_to_imp_vars(equiv_vars(T), imp_vars(T)) = imp_vars(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mode_robdd.equiv_vars.

:- import_module assoc_list.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module sparse_bitset.
:- import_module string.

%---------------------------------------------------------------------------%

init_imp_vars = imp_vars(init, init, init, init).

ImpVars * imp_vars(Imps, RevImps, DisImps, RevDisImps) =
    ImpVars ^ add_imp_map_clauses(mkneg, mkpos, Imps)
        ^ add_imp_map_clauses(mkpos, mkneg, RevImps)
        ^ add_imp_map_clauses(mkneg, mkneg, DisImps)
        ^ add_imp_map_clauses(mkpos, mkpos, RevDisImps).

ImpVarsA + ImpVarsB =
    apply_to_coresp_imp_maps(intersect, ImpVarsA, ImpVarsB).

ImpVarsA `difference` ImpVarsB =
    apply_to_coresp_imp_maps(imp_map_difference, ImpVarsA, ImpVarsB).

ImpVars `delete` Var =
    apply_to_imp_maps(delete_var_from_imp_map(Var), ImpVars).
    % XXX could make this more efficient by using backwards relations.

restrict_threshold(Threshold, ImpVars) =
    apply_to_imp_maps(func(IM) =
        restrict_threshold_2(Threshold, to_assoc_list(IM), init),
        ImpVars).

:- func restrict_threshold_2(var(T), assoc_list(var(T), vars(T)), imp_map(T))
    = imp_map(T).

restrict_threshold_2(_Threshold, [], IM) = IM.
restrict_threshold_2(Threshold, [V - Vs | AL], IM) =
    ( compare((>), V, Threshold) ->
        IM
    ;
        restrict_threshold_2(Threshold, AL,
            IM ^ entry(V) := remove_gt(Vs, Threshold))
    ).

filter(P, ImpVars) = apply_to_imp_maps(filter_imp_map(P), ImpVars).

:- func filter_imp_map(pred(var(T)), imp_map(T)) = imp_map(T).
:- mode filter_imp_map(pred(in) is semidet, in) = out is det.

filter_imp_map(P, IM) =
    map.foldl(func(V, Vs, M) =
        ( P(V) ->
            M ^ entry(V) := filter(P, Vs)
        ;
            M `delete` V
        ),
        IM, IM).

neq_vars(A, B, ImpVars) =
    ImpVars ^ add_clause({neg(A), neg(B)}) ^ add_clause({pos(A), pos(B)}).

imp_vars(A, B, ImpVars) =
    ImpVars ^ add_clause({neg(A), pos(B)}).

at_most_one_of(Vars0, ImpVars) =
    ( remove_least(Var, Vars0, Vars) ->
        ImpVars ^ foldl(not_both(Var), Vars) ^ at_most_one_of(Vars)
    ;
        ImpVars
    ).

not_both(A, B, ImpVars) =
    ImpVars ^ add_clause({neg(A), neg(B)}).

either(A, B, ImpVars) =
    ImpVars ^ add_clause({pos(A), pos(B)}).

normalise_true_false_implication_vars(Changed, TrueVars0, TrueVars,
        FalseVars0, FalseVars, ImpVars0, ImpVars) :-
    (
        is_empty(TrueVars0),
        is_empty(FalseVars0)
    ->
        TrueVars = TrueVars0,
        FalseVars = FalseVars0,
        ImpVars = ImpVars0,
        Changed = no
    ;
        ImpVars0 = imp_vars(Imps0, RevImps0, DisImps0, RevDisImps0),
        normalise_true_false_imp_map(no, Changed0, TrueVars0, TrueVars1,
            FalseVars0, FalseVars1, Imps0, Imps),
        normalise_true_false_imp_map(no, Changed1, FalseVars1, FalseVars2,
            TrueVars1, TrueVars2, RevImps0, RevImps),
        normalise_true_false_imp_map(yes, Changed2, FalseVars2, FalseVars3,
            TrueVars2, TrueVars3, DisImps0, DisImps),
        normalise_true_false_imp_map(yes, Changed3, TrueVars3, TrueVars4,
            FalseVars3, FalseVars4, RevDisImps0, RevDisImps),

        normalise_pairs(keys, Imps, DisImps, Changed4,
            FalseVars4, FalseVars5),
        normalise_pairs(keys, RevImps, RevDisImps, Changed5,
            TrueVars4, TrueVars5),
        normalise_pairs(values, RevImps, DisImps, Changed6,
            FalseVars5, FalseVars6),
        normalise_pairs(values, Imps, RevDisImps, Changed7,
            TrueVars5, TrueVars6),

        ImpVars6 = imp_vars(Imps, RevImps, DisImps, RevDisImps),
        Changed = (Changed0 or Changed1 or Changed2 or Changed3 or
                Changed4 or Changed5 or Changed6 or Changed7),

        (
            Changed = yes,
            normalise_true_false_implication_vars(_, TrueVars6, TrueVars,
                FalseVars6, FalseVars, ImpVars6, ImpVars)
        ;
            Changed = no,
            TrueVars = TrueVars6,
            FalseVars = FalseVars6,
            ImpVars = ImpVars6
        )
    ).

:- pred normalise_true_false_imp_map(bool::in, bool::out,
    vars(T)::in, vars(T)::out, vars(T)::in, vars(T)::out,
    imp_map(T)::in, imp_map(T)::out) is det.

normalise_true_false_imp_map(IsDisImp, Changed, TrueVars0, TrueVars,
        FalseVars0, FalseVars, ImpMap0, ImpMap) :-
    {TrueVars, FalseVars, ImpMap, Changed} = map.foldl(
        ( func(V, Vs, {Ts0, Fs0, IMs0, C0}) = {Ts, Fs, IMs, C} :-
            (
                ( IsDisImp = yes -> Fs0 ; Ts0 ) `contains` V
            ->
                Ts = Ts0 `union` Vs,
                Fs = Fs0,
                IMs = IMs0 `delete` V,
                C = yes
            ;
                ( IsDisImp = yes -> Ts0 ; Fs0 ) `contains` V
            ->
                Ts = Ts0,
                Fs = Fs0,
                IMs = IMs0 `delete` V,
                C = yes
            ;
                FVs = Fs0 `intersect` Vs,
                is_non_empty(FVs)
            ->
                Ts = ( IsDisImp = yes -> Ts0 `insert` V ; Ts0 ),
                Fs = ( IsDisImp = yes -> Fs0 ; Fs0 `insert` V ),
                IMs = IMs0 `delete` V,
                C = yes
            ;
                TVs = Ts0 `intersect` Vs,
                is_non_empty(TVs)
            ->
                Ts = Ts0,
                Fs = Fs0,
                UTVs = Vs `difference` TVs,
                ( is_empty(UTVs) ->
                    IMs = IMs0 `delete` V
                ;
                    IMs = IMs0 ^ elem(V) := UTVs
                ),
                C = yes
            ;
                Ts = Ts0,
                Fs = Fs0,
                IMs = IMs0,
                C = C0
            )
        ), ImpMap0, {TrueVars0, FalseVars0, ImpMap0, no}).

:- type extract ---> keys ; values.

:- pred normalise_pairs(extract::in, imp_map(T)::in, imp_map(T)::in, bool::out,
    vars(T)::in, vars(T)::out) is det.

normalise_pairs(Extract, Imps, DisImps, Changed, FalseVars0, FalseVars) :-
    Intersect = Imps `intersect` DisImps,
    ( is_empty(Intersect) ->
        Changed = no,
        FalseVars = FalseVars0
    ;
        Changed = yes,
        (
            Extract = keys,
            FalseVars = FalseVars0 `union`
                Intersect ^ sorted_keys ^ sorted_list_to_set
        ;
            Extract = values,
            Values = list.foldl(union, Intersect ^ values, init),
            FalseVars = FalseVars0 `union` Values
        )
    ).

%------------------------------------------------------------------------%

propagate_equivalences_into_implications(EQVars, Changed, ImpVars0, ImpVars) :-
    ImpVars0 = imp_vars(Imps0, RevImps0, DisImps, RevDisImps),

    propagate_equivalences_into_disimplications(EQVars, DisImps,
        RevDisImps),

    propagate_equivalences_into_implications_2(EQVars, Changed0,
        Imps0, Imps),
    propagate_equivalences_into_implications_2(EQVars, Changed1,
        RevImps0, RevImps),

    ImpVars = imp_vars(Imps, RevImps, DisImps, RevDisImps),
    Changed = Changed0 `bool.or` Changed1.

:- pred propagate_equivalences_into_implications_2(equiv_vars(T)::in,
    bool::out, imp_map(T)::in, imp_map(T)::out) is det.

propagate_equivalences_into_implications_2(EQVars, Changed, ImpMap0, ImpMap) :-
    {ImpMap, Changed} = map.foldl((func(V, Vs0, {IM, C}) =
        { IM ^ entry(V) := Vs, ( Vs = Vs0 -> C ; yes ) } :-
        Vs = filter(vars_are_not_equivalent(EQVars, V), Vs0)
        ), ImpMap0, {init, no}).

:- pred propagate_equivalences_into_disimplications(equiv_vars(T)::in,
    imp_map(T)::in, imp_map(T)::in) is semidet.

propagate_equivalences_into_disimplications(EQVars, DisImps, RevDisImps) :-
    ImpMap = DisImps `intersect` RevDisImps,
    ( all [VA, VB]
        % XXX this could be very slow.  May want to do it more
        % efficiently.
        ( member(ImpMap, VA, Vs), member(VB, Vs) )
    =>
        vars_are_not_equivalent(EQVars, VA, VB)
    ).

%------------------------------------------------------------------------%

% Look for occurrences of A => B and A <= B and replace then by A <=> B.
propagate_implications_into_equivalences(Changed, EQVars0, EQVars,
        ImpVars0, ImpVars) :-
    ImpVars0 = imp_vars(Imps0, RevImps0, DisImps, RevDisImps),

    ( ( is_empty(Imps0) ; is_empty(RevImps0) ) ->
        Changed = no,
        EQVars = EQVars0,
        ImpVars = ImpVars0
    ;
        {Changed, EQVars, Imps, RevImps} = foldl(
        ( func(V, IVs, {C0, E0, I0, R0}) = {C, E, I, R} :-
            (
                RVs = R0 ^ elem(V),
                EVs = IVs `intersect` RVs,
                is_non_empty(EVs)
            ->
                C = yes,
                E = add_equalities(EVs `insert` V, E0),
                I = I0 ^ entry(V) := IVs `difference` RVs,
                R = R0 ^ entry(V) := RVs `difference` IVs
            ;
                C = C0, E = E0, I = I0, R = R0
            )
        ), Imps0, {no, EQVars0, Imps0, RevImps0}),
        ImpVars = imp_vars(Imps, RevImps, DisImps, RevDisImps)
    ).

%------------------------------------------------------------------------%

extract_implication_vars_from_robdd(Changed, Robdd0, Robdd,
        ImpVars0, ImpVars) :-
    % ImpVars1 = add_backwards_relations(extract_implications(Robdd0)),
    ImpVars1 = extract_implications(Robdd0),
    ImpVars = ImpVars0 * ImpVars1,

    Robdd = remove_implications(ImpVars, Robdd0),

    trace [compile_time(flag("debug_mode_implications")), io(!IO)] (
        VarToString = (func(V) = string.int_to_string(var_to_int(V))),
        robdd_to_dot(Robdd0, VarToString, "extract_impl_before.dot", !IO),
        robdd_to_dot(Robdd, VarToString, "extract_impl_after.dot", !IO)
    ),

    Changed = ( Robdd = Robdd0, empty(ImpVars1) -> no ; yes ).

%------------------------------------------------------------------------%

add_equalities_to_imp_vars(EQVars, ImpVars) =
    map.foldl(func(VA, VB, IVs) =
        IVs ^ imp_vars(VA, VB) ^ imp_vars(VB, VA),
        EQVars ^ leader_map, ImpVars).

%------------------------------------------------------------------------%

:- func entry(var(T), imp_map(T)) = vars(T).

entry(V, M) =
    ( Vs = M ^ elem(V) ->
        Vs
    ;
        init
    ).

:- func 'entry :='(var(T), imp_map(T), vars(T)) = imp_map(T).

'entry :='(V, M, Vs) =
    ( is_empty(Vs) ->
        M `delete` V
    ;
        M ^ elem(V) := Vs
    ).

:- func 'new_relation :='(var(T), imp_map(T), var(T)) = imp_map(T).

'new_relation :='(VA, M, VB) =
    ( Vs = M ^ elem(VA) ->
        M ^ elem(VA) := Vs `insert` VB
    ;
        M ^ elem(VA) := make_singleton_set(VB)
    ).

:- func 'maybe_new_relation :='(var(T), imp_map(T), var(T)) = imp_map(T)
        is semidet.

'maybe_new_relation :='(VA, M0, VB) = M :-
    ( Vs = M0 ^ elem(VA) ->
        \+ ( Vs `contains` VB ),
        M = M0 ^ elem(VA) := Vs `insert` VB
    ;
        M = M0 ^ elem(VA) := make_singleton_set(VB)
    ).

:- pred empty(imp_vars(T)::in) is semidet.

empty(imp_vars(I, RI, DI, RDI)) :-
    is_empty(I),
    is_empty(RI),
    is_empty(DI),
    is_empty(RDI).

%------------------------------------------------------------------------%

:- func apply_to_imp_maps(func(imp_map(T)) = imp_map(T), imp_vars(T)) =
        imp_vars(T).

apply_to_imp_maps(F, ImpVars0) = ImpVars :-
    ImpVars0 = imp_vars(I, RI, DI, RDI),
    ImpVars = imp_vars(F(I), F(RI), F(DI), F(RDI)).

:- func apply_to_coresp_imp_maps(func(imp_map(T), imp_map(T)) = imp_map(T),
    imp_vars(T), imp_vars(T)) = imp_vars(T).

apply_to_coresp_imp_maps(F, ImpVarsA, ImpVarsB) = ImpVars :-
    ImpVarsA = imp_vars(IA, RIA, DIA, RDIA),
    ImpVarsB = imp_vars(IB, RIB, DIB, RDIB),
    ImpVars = imp_vars(F(IA, IB), F(RIA, RIB), F(DIA, DIB), F(RDIA, RDIB)).

:- func imp_map(T) `intersect` imp_map(T) = imp_map(T).

IMA `intersect` IMB = remove_empty_sets(intersect(intersect, IMA, IMB)).

:- func imp_map(T) `imp_map_difference` imp_map(T) = imp_map(T).

IMA `imp_map_difference` IMB =
    ( is_empty(IMA) ->
        IMA
    ;
        map.foldl(func(V, VsB, M) =
            ( VsA = M ^ elem(V) ->
                M ^ entry(V) := VsA `difference` VsB
            ;
                M
            ),
            IMB, IMA)
    ).

:- func remove_empty_sets(imp_map(T)) = imp_map(T).

remove_empty_sets(IM) =
    map.foldl(func(V, Vs, M) =
        ( is_empty(Vs) ->
            M `delete` V
        ;
            M
        ),
        IM, IM).

:- func delete_var_from_imp_map(var(T), imp_map(T)) = imp_map(T).

delete_var_from_imp_map(Var, IM0) =
    map.foldl(func(V, Vs, M) =
        ( Vs `contains` Var ->
            M ^ entry(V) := Vs `delete` Var
        ;
            M
        ),
        IM1, IM1) :-
    IM1 = IM0 `delete` Var.

%------------------------------------------------------------------------%

:- func add_backwards_relations(imp_vars(T)) = imp_vars(T).
:- pragma consider_used(func(add_backwards_relations/1)).

add_backwards_relations(imp_vars(Is0, RIs0, DIs0, RDIs0)) =
    imp_vars(
        add_backwards_to_imp_map(Is0, RIs0),
        add_backwards_to_imp_map(RIs0, Is0),
        add_backwards_to_imp_map(DIs0, DIs0),
        add_backwards_to_imp_map(RDIs0, RDIs0)
    ).

:- func add_backwards_to_imp_map(imp_map(T), imp_map(T)) = imp_map(T).

add_backwards_to_imp_map(IM, RIM) =
    map.foldl(func(VA, Vs, M0) =
        foldl(func(VB, M1) =
            M1 ^ new_relation(VB) := VA,
            Vs, M0),
        RIM, IM).

%------------------------------------------------------------------------%

:- type mklit(T) == (func(var(T)) = literal(T)).

:- func mkpos(var(T)) = literal(T).

mkpos(V) = pos(V).

:- func mkneg(var(T)) = literal(T).

mkneg(V) = neg(V).

:- type bin_clause(T) == { literal(T), literal(T) }.

:- func add_clause(bin_clause(T), imp_vars(T)) = imp_vars(T).

add_clause(Clause, ImpVars) =
    add_clauses([Clause], ImpVars).

:- func add_clauses(list(bin_clause(T)), imp_vars(T)) = imp_vars(T).

add_clauses([], ImpVars) = ImpVars.
add_clauses([Clause | Clauses], ImpVars0) = ImpVars :-
    ( ImpVars1 = add_clause_2(Clause, ImpVars0) ->
        Resolvents = get_resolvents(Clause, ImpVars1),
        ImpVars = add_clauses(Resolvents ++ Clauses, ImpVars1)
    ;
        ImpVars = add_clauses(Clauses, ImpVars0)
    ).

    % Add a new clause to the imp_vars. Fail if the clause is already present.
    %
:- func add_clause_2(bin_clause(T), imp_vars(T)) = imp_vars(T) is semidet.

add_clause_2({neg(VA), pos(VB)}, ImpVars) =
    (ImpVars ^ imps     ^ maybe_new_relation(VA) := VB)
         ^ rev_imps ^ maybe_new_relation(VB) := VA.
add_clause_2({pos(VA), neg(VB)}, ImpVars) =
    (ImpVars ^ rev_imps ^ maybe_new_relation(VA) := VB)
         ^ imps     ^ maybe_new_relation(VB) := VA.
add_clause_2({neg(VA), neg(VB)}, ImpVars) =
    (ImpVars ^ dis_imps ^ maybe_new_relation(VA) := VB)
         ^ dis_imps ^ maybe_new_relation(VB) := VA.
add_clause_2({pos(VA), pos(VB)}, ImpVars) =
    (ImpVars ^ rev_dis_imps ^ maybe_new_relation(VA) := VB)
         ^ rev_dis_imps ^ maybe_new_relation(VB) := VA.

:- func get_resolvents(bin_clause(T), imp_vars(T)) = list(bin_clause(T)).

get_resolvents({LitA, LitB}, ImpVars) =
    get_resolvents_2(LitA, LitB, ImpVars) ++
    get_resolvents_2(LitB, LitA, ImpVars).

:- func get_resolvents_2(literal(T), literal(T), imp_vars(T)) =
        list(bin_clause(T)).

get_resolvents_2(LitA, LitB, ImpVars) = Clauses :-
    Literals = get_literals(LitA, ImpVars),
    Clauses = list.map(func(NewLit) = {NewLit, LitB}, Literals).

:- func get_literals(literal(T), imp_vars(T)) = list(literal(T)).

get_literals(LitA, ImpVars) =
        map(mkpos, to_sorted_list(Pos)) ++
        map(mkneg, to_sorted_list(Neg)) :-
    (
        LitA = pos(VarA),
        Pos = ImpVars ^ imps ^ entry(VarA),
        Neg = ImpVars ^ dis_imps ^ entry(VarA)
    ;
        LitA = neg(VarA),
        Pos = ImpVars ^ rev_dis_imps ^ entry(VarA),
        Neg = ImpVars ^ rev_imps ^ entry(VarA)
    ).

:- func add_imp_map_clauses(mklit(T), mklit(T), imp_map(T), imp_vars(T)) =
        imp_vars(T).

add_imp_map_clauses(MkLitA, MkLitB, IM, ImpVars) =
    map.foldl(func(VarA, Vars, IVs0) =
        foldl(func(VarB, IVs1) =
            add_clause({MkLitA(VarA), MkLitB(VarB)}, IVs1),
            Vars, IVs0),
        IM, ImpVars).

%---------------------------------------------------------------------------%
:- end_module mode_robdd.implications.
%---------------------------------------------------------------------------%
