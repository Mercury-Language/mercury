%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mode_robdd.tfeirn.m.
% Main author: dmo
% Stability: low
%
%-----------------------------------------------------------------------------%

:- module mode_robdd.tfeirn.
:- interface.

:- import_module check_hlds.
:- import_module check_hlds.mode_constraint_robdd.

:- import_module bool.
:- import_module robdd.
:- import_module term.

%-----------------------------------------------------------------------------%

:- type tfeirn(T).
:- type tfeirn == tfeirn(generic).

:- inst tfeirn == ground. % XXX
:- inst norm_tfeirn for tfeirn/1
    --->    mode_robdd(ground, ground, ground, ground, ground, bound(yes)).

:- mode di_tfeirn == in. % XXX
:- mode uo_tfeirn == out. % XXX

:- mode ni_tfeirn == in(norm_tfeirn).
:- mode no_tfeirn == out(norm_tfeirn).

%-----------------------------------------------------------------------------%

% Constants.
:- func one = (tfeirn(T)::no_tfeirn) is det.
:- pragma type_spec(func(one/0), T = mc_type).

:- func zero = (tfeirn(T)::no_tfeirn) is det.
:- pragma type_spec(func(zero/0), T = mc_type).

%-----------------------------------------------------------------------------%

% Conjunction.
:- func tfeirn(T) * tfeirn(T) = tfeirn(T).
:- pragma type_spec(func(('*')/2), T = mc_type).

% Disjunction.
:- func tfeirn(T) + tfeirn(T) = tfeirn(T).
:- pragma type_spec(func(('+')/2), T = mc_type).

%-----------------------------------------------------------------------------%

:- func var(var(T)::in, tfeirn(T)::in(tfeirn)) = (tfeirn(T)::out(tfeirn))
    is det.
:- pragma type_spec(func(var/2), T = mc_type).

:- func not_var(var(T)::in, tfeirn(T)::in(tfeirn)) = (tfeirn(T)::out(tfeirn))
    is det.
:- pragma type_spec(func(not_var/2), T = mc_type).

:- func eq_vars(var(T)::in, var(T)::in, tfeirn(T)::di_tfeirn) =
    (tfeirn(T)::uo_tfeirn) is det.
:- pragma type_spec(func(eq_vars/3), T = mc_type).

:- func neq_vars(var(T)::in, var(T)::in, tfeirn(T)::di_tfeirn) =
    (tfeirn(T)::uo_tfeirn) is det.
:- pragma type_spec(func(neq_vars/3), T = mc_type).

:- func imp_vars(var(T)::in, var(T)::in, tfeirn(T)::di_tfeirn) =
    (tfeirn(T)::uo_tfeirn) is det.
:- pragma type_spec(func(imp_vars/3), T = mc_type).

:- func conj_vars(vars(T)::in, tfeirn(T)::di_tfeirn) = (tfeirn(T)::uo_tfeirn)
    is det.
:- pragma type_spec(func(conj_vars/2), T = mc_type).

:- func conj_not_vars(vars(T)::in, tfeirn(T)::di_tfeirn) =
    (tfeirn(T)::uo_tfeirn) is det.
:- pragma type_spec(func(conj_not_vars/2), T = mc_type).

:- func disj_vars(vars(T)::in, tfeirn(T)::di_tfeirn) = (tfeirn(T)::uo_tfeirn)
    is det.
:- pragma type_spec(func(disj_vars/2), T = mc_type).

    % disj_vars_eq(Vars, Var) <=> (disj_vars(Vars) =:= Var).
:- func disj_vars_eq(vars(T)::in, var(T)::in, tfeirn(T)::di_tfeirn) =
    (tfeirn(T)::uo_tfeirn) is det.
:- pragma type_spec(func(disj_vars_eq/3), T = mc_type).

%-----------------------------------------------------------------------------%

:- func at_most_one_of(vars(T)::in, tfeirn(T)::di_tfeirn) =
    (tfeirn(T)::uo_tfeirn) is det.
:- pragma type_spec(func(at_most_one_of/2), T = mc_type).

:- func not_both(var(T)::in, var(T)::in, tfeirn(T)::di_tfeirn) =
    (tfeirn(T)::uo_tfeirn) is det.
:- pragma type_spec(func(not_both/3), T = mc_type).

:- func io_constraint(var(T)::in, var(T)::in, var(T)::in, tfeirn(T)::di_tfeirn)
    = (tfeirn(T)::uo_tfeirn) is det.
:- pragma type_spec(func(io_constraint/4), T = mc_type).

%-----------------------------------------------------------------------------%

    % Succeed iff the var is entailed by the xROBDD.
:- pred var_entailed(tfeirn(T)::ni_tfeirn, var(T)::in) is semidet.
:- pragma type_spec(pred(var_entailed/2), T = mc_type).

    % Return the set of vars entailed by the xROBDD.
:- func vars_entailed(tfeirn(T)::ni_tfeirn) =
    (vars_entailed_result(T)::out) is det.
:- pragma type_spec(func(vars_entailed/1), T = mc_type).

    % Return the set of vars disentailed by the xROBDD.
:- func vars_disentailed(tfeirn(T)::ni_tfeirn) =
    (vars_entailed_result(T)::out) is det.
:- pragma type_spec(func(vars_disentailed/1), T = mc_type).

%-----------------------------------------------------------------------------%

:- pred known_vars(tfeirn(T)::ni_tfeirn, vars(T)::out, vars(T)::out) is det.
:- pragma type_spec(pred(known_vars/3), T = mc_type).

%-----------------------------------------------------------------------------%

    % Existentially quantify away the var in the xROBDD.
:- func restrict(var(T)::in, tfeirn(T)::ni_tfeirn) =
    (tfeirn(T)::no_tfeirn) is det.
:- pragma type_spec(func(restrict/2), T = mc_type).

    % Existentially quantify away all vars greater than the specified var.
:- func restrict_threshold(var(T)::in, tfeirn(T)::ni_tfeirn) =
    (tfeirn(T)::no_tfeirn) is det.
:- pragma type_spec(func(restrict_threshold/2), T = mc_type).

:- func restrict_filter(pred(var(T))::in(pred(in) is semidet),
    tfeirn(T)::ni_tfeirn) = (tfeirn(T)::no_tfeirn) is det.
:- pragma type_spec(func(restrict_filter/2), T = mc_type).

:- func var_restrict_true(var(T)::in, tfeirn(T)::di_tfeirn) =
    (tfeirn(T)::uo_tfeirn) is det.
:- pragma type_spec(func(var_restrict_true/2), T = mc_type).

:- func var_restrict_false(var(T)::in, tfeirn(T)::di_tfeirn) =
    (tfeirn(T)::uo_tfeirn) is det.
:- pragma type_spec(func(var_restrict_false/2), T = mc_type).

%-----------------------------------------------------------------------------%

    % labelling(Vars, xROBDD, TrueVars, FalseVars)
    %   Takes a set of Vars and an xROBDD and returns a value assignment
    %   for those Vars that is a model of the Boolean function
    %   represented by the xROBDD.
    %   The value assignment is returned in the two sets TrueVars (set
    %   of variables assigned the value 1) and FalseVars (set of
    %   variables assigned the value 0).
    %
    % XXX should try using sparse_bitset here.
:- pred labelling(vars(T)::in, tfeirn(T)::in, vars(T)::out, vars(T)::out)
    is nondet.

    % minimal_model(Vars, xROBDD, TrueVars, FalseVars)
    %   Takes a set of Vars and an xROBDD and returns a value assignment
    %   for those Vars that is a minimal model of the Boolean function
    %   represented by the xROBDD.
    %   The value assignment is returned in the two sets TrueVars (set
    %   of variables assigned the value 1) and FalseVars (set of
    %   variables assigned the value 0).
    %
    % XXX should try using sparse_bitset here.
:- pred minimal_model(vars(T)::in, tfeirn(T)::in, vars(T)::out, vars(T)::out)
    is nondet.

%-----------------------------------------------------------------------------%

:- func ensure_normalised(tfeirn(T)::di_tfeirn) = (tfeirn(T)::no_tfeirn)
    is det.
:- pragma type_spec(func(ensure_normalised/1), T = mc_type).

%-----------------------------------------------------------------------------%

% XXX
% Extract the ROBDD component of the tfeirn.
:- func robdd(tfeirn(T)) = robdd(T).

% Convert the tfeirn to an ROBDD.
:- func to_robdd(tfeirn(T)) = robdd(T).

%-----------------------------------------------------------------------------%

:- func robdd_to_mode_robdd(robdd(T)) = tfeirn(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mode_robdd.equiv_vars.
:- import_module mode_robdd.implications.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module sparse_bitset.

% T - true vars, F - False Vars, E - equivalent vars, N -
% non-equivalent vars, I - implications, R - ROBDD.
%
% Combinations to try:
%   R   (straight ROBDD)
%   TFR
%   TER (Peter Schachte's extension)
%   tfeirn
%   TFENIR

:- interface.

    % This should be abstract, but needs to be exported for insts.
    %
:- type tfeirn(T)
    --->    mode_robdd(
            true_vars :: vars(T),
            false_vars :: vars(T),
            equiv_vars :: equiv_vars(T),
            imp_vars :: imp_vars(T),
            robdd :: robdd(T),
            normalised :: bool
        ).

:- implementation.

%-----------------------------------------------------------------------------%

one = mode_robdd(init, init, init_equiv_vars, init_imp_vars, one, yes).

zero = mode_robdd(init, init, init_equiv_vars, init_imp_vars, zero, yes).

%-----------------------------------------------------------------------------%

mode_robdd(TA, FA, EA, IA, RA, _) * mode_robdd(TB, FB, EB, IB, RB, _)
        = ROBDD :-
    TU = TA `union` TB,
    FU = FA `union` FB,
    EU = EA * EB,
    IU = IA * IB,
    mode_robdd(TA1, FA1, EA1, IA1, RA1, _) =
        normalise(mode_robdd(TU, FU, EU, IU, RA, no)),
    mode_robdd(TB1, FB1, EB1, IB1, RB1, _) =
        normalise(mode_robdd(TU, FU, EU, IU, RB, no)),
    ROBDD = mode_robdd(TA1 `union` TB1, FA1 `union` FB1,
        EA1 * EB1, IA1 * IB1, RA1 * RB1, no).

mode_robdd(TA0, FA0, EA0, IA0, RA0, NA0)
        + mode_robdd(TB0, FB0, EB0, IB0, RB0, NB0) = X :-
    ( if RA0 = zero then
        X = mode_robdd(TB0, FB0, EB0, IB0, RB0, NB0)
    else if RB0 = zero then
        X = mode_robdd(TA0, FA0, EA0, IA0, RA0, NA0)
    else
        X = mode_robdd(T, F, E, I, R, no),
        T = TA0 `intersect` TB0,
        F = FA0 `intersect` FB0,
        E = EA + EB,
        I = IA + IB,
        R = RA + RB,

        TAB = TA0 `difference` TB0,
        FAB = FA0 `difference` FB0,
        EA = EA0 ^ add_equalities(TAB) ^ add_equalities(FAB),

        TBA = TB0 `difference` TA0,
        FBA = FB0 `difference` FA0,
        EB = EB0 ^ add_equalities(TBA) ^ add_equalities(FBA),

        EAB = EA `difference` EB,
        IA = IA0 ^ add_equalities_to_imp_vars(EAB),

        EBA = EB `difference` EA,
        IB = IB0 ^ add_equalities_to_imp_vars(EBA),

        RA1 = foldl(
            func(V, R0) = R0 * var(E ^ det_leader(V)), TAB, RA0),
        RA2 = foldl(
            func(V, R0) = R0 *
                not_var(E ^ det_leader(V)), FAB, RA1),
        EA1 = (EA `difference` EB) + EA0,
        RA3 = add_equivalences(EA1, RA2),
        IA1 = (IA `difference` IB) + IA0,
        RA = add_implications(IA1, RA3),

        RB1 = foldl(
            func(V, R0) = R0 * var(E ^ det_leader(V)), TBA, RB0),
        RB2 = foldl(
            func(V, R0) = R0 *
                not_var(E ^ det_leader(V)), FBA, RB1),
        EB1 = (EB `difference` EA) + EB0,
        RB3 = add_equivalences(EB1, RB2),
        IB1 = (IB `difference` IA) + IB0,
        RB = add_implications(IB1, RB3)
    ).

%-----------------------------------------------------------------------------%

var(V, X) =
    ( if T `contains` V then
        X
    else if F `contains` V then
        zero
    else
        mode_robdd(T `insert` V, F, E, I, R, no)
    ) :-
    X = mode_robdd(T, F, E, I, R, _).

not_var(V, X) =
    ( if F `contains` V then
        X
    else if T `contains` V then
        zero
    else
        mode_robdd(T, F `insert` V, E, I, R, no)
    ) :-
    X = mode_robdd(T, F, E, I, R, _).

eq_vars(VarA, VarB, X) =
    ( if
        ( T `contains` VarA, T `contains` VarB
        ; F `contains` VarA, F `contains` VarB
        )
    then
        X
    else if
        ( T `contains` VarA, F `contains` VarB
        ; F `contains` VarA, T `contains` VarB
        )
    then
        zero
    else
        mode_robdd(T, F, add_equality(VarA, VarB, E), I, R, no)
    ) :-
    X = mode_robdd(T, F, E, I, R, _).

neq_vars(VarA, VarB, X) =
    ( if
        ( T `contains` VarA, T `contains` VarB
        ; F `contains` VarA, F `contains` VarB
        )
    then
        zero
    else if
        ( T `contains` VarA, F `contains` VarB
        ; F `contains` VarA, T `contains` VarB
        )
    then
        X
    else
        mode_robdd(T, F, E, I ^ neq_vars(VarA, VarB), R, no)
    ) :-
    X = mode_robdd(T, F, E, I, R, _).

imp_vars(VarA, VarB, X) =
    ( if T `contains` VarA, F `contains` VarB then
        zero
    else if T `contains` VarB then
        X
    else if F `contains` VarA then
        X
    else
        mode_robdd(T, F, E, I ^ imp_vars(VarA, VarB), R, no)
    ) :-
    X = mode_robdd(T, F, E, I, R, _).

conj_vars(Vars, X) =
    ( if Vars `subset` T then
        X
    else if is_non_empty(Vars `intersect` F) then
        zero
    else
        mode_robdd(T `union` Vars, F, E, I, R, no)
    ) :-
    X = mode_robdd(T, F, E, I, R, _).

conj_not_vars(Vars, X) =
    ( if Vars `subset` F then
        X
    else if is_non_empty(Vars `intersect` T) then
        zero
    else
        mode_robdd(T, F `union` Vars, E, I, R, no)
    ) :-
    X = mode_robdd(T, F, E, I, R, _).

disj_vars(Vars, X0) = X :-
    X0 = mode_robdd(T, F, E, I, R, _N),
    ( if is_non_empty(Vars `intersect` T) then
        X = X0
    else if Vars `subset` F then
        X = zero
    else
        VarsNF = Vars `difference` F,
        ( if remove_least(Var1, VarsNF, VarsNF1) then
            ( if remove_least(Var2, VarsNF1, VarsNF2) then
                ( if is_empty(VarsNF2) then
                    X = mode_robdd(T, F, E,
                        I ^ either(Var1, Var2),
                        R, no)
                else
                    X = X0 `x` disj_vars(VarsNF)
                )
            else
                X = X0 ^ var(Var1)
            )
        else
            X = zero
        )
    ).

disj_vars_eq(Vars, Var, X) =
    ( if F `contains` Var then
        ( if Vars `subset` F then
            X
        else
            X ^ conj_not_vars(Vars)
        )
    else if T `contains` Var then
        ( if Vars `subset` F then
            zero
        else
            X ^ disj_vars(Vars)
        )
    else if is_non_empty(Vars `intersect` T) then
        X ^ var(Var)
    else if Vars `subset` F then
        X ^ not_var(Var)
    else if remove_least(Var1, Vars, Vars1) then
        ( if is_empty(Vars1) then
            X ^ eq_vars(Var, Var1)
        else
            (X ^ imp_vars_set(Vars, Var)) `x`
                (var(Var) =< disj_vars(Vars))
        )
    else
        X ^ not_var(Var)
    ) :-
    X = mode_robdd(T, F, _E, _I, _R, _N).

    % imp_vars_set({V1, ..., Vn}, V) <===> (V1 =< V) * ... * (Vn % =< V)
:- func imp_vars_set(vars(T)::in, var(T)::in, tfeirn(T)::di_tfeirn) =
    (tfeirn(T)::uo_tfeirn) is det.
:- pragma type_spec(func(imp_vars_set/3), T = mc_type).

imp_vars_set(Vars, Var, mode_robdd(T, F, E, I0, R, _)) =
        mode_robdd(T, F, E, I, R, no) :-
    I = I0 ^ foldl(func(VarA, I1) = I1 ^ imp_vars(VarA, Var), Vars).

%-----------------------------------------------------------------------------%

at_most_one_of(Vars, X) =
    ( if count(Vars `difference` F) =< 1 then
        X
    else if count(Vars `intersect` T) > 1 then
        zero
    else
        mode_robdd(T, F, E, I ^ at_most_one_of(Vars), R, no)
    ) :-
    X = mode_robdd(T, F, E, I, R, _).

not_both(VarA, VarB, X) =
    ( if F `contains` VarA then
        X
    else if F `contains` VarB then
        X
    else if T `contains` VarA then
        not_var(VarB, X)
    else if T `contains` VarB then
        not_var(VarA, X)
    else
        mode_robdd(T, F, E, I ^ not_both(VarA, VarB), R, no)
    ) :-
    X = mode_robdd(T, F, E, I, R, _).

io_constraint(V_in, V_out, V_, X) =
    X ^ not_both(V_in, V_) ^ disj_vars_eq(Vars, V_out) :-
    Vars = list_to_set([V_in, V_]).

%-----------------------------------------------------------------------------%

var_entailed(X, V) :-
    (X ^ robdd = zero ; X ^ true_vars `contains` V).

vars_entailed(X) =
    ( if X ^ robdd = zero then
        all_vars
    else
        some_vars(X ^ true_vars)
    ).

vars_disentailed(X) =
    ( if X ^ robdd = zero then
        all_vars
    else
        some_vars(X ^ false_vars)
    ).

%-----------------------------------------------------------------------------%

known_vars(X, TrueVars, FalseVars) :-
    ( if X ^ robdd = zero then
        TrueVars = init,
        FalseVars = init
    else
        TrueVars = X ^ true_vars,
        FalseVars = X ^ false_vars
    ).

%-----------------------------------------------------------------------------%

restrict(V, mode_robdd(T, F, E, I, R, N)) =
    ( if T `contains` V then
        mode_robdd(T `delete` V, F, E, I, R, N)
    else if F `contains` V then
        mode_robdd(T, F `delete` V, E, I, R, N)
    else if L = E ^ leader(V) then
        ( if L \= V then
            mode_robdd(T, F, E `delete` V, I, R, N)
        else
            mode_robdd(T, F, E `delete` V, I `delete` V,
                restrict(V, R), N)
        )
    else
        mode_robdd(T, F, E, I `delete` V, restrict(V, R), N)
    ).

restrict_threshold(V, mode_robdd(T, F, E, I, R, N)) =
    mode_robdd(remove_gt(T, V), remove_gt(F, V), restrict_threshold(V, E),
        restrict_threshold(V, I), restrict_threshold(V, R), N).

restrict_filter(P, mode_robdd(T, F, E, I, R, N)) =
    mode_robdd(filter(P, T), filter(P, F), filter(P, E), filter(P, I),
        restrict_filter(P, R), N).

var_restrict_true(V, mode_robdd(T, F, E, I, R, N)) = X :-
    ( if F `contains` V then
        X = zero
    else if T `contains` V then
        X = mode_robdd(T `delete` V, F, E, I, R, N)
    else
        X0 = normalise(mode_robdd(T `insert` V, F, E, I, R, no)),
        X = X0 ^ true_vars := X0 ^ true_vars `delete` V
    ).

var_restrict_false(V, mode_robdd(T, F, E, I, R, N)) = X :-
    ( if T `contains` V then
        X = zero
    else if F `contains` V then
        X = mode_robdd(T, F `delete` V, E, I, R, N)
    else
        X0 = normalise(mode_robdd(T, F `insert` V, E, I, R, no)),
        X = X0 ^ false_vars := X0 ^ false_vars `delete` V
    ).

%-----------------------------------------------------------------------------%

labelling(Vars0, mode_robdd(T, F, E, I, R, N), TrueVars, FalseVars) :-
    TrueVars0 = T `intersect` Vars0,
    FalseVars0 = F `intersect` Vars0,
    Vars = Vars0 `difference` TrueVars0 `difference` FalseVars0,
    ( if is_empty(Vars) then
        TrueVars = TrueVars0,
        FalseVars = FalseVars0
    else
        labelling_2(Vars, mode_robdd(init, init, E, I, R, N),
            TrueVars1, FalseVars1),
        TrueVars = TrueVars0 `union` TrueVars1,
        FalseVars = FalseVars0 `union` FalseVars1
    ).

:- pred labelling_2(vars(T)::in, tfeirn(T)::in, vars(T)::out, vars(T)::out)
    is nondet.

labelling_2(Vars0, X0, TrueVars, FalseVars) :-
    ( if remove_least(V, Vars0, Vars) then
        (
            X = var_restrict_false(V, X0),
            X ^ robdd \= zero,
            labelling_2(Vars, X, TrueVars, FalseVars0),
            FalseVars = FalseVars0 `insert` V
        ;
            X = var_restrict_true(V, X0),
            X ^ robdd \= zero,
            labelling_2(Vars, X, TrueVars0, FalseVars),
            TrueVars = TrueVars0 `insert` V
        )
    else
        TrueVars = init,
        FalseVars = init
    ).

%-----------------------------------------------------------------------------%

minimal_model(Vars, X0, TrueVars, FalseVars) :-
    ( if is_empty(Vars) then
        TrueVars = init,
        FalseVars = init
    else
        minimal_model_2(Vars, X0, TrueVars0, FalseVars0),
        (
            TrueVars = TrueVars0,
            FalseVars = FalseVars0
        ;
            X = X0 `x` (~conj_vars(TrueVars0)),
            minimal_model(Vars, X, TrueVars, FalseVars)
        )
    ).

:- pred minimal_model_2(vars(T)::in, tfeirn(T)::in, vars(T)::out, vars(T)::out)
    is semidet.

minimal_model_2(Vars0, X0, TrueVars, FalseVars) :-
    ( if remove_least(V, Vars0, Vars) then
        X1 = var_restrict_false(V, X0),
        ( if X1 ^ robdd \= zero then
            minimal_model_2(Vars, X1, TrueVars, FalseVars0),
            FalseVars = FalseVars0 `insert` V
        else
            X2 = var_restrict_true(V, X0),
            X2 ^ robdd \= zero,
            minimal_model_2(Vars, X2, TrueVars0, FalseVars),
            TrueVars = TrueVars0 `insert` V
        )
    else
        TrueVars = init,
        FalseVars = init
    ).

%-----------------------------------------------------------------------------%

ensure_normalised(X) = normalise(X).

:- func normalise(tfeirn(T)::di_tfeirn) = (tfeirn(T)::no_tfeirn) is det.
:- pragma type_spec(func(normalise/1), T = mc_type).

normalise(X0) = X :-
    X0 = mode_robdd(T, F, E, I, R, yes),
    X = mode_robdd(T, F, E, I, R, yes).
normalise(mode_robdd(TrueVars0, FalseVars0, EQVars0, ImpVars0, Robdd0, no))
        = X :-
    % T <-> F
    ( if is_non_empty(TrueVars0 `intersect` FalseVars0) then
        X = zero
    else
        % TF <-> E
        normalise_true_false_equivalent_vars(Changed0,
            TrueVars0, TrueVars1, FalseVars0, FalseVars1,
            EQVars0, EQVars1),

        % TF <-> I
        normalise_true_false_implication_vars(Changed1,
            TrueVars1, TrueVars2, FalseVars1, FalseVars2,
            ImpVars0, ImpVars1),
        Changed2 = Changed0 `bool.or` Changed1,

        % TF -> R
        Robdd1 = restrict_true_false_vars(TrueVars2, FalseVars2, Robdd0),
        Changed3 = Changed2 `bool.or` ( if Robdd1 \= Robdd0 then yes else no),

        ( if
            % TF <- R
            definite_vars(Robdd1,
                some_vars(NewTrueVars), some_vars(NewFalseVars))
        then
            ( if
                is_empty(NewTrueVars),
                is_empty(NewFalseVars)
            then
                Changed4 = Changed3,
                TrueVars = TrueVars2,
                FalseVars = FalseVars2
            else
                Changed4 = yes,
                TrueVars = TrueVars2 `union` NewTrueVars,
                FalseVars = FalseVars2 `union` NewFalseVars
            ),

            % E <-> I
            ( if
                propagate_equivalences_into_implications(
                    EQVars1, Changed5, ImpVars1, ImpVars2)
            then
                propagate_implications_into_equivalences(
                    Changed6, EQVars1, EQVars2,
                    ImpVars2, ImpVars3),
                Changed7 = Changed4 `bool.or` Changed5
                    `bool.or` Changed6,

                % E <-> R
                extract_equivalent_vars_from_robdd(Changed8,
                    Robdd1, Robdd2, EQVars2, EQVars),
                Changed9 = Changed7 `bool.or` Changed8,

                % I <-> R
                extract_implication_vars_from_robdd(Changed10,
                    Robdd2, Robdd, ImpVars3, ImpVars),
                Changed = Changed9 `bool.or` Changed10,

                (
                    Changed = yes,
                    X0 = mode_robdd(TrueVars, FalseVars,
                        EQVars, ImpVars, Robdd, no),
                    X = normalise(X0)
                ;
                    Changed = no,
                    X = mode_robdd(TrueVars, FalseVars,
                        EQVars, ImpVars, Robdd, yes)
                )
            else
                X = zero
            )
        else
            X = zero
        )
    ).

:- pred normalise_true_false_equivalent_vars(bool::out, vars(T)::in,
    vars(T)::out, vars(T)::in, vars(T)::out,
    equiv_vars(T)::in, equiv_vars(T)::out) is det.
:- pragma type_spec(pred(normalise_true_false_equivalent_vars/7), T = mc_type).

normalise_true_false_equivalent_vars(Changed, T0, T, F0, F) -->
    normalise_known_equivalent_vars(Changed0, T0, T),
    normalise_known_equivalent_vars(Changed1, F0, F),
    { Changed = Changed0 `bool.or` Changed1 }.

:- pred extract_equivalent_vars_from_robdd(bool::out, robdd(T)::in,
    robdd(T)::out, equiv_vars(T)::in, equiv_vars(T)::out) is det.
:- pragma type_spec(pred(extract_equivalent_vars_from_robdd/5), T = mc_type).

extract_equivalent_vars_from_robdd(Changed, Robdd0, Robdd, EQVars0, EQVars) :-
    ( if RobddEQVars = equivalent_vars_in_robdd(Robdd0) then
        ( if empty(RobddEQVars) then
            Changed0 = no,
            Robdd1 = Robdd0,
            EQVars = EQVars0
        else
            Changed0 = yes,

            % Remove any equalities we have just found from the
            % ROBDD.
            Robdd1 = squeeze_equiv(RobddEQVars, Robdd0),

            EQVars = EQVars0 * RobddEQVars
        )
    else
        EQVars = init_equiv_vars,
        Changed0 = ( if EQVars = EQVars0 then no else yes ),
        Robdd1 = Robdd0
    ),

    % Remove any other equalities from the ROBDD.
    % Note that we can use EQVars0 here since we have already removed the
    % equivalences in RobddEQVars using squeeze_equiv.
    Robdd = remove_equiv(EQVars0, Robdd1),
    Changed = Changed0 `bool.or` ( if Robdd \= Robdd1 then yes else no ).

:- func x(tfeirn(T)::di_tfeirn, robdd(T)::in) = (tfeirn(T)::uo_tfeirn) is det.
:- pragma type_spec(func(x/2), T = mc_type).

%/*
x(mode_robdd(TA, FA, EA, IA, RA, _), RB) =
        mode_robdd(TA `union` T1, FA `union` F1, EA * E1, IA * I1,
            RA * R1, no) :-
    mode_robdd(T1, F1, E1, I1, R1, _) =
        normalise(mode_robdd(TA, FA, EA, IA, RB, no)).
%*/

/*
x(mode_robdd(TA, FA, EA, IA, RA, _), RB) =
        mode_robdd(TA, FA, EA, IA, RA * RB, no).
*/

%---------------------------------------------------------------------------%

to_robdd(X) =
    (X ^ robdd * conj_vars(X ^ true_vars) * conj_not_vars(X ^ false_vars))
    ^ map.foldl(func(A, B, R) = R * eq_vars(A, B),
            X ^ equiv_vars ^ leader_map)
    ^ add_implications(X ^ imp_vars).

% to_robdd(X) =
%   (X ^ robdd * conj_vars(X ^ true_vars) * conj_not_vars(X ^ false_vars))
%   ^ expand_equiv(X ^ equiv_vars ^ leader_map)
%   ^ add_implications(X ^ imp_vars).

%---------------------------------------------------------------------------%

robdd_to_mode_robdd(R) =
    normalise(mode_robdd(init, init, init_equiv_vars, init_imp_vars,
        R, no)).

%---------------------------------------------------------------------------%
:- end_module mode_robdd.tfeirn.
%-----------------------------------------------------------------------------%
