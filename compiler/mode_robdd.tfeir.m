%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: mode_robdd.tfeir.m.
% Main author: dmo
% Stability: low
% 
%-----------------------------------------------------------------------------%

:- module mode_robdd.tfeir.

:- interface.

:- import_module check_hlds.
:- import_module check_hlds.mode_constraint_robdd.

:- import_module robdd.
:- import_module term.

:- type tfeir(T).
:- type tfeir == tfeir(generic).

:- inst tfeir == ground. % XXX

:- mode di_tfeir == in. % XXX
:- mode uo_tfeir == out. % XXX

% Constants.
:- func one = tfeir(T).
:- pragma type_spec(one/0, T = mc_type).

:- func zero = tfeir(T).
:- pragma type_spec(zero/0, T = mc_type).

% Conjunction.
:- func tfeir(T) * tfeir(T) = tfeir(T).
:- pragma type_spec(('*')/2, T = mc_type).

% Disjunction.
:- func tfeir(T) + tfeir(T) = tfeir(T).
:- pragma type_spec(('+')/2, T = mc_type).

%-----------------------------------------------------------------------------%

:- func var(var(T)::in, tfeir(T)::in(tfeir)) = (tfeir(T)::out(tfeir)) is det.
:- pragma type_spec(var/2, T = mc_type).

:- func not_var(var(T)::in, tfeir(T)::in(tfeir)) = (tfeir(T)::out(tfeir))
	is det.
:- pragma type_spec(not_var/2, T = mc_type).

:- func eq_vars(var(T)::in, var(T)::in, tfeir(T)::di_tfeir) =
	(tfeir(T)::uo_tfeir) is det.
:- pragma type_spec(eq_vars/3, T = mc_type).

:- func neq_vars(var(T)::in, var(T)::in, tfeir(T)::di_tfeir) =
	(tfeir(T)::uo_tfeir) is det.
:- pragma type_spec(neq_vars/3, T = mc_type).

:- func imp_vars(var(T)::in, var(T)::in, tfeir(T)::di_tfeir) =
	(tfeir(T)::uo_tfeir) is det.
:- pragma type_spec(imp_vars/3, T = mc_type).

:- func conj_vars(vars(T)::in, tfeir(T)::di_tfeir) = (tfeir(T)::uo_tfeir)
	is det.
:- pragma type_spec(conj_vars/2, T = mc_type).

:- func conj_not_vars(vars(T)::in, tfeir(T)::di_tfeir) =
	(tfeir(T)::uo_tfeir) is det.
:- pragma type_spec(conj_not_vars/2, T = mc_type).

:- func disj_vars(vars(T)::in, tfeir(T)::di_tfeir) = (tfeir(T)::uo_tfeir)
	is det.
:- pragma type_spec(disj_vars/2, T = mc_type).

:- func at_most_one_of(vars(T)::in, tfeir(T)::di_tfeir) = (tfeir(T)::uo_tfeir)
	is det.
:- pragma type_spec(at_most_one_of/2, T = mc_type).

:- func not_both(var(T)::in, var(T)::in, tfeir(T)::di_tfeir) =
	(tfeir(T)::uo_tfeir) is det.
:- pragma type_spec(not_both/3, T = mc_type).

:- func io_constraint(var(T)::in, var(T)::in, var(T)::in, tfeir(T)::di_tfeir)
	= (tfeir(T)::uo_tfeir) is det.
:- pragma type_spec(io_constraint/4, T = mc_type).

	% disj_vars_eq(Vars, Var) <=> (disj_vars(Vars) =:= Var).
:- func disj_vars_eq(vars(T)::in, var(T)::in, tfeir(T)::di_tfeir) =
	(tfeir(T)::uo_tfeir) is det.
:- pragma type_spec(disj_vars_eq/3, T = mc_type).

:- func var_restrict_true(var(T)::in, tfeir(T)::di_tfeir) =
	(tfeir(T)::uo_tfeir) is det.
:- pragma type_spec(var_restrict_true/2, T = mc_type).

:- func var_restrict_false(var(T)::in, tfeir(T)::di_tfeir) =
	(tfeir(T)::uo_tfeir) is det.
:- pragma type_spec(var_restrict_false/2, T = mc_type).

%-----------------------------------------------------------------------------%

	% Succeed iff the var is entailed by the xROBDD.
:- pred var_entailed(tfeir(T)::in, var(T)::in) is semidet.

	% Return the set of vars entailed by the xROBDD.
:- func vars_entailed(tfeir(T)) = vars_entailed_result(T) is det.

	% Return the set of vars disentailed by the xROBDD.
:- func vars_disentailed(tfeir(T)) = vars_entailed_result(T) is det.

	% Existentially quantify away the var in the xROBDD.
:- func restrict(var(T), tfeir(T)) = tfeir(T) is det.

	% Existentially quantify away all vars greater than the specified var.
:- func restrict_threshold(var(T), tfeir(T)) = tfeir(T) is det.

:- func restrict_filter(pred(var(T))::(pred(in) is semidet),
	tfeir(T)::di_tfeir) = (tfeir(T)::uo_tfeir) is det.

%-----------------------------------------------------------------------------%

	% labelling(Vars, xROBDD, TrueVars, FalseVars)
	%	Takes a set of Vars and an xROBDD and returns a value assignment
	%	for those Vars that is a model of the Boolean function
	%	represented by the xROBDD.
	%	The value assignment is returned in the two sets TrueVars (set
	%	of variables assigned the value 1) and FalseVars (set of
	%	variables assigned the value 0).
	%
	% XXX should try using sparse_bitset here.
:- pred labelling(vars(T)::in, tfeir(T)::in, vars(T)::out, vars(T)::out)
	is nondet.

	% minimal_model(Vars, xROBDD, TrueVars, FalseVars)
	%	Takes a set of Vars and an xROBDD and returns a value assignment
	%	for those Vars that is a minimal model of the Boolean function
	%	represented by the xROBDD.
	%	The value assignment is returned in the two sets TrueVars (set
	%	of variables assigned the value 1) and FalseVars (set of
	%	variables assigned the value 0).
	%
	% XXX should try using sparse_bitset here.
:- pred minimal_model(vars(T)::in, tfeir(T)::in, vars(T)::out, vars(T)::out)
	is nondet.

%-----------------------------------------------------------------------------%

% XXX
% Extract the ROBDD component of the TFEIR.
:- func robdd(tfeir(T)) = robdd(T).

% Convert the TFEIR to an ROBDD.
:- func to_robdd(tfeir(T)) = robdd(T).

%-----------------------------------------------------------------------------%

:- func robdd_to_mode_robdd(robdd(T)) = tfeir(T).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mode_robdd.equiv_vars.
:- import_module mode_robdd.implications.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module robdd.
:- import_module sparse_bitset.

% T - true vars, F - False Vars, E - equivalent vars, N -
% non-equivalent vars, I - implications, R - ROBDD.
%
% Combinations to try:
%	R	(straight ROBDD)
%	TFR
%	TER	(Peter Schachte's extension)
%	TFEIR
%	TFENIR

:- type tfeir(T)
	--->	mode_robdd(
			true_vars :: vars(T),
			false_vars :: vars(T),
			equiv_vars :: equiv_vars(T),
			imp_vars :: imp_vars(T),
			robdd :: robdd(T)
		).

one = mode_robdd(init, init, init_equiv_vars, init_imp_vars, one).

zero = mode_robdd(init, init, init_equiv_vars, init_imp_vars, zero).

mode_robdd(TA, FA, EA, IA, RA) * mode_robdd(TB, FB, EB, IB, RB) =
		normalise(mode_robdd(TA1 `union` TB1, FA1 `union` FB1,
			EA1 * EB1, IA1 * IB1, RA1 * RB1)) :-
	TU = TA `union` TB,
	FU = FA `union` FB,
	EU = EA * EB,
	IU = IA * IB,
	mode_robdd(TA1, FA1, EA1, IA1, RA1) =
		normalise(mode_robdd(TU, FU, EU, IU, RA)),
	mode_robdd(TB1, FB1, EB1, IB1, RB1) =
		normalise(mode_robdd(TU, FU, EU, IU, RB)).

mode_robdd(TA0, FA0, EA0, IA0, RA0) + mode_robdd(TB0, FB0, EB0, IB0, RB0)
		= X :-
	( RA0 = zero ->
		X = mode_robdd(TB0, FB0, EB0, IB0, RB0)
	; RB0 = zero ->
		X = mode_robdd(TA0, FA0, EA0, IA0, RA0)
	;
		X = normalise(mode_robdd(T, F, E, I, R)),
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
		RA2 = foldl(func(V, R0) = R0 *
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

var_entailed(X, V) :-
	(
		X ^ robdd = zero
	;
		X ^ true_vars `contains` V
	).

vars_entailed(X) =
	( X ^ robdd = zero ->
		all_vars
	;
		some_vars(X ^ true_vars)
	).

vars_disentailed(X) =
	( X ^ robdd = zero ->
		all_vars
	;
		some_vars(X ^ false_vars)
	).

restrict(V, mode_robdd(T, F, E, I, R)) =
	( T `contains` V ->
		mode_robdd(T `delete` V, F, E, I, R)
	; F `contains` V ->
		mode_robdd(T, F `delete` V, E, I, R)
	; L = E ^ leader(V) ->
		( L \= V ->
			mode_robdd(T, F, E `delete` V, I, R)
		;
			mode_robdd(T, F, E `delete` V, I `delete` V,
				restrict(V, R))
		)
	;
		mode_robdd(T, F, E, I `delete` V, restrict(V, R))
	).

restrict_threshold(V, mode_robdd(T, F, E, I, R)) =
	mode_robdd(remove_gt(T, V), remove_gt(F, V), restrict_threshold(V, E),
		restrict_threshold(V, I), restrict_threshold(V, R)).

var(V, X) = Res :-
	X = mode_robdd(T, F, E, I, R),
	( T `contains` V ->
		Res = X
	; F `contains` V ->
		Res = zero
	;
		Res = normalise(mode_robdd(T `insert` V, F, E, I, R))
	).

not_var(V, X) = Res :-
	X = mode_robdd(T, F, E, I, R),
	( F `contains` V ->
		Res = X
	; T `contains` V ->
		Res = zero
	;
		Res = normalise(mode_robdd(T, F `insert` V, E, I, R))
	).

eq_vars(VarA, VarB, X) = Res :-
	X = mode_robdd(T, F, E, I, R),
	(
		( T `contains` VarA, T `contains` VarB
		; F `contains` VarA, F `contains` VarB
		)
	->
		Res = X
	;
		( T `contains` VarA, F `contains` VarB
		; F `contains` VarA, T `contains` VarB
		)
	->
		Res = zero
	;
		Res = normalise(
			mode_robdd(T, F, add_equality(VarA, VarB, E), I, R))
	).

neq_vars(VarA, VarB, X) = Res :-
	X = mode_robdd(T, F, E, I, R),
	(
		( T `contains` VarA, T `contains` VarB
		; F `contains` VarA, F `contains` VarB
		)
	->
		Res = zero
	;
		( T `contains` VarA, F `contains` VarB
		; F `contains` VarA, T `contains` VarB
		)
	->
		Res = X
	;
		Res = normalise(
			mode_robdd(T, F, E, I ^ neq_vars(VarA, VarB), R))
	).

imp_vars(VarA, VarB, X) = Res :-
	X = mode_robdd(T, F, E, I, R),
	( T `contains` VarA, F `contains` VarB ->
		Res = zero
	; T `contains` VarB ->
		Res = X
	; F `contains` VarA ->
		Res = X
	;
		Res = normalise(
			mode_robdd(T, F, E, I ^ imp_vars(VarA, VarB), R))
	).

conj_vars(Vars, X) = Res :-
	X = mode_robdd(T, F, E, I, R),
	( Vars `subset` T ->
		Res = X
	; \+ empty(Vars `intersect` F) ->
		Res = zero
	;
		Res = normalise(mode_robdd(T `union` Vars, F, E, I, R))
	).

conj_not_vars(Vars, X) = Res :-
	X = mode_robdd(T, F, E, I, R),
	( Vars `subset` F ->
		Res = X
	; \+ empty(Vars `intersect` T) ->
		Res = zero
	;
		Res = normalise(mode_robdd(T, F `union` Vars, E, I, R))
	).

disj_vars(Vars, X) = Res :-
	X = mode_robdd(T, F, _E, _I, _R),
	( \+ empty(Vars `intersect` T) ->
		Res = X
	; Vars `subset` F ->
		Res = zero
	;
		Res = X `x` disj_vars(Vars)
	).

at_most_one_of(Vars, X) = Res :-
	X = mode_robdd(T, F, E, I, R),
	( count(Vars `difference` F) =< 1 ->
		Res = X
	; count(Vars `intersect` T) > 1 ->
		Res = zero
	;
		Res = normalise(
			mode_robdd(T, F, E, I ^ at_most_one_of(Vars), R))
	).

not_both(VarA, VarB, X) = Res :-
	X = mode_robdd(T, F, E, I, R),
	( F `contains` VarA ->
		Res = X
	; F `contains` VarB ->
		Res = X
	; T `contains` VarA ->
		Res = not_var(VarB, X)
	; T `contains` VarB ->
		Res = not_var(VarA, X)
	;
		Res = normalise(
			mode_robdd(T, F, E, I ^ not_both(VarA, VarB), R))
	).

io_constraint(V_in, V_out, V_, X) = Res :-
	Vars = list_to_set([V_in, V_]),
	Res = X ^ not_both(V_in, V_) ^ disj_vars_eq(Vars, V_out).

disj_vars_eq(Vars, Var, X) = Res :-
	X = mode_robdd(T, F, _E, _I, _R),
	( F `contains` Var ->
		( Vars `subset` F ->
			Res = X
		;
			Res = X ^ conj_not_vars(Vars)
		)
	; T `contains` Var ->
		( Vars `subset` F ->
			Res = zero
		;
			Res = X ^ disj_vars(Vars)
		)
	;
		Res = X `x` (disj_vars(Vars) =:= var(Var))
	).

var_restrict_true(V, mode_robdd(T, F, E, I, R)) = X :-
	( F `contains` V ->
		X = zero
	; T `contains` V ->
		X = mode_robdd(T `delete` V, F, E, I, R)
	;
		X0 = normalise(mode_robdd(T `insert` V, F, E, I, R)),
		X = X0 ^ true_vars := X0 ^ true_vars `delete` V
	).

var_restrict_false(V, mode_robdd(T, F, E, I, R)) = X :-
	( T `contains` V ->
		X = zero
	; F `contains` V ->
		X = mode_robdd(T, F `delete` V, E, I, R)
	;
		X0 = normalise(mode_robdd(T, F `insert` V, E, I, R)),
		X = X0 ^ false_vars := X0 ^ false_vars `delete` V
	).

restrict_filter(P, mode_robdd(T, F, E, I, R)) =
	mode_robdd(filter(P, T), filter(P, F), filter(P, E), filter(P, I),
		restrict_filter(P, R)).

labelling(Vars0, mode_robdd(T, F, E, I, R), TrueVars, FalseVars) :-
	TrueVars0 = T `intersect` Vars0,
	FalseVars0 = F `intersect` Vars0,
	Vars = Vars0 `difference` TrueVars0 `difference` FalseVars0,

	( empty(Vars) ->
		TrueVars = TrueVars0,
		FalseVars = FalseVars0
	;
		labelling_2(Vars, mode_robdd(init, init, E, I, R), TrueVars1,
			FalseVars1),
		TrueVars = TrueVars0 `union` TrueVars1,
		FalseVars = FalseVars0 `union` FalseVars1
	).

:- pred labelling_2(vars(T)::in, tfeir(T)::in, vars(T)::out, vars(T)::out)
		is nondet.

labelling_2(Vars0, X0, TrueVars, FalseVars) :-
	( remove_least(Vars0, V, Vars) ->
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
	;
		TrueVars = init,
		FalseVars = init
	).

minimal_model(Vars, X0, TrueVars, FalseVars) :-
	( empty(Vars) ->
		TrueVars = init,
		FalseVars = init
	;
		minimal_model_2(Vars, X0, TrueVars0, FalseVars0),
		(
			TrueVars = TrueVars0,
			FalseVars = FalseVars0
		;
			X = X0 `x` (~conj_vars(TrueVars0)),
			minimal_model(Vars, X, TrueVars, FalseVars)
		)
	).

:- pred minimal_model_2(vars(T)::in, tfeir(T)::in, vars(T)::out, vars(T)::out)
	is semidet.

minimal_model_2(Vars0, X0, TrueVars, FalseVars) :-
	( remove_least(Vars0, V, Vars) ->
		X1 = var_restrict_false(V, X0),
		( X1 ^ robdd \= zero ->
			minimal_model_2(Vars, X1, TrueVars, FalseVars0),
			FalseVars = FalseVars0 `insert` V
		;
			X2 = var_restrict_true(V, X0),
			X2 ^ robdd \= zero,
			minimal_model_2(Vars, X2, TrueVars0, FalseVars),
			TrueVars = TrueVars0 `insert` V
		)
	;
		TrueVars = init,
		FalseVars = init
	).

%-----------------------------------------------------------------------------%

:- func normalise(tfeir(T)::di_tfeir) = (tfeir(T)::uo_tfeir) is det.
:- pragma type_spec(normalise/1, T = mc_type).

normalise(mode_robdd(TrueVars0, FalseVars0, EQVars0, ImpVars0, Robdd0)) = X :-
	% T <-> F
	( \+ empty(TrueVars0 `intersect` FalseVars0) ->
		X = zero
	;
		% TF <-> E
		normalise_true_false_equivalent_vars(Changed0, TrueVars0,
			TrueVars1, FalseVars0, FalseVars1, EQVars0, EQVars1),

		% TF <-> I
		normalise_true_false_implication_vars(Changed1, TrueVars1,
			TrueVars2, FalseVars1, FalseVars2, ImpVars0, ImpVars1),
		Changed2 = Changed0 `bool__or` Changed1,

		% TF -> R
		Robdd1 = restrict_true_false_vars(TrueVars2, FalseVars2,
			Robdd0),
		Changed3 = Changed2 `bool__or` ( Robdd1 \= Robdd0 -> yes ; no),

		(
			% TF <- R
			definite_vars(Robdd1, some_vars(NewTrueVars),
				some_vars(NewFalseVars))
		->
			(
				empty(NewTrueVars),
				empty(NewFalseVars)
			->
				Changed4 = Changed3,
				TrueVars = TrueVars2,
				FalseVars = FalseVars2
			;
				Changed4 = yes,
				TrueVars = TrueVars2 `union` NewTrueVars,
				FalseVars = FalseVars2 `union` NewFalseVars
			),

			% E <-> I
			(
				propagate_equivalences_into_implications(
					EQVars1, Changed5, ImpVars1, ImpVars2)
			->
				propagate_implications_into_equivalences(
					Changed6, EQVars1, EQVars2,
					ImpVars2, ImpVars3),
				Changed7 = Changed4 `bool__or`
					Changed5 `bool__or` Changed6,

				% E <-> R
				extract_equivalent_vars_from_robdd(Changed8,
					Robdd1, Robdd2,
				EQVars2, EQVars),
				Changed9 = Changed7 `bool__or` Changed8,

				% I <-> R
				extract_implication_vars_from_robdd(Changed10,
					Robdd2,
				Robdd, ImpVars3, ImpVars),
				Changed = Changed9 `bool__or` Changed10,

				X0 = mode_robdd(TrueVars, FalseVars, EQVars,
					ImpVars, Robdd),
				( Changed = yes ->
					X = normalise(X0)
				;
					X = X0
				)
			;
				X = zero
			)
		;
			X = zero
		)
	).

:- pred normalise_true_false_equivalent_vars(bool::out, vars(T)::in,
	vars(T)::out, vars(T)::in, vars(T)::out, equiv_vars(T)::in,
	equiv_vars(T)::out) is det.
:- pragma type_spec(normalise_true_false_equivalent_vars/7, T = mc_type).

normalise_true_false_equivalent_vars(Changed, T0, T, F0, F) -->
	normalise_known_equivalent_vars(Changed0, T0, T),
	normalise_known_equivalent_vars(Changed1, F0, F),
	{ Changed = Changed0 `bool__or` Changed1 }.

:- pred extract_equivalent_vars_from_robdd(bool::out, robdd(T)::in,
	robdd(T)::out, equiv_vars(T)::in, equiv_vars(T)::out) is det.

extract_equivalent_vars_from_robdd(Changed, Robdd0, Robdd, EQVars0, EQVars) :-
	( RobddEQVars = equivalent_vars_in_robdd(Robdd0) ->
		( empty(RobddEQVars) ->
			Changed0 = no,
			Robdd1 = Robdd0,
			EQVars = EQVars0
		;
			Changed0 = yes,

			% Remove any equalities we have just found from the
			% ROBDD.
			Robdd1 = squeeze_equiv(RobddEQVars, Robdd0),

			EQVars = EQVars0 * RobddEQVars
		)
	;
		EQVars = init_equiv_vars,
		Changed0 = ( EQVars = EQVars0 -> no ; yes ),
		Robdd1 = Robdd0
	),

	% Remove any other equalities from the ROBDD.
	% Note that we can use EQVars0 here since we have already removed the
	% equivalences in RobddEQVars using squeeze_equiv.
	Robdd = remove_equiv(EQVars0, Robdd1),
	Changed = Changed0 `bool__or` ( Robdd \= Robdd1 -> yes ; no ).

:- func x(tfeir(T)::di_tfeir, robdd(T)::in) = (tfeir(T)::uo_tfeir) is det.
:- pragma type_spec(x/2, T = mc_type).

x(X, R) = X * mode_robdd(init, init, init_equiv_vars, init_imp_vars, R).

%---------------------------------------------------------------------------%

robdd_to_mode_robdd(R) =
	normalise(mode_robdd(init, init, init_equiv_vars, init_imp_vars, R)).

%---------------------------------------------------------------------------%

to_robdd(X) =
	(X ^ robdd * conj_vars(X ^ true_vars) * conj_not_vars(X ^ false_vars))
	^ map__foldl(func(A, B, R) = R * eq_vars(A, B),
			X ^ equiv_vars ^ leader_map)
	^ add_implications(X ^ imp_vars).

/*
to_robdd(X) =
	(X ^ robdd * conj_vars(X ^ true_vars) * conj_not_vars(X ^ false_vars))
	^ add_equivalences(X ^ equiv_vars)
	^ add_implications(X ^ imp_vars).
*/

%---------------------------------------------------------------------------%
:- end_module mode_robdd.tfeir.
%-----------------------------------------------------------------------------%
