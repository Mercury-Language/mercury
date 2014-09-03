%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: mode_robdd.tfer.m.
% Main author: dmo
% Stability: low
% 
%-----------------------------------------------------------------------------%

:- module mode_robdd.tfer.

:- interface.

:- import_module robdd.
:- import_module term.

:- type tfer(T).
:- type tfer == tfer(generic).

:- inst tfer == ground. % XXX

:- mode di_tfer == in. % XXX
:- mode uo_tfer == out. % XXX

% Constants.
:- func one = tfer(T).
:- func zero = tfer(T).

% Conjunction.
:- func tfer(T) * tfer(T) = tfer(T).

% Disjunction.
:- func tfer(T) + tfer(T) = tfer(T).

%-----------------------------------------------------------------------------%

:- func var(var(T)::in, tfer(T)::in(tfer)) = (tfer(T)::out(tfer)) is det.

:- func not_var(var(T)::in, tfer(T)::in(tfer)) = (tfer(T)::out(tfer)) is det.

:- func eq_vars(var(T)::in, var(T)::in, tfer(T)::di_tfer) =
		(tfer(T)::uo_tfer) is det.

:- func neq_vars(var(T)::in, var(T)::in, tfer(T)::di_tfer) =
		(tfer(T)::uo_tfer) is det.

:- func imp_vars(var(T)::in, var(T)::in, tfer(T)::di_tfer) =
		(tfer(T)::uo_tfer) is det.

:- func conj_vars(vars(T)::in, tfer(T)::di_tfer) = (tfer(T)::uo_tfer)
		is det.

:- func conj_not_vars(vars(T)::in, tfer(T)::di_tfer) =
		(tfer(T)::uo_tfer) is det.

:- func disj_vars(vars(T)::in, tfer(T)::di_tfer) = (tfer(T)::uo_tfer)
		is det.

:- func at_most_one_of(vars(T)::in, tfer(T)::di_tfer) =
		(tfer(T)::uo_tfer) is det.

:- func not_both(var(T)::in, var(T)::in, tfer(T)::di_tfer) =
		(tfer(T)::uo_tfer) is det.

:- func io_constraint(var(T)::in, var(T)::in, var(T)::in, tfer(T)::di_tfer)
		= (tfer(T)::uo_tfer) is det.

		% disj_vars_eq(Vars, Var) <=> (disj_vars(Vars) =:= Var).
:- func disj_vars_eq(vars(T)::in, var(T)::in, tfer(T)::di_tfer) =
		(tfer(T)::uo_tfer) is det.

:- func var_restrict_true(var(T)::in, tfer(T)::di_tfer) =
		(tfer(T)::uo_tfer) is det.

:- func var_restrict_false(var(T)::in, tfer(T)::di_tfer) =
		(tfer(T)::uo_tfer) is det.

%-----------------------------------------------------------------------------%

	% Succeed iff the var is entailed by the xROBDD.
:- pred var_entailed(tfer(T)::in, var(T)::in) is semidet.

	% Return the set of vars entailed by the xROBDD.
:- func vars_entailed(tfer(T)) = vars_entailed_result(T).

	% Return the set of vars disentailed by the xROBDD.
:- func vars_disentailed(tfer(T)) = vars_entailed_result(T).

	% Existentially quantify away the var in the xROBDD.
:- func restrict(var(T), tfer(T)) = tfer(T).

	% Existentially quantify away all vars greater than the specified var.
:- func restrict_threshold(var(T), tfer(T)) = tfer(T).

:- func restrict_filter(pred(var(T))::(pred(in) is semidet),
		tfer(T)::di_tfer) = (tfer(T)::uo_tfer) is det.

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
:- pred labelling(vars(T)::in, tfer(T)::in, vars(T)::out, vars(T)::out)
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
:- pred minimal_model(vars(T)::in, tfer(T)::in, vars(T)::out, vars(T)::out)
		is nondet.

%-----------------------------------------------------------------------------%

% XXX
% Extract the ROBDD component of the TFER.
:- func robdd(tfer(T)) = robdd(T).

% Convert the TFER to an ROBDD.
:- func to_robdd(tfer(T)) = robdd(T).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mode_robdd.equiv_vars.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module robdd.
:- import_module sparse_bitset.

% T - true vars, F - False Vars, E - equivalent vars, N -
% non-equivalent vars, R - ROBDD.
%
% Combinations to try:
%	R	(straight ROBDD)
%	TER	(Peter Schachte's extension)
%	TFENR	(Everything)

:- type tfer(T)
	--->	mode_robdd(
			true_vars :: vars(T),
			false_vars :: vars(T),
			equiv_vars :: equiv_vars(T),
			robdd :: robdd(T)
		).

one = mode_robdd(init, init, init_equiv_vars, one).

zero = mode_robdd(init, init, init_equiv_vars, zero).

mode_robdd(TA, FA, EA, RA) * mode_robdd(TB, FB, EB, RB) =
		normalise(mode_robdd(TA1 `union` TB1, FA1 `union` FB1,
			EA1 * EB1, RA1 * RB1)) :-
	TU = TA `union` TB,
	FU = FA `union` FB,
	EU = EA * EB,
	mode_robdd(TA1, FA1, EA1, RA1) = normalise(mode_robdd(TU, FU, EU, RA)),
	mode_robdd(TB1, FB1, EB1, RB1) = normalise(mode_robdd(TU, FU, EU, RB)).

mode_robdd(TA0, FA0, EA0, RA0) + mode_robdd(TB0, FB0, EB0, RB0) = X :-
	( RA0 = zero ->
		X = mode_robdd(TB0, FB0, EB0, RB0)
	; RB0 = zero ->
		X = mode_robdd(TA0, FA0, EA0, RA0)
	;
		X = mode_robdd(T, F, E, R),
		T = TA0 `intersect` TB0,
		F = FA0 `intersect` FB0,
		E = EA + EB,
		R = RA + RB,

		TAB = TA0 `difference` TB0,
		FAB = FA0 `difference` FB0,
		EA = EA0 ^ add_equalities(TAB) ^ add_equalities(FAB),

		TBA = TB0 `difference` TA0,
		FBA = FB0 `difference` FA0,
		EB = EB0 ^ add_equalities(TBA) ^ add_equalities(FBA),

		RA1 = foldl(
			func(V, R0) = R0 * var(E ^ det_leader(V)), TAB, RA0),
		RA2 = foldl(
			func(V, R0) = R0 *
				not_var(E ^ det_leader(V)), FAB, RA1),
		EA1 = (EA `difference` EB) + EA0,
		RA = add_equivalences(EA1, RA2),

		RB1 = foldl(
			func(V, R0) = R0 * var(E ^ det_leader(V)), TBA, RB0),
		RB2 = foldl(
			func(V, R0) = R0 *
				not_var(E ^ det_leader(V)), FBA, RB1),
		EB1 = (EB `difference` EA) + EB0,
		RB = add_equivalences(EB1, RB2)
	).

var_entailed(X, V) :-
	(X ^ robdd = zero ; X ^ true_vars `contains` V).

vars_entailed(X) =
	(X ^ robdd = zero ->
		all_vars
	;
		some_vars(X ^ true_vars)
	).

vars_disentailed(X) =
	(X ^ robdd = zero ->
		all_vars
	;
		some_vars(X ^ false_vars)
	).

restrict(V, mode_robdd(T, F, E, R)) =
	mode_robdd(T `delete` V, F `delete` V, E `delete` V, restrict(V, R)).

restrict_threshold(V, mode_robdd(T, F, E, R)) =
		mode_robdd(filter(P, T), filter(P, F), restrict_threshold(V, E),
			restrict_threshold(V, R)) :-
	P = (pred(U::in) is semidet :- \+ compare((>), U, V)).

var(V, X) =
	( T `contains` V ->
		X
	; F `contains` V ->
		zero
	;
		normalise(mode_robdd(T `insert` V, F, E, R))
	) :-
	X = mode_robdd(T, F, E, R).

not_var(V, X) =
	( F `contains` V ->
		X
	; T `contains` V ->
		zero
	;
		normalise(mode_robdd(T, F `insert` V, E, R))
	) :-
	X = mode_robdd(T, F, E, R).

eq_vars(VarA, VarB, X) =
	(
		( T `contains` VarA, T `contains` VarB
		; F `contains` VarA, F `contains` VarB
		)
	->
		X
	;
		( T `contains` VarA, F `contains` VarB
		; F `contains` VarA, T `contains` VarB
		)
	->
		zero
	;
		normalise(mode_robdd(T, F, add_equality(VarA, VarB, E), R))
	) :-
	X = mode_robdd(T, F, E, R).

neq_vars(VarA, VarB, X) =
	(
		( T `contains` VarA, T `contains` VarB
		; F `contains` VarA, F `contains` VarB
		)
	->
		zero
	;
		( T `contains` VarA, F `contains` VarB
		; F `contains` VarA, T `contains` VarB
		)
	->
		X
	;
		X `x` neq_vars(VarA, VarB)
	) :-
	X = mode_robdd(T, F, _E, _R).

imp_vars(VarA, VarB, X) =
	( T `contains` VarA, F `contains` VarB ->
		zero
	; T `contains` VarB ->
		X
	; F `contains` VarA ->
		X
	;
		X `x` imp_vars(VarA, VarB)
	) :-
	X = mode_robdd(T, F, _E, _R).

conj_vars(Vars, X) =
	( Vars `subset` T ->
		X
	; \+ empty(Vars `intersect` F) ->
		zero
	;
		normalise(mode_robdd(T `union` Vars, F, E, R))
	) :-
	X = mode_robdd(T, F, E, R).

conj_not_vars(Vars, X) =
	( Vars `subset` F ->
		X
	; \+ empty(Vars `intersect` T) ->
		zero
	;
		normalise(mode_robdd(T, F `union` Vars, E, R))
	) :-
	X = mode_robdd(T, F, E, R).

disj_vars(Vars, X) =
	( \+ empty(Vars `intersect` T) ->
		X
	; Vars `subset` F ->
		zero
	;
		X `x` disj_vars(Vars)
	) :-
	X = mode_robdd(T, F, _E, _R).

at_most_one_of(Vars, X) =
	( count(Vars `difference` F) =< 1 ->
		X
	; count(Vars `intersect` T) > 1 ->
		zero
	;
		X `x` at_most_one_of(Vars)
	) :-
	X = mode_robdd(T, F, _E, _R).

not_both(VarA, VarB, X) =
	( F `contains` VarA ->
		X
	; F `contains` VarB ->
		X
	; T `contains` VarA ->
		not_var(VarB, X)
	; T `contains` VarB ->
		not_var(VarA, X)
	;
		X `x` (not_var(VarA) + not_var(VarB))
	) :-
	X = mode_robdd(T, F, _E, _R).

io_constraint(V_in, V_out, V_, X) =
	X ^ not_both(V_in, V_) ^ disj_vars_eq(Vars, V_out) :-
	Vars = list_to_set([V_in, V_]).

disj_vars_eq(Vars, Var, X) =
	( F `contains` Var ->
		( Vars `subset` F ->
			X
		;
			X ^ conj_not_vars(Vars)
		)
	; T `contains` Var ->
		( Vars `subset` F ->
			zero
		;
			X ^ disj_vars(Vars)
		)
	;
		X `x` (disj_vars(Vars) =:= var(Var))
	) :-
	X = mode_robdd(T, F, _E, _R).

var_restrict_true(V, mode_robdd(T, F, E, R)) = X :-
	( F `contains` V ->
		X = zero
	; T `contains` V ->
		X = mode_robdd(T `delete` V, F, E, R)
	;
		X0 = normalise(mode_robdd(T `insert` V, F, E, R)),
		X = X0 ^ true_vars := X0 ^ true_vars `delete` V
	).

var_restrict_false(V, mode_robdd(T, F, E, R)) = X :-
	( T `contains` V ->
		X = zero
	; F `contains` V ->
		X = mode_robdd(T, F `delete` V, E, R)
	;
		X0 = normalise(mode_robdd(T, F `insert` V, E, R)),
		X = X0 ^ false_vars := X0 ^ false_vars `delete` V
	).

restrict_filter(P, mode_robdd(T, F, E, R)) =
	mode_robdd(filter(P, T), filter(P, F), filter(P, E), restrict_filter(P, R)).

labelling(Vars0, mode_robdd(T, F, E, R), TrueVars, FalseVars) :-
	TrueVars0 = T `intersect` Vars0,
	FalseVars0 = F `intersect` Vars0,
	Vars = Vars0 `difference` TrueVars0 `difference` FalseVars0,

	( empty(Vars) ->
		TrueVars = TrueVars0,
		FalseVars = FalseVars0
	;
		labelling_2(Vars, mode_robdd(init, init, E, R),
			TrueVars1, FalseVars1),
		TrueVars = TrueVars0 `union` TrueVars1,
		FalseVars = FalseVars0 `union` FalseVars1
	).

:- pred labelling_2(vars(T)::in, tfer(T)::in, vars(T)::out, vars(T)::out)
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

:- pred minimal_model_2(vars(T)::in, tfer(T)::in, vars(T)::out, vars(T)::out)
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

:- func normalise(tfer(T)::di_tfer) = (tfer(T)::uo_tfer) is det.

normalise(mode_robdd(TrueVars0, FalseVars0, EQVars0, Robdd0)) = X :-
	%( some [V] (V `member` TrueVars0, V `member` FalseVars0) ->
	( \+ empty(TrueVars0 `intersect` FalseVars0) ->
		X = zero
	;
		% Lines 4, 5
		normalise_true_false_equivalent_vars(Changed0, TrueVars0,
			TrueVars1, FalseVars0, FalseVars1, EQVars0, EQVars1),

		% Line 6
		Robdd1 = restrict_true_false_vars(TrueVars1, FalseVars1,
				Robdd0),
		Changed1 = Changed0 `bool__or` ( Robdd1 \= Robdd0 -> yes ; no),

		% Line 7
		(
			definite_vars(Robdd1,
				some_vars(NewTrueVars), some_vars(NewFalseVars))
		->
			(
				empty(NewTrueVars),
				empty(NewFalseVars)
			->
				Changed2 = Changed1,
				TrueVars2 = TrueVars1,
				FalseVars2 = FalseVars1
			;
				Changed2 = yes,
				TrueVars2 = TrueVars1 `union` NewTrueVars,
				FalseVars2 = FalseVars1 `union` NewFalseVars
			),

			% Lines 8, 9, 10
			extract_equivalent_vars_from_robdd(Changed3, Robdd1,
					Robdd2, EQVars1, EQVars2),
			Changed = Changed2 `bool__or` Changed3,

			% Line 11
			X0 = mode_robdd(TrueVars2, FalseVars2, EQVars2, Robdd2),
			X = ( Changed = yes ->
				normalise(X0)
			;
				X0
			)
		;
			X = zero
		)
	).

:- pred normalise_true_false_equivalent_vars(bool::out, vars(T)::in,
	vars(T)::out, vars(T)::in, vars(T)::out, equiv_vars(T)::in,
	equiv_vars(T)::out) is det.

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

:- func x(tfer(T)::di_tfer, robdd(T)::in) = (tfer(T)::uo_tfer) is det.

x(X, R) = X * mode_robdd(init, init, init_equiv_vars, R).

%---------------------------------------------------------------------------%

% to_robdd(X) = expand_equiv(X ^ equiv_vars,
% 				X ^ robdd * conj_vars(X ^ true_vars)
% 					* conj_not_vars(X ^ false_vars)).

to_robdd(X) = map__foldl(func(A, B, R) = R * eq_vars(A, B),
		X ^ equiv_vars ^ leader_map,
			X ^ robdd * conj_vars(X ^ true_vars)
			* conj_not_vars(X ^ false_vars)).

%---------------------------------------------------------------------------%
:- end_module mode_robdd.tfer.
%-----------------------------------------------------------------------------%
