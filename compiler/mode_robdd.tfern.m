%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: mode_robdd.tfern.m.
% Main author: dmo
% Stability: low
% 
%-----------------------------------------------------------------------------%

:- module mode_robdd.tfern.

:- interface.

:- import_module robdd.
:- import_module term.

:- type mode_robdd(T).
:- type mode_robdd == mode_robdd(generic).

:- inst mode_robdd == ground. % XXX

:- mode di_mode_robdd == in. % XXX
:- mode uo_mode_robdd == out. % XXX

% Constants.
:- func one = mode_robdd(T).
:- func zero = mode_robdd(T).

% Conjunction.
:- func mode_robdd(T) * mode_robdd(T) = mode_robdd(T).

% Disjunction.
:- func mode_robdd(T) + mode_robdd(T) = mode_robdd(T).

%-----------------------------------------------------------------------------%

:- func var(var(T)::in, mode_robdd(T)::in(mode_robdd)) =
	(mode_robdd(T)::out(mode_robdd)) is det.

:- func not_var(var(T)::in, mode_robdd(T)::in(mode_robdd)) =
	(mode_robdd(T)::out(mode_robdd)) is det.

:- func eq_vars(var(T)::in, var(T)::in, mode_robdd(T)::di_mode_robdd) =
	(mode_robdd(T)::uo_mode_robdd) is det.

:- func neq_vars(var(T)::in, var(T)::in, mode_robdd(T)::di_mode_robdd) =
	(mode_robdd(T)::uo_mode_robdd) is det.

:- func imp_vars(var(T)::in, var(T)::in, mode_robdd(T)::di_mode_robdd) =
	(mode_robdd(T)::uo_mode_robdd) is det.

:- func conj_vars(vars(T)::in, mode_robdd(T)::di_mode_robdd) =
	(mode_robdd(T)::uo_mode_robdd) is det.

:- func conj_not_vars(vars(T)::in, mode_robdd(T)::di_mode_robdd) =
	(mode_robdd(T)::uo_mode_robdd) is det.

:- func disj_vars(vars(T)::in, mode_robdd(T)::di_mode_robdd) =
	(mode_robdd(T)::uo_mode_robdd) is det.

:- func at_most_one_of(vars(T)::in, mode_robdd(T)::di_mode_robdd) =
	(mode_robdd(T)::uo_mode_robdd) is det.

:- func not_both(var(T)::in, var(T)::in, mode_robdd(T)::di_mode_robdd) =
	(mode_robdd(T)::uo_mode_robdd) is det.

:- func io_constraint(var(T)::in, var(T)::in, var(T)::in,
	mode_robdd(T)::di_mode_robdd) = (mode_robdd(T)::uo_mode_robdd) is det.

		% disj_vars_eq(Vars, Var) <=> (disj_vars(Vars) =:= Var).
:- func disj_vars_eq(vars(T)::in, var(T)::in, mode_robdd(T)::di_mode_robdd) =
	(mode_robdd(T)::uo_mode_robdd) is det.

:- func var_restrict_true(var(T)::in, mode_robdd(T)::di_mode_robdd) =
	(mode_robdd(T)::uo_mode_robdd) is det.

:- func var_restrict_false(var(T)::in, mode_robdd(T)::di_mode_robdd) =
	(mode_robdd(T)::uo_mode_robdd) is det.

%-----------------------------------------------------------------------------%

	% Succeed iff the var is entailed by the xROBDD.
:- pred var_entailed(mode_robdd(T)::in, var(T)::in) is semidet.

	% Return the set of vars entailed by the xROBDD.
:- func vars_entailed(mode_robdd(T)) = vars_entailed_result(T).

	% Return the set of vars disentailed by the xROBDD.
:- func vars_disentailed(mode_robdd(T)) = vars_entailed_result(T).

	% Existentially quantify away the var in the xROBDD.
:- func restrict(var(T), mode_robdd(T)) = mode_robdd(T).

	% Existentially quantify away all vars greater than the specified var.
:- func restrict_threshold(var(T), mode_robdd(T)) = mode_robdd(T).

:- func restrict_filter(pred(var(T))::(pred(in) is semidet),
	mode_robdd(T)::di_mode_robdd) = (mode_robdd(T)::uo_mode_robdd) is det.

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
:- pred labelling(vars(T)::in, mode_robdd(T)::in, vars(T)::out, vars(T)::out)
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
:- pred minimal_model(vars(T)::in, mode_robdd(T)::in, vars(T)::out,
	vars(T)::out) is nondet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module robdd.
:- import_module sparse_bitset.

% T - true vars, F - False Vars, E - equivalent vars, N -
% non-equivalent vars, R - ROBDD.
%
% Combinations to try:
%	R	(straight ROBDD)
%	TER	(Peter Schachte's extension)
%	TFENR	(Everything)

:- type mode_robdd(T)
	--->	mode_robdd(
			true_vars :: vars(T),
			false_vars :: vars(T),
			robdd :: robdd(T)
		).

one = mode_robdd(init, init, one).

zero = mode_robdd(init, init, zero).

mode_robdd(TA, FA, RA) * mode_robdd(TB, FB, RB) =
		normalise(mode_robdd(TA `union` TB, FA `union` FB, RA * RB)).

mode_robdd(TA, FA, RA0) + mode_robdd(TB, FB, RB0) = X :-
	( RA0 = zero ->
		X = mode_robdd(TB, FB, RB0)
	; RB0 = zero ->
		X = mode_robdd(TA, FA, RA0)
	;
		RA = RA0 * conj_vars(TA `difference` TB) *
			conj_not_vars(FA `difference` FB),
		RB = RB0 * conj_vars(TB `difference` TA) *
			conj_not_vars(FB `difference` FA),
		X = mode_robdd(TA `intersect` TB, FA `intersect` FB, RA + RB)
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

restrict(V, mode_robdd(T, F, R)) =
	mode_robdd(T `delete` V, F `delete` V, restrict(V, R)).

restrict_threshold(V, mode_robdd(T, F, R)) =
		mode_robdd(filter(P, T), filter(P, F),
		restrict_threshold(V, R)) :-
	P = (pred(U::in) is semidet :- \+ compare((>), U, V)).

var(V, X) =
	( T `contains` V ->
		X
	; F `contains` V ->
		zero
	;
		normalise(mode_robdd(T `insert` V, F, R))
	) :-
	X = mode_robdd(T, F, R).

not_var(V, X) =
	( F `contains` V ->
		X
	; T `contains` V ->
		zero
	;
		normalise(mode_robdd(T, F `insert` V, R))
	) :-
	X = mode_robdd(T, F, R).

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
		normalise(mode_robdd(T, F, R * eq_vars(VarA, VarB)))
	) :-
	X = mode_robdd(T, F, R).

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
		normalise(mode_robdd(T, F, R * neq_vars(VarA, VarB)))
	) :-
	X = mode_robdd(T, F, R).

imp_vars(VarA, VarB, X) =
	( T `contains` VarA, F `contains` VarB ->
		zero
	; T `contains` VarB ->
		X
	; F `contains` VarA ->
		X
	;
		normalise(mode_robdd(T, F, R * imp_vars(VarA, VarB)))
	) :-
	X = mode_robdd(T, F, R).

conj_vars(Vars, X) =
	( Vars `subset` T ->
		X
	; \+ empty(Vars `intersect` F) ->
		zero
	;
		normalise(mode_robdd(T `union` Vars, F, R))
	) :-
	X = mode_robdd(T, F, R).

conj_not_vars(Vars, X) =
	( Vars `subset` F ->
		X
	; \+ empty(Vars `intersect` T) ->
		zero
	;
		normalise(mode_robdd(T, F `union` Vars, R))
	) :-
	X = mode_robdd(T, F, R).

disj_vars(Vars, mode_robdd(T, F, R)) =
	( \+ empty(Vars `intersect` T) ->
		mode_robdd(T, F, R)
	; Vars `subset` F ->
		zero
	;
		normalise(mode_robdd(T, F, R * disj_vars(Vars)))
	).

at_most_one_of(Vars, X) =
	( count(Vars `difference` F) =< 1 ->
		X
	; count(Vars `intersect` T) > 1 ->
		zero
	;
		normalise(mode_robdd(T, F, R * at_most_one_of(Vars)))
	) :-
	X = mode_robdd(T, F, R).

not_both(VarA, VarB, X) =
	normalise(X ^ robdd := X ^ robdd * ~(var(VarA) * var(VarB))).

io_constraint(V_in, V_out, V_, X) =
	normalise(X ^ robdd :=
		X ^ robdd *
			( var(V_out) =:= var(V_in) + var(V_) ) *
			( ~(var(V_in) * var(V_)) )).

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
		normalise(mode_robdd(T, F, R * (disj_vars(Vars) =:= var(Var))))
	) :-
	X = mode_robdd(T, F, R).

var_restrict_true(V, mode_robdd(T, F, R)) =
	( F `contains` V ->
		zero
	; T `contains` V ->
		mode_robdd(T `delete` V, F, R)
	;
		normalise(mode_robdd(T, F, var_restrict_true(V, R)))
	).

var_restrict_false(V, mode_robdd(T, F, R)) =
	( T `contains` V ->
		zero
	; F `contains` V ->
		mode_robdd(T, F `delete` V, R)
	;
		normalise(mode_robdd(T, F, var_restrict_false(V, R)))
	).

restrict_filter(P, mode_robdd(T, F, R)) =
	mode_robdd(filter(P, T), filter(P, F), restrict_filter(P, R)).

labelling(Vars, mode_robdd(T, F, R), TrueVars `intersect` Vars `union` T,
		FalseVars `intersect` Vars `union` F) :-
	labelling(Vars, R, TrueVars, FalseVars).

minimal_model(Vars, mode_robdd(T, F, R), TrueVars `intersect` Vars `union` T,
		FalseVars `intersect` Vars `union` F) :-
	minimal_model(Vars, R, TrueVars, FalseVars).

%-----------------------------------------------------------------------------%

:- func normalise(mode_robdd(T)::di_mode_robdd) =
	(mode_robdd(T)::uo_mode_robdd) is det.

normalise(mode_robdd(TrueVars0, FalseVars0, Robdd0)) = X :-
	% ( some [V] (V `member` TrueVars0, V `member` FalseVars0) ->
	( \+ empty(TrueVars0 `intersect` FalseVars0) ->
		X = zero
	;
		Robdd1 = restrict_true_false_vars(TrueVars0, FalseVars0,
				Robdd0),
		(
			definite_vars(Robdd1,
				some_vars(TrueVars1), some_vars(FalseVars1))
		->
			(
				empty(TrueVars1),
				empty(FalseVars1)
			->
				X = mode_robdd(TrueVars0, FalseVars0, Robdd1)
			;
				X = mode_robdd(TrueVars0 `union` TrueVars1,
					FalseVars0 `union` FalseVars1,
					restrict_true_false_vars(TrueVars1,
						FalseVars1, Robdd1))
			)
		;
			X = zero
		)
	).

%-----------------------------------------------------------------------------%
:- end_module mode_robdd.tfern.
%-----------------------------------------------------------------------------%
