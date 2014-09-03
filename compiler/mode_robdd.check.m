%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mode_robbd.check.m.
% Main author: dmo
% Stability: low
%
% This module implements every one of its operations on two robdds,
% compares their results, and reports an error if they differ. This makes
% it easier to debug a new, potantially faster mode_robdd implementation
% by comparing its operation to the operation of an existing, trusted
% implementation.
%
%-----------------------------------------------------------------------------%

:- module mode_robdd.check.
:- interface.

:- import_module mode_robdd.tfeirn.

:- import_module robdd.
:- import_module term.

:- type check_robdd(T).
:- type check_robdd == check_robdd(generic).

:- inst check_robdd == ground.		% XXX the robdd parts should be unique

:- mode di_check_robdd == in.		% XXX the robdd parts should be di
:- mode uo_check_robdd == out.		% XXX the robdd parts should be uo

:- inst norm_check_robdd ---> mode_robdd(ground, norm_tfeirn).
:- mode ni_check_robdd == in(norm_check_robdd).
:- mode no_check_robdd == out(norm_check_robdd).

% Constants.
:- func one = check_robdd(T).
:- func zero = check_robdd(T).

% Conjunction.
:- func check_robdd(T) * check_robdd(T) = check_robdd(T).

% Disjunction.
:- func check_robdd(T) + check_robdd(T) = check_robdd(T).

%-----------------------------------------------------------------------------%

:- func var(var(T)::in, check_robdd(T)::in(check_robdd)) =
	(check_robdd(T)::out(check_robdd)) is det.

:- func not_var(var(T)::in, check_robdd(T)::in(check_robdd)) =
	(check_robdd(T)::out(check_robdd)) is det.

:- func eq_vars(var(T)::in, var(T)::in, check_robdd(T)::di_check_robdd) =
	(check_robdd(T)::uo_check_robdd) is det.

:- func neq_vars(var(T)::in, var(T)::in, check_robdd(T)::di_check_robdd) =
	(check_robdd(T)::uo_check_robdd) is det.

:- func imp_vars(var(T)::in, var(T)::in, check_robdd(T)::di_check_robdd) =
	(check_robdd(T)::uo_check_robdd) is det.

:- func conj_vars(vars(T)::in, check_robdd(T)::di_check_robdd) =
	(check_robdd(T)::uo_check_robdd) is det.

:- func conj_not_vars(vars(T)::in, check_robdd(T)::di_check_robdd) =
	(check_robdd(T)::uo_check_robdd) is det.

:- func disj_vars(vars(T)::in, check_robdd(T)::di_check_robdd) =
	(check_robdd(T)::uo_check_robdd) is det.

:- func at_most_one_of(vars(T)::in, check_robdd(T)::di_check_robdd) =
	(check_robdd(T)::uo_check_robdd) is det.

:- func not_both(var(T)::in, var(T)::in, check_robdd(T)::di_check_robdd) =
	(check_robdd(T)::uo_check_robdd) is det.

:- func io_constraint(var(T)::in, var(T)::in, var(T)::in,

check_robdd(T)::di_check_robdd)
	= (check_robdd(T)::uo_check_robdd) is det.

	% disj_vars_eq(Vars, Var) <=> (disj_vars(Vars) =:= Var).
:- func disj_vars_eq(vars(T)::in, var(T)::in, check_robdd(T)::di_check_robdd) =
	(check_robdd(T)::uo_check_robdd) is det.

:- func var_restrict_true(var(T)::in, check_robdd(T)::di_check_robdd) =
	(check_robdd(T)::uo_check_robdd) is det.

:- func var_restrict_false(var(T)::in, check_robdd(T)::di_check_robdd) =
	(check_robdd(T)::uo_check_robdd) is det.

%-----------------------------------------------------------------------------%

	% Succeed iff the var is entailed by the mode_robdd.
:- pred var_entailed(check_robdd(T)::ni_check_robdd, var(T)::in) is semidet.

	% Return the set of vars entailed by the mode_robdd.
:- func vars_entailed(check_robdd(T)::ni_check_robdd) =
	(vars_entailed_result(T)::out) is det.

% Return the set of vars disentailed by the mode_robdd.
:- func vars_disentailed(check_robdd(T)::ni_check_robdd) =
	(vars_entailed_result(T)::out) is det.

% Existentially quantify away the var in the mode_robdd.
:- func restrict(var(T)::in, check_robdd(T)::ni_check_robdd) =
	(check_robdd(T)::no_check_robdd) is det.

% Existentially quantify away all vars greater than the specified var.
:- func restrict_threshold(var(T)::in, check_robdd(T)::ni_check_robdd) =
	(check_robdd(T)::no_check_robdd) is det.

:- func restrict_filter(pred(var(T))::(pred(in) is semidet),
	check_robdd(T)::ni_check_robdd) =
	(check_robdd(T)::no_check_robdd) is det.

%-----------------------------------------------------------------------------%

	% labelling(Vars, ModeRobdd, TrueVars, FalseVars)
	%	Takes a set of Vars and an ModeRobdd and returns a value
	%	assignment for those Vars that is a model of the
	%	Boolean function represented by the ModeRobdd.
	%	The value assignment is returned in the two sets TrueVars (set
	%	of variables assigned the value 1) and FalseVars (set of
	%	variables assigned the value 0).
	%
	% XXX should try using sparse_bitset here.
:- pred labelling(vars(T)::in, check_robdd(T)::in, vars(T)::out, vars(T)::out)
	is nondet.

	% minimal_model(Vars, ModeRobdd, TrueVars, FalseVars)
	%	Takes a set of Vars and an ModeRobdd and returns a value
	%	assignment for those Vars that is a minimal model of the
	%	Boolean function represented by the ModeRobdd.
	%	The value assignment is returned in the two sets TrueVars (set
	%	of variables assigned the value 1) and FalseVars (set of
	%	variables assigned the value 0).
	%
	% XXX should try using sparse_bitset here.
:- pred minimal_model(vars(T)::in, check_robdd(T)::in, vars(T)::out,
	vars(T)::out) is nondet.

%-----------------------------------------------------------------------------%

:- func ensure_normalised(check_robdd(T)::in) =
	(check_robdd(T)::no_check_robdd) is det.

%-----------------------------------------------------------------------------%

% XXX
:- func robdd(check_robdd(T)) = robdd(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mode_robdd.r.
:- import_module mode_robdd.tfer.
:- import_module mode_robdd.tfeir.
:- import_module mode_robdd.tfeirn.

:- import_module require.
:- import_module robdd.
:- import_module sparse_bitset.

% Uncomment these for debugging.
% :- import_module io
% :- import_module pprint.
% :- import_module unsafe.

% The two fields of this type represent the two mode_robdd implementations
% being compared.
:- type check_robdd(T)
	--->	mode_robdd(
			x1 :: tfeir(T),
			x2 :: tfeirn(T)
		).

:- func check_robdd(tfeir(T), tfeirn(T)) = check_robdd(T).
% :- pragma promise_pure(check_robdd/2).

check_robdd(X1, X2) = mode_robdd(X1, X2) :-
	R1 = to_robdd(X1),
	R2 = to_robdd(X2),
	( R1 = R2 ->
		true
	;
		error("ROBDD representations differ")
		% impure unsafe_perform_io(report_robdd_error(R1, R2))
	).

% :- pred report_robdd_error(robdd(T)::in, robdd(T)::in, io::di, io::uo)
%	is det.
%
% report_robdd_error(R1, R2) -->
% 	% { R12 = R1 * (~ R2) },
% 	% { R21 = R2 * (~ R1) },
% 	io.write_string("ROBDD representations differ\n"),
% 	{ P = (pred(V::in, di, uo) is det --> io.write_int(var_to_int(V))) },
% 	robdd_to_dot(R1, P, "r1.dot"),
% 	robdd_to_dot(R2, P, "r2.dot").
% 	% io.write_string("R1 - R2:\n"),
% 	% pprint.write(80, to_doc(R12)),
% 	% io.write_string("\nR2 - R1:\n"),
% 	% pprint.write(80, to_doc(R21)),
% 	% io.nl.

%-----------------------------------------------------------------------------%

one = mode_robdd(one, one).

zero = mode_robdd(zero, zero).

X * Y = check_robdd(X ^ x1 * Y ^ x1, X ^ x2 * Y ^ x2).

X + Y = check_robdd(X ^ x1 + Y ^ x1, X ^ x2 + Y ^ x2).

var_entailed(X, V) :-
	var_entailed(X ^ x1, V).

vars_entailed(X) =
	vars_entailed(X ^ x1).

vars_disentailed(X) =
	vars_disentailed(X ^ x1).

restrict(V, X) =
	check_robdd(restrict(V, X ^ x1), restrict(V, X ^ x2)).

restrict_threshold(V, X) =
	check_robdd(restrict_threshold(V, X ^ x1),
		restrict_threshold(V, X ^ x2)).

var(V, X) = check_robdd(var(V, X ^ x1), var(V, X ^ x2)).

not_var(V, X) = check_robdd(not_var(V, X ^ x1), not_var(V, X ^ x2)).

eq_vars(VarA, VarB, X) =
	check_robdd(eq_vars(VarA, VarB, X ^ x1), eq_vars(VarA, VarB, X ^ x2)).

neq_vars(VarA, VarB, X) =
	check_robdd(neq_vars(VarA, VarB, X ^ x1), neq_vars(VarA, VarB, X ^ x2)).

imp_vars(VarA, VarB, X) =
	check_robdd(imp_vars(VarA, VarB, X ^ x1), imp_vars(VarA, VarB, X ^ x2)).

conj_vars(Vars, X) =
	check_robdd(conj_vars(Vars, X ^ x1), conj_vars(Vars, X ^ x2)).

conj_not_vars(Vars, X) =
	check_robdd(conj_not_vars(Vars, X ^ x1), conj_not_vars(Vars, X ^ x2)).

disj_vars(Vars, X) =
	check_robdd(disj_vars(Vars, X ^ x1), disj_vars(Vars, X ^ x2)).

at_most_one_of(Vars, X) =
	check_robdd(at_most_one_of(Vars, X ^ x1), at_most_one_of(Vars, X ^ x2)).

not_both(VarA, VarB, X) =
	check_robdd(not_both(VarA, VarB, X ^ x1), not_both(VarA, VarB, X ^ x2)).

io_constraint(V_in, V_out, V_, X) =
	check_robdd(io_constraint(V_in, V_out, V_, X ^ x1),
		io_constraint(V_in, V_out, V_, X ^ x2)).

disj_vars_eq(Vars, Var, X) =
	check_robdd(disj_vars_eq(Vars, Var, X ^ x1),
		disj_vars_eq(Vars, Var, X ^ x2)).

var_restrict_true(V, X) =
	check_robdd(var_restrict_true(V, X ^ x1), var_restrict_true(V, X ^ x2)).

var_restrict_false(V, X) =
	check_robdd(var_restrict_false(V, X ^ x1),
		var_restrict_false(V, X ^ x2)).

restrict_filter(P, X) =
	check_robdd(restrict_filter(P, X ^ x1), restrict_filter(P, X ^ x2)).

labelling(Vars, X, TrueVars, FalseVars) :-
	labelling(Vars, X ^ x1, TrueVars, FalseVars).

minimal_model(Vars, X, TrueVars, FalseVars) :-
	minimal_model(Vars, X ^ x1, TrueVars, FalseVars).

%-----------------------------------------------------------------------------%

robdd(X) = X ^ x1 ^ robdd.

%-----------------------------------------------------------------------------%

ensure_normalised(mode_robdd(X1, X2)) = mode_robdd(X1, ensure_normalised(X2)).

%-----------------------------------------------------------------------------%
:- end_module mode_robdd.check.
%-----------------------------------------------------------------------------%
