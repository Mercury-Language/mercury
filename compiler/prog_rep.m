%---------------------------------------------------------------------------%
% Copyright (C) 2000-2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates a representation of HLDS goals for the declarative
% debugger. Since this representation is to be included in debuggable
% executables, it should be as compact as possible, and therefore contains
% only the information required by the declarative debugger. The structure
% of this representation is defined by browser/program_representation.m.
%
% Author: zs.
%
%---------------------------------------------------------------------------%

:- module ll_backend__prog_rep.

:- interface.

:- import_module hlds__hlds_pred, hlds__hlds_goal, hlds__hlds_module.
:- import_module hlds__instmap.
:- import_module mdb, mdb__program_representation.

:- pred prog_rep__represent_goal(hlds_goal::in, instmap::in, vartypes::in,
	module_info::in, goal_rep::out) is det.

:- implementation.

:- import_module hlds__hlds_data, parse_tree__prog_data.
:- import_module string, list, set, std_util, require, term.

:- type prog_rep__info
	--->	info(
			vartypes    :: vartypes,
			module_info :: module_info
		).

prog_rep__represent_goal(Goal, InstMap0, VarTypes, ModuleInfo, Rep) :-
	prog_rep__represent_goal(Goal, InstMap0, info(VarTypes, ModuleInfo),
		Rep).

:- pred prog_rep__represent_goal(hlds_goal::in, instmap::in,
	prog_rep__info::in, goal_rep::out) is det.

prog_rep__represent_goal(GoalExpr - GoalInfo, InstMap0, Info, Rep) :-
	prog_rep__represent_goal_expr(GoalExpr, GoalInfo, InstMap0, Info, Rep).

:- pred prog_rep__represent_atomic_goal(hlds_goal_info::in,
	instmap::in, prog_rep__info::in, detism_rep::out,
	string::out, int::out, list(var_rep)::out) is det.

prog_rep__represent_atomic_goal(GoalInfo, InstMap0, Info,
		DetismRep, FilenameRep, LinenoRep, ChangedVarsRep) :-
	goal_info_get_determinism(GoalInfo, Detism),
	prog_rep__represent_detism(Detism, DetismRep),
	goal_info_get_context(GoalInfo, Context),
	term__context_file(Context, FilenameRep),
	term__context_line(Context, LinenoRep),
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap),
	instmap_changed_vars(InstMap0, InstMap, Info^vartypes, Info^module_info,
		ChangedVars),
	set__to_sorted_list(ChangedVars, ChangedVarsList),
	list__map(term__var_to_int, ChangedVarsList, ChangedVarsRep).

:- pred prog_rep__represent_detism(determinism::in,
	detism_rep::out) is det.

prog_rep__represent_detism(det, det_rep).
prog_rep__represent_detism(semidet, semidet_rep).
prog_rep__represent_detism(nondet, nondet_rep).
prog_rep__represent_detism(multidet, multidet_rep).
prog_rep__represent_detism(cc_nondet, cc_nondet_rep).
prog_rep__represent_detism(cc_multidet, cc_multidet_rep).
prog_rep__represent_detism(erroneous, erroneous_rep).
prog_rep__represent_detism(failure, failure_rep).

:- pred prog_rep__represent_cons_id(cons_id::in,
	cons_id_rep::out) is det.

prog_rep__represent_cons_id(cons(SymName, _), Rep) :-
	prog_rep__represent_sym_name(SymName, Rep).
prog_rep__represent_cons_id(int_const(Int), Rep) :-
	string__int_to_string(Int, Rep).
prog_rep__represent_cons_id(float_const(Float), Rep) :-
	string__float_to_string(Float, Rep).
prog_rep__represent_cons_id(string_const(String), Rep) :-
	string__append_list(["""", String, """"], Rep).
prog_rep__represent_cons_id(pred_const(_, _, _), Rep) :-
	Rep = "$pred_const".
prog_rep__represent_cons_id(code_addr_const(_, _), Rep) :-
	Rep = "$code_addr_const".
prog_rep__represent_cons_id(type_ctor_info_const(_, _, _), Rep) :-
	Rep = "$type_ctor_info_const".
prog_rep__represent_cons_id(base_typeclass_info_const(_, _, _, _), Rep) :-
	Rep = "$base_typeclass_info_const".
prog_rep__represent_cons_id(tabling_pointer_const(_, _), Rep) :-
	Rep = "$tabling_pointer_const".
prog_rep__represent_cons_id(deep_profiling_proc_static(_), Rep) :-
	Rep = "$deep_profiling_procedure_data".
prog_rep__represent_cons_id(table_io_decl(_), Rep) :-
	Rep = "$table_io_decl".

:- pred prog_rep__represent_sym_name(sym_name::in, string::out) is det.

prog_rep__represent_sym_name(unqualified(String), String).
prog_rep__represent_sym_name(qualified(_, String), String).

%---------------------------------------------------------------------------%

:- pred prog_rep__represent_goal_expr(hlds_goal_expr::in, hlds_goal_info::in,
	instmap::in, prog_rep__info::in, goal_rep::out) is det.

prog_rep__represent_goal_expr(unify(_, _, _, Uni, _), GoalInfo, InstMap0,
		Info, Rep) :-
	(
		Uni = assign(Target, Source),
		term__var_to_int(Target, TargetRep),
		term__var_to_int(Source, SourceRep),
		AtomicGoalRep = unify_assign_rep(TargetRep, SourceRep)
	;
		Uni = construct(Var, ConsId, Args, _, _, _, _),
		term__var_to_int(Var, VarRep),
		prog_rep__represent_cons_id(ConsId, ConsIdRep),
		list__map(term__var_to_int, Args, ArgsRep),
		AtomicGoalRep = unify_construct_rep(VarRep, ConsIdRep, ArgsRep)
	;
		Uni = deconstruct(Var, ConsId, Args, _, _, _),
		term__var_to_int(Var, VarRep),
		prog_rep__represent_cons_id(ConsId, ConsIdRep),
		list__map(term__var_to_int, Args, ArgsRep),
		AtomicGoalRep = unify_deconstruct_rep(VarRep, ConsIdRep,
			ArgsRep)
	;
		Uni = simple_test(Var1, Var2),
		term__var_to_int(Var1, Var1Rep),
		term__var_to_int(Var2, Var2Rep),
		AtomicGoalRep = unify_simple_test_rep(Var1Rep, Var2Rep)
	;
		Uni = complicated_unify(_, _, _),
		error("prog_rep__represent_goal_expr: complicated_unify")
	),
	prog_rep__represent_atomic_goal(GoalInfo, InstMap0, Info,
		DetismRep, FilenameRep, LinenoRep, ChangedVarsRep),
	Rep = atomic_goal_rep(DetismRep, FilenameRep, LinenoRep, ChangedVarsRep,
		AtomicGoalRep).
prog_rep__represent_goal_expr(conj(Goals), _, InstMap0, Info, Rep) :-
	prog_rep__represent_conj(Goals, InstMap0, Info, Reps),
	Rep = conj_rep(Reps).
prog_rep__represent_goal_expr(par_conj(_, _), _, _, _, _) :-
	error("Sorry, not yet implemented:\n\
	parallel conjunctions and declarative debugging").
prog_rep__represent_goal_expr(disj(Goals, _SM), _, InstMap0, Info, Rep)
		:-
	prog_rep__represent_disj(Goals, InstMap0, Info, DisjReps),
	Rep = disj_rep(DisjReps).
prog_rep__represent_goal_expr(not(Goal), _GoalInfo, InstMap0, Info, Rep)
		:-
	prog_rep__represent_goal(Goal, InstMap0, Info, InnerRep),
	Rep = negation_rep(InnerRep).
prog_rep__represent_goal_expr(if_then_else(_, Cond, Then, Else, _SM),
		_, InstMap0, Info, Rep) :-
	prog_rep__represent_goal(Cond, InstMap0, Info, CondRep),
	Cond = _ - CondGoalInfo,
	goal_info_get_instmap_delta(CondGoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap1),
	prog_rep__represent_goal(Then, InstMap1, Info, ThenRep),
	prog_rep__represent_goal(Else, InstMap0, Info, ElseRep),
	Rep = ite_rep(CondRep, ThenRep, ElseRep).
prog_rep__represent_goal_expr(switch(_, _, Cases, _SM), _,
		InstMap0, Info, Rep) :-
	prog_rep__represent_cases(Cases, InstMap0, Info, CaseReps),
	Rep = switch_rep(CaseReps).
prog_rep__represent_goal_expr(some(_, _, Goal), _, InstMap0, Info, Rep)
		:-
	prog_rep__represent_goal(Goal, InstMap0, Info, InnerRep),
	Rep = some_rep(InnerRep).
prog_rep__represent_goal_expr(generic_call(GenericCall, Args, _, _),
		GoalInfo, InstMap0, Info, Rep) :-
	list__map(term__var_to_int, Args, ArgsRep),
	(
		GenericCall = higher_order(PredVar, _, _),
		term__var_to_int(PredVar, PredVarRep),
		AtomicGoalRep = higher_order_call_rep(PredVarRep, ArgsRep)
	;
		GenericCall = class_method(Var, MethodNum, _, _),
		term__var_to_int(Var, VarRep),
		AtomicGoalRep = method_call_rep(VarRep, MethodNum, ArgsRep)
	;
		GenericCall = aditi_builtin(_, _),
		error("Sorry, not yet implemented\n\
		Aditi and declarative debugging")
	),
	prog_rep__represent_atomic_goal(GoalInfo, InstMap0, Info,
		DetismRep, FilenameRep, LinenoRep, ChangedVarsRep),
	Rep = atomic_goal_rep(DetismRep, FilenameRep, LinenoRep,
		ChangedVarsRep, AtomicGoalRep).
prog_rep__represent_goal_expr(call(PredId, _, Args, _, _, _),
		GoalInfo, InstMap0, Info, Rep) :-
	module_info_pred_info(Info^module_info, PredId, PredInfo),
	pred_info_name(PredInfo, PredName),
	list__map(term__var_to_int, Args, ArgsRep),
	AtomicGoalRep = plain_call_rep(PredName, ArgsRep),
	prog_rep__represent_atomic_goal(GoalInfo, InstMap0, Info,
		DetismRep, FilenameRep, LinenoRep, ChangedVarsRep),
	Rep = atomic_goal_rep(DetismRep, FilenameRep, LinenoRep,
		ChangedVarsRep, AtomicGoalRep).
prog_rep__represent_goal_expr(foreign_proc(_,
		_PredId, _, Args, _, _, _),
		GoalInfo, InstMap0, Info, Rep) :-
	list__map(term__var_to_int, Args, ArgsRep),
	AtomicGoalRep = pragma_foreign_code_rep(ArgsRep),
	prog_rep__represent_atomic_goal(GoalInfo, InstMap0, Info,
		DetismRep, FilenameRep, LinenoRep, ChangedVarsRep),
	Rep = atomic_goal_rep(DetismRep, FilenameRep, LinenoRep,
		ChangedVarsRep, AtomicGoalRep).
prog_rep__represent_goal_expr(shorthand(_), _, _, _, _) :-
	% these should have been expanded out by now
	error("prog_rep__represent_goal: unexpected shorthand").

%---------------------------------------------------------------------------%

:- pred prog_rep__represent_conj(hlds_goals::in, instmap::in,
	prog_rep__info::in, list(goal_rep)::out) is det.

prog_rep__represent_conj([], _, _, []).
prog_rep__represent_conj([Goal | Goals], InstMap0, Info, [Rep | Reps]) :-
	prog_rep__represent_goal(Goal, InstMap0, Info, Rep),
	Goal = _ - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap1),
	prog_rep__represent_conj(Goals, InstMap1, Info, Reps).

%---------------------------------------------------------------------------%

:- pred prog_rep__represent_disj(hlds_goals::in, instmap::in,
	prog_rep__info::in, list(goal_rep)::out) is det.

prog_rep__represent_disj([], _, _, []).
prog_rep__represent_disj([Goal | Goals], InstMap0, Info, [Rep | Reps]) :-
	prog_rep__represent_goal(Goal, InstMap0, Info, Rep),
	prog_rep__represent_disj(Goals, InstMap0, Info, Reps).

%---------------------------------------------------------------------------%

:- pred prog_rep__represent_cases(list(case)::in, instmap::in,
	prog_rep__info::in, list(goal_rep)::out) is det.

prog_rep__represent_cases([], _, _, []).
prog_rep__represent_cases([case(_, Goal) | Cases], InstMap0, Info,
		[Rep | Reps]) :-
	prog_rep__represent_goal(Goal, InstMap0, Info, Rep),
	prog_rep__represent_cases(Cases, InstMap0, Info, Reps).

%---------------------------------------------------------------------------%
