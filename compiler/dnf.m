%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% A module for transforming procedure bodies into disjunctive normal form.
% This transformation is necessary for the application of several deductive
% database type transformation algorithms, and is also helpful when one
% wants to produce Prolog code with good performance.
%
% Author: zs.
%
%-----------------------------------------------------------------------------%
%
% The transformation to dnf in general requires the introduction of new
% predicates for the parts of a procedure body that do not fit the dnf pattern.
%
% The dnf pattern has three levels:
%
% The top goal must be a disjunction, a switch or an if then else.
% The middle levels must be conjunctions (possibly of a singleton list).
% The bottom levels must consist of "literal" goals.
%
% A literal goal is an atomic goal, or a negated atomic goal.
%
% Calls and unifications are atomic goals. Existential quantification of
% a call or unification is an atomic goal.
% A call and some test unifications on the output is an atomic goal. If these
% are not made atomic, magic.m just recreates the tests anyway.
%
% The main predicate of this module allows callers to specify that *any*
% goal should be considered atomic unless it involves calls to certain 
% specified predicates. This allows e.g. the magic set transformation to
% consider all goals that do not refer to database predicates to be atomic.
%
% dnf__transform_proc transforms one procedure into dnf. dnf__transform_module
% transforms either all procedures in the module (if the second argument is
% set to 'yes'), or just the procedures belonging to predicates with the
% 'dnf' marker.
%
%-----------------------------------------------------------------------------%

:- module aditi_backend__dnf.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_pred.
:- import_module set, list, bool, std_util.

:- pred dnf__transform_module(module_info::in, bool::in,
	maybe(set(pred_proc_id))::in, module_info::out) is det.

:- pred dnf__transform_proc(proc_info::in, pred_info::in,
	maybe(set(pred_proc_id))::in, module_info::in, module_info::out,
	proc_info::out, list(pred_id)::in, list(pred_id)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ll_backend__code_aux, ll_backend__code_util, hlds__hlds_goal.
:- import_module hlds__hlds_data, parse_tree__prog_data, hlds__instmap.
:- import_module transform_hlds__dependency_graph, check_hlds__det_analysis.
:- import_module check_hlds__mode_util.
:- import_module require, map, list, string, int, bool, std_util, term, varset.

	% Traverse the module structure.

dnf__transform_module(ModuleInfo0, TransformAll, MaybeNonAtomic, ModuleInfo) :-
	module_info_predids(ModuleInfo0, PredIds),
	dnf__transform_preds(PredIds, TransformAll, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo1),
	% The dependency_graph information is now incorrect.
	module_info_clobber_dependency_info(ModuleInfo1, ModuleInfo).

:- pred dnf__transform_preds(list(pred_id)::in, bool::in,
	maybe(set(pred_proc_id))::in, module_info::in, module_info::out) is det.

dnf__transform_preds([], _, _, ModuleInfo, ModuleInfo).
dnf__transform_preds([PredId | PredIds0], TransformAll, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo) :-
	(
		(
			TransformAll = yes
		;
			module_info_preds(ModuleInfo0, PredTable),
			map__lookup(PredTable, PredId, PredInfo),
			pred_info_get_markers(PredInfo, Markers),
			( check_marker(Markers, dnf)
			; check_marker(Markers, aditi)
			)
		)
	->
		dnf__transform_pred(PredId, MaybeNonAtomic,
			ModuleInfo0, ModuleInfo1, NewPredIds),
		list__append(NewPredIds, PredIds0, PredIds1)
	;
		PredIds1 = PredIds0,
		ModuleInfo1 = ModuleInfo0
	),
	dnf__transform_preds(PredIds1, TransformAll, MaybeNonAtomic,
		ModuleInfo1, ModuleInfo).

:- pred dnf__transform_pred(pred_id::in, maybe(set(pred_proc_id))::in,
	module_info::in, module_info::out, list(pred_id)::out) is det.

dnf__transform_pred(PredId, MaybeNonAtomic, ModuleInfo0, ModuleInfo,
		NewPredIds) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_non_imported_procids(PredInfo0, ProcIds),
	dnf__transform_procs(ProcIds, PredId, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo, [], NewPredIds).

:- pred dnf__transform_procs(list(proc_id)::in, pred_id::in,
	maybe(set(pred_proc_id))::in, module_info::in, module_info::out,
	list(pred_id)::in, list(pred_id)::out) is det.

dnf__transform_procs([], _, _, ModuleInfo, ModuleInfo, NewPredIds, NewPredIds).
dnf__transform_procs([ProcId | ProcIds], PredId, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo, NewPredIds0, NewPredIds) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	dnf__transform_proc(ProcInfo0, PredInfo0, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo1, ProcInfo, NewPredIds0, NewPredIds1),

	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	% We must look up the pred table again
	% since dnf__transform_proc may have added new predicates
	module_info_preds(ModuleInfo1, PredTable1),
	map__det_update(PredTable1, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo2),

	dnf__transform_procs(ProcIds, PredId, MaybeNonAtomic,
		ModuleInfo2, ModuleInfo, NewPredIds1, NewPredIds).

dnf__transform_proc(ProcInfo0, PredInfo0, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo, ProcInfo, NewPredIds0, NewPredIds) :-
	pred_info_name(PredInfo0, PredName),
	pred_info_typevarset(PredInfo0, TVarSet),
	pred_info_get_markers(PredInfo0, Markers),
	pred_info_get_class_context(PredInfo0, ClassContext),
	pred_info_get_aditi_owner(PredInfo0, Owner),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_varset(ProcInfo0, VarSet),
	proc_info_inst_varset(ProcInfo0, InstVarSet),
	proc_info_vartypes(ProcInfo0, VarTypes),
	proc_info_typeinfo_varmap(ProcInfo0, TVarMap),
	proc_info_typeclass_info_varmap(ProcInfo0, TCVarMap),
	DnfInfo = dnf_info(TVarSet, VarTypes, ClassContext, 
			VarSet, InstVarSet, Markers, TVarMap, TCVarMap, Owner),

	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap),
	dnf__transform_goal(Goal0, InstMap, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo,
		PredName, DnfInfo, Goal, NewPredIds0, NewPredIds),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo).

%-----------------------------------------------------------------------------%

:- type dnf_info --->	dnf_info(
				tvarset,
				map(prog_var, type),
				class_constraints,
				prog_varset,
				inst_varset,
				pred_markers,
				map(tvar, type_info_locn),
				map(class_constraint, prog_var),
				aditi_owner
			).

:- pred dnf__transform_goal(hlds_goal::in, instmap::in,
	maybe(set(pred_proc_id))::in, module_info::in, module_info::out,
	string::in, dnf_info::in, hlds_goal::out,
	list(pred_id)::in, list(pred_id)::out) is det.

dnf__transform_goal(Goal0, InstMap0, MaybeNonAtomic, ModuleInfo0, ModuleInfo,
		Base, DnfInfo, Goal, NewPredIds0, NewPredIds) :-
	Goal0 = GoalExpr0 - GoalInfo,
	(
		GoalExpr0 = conj(Goals0),
		dnf__transform_conj(Goals0, InstMap0, MaybeNonAtomic,
			ModuleInfo0, ModuleInfo, Base, 0, _, DnfInfo,
			Goals, NewPredIds0, NewPredIds),
		Goal = conj(Goals) - GoalInfo
	;
		GoalExpr0 = par_conj(_Goals0),
		error("sorry, dnf of parallel conjunction not implemented")
	;
		GoalExpr0 = some(Vars, CanRemove, SomeGoal0),
		dnf__make_goal_literal(SomeGoal0, InstMap0, MaybeNonAtomic,
			ModuleInfo0, ModuleInfo, no, yes, Base, 0, _, 
			DnfInfo, SomeGoal, NewPredIds0, NewPredIds),
		Goal = some(Vars, CanRemove, SomeGoal) - GoalInfo
	;
		GoalExpr0 = not(NegGoal0),
		dnf__make_goal_literal(NegGoal0, InstMap0, MaybeNonAtomic,
			ModuleInfo0, ModuleInfo, yes, no, Base, 0, _, 
			DnfInfo, NegGoal, NewPredIds0, NewPredIds),
		Goal = not(NegGoal) - GoalInfo
	;
		GoalExpr0 = disj(Goals0),
		dnf__transform_disj(Goals0, InstMap0, MaybeNonAtomic,
			ModuleInfo0, ModuleInfo, Base, 0, DnfInfo,
			Goals, NewPredIds0, NewPredIds),
		Goal = disj(Goals) - GoalInfo
	;
		GoalExpr0 = switch(Var, CanFail, Cases0),
		dnf__transform_switch(Cases0, InstMap0, MaybeNonAtomic,
			ModuleInfo0, ModuleInfo, Base, 0, DnfInfo,
			Cases, NewPredIds0, NewPredIds),
		Goal = switch(Var, CanFail, Cases) - GoalInfo
	;
		GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
		% XXX should handle nonempty Vars
		dnf__transform_ite(Cond0, Then0, Else0, InstMap0,
			MaybeNonAtomic, ModuleInfo0, ModuleInfo, Base, 0,
			DnfInfo, Cond, Then, Else, NewPredIds0, NewPredIds),
		Goal = if_then_else(Vars, Cond, Then, Else) - GoalInfo
	;
		GoalExpr0 = generic_call(_, _, _, _),
		ModuleInfo = ModuleInfo0,
		NewPredIds = NewPredIds0,
		Goal = Goal0
	;
		GoalExpr0 = call(_, _, _, _, _, _),
		ModuleInfo = ModuleInfo0,
		NewPredIds = NewPredIds0,
		Goal = Goal0
	;
		GoalExpr0 = unify(_, _, _, _, _),
		ModuleInfo = ModuleInfo0,
		NewPredIds = NewPredIds0,
		Goal = Goal0
	;
		GoalExpr0 = foreign_proc(_, _, _, _, _, _, _),
		ModuleInfo = ModuleInfo0,
		NewPredIds = NewPredIds0,
		Goal = Goal0
	;
		GoalExpr0 = shorthand(_),
		% these should have been expanded out by now
		error("dnf__transform_goal: unexpected shorthand")
	).

%-----------------------------------------------------------------------------%

:- pred dnf__transform_disj(list(hlds_goal)::in, instmap::in,
	maybe(set(pred_proc_id))::in, module_info::in, module_info::out,
	string::in, int::in, dnf_info::in, list(hlds_goal)::out,
	list(pred_id)::in, list(pred_id)::out) is det.

dnf__transform_disj([], _, _, ModuleInfo, ModuleInfo, _, _, _, [],
		NewPredIds, NewPredIds).
dnf__transform_disj([Goal0 | Goals0], InstMap0, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo, Base, Counter0, DnfInfo,
		[Goal | Goals], NewPredIds0, NewPredIds) :-
	Goal0 = _ - GoalInfo,
	goal_to_conj_list(Goal0, ConjList0),
	dnf__transform_conj(ConjList0, InstMap0, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo1, Base, Counter0, Counter1, DnfInfo,
		ConjList, NewPredIds0, NewPredIds1),
	conj_list_to_goal(ConjList, GoalInfo, Goal),
	dnf__transform_disj(Goals0, InstMap0, MaybeNonAtomic,
		ModuleInfo1, ModuleInfo, Base, Counter1, DnfInfo,
		Goals, NewPredIds1, NewPredIds).

:- pred dnf__transform_switch(list(case)::in, instmap::in,
	maybe(set(pred_proc_id))::in, module_info::in, module_info::out,
	string::in, int::in, dnf_info::in, list(case)::out,
	list(pred_id)::in, list(pred_id)::out) is det.

dnf__transform_switch([], _, _, ModuleInfo, ModuleInfo, _, _, _, [],
		NewPredIds, NewPredIds).
dnf__transform_switch([Case0 | Cases0], InstMap0, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo, Base, Counter0, DnfInfo,
		[Case | Cases], NewPredIds0, NewPredIds) :-
	Case0 = case(ConsId, Goal0),
	goal_to_conj_list(Goal0, ConjList0),
	% XXX should adjust instmap to account for binding of switch variable
	dnf__transform_conj(ConjList0, InstMap0, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo1, Base, Counter0, Counter1, DnfInfo,
		ConjList, NewPredIds0, NewPredIds1),
	Goal0 = _ - GoalInfo,
	conj_list_to_goal(ConjList, GoalInfo, Goal),
	Case = case(ConsId, Goal),
	dnf__transform_switch(Cases0, InstMap0, MaybeNonAtomic,
		ModuleInfo1, ModuleInfo, Base, Counter1, DnfInfo,
		Cases, NewPredIds1, NewPredIds).

:- pred dnf__transform_ite(hlds_goal::in, hlds_goal::in, hlds_goal::in,
	instmap::in, maybe(set(pred_proc_id))::in, module_info::in,
	module_info::out, string::in, int::in, dnf_info::in,
	hlds_goal::out, hlds_goal::out, hlds_goal::out,
	list(pred_id)::in, list(pred_id)::out) is det.

dnf__transform_ite(Cond0, Then0, Else0, InstMap0, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo, Base, Counter0, DnfInfo,
		Cond, Then, Else, NewPredIds0, NewPredIds) :-
	Cond0 = _ - CondInfo,
	dnf__make_goal_literal(Cond0, InstMap0, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo1, yes, no, Base, Counter0, Counter1,
		DnfInfo, Cond, NewPredIds0, NewPredIds1),
	goal_info_get_instmap_delta(CondInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap1),
	Then0 = _ - ThenInfo,
	goal_to_conj_list(Then0, ThenList0),
	dnf__transform_conj(ThenList0, InstMap1, MaybeNonAtomic,
		ModuleInfo1, ModuleInfo2, Base, Counter1, Counter2, DnfInfo,
		ThenList, NewPredIds1, NewPredIds2),
	conj_list_to_goal(ThenList, ThenInfo, Then),
	Else0 = _ - ElseInfo,
	goal_to_conj_list(Else0, ElseList0),
	dnf__transform_conj(ElseList0, InstMap0, MaybeNonAtomic,
		ModuleInfo2, ModuleInfo, Base, Counter2, _, DnfInfo,
		ElseList, NewPredIds2, NewPredIds),
	conj_list_to_goal(ElseList, ElseInfo, Else).

%-----------------------------------------------------------------------------%

:- pred dnf__transform_conj(list(hlds_goal)::in, instmap::in,
	maybe(set(pred_proc_id))::in, module_info::in, module_info::out,
	string::in, int::in, int::out, dnf_info::in, list(hlds_goal)::out,
	list(pred_id)::in, list(pred_id)::out) is det.

dnf__transform_conj([], _, _, ModuleInfo, ModuleInfo, _, Counter, Counter,
		_, [], NewPreds, NewPreds).
dnf__transform_conj([Goal0 | Goals0], InstMap0, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo, Base, Counter0, Counter, DnfInfo,
		[Goal | Goals], NewPredIds0, NewPredIds) :-
	dnf__make_goal_literal(Goal0, InstMap0, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo1, no, no, Base, Counter0, Counter1,
		DnfInfo, Goal, NewPredIds0, NewPredIds1),
	Goal0 = _ - GoalInfo0,
	goal_info_get_instmap_delta(GoalInfo0, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap1),
	dnf__transform_conj(Goals0, InstMap1, MaybeNonAtomic,
		ModuleInfo1, ModuleInfo, Base, Counter1, Counter, DnfInfo,
		Goals, NewPredIds1, NewPredIds).

%-----------------------------------------------------------------------------%

:- pred dnf__make_goal_literal(hlds_goal::in, instmap::in,
	maybe(set(pred_proc_id))::in, module_info::in, module_info::out,
	bool::in, bool::in, string::in, int::in, int::out, dnf_info::in,
	hlds_goal::out, list(pred_id)::in, list(pred_id)::out) is det.

dnf__make_goal_literal(Goal0, InstMap0, MaybeNonAtomic, ModuleInfo0,
		ModuleInfo, InNeg, InSome, Base, Counter0, Counter,
		DnfInfo, Goal, NewPredIds0, NewPredIds) :-
	(
		dnf__is_considered_literal_goal(Goal0,
			InNeg, InSome, MaybeNonAtomic)
	->
		Goal = Goal0,
		Counter = Counter0,
		ModuleInfo = ModuleInfo0,
		NewPredIds = NewPredIds0
	;
		module_info_get_predicate_table(ModuleInfo0, PredTable0),
		dnf__get_new_pred_name(PredTable0, Base, Name,
			Counter0, Counter),
		dnf__define_new_pred(Goal0, Goal, InstMap0, Name, DnfInfo,
			ModuleInfo0, ModuleInfo, NewPredId),
		NewPredIds = [NewPredId | NewPredIds0]
	).

:- pred dnf__get_new_pred_name(predicate_table::in, string::in, string::out,
	int::in, int::out) is det.

dnf__get_new_pred_name(PredTable, Base, Name, N0, N) :-
	string__int_to_string(N0, Suffix),
	string__append_list([Base, "__part_", Suffix], TrialName),
	N1 is N0 + 1,
	( predicate_table_search_name(PredTable, TrialName, _) ->
		dnf__get_new_pred_name(PredTable, Base, Name, N1, N)
	;
		Name = TrialName,
		N = N1
	).

:- pred dnf__define_new_pred(hlds_goal::in, hlds_goal::out, instmap::in,
	string::in, dnf_info::in, module_info::in, module_info::out,
	pred_id::out) is det.

dnf__define_new_pred(Goal0, Goal, InstMap0, PredName, DnfInfo,
		ModuleInfo0, ModuleInfo, PredId) :-
	DnfInfo = dnf_info(TVarSet, VarTypes, ClassContext, 
			VarSet, InstVarSet, Markers, TVarMap, TCVarMap, Owner),
	Goal0 = _GoalExpr - GoalInfo,
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	set__to_sorted_list(NonLocals, ArgVars),
		% This ClassContext is a conservative approximation.
		% We could get rid of some constraints on variables
		% that are not part of the goal.
	hlds_pred__define_new_pred(Goal0, Goal, ArgVars, _, InstMap0, PredName,
		TVarSet, VarTypes, ClassContext, TVarMap, TCVarMap,
		VarSet, InstVarSet, Markers, Owner, address_is_not_taken,
		ModuleInfo0, ModuleInfo, PredProcId),
	PredProcId = proc(PredId, _).

%-----------------------------------------------------------------------------%

:- pred dnf__is_considered_literal_goal(hlds_goal::in, bool::in, bool::in,
	maybe(set(pred_proc_id))::in) is semidet.

dnf__is_considered_literal_goal(Goal, InNeg, InSome, MaybeNonAtomic) :-
	( Goal = not(SubGoal) - _ ->
		InNeg = no,
		dnf__is_considered_atomic(SubGoal,
			yes, InSome, MaybeNonAtomic)
	;
		dnf__is_considered_atomic(Goal,
			InNeg, InSome, MaybeNonAtomic)
	).

:- pred dnf__is_considered_atomic(hlds_goal::in, bool::in, bool::in,
	maybe(set(pred_proc_id))::in) is semidet.

dnf__is_considered_atomic(Goal, InNeg, InSome, MaybeNonAtomic) :-
	(
		Goal = GoalExpr - _,
		dnf__is_atomic_expr(MaybeNonAtomic, InNeg, InSome,
			GoalExpr, yes)
	;
		MaybeNonAtomic = yes(NonAtomic),
		dnf__free_of_nonatomic(Goal, NonAtomic)
	).

:- pred dnf__is_atomic_expr(maybe(set(pred_proc_id))::in, bool::in, bool::in,
		hlds_goal_expr::in, bool::out) is det.

dnf__is_atomic_expr(_, _, _, conj([]), yes).
	% Don't transform a call and some atomic tests on the outputs, since
	% magic.m will just create another copy of the tests, adding some extra 
	% overhead. This form of conjunction commonly occurs for calls 
	% in implied modes.
dnf__is_atomic_expr(MaybeNonAtomic, _, _, conj([Call | Tests]), IsAtomic) :-
	(
		Call = call(_, _, _, _, _, _) - _,
		MaybeNonAtomic = yes(NonAtomic),	
		dnf__goals_free_of_nonatomic(Tests, NonAtomic)
	->
		IsAtomic = yes 
	;
		IsAtomic = no
	).
dnf__is_atomic_expr(_, _, _, par_conj(_), no).
dnf__is_atomic_expr(_, _, _, generic_call(_, _, _, _), yes).
dnf__is_atomic_expr(_, _, _, call(_, _, _, _, _, _), yes).
dnf__is_atomic_expr(_, _, _, switch(_, _, _), no).
dnf__is_atomic_expr(_, _, _, unify(_, _, _, _, _), yes).
dnf__is_atomic_expr(_, _, _, disj(_), no).
dnf__is_atomic_expr(MaybeNonAtomic, InNeg, InSome, not(NegGoalExpr - _),
		IsAtomic) :-
	( InNeg = no ->
		dnf__is_atomic_expr(MaybeNonAtomic, yes, InSome,
			NegGoalExpr, IsAtomic)
	;
		IsAtomic = no
	).
dnf__is_atomic_expr(MaybeNonAtomic, InNeg, InSome,
		some(_, _, GoalExpr - _), IsAtomic) :-
	( InSome = no ->
		dnf__is_atomic_expr(MaybeNonAtomic, InNeg, yes,
			GoalExpr, IsAtomic)
	;
		IsAtomic = no
	).
dnf__is_atomic_expr(_, _, _, if_then_else(_, _, _, _), no).
dnf__is_atomic_expr(_, _, _, foreign_proc(_, _, _, _, _, _, _), yes).
dnf__is_atomic_expr(MaybeNonAtomic, InNeg, InSome, shorthand(ShorthandGoal),
		IsAtomic) :-
	dnf__is_atomic_expr_shorthand(MaybeNonAtomic, InNeg, InSome,
			ShorthandGoal, IsAtomic).


:- pred dnf__is_atomic_expr_shorthand(maybe(set(pred_proc_id))::in, bool::in,
		bool::in, shorthand_goal_expr::in, bool::out) is det.

dnf__is_atomic_expr_shorthand(_, _, _, bi_implication(_,_), no).


:- pred dnf__free_of_nonatomic(hlds_goal::in,
	set(pred_proc_id)::in) is semidet.

dnf__free_of_nonatomic(conj(Goals) - _, NonAtomic) :-
	dnf__goals_free_of_nonatomic(Goals, NonAtomic).
dnf__free_of_nonatomic(par_conj(Goals) - _, NonAtomic) :-
	dnf__goals_free_of_nonatomic(Goals, NonAtomic).
dnf__free_of_nonatomic(call(PredId, ProcId, _, _, _, _) - _, NonAtomic) :-
	\+ set__member(proc(PredId, ProcId), NonAtomic).
dnf__free_of_nonatomic(switch(_, _, Cases) - _, NonAtomic) :-
	dnf__cases_free_of_nonatomic(Cases, NonAtomic).
dnf__free_of_nonatomic(unify(_, _, _, Uni, _) - _, NonAtomic) :-
	\+ (
		Uni = construct(_, pred_const(PredId, ProcId, _),
			_, _, _, _, _),
		set__member(proc(PredId, ProcId), NonAtomic)
	).
dnf__free_of_nonatomic(disj(Goals) - GoalInfo, NonAtomic) :-
		% For Aditi, nondet disjunctions are non-atomic, 
		% no matter what they contain.
	goal_info_get_determinism(GoalInfo, Detism),
	\+ determinism_components(Detism, _, at_most_many),
	dnf__goals_free_of_nonatomic(Goals, NonAtomic).
dnf__free_of_nonatomic(not(Goal) - _, NonAtomic) :-
	dnf__free_of_nonatomic(Goal, NonAtomic).
dnf__free_of_nonatomic(some(_, _, Goal) - _, NonAtomic) :-
	dnf__free_of_nonatomic(Goal, NonAtomic).
dnf__free_of_nonatomic(if_then_else(_, Cond, Then, Else) - GoalInfo, 
		NonAtomic) :-
		% For Aditi, nondet if-then-elses are non-atomic, 
		% no matter what they contain.
	goal_info_get_determinism(GoalInfo, Detism),
	\+ determinism_components(Detism, _, at_most_many),
	dnf__free_of_nonatomic(Cond, NonAtomic),
	dnf__free_of_nonatomic(Then, NonAtomic),
	dnf__free_of_nonatomic(Else, NonAtomic).
dnf__free_of_nonatomic(foreign_proc(_, _, _, _, _, _, _) - _,
		_NonAtomic).

:- pred dnf__goals_free_of_nonatomic(list(hlds_goal)::in,
	set(pred_proc_id)::in) is semidet.

dnf__goals_free_of_nonatomic([], _NonAtomic).
dnf__goals_free_of_nonatomic([Goal | Goals], NonAtomic) :-
	dnf__free_of_nonatomic(Goal, NonAtomic),
	dnf__goals_free_of_nonatomic(Goals, NonAtomic).

:- pred dnf__cases_free_of_nonatomic(list(case)::in,
	set(pred_proc_id)::in) is semidet.

dnf__cases_free_of_nonatomic([], _NonAtomic).
dnf__cases_free_of_nonatomic([case(_, Goal) | Cases], NonAtomic) :-
	dnf__free_of_nonatomic(Goal, NonAtomic),
	dnf__cases_free_of_nonatomic(Cases, NonAtomic).

%-----------------------------------------------------------------------------%
