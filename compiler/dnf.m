%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

:- module dnf.

:- interface.

:- import_module hlds_module, hlds_pred, set.

:- pred dnf__transform_module(module_info::in, bool::in,
	maybe(set(pred_proc_id))::in, module_info::out) is det.

:- pred dnf__transform_proc(proc_info::in, pred_info::in,
	maybe(set(pred_proc_id))::in, module_info::in, module_info::out,
	proc_info::out, list(pred_id)::in, list(pred_id)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, hlds_data, prog_data, instmap.
:- import_module excess, make_hlds, mode_util.
:- import_module require, map, list, string, int, bool, std_util, term, varset.

	% Traverse the module structure.

dnf__transform_module(ModuleInfo0, TransformAll, MaybeNonAtomic, ModuleInfo1) :-
	module_info_predids(ModuleInfo0, PredIds),
	dnf__transform_preds(PredIds, TransformAll, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo1).

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
			pred_info_get_marker_list(PredInfo, Markers),
			list__member(request(dnf), Markers)
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
		ModuleInfo0, ModuleInfo1, [], NewPredIds),

	% We must look up the pred table again
	% since dnf__transform_procs may have added new predicates
	module_info_preds(ModuleInfo1, PredTable1),
	map__lookup(PredTable1, PredId, PredInfo1),

	pred_info_get_marker_list(PredInfo1, Markers1),
	list__delete_all(Markers1, request(dnf), Markers2),
	pred_info_set_marker_list(PredInfo1, [done(dnf) | Markers2], PredInfo),

	map__det_update(PredTable1, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo).

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

	excess_assignments_proc(ProcInfo0, ModuleInfo0, ProcInfo1),
	dnf__transform_proc(ProcInfo1, PredInfo0, MaybeNonAtomic,
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
	pred_info_get_marker_list(PredInfo0, Markers),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_variables(ProcInfo0, VarSet),
	proc_info_vartypes(ProcInfo0, VarTypes),
	DnfInfo = dnf_info(TVarSet, VarTypes, VarSet, Markers),

	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap),
	dnf__transform_goal(Goal0, InstMap, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo,
		PredName, DnfInfo, Goal, NewPredIds0, NewPredIds),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo).

%-----------------------------------------------------------------------------%

:- type dnf_info --->	dnf_info(
				tvarset,
				map(var, type),
				varset,
				list(marker_status)
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
		GoalExpr0 = some(_, _),
		dnf__transform_conj([Goal0], InstMap0, MaybeNonAtomic,
			ModuleInfo0, ModuleInfo, Base, 0, _, DnfInfo,
			Goals, NewPredIds0, NewPredIds),
		Goal = conj(Goals) - GoalInfo
	;
		GoalExpr0 = not(_),
		dnf__transform_conj([Goal0], InstMap0, MaybeNonAtomic,
			ModuleInfo0, ModuleInfo, Base, 0, _, DnfInfo,
			Goals, NewPredIds0, NewPredIds),
		Goal = conj(Goals) - GoalInfo
	;
		GoalExpr0 = disj(Goals0, SM),
		dnf__transform_disj(Goals0, InstMap0, MaybeNonAtomic,
			ModuleInfo0, ModuleInfo, Base, 0, DnfInfo,
			Goals, NewPredIds0, NewPredIds),
		Goal = disj(Goals, SM) - GoalInfo
	;
		GoalExpr0 = switch(Var, CanFail, Cases0, SM),
		dnf__transform_switch(Cases0, InstMap0, MaybeNonAtomic,
			ModuleInfo0, ModuleInfo, Base, 0, DnfInfo,
			Cases, NewPredIds0, NewPredIds),
		Goal = switch(Var, CanFail, Cases, SM) - GoalInfo
	;
		GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0, SM),
		% XXX should handle nonempty Vars
		dnf__transform_ite(Cond0, Then0, Else0, InstMap0,
			MaybeNonAtomic, ModuleInfo0, ModuleInfo, Base, 0,
			DnfInfo, Cond, Then, Else, NewPredIds0, NewPredIds),
		Goal = if_then_else(Vars, Cond, Then, Else, SM) - GoalInfo
	;
		GoalExpr0 = higher_order_call(_, _, _, _, _),
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
		GoalExpr0 = pragma_c_code(_, _, _, _, _, _, _, _),
		ModuleInfo = ModuleInfo0,
		NewPredIds = NewPredIds0,
		Goal = Goal0
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
		ModuleInfo0, ModuleInfo1, Base, Counter0, Counter1, DnfInfo,
		Cond, NewPredIds0, NewPredIds1),
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
		ModuleInfo0, ModuleInfo1, Base, Counter0, Counter1, DnfInfo,
		Goal, NewPredIds0, NewPredIds1),
	Goal0 = _ - GoalInfo0,
	goal_info_get_instmap_delta(GoalInfo0, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap1),
	dnf__transform_conj(Goals0, InstMap1, MaybeNonAtomic,
		ModuleInfo1, ModuleInfo, Base, Counter1, Counter, DnfInfo,
		Goals, NewPredIds1, NewPredIds).

:- pred dnf__make_goal_literal(hlds_goal::in, instmap::in,
	maybe(set(pred_proc_id))::in, module_info::in, module_info::out,
	string::in, int::in, int::out, dnf_info::in, hlds_goal::out,
	list(pred_id)::in, list(pred_id)::out) is det.

dnf__make_goal_literal(Goal0, InstMap0, MaybeNonAtomic,
		ModuleInfo0, ModuleInfo, Base, Counter0, Counter, DnfInfo,
		Goal, NewPredIds0, NewPredIds) :-
	( dnf__is_considered_literal_goal(Goal0, MaybeNonAtomic) ->
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
	DnfInfo = dnf_info(TVarSet, VarTypes, VarSet, Markers),
	Goal0 = _GoalExpr - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap),

	goal_info_get_context(GoalInfo, Context),
	goal_info_get_determinism(GoalInfo, Detism),
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	set__to_sorted_list(NonLocals, ArgVars),
	dnf__compute_arg_types_modes(ArgVars, VarTypes, InstMap0, InstMap,
		ArgTypes, ArgModes),

	module_info_name(ModuleInfo0, ModuleName),
	SymName = qualified(ModuleName, PredName),
	map__init(TVarMap), % later, polymorphism.m will fill this in. 
	proc_info_create(VarSet, VarTypes, ArgVars, ArgModes, Detism,
		Goal0, Context, TVarMap, ProcInfo),
	pred_info_create(ModuleName, SymName, TVarSet, ArgTypes, true,
		Context, local, Markers, predicate, ProcInfo, ProcId, PredInfo),

	module_info_get_predicate_table(ModuleInfo0, PredTable0),
	predicate_table_insert(PredTable0, PredInfo, PredId,
		PredTable),
	module_info_set_predicate_table(ModuleInfo0, PredTable,
		ModuleInfo),

	GoalExpr = call(PredId, ProcId, ArgVars, not_builtin, no, SymName),
	Goal = GoalExpr - GoalInfo.

:- pred dnf__compute_arg_types_modes(list(var)::in, map(var, type)::in,
	instmap::in, instmap::in, list(type)::out, list(mode)::out) is det.

dnf__compute_arg_types_modes([], _, _, _, [], []).
dnf__compute_arg_types_modes([Var | Vars], VarTypes, InstMap0, InstMap,
		[Type | Types], [Mode | Modes]) :-
	map__lookup(VarTypes, Var, Type),
	instmap__lookup_var(InstMap0, Var, Inst0),
	instmap__lookup_var(InstMap, Var, Inst),
	Mode = (Inst0 -> Inst),
	dnf__compute_arg_types_modes(Vars, VarTypes, InstMap0, InstMap,
		Types, Modes).

%-----------------------------------------------------------------------------%

:- pred dnf__is_considered_literal_goal(hlds_goal::in,
	maybe(set(pred_proc_id))::in) is semidet.

dnf__is_considered_literal_goal(GoalExpr - _, MaybeNonAtomic) :-
	( GoalExpr = not(SubGoalExpr - _) ->
		dnf__is_considered_atomic_expr(SubGoalExpr, MaybeNonAtomic)
	;
		dnf__is_considered_atomic_expr(GoalExpr, MaybeNonAtomic)
	).

:- pred dnf__is_considered_atomic_expr(hlds_goal_expr::in,
	maybe(set(pred_proc_id))::in) is semidet.

dnf__is_considered_atomic_expr(GoalExpr, MaybeNonAtomic) :-
	(
		dnf__is_atomic_expr(GoalExpr, yes)
	;
		MaybeNonAtomic = yes(NonAtomic),
		dnf__expr_free_of_nonatomic(GoalExpr, NonAtomic)
	).

:- pred dnf__is_atomic_expr(hlds_goal_expr::in, bool::out) is det.

dnf__is_atomic_expr(conj(_), no).
dnf__is_atomic_expr(higher_order_call(_, _, _, _, _), yes).
dnf__is_atomic_expr(call(_, _, _, _, _, _), yes).
dnf__is_atomic_expr(switch(_, _, _, _), no).
dnf__is_atomic_expr(unify(_, _, _, _, _), yes).
dnf__is_atomic_expr(disj(_, _), no).
dnf__is_atomic_expr(not(_), no).
dnf__is_atomic_expr(some(_, GoalExpr - _), IsAtomic) :-
	dnf__is_atomic_expr(GoalExpr, IsAtomic).
dnf__is_atomic_expr(if_then_else(_, _, _, _, _), no).
dnf__is_atomic_expr(pragma_c_code(_, _, _, _, _, _, _, _), yes).

:- pred dnf__expr_free_of_nonatomic(hlds_goal_expr::in,
	set(pred_proc_id)::in) is semidet.

dnf__expr_free_of_nonatomic(conj(Goals), NonAtomic) :-
	dnf__goals_free_of_nonatomic(Goals, NonAtomic).
dnf__expr_free_of_nonatomic(call(PredId, ProcId, _, _, _, _), NonAtomic) :-
	\+ set__member(proc(PredId, ProcId), NonAtomic).
dnf__expr_free_of_nonatomic(switch(_, _, Cases, _), NonAtomic) :-
	dnf__cases_free_of_nonatomic(Cases, NonAtomic).
dnf__expr_free_of_nonatomic(unify(_, _, _, _, _), _NonAtomic).
dnf__expr_free_of_nonatomic(disj(Goals, _), NonAtomic) :-
	dnf__goals_free_of_nonatomic(Goals, NonAtomic).
dnf__expr_free_of_nonatomic(not(Goal), NonAtomic) :-
	dnf__goal_free_of_nonatomic(Goal, NonAtomic).
dnf__expr_free_of_nonatomic(some(_, Goal), NonAtomic) :-
	dnf__goal_free_of_nonatomic(Goal, NonAtomic).
dnf__expr_free_of_nonatomic(if_then_else(_, Cond, Then, Else, _), NonAtomic) :-
	dnf__goal_free_of_nonatomic(Cond, NonAtomic),
	dnf__goal_free_of_nonatomic(Then, NonAtomic),
	dnf__goal_free_of_nonatomic(Else, NonAtomic).
dnf__expr_free_of_nonatomic(pragma_c_code(_, _, _, _, _, _, _, _), _NonAtomic).

:- pred dnf__goal_free_of_nonatomic(hlds_goal::in,
	set(pred_proc_id)::in) is semidet.

dnf__goal_free_of_nonatomic(GoalExpr - _, NonAtomic) :-
	dnf__expr_free_of_nonatomic(GoalExpr, NonAtomic).

:- pred dnf__goals_free_of_nonatomic(list(hlds_goal)::in,
	set(pred_proc_id)::in) is semidet.

dnf__goals_free_of_nonatomic([], _NonAtomic).
dnf__goals_free_of_nonatomic([Goal | Goals], NonAtomic) :-
	dnf__goal_free_of_nonatomic(Goal, NonAtomic),
	dnf__goals_free_of_nonatomic(Goals, NonAtomic).

:- pred dnf__cases_free_of_nonatomic(list(case)::in,
	set(pred_proc_id)::in) is semidet.

dnf__cases_free_of_nonatomic([], _NonAtomic).
dnf__cases_free_of_nonatomic([case(_, Goal) | Cases], NonAtomic) :-
	dnf__goal_free_of_nonatomic(Goal, NonAtomic),
	dnf__cases_free_of_nonatomic(Cases, NonAtomic).

%-----------------------------------------------------------------------------%
