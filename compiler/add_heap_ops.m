%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: fjh.
%
% This module is an HLDS-to-HLDS transformation that inserts code to
% handle heap reclamation on backtracking, by saving and restoring
% the values of the heap pointer.
% The transformation involves adding calls to impure
% predicates defined in library/private_builtin.m, which in turn call
% the MR_mark_hp() and MR_restore_hp() macros defined in
% runtime/mercury_heap.h.
%
% This pass is currently only used for the MLDS back-end.
% For some reason (perhaps efficiency?? or more likely just historical?),
% the LLDS back-end inserts the heap operations as it is generating
% LLDS code, rather than via an HLDS to HLDS transformation.
%
% This module is very similar to add_trail_ops.m.
%
%-----------------------------------------------------------------------------%

% XXX check goal_infos for correctness

%-----------------------------------------------------------------------------%

:- module add_heap_ops.
:- interface.
:- import_module hlds_pred, hlds_module.

:- pred add_heap_ops(proc_info::in, module_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prog_data, prog_util, (inst).
:- import_module hlds_goal, hlds_data.
:- import_module goal_util, quantification, modules, type_util.
:- import_module instmap, code_model, code_util.

:- import_module bool, string.
:- import_module assoc_list, list, map, set, varset, std_util, require, term.


%
% As we traverse the goal, we add new variables to hold the
% saved values of the heap pointer.
% So we need to thread a varset and a vartypes mapping through,
% to record the names and types of the new variables.
%
% We also keep the module_info around, so that we can use
% the predicate table that it contains to lookup the pred_ids
% for the builtin procedures that we insert calls to.
% We do not update the module_info as we're traversing the goal.
%

:- type heap_ops_info --->
	heap_ops_info(
		varset :: prog_varset,
		var_types :: vartypes,
		module_info :: module_info
	).

add_heap_ops(Proc0, ModuleInfo0, Proc) :-
	proc_info_goal(Proc0, Goal0),
	proc_info_varset(Proc0, VarSet0),
	proc_info_vartypes(Proc0, VarTypes0),
	TrailOpsInfo0 = heap_ops_info(VarSet0, VarTypes0, ModuleInfo0),
	goal_add_heap_ops(Goal0, Goal, TrailOpsInfo0, TrailOpsInfo),
	TrailOpsInfo = heap_ops_info(VarSet, VarTypes, _),
	proc_info_set_goal(Proc0, Goal, Proc1),
	proc_info_set_varset(Proc1, VarSet, Proc2),
	proc_info_set_vartypes(Proc2, VarTypes, Proc3),
	% The code below does not maintain the non-local variables,
	% so we need to requantify.
	% XXX it would be more efficient to maintain them
	%     rather than recomputing them every time.
	requantify_proc(Proc3, Proc).

:- pred goal_add_heap_ops(hlds_goal::in, hlds_goal::out,
		heap_ops_info::in, heap_ops_info::out) is det.

goal_add_heap_ops(GoalExpr0 - GoalInfo, Goal) -->
	goal_expr_add_heap_ops(GoalExpr0, GoalInfo, Goal).

:- pred goal_expr_add_heap_ops(hlds_goal_expr::in, hlds_goal_info::in,
		hlds_goal::out,
		heap_ops_info::in, heap_ops_info::out) is det.

goal_expr_add_heap_ops(conj(Goals0), GI, conj(Goals) - GI) -->
	conj_add_heap_ops(Goals0, Goals).

goal_expr_add_heap_ops(par_conj(Goals0, SM), GI, par_conj(Goals, SM) - GI) -->
	conj_add_heap_ops(Goals0, Goals).

goal_expr_add_heap_ops(disj([], B), GI, disj([], B) - GI) --> [].

goal_expr_add_heap_ops(disj(Goals0, B), GoalInfo, Goal - GoalInfo) -->
	{ Goals0 = [FirstDisjunct | _] },

	{ goal_info_get_context(GoalInfo, Context) },
	{ goal_info_get_code_model(GoalInfo, CodeModel) },

	%
	% If necessary, save the heap pointer so that we can
	% restore it on back-tracking.
	% We don't need to do this here if it is a model_det or model_semi
	% disjunction and the first disjunct won't allocate any heap --
	% in that case, we delay saving the heap pointer until just before
	% the first disjunct that might allocate heap.
	%
	(
		{ CodeModel = model_non
		; code_util__goal_may_allocate_heap(FirstDisjunct)
		}
	->
		new_saved_hp_var(SavedHeapPointerVar),
		gen_mark_hp(SavedHeapPointerVar, Context, MarkHeapPointerGoal),
		disj_add_heap_ops(Goals0, yes, yes(SavedHeapPointerVar),
			GoalInfo, Goals),
		{ Goal = conj([MarkHeapPointerGoal, disj(Goals, B) -
			GoalInfo]) }
	;
		disj_add_heap_ops(Goals0, yes, no, GoalInfo, Goals),
		{ Goal = disj(Goals, B) }
	).

goal_expr_add_heap_ops(switch(A, B, Cases0, D), GI,
		switch(A, B, Cases, D) - GI) -->
	cases_add_heap_ops(Cases0, Cases).

goal_expr_add_heap_ops(not(InnerGoal), OuterGoalInfo, Goal) -->
	%
	% We handle negations by converting them into if-then-elses:
	%	not(G)  ===>  (if G then fail else true)
	%
	{ goal_info_get_context(OuterGoalInfo, Context) },
	{ InnerGoal = _ - InnerGoalInfo },
	{ goal_info_get_determinism(InnerGoalInfo, Determinism) },
	{ determinism_components(Determinism, _CanFail, NumSolns) },
	{ true_goal(Context, True) },
	{ fail_goal(Context, Fail) },
	{ map__init(SM) },
	ModuleInfo =^ module_info,
	{ NumSolns = at_most_zero ->
		% The "then" part of the if-then-else will be unreachable,
		% but to preserve the invariants that the MLDS back-end
		% relies on, we need to make sure that it can't fail.
		% So we use a call to `private_builtin__unused' (which
		% will call error/1) rather than `fail' for the "then" part.
		generate_call("unused", [], det, no, [], ModuleInfo, Context,
			ThenGoal)
	;
		ThenGoal = Fail
	},
	{ NewOuterGoal = if_then_else([], InnerGoal, ThenGoal, True, SM) },
	goal_expr_add_heap_ops(NewOuterGoal, OuterGoalInfo, Goal).

goal_expr_add_heap_ops(some(A, B, Goal0), GoalInfo,
		some(A, B, Goal) - GoalInfo) -->
	goal_add_heap_ops(Goal0, Goal).

goal_expr_add_heap_ops(if_then_else(A, Cond0, Then0, Else0, E), GoalInfo,
		Goal - GoalInfo) -->
	goal_add_heap_ops(Cond0, Cond),
	goal_add_heap_ops(Then0, Then),
	goal_add_heap_ops(Else0, Else1),
	%
	% If the condition can allocate heap space,
	% save the heap pointer so that we can
	% restore it if the condition fails.
	%
	( { code_util__goal_may_allocate_heap(Cond0) } ->
		new_saved_hp_var(SavedHeapPointerVar),
		{ goal_info_get_context(GoalInfo, Context) },
		gen_mark_hp(SavedHeapPointerVar, Context, MarkHeapPointerGoal),
		%
		% Generate code to restore the heap pointer,
		% and insert that code at the start of the Else branch.
		%
		gen_restore_hp(SavedHeapPointerVar, Context,
			RestoreHeapPointerGoal),
		{ Else1 = _ - Else1GoalInfo },
		{ Else = conj([RestoreHeapPointerGoal, Else1]) -
			Else1GoalInfo },
		{ IfThenElse = if_then_else(A, Cond, Then, Else, E) -
			GoalInfo },
		{ Goal = conj([MarkHeapPointerGoal, IfThenElse]) }
	;
		{ Goal = if_then_else(A, Cond, Then, Else1, E) }
	).


goal_expr_add_heap_ops(call(A,B,C,D,E,F), GI, call(A,B,C,D,E,F) - GI) --> [].

goal_expr_add_heap_ops(generic_call(A,B,C,D), GI, generic_call(A,B,C,D) - GI)
	--> [].

goal_expr_add_heap_ops(unify(A,B,C,D,E), GI, unify(A,B,C,D,E) - GI) --> [].

goal_expr_add_heap_ops(PragmaForeign, GoalInfo, Goal) -->
	{ PragmaForeign = foreign_proc(_,_,_,_,_,_,Impl) },
	( { Impl = nondet(_,_,_,_,_,_,_,_,_) } ->
		% XXX Implementing heap reclamation for nondet pragma
		% foreign_code via transformation is difficult,
		% because there's nowhere in the HLDS pragma_foreign_code
		% goal where we can insert the heap reclamation operations.
		% For now, we don't support this.
		% Instead, we just generate a call to a procedure which
		% will at runtime call error/1 with an appropriate
		% "Sorry, not implemented" error message.
		ModuleInfo =^ module_info,
		{ goal_info_get_context(GoalInfo, Context) },
		{ generate_call("reclaim_heap_nondet_pragma_foreign_code",
			[], erroneous, no, [], ModuleInfo, Context,
			SorryNotImplementedCode) },
		{ Goal = SorryNotImplementedCode }
	;
		{ Goal = PragmaForeign - GoalInfo }
	).

goal_expr_add_heap_ops(shorthand(_), _, _) -->
	% these should have been expanded out by now
	{ error("goal_expr_add_heap_ops: unexpected shorthand") }.

:- pred conj_add_heap_ops(hlds_goals::in, hlds_goals::out,
		heap_ops_info::in, heap_ops_info::out) is det.
conj_add_heap_ops(Goals0, Goals) -->
	list__map_foldl(goal_add_heap_ops, Goals0, Goals).
	
:- pred disj_add_heap_ops(hlds_goals::in, bool::in, maybe(prog_var)::in,
		hlds_goal_info::in, hlds_goals::out,
		heap_ops_info::in, heap_ops_info::out) is det.

disj_add_heap_ops([], _, _, _, []) --> [].
disj_add_heap_ops([Goal0 | Goals0], IsFirstBranch, MaybeSavedHeapPointerVar,
		DisjGoalInfo, DisjGoals) -->
	goal_add_heap_ops(Goal0, Goal1),
	{ Goal1 = _ - GoalInfo },
	{ goal_info_get_context(GoalInfo, Context) },
	%
	% If needed, reset the heap pointer before executing the goal,
	% to reclaim heap space allocated in earlier branches.
	%
	(
		{ IsFirstBranch = no },
		{ MaybeSavedHeapPointerVar = yes(SavedHeapPointerVar0) }
	->
		gen_restore_hp(SavedHeapPointerVar0, Context,
			RestoreHeapPointerGoal),
		{ conj_list_to_goal([RestoreHeapPointerGoal, Goal1], GoalInfo,
			Goal) }
	;
		{ Goal = Goal1 }
	),

	%
	% Save the heap pointer, if we haven't already done so,
	% and if this disjunct might allocate heap space.
	%
	(
		{ MaybeSavedHeapPointerVar = no },
		{ code_util__goal_may_allocate_heap(Goal) }
	->
		% Generate code to save the heap pointer
		new_saved_hp_var(SavedHeapPointerVar),
		gen_mark_hp(SavedHeapPointerVar, Context, MarkHeapPointerGoal),
		% Recursively handle the remaining disjuncts
		disj_add_heap_ops(Goals0, no, yes(SavedHeapPointerVar),
			DisjGoalInfo, Goals1),
		% Put this disjunct and the remaining disjuncts in a
		% nested disjunction, so that the heap pointer variable
		% can scope over these disjuncts
		{ map__init(StoreMap) },
		{ Disj = disj([Goal | Goals1], StoreMap) - DisjGoalInfo },
		{ DisjGoals = [conj([MarkHeapPointerGoal, Disj]) -
			DisjGoalInfo] }
	;
		% Just recursively handle the remaining disjuncts
		disj_add_heap_ops(Goals0, no, MaybeSavedHeapPointerVar,
			DisjGoalInfo, Goals),
		{ DisjGoals = [Goal | Goals] }
	).

:- pred cases_add_heap_ops(list(case)::in, list(case)::out,
		heap_ops_info::in, heap_ops_info::out) is det.
cases_add_heap_ops([], []) --> [].
cases_add_heap_ops([Case0 | Cases0], [Case | Cases]) -->
	{ Case0 = case(ConsId, Goal0) },
	{ Case = case(ConsId, Goal) },
	goal_add_heap_ops(Goal0, Goal),
	cases_add_heap_ops(Cases0, Cases).

%-----------------------------------------------------------------------------%

:- pred gen_mark_hp(prog_var::in, prog_context::in, hlds_goal::out,
		heap_ops_info::in, heap_ops_info::out) is det.

gen_mark_hp(SavedHeapPointerVar, Context, MarkHeapPointerGoal) -->
	ModuleInfo =^ module_info,
	{ generate_call("mark_hp", [SavedHeapPointerVar],
		det, yes(impure),
		[SavedHeapPointerVar - ground_inst],
		ModuleInfo, Context, MarkHeapPointerGoal) }.

:- pred gen_restore_hp(prog_var::in, prog_context::in, hlds_goal::out,
		heap_ops_info::in, heap_ops_info::out) is det.

gen_restore_hp(SavedHeapPointerVar, Context, RestoreHeapPointerGoal) -->
	ModuleInfo =^ module_info,
	{ generate_call("restore_hp", [SavedHeapPointerVar],
		det, yes(impure), [],
		ModuleInfo, Context, RestoreHeapPointerGoal) }.

:- func ground_inst = (inst).
ground_inst = ground(unique, none).

%-----------------------------------------------------------------------------%

:- pred new_saved_hp_var(prog_var::out,
		heap_ops_info::in, heap_ops_info::out) is det.

new_saved_hp_var(Var) -->
	new_var("HeapPointer", heap_pointer_type, Var).
	
:- pred new_var(string::in, (type)::in, prog_var::out,
		heap_ops_info::in, heap_ops_info::out) is det.

new_var(Name, Type, Var, TOI0, TOI) :-
	VarSet0 = TOI0 ^ varset,
	VarTypes0 = TOI0 ^ var_types,
	varset__new_named_var(VarSet0, Name, Var, VarSet),
	map__det_insert(VarTypes0, Var, Type, VarTypes),
	TOI = ((TOI0 ^ varset := VarSet)
		     ^ var_types := VarTypes).

%-----------------------------------------------------------------------------%

:- func heap_pointer_type = (type).
heap_pointer_type = c_pointer_type.

%-----------------------------------------------------------------------------%

:- pred generate_call(string::in, list(prog_var)::in, determinism::in,
	maybe(goal_feature)::in, assoc_list(prog_var, inst)::in,
	module_info::in, term__context::in, hlds_goal::out) is det.

generate_call(PredName, Args, Detism, MaybeFeature, InstMap, Module, Context,
		CallGoal) :-
	mercury_private_builtin_module(BuiltinModule),
	goal_util__generate_simple_call(BuiltinModule, PredName, Args, Detism,
		MaybeFeature, InstMap, Module, Context, CallGoal).

%-----------------------------------------------------------------------------%
