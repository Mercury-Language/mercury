%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: fjh.
%
% This module is an HLDS-to-HLDS transformation that inserts code to
% handle trailing.  The transformation involves adding calls to impure
% predicates defined in library/private_builtin.m, which in turn call
% macros defined in runtime/mercury_trail.h.
%
% This pass is currently only used for the MLDS back-end.
% For some reason (perhaps efficiency?? or more likely just historical?),
% the LLDS back-end inserts the trail operations as it is generating
% LLDS code, rather than via an HLDS to HLDS transformation.
%
% See compiler/notes/trailing.html for more information about trailing
% in the Mercury implementation.
%
% This pass is very similar to add_heap_ops.m.
%
%-----------------------------------------------------------------------------%

% XXX check goal_infos for correctness

%-----------------------------------------------------------------------------%

:- module ml_backend__add_trail_ops.
:- interface.
:- import_module hlds__hlds_pred, hlds__hlds_module.

:- pred add_trail_ops(proc_info::in, module_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_data, parse_tree__prog_util.
:- import_module (parse_tree__inst).
:- import_module hlds__hlds_goal, hlds__hlds_data.
:- import_module hlds__goal_util, hlds__quantification, parse_tree__modules.
:- import_module check_hlds__type_util.
:- import_module backend_libs__code_model, hlds__instmap.

:- import_module bool, string.
:- import_module assoc_list, list, map, set, varset, std_util, require, term.


%
% As we traverse the goal, we add new variables to hold the
% trail tickets (i.e. saved values of the trail pointer)
% and the saved values of the trail ticket counter.
% So we need to thread a varset and a vartypes mapping through,
% to record the names and types of the new variables.
%
% We also keep the module_info around, so that we can use
% the predicate table that it contains to lookup the pred_ids
% for the builtin procedures that we insert calls to.
% We do not update the module_info as we're traversing the goal.
%

:- type trail_ops_info --->
	trail_ops_info(
		varset :: prog_varset,
		var_types :: vartypes,
		module_info :: module_info
	).

add_trail_ops(Proc0, ModuleInfo0, Proc) :-
	proc_info_goal(Proc0, Goal0),
	proc_info_varset(Proc0, VarSet0),
	proc_info_vartypes(Proc0, VarTypes0),
	TrailOpsInfo0 = trail_ops_info(VarSet0, VarTypes0, ModuleInfo0),
	goal_add_trail_ops(Goal0, Goal, TrailOpsInfo0, TrailOpsInfo),
	TrailOpsInfo = trail_ops_info(VarSet, VarTypes, _),
	proc_info_set_goal(Proc0, Goal, Proc1),
	proc_info_set_varset(Proc1, VarSet, Proc2),
	proc_info_set_vartypes(Proc2, VarTypes, Proc3),
	% The code below does not maintain the non-local variables,
	% so we need to requantify.
	% XXX it would be more efficient to maintain them
	%     rather than recomputing them every time.
	requantify_proc(Proc3, Proc).

:- pred goal_add_trail_ops(hlds_goal::in, hlds_goal::out,
		trail_ops_info::in, trail_ops_info::out) is det.

goal_add_trail_ops(GoalExpr0 - GoalInfo, Goal) -->
	goal_expr_add_trail_ops(GoalExpr0, GoalInfo, Goal).

:- pred goal_expr_add_trail_ops(hlds_goal_expr::in, hlds_goal_info::in,
		hlds_goal::out,
		trail_ops_info::in, trail_ops_info::out) is det.

goal_expr_add_trail_ops(conj(Goals0), GI, conj(Goals) - GI) -->
	conj_add_trail_ops(Goals0, Goals).

goal_expr_add_trail_ops(par_conj(Goals0), GI, par_conj(Goals) - GI) -->
	conj_add_trail_ops(Goals0, Goals).

goal_expr_add_trail_ops(disj([]), GI, disj([]) - GI) --> [].

goal_expr_add_trail_ops(disj(Goals0), GoalInfo, Goal - GoalInfo) -->
	{ Goals0 = [_|_] },

	{ goal_info_get_context(GoalInfo, Context) },
	{ goal_info_get_code_model(GoalInfo, CodeModel) },

	%
	% Allocate a new trail ticket so that we can
	% restore things on back-tracking
	%
	new_ticket_var(TicketVar),
	gen_store_ticket(TicketVar, Context, StoreTicketGoal),
	disj_add_trail_ops(Goals0, yes, CodeModel, TicketVar, Goals),
	{ Goal = conj([StoreTicketGoal, disj(Goals) - GoalInfo]) }.

goal_expr_add_trail_ops(switch(A, B, Cases0), GI, switch(A, B, Cases) - GI) -->
	cases_add_trail_ops(Cases0, Cases).

goal_expr_add_trail_ops(not(InnerGoal), OuterGoalInfo, Goal) -->
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
	ModuleInfo =^ module_info,
	{ NumSolns = at_most_zero ->
		% The "then" part of the if-then-else will be unreachable,
		% but to preserve the invariants that the MLDS back-end
		% relies on, we need to make sure that it can't fail.
		% So we use a call to `private_builtin__unused' (which
		% will call error/1) rather than `fail' for the "then" part.
		mercury_private_builtin_module(PrivateBuiltin),
		generate_simple_call(PrivateBuiltin, "unused",
			[], det, no, [], ModuleInfo, Context, ThenGoal)
	;
		ThenGoal = Fail
	},
	{ NewOuterGoal = if_then_else([], InnerGoal, ThenGoal, True) },
	goal_expr_add_trail_ops(NewOuterGoal, OuterGoalInfo, Goal).

goal_expr_add_trail_ops(some(A, B, Goal0), OuterGoalInfo,
		Goal - OuterGoalInfo) -->
	{ Goal0 = _ - InnerGoalInfo },
	{ goal_info_get_code_model(InnerGoalInfo, InnerCodeModel) },
	{ goal_info_get_code_model(OuterGoalInfo, OuterCodeModel) },
	(
		{ InnerCodeModel = model_non },
		{ OuterCodeModel \= model_non }
	->
		% handle commits

		%
		% before executing the goal, we save the ticket counter,
		% and allocate a new trail ticket
		%
		{ goal_info_get_context(OuterGoalInfo, Context) },
		new_ticket_counter_var(SavedTicketCounterVar),
		new_ticket_var(TicketVar),
		gen_mark_ticket_stack(SavedTicketCounterVar, Context,
			MarkTicketStackGoal),
		gen_store_ticket(TicketVar, Context, StoreTicketGoal),
		%
		% next we execute the goal that we're committing across
		%
		goal_add_trail_ops(Goal0, Goal1),
		%
		% if the goal succeeds, then we have committed to that
		% goal, so we need to commit the trail entries
		% and prune any trail tickets that have been allocated
		% since we saved the ticket counter
		%
		gen_reset_ticket_commit(TicketVar, Context,
			ResetTicketCommitGoal),
		gen_prune_tickets_to(SavedTicketCounterVar, Context,
			PruneTicketsToGoal),
		%
		% if the goal fails, then we should undo the trail
		% entries and discard this trail ticket before
		% backtracking over it
		%
		gen_reset_ticket_undo(TicketVar, Context,
			ResetTicketUndoGoal),
		gen_discard_ticket(Context, DiscardTicketGoal),
		{ fail_goal(Context, FailGoal) },

		% put it all together
		{ Goal2 = some(A, B, Goal1) - OuterGoalInfo },
		{ SuccCode = conj([Goal2, ResetTicketCommitGoal,
				PruneTicketsToGoal]) - OuterGoalInfo },
		( { OuterCodeModel = model_semi } ->
			{ FailGoal = _ - FailGoalInfo },
			{ FailCode = conj([ResetTicketUndoGoal,
				DiscardTicketGoal, FailGoal]) - FailGoalInfo },
			{ Goal3 = disj([SuccCode, FailCode]) - OuterGoalInfo }
		;
			{ Goal3 = SuccCode }
		),
		{ Goal = conj([MarkTicketStackGoal, StoreTicketGoal, Goal3]) }
	;
		goal_add_trail_ops(Goal0, Goal1),
		{ Goal = some(A, B, Goal1) }
	).

goal_expr_add_trail_ops(if_then_else(A, Cond0, Then0, Else0), GoalInfo,
		Goal - GoalInfo) -->
	goal_add_trail_ops(Cond0, Cond),
	goal_add_trail_ops(Then0, Then1),
	goal_add_trail_ops(Else0, Else1),
	%
	% Allocate a new trail ticket so that we can
	% restore things if the condition fails.
	%
	new_ticket_var(TicketVar),
	{ goal_info_get_context(GoalInfo, Context) },
	gen_store_ticket(TicketVar, Context, StoreTicketGoal),

	%
	% Commit the trail ticket entries if the condition
	% succeeds.
	%
	{ Then1 = _ - Then1GoalInfo },
	{ Cond = _ - CondGoalInfo },
	{ goal_info_get_code_model(CondGoalInfo, CondCodeModel) },
	( { CondCodeModel = model_non } ->
		gen_reset_ticket_solve(TicketVar, Context,
			ResetTicketSolveGoal),
		{ Then = conj([ResetTicketSolveGoal, Then1]) - Then1GoalInfo }
	;
		gen_reset_ticket_commit(TicketVar, Context,
			ResetTicketCommitGoal),
		gen_prune_ticket(Context, PruneTicketGoal),
		{ Then = conj([ResetTicketCommitGoal, PruneTicketGoal, Then1])
			- Then1GoalInfo }
	),
	gen_reset_ticket_undo(TicketVar, Context, ResetTicketUndoGoal),
	gen_discard_ticket(Context, DiscardTicketGoal),
	{ Else1 = _ - Else1GoalInfo },
	{ Else = conj([ResetTicketUndoGoal, DiscardTicketGoal, Else1])
		- Else1GoalInfo },
	{ IfThenElse = if_then_else(A, Cond, Then, Else) - GoalInfo },
	{ Goal = conj([StoreTicketGoal, IfThenElse]) }.


goal_expr_add_trail_ops(call(A,B,C,D,E,F), GI, call(A,B,C,D,E,F) - GI) --> [].

goal_expr_add_trail_ops(generic_call(A,B,C,D), GI, generic_call(A,B,C,D) - GI)
	--> [].

goal_expr_add_trail_ops(unify(A,B,C,D,E), GI, unify(A,B,C,D,E) - GI) --> [].

goal_expr_add_trail_ops(PragmaForeign, GoalInfo, Goal) -->
	{ PragmaForeign = foreign_proc(_,_,_,_,_,_,Impl) },
	( { Impl = nondet(_,_,_,_,_,_,_,_,_) } ->
		% XXX Implementing trailing for nondet pragma foreign_code
		% via transformation is difficult, because there's nowhere
		% in the HLDS pragma_foreign_code goal where we can insert
		% trailing operations.  For now, we don't support this.
		% Instead, we just generate a call to a procedure which
		% will at runtime call error/1 with an appropriate
		% "Sorry, not implemented" error message.
		ModuleInfo =^ module_info,
		{ goal_info_get_context(GoalInfo, Context) },
		{ generate_call("trailed_nondet_pragma_foreign_code",
			[], erroneous, no, [], ModuleInfo, Context,
			SorryNotImplementedCode) },
		{ Goal = SorryNotImplementedCode }
	;
		{ Goal = PragmaForeign - GoalInfo }
	).

goal_expr_add_trail_ops(shorthand(_), _, _) -->
	% these should have been expanded out by now
	{ error("goal_expr_add_trail_ops: unexpected shorthand") }.

:- pred conj_add_trail_ops(hlds_goals::in, hlds_goals::out,
		trail_ops_info::in, trail_ops_info::out) is det.
conj_add_trail_ops(Goals0, Goals) -->
	list__map_foldl(goal_add_trail_ops, Goals0, Goals).
	
:- pred disj_add_trail_ops(hlds_goals::in, bool::in, code_model::in,
		prog_var::in, hlds_goals::out,
		trail_ops_info::in, trail_ops_info::out) is det.

disj_add_trail_ops([], _, _, _, []) --> [].
disj_add_trail_ops([Goal0 | Goals0], IsFirstBranch, CodeModel, TicketVar,
		[Goal | Goals]) -->
	{ Goal0 = _ - GoalInfo0 },
	{ goal_info_get_context(GoalInfo0, Context) },
	%
	% First undo the effects of any earlier branches
	%
	( { IsFirstBranch = yes } ->
		{ UndoList = [] }
	;
		gen_reset_ticket_undo(TicketVar, Context, ResetTicketUndoGoal),
		( { Goals0 = [] } ->
			%
			% Once we've reached the last disjunct,
			% we can discard the trail ticket
			%
			gen_discard_ticket(Context, DiscardTicketGoal),
			{ UndoList = [ResetTicketUndoGoal, DiscardTicketGoal] }
		;
			{ UndoList = [ResetTicketUndoGoal] }
		)
	),
	%
	% Then execute the disjunct goal
	%
	goal_add_trail_ops(Goal0, Goal1),
	%
	% For model_semi and model_det disjunctions,
	% once we reach the end of the disjunct goal,
	% we're committing to this disjunct, so we need
	% to prune the trail ticket.
	%
	( { CodeModel = model_non } ->
		{ PruneList = [] }
	;
		gen_reset_ticket_commit(TicketVar, Context,
			ResetTicketCommitGoal),
		gen_prune_ticket(Context, PruneTicketGoal),
		{ PruneList = [ResetTicketCommitGoal, PruneTicketGoal] }
	),
	%
	% Package up the stuff we built earlier.
	%
	{ Goal1 = _ - GoalInfo1 },
	{ conj_list_to_goal(UndoList ++ [Goal1] ++ PruneList, GoalInfo1,
		Goal) },

	% Recursively handle the remaining disjuncts
	disj_add_trail_ops(Goals0, no, CodeModel, TicketVar, Goals).

:- pred cases_add_trail_ops(list(case)::in, list(case)::out,
		trail_ops_info::in, trail_ops_info::out) is det.
cases_add_trail_ops([], []) --> [].
cases_add_trail_ops([Case0 | Cases0], [Case | Cases]) -->
	{ Case0 = case(ConsId, Goal0) },
	{ Case = case(ConsId, Goal) },
	goal_add_trail_ops(Goal0, Goal),
	cases_add_trail_ops(Cases0, Cases).

%-----------------------------------------------------------------------------%

:- pred gen_store_ticket(prog_var::in, prog_context::in, hlds_goal::out,
		trail_ops_info::in, trail_ops_info::out) is det.

gen_store_ticket(TicketVar, Context, SaveTicketGoal) -->
	ModuleInfo =^ module_info,
	{ generate_call("store_ticket", [TicketVar],
		det, yes(impure),
		[TicketVar - ground_inst],
		ModuleInfo, Context, SaveTicketGoal) }.

:- pred gen_reset_ticket_undo(prog_var::in, prog_context::in, hlds_goal::out,
		trail_ops_info::in, trail_ops_info::out) is det.

gen_reset_ticket_undo(TicketVar, Context, ResetTicketGoal) -->
	ModuleInfo =^ module_info,
	{ generate_call("reset_ticket_undo", [TicketVar],
		det, yes(impure), [],
		ModuleInfo, Context, ResetTicketGoal) }.

:- pred gen_reset_ticket_solve(prog_var::in, prog_context::in, hlds_goal::out,
		trail_ops_info::in, trail_ops_info::out) is det.

gen_reset_ticket_solve(TicketVar, Context, ResetTicketGoal) -->
	ModuleInfo =^ module_info,
	{ generate_call("reset_ticket_solve", [TicketVar],
		det, yes(impure), [],
		ModuleInfo, Context, ResetTicketGoal) }.

:- pred gen_reset_ticket_commit(prog_var::in, prog_context::in, hlds_goal::out,
		trail_ops_info::in, trail_ops_info::out) is det.

gen_reset_ticket_commit(TicketVar, Context, ResetTicketGoal) -->
	ModuleInfo =^ module_info,
	{ generate_call("reset_ticket_commit", [TicketVar],
		det, yes(impure), [],
		ModuleInfo, Context, ResetTicketGoal) }.

:- pred gen_prune_ticket(prog_context::in, hlds_goal::out,
		trail_ops_info::in, trail_ops_info::out) is det.

gen_prune_ticket(Context, PruneTicketGoal) -->
	ModuleInfo =^ module_info,
	{ generate_call("prune_ticket", [], det, yes(impure), [],
		ModuleInfo, Context, PruneTicketGoal) }.

:- pred gen_discard_ticket(prog_context::in, hlds_goal::out,
		trail_ops_info::in, trail_ops_info::out) is det.

gen_discard_ticket(Context, DiscardTicketGoal) -->
	ModuleInfo =^ module_info,
	{ generate_call("discard_ticket", [], det, yes(impure), [],
		ModuleInfo, Context, DiscardTicketGoal) }.

:- pred gen_mark_ticket_stack(prog_var::in, prog_context::in, hlds_goal::out,
		trail_ops_info::in, trail_ops_info::out) is det.

gen_mark_ticket_stack(SavedTicketCounterVar, Context, MarkTicketStackGoal) -->
	ModuleInfo =^ module_info,
	{ generate_call("mark_ticket_stack", [SavedTicketCounterVar],
		det, yes(impure), [],
		ModuleInfo, Context, MarkTicketStackGoal) }.

:- pred gen_prune_tickets_to(prog_var::in, prog_context::in, hlds_goal::out,
		trail_ops_info::in, trail_ops_info::out) is det.

gen_prune_tickets_to(SavedTicketCounterVar, Context, PruneTicketsToGoal) -->
	ModuleInfo =^ module_info,
	{ generate_call("prune_tickets_to", [SavedTicketCounterVar],
		det, yes(impure), [],
		ModuleInfo, Context, PruneTicketsToGoal) }.

:- func ground_inst = (inst).
ground_inst = ground(unique, none).

%-----------------------------------------------------------------------------%

:- pred new_ticket_var(prog_var::out,
		trail_ops_info::in, trail_ops_info::out) is det.

new_ticket_var(Var) -->
	new_var("TrailTicket", ticket_type, Var).
	
:- pred new_ticket_counter_var(prog_var::out,
		trail_ops_info::in, trail_ops_info::out) is det.

new_ticket_counter_var(Var) -->
	new_var("SavedTicketCounter", ticket_counter_type, Var).
	
:- pred new_var(string::in, (type)::in, prog_var::out,
		trail_ops_info::in, trail_ops_info::out) is det.

new_var(Name, Type, Var, TOI0, TOI) :-
	VarSet0 = TOI0 ^ varset,
	VarTypes0 = TOI0 ^ var_types,
	varset__new_named_var(VarSet0, Name, Var, VarSet),
	map__det_insert(VarTypes0, Var, Type, VarTypes),
	TOI = ((TOI0 ^ varset := VarSet)
		     ^ var_types := VarTypes).

%-----------------------------------------------------------------------------%

:- func ticket_type = (type).
ticket_type = c_pointer_type.

:- func ticket_counter_type = (type).
ticket_counter_type = c_pointer_type.

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
