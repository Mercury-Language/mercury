%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2005 The University of Melbourne.
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

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.

:- pred add_trail_ops(module_info::in, proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__type_util.
:- import_module hlds__code_model.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__instmap.
:- import_module hlds__quantification.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__modules.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

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

add_trail_ops(ModuleInfo0, !Proc) :-
	proc_info_goal(!.Proc, Goal0),
	proc_info_varset(!.Proc, VarSet0),
	proc_info_vartypes(!.Proc, VarTypes0),
	TrailOpsInfo0 = trail_ops_info(VarSet0, VarTypes0, ModuleInfo0),
	goal_add_trail_ops(Goal0, Goal, TrailOpsInfo0, TrailOpsInfo),
	TrailOpsInfo = trail_ops_info(VarSet, VarTypes, _),
	proc_info_set_goal(Goal, !Proc),
	proc_info_set_varset(VarSet, !Proc),
	proc_info_set_vartypes(VarTypes, !Proc),
	% The code below does not maintain the non-local variables,
	% so we need to requantify.
	% XXX it would be more efficient to maintain them
	%     rather than recomputing them every time.
	requantify_proc(!Proc).

:- pred goal_add_trail_ops(hlds_goal::in, hlds_goal::out,
	trail_ops_info::in, trail_ops_info::out) is det.

goal_add_trail_ops(GoalExpr0 - GoalInfo, Goal, !Info) :-
	goal_expr_add_trail_ops(GoalExpr0, GoalInfo, Goal, !Info).

:- pred goal_expr_add_trail_ops(hlds_goal_expr::in, hlds_goal_info::in,
	hlds_goal::out, trail_ops_info::in, trail_ops_info::out) is det.

goal_expr_add_trail_ops(conj(Goals0), GI, conj(Goals) - GI, !Info) :-
	conj_add_trail_ops(Goals0, Goals, !Info).

goal_expr_add_trail_ops(par_conj(Goals0), GI, par_conj(Goals) - GI, !Info) :-
	conj_add_trail_ops(Goals0, Goals, !Info).

goal_expr_add_trail_ops(disj([]), GI, disj([]) - GI, !Info).

goal_expr_add_trail_ops(disj(Goals0), GoalInfo, Goal - GoalInfo, !Info) :-
	Goals0 = [_ | _],

	goal_info_get_context(GoalInfo, Context),
	goal_info_get_code_model(GoalInfo, CodeModel),

	%
	% Allocate a new trail ticket so that we can
	% restore things on back-tracking
	%
	new_ticket_var(TicketVar, !Info),
	gen_store_ticket(TicketVar, Context, StoreTicketGoal, !.Info),
	disj_add_trail_ops(Goals0, yes, CodeModel, TicketVar, Goals, !Info),
	Goal = conj([StoreTicketGoal, disj(Goals) - GoalInfo]).

goal_expr_add_trail_ops(switch(A, B, Cases0), GI, switch(A, B, Cases) - GI,
		!Info) :-
	cases_add_trail_ops(Cases0, Cases, !Info).

goal_expr_add_trail_ops(not(InnerGoal), OuterGoalInfo, Goal, !Info) :-
	%
	% We handle negations by converting them into if-then-elses:
	%	not(G)  ===>  (if G then fail else true)
	%
	goal_info_get_context(OuterGoalInfo, Context),
	InnerGoal = _ - InnerGoalInfo,
	goal_info_get_determinism(InnerGoalInfo, Determinism),
	determinism_components(Determinism, _CanFail, NumSolns),
	true_goal(Context, True),
	fail_goal(Context, Fail),
	ModuleInfo = !.Info ^ module_info,
	( NumSolns = at_most_zero ->
		% The "then" part of the if-then-else will be unreachable,
		% but to preserve the invariants that the MLDS back-end
		% relies on, we need to make sure that it can't fail.
		% So we use a call to `private_builtin__unused' (which
		% will call error/1) rather than `fail' for the "then" part.
		mercury_private_builtin_module(PrivateBuiltin),
		generate_simple_call(PrivateBuiltin, "unused", predicate,
			only_mode, det, [], [], [], ModuleInfo,
			Context, ThenGoal)
	;
		ThenGoal = Fail
	),
	NewOuterGoal = if_then_else([], InnerGoal, ThenGoal, True),
	goal_expr_add_trail_ops(NewOuterGoal, OuterGoalInfo, Goal, !Info).

goal_expr_add_trail_ops(scope(Reason, Goal0), OuterGoalInfo,
		Goal - OuterGoalInfo, !Info) :-
	Goal0 = _ - InnerGoalInfo,
	goal_info_get_code_model(InnerGoalInfo, InnerCodeModel),
	goal_info_get_code_model(OuterGoalInfo, OuterCodeModel),
	(
		InnerCodeModel = model_non,
		OuterCodeModel \= model_non
	->
		% handle commits

		%
		% before executing the goal, we save the ticket counter,
		% and allocate a new trail ticket
		%
		goal_info_get_context(OuterGoalInfo, Context),
		new_ticket_counter_var(SavedTicketCounterVar, !Info),
		new_ticket_var(TicketVar, !Info),
		gen_mark_ticket_stack(SavedTicketCounterVar, Context,
			MarkTicketStackGoal, !.Info),
		gen_store_ticket(TicketVar, Context, StoreTicketGoal, !.Info),
		%
		% next we execute the goal that we're committing across
		%
		goal_add_trail_ops(Goal0, Goal1, !Info),
		%
		% if the goal succeeds, then we have committed to that
		% goal, so we need to commit the trail entries
		% and prune any trail tickets that have been allocated
		% since we saved the ticket counter
		%
		gen_reset_ticket_commit(TicketVar, Context,
			ResetTicketCommitGoal, !.Info),
		gen_prune_tickets_to(SavedTicketCounterVar, Context,
			PruneTicketsToGoal, !.Info),
		%
		% if the goal fails, then we should undo the trail
		% entries and discard this trail ticket before
		% backtracking over it
		%
		gen_reset_ticket_undo(TicketVar, Context,
			ResetTicketUndoGoal, !.Info),
		gen_discard_ticket(Context, DiscardTicketGoal, !.Info),
		fail_goal(Context, FailGoal),

		% put it all together
		Goal2 = scope(Reason, Goal1) - OuterGoalInfo,
		SuccCode = conj([Goal2, ResetTicketCommitGoal,
			PruneTicketsToGoal]) - OuterGoalInfo,
		( OuterCodeModel = model_semi ->
			FailGoal = _ - FailGoalInfo,
			FailCode = conj([ResetTicketUndoGoal,
				DiscardTicketGoal, FailGoal]) - FailGoalInfo,
			Goal3 = disj([SuccCode, FailCode]) - OuterGoalInfo
		;
			Goal3 = SuccCode
		),
		Goal = conj([MarkTicketStackGoal, StoreTicketGoal, Goal3])
	;
		goal_add_trail_ops(Goal0, Goal1, !Info),
		Goal = scope(Reason, Goal1)
	).

goal_expr_add_trail_ops(if_then_else(A, Cond0, Then0, Else0), GoalInfo,
		Goal - GoalInfo, !Info) :-
	goal_add_trail_ops(Cond0, Cond, !Info),
	goal_add_trail_ops(Then0, Then1, !Info),
	goal_add_trail_ops(Else0, Else1, !Info),
	%
	% Allocate a new trail ticket so that we can
	% restore things if the condition fails.
	%
	new_ticket_var(TicketVar, !Info),
	goal_info_get_context(GoalInfo, Context),
	gen_store_ticket(TicketVar, Context, StoreTicketGoal, !.Info),

	%
	% Commit the trail ticket entries if the condition
	% succeeds.
	%
	Then1 = _ - Then1GoalInfo,
	Cond = _ - CondGoalInfo,
	goal_info_get_code_model(CondGoalInfo, CondCodeModel),
	( CondCodeModel = model_non ->
		gen_reset_ticket_solve(TicketVar, Context,
			ResetTicketSolveGoal, !.Info),
		Then = conj([ResetTicketSolveGoal, Then1]) - Then1GoalInfo
	;
		gen_reset_ticket_commit(TicketVar, Context,
			ResetTicketCommitGoal, !.Info),
		gen_prune_ticket(Context, PruneTicketGoal, !.Info),
		Then = conj([ResetTicketCommitGoal, PruneTicketGoal, Then1])
			- Then1GoalInfo
	),
	gen_reset_ticket_undo(TicketVar, Context, ResetTicketUndoGoal, !.Info),
	gen_discard_ticket(Context, DiscardTicketGoal, !.Info),
	Else1 = _ - Else1GoalInfo,
	Else = conj([ResetTicketUndoGoal, DiscardTicketGoal, Else1])
		- Else1GoalInfo,
	IfThenElse = if_then_else(A, Cond, Then, Else) - GoalInfo,
	Goal = conj([StoreTicketGoal, IfThenElse]).

goal_expr_add_trail_ops(Goal @ call(_, _, _, _, _, _), GI, Goal - GI, !Info).

goal_expr_add_trail_ops(Goal @ generic_call(_, _, _, _), GI, Goal - GI, !Info).

goal_expr_add_trail_ops(Goal @ unify(_, _, _, _, _), GI, Goal - GI, !Info).

goal_expr_add_trail_ops(PragmaForeign, GoalInfo, Goal, !Info) :-
	PragmaForeign = foreign_proc(_, _, _, _, _, Impl),
	( Impl = nondet(_,_,_,_,_,_,_,_,_) ->
		% XXX Implementing trailing for nondet pragma foreign_code
		% via transformation is difficult, because there's nowhere
		% in the HLDS pragma_foreign_code goal where we can insert
		% trailing operations.  For now, we don't support this.
		% Instead, we just generate a call to a procedure which
		% will at runtime call error/1 with an appropriate
		% "Sorry, not implemented" error message.
		ModuleInfo = !.Info^ module_info,
		goal_info_get_context(GoalInfo, Context),
		generate_call("trailed_nondet_pragma_foreign_code",
			erroneous, [], [], [], ModuleInfo, Context,
			SorryNotImplementedCode),
		Goal = SorryNotImplementedCode
	;
		Goal = PragmaForeign - GoalInfo
	).

goal_expr_add_trail_ops(shorthand(_), _, _, !Info) :-
	% these should have been expanded out by now
	error("goal_expr_add_trail_ops: unexpected shorthand").

:- pred conj_add_trail_ops(hlds_goals::in, hlds_goals::out,
	trail_ops_info::in, trail_ops_info::out) is det.

conj_add_trail_ops(Goals0, Goals, !Info) :-
	list__map_foldl(goal_add_trail_ops, Goals0, Goals, !Info).

:- pred disj_add_trail_ops(hlds_goals::in, bool::in, code_model::in,
	prog_var::in, hlds_goals::out,
	trail_ops_info::in, trail_ops_info::out) is det.

disj_add_trail_ops([], _, _, _, [], !Info).
disj_add_trail_ops([Goal0 | Goals0], IsFirstBranch, CodeModel, TicketVar,
		[Goal | Goals], !Info) :-
	Goal0 = _ - GoalInfo0,
	goal_info_get_context(GoalInfo0, Context),
	%
	% First undo the effects of any earlier branches
	%
	( IsFirstBranch = yes ->
		UndoList = []
	;
		gen_reset_ticket_undo(TicketVar, Context, ResetTicketUndoGoal,
			!.Info),
		( Goals0 = [] ->
			%
			% Once we've reached the last disjunct,
			% we can discard the trail ticket
			%
			gen_discard_ticket(Context, DiscardTicketGoal, !.Info),
			UndoList = [ResetTicketUndoGoal, DiscardTicketGoal]
		;
			UndoList = [ResetTicketUndoGoal]
		)
	),
	%
	% Then execute the disjunct goal
	%
	goal_add_trail_ops(Goal0, Goal1, !Info),
	%
	% For model_semi and model_det disjunctions,
	% once we reach the end of the disjunct goal,
	% we're committing to this disjunct, so we need
	% to prune the trail ticket.
	%
	( CodeModel = model_non ->
		PruneList = []
	;
		gen_reset_ticket_commit(TicketVar, Context,
			ResetTicketCommitGoal, !.Info),
		gen_prune_ticket(Context, PruneTicketGoal, !.Info),
		PruneList = [ResetTicketCommitGoal, PruneTicketGoal]
	),
	%
	% Package up the stuff we built earlier.
	%
	Goal1 = _ - GoalInfo1,
	conj_list_to_goal(UndoList ++ [Goal1] ++ PruneList, GoalInfo1,
		Goal),

	% Recursively handle the remaining disjuncts
	disj_add_trail_ops(Goals0, no, CodeModel, TicketVar, Goals, !Info).

:- pred cases_add_trail_ops(list(case)::in, list(case)::out,
	trail_ops_info::in, trail_ops_info::out) is det.

cases_add_trail_ops([], [], !Info).
cases_add_trail_ops([Case0 | Cases0], [Case | Cases], !Info) :-
	Case0 = case(ConsId, Goal0),
	Case = case(ConsId, Goal),
	goal_add_trail_ops(Goal0, Goal, !Info),
	cases_add_trail_ops(Cases0, Cases, !Info).

%-----------------------------------------------------------------------------%

:- pred gen_store_ticket(prog_var::in, prog_context::in, hlds_goal::out,
	trail_ops_info::in) is det.

gen_store_ticket(TicketVar, Context, SaveTicketGoal, Info) :-
	generate_call("store_ticket", det, [TicketVar], [impure],
		[TicketVar - ground_inst],
		Info ^ module_info, Context, SaveTicketGoal).

:- pred gen_reset_ticket_undo(prog_var::in, prog_context::in, hlds_goal::out,
	trail_ops_info::in) is det.

gen_reset_ticket_undo(TicketVar, Context, ResetTicketGoal, Info) :-
	generate_call("reset_ticket_undo", det, [TicketVar], [impure],
		[], Info ^ module_info, Context, ResetTicketGoal).

:- pred gen_reset_ticket_solve(prog_var::in, prog_context::in, hlds_goal::out,
	trail_ops_info::in) is det.

gen_reset_ticket_solve(TicketVar, Context, ResetTicketGoal, Info) :-
	generate_call("reset_ticket_solve", det, [TicketVar], [impure],
		[], Info ^ module_info, Context, ResetTicketGoal).

:- pred gen_reset_ticket_commit(prog_var::in, prog_context::in, hlds_goal::out,
	trail_ops_info::in) is det.

gen_reset_ticket_commit(TicketVar, Context, ResetTicketGoal, Info) :-
	generate_call("reset_ticket_commit", det, [TicketVar], [impure],
		[], Info ^ module_info, Context, ResetTicketGoal).

:- pred gen_prune_ticket(prog_context::in, hlds_goal::out,
	trail_ops_info::in) is det.

gen_prune_ticket(Context, PruneTicketGoal, Info) :-
	generate_call("prune_ticket", det, [], [impure],
		[], Info ^ module_info, Context, PruneTicketGoal).

:- pred gen_discard_ticket(prog_context::in, hlds_goal::out,
	trail_ops_info::in) is det.

gen_discard_ticket(Context, DiscardTicketGoal, Info) :-
	generate_call("discard_ticket", det, [], [impure], [],
		Info ^ module_info, Context, DiscardTicketGoal).

:- pred gen_mark_ticket_stack(prog_var::in, prog_context::in, hlds_goal::out,
	trail_ops_info::in) is det.

gen_mark_ticket_stack(SavedTicketCounterVar, Context, MarkTicketStackGoal,
		Info) :-
	generate_call("mark_ticket_stack", det, [SavedTicketCounterVar],
		[impure], [], Info ^ module_info, Context,
		MarkTicketStackGoal).

:- pred gen_prune_tickets_to(prog_var::in, prog_context::in, hlds_goal::out,
	trail_ops_info::in) is det.

gen_prune_tickets_to(SavedTicketCounterVar, Context, PruneTicketsToGoal,
		Info) :-
	generate_call("prune_tickets_to", det, [SavedTicketCounterVar],
		[impure], [], Info ^ module_info, Context,
		PruneTicketsToGoal).

:- func ground_inst = (inst).

ground_inst = ground(unique, none).

%-----------------------------------------------------------------------------%

:- pred new_ticket_var(prog_var::out,
	trail_ops_info::in, trail_ops_info::out) is det.

new_ticket_var(Var, !Info) :-
	new_var("TrailTicket", ticket_type, Var, !Info).

:- pred new_ticket_counter_var(prog_var::out,
	trail_ops_info::in, trail_ops_info::out) is det.

new_ticket_counter_var(Var, !Info) :-
	new_var("SavedTicketCounter", ticket_counter_type, Var, !Info).

:- pred new_var(string::in, (type)::in, prog_var::out,
	trail_ops_info::in, trail_ops_info::out) is det.

new_var(Name, Type, Var, !Info) :-
	VarSet0 = !.Info ^ varset,
	VarTypes0 = !.Info ^ var_types,
	varset__new_named_var(VarSet0, Name, Var, VarSet),
	map__det_insert(VarTypes0, Var, Type, VarTypes),
	!:Info = ((!.Info ^ varset := VarSet) ^ var_types := VarTypes).

%-----------------------------------------------------------------------------%

:- func ticket_type = (type).
ticket_type = c_pointer_type.

:- func ticket_counter_type = (type).
ticket_counter_type = c_pointer_type.

%-----------------------------------------------------------------------------%

:- pred generate_call(string::in, determinism::in, list(prog_var)::in,
	list(goal_feature)::in, assoc_list(prog_var, inst)::in,
	module_info::in, term__context::in, hlds_goal::out) is det.

generate_call(PredName, Detism, Args, Features, InstMap, ModuleInfo,
		Context, CallGoal) :-
	mercury_private_builtin_module(BuiltinModule),
	goal_util__generate_simple_call(BuiltinModule, PredName, predicate,
		only_mode, Detism, Args, Features, InstMap, ModuleInfo,
		Context, CallGoal).

%-----------------------------------------------------------------------------%
