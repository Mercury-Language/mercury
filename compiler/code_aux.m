%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Auxiliary code generator module. Unlike code_util, it imports code_info.
%
% Main authors: conway, zs.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module ll_backend__code_aux.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_llds.
:- import_module ll_backend__code_info.
:- import_module parse_tree__prog_data.

:- import_module bool.

	% code_aux__contains_simple_recursive_call(G, CI, Last) succeeds
	% if G is a conjunction of goals, exactly one of which is a recursive
	% call (CI says what the current procedure is), and there are no
	% other goals that cause control to leave this procedure. Last is
	% set dependening on whether the recursive call is last in the
	% conjunction or not.

	% XXX should avoid the dependency on code_info here
:- pred code_aux__contains_simple_recursive_call(hlds_goal::in, code_info::in,
	bool::out) is semidet.

:- pred code_aux__explain_stack_slots(stack_slots::in, prog_varset::in,
	string::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__goal_form.
:- import_module ll_backend__llds.
:- import_module ll_backend__llds_out.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module std_util.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

code_aux__contains_simple_recursive_call(Goal - _, CodeInfo, Last) :-
	Goal = conj(Goals),
	code_aux__contains_simple_recursive_call_expr(Goals, CodeInfo, Last).

:- pred code_aux__contains_simple_recursive_call_expr(list(hlds_goal)::in,
	code_info::in, bool::out) is semidet.

code_aux__contains_simple_recursive_call_expr([Goal|Goals], CodeInfo, Last) :-
	Goal = GoalExpr - _,
	( contains_only_builtins_expr(GoalExpr) ->
		code_aux__contains_simple_recursive_call_expr(Goals, CodeInfo,
			Last)
	;
		code_aux__is_recursive_call(GoalExpr, CodeInfo),
		(
			Goals = [],
			Last = yes
		;
			Goals = [_ | _],
			contains_only_builtins_list(Goals),
			Last = no
		)
	).

:- pred code_aux__is_recursive_call(hlds_goal_expr::in, code_info::in)
	is semidet.

code_aux__is_recursive_call(Goal, CodeInfo) :-
	Goal = call(CallPredId, CallProcId, _, BuiltinState, _, _),
	BuiltinState = not_builtin,
	code_info__get_pred_id(CodeInfo, PredId),
	PredId = CallPredId,
	code_info__get_proc_id(CodeInfo, ProcId),
	ProcId = CallProcId.

%-----------------------------------------------------------------------------%

code_aux__explain_stack_slots(StackSlots, VarSet, Explanation) :-
	map__to_assoc_list(StackSlots, StackSlotsList),
	code_aux__explain_stack_slots_2(StackSlotsList, VarSet, "",
		Explanation1),
	string__append("\nStack slot assignments (if any):\n", Explanation1,
		Explanation).

:- pred code_aux__explain_stack_slots_2(assoc_list(prog_var, stack_slot)::in,
	prog_varset::in, string::in, string::out) is det.

code_aux__explain_stack_slots_2([], _, !Explanation).
code_aux__explain_stack_slots_2([Var - Slot | Rest], VarSet, !Explanation) :-
	code_aux__explain_stack_slots_2(Rest, VarSet, !Explanation),
	(
		Slot = det_slot(SlotNum),
		StackStr = "stackvar"
	;
		Slot = nondet_slot(SlotNum),
		StackStr = "framevar"
	),
	int_to_string(SlotNum, SlotStr),
	varset__lookup_name(VarSet, Var, VarName),
	string__append_list([VarName, "\t ->\t", StackStr, SlotStr, "\n",
		!.Explanation], !:Explanation).

%---------------------------------------------------------------------------%
