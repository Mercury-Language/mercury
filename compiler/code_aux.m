%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
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

:- import_module parse_tree__prog_data.
:- import_module hlds__hlds_goal.
:- import_module ll_backend__code_info.
:- import_module bool.

	% code_aux__contains_simple_recursive_call(G, CI, Last) succeeds
	% if G is a conjunction of goals, exactly one of which is a recursive
	% call (CI says what the current procedure is), and there are no
	% other goals that cause control to leave this procedure. Last is
	% set dependening on whether the recursive call is last in the
	% conjunction or not.

	% XXX should avoid the dependency on code_info here
:- pred code_aux__contains_simple_recursive_call(hlds_goal, code_info, bool).
:- mode code_aux__contains_simple_recursive_call(in, in, out) is semidet.

:- pred code_aux__explain_stack_slots(stack_slots, prog_varset, string).
:- mode code_aux__explain_stack_slots(in, in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__goal_form.
:- import_module ll_backend__llds, ll_backend__llds_out.
:- import_module string, list, assoc_list, map, std_util, varset.

%-----------------------------------------------------------------------------%

code_aux__contains_simple_recursive_call(Goal - _, CodeInfo, Last) :-
	Goal = conj(Goals),
	code_aux__contains_simple_recursive_call_expr(Goals, CodeInfo, Last).

:- pred code_aux__contains_simple_recursive_call_expr(list(hlds_goal)::in,
	code_info::in, bool::out) is semidet.

code_aux__contains_simple_recursive_call_expr([Goal|Goals], CodeInfo, Last) :-
	Goal = GoalExpr - _,
	(
		contains_only_builtins_expr(GoalExpr)
	->
		code_aux__contains_simple_recursive_call_expr(Goals, CodeInfo,
			Last)
	;
		code_aux__is_recursive_call(GoalExpr, CodeInfo),
		( Goals = [] ->
			Last = yes
		;
			contains_only_builtins_list(Goals),
			Last = no
		)
	).

:- pred code_aux__is_recursive_call(hlds_goal_expr, code_info).
:- mode code_aux__is_recursive_call(in, in) is semidet.

code_aux__is_recursive_call(Goal, CodeInfo) :-
	Goal = call(CallPredId, CallProcId, _, BuiltinState, _, _),
	BuiltinState = not_builtin,
	code_info__get_pred_id(PredId, CodeInfo, _),
	PredId = CallPredId,
	code_info__get_proc_id(ProcId, CodeInfo, _),
	ProcId = CallProcId.

%-----------------------------------------------------------------------------%

code_aux__explain_stack_slots(StackSlots, VarSet, Explanation) :-
	map__to_assoc_list(StackSlots, StackSlotsList),
	code_aux__explain_stack_slots_2(StackSlotsList, VarSet, "",
		Explanation1),
	string__append("\nStack slot assignments (if any):\n", Explanation1,
		Explanation).

:- pred code_aux__explain_stack_slots_2(assoc_list(prog_var, lval), prog_varset,
		string, string).
:- mode code_aux__explain_stack_slots_2(in, in, in, out) is det.

code_aux__explain_stack_slots_2([], _, String, String).
code_aux__explain_stack_slots_2([Var - Lval | Rest], VarSet, String0, String) :-
	code_aux__explain_stack_slots_2(Rest, VarSet, String0, String1),
	( llds_out__lval_to_string(Lval, LvalString0) ->
		LvalString = LvalString0
	;
		LvalString = "some lval"
	),
	varset__lookup_name(VarSet, Var, VarName),
	string__append_list([VarName, "\t ->\t", LvalString, "\n", String1],
		String).

%---------------------------------------------------------------------------%
