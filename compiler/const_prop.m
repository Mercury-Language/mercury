%---------------------------------------------------------------------------%
% Copyright (C) 1997-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: const_prop.m
% main author: conway.
%
% This module provides the facility to evaluate calls at compile time -
% transforming them to simpler goals such as construction unifications.
%
% XXX We should check for overflow.
% XXX Some of this code should be shared with vn_util__simplify_vnrval.
% 
%------------------------------------------------------------------------------%

:- module transform_hlds__const_prop.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_goal, hlds__hlds_pred.
:- import_module parse_tree__prog_data, hlds__instmap.
:- import_module list.

:- pred evaluate_builtin(pred_id, proc_id, list(prog_var), hlds_goal_info,
		hlds_goal_expr, hlds_goal_info, instmap,
		module_info, module_info).
:- mode evaluate_builtin(in, in, in, in, out, out, in, in, out) is semidet.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module ll_backend__code_aux, check_hlds__det_analysis.
:- import_module ll_backend__follow_code, hlds__goal_util.
:- import_module hlds__hlds_goal, hlds__hlds_data, hlds__instmap.
:- import_module check_hlds__inst_match.
:- import_module libs__globals, libs__options, hlds__passes_aux.
:- import_module parse_tree__prog_data, check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module ll_backend__code_util, hlds__quantification.
:- import_module check_hlds__modes.
:- import_module bool, list, int, float, map, require.
:- import_module (parse_tree__inst), hlds__hlds_out, std_util.

%------------------------------------------------------------------------------%

evaluate_builtin(PredId, ProcId, Args, GoalInfo0, Goal, GoalInfo,
		InstMap, ModuleInfo0, ModuleInfo) :-
	predicate_module(ModuleInfo0, PredId, ModuleName),
	predicate_name(ModuleInfo0, PredId, PredName),
	proc_id_to_int(ProcId, ProcInt),
	LookupVarInsts = lambda([V::in, J::out] is det, (
		instmap__lookup_var(InstMap, V, VInst),
		J = V - VInst
	)),
	list__map(LookupVarInsts, Args, ArgInsts),
	evaluate_builtin_2(ModuleName, PredName, ProcInt, ArgInsts, GoalInfo0,
		Goal, GoalInfo, ModuleInfo0, ModuleInfo).

:- pred evaluate_builtin_2(module_name, string, int,
		list(pair(prog_var, (inst))), hlds_goal_info, hlds_goal_expr,
		hlds_goal_info, module_info, module_info).
:- mode evaluate_builtin_2(in, in, in, in, in, out, out, in, out) is semidet.

	% Module_info is not actually used at the moment.

evaluate_builtin_2(Module, Pred, ModeNum, Args, GoalInfo0, Goal, GoalInfo,
		ModuleInfo, ModuleInfo) :-
	% -- not yet:
	% Module = qualified(unqualified("std"), Mod),
	Module = unqualified(Mod),
	(
		Args = [X, Y],
		evaluate_builtin_bi(Mod, Pred, ModeNum, X, Y, W, Cons)
	->
		make_construction(W, Cons, Goal),
		goal_info_get_instmap_delta(GoalInfo0, Delta0),
		W = Var - _WInst,
		instmap_delta_set(Delta0, Var,
			bound(unique, [functor(Cons, [])]), Delta),
		goal_info_set_instmap_delta(GoalInfo0, Delta, GoalInfo)
	;
		Args = [X, Y, Z],
		evaluate_builtin_tri(Mod, Pred, ModeNum, X, Y, Z, W, Cons)
	->
		make_construction(W, Cons, Goal),
		goal_info_get_instmap_delta(GoalInfo0, Delta0),
		W = Var - _WInst,
		instmap_delta_set(Delta0, Var,
			bound(unique, [functor(Cons, [])]), Delta),
		goal_info_set_instmap_delta(GoalInfo0, Delta, GoalInfo)
	;
		evaluate_builtin_test(Mod, Pred, ModeNum, Args, Result)
	->
		make_true_or_fail(Result, GoalInfo0, Goal, GoalInfo)
	;
		fail
	).

%------------------------------------------------------------------------------%

:- pred evaluate_builtin_bi(string, string, int,
		pair(prog_var, (inst)), pair(prog_var, (inst)), 
		pair(prog_var, (inst)), cons_id).
:- mode evaluate_builtin_bi(in, in, in, in, in, out, out) is semidet.

	% Integer arithmetic

evaluate_builtin_bi("int", "+", 0, X, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	ZVal is XVal.

evaluate_builtin_bi("int", "-", 0, X, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	ZVal is -XVal.

evaluate_builtin_bi("int", "\\", 0, X, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	ZVal is \ XVal.

	% Floating point arithmetic

evaluate_builtin_bi("float", "+", 0, X, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	ZVal is XVal.

evaluate_builtin_bi("float", "-", 0, X, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	ZVal is -XVal.

%------------------------------------------------------------------------------%

:- pred evaluate_builtin_tri(string, string, int,
		pair(prog_var, (inst)), pair(prog_var, (inst)),
		pair(prog_var, (inst)), pair(prog_var, (inst)), cons_id).
:- mode evaluate_builtin_tri(in, in, in, in, in, in, out, out) is semidet.

	%
	% Integer arithmetic
	%
evaluate_builtin_tri("int", "+", 0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	ZVal is XVal + YVal.
evaluate_builtin_tri("int", "+", 1, X, Y, Z, X, int_const(XVal)) :-
	Z = _ZVar - bound(_ZUniq, [functor(int_const(ZVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	XVal is ZVal - YVal.
evaluate_builtin_tri("int", "+", 2, X, Y, Z, Y, int_const(YVal)) :-
	Z = _ZVar - bound(_ZUniq, [functor(int_const(ZVal), [])]),
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	YVal is ZVal - XVal.

evaluate_builtin_tri("int", "-", 0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	ZVal is XVal - YVal.
evaluate_builtin_tri("int", "-", 1, X, Y, Z, X, int_const(XVal)) :-
	Z = _ZVar - bound(_ZUniq, [functor(int_const(ZVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	XVal is YVal + ZVal.
evaluate_builtin_tri("int", "-", 2, X, Y, Z, Y, int_const(YVal)) :-
	Z = _ZVar - bound(_ZUniq, [functor(int_const(ZVal), [])]),
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	YVal is XVal - ZVal.

evaluate_builtin_tri("int", "*", 0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	ZVal is XVal * YVal.

	% This isn't actually a builtin.
evaluate_builtin_tri("int", "//", 0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	YVal \= 0,
	ZVal is XVal // YVal.

evaluate_builtin_tri("int", "unchecked_quotient", 0, X, Y, Z, Z,
		int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	YVal \= 0,
	ZVal is unchecked_quotient(XVal, YVal).

	% This isn't actually a builtin.
evaluate_builtin_tri("int", "mod", 0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	YVal \= 0,
	ZVal is XVal mod YVal.

	% This isn't actually a builtin.
evaluate_builtin_tri("int", "rem", 0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	YVal \= 0,
	ZVal is XVal rem YVal.

evaluate_builtin_tri("int", "unchecked_rem", 0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	YVal \= 0,
	ZVal is unchecked_rem(XVal, YVal).

evaluate_builtin_tri("int", "unchecked_left_shift",
		0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	ZVal is unchecked_left_shift(XVal, YVal).

	% This isn't actually a builtin.
evaluate_builtin_tri("int", "<<", 0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	ZVal is XVal << YVal.

evaluate_builtin_tri("int", "unchecked_right_shift",
		0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	ZVal is unchecked_right_shift(XVal, YVal).

	% This isn't actually a builtin.
evaluate_builtin_tri("int", ">>", 0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	ZVal is XVal >> YVal.

evaluate_builtin_tri("int", "/\\", 0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	ZVal is XVal /\ YVal.

evaluate_builtin_tri("int", "\\/", 0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	ZVal is XVal \/ YVal.

evaluate_builtin_tri("int", "^", 0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	ZVal = XVal `xor` YVal.

evaluate_builtin_tri("int", "xor", 0, X, Y, Z, Z, int_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	ZVal = XVal `xor` YVal.

	%
	% float arithmetic
	%

evaluate_builtin_tri("float", "+", 0, X, Y, Z, Z, float_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(float_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(float_const(YVal), [])]),
	ZVal is XVal + YVal.

evaluate_builtin_tri("float", "-", 0, X, Y, Z, Z, float_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(float_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(float_const(YVal), [])]),
	ZVal is XVal - YVal.

evaluate_builtin_tri("float", "*", 0, X, Y, Z, Z, float_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(float_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(float_const(YVal), [])]),
	ZVal is XVal * YVal.

	% This isn't actually a builtin.
evaluate_builtin_tri("float", "/", 0, X, Y, Z, Z, float_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(float_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(float_const(YVal), [])]),
	YVal \= 0.0,
	ZVal is XVal / YVal.

evaluate_builtin_tri("float", "unchecked_quotient", 0, X, Y, Z, Z,
		float_const(ZVal)) :-
	X = _XVar - bound(_XUniq, [functor(float_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(float_const(YVal), [])]),
	YVal \= 0.0,
	ZVal is unchecked_quotient(XVal, YVal).

%------------------------------------------------------------------------------%

:- pred evaluate_builtin_test(string, string, int,
		list(pair(prog_var, inst)), bool).
:- mode evaluate_builtin_test(in, in, in, in, out) is semidet.

	% Integer comparisons

evaluate_builtin_test("int", "<", 0, Args, Result) :-
	Args = [X, Y],
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	( XVal < YVal ->
		Result = yes
	;
		Result = no
	).
evaluate_builtin_test("int", "=<", 0, Args, Result) :-
	Args = [X, Y],
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	( XVal =< YVal ->
		Result = yes
	;
		Result = no
	).
evaluate_builtin_test("int", ">", 0, Args, Result) :-
	Args = [X, Y],
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	( XVal > YVal ->
		Result = yes
	;
		Result = no
	).
evaluate_builtin_test("int", ">=", 0, Args, Result) :-
	Args = [X, Y],
	X = _XVar - bound(_XUniq, [functor(int_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(int_const(YVal), [])]),
	( XVal >= YVal ->
		Result = yes
	;
		Result = no
	).

	% Float comparisons

evaluate_builtin_test("float", "<", 0, Args, Result) :-
	Args = [X, Y],
	X = _XVar - bound(_XUniq, [functor(float_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(float_const(YVal), [])]),
	( XVal < YVal ->
		Result = yes
	;
		Result = no
	).
evaluate_builtin_test("float", "=<", 0, Args, Result) :-
	Args = [X, Y],
	X = _XVar - bound(_XUniq, [functor(float_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(float_const(YVal), [])]),
	( XVal =< YVal ->
		Result = yes
	;
		Result = no
	).
evaluate_builtin_test("float", ">", 0, Args, Result) :-
	Args = [X, Y],
	X = _XVar - bound(_XUniq, [functor(float_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(float_const(YVal), [])]),
	( XVal > YVal ->
		Result = yes
	;
		Result = no
	).
evaluate_builtin_test("float", ">=", 0, Args, Result) :-
	Args = [X, Y],
	X = _XVar - bound(_XUniq, [functor(float_const(XVal), [])]),
	Y = _YVar - bound(_YUniq, [functor(float_const(YVal), [])]),
	( XVal >= YVal ->
		Result = yes
	;
		Result = no
	).

%------------------------------------------------------------------------------%

	% recompute_instmap_delta is run by simplify.m if anything changes,
	% so the insts are not important here.
:- pred make_construction(pair(prog_var, inst), cons_id, hlds_goal_expr).
:- mode make_construction(in, in, out) is det.

make_construction(Var - _, ConsId, Goal) :-
	make_const_construction(Var, ConsId, Goal - _).

%------------------------------------------------------------------------------%

:- pred make_true_or_fail(bool, hlds_goal_info, hlds_goal_expr, hlds_goal_info).
:- mode make_true_or_fail(in, in, out, out) is det.

make_true_or_fail(yes, GoalInfo, conj([]), GoalInfo).
make_true_or_fail(no, GoalInfo, disj([]), GoalInfo).

%------------------------------------------------------------------------------%
