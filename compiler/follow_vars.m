%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: conway.
% Major modification by zs.

% This module traverses the goal for every procedure, filling in the
% follow_vars field for call(...) goals, and filling in the initial
% follow_vars in the proc_info.  These follow_vars fields are
% a map(var, lval) which constitute an advisory indication to the code
% generator as to which register each variable should be placed in.
%
% They are computed by traversing the goal BACKWARDS.
% At the end of the goal, we want the output variables to go into their
% corresponding registers, so we initialize the follow_vars accordingly.
% As we traverse throught the goal, at each call(...) we attach the 
% follow_vars map we have computed, and start computing a new one to
% be attached to the preceding call.  When we finish traversing the goal,
% we attach the last computed follow_vars to the proc_info.

%-----------------------------------------------------------------------------%

:- module follow_vars.

:- interface.

:- import_module hlds_module, hlds_pred, llds.

:- pred find_follow_vars(module_info, module_info, io__state, io__state).
:- mode find_follow_vars(in, out, di, uo) is det.

:- pred find_follow_vars_in_proc(proc_info, module_info, proc_info,
		io__state, io__state).
% :- mode find_follow_vars_in_proc(di, in, uo, di, uo) is det.
:- mode find_follow_vars_in_proc(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, hlds_data, mode_util, term, require.
:- import_module code_util, quantification, arg_info, globals.
:- import_module list, map, set, std_util.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `find_follow_vars_in_goal'
	% for each procedure body.

find_follow_vars(ModuleInfo0, ModuleInfo1) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	find_follow_vars_in_preds(PredIds, ModuleInfo0, ModuleInfo1).

:- pred find_follow_vars_in_preds(list(pred_id), module_info, module_info,
				io__state, io__state).
:- mode find_follow_vars_in_preds(in, in, out, di, uo) is det.

find_follow_vars_in_preds([], ModuleInfo, ModuleInfo) --> [].
find_follow_vars_in_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	find_follow_vars_in_procs(ProcIds, PredId, ModuleInfo0,
		ModuleInfo1),
	find_follow_vars_in_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred find_follow_vars_in_procs(list(proc_id), pred_id, module_info,
					module_info, io__state, io__state).
:- mode find_follow_vars_in_procs(in, in, in, out, di, uo) is det.

find_follow_vars_in_procs([], _PredId, ModuleInfo, ModuleInfo) --> [].
find_follow_vars_in_procs([ProcId | ProcIds], PredId, ModuleInfo0,
					ModuleInfo) -->
	{
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0)
	},

	find_follow_vars_in_proc(ProcInfo0, ModuleInfo0, ProcInfo),

	{
	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1)
	},

	find_follow_vars_in_procs(ProcIds, PredId, ModuleInfo1, ModuleInfo).

find_follow_vars_in_proc(ProcInfo0, ModuleInfo, ProcInfo) -->
	globals__io_get_args_method(ArgsMethod),
	{
	proc_info_goal(ProcInfo0, Goal0),

	find_final_follow_vars(ProcInfo0, FollowVars0),
	find_follow_vars_in_goal(Goal0, ArgsMethod, ModuleInfo, FollowVars0,
		Goal, FollowVars),

	proc_info_set_follow_vars(ProcInfo0, FollowVars, ProcInfo1),
	proc_info_set_goal(ProcInfo1, Goal, ProcInfo)
	}.

%-----------------------------------------------------------------------------%

:- pred find_final_follow_vars(proc_info, follow_vars).
:- mode find_final_follow_vars(in, out) is det.

find_final_follow_vars(ProcInfo, Follow) :-
	proc_info_arg_info(ProcInfo, ArgInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	map__init(Follow0),
	( find_final_follow_vars_2(ArgInfo, HeadVars, Follow0, Follow1) ->
		Follow = Follow1
	;
		error("find_final_follow_vars: failed")
	).

:- pred find_final_follow_vars_2(list(arg_info), list(var),
						follow_vars, follow_vars).
:- mode find_final_follow_vars_2(in, in, in, out) is semidet.

find_final_follow_vars_2([], [], Follow, Follow).
find_final_follow_vars_2([arg_info(Loc, Mode)|Args], [Var|Vars],
							Follow0, Follow) :-
	code_util__arg_loc_to_register(Loc, Reg),
	(
		Mode = top_out
	->
		map__set(Follow0, Var, reg(Reg), Follow1)
	;
		Follow0 = Follow1
	),
	find_final_follow_vars_2(Args, Vars, Follow1, Follow).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred find_follow_vars_in_goal(hlds__goal, args_method, module_info,
				follow_vars, hlds__goal, follow_vars).
:- mode find_follow_vars_in_goal(in, in, in, in, out, out) is det.

find_follow_vars_in_goal(Goal0 - GoalInfo, ArgsMethod, ModuleInfo, FollowVars0,
					Goal - GoalInfo, FollowVars) :-
	find_follow_vars_in_goal_2(Goal0, ArgsMethod, ModuleInfo, FollowVars0,
							Goal, FollowVars).

%-----------------------------------------------------------------------------%

:- pred find_follow_vars_in_goal_2(hlds__goal_expr, args_method, module_info,
		follow_vars, hlds__goal_expr, follow_vars).
:- mode find_follow_vars_in_goal_2(in, in, in, in, out, out) is det.

find_follow_vars_in_goal_2(conj(Goals0), ArgsMethod, ModuleInfo, FollowVars0,
		conj(Goals), FollowVars) :-
	find_follow_vars_in_conj(Goals0, ArgsMethod, ModuleInfo, FollowVars0,
		Goals, FollowVars).

find_follow_vars_in_goal_2(disj(Goals0, _), ArgsMethod, ModuleInfo, FollowVars0,
		disj(Goals, FollowVars0), FollowVars) :-
	find_follow_vars_in_disj(Goals0, ArgsMethod, ModuleInfo, FollowVars0,
		Goals, FollowVars).

find_follow_vars_in_goal_2(not(Goal0), ArgsMethod, ModuleInfo, FollowVars0,
		not(Goal), FollowVars) :-
	find_follow_vars_in_goal(Goal0, ArgsMethod, ModuleInfo, FollowVars0,
		Goal, FollowVars).

find_follow_vars_in_goal_2(switch(Var, Det, Cases0, _), ArgsMethod, ModuleInfo,
		FollowVars0,
		switch(Var, Det, Cases, FollowVars0), FollowVars) :-
	find_follow_vars_in_cases(Cases0, ArgsMethod, ModuleInfo, FollowVars0,
		Cases, FollowVars).

find_follow_vars_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0, _),
		ArgsMethod, ModuleInfo, FollowVars0,
		if_then_else(Vars, Cond, Then, Else, FollowVars0),
		FollowVars) :-
	find_follow_vars_in_goal(Then0, ArgsMethod, ModuleInfo, FollowVars0,
		Then, FollowVars1),
	find_follow_vars_in_goal(Cond0, ArgsMethod, ModuleInfo, FollowVars1,
		Cond, FollowVars),
		% To a first approximation, ignore the else branch.
	find_follow_vars_in_goal(Else0, ArgsMethod, ModuleInfo, FollowVars0,
		Else, _FollowVars1A).

find_follow_vars_in_goal_2(some(Vars, Goal0), ArgsMethod, ModuleInfo,
		FollowVars0, some(Vars, Goal), FollowVars) :-
	find_follow_vars_in_goal(Goal0, ArgsMethod, ModuleInfo, FollowVars0,
		Goal, FollowVars).

find_follow_vars_in_goal_2(
		higher_order_call(PredVar, Args, Types, Modes, Det, _),
		ArgsMethod, ModuleInfo, FollowVars0,
		higher_order_call(PredVar, Args, Types, Modes, Det,
			FollowVars0),
		FollowVars) :-
	determinism_to_code_model(Det, CodeModel),
	make_arg_infos(ArgsMethod, Types, Modes, CodeModel, ModuleInfo,
		ArgInfo),
	find_follow_vars_from_arginfo(ArgInfo, Args, FollowVars).

find_follow_vars_in_goal_2(call(A,B,C,D,E,F,_), _ArgsMethod, ModuleInfo,
		FollowVars0, call(A,B,C,D,E,F, FollowVars0), FollowVars) :-
	(
		hlds__is_builtin_is_inline(D)
	->
		FollowVars = FollowVars0
	;
		% XXX this code should pay attention to ArgsMethod
		find_follow_vars_in_call(A, B, C, ModuleInfo, FollowVars0,
			FollowVars)
	).

find_follow_vars_in_goal_2(unify(A,B,C,D0,E), ArgsMethod, _ModuleInfo,
		FollowVars0, unify(A,B,C,D,E), FollowVars) :-
	(
		B = var(BVar),
		D0 = complicated_unify(Mode, CanFail, _F)
	->
		determinism_components(Det, CanFail, at_most_one),
		determinism_to_code_model(Det, CodeModel),
		arg_info__unify_arg_info(ArgsMethod, CodeModel, ArgInfo),
		find_follow_vars_from_arginfo(ArgInfo, [A, BVar], FollowVars),
		D = complicated_unify(Mode, CanFail, FollowVars0)
	;
		D = D0,
		FollowVars = FollowVars0
	).

find_follow_vars_in_goal_2(pragma_c_code(A,B,C,D,E,F), _ArgInfo, _ModuleInfo,
	FollowVars, pragma_c_code(A,B,C,D,E,F), FollowVars).

%-----------------------------------------------------------------------------%

:- pred find_follow_vars_in_call(pred_id, proc_id, list(var), module_info,
						follow_vars, follow_vars).
:- mode find_follow_vars_in_call(in, in, in, in, in, out) is det.

find_follow_vars_in_call(PredId, ProcId, Args, ModuleInfo, _Follow, Follow) :-
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId, ProcInfo),
	proc_info_arg_info(ProcInfo, ArgInfo),
	find_follow_vars_from_arginfo(ArgInfo, Args, Follow).

:- pred find_follow_vars_from_arginfo(list(arg_info), list(var), follow_vars).
:- mode find_follow_vars_from_arginfo(in, in, out) is det.

find_follow_vars_from_arginfo(ArgInfo, Args, Follow) :-
	map__init(Follow0),
	(
		find_follow_vars_from_arginfo_2(ArgInfo, Args, Follow0, Follow1)
	->
		Follow = Follow1
	;
		error("find_follow_vars_from_arginfo: failed")
	).

:- pred find_follow_vars_from_arginfo_2(list(arg_info), list(var),
						follow_vars, follow_vars).
:- mode find_follow_vars_from_arginfo_2(in, in, in, out) is semidet.

find_follow_vars_from_arginfo_2([], [], Follow, Follow).
find_follow_vars_from_arginfo_2([arg_info(Loc, Mode)|Args], [Var|Vars],
							Follow0, Follow) :-
	code_util__arg_loc_to_register(Loc, Reg),
	(
		Mode = top_in
	->
		map__set(Follow0, Var, reg(Reg), Follow1)
	;
		Follow0 = Follow1
	),
	find_follow_vars_from_arginfo_2(Args, Vars, Follow1, Follow).

%-----------------------------------------------------------------------------%

:- pred find_follow_vars_in_disj(list(hlds__goal), args_method, module_info,
				follow_vars, list(hlds__goal), follow_vars).
:- mode find_follow_vars_in_disj(in, in, in, in, out, out) is det.

find_follow_vars_in_disj([], _ArgsMethod, _ModuleInfo, FollowVars,
			[], FollowVars).
find_follow_vars_in_disj([Goal0|Goals0], ArgsMethod, ModuleInfo, FollowVars0,
						[Goal|Goals], FollowVars) :-
	find_follow_vars_in_goal(Goal0, ArgsMethod, ModuleInfo, FollowVars0,
		Goal, FollowVars),
	find_follow_vars_in_disj(Goals0, ArgsMethod, ModuleInfo, FollowVars0,
		Goals, _FollowVars1).

%-----------------------------------------------------------------------------%

:- pred find_follow_vars_in_cases(list(case), args_method, module_info,
				follow_vars, list(case), follow_vars).
:- mode find_follow_vars_in_cases(in, in, in, in, out, out) is det.

find_follow_vars_in_cases([], _ArgsMethod, _ModuleInfo, FollowVars,
			[], FollowVars).
find_follow_vars_in_cases([case(Cons, Goal0)|Goals0], ArgsMethod, ModuleInfo,
			FollowVars0, [case(Cons, Goal)|Goals], FollowVars) :-
	find_follow_vars_in_goal(Goal0, ArgsMethod, ModuleInfo, FollowVars0,
		Goal, FollowVars),
	find_follow_vars_in_cases(Goals0, ArgsMethod, ModuleInfo, FollowVars0,
		Goals, _FollowVars1).

%-----------------------------------------------------------------------------%

:- pred find_follow_vars_in_conj(list(hlds__goal), args_method, module_info,
			follow_vars, list(hlds__goal), follow_vars).
:- mode find_follow_vars_in_conj(in, in, in, in, out, out) is det.

find_follow_vars_in_conj([], _ArgsMethod, _ModuleInfo, FollowVars,
			[], FollowVars).
find_follow_vars_in_conj([Goal0 | Goals0], ArgsMethod, ModuleInfo, FollowVars0,
			[Goal | Goals], FollowVars) :-
	find_follow_vars_in_conj(Goals0, ArgsMethod, ModuleInfo, FollowVars0,
		Goals, FollowVars1),
	find_follow_vars_in_goal(Goal0, ArgsMethod, ModuleInfo, FollowVars1,
		Goal, FollowVars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
