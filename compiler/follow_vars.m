%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: conway.
% Major modification by zs.

% This module traverses the goal for every procedure, filling in the
% follow_vars fields of some goals. These fields constitute an advisory
% indication to the code generator as to what location each variable
% should be placed in.
%
% The desired locations of variables are computed by traversing the goal
% BACKWARDS. At the end of the procedure, we want the output variables
% to go into their corresponding registers, so we initialize the follow_vars
% accordingly. At each call or higher order call we reset the follow_vars set
% to reflect where variables should be to make the setting up of the arguments
% of the call as efficient as possible.

% See compiler/notes/allocation.html for a description of the framework that
% this pass operates within, and for a description of which goals have their
% follow_vars field filled in.

%-----------------------------------------------------------------------------%

:- module follow_vars.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_goal.

:- pred find_final_follow_vars(proc_info, follow_vars).
:- mode find_final_follow_vars(in, out) is det.

:- pred find_follow_vars_in_goal(hlds_goal, module_info,
				follow_vars, hlds_goal, follow_vars).
:- mode find_follow_vars_in_goal(in, in, in, out, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_data, llds, mode_util, prog_data.
:- import_module code_util, quantification, arg_info, globals.
:- import_module bool, list, map, set, std_util, require.

%-----------------------------------------------------------------------------%

find_final_follow_vars(ProcInfo, Follow) :-
	proc_info_arg_info(ProcInfo, ArgInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	map__init(Follow0),
	( find_final_follow_vars_2(ArgInfo, HeadVars, Follow0, Follow1) ->
		Follow = Follow1
	;
		error("find_final_follow_vars: failed")
	).

:- pred find_final_follow_vars_2(list(arg_info), list(prog_var),
						follow_vars, follow_vars).
:- mode find_final_follow_vars_2(in, in, in, out) is semidet.

find_final_follow_vars_2([], [], Follow, Follow).
find_final_follow_vars_2([arg_info(Loc, Mode) | Args], [Var | Vars],
							Follow0, Follow) :-
	code_util__arg_loc_to_register(Loc, Reg),
	(
		Mode = top_out
	->
		map__det_insert(Follow0, Var, Reg, Follow1)
	;
		Follow0 = Follow1
	),
	find_final_follow_vars_2(Args, Vars, Follow1, Follow).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

find_follow_vars_in_goal(Goal0 - GoalInfo, ModuleInfo, FollowVars0,
					Goal - GoalInfo, FollowVars) :-
	find_follow_vars_in_goal_2(Goal0, ModuleInfo, FollowVars0,
					Goal, FollowVars).

%-----------------------------------------------------------------------------%

:- pred find_follow_vars_in_goal_2(hlds_goal_expr, module_info,
		follow_vars, hlds_goal_expr, follow_vars).
:- mode find_follow_vars_in_goal_2(in, in, in, out, out) is det.

find_follow_vars_in_goal_2(conj(Goals0), ModuleInfo, FollowVars0,
		conj(Goals), FollowVars) :-
	find_follow_vars_in_conj(Goals0, ModuleInfo, FollowVars0,
		no, Goals, FollowVars).

find_follow_vars_in_goal_2(par_conj(Goals0, SM), ModuleInfo,
		FollowVars0, par_conj(Goals, SM), FollowVars) :-
		% find_follow_vars_in_disj treats its list of goals as a
		% series of independent goals, so we can use it to process
		% independent parallel conjunction.
	find_follow_vars_in_disj(Goals0, ModuleInfo, FollowVars0,
		Goals, FollowVars).

	% We record that at the end of each disjunct, live variables should
	% be in the locations given by the initial follow_vars, which reflects
	% the requirements of the code following the disjunction.

find_follow_vars_in_goal_2(disj(Goals0, _), ModuleInfo, FollowVars0,
		disj(Goals, FollowVars0), FollowVars) :-
	find_follow_vars_in_disj(Goals0, ModuleInfo, FollowVars0,
		Goals, FollowVars).

find_follow_vars_in_goal_2(not(Goal0), ModuleInfo, FollowVars0,
		not(Goal), FollowVars) :-
	find_follow_vars_in_goal(Goal0, ModuleInfo, FollowVars0,
		Goal, FollowVars).

	% We record that at the end of each arm of the switch, live variables
	% should be in the locations given by the initial follow_vars, which
	% reflects the requirements of the code following the switch.

find_follow_vars_in_goal_2(switch(Var, Det, Cases0, _), ModuleInfo, FollowVars0,
		switch(Var, Det, Cases, FollowVars0), FollowVars) :-
	find_follow_vars_in_cases(Cases0, ModuleInfo, FollowVars0,
		Cases, FollowVars).

	% Set the follow_vars field for the condition, the then-part and the
	% else-part, since in general they have requirements about where
	% variables should be.

	% We use the requirement of the condition as the requirement of
	% the if-then-else itself, since the condition will definitely
	% be entered first. Since part of the condition may fail early,
	% taking into account the preferences of the else part may be
	% worthwhile. The preferences of the then part are already taken
	% into account, since they are an input to the computation of
	% the follow_vars for the condition.

	% We record that at the end of both the then-part and the else-part,
	% live variables should be in the locations given by the initial
	% follow_vars, which reflects the requirements of the code
	% following the if-then-else.

find_follow_vars_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0, _),
		ModuleInfo, FollowVars0,
		if_then_else(Vars, Cond, Then, Else, FollowVars0),
		FollowVarsCond) :-
	find_follow_vars_in_goal(Then0, ModuleInfo, FollowVars0,
		Then1, FollowVarsThen),
	goal_set_follow_vars(Then1, yes(FollowVarsThen), Then),
	find_follow_vars_in_goal(Cond0, ModuleInfo, FollowVarsThen,
		Cond1, FollowVarsCond),
	goal_set_follow_vars(Cond1, yes(FollowVarsCond), Cond),
	find_follow_vars_in_goal(Else0, ModuleInfo, FollowVars0,
		Else1, FollowVarsElse),
	goal_set_follow_vars(Else1, yes(FollowVarsElse), Else).

find_follow_vars_in_goal_2(some(Vars, Goal0), ModuleInfo,
		FollowVars0, some(Vars, Goal), FollowVars) :-
	find_follow_vars_in_goal(Goal0, ModuleInfo, FollowVars0,
		Goal, FollowVars).

	% XXX These follow-vars aren't correct since the desired positions for
	% XXX the arguments are different from an ordinary call --- they are
	% XXX as required by do_call_{det,semidet,nondet}_closure
find_follow_vars_in_goal_2(
		higher_order_call(PredVar, Args, Types, Modes, Det,
			IsPredOrFunc),
		ModuleInfo, _FollowVars0,
		higher_order_call(PredVar, Args, Types, Modes, Det,
			IsPredOrFunc),
		FollowVars) :-
	determinism_to_code_model(Det, CodeModel),
	module_info_globals(ModuleInfo, Globals),
	arg_info__ho_call_args_method(Globals, ArgsMethod),
	make_arg_infos(ArgsMethod, Types, Modes, CodeModel, ModuleInfo,
		ArgInfo),
	find_follow_vars_from_arginfo(ArgInfo, Args, FollowVars).

	% XXX These follow-vars aren't correct since the desired positions for
	% XXX the arguments are different from an ordinary call --- they are
	% XXX as required by do_call_{det,semidet,nondet}_class_method
find_follow_vars_in_goal_2(
		class_method_call(TypeClassInfoVar, Num, Args, Types, Modes,
			Det),
		ModuleInfo, _FollowVars0,
		class_method_call(TypeClassInfoVar, Num, Args, Types, Modes,
			Det),
		FollowVars) :-
	determinism_to_code_model(Det, CodeModel),
	module_info_globals(ModuleInfo, Globals),
	globals__get_args_method(Globals, ArgsMethod),
	( ArgsMethod = compact ->
		true
	;
		error("Sorry, typeclasses with simple args_method not yet implemented")
	),
	make_arg_infos(ArgsMethod, Types, Modes, CodeModel, ModuleInfo,
		ArgInfo),
	find_follow_vars_from_arginfo(ArgInfo, Args, FollowVars).

find_follow_vars_in_goal_2(call(A,B,C,D,E,F), ModuleInfo,
		FollowVars0, call(A,B,C,D,E,F), FollowVars) :-
	(
		D = inline_builtin
	->
		FollowVars = FollowVars0
	;
		find_follow_vars_in_call(A, B, C, ModuleInfo, FollowVars)
	).

find_follow_vars_in_goal_2(unify(A,B,C,D,E), _ModuleInfo,
		FollowVars0, unify(A,B,C,D,E), FollowVars) :-
	(
		D = assign(LVar, RVar),
		map__search(FollowVars0, LVar, DesiredLoc)
	->
		map__set(FollowVars0, RVar, DesiredLoc, FollowVars)
	;
		FollowVars = FollowVars0
	).

find_follow_vars_in_goal_2(pragma_c_code(A,B,C,D,E,F,G), 
		_ModuleInfo, FollowVars,
		pragma_c_code(A,B,C,D,E,F,G), FollowVars).

%-----------------------------------------------------------------------------%

:- pred find_follow_vars_in_call(pred_id, proc_id, list(prog_var), module_info,
						follow_vars).
:- mode find_follow_vars_in_call(in, in, in, in, out) is det.

find_follow_vars_in_call(PredId, ProcId, Args, ModuleInfo, Follow) :-
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId, ProcInfo),
	proc_info_arg_info(ProcInfo, ArgInfo),
	find_follow_vars_from_arginfo(ArgInfo, Args, Follow).

:- pred find_follow_vars_from_arginfo(list(arg_info), list(prog_var),
		follow_vars).
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

:- pred find_follow_vars_from_arginfo_2(list(arg_info), list(prog_var),
						follow_vars, follow_vars).
:- mode find_follow_vars_from_arginfo_2(in, in, in, out) is semidet.

find_follow_vars_from_arginfo_2([], [], Follow, Follow).
find_follow_vars_from_arginfo_2([arg_info(Loc, Mode) | Args], [Var | Vars],
							Follow0, Follow) :-
	code_util__arg_loc_to_register(Loc, Reg),
	(
		Mode = top_in
	->
		map__set(Follow0, Var, Reg, Follow1)
	;
		Follow0 = Follow1
	),
	find_follow_vars_from_arginfo_2(Args, Vars, Follow1, Follow).

%-----------------------------------------------------------------------------%

	% We attach a follow_vars to each arm of a switch, since inside
	% each arm the preferred locations for variables will in general
	% be different.

	% For the time being, we return the follow_vars computed from
	% the first arm as the preferred requirements of the switch as
	% a whole. This is close to right, since the first disjunct will
	% definitely be the first to be entered. However, the follow_vars
	% computed for the disjunction as a whole can profitably mention
	% variables that are not live in the first disjunct, but may be
	% needed in the second and later disjuncts. In general, we may
	% wish to take into account the requirements of all disjuncts
	% up to the first non-failing disjunct. (The requirements of
	% later disjuncts are not relevant. For model_non disjunctions,
	% they can only be entered with everything in stack slots; for
	% model_det and model_semi disjunctions, they will never be
	% entered at all.)
	%
	% This code is used both for disjunction and parallel conjunction.

:- pred find_follow_vars_in_disj(list(hlds_goal), module_info,
				follow_vars, list(hlds_goal), follow_vars).
:- mode find_follow_vars_in_disj(in, in, in, out, out) is det.

find_follow_vars_in_disj([], _ModuleInfo, FollowVars,
			[], FollowVars).
find_follow_vars_in_disj([Goal0 | Goals0], ModuleInfo, FollowVars0,
						[Goal | Goals], FollowVars) :-
	find_follow_vars_in_goal(Goal0, ModuleInfo, FollowVars0,
		Goal1, FollowVars),
	goal_set_follow_vars(Goal1, yes(FollowVars), Goal),
	find_follow_vars_in_disj(Goals0, ModuleInfo, FollowVars0,
		Goals, _FollowVars1).

%-----------------------------------------------------------------------------%

	% We attach a follow_vars to each arm of a switch, since inside
	% each arm the preferred locations for variables will in general
	% be different.

	% For the time being, we return the follow_vars computed from
	% the first arm as the preferred requirements of the switch as
	% a whole. This can be improved, both to include variables that
	% are not live in that branch (and therefore don't appear in
	% its follow_vars) and to let different branches "vote" on
	% what should be in registers.

:- pred find_follow_vars_in_cases(list(case), module_info,
				follow_vars, list(case), follow_vars).
:- mode find_follow_vars_in_cases(in, in, in, out, out) is det.

find_follow_vars_in_cases([], _ModuleInfo, FollowVars, [], FollowVars).
find_follow_vars_in_cases([case(Cons, Goal0) | Goals0], ModuleInfo,
			FollowVars0, [case(Cons, Goal) | Goals], FollowVars) :-
	find_follow_vars_in_goal(Goal0, ModuleInfo, FollowVars0,
		Goal1, FollowVars),
	goal_set_follow_vars(Goal1, yes(FollowVars), Goal),
	find_follow_vars_in_cases(Goals0, ModuleInfo, FollowVars0,
		Goals, _FollowVars1).

%-----------------------------------------------------------------------------%

	% We attach the follow_vars to each goal that follows a goal
	% that is not cachable by the code generator.

:- pred find_follow_vars_in_conj(list(hlds_goal), module_info,
			follow_vars, bool, list(hlds_goal), follow_vars).
:- mode find_follow_vars_in_conj(in, in, in, in, out, out) is det.

find_follow_vars_in_conj([], _ModuleInfo, FollowVars,
		_AttachToFirst, [], FollowVars).
find_follow_vars_in_conj([Goal0 | Goals0], ModuleInfo, FollowVars0,
		AttachToFirst, [Goal | Goals], FollowVars) :-
	(
		Goal0 = GoalExpr0 - _,
		(
			GoalExpr0 = call(_, _, _, BuiltinState, _, _),
			BuiltinState = inline_builtin
		;
			GoalExpr0 = unify(_, _, _, Unification, _),
			Unification \= complicated_unify(_, _)
		)
	->
		AttachToNext = no
	;
		AttachToNext = yes
	),
	find_follow_vars_in_conj(Goals0, ModuleInfo, FollowVars0,
		AttachToNext, Goals, FollowVars1),
	find_follow_vars_in_goal(Goal0, ModuleInfo, FollowVars1,
		Goal1, FollowVars),
	(
		AttachToFirst = yes,
		goal_set_follow_vars(Goal1, yes(FollowVars), Goal)
	;
		AttachToFirst = no,
		Goal = Goal1
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
