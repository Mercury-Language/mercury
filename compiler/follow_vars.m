%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main authors: conway, zs.

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

:- module ll_backend__follow_vars.

:- interface.

:- import_module parse_tree__prog_data.
:- import_module hlds__hlds_module, hlds__hlds_pred, hlds__hlds_goal.
:- import_module hlds__hlds_llds.

:- import_module map.

:- pred find_final_follow_vars(proc_info::in, follow_vars_map::out, int::out)
	is det.

:- pred find_follow_vars_in_goal(hlds_goal::in, map(prog_var, type)::in,
	module_info::in, follow_vars_map::in, int::in,
	hlds_goal::out, follow_vars_map::out, int::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_data.
:- import_module hlds__hlds_data, hlds__quantification, check_hlds__mode_util.
:- import_module backend_libs__code_model.
:- import_module ll_backend__llds, ll_backend__call_gen.
:- import_module ll_backend__code_util, ll_backend__arg_info.
:- import_module libs__globals.

:- import_module bool, int, list, assoc_list, map, set, std_util, require.

%-----------------------------------------------------------------------------%

find_final_follow_vars(ProcInfo, FollowVarsMap, NextNonReserved) :-
	proc_info_arg_info(ProcInfo, ArgInfo),
	proc_info_headvars(ProcInfo, HeadVars),
	assoc_list__from_corresponding_lists(ArgInfo, HeadVars,
		ArgInfoHeadVars),
	map__init(FollowVarsMap0),
	find_final_follow_vars_2(ArgInfoHeadVars, FollowVarsMap0, 1,
		FollowVarsMap, NextNonReserved).

:- pred find_final_follow_vars_2(assoc_list(arg_info, prog_var)::in,
	follow_vars_map::in, int::in, follow_vars_map::out, int::out) is det.

find_final_follow_vars_2([], FollowMap, NextNonReserved,
		FollowMap, NextNonReserved).
find_final_follow_vars_2([arg_info(Loc, Mode) - Var | ArgInfoVars],
		FollowVarsMap0, NextNonReserved0,
		FollowVarsMap, NextNonReserved) :-
	code_util__arg_loc_to_register(Loc, Reg),
	( Mode = top_out ->
		map__det_insert(FollowVarsMap0, Var, Reg, FollowVarsMap1),
		int__max(NextNonReserved0, Loc + 1, NextNonReserved1)
	;
		FollowVarsMap0 = FollowVarsMap1,
		NextNonReserved1 = NextNonReserved0
	),
	find_final_follow_vars_2(ArgInfoVars, FollowVarsMap1, NextNonReserved1,
		FollowVarsMap, NextNonReserved).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

find_follow_vars_in_goal(Goal0 - GoalInfo0, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		Goal - GoalInfo, FollowVarsMap, NextNonReserved) :-
	find_follow_vars_in_goal_expr(Goal0, GoalInfo0, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		Goal, GoalInfo, FollowVarsMap, NextNonReserved).

%-----------------------------------------------------------------------------%

:- pred find_follow_vars_in_goal_expr(hlds_goal_expr::in, hlds_goal_info::in,
	map(prog_var, type)::in, module_info::in, follow_vars_map::in, int::in,
	hlds_goal_expr::out, hlds_goal_info::out, follow_vars_map::out,
	int::out) is det.

find_follow_vars_in_goal_expr(conj(Goals0), GoalInfo,
		VarTypes, ModuleInfo, FollowVarsMap0, NextNonReserved0,
		conj(Goals), GoalInfo, FollowVarsMap, NextNonReserved) :-
	find_follow_vars_in_conj(Goals0, VarTypes, ModuleInfo,
		no, FollowVarsMap0, NextNonReserved0,
		Goals, FollowVarsMap, NextNonReserved).

find_follow_vars_in_goal_expr(par_conj(Goals0), GoalInfo,
		VarTypes, ModuleInfo, FollowVarsMap0, NextNonReserved0,
		par_conj(Goals), GoalInfo, FollowVarsMap, NextNonReserved) :-
		% find_follow_vars_in_disj treats its list of goals as a
		% series of independent goals, so we can use it to process
		% independent parallel conjunction.
	find_follow_vars_in_disj(Goals0, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		Goals, FollowVarsMap, NextNonReserved).

	% We record that at the end of each disjunct, live variables should
	% be in the locations given by the initial follow_vars, which reflects
	% the requirements of the code following the disjunction.

find_follow_vars_in_goal_expr(disj(Goals0), GoalInfo0,
		VarTypes, ModuleInfo, FollowVarsMap0, NextNonReserved0,
		disj(Goals), GoalInfo, FollowVarsMap, NextNonReserved) :-
	find_follow_vars_in_disj(Goals0, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		Goals, FollowVarsMap, NextNonReserved),
	goal_info_set_store_map(GoalInfo0, FollowVarsMap0, GoalInfo).

find_follow_vars_in_goal_expr(not(Goal0), GoalInfo, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		not(Goal), GoalInfo, FollowVarsMap, NextNonReserved) :-
	find_follow_vars_in_goal(Goal0, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		Goal, FollowVarsMap, NextNonReserved).

	% We record that at the end of each arm of the switch, live variables
	% should be in the locations given by the initial follow_vars, which
	% reflects the requirements of the code following the switch.

find_follow_vars_in_goal_expr(switch(Var, Det, Cases0), GoalInfo0,
		VarTypes, ModuleInfo, FollowVarsMap0, NextNonReserved0,
		switch(Var, Det, Cases), GoalInfo,
		FollowVarsMap, NextNonReserved) :-
	find_follow_vars_in_cases(Cases0, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		Cases, FollowVarsMap, NextNonReserved),
	goal_info_set_store_map(GoalInfo0, FollowVarsMap0, GoalInfo).

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

find_follow_vars_in_goal_expr(
		if_then_else(Vars, Cond0, Then0, Else0), GoalInfo0,
		VarTypes, ModuleInfo, FollowVarsMap0, NextNonReserved0,
		if_then_else(Vars, Cond, Then, Else), GoalInfo,
		FollowVarsMapCond, NextNonReservedCond) :-
	find_follow_vars_in_goal(Then0, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		Then1, FollowVarsMapThen, NextNonReservedThen),
	FollowVarsThen = follow_vars(FollowVarsMapThen, NextNonReservedThen),
	goal_set_follow_vars(Then1, yes(FollowVarsThen), Then),

	find_follow_vars_in_goal(Cond0, VarTypes, ModuleInfo,
		FollowVarsMapThen, NextNonReservedThen,
		Cond1, FollowVarsMapCond, NextNonReservedCond),
	FollowVarsCond = follow_vars(FollowVarsMapCond, NextNonReservedCond),
	goal_set_follow_vars(Cond1, yes(FollowVarsCond), Cond),

	find_follow_vars_in_goal(Else0, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		Else1, FollowVarsMapElse, NextNonReservedElse),
	FollowVarsElse = follow_vars(FollowVarsMapElse, NextNonReservedElse),
	goal_set_follow_vars(Else1, yes(FollowVarsElse), Else),

	goal_info_set_store_map(GoalInfo0, FollowVarsMap0, GoalInfo).

find_follow_vars_in_goal_expr(some(Vars, CanRemove, Goal0), GoalInfo,
		VarTypes, ModuleInfo, FollowVarsMap0, NextNonReserved0,
		some(Vars, CanRemove, Goal), GoalInfo,
		FollowVarsMap, NextNonReserved) :-
	find_follow_vars_in_goal(Goal0, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		Goal, FollowVarsMap, NextNonReserved).

find_follow_vars_in_goal_expr(unify(A,B,C,D,E), GoalInfo,
		_VarTypes, _ModuleInfo, FollowVarsMap0, NextNonReserved,
		unify(A,B,C,D,E), GoalInfo, FollowVarsMap, NextNonReserved) :-
	(
		D = assign(LVar, RVar),
		map__search(FollowVarsMap0, LVar, DesiredLoc)
	->
		map__set(FollowVarsMap0, RVar, DesiredLoc, FollowVarsMap)
	;
		FollowVarsMap = FollowVarsMap0
	).

find_follow_vars_in_goal_expr(foreign_proc(A,B,C,D,E,F,G), GoalInfo,
		_, _ModuleInfo, FollowVarsMap, NextNonReserved,
		foreign_proc(A,B,C,D,E,F,G), GoalInfo,
		FollowVarsMap, NextNonReserved).

find_follow_vars_in_goal_expr(shorthand(_), _, _, _, _, _, _, _, _, _) :-
	% these should have been expanded out by now
	error("find_follow_vars_in_goal_2: unexpected shorthand").

find_follow_vars_in_goal_expr(
		generic_call(GenericCall, Args, Modes, Det), GoalInfo,
		VarTypes, ModuleInfo, _FollowVarsMap0, _NextNonReserved0,
		generic_call(GenericCall, Args, Modes, Det), GoalInfo,
		FollowVarsMap, NextNonReserved) :-
	determinism_to_code_model(Det, CodeModel),
	map__apply_to_list(Args, VarTypes, Types),
	call_gen__maybe_remove_aditi_state_args(GenericCall,
		Args, Types, Modes, EffArgs, EffTypes, EffModes),
	make_arg_infos(EffTypes, EffModes, CodeModel, ModuleInfo, EffArgInfos),
	assoc_list__from_corresponding_lists(EffArgs, EffArgInfos,
		EffArgsInfos),
	arg_info__partition_args(EffArgsInfos, EffInVarInfos, _),
	assoc_list__keys(EffInVarInfos, EffInVars),
	call_gen__generic_call_info(CodeModel, GenericCall, _,
		SpecifierArgInfos, FirstInput),
	map__init(FollowVarsMap0),
	find_follow_vars_from_arginfo(SpecifierArgInfos, FollowVarsMap0, 1,
		FollowVarsMap1, _),
	find_follow_vars_from_sequence(EffInVars, FirstInput, FollowVarsMap1,
		FollowVarsMap, NextNonReserved).

find_follow_vars_in_goal_expr(call(PredId, ProcId, Args, State, E, F),
		GoalInfo, _, ModuleInfo, FollowVarsMap0, NextNonReserved0,
		call(PredId, ProcId, Args, State, E, F), GoalInfo,
		FollowVarsMap, NextNonReserved) :-
	( State = inline_builtin ->
		FollowVarsMap = FollowVarsMap0,
		NextNonReserved = NextNonReserved0
	;
		find_follow_vars_in_call(PredId, ProcId, Args, ModuleInfo,
			FollowVarsMap, NextNonReserved)
	).

%-----------------------------------------------------------------------------%

:- pred find_follow_vars_in_call(pred_id::in, proc_id::in, list(prog_var)::in,
	module_info::in, follow_vars_map::out, int::out) is det.

find_follow_vars_in_call(PredId, ProcId, Args, ModuleInfo,
		FollowVarsMap, NextNonReserved) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_arg_info(ProcInfo, ArgInfo),
	assoc_list__from_corresponding_lists(Args, ArgInfo, ArgsInfos),
	map__init(FollowVarsMap0),
	find_follow_vars_from_arginfo(ArgsInfos, FollowVarsMap0, 1,
		FollowVarsMap, NextNonReserved).

:- pred find_follow_vars_from_arginfo(assoc_list(prog_var, arg_info)::in,
	follow_vars_map::in, int::in, follow_vars_map::out, int::out) is det.

find_follow_vars_from_arginfo([], FollowVarsMap, NextNonReserved,
		FollowVarsMap, NextNonReserved).
find_follow_vars_from_arginfo([ArgVar - arg_info(Loc, Mode) | ArgsInfos],
		FollowVarsMap0, NextNonReserved0,
		FollowVarsMap, NextNonReserved) :-
	code_util__arg_loc_to_register(Loc, Lval),
	( Mode = top_in ->
		( map__insert(FollowVarsMap0, ArgVar, Lval, FollowVarsMap1) ->
			FollowVarsMap2 = FollowVarsMap1
		;
			% The call is not in superhomogeneous form: this
			% argument has appeared before. Since the earlier
			% appearance will have given the variable a smaller
			% register number, we prefer that location to the one
			% we would give to this appearance of the variable.
			FollowVarsMap2 = FollowVarsMap0
		),
		( Lval = reg(r, RegNum) ->
			int__max(NextNonReserved0, RegNum + 1,
				NextNonReserved1)
		;
			error("arg_info puts arg in non-reg lval")
		)
	;
		FollowVarsMap2 = FollowVarsMap0,
		NextNonReserved1 = NextNonReserved0
	),
	find_follow_vars_from_arginfo(ArgsInfos,
		FollowVarsMap2, NextNonReserved1,
		FollowVarsMap, NextNonReserved).

%-----------------------------------------------------------------------------%

:- pred find_follow_vars_from_sequence(list(prog_var)::in, int::in,
	follow_vars_map::in, follow_vars_map::out, int::out) is det.

find_follow_vars_from_sequence([], NextRegNum, FollowVarsMap,
		FollowVarsMap, NextRegNum).
find_follow_vars_from_sequence([InVar | InVars], NextRegNum, FollowVarsMap0,
		FollowVarsMap, NextNonReserved) :-
	(
		map__insert(FollowVarsMap0, InVar, reg(r, NextRegNum),
			FollowVarsMap1)
	->
		FollowVarsMap2 = FollowVarsMap1
	;
		% The call is not in superhomogeneous form: this argument has
		% appeared before. Since the earlier appearance will have given
		% the variable a smaller register number, we prefer that
		% location to the one we would give to this appearance of the
		% variable.
		FollowVarsMap2 = FollowVarsMap0
	),
	find_follow_vars_from_sequence(InVars, NextRegNum + 1, FollowVarsMap2,
		FollowVarsMap, NextNonReserved).

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

:- pred find_follow_vars_in_disj(list(hlds_goal)::in, map(prog_var, type)::in,
	module_info::in, follow_vars_map::in, int::in,
	list(hlds_goal)::out, follow_vars_map::out, int::out) is det.

find_follow_vars_in_disj([], _, _ModuleInfo, FollowVarsMap, NextNonReserved,
		[], FollowVarsMap, NextNonReserved).
find_follow_vars_in_disj([Goal0 | Goals0], VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		[Goal | Goals], FollowVarsMap, NextNonReserved) :-
	find_follow_vars_in_goal(Goal0, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		Goal1, FollowVarsMap, NextNonReserved),
	FollowVars = follow_vars(FollowVarsMap, NextNonReserved),
	goal_set_follow_vars(Goal1, yes(FollowVars), Goal),
	find_follow_vars_in_disj(Goals0, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		Goals, _FollowVarsMap, _NextNonReserved).

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

:- pred find_follow_vars_in_cases(list(case)::in, map(prog_var, type)::in,
	module_info::in, follow_vars_map::in, int::in,
	list(case)::out, follow_vars_map::out, int::out) is det.

find_follow_vars_in_cases([], _, _ModuleInfo, FollowVarsMap, NextNonReserved,
		[], FollowVarsMap, NextNonReserved).
find_follow_vars_in_cases([case(Cons, Goal0) | Goals0], VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		[case(Cons, Goal) | Goals], FollowVarsMap, NextNonReserved) :-
	find_follow_vars_in_goal(Goal0, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved0,
		Goal1, FollowVarsMap, NextNonReserved),
	FollowVars = follow_vars(FollowVarsMap, NextNonReserved),
	goal_set_follow_vars(Goal1, yes(FollowVars), Goal),
	find_follow_vars_in_cases(Goals0, VarTypes, ModuleInfo,
		FollowVarsMap0, NextNonReserved,
		Goals, _FollowVarsMap, _NextNonReserved).

%-----------------------------------------------------------------------------%

	% We attach the follow_vars to each goal that follows a goal
	% that is not cachable by the code generator.

:- pred find_follow_vars_in_conj(list(hlds_goal)::in, map(prog_var, type)::in,
	module_info::in, bool::in, follow_vars_map::in, int::in,
	list(hlds_goal)::out, follow_vars_map::out, int::out) is det.

find_follow_vars_in_conj([], _, _ModuleInfo, _AttachToFirst,
		FollowVarsMap, NextNonReserved,
		[], FollowVarsMap, NextNonReserved).
find_follow_vars_in_conj([Goal0 | Goals0], VarTypes, ModuleInfo, AttachToFirst,
		FollowVarsMap0, NextNonReserved0,
		[Goal | Goals], FollowVarsMap, NextNonReserved) :-
	(
		Goal0 = GoalExpr0 - _,
		(
			GoalExpr0 = call(_, _, _, BuiltinState, _, _),
			BuiltinState = inline_builtin
		;
			GoalExpr0 = unify(_, _, _, Unification, _),
			Unification \= complicated_unify(_, _, _)
		)
	->
		AttachToNext = no
	;
		AttachToNext = yes
	),
	find_follow_vars_in_conj(Goals0, VarTypes, ModuleInfo, AttachToNext,
		FollowVarsMap0, NextNonReserved0,
		Goals, FollowVarsMap1, NextNonReserved1),
	find_follow_vars_in_goal(Goal0, VarTypes, ModuleInfo,
		FollowVarsMap1, NextNonReserved1,
		Goal1, FollowVarsMap, NextNonReserved),
	(
		AttachToFirst = yes,
		FollowVars = follow_vars(FollowVarsMap, NextNonReserved),
		goal_set_follow_vars(Goal1, yes(FollowVars), Goal)
	;
		AttachToFirst = no,
		Goal = Goal1
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
