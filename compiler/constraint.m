%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: constraint.m
% Main author: bromage.
%
% This module will eventually perform constraint propagation on
% an entire module.  At the moment, though, it just does propagation
% within a single goal.
%
% The constraint propagation transformation attempts to improve
% the efficiency of a generate-and-test style program by statically
% scheduling constraints as early as possible, where a "constraint"
% is any goal which has no output and can fail.
%
% XXX Code is broken.  Do not attempt to compile using the
%     --constraint-propagation option!
%-----------------------------------------------------------------------------%

:- module constraint.

:- interface.

:- import_module hlds_module.
:- import_module io.

:- pred constraint_propagation(module_info, module_info, io__state, io__state).
:- mode constraint_propagation(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_pred, hlds_goal, hlds_data.
:- import_module mode_util, passes_aux, code_aux, prog_data, instmap.
:- import_module delay_info, mode_info, inst_match, modes, mode_debug.
:- import_module transform, options, globals.
:- import_module mercury_to_mercury, hlds_out, dependency_graph.

:- import_module bool, list, map, set, std_util, assoc_list, string.
:- import_module varset, term, require.

:- type constraint == hlds_goal.

%-----------------------------------------------------------------------------%

constraint_propagation(ModuleInfo0, ModuleInfo) -->
	{ module_info_ensure_dependency_info(ModuleInfo0, ModuleInfo1) },
	{ module_info_dependency_info(ModuleInfo1, DepInfo) },
	{ hlds_dependency_info_get_dependency_ordering(DepInfo, DepOrd) },
	constraint_propagation2(DepOrd, ModuleInfo1, ModuleInfo).

:- pred constraint_propagation2(dependency_ordering, module_info, module_info,
				io__state, io__state).
:- mode constraint_propagation2(in, in, out, di, uo) is det.
constraint_propagation2([], ModuleInfo, ModuleInfo) --> [].
constraint_propagation2([C | Cs], ModuleInfo0, ModuleInfo) -->
	constraint_propagation3(C, ModuleInfo0, ModuleInfo1),
	constraint_propagation2(Cs, ModuleInfo1, ModuleInfo).

:- pred constraint_propagation3(list(pred_proc_id), module_info, module_info,
				io__state, io__state).
:- mode constraint_propagation3(in, in, out, di, uo) is det.
constraint_propagation3([], ModuleInfo, ModuleInfo) --> [].
constraint_propagation3([proc(Pred, Proc) | Rest], ModuleInfo0, ModuleInfo) -->
	constraint__propagate_in_proc(Pred, Proc, ModuleInfo0, ModuleInfo1),
	modecheck_proc(Proc, Pred, ModuleInfo1, ModuleInfo2, Errs),
	( { Errs \= 0 } ->
	    { error("constraint_propagation3") }
	;
	    []
	),
	constraint_propagation3(Rest, ModuleInfo2, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred constraint__propagate_in_proc(pred_id, proc_id, module_info,
				module_info, io__state, io__state).
:- mode constraint__propagate_in_proc(in, in, in, out, di, uo) is det.
constraint__propagate_in_proc(PredId, ProcId, ModuleInfo0, ModuleInfo,
				IoState0, IoState) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	proc_info_goal(ProcInfo0, Goal0),
	proc_info_variables(ProcInfo0, VarSet0),
	varset__vars(VarSet0, VarList),
	set__list_to_set(VarList, VarSet1),

	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
	proc_info_context(ProcInfo0, Context),
	mode_info_init(IoState0, ModuleInfo0, PredId, ProcId,
			Context, VarSet1, InstMap0, ModeInfo0),

	constraint__propagate_goal(Goal0, Goal, ModeInfo0, ModeInfo),

	mode_info_get_io_state(ModeInfo, IoState),
	mode_info_get_varset(ModeInfo, VarSet),
	mode_info_get_var_types(ModeInfo, VarTypes),
	mode_info_get_module_info(ModeInfo, ModuleInfo1),

	proc_info_set_variables(ProcInfo0, VarSet, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2),
	proc_info_set_goal(ProcInfo2, Goal, ProcInfo),

	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo1, PredTable, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred constraint__propagate_goal(hlds_goal, hlds_goal,
					mode_info, mode_info).
:- mode constraint__propagate_goal(in, out,
					mode_info_di, mode_info_uo) is det.

constraint__propagate_goal(Goal0 - GoalInfo, Goal - GoalInfo) -->
	mode_info_dcg_get_instmap(InstMap0),
	{ goal_info_get_instmap_delta(GoalInfo, DeltaInstMap) },
	{ instmap__apply_instmap_delta(InstMap0, DeltaInstMap, InstMap) },
	mode_info_set_instmap(InstMap),
	constraint__propagate_goal_2(Goal0, Goal),
	mode_info_set_instmap(InstMap).

%-----------------------------------------------------------------------------%

:- pred constraint__propagate_goal_2(hlds_goal_expr, hlds_goal_expr,
				mode_info, mode_info).
:- mode constraint__propagate_goal_2(in, out,
					mode_info_di, mode_info_uo) is det.

constraint__propagate_goal_2(conj(Goals0), conj(Goals)) -->
	mode_checkpoint(enter, "conj"),
	constraint__propagate_conj(Goals0, Goals),
	mode_checkpoint(exit, "conj").

constraint__propagate_goal_2(disj(Goals0, SM), disj(Goals, SM)) -->
	mode_checkpoint(enter, "disj"),
	constraint__propagate_disj(Goals0, Goals),
	mode_checkpoint(exit, "disj").

constraint__propagate_goal_2(switch(Var, Det, Cases0, SM),
				switch(Var, Det, Cases, SM)) -->
	mode_checkpoint(enter, "switch"),
	constraint__propagate_cases(Cases0, Cases),
	mode_checkpoint(exit, "switch").

constraint__propagate_goal_2(if_then_else(Vars, Cond0, Then0, Else0, SM),
			if_then_else(Vars, Cond, Then, Else, SM)) -->
	mode_checkpoint(enter, "if_then_else"),
	mode_info_dcg_get_instmap(InstMap0),
	constraint__propagate_goal(Cond0, Cond),
%	mode_info_dcg_get_instmap(InstMap1),
	constraint__propagate_goal(Then0, Then),
	mode_info_set_instmap(InstMap0),
	constraint__propagate_goal(Else0, Else),
	mode_checkpoint(exit, "if_then_else").

constraint__propagate_goal_2(not(Goal0), not(Goal)) -->
	mode_checkpoint(enter, "not"),
	constraint__propagate_goal(Goal0, Goal),
	mode_checkpoint(exit, "not").

constraint__propagate_goal_2(some(Vars, Goal0), some(Vars, Goal)) -->
	mode_checkpoint(enter, "some"),
	constraint__propagate_goal(Goal0, Goal),
	mode_checkpoint(exit, "some").

constraint__propagate_goal_2(
		higher_order_call(A, B, C, D, E),
		higher_order_call(A, B, C, D, E)) -->
	mode_checkpoint(enter, "higher-order call"),
	mode_checkpoint(exit, "higher-order call").

constraint__propagate_goal_2(
		call(PredId, ProcId, ArgVars, Builtin, Sym, Context),
		call(PredId, ProcId, ArgVars, Builtin, Sym, Context)) -->
	mode_checkpoint(enter, "call"),
	mode_checkpoint(exit, "call").

constraint__propagate_goal_2(unify(A,B,C,D,E), unify(A,B,C,D,E)) -->
	mode_checkpoint(enter, "unify"),
	mode_checkpoint(exit, "unify").

constraint__propagate_goal_2(
		pragma_c_code(A, B, C, D, E, F, G, H), 
		pragma_c_code(A, B, C, D, E, F, G, H)) -->
	mode_checkpoint(enter, "pragma_c_code"),
	mode_checkpoint(exit, "pragma_c_code").

%-----------------------------------------------------------------------------%

:- pred constraint__propagate_disj(list(hlds_goal), list(hlds_goal),
				mode_info, mode_info).
:- mode constraint__propagate_disj(in, out,
				mode_info_di, mode_info_uo) is det.

constraint__propagate_disj([], []) --> [].
constraint__propagate_disj([Goal0|Goals0], [Goal|Goals]) -->
	mode_info_dcg_get_instmap(InstMap0),
	constraint__propagate_goal(Goal0, Goal),
	mode_info_set_instmap(InstMap0),
	constraint__propagate_disj(Goals0, Goals).

%-----------------------------------------------------------------------------%

:- pred constraint__propagate_cases(list(case), list(case),
				mode_info, mode_info).
:- mode constraint__propagate_cases(in, out,
				mode_info_di, mode_info_uo) is det.

constraint__propagate_cases([], []) --> [].
constraint__propagate_cases([case(Cons, Goal0)|Goals0],
			[case(Cons, Goal)|Goals]) -->
	mode_info_dcg_get_instmap(InstMap0),
	constraint__propagate_goal(Goal0, Goal),
	mode_info_set_instmap(InstMap0),
	constraint__propagate_cases(Goals0, Goals).

%-----------------------------------------------------------------------------%

	% constraint__propagate_conj detects the constraints in
	% a conjunction and moves them to as early as possible
	% in the list.

:- pred constraint__propagate_conj(list(hlds_goal), list(hlds_goal),
				mode_info, mode_info).
:- mode constraint__propagate_conj(in, out,
				mode_info_di, mode_info_uo) is det.

constraint__propagate_conj(Goals0, Goals) -->
	=(ModeInfo0),
	{ mode_info_get_delay_info(ModeInfo0, DelayInfo0) },
	{ delay_info__enter_conj(DelayInfo0, DelayInfo1) },
	mode_info_set_delay_info(DelayInfo1),
%	mode_info_add_goals_live_vars(Goals0),

	mode_info_dcg_get_instmap(InstMap0),
	constraint__find_constraints(Goals0, Goals1, Constraints1),
	mode_info_set_instmap(InstMap0),
%	constraint__distribute_constraints(Constraints1, Goals1, Goals),
	{ list__append(Constraints1, Goals1, Goals2) },
	transform__reschedule_conj(Goals2, Goals),

	=(ModeInfo1),
	{ mode_info_get_delay_info(ModeInfo1, DelayInfo2) },
	{ delay_info__leave_conj(DelayInfo2, DelayedGoals, DelayInfo3) },
	mode_info_set_delay_info(DelayInfo3),

	( { DelayedGoals = [] } ->
	    []
	;
	    { error("constraint__propagate_conj") }
	).

:- pred constraint__find_constraints(list(hlds_goal), list(hlds_goal),
				list(constraint), mode_info, mode_info).
:- mode constraint__find_constraints(in, out, out,
				mode_info_di, mode_info_uo) is det.

constraint__find_constraints([], [], []) --> [].
constraint__find_constraints([Goal0 | Goals0], Goals, Constraints) -->
	mode_info_dcg_get_instmap(InstMap0),
	constraint__propagate_goal(Goal0, Goal1),
%	mode_info_dcg_get_instmap(InstMap1),
	{ Goal1 = Goal1Goal - Goal1Info },
	( { Goal1Goal = conj(Goal1List) } ->
	    { list__append(Goal1List, Goals0, Goals1) },
	    mode_info_set_instmap(InstMap0),
	    constraint__find_constraints(Goals1, Goals, Constraints)
	;
	    constraint__find_constraints(Goals0, Goals1, Constraints0),
	    =(ModeInfo),
	    ( { constraint__is_constraint(Goal1Info, ModeInfo) } ->
		{ Constraints = [Goal1 | Constraints0] },
		{ Goals = Goals1 }
	    ;
		{ Constraints = Constraints0 },
		{ Goals = [Goal1 | Goals1] }
	    )
	).

%:- pred constraint__distribute_constraints(list(constraint), list(hlds_goal),
%			list(hlds_goal), mode_info, mode_info).
%:- mode constraint__distribute_constraints(in, in, out,
%			mode_info_di, mode_info_uo) is det.

%-----------------------------------------------------------------------------%

:- pred constraint__is_constraint(hlds_goal_info, mode_info).
:- mode constraint__is_constraint(in, mode_info_ui) is semidet.

constraint__is_constraint(GoalInfo, ModeInfo) :-
	goal_info_get_determinism(GoalInfo, Det),
	constraint__determinism(Det),
	constraint__no_output_vars(GoalInfo, ModeInfo).

:- pred constraint__no_output_vars(hlds_goal_info, mode_info).
:- mode constraint__no_output_vars(in, mode_info_ui) is semidet.

constraint__no_output_vars(GoalInfo, ModeInfo) :-
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	goal_info_get_nonlocals(GoalInfo, Vars),
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_instmap(ModeInfo, InstMap),
	instmap__no_output_vars(InstMap, InstMapDelta, Vars, ModuleInfo).

	% constraint__determinism(Det) is true iff Det is
	% a possible determinism of a constraint.  The
	% determinisms which use a model_semi code model
	% are obviously constraints.  Should erroneous
	% also be treated as a constraint?
:- pred constraint__determinism(determinism).
:- mode constraint__determinism(in) is semidet.
constraint__determinism(semidet).
constraint__determinism(failure).
% constraint__determinism(erroneous).	% maybe

%-----------------------------------------------------------------------------%

:- pred mode_info_write_string(string, mode_info, mode_info).
:- mode mode_info_write_string(in, mode_info_di, mode_info_uo) is det.

mode_info_write_string(Msg, ModeInfo0, ModeInfo) :-
	mode_info_get_io_state(ModeInfo0, IOState0),
	io__write_string(Msg, IOState0, IOState),
	mode_info_set_io_state(ModeInfo0, IOState, ModeInfo).

:- pred mode_info_write_goal(hlds_goal, int, mode_info, mode_info).
:- mode mode_info_write_goal(in, in, mode_info_di, mode_info_uo) is det.

mode_info_write_goal(Goal, Indent, ModeInfo0, ModeInfo) :-
	mode_info_get_io_state(ModeInfo0, IOState0),
%       globals__io_lookup_bool_option(debug_modes, DoCheckPoint,
%		IOState0, IOState1),
	IOState0 = IOState1,
	( semidet_succeed ->
		mode_info_get_module_info(ModeInfo0, ModuleInfo),
		mode_info_get_varset(ModeInfo0, VarSet),
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, no, Indent, "",
				IOState1, IOState)
	;
		IOState = IOState1
	),
	mode_info_set_io_state(ModeInfo0, IOState, ModeInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
