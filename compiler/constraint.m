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
% within a single goal.  The code is 
%-----------------------------------------------------------------------------%

:- module constraint.

:- interface.

:- import_module hlds_module, llds.
:- import_module io.

:- pred constraint_propagation(module_info, module_info, io__state, io__state).
:- mode constraint_propagation(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_pred, hlds_goal, hlds_data.
:- import_module mode_util, passes_aux, code_aux, prog_data.
:- import_module delay_info, mode_info, inst_match, modes.
:- import_module transform, options, globals.
:- import_module mercury_to_mercury, hlds_out, dependency_graph.

:- import_module bool, list, map, set, std_util, assoc_list, string.
:- import_module varset, term, require.

:- type constraint == hlds__goal.

%-----------------------------------------------------------------------------%

constraint_propagation(ModuleInfo0, ModuleInfo) -->
	{ module_info_ensure_dependency_info(ModuleInfo0, ModuleInfo1) },
	{ module_info_dependency_info(ModuleInfo1, DepInfo) },
	{ hlds__dependency_info_get_dependency_ordering(DepInfo, DepOrd) },
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
	constraint__transform_proc_1(Pred, Proc, ModuleInfo0, ModuleInfo1),
	modecheck_proc(Proc, Pred, ModuleInfo1, ModuleInfo2, Errs),
	( { Errs \= 0 } ->
	    { error("constraint_propagation3") }
	;
	    []
	),
	constraint_propagation3(Rest, ModuleInfo2, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred constraint__transform_proc_1(pred_id, proc_id, module_info, module_info,
				io__state, io__state).
:- mode constraint__transform_proc_1(in, in, in, out, di, uo) is det.
constraint__transform_proc_1(PredId, ProcId, ModuleInfo0, ModuleInfo,
				IoState0, IoState) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	pred_info_name(PredInfo0, Name),
	proc_info_declared_determinism(ProcInfo0, Det),
	proc_info_argmodes(ProcInfo0, Modes),
	varset__init(ModeVarSet),

	io__write_string("\nProcessing: ", IoState0, IoState1),
	mercury_output_pred_mode_subdecl(ModeVarSet, unqualified(Name),
				Modes, Det, Context, IoState1, IoState2),
	io__write_string("\n", IoState2, IoState3),

	IoState3 = IoState99,

	proc_info_goal(ProcInfo0, Goal0),
	proc_info_variables(ProcInfo0, VarSet0),
	varset__vars(VarSet0, VarList),
	set__list_to_set(VarList, VarSet1),

	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
	proc_info_context(ProcInfo0, Context),
	mode_info_init(IoState99, ModuleInfo0, no, PredId, ProcId,
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

:- pred constraint__propagate_goal(hlds__goal, hlds__goal,
					mode_info, mode_info).
:- mode constraint__propagate_goal(in, out,
					mode_info_di, mode_info_uo) is det.

constraint__propagate_goal(Goal0 - GoalInfo, Goal - GoalInfo) -->
	mode_info_dcg_get_instmap(InstMap0),
	{ goal_info_get_instmap_delta(GoalInfo, DeltaInstMap) },
	{ apply_instmap_delta(InstMap0, DeltaInstMap, InstMap) },
	mode_info_set_instmap(InstMap),
	constraint__propagate_goal_2(Goal0, Goal),
	mode_info_set_instmap(InstMap).

%-----------------------------------------------------------------------------%

:- pred constraint__propagate_goal_2(hlds__goal_expr, hlds__goal_expr,
				mode_info, mode_info).
:- mode constraint__propagate_goal_2(in, out,
					mode_info_di, mode_info_uo) is det.

constraint__propagate_goal_2(conj(Goals0), conj(Goals)) -->
	constraint__checkpoint(enter, "conj"),
	constraint__propagate_conj(Goals0, Goals),
	constraint__checkpoint(exit, "conj").

constraint__propagate_goal_2(disj(Goals0, FV), disj(Goals, FV)) -->
	constraint__checkpoint(enter, "disj"),
	constraint__propagate_disj(Goals0, Goals),
	constraint__checkpoint(exit, "disj").

constraint__propagate_goal_2(switch(Var, Det, Cases0, FV),
				switch(Var, Det, Cases, FV)) -->
	constraint__checkpoint(enter, "switch"),
	constraint__propagate_cases(Cases0, Cases),
	constraint__checkpoint(exit, "switch").

constraint__propagate_goal_2(if_then_else(Vars, Cond0, Then0, Else0, FV),
			if_then_else(Vars, Cond, Then, Else, FV)) -->
	constraint__checkpoint(enter, "if_then_else"),
	mode_info_dcg_get_instmap(InstMap0),
	constraint__propagate_goal(Cond0, Cond),
%	mode_info_dcg_get_instmap(InstMap1),
	constraint__propagate_goal(Then0, Then),
	mode_info_set_instmap(InstMap0),
	constraint__propagate_goal(Else0, Else),
	constraint__checkpoint(exit, "if_then_else").

constraint__propagate_goal_2(not(Goal0), not(Goal)) -->
	constraint__checkpoint(enter, "not"),
	constraint__propagate_goal(Goal0, Goal),
	constraint__checkpoint(exit, "not").

constraint__propagate_goal_2(some(Vars, Goal0), some(Vars, Goal)) -->
	constraint__checkpoint(enter, "some"),
	constraint__propagate_goal(Goal0, Goal),
	constraint__checkpoint(exit, "some").

constraint__propagate_goal_2(
		higher_order_call(A, B, C, D, E, F),
		higher_order_call(A, B, C, D, E, F)) -->
	constraint__checkpoint(enter, "higher-order call"),
	constraint__checkpoint(exit, "higher-order call").

constraint__propagate_goal_2(
		call(PredId, ProcId, ArgVars, Builtin, Sym, Context, Follow),
		call(PredId, ProcId, ArgVars, Builtin, Sym, Context, Follow)) -->
	constraint__checkpoint(enter, "call"),
	constraint__checkpoint(exit, "call").

constraint__propagate_goal_2(unify(A,B,C,D,E), unify(A,B,C,D,E)) -->
	constraint__checkpoint(enter, "unify"),
	constraint__checkpoint(exit, "unify").


constraint__propagate_goal_2(
		pragma_c_code(A, B, C, D, E, F), 
		pragma_c_code(A, B, C, D, E, F)) -->
	constraint__checkpoint(enter, "pragma_c_code"),
	constraint__checkpoint(exit, "pragma_c_code").

%-----------------------------------------------------------------------------%

:- pred constraint__propagate_disj(list(hlds__goal), list(hlds__goal),
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

:- pred constraint__propagate_conj(list(hlds__goal), list(hlds__goal),
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

:- pred constraint__find_constraints(list(hlds__goal), list(hlds__goal),
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
		mode_info_write_string("Found constraint:\n"),
		mode_info_write_goal(Goal1, 1),
		mode_info_write_string("\n"),
		{ Constraints = [Goal1 | Constraints0] },
		{ Goals = Goals1 }
	    ;
		{ Constraints = Constraints0 },
		{ Goals = [Goal1 | Goals1] }
	    )
	).

%:- pred constraint__distribute_constraints(list(constraint), list(hlds__goal),
%			list(hlds__goal), mode_info, mode_info).
%:- mode constraint__distribute_constraints(in, in, out,
%			mode_info_di, mode_info_uo) is det.

%-----------------------------------------------------------------------------%

:- pred constraint__is_constraint(hlds__goal_info, mode_info).
:- mode constraint__is_constraint(in, mode_info_ui) is semidet.

constraint__is_constraint(GoalInfo, ModeInfo) :-
	goal_info_get_determinism(GoalInfo, Det),
	constraint__determinism(Det),
	constraint__no_output_vars(GoalInfo, ModeInfo).

:- pred constraint__no_output_vars(hlds__goal_info, mode_info).
:- mode constraint__no_output_vars(in, mode_info_ui) is semidet.

constraint__no_output_vars(GoalInfo, ModeInfo) :-
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	(
	    InstMapDelta = unreachable
	;
	    InstMapDelta = reachable(InstMapDelta1),
	    mode_info_get_varset(ModeInfo, VarSet),
	    varset__vars(VarSet, VarList),
	    mode_info_get_module_info(ModeInfo, ModuleInfo),
	    mode_info_get_instmap(ModeInfo, InstMap),
	    constraint__no_output_vars_2(VarList, InstMap, InstMapDelta1, 
				ModuleInfo)
	).

:- pred constraint__no_output_vars_2(list(var), instmap, instmapping, 
				module_info).
:- mode constraint__no_output_vars_2(in, in, in, in) is semidet.

constraint__no_output_vars_2([], _, _, _).
constraint__no_output_vars_2([Var | Vars], InstMap0, InstMapDelta, 
				ModuleInfo) :-
	( map__search(InstMapDelta, Var, Inst) ->
	    instmap_lookup_var(InstMap0, Var, Inst0),
	    inst_matches_binding(Inst, Inst0, ModuleInfo)
	;
	    true
	),
	constraint__no_output_vars_2(Vars, InstMap0, InstMapDelta, ModuleInfo).

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

:- type my_port
	--->	enter
	;	exit
	;	wakeup.

:- pred constraint__checkpoint(my_port, string, mode_info, mode_info).
:- mode constraint__checkpoint(in, in, mode_info_di, mode_info_uo) is det.

constraint__checkpoint(Port, Msg, ModeInfo0, ModeInfo) :-
	mode_info_get_io_state(ModeInfo0, IOState0),
%       globals__io_lookup_bool_option(debug_modes, DoCheckPoint,
%		IOState0, IOState1),
	IOState0 = IOState1,
	( semidet_succeed ->
		constraint__checkpoint_2(Port, Msg, ModeInfo0, IOState1, IOState)
	;
		IOState = IOState1
	),
	mode_info_set_io_state(ModeInfo0, IOState, ModeInfo).

:- pred is_bool(bool::in) is det.
is_bool(_).

:- pred constraint__checkpoint_2(my_port, string, mode_info, io__state, io__state).
:- mode constraint__checkpoint_2(in, in, mode_info_ui, di, uo) is det.

constraint__checkpoint_2(Port, Msg, ModeInfo) -->
	{ mode_info_get_errors(ModeInfo, Errors) },
	{ is_bool(Detail) },	% explicit type qualification needed to
				% resolve type ambiguity
	( { Port = enter } ->
		io__write_string("Enter "),
		{ Detail = yes }
	; { Port = wakeup } ->
		io__write_string("Wake  "),
		{ Detail = no }
	; { Errors = [] } ->
		io__write_string("Exit "),
		{ Detail = yes }
	;
		io__write_string("Delay  "),
		{ Detail = no }
	),
	io__write_string(Msg),
	( { Detail = yes } ->
		io__write_string(":\n"),
		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics),
		{ mode_info_get_instmap(ModeInfo, InstMap) },
		( { InstMap = reachable(InstMapping) } ->
			{ map__to_assoc_list(InstMapping, AssocList) },
			{ mode_info_get_varset(ModeInfo, VarSet) },
			{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
			constraint__write_var_insts(AssocList, VarSet, InstVarSet)
		;
			io__write_string("\tUnreachable\n")
		)
	;
		[]
	),
	io__write_string("\n").

:- pred constraint__write_var_insts(assoc_list(var, inst), varset, varset,
			io__state, io__state).
:- mode constraint__write_var_insts(in, in, in, di, uo) is det.

constraint__write_var_insts([], _, _) --> [].
constraint__write_var_insts([Var - Inst | VarInsts], VarSet, InstVarSet) -->
	io__write_string("\t"),
	mercury_output_var(Var, VarSet),
	io__write_string(" :: "),
	mercury_output_inst(Inst, InstVarSet),
	( { VarInsts = [] } ->
		[]
	;
		io__write_string("\n"),
		constraint__write_var_insts(VarInsts, VarSet, InstVarSet)
	).

%-----------------------------------------------------------------------------%

:- pred mode_info_write_string(string, mode_info, mode_info).
:- mode mode_info_write_string(in, mode_info_di, mode_info_uo) is det.

mode_info_write_string(Msg, ModeInfo0, ModeInfo) :-
	mode_info_get_io_state(ModeInfo0, IOState0),
	io__write_string(Msg, IOState0, IOState),
	mode_info_set_io_state(ModeInfo0, IOState, ModeInfo).

:- pred mode_info_write_goal(hlds__goal, int, mode_info, mode_info).
:- mode mode_info_write_goal(in, in, mode_info_di, mode_info_uo) is det.

mode_info_write_goal(Goal, Indent, ModeInfo0, ModeInfo) :-
	mode_info_get_io_state(ModeInfo0, IOState0),
%       globals__io_lookup_bool_option(debug_modes, DoCheckPoint,
%		IOState0, IOState1),
	IOState0 = IOState1,
	( semidet_succeed ->
		mode_info_get_module_info(ModeInfo0, ModuleInfo),
		mode_info_get_varset(ModeInfo0, VarSet),
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent,
				IOState1, IOState)
	;
		IOState = IOState1
	),
	mode_info_set_io_state(ModeInfo0, IOState, ModeInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
