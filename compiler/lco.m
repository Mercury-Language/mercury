%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main authors: zs, dmo

% This module looks for opportunities to apply the "last call modulo
% constructor application" optimization.

%-----------------------------------------------------------------------------%

:- module lco.

:- interface.

:- import_module hlds_module, hlds_pred.
:- import_module io.

:- pred lco_modulo_constructors(pred_id, proc_id, proc_info, proc_info,
	module_info, module_info, io__state, io__state).
:- mode lco_modulo_constructors(in, in, in, out, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, passes_aux, hlds_out, (inst), instmap, inst_match.
:- import_module mode_util, hlds_data, prog_data, type_util, globals, options.
:- import_module arg_info, inst_table.
:- import_module list, std_util, map, assoc_list, require.
:- import_module bool, set, int, varset.

%-----------------------------------------------------------------------------%

lco_modulo_constructors(PredId, ProcId, ProcInfo0, ProcInfo, ModuleInfo0,
		ModuleInfo) -->
	write_proc_progress_message("% Trying to introduce LCO in ",
		PredId, ProcId, ModuleInfo0),
	{ proc_info_goal(ProcInfo0, Goal0) },
	{ proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0) },
	{ lco_in_goal(proc(PredId, ProcId), Goal0, Goal, ModuleInfo0,
		ModuleInfo1, InstMap0, ProcInfo0, ProcInfo1, Changed) },
	( { Changed = yes } ->
		{ proc_info_set_goal(ProcInfo1, Goal, ProcInfo) },
		{ ModuleInfo = ModuleInfo1 },
		write_proc_progress_message("% Can introduce LCO in ",
			PredId, ProcId, ModuleInfo)
	;
		{ ProcInfo = ProcInfo0 },
		{ ModuleInfo = ModuleInfo0 }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% Do the LCO optimisation and recompute the instmap deltas.
:- pred lco_in_goal(pred_proc_id, hlds_goal, hlds_goal, module_info,
		module_info, instmap, proc_info, proc_info, bool).
:- mode lco_in_goal(in, in, out, in, out, in, in, out, out) is det.

lco_in_goal(PredProcId, Goal0, Goal, Module0, Module, InstMap0,
		ProcInfo0, ProcInfo, Changed):-
	lco_in_sub_goal(PredProcId, Goal0, Goal1, Module0, Module1, InstMap0,
		ProcInfo0, ProcInfo1, Changed),
	(
		Changed = yes,
		proc_info_inst_table(ProcInfo1, InstTable0),
		proc_info_get_initial_instmap(ProcInfo1, Module1, InstMap),
		proc_info_vartypes(ProcInfo1, VarTypes),
		proc_info_headvars(ProcInfo1, ArgVars),
		proc_info_arglives(ProcInfo1, Module1, ArgLives),
		recompute_instmap_delta(ArgVars, ArgLives, VarTypes,
			Goal1, Goal, InstMap, InstTable0, InstTable,
			_GoalChanged, Module1, Module),
		proc_info_set_inst_table(ProcInfo1, InstTable, ProcInfo)
	;
		Changed = no,
		Goal = Goal0,
		Module = Module0,
		ProcInfo = ProcInfo0
	).

% Do the LCO optimisation without recomputing instmap deltas.
:- pred lco_in_sub_goal(pred_proc_id, hlds_goal, hlds_goal, module_info,
		module_info, instmap, proc_info, proc_info, bool).
:- mode lco_in_sub_goal(in, in, out, in, out, in, in, out, out) is det.

lco_in_sub_goal(PredProcId, Goal0 - GoalInfo, Goal - GoalInfo, Module0, Module,
		InstMap0, Proc0, Proc, Changed) :-
	lco_in_goal_2(PredProcId, Goal0, Goal, Module0, Module, InstMap0,
		Proc0, Proc, Changed).

%-----------------------------------------------------------------------------%

:- pred lco_in_goal_2(pred_proc_id, hlds_goal_expr, hlds_goal_expr,
		module_info, module_info, instmap, proc_info, proc_info, bool).
:- mode lco_in_goal_2(in, in, out, in, out, in, in, out, out) is det.

lco_in_goal_2(PredProcId, conj(Goals0), conj(Goals), Module0, Module, InstMap0,
		Proc0, Proc, Changed) :-
	list__reverse(Goals0, RevGoals0),
	lco_in_conj(PredProcId, RevGoals0, [], Goals, Module0, Module, InstMap0,
		Proc0, Proc, Changed).

	% XXX Some execution algorithm issues here.
lco_in_goal_2(_, par_conj(Goals, SM), par_conj(Goals, SM), Module, Module,
		_, Proc, Proc, no).

lco_in_goal_2(PredProcId, disj(Goals0, SM), disj(Goals, SM), Module0, Module,
		InstMap0, Proc0, Proc, Changed) :-
	lco_in_disj(PredProcId, Goals0, Goals, Module0, Module, InstMap0,
		Proc0, Proc, Changed).

lco_in_goal_2(PredProcId, switch(Var, Det, Cases0, SM),
		switch(Var, Det, Cases, SM),
		Module0, Module, InstMap0, Proc0, Proc, Changed) :-
	lco_in_cases(PredProcId, Cases0, Cases, Module0, Module, InstMap0,
		Proc0, Proc, Changed).

lco_in_goal_2(PredProcId, if_then_else(Vars, Cond, Then0, Else0, SM),
		if_then_else(Vars, Cond, Then, Else, SM), Module0, Module,
		InstMap0, Proc0, Proc, Changed) :-
	Cond = _ - CondInfo,
	goal_info_get_instmap_delta(CondInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap1),
	lco_in_sub_goal(PredProcId, Then0, Then, Module0, Module1, InstMap1,
			Proc0, Proc1, Changed0),
	lco_in_sub_goal(PredProcId, Else0, Else, Module1, Module, InstMap0,
			Proc1, Proc, Changed1),
	bool__or(Changed0, Changed1, Changed).

lco_in_goal_2(PredProcId, some(Vars, CanRemove, Goal0),
		some(Vars, CanRemove, Goal), Module0, Module,
		InstMap0, Proc0, Proc, Changed) :-
	lco_in_sub_goal(PredProcId, Goal0, Goal, Module0, Module, InstMap0,
		Proc0, Proc, Changed).

lco_in_goal_2(_, not(Goal), not(Goal), Module, Module, _, Proc, Proc, no).

lco_in_goal_2(_, call(A,B,C,D,E,F), call(A,B,C,D,E,F), Module, Module,
		_, Proc, Proc, no).

lco_in_goal_2(_, generic_call(A,B,C,D), generic_call(A,B,C,D),
		Module, Module, _, Proc, Proc, no).

lco_in_goal_2(_, unify(A,B,C,D,E), unify(A,B,C,D,E), Module, Module,
		_, Proc, Proc, no).

lco_in_goal_2(_, pragma_c_code(A,B,C,D,E,F,G), pragma_c_code(A,B,C,D,E,F,G), 
		Module, Module, _, Proc, Proc, no).

%-----------------------------------------------------------------------------%

:- pred lco_in_disj(pred_proc_id, list(hlds_goal), list(hlds_goal),
		module_info, module_info, instmap, proc_info, proc_info, bool).
:- mode lco_in_disj(in, in, out, in, out, in, in, out, out) is det.

lco_in_disj(_, [], [], Module, Module, _, Proc, Proc, no).
lco_in_disj(PredProcId, [Goal0 | Goals0], [Goal | Goals], Module0, Module,
		InstMap0, Proc0, Proc, Changed) :-
	lco_in_sub_goal(PredProcId, Goal0, Goal, Module0, Module1, InstMap0,
			Proc0, Proc1, Changed0),
	lco_in_disj(PredProcId, Goals0, Goals, Module1, Module, InstMap0,
			Proc1, Proc, Changed1),
	bool__or(Changed0, Changed1, Changed).

%-----------------------------------------------------------------------------%

:- pred lco_in_cases(pred_proc_id, list(case), list(case), module_info,
		module_info, instmap, proc_info, proc_info, bool).
:- mode lco_in_cases(in, in, out, in, out, in, in, out, out) is det.

lco_in_cases(_, [], [], Module, Module, _, Proc, Proc, no).
lco_in_cases(PredProcId, [case(Cons, IMD, Goal0) | Cases0],
		[case(Cons, IMD, Goal) | Cases],
		Module0, Module, InstMap0, Proc0, Proc, Changed) :-
	instmap__apply_instmap_delta(InstMap0, IMD, InstMap1),
	lco_in_sub_goal(PredProcId, Goal0, Goal, Module0, Module1, InstMap1,
			Proc0, Proc1, Changed0),
	lco_in_cases(PredProcId, Cases0, Cases, Module1, Module, InstMap0,
			Proc1, Proc, Changed1),
	bool__or(Changed0, Changed1, Changed).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% lco_in_conj(RevGoals, Unifies, Goals, Module0, Module, InstMap0,
%			Proc0, Proc, Changed)
%
% Given a conjunction whose structure is: "goals*,call,construct*",
% move the construction unifications before the call.
%
% We traverse the conjunction backwards (the caller has reversed the list).
% RevGoals is the list of remaining goals in the reversed conjunction list.
% RevUnifies is the list of assignments and constructions delayed by any
% previous recursive invocations of lco_in_conj. 
%
% invariant: append(reverse(RevGoals), Unifies) = original conjunction

:- pred lco_in_conj(pred_proc_id, list(hlds_goal), list(hlds_goal),
	list(hlds_goal), module_info, module_info, instmap, proc_info,
	proc_info, bool).
:- mode lco_in_conj(in, in, in, out, in, out, in, in, out, out) is det.

lco_in_conj(_, [], Unifies, Unifies, Module, Module, _, Proc, Proc, no).
lco_in_conj(PredProcId, [Goal0 | Goals0], Unifies0, Goals, Module0, Module,
		InstMap0, Proc0, Proc, Changed) :-
	Goal0 = GoalExpr0 - GoalInfo0,
	goal_info_get_instmap_delta(GoalInfo0, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap1),
	(
		GoalExpr0 = unify(_, _, _, Unif, _),
		Unif = construct(_, ConsId, _, _, _, _, _),

		% XXX For now, don't allow LCO on constructions of
		% higher-order terms.  This is because we currently
		% can't express non-ground higher-order terms.
		ConsId \= pred_const(_, _, _)
	->
		Unifies1 = [Goal0 | Unifies0],
		lco_in_conj(PredProcId, Goals0, Unifies1, Goals,
			Module0, Module, InstMap1, Proc0, Proc, Changed)
	;
		GoalExpr0 = call(CalledPredId, ProcId, Vars, _, _, _),

		% Make sure there were actually some constructions of tagged 
		% types after the call.  Otherwise there's no point in doing the
		% optimisation.
		list__filter(goal_is_no_tag_construction(Module0, Proc0),
			Unifies0, NoTagUnifies, Unifies1),
		Unifies1 \= [],

		% AAA for now, don't allow any constructions of no_tag types.
		NoTagUnifies = [],

		% XXX - For now, only allow calls to preds within this module.
		% This is because a new proc will need to be created for the
		% pred that is called.
		module_info_pred_info(Module0, CalledPredId, PredInfo),
		pred_info_import_status(PredInfo, ImportStatus),
		ImportStatus \= imported,

		% XXX Instead of disallowing opt_imported predicates, it
		% would be possible to make a local copy of the
		% predicate and call that.  I'm not sure if this is
		% worth doing, though.
		ImportStatus \= opt_imported,

		% XXX - Also, we currently only allow one reference per
		% variable, so make sure there is no more than one reference
		% to each output variable in the call.  This restriction can
		% be lifted once free(alias_many) is implemented.
		pred_info_procedures(PredInfo, ProcTable),
		map__lookup(ProcTable, ProcId, CalledProcInfo),
		check_only_one_ref_per_var(Unifies1, Vars, Module0,
			CalledProcInfo, Proc0),

		% The conservative GC version of solutions does not deep
		% copy the solutions, so we need to disallow LCO if both the
		% calling proc and called proc are multi-solution.
		\+ (
			module_info_globals(Module0, Globals),
			globals__get_gc_method(Globals, conservative),
			proc_info_interface_determinism(Proc0, CallingDet),
			proc_info_interface_determinism(CalledProcInfo,
				CalledDet),
			determinism_components(CallingDet, _, at_most_many),
			determinism_components(CalledDet,  _, at_most_many)
		),

		set__init(ChangedVarsSet0),
		Proc1 = Proc0,
		Goal1 = Goal0,
		Unifies = Unifies1,
		list__foldl(lambda([G::in, Vs0::in, Vs::out] is det,
			(
				G = Expr - _,
				Expr = unify(_, _, _, U, _),
				U = construct(_, _, UVars, _, _, _, _)
			->
				set__insert_list(Vs0, UVars, Vs)
			;
				Vs = Vs0
			)), Unifies, ChangedVarsSet0, ChangedVarsSet),

		maybe_create_new_proc(PredProcId, ChangedVarsSet,
			Module0, Module1, Goal1, Goal, yes),

		list__append(Unifies, [Goal | NoTagUnifies], LaterGoals),
		list__reverse(Goals0, FrontGoals),
		list__append(FrontGoals, LaterGoals, Goals1)
	->
		Proc = Proc1,
		Goals = Goals1,
		Module = Module1,
		Changed = yes
	;
		% The conjunction does not follow the pattern "unify*, goal"
		% so we cannot optimize it; reconstruct the original goal list
		list__reverse([Goal0 | Goals0], FrontGoals),
		list__append(FrontGoals, Unifies0, Goals1),

		% We may, however, be able to optimise the last conjuct, so
		% give that a go.
		list__reverse(Goals1, RevGoals0),
		( RevGoals0 = [Last0 | RevGoals1] ->
			apply_penultimate_instmap_deltas(Goals1, InstMap0,
				InstMap),
			lco_in_sub_goal(PredProcId, Last0, Last,
				Module0, Module, InstMap, Proc0, Proc, Changed),
			list__reverse([Last | RevGoals1], Goals)
		;
			Goals = Goals1,
			Module = Module0,
			Proc = Proc0,
			Changed = no
		)
	).

:- pred apply_penultimate_instmap_deltas(list(hlds_goal), instmap, instmap).
:- mode apply_penultimate_instmap_deltas(in, in, out) is det.

apply_penultimate_instmap_deltas([], _, _) :-
	error("apply_penultimate_instmap_deltas: empty").
apply_penultimate_instmap_deltas([_], InstMap, InstMap).
apply_penultimate_instmap_deltas([_ - GoalInfo | Goals], InstMap0, InstMap) :-
	Goals = [_|_],
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap1),
	apply_penultimate_instmap_deltas(Goals, InstMap1, InstMap).

%-----------------------------------------------------------------------------%

:- pred goal_is_no_tag_construction(module_info, proc_info, hlds_goal).
:- mode goal_is_no_tag_construction(in, in, in) is semidet.

goal_is_no_tag_construction(Module, Proc, Goal) :-
	Goal = unify(_, _, _, Unif, _) - _,
	Unif = construct(Var, _, _, _, _, _, _),
	proc_info_vartypes(Proc, VarTypes),
	map__search(VarTypes, Var, Type),
	type_constructors(Type, Module, Constructors),
	module_info_globals(Module, Globals),
	type_is_no_tag_type(Constructors, Globals, _FunctorName, _ArgType).

%-----------------------------------------------------------------------------%

:- pred check_only_one_ref_per_var(list(hlds_goal), list(prog_var),
	module_info, proc_info, proc_info).
:- mode check_only_one_ref_per_var(in, in, in, in, in) is semidet.

check_only_one_ref_per_var(Unifies, CallVars, Module, CalledProcInfo,
		CallingProcInfo) :-
	Lambda = lambda([Goal::in, Vars::out, N0::in, N::out] is det, 
		( 
			Goal = unify(_, _, _, Unif, _) - _,
			Unif = construct(_, _, Vars0, _, _, _, _)
		->
			Vars = N0 - Vars0,
			N is N0 + 1
		;
			error("lco:check_only_one_ref_per_var incorrect goal")
		)),
	list__map_foldl(Lambda, Unifies, UnifVars, 0, _),

	proc_info_argmodes(CalledProcInfo,
			argument_modes(CalledInstTable, CalledModes)),
	proc_info_get_initial_instmap(CalledProcInfo, Module,
			CalledInstMap),
	assoc_list__from_corresponding_lists(CallVars, CalledModes,
			CalledVarModes),

	proc_info_headvars(CallingProcInfo, CallingHeadVars),
	proc_info_argmodes(CallingProcInfo, 
		argument_modes(CallingInstTable, CallingHeadModes)),
	proc_info_get_initial_instmap(CallingProcInfo, Module, CallingInstMap),
	assoc_list__from_corresponding_lists(CallingHeadVars, CallingHeadModes,
			CallingHeadVarModes),

	proc_info_vartypes(CallingProcInfo, Types),

	check_only_one_ref_per_var_2(CalledVarModes, UnifVars, CalledInstMap,
		CalledInstTable, Module, Types, CallingHeadVarModes,
		CallingInstMap, CallingInstTable).

:- pred check_only_one_ref_per_var_2(assoc_list(prog_var, mode),
	list(pair(int, list(prog_var))), instmap, inst_table, module_info,
	map(prog_var, type), assoc_list(prog_var, mode), instmap, inst_table).
:- mode check_only_one_ref_per_var_2(in, in, in, in, in, in, in, in, in)
	is semidet.

check_only_one_ref_per_var_2([], _, _, _, _, _, _, _, _).
check_only_one_ref_per_var_2([Var - Mode | VarModes], UnifVars, CalledInstMap,
		CalledInstTable, Module, Types, CallingHeadVarModes,
		CallingInstMap, CallingInstTable) :-
	( 
		map__search(Types, Var, Type),
		mode_to_arg_mode(CalledInstMap, CalledInstTable, Module,
				Mode, Type, top_out)
	->
		% Ensure that no single construction uses the var more the once.
		\+ (
			list__member(_ - ConsVars0, UnifVars),
			list__delete_first(ConsVars0, Var, ConsVars1),
			list__member(Var, ConsVars1)
		),

		% Ensure that there is at most one construction
		% that has this variable on its RHS.
		\+ (
			list__member(N1 - Vars1, UnifVars),
			list__member(N2 - Vars2, UnifVars),
			N1 < N2,
			list__member(Var, Vars1),
			list__member(Var, Vars2)
		),

		% Ensure that, if this variable occurs on the RHS
		% of a construction, then it is not also an output
		% from the calling procedure.
		\+ (
			list__member(_ - Vars, UnifVars),
			list__member(Var, Vars),
			list__member(Var - HMode, CallingHeadVarModes),
			mode_to_arg_mode(CallingInstMap, CallingInstTable,
				Module, HMode, Type, ArgMode), 
			( ArgMode = top_out 
			; ArgMode = ref_in
			)
		)
	;
		true
	),
	check_only_one_ref_per_var_2(VarModes, UnifVars, CalledInstMap,
		CalledInstTable, Module, Types, CallingHeadVarModes,
		CallingInstMap, CallingInstTable).

%-----------------------------------------------------------------------------%

% We need a proc that is the same as the called proc, but with aliasing on
% some of the output variables.  See if the required proc already exists
% and if it doesn't, create it.

:- pred maybe_create_new_proc(pred_proc_id, set(prog_var), module_info,
		module_info, hlds_goal, hlds_goal, bool).
:- mode maybe_create_new_proc(in, in, in, out, in, out, out) is det.

maybe_create_new_proc(CallingPredProcId, ChangedVars, Module0, Module,
		Goal0, Goal, DoOpt) :-
	(
	    Goal0 = call(PredId, ProcId0, Vars, A,B,C) - GoalInfo
	->
	    module_info_pred_info(Module0, PredId, PredInfo0),
	    pred_info_procedures(PredInfo0, ProcTable0),
	    map__lookup(ProcTable0, ProcId0, ProcInfo0),
	    proc_info_argmodes(ProcInfo0, ArgModes0),
	    ArgModes0 = argument_modes(ArgInstTable, Modes0),
	    proc_info_inst_table(ProcInfo0, InstTable0),
	    proc_info_get_initial_instmap(ProcInfo0, Module0, InstMap0),
	    assoc_list__from_corresponding_lists(Vars, Modes0, VarModes0),
	    list__map(change_arg_mode(ChangedVars, Module0, InstMap0,
					InstTable0), 
		    VarModes0, Modes),
	    ArgModes = argument_modes(ArgInstTable, Modes),

	    (
		% See if a procedure with these modes already exists
		find_matching_proc(ProcTable0, InstMap0, ArgModes, Module0,
				ProcId1)
	    ->
		Module = Module0,
		(
		    % Make sure the output arguments of the two procs match up
		    % so that we get a tail call.
		    output_arguments_match(Module, CallingPredProcId,
			proc(PredId, ProcId1), Vars)
		->
		    Goal = call(PredId, ProcId1, Vars, A,B,C) - GoalInfo,
		    DoOpt = yes
		;
		    Goal = Goal0,
		    DoOpt = no
		)
	    ;
		create_new_proc(ProcTable0, ProcId0, ArgModes, InstTable0,
		    ProcTable1, ProcId),
		Goal1 = call(PredId, ProcId, Vars, A,B,C) - GoalInfo,
		pred_info_set_procedures(PredInfo0, ProcTable1, PredInfo1),
		module_info_set_pred_info(Module0, PredId, PredInfo1, Module1),

		% Run lco on the new proc.
		map__lookup(ProcTable1, ProcId, ProcInfo1),
		proc_info_goal(ProcInfo1, ProcGoal0),
		lco_in_goal(proc(PredId, ProcId), ProcGoal0, ProcGoal1,
			Module1, Module2, InstMap0,
			ProcInfo1, ProcInfo2, DoOpt),

		( DoOpt = yes ->
		    Goal = Goal1,

		    % Fix modes of unifications and calls in the new proc
		    % that bind aliased output arguments.
		    proc_info_headvars(ProcInfo2, HeadVars),
		    proc_info_vartypes(ProcInfo2, Types0),
		    proc_info_inst_table(ProcInfo2, ProcInstTable0),
		    proc_info_get_initial_instmap(ProcInfo2, Module2,
			ProcInstMap2),
		    assoc_list__from_corresponding_lists(HeadVars, Modes,
			VarModes),
		    Filter = lambda([VarMode::in, Var::out] is semidet,
			(
			    VarMode = Var - Mode,
			    map__lookup(Types0, Var, Type),
			    mode_to_arg_mode(ProcInstMap2, ProcInstTable0,
				Module2, Mode, Type, ref_in)
			)),
		    list__filter_map(Filter, VarModes, AliasedVars),

		    proc_info_varset(ProcInfo2, VarSet0),
		    proc_info_get_initial_instmap(ProcInfo2, Module2, InstMap),

		    FMI0 = fix_modes_info(VarSet0, Types0, ProcInstTable0,
			InstMap),
		    set__list_to_set(AliasedVars, AliasedVarSet),
		    list__foldl2(
			lambda([V::in, G0::in, G::out, F0::in, F::out] is det,(
			    fix_modes_of_binding_goal(Module2, AliasedVarSet, V,
				G0, G, F0, F1),
			    fix_modes_info_set_instmap(F1, InstMap, F)
			)), AliasedVars, ProcGoal1, ProcGoal, FMI0, FMI),

		    proc_info_set_goal(ProcInfo2, ProcGoal, ProcInfo3),
		    FMI = fix_modes_info(VarSet, Types, ProcInstTable, _),
		    proc_info_set_varset(ProcInfo3, VarSet, ProcInfo4),
		    proc_info_set_vartypes(ProcInfo4, Types, ProcInfo5),
		    proc_info_set_inst_table(ProcInfo5, ProcInstTable,
			ProcInfo),
		    map__set(ProcTable1, ProcId, ProcInfo, ProcTable),
		    pred_info_set_procedures(PredInfo1, ProcTable, PredInfo),
		    module_info_set_pred_info(Module2, PredId, PredInfo, Module)
		;
		    Goal = Goal0,
		    Module = Module0
		)
	    )
	;
		error("lco:maybe_create_new_proc: internal error")
	).

:- pred output_arguments_match(module_info, pred_proc_id, pred_proc_id,
		list(prog_var)).
:- mode output_arguments_match(in, in, in, in) is semidet.

output_arguments_match(Module, PredProcIdA, PredProcIdB, CallVars) :-
	module_info_pred_proc_info(Module, PredProcIdA, PredA, ProcA),
	proc_info_headvars(ProcA, HeadVarsA),
	get_top_out_arg_locs(Module, PredA, ProcA, HeadVarsA, VarLocsA),

	module_info_pred_proc_info(Module, PredProcIdB, PredB, ProcB),
	get_top_out_arg_locs(Module, PredB, ProcB, CallVars, VarLocsB),

	all [VarLoc] (
		list__member(VarLoc, VarLocsA)
	=>
		list__member(VarLoc, VarLocsB)
	).

:- pred get_top_out_arg_locs(module_info, pred_info, proc_info,
		list(prog_var), assoc_list(prog_var, arg_loc)).
:- mode get_top_out_arg_locs(in, in, in, in, out) is det.

get_top_out_arg_locs(Module, Pred, Proc, Vars, VarLocs) :-
	pred_info_arg_types(Pred, Types),
	proc_info_argmodes(Proc, argument_modes(InstTable, Modes)),
	proc_info_interface_code_model(Proc, CodeModel),
	proc_info_get_initial_instmap(Proc, Module, InstMap),

	make_arg_infos(Types, Modes, CodeModel, InstMap,
		InstTable, Module, ArgInfos),

	assoc_list__from_corresponding_lists(Vars, ArgInfos, VarInfos),
	list__filter_map(pred((V - arg_info(Loc, top_out))::in,
			(V - Loc)::out) is semidet,
		VarInfos, VarLocs).

:- pred get_unused_proc_id(proc_id, proc_table, proc_id).
:- mode get_unused_proc_id(in, in, out) is det.

get_unused_proc_id(ProcId0, ProcTable, ProcId) :-
	( map__contains(ProcTable, ProcId0) ->
		hlds_pred__next_proc_id(ProcId0, ProcId1),
		get_unused_proc_id(ProcId1, ProcTable, ProcId)
	;
		ProcId = ProcId0
	).


% If Var is in the set of variables that need their modes changed and mode
% is (free(unique) -> I), then change mode to (free(alias) -> I).
:- pred change_arg_mode(set(prog_var), module_info, instmap, inst_table,
		pair(prog_var, mode), mode).
:- mode change_arg_mode(in, in, in, in, in, out) is det.

change_arg_mode(VarSet, Module, InstMap, InstTable, Var - Mode0, Mode) :-
	( 
		set__member(Var, VarSet),
		mode_is_output(InstMap, InstTable, Module, Mode0) 
	->
		mode_get_insts(Module, Mode0, _, FinalInst),
		Mode = (free(alias) -> FinalInst)
	;
		Mode = Mode0
	).

% Find a procedure in the ProcTable that has argmodes equivalent to those
% given.
:- pred find_matching_proc(proc_table, instmap, argument_modes,
		module_info, proc_id).
:- mode find_matching_proc(in, in, in, in, out) is semidet.

find_matching_proc(ProcTable, InstMapA, ArgModesA, Module, ProcId) :-
	ArgModesA = argument_modes(InstTableA, ModesA),
	Lambda = lambda([ProcInfo::in] is semidet,
		(
			proc_info_argmodes(ProcInfo, ArgModesB),
			proc_info_get_initial_instmap(ProcInfo, Module,
					InstMapB),
			ArgModesB = argument_modes(InstTableB, ModesB),
			assoc_list__from_corresponding_lists(ModesA, ModesB,
				ModesAB),
			\+ ( list__member(A - B, ModesAB),
			    \+ (
				    mode_get_insts(Module, A, IA, FA),
				    mode_get_insts(Module, B, IB, FB),
				    inst_expand(InstMapA, InstTableA,
						Module, IA, I),
				    inst_expand(InstMapB, InstTableB,
						Module, IB, I),
				    inst_expand(InstMapA, InstTableA,
						Module, FA, F),
				    inst_expand(InstMapB, InstTableB,
						Module, FB, F),
				    alias_iff_alias(IA, IB),
				    alias_iff_alias(FA, FB)
			    )
			)
		)),
	get_first_from_map(Lambda, ProcTable, ProcId).

% XXX InstA = alias(_) <=> InstB = alias(_).  
% Get around a bug which currently does not allow this goal as written above.
:- pred alias_iff_alias((inst)::in, (inst)::in) is semidet.

alias_iff_alias(alias(_), alias(_)).
alias_iff_alias(IA, IB) :-
	IA \= alias(_),
	IB \= alias(_).

:- pred create_new_proc(proc_table, proc_id, argument_modes, inst_table,
		proc_table, proc_id).
:- mode create_new_proc(in, in, in, in, out, out) is det.

create_new_proc(ProcTable0, OldProcId, ArgModes, InstTable, ProcTable, 
		NewProcId) :-
	get_unused_proc_id(OldProcId, ProcTable0, NewProcId),
	map__lookup(ProcTable0, OldProcId, ProcInfo0),
	proc_info_set_argmodes(ProcInfo0, ArgModes, ProcInfo1),
	proc_info_set_inst_table(ProcInfo1, InstTable, ProcInfo),
	map__det_insert(ProcTable0, NewProcId, ProcInfo, ProcTable).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type fix_modes_info 
	--->	fix_modes_info(
			prog_varset,
			map(prog_var, type),
			inst_table,
			instmap
		).

:- pred fix_modes_info_apply_instmap_delta(fix_modes_info, instmap_delta, 
	fix_modes_info).
:- mode fix_modes_info_apply_instmap_delta(in, in, out) is det.

fix_modes_info_apply_instmap_delta(FMI0, IMD, FMI) :-
	FMI0 = fix_modes_info(A, B, C, IM0),
	instmap__apply_instmap_delta(IM0, IMD, IM),
	FMI = fix_modes_info(A, B, C, IM).

:- pred fix_modes_info_get_instmap(fix_modes_info, instmap).
:- mode fix_modes_info_get_instmap(in, out) is det.

fix_modes_info_get_instmap(fix_modes_info(_, _, _, InstMap), InstMap).

:- pred fix_modes_info_set_instmap(fix_modes_info, instmap, fix_modes_info).
:- mode fix_modes_info_set_instmap(in, in, out) is det.

fix_modes_info_set_instmap(fix_modes_info(A, B, C, _), InstMap,
	fix_modes_info(A, B, C, InstMap)).

% After creating a new proc with aliased output arguments, it is necessary
% to alter the modes of any unifications within the proc goal that bind those
% arguments.  If the arguments are bound in a call then an assignment
% may need to be added after the call.

:- pred fix_modes_of_binding_goal(module_info, set(prog_var), prog_var,
		hlds_goal, hlds_goal, fix_modes_info, fix_modes_info).
:- mode fix_modes_of_binding_goal(in, in, in, in, out, in, out) is det.

fix_modes_of_binding_goal(Module, AliasedVars, Var,
		GoalExpr0 - GoalInfo, GoalExpr - GoalInfo, FMI0, FMI) :-
	FMI0 = fix_modes_info(_, VarTypes, InstTable, InstMap0),
	goal_info_get_instmap_delta(GoalInfo, IMD),
	instmap__apply_instmap_delta(InstMap0, IMD, InstMap),
	instmap__lookup_var(InstMap0, Var, InitialInst),
	instmap__lookup_var(InstMap,  Var, FinalInst),
	map__lookup(VarTypes, Var, Type),
	(
		% Does the goal bind Var?
		insts_to_arg_mode(InstTable, Module, InitialInst, InstMap0,
			FinalInst, InstMap, Type, ref_in)
	->
		fix_modes_of_binding_goal_2(GoalExpr0, FMI0, GoalInfo,
			Module, AliasedVars, Var, GoalExpr, FMI1)
	;
		GoalExpr = GoalExpr0,
		FMI1 = FMI0
	),
	fix_modes_info_set_instmap(FMI1, InstMap, FMI).

:- pred fix_modes_of_binding_goal_2(hlds_goal_expr, fix_modes_info,
		hlds_goal_info, module_info, set(prog_var), prog_var,
		hlds_goal_expr, fix_modes_info).
:- mode fix_modes_of_binding_goal_2(in, in, in, in, in, in, out, out) is det.

fix_modes_of_binding_goal_2(conj(Goals0), FMI0, _, Module,
		AliasedVars, Var, conj(Goals), FMI) :-
	list__map_foldl(fix_modes_of_binding_goal(Module, AliasedVars, Var),
		Goals0, Goals, FMI0, FMI).

fix_modes_of_binding_goal_2(par_conj(Goals0, SM), FMI0, _GoalInfo0,
		Module, AliasedVars, Var, par_conj(Goals, SM), FMI) :-
	fix_modes_info_get_instmap(FMI0, InstMap0),
	Lambda = lambda([Goal0::in, Goal::out, F0::in, F::out] is det,
		(
			fix_modes_info_set_instmap(F0, InstMap0, F1),
			fix_modes_of_binding_goal(Module, AliasedVars, Var,
				Goal0, Goal, F1, F)
		)),
	list__map_foldl(Lambda, Goals0, Goals, FMI0, FMI).

fix_modes_of_binding_goal_2(call(PredId, ProcId0, Vars0, D, E, F), FMI0,
		GoalInfo0, Module, AliasedVars, Var, Goal, FMI) :-
	( 
		replace_call_proc_with_aliased_version(PredId, ProcId0, FMI0,
			Module, Var, AliasedVars, Vars0, ProcId)
	->
		FMI = FMI0,
		Goal = call(PredId, ProcId, Vars0, D, E, F)
	;
		add_unification_to_goal(Vars0, FMI0, GoalInfo0, Module, Var,
			Vars, FMI, GoalInfo, Assign),
		( Vars = Vars0 ->
			Goal = call(PredId, ProcId0, Vars0, D, E, F)
		;
			Call = call(PredId, ProcId0, Vars, D, E, F) - GoalInfo,
			Goal = conj([Call, Assign])
		)
	).

fix_modes_of_binding_goal_2(generic_call(A, Vars0, C, D), FMI0,
		GoalInfo0, Module, _AliasedVars, Var, Goal, FMI) :-
	add_unification_to_goal(Vars0, FMI0, GoalInfo0, Module, Var,
		Vars, FMI, GoalInfo, Assign),
	HigherOrder = generic_call(A, Vars, C, D) - GoalInfo,
	Goal = conj([HigherOrder, Assign]).

fix_modes_of_binding_goal_2(switch(SVar, Det, Cases0, SM), FMI0, _, 
		Module, AliasedVars, Var, switch(SVar, Det, Cases, SM), FMI) :-
	fix_modes_info_get_instmap(FMI0, InstMap0),
	Lambda = lambda([Case0::in, Case::out, F0::in, F::out] is det,
		(
			Case0 = case(ConsId, CaseIMD, Goal0),
			instmap__apply_instmap_delta(InstMap0, CaseIMD,
				InstMap),
			fix_modes_info_set_instmap(F0, InstMap, F1),
			fix_modes_of_binding_goal(Module, AliasedVars, Var,
				Goal0, Goal, F1, F),
			Case = case(ConsId, CaseIMD, Goal)
		)),
	list__map_foldl(Lambda, Cases0, Cases, FMI0, FMI).

fix_modes_of_binding_goal_2(unify(LHS, RHS0, Modes0, Unif0, Cont), FMI0, 
		GoalInfo0, Module, _AliasedVars, Var, Goal, FMI) :-
	fix_modes_of_unify(Unif0, RHS0, Modes0, FMI0, GoalInfo0, Module, Var,
		Unif, RHS, Modes, FMI, GoalInfo, MaybeAssign),
	UnifyGoal = unify(LHS, RHS, Modes, Unif, Cont),
	( 
		MaybeAssign = no,
		Goal = UnifyGoal
	;
		MaybeAssign = yes(Assign),
		Goal = conj([UnifyGoal - GoalInfo, Assign])
	).

fix_modes_of_binding_goal_2(disj(Goals0, SM), FMI0, _, Module, AliasedVars,
		Var, disj(Goals, SM), FMI) :-
	Lambda = lambda([Goal0::in, Goal::out, F0::in, F::out] is det,
		(
			fix_modes_info_get_instmap(F0, InstMap),
			fix_modes_of_binding_goal(Module, AliasedVars, Var,
				Goal0, Goal, F0, F1),
			fix_modes_info_set_instmap(F1, InstMap, F)
		)),
	list__map_foldl(Lambda, Goals0, Goals, FMI0, FMI).

fix_modes_of_binding_goal_2(not(Goal), FMI, _, _, _, _, not(Goal),
		FMI).

fix_modes_of_binding_goal_2(some(Vars, CanRemove, Goal0), FMI0, _, Module,
		AliasedVars, Var, some(Vars, CanRemove, Goal), FMI) :-
	fix_modes_of_binding_goal(Module, AliasedVars, Var, Goal0, Goal, FMI0,
		FMI).

fix_modes_of_binding_goal_2(if_then_else(Vars, Cond, Then0, Else0, SM),
		FMI0, _, Module, AliasedVars, Var,
		if_then_else(Vars, Cond, Then, Else, SM), FMI) :-
	fix_modes_info_get_instmap(FMI0, InstMap0),
	Cond = _ - CondGoalInfo,
	goal_info_get_instmap_delta(CondGoalInfo, IMD),
	fix_modes_info_apply_instmap_delta(FMI0, IMD, FMI1),
	fix_modes_of_binding_goal(Module, AliasedVars, Var, Then0, Then, FMI1,
		FMI2),
	fix_modes_info_set_instmap(FMI2, InstMap0, FMI3),
	fix_modes_of_binding_goal(Module, AliasedVars, Var, Else0, Else, FMI3,
		FMI).

fix_modes_of_binding_goal_2(pragma_c_code(A, B, C, Vars0, E, F, G),
		FMI0, GoalInfo0, Module, _AliasedVars, Var, Goal, FMI) :-
	add_unification_to_goal(Vars0, FMI0, GoalInfo0, Module, Var,
		Vars, FMI, GoalInfo, Assign),
	PragmaC = pragma_c_code(A, B, C, Vars, E, F, G) - GoalInfo,
	Goal = conj([PragmaC, Assign]).

:- pred add_unification_to_goal(list(prog_var), fix_modes_info, hlds_goal_info,
		module_info, prog_var, list(prog_var), fix_modes_info,
		hlds_goal_info, hlds_goal).
:- mode add_unification_to_goal(in, in, in, in, in, out, out, out, out) is det.

add_unification_to_goal(Vars0, FMI0, GoalInfo0, Module, Var,
		Vars, FMI, CallGoalInfo, Assign):-
	FMI0 = fix_modes_info(VarSet0, VarTypes0, InstTable, InstMap),
	varset__new_var(VarSet0, NewVar, VarSet),
	map__lookup(VarTypes0, Var, Type),
	map__det_insert(VarTypes0, NewVar, Type, VarTypes),

	FMI1 = fix_modes_info(VarSet, VarTypes, InstTable, InstMap),

	goal_info_get_instmap_delta(GoalInfo0, IMD0),
	instmap__apply_instmap_delta(InstMap, IMD0, InstMapAfter),
	instmap__lookup_var(InstMapAfter, Var, Inst),
	map__init(Sub0),
	map__det_insert(Sub0, Var, NewVar, Sub),
	instmap_delta_apply_sub(IMD0, no, Sub, IMD),
	goal_info_set_instmap_delta(GoalInfo0, IMD, CallGoalInfo1),
	goal_info_get_nonlocals(CallGoalInfo1, CallNonLocals0),
	set__delete(CallNonLocals0, Var, CallNonLocals1),
	set__insert(CallNonLocals1, NewVar, CallNonLocals),
	goal_info_set_nonlocals(CallGoalInfo1, CallNonLocals, CallGoalInfo),

	list__replace_all(Vars0, Var, NewVar, Vars),
	Modes = (free(alias) - Inst) - (Inst - Inst),
	goal_info_init(AssignGoalInfo0),
	instmap_delta_from_assoc_list([Var - Inst], AssignIMD),
	goal_info_set_instmap_delta(AssignGoalInfo0, AssignIMD,
		AssignGoalInfo1),
	goal_info_set_determinism(AssignGoalInfo1, det, AssignGoalInfo2),
	set__list_to_set([Var, NewVar], NonLocals),
	goal_info_set_nonlocals(AssignGoalInfo2, NonLocals, AssignGoalInfo),
	Assign0 = unify(Var, var(NewVar), Modes, assign(Var, NewVar),
		unify_context(explicit, [])) - AssignGoalInfo,

	set__init(DummyVars),
	fix_modes_of_binding_goal(Module, DummyVars, Var, Assign0, Assign,
		FMI1, FMI).

:- pred fix_modes_of_unify(unification, unify_rhs, unify_mode, fix_modes_info,
		hlds_goal_info, module_info, prog_var, unification, unify_rhs,
		unify_mode, fix_modes_info, hlds_goal_info, maybe(hlds_goal)).
:- mode fix_modes_of_unify(in, in, in, in, in, in, in, out, out, out, out,
		out, out) is det.

fix_modes_of_unify(construct(LHSVar, ConsId, Vars, UniModes0, E, F, G),
		RHS, Modes, FMI0, GoalInfo, Module, Var,
		construct(LHSVar, ConsId, Vars, UniModes, E, F, G),
		RHS, Modes, FMI, GoalInfo, no) :-
	( LHSVar = Var ->
		FMI0 = fix_modes_info(VarSet, VarTypes, InstTable0, InstMap),
		list__map_foldl(fix_uni_mode(Module, InstMap), 
			UniModes0, UniModes, InstTable0, InstTable),
		FMI = fix_modes_info(VarSet, VarTypes, InstTable, InstMap)
	;
		error("lco:fix_mode_of_unify: LHSVar \\= Var")
	).

fix_modes_of_unify(deconstruct(LHSVar, ConsId, Vars0, UniModes, CanFail),
		RHS0, Modes, FMI0, GoalInfo0, Module, Var, 
		deconstruct(LHSVar, ConsId, Vars, UniModes, CanFail), RHS,
		Modes, FMI, GoalInfo, yes(Assign)) :-
	add_unification_to_goal(Vars0, FMI0, GoalInfo0, Module, Var, Vars,
		FMI, GoalInfo, Assign),
	( RHS0 = functor(ConsId, _) ->
		RHS = functor(ConsId, Vars)
	;
		RHS = RHS0
	).

fix_modes_of_unify(assign(L, R), RHS, Modes0, FMI, GoalInfo, _, _,
		assign(L, R), RHS, Modes, FMI, GoalInfo, no) :-
	Modes = Modes0.

% Shouldn't get simple_test binding a variable.
fix_modes_of_unify(simple_test(_, _),_,_,_,_,_,_,_,_,_,_,_,_) :-
	error("lco:fix_modes_of_unify: simple_test in unify").

% Should already have been transformed into calls by polymorphism.m.
fix_modes_of_unify(complicated_unify(_, _, _),_,_,_,_,_,_,_,_,_,_,_,_) :-
	error("lco:fix_modes_of_unify: complicated_unify").

:- pred fix_uni_mode(module_info, instmap, uni_mode, uni_mode,
		inst_table, inst_table).
:- mode fix_uni_mode(in, in, in, out, in, out) is det.

fix_uni_mode(Module, InstMap0, UniMode0, UniMode, InstTable0, InstTable) :-
	UniMode0 = ((LI0 - RI) -> (LF - RF)),
	(
		inst_is_free(LI0, InstMap0, InstTable0, Module)
	->
		( LI0 = alias(_) ->
			LI = LI0,
			InstTable = InstTable0
		;
			inst_table_get_inst_key_table(InstTable0, IKT0),
			inst_key_table_add(IKT0, free(alias), IK, IKT),
			inst_table_set_inst_key_table(InstTable0, IKT,
				InstTable),
			LI = alias(IK)
		),
		UniMode = ((LI - RI) -> (LF - RF))
	;
		error("lco:fix_uni_mode: unexpected inst")
	).


% Try to find a mode of the predicate that is the same as the input ProcId0
% except that Var is ref_in intead of top_out.  Any varibles in AliasedVars
% that are top_out in ProcId0 may be either top_out or ref_in in ProcId
% (it is better if they are ref_in).  All other args must have the same
% mode in both procedures.

:- pred replace_call_proc_with_aliased_version(pred_id, proc_id,
	fix_modes_info, module_info, prog_var, set(prog_var), list(prog_var),
	proc_id).
:- mode replace_call_proc_with_aliased_version(in, in, in, in, in, in, in, out)
	is semidet.

replace_call_proc_with_aliased_version(PredId, ProcId0, FMI, Module, Var,
		AliasedVars, CallVars, ProcId) :-
	module_info_pred_info(Module, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId0, ProcInfo0),
	proc_info_argmodes(ProcInfo0, argument_modes(InstTableA, ModesA)),
	proc_info_get_initial_instmap(ProcInfo0, Module, InstMapA),
	FMI = fix_modes_info(_, _, InstTable, InstMap),

	Lambda = lambda([ProcInfo::in] is semidet,
	    (
		proc_info_argmodes(ProcInfo, ArgModesB),
		ArgModesB = argument_modes(InstTableB, ModesB),
		proc_info_get_initial_instmap(ProcInfo, Module, InstMapB),
		assoc_list__from_corresponding_lists(ModesA, ModesB, ModesAB),
		assoc_list__from_corresponding_lists(ModesAB, CallVars,
		    ModeVars),
		\+ ( list__member(A - B - V, ModeVars),
		    \+ (
			mode_get_insts(Module, A, IA, FA),
			mode_get_insts(Module, B, IB, FB),
			inst_expand(InstMapA, InstTableA, Module, FA, F),
			inst_expand(InstMapB, InstTableB, Module, FB, F),
			( V = Var ->
			    inst_is_free_alias(IB, InstMapB, InstTableB, Module)
			; set__member(V, AliasedVars) ->
			    % Make sure mode is no worse than what we already
			    % have.
			    inst_is_free_alias(IA, InstMapA, InstTableA, Module)
			    => inst_is_free_alias(IB, InstMapB, InstTableB,
					Module),

			    % If V is free(alias) then either free(alias) or
			    % free(unique) will do for the initial inst here.
			    % If the new proc has free(unique) and there is
			    % another proc that is free(alias) both for
			    % V and Var, then that proc will be found when
			    % fix_modes_of_binding_goal is called for V.
			    instmap__lookup_var(InstMap, V, InstV),
			    inst_is_free_alias(InstV, InstMap, InstTable,
					Module)
				=> inst_is_free(IB, InstMapB, InstTableB,
					Module)
			;
			    % This is safe because the procs being compared
			    % have the same arg inst_table.
			    inst_expand(InstMapA, InstTableA, Module, IA, I),
			    inst_expand(InstMapB, InstTableB, Module, IB, I)
			)
		    )
		)
	    )),
	get_first_from_map(Lambda, ProcTable, ProcId).


% Perhaps these two preds should be in the library?

:- pred get_first_from_map(pred(V), map(K, V), K).
:- mode get_first_from_map(pred(in) is semidet, in, out) is semidet.

get_first_from_map(P, M, K) :-
	map__to_assoc_list(M, AL),
	get_first_from_assoc_list(P, AL, K).

:- pred get_first_from_assoc_list(pred(V), assoc_list(K, V), K).
:- mode get_first_from_assoc_list(pred(in) is semidet, in, out) is semidet.

get_first_from_assoc_list(P, [K0 - V0 | Rest], K) :-
	( call(P, V0) ->
		K = K0
	;
		get_first_from_assoc_list(P, Rest, K)
	).
