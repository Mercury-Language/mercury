%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2001 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File pd_util.m
% Main author: stayl.
%
% Utility predicates for deforestation and partial evaluation.
%
%-----------------------------------------------------------------------------%
:- module pd_util.

:- interface.

:- import_module pd_info, hlds_goal, hlds_module, hlds_pred, mode_errors.
:- import_module prog_data, simplify, (inst).
:- import_module bool, list, map, set, std_util.

	% Pick out the pred_proc_ids of the calls in a list of atomic goals.
:- pred pd_util__goal_get_calls(hlds_goal::in,
		list(pred_proc_id)::out) is det.

	% Call constraint.m to transform a goal so that goals which
	% can fail are executed as early as possible.
:- pred pd_util__propagate_constraints(hlds_goal::in, hlds_goal::out,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.
	
	% Apply simplify.m to the goal.
:- pred pd_util__simplify_goal(list(simplification)::in, hlds_goal::in,
		hlds_goal::out, pd_info::pd_info_di,
		pd_info::pd_info_uo) is det.

	% Apply unique_modes.m to the goal.
:- pred pd_util__unique_modecheck_goal(hlds_goal::in, hlds_goal::out,
		list(mode_error_info)::out, pd_info::pd_info_di, 
		pd_info::pd_info_uo) is det.

	% Apply unique_modes.m to the goal.
:- pred pd_util__unique_modecheck_goal(set(prog_var)::in, hlds_goal::in, 
		hlds_goal::out, list(mode_error_info)::out, 
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

	% Find out which arguments of the procedure are interesting
	% for deforestation.
:- pred pd_util__get_branch_vars_proc(pred_proc_id::in, proc_info::in, 
		pd_arg_info::in, pd_arg_info::out,
		module_info::in, module_info::out) is det.

	% Find out which variables of the goal are interesting
	% for deforestation.
:- pred pd_util__get_branch_vars_goal(hlds_goal::in, 
		maybe(pd_branch_info(prog_var))::out, pd_info::pd_info_di,
		pd_info::pd_info_uo) is det.

	% Recompute the non-locals of the goal.
:- pred pd_util__requantify_goal(hlds_goal::in, set(prog_var)::in,
		hlds_goal::out, pd_info::pd_info_di, pd_info::pd_info_uo)
		is det.

	% Apply mode_util__recompute_instmap_delta to the goal.
:- pred pd_util__recompute_instmap_delta(hlds_goal::in, hlds_goal::out, 
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

	% Convert from information about the argument positions to 
	% information about the argument variables.
:- pred pd_util__convert_branch_info(pd_branch_info(int)::in,
		list(prog_var)::in, pd_branch_info(prog_var)::out) is det.	

	% inst_MSG(InstA, InstB, InstC):
	% 	Take the most specific generalisation of two insts.
	%       The information in InstC is the minimum of the
	%       information in InstA and InstB.  Where InstA and
	%       InstB specify a binding (free or bound), it must be
	%       the same in both.
	% 	The uniqueness of the final inst is taken from InstB.
	% 	The difference between inst_merge and inst_MSG is that the 
	% 	msg of `bound([functor, []])' and `bound([another_functor, []])'
	%	is `ground' rather than `bound([functor, another_functor])'. 
	% 	Also the msgs are not tabled, so the module_info is not
	% 	threaded through.
	% 	If an inst is "rounded off", it must not contain `any' insts
	% 	and must be completely unique or completely non-unique.
	% 	This is used in generalisation to avoid non-termination
	% 	of deforestation - InstA is the inst in an old version,
	% 	we are taking the msg with to avoid non-termination,
	% 	InstB is the inst in the new version we want to create.
	%	It is always safe for inst_MSG to fail - this will just
	% 	result in less optimization.
	% 	Mode analysis should be run on the goal to
	%	check that this doesn't introduce mode errors, since
	% 	the information that was removed may actually have been
	%	necessary for mode correctness.
:- pred inst_MSG(inst, inst, module_info, inst).
:- mode inst_MSG(in, in, in, out) is semidet.


	% Produce an estimate of the size of an inst, based on the 
	% number of nodes in the inst. The inst is expanded down
	% to the first repeat of an already expanded inst_name.
:- pred pd_util__inst_size(module_info::in, (inst)::in, int::out) is det.
:- pred pd_util__inst_list_size(module_info::in, list(inst)::in,
		int::out) is det.

	% pd_util__goals_match(ModuleInfo, OldGoal, OldArgs, OldArgTypes,
	%		NewGoal, NewArgTypes,
	% 		OldToNewVarRenaming, OldToNewTypeSubst)
	%
	% Check the shape of the goals, and return a mapping from
	% variables in the old goal to variables in the new and
	% a substitution to apply to the types. This only
	% attempts to match `simple' lists of goals, which contain
	% only conj, some, not and atomic goals, since deforest.m
	% only attempts to optimize those types of conjunctions.
:- pred pd_util__goals_match(module_info::in, hlds_goal::in, list(prog_var)::in,
		list(type)::in, hlds_goal::in, vartypes::in,
		map(prog_var, prog_var)::out, tsubst::out) is semidet.

	% pd_util__can_reorder_goals(ModuleInfo, FullyStrict, Goal1, Goal2).
	%
	% Goals can be reordered if
	% - the goals are independent
	% - the goals are not impure
	% - any possible change in termination behaviour is allowed
	% 	according to the semantics options.
:- pred pd_util__can_reorder_goals(module_info::in, bool::in, hlds_goal::in,
		hlds_goal::in) is semidet.

	% pd_util__reordering_maintains_termination(FullyStrict, Goal1, Goal2)
	%
	% Succeeds if any possible change in termination behaviour from
	% reordering the goals is allowed according to the semantics options.
	% The information computed by termination analysis is used when
	% making this decision.
:- pred pd_util__reordering_maintains_termination(module_info::in, bool::in, 
		hlds_goal::in, hlds_goal::in) is semidet.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module det_analysis, constraint, pd_cost, hlds_data, instmap.
:- import_module unused_args, inst_match, (inst), quantification, mode_util.
:- import_module code_aux, purity, mode_info, unique_modes, pd_debug.
:- import_module type_util, det_util, det_analysis, options, goal_util.
:- import_module det_report, inst_util.
:- import_module assoc_list, int, require, set, term.

pd_util__goal_get_calls(Goal0, CalledPreds) :-
	goal_to_conj_list(Goal0, GoalList),
	GetCalls = lambda([Goal::in, CalledPred::out] is semidet, (
			Goal = call(PredId, ProcId, _, _, _, _) - _,
			CalledPred = proc(PredId, ProcId)
		)),
	list__filter_map(GetCalls, GoalList, CalledPreds).

%-----------------------------------------------------------------------------%

pd_util__propagate_constraints(Goal0, Goal) -->
	pd_info_lookup_bool_option(local_constraint_propagation,
		ConstraintProp),
	( { ConstraintProp = yes } ->
		pd_debug__message("%% Propagating constraints\n", []),
		pd_debug__output_goal("before constraints\n", Goal0),	
		pd_info_get_module_info(ModuleInfo0),
		pd_info_get_proc_info(ProcInfo0),
		pd_info_get_instmap(InstMap),
		{ proc_info_vartypes(ProcInfo0, VarTypes0) },
		{ proc_info_varset(ProcInfo0, VarSet0) },
		{ constraint_info_init(ModuleInfo0, VarTypes0,
			VarSet0, InstMap, CInfo0) },
		{ Goal0 = _ - GoalInfo0 },
		{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
		{ constraint__propagate_constraints_in_goal(Goal0, Goal1,
			CInfo0, CInfo) },
		{ constraint_info_deconstruct(CInfo, ModuleInfo,
			VarTypes, VarSet, Changed) },
		pd_info_set_module_info(ModuleInfo),
		{ proc_info_set_vartypes(ProcInfo0, VarTypes, ProcInfo1) },
		{ proc_info_set_varset(ProcInfo1, VarSet, ProcInfo) },
		pd_info_set_proc_info(ProcInfo),
		( { Changed = yes } ->
			pd_debug__output_goal(
				"after constraints, before recompute\n",
				Goal1),
			pd_util__requantify_goal(Goal1, NonLocals, Goal2),
			pd_util__recompute_instmap_delta(Goal2, Goal3),
			pd_util__rerun_det_analysis(Goal3, Goal4),
		        { module_info_globals(ModuleInfo, Globals) },
		        { simplify__find_simplifications(no,
				Globals, Simplifications) },
			pd_util__simplify_goal(Simplifications, Goal4, Goal)
		;
			{ Goal = Goal1 }
		)
	;
		{ Goal = Goal0 }
	).

%-----------------------------------------------------------------------------%

pd_util__simplify_goal(Simplifications, Goal0, Goal) -->
	%
	% Construct a simplify_info.
	% 
	pd_info_get_module_info(ModuleInfo0),
	{ module_info_globals(ModuleInfo0, Globals) },
	pd_info_get_pred_proc_id(proc(PredId, ProcId)),
	{ proc_info_vartypes(ProcInfo0, VarTypes0) },
	{ det_info_init(ModuleInfo0, VarTypes0, PredId, ProcId,
		Globals, DetInfo0) },
	pd_info_get_instmap(InstMap0),
	pd_info_get_proc_info(ProcInfo0),
	{ proc_info_varset(ProcInfo0, VarSet0) },
	{ proc_info_inst_varset(ProcInfo0, InstVarSet0) },
	{ proc_info_typeinfo_varmap(ProcInfo0, TVarMap0) },
	{ proc_info_typeclass_info_varmap(ProcInfo0, TCVarMap0) },
	{ simplify_info_init(DetInfo0, Simplifications, InstMap0,
		VarSet0, InstVarSet0, TVarMap0, TCVarMap0, SimplifyInfo0) },

	{ simplify__process_goal(Goal0, Goal, SimplifyInfo0, SimplifyInfo) },

	%
	% Deconstruct the simplify_info.
	%
	{ simplify_info_get_module_info(SimplifyInfo, ModuleInfo) },
	{ simplify_info_get_varset(SimplifyInfo, VarSet) },
	{ simplify_info_get_var_types(SimplifyInfo, VarTypes) },
	{ simplify_info_get_cost_delta(SimplifyInfo, CostDelta) },
	{ simplify_info_get_type_info_varmap(SimplifyInfo, TVarMap) },
	{ simplify_info_get_typeclass_info_varmap(SimplifyInfo, TCVarMap) },
	pd_info_get_proc_info(ProcInfo1),
	{ proc_info_set_varset(ProcInfo1, VarSet, ProcInfo2) },
	{ proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo3) },
	{ proc_info_set_typeinfo_varmap(ProcInfo3, TVarMap, ProcInfo4) },
	{ proc_info_set_typeclass_info_varmap(ProcInfo4, TCVarMap, ProcInfo) },
	pd_info_set_proc_info(ProcInfo),
	pd_info_incr_cost_delta(CostDelta),
	pd_info_set_module_info(ModuleInfo).

%-----------------------------------------------------------------------------%

pd_util__unique_modecheck_goal(Goal0, Goal, Errors) -->
	pd_util__get_goal_live_vars(Goal0, LiveVars),
	pd_util__unique_modecheck_goal(LiveVars, Goal0, Goal, Errors).

pd_util__unique_modecheck_goal(LiveVars, Goal0, Goal, Errors) -->

	% 
	% Construct a mode_info.
	%
	pd_info_get_pred_proc_id(PredProcId),
	{ PredProcId = proc(PredId, ProcId) },
	pd_info_get_module_info(ModuleInfo0),
	pd_info_get_instmap(InstMap0),
	{ term__context_init(Context) },
	pd_info_get_io_state(IO0),
	pd_info_get_pred_info(PredInfo0),
	pd_info_get_proc_info(ProcInfo0),
	{ module_info_set_pred_proc_info(ModuleInfo0, PredId, ProcId,
		PredInfo0, ProcInfo0, ModuleInfo1) },

	% If we perform generalisation, we shouldn't change any called
	% procedures, since that could cause a less efficient version to
	% be chosen.
	{ MayChangeCalledProc = may_not_change_called_proc },
	{ mode_info_init(IO0, ModuleInfo1, PredId, ProcId, Context,
		LiveVars, InstMap0, check_unique_modes,
		MayChangeCalledProc, ModeInfo0) },

	{ unique_modes__check_goal(Goal0, Goal, ModeInfo0, ModeInfo1) },
	pd_info_lookup_bool_option(debug_pd, Debug),
	{ Debug = yes ->
		report_mode_errors(ModeInfo1, ModeInfo)
	;
		ModeInfo = ModeInfo1
	},
	{ mode_info_get_errors(ModeInfo, Errors) },

	%
	% Deconstruct the mode_info.
	%
	{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
	{ mode_info_get_io_state(ModeInfo, IO) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_var_types(ModeInfo, VarTypes) },
	pd_info_set_module_info(ModuleInfo),
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo1) },
	pd_info_set_pred_info(PredInfo),
	{ proc_info_set_varset(ProcInfo1, VarSet, ProcInfo2) },
	{ proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo) },
	pd_info_set_proc_info(ProcInfo),
	pd_info_set_io_state(IO).

	% Work out which vars are live later in the computation based
	% on which of the non-local variables are not clobbered by the goal.
:- pred pd_util__get_goal_live_vars(hlds_goal::in, set(prog_var)::out, 
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

pd_util__get_goal_live_vars(_ - GoalInfo, Vars) -->
	pd_info_get_module_info(ModuleInfo),
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	pd_info_get_instmap(InstMap),
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ set__to_sorted_list(NonLocals, NonLocalsList) },
	{ set__init(Vars0) },
	{ get_goal_live_vars_2(ModuleInfo, NonLocalsList, InstMap,
		InstMapDelta, Vars0, Vars) }.

:- pred pd_util__get_goal_live_vars_2(module_info::in, list(prog_var)::in,
		instmap::in, instmap_delta::in,
		set(prog_var)::in, set(prog_var)::out) is det.

pd_util__get_goal_live_vars_2(_, [], _, _, Vars, Vars).
pd_util__get_goal_live_vars_2(ModuleInfo, [NonLocal | NonLocals], 
		InstMap, InstMapDelta, Vars0, Vars) :-
	( instmap_delta_search_var(InstMapDelta, NonLocal, FinalInst0) ->
		FinalInst = FinalInst0
	;
		instmap__lookup_var(InstMap, NonLocal, FinalInst)
	),
	( inst_is_clobbered(ModuleInfo, FinalInst) ->
		Vars1 = Vars0
	;
		set__insert(Vars0, NonLocal, Vars1)
	),
	pd_util__get_goal_live_vars_2(ModuleInfo, NonLocals, 
		InstMap, InstMapDelta, Vars1, Vars).

%-----------------------------------------------------------------------------%

:- pred pd_util__rerun_det_analysis(hlds_goal::in, hlds_goal::out,
		pd_info::pd_info_di, pd_info::pd_info_uo) is det.

pd_util__rerun_det_analysis(Goal0, Goal) -->
	{ Goal0 = _ - GoalInfo0 },

	{ goal_info_get_determinism(GoalInfo0, Det) },
	{ det_get_soln_context(Det, SolnContext) },

	% det_infer_goal looks up the proc_info in the module_info
	% for the vartypes, so we'd better stick them back in the
	% module_info.
	pd_info_get_pred_proc_id(proc(PredId, ProcId)),
	pd_info_get_pred_info(PredInfo),
	pd_info_get_proc_info(ProcInfo),
	pd_info_get_module_info(ModuleInfo0),
	{ module_info_set_pred_proc_info(ModuleInfo0, PredId, ProcId,
		PredInfo, ProcInfo, ModuleInfo) },
	pd_info_set_module_info(ModuleInfo),

	{ module_info_globals(ModuleInfo, Globals) },
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ det_info_init(ModuleInfo, VarTypes, PredId, ProcId,
		Globals, DetInfo) },
	pd_info_get_instmap(InstMap),
	{ det_infer_goal(Goal0, InstMap, SolnContext, DetInfo,
		Goal, _, Msgs) },

	%
	% Make sure there were no errors.
	%
	pd_info_get_io_state(IO0),
	{ disable_det_warnings(OptionsToRestore, IO0, IO1) },
	{ det_report_msgs(Msgs, ModuleInfo, _, ErrCnt, IO1, IO2) },
	{ restore_det_warnings(OptionsToRestore, IO2, IO) },
	pd_info_set_io_state(IO),
	{ require(unify(ErrCnt, 0),
		"pd_util__rerun_det_analysis: determinism errors") }.

%-----------------------------------------------------------------------------%

pd_util__convert_branch_info(ArgInfo, Args, VarInfo) :-
	ArgInfo = pd_branch_info(ArgMap, LeftArgs, OpaqueArgs),
	map__to_assoc_list(ArgMap, ArgList),
	map__init(BranchVarMap0),
	pd_util__convert_branch_info_2(ArgList, Args,
		BranchVarMap0, BranchVarMap),

	set__to_sorted_list(LeftArgs, LeftArgNos),
	list__map(list__index1_det(Args), LeftArgNos, LeftVars0),
	set__list_to_set(LeftVars0, LeftVars),

	set__to_sorted_list(OpaqueArgs, OpaqueArgNos),
	list__map(list__index1_det(Args), OpaqueArgNos, OpaqueVars0),
	set__list_to_set(OpaqueVars0, OpaqueVars),

	VarInfo = pd_branch_info(BranchVarMap, LeftVars, OpaqueVars).

:- pred pd_util__convert_branch_info_2(assoc_list(int, set(int))::in, 
		list(prog_var)::in, pd_var_info::in, pd_var_info::out) is det.

pd_util__convert_branch_info_2([], _, Info, Info).
pd_util__convert_branch_info_2([ArgNo - Branches | ArgInfos], Args, 
		Info0, Info) :-
	list__index1_det(Args, ArgNo, Arg),
	map__set(Info0, Arg, Branches, Info1),
	pd_util__convert_branch_info_2(ArgInfos, Args, Info1, Info).	

%-----------------------------------------------------------------------------%

:- type pd_var_info 	==	branch_info_map(prog_var).

	% Find out which arguments of the procedure are interesting
	% for deforestation.
pd_util__get_branch_vars_proc(PredProcId, ProcInfo, 
		Info0, Info, ModuleInfo0, ModuleInfo) :-
	proc_info_goal(ProcInfo, Goal),
	proc_info_vartypes(ProcInfo, VarTypes),
	instmap__init_reachable(InstMap0),
	map__init(Vars0),
	set__init(LeftVars0),
	goal_to_conj_list(Goal, GoalList),
	(
		pd_util__get_branch_vars_goal_2(ModuleInfo0, GoalList, no, 
			VarTypes, InstMap0, LeftVars0, LeftVars, Vars0, Vars)
	->
		proc_info_headvars(ProcInfo, HeadVars),
		map__init(ThisProcArgMap0),
		set__init(ThisProcLeftArgs0),
		pd_util__get_extra_info_headvars(HeadVars, 1, LeftVars, Vars, 
			ThisProcArgMap0, ThisProcArgMap1, 
			ThisProcLeftArgs0, ThisProcLeftArgs),
		set__init(OpaqueArgs0),
		BranchInfo0 = pd_branch_info(ThisProcArgMap1, 
				ThisProcLeftArgs, OpaqueArgs0),
		map__set(Info0, PredProcId, BranchInfo0, Info1),

			% Look for opportunities for deforestation in 
			% the sub-branches of the top-level goal.
		pd_util__get_sub_branch_vars_goal(ModuleInfo0, Info1,
			GoalList, VarTypes, InstMap0, Vars, AllVars, ModuleInfo),
		pd_util__get_extra_info_headvars(HeadVars, 1, LeftVars0,
			AllVars, ThisProcArgMap0, ThisProcArgMap, 
			ThisProcLeftArgs0, _),

		proc_info_argmodes(ProcInfo, ArgModes),
		pd_util__get_opaque_args(ModuleInfo, 1, ArgModes, 
			ThisProcArgMap, OpaqueArgs0, OpaqueArgs),

		BranchInfo = pd_branch_info(ThisProcArgMap, ThisProcLeftArgs,
				OpaqueArgs),
		map__set(Info1, PredProcId, BranchInfo, Info)
	;
		ModuleInfo = ModuleInfo0,
		Info = Info0
	).

	% Find output arguments about which we have no extra information,
	% such as io__states. If a later goal in a conjunction depends
	% on one of these, it is unlikely that the deforestation will
	% be able to successfully fold to give a recursive definition.
:- pred pd_util__get_opaque_args(module_info::in, int::in, list(mode)::in,
		branch_info_map(int)::in, set(int)::in, set(int)::out) is det.

pd_util__get_opaque_args(_, _, [], _, OpaqueArgs, OpaqueArgs).
pd_util__get_opaque_args(ModuleInfo, ArgNo, [ArgMode | ArgModes],
		ExtraInfoArgs, OpaqueArgs0, OpaqueArgs) :-
	( 
		mode_is_output(ModuleInfo, ArgMode),
		\+ map__contains(ExtraInfoArgs, ArgNo)
	->
		set__insert(OpaqueArgs0, ArgNo, OpaqueArgs1)
	;
		OpaqueArgs1 = OpaqueArgs0
	),
	NextArg is ArgNo + 1,
	pd_util__get_opaque_args(ModuleInfo, NextArg, ArgModes,
		ExtraInfoArgs, OpaqueArgs1, OpaqueArgs).

	% From the information about variables for which we have extra
	% information in the branches, compute the argument numbers
	% for which we have extra information.
:- pred pd_util__get_extra_info_headvars(list(prog_var)::in, int::in,
		set(prog_var)::in, pd_var_info::in, 
		branch_info_map(int)::in, branch_info_map(int)::out, 
		set(int)::in, set(int)::out) is det.

pd_util__get_extra_info_headvars([], _, _, _, Args, Args, LeftArgs, LeftArgs).
pd_util__get_extra_info_headvars([HeadVar | HeadVars], ArgNo, 
		LeftVars, VarInfo, ThisProcArgs0, ThisProcArgs,
		ThisProcLeftVars0, ThisProcLeftVars) :-
	( map__search(VarInfo, HeadVar, ThisVarInfo) ->
		map__det_insert(ThisProcArgs0, ArgNo,
			ThisVarInfo, ThisProcArgs1)
	;
		ThisProcArgs1 = ThisProcArgs0
	),
	( set__member(HeadVar, LeftVars) ->
		set__insert(ThisProcLeftVars0, ArgNo, ThisProcLeftVars1)
	;
		ThisProcLeftVars1 = ThisProcLeftVars0
	),
	NextArgNo is ArgNo + 1,
	pd_util__get_extra_info_headvars(HeadVars, NextArgNo,
		LeftVars, VarInfo, ThisProcArgs1, ThisProcArgs, 
		ThisProcLeftVars1, ThisProcLeftVars).

%-----------------------------------------------------------------------------%

pd_util__get_branch_vars_goal(Goal, MaybeBranchInfo) -->
	pd_info_get_module_info(ModuleInfo0),
	pd_info_get_instmap(InstMap0),
	pd_info_get_proc_arg_info(ProcArgInfo),
	pd_info_get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ set__init(LeftVars0) },
	{ map__init(Vars0) },
	(
		{ pd_util__get_branch_vars_goal_2(ModuleInfo0, [Goal], no, 
			VarTypes, InstMap0, LeftVars0, LeftVars, Vars0, Vars1) }
	->
		{ pd_util__get_sub_branch_vars_goal(ModuleInfo0, ProcArgInfo, 
			[Goal], VarTypes, InstMap0, Vars1, Vars, ModuleInfo) },
		pd_info_set_module_info(ModuleInfo),

			% OpaqueVars is only filled in for calls.
		{ set__init(OpaqueVars) },
		{ MaybeBranchInfo = yes(
			pd_branch_info(Vars, LeftVars, OpaqueVars)
		) }
	;
		{ MaybeBranchInfo = no }
	).

:- pred pd_util__get_branch_vars_goal_2(module_info::in, list(hlds_goal)::in, 
	bool::in, vartypes::in, instmap::in,
	set(prog_var)::in, set(prog_var)::out,
	pd_var_info::in, pd_var_info::out) is semidet.

pd_util__get_branch_vars_goal_2(_, [], yes, _, _, LeftVars, LeftVars, Vars, Vars).
pd_util__get_branch_vars_goal_2(ModuleInfo, [Goal | Goals], FoundBranch0,
		VarTypes, InstMap0, LeftVars0, LeftVars, Vars0, Vars) :-
	Goal = _ - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap),
	( pd_util__get_branch_instmap_deltas(Goal, InstMapDeltas) ->
		% Only look for goals with one top-level branched goal,
		% since deforestation of goals with more than one is
		% likely to be less productive.
		FoundBranch0 = no,
		pd_util__get_branch_vars(ModuleInfo, Goal,
			InstMapDeltas, InstMap, 1, Vars0, Vars1),
		pd_util__get_left_vars(Goal, LeftVars0, LeftVars1),
		FoundBranch = yes
	;
		Goal = GoalExpr - _,
		goal_is_atomic(GoalExpr),
		FoundBranch = FoundBranch0,
		Vars1 = Vars0,
		LeftVars1 = LeftVars0
	),
	pd_util__get_branch_vars_goal_2(ModuleInfo, Goals, FoundBranch, 
		VarTypes, InstMap, LeftVars1, LeftVars, Vars1, Vars).

:- pred pd_util__get_branch_instmap_deltas(hlds_goal::in, 
		list(instmap_delta)::out) is semidet.

pd_util__get_branch_instmap_deltas(Goal, [CondDelta, ThenDelta, ElseDelta]) :-
	Goal = if_then_else(_, _ - CondInfo, _ - ThenInfo,
		_ - ElseInfo, _) - _,
	goal_info_get_instmap_delta(CondInfo, CondDelta),
	goal_info_get_instmap_delta(ThenInfo, ThenDelta),
	goal_info_get_instmap_delta(ElseInfo, ElseDelta).
pd_util__get_branch_instmap_deltas(switch(_, _, Cases, _) - _,
		InstMapDeltas) :-
	GetCaseInstMapDelta =
		lambda([Case::in, InstMapDelta::out] is det, (
			Case = case(_, _ - CaseInfo),
			goal_info_get_instmap_delta(CaseInfo, InstMapDelta)
		)),
	list__map(GetCaseInstMapDelta, Cases, InstMapDeltas).
pd_util__get_branch_instmap_deltas(disj(Disjuncts, _) - _, InstMapDeltas) :-
	GetDisjunctInstMapDelta =
		lambda([Disjunct::in, InstMapDelta::out] is det, (
			Disjunct = _ - DisjInfo,
			goal_info_get_instmap_delta(DisjInfo, InstMapDelta)
		)),
	list__map(GetDisjunctInstMapDelta, Disjuncts, InstMapDeltas).


	% Get the variables for which we can do unfolding if the goals to
	% the left supply the top-level functor. Eventually this should
	% also check for if-then-elses with simple conditions.
:- pred pd_util__get_left_vars(hlds_goal::in, 
		set(prog_var)::in, set(prog_var)::out) is det.

pd_util__get_left_vars(Goal, Vars0, Vars) :-
	( Goal = switch(Var, _, _, _) - _ ->
		set__insert(Vars0, Var, Vars)
	;
		Vars = Vars0
	).

:- pred pd_util__get_branch_vars(module_info::in, hlds_goal::in, 
		list(instmap_delta)::in, instmap::in, int::in, 
		pd_var_info::in, pd_var_info::out) is semidet.
		
pd_util__get_branch_vars(_, _, [], _, _, Extra, Extra).
pd_util__get_branch_vars(ModuleInfo, Goal, [InstMapDelta | InstMapDeltas], 
		InstMap, BranchNo, ExtraVars0, ExtraVars) :-
	AddExtraInfoVars = 
	    lambda([ChangedVar::in, Vars0::in, Vars::out] is det, (
		(
			instmap__lookup_var(InstMap, ChangedVar, VarInst),
			instmap_delta_search_var(InstMapDelta, 
				ChangedVar, DeltaVarInst),
		    	inst_is_bound_to_functors(ModuleInfo, 
				DeltaVarInst, [_]),
		    	\+ inst_is_bound_to_functors(ModuleInfo, 
				VarInst, [_])
	    	->
			( map__search(Vars0, ChangedVar, Set0) ->
				set__insert(Set0, BranchNo, Set)
			;
				set__singleton_set(Set, BranchNo)
			),
			map__set(Vars0, ChangedVar, Set, Vars)
		;
			Vars = Vars0
		)
	    )),
	instmap_delta_changed_vars(InstMapDelta, ChangedVars),
	set__to_sorted_list(ChangedVars, ChangedVarsList),
	list__foldl(AddExtraInfoVars, ChangedVarsList, ExtraVars0, ExtraVars1),

		% We have extra information about a switched-on variable 
		% at the end of each branch.
	( Goal = switch(SwitchVar, _, _, _) - _ ->
		( map__search(ExtraVars1, SwitchVar, SwitchVarSet0) ->
			set__insert(SwitchVarSet0, BranchNo, SwitchVarSet)
		;
			set__singleton_set(SwitchVarSet, BranchNo)
		),
		map__set(ExtraVars1, SwitchVar, SwitchVarSet, ExtraVars2)
	;
		ExtraVars2 = ExtraVars1
	),
	NextBranch is BranchNo + 1,
	pd_util__get_branch_vars(ModuleInfo, Goal, InstMapDeltas, InstMap, 
		NextBranch, ExtraVars2, ExtraVars).

	% Look at the goals in the branches for extra information.
:- pred pd_util__get_sub_branch_vars_goal(module_info::in, pd_arg_info::in,
		list(hlds_goal)::in, vartypes::in, instmap::in,
		branch_info_map(prog_var)::in, branch_info_map(prog_var)::out,
		module_info::out) is det.

pd_util__get_sub_branch_vars_goal(Module, _, [], _, _, Vars, Vars, Module).
pd_util__get_sub_branch_vars_goal(ModuleInfo0, ProcArgInfo, [Goal | GoalList], 
		VarTypes, InstMap0, Vars0, SubVars, ModuleInfo) :-
	Goal = GoalExpr - GoalInfo,
	( GoalExpr = if_then_else(_, Cond, Then, Else, _) ->
		Cond = _ - CondInfo,
		goal_info_get_instmap_delta(CondInfo, CondDelta),
		instmap__apply_instmap_delta(InstMap0, CondDelta, InstMap1),
		goal_to_conj_list(Then, ThenList),
		pd_util__examine_branch(ModuleInfo0, ProcArgInfo, 1, ThenList,
			VarTypes, InstMap1, Vars0, Vars1),
		goal_to_conj_list(Else, ElseList),
		pd_util__examine_branch(ModuleInfo0, ProcArgInfo, 2, ElseList,
			VarTypes, InstMap0, Vars1, Vars2),
		ModuleInfo1 = ModuleInfo0
	; GoalExpr = disj(Goals, _) ->
		pd_util__examine_branch_list(ModuleInfo0, ProcArgInfo, 
			1, Goals, VarTypes, InstMap0, Vars0, Vars2),
		ModuleInfo1 = ModuleInfo0
	; GoalExpr = switch(Var, _, Cases, _) ->
		pd_util__examine_case_list(ModuleInfo0, ProcArgInfo, 1, Var,
			Cases, VarTypes, InstMap0, Vars0, Vars2, ModuleInfo1)
	;
		ModuleInfo1 = ModuleInfo0,
		Vars2 = Vars0
	),
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap),
	pd_util__get_sub_branch_vars_goal(ModuleInfo1, ProcArgInfo, GoalList,
		VarTypes, InstMap, Vars2, SubVars, ModuleInfo).

:- pred pd_util__examine_branch_list(module_info::in, pd_arg_info::in, int::in,
	list(hlds_goal)::in, vartypes::in, instmap::in,
	branch_info_map(prog_var)::in, branch_info_map(prog_var)::out) is det.

pd_util__examine_branch_list(_, _, _, [], _, _, Vars, Vars).
pd_util__examine_branch_list(ModuleInfo, ProcArgInfo, BranchNo, [Goal | Goals],
		VarTypes, InstMap, Vars0, Vars) :-
	goal_to_conj_list(Goal, GoalList),
	pd_util__examine_branch(ModuleInfo, ProcArgInfo, BranchNo, GoalList,
		VarTypes, InstMap, Vars0, Vars1),
	NextBranch is BranchNo + 1,
	pd_util__examine_branch_list(ModuleInfo, ProcArgInfo, NextBranch,
		Goals, VarTypes, InstMap, Vars1, Vars).

:- pred pd_util__examine_case_list(module_info::in, pd_arg_info::in, int::in,
	prog_var::in, list(case)::in, vartypes::in, instmap::in,
	branch_info_map(prog_var)::in, 
	branch_info_map(prog_var)::out, module_info::out) is det.

pd_util__examine_case_list(Module, _, _, _, [], _, _, Vars, Vars, Module).
pd_util__examine_case_list(ModuleInfo0, ProcArgInfo, BranchNo, Var,
		[case(ConsId, Goal) | Goals], VarTypes, InstMap, 
		Vars0, Vars, ModuleInfo) :-
	map__lookup(VarTypes, Var, Type),
	instmap__bind_var_to_functor(Var, Type, ConsId, InstMap, InstMap1, 
		ModuleInfo0, ModuleInfo1),
	goal_to_conj_list(Goal, GoalList),
	pd_util__examine_branch(ModuleInfo1, ProcArgInfo, BranchNo, GoalList,
		VarTypes, InstMap1, Vars0, Vars1),
	NextBranch is BranchNo + 1,
	pd_util__examine_case_list(ModuleInfo1, ProcArgInfo, NextBranch,
		Var, Goals, VarTypes, InstMap, Vars1, Vars, ModuleInfo).

:- pred pd_util__examine_branch(module_info::in, pd_arg_info::in, int::in,
		list(hlds_goal)::in, vartypes::in, instmap::in,
		branch_info_map(prog_var)::in, branch_info_map(prog_var)::out)
		is det.

pd_util__examine_branch(_, _, _, [], _, _, Vars, Vars).
pd_util__examine_branch(ModuleInfo, ProcArgInfo, BranchNo, 
		[Goal | Goals], VarTypes, InstMap, Vars0, Vars) :-
	( Goal = call(PredId, ProcId, Args, _, _, _) - _ ->
		( 
			map__search(ProcArgInfo, proc(PredId, ProcId), 
				ThisProcArgInfo) 
		->
			pd_util__convert_branch_info(ThisProcArgInfo, 
				Args, BranchInfo),
			BranchInfo = pd_branch_info(Vars1, _, _),
			map__keys(Vars1, ExtraVars1),
			combine_vars(Vars0, BranchNo, ExtraVars1, Vars3)
		;
			Vars3 = Vars0
		)
	; 
		set__init(LeftVars0),
		map__init(Vars1),
		pd_util__get_branch_vars_goal_2(ModuleInfo, [Goal], no, 
			VarTypes, InstMap, LeftVars0, _, Vars1, Vars2)
	->
		map__keys(Vars2, ExtraVars2),
		combine_vars(Vars0, BranchNo, ExtraVars2, Vars3)
	;	
		Vars3 = Vars0
	),
	Goal = _ - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap, InstMapDelta, InstMap1),
	pd_util__examine_branch(ModuleInfo, ProcArgInfo, BranchNo,
		Goals, VarTypes, InstMap1, Vars3, Vars).

:- pred combine_vars(branch_info_map(prog_var)::in, int::in, list(prog_var)::in,
		branch_info_map(prog_var)::out) is det.

combine_vars(Vars, _, [], Vars).
combine_vars(Vars0, BranchNo, [ExtraVar | ExtraVars], Vars) :-
	( map__search(Vars0, ExtraVar, Branches0) ->
		set__insert(Branches0, BranchNo, Branches),
		map__det_update(Vars0, ExtraVar, Branches, Vars1)
	;
		set__singleton_set(Branches, BranchNo),
		map__det_insert(Vars0, ExtraVar, Branches, Vars1)
	),
	combine_vars(Vars1, BranchNo, ExtraVars, Vars).

%-----------------------------------------------------------------------------%

pd_util__requantify_goal(Goal0, NonLocals, Goal) -->
	pd_info_get_proc_info(ProcInfo0),
	{ proc_info_varset(ProcInfo0, VarSet0) },
	{ proc_info_vartypes(ProcInfo0, VarTypes0) },
	{ implicitly_quantify_goal(Goal0, VarSet0, VarTypes0, NonLocals,
		Goal, VarSet, VarTypes, _) },
	{ proc_info_set_varset(ProcInfo0, VarSet, ProcInfo1) },
	{ proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo) },
	pd_info_set_proc_info(ProcInfo).

pd_util__recompute_instmap_delta(Goal0, Goal) -->
	pd_info_get_module_info(ModuleInfo0),
	pd_info_get_instmap(InstMap),
	pd_info_get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ proc_info_inst_varset(ProcInfo, InstVarSet) },
	{ recompute_instmap_delta(yes, Goal0, Goal, VarTypes, InstVarSet,
		InstMap, ModuleInfo0, ModuleInfo) },
	pd_info_set_module_info(ModuleInfo).

%-----------------------------------------------------------------------------%

	% inst_MSG(InstA, InstB, InstC):
	%       The information in InstC is the minimum of the
	%       information in InstA and InstB.  Where InstA and
	%       InstB specify a binding (free or bound), it must be
	%       the same in both.
	% 	Round off bindings to different constructors to ground.
	%	When in doubt, fail. This will only result in less 
	% 	optimization, not loss of correctness.

inst_MSG(InstA, InstB, ModuleInfo, Inst) :-
	set__init(Expansions),
	inst_MSG_1(InstA, InstB, Expansions, ModuleInfo, Inst).

:- type expansions == set(pair(inst)).

:- pred inst_MSG_1(inst, inst, expansions, module_info, inst).
:- mode inst_MSG_1(in, in, in, in, out) is semidet.

inst_MSG_1(InstA, InstB, Expansions, ModuleInfo, Inst) :-
	( InstA = InstB ->
		Inst = InstA
	;
		% We don't do recursive MSGs (we could,
		% but it's probably not worth it).
		\+ set__member(InstA - InstB, Expansions),
		inst_expand(ModuleInfo, InstA, InstA2),
		inst_expand(ModuleInfo, InstB, InstB2),
		set__insert(Expansions, InstA - InstB, Expansions1),
		( InstB2 = not_reached ->
			Inst = InstA2
		;
			inst_MSG_2(InstA2, InstB2, Expansions1,
				ModuleInfo, Inst)
		)
	).

:- pred inst_MSG_2(inst, inst, expansions, module_info, inst).
:- mode inst_MSG_2(in, in, in, in, out) is semidet.

inst_MSG_2(any(_), any(Uniq), _, _, any(Uniq)).
inst_MSG_2(free, free, _M, _, free).

inst_MSG_2(bound(_, ListA), bound(UniqB, ListB), Expansions,
		ModuleInfo, Inst) :-
	bound_inst_list_MSG(ListA, ListB, Expansions,
		ModuleInfo, UniqB, ListB, Inst).
inst_MSG_2(bound(_, _), ground(UniqB, InfoB), _, _, ground(UniqB, InfoB)).

	% fail here, since the increasing inst size could 
	% cause termination problems for deforestation.
inst_MSG_2(ground(_, _), bound(_UniqB, _ListB), _, _, _) :- fail.
inst_MSG_2(ground(_, _), ground(UniqB, InfoB), _, _, ground(UniqB, InfoB)). 
inst_MSG_2(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
		Expansions, ModuleInfo, abstract_inst(Name, Args)) :-
	inst_list_MSG(ArgsA, ArgsB, Expansions, ModuleInfo, Args).
inst_MSG_2(not_reached, Inst, _, _, Inst).

:- pred inst_list_MSG(list(inst), list(inst), expansions,
		module_info, list(inst)).
:- mode inst_list_MSG(in, in, in, in, out) is semidet.

inst_list_MSG([], [], _, _ModuleInfo, []).
inst_list_MSG([ArgA | ArgsA], [ArgB | ArgsB], Expansions,
		ModuleInfo, [Arg | Args]) :-
	inst_MSG_1(ArgA, ArgB, Expansions, ModuleInfo, Arg),
	inst_list_MSG(ArgsA, ArgsB, Expansions, ModuleInfo, Args).

	% bound_inst_list_MSG(Xs, Ys, ModuleInfo, Zs):
	% The two input lists Xs and Ys must already be sorted.
	% If any of the functors in Xs are not in Ys or vice
	% versa, the final inst is ground, unless either of the insts
	% contains any or the insts are the insts are not uniformly 
	% unique (or non-unique), in which case we fail, since 
	% the msg operation could introduce mode errors. 
	% Otherwise, the take the msg of the argument insts.

:- pred bound_inst_list_MSG(list(bound_inst), list(bound_inst),
		expansions, module_info, uniqueness, list(bound_inst), inst).
:- mode bound_inst_list_MSG(in, in, in, in, in, in, out) is semidet.

bound_inst_list_MSG(Xs, Ys, Expansions, ModuleInfo, Uniq, List, Inst) :-
	(
		Xs = [],
		Ys = []
	->
		Inst = bound(Uniq, [])
	;	
		Xs = [X | Xs1],
		Ys = [Y | Ys1],
		X = functor(ConsId, ArgsX),
		Y = functor(ConsId, ArgsY)
	->
		inst_list_MSG(ArgsX, ArgsY, Expansions, ModuleInfo, Args),
		Z = functor(ConsId, Args),
		bound_inst_list_MSG(Xs1, Ys1, Expansions,
			ModuleInfo, Uniq, List, Inst1),
		( Inst1 = bound(Uniq, Zs) ->
			Inst = bound(Uniq, [Z | Zs])
		;
			Inst = Inst1
		)
	;
		% Check that it's OK to round off the uniqueness information.
		( 
			Uniq = shared,
			inst_is_ground(ModuleInfo, bound(shared, List)),
			inst_is_not_partly_unique(ModuleInfo, 
				bound(shared, List))
		;
			Uniq = unique,
			inst_is_unique(ModuleInfo, bound(unique, List))
		),		
		\+ inst_contains_nonstandard_func_mode(bound(shared, List),
			ModuleInfo),
		Inst = ground(Uniq, none)
	).

%-----------------------------------------------------------------------------%

pd_util__inst_size(ModuleInfo, Inst, Size) :-
	set__init(Expansions),
	pd_util__inst_size_2(ModuleInfo, Inst, Expansions, Size).

:- pred pd_util__inst_size_2(module_info::in, (inst)::in,
		set(inst_name)::in, int::out) is det.

pd_util__inst_size_2(_, not_reached, _, 0).
pd_util__inst_size_2(_, any(_), _, 0).
pd_util__inst_size_2(_, free, _, 0).
pd_util__inst_size_2(_, free(_), _, 0).
pd_util__inst_size_2(_, ground(_, _), _, 0).
pd_util__inst_size_2(_, inst_var(_), _, 0).
pd_util__inst_size_2(_, abstract_inst(_, _), _, 0).
pd_util__inst_size_2(ModuleInfo, defined_inst(InstName), Expansions0, Size) :-
	( set__member(InstName, Expansions0) ->
		Size = 1
	;
		set__insert(Expansions0, InstName, Expansions),
		inst_lookup(ModuleInfo, InstName, Inst),
		pd_util__inst_size_2(ModuleInfo, Inst, Expansions, Size)
	).
pd_util__inst_size_2(ModuleInfo, bound(_, Functors), Expansions, Size) :-
	pd_util__bound_inst_size(ModuleInfo, Functors, Expansions, 1, Size).

:- pred pd_util__bound_inst_size(module_info::in, list(bound_inst)::in,
		set(inst_name)::in, int::in, int::out) is det.
		
pd_util__bound_inst_size(_, [], _, Size, Size).
pd_util__bound_inst_size(ModuleInfo, [functor(_, ArgInsts) | Insts],
		Expansions, Size0, Size) :-
	pd_util__inst_list_size(ModuleInfo, ArgInsts,
		Expansions, Size0, Size1),
	Size2 is Size1 + 1,
	pd_util__bound_inst_size(ModuleInfo, Insts, Expansions, Size2, Size).

pd_util__inst_list_size(ModuleInfo, Insts, Size) :-
	set__init(Expansions),
	pd_util__inst_list_size(ModuleInfo, Insts, Expansions, 0, Size).

:- pred pd_util__inst_list_size(module_info::in, list(inst)::in,
		set(inst_name)::in, int::in, int::out) is det.

pd_util__inst_list_size(_, [], _, Size, Size).
pd_util__inst_list_size(ModuleInfo, [Inst | Insts],
		Expansions, Size0, Size) :-
	pd_util__inst_size_2(ModuleInfo, Inst, Expansions, Size1),
	Size2 is Size0 + Size1,
	pd_util__inst_list_size(ModuleInfo, Insts, Expansions, Size2, Size).

%-----------------------------------------------------------------------------%

pd_util__goals_match(_ModuleInfo, OldGoal, OldArgs, OldArgTypes,
		NewGoal, NewVarTypes, OldNewRenaming, TypeSubn) :-

	goal_to_conj_list(OldGoal, OldGoalList),
	goal_to_conj_list(NewGoal, NewGoalList),
	map__init(OldNewRenaming0),
	pd_util__goals_match_2(OldGoalList, NewGoalList,
		OldNewRenaming0, OldNewRenaming),

	%
	% Check that the goal produces a superset of the outputs of the
	% version we are searching for. 
	%
	Search = lambda([K1::in, V1::out] is semidet,
			map__search(OldNewRenaming, K1, V1)),
	list__map(Search, OldArgs, NewArgs),
	NewGoal = _ - NewGoalInfo,
	goal_info_get_nonlocals(NewGoalInfo, NewNonLocals),
	set__delete_list(NewNonLocals, NewArgs, UnmatchedNonLocals),
	set__empty(UnmatchedNonLocals),
	
	% Check that argument types of NewGoal are subsumed by 
	% those of OldGoal.
	pd_util__collect_matching_arg_types(OldArgs, OldArgTypes, 
		OldNewRenaming, [], MatchingArgTypes),
	map__apply_to_list(NewArgs, NewVarTypes, NewArgTypes),
	type_list_subsumes(MatchingArgTypes, NewArgTypes, TypeSubn).

:- pred pd_util__collect_matching_arg_types(list(prog_var)::in, list(type)::in,
		map(prog_var, prog_var)::in,
		list(type)::in, list(type)::out) is det.

pd_util__collect_matching_arg_types([], [], _, Types0, Types) :-
	list__reverse(Types0, Types).
pd_util__collect_matching_arg_types([_|_], [], _, _, _) :-
	error("pd_util__collect_matching_arg_types").
pd_util__collect_matching_arg_types([], [_|_], _, _, _) :-
	error("pd_util__collect_matching_arg_types").
pd_util__collect_matching_arg_types([Arg | Args], [Type | Types], 
		Renaming, MatchingTypes0, MatchingTypes) :-
	( map__contains(Renaming, Arg) ->
		MatchingTypes1 = [Type | MatchingTypes0]
	;
		MatchingTypes1 = MatchingTypes0
	),
	pd_util__collect_matching_arg_types(Args, Types, 
		Renaming, MatchingTypes1, MatchingTypes).

	% Check that the shape of the goals matches, and that there 
	% is a mapping from the variables in the old goal to the
	% variables in the new goal.
:- pred pd_util__goals_match_2(list(hlds_goal)::in,
		list(hlds_goal)::in, map(prog_var, prog_var)::in,
		map(prog_var, prog_var)::out) is semidet.

pd_util__goals_match_2([], [], R, R).
pd_util__goals_match_2([OldGoal | OldGoals], [NewGoal | NewGoals],
		ONRenaming0, ONRenaming) :-	
	(
		(
			OldGoal = unify(_, _, _, OldUnification, _) - _,
			NewGoal = unify(_, _, _, NewUnification, _) - _,
			(
				OldUnification = simple_test(OldVar1, OldVar2),
				NewUnification = simple_test(NewVar1, NewVar2),
				OldArgs = [OldVar1, OldVar2],
				NewArgs = [NewVar1, NewVar2]
			;
				OldUnification = assign(OldVar1, OldVar2),
				NewUnification = assign(NewVar1, NewVar2),
				OldArgs = [OldVar1, OldVar2],
				NewArgs = [NewVar1, NewVar2]
			;
				OldUnification = construct(OldVar, ConsId, 
						OldArgs1, _, _, _, _),
				NewUnification = construct(NewVar, ConsId, 
						NewArgs1, _, _, _, _),
				OldArgs = [OldVar | OldArgs1],
				NewArgs = [NewVar | NewArgs1]
			;
				OldUnification = deconstruct(OldVar, ConsId,
							OldArgs1, _, _, _),
				NewUnification = deconstruct(NewVar, ConsId,
							NewArgs1, _, _, _),
				OldArgs = [OldVar | OldArgs1],
				NewArgs = [NewVar | NewArgs1]
			)	
		;
			OldGoal = call(PredId, ProcId, OldArgs, _, _, _) - _,
			NewGoal = call(PredId, ProcId, NewArgs, _, _, _) - _
		;
			% We don't need to check the modes here -
			% if the goals match and the insts of the argument
			% variables match, the modes of the call must
			% be the same.
			OldGoal = generic_call(OldGenericCall, OldArgs1,
					_, Det) - _,
			NewGoal = generic_call(NewGenericCall, NewArgs1,
					_, Det) - _,
			match_generic_call(OldGenericCall, NewGenericCall),
			goal_util__generic_call_vars(OldGenericCall,
				OldArgs0),
			goal_util__generic_call_vars(NewGenericCall,
				NewArgs0),
			list__append(OldArgs0, OldArgs1, OldArgs),
			list__append(NewArgs0, NewArgs1, NewArgs)
		)
	->
		assoc_list__from_corresponding_lists(OldArgs, 
			NewArgs, ONArgsList),
		MapInsert =
			lambda([KeyValue::in, Map0::in, Map::out] is semidet, (
				KeyValue = Key - Value,
				( map__search(Map0, Key, Value0) ->
					Value = Value0,
					Map = Map0
				;
					map__det_insert(Map0, Key, Value, Map)
				)
			)),
		list__foldl(MapInsert, ONArgsList, ONRenaming0, ONRenaming1)
	;
		(
			OldGoal = not(OldSubGoal) - _,
			NewGoal = not(NewSubGoal) - _
		;
			OldGoal = some(_, _, OldSubGoal) - _,
			NewGoal = some(_, _, NewSubGoal) - _
		)
	->
		goal_to_conj_list(OldSubGoal, OldSubGoalList),
		goal_to_conj_list(NewSubGoal, NewSubGoalList),
		pd_util__goals_match_2(OldSubGoalList, NewSubGoalList,
			ONRenaming0, ONRenaming1)
	;
		fail
	),
	pd_util__goals_match_2(OldGoals, NewGoals, 
		ONRenaming1, ONRenaming).

	% Check that two `generic_call' goals are equivalent.
:- pred match_generic_call(generic_call::in, generic_call::in) is semidet.

match_generic_call(higher_order(_, PredOrFunc, Arity),
		higher_order(_, PredOrFunc, Arity)).
match_generic_call(class_method(_, MethodNum, ClassId, CallId),
		class_method(_, MethodNum, ClassId, CallId)).
match_generic_call(aditi_builtin(Builtin1, CallId),
		aditi_builtin(Builtin2, CallId)) :-
	match_aditi_builtin(Builtin1, Builtin2).

	% Check that two `aditi_builtin' goals are equivalent.
:- pred match_aditi_builtin(aditi_builtin::in, aditi_builtin::in) is semidet.

	% The other fields are all implied by the pred_proc_id.
match_aditi_builtin(aditi_call(PredProcId, _, _, _),
		aditi_call(PredProcId, _, _, _)).
match_aditi_builtin(aditi_tuple_insert_delete(InsertDelete, PredId),
		aditi_tuple_insert_delete(InsertDelete, PredId)).
	% The syntax used does not change the result of the call.
match_aditi_builtin(aditi_insert_delete_modify(Op, PredId, _),
		aditi_insert_delete_modify(Op, PredId, _)).

%-----------------------------------------------------------------------------%

pd_util__can_reorder_goals(ModuleInfo, FullyStrict, EarlierGoal, LaterGoal) :-
	EarlierGoal = _ - EarlierGoalInfo,
	LaterGoal = _ - LaterGoalInfo,

	goal_info_get_determinism(EarlierGoalInfo, EarlierDetism),
	goal_info_get_determinism(LaterGoalInfo, LaterDetism),

	% Check that the reordering would not violate determinism
	% correctness by moving a goal out of a single solution context
	% by placing a goal which can fail after it.
	(
		determinism_components(EarlierDetism, can_fail, _)
	=>	
		\+ determinism_components(LaterDetism, _, at_most_many_cc)
	),

	% Impure goals cannot be reordered.
	\+ goal_info_is_impure(EarlierGoalInfo),
	\+ goal_info_is_impure(LaterGoalInfo),

	pd_util__reordering_maintains_termination(ModuleInfo, FullyStrict, 
		EarlierGoal, LaterGoal),

	%
	% Don't reorder the goals if the later goal depends
	% on the outputs of the current goal.
	%
	\+ goal_depends_on_goal(EarlierGoal, LaterGoal),

	%
	% Don't reorder the goals if the later goal changes the 
	% instantiatedness of any of the non-locals of the earlier
	% goal. This is necessary if the later goal clobbers any 
	% of the non-locals of the earlier goal, and avoids rerunning
	% full mode analysis in other cases.
	%
	\+ goal_depends_on_goal(LaterGoal, EarlierGoal).

:- pred goal_depends_on_goal(hlds_goal::in, hlds_goal::in) is semidet.

goal_depends_on_goal(_ - GoalInfo1, _ - GoalInfo2) :-
	goal_info_get_instmap_delta(GoalInfo1, InstmapDelta1),
	instmap_delta_changed_vars(InstmapDelta1, ChangedVars1),
	goal_info_get_nonlocals(GoalInfo2, NonLocals2),
	set__intersect(ChangedVars1, NonLocals2, Intersection),
	\+ set__empty(Intersection).

pd_util__reordering_maintains_termination(ModuleInfo, FullyStrict, 
		EarlierGoal, LaterGoal) :-
	EarlierGoal = _ - EarlierGoalInfo,
	LaterGoal = _ - LaterGoalInfo,

	goal_info_get_determinism(EarlierGoalInfo, EarlierDetism),
	determinism_components(EarlierDetism, EarlierCanFail, _),
	goal_info_get_determinism(LaterGoalInfo, LaterDetism),
	determinism_components(LaterDetism, LaterCanFail, _),

		% If --fully-strict was specified, don't convert 
		% (can_loop, can_fail) into (can_fail, can_loop). 
	( 
		FullyStrict = yes, 
		\+ code_aux__goal_cannot_loop(ModuleInfo, EarlierGoal)
	->
		LaterCanFail = cannot_fail
	;
		true
	),
		% Don't convert (can_fail, can_loop) into 
		% (can_loop, can_fail), since this could worsen 
		% the termination properties of the program.
	( EarlierCanFail = can_fail ->
		code_aux__goal_cannot_loop(ModuleInfo, LaterGoal)
	;
		true
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
