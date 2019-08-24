%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2012 University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File pd_util.m.
% Main author: stayl.
%
% Utility predicates for deforestation and partial evaluation.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.pd_util.
:- interface.

:- import_module check_hlds.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.simplify.
:- import_module check_hlds.simplify.simplify_tasks.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.pd_info.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % Pick out the pred_proc_ids of the calls in a list of atomic goals.
    %
:- pred goal_get_calls(hlds_goal::in, list(pred_proc_id)::out) is det.

    % Call constraint.m to transform a goal so that goals which
    % can fail are executed as early as possible.
    %
:- pred propagate_constraints(hlds_goal::in, hlds_goal::out,
    pd_info::in, pd_info::out) is det.

    % Apply simplify.m to the goal.
    %
:- pred pd_simplify_goal(simplify_tasks::in, hlds_goal::in,
    hlds_goal::out, pd_info::in, pd_info::out) is det.

    % Apply unique_modes.m to the goal.
    %
:- pred unique_modecheck_goal(hlds_goal::in, hlds_goal::out,
    list(mode_error_info)::out, pd_info::in, pd_info::out) is det.

    % Apply unique_modes.m to the goal.
    %
:- pred unique_modecheck_goal_live_vars(set_of_progvar::in,
    hlds_goal::in, hlds_goal::out, list(mode_error_info)::out,
    pd_info::in, pd_info::out) is det.

    % Find out which arguments of the procedure are interesting
    % for deforestation.
    %
:- pred get_branch_vars_proc(pred_proc_id::in, proc_info::in,
    module_info::in, module_info::out, pd_arg_info::in, pd_arg_info::out)
    is det.

    % Find out which variables of the goal are interesting
    % for deforestation.
    %
:- pred get_branch_vars_goal(hlds_goal::in,
    maybe(pd_branch_info(prog_var))::out, pd_info::in, pd_info::out)
    is det.

    % Recompute the non-locals of the goal.
    %
:- pred pd_requantify_goal(set_of_progvar::in,
    hlds_goal::in, hlds_goal::out, pd_info::in, pd_info::out) is det.

    % Apply mode_util.recompute_instmap_delta to the goal.
    %
:- pred pd_recompute_instmap_delta(hlds_goal::in, hlds_goal::out,
    pd_info::in, pd_info::out) is det.

    % Convert from information about the argument positions to
    % information about the argument variables.
    %
:- pred convert_branch_info(pd_branch_info(int)::in,
    prog_vars::in, pd_branch_info(prog_var)::out) is det.

    % inst_MSG(InstA, InstB, InstC):
    %
    % Take the most specific generalisation of two insts. The information
    % in InstC is the minimum of the information in InstA and InstB.
    % Where InstA and InstB specify a binding (free or bound), it must be
    % the same in both. The uniqueness of the final inst is taken from InstB.
    % The difference between inst_merge and inst_MSG is that the msg of
    % `bound([functor, []])' and `bound([another_functor, []])' is `ground'
    % rather than `bound([functor, another_functor])'. Also the msgs are not
    % tabled, so the module_info is not threaded through.
    % If an inst is "rounded off", it must not contain `any' insts and must be
    % completely unique or completely non-unique. This is used in
    % generalisation to avoid non-termination of deforestation -
    % InstA is the inst in an old version, we are taking the msg with
    % to avoid non-termination, InstB is the inst in the new version
    % we want to create.
    %
    % It is always safe for inst_MSG to fail - this will just result
    % in less optimization. Mode analysis should be run on the goal to check
    % that this doesn't introduce mode errors, since the information that was
    % removed may actually have been necessary for mode correctness.
    %
:- pred inst_MSG(mer_inst::in, mer_inst::in, module_info::in, mer_inst::out)
    is semidet.

    % Produce an estimate of the size of an inst, based on the number of nodes
    % in the inst. The inst is expanded down to the first repeat of an already
    % expanded inst_name.
    %
:- pred inst_size(module_info::in, mer_inst::in, int::out) is det.
:- pred inst_list_size(module_info::in, list(mer_inst)::in, int::out) is det.

    % goals_match(ModuleInfo, OldGoal, OldArgs, OldArgTypes,
    %   NewGoal, NewArgTypes, OldToNewVarRenaming, OldToNewTypeSubst):
    %
    % Check the shape of the goals, and return a mapping from variables
    % in the old goal to variables in the new and a substitution to apply
    % to the types. This only attempts to match `simple' lists of goals,
    % which contain only conj, some, not and atomic goals, since deforest.m
    % only attempts to optimize those types of conjunctions.
    %
:- pred goals_match(module_info::in, hlds_goal::in, prog_vars::in,
    list(mer_type)::in, hlds_goal::in, vartypes::in,
    map(prog_var, prog_var)::out, tsubst::out) is semidet.

    % pd_can_reorder_goals(ModuleInfo, FullyStrict, Goal1, Goal2).
    %
    % Two goals can be reordered if
    % - the goals are independent
    % - the goals are not impure
    % - any possible change in termination behaviour is allowed
    %   according to the semantics options.
    %
    % XXX use the intermodule-analysis framework here (and see if this
    %     version can be merged with the similarly named predicate in
    %     goal_util.m).
    %
:- pred pd_can_reorder_goals(module_info::in, bool::in,
    hlds_goal::in, hlds_goal::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_analysis.
:- import_module check_hlds.det_report.
:- import_module check_hlds.det_util.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_info.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.unique_modes.
:- import_module check_hlds.simplify.simplify_proc.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.instmap.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_type.
:- import_module transform_hlds.constraint.
:- import_module transform_hlds.pd_debug.

:- import_module assoc_list.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.

goal_get_calls(Goal0, CalledPreds) :-
    goal_to_conj_list(Goal0, GoalList),
    GetCalls =
        ( pred(Goal::in, CalledPred::out) is semidet :-
            Goal = hlds_goal(plain_call(PredId, ProcId, _, _, _, _), _),
            CalledPred = proc(PredId, ProcId)
        ),
    list.filter_map(GetCalls, GoalList, CalledPreds).

%-----------------------------------------------------------------------------%

propagate_constraints(!Goal, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    module_info_get_globals(ModuleInfo0, Globals),
    globals.lookup_bool_option(Globals, local_constraint_propagation,
        ConstraintProp),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    (
        ConstraintProp = yes,
        Goal0 = !.Goal,
        trace [io(!IO)] (
            pd_debug_message(DebugPD, "%% Propagating constraints\n", [], !IO),
            pd_debug_output_goal(!.PDInfo, "before constraints\n", Goal0, !IO)
        ),
        pd_info_get_proc_info(!.PDInfo, ProcInfo0),
        pd_info_get_instmap(!.PDInfo, InstMap),
        proc_info_get_vartypes(ProcInfo0, VarTypes0),
        proc_info_get_varset(ProcInfo0, VarSet0),
        constraint_info_init(ModuleInfo0, VarTypes0, VarSet0, InstMap, CInfo0),
        Goal0 = hlds_goal(_, GoalInfo0),
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        constraint.propagate_constraints_in_goal(!Goal, CInfo0, CInfo),
        constraint_info_deconstruct(CInfo, ModuleInfo, VarTypes, VarSet,
            Changed),
        pd_info_set_module_info(ModuleInfo, !PDInfo),
        proc_info_set_vartypes(VarTypes, ProcInfo0, ProcInfo1),
        proc_info_set_varset(VarSet, ProcInfo1, ProcInfo),
        pd_info_set_proc_info(ProcInfo, !PDInfo),
        (
            Changed = yes,
            trace [io(!IO)] (
                pd_debug_output_goal(!.PDInfo,
                    "after constraints, before recompute\n", !.Goal, !IO)
            ),
            pd_requantify_goal(NonLocals, !Goal, !PDInfo),
            pd_recompute_instmap_delta(!Goal, !PDInfo),
            rerun_det_analysis(!Goal, !PDInfo),
            find_simplify_tasks(no, Globals, SimplifyTasks),
            pd_simplify_goal(SimplifyTasks, !Goal, !PDInfo)
        ;
            % Use Goal0 rather than the output of propagate_constraints_in_goal
            % because constraint propagation can make the quantification
            % information more conservative even if it doesn't optimize
            % anything.
            Changed = no,
            !:Goal = Goal0
        )
    ;
        ConstraintProp = no
    ).

%-----------------------------------------------------------------------------%

pd_simplify_goal(SimplifyTasks, !Goal, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    pd_info_get_pred_proc_id(!.PDInfo, proc(PredId, ProcId)),
    pd_info_get_proc_info(!.PDInfo, ProcInfo0),
    pd_info_get_instmap(!.PDInfo, InstMap0),

    simplify_goal_update_vars_in_proc(SimplifyTasks, ModuleInfo0, ModuleInfo,
        PredId, ProcId, ProcInfo0, ProcInfo, InstMap0, !Goal, CostDelta),

    pd_info_set_module_info(ModuleInfo, !PDInfo),
    pd_info_set_proc_info(ProcInfo, !PDInfo),
    pd_info_incr_cost_delta(CostDelta, !PDInfo).

%-----------------------------------------------------------------------------%

unique_modecheck_goal(Goal0, Goal, Errors, !PDInfo) :-
    get_goal_live_vars(!.PDInfo, Goal0, LiveVars),
    unique_modecheck_goal_live_vars(LiveVars, Goal0, Goal, Errors, !PDInfo).

unique_modecheck_goal_live_vars(LiveVars, Goal0, Goal, Errors, !PDInfo) :-
    % Construct a mode_info.
    pd_info_get_pred_proc_id(!.PDInfo, PredProcId),
    PredProcId = proc(PredId, ProcId),
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    pd_info_get_instmap(!.PDInfo, InstMap0),
    term.context_init(Context),
    pd_info_get_pred_info(!.PDInfo, PredInfo0),
    pd_info_get_proc_info(!.PDInfo, ProcInfo0),
    pd_info_get_head_inst_vars(!.PDInfo, HeadInstVars),
    module_info_set_pred_proc_info(PredId, ProcId, PredInfo0, ProcInfo0,
        ModuleInfo0, ModuleInfo1),

    % If we perform generalisation, we shouldn't change any called procedures,
    % since that could cause a less efficient version to be chosen.
    MayChangeCalledProc = may_not_change_called_proc,
    mode_info_init(ModuleInfo1, PredId, ProcId, Context, LiveVars,
        HeadInstVars, InstMap0, check_unique_modes, MayChangeCalledProc,
        ModeInfo0),

    unique_modes_check_goal(Goal0, Goal, ModeInfo0, ModeInfo),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, Debug),
    mode_info_get_errors(ModeInfo, Errors),
    (
        Debug = yes,
        trace [io(!IO)] (
            ErrorSpecs = list.map(mode_error_info_to_spec(ModeInfo), Errors),
            write_error_specs_ignore(ErrorSpecs, Globals, !IO)
        )
    ;
        Debug = no
    ),

    % Deconstruct the mode_info.
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_get_var_types(ModeInfo, VarTypes),
    pd_info_set_module_info(ModuleInfo, !PDInfo),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo1),
    pd_info_set_pred_info(PredInfo, !PDInfo),
    proc_info_set_varset(VarSet, ProcInfo1, ProcInfo2),
    proc_info_set_vartypes(VarTypes, ProcInfo2, ProcInfo),
    pd_info_set_proc_info(ProcInfo, !PDInfo).

    % Work out which vars are live later in the computation based on
    % which of the non-local variables are not clobbered by the goal.
    %
:- pred get_goal_live_vars(pd_info::in, hlds_goal::in,
    set_of_progvar::out) is det.

get_goal_live_vars(PDInfo, hlds_goal(_, GoalInfo), !:Vars) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    pd_info_get_instmap(PDInfo, InstMap),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    set_of_var.to_sorted_list(NonLocals, NonLocalsList),
    set_of_var.init(!:Vars),
    get_goal_live_vars_2(ModuleInfo, NonLocalsList, InstMap, InstMapDelta,
        !Vars).

:- pred get_goal_live_vars_2(module_info::in, prog_vars::in,
    instmap::in, instmap_delta::in,
    set_of_progvar::in, set_of_progvar::out) is det.

get_goal_live_vars_2(_, [], _, _, !Vars).
get_goal_live_vars_2(ModuleInfo, [NonLocal | NonLocals],
        InstMap, InstMapDelta, !Vars) :-
    ( if instmap_delta_search_var(InstMapDelta, NonLocal, FinalInst0) then
        FinalInst = FinalInst0
    else
        instmap_lookup_var(InstMap, NonLocal, FinalInst)
    ),
    ( if inst_is_clobbered(ModuleInfo, FinalInst) then
        true
    else
        set_of_var.insert(NonLocal, !Vars)
    ),
    get_goal_live_vars_2(ModuleInfo, NonLocals, InstMap, InstMapDelta, !Vars).

%-----------------------------------------------------------------------------%

:- pred rerun_det_analysis(hlds_goal::in, hlds_goal::out,
    pd_info::in, pd_info::out) is det.

rerun_det_analysis(Goal0, Goal, !PDInfo) :-
    Goal0 = hlds_goal(_, GoalInfo0),

    Detism = goal_info_get_determinism(GoalInfo0),
    det_get_soln_context(Detism, SolnContext),

    % det_infer_goal looks up the proc_info in the module_info for the
    % vartypes, so we'd better stick them back in the module_info.
    pd_info_get_pred_proc_id(!.PDInfo, PredProcId),
    pd_info_get_pred_info(!.PDInfo, PredInfo),
    pd_info_get_proc_info(!.PDInfo, ProcInfo),
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    module_info_set_pred_proc_info(PredProcId, PredInfo, ProcInfo,
        ModuleInfo0, ModuleInfo1),

    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    det_info_init(ModuleInfo1, PredProcId, VarSet, VarTypes,
        pess_extra_vars_ignore, [], DetInfo0),
    pd_info_get_instmap(!.PDInfo, InstMap),
    det_infer_goal(Goal0, Goal, InstMap, SolnContext, [], no, _, _,
        DetInfo0, DetInfo),
    det_info_get_module_info(DetInfo, ModuleInfo),
    pd_info_set_module_info(ModuleInfo, !PDInfo),
    det_info_get_error_specs(DetInfo, Specs),

    % Make sure there were no errors.
    module_info_get_globals(ModuleInfo, Globals),
    disable_det_warnings(_OptionsToRestore, Globals, GlobalsToUse),
    ContainsErrors = contains_errors(GlobalsToUse, Specs),
    expect(unify(ContainsErrors, no), $pred, "determinism errors").

%-----------------------------------------------------------------------------%

convert_branch_info(ArgInfo, Args, VarInfo) :-
    ArgInfo = pd_branch_info(ArgMap, LeftArgs, OpaqueArgs),
    map.to_assoc_list(ArgMap, ArgList),
    map.init(BranchVarMap0),
    convert_branch_info_2(ArgList, Args, BranchVarMap0, BranchVarMap),

    set.to_sorted_list(LeftArgs, LeftArgNos),
    list.map(list.det_index1(Args), LeftArgNos, LeftVars0),
    set.list_to_set(LeftVars0, LeftVars),

    set.to_sorted_list(OpaqueArgs, OpaqueArgNos),
    list.map(list.det_index1(Args), OpaqueArgNos, OpaqueVars0),
    set.list_to_set(OpaqueVars0, OpaqueVars),

    VarInfo = pd_branch_info(BranchVarMap, LeftVars, OpaqueVars).

:- pred convert_branch_info_2(assoc_list(int, set(int))::in,
    prog_vars::in, pd_var_info::in, pd_var_info::out) is det.

convert_branch_info_2([], _, !VarInfo).
convert_branch_info_2([ArgNo - Branches | ArgInfos], Args, !VarInfo) :-
    list.det_index1(Args, ArgNo, Arg),
    map.set(Arg, Branches, !VarInfo),
    convert_branch_info_2(ArgInfos, Args, !VarInfo).

%-----------------------------------------------------------------------------%

:- type pd_var_info ==  branch_info_map(prog_var).

get_branch_vars_proc(PredProcId, ProcInfo, !ModuleInfo, !ArgInfo) :-
    proc_info_get_goal(ProcInfo, Goal),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    instmap.init_reachable(InstMap0),
    map.init(Vars0),
    set_of_var.init(LeftVars0),
    goal_to_conj_list(Goal, GoalList),
    ( if
        get_branch_vars_goal_2(!.ModuleInfo, GoalList, no,
            VarTypes, InstMap0, LeftVars0, LeftVars, Vars0, Vars)
    then
        proc_info_get_headvars(ProcInfo, HeadVars),
        map.init(ThisProcArgMap0),
        set.init(ThisProcLeftArgs0),
        get_extra_info_headvars(HeadVars, 1, LeftVars, Vars,
            ThisProcArgMap0, ThisProcArgMap1,
            ThisProcLeftArgs0, ThisProcLeftArgs),
        set.init(OpaqueArgs0),
        BranchInfo0 = pd_branch_info(ThisProcArgMap1, ThisProcLeftArgs,
            OpaqueArgs0),
        map.set(PredProcId, BranchInfo0, !ArgInfo),

        % Look for opportunities for deforestation in the sub-branches
        % of the top-level goal.
        get_sub_branch_vars_goal(!.ArgInfo, GoalList,
            VarTypes, InstMap0, Vars, AllVars, !ModuleInfo),
        get_extra_info_headvars(HeadVars, 1, LeftVars0,
            AllVars, ThisProcArgMap0, ThisProcArgMap, ThisProcLeftArgs0, _),

        proc_info_get_argmodes(ProcInfo, ArgModes),
        get_opaque_args(!.ModuleInfo, 1, ArgModes,
            ThisProcArgMap, OpaqueArgs0, OpaqueArgs),

        BranchInfo = pd_branch_info(ThisProcArgMap, ThisProcLeftArgs,
            OpaqueArgs),
        map.set(PredProcId, BranchInfo, !ArgInfo)
    else
        true
    ).

    % Find output arguments about which we have no extra information,
    % such as io.states. If a later goal in a conjunction depends
    % on one of these, it is unlikely that the deforestation will
    % be able to successfully fold to give a recursive definition.
    %
:- pred get_opaque_args(module_info::in, int::in, list(mer_mode)::in,
    branch_info_map(int)::in, set(int)::in, set(int)::out) is det.

get_opaque_args(_, _, [], _, !OpaqueArgs).
get_opaque_args(ModuleInfo, ArgNo, [ArgMode | ArgModes],
        ExtraInfoArgs, !OpaqueArgs) :-
    ( if
        mode_is_output(ModuleInfo, ArgMode),
        not map.contains(ExtraInfoArgs, ArgNo)
    then
        set.insert(ArgNo, !OpaqueArgs)
    else
        true
    ),
    NextArg = ArgNo + 1,
    get_opaque_args(ModuleInfo, NextArg, ArgModes,
        ExtraInfoArgs, !OpaqueArgs).

    % From the information about variables for which we have extra information
    % in the branches, compute the argument numbers for which we have extra
    % information.
    %
:- pred get_extra_info_headvars(prog_vars::in, int::in,
    set_of_progvar::in, pd_var_info::in,
    branch_info_map(int)::in, branch_info_map(int)::out,
    set(int)::in, set(int)::out) is det.

get_extra_info_headvars([], _, _, _, !Args, !LeftArgs).
get_extra_info_headvars([HeadVar | HeadVars], ArgNo,
        LeftVars, VarInfo, !ThisProcArgs, !ThisProcLeftVars) :-
    ( if map.search(VarInfo, HeadVar, ThisVarInfo) then
        map.det_insert(ArgNo, ThisVarInfo, !ThisProcArgs)
    else
        true
    ),
    ( if set_of_var.member(LeftVars, HeadVar) then
        set.insert(ArgNo, !ThisProcLeftVars)
    else
        true
    ),
    NextArgNo = ArgNo + 1,
    get_extra_info_headvars(HeadVars, NextArgNo,
        LeftVars, VarInfo, !ThisProcArgs, !ThisProcLeftVars).

%-----------------------------------------------------------------------------%

get_branch_vars_goal(Goal, MaybeBranchInfo, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    pd_info_get_instmap(!.PDInfo, InstMap0),
    pd_info_get_proc_arg_info(!.PDInfo, ProcArgInfo),
    pd_info_get_proc_info(!.PDInfo, ProcInfo),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    set_of_var.init(LeftVars0),
    map.init(Vars0),
    ( if
        get_branch_vars_goal_2(ModuleInfo0, [Goal], no,
            VarTypes, InstMap0, LeftVars0, LeftVars, Vars0, Vars1)
    then
        get_sub_branch_vars_goal(ProcArgInfo, [Goal],
            VarTypes, InstMap0, Vars1, Vars,
            ModuleInfo0, ModuleInfo),
        pd_info_set_module_info(ModuleInfo, !PDInfo),

        % OpaqueVars is only filled in for calls.
        set.init(OpaqueVars),
        MaybeBranchInfo = yes(pd_branch_info(Vars,
            set_of_var.bitset_to_set(LeftVars), OpaqueVars))
    else
        MaybeBranchInfo = no
    ).

:- pred get_branch_vars_goal_2(module_info::in, hlds_goals::in,
    bool::in, vartypes::in, instmap::in,
    set_of_progvar::in, set_of_progvar::out,
    pd_var_info::in, pd_var_info::out) is semidet.

get_branch_vars_goal_2(_, [], yes, _, _, !LeftVars, !Vars).
get_branch_vars_goal_2(ModuleInfo, [Goal | Goals], !.FoundBranch,
        VarTypes, InstMap0, !LeftVars, !Vars) :-
    Goal = hlds_goal(_, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, InstMap0, InstMap),
    ( if get_branch_instmap_deltas(Goal, InstMapDeltas) then
        % Only look for goals with one top-level branched goal,
        % since deforestation of goals with more than one is
        % likely to be less productive.
        !.FoundBranch = no,
        get_branch_vars(ModuleInfo, Goal, InstMapDeltas, InstMap,
            1, !Vars),
        get_left_vars(Goal, !LeftVars),
        !:FoundBranch = yes
    else
        Goal = hlds_goal(GoalExpr, _),
        goal_expr_has_subgoals(GoalExpr) = does_not_have_subgoals
    ),
    get_branch_vars_goal_2(ModuleInfo, Goals, !.FoundBranch,
        VarTypes, InstMap, !LeftVars, !Vars).

:- pred get_branch_instmap_deltas(hlds_goal::in, list(instmap_delta)::out)
    is semidet.

get_branch_instmap_deltas(Goal, InstMapDeltas) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        GoalExpr = if_then_else(_, Cond, Then, Else),
        Cond = hlds_goal(_, CondInfo),
        Then = hlds_goal(_, ThenInfo),
        Else = hlds_goal(_, ElseInfo),
        CondDelta = goal_info_get_instmap_delta(CondInfo),
        ThenDelta = goal_info_get_instmap_delta(ThenInfo),
        ElseDelta = goal_info_get_instmap_delta(ElseInfo),
        InstMapDeltas = [CondDelta, ThenDelta, ElseDelta]
    ;
        GoalExpr = switch(_, _, Cases),
        GetCaseInstMapDelta =
            ( pred(Case::in, InstMapDelta::out) is det :-
                Case = case(_, _, hlds_goal(_, CaseInfo)),
                InstMapDelta = goal_info_get_instmap_delta(CaseInfo)
            ),
        list.map(GetCaseInstMapDelta, Cases, InstMapDeltas)
    ;
        GoalExpr = disj(Disjuncts),
        GetDisjunctInstMapDelta =
            ( pred(Disjunct::in, InstMapDelta::out) is det :-
                Disjunct = hlds_goal(_, DisjInfo),
                InstMapDelta = goal_info_get_instmap_delta(DisjInfo)
            ),
        list.map(GetDisjunctInstMapDelta, Disjuncts, InstMapDeltas)
    ).

    % Get the variables for which we can do unfolding if the goals to
    % the left supply the top-level functor. Eventually this should
    % also check for if-then-elses with simple conditions.
    %
:- pred get_left_vars(hlds_goal::in,
    set_of_progvar::in, set_of_progvar::out) is det.

get_left_vars(Goal, Vars0, Vars) :-
    ( if Goal = hlds_goal(switch(Var, _, _), _) then
        set_of_var.insert(Var, Vars0, Vars)
    else
        Vars = Vars0
    ).

:- pred get_branch_vars(module_info::in, hlds_goal::in,
    list(instmap_delta)::in, instmap::in, int::in,
    pd_var_info::in, pd_var_info::out) is semidet.

get_branch_vars(_, _, [], _, _, !ExtraVars).
get_branch_vars(ModuleInfo, Goal, [InstMapDelta | InstMapDeltas],
        InstMap, BranchNo, !ExtraVars) :-
    AddExtraInfoVars =
        ( pred(ChangedVar::in, Vars0::in, Vars::out) is det :-
            ( if
                instmap_lookup_var(InstMap, ChangedVar, VarInst),
                instmap_delta_search_var(InstMapDelta, ChangedVar,
                    DeltaVarInst),
                inst_is_bound_to_functors(ModuleInfo, DeltaVarInst, [_]),
                not inst_is_bound_to_functors(ModuleInfo, VarInst, [_])
            then
                ( if map.search(Vars0, ChangedVar, Set0) then
                    set.insert(BranchNo, Set0, Set)
                else
                    Set = set.make_singleton_set(BranchNo)
                ),
                map.set(ChangedVar, Set, Vars0, Vars)
            else
                Vars = Vars0
            )
        ),
    instmap_delta_changed_vars(InstMapDelta, ChangedVars),
    set_of_var.to_sorted_list(ChangedVars, ChangedVarsList),
    list.foldl(AddExtraInfoVars, ChangedVarsList, !ExtraVars),

    % We have extra information about a switched-on variable
    % at the end of each branch.
    ( if Goal = hlds_goal(switch(SwitchVar, _, _), _) then
        ( if map.search(!.ExtraVars, SwitchVar, SwitchVarSet0) then
            set.insert(BranchNo, SwitchVarSet0, SwitchVarSet)
        else
            SwitchVarSet = set.make_singleton_set(BranchNo)
        ),
        map.set(SwitchVar, SwitchVarSet, !ExtraVars)
    else
        true
    ),
    NextBranch = BranchNo + 1,
    get_branch_vars(ModuleInfo, Goal, InstMapDeltas, InstMap,
        NextBranch, !ExtraVars).

    % Look at the goals in the branches for extra information.
    %
:- pred get_sub_branch_vars_goal(pd_arg_info::in,
    hlds_goals::in, vartypes::in, instmap::in,
    branch_info_map(prog_var)::in, branch_info_map(prog_var)::out,
    module_info::in, module_info::out) is det.

get_sub_branch_vars_goal(_, [], _, _, Vars, Vars, !Module).
get_sub_branch_vars_goal(ProcArgInfo, [Goal | GoalList],
        VarTypes, InstMap0, !.Vars, SubVars, !ModuleInfo) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = if_then_else(_, Cond, Then, Else),
        Cond = hlds_goal(_, CondInfo),
        CondDelta = goal_info_get_instmap_delta(CondInfo),
        apply_instmap_delta(CondDelta, InstMap0, InstMap1),
        goal_to_conj_list(Then, ThenList),
        examine_branch(!.ModuleInfo, ProcArgInfo, 1, ThenList,
            VarTypes, InstMap1, !Vars),
        goal_to_conj_list(Else, ElseList),
        examine_branch(!.ModuleInfo, ProcArgInfo, 2, ElseList,
            VarTypes, InstMap0, !Vars)
    ;
        GoalExpr = disj(Goals),
        examine_branch_list(!.ModuleInfo, ProcArgInfo,
            1, Goals, VarTypes, InstMap0, !Vars)
    ;
        GoalExpr = switch(Var, _, Cases),
        examine_case_list(ProcArgInfo, 1, Var,
            Cases, VarTypes, InstMap0, !Vars, !ModuleInfo)
    ;
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = conj(_, _)
        ; GoalExpr = negation(_)
        ; GoalExpr = scope(_, _)
        )
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, InstMap0, InstMap),
    get_sub_branch_vars_goal(ProcArgInfo, GoalList,
        VarTypes, InstMap, !.Vars, SubVars, !ModuleInfo).

:- pred examine_branch_list(module_info::in, pd_arg_info::in, int::in,
    hlds_goals::in, vartypes::in, instmap::in,
    branch_info_map(prog_var)::in, branch_info_map(prog_var)::out) is det.

examine_branch_list(_, _, _, [], _, _, !Vars).
examine_branch_list(ModuleInfo, ProcArgInfo, BranchNo, [Goal | Goals],
        VarTypes, InstMap, !Vars) :-
    goal_to_conj_list(Goal, GoalList),
    examine_branch(ModuleInfo, ProcArgInfo, BranchNo, GoalList,
        VarTypes, InstMap, !Vars),
    NextBranch = BranchNo + 1,
    examine_branch_list(ModuleInfo, ProcArgInfo, NextBranch,
        Goals, VarTypes, InstMap, !Vars).

:- pred examine_case_list(pd_arg_info::in, int::in, prog_var::in,
    list(case)::in, vartypes::in, instmap::in,
    branch_info_map(prog_var)::in, branch_info_map(prog_var)::out,
    module_info::in, module_info::out) is det.

examine_case_list(_, _, _, [], _, _, !Vars, !ModuleInfo).
examine_case_list(ProcArgInfo, BranchNo, Var, [Case | Cases],
        VarTypes, InstMap0, !Vars, !ModuleInfo) :-
    lookup_var_type(VarTypes, Var, Type),
    Case = case(MainConsId, OtherConsIds, Goal),
    bind_var_to_functors(Var, Type, MainConsId, OtherConsIds,
        InstMap0, InstMap1, !ModuleInfo),
    goal_to_conj_list(Goal, GoalList),
    examine_branch(!.ModuleInfo, ProcArgInfo, BranchNo, GoalList,
        VarTypes, InstMap1, !Vars),
    NextBranch = BranchNo + 1,
    examine_case_list(ProcArgInfo, NextBranch, Var, Cases,
        VarTypes, InstMap0, !Vars, !ModuleInfo).

:- pred examine_branch(module_info::in, pd_arg_info::in, int::in,
    hlds_goals::in, vartypes::in, instmap::in,
    branch_info_map(prog_var)::in, branch_info_map(prog_var)::out) is det.

examine_branch(_, _, _, [], _, _, !Vars).
examine_branch(ModuleInfo, ProcArgInfo, BranchNo, [Goal | Goals],
        VarTypes, InstMap, !Vars) :-
    ( if
        Goal = hlds_goal(plain_call(PredId, ProcId, Args, _, _, _), _)
    then
        ( if
            map.search(ProcArgInfo, proc(PredId, ProcId), ThisProcArgInfo)
        then
            convert_branch_info(ThisProcArgInfo, Args, BranchInfo),
            BranchInfo = pd_branch_info(!:Vars, _, _),
            map.keys(!.Vars, ExtraVars1),
            combine_vars(BranchNo, ExtraVars1, !Vars)
        else
            true
        )
    else if
        set_of_var.init(LeftVars0),
        map.init(!:Vars),
        get_branch_vars_goal_2(ModuleInfo, [Goal], no,
            VarTypes, InstMap, LeftVars0, _, !Vars)
    then
        map.keys(!.Vars, ExtraVars2),
        combine_vars(BranchNo, ExtraVars2, !Vars)
    else
        true
    ),
    Goal = hlds_goal(_, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, InstMap, InstMap1),
    examine_branch(ModuleInfo, ProcArgInfo, BranchNo,
        Goals, VarTypes, InstMap1, !Vars).

:- pred combine_vars(int::in, prog_vars::in,
    branch_info_map(prog_var)::in, branch_info_map(prog_var)::out) is det.

combine_vars(_, [], !Vars).
combine_vars(BranchNo, [ExtraVar | ExtraVars], !Vars) :-
    ( if map.search(!.Vars, ExtraVar, Branches0) then
        set.insert(BranchNo, Branches0, Branches),
        map.det_update(ExtraVar, Branches, !Vars)
    else
        Branches = set.make_singleton_set(BranchNo),
        map.det_insert(ExtraVar, Branches, !Vars)
    ),
    combine_vars(BranchNo, ExtraVars, !Vars).

%-----------------------------------------------------------------------------%

pd_requantify_goal(NonLocals, Goal0, Goal, !PDInfo) :-
    some [!ProcInfo] (
        pd_info_get_proc_info(!.PDInfo, !:ProcInfo),
        proc_info_get_varset(!.ProcInfo, VarSet0),
        proc_info_get_vartypes(!.ProcInfo, VarTypes0),
        proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
        implicitly_quantify_goal_general(ordinary_nonlocals_no_lambda,
            NonLocals, _, Goal0, Goal, VarSet0, VarSet,
            VarTypes0, VarTypes, RttiVarMaps0, RttiVarMaps),
        proc_info_set_varset(VarSet, !ProcInfo),
        proc_info_set_vartypes(VarTypes, !ProcInfo),
        proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),
        pd_info_set_proc_info(!.ProcInfo, !PDInfo)
    ).

pd_recompute_instmap_delta(Goal0, Goal, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    pd_info_get_instmap(!.PDInfo, InstMap),
    pd_info_get_proc_info(!.PDInfo, ProcInfo),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    recompute_instmap_delta(recompute_atomic_instmap_deltas,
        Goal0, Goal, VarTypes, InstVarSet, InstMap, ModuleInfo0, ModuleInfo),
    pd_info_set_module_info(ModuleInfo, !PDInfo).

%-----------------------------------------------------------------------------%

inst_MSG(InstA, InstB, ModuleInfo, Inst) :-
    set.init(Expansions),
    inst_MSG_1(InstA, InstB, Expansions, ModuleInfo, Inst).

:- type expansions == set(pair(mer_inst)).

:- pred inst_MSG_1(mer_inst::in, mer_inst::in, expansions::in, module_info::in,
    mer_inst::out) is semidet.

inst_MSG_1(InstA, InstB, !.Expansions, ModuleInfo, Inst) :-
    ( if InstA = InstB then
        Inst = InstA
    else
        % We don't do recursive MSGs
        % (we could, but it's probably not worth it).
        not set.member(InstA - InstB, !.Expansions),
        inst_expand(ModuleInfo, InstA, InstA2),
        inst_expand(ModuleInfo, InstB, InstB2),
        set.insert(InstA - InstB, !Expansions),
        ( if InstB2 = not_reached then
            Inst = InstA2
        else
            inst_MSG_2(InstA2, InstB2, !.Expansions, ModuleInfo, Inst)
        )
    ).

:- pred inst_MSG_2(mer_inst::in, mer_inst::in, expansions::in, module_info::in,
    mer_inst::out) is semidet.

inst_MSG_2(InstA, InstB, Expansions, ModuleInfo, Inst) :-
    (
        InstA = not_reached,
        Inst = InstB
    ;
        InstA = free,
        InstB = free,
        Inst = free
    ;
        InstA = ground(_, _),
        InstB = ground(_, _),
        % XXX Not checking uniqueness seems wrong, and so is not recursing
        % on the contents of any higher order inst.
        Inst = InstB
    ;
        InstA = ground(_, _),
        InstB = bound(_, _, _),
        % Fail here, since the increasing inst size could cause
        % termination problems for deforestation.
        fail
    ;
        InstA = bound(_, _, _),
        InstB = ground(_, _),
        % XXX Not checking uniqueness seems wrong.
        Inst = InstB
    ;
        InstA = bound(_, _, BoundInstsA),
        InstB = bound(UniqB, _, BoundInstsB),
        % XXX Ignoring UniqA seems wrong.
        bound_inst_list_MSG(BoundInstsA, BoundInstsB, Expansions,
            ModuleInfo, UniqB, BoundInstsB, Inst)
    ;
        InstA = any(_, _),
        InstB = any(_, _),
        Inst = InstB
    ;
        InstA = abstract_inst(Name, ArgsA),
        InstB = abstract_inst(Name, ArgsB),
        inst_list_MSG(ArgsA, ArgsB, Expansions, ModuleInfo, Args),
        Inst = abstract_inst(Name, Args)
    ).

:- pred inst_list_MSG(list(mer_inst)::in, list(mer_inst)::in, expansions::in,
    module_info::in, list(mer_inst)::out) is semidet.

inst_list_MSG([], [], _, _ModuleInfo, []).
inst_list_MSG([ArgA | ArgsA], [ArgB | ArgsB], Expansions,
        ModuleInfo, [Arg | Args]) :-
    inst_MSG_1(ArgA, ArgB, Expansions, ModuleInfo, Arg),
    inst_list_MSG(ArgsA, ArgsB, Expansions, ModuleInfo, Args).

    % bound_inst_list_MSG(Xs, Ys, ModuleInfo, Zs):
    %
    % The two input lists Xs and Ys must already be sorted.
    % If any of the functors in Xs are not in Ys or vice
    % versa, the final inst is ground, unless either of the insts
    % contains any or the insts are the insts are not uniformly
    % unique (or non-unique), in which case we fail, since
    % the msg operation could introduce mode errors.
    % Otherwise, the take the msg of the argument insts.
    %
:- pred bound_inst_list_MSG(list(bound_inst)::in, list(bound_inst)::in,
    expansions::in, module_info::in, uniqueness::in,
    list(bound_inst)::in, mer_inst::out) is semidet.

bound_inst_list_MSG(Xs, Ys, Expansions, ModuleInfo, Uniq, BoundInsts, Inst) :-
    ( if
        Xs = [],
        Ys = []
    then
        Inst = bound(Uniq, inst_test_no_results, [])
    else if
        Xs = [X | Xs1],
        Ys = [Y | Ys1],
        X = bound_functor(ConsId, ArgsX),
        Y = bound_functor(ConsId, ArgsY)
    then
        inst_list_MSG(ArgsX, ArgsY, Expansions, ModuleInfo, Args),
        Z = bound_functor(ConsId, Args),
        bound_inst_list_MSG(Xs1, Ys1, Expansions,
            ModuleInfo, Uniq, BoundInsts, Inst1),
        ( if Inst1 = bound(Uniq, _, Zs) then
            Inst = bound(Uniq, inst_test_no_results, [Z | Zs])
        else
            Inst = Inst1
        )
    else
        % Check that it's OK to round off the uniqueness information.
        (
            Uniq = shared,
            NewInst = bound(shared, inst_test_no_results, BoundInsts),
            inst_is_ground(ModuleInfo, NewInst),
            inst_is_not_partly_unique(ModuleInfo, NewInst)
        ;
            Uniq = unique,
            NewInst = bound(unique, inst_test_no_results, BoundInsts),
            inst_is_unique(ModuleInfo, NewInst)
        ),
        not (
            inst_contains_nondefault_func_mode(ModuleInfo,
                bound(shared, inst_test_no_results, BoundInsts))
        ),
        Inst = ground(Uniq, none_or_default_func)
    ).

%-----------------------------------------------------------------------------%

inst_size(ModuleInfo, Inst, Size) :-
    set.init(Expansions),
    inst_size_2(ModuleInfo, Inst, Expansions, Size).

:- pred inst_size_2(module_info::in, mer_inst::in,
    set(inst_name)::in, int::out) is det.

inst_size_2(ModuleInfo, Inst, !.Expansions, Size) :-
    (
        ( Inst = not_reached
        ; Inst = free
        ; Inst = free(_)
        ; Inst = ground(_, _)
        ; Inst = any(_, _)
        ; Inst = inst_var(_)
        ; Inst = abstract_inst(_, _)
        ),
        Size = 0
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_size_2(ModuleInfo, SubInst, !.Expansions, Size)
    ;
        Inst = defined_inst(InstName),
        ( if set.member(InstName, !.Expansions) then
            Size = 1
        else
            set.insert(InstName, !Expansions),
            inst_lookup(ModuleInfo, InstName, SubInst),
            inst_size_2(ModuleInfo, SubInst, !.Expansions, Size)
        )
    ;
        Inst = bound(_, _, BoundInsts),
        bound_inst_size(ModuleInfo, BoundInsts, !.Expansions, 1, Size)
    ).

:- pred bound_inst_size(module_info::in, list(bound_inst)::in,
    set(inst_name)::in, int::in, int::out) is det.

bound_inst_size(_, [], _, !Size).
bound_inst_size(ModuleInfo, [BoundInst | BoundInsts], Expansions, !Size) :-
    BoundInst = bound_functor(_, ArgInsts),
    inst_list_size(ModuleInfo, ArgInsts, Expansions, !Size),
    !:Size = !.Size + 1,
    bound_inst_size(ModuleInfo, BoundInsts, Expansions, !Size).

inst_list_size(ModuleInfo, Insts, Size) :-
    set.init(Expansions),
    inst_list_size(ModuleInfo, Insts, Expansions, 0, Size).

:- pred inst_list_size(module_info::in, list(mer_inst)::in,
    set(inst_name)::in, int::in, int::out) is det.

inst_list_size(_, [], _, !Size).
inst_list_size(ModuleInfo, [Inst | Insts], Expansions, !Size) :-
    inst_size_2(ModuleInfo, Inst, Expansions, InstSize),
    !:Size = !.Size + InstSize,
    inst_list_size(ModuleInfo, Insts, Expansions, !Size).

%-----------------------------------------------------------------------------%

goals_match(_ModuleInfo, OldGoal, OldArgs, OldArgTypes,
        NewGoal, NewVarTypes, OldNewRenaming, TypeSubn) :-
    goal_to_conj_list(OldGoal, OldGoalList),
    goal_to_conj_list(NewGoal, NewGoalList),
    map.init(OldNewRenaming0),
    goals_match_2(OldGoalList, NewGoalList,
        OldNewRenaming0, OldNewRenaming),

    % Check that the goal produces a superset of the outputs of the
    % version we are searching for.
    Search =
        ( pred(K1::in, V1::out) is semidet :-
            map.search(OldNewRenaming, K1, V1)
        ),
    list.map(Search, OldArgs, NewArgs),
    NewGoal = hlds_goal(_, NewGoalInfo),
    NewNonLocals = goal_info_get_nonlocals(NewGoalInfo),
    set_of_var.delete_list(NewArgs, NewNonLocals, UnmatchedNonLocals),
    set_of_var.is_empty(UnmatchedNonLocals),

    % Check that argument types of NewGoal are subsumed by those of OldGoal.
    collect_matching_arg_types(OldArgs, OldArgTypes,
        OldNewRenaming, [], MatchingArgTypes),
    lookup_var_types(NewVarTypes, NewArgs, NewArgTypes),
    type_list_subsumes(MatchingArgTypes, NewArgTypes, TypeSubn).

:- pred collect_matching_arg_types(prog_vars::in, list(mer_type)::in,
    map(prog_var, prog_var)::in, list(mer_type)::in, list(mer_type)::out)
    is det.

collect_matching_arg_types([], [], _, !MatchingTypes) :-
    list.reverse(!MatchingTypes).
collect_matching_arg_types([_ | _], [], _, !MatchingTypes) :-
    unexpected($pred, "list length mismatch").
collect_matching_arg_types([], [_ | _], _, !MatchingTypes) :-
    unexpected($pred, "list length mismatch").
collect_matching_arg_types([Arg | Args], [Type | Types],
        Renaming, !MatchingTypes) :-
    ( if map.contains(Renaming, Arg) then
        !:MatchingTypes = [Type | !.MatchingTypes]
    else
        true
    ),
    collect_matching_arg_types(Args, Types, Renaming, !MatchingTypes).

    % Check that the shape of the goals matches, and that there is a mapping
    % from the variables in the old goal to the variables in the new goal.
    %
:- pred goals_match_2(hlds_goals::in, hlds_goals::in,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is semidet.

goals_match_2([], [], !ONRenaming).
goals_match_2([OldGoal | OldGoals], [NewGoal | NewGoals], !ONRenaming) :-
    % ON here is short for OldNew.
    OldGoal = hlds_goal(OldGoalExpr, _),
    NewGoal = hlds_goal(NewGoalExpr, _),
    ( if
        (
            OldGoalExpr = unify(_, _, _, OldUnification, _),
            NewGoalExpr = unify(_, _, _, NewUnification, _),
            require_complete_switch [OldUnification]
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
                OldUnification = construct(OldVar, ConsId, OldArgs1,
                    _, _, _, _),
                NewUnification = construct(NewVar, ConsId, NewArgs1,
                    _, _, _, _),
                OldArgs = [OldVar | OldArgs1],
                NewArgs = [NewVar | NewArgs1]
            ;
                OldUnification = deconstruct(OldVar, ConsId, OldArgs1,
                    _, _, _),
                NewUnification = deconstruct(NewVar, ConsId, NewArgs1,
                    _, _, _),
                OldArgs = [OldVar | OldArgs1],
                NewArgs = [NewVar | NewArgs1]
            ;
                OldUnification = complicated_unify(_, _, _),
                unexpected($pred,
                    "complicated_unify should have been expanded by now")
            )
        ;
            OldGoalExpr = plain_call(PredId, ProcId, OldArgs, _, _, _),
            NewGoalExpr = plain_call(PredId, ProcId, NewArgs, _, _, _)
        ;
            % We don't need to check the modes here - if the goals match
            % and the insts of the argument variables match, the modes
            % of the call must be the same.
            OldGoalExpr = generic_call(OldGenericCall, OldArgs1, _, _, Det),
            NewGoalExpr = generic_call(NewGenericCall, NewArgs1, _, _, Det),
            match_generic_call(OldGenericCall, NewGenericCall),
            goal_util.generic_call_vars(OldGenericCall, OldArgs0),
            goal_util.generic_call_vars(NewGenericCall, NewArgs0),
            OldArgs = OldArgs0 ++ OldArgs1,
            NewArgs = NewArgs0 ++ NewArgs1
        )
    then
        assoc_list.from_corresponding_lists(OldArgs, NewArgs, ONArgsList),
        MapInsert =
            ( pred(KeyValue::in, Map0::in, Map::out) is semidet :-
                KeyValue = Key - Value,
                ( if map.search(Map0, Key, Value0) then
                    Value = Value0,
                    Map = Map0
                else
                    map.det_insert(Key, Value, Map0, Map)
                )
            ),
        list.foldl(MapInsert, ONArgsList, !ONRenaming)
    else if
        (
            OldGoalExpr = negation(OldSubGoal),
            NewGoalExpr = negation(NewSubGoal)
        ;
            OldGoalExpr = scope(_, OldSubGoal),
            NewGoalExpr = scope(_, NewSubGoal)
        )
    then
        goal_to_conj_list(OldSubGoal, OldSubGoalList),
        goal_to_conj_list(NewSubGoal, NewSubGoalList),
        goals_match_2(OldSubGoalList, NewSubGoalList, !ONRenaming)
    else
        fail
    ),
    goals_match_2(OldGoals, NewGoals, !ONRenaming).

    % Check that two `generic_call' goals are equivalent.
    %
:- pred match_generic_call(generic_call::in, generic_call::in) is semidet.

match_generic_call(GenericCallA, GenericCallB) :-
    (
        GenericCallA = higher_order(_, Purity, PredOrFunc, Arity),
        GenericCallB = higher_order(_, Purity, PredOrFunc, Arity)
    ;
        GenericCallA = class_method(_, MethodNum, ClassId, CallId),
        GenericCallB = class_method(_, MethodNum, ClassId, CallId)
    ).

%-----------------------------------------------------------------------------%

pd_can_reorder_goals(ModuleInfo, FullyStrict, EarlierGoal, LaterGoal) :-
    % The logic here is mostly duplicated in can_reorder_goals and
    % can_reorder_goals_old in goal_util.m.

    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    LaterGoal = hlds_goal(_, LaterGoalInfo),

    EarlierDetism = goal_info_get_determinism(EarlierGoalInfo),
    LaterDetism = goal_info_get_determinism(LaterGoalInfo),

    % Check that the reordering would not violate determinism correctness
    % by moving a goal out of a single solution context by placing a goal
    % which can fail after it.
    (
        determinism_components(EarlierDetism, can_fail, _)
    =>
        not determinism_components(LaterDetism, _, at_most_many_cc)
    ),

    % Impure goals and trace goals cannot be reordered.
    goal_info_get_goal_purity(EarlierGoalInfo, EarlierPurity, EarlierTrace),
    goal_info_get_goal_purity(LaterGoalInfo, LaterPurity, LaterTrace),
    EarlierPurity \= purity_impure,
    LaterPurity \= purity_impure,
    EarlierTrace = contains_no_trace_goal,
    LaterTrace = contains_no_trace_goal,

    goal_util.reordering_maintains_termination_old(ModuleInfo, FullyStrict,
        EarlierGoal, LaterGoal),

    % Don't reorder the goals if the later goal depends on the outputs
    % of the current goal.
    not goal_depends_on_goal(EarlierGoal, LaterGoal),

    % Don't reorder the goals if the later goal changes the instantiatedness
    % of any of the non-locals of the earlier goal. This is necessary if the
    % later goal clobbers any of the non-locals of the earlier goal, and
    % avoids rerunning full mode analysis in other cases.
    not goal_depends_on_goal(LaterGoal, EarlierGoal).

:- pred goal_depends_on_goal(hlds_goal::in, hlds_goal::in) is semidet.

goal_depends_on_goal(hlds_goal(_, GoalInfo1), hlds_goal(_, GoalInfo2)) :-
    InstmapDelta1 = goal_info_get_instmap_delta(GoalInfo1),
    instmap_delta_changed_vars(InstmapDelta1, ChangedVars1),
    NonLocals2 = goal_info_get_nonlocals(GoalInfo2),
    set_of_var.intersect(ChangedVars1, NonLocals2, Intersection),
    set_of_var.is_non_empty(Intersection).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.pd_util.
%-----------------------------------------------------------------------------%
