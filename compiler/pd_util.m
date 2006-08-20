%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2006 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File pd_util.m.
% Main author: stayl.

% Utility predicates for deforestation and partial evaluation.

%-----------------------------------------------------------------------------%

:- module transform_hlds.pd_util.
:- interface.

:- import_module check_hlds.mode_errors.
:- import_module check_hlds.simplify.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.pd_info.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Pick out the pred_proc_ids of the calls in a list of atomic goals.
    %
:- pred goal_get_calls(hlds_goal::in, list(pred_proc_id)::out) is det.

    % Call constraint.m to transform a goal so that goals which
    % can fail are executed as early as possible.
    %
:- pred propagate_constraints(hlds_goal::in, hlds_goal::out,
    pd_info::in, pd_info::out, io::di, io::uo) is det.

    % Apply simplify.m to the goal.
    %
:- pred pd_simplify_goal(simplifications::in, hlds_goal::in,
    hlds_goal::out, pd_info::in, pd_info::out, io::di, io::uo) is det.

    % Apply unique_modes.m to the goal.
    %
:- pred unique_modecheck_goal(hlds_goal::in, hlds_goal::out,
    list(mode_error_info)::out, pd_info::in, pd_info::out,
    io::di, io::uo) is det.

    % Apply unique_modes.m to the goal.
    %
:- pred unique_modecheck_goal(set(prog_var)::in,
    hlds_goal::in, hlds_goal::out, list(mode_error_info)::out,
    pd_info::in, pd_info::out, io::di, io::uo) is det.

    % Find out which arguments of the procedure are interesting
    % for deforestation.
    %
:- pred get_branch_vars_proc(pred_proc_id::in, proc_info::in,
    pd_arg_info::in, pd_arg_info::out,
    module_info::in, module_info::out) is det.

    % Find out which variables of the goal are interesting
    % for deforestation.
    %
:- pred get_branch_vars_goal(hlds_goal::in,
    maybe(pd_branch_info(prog_var))::out, pd_info::in, pd_info::out)
    is det.

    % Recompute the non-locals of the goal.
    %
:- pred requantify_goal(set(prog_var)::in,
    hlds_goal::in, hlds_goal::out, pd_info::in, pd_info::out) is det.

    % Apply mode_util.recompute_instmap_delta to the goal.
    %
:- pred recompute_instmap_delta(hlds_goal::in, hlds_goal::out,
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
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_info.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.purity.
:- import_module check_hlds.unique_modes.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.instmap.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_type.
:- import_module transform_hlds.constraint.
:- import_module transform_hlds.pd_cost.
:- import_module transform_hlds.pd_debug.
:- import_module transform_hlds.unused_args.

:- import_module assoc_list.
:- import_module int.
:- import_module pair.
:- import_module set.
:- import_module term.
:- import_module svmap.
:- import_module svset.

goal_get_calls(Goal0, CalledPreds) :-
    goal_to_conj_list(Goal0, GoalList),
    GetCalls = (pred(Goal::in, CalledPred::out) is semidet :-
        Goal = plain_call(PredId, ProcId, _, _, _, _) - _,
        CalledPred = proc(PredId, ProcId)
    ),
    list.filter_map(GetCalls, GoalList, CalledPreds).

%-----------------------------------------------------------------------------%

propagate_constraints(!Goal, !PDInfo, !IO) :-
    globals.io_lookup_bool_option(local_constraint_propagation,
        ConstraintProp, !IO),
    (
        ConstraintProp = yes,
        Goal0 = !.Goal,
        pd_debug_message("%% Propagating constraints\n", [], !IO),
        pd_debug_output_goal(!.PDInfo, "before constraints\n", Goal0, !IO),
        pd_info_get_module_info(!.PDInfo, ModuleInfo0),
        pd_info_get_proc_info(!.PDInfo, ProcInfo0),
        pd_info_get_instmap(!.PDInfo, InstMap),
        proc_info_get_vartypes(ProcInfo0, VarTypes0),
        proc_info_get_varset(ProcInfo0, VarSet0),
        constraint_info_init(ModuleInfo0, VarTypes0, VarSet0, InstMap, CInfo0),
        Goal0 = _ - GoalInfo0,
        goal_info_get_nonlocals(GoalInfo0, NonLocals),
        constraint.propagate_constraints_in_goal(!Goal, CInfo0, CInfo, !IO),
        constraint_info_deconstruct(CInfo, ModuleInfo, VarTypes, VarSet,
            Changed),
        pd_info_set_module_info(ModuleInfo, !PDInfo),
        proc_info_set_vartypes(VarTypes, ProcInfo0, ProcInfo1),
        proc_info_set_varset(VarSet, ProcInfo1, ProcInfo),
        pd_info_set_proc_info(ProcInfo, !PDInfo),
        (
            Changed = yes,
            pd_debug_output_goal(!.PDInfo,
                "after constraints, before recompute\n", !.Goal, !IO),
            requantify_goal(NonLocals, !Goal, !PDInfo),
            recompute_instmap_delta(!Goal, !PDInfo),
            rerun_det_analysis(!Goal, !PDInfo, !IO),
            module_info_get_globals(ModuleInfo, Globals),
            simplify.find_simplifications(no, Globals, Simplifications),
            pd_simplify_goal(Simplifications, !Goal, !PDInfo, !IO)
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

pd_simplify_goal(Simplifications, Goal0, Goal, !PDInfo, !IO) :-
    % Construct a simplify_info.
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    module_info_get_globals(ModuleInfo0, Globals),
    pd_info_get_pred_proc_id(!.PDInfo, proc(PredId, ProcId)),
    proc_info_get_vartypes(ProcInfo0, VarTypes0),
    det_info_init(ModuleInfo0, VarTypes0, PredId, ProcId, Globals, DetInfo0),
    pd_info_get_instmap(!.PDInfo, InstMap0),
    pd_info_get_proc_info(!.PDInfo, ProcInfo0),
    simplify_info_init(DetInfo0, Simplifications, InstMap0, ProcInfo0,
        SimplifyInfo0),

    simplify_process_goal(Goal0, Goal, SimplifyInfo0, SimplifyInfo, !IO),

    % Deconstruct the simplify_info.
    simplify_info_get_module_info(SimplifyInfo, ModuleInfo),
    simplify_info_get_varset(SimplifyInfo, VarSet),
    simplify_info_get_var_types(SimplifyInfo, VarTypes),
    simplify_info_get_cost_delta(SimplifyInfo, CostDelta),
    simplify_info_get_rtti_varmaps(SimplifyInfo, RttiVarMaps),
    pd_info_get_proc_info(!.PDInfo, ProcInfo1),
    proc_info_set_varset(VarSet, ProcInfo1, ProcInfo2),
    proc_info_set_vartypes(VarTypes, ProcInfo2, ProcInfo3),
    proc_info_set_rtti_varmaps(RttiVarMaps, ProcInfo3, ProcInfo),
    pd_info_set_proc_info(ProcInfo, !PDInfo),
    pd_info_incr_cost_delta(CostDelta, !PDInfo),
    pd_info_set_module_info(ModuleInfo, !PDInfo).

%-----------------------------------------------------------------------------%

unique_modecheck_goal(Goal0, Goal, Errors, !PDInfo, !IO) :-
    get_goal_live_vars(!.PDInfo, Goal0, LiveVars),
    unique_modecheck_goal(LiveVars, Goal0, Goal, Errors,
        !PDInfo, !IO).

unique_modecheck_goal(LiveVars, Goal0, Goal, Errors, !PDInfo, !IO) :-
    % Construct a mode_info.
    pd_info_get_pred_proc_id(!.PDInfo, PredProcId),
    PredProcId = proc(PredId, ProcId),
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    pd_info_get_instmap(!.PDInfo, InstMap0),
    term.context_init(Context),
    pd_info_get_pred_info(!.PDInfo, PredInfo0),
    pd_info_get_proc_info(!.PDInfo, ProcInfo0),
    module_info_set_pred_proc_info(PredId, ProcId, PredInfo0, ProcInfo0,
        ModuleInfo0, ModuleInfo1),

    % If we perform generalisation, we shouldn't change any called procedures,
    % since that could cause a less efficient version to be chosen.
    MayChangeCalledProc = may_not_change_called_proc,
    mode_info_init(ModuleInfo1, PredId, ProcId, Context, LiveVars, InstMap0,
        check_unique_modes, MayChangeCalledProc, ModeInfo0),

    unique_modes.check_goal(Goal0, Goal, ModeInfo0, ModeInfo1, !IO),
    globals.io_lookup_bool_option(debug_pd, Debug, !IO),
    (
        Debug = yes,
        report_mode_errors(ModeInfo1, ModeInfo, !IO)
    ;
        Debug = no,
        ModeInfo = ModeInfo1
    ),
    mode_info_get_errors(ModeInfo, Errors),

    % Deconstruct the mode_info.
    mode_info_get_module_info(ModeInfo, ModuleInfo),
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
    set(prog_var)::out) is det.

get_goal_live_vars(PDInfo, _ - GoalInfo, !:Vars) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    pd_info_get_instmap(PDInfo, InstMap),
    goal_info_get_nonlocals(GoalInfo, NonLocals),
    set.to_sorted_list(NonLocals, NonLocalsList),
    set.init(!:Vars),
    get_goal_live_vars_2(ModuleInfo, NonLocalsList, InstMap, InstMapDelta,
        !Vars).

:- pred get_goal_live_vars_2(module_info::in, prog_vars::in,
    instmap::in, instmap_delta::in,
    set(prog_var)::in, set(prog_var)::out) is det.

get_goal_live_vars_2(_, [], _, _, !Vars).
get_goal_live_vars_2(ModuleInfo, [NonLocal | NonLocals],
        InstMap, InstMapDelta, !Vars) :-
    ( instmap_delta_search_var(InstMapDelta, NonLocal, FinalInst0) ->
        FinalInst = FinalInst0
    ;
        instmap.lookup_var(InstMap, NonLocal, FinalInst)
    ),
    ( inst_is_clobbered(ModuleInfo, FinalInst) ->
        true
    ;
        svset.insert(NonLocal, !Vars)
    ),
    get_goal_live_vars_2(ModuleInfo, NonLocals, InstMap, InstMapDelta, !Vars).

%-----------------------------------------------------------------------------%

:- pred rerun_det_analysis(hlds_goal::in, hlds_goal::out,
    pd_info::in, pd_info::out, io::di, io::uo) is det.

rerun_det_analysis(Goal0, Goal, !PDInfo, !IO) :-
    Goal0 = _ - GoalInfo0,

    goal_info_get_determinism(GoalInfo0, Det),
    det_get_soln_context(Det, SolnContext),

    % det_infer_goal looks up the proc_info in the module_info for the
    % vartypes, so we'd better stick them back in the module_info.
    pd_info_get_pred_proc_id(!.PDInfo, proc(PredId, ProcId)),
    pd_info_get_pred_info(!.PDInfo, PredInfo),
    pd_info_get_proc_info(!.PDInfo, ProcInfo),
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    module_info_set_pred_proc_info(PredId, ProcId, PredInfo, ProcInfo,
        ModuleInfo0, ModuleInfo),
    pd_info_set_module_info(ModuleInfo, !PDInfo),

    module_info_get_globals(ModuleInfo, Globals),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    det_info_init(ModuleInfo, VarTypes, PredId, ProcId, Globals, DetInfo),
    pd_info_get_instmap(!.PDInfo, InstMap),
    det_infer_goal(Goal0, Goal, InstMap, SolnContext, [], no, DetInfo, _, _,
        Msgs),

    % Make sure there were no errors.
    disable_det_warnings(OptionsToRestore, !IO),
    det_report_msgs(Msgs, ModuleInfo, _, ErrCnt, !IO),
    restore_det_warnings(OptionsToRestore, !IO),
    expect(unify(ErrCnt, 0), this_file,
        "rerun_det_analysis: determinism errors").

%-----------------------------------------------------------------------------%

convert_branch_info(ArgInfo, Args, VarInfo) :-
    ArgInfo = pd_branch_info(ArgMap, LeftArgs, OpaqueArgs),
    map.to_assoc_list(ArgMap, ArgList),
    map.init(BranchVarMap0),
    convert_branch_info_2(ArgList, Args, BranchVarMap0, BranchVarMap),

    set.to_sorted_list(LeftArgs, LeftArgNos),
    list.map(list.index1_det(Args), LeftArgNos, LeftVars0),
    set.list_to_set(LeftVars0, LeftVars),

    set.to_sorted_list(OpaqueArgs, OpaqueArgNos),
    list.map(list.index1_det(Args), OpaqueArgNos, OpaqueVars0),
    set.list_to_set(OpaqueVars0, OpaqueVars),

    VarInfo = pd_branch_info(BranchVarMap, LeftVars, OpaqueVars).

:- pred convert_branch_info_2(assoc_list(int, set(int))::in,
    prog_vars::in, pd_var_info::in, pd_var_info::out) is det.

convert_branch_info_2([], _, !VarInfo).
convert_branch_info_2([ArgNo - Branches | ArgInfos], Args, !VarInfo) :-
    list.index1_det(Args, ArgNo, Arg),
    svmap.set(Arg, Branches, !VarInfo),
    convert_branch_info_2(ArgInfos, Args, !VarInfo).

%-----------------------------------------------------------------------------%

:- type pd_var_info     ==  branch_info_map(prog_var).

get_branch_vars_proc(PredProcId, ProcInfo, !ArgInfo, !ModuleInfo) :-
    proc_info_get_goal(ProcInfo, Goal),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    instmap.init_reachable(InstMap0),
    map.init(Vars0),
    set.init(LeftVars0),
    goal_to_conj_list(Goal, GoalList),
    (
        get_branch_vars_goal_2(!.ModuleInfo, GoalList, no,
            VarTypes, InstMap0, LeftVars0, LeftVars, Vars0, Vars)
    ->
        proc_info_get_headvars(ProcInfo, HeadVars),
        map.init(ThisProcArgMap0),
        set.init(ThisProcLeftArgs0),
        get_extra_info_headvars(HeadVars, 1, LeftVars, Vars,
            ThisProcArgMap0, ThisProcArgMap1,
            ThisProcLeftArgs0, ThisProcLeftArgs),
        set.init(OpaqueArgs0),
        BranchInfo0 = pd_branch_info(ThisProcArgMap1, ThisProcLeftArgs,
            OpaqueArgs0),
        svmap.set(PredProcId, BranchInfo0, !ArgInfo),

            % Look for opportunities for deforestation in
            % the sub-branches of the top-level goal.
        get_sub_branch_vars_goal(!.ArgInfo, GoalList,
            VarTypes, InstMap0, Vars, AllVars, !ModuleInfo),
        get_extra_info_headvars(HeadVars, 1, LeftVars0,
            AllVars, ThisProcArgMap0, ThisProcArgMap, ThisProcLeftArgs0, _),

        proc_info_get_argmodes(ProcInfo, ArgModes),
        get_opaque_args(!.ModuleInfo, 1, ArgModes,
            ThisProcArgMap, OpaqueArgs0, OpaqueArgs),

        BranchInfo = pd_branch_info(ThisProcArgMap, ThisProcLeftArgs,
            OpaqueArgs),
        svmap.set(PredProcId, BranchInfo, !ArgInfo)
    ;
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
    (
        mode_is_output(ModuleInfo, ArgMode),
        \+ map.contains(ExtraInfoArgs, ArgNo)
    ->
        set.insert(!.OpaqueArgs, ArgNo, !:OpaqueArgs)
    ;
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
    set(prog_var)::in, pd_var_info::in,
    branch_info_map(int)::in, branch_info_map(int)::out,
    set(int)::in, set(int)::out) is det.

get_extra_info_headvars([], _, _, _, !Args, !LeftArgs).
get_extra_info_headvars([HeadVar | HeadVars], ArgNo,
        LeftVars, VarInfo, !ThisProcArgs, !ThisProcLeftVars) :-
    ( map.search(VarInfo, HeadVar, ThisVarInfo) ->
        svmap.det_insert(ArgNo, ThisVarInfo, !ThisProcArgs)
    ;
        true
    ),
    ( set.member(HeadVar, LeftVars) ->
        svset.insert(ArgNo, !ThisProcLeftVars)
    ;
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
    set.init(LeftVars0),
    map.init(Vars0),
    (
        get_branch_vars_goal_2(ModuleInfo0, [Goal], no,
            VarTypes, InstMap0, LeftVars0, LeftVars, Vars0, Vars1)
    ->
        get_sub_branch_vars_goal(ProcArgInfo, [Goal],
            VarTypes, InstMap0, Vars1, Vars,
            ModuleInfo0, ModuleInfo),
        pd_info_set_module_info(ModuleInfo, !PDInfo),

        % OpaqueVars is only filled in for calls.
        set.init(OpaqueVars),
        MaybeBranchInfo = yes(pd_branch_info(Vars, LeftVars, OpaqueVars))
    ;
        MaybeBranchInfo = no
    ).

:- pred get_branch_vars_goal_2(module_info::in, hlds_goals::in,
    bool::in, vartypes::in, instmap::in,
    set(prog_var)::in, set(prog_var)::out,
    pd_var_info::in, pd_var_info::out) is semidet.

get_branch_vars_goal_2(_, [], yes, _, _, !LeftVars, !Vars).
get_branch_vars_goal_2(ModuleInfo, [Goal | Goals], !.FoundBranch,
        VarTypes, InstMap0, !LeftVars, !Vars) :-
    Goal = _ - GoalInfo,
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    instmap.apply_instmap_delta(InstMap0, InstMapDelta, InstMap),
    ( get_branch_instmap_deltas(Goal, InstMapDeltas) ->
        % Only look for goals with one top-level branched goal,
        % since deforestation of goals with more than one is
        % likely to be less productive.
        !.FoundBranch = no,
        get_branch_vars(ModuleInfo, Goal, InstMapDeltas, InstMap,
            1, !Vars),
        get_left_vars(Goal, !LeftVars),
        !:FoundBranch = yes
    ;
        Goal = GoalExpr - _,
        goal_is_atomic(GoalExpr)
    ),
    get_branch_vars_goal_2(ModuleInfo, Goals, !.FoundBranch,
        VarTypes, InstMap, !LeftVars, !Vars).

:- pred get_branch_instmap_deltas(hlds_goal::in,
    list(instmap_delta)::out) is semidet.

get_branch_instmap_deltas(Goal, [CondDelta, ThenDelta, ElseDelta]) :-
    Goal = if_then_else(_, _ - CondInfo, _ - ThenInfo, _ - ElseInfo) - _,
    goal_info_get_instmap_delta(CondInfo, CondDelta),
    goal_info_get_instmap_delta(ThenInfo, ThenDelta),
    goal_info_get_instmap_delta(ElseInfo, ElseDelta).
get_branch_instmap_deltas(switch(_, _, Cases) - _, InstMapDeltas) :-
    GetCaseInstMapDelta =
        (pred(Case::in, InstMapDelta::out) is det :-
            Case = case(_, _ - CaseInfo),
            goal_info_get_instmap_delta(CaseInfo, InstMapDelta)
        ),
    list.map(GetCaseInstMapDelta, Cases, InstMapDeltas).
get_branch_instmap_deltas(disj(Disjuncts) - _, InstMapDeltas) :-
    GetDisjunctInstMapDelta =
        (pred(Disjunct::in, InstMapDelta::out) is det :-
            Disjunct = _ - DisjInfo,
            goal_info_get_instmap_delta(DisjInfo, InstMapDelta)
        ),
    list.map(GetDisjunctInstMapDelta, Disjuncts, InstMapDeltas).

    % Get the variables for which we can do unfolding if the goals to
    % the left supply the top-level functor. Eventually this should
    % also check for if-then-elses with simple conditions.
    %
:- pred get_left_vars(hlds_goal::in,
    set(prog_var)::in, set(prog_var)::out) is det.

get_left_vars(Goal, Vars0, Vars) :-
    ( Goal = switch(Var, _, _) - _ ->
        set.insert(Vars0, Var, Vars)
    ;
        Vars = Vars0
    ).

:- pred get_branch_vars(module_info::in, hlds_goal::in,
    list(instmap_delta)::in, instmap::in, int::in,
    pd_var_info::in, pd_var_info::out) is semidet.

get_branch_vars(_, _, [], _, _, !ExtraVars).
get_branch_vars(ModuleInfo, Goal, [InstMapDelta | InstMapDeltas],
        InstMap, BranchNo, !ExtraVars) :-
    AddExtraInfoVars =
        (pred(ChangedVar::in, Vars0::in, Vars::out) is det :-
            (
                instmap.lookup_var(InstMap, ChangedVar, VarInst),
                instmap_delta_search_var(InstMapDelta, ChangedVar,
                    DeltaVarInst),
                inst_is_bound_to_functors(ModuleInfo, DeltaVarInst, [_]),
                \+ inst_is_bound_to_functors(ModuleInfo, VarInst, [_])
            ->
                ( map.search(Vars0, ChangedVar, Set0) ->
                    set.insert(Set0, BranchNo, Set)
                ;
                    set.singleton_set(Set, BranchNo)
                ),
                map.set(Vars0, ChangedVar, Set, Vars)
            ;
                Vars = Vars0
            )
        ),
    instmap_delta_changed_vars(InstMapDelta, ChangedVars),
    set.to_sorted_list(ChangedVars, ChangedVarsList),
    list.foldl(AddExtraInfoVars, ChangedVarsList, !ExtraVars),

    % We have extra information about a switched-on variable
    % at the end of each branch.
    ( Goal = switch(SwitchVar, _, _) - _ ->
        ( map.search(!.ExtraVars, SwitchVar, SwitchVarSet0) ->
            set.insert(SwitchVarSet0, BranchNo, SwitchVarSet)
        ;
            set.singleton_set(SwitchVarSet, BranchNo)
        ),
        svmap.set(SwitchVar, SwitchVarSet, !ExtraVars)
    ;
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
        VarTypes, InstMap0, Vars0, SubVars, !ModuleInfo) :-
    Goal = GoalExpr - GoalInfo,
    ( GoalExpr = if_then_else(_, Cond, Then, Else) ->
        Cond = _ - CondInfo,
        goal_info_get_instmap_delta(CondInfo, CondDelta),
        instmap.apply_instmap_delta(InstMap0, CondDelta, InstMap1),
        goal_to_conj_list(Then, ThenList),
        examine_branch(!.ModuleInfo, ProcArgInfo, 1, ThenList,
            VarTypes, InstMap1, Vars0, Vars1),
        goal_to_conj_list(Else, ElseList),
        examine_branch(!.ModuleInfo, ProcArgInfo, 2, ElseList,
            VarTypes, InstMap0, Vars1, Vars2)
    ; GoalExpr = disj(Goals) ->
        examine_branch_list(!.ModuleInfo, ProcArgInfo,
            1, Goals, VarTypes, InstMap0, Vars0, Vars2)
    ; GoalExpr = switch(Var, _, Cases) ->
        examine_case_list(ProcArgInfo, 1, Var,
            Cases, VarTypes, InstMap0, Vars0, Vars2, !ModuleInfo)
    ;
        Vars2 = Vars0
    ),
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    instmap.apply_instmap_delta(InstMap0, InstMapDelta, InstMap),
    get_sub_branch_vars_goal(ProcArgInfo, GoalList,
        VarTypes, InstMap, Vars2, SubVars, !ModuleInfo).

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
examine_case_list(ProcArgInfo, BranchNo, Var,
        [case(ConsId, Goal) | Goals], VarTypes, InstMap, !Vars, !ModuleInfo) :-
    map.lookup(VarTypes, Var, Type),
    instmap.bind_var_to_functor(Var, Type, ConsId, InstMap, InstMap1,
        !ModuleInfo),
    goal_to_conj_list(Goal, GoalList),
    examine_branch(!.ModuleInfo, ProcArgInfo, BranchNo, GoalList,
        VarTypes, InstMap1, !Vars),
    NextBranch = BranchNo + 1,
    examine_case_list(ProcArgInfo, NextBranch, Var, Goals,
        VarTypes, InstMap, !Vars, !ModuleInfo).

:- pred examine_branch(module_info::in, pd_arg_info::in, int::in,
    hlds_goals::in, vartypes::in, instmap::in,
    branch_info_map(prog_var)::in, branch_info_map(prog_var)::out) is det.

examine_branch(_, _, _, [], _, _, !Vars).
examine_branch(ModuleInfo, ProcArgInfo, BranchNo, [Goal | Goals],
        VarTypes, InstMap, !Vars) :-
    ( Goal = plain_call(PredId, ProcId, Args, _, _, _) - _ ->
        ( map.search(ProcArgInfo, proc(PredId, ProcId), ThisProcArgInfo) ->
            convert_branch_info(ThisProcArgInfo, Args, BranchInfo),
            BranchInfo = pd_branch_info(!:Vars, _, _),
            map.keys(!.Vars, ExtraVars1),
            combine_vars(BranchNo, ExtraVars1, !Vars)
        ;
            true
        )
    ;
        set.init(LeftVars0),
        map.init(!:Vars),
        get_branch_vars_goal_2(ModuleInfo, [Goal], no,
            VarTypes, InstMap, LeftVars0, _, !Vars)
    ->
        map.keys(!.Vars, ExtraVars2),
        combine_vars(BranchNo, ExtraVars2, !Vars)
    ;
        true
    ),
    Goal = _ - GoalInfo,
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    instmap.apply_instmap_delta(InstMap, InstMapDelta, InstMap1),
    examine_branch(ModuleInfo, ProcArgInfo, BranchNo,
        Goals, VarTypes, InstMap1, !Vars).

:- pred combine_vars(int::in, prog_vars::in,
    branch_info_map(prog_var)::in, branch_info_map(prog_var)::out) is det.

combine_vars(_, [], !Vars).
combine_vars(BranchNo, [ExtraVar | ExtraVars], !Vars) :-
    ( map.search(!.Vars, ExtraVar, Branches0) ->
        set.insert(Branches0, BranchNo, Branches),
        svmap.det_update(ExtraVar, Branches, !Vars)
    ;
        set.singleton_set(Branches, BranchNo),
        svmap.det_insert(ExtraVar, Branches, !Vars)
    ),
    combine_vars(BranchNo, ExtraVars, !Vars).

%-----------------------------------------------------------------------------%

requantify_goal(NonLocals, Goal0, Goal, !PDInfo) :-
    some [!ProcInfo] (
        pd_info_get_proc_info(!.PDInfo, !:ProcInfo),
        proc_info_get_varset(!.ProcInfo, VarSet0),
        proc_info_get_vartypes(!.ProcInfo, VarTypes0),
        proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
        implicitly_quantify_goal(NonLocals, _, Goal0, Goal, VarSet0, VarSet,
            VarTypes0, VarTypes, RttiVarMaps0, RttiVarMaps),
        proc_info_set_varset(VarSet, !ProcInfo),
        proc_info_set_vartypes(VarTypes, !ProcInfo),
        proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),
        pd_info_set_proc_info(!.ProcInfo, !PDInfo)
    ).

recompute_instmap_delta(Goal0, Goal, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    pd_info_get_instmap(!.PDInfo, InstMap),
    pd_info_get_proc_info(!.PDInfo, ProcInfo),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    recompute_instmap_delta(yes, Goal0, Goal, VarTypes, InstVarSet,
        InstMap, ModuleInfo0, ModuleInfo),
    pd_info_set_module_info(ModuleInfo, !PDInfo).

%-----------------------------------------------------------------------------%

inst_MSG(InstA, InstB, ModuleInfo, Inst) :-
    set.init(Expansions),
    inst_MSG_1(InstA, InstB, Expansions, ModuleInfo, Inst).

:- type expansions == set(pair(mer_inst)).

:- pred inst_MSG_1(mer_inst::in, mer_inst::in, expansions::in, module_info::in,
    mer_inst::out) is semidet.

inst_MSG_1(InstA, InstB, Expansions, ModuleInfo, Inst) :-
    ( InstA = InstB ->
        Inst = InstA
    ;
        % We don't do recursive MSGs
        % (we could, but it's probably not worth it).
        \+ set.member(InstA - InstB, Expansions),
        inst_expand(ModuleInfo, InstA, InstA2),
        inst_expand(ModuleInfo, InstB, InstB2),
        set.insert(Expansions, InstA - InstB, Expansions1),
        ( InstB2 = not_reached ->
            Inst = InstA2
        ;
            inst_MSG_2(InstA2, InstB2, Expansions1, ModuleInfo, Inst)
        )
    ).

:- pred inst_MSG_2(mer_inst::in, mer_inst::in, expansions::in, module_info::in,
    mer_inst::out) is semidet.

inst_MSG_2(any(_), any(Uniq), _, _, any(Uniq)).
inst_MSG_2(free, free, _M, _, free).

inst_MSG_2(bound(_, ListA), bound(UniqB, ListB), Expansions,
        ModuleInfo, Inst) :-
    bound_inst_list_MSG(ListA, ListB, Expansions,
        ModuleInfo, UniqB, ListB, Inst).
inst_MSG_2(bound(_, _), ground(UniqB, InfoB), _, _, ground(UniqB, InfoB)).

    % Fail here, since the increasing inst size could
    % cause termination problems for deforestation.
inst_MSG_2(ground(_, _), bound(_UniqB, _ListB), _, _, _) :- fail.
inst_MSG_2(ground(_, _), ground(UniqB, InfoB), _, _, ground(UniqB, InfoB)).
inst_MSG_2(abstract_inst(Name, ArgsA), abstract_inst(Name, ArgsB),
        Expansions, ModuleInfo, abstract_inst(Name, Args)) :-
    inst_list_MSG(ArgsA, ArgsB, Expansions, ModuleInfo, Args).
inst_MSG_2(not_reached, Inst, _, _, Inst).

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

bound_inst_list_MSG(Xs, Ys, Expansions, ModuleInfo, Uniq, List, Inst) :-
    (
        Xs = [],
        Ys = []
    ->
        Inst = bound(Uniq, [])
    ;
        Xs = [X | Xs1],
        Ys = [Y | Ys1],
        X = bound_functor(ConsId, ArgsX),
        Y = bound_functor(ConsId, ArgsY)
    ->
        inst_list_MSG(ArgsX, ArgsY, Expansions, ModuleInfo, Args),
        Z = bound_functor(ConsId, Args),
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
            inst_is_not_partly_unique(ModuleInfo, bound(shared, List))
        ;
            Uniq = unique,
            inst_is_unique(ModuleInfo, bound(unique, List))
        ),
        \+ inst_contains_nonstandard_func_mode(ModuleInfo,
            bound(shared, List)),
        Inst = ground(Uniq, none)
    ).

%-----------------------------------------------------------------------------%

inst_size(ModuleInfo, Inst, Size) :-
    set.init(Expansions),
    inst_size_2(ModuleInfo, Inst, Expansions, Size).

:- pred inst_size_2(module_info::in, mer_inst::in,
    set(inst_name)::in, int::out) is det.

inst_size_2(_, not_reached, _, 0).
inst_size_2(_, any(_), _, 0).
inst_size_2(_, free, _, 0).
inst_size_2(_, free(_), _, 0).
inst_size_2(_, ground(_, _), _, 0).
inst_size_2(_, inst_var(_), _, 0).
inst_size_2(ModuleInfo, constrained_inst_vars(_, Inst), Expansions,
        Size) :-
    inst_size_2(ModuleInfo, Inst, Expansions, Size).
inst_size_2(_, abstract_inst(_, _), _, 0).
inst_size_2(ModuleInfo, defined_inst(InstName), Expansions0, Size) :-
    ( set.member(InstName, Expansions0) ->
        Size = 1
    ;
        set.insert(Expansions0, InstName, Expansions),
        inst_lookup(ModuleInfo, InstName, Inst),
        inst_size_2(ModuleInfo, Inst, Expansions, Size)
    ).
inst_size_2(ModuleInfo, bound(_, Functors), Expansions, Size) :-
    bound_inst_size(ModuleInfo, Functors, Expansions, 1, Size).

:- pred bound_inst_size(module_info::in, list(bound_inst)::in,
    set(inst_name)::in, int::in, int::out) is det.

bound_inst_size(_, [], _, Size, Size).
bound_inst_size(ModuleInfo, [bound_functor(_, ArgInsts) | Insts],
        Expansions, Size0, Size) :-
    inst_list_size(ModuleInfo, ArgInsts, Expansions, Size0, Size1),
    Size2 = Size1 + 1,
    bound_inst_size(ModuleInfo, Insts, Expansions, Size2, Size).

inst_list_size(ModuleInfo, Insts, Size) :-
    set.init(Expansions),
    inst_list_size(ModuleInfo, Insts, Expansions, 0, Size).

:- pred inst_list_size(module_info::in, list(mer_inst)::in,
    set(inst_name)::in, int::in, int::out) is det.

inst_list_size(_, [], _, Size, Size).
inst_list_size(ModuleInfo, [Inst | Insts],
        Expansions, Size0, Size) :-
    inst_size_2(ModuleInfo, Inst, Expansions, Size1),
    Size2 = Size0 + Size1,
    inst_list_size(ModuleInfo, Insts, Expansions, Size2, Size).

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
    Search = (pred(K1::in, V1::out) is semidet :-
        map.search(OldNewRenaming, K1, V1)
    ),
    list.map(Search, OldArgs, NewArgs),
    NewGoal = _ - NewGoalInfo,
    goal_info_get_nonlocals(NewGoalInfo, NewNonLocals),
    set.delete_list(NewNonLocals, NewArgs, UnmatchedNonLocals),
    set.empty(UnmatchedNonLocals),

    % Check that argument types of NewGoal are subsumed by those of OldGoal.
    collect_matching_arg_types(OldArgs, OldArgTypes,
        OldNewRenaming, [], MatchingArgTypes),
    map.apply_to_list(NewArgs, NewVarTypes, NewArgTypes),
    type_list_subsumes(MatchingArgTypes, NewArgTypes, TypeSubn).

:- pred collect_matching_arg_types(prog_vars::in, list(mer_type)::in,
    map(prog_var, prog_var)::in, list(mer_type)::in, list(mer_type)::out)
    is det.

collect_matching_arg_types([], [], _, !MatchingTypes) :-
    list.reverse(!MatchingTypes).
collect_matching_arg_types([_ | _], [], _, !MatchingTypes) :-
    unexpected(this_file, "collect_matching_arg_types").
collect_matching_arg_types([], [_ | _], _, !MatchingTypes) :-
    unexpected(this_file, "collect_matching_arg_types").
collect_matching_arg_types([Arg | Args], [Type | Types],
        Renaming, !MatchingTypes) :-
    ( map.contains(Renaming, Arg) ->
        !:MatchingTypes = [Type | !.MatchingTypes]
    ;
        true
    ),
    collect_matching_arg_types(Args, Types,
        Renaming, !MatchingTypes).

    % Check that the shape of the goals matches, and that there is a mapping
    % from the variables in the old goal to the variables in the new goal.
    %
:- pred goals_match_2(hlds_goals::in, hlds_goals::in,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is semidet.

goals_match_2([], [], !ONRenaming).
goals_match_2([OldGoal | OldGoals], [NewGoal | NewGoals],
        !ONRenaming) :-
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
            )
        ;
            OldGoal = plain_call(PredId, ProcId, OldArgs, _, _, _) - _,
            NewGoal = plain_call(PredId, ProcId, NewArgs, _, _, _) - _
        ;
            % We don't need to check the modes here - if the goals match
            % and the insts of the argument variables match, the modes
            % of the call must be the same.
            OldGoal = generic_call(OldGenericCall, OldArgs1, _, Det) - _,
            NewGoal = generic_call(NewGenericCall, NewArgs1, _, Det) - _,
            match_generic_call(OldGenericCall, NewGenericCall),
            goal_util.generic_call_vars(OldGenericCall, OldArgs0),
            goal_util.generic_call_vars(NewGenericCall, NewArgs0),
            list.append(OldArgs0, OldArgs1, OldArgs),
            list.append(NewArgs0, NewArgs1, NewArgs)
        )
    ->
        assoc_list.from_corresponding_lists(OldArgs, NewArgs, ONArgsList),
        MapInsert =
            (pred(KeyValue::in, Map0::in, Map::out) is semidet :-
                KeyValue = Key - Value,
                ( map.search(Map0, Key, Value0) ->
                    Value = Value0,
                    Map = Map0
                ;
                    map.det_insert(Map0, Key, Value, Map)
                )
            ),
        list.foldl(MapInsert, ONArgsList, !ONRenaming)
    ;
        (
            OldGoal = negation(OldSubGoal) - _,
            NewGoal = negation(NewSubGoal) - _
        ;
            OldGoal = scope(_, OldSubGoal) - _,
            NewGoal = scope(_, NewSubGoal) - _
        )
    ->
        goal_to_conj_list(OldSubGoal, OldSubGoalList),
        goal_to_conj_list(NewSubGoal, NewSubGoalList),
        goals_match_2(OldSubGoalList, NewSubGoalList, !ONRenaming)
    ;
        fail
    ),
    goals_match_2(OldGoals, NewGoals, !ONRenaming).

    % Check that two `generic_call' goals are equivalent.
    %
:- pred match_generic_call(generic_call::in, generic_call::in) is semidet.

match_generic_call(higher_order(_, Purity, PredOrFunc, Arity),
        higher_order(_, Purity, PredOrFunc, Arity)).
match_generic_call(class_method(_, MethodNum, ClassId, CallId),
        class_method(_, MethodNum, ClassId, CallId)).

%-----------------------------------------------------------------------------%

pd_can_reorder_goals(ModuleInfo, FullyStrict, EarlierGoal, LaterGoal) :-
    % The logic here is mostly duplicated in can_reorder_goals and
    % can_reorder_goals_old in goal_util.m.

    EarlierGoal = _ - EarlierGoalInfo,
    LaterGoal = _ - LaterGoalInfo,

    goal_info_get_determinism(EarlierGoalInfo, EarlierDetism),
    goal_info_get_determinism(LaterGoalInfo, LaterDetism),

    % Check that the reordering would not violate determinism correctness
    % by moving a goal out of a single solution context by placing a goal
    % which can fail after it.
    (
        determinism_components(EarlierDetism, can_fail, _)
    =>
        \+ determinism_components(LaterDetism, _, at_most_many_cc)
    ),

    % Impure goals and trace goals cannot be reordered.
    goal_info_get_goal_purity(EarlierGoalInfo, EarlierPurity, EarlierTrace),
    goal_info_get_goal_purity(LaterGoalInfo, LaterPurity, LaterTrace),
    EarlierPurity \= purity_impure,
    LaterPurity \= purity_impure,
    EarlierTrace = contains_no_trace_goal,
    LaterTrace = contains_no_trace_goal,

    goal_util.reordering_maintains_termination(ModuleInfo, FullyStrict,
        EarlierGoal, LaterGoal),

    % Don't reorder the goals if the later goal depends on the outputs
    % of the current goal.
    \+ goal_depends_on_goal(EarlierGoal, LaterGoal),

    % Don't reorder the goals if the later goal changes the instantiatedness
    % of any of the non-locals of the earlier goal. This is necessary if the
    % later goal clobbers any of the non-locals of the earlier goal, and
    % avoids rerunning full mode analysis in other cases.
    \+ goal_depends_on_goal(LaterGoal, EarlierGoal).

:- pred goal_depends_on_goal(hlds_goal::in, hlds_goal::in) is semidet.

goal_depends_on_goal(_ - GoalInfo1, _ - GoalInfo2) :-
    goal_info_get_instmap_delta(GoalInfo1, InstmapDelta1),
    instmap_delta_changed_vars(InstmapDelta1, ChangedVars1),
    goal_info_get_nonlocals(GoalInfo2, NonLocals2),
    set.intersect(ChangedVars1, NonLocals2, Intersection),
    \+ set.empty(Intersection).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "pd_util.m".

%-----------------------------------------------------------------------------%
