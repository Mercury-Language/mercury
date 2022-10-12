%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2012 University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File pd_util.m.
% Main author: stayl.
%
% Utility predicates for deforestation and partial evaluation.
%
%---------------------------------------------------------------------------%

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
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.pd_info.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

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
    list(prog_var)::in, pd_branch_info(prog_var)::out) is det.

    % inst_MSG(ModuleInfo, InstA, InstB, InstC):
    %
    % Take the Most Specific Generalisation of two insts. The information
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
:- pred inst_MSG(module_info::in, mer_inst::in, mer_inst::in, mer_inst::out)
    is semidet.

    % Produce an estimate of the size of an inst, based on the number of nodes
    % in the inst. The inst is expanded down to the first repeat of an already
    % expanded inst_name.
    %
:- pred inst_size(module_info::in, mer_inst::in, int::out) is det.
:- pred inst_list_size(module_info::in, list(mer_inst)::in, int::out) is det.

    % goals_match(ModuleInfo, OldGoal, OldArgVars, OldArgTypes,
    %   NewGoal, NewVarTable, OldToNewVarRenaming, OldToNewTypeSubst):
    %
    % Check the shape of the goals, and return a mapping from variables
    % in the old goal to variables in the new and a substitution to apply
    % to the types. This only attempts to match `simple' lists of goals,
    % which contain only conj, some, not and atomic goals, since deforest.m
    % only attempts to optimize those types of conjunctions.
    %
:- pred goals_match(module_info::in, hlds_goal::in, list(prog_var)::in,
    list(mer_type)::in, hlds_goal::in, var_table::in,
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_analysis.
:- import_module check_hlds.det_report.
:- import_module check_hlds.det_util.
:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_info.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module check_hlds.simplify.simplify_proc.
:- import_module check_hlds.unique_modes.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.instmap.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.write_error_spec.
:- import_module transform_hlds.constraint.
:- import_module transform_hlds.pd_debug.

:- import_module assoc_list.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term_context.

%---------------------------------------------------------------------------%

goal_get_calls(Goal0, CalledPreds) :-
    goal_to_conj_list(Goal0, Conjuncts),
    GetCalls =
        ( pred(Goal::in, CalledPred::out) is semidet :-
            Goal = hlds_goal(plain_call(PredId, ProcId, _, _, _, _), _),
            CalledPred = proc(PredId, ProcId)
        ),
    list.filter_map(GetCalls, Conjuncts, CalledPreds).

%---------------------------------------------------------------------------%

propagate_constraints(!Goal, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    module_info_get_globals(ModuleInfo0, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    ConstraintProp = OptTuple ^ ot_prop_local_constraints,
    (
        ConstraintProp = prop_local_constraints,
        Goal0 = !.Goal,
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo,
                "%% Propagating constraints\n", [], !IO),
            pd_debug_output_goal(!.PDInfo, "before constraints\n", Goal0, !IO)
        ),
        pd_info_get_proc_info(!.PDInfo, ProcInfo0),
        pd_info_get_instmap(!.PDInfo, InstMap),
        proc_info_get_var_table(ProcInfo0, VarTable0),
        constraint_info_init(ModuleInfo0, VarTable0, InstMap, CInfo0),
        Goal0 = hlds_goal(_, GoalInfo0),
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        constraint.propagate_constraints_in_goal(!Goal, CInfo0, CInfo),
        constraint_info_deconstruct(CInfo, ModuleInfo, VarTable, Changed),
        pd_info_set_module_info(ModuleInfo, !PDInfo),
        proc_info_set_var_table(VarTable, ProcInfo0, ProcInfo),
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
            find_simplify_tasks(Globals, do_not_generate_warnings,
                SimplifyTasks),
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
        ConstraintProp = do_not_prop_local_constraints
    ).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

unique_modecheck_goal(Goal0, Goal, Errors, !PDInfo) :-
    get_goal_live_vars(!.PDInfo, Goal0, LiveVars),
    unique_modecheck_goal_live_vars(LiveVars, Goal0, Goal, Errors, !PDInfo).

unique_modecheck_goal_live_vars(LiveVars, Goal0, Goal, Errors, !PDInfo) :-
    % Construct a mode_info.
    pd_info_get_pred_proc_id(!.PDInfo, PredProcId),
    PredProcId = proc(PredId, ProcId),
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    pd_info_get_instmap(!.PDInfo, InstMap0),
    Context = dummy_context,
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
            module_info_get_name(ModuleInfo, ModuleName),
            get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
            write_error_specs(DebugStream, Globals, ErrorSpecs, !IO)
        )
    ;
        Debug = no
    ),

    pd_info_set_module_info(ModuleInfo, !PDInfo),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo1),
    pd_info_set_pred_info(PredInfo, !PDInfo),
    % Deconstruct the mode_info.
    mode_info_get_var_table(ModeInfo, VarTable),
    proc_info_set_var_table(VarTable, ProcInfo1, ProcInfo),
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
    get_goal_live_vars_2(ModuleInfo, InstMap, InstMapDelta, NonLocalsList,
        !Vars).

:- pred get_goal_live_vars_2(module_info::in, instmap::in, instmap_delta::in,
    list(prog_var)::in,
    set_of_progvar::in, set_of_progvar::out) is det.

get_goal_live_vars_2(_, _, _, [], !Vars).
get_goal_live_vars_2(ModuleInfo, InstMap, InstMapDelta, [NonLocal | NonLocals],
        !Vars) :-
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
    get_goal_live_vars_2(ModuleInfo, InstMap, InstMapDelta, NonLocals, !Vars).

%---------------------------------------------------------------------------%

:- pred rerun_det_analysis(hlds_goal::in, hlds_goal::out,
    pd_info::in, pd_info::out) is det.

rerun_det_analysis(Goal0, Goal, !PDInfo) :-
    Goal0 = hlds_goal(_, GoalInfo0),

    Detism = goal_info_get_determinism(GoalInfo0),
    det_get_soln_context(Detism, SolnContext),

    % det_infer_goal looks up the proc_info in the module_info for the
    % var_table, so we'd better stick them back in the module_info.
    pd_info_get_pred_proc_id(!.PDInfo, PredProcId),
    pd_info_get_pred_info(!.PDInfo, PredInfo),
    pd_info_get_proc_info(!.PDInfo, ProcInfo),
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    module_info_set_pred_proc_info(PredProcId, PredInfo, ProcInfo,
        ModuleInfo0, ModuleInfo1),

    proc_info_get_var_table(ProcInfo, VarTable),
    det_info_init(ModuleInfo1, PredProcId, VarTable,
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

%---------------------------------------------------------------------------%

:- type pd_var_info == branch_info_map(prog_var).

get_branch_vars_proc(PredProcId, ProcInfo, !ModuleInfo, !ArgInfo) :-
    proc_info_get_goal(ProcInfo, Goal),
    proc_info_get_var_table(ProcInfo, VarTable),
    instmap.init_reachable(InstMap0),
    map.init(Vars0),
    set_of_var.init(LeftVars0),
    goal_to_conj_list(Goal, Conjuncts),
    ( if
        get_branch_vars_goal_2(!.ModuleInfo, VarTable, InstMap0, no, Conjuncts,
            LeftVars0, LeftVars, Vars0, Vars)
    then
        proc_info_get_headvars(ProcInfo, HeadVars),
        map.init(ThisProcArgMap0),
        set.init(ThisProcLeftArgs0),
        get_extra_info_headvars(Vars, LeftVars, 1, HeadVars,
            ThisProcArgMap0, ThisProcArgMap1,
            ThisProcLeftArgs0, ThisProcLeftArgs),
        set.init(OpaqueArgs0),
        BranchInfo0 = pd_branch_info(ThisProcArgMap1, ThisProcLeftArgs,
            OpaqueArgs0),
        map.set(PredProcId, BranchInfo0, !ArgInfo),

        % Look for opportunities for deforestation in the sub-branches
        % of the top-level goal.
        get_sub_branch_vars_goal(!.ArgInfo, VarTable, InstMap0, Conjuncts,
            Vars, AllVars, !ModuleInfo),
        get_extra_info_headvars(AllVars, LeftVars0, 1, HeadVars,
            ThisProcArgMap0, ThisProcArgMap, ThisProcLeftArgs0, _),

        proc_info_get_argmodes(ProcInfo, ArgModes),
        get_opaque_args(!.ModuleInfo, ThisProcArgMap, 1, ArgModes,
            OpaqueArgs0, OpaqueArgs),

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
:- pred get_opaque_args(module_info::in, branch_info_map(int)::in,
    int::in, list(mer_mode)::in, set(int)::in, set(int)::out) is det.

get_opaque_args(_, _, _, [], !OpaqueArgs).
get_opaque_args(ModuleInfo, ExtraInfoArgs, ArgNo, [ArgMode | ArgModes],
        !OpaqueArgs) :-
    ( if
        mode_is_output(ModuleInfo, ArgMode),
        not map.contains(ExtraInfoArgs, ArgNo)
    then
        set.insert(ArgNo, !OpaqueArgs)
    else
        true
    ),
    NextArg = ArgNo + 1,
    get_opaque_args(ModuleInfo, ExtraInfoArgs, NextArg, ArgModes, !OpaqueArgs).

    % From the information about variables for which we have extra information
    % in the branches, compute the argument numbers for which we have extra
    % information.
    %
:- pred get_extra_info_headvars(pd_var_info::in, set_of_progvar::in,
    int::in, list(prog_var)::in,
    branch_info_map(int)::in, branch_info_map(int)::out,
    set(int)::in, set(int)::out) is det.

get_extra_info_headvars(_, _, _, [], !Args, !LeftArgs).
get_extra_info_headvars(VarInfo, LeftVars, ArgNo, [HeadVar | HeadVars],
        !ThisProcArgs, !ThisProcLeftVars) :-
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
    get_extra_info_headvars(VarInfo, LeftVars, ArgNo + 1, HeadVars,
        !ThisProcArgs, !ThisProcLeftVars).

%---------------------------------------------------------------------------%

get_branch_vars_goal(Goal, MaybeBranchInfo, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    pd_info_get_instmap(!.PDInfo, InstMap0),
    pd_info_get_proc_arg_info(!.PDInfo, ProcArgInfo),
    pd_info_get_proc_info(!.PDInfo, ProcInfo),
    proc_info_get_var_table(ProcInfo, VarTable),
    set_of_var.init(LeftVars0),
    map.init(Vars0),
    ( if
        get_branch_vars_goal_2(ModuleInfo0, VarTable, InstMap0, no, [Goal],
            LeftVars0, LeftVars, Vars0, Vars1)
    then
        get_sub_branch_vars_goal(ProcArgInfo, VarTable, InstMap0,
            [Goal], Vars1, Vars, ModuleInfo0, ModuleInfo),
        pd_info_set_module_info(ModuleInfo, !PDInfo),

        % OpaqueVars is only filled in for calls.
        set.init(OpaqueVars),
        MaybeBranchInfo = yes(pd_branch_info(Vars,
            set_of_var.bitset_to_set(LeftVars), OpaqueVars))
    else
        MaybeBranchInfo = no
    ).

:- pred get_branch_vars_goal_2(module_info::in, var_table::in, instmap::in,
    bool::in, list(hlds_goal)::in, set_of_progvar::in, set_of_progvar::out,
    pd_var_info::in, pd_var_info::out) is semidet.

get_branch_vars_goal_2(_, _, _, yes, [], !LeftVars, !Vars).
get_branch_vars_goal_2(ModuleInfo, VarTable, InstMap0, !.FoundBranch,
        [Goal | Goals], !LeftVars, !Vars) :-
    Goal = hlds_goal(_, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, InstMap0, InstMap),
    ( if get_branch_instmap_deltas(Goal, InstMapDeltas) then
        % Only look for goals with one top-level branched goal,
        % since deforestation of goals with more than one is likely to be
        % less productive.
        !.FoundBranch = no,
        get_branch_vars(ModuleInfo, InstMap, Goal, 1, InstMapDeltas, !Vars),
        get_left_vars(Goal, !LeftVars),
        !:FoundBranch = yes
    else
        Goal = hlds_goal(GoalExpr, _),
        goal_expr_has_subgoals(GoalExpr) = does_not_have_subgoals
    ),
    get_branch_vars_goal_2(ModuleInfo, VarTable, InstMap, !.FoundBranch,
        Goals, !LeftVars, !Vars).

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

:- pred get_branch_vars(module_info::in, instmap::in, hlds_goal::in,
    int::in, list(instmap_delta)::in,
    pd_var_info::in, pd_var_info::out) is semidet.

get_branch_vars(_, _, _, _, [], !ExtraVars).
get_branch_vars(ModuleInfo, InstMap, Goal, BranchNo,
        [InstMapDelta | InstMapDeltas], !ExtraVars) :-
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
    get_branch_vars(ModuleInfo, InstMap, Goal, BranchNo + 1,
        InstMapDeltas, !ExtraVars).

    % Look at the goals in the branches for extra information.
    %
:- pred get_sub_branch_vars_goal(pd_arg_info::in, var_table::in, instmap::in,
    list(hlds_goal)::in,
    branch_info_map(prog_var)::in, branch_info_map(prog_var)::out,
    module_info::in, module_info::out) is det.

get_sub_branch_vars_goal(_, _, _, [], Vars, Vars, !Module).
get_sub_branch_vars_goal(ProcArgInfo, VarTable, InstMap0, [Goal | Goals],
        !.Vars, SubVars, !ModuleInfo) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = if_then_else(_, Cond, Then, Else),
        Cond = hlds_goal(_, CondInfo),
        CondDelta = goal_info_get_instmap_delta(CondInfo),
        apply_instmap_delta(CondDelta, InstMap0, InstMap1),
        goal_to_conj_list(Then, ThenList),
        examine_branch(!.ModuleInfo, ProcArgInfo, VarTable, InstMap1,
            1, ThenList, !Vars),
        goal_to_conj_list(Else, ElseList),
        examine_branch(!.ModuleInfo, ProcArgInfo, VarTable, InstMap0,
            2, ElseList, !Vars)
    ;
        GoalExpr = disj(SubGoals),
        examine_branch_list(!.ModuleInfo, ProcArgInfo, VarTable, InstMap0,
            1, SubGoals, !Vars)
    ;
        GoalExpr = switch(Var, _, Cases),
        examine_case_list(ProcArgInfo, VarTable, InstMap0, Var,
            1, Cases, !Vars, !ModuleInfo)
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
    get_sub_branch_vars_goal(ProcArgInfo, VarTable, InstMap, Goals,
        !.Vars, SubVars, !ModuleInfo).

:- pred examine_branch_list(module_info::in, pd_arg_info::in,  var_table::in,
    instmap::in,int::in, list(hlds_goal)::in,
    branch_info_map(prog_var)::in, branch_info_map(prog_var)::out) is det.

examine_branch_list(_, _, _, _, _, [], !Vars).
examine_branch_list(ModuleInfo, ProcArgInfo, VarTable, InstMap,
        BranchNo, [Goal | Goals], !Vars) :-
    goal_to_conj_list(Goal, Conjuncts),
    examine_branch(ModuleInfo, ProcArgInfo, VarTable, InstMap,
        BranchNo, Conjuncts, !Vars),
    examine_branch_list(ModuleInfo, ProcArgInfo, VarTable, InstMap,
        BranchNo + 1, Goals, !Vars).

:- pred examine_case_list(pd_arg_info::in, var_table::in, instmap::in,
    prog_var::in, int::in, list(case)::in,
    branch_info_map(prog_var)::in, branch_info_map(prog_var)::out,
    module_info::in, module_info::out) is det.

examine_case_list(_, _, _, _, _, [], !Vars, !ModuleInfo).
examine_case_list(ProcArgInfo, VarTable, InstMap0, Var, BranchNo,
        [Case | Cases], !Vars, !ModuleInfo) :-
    lookup_var_type(VarTable, Var, Type),
    Case = case(MainConsId, OtherConsIds, Goal),
    bind_var_to_functors(Var, Type, MainConsId, OtherConsIds,
        InstMap0, InstMap1, !ModuleInfo),
    goal_to_conj_list(Goal, Conjuncts),
    examine_branch(!.ModuleInfo, ProcArgInfo, VarTable, InstMap1,
        BranchNo, Conjuncts, !Vars),
    NextBranch = BranchNo + 1,
    examine_case_list(ProcArgInfo, VarTable, InstMap0, Var, NextBranch,
        Cases, !Vars, !ModuleInfo).

:- pred examine_branch(module_info::in, pd_arg_info::in,  var_table::in,
    instmap::in, int::in, list(hlds_goal)::in,
    branch_info_map(prog_var)::in, branch_info_map(prog_var)::out) is det.

examine_branch(_, _, _, _, _, [], !Vars).
examine_branch(ModuleInfo, ProcArgInfo, VarTable, InstMap,
        BranchNo, [Goal | Goals], !Vars) :-
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
        get_branch_vars_goal_2(ModuleInfo, VarTable, InstMap, no, [Goal],
            LeftVars0, _, !Vars)
    then
        map.keys(!.Vars, ExtraVars2),
        combine_vars(BranchNo, ExtraVars2, !Vars)
    else
        true
    ),
    Goal = hlds_goal(_, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, InstMap, InstMap1),
    examine_branch(ModuleInfo, ProcArgInfo, VarTable, InstMap1,
        BranchNo, Goals, !Vars).

:- pred combine_vars(int::in, list(prog_var)::in,
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

%---------------------------------------------------------------------------%

pd_requantify_goal(NonLocals, Goal0, Goal, !PDInfo) :-
    some [!ProcInfo] (
        pd_info_get_proc_info(!.PDInfo, !:ProcInfo),
        proc_info_get_var_table(!.ProcInfo, VarTable0),
        proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
        implicitly_quantify_goal_general(ord_nl_no_lambda, NonLocals, _,
            Goal0, Goal, VarTable0, VarTable, RttiVarMaps0, RttiVarMaps),
        proc_info_set_var_table(VarTable, !ProcInfo),
        proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),
        pd_info_set_proc_info(!.ProcInfo, !PDInfo)
    ).

pd_recompute_instmap_delta(Goal0, Goal, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    pd_info_get_instmap(!.PDInfo, InstMap),
    pd_info_get_proc_info(!.PDInfo, ProcInfo),
    proc_info_get_var_table(ProcInfo, VarTable),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    recompute_instmap_delta(recomp_atomics, VarTable, InstVarSet,
        InstMap, Goal0, Goal, ModuleInfo0, ModuleInfo),
    pd_info_set_module_info(ModuleInfo, !PDInfo).

%---------------------------------------------------------------------------%

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
    list(prog_var)::in, pd_var_info::in, pd_var_info::out) is det.

convert_branch_info_2([], _, !VarInfo).
convert_branch_info_2([ArgNo - Branches | ArgInfos], ArgVars, !VarInfo) :-
    list.det_index1(ArgVars, ArgNo, ArgVar),
    map.set(ArgVar, Branches, !VarInfo),
    convert_branch_info_2(ArgInfos, ArgVars, !VarInfo).

%---------------------------------------------------------------------------%

inst_MSG(ModuleInfo, InstA, InstB, Inst) :-
    set.init(Expansions),
    inst_MSG_1(ModuleInfo, Expansions, InstA, InstB, Inst).

:- type expansions == set(pair(mer_inst)).

:- pred inst_MSG_1(module_info::in, expansions::in, mer_inst::in, mer_inst::in,
    mer_inst::out) is semidet.

inst_MSG_1(ModuleInfo, !.Expansions, InstA, InstB, Inst) :-
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
            inst_MSG_2(ModuleInfo, !.Expansions, InstA2, InstB2, Inst)
        )
    ).

:- pred inst_MSG_2(module_info::in, expansions::in, mer_inst::in, mer_inst::in,
    mer_inst::out) is semidet.

inst_MSG_2(ModuleInfo, Expansions, InstA, InstB, Inst) :-
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
        bound_inst_list_MSG(ModuleInfo, Expansions, UniqB,
            BoundInstsA, BoundInstsB, BoundInstsB, Inst)
    ;
        InstA = any(_, _),
        InstB = any(_, _),
        Inst = InstB
    ;
        InstA = abstract_inst(Name, ArgsA),
        InstB = abstract_inst(Name, ArgsB),
        inst_list_MSG(ModuleInfo, Expansions, ArgsA, ArgsB, Args),
        Inst = abstract_inst(Name, Args)
    ).

:- pred inst_list_MSG(module_info::in, expansions::in,
    list(mer_inst)::in, list(mer_inst)::in, list(mer_inst)::out) is semidet.

inst_list_MSG(_, _, [], [], []).
inst_list_MSG(ModuleInfo, Expansions, [ArgA | ArgsA], [ArgB | ArgsB],
        [Arg | Args]) :-
    inst_MSG_1(ModuleInfo, Expansions, ArgA, ArgB, Arg),
    inst_list_MSG(ModuleInfo, Expansions, ArgsA, ArgsB, Args).

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
:- pred bound_inst_list_MSG(module_info::in, expansions::in, uniqueness::in,
    list(bound_inst)::in, list(bound_inst)::in, list(bound_inst)::in,
    mer_inst::out) is semidet.

bound_inst_list_MSG(ModuleInfo, Expansions, Uniq, Xs, Ys, BoundInsts, Inst) :-
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
        inst_list_MSG(ModuleInfo, Expansions, ArgsX, ArgsY, Args),
        Z = bound_functor(ConsId, Args),
        bound_inst_list_MSG(ModuleInfo, Expansions, Uniq, Xs1, Ys1,
            BoundInsts, Inst1),
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

%---------------------------------------------------------------------------%

inst_size(ModuleInfo, Inst, Size) :-
    set.init(Expansions),
    inst_size_2(ModuleInfo, Expansions, Inst, Size).

:- pred inst_size_2(module_info::in, set(inst_name)::in,
    mer_inst::in, int::out) is det.

inst_size_2(ModuleInfo, !.Expansions, Inst, Size) :-
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
        inst_size_2(ModuleInfo, !.Expansions, SubInst, Size)
    ;
        Inst = defined_inst(InstName),
        ( if set.member(InstName, !.Expansions) then
            Size = 1
        else
            set.insert(InstName, !Expansions),
            inst_lookup(ModuleInfo, InstName, SubInst),
            inst_size_2(ModuleInfo, !.Expansions, SubInst, Size)
        )
    ;
        Inst = bound(_, _, BoundInsts),
        bound_inst_size(ModuleInfo, !.Expansions, BoundInsts, 1, Size)
    ).

:- pred bound_inst_size(module_info::in, set(inst_name)::in,
    list(bound_inst)::in, int::in, int::out) is det.

bound_inst_size(_, _, [], !Size).
bound_inst_size(ModuleInfo, Expansions, [BoundInst | BoundInsts], !Size) :-
    BoundInst = bound_functor(_, ArgInsts),
    inst_list_size(ModuleInfo, Expansions, ArgInsts, !Size),
    !:Size = !.Size + 1,
    bound_inst_size(ModuleInfo, Expansions, BoundInsts, !Size).

inst_list_size(ModuleInfo, Insts, Size) :-
    set.init(Expansions),
    inst_list_size(ModuleInfo, Expansions, Insts, 0, Size).

:- pred inst_list_size(module_info::in, set(inst_name)::in,
    list(mer_inst)::in, int::in, int::out) is det.

inst_list_size(_, _, [], !Size).
inst_list_size(ModuleInfo, Expansions, [Inst | Insts], !Size) :-
    inst_size_2(ModuleInfo, Expansions, Inst, InstSize),
    !:Size = !.Size + InstSize,
    inst_list_size(ModuleInfo, Expansions, Insts, !Size).

%---------------------------------------------------------------------------%

goals_match(_ModuleInfo, OldGoal, OldArgVars, OldArgTypes,
        NewGoal, NewVarTable, OldNewRenaming, TypeSubn) :-
    goal_to_conj_list(OldGoal, OldConjuncts),
    goal_to_conj_list(NewGoal, NewConjuncts),
    map.init(OldNewRenaming0),
    goals_match_2(OldConjuncts, NewConjuncts, OldNewRenaming0, OldNewRenaming),

    % Check that the goal produces a superset of the outputs of the
    % version we are searching for.
    list.map(map.search(OldNewRenaming), OldArgVars, NewArgVars),
    NewGoal = hlds_goal(_, NewGoalInfo),
    NewNonLocals = goal_info_get_nonlocals(NewGoalInfo),
    set_of_var.delete_list(NewArgVars, NewNonLocals, UnmatchedNonLocals),
    set_of_var.is_empty(UnmatchedNonLocals),

    % Check that argument types of NewGoal are subsumed by those of OldGoal.
    collect_matching_arg_types(OldNewRenaming, OldArgVars, OldArgTypes,
        [], MatchingArgTypes),
    lookup_var_types(NewVarTable, NewArgVars, NewArgTypes),
    type_list_subsumes(MatchingArgTypes, NewArgTypes, TypeSubn).

:- pred collect_matching_arg_types(map(prog_var, prog_var)::in,
    list(prog_var)::in, list(mer_type)::in,
    list(mer_type)::in, list(mer_type)::out) is det.

collect_matching_arg_types(_, [], [], !MatchingTypes) :-
    list.reverse(!MatchingTypes).
collect_matching_arg_types(_, [_ | _], [], !MatchingTypes) :-
    unexpected($pred, "list length mismatch").
collect_matching_arg_types(_, [], [_ | _], !MatchingTypes) :-
    unexpected($pred, "list length mismatch").
collect_matching_arg_types(Renaming, [ArgVar | ArgVars], [Type | Types],
        !MatchingTypes) :-
    ( if map.contains(Renaming, ArgVar) then
        !:MatchingTypes = [Type | !.MatchingTypes]
    else
        true
    ),
    collect_matching_arg_types(Renaming, ArgVars, Types, !MatchingTypes).

    % Check that the shape of the goals matches, and that there is a mapping
    % from the variables in the old goal to the variables in the new goal.
    %
:- pred goals_match_2(list(hlds_goal)::in, list(hlds_goal)::in,
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
        goal_to_conj_list(OldSubGoal, OldSubConjuncts),
        goal_to_conj_list(NewSubGoal, NewSubConjuncts),
        goals_match_2(OldSubConjuncts, NewSubConjuncts, !ONRenaming)
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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
:- end_module transform_hlds.pd_util.
%---------------------------------------------------------------------------%
