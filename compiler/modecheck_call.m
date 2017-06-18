%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2001, 2003-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: modecheck_call.m.
% Main author: fjh.
%
% This file contains the code to modecheck a call.
%
% Check that there is a mode declaration for the predicate which matches
% the current instantiation of the arguments. (Also handle calls to implied
% modes.) If the called predicate is one for which we must infer the modes,
% then a new mode for the called predicate whose initial insts are the result
% of normalising the current inst of the arguments.
%
%---------------------------------------------------------------------------%

:- module check_hlds.modecheck_call.
:- interface.

:- import_module check_hlds.mode_info.
:- import_module check_hlds.modecheck_util.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred modecheck_call_pred(pred_id::in, maybe(determinism)::in,
    proc_id::in, proc_id::out, list(prog_var)::in, list(prog_var)::out,
    hlds_goal_info::in, extra_goals::out, mode_info::in, mode_info::out)
    is det.

:- pred modecheck_higher_order_call(pred_or_func::in, prog_var::in,
    list(prog_var)::in, list(prog_var)::out, list(mer_mode)::out,
    determinism::out, extra_goals::out,
    mode_info::in, mode_info::out) is det.

:- pred modecheck_event_call(list(mer_mode)::in,
    list(prog_var)::in, list(prog_var)::out,
    mode_info::in, mode_info::out) is det.

:- pred modecheck_builtin_cast(list(mer_mode)::in,
    list(prog_var)::in, list(prog_var)::out, determinism::out,
    extra_goals::out, mode_info::in, mode_info::out) is det.

    % Given two modes of a predicate, figure out whether they are
    % indistinguishable; that is, whether any valid call to one mode
    % would also be a valid call to the other. (If so, it is a mode error.)
    % Note that mode declarations which only have different final insts
    % do not count as distinguishable.
    %
:- pred modes_are_indistinguishable(proc_id::in, proc_id::in, pred_info::in,
    module_info::in) is semidet.

    % Given two modes of a predicate, figure out whether they are identical,
    % except that one is cc_nondet/cc_multi and the other is nondet/multi.
    % This is used by determinism analysis to substitute a multi mode
    % for a cc_multi one if the call occurs in a non-cc context.
    %
:- pred modes_are_identical_bar_cc(proc_id::in, proc_id::in, pred_info::in,
    module_info::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.unify_proc.
:- import_module hlds.instmap.
:- import_module hlds.vartypes.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module map.
:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

modecheck_call_pred(PredId, DeterminismKnown, ProcId0, TheProcId,
        ArgVars0, ArgVars, _GoalInfo, ExtraGoals, !ModeInfo) :-
    mode_info_get_may_change_called_proc(!.ModeInfo, MayChangeCalledProc),
    mode_info_get_preds(!.ModeInfo, Preds),
    map.lookup(Preds, PredId, PredInfo),
    pred_info_get_proc_table(PredInfo, Procs),
    (
        MayChangeCalledProc = may_not_change_called_proc,
        ( if ProcId0 = invalid_proc_id then
            unexpected($module, $pred, "invalid proc_id")
        else
            ProcIds = [ProcId0]
        )
    ;
        MayChangeCalledProc = may_change_called_proc,
        % Get the list of different possible modes for the called predicate.
        ProcIds = pred_info_all_procids(PredInfo)
    ),

    compute_arg_offset(PredInfo, ArgOffset),
    pred_info_get_markers(PredInfo, Markers),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    ( if
        % In order to give better diagnostics, we handle the cases where there
        % are zero or one modes for the called predicate specially.
        %
        ProcIds = [],
        not check_marker(Markers, marker_infer_modes)
    then
        set_of_var.init(WaitingVars),
        mode_info_error(WaitingVars, mode_error_no_mode_decl, !ModeInfo),
        TheProcId = invalid_proc_id,
        ArgVars = ArgVars0,
        ExtraGoals = no_extra_goals
    else if
        ProcIds = [ProcId],
        ( not check_marker(Markers, marker_infer_modes)
        ; MayChangeCalledProc = may_not_change_called_proc
        )
    then
        TheProcId = ProcId,
        map.lookup(Procs, ProcId, ProcInfo),

        % Check that `ArgsVars0' have livenesses which match the
        % expected livenesses.
        %
        mode_info_get_module_info(!.ModeInfo, ModuleInfo),
        proc_info_arglives(ProcInfo, ModuleInfo, ProcArgLives0),
        modecheck_var_list_is_live_no_exact_match(ArgVars0, ProcArgLives0,
            ArgOffset, !ModeInfo),

        % Check that `ArgsVars0' have insts which match the expected
        % initial insts, and set their new final insts (introducing
        % extra unifications for implied modes, if necessary).
        %
        proc_info_get_argmodes(ProcInfo, ProcArgModes0),
        proc_info_get_inst_varset(ProcInfo, ProcInstVarSet),
        mode_info_get_instvarset(!.ModeInfo, InstVarSet0),
        rename_apart_inst_vars(InstVarSet0, ProcInstVarSet, InstVarSet,
            ProcArgModes0, ProcArgModes),
        mode_info_set_instvarset(InstVarSet, !ModeInfo),
        mode_list_get_initial_insts(ModuleInfo, ProcArgModes, InitialInsts),
        modecheck_var_has_inst_list_no_exact_match(ArgVars0, InitialInsts,
            ArgOffset, InstVarSub, !ModeInfo),

        modecheck_end_of_call(ProcInfo, ProcArgModes, ArgVars0,
            ArgOffset, InstVarSub, ArgVars, ExtraGoals, !ModeInfo)
    else
        % Set the current error list to empty (and save the old one in
        % `OldErrors'). This is so the test for `Errors = []' in
        % find_matching_modes will work.
        mode_info_get_errors(!.ModeInfo, OldErrors),
        mode_info_set_errors([], !ModeInfo),

        set_of_var.init(WaitingVars0),
        modecheck_find_matching_modes(ProcIds, PredId, Procs, ArgVars0,
            [], RevMatchingProcIds, WaitingVars0, WaitingVars1, !ModeInfo),

        (
            RevMatchingProcIds = [],
            no_matching_modes(PredId, ArgVars0, DeterminismKnown,
                WaitingVars1, TheProcId, !ModeInfo),
            ArgVars = ArgVars0,
            ExtraGoals = no_extra_goals
        ;
            RevMatchingProcIds = [_ | _],
            list.reverse(RevMatchingProcIds, MatchingProcIds),
            choose_best_match(!.ModeInfo, MatchingProcIds, PredId, Procs,
                ArgVars0, TheProcId, InstVarSub, ProcArgModes),
            map.lookup(Procs, TheProcId, ProcInfo),
            proc_info_get_mode_errors(ProcInfo, CalleeModeErrors),
            (
                CalleeModeErrors = [_ | _],
                % mode error in callee for this mode
                ArgVars = ArgVars0,
                WaitingVars = set_of_var.list_to_set(ArgVars),
                ExtraGoals = no_extra_goals,
                instmap_lookup_vars(InstMap, ArgVars, ArgInsts),
                mode_info_set_call_arg_context(0, !ModeInfo),
                mode_info_error(WaitingVars,
                    mode_error_in_callee(ArgVars, ArgInsts, PredId, TheProcId,
                        CalleeModeErrors),
                    !ModeInfo)
            ;
                CalleeModeErrors = [],
                modecheck_end_of_call(ProcInfo, ProcArgModes, ArgVars0,
                    ArgOffset, InstVarSub, ArgVars, ExtraGoals, !ModeInfo)
            )
        ),

        % Restore the error list, appending any new error(s).
        mode_info_get_errors(!.ModeInfo, NewErrors),
        list.append(OldErrors, NewErrors, Errors),
        mode_info_set_errors(Errors, !ModeInfo)
    ).

%---------------------%

:- type proc_mode
    --->    proc_mode(proc_id, inst_var_sub, list(mer_mode)).

:- pred modecheck_find_matching_modes(list(proc_id)::in, pred_id::in,
    proc_table::in, list(prog_var)::in, list(proc_mode)::in,
    list(proc_mode)::out, set_of_progvar::in, set_of_progvar::out,
    mode_info::in, mode_info::out) is det.

modecheck_find_matching_modes([], _PredId, _Procs, _ArgVars,
        !MatchingProcIds, !WaitingVars, !ModeInfo).
modecheck_find_matching_modes([ProcId | ProcIds], PredId, Procs, ArgVars0,
        !MatchingProcIds, !WaitingVars, !ModeInfo) :-
    % Find the initial insts and the final livenesses of the arguments
    % for this mode of the called pred.
    map.lookup(Procs, ProcId, ProcInfo),
    proc_info_get_argmodes(ProcInfo, ProcArgModes0),
    proc_info_get_inst_varset(ProcInfo, ProcInstVarSet),
    mode_info_get_instvarset(!.ModeInfo, InstVarSet0),
    rename_apart_inst_vars(InstVarSet0, ProcInstVarSet, InstVarSet,
        ProcArgModes0, ProcArgModes),
    mode_info_set_instvarset(InstVarSet, !ModeInfo),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    proc_info_arglives(ProcInfo, ModuleInfo, ProcArgLives0),

    % Check whether the livenesses of the args matches their expected liveness.
    modecheck_var_list_is_live_no_exact_match(ArgVars0, ProcArgLives0, 0,
        !ModeInfo),

    % Check whether the insts of the args matches their expected initial insts.
    %
    % If we're doing mode inference for the called procedure, and the
    % called procedure has been inferred as an invalid mode, then don't use
    % it unless it is an exact match.
    %
    % XXX Do we really want mode inference to use implied modes?
    % Would it be better to always require an exact match when doing mode
    % inference, to ensure that we add new inferred modes rather than using
    % implied modes?

    mode_list_get_initial_insts(ModuleInfo, ProcArgModes, InitialInsts),
    ( if proc_info_is_valid_mode(ProcInfo) then
        modecheck_var_has_inst_list_no_exact_match(ArgVars0, InitialInsts, 0,
            InstVarSub, !ModeInfo)
    else
        modecheck_var_has_inst_list_exact_match(ArgVars0, InitialInsts, 0,
            InstVarSub, !ModeInfo)
    ),

    % If we got an error, reset the error list and save the list of vars
    % to wait on. Otherwise, insert the proc_id in the list of matching
    % proc_ids.
    mode_info_get_errors(!.ModeInfo, Errors),
    (
        Errors = [FirstError | _],
        mode_info_set_errors([], !ModeInfo),
        FirstError = mode_error_info(ErrorWaitingVars, _, _, _),
        set_of_var.union(ErrorWaitingVars, !WaitingVars)
    ;
        Errors = [],
        NewMatch = proc_mode(ProcId, InstVarSub, ProcArgModes),
        !:MatchingProcIds = [NewMatch | !.MatchingProcIds]
    ),

    % Keep trying with the other modes for the called pred.
    modecheck_find_matching_modes(ProcIds, PredId, Procs, ArgVars0,
        !MatchingProcIds, !WaitingVars, !ModeInfo).

%---------------------%

:- pred no_matching_modes(pred_id::in, list(prog_var)::in,
    maybe(determinism)::in, set_of_progvar::in, proc_id::out,
    mode_info::in, mode_info::out) is det.

no_matching_modes(PredId, ArgVars, DeterminismKnown, WaitingVars, TheProcId,
        !ModeInfo) :-
    % There were no matching modes.
    % If we are inferring modes for this called predicate, then
    % just insert a new mode declaration which will match.
    % Otherwise, report an error.

    mode_info_get_preds(!.ModeInfo, Preds),
    map.lookup(Preds, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    ( if check_marker(Markers, marker_infer_modes) then
        insert_new_mode(PredId, ArgVars, DeterminismKnown, TheProcId,
            !ModeInfo),
        % We don't yet know the final insts for the newly created mode
        % of the called predicate, so we set the instmap to unreachable,
        % indicating that we have no information about the modes at this
        % point in the computation.
        instmap.init_unreachable(Instmap),
        mode_info_set_instmap(Instmap, !ModeInfo)
    else
        TheProcId = invalid_proc_id,    % dummy value
        mode_info_get_instmap(!.ModeInfo, InstMap),
        instmap_lookup_vars(InstMap, ArgVars, ArgInsts),
        mode_info_set_call_arg_context(0, !ModeInfo),
        mode_info_error(WaitingVars,
            mode_error_no_matching_mode(ArgVars, ArgInsts), !ModeInfo)
    ).

    % Insert a new inferred mode for a predicate.
    % The initial insts are determined by using a normalised
    % version of the call pattern (i.e. the insts of the arg vars).
    % The final insts are initially just assumed to be all `not_reached'.
    % The determinism for this mode will be inferred.
    %
:- pred insert_new_mode(pred_id::in, list(prog_var)::in,
    maybe(determinism)::in, proc_id::out,
    mode_info::in, mode_info::out) is det.

insert_new_mode(PredId, ArgVars, MaybeDet, ProcId, !ModeInfo) :-
    % Figure out the values of all the variables we need
    % to create a new mode for this predicate.
    get_var_insts_and_lives(!.ModeInfo, ArgVars, InitialInsts, ArgLives),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    module_info_get_preds(ModuleInfo0, Preds0),
    map.lookup(Preds0, PredId, PredInfo0),
    pred_info_get_context(PredInfo0, Context),
    list.length(ArgVars, Arity),
    list.duplicate(Arity, not_reached, FinalInsts),
    inst_lists_to_mode_list(InitialInsts, FinalInsts, Modes),
    mode_info_get_instvarset(!.ModeInfo, InstVarSet),

    % Call unify_proc.request_proc, which will create the new procedure,
    % set its "can-process" flag to `no', and insert it into the queue
    % of requested procedures.
    unify_proc.request_proc(PredId, Modes, InstVarSet, yes(ArgLives),
        MaybeDet, Context, ProcId, ModuleInfo0, ModuleInfo),

    mode_info_set_module_info(ModuleInfo, !ModeInfo),

    % Since we've created a new inferred mode for this predicate,
    % things have changed, so we will need to do at least one more
    % pass of the fixpoint analysis.
    mode_info_set_changed_flag(yes, !ModeInfo).

%---------------------%

:- pred modecheck_end_of_call(proc_info::in, list(mer_mode)::in,
    list(prog_var)::in, int::in, inst_var_sub::in, list(prog_var)::out,
    extra_goals::out, mode_info::in, mode_info::out) is det.

modecheck_end_of_call(ProcInfo, ProcArgModes, ArgVars0, ArgOffset,
        InstVarSub, ArgVars, ExtraGoals, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_list_get_initial_insts(ModuleInfo, ProcArgModes, InitialInsts0),
    inst_list_apply_substitution(InstVarSub, InitialInsts0, InitialInsts),
    mode_list_get_final_insts(ModuleInfo, ProcArgModes, FinalInsts0),
    inst_list_apply_substitution(InstVarSub, FinalInsts0, FinalInsts),
    modecheck_set_var_inst_list(ArgVars0, InitialInsts, FinalInsts,
        ArgOffset, ArgVars, ExtraGoals, !ModeInfo),
    proc_info_never_succeeds(ProcInfo, NeverSucceeds),
    (
        NeverSucceeds = yes,
        instmap.init_unreachable(Instmap),
        mode_info_set_instmap(Instmap, !ModeInfo)
    ;
        NeverSucceeds = no
    ).

%---------------------------------------------------------------------------%

modecheck_higher_order_call(PredOrFunc, PredVar, Args0, Args, Modes, Det,
        ExtraGoals, !ModeInfo) :-
    % First, check that `PredVar' has a higher-order pred inst
    % (of the appropriate arity).
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap_lookup_var(InstMap0, PredVar, PredVarInst0),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    inst_expand(ModuleInfo0, PredVarInst0, PredVarInst),
    list.length(Args0, Arity),
    ( if
        (
            PredVarInst = ground(_Uniq, HOInstInfo)
        ;
            PredVarInst = any(_Uniq, HOInstInfo)
        ),
        (
            HOInstInfo = higher_order(PredInstInfo)
        ;
            % If PredVar has no higher-order inst information, but is
            % a function type, then assume the default function mode.
            HOInstInfo = none_or_default_func,
            mode_info_get_var_types(!.ModeInfo, VarTypes),
            lookup_var_type(VarTypes, PredVar, Type),
            type_is_higher_order_details(Type, _, pf_function, _, ArgTypes),
            PredInstInfo = pred_inst_info_default_func_mode(
                list.length(ArgTypes))
        ),
        PredInstInfo = pred_inst_info(PredOrFunc, ModesPrime, _, DetPrime),
        list.length(ModesPrime, Arity)
    then
        ( if
            % If PredVar is inst `any' then it gets bound. If it is locked,
            % this is a mode error.
            PredVarInst = any(A, B),
            mode_info_var_is_locked(!.ModeInfo, PredVar, Reason)
        then
            BetterPredVarInst = ground(A, B),
            WaitingVars = set_of_var.make_singleton(PredVar),
            mode_info_error(WaitingVars, mode_error_bind_var(Reason, PredVar,
                PredVarInst, BetterPredVarInst), !ModeInfo),
            Modes = [],
            Det = detism_erroneous,
            Args = Args0,
            ExtraGoals = no_extra_goals
        else
            Det = DetPrime,
            Modes = ModesPrime,
            ArgOffset = 1,
            modecheck_arg_list(ArgOffset, Modes, ExtraGoals, Args0, Args,
                !ModeInfo),

            ( if determinism_components(Det, _, at_most_zero) then
                instmap.init_unreachable(Instmap),
                mode_info_set_instmap(Instmap, !ModeInfo)
            else
                true
            )
        )
    else
        % The error occurred in argument 1, i.e. the pred term.
        mode_info_set_call_arg_context(1, !ModeInfo),
        WaitingVars = set_of_var.make_singleton(PredVar),
        mode_info_error(WaitingVars,
            mode_error_higher_order_pred_var(PredOrFunc, PredVar, PredVarInst,
                Arity),
            !ModeInfo),
        Modes = [],
        Det = detism_erroneous,
        Args = Args0,
        ExtraGoals = no_extra_goals
    ).

%---------------------------------------------------------------------------%

modecheck_event_call(Modes, Args0, Args, !ModeInfo) :-
    ArgOffset = 0,
    modecheck_arg_list(ArgOffset, Modes, ExtraGoals, Args0, Args, !ModeInfo),
    expect(unify(ExtraGoals, no_extra_goals), $module, $pred,
        "ExtraGoals").

%---------------------------------------------------------------------------%

modecheck_builtin_cast(Modes, Args0, Args, Det, ExtraGoals, !ModeInfo) :-
    Det = detism_det,
    % These should always be mode correct.
    ArgOffset = 0,
    modecheck_arg_list(ArgOffset, Modes, ExtraGoals, Args0, Args, !ModeInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred modecheck_arg_list(int::in, list(mer_mode)::in, extra_goals::out,
    list(prog_var)::in, list(prog_var)::out, mode_info::in, mode_info::out)
    is det.

modecheck_arg_list(ArgOffset, Modes, ExtraGoals, Args0, Args, !ModeInfo) :-
    % Check that `Args0' have livenesses which match the expected livenesses.
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    get_arg_lives(ModuleInfo0, Modes, ExpectedArgLives),
    modecheck_var_list_is_live_no_exact_match(Args0, ExpectedArgLives,
        ArgOffset, !ModeInfo),

    % Check that `Args0' have insts which match the expected initial insts,
    % and set their new final insts (introducing extra unifications for
    % implied modes, if necessary).
    mode_list_get_initial_insts(ModuleInfo0, Modes, InitialInsts),
    modecheck_var_has_inst_list_no_exact_match(Args0, InitialInsts,
        ArgOffset, InstVarSub, !ModeInfo),
    mode_list_get_final_insts(ModuleInfo0, Modes, FinalInsts0),
    inst_list_apply_substitution(InstVarSub, FinalInsts0, FinalInsts),
    modecheck_set_var_inst_list(Args0, InitialInsts, FinalInsts,
        ArgOffset, Args, ExtraGoals, !ModeInfo).

%---------------------------------------------------------------------------%

:- pred get_var_insts_and_lives(mode_info::in, list(prog_var)::in,
    list(mer_inst)::out, list(is_live)::out) is det.

get_var_insts_and_lives(_, [], [], []).
get_var_insts_and_lives(ModeInfo, [Var | Vars],
        [Inst | Insts], [IsLive | IsLives]) :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_instmap(ModeInfo, InstMap),
    mode_info_get_var_types(ModeInfo, VarTypes),
    instmap_lookup_var(InstMap, Var, Inst0),
    lookup_var_type(VarTypes, Var, Type),
    normalise_inst(ModuleInfo, Type, Inst0, Inst),
    mode_info_var_is_live(ModeInfo, Var, IsLive0),
    (
        IsLive0 = is_live,
        IsLive = is_live
    ;
        IsLive0 = is_dead,
        % To reduce the potentially exponential explosion in the number of
        % modes, we only set IsLive to `dead' (meaning that the procedure
        % requires its argument to be dead, so that it can do destructive
        % update) if there really is a good chance of being able to do
        % destructive update.
        ( if
            inst_is_ground(ModuleInfo, Inst),
            inst_is_mostly_unique(ModuleInfo, Inst)
        then
            IsLive = is_dead
        else
            IsLive = is_live
        )
    ),
    get_var_insts_and_lives(ModeInfo, Vars, Insts, IsLives).

%---------------------------------------------------------------------------%

modes_are_indistinguishable(ProcId, OtherProcId, PredInfo, ModuleInfo) :-
    % The code of this predicate is similar to the code for
    % modes_are_identical/4 and compare_proc/5 below.
    %
    pred_info_get_proc_table(PredInfo, Procs),
    map.lookup(Procs, ProcId, ProcInfo),
    map.lookup(Procs, OtherProcId, OtherProcInfo),

    % Compare the initial insts of the arguments.
    proc_info_get_argmodes(ProcInfo, ProcArgModes),
    proc_info_get_argmodes(OtherProcInfo, OtherProcArgModes),
    mode_list_get_initial_insts(ModuleInfo, ProcArgModes, InitialInsts),
    mode_list_get_initial_insts(ModuleInfo, OtherProcArgModes,
        OtherInitialInsts),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    compare_inst_list(ModuleInfo, InitialInsts, OtherInitialInsts, no,
        ArgTypes, CompareInsts),
    CompareInsts = same,

    % Compare the expected livenesses of the arguments.
    get_arg_lives(ModuleInfo, ProcArgModes, ProcArgLives),
    get_arg_lives(ModuleInfo, OtherProcArgModes, OtherProcArgLives),
    compare_liveness_list(ProcArgLives, OtherProcArgLives, CompareLives),
    CompareLives = same,

    % Compare the determinisms; if both are cc_, or if both are not cc_,
    % then they are indistinguishable.
    proc_info_interface_determinism(ProcInfo, Detism),
    proc_info_interface_determinism(OtherProcInfo, OtherDetism),
    determinism_components(Detism, _CanFail, Solns),
    determinism_components(OtherDetism, _OtherCanFail, OtherSolns),
    ( Solns = at_most_many_cc, OtherSolns = at_most_many_cc
    ; Solns \= at_most_many_cc, OtherSolns \= at_most_many_cc
    ).

%---------------------------------------------------------------------------%

modes_are_identical_bar_cc(ProcId, OtherProcId, PredInfo, ModuleInfo) :-
    % The code of this predicate is similar to the code for
    % compare_proc/5 below and modes_are_indistinguishable/4 above.

    pred_info_get_proc_table(PredInfo, Procs),
    map.lookup(Procs, ProcId, ProcInfo),
    map.lookup(Procs, OtherProcId, OtherProcInfo),

    % Compare the initial insts of the arguments
    proc_info_get_argmodes(ProcInfo, ProcArgModes),
    proc_info_get_argmodes(OtherProcInfo, OtherProcArgModes),
    mode_list_get_initial_insts(ModuleInfo, ProcArgModes, InitialInsts),
    mode_list_get_initial_insts(ModuleInfo, OtherProcArgModes,
        OtherInitialInsts),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    compare_inst_list(ModuleInfo, InitialInsts, OtherInitialInsts, no,
        ArgTypes, CompareInitialInsts),
    CompareInitialInsts = same,

    % Compare the final insts of the arguments
    mode_list_get_final_insts(ModuleInfo, ProcArgModes, FinalInsts),
    mode_list_get_final_insts(ModuleInfo, OtherProcArgModes,
        OtherFinalInsts),
    compare_inst_list(ModuleInfo, FinalInsts, OtherFinalInsts, no,
        ArgTypes, CompareFinalInsts),
    CompareFinalInsts = same,

    % Compare the expected livenesses of the arguments
    get_arg_lives(ModuleInfo, ProcArgModes, ProcArgLives),
    get_arg_lives(ModuleInfo, OtherProcArgModes, OtherProcArgLives),
    compare_liveness_list(ProcArgLives, OtherProcArgLives, CompareLives),
    CompareLives = same,

    % Compare the determinisms, ignoring the cc part.
    proc_info_interface_determinism(ProcInfo, Detism),
    proc_info_interface_determinism(OtherProcInfo, OtherDetism),
    determinism_components(Detism, CanFail, Solns),
    determinism_components(OtherDetism, OtherCanFail, OtherSolns),
    CanFail = OtherCanFail,
    ( Solns = OtherSolns
    ; Solns = at_most_many_cc, OtherSolns = at_most_many
    ; Solns = at_most_many, OtherSolns = at_most_many_cc
    ).

%---------------------------------------------------------------------------%

% The algorithm for choose_best_match is supposed to be equivalent
% to the following specification:
%
%   1.  Remove any modes that are strictly less instantiated or
%       less informative on input than other valid modes; e.g we prefer
%       an (in, in, out) mode over an (out, in, out) mode, but not necessarily
%       over an (out, out, in) mode, and prefer a (ground -> ...) mode
%       over a (any -> ...) mode, and prefer a (bound(f) -> ...) mode
%       over a (ground -> ...) mode, and prefer a (... -> dead) mode
%       over a (... -> not dead) mode.
%
%       Also prefer a (any -> ...) mode over a (free -> ...) mode,
%       unless the actual argument is free, in which case we prefer
%       the (free -> ...) mode.
%
%   2.  If neither is prefered over the other by step 1, then prioritize them
%       by determinism, according to the standard partial order (best first):
%
%                           erroneous
%                          /       \
%                       det         failure
%                     /    \       /
%                 multi     semidet
%                    \      /
%                     nondet
%
%   3.  If there are still multiple possibilities, take them in
%       declaration order.

:- type match
    --->    better
    ;       worse
    ;       same
    ;       incomparable.

:- pred choose_best_match(mode_info::in, list(proc_mode)::in, pred_id::in,
    proc_table::in, list(prog_var)::in, proc_id::out, inst_var_sub::out,
    list(mer_mode)::out) is det.

choose_best_match(_, [], _, _, _, _, _, _) :-
    unexpected($module, $pred, "no best match").
choose_best_match(ModeInfo, [ProcMode | ProcModes], PredId,
        Procs, ArgVars, TheProcId, TheInstVarSub, TheArgModes) :-
    ProcMode = proc_mode(ProcId, InstVarSub, ArgModes),
    % ProcMode is best iff there is no other proc_mode which is better.
    ( if
        some [OtherProcId] (
            list.member(proc_mode(OtherProcId, _, _), ProcModes),
            compare_proc(ModeInfo, OtherProcId, ProcId, ArgVars, Procs, better)
        )
    then
        choose_best_match(ModeInfo, ProcModes, PredId, Procs, ArgVars,
            TheProcId, TheInstVarSub, TheArgModes)
    else
        TheProcId = ProcId,
        TheInstVarSub = InstVarSub,
        TheArgModes = ArgModes
    ).

    % Given two modes of a predicate, figure out whether one of them is a
    % better match than the other, for calls which could match either mode.
    %
:- pred compare_proc(mode_info::in, proc_id::in, proc_id::in,
    list(prog_var)::in, proc_table::in, match::out) is det.

compare_proc(ModeInfo, ProcId, OtherProcId, ArgVars, Procs, Compare) :-
    % The code of this predicate is similar to the code for
    % modes_are_indistinguishable/4 and modes_are_identical_bar_cc/4 above.

    map.lookup(Procs, ProcId, ProcInfo),
    map.lookup(Procs, OtherProcId, OtherProcInfo),

    % Compare the initial insts of the arguments.
    proc_info_get_argmodes(ProcInfo, ProcArgModes),
    proc_info_get_argmodes(OtherProcInfo, OtherProcArgModes),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_var_types(ModeInfo, VarTypes),
    lookup_var_types(VarTypes, ArgVars, ArgTypes),
    mode_list_get_initial_insts(ModuleInfo, ProcArgModes, InitialInsts),
    mode_list_get_initial_insts(ModuleInfo, OtherProcArgModes,
        OtherInitialInsts),
    get_var_insts_and_lives(ModeInfo, ArgVars, ArgInitialInsts, _ArgLives),
    compare_inst_list(ModuleInfo, InitialInsts, OtherInitialInsts,
        yes(ArgInitialInsts), ArgTypes, CompareInsts),

    % Compare the expected livenesses of the arguments.
    get_arg_lives(ModuleInfo, ProcArgModes, ProcArgLives),
    get_arg_lives(ModuleInfo, OtherProcArgModes, OtherProcArgLives),
    compare_liveness_list(ProcArgLives, OtherProcArgLives, CompareLives),

    % Compare the determinisms.
    proc_info_interface_determinism(ProcInfo, Detism),
    proc_info_interface_determinism(OtherProcInfo, OtherDetism),
    determinism_components(Detism, CanFail, SolnCount),
    determinism_components(OtherDetism, OtherCanFail, OtherSolnCount),
    compare_solncounts(SolnCount, OtherSolnCount, CompareSolnCounts),
    (
        CompareSolnCounts = first_tighter_than,
        CompareDet = better
    ;
        CompareSolnCounts = first_same_as,
        compare_canfails(CanFail, OtherCanFail, CompareCanFails),
        (
            CompareCanFails = first_tighter_than,
            CompareDet = better
        ;
            CompareCanFails = first_same_as,
            CompareDet = same
        ;
            CompareCanFails = first_looser_than,
            CompareDet = worse
        )
    ;
        CompareSolnCounts = first_looser_than,
        CompareDet = worse
    ),

    % Combine the results, with the insts & lives comparisons
    % taking priority over the determinism comparison.
    combine_results(CompareInsts, CompareLives, Compare0),
    prioritized_combine_results(Compare0, CompareDet, Compare).

:- pred compare_inst_list(module_info::in,
    list(mer_inst)::in, list(mer_inst)::in,
    maybe(list(mer_inst))::in, list(mer_type)::in, match::out) is det.

compare_inst_list(ModuleInfo, InstsA, InstsB, ArgInsts, Types, Result) :-
    ( if
        compare_inst_list_2(ModuleInfo, InstsA, InstsB, ArgInsts,
            Types, Result0)
    then
        Result = Result0
    else
        unexpected($module, $pred, "length mismatch")
    ).

:- pred compare_inst_list_2(module_info::in,
    list(mer_inst)::in, list(mer_inst)::in,
    maybe(list(mer_inst))::in, list(mer_type)::in, match::out) is semidet.

compare_inst_list_2(_, [], [], _, [], same).
compare_inst_list_2(ModuleInfo, [InstA | InstsA], [InstB | InstsB],
        no, [Type | Types], Result) :-
    compare_inst(ModuleInfo, InstA, InstB, no, Type, Result0),
    compare_inst_list_2(ModuleInfo, InstsA, InstsB, no, Types, Result1),
    combine_results(Result0, Result1, Result).
compare_inst_list_2(ModuleInfo, [InstA | InstsA], [InstB | InstsB],
        yes([ArgInst | ArgInsts]), [Type | Types], Result) :-
    compare_inst(ModuleInfo, InstA, InstB, yes(ArgInst), Type, Result0),
    compare_inst_list_2(ModuleInfo, InstsA, InstsB, yes(ArgInsts), Types,
        Result1),
    combine_results(Result0, Result1, Result).

:- pred compare_liveness_list(list(is_live)::in, list(is_live)::in, match::out)
    is det.

compare_liveness_list([], [], same).
compare_liveness_list([_ | _], [], _) :-
    unexpected($module, $pred, "length mismatch").
compare_liveness_list([], [_ | _], _) :-
    unexpected($module, $pred, "length mismatch").
compare_liveness_list([LiveA | LiveAs], [LiveB | LiveBs], Result) :-
    compare_liveness(LiveA, LiveB, Result0),
    compare_liveness_list(LiveAs, LiveBs, Result1),
    combine_results(Result0, Result1, Result).

    % Compare_liveness -- prefer dead to live. If either is a valid match,
    % then the actual argument must be dead, so prefer the mode which can take
    % advantage of that).
    %
:- pred compare_liveness(is_live::in, is_live::in, match::out) is det.

compare_liveness(is_dead, is_dead, same).
compare_liveness(is_dead, is_live, better).
compare_liveness(is_live, is_dead, worse).
compare_liveness(is_live, is_live, same).

    % Combine two results, giving priority to the first one.
    %
:- pred prioritized_combine_results(match::in, match::in, match::out) is det.

prioritized_combine_results(better, _, better).
prioritized_combine_results(worse, _, worse).
prioritized_combine_results(same, Result, Result).
prioritized_combine_results(incomparable, _, incomparable).

    % Combine two results, giving them equal priority.
    %
:- pred combine_results(match::in, match::in, match::out) is det.

combine_results(better, better, better).
combine_results(better, same, better).
combine_results(better, worse, incomparable).
combine_results(better, incomparable, incomparable).
combine_results(worse, worse, worse).
combine_results(worse, same, worse).
combine_results(worse, better, incomparable).
combine_results(worse, incomparable, incomparable).
combine_results(same, Result, Result).
combine_results(incomparable, _, incomparable).

    % Compare two initial insts, to figure out which would be a better match.
    %
    % More information is better:
    %   prefer bound(f) to ground
    %   prefer unique to mostly_unique or ground, and
    %   prefer mostly_unique to ground
    %       (unique > mostly_unique > shared > mostly_dead > dead)
    % More bound is better:
    %       (if both can match, the one which is more bound
    %       is better, because it may be an exact match, whereas
    %       the other one would be an implied mode)
    %   prefer ground to free   (i.e. prefer in to out)
    %   prefer ground to any    (e.g. prefer in to in(any))
    %   prefer any to free  (e.g. prefer any->ground to out)
    %
:- pred compare_inst(module_info::in, mer_inst::in, mer_inst::in,
    maybe(mer_inst)::in, mer_type::in, match::out) is det.

compare_inst(ModuleInfo, InstA, InstB, MaybeArgInst, Type, Result) :-
    % inst_matches_initial(A,B) succeeds iff
    %   A specifies at least as much information
    %   and at least as much binding as B --
    %   with the exception that `any' matches_initial `free'
    %   and perhaps vice versa.
    ( if inst_matches_initial(InstA, InstB, Type, ModuleInfo) then
        A_mi_B = yes
    else
        A_mi_B = no
    ),
    ( if inst_matches_initial(InstB, InstA, Type, ModuleInfo) then
        B_mi_A = yes
    else
        B_mi_A = no
    ),
    ( A_mi_B = yes, B_mi_A = no,  Result = better
    ; A_mi_B = no,  B_mi_A = yes, Result = worse
    ; A_mi_B = no,  B_mi_A = no,  Result = incomparable
    ; A_mi_B = yes, B_mi_A = yes,
        % We need to further disambiguate the cases involving `any' and `free',
        % since `any' matches_initial `free' and vice versa.
        % For these cases, we want to take the actual inst of the argument
        % into account: if the argument is `free', we should prefer `free',
        % but otherwise, we should prefer `any'.
        %
        (
            MaybeArgInst = no,
            Result0 = same
        ;
            MaybeArgInst = yes(ArgInst),
            ( if
                inst_matches_initial_no_implied_modes(ArgInst,
                    InstA, Type, ModuleInfo)
            then
                Arg_mf_A = yes
            else
                Arg_mf_A = no
            ),
            ( if
                inst_matches_initial_no_implied_modes(ArgInst,
                    InstB, Type, ModuleInfo)
            then
                Arg_mf_B = yes
            else
                Arg_mf_B = no
            ),
            ( Arg_mf_A = yes, Arg_mf_B = no,  Result0 = better
            ; Arg_mf_A = no,  Arg_mf_B = yes, Result0 = worse
            ; Arg_mf_A = yes, Arg_mf_B = yes, Result0 = same
            ; Arg_mf_A = no,  Arg_mf_B = no,  Result0 = same
            )
        ),
        (
            Result0 = same,
            % If the actual arg inst is not available, or comparing with
            % the arg inst doesn't help, then compare the two proc insts.
            ( if
                inst_matches_initial_no_implied_modes(InstA,
                    InstB, Type, ModuleInfo)
            then
                A_mf_B = yes
            else
                A_mf_B = no
            ),
            ( if
                inst_matches_initial_no_implied_modes(InstB,
                    InstA, Type, ModuleInfo)
            then
                B_mf_A = yes
            else
                B_mf_A = no
            ),
            ( A_mf_B = yes, B_mf_A = no,  Result = better
            ; A_mf_B = no,  B_mf_A = yes, Result = worse
            ; A_mf_B = no,  B_mf_A = no,  Result = incomparable
            ; A_mf_B = yes, B_mf_A = yes, Result = same
            )
        ;
            ( Result0 = better
            ; Result0 = worse
            ),
            Result = Result0
        )
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.modecheck_call.
%---------------------------------------------------------------------------%
