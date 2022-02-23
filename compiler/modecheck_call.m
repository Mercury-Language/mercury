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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_comparison.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.proc_requests.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module hlds.vartypes.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module map.
:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

modecheck_call_pred(PredId, MaybeDetism, ProcId0, TheProcId,
        ArgVars0, ArgVars, _GoalInfo, ExtraGoals, !ModeInfo) :-
    mode_info_get_may_change_called_proc(!.ModeInfo, MayChangeCalledProc),
    mode_info_get_pred_id_table(!.ModeInfo, PredIdTable),
    map.lookup(PredIdTable, PredId, PredInfo),
    pred_info_get_proc_table(PredInfo, Procs),
    (
        MayChangeCalledProc = may_not_change_called_proc,
        ( if ProcId0 = invalid_proc_id then
            unexpected($pred, "invalid proc_id")
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
        ProcIds = [],
        not check_marker(Markers, marker_infer_modes)
    then
        set_of_var.init(WaitingVars),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        expect(unify(PredOrFunc, pf_predicate), $pred,
            "function with no mode, not even the default"),
        mode_info_error(WaitingVars, mode_error_callee_pred_has_no_mode_decl,
            !ModeInfo),
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
        mode_info_get_module_info(!.ModeInfo, ModuleInfo),
        proc_info_arglives(ModuleInfo, ProcInfo, ProcArgLives0),
        modecheck_var_list_is_live_no_exact_match(ArgVars0, ProcArgLives0,
            ArgOffset, !ModeInfo),

        % Check that `ArgsVars0' have insts which match the expected
        % initial insts, and set their new final insts (introducing
        % extra unifications for implied modes, if necessary).
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
            [], RevMatchingProcIds, [], RevProcInitialInsts,
            WaitingVars0, WaitingVars1, !ModeInfo),

        (
            RevMatchingProcIds = [],
            list.reverse(RevProcInitialInsts, ProcInitialInsts),
            no_matching_modes(PredId, ArgVars0, ProcInitialInsts,
                MaybeDetism, WaitingVars1, TheProcId, !ModeInfo),
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
                CalleeModeErrors = [],
                modecheck_end_of_call(ProcInfo, ProcArgModes, ArgVars0,
                    ArgOffset, InstVarSub, ArgVars, ExtraGoals, !ModeInfo)
            ;
                CalleeModeErrors = [_ | _],
                % Mode error in callee for this mode.
                ArgVars = ArgVars0,
                WaitingVars = set_of_var.list_to_set(ArgVars),
                ExtraGoals = no_extra_goals,
                instmap_lookup_vars(InstMap, ArgVars, ArgInsts),
                mode_info_set_call_arg_context(0, !ModeInfo),
                ModeError = mode_error_in_callee(ArgVars, ArgInsts,
                    PredId, TheProcId, CalleeModeErrors),
                mode_info_error(WaitingVars, ModeError, !ModeInfo)
            )
        ),

        % Restore the error list, appending any new error(s).
        mode_info_get_errors(!.ModeInfo, NewErrors),
        list.append(OldErrors, NewErrors, Errors),
        mode_info_set_errors(Errors, !ModeInfo)
    ).

%---------------------%

:- pred modecheck_find_matching_modes(list(proc_id)::in, pred_id::in,
    proc_table::in, list(prog_var)::in,
    list(proc_mode)::in, list(proc_mode)::out,
    list(list(mer_inst))::in, list(list(mer_inst))::out,
    set_of_progvar::in, set_of_progvar::out,
    mode_info::in, mode_info::out) is det.

modecheck_find_matching_modes([], _PredId, _Procs, _ArgVars,
        !MatchingProcIds, !RevProcInitialInsts, !WaitingVars, !ModeInfo).
modecheck_find_matching_modes([ProcId | ProcIds], PredId, Procs, ArgVars0,
        !MatchingProcIds, !RevProcInitialInsts, !WaitingVars, !ModeInfo) :-
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
    proc_info_arglives(ModuleInfo, ProcInfo, ProcArgLives0),

    % Check whether the livenesses of the args matches their expected liveness.
    modecheck_var_list_is_live_no_exact_match(ArgVars0, ProcArgLives0, 0,
        !ModeInfo),

    % Check whether the insts of the args matches their expected initial insts.
    %
    % If we are doing mode inference for the called procedure, and the
    % called procedure has been inferred as an invalid mode, then don't use
    % it unless it is an exact match.
    %
    % XXX Do we really want mode inference to use implied modes?
    % Would it be better to always require an exact match when doing mode
    % inference, to ensure that we add new inferred modes rather than using
    % implied modes?

    mode_list_get_initial_insts(ModuleInfo, ProcArgModes, InitialInsts),
    !:RevProcInitialInsts = [InitialInsts | !.RevProcInitialInsts],
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
        !MatchingProcIds, !RevProcInitialInsts, !WaitingVars, !ModeInfo).

%---------------------%

:- pred no_matching_modes(pred_id::in, list(prog_var)::in,
    list(list(mer_inst))::in, maybe(determinism)::in, set_of_progvar::in,
    proc_id::out, mode_info::in, mode_info::out) is det.

no_matching_modes(PredId, ArgVars, ProcInitialInsts, MaybeDetism, WaitingVars,
        NewProcId, !ModeInfo) :-
    % There were no matching modes.
    % If we are inferring modes for this called predicate, then
    % just insert a new mode declaration which will match.
    % Otherwise, report an error.

    mode_info_get_pred_id_table(!.ModeInfo, PredIdTable),
    map.lookup(PredIdTable, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    ( if check_marker(Markers, marker_infer_modes) then
        insert_new_mode(PredId, ArgVars, MaybeDetism, NewProcId, !ModeInfo),
        % We don't yet know the final insts for the newly created mode
        % of the called predicate, so we set the instmap to unreachable,
        % indicating that we have no information about the modes at this
        % point in the computation.
        instmap.init_unreachable(Instmap),
        mode_info_set_instmap(Instmap, !ModeInfo)
    else
        NewProcId = invalid_proc_id,    % dummy value
        mode_info_get_instmap(!.ModeInfo, InstMap),
        mode_info_set_call_arg_context(0, !ModeInfo),
        ModeError = mode_error_no_matching_mode(InstMap, ArgVars,
            ProcInitialInsts),
        mode_info_error(WaitingVars, ModeError, !ModeInfo)
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
    module_info_pred_info(ModuleInfo0, PredId, PredInfo0),
    pred_info_get_context(PredInfo0, Context),
    list.length(ArgVars, Arity),
    list.duplicate(Arity, not_reached, FinalInsts),
    inst_lists_to_mode_list(InitialInsts, FinalInsts, Modes),
    mode_info_get_instvarset(!.ModeInfo, InstVarSet),

    % Call request_proc, which will create the new procedure,
    % set its "can-process" flag to `no', and insert it into the queue
    % of requested procedures.
    request_proc(PredId, Modes, InstVarSet, yes(ArgLives), MaybeDet, Context,
        ProcId, ModuleInfo0, ModuleInfo),
    mode_info_set_module_info(ModuleInfo, !ModeInfo),

    % Since we have created a new inferred mode for this predicate,
    % things have changed, so we will need to do at least one more pass
    % of the fixpoint analysis.
    mode_info_set_changed_flag(yes, !ModeInfo).

:- pred get_var_insts_and_lives(mode_info::in, list(prog_var)::in,
    list(mer_inst)::out, list(is_live)::out) is det.

get_var_insts_and_lives(_, [], [], []).
get_var_insts_and_lives(ModeInfo, [Var | Vars],
        [Inst | Insts], [IsLive | IsLives]) :-
    get_var_inst(ModeInfo, Var, Inst),
    mode_info_var_is_live(ModeInfo, Var, IsLive0),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
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

%---------------------%

:- pred modecheck_end_of_call(proc_info::in, list(mer_mode)::in,
    list(prog_var)::in, int::in, inst_var_sub::in, list(prog_var)::out,
    extra_goals::out, mode_info::in, mode_info::out) is det.

modecheck_end_of_call(ProcInfo, ProcArgModes, ArgVars0, ArgOffset,
        InstVarSub, ArgVars, ExtraGoals, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_list_get_initial_insts(ModuleInfo, ProcArgModes, InitialInsts0),
    mode_list_get_final_insts(ModuleInfo, ProcArgModes, FinalInsts0),
    inst_list_apply_substitution(InstVarSub, InitialInsts0, InitialInsts),
    inst_list_apply_substitution(InstVarSub, FinalInsts0, FinalInsts),
    modecheck_set_var_inst_list(ArgVars0, InitialInsts, FinalInsts,
        ArgOffset, ArgVars, ExtraGoals, !ModeInfo),
    proc_info_never_succeeds(ProcInfo, CanSucceed),
    (
        CanSucceed = proc_cannot_succeed,
        instmap.init_unreachable(Instmap),
        mode_info_set_instmap(Instmap, !ModeInfo)
    ;
        CanSucceed = proc_can_maybe_succeed
    ).

%---------------------------------------------------------------------------%

modecheck_higher_order_call(PredOrFunc, PredVar, ArgVars0, ArgVars,
        Modes, Detism, ExtraGoals, !ModeInfo) :-
    % First, check that `PredVar' has a higher-order inst,
    % with the right pred_or_func and the appropriate arity.
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap_lookup_var(InstMap0, PredVar, PredVarInst0),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    inst_expand(ModuleInfo0, PredVarInst0, PredVarInst),
    ActualPredFormArity = arg_list_arity(ArgVars0),
    user_arity_pred_form_arity(PredOrFunc,
        ActualUserArity, ActualPredFormArity),
    get_higher_order_inst_match(!.ModeInfo, PredOrFunc, PredVar, PredVarInst,
        ActualUserArity, Match),
    (
        Match = higher_order_match(PredInstInfo),
        PredInstInfo = pred_inst_info(_, Modes0, _, Detism0),
        ( if
            % If PredVar is inst `any' then it gets bound. If it is locked,
            % this is a mode error.
            PredVarInst = any(A, B),
            mode_info_var_is_locked(!.ModeInfo, PredVar, Reason)
        then
            BetterPredVarInst = ground(A, B),
            WaitingVars = set_of_var.make_singleton(PredVar),
            ModeError = mode_error_bind_locked_var(Reason, PredVar,
                PredVarInst, BetterPredVarInst),
            mode_info_error(WaitingVars, ModeError, !ModeInfo),
            Modes = [],
            Detism = detism_erroneous,
            ArgVars = ArgVars0,
            ExtraGoals = no_extra_goals
        else
            Modes = Modes0,
            Detism = Detism0,
            ArgOffset = 1,
            modecheck_arg_list(ArgOffset, Modes, ExtraGoals, ArgVars0, ArgVars,
                !ModeInfo),
            ( if determinism_components(Detism, _, at_most_zero) then
                instmap.init_unreachable(Instmap),
                mode_info_set_instmap(Instmap, !ModeInfo)
            else
                true
            )
        )
    ;
        Match = higher_order_mismatch(Mismatch),
        % The error occurred in argument 1, i.e. the pred term.
        mode_info_set_call_arg_context(1, !ModeInfo),
        WaitingVars = set_of_var.make_singleton(PredVar),
        ModeError = mode_error_bad_higher_order_inst(PredVar, PredVarInst,
            PredOrFunc, ActualUserArity, Mismatch),
        mode_info_error(WaitingVars, ModeError, !ModeInfo),
        Modes = [],
        Detism = detism_erroneous,
        ArgVars = ArgVars0,
        ExtraGoals = no_extra_goals
    ).

:- type higher_order_match
    --->    higher_order_match(pred_inst_info)
    ;       higher_order_mismatch(higher_order_mismatch_info).

:- pred get_higher_order_inst_match(mode_info::in, pred_or_func::in,
    prog_var::in, mer_inst::in, user_arity::in,
    higher_order_match::out) is det.

get_higher_order_inst_match(ModeInfo, ExpectedPredOrFunc, PredVar, PredVarInst,
        ExpectedUserArity, Match) :-
    ( if
        ( PredVarInst = ground(_Uniq, HOInstInfo)
        ; PredVarInst = any(_Uniq, HOInstInfo)
        )
    then
        (
            HOInstInfo = higher_order(PredInstInfo0),
            Match0 = higher_order_match(PredInstInfo0)
        ;
            % If PredVarInst has no higher-order inst information,
            % then look for higher-order inst information in the type.
            % Otherwise, if PredVar has a function type, assume the default
            % function mode.
            HOInstInfo = none_or_default_func,
            mode_info_get_var_types(ModeInfo, VarTypes),
            lookup_var_type(VarTypes, PredVar, Type0),
            Type = strip_kind_annotation(Type0),
            ( if
                Type = higher_order_type(TypePredOrFunc,
                    ArgTypes, TypeHOInstInfo, _Purity, _EvalMethod)
            then
                (
                    TypeHOInstInfo = higher_order(PredInstInfo0),
                    PredInstInfo0 = pred_inst_info(TypeHOPredOrFunc, _, _, _),
                    expect(unify(TypePredOrFunc, TypeHOPredOrFunc), $pred,
                        "TypePredOrFunc != TypeHOPredOrFunc"),
                    Match0 = higher_order_match(PredInstInfo0)
                ;
                    TypeHOInstInfo = none_or_default_func,
                    (
                        TypePredOrFunc = pf_function,
                        list.length(ArgTypes, NumArgs),
                        PredInstInfo0 =
                            pred_inst_info_default_func_mode(NumArgs),
                        Match0 = higher_order_match(PredInstInfo0)
                    ;
                        TypePredOrFunc = pf_predicate,
                        Mismatch0 = mismatch_no_higher_order_inst_info,
                        Match0 = higher_order_mismatch(Mismatch0)
                    )
                )
            else
                Match0 = higher_order_mismatch(mismatch_not_higher_order_type)
            )
        ),
        (
            Match0 = higher_order_mismatch(Mismatch),
            Match = higher_order_mismatch(Mismatch)
        ;
            Match0 = higher_order_match(PredInstInfo),
            PredInstInfo = pred_inst_info(ActualPredOrFunc, Modes, _, _),
            ( if ExpectedPredOrFunc = ActualPredOrFunc then
                ActualPredFormArity = arg_list_arity(Modes),
                user_arity_pred_form_arity(ActualPredOrFunc,
                    ActualUserArity, ActualPredFormArity),
                ( if ExpectedUserArity = ActualUserArity then
                    Match = higher_order_match(PredInstInfo)
                else
                    Mismatch = mismatch_on_arity(ActualUserArity),
                    Match = higher_order_mismatch(Mismatch)
                )
            else
                Mismatch = mismatch_pred_vs_func(ActualPredOrFunc),
                Match = higher_order_mismatch(Mismatch)
            )
        )
    else
        Match = higher_order_mismatch(mismatch_no_higher_order_inst_info)
    ).

%---------------------------------------------------------------------------%

modecheck_event_call(Modes, Args0, Args, !ModeInfo) :-
    ArgOffset = 0,
    modecheck_arg_list(ArgOffset, Modes, ExtraGoals, Args0, Args, !ModeInfo),
    expect(unify(ExtraGoals, no_extra_goals), $pred, "ExtraGoals").

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
:- end_module check_hlds.modecheck_call.
%---------------------------------------------------------------------------%
