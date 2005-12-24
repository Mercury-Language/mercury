%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mode_info.m.
% Main author: fjh.

% This file defines the mode_info data structure, which is used to hold
% the information we need during mode analysis.

%-----------------------------------------------------------------------------%

:- module check_hlds__mode_info.
:- interface.

:- import_module check_hlds.delay_info.
:- import_module check_hlds.mode_errors.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bag.
:- import_module bool.
:- import_module list.
:- import_module set.
:- import_module std_util.

:- interface.

    % The mode_info data structure and access predicates.

    % XXX `side' is not used
:- type mode_context
    --->    call(
                call_id,
                int             % Argument number (offset so that the real
                                % arguments start at number 1 whereas the
                                % type_info arguments have numbers <= 0).
            )
    ;       unify(
                unify_context,  % original source of the unification
                side            % LHS or RHS
            )
    ;       uninitialized.

:- type side
    --->    left
    ;       right.

:- type call_context
    --->    unify(unify_context)
    ;       call(call_id).

:- type var_lock_reason
    --->    negation
    ;       if_then_else
    ;       lambda(pred_or_func)
    ;       par_conj.

    % Specify how to process goals - using either modes.m or unique_modes.m.
:- type how_to_check_goal
    --->    check_modes
    ;       check_unique_modes.

    % Is mode analysis allowed to change which procedure of a predicate
    % is called. It may not change the called procedure after deforestation
    % has performed a generalisation step, since that could result
    % in selecting a less efficient mode, or one which doesn't even
    % have the same termination behaviour.
    % Also, when rechecking a goal after adding extra goals, it is
    % not necessary to choose again which procedure is to be called.
:- type may_change_called_proc
    --->    may_change_called_proc
    ;       may_not_change_called_proc.

:- type locked_vars == assoc_list(var_lock_reason, set(prog_var)).

:- type mode_info.

:- type debug_flags
    --->    debug_flags(
                verbose     :: bool,
                            % The value --debug-modes-verbose
                minimal     :: bool,
                            % The value --debug-modes-minimal
                statistics  :: bool
                            % The value --debug-modes-statistics
            ).

    % Initialize the mode_info.
    %
:- pred mode_info_init(module_info::in, pred_id::in,
    proc_id::in, prog_context::in, set(prog_var)::in, instmap::in,
    how_to_check_goal::in, may_change_called_proc::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%

:- pred mode_info_get_module_info(mode_info::in, module_info::out) is det.
:- pred mode_info_get_preds(mode_info::in, pred_table::out) is det.
:- pred mode_info_get_modes(mode_info::in, mode_table::out) is det.
:- pred mode_info_get_insts(mode_info::in, inst_table::out) is det.
:- pred mode_info_get_predid(mode_info::in, pred_id::out) is det.
:- pred mode_info_get_procid(mode_info::in, proc_id::out) is det.
:- pred mode_info_get_debug_modes(mode_info::in, maybe(debug_flags)::out)
    is det.
:- pred mode_info_get_context(mode_info::in, prog_context::out) is det.
:- pred mode_info_get_mode_context(mode_info::in, mode_context::out) is det.
:- pred mode_info_get_instmap(mode_info::in, instmap::out) is det.
:- pred mode_info_get_locked_vars(mode_info::in, locked_vars::out) is det.
:- pred mode_info_get_errors(mode_info::in, list(mode_error_info)::out) is det.
:- pred mode_info_get_warnings(mode_info::in, list(mode_warning_info)::out)
    is det.
:- pred mode_info_get_need_to_requantify(mode_info::in, bool::out) is det.
:- pred mode_info_get_in_promise_purity_scope(mode_info::in, bool::out) is det.
:- pred mode_info_get_num_errors(mode_info::in, int::out) is det.
:- pred mode_info_get_liveness(mode_info::in, set(prog_var)::out) is det.
:- pred mode_info_get_varset(mode_info::in, prog_varset::out) is det.
:- pred mode_info_get_instvarset(mode_info::in, inst_varset::out) is det.
:- pred mode_info_get_var_types(mode_info::in, vartypes::out) is det.
:- pred mode_info_get_delay_info(mode_info::in, delay_info::out) is det.
:- pred mode_info_get_live_vars(mode_info::in, bag(prog_var)::out) is det.
:- pred mode_info_get_nondet_live_vars(mode_info::in, bag(prog_var)::out)
    is det.
:- pred mode_info_get_last_checkpoint_insts(mode_info::in, instmap::out)
    is det.
:- pred mode_info_get_parallel_vars(mode_info::in,
    list(pair(set(prog_var)))::out) is det.
:- pred mode_info_get_changed_flag(mode_info::in, bool::out) is det.
:- pred mode_info_get_how_to_check(mode_info::in,
    how_to_check_goal::out) is det.
:- pred mode_info_get_may_change_called_proc(mode_info::in,
    may_change_called_proc::out) is det.
:- pred mode_info_get_initial_instmap(mode_info::in, instmap::out) is det.

%-----------------------------------------------------------------------------%

:- pred mode_info_set_module_info(module_info::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_predid(pred_id::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_procid(proc_id::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_context(prog_context::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_mode_context(mode_context::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_call_context(call_context::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_call_arg_context(int::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_unset_call_context(
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_instmap(instmap::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_locked_vars(locked_vars::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_errors(list(mode_error_info)::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_warnings(list(mode_warning_info)::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_need_to_requantify(bool::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_in_promise_purity_scope(bool::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_add_live_vars(set(prog_var)::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_remove_live_vars(set(prog_var)::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_varset(prog_varset::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_var_types(vartypes::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_delay_info(delay_info::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_live_vars(bag(prog_var)::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_nondet_live_vars(bag(prog_var)::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_last_checkpoint_insts(instmap::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_parallel_vars(list(pair(set(prog_var)))::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_changed_flag(bool::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_how_to_check(how_to_check_goal::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_may_change_called_proc(may_change_called_proc::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_checking_extra_goals(bool::in,
    mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%

:- pred mode_info_get_types_of_vars(mode_info::in,
    list(prog_var)::in, list(mer_type)::out) is det.

:- pred mode_info_lock_vars(var_lock_reason::in, set(prog_var)::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_unlock_vars(var_lock_reason::in, set(prog_var)::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_var_is_locked(mode_info::in, prog_var::in,
    var_lock_reason::out) is semidet.

    % Find the simple_call_id to use in error messages
    % for the given pred_id.
    %
:- pred mode_info_get_call_id(mode_info::in, pred_id::in,
    simple_call_id::out) is det.

:- pred mode_info_var_list_is_live(mode_info::in, list(prog_var)::in,
    list(is_live)::out) is det.

:- pred mode_info_var_is_live(mode_info::in, prog_var::in,
    is_live::out) is det.

:- pred mode_info_var_is_nondet_live(mode_info::in, prog_var::in,
    is_live::out) is det.

%-----------------------------------------------------------------------------%

    % Record a mode error (and associated context info) in the mode_info.
    %
:- pred mode_info_error(set(prog_var)::in, mode_error::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_add_error(mode_error_info::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_warning(mode_warning::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_add_warning(mode_warning_info::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_need_to_requantify(mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%

    % The mode_info contains a flag indicating whether initialisation calls,
    % converting a solver variable from `free' to `any', may be inserted
    % during mode analysis.
    %
:- pred mode_info_may_initialise_solver_vars(mode_info::in)
    is semidet.

:- pred mode_info_get_may_initialise_solver_vars(bool::out, mode_info::in)
    is det.

:- pred mode_info_set_may_initialise_solver_vars(bool::in,
    mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.delay_info.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.mode_util.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.

:- import_module int.
:- import_module map.
:- import_module queue.
:- import_module std_util.
:- import_module string.
:- import_module svbag.
:- import_module term.
:- import_module varset.

:- type mode_sub_info
    --->    mode_sub_info(
                % The mode which we are checking.
                procid                  :: proc_id,

                % The variables in the current proc.
                varset                  :: prog_varset,

                % This field is used by the checkpoint code when debug_modes
                % is on. It has the instmap that was current at the last mode
                % checkpoint, so that checkpoints do not print out the insts
                % of variables whose insts have not changed since the last
                % checkpoint. This field will always contain an unreachable
                % instmap if debug_modes is off, since its information is not
                % needed then.
                last_checkpoint_insts   :: instmap,

                % Changed flag: if `yes', then we may need to repeat
                % mode inference.
                changed_flag            :: bool,

                % Are we rechecking a goal after introducing unifications
                % for complicated sub-unifications or an implied mode? If so,
                % redoing the mode check should not introduce more extra
                % unifications.
                checking_extra_goals    :: bool,

                % The initial instmap of the procedure body. Used to decide
                % whether a unification that cannot fail could be influenced
                % by an argument mode that enforces a subtype.
                initial_instmap         :: instmap,

                % The mode warnings found.
                warnings                :: list(mode_warning_info),

                % Set to `yes' if we need to requantify the procedure body
                % after mode analysis finishes.
                need_to_requantify      :: bool,

                % Set to `yes' if we are in a promise_<purity> scope.  This
                % information is needed to check that potentially impure
                % uses of inst any non-locals in negated contexts are
                % properly acknowledged by the programmer.
                in_promise_purity_scope :: bool
            ).

:- type mode_info
    --->    mode_info(
                module_info             :: module_info,

                % The pred we are checking.
                predid                  :: pred_id,

                % The types of the variables.
                var_types               :: vartypes,

                % Is mode debugging of this procedure enabled? If yes,
                % is verbose mode debugging enabled, is minimal mode debugging
                % enabled, and is statistics printing enabled?
                debug                   :: maybe(debug_flags),

                % The line number of the subgoal we are currently checking.
                context                 :: prog_context,

                % A description of where in the goal the error occurred.
                mode_context            :: mode_context,

                % The current instantiatedness of the variables.
                instmap                 :: instmap,

                % The "locked" variables, i.e. variables which cannot be
                % further instantiated inside a negated context.
                locked_vars             :: locked_vars,

                % Info about delayed goals.
                delay_info              :: delay_info,

                % The mode errors found.
                errors                  :: list(mode_error_info),

                % The live variables, i.e. those variables which may be
                % referenced again on forward execution or after shallow
                % backtracking. (By shallow backtracking, I mean semidet
                % backtracking in a negation, if-then-else, or semidet
                % disjunction within the current predicate.)
                live_vars               :: bag(prog_var),

                % The nondet-live variables, i.e. those variables which may be
                % referenced again after deep backtracking TO THE CURRENT
                % EXECUTION POINT. These are the variables which need to be
                % made mostly_unique rather than unique when we get to a
                % nondet disjunction or a nondet call.  We do not include
                % variables which may be referenced again after backtracking
                % to a point EARLIER THAN the current execution point, since
                % those variables will *already* have been marked as
                % mostly_unique rather than unique.)
                nondet_live_vars        :: bag(prog_var),

                instvarset  :: inst_varset,

                % A stack of pairs of sets of variables used to mode-check
                % parallel conjunctions. The first set is the nonlocals of the
                % parallel conjunction. The second set is a subset of the
                % first, and is the set of variables that have been [further]
                % bound inside the current parallel conjunct - the stack
                % is for the correct handling of nested parallel conjunctions.
                parallel_vars   :: list(pair(set(prog_var), set(prog_var))),

                how_to_check    :: how_to_check_goal,

                % Is mode analysis allowed to change which procedure is called?
                may_change_called_proc :: may_change_called_proc,

                % `yes' if calls to the initialisation predicates for solver
                % vars can be inserted during mode analysis in order to make
                % goals schedulable.
                may_initialise_solver_vars :: bool,

                mode_sub_info   :: mode_sub_info
            ).

%-----------------------------------------------------------------------------%

mode_info_init(ModuleInfo, PredId, ProcId, Context, LiveVars, InstMapping0,
        HowToCheck, MayChangeProc, ModeInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals__lookup_bool_option(Globals, debug_modes, DebugModes),
    globals__lookup_int_option(Globals, debug_modes_pred_id,
        DebugModesPredId),
    pred_id_to_int(PredId, PredIdInt),
    (
        DebugModes = yes,
        ( DebugModesPredId >= 0 => DebugModesPredId = PredIdInt )
    ->
        globals__lookup_bool_option(Globals, debug_modes_verbose,
            DebugVerbose),
        globals__lookup_bool_option(Globals, debug_modes_minimal,
            DebugMinimal),
        globals__lookup_bool_option(Globals, debug_modes_statistics,
            Statistics),
        Flags = debug_flags(DebugVerbose, DebugMinimal, Statistics),
        Debug = yes(Flags)
    ;
        Debug = no
    ),

    instmap__init_unreachable(Unreachable),
    mode_context_init(ModeContext),
    LockedVars = [],
    delay_info__init(DelayInfo),
    ErrorList = [],
    WarningList = [],
        % look up the varset and var types
    module_info_preds(ModuleInfo, Preds),
    map__lookup(Preds, PredId, PredInfo),
    pred_info_procedures(PredInfo, Procs),
    map__lookup(Procs, ProcId, ProcInfo),
    proc_info_varset(ProcInfo, VarSet),
    proc_info_vartypes(ProcInfo, VarTypes),
    proc_info_inst_varset(ProcInfo, InstVarSet),

    bag__from_set(LiveVars, LiveVarsBag),
    bag__from_set(LiveVars, NondetLiveVarsBag),

    Changed = no,
    CheckingExtraGoals = no,
    MayInitSolverVars = yes,
    NeedToRequantify = no,
    InNegatedContext = no,

    ModeSubInfo = mode_sub_info(ProcId, VarSet, Unreachable, Changed,
        CheckingExtraGoals, InstMapping0, WarningList, NeedToRequantify,
        InNegatedContext),

    ModeInfo = mode_info(ModuleInfo, PredId, VarTypes, Debug,
        Context, ModeContext, InstMapping0, LockedVars, DelayInfo,
        ErrorList, LiveVarsBag, NondetLiveVarsBag, InstVarSet, [],
        HowToCheck, MayChangeProc, MayInitSolverVars, ModeSubInfo).

%-----------------------------------------------------------------------------%

mode_info_get_module_info(MI, MI ^ module_info).
mode_info_get_predid(MI, MI ^ predid).
mode_info_get_procid(MI, MI ^ mode_sub_info ^ procid).
mode_info_get_debug_modes(MI, MI ^ debug).
mode_info_get_varset(MI, MI ^ mode_sub_info ^ varset).
mode_info_get_var_types(MI, MI ^ var_types).
mode_info_get_context(MI, MI ^ context).
mode_info_get_mode_context(MI, MI ^ mode_context).
mode_info_get_instmap(MI, MI ^ instmap).
mode_info_get_instvarset(ModeInfo, ModeInfo ^ instvarset).
mode_info_get_locked_vars(MI, MI ^ locked_vars).
mode_info_get_errors(MI, MI ^ errors).
mode_info_get_warnings(MI, MI ^ mode_sub_info ^ warnings).
mode_info_get_need_to_requantify(MI, MI ^ mode_sub_info ^ need_to_requantify).
mode_info_get_in_promise_purity_scope(MI,
    MI ^ mode_sub_info ^ in_promise_purity_scope).
mode_info_get_delay_info(MI, MI ^ delay_info).
mode_info_get_live_vars(MI, MI ^ live_vars).
mode_info_get_nondet_live_vars(MI, MI ^ nondet_live_vars).
mode_info_get_last_checkpoint_insts(MI,
    MI ^ mode_sub_info ^ last_checkpoint_insts).
mode_info_get_parallel_vars(MI, MI ^  parallel_vars).
mode_info_get_changed_flag(MI, MI ^ mode_sub_info ^ changed_flag).
mode_info_get_how_to_check(MI, MI ^ how_to_check).
mode_info_get_may_change_called_proc(MI, MI ^ may_change_called_proc).
mode_info_get_initial_instmap(MI, MI ^ mode_sub_info ^ initial_instmap).

mode_info_set_module_info(ModuleInfo, MI, MI ^ module_info := ModuleInfo).
mode_info_set_predid(PredId, MI, MI ^ predid := PredId).
mode_info_set_procid(ProcId, MI, MI ^ mode_sub_info ^ procid := ProcId).
mode_info_set_varset(VarSet, MI, MI ^ mode_sub_info ^ varset := VarSet).
mode_info_set_var_types(VTypes, MI, MI ^ var_types := VTypes).
mode_info_set_context(Context, MI, MI ^ context := Context).
mode_info_set_mode_context(ModeContext, MI, MI ^ mode_context := ModeContext).
mode_info_set_locked_vars(LockedVars, MI, MI ^ locked_vars := LockedVars).
mode_info_set_errors(Errors, MI, MI ^ errors := Errors).
mode_info_set_warnings(Warnings, MI,
    MI ^ mode_sub_info ^ warnings := Warnings).
mode_info_set_need_to_requantify(NTRQ, MI,
    MI ^ mode_sub_info ^ need_to_requantify := NTRQ).
mode_info_set_in_promise_purity_scope(INC, MI,
    MI ^ mode_sub_info ^ in_promise_purity_scope := INC).
mode_info_set_delay_info(DelayInfo, MI, MI ^ delay_info := DelayInfo).
mode_info_set_live_vars(LiveVarsList, MI, MI ^ live_vars := LiveVarsList).
mode_info_set_nondet_live_vars(NondetLiveVars, MI,
    MI ^ nondet_live_vars := NondetLiveVars).
mode_info_set_last_checkpoint_insts(LastCheckpointInsts, MI,
    MI ^ mode_sub_info ^ last_checkpoint_insts := LastCheckpointInsts).
mode_info_set_parallel_vars(PVars, MI, MI ^ parallel_vars := PVars).
mode_info_set_changed_flag(Changed, MI,
    MI ^ mode_sub_info ^ changed_flag := Changed).
mode_info_set_how_to_check(How, MI, MI ^ how_to_check := How).
mode_info_set_may_change_called_proc(MayChange, MI,
    MI ^ may_change_called_proc := MayChange).

%-----------------------------------------------------------------------------%

mode_info_get_preds(ModeInfo, Preds) :-
    module_info_preds(ModeInfo ^ module_info, Preds).

mode_info_get_modes(ModeInfo, Modes) :-
    module_info_get_mode_table(ModeInfo ^ module_info, Modes).

mode_info_get_insts(ModeInfo, Insts) :-
    module_info_get_inst_table(ModeInfo ^ module_info, Insts).

mode_info_set_call_context(unify(UnifyContext), !MI) :-
    mode_info_set_mode_context(unify(UnifyContext, left), !MI).
mode_info_set_call_context(call(CallId), !MI) :-
    mode_info_set_mode_context(call(CallId, 0), !MI).

mode_info_set_call_arg_context(ArgNum, ModeInfo0, ModeInfo) :-
    mode_info_get_mode_context(ModeInfo0, ModeContext0),
    ( ModeContext0 = call(CallId, _) ->
        mode_info_set_mode_context(call(CallId, ArgNum),
            ModeInfo0, ModeInfo)
    ; ModeContext0 = unify(_UnifyContext, _Side) ->
        % This only happens when checking that the typeinfo variables
        % for polymorphic complicated unifications are ground.
        % For that case, we don't care about the ArgNum.
        ModeInfo = ModeInfo0
    ;
        unexpected(this_file, "mode_info_set_call_arg_context")
    ).

mode_info_unset_call_context(!MI) :-
    mode_info_set_mode_context(uninitialized, !MI).

%-----------------------------------------------------------------------------%

mode_info_set_instmap(InstMap, !MI) :-
    InstMap0 = !.MI ^ instmap,
    !:MI = !.MI ^ instmap := InstMap,
    (
        instmap__is_unreachable(InstMap),
        instmap__is_reachable(InstMap0)
    ->
        DelayInfo0 = !.MI ^ delay_info,
        delay_info__bind_all_vars(DelayInfo0, DelayInfo),
        !:MI = !.MI ^ delay_info := DelayInfo
    ;
        true
    ).

%-----------------------------------------------------------------------------%

mode_info_get_num_errors(ModeInfo, NumErrors) :-
    list__length(ModeInfo^errors, NumErrors).

%-----------------------------------------------------------------------------%

    % We keep track of the live variables and the nondet-live variables
    % a bag, represented as a list of sets of vars.
    % This allows us to easily add and remove sets of variables.
    % It's probably not maximally efficient.

    % Add a set of vars to the bag of live vars and
    % the bag of nondet-live vars.

mode_info_add_live_vars(NewLiveVars, !MI) :-
    LiveVars0 = !.MI ^ live_vars,
    NondetLiveVars0 = !.MI ^ nondet_live_vars,
    svbag__insert_set(NewLiveVars, LiveVars0, LiveVars),
    svbag__insert_set(NewLiveVars, NondetLiveVars0, NondetLiveVars),
    !:MI = !.MI ^ live_vars := LiveVars,
    !:MI = !.MI ^ nondet_live_vars := NondetLiveVars.

    % Remove a set of vars from the bag of live vars and
    % the bag of nondet-live vars.

mode_info_remove_live_vars(OldLiveVars, !MI) :-
    LiveVars0 = !.MI ^ live_vars,
    NondetLiveVars0 = !.MI ^ nondet_live_vars,
    svbag__det_remove_set(OldLiveVars, LiveVars0, LiveVars),
    svbag__det_remove_set(OldLiveVars, NondetLiveVars0, NondetLiveVars),
    !:MI = !.MI ^ live_vars := LiveVars,
    !:MI = !.MI ^ nondet_live_vars := NondetLiveVars,
        % when a variable becomes dead, we may be able to wake
        % up a goal which is waiting on that variable
    set__to_sorted_list(OldLiveVars, VarList),
    DelayInfo0 = !.MI ^ delay_info,
    delay_info__bind_var_list(VarList, DelayInfo0, DelayInfo),
    !:MI = !.MI ^ delay_info := DelayInfo.

    % Check whether a list of variables are live or not

mode_info_var_list_is_live(_, [], []).
mode_info_var_list_is_live(ModeInfo, [Var | Vars], [Live | Lives]) :-
    mode_info_var_is_live(ModeInfo, Var, Live),
    mode_info_var_list_is_live(ModeInfo, Vars, Lives).

    % Check whether a variable is live or not

mode_info_var_is_live(ModeInfo, Var, Result) :-
    ( bag__contains(ModeInfo ^ live_vars, Var) ->
        Result = live
    ;
        Result = dead
    ).

    % Check whether a variable is nondet_live or not.

mode_info_var_is_nondet_live(ModeInfo, Var, Result) :-
    ( bag__contains(ModeInfo ^ nondet_live_vars, Var) ->
        Result = live
    ;
        Result = dead
    ).

mode_info_get_liveness(ModeInfo, LiveVars) :-
    bag__to_list_without_duplicates(ModeInfo ^ live_vars, SortedList),
    set__sorted_list_to_set(SortedList, LiveVars).

%-----------------------------------------------------------------------------%

mode_info_get_types_of_vars(ModeInfo, Vars, TypesOfVars) :-
    mode_info_get_var_types(ModeInfo, VarTypes),
    map__apply_to_list(Vars, VarTypes, TypesOfVars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % The locked variables are stored as a stack
    % of sets of variables.  A variable is locked if it is
    % a member of any of the sets.  To lock a set of vars, we just
    % push them on the stack, and to unlock a set of vars, we just
    % pop them off the stack.  The stack is implemented as a list.

mode_info_lock_vars(Reason, Vars, !ModeInfo) :-
    mode_info_get_locked_vars(!.ModeInfo, LockedVars),
    mode_info_set_locked_vars([Reason - Vars | LockedVars], !ModeInfo).

mode_info_unlock_vars(Reason, Vars, !ModeInfo) :-
    mode_info_get_locked_vars(!.ModeInfo, LockedVars0),
    (
        LockedVars0 = [Reason - TheseVars | LockedVars1],
        set__equal(TheseVars, Vars)
    ->
        LockedVars = LockedVars1
    ;
        unexpected(this_file,
            "mode_info_unlock_vars: some kind of nesting error")
    ),
    mode_info_set_locked_vars(LockedVars, !ModeInfo).

mode_info_var_is_locked(ModeInfo, Var, Reason) :-
    mode_info_get_locked_vars(ModeInfo, LockedVarsList),
    mode_info_var_is_locked_2(LockedVarsList, Var, Reason).

:- pred mode_info_var_is_locked_2(locked_vars::in, prog_var::in,
    var_lock_reason::out) is semidet.

mode_info_var_is_locked_2([ThisReason - Set | Sets], Var, Reason) :-
    ( set__member(Var, Set) ->
        Reason = ThisReason
    ;
        mode_info_var_is_locked_2(Sets, Var, Reason)
    ).

mode_info_set_checking_extra_goals(Checking, !MI) :-
    (
        yes = !.MI ^ mode_sub_info ^ checking_extra_goals,
        Checking = yes
    ->
        % This should never happen - once the extra goals are
        % introduced, rechecking the goal should not introduce
        % more extra goals.
        unexpected(this_file, 
            "mode analysis: rechecking extra goals " ++
            "adds more extra goals")
    ;
        !:MI = !.MI ^ mode_sub_info ^ checking_extra_goals := Checking
    ).

%-----------------------------------------------------------------------------%

mode_info_get_call_id(ModeInfo, PredId, CallId) :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_call_id(PredInfo, CallId).

%-----------------------------------------------------------------------------%

mode_info_error(Vars, ModeError, !ModeInfo) :-
    mode_info_get_context(!.ModeInfo, Context),
    mode_info_get_mode_context(!.ModeInfo, ModeContext),
    ModeErrorInfo = mode_error_info(Vars, ModeError, Context, ModeContext),
    mode_info_add_error(ModeErrorInfo, !ModeInfo).

mode_info_add_error(ModeErrorInfo, !ModeInfo) :-
    mode_info_get_errors(!.ModeInfo, Errors0),
    list__append(Errors0, [ModeErrorInfo], Errors),
    mode_info_set_errors(Errors, !ModeInfo).

mode_info_warning(ModeWarning, !ModeInfo) :-
    mode_info_get_context(!.ModeInfo, Context),
    mode_info_get_mode_context(!.ModeInfo, ModeContext),
    ModeWarningInfo = mode_warning_info(ModeWarning, Context, ModeContext),
    mode_info_add_warning(ModeWarningInfo, !ModeInfo).

mode_info_add_warning(ModeWarningInfo, !ModeInfo) :-
    mode_info_get_warnings(!.ModeInfo, Warnings0),
    list__append(Warnings0, [ModeWarningInfo], Warnings),
    mode_info_set_warnings(Warnings, !ModeInfo).

mode_info_need_to_requantify(!ModeInfo) :-
    mode_info_set_need_to_requantify(yes, !ModeInfo).

%-----------------------------------------------------------------------------%

mode_info_may_initialise_solver_vars(ModeInfo) :-
    ModeInfo ^ may_initialise_solver_vars = yes.

mode_info_get_may_initialise_solver_vars(MayInit, !.ModeInfo) :-
    MayInit = !.ModeInfo ^ may_initialise_solver_vars.

mode_info_set_may_initialise_solver_vars(MayInit, !ModeInfo) :-
    !:ModeInfo = !.ModeInfo ^ may_initialise_solver_vars := MayInit.

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "mode_info.m".

%-----------------------------------------------------------------------------%
