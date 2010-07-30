%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001, 2003-2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mode_info.m.
% Main author: fjh.
%
% This file defines the mode_info data structure, which is used to hold
% the information we need during mode analysis.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.mode_info.
:- interface.

:- import_module check_hlds.delay_info.
:- import_module check_hlds.mode_errors.
:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module hlds.instmap.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bag.
:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module set.

%-----------------------------------------------------------------------------%

    % The mode_info data structure and access predicates.

    % XXX `side' is not used
:- type mode_context
    --->    mode_context_call(
                call_id,
                int             % Argument number (offset so that the real
                                % arguments start at number 1 whereas the
                                % type_info arguments have numbers <= 0).
            )
    ;       mode_context_unify(
                unify_context,  % original source of the unification
                side            % LHS or RHS
            )
    ;       mode_context_uninitialized.

:- type side
    --->    left
    ;       right.

:- type call_context
    --->    call_context_unify(unify_context)
    ;       call_context_call(call_id).

:- type var_lock_reason
    --->    var_lock_negation
    ;       var_lock_if_then_else
    ;       var_lock_lambda(pred_or_func)
    ;       var_lock_trace_goal
    ;       var_lock_atomic_goal
    ;       var_lock_par_conj.

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

    % The mode_info contains a flag indicating whether initialisation calls,
    % converting a solver variable from `free' to `any', may be inserted
    % during mode analysis.
:- type may_init_solver_vars
    --->    may_init_solver_vars
    ;       may_not_init_solver_vars.

:- type in_promise_purity_scope
    --->    in_promise_purity_scope
    ;       not_in_promise_purity_scope.

:- type in_from_ground_term_scope
    --->    in_from_ground_term_scope
    ;       not_in_from_ground_term_scope.

:- type had_from_ground_term_scope
    --->    had_from_ground_term_scope
    ;       did_not_have_from_ground_term_scope.

:- type make_ground_terms_unique
    --->    make_ground_terms_unique
    ;       do_not_make_ground_terms_unique.

:- type in_dupl_for_switch
    --->    in_dupl_for_switch
    ;       not_in_dupl_for_switch.

    % We use a stack of pairs of sets of variables used to mode-check
    % parallel conjunctions. The first set is the nonlocals of the
    % parallel conjunction. The second set is a subset of the
    % first, and is the set of variables that have been [further]
    % bound inside the current parallel conjunct. The stack
    % is for the correct handling of nested parallel conjunctions.
:- type par_conj_mode_check_stack == list(par_conj_mode_check).
:- type par_conj_mode_check
    --->    par_conj_mode_check(
                par_conj_nonlocals  :: set(prog_var),
                par_conj_bound      :: set(prog_var)
            ).

%-----------------------------------------------------------------------------%

:- pred mode_info_get_module_info(mode_info::in, module_info::out) is det.
:- pred mode_info_get_preds(mode_info::in, pred_table::out) is det.
:- pred mode_info_get_modes(mode_info::in, mode_table::out) is det.
:- pred mode_info_get_insts(mode_info::in, inst_table::out) is det.
:- pred mode_info_get_pred_id(mode_info::in, pred_id::out) is det.
:- pred mode_info_get_proc_id(mode_info::in, proc_id::out) is det.
:- pred mode_info_get_debug_modes(mode_info::in, maybe(debug_flags)::out)
    is det.
:- pred mode_info_get_context(mode_info::in, prog_context::out) is det.
:- pred mode_info_get_mode_context(mode_info::in, mode_context::out) is det.
:- pred mode_info_get_instmap(mode_info::in, instmap::out) is det.
:- pred mode_info_get_locked_vars(mode_info::in, locked_vars::out) is det.
:- pred mode_info_get_errors(mode_info::in, list(mode_error_info)::out) is det.
:- pred mode_info_get_warnings(mode_info::in, list(mode_warning_info)::out)
    is det.
:- pred mode_info_get_need_to_requantify(mode_info::in,
    need_to_requantify::out) is det.
:- pred mode_info_get_in_promise_purity_scope(mode_info::in,
    in_promise_purity_scope::out) is det.
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
    par_conj_mode_check_stack::out) is det.
:- pred mode_info_get_changed_flag(mode_info::in, bool::out) is det.
:- pred mode_info_get_how_to_check(mode_info::in,
    how_to_check_goal::out) is det.
:- pred mode_info_get_may_change_called_proc(mode_info::in,
    may_change_called_proc::out) is det.
:- pred mode_info_get_initial_instmap(mode_info::in, instmap::out) is det.
:- pred mode_info_get_checking_extra_goals(mode_info::in, bool::out) is det.
:- pred mode_info_get_may_init_solver_vars(mode_info::in,
    may_init_solver_vars::out) is det.
:- pred mode_info_get_in_from_ground_term(mode_info::in,
    in_from_ground_term_scope::out) is det.
:- pred mode_info_get_had_from_ground_term(mode_info::in,
    had_from_ground_term_scope::out) is det.
:- pred mode_info_get_make_ground_terms_unique(mode_info::in,
    make_ground_terms_unique::out) is det.
:- pred mode_info_get_in_dupl_for_switch(mode_info::in,
    in_dupl_for_switch::out) is det.

%-----------------------------------------------------------------------------%

:- pred mode_info_set_module_info(module_info::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_pred_id(pred_id::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_proc_id(proc_id::in,
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
:- pred mode_info_set_need_to_requantify(need_to_requantify::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_in_promise_purity_scope(in_promise_purity_scope::in,
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
:- pred mode_info_set_parallel_vars(par_conj_mode_check_stack::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_changed_flag(bool::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_how_to_check(how_to_check_goal::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_may_change_called_proc(may_change_called_proc::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_checking_extra_goals(bool::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_may_init_solver_vars(may_init_solver_vars::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_in_from_ground_term(in_from_ground_term_scope::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_had_from_ground_term(had_from_ground_term_scope::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_make_ground_terms_unique(make_ground_terms_unique::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_in_dupl_for_switch(in_dupl_for_switch::in,
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

    % Check whether a variable or a list of variables are live or not.
    %
:- pred mode_info_var_list_is_live(mode_info::in, list(prog_var)::in,
    list(is_live)::out) is det.
:- pred mode_info_var_is_live(mode_info::in, prog_var::in,
    is_live::out) is det.

    % Check whether a variable is nondet_live or not.
    %
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

:- pred mode_info_may_init_solver_vars(mode_info::in) is semidet.

    % Succeeds if automatic initialisation of solver variables is
    % supported.  This is only the case if the developer-only option
    % `--solver-type-auto-init' is enabled.
    %
:- pred mode_info_solver_init_is_supported(mode_info::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.delay_info.
:- import_module check_hlds.mode_errors.
:- import_module hlds.hlds_clauses.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.

:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module string.
:- import_module svbag.
:- import_module term.
:- import_module varset.

:- type mode_sub_info
    --->    mode_sub_info(
                % The pred we are checking.
                msi_pred_id                 :: pred_id,

                % The mode which we are checking.
                msi_proc_id                 :: proc_id,

                % The variables in the current proc.
                msi_varset                  :: prog_varset,

                % The types of the variables.
                msi_vartypes                :: vartypes,

                % Is mode debugging of this procedure enabled? If yes,
                % is verbose mode debugging enabled, is minimal mode debugging
                % enabled, and is statistics printing enabled?
                msi_debug                   :: maybe(debug_flags),

                % The "locked" variables, i.e. variables which cannot be
                % further instantiated inside a negated context.
                msi_locked_vars             :: locked_vars,

                % The live variables, i.e. those variables which may be
                % referenced again on forward execution or after shallow
                % backtracking. (By shallow backtracking, I mean semidet
                % backtracking in a negation, if-then-else, or semidet
                % disjunction within the current predicate.)
                msi_live_vars               :: bag(prog_var),

                msi_instvarset              :: inst_varset,

                % A stack of pairs of sets of variables used to mode-check
                % parallel conjunctions. The first set is the nonlocals of the
                % parallel conjunction. The second set is a subset of the
                % first, and is the set of variables that have been [further]
                % bound inside the current parallel conjunct - the stack
                % is for the correct handling of nested parallel conjunctions.
                msi_par_conj                :: par_conj_mode_check_stack,

                msi_how_to_check            :: how_to_check_goal,

                % Is mode analysis allowed to change which procedure is called?
                msi_may_change_called_proc  :: may_change_called_proc,

                % `yes' if calls to the initialisation predicates for solver
                % vars can be inserted during mode analysis in order to make
                % goals schedulable.
                msi_may_init_solver_vars    :: may_init_solver_vars,

                % This field is used by the checkpoint code when debug_modes
                % is on. It has the instmap that was current at the last mode
                % checkpoint, so that checkpoints do not print out the insts
                % of variables whose insts have not changed since the last
                % checkpoint. This field will always contain an unreachable
                % instmap if debug_modes is off, since its information is not
                % needed then.
                msi_last_checkpoint_insts   :: instmap,

                % Changed flag: if `yes', then we may need to repeat
                % mode inference.
                msi_changed_flag            :: bool,

                % Are we rechecking a goal after introducing unifications
                % for complicated sub-unifications or an implied mode? If so,
                % redoing the mode check should not introduce more extra
                % unifications.
                msi_checking_extra_goals    :: bool,

                % The initial instmap of the procedure body. Used to decide
                % whether a unification that cannot fail could be influenced
                % by an argument mode that enforces a subtype.
                msi_initial_instmap         :: instmap,

                % The mode warnings found.
                msi_warnings                :: list(mode_warning_info),

                % Says whether we need to requantify the procedure body
                % after mode analysis finishes.
                msi_need_to_requantify      :: need_to_requantify,

                % Says whether we are in a promise_<purity> scope.
                % This information is needed to check that potentially impure
                % uses of inst any non-locals in negated contexts are properly
                % acknowledged by the programmer.
                msi_in_promise_purity_scope :: in_promise_purity_scope,

                % Says whether we are in a from_ground_term scope.
                % This information allows us to optimize some aspects of
                % mode analysis.
                msi_in_from_ground_term     :: in_from_ground_term_scope,

                % Says whether we have ever come across in a from_ground_term
                % scope.
                msi_had_from_ground_term    :: had_from_ground_term_scope,

                % Says whether we should copy the ground terms created by
                % from_ground_term scopes, making them unique.
                msi_make_ground_terms_unique :: make_ground_terms_unique,

                % Set to `yes' if we are inside a goal with a
                % duplicate_for_switch feature.
                msi_in_dupl_for_switch      :: in_dupl_for_switch
            ).

    % Please try to keep the size of this structure down to eight fields.
    % Even one more field will cause the Boehm allocator to round up the size
    % of each memory cell to 16 words.
    %
:- type mode_info
    --->    mode_info(
/*  1 */        mi_module_info              :: module_info,

                % The current instantiatedness of the variables.
/*  2 */        mi_instmap                  :: instmap,

                % Info about delayed goals.
/*  3 */        mi_delay_info               :: delay_info,

                % The mode errors found.
/*  4 */        mi_errors                   :: list(mode_error_info),

                % A description of where in the goal the error occurred.
/*  5 */        mi_mode_context             :: mode_context,

                % The line number of the subgoal we are currently checking.
/*  6 */        mi_context                  :: prog_context,

                % The nondet-live variables, i.e. those variables which may be
                % referenced again after deep backtracking TO THE CURRENT
                % EXECUTION POINT. These are the variables which need to be
                % made mostly_unique rather than unique when we get to a
                % nondet disjunction or a nondet call.  We do not include
                % variables which may be referenced again after backtracking
                % to a point EARLIER THAN the current execution point, since
                % those variables will *already* have been marked as
                % mostly_unique rather than unique.)
/*  7 */        mi_nondet_live_vars         :: bag(prog_var),

/*  8 */        mi_sub_info                 :: mode_sub_info
            ).

%-----------------------------------------------------------------------------%

mode_info_init(ModuleInfo, PredId, ProcId, Context, LiveVars, InstMap0,
        HowToCheck, MayChangeProc, ModeInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_modes, DebugModes),
    globals.lookup_int_option(Globals, debug_modes_pred_id,
        DebugModesPredId),
    pred_id_to_int(PredId, PredIdInt),
    (
        DebugModes = yes,
        ( DebugModesPredId >= 0 => DebugModesPredId = PredIdInt )
    ->
        globals.lookup_bool_option(Globals, debug_modes_verbose, DebugVerbose),
        globals.lookup_bool_option(Globals, debug_modes_minimal, DebugMinimal),
        globals.lookup_bool_option(Globals, debug_modes_statistics,
            Statistics),
        Flags = debug_flags(DebugVerbose, DebugMinimal, Statistics),
        Debug = yes(Flags)
    ;
        Debug = no
    ),

    module_info_proc_info(ModuleInfo, PredId, ProcId, ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),

    bag.from_set(LiveVars, LiveVarsBag),
    instmap.init_unreachable(LastCheckpointInstMap),
    LockedVars = [],
    ParallelVars = [],
    WarningList = [],

    Changed = no,
    CheckingExtraGoals = no,
    MayInitSolverVars = may_init_solver_vars,
    NeedToRequantify = do_not_need_to_requantify,
    InPromisePurityScope = not_in_promise_purity_scope,
    InFromGroundTerm = not_in_from_ground_term_scope,
    HadFromGroundTerm = did_not_have_from_ground_term_scope,
    MakeGroundTermsUnique = do_not_make_ground_terms_unique,
    InDuplForSwitch = not_in_dupl_for_switch,

    ModeSubInfo = mode_sub_info(PredId, ProcId, VarSet, VarTypes, Debug,
        LockedVars, LiveVarsBag, InstVarSet, ParallelVars, HowToCheck,
        MayChangeProc, MayInitSolverVars, LastCheckpointInstMap, Changed,
        CheckingExtraGoals, InstMap0, WarningList, NeedToRequantify,
        InPromisePurityScope, InFromGroundTerm, HadFromGroundTerm,
        MakeGroundTermsUnique, InDuplForSwitch),

    mode_context_init(ModeContext),
    delay_info_init(DelayInfo),
    ErrorList = [],
    bag.from_set(LiveVars, NondetLiveVarsBag),

    ModeInfo = mode_info(ModuleInfo, InstMap0, DelayInfo, ErrorList,
        ModeContext, Context, NondetLiveVarsBag, ModeSubInfo).

%-----------------------------------------------------------------------------%

mode_info_get_module_info(MI, MI ^ mi_module_info).
mode_info_get_pred_id(MI, MI ^ mi_sub_info ^ msi_pred_id).
mode_info_get_proc_id(MI, MI ^ mi_sub_info ^ msi_proc_id).
mode_info_get_debug_modes(MI, MI ^ mi_sub_info ^ msi_debug).
mode_info_get_varset(MI, MI ^ mi_sub_info ^ msi_varset).
mode_info_get_var_types(MI, MI ^ mi_sub_info ^ msi_vartypes).
mode_info_get_context(MI, MI ^ mi_context).
mode_info_get_mode_context(MI, MI ^ mi_mode_context).
mode_info_get_instmap(MI, MI ^ mi_instmap).
mode_info_get_instvarset(ModeInfo, ModeInfo ^ mi_sub_info ^ msi_instvarset).
mode_info_get_locked_vars(MI, MI ^ mi_sub_info ^ msi_locked_vars).
mode_info_get_errors(MI, MI ^ mi_errors).
mode_info_get_warnings(MI, MI ^ mi_sub_info ^ msi_warnings).
mode_info_get_need_to_requantify(MI,
    MI ^ mi_sub_info ^ msi_need_to_requantify).
mode_info_get_in_promise_purity_scope(MI,
    MI ^ mi_sub_info ^ msi_in_promise_purity_scope).
mode_info_get_delay_info(MI, MI ^ mi_delay_info).
mode_info_get_live_vars(MI, MI ^ mi_sub_info ^ msi_live_vars).
mode_info_get_nondet_live_vars(MI, MI ^ mi_nondet_live_vars).
mode_info_get_last_checkpoint_insts(MI,
    MI ^ mi_sub_info ^ msi_last_checkpoint_insts).
mode_info_get_parallel_vars(MI, MI ^  mi_sub_info ^ msi_par_conj).
mode_info_get_changed_flag(MI, MI ^ mi_sub_info ^ msi_changed_flag).
mode_info_get_how_to_check(MI, MI ^ mi_sub_info ^ msi_how_to_check).
mode_info_get_may_change_called_proc(MI,
    MI ^ mi_sub_info ^ msi_may_change_called_proc).
mode_info_get_may_init_solver_vars(MI,
    MI ^ mi_sub_info ^ msi_may_init_solver_vars).
mode_info_get_initial_instmap(MI, MI ^ mi_sub_info ^ msi_initial_instmap).
mode_info_get_checking_extra_goals(MI,
    MI ^ mi_sub_info ^ msi_checking_extra_goals).
mode_info_get_in_from_ground_term(MI,
    MI ^ mi_sub_info ^ msi_in_from_ground_term).
mode_info_get_had_from_ground_term(MI,
    MI ^ mi_sub_info ^ msi_had_from_ground_term).
mode_info_get_make_ground_terms_unique(MI,
    MI ^ mi_sub_info ^ msi_make_ground_terms_unique).
mode_info_get_in_dupl_for_switch(MI,
    MI ^ mi_sub_info ^ msi_in_dupl_for_switch).

mode_info_set_module_info(ModuleInfo, MI, MI ^ mi_module_info := ModuleInfo).
mode_info_set_pred_id(PredId, MI, MI ^ mi_sub_info ^ msi_pred_id := PredId).
mode_info_set_proc_id(ProcId, MI, MI ^ mi_sub_info ^ msi_proc_id := ProcId).
mode_info_set_varset(VarSet, MI, MI ^ mi_sub_info ^ msi_varset := VarSet).
mode_info_set_var_types(VarTypes, MI,
    MI ^ mi_sub_info ^ msi_vartypes := VarTypes).
mode_info_set_context(Context, MI, MI ^ mi_context := Context).
mode_info_set_mode_context(ModeContext,
    MI, MI ^ mi_mode_context := ModeContext).
mode_info_set_locked_vars(LockedVars, MI,
    MI ^ mi_sub_info ^ msi_locked_vars := LockedVars).
mode_info_set_errors(Errors, MI, MI ^ mi_errors := Errors).
mode_info_set_warnings(Warnings, MI,
    MI ^ mi_sub_info ^ msi_warnings := Warnings).
mode_info_set_need_to_requantify(NTRQ, MI,
    MI ^ mi_sub_info ^ msi_need_to_requantify := NTRQ).
mode_info_set_in_promise_purity_scope(INC, MI,
    MI ^ mi_sub_info ^ msi_in_promise_purity_scope := INC).
mode_info_set_delay_info(DelayInfo, MI, MI ^ mi_delay_info := DelayInfo).
mode_info_set_live_vars(LiveVarsList, MI,
    MI ^ mi_sub_info ^ msi_live_vars := LiveVarsList).
mode_info_set_nondet_live_vars(NondetLiveVars, MI,
    MI ^ mi_nondet_live_vars := NondetLiveVars).
mode_info_set_last_checkpoint_insts(LastCheckpointInsts, MI,
    MI ^ mi_sub_info ^ msi_last_checkpoint_insts := LastCheckpointInsts).
mode_info_set_parallel_vars(PVars, MI,
    MI ^ mi_sub_info ^ msi_par_conj := PVars).
mode_info_set_changed_flag(Changed, MI,
    MI ^ mi_sub_info ^ msi_changed_flag := Changed).
mode_info_set_how_to_check(How, MI,
    MI ^ mi_sub_info ^ msi_how_to_check := How).
mode_info_set_may_change_called_proc(MayChange, MI,
    MI ^ mi_sub_info ^ msi_may_change_called_proc := MayChange).
mode_info_set_may_init_solver_vars(MayInit, MI,
    MI ^ mi_sub_info ^ msi_may_init_solver_vars := MayInit).
mode_info_set_in_from_ground_term(IFGI, MI,
    MI ^ mi_sub_info ^ msi_in_from_ground_term := IFGI).
mode_info_set_had_from_ground_term(HFGI, MI,
    MI ^ mi_sub_info ^ msi_had_from_ground_term := HFGI).
mode_info_set_make_ground_terms_unique(MGTU, MI,
    MI ^ mi_sub_info ^ msi_make_ground_terms_unique := MGTU).
mode_info_set_in_dupl_for_switch(INFS, MI,
    MI ^ mi_sub_info ^ msi_in_dupl_for_switch := INFS).

%-----------------------------------------------------------------------------%

mode_info_get_preds(ModeInfo, Preds) :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_preds(ModuleInfo, Preds).

mode_info_get_modes(ModeInfo, Modes) :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_mode_table(ModuleInfo, Modes).

mode_info_get_insts(ModeInfo, Insts) :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_inst_table(ModuleInfo, Insts).

mode_info_set_call_context(call_context_unify(UnifyContext), !MI) :-
    mode_info_set_mode_context(mode_context_unify(UnifyContext, left), !MI).
mode_info_set_call_context(call_context_call(CallId), !MI) :-
    mode_info_set_mode_context(mode_context_call(CallId, 0), !MI).

mode_info_set_call_arg_context(ArgNum, !ModeInfo) :-
    mode_info_get_mode_context(!.ModeInfo, ModeContext0),
    (
        ModeContext0 = mode_context_call(CallId, _),
        mode_info_set_mode_context(mode_context_call(CallId, ArgNum),
            !ModeInfo)
    ;
        ModeContext0 = mode_context_unify(_UnifyContext, _Side),
        % This only happens when checking that the typeinfo variables
        % for polymorphic complicated unifications are ground.
        % For that case, we don't care about the ArgNum.
        true
    ;
        ModeContext0 = mode_context_uninitialized,
        unexpected(this_file, "mode_info_set_call_arg_context uninitialized")
    ).

mode_info_unset_call_context(!MI) :-
    mode_info_set_mode_context(mode_context_uninitialized, !MI).

%-----------------------------------------------------------------------------%

mode_info_set_instmap(InstMap, !MI) :-
    mode_info_get_instmap(!.MI, InstMap0),
    !MI ^ mi_instmap := InstMap,
    (
        instmap_is_unreachable(InstMap),
        instmap_is_reachable(InstMap0)
    ->
        mode_info_get_delay_info(!.MI, DelayInfo0),
        delay_info_bind_all_vars(DelayInfo0, DelayInfo),
        mode_info_set_delay_info(DelayInfo, !MI)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

mode_info_get_num_errors(ModeInfo, NumErrors) :-
    mode_info_get_errors(ModeInfo, Errors),
    list.length(Errors, NumErrors).

%-----------------------------------------------------------------------------%

    % We keep track of the live variables and the nondet-live variables
    % a bag, represented as a list of sets of vars.
    % This allows us to easily add and remove sets of variables.
    % It's probably not maximally efficient.

    % Add a set of vars to the bag of live vars and the bag of
    % nondet-live vars.

mode_info_add_live_vars(NewLiveVars, !MI) :-
    mode_info_get_live_vars(!.MI, LiveVars0),
    mode_info_get_nondet_live_vars(!.MI, NondetLiveVars0),
    svbag.insert_set(NewLiveVars, LiveVars0, LiveVars),
    svbag.insert_set(NewLiveVars, NondetLiveVars0, NondetLiveVars),
    mode_info_set_live_vars(LiveVars, !MI),
    mode_info_set_nondet_live_vars(NondetLiveVars, !MI).

    % Remove a set of vars from the bag of live vars and
    % the bag of nondet-live vars.

mode_info_remove_live_vars(OldLiveVars, !MI) :-
    mode_info_get_live_vars(!.MI, LiveVars0),
    mode_info_get_nondet_live_vars(!.MI, NondetLiveVars0),
    svbag.det_remove_set(OldLiveVars, LiveVars0, LiveVars),
    svbag.det_remove_set(OldLiveVars, NondetLiveVars0, NondetLiveVars),
    mode_info_set_live_vars(LiveVars, !MI),
    mode_info_set_nondet_live_vars(NondetLiveVars, !MI),

    % When a variable becomes dead, we may be able to wake up a goal
    % which is waiting on that variable.
    set.to_sorted_list(OldLiveVars, VarList),
    mode_info_get_delay_info(!.MI, DelayInfo0),
    delay_info_bind_var_list(VarList, DelayInfo0, DelayInfo),
    mode_info_set_delay_info(DelayInfo, !MI).

mode_info_var_list_is_live(_, [], []).
mode_info_var_list_is_live(ModeInfo, [Var | Vars], [Live | Lives]) :-
    mode_info_var_is_live(ModeInfo, Var, Live),
    mode_info_var_list_is_live(ModeInfo, Vars, Lives).

    % Check whether a variable is live or not

mode_info_var_is_live(ModeInfo, Var, Result) :-
    mode_info_get_live_vars(ModeInfo, LiveVars0),
    ( bag.contains(LiveVars0, Var) ->
        Result = is_live
    ;
        Result = is_dead
    ).

mode_info_var_is_nondet_live(ModeInfo, Var, Result) :-
    mode_info_get_nondet_live_vars(ModeInfo, NondetLiveVars0),
    ( bag.contains(NondetLiveVars0, Var) ->
        Result = is_live
    ;
        Result = is_dead
    ).

mode_info_get_liveness(ModeInfo, LiveVars) :-
    mode_info_get_live_vars(ModeInfo, LiveVarsBag),
    bag.to_list_without_duplicates(LiveVarsBag, SortedList),
    set.sorted_list_to_set(SortedList, LiveVars).

%-----------------------------------------------------------------------------%

mode_info_get_types_of_vars(ModeInfo, Vars, TypesOfVars) :-
    mode_info_get_var_types(ModeInfo, VarTypes),
    map.apply_to_list(Vars, VarTypes, TypesOfVars).

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
        set.equal(TheseVars, Vars)
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
    ( set.member(Var, Set) ->
        Reason = ThisReason
    ;
        mode_info_var_is_locked_2(Sets, Var, Reason)
    ).

mode_info_set_checking_extra_goals(Checking, !MI) :-
    mode_info_get_checking_extra_goals(!.MI, Checking0),
    (
        Checking0 = yes,
        Checking = yes
    ->
        % This should never happen - once the extra goals are introduced,
        % rechecking the goal should not introduce more extra goals.
        unexpected(this_file,
            "mode analysis: rechecking extra goals adds more extra goals")
    ;
        !MI ^ mi_sub_info ^ msi_checking_extra_goals := Checking
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
    list.append(Errors0, [ModeErrorInfo], Errors),
    mode_info_set_errors(Errors, !ModeInfo).

mode_info_warning(ModeWarning, !ModeInfo) :-
    mode_info_get_context(!.ModeInfo, Context),
    mode_info_get_mode_context(!.ModeInfo, ModeContext),
    ModeWarningInfo = mode_warning_info(ModeWarning, Context, ModeContext),
    mode_info_add_warning(ModeWarningInfo, !ModeInfo).

mode_info_add_warning(ModeWarningInfo, !ModeInfo) :-
    mode_info_get_warnings(!.ModeInfo, Warnings0),
    list.append(Warnings0, [ModeWarningInfo], Warnings),
    mode_info_set_warnings(Warnings, !ModeInfo).

mode_info_need_to_requantify(!ModeInfo) :-
    mode_info_set_need_to_requantify(need_to_requantify, !ModeInfo).

%-----------------------------------------------------------------------------%

mode_info_may_init_solver_vars(ModeInfo) :-
    mode_info_get_may_init_solver_vars(ModeInfo, MayInitSolverVars),
    MayInitSolverVars = may_init_solver_vars.

%-----------------------------------------------------------------------------%

mode_info_solver_init_is_supported(ModeInfo) :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, solver_type_auto_init, yes).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "mode_info.m".

%-----------------------------------------------------------------------------%
