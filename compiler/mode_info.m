%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2001, 2003-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mode_info.m.
% Main author: fjh.
%
% This file defines the mode_info data structure, which is used to hold
% the information we need during mode analysis.
%
%---------------------------------------------------------------------------%

:- module check_hlds.mode_info.
:- interface.

:- import_module check_hlds.delay_info.
:- import_module check_hlds.mode_errors.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_inst_mode.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bag.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % The mode_info data structure and access predicates.

    % XXX `side' is not used
:- type mode_context
    --->    mode_context_call(
                mode_call_id,
                int             % Argument number (offset so that the real
                                % arguments start at number 1 whereas the
                                % type_info arguments have numbers <= 0).
            )
    ;       mode_context_unify(
                unify_context,  % original source of the unification
                side            % LHS or RHS
            )
    ;       mode_context_uninitialized.

    % Initialize a mode_context.
    %
:- pred mode_context_init(mode_context::out) is det.

:- type side
    --->    left
    ;       right.

:- type call_context
    --->    call_context_unify(unify_context)
    ;       call_context_call(mode_call_id).

:- type mode_call_id
    --->    mode_call_plain(pred_id)
    ;       mode_call_generic(generic_call_id).

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

:- type locked_vars == assoc_list(var_lock_reason, set_of_progvar).

:- type mode_info.

:- type debug_flags
    --->    debug_flags(
                % The empty string when mode checking, and a prefix to put
                % in front of goal kind names when unique mode checking.
                unique_prefix   :: string,

                % The value --debug-modes-verbose.
                verbose         :: bool,

                % The value --debug-modes-minimal.
                minimal         :: bool,

                % The value --debug-modes-statistics.
                statistics      :: bool
            ).

    % Initialize the mode_info.
    %
:- pred mode_info_init(module_info::in, pred_id::in, proc_id::in,
    prog_context::in, set_of_progvar::in, head_inst_vars::in, instmap::in,
    how_to_check_goal::in, may_change_called_proc::in, mode_info::out) is det.

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
                par_conj_nonlocals  :: set_of_progvar,
                par_conj_bound      :: set_of_progvar
            ).

:- type pred_var_multimode_error_map
    == map(prog_var, pred_id_var_multimode_error).

%---------------------------------------------------------------------------%
%
% Please keep these getter and setter predicates in the same order
% as the fields in mode_info/mode_sub_info.
%

    % Getters of the fields of mode_info.
:- pred mode_info_get_module_info(mode_info::in, module_info::out) is det.
:- pred mode_info_get_instmap(mode_info::in, instmap::out) is det.
:- pred mode_info_get_delay_info(mode_info::in, delay_info::out) is det.
:- pred mode_info_get_errors(mode_info::in, list(mode_error_info)::out) is det.
:- pred mode_info_get_mode_context(mode_info::in, mode_context::out) is det.
:- pred mode_info_get_context(mode_info::in, prog_context::out) is det.
:- pred mode_info_get_nondet_live_vars(mode_info::in, bag(prog_var)::out)
    is det.

    % Getters of the fields of mode_sub_info.
:- pred mode_info_get_pred_id(mode_info::in, pred_id::out) is det.
:- pred mode_info_get_proc_id(mode_info::in, proc_id::out) is det.
:- pred mode_info_get_varset(mode_info::in, prog_varset::out) is det.
:- pred mode_info_get_var_types(mode_info::in, vartypes::out) is det.
:- pred mode_info_get_debug_modes(mode_info::in, maybe(debug_flags)::out)
    is det.
:- pred mode_info_get_locked_vars(mode_info::in, locked_vars::out) is det.
:- pred mode_info_get_live_vars(mode_info::in, bag(prog_var)::out) is det.
:- pred mode_info_get_instvarset(mode_info::in, inst_varset::out) is det.
:- pred mode_info_get_parallel_vars(mode_info::in,
    par_conj_mode_check_stack::out) is det.
:- pred mode_info_get_last_checkpoint_insts(mode_info::in, instmap::out)
    is det.
:- pred mode_info_get_initial_instmap(mode_info::in, instmap::out) is det.
:- pred mode_info_get_head_inst_vars(mode_info::in, head_inst_vars::out)
    is det.
:- pred mode_info_get_warnings(mode_info::in, list(mode_warning_info)::out)
    is det.
:- pred mode_info_get_pred_var_multimode_error_map(mode_info::in,
    pred_var_multimode_error_map::out) is det.
:- pred mode_info_get_how_to_check(mode_info::in,
    how_to_check_goal::out) is det.
:- pred mode_info_get_may_change_called_proc(mode_info::in,
    may_change_called_proc::out) is det.
:- pred mode_info_get_changed_flag(mode_info::in, bool::out) is det.
:- pred mode_info_get_checking_extra_goals(mode_info::in, bool::out) is det.
:- pred mode_info_get_need_to_requantify(mode_info::in,
    need_to_requantify::out) is det.
:- pred mode_info_get_in_promise_purity_scope(mode_info::in,
    in_promise_purity_scope::out) is det.
:- pred mode_info_get_in_from_ground_term(mode_info::in,
    in_from_ground_term_scope::out) is det.
:- pred mode_info_get_had_from_ground_term(mode_info::in,
    had_from_ground_term_scope::out) is det.
:- pred mode_info_get_make_ground_terms_unique(mode_info::in,
    make_ground_terms_unique::out) is det.
:- pred mode_info_get_in_dupl_for_switch(mode_info::in,
    in_dupl_for_switch::out) is det.

%---------------------%

    % Setters of the fields of mode_info.
:- pred mode_info_set_module_info(module_info::in,
    mode_info::in, mode_info::out) is det.
% There is a mode_info_set_instmap, but it is not JUST a setter.
:- pred mode_info_set_delay_info(delay_info::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_errors(list(mode_error_info)::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_mode_context(mode_context::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_context(prog_context::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_nondet_live_vars(bag(prog_var)::in,
    mode_info::in, mode_info::out) is det.

    % Setters of the fields of mode_sub_info.
:- pred mode_info_set_pred_id(pred_id::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_proc_id(proc_id::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_varset(prog_varset::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_var_types(vartypes::in,
    mode_info::in, mode_info::out) is det.
% There is no mode_info_set_debug_modes; this field is read-only.
:- pred mode_info_set_locked_vars(locked_vars::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_live_vars(bag(prog_var)::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_instvarset(inst_varset::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_parallel_vars(par_conj_mode_check_stack::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_last_checkpoint_insts(instmap::in,
    mode_info::in, mode_info::out) is det.
% There is no mode_info_set_initial_instmap; this field is read-only.
% There is no mode_info_set_head_var_insts; this field is read-only.
:- pred mode_info_set_warnings(list(mode_warning_info)::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_pred_var_multimode_error_map(
    pred_var_multimode_error_map::in,
    mode_info::in, mode_info::out) is det.
% There is no mode_info_set_how_to_check; this field is read-only.
:- pred mode_info_set_may_change_called_proc(may_change_called_proc::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_changed_flag(bool::in,
    mode_info::in, mode_info::out) is det.
% There is a mode_info_set_checking_extra_goals, but it is not JUST a setter.
:- pred mode_info_set_need_to_requantify(need_to_requantify::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_in_promise_purity_scope(in_promise_purity_scope::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_in_from_ground_term(in_from_ground_term_scope::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_had_from_ground_term(had_from_ground_term_scope::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_make_ground_terms_unique(make_ground_terms_unique::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_in_dupl_for_switch(in_dupl_for_switch::in,
    mode_info::in, mode_info::out) is det.

%---------------------------------------------------------------------------%
%
% Nontrivial getter predicates.
%

:- pred mode_info_get_num_errors(mode_info::in, int::out) is det.
:- pred mode_info_get_liveness(mode_info::in, set_of_progvar::out) is det.

:- pred mode_info_get_pred_id_table(mode_info::in, pred_id_table::out) is det.
:- pred mode_info_get_insts(mode_info::in, inst_table::out) is det.
:- pred mode_info_get_modes(mode_info::in, mode_table::out) is det.

:- pred mode_info_get_types_of_vars(mode_info::in,
    list(prog_var)::in, list(mer_type)::out) is det.

%---------------------------------------------------------------------------%
%
% Nontrivial setter predicates.
%

:- pred mode_info_set_instmap(instmap::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_checking_extra_goals(bool::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_set_call_context(call_context::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_unset_call_context(
    mode_info::in, mode_info::out) is det.
:- pred mode_info_set_call_arg_context(int::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_need_to_requantify(mode_info::in, mode_info::out) is det.

%---------------------------------------------------------------------------%
%
% Predicates related to liveness.
%

    % Check whether a variable or a list of variables are live or not.
    %
:- pred mode_info_var_is_live(mode_info::in, prog_var::in,
    is_live::out) is det.
:- pred mode_info_var_list_is_live(mode_info::in, list(prog_var)::in,
    list(is_live)::out) is det.

    % Check whether a variable is nondet_live or not.
    %
:- pred mode_info_var_is_nondet_live(mode_info::in, prog_var::in,
    is_live::out) is det.

    % We keep track of the live variables and the nondet-live variables
    % as two bags. This allows us to easily add and remove sets of variables.
    % It is probably not maximally efficient.
    %
    % mode_info_add_live_vars adds the given set of vars
    % to the bag of live vars and the bag of nondet-live vars.
    %
    % mode_info_remove_live_vars removes the given set of vars
    % from the bag of live vars and the bag of nondet-live vars.
    %
:- pred mode_info_add_live_vars(set_of_progvar::in,
    mode_info::in, mode_info::out) is det.
:- pred mode_info_remove_live_vars(set_of_progvar::in,
    mode_info::in, mode_info::out) is det.

%---------------------------------------------------------------------------%
%
% Predicates related to locked variables.
%

:- pred mode_info_var_is_locked(mode_info::in, prog_var::in,
    var_lock_reason::out) is semidet.

:- pred mode_info_lock_vars(var_lock_reason::in, set_of_progvar::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_unlock_vars(var_lock_reason::in, set_of_progvar::in,
    mode_info::in, mode_info::out) is det.

%---------------------------------------------------------------------------%
%
% Predicates related to errors and warnings.
%

    % Record a mode error (and associated context info) in the mode_info.
    %
:- pred mode_info_error(set_of_progvar::in, mode_error::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_add_error(mode_error_info::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_warning(mode_warning::in,
    mode_info::in, mode_info::out) is det.

:- pred mode_info_add_warning(mode_warning_info::in,
    mode_info::in, mode_info::out) is det.

    % Find the pf_sym_name_arity to use in error messages
    % for the given pred_id.
    %
:- pred mode_info_get_pf_sym_name_arity(mode_info::in, pred_id::in,
    pf_sym_name_arity::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_util.

:- import_module getopt.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

    % The information we need while we modecheck a procedure.
    %
    % The mode_info and mode_sub_info types constitute a single logical
    % data structure split into two parts for efficiency purposes.
    %
    % The most frequently used fields are in the mode_info type,
    % while all the other fields are in the mode_sub_info type.
    %
    % The sub-word-sized arguments of the mode_sub_info type
    % are all at the end of the structure, to allow them to be packed together.
:- type mode_info
    --->    mode_info(
                % The Boehm collector allocates blocks whose sizes are
                % multiples of 2, so we should keep the number of fields
                % in a mode_info to be a multiple of 2 as well.

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

                % This field is used by the checkpoint code when debug_modes
                % is on. It has the instmap that was current at the last mode
                % checkpoint, so that checkpoints do not print out the insts
                % of variables whose insts have not changed since the last
                % checkpoint. This field will always contain an unreachable
                % instmap if debug_modes is off, since its information is not
                % needed then.
                msi_last_checkpoint_insts   :: instmap,

                % The initial instmap of the procedure body. Used to decide
                % whether a unification that cannot fail could be influenced
                % by an argument mode that enforces a subtype.
                msi_initial_instmap         :: instmap,

                % The inst vars that appear in the procedure head and their
                % constraints.
                msi_head_inst_vars          :: head_inst_vars,

                % The mode warnings found.
                msi_warnings                :: list(mode_warning_info),

                % This field maps variables for which we have generated
                % a mode_error_unify_var_multimode_pred to the rest of the
                % fields in that mode error. If this variable is later found
                % to be insufficiently instantiated, we will want to print
                % the mode_error_unify_var_multimode_pred as a possible
                % explanation of that error.
                %
                % Any code that creates a mode_error_unify_var_multimode_pred
                % should include the information in that error in this map.
                %
                % Since this information is valid only in straight-line
                % conjunctions, entries added to this map must be thrown away
                % when the code that added them is backtracked over.
                % The simplest way to make this happen is to take a snapshot
                % of the value of this map at the start of each branched
                % control structure, and to reset it to that snapshot
                % at the start of each branch.
                msi_pred_var_multimode_error_map
                                            :: pred_var_multimode_error_map,

                % All the arguments from here on are sub-word-sized,
                % which should allow the compiler to pack them together.

                msi_how_to_check            :: how_to_check_goal,

                % Is mode analysis allowed to change which procedure is called?
                msi_may_change_called_proc  :: may_change_called_proc,

                % Changed flag: if `yes', then we may need to repeat
                % mode inference.
                msi_changed_flag            :: bool,

                % Are we rechecking a goal after introducing unifications
                % for complicated sub-unifications or an implied mode?
                % If so, redoing the mode check should not introduce more
                % extra unifications.
                msi_checking_extra_goals    :: bool,

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

%---------------------------------------------------------------------------%

mode_context_init(mode_context_uninitialized).

%---------------------------------------------------------------------------%

mode_info_init(ModuleInfo, PredId, ProcId, Context, LiveVars, HeadInstVars,
        InstMap0, HowToCheck, MayChangeProc, ModeInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_modes, DebugModes),
    globals.lookup_int_option(Globals, debug_modes_pred_id, DebugModesPredId),
    pred_id_to_int(PredId, PredIdInt),
    ( if
        DebugModes = yes,
        ( DebugModesPredId >= 0 => DebugModesPredId = PredIdInt )
    then
        (
            HowToCheck = check_modes,
            UniquePrefix = ""
        ;
            HowToCheck = check_unique_modes,
            UniquePrefix = "unique "
        ),
        globals.lookup_bool_option(Globals, debug_modes_verbose, DebugVerbose),
        globals.lookup_bool_option(Globals, debug_modes_minimal, DebugMinimal),
        globals.lookup_bool_option(Globals, debug_modes_statistics,
            Statistics),
        Flags = debug_flags(UniquePrefix, DebugVerbose, DebugMinimal,
            Statistics),
        MaybeDebug = yes(Flags)
    else
        MaybeDebug = no
    ),

    module_info_proc_info(ModuleInfo, PredId, ProcId, ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),

    bag.from_sorted_list(set_of_var.to_sorted_list(LiveVars), LiveVarsBag),
    instmap.init_unreachable(LastCheckpointInstMap),
    LockedVars = [],
    ParallelVars = [],
    Warnings = [],

    Changed = no,
    CheckingExtraGoals = no,
    NeedToRequantify = do_not_need_to_requantify,
    InPromisePurityScope = not_in_promise_purity_scope,
    InFromGroundTerm = not_in_from_ground_term_scope,
    HadFromGroundTerm = did_not_have_from_ground_term_scope,
    MakeGroundTermsUnique = do_not_make_ground_terms_unique,
    InDuplForSwitch = not_in_dupl_for_switch,
    map.init(PredVarMultiModeMap),

    ModeSubInfo = mode_sub_info(PredId, ProcId, VarSet, VarTypes,
        MaybeDebug, LockedVars, LiveVarsBag, InstVarSet, ParallelVars,
        LastCheckpointInstMap, InstMap0, HeadInstVars, Warnings,
        PredVarMultiModeMap,
        HowToCheck, MayChangeProc, Changed, CheckingExtraGoals,
        NeedToRequantify, InPromisePurityScope, InFromGroundTerm,
        HadFromGroundTerm, MakeGroundTermsUnique, InDuplForSwitch),

    mode_context_init(ModeContext),
    delay_info_init(DelayInfo),
    ErrorList = [],
    bag.from_sorted_list(set_of_var.to_sorted_list(LiveVars),
        NondetLiveVarsBag),

    ModeInfo = mode_info(ModuleInfo, InstMap0, DelayInfo, ErrorList,
        ModeContext, Context, NondetLiveVarsBag, ModeSubInfo).

%---------------------------------------------------------------------------%
%
% Getter and setter predicates for mode_info.
% The setter predicates where the new value of the field is often
% the same as its old value check whether the new value may differ
% from the old one, and allocate a new mode_info structure (and maybe
% a new mode_sub_info structure) only if the old and new values are
% *not* guaranteed to be the same.
%
% I (zs) used profiling data derived from a bootcheck to establish
% which mode_info fields fall into this category. See below for further info.
%

    % Getters of the fields of mode_info.
mode_info_get_module_info(MI, X) :-
    X = MI ^ mi_module_info.
mode_info_get_instmap(MI, X) :-
    X = MI ^ mi_instmap.
mode_info_get_delay_info(MI, X) :-
    X = MI ^ mi_delay_info.
mode_info_get_errors(MI, X) :-
    X = MI ^ mi_errors.
mode_info_get_mode_context(MI, X) :-
    X = MI ^ mi_mode_context.
mode_info_get_context(MI, X) :-
    X = MI ^ mi_context.
mode_info_get_nondet_live_vars(MI, X) :-
    X = MI ^ mi_nondet_live_vars.

    % Getters of the fields of mode_sub_info.
mode_info_get_pred_id(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_pred_id.
mode_info_get_proc_id(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_proc_id.
mode_info_get_varset(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_varset.
mode_info_get_var_types(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_vartypes.
mode_info_get_debug_modes(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_debug.
mode_info_get_locked_vars(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_locked_vars.
mode_info_get_live_vars(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_live_vars.
mode_info_get_instvarset(ModeInfo, X) :-
    X = ModeInfo ^ mi_sub_info ^ msi_instvarset.
mode_info_get_parallel_vars(MI, X) :-
    X = MI ^  mi_sub_info ^ msi_par_conj.
mode_info_get_last_checkpoint_insts(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_last_checkpoint_insts.
mode_info_get_initial_instmap(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_initial_instmap.
mode_info_get_head_inst_vars(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_head_inst_vars.
mode_info_get_warnings(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_warnings.
mode_info_get_pred_var_multimode_error_map(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_pred_var_multimode_error_map.
mode_info_get_how_to_check(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_how_to_check.
mode_info_get_may_change_called_proc(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_may_change_called_proc.
mode_info_get_changed_flag(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_changed_flag.
mode_info_get_checking_extra_goals(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_checking_extra_goals.
mode_info_get_need_to_requantify(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_need_to_requantify.
mode_info_get_in_promise_purity_scope(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_in_promise_purity_scope.
mode_info_get_in_from_ground_term(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_in_from_ground_term.
mode_info_get_had_from_ground_term(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_had_from_ground_term.
mode_info_get_make_ground_terms_unique(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_make_ground_terms_unique.
mode_info_get_in_dupl_for_switch(MI, X) :-
    X = MI ^ mi_sub_info ^ msi_in_dupl_for_switch.

%---------------------%

    % Setters of the fields of mode_info.
mode_info_set_module_info(X, !MI) :-
    ( if private_builtin.pointer_equal(X, !.MI ^ mi_module_info) then
        true
    else
        !MI ^ mi_module_info := X
    ).
mode_info_set_delay_info(X, !MI) :-
    ( if private_builtin.pointer_equal(X, !.MI ^ mi_delay_info) then
        true
    else
        !MI ^ mi_delay_info := X
    ).
mode_info_set_errors(X, !MI) :-
    ( if private_builtin.pointer_equal(X, !.MI ^ mi_errors) then
        true
    else
        !MI ^ mi_errors := X
    ).
mode_info_set_mode_context(X, !MI) :-
    !MI ^ mi_mode_context := X.
mode_info_set_context(X, !MI) :-
    ( if private_builtin.pointer_equal(X, !.MI ^ mi_context) then
        true
    else
        !MI ^ mi_context := X
    ).
mode_info_set_nondet_live_vars(X, !MI) :-
    ( if private_builtin.pointer_equal(X, !.MI ^ mi_nondet_live_vars) then
        true
    else
        !MI ^ mi_nondet_live_vars := X
    ).

    % Setters of the fields of mode_sub_info.
mode_info_set_pred_id(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_pred_id := X.
mode_info_set_proc_id(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_proc_id := X.
mode_info_set_varset(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_varset := X.
mode_info_set_var_types(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_vartypes := X.
mode_info_set_locked_vars(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_locked_vars := X.
mode_info_set_live_vars(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_live_vars := X.
mode_info_set_instvarset(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_instvarset := X.
mode_info_set_parallel_vars(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_par_conj := X.
mode_info_set_last_checkpoint_insts(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_last_checkpoint_insts := X.
mode_info_set_warnings(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_warnings := X.
mode_info_set_pred_var_multimode_error_map(X, !MI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.MI ^ mi_sub_info ^ msi_pred_var_multimode_error_map)
    then
        true
    else
        !MI ^ mi_sub_info ^ msi_pred_var_multimode_error_map := X
    ).
mode_info_set_may_change_called_proc(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_may_change_called_proc := X.
mode_info_set_changed_flag(X, !MI) :-
    ( if
        private_builtin.pointer_equal(X, !.MI ^ mi_sub_info ^ msi_changed_flag)
    then
        true
    else
        !MI ^ mi_sub_info ^ msi_changed_flag := X
    ).
mode_info_set_need_to_requantify(X, !MI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.MI ^ mi_sub_info ^ msi_need_to_requantify)
    then
        true
    else
        !MI ^ mi_sub_info ^ msi_need_to_requantify := X
    ).
mode_info_set_in_promise_purity_scope(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_in_promise_purity_scope := X.
mode_info_set_in_from_ground_term(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_in_from_ground_term := X.
mode_info_set_had_from_ground_term(X, !MI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.MI ^ mi_sub_info ^ msi_had_from_ground_term)
    then
        true
    else
        !MI ^ mi_sub_info ^ msi_had_from_ground_term := X
    ).
mode_info_set_make_ground_terms_unique(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_make_ground_terms_unique := X.
mode_info_set_in_dupl_for_switch(X, !MI) :-
    !MI ^ mi_sub_info ^ msi_in_dupl_for_switch := X.

%---------------------------------------------------------------------------%

% I (zs) decided which mode_info fields to call pointer_equal for
% based on this profiling information derived from a bootcheck. During this
% bootcheck, each of the setter predicates was given a number from 0 to 25,
% and called gather_mode_info_stats below to record whether the new value
% was the same bit pattern as the old one.
%
% The profiling code follows the data derived from it.

%          same      diff    same%
%  0:  22869923    651583  97.230% module_info
%  1:         0         1   0.000% pred_id
%  2:         1         0 100.000% proc_id
%  3:         4      8815   0.045% varset
%  4:         4      8815   0.045% vartypes
%  5:   1824389   3523646  34.113% context
%  6:   1314611  32769008   3.857% mode_context
%  7:         0    438852   0.000% locked_vars
%  8:         0   1283911   0.000% instvarset
%  9:   3234680   1901125  62.983% errors
% 10:         0       892   0.000% warnings
% 11:     57326     19964  74.170% need_to_requantify
% 12:         0       868   0.000% promise_purity_scope
% 13:  12223802   2022001  85.806% delay_info
% 14:     58338   9161895   0.633% live_vars
% 15:   1707132  12480878  12.032% nondet_live_vars
% 16:         0         0          last_checkpoint
% 17:         0         0          parallel vars
% 18:    604507       562  99.907% changed_flag
% 19:         0         0          how_to_check
% 20:         0     16620   0.000% may_change_called_proc
% 21:   1156755    684924  62.810% may_init_solver_vars
% 22:         0         0          in_from_ground_term
% 23:      2355      1775  57.022% had_from_ground_term
% 24:         0         1   0.000% make_ground_term_unique
% 25:       212     16930   1.237% in_dupl_for_switch

% :- import_module io.
% :- pred write_mode_info_stats(io::di, io::uo) is det.
%
% :- pragma foreign_decl("C", local,
% "
% #define MR_NUM_MODE_INFO_STATS    26
% unsigned long MR_stats_same[MR_NUM_MODE_INFO_STATS];
% unsigned long MR_stats_diff[MR_NUM_MODE_INFO_STATS];
% ").
%
% :- pred gather_mode_info_stats(int::in, T::in, T::in,
%     mode_info::in, mode_info::out) is det.
%
% :- pragma foreign_proc("C",
%     gather_mode_info_stats(N::in, Old::in, New::in, MI0::in, MI::out),
%     [will_not_call_mercury, promise_pure],
% "
%     if (((MR_Unsigned) Old) == ((MR_Unsigned) New)) {
%         ++MR_stats_same[N];
%     } else {
%         ++MR_stats_diff[N];
%     }
%
%     MI = MI0;
% ").
%
% :- pragma foreign_proc("C",
%     write_mode_info_stats(IO0::di, IO::uo),
%     [will_not_call_mercury, promise_pure],
% "
%     FILE *fp;
%
%     fp = fopen(""/tmp/MODE_INFO_STATS"", ""a"");
%     if (fp != NULL) {
%         int i;
%         for (i = 0; i < MR_NUM_MODE_INFO_STATS; i++) {
%             fprintf(fp, ""%d: %lu %lu\\n"",
%                 i, MR_stats_same[i], MR_stats_diff[i]);
%         }
%     }
%
%     IO = IO0;
% ").

%---------------------------------------------------------------------------%

mode_info_get_num_errors(ModeInfo, NumErrors) :-
    mode_info_get_errors(ModeInfo, Errors),
    list.length(Errors, NumErrors).

mode_info_get_liveness(ModeInfo, LiveVars) :-
    mode_info_get_live_vars(ModeInfo, LiveVarsBag),
    bag.to_list_without_duplicates(LiveVarsBag, SortedList),
    set_of_var.sorted_list_to_set(SortedList, LiveVars).

mode_info_get_pred_id_table(ModeInfo, PredIdTable) :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_pred_id_table(ModuleInfo, PredIdTable).

mode_info_get_insts(ModeInfo, Insts) :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_inst_table(ModuleInfo, Insts).

mode_info_get_modes(ModeInfo, Modes) :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_mode_table(ModuleInfo, Modes).

mode_info_get_types_of_vars(ModeInfo, Vars, TypesOfVars) :-
    mode_info_get_var_types(ModeInfo, VarTypes),
    lookup_var_types(VarTypes, Vars, TypesOfVars).

%---------------------------------------------------------------------------%

mode_info_set_instmap(InstMap, !MI) :-
    mode_info_get_instmap(!.MI, InstMap0),
    !MI ^ mi_instmap := InstMap,
    ( if
        instmap_is_unreachable(InstMap),
        instmap_is_reachable(InstMap0)
    then
        mode_info_get_delay_info(!.MI, DelayInfo0),
        delay_info_bind_all_vars(DelayInfo0, DelayInfo),
        mode_info_set_delay_info(DelayInfo, !MI)
    else
        true
    ).

mode_info_set_checking_extra_goals(Checking, !MI) :-
    mode_info_get_checking_extra_goals(!.MI, Checking0),
    ( if
        Checking0 = yes,
        Checking = yes
    then
        % This should never happen - once the extra goals are introduced,
        % rechecking the goal should not introduce more extra goals.
        unexpected($pred, "rechecking extra goals adds more extra goals")
    else
        !MI ^ mi_sub_info ^ msi_checking_extra_goals := Checking
    ).

mode_info_set_call_context(CallContext, !MI) :-
    (
        CallContext = call_context_unify(UnifyContext),
        mode_info_set_mode_context(mode_context_unify(UnifyContext, left), !MI)
    ;
        CallContext = call_context_call(CallId),
        mode_info_set_mode_context(mode_context_call(CallId, 0), !MI)
    ).

mode_info_unset_call_context(!MI) :-
    mode_info_set_mode_context(mode_context_uninitialized, !MI).

mode_info_set_call_arg_context(ArgNum, !ModeInfo) :-
    mode_info_get_mode_context(!.ModeInfo, ModeContext0),
    (
        ModeContext0 = mode_context_call(CallId, _),
        mode_info_set_mode_context(mode_context_call(CallId, ArgNum),
            !ModeInfo)
    ;
        ModeContext0 = mode_context_unify(_UnifyContext, _Side)
        % This only happens when checking that the typeinfo variables
        % for polymorphic complicated unifications are ground.
        % For that case, we don't care about the ArgNum.
    ;
        ModeContext0 = mode_context_uninitialized,
        unexpected($pred, "uninitialized")
    ).

mode_info_need_to_requantify(!ModeInfo) :-
    mode_info_set_need_to_requantify(need_to_requantify, !ModeInfo).

%---------------------------------------------------------------------------%

mode_info_var_is_live(ModeInfo, Var, Result) :-
    mode_info_get_live_vars(ModeInfo, LiveVars0),
    ( if bag.contains(LiveVars0, Var) then
        Result = is_live
    else
        Result = is_dead
    ).

mode_info_var_list_is_live(_, [], []).
mode_info_var_list_is_live(ModeInfo, [Var | Vars], [Live | Lives]) :-
    mode_info_var_is_live(ModeInfo, Var, Live),
    mode_info_var_list_is_live(ModeInfo, Vars, Lives).

mode_info_var_is_nondet_live(ModeInfo, Var, Result) :-
    mode_info_get_nondet_live_vars(ModeInfo, NondetLiveVars0),
    ( if bag.contains(NondetLiveVars0, Var) then
        Result = is_live
    else
        Result = is_dead
    ).

mode_info_add_live_vars(NewLiveVars, !MI) :-
    set_of_var.to_sorted_list(NewLiveVars, NewLiveVarsList),
    mode_info_get_live_vars(!.MI, LiveVars0),
    mode_info_get_nondet_live_vars(!.MI, NondetLiveVars0),
    bag.insert_list(NewLiveVarsList, LiveVars0, LiveVars),
    bag.insert_list(NewLiveVarsList, NondetLiveVars0, NondetLiveVars),
    mode_info_set_live_vars(LiveVars, !MI),
    mode_info_set_nondet_live_vars(NondetLiveVars, !MI).

mode_info_remove_live_vars(OldLiveVars, !MI) :-
    set_of_var.to_sorted_list(OldLiveVars, OldLiveVarsList),
    mode_info_get_live_vars(!.MI, LiveVars0),
    mode_info_get_nondet_live_vars(!.MI, NondetLiveVars0),
    bag.det_remove_list(OldLiveVarsList, LiveVars0, LiveVars),
    bag.det_remove_list(OldLiveVarsList, NondetLiveVars0, NondetLiveVars),
    mode_info_set_live_vars(LiveVars, !MI),
    mode_info_set_nondet_live_vars(NondetLiveVars, !MI),

    % When a variable becomes dead, we may be able to wake up a goal
    % which is waiting on that variable.
    mode_info_get_delay_info(!.MI, DelayInfo0),
    delay_info_bind_var_list(OldLiveVarsList, DelayInfo0, DelayInfo),
    mode_info_set_delay_info(DelayInfo, !MI).

%---------------------------------------------------------------------------%

    % The locked variables are stored as a stack of sets of variables.
    % A variable is locked if it is a member of any of the sets.
    % To lock a set of vars, we just push them on the stack,
    % and to unlock a set of vars, we just pop them off the stack.
    % The stack is implemented as a list.

mode_info_var_is_locked(ModeInfo, Var, Reason) :-
    mode_info_get_locked_vars(ModeInfo, LockedVarsList),
    mode_info_var_is_locked_loop(LockedVarsList, Var, Reason).

:- pred mode_info_var_is_locked_loop(locked_vars::in, prog_var::in,
    var_lock_reason::out) is semidet.

mode_info_var_is_locked_loop([ThisReason - Set | Sets], Var, Reason) :-
    ( if set_of_var.member(Set, Var) then
        Reason = ThisReason
    else
        mode_info_var_is_locked_loop(Sets, Var, Reason)
    ).

mode_info_lock_vars(Reason, Vars, !ModeInfo) :-
    mode_info_get_locked_vars(!.ModeInfo, LockedVars),
    mode_info_set_locked_vars([Reason - Vars | LockedVars], !ModeInfo).

mode_info_unlock_vars(Reason, Vars, !ModeInfo) :-
    mode_info_get_locked_vars(!.ModeInfo, LockedVars0),
    ( if
        LockedVars0 = [Reason - TheseVars | LockedVars1],
        set_of_var.equal(TheseVars, Vars)
    then
        LockedVars = LockedVars1
    else
        unexpected($pred, "some kind of nesting error")
    ),
    mode_info_set_locked_vars(LockedVars, !ModeInfo).

%---------------------------------------------------------------------------%

mode_info_error(Vars, ModeError, !ModeInfo) :-
    mode_info_get_context(!.ModeInfo, Context),
    mode_info_get_mode_context(!.ModeInfo, ModeContext),
    ModeErrorInfo = mode_error_info(Vars, ModeError, Context, ModeContext),
    mode_info_add_error(ModeErrorInfo, !ModeInfo).

mode_info_add_error(ModeErrorInfo, !ModeInfo) :-
    mode_info_get_errors(!.ModeInfo, Errors0),
    list.append(Errors0, [ModeErrorInfo], Errors),
    mode_info_set_errors(Errors, !ModeInfo),
    mode_info_get_debug_modes(!.ModeInfo, DebugModes),
    (
        DebugModes = no
    ;
        DebugModes = yes(_DebugFlags),
        trace [io(!IO)] (
            mode_info_get_module_info(!.ModeInfo, ModuleInfo),
            module_info_get_globals(ModuleInfo, Globals0),
            module_info_get_name(ModuleInfo, ModuleName),
            get_debug_output_stream(Globals0, ModuleName, DebugStream, !IO),
            list.length(Errors, ErrorNum),
            io.format(DebugStream, "Adding error_spec %d\n",
                [i(ErrorNum)], !IO),
            Spec = mode_error_info_to_spec(!.ModeInfo, ModeErrorInfo),
            globals.set_option(print_error_spec_id, bool(yes),
                Globals0, Globals),
            write_error_spec(DebugStream, Globals, Spec, !IO),
            io.flush_output(DebugStream, !IO)
        )
    ).

mode_info_warning(ModeWarning, !ModeInfo) :-
    mode_info_get_context(!.ModeInfo, Context),
    mode_info_get_mode_context(!.ModeInfo, ModeContext),
    ModeWarningInfo = mode_warning_info(ModeWarning, Context, ModeContext),
    mode_info_add_warning(ModeWarningInfo, !ModeInfo).

mode_info_add_warning(ModeWarningInfo, !ModeInfo) :-
    mode_info_get_warnings(!.ModeInfo, Warnings0),
    list.append(Warnings0, [ModeWarningInfo], Warnings),
    mode_info_set_warnings(Warnings, !ModeInfo).

mode_info_get_pf_sym_name_arity(ModeInfo, PredId, CallId) :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_pf_sym_name_arity(PredInfo, CallId).

%---------------------------------------------------------------------------%
:- end_module check_hlds.mode_info.
%---------------------------------------------------------------------------%
