%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: simplify_info.m.
%
% This module defines the simplify_info type and its access predicates.
% This contains information for use by the other submodules of simplify.m:
% both static information (such as identify of the procedure whose body is
% being simplified), and information specific to the current point in the
% simplification process (such as the current instmap).
%
%---------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_info.
:- interface.

:- import_module check_hlds.simplify.simplify_tasks.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.trace_params.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

:- type innermost_proc
    --->    imp_whole_proc
            % The innermost proc is the whole original procedure.
    ;       imp_lambda(prog_context).
            % The innermost proc is a lambda expression
            % at the given source location, which a later compiler pass
            % will transform into a separate procedure.

:- type simplify_nested_context
    --->    simplify_nested_context(
                % Are we currently inside a goal that was duplicated
                % for a switch?
                snc_inside_dupl_for_switch  :: bool,

                % Count of the number of lambdas which enclose
                % the current goal.
                snc_num_enclosing_lambdas   :: int,

                % Can the procedure we are inside be invoked from an
                % all-solutions predicate?
                % If we are inside one or more lambda expressions,
                % the relevant procedure is the innermost lambda,
                % which will later be transformed into a separate procedure.
                % If yes, identify the procedure.
                snc_proc_is_model_non       :: maybe(innermost_proc)
            ).

:- type maybe_allow_messages
    --->    do_not_allow_messages
    ;       allow_messages.

%---------------------------------------------------------------------------%

:- type simplify_info.

    % Initialise the simplify_info.
    %
:- pred simplify_info_init(module_info::in, pred_id::in, proc_id::in,
    proc_info::in, simplify_tasks::in, simplify_info::out) is det.

    % Reinitialise the simplify_info before reprocessing a goal.
    %
:- pred simplify_info_reinit(simplify_tasks::in,
    simplify_info::in, simplify_info::out) is det.

%---------------------------------------------------------------------------%
%
% Operations on simplify_infos.
%

:- pred simplify_info_get_pred_proc_info(simplify_info::in, pred_info::out,
    proc_info::out) is det.

:- pred simplify_info_add_elim_vars(list(prog_var)::in,
    simplify_info::in, simplify_info::out) is det.

:- pred simplify_info_add_message(error_spec::in,
    simplify_info::in, simplify_info::out) is det.

:- pred simplify_info_incr_cost_delta(int::in,
    simplify_info::in, simplify_info::out) is det.

:- pred simplify_info_apply_substitutions_and_duplicate(prog_var::in,
    prog_var::in, tsubst::in, simplify_info::in, simplify_info::out) is det.

%---------------------------------------------------------------------------%
%
% Getters and setters of fields of simplify_info.
%

:- pred simplify_info_get_simplify_tasks(simplify_info::in,
    simplify_tasks::out) is det.
:- pred simplify_info_get_module_info(simplify_info::in, module_info::out)
    is det.
:- pred simplify_info_get_var_types(simplify_info::in, vartypes::out) is det.
:- pred simplify_info_get_varset(simplify_info::in, prog_varset::out) is det.
:- pred simplify_info_get_should_requantify(simplify_info::in, bool::out)
    is det.
:- pred simplify_info_get_should_rerun_det(simplify_info::in, bool::out)
    is det.

:- pred simplify_info_get_pred_proc_id(simplify_info::in,
    pred_proc_id::out) is det.
:- pred simplify_info_get_inst_varset(simplify_info::in,
    inst_varset::out) is det.
:- pred simplify_info_get_fully_strict(simplify_info::in, bool::out) is det.
:- pred simplify_info_get_trace_level_optimized(simplify_info::in,
    trace_level::out, bool::out) is det.

:- pred simplify_info_get_rtti_varmaps(simplify_info::in, rtti_varmaps::out)
    is det.
:- pred simplify_info_get_elim_vars(simplify_info::in,
    list(list(prog_var))::out) is det.
:- pred simplify_info_get_allow_messages(simplify_info::in,
    maybe_allow_messages::out) is det.
:- pred simplify_info_get_error_specs(simplify_info::in, list(error_spec)::out)
    is det.
:- pred simplify_info_get_cost_delta(simplify_info::in, int::out) is det.
:- pred simplify_info_get_has_parallel_conj(simplify_info::in,
    has_parallel_conj::out) is det.
:- pred simplify_info_get_found_contains_trace(simplify_info::in, bool::out)
    is det.
:- pred simplify_info_get_has_user_event(simplify_info::in,
    has_user_event::out) is det.
:- pred simplify_info_get_deleted_call_callees(simplify_info::in,
    set(pred_proc_id)::out) is det.

:- pred simplify_info_set_simplify_tasks(simplify_tasks::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_module_info(module_info::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_var_types(vartypes::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_varset(prog_varset::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_should_requantify(
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_should_rerun_det(
    simplify_info::in, simplify_info::out) is det.

:- pred simplify_info_set_rtti_varmaps(rtti_varmaps::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_elim_vars(list(list(prog_var))::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_allow_messages(maybe_allow_messages::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_error_specs(list(error_spec)::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_cost_delta(int::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_has_parallel_conj(has_parallel_conj::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_found_contains_trace(bool::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_has_user_event(has_user_event::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_deleted_call_callees(set(pred_proc_id)::in,
    simplify_info::in, simplify_info::out) is det.

%---------------------------------------------------------------------------%

:- pred simplify_do_warn_simple_code(simplify_info::in) is semidet.
:- pred simplify_do_warn_duplicate_calls(simplify_info::in) is semidet.
:- pred simplify_do_warn_implicit_stream_calls(simplify_info::in) is semidet.
:- pred simplify_do_format_calls(simplify_info::in) is semidet.
:- pred simplify_do_warn_obsolete(simplify_info::in) is semidet.
:- pred simplify_do_mark_code_model_changes(simplify_info::in) is semidet.
:- pred simplify_do_after_front_end(simplify_info::in) is semidet.
:- pred simplify_do_excess_assign(simplify_info::in) is semidet.
:- pred simplify_do_test_after_switch(simplify_info::in) is semidet.
:- pred simplify_do_elim_removable_scopes(simplify_info::in) is semidet.
:- pred simplify_do_opt_duplicate_calls(simplify_info::in) is semidet.
:- pred simplify_do_const_prop(simplify_info::in) is semidet.
:- pred simplify_do_common_struct(simplify_info::in) is semidet.
:- pred simplify_do_extra_common_struct(simplify_info::in) is semidet.
:- pred simplify_do_ignore_par_conjunctions(simplify_info::in) is semidet.

    % Succeed if either warn_duplicate_calls or opt_duplicate_calls is set,
    % and return whether opt_duplicate_calls is set.
    %
:- pred simplify_do_warn_or_opt_duplicate_calls(simplify_info::in, bool::out)
    is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.globals.
:- import_module libs.options.

:- import_module int.
:- import_module map.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type simplify_info
    --->    simplify_info(
                % The most frequently used writeable fields, held at eight
                % fields (adding even just one more field would cause Boehm
                % to allocate a 16 word cell).
                %
                % The other writeable fields are in the simplify_sub_info,
                % while all the readonly fields are in the simp_params.

                % The tasks we do in this invocation of simplification.
/* 1 */         simp_simplify_tasks         :: simplify_tasks,

                % The whole module. Those parts of it that are also contained
                % in other fields of simplify_info and simplify_sub_info
                % may be out of date in the module_info.
/* 2 */         simp_module_info            :: module_info,

                % The variables of the procedure being simplified.
/* 3 */         simp_varset                 :: prog_varset,
/* 4 */         simp_vartypes               :: vartypes,

                % Does the goal need requantification?
/* 5 */         simp_should_requantify      :: bool,

                % Does determinism analysis need to be rerun?
/* 6 */         simp_should_rerun_det       :: bool,

/* 7 */         simp_params                 :: simplify_info_params,
/* 8 */         simp_sub_info               :: simplify_sub_info
            ).

:- type simplify_info_params
    --->    simplify_info_params(
                % The id of the procedure we are simplifying, and the one
                % field of its proc_info that we need but never change.
                sip_pred_proc_id            :: pred_proc_id,
                sip_inst_varset             :: inst_varset,

                % The value of the --fully-strict option.
                sip_fully_strict            :: bool,

                sip_trace_level             :: trace_level,

                % The value of the --trace-optimized option.
                sip_trace_optimized         :: bool
            ).

:- type simplify_sub_info
    --->    simplify_sub_info(
                % Information about the typeinfo and typeclass info vars
                % for the type variables of the procedure being simplified.
                % Logically, this field belongs next to simp_varset and
                % simp_vartypes, but it is not used frequently enough
                % to store at the top level of simplify_info.
                ssimp_rtti_varmaps          :: rtti_varmaps,

                % The variables we have eliminated. Each list of vars consists
                % of a list of consecutive variable numbers in ascending order.
                % The relative order of the lists is unknown.
                ssimp_elim_vars             :: list(list(prog_var)),

                % The default value of this field is allow_messages,
                % but it is set to do_not_allow_messages during the second pass
                % of simplification. See the comment in simplify_proc.m
                % next to the call to simplify_info_set_allow_messages
                % for the reason why.
                ssimp_allow_messages        :: maybe_allow_messages,

                ssimp_error_specs           :: list(error_spec),

                % Measure of the improvement in the goal from simplification.
                ssimp_cost_delta            :: int,

                % Have we seen a parallel conjunction?
                ssimp_has_parallel_conj     :: has_parallel_conj,

                % Have we seen a goal with a feature that says it contains
                % a trace goal?
                ssimp_found_contains_trace  :: bool,

                % Have we seen an event call?
                ssimp_has_user_event        :: has_user_event,

                % The set of predicates that we deleted calls to while
                % simplifying the procedure body.
                ssimp_deleted_call_callees  :: set(pred_proc_id)
            ).

simplify_info_init(ModuleInfo, PredId, ProcId, ProcInfo, SimplifyTasks,
        Info) :-
    PredProcId = proc(PredId, ProcId),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, fully_strict, FullyStrict),
    globals.get_trace_level(Globals, TraceLevel),
    globals.lookup_bool_option(Globals, trace_optimized, TraceOptimized),

    Params = simplify_info_params(PredProcId, InstVarSet, FullyStrict,
        TraceLevel, TraceOptimized),

    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    ElimVars = [],
    AllowMsgs = allow_messages,
    Specs = [],
    CostDelta = 0,
    HasParallelConj = has_no_parallel_conj,
    HasUserEvent = has_no_user_event,
    set.init(TraceGoalProcs),

    SubInfo = simplify_sub_info(RttiVarMaps, ElimVars, AllowMsgs, Specs,
        CostDelta, HasParallelConj, no, HasUserEvent, TraceGoalProcs),

    % SimplifyTasks
    % ModuleInfo
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    ShouldRequantity = no,
    ShouldRerunDet = no,

    Info = simplify_info(SimplifyTasks, ModuleInfo, VarSet, VarTypes,
        ShouldRequantity, ShouldRerunDet, Params, SubInfo).

simplify_info_reinit(SimplifyTasks, !Info) :-
    !Info ^ simp_simplify_tasks := SimplifyTasks,
    !Info ^ simp_should_requantify := no,
    !Info ^ simp_should_rerun_det := no,
    !Info ^ simp_sub_info ^ ssimp_has_parallel_conj := has_no_parallel_conj,
    !Info ^ simp_sub_info ^ ssimp_has_user_event := has_no_user_event.

%---------------------------------------------------------------------------%

simplify_info_get_pred_proc_info(Info, PredInfo, ProcInfo) :-
    simplify_info_get_module_info(Info, ModuleInfo),
    simplify_info_get_pred_proc_id(Info, PredProcId),
    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo).

simplify_info_add_elim_vars(ElimVars, !Info) :-
    simplify_info_get_elim_vars(!.Info, ElimVarsLists0),
    ElimVarsLists = [ElimVars | ElimVarsLists0],
    simplify_info_set_elim_vars(ElimVarsLists, !Info).

simplify_info_add_message(Spec, !Info) :-
    simplify_info_get_allow_messages(!.Info, AllowMsgs),
    (
        AllowMsgs = do_not_allow_messages
    ;
        AllowMsgs = allow_messages,
        simplify_info_get_error_specs(!.Info, Specs0),
        Specs = [Spec | Specs0],
        simplify_info_set_error_specs(Specs, !Info)
    ).

simplify_info_incr_cost_delta(Incr, !Info) :-
    simplify_info_get_cost_delta(!.Info, CostDelta0),
    CostDelta = CostDelta0 + Incr,
    simplify_info_set_cost_delta(CostDelta, !Info).

simplify_info_apply_substitutions_and_duplicate(ToVar, FromVar, TSubst,
        !Info) :-
    simplify_info_get_var_types(!.Info, VarTypes0),
    simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    apply_rec_subst_to_vartypes(TSubst, VarTypes0, VarTypes),
    Renaming = map.singleton(ToVar, FromVar),
    apply_substitutions_to_rtti_varmaps(map.init, TSubst, Renaming,
        RttiVarMaps0, RttiVarMaps1),
    rtti_var_info_duplicate(FromVar, ToVar, RttiVarMaps1, RttiVarMaps),
    simplify_info_set_var_types(VarTypes, !Info),
    simplify_info_set_rtti_varmaps(RttiVarMaps, !Info).

%---------------------------------------------------------------------------%

simplify_info_get_simplify_tasks(Info, X) :-
    X = Info ^ simp_simplify_tasks.
simplify_info_get_module_info(Info, X) :-
    X = Info ^ simp_module_info.
simplify_info_get_varset(Info, X) :-
    X = Info ^ simp_varset.
simplify_info_get_var_types(Info, X) :-
    X = Info ^ simp_vartypes.
simplify_info_get_should_requantify(Info, X) :-
    X = Info ^ simp_should_requantify.
simplify_info_get_should_rerun_det(Info, X) :-
    X = Info ^ simp_should_rerun_det.

simplify_info_get_pred_proc_id(Info, X) :-
    X = Info ^ simp_params ^ sip_pred_proc_id.
simplify_info_get_inst_varset(Info, X) :-
    X = Info ^ simp_params ^ sip_inst_varset.
simplify_info_get_fully_strict(Info, X) :-
    X = Info ^ simp_params ^ sip_fully_strict.
simplify_info_get_trace_level_optimized(Info, X, Y) :-
    X = Info ^ simp_params ^ sip_trace_level,
    Y = Info ^ simp_params ^ sip_trace_optimized.

simplify_info_get_rtti_varmaps(Info, X) :-
    X = Info ^ simp_sub_info ^ ssimp_rtti_varmaps.
simplify_info_get_elim_vars(Info, X) :-
    X = Info ^ simp_sub_info ^ ssimp_elim_vars.
simplify_info_get_allow_messages(Info, X) :-
    X = Info ^ simp_sub_info ^ ssimp_allow_messages.
simplify_info_get_error_specs(Info, X) :-
    X = Info ^ simp_sub_info ^ ssimp_error_specs.
simplify_info_get_cost_delta(Info, X) :-
    X = Info ^ simp_sub_info ^ ssimp_cost_delta.
simplify_info_get_has_parallel_conj(Info, X) :-
    X = Info ^ simp_sub_info ^ ssimp_has_parallel_conj.
simplify_info_get_found_contains_trace(Info, X) :-
    X = Info ^ simp_sub_info ^ ssimp_found_contains_trace.
simplify_info_get_has_user_event(Info, X) :-
    X = Info ^ simp_sub_info ^ ssimp_has_user_event.
simplify_info_get_deleted_call_callees(Info, X) :-
    X = Info ^ simp_sub_info ^ ssimp_deleted_call_callees.

%---------------------%

simplify_info_set_simplify_tasks(X, !Info) :-
    !Info ^ simp_simplify_tasks := X.
simplify_info_set_module_info(X, !Info) :-
    ( if private_builtin.pointer_equal(X, !.Info ^ simp_module_info) then
        true
    else
        !Info ^ simp_module_info := X
    ).
simplify_info_set_varset(X, !Info) :-
    ( if private_builtin.pointer_equal(X, !.Info ^ simp_varset) then
        true
    else
        !Info ^ simp_varset := X
    ).
simplify_info_set_var_types(X, !Info) :-
    ( if private_builtin.pointer_equal(X, !.Info ^ simp_vartypes) then
        true
    else
        !Info ^ simp_vartypes := X
    ).
simplify_info_set_should_requantify(!Info) :-
    X = yes,
    ( if X = !.Info ^ simp_should_requantify then
        true
    else
        !Info ^ simp_should_requantify := X
    ).
simplify_info_set_should_rerun_det(!Info) :-
    X = yes,
    ( if X = !.Info ^ simp_should_rerun_det then
        true
    else
        !Info ^ simp_should_rerun_det := X
    ).

simplify_info_set_rtti_varmaps(X, !Info) :-
    ( if
        private_builtin.pointer_equal(X,
            !.Info ^ simp_sub_info ^ ssimp_rtti_varmaps)
    then
        true
    else
        !Info ^ simp_sub_info ^ ssimp_rtti_varmaps := X
    ).
simplify_info_set_elim_vars(X, !Info) :-
    !Info ^ simp_sub_info ^ ssimp_elim_vars := X.
simplify_info_set_allow_messages(X, !Info) :-
    !Info ^ simp_sub_info ^ ssimp_allow_messages := X.
simplify_info_set_error_specs(X, !Info) :-
    !Info ^ simp_sub_info ^ ssimp_error_specs := X.
simplify_info_set_cost_delta(X, !Info) :-
    ( if X = !.Info ^ simp_sub_info ^ ssimp_cost_delta then
        true
    else
        !Info ^ simp_sub_info ^ ssimp_cost_delta := X
    ).
simplify_info_set_has_parallel_conj(X, !Info) :-
    ( if X = !.Info ^ simp_sub_info ^ ssimp_has_parallel_conj then
        true
    else
        !Info ^ simp_sub_info ^ ssimp_has_parallel_conj := X
    ).
simplify_info_set_found_contains_trace(X, !Info) :-
    ( if X = !.Info ^ simp_sub_info ^ ssimp_found_contains_trace then
        true
    else
        !Info ^ simp_sub_info ^ ssimp_found_contains_trace := X
    ).
simplify_info_set_has_user_event(X, !Info) :-
    ( if X = !.Info ^ simp_sub_info ^ ssimp_has_user_event then
        true
    else
        !Info ^ simp_sub_info ^ ssimp_has_user_event := X
    ).
simplify_info_set_deleted_call_callees(X, !Info) :-
    ( if
        private_builtin.pointer_equal(X,
            !.Info ^ simp_sub_info ^ ssimp_deleted_call_callees)
    then
        true
    else
        !Info ^ simp_sub_info ^ ssimp_deleted_call_callees := X
    ).

% Access stats for the det_info structure, derived on 2017 march 8
% using the commented-out code below:
%
%  i      read      same      diff   same%
%  0  54320730         0    428653   0.00%  simplify_tasks
%  1  22537405  10190586     39961  99.61%  module_info
%  2   1164463     29082     87284  24.99%  varset
%  3  11655215    103357     14630  87.60%  vartypes
%  4    593952     34444     81741  29.65%  rtti_varmaps
%  5    144530         0         0          fully_strict
%  6   1268871         0         0          pred_id, proc_id
%  7     25167         0         0          inst_varset
%  8    431612         0      4700   0.00%  elim_vars
%  9      8132         0    428653   0.00%  allow_messages
% 10    431084         0      4172   0.00%  error_specs
% 11    858622     41608     25167  62.31%  should_requantify
% 12    858622       531      1606  24.85%  should_rerun_det
% 13    148274     83477     61740  57.48%  cost_delta
% 14    426912        52       170  23.42%  has_parallel_conj
% 15    429969      8066       887  90.09%  found_contains_trace
% 16    426912        16        36  30.77%  has_user_event
% 17    484085     56008      1165  97.96%  deleted_call_callees
%
% :- pragma foreign_decl("C", local,
% "
% #define MR_NUM_INFO_STATS    18
% unsigned long MR_stats_read[MR_NUM_INFO_STATS];
% unsigned long MR_stats_same[MR_NUM_INFO_STATS];
% unsigned long MR_stats_diff[MR_NUM_INFO_STATS];
% ").
% 
% :- pred gather_info_read_stats(int::in,
%     T::in, T::out) is det.
% 
% :- pragma foreign_proc("C",
%     gather_info_read_stats(N::in, X0::in, X::out),
%     [will_not_call_mercury, promise_pure],
% "
%     ++MR_stats_read[N];
%     X = X0;
% ").
% 
% :- pred gather_info_write_stats(int::in, T::in, T::in,
%     simplify_info::in, simplify_info::out) is det.
% 
% :- pragma foreign_proc("C",
%     gather_info_write_stats(N::in, Old::in, New::in, Info0::in, Info::out),
%     [will_not_call_mercury, promise_pure],
% "
%     if (((MR_Unsigned) Old) == ((MR_Unsigned) New)) {
%         ++MR_stats_same[N];
%     } else {
%         ++MR_stats_diff[N];
%     }
% 
%     Info = Info0;
% ").
% 
% :- interface.
% :- import_module io.
% :- pred write_simplify_info_stats(io::di, io::uo) is det.
% :- implementation.
% 
% :- pragma foreign_proc("C",
%     write_simplify_info_stats(IO0::di, IO::uo),
%     [will_not_call_mercury, promise_pure],
% "
%     FILE *fp;
% 
%     fp = fopen(""/tmp/SIMPLIFY_INFO_STATS"", ""a"");
%     if (fp != NULL) {
%         int i;
%         for (i = 0; i < MR_NUM_INFO_STATS; i++) {
%             fprintf(fp, ""stat_rsd %d %lu %lu %lu\\n"",
%                 i, MR_stats_read[i], MR_stats_same[i], MR_stats_diff[i]);
%         }
%     }
% 
%     IO = IO0;
% ").

%---------------------------------------------------------------------------%

simplify_do_warn_simple_code(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_warn_simple_code = yes.
simplify_do_warn_duplicate_calls(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_warn_duplicate_calls = yes.
simplify_do_warn_implicit_stream_calls(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_warn_implicit_stream_calls = yes.
simplify_do_format_calls(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_format_calls = yes.
simplify_do_warn_obsolete(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_warn_obsolete = yes.
simplify_do_mark_code_model_changes(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_mark_code_model_changes = yes.
simplify_do_after_front_end(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_after_front_end = yes.
simplify_do_excess_assign(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_excess_assign = yes.
simplify_do_test_after_switch(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_test_after_switch = yes.
simplify_do_elim_removable_scopes(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_elim_removable_scopes = yes.
simplify_do_opt_duplicate_calls(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_opt_duplicate_calls = yes.
simplify_do_const_prop(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_constant_prop = yes.
simplify_do_common_struct(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_common_struct = yes.
simplify_do_extra_common_struct(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_extra_common_struct = yes.
simplify_do_ignore_par_conjunctions(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_ignore_par_conjunctions = yes.

simplify_do_warn_or_opt_duplicate_calls(Info, OptDuplicateCalls) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    WarnDuplicateCalls = SimplifyTasks ^ do_warn_duplicate_calls,
    OptDuplicateCalls = SimplifyTasks ^ do_opt_duplicate_calls,
    ( WarnDuplicateCalls = yes
    ; OptDuplicateCalls = yes
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_info.
%---------------------------------------------------------------------------%
