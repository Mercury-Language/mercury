%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: simplify_info.m.
%
% This module defines the simplify_info type and its access predicates.
% This contains information for use by the other submodules of simplify.m:
% both static information (such as identify of the procedure whose body is
% being simplified), and information specific to the current point in the
% simplification process (such as the current instmap).
%
%----------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_info.
:- interface.

:- import_module check_hlds.simplify.simplify_tasks.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module maybe.

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

%----------------------------------------------------------------------------%

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

:- pred simplify_info_add_error_spec(error_spec::in,
    simplify_info::in, simplify_info::out) is det.

:- pred simplify_info_add_simple_code_spec(error_spec::in,
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
:- pred simplify_info_get_rtti_varmaps(simplify_info::in, rtti_varmaps::out)
    is det.
:- pred simplify_info_get_fully_strict(simplify_info::in, bool::out) is det.

:- pred simplify_info_get_pred_proc_id(simplify_info::in,
    pred_id::out, proc_id::out) is det.
:- pred simplify_info_get_inst_varset(simplify_info::in,
    inst_varset::out) is det.
:- pred simplify_info_get_elim_vars(simplify_info::in,
    list(list(prog_var))::out) is det.
:- pred simplify_info_get_error_specs(simplify_info::in, list(error_spec)::out)
    is det.
:- pred simplify_info_get_should_requantify(simplify_info::in, bool::out)
    is det.
:- pred simplify_info_get_should_rerun_det(simplify_info::in, bool::out)
    is det.
:- pred simplify_info_get_cost_delta(simplify_info::in, int::out) is det.
:- pred simplify_info_get_has_parallel_conj(simplify_info::in,
    has_parallel_conj::out) is det.
:- pred simplify_info_get_found_contains_trace(simplify_info::in, bool::out)
    is det.
:- pred simplify_info_get_has_user_event(simplify_info::in,
    has_user_event::out) is det.

:- pred simplify_info_set_simplify_tasks(simplify_tasks::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_module_info(module_info::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_var_types(vartypes::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_varset(prog_varset::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_rtti_varmaps(rtti_varmaps::in,
    simplify_info::in, simplify_info::out) is det.

:- pred simplify_info_set_elim_vars(list(list(prog_var))::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_error_specs(list(error_spec)::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_should_requantify(
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_should_rerun_det(
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_cost_delta(int::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_has_parallel_conj(has_parallel_conj::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_found_contains_trace(bool::in,
    simplify_info::in, simplify_info::out) is det.
:- pred simplify_info_set_has_user_event(has_user_event::in,
    simplify_info::in, simplify_info::out) is det.

%---------------------------------------------------------------------------%

:- pred simplify_do_warn_simple_code(simplify_info::in) is semidet.
:- pred simplify_do_warn_duplicate_calls(simplify_info::in) is semidet.
:- pred simplify_do_format_calls(simplify_info::in) is semidet.
:- pred simplify_do_warn_obsolete(simplify_info::in) is semidet.
:- pred simplify_do_mark_code_model_changes(simplify_info::in) is semidet.
:- pred simplify_do_after_front_end(simplify_info::in) is semidet.
:- pred simplify_do_excess_assign(simplify_info::in) is semidet.
:- pred simplify_do_elim_removable_scopes(simplify_info::in) is semidet.
:- pred simplify_do_opt_duplicate_calls(simplify_info::in) is semidet.
:- pred simplify_do_const_prop(simplify_info::in) is semidet.
:- pred simplify_do_common_struct(simplify_info::in) is semidet.
:- pred simplify_do_extra_common_struct(simplify_info::in) is semidet.
:- pred simplify_do_ignore_par_conjunctions(simplify_info::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_type_subst.

:- import_module int.
:- import_module map.
:- import_module require.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type simplify_info
    --->    simplify_info(
                % The most frequently used fields.
                % XXX Move another field here from simplify_sub_info
                % to make eight. Boehm will use eight words even for
                % seven fields.

                % The tasks we do in this invocation of simplification.
/* 1 */         simp_simplify_tasks         :: simplify_tasks,

                % The whole module. Those parts of it that are also contained
                % in other fields of simplify_info and simplify_sub_info
                % may be out of date in the module_info.
/* 2 */         simp_module_info            :: module_info,

                % Some fields of the proc_info of the procedure being
                % simplified.
/* 3 */         simp_varset                 :: prog_varset,
/* 4 */         simp_vartypes               :: vartypes,
/* 5 */         simp_rtti_varmaps           :: rtti_varmaps,

                % The value of the --fully-strict option.
/* 6 */         simp_fully_strict           :: bool,

/* 7 */         simp_sub_info               :: simplify_sub_info
            ).

:- type simplify_sub_info
    --->    simplify_sub_info(
                % The id of the procedure we are simplifying, and some of the
                % fields of its proc_info.
                ssimp_pred_id               :: pred_id,
                ssimp_proc_id               :: proc_id,
                ssimp_inst_varset           :: inst_varset,

                % The variables we have eliminated. Each list of vars consists
                % of a list of consecutive variable numbers in ascending order.
                % The relative order of the lists is unknown.
                ssimp_elim_vars             :: list(list(prog_var)),

                ssimp_error_specs           :: list(error_spec),

                % Does the goal need requantification?
                ssimp_should_requantify     :: bool,

                % Does determinism analysis need to be rerun?
                ssimp_should_rerun_det      :: bool,

                % Measure of the improvement in the goal from simplification.
                ssimp_cost_delta            :: int,

                % Have we seen a parallel conjunction?
                ssimp_has_parallel_conj     :: has_parallel_conj,

                % Have we seen a goal with a feature that says it contains
                % a trace goal?
                ssimp_found_contains_trace  :: bool,

                % Have we seen an event call?
                ssimp_has_user_event        :: has_user_event
            ).

simplify_info_init(ModuleInfo, PredId, ProcId, ProcInfo, SimplifyTasks,
        Info) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, fully_strict, FullyStrict),
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    ElimVars = [],
    Specs = [],
    ShouldRequantity = no,
    ShouldRerunDet = no,
    CostDelta = 0,
    HasParallelConj = has_no_parallel_conj,
    HasUserEvent = has_no_user_event,
    SubInfo = simplify_sub_info(PredId, ProcId, InstVarSet,
        ElimVars, Specs, ShouldRequantity, ShouldRerunDet, CostDelta,
        HasParallelConj, no, HasUserEvent),
    Info = simplify_info(SimplifyTasks, ModuleInfo, VarSet, VarTypes,
        RttiVarMaps, FullyStrict, SubInfo).

simplify_info_reinit(SimplifyTasks, !Info) :-
    !Info ^ simp_simplify_tasks := SimplifyTasks,
    !Info ^ simp_sub_info ^ ssimp_should_requantify := no,
    !Info ^ simp_sub_info ^ ssimp_should_rerun_det := no,
    !Info ^ simp_sub_info ^ ssimp_has_parallel_conj := has_no_parallel_conj,
    !Info ^ simp_sub_info ^ ssimp_has_user_event := has_no_user_event.

%-----------------------------------------------------------------------------%

simplify_info_get_pred_proc_info(Info, PredInfo, ProcInfo) :-
    simplify_info_get_module_info(Info, ModuleInfo),
    simplify_info_get_pred_proc_id(Info, PredId, ProcId),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo).

simplify_info_add_elim_vars(ElimVars, !Info) :-
    simplify_info_get_elim_vars(!.Info, ElimVarsLists0),
    ElimVarsLists = [ElimVars | ElimVarsLists0],
    simplify_info_set_elim_vars(ElimVarsLists, !Info).

simplify_info_add_error_spec(Spec, !Info) :-
    simplify_info_get_error_specs(!.Info, Specs0),
    Specs = [Spec | Specs0],
    simplify_info_set_error_specs(Specs, !Info).

simplify_info_add_simple_code_spec(Spec, !Info) :-
    ( simplify_do_warn_simple_code(!.Info) ->
        simplify_info_add_error_spec(Spec, !Info)
    ;
        true
    ).

simplify_info_incr_cost_delta(Incr, !Info) :-
    simplify_info_get_cost_delta(!.Info, CostDelta0),
    CostDelta = CostDelta0 + Incr,
    simplify_info_set_cost_delta(CostDelta, !Info).

simplify_info_apply_substitutions_and_duplicate(ToVar, FromVar, TSubst,
        !Info) :-
    simplify_info_get_var_types(!.Info, VarTypes0),
    simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    transform_var_types(apply_rec_subst_to_type(TSubst), VarTypes0, VarTypes),
    Renaming = map.singleton(ToVar, FromVar),
    apply_substitutions_to_rtti_varmaps(map.init, TSubst, Renaming,
        RttiVarMaps0, RttiVarMaps1),
    rtti_var_info_duplicate(FromVar, ToVar, RttiVarMaps1, RttiVarMaps),
    simplify_info_set_var_types(VarTypes, !Info),
    simplify_info_set_rtti_varmaps(RttiVarMaps, !Info).

%-----------------------------------------------------------------------------%

simplify_info_get_simplify_tasks(Info, Tasks) :-
    Tasks = Info ^ simp_simplify_tasks.
simplify_info_get_module_info(Info, MI) :-
    MI = Info ^ simp_module_info.
simplify_info_get_varset(Info, VS) :-
    VS = Info ^ simp_varset.
simplify_info_get_var_types(Info, VT) :-
    VT = Info ^ simp_vartypes.
simplify_info_get_rtti_varmaps(Info, RVM) :-
    RVM = Info ^ simp_rtti_varmaps.
simplify_info_get_fully_strict(Info, FS) :-
    FS = Info ^ simp_fully_strict.

simplify_info_get_pred_proc_id(Info, PredId, ProcId) :-
    SubInfo = Info ^ simp_sub_info,
    PredId = SubInfo ^ ssimp_pred_id,
    ProcId = SubInfo ^ ssimp_proc_id.
simplify_info_get_inst_varset(Info, IVS) :-
    IVS = Info ^ simp_sub_info ^ ssimp_inst_varset.
simplify_info_get_elim_vars(Info, EV) :-
    EV = Info ^ simp_sub_info ^ ssimp_elim_vars.
simplify_info_get_error_specs(Info, Specs) :-
    Specs = Info ^ simp_sub_info ^ ssimp_error_specs.
simplify_info_get_should_requantify(Info, RQ) :-
    RQ = Info ^ simp_sub_info ^ ssimp_should_requantify.
simplify_info_get_should_rerun_det(Info, RRD) :-
    RRD = Info ^ simp_sub_info ^ ssimp_should_rerun_det.
simplify_info_get_cost_delta(Info, CD) :-
    CD = Info ^ simp_sub_info ^ ssimp_cost_delta.
simplify_info_get_has_parallel_conj(Info, HPC) :-
    HPC = Info ^ simp_sub_info ^ ssimp_has_parallel_conj.
simplify_info_get_found_contains_trace(Info, FCT) :-
    FCT = Info ^ simp_sub_info ^ ssimp_found_contains_trace.
simplify_info_get_has_user_event(Info, HUE) :-
    HUE = Info ^ simp_sub_info ^ ssimp_has_user_event.

simplify_info_set_simplify_tasks(Tasks, !Info) :-
    !Info ^ simp_simplify_tasks := Tasks.
simplify_info_set_module_info(MI, !Info) :-
    !Info ^ simp_module_info := MI.
simplify_info_set_varset(VS, !Info) :-
    !Info ^ simp_varset := VS.
simplify_info_set_var_types(VT, !Info) :-
    !Info ^ simp_vartypes := VT.
simplify_info_set_rtti_varmaps(RVM, !Info) :-
    !Info ^ simp_rtti_varmaps := RVM.

simplify_info_set_elim_vars(EV, !Info) :-
    !Info ^ simp_sub_info ^ ssimp_elim_vars := EV.
simplify_info_set_error_specs(Specs, !Info) :-
    !Info ^ simp_sub_info ^ ssimp_error_specs := Specs.
simplify_info_set_should_requantify(!Info) :-
    !Info ^ simp_sub_info ^ ssimp_should_requantify := yes.
simplify_info_set_should_rerun_det(!Info) :-
    !Info ^ simp_sub_info ^ ssimp_should_rerun_det := yes.
simplify_info_set_cost_delta(CD, !Info) :-
    !Info ^ simp_sub_info ^ ssimp_cost_delta := CD.
simplify_info_set_has_parallel_conj(HPC, !Info) :-
    !Info ^ simp_sub_info ^ ssimp_has_parallel_conj := HPC.
simplify_info_set_found_contains_trace(FCT, !Info) :-
    !Info ^ simp_sub_info ^ ssimp_found_contains_trace := FCT.
simplify_info_set_has_user_event(HUE, !Info) :-
    !Info ^ simp_sub_info ^ ssimp_has_user_event := HUE.

%---------------------------------------------------------------------------%

simplify_do_warn_simple_code(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_warn_simple_code = yes.
simplify_do_warn_duplicate_calls(Info) :-
    simplify_info_get_simplify_tasks(Info, SimplifyTasks),
    SimplifyTasks ^ do_warn_duplicate_calls = yes.
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

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_info.
%---------------------------------------------------------------------------%
