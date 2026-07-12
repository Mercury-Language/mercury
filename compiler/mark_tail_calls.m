%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2008, 2010-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mark_tail_calls.m.
% Main author: zs.
%
% This module adds a feature to all recursive calls that can be implemented
% as tail calls.
%
% Since an assignment unification that simply renames an output of a recursive
% call may prevent that call from being recognized as a tail call, you probably
% want to run excess assign elimination just before invoking this module.
%
% This module also contains code to detect recursive calls which are not
% *tail* recursive, and generating warnings for them. The point of this
% is to point out to the programmer the calls that may lead to stack usage
% that is proportional to the size of the input, and may thus lead to
% stack exhaustion for large inputs.
%
% Every recursive call we identify as a tail call will be implemented
% as a tail call by the LLDS backend. This is not true for the MLDS backend,
% which imposes additional requirements on tail calls. Calls that we
% identify as tail calls that turn out not to be implementable as such
% will be reported by the MLDS code generator (specifically, ml_proc_gen.m
% and ml_call_gen.m), using a predicate defined here which is exported
% for this purpose.
% XXX This should not be needed, because this module should have
% already reported any such calls.
%
%---------------------------------------------------------------------------%

:- module hlds.mark_tail_calls.
:- interface.

:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module list.
:- import_module set.
:- import_module set_tree234.

%---------------------------------------------------------------------------%

    % Mark both self and mutual tail recursive calls in the module,
    % and generate warnings about the absence of tail recursion where requested
    % by the values of the options and/or by pragmas.
    %
:- pred mark_self_and_mutual_tail_rec_calls_in_module_for_mlds_code_gen(
    hlds_dependency_info::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------%

    % Mark tail calls as needed by the LLDS code generator.
    %
    % This can mean
    %
    % - marking self-tail-recursive calls so that the code generator can emit
    %   TAIL events and tail recursive calls instead of non tail recursive
    %   calls followed by an EXIT event. This is needed only if we are
    %   generating code for the debugger.
    %
    % - generating warnings for recursive calls that are not *tail* recursive,
    %   if the warn_non_tail_recursion option is set.
    %
    % It can also mean both, or neither.
    %
    % The LLDS code generator can be invoked to compile procedures either
    % phase-after-phase, or procedure-after-procedure; the two do
    % the same jobs, but in different order. It calls the in_pred version
    % when compiling by phases, and it calls the in_proc version when
    % compiling by procedures.
    %
:- pred mark_tail_rec_calls_in_pred_for_llds_code_gen(
    scc_map(pred_proc_id)::in, pred_id::in, module_info::in, module_info::out,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred mark_tail_rec_calls_in_proc_for_llds_code_gen(module_info::in,
    pred_id::in, proc_id::in, pred_info::in,
    scc_map(pred_proc_id)::in, proc_info::in, proc_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%
% These types and predicates are exported for ml_proc_gen.m and ml_call_gen.m.
% ml_proc_gen.m sets up the warning parameters, and ml_call_gen.m uses them
% when it finds that it even though mark_tail_calls.m has marked a call
% as a tail call, it cannot actually implement that call as a tail call.
%

:- type report_requested_by
    --->    request_by_code
            % Usually, the code doing the requesting
            % is a require_tail_recursion pragma, but it can also be
            % a disable_warnings scope.
    ;       request_by_option.

:- type maybe_warn_non_tail_self_rec
    --->    do_not_warn_non_tail_self_rec
    ;       warn_non_tail_self_rec.

:- type maybe_warn_non_tail_mutual_rec
    --->    do_not_warn_non_tail_mutual_rec
    ;       warn_non_tail_mutual_rec.

:- type warn_non_tail_rec_params
    --->    warn_non_tail_rec_params(
                report_requested_by,
                warning_or_error,
                report_in_which_grades,
                maybe_warn_non_tail_self_rec,
                maybe_warn_non_tail_mutual_rec
            ).

:- pred get_default_warn_parms(globals::in,
    warn_non_tail_rec_params::out) is det.

    % maybe_override_warn_params_for_proc(ProcInfo, Params, ProcParams):
    %
    % If the given procedure has a pragma that governs what non-tail recursion
    % warnings (if any) we should generate for its code, return a value for
    % ProcParams that reflects this pragma. Otherwise, return Params,
    % the parameters that apply by default.
    %
:- pred maybe_override_warn_params_for_proc(proc_info::in,
    warn_non_tail_rec_params::in, warn_non_tail_rec_params::out) is det.

%---------------------------------------------------------------------------%

:- type nontail_rec_call_reason
    --->    ntrcr_program(set_tree234(later_op))
            % The call is not a tail call in the program, because
            % it is followed by some operations.
            %
            % If the set is nonempty, which it always should be when
            % we generate a report from inside this module, the elements
            % of the set describe the kinds of operations that will happen
            % after the call.
            %
            % If the set is empty, which it always will be when
            % we generate a report from outside this module, then we have
            % no such information. (Our outside callers are part of the
            % MLDS code generator, and its tasks do not include keeping track
            % of such things).
            %
            % The ability of the MLDS code generator to generate diagnostics
            % about non-tail recursion is mostly a historical accident.
            % We should take away that ability as soon as we are sure that
            % all the non-tail recursive calls that the code generator
            % would report are reported by this module as well.
            %
            % However, until that is done, we do NOT want both (a) the code of
            % do_mark_tail_rec_calls_in_proc and (b) the MLDS code generator
            % to generate reports for the same non-tail recursive call.
            % This is because we add a short explanation for any later_op
            % in the set that is not visible (or at least not easily visible)
            % to users. The code that writes out error_specs removes any
            % duplicates, but it cannot remove *almost* duplicates
            % that differ only in the presence vs absence of these
            % explanations. The code below that prints the (potentially)
            % more informative message therefore attaches this feature
            % to the goal the message is about. When a call to
            % maybe_report_nontail_recursive_call from the MLDS code generator
            % sees this feature, it should refrain from generating a report.

    ;       ntrcr_mlds_in_scc_not_in_tscc
            % The call is a tail call in the program, but the MLDS code
            % generator can't optimize it because the caller and the callee,
            % although they are in the same SCC, are not in the same *T*SCC.

    ;       ntrcr_mlds_in_tscc_stack_ref
            % The call is a tail call in the program, but the MLDS code
            % generator can't optimize it. The caller and the callee
            % are in the same TSCC, but making the call a tail call
            % would leave at least one dangling stack reference.

    ;       ntrcr_mlds_model_non_in_cont_func.
            % The call is a tail call in the program, but the MLDS code
            % generator can't optimize it, because the call site is not
            % in the main function of its predicate, but in a separate
            % continuation function.

:- type later_op.

:- type nontail_rec_obviousness
    --->    non_obvious_nontail_rec
    ;       obvious_nontail_rec.

:- pred maybe_report_nontail_recursive_call(module_info::in,
    warn_non_tail_rec_params::in, pred_proc_id::in, pred_proc_id::in,
    set(goal_feature)::in, prog_context::in,
    nontail_rec_call_reason::in, nontail_rec_obviousness::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.mode_top_functor.
:- import_module hlds.type_util.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

mark_self_and_mutual_tail_rec_calls_in_module_for_mlds_code_gen(DepInfo,
        !ModuleInfo, !Specs) :-
    MaybeSelfRec = yes(feature_self_or_mutual_tail_rec_call),
    MaybeMutualRec = yes(feature_self_or_mutual_tail_rec_call),
    module_info_get_globals(!.ModuleInfo, Globals),
    get_default_warn_parms(Globals, WarnNonTailRecParams),
    Params = tail_rec_params(MaybeSelfRec, MaybeMutualRec,
        record_tail_recursion, WarnNonTailRecParams),
    get_bottom_up_sccs_with_entry_points(!.ModuleInfo, DepInfo,
        BottomUpSCCsEntryPoints),
    mark_tail_rec_calls_in_sccs_for_mlds(Params, BottomUpSCCsEntryPoints,
        !ModuleInfo, !Specs).

:- pred mark_tail_rec_calls_in_sccs_for_mlds(tail_rec_params::in,
    list(scc_with_entry_points)::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_tail_rec_calls_in_sccs_for_mlds(_Params, [], !ModuleInfo, !Specs).
mark_tail_rec_calls_in_sccs_for_mlds(Params, [SCCEntry | SCCEntries],
        !ModuleInfo, !Specs) :-
    SCCEntry = scc_with_entry_points(SCC, _CalledFromHigherSCC, _Exported),
    mark_tail_rec_calls_in_scc_for_mlds(Params, SCC, set.to_sorted_list(SCC),
        !ModuleInfo, !Specs),
    mark_tail_rec_calls_in_sccs_for_mlds(Params, SCCEntries,
        !ModuleInfo, !Specs).

:- pred mark_tail_rec_calls_in_scc_for_mlds(tail_rec_params::in,
    set(pred_proc_id)::in, list(pred_proc_id)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_tail_rec_calls_in_scc_for_mlds(_Params, _SCC, [], !ModuleInfo, !Specs).
mark_tail_rec_calls_in_scc_for_mlds(Params, SCC, [PredProcId | PredProcIds],
        !ModuleInfo, !Specs) :-
    PredProcId = proc(PredId, ProcId),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_proc_info(PredInfo0, ProcId, ProcInfo0),
    maybe_override_params_for_proc(ProcInfo0, Params, ProcParams),
    do_mark_tail_rec_calls_in_proc(ProcParams, !.ModuleInfo, SCC,
        PredId, ProcId, PredInfo0, ProcInfo0, ProcInfo,
        WasProcChanged, !Specs),
    (
        WasProcChanged = proc_was_not_changed
    ;
        WasProcChanged = proc_may_have_been_changed,
        pred_info_set_proc_info(ProcId, ProcInfo, PredInfo0, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ),
    mark_tail_rec_calls_in_scc_for_mlds(Params, SCC, PredProcIds,
        !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

mark_tail_rec_calls_in_pred_for_llds_code_gen(SCCMap, PredId,
        ModuleInfo, ModuleInfo, !PredInfo, !Specs) :-
    % We don't update ModuleInfo. Nevertheless, the passes_aux traversal
    % that our caller uses to call us requires us to pass back a new
    % ModuleInfo, even though it will itself put the updated PredInfo
    % back into ModuleInfo.
    module_info_get_globals(ModuleInfo, Globals),
    get_params_for_llds_code_gen(Globals, Params),
    ProcIds = pred_info_will_codegen_proc_ids(!.PredInfo),
    mark_tail_rec_calls_in_procs_for_llds(Params, ModuleInfo, SCCMap,
        PredId, ProcIds, !PredInfo, !Specs).

:- pred mark_tail_rec_calls_in_procs_for_llds(tail_rec_params::in,
    module_info::in, scc_map(pred_proc_id)::in, pred_id::in, list(proc_id)::in,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_tail_rec_calls_in_procs_for_llds(_Params, _ModuleInfo, _SCCMap,
        _PredId, [], !PredInfo, !Specs).
mark_tail_rec_calls_in_procs_for_llds(Params, ModuleInfo, SCCMap,
        PredId, [ProcId | ProcIds], !PredInfo, !Specs) :-
    pred_info_proc_info(!.PredInfo, ProcId, ProcInfo0),
    maybe_override_params_for_proc(ProcInfo0, Params, ProcParams),
    map.lookup(SCCMap, proc(PredId, ProcId), SCC),
    do_mark_tail_rec_calls_in_proc(ProcParams, ModuleInfo, SCC, PredId,
        ProcId, !.PredInfo, ProcInfo0, ProcInfo, WasProcChanged, !Specs),
    (
        WasProcChanged = proc_was_not_changed
    ;
        WasProcChanged = proc_may_have_been_changed,
        pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo)
    ),
    mark_tail_rec_calls_in_procs_for_llds(Params, ModuleInfo, SCCMap,
        PredId, ProcIds, !PredInfo, !Specs).

%---------------------------------------------------------------------------%

mark_tail_rec_calls_in_proc_for_llds_code_gen(ModuleInfo, PredId, ProcId,
        PredInfo, SCCMap, !ProcInfo, !Specs) :-
    module_info_get_globals(ModuleInfo, Globals),
    get_params_for_llds_code_gen(Globals, Params),
    maybe_override_params_for_proc(!.ProcInfo, Params, ProcParams),
    map.lookup(SCCMap, proc(PredId, ProcId), SCC),
    % mark_tail_rec_calls_in_proc_for_llds_code_gen is called only when we are
    % doing proc-by-proc, as opposed to phase-by-phase, code generation.
    % For this, we don't need to put the new proc_info back into its pred_info.
    do_mark_tail_rec_calls_in_proc(ProcParams, ModuleInfo, SCC, PredId, ProcId,
        PredInfo, !ProcInfo, _WasProcChanged, !Specs).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- func no_warnings_non_tail_rec_params(report_requested_by)
    = warn_non_tail_rec_params.

no_warnings_non_tail_rec_params(RequestBy) = Params :-
    % Since neither SelfRec nor MutualRec is set, the values of
    % WarnOrError and Grades do not matter.
    Params = warn_non_tail_rec_params(RequestBy, we_warning,
        in_tailrec_grades_only,
        do_not_warn_non_tail_self_rec, do_not_warn_non_tail_mutual_rec).

get_default_warn_parms(Globals, WarnNonTailRecParams) :-
    % XXX Should we add an option to control the setting of this default?
    % I (zs) do not think so.
    Grades = in_tailrec_grades_only,

    globals.lookup_bool_option(Globals, warn_non_tail_recursion_self,
        WarnNonTailSelfRecBool),
    (
        WarnNonTailSelfRecBool = yes,
        WarnNonTailSelfRecOpt = warn_non_tail_self_rec
    ;
        WarnNonTailSelfRecBool = no,
        WarnNonTailSelfRecOpt = do_not_warn_non_tail_self_rec
    ),
    globals.lookup_bool_option(Globals, warn_non_tail_recursion_mutual,
        WarnNonTailMutualRecBool),
    (
        WarnNonTailMutualRecBool = yes,
        WarnNonTailMutualRecOpt = warn_non_tail_mutual_rec
    ;
        WarnNonTailMutualRecBool = no,
        WarnNonTailMutualRecOpt = do_not_warn_non_tail_mutual_rec
    ),
    WarnNonTailRecParams = warn_non_tail_rec_params(request_by_option,
        we_warning, Grades, WarnNonTailSelfRecOpt, WarnNonTailMutualRecOpt).

maybe_override_warn_params_for_proc(ProcInfo, WarnParams, ProcWarnParams) :-
    proc_info_get_maybe_require_tailrec_info(ProcInfo, MaybeRequireTailRec),
    (
        MaybeRequireTailRec = no,
        ProcWarnParams = WarnParams
    ;
        MaybeRequireTailRec = yes(Pragma),
        (
            Pragma = disable_nontailrec_reports(_),
            ProcWarnParams = no_warnings_non_tail_rec_params(request_by_code)
        ;
            Pragma = enable_nontailrec_reports(WarnOrError, RecType, Grades,
                _Context),
            (
                RecType = only_self_recursion_must_be_tail,
                SelfRec = warn_non_tail_self_rec,
                MutualRec = do_not_warn_non_tail_mutual_rec
            ;
                RecType = both_self_and_mutual_recursion_must_be_tail,
                SelfRec = warn_non_tail_self_rec,
                MutualRec = warn_non_tail_mutual_rec
            ),
            ProcWarnParams = warn_non_tail_rec_params(request_by_code,
                WarnOrError, Grades, SelfRec, MutualRec)
        )
    ).

:- pred maybe_override_params_for_proc(proc_info::in,
    tail_rec_params::in, tail_rec_params::out) is det.

maybe_override_params_for_proc(ProcInfo, Params, ProcParams) :-
    WarnParams = Params ^ warn_params,
    maybe_override_warn_params_for_proc(ProcInfo, WarnParams, ProcWarnParams),
    ProcParams = Params ^ warn_params := ProcWarnParams.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % The MLDS code generator wants to know which procedures have
    % self and/or mutual tail calls, so it does not have to look for TSCC
    % (via-tail-call-only SCCs) among procedures that don't have any tail
    % calls.
    %
    % The LLDS code generator wants to know whether it should prepare
    % for any TAIL events in the procedure body.
    %
    % This module gives both generators the information they need
    % by recording the absence or presence of both self and mutually
    % recursive tail calls in the has_tail_call field of the proc_info.
    %
    % (For the time being, the debugger supports TAIL events only for
    % *self*-tail-recursive calls, and not for mutually-tail-recursive calls,
    % so it ignores the part of the has_tail_call field that talks about
    % mutually recursive tail calls.)
    %
:- type maybe_record_tail_rec
    --->    do_not_record_tail_recursion
    ;       record_tail_recursion.

:- type tail_rec_params
    --->    tail_rec_params(
                % If set to `yes(Feature)', add Feature to the goal_info
                % of self-tail-recursive calls.
                self_rec_goal_feature       :: maybe(goal_feature),

                % If set to `yes(Feature)', add Feature to the goal_info
                % of mutually--tail-recursive calls.
                mutual_rec_goal_feature     :: maybe(goal_feature),

                should_record_tail_rec      :: maybe_record_tail_rec,

                % The parameters governing whether what warnings or errors
                % we should generate about tail recursive calls or their
                % absence.
                warn_params                 :: warn_non_tail_rec_params
            ).

:- pred get_params_for_llds_code_gen(globals::in,
    tail_rec_params::out) is det.

get_params_for_llds_code_gen(Globals, Params) :-
    get_default_warn_parms(Globals, WarnNonTailRecParams),
    globals.lookup_bool_option(Globals, exec_trace_tail_rec, ExecTraceTailRec),
    (
        ExecTraceTailRec = yes,
        MaybeSelf = yes(feature_debug_self_tail_rec_call),
        Params = tail_rec_params(MaybeSelf, no, record_tail_recursion,
            WarnNonTailRecParams)
    ;
        ExecTraceTailRec = no,
        Params = tail_rec_params(no, no, do_not_record_tail_recursion,
            WarnNonTailRecParams)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type was_proc_changed
    --->    proc_was_not_changed
    ;       proc_may_have_been_changed.

:- pred do_mark_tail_rec_calls_in_proc(tail_rec_params::in,
    module_info::in, set(pred_proc_id)::in,
    pred_id::in, proc_id::in, pred_info::in, proc_info::in, proc_info::out,
    was_proc_changed::out, list(error_spec)::in, list(error_spec)::out) is det.

do_mark_tail_rec_calls_in_proc(Params, ModuleInfo, SCC, PredId, ProcId,
        PredInfo, !ProcInfo, WasProcChanged, !Specs) :-
    proc_info_interface_determinism(!.ProcInfo, Detism),
    determinism_components(Detism, _CanFail, SolnCount),
    (
        % For at_most_zero procedures, there is no point in handling tail calls
        % specially.
        SolnCount = at_most_zero,
        WasProcChanged = proc_was_not_changed
    ;
        ( SolnCount = at_most_one
        ; SolnCount = at_most_many
        ; SolnCount = at_most_many_cc
        ),

        Params = tail_rec_params(MaybeSelfFeature, MaybeMutualFeature,
            MaybeRecordTailCalls, WarnNonTailRecParams),
        ( if
            % It is reasonably common that we don't need to check
            % for tail calls at all.
            MaybeSelfFeature = no,
            MaybeMutualFeature = no,
            MaybeRecordTailCalls = do_not_record_tail_recursion,
            WarnNonTailRecParams = warn_non_tail_rec_params(_, _, _,
                do_not_warn_non_tail_self_rec, do_not_warn_non_tail_mutual_rec)
        then
            WasProcChanged = proc_was_not_changed
        else
            pred_info_get_arg_types(PredInfo, Types),
            proc_info_get_goal(!.ProcInfo, Goal0),
            proc_info_get_argmodes(!.ProcInfo, Modes),
            proc_info_get_headvars(!.ProcInfo, HeadVars),
            proc_info_get_var_table(!.ProcInfo, VarTable),
            find_output_args(ModuleInfo, Types, Modes, HeadVars, Outputs),

            Info0 = mark_tail_rec_calls_info(ModuleInfo, PredInfo,
                proc(PredId, ProcId), SCC, VarTable, Params,
                has_no_self_tail_rec_call, has_no_mutual_tail_rec_call,
                not_found_any_rec_calls, []),
            AtTail0 = at_tail_info(set_tree234.init, Outputs),
            mark_tail_rec_calls_in_goal(Goal0, Goal,
                AtTail0, _AtTail, Info0, Info),
            Info = mark_tail_rec_calls_info(_, _, _, _, _, _,
                HasSelfTailRecCall, HasMutualTailRecCall,
                FoundAnyRecCalls, GoalSpecs),

            proc_info_set_goal(Goal, !ProcInfo),

            maybe_report_no_tail_or_nontail_recursive_calls(PredInfo,
                !.ProcInfo, FoundAnyRecCalls, !Specs),
            (
                MaybeRecordTailCalls = do_not_record_tail_recursion
            ;
                MaybeRecordTailCalls = record_tail_recursion,
                HasTailRecCall = has_tail_rec_call(HasSelfTailRecCall,
                    HasMutualTailRecCall),
                proc_info_set_has_tail_rec_call(HasTailRecCall, !ProcInfo)
            ),
            !:Specs = GoalSpecs ++ !.Specs,
            WasProcChanged = proc_may_have_been_changed
        )
    ).

:- pred find_output_args(module_info::in,
     list(mer_type)::in, list(mer_mode)::in, list(prog_var)::in,
     list(prog_var)::out) is det.

find_output_args(ModuleInfo, Types, Modes, Vars, OutputVars) :-
    ( if
        Types = [HeadType | TailTypes],
        Modes = [HeadMode | TailModes],
        Vars = [HeadVar | TailVars]
    then
        find_output_args(ModuleInfo, TailTypes, TailModes, TailVars,
            TailOutputVars),
        mode_to_top_functor_mode(ModuleInfo, HeadMode, HeadType,
            TopFunctorMode),
        (
            ( TopFunctorMode = top_in
            ; TopFunctorMode = top_unused
            ),
            OutputVars = TailOutputVars
        ;
            TopFunctorMode = top_out,
            IsDummy = is_type_a_dummy(ModuleInfo, HeadType),
            (
                IsDummy = is_not_dummy_type,
                OutputVars = [HeadVar | TailOutputVars]
            ;
                IsDummy = is_dummy_type,
                OutputVars = TailOutputVars
            )
        )
    else if
        Types = [],
        Modes = [],
        Vars = []
    then
        OutputVars = []
    else
        unexpected($pred, "list length mismatch")
    ).

%---------------------------------------------------------------------------%

:- type at_tail_info
    --->    at_tail_info(
                % Is the current position within the procedure a tail position?
                % The answer is "yes" if-and-only-if this set is empty.
                set_tree234(later_op),

                % If the current position within the procedure is a tail
                % position, then what are the output arguments? These will
                % start out as the vector of the procedure's output arguments,
                % but if our backwards traversal sees an assignment unification
                % to one of these variables, then we replace it in this list
                % with the id of the variable it is assigned from.
                % (This is because any recursive call must define the
                % assigned-from variable, since the mode system ensures
                % that the assigned-to variable cannot have as generators
                % *both* the recursive call and the assignment unification.)
                %
                % If the first argument is a nonempty set, then this field
                % is meaningless. The reason why we do not have a separate
                % function symbol in this type that eliminates this field
                % in that event is that we have tried that, and it complicates
                % all our code for managing branching goals.
                list(prog_var)
            ).

:- type later_op
    --->    later_unify
    ;       later_unify_assign
    ;       later_nonrec_call
    ;       later_rec_call

    ;       later_par_join
            % Each conjunct in a parallel conjunction is followed by
            % the join of the parallel threads.

    ;       later_disjunction
    ;       later_disjunction_fail
    ;       later_switch
    ;       later_ite
    ;       later_negation
            % These mean that a branched goal (a disjunction, a switch,
            % or an if-then-else) follows the goal whose initial at_tail_info
            % contains one of these later_ops.
            %
            % Just in case we can use these details later, we distinguish
            % empty disjunctions (later_disjunction_fail) from nonempty ones
            % (later_disjunction), and negations (which are effectively
            % special cases of if-then-elses) from their general form.

    ;       later_next_disjunct
            % This applies to goals that represent non-last disjuncts.
            % Such goals are followed by the code of the next disjunct.

    ;       later_cond_end
            % This applies to goal that represent an if-then-else's condition.
            % Such goals are followed by the code that switches between
            % the then part and the else part.

    ;       later_negation_end
            % This applies to goals inside negations. Such goals are
            % followed by the code that flips success to failure or vice versa.

    ;       later_commit
            % This applies to goals inside scopes that perform commits.
            % Such goals are followed by the code that performs that commit.

    ;       later_trail_prune.
            % This applies to goals that represent disjuncts
            % - in a model_det or model_semi disjunction
            % - in trailing grades.
            % Such goals are followed by an operation that prunes away
            % the trail ticket that was created to represent the disjunction.

:- type call_is_self_or_mutual_rec
    --->    call_is_self_rec
    ;       call_is_mutual_rec.

:- type mark_tail_rec_calls_info
    --->    mark_tail_rec_calls_info(
                mtc_module                  :: module_info,
                mtc_pred_info               :: pred_info,
                mtc_cur_proc                :: pred_proc_id,
                mtc_cur_scc                 :: set(pred_proc_id),
                mtc_var_table               :: var_table,
                mtc_params                  :: tail_rec_params,
                mtc_self_tail_rec_calls     :: has_self_tail_rec_call,
                mtc_mutual_tail_rec_calls   :: has_mutual_tail_rec_call,
                mtc_any_rec_calls           :: found_any_rec_calls,
                mtc_error_specs             :: list(error_spec)
            ).

%---------------------------------------------------------------------------%

    % mark_tail_rec_calls_in_goal(Goal0, Goal, !AtTail, !Info):
    %
    % This predicate performs a backwards traversal of Goal0.
    % It can transform Goal0 into Goal by adding features to self- or
    % mutually-tail-recursive calls as Params calls for it.
    % Params can also ask it to generate warnings for recursive calls
    % that are not *tail* recursive.
    %
    % Since we do a *backward* traversal, AtTail0 describes the situation
    % *after* Goal0, and the value of AtTail we return describes the situation
    % *before* Goal0 (which is also the situation before its marked-up twin
    % Goal).
    %
    % When the backwards traversal starts, the value of AtTail0 is initialized
    % to at_tail(MaybeOutputArgs). If Goal0 neither is a tailcall nor contains
    % a tailcall, but could actually follow a tailcall (which is possible
    % if it is either an assignment unification that simply renames an output
    % variable, or a conjunction of such unifications), then we return AtTail
    % as at_tail, but with a value of MaybeOutputArgs that is updated
    % to account for the renaming. We want this because Goal0 is a tail
    % recursive call only if (a) AtTail0 is at_tail(MaybeOutputArgs),
    % it is a call to !.Info ^ mtc_cur_proc or to a procedure in !.Info ^
    % mtc_cur_scc whose argument list matches MaybeOutputArgs in the
    % output argument positions, i.e. the positions in which MaybeOutputArgs
    % has a yes(_).
    %
    % When we see a goal that cannot follow a tail call (a goal which may be
    % a tail call itself), we return not_at_tail as the value of AtTail.
    % Its argument will say whether we have seen a recursive call (tail
    % or otherwise) earlier on the backwards traversal, i.e. in Goal or
    % in code that follows Goal.
    %
    % We record whether we have found any (self or mutual) recursive calls
    % in the mtc_any_rec_calls and mtc_self_tail_rec_calls fields.
    %
:- pred mark_tail_rec_calls_in_goal(hlds_goal::in, hlds_goal::out,
    at_tail_info::in, at_tail_info::out,
    mark_tail_rec_calls_info::in, mark_tail_rec_calls_info::out) is det.

mark_tail_rec_calls_in_goal(Goal0, Goal, AtTail0, AtTail, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        mark_tail_rec_calls_in_plain_call(GoalExpr0, GoalInfo0, Goal,
            AtTail0, AtTail, !Info)
    ;
        GoalExpr0 = unify(LHSVar, _, _, Unify0, _),
        Goal = Goal0,
        VarTable = !.Info ^ mtc_var_table,
        lookup_var_entry(VarTable, LHSVar, LHSVarEntry),
        LHSVarIsDummy = LHSVarEntry ^ vte_is_dummy,
        (
            LHSVarIsDummy = is_dummy_type,
            % Unifications involving dummy type variables are no-ops,
            % and do not prevent a preceding call from being a tail call.
            AtTail = AtTail0
        ;
            LHSVarIsDummy = is_not_dummy_type,
            (
                ( Unify0 = construct(_, _, _, _, _, _, _)
                ; Unify0 = deconstruct(_, _, _, _, _, _)
                ; Unify0 = simple_test(_, _)
                ; Unify0 = complicated_unify(_, _, _)
                ),
                add_later_op(later_unify, AtTail0, AtTail)
            ;
                Unify0 = assign(ToVar, FromVar),
                AtTail0 = at_tail_info(Laters0, Outputs0),
                ( if
                    set_tree234.is_empty(Laters0),
                    is_output_arg_rename(ToVar, FromVar, Outputs0, Outputs)
                then
                    AtTail = at_tail_info(Laters0, Outputs)
                else
                    add_later_op(later_unify_assign, AtTail0, AtTail)
                )
            )
        )
    ;
        ( GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ),
        Goal = Goal0,
        add_later_op(later_nonrec_call, AtTail0, AtTail)
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            AtTail1 = AtTail0
        ;
            ConjType = parallel_conj,
            % Tail calls in parallel conjunctions are only supported when
            % loop control is enabled. But loop control would have rewritten
            % the conjunction into a loop control scope, and therefore any
            % parallel conjunctions we find at *this* point cannot support
            % tail calls.
            add_later_op(later_par_join, AtTail0, AtTail1)
        ),
        list.reverse(Goals0, RevGoals0),
        mark_tail_rec_calls_in_rev_conj(RevGoals0, RevGoals, AtTail1, AtTail,
            !Info),
        list.reverse(RevGoals, Goals),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        % No goal inside a negation can be a *tail* call. However, a negation
        % can contain recursive calls about whose non-tail-call nature
        % we COULD generate a report.
        GoalExpr0 = negation(SubGoal0),
        add_later_op(later_negation_end, AtTail0, AtTail1),
        mark_tail_rec_calls_in_goal(SubGoal0, SubGoal,
            AtTail1, AtTail2, !Info),
        add_later_op(later_negation, AtTail2, AtTail),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Disjuncts0),
        ( if list.split_last(Disjuncts0, NonLastDisjuncts0, LastDisjunct0) then
            % If the disjunction is in tail position, then it is possible
            % for a goal inside the last disjunct to be a tail call,
            % unless the disjunction is a model_det or model_semi disjunction
            % in a trailing grade.
            Detism = goal_info_get_determinism(GoalInfo0),
            determinism_to_code_model(Detism, CodeModel),
            ( if
                ( CodeModel = model_det ; CodeModel = model_semi ),
                ModuleInfo = !.Info ^ mtc_module,
                module_info_get_globals(ModuleInfo, Globals),
                globals.lookup_bool_option(Globals, use_trail, yes)
            then
                add_later_op(later_trail_prune, AtTail0, AtTail1)
            else
                AtTail1 = AtTail0
            ),
            mark_tail_rec_calls_in_goal(LastDisjunct0, LastDisjunct,
                AtTail1, LastAtTail, !Info),
            % Even if the disjunction as a whole is in tail position,
            % a goal inside a nonlast disjunct cannot be a tail call,
            % because if it fails, its execution will be followed
            % by backtracking to later disjuncts.
            add_later_op(later_next_disjunct, AtTail1, NonLastAtTail0),
            list.map_foldl2(
                mark_tail_rec_calls_in_nonlast_disjunct(NonLastAtTail0),
                NonLastDisjuncts0, NonLastDisjuncts,
                [], NonLastAtTails, !Info),
            join_branch_at_tails(LastAtTail, NonLastAtTails,
                BeforeDisjunctsAtTail),
            add_later_op(later_disjunction, BeforeDisjunctsAtTail, AtTail),
            GoalExpr = disj(NonLastDisjuncts ++ [LastDisjunct]),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        else
            % There are no disjuncts. Any goals before the disjunction
            % will be followed by disj([]), which is `fail', so they cannot
            % be tail calls.
            add_later_op(later_disjunction_fail, AtTail0, AtTail),
            Goal = Goal0
        )
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list.map_foldl2(mark_tail_rec_calls_in_case(AtTail0), Cases0, Cases,
            [], AtTails, !Info),
        list.det_head_tail(AtTails, HeadAtTail, TailAtTails),
        join_branch_at_tails(HeadAtTail, TailAtTails, AtTail1),
        add_later_op(later_switch, AtTail1, AtTail),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        mark_tail_rec_calls_in_goal(Then0, Then, AtTail0, AtTailBeforeThen,
            !Info),
        mark_tail_rec_calls_in_goal(Else0, Else, AtTail0, AtTailBeforeElse,
            !Info),
        join_branch_at_tails(AtTailBeforeThen, [AtTailBeforeElse],
            AtTailAfterCond0),
        add_later_op(later_cond_end, AtTailAfterCond0, AtTailAfterCond),
        mark_tail_rec_calls_in_goal(Cond0, Cond, AtTailAfterCond, AtTail1,
            !Info),
        add_later_op(later_ite, AtTail1, AtTail),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        (
            Reason = disable_warnings(HeadWarning, TailWarnings),
            ( if
                ( HeadWarning = goal_warning_non_tail_recursive_calls
                ; list.member(goal_warning_non_tail_recursive_calls,
                    TailWarnings)
                )
            then
                OldParams = !.Info ^ mtc_params,
                InnerParams = OldParams ^ warn_params :=
                    no_warnings_non_tail_rec_params(request_by_code),
                InnerInfo0 = !.Info ^ mtc_params := InnerParams,
                mark_tail_rec_calls_in_goal(SubGoal0, SubGoal, AtTail0, AtTail,
                    InnerInfo0, InnerInfo),
                !:Info = InnerInfo ^ mtc_params := OldParams
            else
                mark_tail_rec_calls_in_goal(SubGoal0, SubGoal, AtTail0, AtTail,
                    !Info)
            )
        ;
            ( Reason = exist_quant(_, _)
            ; Reason = promise_solutions(_, _)
            ; Reason = commit(_)
            ),
            add_later_op(later_commit, AtTail0, AtTail1),
            mark_tail_rec_calls_in_goal(SubGoal0, SubGoal, AtTail1, AtTail,
                !Info)
        ;
            ( Reason = promise_purity(_)
            ; Reason = barrier(_)
            ; Reason = from_ground_term(_, _)
            ; Reason = trace_goal(_, _, _, _, _)
            ; Reason = loop_control(_, _, _)
            ),
            mark_tail_rec_calls_in_goal(SubGoal0, SubGoal, AtTail0, AtTail,
                !Info)
        ;
            ( Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ),
            unexpected($file, $pred, "unexpected scope kind")
        ),
        Goal = hlds_goal(scope(Reason, SubGoal), GoalInfo0)
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred mark_tail_rec_calls_in_plain_call(
    hlds_goal_expr::in(goal_expr_plain_call), hlds_goal_info::in,
    hlds_goal::out, at_tail_info::in, at_tail_info::out,
    mark_tail_rec_calls_info::in, mark_tail_rec_calls_info::out) is det.

mark_tail_rec_calls_in_plain_call(GoalExpr0, GoalInfo0, Goal,
        AtTail0, AtTail, !Info) :-
    GoalExpr0 = plain_call(CalleePredId, CalleeProcId, ArgVars, Builtin,
        _UnifyContext, _SymName),
    CalleePredProcId = proc(CalleePredId, CalleeProcId),
    CurPredProcId = !.Info ^ mtc_cur_proc,
    CurSCCPredProcIds = !.Info ^ mtc_cur_scc,
    ( if
        Builtin = not_builtin,
        ( if CalleePredProcId = CurPredProcId then
            SelfOrMutual = call_is_self_rec
        else if set.member(CalleePredProcId, CurSCCPredProcIds) then
            SelfOrMutual = call_is_mutual_rec
        else
            false
        )
    then
        !Info ^ mtc_any_rec_calls := found_any_rec_calls,
        AtTail0 = at_tail_info(Laters0, OutputVars),
        ( if
            set_tree234.is_empty(Laters0),
            require_det (
                ModuleInfo = !.Info ^ mtc_module,
                module_info_pred_info(ModuleInfo, CalleePredId,
                    CalleePredInfo),
                pred_info_get_arg_types(CalleePredInfo, CalleeArgTypes),
                pred_info_proc_info(CalleePredInfo, CalleeProcId,
                    CalleeProcInfo),
                proc_info_get_argmodes(CalleeProcInfo, CalleeArgModes),
                find_output_args(ModuleInfo, CalleeArgTypes, CalleeArgModes,
                    ArgVars, CalleeOutputVars)
            ),
            % For self-recursive calls, the caller and callee
            % will obviously have
            %
            % - the same number of arguments, and
            % - the same number of output arguments.
            %
            % Neither condition is a given for mutually recursive calls,
            % which is why we check only output arguments, not all arguments.
            %
            % For a recursive call (either self or mutual) to be a *tail* call,
            % the callee must have the same sequence of output arguments
            % (the same set of variables in the same order) as the caller.
            %
            % CalleeOutputVars is the sequence of output vars of this call;
            % OutputVars is the sequence of output vars of the caller,
            % updated to reflect any "renaming" done by assignment
            % unifications after the call. For example, if the first
            % output argument of the caller is X, but the call is followed
            % by the assignment X: = X0 (which our backwards traversal
            % will have already seen, and updated AtTail0 accordingly),
            % then this call cannot be a tail call unless its first output
            % argument is X0. (The call cannot output X, since X cannot
            % have two producers.)
            OutputVars = CalleeOutputVars
        then
            !.Info ^ mtc_params = tail_rec_params(
                MaybeSelfFeature, MaybeMutualFeature,
                MaybeRecord, _WarnParams),
            (
                SelfOrMutual = call_is_self_rec,
                (
                    MaybeSelfFeature = no,
                    Goal = hlds_goal(GoalExpr0, GoalInfo0)
                ;
                    MaybeSelfFeature = yes(SelfFeature),
                    goal_info_add_feature(SelfFeature, GoalInfo0, GoalInfo),
                    Goal = hlds_goal(GoalExpr0, GoalInfo)
                ),
                (
                    MaybeRecord = do_not_record_tail_recursion
                ;
                    MaybeRecord = record_tail_recursion,
                    !Info ^ mtc_self_tail_rec_calls := has_self_tail_rec_call
                )
            ;
                SelfOrMutual = call_is_mutual_rec,
                (
                    MaybeMutualFeature = no,
                    Goal = hlds_goal(GoalExpr0, GoalInfo0)
                ;
                    MaybeMutualFeature = yes(MutualFeature),
                    goal_info_add_feature(MutualFeature, GoalInfo0, GoalInfo),
                    Goal = hlds_goal(GoalExpr0, GoalInfo)
                ),
                (
                    MaybeRecord = do_not_record_tail_recursion
                ;
                    MaybeRecord = record_tail_recursion,
                    !Info ^ mtc_mutual_tail_rec_calls :=
                        has_mutual_tail_rec_call
                )
            )
        else
            ( if set_tree234.contains(Laters0, later_rec_call) then
                Obviousness = obvious_nontail_rec,
                % Record the obviousness for the MLDS code generator.
                goal_info_add_feature(feature_obvious_nontail_rec_call,
                    GoalInfo0, GoalInfo1)
            else
                Obviousness = non_obvious_nontail_rec,
                GoalInfo1 = GoalInfo0
            ),
            ModuleInfo = !.Info ^ mtc_module,
            CallerPredProcId = !.Info ^ mtc_cur_proc,
            Context = goal_info_get_context(GoalInfo0),
            WarnParams = !.Info ^ mtc_params ^ warn_params,
            Specs0 = !.Info ^ mtc_error_specs,
            Features = goal_info_get_features(GoalInfo0),
            maybe_report_nontail_recursive_call(ModuleInfo, WarnParams,
                CallerPredProcId, CalleePredProcId, Features, Context,
                ntrcr_program(Laters0), Obviousness, Specs0, Specs),
            !Info ^ mtc_error_specs := Specs,

            % Mark the goal so that the code generator, which lacks the info
            % from which we derive Laters0, does not generate a second,
            % slightly different, diagnostic for it.
            goal_info_add_feature(feature_non_tailrec_reported,
                GoalInfo1, GoalInfo),
            Goal = hlds_goal(GoalExpr0, GoalInfo)
        ),
        set_tree234.insert(later_rec_call, Laters0, Laters),
        AtTail = at_tail_info(Laters, OutputVars)
    else
        Goal = hlds_goal(GoalExpr0, GoalInfo0),
        add_later_op(later_nonrec_call, AtTail0, AtTail)
    ).

:- pred is_output_arg_rename(prog_var::in, prog_var::in,
    list(prog_var)::in, list(prog_var)::out) is semidet.

is_output_arg_rename(ToVar, FromVar, [Var0 | Vars0], [Var | Vars]) :-
    ( if ToVar = Var0 then
        % The assignment assigns FromVar to ToVar. Any tail recursive call
        % cannot assign to ToVar (since this assignment is the atomic goal
        % that produces ToVar); it will have to generate FromVar instead.
        Var = FromVar,
        Vars = Vars0
    else
        Var = Var0,
        is_output_arg_rename(ToVar, FromVar, Vars0, Vars)
    ).

:- pred mark_tail_rec_calls_in_rev_conj(
    list(hlds_goal)::in, list(hlds_goal)::out,
    at_tail_info::in, at_tail_info::out,
    mark_tail_rec_calls_info::in, mark_tail_rec_calls_info::out) is det.

mark_tail_rec_calls_in_rev_conj([], [], !AtTail, !Info).
mark_tail_rec_calls_in_rev_conj([RevGoal0 | RevGoals0], [RevGoal | RevGoals],
        !AtTail, !Info) :-
    mark_tail_rec_calls_in_goal(RevGoal0, RevGoal, !AtTail, !Info),
    mark_tail_rec_calls_in_rev_conj(RevGoals0, RevGoals, !AtTail, !Info).

:- pred mark_tail_rec_calls_in_nonlast_disjunct(at_tail_info::in,
    hlds_goal::in, hlds_goal::out,
    list(at_tail_info)::in, list(at_tail_info)::out,
    mark_tail_rec_calls_info::in, mark_tail_rec_calls_info::out) is det.

mark_tail_rec_calls_in_nonlast_disjunct(AtTail0, !Disjunct, !AtTails, !Info) :-
    mark_tail_rec_calls_in_goal(!Disjunct, AtTail0, AtTail, !Info),
    !:AtTails = [AtTail | !.AtTails].

:- pred mark_tail_rec_calls_in_case(at_tail_info::in, case::in, case::out,
    list(at_tail_info)::in, list(at_tail_info)::out,
    mark_tail_rec_calls_info::in, mark_tail_rec_calls_info::out) is det.

mark_tail_rec_calls_in_case(AtTail0, Case0, Case, !AtTails, !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    mark_tail_rec_calls_in_goal(Goal0, Goal, AtTail0, AtTail, !Info),
    !:AtTails = [AtTail | !.AtTails],
    Case = case(MainConsId, OtherConsIds, Goal).

%---------------------------------------------------------------------------%

:- pred add_later_op(later_op::in,
    at_tail_info::in, at_tail_info::out) is det.

add_later_op(Later, AtTail0, AtTail) :-
    AtTail0 = at_tail_info(Laters0, Outputs0),
    set_tree234.insert(Later, Laters0, Laters),
    AtTail  = at_tail_info(Laters, Outputs0).

    % Once we have finished the backwards traversal of every branch
    % in a branched control structure (disjunction, switch or if-then-else),
    % join together the at_tail_info structures we got at the end of
    % each traversal (which represent what we know about the program points
    % at the *starts* of those branches) to compute what we know about
    % the program point before the branched control structure itself.
    %
:- pred join_branch_at_tails(at_tail_info::in, list(at_tail_info)::in,
    at_tail_info::out) is det.

join_branch_at_tails(HeadAtTail, TailAtTails, AtTail) :-
    (
        TailAtTails = [],
        AtTail = HeadAtTail
    ;
        TailAtTails = [HeadTailAtTail | TailTailAtTails],
        HeadAtTail = at_tail_info(HeadLaters, HeadOutputArgs),
        HeadTailAtTail = at_tail_info(HeadTailLaters, _HeadTailOutputArgs),
        set_tree234.union(HeadLaters, HeadTailLaters, NextLaters),
        % NOTE HeadOutputArgs and _HeadTailOutputArgs may be different,
        % since different branches may compute the final output arguments
        % using different code. However, this does not matter, because
        %
        % - when we traverse backwards past any branched control structure,
        %   we add that fact to the later_ops set, and
        % - the resulting guaranteed-to-be-nonempty later_ops set
        %   implies that the second field of the resulting at_tail_info
        %   will not be used for anything.
        NextAtTail = at_tail_info(NextLaters, HeadOutputArgs),
        join_branch_at_tails(NextAtTail, TailTailAtTails, AtTail)
    ).

%---------------------------------------------------------------------------%

maybe_report_nontail_recursive_call(ModuleInfo, WarnParams,
        CallerPredProcId, CalleePredProcId, Features, Context,
        Reason, Obviousness, !Specs) :-
    WarnParams = warn_non_tail_rec_params(RequestBy, WarnOrError, Grades,
        WarnNonTailSelfRec, WarnNonTailMutualRec),
    ( if
        ( if CallerPredProcId = CalleePredProcId then
            WarnNonTailSelfRec = warn_non_tail_self_rec
        else
            WarnNonTailMutualRec = warn_non_tail_mutual_rec
        ),
        (
            Obviousness = non_obvious_nontail_rec
        ;
            Obviousness = obvious_nontail_rec,
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals,
                warn_obvious_non_tail_recursion, yes)
        ),
        (
            Grades = in_all_grades
        ;
            Grades = in_tailrec_grades_only,
            grade_supports_tail_recursion(ModuleInfo)
        ),
        not set.contains(Features, feature_non_tailrec_reported)
    then
        report_nontail_recursive_call(ModuleInfo,
            CallerPredProcId, CalleePredProcId, Context, Reason,
            RequestBy, WarnOrError, !Specs)
    else
        true
    ).

:- pred grade_supports_tail_recursion(module_info::in) is semidet.

grade_supports_tail_recursion(ModuleInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, profile_deep, no),
    globals.lookup_bool_option(Globals, exec_trace, no),
    globals.lookup_bool_option(Globals, source_to_source_debug, no),
    globals.lookup_bool_option(Globals, use_minimal_model_stack_copy, no),
    globals.lookup_bool_option(Globals, use_minimal_model_own_stacks, no),
    globals.get_gc_method(Globals, GC),
    GC \= gc_accurate.

:- pred report_nontail_recursive_call(module_info::in,
    pred_proc_id::in, pred_proc_id::in, prog_context::in,
    nontail_rec_call_reason::in, report_requested_by::in, warning_or_error::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_nontail_recursive_call(ModuleInfo, CallerPredProcId, CalleePredProcId,
        Context, Reason, RequestBy, WarnOrError, !Specs) :-
    CallerPredProcId = proc(CallerPredId, CallerProcId),
    module_info_pred_info(ModuleInfo, CallerPredId, CallerPredInfo),
    CallerPredOrFunc = pred_info_is_pred_or_func(CallerPredInfo),
    CallerName = pred_info_name(CallerPredInfo),
    CallerPredFormArity = pred_info_pred_form_arity(CallerPredInfo),
    CallerPFSNA = pf_sym_name_arity(CallerPredOrFunc, unqualified(CallerName),
        CallerPredFormArity),
    pred_info_get_proc_table(CallerPredInfo, CallerProcTable),
    map.count(CallerProcTable, CallerNumProcs),
    ( if CallerNumProcs > 1 then
        MaybeCallerProcId = yes(CallerProcId)
    else
        MaybeCallerProcId = no
    ),
    ( if CallerPredProcId = CalleePredProcId then
        add_message_for_nontail_self_recursive_call(CallerPFSNA,
            MaybeCallerProcId, Context, Reason, RequestBy, WarnOrError, !Specs)
    else
        CalleePredProcId = proc(CalleePredId, _),
        module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
        CalleePredOrFunc = pred_info_is_pred_or_func(CalleePredInfo),
        CalleeName = qualified(pred_info_module(CalleePredInfo),
            pred_info_name(CalleePredInfo)),
        CalleePredFormArity = pred_info_pred_form_arity(CalleePredInfo),
        CalleePFSNA = pf_sym_name_arity(CalleePredOrFunc, CalleeName,
            CalleePredFormArity),
        add_message_for_nontail_mutual_recursive_call(CallerPFSNA,
            MaybeCallerProcId, CalleePFSNA, Context, Reason,
            RequestBy, WarnOrError, !Specs)
    ).

%---------------------------------------------------------------------------%

    % add_message_for_nontail_self_recursive_call(PFSymNameArity,
    %   MaybeCallerProcId, Context, Reason, RequestBy, WarnOrError, !Specs):
    %
    % Add an error_spec to !Specs reporting that the recursive call inside
    % the procedure described by PFSymNameArity and CallerProcId (if specified)
    % at Context is not *tail* recursive. Set its severity based on
    % WarnOrError.
    %
:- pred add_message_for_nontail_self_recursive_call(pf_sym_name_arity::in,
    maybe(proc_id)::in, prog_context::in, nontail_rec_call_reason::in,
    report_requested_by::in, warning_or_error::in,
    list(error_spec)::in, list(error_spec)::out) is det.

add_message_for_nontail_self_recursive_call(CallerPFSNA, MaybeCallerProcId,
        Context, Reason, RequestBy, WarnOrError, !Specs) :-
    ( RequestBy = request_by_code,   Option = warn_requested_by_code
    ; RequestBy = request_by_option, Option = warn_non_tail_recursion_self
    ),
    nontail_rec_call_reason_to_pieces(Reason, Context,
        ReasonPieces, VerboseMsgs),
    woe_to_severity_and_string(Option, WarnOrError, Severity, WarnOrErrorWord),
    caller_proc_id_pieces(MaybeCallerProcId, ProcIdPieces),
    MainPieces = [WarnOrErrorWord, words("in")] ++ ProcIdPieces ++
        [unqual_pf_sym_name_pred_form_arity(CallerPFSNA), suffix(":"), nl,
        words("this")] ++ color_as_subject([words("self-recursive call")]) ++
        ReasonPieces,
    MainMsg = msg(Context, MainPieces),
    Spec = error_spec($pred, Severity, phase_code_gen,
        [MainMsg | VerboseMsgs]),
    !:Specs = [Spec | !.Specs].

    % add_message_for_nontail_mutual_recursive_call(CallerPFSNA,
    %   MaybeCallerProcId, CalleePFSNA, Context, Reason, RequestBy,
    %   WarnOrError, !Specs):
    %
    % Add an error_spec to !Specs reporting that the mutually recursive call
    % to CalleePFSNA inside the procedure described by CallerPFSNA and
    % CallerProcId (if specified) at Context is not *tail* recursive.
    % Set its severity based on WarnOrError.
    %
:- pred add_message_for_nontail_mutual_recursive_call(pf_sym_name_arity::in,
    maybe(proc_id)::in, pf_sym_name_arity::in, prog_context::in,
    nontail_rec_call_reason::in, report_requested_by::in, warning_or_error::in,
    list(error_spec)::in, list(error_spec)::out) is det.

add_message_for_nontail_mutual_recursive_call(CallerPFSNA, MaybeCallerProcId,
        CalleePFSNA, Context, Reason, RequestBy, WarnOrError, !Specs) :-
    ( RequestBy = request_by_code,   Option = warn_requested_by_code
    ; RequestBy = request_by_option, Option = warn_non_tail_recursion_mutual
    ),
    nontail_rec_call_reason_to_pieces(Reason, Context,
        ReasonPieces, VerboseMsgs),
    woe_to_severity_and_string(Option, WarnOrError, Severity, WarnOrErrorWord),
    caller_proc_id_pieces(MaybeCallerProcId, ProcIdPieces),
    MainPieces = [WarnOrErrorWord, words("in")] ++ ProcIdPieces ++
        [unqual_pf_sym_name_pred_form_arity(CallerPFSNA), suffix(":"), nl,
        words("this")] ++
        color_as_subject([words("mutually recursive call")]) ++
        [words("to"), unqual_pf_sym_name_pred_form_arity(CalleePFSNA)] ++
        ReasonPieces,
    MainMsg = msg(Context, MainPieces),
    Spec = error_spec($pred, Severity, phase_code_gen,
        [MainMsg | VerboseMsgs]),
    !:Specs = [Spec | !.Specs].

:- pred woe_to_severity_and_string(option::in, warning_or_error::in,
    spec_severity::out, format_piece::out) is det.

woe_to_severity_and_string(Option, WarnOrError, Severity, WarnOrErrorWord) :-
    (
        WarnOrError = we_warning,
        Severity = severity_warning(Option),
        WarnOrErrorWord = words("Warning")
    ;
        WarnOrError = we_error,
        Severity = severity_error,
        WarnOrErrorWord = words("Error")
    ).

:- pred caller_proc_id_pieces(maybe(proc_id)::in, list(format_piece)::out)
    is det.

caller_proc_id_pieces(MaybeCallerProcId, ProcIdPieces) :-
    (
        MaybeCallerProcId = no,
        ProcIdPieces = []
    ;
        MaybeCallerProcId = yes(CallerProcId),
        proc_id_to_int(CallerProcId, CallerProcNumber0),
        % Internally, proc_ids start at zero. For users, they start at one.
        CallerProcNumber = CallerProcNumber0 + 1,
        ProcIdPieces = [words("mode number"),
            int_fixed(CallerProcNumber), words("of")]
    ).

:- pred nontail_rec_call_reason_to_pieces(nontail_rec_call_reason::in,
    prog_context::in, list(format_piece)::out, list(diag_msg)::out) is det.

nontail_rec_call_reason_to_pieces(Reason, Context,
        ReasonPieces, VerboseMsgs) :-
    (
        Reason = ntrcr_program(LaterSet),
        set_tree234.to_sorted_list(LaterSet, Laters),
        list.map(warning_pieces_about_later_op, Laters, WarningPieceLists),
        list.condense(WarningPieceLists, WarningPieces),
        ReasonPieces = [words("is")] ++
            color_as_incorrect([words("not tail recursive.")]) ++ [nl] ++
            WarningPieces,
        VerboseMsgs = []
    ;
        Reason = ntrcr_mlds_in_scc_not_in_tscc,
        ReasonPieces = [words("is tail recursive, but")] ++
            color_as_incorrect([words("tail recursion optimization"),
                words("cannot be applied to it,")]) ++
            [words("because the callee cannot reach the caller"),
            words("via tail calls only."), nl],
        VerbosePieces = [words("With --high-level-code,"),
            words("the compiler can implement mutual tail recursion"),
            words("only for sets of procedures where"),
            words("every procedure in the set can reach")] ++
            color_as_hint([words("every")]) ++
            [words("other procedure in the set using")] ++
            color_as_hint([words("tail calls only.")]) ++ [nl],
        VerboseMsgs = [simple_msg(Context,
            [verbose_only(verbose_once, VerbosePieces)])]
    ;
        Reason = ntrcr_mlds_in_tscc_stack_ref,
        ReasonPieces = [words("is tail recursive, but")] ++
            color_as_incorrect([words("tail recursion optimization"),
                words("cannot be applied to it,")]) ++
            [words("because that would leave dangling stack references"),
            words("in the generated target language code."), nl],
        VerboseMsgs = []
    ;
        Reason = ntrcr_mlds_model_non_in_cont_func,
        ReasonPieces = [words("is tail recursive, but")] ++
            color_as_incorrect([words("tail recursion optimization"),
                words("cannot be applied to it,")]) ++
            [words("because it occurs after a choice point."), nl],
        VerboseMsgs = []
    ).

:- pred warning_pieces_about_later_op(later_op::in, list(format_piece)::out)
    is det.

warning_pieces_about_later_op(Later, Pieces) :-
    (
        ( Later = later_unify
        ; Later = later_unify_assign
        ; Later = later_nonrec_call
        ; Later = later_rec_call
        ; Later = later_disjunction
        ; Later = later_disjunction_fail
        ; Later = later_switch
        ; Later = later_ite
        ; Later = later_negation
        ; Later = later_next_disjunct
        ; Later = later_cond_end
        ; Later = later_negation_end
        ),
        % These operations should be visible to the user, so reporting them
        % would be much more likely to be clutter than useful.
        Pieces = []
    ;
        Later = later_par_join,
        Pieces = [words("This call is inside a parallel conjunction."),
            words("The code of each parallel conjunct is followed by code"),
            words("to wait for all the other conjuncts."), nl]
    ;
        Later = later_commit,
        Pieces = [words("This call is inside a scope"),
            words("that changes determinism."),
            words("The code inside such scopes is followed by code"),
            words("that manages this change."), nl]
    ;
        Later = later_trail_prune,
        Pieces = [words("This call is inside a disjunction"),
            words("that cannot succeed more than once."),
            words("In trailing grades, each disjunct in such disjunctions"),
            words("is followed by code to manage the trail."), nl]
    ).

%---------------------------------------------------------------------------%

    % Have we found any recursive calls so far?
    %
    % We use this to generate warnings about pragmas that intend to control
    % how recursive calls that are not *tail* recursive should be treated,
    % when the procedure they are about contains no recursive calls at all,
    % either self-recursive or (if we have SCC information) mutually recursive.
    %
:- type found_any_rec_calls
    --->    not_found_any_rec_calls
    ;       found_any_rec_calls.

    % maybe_report_no_tail_or_nontail_recursive_calls(PredInfo, ProcInfo,
    %   FoundAnyRecCalls, Context, !Specs):
    %
    % If FoundAnyRecCalls = not_found_any_rec_calls but ProcInfo says
    % that the procedure has a pragma about tail recursive calls on it,
    % then add a message to !Specs reporting that the procedure described by
    % PFSymNameArity contains no recursive calls at all, tail-recursive or
    % otherwise.
    %
:- pred maybe_report_no_tail_or_nontail_recursive_calls(pred_info::in,
    proc_info::in, found_any_rec_calls::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_report_no_tail_or_nontail_recursive_calls(PredInfo, ProcInfo,
        FoundAnyRecCalls, !Specs) :-
    (
        FoundAnyRecCalls = found_any_rec_calls
    ;
        FoundAnyRecCalls = not_found_any_rec_calls,
        proc_info_get_maybe_require_tailrec_info(ProcInfo,
            MaybeRequireTailRec),
        (
            MaybeRequireTailRec = no
        ;
            MaybeRequireTailRec = yes(RequireTailRecInfo),
            (
                RequireTailRecInfo = disable_nontailrec_reports(Context),
                % In the absence of any recursive calls,
                % the pragma that records disable_nontailrec_reports
                % is totally useless.
                WarnOrError = we_error
            ;
                RequireTailRecInfo = enable_nontailrec_reports(WarnOrError,
                    _, _, Context)
            ),
            pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
            pred_info_get_name(PredInfo, PredName),
            PredFormArity = pred_info_pred_form_arity(PredInfo),
            PFSymNameArity = pf_sym_name_arity(PredOrFunc,
                unqualified(PredName), PredFormArity),
            report_no_tail_or_nontail_recursive_calls(PFSymNameArity, Context,
                WarnOrError, warn_requested_by_code, !Specs)
        )
    ).

:- pred report_no_tail_or_nontail_recursive_calls(pf_sym_name_arity::in,
    prog_context::in, warning_or_error::in, option::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_no_tail_or_nontail_recursive_calls(PFSymNameArity, Context,
        WarnOrError, Option, !Specs) :-
    woe_to_severity_and_string(Option, WarnOrError, Severity, WarnOrErrorWord),
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, _, _),
    Pieces = [WarnOrErrorWord,
        words("in"), pragma_decl("require_tail_recursion"), words("for"),
        unqual_pf_sym_name_pred_form_arity(PFSymNameArity), suffix(":"), nl,
        words("the code defining this"), p_or_f(PredOrFunc),
        words("contains")] ++
        color_as_incorrect([words("no recursive calls at all,")]) ++
        [words("tail-recursive or otherwise."), nl],
    Spec = spec($pred, Severity, phase_code_gen, Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
:- end_module hlds.mark_tail_calls.
%---------------------------------------------------------------------------%
