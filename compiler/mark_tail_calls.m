%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2008, 2010-2012 The University of Melbourne.
% Copyright (C) 2014-2017 The Mercury Team.
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
% and ml_call_gen.m), using the predicates defined here which are exported
% for this purpose.
%
%---------------------------------------------------------------------------%

:- module hlds.mark_tail_calls.
:- interface.

:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % Mark both self and mutual tail recursive calls in the module.
    %
    % Unlike the predicates below serving the LLDS code generator,
    % this predicate never generates any error messages, and it never
    % restricts its attention to only *self* tail recursive calls.
    %
:- pred mark_self_and_mutual_tail_rec_calls_in_module(hlds_dependency_info::in,
    module_info::in, module_info::out) is det.

%---------------------%

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

:- type maybe_warn_non_tail_self_rec
    --->    do_not_warn_non_tail_self_rec
    ;       warn_non_tail_self_rec.

:- type maybe_warn_non_tail_mutual_rec
    --->    do_not_warn_non_tail_mutual_rec
    ;       warn_non_tail_mutual_rec.

:- type warn_non_tail_rec_params
    --->    warn_non_tail_rec_params(
                warning_or_error,
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

:- type nontail_rec_call_reason
    --->    ntrcr_program
            % The call is not a tail call in the program.

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

:- type nontail_rec_obviousness
    --->    non_obvious_nontail_rec
    ;       obvious_nontail_rec.

:- pred maybe_report_nontail_recursive_call(module_info::in,
    pred_proc_id::in, pred_proc_id::in, prog_context::in,
    nontail_rec_call_reason::in, nontail_rec_obviousness::in,
    warn_non_tail_rec_params::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%
% These predicates are exported to the MLDS code generator; see the comment
% at the top of this module for the reason.
%

    % add_message_for_nontail_self_recursive_call(PFSymNameArity, ProcId,
    %    Context, WarnOrError, !Specs):
    %
    % Add an error_spec to !Specs reporting that the recursive call inside
    % the procedure described by PFSymNameArity and ProcId at Context
    % is not *tail* recursive. Set its severity based on WarnOrError.
    %
:- pred add_message_for_nontail_self_recursive_call(pf_sym_name_arity::in,
    proc_id::in, prog_context::in, nontail_rec_call_reason::in,
    warning_or_error::in, list(error_spec)::in, list(error_spec)::out) is det.

    % add_message_for_nontail_mutual_recursive_call(CallerCallId, CallerProcId,
    %    CalleeCallId, WarnOrError, Context, !Specs):
    %
    % Add an error_spec to !Specs reporting that the mutually recursive call
    % inside the procedure described by PFSymNameArity and ProcId at Context
    % is not *tail* recursive. Set its severity based on WarnOrError.
    %
:- pred add_message_for_nontail_mutual_recursive_call(pf_sym_name_arity::in,
    proc_id::in, pf_sym_name_arity::in, prog_context::in,
    nontail_rec_call_reason::in, warning_or_error::in,
    list(error_spec)::in, list(error_spec)::out) is det.

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

    % maybe_report_no_tail_or_nontail_recursive_calls(PredInfo, ProcInfo
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_top_functor.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.vartypes.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data_pragma.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

mark_self_and_mutual_tail_rec_calls_in_module(DepInfo, !ModuleInfo) :-
    MaybeSelfRec = yes(feature_self_or_mutual_tail_rec_call),
    MaybeMutualRec = yes(feature_self_or_mutual_tail_rec_call),
    Params = tail_rec_params(MaybeSelfRec, MaybeMutualRec,
        do_not_record_tail_recursion, no_warnings_non_tail_rec_params),
    get_bottom_up_sccs_with_entry_points(!.ModuleInfo, DepInfo,
        BottomUpSCCsEntryPoints),
    mark_tail_rec_calls_in_sccs(Params, BottomUpSCCsEntryPoints,
        !ModuleInfo, [], _Specs).

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
    mark_tail_rec_calls_in_sccs(Params, BottomUpSCCsEntryPoints,
        !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred mark_tail_rec_calls_in_sccs(tail_rec_params::in,
    list(scc_with_entry_points)::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_tail_rec_calls_in_sccs(_Params, [], !ModuleInfo, !Specs).
mark_tail_rec_calls_in_sccs(Params, [SCCEntry | SCCEntries],
        !ModuleInfo, !Specs) :-
    SCCEntry = scc_with_entry_points(SCC, _CalledFromHigherSCC, _Exported),
    mark_tail_rec_calls_in_scc(Params, SCC, set.to_sorted_list(SCC),
        !ModuleInfo, !Specs),
    mark_tail_rec_calls_in_sccs(Params, SCCEntries, !ModuleInfo, !Specs).

:- pred mark_tail_rec_calls_in_scc(tail_rec_params::in,
    set(pred_proc_id)::in, list(pred_proc_id)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_tail_rec_calls_in_scc(_Params, _SCC, [], !ModuleInfo, !Specs).
mark_tail_rec_calls_in_scc(Params, SCC, [PredProcId | PredProcIds],
        !ModuleInfo, !Specs) :-
    PredProcId = proc(PredId, ProcId),
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),

    maybe_override_params_for_proc(ProcInfo0, Params, ProcParams),

    do_mark_tail_rec_calls_in_proc(ProcParams, !.ModuleInfo, SCC,
        PredId, ProcId, PredInfo0, ProcInfo0, ProcInfo, WasProcChanged,
        [], ProcSpecs),
    !:Specs = ProcSpecs ++ !.Specs,

    (
        WasProcChanged = proc_was_not_changed
    ;
        WasProcChanged = proc_may_have_been_changed,
        map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
        pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
        map.det_update(PredId, PredInfo, PredTable0, PredTable),
        module_info_set_preds(PredTable, !ModuleInfo)
    ),

    mark_tail_rec_calls_in_scc(Params, SCC, PredProcIds, !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

mark_tail_rec_calls_in_pred_for_llds_code_gen(SCCMap, PredId, !ModuleInfo,
        !PredInfo, !Specs) :-
    % We don't update ModuleInfo. Nevertheless, the passes_aux traversal
    % that our caller uses to call us requires us to pass back a new
    % ModuleInfo, even though it will itself put the updated PredInfo
    % back into ModuleInfo.
    module_info_get_globals(!.ModuleInfo, Globals),
    get_params_for_llds_code_gen(Globals, Params),
    ProcIds = pred_info_valid_non_imported_procids(!.PredInfo),
    mark_tail_rec_calls_in_procs(Params, !.ModuleInfo, SCCMap,
        PredId, ProcIds, !PredInfo, !Specs).

:- pred mark_tail_rec_calls_in_procs(tail_rec_params::in,
    module_info::in, scc_map(pred_proc_id)::in, pred_id::in, list(proc_id)::in,
    pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

mark_tail_rec_calls_in_procs(_Params, _ModuleInfo, _SCCMap,
        _PredId, [], !PredInfo, !Specs).
mark_tail_rec_calls_in_procs(Params, ModuleInfo, SCCMap,
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
    mark_tail_rec_calls_in_procs(Params, ModuleInfo, SCCMap,
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

:- func no_warnings_non_tail_rec_params = warn_non_tail_rec_params.

no_warnings_non_tail_rec_params = Params :-
    % Since neither SelfRec nor MutualRec is set, the value of
    % WarnOrError does not matter.
    Params = warn_non_tail_rec_params(we_warning,
        do_not_warn_non_tail_self_rec, do_not_warn_non_tail_mutual_rec).

get_default_warn_parms(Globals, WarnNonTailRecParams) :-
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
    WarnNonTailRecParams = warn_non_tail_rec_params(we_warning,
        WarnNonTailSelfRecOpt, WarnNonTailMutualRecOpt).

maybe_override_warn_params_for_proc(ProcInfo, WarnParams, ProcWarnParams) :-
    proc_info_get_maybe_require_tailrec_info(ProcInfo, MaybeRequireTailRec),
    (
        MaybeRequireTailRec = no,
        ProcWarnParams = WarnParams
    ;
        MaybeRequireTailRec = yes(Pragma),
        (
            Pragma = suppress_tailrec_warnings(_),
            ProcWarnParams = no_warnings_non_tail_rec_params
        ;
            Pragma = enable_tailrec_warnings(WarnOrError, RecType, _Context),
            (
                RecType = only_self_recursion_must_be_tail,
                SelfRec = warn_non_tail_self_rec,
                MutualRec = do_not_warn_non_tail_mutual_rec
            ;
                RecType = both_self_and_mutual_recursion_must_be_tail,
                SelfRec = warn_non_tail_self_rec,
                MutualRec = warn_non_tail_mutual_rec
            ),
            ProcWarnParams =
                warn_non_tail_rec_params(WarnOrError, SelfRec, MutualRec)
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
            WarnNonTailRecParams = warn_non_tail_rec_params(_WarnOrError,
                do_not_warn_non_tail_self_rec, do_not_warn_non_tail_mutual_rec)
        then
            WasProcChanged = proc_was_not_changed
        else
            pred_info_get_arg_types(PredInfo, Types),
            proc_info_get_goal(!.ProcInfo, Goal0),
            proc_info_get_argmodes(!.ProcInfo, Modes),
            proc_info_get_headvars(!.ProcInfo, HeadVars),
            proc_info_get_vartypes(!.ProcInfo, VarTypes),
            find_output_args(ModuleInfo, Types, Modes, HeadVars, Outputs),

            Info0 = mark_tail_rec_calls_info(ModuleInfo, PredInfo,
                proc(PredId, ProcId), SCC, VarTypes, Params,
                has_no_self_tail_rec_call, has_no_mutual_tail_rec_call,
                not_found_any_rec_calls, []),
            mark_tail_rec_calls_in_goal(Goal0, Goal, at_tail(Outputs), _,
                Info0, Info),
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

    % Is the current position within the procedure a tail position?
    % If it is, what are the output arguments?
    %
:- type at_tail
    --->    at_tail(list(prog_var))
    ;       not_at_tail(later_rec_call).

:- type later_rec_call
    --->    have_seen_later_rec_call
    ;       have_not_seen_later_rec_call.

:- type call_is_self_or_mutual_rec
    --->    call_is_self_rec
    ;       call_is_mutual_rec.

:- type mark_tail_rec_calls_info
    --->    mark_tail_rec_calls_info(
                mtc_module                  :: module_info,
                mtc_pred_info               :: pred_info,
                mtc_cur_proc                :: pred_proc_id,
                mtc_cur_scc                 :: set(pred_proc_id),
                mtc_vartypes                :: vartypes,
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
    at_tail::in, at_tail::out,
    mark_tail_rec_calls_info::in, mark_tail_rec_calls_info::out) is det.

mark_tail_rec_calls_in_goal(Goal0, Goal, AtTail0, AtTail, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        % Note: we don't give tailcall warnings for negated goals, maybe we
        % should?
        ; GoalExpr0 = negation(_)
        ),
        Goal = Goal0,
        not_at_tail(AtTail0, AtTail)
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
                    no_warnings_non_tail_rec_params,
                InnerInfo0 = !.Info ^ mtc_params := InnerParams,
                mark_tail_rec_calls_in_goal(SubGoal0, SubGoal, AtTail0, AtTail,
                    InnerInfo0, InnerInfo),
                !:Info = InnerInfo ^ mtc_params := OldParams
            else
                mark_tail_rec_calls_in_goal(SubGoal0, SubGoal, AtTail0, AtTail,
                    !Info)
            )
        ;
            ( Reason = exist_quant(_)
            ; Reason = promise_solutions(_, _)
            ; Reason = commit(_)
            ),
            not_at_tail(AtTail0, AtTail1),
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
        GoalExpr0 = unify(LHS, _, _, Unify0, _),
        Goal = Goal0,
        ModuleInfo = !.Info ^ mtc_module,
        VarTypes = !.Info ^ mtc_vartypes,
        ( if var_is_of_dummy_type(ModuleInfo, VarTypes, LHS) then
            % Unifications involving dummy type variables are no-ops,
            % and do not inhibit a preceding tail call.
            AtTail = AtTail0
        else
            (
                ( Unify0 = construct(_, _, _, _, _, _, _)
                ; Unify0 = deconstruct(_, _, _, _, _, _)
                ; Unify0 = simple_test(_, _)
                ; Unify0 = complicated_unify(_, _, _)
                ),
                not_at_tail(AtTail0, AtTail)
            ;
                Unify0 = assign(ToVar, FromVar),
                ( if
                    AtTail0 = at_tail(Outputs0),
                    is_output_arg_rename(ToVar, FromVar, Outputs0, Outputs)
                then
                    AtTail = at_tail(Outputs)
                else
                    AtTail = not_at_tail(have_not_seen_later_rec_call)
                )
            )
        )
    ;
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
            ( if
                AtTail0 = at_tail(OutputVars),
                require_det (
                    ModuleInfo = !.Info ^ mtc_module,
                    module_info_pred_info(ModuleInfo, CalleePredId,
                        CalleePredInfo),
                    pred_info_get_arg_types(CalleePredInfo, CalleeArgTypes),
                    pred_info_proc_info(CalleePredInfo, CalleeProcId,
                        CalleeProcInfo),
                    proc_info_get_argmodes(CalleeProcInfo, CalleeArgModes),
                    find_output_args(ModuleInfo,
                        CalleeArgTypes, CalleeArgModes, ArgVars,
                        CalleeOutputVars)
                ),

                % For self-recursive calls, the caller and callee
                % will obviously have
                %
                % - the same number of arguments, and
                % - the same number of output arguments.
                %
                % Neither condition is a given for mutually recursive calls,
                % which is why we check only output arguments, not all
                % arguments.
                %
                % For a recursive call (either self or mutual) to be a *tail*
                % call, the callee must have the same sequence of output
                % arguments (the same set of variables in the same order)
                % as the caller.
                %
                % CalleeOutputVars is the sequence of output vars of this call;
                % OutputVars is the sequence of output vars of the caller,
                % updated to reflect any "renaming" done by assigment
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
                        Goal = Goal0
                    ;
                        MaybeSelfFeature = yes(SelfFeature),
                        goal_info_add_feature(SelfFeature,
                            GoalInfo0, GoalInfo),
                        Goal = hlds_goal(GoalExpr0, GoalInfo)
                    ),
                    (
                        MaybeRecord = do_not_record_tail_recursion
                    ;
                        MaybeRecord = record_tail_recursion,
                        !Info ^ mtc_self_tail_rec_calls :=
                            has_self_tail_rec_call
                    )
                ;
                    SelfOrMutual = call_is_mutual_rec,
                    (
                        MaybeMutualFeature = no,
                        Goal = Goal0
                    ;
                        MaybeMutualFeature = yes(MutualFeature),
                        goal_info_add_feature(MutualFeature,
                            GoalInfo0, GoalInfo),
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
                (
                    ( AtTail0 = at_tail(_)
                    ; AtTail0 = not_at_tail(have_not_seen_later_rec_call)
                    ),
                    Obviousness = non_obvious_nontail_rec,
                    Goal = Goal0
                ;
                    AtTail0 = not_at_tail(have_seen_later_rec_call),
                    Obviousness = obvious_nontail_rec,
                    % Record the obviousness for the MLDS code generator.
                    goal_info_add_feature(feature_obvious_nontail_rec_call,
                        GoalInfo0, GoalInfo),
                    Goal = hlds_goal(GoalExpr0, GoalInfo)
                ),
                ModuleInfo = !.Info ^ mtc_module,
                CallerPredProcId = !.Info ^ mtc_cur_proc,
                Context = goal_info_get_context(GoalInfo0),
                WarnParams = !.Info ^ mtc_params ^ warn_params,
                Specs0 = !.Info ^ mtc_error_specs,
                maybe_report_nontail_recursive_call(ModuleInfo,
                    CallerPredProcId, CalleePredProcId, Context,
                    ntrcr_program, Obviousness, WarnParams, Specs0, Specs),
                !Info ^ mtc_error_specs := Specs
            ),
            AtTail = not_at_tail(have_seen_later_rec_call)
        else
            Goal = Goal0,
            not_at_tail(AtTail0, AtTail)
        )
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
            not_at_tail(AtTail0, AtTail1)
        ),
        list.reverse(Goals0, RevGoals0),
        mark_tail_rec_calls_in_conj(RevGoals0, RevGoals, AtTail1, AtTail,
            !Info),
        list.reverse(RevGoals, Goals),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Disjuncts0),
        ( if list.split_last(Disjuncts0, NonLastDisjuncts0, LastDisjunct0) then
            % If the disjunction is in tail position, then it is possible
            % for a goal inside the last disjunct to be a tail call.
            mark_tail_rec_calls_in_goal(LastDisjunct0, LastDisjunct,
                AtTail0, LastAtTail, !Info),
            % Even if the disjunction as a whole is in tail position,
            % a goal inside a nonlast disjunct cannot be a tail call,
            % because if it fails, its execution will be followed
            % by backtracking to later disjuncts.
            project_seen_later_rec_call(LastAtTail, SeenLaterRecCall0),
            NonLastAtTail0 = not_at_tail(SeenLaterRecCall0),
            list.map_foldl2(
                mark_tail_rec_calls_in_nonlast_disjunct(NonLastAtTail0),
                NonLastDisjuncts0, NonLastDisjuncts,
                SeenLaterRecCall0, SeenLaterRecCall, !Info),
            AtTail = not_at_tail(SeenLaterRecCall),
            GoalExpr = disj(NonLastDisjuncts ++ [LastDisjunct]),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        else
            % There are no disjuncts. Any goals before the disjunction
            % will be followed by disj([]), which is `fail', so they cannot
            % be tail calls.
            project_seen_later_rec_call(AtTail0, SeenLaterRecCall),
            AtTail = not_at_tail(SeenLaterRecCall),
            Goal = Goal0
        )
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        project_seen_later_rec_call(AtTail0, SeenLaterRecCall0),
        list.map_foldl2(mark_tail_rec_calls_in_case(AtTail0), Cases0, Cases,
            SeenLaterRecCall0, SeenLaterRecCall, !Info),
        AtTail = not_at_tail(SeenLaterRecCall),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        mark_tail_rec_calls_in_goal(Then0, Then, AtTail0, AtTailBeforeThen,
            !Info),
        mark_tail_rec_calls_in_goal(Else0, Else, AtTail0, AtTailBeforeElse,
            !Info),
        project_seen_later_rec_call(AtTailBeforeThen, SeenRecCallInThen),
        project_seen_later_rec_call(AtTailBeforeElse, SeenRecCallInElse),
        ( if
            ( SeenRecCallInThen = have_seen_later_rec_call
            ; SeenRecCallInElse = have_seen_later_rec_call
            )
        then
            SeenRecCallAfterCond = have_seen_later_rec_call
        else
            SeenRecCallAfterCond = have_not_seen_later_rec_call
        ),
        AtTailAfterCond = not_at_tail(SeenRecCallAfterCond),
        mark_tail_rec_calls_in_goal(Cond0, Cond, AtTailAfterCond, AtTail,
            !Info),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
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

:- pred mark_tail_rec_calls_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    at_tail::in, at_tail::out,
    mark_tail_rec_calls_info::in, mark_tail_rec_calls_info::out) is det.

mark_tail_rec_calls_in_conj([], [], !AtTail, !Info).
mark_tail_rec_calls_in_conj([RevGoal0 | RevGoals0], [RevGoal | RevGoals],
        !AtTail, !Info) :-
    mark_tail_rec_calls_in_goal(RevGoal0, RevGoal, !AtTail, !Info),
    mark_tail_rec_calls_in_conj(RevGoals0, RevGoals, !AtTail, !Info).

:- pred mark_tail_rec_calls_in_nonlast_disjunct(at_tail::in,
    hlds_goal::in, hlds_goal::out, later_rec_call::in, later_rec_call::out,
    mark_tail_rec_calls_info::in, mark_tail_rec_calls_info::out) is det.

mark_tail_rec_calls_in_nonlast_disjunct(AtTail0, !Disjunct,
        !SeenLaterRecCall, !Info) :-
    mark_tail_rec_calls_in_goal(!Disjunct, AtTail0, AtTail, !Info),
    accumulate_seen_later_rec_call(AtTail, !SeenLaterRecCall).

:- pred mark_tail_rec_calls_in_case(at_tail::in, case::in, case::out,
    later_rec_call::in, later_rec_call::out,
    mark_tail_rec_calls_info::in, mark_tail_rec_calls_info::out) is det.

mark_tail_rec_calls_in_case(AtTail0, Case0, Case, !SeenLaterRecCall, !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    mark_tail_rec_calls_in_goal(Goal0, Goal, AtTail0, AtTail, !Info),
    accumulate_seen_later_rec_call(AtTail, !SeenLaterRecCall),
    Case = case(MainConsId, OtherConsIds, Goal).

:- pred accumulate_seen_later_rec_call(at_tail::in,
    later_rec_call::in, later_rec_call::out) is det.

accumulate_seen_later_rec_call(AtTail, !SeenLaterRecCall) :-
    (
        AtTail = at_tail(_)
    ;
        AtTail = not_at_tail(AtTailSeenLaterRecCall),
        (
            AtTailSeenLaterRecCall = have_not_seen_later_rec_call
        ;
            AtTailSeenLaterRecCall = have_seen_later_rec_call,
            !:SeenLaterRecCall = have_seen_later_rec_call
        )
    ).

:- pred project_seen_later_rec_call(at_tail::in, later_rec_call::out) is det.

project_seen_later_rec_call(AtTail, SeenLaterRecCall) :-
    (
        AtTail = at_tail(_),
        SeenLaterRecCall = have_not_seen_later_rec_call
    ;
        AtTail = not_at_tail(SeenLaterRecCall)
    ).

:- pred not_at_tail(at_tail::in, at_tail::out) is det.

not_at_tail(Before, After) :-
    (
        Before = at_tail(_),
        After = not_at_tail(have_not_seen_later_rec_call)
    ;
        Before = not_at_tail(_),
        After = Before
    ).

%---------------------------------------------------------------------------%

maybe_report_nontail_recursive_call(ModuleInfo,
        CallerPredProcId, CalleePredProcId, Context, Reason, Obviousness,
        WarnParams, !Specs) :-
    WarnParams = warn_non_tail_rec_params(WarnOrError,
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
        )
    then
        report_nontail_recursive_call(ModuleInfo,
            CallerPredProcId, CalleePredProcId, Context, Reason, WarnOrError,
            !Specs)
    else
        true
    ).

:- pred report_nontail_recursive_call(module_info::in,
    pred_proc_id::in, pred_proc_id::in, prog_context::in,
    nontail_rec_call_reason::in, warning_or_error::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_nontail_recursive_call(ModuleInfo, CallerPredProcId, CalleePredProcId,
        Context, Reason, WarnOrError, !Specs) :-
    CallerPredProcId = proc(CallerPredId, CallerProcId),
    module_info_pred_info(ModuleInfo, CallerPredId, CallerPredInfo),
    CallerPredOrFunc = pred_info_is_pred_or_func(CallerPredInfo),
    CallerName = pred_info_name(CallerPredInfo),
    CallerArity = pred_info_orig_arity(CallerPredInfo),
    CallerId = pf_sym_name_arity(CallerPredOrFunc, unqualified(CallerName),
        CallerArity),
    ( if CallerPredProcId = CalleePredProcId then
        add_message_for_nontail_self_recursive_call(CallerId, CallerProcId,
            Context, Reason, WarnOrError, !Specs)
    else
        CalleePredProcId = proc(CalleePredId, _),
        module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
        CalleePredOrFunc = pred_info_is_pred_or_func(CalleePredInfo),
        CalleeName = qualified(pred_info_module(CalleePredInfo),
            pred_info_name(CalleePredInfo)),
        CalleeArity = pred_info_orig_arity(CalleePredInfo),
        CalleeId =
            pf_sym_name_arity(CalleePredOrFunc, CalleeName, CalleeArity),
        add_message_for_nontail_mutual_recursive_call(CallerId,
            CallerProcId, CalleeId, Context, Reason, WarnOrError, !Specs)
    ).

%---------------------------------------------------------------------------%

add_message_for_nontail_self_recursive_call(PFSymNameArity, ProcId, Context,
        Reason, WarnOrError, !Specs) :-
    nontail_rec_call_reason_to_pieces(Reason, Context,
        ReasonPieces, VerboseMsgs),
    woe_to_severity_and_string(WarnOrError, Severity, WarnOrErrorWord),
    proc_id_to_int(ProcId, ProcNumber0),
    ProcNumber = ProcNumber0 + 1,
    MainPieces = [words("In mode number"), int_fixed(ProcNumber),
        words("of"), unqual_pf_sym_name_orig_arity(PFSymNameArity),
        suffix(":"), nl,
        WarnOrErrorWord, words("self-recursive call")] ++ ReasonPieces,
    MainMsg = simplest_msg(Context, MainPieces),
    Spec = error_spec($pred, Severity, phase_code_gen,
        [MainMsg | VerboseMsgs]),
    !:Specs = [Spec | !.Specs].

add_message_for_nontail_mutual_recursive_call(CallerId, CallerProcId,
        CalleeId, Context, Reason, WarnOrError, !Specs) :-
    nontail_rec_call_reason_to_pieces(Reason, Context,
        ReasonPieces, VerboseMsgs),
    woe_to_severity_and_string(WarnOrError, Severity, WarnOrErrorWord),
    proc_id_to_int(CallerProcId, ProcNumber0),
    ProcNumber = ProcNumber0 + 1,
    MainPieces = [words("In mode number"), int_fixed(ProcNumber), words("of"),
        unqual_pf_sym_name_orig_arity(CallerId), suffix(":"), nl,
        WarnOrErrorWord, words("mutually recursive call to"),
        unqual_pf_sym_name_orig_arity(CalleeId)] ++ ReasonPieces,
    MainMsg = simplest_msg(Context, MainPieces),
    Spec = error_spec($pred, Severity, phase_code_gen,
        [MainMsg | VerboseMsgs]),
    !:Specs = [Spec | !.Specs].

:- pred woe_to_severity_and_string(warning_or_error::in,
    error_severity::out, format_component::out) is det.

woe_to_severity_and_string(we_warning, severity_warning, words("warning:")).
woe_to_severity_and_string(we_error, severity_error, words("error:")).

:- pred nontail_rec_call_reason_to_pieces(nontail_rec_call_reason::in,
    prog_context::in, list(format_component)::out, list(error_msg)::out)
    is det.

nontail_rec_call_reason_to_pieces(Reason, Context,
        ReasonPieces, VerboseMsgs) :-
    (
        Reason = ntrcr_program,
        ReasonPieces = [words("is not tail recursive."), nl],
        VerboseMsgs = []
    ;
        Reason = ntrcr_mlds_in_scc_not_in_tscc,
        ReasonPieces = [words("is tail recursive,"),
            words("but tail recursion optimization cannot be applied to it,"),
            words("because the *callee* cannot reach the caller"),
            words("via tail calls only."), nl],
        VerbosePieces = [words("The MLDS backend"),
            words("can optimize only *mutual* tail recursion;"),
            words("it cannot optimize tail recursion"),
            words("if it goes only one way between two procedures."), nl],
        VerboseMsgs = [simple_msg(Context,
            [verbose_only(verbose_once, VerbosePieces)])]
    ;
        Reason = ntrcr_mlds_in_tscc_stack_ref,
        ReasonPieces = [words("is tail recursive,"),
            words("but tail recursion optimization cannot be applied to it,"),
            words("because that would leave dangling stack references"),
            words("in the generated target language code."), nl],
        VerboseMsgs = []
    ;
        Reason = ntrcr_mlds_model_non_in_cont_func,
        ReasonPieces = [words("is tail recursive,"),
            words("but tail recursion optimization cannot be applied to it,"),
            words("because it occurs after a choice point."), nl],
        VerboseMsgs = []
    ).

%---------------------------------------------------------------------------%

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
            ( RequireTailRecInfo = enable_tailrec_warnings(_, _, Context)
            ; RequireTailRecInfo = suppress_tailrec_warnings(Context)
            ),
            pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
            pred_info_get_name(PredInfo, PredName),
            pred_info_get_orig_arity(PredInfo, PredArity),
            PFSymNameArity = pf_sym_name_arity(PredOrFunc,
                unqualified(PredName), PredArity),
            report_no_tail_or_nontail_recursive_calls(PFSymNameArity, Context,
                !Specs)
        )
    ).

:- pred report_no_tail_or_nontail_recursive_calls(pf_sym_name_arity::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

report_no_tail_or_nontail_recursive_calls(PFSymNameArity, Context, !Specs) :-
    PFSymNameArity = pf_sym_name_arity(PredOrFunc, _, _),
    Pieces = [words("In"), pragma_decl("require_tail_recursion"), words("for"),
        unqual_pf_sym_name_orig_arity(PFSymNameArity), suffix(":"), nl,
        words("warning: the code defining this"), p_or_f(PredOrFunc),
        words("contains no recursive calls at all,"),
        words("tail-recursive or otherwise."), nl],
    Spec = simplest_spec($pred, severity_warning, phase_code_gen,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
:- end_module hlds.mark_tail_calls.
%---------------------------------------------------------------------------%
