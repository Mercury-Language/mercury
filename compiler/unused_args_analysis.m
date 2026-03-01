%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2003-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: unused_args_analysis.m.
%
% This module contains the interface between the unused args
% analysis and optimization pass on the one hand, and our intermodule
% analysis framework.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.unused_args_analysis.
:- interface.

:- import_module analysis.
:- import_module analysis.framework.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred record_analysis_unused_args(proc_to_unused_args_map::in,
    list(pred_proc_id)::in, module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%
% Instances used by mmc_analysis.m.
%

:- type unused_args_func_info
    --->    unused_args_func_info(pred_form_arity).

:- type unused_args_call
    --->    unused_args_call.

:- type unused_args_answer
    --->    unused_args_answer(
                % The list of unused arguments is in sorted order.
                args    :: list(int)
            ).

:- instance analysis(unused_args_func_info, unused_args_call,
    unused_args_answer).

:- instance partial_order(unused_args_func_info, unused_args_call).
:- instance call_pattern(unused_args_func_info, unused_args_call).
:- instance to_term(unused_args_call).

:- instance partial_order(unused_args_func_info, unused_args_answer).
:- instance answer_pattern(unused_args_func_info, unused_args_answer).
:- instance to_term(unused_args_answer).

%---------------------------------------------------------------------------%

:- func get_unused_args(unused_args_answer) = list(int).

:- func analysis_name = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.operations.
:- import_module hlds.goal_refs.
:- import_module hlds.hlds_goal.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module transform_hlds.mmc_analysis.

:- import_module map.
:- import_module set.
:- import_module term.
:- import_module term_context.
:- import_module term_conversion.

%---------------------------------------------------------------------------%

record_analysis_unused_args(ProcToUnusedArgsMap, FixpointPredProcIds,
        !ModuleInfo) :-
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo0),
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    list.foldl(
        maybe_record_intermod_unused_args(!.ModuleInfo, ProcToUnusedArgsMap),
        PredIds, AnalysisInfo0, AnalysisInfo1),
    list.foldl(record_intermod_dependencies(!.ModuleInfo),
        FixpointPredProcIds, AnalysisInfo1, AnalysisInfo),
    module_info_set_analysis_info(AnalysisInfo, !ModuleInfo).

%---------------------------------------------------------------------------%

:- pred maybe_record_intermod_unused_args(module_info::in,
    proc_to_unused_args_map::in, pred_id::in,
    analysis_info::in, analysis_info::out) is det.

maybe_record_intermod_unused_args(ModuleInfo, ProcToUnusedArgsMap, PredId,
        !AnalysisInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_all_procids(PredInfo),
    list.foldl(
        maybe_record_intermod_unused_args_for_proc(ModuleInfo,
            ProcToUnusedArgsMap, PredId, PredInfo),
        ProcIds, !AnalysisInfo).

:- pred maybe_record_intermod_unused_args_for_proc(module_info::in,
    proc_to_unused_args_map::in, pred_id::in, pred_info::in, proc_id::in,
    analysis_info::in, analysis_info::out) is det.

maybe_record_intermod_unused_args_for_proc(ModuleInfo, ProcToUnusedArgsMap,
        PredId, PredInfo, ProcId, !AnalysisInfo) :-
    ( if
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        not is_unify_index_or_compare_pred(PredInfo)
    then
        PPId = proc(PredId, ProcId),
        ( if map.search(ProcToUnusedArgsMap, PPId, UnusedArgs) then
            Answer = unused_args_answer(UnusedArgs)
        else
            Answer = unused_args_answer([])
        ),
        ppid_to_module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
        record_result(ModuleName, FuncId, unused_args_call, Answer, optimal,
            !AnalysisInfo)
    else
        true
    ).

%---------------------------------------------------------------------------%

    % If a procedure in this module calls a procedure from another module,
    % then we assume that this module depends on the analysis results of that
    % other procedure.
    %
    % This way of constructing the intermodule dependency graph is easier than
    % actually keeping track of which external analysis results we have used
    % in order to reach analysis results for this module.
    % It works because (1) we only have one type of call pattern so we don't
    % need to know which call patterns are used, and (2) we only record the
    % entire module as a dependency, so we don't have to know which exported
    % procedure is calling (directly or indirectly) which imported procedure.
    %
:- pred record_intermod_dependencies(module_info::in, pred_proc_id::in,
    analysis_info::in, analysis_info::out) is det.

record_intermod_dependencies(ModuleInfo, CallerPredProcId, !AnalysisInfo) :-
    module_info_pred_proc_info(ModuleInfo, CallerPredProcId,
        _CallerPredInfo, CallerProcInfo),
    proc_info_get_goal(CallerProcInfo, Goal),
    pred_proc_ids_called_from_goal(Goal, CalleePredProcIds),
    list.foldl(record_intermod_dependencies_for_ppid(ModuleInfo),
        CalleePredProcIds, !AnalysisInfo).

:- pred record_intermod_dependencies_for_ppid(module_info::in,
    pred_proc_id::in, analysis_info::in, analysis_info::out) is det.

record_intermod_dependencies_for_ppid(ModuleInfo, CalleePredProcId,
        !AnalysisInfo) :-
    CalleePredProcId = proc(CalleePredId, _),
    module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
    ( if
        pred_info_is_imported_not_external(CalleePredInfo),
        not is_unify_index_or_compare_pred(CalleePredInfo)
    then
        ppid_to_module_name_func_id(ModuleInfo, CalleePredProcId,
            CalleeModule, CalleeFuncId),
        Call = unused_args_call,
        Answer = _ : unused_args_answer,
        get_func_info(ModuleInfo, CalleeModule, CalleeFuncId, Call, Answer,
            FuncInfo),
        record_dependency(CalleeModule, CalleeFuncId, FuncInfo, Call, Answer,
            !AnalysisInfo)
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Types and instances used by mmc_analysis.m.
%

:- instance analysis(unused_args_func_info, unused_args_call,
    unused_args_answer) where
[
    analysis_name(_, _) = analysis_name,
    analysis_version_number(_, _) = 3,
    preferred_fixpoint_type(_, _) = least_fixpoint,
    bottom(unused_args_func_info(pred_form_arity(Arity)), _) =
        unused_args_answer(1 .. Arity),
    top(_, _) = unused_args_answer([]),
    (get_func_info(ModuleInfo, ModuleName, FuncId, _, _, FuncInfo) :-
        func_id_to_ppid(ModuleInfo, ModuleName, FuncId, proc(PredId, _)),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_orig_arity(PredInfo, PredFormArity),
        FuncInfo = unused_args_func_info(PredFormArity)
    )
].

:- instance call_pattern(unused_args_func_info, unused_args_call) where [].
:- instance partial_order(unused_args_func_info, unused_args_call) where [
    ( more_precise_than(_, _, _) :-
        semidet_fail
    ),
    equivalent(_, Call, Call)
].

:- instance to_term(unused_args_call) where [
    ( to_term(unused_args_call) = Term :-
        Term = term.functor(atom("any"), [], dummy_context)
    ),
    ( from_term(Term, unused_args_call) :-
        Term = term.functor(atom("any"), [], _)
    )
].

:- instance answer_pattern(unused_args_func_info, unused_args_answer) where [].
:- instance partial_order(unused_args_func_info, unused_args_answer) where [
    (more_precise_than(_, Answer1, Answer2) :-
        Answer1 = unused_args_answer(Args1),
        Answer2 = unused_args_answer(Args2),
        set.subset(sorted_list_to_set(Args2), sorted_list_to_set(Args1))
    ),
    equivalent(_, Args, Args)
].

:- instance to_term(unused_args_answer) where [
    ( to_term(unused_args_answer(Args)) = Term :-
        type_to_term(Args, Term)
    ),
    ( from_term(Term, unused_args_answer(Args)) :-
        term_to_type(Term, Args)
    )
].

get_unused_args(UnusedArgs) = UnusedArgs ^ args.

analysis_name = "unused_args".

%---------------------------------------------------------------------------%
:- end_module transform_hlds.unused_args_analysis.
%---------------------------------------------------------------------------%
