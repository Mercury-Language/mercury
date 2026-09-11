%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2011 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mercury_compile_simplify.m.
%
% This module implements the interface between compiler's top level
% (mainly the front-end, but also the back-end) and the simplification pass.
%
%---------------------------------------------------------------------------%

:- module top_level.mercury_compile_simplify.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % This type indicates what stage of compilation we are running
    % the simplification pass at. The exact simplifications tasks we run
    % will depend upon this.
    %
:- type simplify_pass
    --->    simplify_pass_frontend
            % Running simplification immediately after all the semantic
            % analysis passes in the frontend. As of 2025 sep 13,
            % this is stage 65.

    ;       simplify_pass_post_untuple
            % Running simplification after the untupling transformation
            % has been applied. As of 2025 sep 13, this is stage 133.

    ;       simplify_pass_pre_implicit_parallelism
            % Running simplification before the implicit parallelism
            % transformation, if it is enabled. This helps ensure that
            % the HLDS matches the feedback data. As of 2025 sep 13,
            % this is stage 172.

    ;       simplify_pass_pre_prof_transforms
            % Running simplification before either the term size profiling
            % or the profiling transformation, if either is enabled.
            % The reason for this pass is mainly to make up for the fact
            % that some simplifications that we can do using standard code
            % before these transforms would require more complex analysis,
            % if they were possible at all, after these transforms,
            % due to their introduction of e.g. impure code into the HLDS.
            % As of 2025 sep 13, this is stage 215.

    ;       simplify_pass_ll_backend
            % One of the passes of the LLDS backend. As of 2025 sep 13,
            % this is stage 325.

    ;       simplify_pass_ml_backend.
            % The first pass of the MLDS backend. As of 2025 sep 13,
            % this is stage 405.

    % This predicate sets up and maybe runs the simplification pass.
    %
:- pred maybe_simplify(io.text_output_stream::in,
    maybe(io.text_output_stream)::in, bool::in, simplify_pass::in,
    bool::in, bool::in, module_info::in, module_info::out,
    maybe_written_specs::in, maybe_written_specs::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.simplify.
:- import_module check_hlds.simplify.simplify_proc.
:- import_module check_hlds.simplify.simplify_tasks.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_pred_tests.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_proc_id.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.write_error_spec.

:- import_module list.
:- import_module map.
:- import_module require.

%---------------------------------------------------------------------------%

maybe_simplify(ProgressStream, MaybeErrorStream, Warn, SimplifyPass,
        Verbose, Stats, !HLDS, !MaybeWrittenSpecs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    some [!SimpList] (
        ( Warn = no,  WarnGen = do_not_generate_warnings
        ; Warn = yes, WarnGen = generate_warnings
        ),
        find_simplify_tasks(Globals, WarnGen, SimplifyTasks0),
        !:SimpList = simplify_tasks_to_list(SimplifyTasks0),
        (
            SimplifyPass = simplify_pass_frontend,
            list.cons(simptask_after_front_end, !SimpList),
            list.cons(simptask_try_opt_const_structs, !SimpList),
            globals.lookup_accumulating_option(Globals, dump_hlds,
                DumpHLDSStages),
            (
                DumpHLDSStages = []
            ;
                DumpHLDSStages = [_ | _],
                % This makes HLDS dumps both smaller and more readable
                % (by reducing clutter).
                list.cons(simptask_delete_dead_vars, !SimpList)
            )
        ;
            SimplifyPass = simplify_pass_post_untuple,
            list.cons(simptask_mark_code_model_changes, !SimpList)
        ;
            SimplifyPass = simplify_pass_pre_prof_transforms,

            % We run the simplify pass before the profiling transformations
            % only if those transformations are being applied; otherwise we
            % just leave things to the backend simplification passes.

            globals.lookup_bool_option(Globals, pre_prof_transforms_simplify,
                PreProfSimplify),
            (
                PreProfSimplify = yes,
                list.cons(simptask_mark_code_model_changes, !SimpList)
            ;
                PreProfSimplify = no,
                !:SimpList = []
            )
        ;
            SimplifyPass = simplify_pass_pre_implicit_parallelism,

            % We run the simplify pass before the implicit parallelism pass if
            % implicit parallelism is enabled.

            globals.lookup_bool_option(Globals,
                pre_implicit_parallelism_simplify, PreParSimplify),
            (
                PreParSimplify = yes,
                list.cons(simptask_mark_code_model_changes, !SimpList)
            ;
                PreParSimplify = no,
                !:SimpList = []
            )
        ;
            SimplifyPass = simplify_pass_ml_backend,
            list.cons(simptask_mark_code_model_changes, !SimpList)
        ;
            SimplifyPass = simplify_pass_ll_backend,
            % Don't perform constant propagation if one of the
            % profiling transformations has been applied.
            %
            % NOTE: Any changes made here may also need to be made
            % to the relevant parts of backend_pass_by_preds_4/12.
            globals.get_opt_tuple(Globals, OptTuple),
            ConstProp = OptTuple ^ ot_prop_constants,
            globals.lookup_bool_option(Globals, profile_deep, DeepProf),
            globals.lookup_bool_option(Globals, record_term_sizes_as_words,
                TSWProf),
            globals.lookup_bool_option(Globals, record_term_sizes_as_cells,
                TSCProf),
            ( if
                ConstProp = prop_constants,
                DeepProf = no,
                TSWProf = no,
                TSCProf = no
            then
                list.cons(simptask_constant_prop, !SimpList)
            else
                list.delete_all(!.SimpList, simptask_constant_prop, !:SimpList)
            ),
            list.cons(simptask_mark_code_model_changes, !SimpList),
            list.cons(simptask_elim_removable_scopes, !SimpList)
        ),
        SimpList = !.SimpList
    ),
    (
        SimpList = [_ | _],
        (
            MaybeErrorStream = yes(ErrorStreamA),
            maybe_write_not_yet_written_specs(ErrorStreamA, Globals, Verbose,
                !MaybeWrittenSpecs, !IO)
        ;
            MaybeErrorStream = no
        ),
        maybe_write_string(ProgressStream, Verbose,
            "% Simplifying goals...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        SimplifyTasks = list_to_simplify_tasks(Globals, SimpList),
        process_valid_nonimported_preds_errors(
            update_pred_error(simplify_pred(ProgressStream, SimplifyTasks)),
            !HLDS, [], SimplifySpecs),
        (
            SimplifyPass = simplify_pass_frontend,
            (
                MaybeErrorStream = yes(ErrorStreamB),
                add_to_be_written_specs(SimplifySpecs, !MaybeWrittenSpecs),
                maybe_write_not_yet_written_specs(ErrorStreamB,
                    Globals, Verbose, !MaybeWrittenSpecs, !IO)
            ;
                MaybeErrorStream = no,
                expect(unify(SimplifySpecs, []), $pred, "SimplifySpecs != []")
            )
        ;
            ( SimplifyPass = simplify_pass_ll_backend
            ; SimplifyPass = simplify_pass_ml_backend
            ; SimplifyPass = simplify_pass_post_untuple
            ; SimplifyPass = simplify_pass_pre_prof_transforms
            ; SimplifyPass = simplify_pass_pre_implicit_parallelism
            )
        ),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        SimpList = []
    ).

:- pred simplify_pred(io.text_output_stream::in,
    simplify_tasks::in, pred_id::in,
    module_info::in, module_info::out, pred_info::in, pred_info::out,
    list(diag_spec)::in, list(diag_spec)::out) is det.

simplify_pred(ProgressStream, SimplifyTasks0, PredId,
        !ModuleInfo, !PredInfo, !Specs) :-
    trace [io(!IO)] (
        maybe_write_pred_progress_message(ProgressStream, !.ModuleInfo,
            "Simplifying", PredId, !IO)
    ),
    ProcIds = pred_info_will_codegen_proc_ids(!.PredInfo),
    % Don't warn for compiler-generated procedures.
    ( if is_unify_index_or_compare_pred(!.PredInfo) then
        SimplifyTasks = SimplifyTasks0 ^ do_warn_dodgy_simple_code
            := do_not_warn_dodgy_simple_code
    else
        SimplifyTasks = SimplifyTasks0
    ),
    PredSpecsAcc0 = init_diag_spec_accumulator,
    simplify_pred_procs(ProgressStream, SimplifyTasks, PredId, ProcIds,
        [], InputSpecDeletePPIds, !PredInfo, !ModuleInfo,
        PredSpecsAcc0, PredSpecsAcc),
    AfterFrontEnd = SimplifyTasks ^ do_after_front_end,
    (
        AfterFrontEnd = not_after_front_end
    ;
        AfterFrontEnd = after_front_end,
        list.foldl(delete_specified_proc, InputSpecDeletePPIds, !ModuleInfo)
    ),
    PredSpecs = diag_spec_accumulator_to_list(PredSpecsAcc),
    !:Specs = PredSpecs ++ !.Specs,
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
    trace [io(!IO)] (
        maybe_report_stats(ProgressStream, Statistics, !IO)
    ).

:- pred delete_specified_proc(pred_proc_id::in,
    module_info::in, module_info::out) is det.

delete_specified_proc(proc(PredId, ProcId), !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.delete(ProcId, ProcTable0, ProcTable),
    pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

%---------------------------------------------------------------------------%
:- end_module top_level.mercury_compile_simplify.
%---------------------------------------------------------------------------%
