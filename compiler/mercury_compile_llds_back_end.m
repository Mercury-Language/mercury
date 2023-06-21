%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2017-2018 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mercury_compile_llds_back_end.m.
%
% This module implements the LLDS backend for the top level of the Mercury
% compiler. It invokes the different passes of this backend as appropriate.
%
%---------------------------------------------------------------------------%

:- module top_level.mercury_compile_llds_back_end.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.passes_aux.
:- import_module libs.
:- import_module libs.maybe_util.
:- import_module libs.op_mode.
:- import_module ll_backend.
:- import_module ll_backend.global_data.
:- import_module ll_backend.llds.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module bool.
:- import_module io.
:- import_module list.

:- pred llds_backend_pass(io.text_output_stream::in, io.text_output_stream::in,
    module_info::in, module_info::out, global_data::out,
    list(c_procedure)::out, dump_info::in, dump_info::out,
    io::di, io::uo) is det.

:- pred map_args_to_regs(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

:- pred llds_output_pass(io.text_output_stream::in, io.text_output_stream::in,
    op_mode_codegen::in, module_info::in, global_data::in,
    list(c_procedure)::in, module_name::in,
    maybe_succeeded::out, list(string)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.base_typeclass_info.
:- import_module backend_libs.compile_target_code.
:- import_module backend_libs.export.
:- import_module backend_libs.foreign.
:- import_module backend_libs.proc_label.
:- import_module backend_libs.type_class_info.
:- import_module backend_libs.type_ctor_info.
:- import_module check_hlds.
:- import_module check_hlds.simplify.
:- import_module check_hlds.simplify.simplify_proc.
:- import_module check_hlds.simplify.simplify_tasks.
:- import_module hlds.arg_info.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.mark_tail_calls.
:- import_module libs.dependency_graph.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.dupproc.
:- import_module ll_backend.follow_code.
:- import_module ll_backend.liveness.
:- import_module ll_backend.llds_out.
:- import_module ll_backend.llds_out.llds_out_file.
:- import_module ll_backend.optimize.
:- import_module ll_backend.proc_gen.
:- import_module ll_backend.saved_vars.
:- import_module ll_backend.stack_alloc.
:- import_module ll_backend.stack_layout.
:- import_module ll_backend.stack_opt.
:- import_module ll_backend.store_alloc.
:- import_module ll_backend.transform_llds.
:- import_module ll_backend.unify_gen_construct.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.write_error_spec.
:- import_module top_level.mercury_compile_front_end.

:- import_module assoc_list.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%

llds_backend_pass(ProgressStream, ErrorStream, !HLDS, !:GlobalData, LLDS,
        !DumpInfo, !IO) :-
    module_info_get_name(!.HLDS, ModuleName),
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, unboxed_float, OptUnboxFloat),
    globals.get_opt_tuple(Globals, OptTuple),
    DoCommonData = OptTuple ^ ot_use_common_data,
    (
        OptUnboxFloat = yes,
        UnboxFloats = have_unboxed_floats
    ;
        OptUnboxFloat = no,
        UnboxFloats = do_not_have_unboxed_floats
    ),
    globals.lookup_bool_option(Globals, unboxed_int64s, OptUnboxInt64s),
    (
        OptUnboxInt64s = yes,
        UnboxInt64s = have_unboxed_int64s
    ;
        OptUnboxInt64s = no,
        UnboxInt64s = do_not_have_unboxed_int64s
    ),
    StaticCellInfo0 = init_static_cell_info(ModuleName, UnboxFloats,
        UnboxInt64s, DoCommonData),
    module_info_get_ts_rev_string_table(!.HLDS, TSStringTableSize,
        TSRevStringTable),
    global_data_init(StaticCellInfo0, TSStringTableSize, TSRevStringTable,
        !:GlobalData),

    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    % map_args_to_regs affects the interface to a predicate,
    % so it must be done in one phase immediately before code generation.
    map_args_to_regs(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 305, "args_to_regs",
        !DumpInfo, !IO),

    globals.lookup_bool_option(Globals, trad_passes, TradPasses),
    add_all_tabling_info_structs(!.HLDS, !GlobalData),
    (
        TradPasses = no,
        llds_backend_pass_by_phases(ProgressStream, !HLDS, LLDS, !GlobalData,
            [], Specs, !DumpInfo, !IO)
    ;
        TradPasses = yes,
        llds_backend_pass_by_preds(ProgressStream, !HLDS, LLDS, !GlobalData,
            [], Specs)
    ),
    write_error_specs(ErrorStream, Globals, Specs, !IO).

%---------------------------------------------------------------------------%

:- pred llds_backend_pass_by_phases(io.text_output_stream::in,
    module_info::in, module_info::out, list(c_procedure)::out,
    global_data::in, global_data::out,
    list(error_spec)::in, list(error_spec)::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

llds_backend_pass_by_phases(ProgressStream, !HLDS, !:LLDS, !GlobalData, !Specs,
        !DumpInfo, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_saved_vars(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 310, "saved_vars_const",
        !DumpInfo, !IO),

    maybe_stack_opt(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 315, "saved_vars_cell",
        !DumpInfo, !IO),

    maybe_followcode(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 320, "followcode", !DumpInfo, !IO),

    maybe_simplify(ProgressStream, maybe.no, bool.no, simplify_pass_ll_backend,
        Verbose, Stats, !HLDS, [], SimplifySpecs, !IO),
    expect(unify(contains_errors(Globals, SimplifySpecs), no), $pred,
        "simplify has errors"),
    maybe_dump_hlds(ProgressStream, !.HLDS, 325, "ll_backend_simplify",
        !DumpInfo, !IO),

    compute_liveness(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 330, "liveness", !DumpInfo, !IO),

    maybe_mark_tail_rec_calls(ProgressStream, Verbose, Stats,
        !HLDS, !Specs, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 332, "mark_debug_tailrec_calls",
        !DumpInfo, !IO),

    compute_stack_vars(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 335, "stackvars", !DumpInfo, !IO),

    allocate_store_map(ProgressStream, Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(ProgressStream, !.HLDS, 340, "store_map", !DumpInfo, !IO),

    generate_llds_code_for_module(ProgressStream, !.HLDS, Verbose, Stats,
        !GlobalData, !:LLDS, !IO),

    maybe_optimize_llds(ProgressStream, !.HLDS, !.GlobalData, Verbose, Stats,
        !LLDS, !IO),

    maybe_generate_stack_layouts(ProgressStream, !.HLDS, !.LLDS, Verbose,
        Stats, !GlobalData, !IO).
    % maybe_dump_global_data(!.GlobalData, !IO).

:- pred llds_backend_pass_by_preds(io.text_output_stream::in,
    module_info::in, module_info::out, list(c_procedure)::out,
    global_data::in, global_data::out,
    list(error_spec)::in, list(error_spec)::out) is det.

llds_backend_pass_by_preds(ProgressStream, !HLDS, LLDS, !GlobalData, !Specs) :-
    module_info_get_valid_pred_ids(!.HLDS, PredIds),
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    ProcDups = OptTuple ^ ot_opt_proc_dups,
    (
        ProcDups = do_not_opt_proc_dups,
        OrderedPredIds = PredIds,
        MaybeDupProcMap = no
    ;
        ProcDups = opt_proc_dups,
        PredDepInfo = build_pred_dependency_graph(!.HLDS, PredIds,
            do_not_include_imported),
        OrderedPredIds = dependency_info_get_condensed_bottom_up_sccs(
            PredDepInfo),
        MaybeDupProcMap = yes(map.init)
    ),
    % The dependency information used to warn about mutual tail recursion
    % cannot be shared with the information used to remove duplicate
    % procedures, they use different nodes in the dependency graph
    % (predicates versus procedures).
    module_info_rebuild_dependency_info(!HLDS, ProcDepInfo),
    SCCMap = dependency_info_make_scc_map(ProcDepInfo),
    generate_const_structs(!.HLDS, ConstStructMap, !GlobalData),
    llds_backend_pass_by_preds_loop_over_preds(ProgressStream, !HLDS,
        ConstStructMap, SCCMap, OrderedPredIds, MaybeDupProcMap,
        cord.init, CProcsCord, !GlobalData, !Specs),
    LLDS = cord.list(CProcsCord).

:- type dup_proc_label_map ==
    map(mdbcomp.prim_data.proc_label, mdbcomp.prim_data.proc_label).

:- pred llds_backend_pass_by_preds_loop_over_preds(io.text_output_stream::in,
    module_info::in, module_info::out, const_struct_map::in,
    scc_map(pred_proc_id)::in, list(pred_id)::in,
    maybe(dup_proc_label_map)::in,
    cord(c_procedure)::in, cord(c_procedure)::out,
    global_data::in, global_data::out,
    list(error_spec)::in, list(error_spec)::out) is det.

llds_backend_pass_by_preds_loop_over_preds(_, !HLDS, _, _,
        [], _, !CProcsCord, !GlobalData, !Specs).
llds_backend_pass_by_preds_loop_over_preds(ProgressStream, !HLDS,
        ConstStructMap, SCCMap, [PredId | PredIds], !.MaybeDupProcMap,
        !CProcsCord, !GlobalData, !Specs) :-
    llds_backend_pass_by_preds_do_one_pred(ProgressStream, !HLDS,
        ConstStructMap, SCCMap, PredId, !MaybeDupProcMap,
        !CProcsCord, !GlobalData, !Specs),
    llds_backend_pass_by_preds_loop_over_preds(ProgressStream, !HLDS,
        ConstStructMap, SCCMap, PredIds, !.MaybeDupProcMap,
        !CProcsCord, !GlobalData, !Specs).

:- pred llds_backend_pass_by_preds_do_one_pred(io.text_output_stream::in,
    module_info::in, module_info::out, const_struct_map::in,
    scc_map(pred_proc_id)::in, pred_id::in,
    maybe(dup_proc_label_map)::in, maybe(dup_proc_label_map)::out,
    cord(c_procedure)::in, cord(c_procedure)::out,
    global_data::in, global_data::out,
    list(error_spec)::in, list(error_spec)::out) is det.

llds_backend_pass_by_preds_do_one_pred(ProgressStream, !HLDS, ConstStructMap,
        SCCMap, PredId, !MaybeDupProcMap, !CProcsCord, !GlobalData, !Specs) :-
    module_info_pred_info(!.HLDS, PredId, PredInfo),
    ProcIds = pred_info_valid_non_imported_procids(PredInfo),
    (
        ProcIds = []
    ;
        ProcIds = [_ | _],
        module_info_get_globals(!.HLDS, Globals0),
        globals.lookup_bool_option(Globals0, verbose, Verbose),
        (
            Verbose = yes,
            trace [io(!IO)] (
                PredStr = pred_id_to_user_string(!.HLDS, PredId),
                io.format(ProgressStream, "%% Generating code for %s\n",
                    [s(PredStr)], !IO),
                maybe_flush_output(ProgressStream, Verbose, !IO)
            )
        ;
            Verbose = no
        ),
        ( if
            PredModule = pred_info_module(PredInfo),
            PredName = pred_info_name(PredInfo),
            PredArity = pred_info_orig_arity(PredInfo),
            no_type_info_builtin(PredModule, PredName, PredArity)
        then
            % These predicates should never be traced, since they do not obey
            % typeinfo_liveness. Since they may be opt_imported into other
            % modules, we must switch off the tracing of such preds on a
            % pred-by-pred basis; module-by-module wouldn't work.
            globals.get_trace_level(Globals0, TraceLevel),
            globals.set_trace_level_none(Globals0, Globals1),
            module_info_set_globals(Globals1, !HLDS),
            llds_backend_pass_for_pred(ProgressStream, ConstStructMap, SCCMap,
                PredId, PredInfo, ProcIds, IdCProcs,
                !GlobalData, !HLDS, !Specs),
            module_info_get_globals(!.HLDS, Globals2),
            globals.set_trace_level(TraceLevel, Globals2, Globals),
            module_info_set_globals(Globals, !HLDS)
        else
            llds_backend_pass_for_pred(ProgressStream, ConstStructMap, SCCMap,
                PredId, PredInfo, ProcIds, IdCProcs,
                !GlobalData, !HLDS, !Specs)
        ),
        (
            !.MaybeDupProcMap = no,
            assoc_list.values(IdCProcs, CProcs)
        ;
            !.MaybeDupProcMap = yes(DupProcMap0),
            eliminate_duplicate_procs(IdCProcs, CProcs,
                DupProcMap0, DupProcMap),
            !:MaybeDupProcMap = yes(DupProcMap)
        ),
        !:CProcsCord = !.CProcsCord ++ cord.from_list(CProcs),
        globals.lookup_bool_option(Globals0, statistics, Stats),
        trace [io(!IO)] (
            maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
            maybe_report_stats(ProgressStream, Stats, !IO)
        )
    ).

:- pred llds_backend_pass_for_pred(io.text_output_stream::in,
    const_struct_map::in, scc_map(pred_proc_id)::in, pred_id::in,
    pred_info::in, list(proc_id)::in,
    assoc_list(mdbcomp.prim_data.proc_label, c_procedure)::out,
    global_data::in, global_data::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

llds_backend_pass_for_pred(_, _, _, _, _, [], [], !GlobalData, !HLDS, !Specs).
llds_backend_pass_for_pred(ProgressStream, ConstStructMap, SCCMap,
        PredId, PredInfo, [ProcId | ProcIds],
        [ProcLabel - CProc | ProcLabelsCProcs], !GlobalData, !HLDS, !Specs) :-
    ProcLabel = make_proc_label(!.HLDS, PredId, ProcId),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.lookup(ProcTable, ProcId, ProcInfo),
    llds_backend_pass_for_proc(ProgressStream, ConstStructMap, SCCMap,
        PredId, ProcId, PredInfo, ProcInfo, CProc, !GlobalData, !HLDS, !Specs),
    llds_backend_pass_for_pred(ProgressStream, ConstStructMap, SCCMap, PredId,
        PredInfo, ProcIds, ProcLabelsCProcs, !GlobalData, !HLDS, !Specs).

:- pred llds_backend_pass_for_proc(io.text_output_stream::in,
    const_struct_map::in, scc_map(pred_proc_id)::in,
    pred_id::in, proc_id::in, pred_info::in, proc_info::in, c_procedure::out,
    global_data::in, global_data::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

llds_backend_pass_for_proc(ProgressStream, ConstStructMap, SCCMap,
        PredId, ProcId, PredInfo, !.ProcInfo, CProc,
        !GlobalData, !HLDS, !Specs) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    SavedVarsConst = OptTuple ^ ot_opt_saved_vars_const,
    PredProcId = proc(PredId, ProcId),
    (
        SavedVarsConst = opt_saved_vars_const,
        saved_vars_proc(ProgressStream, PredProcId, !ProcInfo, !HLDS)
    ;
        SavedVarsConst = do_not_opt_saved_vars_const
    ),
    SavedVarsCell = OptTuple ^ ot_opt_svcell,
    (
        SavedVarsCell = opt_svcell,
        stack_opt_cell(PredProcId, !ProcInfo, !HLDS)
    ;
        SavedVarsCell = do_not_opt_svcell
    ),
    FollowCode = OptTuple ^ ot_opt_follow_code,
    (
        FollowCode = opt_follow_code,
        move_follow_code_in_proc(PredProcId, !ProcInfo, !HLDS)
    ;
        FollowCode = do_not_opt_follow_code
    ),
    find_simplify_tasks(Globals, do_not_generate_warnings, SimplifyTasks0),
    SimpList0 = simplify_tasks_to_list(SimplifyTasks0),

    ConstProp = OptTuple ^ ot_prop_constants,
    globals.lookup_bool_option(Globals, profile_deep, DeepProf),
    globals.lookup_bool_option(Globals, record_term_sizes_as_words, TSWProf),
    globals.lookup_bool_option(Globals, record_term_sizes_as_cells, TSCProf),
    ProfTrans = bool.or_list([DeepProf, TSWProf, TSCProf]),

    % Don't run constant propagation if any of the profiling
    % transformations has been applied.
    %
    % NOTE: Any changes here may also need to be made to
    % mercury_compile.simplify.

    ( if
        ConstProp = prop_constants,
        ProfTrans = no
    then
        list.cons(simptask_constant_prop, SimpList0, SimpList1)
    else
        SimpList1 = list.delete_all(SimpList0, simptask_constant_prop)
    ),

    SimpList = [simptask_mark_code_model_changes,
        simptask_elim_removable_scopes | SimpList1],
    SimplifyTasks = list_to_simplify_tasks(Globals, SimpList),
    trace [io(!IO)] (
        maybe_write_proc_progress_message(ProgressStream, !.HLDS,
            "Simplifying", PredProcId, !IO)
    ),
    simplify_proc(yes(ProgressStream), SimplifyTasks, PredId, ProcId,
        !HLDS, !ProcInfo),
    trace [io(!IO)] (
        maybe_write_proc_progress_message(ProgressStream, !.HLDS,
            "Computing liveness in", PredProcId, !IO)
    ),
    detect_liveness_proc(!.HLDS, PredProcId, !ProcInfo),
    trace [io(!IO)] (
        maybe_write_proc_progress_message(ProgressStream, !.HLDS,
            "Marking directly tail recursive calls in", PredProcId, !IO)
    ),
    mark_tail_rec_calls_in_proc_for_llds_code_gen(!.HLDS, PredId, ProcId,
        PredInfo, SCCMap, !ProcInfo, !Specs),

    trace [io(!IO)] (
        maybe_write_proc_progress_message(ProgressStream, !.HLDS,
            "Allocating stack slots in", PredProcId, !IO)
    ),
    allocate_stack_slots_in_proc(!.HLDS, PredProcId, !ProcInfo),
    trace [io(!IO)] (
        maybe_write_proc_progress_message(ProgressStream, !.HLDS,
            "Allocating storage locations for live vars in", PredProcId, !IO)
    ),
    allocate_store_maps(final_allocation, !.HLDS, PredProcId, !ProcInfo),
    trace [io(!IO)] (
        maybe_write_proc_progress_message(ProgressStream, !.HLDS,
            "Generating low-level (LLDS) code for", PredProcId, !IO)
    ),
    generate_proc_code(!.HLDS, ConstStructMap, PredId, PredInfo,
         ProcId, !.ProcInfo, CProc0, !GlobalData),
    Optimize = OptTuple ^ ot_optimize,
    (
        Optimize = optimize,
        module_info_get_name(!.HLDS, ModuleName),
        optimize_proc(Globals, ModuleName, !.GlobalData, CProc0, CProc)
    ;
        Optimize = do_not_optimize,
        CProc = CProc0
    ),
    trace [io(!IO)] (
        maybe_write_proc_progress_message(ProgressStream, !.HLDS,
            "Generating call continuation information for", PredProcId, !IO)
    ),
    maybe_collect_call_continuations_in_cproc(!.HLDS, CProc, !GlobalData).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The various passes of the LLDS backend.
%

map_args_to_regs(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(ProgressStream, Verbose,
        "% Mapping args to regs...", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    generate_arg_info(!HLDS),
    maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

:- pred maybe_saved_vars(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_saved_vars(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    SavedVars = OptTuple ^ ot_opt_saved_vars_const,
    (
        SavedVars = opt_saved_vars_const,
        maybe_write_string(ProgressStream, Verbose,
            "% Minimizing variable saves using constants...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        process_valid_nonimported_procs(
            update_module(saved_vars_proc(ProgressStream)), !HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        SavedVars = do_not_opt_saved_vars_const
    ).

:- pred maybe_stack_opt(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_stack_opt(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    SavedVars = OptTuple ^ ot_opt_svcell,
    (
        SavedVars = opt_svcell,
        maybe_write_string(ProgressStream, Verbose,
            "% Minimizing variable saves using cells...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        process_valid_nonimported_procs(update_module(stack_opt_cell), !HLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        SavedVars = do_not_opt_svcell
    ).

:- pred maybe_followcode(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_followcode(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    FollowCode = OptTuple ^ ot_opt_follow_code,
    (
        FollowCode = opt_follow_code,
        maybe_write_string(ProgressStream, Verbose,
            "% Migrating branch code...", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        process_valid_nonimported_procs(
            update_module(move_follow_code_in_proc), !HLDS),
        maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        FollowCode = do_not_opt_follow_code
    ).

:- pred compute_liveness(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

compute_liveness(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, parallel_liveness, ParallelLiveness),
    globals.lookup_int_option(Globals, debug_liveness, DebugLiveness),
    maybe_write_string(ProgressStream, Verbose,
        "% Computing liveness...\n", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    ( if
        ParallelLiveness = yes,
        DebugLiveness = -1
    then
        detect_liveness_preds_parallel(!HLDS)
    else
        process_valid_nonimported_procs(update_proc_ids(detect_liveness_proc),
            !HLDS)
    ),
    maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

:- pred maybe_mark_tail_rec_calls(io.text_output_stream::in,
    bool::in, bool::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

maybe_mark_tail_rec_calls(ProgressStream, Verbose, Stats,
        !HLDS, !Specs, !IO) :-
    maybe_write_string(ProgressStream, Verbose,
        "% Marking directly tail recursive calls...", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    module_info_rebuild_dependency_info(!HLDS, DepInfo),
    SCCMap = dependency_info_make_scc_map(DepInfo),
    process_valid_nonimported_preds_errors(
        update_pred_error(
            mark_tail_rec_calls_in_pred_for_llds_code_gen(SCCMap)),
        !HLDS, !Specs, !IO),
    maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

:- pred compute_stack_vars(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

compute_stack_vars(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(ProgressStream, Verbose,
        "% Computing stack vars...", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    process_valid_nonimported_procs(
        update_proc_ids(allocate_stack_slots_in_proc), !HLDS),
    maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

:- pred allocate_store_map(io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

allocate_store_map(ProgressStream, Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(ProgressStream, Verbose,
        "% Allocating store map...", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    process_valid_nonimported_procs(
        update_proc_ids(allocate_store_maps(final_allocation)), !HLDS),
    maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

:- pred generate_llds_code_for_module(io.text_output_stream::in,
    module_info::in, bool::in, bool::in, global_data::in, global_data::out,
    list(c_procedure)::out, io::di, io::uo) is det.

generate_llds_code_for_module(ProgressStream, HLDS, Verbose, Stats,
        !GlobalData, LLDS, !IO) :-
    maybe_write_string(ProgressStream, Verbose, "% Generating code...\n", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    generate_module_code(HLDS, LLDS, !GlobalData),
    maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

:- pred maybe_optimize_llds(io.text_output_stream::in, module_info::in,
    global_data::in, bool::in, bool::in,
    list(c_procedure)::in, list(c_procedure)::out, io::di, io::uo) is det.

maybe_optimize_llds(ProgressStream, HLDS, GlobalData, Verbose, Stats,
        !LLDS, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    Optimize = OptTuple ^ ot_optimize,
    (
        Optimize = optimize,
        maybe_write_string(ProgressStream, Verbose,
            "% Doing optimizations...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        module_info_get_name(HLDS, ModuleName),
        optimize_procs(Globals, ModuleName, GlobalData, !LLDS),
        maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        Optimize = do_not_optimize
    ).

:- pred maybe_generate_stack_layouts(io.text_output_stream::in,
    module_info::in, list(c_procedure)::in, bool::in, bool::in,
    global_data::in, global_data::out, io::di, io::uo) is det.

maybe_generate_stack_layouts(ProgressStream, HLDS, LLDS, Verbose, Stats,
        !GlobalData, !IO) :-
    maybe_write_string(ProgressStream, Verbose,
        "% Generating call continuation information...", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    maybe_collect_call_continuations_in_cprocs(HLDS, LLDS, !GlobalData),
    maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

llds_output_pass(ProgressStream, ErrorStream, OpModeCodeGen, HLDS,
        GlobalData0, Procs, ModuleName, Succeeded, FactTableObjFiles, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_write_string(ProgressStream, Verbose,
        "% Generating layout data...", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    % Here we generate the LLDS representations for various data structures
    % used for RTTI, type classes, and stack layouts.
    % XXX This should perhaps be part of backend_pass rather than output_pass.
    type_ctor_info.generate_rtti(HLDS, TypeCtorRttiData),
    generate_base_typeclass_info_rtti(HLDS, OldTypeClassInfoRttiData),
    globals.lookup_bool_option(Globals, new_type_class_rtti, NewTypeClassRtti),
    generate_type_class_info_rtti(HLDS, NewTypeClassRtti,
        NewTypeClassInfoRttiData),
    TypeClassInfoRttiData =
        OldTypeClassInfoRttiData ++ NewTypeClassInfoRttiData,
    stack_layout.generate_llds_layout_data(HLDS, GlobalData0, GlobalData,
        PseudoTypeInfos, HLDSVarNums, ShortLocns, LongLocns,
        UserEventVarNums, UserEvents,
        NoVarLabelLayouts, SVarLabelLayouts, LVarLabelLayouts,
        InternalLabelToLayoutMap, ProcLabelToLayoutMap,
        CallSites, CoveragePoints, ProcStatics,
        ProcHeadVarNums, ProcVarNames, ProcBodyBytecodes,
        TableIoEntries, TableIoEntryMap, ProcEventLayouts,
        ExecTraces, ProcLayoutDatas, ModuleLayoutDatas),
    maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO),

    % Here we perform some optimizations on the LLDS data.
    % XXX This should perhaps be part of backend_pass rather than output_pass.
    % XXX We assume that the foreign language we use is C.
    llds_get_c_interface_info(HLDS, lang_c, C_InterfaceInfo),
    global_data_get_all_proc_vars(GlobalData, TablingInfoStructs),
    global_data_get_all_closure_layouts(GlobalData, ClosureLayoutDatas),
    global_data_get_static_cell_info(GlobalData, StaticCellInfo),
    get_static_cells(StaticCellInfo,
        ScalarCommonCellDatas, VectorCommonCellDatas),
    global_data_get_all_alloc_sites(GlobalData, AllocSites, AllocIdMap),
    global_data_get_threadscope_string_table(GlobalData, TSStringTable),

    % Next we put it all together and output it to one or more C files.
    RttiDatas = TypeCtorRttiData ++ TypeClassInfoRttiData,
    module_info_get_complexity_proc_infos(HLDS, ComplexityProcs),

    C_InterfaceInfo = foreign_interface_info(ModuleSymName,
        C_HeaderCodes0, C_BodyCodes, C_Includes,
        _C_ExportDecls, C_ExportDefns),
    MangledModuleName = sym_name_mangle(ModuleSymName),
    CModuleName = MangledModuleName ++ "_module",

    % Split the code up into bite-size chunks for the C compiler.
    globals.get_opt_tuple(Globals, OptTuple),
    JustOneCFunc = OptTuple ^ ot_use_just_one_c_func,
    % Three test cases, tests/hard_coded/backend_external*.m,
    % include hand-written C modules as a text of ":- external" pragmas.
    % The name of these hand-written C modules clashes with CModuleName.
    % The simplest way to avoid such clashes which are detectable only
    % at the high optimization levels at which use_just_one_c_func
    % is turned on (see Mantis bug #543) is to use the same machinery
    % to generate C modules *with* use_just_one_c_func as *without* it.
    (
        JustOneCFunc = use_just_one_c_func,
        ChunkedProcs = [Procs]
    ;
        JustOneCFunc = do_not_use_just_one_c_func,
        ProcsPerFunc = OptTuple ^ ot_procs_per_c_function,
        list.chunk(Procs, ProcsPerFunc, ChunkedProcs)
    ),
    proc_chunks_to_c_modules(CModuleName, ChunkedProcs, ChunkedModules),
    list.map_foldl(make_foreign_import_header_code(Globals), C_Includes,
        C_IncludeHeaderCodes, !IO),

    make_decl_guards(ModuleSymName, DeclGuardStart, DeclGuardEnd),
    % We don't want to put C_LocalHeaderCodes between DeclGuardStart and
    % DeclGuardEnd, because C_IncludeHeaderCodes may include our own
    % header file, which defines the module's guard macro, which in turn
    % #ifdefs out the stuff between DeclGuardStart and DeclGuardEnd.
    list.filter(foreign_decl_code_is_local, C_HeaderCodes0,
        C_LocalHeaderCodes, C_ExportedHeaderCodes),
    C_HeaderCodes = C_IncludeHeaderCodes ++ C_LocalHeaderCodes ++
        [DeclGuardStart | C_ExportedHeaderCodes] ++ [DeclGuardEnd],

    module_info_user_init_pred_target_names(HLDS, UserInitPredCNames),
    module_info_user_final_pred_target_names(HLDS, UserFinalPredCNames),

    CFile = c_file(ModuleSymName, C_HeaderCodes, C_BodyCodes, C_ExportDefns,
        TablingInfoStructs, ScalarCommonCellDatas, VectorCommonCellDatas,
        RttiDatas, PseudoTypeInfos, HLDSVarNums, ShortLocns, LongLocns,
        UserEventVarNums, UserEvents,
        NoVarLabelLayouts, SVarLabelLayouts, LVarLabelLayouts,
        InternalLabelToLayoutMap, ProcLabelToLayoutMap,
        CallSites, CoveragePoints, ProcStatics,
        ProcHeadVarNums, ProcVarNames, ProcBodyBytecodes, TSStringTable,
        TableIoEntries, TableIoEntryMap, ProcEventLayouts, ExecTraces,
        ProcLayoutDatas, ModuleLayoutDatas, ClosureLayoutDatas,
        AllocSites, AllocIdMap, ChunkedModules,
        UserInitPredCNames, UserFinalPredCNames, ComplexityProcs),

    output_llds_file(Globals, CFile, TargetCodeSucceeded, !IO),
    (
        TargetCodeSucceeded = succeeded,

        C_InterfaceInfo = foreign_interface_info(_, _, _, _, C_ExportDecls, _),
        export.produce_header_file(HLDS, C_ExportDecls, ModuleName, !IO),

        % Finally we invoke the C compiler on the generated C files,
        % if we were asked to do so.
        (
            ( OpModeCodeGen = opmcg_target_and_object_code_only
            ; OpModeCodeGen = opmcg_target_object_and_executable
            ),
            llds_c_to_obj(Globals, ProgressStream, ErrorStream, ModuleName,
                CompileSucceeded, !IO),
            module_info_get_fact_table_file_names(HLDS, FactTableBaseFiles),
            list.map2_foldl(
                compile_fact_table_file(Globals, ProgressStream, ErrorStream),
                FactTableBaseFiles, FactTableObjFiles,
                FactTableCompileSucceededs, !IO),
            Succeeded =
                and_list([CompileSucceeded | FactTableCompileSucceededs]),
            maybe_set_exit_status(Succeeded, !IO)
        ;
            OpModeCodeGen = opmcg_target_code_only,
            Succeeded = succeeded,
            FactTableObjFiles = []
        )
    ;
        TargetCodeSucceeded = did_not_succeed,
        Succeeded = did_not_succeed,
        FactTableObjFiles = []
    ).

    % Foreign_interface_info holds information used when generating
    % code that uses the foreign language interface.
    %
:- type foreign_interface_info
    --->    foreign_interface_info(
                module_name,

                % Info about stuff imported from C:
                list(foreign_decl_code),
                list(foreign_body_code),
                list(fim_spec),

                % Info about stuff exported to C:
                foreign_export_decls,
                foreign_export_defns
            ).

    % Gather together the information from the HLDS, given the foreign
    % language we are going to use, that is used for the foreign language
    % interface.
    % This stuff mostly just gets passed directly to the LLDS unchanged, but
    % we do do a bit of code generation -- for example, we call
    % export.get_foreign_export_{decls,defns} here, which do the generation
    % of C code for `pragma foreign_export' declarations.
    %
:- pred llds_get_c_interface_info(module_info::in, foreign_language::in,
    foreign_interface_info::out) is det.

llds_get_c_interface_info(HLDS, UseForeignLanguage, ForeignInterfaceInfo) :-
    module_info_get_name(HLDS, ModuleName),
    ForeignSelfImport = fim_spec(UseForeignLanguage, ModuleName),
    module_info_get_foreign_decl_codes_user(HLDS, ForeignDeclCodeUserCord),
    module_info_get_foreign_decl_codes_aux(HLDS, ForeignDeclCodeAuxCord),
    module_info_get_foreign_body_codes(HLDS, ForeignBodyCodeCord),
    module_info_get_c_j_cs_fims(HLDS, CJCsEFIMs0),
    ForeignDeclCodes =
        cord.list(ForeignDeclCodeUserCord ++ ForeignDeclCodeAuxCord),
    ForeignBodyCodes = cord.list(ForeignBodyCodeCord),
    add_fim_spec(ForeignSelfImport, CJCsEFIMs0, CJCsEFIMs),

    % Always include the module we are compiling amongst the foreign import
    % modules so that pragma foreign_exported procedures are visible to
    % foreign code in this module.
    %
    % XXX The frontend should really handle this but it is quite inconsistent
    % in its treatment of self-imports. Both this backend (the LLDS)
    % and the MLDS backend currently handle self foreign imports directly.

    foreign.filter_decls(UseForeignLanguage, ForeignDeclCodes,
        WantedForeignDeclCodes, _OtherDeclCodes),
    foreign.filter_bodys(UseForeignLanguage, ForeignBodyCodes,
        WantedForeignBodyCodes, _OtherBodyCodes),
    WantedForeignImports = set.to_sorted_list(
        get_lang_fim_specs(CJCsEFIMs, UseForeignLanguage)),
    export.get_foreign_export_decls(HLDS, ForeignExportDecls),
    export.get_foreign_export_defns(HLDS, ForeignExportDefns),

    ForeignInterfaceInfo = foreign_interface_info(ModuleName,
        WantedForeignDeclCodes, WantedForeignBodyCodes,
        WantedForeignImports, ForeignExportDecls, ForeignExportDefns).

:- pred foreign_decl_code_is_local(foreign_decl_code::in) is semidet.

foreign_decl_code_is_local(Decl) :-
    Decl = foreign_decl_code(_, foreign_decl_is_local, _, _).

:- pred make_decl_guards(sym_name::in, foreign_decl_code::out,
    foreign_decl_code::out) is det.

make_decl_guards(ModuleName, StartGuard, EndGuard) :-
    Define = decl_guard(ModuleName),
    Start = "#ifndef " ++ Define ++ "\n#define " ++ Define ++ "\n",
    End = "\n#endif",
    StartGuard = foreign_decl_code(lang_c, foreign_decl_is_exported,
        floi_literal(Start), dummy_context),
    EndGuard = foreign_decl_code(lang_c, foreign_decl_is_exported,
        floi_literal(End), dummy_context).

:- pred make_foreign_import_header_code(globals::in, fim_spec::in,
    foreign_decl_code::out, io::di, io::uo) is det.

make_foreign_import_header_code(Globals, FIMSpec, Include, !IO) :-
    FIMSpec = fim_spec(Lang, ModuleName),
    (
        Lang = lang_c,
        module_name_to_search_file_name(Globals, $pred,
            ext_other(other_ext(".mh")), newext_mh(ext_mh_mh),
            ModuleName, HeaderFileName, !IO),
        IncludeString = "#include """ ++ HeaderFileName ++ """\n",
        Include = foreign_decl_code(lang_c, foreign_decl_is_exported,
            floi_literal(IncludeString), dummy_context)
    ;
        Lang = lang_csharp,
        sorry($pred, ":- import_module not yet implemented: " ++
            "`:- pragma foreign_import_module' for C#")
    ;
        Lang = lang_java,
        sorry($pred, ":- import_module not yet implemented: " ++
            "`:- pragma foreign_import_module' for Java")
    ).

:- pred proc_chunks_to_c_modules(string::in, list(list(c_procedure))::in,
    list(comp_gen_c_module)::out) is det.

proc_chunks_to_c_modules(ModName, ChunkList, Modules) :-
    proc_chunks_to_c_modules_loop(ModName, 0, ChunkList, Modules).

:- pred proc_chunks_to_c_modules_loop(string::in, int::in,
    list(list(c_procedure))::in, list(comp_gen_c_module)::out) is det.

proc_chunks_to_c_modules_loop(_CModuleName, _Num, [], []).
proc_chunks_to_c_modules_loop(CModuleName, Num,
        [Chunk | Chunks], [Module | Modules]) :-
    string.int_to_string(Num, NumString),
    Module = comp_gen_c_module(CModuleName ++ NumString, Chunk),
    proc_chunks_to_c_modules_loop(CModuleName, Num + 1, Chunks, Modules).

:- pred output_llds_file(globals::in, c_file::in, maybe_succeeded::out,
    io::di, io::uo) is det.

output_llds_file(Globals, LLDS0, Succeeded, !IO) :-
    transform_llds(Globals, LLDS0, LLDS),
    output_llds(Globals, LLDS, Succeeded, !IO).

:- pred llds_c_to_obj(globals::in,
    io.text_output_stream::in, io.text_output_stream::in, module_name::in,
    maybe_succeeded::out, io::di, io::uo) is det.

llds_c_to_obj(Globals, ProgressStream, ErrorStream, ModuleName,
        Succeeded, !IO) :-
    get_linked_target_type(Globals, LinkedTargetType),
    get_object_code_type(Globals, LinkedTargetType, PIC),
    pic_object_file_extension(Globals, PIC, ObjOtherExt, ObjNewExt, _),
    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(other_ext(".c")), newext_target_c_cs(ext_target_c),
        ModuleName, C_File, !IO),
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(ObjOtherExt), newext_target_obj(ObjNewExt),
        ModuleName, O_File, !IO),
    compile_target_code.do_compile_c_file(Globals, ProgressStream, ErrorStream,
        PIC, C_File, O_File, Succeeded, !IO).

:- pred compile_fact_table_file(globals::in,
    io.text_output_stream::in, io.text_output_stream::in, string::in,
    string::out, maybe_succeeded::out, io::di, io::uo) is det.

compile_fact_table_file(Globals, ProgressStream, ErrorStream,
        BaseName, O_FileName, Succeeded, !IO) :-
    get_linked_target_type(Globals, LinkedTargetType),
    get_object_code_type(Globals, LinkedTargetType, PIC),
    pic_object_file_extension(Globals, PIC, ObjOtherExt, NewExtObj, _),
    % XXX EXT
    C_FileName = BaseName ++ ".c",
    O_FileName = BaseName ++ extension_to_string(Globals,
        ext_other(ObjOtherExt), newext_target_obj(NewExtObj)),
    compile_target_code.do_compile_c_file(Globals, ProgressStream, ErrorStream,
        PIC, C_FileName, O_FileName, Succeeded, !IO).

%---------------------------------------------------------------------------%
:- end_module top_level.mercury_compile_llds_back_end.
%---------------------------------------------------------------------------%
