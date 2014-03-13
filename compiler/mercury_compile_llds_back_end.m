%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mercury_compile_llds_back_end.m.
%
%  This module implements the LLDS backend for the top level of the Mercury
% compiler. It invokes the different passes of this backend as appropriate.
%
%-----------------------------------------------------------------------------%

:- module top_level.mercury_compile_llds_back_end.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.passes_aux.
:- import_module mdbcomp.prim_data.
:- import_module ll_backend.global_data.
:- import_module ll_backend.llds.

:- import_module bool.
:- import_module io.
:- import_module list.

:- pred llds_backend_pass(module_info::in, module_info::out,
    global_data::out, list(c_procedure)::out, dump_info::in, dump_info::out,
    io::di, io::uo) is det.

:- pred llds_output_pass(module_info::in, global_data::in,
    list(c_procedure)::in, module_name::in, bool::out,
    list(string)::out, io::di, io::uo) is det.

:- pred map_args_to_regs(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.base_typeclass_info.
:- import_module backend_libs.compile_target_code.
:- import_module backend_libs.export.
:- import_module backend_libs.foreign.
:- import_module backend_libs.proc_label.
:- import_module backend_libs.rtti.
:- import_module backend_libs.type_class_info.
:- import_module backend_libs.type_ctor_info.
:- import_module check_hlds.simplify.
:- import_module hlds.arg_info.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.mark_tail_calls.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.dupproc.
:- import_module ll_backend.follow_code.
:- import_module ll_backend.global_data.
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
:- import_module ll_backend.unify_gen.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module top_level.mercury_compile_front_end.
:- import_module transform_hlds.dependency_graph.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

llds_backend_pass(!HLDS, !:GlobalData, LLDS, !DumpInfo, !IO) :-
    module_info_get_name(!.HLDS, ModuleName),
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, unboxed_float, OptUnboxFloat),
    globals.lookup_bool_option(Globals, common_data, DoCommonData),
    (
        OptUnboxFloat = yes,
        UnboxFloats = have_unboxed_floats
    ;
        OptUnboxFloat = no,
        UnboxFloats = do_not_have_unboxed_floats
    ),
    StaticCellInfo0 = init_static_cell_info(ModuleName, UnboxFloats,
        DoCommonData),
    module_info_get_ts_rev_string_table(!.HLDS, TSStringTableSize,
        TSRevStringTable),
    global_data_init(StaticCellInfo0, TSStringTableSize, TSRevStringTable,
        !:GlobalData),

    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    % map_args_to_regs affects the interface to a predicate,
    % so it must be done in one phase immediately before code generation.
    map_args_to_regs(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 305, "args_to_regs", !DumpInfo, !IO),

    globals.lookup_bool_option(Globals, trad_passes, TradPasses),
    add_all_tabling_info_structs(!.HLDS, !GlobalData),
    (
        TradPasses = no,
        llds_backend_pass_by_phases(!HLDS, !GlobalData, LLDS, !DumpInfo, !IO)
    ;
        TradPasses = yes,
        llds_backend_pass_by_preds(!HLDS, !GlobalData, LLDS, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred llds_backend_pass_by_phases(module_info::in, module_info::out,
    global_data::in, global_data::out, list(c_procedure)::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

llds_backend_pass_by_phases(!HLDS, !GlobalData, !:LLDS, !DumpInfo, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_saved_vars(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 310, "saved_vars_const", !DumpInfo, !IO),

    maybe_stack_opt(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 315, "saved_vars_cell", !DumpInfo, !IO),

    maybe_followcode(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 320, "followcode", !DumpInfo, !IO),

    maybe_simplify(no, simplify_pass_ll_backend, Verbose, Stats, !HLDS,
        [], SimplifySpecs, !IO),
    expect(unify(contains_errors(Globals, SimplifySpecs), no), $module, $pred,
        "simplify has errors"),
    maybe_dump_hlds(!.HLDS, 325, "ll_backend_simplify", !DumpInfo, !IO),

    compute_liveness(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 330, "liveness", !DumpInfo, !IO),

    maybe_mark_tail_rec_calls(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 332, "mark_debug_tailrec_calls", !DumpInfo, !IO),

    maybe_warn_non_tail_recursion(Verbose, Stats, !.HLDS, !IO),

    compute_stack_vars(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 335, "stackvars", !DumpInfo, !IO),

    allocate_store_map(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 340, "store_map", !DumpInfo, !IO),

    generate_llds_code_for_module(!.HLDS, Verbose, Stats, !GlobalData, !:LLDS,
        !IO),

    maybe_generate_stack_layouts(!.HLDS, !.LLDS, Verbose, Stats, !GlobalData,
        !IO),
    % maybe_dump_global_data(!.GlobalData, !IO),

    maybe_optimize_llds(!.HLDS, !.GlobalData, Verbose, Stats, !LLDS, !IO).

:- pred llds_backend_pass_by_preds(module_info::in, module_info::out,
    global_data::in, global_data::out, list(c_procedure)::out,
    io::di, io::uo) is det.

llds_backend_pass_by_preds(!HLDS, !GlobalData, LLDS, !IO) :-
    module_info_get_valid_predids(PredIds, !HLDS),
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, optimize_proc_dups, ProcDups),
    (
        ProcDups = no,
        OrderedPredIds = PredIds,
        MaybeDupProcMap = no
    ;
        ProcDups = yes,
        dependency_graph.build_pred_dependency_graph(!.HLDS, PredIds,
            do_not_include_imported, DepInfo),
        hlds_dependency_info_get_dependency_ordering(DepInfo, PredSCCs),
        list.condense(PredSCCs, OrderedPredIds),
        MaybeDupProcMap = yes(map.init)
    ),
    generate_const_structs(!.HLDS, ConstStructMap, !GlobalData),
    llds_backend_pass_by_preds_loop_over_preds(!HLDS, ConstStructMap,
        OrderedPredIds, MaybeDupProcMap, [], RevCodes, !GlobalData, !IO),
    list.reverse(RevCodes, Codes),
    list.condense(Codes, LLDS).

:- pred llds_backend_pass_by_preds_loop_over_preds(
    module_info::in, module_info::out, const_struct_map::in, list(pred_id)::in,
    maybe(map(mdbcomp.prim_data.proc_label, mdbcomp.prim_data.proc_label))::in,
    list(list(c_procedure))::in, list(list(c_procedure))::out,
    global_data::in, global_data::out, io::di, io::uo) is det.

llds_backend_pass_by_preds_loop_over_preds(!HLDS, _,
        [], _, !RevCodes, !GlobalData, !IO).
llds_backend_pass_by_preds_loop_over_preds(!HLDS, ConstStructMap,
        [PredId | PredIds], !.MaybeDupProcMap, !RevCodes, !GlobalData, !IO) :-
    module_info_get_preds(!.HLDS, PredTable),
    map.lookup(PredTable, PredId, PredInfo),
    ProcIds = pred_info_non_imported_procids(PredInfo),
    (
        ProcIds = [],
        ProcList = []
    ;
        ProcIds = [_ | _],
        module_info_get_globals(!.HLDS, Globals0),
        globals.lookup_bool_option(Globals0, verbose, Verbose),
        (
            Verbose = yes,
            io.write_string("% Generating code for ", !IO),
            write_pred_id(!.HLDS, PredId, !IO),
            io.write_string("\n", !IO),
            maybe_flush_output(Verbose, !IO)
        ;
            Verbose = no
        ),
        (
            PredModule = pred_info_module(PredInfo),
            PredName = pred_info_name(PredInfo),
            PredArity = pred_info_orig_arity(PredInfo),
            no_type_info_builtin(PredModule, PredName, PredArity)
        ->
            % These predicates should never be traced, since they do not obey
            % typeinfo_liveness. Since they may be opt_imported into other
            % modules, we must switch off the tracing of such preds on a
            % pred-by-pred basis; module-by-module wouldn't work.
            globals.get_trace_level(Globals0, TraceLevel),
            globals.set_trace_level_none(Globals0, Globals1),
            module_info_set_globals(Globals1, !HLDS),
            llds_backend_pass_for_pred(!HLDS, ConstStructMap, PredId, PredInfo,
                ProcIds, IdProcList, !GlobalData, !IO),
            module_info_get_globals(!.HLDS, Globals2),
            globals.set_trace_level(TraceLevel, Globals2, Globals),
            module_info_set_globals(Globals, !HLDS)
        ;
            llds_backend_pass_for_pred(!HLDS, ConstStructMap,
                PredId, PredInfo, ProcIds, IdProcList, !GlobalData, !IO)
        ),
        (
            !.MaybeDupProcMap = no,
            assoc_list.values(IdProcList, ProcList)
        ;
            !.MaybeDupProcMap = yes(DupProcMap0),
            eliminate_duplicate_procs(IdProcList, ProcList,
                DupProcMap0, DupProcMap),
            !:MaybeDupProcMap = yes(DupProcMap)
        ),
        maybe_write_string(Verbose, "% done.\n", !IO),
        globals.lookup_bool_option(Globals0, statistics, Stats),
        maybe_report_stats(Stats, !IO)
    ),
    !:RevCodes = [ProcList | !.RevCodes],
    llds_backend_pass_by_preds_loop_over_preds(!HLDS, ConstStructMap, PredIds,
        !.MaybeDupProcMap, !RevCodes, !GlobalData, !IO).

:- pred llds_backend_pass_for_pred( module_info::in, module_info::out,
    const_struct_map::in, pred_id::in, pred_info::in, list(proc_id)::in,
    assoc_list(mdbcomp.prim_data.proc_label, c_procedure)::out,
    global_data::in, global_data::out, io::di, io::uo) is det.

llds_backend_pass_for_pred(!HLDS, _, _, _, [], [], !GlobalData, !IO).
llds_backend_pass_for_pred(!HLDS, ConstStructMap, PredId, PredInfo,
        [ProcId | ProcIds], [ProcLabel - ProcCode | ProcCodes],
        !GlobalData, !IO) :-
    ProcLabel = make_proc_label(!.HLDS, PredId, ProcId),
    pred_info_get_procedures(PredInfo, ProcTable),
    map.lookup(ProcTable, ProcId, ProcInfo),
    llds_backend_pass_for_proc(!HLDS, ConstStructMap, PredId, PredInfo,
        ProcId, ProcInfo, ProcCode, !GlobalData, !IO),
    llds_backend_pass_for_pred(!HLDS, ConstStructMap, PredId, PredInfo,
        ProcIds, ProcCodes, !GlobalData, !IO).

:- pred llds_backend_pass_for_proc( module_info::in, module_info::out,
    const_struct_map::in, pred_id::in, pred_info::in,
    proc_id::in, proc_info::in, c_procedure::out,
    global_data::in, global_data::out, io::di, io::uo) is det.

llds_backend_pass_for_proc(!HLDS, ConstStructMap, PredId, PredInfo,
        ProcId, !.ProcInfo, ProcCode, !GlobalData, !IO) :-
    PredProcId = proc(PredId, ProcId),
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, optimize_saved_vars_const,
        SavedVarsConst),
    (
        SavedVarsConst = yes,
        saved_vars_proc(proc(PredId, ProcId), !ProcInfo, !HLDS)
    ;
        SavedVarsConst = no
    ),
    globals.lookup_bool_option(Globals, optimize_saved_vars_cell,
        SavedVarsCell),
    (
        SavedVarsCell = yes,
        stack_opt_cell(PredProcId, !ProcInfo, !HLDS)
    ;
        SavedVarsCell = no
    ),
    globals.lookup_bool_option(Globals, follow_code, FollowCode),
    (
        FollowCode = yes,
        move_follow_code_in_proc(PredProcId, !ProcInfo, !HLDS)
    ;
        FollowCode = no
    ),
    find_simplifications(no, Globals, Simplifications0),
    SimpList0 = simplifications_to_list(Simplifications0),

    globals.lookup_bool_option(Globals, constant_propagation, ConstProp),
    globals.lookup_bool_option(Globals, profile_deep, DeepProf),
    globals.lookup_bool_option(Globals, record_term_sizes_as_words, TSWProf),
    globals.lookup_bool_option(Globals, record_term_sizes_as_cells, TSCProf),
    ProfTrans = bool.or_list([DeepProf, TSWProf, TSCProf]),

    % Don't run constant propagation if any of the profiling
    % transformations has been applied.
    %
    % NOTE: Any changes here may also need to be made to
    % mercury_compile.simplify.

    (
        ConstProp = yes,
        ProfTrans = no
    ->
        list.cons(simp_constant_prop, SimpList0, SimpList1)
    ;
        SimpList1 = list.delete_all(SimpList0, simp_constant_prop)
    ),

    SimpList = [simp_do_once, simp_elim_removable_scopes | SimpList1],
    Simplifications = list_to_simplifications(SimpList),
    write_proc_progress_message("% Simplifying ", PredId, ProcId,
        !.HLDS, !IO),
    simplify_proc(Simplifications, PredId, ProcId, !HLDS, !ProcInfo),
    write_proc_progress_message("% Computing liveness in ", PredId, ProcId,
        !.HLDS, !IO),
    detect_liveness_proc(!.HLDS, PredProcId, !ProcInfo),
    globals.lookup_bool_option(Globals, exec_trace_tail_rec, ExecTraceTailRec),
    globals.lookup_bool_option(Globals, warn_non_tail_recursion,
        WarnTailCalls),
    MarkTailCalls = bool.or(ExecTraceTailRec, WarnTailCalls),
    (
        MarkTailCalls = yes,
        write_proc_progress_message(
            "% Marking directly tail recursive calls in ", PredId, ProcId,
            !.HLDS, !IO),
        mark_tail_calls(feature_debug_tail_rec_call, !.HLDS, PredProcId,
            PredInfo, !ProcInfo)
    ;
        MarkTailCalls = no
    ),
    (
        WarnTailCalls = yes,
        warn_non_tail_calls_in_proc(Globals, PredId, ProcId, PredInfo,
            !.ProcInfo, !IO)
    ;
        WarnTailCalls = no
    ),
    write_proc_progress_message("% Allocating stack slots in ", PredId,
        ProcId, !.HLDS, !IO),
    allocate_stack_slots_in_proc(!.HLDS, PredProcId, !ProcInfo),
    write_proc_progress_message(
        "% Allocating storage locations for live vars in ",
        PredId, ProcId, !.HLDS, !IO),
    allocate_store_maps(final_allocation, !.HLDS, PredProcId, !ProcInfo),
    write_proc_progress_message("% Generating low-level (LLDS) code for ",
        PredId, ProcId, !.HLDS, !IO),
    generate_proc_code(!.HLDS, ConstStructMap, PredId, PredInfo,
         ProcId, !.ProcInfo, !GlobalData, ProcCode0),
    globals.lookup_bool_option(Globals, optimize, Optimize),
    (
        Optimize = yes,
        optimize_proc(Globals, !.GlobalData, ProcCode0, ProcCode)
    ;
        Optimize = no,
        ProcCode = ProcCode0
    ),
    Instructions = ProcCode ^ cproc_code,
    write_proc_progress_message(
        "% Generating call continuation information for ",
        PredId, ProcId, !.HLDS, !IO),
    continuation_info.maybe_process_proc_llds(Instructions, PredProcId,
        !.HLDS, !GlobalData).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% The various passes of the LLDS backend.
%

map_args_to_regs(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Mapping args to regs...", !IO),
    maybe_flush_output(Verbose, !IO),
    generate_arg_info(!HLDS),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred maybe_saved_vars(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_saved_vars(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, optimize_saved_vars_const, SavedVars),
    (
        SavedVars = yes,
        maybe_write_string(Verbose,
            "% Minimizing variable saves using constants...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_module(saved_vars_proc), !HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        SavedVars = no
    ).

:- pred maybe_stack_opt(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_stack_opt(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, optimize_saved_vars_cell, SavedVars),
    (
        SavedVars = yes,
        maybe_write_string(Verbose,
            "% Minimizing variable saves using cells...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_module(stack_opt_cell), !HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        SavedVars = no
    ).

:- pred maybe_followcode(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_followcode(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, follow_code, FollowCode),
    (
        FollowCode = yes,
        maybe_write_string(Verbose, "% Migrating branch code...", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_module(move_follow_code_in_proc),
            !HLDS),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        FollowCode = no
    ).

:- pred compute_liveness(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

compute_liveness(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, parallel_liveness, ParallelLiveness),
    globals.lookup_int_option(Globals, debug_liveness, DebugLiveness),
    maybe_write_string(Verbose, "% Computing liveness...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    (
        ParallelLiveness = yes,
        DebugLiveness = -1
    ->
        detect_liveness_preds_parallel(!HLDS)
    ;
        process_all_nonimported_procs(update_proc_ids(detect_liveness_proc),
            !HLDS)
    ),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred maybe_mark_tail_rec_calls(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_mark_tail_rec_calls(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, exec_trace_tail_rec, ExecTraceTailRec),
    globals.lookup_bool_option(Globals, warn_non_tail_recursion,
        WarnTailCalls),
    MarkTailCalls = bool.or(ExecTraceTailRec, WarnTailCalls),
    (
        MarkTailCalls = yes,
        maybe_write_string(Verbose,
            "% Marking directly tail recursive calls...", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(
            update_proc_ids_pred(mark_tail_calls(feature_debug_tail_rec_call)),
            !HLDS),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        MarkTailCalls = no
    ).

:- pred maybe_warn_non_tail_recursion(bool::in, bool::in,
    module_info::in, io::di, io::uo) is det.

maybe_warn_non_tail_recursion(Verbose, Stats, HLDS, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, warn_non_tail_recursion,
        WarnTailCalls),
    (
        WarnTailCalls = yes,
        maybe_write_string(Verbose,
            "% Warning about non-tail recursive calls...\n", !IO),
        warn_non_tail_calls(HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        WarnTailCalls = no
    ).

:- pred compute_stack_vars(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

compute_stack_vars(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Computing stack vars...", !IO),
    maybe_flush_output(Verbose, !IO),
    process_all_nonimported_procs(
        update_proc_ids(allocate_stack_slots_in_proc), !HLDS),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred allocate_store_map(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

allocate_store_map(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Allocating store map...", !IO),
    maybe_flush_output(Verbose, !IO),
    process_all_nonimported_procs(
        update_proc_ids(allocate_store_maps(final_allocation)), !HLDS),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred generate_llds_code_for_module(module_info::in, bool::in, bool::in,
    global_data::in, global_data::out, list(c_procedure)::out,
    io::di, io::uo) is det.

generate_llds_code_for_module(HLDS, Verbose, Stats, !GlobalData, LLDS, !IO) :-
    maybe_write_string(Verbose, "% Generating code...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    generate_module_code(HLDS, _HLDS, !GlobalData, LLDS, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred maybe_optimize_llds(module_info::in, global_data::in,
    bool::in, bool::in, list(c_procedure)::in, list(c_procedure)::out,
    io::di, io::uo) is det.

maybe_optimize_llds(HLDS, GlobalData, Verbose, Stats, !LLDS, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, optimize, Optimize),
    (
        Optimize = yes,
        maybe_write_string(Verbose, "% Doing optimizations...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        optimize_procs(Globals, GlobalData, !LLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Optimize = no
    ).

:- pred maybe_generate_stack_layouts(module_info::in, list(c_procedure)::in,
    bool::in, bool::in, global_data::in, global_data::out, io::di, io::uo)
    is det.

maybe_generate_stack_layouts(HLDS, LLDS, Verbose, Stats, !GlobalData, !IO) :-
    maybe_write_string(Verbose,
        "% Generating call continuation information...", !IO),
    maybe_flush_output(Verbose, !IO),
    continuation_info.maybe_process_llds(LLDS, HLDS, !GlobalData),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

llds_output_pass(HLDS, GlobalData0, Procs, ModuleName, Succeeded,
        FactTableObjFiles, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_write_string(Verbose,
        "% Generating layout data...", !IO),
    maybe_flush_output(Verbose, !IO),
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
        TableIoDecls, TableIoDeclMap, ProcEventLayouts,
        ExecTraces, ProcLayoutDatas, ModuleLayoutDatas),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO),

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

    C_InterfaceInfo = foreign_interface_info(ModuleSymName, C_HeaderCode0,
        C_Includes, C_BodyCode0, _C_ExportDecls, C_ExportDefns),
    MangledModuleName = sym_name_mangle(ModuleSymName),
    CModuleName = MangledModuleName ++ "_module",
    get_c_body_code(C_BodyCode0, C_BodyCode),

    % Split the code up into bite-size chunks for the C compiler.
    %
    globals.lookup_int_option(Globals, procs_per_c_function, ProcsPerFunc),
    ( ProcsPerFunc = 0 ->
        % ProcsPerFunc = 0 really means infinity -
        % we store all the procs in a single function.
        ChunkedModules = [comp_gen_c_module(CModuleName, Procs)]
    ;
        list.chunk(Procs, ProcsPerFunc, ChunkedProcs),
        combine_chunks(ChunkedProcs, CModuleName, ChunkedModules)
    ),
    list.map_foldl(make_foreign_import_header_code(Globals), C_Includes,
        C_IncludeHeaderCode, !IO),

    % The lists are reversed because insertions into them are at the front.
    % We don't want to put C_LocalHeaderCode between Start and End, because
    % C_IncludeHeaderCode may include our own header file, which defines
    % the module's guard macro, which in turn #ifdefs out the stuff between
    % Start and End.
    list.filter(foreign_decl_code_is_local, list.reverse(C_HeaderCode0),
        C_LocalHeaderCode, C_ExportedHeaderCode),
    make_decl_guards(ModuleSymName, Start, End),
    C_HeaderCode = list.reverse(C_IncludeHeaderCode) ++
        C_LocalHeaderCode ++ [Start | C_ExportedHeaderCode] ++ [End],

    module_info_user_init_pred_c_names(HLDS, UserInitPredCNames),
    module_info_user_final_pred_c_names(HLDS, UserFinalPredCNames),

    CFile = c_file(ModuleSymName, C_HeaderCode, C_BodyCode, C_ExportDefns,
        TablingInfoStructs, ScalarCommonCellDatas, VectorCommonCellDatas,
        RttiDatas, PseudoTypeInfos, HLDSVarNums, ShortLocns, LongLocns,
        UserEventVarNums, UserEvents,
        NoVarLabelLayouts, SVarLabelLayouts, LVarLabelLayouts,
        InternalLabelToLayoutMap, ProcLabelToLayoutMap,
        CallSites, CoveragePoints, ProcStatics,
        ProcHeadVarNums, ProcVarNames, ProcBodyBytecodes, TSStringTable,
        TableIoDecls, TableIoDeclMap, ProcEventLayouts, ExecTraces,
        ProcLayoutDatas, ModuleLayoutDatas, ClosureLayoutDatas,
        AllocSites, AllocIdMap, ChunkedModules,
        UserInitPredCNames, UserFinalPredCNames, ComplexityProcs),

    output_llds_file(Globals, CFile, TargetCodeSucceeded, !IO),
    (
        TargetCodeSucceeded = yes,

        C_InterfaceInfo = foreign_interface_info(_, _, _, _, C_ExportDecls, _),
        export.produce_header_file(HLDS, C_ExportDecls, ModuleName, !IO),

        % Finally we invoke the C compiler to compile it.
        globals.lookup_bool_option(Globals, target_code_only, TargetCodeOnly),
        (
            TargetCodeOnly = no,
            io.output_stream(OutputStream, !IO),
            llds_c_to_obj(Globals, OutputStream, ModuleName, CompileOK, !IO),
            module_get_fact_table_files(HLDS, FactTableBaseFiles),
            list.map2_foldl(compile_fact_table_file(Globals, OutputStream),
                FactTableBaseFiles, FactTableObjFiles, FactTableCompileOKs,
                !IO),
            bool.and_list([CompileOK | FactTableCompileOKs], Succeeded),
            maybe_set_exit_status(Succeeded, !IO)
        ;
            TargetCodeOnly = yes,
            Succeeded = yes,
            FactTableObjFiles = []
        )
    ;
        TargetCodeSucceeded = no,
        Succeeded = no,
        FactTableObjFiles = []
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

llds_get_c_interface_info(HLDS, UseForeignLanguage, Foreign_InterfaceInfo) :-
    module_info_get_name(HLDS, ModuleName),
    module_info_get_foreign_decl(HLDS, ForeignDecls),
    module_info_get_foreign_import_module(HLDS, ForeignImports0),

    % Always include the module we are compiling amongst the foreign import
    % modules so that pragma foreign_exported procedures are visible to
    % foreign code in this module.
    %
    % XXX The frontend should really handle this but it is quite
    % inconsistent in its treatement of self-imports.  Both this backend
    % (the LLDS) and the MLDS backend currently handle self foreign imports
    % directly.

    ForeignSelfImport = foreign_import_module_info(UseForeignLanguage,
        ModuleName, term.context_init),
    ForeignImports = [ ForeignSelfImport | ForeignImports0 ],
    module_info_get_foreign_body_code(HLDS, ForeignBodyCode),
    foreign.filter_decls(UseForeignLanguage, ForeignDecls,
        WantedForeignDecls, _OtherDecls),
    foreign.filter_imports(UseForeignLanguage, ForeignImports,
        WantedForeignImports, _OtherImports),
    foreign.filter_bodys(UseForeignLanguage, ForeignBodyCode,
        WantedForeignBodys, _OtherBodys),
    export.get_foreign_export_decls(HLDS, Foreign_ExportDecls),
    export.get_foreign_export_defns(HLDS, Foreign_ExportDefns),

    Foreign_InterfaceInfo = foreign_interface_info(ModuleName,
        WantedForeignDecls, WantedForeignImports,
        WantedForeignBodys, Foreign_ExportDecls, Foreign_ExportDefns).

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
        literal(Start), term.context_init),
    EndGuard = foreign_decl_code(lang_c, foreign_decl_is_exported,
        literal(End), term.context_init).

:- pred make_foreign_import_header_code(globals::in,
    foreign_import_module_info::in, foreign_decl_code::out,
    io::di, io::uo) is det.

make_foreign_import_header_code(Globals, ForeignImportModule, Include, !IO) :-
    ForeignImportModule = foreign_import_module_info(Lang, ModuleName,
        Context),
    (
        Lang = lang_c,
        module_name_to_search_file_name(Globals, ModuleName, ".mh",
            HeaderFileName, !IO),
        IncludeString = "#include """ ++ HeaderFileName ++ """\n",
        Include = foreign_decl_code(lang_c, foreign_decl_is_exported,
            literal(IncludeString), Context)
    ;
        Lang = lang_csharp,
        sorry($module, $pred, ":- import_module not yet implemented: " ++
            "`:- pragma foreign_import_module' for C#")
    ;
        Lang = lang_il,
        sorry($module, $pred, ":- import_module not yet implemented: " ++
            "`:- pragma foreign_import_module' for IL")
    ;
        Lang = lang_java,
        sorry($module, $pred, ":- import_module not yet implemented: " ++
            "`:- pragma foreign_import_module' for Java")
    ;
        Lang = lang_erlang,
        sorry($module, $pred, ":- import_module not yet implemented: " ++
            "`:- pragma foreign_import_module' for Erlang")
    ).

:- pred get_c_body_code(foreign_body_info::in, list(user_foreign_code)::out)
    is det.

get_c_body_code([], []).
get_c_body_code([foreign_body_code(Lang, Code, Context) | CodesAndContexts],
        [user_foreign_code(Lang, Code, Context) | C_Modules]) :-
    get_c_body_code(CodesAndContexts, C_Modules).

:- pred combine_chunks(list(list(c_procedure))::in, string::in,
    list(comp_gen_c_module)::out) is det.

combine_chunks(ChunkList, ModName, Modules) :-
    combine_chunks_2(ChunkList, ModName, 0, Modules).

:- pred combine_chunks_2(list(list(c_procedure))::in,
    string::in, int::in, list(comp_gen_c_module)::out) is det.

combine_chunks_2([], _ModName, _N, []).
combine_chunks_2([Chunk | Chunks], ModuleName, Num, [Module | Modules]) :-
    string.int_to_string(Num, NumString),
    ThisModuleName = ModuleName ++ NumString,
    Module = comp_gen_c_module(ThisModuleName, Chunk),
    Num1 = Num + 1,
    combine_chunks_2(Chunks, ModuleName, Num1, Modules).

:- pred output_llds_file(globals::in, c_file::in, bool::out, io::di, io::uo)
    is det.

output_llds_file(Globals, LLDS0, Succeeded, !IO) :-
    transform_llds(Globals, LLDS0, LLDS),
    output_llds(Globals, LLDS, Succeeded, !IO).

:- pred llds_c_to_obj(globals::in, io.output_stream::in, module_name::in,
    bool::out, io::di, io::uo) is det.

llds_c_to_obj(Globals, ErrorStream, ModuleName, Succeeded, !IO) :-
    get_linked_target_type(Globals, LinkedTargetType),
    get_object_code_type(Globals, LinkedTargetType, PIC),
    maybe_pic_object_file_extension(Globals, PIC, Obj),
    module_name_to_file_name(Globals, ModuleName, ".c", do_not_create_dirs,
        C_File, !IO),
    module_name_to_file_name(Globals, ModuleName, Obj, do_create_dirs,
        O_File, !IO),
    compile_target_code.do_compile_c_file(ErrorStream, PIC, C_File, O_File,
        Globals, Succeeded, !IO).

:- pred compile_fact_table_file(globals::in, io.output_stream::in, string::in,
    string::out, bool::out, io::di, io::uo) is det.

compile_fact_table_file(Globals, ErrorStream, BaseName, O_File, Succeeded,
        !IO) :-
    get_linked_target_type(Globals, LinkedTargetType),
    get_object_code_type(Globals, LinkedTargetType, PIC),
    maybe_pic_object_file_extension(Globals, PIC, Obj),
    C_File = BaseName ++ ".c",
    O_File = BaseName ++ Obj,
    compile_target_code.do_compile_c_file(ErrorStream, PIC, C_File, O_File,
        Globals, Succeeded, !IO).

%-----------------------------------------------------------------------------%
:- end_module top_level.mercury_compile_llds_back_end.
%-----------------------------------------------------------------------------%
