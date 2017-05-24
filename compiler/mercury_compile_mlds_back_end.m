%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mercury_compile_mlds_back_end.m.
%
% This module implements the MLDS backend for the top level of the Mercury
% compiler. It invokes the different passes of this backend as appropriate.
%
%---------------------------------------------------------------------------%

:- module top_level.mercury_compile_mlds_back_end.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.passes_aux.
:- import_module libs.
:- import_module libs.globals.
:- import_module ml_backend.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.

    % Return `yes' iff this module defines the main/2 entry point.
    %
:- func mlds_has_main(mlds) = has_main.

:- pred mlds_backend(module_info::in, module_info::out, mlds::out,
    list(error_spec)::out, dump_info::in, dump_info::out, io::di, io::uo)
    is det.

:- pred maybe_mark_static_terms(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

:- pred mlds_to_high_level_c(globals::in, mlds::in, bool::out,
    io::di, io::uo) is det.

:- pred mlds_to_java(module_info::in, mlds::in, bool::out,
    io::di, io::uo) is det.

:- pred mlds_to_csharp(module_info::in, mlds::in, bool::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.base_typeclass_info.
:- import_module backend_libs.type_class_info.
:- import_module backend_libs.type_ctor_info.
:- import_module hlds.mark_static_terms.            % HLDS -> HLDS
:- import_module libs.file_util.
:- import_module libs.options.
:- import_module ml_backend.add_trail_ops.          % HLDS -> HLDS
:- import_module ml_backend.add_heap_ops.           % HLDS -> HLDS
:- import_module ml_backend.ml_proc_gen.            % HLDS -> MLDS
:- import_module ml_backend.rtti_to_mlds.           % HLDS/RTTI -> MLDS
:- import_module ml_backend.ml_elim_nested.         % MLDS -> MLDS
:- import_module ml_backend.ml_tailcall.            % MLDS -> MLDS
:- import_module ml_backend.ml_optimize.            % MLDS -> MLDS
:- import_module ml_backend.mlds_to_c.              % MLDS -> C
:- import_module ml_backend.mlds_to_java.           % MLDS -> Java
:- import_module ml_backend.mlds_to_cs.             % MLDS -> C#
:- import_module ml_backend.ml_util.                % MLDS utility predicates
:- import_module parse_tree.file_names.
:- import_module top_level.mercury_compile_front_end.
:- import_module top_level.mercury_compile_llds_back_end.

:- import_module getopt_io.
:- import_module pprint.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

mlds_has_main(MLDS) =
    ( if
        Defns = MLDS ^ mlds_proc_defns,
        defns_contain_main(Defns)
    then
        has_main
    else
        no_main
    ).

%---------------------------------------------------------------------------%

mlds_backend(!HLDS, !:MLDS, Specs, !DumpInfo, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_simplify(no, simplify_pass_ml_backend, Verbose, Stats, !HLDS, [],
        SimplifySpecs, !IO),
    expect(unify(contains_errors(Globals, SimplifySpecs), no), $module, $pred,
        "simplify has errors"),
    maybe_dump_hlds(!.HLDS, 405, "ml_backend_simplify", !DumpInfo, !IO),

    % NOTE: it is unsafe for passes after add_trail_ops to reorder
    % disjunctions if trail usage has been optimized.  Such reordering
    % may result in the trail being corrupted.
    %
    maybe_add_trail_ops(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 410, "add_trail_ops", !DumpInfo, !IO),

    maybe_add_heap_ops(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 415, "add_heap_ops", !DumpInfo, !IO),

    maybe_mark_static_terms(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 420, "mark_static", !DumpInfo, !IO),

    % We need to do map_args_to_regs, even though that module is meant
    % for the LLDS back-end, because with the MLDS back-end the arg_infos
    % that map_args_to_regs generates are used by continuation_info.m,
    % which is used by ml_unify_gen.m when outputting closure layout structs.
    map_args_to_regs(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 425, "args_to_regs", !DumpInfo, !IO),

    maybe_write_string(Verbose, "% Converting HLDS to MLDS...\n", !IO),
    ml_code_gen(!HLDS, !:MLDS),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO),
    maybe_dump_hlds(!.HLDS, 499, "final", !DumpInfo, !IO),
    maybe_dump_mlds(Globals, !.MLDS, 0, "initial", !IO),

    maybe_write_string(Verbose, "% Generating RTTI data...\n", !IO),
    mlds_gen_rtti_data(!.HLDS, !MLDS),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO),
    maybe_dump_mlds(Globals, !.MLDS, 10, "rtti", !IO),

    % Detection of tail calls needs to occur before the
    % chain_gc_stack_frame pass of ml_elim_nested, because we need to
    % unlink the stack frame from the stack chain before tail calls.
    globals.lookup_bool_option(Globals, optimize_tailcalls, OptimizeTailCalls),
    (
        OptimizeTailCalls = yes,
        maybe_write_string(Verbose, "% Detecting tail calls...\n", !IO),
        ml_mark_tailcalls(Globals, !.HLDS, Specs, !MLDS),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        OptimizeTailCalls = no,
        Specs = []
    ),
    maybe_report_stats(Stats, !IO),
    maybe_dump_mlds(Globals, !.MLDS, 20, "tailcalls", !IO),

    % Run the ml_optimize pass before ml_elim_nested, so that we eliminate
    % as many local variables as possible before the ml_elim_nested
    % transformations. We also want to do tail recursion optimization before
    % ml_elim_nested, since this means that the stack-handling code for
    % accurate GC will go outside the loop rather than inside the loop.
    %
    % However, we need to disable optimize_initializations, because
    % ml_elim_nested doesn't correctly handle code containing initializations.
    globals.lookup_bool_option(Globals, optimize, Optimize),
    (
        Optimize = yes,
        globals.set_option(optimize_initializations, bool(no),
            Globals, NoInitOptGlobals),
        maybe_write_string(Verbose, "% Optimizing MLDS...\n", !IO),
        mlds_optimize(NoInitOptGlobals, !MLDS),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        Optimize = no
    ),
    maybe_report_stats(Stats, !IO),
    maybe_dump_mlds(Globals, !.MLDS, 25, "optimize1", !IO),

    % Note that we call ml_elim_nested twice -- the first time to chain
    % the stack frames together, for accurate GC, and the second time to
    % flatten nested functions. These two passes are quite similar, but
    % must be done separately. Currently chaining the stack frames together
    % for accurate GC needs to be done first, because the code for doing that
    % can't handle the env_ptr references that the other pass generates.

    globals.get_gc_method(Globals, GC),
    (
        GC = gc_accurate,
        maybe_write_string(Verbose,
            "% Threading GC stack frames...\n", !IO),
        ml_elim_nested(chain_gc_stack_frames, Globals, !MLDS),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        ( GC = gc_automatic
        ; GC = gc_none
        ; GC = gc_boehm
        ; GC = gc_boehm_debug
        ; GC = gc_hgc
        )
    ),
    maybe_report_stats(Stats, !IO),
    maybe_dump_mlds(Globals, !.MLDS, 30, "gc_frames", !IO),

    globals.lookup_bool_option(Globals, gcc_nested_functions, NestedFuncs),
    (
        NestedFuncs = no,
        maybe_write_string(Verbose, "% Flattening nested functions...\n", !IO),
        ml_elim_nested(hoist_nested_funcs, Globals, !MLDS),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        NestedFuncs = yes
    ),
    maybe_report_stats(Stats, !IO),
    maybe_dump_mlds(Globals, !.MLDS, 35, "nested_funcs", !IO),

    % Run the ml_optimize pass again after ml_elim_nested, to do
    % optimize_initializations. (It may also help pick up some additional
    % optimization opportunities for the other optimizations in this pass.)
    (
        Optimize = yes,
        maybe_write_string(Verbose, "% Optimizing MLDS again...\n", !IO),
        mlds_optimize(Globals, !MLDS),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        Optimize = no
    ),
    maybe_report_stats(Stats, !IO),
    maybe_dump_mlds(Globals, !.MLDS, 40, "optimize2", !IO),

    maybe_dump_mlds(Globals, !.MLDS, 99, "final", !IO).

%---------------------------------------------------------------------------%

maybe_mark_static_terms(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, static_ground_cells, SGCells),
    (
        SGCells = yes,
        maybe_write_string(Verbose, "% Marking static ground terms...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_proc(mark_static_terms), !HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        SGCells = no
    ).

:- pred maybe_add_trail_ops(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_add_trail_ops(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, use_trail, UseTrail),
    (
        UseTrail = no,
        EmitTrailOps = no
    ;
        UseTrail = yes,
        globals.lookup_bool_option(Globals, disable_trail_ops,
            DisableTrailOps),
        (
            DisableTrailOps = yes,
            EmitTrailOps = no
        ;
            DisableTrailOps = no,
            EmitTrailOps = yes
        )
    ),
    (
        EmitTrailOps = yes,
        globals.lookup_bool_option(Globals, optimize_trail_usage, OptTrailUse),
        globals.get_target(Globals, Target),
        (
            Target = target_c,
            globals.lookup_bool_option(Globals, generate_trail_ops_inline,
                GenerateInline)
        ;
            % XXX Currently, we can only generate trail ops inline for
            % the C backends.
            %
            ( Target = target_csharp
            ; Target = target_java
            ; Target = target_erlang
            ),
            GenerateInline = no
        ),
        maybe_write_string(Verbose, "% Adding trailing operations...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(
            update_proc(add_trail_ops(OptTrailUse, GenerateInline)), !HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        EmitTrailOps = no
    ).

:- pred maybe_add_heap_ops(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_add_heap_ops(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_gc_method(Globals, GC),
    globals.lookup_bool_option(Globals, reclaim_heap_on_semidet_failure,
        SemidetReclaim),
    globals.lookup_bool_option(Globals, reclaim_heap_on_nondet_failure,
        NondetReclaim),
    ( if
        gc_is_conservative(GC) = yes
    then
        % We can't do heap reclamation with conservative GC.
        true
    else if
        SemidetReclaim = no,
        NondetReclaim = no
    then
        true
    else if
        SemidetReclaim = yes,
        NondetReclaim = yes
    then
        maybe_write_string(Verbose,
            "% Adding heap reclamation operations...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_proc(add_heap_ops), !HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    else
        Msg = "Sorry, not implemented: `--high-level-code' and just one of " ++
            "`--reclaim-heap-on-semidet-failure' and " ++
            "`--reclaim-heap-on-nondet-failure'. " ++
            "Use `--(no-)reclaim-heap-on-failure' instead.",
        write_error_pieces_plain(Globals, [words(Msg)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred mlds_gen_rtti_data(module_info::in, mlds::in, mlds::out) is det.

mlds_gen_rtti_data(HLDS, !MLDS) :-
    type_ctor_info.generate_rtti(HLDS, TypeCtorRtti),
    generate_base_typeclass_info_rtti(HLDS, TypeClassInfoRtti),

    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, new_type_class_rtti, NewTypeClassRtti),
    generate_type_class_info_rtti(HLDS, NewTypeClassRtti,
        NewTypeClassInfoRttiData),
    RttiDatas = TypeCtorRtti ++ TypeClassInfoRtti ++ NewTypeClassInfoRttiData,
    GlobalData0 = !.MLDS ^ mlds_global_defns,
    add_rtti_datas_to_mlds(HLDS, RttiDatas, GlobalData0, GlobalData),
    !MLDS ^ mlds_global_defns := GlobalData.

%---------------------------------------------------------------------------%
%
% The `--high-level-code' MLDS output pass.
%

mlds_to_high_level_c(Globals, MLDS, Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_write_string(Verbose, "% Converting MLDS to C...\n", !IO),
    output_c_mlds(MLDS, Globals, "", Succeeded, !IO),
    maybe_write_string(Verbose, "% Finished converting MLDS to C.\n", !IO),
    maybe_report_stats(Stats, !IO).

mlds_to_java(HLDS, MLDS, Succeeded, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_write_string(Verbose, "% Converting MLDS to Java...\n", !IO),
    output_java_mlds(HLDS, MLDS, Succeeded, !IO),
    maybe_write_string(Verbose, "% Finished converting MLDS to Java.\n", !IO),
    maybe_report_stats(Stats, !IO).

mlds_to_csharp(HLDS, MLDS, Succeeded, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_write_string(Verbose, "% Converting MLDS to C#...\n", !IO),
    output_csharp_mlds(HLDS, MLDS, Succeeded, !IO),
    maybe_write_string(Verbose, "% Finished converting MLDS to C#.\n", !IO),
    maybe_report_stats(Stats, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred maybe_dump_mlds(globals::in, mlds::in, int::in, string::in,
    io::di, io::uo) is det.

maybe_dump_mlds(Globals, MLDS, StageNum, StageName, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_accumulating_option(Globals, dump_mlds, DumpStages),
    globals.lookup_accumulating_option(Globals, verbose_dump_mlds,
        VerboseDumpStages),
    StageNumStr = stage_num_str(StageNum),
    ( if should_dump_stage(StageNum, StageNumStr, StageName, DumpStages) then
        maybe_write_string(Verbose, "% Dumping out MLDS as C...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        DumpSuffix = "_dump." ++ StageNumStr ++ "-" ++ StageName,
        output_c_mlds(MLDS, Globals, DumpSuffix, _Succeeded, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    else
        true
    ),
    ( if
        should_dump_stage(StageNum, StageNumStr, StageName, VerboseDumpStages)
    then
        maybe_write_string(Verbose, "% Dumping out raw MLDS...\n", !IO),
        ModuleName = mlds_get_module_name(MLDS),
        module_name_to_file_name(Globals, ModuleName, ".mlds_dump",
            do_create_dirs, BaseFileName, !IO),
        DumpFile = BaseFileName ++ "." ++ StageNumStr ++ "-" ++ StageName,
        dump_mlds(Globals, DumpFile, MLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    else
        true
    ).

:- pred dump_mlds(globals::in, string::in, mlds::in, io::di, io::uo) is det.

dump_mlds(Globals, DumpFile, MLDS, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),
    maybe_write_string(Verbose, "% Dumping out MLDS to `", !IO),
    maybe_write_string(Verbose, DumpFile, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_output(DumpFile, Res, !IO),
    (
        Res = ok(FileStream),
        io.set_output_stream(FileStream, OutputStream, !IO),
        pprint.write(80, pprint.to_doc(MLDS), !IO),
        io.nl(!IO),
        io.set_output_stream(OutputStream, _, !IO),
        io.close_output(FileStream, !IO),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Res = error(IOError),
        maybe_write_string(Verbose, "\n", !IO),
        ErrorMessage = "can't open file `" ++ DumpFile ++ "' for output: " ++
            io.error_message(IOError),
        report_error(ErrorMessage, !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module top_level.mercury_compile_mlds_back_end.
%---------------------------------------------------------------------------%
