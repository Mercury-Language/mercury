%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2017-2026 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mercury_compile_augment.m.
% Main authors: fjh, zs.
%
% This module handles the op_modes used by most invocations of the compiler.
% The constraints on pass ordering are documented in
% compiler/notes/compiler_design.html.
%
%---------------------------------------------------------------------------%

:- module top_level.mercury_compile_augment.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_parse_tree.
:- import_module parse_tree.read_modules.
:- import_module recompilation.
:- import_module recompilation.check.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- pred augment_and_process_source_file(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, op_mode_augment::in,
    op_mode_invoked_by_mmc_make::in, file_name::in, maybe(timestamp)::in,
    read_module_errors::in, parse_tree_src::in,
    file_components_to_recompile::in,
    list(module_name)::out, list(string)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    maybe_written_specs::in, maybe_written_specs::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module analysis.operations.
:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module backend_libs.export.
:- import_module backend_libs.link_target_code_c.
:- import_module check_hlds.
:- import_module check_hlds.xml_documentation.
:- import_module hlds.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.passes_aux.
:- import_module libs.maybe_util.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.check_module_interface.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_names.
:- import_module parse_tree.grab_modules.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.write_error_spec.
:- import_module recompilation.usage.
:- import_module recompilation.used_file.
:- import_module top_level.mercury_compile_front_end.
:- import_module top_level.mercury_compile_llds_back_end.
:- import_module top_level.mercury_compile_make_hlds.
:- import_module top_level.mercury_compile_middle_passes.
:- import_module top_level.mercury_compile_mlds_back_end.

:- import_module bool.
:- import_module cord.
:- import_module getopt.
:- import_module io.file.
:- import_module map.
:- import_module set.

%---------------------------------------------------------------------------%

augment_and_process_source_file(ProgressStream, ErrorStream, Globals,
        OpModeAugment, InvokedByMmcMake, SourceFileName, MaybeTimestamp,
        ReadModuleErrors, ParseTreeSrc, MaybeModulesToRecompile,
        ModulesToLink, ExtraObjFiles,
        !HaveParseTreeMaps, !MaybeWrittenSpecs, !IO) :-
    ModuleName = ParseTreeSrc ^ pts_module_name,
    parse_tree_src_to_burdened_module_list(Globals, SourceFileName,
        ReadModuleErrors, MaybeTimestamp, ParseTreeSrc,
        SplitSpecs, BurdenedModules0),
    add_to_be_written_specs(SplitSpecs, !MaybeWrittenSpecs),
    (
        MaybeModulesToRecompile = some_file_components(ModulesToRecompile),
        ToRecompile =
            ( pred(BM::in) is semidet :-
                BM = burdened_module(_, PTMS),
                list.member(PTMS ^ ptms_module_name, ModulesToRecompile)
            ),
        list.filter(ToRecompile, BurdenedModules0,
            BurdenedModulesToRecompile)
    ;
        MaybeModulesToRecompile = all_file_components,
        BurdenedModulesToRecompile = BurdenedModules0
    ),

    globals.lookup_bool_option(Globals, trace_prof, TraceProf),
    ( if
        non_traced_mercury_builtin_module(ModuleName),
        not (
            ModuleName = mercury_profiling_builtin_module,
            TraceProf = yes
        )
    then
        % Some predicates in the builtin modules are missing typeinfo
        % arguments, which means that execution tracing will not work on them.
        % Predicates defined there should never be part of an execution trace
        % anyway; they are effectively language primitives.
        % (They may still be parts of stack traces.)
        globals.set_option(trace_stack_layout, bool(no), Globals, Globals1),
        globals.set_trace_level_none(Globals1, GlobalsToUse)
    else
        GlobalsToUse = Globals
    ),
    augment_and_process_all_submodules(ProgressStream, ErrorStream,
        GlobalsToUse, OpModeAugment, InvokedByMmcMake, MaybeTimestamp,
        BurdenedModulesToRecompile, ModulesToLink, ExtraObjFiles,
        !HaveParseTreeMaps, !MaybeWrittenSpecs, !IO).

%---------------------------------------------------------------------------%

    % For the MLDS->C and LLDS->C back-ends, we compile
    % each submodule to its own C file.
    %
:- pred augment_and_process_all_submodules(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, op_mode_augment::in,
    op_mode_invoked_by_mmc_make::in, maybe(timestamp)::in,
    list(burdened_module)::in, list(module_name)::out, list(string)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    maybe_written_specs::in, maybe_written_specs::out, io::di, io::uo) is det.

augment_and_process_all_submodules(ProgressStream, ErrorStream, Globals,
        OpModeAugment, InvokedByMmcMake, MaybeTimestamp,
        BurdenedModules, ModulesToLink, ExtraObjFiles,
        !HaveParseTreeMaps, !MaybeWrittenSpecs, !IO) :-
    list.map_foldl3(
        augment_and_process_module(ProgressStream, ErrorStream, Globals,
            OpModeAugment, InvokedByMmcMake, MaybeTimestamp),
        BurdenedModules, ExtraObjFileLists,
        !HaveParseTreeMaps, !MaybeWrittenSpecs, !IO),
    list.map(burdened_module_to_module_name, BurdenedModules, ModulesToLink),
    list.condense(ExtraObjFileLists, ExtraObjFiles).

:- pred burdened_module_to_module_name(burdened_module::in, module_name::out)
    is det.

burdened_module_to_module_name(BurdenedModule, ModuleName) :-
    BurdenedModule = burdened_module(_, ParseTreeModuleSrc),
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name.

%---------------------------------------------------------------------------%

    % Given the parse tree of a module, read in the interface and optimization
    % files it needs, and compile it.
    %
    % Stage number assignments:
    %
    %     1 to  99  front end pass
    %   100 to 299  middle pass
    %   300 to 399  LLDS back end pass
    %   400 to 499  MLDS back end pass
    %
    % The initial arrangement had the stage numbers increasing by five
    % so that new stages can be slotted in without too much trouble.
    %
:- pred augment_and_process_module(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, op_mode_augment::in,
    op_mode_invoked_by_mmc_make::in, maybe(timestamp)::in,
    burdened_module::in, list(string)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    maybe_written_specs::in, maybe_written_specs::out, io::di, io::uo) is det.

augment_and_process_module(ProgressStream, ErrorStream, Globals,
        OpModeAugment, InvokedByMmcMake, MaybeTimestamp,
        BurdenedModule, ExtraObjFiles,
        !HaveParseTreeMaps, !MaybeWrittenSpecs, !IO) :-
    BurdenedModule = burdened_module(Baggage0, ParseTreeModuleSrc),
    check_module_interface_for_no_exports(Globals, ParseTreeModuleSrc,
        NoExportSpecs),
    add_to_be_written_specs(NoExportSpecs, !MaybeWrittenSpecs),
    % XXX STREAM We could switch from a general progress stream
    % to a module-specific progress stream.
    grab_qual_imported_modules_augment(ProgressStream, Globals,
        MaybeTimestamp, ParseTreeModuleSrc, AugCompUnit,
        Baggage0, Baggage, !HaveParseTreeMaps, !IO),
    BaggageErrors = Baggage ^ mb_errors,
    BaggageSpecs = get_read_module_specs(BaggageErrors),
    add_to_be_written_specs(BaggageSpecs, !MaybeWrittenSpecs),
    ( if set.is_empty(BaggageErrors ^ rm_fatal_errors) then
        process_augmented_module(ProgressStream, ErrorStream, Globals,
            OpModeAugment, InvokedByMmcMake, Baggage, AugCompUnit,
            ExtraObjFiles, no_prev_dump, _,
            !HaveParseTreeMaps, !MaybeWrittenSpecs, !IO)
    else
        ExtraObjFiles = []
    ).

:- pred process_augmented_module(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    op_mode_augment::in, op_mode_invoked_by_mmc_make::in,
    module_baggage::in, aug_compilation_unit::in,
    list(string)::out, dump_info::in, dump_info::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    maybe_written_specs::in, maybe_written_specs::out, io::di, io::uo) is det.

process_augmented_module(ProgressStream, ErrorStream, Globals,
        OpModeAugment, InvokedByMmcMake, Baggage, AugCompUnit, ExtraObjFiles,
        !DumpInfo, !HaveParseTreeMaps, !MaybeWrittenSpecs, !IO) :-
    make_hlds_pass(ProgressStream, ErrorStream, Globals,
        OpModeAugment, InvokedByMmcMake, Baggage, AugCompUnit,
        HLDS1, QualInfo, MaybeTimestampMap, MakeHldsResult,
        !DumpInfo, !HaveParseTreeMaps, !MaybeWrittenSpecs, !IO),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_not_yet_written_specs(ErrorStream, Globals, Verbose,
        !MaybeWrittenSpecs, !IO),
    MakeHldsResult = make_hlds_result(InvalidTypeSpecs, InvalidInstModeSpecs,
        OptBlockingSpecs, ExpandSpecs, EventSetSpecs),
    % All of the errors in ExpandSpecs report errors that make some type
    % definition invalid.
    % Any errors with the definition of events are likely to lead to
    % spurious errors when typechecking event calls. Stopping before
    % typechecking will prevent the compiler from complaining about them.
    BlockingTypeSpecs = InvalidTypeSpecs ++ ExpandSpecs ++ EventSetSpecs,
    (
        BlockingTypeSpecs = [_ | _],
        % We can't continue after an undefined type error, since typecheck
        % would get internal errors.
        maybe_write_string(ProgressStream, Verbose,
            "% Program contains undefined type error(s).\n", !IO),
        ExtraObjFiles = [],
        io.set_exit_status(1, !IO)
    ;
        BlockingTypeSpecs = [],
        SpecsSoFar = maybe_written_specs_to_specs(!.MaybeWrittenSpecs),
        SemanticErrors = contains_errors(Globals, SpecsSoFar),
        OptBlockingErrors = contains_errors(Globals, OptBlockingSpecs),
        bool.or(SemanticErrors, OptBlockingErrors, PreHLDSErrors),
        frontend_pass(ProgressStream, ErrorStream, OpModeAugment, QualInfo,
            InvalidInstModeSpecs, PreHLDSErrors, FrontEndErrors,
            HLDS1, HLDS20, !DumpInfo, !MaybeWrittenSpecs, !IO),
        io.get_exit_status(ExitStatus, !IO),
        ( if
            PreHLDSErrors = no,
            FrontEndErrors = no,
            SpecsSofar = maybe_written_specs_to_specs(!.MaybeWrittenSpecs),
            contains_errors(Globals, SpecsSofar) = no,
            ExitStatus = 0
        then
            process_augmented_module_after_front_end(ProgressStream,
                ErrorStream, Globals, OpModeAugment, Baggage,
                MaybeTimestampMap, HLDS20, ExtraObjFiles,
                !DumpInfo, !MaybeWrittenSpecs, !IO)
        else
            % If the number of errors is > 0, make sure that the compiler
            % exits with a non-zero exit status. However, do not override
            % an exit status that may already be nonzero.
            ( if ExitStatus = 0 then
                io.set_exit_status(1, !IO)
            else
                true
            ),
            ExtraObjFiles = []
        )
    ).

:- pred process_augmented_module_after_front_end(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    op_mode_augment::in, module_baggage::in, maybe(module_timestamp_map)::in,
    module_info::in, list(string)::out, dump_info::in, dump_info::out,
    maybe_written_specs::in, maybe_written_specs::out, io::di, io::uo) is det.

process_augmented_module_after_front_end(ProgressStream, ErrorStream, Globals,
        OpModeAugment, Baggage, MaybeTimestampMap, HLDS20, ExtraObjFiles,
        !DumpInfo, !MaybeWrittenSpecs, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),
    maybe_write_dependency_graph(ProgressStream, Stats,
        HLDS20, HLDS21, !IO),
    (
        OpModeAugment = opmau_typecheck_only,
        ExtraObjFiles = []
    ;
        OpModeAugment = opmau_make_plain_opt,
        % Only run up to typechecking when making the .opt file.
        ExtraObjFiles = []
    ;
        OpModeAugment = opmau_make_trans_opt,
        output_trans_opt_file(ProgressStream, HLDS21, !DumpInfo,
            !MaybeWrittenSpecs, !IO),
        ExtraObjFiles = []
    ;
        OpModeAugment = opmau_make_analysis_registry,
        prepare_for_intermodule_analysis(ProgressStream, Globals,
            Verbose, Stats, AnalysisSpecs, HLDS21, HLDS22, !IO),
        (
            AnalysisSpecs = [],
            output_analysis_file(ProgressStream, HLDS22,
                !DumpInfo, !MaybeWrittenSpecs, !IO)
        ;
            AnalysisSpecs = [_ | _],
            add_to_be_written_specs(AnalysisSpecs, !MaybeWrittenSpecs)
        ),
        ExtraObjFiles = []
    ;
        OpModeAugment = opmau_make_xml_documentation,
        xml_documentation(ProgressStream, HLDS21, !IO),
        ExtraObjFiles = []
    ;
        OpModeAugment = opmau_front_and_middle(OpModeFrontAndMiddle),
        maybe_prepare_for_intermodule_analysis(ProgressStream, Globals,
            Verbose, Stats, AnalysisSpecs, HLDS21, HLDS22, !IO),
        (
            AnalysisSpecs = [],
            MaybeTopModule = Baggage ^ mb_maybe_top_module,
            after_front_end_passes(ProgressStream, ErrorStream, Globals,
                OpModeFrontAndMiddle, MaybeTopModule, MaybeTimestampMap,
                HLDS22, ExtraObjFiles, !DumpInfo, !MaybeWrittenSpecs, !IO)
        ;
            AnalysisSpecs = [_ | _],
            add_to_be_written_specs(AnalysisSpecs, !MaybeWrittenSpecs),
            ExtraObjFiles = []
        )
    ).

%---------------------------------------------------------------------------%

:- pred maybe_write_dependency_graph(io.text_output_stream::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_write_dependency_graph(ProgressStream, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, show_dependency_graph, ShowDepGraph),
    (
        ShowDepGraph = yes,
        dependency_graph_to_string(DepGraphStr, !HLDS),
        module_info_get_name(!.HLDS, ModuleName),
        module_name_to_cur_dir_file_name(ext_cur_user_depgraph, ModuleName,
            DepGraphFileName),
        write_string_to_file(ProgressStream, Globals,
            "Writing dependency graph", DepGraphFileName, DepGraphStr,
            _Succeeded, !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        ShowDepGraph = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_prepare_for_intermodule_analysis(io.text_output_stream::in,
    globals::in, bool::in, bool::in, list(error_spec)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_prepare_for_intermodule_analysis(ProgressStream, Globals,
        Verbose, Stats, Specs, !HLDS, !IO) :-
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    (
        IntermodAnalysis = yes,
        prepare_for_intermodule_analysis(ProgressStream, Globals,
            Verbose, Stats, Specs, !HLDS, !IO)
    ;
        IntermodAnalysis = no,
        Specs = []
    ).

:- pred prepare_for_intermodule_analysis(io.text_output_stream::in,
    globals::in, bool::in, bool::in, list(error_spec)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

prepare_for_intermodule_analysis(ProgressStream, Globals,
        Verbose, Stats, Specs, !HLDS, !IO) :-
    maybe_write_string(ProgressStream, Verbose,
        "% Preparing for intermodule analysis...\n", !IO),

    module_info_get_all_avail_modules(!.HLDS, AllAvailModules),

    globals.lookup_accumulating_option(Globals, local_module_id,
        LocalModulesList),
    SymNames = list.map(string_to_sym_name, LocalModulesList),
    LocalModuleNames = set.list_to_set(SymNames),

    module_info_get_analysis_info(!.HLDS, AnalysisInfo0),
    prepare_intermodule_analysis(ProgressStream, Globals, AllAvailModules,
        LocalModuleNames, Specs, AnalysisInfo0, AnalysisInfo, !IO),
    module_info_set_analysis_info(AnalysisInfo, !HLDS),

    maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

%---------------------------------------------------------------------------%

:- pred after_front_end_passes(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    op_mode_front_and_middle::in, maybe_top_module::in,
    maybe(module_timestamp_map)::in, module_info::in,
    list(string)::out, dump_info::in, dump_info::out,
    maybe_written_specs::in, maybe_written_specs::out, io::di, io::uo) is det.

after_front_end_passes(ProgressStream, ErrorStream, Globals,
        OpModeFrontAndMiddle, MaybeTopModule, MaybeTimestampMap, !.HLDS,
        ExtraObjFiles, !DumpInfo, !MaybeWrittenSpecs, !IO) :-
    globals.lookup_bool_option(Globals, statistics, Stats),
    maybe_output_prof_call_graph(ProgressStream, Stats, !HLDS, !IO),
    middle_pass(ProgressStream, ErrorStream, OpModeFrontAndMiddle,
        !HLDS, !DumpInfo, !MaybeWrittenSpecs, !IO),

    % Remove any existing `.used' file before writing the output file.
    % This avoids leaving the old `used' file lying around if compilation
    % is interrupted after the new output file is written but before the new
    % `.used' file is written.
    module_info_get_name(!.HLDS, ModuleName),
    % XXX LEGACY
    module_name_to_file_name(Globals, $pred,
        ext_cur_ngs_gs(ext_cur_ngs_gs_misc_used), ModuleName,
        UsageFileName, _UsageFileNameProposed),
    io.file.remove_file(UsageFileName, _, !IO),

    FrontEndSpecs = maybe_written_specs_to_specs(!.MaybeWrittenSpecs),
    FrontEndErrors =
        contains_errors_or_warnings_treated_as_errors(Globals, FrontEndSpecs),
    io.get_exit_status(ExitStatus, !IO),
    ( if
        FrontEndErrors = no,
        ExitStatus = 0
    then
        (
            OpModeFrontAndMiddle = opfam_errorcheck_only,
            ExtraObjFiles = []
        ;
            ( OpModeFrontAndMiddle = opfam_target_code_only
            ; OpModeFrontAndMiddle = opfam_target_and_object_code_only
            ; OpModeFrontAndMiddle = opfam_target_object_and_executable
            ),
            OpModeCodeGen = coerce(OpModeFrontAndMiddle),
            choose_and_execute_backend_passes(ProgressStream, ErrorStream,
                Globals, OpModeCodeGen, ModuleName, MaybeTopModule,
                MaybeTimestampMap, !.HLDS, ExtraObjFiles,
                !DumpInfo, !MaybeWrittenSpecs, !IO)
        )
    else
        % Make sure that the compiler exits with a non-zero exit status.
        ( if ExitStatus = 0 then
            io.set_exit_status(1, !IO)
        else
            true
        ),
        ExtraObjFiles = []
    ).

:- pred choose_and_execute_backend_passes(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    op_mode_codegen::in, module_name::in, maybe_top_module::in,
    maybe(module_timestamp_map)::in, module_info::in,
    list(string)::out, dump_info::in, dump_info::out,
    maybe_written_specs::in, maybe_written_specs::out, io::di, io::uo) is det.

choose_and_execute_backend_passes(ProgressStream, ErrorStream, Globals,
        OpModeCodeGen, ModuleName, MaybeTopModule, MaybeTimestampMap, !.HLDS,
        ExtraObjFiles, !DumpInfo, !MaybeWrittenSpecs, !IO) :-
    globals.get_target(Globals, Target),
    (
        Target = target_csharp,
        hlds_to_mlds(ProgressStream, !.HLDS, MLDS,
            !DumpInfo, !MaybeWrittenSpecs, !IO),
        % mlds_to_csharp never goes beyond generating C# code.
        mlds_to_csharp(ProgressStream, !.HLDS, MLDS, Succeeded, !IO),
        ExtraObjFiles = []
    ;
        Target = target_java,
        execute_mlds_java_backend(ProgressStream, Globals, OpModeCodeGen,
            ModuleName, !.HLDS, Succeeded, !DumpInfo, !MaybeWrittenSpecs, !IO),
        ExtraObjFiles = []
    ;
        Target = target_c,
        % Produce the grade independent header file <module>.mh,
        % which contains function prototypes for the procedures
        % named by by foreign_export pragmas.
        export.output_mh_header_file(ProgressStream, !.HLDS, !IO),
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        (
            HighLevelCode = yes,
            execute_mlds_c_backend(ProgressStream, Globals, OpModeCodeGen,
                ModuleName, !.HLDS, Succeeded,
                !DumpInfo, !MaybeWrittenSpecs, !IO),
            ExtraObjFiles = []
        ;
            HighLevelCode = no,
            execute_llds_c_backend(ProgressStream, ErrorStream, Globals,
                OpModeCodeGen, ModuleName, !HLDS, ExtraObjFiles, Succeeded,
                !DumpInfo, !MaybeWrittenSpecs, !IO)
        )
    ),
    (
        Succeeded = succeeded,
        update_recompilation_used_file(ProgressStream, Globals, ModuleName,
            MaybeTopModule, MaybeTimestampMap, !.HLDS, !IO)
    ;
        Succeeded = did_not_succeed
        % An error should have been reported earlier.
    ).

:- pred execute_mlds_java_backend(io.text_output_stream::in, globals::in,
    op_mode_codegen::in, module_name::in, module_info::in,
    maybe_succeeded::out, dump_info::in, dump_info::out,
    maybe_written_specs::in, maybe_written_specs::out, io::di, io::uo) is det.

execute_mlds_java_backend(ProgressStream, Globals, OpModeCodeGen,
        ModuleName, HLDS, Succeeded, !DumpInfo, !MaybeWrittenSpecs, !IO) :-
    hlds_to_mlds(ProgressStream, HLDS, MLDS,
        !DumpInfo, !MaybeWrittenSpecs, !IO),
    mlds_to_java(ProgressStream, HLDS, MLDS, TargetCodeSucceeded, !IO),
    (
        OpModeCodeGen = opfam_target_code_only,
        Succeeded = TargetCodeSucceeded
    ;
        ( OpModeCodeGen = opfam_target_and_object_code_only
        ; OpModeCodeGen = opfam_target_object_and_executable
        ),
        (
            TargetCodeSucceeded = did_not_succeed,
            Succeeded = did_not_succeed
        ;
            TargetCodeSucceeded = succeeded,
            ExtJava = ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java),
            % XXX LEGACY
            module_name_to_file_name(Globals, $pred, ExtJava,
                ModuleName, JavaFile, _JavaFileProposed),
            compile_java_files(ProgressStream, Globals, JavaFile, [],
                Succeeded, !IO),
            maybe_set_exit_status(Succeeded, !IO)
        )
    ).

:- pred execute_mlds_c_backend(io.text_output_stream::in, globals::in,
    op_mode_codegen::in, module_name::in, module_info::in,
    maybe_succeeded::out, dump_info::in, dump_info::out,
    maybe_written_specs::in, maybe_written_specs::out, io::di, io::uo) is det.

execute_mlds_c_backend(ProgressStream, Globals, OpModeCodeGen, ModuleName,
        HLDS, Succeeded, !DumpInfo, !MaybeWrittenSpecs, !IO) :-
        export.output_mh_header_file(ProgressStream, HLDS, !IO),
    hlds_to_mlds(ProgressStream, HLDS, MLDS,
        !DumpInfo, !MaybeWrittenSpecs, !IO),
    mlds_to_high_level_c(ProgressStream, Globals, MLDS,
        TargetCodeSucceeded, !IO),
    (
        OpModeCodeGen = opfam_target_code_only,
        Succeeded = TargetCodeSucceeded
    ;
        ( OpModeCodeGen = opfam_target_and_object_code_only
        ; OpModeCodeGen = opfam_target_object_and_executable
        ),
        (
            TargetCodeSucceeded = did_not_succeed,
            Succeeded = did_not_succeed
        ;
            TargetCodeSucceeded = succeeded,
            get_linked_target_type_for_c(Globals, TargetType),
            get_object_code_type(Globals, TargetType, PIC),
            maybe_pic_object_file_extension(PIC, ObjExt, _),
            ExtC = ext_cur_ngs_gs(ext_cur_ngs_gs_target_c),
            ExtObj = ext_cur_ngs_gas(ObjExt),
            % XXX EXT Why not _create_dirs?
            % XXX LEGACY
            module_name_to_file_name(Globals, $pred, ExtC,
                ModuleName, C_File, _C_FileProposed),
            % XXX LEGACY
            module_name_to_file_name_create_dirs(Globals, $pred, ExtObj,
                ModuleName, O_File, _O_FileProposed, !IO),
            do_compile_c_file(ProgressStream, Globals, PIC, C_File, O_File,
                Succeeded, !IO),
            maybe_set_exit_status(Succeeded, !IO)
        )
    ).

:- pred execute_llds_c_backend(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    op_mode_codegen::in, module_name::in, module_info::in, module_info::out,
    list(string)::out, maybe_succeeded::out, dump_info::in, dump_info::out,
    maybe_written_specs::in, maybe_written_specs::out, io::di, io::uo) is det.

execute_llds_c_backend(ProgressStream, ErrorStream, Globals, OpModeCodeGen,
        ModuleName, !HLDS, ExtraObjFiles, Succeeded,
        !DumpInfo, !MaybeWrittenSpecs, !IO) :-
    hlds_to_llds(ProgressStream, ErrorStream, !HLDS,
        GlobalData, LLDS, !DumpInfo, !MaybeWrittenSpecs, !IO),
    llds_to_c(ProgressStream, !.HLDS, GlobalData, LLDS,
        TargetCodeSucceeded, !IO),
    (
        TargetCodeSucceeded = succeeded,
        (
            OpModeCodeGen = opfam_target_code_only,
            Succeeded = succeeded,
            ExtraObjFiles = []
        ;
            ( OpModeCodeGen = opfam_target_and_object_code_only
            ; OpModeCodeGen = opfam_target_object_and_executable
            ),
            llds_c_to_obj(ProgressStream, Globals, ModuleName,
                CompileSucceeded, !IO),
            module_info_get_fact_table_file_names(!.HLDS, FactTableBaseFiles),
            list.map2_foldl(
                fact_table_file_to_obj(ProgressStream, Globals),
                FactTableBaseFiles, FactTableObjFiles,
                FactTableCompileSucceededs, !IO),
            ExtraObjFiles = FactTableObjFiles,
            Succeeded = and_list([CompileSucceeded |
                FactTableCompileSucceededs]),
            maybe_set_exit_status(Succeeded, !IO)
        )
    ;
        TargetCodeSucceeded = did_not_succeed,
        Succeeded = did_not_succeed,
        ExtraObjFiles = []
    ).

:- pred update_recompilation_used_file(io.text_output_stream::in, globals::in,
    module_name::in, maybe_top_module::in, maybe(module_timestamp_map)::in,
    module_info::in, io::di, io::uo) is det.

update_recompilation_used_file(ProgressStream, Globals, ModuleName,
        MaybeTopModule, MaybeTimestampMap, HLDS, !IO) :-
    module_info_get_maybe_recompilation_info(HLDS, MaybeRecompInfo),
    ( if
        MaybeRecompInfo = yes(RecompInfo),
        MaybeTimestampMap = yes(TimestampMap)
    then
        construct_used_file_contents(HLDS, RecompInfo, MaybeTopModule,
            TimestampMap, UsedFileContents),
        write_usage_file(ProgressStream, HLDS, UsedFileContents, !IO)
    else
        true
    ),
    module_name_to_target_timestamp_file_name_create_dirs(Globals, ModuleName,
        TimestampFile, !IO),
    touch_file_datestamp(ProgressStream, Globals, TimestampFile,
        _Succeeded, !IO).

%---------------------%

    % Outputs the file <module_name>.prof, which contains the static
    % call graph in terms of label names, if the profiling flag is enabled.
    %
:- pred maybe_output_prof_call_graph(io.text_output_stream::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_output_prof_call_graph(ProgressStream, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, profile_calls, ProfileCalls),
    globals.lookup_bool_option(Globals, profile_time, ProfileTime),
    ( if
        ( ProfileCalls = yes
        ; ProfileTime = yes
        )
    then
        module_info_get_name(!.HLDS, ModuleName),
        % XXX LEGACY
        module_name_to_file_name_create_dirs(Globals, $pred,
            ext_cur_ngs(ext_cur_ngs_misc_call_graph_for_prof), ModuleName,
            ProfFileName, _ProfFileNameProposed, !IO),
        prof_dependency_graph_to_string(DepGraphStr, !HLDS),
        write_string_to_file(ProgressStream, Globals,
            "Writing profiling call graph", ProfFileName, DepGraphStr,
            _Succeeded, !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module top_level.mercury_compile_augment.
%---------------------------------------------------------------------------%
