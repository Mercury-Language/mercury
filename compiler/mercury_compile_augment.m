%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2017-2025 The Mercury Team.
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
:- import_module parse_tree.error_spec.
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
    list(module_name)::out, list(string)::out, list(error_spec)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module analysis.operations.
:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module backend_libs.export.
:- import_module backend_libs.link_target_code.
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
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.grab_modules.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_data.
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
        ModulesToLink, ExtraObjFiles, !:Specs, !HaveParseTreeMaps, !IO) :-
    ModuleName = ParseTreeSrc ^ pts_module_name,
    parse_tree_src_to_burdened_module_list(Globals, SourceFileName,
        ReadModuleErrors, MaybeTimestamp, ParseTreeSrc,
        !:Specs, BurdenedModules0),
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
        !Specs, !HaveParseTreeMaps, !IO).

%---------------------------------------------------------------------------%

    % For the MLDS->C and LLDS->C back-ends, we compile
    % each submodule to its own C file.
    %
:- pred augment_and_process_all_submodules(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, op_mode_augment::in,
    op_mode_invoked_by_mmc_make::in, maybe(timestamp)::in,
    list(burdened_module)::in, list(module_name)::out, list(string)::out,
    list(error_spec)::in, list(error_spec)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

augment_and_process_all_submodules(ProgressStream, ErrorStream, Globals,
        OpModeAugment, InvokedByMmcMake, MaybeTimestamp,
        BurdenedModules, ModulesToLink, ExtraObjFiles,
        !Specs, !HaveParseTreeMaps, !IO) :-
    list.map_foldl3(
        augment_and_process_module(ProgressStream, ErrorStream, Globals,
            OpModeAugment, InvokedByMmcMake, MaybeTimestamp),
        BurdenedModules, ExtraObjFileLists,
        !Specs, !HaveParseTreeMaps, !IO),
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
    list(error_spec)::in, list(error_spec)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

augment_and_process_module(ProgressStream, ErrorStream, Globals,
        OpModeAugment, InvokedByMmcMake, MaybeTimestamp,
        BurdenedModule, ExtraObjFiles, !Specs, !HaveParseTreeMaps, !IO) :-
    BurdenedModule = burdened_module(Baggage0, ParseTreeModuleSrc),
    check_module_interface_for_no_exports(Globals, ParseTreeModuleSrc, !Specs),
    % XXX STREAM We could switch from a general progress stream
    % to a module-specific progress stream.
    grab_qual_imported_modules_augment(ProgressStream, Globals,
        MaybeTimestamp, ParseTreeModuleSrc, AugCompUnit,
        Baggage0, Baggage, !HaveParseTreeMaps, !IO),
    Errors = Baggage ^ mb_errors,
    !:Specs = get_read_module_specs(Errors) ++ !.Specs,
    ( if set.is_empty(Errors ^ rm_fatal_errors) then
        process_augmented_module(ProgressStream, ErrorStream, Globals,
            OpModeAugment, InvokedByMmcMake, Baggage, AugCompUnit,
            ExtraObjFiles, no_prev_dump, _,
            !Specs, !HaveParseTreeMaps, !IO)
    else
        ExtraObjFiles = []
    ).

:- pred process_augmented_module(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    op_mode_augment::in, op_mode_invoked_by_mmc_make::in,
    module_baggage::in, aug_compilation_unit::in,
    list(string)::out, dump_info::in, dump_info::out,
    list(error_spec)::in, list(error_spec)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

process_augmented_module(ProgressStream, ErrorStream, Globals,
        OpModeAugment, InvokedByMmcMake, Baggage, AugCompUnit, ExtraObjFiles,
        !DumpInfo, !Specs, !HaveParseTreeMaps, !IO) :-
    make_hlds_pass(ProgressStream, ErrorStream, Globals,
        OpModeAugment, InvokedByMmcMake, Baggage, AugCompUnit,
        HLDS1, QualInfo, MaybeTimestampMap, UndefTypes, UndefModes,
        PreHLDSErrors, !DumpInfo, !Specs, !HaveParseTreeMaps, !IO),
    frontend_pass(ProgressStream, ErrorStream, OpModeAugment, QualInfo,
        UndefTypes, UndefModes, PreHLDSErrors, FrontEndErrors,
        HLDS1, HLDS20, !DumpInfo, !Specs, !IO),
    io.get_exit_status(ExitStatus, !IO),
    ( if
        PreHLDSErrors = no,
        FrontEndErrors = no,
        contains_errors(Globals, !.Specs) = no,
        ExitStatus = 0
    then
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
            output_trans_opt_file(ProgressStream, HLDS21, !Specs,
                !DumpInfo, !IO),
            ExtraObjFiles = []
        ;
            OpModeAugment = opmau_make_analysis_registry,
            prepare_for_intermodule_analysis(ProgressStream, Globals,
                Verbose, Stats, AnalysisSpecs, HLDS21, HLDS22, !IO),
            (
                AnalysisSpecs = [],
                output_analysis_file(ProgressStream, HLDS22, !Specs,
                    !DumpInfo, !IO)
            ;
                AnalysisSpecs = [_ | _],
                !:Specs = AnalysisSpecs ++ !.Specs
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
                    HLDS22, ExtraObjFiles, !Specs, !DumpInfo, !IO)
            ;
                AnalysisSpecs = [_ | _],
                !:Specs = AnalysisSpecs ++ !.Specs,
                ExtraObjFiles = []
            )
        )
    else
        % If the number of errors is > 0, make sure that the compiler
        % exits with a non-zero exit status.
        ( if ExitStatus = 0 then
            io.set_exit_status(1, !IO)
        else
            true
        ),
        ExtraObjFiles = []
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
    list(string)::out, list(error_spec)::in, list(error_spec)::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

after_front_end_passes(ProgressStream, ErrorStream, Globals,
        OpModeFrontAndMiddle, MaybeTopModule, MaybeTimestampMap, !.HLDS,
        ExtraObjFiles, !Specs, !DumpInfo, !IO) :-
    globals.lookup_bool_option(Globals, statistics, Stats),
    maybe_output_prof_call_graph(ProgressStream, Stats, !HLDS, !IO),
    middle_pass(ProgressStream, ErrorStream, OpModeFrontAndMiddle,
        !HLDS, !DumpInfo, !Specs, !IO),

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

    FrontEndErrors =
        contains_errors_or_warnings_treated_as_errors(Globals, !.Specs),
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
                !Specs, !DumpInfo, !IO)
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
    list(string)::out, list(error_spec)::in, list(error_spec)::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

choose_and_execute_backend_passes(ProgressStream, ErrorStream, Globals,
        OpModeCodeGen, ModuleName, MaybeTopModule, MaybeTimestampMap, !.HLDS,
        ExtraObjFiles, !Specs, !DumpInfo, !IO) :-
    globals.get_target(Globals, Target),
    (
        Target = target_csharp,
        mlds_backend(ProgressStream, !.HLDS, MLDS, !Specs, !DumpInfo, !IO),
        % mlds_to_csharp never goes beyond generating C# code.
        mlds_to_csharp(ProgressStream, !.HLDS, MLDS, Succeeded, !IO),
        ExtraObjFiles = []
    ;
        Target = target_java,
        mlds_backend(ProgressStream, !.HLDS, MLDS, !Specs, !DumpInfo, !IO),
        mlds_to_java(ProgressStream, !.HLDS, MLDS, TargetCodeSucceeded, !IO),
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
                % XXX LEGACY
                module_name_to_file_name(Globals, $pred,
                    ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java),
                    ModuleName, JavaFile, _JavaFileProposed),
                compile_java_files(Globals, ProgressStream,
                    JavaFile, [], Succeeded, !IO),
                maybe_set_exit_status(Succeeded, !IO)
            )
        ),
        ExtraObjFiles = []
    ;
        Target = target_c,
        % Produce the grade independent header file <module>.mh,
        % which contains function prototypes for the procedures
        % named by by foreign_export pragmas.
        export.get_foreign_export_decls(!.HLDS, ExportDecls),
        % NOTE Both ExportDecls and ModuleName are derived from !.HLDS,
        % but this is not true for the output_mh_header_file's other caller.
        export.output_mh_header_file(ProgressStream, !.HLDS, ExportDecls,
            ModuleName, !IO),
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        (
            HighLevelCode = yes,
            mlds_backend(ProgressStream, !.HLDS, MLDS, !Specs, !DumpInfo, !IO),
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
                    % XXX EXT Why not _create_dirs?
                    % XXX LEGACY
                    module_name_to_file_name(Globals, $pred,
                        ext_cur_ngs_gs(ext_cur_ngs_gs_target_c),
                        ModuleName, C_File, _C_FileProposed),
                    get_linked_target_type_for_c(Globals, TargetType),
                    get_object_code_type(Globals, TargetType, PIC),
                    maybe_pic_object_file_extension(PIC, ObjExt, _),
                    % XXX LEGACY
                    module_name_to_file_name_create_dirs(Globals, $pred,
                        ext_cur_ngs_gas(ObjExt), ModuleName,
                        O_File, _O_FileProposed, !IO),
                    do_compile_c_file(Globals, ProgressStream, PIC,
                        C_File, O_File, Succeeded, !IO),
                    maybe_set_exit_status(Succeeded, !IO)
                )
            ),
            ExtraObjFiles = []
        ;
            HighLevelCode = no,
            llds_backend_pass(ProgressStream, ErrorStream, !HLDS,
                GlobalData, LLDS, !DumpInfo, !IO),
            % llds_output_pass looks up the target_code_only option
            % to see whether it should generate object code, using the
            % same logic as the HighLevelCode = yes case above.
            % XXX Move that logic here, for symmetry.
            llds_output_pass(ProgressStream, OpModeCodeGen,
                !.HLDS, GlobalData, LLDS, ModuleName, Succeeded,
                ExtraObjFiles, !IO)
        )
    ),
    (
        Succeeded = succeeded,
        module_info_get_maybe_recompilation_info(!.HLDS, MaybeRecompInfo),
        ( if
            MaybeRecompInfo = yes(RecompInfo),
            MaybeTimestampMap = yes(TimestampMap)
        then
            construct_used_file_contents(!.HLDS, RecompInfo,
                MaybeTopModule, TimestampMap, UsedFileContents),
            write_usage_file(ProgressStream, !.HLDS, UsedFileContents, !IO)
        else
            true
        ),
        module_name_to_target_timestamp_file_name_create_dirs(Globals,
            ModuleName, TimestampFile, !IO),
        touch_file_datestamp(Globals, ProgressStream, TimestampFile,
            _Succeededs, !IO)
    ;
        Succeeded = did_not_succeed
        % An error should have been reported earlier.
    ).

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
