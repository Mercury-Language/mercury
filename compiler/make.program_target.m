%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.program_target.m.
% Main author: stayl.
%
% Build targets which relate to whole programs or libraries.
%
%---------------------------------------------------------------------------%

:- module make.program_target.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.
:- import_module pair.

%---------------------------------------------------------------------------%

    % make_linked_target(Globals, Target, Succeeded, !Info, !Specs, !IO):
    %
    % Build a library or an executable.
    %
:- pred make_linked_target(globals::in, linked_target_file::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

    % make_misc_target(Globals, Target, Succeeded, !Info, !Specs, !IO):
    %
    % Handle miscellaneous target types, including clean-up, library
    % installation, and building all files of a given type for all
    % modules in the program.
    %
:- pred make_misc_target(globals::in, pair(module_name, misc_target_type)::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

    % install_library_grade(LinkSucceeded0, ModuleName, AllModules,
    %   Globals, Grade, Succeeded, !Info, !IO)
    %
:- pred install_library_grade(maybe_succeeded::in,
    module_name::in, list(module_name)::in,
    globals::in, string::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.check_libgrades.
:- import_module libs.compute_grade.
:- import_module libs.file_util.
:- import_module libs.handle_options.
:- import_module libs.options.
:- import_module libs.process_util.
:- import_module libs.shell_util.
:- import_module libs.timestamp.
:- import_module make.build.
:- import_module make.dependencies.
:- import_module make.module_dep_file.
:- import_module make.module_target.
:- import_module make.options_file.
:- import_module make.util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.module_deps_graph.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.write_error_spec.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module digraph.
:- import_module dir.
:- import_module getopt.
:- import_module int.
:- import_module io.file.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

make_linked_target(Globals, LinkedTargetFile, LinkedTargetSucceeded,
        !Info, !Specs, !IO) :-
    LinkedTargetFile = linked_target_file(_MainModuleName, FileType),
    (
        FileType = shared_library,
        ExtraOptions = ["--compile-to-shared-lib"]
    ;
        ( FileType = executable
        ; FileType = static_library
        ; FileType = csharp_executable
        ; FileType = csharp_library
        ; FileType = java_executable
        ; FileType = java_archive
        ),
        ExtraOptions = []
    ),
    globals.lookup_accumulating_option(Globals, lib_linkages, LibLinkages),
    ( if
        (
            FileType = static_library,
            not list.member("static", LibLinkages)
        ;
            FileType = shared_library,
            not list.member("shared", LibLinkages)
        )
    then
        LinkedTargetSucceeded = succeeded
    else
        maybe_check_libraries_are_installed(Globals, LibgradeCheckSpecs, !IO),
        (
            LibgradeCheckSpecs = [],
            maybe_with_analysis_cache_dir_3(Globals,
                make_linked_target_1(Globals, LinkedTargetFile, ExtraOptions),
                LinkedTargetSucceeded, !Info, !Specs, !IO)
        ;
            LibgradeCheckSpecs = [_ | _],
            !:Specs = LibgradeCheckSpecs ++ !.Specs,
            LinkedTargetSucceeded = did_not_succeed
        )
    ).

:- pred make_linked_target_1(globals::in, linked_target_file::in,
    list(string)::in, maybe_succeeded::out, make_info::in, make_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

make_linked_target_1(Globals, LinkedTargetFile, ExtraOptions, Succeeded,
        !Info, !Specs, !IO) :-
    LinkedTargetFile = linked_target_file(MainModuleName, _FileType),

    % When using `--intermodule-analysis', perform an analysis pass first.
    % The analysis of one module may invalidate the results of a module
    % we analysed earlier, so this step must be carried out until all the
    % `.analysis' files are in a valid state before we can continue.
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    (
        IntermodAnalysis = yes,
        make_misc_target_builder(Globals, MainModuleName,
            misc_target_build_analyses, IntermodAnalysisSucceeded,
            !Info, !Specs, !IO)
    ;
        IntermodAnalysis = no,
        IntermodAnalysisSucceeded = succeeded
    ),
    (
        IntermodAnalysisSucceeded = succeeded,
        DetectedGradeFlags = make_info_get_detected_grade_flags(!.Info),
        OptionVariables = make_info_get_options_variables(!.Info),
        OptionArgs = make_info_get_option_args(!.Info),
        setup_for_build_with_module_options(invoked_by_mmc_make,
            MainModuleName, DetectedGradeFlags, OptionVariables, OptionArgs,
            ExtraOptions, MayBuild, !IO),
        (
            MayBuild = may_build(_AllOptionArgs, BuildGlobals),
            make_linked_target_2(BuildGlobals, LinkedTargetFile,
                Succeeded, !Info, !IO)
        ;
            MayBuild = may_not_build(Specs),
            get_error_output_stream(Globals, MainModuleName, ErrorStream, !IO),
            write_error_specs(ErrorStream, Globals, Specs, !IO),
            Succeeded = did_not_succeed
        )
    ;
        IntermodAnalysisSucceeded = did_not_succeed,
        Succeeded = did_not_succeed
    ).

:- pred make_linked_target_2(globals::in, linked_target_file::in,
    maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_linked_target_2(Globals, LinkedTargetFile, Succeeded, !Info, !IO) :-
    LinkedTargetFile = linked_target_file(MainModuleName, FileType),
    find_reachable_local_modules(Globals, MainModuleName, DepsSucceeded,
        AllModules, !Info, !IO),
    KeepGoing = make_info_get_keep_going(!.Info),
    ( if
        DepsSucceeded = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed
    else
        get_object_code_type(Globals, FileType, PIC),

        % Build the `.c' files first so that errors are reported
        % as soon as possible.
        globals.get_target(Globals, CompilationTarget),
        (
            CompilationTarget = target_c,
            IntermediateTargetType = module_target_c_code,
            ObjectTargetType = module_target_object_code(PIC)
        ;
            CompilationTarget = target_csharp,
            IntermediateTargetType = module_target_csharp_code,
            ObjectTargetType = module_target_csharp_code
        ;
            CompilationTarget = target_java,
            IntermediateTargetType = module_target_java_code,
            ObjectTargetType = module_target_java_class_code
        ),

        AllModulesList = set.to_sorted_list(AllModules),
        get_target_modules(Globals, IntermediateTargetType, AllModulesList,
            ObjModulesAlpha, !Info, !IO),
        order_target_modules(Globals, ObjModulesAlpha, ObjModules, !Info, !IO),
        remove_nested_modules(Globals, ObjModules, ObjModulesNonnested,
            !Info, !IO),
        IntermediateTargetsNonnested =
            make_dependency_list(ObjModulesNonnested, IntermediateTargetType),
        ObjTargets = make_dependency_list(ObjModules, ObjectTargetType),

        list.map_foldl2(get_foreign_object_targets(Globals, PIC),
            ObjModules, ForeignObjTargetsList, !Info, !IO),
        ForeignObjTargets = list.condense(ForeignObjTargetsList),

        % Ensure all interface files are present before continuing.
        % This prevents a problem when two parallel branches try to generate
        % the same missing interface file later.

        make_all_interface_files(Globals, AllModulesList, IntsSucceeded,
            !Info, !IO),
        ( if
            IntsSucceeded = did_not_succeed,
            KeepGoing = do_not_keep_going
        then
            BuildDepsSucceeded = did_not_succeed
        else
            foldl2_make_module_targets_maybe_parallel(KeepGoing,
                [], Globals, IntermediateTargetsNonnested,
                BuildDepsSucceeded0, !Info, !IO),
            (
                BuildDepsSucceeded0 = succeeded,
                ( if ObjectTargetType = module_target_java_class_code then
                    make_java_files(Globals, MainModuleName, ObjModules,
                        BuildJavaSucceeded, !Info, !IO),
                    (
                        BuildJavaSucceeded = succeeded,
                        % Disable the `--rebuild' option during this pass,
                        % otherwise all the Java classes will be built again.
                        globals.set_option(rebuild, bool(no),
                            Globals, NoRebuildGlobals),
                        foldl2_make_module_targets_maybe_parallel(KeepGoing,
                            [], NoRebuildGlobals, ObjTargets,
                            BuildDepsSucceeded1, !Info, !IO)
                    ;
                        BuildJavaSucceeded = did_not_succeed,
                        BuildDepsSucceeded1 = did_not_succeed
                    )
                else
                    foldl2_make_module_targets_maybe_parallel(KeepGoing,
                        [], Globals, ObjTargets,
                        BuildDepsSucceeded1, !Info, !IO)
                )
            ;
                BuildDepsSucceeded0 = did_not_succeed,
                BuildDepsSucceeded1 = did_not_succeed
            ),
            (
                BuildDepsSucceeded1 = succeeded,
                foldl2_make_module_targets(KeepGoing, [],
                    Globals, ForeignObjTargets, BuildDepsSucceeded,
                    !Info, !IO)
            ;
                BuildDepsSucceeded1 = did_not_succeed,
                BuildDepsSucceeded = did_not_succeed
            )
        ),

        linked_target_file_name(Globals, MainModuleName, FileType,
            OutputFileName, !IO),
        get_file_timestamp([dir.this_directory], OutputFileName,
            MaybeTimestamp, !Info, !IO),
        check_dependencies(Globals, OutputFileName, MaybeTimestamp,
            BuildDepsSucceeded, ObjTargets, BuildDepsResult, !Info, !IO),
        ( if
            DepsSucceeded = succeeded,
            BuildDepsResult \= deps_error
        then
            globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
            setup_checking_for_interrupt(Cookie, !IO),
            prepare_to_redirect_output(MainModuleName, RedirectResult,
                !Info, !IO),
            (
                RedirectResult = no,
                Succeeded0 = did_not_succeed
            ;
                RedirectResult = yes(ErrorStream),
                build_linked_target(MainModuleName, FileType,
                    OutputFileName, MaybeTimestamp, AllModules, ObjModules,
                    CompilationTarget, PIC, DepsSucceeded, BuildDepsResult,
                    Globals, ErrorStream, Succeeded0, !Info, !IO),
                unredirect_output(Globals, MainModuleName, ErrorStream,
                    !Info, !IO)
            ),
            Cleanup = linked_target_cleanup(Globals, MainModuleName,
                FileType, OutputFileName),
            teardown_checking_for_interrupt(VeryVerbose, Cookie, Cleanup,
                Succeeded0, Succeeded, !Info, !IO)
        else
            Succeeded = did_not_succeed
        )
    ).

:- pred get_target_modules(globals::in, module_target_type::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_modules(Globals, TargetType, AllModules, TargetModules,
        !Info, !IO) :-
    ( if TargetType = module_target_errors then
        % `.err' files are only produced for the top-level module
        % in each source file.
        list.foldl3(get_target_modules_2(Globals), AllModules,
            [], TargetModules, !Info, !IO)
    else
        TargetModules = AllModules
    ).

:- pred get_target_modules_2(globals::in, module_name::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_modules_2(Globals, ModuleName, !TargetModules, !Info, !IO) :-
    get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
        !Info, !IO),
    ( if
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_source_file_module_name(ModuleDepInfo,
            SourceFileModuleName),
        ModuleName = SourceFileModuleName
    then
        !:TargetModules = [ModuleName | !.TargetModules]
    else
        true
    ).

:- pred order_target_modules(globals::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

order_target_modules(Globals, Modules, OrderedModules, !Info, !IO) :-
    globals.lookup_bool_option(Globals, order_make_by_timestamp,
        OrderByTimestamp),
    (
        OrderByTimestamp = yes,
        list.map_foldl2(pair_module_with_timestamp(Globals),
            Modules, PairedModules, !Info, !IO),
        list.sort(compare_paired_modules, PairedModules, OrderedPairs),
        list.map(pair.snd, OrderedPairs, OrderedModules)
    ;
        OrderByTimestamp = no,
        OrderedModules = Modules
    ).

:- pred pair_module_with_timestamp(globals::in,
    module_name::in, pair(timestamp, module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

pair_module_with_timestamp(Globals, Module, Timestamp - Module, !Info, !IO) :-
    Search = do_not_search,
    Target = target_file(Module, module_target_source),
    get_target_timestamp(Globals, Search, Target, MaybeTimestamp, !Info, !IO),
    (
        MaybeTimestamp = ok(Timestamp)
    ;
        MaybeTimestamp = error(_),
        Timestamp = oldest_timestamp
    ).

:- pred compare_paired_modules(pair(timestamp, module_name)::in,
    pair(timestamp, module_name)::in, comparison_result::out) is det.

compare_paired_modules(TimeA - ModuleA, TimeB - ModuleB, Res) :-
    compare(TimeRes, TimeA, TimeB),
    % More recently touched files should appear earlier in the list.
    (
        TimeRes = (<),
        Res = (>)
    ;
        TimeRes = (>),
        Res = (<)
    ;
        TimeRes = (=),
        compare(Res, ModuleA, ModuleB)
    ).

:- pred get_foreign_object_targets(globals::in, pic::in, module_name::in,
    list(dependency_file)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

get_foreign_object_targets(Globals, PIC, ModuleName, ObjectTargets,
        !Info, !IO) :-
    % Find externally compiled foreign code files for
    % `:- pragma foreign_proc' declarations.
    %
    % Any changed here may require corresponding changes in
    % external_foreign_code_files.

    globals.get_target(Globals, CompilationTarget),
    get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
        !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        unexpected($pred, "unknown imports")
    ),

    % None of the current backends require externally compiled foreign
    % code, except the C backend for fact tables.
    (
        CompilationTarget = target_c,
        FactFileToTarget =
            ( func(FactFile) =
                dep_target(target_file(ModuleName,
                    module_target_fact_table_object(PIC, FactFile)))
            ),
        module_dep_info_get_fact_tables(ModuleDepInfo, FactTableFiles),
        FactObjectTargets =
            list.map(FactFileToTarget, set.to_sorted_list(FactTableFiles)),
        ObjectTargets = FactObjectTargets
    ;
        ( CompilationTarget = target_java
        ; CompilationTarget = target_csharp
        ),
        ObjectTargets = []
    ).

:- pred build_linked_target(module_name::in, linked_target_type::in,
    file_name::in, maybe_error(timestamp)::in, set(module_name)::in,
    list(module_name)::in, compilation_target::in, pic::in,
    maybe_succeeded::in, dependencies_result::in,
    globals::in, io.text_output_stream::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_linked_target(MainModuleName, FileType, OutputFileName, MaybeTimestamp,
        AllModules, ObjModules, CompilationTarget, PIC, DepsSucceeded,
        BuildDepsResult, Globals, ErrorStream, Succeeded, !Info, !IO) :-
    globals.lookup_maybe_string_option(Globals, pre_link_command,
        MaybePreLinkCommand),
    (
        MaybePreLinkCommand = yes(PreLinkCommand),
        make_all_module_command(PreLinkCommand, MainModuleName,
            set.to_sorted_list(AllModules), CommandString, !IO),
        % XXX STREAM This preserves old behavior, but our caller
        % should pass to us a progress stream *explicitly*.
        io.output_stream(OutputStream, !IO),
        ProgressStream = OutputStream,
        invoke_system_command(Globals, ProgressStream, ErrorStream,
            OutputStream, cmd_verbose, CommandString, PreLinkSucceeded, !IO)
    ;
        MaybePreLinkCommand = no,
        PreLinkSucceeded = succeeded
    ),
    (
        PreLinkSucceeded = succeeded,
        build_linked_target_2(Globals, MainModuleName, FileType,
            OutputFileName, MaybeTimestamp, AllModules, ObjModules,
            CompilationTarget, PIC, DepsSucceeded, BuildDepsResult,
            ErrorStream, Succeeded, !Info, !IO)
    ;
        PreLinkSucceeded = did_not_succeed,
        Succeeded = did_not_succeed
    ).

:- pred build_linked_target_2(globals::in, module_name::in,
    linked_target_type::in, file_name::in, maybe_error(timestamp)::in,
    set(module_name)::in, list(module_name)::in, compilation_target::in,
    pic::in, maybe_succeeded::in, dependencies_result::in,
    io.text_output_stream::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_linked_target_2(Globals, MainModuleName, FileType, OutputFileName,
        MaybeTimestamp, AllModules, ObjModules, CompilationTarget, PIC,
        DepsSucceeded, BuildDepsResult, ErrorStream, Succeeded, !Info, !IO) :-
    % Clear the option -- we will pass the list of files directly.
    globals.lookup_accumulating_option(Globals, link_objects, LinkObjects),
    globals.set_option(link_objects, accumulating([]),
        Globals, NoLinkObjsGlobals),

    % XXX STREAM This preserves old behavior, but our caller
    % should pass to us a progress stream *explicitly*.
    io.output_stream(OutputStream, !IO),
    ProgressStream = OutputStream,

    % Remake the `_init.o' file.
    % XXX We should probably make a `_init.o' file for shared
    % libraries linked using dlopen().
    AllModulesList = set.to_sorted_list(AllModules),
    (
        FileType = executable,
        make_init_obj_file(NoLinkObjsGlobals, ProgressStream, ErrorStream,
            MainModuleName, AllModulesList, InitObjectResult, !IO),
        MaybeInitObjectResult = yes(InitObjectResult)
    ;
        ( FileType = static_library
        ; FileType = shared_library
        ; FileType = csharp_executable
        ; FileType = csharp_library
        ; FileType = java_executable
        ; FileType = java_archive
        ),
        MaybeInitObjectResult = no
    ),
    (
        MaybeInitObjectResult = yes(InitObjectResult1),
        (
            InitObjectResult1 = yes(InitObject),
            % We may need to update the timestamp of the `_init.o' file.
            FileTimestamps0 = make_info_get_file_timestamps(!.Info),
            map.delete(InitObject, FileTimestamps0, FileTimestamps1),
            make_info_set_file_timestamps(FileTimestamps1, !Info),
            % There is no module_target_type for the `_init.o' file,
            % so mki_target_file_timestamps should not contain anything
            % that needs to be invalidated.
            InitObjects = [InitObject],
            DepsResult2 = BuildDepsResult
        ;
            InitObjectResult1 = no,
            DepsResult2 = deps_error,
            InitObjects = []
        )
    ;
        MaybeInitObjectResult = no,
        DepsResult2 = BuildDepsResult,
        InitObjects = []
    ),

    ObjectsToCheck = InitObjects ++ LinkObjects,

    % Report errors if any of the extra objects aren't present.
    list.map_foldl2(dependency_status(NoLinkObjsGlobals),
        list.map((func(F) = dep_file(F)), ObjectsToCheck), ExtraObjStatus,
        !Info, !IO),
    ( if list.member(deps_status_error, ExtraObjStatus) then
        DepsResult3 = deps_error
    else
        DepsResult3 = DepsResult2
    ),
    BuildDepsSucceeded =
        ( if DepsResult3 = deps_error then did_not_succeed else succeeded ),
    list.map_foldl2(get_file_timestamp([dir.this_directory]),
        ObjectsToCheck, ExtraObjectTimestamps, !Info, !IO),
    DepFileToStr =
        ( pred(FN::in, FN::out, IO::di, IO::uo) is det :-
            true
        ),
    check_dependency_timestamps(NoLinkObjsGlobals, OutputFileName,
        MaybeTimestamp, BuildDepsSucceeded, ObjectsToCheck, DepFileToStr,
        ExtraObjectTimestamps, ExtraObjectDepsResult, !IO),

    (
        DepsSucceeded = succeeded,
        DepsResult4 = DepsResult3
    ;
        DepsSucceeded = did_not_succeed,
        DepsResult4 = deps_error
    ),
    ( DepsResult4 = deps_error, DepsResult = DepsResult4
    ; DepsResult4 = deps_out_of_date, DepsResult = DepsResult4
    ; DepsResult4 = deps_up_to_date, DepsResult = ExtraObjectDepsResult
    ),
    (
        DepsResult = deps_error,
        file_error_msg(OutputFileName, ErrorMsg),
        % XXX MAKE_STREAM
        maybe_write_msg_locked(!.Info, ErrorMsg, !IO),
        Succeeded = did_not_succeed
    ;
        DepsResult = deps_up_to_date,
        MainModuleLinkedTarget =
            top_target_file(MainModuleName, linked_target(FileType)),
        linked_target_file_name(Globals, MainModuleName, FileType,
            MainModuleLinkedFileName, !IO),
        globals.lookup_bool_option(NoLinkObjsGlobals, use_grade_subdirs,
            UseGradeSubdirs),
        (
            UseGradeSubdirs = yes,
            post_link_make_symlink_or_copy(NoLinkObjsGlobals,
                ProgressStream, ErrorStream, FileType, MainModuleName,
                Succeeded, MadeSymlinkOrCopy, !IO),
            (
                MadeSymlinkOrCopy = yes,
                maybe_symlink_or_copy_linked_target_msg(NoLinkObjsGlobals,
                    MainModuleLinkedFileName, LinkMsg),
                % XXX MAKE_STREAM
                maybe_write_msg(LinkMsg, !IO)
            ;
                MadeSymlinkOrCopy = no,
                maybe_warn_up_to_date_target_msg(NoLinkObjsGlobals,
                    MainModuleLinkedTarget, MainModuleLinkedFileName,
                    !Info, UpToDateMsg),
                % XXX MAKE_STREAM
                maybe_write_msg(UpToDateMsg, !IO)
            )
        ;
            UseGradeSubdirs = no,
            maybe_warn_up_to_date_target_msg(NoLinkObjsGlobals,
                MainModuleLinkedTarget, MainModuleLinkedFileName, !Info,
                UpToDateMsg),
            % XXX MAKE_STREAM
            maybe_write_msg(UpToDateMsg, !IO),
            Succeeded = succeeded
        )
    ;
        DepsResult = deps_out_of_date,
        maybe_making_filename_msg(NoLinkObjsGlobals, OutputFileName,
            MakingMsg),
        % XXX MAKE_STREAM
        maybe_write_msg(MakingMsg, !IO),

        % Find the extra object files for externally compiled foreign
        % procedures and fact tables. We don't need to include these in the
        % timestamp checking above -- they will have been checked when the
        % module's object file was built.
        list.map_foldl2(get_module_foreign_object_files(Globals, PIC),
            AllModulesList, ForeignObjectFileLists, !Info, !IO),
        ForeignObjects = list.condense(ForeignObjectFileLists),

        (
            CompilationTarget = target_c,
            maybe_pic_object_file_extension(NoLinkObjsGlobals, PIC,
                ObjOtherExtToUse, ObjNewExt, _),
            NewExt = newext_target_obj(ObjNewExt)
        ;
            CompilationTarget = target_csharp,
            % There is no separate object code step.
            ObjOtherExtToUse = other_ext(".cs"),
            NewExt = newext_target_c_cs(ext_target_cs)
        ;
            CompilationTarget = target_java,
            ObjOtherExtToUse = other_ext(".class"),
            NewExt = newext_target_java(ext_target_java_class)
        ),
        list.map_foldl(
            module_name_to_file_name(NoLinkObjsGlobals, $pred,
                do_not_create_dirs, ext_other(ObjOtherExtToUse), NewExt),
            ObjModules, ObjList, !IO),

        % LinkObjects may contain `.a' files which must come
        % after all the object files on the linker command line.
        AllObjects = InitObjects ++ ObjList ++ ForeignObjects ++ LinkObjects,
        (
            ( CompilationTarget = target_c
            ; CompilationTarget = target_java
            ; CompilationTarget = target_csharp
            ),
            % Run the link in a separate process so it can be killed
            % if an interrupt is received.
            call_in_forked_process(
                compile_target_code.link(NoLinkObjsGlobals,
                    ProgressStream, ErrorStream, FileType,
                    MainModuleName, AllObjects),
                Succeeded, !IO)
        ),
        CmdLineTargets0 = make_info_get_command_line_targets(!.Info),
        set.delete(top_target_file(MainModuleName, linked_target(FileType)),
            CmdLineTargets0, CmdLineTargets),
        make_info_set_command_line_targets(CmdLineTargets, !Info),
        (
            Succeeded = succeeded,
            FileTimestamps2 = make_info_get_file_timestamps(!.Info),
            map.delete(OutputFileName, FileTimestamps2, FileTimestamps),
            make_info_set_file_timestamps(FileTimestamps, !Info)
            % There is no module_target_type for the linked target,
            % so mki_target_file_timestamps should not contain anything
            % that needs to be invalidated.
        ;
            Succeeded = did_not_succeed,
            file_error_msg(OutputFileName, ErrorMsg),
            % XXX MAKE_STREAM
            maybe_write_msg_locked(!.Info, ErrorMsg, !IO)
        )
    ).

:- pred get_module_foreign_object_files(globals::in, pic::in,
    module_name::in, list(file_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_module_foreign_object_files(Globals, PIC, ModuleName, ForeignObjectFiles,
        !MakeInfo, !IO) :-
    get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
        !MakeInfo, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        external_foreign_code_files(Globals, PIC, ModuleDepInfo,
            ForeignFiles, !IO),
        ForeignObjectFiles = list.map(
            (func(foreign_code_file(_, _, ObjFile)) = ObjFile),
            ForeignFiles)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        % This error should have been detected earlier.
        unexpected($pred, "error in dependencies")
    ).

:- pred linked_target_cleanup(globals::in, module_name::in,
    linked_target_type::in, file_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

linked_target_cleanup(Globals, MainModuleName, FileType, OutputFileName,
        !Info, !IO) :-
    make_remove_file(Globals, verbose_make, OutputFileName, !Info, !IO),
    (
        FileType = executable,
        remove_init_files(Globals, verbose_make, MainModuleName, !Info, !IO)
    ;
        ( FileType = static_library
        ; FileType = shared_library
        ; FileType = csharp_executable
        ; FileType = csharp_library
        ; FileType = java_executable
        ; FileType = java_archive
        )
    ).

%---------------------------------------------------------------------------%

    % When compiling to Java we want to invoke `javac' just once, passing it a
    % list of all out-of-date `.java' files. This is a lot quicker than
    % compiling each Java file individually.
    %
:- pred make_java_files(globals::in, module_name::in, list(module_name)::in,
    maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_java_files(Globals, MainModuleName, ObjModules, Succeeded, !Info, !IO) :-
    out_of_date_java_modules(Globals, ObjModules, OutOfDateModules,
        !Info, !IO),
    (
        OutOfDateModules = [],
        Succeeded = succeeded
    ;
        OutOfDateModules = [_ | _],
        build_java_files(Globals, MainModuleName, OutOfDateModules, Succeeded,
            !Info, !IO),
        % javac might write more `.class' files than we anticipated (though
        % it probably won't) so clear out all the timestamps which might be
        % affected.
        Timestamps0 = make_info_get_file_timestamps(!.Info),
        map.foldl(delete_java_class_timestamps, Timestamps0,
            map.init, Timestamps),
        make_info_set_file_timestamps(Timestamps, !Info),
        % For simplicity, clear out all target file timestamps.
        make_info_set_target_file_timestamps(init_target_file_timestamps,
            !Info)
    ).

:- pred out_of_date_java_modules(globals::in, list(module_name)::in,
    list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

out_of_date_java_modules(Globals, ObjModules, OutOfDateModules, !Info, !IO) :-
    (
        ObjModules = [],
        OutOfDateModules = []
    ;
        ObjModules = [ModuleName | ModuleNames],
        out_of_date_java_modules(Globals, ModuleNames, OutOfDateModules0,
            !Info, !IO),
        JavaTarget = target_file(ModuleName, module_target_java_code),
        ClassTarget = target_file(ModuleName, module_target_java_class_code),
        get_target_timestamp(Globals, do_not_search, JavaTarget,
            MaybeJavaTimestamp, !Info, !IO),
        get_target_timestamp(Globals, do_not_search, ClassTarget,
            MaybeClassTimestamp, !Info, !IO),
        ( if
            MaybeJavaTimestamp = ok(JavaTimestamp),
            MaybeClassTimestamp = ok(ClassTimestamp),
            ClassTimestamp @>= JavaTimestamp
        then
            OutOfDateModules = OutOfDateModules0
        else
            OutOfDateModules = [ModuleName | OutOfDateModules0]
        )
    ).

:- pred build_java_files(globals::in, module_name::in, list(module_name)::in,
    maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_java_files(Globals, MainModuleName, ModuleNames, Succeeded,
        !Info, !IO) :-
    verbose_make_msg(Globals,
        io.write_string("Making Java class files\n"), !IO),
    list.map_foldl(
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".java")),
            newext_target_java(ext_target_java_java)),
        ModuleNames, JavaFiles, !IO),
    % We redirect errors to a file named after the main module.
    prepare_to_redirect_output(MainModuleName, RedirectResult, !Info, !IO),
    (
        RedirectResult = no,
        Succeeded = did_not_succeed
    ;
        RedirectResult = yes(ErrorStream),
        build_java_files_2(JavaFiles, Globals, ErrorStream, Succeeded,
            !Info, !IO),
        unredirect_output(Globals, MainModuleName, ErrorStream, !Info, !IO)
    ).

:- pred build_java_files_2(list(string)::in, globals::in,
    io.text_output_stream::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_java_files_2(JavaFiles, Globals, ErrorStream, Succeeded, !Info, !IO) :-
    list.det_head_tail(JavaFiles, HeadJavaFile, TailJavaFiles),
    % XXX STREAM This preserves old behavior, but our caller
    % should pass to us a progress stream *explicitly*.
    io.output_stream(OutputStream, !IO),
    ProgressStream = OutputStream,
    call_in_forked_process(
        compile_java_files(Globals, ProgressStream, ErrorStream,
            HeadJavaFile, TailJavaFiles),
        Succeeded, !IO).

:- pred delete_java_class_timestamps(string::in, maybe_error(timestamp)::in,
    file_timestamps::in, file_timestamps::out) is det.

delete_java_class_timestamps(FileName, MaybeTimestamp, !Timestamps) :-
    ( if string.suffix(FileName, ".class") then
        true
    else
        map.det_insert(FileName, MaybeTimestamp, !Timestamps)
    ).

%---------------------------------------------------------------------------%

make_misc_target(Globals, MainModuleName - TargetType, Succeeded,
        !Info, !Specs, !IO) :-
    DetectedGradeFlags = make_info_get_detected_grade_flags(!.Info),
    OptionVariables = make_info_get_options_variables(!.Info),
    OptionArgs = make_info_get_option_args(!.Info),
    ExtraOptions = [],
    setup_for_build_with_module_options(invoked_by_mmc_make, MainModuleName,
        DetectedGradeFlags, OptionVariables, OptionArgs, ExtraOptions,
        MayBuild, !IO),
    (
        MayBuild = may_build(_AllOptionArgs, BuildGlobals),
        make_misc_target_builder(BuildGlobals, MainModuleName,
            TargetType, Succeeded, !Info, !Specs, !IO)
    ;
        MayBuild = may_not_build(Specs),
        get_error_output_stream(Globals, MainModuleName, ErrorStream, !IO),
        write_error_specs(ErrorStream, Globals, Specs, !IO),
        Succeeded = did_not_succeed
    ).

:- pred make_misc_target_builder(globals::in, module_name::in,
    misc_target_type::in, maybe_succeeded::out, make_info::in, make_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

make_misc_target_builder(Globals, MainModuleName, TargetType, Succeeded,
        !Info, !Specs, !IO) :-
    % Don't rebuild .module_dep files when cleaning up.
    RebuildModuleDeps = make_info_get_rebuild_module_deps(!.Info),
    ( if
        ( TargetType = misc_target_clean
        ; TargetType = misc_target_realclean
        )
    then
        make_info_set_rebuild_module_deps(do_not_rebuild_module_deps, !Info)
    else
        true
    ),
    find_reachable_local_modules(Globals, MainModuleName, Succeeded0,
        AllModulesSet, !Info, !IO),
    make_info_set_rebuild_module_deps(RebuildModuleDeps, !Info),
    AllModules = set.to_sorted_list(AllModulesSet),
    (
        TargetType = misc_target_clean,
        Succeeded = succeeded,
        list.foldl2(make_module_clean(Globals), AllModules, !Info, !IO),
        remove_init_files(Globals, very_verbose, MainModuleName, !Info, !IO)
    ;
        TargetType = misc_target_realclean,
        Succeeded = succeeded,
        make_main_module_realclean(Globals, MainModuleName, !Info, !IO),
        list.foldl2(make_module_realclean(Globals), AllModules, !Info, !IO)
    ;
        TargetType = misc_target_build_all(ModuleTargetType),
        get_target_modules(Globals, ModuleTargetType, AllModules,
            TargetModules, !Info, !IO),
        KeepGoing = make_info_get_keep_going(!.Info),
        ( if Succeeded0 = did_not_succeed, KeepGoing = do_not_keep_going then
            Succeeded = did_not_succeed
        else
            % Ensure all interface files are present before continuing.
            % This prevents a problem when two parallel branches
            % try to generate the same missing interface file later.
            make_all_interface_files(Globals, AllModules, Succeeded1,
                !Info, !IO),
            ( if
                Succeeded1 = did_not_succeed,
                KeepGoing = do_not_keep_going
            then
                Succeeded = did_not_succeed
            else
                maybe_with_analysis_cache_dir_2(Globals,
                    foldl2_make_module_targets_maybe_parallel(KeepGoing,
                        [], Globals,
                        make_dependency_list(TargetModules, ModuleTargetType)),
                    Succeeded2, !Info, !IO),
                Succeeded = Succeeded0 `and` Succeeded1 `and` Succeeded2
            )
        )
    ;
        TargetType = misc_target_build_analyses,
        maybe_with_analysis_cache_dir_2(Globals,
            build_analysis_files(Globals, MainModuleName, AllModules,
                Succeeded0),
            Succeeded, !Info, !IO)
    ;
        TargetType = misc_target_build_library,
        make_all_interface_files(Globals, AllModules, IntSucceeded,
            !Info, !IO),
        (
            IntSucceeded = succeeded,
            maybe_with_analysis_cache_dir_3(Globals,
                build_library(MainModuleName, AllModules, Globals),
                Succeeded, !Info, !Specs, !IO)
        ;
            IntSucceeded = did_not_succeed,
            Succeeded = did_not_succeed
        )
    ;
        TargetType = misc_target_install_library,
        make_misc_target(Globals, MainModuleName - misc_target_build_library,
            LibSucceeded, !Info, !Specs, !IO),
        (
            LibSucceeded = succeeded,
            install_library(Globals, MainModuleName, Succeeded, !Info, !IO)
        ;
            LibSucceeded = did_not_succeed,
            Succeeded = did_not_succeed
        )
    ;
        TargetType = misc_target_build_xml_docs,
        get_target_modules(Globals, module_target_xml_doc, AllModules,
            TargetModules, !Info, !IO),
        KeepGoing = make_info_get_keep_going(!.Info),
        ( if Succeeded0 = did_not_succeed, KeepGoing = do_not_keep_going then
            Succeeded = did_not_succeed
        else
            foldl2_make_module_targets(KeepGoing, [],
                Globals,
                make_dependency_list(TargetModules, module_target_xml_doc),
                Succeeded1, !Info, !IO),
            Succeeded = Succeeded0 `and` Succeeded1
        )
    ).

%---------------------------------------------------------------------------%

:- pred make_all_interface_files(globals::in, list(module_name)::in,
    maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_all_interface_files(Globals, AllModules0, Succeeded, !Info, !IO) :-
    remove_nested_modules(Globals, AllModules0, NonnestedModules, !Info, !IO),
    list.foldl3(collect_modules_with_children(Globals), NonnestedModules,
        [], ParentModules, !Info, !IO),
    Int3s = make_dependency_list(NonnestedModules, module_target_int3),
    Int0s = make_dependency_list(ParentModules, module_target_int0),
    Int1s = make_dependency_list(NonnestedModules, module_target_int1),
    globals.get_any_intermod(Globals, AnyIntermod),
    (
        AnyIntermod = yes,
        Opts = make_dependency_list(NonnestedModules, module_target_opt)
    ;
        AnyIntermod = no,
        Opts = []
    ),
    KeepGoing = make_info_get_keep_going(!.Info),
    % Private interfaces (.int0) need to be made before building long interface
    % files in parallel, otherwise two processes may try to build the same
    % private interface file.
    foldl2_make_module_targets_maybe_parallel(KeepGoing, [], Globals, Int3s,
        Succeeded0, !Info, !IO),
    (
        Succeeded0 = succeeded,
        foldl2_make_module_targets(KeepGoing, [],
            Globals, Int0s, Succeeded1, !Info, !IO),
        (
            Succeeded1 = succeeded,
            foldl2_make_module_targets_maybe_parallel(KeepGoing, [],
                Globals, Int1s, Succeeded2, !Info, !IO),
            (
                Succeeded2 = succeeded,
                foldl2_make_module_targets_maybe_parallel(KeepGoing, [],
                    Globals, Opts, Succeeded, !Info, !IO)
            ;
                Succeeded2 = did_not_succeed,
                Succeeded = did_not_succeed
            )
        ;
            Succeeded1 = did_not_succeed,
            Succeeded = did_not_succeed
        )
    ;
        Succeeded0 = did_not_succeed,
        Succeeded = did_not_succeed
    ).

:- pred collect_modules_with_children(globals::in, module_name::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

collect_modules_with_children(Globals, ModuleName, !ParentModules,
        !Info, !IO) :-
    get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
        !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_children(ModuleDepInfo, Children),
        ( if set.is_empty(Children) then
            true
        else
            !:ParentModules = [ModuleName | !.ParentModules]
        )
    ;
        MaybeModuleDepInfo = no_module_dep_info
    ).

%---------------------------------------------------------------------------%

:- type build2(Info) == pred(maybe_succeeded, Info, Info, io, io).
:- inst build2 == (pred(out, in, out, di, uo) is det).

    % If `--analysis-file-cache' is enabled, create a temporary directory for
    % holding analysis cache files and pass that to child processes.
    % After P is finished, remove the cache directory completely.
    %
:- pred maybe_with_analysis_cache_dir_2(globals::in,
    build2(make_info)::in(build2), maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

maybe_with_analysis_cache_dir_2(Globals, P, Succeeded, !Info, !IO) :-
    should_we_use_analysis_cache_dir(Globals, !.Info,
        UseAnalysisCacheDir, !IO),
    (
        UseAnalysisCacheDir = do_not_use_analysis_cache_dir,
        P(Succeeded, !Info, !IO)
    ;
        UseAnalysisCacheDir = use_analysis_cache_dir(CacheDir, CacheDirOption),
        OrigOptionArgs = make_info_get_option_args(!.Info),
        % Pass the name of the cache directory to child processes
        make_info_set_option_args(OrigOptionArgs ++ [CacheDirOption, CacheDir],
            !Info),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        setup_checking_for_interrupt(Cookie, !IO),
        P(Succeeded1, !Info, !IO),
        Cleanup = remove_cache_dir(Globals, CacheDir),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, Cleanup,
            Succeeded1, Succeeded, !Info, !IO),
        remove_cache_dir(Globals, CacheDir, !Info, !IO),
        make_info_set_option_args(OrigOptionArgs, !Info)
    ;
        UseAnalysisCacheDir = analysis_cache_dir_create_failed,
        Succeeded = did_not_succeed
    ).

%---------------------%

:- type build3(Info) == pred(maybe_succeeded, Info, Info,
    list(error_spec), list(error_spec), io, io).
:- inst build3 == (pred(out, in, out, in, out, di, uo) is det).

    % If `--analysis-file-cache' is enabled, create a temporary directory for
    % holding analysis cache files and pass that to child processes.
    % After P is finished, remove the cache directory completely.
    %
:- pred maybe_with_analysis_cache_dir_3(globals::in,
    build3(make_info)::in(build3), maybe_succeeded::out,
    make_info::in, make_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

maybe_with_analysis_cache_dir_3(Globals, P, Succeeded, !Info, !Specs, !IO) :-
    should_we_use_analysis_cache_dir(Globals, !.Info,
        UseAnalysisCacheDir, !IO),
    (
        UseAnalysisCacheDir = do_not_use_analysis_cache_dir,
        P(Succeeded, !Info, !Specs, !IO)
    ;
        UseAnalysisCacheDir = use_analysis_cache_dir(CacheDir, CacheDirOption),
        OrigOptionArgs = make_info_get_option_args(!.Info),
        % Pass the name of the cache directory to child processes
        make_info_set_option_args(OrigOptionArgs ++ [CacheDirOption, CacheDir],
            !Info),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        setup_checking_for_interrupt(Cookie, !IO),
        P(Succeeded1, !Info, !Specs, !IO),
        Cleanup = remove_cache_dir(Globals, CacheDir),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, Cleanup,
            Succeeded1, Succeeded, !Info, !IO),
        remove_cache_dir(Globals, CacheDir, !Info, !IO),
        make_info_set_option_args(OrigOptionArgs, !Info)
    ;
        UseAnalysisCacheDir = analysis_cache_dir_create_failed,
        Succeeded = did_not_succeed
    ).

%---------------------%

:- type maybe_use_analysis_cache_dir
    --->    do_not_use_analysis_cache_dir
    ;       use_analysis_cache_dir(string, string)
    ;       analysis_cache_dir_create_failed.

    % If `--analysis-file-cache' is enabled, create a temporary directory for
    % holding analysis cache files.
    %
:- pred should_we_use_analysis_cache_dir(globals::in, make_info::in,
    maybe_use_analysis_cache_dir::out, io::di, io::uo) is det.

should_we_use_analysis_cache_dir(Globals, Info, UseAnalysisCacheDir, !IO) :-
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    globals.lookup_bool_option(Globals, analysis_file_cache, Caching),
    globals.lookup_string_option(Globals, analysis_file_cache_dir, CacheDir0),
    CacheDirOption = "--analysis-file-cache-dir",
    ( if
        (
            IntermodAnalysis = no
        ;
            Caching = no
        ;
            % Cache directory given on command line.
            CacheDir0 \= ""
        ;
            % Analysis file cache directory already set up in a parent call.
            list.member(CacheDirOption, make_info_get_option_args(Info))
        )
    then
        UseAnalysisCacheDir = do_not_use_analysis_cache_dir
    else
        create_analysis_cache_dir(Globals, Succeeded, CacheDir, !IO),
        (
            Succeeded = succeeded,
            UseAnalysisCacheDir =
                use_analysis_cache_dir(CacheDir, CacheDirOption)
        ;
            Succeeded = did_not_succeed,
            UseAnalysisCacheDir = analysis_cache_dir_create_failed
        )
    ).

%---------------------%

:- pred create_analysis_cache_dir(globals::in, maybe_succeeded::out,
    string::out, io::di, io::uo) is det.

create_analysis_cache_dir(Globals, Succeeded, CacheDir, !IO) :-
    choose_analysis_cache_dir_name(Globals, CacheDir),
    verbose_make_msg_option(Globals, verbose_make,
        io.format("Creating %s\n", [s(CacheDir)]), !IO),
    dir.make_directory(CacheDir, MakeRes, !IO),
    (
        MakeRes = ok,
        Succeeded = succeeded
    ;
        MakeRes = error(Error),
        io.format("Error: making directory %s: %s\n",
            [s(CacheDir), s(io.error_message(Error))], !IO),
        Succeeded = did_not_succeed
    ).

:- pred choose_analysis_cache_dir_name(globals::in, string::out) is det.

choose_analysis_cache_dir_name(Globals, DirName) :-
    % XXX This code should be unnecessary.
    % The code of file_names.m should return filenames as not one string,
    % but as a <directory path, file name> pair. Besides leaving it up
    % to the caller whether they want to create the directory path,
    % the directory path would (or at least, SHOULD) be exactly what
    % this predicate computes.
    globals.lookup_bool_option(Globals, use_grade_subdirs, UseGradeSubdirs),
    globals.lookup_string_option(Globals, target_arch, TargetArch),
    (
        UseGradeSubdirs = yes,
        grade_directory_component(Globals, Grade),
        DirComponents = ["Mercury", Grade, TargetArch, "Mercury",
            "analysis_cache"]
    ;
        UseGradeSubdirs = no,
        DirComponents = ["Mercury", "analysis_cache"]
    ),
    DirName = dir.relative_path_name_from_components(DirComponents).

:- pred remove_cache_dir(globals::in, string::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_cache_dir(Globals, CacheDir, !Info, !IO) :-
    verbose_make_msg_option(Globals, verbose_make,
        io.format("Removing %s\n", [s(CacheDir)]), !IO),
    io.file.remove_file_recursively(CacheDir, _, !IO).

%---------------------------------------------------------------------------%

:- pred build_analysis_files(globals::in, module_name::in,
    list(module_name)::in, maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_analysis_files(Globals, MainModuleName, AllModules,
        Succeeded0, Succeeded, !Info, !IO) :-
    KeepGoing = make_info_get_keep_going(!.Info),
    ( if
        Succeeded0 = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed
    else
        % Ensure all interface files are present before continuing.
        % This prevents a problem when two parallel branches try to generate
        % the same missing interface file later.
        % (Although we can't actually build analysis files in parallel yet.)
        make_all_interface_files(Globals, AllModules, Succeeded1, !Info, !IO),
        ( if
            Succeeded1 = did_not_succeed,
            KeepGoing = do_not_keep_going
        then
            Succeeded = did_not_succeed
        else
            build_analysis_files_1(Globals, MainModuleName, AllModules,
                Succeeded, !Info, !IO)
        )
    ).

:- pred build_analysis_files_1(globals::in, module_name::in,
    list(module_name)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_analysis_files_1(Globals, MainModuleName, AllModules, Succeeded,
        !Info, !IO) :-
    get_target_modules(Globals, module_target_analysis_registry, AllModules,
        TargetModules0, !Info, !IO),
    reverse_ordered_modules(make_info_get_module_dependencies(!.Info),
        TargetModules0, TargetModules1),
    % Filter out the non-local modules so we don't try to reanalyse them.
    list.filter(list.contains(AllModules), TargetModules1, TargetModules),
    make_local_module_id_options(Globals, MainModuleName, Succeeded0,
        LocalModulesOpts, !Info, !IO),
    (
        Succeeded0 = succeeded,
        build_analysis_files_2(Globals, MainModuleName, TargetModules,
            LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
    ;
        Succeeded0 = did_not_succeed,
        Succeeded = did_not_succeed
    ).

:- pred build_analysis_files_2(globals::in, module_name::in,
    list(module_name)::in, list(string)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_analysis_files_2(Globals, MainModuleName, TargetModules,
        LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO) :-
    KeepGoing = make_info_get_keep_going(!.Info),
    foldl2_make_module_targets(KeepGoing, LocalModulesOpts, Globals,
        make_dependency_list(TargetModules, module_target_analysis_registry),
        Succeeded1, !Info, !IO),
    % Maybe we should have an option to reanalyse cliques before moving
    % upwards in the dependency graph?

    % Find which module analysis files are suboptimal or invalid.
    % If there are any invalid files then we repeat the analysis pass.
    % If there are only suboptimal files then we repeat the analysis up
    % to the number of times given by the user.
    ReanalysisPasses = make_info_get_reanalysis_passes(!.Info),
    ReanalyseSuboptimal = (if ReanalysisPasses > 1 then yes else no),
    modules_needing_reanalysis(ReanalyseSuboptimal, Globals, TargetModules,
        InvalidModules, SuboptimalModules, !IO),
    ( if list.is_not_empty(InvalidModules) then
        maybe_reanalyse_modules_msg(Globals, ReanalysingMsg),
        % XXX MAKE_STREAM
        maybe_write_msg(ReanalysingMsg, !IO),
        list.foldl(reset_analysis_registry_dependency_status,
            InvalidModules, !Info),
        build_analysis_files_2(Globals, MainModuleName, TargetModules,
            LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
    else if list.is_not_empty(SuboptimalModules) then
        list.foldl(reset_analysis_registry_dependency_status,
            SuboptimalModules, !Info),
        make_info_set_reanalysis_passes(ReanalysisPasses - 1, !Info),
        maybe_reanalyse_modules_msg(Globals, ReanalysingMsg),
        % XXX MAKE_STREAM
        maybe_write_msg(ReanalysingMsg, !IO),
        build_analysis_files_2(Globals, MainModuleName, TargetModules,
            LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
    else
        Succeeded = Succeeded0 `and` Succeeded1
    ).

%---------------------------------------------------------------------------%

    % Return a list of modules in reverse order of their dependencies, i.e.
    % the list is the module dependency graph from bottom-up. Mutually
    % dependent modules (modules which form a clique in the dependency graph)
    % are returned adjacent in the list in arbitrary order.
    %
:- pred reverse_ordered_modules(map(module_name, maybe_module_dep_info)::in,
    list(module_name)::in, list(module_name)::out) is det.

reverse_ordered_modules(ModuleDeps, Modules0, Modules) :-
    list.foldl2(
        add_module_relations(lookup_module_dep_info_in_maybe_map(ModuleDeps)),
        Modules0, digraph.init, _IntDepsGraph, digraph.init, ImpDepsGraph),
    SccSets = digraph.return_sccs_in_to_from_order(ImpDepsGraph),
    list.map(set.to_sorted_list, SccSets, SccLists),
    list.condense(SccLists, Modules).

    % add_module_relations(LookupModuleImports, ModuleName,
    %   !IntDepsRel, !ImplDepsRel)
    %
    % Add a module's interface and implementation dependencies to IntDepsRel
    % and ImplDepsRel respectively. Dependencies are found using the
    % LookupModuleImports function.
    %
:- pred add_module_relations(lookup_module_dep_info::in, module_name::in,
    digraph(module_name)::in, digraph(module_name)::out,
    digraph(module_name)::in, digraph(module_name)::out) is det.

add_module_relations(LookupModuleImports, ModuleName,
        !IntDepsGraph, !ImplDepsGraph) :-
    ModuleDepInfo = LookupModuleImports(ModuleName),
    add_module_dep_info_to_deps_graph(ModuleDepInfo, LookupModuleImports,
        !IntDepsGraph, !ImplDepsGraph).

%---------------------------------------------------------------------------%

:- func lookup_module_dep_info_in_maybe_map(
    map(module_name, maybe_module_dep_info), module_name)
    = module_dep_info.

lookup_module_dep_info_in_maybe_map(ModuleDeps, ModuleName) = ModuleDepInfo :-
    map.lookup(ModuleDeps, ModuleName, MaybeModuleDepInfo),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        unexpected($pred, "MaybeModuleDepInfo = no")
    ).

:- pred modules_needing_reanalysis(bool::in, globals::in,
    list(module_name)::in, list(module_name)::out, list(module_name)::out,
    io::di, io::uo) is det.

modules_needing_reanalysis(_, _, [], [], [], !IO).
modules_needing_reanalysis(ReanalyseSuboptimal, Globals, [Module | Modules],
        InvalidModules, SuboptimalModules, !IO) :-
    do_read_module_overall_status(mmc, Globals, Module, ModuleStatus, !IO),
    (
        ModuleStatus = optimal,
        modules_needing_reanalysis(ReanalyseSuboptimal, Globals, Modules,
            InvalidModules, SuboptimalModules, !IO)
    ;
        ModuleStatus = suboptimal,
        modules_needing_reanalysis(ReanalyseSuboptimal, Globals, Modules,
            InvalidModules, SuboptimalModules0, !IO),
        (
            ReanalyseSuboptimal = yes,
            SuboptimalModules = [Module | SuboptimalModules0]
        ;
            ReanalyseSuboptimal = no,
            SuboptimalModules = SuboptimalModules0
        )
    ;
        ModuleStatus = invalid,
        modules_needing_reanalysis(ReanalyseSuboptimal, Globals, Modules,
            InvalidModules0, SuboptimalModules, !IO),
        InvalidModules = [Module | InvalidModules0]
    ).

:- pred reset_analysis_registry_dependency_status(module_name::in,
    make_info::in, make_info::out) is det.

reset_analysis_registry_dependency_status(ModuleName, !Info) :-
    Dep = dep_target(target_file(ModuleName, module_target_analysis_registry)),
    DepStatusMap0 = make_info_get_dependency_status(!.Info),
    version_hash_table.set(Dep, deps_status_not_considered,
        DepStatusMap0, DepStatusMap),
    make_info_set_dependency_status(DepStatusMap, !Info).

%---------------------------------------------------------------------------%

:- pred build_library(module_name::in, list(module_name)::in, globals::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

build_library(MainModuleName, AllModules, Globals, Succeeded,
        !Info, !Specs, !IO) :-
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        build_c_library(Globals, MainModuleName, AllModules, Succeeded,
            !Info, !Specs, !IO)
    ;
        Target = target_csharp,
        build_csharp_library(Globals, MainModuleName, Succeeded,
            !Info, !Specs, !IO)
    ;
        Target = target_java,
        build_java_library(Globals, MainModuleName, Succeeded,
            !Info, !Specs, !IO)
    ).

:- pred build_c_library(globals::in, module_name::in, list(module_name)::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

build_c_library(Globals, MainModuleName, AllModules, Succeeded,
        !Info, !Specs, !IO) :-
    make_linked_target(Globals,
        linked_target_file(MainModuleName, static_library),
        StaticSucceeded, !Info, !Specs, !IO),
    shared_libraries_supported(Globals, SharedLibsSupported),
    (
        StaticSucceeded = succeeded,
        (
            SharedLibsSupported = yes,
            make_linked_target(Globals,
                linked_target_file(MainModuleName, shared_library),
                SharedLibsSucceeded, !Info, !Specs, !IO)
        ;
            SharedLibsSupported = no,
            SharedLibsSucceeded = succeeded
        ),
        % We can only build the .init file if we have succesfully built
        % the .c files.
        (
            SharedLibsSucceeded = succeeded,
            % Errors while making the .init file should be very rare.
            % XXX STREAM This preserves old behavior, but our caller
            % should pass to us a progress stream *explicitly*.
            io.output_stream(OutputStream, !IO),
            ProgressStream = OutputStream,
            ErrorStream = OutputStream,
            make_library_init_file(Globals, ProgressStream, ErrorStream,
                MainModuleName, AllModules, Succeeded, !IO)
        ;
            SharedLibsSucceeded = did_not_succeed,
            Succeeded = did_not_succeed
        )
    ;
        StaticSucceeded = did_not_succeed,
        Succeeded = did_not_succeed
    ).

:- pred build_csharp_library(globals::in, module_name::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

build_csharp_library(Globals, MainModuleName, Succeeded, !Info, !Specs, !IO) :-
    make_linked_target(Globals,
        linked_target_file(MainModuleName, csharp_library),
        Succeeded, !Info, !Specs, !IO).

:- pred build_java_library(globals::in, module_name::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

build_java_library(Globals, MainModuleName, Succeeded, !Info, !Specs, !IO) :-
    make_linked_target(Globals,
        linked_target_file(MainModuleName, java_archive),
        Succeeded, !Info, !Specs, !IO).

%---------------------------------------------------------------------------%

:- pred install_library(globals::in, module_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_library(Globals, MainModuleName, Succeeded, !Info, !IO) :-
    find_reachable_local_modules(Globals, MainModuleName, DepsSucceeded,
        AllModules0, !Info, !IO),
    AllModules = set.to_sorted_list(AllModules0),
    make_install_dirs(Globals, DirSucceeded, LinkSucceeded, !IO),
    ( if
        DepsSucceeded = succeeded,
        DirSucceeded = succeeded
    then
        list.map_foldl2(install_ints_and_headers(Globals, LinkSucceeded),
            AllModules, IntsSucceeded, !Info, !IO),
        install_extra_headers(Globals, ExtraHdrsSucceeded, !IO),

        grade_directory_component(Globals, Grade),
        install_library_grade_files(Globals, LinkSucceeded, Grade,
            MainModuleName, AllModules, GradeSucceeded, !Info, !IO),
        ( if
            and_list([ExtraHdrsSucceeded | IntsSucceeded]) = succeeded,
            GradeSucceeded = succeeded
        then
            KeepGoing = make_info_get_keep_going(!.Info),
            % XXX With Mmake, LIBGRADES is target-specific.
            globals.lookup_accumulating_option(Globals, libgrades, LibGrades0),
            LibGrades = list.delete_all(LibGrades0, Grade),
            foldl2_install_library_grades(KeepGoing,
                LinkSucceeded, MainModuleName, AllModules,
                Globals, LibGrades, Succeeded, !Info, !IO)
        else
            Succeeded = did_not_succeed
        )
    else
        Succeeded = did_not_succeed
    ).

:- pred install_ints_and_headers(globals::in, maybe_succeeded::in,
    module_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_ints_and_headers(Globals, SubdirLinkSucceeded, ModuleName, Succeeded,
        !Info, !IO) :-
    get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
        !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        % We always install the `.int0' files for a library even though they
        % are only required by the `.opt' files. This is because when building
        % a program with --intermodule-optimization enabled, the compiler will
        % look for `.int0' files of any libraries the program uses. It will do
        % this even for libraries that were not installed with
        % --intermodule-optimization enabled, returning an error if it cannot
        % find the `.int0' file.
        module_dep_info_get_children(ModuleDepInfo, Children),
        ( if set.is_empty(Children) then
            Exts0 = []
        else
            Exts0 = [{other_ext(".int0"), newext_int(ext_int_int0), "int0s"}]
        ),
        globals.get_any_intermod(Globals, AnyIntermod),
        (
            AnyIntermod = yes,
            Exts1 = [{other_ext(".opt"), newext_opt(ext_opt_plain), "opts"}
                | Exts0]
        ;
            AnyIntermod = no,
            Exts1 = Exts0
        ),
        Exts = [{other_ext(".int"), newext_int(ext_int_int1), "ints"},
            {other_ext(".int2"), newext_int(ext_int_int2), "int2s"},
            {other_ext(".int3"), newext_int(ext_int_int3), "int3s"},
            {other_ext(".module_dep"),
                newext_misc_ngs(ext_misc_ngs_module_dep), "module_deps"}
            | Exts1],
        globals.lookup_string_option(Globals, install_prefix, Prefix),
        LibDir = Prefix/"lib"/"mercury",
        list.map_foldl(
            install_subdir_file(Globals, SubdirLinkSucceeded, LibDir/"ints",
                ModuleName),
            Exts, Results, !IO),

        globals.get_target(Globals, Target),
        (
            % `.mh' files are (were) only generated for modules containing
            % `:- pragma foreign_export' declarations.
            % But `.mh' files are expected by Mmake so always generate them,
            % otherwise there is trouble using libraries installed by
            % `mmc --make' with Mmake.
            % XXX If we ever phase out mmake we could revert this behaviour.
            Target = target_c,
            % XXX Should we test
            % ModuleDepInfo ^ contains_foreign_export
            %   = contains_foreign_export?
            module_name_to_file_name(Globals, $pred, do_not_create_dirs,
                ext_other(other_ext(".mh")), newext_mh(ext_mh_mh),
                ModuleName, FileName, !IO),
            install_file(Globals, FileName, LibDir/"inc", HeaderSucceeded1,
                !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            install_subdir_file(Globals, SubdirLinkSucceeded, LibDir/"ints",
                ModuleName, {other_ext(".mh"), newext_mh(ext_mh_mh), "mhs"},
                HeaderSucceeded2, !IO),

            HeaderSucceeded = HeaderSucceeded1 `and` HeaderSucceeded2
        ;
            ( Target = target_java
            ; Target = target_csharp
            ),
            HeaderSucceeded = succeeded
        ),
        Succeeded = and_list([HeaderSucceeded | Results])
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        Succeeded = did_not_succeed
    ).

:- pred install_extra_headers(globals::in, maybe_succeeded::out,
    io::di, io::uo) is det.

install_extra_headers(Globals, ExtraHdrsSucceeded, !IO) :-
    globals.lookup_accumulating_option(Globals, extra_library_header,
        ExtraHdrs),
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    IncDir = Prefix / "lib" / "mercury" / "inc",
    list.foldl2(install_extra_header(Globals, IncDir), ExtraHdrs,
        succeeded, ExtraHdrsSucceeded, !IO).

:- pred install_extra_header(globals::in, dir_name::in, string::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

install_extra_header(Globals, IncDir, FileName, !Succeeded, !IO) :-
    install_file(Globals, FileName, IncDir, InstallSucceeded, !IO),
    !:Succeeded = !.Succeeded `and` InstallSucceeded.

install_library_grade(LinkSucceeded0, ModuleName, AllModules, Globals, Grade,
        Succeeded, !Info, !IO) :-
    % Only remove grade-dependent files after installing if
    % --use-grade-subdirs is not specified by the user.
    globals.lookup_bool_option(Globals, use_grade_subdirs, UseGradeSubdirs),
    CleanAfter = not(UseGradeSubdirs),

    % Set up so that grade-dependent files for the current grade
    % don't overwrite the files for the default grade.
    OptionArgs0 = make_info_get_option_args(!.Info),
    OptionArgs = OptionArgs0 ++ ["--grade", Grade, "--use-grade-subdirs"],

    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.format("Installing grade %s\n", [s(Grade)], !IO)
        ), !IO),

    lookup_mmc_options(make_info_get_options_variables(!.Info), MaybeMCFlags),
    (
        MaybeMCFlags = ok1(MCFlags),
        DetectedGradeFlags = make_info_get_detected_grade_flags(!.Info),
        AllFlags = DetectedGradeFlags ++ MCFlags ++ OptionArgs,
        io.output_stream(CurStream, !IO),
        handle_given_options(CurStream, AllFlags, _, _, OptionsSpecs,
            LibGlobals, !IO)
    ;
        MaybeMCFlags = error1(LookupSpecs),
        write_error_specs(Globals, LookupSpecs, !IO),
        % Errors should have been caught before.
        unexpected($pred, "bad DEFAULT_MCFLAGS")
    ),

    (
        OptionsSpecs = [_ | _],
        get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
        usage_errors(ErrorStream, Globals, OptionsSpecs, !IO),
        Succeeded = did_not_succeed
    ;
        OptionsSpecs = [],

        % Remove the grade-dependent targets from the status map
        % (we need to rebuild them in the new grade).

        % XXX version_hash_table.delete is not working properly so just clear
        % the dependency status cache completely.
        %
        % StatusMap0 = !.Info ^ dependency_status,
        % StatusMap = version_hash_table.fold(remove_grade_dependent_targets,
        %     StatusMap0, StatusMap0),
        StatusMap = version_hash_table.init_default(dependency_file_hash),

        make_info_set_dependency_status(StatusMap, !Info),
        make_info_set_option_args(OptionArgs, !Info),

        % Building the library in the new grade is done in a separate process
        % to make it easier to stop and clean up on an interrupt.
        globals.lookup_bool_option(LibGlobals, very_verbose, VeryVerbose),
        setup_checking_for_interrupt(Cookie, !IO),
        call_in_forked_process(
            install_library_grade_2(LibGlobals, LinkSucceeded0,
                ModuleName, AllModules, !.Info, CleanAfter),
            Succeeded0, !IO),
        Cleanup = maybe_make_grade_clean(LibGlobals, CleanAfter, ModuleName,
            AllModules),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, Cleanup,
            Succeeded0, Succeeded, !Info, !IO)
    ).

:- func remove_grade_dependent_targets(dependency_file, dependency_status,
    version_hash_table(dependency_file, dependency_status)) =
    version_hash_table(dependency_file, dependency_status).
% See the comment above for the reason why we don't use this function
% (that should be a predicate anyway).
:- pragma consider_used(func(remove_grade_dependent_targets/3)).

remove_grade_dependent_targets(File, _Status, StatusMap0) = StatusMap :-
    ( if
        File = dep_target(target_file(_, Target)),
        target_is_grade_or_arch_dependent(Target)
    then
        StatusMap = delete(StatusMap0, File)
    else
        StatusMap = StatusMap0
    ).

:- pred install_library_grade_2(globals::in, maybe_succeeded::in,
    module_name::in, list(module_name)::in, make_info::in,
    bool::in, maybe_succeeded::out, io::di, io::uo) is det.

install_library_grade_2(Globals, LinkSucceeded0, ModuleName, AllModules,
        Info0, CleanAfter, Succeeded, !IO) :-
    make_misc_target(Globals, ModuleName - misc_target_build_library,
        LibSucceeded, Info0, Info1, [], Specs, !IO),
    (
        LibSucceeded = succeeded,
        % `GradeDir' differs from `Grade' in that it is in canonical form.
        grade_directory_component(Globals, GradeDir),
        install_library_grade_files(Globals, LinkSucceeded0, GradeDir,
            ModuleName, AllModules, Succeeded, Info1, Info2, !IO),
        maybe_make_grade_clean(Globals, CleanAfter, ModuleName, AllModules,
            Info2, _Info, !IO)
    ;
        LibSucceeded = did_not_succeed,
        write_error_specs(Globals, Specs, !IO),
        Succeeded = did_not_succeed
    ).

    % Install the `.a', `.so', `.jar', `.opt' and `.mih' files for
    % the current grade.
    %
    % NOTE: changes here may require changes to
    %       file_util.get_install_name_option/4.
    %
:- pred install_library_grade_files(globals::in, maybe_succeeded::in,
    string::in, module_name::in, list(module_name)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_library_grade_files(Globals, LinkSucceeded0, GradeDir, ModuleName,
        AllModules, Succeeded, !Info, !IO) :-
    make_grade_install_dirs(Globals, GradeDir, DirResult, LinkSucceeded1, !IO),
    LinkSucceeded = LinkSucceeded0 `and` LinkSucceeded1,
    (
        DirResult = succeeded,
        linked_target_file_name(Globals, ModuleName, static_library,
            LibFileName, !IO),
        linked_target_file_name(Globals, ModuleName, shared_library,
            SharedLibFileName, !IO),
        linked_target_file_name(Globals, ModuleName, csharp_library,
            DllFileName, !IO),
        linked_target_file_name(Globals, ModuleName, java_archive,
            JarFileName, !IO),

        globals.lookup_string_option(Globals, install_prefix, Prefix),

        ( if string.prefix(GradeDir, "csharp") then
            GradeLibDir = Prefix/"lib"/"mercury"/"lib"/GradeDir,
            install_file(Globals, DllFileName, GradeLibDir, LibsSucceeded,
                !IO),
            InitSucceeded = succeeded
        else if string.prefix(GradeDir, "java") then
            GradeLibDir = Prefix/"lib"/"mercury"/"lib"/GradeDir,
            install_file(Globals, JarFileName, GradeLibDir, LibsSucceeded,
                !IO),
            InitSucceeded = succeeded
        else
            GradeLibDir = Prefix/"lib"/"mercury"/"lib"/GradeDir,
            maybe_install_library_file(Globals, "static", LibFileName,
                GradeLibDir, LibSucceeded0, !IO),
            ( if LibFileName = SharedLibFileName then
                LibsSucceeded = LibSucceeded0
            else
                maybe_install_library_file(Globals, "shared",
                    SharedLibFileName, GradeLibDir, SharedLibSucceeded, !IO),
                LibsSucceeded = LibSucceeded0 `and` SharedLibSucceeded
            ),
            install_grade_init(Globals, GradeDir, ModuleName, InitSucceeded,
                !IO)
        ),

        list.map_foldl2(
            install_grade_ints_and_headers(Globals, LinkSucceeded, GradeDir),
            AllModules, IntsHeadersSucceeded, !Info, !IO),
        Succeeded = and_list(
            [LibsSucceeded, InitSucceeded | IntsHeadersSucceeded])
    ;
        DirResult = did_not_succeed,
        Succeeded = did_not_succeed
    ).

    % Install the `.init' file for the current grade.
    %
:- pred install_grade_init(globals::in, string::in, module_name::in,
    maybe_succeeded::out, io::di, io::uo) is det.

install_grade_init(Globals, GradeDir, ModuleName, Succeeded, !IO) :-
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    GradeModulesDir = Prefix / "lib" / "mercury" / "modules" / GradeDir,
    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(other_ext(".init")), newext_lib_gs(ext_lib_gs_init),
        ModuleName, InitFileName, !IO),
    install_file(Globals, InitFileName, GradeModulesDir, Succeeded, !IO).

    % Install the `.opt', `.analysis' and `.mih' files for the current grade.
    %
:- pred install_grade_ints_and_headers(globals::in, maybe_succeeded::in,
    string::in, module_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_grade_ints_and_headers(Globals, LinkSucceeded, GradeDir, ModuleName,
        Succeeded, !Info, !IO) :-
    get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
        !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(_ModuleDepInfo),
        globals.lookup_string_option(Globals, install_prefix, Prefix),
        LibDir = Prefix/"lib"/"mercury",

        globals.get_target(Globals, Target),
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        ( if
            Target = target_c,
            HighLevelCode = yes
        then
            GradeIncDir = LibDir/"lib"/GradeDir/"inc",
            install_subdir_file(Globals, LinkSucceeded, GradeIncDir,
                ModuleName,
                {other_ext(".mih"), newext_mih(ext_mih_mih), "mihs"},
                HeaderSucceeded1, !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            IntDir = LibDir/"ints",
            install_subdir_file(Globals, LinkSucceeded, IntDir, ModuleName,
                {other_ext(".mih"), newext_mih(ext_mih_mih), "mihs"},
                HeaderSucceeded2, !IO),
            HeaderSucceeded = HeaderSucceeded1 `and` HeaderSucceeded2
        else
            HeaderSucceeded = succeeded
        ),

        GradeIntDir = LibDir/"ints"/GradeDir,
        globals.get_any_intermod(Globals, AnyIntermod),
        (
            AnyIntermod = yes,
            install_subdir_file(Globals, LinkSucceeded, GradeIntDir,
                ModuleName,
                {other_ext(".opt"), newext_opt(ext_opt_plain), "opts"},
                OptSucceeded, !IO)
        ;
            AnyIntermod = no,
            OptSucceeded = succeeded
        ),
        globals.lookup_bool_option(Globals, intermodule_analysis,
            IntermodAnalysis),
        (
            IntermodAnalysis = yes,
            install_subdir_file(Globals, LinkSucceeded, GradeIntDir,
                ModuleName,
                {other_ext(".analysis"), newext_analysis(ext_an_analysis),
                    "analysiss"},
                IntermodAnalysisSucceeded, !IO)
        ;
            IntermodAnalysis = no,
            IntermodAnalysisSucceeded = succeeded
        ),
        Succeeded = HeaderSucceeded `and` OptSucceeded `and`
            IntermodAnalysisSucceeded
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        Succeeded = did_not_succeed
    ).

    % Install a file in the given directory, and in directory/Mercury/exts
    % if the symlinks for the subdirectories couldn't be created
    % (e.g. on Windows).
    %
:- pred install_subdir_file(globals::in, maybe_succeeded::in, dir_name::in,
    module_name::in, {other_ext, newext, string}::in,
    maybe_succeeded::out, io::di, io::uo) is det.

install_subdir_file(Globals, SubdirLinkSucceeded, InstallDir, ModuleName,
        {Ext, NewExt, Exts}, Succeeded, !IO) :-
    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(Ext), NewExt, ModuleName, FileName, !IO),
    install_file(Globals, FileName, InstallDir, Succeeded1, !IO),
    (
        SubdirLinkSucceeded = did_not_succeed,
        install_file(Globals, FileName, InstallDir/"Mercury"/Exts,
            Succeeded2, !IO),
        Succeeded = Succeeded1 `and` Succeeded2
    ;
        SubdirLinkSucceeded = succeeded,
        Succeeded = Succeeded1
    ).

:- pred maybe_install_library_file(globals::in, string::in, file_name::in,
    dir_name::in, maybe_succeeded::out, io::di, io::uo) is det.

maybe_install_library_file(Globals, Linkage, FileName, InstallDir, Succeeded,
        !IO) :-
    globals.lookup_accumulating_option(Globals, lib_linkages, LibLinkages),
    ( if list.member(Linkage, LibLinkages) then
        install_file(Globals, FileName, InstallDir, Succeeded0, !IO),

        % We need to update the archive index after we copy a .a file to
        % the installation directory because the linkers on some OSs
        % complain if we don't.
        ( if
            Linkage = "static",
            Succeeded0 = succeeded
        then
            % Since mmc --make uses --use-subdirs the above FileName will
            % be directory qualified. We don't care about the build
            % directory here so we strip that qualification off.

            BaseFileName = dir.det_basename(FileName),
            generate_archive_index(Globals, BaseFileName, InstallDir,
                Succeeded, !IO)
        else
            Succeeded = Succeeded0
        )
    else
        Succeeded = succeeded
    ).

:- pred install_file(globals::in, file_name::in, dir_name::in,
    maybe_succeeded::out, io::di, io::uo) is det.

install_file(Globals, FileName, InstallDir, Succeeded, !IO) :-
    % XXX STREAM This preserves old behavior, but our caller
    % should pass to us a progress stream *explicitly*.
    io.output_stream(OutputStream, !IO),
    ProgressStream = OutputStream,
    ErrorStream = OutputStream,
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.format(ProgressStream, "Installing file %s in %s\n",
                [s(FileName), s(InstallDir)], !IO),
            io.flush_output(ProgressStream, !IO)
        ), !IO),
    Command = make_install_file_command(Globals, FileName, InstallDir),
    invoke_system_command(Globals, ProgressStream, ErrorStream, OutputStream,
        cmd_verbose, Command, Succeeded, !IO).

:- pred install_directory(globals::in, dir_name::in, dir_name::in,
    maybe_succeeded::out, io::di, io::uo) is det.
:- pragma consider_used(pred(install_directory/6)).

install_directory(Globals, SourceDirName, InstallDir, Succeeded, !IO) :-
    % XXX STREAM This preserves old behavior, but our caller
    % should pass to us a progress stream *explicitly*.
    io.output_stream(OutputStream, !IO),
    ProgressStream = OutputStream,
    ErrorStream = OutputStream,
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.format(ProgressStream, "Installing directory %s in %s\n",
                [s(SourceDirName), s(InstallDir)], !IO),
            io.flush_output(ProgressStream, !IO)
        ), !IO),
    Command = make_install_dir_command(Globals, SourceDirName, InstallDir),
    invoke_system_command(Globals, ProgressStream, ErrorStream, OutputStream,
        cmd_verbose, Command, Succeeded, !IO).

:- pred make_install_dirs(globals::in,
    maybe_succeeded::out, maybe_succeeded::out, io::di, io::uo) is det.

make_install_dirs(Globals, Result, LinkResult, !IO) :-
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    LibDir = Prefix/"lib"/"mercury",
    make_directory(LibDir/"inc", Result1, !IO),
    make_directory(LibDir/"modules", Result2, !IO),

    IntsSubdir = LibDir/"ints"/"Mercury",
    make_directory(IntsSubdir, Result3, !IO),
    Results0 = [Result1, Result2, Result3],

    Subdirs = ["int0", "int", "int2", "int3", "opt", "trans_opt",
        "mh", "mih", "module_dep"],
    list.map_foldl(make_install_symlink(Globals, IntsSubdir), Subdirs,
        LinkResults, !IO),
    LinkResult = and_list(LinkResults),
    (
        LinkResult = succeeded,
        Results = Results0
    ;
        LinkResult = did_not_succeed,
        list.map_foldl(
            ( pred(Ext::in, MkDirResult::out, !.IO::di, !:IO::uo) is det:-
                make_directory(IntsSubdir/(Ext ++ "s"), MkDirResult, !IO)
            ), Subdirs, MkDirResults, !IO),
        Results = Results0 ++ MkDirResults
    ),
    print_mkdir_errors(Results, Result, !IO).

:- pred make_grade_install_dirs(globals::in, string::in,
    maybe_succeeded::out, maybe_succeeded::out, io::di, io::uo) is det.

make_grade_install_dirs(Globals, Grade, Result, LinkResult, !IO) :-
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    LibDir = Prefix/"lib"/"mercury",

    GradeIntsSubdir = LibDir/"ints"/Grade/"Mercury",
    make_directory(GradeIntsSubdir, Result1, !IO),

    GradeIncSubdir = LibDir/"lib"/Grade/"inc"/"Mercury",
    make_directory(GradeIncSubdir, Result2, !IO),

    GradeModuleSubdir = LibDir/"modules"/Grade,
    make_directory(GradeModuleSubdir, Result3, !IO),

    Results0 = [Result1, Result2, Result3],

    make_install_symlink(Globals, GradeIncSubdir, "mih", LinkResult0, !IO),
    list.map_foldl(make_install_symlink(Globals, GradeIntsSubdir),
        ["opt", "trans_opt", "analysis"], LinkResults, !IO),
    LinkResult = and_list([LinkResult0 | LinkResults]),
    (
        LinkResult = succeeded,
        Results = Results0
    ;
        LinkResult = did_not_succeed,
        make_directory(GradeIncSubdir/"mihs", Result4, !IO),
        make_directory(GradeIntsSubdir/"opts", Result5, !IO),
        make_directory(GradeIntsSubdir/"trans_opts", Result6, !IO),
        make_directory(GradeIntsSubdir/"analysiss", Result7, !IO),
        Results = [Result4, Result5, Result6, Result7 | Results0]
    ),
    print_mkdir_errors(Results, Result, !IO).

:- pred print_mkdir_errors(list(io.res)::in, maybe_succeeded::out,
    io::di, io::uo) is det.

print_mkdir_errors([], succeeded, !IO).
print_mkdir_errors([ok | Rest], Succeeded, !IO) :-
    print_mkdir_errors(Rest, Succeeded, !IO).
print_mkdir_errors([error(Error) | Rest], did_not_succeed, !IO) :-
    io.format("Error creating installation directories: %s\n",
        [s(io.error_message(Error))], !IO),
    print_mkdir_errors(Rest, _, !IO).

:- pred make_install_symlink(globals::in, string::in, string::in,
    maybe_succeeded::out, io::di, io::uo) is det.

make_install_symlink(Globals, Subdir, Ext, Succeeded, !IO) :-
    LinkName = Subdir/(Ext ++ "s"),
    maybe_make_symlink(Globals, "..", LinkName, Succeeded, !IO).

    % Generate (or update) the index for an archive file,
    % i.e. run ranlib on a .a file.
    %
:- pred generate_archive_index(globals::in, file_name::in, dir_name::in,
    maybe_succeeded::out, io::di, io::uo) is det.

generate_archive_index(Globals, FileName, InstallDir, Succeeded, !IO) :-
    % XXX STREAM This preserves old behavior, but our caller
    % should pass to us a progress stream *explicitly*.
    io.output_stream(OutputStream, !IO),
    ProgressStream = OutputStream,
    ErrorStream = OutputStream,
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.format(ProgressStream,
                "Generating archive index for file %s in %s\n",
                [s(FileName), s(InstallDir)], !IO),
            io.flush_output(ProgressStream, !IO)
        ), !IO),
    globals.lookup_string_option(Globals, ranlib_command, RanLibCommand),
    globals.lookup_string_option(Globals, ranlib_flags, RanLibFlags),
    % XXX What is the point of using more than one space?
    Command = string.join_list("    ", [
        quote_shell_cmd_arg(RanLibCommand),
        RanLibFlags,
        quote_shell_cmd_arg(InstallDir / FileName)
    ]),
    invoke_system_command(Globals, ProgressStream, ErrorStream, OutputStream,
        cmd_verbose, Command, Succeeded, !IO).

%---------------------------------------------------------------------------%

:- pred maybe_make_grade_clean(globals::in, bool::in, module_name::in,
    list(module_name)::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

maybe_make_grade_clean(Globals, Clean, ModuleName, AllModules, !Info, !IO) :-
    (
        Clean = yes,
        make_grade_clean(Globals, ModuleName, AllModules, !Info, !IO)
    ;
        Clean = no
    ).

    % Clean up grade-dependent files.
    %
:- pred make_grade_clean(globals::in, module_name::in, list(module_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_grade_clean(Globals, ModuleName, AllModules, !Info, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            grade_directory_component(Globals, Grade),
            io.format(
                "Cleaning up grade-dependent files for `%s' in grade %s.\n",
                [s(sym_name_to_escaped_string(ModuleName)), s(Grade)], !IO)
        ), !IO),

    make_main_module_realclean(Globals, ModuleName, !Info, !IO),
    list.foldl2(make_module_clean(Globals), AllModules, !Info, !IO).

:- pred make_main_module_realclean(globals::in, module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_main_module_realclean(Globals, ModuleName, !Info, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.format("Removing executable and library files for `%s'.\n",
                [s(sym_name_to_escaped_string(ModuleName))], !IO)
        ), !IO),

    LinkedTargetTypes = [
        executable,
        static_library,
        shared_library,
        csharp_executable,
        csharp_library,
        java_executable,
        java_archive
    ],
    list.map_foldl(linked_target_file_name(Globals, ModuleName),
        LinkedTargetTypes, FileNames, !IO),
    % Remove the symlinks created for `--use-grade-subdirs'.
    globals.set_option(use_grade_subdirs, bool(no), Globals, NoSubdirGlobals),
    list.map_foldl(linked_target_file_name(NoSubdirGlobals, ModuleName),
        LinkedTargetTypes, ThisDirFileNames, !IO),
    % XXX This symlink should not be necessary anymore for `mmc --make'.
    module_name_to_file_name(NoSubdirGlobals, $pred, do_not_create_dirs,
        ext_other(other_ext(".init")), newext_lib_gs(ext_lib_gs_init),
        ModuleName, ThisDirInitFileName, !IO),

    list.foldl2(make_remove_file(Globals, very_verbose),
        FileNames ++ ThisDirFileNames ++ [ThisDirInitFileName],
        !Info, !IO),
    remove_make_module_file(Globals, very_verbose, ModuleName,
        ext_other(other_ext(".init")), newext_lib_gs(ext_lib_gs_init),
        !Info, !IO),
    remove_init_files(Globals, very_verbose, ModuleName, !Info, !IO).

:- pred remove_init_files(globals::in, option::in, module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_init_files(Globals, Verbose, ModuleName, !Info, !IO) :-
    globals.lookup_string_option(Globals, object_file_extension, ObjExt),
    globals.lookup_string_option(Globals, pic_object_file_extension,
        PicObjExt),
    % XXX EXT
    remove_make_module_file(Globals, Verbose, ModuleName,
        ext_other(other_ext("_init.c")),
        newext_target_init_c(ext_init_c), !Info, !IO),
    remove_make_module_file(Globals, Verbose, ModuleName,
        ext_other(other_ext("_init" ++ ObjExt)),
        newext_target_init_obj(ext_init_obj_obj_opt), !Info, !IO),
    remove_make_module_file(Globals, Verbose, ModuleName,
        ext_other(other_ext("_init" ++ PicObjExt)),
        newext_target_init_obj(ext_init_obj_pic_obj_opt), !Info, !IO).

%---------------------------------------------------------------------------%

:- pred make_module_clean(globals::in, module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_module_clean(Globals, ModuleName, !Info, !IO) :-
    verbose_make_msg_option(Globals, verbose_make,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.format("Cleaning up target files for module `%s'.\n",
                [s(sym_name_to_escaped_string(ModuleName))], !IO)
        ), !IO),

    list.foldl2(
        remove_make_target_file_by_name(Globals, $pred, very_verbose,
            ModuleName),
        [module_target_errors,
        module_target_c_code,
        module_target_c_header(header_mih),
        module_target_csharp_code,
        module_target_java_code,
        module_target_java_class_code], !Info, !IO),

    remove_make_module_file(Globals, very_verbose, ModuleName,
        ext_other(other_ext(".used")), newext_misc_gs(ext_misc_gs_used),
        !Info, !IO),
    remove_make_module_file(Globals, very_verbose, ModuleName,
        ext_other(other_ext(".prof")), newext_misc_ngs(ext_misc_ngs_prof),
        !Info, !IO),

    get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
        !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_fact_tables(ModuleDepInfo, FactTableFilesSet),
        set.to_sorted_list(FactTableFilesSet, FactTableFiles)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        FactTableFiles = []
    ),

    list.foldl2(remove_fact_table_c_file(Globals), FactTableFiles, !Info, !IO),

    foreign_language_module_name(ModuleName, lang_c, CCodeModule),
    remove_make_target_file_by_name(Globals, $pred, very_verbose, CCodeModule,
        module_target_c_code, !Info, !IO),

    remove_object_and_assembler_files(Globals, ModuleName, pic,
        FactTableFiles, !Info, !IO),
    remove_object_and_assembler_files(Globals, ModuleName, non_pic,
        FactTableFiles, !Info, !IO).

:- pred remove_fact_table_c_file(globals::in, string::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_fact_table_c_file(Globals, FactTableFile, !Info, !IO) :-
    fact_table_file_name(Globals, $pred, do_not_create_dirs,
        other_ext(".c"), newext_target_c_cs(ext_target_c),
        FactTableFile, FactTableCFile, !IO),
    make_remove_file(Globals, very_verbose, FactTableCFile, !Info, !IO).

:- pred remove_object_and_assembler_files(globals::in, module_name::in,
    pic::in, list(file_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_object_and_assembler_files(Globals, ModuleName, PIC, FactTableFiles,
        !Info, !IO) :-
    remove_make_target_file_by_name(Globals, $pred, very_verbose, ModuleName,
        module_target_object_code(PIC), !Info, !IO),
    remove_make_target_file_by_name(Globals, $pred, very_verbose, ModuleName,
        module_target_foreign_object(PIC, lang_c), !Info, !IO),
    list.foldl2(
        remove_fact_table_object_and_assembler_files(Globals, ModuleName, PIC),
        FactTableFiles, !Info, !IO).

:- pred remove_fact_table_object_and_assembler_files(globals::in,
    module_name::in, pic::in, file_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_fact_table_object_and_assembler_files(Globals, ModuleName, PIC,
        FactTableFile, !Info, !IO) :-
    remove_make_target_file_by_name(Globals, $pred, very_verbose,
        ModuleName, module_target_fact_table_object(PIC, FactTableFile),
        !Info, !IO).

%---------------------------------------------------------------------------%

:- pred make_module_realclean(globals::in, module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_module_realclean(Globals, ModuleName, !Info, !IO) :-
    make_module_clean(Globals, ModuleName, !Info, !IO),

    verbose_make_msg_option(Globals, verbose_make,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.format("Cleaning up interface files for module `%s'\n",
                [s(sym_name_to_escaped_string(ModuleName))], !IO)
        ), !IO),
    Targets = [module_target_int0, module_target_int1, module_target_int2,
        module_target_int3, module_target_opt,
        module_target_analysis_registry,
        module_target_c_header(header_mh),
        module_target_track_flags],
    list.foldl2(
        remove_make_target_file_by_name(Globals, $pred, very_verbose,
            ModuleName),
        Targets, !Info, !IO),
    remove_make_module_file(Globals, very_verbose, ModuleName,
        ext_other(make_module_dep_file_extension),
        newext_misc_ngs(ext_misc_ngs_module_dep), !Info, !IO),
    remove_make_module_file(Globals, very_verbose, ModuleName,
        ext_other(other_ext(".imdg")), newext_analysis(ext_an_imdg),
        !Info, !IO),
    remove_make_module_file(Globals, very_verbose, ModuleName,
        ext_other(other_ext(".request")), newext_analysis(ext_an_request),
        !Info, !IO).

%---------------------------------------------------------------------------%
:- end_module make.program_target.
%---------------------------------------------------------------------------%
