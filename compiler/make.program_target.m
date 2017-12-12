%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: make.program_target.m.
% Main author: stayl.
%
% Build targets which relate to whole programs or libraries.
%
%-----------------------------------------------------------------------------%

:- module make.program_target.
:- interface.

%-----------------------------------------------------------------------------%

    % make_linked_target(Globals, Target, Success, !Info, !IO):
    %
    % Build a library or an executable.
    %
:- pred make_linked_target(globals::in, linked_target_file::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % make_misc_target(Globals, Target, Success, !Info, !IO):
    %
    % Handle miscellaneous target types, including clean-up, library
    % installation, and building all files of a given type for all
    % modules in the program.
    %
:- pred make_misc_target(globals::in, pair(module_name, misc_target_type)::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module libs.compute_grade.
:- import_module libs.process_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_deps_graph.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module digraph.
:- import_module dir.
:- import_module getopt_io.
:- import_module int.
:- import_module multi_map.
:- import_module require.

%-----------------------------------------------------------------------------%

make_linked_target(Globals, LinkedTargetFile, LinkedTargetSucceeded,
        !Info, !IO) :-
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
        ; FileType = erlang_launcher
        ; FileType = erlang_archive
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
        LinkedTargetSucceeded = yes
    else
        globals.lookup_bool_option(Globals, libgrade_install_check,
            LibgradeCheck),
        (
            LibgradeCheck = yes,
            check_libraries_are_installed(Globals, LibgradeCheckSucceeded, !IO)
        ;
            LibgradeCheck = no,
            LibgradeCheckSucceeded = yes
        ),
        (
            LibgradeCheckSucceeded = yes,
            maybe_with_analysis_cache_dir(Globals,
                make_linked_target_1(Globals, LinkedTargetFile, ExtraOptions),
                LinkedTargetSucceeded, !Info, !IO)
        ;
            LibgradeCheckSucceeded = no,
            LinkedTargetSucceeded = no
        )
    ).

:- pred make_linked_target_1(globals::in, linked_target_file::in,
    list(string)::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

make_linked_target_1(Globals, LinkedTargetFile, ExtraOptions, Succeeded,
        !Info, !IO) :-
    LinkedTargetFile = linked_target_file(MainModuleName, _FileType),

    % When using `--intermodule-analysis', perform an analysis pass first.
    % The analysis of one module may invalidate the results of a module
    % we analysed earlier, so this step must be carried out until all the
    % `.analysis' files are in a valid state before we can continue.
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    (
        IntermodAnalysis = yes,
        make_misc_target_builder(MainModuleName - misc_target_build_analyses,
            Globals, ExtraOptions, IntermodAnalysisSucceeded, !Info, !IO)
    ;
        IntermodAnalysis = no,
        IntermodAnalysisSucceeded = yes
    ),

    (
        IntermodAnalysisSucceeded = yes,
        build_with_module_options(Globals, MainModuleName, ExtraOptions,
            make_linked_target_2(LinkedTargetFile), Succeeded, !Info, !IO)
    ;
        IntermodAnalysisSucceeded = no,
        Succeeded = no
    ).

:- pred make_linked_target_2(linked_target_file::in,
    globals::in, list(string)::in, bool::out, make_info::in, make_info::out,
     io::di, io::uo) is det.

make_linked_target_2(LinkedTargetFile, Globals, _, Succeeded, !Info, !IO) :-
    LinkedTargetFile = linked_target_file(MainModuleName, FileType),
    find_reachable_local_modules(Globals, MainModuleName, DepsSuccess,
        AllModules, !Info, !IO),
    globals.lookup_bool_option(Globals, keep_going, KeepGoing),
    ( if
        DepsSuccess = no,
        KeepGoing = no
    then
        Succeeded = no
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
        ;
            CompilationTarget = target_erlang,
            IntermediateTargetType = module_target_erlang_code,
            ObjectTargetType = module_target_erlang_beam_code
        ),

        AllModulesList = set.to_sorted_list(AllModules),
        get_target_modules(Globals, IntermediateTargetType, AllModulesList,
            ObjModulesAlpha, !Info, !IO),
        order_target_modules(Globals, ObjModulesAlpha, ObjModules, !Info, !IO),
        remove_nested_modules(Globals, ObjModules, ObjModulesNonnested, !Info,
            !IO),
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
            IntsSucceeded = no,
            KeepGoing = no
        then
            BuildDepsSucceeded = no
        else
            foldl2_maybe_stop_at_error_maybe_parallel(KeepGoing,
                make_module_target, Globals, IntermediateTargetsNonnested,
                BuildDepsSucceeded0, !Info, !IO),
            (
                BuildDepsSucceeded0 = yes,
                ( if ObjectTargetType = module_target_java_class_code then
                    make_java_files(Globals, MainModuleName, ObjModules,
                        BuildJavaSucceeded, !Info, !IO),
                    (
                        BuildJavaSucceeded = yes,
                        % Disable the `--rebuild' option during this pass,
                        % otherwise all the Java classes will be built again.
                        globals.set_option(rebuild, bool(no),
                            Globals, NoRebuildGlobals),
                        foldl2_maybe_stop_at_error_maybe_parallel(
                            KeepGoing, make_module_target, NoRebuildGlobals,
                            ObjTargets, BuildDepsSucceeded1, !Info, !IO)
                    ;
                        BuildJavaSucceeded = no,
                        BuildDepsSucceeded1 = no
                    )
                else
                    foldl2_maybe_stop_at_error_maybe_parallel(KeepGoing,
                        make_module_target, Globals, ObjTargets,
                        BuildDepsSucceeded1, !Info, !IO)
                )
            ;
                BuildDepsSucceeded0 = no,
                BuildDepsSucceeded1 = no
            ),
            (
                BuildDepsSucceeded1 = yes,
                foldl2_maybe_stop_at_error(KeepGoing, make_module_target,
                    Globals, ForeignObjTargets, BuildDepsSucceeded,
                    !Info, !IO)
            ;
                BuildDepsSucceeded1 = no,
                BuildDepsSucceeded = no
            )
        ),

        linked_target_file_name(Globals, MainModuleName, FileType,
            OutputFileName, !IO),
        get_file_timestamp([dir.this_directory], OutputFileName,
            MaybeTimestamp, !Info, !IO),
        check_dependencies(Globals, OutputFileName, MaybeTimestamp,
            BuildDepsSucceeded, ObjTargets, BuildDepsResult, !Info, !IO),
        ( if
            DepsSuccess = yes,
            BuildDepsResult \= deps_error
        then
            globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
            build_with_check_for_interrupt(VeryVerbose,
                build_with_output_redirect(Globals, MainModuleName,
                    build_linked_target(MainModuleName, FileType,
                        OutputFileName, MaybeTimestamp, AllModules, ObjModules,
                        CompilationTarget, PIC, DepsSuccess, BuildDepsResult)
                    ),
                linked_target_cleanup(Globals, MainModuleName, FileType,
                    OutputFileName),
                Succeeded, !Info, !IO)
        else
            Succeeded = no
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
    get_module_dependencies(Globals, ModuleName, MaybeImports, !Info, !IO),
    ( if
        MaybeImports = yes(Imports),
        ModuleName = Imports ^ mai_source_file_module_name
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

    globals.get_target(Globals, CompilationTarget),
    get_module_dependencies(Globals, ModuleName, MaybeImports, !Info, !IO),
    (
        MaybeImports = yes(Imports)
    ;
        MaybeImports = no,
        unexpected($module, $pred, "unknown imports")
    ),

    % XXX only used by the IL backend.
    ForeignObjectTargets = [],

    % Find out if any externally compiled foreign code files for fact tables
    % exist.
    (
        CompilationTarget = target_c,
        FactObjectTargets = list.map(
            (func(FactFile) =
                dep_target(target_file(ModuleName,
                    module_target_fact_table_object(PIC, FactFile)))
            ),
            Imports ^ mai_fact_table_deps),
        ObjectTargets = FactObjectTargets ++ ForeignObjectTargets
    ;
        ( CompilationTarget = target_java
        ; CompilationTarget = target_csharp
        ; CompilationTarget = target_erlang
        ),
        ObjectTargets = ForeignObjectTargets
    ).

:- pred build_linked_target(module_name::in, linked_target_type::in,
    file_name::in, maybe_error(timestamp)::in, set(module_name)::in,
    list(module_name)::in, compilation_target::in, pic::in, bool::in,
    dependencies_result::in, globals::in, io.output_stream::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_linked_target(MainModuleName, FileType, OutputFileName, MaybeTimestamp,
        AllModules, ObjModules, CompilationTarget, PIC, DepsSuccess,
        BuildDepsResult, Globals, ErrorStream, Succeeded, !Info, !IO) :-
    globals.lookup_maybe_string_option(Globals, pre_link_command,
        MaybePreLinkCommand),
    (
        MaybePreLinkCommand = yes(PreLinkCommand),
        make_all_module_command(Globals, PreLinkCommand, MainModuleName,
            to_sorted_list(AllModules), CommandString, !IO),
        invoke_system_command(Globals, ErrorStream, cmd_verbose,
            CommandString, PreLinkSucceeded, !IO)
    ;
        MaybePreLinkCommand = no,
        PreLinkSucceeded = yes
    ),
    (
        PreLinkSucceeded = yes,
        build_linked_target_2(Globals, MainModuleName, FileType,
            OutputFileName, MaybeTimestamp, AllModules, ObjModules,
            CompilationTarget, PIC, DepsSuccess, BuildDepsResult, ErrorStream,
            Succeeded, !Info, !IO)
    ;
        PreLinkSucceeded = no,
        Succeeded = no
    ).

:- pred build_linked_target_2(globals::in, module_name::in,
    linked_target_type::in, file_name::in, maybe_error(timestamp)::in,
    set(module_name)::in, list(module_name)::in, compilation_target::in,
    pic::in, bool::in, dependencies_result::in, io.output_stream::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

build_linked_target_2(Globals, MainModuleName, FileType, OutputFileName,
        MaybeTimestamp, AllModules, ObjModules, CompilationTarget, PIC,
        DepsSuccess, BuildDepsResult, ErrorStream, Succeeded, !Info, !IO) :-
    % Clear the option -- we'll pass the list of files directly.
    globals.lookup_accumulating_option(Globals, link_objects, LinkObjects),
    globals.set_option(link_objects, accumulating([]),
        Globals, NoLinkObjsGlobals),

    % Remake the `_init.o' file.
    % XXX We should probably make a `_init.o' file for shared
    % libraries linked using dlopen().
    AllModulesList = set.to_sorted_list(AllModules),
    (
        FileType = executable,
        make_init_obj_file(NoLinkObjsGlobals, ErrorStream, MainModuleName,
            AllModulesList, InitObjectResult, !IO),
        MaybeInitObjectResult = yes(InitObjectResult)
    ;
        FileType = erlang_launcher,
        make_erlang_program_init_file(NoLinkObjsGlobals, ErrorStream,
            MainModuleName, AllModulesList, InitObjectResult, !IO),
        MaybeInitObjectResult = yes(InitObjectResult)
    ;
        ( FileType = static_library
        ; FileType = shared_library
        ; FileType = csharp_executable
        ; FileType = csharp_library
        ; FileType = java_executable
        ; FileType = java_archive
        ; FileType = erlang_archive
        ),
        MaybeInitObjectResult = no
    ),
    (
        MaybeInitObjectResult = yes(InitObjectResult1),
        (
            InitObjectResult1 = yes(InitObject),
            % We may need to update the timestamp of the `_init.o'
            % or `_init.beam' file.
            !Info ^ file_timestamps :=
                map.delete(!.Info ^ file_timestamps, InitObject),
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
        list.map((func(F) = dep_file(F, no)), ObjectsToCheck),
            ExtraObjStatus, !Info, !IO),

    ( if list.member(deps_status_error, ExtraObjStatus) then
        DepsResult3 = deps_error
    else
        DepsResult3 = DepsResult2
    ),
    BuildDepsSuccess = ( if DepsResult3 = deps_error then no else yes ),
    list.map_foldl2(get_file_timestamp([dir.this_directory]),
        ObjectsToCheck, ExtraObjectTimestamps, !Info, !IO),
    check_dependency_timestamps(NoLinkObjsGlobals, OutputFileName,
        MaybeTimestamp, BuildDepsSuccess, ObjectsToCheck, io.write,
        ExtraObjectTimestamps, ExtraObjectDepsResult, !IO),

    (
        DepsSuccess = yes,
        DepsResult4 = DepsResult3
    ;
        DepsSuccess = no,
        DepsResult4 = deps_error
    ),
    ( DepsResult4 = deps_error, DepsResult = DepsResult4
    ; DepsResult4 = deps_out_of_date, DepsResult = DepsResult4
    ; DepsResult4 = deps_up_to_date, DepsResult = ExtraObjectDepsResult
    ),
    (
        DepsResult = deps_error,
        file_error(!.Info, OutputFileName, !IO),
        Succeeded = no
    ;
        DepsResult = deps_up_to_date,
        MsgTarget = MainModuleName - linked_target(FileType),
        globals.lookup_bool_option(NoLinkObjsGlobals, use_grade_subdirs,
            UseGradeSubdirs),
        (
            UseGradeSubdirs = yes,
            post_link_make_symlink_or_copy(NoLinkObjsGlobals, ErrorStream,
                FileType, MainModuleName, Succeeded, MadeSymlinkOrCopy, !IO),
            (
                MadeSymlinkOrCopy = yes,
                maybe_symlink_or_copy_linked_target_message(NoLinkObjsGlobals,
                    MsgTarget, !IO)
            ;
                MadeSymlinkOrCopy = no,
                maybe_warn_up_to_date_target(NoLinkObjsGlobals, MsgTarget,
                    !Info, !IO)
            )
        ;
            UseGradeSubdirs = no,
            maybe_warn_up_to_date_target(NoLinkObjsGlobals, MsgTarget,
                !Info, !IO),
            Succeeded = yes
        )
    ;
        DepsResult = deps_out_of_date,
        maybe_make_linked_target_message(NoLinkObjsGlobals, OutputFileName,
            !IO),

        % Find the extra object files for externally compiled foreign
        % procedures and fact tables. We don't need to include these in the
        % timestamp checking above -- they will have been checked when the
        % module's object file was built.
        list.map_foldl2(
            ( pred(ModuleName::in, ForeignFiles::out,
                    MakeInfo0::in, MakeInfo::out, !.IO::di, !:IO::uo) is det :-
                get_module_dependencies(Globals, ModuleName, MaybeImports,
                    MakeInfo0, MakeInfo, !IO),
                (
                    MaybeImports = yes(Imports),
                    external_foreign_code_files(Globals, PIC, Imports,
                        ForeignFiles, !IO)
                ;
                    MaybeImports = no,
                    % This error should have been detected earlier.
                    unexpected($module, $pred, "error in dependencies")
                )
            ), AllModulesList, ExtraForeignFiles, !Info, !IO),
        ForeignObjects = list.map(
            (func(foreign_code_file(_, _, ObjFile)) = ObjFile),
            list.condense(ExtraForeignFiles)),

        (
            CompilationTarget = target_c,
            maybe_pic_object_file_extension(NoLinkObjsGlobals, PIC,
                ObjExtToUse)
        ;
            CompilationTarget = target_csharp,
            % There is no separate object code step.
            ObjExtToUse = ".cs"
        ;
            CompilationTarget = target_java,
            globals.lookup_string_option(NoLinkObjsGlobals,
                java_object_file_extension, ObjExtToUse)
        ;
            CompilationTarget = target_erlang,
            globals.lookup_string_option(NoLinkObjsGlobals,
                erlang_object_file_extension, ObjExtToUse)
        ),
        list.map_foldl(
            module_name_to_file_name(NoLinkObjsGlobals, do_not_create_dirs,
                ObjExtToUse),
            ObjModules, ObjList, !IO),

        % LinkObjects may contain `.a' files which must come
        % after all the object files on the linker command line.
        AllObjects = InitObjects ++ ObjList ++ ForeignObjects ++ LinkObjects,
        (
            ( CompilationTarget = target_c
            ; CompilationTarget = target_erlang
            ; CompilationTarget = target_java
            ; CompilationTarget = target_csharp
            ),
            % Run the link in a separate process so it can be killed
            % if an interrupt is received.
            call_in_forked_process(
                compile_target_code.link(NoLinkObjsGlobals, ErrorStream,
                    FileType, MainModuleName, AllObjects),
                Succeeded, !IO)
        ),
        CmdLineTargets0 = !.Info ^ command_line_targets,
        set.delete(MainModuleName - linked_target(FileType),
            CmdLineTargets0, CmdLineTargets),
        !Info ^ command_line_targets := CmdLineTargets,
        (
            Succeeded = yes,
            FileTimestamps0 = !.Info ^ file_timestamps,
            map.delete(OutputFileName, FileTimestamps0, FileTimestamps),
            !Info ^ file_timestamps := FileTimestamps
        ;
            Succeeded = no,
            file_error(!.Info, OutputFileName, !IO)
        )
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
        ; FileType = erlang_launcher
        ; FileType = erlang_archive
        )
    ).

%-----------------------------------------------------------------------------%

    % When compiling to Java we want to invoke `javac' just once, passing it a
    % list of all out-of-date `.java' files. This is a lot quicker than
    % compiling each Java file individually.
    %
:- pred make_java_files(globals::in, module_name::in, list(module_name)::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

make_java_files(Globals, MainModuleName, ObjModules, Succeeded, !Info, !IO) :-
    out_of_date_java_modules(Globals, ObjModules, OutOfDateModules,
        !Info, !IO),
    (
        OutOfDateModules = [],
        Succeeded = yes
    ;
        OutOfDateModules = [_ | _],
        build_java_files(Globals, MainModuleName, OutOfDateModules, Succeeded,
            !Info, !IO),
        % javac might write more `.class' files than we anticipated (though
        % it probably won't) so clear out all the timestamps which might be
        % affected.
        Timestamps0 = !.Info ^ file_timestamps,
        map.foldl(delete_java_class_timestamps, Timestamps0,
            map.init, Timestamps),
        !Info ^ file_timestamps := Timestamps
    ).

:- pred out_of_date_java_modules(globals::in, list(module_name)::in,
    list(module_name)::out, make_info::in, make_info::out, io::di, io::uo)
    is det.

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
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

build_java_files(Globals, MainModuleName, ModuleNames, Succeeded,
        !Info, !IO) :-
    verbose_make_msg(Globals,
        io.write_string("Making Java class files\n"), !IO),
    list.map_foldl(
        module_name_to_file_name(Globals, do_create_dirs, ".java"),
        ModuleNames, JavaFiles, !IO),
    % We redirect errors to a file named after the main module.
    build_with_output_redirect(Globals, MainModuleName,
        build_java_files_2(JavaFiles), Succeeded, !Info, !IO).

:- pred build_java_files_2(list(string)::in, globals::in, io.output_stream::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

build_java_files_2(JavaFiles, Globals, ErrorStream, Succeeded, !Info, !IO) :-
    call_in_forked_process(
        compile_java_files(Globals, ErrorStream, JavaFiles),
        Succeeded, !IO).

:- pred delete_java_class_timestamps(string::in, maybe_error(timestamp)::in,
    file_timestamps::in, file_timestamps::out) is det.

delete_java_class_timestamps(FileName, MaybeTimestamp, !Timestamps) :-
    ( if string.suffix(FileName, ".class") then
        true
    else
        map.det_insert(FileName, MaybeTimestamp, !Timestamps)
    ).

%-----------------------------------------------------------------------------%

make_misc_target(Globals, MainModuleName - TargetType, Succeeded, !Info,
        !IO) :-
    build_with_module_options(Globals, MainModuleName, [],
        make_misc_target_builder(MainModuleName - TargetType),
        Succeeded, !Info, !IO).

:- pred make_misc_target_builder(pair(module_name, misc_target_type)::in,
    globals::in, list(string)::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_misc_target_builder(MainModuleName - TargetType, Globals, _, Succeeded,
        !Info, !IO) :-
    % Don't rebuild .module_dep files when cleaning up.
    RebuildModuleDeps = !.Info ^ rebuild_module_deps,
    ( if
        ( TargetType = misc_target_clean
        ; TargetType = misc_target_realclean
        )
    then
        !Info ^ rebuild_module_deps := do_not_rebuild_module_deps
    else
        true
    ),
    find_reachable_local_modules(Globals, MainModuleName, Succeeded0,
        AllModulesSet, !Info, !IO),
    !Info ^ rebuild_module_deps := RebuildModuleDeps,
    AllModules = set.to_sorted_list(AllModulesSet),
    (
        TargetType = misc_target_clean,
        Succeeded = yes,
        list.foldl2(make_module_clean(Globals), AllModules, !Info, !IO),
        remove_init_files(Globals, very_verbose, MainModuleName, !Info, !IO)
    ;
        TargetType = misc_target_realclean,
        Succeeded = yes,
        make_main_module_realclean(Globals, MainModuleName, !Info, !IO),
        list.foldl2(make_module_realclean(Globals), AllModules, !Info, !IO)
    ;
        TargetType = misc_target_build_all(ModuleTargetType),
        get_target_modules(Globals, ModuleTargetType, AllModules,
            TargetModules, !Info, !IO),
        globals.lookup_bool_option(Globals, keep_going, KeepGoing),
        ( if Succeeded0 = no, KeepGoing = no then
            Succeeded = no
        else
            % Ensure all interface files are present before continuing.
            % This prevents a problem when two parallel branches
            % try to generate the same missing interface file later.
            make_all_interface_files(Globals, AllModules, Succeeded1,
                !Info, !IO),
            ( if Succeeded1 = no, KeepGoing = no then
                Succeeded = no
            else
                maybe_with_analysis_cache_dir(Globals,
                    foldl2_maybe_stop_at_error_maybe_parallel(KeepGoing,
                        make_module_target, Globals,
                        make_dependency_list(TargetModules, ModuleTargetType)),
                    Succeeded2, !Info, !IO),
                Succeeded = Succeeded0 `and` Succeeded1 `and` Succeeded2
            )
        )
    ;
        TargetType = misc_target_build_analyses,
        maybe_with_analysis_cache_dir(Globals,
            build_analysis_files(Globals, MainModuleName, AllModules,
                Succeeded0),
            Succeeded, !Info, !IO)
    ;
        TargetType = misc_target_build_library,
        make_all_interface_files(Globals, AllModules, IntSucceeded,
            !Info, !IO),
        (
            IntSucceeded = yes,
            maybe_with_analysis_cache_dir(Globals,
                build_library(MainModuleName, AllModules, Globals),
                Succeeded, !Info, !IO)
        ;
            IntSucceeded = no,
            Succeeded = no
        )
    ;
        TargetType = misc_target_install_library,
        make_misc_target(Globals, MainModuleName - misc_target_build_library,
            LibSucceeded, !Info, !IO),
        (
            LibSucceeded = yes,
            install_library(Globals, MainModuleName, Succeeded, !Info, !IO)
        ;
            LibSucceeded = no,
            Succeeded = no
        )
    ;
        TargetType = misc_target_build_xml_docs,
        get_target_modules(Globals, module_target_xml_doc, AllModules,
            TargetModules, !Info, !IO),
        globals.lookup_bool_option(Globals, keep_going, KeepGoing),
        ( if Succeeded0 = no, KeepGoing = no then
            Succeeded = no
        else
            foldl2_maybe_stop_at_error(KeepGoing, make_module_target,
                Globals,
                make_dependency_list(TargetModules, module_target_xml_doc),
                Succeeded1, !Info, !IO),
            Succeeded = Succeeded0 `and` Succeeded1
        )
    ).

%-----------------------------------------------------------------------------%

:- pred make_all_interface_files(globals::in, list(module_name)::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_all_interface_files(Globals, AllModules0, Succeeded, !Info, !IO) :-
    remove_nested_modules(Globals, AllModules0, NonnestedModules, !Info, !IO),
    list.foldl3(collect_modules_with_children(Globals), NonnestedModules,
        [], ParentModules, !Info, !IO),
    ShortInts = make_dependency_list(NonnestedModules,
        module_target_unqualified_short_interface),
    PrivateInts = make_dependency_list(ParentModules,
        module_target_private_interface),
    LongInts = make_dependency_list(NonnestedModules,
        module_target_long_interface),
    globals.get_any_intermod(Globals, AnyIntermod),
    (
        AnyIntermod = yes,
        OptFiles = make_dependency_list(NonnestedModules,
            module_target_intermodule_interface)
    ;
        AnyIntermod = no,
        OptFiles = []
    ),
    globals.lookup_bool_option(Globals, keep_going, KeepGoing),
    % Private interfaces (.int0) need to be made before building long interface
    % files in parallel, otherwise two processes may try to build the same
    % private interface file.
    foldl2_maybe_stop_at_error(KeepGoing,
        foldl2_maybe_stop_at_error(KeepGoing, make_module_target),
        Globals, [ShortInts, PrivateInts], Succeeded0, !Info, !IO),
    (
        Succeeded0 = yes,
        foldl2_maybe_stop_at_error(KeepGoing,
            foldl2_maybe_stop_at_error_maybe_parallel(KeepGoing,
                make_module_target),
            Globals, [LongInts, OptFiles], Succeeded, !Info, !IO)
    ;
        Succeeded0 = no,
        Succeeded = no
    ).

:- pred collect_modules_with_children(globals::in, module_name::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

collect_modules_with_children(Globals, ModuleName, !ParentModules,
        !Info, !IO) :-
    get_module_dependencies(Globals, ModuleName, MaybeImports, !Info, !IO),
    (
        MaybeImports = yes(Imports),
        Children = Imports ^ mai_children,
        ( if multi_map.is_empty(Children) then
            true
        else
            !:ParentModules = [ModuleName | !.ParentModules]
        )
    ;
        MaybeImports = no
    ).

%-----------------------------------------------------------------------------%

    % If `--analysis-file-cache' is enabled, create a temporary directory for
    % holding analysis cache files and pass that to child processes.
    % After P is finished, remove the cache directory completely.
    %
:- pred maybe_with_analysis_cache_dir(globals::in,
    build0(make_info)::in(build0), bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

maybe_with_analysis_cache_dir(Globals, P, Succeeded, !Info, !IO) :-
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
            list.member(CacheDirOption, !.Info ^ option_args)
        )
    then
        P(Succeeded, !Info, !IO)
    else
        create_analysis_cache_dir(Globals, Succeeded0, CacheDir, !IO),
        (
            Succeeded0 = yes,
            OrigOptionArgs = !.Info ^ option_args,
            % Pass the name of the cache directory to child processes
            !Info ^ option_args :=
                OrigOptionArgs ++ [CacheDirOption, CacheDir],
            globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
            build_with_check_for_interrupt(VeryVerbose, P,
                remove_cache_dir(Globals, CacheDir), Succeeded, !Info, !IO),
            remove_cache_dir(Globals, CacheDir, !Info, !IO),
            !Info ^ option_args := OrigOptionArgs
        ;
            Succeeded0 = no,
            Succeeded = no
        )
    ).

:- pred create_analysis_cache_dir(globals::in, bool::out, string::out,
    io::di, io::uo) is det.

create_analysis_cache_dir(Globals, Succeeded, CacheDir, !IO) :-
    choose_cache_dir_name(Globals, CacheDir, !IO),
    verbose_make_msg_option(Globals, verbose_make,
        io.format("Creating %s\n", [s(CacheDir)]), !IO),
    dir.make_directory(CacheDir, MakeRes, !IO),
    (
        MakeRes = ok,
        Succeeded = yes
    ;
        MakeRes = error(Error),
        io.write_string("Error: making directory ", !IO),
        io.write_string(CacheDir, !IO),
        io.write_string(": ", !IO),
        io.write_string(io.error_message(Error), !IO),
        io.nl(!IO),
        Succeeded = no
    ).

:- pred choose_cache_dir_name(globals::in, string::out, io::di, io::uo) is det.

choose_cache_dir_name(Globals, DirName, !IO) :-
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
    io.remove_file_recursively(CacheDir, _, !IO).

%-----------------------------------------------------------------------------%

:- pred build_analysis_files(globals::in, module_name::in,
    list(module_name)::in, bool::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

build_analysis_files(Globals, MainModuleName, AllModules,
        Succeeded0, Succeeded, !Info, !IO) :-
    globals.lookup_bool_option(Globals, keep_going, KeepGoing),
    ( if
        Succeeded0 = no,
        KeepGoing = no
    then
        Succeeded = no
    else
        % Ensure all interface files are present before continuing.
        % This prevents a problem when two parallel branches try to generate
        % the same missing interface file later.
        % (Although we can't actually build analysis files in parallel yet.)
        make_all_interface_files(Globals, AllModules, Succeeded1, !Info, !IO),
        ( if
            Succeeded1 = no,
            KeepGoing = no
        then
            Succeeded = no
        else
            build_analysis_files_1(Globals, MainModuleName, AllModules,
                Succeeded, !Info, !IO)
        )
    ).

:- pred build_analysis_files_1(globals::in, module_name::in,
    list(module_name)::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

build_analysis_files_1(Globals, MainModuleName, AllModules, Succeeded,
        !Info, !IO) :-
    get_target_modules(Globals, module_target_analysis_registry, AllModules,
        TargetModules0, !Info, !IO),
    reverse_ordered_modules(!.Info ^ module_dependencies,
        TargetModules0, TargetModules1),
    % Filter out the non-local modules so we don't try to reanalyse them.
    list.filter((pred(Mod::in) is semidet :- list.member(Mod, AllModules)),
        TargetModules1, TargetModules),
    make_local_module_id_options(Globals, MainModuleName, Succeeded0,
        LocalModulesOpts, !Info, !IO),
    (
        Succeeded0 = yes,
        build_analysis_files_2(Globals, MainModuleName, TargetModules,
            LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
    ;
        Succeeded0 = no,
        Succeeded = no
    ).

:- pred build_analysis_files_2(globals::in, module_name::in,
    list(module_name)::in, list(string)::in, bool::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_analysis_files_2(Globals, MainModuleName, TargetModules,
        LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO) :-
    globals.lookup_bool_option(Globals, keep_going, KeepGoing),
    foldl2_maybe_stop_at_error(KeepGoing,
        make_module_target_extra_options(LocalModulesOpts), Globals,
        make_dependency_list(TargetModules, module_target_analysis_registry),
        Succeeded1, !Info, !IO),
    % Maybe we should have an option to reanalyse cliques before moving
    % upwards in the dependency graph?

    % Find which module analysis files are suboptimal or invalid.
    % If there are any invalid files then we repeat the analysis pass.
    % If there are only suboptimal files then we repeat the analysis up
    % to the number of times given by the user.
    ReanalysisPasses = !.Info ^ reanalysis_passes,
    ReanalyseSuboptimal = (if ReanalysisPasses > 1 then yes else no),
    modules_needing_reanalysis(ReanalyseSuboptimal, Globals, TargetModules,
        InvalidModules, SuboptimalModules, !IO),
    ( if list.is_not_empty(InvalidModules) then
        maybe_reanalyse_modules_message(Globals, !IO),
        list.foldl(reset_analysis_registry_dependency_status,
            InvalidModules, !Info),
        build_analysis_files_2(Globals, MainModuleName, TargetModules,
            LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
    else if list.is_not_empty(SuboptimalModules) then
        list.foldl(reset_analysis_registry_dependency_status,
            SuboptimalModules, !Info),
        !Info ^ reanalysis_passes := ReanalysisPasses - 1,
        maybe_reanalyse_modules_message(Globals, !IO),
        build_analysis_files_2(Globals, MainModuleName, TargetModules,
            LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
    else
        Succeeded = Succeeded0 `and` Succeeded1
    ).

%-----------------------------------------------------------------------------%

    % Return a list of modules in reverse order of their dependencies, i.e.
    % the list is the module dependency graph from bottom-up. Mutually
    % dependent modules (modules which form a clique in the dependency graph)
    % are returned adjacent in the list in arbitrary order.
    %
:- pred reverse_ordered_modules(
    map(module_name, maybe(module_and_imports))::in,
    list(module_name)::in, list(module_name)::out) is det.

reverse_ordered_modules(ModuleDeps, Modules0, Modules) :-
    list.foldl2(add_module_relations(
        lookup_module_and_imports_in_maybe_map(ModuleDeps)),
        Modules0, digraph.init, _IntDepsGraph, digraph.init, ImplDepsGraph),
    digraph.atsort(ImplDepsGraph, Order0),
    list.reverse(Order0, Order1),
    list.map(set.to_sorted_list, Order1, Order2),
    list.condense(Order2, Modules).

    % add_module_relations(LookupModuleImports, ModuleName,
    %   !IntDepsRel, !ImplDepsRel)
    %
    % Add a module's interface and implementation dependencies to IntDepsRel
    % and ImplDepsRel respectively. Dependencies are found using the
    % LookupModuleImports function.
    %
:- pred add_module_relations(
    lookup_module_and_imports::lookup_module_and_imports,
    module_name::in, digraph(module_name)::in, digraph(module_name)::out,
    digraph(module_name)::in, digraph(module_name)::out) is det.

add_module_relations(LookupModuleImports, ModuleName,
        !IntDepsGraph, !ImplDepsGraph) :-
    ModuleImports = LookupModuleImports(ModuleName),
    add_module_and_imports_to_deps_graph(ModuleImports, LookupModuleImports,
        !IntDepsGraph, !ImplDepsGraph).

%-----------------------------------------------------------------------------%

:- func lookup_module_and_imports_in_maybe_map(
    map(module_name, maybe(module_and_imports)), module_name)
    = module_and_imports.

lookup_module_and_imports_in_maybe_map(ModuleDeps, ModuleName)
        = ModuleImports :-
    map.lookup(ModuleDeps, ModuleName, MaybeModuleImports),
    (
        MaybeModuleImports = yes(ModuleImports)
    ;
        MaybeModuleImports = no,
        unexpected($module, $pred, "MaybeModuleImports = no")
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
    !Info ^ dependency_status ^ elem(Dep) := deps_status_not_considered.

%-----------------------------------------------------------------------------%

:- pred build_library(module_name::in, list(module_name)::in, globals::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

build_library(MainModuleName, AllModules, Globals, Succeeded, !Info, !IO) :-
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        build_c_library(Globals, MainModuleName, AllModules, Succeeded,
            !Info, !IO)
    ;
        Target = target_csharp,
        build_csharp_library(Globals, MainModuleName, Succeeded, !Info, !IO)
    ;
        Target = target_java,
        build_java_library(Globals, MainModuleName, Succeeded, !Info, !IO)
    ;
        Target = target_erlang,
        build_erlang_library(Globals, MainModuleName, AllModules, Succeeded,
            !Info, !IO)
    ).

:- pred build_c_library(globals::in, module_name::in, list(module_name)::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

build_c_library(Globals, MainModuleName, AllModules, Succeeded, !Info, !IO) :-
    make_linked_target(Globals,
        linked_target_file(MainModuleName, static_library),
        StaticSucceeded, !Info, !IO),
    shared_libraries_supported(Globals, SharedLibsSupported),
    (
        StaticSucceeded = yes,
        (
            SharedLibsSupported = yes,
            make_linked_target(Globals,
                linked_target_file(MainModuleName, shared_library),
                SharedLibsSucceeded, !Info, !IO)
        ;
            SharedLibsSupported = no,
            SharedLibsSucceeded = yes
        ),
        % We can only build the .init file if we have succesfully built
        % the .c files.
        (
            SharedLibsSucceeded = yes,
            % Errors while making the .init file should be very rare.
            io.output_stream(ErrorStream, !IO),
            make_library_init_file(Globals, ErrorStream, MainModuleName,
                AllModules, Succeeded, !IO)
        ;
            SharedLibsSucceeded = no,
            Succeeded = no
        )
    ;
        StaticSucceeded = no,
        Succeeded = no
    ).

:- pred build_csharp_library(globals::in, module_name::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_csharp_library(Globals, MainModuleName, Succeeded, !Info, !IO) :-
    make_linked_target(Globals,
        linked_target_file(MainModuleName, csharp_library),
        Succeeded, !Info, !IO).

:- pred build_java_library(globals::in, module_name::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_java_library(Globals, MainModuleName, Succeeded, !Info, !IO) :-
    make_linked_target(Globals,
        linked_target_file(MainModuleName, java_archive),
        Succeeded, !Info, !IO).

:- pred build_erlang_library(globals::in, module_name::in,
    list(module_name)::in, bool::out, make_info::in,
    make_info::out, io::di, io::uo) is det.

build_erlang_library(Globals, MainModuleName, AllModules, Succeeded,
        !Info, !IO) :-
    make_linked_target(Globals,
        linked_target_file(MainModuleName, erlang_archive),
        Succeeded0, !Info, !IO),
    (
        Succeeded0 = yes,
        % Errors while making the .init file should be very rare.
        io.output_stream(ErrorStream, !IO),
        make_erlang_library_init_file(Globals, ErrorStream, MainModuleName,
            AllModules, Succeeded, !IO)
    ;
        Succeeded0 = no,
        Succeeded = no
    ).

%-----------------------------------------------------------------------------%

:- pred install_library(globals::in, module_name::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_library(Globals, MainModuleName, Succeeded, !Info, !IO) :-
    find_reachable_local_modules(Globals, MainModuleName, DepsSuccess,
        AllModules0, !Info, !IO),
    AllModules = set.to_sorted_list(AllModules0),
    make_install_dirs(Globals, DirSucceeded, LinkSucceeded, !IO),
    ( if
        DepsSuccess = yes,
        DirSucceeded = yes
    then
        list.map_foldl2(install_ints_and_headers(Globals, LinkSucceeded),
            AllModules, IntsSucceeded, !Info, !IO),
        install_extra_headers(Globals, ExtraHdrsSucceeded, !IO),

        grade_directory_component(Globals, Grade),
        install_library_grade_files(Globals, LinkSucceeded, Grade,
            MainModuleName, AllModules, GradeSucceeded, !Info, !IO),
        ( if
            bool.and_list([ExtraHdrsSucceeded | IntsSucceeded]) = yes,
            GradeSucceeded = yes
        then
            % XXX With Mmake, LIBGRADES is target-specific.
            globals.lookup_accumulating_option(Globals, libgrades, LibGrades0),
            globals.lookup_bool_option(Globals, keep_going, KeepGoing),
            LibGrades = list.delete_all(LibGrades0, Grade),
            foldl2_maybe_stop_at_error(KeepGoing,
                install_library_grade(LinkSucceeded,
                    MainModuleName, AllModules),
                Globals, LibGrades, Succeeded, !Info, !IO)
        else
            Succeeded = no
        )
    else
        Succeeded = no
    ).

:- pred install_ints_and_headers(globals::in, bool::in, module_name::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

install_ints_and_headers(Globals, SubdirLinkSucceeded, ModuleName, Succeeded,
        !Info, !IO) :-
    get_module_dependencies(Globals, ModuleName, MaybeModuleAndImports,
        !Info, !IO),
    (
        MaybeModuleAndImports = yes(ModuleAndImports),
        globals.get_any_intermod(Globals, AnyIntermod),
        (
            AnyIntermod = yes,
            % `.int0' files are imported by `.opt' files.
            ( if multi_map.is_empty(ModuleAndImports ^ mai_children) then
                Exts = ["opt"]
            else
                Exts = ["int0", "opt"]
            )
        ;
            AnyIntermod = no,
            Exts = []
        ),

        globals.lookup_string_option(Globals, install_prefix, Prefix),
        LibDir = Prefix/"lib"/"mercury",
        list.map_foldl(
            install_subdir_file(Globals, SubdirLinkSucceeded, LibDir/"ints",
                ModuleName),
            ["int", "int2", "int3", "module_dep" | Exts],
            Results, !IO),

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
            % ModuleAndImports ^ contains_foreign_export
            %   = contains_foreign_export?
            module_name_to_file_name(Globals, do_not_create_dirs, ".mh",
                ModuleName, FileName, !IO),
            install_file(Globals, FileName, LibDir/"inc", HeaderSucceeded1,
                !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            install_subdir_file(Globals, SubdirLinkSucceeded, LibDir/"ints",
                ModuleName, "mh", HeaderSucceeded2, !IO),

            HeaderSucceeded = HeaderSucceeded1 `and` HeaderSucceeded2
        ;
            Target = target_erlang,
            module_name_to_file_name(Globals, do_not_create_dirs, ".hrl",
                ModuleName, FileName, !IO),
            install_file(Globals, FileName, LibDir/"inc", HeaderSucceeded, !IO)
        ;
            ( Target = target_java
            ; Target = target_csharp
            ),
            HeaderSucceeded = yes
        ),
        Succeeded = bool.and_list([HeaderSucceeded | Results])
    ;
        MaybeModuleAndImports = no,
        Succeeded = no
    ).

:- pred install_extra_headers(globals::in, bool::out, io::di, io::uo) is det.

install_extra_headers(Globals, ExtraHdrsSucceeded, !IO) :-
    globals.lookup_accumulating_option(Globals, extra_library_header,
        ExtraHdrs),
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    IncDir = Prefix / "lib" / "mercury" / "inc",
    list.foldl2(install_extra_header(Globals, IncDir), ExtraHdrs,
        yes, ExtraHdrsSucceeded, !IO).

:- pred install_extra_header(globals::in, dir_name::in, string::in,
    bool::in, bool::out, io::di, io::uo) is det.

install_extra_header(Globals, IncDir, FileName, !Succeeded, !IO) :-
    install_file(Globals, FileName, IncDir, InstallSucceeded, !IO),
    !:Succeeded = bool.and(InstallSucceeded, !.Succeeded).

:- pred install_library_grade(bool::in, module_name::in, list(module_name)::in,
    globals::in, string::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

install_library_grade(LinkSucceeded0, ModuleName, AllModules, Globals, Grade,
        Succeeded, !Info, !IO) :-
    % Only remove grade-dependent files after installing if
    % --use-grade-subdirs is not specified by the user.
    globals.lookup_bool_option(Globals, use_grade_subdirs, UseGradeSubdirs),
    CleanAfter = not(UseGradeSubdirs),

    % Set up so that grade-dependent files for the current grade
    % don't overwrite the files for the default grade.
    OptionArgs0 = !.Info ^ option_args,
    OptionArgs = OptionArgs0 ++ ["--grade", Grade, "--use-grade-subdirs"],

    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Installing grade ", !IO),
            io.write_string(Grade, !IO),
            io.nl(!IO)
        ), !IO),

    lookup_mmc_options(Globals, !.Info ^ options_variables, MaybeMCFlags, !IO),
    (
        MaybeMCFlags = yes(MCFlags),
        DetectedGradeFlags = !.Info ^ detected_grade_flags,
        AllFlags = DetectedGradeFlags ++ MCFlags ++ OptionArgs,
        handle_given_options(AllFlags, _, _, OptionsSpecs, LibGlobals, !IO)
    ;
        MaybeMCFlags = no,
        % Errors should have been caught before.
        unexpected($module, $pred, "bad DEFAULT_MCFLAGS")
    ),

    (
        OptionsSpecs = [_ | _],
        usage_errors(Globals, OptionsSpecs, !IO),
        Succeeded = no
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

        !Info ^ dependency_status := StatusMap,
        !Info ^ option_args := OptionArgs,

        % Building the library in the new grade is done in a separate process
        % to make it easier to stop and clean up on an interrupt.
        Cleanup = maybe_make_grade_clean(LibGlobals, CleanAfter, ModuleName,
            AllModules),
        globals.lookup_bool_option(LibGlobals, very_verbose, VeryVerbose),
        build_with_check_for_interrupt(VeryVerbose,
            ( pred(GradeSuccess::out, MInfo::in, MInfo::out,
                    !.IO::di, !:IO::uo) is det :-
                call_in_forked_process(
                    ( pred(GradeSuccess0::out, !.IO::di, !:IO::uo) is det :-
                        install_library_grade_2(LibGlobals, LinkSucceeded0,
                            ModuleName, AllModules, MInfo, CleanAfter,
                            GradeSuccess0, !IO)
                    ), GradeSuccess, !IO)
            ), Cleanup, Succeeded, !Info, !IO)
    ).

:- func remove_grade_dependent_targets(dependency_file, dependency_status,
    version_hash_table(dependency_file, dependency_status)) =
    version_hash_table(dependency_file, dependency_status).
% See the comment above for the reason why we don't use this predicate.
:- pragma consider_used(remove_grade_dependent_targets/3).

remove_grade_dependent_targets(File, _Status, StatusMap0) = StatusMap :-
    ( if
        File = dep_target(target_file(_, Target)),
        target_is_grade_or_arch_dependent(Target)
    then
        StatusMap = delete(StatusMap0, File)
    else
        StatusMap = StatusMap0
    ).

:- pred install_library_grade_2(globals::in, bool::in, module_name::in,
    list(module_name)::in, make_info::in, bool::in, bool::out,
    io::di, io::uo) is det.

install_library_grade_2(Globals, LinkSucceeded0, ModuleName, AllModules,
        Info0, CleanAfter, Succeeded, !IO) :-
    make_misc_target(Globals, ModuleName - misc_target_build_library,
        LibSucceeded, Info0, Info1, !IO),
    (
        LibSucceeded = yes,
        % `GradeDir' differs from `Grade' in that it is in canonical form.
        grade_directory_component(Globals, GradeDir),
        install_library_grade_files(Globals, LinkSucceeded0, GradeDir,
            ModuleName, AllModules, Succeeded, Info1, Info2, !IO),
        maybe_make_grade_clean(Globals, CleanAfter, ModuleName, AllModules,
            Info2, _Info, !IO)
    ;
        LibSucceeded = no,
        Succeeded = no
    ).

    % Install the `.a', `.so', `.jar', `.beams', `.opt' and `.mih' files for
    % the current grade.
    %
    % NOTE: changes here may require changes to
    %       file_util.get_install_name_option/4.
    %
:- pred install_library_grade_files(globals::in, bool::in, string::in,
    module_name::in, list(module_name)::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_library_grade_files(Globals, LinkSucceeded0, GradeDir, ModuleName,
        AllModules, Succeeded, !Info, !IO) :-
    make_grade_install_dirs(Globals, GradeDir, DirResult, LinkSucceeded1, !IO),
    LinkSucceeded = LinkSucceeded0 `and` LinkSucceeded1,
    (
        DirResult = yes,
        linked_target_file_name(Globals, ModuleName, static_library,
            LibFileName, !IO),
        linked_target_file_name(Globals, ModuleName, shared_library,
            SharedLibFileName, !IO),
        linked_target_file_name(Globals, ModuleName, csharp_library,
            DllFileName, !IO),
        linked_target_file_name(Globals, ModuleName, java_archive,
            JarFileName, !IO),
        linked_target_file_name(Globals, ModuleName, erlang_archive,
            ErlangArchiveFileName, !IO),

        globals.lookup_string_option(Globals, install_prefix, Prefix),

        ( if string.prefix(GradeDir, "csharp") then
            GradeLibDir = Prefix/"lib"/"mercury"/"lib"/GradeDir,
            install_file(Globals, DllFileName, GradeLibDir, LibsSucceeded,
                !IO),
            InitSucceeded = yes
        else if string.prefix(GradeDir, "java") then
            GradeLibDir = Prefix/"lib"/"mercury"/"lib"/GradeDir,
            install_file(Globals, JarFileName, GradeLibDir, LibsSucceeded,
                !IO),
            InitSucceeded = yes
        else if string.prefix(GradeDir, "erlang") then
            GradeLibDir = Prefix/"lib"/"mercury"/"lib"/GradeDir,
            % Our "Erlang archives" are actually directories.
            install_directory(Globals, ErlangArchiveFileName, GradeLibDir,
                LibsSucceeded, !IO),
            install_grade_init(Globals, GradeDir, ModuleName, InitSucceeded,
                !IO)
        else
            GradeLibDir = Prefix/"lib"/"mercury"/"lib"/GradeDir,
            maybe_install_library_file(Globals, "static", LibFileName,
                GradeLibDir, LibSuccess, !IO),
            ( if LibFileName = SharedLibFileName then
                LibsSucceeded = LibSuccess
            else
                maybe_install_library_file(Globals, "shared",
                    SharedLibFileName, GradeLibDir, SharedLibSuccess, !IO),
                LibsSucceeded = LibSuccess `and` SharedLibSuccess
            ),
            install_grade_init(Globals, GradeDir, ModuleName, InitSucceeded,
                !IO)
        ),

        list.map_foldl2(
            install_grade_ints_and_headers(Globals, LinkSucceeded, GradeDir),
            AllModules, IntsHeadersSucceeded, !Info, !IO),
        Succeeded = bool.and_list(
            [LibsSucceeded, InitSucceeded | IntsHeadersSucceeded])
    ;
        DirResult = no,
        Succeeded = no
    ).

    % Install the `.init' file for the current grade.
    %
:- pred install_grade_init(globals::in, string::in, module_name::in, bool::out,
    io::di, io::uo) is det.

install_grade_init(Globals, GradeDir, ModuleName, Succeeded, !IO) :-
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    GradeModulesDir = Prefix / "lib" / "mercury" / "modules" / GradeDir,
    module_name_to_file_name(Globals, do_not_create_dirs, ".init",
        ModuleName, InitFileName, !IO),
    install_file(Globals, InitFileName, GradeModulesDir, Succeeded, !IO).

    % Install the `.opt', `.analysis' and `.mih' files for the current grade.
    %
:- pred install_grade_ints_and_headers(globals::in, bool::in, string::in,
    module_name::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

install_grade_ints_and_headers(Globals, LinkSucceeded, GradeDir, ModuleName,
        Succeeded, !Info, !IO) :-
    get_module_dependencies(Globals, ModuleName, MaybeImports, !Info, !IO),
    (
        MaybeImports = yes(_Imports),
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
                ModuleName, "mih", HeaderSucceeded1, !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            IntDir = LibDir/"ints",
            install_subdir_file(Globals, LinkSucceeded, IntDir, ModuleName,
                "mih", HeaderSucceeded2, !IO),

            HeaderSucceeded = HeaderSucceeded1 `and` HeaderSucceeded2
        else
            HeaderSucceeded = yes
        ),

        GradeIntDir = LibDir/"ints"/GradeDir,
        globals.get_any_intermod(Globals, AnyIntermod),
        (
            AnyIntermod = yes,
            install_subdir_file(Globals, LinkSucceeded, GradeIntDir,
                ModuleName, "opt", OptSucceeded, !IO)
        ;
            AnyIntermod = no,
            OptSucceeded = yes
        ),
        globals.lookup_bool_option(Globals, intermodule_analysis,
            IntermodAnalysis),
        (
            IntermodAnalysis = yes,
            install_subdir_file(Globals, LinkSucceeded, GradeIntDir,
                ModuleName, "analysis", IntermodAnalysisSucceeded, !IO)
        ;
            IntermodAnalysis = no,
            IntermodAnalysisSucceeded = yes
        ),

        Succeeded = HeaderSucceeded `and` OptSucceeded `and`
            IntermodAnalysisSucceeded
    ;
        MaybeImports = no,
        Succeeded = no
    ).

    % Install a file in the given directory, and in directory/Mercury/exts
    % if the symlinks for the subdirectories couldn't be created
    % (e.g. on Windows).
    %
:- pred install_subdir_file(globals::in, bool::in, dir_name::in,
    module_name::in, string::in, bool::out, io::di, io::uo) is det.

install_subdir_file(Globals, SubdirLinkSucceeded, InstallDir, ModuleName, Ext,
        Succeeded, !IO) :-
    module_name_to_file_name(Globals, do_not_create_dirs, "." ++ Ext,
        ModuleName, FileName, !IO),
    install_file(Globals, FileName, InstallDir, Succeeded1, !IO),
    (
        SubdirLinkSucceeded = no,
        install_file(Globals, FileName, InstallDir/"Mercury"/(Ext ++ "s"),
            Succeeded2, !IO),
        Succeeded = Succeeded1 `and` Succeeded2
    ;
        SubdirLinkSucceeded = yes,
        Succeeded = Succeeded1
    ).

:- pred maybe_install_library_file(globals::in, string::in, file_name::in,
    dir_name::in, bool::out, io::di, io::uo) is det.

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
            Succeeded0 = yes
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
        Succeeded = yes
    ).

:- pred install_file(globals::in, file_name::in, dir_name::in, bool::out,
    io::di, io::uo) is det.

install_file(Globals, FileName, InstallDir, Succeeded, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Installing file ", !IO),
            io.write_string(FileName, !IO),
            io.write_string(" in ", !IO),
            io.write_string(InstallDir, !IO),
            io.nl(!IO)
        ), !IO),
    Command = make_install_file_command(Globals, FileName, InstallDir),
    io.output_stream(OutputStream, !IO),
    invoke_system_command(Globals, OutputStream, cmd_verbose, Command,
        Succeeded, !IO).

:- pred install_directory(globals::in, dir_name::in, dir_name::in, bool::out,
    io::di, io::uo) is det.

install_directory(Globals, SourceDirName, InstallDir, Succeeded, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Installing directory ", !IO),
            io.write_string(SourceDirName, !IO),
            io.write_string(" in ", !IO),
            io.write_string(InstallDir, !IO),
            io.nl(!IO)
        ), !IO),
    Command = make_install_dir_command(Globals, SourceDirName, InstallDir),
    io.output_stream(OutputStream, !IO),
    invoke_system_command(Globals, OutputStream, cmd_verbose, Command,
        Succeeded, !IO).

:- pred make_install_dirs(globals::in, bool::out, bool::out, io::di, io::uo)
    is det.

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
    LinkResult = bool.and_list(LinkResults),
    (
        LinkResult = yes,
        Results = Results0
    ;
        LinkResult = no,
        list.map_foldl(
            ( pred(Ext::in, MkDirResult::out, !.IO::di, !:IO::uo) is det:-
                make_directory(IntsSubdir/(Ext ++ "s"), MkDirResult, !IO)
            ), Subdirs, MkDirResults, !IO),
        Results = Results0 ++ MkDirResults
    ),
    print_mkdir_errors(Results, Result, !IO).

:- pred make_grade_install_dirs(globals::in, string::in, bool::out, bool::out,
    io::di, io::uo) is det.

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
    LinkResult = bool.and_list([LinkResult0 | LinkResults]),
    (
        LinkResult = yes,
        Results = Results0
    ;
        LinkResult = no,
        make_directory(GradeIncSubdir/"mihs", Result4, !IO),
        make_directory(GradeIntsSubdir/"opts", Result5, !IO),
        make_directory(GradeIntsSubdir/"trans_opts", Result6, !IO),
        make_directory(GradeIntsSubdir/"analysiss", Result7, !IO),
        Results = [Result4, Result5, Result6, Result7 | Results0]
    ),
    print_mkdir_errors(Results, Result, !IO).

:- pred print_mkdir_errors(list(io.res)::in, bool::out,
    io::di, io::uo) is det.

print_mkdir_errors([], yes, !IO).
print_mkdir_errors([ok | Rest], Succeeded, !IO) :-
    print_mkdir_errors(Rest, Succeeded, !IO).
print_mkdir_errors([error(Error) | Rest], no, !IO) :-
    io.write_string("Error creating installation directories: ", !IO),
    io.write_string(io.error_message(Error), !IO),
    io.nl(!IO),
    print_mkdir_errors(Rest, _, !IO).

:- pred make_install_symlink(globals::in, string::in, string::in, bool::out,
    io::di, io::uo) is det.

make_install_symlink(Globals, Subdir, Ext, Succeeded, !IO) :-
    LinkName = Subdir/(Ext ++ "s"),
    maybe_make_symlink(Globals, "..", LinkName, Succeeded, !IO).

    % Generate (or update) the index for an archive file,
    % i.e. run ranlib on a .a file.
    %
:- pred generate_archive_index(globals::in, file_name::in, dir_name::in,
    bool::out, io::di, io::uo) is det.

generate_archive_index(Globals, FileName, InstallDir, Succeeded, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Generating archive index for file ", !IO),
            io.write_string(FileName, !IO),
            io.write_string(" in ", !IO),
            io.write_string(InstallDir, !IO),
            io.nl(!IO)
        ), !IO),
    globals.lookup_string_option(Globals, ranlib_command, RanLibCommand),
    globals.lookup_string_option(Globals, ranlib_flags, RanLibFlags),
    Command = string.join_list("    ", [
        quote_arg(RanLibCommand),
        RanLibFlags,
        quote_arg(InstallDir / FileName)
    ]),
    io.output_stream(OutputStream, !IO),
    invoke_system_command(Globals, OutputStream, cmd_verbose, Command,
        Succeeded, !IO).

%-----------------------------------------------------------------------------%

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
            io.write_string("Cleaning up grade-dependent files for `", !IO),
            write_sym_name(ModuleName, !IO),
            io.write_string("'in grade ", !IO),
            grade_directory_component(Globals, Grade),
            io.write_string(Grade, !IO),
            io.write_string(".\n", !IO)
        ), !IO),

    make_main_module_realclean(Globals, ModuleName, !Info, !IO),
    list.foldl2(make_module_clean(Globals), AllModules, !Info, !IO).

:- pred make_main_module_realclean(globals::in, module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_main_module_realclean(Globals, ModuleName, !Info, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Removing executable and library files for `",
                !IO),
            write_sym_name(ModuleName, !IO),
            io.write_string("'.\n", !IO)
        ), !IO),

    LinkedTargetTypes = [
        executable,
        static_library,
        shared_library,
        csharp_executable,
        csharp_library,
        java_executable,
        java_archive,
        erlang_launcher,
        erlang_archive
    ],
    list.map_foldl(linked_target_file_name(Globals, ModuleName),
        LinkedTargetTypes, FileNames, !IO),
    % Remove the symlinks created for `--use-grade-subdirs'.
    globals.set_option(use_grade_subdirs, bool(no), Globals, NoSubdirGlobals),
    list.map_foldl(linked_target_file_name(NoSubdirGlobals, ModuleName),
        LinkedTargetTypes, ThisDirFileNames, !IO),
    % XXX This symlink should not be necessary anymore for `mmc --make'.
    module_name_to_file_name(NoSubdirGlobals, do_not_create_dirs, ".init",
        ModuleName, ThisDirInitFileName, !IO),

    list.foldl2(make_remove_file(Globals, very_verbose),
        FileNames ++ ThisDirFileNames ++ [ThisDirInitFileName],
        !Info, !IO),
    make_remove_module_file(Globals, very_verbose, ModuleName, ".init",
        !Info, !IO),
    remove_init_files(Globals, very_verbose, ModuleName, !Info, !IO).

:- pred remove_init_files(globals::in, option::in, module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_init_files(Globals, Verbose, ModuleName, !Info, !IO) :-
    globals.lookup_string_option(Globals, object_file_extension, ObjExt),
    globals.lookup_string_option(Globals, pic_object_file_extension,
        PicObjExt),
    globals.lookup_string_option(Globals, erlang_object_file_extension,
        BeamExt),
    list.foldl2(make_remove_module_file(Globals, Verbose, ModuleName),
        ["_init.c", "_init" ++ ObjExt, "_init" ++ PicObjExt,
            "_init.erl", "_init" ++ BeamExt],
        !Info, !IO).

%-----------------------------------------------------------------------------%

:- pred make_module_clean(globals::in, module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_module_clean(Globals, ModuleName, !Info, !IO) :-
    verbose_make_msg_option(Globals, verbose_make,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Cleaning up target files for module `", !IO),
            write_sym_name(ModuleName, !IO),
            io.write_string("'.\n", !IO)
        ), !IO),

    list.foldl2(
        make_remove_target_file_by_name(Globals, very_verbose, ModuleName),
        [module_target_errors,
        module_target_c_code,
        module_target_c_header(header_mih),
        module_target_csharp_code,
        module_target_java_code,
        module_target_java_class_code,
        module_target_erlang_code,
        module_target_erlang_header,
        module_target_erlang_beam_code], !Info, !IO),

    make_remove_module_file(Globals, very_verbose, ModuleName, ".used",
        !Info, !IO),
    make_remove_module_file(Globals, very_verbose, ModuleName, ".prof",
        !Info, !IO),

    get_module_dependencies(Globals, ModuleName, MaybeImports, !Info, !IO),
    (
        MaybeImports = yes(Imports),
        FactTableFiles = Imports ^ mai_fact_table_deps
    ;
        MaybeImports = no,
        FactTableFiles = []
    ),

    list.foldl2(remove_fact_table_c_file(Globals, ModuleName),
        FactTableFiles, !Info, !IO),

    CCodeModule = foreign_language_module_name(ModuleName, lang_c),
    make_remove_target_file_by_name(Globals, very_verbose, CCodeModule,
        module_target_c_code, !Info, !IO),

    remove_object_and_assembler_files(Globals, ModuleName, pic,
        FactTableFiles, !Info, !IO),
    remove_object_and_assembler_files(Globals, ModuleName, non_pic,
        FactTableFiles, !Info, !IO).

:- pred remove_fact_table_c_file(globals::in, module_name::in, string::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_fact_table_c_file(Globals, ModuleName, FactTableFile, !Info, !IO) :-
    fact_table_file_name(Globals, ModuleName, FactTableFile,
        ".c", do_not_create_dirs, FactTableCFile, !IO),
    make_remove_file(Globals, very_verbose, FactTableCFile, !Info, !IO).

:- pred remove_object_and_assembler_files(globals::in, module_name::in,
    pic::in, list(file_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_object_and_assembler_files(Globals, ModuleName, PIC, FactTableFiles,
        !Info, !IO) :-
    make_remove_target_file_by_name(Globals, very_verbose, ModuleName,
        module_target_object_code(PIC), !Info, !IO),
    make_remove_target_file_by_name(Globals, very_verbose, ModuleName,
        module_target_foreign_object(PIC, lang_c), !Info, !IO),
    list.foldl2(
        remove_fact_table_object_and_assembler_files(Globals, ModuleName, PIC),
        FactTableFiles, !Info, !IO).

:- pred remove_fact_table_object_and_assembler_files(globals::in,
    module_name::in, pic::in, file_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_fact_table_object_and_assembler_files(Globals, ModuleName, PIC,
        FactTableFile, !Info, !IO) :-
    make_remove_target_file_by_name(Globals, very_verbose,
        ModuleName, module_target_fact_table_object(PIC, FactTableFile),
        !Info, !IO).

%-----------------------------------------------------------------------------%

:- pred make_module_realclean(globals::in, module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_module_realclean(Globals, ModuleName, !Info, !IO) :-
    make_module_clean(Globals, ModuleName, !Info, !IO),

    verbose_make_msg_option(Globals, verbose_make,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Cleaning up interface files for module `", !IO),
            write_sym_name(ModuleName, !IO),
            io.write_string("'.\n", !IO)
        ), !IO),
    list.foldl2(
        make_remove_target_file_by_name(Globals, very_verbose, ModuleName),
        [module_target_private_interface,
        module_target_long_interface,
        module_target_short_interface,
        module_target_unqualified_short_interface,
        module_target_intermodule_interface,
        module_target_analysis_registry,
        module_target_c_header(header_mh),
        module_target_erlang_header,
        module_target_track_flags], !Info, !IO),
    make_remove_module_file(Globals, very_verbose, ModuleName,
        make_module_dep_file_extension, !Info, !IO),
    make_remove_module_file(Globals, very_verbose, ModuleName,
        ".imdg", !Info, !IO),
    make_remove_module_file(Globals, very_verbose, ModuleName,
        ".request", !Info, !IO).

%-----------------------------------------------------------------------------%
%
% Check that the Mercury libraries required to build a linked target
% are installed in the selected grade.
%

    % Check that all Mercury libraries required by the linked target are
    % installed in the selected grade.
    %
:- pred check_libraries_are_installed(globals::in, bool::out, io::di, io::uo)
    is det.

check_libraries_are_installed(Globals, Succeeded, !IO) :-
    % NOTE: we don't look up the value of the option init_files here because
    % that may include .init files other than those associated with any
    % libraries.
    globals.lookup_accumulating_option(Globals, mercury_libraries, Libs),
    globals.lookup_accumulating_option(Globals, init_file_directories,
        InitFileDirs),
    grade_directory_component(Globals, Grade),
    check_stdlib_is_installed(Globals, Grade, Succeeded0, !IO),
    list.foldl2(check_library_is_installed(Globals, InitFileDirs, Grade),
        Libs, Succeeded0, Succeeded, !IO).

:- pred check_stdlib_is_installed(globals::in, string::in, bool::out,
    io::di, io::uo) is det.

check_stdlib_is_installed(Globals, Grade, Succeeded, !IO) :-
    verbose_make_msg_option(Globals, debug_make,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.format("Checking that the Mercury standard library is " ++
                "installed in grade `%s'.\n", [s(Grade)], !IO)
        ), !IO),
    globals.lookup_maybe_string_option(Globals,
        mercury_standard_library_directory, MaybeStdLibDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        % We check for the presence mer_std.init in the required grade.
        % Unless the installation is broken this implies the presence
        % of the other standard .init files in that grade.
        StdLibInitFile = StdLibDir / "modules" / Grade / "mer_std.init",
        io.see(StdLibInitFile, Result, !IO),
        (
            Result = ok,
            io.seen(!IO),
            Succeeded = yes
        ;
            Result = error(_),
            io.stderr_stream(Stderr, !IO),
            io.progname_base("mercury_compile", ProgName, !IO),
            io.format(Stderr,
                "%s: error: the Mercury standard library "  ++
                "cannot be found in grade %s.\n",
                [s(ProgName), s(Grade)], !IO),
            Succeeded = no
        )
    ;
        MaybeStdLibDir = no,
        Succeeded = yes
    ).

:- pred check_library_is_installed(globals::in, list(string)::in, string::in,
    string::in, bool::in, bool::out, io::di, io::uo) is det.

check_library_is_installed(Globals, Dirs, Grade, LibName, !Succeeded, !IO) :-
    verbose_make_msg_option(Globals, debug_make,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.format("Checking that %s is installed in grade `%s'.\n",
                [s(LibName), s(Grade)], !IO)
        ), !IO),
    % We check for the presence of a library in a particular grade by seeing
    % whether its .init file exists. This will work because all libraries
    % have a grade dependent .init file.
    InitFileName = LibName ++ ".init",
    search_for_file_returning_dir(Dirs, InitFileName, MaybeDirName, !IO),
    (
        MaybeDirName = ok(_)
    ;
        MaybeDirName = error(_),
        io.stderr_stream(Stderr, !IO),
        io.progname_base("mercury_compile", ProgName, !IO),
        io.format(Stderr,
            "%s: error: the library `%s' cannot be found in grade `%s'.\n",
            [s(ProgName), s(LibName), s(Grade)], !IO),
        !:Succeeded = no
    ).

%-----------------------------------------------------------------------------%
:- end_module make.program_target.
%-----------------------------------------------------------------------------%
