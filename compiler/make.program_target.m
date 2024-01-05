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
:- pred make_linked_target(io.text_output_stream::in, globals::in,
    linked_target_file::in, maybe_succeeded::out,
    make_info::in, make_info::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

    % make_misc_target(Globals, Target, Succeeded, !Info, !Specs, !IO):
    %
    % Handle miscellaneous target types, including clean-up, library
    % installation, and building all files of a given type for all
    % modules in the program.
    %
:- pred make_misc_target(io.text_output_stream::in, globals::in,
    pair(module_name, misc_target_type)::in, maybe_succeeded::out,
    make_info::in, make_info::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

    % install_library_grade(LinkSucceeded0, ModuleName, AllModules,
    %   ProgressStream, Globals, Grade, Succeeded, !Info, !IO)
    %
:- pred install_library_grade(maybe_succeeded::in,
    module_name::in, list(module_name)::in, io.text_output_stream::in,
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
:- import_module libs.system_cmds.
:- import_module libs.timestamp.
:- import_module make.build.
:- import_module make.check_up_to_date.
:- import_module make.find_local_modules.
:- import_module make.get_module_dep_info.
:- import_module make.module_target.
:- import_module make.options_file.
:- import_module make.timestamp.
:- import_module make.util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.module_deps_graph.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.write_error_spec.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module cord.
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

make_linked_target(ProgressStream, Globals, LinkedTargetFile,
        LinkedTargetSucceeded, !Info, !Specs, !IO) :-
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
            maybe_with_analysis_cache_dir_3(ProgressStream, Globals,
                make_linked_target_1(Globals, LinkedTargetFile,
                    ExtraOptions),
                LinkedTargetSucceeded, !Info, !Specs, !IO)
        ;
            LibgradeCheckSpecs = [_ | _],
            !:Specs = LibgradeCheckSpecs ++ !.Specs,
            LinkedTargetSucceeded = did_not_succeed
        )
    ).

    % The form of the argument list is dictated by the build3 type.
    %
:- pred make_linked_target_1(globals::in, linked_target_file::in,
    list(string)::in, io.text_output_stream::in, maybe_succeeded::out,
    make_info::in, make_info::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

make_linked_target_1(Globals, LinkedTargetFile, ExtraOptions,
        ProgressStream, Succeeded, !Info, !Specs, !IO) :-
    LinkedTargetFile = linked_target_file(MainModuleName, _FileType),

    % When using `--intermodule-analysis', perform an analysis pass first.
    % The analysis of one module may invalidate the results of a module
    % we analysed earlier, so this step must be carried out until all the
    % `.analysis' files are in a valid state before we can continue.
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    (
        IntermodAnalysis = yes,
        make_misc_target_builder(ProgressStream, Globals, MainModuleName,
            misc_target_build_analyses, IntermodAnalysisSucceeded,
            !Info, !Specs, !IO)
    ;
        IntermodAnalysis = no,
        IntermodAnalysisSucceeded = succeeded
    ),
    (
        IntermodAnalysisSucceeded = succeeded,
        get_default_options(Globals, DefaultOptionTable),
        DetectedGradeFlags = make_info_get_detected_grade_flags(!.Info),
        OptionVariables = make_info_get_options_variables(!.Info),
        OptionArgs = make_info_get_option_args(!.Info),
        setup_for_build_with_module_options(ProgressStream, DefaultOptionTable,
            invoked_by_mmc_make, MainModuleName, DetectedGradeFlags,
            OptionVariables, OptionArgs, ExtraOptions, MayBuild, !IO),
        (
            MayBuild = may_build(_AllOptionArgs, BuildGlobals),
            make_linked_target_2(ProgressStream, BuildGlobals,
                LinkedTargetFile, Succeeded, !Info, !IO)
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

:- pred make_linked_target_2(io.text_output_stream::in, globals::in,
    linked_target_file::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_linked_target_2(ProgressStream, Globals, LinkedTargetFile, Succeeded,
        !Info, !IO) :-
    LinkedTargetFile = linked_target_file(MainModuleName, FileType),
    find_reachable_local_modules(ProgressStream, Globals,
        MainModuleName, DepsSucceeded, AllModules, !Info, !IO),
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
        % XXX This assignment to ObjModulesAlpha represents inlining
        %   get_target_modules(Globals, IntermediateTargetType, AllModulesList,
        %       ObjModulesAlpha, !Info, !IO),
        % knowing that IntermediateTargetType cannot be module_target_errors.
        ObjModulesAlpha = AllModulesList,
        order_target_modules(ProgressStream, Globals,
            ObjModulesAlpha, ObjModules, !Info, !IO),
        filter_out_nested_modules(ProgressStream, Globals,
            ObjModules, ObjModulesNonnested, !Info, !IO),
        IntermediateTargetsNonnested =
            make_dependency_list(ObjModulesNonnested, IntermediateTargetType),
        ObjTargets = make_dependency_list(ObjModules, ObjectTargetType),

        list.map_foldl2(
            get_foreign_object_targets(ProgressStream, Globals, PIC),
            ObjModules, ForeignObjTargetsList, !Info, !IO),
        ForeignObjTargets = list.condense(ForeignObjTargetsList),

        % Ensure all interface files are present before continuing.
        % This prevents a problem when two parallel branches try to generate
        % the same missing interface file later.

        build_int_opt_files(ProgressStream, Globals,
            build_all_ints_opts, AllModulesList, IntsSucceeded, !Info, !IO),
        ( if
            IntsSucceeded = did_not_succeed,
            KeepGoing = do_not_keep_going
        then
            BuildDepsSucceeded = did_not_succeed
        else
            foldl2_make_module_targets_maybe_parallel(KeepGoing, [],
                ProgressStream, Globals, IntermediateTargetsNonnested,
                BuildDepsSucceeded0, !Info, !IO),
            (
                BuildDepsSucceeded0 = succeeded,
                ( if ObjectTargetType = module_target_java_class_code then
                    make_java_files(ProgressStream, Globals,
                        MainModuleName, ObjModules, BuildJavaSucceeded,
                        !Info, !IO),
                    (
                        BuildJavaSucceeded = succeeded,
                        % Disable the `--rebuild' option during this pass,
                        % otherwise all the Java classes will be built again.
                        globals.set_option(rebuild, bool(no),
                            Globals, NoRebuildGlobals),
                        foldl2_make_module_targets_maybe_parallel(KeepGoing,
                            [], ProgressStream, NoRebuildGlobals, ObjTargets,
                            BuildDepsSucceeded1, !Info, !IO)
                    ;
                        BuildJavaSucceeded = did_not_succeed,
                        BuildDepsSucceeded1 = did_not_succeed
                    )
                else
                    foldl2_make_module_targets_maybe_parallel(KeepGoing, [],
                        ProgressStream, Globals, ObjTargets,
                        BuildDepsSucceeded1, !Info, !IO)
                )
            ;
                BuildDepsSucceeded0 = did_not_succeed,
                BuildDepsSucceeded1 = did_not_succeed
            ),
            (
                BuildDepsSucceeded1 = succeeded,
                % We expect ForeignObjTargets to be the empty list during
                % most compiler invocations, and to be a very short list during
                % most of the remaining compiler invocations. This means that
                % parallelism here would probably incur more cost in overhead
                % than it could possibly gain back.
                foldl2_make_module_targets(KeepGoing, [],
                    ProgressStream, Globals, ForeignObjTargets,
                    BuildDepsSucceeded, !Info, !IO)
            ;
                BuildDepsSucceeded1 = did_not_succeed,
                BuildDepsSucceeded = did_not_succeed
            )
        ),

        linked_target_file_name_full_curdir(Globals, MainModuleName, FileType,
            FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName, !IO),
        get_file_timestamp([dir.this_directory], FullMainModuleLinkedFileName,
            MaybeTimestamp, !Info, !IO),
        (
            MaybeTimestamp = error(_),
            MaybeOldestLhsTimestamp = some_lhs_file_is_missing
        ;
            MaybeTimestamp = ok(LinkedFileTimestamp),
            MaybeOldestLhsTimestamp =
                all_lhs_files_exist_oldest_timestamp(LinkedFileTimestamp)
        ),
        % XXX We pass BuildDepsSucceeded here, but BuildDepsSucceeded being
        % did_not_succeed does not prevent LhsResult being can_rebuild_lhs(_).
        % This means that we can go on to attempt to (re)build the target
        % even if we couldn't build all its prerequisites. To me (zs),
        % that looks like a bug, even if the keep_going flag is set.
        should_we_rebuild_lhs(ProgressStream, Globals,
            FullMainModuleLinkedFileName, MaybeOldestLhsTimestamp,
            BuildDepsSucceeded, ObjTargets, LhsResult, !Info, !IO),
        ( if
            DepsSucceeded = succeeded,
            LhsResult = can_rebuild_lhs(ShouldRebuildLhs)
        then
            globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
            setup_checking_for_interrupt(Cookie, !IO),
            open_module_error_stream(ProgressStream, Globals, MainModuleName,
                MaybeErrorStream, !Info, !IO),
            (
                MaybeErrorStream = es_error_already_reported,
                Succeeded0 = did_not_succeed
            ;
                MaybeErrorStream = es_ok(MESI, ErrorStream),
                build_linked_target(ProgressStream, Globals,
                    MainModuleName, FileType, FullMainModuleLinkedFileName,
                    CurDirMainModuleLinkedFileName, MaybeOldestLhsTimestamp,
                    AllModules, ObjModules, CompilationTarget, PIC,
                    ShouldRebuildLhs, Succeeded0, !Info, !IO),
                close_module_error_stream_handle_errors(ProgressStream,
                    Globals, MainModuleName, MESI, ErrorStream, !Info, !IO)
            ),
            Cleanup = linked_target_cleanup(ProgressStream, Globals,
                MainModuleName, FileType,
                FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName),
            teardown_checking_for_interrupt(VeryVerbose, Cookie, Cleanup,
                Succeeded0, Succeeded, !Info, !IO)
        else
            Succeeded = did_not_succeed
        )
    ).

%---------------------%

:- pred order_target_modules(io.text_output_stream::in, globals::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

order_target_modules(ProgressStream, Globals, Modules, OrderedModules,
        !Info, !IO) :-
    globals.lookup_bool_option(Globals, order_make_by_timestamp,
        OrderByTimestamp),
    (
        OrderByTimestamp = yes,
        list.map_foldl2(pair_module_with_timestamp(ProgressStream, Globals),
            Modules, PairedModules, !Info, !IO),
        list.sort(compare_paired_modules, PairedModules, RevOrderedPairs),
        % More recently touched files, i.e. files with *larger* timestamps,
        % should appear *earlier* in the list.
        list.reverse(RevOrderedPairs, OrderedPairs),
        list.map(pair.snd, OrderedPairs, OrderedModules)
    ;
        OrderByTimestamp = no,
        list.map(pair_module_with_name, Modules, PairedModules),
        list.sort(compare_paired_modules, PairedModules, OrderedPairs),
        list.map(pair.snd, OrderedPairs, OrderedModules)
    ).

:- pred pair_module_with_timestamp(io.text_output_stream::in, globals::in,
    module_name::in, pair(timestamp, module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

pair_module_with_timestamp(ProgressStream, Globals, Module,
        Timestamp - Module, !Info, !IO) :-
    Search = do_not_search,
    Target = target_file(Module, module_target_source),
    get_target_timestamp(ProgressStream, Globals, Search, Target,
        MaybeTimestamp, !Info, !IO),
    (
        MaybeTimestamp = ok(Timestamp)
    ;
        MaybeTimestamp = error(_),
        Timestamp = oldest_timestamp
    ).

:- pred pair_module_with_name(module_name::in,
    pair(string, module_name)::out) is det.

pair_module_with_name(Module, Name - Module) :-
    Name = sym_name_to_string(Module).

:- pred compare_paired_modules(pair(T, module_name)::in,
    pair(T, module_name)::in, comparison_result::out) is det.

compare_paired_modules(KeyA - ModuleA, KeyB - ModuleB, Result) :-
    compare(KeyResult, KeyA, KeyB),
    % More recently touched files should appear earlier in the list.
    (
        ( KeyResult = (<)
        ; KeyResult = (>)
        ),
        Result = KeyResult
    ;
        KeyResult = (=),
        ModuleAStr = sym_name_to_string(ModuleA),
        ModuleBStr = sym_name_to_string(ModuleB),
        compare(Result, ModuleAStr, ModuleBStr)
    ).

%---------------------%

    % Remove all nested modules from a list of modules.
    %
:- pred filter_out_nested_modules(io.text_output_stream::in, globals::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

filter_out_nested_modules(ProgressStream, Globals, Modules0, Modules,
        !Info, !IO) :-
    list.foldl3(collect_nested_modules(ProgressStream, Globals), Modules0,
        set.init, NestedModules, !Info, !IO),
    list.negated_filter(set.contains(NestedModules), Modules0, Modules).

%---------------------%

:- pred get_foreign_object_targets(io.text_output_stream::in, globals::in,
    pic::in, module_name::in, list(dependency_file)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_foreign_object_targets(ProgressStream, Globals, PIC,
        ModuleName, ObjectTargets, !Info, !IO) :-
    % Find externally compiled foreign code files for
    % `:- pragma foreign_proc' declarations.
    %
    % Any changed here may require corresponding changes in
    % external_foreign_code_files.

    globals.get_target(Globals, CompilationTarget),
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
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

%---------------------------------------------------------------------------%

:- pred build_linked_target(io.text_output_stream::in, globals::in,
    module_name::in, linked_target_type::in, file_name::in, file_name::in,
    maybe_oldest_lhs_file::in, set(module_name)::in, list(module_name)::in,
    compilation_target::in, pic::in, should_rebuild_lhs::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

build_linked_target(ProgressStream, Globals, MainModuleName, FileType,
        FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
        MaybeOldestLhsTimestamp, AllModules, ObjModules,
        CompilationTarget, PIC, ShouldRebuildLhs, Succeeded, !Info, !IO) :-
    globals.lookup_maybe_string_option(Globals, pre_link_command,
        MaybePreLinkCommand),
    (
        MaybePreLinkCommand = yes(PreLinkCommand),
        make_all_module_command(PreLinkCommand, MainModuleName,
            set.to_sorted_list(AllModules), CommandString, !IO),
        OutputStream = ProgressStream,
        invoke_system_command(Globals, ProgressStream, OutputStream,
            cmd_verbose, CommandString, PreLinkSucceeded, !IO)
    ;
        MaybePreLinkCommand = no,
        PreLinkSucceeded = succeeded
    ),
    (
        PreLinkSucceeded = succeeded,
        build_linked_target_2(ProgressStream, Globals,
            MainModuleName, FileType,
            FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
            MaybeOldestLhsTimestamp, AllModules, ObjModules,
            CompilationTarget, PIC, ShouldRebuildLhs, Succeeded, !Info, !IO)
    ;
        PreLinkSucceeded = did_not_succeed,
        Succeeded = did_not_succeed
    ).

:- pred build_linked_target_2(io.text_output_stream::in, globals::in,
    module_name::in, linked_target_type::in, file_name::in, file_name::in,
    maybe_oldest_lhs_file::in, set(module_name)::in, list(module_name)::in,
    compilation_target::in, pic::in, should_rebuild_lhs::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

build_linked_target_2(ProgressStream, Globals0, MainModuleName, FileType,
        FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
        MaybeOldestLhsTimestamp, AllModules, ObjModules,
        CompilationTarget, PIC, ShouldRebuildLhs, Succeeded, !Info, !IO) :-
    % Clear the link_objects option -- we will pass the list of files directly.
    globals.lookup_accumulating_option(Globals0, link_objects, LinkObjects),
    globals.set_option(link_objects, accumulating([]),
        Globals0, NoLinkObjsGlobals),

    % Remake the `_init.o' file.
    % XXX We should probably make a `_init.o' file for shared
    % libraries linked using dlopen().
    AllModulesList = set.to_sorted_list(AllModules),
    (
        FileType = executable,
        make_init_obj_file_check_result(ProgressStream, NoLinkObjsGlobals,
            MainModuleName, AllModulesList, InitObjSucceeded, InitObjects,
            !Info, !IO)
    ;
        ( FileType = static_library
        ; FileType = shared_library
        ; FileType = csharp_executable
        ; FileType = csharp_library
        ; FileType = java_executable
        ; FileType = java_archive
        ),
        InitObjSucceeded = succeeded,
        InitObjects = []
    ),

    % Report errors if any of the extra objects aren't present.
    ObjectsToCheck = InitObjects ++ LinkObjects,
    ObjectsToCheckDepFiles = list.map((func(F) = dep_file(F)), ObjectsToCheck),
    list.map_foldl2(
        get_dependency_file_status(ProgressStream, NoLinkObjsGlobals),
        ObjectsToCheckDepFiles, ExtraObjDepStatuses, !Info, !IO),
    ( if
        some [ExtraObjDepStatus] (
            list.member(ExtraObjDepStatus, ExtraObjDepStatuses),
            ExtraObjDepStatus =
                dependency_status_result(_, _, deps_status_error)
        )
    then
        ExtraObjSucceeded = did_not_succeed
    else
        ExtraObjSucceeded = succeeded
    ),
    BuildDepsSucceeded = InitObjSucceeded `and` ExtraObjSucceeded,

    list.map_foldl2(get_file_timestamp([dir.this_directory]),
        ObjectsToCheck, ExtraObjectTimestamps, !Info, !IO),
    % XXX We pass BuildDepsSucceeded here, but BuildDepsSucceeded being
    % did_not_succeed does not prevent LhsResult being can_rebuild_lhs(_).
    % This means that we can go on to attempt to (re)build the target
    % even if we couldn't build all its prerequisites. To me (zs),
    % that looks like a bug, even if the keep_going flag is set.
    should_we_rebuild_lhs_given_timestamps(ProgressStream, NoLinkObjsGlobals,
        FullMainModuleLinkedFileName, MaybeOldestLhsTimestamp,
        BuildDepsSucceeded, ExtraObjDepStatuses, ExtraObjectTimestamps,
        ExtraObjectLhsResult, !IO),
    (
        ExtraObjectLhsResult = rhs_error,
        % XXX We should get here if BuildDepsSucceeded = did_not_succeed,
        % as well as if ExtraObjectDepsResult = rhs_error.
        file_error_msg(FullMainModuleLinkedFileName, ErrorMsg),
        maybe_write_msg_locked(ProgressStream, !.Info, ErrorMsg, !IO),
        Succeeded = did_not_succeed
    ;
        ExtraObjectLhsResult = can_rebuild_lhs(ExtraObjShouldRebuildLhs),
        % Our caller has checked whether the lhs is up to date with respect
        % to *some* files on the rhs, and just above we checked whether they
        % are up to date with respect to *other* files on the rhs.
        % XXX This seems like a clumsy arrangement.
        ( if
            ShouldRebuildLhs = all_lhs_files_up_to_date,
            ExtraObjShouldRebuildLhs = all_lhs_files_up_to_date
        then
            post_link_maybe_warn_linked_target_up_to_date(ProgressStream,
                NoLinkObjsGlobals, MainModuleName, FileType,
                FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
                Succeeded, !Info, !IO)
        else
            rebuild_linked_target(ProgressStream, NoLinkObjsGlobals,
                MainModuleName, FileType, FullMainModuleLinkedFileName,
                AllModulesList, ObjModules, InitObjects, LinkObjects,
                CompilationTarget, PIC, Succeeded, !Info, !IO)
        )
    ).

:- pred make_init_obj_file_check_result(io.text_output_stream::in, globals::in,
    module_name::in, list(module_name)::in,
    maybe_succeeded::out, list(file_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_init_obj_file_check_result(ProgressStream, NoLinkObjsGlobals,
        MainModuleName, AllModulesList, Succeeded, InitObjects, !Info, !IO) :-
    make_init_obj_file(NoLinkObjsGlobals, ProgressStream,
        MainModuleName, AllModulesList, InitObjectResult, !IO),
    (
        InitObjectResult = yes(InitObject),
        % We may need to update the timestamp of the `_init.o' file.
        FileTimestamps0 = make_info_get_file_timestamps(!.Info),
        map.delete(InitObject, FileTimestamps0, FileTimestamps1),
        make_info_set_file_timestamps(FileTimestamps1, !Info),
        % There is no module_target_type for the `_init.o' file,
        % so mki_target_file_timestamps should not contain anything
        % that needs to be invalidated.
        Succeeded = succeeded,
        InitObjects = [InitObject]
    ;
        InitObjectResult = no,
        Succeeded = did_not_succeed,
        InitObjects = []
    ).

:- pred post_link_maybe_warn_linked_target_up_to_date(
    io.text_output_stream::in, globals::in,
    module_name::in, linked_target_type::in, file_name::in, file_name::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

post_link_maybe_warn_linked_target_up_to_date(ProgressStream,
        NoLinkObjsGlobals, MainModuleName, FileType,
        FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
        Succeeded, !Info, !IO) :-
    MainModuleLinkedTarget =
        top_target_file(MainModuleName, linked_target(FileType)),
    ( if FullMainModuleLinkedFileName = CurDirMainModuleLinkedFileName then
        maybe_warn_up_to_date_target_msg(NoLinkObjsGlobals,
            MainModuleLinkedTarget, FullMainModuleLinkedFileName, !Info,
            UpToDateMsg),
        maybe_write_msg(ProgressStream, UpToDateMsg, !IO),
        Succeeded = succeeded
    else
        post_link_maybe_make_symlink_or_copy(NoLinkObjsGlobals, ProgressStream,
            FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
            MainModuleName, FileType, Succeeded, MadeSymlinkOrCopy, !IO),
        (
            MadeSymlinkOrCopy = yes,
            maybe_symlink_or_copy_linked_target_msg(NoLinkObjsGlobals,
                FullMainModuleLinkedFileName, LinkMsg),
            maybe_write_msg(ProgressStream, LinkMsg, !IO)
        ;
            MadeSymlinkOrCopy = no,
            maybe_warn_up_to_date_target_msg(NoLinkObjsGlobals,
                MainModuleLinkedTarget, FullMainModuleLinkedFileName,
                !Info, UpToDateMsg),
            maybe_write_msg(ProgressStream, UpToDateMsg, !IO)
        )
    ).

:- pred rebuild_linked_target(io.text_output_stream::in, globals::in,
    module_name::in, linked_target_type::in, file_name::in,
    list(module_name)::in, list(module_name)::in,
    list(file_name)::in, list(file_name)::in, compilation_target::in, pic::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

rebuild_linked_target(ProgressStream, NoLinkObjsGlobals,
        MainModuleName, FileType, FullMainModuleLinkedFileName,
        AllModulesList, ObjModules, InitObjects, LinkObjects,
        CompilationTarget, PIC, Succeeded, !Info, !IO) :-
    maybe_making_filename_msg(NoLinkObjsGlobals,
        FullMainModuleLinkedFileName, MakingMsg),
    maybe_write_msg(ProgressStream, MakingMsg, !IO),

    % Find the extra object files for externally compiled foreign
    % procedures and fact tables. We don't need to include these in the
    % timestamp checking above -- they will have been checked when the
    % module's object file was built.
    list.map_foldl2(
        get_module_foreign_object_files(ProgressStream, NoLinkObjsGlobals,
            PIC),
        AllModulesList, ForeignObjectFileLists, !Info, !IO),
    ForeignObjects = list.condense(ForeignObjectFileLists),

    (
        CompilationTarget = target_c,
        maybe_pic_object_file_extension(PIC, ObjExt, _),
        Ext = ext_cur_ngs_gs(ObjExt)
    ;
        CompilationTarget = target_csharp,
        % There is no separate object code step.
        Ext = ext_cur_ngs_gs(ext_cur_ngs_gs_target_cs)
    ;
        CompilationTarget = target_java,
        Ext = ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_class)
    ),
    list.map(module_name_to_file_name(NoLinkObjsGlobals, $pred, Ext),
        ObjModules, ObjList),

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
            compile_target_code.link(NoLinkObjsGlobals, ProgressStream,
                FileType, MainModuleName, AllObjects),
            Succeeded, !IO)
    ),
    CmdLineTargets0 = make_info_get_command_line_targets(!.Info),
    set.delete(top_target_file(MainModuleName, linked_target(FileType)),
        CmdLineTargets0, CmdLineTargets),
    make_info_set_command_line_targets(CmdLineTargets, !Info),
    (
        Succeeded = succeeded,
        FileTimestamps2 = make_info_get_file_timestamps(!.Info),
        map.delete(FullMainModuleLinkedFileName,
            FileTimestamps2, FileTimestamps),
        make_info_set_file_timestamps(FileTimestamps, !Info)
        % There is no module_target_type for the linked target,
        % so mki_target_file_timestamps should not contain anything
        % that needs to be invalidated.
    ;
        Succeeded = did_not_succeed,
        file_error_msg(FullMainModuleLinkedFileName, ErrorMsg),
        maybe_write_msg_locked(ProgressStream, !.Info, ErrorMsg, !IO)
    ).

:- pred get_module_foreign_object_files(io.text_output_stream::in, globals::in,
    pic::in, module_name::in, list(file_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_module_foreign_object_files(ProgressStream, Globals, PIC,
        ModuleName, ForeignObjectFiles, !MakeInfo, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !MakeInfo, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        get_any_fact_table_object_code_files(Globals, PIC, ModuleDepInfo,
            ForeignFiles, !IO),
        ForeignObjectFiles = list.map(
            (func(foreign_code_file(_, _, ObjFile)) = ObjFile),
            ForeignFiles)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        % This error should have been detected earlier.
        unexpected($pred, "error in dependencies")
    ).

:- pred linked_target_cleanup(io.text_output_stream::in, globals::in,
    module_name::in, linked_target_type::in, file_name::in, file_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

linked_target_cleanup(ProgressStream, Globals, MainModuleName, FileType,
        FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
        !Info, !IO) :-
    remove_file_for_make(ProgressStream, Globals, verbose_make,
        FullMainModuleLinkedFileName, !Info, !IO),
    ( if FullMainModuleLinkedFileName = CurDirMainModuleLinkedFileName then
        true
    else
        remove_file_for_make(ProgressStream, Globals, verbose_make,
            CurDirMainModuleLinkedFileName, !Info, !IO)
    ),
    (
        FileType = executable,
        remove_init_files(ProgressStream, Globals, verbose_make,
            MainModuleName, !Info, !IO)
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
:- pred make_java_files(io.text_output_stream::in, globals::in,
    module_name::in, list(module_name)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_java_files(ProgressStream, Globals, MainModuleName, ObjModules,
        Succeeded, !Info, !IO) :-
    out_of_date_java_modules(ProgressStream, Globals,
        ObjModules, OutOfDateModules, !Info, !IO),
    (
        OutOfDateModules = [],
        Succeeded = succeeded
    ;
        OutOfDateModules = [_ | _],
        build_java_files(ProgressStream, Globals,
            MainModuleName, OutOfDateModules, Succeeded, !Info, !IO),
        % javac might write more `.class' files than we anticipated (though
        % it probably won't) so clear out all the timestamps which might be
        % affected.
        Timestamps0 = make_info_get_file_timestamps(!.Info),
        map.foldl(do_not_reinsert_java_class_timestamps, Timestamps0,
            map.init, Timestamps),
        make_info_set_file_timestamps(Timestamps, !Info),
        % For simplicity, clear out all target file timestamps.
        make_info_set_target_file_timestamps(init_target_file_timestamps,
            !Info)
    ).

:- pred out_of_date_java_modules(io.text_output_stream::in, globals::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

out_of_date_java_modules(ProgressStream, Globals, ObjModules, OutOfDateModules,
        !Info, !IO) :-
    (
        ObjModules = [],
        OutOfDateModules = []
    ;
        ObjModules = [ModuleName | ModuleNames],
        out_of_date_java_modules(ProgressStream, Globals,
            ModuleNames, OutOfDateModules0, !Info, !IO),
        JavaTarget = target_file(ModuleName, module_target_java_code),
        ClassTarget = target_file(ModuleName, module_target_java_class_code),
        get_target_timestamp(ProgressStream, Globals, do_not_search,
            JavaTarget, MaybeJavaTimestamp, !Info, !IO),
        get_target_timestamp(ProgressStream, Globals, do_not_search,
            ClassTarget, MaybeClassTimestamp, !Info, !IO),
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

:- pred build_java_files(io.text_output_stream::in, globals::in,
    module_name::in, list(module_name)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_java_files(ProgressStream, Globals, MainModuleName, ModuleNames,
        Succeeded, !Info, !IO) :-
    verbose_make_one_part_msg(Globals, "Making Java class files", MakingMsg),
    maybe_write_msg(ProgressStream, MakingMsg, !IO),
    % XXX FILE_NAMES
    list.map_foldl(
        module_name_to_file_name_create_dirs(Globals, $pred,
            ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java)),
        ModuleNames, JavaFiles, !IO),
    % We redirect errors to a file named after the main module.
    open_module_error_stream(ProgressStream, Globals, MainModuleName,
        MaybeErrorStream, !Info, !IO),
    (
        MaybeErrorStream = es_error_already_reported,
        Succeeded = did_not_succeed
    ;
        MaybeErrorStream = es_ok(MESI, ErrorStream),
        build_java_files_2(ProgressStream, Globals, JavaFiles, Succeeded,
            !Info, !IO),
        close_module_error_stream_handle_errors(ProgressStream, Globals,
            MainModuleName, MESI, ErrorStream, !Info, !IO)
    ).

:- pred build_java_files_2(io.text_output_stream::in, globals::in,
    list(string)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_java_files_2(ProgressStream, Globals, JavaFiles, Succeeded,
        !Info, !IO) :-
    list.det_head_tail(JavaFiles, HeadJavaFile, TailJavaFiles),
    call_in_forked_process(
        compile_java_files(Globals, ProgressStream,
            HeadJavaFile, TailJavaFiles),
        Succeeded, !IO).

:- pred do_not_reinsert_java_class_timestamps(string::in,
    maybe_error(timestamp)::in,
    file_timestamps::in, file_timestamps::out) is det.

do_not_reinsert_java_class_timestamps(FileName, MaybeTimestamp, !Timestamps) :-
    ( if string.suffix(FileName, ".class") then
        true
    else
        map.det_insert(FileName, MaybeTimestamp, !Timestamps)
    ).

%---------------------------------------------------------------------------%

make_misc_target(ProgressStream, Globals, MainModuleName - TargetType,
        Succeeded, !Info, !Specs, !IO) :-
    get_default_options(Globals, DefaultOptionTable),
    DetectedGradeFlags = make_info_get_detected_grade_flags(!.Info),
    OptionVariables = make_info_get_options_variables(!.Info),
    OptionArgs = make_info_get_option_args(!.Info),
    ExtraOptions = [],
    setup_for_build_with_module_options(ProgressStream, DefaultOptionTable,
        invoked_by_mmc_make, MainModuleName, DetectedGradeFlags,
        OptionVariables, OptionArgs, ExtraOptions, MayBuild, !IO),
    (
        MayBuild = may_build(_AllOptionArgs, BuildGlobals),
        make_misc_target_builder(ProgressStream, BuildGlobals, MainModuleName,
            TargetType, Succeeded, !Info, !Specs, !IO)
    ;
        MayBuild = may_not_build(Specs),
        % XXX MAKE_STREAM
        get_error_output_stream(Globals, MainModuleName, ErrorStream, !IO),
        write_error_specs(ErrorStream, Globals, Specs, !IO),
        Succeeded = did_not_succeed
    ).

:- pred make_misc_target_builder(io.text_output_stream::in, globals::in,
    module_name::in, misc_target_type::in, maybe_succeeded::out,
    make_info::in, make_info::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

make_misc_target_builder(ProgressStream, Globals, MainModuleName, TargetType,
        Succeeded, !Info, !Specs, !IO) :-
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
    find_reachable_local_modules(ProgressStream, Globals, MainModuleName,
        Succeeded0, AllModulesSet, !Info, !IO),
    make_info_set_rebuild_module_deps(RebuildModuleDeps, !Info),
    AllModules = set.to_sorted_list(AllModulesSet),
    (
        TargetType = misc_target_clean,
        Succeeded = succeeded,
        list.foldl2(make_module_clean(ProgressStream, Globals),
            AllModules, !Info, !IO),
        remove_init_files(ProgressStream, Globals, very_verbose,
            MainModuleName, !Info, !IO)
    ;
        TargetType = misc_target_realclean,
        Succeeded = succeeded,
        make_main_module_realclean(ProgressStream, Globals, MainModuleName,
            !Info, !IO),
        list.foldl2(make_module_realclean(ProgressStream, Globals), AllModules,
            !Info, !IO)
    ;
        TargetType = misc_target_build_all(ModuleTargetType),
        get_target_modules(ProgressStream, Globals, ModuleTargetType,
            AllModules, TargetModules, !Info, !IO),
        KeepGoing = make_info_get_keep_going(!.Info),
        ( if Succeeded0 = did_not_succeed, KeepGoing = do_not_keep_going then
            Succeeded = did_not_succeed
        else
            % Ensure all interface files EARLIER THAN ModuleTargetType
            % are present before continuing.
            % This prevents a problem when two parallel branches
            % try to generate the same missing interface file later.
            (
                ( ModuleTargetType = module_target_source
                ; ModuleTargetType = module_target_track_flags
                ; ModuleTargetType = module_target_int3
                ),
                % There are no earlier interface files.
                Succeeded1 = succeeded
            ;
                ModuleTargetType = module_target_int0,
                build_int_opt_files(ProgressStream, Globals,
                    build_int3s, AllModules, Succeeded1, !Info, !IO)
            ;
                ( ModuleTargetType = module_target_int1
                ; ModuleTargetType = module_target_int2
                ),
                build_int_opt_files(ProgressStream, Globals,
                    build_int3s_int0s, AllModules, Succeeded1, !Info, !IO)
            ;
                ModuleTargetType = module_target_opt,
                build_int_opt_files(ProgressStream, Globals,
                    build_all_ints, AllModules, Succeeded1, !Info, !IO)
            ;
                ( ModuleTargetType = module_target_errors
                ; ModuleTargetType = module_target_analysis_registry
                ; ModuleTargetType = module_target_c_header(_)
                ; ModuleTargetType = module_target_c_code
                ; ModuleTargetType = module_target_csharp_code
                ; ModuleTargetType = module_target_java_code
                ; ModuleTargetType = module_target_java_class_code
                ; ModuleTargetType = module_target_object_code(_)
                ; ModuleTargetType = module_target_fact_table_object(_, _)
                ; ModuleTargetType = module_target_xml_doc
                ),
                build_int_opt_files(ProgressStream, Globals,
                    build_all_ints_opts, AllModules, Succeeded1, !Info, !IO)
            ),
            ( if
                Succeeded1 = did_not_succeed,
                KeepGoing = do_not_keep_going
            then
                Succeeded = did_not_succeed
            else
                Targets =
                    make_dependency_list(TargetModules, ModuleTargetType),
                maybe_with_analysis_cache_dir_2(ProgressStream, Globals,
                    foldl2_make_module_targets_maybe_parallel_build2(KeepGoing,
                        [], Globals, Targets),
                    Succeeded2, !Info, !IO),
                Succeeded = Succeeded0 `and` Succeeded1 `and` Succeeded2
            )
        )
    ;
        TargetType = misc_target_build_analyses,
        maybe_with_analysis_cache_dir_2(ProgressStream, Globals,
            build_analysis_files(Globals, MainModuleName,
                AllModules, Succeeded0),
            Succeeded, !Info, !IO)
    ;
        TargetType = misc_target_build_library,
        build_int_opt_files(ProgressStream, Globals, build_all_ints_opts,
            AllModules, IntSucceeded, !Info, !IO),
        (
            IntSucceeded = succeeded,
            maybe_with_analysis_cache_dir_3(ProgressStream, Globals,
                build_library(MainModuleName, AllModules, Globals),
                Succeeded, !Info, !Specs, !IO)
        ;
            IntSucceeded = did_not_succeed,
            Succeeded = did_not_succeed
        )
    ;
        TargetType = misc_target_install_library,
        make_misc_target(ProgressStream, Globals,
            MainModuleName - misc_target_build_library, LibSucceeded,
            !Info, !Specs, !IO),
        (
            LibSucceeded = succeeded,
            install_library(ProgressStream, Globals,
                MainModuleName, Succeeded, !Info, !IO)
        ;
            LibSucceeded = did_not_succeed,
            Succeeded = did_not_succeed
        )
    ;
        TargetType = misc_target_build_xml_docs,
        get_target_modules(ProgressStream, Globals, module_target_xml_doc,
            AllModules, TargetModules, !Info, !IO),
        KeepGoing = make_info_get_keep_going(!.Info),
        ( if Succeeded0 = did_not_succeed, KeepGoing = do_not_keep_going then
            Succeeded = did_not_succeed
        else
            XmlDocs =
                make_dependency_list(TargetModules, module_target_xml_doc),
            foldl2_make_module_targets(KeepGoing, [],
                ProgressStream, Globals, XmlDocs, Succeeded1, !Info, !IO),
            Succeeded = Succeeded0 `and` Succeeded1
        )
    ).

%---------------------------------------------------------------------------%

:- type build_what
    --->    build_int3s
    ;       build_int3s_int0s
    ;       build_all_ints
    ;       build_all_ints_opts.

:- pred build_int_opt_files(io.text_output_stream::in, globals::in,
    build_what::in, list(module_name)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_int_opt_files(ProgressStream, Globals, BuildWhat, AllModules0,
        Succeeded, !Info, !IO) :-
    get_nonnested_and_parent_modules(ProgressStream, Globals, AllModules0,
        NonnestedModules, ParentModules, !Info, !IO),

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
    foldl2_make_module_targets_maybe_parallel(KeepGoing, [],
        ProgressStream, Globals, Int3s, Succeeded0, !Info, !IO),
    ( if
        ( Succeeded0 = did_not_succeed
        ; BuildWhat = build_int3s
        )
    then
        Succeeded = Succeeded0
    else
        foldl2_make_module_targets(KeepGoing, [],
            ProgressStream, Globals, Int0s, Succeeded1, !Info, !IO),
        ( if
            ( Succeeded1 = did_not_succeed
            ; BuildWhat = build_int3s_int0s
            )
        then
            Succeeded = Succeeded1
        else
            foldl2_make_module_targets_maybe_parallel(KeepGoing, [],
                ProgressStream, Globals, Int1s, Succeeded2, !Info, !IO),
            ( if
                ( Succeeded2 = did_not_succeed
                ; BuildWhat = build_all_ints
                )
            then
                Succeeded = Succeeded2
            else
                foldl2_make_module_targets_maybe_parallel(KeepGoing, [],
                    ProgressStream, Globals, Opts, Succeeded, !Info, !IO)
            )
        )
    ).

%---------------------------------------------------------------------------%

:- type build2(Info) == pred(io.text_output_stream, maybe_succeeded,
    Info, Info, io, io).
:- inst build2 == (pred(in, out, in, out, di, uo) is det).

    % If `--analysis-file-cache' is enabled, create a temporary directory for
    % holding analysis cache files and pass that to child processes.
    % After P is finished, remove the cache directory completely.
    %
:- pred maybe_with_analysis_cache_dir_2(io.text_output_stream::in, globals::in,
    build2(make_info)::in(build2), maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

maybe_with_analysis_cache_dir_2(ProgressStream, Globals, P, Succeeded,
        !Info, !IO) :-
    should_we_use_analysis_cache_dir(ProgressStream, Globals, !.Info,
        UseAnalysisCacheDir, !IO),
    (
        UseAnalysisCacheDir = do_not_use_analysis_cache_dir,
        P(ProgressStream, Succeeded, !Info, !IO)
    ;
        UseAnalysisCacheDir = use_analysis_cache_dir(CacheDir, CacheDirOption),
        OrigOptionArgs = make_info_get_option_args(!.Info),
        % Pass the name of the cache directory to child processes.
        make_info_set_option_args(OrigOptionArgs ++ [CacheDirOption, CacheDir],
            !Info),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        setup_checking_for_interrupt(Cookie, !IO),
        P(ProgressStream, Succeeded1, !Info, !IO),
        Cleanup = remove_cache_dir(ProgressStream, Globals, CacheDir),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, Cleanup,
            Succeeded1, Succeeded, !Info, !IO),
        remove_cache_dir(ProgressStream, Globals, CacheDir, !Info, !IO),
        make_info_set_option_args(OrigOptionArgs, !Info)
    ;
        UseAnalysisCacheDir = analysis_cache_dir_create_failed,
        Succeeded = did_not_succeed
    ).

%---------------------%

:- type build3(Info) == pred(io.text_output_stream, maybe_succeeded,
    Info, Info, list(error_spec), list(error_spec), io, io).
:- inst build3 == (pred(in, out, in, out, in, out, di, uo) is det).

    % If `--analysis-file-cache' is enabled, create a temporary directory for
    % holding analysis cache files and pass that to child processes.
    % After P is finished, remove the cache directory completely.
    %
:- pred maybe_with_analysis_cache_dir_3(io.text_output_stream::in, globals::in,
    build3(make_info)::in(build3), maybe_succeeded::out,
    make_info::in, make_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

maybe_with_analysis_cache_dir_3(ProgressStream, Globals, P, Succeeded,
        !Info, !Specs, !IO) :-
    should_we_use_analysis_cache_dir(ProgressStream, Globals, !.Info,
        UseAnalysisCacheDir, !IO),
    (
        UseAnalysisCacheDir = do_not_use_analysis_cache_dir,
        P(ProgressStream, Succeeded, !Info, !Specs, !IO)
    ;
        UseAnalysisCacheDir = use_analysis_cache_dir(CacheDir, CacheDirOption),
        OrigOptionArgs = make_info_get_option_args(!.Info),
        % Pass the name of the cache directory to child processes.
        make_info_set_option_args(OrigOptionArgs ++ [CacheDirOption, CacheDir],
            !Info),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        setup_checking_for_interrupt(Cookie, !IO),
        P(ProgressStream, Succeeded1, !Info, !Specs, !IO),
        Cleanup = remove_cache_dir(ProgressStream, Globals, CacheDir),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, Cleanup,
            Succeeded1, Succeeded, !Info, !IO),
        remove_cache_dir(ProgressStream, Globals, CacheDir, !Info, !IO),
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
:- pred should_we_use_analysis_cache_dir(io.text_output_stream::in,
    globals::in, make_info::in, maybe_use_analysis_cache_dir::out,
    io::di, io::uo) is det.

should_we_use_analysis_cache_dir(ProgressStream, Globals, Info,
        UseAnalysisCacheDir, !IO) :-
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
        create_analysis_cache_dir(ProgressStream, Globals, Succeeded,
            CacheDir, !IO),
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

:- pred create_analysis_cache_dir(io.text_output_stream::in, globals::in,
    maybe_succeeded::out, string::out, io::di, io::uo) is det.

create_analysis_cache_dir(ProgressStream, Globals, Succeeded, CacheDir, !IO) :-
    analysis_cache_dir_name(Globals, CacheDir),
    verbose_make_two_part_msg(Globals, "Creating", CacheDir, CreatingMsg),
    maybe_write_msg(ProgressStream, CreatingMsg, !IO),
    dir.make_directory(CacheDir, MakeRes, !IO),
    (
        MakeRes = ok,
        Succeeded = succeeded
    ;
        MakeRes = error(Error),
        io.format(ProgressStream, "Error: making directory %s: %s\n",
            [s(CacheDir), s(io.error_message(Error))], !IO),
        Succeeded = did_not_succeed
    ).

:- pred remove_cache_dir(io.text_output_stream::in, globals::in, string::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_cache_dir(ProgressStream, Globals, CacheDir, !Info, !IO) :-
    verbose_make_two_part_msg(Globals, "Removing", CacheDir, RemovingMsg),
    maybe_write_msg(ProgressStream, RemovingMsg, !IO),
    io.file.remove_file_recursively(CacheDir, _, !IO).

%---------------------------------------------------------------------------%

    % The form of the argument list is dictated by the build2 type.
    %
:- pred build_analysis_files(globals::in, module_name::in,
    list(module_name)::in, maybe_succeeded::in,
    io.text_output_stream::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_analysis_files(Globals, MainModuleName, AllModules,
        Succeeded0, ProgressStream, Succeeded, !Info, !IO) :-
    KeepGoing = make_info_get_keep_going(!.Info),
    ( if
        Succeeded0 = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed
    else
        % Ensure all .intN and .opt files are present before continuing.
        % This prevents a problem when two parallel branches try to generate
        % the same missing file later.
        % (Although we can't actually build analysis files in parallel yet.)
        build_int_opt_files(ProgressStream, Globals, build_all_ints_opts,
            AllModules, Succeeded1, !Info, !IO),
        ( if
            Succeeded1 = did_not_succeed,
            KeepGoing = do_not_keep_going
        then
            Succeeded = did_not_succeed
        else
            build_analysis_files_1(ProgressStream, Globals,
                MainModuleName, AllModules, Succeeded, !Info, !IO)
        )
    ).

:- pred build_analysis_files_1(io.text_output_stream::in, globals::in,
    module_name::in, list(module_name)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_analysis_files_1(ProgressStream, Globals, MainModuleName, AllModules,
        Succeeded, !Info, !IO) :-
    get_target_modules(ProgressStream, Globals,
        module_target_analysis_registry, AllModules, TargetModules0,
        !Info, !IO),
    get_reverse_ordered_modules(
        make_info_get_maybe_module_dep_info_map(!.Info),
        TargetModules0, TargetModules1),
    % Filter out the non-local modules so we don't try to reanalyse them.
    list.filter(list.contains(AllModules), TargetModules1, TargetModules),
    make_local_module_id_options(ProgressStream, Globals, MainModuleName,
        Succeeded0, LocalModulesOpts, !Info, !IO),
    (
        Succeeded0 = succeeded,
        build_analysis_files_2(ProgressStream, Globals, MainModuleName,
            TargetModules, LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
    ;
        Succeeded0 = did_not_succeed,
        Succeeded = did_not_succeed
    ).

:- pred build_analysis_files_2(io.text_output_stream::in, globals::in,
    module_name::in, list(module_name)::in, list(string)::in,
    maybe_succeeded::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_analysis_files_2(ProgressStream, Globals, MainModuleName, TargetModules,
        LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO) :-
    KeepGoing = make_info_get_keep_going(!.Info),
    Registries =
        make_dependency_list(TargetModules, module_target_analysis_registry),
    foldl2_make_module_targets(KeepGoing, LocalModulesOpts, ProgressStream,
        Globals, Registries, Succeeded1, !Info, !IO),
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
        maybe_write_msg(ProgressStream, ReanalysingMsg, !IO),
        list.foldl(reset_analysis_registry_dependency_status,
            InvalidModules, !Info),
        build_analysis_files_2(ProgressStream, Globals, MainModuleName,
            TargetModules, LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
    else if list.is_not_empty(SuboptimalModules) then
        list.foldl(reset_analysis_registry_dependency_status,
            SuboptimalModules, !Info),
        make_info_set_reanalysis_passes(ReanalysisPasses - 1, !Info),
        maybe_reanalyse_modules_msg(Globals, ReanalysingMsg),
        maybe_write_msg(ProgressStream, ReanalysingMsg, !IO),
        build_analysis_files_2(ProgressStream, Globals, MainModuleName,
            TargetModules, LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
    else
        Succeeded = Succeeded0 `and` Succeeded1
    ).

%---------------------------------------------------------------------------%

:- pred get_target_modules(io.text_output_stream::in, globals::in,
    module_target_type::in, list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_modules(ProgressStream, Globals, TargetType,
        AllModules, TargetModules, !Info, !IO) :-
    ( if TargetType = module_target_errors then
        % `.err' files are only produced for the top-level module
        % in each source file.
        list.foldl3(
            get_non_nested_target_modules(ProgressStream, Globals),
            AllModules, cord.init, TargetModulesCord, !Info, !IO),
        TargetModules = cord.list(TargetModulesCord)
    else
        TargetModules = AllModules
    ).

:- pred get_non_nested_target_modules(io.text_output_stream::in, globals::in,
    module_name::in, cord(module_name)::in, cord(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_non_nested_target_modules(ProgressStream, Globals, ModuleName,
        !TargetModulesCord, !Info, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    ( if
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_source_file_module_name(ModuleDepInfo,
            SourceFileModuleName),
        ModuleName = SourceFileModuleName
    then
        cord.snoc(ModuleName, !TargetModulesCord)
    else
        true
    ).

%---------------------------------------------------------------------------%

    % Return a list of modules in reverse order of their dependencies, i.e.
    % the list is the module dependency graph from bottom-up. Mutually
    % dependent modules (modules which form a clique in the dependency graph)
    % are returned adjacent in the list in arbitrary order.
    %
:- pred get_reverse_ordered_modules(
    map(module_name, maybe_module_dep_info)::in,
    list(module_name)::in, list(module_name)::out) is det.

get_reverse_ordered_modules(ModuleDeps, Modules0, Modules) :-
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
    DepStatusMap0 = make_info_get_dep_file_status_map(!.Info),
    version_hash_table.set(Dep, deps_status_not_considered,
        DepStatusMap0, DepStatusMap),
    make_info_set_dep_file_status_map(DepStatusMap, !Info).

%---------------------------------------------------------------------------%

    % The form of the argument list is dictated by the build3 type.
    %
:- pred build_library(module_name::in, list(module_name)::in,
    globals::in, io.text_output_stream::in, maybe_succeeded::out,
    make_info::in, make_info::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

build_library(MainModuleName, AllModules, Globals, ProgressStream, Succeeded,
        !Info, !Specs, !IO) :-
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        build_c_library(ProgressStream, Globals, MainModuleName, AllModules,
            Succeeded, !Info, !Specs, !IO)
    ;
        Target = target_csharp,
        build_csharp_library(ProgressStream, Globals, MainModuleName,
            Succeeded, !Info, !Specs, !IO)
    ;
        Target = target_java,
        build_java_library(ProgressStream, Globals, MainModuleName,
            Succeeded, !Info, !Specs, !IO)
    ).

:- pred build_c_library(io.text_output_stream::in, globals::in,
    module_name::in, list(module_name)::in, maybe_succeeded::out,
    make_info::in, make_info::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

build_c_library(ProgressStream, Globals, MainModuleName, AllModules, Succeeded,
        !Info, !Specs, !IO) :-
    make_linked_target(ProgressStream, Globals,
        linked_target_file(MainModuleName, static_library),
        StaticSucceeded, !Info, !Specs, !IO),
    shared_libraries_supported(Globals, SharedLibsSupported),
    (
        StaticSucceeded = succeeded,
        (
            SharedLibsSupported = yes,
            make_linked_target(ProgressStream, Globals,
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
            make_library_init_file(Globals, ProgressStream,
                MainModuleName, AllModules, Succeeded, !IO)
        ;
            SharedLibsSucceeded = did_not_succeed,
            Succeeded = did_not_succeed
        )
    ;
        StaticSucceeded = did_not_succeed,
        Succeeded = did_not_succeed
    ).

:- pred build_csharp_library(io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out, make_info::in, make_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

build_csharp_library(ProgressStream, Globals, MainModuleName, Succeeded,
        !Info, !Specs, !IO) :-
    make_linked_target(ProgressStream, Globals,
        linked_target_file(MainModuleName, csharp_library),
        Succeeded, !Info, !Specs, !IO).

:- pred build_java_library(io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out, make_info::in, make_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

build_java_library(ProgressStream, Globals, MainModuleName, Succeeded,
        !Info, !Specs, !IO) :-
    make_linked_target(ProgressStream, Globals,
        linked_target_file(MainModuleName, java_archive),
        Succeeded, !Info, !Specs, !IO).

%---------------------------------------------------------------------------%

:- pred install_library(io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_library(ProgressStream, Globals, MainModuleName, Succeeded,
        !Info, !IO) :-
    find_reachable_local_modules(ProgressStream, Globals, MainModuleName,
        DepsSucceeded, AllModules0, !Info, !IO),
    AllModules = set.to_sorted_list(AllModules0),
    make_install_dirs(ProgressStream, Globals,
        DirSucceeded, LinkSucceeded, !IO),
    ( if
        DepsSucceeded = succeeded,
        DirSucceeded = succeeded
    then
        list.map_foldl2(
            install_ints_and_headers(ProgressStream, Globals, LinkSucceeded),
            AllModules, IntsSucceeded, !Info, !IO),
        install_extra_headers(ProgressStream, Globals,
            ExtraHdrsSucceeded, !IO),

        grade_directory_component(Globals, Grade),
        install_library_grade_files(ProgressStream, Globals, LinkSucceeded,
            Grade, MainModuleName, AllModules, GradeSucceeded, !Info, !IO),
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
                ProgressStream, Globals, LibGrades, Succeeded, !Info, !IO)
        else
            Succeeded = did_not_succeed
        )
    else
        Succeeded = did_not_succeed
    ).

:- pred install_ints_and_headers(io.text_output_stream::in, globals::in,
    maybe_succeeded::in, module_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_ints_and_headers(ProgressStream, Globals, SubdirLinkSucceeded,
        ModuleName, Succeeded, !Info, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
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
            ExtExtDirs0 = []
        else
            ExtExtDirs0 = [{ext_cur_ngs(ext_cur_ngs_int_int0), "int0s"}]
        ),
        globals.get_any_intermod(Globals, AnyIntermod),
        (
            AnyIntermod = yes,
            ExtExtDirs1 =
                [{ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_opt_plain),
                "opts"} | ExtExtDirs0]
        ;
            AnyIntermod = no,
            ExtExtDirs1 = ExtExtDirs0
        ),
        ExtExtDirs = [{ext_cur_ngs(ext_cur_ngs_int_int1), "ints"},
            {ext_cur_ngs(ext_cur_ngs_int_int2), "int2s"},
            {ext_cur_ngs(ext_cur_ngs_int_int3), "int3s"},
            {ext_cur_ngs(ext_cur_ngs_misc_module_dep), "module_deps"}
            | ExtExtDirs1],
        globals.lookup_string_option(Globals, install_prefix, Prefix),
        LibDir = Prefix/"lib"/"mercury",
        list.map_foldl(
            install_subdir_file(ProgressStream, Globals, SubdirLinkSucceeded,
                LibDir/"ints", ModuleName),
            ExtExtDirs, Results, !IO),

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
            module_name_to_file_name(Globals, $pred, ext_cur(ext_cur_mh),
                ModuleName, FileName),
            install_file(ProgressStream, Globals, FileName, LibDir/"inc",
                HeaderSucceeded1, !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            install_subdir_file(ProgressStream, Globals, SubdirLinkSucceeded,
                LibDir/"ints", ModuleName, {ext_cur(ext_cur_mh), "mhs"},
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

:- pred install_extra_headers(io.text_output_stream::in, globals::in,
    maybe_succeeded::out, io::di, io::uo) is det.

install_extra_headers(ProgressStream, Globals, ExtraHdrsSucceeded, !IO) :-
    globals.lookup_accumulating_option(Globals, extra_library_header,
        ExtraHdrs),
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    IncDir = Prefix / "lib" / "mercury" / "inc",
    list.foldl2(install_extra_header(ProgressStream, Globals, IncDir),
        ExtraHdrs, succeeded, ExtraHdrsSucceeded, !IO).

:- pred install_extra_header(io.text_output_stream::in, globals::in,
    dir_name::in, string::in, maybe_succeeded::in, maybe_succeeded::out,
    io::di, io::uo) is det.

install_extra_header(ProgressStream, Globals, IncDir, FileName,
        !Succeeded, !IO) :-
    install_file(ProgressStream, Globals, FileName, IncDir,
        InstallSucceeded, !IO),
    !:Succeeded = !.Succeeded `and` InstallSucceeded.

install_library_grade(LinkSucceeded0, ModuleName, AllModules,
        ProgressStream, Globals, Grade, Succeeded, !Info, !IO) :-
    % Only remove grade-dependent files after installing if
    % --use-grade-subdirs is not specified by the user.
    globals.get_subdir_setting(Globals, SubdirSetting),
    (
        ( SubdirSetting = use_cur_dir
        ; SubdirSetting = use_cur_ngs_subdir
        ),
        CleanAfter = yes
    ;
        SubdirSetting = use_cur_ngs_gs_subdir,
        CleanAfter = no
    ),

    % Set up so that grade-dependent files for the current grade
    % don't overwrite the files for the default grade.
    OptionArgs0 = make_info_get_option_args(!.Info),
    OptionArgs = OptionArgs0 ++ ["--grade", Grade, "--use-grade-subdirs"],

    verbose_make_two_part_msg(Globals, "Installing grade", Grade, InstallMsg),
    maybe_write_msg(ProgressStream, InstallMsg, !IO),

    lookup_mmc_options(make_info_get_options_variables(!.Info), MaybeMCFlags),
    (
        MaybeMCFlags = ok1(MCFlags),
        get_default_options(Globals, DefaultOptionTable),
        DetectedGradeFlags = make_info_get_detected_grade_flags(!.Info),
        AllFlags = DetectedGradeFlags ++ MCFlags ++ OptionArgs,
        handle_given_options(ProgressStream, DefaultOptionTable, AllFlags,
            _, _, OptionsSpecs, LibGlobals, !IO)
    ;
        MaybeMCFlags = error1(LookupSpecs),
        write_error_specs(ProgressStream, Globals, LookupSpecs, !IO),
        % Errors should have been caught before.
        unexpected($pred, "bad DEFAULT_MCFLAGS")
    ),

    (
        OptionsSpecs = [_ | _],
        usage_errors(ProgressStream, Globals, OptionsSpecs, !IO),
        Succeeded = did_not_succeed
    ;
        OptionsSpecs = [],

        % Remove the grade-dependent targets from the status map
        % (we need to rebuild them in the new grade).
        %
        % NOTE This code was disabled from 2008 jul 14 until 2023 dec 12
        % due to a bug in version_hash_table.delete. That bug, which was
        % due to the holes left by deletes in open addressing probe sequences,
        % was fixed by switching to separate chaining on 2009 mar 26.
        % The replacement code was
        %
        %   StatusMap = version_hash_table.init_default(dependency_file_hash)
        %
        % NOTE that each delete made by remove_target_file_if_grade_dependent
        % will create a new version_hash_table, even though, with the exception
        % of the last one, none of them can never be referred to again.
        % It is not clear whether keeping all non-grade-dependent files'
        % statuses in the map makes paying this cost worthwhile.
        StatusMap0 = make_info_get_dep_file_status_map(!.Info),
        version_hash_table.fold(remove_target_file_if_grade_dependent,
            StatusMap0, StatusMap0, StatusMap),

        make_info_set_dep_file_status_map(StatusMap, !Info),
        make_info_set_option_args(OptionArgs, !Info),

        % Reset the target file timestamp cache, as the information it contains
        % is not valid for the changed grade and grade-subdir setting.
        make_info_set_target_file_timestamps(init_target_file_timestamps,
            !Info),

        % Building the library in the new grade is done in a separate process
        % to make it easier to stop and clean up on an interrupt.
        globals.lookup_bool_option(LibGlobals, very_verbose, VeryVerbose),
        setup_checking_for_interrupt(Cookie, !IO),
        call_in_forked_process(
            install_library_grade_2(ProgressStream, LibGlobals, LinkSucceeded0,
                ModuleName, AllModules, !.Info, CleanAfter),
            Succeeded0, !IO),
        Cleanup = maybe_make_grade_clean(ProgressStream, LibGlobals,
            CleanAfter, ModuleName, AllModules),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, Cleanup,
            Succeeded0, Succeeded, !Info, !IO)
    ).

:- pred install_library_grade_2(io.text_output_stream::in, globals::in,
    maybe_succeeded::in, module_name::in, list(module_name)::in, make_info::in,
    bool::in, maybe_succeeded::out, io::di, io::uo) is det.

install_library_grade_2(ProgressStream, Globals, LinkSucceeded0,
        ModuleName, AllModules, Info0, CleanAfter, Succeeded, !IO) :-
    make_misc_target(ProgressStream, Globals,
        ModuleName - misc_target_build_library, LibSucceeded,
        Info0, Info1, [], Specs, !IO),
    (
        LibSucceeded = succeeded,
        % `GradeDir' differs from `Grade' in that it is in canonical form.
        grade_directory_component(Globals, GradeDir),
        install_library_grade_files(ProgressStream, Globals, LinkSucceeded0,
            GradeDir, ModuleName, AllModules, Succeeded, Info1, Info2, !IO),
        maybe_make_grade_clean(ProgressStream, Globals, CleanAfter,
            ModuleName, AllModules, Info2, _Info, !IO)
    ;
        LibSucceeded = did_not_succeed,
        % XXX MAKE_STREAM
        io.output_stream(ErrorStream, !IO),
        write_error_specs(ErrorStream, Globals, Specs, !IO),
        Succeeded = did_not_succeed
    ).

    % Install the `.a', `.so', `.jar', `.opt' and `.mih' files for
    % the current grade.
    %
    % NOTE: changes here may require changes to
    %       file_util.get_install_name_option/4.
    %
:- pred install_library_grade_files(io.text_output_stream::in, globals::in,
    maybe_succeeded::in, string::in, module_name::in, list(module_name)::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

install_library_grade_files(ProgressStream, Globals, LinkSucceeded0, GradeDir,
        ModuleName, AllModules, Succeeded, !Info, !IO) :-
    make_grade_install_dirs(ProgressStream, Globals, GradeDir,
        DirResult, LinkSucceeded1, !IO),
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
            install_file(ProgressStream, Globals, DllFileName, GradeLibDir,
                LibsSucceeded, !IO),
            InitSucceeded = succeeded
        else if string.prefix(GradeDir, "java") then
            GradeLibDir = Prefix/"lib"/"mercury"/"lib"/GradeDir,
            install_file(ProgressStream, Globals, JarFileName, GradeLibDir,
                LibsSucceeded, !IO),
            InitSucceeded = succeeded
        else
            GradeLibDir = Prefix/"lib"/"mercury"/"lib"/GradeDir,
            maybe_install_library_file(ProgressStream, Globals, "static",
                LibFileName, GradeLibDir, LibSucceeded0, !IO),
            ( if LibFileName = SharedLibFileName then
                LibsSucceeded = LibSucceeded0
            else
                maybe_install_library_file(ProgressStream, Globals, "shared",
                    SharedLibFileName, GradeLibDir, SharedLibSucceeded, !IO),
                LibsSucceeded = LibSucceeded0 `and` SharedLibSucceeded
            ),
            install_grade_init(ProgressStream, Globals, GradeDir, ModuleName,
                InitSucceeded, !IO)
        ),

        list.map_foldl2(
            install_grade_ints_and_headers(ProgressStream, Globals,
                LinkSucceeded, GradeDir),
            AllModules, IntsHeadersSucceeded, !Info, !IO),
        Succeeded = and_list(
            [LibsSucceeded, InitSucceeded | IntsHeadersSucceeded])
    ;
        DirResult = did_not_succeed,
        Succeeded = did_not_succeed
    ).

    % Install the `.init' file for the current grade.
    %
:- pred install_grade_init(io.text_output_stream::in, globals::in,
    string::in, module_name::in, maybe_succeeded::out, io::di, io::uo) is det.

install_grade_init(ProgressStream, Globals, GradeDir, ModuleName,
        Succeeded, !IO) :-
    globals.lookup_string_option(Globals, install_prefix, Prefix),
    GradeModulesDir = Prefix / "lib" / "mercury" / "modules" / GradeDir,
    module_name_to_file_name(Globals, $pred, ext_cur_gs(ext_cur_gs_lib_init),
        ModuleName, InitFileName),
    install_file(ProgressStream, Globals, InitFileName, GradeModulesDir,
        Succeeded, !IO).

    % Install the `.opt', `.analysis' and `.mih' files for the current grade.
    %
:- pred install_grade_ints_and_headers(io.text_output_stream::in, globals::in,
    maybe_succeeded::in, string::in, module_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_grade_ints_and_headers(ProgressStream, Globals, LinkSucceeded,
        GradeDir, ModuleName, Succeeded, !Info, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
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
            install_subdir_file(ProgressStream, Globals, LinkSucceeded,
                GradeIncDir, ModuleName,
                {ext_cur_ngs_gs_max_cur(ext_cur_ngs_gs_max_cur_mih), "mihs"},
                HeaderSucceeded1, !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            IntDir = LibDir/"ints",
            install_subdir_file(ProgressStream, Globals, LinkSucceeded,
                IntDir, ModuleName,
                {ext_cur_ngs_gs_max_cur(ext_cur_ngs_gs_max_cur_mih), "mihs"},
                HeaderSucceeded2, !IO),
            HeaderSucceeded = HeaderSucceeded1 `and` HeaderSucceeded2
        else
            HeaderSucceeded = succeeded
        ),

        GradeIntDir = LibDir/"ints"/GradeDir,
        globals.get_any_intermod(Globals, AnyIntermod),
        (
            AnyIntermod = yes,
            install_subdir_file(ProgressStream, Globals, LinkSucceeded,
                GradeIntDir, ModuleName,
                {ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_opt_plain),
                    "opts"},
                OptSucceeded, !IO)
        ;
            AnyIntermod = no,
            OptSucceeded = succeeded
        ),
        globals.lookup_bool_option(Globals, intermodule_analysis,
            IntermodAnalysis),
        (
            IntermodAnalysis = yes,
            install_subdir_file(ProgressStream, Globals, LinkSucceeded,
                GradeIntDir, ModuleName,
                {ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_analysis),
                    "analyses"},
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
:- pred install_subdir_file(io.text_output_stream::in, globals::in,
    maybe_succeeded::in, dir_name::in, module_name::in, {ext, string}::in,
    maybe_succeeded::out, io::di, io::uo) is det.

install_subdir_file(ProgressStream, Globals, SubdirLinkSucceeded, InstallDir,
        ModuleName, {Ext, ExtDir}, Succeeded, !IO) :-
    module_name_to_file_name(Globals, $pred, Ext, ModuleName, FileName),
    install_file(ProgressStream, Globals, FileName, InstallDir,
        Succeeded1, !IO),
    (
        SubdirLinkSucceeded = did_not_succeed,
        install_file(ProgressStream, Globals, FileName,
            InstallDir/"Mercury"/ExtDir, Succeeded2, !IO),
        Succeeded = Succeeded1 `and` Succeeded2
    ;
        SubdirLinkSucceeded = succeeded,
        Succeeded = Succeeded1
    ).

:- pred maybe_install_library_file(io.text_output_stream::in, globals::in,
    string::in, file_name::in, dir_name::in, maybe_succeeded::out,
    io::di, io::uo) is det.

maybe_install_library_file(ProgressStream, Globals, Linkage,
        FileName, InstallDir, Succeeded, !IO) :-
    globals.lookup_accumulating_option(Globals, lib_linkages, LibLinkages),
    ( if list.member(Linkage, LibLinkages) then
        install_file(ProgressStream, Globals, FileName, InstallDir,
            Succeeded0, !IO),

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
            generate_archive_index(ProgressStream, Globals, BaseFileName,
                InstallDir, Succeeded, !IO)
        else
            Succeeded = Succeeded0
        )
    else
        Succeeded = succeeded
    ).

:- pred install_file(io.text_output_stream::in, globals::in,
    file_name::in, dir_name::in, maybe_succeeded::out, io::di, io::uo) is det.

install_file(ProgressStream, Globals, FileName, InstallDir, Succeeded, !IO) :-
    % XXX MAKE_STREAM
    OutputStream = ProgressStream,
    verbose_make_four_part_msg(Globals, "Installing file", FileName,
        "in", InstallDir, InstallMsg),
    maybe_write_msg(ProgressStream, InstallMsg, !IO),
    Command = make_install_file_command(Globals, FileName, InstallDir),
    invoke_system_command(Globals, ProgressStream, OutputStream,
        cmd_verbose, Command, Succeeded, !IO).

:- pred install_directory(io.text_output_stream::in, globals::in,
    dir_name::in, dir_name::in, maybe_succeeded::out, io::di, io::uo) is det.
:- pragma consider_used(pred(install_directory/7)).

install_directory(ProgressStream, Globals, SourceDirName, InstallDir,
        Succeeded, !IO) :-
    % XXX MAKE_STREAM
    OutputStream = ProgressStream,
    verbose_make_four_part_msg(Globals, "Installing directory", SourceDirName,
        "in", InstallDir, InstallMsg),
    maybe_write_msg(ProgressStream, InstallMsg, !IO),
    Command = make_install_dir_command(Globals, SourceDirName, InstallDir),
    invoke_system_command(Globals, ProgressStream, OutputStream,
        cmd_verbose, Command, Succeeded, !IO).

:- pred make_install_dirs(io.text_output_stream::in, globals::in,
    maybe_succeeded::out, maybe_succeeded::out, io::di, io::uo) is det.

make_install_dirs(ProgressStream, Globals, Result, LinkResult, !IO) :-
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
    print_mkdir_errors(ProgressStream, Results, Result, !IO).

:- pred make_grade_install_dirs(io.text_output_stream::in, globals::in,
    string::in, maybe_succeeded::out, maybe_succeeded::out,
    io::di, io::uo) is det.

make_grade_install_dirs(ProgressStream, Globals, Grade,
        Result, LinkResult, !IO) :-
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
        make_directory(GradeIntsSubdir/"analyses", Result7, !IO),
        Results = [Result4, Result5, Result6, Result7 | Results0]
    ),
    print_mkdir_errors(ProgressStream, Results, Result, !IO).

:- pred print_mkdir_errors(io.text_output_stream::in, list(io.res)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

print_mkdir_errors(_ProgressStream, [], succeeded, !IO).
print_mkdir_errors(ProgressStream, [Result | Results], Succeeded, !IO) :-
    (
        Result = ok,
        print_mkdir_errors(ProgressStream, Results, Succeeded, !IO)
    ;
        Result = error(Error),
        io.format(ProgressStream,
            "Error creating installation directories: %s\n",
            [s(io.error_message(Error))], !IO),
        print_mkdir_errors(ProgressStream, Results, _, !IO),
        Succeeded = did_not_succeed
    ).

:- pred make_install_symlink(globals::in, string::in, string::in,
    maybe_succeeded::out, io::di, io::uo) is det.

make_install_symlink(Globals, Subdir, Ext, Succeeded, !IO) :-
    LinkName = Subdir/(Ext ++ "s"),
    maybe_make_symlink(Globals, "..", LinkName, Succeeded, !IO).

    % Generate (or update) the index for an archive file,
    % i.e. run ranlib on a .a file.
    %
:- pred generate_archive_index(io.text_output_stream::in, globals::in,
    file_name::in, dir_name::in, maybe_succeeded::out, io::di, io::uo) is det.

generate_archive_index(ProgressStream, Globals, FileName, InstallDir,
        Succeeded, !IO) :-
    verbose_make_four_part_msg(Globals, "Generating archive index for file",
         FileName, "in", InstallDir, InstallMsg),
    maybe_write_msg(ProgressStream, InstallMsg, !IO),
    globals.lookup_string_option(Globals, ranlib_command, RanLibCommand),
    globals.lookup_string_option(Globals, ranlib_flags, RanLibFlags),
    % XXX What is the point of using more than one space?
    Command = string.join_list("    ", [
        quote_shell_cmd_arg(RanLibCommand),
        RanLibFlags,
        quote_shell_cmd_arg(InstallDir / FileName)
    ]),
    % XXX MAKE_STREAM
    CmdOutputStream = ProgressStream,
    invoke_system_command(Globals, ProgressStream,
        CmdOutputStream, cmd_verbose, Command, Succeeded, !IO).

%---------------------%

:- pred remove_target_file_if_grade_dependent(dependency_file::in,
    dependency_status::in,
    version_hash_table(dependency_file, dependency_status)::in,
    version_hash_table(dependency_file, dependency_status)::out) is det.

remove_target_file_if_grade_dependent(File, _Status, !StatusMap) :-
    ( if
        File = dep_target(target_file(_, TargetType)),
        target_is_grade_or_arch_dependent(TargetType)
    then
        version_hash_table.delete(File, !StatusMap)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred maybe_make_grade_clean(io.text_output_stream::in, globals::in,
    bool::in, module_name::in, list(module_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

maybe_make_grade_clean(ProgressStream, Globals, Clean, ModuleName, AllModules,
        !Info, !IO) :-
    (
        Clean = yes,
        make_grade_clean(ProgressStream, Globals, ModuleName, AllModules,
            !Info, !IO)
    ;
        Clean = no
    ).

    % Clean up grade-dependent files.
    %
:- pred make_grade_clean(io.text_output_stream::in, globals::in,
    module_name::in, list(module_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_grade_clean(ProgressStream, Globals, ModuleName, AllModules,
        !Info, !IO) :-
    grade_directory_component(Globals, Grade),
    % XXX MAKE_EXTRA_PERIOD
    string.format("Cleaning up grade-dependent files for `%s' in grade %s.",
        [s(escaped_sym_name_to_string(ModuleName)), s(Grade)], Part1),
    verbose_make_one_part_msg(Globals, Part1, CleaningMsg),
    maybe_write_msg(ProgressStream, CleaningMsg, !IO),

    make_main_module_realclean(ProgressStream, Globals,
        ModuleName, !Info, !IO),
    list.foldl2(make_module_clean(ProgressStream, Globals),
        AllModules, !Info, !IO).

:- pred make_main_module_realclean(io.text_output_stream::in, globals::in,
    module_name::in, make_info::in, make_info::out, io::di, io::uo) is det.

make_main_module_realclean(ProgressStream, Globals, ModuleName, !Info, !IO) :-
    % XXX MAKE_EXTRA_PERIOD
    string.format("Removing executable and library files for `%s'.",
        [s(escaped_sym_name_to_string(ModuleName))], Part1),
    verbose_make_one_part_msg(Globals, Part1, CleaningMsg),
    maybe_write_msg(ProgressStream, CleaningMsg, !IO),

    LinkedTargetTypes = [
        executable,
        static_library,
        shared_library,
        csharp_executable,
        csharp_library,
        java_executable,
        java_archive
    ],
    list.map2_foldl(linked_target_file_name_full_curdir(Globals, ModuleName),
        LinkedTargetTypes, FileNames, ThisDirFileNames, !IO),
    % Remove the symlinks created for `--use-grade-subdirs'.
    % XXX This symlink should not be necessary anymore for `mmc --make'.
    module_name_to_file_name_full_curdir(Globals, $pred,
        ext_cur_gs(ext_cur_gs_lib_init), ModuleName,
        FullInitFileName, ThisDirInitFileName),
    FilesToRemove = FileNames ++ ThisDirFileNames ++
        [FullInitFileName, ThisDirInitFileName],
    list.foldl2(remove_file_for_make(ProgressStream, Globals, very_verbose),
        FilesToRemove, !Info, !IO),
    remove_init_files(ProgressStream, Globals, very_verbose, ModuleName,
        !Info, !IO).

:- pred remove_init_files(io.text_output_stream::in, globals::in, option::in,
    module_name::in, make_info::in, make_info::out, io::di, io::uo) is det.

remove_init_files(ProgressStream, Globals, Verbose, ModuleName, !Info, !IO) :-
    remove_module_file_for_make(ProgressStream, Globals, Verbose, ModuleName,
        ext_cur_ngs_gs(ext_cur_ngs_gs_init_c), !Info, !IO),
    remove_module_file_for_make(ProgressStream, Globals, Verbose, ModuleName,
        ext_cur_ngs_gs(ext_cur_ngs_gs_init_obj_obj_opt), !Info, !IO),
    remove_module_file_for_make(ProgressStream, Globals, Verbose, ModuleName,
        ext_cur_ngs_gs(ext_cur_ngs_gs_init_obj_pic_obj_opt), !Info, !IO).

%---------------------------------------------------------------------------%

:- pred make_module_clean(io.text_output_stream::in, globals::in,
    module_name::in, make_info::in, make_info::out, io::di, io::uo) is det.

make_module_clean(ProgressStream, Globals, ModuleName, !Info, !IO) :-
    % XXX MAKE_EXTRA_PERIOD
    string.format("Cleaning up target files for module `%s'.",
        [s(escaped_sym_name_to_string(ModuleName))], Part1),
    verbose_make_one_part_msg(Globals, Part1, CleaningMsg),
    maybe_write_msg(ProgressStream, CleaningMsg, !IO),

    list.foldl2(
        remove_make_target_file_by_name(ProgressStream, Globals, $pred,
            very_verbose, ModuleName),
        [module_target_errors,
        module_target_c_code,
        module_target_c_header(header_mih),
        module_target_csharp_code,
        module_target_java_code,
        module_target_java_class_code], !Info, !IO),

    remove_module_file_for_make(ProgressStream, Globals, very_verbose,
        ModuleName, ext_cur_ngs_gs(ext_cur_ngs_gs_misc_used), !Info, !IO),
    remove_module_file_for_make(ProgressStream, Globals, very_verbose,
        ModuleName, ext_cur_ngs(ext_cur_ngs_misc_prof), !Info, !IO),

    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_fact_tables(ModuleDepInfo, FactTableFilesSet),
        set.to_sorted_list(FactTableFilesSet, FactTableFiles)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        FactTableFiles = []
    ),

    list.foldl2(remove_fact_table_c_file(ProgressStream, Globals),
        FactTableFiles, !Info, !IO),

    foreign_language_module_name(ModuleName, lang_c, CCodeModule),
    remove_make_target_file_by_name(ProgressStream, Globals, $pred,
        very_verbose, CCodeModule, module_target_c_code, !Info, !IO),

    remove_object_and_assembler_files(ProgressStream, Globals,
        ModuleName, pic, FactTableFiles, !Info, !IO),
    remove_object_and_assembler_files(ProgressStream, Globals,
        ModuleName, non_pic, FactTableFiles, !Info, !IO).

:- pred remove_fact_table_c_file(io.text_output_stream::in, globals::in,
    string::in, make_info::in, make_info::out, io::di, io::uo) is det.

remove_fact_table_c_file(ProgressStream, Globals, FactTableFile, !Info, !IO) :-
    fact_table_file_name_return_dirs(Globals, $pred,
        ext_cur_ngs_gs(ext_cur_ngs_gs_target_c),
        FactTableFile, _FactTableDirs, FactTableCFile),
    remove_file_for_make(ProgressStream, Globals, very_verbose,
        FactTableCFile, !Info, !IO).

:- pred remove_object_and_assembler_files(io.text_output_stream::in,
    globals::in, module_name::in, pic::in, list(file_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_object_and_assembler_files(ProgressStream, Globals, ModuleName, PIC,
        FactTableFiles, !Info, !IO) :-
    remove_make_target_file_by_name(ProgressStream, Globals, $pred,
        very_verbose, ModuleName, module_target_object_code(PIC), !Info, !IO),
    list.foldl2(
        remove_fact_table_object_and_assembler_files(ProgressStream, Globals,
            ModuleName, PIC),
        FactTableFiles, !Info, !IO).

:- pred remove_fact_table_object_and_assembler_files(io.text_output_stream::in,
    globals::in, module_name::in, pic::in, file_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_fact_table_object_and_assembler_files(ProgressStream, Globals,
        ModuleName, PIC, FactTableFile, !Info, !IO) :-
    remove_make_target_file_by_name(ProgressStream, Globals, $pred,
        very_verbose, ModuleName,
        module_target_fact_table_object(PIC, FactTableFile), !Info, !IO).

%---------------------------------------------------------------------------%

:- pred make_module_realclean(io.text_output_stream::in, globals::in,
    module_name::in, make_info::in, make_info::out, io::di, io::uo) is det.

make_module_realclean(ProgressStream, Globals, ModuleName, !Info, !IO) :-
    make_module_clean(ProgressStream, Globals, ModuleName, !Info, !IO),

    string.format("Cleaning up interface files for module `%s'",
        [s(escaped_sym_name_to_string(ModuleName))], Part1),
    verbose_make_one_part_msg(Globals, Part1, CleaningMsg),
    maybe_write_msg(ProgressStream, CleaningMsg, !IO),
    Targets = [module_target_int0, module_target_int1, module_target_int2,
        module_target_int3, module_target_opt,
        module_target_analysis_registry,
        module_target_c_header(header_mh),
        module_target_track_flags],
    list.foldl2(
        remove_make_target_file_by_name(ProgressStream, Globals, $pred,
            very_verbose, ModuleName),
        Targets, !Info, !IO),
    remove_module_file_for_make(ProgressStream, Globals, very_verbose,
        ModuleName, ext_cur_ngs(ext_cur_ngs_misc_module_dep),
        !Info, !IO),
    remove_module_file_for_make(ProgressStream, Globals, very_verbose,
        ModuleName, ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_imdg),
        !Info, !IO),
    remove_module_file_for_make(ProgressStream, Globals, very_verbose,
        ModuleName, ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_request),
        !Info, !IO).

%---------------------------------------------------------------------------%

:- pred collect_nested_modules(io.text_output_stream::in, globals::in,
    module_name::in, set(module_name)::in, set(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

collect_nested_modules(ProgressStream, Globals, ModuleName,
        !NestedModules, !Info, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_maybe_top_module(ModuleDepInfo, MaybeTopModule),
        NestedSubModules = get_nested_children_of_top_module(MaybeTopModule),
        set.union(NestedSubModules, !NestedModules)
    ;
        MaybeModuleDepInfo = no_module_dep_info
    ).

%---------------------------------------------------------------------------%

:- pred get_nonnested_and_parent_modules(io.text_output_stream::in,
    globals::in, list(module_name)::in,
    list(module_name)::out, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_nonnested_and_parent_modules(ProgressStream, Globals, ModuleNames,
        NonnestedModules, ParentModules, !Info, !IO) :-
    list.foldl4(
        acc_nonnested_and_parent_modules(ProgressStream, Globals),
        ModuleNames,
        [], NonnestedModules, [], ParentModules, !Info, !IO).

:- pred acc_nonnested_and_parent_modules(io.text_output_stream::in,
    globals::in, module_name::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

acc_nonnested_and_parent_modules(ProgressStream, Globals, ModuleName,
        !NonnestedModules, !ParentModules, !Info, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_maybe_top_module(ModuleDepInfo, MaybeTopModule),
        (
            MaybeTopModule = top_module(_NestedSubModules),
            % don't include in NestedModules
            %   which means DO include in NonnestedModules
            !:NonnestedModules = [ModuleName | !.NonnestedModules],
            module_dep_info_get_children(ModuleDepInfo, Children),
            ( if set.is_empty(Children) then
                true
            else
                !:ParentModules = [ModuleName | !.ParentModules]
            )
        ;
            MaybeTopModule = not_top_module
            % do include in NestedModules
            %   which means DO NOT include in NonnestedModules
            %   which means DO NOT include in ParentModules
        )
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        % don't include in NestedModules
        %   which means DO include in NonnestedModules
        % do not include in ParentModules
        %   due to absence of info about any children
        !:NonnestedModules = [ModuleName | !.NonnestedModules]
    ).

%---------------------------------------------------------------------------%

    % Find all modules in the current directory which are reachable (by import)
    % from the given module. Return a list of `--local-module-id' options
    % suitable for the command line.
    %
:- pred make_local_module_id_options(io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out, list(string)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_local_module_id_options(ProgressStream, Globals, ModuleName,
        Succeeded, Options, !Info, !IO) :-
    find_reachable_local_modules(ProgressStream, Globals, ModuleName,
        Succeeded, LocalModules, !Info, !IO),
    set.fold(make_local_module_id_option, LocalModules, [], Options).

:- pred make_local_module_id_option(module_name::in, list(string)::in,
    list(string)::out) is det.

make_local_module_id_option(ModuleName, Opts0, Opts) :-
    ModuleNameStr = sym_name_to_string(ModuleName),
    Opts = ["--local-module-id", ModuleNameStr | Opts0].

%---------------------------------------------------------------------------%
:- end_module make.program_target.
%---------------------------------------------------------------------------%
