%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2025 The Mercury team.
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module analysis.framework.
:- import_module analysis.operations.
:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module backend_libs.link_target_code.
:- import_module libs.check_libgrades.
:- import_module libs.file_util.
:- import_module libs.options.
:- import_module libs.process_util.
:- import_module libs.system_cmds.
:- import_module libs.timestamp.
:- import_module make.build.
:- import_module make.check_up_to_date.
:- import_module make.clean.
:- import_module make.find_local_modules.
:- import_module make.get_module_dep_info.
:- import_module make.library_install.
:- import_module make.module_target.
:- import_module make.options_file.
:- import_module make.timestamp.
:- import_module make.util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.find_module.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.module_deps_graph.
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
    LinkedTargetFile = linked_target_file(_MainModuleName, LinkedTargetType),
    (
        LinkedTargetType = shared_library,
        ExtraOptions = ["--compile-to-shared-lib"]
    ;
        ( LinkedTargetType = executable
        ; LinkedTargetType = static_library
        ; LinkedTargetType = csharp_executable
        ; LinkedTargetType = csharp_library
        ; LinkedTargetType = java_executable
        ; LinkedTargetType = java_archive
        ),
        ExtraOptions = []
    ),
    globals.get_library_install_linkages(Globals, LibraryInstallLinkages),
    ( if
        (
            LinkedTargetType = static_library,
            not set.member(sos_static, LibraryInstallLinkages)
        ;
            LinkedTargetType = shared_library,
            not set.member(sos_shared, LibraryInstallLinkages)
        )
    then
        % XXX What is the justification for this?
        LinkedTargetSucceeded = succeeded
    else
        maybe_check_libraries_are_installed(Globals, LibgradeCheckSpecs, !IO),
        (
            LibgradeCheckSpecs = [],
            maybe_with_analysis_cache_dir_3(ProgressStream, Globals,
                make_linked_target_1(Globals, LinkedTargetFile, ExtraOptions),
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
    LinkedTargetFile = linked_target_file(MainModuleName, _LinkedTargetType),

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
        MaybeStdLibGrades = make_info_get_maybe_stdlib_grades(!.Info),
        EnvOptFileVariables = make_info_get_env_optfile_variables(!.Info),
        EnvVarArgs = make_info_get_env_var_args(!.Info),
        OptionArgs = make_info_get_option_args(!.Info),
        setup_for_build_with_module_options(ProgressStream, DefaultOptionTable,
            MaybeStdLibGrades, invoked_by_mmc_make, MainModuleName,
            EnvOptFileVariables, EnvVarArgs, OptionArgs, ExtraOptions,
            MayBuild, !IO),
        (
            MayBuild = may_build(_AllOptionArgs, BuildGlobals),
            make_linked_target_2(ProgressStream, BuildGlobals,
                LinkedTargetFile, Succeeded, !Info, !IO)
        ;
            MayBuild = may_not_build(Specs),
            % The errors we can get here report problems we encounter
            % while trying to set up to compile a module, and not specific
            % to the Mercury module itself, except insofar as the problem
            % may be caused by e.g. an unrecognized option name in a
            % module-specific MCFLAGS make variable.
            write_error_specs(ProgressStream, Globals, Specs, !IO),
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
    LinkedTargetFile = linked_target_file(MainModuleName, LinkedTargetType),
    find_reachable_local_modules(ProgressStream, Globals,
        MainModuleName, DepsSucceeded, AllModules, !Info, !IO),
    KeepGoing = make_info_get_keep_going(!.Info),
    ( if
        DepsSucceeded = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        Succeeded = did_not_succeed
    else
        get_object_code_type(Globals, LinkedTargetType, PIC),

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
                (
                    ObjectTargetType = module_target_java_class_code,
                    make_java_files(ProgressStream, Globals,
                        MainModuleName, ObjModules, BuildJavaSucceeded,
                        !Info, !IO),
                    (
                        BuildJavaSucceeded = succeeded,
                        % Disable the `--rebuild' option during this pass,
                        % otherwise all the Java classes will be built again.
                        globals.set_option(part_opmode_rebuild, bool(no),
                            Globals, NoRebuildGlobals),
                        foldl2_make_module_targets_maybe_parallel(KeepGoing,
                            [], ProgressStream, NoRebuildGlobals, ObjTargets,
                            BuildDepsSucceeded1, !Info, !IO)
                    ;
                        BuildJavaSucceeded = did_not_succeed,
                        BuildDepsSucceeded1 = did_not_succeed
                    )
                ;
                    ( ObjectTargetType = module_target_object_code(_)
                    ; ObjectTargetType = module_target_csharp_code
                    ),
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

        linked_target_file_name_full_curdir(Globals,
            MainModuleName, LinkedTargetType,
            FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName, !IO),
        get_file_timestamp(search_auth_cur_dir, FullMainModuleLinkedFileName,
            _SearchDirs, MaybeTimestamp, !Info, !IO),
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
            open_module_error_stream(ProgressStream, Globals, !.Info,
                MainModuleName, MaybeErrorStream, !IO),
            (
                MaybeErrorStream = es_error_already_reported,
                Succeeded0 = did_not_succeed
            ;
                MaybeErrorStream = es_ok(MESI, ErrorStream),
                build_linked_target(ProgressStream, Globals,
                    MainModuleName, LinkedTargetType,
                    FullMainModuleLinkedFileName,
                    CurDirMainModuleLinkedFileName, MaybeOldestLhsTimestamp,
                    AllModules, ObjModules, CompilationTarget, PIC,
                    ShouldRebuildLhs, Succeeded0, !Info, !IO),
                close_module_error_stream_handle_errors(ProgressStream,
                    Globals, MESI, ErrorStream, !.Info, !IO)
            ),
            CleanupPred = linked_target_cleanup(ProgressStream, Globals,
                MainModuleName, LinkedTargetType,
                FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName),
            teardown_checking_for_interrupt(VeryVerbose, Cookie, CleanupPred,
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
    Target = target_file(Module, module_target_source),
    get_target_timestamp(ProgressStream, Globals, Target, MaybeTimestamp,
        !Info, !IO),
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

build_linked_target(ProgressStream, Globals, MainModuleName, LinkedTargetType,
        FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
        MaybeOldestLhsTimestamp, AllModules, ObjModules,
        CompilationTarget, PIC, ShouldRebuildLhs, Succeeded, !Info, !IO) :-
    globals.lookup_maybe_string_option(Globals, make_pre_link_command,
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
            MainModuleName, LinkedTargetType,
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

build_linked_target_2(ProgressStream, Globals0, MainModuleName,
        LinkedTargetType,
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
        LinkedTargetType = executable,
        make_init_obj_file_check_result(ProgressStream, NoLinkObjsGlobals,
            MainModuleName, AllModulesList, InitObjSucceeded, InitObjects,
            !Info, !IO)
    ;
        ( LinkedTargetType = static_library
        ; LinkedTargetType = shared_library
        ; LinkedTargetType = csharp_executable
        ; LinkedTargetType = csharp_library
        ; LinkedTargetType = java_executable
        ; LinkedTargetType = java_archive
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

    list.map2_foldl2(get_file_timestamp(search_auth_cur_dir),
        ObjectsToCheck, _SearchDirs, ExtraObjectTimestamps, !Info, !IO),
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
                NoLinkObjsGlobals, MainModuleName, LinkedTargetType,
                FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
                Succeeded, !Info, !IO)
        else
            rebuild_linked_target(ProgressStream, NoLinkObjsGlobals,
                MainModuleName, LinkedTargetType, FullMainModuleLinkedFileName,
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
    globals.lookup_bool_option(NoLinkObjsGlobals, part_opmode_rebuild,
        MustRecompile),
    make_init_obj_file(ProgressStream, NoLinkObjsGlobals, MustRecompile,
        MainModuleName, AllModulesList, InitObjectResult, !IO),
    (
        InitObjectResult = yes(InitObject),
        % We may need to update the timestamp of the `_init.o' file.
        FileTimestampMap0 = make_info_get_file_timestamp_map(!.Info),
        map.delete(InitObject, FileTimestampMap0, FileTimestampMap1),
        make_info_set_file_timestamp_map(FileTimestampMap1, !Info),
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
        NoLinkObjsGlobals, MainModuleName, LinkedTargetType,
        FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
        Succeeded, !Info, !IO) :-
    MainModuleLinkedTarget =
        top_target_file(MainModuleName, linked_target(LinkedTargetType)),
    ( if FullMainModuleLinkedFileName = CurDirMainModuleLinkedFileName then
        maybe_warn_up_to_date_target_msg(NoLinkObjsGlobals,
            MainModuleLinkedTarget, FullMainModuleLinkedFileName, !Info,
            UpToDateMsg),
        maybe_write_msg(ProgressStream, UpToDateMsg, !IO),
        Succeeded = succeeded
    else
        post_link_maybe_make_symlink_or_copy(NoLinkObjsGlobals, ProgressStream,
            FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
            MainModuleName, LinkedTargetType, Succeeded,
            MadeSymlinkOrCopy, !IO),
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
        MainModuleName, LinkedTargetType, FullMainModuleLinkedFileName,
        AllModulesList, ObjModules, InitObjectFileNames, LinkObjectFileNames,
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
        AllModulesList, ForeignObjectFileNameLists, !Info, !IO),
    ForeignObjectFileNames = list.condense(ForeignObjectFileNameLists),

    (
        CompilationTarget = target_c,
        maybe_pic_object_file_extension(PIC, ObjExt, _),
        Ext = ext_cur_ngs_gas(ObjExt)
    ;
        CompilationTarget = target_csharp,
        % There is no separate object code step.
        Ext = ext_cur_ngs_gs(ext_cur_ngs_gs_target_cs)
    ;
        CompilationTarget = target_java,
        Ext = ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_class)
    ),
    % XXX LEGACY
    list.map2(module_name_to_file_name(NoLinkObjsGlobals, $pred, Ext),
        ObjModules, ModuleObjFileNames, _ModuleObjFileNamesProposed),

    % LinkObjectFileNames may contain `.a' files which must come
    % after all the object files on the linker command line.
    AllObjects = InitObjectFileNames ++ ModuleObjFileNames ++
        ForeignObjectFileNames ++ LinkObjectFileNames,
    (
        ( CompilationTarget = target_c
        ; CompilationTarget = target_java
        ; CompilationTarget = target_csharp
        ),
        % Run the link in a separate process so it can be killed
        % if an interrupt is received.
        call_in_forked_process(
            link_and_write_error_specs(NoLinkObjsGlobals, ProgressStream,
                LinkedTargetType, MainModuleName, AllObjects),
            Succeeded, !IO)
    ),
    CmdLineTargets0 = make_info_get_command_line_targets(!.Info),
    set.delete(
        top_target_file(MainModuleName, linked_target(LinkedTargetType)),
        CmdLineTargets0, CmdLineTargets),
    make_info_set_command_line_targets(CmdLineTargets, !Info),
    (
        Succeeded = succeeded,
        FileTimestampMap0 = make_info_get_file_timestamp_map(!.Info),
        map.delete(FullMainModuleLinkedFileName,
            FileTimestampMap0, FileTimestampMap),
        make_info_set_file_timestamp_map(FileTimestampMap, !Info)
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

:- pred link_and_write_error_specs(globals::in, io.text_output_stream::in,
    linked_target_type::in, module_name::in, list(string)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

link_and_write_error_specs(Globals, ProgressStream,
        LinkTargetType, ModuleName, ObjectsList, Succeeded, !IO) :-
    link_files_into_executable_or_library_for_c_cs_java(ProgressStream,
        Globals, LinkTargetType, ModuleName, ObjectsList,
        Specs, Succeeded, !IO),
    % The errors we can get here are not specific to any Mercury module.
    write_error_specs(ProgressStream, Globals, Specs, !IO).

:- pred linked_target_cleanup(io.text_output_stream::in, globals::in,
    module_name::in, linked_target_type::in, file_name::in, file_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

linked_target_cleanup(ProgressStream, Globals,
        MainModuleName, LinkedTargetType,
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
        LinkedTargetType = executable,
        remove_init_files(ProgressStream, Globals, verbose_make,
            MainModuleName, !Info, !IO)
    ;
        ( LinkedTargetType = static_library
        ; LinkedTargetType = shared_library
        ; LinkedTargetType = csharp_executable
        ; LinkedTargetType = csharp_library
        ; LinkedTargetType = java_executable
        ; LinkedTargetType = java_archive
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
    find_out_of_date_java_modules(ProgressStream, Globals,
        ObjModules, OutOfDateModules, !Info, !IO),
    (
        OutOfDateModules = [],
        Succeeded = succeeded
    ;
        OutOfDateModules = [_ | _],
        build_java_files(ProgressStream, Globals,
            MainModuleName, OutOfDateModules, Succeeded, !.Info, !IO),
        % javac might write more `.class' files than we anticipated (though
        % it probably won't) so clear out all the timestamps which might be
        % affected.
        TimestampMap0 = make_info_get_file_timestamp_map(!.Info),
        map.foldl(reinsert_timestamps_for_non_class_files, TimestampMap0,
            map.init, TimestampMap),
        make_info_set_file_timestamp_map(TimestampMap, !Info),
        % For simplicity, clear out all target file timestamps.
        make_info_set_target_file_timestamp_map(init_target_file_timestamp_map,
            !Info)
    ).

:- pred find_out_of_date_java_modules(io.text_output_stream::in, globals::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_out_of_date_java_modules(ProgressStream, Globals,
        ObjModules, OutOfDateModules, !Info, !IO) :-
    (
        ObjModules = [],
        OutOfDateModules = []
    ;
        ObjModules = [ModuleName | ModuleNames],
        find_out_of_date_java_modules(ProgressStream, Globals,
            ModuleNames, OutOfDateModules0, !Info, !IO),
        JavaTarget = target_file(ModuleName, module_target_java_code),
        ClassTarget = target_file(ModuleName, module_target_java_class_code),
        get_target_timestamp(ProgressStream, Globals,
            JavaTarget, MaybeJavaTimestamp, !Info, !IO),
        get_target_timestamp(ProgressStream, Globals,
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
    make_info::in, io::di, io::uo) is det.

build_java_files(ProgressStream, Globals, MainModuleName, ModuleNames,
        Succeeded, Info, !IO) :-
    verbose_make_one_part_msg(Globals, "Making Java class files", MakingMsg),
    maybe_write_msg(ProgressStream, MakingMsg, !IO),
    % XXX FILE_NAMES
    % XXX LEGACY
    list.map2_foldl(
        module_name_to_file_name_create_dirs(Globals, $pred,
            ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java)),
        ModuleNames, JavaFiles, _JavaFilesProposed, !IO),
    % We redirect errors to a file named after the main module.
    open_module_error_stream(ProgressStream, Globals, Info, MainModuleName,
        MaybeErrorStream, !IO),
    (
        MaybeErrorStream = es_error_already_reported,
        Succeeded = did_not_succeed
    ;
        MaybeErrorStream = es_ok(MESI, ErrorStream),
        build_java_files_2(ProgressStream, Globals, JavaFiles, Succeeded, !IO),
        close_module_error_stream_handle_errors(ProgressStream, Globals,
            MESI, ErrorStream, Info, !IO)
    ).

:- pred build_java_files_2(io.text_output_stream::in, globals::in,
    list(string)::in, maybe_succeeded::out, io::di, io::uo) is det.

build_java_files_2(ProgressStream, Globals, JavaFiles, Succeeded, !IO) :-
    list.det_head_tail(JavaFiles, HeadJavaFile, TailJavaFiles),
    call_in_forked_process(
        compile_java_files(Globals, ProgressStream,
            HeadJavaFile, TailJavaFiles),
        Succeeded, !IO).

:- pred reinsert_timestamps_for_non_class_files(string::in,
    {list(dir_name), maybe_error(timestamp)}::in,
    file_timestamp_map::in, file_timestamp_map::out) is det.

reinsert_timestamps_for_non_class_files(FileName, DirNamesMaybeTimestamp,
        !TimestampMap) :-
    ( if string.suffix(FileName, ".class") then
        true
    else
        map.det_insert(FileName, DirNamesMaybeTimestamp, !TimestampMap)
    ).

%---------------------------------------------------------------------------%

make_misc_target(ProgressStream, Globals, MainModuleName - TargetType,
        Succeeded, !Info, !Specs, !IO) :-
    get_default_options(Globals, DefaultOptionTable),
    MaybeStdLibGrades = make_info_get_maybe_stdlib_grades(!.Info),
    EnvOptFileVariables = make_info_get_env_optfile_variables(!.Info),
    EnvVarArgs = make_info_get_env_var_args(!.Info),
    OptionArgs = make_info_get_option_args(!.Info),
    ExtraOptions = [],
    setup_for_build_with_module_options(ProgressStream, DefaultOptionTable,
        MaybeStdLibGrades, invoked_by_mmc_make, MainModuleName,
        EnvOptFileVariables, EnvVarArgs, OptionArgs, ExtraOptions,
        MayBuild, !IO),
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
        ( TargetType = misc_target_install_library
        ; TargetType = misc_target_install_library_gs_gas
        ),
        make_misc_target(ProgressStream, Globals,
            MainModuleName - misc_target_build_library, LibSucceeded,
            !Info, !Specs, !IO),
        (
            LibSucceeded = succeeded,
            (
                TargetType = misc_target_install_library,
                install_library(ProgressStream, Globals,
                    MainModuleName, Succeeded, !Info, !IO)
            ;
                TargetType = misc_target_install_library_gs_gas,
                install_library_gs_gas(ProgressStream, Globals,
                    MainModuleName, Succeeded, !Info, !IO)
            )
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
        NewOptionArgs = OrigOptionArgs ++ [CacheDirOption, CacheDir],
        make_info_set_option_args(NewOptionArgs, !Info),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        setup_checking_for_interrupt(Cookie, !IO),
        P(ProgressStream, TaskSucceeded, !Info, !IO),
        CleanupPred = remove_cache_dir(ProgressStream, Globals, CacheDir),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, CleanupPred,
            TaskSucceeded, Succeeded, !Info, !IO),
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
        NewOptionArgs = OrigOptionArgs ++ [CacheDirOption, CacheDir],
        make_info_set_option_args(NewOptionArgs, !Info),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        setup_checking_for_interrupt(Cookie, !IO),
        P(ProgressStream, TaskSucceeded, !Info, !Specs, !IO),
        CleanupPred = remove_cache_dir(ProgressStream, Globals, CacheDir),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, CleanupPred,
            TaskSucceeded, Succeeded, !Info, !IO),
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
    % XXX LEGACY
    analysis_cache_dir_name(Globals, CacheDir, _CacheDirProposed),
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

remove_cache_dir(ProgressStream, Globals, CacheDir, Info, Info, !IO) :-
    % The unnecessary Info arguments are required by the (current)
    % interface of teardown_checking_for_interrupt.
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
    get_bottom_up_ordered_modules(
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

    % Return a list of modules in reverse order of their dependencies,
    % i.e. the list is the module dependency graph from bottom-up. Mutually
    % dependent modules (modules which form a clique in the dependency graph)
    % are returned adjacent in the list in arbitrary order.
    %
:- pred get_bottom_up_ordered_modules(
    map(module_name, maybe_module_dep_info)::in,
    list(module_name)::in, list(module_name)::out) is det.

get_bottom_up_ordered_modules(ModuleDeps, Modules0, Modules) :-
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
:- pred add_module_relations(lookup_module_dep_info_func::in, module_name::in,
    digraph(module_name)::in, digraph(module_name)::out,
    digraph(module_name)::in, digraph(module_name)::out) is det.

add_module_relations(LookupModuleImportsFunc, ModuleName,
        !IntDepsGraph, !ImplDepsGraph) :-
    ModuleDepInfo = LookupModuleImportsFunc(ModuleName),
    add_module_dep_info_to_deps_graph(ModuleDepInfo, LookupModuleImportsFunc,
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
    are_shared_libraries_supported(Globals, SharedLibsSupported),
    (
        StaticSucceeded = succeeded,
        (
            SharedLibsSupported = shared_libraries_supported,
            make_linked_target(ProgressStream, Globals,
                linked_target_file(MainModuleName, shared_library),
                SharedLibsSucceeded, !Info, !Specs, !IO)
        ;
            SharedLibsSupported = shared_libraries_not_supported,
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

