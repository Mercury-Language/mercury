%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2026 The Mercury team.
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
    make_info::in, make_info::out, list(diag_spec)::in, list(diag_spec)::out,
    io::di, io::uo) is det.

    % make_misc_target(Globals, Target, Succeeded, !Info, !Specs, !IO):
    %
    % Handle miscellaneous target types, including clean-up, library
    % installation, and building all files of a given type for all
    % modules in the program.
    %
:- pred make_misc_target(io.text_output_stream::in, globals::in,
    pair(module_name, misc_target_type)::in, maybe_succeeded::out,
    make_info::in, make_info::out, list(diag_spec)::in, list(diag_spec)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module backend_libs.link_target_code.
:- import_module backend_libs.link_target_code_c.
:- import_module backend_libs.link_target_util.
:- import_module libs.check_libgrades.
:- import_module libs.file_util.
:- import_module libs.options.
:- import_module libs.process_util.
:- import_module libs.system_cmds.
:- import_module libs.timestamp.
:- import_module make.analysis.
:- import_module make.build.
:- import_module make.check_up_to_date.
:- import_module make.clean.
:- import_module make.find_local_modules.
:- import_module make.get_module_dep_info.
:- import_module make.int_opt.
:- import_module make.library_install.
:- import_module make.module_target.
:- import_module make.options_file.
:- import_module make.order.
:- import_module make.timestamp.
:- import_module make.util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.find_module.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module getopt.
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
        LinkedTargetType = c_shared_library,
        ExtraOptions = ["--compile-to-shared-lib"]
    ;
        ( LinkedTargetType = c_executable
        ; LinkedTargetType = c_static_library
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
            LinkedTargetType = c_static_library,
            not set.member(sos_static, LibraryInstallLinkages)
        ;
            LinkedTargetType = c_shared_library,
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
    make_info::in, make_info::out, list(diag_spec)::in, list(diag_spec)::out,
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
        Params = make_info_get_compiler_params(!.Info),
        setup_for_build_with_module_options(ProgressStream, DefaultOptionTable,
            MaybeStdLibGrades, invoked_by_mmc_make, MainModuleName,
            Params, ExtraOptions, MayBuild, !IO),
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
            write_oom_diag_specs(ProgressStream, Globals, Specs, !IO),
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
        % XXX This assignment to ProgModulesAlpha represents inlining
        %   get_target_modules(Globals, IntermediateTargetType, AllModulesList,
        %       ProgModulesAlpha, !Info, !IO),
        % knowing that IntermediateTargetType cannot be module_target_errors.
        ProgModulesAlpha = AllModulesList,
        order_target_modules(ProgressStream, Globals,
            ProgModulesAlpha, ProgModules, !Info, !IO),
        filter_out_nested_modules(ProgressStream, Globals,
            ProgModules, ProgModulesNonnested, !Info, !IO),
        IntermediateTargetsNonnested =
            make_target_id_list(ProgModulesNonnested, IntermediateTargetType),
        ObjTargets = make_target_id_list(ProgModules, ObjectTargetType),

        list.map_foldl2(
            get_foreign_object_targets(ProgressStream, Globals, PIC),
            ProgModules, ForeignObjTargetsList, !Info, !IO),
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
                    make_class_files_for_all_program_modules(ProgressStream,
                        Globals, MainModuleName, ProgModules,
                        BuildJavaSucceeded, !Info, !IO),
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
                build_linked_target_with_any_prelink(ProgressStream, Globals,
                    CompilationTarget, PIC, MainModuleName, LinkedTargetType,
                    FullMainModuleLinkedFileName,
                    CurDirMainModuleLinkedFileName, MaybeOldestLhsTimestamp,
                    AllModules, ProgModules, ShouldRebuildLhs,
                    Succeeded0, !Info, !IO),
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

:- pred get_foreign_object_targets(io.text_output_stream::in, globals::in,
    pic::in, module_name::in, list(target_id)::out,
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

    % None of the current backends require externally compiled foreign code,
    % except the C backend for fact tables.
    (
        CompilationTarget = target_c,
        FactFileToTarget =
            ( func(FactFile) =
                merc_target(target_file(ModuleName,
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

:- pred build_linked_target_with_any_prelink(io.text_output_stream::in,
    globals::in, compilation_target::in, pic::in,
    module_name::in, linked_target_type::in, file_name::in, file_name::in,
    maybe_oldest_lhs_file::in, set(module_name)::in, list(module_name)::in,
    should_rebuild_lhs::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_linked_target_with_any_prelink(ProgressStream, Globals,
        CompilationTarget, PIC, MainModuleName, LinkedTargetType,
        FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
        MaybeOldestLhsTimestamp, AllModules, ProgModules,
        ShouldRebuildLhs, Succeeded, !Info, !IO) :-
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
        maybe_build_linked_target(ProgressStream, Globals,
            CompilationTarget, PIC, MainModuleName, LinkedTargetType,
            FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
            MaybeOldestLhsTimestamp, AllModules, ProgModules,
            ShouldRebuildLhs, Succeeded, !Info, !IO)
    ;
        PreLinkSucceeded = did_not_succeed,
        Succeeded = did_not_succeed
    ).

:- pred maybe_build_linked_target(io.text_output_stream::in, globals::in,
    compilation_target::in, pic::in, module_name::in, linked_target_type::in,
    file_name::in, file_name::in, maybe_oldest_lhs_file::in,
    set(module_name)::in, list(module_name)::in, should_rebuild_lhs::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

maybe_build_linked_target(ProgressStream, Globals0, CompilationTarget, PIC,
        MainModuleName, LinkedTargetType,
        FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
        MaybeOldestLhsTimestamp, AllModules, ProgModules,
        ShouldRebuildLhs, Succeeded, !Info, !IO) :-
    % Clear the link_objects option -- we will pass the list of files directly.
    globals.lookup_accumulating_option(Globals0, link_objects, LinkObjects),
    globals.set_option(link_objects, accumulating([]),
        Globals0, NoLinkObjsGlobals),

    % Remake the `_init.o' file.
    % XXX We should probably make a `_init.o' file for shared
    % libraries linked using dlopen().
    AllModulesList = set.to_sorted_list(AllModules),
    (
        LinkedTargetType = c_executable,
        make_init_obj_file_check_result(ProgressStream, NoLinkObjsGlobals,
            MainModuleName, AllModulesList, InitObjSucceeded, InitObjects,
            !Info, !IO)
    ;
        ( LinkedTargetType = c_static_library
        ; LinkedTargetType = c_shared_library
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
    ObjectsToCheckTargets =
        list.map((func(F) = non_merc_target(F)), ObjectsToCheck),
    list.map_foldl2(
        get_target_id_status(ProgressStream, NoLinkObjsGlobals),
        ObjectsToCheckTargets, ExtraObjTargetStatuses, !Info, !IO),
    ( if
        some [ExtraObjTargetStatus] (
            list.member(ExtraObjTargetStatus, ExtraObjTargetStatuses),
            ExtraObjTargetStatus =
                target_status_result(_, _, target_status_error)
        )
    then
        ExtraObjSucceeded = did_not_succeed
    else
        ExtraObjSucceeded = succeeded
    ),
    BuildObjsSucceeded = InitObjSucceeded `and` ExtraObjSucceeded,

    list.map2_foldl2(get_file_timestamp(search_auth_cur_dir),
        ObjectsToCheck, _SearchDirs, ExtraObjectTimestamps, !Info, !IO),
    % XXX We pass BuildObjsSucceeded here, but BuildObjsSucceeded being
    % did_not_succeed does not prevent LhsResult being can_rebuild_lhs(_).
    % This means that we can go on to attempt to (re)build the target
    % even if we couldn't build all its prerequisites. To me (zs),
    % that looks like a bug, even if the keep_going flag is set.
    should_we_rebuild_lhs_given_timestamps(ProgressStream, NoLinkObjsGlobals,
        FullMainModuleLinkedFileName, MaybeOldestLhsTimestamp,
        BuildObjsSucceeded, ExtraObjTargetStatuses, ExtraObjectTimestamps,
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
            maybe_warn_linked_target_up_to_date(ProgressStream,
                NoLinkObjsGlobals, MainModuleName, LinkedTargetType,
                FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
                Succeeded, !Info, !IO)
        else
            build_linked_target(ProgressStream, NoLinkObjsGlobals,
                CompilationTarget, PIC, MainModuleName, LinkedTargetType,
                FullMainModuleLinkedFileName, AllModulesList, ProgModules,
                InitObjects, LinkObjects, Succeeded, !Info, !IO)
        )
    ).

:- pred build_linked_target(io.text_output_stream::in, globals::in,
     compilation_target::in, pic::in, module_name::in, linked_target_type::in,
     file_name::in, list(module_name)::in, list(module_name)::in,
    list(file_name)::in, list(file_name)::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

build_linked_target(ProgressStream, NoLinkObjsGlobals, CompilationTarget, PIC,
        MainModuleName, LinkedTargetType, FullMainModuleLinkedFileName,
        AllModulesList, ProgModules, InitObjectFileNames, LinkObjectFileNames,
        Succeeded, !Info, !IO) :-
    maybe_making_filename_msg(NoLinkObjsGlobals,
        FullMainModuleLinkedFileName, MakingMsg),
    maybe_write_msg(ProgressStream, MakingMsg, !IO),

    % Find the extra object files for externally compiled foreign
    % procedures and fact tables. We don't need to include these in the
    % timestamp checking above -- they will have been checked when the
    % module's object file was built.
    list.map_foldl2(
        get_module_fact_table_object_files(ProgressStream, NoLinkObjsGlobals,
            PIC),
        AllModulesList, FactTableObjFileNameLists, !Info, !IO),
    FactTableObjFileNames = list.condense(FactTableObjFileNameLists),

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
        ProgModules, ProgModuleObjFileNames, _ProgModuleObjFileNamesProposed),

    % InitObjectFileNames, passed from maybe_build_linked_target:
    %   Contains the _init.o object file. Makes sense only for C.
    % ProgModuleObjFileNames:
    %   Contains the "object" files of the modules of the program itself.
    % FactTableObjFileNames:
    %   Contains the set of object files implementing fact tables.
    %   Makes sense only for C.
    % LinkObjectFileNames, passed from maybe_build_linked_targets:
    %   Contains the value of the link_objects option. This consists of
    %   .a files (that make sense only for C) that must come after
    %    all the object files on the linker command line.
    AllObjects = InitObjectFileNames ++ ProgModuleObjFileNames ++
        FactTableObjFileNames ++ LinkObjectFileNames,
    % Run the link in a separate process so it can be killed
    % if an interrupt is received.
    call_in_forked_process(
        link_and_write_error_specs(NoLinkObjsGlobals, ProgressStream,
            LinkedTargetType, MainModuleName, AllObjects),
        Succeeded, !IO),
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

:- pred get_module_fact_table_object_files(io.text_output_stream::in,
    globals::in, pic::in, module_name::in, list(file_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_module_fact_table_object_files(ProgressStream, Globals, PIC,
        ModuleName, FactTableObjFiles, !MakeInfo, !IO) :-
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !MakeInfo, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        get_any_fact_table_object_code_files(Globals, PIC, ModuleDepInfo,
            FactTableFiles, !IO),
        FactTableObjFiles = list.map(
            (func(foreign_code_file(_, _, ObjFile)) = ObjFile),
            FactTableFiles)
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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

:- pred maybe_warn_linked_target_up_to_date(io.text_output_stream::in,
    globals::in, module_name::in, linked_target_type::in,
    file_name::in, file_name::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

maybe_warn_linked_target_up_to_date(ProgressStream,
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
        % We did not just perform a link, but check whether the other copy
        % of the would-have-been-linked-if-that-were-needed file has to be
        % re-copied.
        post_link_maybe_make_symlink_or_copy(ProgressStream, NoLinkObjsGlobals,
            LinkedTargetType, MainModuleName,
            FullMainModuleLinkedFileName, CurDirMainModuleLinkedFileName,
            Succeeded, MadeSymlinkOrCopy, !IO),
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

%---------------------------------------------------------------------------%

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
        LinkedTargetType = c_executable,
        remove_init_files(ProgressStream, Globals, verbose_make,
            MainModuleName, !Info, !IO)
    ;
        ( LinkedTargetType = c_static_library
        ; LinkedTargetType = c_shared_library
        ; LinkedTargetType = csharp_executable
        ; LinkedTargetType = csharp_library
        ; LinkedTargetType = java_executable
        ; LinkedTargetType = java_archive
        )
    ).

%---------------------------------------------------------------------------%

    % In java grades, we want to invoke `javac' just once, passing it
    % a list of all the program's out-of-date `.java' files. This is
    % a lot quicker than compiling each Java file individually.
    % (For the reason, see the comment on the module_target_java_class_code
    % arm of the big switch in the find_direct_prereqs_of_module_target
    % predicate in make.prereqs.m.)
    %
:- pred make_class_files_for_all_program_modules(io.text_output_stream::in,
    globals::in, module_name::in, list(module_name)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_class_files_for_all_program_modules(ProgressStream, Globals,
        MainModuleName, ProgModules, Succeeded, !Info, !IO) :-
    find_java_files_to_recompile(ProgressStream, Globals,
        ProgModules, JavaFilesToRecompile, !Info, !IO),
    (
        JavaFilesToRecompile = [],
        Succeeded = succeeded
    ;
        JavaFilesToRecompile = [HeadJavaFile | TailJavaFiles],
        recompile_given_java_files(ProgressStream, Globals,
            MainModuleName, HeadJavaFile, TailJavaFiles,
            Succeeded, !.Info, !IO),
        (
            Succeeded = did_not_succeed
        ;
            Succeeded = succeeded,
            % javac may write not just the .class files corresponding to
            % JavaFilesToRecompile, but also other .class files.
            % Since we have no simple, efficient way to predict what .class
            % files the recompilation may have affected, we intentionally
            % forget all our previously-recorded timestamps for .class files.
            TimestampMap0 = make_info_get_file_timestamp_map(!.Info),
            map.foldl(reinsert_timestamps_for_non_class_files, TimestampMap0,
                map.init, TimestampMap),
            make_info_set_file_timestamp_map(TimestampMap, !Info),
            % For simplicity, clear out all target file timestamps.
            make_info_set_target_file_timestamp_map(
                init_target_file_timestamp_map, !Info)
        )
    ).

:- pred find_java_files_to_recompile(io.text_output_stream::in,
    globals::in, list(module_name)::in, list(file_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_java_files_to_recompile(_, _, [], [], !Info, !IO).
find_java_files_to_recompile(ProgressStream, Globals,
        [HeadModuleName | TailModuleNames], JavaFilesToRecompile,
        !Info, !IO) :-
    find_java_files_to_recompile(ProgressStream, Globals,
        TailModuleNames, TailJavaFilesToRecompile, !Info, !IO),
    JavaTarget = target_file(HeadModuleName, module_target_java_code),
    ClassTarget = target_file(HeadModuleName, module_target_java_class_code),
    get_target_timestamp(ProgressStream, Globals,
        JavaTarget, MaybeJavaTimestamp, !Info, !IO),
    get_target_timestamp(ProgressStream, Globals,
        ClassTarget, MaybeClassTimestamp, !Info, !IO),
    ( if
        MaybeJavaTimestamp = ok(JavaTimestamp),
        MaybeClassTimestamp = ok(ClassTimestamp),
        ClassTimestamp @>= JavaTimestamp
    then
        JavaFilesToRecompile = TailJavaFilesToRecompile
    else
        % XXX Note that get_target_timestamp *also* converts HeadModuleName
        % to HeadJavaFile, but *only* if the timestamp of JavaTarget
        % is not in the cache.
        %
        % XXX FILE_NAMES
        % XXX LEGACY
        JavaExt = ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java),
        module_name_to_file_name(Globals, $pred, JavaExt, HeadModuleName,
            HeadJavaFile, _HeadJavaFileProposed),
        JavaFilesToRecompile = [HeadJavaFile | TailJavaFilesToRecompile]
    ).

:- pred recompile_given_java_files(io.text_output_stream::in, globals::in,
    module_name::in, file_name::in, list(file_name)::in, maybe_succeeded::out,
    make_info::in, io::di, io::uo) is det.

recompile_given_java_files(ProgressStream, Globals, MainModuleName,
        HeadJavaFile, TailJavaFiles, Succeeded, Info, !IO) :-
    verbose_make_one_part_msg(Globals, "Making Java class files", MakingMsg),
    maybe_write_msg(ProgressStream, MakingMsg, !IO),
    % We redirect errors to a file named after the main module.
    open_module_error_stream(ProgressStream, Globals, Info, MainModuleName,
        MaybeErrorStream, !IO),
    (
        MaybeErrorStream = es_error_already_reported,
        Succeeded = did_not_succeed
    ;
        MaybeErrorStream = es_ok(MESI, ErrorStream),
        call_in_forked_process(
            compile_java_files(ProgressStream, Globals,
                HeadJavaFile, TailJavaFiles),
            Succeeded, !IO),
        close_module_error_stream_handle_errors(ProgressStream, Globals,
            MESI, ErrorStream, Info, !IO)
    ).

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
    Params = make_info_get_compiler_params(!.Info),
    ExtraOptions = [],
    setup_for_build_with_module_options(ProgressStream, DefaultOptionTable,
        MaybeStdLibGrades, invoked_by_mmc_make, MainModuleName,
        Params, ExtraOptions, MayBuild, !IO),
    (
        MayBuild = may_build(_AllOptionArgs, BuildGlobals),
        make_misc_target_builder(ProgressStream, BuildGlobals, MainModuleName,
            TargetType, Succeeded, !Info, !Specs, !IO)
    ;
        MayBuild = may_not_build(Specs),
        % XXX MAKE_STREAM
        get_error_output_stream(Globals, MainModuleName, ErrorStream, !IO),
        write_oom_diag_specs(ErrorStream, Globals, Specs, !IO),
        Succeeded = did_not_succeed
    ).

:- pred make_misc_target_builder(io.text_output_stream::in, globals::in,
    module_name::in, misc_target_type::in, maybe_succeeded::out,
    make_info::in, make_info::out, list(diag_spec)::in, list(diag_spec)::out,
    io::di, io::uo) is det.

make_misc_target_builder(ProgressStream, Globals, MainModuleName, TargetType,
        Succeeded, !Info, !Specs, !IO) :-
    find_reachable_local_modules_for_misc(ProgressStream, Globals,
        MainModuleName, TargetType, Succeeded0, AllModuleNames, !Info, !IO),
    (
        TargetType = misc_target_clean,
        Succeeded = succeeded,
        list.foldl2(make_module_clean(ProgressStream, Globals),
            AllModuleNames, !Info, !IO),
        remove_init_files(ProgressStream, Globals, very_verbose,
            MainModuleName, !Info, !IO)
    ;
        TargetType = misc_target_realclean,
        Succeeded = succeeded,
        make_main_module_realclean(ProgressStream, Globals, MainModuleName,
            !Info, !IO),
        list.foldl2(make_module_realclean(ProgressStream, Globals),
            AllModuleNames, !Info, !IO)
    ;
        TargetType = misc_target_build_all(ModuleTargetType),
        get_target_modules(ProgressStream, Globals, ModuleTargetType,
            AllModuleNames, TargetModules, !Info, !IO),
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
                    build_int3s, AllModuleNames, Succeeded1, !Info, !IO)
            ;
                ( ModuleTargetType = module_target_int1
                ; ModuleTargetType = module_target_int2
                ),
                build_int_opt_files(ProgressStream, Globals,
                    build_int3s_int0s, AllModuleNames, Succeeded1, !Info, !IO)
            ;
                ModuleTargetType = module_target_opt,
                build_int_opt_files(ProgressStream, Globals,
                    build_all_ints, AllModuleNames, Succeeded1, !Info, !IO)
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
                    build_all_ints_opts, AllModuleNames, Succeeded1,
                    !Info, !IO)
            ),
            ( if
                Succeeded1 = did_not_succeed,
                KeepGoing = do_not_keep_going
            then
                Succeeded = did_not_succeed
            else
                Targets = make_target_id_list(TargetModules, ModuleTargetType),
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
                AllModuleNames, Succeeded0),
            Succeeded, !Info, !IO)
    ;
        TargetType = misc_target_build_library,
        build_int_opt_files(ProgressStream, Globals, build_all_ints_opts,
            AllModuleNames, IntSucceeded, !Info, !IO),
        (
            IntSucceeded = succeeded,
            maybe_with_analysis_cache_dir_3(ProgressStream, Globals,
                build_library(MainModuleName, AllModuleNames, Globals),
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
        SucceededSoFar = Succeeded0 `and` LibSucceeded,
        (
            SucceededSoFar = succeeded,
            (
                TargetType = misc_target_install_library,
                install_library(ProgressStream, Globals,
                    MainModuleName, AllModuleNames, Succeeded, !Info, !IO)
            ;
                TargetType = misc_target_install_library_gs_gas,
                install_library_gs_gas(ProgressStream, Globals,
                    MainModuleName, AllModuleNames, Succeeded, !Info, !IO)
            )
        ;
            SucceededSoFar = did_not_succeed,
            Succeeded = did_not_succeed
        )
    ;
        TargetType = misc_target_build_xml_docs,
        get_target_modules(ProgressStream, Globals, module_target_xml_doc,
            AllModuleNames, TargetModules, !Info, !IO),
        KeepGoing = make_info_get_keep_going(!.Info),
        ( if Succeeded0 = did_not_succeed, KeepGoing = do_not_keep_going then
            Succeeded = did_not_succeed
        else
            XmlDocs =
                make_target_id_list(TargetModules, module_target_xml_doc),
            foldl2_make_module_targets(KeepGoing, [],
                ProgressStream, Globals, XmlDocs, Succeeded1, !Info, !IO),
            Succeeded = Succeeded0 `and` Succeeded1
        )
    ).

:- pred find_reachable_local_modules_for_misc(io.text_output_stream::in,
    globals::in, module_name::in, misc_target_type::in,
    maybe_succeeded::out, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_reachable_local_modules_for_misc(ProgressStream, Globals,
        MainModuleName, TargetType, Succeeded, AllModuleNames, !Info, !IO) :-
    (
        ( TargetType = misc_target_clean
        ; TargetType = misc_target_realclean
        ),
        % Don't rebuild .module_dep files when cleaning up.
        RebuildModuleDeps = make_info_get_rebuild_module_deps(!.Info),
        make_info_set_rebuild_module_deps(do_not_rebuild_module_deps, !Info),
        find_reachable_local_modules(ProgressStream, Globals, MainModuleName,
            Succeeded, AllModuleNamesSet, !Info, !IO),
        make_info_set_rebuild_module_deps(RebuildModuleDeps, !Info)
    ;
        ( TargetType = misc_target_build_all(_)
        ; TargetType = misc_target_build_analyses
        ; TargetType = misc_target_build_library
        ; TargetType = misc_target_install_library
        ; TargetType = misc_target_install_library_gs_gas
        ; TargetType = misc_target_build_xml_docs
        ),
        find_reachable_local_modules(ProgressStream, Globals, MainModuleName,
            Succeeded, AllModuleNamesSet, !Info, !IO)
    ),
    AllModuleNames = set.to_sorted_list(AllModuleNamesSet).

%---------------------------------------------------------------------------%

    % The form of the argument list is dictated by the build3 type.
    %
:- pred build_library(module_name::in, list(module_name)::in,
    globals::in, io.text_output_stream::in, maybe_succeeded::out,
    make_info::in, make_info::out, list(diag_spec)::in, list(diag_spec)::out,
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
    make_info::in, make_info::out, list(diag_spec)::in, list(diag_spec)::out,
    io::di, io::uo) is det.

build_c_library(ProgressStream, Globals, MainModuleName, AllModules, Succeeded,
        !Info, !Specs, !IO) :-
    make_linked_target(ProgressStream, Globals,
        linked_target_file(MainModuleName, c_static_library),
        StaticSucceeded, !Info, !Specs, !IO),
    are_shared_libraries_supported(Globals, SharedLibsSupported),
    (
        StaticSucceeded = succeeded,
        (
            SharedLibsSupported = shared_libraries_supported,
            make_linked_target(ProgressStream, Globals,
                linked_target_file(MainModuleName, c_shared_library),
                SharedLibsSucceeded, !Info, !Specs, !IO)
        ;
            SharedLibsSupported = shared_libraries_not_supported,
            SharedLibsSucceeded = succeeded
        ),
        % We can only build the .init file if we have successfully built
        % the .c files.
        (
            SharedLibsSucceeded = succeeded,
            % Errors while making the .init file should be very rare.
            make_library_init_file(ProgressStream, Globals,
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
    list(diag_spec)::in, list(diag_spec)::out, io::di, io::uo) is det.

build_csharp_library(ProgressStream, Globals, MainModuleName, Succeeded,
        !Info, !Specs, !IO) :-
    make_linked_target(ProgressStream, Globals,
        linked_target_file(MainModuleName, csharp_library),
        Succeeded, !Info, !Specs, !IO).

:- pred build_java_library(io.text_output_stream::in, globals::in,
    module_name::in, maybe_succeeded::out, make_info::in, make_info::out,
    list(diag_spec)::in, list(diag_spec)::out, io::di, io::uo) is det.

build_java_library(ProgressStream, Globals, MainModuleName, Succeeded,
        !Info, !Specs, !IO) :-
    make_linked_target(ProgressStream, Globals,
        linked_target_file(MainModuleName, java_archive),
        Succeeded, !Info, !Specs, !IO).

%---------------------------------------------------------------------------%
:- end_module make.program_target.
%---------------------------------------------------------------------------%
