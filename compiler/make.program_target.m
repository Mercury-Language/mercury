%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2006 The University of Melbourne.
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

    % make_linked_target(Target, Success, !Info):
    %
    % Build a library or an executable.
    %
:- pred make_linked_target(linked_target_file::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % make_misc_target(Target, Success, !Info):
    %
    % Handle miscellaneous target types, including clean-up, library
    % installation, and building all files of a given type for all
    % modules in the program.
    %
:- pred make_misc_target(pair(module_name, misc_target_type)::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module relation.
:- import_module svrelation.

%-----------------------------------------------------------------------------%

make_linked_target(LinkedTargetFile, Succeeded, !Info, !IO) :-
    LinkedTargetFile = linked_target_file(MainModuleName, FileType),
    (
        FileType = shared_library,
        ExtraOptions = ["--compile-to-shared-lib"]
    ;
        ( FileType = executable
        ; FileType = java_archive
        ; FileType = static_library
        ),
        ExtraOptions = []
    ),
    globals.io_lookup_accumulating_option(lib_linkages, LibLinkages, !IO),
    (
        (
            FileType = static_library,
            not list.member("static", LibLinkages)
        ;
            FileType = shared_library,
            not list.member("shared", LibLinkages)
        )
    ->
        Succeeded = yes
    ;
        % When using `--intermodule-analysis', perform an analysis pass first.
        % The analysis of one module may invalidate the results of a module we
        % analysed earlier, so this step must be carried out until all the
        % `.analysis' files are in a valid state before we can continue.
        globals.io_lookup_bool_option(intermodule_analysis,
            IntermoduleAnalysis, !IO),
        (
            IntermoduleAnalysis = yes,
            make_misc_target_builder(
                MainModuleName - misc_target_build_analyses,
                ExtraOptions, Succeeded0, !Info, !IO)
        ;
            IntermoduleAnalysis = no,
            Succeeded0 = yes
        ),
        (
            Succeeded0 = yes,
            build_with_module_options(MainModuleName, ExtraOptions,
                make_linked_target_2(
                    linked_target_file(MainModuleName, FileType)),
                Succeeded, !Info, !IO)
        ;
            Succeeded0 = no,
            Succeeded = no
        )
    ).

:- pred make_linked_target_2(linked_target_file::in, list(string)::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

make_linked_target_2(LinkedTargetFile, _, Succeeded, !Info, !IO) :-
    LinkedTargetFile = linked_target_file(MainModuleName, FileType),
    find_reachable_local_modules(MainModuleName, DepsSuccess,
        AllModules, !Info, !IO),
    globals.io_lookup_bool_option(keep_going, KeepGoing, !IO),
    (
        DepsSuccess = no,
        KeepGoing = no
    ->
        Succeeded = no
    ;
        get_object_code_type(FileType, PIC, !IO),

        % Build the `.c' files first so that errors are reported
        % as soon as possible.
        globals.io_get_target(CompilationTarget, !IO),
        (
            CompilationTarget = target_c,
            IntermediateTargetType = module_target_c_code,
            ObjectTargetType = module_target_object_code(PIC)
        ;
            CompilationTarget = target_asm,
            IntermediateTargetType = module_target_asm_code(PIC),
            ObjectTargetType = module_target_object_code(PIC)
        ;
            CompilationTarget = target_il,
            IntermediateTargetType = module_target_il_code,
            ObjectTargetType = module_target_il_asm
        ;
            CompilationTarget = target_java,
            IntermediateTargetType = module_target_java_code,
            % XXX Whoever finishes the Java backend can fill this in.
            ObjectTargetType = module_target_object_code(non_pic)
        ),

        get_target_modules(IntermediateTargetType,
            set.to_sorted_list(AllModules), ObjModules, !Info, !IO),
        IntermediateTargets = make_dependency_list(ObjModules,
            IntermediateTargetType),
        ObjTargets = make_dependency_list(ObjModules, ObjectTargetType),

        list.map_foldl2(get_foreign_object_targets(PIC),
            ObjModules, ForeignObjTargetsList, !Info, !IO),
        ForeignObjTargets = list.condense(ForeignObjTargetsList),

        foldl2_maybe_stop_at_error(KeepGoing,
            foldl2_maybe_stop_at_error(KeepGoing, make_module_target),
            [IntermediateTargets, ObjTargets, ForeignObjTargets],
            BuildDepsSucceeded, !Info, !IO),

        linked_target_file_name(MainModuleName, FileType, OutputFileName, !IO),
        get_file_timestamp([dir.this_directory], OutputFileName,
            MaybeTimestamp, !Info, !IO),
        check_dependencies(OutputFileName, MaybeTimestamp, BuildDepsSucceeded,
            ObjTargets, BuildDepsResult, !Info, !IO),
        (
            DepsSuccess = yes,
            BuildDepsResult \= deps_error
        ->
            build_with_check_for_interrupt(
                build_with_output_redirect(MainModuleName,
                    build_linked_target(MainModuleName, FileType,
                        OutputFileName, MaybeTimestamp, AllModules, ObjModules,
                        CompilationTarget, PIC, DepsSuccess, BuildDepsResult)
                    ),
                linked_target_cleanup(MainModuleName, FileType, OutputFileName,
                    CompilationTarget),
                Succeeded, !Info, !IO)
        ;
            Succeeded = no
        )
    ).

:- pred get_target_modules(module_target_type::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_modules(TargetType, AllModules, TargetModules, !Info, !IO) :-
    globals.io_get_target(CompilationTarget, !IO),
    (
        (
            TargetType = module_target_errors
        ;
            CompilationTarget = target_asm,
            ( TargetType = module_target_asm_code(_)
            ; TargetType = module_target_object_code(_)
            )
        )
    ->
        % `.err' and `.s' files are only produced for the top-level module
        % in each source file.
        list.foldl3(get_target_modules_2, AllModules, [], TargetModules,
            !Info, !IO)
    ;
        TargetModules = AllModules
    ).

:- pred get_target_modules_2(module_name::in,
    list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_modules_2(ModuleName, !TargetModules, !Info, !IO) :-
    get_module_dependencies(ModuleName, MaybeImports, !Info, !IO),
    (
        MaybeImports = yes(Imports),
        ModuleName = Imports ^ source_file_module_name
    ->
        !:TargetModules = [ModuleName | !.TargetModules]
    ;
        true
    ).

:- pred get_foreign_object_targets(pic::in, module_name::in,
    list(dependency_file)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

get_foreign_object_targets(PIC, ModuleName, ObjectTargets, !Info, !IO) :-
    % Find externally compiled foreign code files for
    % `:- pragma foreign_proc' declarations.

    globals.io_get_target(CompilationTarget, !IO),
    get_module_dependencies(ModuleName, MaybeImports, !Info, !IO),
    (
        MaybeImports = yes(Imports)
    ;
        MaybeImports = no,
        unexpected(this_file, "unknown imports")
    ),
    (
        CompilationTarget = target_asm,
        Imports ^ has_foreign_code = contains_foreign_code(Langs),
        set.member(lang_c, Langs)
    ->
        ForeignObjectFileType = module_target_foreign_object(PIC, lang_c),
        ForeignObjectTarget   = target_file(ModuleName, ForeignObjectFileType),
        ForeignObjectTargets  = [dep_target(ForeignObjectTarget)]
    ;
        CompilationTarget = target_il,
        Imports ^ has_foreign_code = contains_foreign_code(Langs)
    ->
        ForeignObjectTargets = list.map(
            (func(L) = dep_target(target_file(ModuleName,
                module_target_foreign_il_asm(L)))
            ), set.to_sorted_list(Langs))
    ;
        ForeignObjectTargets = []
    ),

    % Find out if any externally compiled foreign code files for fact tables
    % exist.
    (
        ( CompilationTarget = target_c
        ; CompilationTarget = target_asm
        ),
        FactObjectTargets = list.map(
            (func(FactFile) =
                dep_target(target_file(ModuleName,
                    module_target_fact_table_object(PIC, FactFile)))
            ),
            Imports ^ fact_table_deps),
        ObjectTargets = FactObjectTargets ++ ForeignObjectTargets
    ;
        ( CompilationTarget = target_java
        ; CompilationTarget = target_il
        ),
        ObjectTargets = ForeignObjectTargets
    ).

:- pred build_linked_target(module_name::in, linked_target_type::in,
    file_name::in, maybe_error(timestamp)::in, set(module_name)::in,
    list(module_name)::in, compilation_target::in, pic::in,
    bool::in, dependencies_result::in, io.output_stream::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

build_linked_target(MainModuleName, FileType, OutputFileName, MaybeTimestamp,
        AllModules, ObjModules, CompilationTarget, PIC, DepsSuccess,
        BuildDepsResult, ErrorStream, Succeeded, !Info, !IO) :-
    globals.io_lookup_maybe_string_option(pre_link_command,
        MaybePreLinkCommand, !IO),
    (
        MaybePreLinkCommand = yes(PreLinkCommand),
        make_all_module_command(PreLinkCommand, MainModuleName,
            to_sorted_list(AllModules), CommandString, !IO),
        invoke_system_command(ErrorStream, cmd_verbose, CommandString,
            PreLinkSucceeded, !IO)
    ;
        MaybePreLinkCommand = no,
        PreLinkSucceeded = yes
    ),
    (
        PreLinkSucceeded = yes,
        build_linked_target_2(MainModuleName, FileType, OutputFileName,
            MaybeTimestamp, AllModules, ObjModules, CompilationTarget, PIC,
            DepsSuccess, BuildDepsResult, ErrorStream, Succeeded, !Info, !IO)
    ;
        PreLinkSucceeded = no,
        Succeeded = no
    ).

:- pred build_linked_target_2(module_name::in, linked_target_type::in,
    file_name::in, maybe_error(timestamp)::in, set(module_name)::in,
    list(module_name)::in, compilation_target::in, pic::in,
    bool::in, dependencies_result::in, io.output_stream::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_linked_target_2(MainModuleName, FileType, OutputFileName, MaybeTimestamp,
        AllModules, ObjModules, CompilationTarget, PIC, DepsSuccess,
        BuildDepsResult, ErrorStream, Succeeded, !Info, !IO) :-
    globals.io_lookup_accumulating_option(link_objects, LinkObjects, !IO),

    % Clear the option -- we'll pass the list of files directly.
    globals.io_set_option(link_objects, accumulating([]), !IO),

    % Remake the `_init.o' file.
    % XXX We should probably make a `_init.o' file for shared
    % libraries linked using dlopen().
    AllModulesList = set.to_sorted_list(AllModules),
    (
        FileType = executable,
        ( CompilationTarget = target_c
        ; CompilationTarget = target_asm
        )
    ->
        compile_target_code.make_init_obj_file(ErrorStream,
            MainModuleName, AllModulesList, InitObjectResult, !IO),
        (
            InitObjectResult = yes(InitObject),
            % We may need to update the timestamp of the `_init.o' file.
            !:Info = !.Info ^ file_timestamps :=
                map.delete(!.Info ^ file_timestamps, InitObject),
            InitObjects = [InitObject],
            DepsResult2 = BuildDepsResult
        ;
            InitObjectResult = no,
            DepsResult2 = deps_error,
            InitObjects = []
        )
    ;
        DepsResult2 = BuildDepsResult,
        InitObjects = []
    ),

    ObjectsToCheck = InitObjects ++ LinkObjects,

    % Report errors if any of the extra objects aren't present.
    list.map_foldl2(dependency_status,
        list.map((func(F) = dep_file(F, no)), ObjectsToCheck),
            ExtraObjStatus, !Info, !IO),

    DepsResult3 =
        ( list.member(deps_status_error, ExtraObjStatus) ->
            deps_error ; DepsResult2 ),
    BuildDepsSuccess = ( DepsResult3 \= deps_error -> yes ; no ),
    list.map_foldl2(get_file_timestamp([dir.this_directory]),
        ObjectsToCheck, ExtraObjectTimestamps, !Info, !IO),
    check_dependency_timestamps(OutputFileName, MaybeTimestamp,
        BuildDepsSuccess, ObjectsToCheck, io.write,
        ExtraObjectTimestamps, ExtraObjectDepsResult, !IO),

    DepsResult4 = ( DepsSuccess = yes -> DepsResult3 ; deps_error ),
    ( DepsResult4 = deps_error, DepsResult = DepsResult4
    ; DepsResult4 = deps_out_of_date, DepsResult = DepsResult4
    ; DepsResult4 = deps_up_to_date, DepsResult = ExtraObjectDepsResult
    ),
    (
        DepsResult = deps_error,
        file_error(OutputFileName, !IO),
        Succeeded = no
    ;
        DepsResult = deps_up_to_date,
        globals.io_lookup_bool_option(use_grade_subdirs, UseGradeSubdirs,
            !IO),
        (
            UseGradeSubdirs = yes,
            maybe_symlink_or_copy_linked_target_message(
                MainModuleName - linked_target(FileType), !IO),
            compile_target_code.post_link_make_symlink_or_copy(ErrorStream,
                FileType, MainModuleName, Succeeded, !IO)
        ;
            UseGradeSubdirs = no,
            maybe_warn_up_to_date_target(
                MainModuleName - linked_target(FileType), !Info, !IO),
            Succeeded = yes
        )
    ;
        DepsResult = deps_out_of_date,
        maybe_make_linked_target_message(OutputFileName, !IO),

        % Find the extra object files for externally compiled foreign
        % procedures and fact tables. We don't need to include these in the
        % timestamp checking above -- they will have been checked when the
        % module's object file was built.
        list.map_foldl2(
            (pred(ModuleName::in, ForeignFiles::out,
                MakeInfo0::in, MakeInfo::out, !.IO::di, !:IO::uo) is det :-
            get_module_dependencies(ModuleName, MaybeImports,
                MakeInfo0, MakeInfo, !IO),
            (
                MaybeImports = yes(Imports),
                external_foreign_code_files(PIC, Imports, ForeignFiles, !IO)
            ;
                MaybeImports = no,
                % This error should have been detected earlier.
                unexpected(this_file,
                    "build_linked_target: error in dependencies")
            )
            ), AllModulesList, ExtraForeignFiles, !Info, !IO),
        ForeignObjects = list.map(
            (func(foreign_code_file(_, _, ObjFile)) = ObjFile),
            list.condense(ExtraForeignFiles)),

        maybe_pic_object_file_extension(PIC, ObjExtToUse, !IO),
        list.map_foldl(
            (pred(ObjModule::in, ObjToLink::out, !.IO::di, !:IO::uo) is det :-
                module_name_to_file_name(ObjModule,
                    ObjExtToUse, no, ObjToLink, !IO)
            ), ObjModules, ObjList, !IO),

        % LinkObjects may contain `.a' files which must come
        % after all the object files on the linker command line.
        AllObjects = InitObjects ++ ObjList ++ ForeignObjects ++ LinkObjects,
        (
            CompilationTarget = target_c,
            % Run the link in a separate process so it can be killed
            % if an interrupt is received.
            call_in_forked_process(
                compile_target_code.link(ErrorStream,
                    FileType, MainModuleName, AllObjects),
                Succeeded, !IO)
        ;
            CompilationTarget = target_asm,
            % Run the link in a separate process so it can
            % be killed if an interrupt is received.
            call_in_forked_process(
                compile_target_code.link(ErrorStream,
                    FileType, MainModuleName, AllObjects),
                Succeeded, !IO)
        ;
            CompilationTarget = target_il,
            Succeeded = yes
        ;
            CompilationTarget = target_java,
            create_java_shell_script(MainModuleName, Succeeded, !IO)
        ),
        !:Info = !.Info ^ command_line_targets :=
            set.delete(!.Info ^ command_line_targets,
                MainModuleName - linked_target(FileType)),
        (
            Succeeded = yes,
            !:Info = !.Info ^ file_timestamps :=
                map.delete(!.Info ^ file_timestamps, OutputFileName)
        ;
            Succeeded = no,
            file_error(OutputFileName, !IO)
        )
    ),
    globals.io_set_option(link_objects, accumulating(LinkObjects), !IO).

    % join_string_list(Strings, Prefix, Suffix, Serarator, Result)
    %
    % Appends the strings in the list `Strings' together into the
    % string Result. Each string is prefixed by Prefix, suffixed by
    % Suffix and separated by Separator.
    %
:- pred join_string_list(list(string)::in, string::in, string::in,
    string::in, string::out) is det.

join_string_list([], _Prefix, _Suffix, _Separator, "").
join_string_list([String | Strings], Prefix, Suffix, Separator, Result) :-
    (
        Strings = [],
        string.append_list([Prefix, String, Suffix], Result)
    ;
        Strings = [_ | _],
        join_string_list(Strings, Prefix, Suffix, Separator, Result0),
        string.append_list([Prefix, String, Suffix, Separator,
            Result0], Result)
    ).

:- pred linked_target_cleanup(module_name::in, linked_target_type::in,
    file_name::in, compilation_target::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

linked_target_cleanup(MainModuleName, FileType, OutputFileName,
        CompilationTarget, !Info, !IO) :-
    make_remove_file(OutputFileName, !Info, !IO),
    (
        FileType = executable,
        ( CompilationTarget = target_c
        ; CompilationTarget = target_asm
        )
    ->
        remove_init_files(MainModuleName, !Info, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

make_misc_target(MainModuleName - TargetType, Succeeded, !Info, !IO) :-
    build_with_module_options(MainModuleName, [],
        make_misc_target_builder(MainModuleName - TargetType),
        Succeeded, !Info, !IO).

:- pred make_misc_target_builder(pair(module_name, misc_target_type)::in,
    list(string)::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

make_misc_target_builder(MainModuleName - TargetType, _, Succeeded,
        !Info, !IO) :-
    % Don't rebuild dependencies when cleaning up.
    RebuildDeps = !.Info ^ rebuild_dependencies,
    (
        ( TargetType = misc_target_clean
        ; TargetType = misc_target_realclean
        )
    ->
        !:Info = !.Info ^ rebuild_dependencies := no
    ;
        true
    ),
    find_reachable_local_modules(MainModuleName, Succeeded0, AllModulesSet,
        !Info, !IO),
    !:Info = !.Info ^ rebuild_dependencies := RebuildDeps,
    AllModules = set.to_sorted_list(AllModulesSet),
    (
        TargetType = misc_target_clean,
        Succeeded = yes,
        list.foldl2(make_module_clean, AllModules, !Info, !IO),
        remove_init_files(MainModuleName, !Info, !IO)
    ;
        TargetType = misc_target_realclean,
        Succeeded = yes,
        make_main_module_realclean(MainModuleName, !Info, !IO),
        list.foldl2(make_module_realclean, AllModules, !Info, !IO)
    ;
        TargetType = misc_target_build_all(ModuleTargetType),
        get_target_modules(ModuleTargetType, AllModules, TargetModules,
            !Info, !IO),
        globals.io_lookup_bool_option(keep_going, KeepGoing, !IO),
        ( Succeeded0 = no, KeepGoing = no ->
            Succeeded = no
        ;
            foldl2_maybe_stop_at_error(KeepGoing,
                make_module_target,
                make_dependency_list(TargetModules, ModuleTargetType),
                Succeeded1, !Info, !IO),
            Succeeded = Succeeded0 `and` Succeeded1
        )
    ;
        TargetType = misc_target_build_analyses,
        build_analysis_files(MainModuleName, AllModules, Succeeded0, Succeeded,
            !Info, !IO)
    ;
        TargetType = misc_target_build_library,
        ShortInts = make_dependency_list(AllModules,
            module_target_unqualified_short_interface),
        LongInts = make_dependency_list(AllModules,
            module_target_long_interface),
        globals.io_lookup_bool_option(intermodule_optimization,
            Intermod, !IO),
        (
            Intermod = yes,
            OptFiles = make_dependency_list(AllModules,
                module_target_intermodule_interface)
        ;
            Intermod = no,
            OptFiles = []
        ),
        globals.io_lookup_bool_option(keep_going, KeepGoing, !IO),
        foldl2_maybe_stop_at_error(KeepGoing,
            foldl2_maybe_stop_at_error(KeepGoing, make_module_target),
            [ShortInts, LongInts, OptFiles],
            IntSucceeded, !Info, !IO),
        (
            IntSucceeded = yes,
            make_linked_target(
                linked_target_file(MainModuleName, static_library),
                StaticSucceeded, !Info, !IO),
            shared_libraries_supported(SharedLibsSupported, !IO),
            (
                StaticSucceeded = yes,
                (
                    SharedLibsSupported = yes,
                    make_linked_target(
                        linked_target_file(MainModuleName, shared_library),
                        SharedLibsSucceeded, !Info, !IO)
                ;
                    SharedLibsSupported = no,
                    SharedLibsSucceeded = yes
                ),
                % We can only build the .init file if we have succesfully
                % built the .c files.
                (
                    SharedLibsSucceeded = yes,
                    % Errors while making the .init file should be very rare.
                    io.output_stream(ErrorStream, !IO),
                    make_init_file(ErrorStream, MainModuleName,
                        AllModules, Succeeded, !IO)
                ;
                    SharedLibsSucceeded = no,
                    Succeeded = no 
                )
            ;
                StaticSucceeded = no,
                Succeeded = no
            )
        ;
            IntSucceeded = no,
            Succeeded = no
        )
    ;
        TargetType = misc_target_install_library,
        make_misc_target(MainModuleName - misc_target_build_library,
            LibSucceeded, !Info, !IO),
        (
            LibSucceeded = yes,
            install_library(MainModuleName, Succeeded, !Info, !IO)
        ;
            LibSucceeded = no,
            Succeeded = no
        )
    ).

:- pred build_analysis_files(module_name::in, list(module_name)::in,
    bool::in, bool::out, make_info::in, make_info::out, io::di, io::uo)
    is det.

build_analysis_files(MainModuleName, AllModules, Succeeded0, Succeeded,
        !Info, !IO) :-
    get_target_modules(module_target_analysis_registry, AllModules,
        TargetModules0, !Info, !IO),
    globals.io_lookup_bool_option(keep_going, KeepGoing, !IO),
    ( Succeeded0 = no, KeepGoing = no ->
        Succeeded = no
    ;
        reverse_ordered_modules(!.Info ^ module_dependencies,
            TargetModules0, TargetModules1),
        % Filter out the non-local modules so we don't try to reanalyse them.
        list.filter((pred(Mod::in) is semidet :- list.member(Mod, AllModules)),
            TargetModules1, TargetModules),
        make_local_module_id_options(MainModuleName, Succeeded1,
            LocalModulesOpts, !Info, !IO),
        (
            Succeeded1 = yes,
            build_analysis_files_2(MainModuleName, TargetModules, 
                LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
        ;
            Succeeded1 = no,
            Succeeded = no
        )
    ).

:- pred build_analysis_files_2(module_name::in, list(module_name)::in,
    list(string)::in, bool::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

build_analysis_files_2(MainModuleName, TargetModules, LocalModulesOpts,
        Succeeded0, Succeeded, !Info, !IO) :-
    globals.io_lookup_bool_option(keep_going, KeepGoing, !IO),
    foldl2_maybe_stop_at_error(KeepGoing,
        make_module_target_extra_options(LocalModulesOpts),
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
    modules_needing_reanalysis(ReanalyseSuboptimal, TargetModules,
        InvalidModules, SuboptimalModules, !IO),
    ( list.is_not_empty(InvalidModules) ->
        maybe_reanalyse_modules_message(!IO),
        list.foldl(reset_analysis_registry_dependency_status,
            InvalidModules, !Info),
        build_analysis_files_2(MainModuleName, TargetModules, LocalModulesOpts,
            Succeeded0, Succeeded, !Info, !IO)
    ; list.is_not_empty(SuboptimalModules) ->
        list.foldl(reset_analysis_registry_dependency_status,
            SuboptimalModules, !Info),
        !:Info = !.Info ^ reanalysis_passes := ReanalysisPasses - 1,
        maybe_reanalyse_modules_message(!IO),
        build_analysis_files_2(MainModuleName, TargetModules, LocalModulesOpts,
            Succeeded0, Succeeded, !Info, !IO)
    ;
        Succeeded = Succeeded0 `and` Succeeded1
    ).

    % Return a list of modules in reverse order of their dependencies, i.e.
    % the list is the module dependency graph from bottom-up.  Mutually
    % dependent modules (modules which form a clique in the dependency graph)
    % are returned adjacent in the list in arbitrary order.
    %
:- pred reverse_ordered_modules(map(module_name, maybe(module_imports))::in,
    list(module_name)::in, list(module_name)::out) is det.

reverse_ordered_modules(ModuleDeps, Modules0, Modules) :-
    list.foldl2(add_module_relations(lookup_module_imports(ModuleDeps)),
        Modules0, relation.init, _IntRel, relation.init, ImplRel),
    relation.atsort(ImplRel, Order0),
    list.reverse(Order0, Order1),
    list.map(set.to_sorted_list, Order1, Order2),
    list.condense(Order2, Modules).

:- func lookup_module_imports(map(module_name, maybe(module_imports)),
    module_name) = module_imports.

lookup_module_imports(ModuleDeps, ModuleName) = ModuleImports :-
    map.lookup(ModuleDeps, ModuleName, MaybeModuleImports),
    (
        MaybeModuleImports = yes(ModuleImports)
    ;
        MaybeModuleImports = no,
        unexpected(this_file, "lookup_module_imports")
    ).

:- pred modules_needing_reanalysis(bool::in, list(module_name)::in,
    list(module_name)::out, list(module_name)::out, io::di, io::uo) is det.

modules_needing_reanalysis(_, [], [], [], !IO).
modules_needing_reanalysis(ReanalyseSuboptimal, [Module | Modules],
        InvalidModules, SuboptimalModules, !IO) :-
    analysis.read_module_overall_status(mmc, module_name_to_module_id(Module),
        MaybeModuleStatus, !IO),
    (
        MaybeModuleStatus = yes(ModuleStatus),
        (
            ModuleStatus = optimal,
            modules_needing_reanalysis(ReanalyseSuboptimal, Modules,
                InvalidModules, SuboptimalModules, !IO)
        ;
            ModuleStatus = suboptimal,
            modules_needing_reanalysis(ReanalyseSuboptimal, Modules,
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
            modules_needing_reanalysis(ReanalyseSuboptimal, Modules,
                InvalidModules0, SuboptimalModules, !IO),
            InvalidModules = [Module | InvalidModules0]
        )
    ;
        MaybeModuleStatus = no,
        % The analysis file does not exist.  For some reason it wasn't created
        % in this or an earlier pass (and hence probably won't be created no
        % matter how many times we repeat the analysis).
        %
        % XXX: Currently modules which are basically empty do not get
        % `.analysis' files produced.  After that is fixed we can probably
        % consider modules with missing `.analysis' files to be invalid.
        %
        % XXX: MaybeModuleStatus could be `no' for some other reason
        %
        modules_needing_reanalysis(ReanalyseSuboptimal, Modules,
            InvalidModules, SuboptimalModules, !IO)
    ).

:- pred reset_analysis_registry_dependency_status(module_name::in,
    make_info::in, make_info::out) is det.

reset_analysis_registry_dependency_status(ModuleName, Info,
        Info ^ dependency_status ^ elem(Dep) := deps_status_not_considered) :-
    Dep = dep_target(target_file(ModuleName, module_target_analysis_registry)).

%-----------------------------------------------------------------------------%

:- pred install_library(module_name::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_library(MainModuleName, Succeeded, !Info, !IO) :-
    find_reachable_local_modules(MainModuleName, DepsSuccess, AllModules0,
        !Info, !IO),
    AllModules = set.to_sorted_list(AllModules0) ,
    make_install_dirs(DirSucceeded, LinkSucceeded, !IO),
    (
        DepsSuccess = yes,
        DirSucceeded = yes
    ->
        globals.io_lookup_string_option(install_prefix, Prefix, !IO),

        % XXX Trace goal fix.
        ModulesDir = Prefix/"lib"/"mercury"/"modules",
        module_name_to_file_name(MainModuleName, ".init", no, InitFileName,
            !IO),
        install_file(InitFileName, ModulesDir, InitSucceeded, !IO),

        list.map_foldl2(install_ints_and_headers(LinkSucceeded), AllModules,
            IntsSucceeded, !Info, !IO),

        globals.io_get_globals(Globals, !IO),
        grade_directory_component(Globals, Grade),
        install_library_grade_files(LinkSucceeded, Grade, MainModuleName,
            AllModules, GradeSucceeded, !Info, !IO),
        (
            InitSucceeded = yes,
            bool.and_list(IntsSucceeded) = yes,
            GradeSucceeded = yes
        ->
            % XXX With Mmake, LIBGRADES is target-specific.
            globals.io_lookup_accumulating_option(libgrades, LibGrades0, !IO),
            globals.io_lookup_bool_option(keep_going, KeepGoing, !IO),
            LibGrades = list.delete_all(LibGrades0, Grade),
            foldl2_maybe_stop_at_error(KeepGoing,
                install_library_grade(LinkSucceeded,
                    MainModuleName, AllModules),
                LibGrades, Succeeded, !Info, !IO)
        ;
            Succeeded = no
        )
    ;
        Succeeded = no
    ).

:- pred install_ints_and_headers(bool::in, module_name::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_ints_and_headers(SubdirLinkSucceeded, ModuleName, Succeeded, !Info,
        !IO) :-
    get_module_dependencies(ModuleName, MaybeImports, !Info, !IO),
    (
        MaybeImports = yes(Imports),
        globals.io_lookup_bool_option(intermodule_optimization, Intermod, !IO),
        (
            Intermod = yes,
            % `.int0' files are imported by `.opt' files.
            (
                Imports ^ children = [_ | _],
                Exts = ["int0", "opt"]
            ;
                Imports ^ children = [],
                Exts = ["opt"]
            )
        ;
            Intermod = no,
            Exts = []
        ),

        globals.io_lookup_string_option(install_prefix, Prefix, !IO),
        LibDir = Prefix/"lib"/"mercury",
        list.map_foldl(
            install_subdir_file(SubdirLinkSucceeded, LibDir/"ints",
                ModuleName),
            ["int", "int2", "int3", "module_dep" | Exts],
            Results, !IO),

        globals.io_get_target(Target, !IO),
        (
            % `.mh' files are (were) only generated for modules containing
            % `:- pragma foreign_export' declarations.
            % But `.mh' files are expected by Mmake so always generate them,
            % otherwise there is trouble using libraries installed by
            % `mmc --make' with Mmake.
            % XXX If we ever phase out mmake we could revert this behaviour.
            ( Target = target_c ; Target = target_asm )
            % Imports ^ contains_foreign_export = contains_foreign_export
        ->
            module_name_to_file_name(ModuleName, ".mh", no, FileName, !IO),
            install_file(FileName, LibDir/"inc", HeaderSucceeded1, !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            install_subdir_file(SubdirLinkSucceeded, LibDir/"ints", ModuleName,
                "mh", HeaderSucceeded2, !IO),

            HeaderSucceeded = HeaderSucceeded1 `and` HeaderSucceeded2
        ;
            HeaderSucceeded = yes
        ),
        Succeeded = bool.and_list([HeaderSucceeded | Results])
    ;
        MaybeImports = no,
        Succeeded = no
    ).

:- pred install_library_grade(bool::in, module_name::in, list(module_name)::in,
    string::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

install_library_grade(LinkSucceeded0, ModuleName, AllModules, Grade, Succeeded,
        !Info, !IO) :-
    % Building the library in the new grade is done in a separate process
    % to make it easier to stop and clean up on an interrupt.

    Cleanup = make_grade_clean(ModuleName, AllModules),
    build_with_check_for_interrupt(
        ( pred(GradeSuccess::out, MInfo::in, MInfo::out, !.IO::di, !:IO::uo)
                is det :-
            call_in_forked_process(
                (pred(GradeSuccess0::out, !.IO::di, !:IO::uo) is det :-
                install_library_grade_2(LinkSucceeded0,
                    Grade, ModuleName, AllModules,
                    MInfo, GradeSuccess0, !IO)
                ), GradeSuccess, !IO)
        ), Cleanup, Succeeded, !Info, !IO).

:- pred install_library_grade_2(bool::in, string::in, module_name::in,
    list(module_name)::in, make_info::in, bool::out, io::di, io::uo) is det.

install_library_grade_2(LinkSucceeded0, Grade, ModuleName, AllModules,
        Info0, Succeeded, !IO) :-
    globals.io_get_globals(OrigGlobals, !IO),

    % Set up so that grade-dependent files for the current grade
    % don't overwrite the files for the default grade.
    OptionArgs0 = Info0 ^ option_args,
    OptionArgs = OptionArgs0 ++ ["--grade", Grade, "--use-grade-subdirs"],

    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Installing grade ", !IO),
            io.write_string(Grade, !IO),
            io.nl(!IO)
        ), !IO),

    lookup_mmc_options(Info0 ^ options_variables, MaybeMCFlags, !IO),
    (
        MaybeMCFlags = yes(MCFlags),
        handle_options(MCFlags ++ OptionArgs, OptionsErrors, _, _, _, !IO)
    ;
        MaybeMCFlags = no,
        % Errors should have been caught before.
        unexpected(this_file, "install_library_grade: bad DEFAULT_MCFLAGS")
    ),

    (
        OptionsErrors = [_ | _],
        usage_errors(OptionsErrors, !IO),
        Succeeded = no
    ;
        OptionsErrors = [],

        % Remove the grade-dependent targets from the status map
        % (we need to rebuild them in the new grade).
        StatusMap0 = Info0 ^ dependency_status,
        StatusMap = map.from_assoc_list(list.filter(
            (pred((File - _)::in) is semidet :-
                \+ (
                    File = dep_target(target_file(_, Target)),
                    target_is_grade_or_arch_dependent(Target)
                )
            ),
            map.to_assoc_list(StatusMap0))),
        Info1 = (Info0 ^ dependency_status := StatusMap)
            ^ option_args := OptionArgs,
        make_misc_target(ModuleName - misc_target_build_library, LibSucceeded,
            Info1, Info2, !IO),
        (
            LibSucceeded = yes,
            % `GradeDir' differs from `Grade' in that it is in canonical form,
            % and it does not include any `.picreg' component.
            globals.io_get_globals(Globals, !IO),
            grade_directory_component(Globals, GradeDir),
            install_library_grade_files(LinkSucceeded0, GradeDir,
                ModuleName, AllModules, Succeeded, Info2, Info3, !IO),
            make_grade_clean(ModuleName, AllModules, Info3, _, !IO)
        ;
            LibSucceeded = no,
            Succeeded = no
        )
    ),
    globals.io_set_globals(unsafe_promise_unique(OrigGlobals), !IO).

    % Install the `.a', `.so', `.jar', `.opt' and `.mih' files for the current
    % grade.
    %
    % NOTE: changes here may require changes to
    %       modules.get_install_name_option/4.
    %
:- pred install_library_grade_files(bool::in, string::in, module_name::in,
    list(module_name)::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

install_library_grade_files(LinkSucceeded0, GradeDir, ModuleName, AllModules,
        Succeeded, !Info, !IO) :-
    make_grade_install_dirs(GradeDir, DirResult, LinkSucceeded1, !IO),
    LinkSucceeded = LinkSucceeded0 `and` LinkSucceeded1,
    (
        DirResult = yes,
        linked_target_file_name(ModuleName, static_library, LibFileName, !IO),
        linked_target_file_name(ModuleName, shared_library, SharedLibFileName,
            !IO),
        linked_target_file_name(ModuleName, java_archive, JarFileName, !IO),

        globals.io_lookup_string_option(install_prefix, Prefix, !IO),

        ( GradeDir = "java" ->
            GradeLibDir = Prefix/"lib"/"mercury"/"lib"/"java",
            install_file(JarFileName, GradeLibDir, LibsSucceeded, !IO)
        ;
            GradeLibDir = Prefix/"lib"/"mercury"/"lib"/GradeDir,
            maybe_install_library_file("static", LibFileName, GradeLibDir,
                LibSuccess, !IO),
            ( LibFileName = SharedLibFileName ->
                LibsSucceeded = LibSuccess
            ;
                maybe_install_library_file("shared", SharedLibFileName,
                    GradeLibDir, SharedLibSuccess, !IO),
                LibsSucceeded = LibSuccess `and` SharedLibSuccess
            )
        ),

        install_grade_init(GradeDir, ModuleName, InitSucceded, !IO),

        list.map_foldl2(
            install_grade_ints_and_headers(LinkSucceeded, GradeDir),
            AllModules, IntsHeadersSucceeded, !Info, !IO),
        Succeeded =
            bool.and_list([LibsSucceeded, InitSucceded | IntsHeadersSucceeded])
    ;
        DirResult = no,
        Succeeded = no
    ).

    % Install the `.init' file for the current grade.
    %
:- pred install_grade_init(string::in, module_name::in, bool::out,
    io::di, io::uo) is det.

install_grade_init(GradeDir, ModuleName, Succeeded, !IO) :-
    globals.io_lookup_string_option(install_prefix, Prefix, !IO),
    GradeModulesDir = Prefix / "lib" / "mercury" / "modules" / GradeDir,
    module_name_to_file_name(ModuleName, ".init", no, InitFileName, !IO),
    install_file(InitFileName, GradeModulesDir, Succeeded, !IO).

    % Install the `.opt', `.analysis' and `.mih' files for the current grade.
    %
:- pred install_grade_ints_and_headers(bool::in, string::in, module_name::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

install_grade_ints_and_headers(LinkSucceeded, GradeDir, ModuleName, Succeeded,
        !Info, !IO) :-
    get_module_dependencies(ModuleName, MaybeImports, !Info, !IO),
    (
        MaybeImports = yes(Imports),
        globals.io_lookup_string_option(install_prefix, Prefix, !IO),
        LibDir = Prefix/"lib"/"mercury",

        globals.io_get_target(Target, !IO),
        globals.io_lookup_bool_option(highlevel_code, HighLevelCode, !IO),
        (
            (
                Target = target_c,
                HighLevelCode = yes
            ;
                Target = target_asm,
                Imports ^ has_foreign_code = contains_foreign_code(_)
            )
        ->
            GradeIncDir = LibDir/"lib"/GradeDir/"inc",
            install_subdir_file(LinkSucceeded, GradeIncDir, ModuleName, "mih",
                HeaderSucceeded1, !IO),

            % This is needed so that the file will be
            % found in Mmake's VPATH.
            IntDir = LibDir/"ints",
            install_subdir_file(LinkSucceeded, IntDir, ModuleName, "mih",
                HeaderSucceeded2, !IO),

            HeaderSucceeded = HeaderSucceeded1 `and` HeaderSucceeded2
        ;
            HeaderSucceeded = yes
        ),

        GradeIntDir = LibDir/"ints"/GradeDir,
        globals.io_lookup_bool_option(intermodule_optimization, Intermod, !IO),
        (
            Intermod = yes,
            install_subdir_file(LinkSucceeded, GradeIntDir, ModuleName, "opt",
                OptSucceeded, !IO)
        ;
            Intermod = no,
            OptSucceeded = yes
        ),

        globals.io_lookup_bool_option(intermodule_analysis, IntermodAnalysis,
            !IO),
        (
            IntermodAnalysis = yes,
            install_subdir_file(LinkSucceeded, GradeIntDir,
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
:- pred install_subdir_file(bool::in, dir_name::in, module_name::in,
    string::in, bool::out, io::di, io::uo) is det.

install_subdir_file(SubdirLinkSucceeded, InstallDir, ModuleName, Ext,
        Succeeded, !IO) :-
    module_name_to_file_name(ModuleName, "." ++ Ext, no, FileName, !IO),
    install_file(FileName, InstallDir, Succeeded1, !IO),
    (
        SubdirLinkSucceeded = no,
        install_file(FileName, InstallDir/"Mercury"/(Ext ++ "s"), Succeeded2,
            !IO),
        Succeeded = Succeeded1 `and` Succeeded2
    ;
        SubdirLinkSucceeded = yes,
        Succeeded = Succeeded1
    ).

:- pred maybe_install_library_file(string::in, file_name::in, dir_name::in,
    bool::out, io::di, io::uo) is det.

maybe_install_library_file(Linkage, FileName, InstallDir, Succeeded, !IO) :-
    globals.io_lookup_accumulating_option(lib_linkages, LibLinkages, !IO),
    ( list.member(Linkage, LibLinkages) ->
        install_file(FileName, InstallDir, Succeeded0, !IO),
        %
        % We need to update the archive index after we copy a .a file to
        % the installation directory because the linkers on some OSs
        % complain if we don't.
        % 
        (
            Linkage = "static",
            Succeeded0 = yes
        ->
            % Since mmc --make uses --use-subdirs the above FileName will
            % be directory qualified.  We don't care about the build
            % directory here so we strip that qualification off.
            %
            BaseFileName = dir.det_basename(FileName),
            generate_archive_index(BaseFileName, InstallDir, Succeeded, !IO)
        ;
            Succeeded = Succeeded0
        )
    ;
        Succeeded = yes
    ).

:- pred install_file(file_name::in, dir_name::in, bool::out,
    io::di, io::uo) is det.

install_file(FileName, InstallDir, Succeeded, !IO) :-
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Installing file ", !IO),
            io.write_string(FileName, !IO),
            io.write_string(" in ", !IO),
            io.write_string(InstallDir, !IO),
            io.nl(!IO)
        ), !IO),
    globals.io_lookup_string_option(install_command, InstallCommand, !IO),
    Command = string.join_list("   ", list.map(quote_arg,
        [InstallCommand, FileName, InstallDir])),
    io.output_stream(OutputStream, !IO),
    invoke_system_command(OutputStream, cmd_verbose, Command, Succeeded, !IO).

:- pred make_install_dirs(bool::out, bool::out, io::di, io::uo) is det.

make_install_dirs(Result, LinkResult, !IO) :-
    globals.io_lookup_string_option(install_prefix, Prefix, !IO),
    LibDir = Prefix/"lib"/"mercury",
    make_directory(LibDir/"inc", Result1, !IO),
    make_directory(LibDir/"modules", Result2, !IO),

    IntsSubdir = LibDir/"ints"/"Mercury",
    make_directory(IntsSubdir, Result3, !IO),
    Results0 = [Result1, Result2, Result3],

    Subdirs = ["int0", "int", "int2", "int3", "opt", "trans_opt",
        "mh", "mih", "module_dep"],
    list.map_foldl(make_install_symlink(IntsSubdir), Subdirs, LinkResults,
        !IO),
    LinkResult = bool.and_list(LinkResults),
    (
        LinkResult = yes,
        Results = Results0
    ;
        LinkResult = no,
        list.map_foldl(
            (pred(Ext::in, MkDirResult::out, !.IO::di, !:IO::uo) is det:-
                make_directory(IntsSubdir/(Ext ++ "s"), MkDirResult, !IO)
            ), Subdirs, MkDirResults, !IO),
        Results = Results0 ++ MkDirResults
    ),
    print_mkdir_errors(Results, Result, !IO).

:- pred make_grade_install_dirs(string::in, bool::out, bool::out,
    io::di, io::uo) is det.

make_grade_install_dirs(Grade, Result, LinkResult, !IO) :-
    globals.io_lookup_string_option(install_prefix, Prefix, !IO),
    LibDir = Prefix/"lib"/"mercury",

    GradeIntsSubdir = LibDir/"ints"/Grade/"Mercury",
    make_directory(GradeIntsSubdir, Result1, !IO),

    GradeIncSubdir = LibDir/"lib"/Grade/"inc"/"Mercury",
    make_directory(GradeIncSubdir, Result2, !IO),

    GradeModuleSubdir = LibDir/"modules"/Grade,
    make_directory(GradeModuleSubdir, Result3, !IO),

    Results0 = [Result1, Result2, Result3],

    make_install_symlink(GradeIncSubdir, "mih", LinkResult0, !IO),
    list.map_foldl(make_install_symlink(GradeIntsSubdir),
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

:- pred make_install_symlink(string::in, string::in, bool::out,
    io::di, io::uo) is det.

make_install_symlink(Subdir, Ext, Succeeded, !IO) :-
    LinkName = Subdir/(Ext ++ "s"),
    maybe_make_symlink("..", LinkName, Succeeded, !IO).

    % Generate (or update) the index for an archive file,
    % i.e. run ranlib on a .a file.
    %
:- pred generate_archive_index(file_name::in, dir_name::in, bool::out,
    io::di, io::uo) is det.

generate_archive_index(FileName, InstallDir, Succeeded, !IO) :-
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Generating archive index for file ", !IO),
            io.write_string(FileName, !IO),
            io.write_string(" in ", !IO),
            io.write_string(InstallDir, !IO),
            io.nl(!IO)
        ), !IO),
    globals.io_lookup_string_option(ranlib_command, RanLibCommand, !IO),
    globals.io_lookup_string_option(ranlib_flags, RanLibFlags, !IO),
    Command = string.join_list("    ", [
        quote_arg(RanLibCommand),
        RanLibFlags,
        quote_arg(InstallDir / FileName)
    ]),
    io.output_stream(OutputStream, !IO),
    invoke_system_command(OutputStream, cmd_verbose, Command, Succeeded, !IO).

%-----------------------------------------------------------------------------%

    % Clean up grade-dependent files.
    %
:- pred make_grade_clean(module_name::in, list(module_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_grade_clean(ModuleName, AllModules, !Info, !IO) :-
    make_main_module_realclean(ModuleName, !Info, !IO),
    list.foldl2(make_module_clean, AllModules, !Info, !IO).

:- pred make_main_module_realclean(module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_main_module_realclean(ModuleName, !Info, !IO) :-
    linked_target_file_name(ModuleName, executable, ExeFileName, !IO),
    linked_target_file_name(ModuleName, static_library, LibFileName, !IO),
    linked_target_file_name(ModuleName, shared_library, SharedLibFileName,
        !IO),
    linked_target_file_name(ModuleName, java_archive, JarFileName, !IO),

    % Remove the symlinks created for `--use-grade-subdirs'.
    globals.io_lookup_bool_option(use_grade_subdirs, UseGradeSubdirs, !IO),
    globals.io_set_option(use_grade_subdirs, bool(no), !IO),
    linked_target_file_name(ModuleName, executable, ThisDirExeFileName, !IO),
    linked_target_file_name(ModuleName, static_library,
        ThisDirLibFileName, !IO),
    linked_target_file_name(ModuleName, shared_library,
        ThisDirSharedLibFileName, !IO),
    linked_target_file_name(ModuleName, java_archive, ThisDirJarFileName, !IO),
    globals.io_set_option(use_grade_subdirs, bool(UseGradeSubdirs), !IO),

    list.foldl2(make_remove_file,
        [ExeFileName, LibFileName, SharedLibFileName, JarFileName,
        ThisDirExeFileName, ThisDirLibFileName,
        ThisDirSharedLibFileName, ThisDirJarFileName],
        !Info, !IO),
    make_remove_file(ModuleName, ".init", !Info, !IO),
    remove_init_files(ModuleName, !Info, !IO).

:- pred remove_init_files(module_name::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

remove_init_files(ModuleName, !Info, !IO) :-
    globals.io_lookup_string_option(object_file_extension, ObjExt, !IO),
    globals.io_lookup_string_option(pic_object_file_extension, PicObjExt,
        !IO),
    globals.io_lookup_string_option(link_with_pic_object_file_extension,
        LinkWithPicObjExt, !IO),
    list.foldl2(make_remove_file(ModuleName), ["_init.c", "_init" ++ ObjExt,
        "_init" ++ PicObjExt, "_init" ++ LinkWithPicObjExt],
        !Info, !IO).

:- pred make_module_clean(module_name::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

make_module_clean(ModuleName, !Info, !IO) :-
    list.foldl2(make_remove_target_file(ModuleName),
        [module_target_errors, module_target_c_code,
        module_target_c_header(header_mih), module_target_il_code,
        module_target_java_code], !Info, !IO),

    list.foldl2(make_remove_file(ModuleName), [".used", ".prof"], !Info, !IO),

    get_module_dependencies(ModuleName, MaybeImports, !Info, !IO),
    (
        MaybeImports = yes(Imports),
        FactTableFiles = Imports ^ fact_table_deps
    ;
        MaybeImports = no,
        FactTableFiles = []
    ),

    list.foldl2(
        (pred(FactTableFile::in, !.Info::in, !:Info::out, di, uo) is det -->
            fact_table_file_name(ModuleName, FactTableFile,
                ".c", no, FactTableCFile),
            make_remove_file(FactTableCFile, !Info)
        ), FactTableFiles, !Info, !IO),

    CCodeModule = foreign_language_module_name(ModuleName, lang_c),
    make_remove_target_file(CCodeModule, module_target_c_code, !Info, !IO),

    % Remove object and assembler files.
    list.foldl2(
        (pred(PIC::in, !.Info::in, !:Info::out, !.IO::di, !:IO::uo) is det :-
        make_remove_target_file(ModuleName, module_target_object_code(PIC),
            !Info, !IO),
        make_remove_target_file(ModuleName, module_target_asm_code(PIC),
            !Info, !IO),
        make_remove_target_file(ModuleName,
            module_target_foreign_object(PIC, lang_c), !Info, !IO),
        list.foldl2(
            (pred(FactTableFile::in, !.Info::in, !:Info::out,
                    !.IO::di, !:IO::uo) is det :-
                make_remove_target_file(ModuleName,
                    module_target_fact_table_object(PIC, FactTableFile),
                    !Info, !IO)
            ), FactTableFiles, !Info, !IO)
        ),
        [pic, link_with_pic, non_pic], !Info, !IO),

    % Remove IL foreign code files.
    CSharpModule = foreign_language_module_name(ModuleName, lang_csharp),
    make_remove_file(CSharpModule,
        foreign_language_file_extension(lang_csharp), !Info, !IO),
    make_remove_target_file(CSharpModule,
        module_target_foreign_il_asm(lang_csharp), !Info, !IO),

    McppModule = foreign_language_module_name(ModuleName,
        lang_managed_cplusplus),
    make_remove_file(McppModule,
        foreign_language_file_extension(lang_managed_cplusplus),
        !Info, !IO),
    make_remove_target_file(McppModule,
        module_target_foreign_il_asm(lang_managed_cplusplus), !Info, !IO).

:- pred make_module_realclean(module_name::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

make_module_realclean(ModuleName, !Info, !IO) :-
    make_module_clean(ModuleName, !Info, !IO),
    list.foldl2(make_remove_target_file(ModuleName),
        [
            module_target_private_interface, 
            module_target_long_interface, 
            module_target_short_interface,
            module_target_unqualified_short_interface, 
            module_target_intermodule_interface, 
            module_target_analysis_registry,
            module_target_c_header(header_mh)
        ],
        !Info, !IO),
    make_remove_file(ModuleName, make_module_dep_file_extension, !Info, !IO),
    make_remove_file(ModuleName, ".imdg", !Info, !IO),
    make_remove_file(ModuleName, ".request", !Info, !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "make.program_target.m".

%-----------------------------------------------------------------------------%
:- end_module make.program_target.
%-----------------------------------------------------------------------------%
