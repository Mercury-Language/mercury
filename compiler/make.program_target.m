%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2008 The University of Melbourne.
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
:- import_module libs.compiler_util.
:- import_module libs.handle_options.
:- import_module libs.process_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_out.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module digraph.
:- import_module dir.
:- import_module getopt_io.
:- import_module svmap.

%-----------------------------------------------------------------------------%

make_linked_target(LinkedTargetFile, LinkedTargetSucceeded, !Info, !IO) :-
    LinkedTargetFile = linked_target_file(_MainModuleName, FileType),
    (
        FileType = shared_library,
        ExtraOptions = ["--compile-to-shared-lib"]
    ;
        ( FileType = executable
        ; FileType = java_archive
        ; FileType = erlang_archive
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
        LinkedTargetSucceeded = yes
    ;
        globals.io_lookup_bool_option(libgrade_install_check,
            LibgradeCheck, !IO),
        (
            LibgradeCheck = yes,
            check_libraries_are_installed(LibgradeCheckSucceeded, !IO)
        ;
            LibgradeCheck = no,
            LibgradeCheckSucceeded = yes
        ),
        (
            LibgradeCheckSucceeded = yes,
            maybe_with_analysis_cache_dir(
                make_linked_target_1(LinkedTargetFile, ExtraOptions),
                LinkedTargetSucceeded, !Info, !IO)
        ;
            LibgradeCheckSucceeded = no,
            LinkedTargetSucceeded = no
        )
    ).

:- pred make_linked_target_1(linked_target_file::in, list(string)::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

make_linked_target_1(LinkedTargetFile, ExtraOptions, Succeeded, !Info, !IO) :-
    LinkedTargetFile = linked_target_file(MainModuleName, _FileType),

    % When using `--intermodule-analysis', perform an analysis pass
    % first.  The analysis of one module may invalidate the results of
    % a module we analysed earlier, so this step must be carried out
    % until all the `.analysis' files are in a valid state before we
    % can continue.
    globals.io_lookup_bool_option(intermodule_analysis, IntermodAnalysis, !IO),
    (
        IntermodAnalysis = yes,
        make_misc_target_builder(
            MainModuleName - misc_target_build_analyses,
            ExtraOptions, IntermodAnalysisSucceeded, !Info, !IO)
    ;
        IntermodAnalysis = no,
        IntermodAnalysisSucceeded = yes
    ),

    (
        IntermodAnalysisSucceeded = yes,
        build_with_module_options(MainModuleName, ExtraOptions,
            make_linked_target_2(LinkedTargetFile),
            Succeeded, !Info, !IO)
    ;
        IntermodAnalysisSucceeded = no,
        Succeeded = no
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
            ObjectTargetType = module_target_java_class_code
        ;
            CompilationTarget = target_x86_64,
            sorry(this_file, "mmc --make and target x86_64")
        ;
            CompilationTarget = target_erlang,
            IntermediateTargetType = module_target_erlang_code,
            ObjectTargetType = module_target_erlang_beam_code
        ),

        AllModulesList = set.to_sorted_list(AllModules),
        get_target_modules(IntermediateTargetType,
            AllModulesList, ObjModules, !Info, !IO),
        IntermediateTargets = make_dependency_list(ObjModules,
            IntermediateTargetType),
        ObjTargets = make_dependency_list(ObjModules, ObjectTargetType),

        list.map_foldl2(get_foreign_object_targets(PIC),
            ObjModules, ForeignObjTargetsList, !Info, !IO),
        ForeignObjTargets = list.condense(ForeignObjTargetsList),

        % Ensure all interface files are present before continuing.
        % This prevents a problem when two parallel branches try to generate
        % the same missing interface file later.

        make_all_interface_files(AllModulesList, IntsSucceeded, !Info, !IO),
        ( IntsSucceeded = no, KeepGoing = no ->
            BuildDepsSucceeded = no
        ;
            foldl2_maybe_stop_at_error_maybe_parallel(KeepGoing,
                make_module_target, IntermediateTargets,
                BuildDepsSucceeded0, !Info, !IO),
            (
                BuildDepsSucceeded0 = yes,
                maybe_make_java_files(MainModuleName,
                    ObjectTargetType, ObjModules, BuildDepsSucceeded1,
                    !Info, !IO)
            ;
                BuildDepsSucceeded0 = no,
                BuildDepsSucceeded1 = no
            ),
            (
                BuildDepsSucceeded1 = yes,
                foldl2_maybe_stop_at_error_maybe_parallel(KeepGoing,
                    make_module_target, ObjTargets, BuildDepsSucceeded2,
                    !Info, !IO)
            ;
                BuildDepsSucceeded1 = no,
                BuildDepsSucceeded2 = no
            ),
            (
                BuildDepsSucceeded2 = yes,
                foldl2_maybe_stop_at_error(KeepGoing, make_module_target,
                    ForeignObjTargets,
                    BuildDepsSucceeded, !Info, !IO)
            ;
                BuildDepsSucceeded2 = no,
                BuildDepsSucceeded = no
            )
        ),

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
        ; CompilationTarget = target_x86_64
        ; CompilationTarget = target_erlang
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
        (
            ( CompilationTarget = target_c
            ; CompilationTarget = target_asm
            ),
            make_init_obj_file(ErrorStream, MainModuleName,
                AllModulesList, InitObjectResult, !IO),
            MaybeInitObjectResult = yes(InitObjectResult)
        ;
            CompilationTarget = target_erlang,
            make_erlang_program_init_file(ErrorStream, MainModuleName,
                AllModulesList, InitObjectResult, !IO),
            MaybeInitObjectResult = yes(InitObjectResult)
        ;
            ( CompilationTarget = target_il
            ; CompilationTarget = target_java
            ; CompilationTarget = target_x86_64
            ),
            MaybeInitObjectResult = no
        ),
        (
            MaybeInitObjectResult = yes(InitObjectResult1),
            (
                InitObjectResult1 = yes(InitObject),
                % We may need to update the timestamp of the `_init.o'
                % or `_init.beam' file.
                !:Info = !.Info ^ file_timestamps :=
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
        )
    ;
        ( FileType = static_library
        ; FileType = shared_library
        ; FileType = java_archive
        ; FileType = erlang_archive
        ),
        DepsResult2 = BuildDepsResult,
        InitObjects = []
    ),

    ObjectsToCheck = InitObjects ++ LinkObjects,

    % Report errors if any of the extra objects aren't present.
    list.map_foldl2(dependency_status,
        list.map((func(F) = dep_file(F, no)), ObjectsToCheck),
            ExtraObjStatus, !Info, !IO),

    ( list.member(deps_status_error, ExtraObjStatus) ->
        DepsResult3 = deps_error
    ;
        DepsResult3 = DepsResult2
    ),
    BuildDepsSuccess = ( DepsResult3 \= deps_error -> yes ; no ),
    list.map_foldl2(get_file_timestamp([dir.this_directory]),
        ObjectsToCheck, ExtraObjectTimestamps, !Info, !IO),
    check_dependency_timestamps(OutputFileName, MaybeTimestamp,
        BuildDepsSuccess, ObjectsToCheck, io.write,
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
        file_error(OutputFileName, !IO),
        Succeeded = no
    ;
        DepsResult = deps_up_to_date,
        MsgTarget = MainModuleName - linked_target(FileType),
        globals.io_lookup_bool_option(use_grade_subdirs, UseGradeSubdirs, !IO),
        (
            UseGradeSubdirs = yes,
            post_link_make_symlink_or_copy(ErrorStream,
                FileType, MainModuleName, Succeeded, MadeSymlinkOrCopy, !IO),
            (
                MadeSymlinkOrCopy = yes,
                maybe_symlink_or_copy_linked_target_message(MsgTarget, !IO)
            ;
                MadeSymlinkOrCopy = no,
                maybe_warn_up_to_date_target(MsgTarget, !Info, !IO)
            )
        ;
            UseGradeSubdirs = no,
            maybe_warn_up_to_date_target(MsgTarget, !Info, !IO),
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

        (
            ( CompilationTarget = target_c
            ; CompilationTarget = target_asm
            ; CompilationTarget = target_x86_64
            ),
            maybe_pic_object_file_extension(PIC, ObjExtToUse, !IO)
        ;
            CompilationTarget = target_il,
            ObjExtToUse = ".dll"
        ;
            CompilationTarget = target_java,
            globals.io_lookup_string_option(java_object_file_extension,
                ObjExtToUse, !IO)
        ;
            CompilationTarget = target_erlang,
            globals.io_lookup_string_option(erlang_object_file_extension,
                ObjExtToUse, !IO)
        ),
        list.map_foldl(
            (pred(ObjModule::in, ObjToLink::out, !.IO::di, !:IO::uo) is det :-
                module_name_to_file_name(ObjModule, ObjExtToUse,
                    do_not_create_dirs, ObjToLink, !IO)
            ), ObjModules, ObjList, !IO),

        % LinkObjects may contain `.a' files which must come
        % after all the object files on the linker command line.
        AllObjects = InitObjects ++ ObjList ++ ForeignObjects ++ LinkObjects,
        (
            ( CompilationTarget = target_c
            ; CompilationTarget = target_asm
            ; CompilationTarget = target_erlang
            ),
            % Run the link in a separate process so it can be killed
            % if an interrupt is received.
            call_in_forked_process(
                compile_target_code.link(ErrorStream,
                    FileType, MainModuleName, AllObjects),
                Succeeded, !IO)
        ;
            CompilationTarget = target_x86_64,
            sorry(this_file, "mmc --make and target x86_64")
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
    make_remove_file(verbose_make, OutputFileName, !Info, !IO),
    (
        FileType = executable,
        ( CompilationTarget = target_c
        ; CompilationTarget = target_asm
        )
    ->
        remove_init_files(verbose_make, MainModuleName, !Info, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % When compiling to Java we want to invoke `javac' just once, passing it a
    % list of all out-of-date `.java' files.  This is a lot quicker than
    % compiling each Java file individually.
    %
:- pred maybe_make_java_files(module_name::in, module_target_type::in,
    list(module_name)::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

maybe_make_java_files(MainModuleName, ObjectTargetType, ObjModules, Succeeded,
        !Info, !IO) :-
    ( ObjectTargetType = module_target_java_class_code ->
        out_of_date_java_modules(ObjModules, OutOfDateModules, !Info, !IO),
        (
            OutOfDateModules = [],
            Succeeded = yes
        ;
            OutOfDateModules = [_ | _],
            build_java_files(MainModuleName, OutOfDateModules, Succeeded,
                !Info, !IO),
            % javac might write more `.class' files than we anticipated (though
            % it probably won't) so clear out all the timestamps which might be
            % affected.
            Timestamps0 = !.Info ^ file_timestamps,
            map.foldl(delete_java_class_timestamps, Timestamps0,
                map.init, Timestamps),
            !Info ^ file_timestamps := Timestamps
        )
    ;
        Succeeded = yes
    ).

:- pred out_of_date_java_modules(list(module_name)::in, list(module_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

out_of_date_java_modules(ObjModules, OutOfDateModules, !Info, !IO) :-
    (
        ObjModules = [],
        OutOfDateModules = []
    ;
        ObjModules = [ModuleName | Rest],
        out_of_date_java_modules(Rest, OutOfDateModules0, !Info, !IO),
        JavaTarget = target_file(ModuleName, module_target_java_code),
        ClassTarget = target_file(ModuleName, module_target_java_class_code),
        get_target_timestamp(do_not_search, JavaTarget, MaybeJavaTimestamp,
            !Info, !IO),
        get_target_timestamp(do_not_search, ClassTarget, MaybeClassTimestamp,
            !Info, !IO),
        (
            MaybeJavaTimestamp = ok(JavaTimestamp),
            MaybeClassTimestamp = ok(ClassTimestamp),
            ClassTimestamp @>= JavaTimestamp
        ->
            OutOfDateModules = OutOfDateModules0
        ;
            OutOfDateModules = [ModuleName | OutOfDateModules0]
        )
    ).

:- pred build_java_files(module_name::in, list(module_name)::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_java_files(MainModuleName, ModuleNames, Succeeded, !Info, !IO) :-
    verbose_msg(io.write_string("Making Java class files\n"), !IO),
    ToJavaFile =
        (pred(ModuleName::in, JavaFile::out, !.IO::di, !:IO::uo) is det :-
            module_name_to_file_name(ModuleName, ".java", do_create_dirs,
                JavaFile, !IO)
        ),
    list.map_foldl(ToJavaFile, ModuleNames, JavaFiles, !IO),
    % We redirect errors to a file named after the main module.
    build_with_output_redirect(MainModuleName,
        build_java_files_2(JavaFiles), Succeeded, !Info, !IO).

:- pred build_java_files_2(list(string)::in, io.output_stream::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_java_files_2(JavaFiles, ErrorStream, Succeeded, !Info, !IO) :-
    call_in_forked_process(
        compile_target_code.compile_java_files(ErrorStream, JavaFiles),
        Succeeded, !IO).

:- pred delete_java_class_timestamps(string::in, maybe_error(timestamp)::in,
    file_timestamps::in, file_timestamps::out) is det.

delete_java_class_timestamps(FileName, MaybeTimestamp, !Timestamps) :-
    ( string.suffix(FileName, ".class") ->
        true
    ;
        svmap.det_insert(FileName, MaybeTimestamp, !Timestamps)
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
    % Don't rebuild .module_dep files when cleaning up.
    RebuildModuleDeps = !.Info ^ rebuild_module_deps,
    (
        ( TargetType = misc_target_clean
        ; TargetType = misc_target_realclean
        )
    ->
        !:Info = !.Info ^ rebuild_module_deps := do_not_rebuild_module_deps
    ;
        true
    ),
    find_reachable_local_modules(MainModuleName, Succeeded0, AllModulesSet,
        !Info, !IO),
    !:Info = !.Info ^ rebuild_module_deps := RebuildModuleDeps,
    AllModules = set.to_sorted_list(AllModulesSet),
    (
        TargetType = misc_target_clean,
        Succeeded = yes,
        list.foldl2(make_module_clean, AllModules, !Info, !IO),
        remove_init_files(very_verbose, MainModuleName, !Info, !IO)
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
            maybe_with_analysis_cache_dir(
                foldl2_maybe_stop_at_error(KeepGoing,
                    make_module_target,
                    make_dependency_list(TargetModules, ModuleTargetType)),
                Succeeded1, !Info, !IO),
            Succeeded = Succeeded0 `and` Succeeded1
        )
    ;
        TargetType = misc_target_build_analyses,
        maybe_with_analysis_cache_dir(
            build_analysis_files(MainModuleName, AllModules, Succeeded0),
            Succeeded, !Info, !IO)
    ;
        TargetType = misc_target_build_library,
        make_all_interface_files(AllModules, IntSucceeded, !Info, !IO),
        (
            IntSucceeded = yes,
            maybe_with_analysis_cache_dir(
                build_library(MainModuleName, AllModules),
                Succeeded, !Info, !IO)
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
    ;
        TargetType = misc_target_build_xml_docs,
        get_target_modules(module_target_xml_doc, AllModules,
            TargetModules, !Info, !IO),
        globals.io_lookup_bool_option(keep_going, KeepGoing, !IO),
        ( Succeeded0 = no, KeepGoing = no ->
            Succeeded = no
        ;
            foldl2_maybe_stop_at_error(KeepGoing,
                make_module_target,
                make_dependency_list(TargetModules, module_target_xml_doc),
                Succeeded1, !Info, !IO),
            Succeeded = Succeeded0 `and` Succeeded1
        )
    ).

%-----------------------------------------------------------------------------%

:- pred make_all_interface_files(list(module_name)::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_all_interface_files(AllModules, Succeeded, !Info, !IO) :-
    ShortInts = make_dependency_list(AllModules,
        module_target_unqualified_short_interface),
    LongInts = make_dependency_list(AllModules,
        module_target_long_interface),
    globals.io_get_any_intermod(AnyIntermod, !IO),
    (
        AnyIntermod = yes,
        OptFiles = make_dependency_list(AllModules,
            module_target_intermodule_interface)
    ;
        AnyIntermod = no,
        OptFiles = []
    ),
    globals.io_lookup_bool_option(keep_going, KeepGoing, !IO),
    foldl2_maybe_stop_at_error(KeepGoing,
        foldl2_maybe_stop_at_error(KeepGoing, make_module_target),
        [ShortInts, LongInts, OptFiles],
        Succeeded, !Info, !IO).

%-----------------------------------------------------------------------------%

    % If `--analysis-file-cache' is enabled, create a temporary directory for
    % holding analysis cache files and pass that to child processes.
    % After P is finished, remove the cache directory completely.
    %
:- pred maybe_with_analysis_cache_dir(build0(make_info)::in(build0),
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

maybe_with_analysis_cache_dir(P, Succeeded, !Info, !IO) :-
    globals.io_lookup_bool_option(intermodule_analysis, IntermodAnalysis, !IO),
    globals.io_lookup_bool_option(analysis_file_cache, Caching, !IO),
    globals.io_lookup_string_option(analysis_file_cache_dir, CacheDir0, !IO),
    CacheDirOption = "--analysis-file-cache-dir",
    (
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
    ->
        P(Succeeded, !Info, !IO)
    ;
        create_analysis_cache_dir(Succeeded0, CacheDir, !IO),
        (
            Succeeded0 = yes,
            OrigOptionArgs = !.Info ^ option_args,
            % Pass the name of the cache directory to child processes
            !Info ^ option_args := OrigOptionArgs ++
                [CacheDirOption, CacheDir],
            build_with_check_for_interrupt(P, remove_cache_dir(CacheDir),
                Succeeded, !Info, !IO),
            remove_cache_dir(CacheDir, !Info, !IO),
            !Info ^ option_args := OrigOptionArgs
        ;
            Succeeded0 = no,
            Succeeded = no
        )
    ).

:- pred create_analysis_cache_dir(bool::out, string::out, io::di, io::uo)
    is det.

create_analysis_cache_dir(Succeeded, CacheDir, !IO) :-
    choose_cache_dir_name(CacheDir, !IO),
    verbose_msg(verbose_make,
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

:- pred choose_cache_dir_name(string::out, io::di, io::uo) is det.

choose_cache_dir_name(DirName, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_bool_option(Globals, use_grade_subdirs, UseGradeSubdirs),
    globals.lookup_string_option(Globals, fullarch, FullArch),
    (
        UseGradeSubdirs = yes,
        grade_directory_component(Globals, Grade),
        DirComponents = ["Mercury", Grade, FullArch, "Mercury",
            "analysis_cache"]
    ;
        UseGradeSubdirs = no,
        DirComponents = ["Mercury", "analysis_cache"]
    ),
    DirName = dir.relative_path_name_from_components(DirComponents).

:- pred remove_cache_dir(string::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

remove_cache_dir(CacheDir, !Info, !IO) :-
    verbose_msg(verbose_make,
        io.format("Removing %s\n", [s(CacheDir)]), !IO),
    io.remove_file_recursively(CacheDir, _, !IO).

%-----------------------------------------------------------------------------%

:- pred build_analysis_files(module_name::in, list(module_name)::in,
    bool::in, bool::out, make_info::in, make_info::out, io::di, io::uo)
    is det.

build_analysis_files(MainModuleName, AllModules, Succeeded0, Succeeded,
        !Info, !IO) :-
    globals.io_lookup_bool_option(keep_going, KeepGoing, !IO),
    (
        Succeeded0 = no,
        KeepGoing = no
    ->
        Succeeded = no
    ;
        % Ensure all interface files are present before continuing.  This
        % prevents a problem when two parallel branches try to generate the
        % same missing interface file later.
        % (Although we can't actually build analysis files in parallel yet.)
        make_all_interface_files(AllModules, Succeeded1, !Info, !IO),
        (
            Succeeded1 = no,
            KeepGoing = no
        ->
            Succeeded = no
        ;
            build_analysis_files_1(MainModuleName, AllModules,
                Succeeded, !Info, !IO)
        )
    ).

:- pred build_analysis_files_1(module_name::in, list(module_name)::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

build_analysis_files_1(MainModuleName, AllModules, Succeeded, !Info, !IO) :-
    get_target_modules(module_target_analysis_registry, AllModules,
        TargetModules0, !Info, !IO),
    reverse_ordered_modules(!.Info ^ module_dependencies,
        TargetModules0, TargetModules1),
    % Filter out the non-local modules so we don't try to reanalyse them.
    list.filter((pred(Mod::in) is semidet :- list.member(Mod, AllModules)),
        TargetModules1, TargetModules),
    make_local_module_id_options(MainModuleName, Succeeded0,
        LocalModulesOpts, !Info, !IO),
    (
        Succeeded0 = yes,
        build_analysis_files_2(MainModuleName, TargetModules,
            LocalModulesOpts, Succeeded0, Succeeded, !Info, !IO)
    ;
        Succeeded0 = no,
        Succeeded = no
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
        Modules0, digraph.init, _IntDepsGraph, digraph.init, ImplDepsGraph),
    digraph.atsort(ImplDepsGraph, Order0),
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
    read_module_overall_status(mmc, Module, ModuleStatus, !IO),
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
    ).

:- pred reset_analysis_registry_dependency_status(module_name::in,
    make_info::in, make_info::out) is det.

reset_analysis_registry_dependency_status(ModuleName, !Info) :-
    Dep = dep_target(target_file(ModuleName, module_target_analysis_registry)),
    !Info ^ dependency_status ^ elem(Dep) := deps_status_not_considered.

%-----------------------------------------------------------------------------%

:- pred build_library(module_name::in, list(module_name)::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_library(MainModuleName, AllModules, Succeeded, !Info, !IO) :-
    globals.io_get_target(Target, !IO),
    (
        ( Target = target_c
        ; Target = target_asm
        ),
        build_c_library(MainModuleName, AllModules, Succeeded, !Info, !IO)
    ;
        Target = target_il,
        sorry(this_file, "build_library: target IL not supported yet")
    ;
        Target = target_java,
        build_java_library(MainModuleName, Succeeded, !Info, !IO)
    ;
        Target = target_x86_64,
        sorry(this_file, "build_library: target x86_64 not supported yet")
    ;
        Target = target_erlang,
        build_erlang_library(MainModuleName, AllModules, Succeeded, !Info, !IO)
    ).

:- pred build_c_library(module_name::in, list(module_name)::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_c_library(MainModuleName, AllModules, Succeeded, !Info, !IO) :-
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
        % We can only build the .init file if we have succesfully built
        % the .c files.
        (
            SharedLibsSucceeded = yes,
            % Errors while making the .init file should be very rare.
            io.output_stream(ErrorStream, !IO),
            make_library_init_file(ErrorStream, MainModuleName, AllModules,
                Succeeded, !IO)
        ;
            SharedLibsSucceeded = no,
            Succeeded = no
        )
    ;
        StaticSucceeded = no,
        Succeeded = no
    ).

:- pred build_java_library(module_name::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_java_library(MainModuleName, Succeeded, !Info, !IO) :-
    make_linked_target(
        linked_target_file(MainModuleName, java_archive),
        Succeeded, !Info, !IO).

:- pred build_erlang_library(module_name::in, list(module_name)::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

build_erlang_library(MainModuleName, AllModules, Succeeded, !Info, !IO) :-
    make_linked_target(
        linked_target_file(MainModuleName, erlang_archive),
        Succeeded0, !Info, !IO),
    (
        Succeeded0 = yes,
        % Errors while making the .init file should be very rare.
        io.output_stream(ErrorStream, !IO),
        make_erlang_library_init_file(ErrorStream, MainModuleName, AllModules,
            Succeeded, !IO)
    ;
        Succeeded0 = no,
        Succeeded = no
    ).

%-----------------------------------------------------------------------------%

:- pred install_library(module_name::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

install_library(MainModuleName, Succeeded, !Info, !IO) :-
    find_reachable_local_modules(MainModuleName, DepsSuccess, AllModules0,
        !Info, !IO),
    AllModules = set.to_sorted_list(AllModules0),
    make_install_dirs(DirSucceeded, LinkSucceeded, !IO),
    (
        DepsSuccess = yes,
        DirSucceeded = yes
    ->
        list.map_foldl2(install_ints_and_headers(LinkSucceeded), AllModules,
            IntsSucceeded, !Info, !IO),
        install_extra_headers(ExtraHdrsSucceeded, !IO),
        
        globals.io_get_globals(Globals, !IO),
        grade_directory_component(Globals, Grade),
        install_library_grade_files(LinkSucceeded, Grade, MainModuleName,
            AllModules, GradeSucceeded, !Info, !IO),
        (
            bool.and_list([ExtraHdrsSucceeded | IntsSucceeded]) = yes,
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
        globals.io_get_any_intermod(AnyIntermod, !IO),
        (
            AnyIntermod = yes,
            % `.int0' files are imported by `.opt' files.
            (
                Imports ^ children = [_ | _],
                Exts = ["int0", "opt"]
            ;
                Imports ^ children = [],
                Exts = ["opt"]
            )
        ;
            AnyIntermod = no,
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
            ( Target = target_c
            ; Target = target_asm
            ),
            % XXX Should we test
            % Imports ^ contains_foreign_export = contains_foreign_export?
            module_name_to_file_name(ModuleName, ".mh", do_not_create_dirs,
                FileName, !IO),
            install_file(FileName, LibDir/"inc", HeaderSucceeded1, !IO),

            % This is needed so that the file will be found in Mmake's VPATH.
            install_subdir_file(SubdirLinkSucceeded, LibDir/"ints", ModuleName,
                "mh", HeaderSucceeded2, !IO),

            HeaderSucceeded = HeaderSucceeded1 `and` HeaderSucceeded2
        ;
            Target = target_erlang,
            module_name_to_file_name(ModuleName, ".hrl", do_not_create_dirs,
                FileName, !IO),
            install_file(FileName, LibDir/"inc", HeaderSucceeded, !IO)
        ;
            ( Target = target_java
            ; Target = target_il
            ; Target = target_x86_64
            ),
            HeaderSucceeded = yes
        ),
        Succeeded = bool.and_list([HeaderSucceeded | Results])
    ;
        MaybeImports = no,
        Succeeded = no
    ).

:- pred install_extra_headers(bool::out, io::di, io::uo) is det.

install_extra_headers(ExtraHdrsSucceeded, !IO) :-
    globals.io_lookup_accumulating_option(extra_library_header, ExtraHdrs,
        !IO),
    globals.io_lookup_string_option(install_prefix, Prefix, !IO),
    IncDir = Prefix / "lib" / "mercury" / "inc",
    list.foldl2(install_extra_header(IncDir), ExtraHdrs,
        yes, ExtraHdrsSucceeded, !IO).

:- pred install_extra_header(dir_name::in, string::in, bool::in, bool::out,
    io::di, io::uo) is det.

install_extra_header(IncDir, FileName, !Succeeded, !IO) :-
    install_file(FileName, IncDir, InstallSucceeded, !IO),
    !:Succeeded = bool.and(InstallSucceeded, !.Succeeded). 

:- pred install_library_grade(bool::in, module_name::in, list(module_name)::in,
    string::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

install_library_grade(LinkSucceeded0, ModuleName, AllModules, Grade, Succeeded,
        Info0, Info, !IO) :-
    globals.io_get_globals(OrigGlobals, !IO),

    % Only remove grade-dependent files after installing if
    % --use-grade-subdirs is not specified by the user.
    globals.lookup_bool_option(OrigGlobals,
        use_grade_subdirs, UseGradeSubdirs),
    CleanAfter = not(UseGradeSubdirs),

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
        Succeeded = no,
        Info = Info0
    ;
        OptionsErrors = [],

        % Remove the grade-dependent targets from the status map
        % (we need to rebuild them in the new grade).

        % XXX version_hash_table.delete is not working properly so just clear
        % the dependency status cache completely.
        %
        % StatusMap0 = Info0 ^ dependency_status,
        % StatusMap = version_hash_table.fold(remove_grade_dependent_targets,
        %     StatusMap0, StatusMap0),
        StatusMap = version_hash_table.new_default(dependency_file_hash),

        Info1 = (Info0 ^ dependency_status := StatusMap)
            ^ option_args := OptionArgs,

        % Building the library in the new grade is done in a separate process
        % to make it easier to stop and clean up on an interrupt.
        Cleanup = maybe_make_grade_clean(CleanAfter, ModuleName, AllModules),
        build_with_check_for_interrupt(
            ( pred(GradeSuccess::out, MInfo::in, MInfo::out, !.IO::di, !:IO::uo)
                    is det :-
                call_in_forked_process(
                    (pred(GradeSuccess0::out, !.IO::di, !:IO::uo) is det :-
                        install_library_grade_2(LinkSucceeded0,
                            ModuleName, AllModules, MInfo, CleanAfter,
                            GradeSuccess0, !IO)
                    ), GradeSuccess, !IO)
            ), Cleanup, Succeeded, Info1, Info, !IO)
    ),
    globals.io_set_globals(OrigGlobals, !IO).

:- func remove_grade_dependent_targets(dependency_file, dependency_status,
    version_hash_table(dependency_file, dependency_status)) =
    version_hash_table(dependency_file, dependency_status).

remove_grade_dependent_targets(File, _Status, StatusMap0) = StatusMap :-
    (
        File = dep_target(target_file(_, Target)),
        target_is_grade_or_arch_dependent(Target)
    ->
        StatusMap = delete(StatusMap0, File)
    ;
        StatusMap = StatusMap0
    ).

:- pred install_library_grade_2(bool::in, module_name::in,
    list(module_name)::in, make_info::in, bool::in, bool::out,
    io::di, io::uo) is det.

install_library_grade_2(LinkSucceeded0, ModuleName, AllModules,
        Info0, CleanAfter, Succeeded, !IO) :-
    make_misc_target(ModuleName - misc_target_build_library, LibSucceeded,
        Info0, Info1, !IO),
    (
        LibSucceeded = yes,
        % `GradeDir' differs from `Grade' in that it is in canonical form,
        % and it does not include any `.picreg' component.
        globals.io_get_globals(Globals, !IO),
        grade_directory_component(Globals, GradeDir),
        install_library_grade_files(LinkSucceeded0, GradeDir,
            ModuleName, AllModules, Succeeded, Info1, Info2, !IO),
        maybe_make_grade_clean(CleanAfter, ModuleName, AllModules,
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
        linked_target_file_name(ModuleName, erlang_archive,
            ErlangArchiveFileName, !IO),

        globals.io_lookup_string_option(install_prefix, Prefix, !IO),

        ( GradeDir = "java" ->
            GradeLibDir = Prefix/"lib"/"mercury"/"lib"/"java",
            install_file(JarFileName, GradeLibDir, LibsSucceeded, !IO),
            InitSucceeded = yes
        ; GradeDir = "erlang" ->
            GradeLibDir = Prefix/"lib"/"mercury"/"lib"/"erlang",
            % Our "Erlang archives" are actually directories.
            install_directory(ErlangArchiveFileName, GradeLibDir,
                LibsSucceeded, !IO),
            install_grade_init(GradeDir, ModuleName, InitSucceeded, !IO)
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
            ),
            install_grade_init(GradeDir, ModuleName, InitSucceeded, !IO)
        ),

        list.map_foldl2(
            install_grade_ints_and_headers(LinkSucceeded, GradeDir),
            AllModules, IntsHeadersSucceeded, !Info, !IO),
        Succeeded = bool.and_list(
            [LibsSucceeded, InitSucceeded | IntsHeadersSucceeded])
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
    module_name_to_file_name(ModuleName, ".init", do_not_create_dirs,
        InitFileName, !IO),
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

            % This is needed so that the file will be found in Mmake's VPATH.
            IntDir = LibDir/"ints",
            install_subdir_file(LinkSucceeded, IntDir, ModuleName, "mih",
                HeaderSucceeded2, !IO),

            HeaderSucceeded = HeaderSucceeded1 `and` HeaderSucceeded2
        ;
            HeaderSucceeded = yes
        ),

        GradeIntDir = LibDir/"ints"/GradeDir,
        globals.io_get_any_intermod(AnyIntermod, !IO),
        (
            AnyIntermod = yes,
            install_subdir_file(LinkSucceeded, GradeIntDir, ModuleName, "opt",
                OptSucceeded, !IO)
        ;
            AnyIntermod = no,
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
    module_name_to_file_name(ModuleName, "." ++ Ext, do_not_create_dirs,
        FileName, !IO),
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

        % We need to update the archive index after we copy a .a file to
        % the installation directory because the linkers on some OSs
        % complain if we don't.
        (
            Linkage = "static",
            Succeeded0 = yes
        ->
            % Since mmc --make uses --use-subdirs the above FileName will
            % be directory qualified.  We don't care about the build
            % directory here so we strip that qualification off.

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

:- pred install_directory(dir_name::in, dir_name::in, bool::out,
    io::di, io::uo) is det.

install_directory(SourceDirName, InstallDir, Succeeded, !IO) :-
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Installing directory ", !IO),
            io.write_string(SourceDirName, !IO),
            io.write_string(" in ", !IO),
            io.write_string(InstallDir, !IO),
            io.nl(!IO)
        ), !IO),
    globals.io_lookup_string_option(install_command, InstallCommand, !IO),
    globals.io_lookup_string_option(install_command_dir_option,
        InstallCommandDirOption, !IO),
    Command = string.join_list("   ", list.map(quote_arg,
        [InstallCommand, InstallCommandDirOption, SourceDirName, InstallDir])),
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

:- pred maybe_make_grade_clean(bool::in, module_name::in,
    list(module_name)::in, make_info::in, make_info::out, io::di, io::uo)
    is det.

maybe_make_grade_clean(Clean, ModuleName, AllModules, !Info, !IO) :-
    (
        Clean = yes,
        make_grade_clean(ModuleName, AllModules, !Info, !IO)
    ;
        Clean = no
    ).

    % Clean up grade-dependent files.
    %
:- pred make_grade_clean(module_name::in, list(module_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_grade_clean(ModuleName, AllModules, !Info, !IO) :-
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Cleaning up grade-dependent files for `", !IO),
            write_sym_name(ModuleName, !IO),
            io.write_string("'in grade ", !IO),
            globals.io_get_globals(Globals, !IO),
            grade_directory_component(Globals, Grade),
            io.write_string(Grade, !IO),
            io.write_string(".\n", !IO)
        ), !IO),

    make_main_module_realclean(ModuleName, !Info, !IO),
    list.foldl2(make_module_clean, AllModules, !Info, !IO).

:- pred make_main_module_realclean(module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_main_module_realclean(ModuleName, !Info, !IO) :-
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Removing executable and library files for `",
                !IO),
            write_sym_name(ModuleName, !IO),
            io.write_string("'.\n", !IO)
        ), !IO),
    linked_target_file_name(ModuleName, executable, ExeFileName, !IO),
    linked_target_file_name(ModuleName, static_library, LibFileName, !IO),
    linked_target_file_name(ModuleName, shared_library, SharedLibFileName,
        !IO),
    linked_target_file_name(ModuleName, java_archive, JarFileName, !IO),
    linked_target_file_name(ModuleName, erlang_archive,
        ErlangArchiveFileName, !IO),

    % Remove the symlinks created for `--use-grade-subdirs'.
    globals.io_lookup_bool_option(use_grade_subdirs, UseGradeSubdirs, !IO),
    globals.io_set_option(use_grade_subdirs, bool(no), !IO),
    linked_target_file_name(ModuleName, executable, ThisDirExeFileName, !IO),
    linked_target_file_name(ModuleName, static_library,
        ThisDirLibFileName, !IO),
    linked_target_file_name(ModuleName, shared_library,
        ThisDirSharedLibFileName, !IO),
    linked_target_file_name(ModuleName, java_archive, ThisDirJarFileName, !IO),
    linked_target_file_name(ModuleName, erlang_archive,
        ThisDirErlangArchiveFileName, !IO),
    % XXX this symlink should not be necessary anymore for `mmc --make'
    module_name_to_file_name(ModuleName, ".init", do_not_create_dirs,
        ThisDirInitFileName, !IO),
    globals.io_set_option(use_grade_subdirs, bool(UseGradeSubdirs), !IO),

    list.foldl2(make_remove_file(very_verbose),
        [ExeFileName, LibFileName, SharedLibFileName, JarFileName,
        ErlangArchiveFileName,
        ThisDirExeFileName, ThisDirLibFileName,
        ThisDirSharedLibFileName, ThisDirJarFileName,
        ThisDirErlangArchiveFileName, ThisDirInitFileName],
        !Info, !IO),
    make_remove_file(very_verbose, ModuleName, ".init", !Info, !IO),
    remove_init_files(very_verbose, ModuleName, !Info, !IO).

:- pred remove_init_files(option::in, module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_init_files(Verbose, ModuleName, !Info, !IO) :-
    globals.io_lookup_string_option(object_file_extension, ObjExt, !IO),
    globals.io_lookup_string_option(pic_object_file_extension, PicObjExt,
        !IO),
    globals.io_lookup_string_option(link_with_pic_object_file_extension,
        LinkWithPicObjExt, !IO),
    globals.io_lookup_string_option(erlang_object_file_extension, BeamExt,
        !IO),
    list.foldl2(make_remove_file(Verbose, ModuleName),
        ["_init.c", "_init" ++ ObjExt,
            "_init" ++ PicObjExt, "_init" ++ LinkWithPicObjExt,
            "_init.erl", "_init" ++ BeamExt],
        !Info, !IO).

:- pred make_module_clean(module_name::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

make_module_clean(ModuleName, !Info, !IO) :-
    verbose_msg(verbose_make,
        (pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Cleaning up target files for module `", !IO),
            write_sym_name(ModuleName, !IO),
            io.write_string("'.\n", !IO)
        ), !IO),

    list.foldl2(make_remove_target_file(very_verbose, ModuleName),
        [module_target_errors, module_target_c_code,
        module_target_c_header(header_mih), module_target_il_code,
        module_target_java_code, module_target_erlang_code,
        module_target_erlang_header,
        module_target_erlang_beam_code], !Info, !IO),

    list.foldl2(make_remove_file(very_verbose, ModuleName),
        [".used", ".prof"], !Info, !IO),

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
                ".c", do_not_create_dirs, FactTableCFile),
            make_remove_file(very_verbose, FactTableCFile, !Info)
        ), FactTableFiles, !Info, !IO),

    CCodeModule = foreign_language_module_name(ModuleName, lang_c),
    make_remove_target_file(very_verbose, CCodeModule, module_target_c_code,
        !Info, !IO),

    % Remove object and assembler files.
    list.foldl2(
        (pred(PIC::in, !.Info::in, !:Info::out, !.IO::di, !:IO::uo) is det :-
        make_remove_target_file(very_verbose, ModuleName,
            module_target_object_code(PIC),
            !Info, !IO),
        make_remove_target_file(very_verbose, ModuleName,
            module_target_asm_code(PIC), !Info, !IO),
        make_remove_target_file(very_verbose, ModuleName,
            module_target_foreign_object(PIC, lang_c), !Info, !IO),
        list.foldl2(
            (pred(FactTableFile::in, !.Info::in, !:Info::out,
                    !.IO::di, !:IO::uo) is det :-
                make_remove_target_file(very_verbose, ModuleName,
                    module_target_fact_table_object(PIC, FactTableFile),
                    !Info, !IO)
            ), FactTableFiles, !Info, !IO)
        ),
        [pic, link_with_pic, non_pic], !Info, !IO),

    % Remove IL foreign code files.
    CSharpModule = foreign_language_module_name(ModuleName, lang_csharp),
    make_remove_file(very_verbose, CSharpModule,
        foreign_language_file_extension(lang_csharp), !Info, !IO),
    make_remove_target_file(very_verbose, CSharpModule,
        module_target_foreign_il_asm(lang_csharp), !Info, !IO).

:- pred make_module_realclean(module_name::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

make_module_realclean(ModuleName, !Info, !IO) :-
    make_module_clean(ModuleName, !Info, !IO),

    verbose_msg(verbose_make,
        (pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Cleaning up interface files for module `", !IO),
            write_sym_name(ModuleName, !IO),
            io.write_string("'.\n", !IO)
        ), !IO),
    list.foldl2(make_remove_target_file(very_verbose, ModuleName),
        [
            module_target_private_interface,
            module_target_long_interface,
            module_target_short_interface,
            module_target_unqualified_short_interface,
            module_target_intermodule_interface,
            module_target_analysis_registry,
            module_target_c_header(header_mh),
            module_target_erlang_header,
            module_target_track_flags
        ],
        !Info, !IO),
    make_remove_file(very_verbose, ModuleName, make_module_dep_file_extension,
        !Info, !IO),
    make_remove_file(very_verbose, ModuleName, ".imdg", !Info, !IO),
    make_remove_file(very_verbose, ModuleName, ".request", !Info, !IO).

%-----------------------------------------------------------------------------%
%
% Check that the Mercury libraries required to build a linked target
% are installed in the selected grade.
%

    % Check that all Mercury libraries required by the linked target are
    % installed in the selected grade.
    %
:- pred check_libraries_are_installed(bool::out, io::di, io::uo) is det.

check_libraries_are_installed(Succeeded, !IO) :-
    io_get_globals(Globals, !IO),
    % NOTE: we don't look up the value of the option init_files here because
    % that may include .init files other than those associated with any
    % libraries.
    globals.lookup_accumulating_option(Globals, mercury_libraries, Libs),
    globals.lookup_accumulating_option(Globals, init_file_directories,
        InitFileDirs),
    grade_directory_component(Globals, Grade),
    check_stdlib_is_installed(Grade, Succeeded0, !IO),
    list.foldl2(check_library_is_installed(InitFileDirs, Grade),
        Libs, Succeeded0, Succeeded, !IO).

:- pred check_stdlib_is_installed(string::in, bool::out, io::di, io::uo)
    is det.

check_stdlib_is_installed(Grade, Succeeded, !IO) :-
    verbose_msg(debug_make,
        (pred(!.IO::di, !:IO::uo) is det :-
            io.format("Checking that the Mercury standard library is " ++
                "installed in grade `%s'.\n", [s(Grade)], !IO)
        ), !IO),
    globals.io_lookup_maybe_string_option(
        mercury_standard_library_directory, MaybeStdLibDir, !IO),
    (
        MaybeStdLibDir = yes(StdLibDir),
        % We check for the presence mer_std.init in the required grade.
        % Unless the installation is broken this implies the presence
        % of the the other standard .init files in that grade.
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
                " cannot be found in grade %s.\n",
                [s(ProgName), s(Grade)], !IO),
            Succeeded = no
        )
    ;
        MaybeStdLibDir = no,
        Succeeded = yes
    ).

:- pred check_library_is_installed(list(string)::in, string::in,
    string::in, bool::in, bool::out, io::di, io::uo) is det.

check_library_is_installed(Dirs, Grade, LibName, !Succeeded, !IO) :-
    verbose_msg(debug_make,
        (pred(!.IO::di, !:IO::uo) is det :-
            io.format("Checking that %s is installed in grade `%s'.\n",
                [s(LibName), s(Grade)], !IO)
        ), !IO),
    % We check for the presence of a library in a particular grade by seeing
    % whether its .init file exists.  This will work because all libraries
    % have a grade dependent .init file.
    InitFileName = LibName ++ ".init",
    search_for_file(Dirs, InitFileName, SearchResult, !IO),
    (
        SearchResult = ok(_),
        % search_for_file/5 has opened the file, so close it.
        io.seen(!IO)
    ;
        SearchResult = error(_),
        io.stderr_stream(Stderr, !IO),
        io.progname_base("mercury_compile", ProgName, !IO),
        io.format(Stderr,
            "%s: error: the library `%s' cannot be found in grade `%s'.\n",
            [s(ProgName), s(LibName), s(Grade)], !IO),
        !:Succeeded = no
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "make.program_target.m".

%-----------------------------------------------------------------------------%
:- end_module make.program_target.
%-----------------------------------------------------------------------------%
