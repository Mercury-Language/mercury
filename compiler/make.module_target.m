%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: make.module_target.m.
% Main author: stayl.
%
% Build targets which relate to a single module (e.g. C code, object code,
% interface files).
%
%-----------------------------------------------------------------------------%

:- module make.module_target.
:- interface.

%-----------------------------------------------------------------------------%

    % make_module_target(Target, Success, !Info).
    %
    % Make a target corresponding to a single module.
    %
:- pred make_module_target(dependency_file::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % make_module_target_extra_options(ExtraOpts, Target, Success, !Info)
    %
    % Make a target corresponding to a single module, with extra command line
    % options.
    %
:- pred make_module_target_extra_options(list(string)::in,
    dependency_file::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % record_made_target(Target, Task, MakeSucceeded)
    %
    % Record whether building a target succeeded or not.
    % Makes sure any timestamps for files which may have changed
    % in building the target are recomputed next time they are needed.
    % Exported for use by make.module_dep_file.write_module_dep_file.
    %
:- pred record_made_target(target_file::in, compilation_task_type::in,
    bool::in, make_info::in, make_info::out, io::di, io::uo) is det.

:- type foreign_code_file
    --->    foreign_code_file(
                foreign_language    :: foreign_language,

                target_file         :: file_name,
                                    % Name of the file produced by the Mercury
                                    % compiler, e.g. module_c_code.c.

                object_file         :: file_name
                                    % Name of the file produced by the foreign
                                    % language compiler, e.g. module_c_code.o.
            ).

    % Find the foreign code files generated when a module is processed.
    % The `pic' field is only used for C foreign code.
    %
:- pred external_foreign_code_files(pic::in, module_and_imports::in,
    list(foreign_code_file)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module libs.compiler_util.
:- import_module libs.process_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_foreign.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module dir.
:- import_module float.
:- import_module svmap.

%-----------------------------------------------------------------------------%

:- pred make_module_target(dependency_file::in, bool::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_module_target(TargetFile, Succeeded1, Succeeded1 `and` Succeeded2,
        !Info, !IO) :-
    make_module_target(TargetFile, Succeeded2, !Info, !IO).

make_module_target(DepFile, Succeeded, !Info, !IO) :-
    make_module_target_extra_options([], DepFile, Succeeded, !Info, !IO).

make_module_target_extra_options(_ExtraOptions, dep_file(_, _) @ Dep,
        Succeeded, !Info, !IO) :-
    dependency_status(Dep, Status, !Info, !IO),
    (
        Status = deps_status_error,
        Succeeded = no
    ;
        ( Status = deps_status_not_considered
        ; Status = deps_status_being_built
        ; Status = deps_status_up_to_date
        ),
        Succeeded = yes
    ).
make_module_target_extra_options(ExtraOptions, dep_target(TargetFile) @ Dep,
        Succeeded, !Info, !IO) :-
    dependency_status(Dep, Status, !Info, !IO),
    (
        Status = deps_status_not_considered,
        TargetFile = target_file(ModuleName, FileType),
        get_module_dependencies(ModuleName, MaybeImports, !Info, !IO),
        (
            MaybeImports = no,
            Succeeded = no,
            !Info ^ dependency_status ^ elem(Dep) := deps_status_error
        ;
            MaybeImports = yes(Imports),
            globals.io_get_globals(Globals, !IO),
            CompilationTask = compilation_task(Globals, FileType),
            (
                % For a target built by processing a Mercury source file,
                % the target for a nested sub-module is produced as a side
                % effect of making the target for the top-level module in
                % the file.
                CompilationTask = process_module(_) - _,
                Imports ^ mai_source_file_module_name \= ModuleName
            ->
                NestedTargetFile = target_file(
                    Imports ^ mai_source_file_module_name, FileType),
                make_module_target_extra_options(ExtraOptions,
                    dep_target(NestedTargetFile),
                    Succeeded, !Info, !IO)
            ;
                CompilationTask = CompilationTaskType - _,
                touched_files(TargetFile, CompilationTaskType,
                    TouchedTargetFiles, TouchedFiles, !Info, !IO),
                list.foldl(update_target_status(deps_status_being_built),
                    TouchedTargetFiles, !Info),

                debug_file_msg(TargetFile, "checking dependencies", !IO),

                ( CompilationTask = process_module(_) - _ ->
                    ModulesToCheck =
                        [ModuleName | Imports ^ mai_nested_children]
                ;
                    ModulesToCheck = [ModuleName]
                ),
                module_names_to_index_set(ModulesToCheck, ModulesToCheckSet,
                    !Info),

                deps_set_foldl3_maybe_stop_at_error(!.Info ^ keep_going,
                    union_deps(target_dependencies(Globals, FileType)),
                    ModulesToCheckSet, DepsSuccess, init, DepFiles0,
                    !Info, !IO),
                dependency_file_index_set_to_plain_set(!.Info, DepFiles0,
                    DepFilesSet0),
                (
                    TargetFile = target_file(_, TargetType),
                    TargetType = module_target_private_interface
                ->
                    % Avoid circular dependencies (the `.int0' files
                    % for the nested sub-modules depend on this module's
                    % `.int0' file).
                    PrivateInts = make_dependency_list(ModulesToCheck,
                        module_target_private_interface),
                    DepFilesToMake = set.to_sorted_list(
                        set.delete_list(DepFilesSet0, PrivateInts))
                ;
                    DepFilesToMake = set.to_sorted_list(DepFilesSet0)
                ),

                debug_msg(
                   (pred(!.IO::di, !:IO::uo) is det :-
                        make_write_target_file(TargetFile, !IO),
                        io.write_string(": dependencies:\n", !IO),
                        dependency_file_index_set_to_plain_set(!.Info,
                            DepFiles0, PlainSet),
                        make_write_dependency_file_list(
                            to_sorted_list(PlainSet), !IO)
                ), !IO),

                globals.io_lookup_bool_option(keep_going, KeepGoing, !IO),
                (
                    DepsSuccess = no,
                    KeepGoing = no
                ->
                    DepsResult = deps_error
                ;
                    make_dependency_files(TargetFile, DepFilesToMake,
                        TouchedTargetFiles, TouchedFiles, DepsResult0,
                        !Info, !IO),
                    (
                        DepsSuccess = yes,
                        DepsResult = DepsResult0
                    ;
                        DepsSuccess = no,
                        DepsResult = deps_error
                    )
                ),
                (
                    DepsResult = deps_error,
                    Succeeded = no,
                    list.foldl(update_target_status(deps_status_error),
                        TouchedTargetFiles, !Info)
                ;
                    DepsResult = deps_out_of_date,
                    !:Info = !.Info ^ command_line_targets :=
                        set.delete(!.Info ^ command_line_targets,
                            ModuleName - module_target(FileType)),
                    build_target(CompilationTask, TargetFile, Imports,
                        TouchedTargetFiles, TouchedFiles, ExtraOptions,
                        Succeeded, !Info, !IO)
                ;
                    DepsResult = deps_up_to_date,
                    maybe_warn_up_to_date_target(
                        ModuleName - module_target(FileType), !Info, !IO),
                    debug_file_msg(TargetFile, "up to date", !IO),
                    Succeeded = yes,
                    list.foldl(update_target_status(deps_status_up_to_date),
                        [TargetFile | TouchedTargetFiles], !Info)
                )
            )
        )
    ;
        Status = deps_status_up_to_date,
        Succeeded = yes
    ;
        Status = deps_status_being_built,
        (
            TargetFile = target_file(_FileName, FileType),
            FileType = module_target_foreign_il_asm(_Lang)
        ->
            io.write_string("Error: circular dependency detected " ++
                "while building\n", !IO),
            io.write_string("  `", !IO),
            make_write_dependency_file(Dep, !IO),
            io.write_string("'.\n", !IO),
            io.write_string("  This is due to a forbidden " ++
                "foreign_import_module cycle.\n", !IO),
            io.set_exit_status(1, !IO)
        ;
            unexpected(this_file, "make_module_target: " ++
                "target being built, circular dependencies?")
        ),
        Succeeded = no
    ;
        Status = deps_status_error,
        Succeeded = no
    ).

:- pred make_dependency_files(target_file::in, list(dependency_file)::in,
    list(target_file)::in, list(file_name)::in, dependencies_result::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_dependency_files(TargetFile, DepFilesToMake, TouchedTargetFiles,
        TouchedFiles, DepsResult, !Info, !IO) :-
    % Build the dependencies.

    globals.io_lookup_bool_option(keep_going, KeepGoing, !IO),
    foldl2_maybe_stop_at_error(KeepGoing, make_module_target,
        DepFilesToMake, MakeDepsSuccess, !Info, !IO),

    % Check that the target files exist.

    list.map_foldl2(get_target_timestamp(do_not_search), TouchedTargetFiles,
        TargetTimestamps, !Info, !IO),
    (
        MakeDepsSuccess = no,
        debug_file_msg(TargetFile, "error making dependencies", !IO),
        DepsResult = deps_error
    ;
        MakeDepsSuccess = yes,
        ( list.member(error(_), TargetTimestamps) ->
            debug_file_msg(TargetFile, "target file does not exist", !IO),
            DepsResult = deps_out_of_date
        ;
            (
                TargetFile = target_file(ModuleName, FileType),
                FileType = module_target_analysis_registry
            ->
                force_reanalysis_of_suboptimal_module(ModuleName,
                    ForceReanalysis, !.Info, !IO)
            ;
                ForceReanalysis = no
            ),
            (
                ForceReanalysis = yes,
                DepsResult = deps_out_of_date
            ;
                ForceReanalysis = no,

                % Compare the oldest of the timestamps of the touched
                % files with the timestamps of the dependencies.

                list.map_foldl2(get_timestamp_file_timestamp,
                    TouchedTargetFiles, TouchedTargetFileTimestamps,
                    !Info, !IO),
                list.map_foldl2(get_file_timestamp([dir.this_directory]),
                    TouchedFiles, TouchedFileTimestamps, !Info, !IO),
                MaybeOldestTimestamp0 = list.foldl(find_oldest_timestamp,
                    TouchedTargetFileTimestamps, ok(newest_timestamp)),
                MaybeOldestTimestamp = list.foldl(find_oldest_timestamp,
                    TouchedFileTimestamps, MaybeOldestTimestamp0),

                get_file_name(do_not_search, TargetFile, TargetFileName,
                    !Info, !IO),
                check_dependencies(TargetFileName, MaybeOldestTimestamp,
                    MakeDepsSuccess, DepFilesToMake, DepsResult, !Info, !IO)
            )
        )
    ).

:- pred force_reanalysis_of_suboptimal_module(module_name::in, bool::out,
    make_info::in, io::di, io::uo) is det.

force_reanalysis_of_suboptimal_module(ModuleName, ForceReanalysis, Info,
        !IO) :-
    ( Info ^ reanalysis_passes > 0 ->
        analysis.read_module_overall_status(mmc, ModuleName, AnalysisStatus,
            !IO),
        (
            ( AnalysisStatus = suboptimal
            ; AnalysisStatus = invalid
            ),
            ForceReanalysis = yes
        ;
            AnalysisStatus = optimal,
            ForceReanalysis = no
        )
    ;
        ForceReanalysis = no
    ).

%-----------------------------------------------------------------------------%

:- pred build_target(compilation_task_result::in, target_file::in,
    module_and_imports::in, list(target_file)::in, list(file_name)::in,
    list(string)::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

build_target(CompilationTask, TargetFile, Imports, TouchedTargetFiles,
        TouchedFiles, ExtraOptions, Succeeded, !Info, !IO) :-
    maybe_make_target_message(TargetFile, !IO),
    TargetFile = target_file(ModuleName, _FileType),
    CompilationTask = Task - TaskOptions,
    (
        CompilationTask = process_module(ModuleTask) - _,
        forkable_module_compilation_task_type(ModuleTask) = yes,
        \+ can_fork
    ->
        % We need a temporary file to pass the arguments to the mmc process
        % which will do the compilation.  It's created here
        % (not in invoke_mmc) so it can be cleaned up by
        % build_with_check_for_interrupt.
        io.make_temp(ArgFileName, !IO),
        MaybeArgFileName = yes(ArgFileName)
    ;
        MaybeArgFileName = no
    ),
    Cleanup =
        (pred(!.MakeInfo::in, !:MakeInfo::out, !.IO::di, !:IO::uo) is det :-
            % XXX Remove `.int.tmp' files.
            list.foldl2(make_remove_target_file(very_verbose),
                TouchedTargetFiles, !MakeInfo, !IO),
            list.foldl2(make_remove_file(very_verbose), TouchedFiles,
                !MakeInfo, !IO),
            (
                MaybeArgFileName = yes(ArgFileName2),
                io.remove_file(ArgFileName2, _, !IO)
            ;
                MaybeArgFileName = no
            )
        ),

    get_real_milliseconds(Time0, !IO),
    build_with_check_for_interrupt(
        build_with_module_options_and_output_redirect(ModuleName,
            ExtraOptions ++ TaskOptions,
            build_target_2(ModuleName, Task, MaybeArgFileName, Imports)),
        Cleanup, Succeeded, !Info, !IO),
    record_made_target_2(Succeeded, TargetFile, TouchedTargetFiles,
        TouchedFiles, !Info, !IO),
    get_real_milliseconds(Time, !IO),

    globals.io_lookup_bool_option(show_make_times, ShowMakeTimes, !IO),
    (
        ShowMakeTimes = yes,
        DiffSecs = float(Time - Time0) / 1000.0,
        % Avoid cluttering the screen with short running times.
        ( DiffSecs >= 0.4 ->
            io.write_string("Making ", !IO),
            make_write_target_file(TargetFile, !IO),
            io.format(" took %.2fs\n", [f(DiffSecs)], !IO)
        ;
            true
        )
    ;
        ShowMakeTimes = no
    ).

:- pred build_target_2(module_name::in, compilation_task_type::in,
    maybe(file_name)::in, module_and_imports::in, list(string)::in,
    io.output_stream::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

build_target_2(ModuleName, process_module(ModuleTask), ArgFileName,
        _Imports, AllOptionArgs, ErrorStream, Succeeded, !Info, !IO) :-
    ModuleArg = sym_name_to_string(ModuleName),

    globals.io_lookup_bool_option(verbose_commands, Verbose, !IO),
    (
        Verbose = yes,
        AllArgs = list.append(AllOptionArgs, [ModuleArg]),
        io.write_string("Invoking command `mmc ", !IO),
        % XXX Don't write the default options.
        io.write_list(list.map(quote_arg, AllArgs), " ", io.write_string, !IO),
        io.write_string("'", !IO),
        io.nl(!IO)
    ;
        Verbose = no
    ),

    % Run compilations to target code in a separate process.  This is
    % necessary for `--target asm' because the GCC backend can only be
    % invoked once per process. It's a good idea for other the backends
    % because it avoids problems with the Boehm GC retaining memory by
    % scanning too much of the Mercury stacks. If the compilation is run in
    % a separate process, it is also easier to kill if an interrupt arrives.
    % We do the same for intermodule-optimization interfaces because if type
    % checking gets overloaded by ambiguities it can be difficult to kill
    % the compiler otherwise.
    io.set_output_stream(ErrorStream, OldOutputStream, !IO),
    (
        forkable_module_compilation_task_type(ModuleTask) = yes
    ->
        call_in_forked_process_with_backup(
            call_mercury_compile_main([ModuleArg]),
            invoke_mmc(ErrorStream, ArgFileName, AllOptionArgs ++ [ModuleArg]),
            Succeeded, !IO)
    ;
        call_mercury_compile_main([ModuleArg], Succeeded, !IO)
    ),
    io.set_output_stream(OldOutputStream, _, !IO),

    (
        ( ModuleTask = task_compile_to_target_code
        ; ModuleTask = task_errorcheck
        )
    ->
        % The `.err_date' file is needed because the `.err' file is touched
        % by all phases of compilation, including writing interfaces.
        touch_interface_datestamp(ModuleName, ".err_date", !IO)
    ;
        true
    ).

build_target_2(ModuleName, target_code_to_object_code(PIC), _,
        Imports, _, ErrorStream, Succeeded, !Info, !IO) :-
    globals.io_get_target(CompilationTarget, !IO),

    % Run the compilation in a child process so it can be killed
    % if an interrupt arrives.
    call_in_forked_process(
        build_object_code(ModuleName, CompilationTarget, PIC, ErrorStream,
            Imports),
        Succeeded, !IO).

build_target_2(ModuleName, foreign_code_to_object_code(PIC, Lang), _,
        Imports, _, ErrorStream, Succeeded, !Info, !IO) :-
    foreign_code_file(ModuleName, PIC, Lang, ForeignCodeFile, !IO),

    % Run the compilation in a child process so it can be killed
    % if an interrupt arrives.
    call_in_forked_process(
        compile_foreign_code_file(ErrorStream, PIC, Imports, ForeignCodeFile),
        Succeeded, !IO).

build_target_2(ModuleName, fact_table_code_to_object_code(PIC, FactTableFile),
        _, Imports, _, ErrorStream, Succeeded, !Info, !IO) :-
    fact_table_foreign_code_file(ModuleName, PIC, FactTableFile,
        FactTableForeignCode, !IO),

    % Run the compilation in a child process so it can be killed
    % if an interrupt arrives.
    call_in_forked_process(
        compile_foreign_code_file(ErrorStream, PIC, Imports,
            FactTableForeignCode),
        Succeeded, !IO).

:- pred build_object_code(module_name::in, compilation_target::in, pic::in,
    io.output_stream::in, module_and_imports::in, bool::out,
    io::di, io::uo) is det.

build_object_code(ModuleName, target_c, PIC, ErrorStream, _Imports,
        Succeeded, !IO) :-
    compile_target_code.compile_c_file(ErrorStream, PIC, ModuleName,
        Succeeded, !IO).
build_object_code(ModuleName, target_asm, PIC, ErrorStream, _Imports,
        Succeeded, !IO) :-
    compile_target_code.assemble(ErrorStream, PIC, ModuleName,
        Succeeded, !IO).
build_object_code(ModuleName, target_java, _, ErrorStream, _Imports, Succeeded,
        !IO) :-
    module_name_to_file_name(ModuleName, ".java", do_create_dirs, JavaFile,
        !IO),
    compile_target_code.compile_java_files(ErrorStream, [JavaFile],
        Succeeded, !IO).
build_object_code(ModuleName, target_il, _, ErrorStream, Imports, Succeeded,
        !IO) :-
    compile_target_code.il_assemble(ErrorStream, ModuleName,
        Imports ^ mai_has_main, Succeeded, !IO).
build_object_code(_ModuleName, target_x86_64, _, _ErrorStream, _Imports,
        _Succeeded, _, _) :-
    sorry(this_file, "NYI mmc --make and target x86_64").
build_object_code(ModuleName, target_erlang, _, ErrorStream, _Imports,
        Succeeded, !IO) :-
    module_name_to_file_name(ModuleName, ".erl", do_create_dirs, ErlangFile,
        !IO),
    compile_target_code.compile_erlang_file(ErrorStream, ErlangFile,
        Succeeded, !IO).

:- pred compile_foreign_code_file(io.output_stream::in, pic::in,
    module_and_imports::in, foreign_code_file::in, bool::out,
    io::di, io::uo) is det.

compile_foreign_code_file(ErrorStream, PIC, _Imports,
        foreign_code_file(lang_c, CFile, ObjFile), Succeeded, !IO) :-
    compile_target_code.compile_c_file(ErrorStream, PIC,
        CFile, ObjFile, Succeeded, !IO).
compile_foreign_code_file(ErrorStream, _, _Imports,
        foreign_code_file(lang_il, ILFile, DLLFile), Succeeded, !IO) :-
    compile_target_code.il_assemble(ErrorStream, ILFile, DLLFile,
        no_main, Succeeded, !IO).
compile_foreign_code_file(ErrorStream, _, _Imports,
        foreign_code_file(lang_java, JavaFile, _ClassFile), Succeeded, !IO) :-
    compile_target_code.compile_java_files(ErrorStream, [JavaFile],
        Succeeded, !IO).
compile_foreign_code_file(ErrorStream, _, Imports,
        foreign_code_file(lang_csharp, CSharpFile, DLLFile),
        Succeeded, !IO) :-
    compile_target_code.compile_csharp_file(ErrorStream, Imports,
        CSharpFile, DLLFile, Succeeded, !IO).
compile_foreign_code_file(ErrorStream, _, _Imports,
        foreign_code_file(lang_erlang, ErlFile, _BeamFile), Succeeded, !IO) :-
    compile_target_code.compile_erlang_file(ErrorStream, ErlFile,
        Succeeded, !IO).

:- func forkable_module_compilation_task_type(module_compilation_task_type)
    = bool.

forkable_module_compilation_task_type(task_errorcheck) = no.
forkable_module_compilation_task_type(task_make_short_interface) = no.
forkable_module_compilation_task_type(task_make_interface) = no.
forkable_module_compilation_task_type(task_make_private_interface) = no.
forkable_module_compilation_task_type(task_make_optimization_interface) = yes.
forkable_module_compilation_task_type(task_make_analysis_registry) = yes.
forkable_module_compilation_task_type(task_compile_to_target_code) = yes.
forkable_module_compilation_task_type(task_make_xml_doc) = yes.

%-----------------------------------------------------------------------------%

:- pred foreign_code_file(module_name::in, pic::in, foreign_language::in,
    foreign_code_file::out, io::di, io::uo) is det.

foreign_code_file(ModuleName, PIC, Lang, ForeignCodeFile, !IO) :-
    globals.io_get_globals(Globals, !IO),
    (
        ForeignModName0 = foreign_language_module_name(ModuleName, Lang),
        SrcExt0 = foreign_language_file_extension(Lang)
    ->
        ForeignModName = ForeignModName0,
        SrcExt = SrcExt0
    ;
        unexpected(this_file, "unsupported foreign language")
    ),
    ObjExt = get_object_extension(Globals, PIC),
    module_name_to_file_name(ForeignModName, SrcExt, do_create_dirs,
        SrcFileName, !IO),
    module_name_to_file_name(ForeignModName, ObjExt, do_create_dirs,
        ObjFileName, !IO),
    ForeignCodeFile = foreign_code_file(Lang, SrcFileName, ObjFileName).

:- pred fact_table_foreign_code_file(module_name::in, pic::in, string::in,
    foreign_code_file::out, io::di, io::uo) is det.

fact_table_foreign_code_file(ModuleName, PIC, FactTableName, ForeignCodeFile,
        !IO) :-
    globals.io_get_globals(Globals, !IO),
    ObjExt = get_object_extension(Globals, PIC),
    fact_table_file_name(ModuleName, FactTableName, ".c", do_create_dirs,
        CFile, !IO),
    fact_table_file_name(ModuleName, FactTableName, ObjExt, do_create_dirs,
        ObjFile, !IO),
    ForeignCodeFile = foreign_code_file(lang_c, CFile, ObjFile).

:- func get_object_extension(globals, pic) = string.

get_object_extension(Globals, PIC) = Ext :-
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        maybe_pic_object_file_extension(Globals, PIC, Ext)
    ;
        CompilationTarget = target_asm,
        maybe_pic_object_file_extension(Globals, PIC, Ext)
    ;
        CompilationTarget = target_il,
        Ext = ".dll"
    ;
        CompilationTarget = target_java,
        sorry(this_file, "object extension for java")
    ;
        CompilationTarget = target_x86_64,
        sorry(this_file, "mmc --make NYI and target x86_64")
    ;
        CompilationTarget = target_erlang,
        sorry(this_file, "mmc --make NYI and target erlang")
    ).

%-----------------------------------------------------------------------------%

:- pred call_mercury_compile_main(list(string)::in, bool::out,
    io::di, io::uo) is det.

call_mercury_compile_main(Args, Succeeded, !IO) :-
    io.get_exit_status(Status0, !IO),
    io.set_exit_status(0, !IO),
    mercury_compile.main(Args, !IO),
    io.get_exit_status(Status, !IO),
    Succeeded = ( Status = 0 -> yes ; no ),
    io.set_exit_status(Status0, !IO).

:- pred invoke_mmc(io.output_stream::in, maybe(file_name)::in,
    list(string)::in, bool::out, io::di, io::uo) is det.

invoke_mmc(ErrorStream, MaybeArgFileName, Args, Succeeded, !IO) :-
    io.progname("", ProgName, !IO),
    ( ProgName = "" ->
        io.get_environment_var("MERCURY_COMPILER", MaybeMercuryCompiler, !IO),
        (
            MaybeMercuryCompiler = yes(MercuryCompiler)
        ;
            MaybeMercuryCompiler = no,
            MercuryCompiler = "mmc"
        )
    ;
        MercuryCompiler = ProgName
    ),

    QuotedArgs = list.map(quote_arg, Args),

    % Some operating systems (e.g. Windows) have shells with ludicrously
    % short limits on the length of command lines, so we need to write the
    % arguments to a file which will be read by the child mmc process.
    % This code is only called if fork() doesn't work, so there's no point
    % checking whether the shell actually has this limitation.
    % The temporary file is created by the caller so that it will be removed
    % by build_with_check_for_interrupt if an interrupt occurs.
    (
        MaybeArgFileName = yes(ArgFileName)
    ;
        MaybeArgFileName = no,
        unexpected(this_file,
            "make.module_target.invoke_mmc: argument file not created")
    ),

    io.open_output(ArgFileName, ArgFileOpenRes, !IO),
    (
        ArgFileOpenRes = ok(ArgFileStream),
        io.write_string(ArgFileStream, "MCFLAGS = ", !IO),
        io.write_list(ArgFileStream, QuotedArgs, " ", io.write_string, !IO),
        io.nl(ArgFileStream, !IO),
        io.close_output(ArgFileStream, !IO),

        Command = string.join_list(" ",
            [quote_arg(MercuryCompiler),
                "--arg-file", quote_arg(ArgFileName)]),

        % We've already written the command.
        CommandVerbosity = cmd_verbose,
        invoke_system_command(ErrorStream, CommandVerbosity, Command,
            Succeeded, !IO)
    ;
        ArgFileOpenRes = error(Error),
        Succeeded = no,
        io.write_string("Error opening `", !IO),
        io.write_string(ArgFileName, !IO),
        io.write_string("' for output: ", !IO),
        io.write_string(io.error_message(Error), !IO),
        io.nl(!IO)
    ),
    io.remove_file(ArgFileName, _, !IO).

%-----------------------------------------------------------------------------%

record_made_target(TargetFile, CompilationTask, Succeeded, !Info, !IO) :-
    touched_files(TargetFile, CompilationTask, TouchedTargetFiles,
        TouchedFiles, !Info, !IO),
    record_made_target_2(Succeeded, TargetFile, TouchedTargetFiles,
        TouchedFiles, !Info, !IO).

:- pred record_made_target_2(bool::in, target_file::in, list(target_file)::in,
    list(file_name)::in, make_info::in, make_info::out, io::di, io::uo) is det.

record_made_target_2(Succeeded, TargetFile, TouchedTargetFiles,
        OtherTouchedFiles, !Info, !IO) :-
    (
        Succeeded = yes,
        TargetStatus = deps_status_up_to_date
    ;
        Succeeded = no,
        TargetStatus = deps_status_error,
        target_file_error(TargetFile, !IO)
    ),

    list.foldl(update_target_status(TargetStatus), TouchedTargetFiles, !Info),

    list.map_foldl2(get_file_name(do_not_search), TouchedTargetFiles,
        TouchedTargetFileNames, !Info, !IO),

    some [!Timestamps] (
        !:Timestamps = !.Info ^ file_timestamps,
        list.foldl(delete_timestamp, TouchedTargetFileNames, !Timestamps),
        list.foldl(delete_timestamp, OtherTouchedFiles, !Timestamps),

        % When an .analysis file is made, that potentially invalidates other
        % .analysis files so we have to delete their timestamps.  The exact
        % list of files which might be affected can be found by reading the
        % corresponding .imdg file.  But it's simpler to just delete the
        % timestamps of all the .analysis files that we know about.
        ( TargetFile = target_file(_, module_target_analysis_registry) ->
            map.foldl(delete_analysis_registry_timestamps, !.Timestamps,
                !Timestamps)
        ;
            true
        ),

        !Info ^ file_timestamps := !.Timestamps
    ).

:- pred update_target_status(dependency_status::in, target_file::in,
    make_info::in, make_info::out) is det.

update_target_status(TargetStatus, TargetFile, !Info) :-
    Dep = dep_target(TargetFile),
    !Info ^ dependency_status ^ elem(Dep) := TargetStatus.

:- pred delete_analysis_registry_timestamps(string::in,
    maybe_error(timestamp)::in,
    file_timestamps::in, file_timestamps::out) is det.

delete_analysis_registry_timestamps(FileName, _, !Timestamps) :-
    ( string.suffix(FileName, ".analysis") ->
        delete_timestamp(FileName, !Timestamps)
    ;
        true
    ).

:- pred delete_timestamp(string::in, file_timestamps::in, file_timestamps::out)
    is det.

delete_timestamp(TouchedFile, !Timestamps) :-
    trace [io(!IO)] (
        debug_msg((pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Deleting timestamp for ", !IO),
            io.write_string(TouchedFile, !IO),
            io.nl(!IO)
        ), !IO)
    ),
    svmap.delete(TouchedFile, !Timestamps).

%-----------------------------------------------------------------------------%

:- type compilation_task_result == pair(compilation_task_type, list(string)).

:- func compilation_task(globals, module_target_type) =
    compilation_task_result.

compilation_task(_, module_target_source) = _ :-
    unexpected(this_file, "compilation_task").
compilation_task(_, module_target_track_flags) = _ :-
    unexpected(this_file, "compilation_task").
compilation_task(_, module_target_errors) =
    process_module(task_errorcheck) - ["--errorcheck-only"].
compilation_task(_, module_target_unqualified_short_interface) =
    process_module(task_make_short_interface) - ["--make-short-interface"].
compilation_task(Globals, module_target_short_interface) =
    compilation_task(Globals, module_target_long_interface).
compilation_task(_, module_target_long_interface) =
    process_module(task_make_interface) - ["--make-interface"].
compilation_task(_, module_target_private_interface) =
    process_module(task_make_private_interface) - ["--make-private-interface"].
compilation_task(_, module_target_intermodule_interface) =
    process_module(task_make_optimization_interface) -
        ["--make-optimization-interface"].
compilation_task(_, module_target_analysis_registry) =
    process_module(task_make_analysis_registry) - ["--make-analysis-registry"].
compilation_task(Globals, module_target_c_header(_)) =
        compilation_task(Globals, module_target_c_code).
compilation_task(_, module_target_c_code) =
    process_module(task_compile_to_target_code) - ["--compile-to-c"].
compilation_task(_, module_target_il_code) =
    process_module(task_compile_to_target_code) - ["--il-only"].
compilation_task(_, module_target_il_asm) =
        target_code_to_object_code(non_pic) - [].
compilation_task(_, module_target_java_code) =
    process_module(task_compile_to_target_code) - ["--java-only"].
compilation_task(_, module_target_java_class_code) =
        target_code_to_object_code(non_pic) - [].
compilation_task(Globals, module_target_erlang_header) =
        compilation_task(Globals, module_target_erlang_code).
compilation_task(_, module_target_erlang_code) =
    process_module(task_compile_to_target_code) - ["--erlang-only"].
compilation_task(_, module_target_erlang_beam_code) =
        target_code_to_object_code(non_pic) - [].
compilation_task(_, module_target_asm_code(PIC)) =
    process_module(task_compile_to_target_code) -
        ( PIC = pic -> ["--pic"] ; [] ).
compilation_task(_, module_target_object_code(PIC)) =
    target_code_to_object_code(PIC) - get_pic_flags(PIC).
compilation_task(_, module_target_foreign_il_asm(Lang)) =
    foreign_code_to_object_code(non_pic, Lang) - [].
compilation_task(_, module_target_foreign_object(PIC, Lang)) =
    foreign_code_to_object_code(PIC, Lang) - get_pic_flags(PIC).
compilation_task(_, module_target_fact_table_object(PIC, FactTable)) =
    fact_table_code_to_object_code(PIC, FactTable) - get_pic_flags(PIC).
compilation_task(_, module_target_xml_doc) =
    process_module(task_make_xml_doc) - ["--make-xml-doc"].

:- func get_pic_flags(pic) = list(string).

% `--pic-reg' is harmless for architectures and grades where it is not needed
% (it's only needed for grades using GCC global register variables on x86).
get_pic_flags(pic) = ["--pic", "--pic-reg"].
get_pic_flags(link_with_pic) = ["--pic-reg"].
get_pic_flags(non_pic) = [].

    % Find the files which could be touched by a compilation task.
    %
:- pred touched_files(target_file::in, compilation_task_type::in,
    list(target_file)::out, list(file_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

touched_files(TargetFile, process_module(Task), TouchedTargetFiles,
        TouchedFileNames, !Info, !IO) :-
    TargetFile = target_file(ModuleName, FileType),
    get_module_dependencies(ModuleName, MaybeImports, !Info, !IO),
    (
        MaybeImports = yes(Imports0),
        Imports = Imports0
    ;
        MaybeImports = no,
        % This error should have been caught earlier. We shouldn't be
        % attempting to build a target if we couldn't find the dependencies
        % for the module.
        unexpected(this_file, "touched_files: no module dependencies")
    ),

    NestedChildren = Imports ^ mai_nested_children,
    SourceFileModuleNames = [ModuleName | NestedChildren],

    list.map_foldl2(get_module_dependencies, NestedChildren,
        MaybeNestedImportsList, !Info, !IO),
    (
        list.map(
            (pred(yes(NestedModuleImports)::in, NestedModuleImports::out)
                is semidet),
            MaybeNestedImportsList, NestedImportsList)
    ->
        ModuleImportsList = [Imports | NestedImportsList]
    ;
        % This error should have been caught earlier. We shouldn't be
        % attempting to build a target if we couldn't find the dependencies
        % for the module or its nested sub-modules.
        unexpected(this_file, "touched_files: no nested module dependencies")
    ),

    globals.io_get_target(CompilationTarget, !IO),
    (
        Task = task_compile_to_target_code,
        CompilationTarget = target_asm
    ->
        % For `--target asm' the code for the nested children is placed
        % in the `.s' file for the top-level module in the source file.
        TargetModuleNames = [ModuleName]
    ;
        TargetModuleNames = SourceFileModuleNames
    ),

    %
    % Find out what header files are generated.
    %
    (
        Task = task_compile_to_target_code,
        list.map_foldl(
            external_foreign_code_files(target_type_to_pic(FileType)),
                ModuleImportsList, ForeignCodeFileList, !IO),
        ForeignCodeFiles =
            list.map((func(ForeignFile) = ForeignFile ^ target_file),
                list.condense(ForeignCodeFileList)),
        (
            CompilationTarget = target_c,
            globals.io_lookup_bool_option(highlevel_code, HighLevelCode, !IO),
            (
                HighLevelCode = yes,
                %
                % When compiling to high-level C, we always generate
                % a header file.
                %
                HeaderModuleNames = SourceFileModuleNames,
                HeaderTargets0 = make_target_file_list(HeaderModuleNames,
                    module_target_c_header(header_mih))
            ;
                HighLevelCode = no,
                HeaderTargets0 = []
            )
        ;
            CompilationTarget = target_asm,
            %
            % When compiling to assembler, we only generate
            % a header file if the module contains foreign code.
            %
            HeaderModuleNames =
                list.filter_map(
                    (func(MImports) = MImports ^ mai_module_name is semidet :-
                        contains_foreign_code(_) =
                            MImports ^ mai_has_foreign_code
                    ), ModuleImportsList),
            HeaderTargets0 = make_target_file_list(HeaderModuleNames,
                module_target_c_header(header_mih))
        ;
            ( CompilationTarget = target_il
            ; CompilationTarget = target_java
            ),
            HeaderTargets0 = []
        ;
            CompilationTarget = target_erlang,
            % When compiling to Erlang we always generate a header file.
            HeaderModuleNames = SourceFileModuleNames,
            HeaderTargets0 = make_target_file_list(HeaderModuleNames,
                module_target_erlang_header)
        ;
            CompilationTarget = target_x86_64,
            sorry(this_file, "NYI mmc --make and target x86_64")
        ),

        (
            ( CompilationTarget = target_c
            ; CompilationTarget = target_asm
            ),
            Names = SourceFileModuleNames,
            HeaderTargets =
                make_target_file_list(Names, module_target_c_header(header_mh))
                ++ HeaderTargets0
        ;
            ( CompilationTarget = target_il
            ; CompilationTarget = target_java
            ; CompilationTarget = target_erlang
            ),
            HeaderTargets = HeaderTargets0
        ),

        TouchedTargetFiles0 = make_target_file_list(TargetModuleNames,
            FileType),
        TouchedTargetFiles = TouchedTargetFiles0 ++ HeaderTargets
    ;
        Task = task_make_interface,
        % Both long and short interface files are produced
        % when making the interface.
        ForeignCodeFiles = [],
        TouchedTargetFiles =
            make_target_file_list(TargetModuleNames,
                module_target_long_interface)
            ++
            make_target_file_list(TargetModuleNames,
                module_target_short_interface)
    ;
        ( Task = task_errorcheck
        ; Task = task_make_short_interface
        ; Task = task_make_private_interface
        ; Task = task_make_optimization_interface
        ; Task = task_make_analysis_registry
        ; Task = task_make_xml_doc
        ),
        ForeignCodeFiles = [],
        TouchedTargetFiles = make_target_file_list(TargetModuleNames, FileType)
    ),
    globals.io_get_globals(Globals, !IO),
    list.foldl2(
        (pred(TouchedTargetFile::in,
            !.TimestampFiles::in, !:TimestampFiles::out, !.IO::di, !:IO::uo)
            is det :-
        TouchedTargetFile = target_file(TargetModuleName, TargetFileType),
        ( TimestampExt = timestamp_extension(Globals, TargetFileType) ->
            module_name_to_file_name(TargetModuleName, TimestampExt,
                do_not_create_dirs, TimestampFile, !IO),
            list.cons(TimestampFile, !TimestampFiles)
        ;
            true
        )
    ), TouchedTargetFiles, [], TimestampFileNames, !IO),
    TouchedFileNames = ForeignCodeFiles ++ TimestampFileNames.

touched_files(TargetFile, target_code_to_object_code(_), [TargetFile], [],
        !Info, !IO).

touched_files(TargetFile, foreign_code_to_object_code(PIC, Lang),
        [TargetFile], [ForeignObjectFile], !Info, !IO) :-
    TargetFile = target_file(ModuleName, _),
    foreign_code_file(ModuleName, PIC, Lang, ForeignCodeFile, !IO),
    ForeignObjectFile = ForeignCodeFile ^ object_file.

touched_files(TargetFile, fact_table_code_to_object_code(PIC, FactTableName),
        [TargetFile], [FactTableObjectFile], !Info, !IO) :-
    TargetFile = target_file(ModuleName, _),
    globals.io_get_globals(Globals, !IO),
    ObjExt = get_object_extension(Globals, PIC),
    fact_table_file_name(ModuleName, FactTableName, ObjExt, do_create_dirs,
        FactTableObjectFile, !IO).

external_foreign_code_files(PIC, Imports, ForeignFiles, !IO) :-
    % Find externally compiled foreign code files for
    % `:- pragma foreign_proc' declarations.

    maybe_pic_object_file_extension(PIC, ObjExt, !IO),
    globals.io_get_target(CompilationTarget, !IO),
    ModuleName = Imports ^ mai_module_name,
    (
        CompilationTarget = target_asm,
        Imports ^ mai_has_foreign_code = contains_foreign_code(Langs),
        set.member(lang_c, Langs)
    ->
        module_name_to_file_name(
            foreign_language_module_name(ModuleName, lang_c),
            ".c", do_not_create_dirs, CCodeFileName, !IO),
        module_name_to_file_name(
            foreign_language_module_name(ModuleName, lang_c),
            ObjExt, do_not_create_dirs, ObjFileName, !IO),
        ForeignFiles0 = [foreign_code_file(lang_c, CCodeFileName, ObjFileName)]
    ;
        CompilationTarget = target_il,
        Imports ^ mai_has_foreign_code = contains_foreign_code(Langs)
    ->
        list.map_foldl(external_foreign_code_files_for_il(ModuleName),
            set.to_sorted_list(Langs), ForeignFilesList, !IO),
        list.condense(ForeignFilesList, ForeignFiles0)
    ;
        ForeignFiles0 = []
    ),

    % Find externally compiled foreign code files for fact tables.
    (
        ( CompilationTarget = target_c
        ; CompilationTarget = target_asm
        ),
        list.map_foldl(
            (pred(FactTableFile::in, FactTableForeignFile::out, di, uo)
                    is det -->
                fact_table_file_name(ModuleName, FactTableFile,
                    ".c", do_not_create_dirs, FactTableCFile),
                fact_table_file_name(ModuleName, FactTableFile,
                    ObjExt, do_not_create_dirs, FactTableObjFile),
                { FactTableForeignFile = foreign_code_file(lang_c,
                    FactTableCFile, FactTableObjFile) }
            ), Imports ^ mai_fact_table_deps, FactTableForeignFiles, !IO),
        ForeignFiles = ForeignFiles0 ++ FactTableForeignFiles
    ;
        ( CompilationTarget = target_java
        ; CompilationTarget = target_il
        ; CompilationTarget = target_x86_64
        ; CompilationTarget = target_erlang
        ),
        ForeignFiles = ForeignFiles0
    ).

:- pred external_foreign_code_files_for_il(module_name::in,
    foreign_language::in, list(foreign_code_file)::out,
    io::di, io::uo) is det.

external_foreign_code_files_for_il(ModuleName, Language, ForeignFiles, !IO) :-
    (
        ForeignModuleName = foreign_language_module_name(ModuleName, Language),
        ForeignExt = foreign_language_file_extension(Language)
    ->
        module_name_to_file_name(ForeignModuleName, ForeignExt, do_create_dirs,
            ForeignFileName, !IO),
        module_name_to_file_name(ForeignModuleName, ".dll", do_create_dirs,
            ForeignDLLFileName, !IO),
        ForeignFiles = [foreign_code_file(Language, ForeignFileName,
            ForeignDLLFileName)]
    ;
        % No external file is generated for this foreign language.
        ForeignFiles = []
    ).

:- func target_type_to_pic(module_target_type) = pic.

target_type_to_pic(TargetType) = Result :-
    (
        ( TargetType = module_target_asm_code(PIC)
        ; TargetType = module_target_object_code(PIC)
        ),
        Result = PIC
    ;
        ( TargetType = module_target_source
        ; TargetType = module_target_errors
        ; TargetType = module_target_private_interface
        ; TargetType = module_target_long_interface
        ; TargetType = module_target_short_interface
        ; TargetType = module_target_unqualified_short_interface
        ; TargetType = module_target_intermodule_interface
        ; TargetType = module_target_analysis_registry
        ; TargetType = module_target_track_flags
        ; TargetType = module_target_c_header(_)
        ; TargetType = module_target_c_code
        ; TargetType = module_target_il_code
        ; TargetType = module_target_il_asm
        ; TargetType = module_target_java_code
        ; TargetType = module_target_java_class_code
        ; TargetType = module_target_erlang_header
        ; TargetType = module_target_erlang_code
        ; TargetType = module_target_erlang_beam_code
        ; TargetType = module_target_foreign_il_asm(_)
        ; TargetType = module_target_foreign_object(_, _)
        ; TargetType = module_target_fact_table_object(_, _)
        ; TargetType = module_target_xml_doc
        ),
        Result = non_pic
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "make.module_target.m".

%-----------------------------------------------------------------------------%
:- end_module make.module_target.
%-----------------------------------------------------------------------------%
