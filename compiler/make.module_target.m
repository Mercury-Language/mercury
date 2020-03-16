%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
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

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module make.dependencies.
:- import_module parse_tree.
:- import_module parse_tree.module_imports.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % make_module_target(Target, Success, !Info).
    %
    % Make a target corresponding to a single module.
    %
:- pred make_module_target(globals::in, dependency_file::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % make_module_target_extra_options(ExtraOpts, Target, Success, !Info)
    %
    % Make a target corresponding to a single module, with extra command line
    % options.
    %
:- pred make_module_target_extra_options(list(string)::in, globals::in,
    dependency_file::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % record_made_target(Globals, Target, Task, MakeSucceeded, !Info, !IO)
    %
    % Record whether building a target succeeded or not.
    % Makes sure any timestamps for files which may have changed
    % in building the target are recomputed next time they are needed.
    % Exported for use by make.module_dep_file.write_module_dep_file.
    %
:- pred record_made_target(globals::in, target_file::in,
    compilation_task_type::in, bool::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

:- type foreign_code_file
    --->    foreign_code_file(
                foreign_language    :: foreign_language,

                % Name of the file produced by the Mercury compiler,
                % e.g. module_c_code.c.
                target_file         :: file_name,

                % Name of the file produced by the foreign language compiler,
                % e.g. module_c_code.o.
                object_file         :: file_name
            ).

    % Find the foreign code files generated when a module is processed.
    % The `pic' field is only used for C foreign code.
    %
:- pred external_foreign_code_files(globals::in, pic::in,
    module_and_imports::in, list(foreign_code_file)::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module libs.process_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_foreign.
:- import_module top_level.                      % XXX unwanted dependency
:- import_module top_level.mercury_compile_main. % XXX unwanted dependency
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module dir.
:- import_module int.
:- import_module float.
:- import_module require.
:- import_module sparse_bitset.

%-----------------------------------------------------------------------------%

make_module_target(Globals, DepFile, Succeeded, !Info, !IO) :-
    make_module_target_extra_options([], Globals, DepFile, Succeeded,
        !Info, !IO).

make_module_target_extra_options(ExtraOptions, Globals, Dep, Succeeded,
        !Info, !IO) :-
    (
        Dep = dep_file(_, _),
        dependency_status(Globals, Dep, Status, !Info, !IO),
        (
            Status = deps_status_error,
            Succeeded = no
        ;
            ( Status = deps_status_not_considered
            ; Status = deps_status_being_built
            ; Status = deps_status_up_to_date
            ),
            Succeeded = yes
        )
    ;
        Dep = dep_target(TargetFile),
        make_module_target_file_extra_options(ExtraOptions, Globals,
            TargetFile, Succeeded, !Info, !IO)
    ).

:- pred make_module_target_file_extra_options(list(string)::in, globals::in,
    target_file::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_module_target_file_extra_options(ExtraOptions, Globals, TargetFile,
        Succeeded, !Info, !IO) :-
    Dep = dep_target(TargetFile),
    dependency_status(Globals, Dep, Status, !Info, !IO),
    (
        Status = deps_status_not_considered,
        TargetFile = target_file(ModuleName, _TargetType),
        get_module_dependencies(Globals, ModuleName, MaybeModuleAndImports,
            !Info, !IO),
        (
            MaybeModuleAndImports = no,
            Succeeded = no,
            DepStatus0 = !.Info ^ dependency_status,
            version_hash_table.set(Dep, deps_status_error,
                DepStatus0, DepStatus),
            !Info ^ dependency_status := DepStatus
        ;
            MaybeModuleAndImports = yes(ModuleAndImports),
            make_module_target_file_main_path(ExtraOptions, Globals,
                TargetFile, ModuleAndImports, Succeeded, !Info, !IO)
        )
    ;
        Status = deps_status_up_to_date,
        Succeeded = yes
    ;
        Status = deps_status_being_built,
        unexpected($pred, "target being built, circular dependencies?")
    ;
        Status = deps_status_error,
        Succeeded = no
    ).

:- pred make_module_target_file_main_path(list(string)::in, globals::in,
    target_file::in, module_and_imports::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_module_target_file_main_path(ExtraOptions, Globals, TargetFile,
        ModuleAndImports, Succeeded, !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    CompilationTask = compilation_task(TargetType),
    module_and_imports_get_source_file_module_name(ModuleAndImports,
        SourceFileModuleName),
    CompilationTask = task_and_options(CompilationTaskType, _),
    ( if
        % For a target built by processing a Mercury source file,
        % the target for a nested sub-module is produced as a side effect
        % of making the target for the top-level module in the file.
        CompilationTaskType = process_module(_),
        SourceFileModuleName \= ModuleName
    then
        NestedTargetFile =
            target_file(SourceFileModuleName, TargetType),
        make_module_target_extra_options(ExtraOptions, Globals,
            dep_target(NestedTargetFile), Succeeded, !Info, !IO)
    else
        touched_files(Globals, TargetFile, CompilationTaskType,
            TouchedTargetFiles, TouchedFiles, !Info, !IO),
        list.foldl(update_target_status(deps_status_being_built),
            TouchedTargetFiles, !Info),

        debug_file_msg(Globals, TargetFile, "checking dependencies", !IO),

        ( if CompilationTaskType = process_module(_) then
            module_and_imports_get_nested_children(ModuleAndImports,
                NestedChildren),
            ModulesToCheck = [ModuleName | set.to_sorted_list(NestedChildren)]
        else
            ModulesToCheck = [ModuleName]
        ),
        module_names_to_index_set(ModulesToCheck, ModulesToCheckSet, !Info),

        deps_set_foldl3_maybe_stop_at_error(!.Info ^ keep_going,
            union_deps(target_dependencies(Globals, TargetType)),
            Globals, ModulesToCheckSet, DepsSuccess,
            sparse_bitset.init, DepFiles0, !Info, !IO),
        dependency_file_index_set_to_plain_set(!.Info, DepFiles0,
            DepFilesSet0),
        ( if TargetType = module_target_int0 then
            % Avoid circular dependencies (the `.int0' files for the
            % nested sub-modules depend on this module's `.int0' file).
            PrivateInts = make_dependency_list(ModulesToCheck,
                module_target_int0),
            DepFilesToMake = set.to_sorted_list(
                set.delete_list(DepFilesSet0, PrivateInts))
        else
            DepFilesToMake = set.to_sorted_list(DepFilesSet0)
        ),

        debug_make_msg(Globals,
           ( pred(!.IO::di, !:IO::uo) is det :-
                make_write_target_file(Globals, TargetFile, !IO),
                io.write_string(": dependencies:\n", !IO),
                dependency_file_index_set_to_plain_set(!.Info,
                    DepFiles0, PlainSet),
                make_write_dependency_file_list(Globals,
                    set.to_sorted_list(PlainSet), !IO)
            ), !IO),

        globals.lookup_bool_option(Globals, keep_going, KeepGoing),
        ( if
            DepsSuccess = no,
            KeepGoing = no
        then
            DepsResult = deps_error
        else
            make_dependency_files(Globals, TargetFile, DepFilesToMake,
                TouchedTargetFiles, TouchedFiles, DepsResult0, !Info, !IO),
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
            Targets0 = !.Info ^ command_line_targets,
            set.delete(ModuleName - module_target(TargetType),
                Targets0, Targets),
            !Info ^ command_line_targets := Targets,
            build_target(Globals, CompilationTask, TargetFile,
                ModuleAndImports, TouchedTargetFiles, TouchedFiles,
                ExtraOptions, Succeeded, !Info, !IO)
        ;
            DepsResult = deps_up_to_date,
            maybe_warn_up_to_date_target(Globals,
                ModuleName - module_target(TargetType), !Info, !IO),
            debug_file_msg(Globals, TargetFile, "up to date", !IO),
            Succeeded = yes,
            list.foldl(update_target_status(deps_status_up_to_date),
                [TargetFile | TouchedTargetFiles], !Info)
        )
    ).

:- pred make_dependency_files(globals::in, target_file::in,
    list(dependency_file)::in, list(target_file)::in, list(file_name)::in,
    dependencies_result::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

make_dependency_files(Globals, TargetFile, DepFilesToMake, TouchedTargetFiles,
        TouchedFiles, DepsResult, !Info, !IO) :-
    % Build the dependencies.

    globals.lookup_bool_option(Globals, keep_going, KeepGoing),
    foldl2_maybe_stop_at_error(KeepGoing, make_module_target,
        Globals, DepFilesToMake, MakeDepsSuccess, !Info, !IO),

    % Check that the target files exist.

    list.map_foldl2(get_target_timestamp(Globals, do_not_search),
        TouchedTargetFiles, TargetTimestamps, !Info, !IO),
    (
        MakeDepsSuccess = no,
        debug_file_msg(Globals, TargetFile, "error making dependencies", !IO),
        DepsResult = deps_error
    ;
        MakeDepsSuccess = yes,
        ( if list.member(error(_), TargetTimestamps) then
            debug_file_msg(Globals, TargetFile, "target file does not exist",
                !IO),
            DepsResult = deps_out_of_date
        else
            ( if
                TargetFile = target_file(ModuleName, TargetType),
                TargetType = module_target_analysis_registry
            then
                force_reanalysis_of_suboptimal_module(Globals, ModuleName,
                    ForceReanalysis, !.Info, !IO)
            else
                ForceReanalysis = no
            ),
            (
                ForceReanalysis = yes,
                DepsResult = deps_out_of_date
            ;
                ForceReanalysis = no,

                % Compare the oldest of the timestamps of the touched
                % files with the timestamps of the dependencies.

                list.map_foldl2(get_timestamp_file_timestamp(Globals),
                    TouchedTargetFiles, TouchedTargetFileTimestamps,
                    !Info, !IO),
                list.map_foldl2(get_file_timestamp([dir.this_directory]),
                    TouchedFiles, TouchedFileTimestamps, !Info, !IO),
                MaybeOldestTimestamp0 = list.foldl(find_oldest_timestamp,
                    TouchedTargetFileTimestamps, ok(newest_timestamp)),
                MaybeOldestTimestamp = list.foldl(find_oldest_timestamp,
                    TouchedFileTimestamps, MaybeOldestTimestamp0),

                get_file_name(Globals, do_not_search, TargetFile,
                    TargetFileName, !Info, !IO),
                check_dependencies(Globals, TargetFileName,
                    MaybeOldestTimestamp, MakeDepsSuccess, DepFilesToMake,
                    DepsResult, !Info, !IO)
            )
        )
    ).

:- pred force_reanalysis_of_suboptimal_module(globals::in, module_name::in,
    bool::out, make_info::in, io::di, io::uo) is det.

force_reanalysis_of_suboptimal_module(Globals, ModuleName, ForceReanalysis,
        Info, !IO) :-
    ( if Info ^ reanalysis_passes > 0 then
        do_read_module_overall_status(mmc, Globals, ModuleName, AnalysisStatus,
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
    else
        ForceReanalysis = no
    ).

%-----------------------------------------------------------------------------%

:- pred build_target(globals::in, compilation_task_type_and_options::in,
    target_file::in, module_and_imports::in, list(target_file)::in,
    list(file_name)::in, list(string)::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_target(Globals, CompilationTask, TargetFile, Imports, TouchedTargetFiles,
        TouchedFiles, ExtraOptions, Succeeded, !Info, !IO) :-
    maybe_make_target_message(Globals, TargetFile, !IO),
    TargetFile = target_file(ModuleName, _TargetType),
    CompilationTask = task_and_options(Task, TaskOptions),
    ( if
        Task = process_module(ModuleTask),
        forkable_module_compilation_task_type(ModuleTask) = yes,
        not can_fork
    then
        % We need a temporary file to pass the arguments to the mmc process
        % which will do the compilation. It is created here (not in invoke_mmc)
        % so it can be cleaned up by build_with_check_for_interrupt.
        io.make_temp_file(ArgFileNameResult, !IO),
        (
            ArgFileNameResult = ok(ArgFileName),
            MaybeArgFileName = yes(ArgFileName),
            ArgFileNameSuccess = ok : io.res
        ;
            ArgFileNameResult = error(Error),
            MaybeArgFileName = no,
            ArgFileNameSuccess = error(Error)
        )
    else
        MaybeArgFileName = no,
        ArgFileNameSuccess = ok
    ),

    (
        ArgFileNameSuccess = ok,
        get_real_milliseconds(Time0, !IO),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        Cleanup = cleanup_files(Globals, MaybeArgFileName,
            TouchedTargetFiles, TouchedFiles),
        build_with_check_for_interrupt(VeryVerbose,
            build_with_module_options_and_output_redirect(Globals, ModuleName,
                ExtraOptions ++ TaskOptions,
                build_target_2(ModuleName, Task, MaybeArgFileName, Imports)),
            Cleanup, Succeeded, !Info, !IO),
        record_made_target_2(Globals, Succeeded, TargetFile,
            TouchedTargetFiles, TouchedFiles, !Info, !IO),
        get_real_milliseconds(Time, !IO),

        globals.lookup_bool_option(Globals, show_make_times, ShowMakeTimes),
        (
            ShowMakeTimes = yes,
            DiffSecs = float(Time - Time0) / 1000.0,
            % Avoid cluttering the screen with short running times.
            ( if DiffSecs >= 0.4 then
                io.write_string("Making ", !IO),
                make_write_target_file(Globals, TargetFile, !IO),
                io.format(" took %.2fs\n", [f(DiffSecs)], !IO)
            else
                true
            )
        ;
            ShowMakeTimes = no
        )
    ;
        ArgFileNameSuccess = error(ArgFileError),
        io.format(stderr_stream, "Could not create temporary file: %s\n",
            [s(error_message(ArgFileError))], !IO),
        Succeeded = no
    ).

:- pred cleanup_files(globals::in, maybe(string)::in,
    list(target_file)::in, list(string)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

cleanup_files(Globals, MaybeArgFileName, TouchedTargetFiles, TouchedFiles,
        !MakeInfo, !IO) :-
    % XXX Remove `.int.tmp' files.
    list.foldl2(make_remove_target_file(Globals, very_verbose),
        TouchedTargetFiles, !MakeInfo, !IO),
    list.foldl2(make_remove_file(Globals, very_verbose),
        TouchedFiles, !MakeInfo, !IO),
    (
        MaybeArgFileName = yes(ArgFileName2),
        io.remove_file(ArgFileName2, _, !IO)
    ;
        MaybeArgFileName = no
    ).

:- pred build_target_2(module_name::in, compilation_task_type::in,
    maybe(file_name)::in, module_and_imports::in, globals::in,
    list(string)::in, io.output_stream::in, bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_target_2(ModuleName, Task, ArgFileName, Imports, Globals, AllOptionArgs,
        ErrorStream, Succeeded, !Info, !IO) :-
    (
        Task = process_module(ModuleTask),
        ModuleArg = sym_name_to_string(ModuleName),

        globals.lookup_bool_option(Globals, verbose_commands, Verbose),
        (
            Verbose = yes,
            AllArgs = list.append(AllOptionArgs, [ModuleArg]),
            io.write_string("Invoking self `mmc ", !IO),
            % XXX Don't write the default options.
            io.write_list(list.map(quote_arg, AllArgs), " ", io.write_string,
                !IO),
            io.write_string("'", !IO),
            io.nl(!IO)
        ;
            Verbose = no
        ),

        % Run compilations to target code in a separate process. This avoids
        % problems with the Boehm GC retaining memory by scanning too much of
        % the Mercury stacks. If the compilation is run in a separate process,
        % it is also easier to kill if an interrupt arrives. We do the same for
        % intermodule-optimization interfaces because if type checking gets
        % overloaded by ambiguities, it can be difficult to kill the compiler
        % otherwise.
        % XXX The above comment is likely to be quite out-of-date.
        io.set_output_stream(ErrorStream, OldOutputStream, !IO),
        IsForkable = forkable_module_compilation_task_type(ModuleTask),
        (
            IsForkable = yes,
            call_in_forked_process_with_backup(
                call_mercury_compile_main(Globals, [ModuleArg]),
                invoke_mmc(Globals, ErrorStream, ArgFileName,
                    AllOptionArgs ++ [ModuleArg]),
                Succeeded, !IO)
        ;
            IsForkable = no,
            call_mercury_compile_main(Globals, [ModuleArg], Succeeded, !IO)
        ),
        io.set_output_stream(OldOutputStream, _, !IO),

        ( if
            ( ModuleTask = task_compile_to_target_code
            ; ModuleTask = task_errorcheck
            )
        then
            % The `.err_date' file is needed because the `.err' file is touched
            % by all phases of compilation, including writing interfaces.
            touch_interface_datestamp(Globals, ModuleName, ".err_date", !IO)
        else
            true
        )
    ;
        Task = target_code_to_object_code(PIC),
        globals.get_target(Globals, CompilationTarget),

        % Run the compilation in a child process so it can be killed
        % if an interrupt arrives.
        call_in_forked_process(
            build_object_code(Globals, ModuleName, CompilationTarget, PIC,
                ErrorStream, Imports),
            Succeeded, !IO)
    ;
        Task = foreign_code_to_object_code(PIC, Lang),
        get_foreign_code_file(Globals, ModuleName, PIC, Lang, ForeignCodeFile,
            !IO),

        % Run the compilation in a child process so it can be killed
        % if an interrupt arrives.
        call_in_forked_process(
            compile_foreign_code_file(Globals, ErrorStream, PIC, Imports,
                ForeignCodeFile),
            Succeeded, !IO)
    ;
        Task = fact_table_code_to_object_code(PIC, FactTableFile),
        fact_table_foreign_code_file(Globals, ModuleName, PIC, FactTableFile,
            FactTableForeignCode, !IO),

        % Run the compilation in a child process so it can be killed
        % if an interrupt arrives.
        call_in_forked_process(
            compile_foreign_code_file(Globals, ErrorStream, PIC, Imports,
                FactTableForeignCode),
            Succeeded, !IO)
    ).

:- pred build_object_code(globals::in, module_name::in, compilation_target::in,
    pic::in, io.output_stream::in, module_and_imports::in, bool::out,
    io::di, io::uo) is det.

build_object_code(Globals, ModuleName, Target, PIC, ErrorStream, _Imports,
        Succeeded, !IO) :-
    (
        Target = target_c,
        compile_c_file(Globals, ErrorStream, PIC, ModuleName, Succeeded, !IO)
    ;
        Target = target_java,
        module_name_to_file_name(Globals, do_create_dirs, ".java",
            ModuleName, JavaFile, !IO),
        compile_java_files(Globals, ErrorStream, [JavaFile], Succeeded, !IO)
    ;
        Target = target_csharp,
        module_name_to_file_name(Globals, do_create_dirs, ".cs",
            ModuleName, CsharpFile, !IO),
        compile_target_code.link(Globals, ErrorStream, csharp_library,
            ModuleName, [CsharpFile], Succeeded, !IO)
    ;
        Target = target_erlang,
        module_name_to_file_name(Globals, do_create_dirs, ".erl",
            ModuleName, ErlangFile, !IO),
        compile_erlang_file(Globals, ErrorStream, ErlangFile, Succeeded, !IO)
    ).

:- pred compile_foreign_code_file(globals::in, io.output_stream::in, pic::in,
    module_and_imports::in, foreign_code_file::in, bool::out,
    io::di, io::uo) is det.

compile_foreign_code_file(Globals, ErrorStream, PIC, Imports, ForeignCodeFile,
        Succeeded, !IO) :-
    (
        ForeignCodeFile = foreign_code_file(lang_c, CFile, ObjFile),
        do_compile_c_file(Globals, ErrorStream, PIC, CFile, ObjFile, Succeeded,
            !IO)
    ;
        ForeignCodeFile = foreign_code_file(lang_java, JavaFile, _ClassFile),
        compile_java_files(Globals, ErrorStream, [JavaFile], Succeeded, !IO)
    ;
        ForeignCodeFile = foreign_code_file(lang_csharp, CSharpFile, DLLFile),
        compile_csharp_file(Globals, ErrorStream, Imports, CSharpFile, DLLFile,
            Succeeded, !IO)
    ;
        ForeignCodeFile = foreign_code_file(lang_erlang, ErlFile, _BeamFile),
        compile_erlang_file(Globals, ErrorStream, ErlFile, Succeeded, !IO)
    ).

:- func forkable_module_compilation_task_type(module_compilation_task_type)
    = bool.

forkable_module_compilation_task_type(task_errorcheck) = no.
forkable_module_compilation_task_type(task_make_int0) = no.
forkable_module_compilation_task_type(task_make_int12) = no.
forkable_module_compilation_task_type(task_make_int3) = no.
forkable_module_compilation_task_type(task_make_opt) = yes.
forkable_module_compilation_task_type(task_make_analysis_registry) = yes.
forkable_module_compilation_task_type(task_compile_to_target_code) = yes.
forkable_module_compilation_task_type(task_make_xml_doc) = yes.

%-----------------------------------------------------------------------------%

:- pred get_foreign_code_file(globals::in, module_name::in, pic::in,
    foreign_language::in, foreign_code_file::out, io::di, io::uo) is det.

get_foreign_code_file(Globals, ModuleName, PIC, Lang, ForeignCodeFile, !IO) :-
    ( if
        ForeignModName0 = foreign_language_module_name(ModuleName, Lang),
        SrcExt0 = foreign_language_file_extension(Lang)
    then
        ForeignModName = ForeignModName0,
        SrcExt = SrcExt0
    else
        unexpected($pred, "unsupported foreign language")
    ),
    ObjExt = get_object_extension(Globals, PIC),
    module_name_to_file_name(Globals, do_create_dirs, SrcExt,
        ForeignModName, SrcFileName, !IO),
    module_name_to_file_name(Globals, do_create_dirs, ObjExt,
        ForeignModName, ObjFileName, !IO),
    ForeignCodeFile = foreign_code_file(Lang, SrcFileName, ObjFileName).

:- pred fact_table_foreign_code_file(globals::in, module_name::in, pic::in,
    string::in, foreign_code_file::out, io::di, io::uo) is det.

fact_table_foreign_code_file(Globals, ModuleName, PIC, FactTableName,
        ForeignCodeFile, !IO) :-
    ObjExt = get_object_extension(Globals, PIC),
    fact_table_file_name(Globals, ModuleName, FactTableName, ".c",
        do_create_dirs, CFile, !IO),
    fact_table_file_name(Globals, ModuleName, FactTableName, ObjExt,
        do_create_dirs, ObjFile, !IO),
    ForeignCodeFile = foreign_code_file(lang_c, CFile, ObjFile).

:- func get_object_extension(globals, pic) = string.

get_object_extension(Globals, PIC) = Ext :-
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        maybe_pic_object_file_extension(Globals, PIC, Ext)
    ;
        CompilationTarget = target_csharp,
        sorry($pred, "object extension for csharp")
    ;
        CompilationTarget = target_java,
        sorry($pred, "object extension for java")
    ;
        CompilationTarget = target_erlang,
        sorry($pred, "mmc --make NYI and target erlang")
    ).

%-----------------------------------------------------------------------------%

:- pred call_mercury_compile_main(globals::in, list(string)::in, bool::out,
    io::di, io::uo) is det.

call_mercury_compile_main(Globals, Args, Succeeded, !IO) :-
    io.get_exit_status(Status0, !IO),
    io.set_exit_status(0, !IO),
    mercury_compile_main.main_for_make(Globals, Args, !IO),
    io.get_exit_status(Status, !IO),
    Succeeded = ( if Status = 0 then yes else no ),
    io.set_exit_status(Status0, !IO).

:- pred invoke_mmc(globals::in, io.output_stream::in, maybe(file_name)::in,
    list(string)::in, bool::out, io::di, io::uo) is det.

invoke_mmc(Globals, ErrorStream, MaybeArgFileName, Args, Succeeded, !IO) :-
    io.progname("", ProgName, !IO),
    ( if
        % NOTE: if the compiler is built in the Java grade then ProgName will
        % be set to "top_level" (which was the name compiled into the module
        % containing the predicate main/2). We don't want to attempt to
        % invoke an executable named "top_level" however, since the wrapper
        % script will have been renamed to "mercury_compile" by the Mmakefile
        % in the compiler directory.
        ( ProgName = ""
        ; target_is_java
        )
    then
        io.get_environment_var("MERCURY_COMPILER", MaybeMercuryCompiler, !IO),
        (
            MaybeMercuryCompiler = yes(MercuryCompiler)
        ;
            MaybeMercuryCompiler = no,
            MercuryCompiler = "mmc"
        )
    else
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
        unexpected($pred, "argument file not created")
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
        invoke_system_command(Globals, ErrorStream, CommandVerbosity, Command,
            Succeeded, !IO)
    ;
        ArgFileOpenRes = error(Error),
        Succeeded = no,
        io.error_message(Error, ErrorMsg),
        io.format("Error opening `%s' for output: %s\n",
            [s(ArgFileName), s(ErrorMsg)], !IO)
    ),
    io.remove_file(ArgFileName, _, !IO).

:- pred target_is_java is semidet.

:- pragma foreign_proc("Java",
    target_is_java,
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

target_is_java :-
    semidet_fail.

%-----------------------------------------------------------------------------%

record_made_target(Globals, TargetFile, CompilationTask, Succeeded,
        !Info, !IO) :-
    touched_files(Globals, TargetFile, CompilationTask, TouchedTargetFiles,
        TouchedFiles, !Info, !IO),
    record_made_target_2(Globals, Succeeded, TargetFile, TouchedTargetFiles,
        TouchedFiles, !Info, !IO).

:- pred record_made_target_2(globals::in, bool::in, target_file::in,
    list(target_file)::in, list(file_name)::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

record_made_target_2(Globals, Succeeded, TargetFile, TouchedTargetFiles,
        OtherTouchedFiles, !Info, !IO) :-
    (
        Succeeded = yes,
        TargetStatus = deps_status_up_to_date
    ;
        Succeeded = no,
        TargetStatus = deps_status_error,
        target_file_error(!.Info, Globals, TargetFile, !IO)
    ),

    list.foldl(update_target_status(TargetStatus), TouchedTargetFiles, !Info),

    list.map_foldl2(get_file_name(Globals, do_not_search), TouchedTargetFiles,
        TouchedTargetFileNames, !Info, !IO),

    some [!Timestamps] (
        !:Timestamps = !.Info ^ file_timestamps,
        list.foldl(delete_timestamp(Globals), TouchedTargetFileNames,
            !Timestamps),
        list.foldl(delete_timestamp(Globals), OtherTouchedFiles, !Timestamps),

        % When an .analysis file is made, that potentially invalidates other
        % .analysis files so we have to delete their timestamps. The exact list
        % of files which might be affected can be found by reading the
        % corresponding .imdg file. But it is simpler to just delete the
        % timestamps of all the .analysis files that we know about.
        ( if TargetFile = target_file(_, module_target_analysis_registry) then
            map.foldl(delete_analysis_registry_timestamps(Globals),
                !.Timestamps, !Timestamps)
        else
            true
        ),

        !Info ^ file_timestamps := !.Timestamps
    ).

:- pred update_target_status(dependency_status::in, target_file::in,
    make_info::in, make_info::out) is det.

update_target_status(TargetStatus, TargetFile, !Info) :-
    Dep = dep_target(TargetFile),
    DepStatus0 = !.Info ^ dependency_status,
    version_hash_table.set(Dep, TargetStatus, DepStatus0, DepStatus),
    !Info ^ dependency_status := DepStatus.

:- pred delete_analysis_registry_timestamps(globals::in, string::in,
    maybe_error(timestamp)::in,
    file_timestamps::in, file_timestamps::out) is det.

delete_analysis_registry_timestamps(Globals, FileName, _, !Timestamps) :-
    ( if string.suffix(FileName, ".analysis") then
        delete_timestamp(Globals, FileName, !Timestamps)
    else
        true
    ).

:- pred delete_timestamp(globals::in, string::in,
    file_timestamps::in, file_timestamps::out) is det.

delete_timestamp(Globals, TouchedFile, !Timestamps) :-
    trace [io(!IO)] (
        debug_make_msg(Globals,
            ( pred(!.IO::di, !:IO::uo) is det :-
                io.write_string("Deleting timestamp for ", !IO),
                io.write_string(TouchedFile, !IO),
                io.nl(!IO)
            ), !IO)
    ),
    map.delete(TouchedFile, !Timestamps).

%-----------------------------------------------------------------------------%

:- type compilation_task_type_and_options
    --->    task_and_options(
                compilation_task_type,
                list(string)
            ).

:- func compilation_task(module_target_type) =
    compilation_task_type_and_options.

compilation_task(Target) = Result :-
    (
        ( Target = module_target_source
        ; Target = module_target_track_flags
        ),
        unexpected($pred, "compilation_task")
    ;
        Target = module_target_errors,
        Result = task_and_options(process_module(task_errorcheck),
            ["--errorcheck-only"])
    ;
        Target = module_target_int0,
        Result = task_and_options(process_module(task_make_int0),
            ["--make-private-interface"])
    ;
        ( Target = module_target_int1
        ; Target = module_target_int2
        ),
        Result = task_and_options(process_module(task_make_int12),
            ["--make-interface"])
    ;
        Target = module_target_int3,
        Result = task_and_options(process_module(task_make_int3),
            ["--make-short-interface"])
    ;
        Target = module_target_opt,
        Result = task_and_options(process_module(task_make_opt),
            ["--make-optimization-interface"])
    ;
        Target = module_target_analysis_registry,
        Result = task_and_options(process_module(task_make_analysis_registry),
            ["--make-analysis-registry"])
    ;
        ( Target = module_target_c_header(_)
        ; Target = module_target_c_code
        ),
        Result = task_and_options(process_module(task_compile_to_target_code),
            ["--compile-to-c"])
    ;
        Target = module_target_csharp_code,
        Result = task_and_options(process_module(task_compile_to_target_code),
            ["--csharp-only"])
    ;
        Target = module_target_java_code,
        Result = task_and_options(process_module(task_compile_to_target_code),
            ["--java-only"])
    ;
        Target = module_target_java_class_code,
        Result = task_and_options(target_code_to_object_code(non_pic), [])
    ;
        ( Target = module_target_erlang_header
        ; Target = module_target_erlang_code
        ),
        Result = task_and_options(process_module(task_compile_to_target_code),
            ["--erlang-only"])
    ;
        Target = module_target_erlang_beam_code,
        Result = task_and_options(target_code_to_object_code(non_pic), [])
    ;
        Target = module_target_object_code(PIC),
        Result = task_and_options(target_code_to_object_code(PIC), [])
    ;
        Target = module_target_foreign_object(PIC, Lang),
        Result = task_and_options(foreign_code_to_object_code(PIC, Lang), [])
    ;
        Target = module_target_fact_table_object(PIC, FactTable),
        Result = task_and_options(
            fact_table_code_to_object_code(PIC, FactTable), [])
    ;
        Target = module_target_xml_doc,
        Result = task_and_options(process_module(task_make_xml_doc),
            ["--make-xml-doc"])
    ).

    % Find the files which could be touched by a compilation task.
    %
:- pred touched_files(globals::in, target_file::in, compilation_task_type::in,
    list(target_file)::out, list(file_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

touched_files(Globals, TargetFile, Task, TouchedTargetFiles, TouchedFileNames,
        !Info, !IO) :-
    (
        Task = process_module(ModuleTask),
        touched_files_process_module(Globals, TargetFile, ModuleTask,
            TouchedTargetFiles, TouchedFileNames, !Info, !IO)
    ;
        Task = target_code_to_object_code(_),
        TouchedTargetFiles = [TargetFile],
        TouchedFileNames = []
    ;
        Task = foreign_code_to_object_code(PIC, Lang),
        TouchedTargetFiles = [TargetFile],
        TargetFile = target_file(ModuleName, _),
        get_foreign_code_file(Globals, ModuleName, PIC, Lang, ForeignCodeFile,
            !IO),
        ForeignObjectFile = ForeignCodeFile ^ object_file,
        TouchedFileNames = [ForeignObjectFile]
    ;
        Task = fact_table_code_to_object_code(PIC, FactTableName),
        TouchedTargetFiles = [TargetFile],
        TargetFile = target_file(ModuleName, _),
        ObjExt = get_object_extension(Globals, PIC),
        fact_table_file_name(Globals, ModuleName, FactTableName, ObjExt,
            do_create_dirs, FactTableObjectFile, !IO),
        TouchedFileNames = [FactTableObjectFile]
    ).

:- pred touched_files_process_module(globals::in, target_file::in,
    module_compilation_task_type::in, list(target_file)::out,
    list(file_name)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

touched_files_process_module(Globals, TargetFile, Task, TouchedTargetFiles,
        TouchedFileNames, !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    get_module_dependencies(Globals, ModuleName, MaybeModuleAndImports,
        !Info, !IO),
    (
        MaybeModuleAndImports = yes(ModuleAndImports)
    ;
        MaybeModuleAndImports = no,
        % This error should have been caught earlier. We shouldn't be
        % attempting to build a target if we couldn't find the dependencies
        % for the module.
        unexpected($pred, "no module dependencies")
    ),

    module_and_imports_get_nested_children(ModuleAndImports,
        NestedChildrenSet),
    set.to_sorted_list(NestedChildrenSet, NestedChildren),
    SourceFileModuleNames = [ModuleName | NestedChildren],

    list.map_foldl2(get_module_dependencies(Globals), NestedChildren,
        MaybeNestedImportsList, !Info, !IO),
    ( if
        list.map(
            ( pred(yes(NestedModuleImports)::in, NestedModuleImports::out)
                is semidet),
            MaybeNestedImportsList, NestedImportsList)
    then
        ModuleImportsList = [ModuleAndImports | NestedImportsList]
    else
        % This error should have been caught earlier. We shouldn't be
        % attempting to build a target if we couldn't find the dependencies
        % for the module or its nested sub-modules.
        unexpected($pred, "no nested module dependencies")
    ),

    globals.get_target(Globals, CompilationTarget),
    TargetModuleNames = SourceFileModuleNames,

    % Find out what header files are generated.
    (
        Task = task_compile_to_target_code,
        TargetPIC = target_type_to_pic(TargetType),
        list.map_foldl(external_foreign_code_files(Globals, TargetPIC),
            ModuleImportsList, ForeignCodeFileList, !IO),
        ForeignCodeFiles =
            list.map((func(ForeignFile) = ForeignFile ^ target_file),
                list.condense(ForeignCodeFileList)),
        (
            CompilationTarget = target_c,
            globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
            (
                HighLevelCode = yes,
                % When compiling to high-level C, we always generate
                % a header file.
                HeaderModuleNames = SourceFileModuleNames,
                HeaderTargets0 = make_target_file_list(HeaderModuleNames,
                    module_target_c_header(header_mih))
            ;
                HighLevelCode = no,
                HeaderTargets0 = []
            )
        ;
            ( CompilationTarget = target_csharp
            ; CompilationTarget = target_java
            ),
            HeaderTargets0 = []
        ;
            CompilationTarget = target_erlang,
            % When compiling to Erlang we always generate a header file.
            HeaderModuleNames = SourceFileModuleNames,
            HeaderTargets0 = make_target_file_list(HeaderModuleNames,
                module_target_erlang_header)
        ),

        (
            CompilationTarget = target_c,
            Names = SourceFileModuleNames,
            HeaderTargets =
                make_target_file_list(Names, module_target_c_header(header_mh))
                ++ HeaderTargets0
        ;
            ( CompilationTarget = target_csharp
            ; CompilationTarget = target_java
            ; CompilationTarget = target_erlang
            ),
            HeaderTargets = HeaderTargets0
        ),

        TouchedTargetFiles0 = make_target_file_list(TargetModuleNames,
            TargetType),
        TouchedTargetFiles = TouchedTargetFiles0 ++ HeaderTargets
    ;
        Task = task_make_int12,
        % Both long and short interface files are produced
        % when making the interface.
        ForeignCodeFiles = [],
        TouchedTargetFiles =
            make_target_file_list(TargetModuleNames, module_target_int1) ++
            make_target_file_list(TargetModuleNames, module_target_int2)
    ;
        ( Task = task_errorcheck
        ; Task = task_make_int0
        ; Task = task_make_int3
        ; Task = task_make_opt
        ; Task = task_make_analysis_registry
        ; Task = task_make_xml_doc
        ),
        ForeignCodeFiles = [],
        TouchedTargetFiles =
            make_target_file_list(TargetModuleNames, TargetType)
    ),
    list.foldl2(gather_target_file_timestamp_file_names(Globals),
        TouchedTargetFiles, [], TimestampFileNames, !IO),
    TouchedFileNames = ForeignCodeFiles ++ TimestampFileNames.

:- pred gather_target_file_timestamp_file_names(globals::in, target_file::in,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

gather_target_file_timestamp_file_names(Globals, TouchedTargetFile,
        !TimestampFileNames, !IO) :-
    TouchedTargetFile = target_file(TargetModuleName, TargetType),
    ( if timestamp_extension(TargetType, TimestampExt) then
        module_name_to_file_name(Globals, do_not_create_dirs, TimestampExt,
            TargetModuleName, TimestampFile, !IO),
        list.cons(TimestampFile, !TimestampFileNames)
    else
        true
    ).

external_foreign_code_files(Globals, PIC, ModuleAndImports, ForeignFiles,
        !IO) :-
    % Find externally compiled foreign code files for
    % `:- pragma foreign_proc' declarations.
    %
    % Any changed here may require corresponding changes in 
    % get_foreign_object_targets.

    maybe_pic_object_file_extension(Globals, PIC, ObjExt),
    globals.get_target(Globals, CompilationTarget),
    module_and_imports_get_module_name(ModuleAndImports, ModuleName),

    % None of the current backends require externally compiled foreign code,
    % except the C backend for fact tables.
    (
        CompilationTarget = target_c,
        module_and_imports_get_fact_table_deps(ModuleAndImports, FactDeps),
        list.map_foldl(
            get_fact_table_foreign_code_file(Globals, ObjExt, ModuleName),
            FactDeps, FactTableForeignFiles, !IO),
        ForeignFiles = FactTableForeignFiles
    ;
        ( CompilationTarget = target_java
        ; CompilationTarget = target_csharp
        ; CompilationTarget = target_erlang
        ),
        ForeignFiles = []
    ).

:- pred get_fact_table_foreign_code_file(globals::in, string::in,
    module_name::in, file_name::in, foreign_code_file::out,
    io::di, io::uo) is det.

get_fact_table_foreign_code_file(Globals, ObjExt, ModuleName, FactTableFile,
        FactTableForeignFile, !IO) :-
    fact_table_file_name(Globals, ModuleName, FactTableFile,
        ".c", do_not_create_dirs, FactTableCFile, !IO),
    fact_table_file_name(Globals, ModuleName, FactTableFile,
        ObjExt, do_not_create_dirs, FactTableObjFile, !IO),
    FactTableForeignFile =
        foreign_code_file(lang_c, FactTableCFile, FactTableObjFile).

:- func target_type_to_pic(module_target_type) = pic.

target_type_to_pic(TargetType) = Result :-
    (
        TargetType = module_target_object_code(PIC),
        Result = PIC
    ;
        ( TargetType = module_target_source
        ; TargetType = module_target_errors
        ; TargetType = module_target_int0
        ; TargetType = module_target_int1
        ; TargetType = module_target_int2
        ; TargetType = module_target_int3
        ; TargetType = module_target_opt
        ; TargetType = module_target_analysis_registry
        ; TargetType = module_target_track_flags
        ; TargetType = module_target_c_header(_)
        ; TargetType = module_target_c_code
        ; TargetType = module_target_csharp_code
        ; TargetType = module_target_java_code
        ; TargetType = module_target_java_class_code
        ; TargetType = module_target_erlang_header
        ; TargetType = module_target_erlang_code
        ; TargetType = module_target_erlang_beam_code
        ; TargetType = module_target_foreign_object(_, _)
        ; TargetType = module_target_fact_table_object(_, _)
        ; TargetType = module_target_xml_doc
        ),
        Result = non_pic
    ).

%-----------------------------------------------------------------------------%
:- end_module make.module_target.
%-----------------------------------------------------------------------------%
