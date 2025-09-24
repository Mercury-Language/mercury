%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.module_target.m.
% Main author: stayl.
%
% Build targets which relate to a single module (e.g. C code, object code,
% interface files).
%
%---------------------------------------------------------------------------%

:- module make.module_target.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module make.make_info.
:- import_module parse_tree.
:- import_module parse_tree.module_dep_info.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % make_module_target(ExtraOpts, ProgressStream, Globals, Target,
    %   Succeeded, !Info, !IO):
    %
    % Make a target corresponding to a single module, possibly with
    % extra command line options.
    %
    % ExtraOpts must be the first argument, because we curry it.
    %
:- pred make_module_target(list(string)::in, io.text_output_stream::in,
    globals::in, target_id::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % record_made_target(ProgressStream, Globals, TargetFile,
    %   TargetFileName, Task, MakeSucceeded, !Info, !IO):
    %
    % Record whether building a target succeeded or not.
    % Makes sure any timestamps for files which may have changed
    % in building the target are recomputed next time they are needed.
    % Exported for use by make.module_dep_file.write_module_dep_file.
    %
:- pred record_made_target(io.text_output_stream::in, globals::in,
    target_file::in, file_name::in,
    compilation_task_type::in, maybe_succeeded::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

:- type foreign_code_file
    --->    foreign_code_file(
                fcf_foreign_language    :: foreign_language,

                % Name of the file produced by the Mercury compiler,
                % e.g. module_c_code.c.
                fcf_target_file         :: file_name,

                % Name of the file produced by the foreign language compiler,
                % e.g. module_c_code.o.
                fcf_object_file         :: file_name
            ).

    % Find the foreign code files generated for fact tables
    % when a module is processed. For now, the result will be the empty list
    % for any target language other than C.
    %
:- pred get_any_fact_table_object_code_files(globals::in, pic::in,
    module_dep_info::in, list(foreign_code_file)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.link_target_code.
:- import_module libs.options.
:- import_module libs.process_util.
:- import_module libs.shell_util.
:- import_module libs.system_cmds.
:- import_module libs.timestamp.
:- import_module make.build.
:- import_module make.check_up_to_date.
:- import_module make.file_names.
:- import_module make.get_module_dep_info.
:- import_module make.prereqs.
:- import_module make.util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.write_error_spec.
:- import_module top_level.                      % XXX unwanted dependency
:- import_module top_level.mercury_compile_main. % XXX unwanted dependency

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module io.environment.
:- import_module io.file.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

make_module_target(ExtraOptions, ProgressStream, Globals, TargetId, Succeeded,
        !Info, !IO) :-
    get_target_id_status(ProgressStream, Globals, TargetId, StatusResult,
        !Info, !IO),
    StatusResult = target_status_result(_TargetId, _TargetFileName, Status),
    (
        Status = target_status_error,
        Succeeded = did_not_succeed
    ;
        Status = target_status_up_to_date,
        Succeeded = succeeded
    ;
        Status = target_status_not_considered,
        (
            TargetId = non_merc_target(_),
            Succeeded = succeeded
        ;
            TargetId = merc_target(TargetFile),
            TargetFile = target_file(ModuleName, TargetType),
            get_maybe_module_dep_info(ProgressStream, Globals,
                ModuleName, MaybeModuleDepInfo, !Info, !IO),
            (
                MaybeModuleDepInfo = no_module_dep_info,
                Succeeded = did_not_succeed,
                TargetStatusMap0 = make_info_get_target_status_map(!.Info),
                version_hash_table.set(TargetId, target_status_error,
                    TargetStatusMap0, TargetStatusMap),
                make_info_set_target_status_map(TargetStatusMap, !Info)
            ;
                MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
                get_compilation_task_and_options(TargetType,
                    CompilationTaskAndOptions),
                module_dep_info_get_source_file_module_name(ModuleDepInfo,
                    SourceFileModuleName),
                CompilationTaskAndOptions =
                    task_and_options(CompilationTaskType, _),
                ( if
                    % For a target built by processing a Mercury source file,
                    % the target for a nested submodule is produced as
                    % a side effect of making the target for the top-level
                    % module in the file.
                    CompilationTaskType = process_module(_),
                    SourceFileModuleName \= ModuleName
                then
                    MainTargetFile =
                        target_file(SourceFileModuleName, TargetType),
                    % Recursive call. We should not recurse more than once.
                    make_module_target(ExtraOptions, ProgressStream, Globals,
                        merc_target(MainTargetFile), Succeeded, !Info, !IO)
                else
                    make_module_target_file_main_path(ExtraOptions,
                        ProgressStream, Globals, TargetFile,
                        CompilationTaskAndOptions, ModuleDepInfo,
                        Succeeded, !Info, !IO)
                )
            )
        )
    ;
        Status = target_status_being_built,
        (
            TargetId = non_merc_target(_),
            Succeeded = succeeded
        ;
            TargetId = merc_target(_),
            unexpected($pred, "target being built, circular dependencies?")
        )
    ).

:- pred make_module_target_file_main_path(list(string)::in,
    io.text_output_stream::in, globals::in,
    target_file::in, compilation_task_type_and_options::in,
    module_dep_info::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_module_target_file_main_path(ExtraOptions, ProgressStream, Globals,
        TargetFile, CompilationTaskAndOptions, ModuleDepInfo, Succeeded,
        !Info, !IO) :-
    % NOTE Our caller computes CompilationTaskAndOptions from the target_type
    % component of the TargetFile argument, which means that the only valid
    % target type/compilation task pairs are those that occur in the
    % predicate that does that computation, get_compilation_task_and_options.
    TargetFile = target_file(ModuleName, TargetType),
    % XXX LEGACY
    module_target_file_to_file_name(Globals, $pred,
        TargetFile, TargetFileName, _TargetFileNameProposed, !IO),
    CompilationTaskAndOptions = task_and_options(CompilationTaskType, _),
    find_lhs_files_of_task(ProgressStream, Globals, TargetFile,
        CompilationTaskType, MakeLhsFiles, !Info, !IO),
    MakeLhsFiles =
        make_lhs_files(DatelessLhsTargetFiles, DatedLhsTargetFiles, _, _),
    list.foldl(update_target_status(target_status_being_built),
        DatelessLhsTargetFiles, !Info),
    list.foldl(update_target_status(target_status_being_built),
        DatedLhsTargetFiles, !Info),

    debug_make_msg(Globals,
        string.format("%s: checking dependencies\n", [s(TargetFileName)]),
        CheckingMsg),
    maybe_write_msg(ProgressStream, CheckingMsg, !IO),

    find_direct_prereqs_of_target_file(ProgressStream, Globals,
        CompilationTaskType, ModuleDepInfo, TargetFile, RhsResult,
        !Info, !IO),

    KeepGoing = make_info_get_keep_going(!.Info),
    ( if
        RhsResult = could_not_find_some_prereqs(_),
        KeepGoing = do_not_keep_going
    then
        LhsResult = rhs_error
    else
        ( RhsResult = could_not_find_some_prereqs(RhsTargetIdSet)
        ; RhsResult = found_all_prereqs(RhsTargetIdSet)
        ),
        % XXX MAKE sort
        RhsTargetIds = set.to_sorted_list(RhsTargetIdSet),
        % Build the files on the rhs.
        foldl2_make_module_targets(KeepGoing, [], ProgressStream, Globals,
            RhsTargetIds, MakeRhsFilesSucceeded, !Info, !IO),
        (
            MakeRhsFilesSucceeded = did_not_succeed,
            debug_make_msg(Globals,
                string.format("%s: error making prerequisites\n",
                    [s(TargetFileName)]),
                RhsErrorDebugMsg),
            maybe_write_msg(ProgressStream, RhsErrorDebugMsg, !IO),
            LhsResult = rhs_error
        ;
            MakeRhsFilesSucceeded = succeeded,
            % We succeeded in making RhsTargetIds. However, if there are
            % prerequisities that we could not find, then we cannot build
            % the lhs files.
            (
                RhsResult = found_all_prereqs(_),
                must_or_should_we_rebuild_lhs(ProgressStream, Globals,
                    TargetFile, TargetFileName, MakeLhsFiles, RhsTargetIds,
                    LhsResult, !Info, !IO)
            ;
                RhsResult = could_not_find_some_prereqs(_),
                LhsResult = rhs_error
            )
        )
    ),
    (
        LhsResult = rhs_error,
        Succeeded = did_not_succeed,
        LhsTargetFiles = DatelessLhsTargetFiles ++ DatedLhsTargetFiles,
        list.foldl(update_target_status(target_status_error),
            LhsTargetFiles, !Info)
    ;
        LhsResult = can_rebuild_lhs(some_lhs_file_needs_rebuilding),
        Targets0 = make_info_get_command_line_targets(!.Info),
        set.delete(top_target_file(ModuleName, module_target(TargetType)),
            Targets0, Targets),
        make_info_set_command_line_targets(Targets, !Info),
        build_target(ProgressStream, Globals, CompilationTaskAndOptions,
            TargetFile, TargetFileName, ModuleDepInfo, MakeLhsFiles,
            ExtraOptions, Succeeded, !Info, !IO)
    ;
        LhsResult = can_rebuild_lhs(all_lhs_files_up_to_date),
        TopTargetFile = top_target_file(ModuleName, module_target(TargetType)),
        maybe_warn_up_to_date_target_msg(Globals, TopTargetFile,
            TargetFileName, !Info, UpToDateMsg),
        maybe_write_msg(ProgressStream, UpToDateMsg, !IO),
        debug_make_msg(Globals,
            string.format("%s: up to date\n", [s(TargetFileName)]),
            UpToDateDebugMsg),
        maybe_write_msg(ProgressStream, UpToDateDebugMsg, !IO),
        Succeeded = succeeded,
        LhsTargetFiles = DatelessLhsTargetFiles ++ DatedLhsTargetFiles,
        list.foldl(update_target_status(target_status_up_to_date),
            [TargetFile | LhsTargetFiles], !Info)
    ).

%---------------------------------------------------------------------------%

:- pred build_target(io.text_output_stream::in, globals::in,
    compilation_task_type_and_options::in,
    target_file::in, file_name::in, module_dep_info::in, make_lhs_files::in,
    list(string)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_target(ProgressStream, Globals, CompilationTask,
        TargetFile, TargetFileName, ModuleDepInfo, MakeLhsFiles,
        ExtraOptions, Succeeded, !Info, !IO) :-
    maybe_making_filename_msg(Globals, TargetFileName, MakingMsg),
    maybe_write_msg(ProgressStream, MakingMsg, !IO),
    TargetFile = target_file(ModuleName, _TargetType),
    CompilationTask = task_and_options(Task, TaskOptions),
    ExtraAndTaskOptions = ExtraOptions ++ TaskOptions,
    ( if
        Task = process_module(ModuleTask),
        do_task_in_separate_process(ModuleTask) = yes,
        not can_fork
    then
        % If we want to perform a compilation task in a separate process,
        % but fork() is unavailable, then we will invoke the Mercury compiler
        % with a bunch of command line arguments. On Windows, the command line
        % is likely to exceed that maximum command line length, so we need to
        % pass the command line arguments via a temporary file, created here
        % (not in invoke_mmc) so it can be cleaned up by
        % teardown_checking_for_interrupt.
        io.file.make_temp_file(ArgFileNameResult, !IO),
        (
            ArgFileNameResult = ok(ArgFileName),
            MaybeArgFileName = yes(ArgFileName),
            ArgFileNameRes = ok : io.res
        ;
            ArgFileNameResult = error(Error),
            MaybeArgFileName = no,
            ArgFileNameRes = error(Error)
        )
    else
        MaybeArgFileName = no,
        ArgFileNameRes = ok
    ),

    (
        ArgFileNameRes = ok,
        get_real_milliseconds(StartTimeMs, !IO),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        Cleanup = cleanup_files(ProgressStream, Globals, MaybeArgFileName,
            MakeLhsFiles),
        setup_checking_for_interrupt(Cookie, !IO),
        get_default_options(Globals, DefaultOptionTable),
        MaybeStdLibGrades = make_info_get_maybe_stdlib_grades(!.Info),
        EnvOptFileVariables = make_info_get_env_optfile_variables(!.Info),
        EnvVarArgs = make_info_get_env_var_args(!.Info),
        OptionArgs = make_info_get_option_args(!.Info),
        setup_for_build_with_module_options(ProgressStream, DefaultOptionTable,
            MaybeStdLibGrades, invoked_by_mmc_make, ModuleName,
            EnvOptFileVariables, EnvVarArgs, OptionArgs,
            ExtraAndTaskOptions, MayBuild, !IO),
        (
            MayBuild = may_build(AllOptionArgs, BuildGlobals),
            open_module_error_stream(ProgressStream, Globals, !.Info,
                ModuleName, MaybeErrorStream, !IO),
            (
                MaybeErrorStream = es_error_already_reported,
                Succeeded0 = did_not_succeed
            ;
                MaybeErrorStream = es_ok(MESI, ErrorStream),
                build_target_2(ProgressStream, ErrorStream, BuildGlobals,
                    Task, ModuleName, ModuleDepInfo,
                    MaybeArgFileName, AllOptionArgs, Succeeded0, !IO),
                close_module_error_stream_handle_errors(ProgressStream,
                    Globals, MESI, ErrorStream, !.Info, !IO)
            )
        ;
            MayBuild = may_not_build(Specs),
            write_error_specs(ProgressStream, Globals, Specs, !IO),
            Succeeded0 = did_not_succeed
        ),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, Cleanup,
            Succeeded0, Succeeded, !Info, !IO),
        record_made_target_given_make_lhs_files(ProgressStream, Globals,
            Succeeded, TargetFile, TargetFileName, MakeLhsFiles, !Info, !IO),
        get_real_milliseconds(EndTimeMs, !IO),

        globals.lookup_bool_option(Globals, show_make_times, ShowMakeTimes),
        (
            ShowMakeTimes = yes,
            DiffSecs = float(EndTimeMs - StartTimeMs) / 1000.0,
            % Avoid cluttering the screen with short running times.
            ( if DiffSecs >= 0.5 then
                io.format(ProgressStream, "Making %s took %.2fs\n",
                    [s(TargetFileName), f(DiffSecs)], !IO)
            else
                true
            )
        ;
            ShowMakeTimes = no
        )
    ;
        ArgFileNameRes = error(ArgFileError),
        io.format(ProgressStream, "Could not create temporary file: %s\n",
            [s(error_message(ArgFileError))], !IO),
        Succeeded = did_not_succeed
    ).

:- pred build_target_2(io.text_output_stream::in, io.text_output_stream::in,
    globals::in, compilation_task_type::in, module_name::in,
    module_dep_info::in, maybe(file_name)::in, list(string)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

build_target_2(ProgressStream, ErrorStream, Globals, Task, ModuleName,
        ModuleDepInfo, ArgFileName, AllOptionArgs, Succeeded, !IO) :-
    (
        Task = process_module(ModuleTask),
        ModuleArg = sym_name_to_string(ModuleName),

        globals.lookup_bool_option(Globals, verbose_commands, Verbose),
        (
            Verbose = yes,
            AllArgs = AllOptionArgs ++ [ModuleArg],
            % XXX XXX Don't write the options whose values are the same
            % as the defaults.
            AllArgStrs = list.map(quote_shell_cmd_arg, AllArgs),
            AllArgsStr = string.join_list(" ", AllArgStrs),
            io.format(ProgressStream, "Invoking self `mmc %s'\n",
                [s(AllArgsStr)], !IO)
        ;
            Verbose = no
        ),

        % Run some tasks in a separate process instead of within the mmc --make
        % process. This avoids problems with the Boehm GC retaining memory by
        % scanning too much of the Mercury stacks. If the compilation is run
        % in a separate process, it is also easier to kill if an interrupt
        % arrives.
        % XXX The above comment is likely to be quite out-of-date.
        CallInSeparateProcess = do_task_in_separate_process(ModuleTask),
        (
            CallInSeparateProcess = yes,
            call_in_forked_process_with_backup(
                call_mercury_compile_main(ProgressStream, ErrorStream, Globals,
                    [ModuleArg]),
                invoke_mmc(Globals, ProgressStream, ErrorStream,
                    ArgFileName, AllOptionArgs ++ [ModuleArg]),
                CompileSucceeded, !IO)
        ;
            CallInSeparateProcess = no,
            call_mercury_compile_main(ProgressStream, ErrorStream, Globals,
                [ModuleArg], CompileSucceeded, !IO)
        ),

        ( if
            ( ModuleTask = task_compile_to_c
            ; ModuleTask = task_compile_to_java
            ; ModuleTask = task_compile_to_csharp
            ; ModuleTask = task_errorcheck
            )
        then
            % The `.err_date' file is needed because the `.err' file is touched
            % by all phases of compilation, including writing interfaces.
            touch_module_ext_datestamp(Globals, ProgressStream,
                ModuleName, ext_cur_ngs_gs(ext_cur_ngs_gs_misc_err_date),
                TouchSucceeded, !IO),
            Succeeded = CompileSucceeded `and` TouchSucceeded
        else
            Succeeded = CompileSucceeded
        )
    ;
        Task = target_code_to_object_code(PIC),
        globals.get_target(Globals, CompilationTarget),
        % Run the compilation in a child process, so it can be killed
        % if an interrupt arrives.
        call_in_forked_process(
            build_object_code(ProgressStream, ErrorStream, Globals,
                CompilationTarget, PIC, ModuleName, ModuleDepInfo),
            Succeeded, !IO)
    ;
        Task = fact_table_code_to_object_code(PIC, FactTableFileName),
        get_object_extension(Globals, PIC, ObjExt),
        get_fact_table_foreign_code_file(Globals, do_create_dirs,
            ext_cur_ngs_gas(ObjExt),
            FactTableFileName, FactTableForeignCode, !IO),
        % Run the compilation in a child process, so it can be killed
        % if an interrupt arrives.
        call_in_forked_process(
            compile_foreign_code_file(Globals, ProgressStream, PIC,
                FactTableForeignCode),
            Succeeded, !IO)
    ).

:- pred build_object_code(io.text_output_stream::in, io.text_output_stream::in,
    globals::in, compilation_target::in, pic::in, module_name::in,
    module_dep_info::in, maybe_succeeded::out, io::di, io::uo) is det.

build_object_code(ProgressStream, ErrorStream, Globals, Target, PIC,
        ModuleName, _ModuleDepInfo, Succeeded, !IO) :-
    (
        Target = target_c,
        compile_c_file(Globals, ProgressStream, PIC, ModuleName,
            Succeeded, !IO)
    ;
        Target = target_java,
        % XXX LEGACY
        module_name_to_file_name_create_dirs(Globals, $pred,
            ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java),
            ModuleName, JavaFile, _JavaFileProposed, !IO),
        compile_java_files(Globals, ProgressStream, JavaFile, [],
            Succeeded, !IO)
    ;
        Target = target_csharp,
        % XXX This code uses infrastructure we built for *linking several
        % files* as a way to *compile just one file*. It would be nice
        % to know the reasoning behind that decision.
        %
        % The reason for this is that C# has no direct analog of object
        % or class files, so we have to fake the effect of building an
        % object file by creating a .dll - juliensf.
        %
        % XXX LEGACY
        module_name_to_file_name_create_dirs(Globals, $pred,
            ext_cur_ngs_gs(ext_cur_ngs_gs_target_cs),
            ModuleName, CsharpFile, _CsharpFileProposed, !IO),
        link_files_into_executable_or_library_for_c_cs_java(ProgressStream,
            Globals, csharp_library, ModuleName, [CsharpFile],
            Specs, Succeeded, !IO),
        % XXX MAKE This predicate, build_object_code, is invoked only as the
        % top call in a newly-spawned-off process. We cannot return Specs
        % to our caller, because our caller is in a separate process.
        write_error_specs(ErrorStream, Globals, Specs, !IO)
    ).

:- pred compile_foreign_code_file(globals::in, io.text_output_stream::in,
    pic::in, foreign_code_file::in, maybe_succeeded::out,
    io::di, io::uo) is det.

compile_foreign_code_file(Globals, ProgressStream, PIC, ForeignCodeFile,
        Succeeded, !IO) :-
    (
        ForeignCodeFile = foreign_code_file(lang_c, CFile, ObjFile),
        do_compile_c_file(Globals, ProgressStream, PIC,
            CFile, ObjFile, Succeeded, !IO)
    ;
        ForeignCodeFile = foreign_code_file(lang_java, _, _),
        unexpected($pred, "compiling Java foreign code file not supported")
    ;
        ForeignCodeFile = foreign_code_file(lang_csharp, _, _),
        unexpected($pred, "compiling C# foreign code file not supported")
    ).

:- func do_task_in_separate_process(module_compilation_task_type) = bool.

do_task_in_separate_process(task_errorcheck) = no.
do_task_in_separate_process(task_make_int0) = no.
do_task_in_separate_process(task_make_int12) = no.
do_task_in_separate_process(task_make_int3) = no.
do_task_in_separate_process(task_make_opt) = yes.
do_task_in_separate_process(task_make_analysis_registry) = yes.
do_task_in_separate_process(task_compile_to_c) = yes.
do_task_in_separate_process(task_compile_to_java) = yes.
do_task_in_separate_process(task_compile_to_csharp) = yes.
do_task_in_separate_process(task_make_xml_doc) = yes.

%---------------------------------------------------------------------------%

:- pred cleanup_files(io.text_output_stream::in, globals::in,
    maybe(string)::in, make_lhs_files::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

cleanup_files(ProgressStream, Globals, MaybeArgFileName, MakeLhsFiles,
        !MakeInfo, !IO) :-
    MakeLhsFiles = make_lhs_files(DatelessLhsTargetFiles, DatedLhsTargetFiles,
        LhsDateFileNames, LhsForeignCodeFileNames),
    % XXX Remove `.int.tmp' files.
    list.foldl2(
        remove_make_target_file(ProgressStream, Globals, $pred,
            very_verbose),
        DatelessLhsTargetFiles, !MakeInfo, !IO),
    list.foldl2(
        remove_make_target_file(ProgressStream, Globals, $pred,
            very_verbose),
        DatedLhsTargetFiles, !MakeInfo, !IO),
    list.foldl2(remove_file_for_make(ProgressStream, Globals, very_verbose),
        LhsDateFileNames, !MakeInfo, !IO),
    list.foldl2(remove_file_for_make(ProgressStream, Globals, very_verbose),
        LhsForeignCodeFileNames, !MakeInfo, !IO),
    (
        MaybeArgFileName = yes(ArgFileName),
        io.file.remove_file(ArgFileName, _, !IO)
    ;
        MaybeArgFileName = no
    ).

%---------------------------------------------------------------------------%

:- pred get_object_extension(globals::in, pic::in, ext_cur_ngs_gas::out)
    is det.

get_object_extension(Globals, PIC, ExtObj) :-
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        maybe_pic_object_file_extension(PIC, ExtObj, _)
    ;
        CompilationTarget = target_csharp,
        sorry($pred, "object extension for csharp")
    ;
        CompilationTarget = target_java,
        sorry($pred, "object extension for java")
    ).

%---------------------------------------------------------------------------%

:- pred call_mercury_compile_main(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, list(string)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

call_mercury_compile_main(ProgressStream, ErrorStream, Globals, Args,
        Succeeded, !IO) :-
    io.get_exit_status(Status0, !IO),
    io.set_exit_status(0, !IO),
    mercury_compile_main.main_for_make(ProgressStream, ErrorStream, Globals,
        Args, !IO),
    io.get_exit_status(Status, !IO),
    Succeeded = ( if Status = 0 then succeeded else did_not_succeed ),
    io.set_exit_status(Status0, !IO).

:- pred invoke_mmc(globals::in,
    io.text_output_stream::in, io.text_output_stream::in,
    maybe(file_name)::in, list(string)::in, maybe_succeeded::out,
    io::di, io::uo) is det.

invoke_mmc(Globals, ProgressStream, ErrorStream, MaybeArgFileName, Args,
        Succeeded, !IO) :-
    io.progname("", ProgName, !IO),
    ( if
        % NOTE: If the compiler is built in the Java grade, then ProgName will
        % be set to "top_level" (which was the name compiled into the module
        % containing the predicate main/2). We don't want to attempt to
        % invoke an executable named "top_level" however, since the wrapper
        % script will have been renamed to "mercury_compile" by the Mmakefile
        % in the compiler directory.
        ( ProgName = ""
        ; target_is_java
        )
    then
        io.environment.get_environment_var("MERCURY_COMPILER",
            MaybeMercuryCompiler, !IO),
        (
            MaybeMercuryCompiler = yes(MercuryCompiler)
        ;
            MaybeMercuryCompiler = no,
            MercuryCompiler = "mmc"
        )
    else
        MercuryCompiler = ProgName
    ),

    QuotedArgs = list.map(quote_shell_cmd_arg, Args),

    % Some operating systems (e.g. Windows) have shells with ludicrously
    % short limits on the length of command lines, so we need to write the
    % arguments to a file which will be read by the child mmc process.
    % This code is only called if fork() doesn't work, so there is no point
    % checking whether the shell actually has this limitation.
    % The temporary file is created by the caller so that it will be removed
    % by teardown_checking_for_interrupt if an interrupt occurs.
    (
        MaybeArgFileName = yes(ArgFileName)
    ;
        MaybeArgFileName = no,
        unexpected($pred, "argument file not created")
    ),

    io.open_output(ArgFileName, ArgFileOpenRes, !IO),
    (
        ArgFileOpenRes = ok(ArgFileStream),
        io.format(ArgFileStream, "MCFLAGS = %s\n",
            [s(string.join_list(" ", QuotedArgs))], !IO),
        io.close_output(ArgFileStream, !IO),

        Command = string.format("%s --arg-file %s", [
            s(quote_shell_cmd_arg(MercuryCompiler)),
            s(quote_shell_cmd_arg(ArgFileName))]),

        % We have already written the command.
        CommandVerbosity = cmd_verbose,
        invoke_system_command(Globals, ProgressStream, ErrorStream,
            CommandVerbosity, Command, Succeeded, !IO)
    ;
        ArgFileOpenRes = error(Error),
        Succeeded = did_not_succeed,
        io.error_message(Error, ErrorMsg),
        io.format(ProgressStream, "Error opening `%s' for output: %s\n",
            [s(ArgFileName), s(ErrorMsg)], !IO)
    ),
    io.file.remove_file(ArgFileName, _, !IO).

:- pred target_is_java is semidet.

:- pragma foreign_proc("Java",
    target_is_java,
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

target_is_java :-
    semidet_fail.

%---------------------------------------------------------------------------%

record_made_target(ProgressStream, Globals, TargetFile, TargetFileName,
        CompilationTask, Succeeded, !Info, !IO) :-
    find_lhs_files_of_task(ProgressStream, Globals, TargetFile,
        CompilationTask, MakeLhsFiles, !Info, !IO),
    record_made_target_given_make_lhs_files(ProgressStream, Globals,
        Succeeded, TargetFile, TargetFileName, MakeLhsFiles, !Info, !IO).

:- pred record_made_target_given_make_lhs_files(io.text_output_stream::in,
    globals::in, maybe_succeeded::in, target_file::in, string::in,
    make_lhs_files::in, make_info::in, make_info::out, io::di, io::uo) is det.

record_made_target_given_make_lhs_files(ProgressStream, Globals,
        Succeeded, TargetFile, TargetFileName, MakeLhsFiles, !Info, !IO) :-
    (
        Succeeded = succeeded,
        TargetStatus = target_status_up_to_date
    ;
        Succeeded = did_not_succeed,
        TargetStatus = target_status_error,
        file_error_msg(TargetFileName, ErrorMsg),
        maybe_write_msg_locked(ProgressStream, !.Info, ErrorMsg, !IO)
    ),

    MakeLhsFiles = make_lhs_files(DatelessLhsTargetFiles, DatedLhsTargetFiles,
        LhsDateFiles, LhsForeignCodeFileNames),
    list.foldl(update_target_status(TargetStatus),
        DatelessLhsTargetFiles, !Info),
    list.foldl(update_target_status(TargetStatus),
        DatedLhsTargetFiles, !Info),

    % XXX LEGACY
    list.map2_foldl2(
        module_maybe_nested_target_file_to_file_name(ProgressStream, Globals,
            $pred),
        DatelessLhsTargetFiles,
        DatelessLhsFileNames, _DatelessLhsFileNamesProposed, !Info, !IO),
    list.map2_foldl2(
        module_maybe_nested_target_file_to_file_name(ProgressStream, Globals,
            $pred),
        DatedLhsTargetFiles,
        DatedLhsFileNames, _DatedLhsFileNamesProposed, !Info, !IO),

    some [!FileTimestampMap] (
        !:FileTimestampMap = make_info_get_file_timestamp_map(!.Info),
        list.foldl(delete_timestamp(ProgressStream, Globals),
            DatelessLhsFileNames, !FileTimestampMap),
        list.foldl(delete_timestamp(ProgressStream, Globals),
            DatedLhsFileNames, !FileTimestampMap),
        list.foldl(delete_timestamp(ProgressStream, Globals),
            LhsDateFiles, !FileTimestampMap),
        list.foldl(delete_timestamp(ProgressStream, Globals),
            LhsForeignCodeFileNames, !FileTimestampMap),

        % When an .analysis file is made, that potentially invalidates other
        % .analysis files so we have to delete their timestamps. The exact list
        % of files which might be affected can be found by reading the
        % corresponding .imdg file. But it is simpler to just delete the
        % timestamps of all the .analysis files that we know about.
        ( if TargetFile = target_file(_, module_target_analysis_registry) then
            map.foldl(
                delete_analysis_registry_timestamps(ProgressStream, Globals),
                !.FileTimestampMap, !FileTimestampMap)
        else
            true
        ),
        make_info_set_file_timestamp_map(!.FileTimestampMap, !Info)
    ),

    TargetFileTimestampMap0 = make_info_get_target_file_timestamp_map(!.Info),
    list.foldl(version_hash_table.delete, DatelessLhsTargetFiles,
        TargetFileTimestampMap0, TargetFileTimestampMap1),
    list.foldl(version_hash_table.delete, DatedLhsTargetFiles,
        TargetFileTimestampMap1, TargetFileTimestampMap),
    make_info_set_target_file_timestamp_map(TargetFileTimestampMap, !Info).

:- pred update_target_status(target_status::in, target_file::in,
    make_info::in, make_info::out) is det.

update_target_status(TargetStatus, TargetFile, !Info) :-
    TargetId = merc_target(TargetFile),
    TargetStatusMap0 = make_info_get_target_status_map(!.Info),
    version_hash_table.set(TargetId, TargetStatus,
        TargetStatusMap0, TargetStatusMap),
    make_info_set_target_status_map(TargetStatusMap, !Info).

:- pred delete_analysis_registry_timestamps(io.text_output_stream::in,
    globals::in, string::in, {list(dir_name), maybe_error(timestamp)}::in,
    file_timestamp_map::in, file_timestamp_map::out) is det.

delete_analysis_registry_timestamps(ProgressStream, Globals, FileName, _,
        !TimestampMap) :-
    ( if string.suffix(FileName, ".analysis") then
        delete_timestamp(ProgressStream, Globals, FileName, !TimestampMap)
    else
        true
    ).

:- pred delete_timestamp(io.text_output_stream::in, globals::in, string::in,
    file_timestamp_map::in, file_timestamp_map::out) is det.

delete_timestamp(ProgressStream, Globals, TouchedFile, !TimestampMap) :-
    trace [io(!IO)] (
        debug_make_msg(Globals,
            string.format("Deleting timestamp for %s\n", [s(TouchedFile)]),
            DebugMsg),
        maybe_write_msg(ProgressStream, DebugMsg, !IO)
    ),
    map.delete(TouchedFile, !TimestampMap).

%---------------------------------------------------------------------------%

:- type compilation_task_type_and_options
    --->    task_and_options(
                compilation_task_type,
                list(string)
            ).

:- pred get_compilation_task_and_options(module_target_type::in,
    compilation_task_type_and_options::out) is det.

get_compilation_task_and_options(Target, Result) :-
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
        Result = task_and_options(process_module(task_compile_to_c),
            ["--compile-to-c"])
    ;
        Target = module_target_csharp_code,
        Result = task_and_options(process_module(task_compile_to_csharp),
            ["--csharp-only"])
    ;
        Target = module_target_java_code,
        Result = task_and_options(process_module(task_compile_to_java),
            ["--java-only"])
    ;
        Target = module_target_java_class_code,
        Result = task_and_options(target_code_to_object_code(non_pic), [])
    ;
        Target = module_target_object_code(PIC),
        Result = task_and_options(target_code_to_object_code(PIC), [])
    ;
        Target = module_target_fact_table_object(PIC, FactTable),
        Result = task_and_options(
            fact_table_code_to_object_code(PIC, FactTable), [])
    ;
        Target = module_target_xml_doc,
        Result = task_and_options(process_module(task_make_xml_doc),
            ["--make-xml-doc"])
    ).

%---------------------------------------------------------------------------%

    % Find the files which could be touched by a compilation task.
    %
:- pred find_lhs_files_of_task(io.text_output_stream::in,
    globals::in, target_file::in, compilation_task_type::in,
    make_lhs_files::out, make_info::in, make_info::out, io::di, io::uo) is det.

find_lhs_files_of_task(ProgressStream, Globals, TargetFile, Task, MakeLhsFiles,
        !Info, !IO) :-
    % NOTE Our ancestor computes Task from the target_type component
    % of the TargetFile argument, which means that the only valid
    % target type/compilation task pairs are those that occur in the
    % predicate that does that computation, get_compilation_task_and_options.
    (
        Task = process_module(ModuleTask),
        find_lhs_files_of_process_module(ProgressStream, Globals,
            TargetFile, ModuleTask, MakeLhsFiles, !Info, !IO)
    ;
        Task = target_code_to_object_code(_),
        MakeLhsFiles = make_lhs_files([TargetFile], [], [], [])
    ;
        Task = fact_table_code_to_object_code(PIC, FactTableName),
        get_object_extension(Globals, PIC, ObjExt),
        % XXX LEGACY
        fact_table_file_name_return_dirs(Globals, $pred,
            ext_cur_ngs_gas(ObjExt), FactTableName,
            FactTableDirs, _FactTableDirsProposed,
            FactTableObjectFileName, _FactTableObjectFileNameProposed),
        create_any_dirs_on_path(FactTableDirs, !IO),
        % Fact table object files (.o or .pic_o) don't have date files.
        % XXX MAKE double inclusion in first and last fields
        MakeLhsFiles = make_lhs_files([TargetFile], [], [],
            [FactTableObjectFileName])
    ).

:- pred find_lhs_files_of_process_module(io.text_output_stream::in,
    globals::in, target_file::in, module_compilation_task_type::in,
    make_lhs_files::out, make_info::in, make_info::out, io::di, io::uo) is det.

find_lhs_files_of_process_module(ProgressStream, Globals, TargetFile, Task,
        MakeLhsFiles, !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        % This error should have been caught earlier. We should not be
        % attempting to build a target if we couldn't find the dependencies
        % for the module.
        unexpected($pred, "no module dependencies")
    ),

    module_dep_info_get_maybe_top_module(ModuleDepInfo, MaybeTopModule),
    NestedSubModules = get_nested_children_list_of_top_module(MaybeTopModule),
    SourceFileModuleNames = [ModuleName | NestedSubModules],

    list.map_foldl2(get_maybe_module_dep_info(ProgressStream, Globals),
        NestedSubModules, MaybeNestedModuleDepInfos, !Info, !IO),
    ( if
        list.map((pred(some_module_dep_info(MDI)::in, MDI::out) is semidet),
            MaybeNestedModuleDepInfos, NestedModuleDepInfos)
    then
        ModuleDepInfos = [ModuleDepInfo | NestedModuleDepInfos]
    else
        % This error should have been caught earlier. We should not be
        % attempting to build a target if we couldn't find the dependencies
        % for the module or its nested sub-modules.
        unexpected($pred, "no nested module dependencies")
    ),

    (
        Task = task_compile_to_c,
        DirectLhsTargetFiles =
            make_target_file_list(SourceFileModuleNames, TargetType),
        % Find out what header files are generated.
        TargetPIC = target_type_to_pic(TargetType),
        list.map_foldl(
            get_any_fact_table_object_code_files(Globals, TargetPIC),
            ModuleDepInfos, ForeignCodeFiles, !IO),
        LhsForeignCodeFileNames =
            list.map((func(ForeignFile) = ForeignFile ^ fcf_target_file),
                list.condense(ForeignCodeFiles)),
        MhTargetFiles = make_target_file_list(SourceFileModuleNames,
            module_target_c_header(header_mh)),
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        (
            HighLevelCode = yes,
            % When compiling to high-level C, we always generate both
            % a .mih file and a .mh file.
            MihTargetFiles = make_target_file_list(SourceFileModuleNames,
                module_target_c_header(header_mih)),
            LhsTargetFiles = DirectLhsTargetFiles ++ MhTargetFiles ++
                MihTargetFiles
        ;
            HighLevelCode = no,
            % When compiling to high-level C, we generate only a .mh file.
            LhsTargetFiles = DirectLhsTargetFiles ++ MhTargetFiles
        )
    ;
        ( Task = task_compile_to_java
        ; Task = task_compile_to_csharp
        ),
        % We do not generate any kind of header file for either Java or C#.
        LhsTargetFiles =
            make_target_file_list(SourceFileModuleNames, TargetType),
        % We support fact tables only when targeting C, not when targeting
        % Java or C#.
        LhsForeignCodeFileNames = []
    ;
        Task = task_make_int0,
        % LhsTargetFiles must only include modules with children,
        % as we no longer write out .int0 files for modules without children.
        list.filter_map(is_ancestor_module, ModuleDepInfos, AncestorModules),
        LhsTargetFiles = make_target_file_list(AncestorModules, TargetType),
        LhsForeignCodeFileNames = []
    ;
        Task = task_make_int12,
        % "mmc --make-interface" generates both .int and .int2 files.
        LhsTargetFiles =
            make_target_file_list(SourceFileModuleNames, module_target_int1) ++
            make_target_file_list(SourceFileModuleNames, module_target_int2),
        LhsForeignCodeFileNames = []
    ;
        ( Task = task_errorcheck
        ; Task = task_make_int3
        ; Task = task_make_opt
        ; Task = task_make_analysis_registry
        ; Task = task_make_xml_doc
        ),
        LhsTargetFiles =
            make_target_file_list(SourceFileModuleNames, TargetType),
        LhsForeignCodeFileNames = []
    ),
    split_dateless_dated_target_files(Globals, LhsTargetFiles,
        DatelessLhsTargetFiles, DatedLhsTargetFiles, LhsDateFileNames),
    MakeLhsFiles = make_lhs_files(DatelessLhsTargetFiles, DatedLhsTargetFiles,
        LhsDateFileNames, LhsForeignCodeFileNames).

:- pred is_ancestor_module(module_dep_info::in, module_name::out) is semidet.

is_ancestor_module(ModuleDepInfo, ModuleName) :-
    module_dep_info_get_children(ModuleDepInfo, Children),
    not set.is_empty(Children),
    module_dep_info_get_module_name(ModuleDepInfo, ModuleName).

:- pred split_dateless_dated_target_files(globals::in, list(target_file)::in,
    list(target_file)::out, list(target_file)::out, list(file_name)::out)
    is det.

split_dateless_dated_target_files(_Globals, [], [], [], []).
split_dateless_dated_target_files(Globals,
        [LhsTargetFile | LhsTargetFiles],
        !:DatelessLhsTargetFiles, !:DatedLhsTargetFiles, !:LhsDateFileNames) :-
    % The list of target files our caller calls us with
    % typically contains one or two entries for each (sub)module
    % contained in a source file. This means that almost all lists
    % will be very short, so using accumulators to make this predicate
    % tail recursive would have bigger costs than benefits.
    split_dateless_dated_target_files(Globals, LhsTargetFiles,
        !:DatelessLhsTargetFiles, !:DatedLhsTargetFiles, !:LhsDateFileNames),
    LhsTargetFile = target_file(ModuleName, TargetType),
    ( if date_file_extension(TargetType, DateFileExt) then
        !:DatedLhsTargetFiles = [LhsTargetFile | !.DatedLhsTargetFiles],
        % XXX LEGACY
        module_name_to_file_name(Globals, $pred, DateFileExt,
            ModuleName, LhsDateFileName, _LhsDateFileNamePropoposed),
        !:LhsDateFileNames = [LhsDateFileName | !.LhsDateFileNames]
    else
        !:DatelessLhsTargetFiles = [LhsTargetFile | !.DatelessLhsTargetFiles]
    ).

%---------------------------------------------------------------------------%

get_any_fact_table_object_code_files(Globals, PIC, ModuleDepInfo,
        ForeignFiles, !IO) :-
    % Find externally compiled foreign code files for
    % `:- pragma foreign_proc' declarations.
    %
    % Any changes here may require corresponding changes in
    % get_foreign_object_targets.
    globals.get_target(Globals, CompilationTarget),

    % None of the current backends require externally compiled foreign code,
    % except the C backend for fact tables.
    (
        CompilationTarget = target_c,
        maybe_pic_object_file_extension(PIC, ObjExt, _),
        module_dep_info_get_fact_tables(ModuleDepInfo, FactTableFiles),
        list.map_foldl(
            get_fact_table_foreign_code_file(Globals, do_not_create_dirs,
                ext_cur_ngs_gas(ObjExt)),
            set.to_sorted_list(FactTableFiles), FactTableForeignFiles, !IO),
        ForeignFiles = FactTableForeignFiles
    ;
        ( CompilationTarget = target_java
        ; CompilationTarget = target_csharp
        ),
        ForeignFiles = []
    ).

:- pred get_fact_table_foreign_code_file(globals::in, maybe_create_dirs::in,
    ext::in, file_name::in, foreign_code_file::out, io::di, io::uo) is det.

get_fact_table_foreign_code_file(Globals, Mkdir, ObjExt,
        FactTableFileName, ForeignCodeFile, !IO) :-
    % XXX EXT Neither of these calls should be needed.
    % XXX LEGACY
    fact_table_file_name_return_dirs(Globals, $pred,
        ext_cur_ngs_gs(ext_cur_ngs_gs_target_c), FactTableFileName,
        FactTableDirsC, _FactTableDirsCProposed,
        FactTableCFileName, _FactTableCFileNameProposed),
    maybe_create_any_dirs_on_path(Mkdir, FactTableDirsC, !IO),
    % XXX LEGACY
    fact_table_file_name_return_dirs(Globals, $pred, ObjExt, FactTableFileName,
        FactTableDirsO, _FactTableDirsOProposed,
        FactTableObjFileName, _FactTableObjFileNameProposed),
    maybe_create_any_dirs_on_path(Mkdir, FactTableDirsO, !IO),
    ForeignCodeFile =
        foreign_code_file(lang_c, FactTableCFileName, FactTableObjFileName).

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
        ; TargetType = module_target_fact_table_object(_, _)
        ; TargetType = module_target_xml_doc
        ),
        Result = non_pic
    ).

%---------------------------------------------------------------------------%
:- end_module make.module_target.
%---------------------------------------------------------------------------%
