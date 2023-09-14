%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2022 The Mercury team.
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
:- import_module make.dependencies.
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
    globals::in, dependency_file::in, maybe_succeeded::out,
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
:- pred external_foreign_code_files(globals::in, pic::in, module_dep_info::in,
    list(foreign_code_file)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module libs.options.
:- import_module libs.process_util.
:- import_module libs.shell_util.
:- import_module libs.timestamp.
:- import_module make.build.
:- import_module make.deps_set.
:- import_module make.file_names.
:- import_module make.module_dep_file.
:- import_module make.timestamp.
:- import_module make.util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.write_error_spec.
:- import_module top_level.                      % XXX unwanted dependency
:- import_module top_level.mercury_compile_main. % XXX unwanted dependency
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module dir.
:- import_module float.
:- import_module int.
:- import_module io.environment.
:- import_module io.file.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module sparse_bitset.
:- import_module string.
:- import_module version_hash_table.

%---------------------------------------------------------------------------%

make_module_target(ExtraOptions, ProgressStream, Globals, Dep, Succeeded,
        !Info, !IO) :-
    get_dependency_status(ProgressStream, Globals,
        Dep, StatusResult, !Info, !IO),
    StatusResult = dependency_status_result(_Dep, _DepFileName, Status),
    (
        Status = deps_status_error,
        Succeeded = did_not_succeed
    ;
        Status = deps_status_up_to_date,
        Succeeded = succeeded
    ;
        Status = deps_status_not_considered,
        (
            Dep = dep_file(_),
            Succeeded = succeeded
        ;
            Dep = dep_target(TargetFile),
            TargetFile = target_file(ModuleName, TargetType),
            get_maybe_module_dep_info(ProgressStream, Globals,
                ModuleName, MaybeModuleDepInfo, !Info, !IO),
            (
                MaybeModuleDepInfo = no_module_dep_info,
                Succeeded = did_not_succeed,
                DepStatusMap0 = make_info_get_dependency_status(!.Info),
                version_hash_table.set(Dep, deps_status_error,
                    DepStatusMap0, DepStatusMap),
                make_info_set_dependency_status(DepStatusMap, !Info)
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
                    NestedTargetFile =
                        target_file(SourceFileModuleName, TargetType),
                    make_module_target(ExtraOptions, ProgressStream, Globals,
                        dep_target(NestedTargetFile), Succeeded, !Info, !IO)
                else
                    make_module_target_file_main_path(ExtraOptions,
                        ProgressStream, Globals, TargetFile,
                        CompilationTaskAndOptions, ModuleDepInfo,
                        Succeeded, !Info, !IO)
                )
            )
        )
    ;
        Status = deps_status_being_built,
        (
            Dep = dep_file(_),
            Succeeded = succeeded
        ;
            Dep = dep_target(_),
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
    TargetFile = target_file(ModuleName, TargetType),
    get_make_target_file_name(Globals, $pred, TargetFile, TargetFileName, !IO),
    CompilationTaskAndOptions = task_and_options(CompilationTaskType, _),
    find_files_maybe_touched_by_task(ProgressStream, Globals, TargetFile,
        CompilationTaskType, TouchedTargetFiles, TouchedFiles, !Info, !IO),
    list.foldl(update_target_status(deps_status_being_built),
        TouchedTargetFiles, !Info),

    debug_make_msg(Globals,
        string.format("%s: checking dependencies\n", [s(TargetFileName)]),
        CheckingMsg),
    maybe_write_msg(ProgressStream, CheckingMsg, !IO),

    ( if CompilationTaskType = process_module(_) then
        module_dep_info_get_maybe_top_module(ModuleDepInfo, MaybeTopModule),
        NestedSubModules =
            get_nested_children_list_of_top_module(MaybeTopModule),
        ModulesToCheck = [ModuleName | NestedSubModules]
    else
        ModulesToCheck = [ModuleName]
    ),
    module_names_to_index_set(ModulesToCheck, ModuleIndexesToCheckSet, !Info),
    ModuleIndexesToCheck = to_sorted_list(ModuleIndexesToCheckSet),

    KeepGoing = make_info_get_keep_going(!.Info),
    find_target_dependencies_of_modules(KeepGoing, Globals, TargetType,
        ModuleIndexesToCheck, succeeded, DepsSucceeded,
        sparse_bitset.init, DepFiles0, !Info, !IO),
    % NOTE: converting the dep_set to a plain set is relatively expensive,
    % so it would be better to avoid it. Also, there should be a definite
    % improvement if we could represent the dependency_status map with an
    % array indexed by dependency_file_indexes, instead of a hash table
    % indexed by dependency_file terms.
    dependency_file_index_set_to_plain_set(!.Info, DepFiles0, DepFilesSet0),
    ( if TargetType = module_target_int0 then
        % Avoid circular dependencies (the `.int0' files for the
        % nested sub-modules depend on this module's `.int0' file).
        PrivateInts = make_dependency_list(ModulesToCheck, module_target_int0),
        set.delete_list(PrivateInts, DepFilesSet0, DepFilesSet)
    else
        DepFilesSet = DepFilesSet0
    ),
    DepFilesToMake = set.to_sorted_list(DepFilesSet),

    globals.lookup_bool_option(Globals, debug_make, DebugMake),
    (
        DebugMake = no
    ;
        DebugMake = yes,
        dependency_file_index_set_to_plain_set(!.Info,
            DepFiles0, DepFilesPlainSet),
        list.map_foldl(dependency_file_to_file_name(Globals),
            set.to_sorted_list(DepFilesPlainSet), DepFileNames, !IO),
        io.format(ProgressStream, "%s: dependencies:\n",
            [s(TargetFileName)], !IO),
        WriteDepFileName =
            ( pred(FN::in, SIO0::di, SIO::uo) is det :-
                io.format(ProgressStream, "\t%s\n", [s(FN)], SIO0, SIO)
            ),
        list.foldl(WriteDepFileName, DepFileNames, !IO)
    ),

    ( if
        DepsSucceeded = did_not_succeed,
        KeepGoing = do_not_keep_going
    then
        DepsResult = deps_error
    else
        make_dependency_files(ProgressStream, Globals, TargetFile,
            TargetFileName, DepFilesToMake, TouchedTargetFiles, TouchedFiles,
            DepsResult0, !Info, !IO),
        (
            DepsSucceeded = succeeded,
            DepsResult = DepsResult0
        ;
            DepsSucceeded = did_not_succeed,
            DepsResult = deps_error
        )
    ),
    (
        DepsResult = deps_error,
        Succeeded = did_not_succeed,
        list.foldl(update_target_status(deps_status_error),
            TouchedTargetFiles, !Info)
    ;
        DepsResult = deps_out_of_date,
        Targets0 = make_info_get_command_line_targets(!.Info),
        set.delete(top_target_file(ModuleName, module_target(TargetType)),
            Targets0, Targets),
        make_info_set_command_line_targets(Targets, !Info),
        build_target(ProgressStream, Globals, CompilationTaskAndOptions,
            TargetFile, ModuleDepInfo, TouchedTargetFiles, TouchedFiles,
            ExtraOptions, Succeeded, !Info, !IO)
    ;
        DepsResult = deps_up_to_date,
        TopTargetFile = top_target_file(ModuleName, module_target(TargetType)),
        maybe_warn_up_to_date_target_msg(Globals, TopTargetFile,
            TargetFileName, !Info, UpToDateMsg),
        maybe_write_msg(ProgressStream, UpToDateMsg, !IO),
        debug_make_msg(Globals,
            string.format("%s: up to date\n", [s(TargetFileName)]),
            DebugMsg),
        maybe_write_msg(ProgressStream, DebugMsg, !IO),
        Succeeded = succeeded,
        list.foldl(update_target_status(deps_status_up_to_date),
            [TargetFile | TouchedTargetFiles], !Info)
    ).

:- pred make_dependency_files(io.text_output_stream::in, globals::in,
    target_file::in, file_name::in, list(dependency_file)::in,
    list(target_file)::in, list(file_name)::in, dependencies_result::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_dependency_files(ProgressStream, Globals, TargetFile, TargetFileName,
        DepFilesToMake, TouchedTargetFiles, TouchedFiles, DepsResult,
        !Info, !IO) :-
    % Build the dependencies.
    KeepGoing = make_info_get_keep_going(!.Info),
    foldl2_make_module_targets(KeepGoing, [], ProgressStream, Globals,
        DepFilesToMake, MakeDepsSucceeded, !Info, !IO),

    % Check that the target files exist.
    list.map_foldl2(
        get_target_timestamp(ProgressStream, Globals, do_not_search),
        TouchedTargetFiles, TargetTimestamps, !Info, !IO),
    (
        MakeDepsSucceeded = did_not_succeed,
        debug_make_msg(Globals,
            string.format("%s: error making dependencies\n",
                [s(TargetFileName)]),
            DebugMsg),
        maybe_write_msg(ProgressStream, DebugMsg, !IO),
        DepsResult = deps_error
    ;
        MakeDepsSucceeded = succeeded,
        ( if list.member(error(_), TargetTimestamps) then
            debug_make_msg(Globals,
                string.format("%s: target file does not exist\n",
                    [s(TargetFileName)]),
                DebugMsg),
            maybe_write_msg(ProgressStream, DebugMsg, !IO),
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
                    TouchedFiles, TouchedFileTimestamps,
                    !Info, !IO),
                find_error_or_oldest_ok_timestamp(
                    TouchedTargetFileTimestamps ++ TouchedFileTimestamps,
                    MaybeOldestTimestamp),

                % Our caller computes TargetFileName using the call
                %
                %   get_make_target_file_name(Globals, $pred, TargetFile,
                %       TargetFileName, !IO)
                %
                % get_file_name(... do_not_search, ...) and
                % get_make_target_file_name both just call
                % module_name_to_file_name for *most*, but not *all*
                % target file types. For the other three target types, namely
                %
                %   module_target_source
                %   module_target_foreign_object
                %   module_target_fact_table_object
                %
                % one of the following three must hold:
                %
                % - the different code paths in the above two predicates
                %   may be equivalent, in which case one of those predicates
                %   is redundant;
                %
                % - this code point cannot be reached with these target file
                %   types, a proposition for which I (zs) see no evidence, or
                %
                % - the call to get_file_name here, instead of
                %   get_make_target_file_name, is, and always has been,
                %   a BUG.
                %
                % The fact that a hlc.gc bootcheck does not cause the
                % call to expect below to throw an exception seems to argue
                % against the last alternative above.
                get_file_name(ProgressStream, Globals, $pred, not_for_search,
                    TargetFile, TargetFileNameB, !Info, !IO),
                expect(unify(TargetFileName, TargetFileNameB), $pred,
                    "TargetFileName mismatch"),
                check_dependencies(ProgressStream, Globals, TargetFileNameB,
                    MaybeOldestTimestamp, MakeDepsSucceeded, DepFilesToMake,
                    DepsResult, !Info, !IO)
            )
        )
    ).

:- pred force_reanalysis_of_suboptimal_module(globals::in, module_name::in,
    bool::out, make_info::in, io::di, io::uo) is det.

force_reanalysis_of_suboptimal_module(Globals, ModuleName, ForceReanalysis,
        Info, !IO) :-
    ( if make_info_get_reanalysis_passes(Info) > 0 then
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

%---------------------------------------------------------------------------%

:- pred build_target(io.text_output_stream::in, globals::in,
    compilation_task_type_and_options::in,
    target_file::in, module_dep_info::in, list(target_file)::in,
    list(file_name)::in, list(string)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_target(ProgressStream, Globals, CompilationTask, TargetFile,
        ModuleDepInfo, TouchedTargetFiles, TouchedFiles, ExtraOptions,
        Succeeded, !Info, !IO) :-
    % XXX MAKE_FILENAME Either our caller should be able to give us
    % TargetFileName, or we could compute it here, and give it to code below.
    get_make_target_file_name(Globals, $pred, TargetFile, TargetFileName, !IO),
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
        % If we will perform a compilation task in a separate process,
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
        get_real_milliseconds(Time0, !IO),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        Cleanup = cleanup_files(ProgressStream, Globals, MaybeArgFileName,
            TouchedTargetFiles, TouchedFiles),
        setup_checking_for_interrupt(Cookie, !IO),
        get_default_options(Globals, DefaultOptionTable),
        DetectedGradeFlags = make_info_get_detected_grade_flags(!.Info),
        OptionVariables = make_info_get_options_variables(!.Info),
        OptionArgs = make_info_get_option_args(!.Info),
        setup_for_build_with_module_options(ProgressStream, DefaultOptionTable,
            invoked_by_mmc_make, ModuleName,
            DetectedGradeFlags, OptionVariables, OptionArgs,
            ExtraAndTaskOptions, MayBuild, !IO),
        % XXX MAKE A significant number of test case failures in the
        % tests/invalid directory are caused by a missing final line,
        % "For more information, recompile with `-E'.". I (zs) think that
        % calling maybe_print_delayed_error_messages here should fix this.
        (
            MayBuild = may_build(AllOptionArgs, BuildGlobals),
            open_module_error_stream(ModuleName, ProgressStream,
                MaybeErrorStream, !Info, !IO),
            (
                MaybeErrorStream = es_error_already_reported,
                Succeeded0 = did_not_succeed
            ;
                MaybeErrorStream = es_ok(ErrorStream),
                build_target_2(ModuleName, Task, MaybeArgFileName,
                    ModuleDepInfo, BuildGlobals, AllOptionArgs,
                    ProgressStream, ErrorStream, Succeeded0, !Info, !IO),
                close_module_error_stream_handle_errors(Globals, ModuleName,
                    ProgressStream, ErrorStream, !Info, !IO)
            )
        ;
            MayBuild = may_not_build(Specs),
            get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
            write_error_specs(ErrorStream, Globals, Specs, !IO),
            Succeeded0 = did_not_succeed
        ),
        teardown_checking_for_interrupt(VeryVerbose, Cookie, Cleanup,
            Succeeded0, Succeeded, !Info, !IO),
        record_made_target_given_maybe_touched_files(ProgressStream, Globals,
            Succeeded, TargetFile, TargetFileName,
            TouchedTargetFiles, TouchedFiles, !Info, !IO),
        get_real_milliseconds(Time, !IO),

        globals.lookup_bool_option(Globals, show_make_times, ShowMakeTimes),
        (
            ShowMakeTimes = yes,
            DiffSecs = float(Time - Time0) / 1000.0,
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

:- pred cleanup_files(io.text_output_stream::in, globals::in,
    maybe(string)::in, list(target_file)::in, list(string)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

cleanup_files(ProgressStream, Globals, MaybeArgFileName,
        TouchedTargetFiles, TouchedFiles, !MakeInfo, !IO) :-
    % XXX Remove `.int.tmp' files.
    list.foldl2(remove_make_target_file(ProgressStream, Globals, $pred,
        very_verbose), TouchedTargetFiles, !MakeInfo, !IO),
    list.foldl2(remove_file_for_make(ProgressStream, Globals, very_verbose),
        TouchedFiles, !MakeInfo, !IO),
    (
        MaybeArgFileName = yes(ArgFileName2),
        io.file.remove_file(ArgFileName2, _, !IO)
    ;
        MaybeArgFileName = no
    ).

:- pred build_target_2(module_name::in, compilation_task_type::in,
    maybe(file_name)::in, module_dep_info::in, globals::in,
    list(string)::in, io.text_output_stream::in, io.text_output_stream::in,
    maybe_succeeded::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

build_target_2(ModuleName, Task, ArgFileName, ModuleDepInfo, Globals,
        AllOptionArgs, ProgressStream, ErrorStream, Succeeded, !Info, !IO) :-
    (
        Task = process_module(ModuleTask),
        ModuleArg = sym_name_to_string(ModuleName),

        globals.lookup_bool_option(Globals, verbose_commands, Verbose),
        (
            Verbose = yes,
            AllArgs = AllOptionArgs ++ [ModuleArg],
            % XXX Don't write the default options.
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
        io.set_output_stream(ErrorStream, OldOutputStream, !IO),
        CallInSeparateProcess = do_task_in_separate_process(ModuleTask),
        (
            CallInSeparateProcess = yes,
            call_in_forked_process_with_backup(
                call_mercury_compile_main(Globals, [ModuleArg]),
                invoke_mmc(Globals, ProgressStream, ErrorStream,
                    ArgFileName, AllOptionArgs ++ [ModuleArg]),
                CompileSucceeded, !IO)
        ;
            CallInSeparateProcess = no,
            call_mercury_compile_main(Globals, [ModuleArg],
                CompileSucceeded, !IO)
        ),
        io.set_output_stream(OldOutputStream, _, !IO),

        ( if
            ( ModuleTask = task_compile_to_target_code
            ; ModuleTask = task_errorcheck
            )
        then
            % The `.err_date' file is needed because the `.err' file is touched
            % by all phases of compilation, including writing interfaces.
            touch_module_ext_datestamp(Globals, ProgressStream, ErrorStream,
                ModuleName, ext_cur_ngs(ext_cur_ngs_misc_err_date),
                TouchSucceeded, !IO),
            Succeeded = CompileSucceeded `and` TouchSucceeded
        else
            Succeeded = CompileSucceeded
        )
    ;
        Task = target_code_to_object_code(PIC),
        globals.get_target(Globals, CompilationTarget),

        % Run the compilation in a child process so it can be killed
        % if an interrupt arrives.
        call_in_forked_process(
            build_object_code(Globals, ModuleName, CompilationTarget, PIC,
                ProgressStream, ErrorStream, ModuleDepInfo),
            Succeeded, !IO)
    ;
        Task = foreign_code_to_object_code(PIC, Lang),
        get_foreign_code_file(Globals, ModuleName, PIC, Lang, ForeignCodeFile,
            !IO),

        % Run the compilation in a child process so it can be killed
        % if an interrupt arrives.
        call_in_forked_process(
            compile_foreign_code_file(Globals, ProgressStream, ErrorStream,
                PIC, ModuleDepInfo, ForeignCodeFile),
            Succeeded, !IO)
    ;
        Task = fact_table_code_to_object_code(PIC, FactTableFileName),
        get_object_extension(Globals, PIC, ObjExt),
        get_fact_table_foreign_code_file(Globals, do_create_dirs,
            ext_cur_ngs_gs(ObjExt),
            FactTableFileName, FactTableForeignCode, !IO),

        % Run the compilation in a child process so it can be killed
        % if an interrupt arrives.
        call_in_forked_process(
            compile_foreign_code_file(Globals, ProgressStream, ErrorStream,
                PIC, ModuleDepInfo, FactTableForeignCode),
            Succeeded, !IO)
    ).

:- pred build_object_code(globals::in, module_name::in, compilation_target::in,
    pic::in, io.text_output_stream::in, io.text_output_stream::in,
    module_dep_info::in, maybe_succeeded::out, io::di, io::uo) is det.

build_object_code(Globals, ModuleName, Target, PIC,
        ProgressStream, ErrorStream, _ModuleDepInfo, Succeeded, !IO) :-
    (
        Target = target_c,
        compile_c_file(Globals, ProgressStream, ErrorStream, PIC,
            ModuleName, Succeeded, !IO)
    ;
        Target = target_java,
        module_name_to_file_name_create_dirs(Globals, $pred,
            ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java),
            ModuleName, JavaFile, !IO),
        compile_java_files(Globals, ProgressStream, ErrorStream,
            JavaFile, [], Succeeded, !IO)
    ;
        Target = target_csharp,
        module_name_to_file_name_create_dirs(Globals, $pred,
            ext_cur_ngs_gs(ext_cur_ngs_gs_target_cs),
            ModuleName, CsharpFile, !IO),
        compile_target_code.link(Globals, ProgressStream, ErrorStream,
            csharp_library, ModuleName, [CsharpFile], Succeeded, !IO)
    ).

:- pred compile_foreign_code_file(globals::in,
    io.text_output_stream::in, io.text_output_stream::in, pic::in,
    module_dep_info::in, foreign_code_file::in, maybe_succeeded::out,
    io::di, io::uo) is det.

compile_foreign_code_file(Globals, ProgressStream, ErrorStream, PIC,
        ModuleDepInfo, ForeignCodeFile, Succeeded, !IO) :-
    (
        ForeignCodeFile = foreign_code_file(lang_c, CFile, ObjFile),
        do_compile_c_file(Globals, ProgressStream, ErrorStream, PIC,
            CFile, ObjFile, Succeeded, !IO)
    ;
        ForeignCodeFile = foreign_code_file(lang_java, JavaFile, _ClassFile),
        compile_java_files(Globals, ProgressStream, ErrorStream, JavaFile, [],
            Succeeded, !IO)
    ;
        ForeignCodeFile = foreign_code_file(lang_csharp, CSharpFile, DLLFile),
        compile_csharp_file(Globals, ProgressStream, ErrorStream,
            ModuleDepInfo, CSharpFile, DLLFile, Succeeded, !IO)
    ).

:- func do_task_in_separate_process(module_compilation_task_type) = bool.

do_task_in_separate_process(task_errorcheck) = no.
do_task_in_separate_process(task_make_int0) = no.
do_task_in_separate_process(task_make_int12) = no.
do_task_in_separate_process(task_make_int3) = no.
do_task_in_separate_process(task_make_opt) = yes.
do_task_in_separate_process(task_make_analysis_registry) = yes.
do_task_in_separate_process(task_compile_to_target_code) = yes.
do_task_in_separate_process(task_make_xml_doc) = yes.

%---------------------------------------------------------------------------%

:- pred get_foreign_code_file(globals::in, module_name::in, pic::in,
    foreign_language::in, foreign_code_file::out, io::di, io::uo) is det.

get_foreign_code_file(Globals, ModuleName, PIC, Lang, ForeignCodeFile, !IO) :-
    foreign_language_module_name(ModuleName, Lang, ForeignModName),
    foreign_language_file_extension(Lang, SrcExt),
    get_object_extension(Globals, PIC, ObjExt),
    module_name_to_file_name_create_dirs(Globals, $pred,
        SrcExt, ForeignModName, SrcFileName, !IO),
    module_name_to_file_name_create_dirs(Globals, $pred,
        ext_cur_ngs_gs(ObjExt), ForeignModName, ObjFileName, !IO),
    ForeignCodeFile = foreign_code_file(Lang, SrcFileName, ObjFileName).

:- pred get_object_extension(globals::in, pic::in, ext_cur_ngs_gs::out) is det.

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

:- pred call_mercury_compile_main(globals::in, list(string)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

call_mercury_compile_main(Globals, Args, Succeeded, !IO) :-
    io.get_exit_status(Status0, !IO),
    io.set_exit_status(0, !IO),
    mercury_compile_main.main_for_make(Globals, Args, !IO),
    io.get_exit_status(Status, !IO),
    Succeeded = ( if Status = 0 then succeeded else did_not_succeed ),
    io.set_exit_status(Status0, !IO).

:- pred invoke_mmc(globals::in,
    io.text_output_stream::in, io.text_output_stream::in, maybe(file_name)::in,
    list(string)::in, maybe_succeeded::out, io::di, io::uo) is det.

invoke_mmc(Globals, ProgressStream, ErrorStream,
        MaybeArgFileName, Args, Succeeded, !IO) :-
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
            ErrorStream, CommandVerbosity, Command, Succeeded, !IO)
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
    find_files_maybe_touched_by_task(ProgressStream, Globals, TargetFile,
        CompilationTask, TouchedTargetFiles, TouchedFiles, !Info, !IO),
    record_made_target_given_maybe_touched_files(ProgressStream, Globals,
        Succeeded, TargetFile, TargetFileName,
        TouchedTargetFiles, TouchedFiles, !Info, !IO).

:- pred record_made_target_given_maybe_touched_files(io.text_output_stream::in,
    globals::in, maybe_succeeded::in, target_file::in, string::in,
    list(target_file)::in, list(file_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

record_made_target_given_maybe_touched_files(ProgressStream, Globals,
        Succeeded, TargetFile, TargetFileName,
        TouchedTargetFiles, OtherTouchedFiles, !Info, !IO) :-
    (
        Succeeded = succeeded,
        TargetStatus = deps_status_up_to_date
    ;
        Succeeded = did_not_succeed,
        TargetStatus = deps_status_error,
        file_error_msg(TargetFileName, ErrorMsg),
        maybe_write_msg_locked(ProgressStream, !.Info, ErrorMsg, !IO)
    ),

    list.foldl(update_target_status(TargetStatus), TouchedTargetFiles, !Info),

    list.map_foldl2(
        get_file_name(ProgressStream, Globals, $pred, not_for_search),
        TouchedTargetFiles, TouchedTargetFileNames, !Info, !IO),

    some [!FileTimestamps] (
        !:FileTimestamps = make_info_get_file_timestamps(!.Info),
        list.foldl(delete_timestamp(ProgressStream, Globals),
            TouchedTargetFileNames, !FileTimestamps),
        list.foldl(delete_timestamp(ProgressStream, Globals),
            OtherTouchedFiles, !FileTimestamps),

        % When an .analysis file is made, that potentially invalidates other
        % .analysis files so we have to delete their timestamps. The exact list
        % of files which might be affected can be found by reading the
        % corresponding .imdg file. But it is simpler to just delete the
        % timestamps of all the .analysis files that we know about.
        ( if TargetFile = target_file(_, module_target_analysis_registry) then
            map.foldl(
                delete_analysis_registry_timestamps(ProgressStream, Globals),
                !.FileTimestamps, !FileTimestamps)
        else
            true
        ),
        make_info_set_file_timestamps(!.FileTimestamps, !Info)
    ),

    TargetFileTimestamps0 = make_info_get_target_file_timestamps(!.Info),
    version_hash_table.delete(TargetFile,
        TargetFileTimestamps0, TargetFileTimestamps),
    make_info_set_target_file_timestamps(TargetFileTimestamps, !Info).

:- pred update_target_status(dependency_status::in, target_file::in,
    make_info::in, make_info::out) is det.

update_target_status(TargetStatus, TargetFile, !Info) :-
    Dep = dep_target(TargetFile),
    DepStatusMap0 = make_info_get_dependency_status(!.Info),
    version_hash_table.set(Dep, TargetStatus, DepStatusMap0, DepStatusMap),
    make_info_set_dependency_status(DepStatusMap, !Info).

:- pred delete_analysis_registry_timestamps(io.text_output_stream::in,
    globals::in, string::in, maybe_error(timestamp)::in,
    file_timestamps::in, file_timestamps::out) is det.

delete_analysis_registry_timestamps(ProgressStream, Globals, FileName, _,
        !Timestamps) :-
    ( if string.suffix(FileName, ".analysis") then
        delete_timestamp(ProgressStream, Globals, FileName, !Timestamps)
    else
        true
    ).

:- pred delete_timestamp(io.text_output_stream::in, globals::in, string::in,
    file_timestamps::in, file_timestamps::out) is det.

delete_timestamp(ProgressStream, Globals, TouchedFile, !Timestamps) :-
    trace [io(!IO)] (
        debug_make_msg(Globals,
            string.format("Deleting timestamp for %s\n", [s(TouchedFile)]),
            DebugMsg),
        maybe_write_msg(ProgressStream, DebugMsg, !IO)
    ),
    map.delete(TouchedFile, !Timestamps).

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
:- pred find_files_maybe_touched_by_task(io.text_output_stream::in,
    globals::in, target_file::in, compilation_task_type::in,
    list(target_file)::out, list(file_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_files_maybe_touched_by_task(ProgressStream, Globals, TargetFile, Task,
        TouchedTargetFiles, TouchedFileNames, !Info, !IO) :-
    (
        Task = process_module(ModuleTask),
        find_files_maybe_touched_by_process_module(ProgressStream, Globals,
            TargetFile, ModuleTask, TouchedTargetFiles, TouchedFileNames,
            !Info, !IO)
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
        get_object_extension(Globals, PIC, ObjExt),
        fact_table_file_name_return_dirs(Globals, $pred,
            ext_cur_ngs_gs(ObjExt),
            FactTableName, FactTableDirs, FactTableObjectFile),
        create_any_dirs_on_path(FactTableDirs, !IO),
        TouchedFileNames = [FactTableObjectFile]
    ).

:- pred find_files_maybe_touched_by_process_module(io.text_output_stream::in,
    globals::in, target_file::in, module_compilation_task_type::in,
    list(target_file)::out, list(file_name)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

find_files_maybe_touched_by_process_module(ProgressStream, Globals,
        TargetFile, Task, TouchedTargetFiles, TouchedFileNames, !Info, !IO) :-
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

    module_dep_info_get_maybe_top_module(ModuleDepInfo,
        MaybeTopModule),
    NestedSubModules = get_nested_children_list_of_top_module(MaybeTopModule),
    SourceFileModuleNames = [ModuleName | NestedSubModules],

    list.map_foldl2(get_maybe_module_dep_info(ProgressStream, Globals),
        NestedSubModules, MaybeNestedModuleDepInfos, !Info, !IO),
    ( if
        list.map(
            ( pred(some_module_dep_info(MDI)::in, MDI::out) is semidet),
            MaybeNestedModuleDepInfos, NestedModuleDepInfos)
    then
        ModuleDepInfos = [ModuleDepInfo | NestedModuleDepInfos]
    else
        % This error should have been caught earlier. We should not be
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
            ModuleDepInfos, ForeignCodeFileList, !IO),
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
        module_name_to_file_name(Globals, $pred, TimestampExt,
            TargetModuleName, TimestampFile),
        list.cons(TimestampFile, !TimestampFileNames)
    else
        true
    ).

%---------------------------------------------------------------------------%

external_foreign_code_files(Globals, PIC, ModuleDepInfo, ForeignFiles, !IO) :-
    % Find externally compiled foreign code files for
    % `:- pragma foreign_proc' declarations.
    %
    % Any changes here may require corresponding changes in
    % get_foreign_object_targets.

    maybe_pic_object_file_extension(PIC, ObjExt, _),
    globals.get_target(Globals, CompilationTarget),

    % None of the current backends require externally compiled foreign code,
    % except the C backend for fact tables.
    (
        CompilationTarget = target_c,
        module_dep_info_get_fact_tables(ModuleDepInfo, FactTableFiles),
        list.map_foldl(
            get_fact_table_foreign_code_file(Globals, do_not_create_dirs,
                ext_cur_ngs_gs(ObjExt)),
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
    fact_table_file_name_return_dirs(Globals, $pred,
        ext_cur_ngs_gs(ext_cur_ngs_gs_target_c),
        FactTableFileName, FactTableDirsC, FactTableCFileName),
    maybe_create_any_dirs_on_path(Mkdir, FactTableDirsC, !IO),
    fact_table_file_name_return_dirs(Globals, $pred, ObjExt,
        FactTableFileName, FactTableDirsO, FactTableObjFileName),
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
        ; TargetType = module_target_foreign_object(_, _)
        ; TargetType = module_target_fact_table_object(_, _)
        ; TargetType = module_target_xml_doc
        ),
        Result = non_pic
    ).

%---------------------------------------------------------------------------%
:- end_module make.module_target.
%---------------------------------------------------------------------------%
