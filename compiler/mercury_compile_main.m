%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2017-2025 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mercury_compile_main.m.
% Main authors: fjh, zs.
%
% This is the top-level of the Mercury compiler.
%
%---------------------------------------------------------------------------%

:- module top_level.mercury_compile_main.
:- interface.

:- import_module libs.
:- import_module libs.globals.

:- import_module io.
:- import_module list.

    % This is the main entry point for the Mercury compiler.
    % It is called from mercury_compile.main.
    %
:- pred real_main(io::di, io::uo) is det.

    % main_for_make(ProgressStream, ErrorStream, Globals, Args, !IO)
    % is called from make.module_target.call_mercury_compile_main.
    %
:- pred main_for_make(io.text_output_stream::in, io.text_output_stream::in,
    globals::in, list(string)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module backend_libs.link_target_code.
:- import_module hlds.
:- import_module hlds.instmap.
:- import_module libs.check_libgrades.
:- import_module libs.file_util.
:- import_module libs.handle_options.
:- import_module libs.maybe_util.
:- import_module libs.op_mode.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module make.
:- import_module make.build.
:- import_module make.deps_cache.
:- import_module make.options_file.
:- import_module make.top_level.
:- import_module mdbcomp.
:- import_module mdbcomp.shared_utilities.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.comp_unit_interface.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.make_module_file_names.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_parse_tree.
:- import_module parse_tree.read_modules.
:- import_module parse_tree.source_file_map.
:- import_module parse_tree.write_deps_file.
:- import_module parse_tree.write_error_spec.
:- import_module parse_tree.write_module_interface_files.
:- import_module recompilation.
:- import_module recompilation.check.
:- import_module top_level.mercury_compile_args.
:- import_module top_level.mercury_compile_augment.

:- import_module assoc_list.
:- import_module benchmarking.
:- import_module bool.
:- import_module cord.
:- import_module dir.
:- import_module gc.
:- import_module getopt.
:- import_module io.environment.
:- import_module library.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

real_main(!IO) :-
    gc_init(!IO),

    % All messages go to stderr.
    io.stderr_stream(StdErr, !IO),
    ProgressStream = StdErr,
    ErrorStream = StdErr,
    % XXX STREAM
    io.set_output_stream(StdErr, _, !IO),

    unlimit_stack(!IO),
    io.command_line_arguments(CmdLineArgs, !IO),

    setup_all_args(ProgressStream, ErrorStream, CmdLineArgs, ArgResult, !IO),
    (
        ArgResult = apr_success(Globals, EnvOptFileVariables, EnvVarArgs,
            OptionArgs, NonOptionArgs),
        main_after_setup(ProgressStream, ErrorStream, Globals,
            EnvOptFileVariables, EnvVarArgs, OptionArgs, NonOptionArgs, !IO),
        trace [compile_time(flag("file_name_translations")), io(!TIO)] (
            write_translations_record_if_any(Globals, !TIO)
        )
    ;
        ArgResult = apr_failure
        % All the error messages that explain the reason for the failure
        % have already been printed.
    ),
    record_make_deps_cache_stats(!IO),
    record_write_deps_file_cache_stats(!IO),
    record_instmap_delta_restrict_stats(!IO),
    close_any_specific_compiler_streams(!IO).

%---------------------------------------------------------------------------%

main_for_make(ProgressStream, ErrorStream, Globals, Args, !IO) :-
    io.environment.get_environment_var_map(EnvVarMap, !IO),
    EnvOptFileVariables = env_optfile_variables_init(EnvVarMap),
    get_args_representing_env_vars(EnvVarArgs, !IO),
    OptionArgs = [],
    main_after_setup(ProgressStream, ErrorStream, Globals,
        EnvOptFileVariables, EnvVarArgs, OptionArgs, Args, !IO).

%---------------------------------------------------------------------------%

:- pred main_after_setup(io.text_output_stream::in, io.text_output_stream::in,
    globals::in, env_optfile_variables::in,
    list(string)::in, list(string)::in, list(string)::in,
    io::di, io::uo) is det.

main_after_setup(ProgressStream, ErrorStream, Globals, EnvOptFileVariables,
        EnvVarArgs, OptionArgs, Args, !IO) :-
    globals.lookup_bool_option(Globals, version, Version),
    globals.lookup_bool_option(Globals, help, Help),

    % NOTE: --help takes precedence over any other modes of operation as we do
    % not wish to place unnecessary obstacles before users who want help.
    % --version takes precedence over the remaining modes of operation since
    % this behaviour is common in other compilers and command line tools and
    % will be in line with the expectations of at least some users.
    % XXX Should we use ErrorStream (which is stderr) instead of StdOutStream?
    ( if Help = yes then
        io.stdout_stream(StdOutStream, !IO),
        long_usage(StdOutStream, !IO)
    else if Version = yes then
        io.stdout_stream(StdOutStream, !IO),
        display_compiler_version(StdOutStream, !IO)
    else
        globals.get_op_mode(Globals, OpMode),
        HaveParseTreeMaps0 = init_have_parse_tree_maps,
        Specs0 = [],
        do_op_mode(ProgressStream, ErrorStream, Globals, OpMode,
            EnvOptFileVariables, EnvVarArgs, OptionArgs, Args,
            HaveParseTreeMaps0, _HaveParseTreeMaps, Specs0, Specs, !IO),
        write_error_specs(ErrorStream, Globals, Specs, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred do_op_mode(io.text_output_stream::in, io.text_output_stream::in,
    globals::in, op_mode::in, env_optfile_variables::in,
    list(string)::in, list(string)::in, list(string)::in,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

do_op_mode(ProgressStream, ErrorStream, Globals, OpMode, EnvOptFileVariables,
        EnvVarArgs, OptionArgs, Args, !HaveParseTreeMaps, !Specs, !IO) :-
    (
        OpMode = opm_top_make,
        % make_process_compiler_args itself does not pay attention to the
        % value of filenames_from_stdin, but we definitely should not let it
        % pass filenames_from_stdin=yes to any subcompilations.
        globals.set_option(filenames_from_stdin, bool(no),
            Globals, MakeGlobals),
        make_process_compiler_args(ProgressStream, MakeGlobals,
            EnvOptFileVariables, EnvVarArgs, OptionArgs, Args, !IO)
    ;
        OpMode = opm_top_generate_source_file_mapping,
        source_file_map.write_source_file_map(ProgressStream, Globals,
            Args, !IO)
    ;
        OpMode = opm_top_generate_standalone_interface(StandaloneIntBasename),
        do_op_mode_standalone_interface(ProgressStream, ErrorStream, Globals,
            StandaloneIntBasename, !IO)
    ;
        OpMode = opm_top_query(OpModeQuery),
        do_op_mode_query(ErrorStream, Globals, OpModeQuery, !IO)
    ;
        OpMode = opm_top_args(OpModeArgs, InvokedByMmcMake),
        globals.lookup_bool_option(Globals, filenames_from_stdin,
            FileNamesFromStdin),
        ( if
            Args = [],
            FileNamesFromStdin = no
        then
            io.stderr_stream(StdErr, !IO),
            short_usage(StdErr, !IO)
        else
            do_op_mode_args(ProgressStream, ErrorStream, Globals, OpModeArgs,
                InvokedByMmcMake, FileNamesFromStdin, EnvOptFileVariables,
                EnvVarArgs, OptionArgs, Args, !HaveParseTreeMaps, !Specs, !IO)
        )
    ).

:- pred do_op_mode_standalone_interface(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, string::in,
    io::di, io::uo) is det.

do_op_mode_standalone_interface(ProgressStream, ErrorStream, Globals,
        StandaloneIntBasename, !IO) :-
    globals.get_target(Globals, Target),
    (
        ( Target = target_csharp
        ; Target = target_java
        ),
        io.progname_base("mercury_compile", ProgName, !IO),
        Pieces = [fixed(ProgName), suffix(":"), nl,
            words("Error:"), quote("--generate-standalone-interface"),
            words("is not required for target language"),
            words(compilation_target_string(Target)), suffix("."), nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_options,
            Pieces),
        write_error_spec(ErrorStream, Globals, Spec, !IO)
    ;
        Target = target_c,
        make_standalone_interface(Globals, ProgressStream,
            StandaloneIntBasename, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred do_op_mode_query(io.text_output_stream::in, globals::in,
    op_mode_query::in, io::di, io::uo) is det.

do_op_mode_query(ErrorStream, Globals, OpModeQuery, !IO) :-
    io.stdout_stream(StdOutStream, !IO),
    (
        OpModeQuery = opmq_output_cc,
        globals.lookup_string_option(Globals, cc, CC),
        io.print_line(StdOutStream, CC, !IO)
    ;
        OpModeQuery = opmq_output_c_compiler_type,
        globals.lookup_string_option(Globals, c_compiler_type, CC_Type),
        io.print_line(StdOutStream, CC_Type, !IO)
    ;
        OpModeQuery = opmq_output_cflags,
        get_c_compiler_flags(Globals, CFlags),
        io.print_line(StdOutStream, CFlags, !IO)
    ;
        OpModeQuery = opmq_output_c_include_directory_flags,
        get_c_include_dir_flags(Globals, CInclFlags),
        io.print_line(StdOutStream, CInclFlags, !IO)
    ;
        OpModeQuery = opmq_output_csharp_compiler,
        globals.lookup_string_option(Globals, csharp_compiler, CSC),
        io.print_line(StdOutStream, CSC, !IO)
    ;
        OpModeQuery = opmq_output_csharp_compiler_type,
        globals.lookup_string_option(Globals, csharp_compiler_type, CSC_Type),
        io.print_line(StdOutStream, CSC_Type, !IO)
    ;
        OpModeQuery = opmq_output_java_class_dir,
        % XXX LEGACY
        get_java_dir_path(Globals, ext_cur_ngs_gs_java_class,
            ClassDirNames, _ClassDirNamesProposed),
        ClassDirName = dir.relative_path_name_from_components(ClassDirNames),
        io.print_line(StdOutStream, ClassDirName, !IO)
    ;
        OpModeQuery = opmq_output_grade_defines,
        get_c_grade_defines(Globals, GradeDefines),
        io.print_line(StdOutStream, GradeDefines, !IO)
    ;
        OpModeQuery = opmq_output_link_command,
        globals.lookup_string_option(Globals, link_executable_command,
            LinkCommand),
        io.print_line(StdOutStream, LinkCommand, !IO)
    ;
        OpModeQuery = opmq_output_shared_lib_link_command,
        globals.lookup_string_option(Globals, link_shared_lib_command,
            LinkCommand),
        io.print_line(StdOutStream, LinkCommand, !IO)
    ;
        OpModeQuery = opmq_output_library_link_flags,
        output_library_link_flags(Globals, StdOutStream, Specs, !IO),
        write_error_specs(ErrorStream, Globals, Specs, !IO)
    ;
        OpModeQuery = opmq_output_grade_string,
        % When Mmake asks for the grade, it really wants the directory
        % component to use. This is consistent with scripts/canonical_grade.
        globals.get_grade_dir(Globals, Grade),
        io.print_line(StdOutStream, Grade, !IO)
    ;
        OpModeQuery = opmq_output_libgrades,
        globals.lookup_accumulating_option(Globals, libgrades, LibGrades),
        list.foldl(io.print_line(StdOutStream), LibGrades, !IO)
    ;
        OpModeQuery = opmq_output_stdlib_grades,
        globals.get_maybe_stdlib_grades(Globals, MaybeStdLibGrades),
        (
            MaybeStdLibGrades = stdlib_grades_known(StdLibGrades),
            set.fold(io.print_line(StdOutStream), StdLibGrades, !IO)
        ;
            MaybeStdLibGrades = stdlib_grades_unknown
            % During the construction of Globals, handle_opmode_implications
            % should have set detect_stdlib_grades to "yes", which should
            % cause handle_libgrades to detect the standard library's grades.
            % That predicate would leave this field of the globals containing
            % stdlib_grades_unknown only if that process got an error.
            % In such cases, it would print a message about the error,
            % which leaves us nothing to do here.
        )
    ;
        OpModeQuery = opmq_output_stdlib_modules,
        GetStdLibModules =
            ( pred(Line::out) is multi :-
                library.stdlib_module_doc_undoc(ModuleName, DocUndoc),
                (
                    DocUndoc = doc,
                    DocStr = "DOC"
                ;
                    DocUndoc = undoc,
                    DocStr = "UNDOC"
                ),
                Line = DocStr ++ " " ++ ModuleName ++ ".m\n"
            ),
        solutions.solutions(GetStdLibModules, StdLibLines),
        list.foldl(io.write_string(StdOutStream), StdLibLines, !IO)
    ;
        OpModeQuery = opmq_output_target_arch,
        globals.lookup_string_option(Globals, target_arch, TargetArch),
        io.print_line(StdOutStream, TargetArch, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Do the modes of operation that process the argument list.
%

:- pred do_op_mode_args(io.text_output_stream::in, io.text_output_stream::in,
    globals::in, op_mode_args::in, op_mode_invoked_by_mmc_make::in,
    bool::in, env_optfile_variables::in,
    list(string)::in, list(string)::in, list(string)::in,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

do_op_mode_args(ProgressStream, ErrorStream, Globals, OpModeArgs,
        InvokedByMmcMake, FileNamesFromStdin, EnvOptFileVariables,
        EnvVarArgs, OptionArgs, Args, !HaveParseTreeMaps, !Specs, !IO) :-
    maybe_check_libraries_are_installed(Globals, LibgradeCheckSpecs, !IO),
    io.stderr_stream(StdErr, !IO),
    (
        LibgradeCheckSpecs = [],
        (
            FileNamesFromStdin = yes,
            % Mmc --make does not set --filenames-from-stdin.
            expect(unify(InvokedByMmcMake, op_mode_not_invoked_by_mmc_make),
                $pred, "InvokedByMmcMake != op_mode_not_invoked_by_mmc_make"),
            io.stdin_stream(StdIn, !IO),
            setup_and_process_compiler_stdin_args(ProgressStream, ErrorStream,
                StdIn, Globals, OpModeArgs, InvokedByMmcMake,
                EnvOptFileVariables, EnvVarArgs, OptionArgs,
                cord.empty, ModulesToLinkCord, cord.empty, ExtraObjFilesCord,
                !HaveParseTreeMaps, !Specs, !IO)
        ;
            FileNamesFromStdin = no,
            (
                InvokedByMmcMake = op_mode_not_invoked_by_mmc_make,
                setup_and_process_compiler_cmd_line_args(ProgressStream,
                    ErrorStream, Globals, OpModeArgs, InvokedByMmcMake,
                    EnvOptFileVariables, EnvVarArgs, OptionArgs, Args,
                    cord.empty, ModulesToLinkCord,
                    cord.empty, ExtraObjFilesCord,
                    !HaveParseTreeMaps, !Specs, !IO)
            ;
                InvokedByMmcMake = op_mode_invoked_by_mmc_make,
                % `mmc --make' has already set up the options.
                do_process_compiler_cmd_line_args(ProgressStream, ErrorStream,
                    Globals, OpModeArgs, InvokedByMmcMake, OptionArgs, Args,
                    cord.empty, ModulesToLinkCord,
                    cord.empty, ExtraObjFilesCord, !HaveParseTreeMaps, !IO)
            )
        ),

        % Print all remaining module-specific error_specs,
        % as well as the ones generated just above.
        write_error_specs(ErrorStream, Globals, !.Specs, !IO),
        maybe_print_delayed_error_messages(ErrorStream, Globals, !IO),

        io.get_exit_status(ExitStatus, !IO),
        ( if ExitStatus = 0 then
            ModulesToLink = cord.list(ModulesToLinkCord),
            ExtraObjFiles = cord.list(ExtraObjFilesCord),
            ( if
                OpModeArgs = opma_augment(opmau_front_and_middle(
                    opfam_target_object_and_executable)),
                ModulesToLink = [FirstModule | _]
            then
                generate_executable(ProgressStream, ErrorStream, Globals,
                    InvokedByMmcMake, EnvOptFileVariables,
                    EnvVarArgs, OptionArgs,
                    FirstModule, ModulesToLink, ExtraObjFiles, !IO)
            else
                true
            )
        else
            true
        )
    ;
        LibgradeCheckSpecs = [_ | _],
        % Print all remaining module-specific error_specs.
        write_error_specs(ErrorStream, Globals, !.Specs, !IO),
        maybe_print_delayed_error_messages(ErrorStream, Globals, !IO),

        % Print the error_specs from the library check, which are
        % not specific to any module.
        write_error_specs(StdErr, Globals, LibgradeCheckSpecs, !IO)
    ),

    globals.lookup_bool_option(Globals, statistics, Statistics),
    (
        Statistics = yes,
        ( if benchmarking.full_memory_stats_are_available then
            benchmarking.report_full_memory_stats(StdErr, !IO)
        else
            true
        )
    ;
        Statistics = no
    ).

:- pred generate_executable(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, op_mode_invoked_by_mmc_make::in,
    env_optfile_variables::in, list(string)::in, list(string)::in,
    module_name::in, list(module_name)::in, list(string)::in,
    io::di, io::uo) is det.

generate_executable(ProgressStream, ErrorStream, Globals, InvokedByMmcMake,
        EnvOptFileVariables, EnvVarArgs, OptionArgs,
        FirstModule, ModulesToLink, ExtraObjFiles, !IO) :-
    globals.get_target(Globals, Target),
    (
        Target = target_java,
        % For Java, at the "link" step we just generate a shell script;
        % the actual linking will be done at runtime by the Java interpreter.
        create_java_shell_script(ProgressStream, Globals, FirstModule,
            Succeeded, !IO)
    ;
        ( Target = target_c
        ; Target = target_csharp
        ),
        % XXX STREAM
        % Should we go from non-main-module-specific
        % progress and error streams to main-module-specific streams?
        (
            InvokedByMmcMake = op_mode_invoked_by_mmc_make,
            % `mmc --make' has already set up the options.
            link_modules_into_executable_or_shared_library(ProgressStream,
                Globals, ModulesToLink, ExtraObjFiles, Specs, Succeeded, !IO)
        ;
            InvokedByMmcMake = op_mode_not_invoked_by_mmc_make,
            get_default_options(Globals, DefaultOptionTable),
            globals.get_maybe_stdlib_grades(Globals, MaybeStdLibGrades),
            setup_for_build_with_module_options(ProgressStream,
                DefaultOptionTable, MaybeStdLibGrades, not_invoked_by_mmc_make,
                FirstModule, EnvOptFileVariables, EnvVarArgs, OptionArgs,
                [], MayBuild, !IO),
            (
                MayBuild = may_not_build(Specs),
                Succeeded = did_not_succeed
            ;
                MayBuild = may_build(_AllOptionArgs, BuildGlobals),
                link_modules_into_executable_or_shared_library(ProgressStream,
                    BuildGlobals, ModulesToLink, ExtraObjFiles,
                    Specs, Succeeded, !IO)
            )
        ),
        write_error_specs(ErrorStream, Globals, Specs, !IO)
    ),
    maybe_set_exit_status(Succeeded, !IO).

%---------------------------------------------------------------------------%

:- pred setup_and_process_compiler_stdin_args(io.text_output_stream::in,
    io.text_output_stream::in, io.text_input_stream::in, globals::in,
    op_mode_args::in, op_mode_invoked_by_mmc_make::in,
    env_optfile_variables::in, list(string)::in, list(string)::in,
    cord(module_name)::in, cord(module_name)::out,
    cord(string)::in, cord(string)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

setup_and_process_compiler_stdin_args(ProgressStream, ErrorStream, StdIn,
        Globals, OpModeArgs, InvokedByMmcMake, EnvOptFileVariables,
        EnvVarArgs, OptionArgs, !Modules, !ExtraObjFiles,
        !HaveParseTreeMaps, !Specs, !IO) :-
    ( if cord.is_empty(!.Modules) then
        true
    else
        gc.garbage_collect(!IO)
    ),
    io.read_line_as_string(StdIn, LineResult, !IO),
    (
        LineResult = ok(Line),
        Arg = string.rstrip(Line),
        setup_and_process_compiler_arg(ProgressStream, ErrorStream, Globals,
            OpModeArgs, InvokedByMmcMake, EnvOptFileVariables,
            EnvVarArgs, OptionArgs, Arg, ArgModules, ArgExtraObjFiles,
            !HaveParseTreeMaps, !Specs, !IO),
        !:Modules = !.Modules ++ cord.from_list(ArgModules),
        !:ExtraObjFiles = !.ExtraObjFiles ++ cord.from_list(ArgExtraObjFiles),
        setup_and_process_compiler_stdin_args(ProgressStream, ErrorStream,
            StdIn, Globals, OpModeArgs, InvokedByMmcMake, EnvOptFileVariables,
            EnvVarArgs, OptionArgs, !Modules, !ExtraObjFiles,
            !HaveParseTreeMaps, !Specs, !IO)
    ;
        LineResult = eof
    ;
        LineResult = error(Error),
        io.error_message(Error, Msg),
        Pieces = [words("Error reading module name from standard input:"),
            words(Msg), suffix("."), nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_read_files, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------%

:- pred setup_and_process_compiler_cmd_line_args(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    op_mode_args::in, op_mode_invoked_by_mmc_make::in,
    env_optfile_variables::in,
    list(string)::in, list(string)::in, list(string)::in,
    cord(module_name)::in, cord(module_name)::out,
    cord(string)::in, cord(string)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

setup_and_process_compiler_cmd_line_args(_, _, _, _, _, _, _, _, [],
        !Modules, !ExtraObjFiles, !HaveParseTreeMaps, !Specs, !IO).
setup_and_process_compiler_cmd_line_args(ProgressStream, ErrorStream, Globals,
        OpModeArgs, InvokedByMmcMake, EnvOptFileVariables,
        EnvVarArgs, OptionArgs, [Arg | Args], !Modules, !ExtraObjFiles,
        !HaveParseTreeMaps, !Specs, !IO) :-
    setup_and_process_compiler_arg(ProgressStream, ErrorStream, Globals,
        OpModeArgs, InvokedByMmcMake, EnvOptFileVariables,
        EnvVarArgs, OptionArgs, Arg, ArgModules, ArgExtraObjFiles,
        !HaveParseTreeMaps, !Specs, !IO),
    (
        Args = []
    ;
        Args = [_ | _],
        gc.garbage_collect(!IO)
    ),
    !:Modules = !.Modules ++ cord.from_list(ArgModules),
    !:ExtraObjFiles = !.ExtraObjFiles ++ cord.from_list(ArgExtraObjFiles),
    setup_and_process_compiler_cmd_line_args(ProgressStream, ErrorStream,
        Globals, OpModeArgs, InvokedByMmcMake, EnvOptFileVariables,
        EnvVarArgs, OptionArgs, Args, !Modules, !ExtraObjFiles,
        !HaveParseTreeMaps, !Specs, !IO).

:- pred do_process_compiler_cmd_line_args(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, op_mode_args::in,
    op_mode_invoked_by_mmc_make::in, list(string)::in, list(string)::in,
    cord(module_name)::in, cord(module_name)::out,
    cord(string)::in, cord(string)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

do_process_compiler_cmd_line_args(_, _, _, _, _, _, [],
        !ModulesToLink, !ExtraObjFiles, !HaveParseTreeMaps, !IO).
do_process_compiler_cmd_line_args(ProgressStream, ErrorStream, Globals,
        OpModeArgs, InvokedByMmcMake, OptionArgs, [Arg | Args],
        !ModulesToLink, !ExtraObjFiles, !HaveParseTreeMaps, !IO) :-
    % `mmc --make' has already set up the options.
    FileOrModule = string_to_file_or_module(Arg),
    do_process_compiler_arg(ProgressStream, ErrorStream, Globals,
        OpModeArgs, InvokedByMmcMake, OptionArgs, FileOrModule,
        ArgModules, ArgExtraObjFiles, !HaveParseTreeMaps, !IO),
    (
        Args = []
    ;
        Args = [_ | _],
        gc.garbage_collect(!IO)
    ),
    !:ModulesToLink = !.ModulesToLink ++ cord.from_list(ArgModules),
    !:ExtraObjFiles = !.ExtraObjFiles ++ cord.from_list(ArgExtraObjFiles),
    do_process_compiler_cmd_line_args(ProgressStream, ErrorStream, Globals,
        OpModeArgs, InvokedByMmcMake, OptionArgs, Args,
        !ModulesToLink, !ExtraObjFiles, !HaveParseTreeMaps, !IO).

%---------------------%

    % Figure out whether the compiler argument is a module name or a file name.
    % Open the specified file or module, and process it.
    % Return the list of modules (including submodules,
    % if they were compiled to separate object files)
    % that should be linked into the final executable.
    %
    % The actual work is done by do_process_compiler_arg.
    % XXX The job of this predicate is apparently to ensure that
    % we go through the machinery of mmc --make, represented here by
    % build_with_module_options_args, even if we were NOT invoked with --make.
    % This seems strange to me. -zs
    %
:- pred setup_and_process_compiler_arg(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    op_mode_args::in, op_mode_invoked_by_mmc_make::in,
    env_optfile_variables::in, list(string)::in, list(string)::in, string::in,
    list(module_name)::out, list(string)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

setup_and_process_compiler_arg(ProgressStream, ErrorStream, Globals,
        OpModeArgs, InvokedByMmcMake, EnvOptFileVariables,
        EnvVarArgs, OptionArgs, Arg, ModulesToLink, ExtraObjFiles,
        !HaveParseTreeMaps, !Specs, !IO) :-
    get_default_options(Globals, DefaultOptionTable),
    FileOrModule = string_to_file_or_module(Arg),
    ModuleName = file_or_module_to_module_name(FileOrModule),
    globals.get_maybe_stdlib_grades(Globals, MaybeStdLibGrades),
    ExtraOptions = [],
    setup_for_build_with_module_options(ProgressStream, DefaultOptionTable,
        MaybeStdLibGrades, not_invoked_by_mmc_make, ModuleName,
        EnvOptFileVariables, EnvVarArgs, OptionArgs, ExtraOptions,
        MayBuild, !IO),
    (
        MayBuild = may_not_build(SetupSpecs),
        % XXX STREAM
        % Should we print SetupSpecs to the module-specific error stream?
        write_error_specs(ErrorStream, Globals, SetupSpecs, !IO),
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        MayBuild = may_build(_AllOptionArgs, BuildGlobals),
        do_process_compiler_arg(ProgressStream, ErrorStream, BuildGlobals,
            OpModeArgs, InvokedByMmcMake, OptionArgs, FileOrModule,
            ModulesToLink, ExtraObjFiles, !HaveParseTreeMaps, !IO)
    ).

%---------------------%

:- pred do_process_compiler_arg(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    op_mode_args::in, op_mode_invoked_by_mmc_make::in, list(string)::in,
    file_or_module::in, list(module_name)::out, list(string)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

do_process_compiler_arg(ProgressStream, ErrorStream, Globals0,
        OpModeArgs, InvokedByMmcMake, OptionArgs, FileOrModule,
        ModulesToLink, ExtraObjFiles, !HaveParseTreeMaps, !IO) :-
    % XXX ITEM_LIST There is an inconsistency between the various OpModeArgs
    % that construct a module_and_imports structure in how they do it.
    %
    % The op modes that generate one or more dependency files call predicates
    % in generate_dep_d_files.m, which all end up constructing that structure
    % by calling init_module_and_imports. On the other hand, the op modes
    % that augment the module call augment_and_process_module, which
    % calls grab_imported_modules, which constructs that structure
    % using make_module_and_imports. And once they create an initial
    % module_and_imports structure, they subject that structure to
    % different further processing.
    %
    % I (zs) think that this is probably the reason why the .d files
    % of a program contain one set of contents just after the program's
    % dependencies are built (or rebuilt), and a different set of contents
    % after we start generated interface files and target code for the
    % program's modules.
    %
    % This may be *acceptable* behavior if the approaches using
    % init_module_and_imports and make_module_and_imports both compute
    % supersets of all the actual dependencies, even if e.g.
    % the approach using make_module_and_imports computes a *bigger*
    % superset. However, it is definitely not *good* behavior.
    %
    % The best fix seems to be to use a single approach, and that
    % approach should be the one using make_module_and_imports.
    %
    % XXX The predicates named in the above comment have been deleted, though
    % I (zs) think that the problem it describes probably still remains.

    % XXX Another, different problem is that
    %
    % - some of the predicates called from here update the initial Globals0
    %   to disable smart recompilation (when they find some situation that
    %   smart recompilation is not able to handle), but then
    %
    % - these updated values of Globals get to the end of a scope, and
    %   control returns to a caller that has access only to the original
    %   Globals0, effectively undoing the disabling of smart recompilation,

    (
        OpModeArgs = opma_generate_dependencies(MaybeMakeInts),
        generate_and_write_dep_file_gendep(ProgressStream, Globals0,
            FileOrModule, DepsMap, DepSpecs, !IO),
        ( if
            MaybeMakeInts = do_make_ints,
            contains_errors(Globals0, DepSpecs) = no
        then
            deps_make_ints(ProgressStream, Globals0, DepsMap,
                DepSpecs, Specs, !HaveParseTreeMaps, !IO)
        else
            Specs = DepSpecs
        ),
        SpecsList = [Specs],
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        OpModeArgs = opma_generate_dependency_file,
        generate_and_write_d_file_gendep(ProgressStream, Globals0,
            FileOrModule, _DepsMap, DepSpecs, !IO),
        SpecsList = [DepSpecs],
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        OpModeArgs = opma_convert_to_mercury,
        read_module_or_file(ProgressStream, Globals0, Globals, FileOrModule,
            do_not_return_timestamp, HaveReadSrc, !HaveParseTreeMaps, !IO),
        (
            HaveReadSrc = have_not_read_module(_FileName, Errors)
        ;
            HaveReadSrc = have_module(_FileName, ParseTreeSrc, Source),
            have_parse_tree_source_get_maybe_timestamp_errors(Source,
                _MaybeTimestamp, Errors),
            ( if halt_at_module_error(Globals, Errors) then
                true
            else
                ModuleName = ParseTreeSrc ^ pts_module_name,
                module_name_to_cur_dir_file_name(ext_cur_user_ugly,
                    ModuleName, UglyFileName),
                output_parse_tree_src(ProgressStream, Globals, UglyFileName,
                    ParseTreeSrc, _Succeeded, !IO)
            )
        ),
        Specs = get_read_module_specs(Errors),
        SpecsList = [Specs],
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        OpModeArgs = opma_make_interface(InterfaceFile),
        do_process_compiler_arg_make_interface(ProgressStream, Globals0,
            InterfaceFile, FileOrModule, SpecsList, !HaveParseTreeMaps, !IO),
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        OpModeArgs = opma_augment(OpModeAugment),
        find_modules_to_recompile(ProgressStream, Globals0, Globals,
            FileOrModule, ModulesToRecompile, !HaveParseTreeMaps, !IO),
        ( if ModulesToRecompile = some_modules([]) then
            % XXX Currently smart recompilation is disabled if mmc is linking
            % the executable, because it doesn't know how to check whether
            % all the necessary intermediate files are present and up-to-date.
            SpecsList = [],
            ModulesToLink = [],
            ExtraObjFiles = []
        else
            read_augment_and_process_module(ProgressStream, ErrorStream,
                Globals, OpModeAugment, InvokedByMmcMake, OptionArgs,
                FileOrModule, ModulesToRecompile, ModulesToLink, ExtraObjFiles,
                Specs, !HaveParseTreeMaps, !IO),
            SpecsList = [Specs]
        )
    ),
    list.foldl(write_error_specs(ErrorStream, Globals0), SpecsList, !IO),
    maybe_print_delayed_error_messages(ErrorStream, Globals0, !IO).

%---------------------%

:- pred deps_make_ints(io.text_output_stream::in, globals::in, deps_map::in,
    list(error_spec)::in, list(error_spec)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

deps_make_ints(ProgressStream, Globals, DepsMap,
        !Specs, !HaveParseTreeMaps, !IO) :-
    map.values(DepsMap, DepsList),
    list.filter_map_foldl(gather_local_burdened_modules,
        DepsList, BurdenedModules, [], Ancestors),
    % XXX This code should be parallelized.
    % We could replace the next call with a loop that, in each iteration,
    %
    % - calls generate_parse_tree_int3 for a given burdened_module,
    %   adding that parse_tree_int3 to !HaveParseTreeMaps, and then
    % - adds that parse_tree_int3 to a work queue
    %
    % while independently, each of several workers execute a loop in which
    % they take a parse_tree_int3 off the work queue and invoke
    % write_parse_tree_int3 on it.
    list.map2_foldl2(
        generate_and_write_interface_file_int3(ProgressStream, Globals,
            do_add_new_to_hptm),
        BurdenedModules, _Succeededs3, SpecsList3,
        !HaveParseTreeMaps, !IO),
    list.condense(SpecsList3, Specs3),
    !:Specs = Specs3 ++ !.Specs,
    Errors3 = contains_errors(Globals, Specs3),
    (
        Errors3 = yes
    ;
        Errors3 = no,
        list.sort(Ancestors, SortedAncestors),
        assoc_list.values(SortedAncestors, AncestorBurdenedModules),
        % XXX This code could be parallelized by the same method as proposed
        % for .int3 files above, starting with a !HaveParseTreeMaps
        % containing the parse trees of all the .int3 files generated above.
        %
        % There is a complication, but it does not need a change in code.
        %
        % The complication is that the process of generating a .int0 file
        % for mod_a.mod_b.mod_c.m requires access to the parse trees of
        % for mod_a.int0 and mod_a.mod_b.int0. In our context, this means that
        % we should not invoke generate_parse_tree_int0 for a module
        % until we have already generated the parse trees of all its
        % ancestors (if any). The reason why this does not need any extra code
        % is the call to list.sort above. Ancestors is an assoc_list,
        % but each of its keys is unique, so the sort of the assoc_list
        % effectively sorts only on the keys. Each key is the list of
        % the module qualifiers in front of the base module name, followed
        % by the base module name. Since the empty lists sorts before
        % any nonempty list, any comparison of a list of module name components
        % (representing an arbitrary module name) with any initial subsequence
        % of those components (representing the arbitrary module's ancestors)
        % will put the latter first, thus guaranteeing that we process
        % ancestor modules before their descendants.
        list.map2_foldl2(
            generate_and_write_interface_file_int0(ProgressStream, Globals,
                do_add_new_to_hptm),
            AncestorBurdenedModules, _Succeededs0, RawSpecsList0,
            !HaveParseTreeMaps, !IO),
        % The code above created a .int3 file for every module in
        % BurdenedModules, but some of those modules may import modules
        % that are NOT in BurdenedModules. These modules may be in
        % other directories in which we have not yet created .int3 files.
        % If this is the case, then mention this fact in a message
        % that won't (by itself) prevent the compiler from exiting
        % with a successful exit status.
        list.condense(RawSpecsList0, RawSpecs0),
        handle_not_found_files(RawSpecs0, Specs0, Continue0),
        !:Specs = Specs0 ++ !.Specs,
        Errors0 = contains_errors(Globals, Specs0),
        ( if
            Errors0 = no,
            Continue0 = yes
        then
            % XXX This code could be parallelized by the same method as
            % proposed for .int3 files above, starting with a
            % !HaveParseTreeMaps containing the parse trees of all the
            % .int3 and .int0 files generated above.
            %
            % XXX We should teach the implementation of mmc --make
            % to use this technique of holding onto the parse trees
            % of the files it generates, to allow later actions to get
            % access to those parse trees without reading the file
            % they were written out to.
            %
            % XXX At the moment, generate_parse_tree_int12 takes some shortcuts
            % when creating type_ctor_checked_defns that would need to be fixed
            % before mmc --make reuses the parse trees it returns.
            % Those shortcuts are marked by "XXX CLEANUP".
            list.map2_foldl2(
                generate_and_write_interface_file_int1_int2(ProgressStream,
                    Globals, do_add_new_to_hptm),
                BurdenedModules, _Succeededs12, RawSpecsList12,
                !HaveParseTreeMaps, !IO),
            list.condense(RawSpecsList12, RawSpecs12),
            handle_not_found_files(RawSpecs12, Specs12, _Continue12),
            !:Specs = Specs12 ++ !.Specs
        else
            true
        )
    ).

:- pred gather_local_burdened_modules(deps::in,
    burdened_module::out,
    assoc_list(list(string), burdened_module)::in,
    assoc_list(list(string), burdened_module)::out) is semidet.

gather_local_burdened_modules(Deps, BurdenedModule, !Ancestors) :-
    Deps = deps(_HaveProcessed, MaybeDummy, BurdenedModule),
    BurdenedModule = burdened_module(_Baggage, ParseTreeModuleSrc),
    MaybeDummy = non_dummy_burdened_module,
    IncludeMap = ParseTreeModuleSrc ^ ptms_include_map,
    ( if map.is_empty(IncludeMap) then
        true
    else
        ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
        ModuleNameComponents = sym_name_to_list(ModuleName),
        !:Ancestors = [ModuleNameComponents - BurdenedModule | !.Ancestors]
    ).

:- pred handle_not_found_files(list(error_spec)::in, list(error_spec)::out,
    bool::out) is det.

handle_not_found_files(Specs0, Specs, Continue) :-
    list.foldl2(acc_not_found_files, Specs0,
        [], NotFoundFiles, [], OtherSpecs),
    (
        NotFoundFiles = [],
        Specs = OtherSpecs,
        % Continue if OtherSpecs allows it; our caller will test that.
        Continue = yes
    ;
        NotFoundFiles = [_ | _],
        list.sort(NotFoundFiles, SortedNotFoundFiles),
        list.split_upto(10, SortedNotFoundFiles, FilesToShow, FilesNotToShow),
        (
            FilesNotToShow = [],
            NotFoundPieces = [invis_order_default_end(999, ""),
                words("Could not find the following files:")] ++
                indented_list(FilesToShow)
        ;
            FilesNotToShow = [_ | _],
            NotFoundPieces = [invis_order_default_end(999, ""),
                words("Could not find many files, including these:")] ++
                indented_list(FilesToShow)
        ),
        Pieces = NotFoundPieces ++
            [words("and thus could not create some interface files."), nl],
        Spec = no_ctxt_spec($pred, severity_informational,
            phase_read_files, Pieces),
        Specs = [Spec | OtherSpecs],
        Continue = no
    ).

:- pred acc_not_found_files(error_spec::in,
    list(format_piece)::in, list(format_piece)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

acc_not_found_files(Spec, !NotFoundFiles, !OtherSpecs) :-
    extract_spec_phase(Spec, Phase),
    ( if Phase = phase_find_files(FileName) then
        !:NotFoundFiles = [fixed(FileName) | !.NotFoundFiles]
    else
        !:OtherSpecs = [Spec | !.OtherSpecs]
    ).

%---------------------%

:- pred do_process_compiler_arg_make_interface(io.text_output_stream::in,
    globals::in, op_mode_interface_file::in, file_or_module::in,
    list(list(error_spec))::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

do_process_compiler_arg_make_interface(ProgressStream, Globals0,
        InterfaceFile, FileOrModule, SpecLists, !HaveParseTreeMaps, !IO) :-
    (
        InterfaceFile = omif_int3,
        ReturnTimestamp = do_not_return_timestamp
    ;
        InterfaceFile = omif_int0,
        globals.lookup_bool_option(Globals0, generate_item_version_numbers,
            GenerateVersionNumbers),
        ReturnTimestamp =
            version_numbers_return_timestamp(GenerateVersionNumbers)
    ;
        InterfaceFile = omif_int1_int2,
        globals.lookup_bool_option(Globals0, generate_item_version_numbers,
            GenerateVersionNumbers),
        ReturnTimestamp =
            version_numbers_return_timestamp(GenerateVersionNumbers)
    ),
    read_module_or_file(ProgressStream, Globals0, Globals, FileOrModule,
        ReturnTimestamp, HaveReadSrc, !HaveParseTreeMaps, !IO),
    (
        HaveReadSrc = have_not_read_module(_FileName, ReadErrors),
        ReadSpecs = get_read_module_specs(ReadErrors),
        SpecLists = [ReadSpecs]
    ;
        HaveReadSrc = have_module(FileName, ParseTreeSrc, Source),
        have_parse_tree_source_get_maybe_timestamp_errors(Source,
            MaybeTimestamp, ReadErrors),
        ReadSpecs = get_read_module_specs(ReadErrors),
        ( if halt_at_module_error(Globals, ReadErrors) then
            SpecLists = [ReadSpecs]
        else
            parse_tree_src_to_burdened_module_list(Globals, FileName,
                ReadErrors, MaybeTimestamp, ParseTreeSrc,
                SplitSpecs, BurdenedModules),
            % parse_tree_src_to_burdened_module_list includes in SplitSpecs
            % the errors it gets from ReadErrors.
            ReadSplitSpecs0 = SplitSpecs,
            filter_interface_generation_specs(Globals, ReadSplitSpecs0,
                ReadSplitSpecs),
            (
                InterfaceFile = omif_int0,
                IsAncestor =
                    ( pred(BM::in) is semidet :-
                        BM = burdened_module(_, PTMS),
                        IncludeMap = PTMS ^ ptms_include_map,
                        not map.is_empty(IncludeMap)
                    ),
                list.filter(IsAncestor,
                    BurdenedModules, AncestorBurdenedModules),
                list.map2_foldl2(
                    generate_and_write_interface_file_int0(ProgressStream,
                        Globals0, do_not_add_new_to_hptm),
                    AncestorBurdenedModules, _Succeededs, WriteSpecsList,
                    !HaveParseTreeMaps, !IO)
            ;
                InterfaceFile = omif_int1_int2,
                list.map2_foldl2(
                    generate_and_write_interface_file_int1_int2(ProgressStream,
                        Globals0, do_not_add_new_to_hptm),
                    BurdenedModules, _Succeededs, WriteSpecsList,
                    !HaveParseTreeMaps, !IO)
            ;
                InterfaceFile = omif_int3,
                list.map2_foldl2(
                    generate_and_write_interface_file_int3(ProgressStream,
                        Globals0, do_not_add_new_to_hptm),
                    BurdenedModules, _Succeededs, WriteSpecsList,
                    !HaveParseTreeMaps, !IO)
            ),
            SpecLists = [ReadSplitSpecs | WriteSpecsList]
        )
    ).

:- func version_numbers_return_timestamp(bool) = maybe_return_timestamp.

version_numbers_return_timestamp(no) = do_not_return_timestamp.
version_numbers_return_timestamp(yes) = do_return_timestamp.

%---------------------%

:- pred find_modules_to_recompile(io.text_output_stream::in, globals::in,
    globals::out, file_or_module::in, modules_to_recompile::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

find_modules_to_recompile(ProgressStream, Globals0, Globals, FileOrModule,
        ModulesToRecompile, !HaveParseTreeMaps, !IO) :-
    globals.lookup_bool_option(Globals0, smart_recompilation, Smart0),
    io_get_disable_smart_recompilation(DisableSmart, !IO),
    (
        DisableSmart = disable_smart_recompilation,
        globals.set_option(smart_recompilation, bool(no),
            Globals0, Globals),
        Smart = no
    ;
        DisableSmart = do_not_disable_smart_recompilation,
        Globals = Globals0,
        Smart = Smart0
    ),
    (
        Smart = yes,
        % Note that `--smart-recompilation' only works with
        % `--target-code-only', which is always set when the compiler is
        % invoked by mmake. Using smart recompilation without using mmake
        % is not a sensible thing to do. handle_options.m will disable smart
        % recompilation if `--target-code-only' is not set.
        (
            FileOrModule = fm_module(ModuleName)
        ;
            FileOrModule = fm_file(FileName),
            % XXX This won't work if the module name doesn't match
            % the file name -- such modules will always be recompiled.
            %
            % This problem will be fixed when mmake functionality
            % is moved into the compiler. The file_name->module_name
            % mapping will be explicitly recorded.
            file_name_to_module_name(FileName, ModuleName)
        ),
        recompilation.check.should_recompile(ProgressStream, Globals,
            ModuleName, ModulesToRecompile, !HaveParseTreeMaps, !IO)
    ;
        Smart = no,
        ModulesToRecompile = all_modules
    ).

%---------------------------------------------------------------------------%

:- pred read_augment_and_process_module(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, op_mode_augment::in,
    op_mode_invoked_by_mmc_make::in, list(string)::in,
    file_or_module::in, modules_to_recompile::in,
    list(module_name)::out, list(string)::out, list(error_spec)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

read_augment_and_process_module(ProgressStream, ErrorStream, Globals0,
        OpModeAugment, InvokedByMmcMake, OptionArgs, FileOrModule,
        MaybeModulesToRecompile, ModulesToLink, ExtraObjFiles, Specs,
        !HaveParseTreeMaps, !IO) :-
    (
        ( OpModeAugment = opmau_make_plain_opt
        ; OpModeAugment = opmau_make_trans_opt
        ; OpModeAugment = opmau_make_analysis_registry
        ; OpModeAugment = opmau_make_xml_documentation
        )
    ;
        ( OpModeAugment = opmau_typecheck_only
        ; OpModeAugment = opmau_front_and_middle(_)
        ),
        globals.lookup_bool_option(Globals0, report_cmd_line_args_in_doterr,
            ReportCmdLineArgsDotErr),
        io.stderr_stream(StdErrStream, !IO),
        maybe_report_cmd_line(StdErrStream, ReportCmdLineArgsDotErr,
            OptionArgs, [], !IO)
    ),
    read_module_or_file(ProgressStream, Globals0, Globals, FileOrModule,
        do_return_timestamp, HaveReadSrc, !HaveParseTreeMaps, !IO),
    (
        HaveReadSrc = have_not_read_module(_, Errors),
        Specs = get_read_module_specs(Errors),
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        HaveReadSrc = have_module(SourceFileName, ParseTreeSrc, Source),
        have_parse_tree_source_get_maybe_timestamp_errors(Source,
            MaybeTimestamp, ReadModuleErrors),
        ( if halt_at_module_error(Globals, ReadModuleErrors) then
            Specs = get_read_module_specs(ReadModuleErrors),
            ModulesToLink = [],
            ExtraObjFiles = []
        else
            % XXX STREAM
            % We should test whether to go from non-module-specific
            % progress and error streams to module-specific streams.
            augment_and_process_source_file(ProgressStream, ErrorStream,
                Globals, OpModeAugment, InvokedByMmcMake, SourceFileName,
                MaybeTimestamp, ReadModuleErrors, ParseTreeSrc,
                MaybeModulesToRecompile, ModulesToLink, ExtraObjFiles,
                Specs, !HaveParseTreeMaps, !IO)
        )
    ).

%---------------------%

:- pred maybe_report_cmd_line(io.text_output_stream::in, bool::in,
    list(string)::in, list(string)::in, io::di, io::uo) is det.

maybe_report_cmd_line(OutStream, Report, OptionArgs, Args, !IO) :-
    (
        Report = no
    ;
        Report = yes,
        io.format(OutStream, "%% Command line options start\n", [], !IO),
        io.format(OutStream, "%% %s\n",
            [s(string.join_list("\n% ", OptionArgs ++ Args))], !IO),
        io.format(OutStream, "%% Command line options end\n", [], !IO)
    ).

%---------------------%

:- func string_to_file_or_module(string) = file_or_module.

string_to_file_or_module(String) = FileOrModule :-
    ( if string.remove_suffix(String, ".m", FileName) then
        % If the argument name ends in `.m', then we assume it is a file name.
        FileOrModule = fm_file(FileName)
    else
        % If it doesn't end in `.m', then we assume it is a module name.
        % (Is it worth checking that the name doesn't contain directory
        % separators, and issuing a warning or error in that case?)
        file_name_to_module_name(String, ModuleName),
        FileOrModule = fm_module(ModuleName)
    ).

:- func file_or_module_to_module_name(file_or_module) = module_name.

file_or_module_to_module_name(fm_file(FileName)) = ModuleName :-
    % Assume the module name matches the file name.
    file_name_to_module_name(FileName, ModuleName).
file_or_module_to_module_name(fm_module(ModuleName)) = ModuleName.

%---------------------------------------------------------------------------%

:- pred read_module_or_file(io.text_output_stream::in,
    globals::in, globals::out, file_or_module::in, maybe_return_timestamp::in,
    have_module(parse_tree_src)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

read_module_or_file(ProgressStream, Globals0, Globals, FileOrModuleName,
        ReturnTimestamp, HaveSrc, !HaveParseTreeMaps, !IO) :-
    globals.lookup_bool_option(Globals0, verbose, Verbose),
    (
        FileOrModuleName = fm_module(ModuleName),
        ModuleNameStr = sym_name_to_string(ModuleName),
        string.format("%% Parsing file `%s'.m and imported interfaces...\n",
            [s(ModuleNameStr)], ParsingMsg)
    ;
        FileOrModuleName = fm_file(FileName0),
        string.format("%% Parsing file `%s'.m and imported interfaces...\n",
            [s(FileName0)], ParsingMsg),
        % This is only the *default* module name, but it is the only one
        % we can use until we actually read the file.
        file_name_to_module_name(FileName0, ModuleName)
    ),
    maybe_write_string(ProgressStream, Verbose, ParsingMsg, !IO),
    ( if
        % Avoid rereading the module if it was already read
        % by recompilation.version.m.
        map.search(!.HaveParseTreeMaps ^ hptm_src, ModuleName, HaveSrc0),
        HaveSrc0 = have_module(FN, PT, Source0),
        Source0 = was_read(MaybeTimestamp0, E),
        return_timestamp_if_needed(ReturnTimestamp,
            MaybeTimestamp0, MaybeTimestamp),
        Source = was_read(MaybeTimestamp, E),
        HaveSrc1 = have_module(FN, PT, Source)
    then
        Globals = Globals0,
        HaveSrc = HaveSrc1,
        % XXX When we have read the module before, it *could* have had
        % problems that should cause smart recompilation to be disabled.
        HaveReadModuleMapSrc0 = !.HaveParseTreeMaps ^ hptm_src,
        map.delete(ModuleName, HaveReadModuleMapSrc0, HaveReadModuleMapSrc),
        !HaveParseTreeMaps ^ hptm_src := HaveReadModuleMapSrc
    else
        % We don't search `--search-directories' for source files
        % because that can result in the generated interface files
        % being created in the wrong directory.
        (
            FileOrModuleName = fm_module(_),
            read_module_src(ProgressStream, Globals0, rrm_std,
                do_not_ignore_errors, do_not_search, ModuleName, [],
                always_read_module(ReturnTimestamp), HaveReadSrc, !IO)
        ;
            FileOrModuleName = fm_file(FileName),
            FileNameDotM = FileName ++ ".m",
            read_module_src_from_file(ProgressStream, Globals0,
                FileName, FileNameDotM, rrm_file, do_not_search,
                always_read_module(ReturnTimestamp), HaveReadSrc, !IO)
        ),
        HaveSrc = coerce(HaveReadSrc),
        io_get_disable_smart_recompilation(DisableSmart, !IO),
        (
            DisableSmart = disable_smart_recompilation,
            globals.set_option(smart_recompilation, bool(no),
                Globals0, Globals)
        ;
            DisableSmart = do_not_disable_smart_recompilation,
            Globals = Globals0
        )
    ),
    globals.lookup_bool_option(Globals, detailed_statistics, Stats),
    maybe_report_stats(ProgressStream, Stats, !IO).

%---------------------------------------------------------------------------%

:- pred halt_at_module_error(globals::in, read_module_errors::in) is semidet.

halt_at_module_error(Globals, Errors) :-
    (
        set.is_non_empty(Errors ^ rm_fatal_errors)
    ;
        set.is_non_empty(Errors ^ rm_nonfatal_errors),
        globals.lookup_bool_option(Globals, halt_at_syntax_errors, HaltSyntax),
        HaltSyntax = yes
    ).

%---------------------------------------------------------------------------%

:- pred gc_init(io::di, io::uo) is det.

% This version is only used if there is no matching foreign_proc version.
gc_init(!IO).

:- pragma foreign_proc("C",
    gc_init(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
#ifdef MR_BOEHM_GC
    // Explicitly force the initial heap size to be at least 4 Mb.
    //
    // This works around a bug in the Boehm collector (for versions up
    // to at least 6.2) where the collector would sometimes abort with
    // the message `unexpected mark stack overflow' (e.g. in grade hlc.gc
    // on dec-alpha-osf3.2).
    //
    // Doing this should also improve performance slightly by avoiding
    // frequent garbage collection during start-up.
    GC_expand_hp(4 * 1024 * 1024);
#endif
").

%---------------------------------------------------------------------------%
:- end_module top_level.mercury_compile_main.
%---------------------------------------------------------------------------%
