%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mercury_compile_main.m.
% Main authors: fjh, zs.
%
% This is the top-level of the Mercury compiler.
%
% This module invokes the different passes of the compiler as appropriate.
% The constraints on pass ordering are documented in
% compiler/notes/compiler_design.html.
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

:- import_module analysis.
:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module backend_libs.export.
:- import_module check_hlds.
:- import_module check_hlds.xml_documentation.
:- import_module hlds.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module hlds.make_hlds.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.passes_aux.
:- import_module libs.check_libgrades.
:- import_module libs.compute_grade.
:- import_module libs.file_util.
:- import_module libs.handle_options.
:- import_module libs.maybe_util.
:- import_module libs.op_mode.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module make.
:- import_module make.build.
:- import_module make.deps_cache.
:- import_module make.options_file.
:- import_module make.top_level.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.shared_utilities.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.check_module_interface.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.generate_dep_d_files.
:- import_module parse_tree.grab_modules.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.
:- import_module parse_tree.source_file_map.
:- import_module parse_tree.write_deps_file.
:- import_module parse_tree.write_error_spec.
:- import_module parse_tree.write_module_interface_files.
:- import_module recompilation.
:- import_module recompilation.check.
:- import_module recompilation.usage.
:- import_module recompilation.used_file.
:- import_module top_level.mercury_compile_front_end.
:- import_module top_level.mercury_compile_llds_back_end.
:- import_module top_level.mercury_compile_make_hlds.
:- import_module top_level.mercury_compile_middle_passes.
:- import_module top_level.mercury_compile_mlds_back_end.

:- import_module assoc_list.
:- import_module benchmarking.
:- import_module bool.
:- import_module cord.
:- import_module dir.
:- import_module gc.
:- import_module getopt.
:- import_module io.environment.
:- import_module io.file.
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
    io.command_line_arguments(CmdLineArgs, !IO),

    trace [
        compile_time(flag("cmd_line_args")),
        run_time(env("MMC_CMD_LINE_ARGS")),
        io(!TIO)]
    (
        dump_args(ErrorStream, "REAL_MAIN", CmdLineArgs, !TIO)
    ),

    unlimit_stack(!IO),

    % Replace all @file arguments with the contents of the file.
    expand_at_file_arguments(CmdLineArgs, Res, !IO),
    (
        Res = ok(ExpandedCmdLineArgs),
        real_main_after_expansion(ProgressStream, ErrorStream,
            ExpandedCmdLineArgs, !IO)
    ;
        Res = error(E),
        io.format(ProgressStream, "%s\n", [s(io.error_message(E))], !IO),
        io.set_exit_status(1, !IO)
    ),
    record_make_deps_cache_stats(!IO),
    record_write_deps_file_cache_stats(!IO),
    record_instmap_delta_restrict_stats(!IO),
    close_any_specific_compiler_streams(!IO).

    % Expand @File arguments.
    % Each argument in the above form is replaced with a list of arguments
    % where each arg is each line in the file File which is not just
    % whitespace.
    %
:- pred expand_at_file_arguments(list(string)::in, io.res(list(string))::out,
    io::di, io::uo) is det.

expand_at_file_arguments([], ok([]), !IO).
expand_at_file_arguments([Arg | Args], Result, !IO) :-
    ( if string.remove_prefix("@", Arg, File) then
        io.open_input(File, OpenRes, !IO),
        (
            OpenRes = ok(S),
            expand_file_into_arg_list(S, ReadRes, !IO),
            (
                ReadRes = ok(FileArgs),
                expand_at_file_arguments(FileArgs ++ Args, Result, !IO)
            ;
                ReadRes = error(E),
                Result = error(at_file_error(File, E))
            )
        ;
            OpenRes = error(_E),
            Msg = "mercury_compile: cannot open '" ++ File ++ "'",
            Result = error(io.make_io_error(Msg))
        )
    else
        expand_at_file_arguments(Args, Result0, !IO),
        (
            Result0 = ok(ExpandedArgs),
            Result = ok([Arg | ExpandedArgs])
        ;
            Result0 = error(E),
            Result = error(E)
        )
    ).

:- func at_file_error(string, io.error) = io.error.

at_file_error(File, E) =
    io.make_io_error("While attempting to process '" ++ File ++
        "' the following error occurred: " ++ io.error_message(E)).

    % Read each of the command line arguments from the given input file.
    % Note lines which consist purely of whitespace are ignored.
    %
:- pred expand_file_into_arg_list(io.text_input_stream::in,
    io.res(list(string))::out, io::di, io::uo) is det.

expand_file_into_arg_list(S, Res, !IO) :-
    io.read_line_as_string(S, LineRes, !IO),
    (
        LineRes = ok(Line),
        expand_file_into_arg_list(S, Res0, !IO),
        (
            Res0 = ok(Lines),
            StrippedLine = strip(Line),
            ( if StrippedLine = "" then
                Res = ok(Lines)
            else
                Res = ok([StrippedLine | Lines])
            )
        ;
            Res0 = error(_E),
            Res = Res0
        )
    ;
        LineRes = eof,
        Res = ok([])
    ;
        LineRes = error(E),
        Res = error(E)
    ).

%---------------------------------------------------------------------------%

:- type option_processing_result
    --->    opr_failure(
                oprf_fatal_errors           :: list(error_spec)
            )
    ;       opr_success(
                oprs_detected_grade_flags   :: list(string),
                oprs_options_variables      :: options_variables,
                oprs_mcflags                :: list(string),
                oprs_option_args            :: list(string),
                oprs_nonoption_args         :: list(string),
                oprs_nonfatal_errors        :: list(error_spec)
            ).

:- pred real_main_after_expansion(io.text_output_stream::in,
    io.text_output_stream::in, list(string)::in, io::di, io::uo) is det.

real_main_after_expansion(ProgressStream, ErrorStream, CmdLineArgs, !IO) :-
    getopt.init_option_table(option_defaults, DefaultOptionTable),
    ( if CmdLineArgs = ["--arg-file", ArgFile | ExtraArgs] then
        % Diagnose bad invocations, e.g. shell redirection operators treated
        % as command line arguments.
        (
            ExtraArgs = []
        ;
            ExtraArgs = [_ | _],
            unexpected($pred,
                "extra arguments with --arg-file: " ++ string(ExtraArgs))
        ),
        process_options_arg_file(ProgressStream, DefaultOptionTable,
            ArgFile, Result, !IO)
    else
        process_options_std(ProgressStream, ErrorStream, DefaultOptionTable,
            CmdLineArgs, Result, !IO)
    ),
    (
        Result = opr_success(DetectedGradeFlags, OptionsVariables, MCFlags,
            OptionArgs, NonOptionArgs, OptionSpecs),

        % NOTE: The order of the flags here must be:
        %
        %   (1) flags for detected library grades
        %   (2) flags from Mercury.config and any Mercury.options files
        %   (3) flags from any command line options
        %
        % The order is important, because flags given later in this list
        % can override those given earlier.
        %
        % XXX The relationship between --no-libgrade or --libgrade options set
        % via the DEFAULT_MCFLAGS variable and detected library grades is
        % currently not defined. It does not matter at the moment, since
        % Mercury.config does not contain either of those two flags.
        AllFlags = DetectedGradeFlags ++ MCFlags ++ OptionArgs,
        trace [
            compile_time(flag("cmd_line_args")),
            run_time(env("MMC_CMD_LINE_ARGS")),
            io(!TIO)]
        (
            dump_args(ErrorStream, "AllFlags", AllFlags, !TIO)
        ),
        handle_given_options(ProgressStream, DefaultOptionTable, AllFlags,
            _, _, Specs, ActualGlobals, !IO),

        % Now that we have constructed a globals, print out any errors and/or
        % warnings generated by the predicates in options_file.m.
        write_error_specs(ErrorStream, ActualGlobals, OptionSpecs, !IO),

        % When computing the option arguments to pass to `--make', only include
        % the command-line arguments, not the contents of DEFAULT_MCFLAGS.
        (
            Specs = [_ | _],
            usage_errors(ProgressStream, ActualGlobals, Specs, !IO)
        ;
            Specs = [],
            main_after_setup(ProgressStream, ErrorStream, ActualGlobals,
                DetectedGradeFlags, OptionsVariables,
                OptionArgs, NonOptionArgs, !IO),
            trace [compile_time(flag("file_name_translations")), io(!TIO)] (
                write_translations_record_if_any(ActualGlobals, !TIO)
            )
        )
    ;
        Result = opr_failure(OptionSpecs),
        % Usually, any error_specs we write out here were generated
        % in options_file.m.
        write_error_specs_opt_table(ErrorStream, DefaultOptionTable,
            OptionSpecs, !IO),
        io.set_exit_status(1, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred process_options_arg_file(io.text_output_stream::in, option_table::in,
    string::in, option_processing_result::out, io::di, io::uo) is det.

process_options_arg_file(ProgressStream, DefaultOptionTable, ArgFile,
        Result, !IO) :-
    io.environment.get_environment_var_map(EnvVarMap, !IO),
    % All the configuration and options file options are passed in the
    % given file, which is created by the parent `mmc --make' process.
    % (make.module_target does this to overcome limits on the lengths
    % of command lines on Windows.) The environment is ignored, unlike
    % with @file syntax.
    read_args_file(ProgressStream, ArgFile, MaybeArgs1,
        ArgsNonUndefSpecs, ArgsUndefSpecs, !IO),
    % Since the args file is supposed to be generated by the parent
    % `mmc --make' process, there shouldn't be any references to
    % undefined make variables in it. If there are, that process has
    % screwed up. We can't fix that bug unless the bug is reported,
    % which requires printing the error messages that it yields.
    Specs = ArgsNonUndefSpecs ++ ArgsUndefSpecs,
    DetectedGradeFlags = [],
    OptionsVariables = options_variables_init(EnvVarMap),
    (
        MaybeArgs1 = yes(Args1),
        % Separate the option args from the non-option args.
        getopt.record_arguments(short_option, long_option, DefaultOptionTable,
            Args1, NonOptionArgs, OptionArgs, MaybeError, _OptionValues),
        (
            MaybeError = found_option_error(OptionError),
            OptionErrorStr = option_error_to_string(OptionError),
            Spec = simplest_no_context_spec($pred, severity_error,
                phase_options, [words(OptionErrorStr), suffix("."), nl]),
            Result = opr_failure([Spec])
        ;
            MaybeError = no_option_error,
            Result = opr_success(DetectedGradeFlags, OptionsVariables, [],
                OptionArgs, NonOptionArgs, Specs)
        )
    ;
        MaybeArgs1 = no,
        OptionArgs = [],
        NonOptionArgs = [],
        Result = opr_success(DetectedGradeFlags, OptionsVariables, [],
            OptionArgs, NonOptionArgs, Specs)
    ).

:- pred process_options_std(io.text_output_stream::in,
    io.text_output_stream::in, option_table::in, list(string)::in,
    option_processing_result::out, io::di, io::uo) is det.

process_options_std(ProgressStream, ErrorStream, DefaultOptionTable,
        CmdLineArgs, Result, !IO) :-
    % Find out which options files to read.
    % Don't report errors yet, as the errors may no longer exist
    % after we have read in options files.

    OptionOps =
        option_ops_userdata(short_option, long_option, special_handler),
    getopt.process_options_userdata_io(OptionOps, CmdLineArgs,
        OptionArgs, NonOptionArgs, MaybeError, _OptionsSet,
        DefaultOptionTable, ArgsOptionTable, cord.init, _UserData, !IO),
    (
        MaybeError = yes(OptionError),
        OptionErrorStr = option_error_to_string(OptionError),
        Spec = simplest_no_context_spec($pred, severity_error,
            phase_options, [words(OptionErrorStr), suffix("."), nl]),
        Result = opr_failure([Spec])
    ;
        MaybeError = no,
        getopt.lookup_accumulating_option(ArgsOptionTable,
            options_search_directories, OptionSearchDirectories),
        getopt.lookup_accumulating_option(ArgsOptionTable,
            options_files, OptionsFiles),
        read_options_files_named_in_options_file_option(ProgressStream,
            OptionSearchDirectories, OptionsFiles, OptionsVariables0,
            OptFileNonUndefSpecs, OptFileUndefSpecs, !IO),
        getopt.lookup_bool_option(ArgsOptionTable,
            warn_undefined_options_variables, WarnUndef),
        (
            WarnUndef = no,
            OptFileSpecs = OptFileNonUndefSpecs
        ;
            WarnUndef = yes,
            OptFileSpecs = OptFileNonUndefSpecs ++ OptFileUndefSpecs
        ),
        io.environment.get_environment_var_map(EnvVarMap, !IO),
        (
            OptFileSpecs = [],
            maybe_dump_options_file(ErrorStream, ArgsOptionTable,
                OptionsVariables0, !IO),
            lookup_mmc_options(OptionsVariables0, MaybeMCFlags0),
            (
                MaybeMCFlags0 = error1(Specs),
                Result = opr_failure(Specs)
            ;
                MaybeMCFlags0 = ok1(MCFlags0),
                trace [
                    compile_time(flag("cmd_line_args")),
                    run_time(env("MMC_CMD_LINE_ARGS")),
                    io(!TIO)]
                (
                    dump_args(ErrorStream, "MCFlags0", MCFlags0, !TIO),
                    dump_args(ErrorStream, "CmdLineArgs", CmdLineArgs, !TIO)
                ),
                % Process the options again to find out which configuration
                % file to read.
                % XXX HANDLE_OPTIONS Ignoring _MaybeError seems a bit careless.
                getopt.process_options_userdata_io(OptionOps,
                    MCFlags0 ++ CmdLineArgs, _OptionArgsMC, _NonOptionArgsMC,
                    _MaybeErrorMC, _OptionsSetMC,
                    DefaultOptionTable, FlagsArgsOptionTable,
                    cord.init, _UserDataMC, !IO),
                process_options_std_config_file(ProgressStream,
                    FlagsArgsOptionTable, EnvVarMap, WarnUndef,
                    DetectedGradeFlags, OptionsVariables0, OptionsVariables,
                    MaybeMCFlags, OptFileOkSpecs, !IO),
                Specs = OptFileSpecs ++ OptFileOkSpecs,
                (
                    MaybeMCFlags = no,
                    Result = opr_failure(Specs)
                ;
                    MaybeMCFlags = yes(MCFlags),
                    Result = opr_success(DetectedGradeFlags, OptionsVariables,
                        MCFlags, OptionArgs, NonOptionArgs, Specs)
                )
            )
        ;
            OptFileSpecs = [_ | _],
            Result = opr_failure(OptFileSpecs)
        )
    ).

:- pred process_options_std_config_file(io.text_output_stream::in,
    option_table::in, environment_var_map::in, bool::in, list(string)::out,
    options_variables::in, options_variables::out,
    maybe(list(string))::out, list(error_spec)::out, io::di, io::uo) is det.

process_options_std_config_file(ProgressStream, FlagsArgsOptionTable,
        EnvVarMap, WarnUndef, DetectedGradeFlags,
        OptionsVariables0, OptionsVariables, MaybeMCFlags, Specs, !IO) :-
    getopt.lookup_maybe_string_option(FlagsArgsOptionTable, config_file,
        MaybeConfigFile0),
    % The meanings of the possible values of MaybeConfigFile0 are as follows.
    %
    % - A value of yes("") means either that the the command line had no
    %   --config-file option (since yes("") is the default for this option),
    %   or that the command line contained --config-file ''.
    %   The latter is *extremely* unlikely.
    %
    % - A value of yes(ConfigFile0) where ConfigFile0 != "" means that
    %   the command line had a meaningful --config-file option.
    %
    % - A value of no means that the command line contained --no-config-file.
    %
    % The next if-then-else replaces yes("") with one or the other
    % of the other two options.
    ( if MaybeConfigFile0 = yes("") then
        getopt.lookup_maybe_string_option(FlagsArgsOptionTable,
            mercury_configuration_directory, MaybeConfDir),
        (
            MaybeConfDir = yes(ConfDir),
            MaybeConfigFile = yes(ConfDir/"conf"/"Mercury.config")
        ;
            MaybeConfDir = no,
            MaybeConfigFile = no
        )
    else
        MaybeConfigFile = MaybeConfigFile0
    ),
    (
        MaybeConfigFile = yes(ConfigFile),
        read_named_options_file(ProgressStream, ConfigFile,
            OptionsVariables0, OptionsVariables,
            ConfigNonUndefSpecs, ConfigUndefSpecs, !IO),
        % All entries in ConfigNonUndefSpecs are unconditionally errors.
        % All entries in ConfigUndefSpecs are unconditionally warnings.
        (
            WarnUndef = no,
            ConfigSpecs = ConfigNonUndefSpecs
        ;
            WarnUndef = yes,
            ConfigSpecs = ConfigNonUndefSpecs ++ ConfigUndefSpecs
        ),
        (
            ConfigNonUndefSpecs = [],
            lookup_mmc_options(OptionsVariables, MaybeMCFlags1),
            (
                MaybeMCFlags1 = ok1(MCFlags1),
                MaybeMCFlags = yes(MCFlags1),
                Specs0 = ConfigSpecs
            ;
                MaybeMCFlags1 = error1(MCFlagsSpecs),
                % All error_specs in MCFlagsSpecs are errors, not warnings.
                MaybeMCFlags = no,
                Specs0 = ConfigSpecs ++ MCFlagsSpecs
            ),
            % XXX Record _StdLibGrades in the final globals structure.
            % XXX Or even better, do this call only when we have finished
            % *all* option processing, and have the *final* globals structure.
            maybe_detect_stdlib_grades(FlagsArgsOptionTable, OptionsVariables,
                _MaybeStdLibGrades, DetectedGradeFlags, !IO),
            % maybe_detect_stdlib_grades does this lookup, but it returns
            % any error in MaybeConfigMerStdLibDir (as part of
            % _MaybeStdLibGrades) only if that error is relevant,
            % i.e. if we cannot get the location of the Mercury standard
            % library from the mercury_standard_library_directory option.
            % Including such irrelevant errors in Specs preserves old behavior,
            % though I (zs) do not understand the reason for that behavior.
            lookup_mercury_stdlib_dir(OptionsVariables,
                MaybeConfigMerStdLibDir),
            Specs = Specs0 ++ get_any_errors1(MaybeConfigMerStdLibDir)
        ;
            ConfigNonUndefSpecs = [_ | _],
            DetectedGradeFlags = [],
            MaybeMCFlags = no,
            Specs = ConfigSpecs
        )
    ;
        MaybeConfigFile = no,
        DetectedGradeFlags = [],
        OptionsVariables = options_variables_init(EnvVarMap),
        lookup_mmc_options(OptionsVariables, MaybeMCFlags1),
        (
            MaybeMCFlags1 = ok1(MCFlags1),
            MaybeMCFlags = yes(MCFlags1),
            Specs = []
        ;
            MaybeMCFlags1 = error1(MCFlagsSpecs),
            % All error_specs in MCFlagsSpecs are errors, not warnings.
            MaybeMCFlags = no,
            Specs = MCFlagsSpecs
        )
    ).

:- pred maybe_dump_options_file(io.text_output_stream::in, option_table::in,
    options_variables::in, io::di, io::uo) is det.

maybe_dump_options_file(OutStream, ArgsOptionTable, OptionsVariables, !IO) :-
    lookup_string_option(ArgsOptionTable, dump_options_file, DumpOptionsFile),
    ( if DumpOptionsFile = "" then
        true
    else
        dump_options_file(OutStream, DumpOptionsFile, OptionsVariables, !IO)
    ).

%---------------------------------------------------------------------------%

main_for_make(ProgressStream, ErrorStream, Globals, Args, !IO) :-
    DetectedGradeFlags = [],
    io.environment.get_environment_var_map(EnvVarMap, !IO),
    OptionsVariables = options_variables_init(EnvVarMap),
    OptionArgs = [],
    main_after_setup(ProgressStream, ErrorStream, Globals, DetectedGradeFlags,
        OptionsVariables, OptionArgs, Args, !IO).

%---------------------------------------------------------------------------%

:- pred main_after_setup(io.text_output_stream::in, io.text_output_stream::in,
    globals::in, list(string)::in, options_variables::in,
    list(string)::in, list(string)::in, io::di, io::uo) is det.

main_after_setup(ProgressStream, ErrorStream, Globals, DetectedGradeFlags,
        OptionVariables, OptionArgs, Args, !IO) :-
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
            DetectedGradeFlags, OptionVariables, OptionArgs, Args,
            HaveParseTreeMaps0, _HaveParseTreeMaps, Specs0, Specs, !IO),
        write_error_specs(ErrorStream, Globals, Specs, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred do_op_mode(io.text_output_stream::in, io.text_output_stream::in,
    globals::in, op_mode::in, list(string)::in, options_variables::in,
    list(string)::in, list(string)::in,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

do_op_mode(ProgressStream, ErrorStream, Globals, OpMode, DetectedGradeFlags,
        OptionVariables, OptionArgs, Args, !HaveParseTreeMaps, !Specs, !IO) :-
    (
        OpMode = opm_top_make,
        % make_process_compiler_args itself does not pay attention to the
        % value of filenames_from_stdin, but we definitely should not let it
        % pass filenames_from_stdin=yes to any subcompilations.
        globals.set_option(filenames_from_stdin, bool(no),
            Globals, MakeGlobals),
        make_process_compiler_args(ProgressStream, MakeGlobals,
            DetectedGradeFlags, OptionVariables, OptionArgs, Args, !IO)
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
        do_op_mode_query(ErrorStream, Globals, OpModeQuery,
            OptionVariables, !IO)
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
            do_op_mode_args(ProgressStream, ErrorStream, Globals,
                OpModeArgs, InvokedByMmcMake, FileNamesFromStdin,
                DetectedGradeFlags, OptionVariables, OptionArgs, Args,
                !HaveParseTreeMaps, !Specs, !IO)
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
        Spec = simplest_no_context_spec($pred, severity_error, phase_options,
            Pieces),
        write_error_spec(ErrorStream, Globals, Spec, !IO)
    ;
        Target = target_c,
        make_standalone_interface(Globals, ProgressStream,
            StandaloneIntBasename, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred do_op_mode_query(io.text_output_stream::in, globals::in,
    op_mode_query::in, options_variables::in, io::di, io::uo) is det.

do_op_mode_query(ErrorStream, Globals, OpModeQuery, OptionVariables, !IO) :-
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
        output_c_compiler_flags(Globals, StdOutStream, !IO),
        io.nl(StdOutStream, !IO)
    ;
        OpModeQuery = opmq_output_c_include_directory_flags,
        output_c_include_directory_flags(Globals, StdOutStream, !IO)
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
        get_java_dir_path(Globals, ext_cur_ngs_gs_java_class, ClassDirNames),
        ClassDirName = dir.relative_path_name_from_components(ClassDirNames),
        io.print_line(StdOutStream, ClassDirName, !IO)
    ;
        OpModeQuery = opmq_output_grade_defines,
        output_c_grade_defines(Globals, StdOutStream, !IO)
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
        output_library_link_flags(Globals, StdOutStream, !IO)
    ;
        OpModeQuery = opmq_output_grade_string,
        % When Mmake asks for the grade, it really wants the directory
        % component to use. This is consistent with scripts/canonical_grade.
        grade_directory_component(Globals, Grade),
        io.print_line(StdOutStream, Grade, !IO)
    ;
        OpModeQuery = opmq_output_libgrades,
        globals.lookup_accumulating_option(Globals, libgrades, LibGrades),
        list.foldl(io.print_line(StdOutStream), LibGrades, !IO)
    ;
        OpModeQuery = opmq_output_stdlib_grades,
        globals.get_options(Globals, OptionTable),
        find_mercury_stdlib(OptionTable, OptionVariables,
            MaybeMerStdLibDir, !IO),
        (
            MaybeMerStdLibDir = ok1(MerStdLibDir),
            do_detect_libgrades(MerStdLibDir, StdlibGrades, !IO),
            set.fold(io.print_line(StdOutStream), StdlibGrades, !IO)
        ;
            MaybeMerStdLibDir = error1(Specs),
            write_error_specs(ErrorStream, Globals, Specs, !IO)
        )
    ;
        OpModeQuery = opmq_output_stdlib_modules,
        GetStdlibModules =
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
        solutions.solutions(GetStdlibModules, StdlibLines),
        list.foldl(io.write_string(StdOutStream), StdlibLines, !IO)
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
    bool::in, list(string)::in, options_variables::in,
    list(string)::in, list(string)::in,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

do_op_mode_args(ProgressStream, ErrorStream, Globals,
        OpModeArgs, InvokedByMmcMake, FileNamesFromStdin, DetectedGradeFlags,
        OptionVariables, OptionArgs, Args, !HaveParseTreeMaps, !Specs, !IO) :-
    (
        InvokedByMmcMake = op_mode_not_invoked_by_mmc_make,
        % We used to do this check once per compiler arg, which was
        % quite wasteful. However, there was an almost-justifiable
        % reason for that. The check used to be done in the now-deleted
        % predicate process_compiler_arg_build, the part of what is now
        % setup_and_process_compiler_arg that happens after the setup.
        % This meant that maybe_check_libraries_are_installed was called
        % not with the original globals (which is Globals here), but with
        % the globals created by setup_for_build_with_module_options.
        % However, the only options in the globals structure that
        % maybe_check_libraries_are_installed pays attention to are
        %
        % - global settings, such as libgrade_install_check and
        %   mercury_libraries, and
        %
        % - grade options.
        %
        % No sensible Mercury.options file will contain options that
        % touch the value of any option in either of those categories.
        % It shouldn't be able to touch the values of the relevant options
        % in the first category, because they don't have any names which
        % would allow them to be specified. They *could* touch the values
        % of grade options, but the result will be a program whose modules
        % can't be linked together due to incompatible grades.
        % (XXX It would be nice if we detected such errors in Mercury.options
        % files *without* letting the issue through to the linker.)
        %
        % We do this check only when not invoked by mmc --make, because during
        % compiler invocations that *set* --invoked-by-mmc-make,
        % make_linked_target in make.program_target.m should have
        % done it already.
        maybe_check_libraries_are_installed(Globals, LibgradeCheckSpecs, !IO)
    ;
        InvokedByMmcMake = op_mode_invoked_by_mmc_make,
        LibgradeCheckSpecs = []
    ),
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
                DetectedGradeFlags, OptionVariables, OptionArgs,
                cord.empty, ModulesToLinkCord, cord.empty, ExtraObjFilesCord,
                !HaveParseTreeMaps, !Specs, !IO)
        ;
            FileNamesFromStdin = no,
            (
                InvokedByMmcMake = op_mode_not_invoked_by_mmc_make,
                setup_and_process_compiler_cmd_line_args(ProgressStream,
                    ErrorStream, Globals, OpModeArgs, InvokedByMmcMake,
                    DetectedGradeFlags, OptionVariables, OptionArgs, Args,
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
        ModulesToLink = cord.list(ModulesToLinkCord),
        ExtraObjFiles = cord.list(ExtraObjFilesCord)
    ;
        LibgradeCheckSpecs = [_ | _],
        !:Specs = LibgradeCheckSpecs ++ !.Specs,
        ModulesToLink = [],
        ExtraObjFiles = []
    ),

    io.get_exit_status(ExitStatus, !IO),
    ( if ExitStatus = 0 then
        ( if
            OpModeArgs = opma_augment(opmau_generate_code(
                opmcg_target_object_and_executable)),
            ModulesToLink = [FirstModule | _]
        then
            file_name_to_module_name(FirstModule, MainModuleName),
            globals.get_target(Globals, Target),
            (
                Target = target_java,
                % For Java, at the "link" step we just generate a shell script;
                % the actual linking will be done at runtime by
                % the Java interpreter.
                create_java_shell_script(ProgressStream, Globals,
                    MainModuleName, Succeeded, !IO)
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
                    link_module_list(ProgressStream, ModulesToLink,
                        ExtraObjFiles, Globals, Succeeded, !IO)
                ;
                    InvokedByMmcMake = op_mode_not_invoked_by_mmc_make,
                    get_default_options(Globals, DefaultOptionTable),
                    setup_for_build_with_module_options(ProgressStream,
                        DefaultOptionTable, not_invoked_by_mmc_make,
                        MainModuleName, DetectedGradeFlags,
                        OptionVariables, OptionArgs, [], MayBuild, !IO),
                    (
                        MayBuild = may_not_build(SetupSpecs),
                        write_error_specs(ErrorStream, Globals,
                            SetupSpecs, !IO),
                        Succeeded = did_not_succeed
                    ;
                        MayBuild = may_build(_AllOptionArgs, BuildGlobals),
                        link_module_list(ProgressStream, ModulesToLink,
                            ExtraObjFiles, BuildGlobals, Succeeded, !IO)
                    )
                )
            ),
            maybe_set_exit_status(Succeeded, !IO)
        else
            true
        )
    else
        true
    ),
    % This output is not specific to any module.
    % NOTE I (zs) am not sure whether there *can* be any delayed error messages
    % at this point.
    io.stderr_stream(StdErr, !IO),
    maybe_print_delayed_error_messages(StdErr, Globals, !IO),
    globals.lookup_bool_option(Globals, statistics, Statistics),
    (
        Statistics = yes,
        benchmarking.report_full_memory_stats(StdErr, !IO)
    ;
        Statistics = no
    ).

%---------------------------------------------------------------------------%

:- pred setup_and_process_compiler_stdin_args(io.text_output_stream::in,
    io.text_output_stream::in, io.text_input_stream::in, globals::in,
    op_mode_args::in, op_mode_invoked_by_mmc_make::in, list(string)::in,
    options_variables::in, list(string)::in,
    cord(string)::in, cord(string)::out,
    cord(string)::in, cord(string)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

setup_and_process_compiler_stdin_args(ProgressStream, ErrorStream, StdIn,
        Globals, OpModeArgs, InvokedByMmcMake, DetectedGradeFlags,
        OptionVariables, OptionArgs, !Modules, !ExtraObjFiles,
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
            OpModeArgs, InvokedByMmcMake, DetectedGradeFlags, OptionVariables,
            OptionArgs, Arg, ArgModules, ArgExtraObjFiles,
            !HaveParseTreeMaps, !Specs, !IO),
        !:Modules = !.Modules ++ cord.from_list(ArgModules),
        !:ExtraObjFiles = !.ExtraObjFiles ++ cord.from_list(ArgExtraObjFiles),
        setup_and_process_compiler_stdin_args(ProgressStream, ErrorStream,
            StdIn, Globals, OpModeArgs, InvokedByMmcMake, DetectedGradeFlags,
            OptionVariables, OptionArgs, !Modules, !ExtraObjFiles,
            !HaveParseTreeMaps, !Specs, !IO)
    ;
        LineResult = eof
    ;
        LineResult = error(Error),
        io.error_message(Error, Msg),
        Pieces = [words("Error reading module name from standard input:"),
            words(Msg), suffix("."), nl],
        Spec = simplest_no_context_spec($pred, severity_error,
            phase_read_files, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------%

:- pred setup_and_process_compiler_cmd_line_args(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    op_mode_args::in, op_mode_invoked_by_mmc_make::in,
    list(string)::in, options_variables::in,
    list(string)::in, list(string)::in,
    cord(string)::in, cord(string)::out, cord(string)::in, cord(string)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

setup_and_process_compiler_cmd_line_args(_, _, _, _, _, _, _, _, [],
        !Modules, !ExtraObjFiles, !HaveParseTreeMaps, !Specs, !IO).
setup_and_process_compiler_cmd_line_args(ProgressStream, ErrorStream, Globals,
        OpModeArgs, InvokedByMmcMake, DetectedGradeFlags,
        OptionVariables, OptionArgs, [Arg | Args], !Modules, !ExtraObjFiles,
        !HaveParseTreeMaps, !Specs, !IO) :-
    setup_and_process_compiler_arg(ProgressStream, ErrorStream, Globals,
        OpModeArgs, InvokedByMmcMake, DetectedGradeFlags,
        OptionVariables, OptionArgs, Arg, ArgModules, ArgExtraObjFiles,
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
        Globals, OpModeArgs, InvokedByMmcMake, DetectedGradeFlags,
        OptionVariables, OptionArgs, Args, !Modules, !ExtraObjFiles,
        !HaveParseTreeMaps, !Specs, !IO).

:- pred do_process_compiler_cmd_line_args(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    op_mode_args::in, op_mode_invoked_by_mmc_make::in,
    list(string)::in, list(string)::in,
    cord(string)::in, cord(string)::out, cord(string)::in, cord(string)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

do_process_compiler_cmd_line_args(_, _, _, _, _, _, [],
        !Modules, !ExtraObjFiles, !HaveParseTreeMaps, !IO).
do_process_compiler_cmd_line_args(ProgressStream, ErrorStream, Globals,
        OpModeArgs, InvokedByMmcMake, OptionArgs, [Arg | Args],
        !Modules, !ExtraObjFiles, !HaveParseTreeMaps, !IO) :-
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
    !:Modules = !.Modules ++ cord.from_list(ArgModules),
    !:ExtraObjFiles = !.ExtraObjFiles ++ cord.from_list(ArgExtraObjFiles),
    do_process_compiler_cmd_line_args(ProgressStream, ErrorStream, Globals,
        OpModeArgs, InvokedByMmcMake, OptionArgs, Args,
        !Modules, !ExtraObjFiles, !HaveParseTreeMaps, !IO).

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
    list(string)::in, options_variables::in, list(string)::in, string::in,
    list(string)::out, list(string)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

setup_and_process_compiler_arg(ProgressStream, ErrorStream, Globals,
        OpModeArgs, InvokedByMmcMake, DetectedGradeFlags,
        OptionVariables, OptionArgs, Arg, ModulesToLink, ExtraObjFiles,
        !HaveParseTreeMaps, !Specs, !IO) :-
    get_default_options(Globals, DefaultOptionTable),
    FileOrModule = string_to_file_or_module(Arg),
    ModuleName = file_or_module_to_module_name(FileOrModule),
    ExtraOptions = [],
    setup_for_build_with_module_options(ProgressStream, DefaultOptionTable,
        not_invoked_by_mmc_make, ModuleName, DetectedGradeFlags,
        OptionVariables, OptionArgs, ExtraOptions, MayBuild, !IO),
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
    op_mode_args::in, op_mode_invoked_by_mmc_make::in,
    list(string)::in, file_or_module::in, list(string)::out, list(string)::out,
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
        (
            FileOrModule = fm_file(FileName),
            generate_dep_file_for_file(ProgressStream, Globals0, FileName,
                DepsMap, DepSpecs, !IO)
        ;
            FileOrModule = fm_module(ModuleName),
            generate_dep_file_for_module(ProgressStream, Globals0, ModuleName,
                DepsMap, DepSpecs, !IO)
        ),
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
        (
            FileOrModule = fm_file(FileName),
            generate_d_file_for_file(ProgressStream, Globals0, FileName,
                _DepsMap, DepSpecs, !IO)
        ;
            FileOrModule = fm_module(ModuleName),
            generate_d_file_for_module(ProgressStream, Globals0, ModuleName,
                _DepsMap, DepSpecs, !IO)
        ),
        SpecsList = [DepSpecs],
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        OpModeArgs = opma_convert_to_mercury,
        read_module_or_file(ProgressStream, Globals0, Globals, FileOrModule,
            dont_return_timestamp, HaveReadSrc, !HaveParseTreeMaps, !IO),
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
                module_name_to_file_name_create_dirs(Globals, $pred,
                    ext_cur(ext_cur_user_ugly), ModuleName, UglyFileName, !IO),
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
    list.map2_foldl2(
        write_short_interface_file_int3(ProgressStream, Globals,
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
        list.map2_foldl2(
            write_private_interface_file_int0(ProgressStream, Globals,
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
            list.map2_foldl2(
                write_interface_file_int1_int2(ProgressStream, Globals,
                    do_add_new_to_hptm),
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
        Spec = simplest_no_context_spec($pred, severity_informational,
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
        ReturnTimestamp = dont_return_timestamp
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
            ReadSplitSpecs0 = ReadSpecs ++ SplitSpecs,
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
                    write_private_interface_file_int0(ProgressStream, Globals0,
                        do_not_add_new_to_hptm),
                    AncestorBurdenedModules, _Succeededs, WriteSpecsList,
                    !HaveParseTreeMaps, !IO)
            ;
                InterfaceFile = omif_int1_int2,
                list.map2_foldl2(
                    write_interface_file_int1_int2(ProgressStream, Globals0,
                        do_not_add_new_to_hptm),
                    BurdenedModules, _Succeededs, WriteSpecsList,
                    !HaveParseTreeMaps, !IO)
            ;
                InterfaceFile = omif_int3,
                list.map2_foldl2(
                    write_short_interface_file_int3(ProgressStream, Globals0,
                        do_not_add_new_to_hptm),
                    BurdenedModules, _Succeededs, WriteSpecsList,
                    !HaveParseTreeMaps, !IO)
            ),
            SpecLists = [ReadSplitSpecs | WriteSpecsList]
        )
    ).

:- func version_numbers_return_timestamp(bool) = maybe_return_timestamp.

version_numbers_return_timestamp(no) = dont_return_timestamp.
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
        % XXX EXT Is there some reason why recompilation.check.m shouldn't know
        % how to find target and timestamp files? If not, then the
        % FindTargetFiles and FindTargetFiles arguments passed to
        % should_recompile should be deleted, and the logic of the next two
        % calls should be moved to recompilation.check.m and turned into
        % first order code.
        find_smart_recompilation_target_files(Globals, FindTargetFiles),
        find_timestamp_files(Globals, FindTimestampFiles),
        recompilation.check.should_recompile(ProgressStream, Globals,
            ModuleName, FindTargetFiles, FindTimestampFiles,
            ModulesToRecompile, !HaveParseTreeMaps, !IO)
    ;
        Smart = no,
        ModulesToRecompile = all_modules
    ).

%---------------------%

    % Return a closure which will work out what the target files are for
    % a module, so recompilation_check.m can check that they are up-to-date
    % when deciding whether compilation is necessary.
    % Note that `--smart-recompilation' only works with
    % `--target-code-only', which is always set when the compiler is
    % invoked by mmake. Using smart recompilation without using mmake
    % is not a sensible thing to do. handle_options.m will disable smart
    % recompilation if `--target-code-only' is not set.
    %
:- pred find_smart_recompilation_target_files(globals::in,
    find_target_file_names::out(find_target_file_names)) is det.

find_smart_recompilation_target_files(Globals, FindTargetFiles) :-
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        TargetExt = ext_cur_ngs_gs(ext_cur_ngs_gs_target_c)
    ;
        CompilationTarget = target_csharp,
        TargetExt = ext_cur_ngs_gs(ext_cur_ngs_gs_target_cs)
    ;
        CompilationTarget = target_java,
        TargetExt = ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java)
    ),
    FindTargetFiles =
        usual_find_target_files(Globals, TargetExt).

:- pred usual_find_target_files(globals::in, ext::in,
    module_name::in, list(file_name)::out, io::di, io::uo) is det.

usual_find_target_files(Globals, TargetExt,
        ModuleName, TargetFiles, !IO) :-
    % XXX Should we check the generated header files?
    module_name_to_file_name_create_dirs(Globals, $pred, TargetExt,
        ModuleName, FileName, !IO),
    TargetFiles = [FileName].

:- pred find_timestamp_files(globals::in,
    find_timestamp_file_names::out(find_timestamp_file_names)) is det.

find_timestamp_files(Globals, FindTimestampFiles) :-
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        TimestampExt = ext_cur_ngs_gs(ext_cur_ngs_gs_target_date_c)
    ;
        CompilationTarget = target_csharp,
        TimestampExt = ext_cur_ngs_gs(ext_cur_ngs_gs_target_date_cs)
    ;
        CompilationTarget = target_java,
        TimestampExt = ext_cur_ngs_gs(ext_cur_ngs_gs_target_date_java)
    ),
    FindTimestampFiles =
        find_timestamp_files_2(Globals, TimestampExt).

:- pred find_timestamp_files_2(globals::in, ext::in,
    module_name::in, list(file_name)::out, io::di, io::uo) is det.

find_timestamp_files_2(Globals, TimestampExt,
        ModuleName, TimestampFiles, !IO) :-
    module_name_to_file_name_create_dirs(Globals, $pred, TimestampExt,
        ModuleName, FileName, !IO),
    TimestampFiles = [FileName].

%---------------------------------------------------------------------------%

:- pred read_augment_and_process_module(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, op_mode_augment::in,
    op_mode_invoked_by_mmc_make::in, list(string)::in, file_or_module::in,
    modules_to_recompile::in, list(string)::out, list(string)::out,
    list(error_spec)::out,
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
        ( OpModeAugment = opmau_errorcheck_only
        ; OpModeAugment = opmau_typecheck_only
        ; OpModeAugment = opmau_generate_code(_)
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
            read_augment_and_process_module_ok(ProgressStream, ErrorStream,
                Globals, OpModeAugment, InvokedByMmcMake, SourceFileName,
                MaybeTimestamp, ReadModuleErrors, ParseTreeSrc,
                MaybeModulesToRecompile, ModulesToLink, ExtraObjFiles,
                Specs, !HaveParseTreeMaps, !IO)
        )
    ).

:- pred read_augment_and_process_module_ok(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, op_mode_augment::in,
    op_mode_invoked_by_mmc_make::in, file_name::in, maybe(timestamp)::in,
    read_module_errors::in, parse_tree_src::in,
    modules_to_recompile::in, list(string)::out, list(string)::out,
    list(error_spec)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

read_augment_and_process_module_ok(ProgressStream, ErrorStream, Globals,
        OpModeAugment, InvokedByMmcMake, SourceFileName, MaybeTimestamp,
        ReadModuleErrors, ParseTreeSrc, MaybeModulesToRecompile,
        ModulesToLink, ExtraObjFiles, !:Specs, !HaveParseTreeMaps, !IO) :-
    ModuleName = ParseTreeSrc ^ pts_module_name,
    parse_tree_src_to_burdened_module_list(Globals, SourceFileName,
        ReadModuleErrors, MaybeTimestamp, ParseTreeSrc,
        !:Specs, BurdenedModules0),
    (
        MaybeModulesToRecompile = some_modules(ModulesToRecompile),
        ToRecompile =
            ( pred(BM::in) is semidet :-
                BM = burdened_module(_, PTMS),
                list.member(PTMS ^ ptms_module_name, ModulesToRecompile)
            ),
        list.filter(ToRecompile, BurdenedModules0,
            BurdenedModulesToRecompile)
    ;
        MaybeModulesToRecompile = all_modules,
        BurdenedModulesToRecompile = BurdenedModules0
    ),

    find_timestamp_files(Globals, FindTimestampFiles),
    globals.lookup_bool_option(Globals, trace_prof, TraceProf),
    ( if
        non_traced_mercury_builtin_module(ModuleName),
        not (
            ModuleName = mercury_profiling_builtin_module,
            TraceProf = yes
        )
    then
        % Some predicates in the builtin modules are missing typeinfo
        % arguments, which means that execution tracing will not work
        % on them. Predicates defined there should never be part of
        % an execution trace anyway; they are effectively language
        % primitives. (They may still be parts of stack traces.)
        globals.set_option(trace_stack_layout, bool(no), Globals, Globals1),
        globals.set_trace_level_none(Globals1, GlobalsToUse)
    else
        GlobalsToUse = Globals
    ),
    augment_and_process_all_submodules(ProgressStream, ErrorStream,
        GlobalsToUse, OpModeAugment, InvokedByMmcMake, MaybeTimestamp,
        FindTimestampFiles, BurdenedModulesToRecompile,
        ModulesToLink, ExtraObjFiles, !Specs, !HaveParseTreeMaps, !IO).

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

:- type file_or_module
    --->    fm_file(file_name)
    ;       fm_module(module_name).

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

    % For the MLDS->C and LLDS->C back-ends, we currently compile
    % each submodule to its own C file.
    % XXX Maybe it would be better to compile nested modules
    % to a single C file, with code like this:
    %
    %   list.map2_foldl(compile_to_llds, SubModuleList,
    %       LLDS_FragmentList),
    %   merge_llds_fragments(LLDS_FragmentList, LLDS),
    %   output_pass(LLDS_FragmentList)
    %
:- pred augment_and_process_all_submodules(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, op_mode_augment::in,
    op_mode_invoked_by_mmc_make::in, maybe(timestamp)::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    list(burdened_module)::in, list(string)::out, list(string)::out,
    list(error_spec)::in, list(error_spec)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

augment_and_process_all_submodules(ProgressStream, ErrorStream, Globals,
        OpModeAugment, InvokedByMmcMake, MaybeTimestamp, FindTimestampFiles,
        BurdenedModules, ModulesToLink, ExtraObjFiles,
        !Specs, !HaveParseTreeMaps, !IO) :-
    list.map_foldl3(
        augment_and_process_module(ProgressStream, ErrorStream, Globals,
            OpModeAugment, InvokedByMmcMake, MaybeTimestamp,
            FindTimestampFiles),
        BurdenedModules, ExtraObjFileLists,
        !Specs, !HaveParseTreeMaps, !IO),
    list.map(module_to_link, BurdenedModules, ModulesToLink),
    list.condense(ExtraObjFileLists, ExtraObjFiles).

:- pred module_to_link(burdened_module::in, string::out) is det.

module_to_link(BurdenedModule, ModuleToLink) :-
    BurdenedModule = burdened_module(_, ParseTreeModuleSrc),
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    module_name_to_file_name_stem(ModuleName, ModuleToLink).

%---------------------------------------------------------------------------%

    % Given the parse tree of a module, read in the interface and optimization
    % files it needs, and compile it.
    %
    % Stage number assignments:
    %
    %     1 to  99  front end pass
    %   100 to 299  middle pass
    %   300 to 399  LLDS back end pass
    %   400 to 499  MLDS back end pass
    %   500 to 599  bytecode back end pass
    %
    % The initial arrangement had the stage numbers increasing by five
    % so that new stages can be slotted in without too much trouble.
    %
:- pred augment_and_process_module(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, op_mode_augment::in,
    op_mode_invoked_by_mmc_make::in, maybe(timestamp)::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    burdened_module::in, list(string)::out,
    list(error_spec)::in, list(error_spec)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

augment_and_process_module(ProgressStream, ErrorStream, Globals,
        OpModeAugment, InvokedByMmcMake, MaybeTimestamp, FindTimestampFiles,
        BurdenedModule, ExtraObjFiles, !Specs, !HaveParseTreeMaps, !IO) :-
    BurdenedModule = burdened_module(Baggage0, ParseTreeModuleSrc),
    check_module_interface_for_no_exports(Globals, ParseTreeModuleSrc, !Specs),
    % XXX STREAM We could switch from a general progress stream
    % to a module-specific progress stream.
    grab_qual_imported_modules_augment(ProgressStream, Globals,
        MaybeTimestamp, ParseTreeModuleSrc, AugCompUnit,
        Baggage0, Baggage, !HaveParseTreeMaps, !IO),
    Errors = Baggage ^ mb_errors,
    !:Specs = get_read_module_specs(Errors) ++ !.Specs,
    ( if set.is_empty(Errors ^ rm_fatal_errors) then
        process_augmented_module(ProgressStream, ErrorStream, Globals,
            OpModeAugment, InvokedByMmcMake, Baggage, AugCompUnit,
            FindTimestampFiles, ExtraObjFiles, no_prev_dump, _,
            !Specs, !HaveParseTreeMaps, !IO)
    else
        ExtraObjFiles = []
    ).

:- pred process_augmented_module(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    op_mode_augment::in, op_mode_invoked_by_mmc_make::in,
    module_baggage::in, aug_compilation_unit::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    list(string)::out, dump_info::in, dump_info::out,
    list(error_spec)::in, list(error_spec)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

process_augmented_module(ProgressStream, ErrorStream, Globals0,
        OpModeAugment, InvokedByMmcMake, Baggage, AugCompUnit,
        FindTimestampFiles, ExtraObjFiles, !DumpInfo, !Specs,
        !HaveParseTreeMaps, !IO) :-
    (
        ( OpModeAugment = opmau_typecheck_only
        ; OpModeAugment = opmau_errorcheck_only
        ),
        Globals = Globals0,
        % If we are only typechecking or error checking, then we should not
        % modify any files; this includes writing to .d files.
        WriteDFile = do_not_write_d_file
    ;
        OpModeAugment = opmau_make_trans_opt,
        disable_warning_options(Globals0, Globals),
        WriteDFile = write_d_file
    ;
        OpModeAugment = opmau_generate_code(_),
        Globals = Globals0,
        WriteDFile = write_d_file
    ;
        OpModeAugment = opmau_make_plain_opt,
        disable_warning_options(Globals0, Globals),
        % Don't write the `.d' file when making the `.opt' file because
        % we can't work out the full transitive implementation dependencies.
        WriteDFile = do_not_write_d_file
    ;
        ( OpModeAugment = opmau_make_analysis_registry
        ; OpModeAugment = opmau_make_xml_documentation
        ),
        Globals = Globals0,
        % XXX I (zs) think we should assign do_not_write_d_file for these.
        WriteDFile = write_d_file
    ),
    make_hlds_pass(ProgressStream, ErrorStream, Globals,
        OpModeAugment, InvokedByMmcMake, WriteDFile, Baggage, AugCompUnit,
        HLDS1, QualInfo, MaybeTimestampMap, UndefTypes, UndefModes,
        PreHLDSErrors, !DumpInfo, !Specs, !HaveParseTreeMaps, !IO),
    frontend_pass(ProgressStream, ErrorStream, OpModeAugment, QualInfo,
        UndefTypes, UndefModes, PreHLDSErrors, FrontEndErrors,
        HLDS1, HLDS20, !DumpInfo, !Specs, !IO),
    io.get_exit_status(ExitStatus, !IO),
    ( if
        PreHLDSErrors = no,
        FrontEndErrors = no,
        contains_errors(Globals, !.Specs) = no,
        ExitStatus = 0
    then
        globals.lookup_bool_option(Globals, verbose, Verbose),
        globals.lookup_bool_option(Globals, statistics, Stats),
        maybe_write_dependency_graph(ProgressStream, ErrorStream,
            Verbose, Stats, HLDS20, HLDS21, !IO),
        (
            OpModeAugment = opmau_typecheck_only,
            ExtraObjFiles = []
        ;
            OpModeAugment = opmau_errorcheck_only,
            % We may still want to run `unused_args' so that we get
            % the appropriate warnings.
            globals.lookup_bool_option(Globals, warn_unused_args, UnusedArgs),
            (
                UnusedArgs = yes,
                globals.get_opt_tuple(Globals, OptTuple),
                NoOptUnusedArgsOptTuple = OptTuple ^ ot_opt_unused_args
                    := do_not_opt_unused_args,
                globals.set_opt_tuple(NoOptUnusedArgsOptTuple,
                    Globals, NoOptUnusedArgsGlobals),
                module_info_set_globals(NoOptUnusedArgsGlobals,
                    HLDS21, HLDS21a),
                maybe_unused_args(ProgressStream, Verbose, Stats,
                    _UnusedArgsInfos, HLDS21a, _HLDS22, !Specs, !IO)
            ;
                UnusedArgs = no
            ),
            ExtraObjFiles = []
        ;
            OpModeAugment = opmau_make_plain_opt,
            % Only run up to typechecking when making the .opt file.
            ExtraObjFiles = []
        ;
            OpModeAugment = opmau_make_trans_opt,
            output_trans_opt_file(ProgressStream, HLDS21, !Specs,
                !DumpInfo, !IO),
            ExtraObjFiles = []
        ;
            OpModeAugment = opmau_make_analysis_registry,
            prepare_for_intermodule_analysis(ProgressStream, Globals,
                Verbose, Stats, AnalysisSpecs, HLDS21, HLDS22, !IO),
            (
                AnalysisSpecs = [],
                output_analysis_file(ProgressStream, HLDS22, !Specs,
                    !DumpInfo, !IO)
            ;
                AnalysisSpecs = [_ | _],
                !:Specs = AnalysisSpecs ++ !.Specs
            ),
            ExtraObjFiles = []
        ;
            OpModeAugment = opmau_make_xml_documentation,
            xml_documentation(ProgressStream, HLDS21, !IO),
            ExtraObjFiles = []
        ;
            OpModeAugment = opmau_generate_code(OpModeCodeGen),
            maybe_prepare_for_intermodule_analysis(ProgressStream, Globals,
                Verbose, Stats, AnalysisSpecs, HLDS21, HLDS22, !IO),
            (
                AnalysisSpecs = [],
                MaybeTopModule = Baggage ^ mb_maybe_top_module,
                after_front_end_passes(ProgressStream, ErrorStream, Globals,
                    OpModeCodeGen, MaybeTopModule,
                    FindTimestampFiles, MaybeTimestampMap, HLDS22,
                    ExtraObjFiles, !Specs, !DumpInfo, !IO)
            ;
                AnalysisSpecs = [_ | _],
                !:Specs = AnalysisSpecs ++ !.Specs,
                ExtraObjFiles = []
            )
        )
    else
        % If the number of errors is > 0, make sure that the compiler
        % exits with a non-zero exit status.
        ( if ExitStatus = 0 then
            io.set_exit_status(1, !IO)
        else
            true
        ),
        ExtraObjFiles = []
    ).

:- pred disable_warning_options(globals::in, globals::out) is det.

disable_warning_options(Globals0, Globals) :-
    globals.get_options(Globals0, OptionTable0),
    set_all_options_to(style_warning_options, bool(no),
        OptionTable0, OptionTable1),
    set_all_options_to(non_style_warning_options, bool(no),
        OptionTable1, OptionTable),
    globals.set_options(OptionTable, Globals0, Globals).

%---------------------------------------------------------------------------%

:- pred maybe_write_dependency_graph(io.text_output_stream::in,
    io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_write_dependency_graph(ProgressStream, ErrorStream, Verbose, Stats,
        !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, show_dependency_graph, ShowDepGraph),
    (
        ShowDepGraph = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Writing dependency graph...", !IO),
        module_info_get_name(!.HLDS, ModuleName),
        module_name_to_file_name_create_dirs(Globals, $pred,
            ext_cur(ext_cur_user_depgraph), ModuleName, DepGraphFileName, !IO),
        io.open_output(DepGraphFileName, OpenResult, !IO),
        (
            OpenResult = ok(FileStream),
            write_dependency_graph(FileStream, !HLDS, !IO),
            io.close_output(FileStream, !IO),
            maybe_write_string(ProgressStream, Verbose, " done.\n", !IO)
        ;
            OpenResult = error(IOError),
            ErrorMsg = "unable to write dependency graph: " ++
                io.error_message(IOError),
            report_error(ErrorStream, ErrorMsg, !IO)
        ),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        ShowDepGraph = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_prepare_for_intermodule_analysis(io.text_output_stream::in,
    globals::in, bool::in, bool::in, list(error_spec)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_prepare_for_intermodule_analysis(ProgressStream, Globals,
        Verbose, Stats, Specs, !HLDS, !IO) :-
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    (
        IntermodAnalysis = yes,
        prepare_for_intermodule_analysis(ProgressStream, Globals,
            Verbose, Stats, Specs, !HLDS, !IO)
    ;
        IntermodAnalysis = no,
        Specs = []
    ).

:- pred prepare_for_intermodule_analysis(io.text_output_stream::in,
    globals::in, bool::in, bool::in, list(error_spec)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

prepare_for_intermodule_analysis(ProgressStream, Globals,
        Verbose, Stats, Specs, !HLDS, !IO) :-
    maybe_write_string(ProgressStream, Verbose,
        "% Preparing for intermodule analysis...\n", !IO),

    module_info_get_all_deps(!.HLDS, ModuleNames),

    globals.lookup_accumulating_option(Globals, local_module_id,
        LocalModulesList),
    SymNames = list.map(string_to_sym_name, LocalModulesList),
    LocalModuleNames = set.list_to_set(SymNames),

    module_info_get_analysis_info(!.HLDS, AnalysisInfo0),
    prepare_intermodule_analysis(ProgressStream, Globals, ModuleNames,
        LocalModuleNames, Specs, AnalysisInfo0, AnalysisInfo, !IO),
    module_info_set_analysis_info(AnalysisInfo, !HLDS),

    maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

%---------------------------------------------------------------------------%

:- pred after_front_end_passes(io.text_output_stream::in,
    io.text_output_stream::in, globals::in,
    op_mode_codegen::in, maybe_top_module::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    maybe(module_timestamp_map)::in, module_info::in,
    list(string)::out, list(error_spec)::in, list(error_spec)::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

after_front_end_passes(ProgressStream, ErrorStream, Globals, OpModeCodeGen,
        MaybeTopModule, FindTimestampFiles, MaybeTimestampMap, !.HLDS,
        ExtraObjFiles, !Specs, !DumpInfo, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),
    maybe_output_prof_call_graph(ProgressStream, ErrorStream, Verbose, Stats,
        !HLDS, !IO),
    middle_pass(ProgressStream, ErrorStream, !HLDS, !DumpInfo, !Specs, !IO),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    globals.get_target(Globals, Target),

    % Remove any existing `.used' file before writing the output file.
    % This avoids leaving the old `used' file lying around if compilation
    % is interrupted after the new output file is written but before the new
    % `.used' file is written.

    module_info_get_name(!.HLDS, ModuleName),
    module_name_to_file_name(Globals, $pred,
        ext_cur_ngs_gs(ext_cur_ngs_gs_misc_used), ModuleName, UsageFileName),
    io.file.remove_file(UsageFileName, _, !IO),

    FrontEndErrors =
        contains_errors_or_warnings_treated_as_errors(Globals, !.Specs),
    io.get_exit_status(ExitStatus, !IO),
    ( if
        FrontEndErrors = no,
        ExitStatus = 0
    then
        (
            Target = target_csharp,
            mlds_backend(ProgressStream, ErrorStream, !.HLDS, _, MLDS,
                NewSpecs, !DumpInfo, !IO),
            !:Specs = NewSpecs ++ !.Specs,
            % mlds_to_csharp never goes beyond generating C# code.
            mlds_to_csharp(ProgressStream, !.HLDS, MLDS, Succeeded, !IO),
            ExtraObjFiles = []
        ;
            Target = target_java,
            mlds_backend(ProgressStream, ErrorStream, !.HLDS, _, MLDS,
                NewSpecs, !DumpInfo, !IO),
            !:Specs = NewSpecs ++ !.Specs,
            mlds_to_java(ProgressStream, !.HLDS, MLDS, TargetCodeSucceeded,
                !IO),
            (
                OpModeCodeGen = opmcg_target_code_only,
                Succeeded = TargetCodeSucceeded
            ;
                ( OpModeCodeGen = opmcg_target_and_object_code_only
                ; OpModeCodeGen = opmcg_target_object_and_executable
                ),
                (
                    TargetCodeSucceeded = did_not_succeed,
                    Succeeded = did_not_succeed
                ;
                    TargetCodeSucceeded = succeeded,
                    module_name_to_file_name(Globals, $pred,
                        ext_cur_ngs_gs_java(ext_cur_ngs_gs_java_java),
                        ModuleName, JavaFile),
                    compile_java_files(Globals, ProgressStream,
                        JavaFile, [], Succeeded, !IO),
                    maybe_set_exit_status(Succeeded, !IO)
                )
            ),
            ExtraObjFiles = []
        ;
            Target = target_c,
            % Produce the grade independent header file <module>.mh
            % containing function prototypes for the procedures referred to
            % by foreign_export pragmas.
            export.get_foreign_export_decls(!.HLDS, ExportDecls),
            export.produce_header_file(ProgressStream, !.HLDS, ExportDecls,
                ModuleName, !IO),
            (
                HighLevelCode = yes,
                mlds_backend(ProgressStream, ErrorStream, !.HLDS, _, MLDS,
                    NewSpecs, !DumpInfo, !IO),
                !:Specs = NewSpecs ++ !.Specs,
                mlds_to_high_level_c(ProgressStream, Globals, MLDS,
                    TargetCodeSucceeded, !IO),
                (
                    OpModeCodeGen = opmcg_target_code_only,
                    Succeeded = TargetCodeSucceeded
                ;
                    ( OpModeCodeGen = opmcg_target_and_object_code_only
                    ; OpModeCodeGen = opmcg_target_object_and_executable
                    ),
                    (
                        TargetCodeSucceeded = did_not_succeed,
                        Succeeded = did_not_succeed
                    ;
                        TargetCodeSucceeded = succeeded,
                        % XXX EXT Why not _create_dirs?
                        module_name_to_file_name(Globals, $pred,
                            ext_cur_ngs_gs(ext_cur_ngs_gs_target_c),
                            ModuleName, C_File),
                        get_linked_target_type(Globals, TargetType),
                        get_object_code_type(Globals, TargetType, PIC),
                        maybe_pic_object_file_extension(PIC, ObjExt, _),
                        module_name_to_file_name_create_dirs(Globals, $pred,
                            ext_cur_ngs_gs(ObjExt), ModuleName, O_File, !IO),
                        do_compile_c_file(Globals, ProgressStream, PIC,
                            C_File, O_File, Succeeded, !IO),
                        maybe_set_exit_status(Succeeded, !IO)
                    )
                ),
                ExtraObjFiles = []
            ;
                HighLevelCode = no,
                llds_backend_pass(ProgressStream, ErrorStream, !HLDS,
                    GlobalData, LLDS, !DumpInfo, !IO),
                % llds_output_pass looks up the target_code_only option
                % to see whether it should generate object code, using the
                % same logic as the HighLevelCode = yes case above.
                % XXX Move that logic here, for symmetry.
                llds_output_pass(ProgressStream, OpModeCodeGen,
                    !.HLDS, GlobalData, LLDS, ModuleName, Succeeded,
                    ExtraObjFiles, !IO)
            )
        ),
        (
            Succeeded = succeeded,
            module_info_get_maybe_recompilation_info(!.HLDS, MaybeRecompInfo),
            ( if
                MaybeRecompInfo = yes(RecompInfo),
                MaybeTimestampMap = yes(TimestampMap)
            then
                construct_used_file_contents(!.HLDS, RecompInfo,
                    MaybeTopModule, TimestampMap, UsedFileContents),
                write_usage_file(ProgressStream, !.HLDS, UsedFileContents, !IO)
            else
                true
            ),
            FindTimestampFiles(ModuleName, TimestampFiles, !IO),
            list.map_foldl(
                touch_file_datestamp(Globals, ProgressStream),
                TimestampFiles, _Succeededs, !IO)
        ;
            Succeeded = did_not_succeed
            % An error should have been reported earlier.
        )
    else
        % Make sure that the compiler exits with a non-zero exit status.
        ( if ExitStatus = 0 then
            io.set_exit_status(1, !IO)
        else
            true
        ),
        ExtraObjFiles = []
    ).

%---------------------%

    % Outputs the file <module_name>.prof, which contains the static
    % call graph in terms of label names, if the profiling flag is enabled.
    %
:- pred maybe_output_prof_call_graph(io.text_output_stream::in,
    io.text_output_stream::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_output_prof_call_graph(ProgressStream, ErrorStream, Verbose, Stats,
        !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, profile_calls, ProfileCalls),
    globals.lookup_bool_option(Globals, profile_time, ProfileTime),
    ( if
        ( ProfileCalls = yes
        ; ProfileTime = yes
        )
    then
        maybe_write_string(ProgressStream, Verbose,
            "% Outputting profiling call graph...", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        module_info_get_name(!.HLDS, ModuleName),
        module_name_to_file_name_create_dirs(Globals, $pred,
            ext_cur_ngs(ext_cur_ngs_misc_prof), ModuleName, ProfFileName, !IO),
        io.open_output(ProfFileName, Res, !IO),
        (
            Res = ok(FileStream),
            write_prof_dependency_graph(FileStream, !HLDS, !IO),
            io.close_output(FileStream, !IO)
        ;
            Res = error(IOError),
            ErrorMsg = "unable to write profiling static call graph: " ++
                io.error_message(IOError),
            report_error(ErrorStream, ErrorMsg, !IO)
        ),
        maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred dump_args(io.text_output_stream::in, string::in, list(string)::in,
    io::di, io::uo) is det.

dump_args(OutStream, Marker, Args, !IO) :-
    io.format(OutStream, "%s START\n", [s(Marker)], !IO),
    list.foldl(dump_arg(OutStream), Args, !IO),
    io.format(OutStream, "%s END\n", [s(Marker)], !IO).

:- pred dump_arg(io.text_output_stream::in, string::in, io::di, io::uo) is det.

dump_arg(OutStream, Arg, !IO) :-
    io.format(OutStream, "<%s>\n", [s(Arg)], !IO).

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
