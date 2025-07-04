%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2017-2025 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mercury_compile_args.m.
% Main authors: fjh, zs.
%
% This module handles argument processing for the Mercury compiler.
%
%---------------------------------------------------------------------------%

:- module top_level.mercury_compile_args.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module make.
:- import_module make.options_file.

:- import_module io.
:- import_module list.

:- type arg_processing_result
    --->    apr_failure
    ;       apr_success(
                aprs_globals                :: globals,
                aprs_env_optfile_variables  :: env_optfile_variables,
                aprs_env_var_args           :: list(string),
                aprs_option_args            :: list(string),
                aprs_nonoption_args         :: list(string)
            ).

:- pred setup_all_args(io.text_output_stream::in, io.text_output_stream::in,
    list(string)::in, arg_processing_result::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred get_args_representing_env_vars(list(string)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.handle_options.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module cord.
:- import_module dir.
:- import_module getopt.
:- import_module io.environment.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

setup_all_args(ProgressStream, ErrorStream, CmdLineArgs, ArgResult, !IO) :-
    trace [
        compile_time(flag("cmd_line_args")),
        run_time(env("MMC_CMD_LINE_ARGS")),
        io(!TIO)]
    (
        dump_args(ErrorStream, "REAL_MAIN", CmdLineArgs, !TIO)
    ),

    % Replace all @file arguments with the contents of the file.
    expand_at_file_arguments(CmdLineArgs, ExpandResult, !IO),
    (
        ExpandResult = ok(ExpandedCmdLineArgs),
        get_option_default_table(DefaultOptionTable),
        % XXX Eventually we shouldn't have to pass DefaultOptionTable.
        ( if ExpandedCmdLineArgs = ["--arg-file", ArgFile | ExtraArgs] then
            % Diagnose bad invocations, such as shell redirection operators
            % treated as command line arguments.
            (
                ExtraArgs = []
            ;
                ExtraArgs = [_ | _],
                unexpected($pred,
                    "extra arguments with --arg-file: " ++ string(ExtraArgs))
            ),
            process_options_arg_file(ProgressStream, DefaultOptionTable,
                ArgFile, OptResult, !IO)
        else
            process_options_std(ProgressStream, ErrorStream,
                DefaultOptionTable, ExpandedCmdLineArgs, OptResult, !IO)
        ),
        (
            OptResult = opr_success(EnvOptFileVariables, MCFlags,
                OptionArgs, NonOptionArgs, OptionSpecs),

            get_args_representing_env_vars(EnvVarArgs, !IO),
            % NOTE: The order of the flags here must be:
            %
            %   (1) flags from Mercury.config and any Mercury.options files
            %   (2) flags from environment variables
            %   (3) flags from any command line options
            %
            % The order is important, because flags given later in this list
            % can override those given earlier.
            AllFlags = MCFlags ++ EnvVarArgs ++ OptionArgs,
            trace [compile_time(flag("cmd_line_args")),
                run_time(env("MMC_CMD_LINE_ARGS")),
                io(!TIO)]
            (
                dump_args(ErrorStream, "AllFlags", AllFlags, !TIO)
            ),
            MaybeStdLibGrades = stdlib_grades_unknown,
            lookup_mercury_stdlib_dir(EnvOptFileVariables,
                MaybeEnvOptFileStdLibDirs),
            handle_given_options(ProgressStream, DefaultOptionTable,
                MaybeStdLibGrades, MaybeEnvOptFileStdLibDirs, AllFlags, _, _,
                Specs, Globals, !IO),

            % Now that we have constructed a globals, print out any errors
            % and/or warnings generated by the predicates in options_file.m.
            write_error_specs(ErrorStream, Globals, OptionSpecs, !IO),

            % When computing the option arguments to pass to `--make',
            % only include the command-line arguments, not the contents of
            % DEFAULT_MCFLAGS.
            (
                Specs = [_ | _],
                usage_errors(ProgressStream, Globals, Specs, !IO),
                ArgResult = apr_failure
            ;
                Specs = [],
                ArgResult = apr_success(Globals, EnvOptFileVariables,
                    EnvVarArgs, OptionArgs, NonOptionArgs)
            )
        ;
            OptResult = opr_failure(OptionSpecs),
            % Usually, any error_specs we write out here were generated
            % in options_file.m.
            write_error_specs_opt_table(ErrorStream, DefaultOptionTable,
                OptionSpecs, !IO),
            io.set_exit_status(1, !IO),
            ArgResult = apr_failure
        )
    ;
        ExpandResult = error(E),
        io.format(ProgressStream, "%s\n", [s(io.error_message(E))], !IO),
        io.set_exit_status(1, !IO),
        ArgResult = apr_failure
    ).

%---------------------------------------------------------------------------%

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
                oprs_env_optfile_variables  :: env_optfile_variables,
                oprs_mcflags                :: list(string),
                oprs_option_args            :: list(string),
                oprs_nonoption_args         :: list(string),
                oprs_nonfatal_errors        :: list(error_spec)
            ).

%---------------------%

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
    EnvOptFileVariables = env_optfile_variables_init(EnvVarMap),
    (
        MaybeArgs1 = yes(Args1),
        get_short_option(ShortOption),
        get_long_option(LongOption),
        % Separate the option args from the non-option args.
        getopt.record_arguments(ShortOption, LongOption, DefaultOptionTable,
            Args1, NonOptionArgs, OptionArgs, MaybeError, _OptionValues),
        (
            MaybeError = found_option_error(OptionError),
            OptionErrorStr = option_error_to_string(OptionError),
            Spec = no_ctxt_spec($pred, severity_error,
                phase_options, [words(OptionErrorStr), suffix("."), nl]),
            Result = opr_failure([Spec])
        ;
            MaybeError = no_option_error,
            Result = opr_success(EnvOptFileVariables, [],
                OptionArgs, NonOptionArgs, Specs)
        )
    ;
        MaybeArgs1 = no,
        OptionArgs = [],
        NonOptionArgs = [],
        Result = opr_success(EnvOptFileVariables, [],
            OptionArgs, NonOptionArgs, Specs)
    ).

%---------------------%

:- pred process_options_std(io.text_output_stream::in,
    io.text_output_stream::in, option_table::in, list(string)::in,
    option_processing_result::out, io::di, io::uo) is det.

process_options_std(ProgressStream, ErrorStream, DefaultOptionTable,
        CmdLineArgs, Result, !IO) :-
    % Find out which options files to read.
    % Don't report errors yet, as the errors may no longer exist
    % after we have read in options files.
    get_short_option(ShortOption),
    get_long_option(LongOption),
    OptionOps = option_ops_userdata(ShortOption, LongOption, special_handler),
    getopt.process_options_userdata_io(OptionOps, CmdLineArgs,
        OptionArgs, NonOptionArgs, MaybeError, _OptionsSet,
        DefaultOptionTable, ArgsOptionTable, cord.init, _UserData, !IO),
    (
        MaybeError = yes(OptionError),
        Specs = report_option_error(OptionError),
        Result = opr_failure(Specs)
    ;
        MaybeError = no,
        read_options_files_named_in_options_file_option(ProgressStream,
            ArgsOptionTable, EnvOptFileVariables0,
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
                EnvOptFileVariables0, !IO),
            lookup_mmc_options(EnvOptFileVariables0, MaybeMCFlags0),
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
                    EnvOptFileVariables0, EnvOptFileVariables,
                    MaybeMCFlags, OptFileOkSpecs, !IO),
                Specs = OptFileSpecs ++ OptFileOkSpecs,
                (
                    MaybeMCFlags = no,
                    Result = opr_failure(Specs)
                ;
                    MaybeMCFlags = yes(MCFlags),
                    Result = opr_success(EnvOptFileVariables, MCFlags,
                        OptionArgs, NonOptionArgs, Specs)
                )
            )
        ;
            OptFileSpecs = [_ | _],
            Result = opr_failure(OptFileSpecs)
        )
    ).

%---------------------%

:- pred process_options_std_config_file(io.text_output_stream::in,
    option_table::in, environment_var_map::in, bool::in,
    env_optfile_variables::in, env_optfile_variables::out,
    maybe(list(string))::out, list(error_spec)::out, io::di, io::uo) is det.

process_options_std_config_file(ProgressStream, FlagsArgsOptionTable,
        EnvVarMap, WarnUndef, EnvOptFileVariables0, EnvOptFileVariables,
        MaybeMCFlags, Specs, !IO) :-
    getopt.lookup_maybe_string_option(FlagsArgsOptionTable, config_file,
        MaybeConfigFile0),
    % The meanings of the possible values of MaybeConfigFile0 are as follows.
    %
    % - A value of yes("") means either that the command line had no
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
            EnvOptFileVariables0, EnvOptFileVariables,
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
            lookup_mmc_options(EnvOptFileVariables, MaybeMCFlags1),
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

            % maybe_libgrade_opts_for_detected_stdlib_grades does this lookup,
            % but only if --mercury-stdlib-dir is NOT specified. Because of
            % that, it is simpler to repeat the call here than to try to
            % optimize it away.
            % XXX I (zs) think that checking whether this call returns any
            % errors would be a good idea even if --mercury-stdlib-dir IS
            % specified, because it would force any reported problems to be
            % found and fixed up front.
            lookup_mercury_stdlib_dir(EnvOptFileVariables,
                MaybeConfigMerStdLibDir),
            Specs = Specs0 ++ get_any_errors1(MaybeConfigMerStdLibDir)
        ;
            ConfigNonUndefSpecs = [_ | _],
            MaybeMCFlags = no,
            Specs = ConfigSpecs
        )
    ;
        MaybeConfigFile = no,
        EnvOptFileVariables = env_optfile_variables_init(EnvVarMap),
        lookup_mmc_options(EnvOptFileVariables, MaybeMCFlags1),
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

%---------------------------------------------------------------------------%

:- func report_option_error(option_error(option)) = list(error_spec).

report_option_error(OptionError) = Specs :-
    OptionErrorStr = option_error_to_string(OptionError),
    MainMsg = no_ctxt_msg([words("Error:"), words(OptionErrorStr),
        suffix("."), nl]),
    ( if
        OptionError = unrecognized_option(OptionStr),
        ( if string.remove_prefix("--no-", OptionStr, BaseOptionStr0) then
            IsNegatedOption = yes,
            BaseOptionStr = BaseOptionStr0
        else if string.remove_prefix("--", OptionStr, BaseOptionStr0) then
            IsNegatedOption = no,
            BaseOptionStr = BaseOptionStr0
        else
            % The option was a short option and we cannot meaningfully
            % find good "Did you mean ..." suggestions for those.
            fail
        ),
        BaseOptionStr \= ""
    then
        (
            IsNegatedOption = yes,
            Prefix = "--no-",
            all_negatable_long_option_strings(OptionStrs)
        ;
            IsNegatedOption = no,
            Prefix = "--",
            all_long_option_strings(OptionStrs)
        ),
        maybe_construct_prefixed_did_you_mean_pieces(Prefix, BaseOptionStr,
            OptionStrs, DidYouMeanPieces),
        DidYouMeanMsg = error_msg(no, always_treat_as_first, 0u,
            [always(DidYouMeanPieces)]),
        Msgs = [MainMsg, DidYouMeanMsg]
    else
        Msgs = [MainMsg]
    ),
    Specs = [error_spec($pred, severity_error, phase_options, Msgs)].

%---------------------------------------------------------------------------%

:- pred maybe_dump_options_file(io.text_output_stream::in, option_table::in,
    env_optfile_variables::in, io::di, io::uo) is det.

maybe_dump_options_file(OutStream, ArgsOptionTable, EnvOptFileVariables,
        !IO) :-
    lookup_string_option(ArgsOptionTable, dump_options_file, DumpOptionsFile),
    ( if DumpOptionsFile = "" then
        true
    else
        dump_options_file(OutStream, DumpOptionsFile, EnvOptFileVariables, !IO)
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
%---------------------------------------------------------------------------%

get_args_representing_env_vars(EnvVarArgs, !IO) :-
    io.environment.get_environment_var("MERCURY_COLOR_SCHEME",
        MaybeColorScheme, !IO),
    ( if
        MaybeColorScheme = yes(ColorScheme),
        ColorScheme \= ""
    then
        EnvVarColorSchemeArgs = ["--color-scheme-envvar", ColorScheme]
    else
        EnvVarColorSchemeArgs = []
    ),
    io.environment.get_environment_var("MERCURY_ENABLE_COLOR",
        MaybeEnableColor, !IO),
    ( if
        MaybeEnableColor = yes(EnableColor),
        % We ignore the value of EnableColor if it has a value *other than*
        % the ones recognized in this switch.
        (
            ( EnableColor = "never"
            ; EnableColor = "0"
            ),
            EnableArg = "--no-color-diagnostics"
        ;
            ( EnableColor = "always"
            ; EnableColor = "1"
            ),
            EnableArg = "--color-diagnostics"
        )
    then
        EnvVarEnableColorArgs = [EnableArg]
    else
        io.environment.get_environment_var("NO_COLOR", MaybeNoColor, !IO),
        ( if
            MaybeNoColor = yes(NoColorValue),
            NoColorValue \= ""
        then
            % The environment variable NO_COLOR is present and nonempty.
            EnvVarEnableColorArgs = ["--no-color-diagnostics"]
        else
            EnvVarEnableColorArgs = []
        )
    ),
    EnvVarArgs = EnvVarColorSchemeArgs ++ EnvVarEnableColorArgs.

%---------------------------------------------------------------------------%
:- end_module top_level.mercury_compile_args.
%---------------------------------------------------------------------------%
