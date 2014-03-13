%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mercury_compile.m.
% Main authors: fjh, zs.
%
% This is the top-level of the Mercury compiler.
%
% This module invokes the different passes of the compiler as appropriate.
% The constraints on pass ordering are documented in
% compiler/notes/compiler_design.html.
%
%-----------------------------------------------------------------------------%

:- module top_level.mercury_compile.
:- interface.

:- import_module libs.globals.

:- import_module io.
:- import_module list.

    % This is the main entry point for the Mercury compiler.
    % It is called from top_level.main.
    %
:- pred real_main(io::di, io::uo) is det.

    % main_for_make(Globals, Args, !IO) is called from
    % make.module_target.call_mercury_compile_main.
    %
:- pred main_for_make(globals::in, list(string)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.compile_target_code.
:- import_module backend_libs.export.
:- import_module check_hlds.xml_documentation.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.file_util.
:- import_module libs.handle_options.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module ll_backend.llds_to_x86_64.
:- import_module ll_backend.llds_to_x86_64_out.
:- import_module make.
:- import_module make.options_file.
:- import_module make.util.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.shared_utilities.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_event.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.
:- import_module parse_tree.source_file_map.
:- import_module parse_tree.write_deps_file.
:- import_module recompilation.
:- import_module recompilation.check.
:- import_module recompilation.usage.
:- import_module top_level.mercury_compile_erl_back_end.
:- import_module top_level.mercury_compile_front_end.
:- import_module top_level.mercury_compile_llds_back_end.
:- import_module top_level.mercury_compile_middle_passes.
:- import_module top_level.mercury_compile_mlds_back_end.
:- import_module transform_hlds.dependency_graph.
:- import_module transform_hlds.intermod.
:- import_module transform_hlds.trans_opt.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module dir.
:- import_module gc.
:- import_module getopt_io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module unit.

%-----------------------------------------------------------------------------%

real_main(!IO) :-
    gc_init(!IO),

    % All messages go to stderr.
    io.stderr_stream(StdErr, !IO),
    io.set_output_stream(StdErr, _, !IO),
    io.command_line_arguments(CmdLineArgs, !IO),

    unlimit_stack(!IO),

    % Replace all @file arguments with the contents of the file
    expand_at_file_arguments(CmdLineArgs, Res, !IO),
    (
        Res = ok(ExpandedCmdLineArgs),
        real_main_after_expansion(ExpandedCmdLineArgs, !IO)
    ;
        Res = error(E),
        io.set_exit_status(1, !IO),

        io.write_string(io.error_message(E), !IO),
        io.nl(!IO)
    ).

    % Expand @File arguments.
    % Each argument in the above form is replaced with a list of arguments
    % where each arg is each line in the file File which is not just
    % whitespace.
    %
:- pred expand_at_file_arguments(list(string)::in, io.res(list(string))::out,
    io::di, io::uo) is det.

expand_at_file_arguments([], ok([]), !IO).
expand_at_file_arguments([Arg | Args], Result, !IO) :-
    ( string.remove_prefix("@", Arg, File) ->
        io.open_input(File, OpenRes, !IO),
        (
            OpenRes = ok(S),
            expand_file_into_arg_list(S, ReadRes, !IO),
            ( ReadRes = ok(FileArgs),
                expand_at_file_arguments(FileArgs ++ Args, Result, !IO)
            ; ReadRes = error(E),
                Result = error(at_file_error(File, E))
            )
        ;
            OpenRes = error(_E),
            Msg = "mercury_compile: cannot open '" ++ File ++ "'",
            Result = error(io.make_io_error(Msg))
        )
    ;
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
:- pred expand_file_into_arg_list(io.input_stream::in,
    io.res(list(string))::out, io::di, io::uo) is det.

expand_file_into_arg_list(S, Res, !IO) :-
    io.read_line_as_string(S, LineRes, !IO),
    (
        LineRes = ok(Line),
        expand_file_into_arg_list(S, Res0, !IO),
        ( Res0 = ok(Lines),
            StrippedLine = strip(Line),
            ( StrippedLine = "" ->
                Res = ok(Lines)
            ;
                Res = ok([StrippedLine | Lines])
            )
        ; Res0 = error(_E),
            Res = Res0
        )
    ;
        LineRes = eof,
        Res = ok([])
    ;
        LineRes = error(E),
        Res = error(E)
    ).

%-----------------------------------------------------------------------------%

:- pred real_main_after_expansion(list(string)::in, io::di, io::uo) is det.

real_main_after_expansion(CmdLineArgs, !IO) :-
    % XXX Processing the options up to three times is not what you call
    % elegant.
    ( CmdLineArgs = ["--arg-file", ArgFile | ExtraArgs] ->
        % All the configuration and options file options are passed in the
        % given file, which is created by the parent `mmc --make' process.
        % (make.module_target does this to overcome limits on the lengths
        % of command lines on Windows.) The environment is ignored, unlike
        % with @file syntax.

        % Diagnose bad invocations, e.g. shell redirection operators treated
        % as command line arguments.
        (
            ExtraArgs = []
        ;
            ExtraArgs = [_ | _],
            unexpected($module, $pred,
                "extra arguments with --arg-file: " ++ string(ExtraArgs))
        ),

        % `mmc --mmake' does not use the --arg-file mechanism for linking.
        Link = no,

        % Read_args_file may attempt to look up options, so we need
        % to initialize the globals.
        generate_default_globals(DummyGlobals, !IO),
        options_file.read_args_file(DummyGlobals, ArgFile, MaybeArgs1, !IO),
        (
            MaybeArgs1 = yes(Args1),
            separate_option_args(Args1, OptionArgs, NonOptionArgs, !IO)
        ;
            MaybeArgs1 = no,
            OptionArgs = [],
            NonOptionArgs = []
        ),
        DetectedGradeFlags = [],
        Variables = options_variables_init,
        MaybeMCFlags = yes([])
    ;
        % Find out which options files to read.
        % Don't report errors yet, as the errors may no longer exist
        % after we have read in options files.
        handle_given_options(CmdLineArgs, OptionArgs, NonOptionArgs,
            Link, _Errors0, ArgsGlobals, !IO),
        read_options_files(ArgsGlobals, options_variables_init,
            MaybeVariables0, !IO),
        (
            MaybeVariables0 = yes(Variables0),
            lookup_mmc_options(ArgsGlobals, Variables0, MaybeMCFlags0, !IO),
            (
                MaybeMCFlags0 = yes(MCFlags0),

                % Process the options again to find out which configuration
                % file to read.
                handle_given_options(MCFlags0 ++ CmdLineArgs, _, _, _,
                    FlagsErrors, FlagsArgsGlobals, !IO),
                (
                    FlagsErrors = [_ | _],
                    usage_errors(FlagsErrors, !IO),
                    DetectedGradeFlags = [],
                    Variables = options_variables_init,
                    MaybeMCFlags = no
                ;
                    FlagsErrors = [],
                    globals.lookup_maybe_string_option(FlagsArgsGlobals,
                        config_file, MaybeConfigFile),
                    (
                        MaybeConfigFile = yes(ConfigFile),
                        read_options_file(FlagsArgsGlobals, ConfigFile,
                            Variables0, MaybeVariables, !IO),
                        (
                            MaybeVariables = yes(Variables),
                            lookup_mmc_options(FlagsArgsGlobals, Variables,
                                MaybeMCFlags, !IO),
                            lookup_mercury_stdlib_dir(FlagsArgsGlobals, Variables,
                                MaybeMerStdLibDir, !IO),
                            detect_libgrades(FlagsArgsGlobals, MaybeMerStdLibDir,
                                DetectedGradeFlags, !IO)
                        ;
                            MaybeVariables = no,
                            MaybeMCFlags = no,
                            DetectedGradeFlags = [],
                            Variables = options_variables_init
                        )
                    ;
                        MaybeConfigFile = no,
                        DetectedGradeFlags = [],
                        Variables = options_variables_init,
                        lookup_mmc_options(FlagsArgsGlobals, Variables,
                            MaybeMCFlags, !IO)
                    )
                )
            ;
                MaybeMCFlags0 = no,
                Variables = options_variables_init,
                DetectedGradeFlags = [],
                MaybeMCFlags = no
            )
        ;
            MaybeVariables0 = no,
            Variables = options_variables_init,
            DetectedGradeFlags = [],
            MaybeMCFlags = no
        )
    ),
    (
        MaybeMCFlags = yes(MCFlags),
        % 
        % NOTE: the order of the flags here is important.  It must be:
        % 
        %   (1) flags for detected library grades
        %   (2) flags from Mercury.config and any Mercury.options files
        %   (3) flags from any command line options
        %
        % Flags given later in this list will override those given earlier.
        %
        % XXX the relationship between --no-libgrade or --libgrade options set
        % via the DEFAULT_MCFLAGS variable and detected library grades is
        % currently not defined.  It does not  matter at the moment, since
        % Mercury.config does not contain either of those two flags.
        %
        AllFlags = DetectedGradeFlags ++ MCFlags ++ OptionArgs,
        handle_given_options(AllFlags, _, _, _, Errors, ActualGlobals, !IO),

        % When computing the option arguments to pass to `--make', only include
        % the command-line arguments, not the contents of DEFAULT_MCFLAGS.
        (
            Errors = [_ | _],
            usage_errors(Errors, !IO)
        ;
            Errors = [],
            main_after_setup(DetectedGradeFlags, Variables, OptionArgs,
                NonOptionArgs, Link, ActualGlobals, !IO)
        )
    ;
        MaybeMCFlags = no,
        io.set_exit_status(1, !IO)
    ).

%-----------------------------------------------------------------------------%

main_for_make(Globals, Args, !IO) :-
    main_after_setup([], options_variables_init, [], Args, no, Globals, !IO).

%-----------------------------------------------------------------------------%

:- pred main_after_setup(list(string)::in, options_variables::in,
    list(string)::in, list(string)::in, bool::in, globals::in,
    io::di, io::uo) is det.

main_after_setup(DetectedGradeFlags, OptionVariables, OptionArgs, Args,
        Link, Globals, !IO) :-
    globals.lookup_bool_option(Globals, version, Version),
    globals.lookup_bool_option(Globals, help, Help),
    globals.lookup_bool_option(Globals, generate_source_file_mapping,
        GenerateMapping),
    globals.lookup_bool_option(Globals, output_grade_string,
        OutputGrade),
    globals.lookup_bool_option(Globals, output_link_command,
        OutputLinkCommand),
    globals.lookup_bool_option(Globals, output_shared_lib_link_command,
        OutputShLibLinkCommand),
    globals.lookup_bool_option(Globals, filenames_from_stdin,
        FileNamesFromStdin),
    globals.lookup_bool_option(Globals, output_libgrades,
        OutputLibGrades),
    globals.lookup_bool_option(Globals, output_cc, OutputCC),
    globals.lookup_bool_option(Globals, output_c_compiler_type, OutputCCType),
    globals.lookup_bool_option(Globals, output_cflags, OutputCFlags),
    globals.lookup_bool_option(Globals, output_csharp_compiler_type,
        OutputCSCType),
    globals.lookup_bool_option(Globals, output_library_link_flags,
        OutputLibraryLinkFlags),
    globals.lookup_bool_option(Globals, output_grade_defines,
        OutputGradeDefines),
    globals.lookup_bool_option(Globals, output_c_include_directory_flags,
        OutputCInclDirFlags),
    globals.lookup_bool_option(Globals, make, Make),
    globals.lookup_maybe_string_option(Globals,
        generate_standalone_interface, GenerateStandaloneInt),
    globals.lookup_bool_option(Globals,
        report_cmd_line_args, ReportCmdLineArgs),
    maybe_report_cmd_line(ReportCmdLineArgs, OptionArgs, Args, !IO),
    ( Version = yes ->
        io.stdout_stream(Stdout, !IO),
        io.set_output_stream(Stdout, OldOutputStream, !IO),
        display_compiler_version(!IO),
        io.set_output_stream(OldOutputStream, _, !IO)
    ; Help = yes ->
        io.stdout_stream(Stdout, !IO),
        io.set_output_stream(Stdout, OldOutputStream, !IO),
        long_usage(!IO),
        io.set_output_stream(OldOutputStream, _, !IO)
    ; OutputGrade = yes ->
        % When Mmake asks for the grade, it really wants
        % the directory component to use. This is consistent
        % with scripts/canonical_grade.
        grade_directory_component(Globals, Grade),
        io.stdout_stream(Stdout, !IO),
        io.write_string(Stdout, Grade, !IO),
        io.write_string(Stdout, "\n", !IO)
    ; OutputLinkCommand = yes ->
        globals.lookup_string_option(Globals, link_executable_command,
            LinkCommand),
        io.stdout_stream(Stdout, !IO),
        io.write_string(Stdout, LinkCommand, !IO),
        io.write_string(Stdout, "\n", !IO)
    ; OutputShLibLinkCommand = yes ->
        globals.lookup_string_option(Globals, link_shared_lib_command,
            LinkCommand),
        io.stdout_stream(Stdout, !IO),
        io.write_string(Stdout, LinkCommand, !IO),
        io.write_string(Stdout, "\n", !IO)
    ; OutputLibGrades = yes ->
        globals.lookup_accumulating_option(Globals, libgrades, LibGrades),
        (
            LibGrades = []
        ;
            LibGrades = [_ | _],
            io.stdout_stream(Stdout, !IO),
            io.write_list(Stdout, LibGrades, "\n", io.write_string, !IO),
            io.nl(Stdout, !IO)
        )
    ; OutputCC = yes ->
        globals.lookup_string_option(Globals, cc, CC),
        io.stdout_stream(StdOut, !IO),
        io.write_string(StdOut, CC ++ "\n", !IO)
    ; OutputCCType = yes ->
        globals.lookup_string_option(Globals, c_compiler_type, CC_Type),
        io.stdout_stream(StdOut, !IO),
        io.write_string(StdOut, CC_Type ++ "\n", !IO)
    ; OutputCFlags = yes ->
        io.stdout_stream(StdOut, !IO),
        output_c_compiler_flags(Globals, StdOut, !IO),
        io.nl(StdOut, !IO)
    ; OutputCSCType = yes ->
        globals.lookup_string_option(Globals, csharp_compiler_type, CSC_Type),
        io.stdout_stream(StdOut, !IO),
        io.write_string(StdOut, CSC_Type ++ "\n", !IO)
    ; OutputLibraryLinkFlags = yes ->
        io.stdout_stream(StdOut, !IO),
        output_library_link_flags(Globals, StdOut, !IO)
    ; OutputGradeDefines = yes ->
        io.stdout_stream(StdOut, !IO),
        output_grade_defines(Globals, StdOut, !IO)
    ; OutputCInclDirFlags = yes ->
        io.stdout_stream(StdOut, !IO),
        output_c_include_directory_flags(Globals, StdOut, !IO)
    ; GenerateMapping = yes ->
        source_file_map.write_source_file_map(Globals, Args, !IO)
    ; GenerateStandaloneInt = yes(StandaloneIntBasename) ->
        globals.get_target(Globals, Target),
        (
            ( Target = target_il
            ; Target = target_csharp
            ; Target = target_java
            ; Target = target_erlang
            ),
            NYIMsg = [
                words("Sorry,"),
                quote("--generate-standalone-interface"),
                words("is not yet supported with target language"),
                words(compilation_target_string(Target)),
                suffix(".")
            ],
            write_error_pieces_plain(Globals, NYIMsg, !IO),
            io.set_exit_status(1, !IO)
        ;
            ( Target = target_c
            ; Target = target_x86_64
            ),
            make_standalone_interface(Globals, StandaloneIntBasename, !IO)
        )
    ; Make = yes ->
        make_process_args(Globals, DetectedGradeFlags, OptionVariables,
            OptionArgs, Args, !IO)
    ; Args = [], FileNamesFromStdin = no ->
        usage(!IO)
    ;
        process_all_args(Globals, DetectedGradeFlags, OptionVariables,
            OptionArgs, Args, ModulesToLink, ExtraObjFiles, !IO),
        io.get_exit_status(ExitStatus, !IO),
        ( ExitStatus = 0 ->
            (
                Link = yes,
                ModulesToLink = [FirstModule | _]
            ->
                file_name_to_module_name(FirstModule,
                    MainModuleName),
                globals.get_target(Globals, Target),
                (
                    Target = target_java,
                    % For Java, at the "link" step we just generate a shell
                    % script; the actual linking will be done at runtime by
                    % the Java interpreter.
                    create_java_shell_script(Globals, MainModuleName,
                        Succeeded, !IO)
                ;
                    ( Target = target_c
                    ; Target = target_csharp
                    ; Target = target_il
                    ; Target = target_x86_64
                    ; Target = target_erlang
                    ),
                    compile_with_module_options(Globals, MainModuleName,
                        DetectedGradeFlags, OptionVariables, OptionArgs,
                        link_module_list(ModulesToLink, ExtraObjFiles),
                        Succeeded, !IO)
                ),
                maybe_set_exit_status(Succeeded, !IO)
            ;
                true
            )
        ;
            % If we found some errors, but the user didn't enable the `-E'
            % (`--verbose-errors') option, give them a hint about it.
            % Of course, we should only output the hint when we have further
            % information to give the user.
            globals.lookup_bool_option(Globals, verbose_errors,
                VerboseErrors),
            globals.io_get_extra_error_info(ExtraErrorInfo, !IO),
            (
                VerboseErrors = no,
                (
                    ExtraErrorInfo = yes,
                    io.write_string("For more information, " ++
                        "recompile with `-E'.\n", !IO)
                ;
                    ExtraErrorInfo = no
                )
            ;
                VerboseErrors = yes
            )
        ),
        globals.lookup_bool_option(Globals, statistics, Statistics),
        (
            Statistics = yes,
            io.report_stats("full_memory_stats", !IO)
        ;
            Statistics = no
        )
    ).

:- pred maybe_report_cmd_line(bool::in, list(string)::in, list(string)::in,
    io::di, io::uo) is det.

maybe_report_cmd_line(Report, OptionArgs, Args, !IO) :-
    (
        Report = no
    ;
        Report = yes,
        io.format("%% Command line options start\n", [], !IO),
        io.format("%% %s\n", [s(string.join_list("\n% ", OptionArgs ++ Args))],
            !IO),
        io.format("%% Command line options end\n", [], !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred process_all_args(globals::in, list(string)::in, options_variables::in,
    list(string)::in, list(string)::in, list(string)::out, list(string)::out,
    io::di, io::uo) is det.

process_all_args(Globals, DetectedGradeFlags, OptionVariables, OptionArgs,
        Args, ModulesToLink, ExtraObjFiles, !IO) :-
    process_args(Globals, DetectedGradeFlags, OptionVariables, OptionArgs,
        Args, ModulesToLink, ExtraObjFiles, !IO).

:- pred do_rename_file(globals::in, string::in, string::in, io.res::out,
    io::di, io::uo) is det.

do_rename_file(Globals, OldFileName, NewFileName, Result, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    maybe_write_string(Verbose, "% Renaming `", !IO),
    maybe_write_string(Verbose, OldFileName, !IO),
    maybe_write_string(Verbose, "' as `", !IO),
    maybe_write_string(Verbose, NewFileName, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    io.rename_file(OldFileName, NewFileName, Result0, !IO),
    (
        Result0 = error(Error0),
        maybe_write_string(VeryVerbose, " failed.\n", !IO),
        maybe_flush_output(VeryVerbose, !IO),
        io.error_message(Error0, ErrorMsg0),
        % On some systems, we need to remove any existing target file first.
        % So try again that way.
        maybe_write_string(VeryVerbose, "% Removing `", !IO),
        maybe_write_string(VeryVerbose, OldFileName, !IO),
        maybe_write_string(VeryVerbose, "'...", !IO),
        maybe_flush_output(VeryVerbose, !IO),
        io.remove_file(NewFileName, Result1, !IO),
        (
            Result1 = error(Error1),
            maybe_write_string(Verbose, " failed.\n", !IO),
            maybe_flush_output(Verbose, !IO),
            io.error_message(Error1, ErrorMsg1),
            string.append_list(["can't rename file `", OldFileName,
                "' as `", NewFileName, "': ", ErrorMsg0,
                "; and can't remove file `", NewFileName, "': ", ErrorMsg1],
                Message),
            report_error(Message, !IO),
            Result = Result1
        ;
            Result1 = ok,
            maybe_write_string(VeryVerbose, " done.\n", !IO),
            maybe_write_string(VeryVerbose, "% Renaming `", !IO),
            maybe_write_string(VeryVerbose, OldFileName, !IO),
            maybe_write_string(VeryVerbose, "' as `", !IO),
            maybe_write_string(VeryVerbose, NewFileName, !IO),
            maybe_write_string(VeryVerbose, "' again...", !IO),
            maybe_flush_output(VeryVerbose, !IO),
            io.rename_file(OldFileName, NewFileName, Result2, !IO),
            (
                Result2 = error(Error2),
                maybe_write_string(Verbose, " failed.\n", !IO),
                maybe_flush_output(Verbose, !IO),
                io.error_message(Error2, ErrorMsg),
                string.append_list(
                    ["can't rename file `", OldFileName, "' as `", NewFileName,
                    "': ", ErrorMsg], Message),
                report_error(Message, !IO)
            ;
                Result2 = ok,
                maybe_write_string(Verbose, " done.\n", !IO)
            ),
            Result = Result2
        )
    ;
        Result0 = ok,
        maybe_write_string(Verbose, " done.\n", !IO),
        Result = Result0
    ).

:- pred process_args_callback(list(string)::in, options_variables::in,
    list(string)::in, list(string)::in, globals::in,
    {list(string), list(string)}::out, io::di, io::uo) is det.

process_args_callback(DetectedGradeFlags, OptionVariables, OptionArgs,
        Args, Globals, {ModulesToLink, ExtraObjFiles}, !IO) :-
    process_args(Globals, DetectedGradeFlags, OptionVariables, OptionArgs,
        Args, ModulesToLink, ExtraObjFiles, !IO).

:- pred process_args(globals::in, list(string)::in, options_variables::in,
    list(string)::in, list(string)::in,
    list(string)::out, list(string)::out, io::di, io::uo) is det.

process_args(Globals, DetectedGradeFlags, OptionVariables, OptionArgs, Args,
        ModulesToLink, ExtraObjFiles, !IO) :-
    globals.lookup_bool_option(Globals, filenames_from_stdin,
        FileNamesFromStdin),
    (
        FileNamesFromStdin = yes,
        process_stdin_arg_list(Globals, DetectedGradeFlags, OptionVariables,
            OptionArgs, cord.empty, ModulesToLinkCord,
            cord.empty, ExtraObjFilesCord, !IO)
    ;
        FileNamesFromStdin = no,
        process_arg_list(Globals, DetectedGradeFlags, OptionVariables,
            OptionArgs, Args, cord.empty, ModulesToLinkCord,
            cord.empty, ExtraObjFilesCord, !IO)
    ),
    ModulesToLink = cord.list(ModulesToLinkCord),
    ExtraObjFiles = cord.list(ExtraObjFilesCord).

:- pred process_stdin_arg_list(globals::in, list(string)::in,
    options_variables::in, list(string)::in,
    cord(string)::in, cord(string)::out,
    cord(string)::in, cord(string)::out,
    io::di, io::uo) is det.

process_stdin_arg_list(Globals, DetectedGradeFlags, OptionVariables,
        OptionArgs, !Modules, !ExtraObjFiles, !IO) :-
    ( is_empty(!.Modules) ->
        true
    ;
        garbage_collect(!IO)
    ),
    io.read_line_as_string(FileResult, !IO),
    (
        FileResult = ok(Line),
        Arg = string.rstrip(Line),
        process_arg(Globals, DetectedGradeFlags, OptionVariables, OptionArgs,
            Arg, ArgModules, ArgExtraObjFiles, !IO),
        !:Modules = !.Modules ++ from_list(ArgModules),
        !:ExtraObjFiles = !.ExtraObjFiles ++ from_list(ArgExtraObjFiles),
        process_stdin_arg_list(Globals, DetectedGradeFlags, OptionVariables,
            OptionArgs, !Modules, !ExtraObjFiles, !IO)
    ;
        FileResult = eof
    ;
        FileResult = error(Error),
        io.error_message(Error, Msg),
        io.write_string("Error reading module name: ", !IO),
        io.write_string(Msg, !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred process_arg_list(globals::in, list(string)::in, options_variables::in,
    list(string)::in, list(string)::in,
    cord(string)::in, cord(string)::out, cord(string)::in, cord(string)::out,
    io::di, io::uo) is det.

process_arg_list(_, _, _, _,
        [], !Modules, !ExtraObjFiles, !IO).
process_arg_list(Globals, DetectedGradeFlags, OptionVariables, OptionArgs,
        [Arg | Args], !Modules, !ExtraObjFiles, !IO) :-
    process_arg(Globals, DetectedGradeFlags, OptionVariables, OptionArgs, Arg,
        ArgModules, ArgExtraObjFiles, !IO),
    (
        Args = [_ | _],
        garbage_collect(!IO)
    ;
        Args = []
    ),
    !:Modules = !.Modules ++ from_list(ArgModules),
    !:ExtraObjFiles = !.ExtraObjFiles ++ from_list(ArgExtraObjFiles),
    process_arg_list(Globals, DetectedGradeFlags, OptionVariables, OptionArgs,
        Args, !Modules, !ExtraObjFiles, !IO).

    % Figure out whether the argument is a module name or a file name.
    % Open the specified file or module, and process it.
    % Return the list of modules (including sub-modules,
    % if they were compiled to separate object files)
    % that should be linked into the final executable.

:- pred process_arg(globals::in, list(string)::in, options_variables::in,
    list(string)::in, string::in, list(string)::out, list(string)::out,
    io::di, io::uo) is det.

process_arg(Globals, DetectedGradeFlags, OptionVariables, OptionArgs, Arg,
        ModulesToLink, ExtraObjFiles, !IO) :-
    FileOrModule = string_to_file_or_module(Arg),
    globals.lookup_bool_option(Globals, invoked_by_mmc_make, InvokedByMake),
    (
        InvokedByMake = no,
        build_with_module_options_args(Globals,
            file_or_module_to_module_name(FileOrModule),
            DetectedGradeFlags, OptionVariables, OptionArgs, [],
            process_arg_build(FileOrModule, OptionArgs),
            _, [], MaybeTuple, !IO),
        (
            MaybeTuple = yes(Tuple),
            Tuple = {ModulesToLink, ExtraObjFiles}
        ;
            MaybeTuple = no,
            ModulesToLink = [],
            ExtraObjFiles = []
        )
    ;
        InvokedByMake = yes,
        % `mmc --make' has already set up the options.
        process_arg_2(Globals, OptionArgs, FileOrModule, ModulesToLink,
            ExtraObjFiles, !IO)
    ).

:- pred process_arg_build(file_or_module::in,
    list(string)::in, globals::in, list(string)::in, bool::out,
    list(string)::in, {list(string), list(string)}::out,
    io::di, io::uo) is det.

process_arg_build(FileOrModule, OptionArgs, Globals, _, yes, _,
        {Modules, ExtraObjFiles}, !IO) :-
    process_arg_2(Globals, OptionArgs, FileOrModule, Modules, ExtraObjFiles,
        !IO).

:- pred process_arg_2(globals::in, list(string)::in,
    file_or_module::in, list(string)::out, list(string)::out,
    io::di, io::uo) is det.

process_arg_2(Globals, OptionArgs, FileOrModule, ModulesToLink, ExtraObjFiles,
        !IO) :-
    globals.lookup_bool_option(Globals, generate_dependencies, GenerateDeps),
    (
        GenerateDeps = yes,
        ModulesToLink = [],
        ExtraObjFiles = [],
        (
            FileOrModule = fm_file(FileName),
            generate_file_dependencies(Globals, FileName, !IO)
        ;
            FileOrModule = fm_module(ModuleName),
            generate_module_dependencies(Globals, ModuleName, !IO)
        )
    ;
        GenerateDeps = no,
        globals.lookup_bool_option(Globals, generate_dependency_file,
            GenerateDepFile),
        (
            GenerateDepFile = yes,
            ModulesToLink = [],
            ExtraObjFiles = [],
            (
                FileOrModule = fm_file(FileName),
                generate_file_dependency_file(Globals, FileName, !IO)
            ;
                FileOrModule = fm_module(ModuleName),
                generate_module_dependency_file(Globals, ModuleName, !IO)
            )
        ;
            GenerateDepFile = no,
            process_module(Globals, OptionArgs, FileOrModule, ModulesToLink,
                ExtraObjFiles, !IO)
        )
    ).

:- type file_or_module
    --->    fm_file(file_name)
    ;       fm_module(module_name).

:- func string_to_file_or_module(string) = file_or_module.

string_to_file_or_module(String) = FileOrModule :-
    ( string.remove_suffix(String, ".m", FileName) ->
        % If the argument name ends in `.m', then we assume it is a file name.
        FileOrModule = fm_file(FileName)
    ;
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

:- pred read_module_or_file(globals::in, globals::out, file_or_module::in,
    maybe_return_timestamp::in, module_name::out, file_name::out,
    maybe(timestamp)::out, list(item)::out,
    list(error_spec)::out, module_error::out,
    have_read_module_map::in, have_read_module_map::out,
    io::di, io::uo) is det.

read_module_or_file(Globals0, Globals, FileOrModuleName, ReturnTimestamp,
        ModuleName, SourceFileName, MaybeTimestamp, Items, Specs, Error,
        !HaveReadModuleMap, !IO) :-
    (
        FileOrModuleName = fm_module(ModuleName),
        globals.lookup_bool_option(Globals0, verbose, Verbose),
        maybe_write_string(Verbose, "% Parsing module `", !IO),
        ModuleNameString = sym_name_to_string(ModuleName),
        maybe_write_string(Verbose, ModuleNameString, !IO),
        maybe_write_string(Verbose, "' and imported interfaces...\n", !IO),
        (
            % Avoid rereading the module if it was already read
            % by recompilation_version.m.
            find_read_module(!.HaveReadModuleMap, ModuleName, ".m",
                ReturnTimestamp, ItemsPrime, SpecsPrime, ErrorPrime,
                SourceFileNamePrime, MaybeTimestampPrime)
        ->
            % XXX When we have read the module before, it *could* have had
            % problems that should cause smart recompilation to be disabled.
            Globals = Globals0,
            map.delete(ModuleName - ".m", !HaveReadModuleMap),
            Items = ItemsPrime,
            Specs = SpecsPrime,
            Error = ErrorPrime,
            SourceFileName = SourceFileNamePrime,
            MaybeTimestamp = MaybeTimestampPrime
        ;
            % We don't search `--search-directories' for source files
            % because that can result in the generated interface files
            % being created in the wrong directory.
            read_module(Globals0, ModuleName, ".m", "Reading module",
                do_not_search, ReturnTimestamp, Items, Specs, Error,
                SourceFileName, MaybeTimestamp, !IO),
            io_get_disable_smart_recompilation(DisableSmart, !IO),
            (
                DisableSmart = yes,
                globals.set_option(smart_recompilation, bool(no),
                    Globals0, Globals)
            ;
                DisableSmart = no,
                Globals = Globals0
            )
        ),
        globals.lookup_bool_option(Globals, statistics, Stats),
        maybe_report_stats(Stats, !IO)
    ;
        FileOrModuleName = fm_file(FileName),
        globals.lookup_bool_option(Globals0, verbose, Verbose),
        maybe_write_string(Verbose, "% Parsing file `", !IO),
        maybe_write_string(Verbose, FileName, !IO),
        maybe_write_string(Verbose, "' and imported interfaces...\n", !IO),

        file_name_to_module_name(FileName, DefaultModuleName),
        (
            % Avoid rereading the module if it was already read
            % by recompilation_version.m.
            find_read_module(!.HaveReadModuleMap, DefaultModuleName, ".m",
                ReturnTimestamp, ItemsPrime, SpecsPrime, ErrorPrime,
                _, MaybeTimestampPrime)
        ->
            % XXX When we have read the module before, it *could* have had
            % problems that should cause smart recompilation to be disabled.
            Globals = Globals0,
            map.delete(ModuleName - ".m", !HaveReadModuleMap),
            ModuleName = DefaultModuleName,
            Items = ItemsPrime,
            Specs = SpecsPrime,
            Error = ErrorPrime,
            MaybeTimestamp = MaybeTimestampPrime
        ;
            % We don't search `--search-directories' for source files
            % because that can result in the generated interface files
            % being created in the wrong directory.
            read_module_from_file(Globals0, FileName, ".m", "Reading file",
                do_not_search, ReturnTimestamp, Items, Specs, Error,
                ModuleName, MaybeTimestamp, !IO),
            io_get_disable_smart_recompilation(DisableSmart, !IO),
            (
                DisableSmart = yes,
                globals.set_option(smart_recompilation, bool(no),
                    Globals0, Globals)
            ;
                DisableSmart = no,
                Globals = Globals0
            ),

            % XXX If the module name doesn't match the file name, the compiler
            % won't be able to find the `.used' file (the name of the `.used'
            % file is derived from the module name not the file name).
            % This will be fixed when mmake functionality is moved into
            % the compiler.

            globals.lookup_bool_option(Globals, smart_recompilation, Smart),
            (
                Smart = yes,
                ModuleName \= DefaultModuleName
                % We want to give this warning even if smart recompilation
                % was disabled before this.
            ->
                globals.lookup_bool_option(Globals, warn_smart_recompilation,
                    Warn),
                (
                    Warn = yes,
                    Pieces =
                        [words("Warning:"),
                        words("module name does not match file name: "), nl,
                        fixed(FileName), words("contains module"),
                        sym_name(ModuleName), suffix("."), nl,
                        words("Smart recompilation will not work unless"),
                        words("a module name to file name mapping is created"),
                        words("using `mmc -f *.m'."), nl],
                    write_error_pieces_plain(Globals, Pieces, !IO),
                    record_warning(Globals, !IO)
                ;
                    Warn = no
                ),
                io_set_disable_smart_recompilation(yes, !IO)
            ;
                true
            )
        ),
        globals.lookup_bool_option(Globals, detailed_statistics, Stats),
        maybe_report_stats(Stats, !IO),
        SourceFileName = FileName ++ ".m"
    ).

:- func version_numbers_return_timestamp(bool) = maybe_return_timestamp.

version_numbers_return_timestamp(no) = do_not_return_timestamp.
version_numbers_return_timestamp(yes) = do_return_timestamp.

:- pred process_module(globals::in, list(string)::in, file_or_module::in,
    list(string)::out, list(string)::out, io::di, io::uo) is det.

process_module(Globals0, OptionArgs, FileOrModule, ModulesToLink,
        ExtraObjFiles, !IO) :-
    globals.lookup_bool_option(Globals0, halt_at_syntax_errors, HaltSyntax),
    globals.lookup_bool_option(Globals0, make_interface, MakeInterface),
    globals.lookup_bool_option(Globals0, make_short_interface,
        MakeShortInterface),
    globals.lookup_bool_option(Globals0, make_private_interface,
        MakePrivateInterface),
    globals.lookup_bool_option(Globals0, convert_to_mercury,
        ConvertToMercury),
    globals.lookup_bool_option(Globals0, generate_item_version_numbers,
        GenerateVersionNumbers),
    (
        ( MakeInterface = yes ->
            ProcessModule = call_make_interface(Globals0),
            ReturnTimestamp =
                version_numbers_return_timestamp(GenerateVersionNumbers)
        ; MakeShortInterface = yes ->
            ProcessModule = call_make_short_interface(Globals0),
            ReturnTimestamp = do_not_return_timestamp
        ; MakePrivateInterface = yes ->
            ProcessModule = call_make_private_interface(Globals0),
            ReturnTimestamp =
                version_numbers_return_timestamp(GenerateVersionNumbers)
        ;
            fail
        )
    ->
        read_module_or_file(Globals0, Globals, FileOrModule, ReturnTimestamp,
            ModuleName, FileName, MaybeTimestamp, Items, Specs0, Error,
            map.init, _, !IO),
        ( halt_at_module_error(HaltSyntax, Error) ->
            true
        ;
            split_into_submodules(ModuleName, Items, SubModuleList,
                Specs0, Specs),
            % XXX _NumErrors
            write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors,
                !IO),
            list.foldl(
                apply_process_module(ProcessModule, FileName, ModuleName,
                    MaybeTimestamp),
                SubModuleList, !IO)
        ),
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        ConvertToMercury = yes
    ->
        read_module_or_file(Globals0, Globals, FileOrModule,
            do_not_return_timestamp, ModuleName, _, _, Items, Specs, Error,
            map.init, _, !IO),
        % XXX _NumErrors
        write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors, !IO),
        ( halt_at_module_error(HaltSyntax, Error) ->
            true
        ;
            module_name_to_file_name(Globals, ModuleName, ".ugly",
                do_create_dirs, OutputFileName, !IO),
            convert_to_mercury(Globals, ModuleName, OutputFileName, Items, !IO)
        ),
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        globals.lookup_bool_option(Globals0, smart_recompilation, Smart0),
        io_get_disable_smart_recompilation(DisableSmart, !IO),
        (
            DisableSmart = yes,
            globals.set_option(smart_recompilation, bool(no),
                Globals0, Globals),
            Smart = no
        ;
            DisableSmart = no,
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
            find_smart_recompilation_target_files(Globals, FindTargetFiles),
            find_timestamp_files(Globals, FindTimestampFiles),
            recompilation.check.should_recompile(Globals, ModuleName,
                FindTargetFiles, FindTimestampFiles, ModulesToRecompile,
                HaveReadModuleMap, !IO)
        ;
            Smart = no,
            map.init(HaveReadModuleMap),
            ModulesToRecompile = all_modules
        ),
        ( ModulesToRecompile = some_modules([]) ->
            % XXX Currently smart recompilation is disabled if mmc is linking
            % the executable because it doesn't know how to check whether
            % all the necessary intermediate files are present and up-to-date.
            ModulesToLink = [],
            ExtraObjFiles = []
        ;
            process_module_2(Globals, OptionArgs, FileOrModule,
                ModulesToRecompile, HaveReadModuleMap, ModulesToLink,
                ExtraObjFiles, !IO)
        )
    ).

:- pred apply_process_module(
    pred(file_name, module_name, maybe(timestamp),
        pair(module_name, list(item)), io, io)::
        in(pred(in, in, in, in, di, uo) is det),
    file_name::in, module_name::in, maybe(timestamp)::in,
    pair(module_name, list(item))::in, io::di, io::uo) is det.

apply_process_module(ProcessModule, FileName, ModuleName, MaybeTimestamp,
        SubModule, !IO) :-
    ProcessModule(FileName, ModuleName, MaybeTimestamp, SubModule, !IO).

:- pred process_module_2_callback(list(string)::in, file_or_module::in,
    modules_to_recompile::in, have_read_module_map::in, globals::in,
    {list(string), list(string)}::out, io::di, io::uo) is det.

process_module_2_callback(OptionArgs, FileOrModule, MaybeModulesToRecompile,
        HaveReadModuleMap0, Globals, Result, !IO) :-
    process_module_2(Globals, OptionArgs, FileOrModule,
        MaybeModulesToRecompile, HaveReadModuleMap0, ModulesToLink,
        ExtraObjFiles, !IO),
    Result = {ModulesToLink, ExtraObjFiles}.

:- pred process_module_2(globals::in, list(string)::in, file_or_module::in,
    modules_to_recompile::in, have_read_module_map::in,
    list(string)::out, list(string)::out, io::di, io::uo) is det.

process_module_2(Globals0, OptionArgs, FileOrModule, MaybeModulesToRecompile,
        HaveReadModuleMap0, ModulesToLink, ExtraObjFiles, !IO) :-
    globals.lookup_bool_option(Globals0, make_short_interface,
        MakeShortInt),
    globals.lookup_bool_option(Globals0, make_interface,
        MakeInt),
    globals.lookup_bool_option(Globals0, make_optimization_interface,
        MakeOptInt),
    globals.lookup_bool_option(Globals0, make_transitive_opt_interface,
        MakeTransOptInt),
    globals.lookup_bool_option(Globals0, make_analysis_registry,
        MakeAnalysisRegistry),
    globals.lookup_bool_option(Globals0, make_xml_documentation,
        MakeXmlDocumentation),
    bool.or_list([MakeShortInt, MakeInt, MakeOptInt, MakeTransOptInt,
        MakeAnalysisRegistry, MakeXmlDocumentation], DirectReport),
    (
        DirectReport = yes
    ;
        DirectReport = no,
        globals.lookup_bool_option(Globals0,
            report_cmd_line_args_in_doterr, ReportCmdLineArgsDotErr),
        maybe_report_cmd_line(ReportCmdLineArgsDotErr, OptionArgs, [], !IO)
    ),

    read_module_or_file(Globals0, Globals, FileOrModule, do_return_timestamp,
        ModuleName, FileName, MaybeTimestamp, Items, Specs0, Error,
        HaveReadModuleMap0, HaveReadModuleMap, !IO),
    globals.lookup_bool_option(Globals, halt_at_syntax_errors, HaltSyntax),
    ( halt_at_module_error(HaltSyntax, Error) ->
        % XXX _NumErrors
        write_error_specs(Specs0, Globals, 0, _NumWarnings, 0, _NumErrors,
            !IO),
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        split_into_submodules(ModuleName, Items, SubModuleList0,
            Specs0, Specs1),
        (
            MaybeModulesToRecompile = some_modules(ModulesToRecompile),
            ToRecompile = (pred((SubModule - _)::in) is semidet :-
                list.member(SubModule, ModulesToRecompile)
            ),
            list.filter(ToRecompile, SubModuleList0, SubModuleListToCompile)
        ;
            MaybeModulesToRecompile = all_modules,
            SubModuleListToCompile = SubModuleList0
        ),
        assoc_list.keys(SubModuleList0, NestedSubModules0),
        list.delete_all(NestedSubModules0, ModuleName, NestedSubModules),

        find_timestamp_files(Globals, FindTimestampFiles),

        globals.lookup_bool_option(Globals, trace_prof, TraceProf),

        (
            non_traced_mercury_builtin_module(ModuleName),
            not (
                ModuleName = mercury_profiling_builtin_module,
                TraceProf = yes
            )
        ->
            % Some predicates in the builtin modules are missing typeinfo
            % arguments, which means that execution tracing will not work
            % on them. Predicates defined there should never be part of
            % an execution trace anyway; they are effectively language
            % primitives. (They may still be parts of stack traces.)
            globals.set_option(trace_stack_layout, bool(no),
                Globals, GlobalsNoTrace0),
            globals.set_trace_level_none(
                GlobalsNoTrace0, GlobalsNoTrace),
            GlobalsToUse = GlobalsNoTrace
        ;
            GlobalsToUse = Globals
        ),
        compile_all_submodules(GlobalsToUse, FileName, ModuleName,
            NestedSubModules, MaybeTimestamp, HaveReadModuleMap,
            FindTimestampFiles, SubModuleListToCompile, Specs1,
            ModulesToLink, ExtraObjFiles, !IO)
    ).

    % For the MLDS->C and LLDS->C back-ends, we currently
    % compile each sub-module to its own C file.
    % XXX it would be better to do something like
    %
    %   list.map2_foldl(compile_to_llds, SubModuleList,
    %       LLDS_FragmentList),
    %   merge_llds_fragments(LLDS_FragmentList, LLDS),
    %   output_pass(LLDS_FragmentList)
    %
    % i.e. compile nested modules to a single C file.

:- pred compile_all_submodules(globals::in, string::in, module_name::in,
    list(module_name)::in, maybe(timestamp)::in, have_read_module_map::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    list(pair(module_name, list(item)))::in, list(error_spec)::in,
    list(string)::out, list(string)::out, io::di, io::uo) is det.

compile_all_submodules(Globals, FileName, SourceFileModuleName,
        NestedSubModules, MaybeTimestamp, HaveReadModuleMap,
        FindTimestampFiles, SubModuleList, !.Specs,
        ModulesToLink, ExtraObjFiles, !IO) :-
    list.map_foldl2(
        compile(Globals, FileName, SourceFileModuleName, NestedSubModules,
            MaybeTimestamp, HaveReadModuleMap, FindTimestampFiles),
        SubModuleList, ExtraObjFileLists, !Specs, !IO),
    % XXX _NumErrors
    write_error_specs(!.Specs, Globals, 0, _NumWarnings, 0, _NumErrors, !IO),
    list.map(module_to_link, SubModuleList, ModulesToLink),
    list.condense(ExtraObjFileLists, ExtraObjFiles).

:- pred call_make_interface(globals::in, file_name::in, module_name::in,
    maybe(timestamp)::in, pair(module_name, list(item))::in, io::di, io::uo)
    is det.

call_make_interface(Globals, SourceFileName, SourceFileModuleName,
        MaybeTimestamp, ModuleName - Items, !IO) :-
    make_interface(Globals, SourceFileName, SourceFileModuleName,
        ModuleName, MaybeTimestamp, Items, !IO).

:- pred call_make_short_interface(globals::in, file_name::in, module_name::in,
    maybe(timestamp)::in, pair(module_name, list(item))::in,
    io::di, io::uo) is det.

call_make_short_interface(Globals, SourceFileName, _, _, ModuleName - Items,
        !IO) :-
    make_short_interface(Globals, SourceFileName, ModuleName, Items, !IO).

:- pred call_make_private_interface(globals::in, file_name::in,
    module_name::in, maybe(timestamp)::in, pair(module_name, list(item))::in,
    io::di, io::uo) is det.

call_make_private_interface(Globals, SourceFileName, SourceFileModuleName,
        MaybeTimestamp, ModuleName - Items, !IO) :-
    make_private_interface(Globals, SourceFileName, SourceFileModuleName,
        ModuleName, MaybeTimestamp, Items, !IO).

:- pred halt_at_module_error(bool::in, module_error::in) is semidet.

halt_at_module_error(_, fatal_module_errors).
halt_at_module_error(HaltSyntax, some_module_errors) :- HaltSyntax = yes.

:- pred module_to_link(pair(module_name, list(item))::in, string::out) is det.

module_to_link(ModuleName - _Items, ModuleToLink) :-
    module_name_to_file_name_stem(ModuleName, ModuleToLink).

:- type compile == pred(globals, bool, io, io).
:- inst compile == (pred(in, out, di, uo) is det).

:- pred compile_with_module_options(globals::in, module_name::in,
    list(string)::in, options_variables::in, list(string)::in,
    compile::in(compile), bool::out, io::di, io::uo) is det.

compile_with_module_options(Globals, ModuleName, DetectedGradeFlags,
        OptionVariables, OptionArgs, Compile, Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, invoked_by_mmc_make, InvokedByMake),
    (
        InvokedByMake = yes,
        % `mmc --make' has already set up the options.
        Compile(Globals, Succeeded, !IO)
    ;
        InvokedByMake = no,
        Builder =
            (pred(BuildGlobals::in, _::in, Succeeded0::out, X::in, X::out,
                    IO0::di, IO::uo) is det :-
                Compile(BuildGlobals, Succeeded0, IO0, IO)
            ),
        build_with_module_options_args(Globals, ModuleName, DetectedGradeFlags,
            OptionVariables, OptionArgs, [], Builder, Succeeded, unit, _, !IO)
    ).

%-----------------------------------------------------------------------------%

    % Return a closure which will work out what the target files are for
    % a module, so recompilation_check.m can check that they are
    % up-to-date which deciding whether compilation is necessary.
    % Note that `--smart-recompilation' only works with
    % `--target-code-only', which is always set when the compiler is
    % invoked by mmake. Using smart recompilation without using mmake is
    % not a sensible thing to do.  handle_options.m will disable smart
    % recompilation if `--target-code-only' is not set.
    %
:- pred find_smart_recompilation_target_files(globals::in,
    find_target_file_names::out(find_target_file_names)) is det.

find_smart_recompilation_target_files(Globals, FindTargetFiles) :-
    globals.get_target(Globals, CompilationTarget),
    ( CompilationTarget = target_c, TargetSuffix = ".c"
    ; CompilationTarget = target_il, TargetSuffix = ".il"
    ; CompilationTarget = target_csharp, TargetSuffix = ".cs"
    ; CompilationTarget = target_java, TargetSuffix = ".java"
    ; CompilationTarget = target_x86_64, TargetSuffix = ".s"
    ; CompilationTarget = target_erlang, TargetSuffix = ".erl"
    ),
    FindTargetFiles = usual_find_target_files(Globals, TargetSuffix).

:- pred usual_find_target_files(globals::in,
    string::in, module_name::in, list(file_name)::out,
    io::di, io::uo) is det.

usual_find_target_files(Globals, TargetSuffix, ModuleName, TargetFiles,
        !IO) :-
    % XXX Should we check the generated header files?
    module_name_to_file_name(Globals, ModuleName, TargetSuffix,
        do_create_dirs, FileName, !IO),
    TargetFiles = [FileName].

:- pred find_timestamp_files(globals::in,
    find_timestamp_file_names::out(find_timestamp_file_names)) is det.

find_timestamp_files(Globals, FindTimestampFiles) :-
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        TimestampSuffix = ".c_date"
    ;
        CompilationTarget = target_il,
        TimestampSuffix = ".il_date"
    ;
        CompilationTarget = target_csharp,
        TimestampSuffix = ".cs_date"
    ;
        CompilationTarget = target_java,
        TimestampSuffix = ".java_date"
    ;
        CompilationTarget = target_x86_64,
        TimestampSuffix = ".s_date"
    ;
        CompilationTarget = target_erlang,
        TimestampSuffix = ".erl_date"
    ),
    FindTimestampFiles = find_timestamp_files_2(Globals, TimestampSuffix).

:- pred find_timestamp_files_2(globals::in, string::in, module_name::in,
    list(file_name)::out, io::di, io::uo) is det.

find_timestamp_files_2(Globals, TimestampSuffix, ModuleName, TimestampFiles,
        !IO) :-
    module_name_to_file_name(Globals, ModuleName, TimestampSuffix,
        do_create_dirs, FileName, !IO),
    TimestampFiles = [FileName].

%-----------------------------------------------------------------------------%

    % Given a fully expanded module (i.e. a module name and a list of all
    % the items in the module and any of its imports), compile it.

    % Stage number assignments:
    %
    %     1 to  99  front end pass
    %   100 to 299  middle pass
    %   300 to 399  LLDS back end pass
    %   400 to 499  MLDS back end pass
    %   500 to 599  bytecode back end pass
    %
    % The initial arrangement has the stage numbers increasing by five
    % so that new stages can be slotted in without too much trouble.

:- pred compile(globals::in, file_name::in, module_name::in,
    list(module_name)::in, maybe(timestamp)::in, have_read_module_map::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    pair(module_name, list(item))::in, list(string)::out,
    list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

compile(Globals, SourceFileName, SourceFileModuleName, NestedSubModules0,
        MaybeTimestamp, HaveReadModuleMap, FindTimestampFiles,
        ModuleName - Items, ExtraObjFiles, !Specs, !IO) :-
    check_for_no_exports(Globals, Items, ModuleName, !Specs, !IO),
    ( ModuleName = SourceFileModuleName ->
        NestedSubModules = NestedSubModules0
    ;
        NestedSubModules = []
    ),
    grab_imported_modules(Globals, SourceFileName, SourceFileModuleName,
        ModuleName, NestedSubModules, HaveReadModuleMap, MaybeTimestamp,
        Items, Module, !IO),
    module_and_imports_get_results(Module, _, ImportedSpecs, Error),
    !:Specs = ImportedSpecs ++ !.Specs,
    (
        ( Error = no_module_errors
        ; Error = some_module_errors
        ),
        mercury_compile(Globals, Module, NestedSubModules, FindTimestampFiles,
            ExtraObjFiles, no_prev_dump, _, !Specs, !IO)
    ;
        Error = fatal_module_errors,
        ExtraObjFiles = []
    ).

:- pred mercury_compile(globals::in, module_and_imports::in,
    list(module_name)::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    list(string)::out, dump_info::in, dump_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

mercury_compile(Globals, ModuleAndImports, NestedSubModules,
        FindTimestampFiles, ExtraObjFiles, !DumpInfo, !Specs, !IO) :-
    module_and_imports_get_module_name(ModuleAndImports, ModuleName),
    % If we are only typechecking or error checking, then we should not
    % modify any files, this includes writing to .d files.
    globals.lookup_bool_option(Globals, typecheck_only, TypeCheckOnly),
    globals.lookup_bool_option(Globals, errorcheck_only, ErrorCheckOnly),
    bool.or(TypeCheckOnly, ErrorCheckOnly, DontWriteDFile),
    pre_hlds_pass(Globals, ModuleAndImports, DontWriteDFile, HLDS1, QualInfo,
        MaybeTimestamps, UndefTypes, UndefModes, Errors1, !DumpInfo,
        !Specs, !IO),
    frontend_pass(QualInfo, UndefTypes, UndefModes, Errors1, Errors2,
        HLDS1, HLDS20, !DumpInfo, !Specs, !IO),
    (
        Errors1 = no,
        Errors2 = no,
        contains_errors(Globals, !.Specs) = no
    ->
        globals.lookup_bool_option(Globals, verbose, Verbose),
        globals.lookup_bool_option(Globals, statistics, Stats),
        maybe_write_dependency_graph(Verbose, Stats, HLDS20, HLDS21, !IO),
        globals.lookup_bool_option(Globals, make_optimization_interface,
            MakeOptInt),
        globals.lookup_bool_option(Globals, make_transitive_opt_interface,
            MakeTransOptInt),
        globals.lookup_bool_option(Globals, make_analysis_registry,
            MakeAnalysisRegistry),
        globals.lookup_bool_option(Globals, make_xml_documentation,
            MakeXmlDocumentation),
        ( TypeCheckOnly = yes ->
            ExtraObjFiles = []
        ; ErrorCheckOnly = yes ->
            % We may still want to run `unused_args' so that we get
            % the appropriate warnings.
            globals.lookup_bool_option(Globals, warn_unused_args, UnusedArgs),
            (
                UnusedArgs = yes,
                globals.set_option(optimize_unused_args, bool(no),
                    Globals, NoOptUnusedArgsGlobals),
                module_info_set_globals(NoOptUnusedArgsGlobals,
                    HLDS21, HLDS21a),
                maybe_unused_args(Verbose, Stats, HLDS21a, _HLDS22, !IO)
            ;
                UnusedArgs = no
            ),
            ExtraObjFiles = []
        ; MakeOptInt = yes ->
            % Only run up to typechecking when making the .opt file.
            ExtraObjFiles = []
        ; MakeTransOptInt = yes ->
            output_trans_opt_file(HLDS21, !DumpInfo, !IO),
            ExtraObjFiles = []
        ; MakeAnalysisRegistry = yes ->
            prepare_for_intermodule_analysis(Globals, Verbose, Stats,
                HLDS21, HLDS22, !IO),
            output_analysis_file(HLDS22, !DumpInfo, !IO),
            ExtraObjFiles = []
        ; MakeXmlDocumentation = yes ->
            xml_documentation(HLDS21, !IO),
            ExtraObjFiles = []
        ;
            maybe_prepare_for_intermodule_analysis(Globals, Verbose, Stats,
                HLDS21, HLDS22, !IO),
            mercury_compile_after_front_end(NestedSubModules,
                FindTimestampFiles, MaybeTimestamps, ModuleName, HLDS22,
                !.Specs, ExtraObjFiles, !DumpInfo, !IO)
        )
    ;
        % If the number of errors is > 0, make sure that the compiler
        % exits with a non-zero exit status.
        io.get_exit_status(ExitStatus, !IO),
        ( ExitStatus = 0 ->
            io.set_exit_status(1, !IO)
        ;
            true
        ),
        ExtraObjFiles = []
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_prepare_for_intermodule_analysis(globals::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_prepare_for_intermodule_analysis(Globals, Verbose, Stats, !HLDS, !IO) :-
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    (
        IntermodAnalysis = yes,
        prepare_for_intermodule_analysis(Globals, Verbose, Stats, !HLDS, !IO)
    ;
        IntermodAnalysis = no
    ).

:- pred prepare_for_intermodule_analysis(globals::in, bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

prepare_for_intermodule_analysis(Globals, Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Preparing for intermodule analysis...\n",
        !IO),

    module_info_get_all_deps(!.HLDS, ModuleNames),

    globals.lookup_accumulating_option(Globals, local_module_id,
        LocalModulesList),
    SymNames = list.map(string_to_sym_name, LocalModulesList),
    LocalModuleNames = set.from_list(SymNames),

    module_info_get_analysis_info(!.HLDS, AnalysisInfo0),
    prepare_intermodule_analysis(Globals, ModuleNames, LocalModuleNames,
        AnalysisInfo0, AnalysisInfo, !IO),
    module_info_set_analysis_info(AnalysisInfo, !HLDS),

    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%

:- pred mercury_compile_after_front_end(list(module_name)::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    maybe(module_timestamps)::in, module_name::in, module_info::in,
    list(error_spec)::in, list(string)::out, dump_info::in, dump_info::out,
    io::di, io::uo) is det.

mercury_compile_after_front_end(NestedSubModules, FindTimestampFiles,
        MaybeTimestamps, ModuleName, !.HLDS, Specs, ExtraObjFiles,
        !DumpInfo, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),
    maybe_output_prof_call_graph(Verbose, Stats, !HLDS, !IO),
    middle_pass(ModuleName, !HLDS, !DumpInfo, !IO),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    globals.get_target(Globals, Target),
    globals.lookup_bool_option(Globals, target_code_only, TargetCodeOnly),

    % Remove any existing `.used' file before writing the output file.
    % This avoids leaving the old `used' file lying around if compilation
    % is interrupted after the new output file is written but before the new
    % `.used' file is written.

    module_name_to_file_name(Globals, ModuleName, ".used",
        do_not_create_dirs, UsageFileName, !IO),
    io.remove_file(UsageFileName, _, !IO),

    FrontEndErrors = contains_errors(Globals, Specs),
    module_info_get_num_errors(!.HLDS, NumErrors),
    (
        FrontEndErrors = no,
        NumErrors = 0
    ->
        (
            ( Target = target_c
            ; Target = target_x86_64
            ),
            % Produce the grade independent header file <module>.mh
            % containing function prototypes for the procedures
            % referred to by foreign_export pragmas.
            export.get_foreign_export_decls(!.HLDS, ExportDecls),
            export.produce_header_file(!.HLDS, ExportDecls, ModuleName, !IO)
        ;
            ( Target = target_java
            ; Target = target_csharp
            ; Target = target_il
            ; Target = target_erlang
            )
        ),
        (
            Target = target_il,
            mlds_backend(!.HLDS, _, MLDS, !DumpInfo, !IO),
            mlds_to_il_assembler(Globals, MLDS, TargetCodeSucceeded, !IO),
            (
                TargetCodeSucceeded = yes,
                TargetCodeOnly = no
            ->
                HasMain = mlds_has_main(MLDS),
                io.output_stream(OutputStream, !IO),
                il_assemble(OutputStream, ModuleName, HasMain,
                    Globals, Succeeded, !IO),
                maybe_set_exit_status(Succeeded, !IO)
            ;
                Succeeded = TargetCodeSucceeded
            ),
            ExtraObjFiles = []
        ;
            Target = target_csharp,
            mlds_backend(!.HLDS, _, MLDS, !DumpInfo, !IO),
            mlds_to_csharp(!.HLDS, MLDS, Succeeded, !IO),
            ExtraObjFiles = []
        ;
            Target = target_java,
            mlds_backend(!.HLDS, _, MLDS, !DumpInfo, !IO),
            mlds_to_java(!.HLDS, MLDS, TargetCodeSucceeded, !IO),
            (
                TargetCodeSucceeded = yes,
                TargetCodeOnly = no
            ->
                io.output_stream(OutputStream, !IO),
                module_name_to_file_name(Globals, ModuleName, ".java",
                    do_not_create_dirs, JavaFile, !IO),
                compile_java_files(OutputStream, [JavaFile],
                    Globals, Succeeded, !IO),
                maybe_set_exit_status(Succeeded, !IO)
            ;
                Succeeded = TargetCodeSucceeded
            ),
            ExtraObjFiles = []
        ;
            Target = target_c,
            (
                HighLevelCode = yes,
                mlds_backend(!.HLDS, _, MLDS, !DumpInfo, !IO),
                mlds_to_high_level_c(Globals, MLDS, TargetCodeSucceeded, !IO),
                (
                    TargetCodeSucceeded = yes,
                    TargetCodeOnly = no
                ->
                    module_name_to_file_name(Globals, ModuleName, ".c",
                        do_not_create_dirs, C_File, !IO),
                    get_linked_target_type(Globals, TargetType),
                    get_object_code_type(Globals, TargetType, PIC),
                    maybe_pic_object_file_extension(Globals, PIC, Obj),
                    module_name_to_file_name(Globals, ModuleName, Obj,
                        do_create_dirs, O_File, !IO),
                    io.output_stream(OutputStream, !IO),
                    do_compile_c_file(OutputStream, PIC,
                        C_File, O_File, Globals, Succeeded, !IO),
                    maybe_set_exit_status(Succeeded, !IO)
                ;
                    Succeeded = TargetCodeSucceeded
                ),
                ExtraObjFiles = []
            ;
                HighLevelCode = no,
                llds_backend_pass(!HLDS, GlobalData, LLDS, !DumpInfo, !IO),
                llds_output_pass(!.HLDS, GlobalData, LLDS, ModuleName,
                    Succeeded, ExtraObjFiles, !IO)
            )
        ;
            Target = target_x86_64,
            llds_backend_pass(!HLDS, _GlobalData, LLDS, !DumpInfo, !IO),
            % XXX Eventually we will call the LLDS->x86_64 asm code generator
            % here and then output the assembler. At the moment, we just output
            % the LLDS as C code.
            llds_to_x86_64_asm(!.HLDS, LLDS, X86_64_Asm),
            % XXX This should eventually be written to a file rather
            % than stdout.
            io.stdout_stream(Stdout, !IO),
            output_x86_64_asm(Stdout, X86_64_Asm, !IO),
            Succeeded = yes,
            ExtraObjFiles = []
        ;
            Target = target_erlang,
            erlang_backend(!.HLDS, ELDS, !DumpInfo, !IO),
            elds_to_erlang(!.HLDS, ELDS, Succeeded, !IO),
            ExtraObjFiles = []
        ),
        (
            Succeeded = yes,
            recompilation.usage.write_usage_file(!.HLDS, NestedSubModules,
                MaybeTimestamps, !IO),
            FindTimestampFiles(ModuleName, TimestampFiles, !IO),
            list.foldl(touch_datestamp(Globals), TimestampFiles, !IO)
        ;
            Succeeded = no
            % An error should have been reported earlier.
        )
    ;
        % If the number of errors is > 0, make sure that the compiler
        % exits with a non-zero exit status.
        io.get_exit_status(ExitStatus, !IO),
        ( ExitStatus = 0 ->
            io.set_exit_status(1, !IO)
        ;
            true
        ),
        ExtraObjFiles = []
    ).

:- pred mercury_compile_asm_c_code(globals::in, module_name::in,
    string::out, io::di, io::uo) is det.

mercury_compile_asm_c_code(Globals, ModuleName, CCode_O_File, !IO) :-
    get_linked_target_type(Globals, TargetType),
    get_object_code_type(Globals, TargetType, PIC),
    maybe_pic_object_file_extension(Globals, PIC, Obj),
    module_name_to_file_name(Globals, ModuleName, ".c",
        do_not_create_dirs, CCode_C_File, !IO),
    ForeignModuleName = foreign_language_module_name(ModuleName, lang_c),
    module_name_to_file_name(Globals, ForeignModuleName, Obj,
        do_create_dirs, CCode_O_File, !IO),
    io.output_stream(OutputStream, !IO),
    do_compile_c_file(OutputStream, PIC, CCode_C_File, CCode_O_File,
        Globals, CompileOK, !IO),
    maybe_set_exit_status(CompileOK, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred pre_hlds_pass(globals::in, module_and_imports::in, bool::in,
    module_info::out, make_hlds_qual_info::out, maybe(module_timestamps)::out,
    bool::out, bool::out, bool::out, dump_info::in, dump_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

pre_hlds_pass(Globals, ModuleAndImports0, DontWriteDFile0, HLDS1, QualInfo,
        MaybeTimestamps, UndefTypes, UndefModes, FoundError, !DumpInfo,
        !Specs, !IO) :-
    globals.lookup_bool_option(Globals, statistics, Stats),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, invoked_by_mmc_make, MMCMake),
    DontWriteDFile1 = bool.or(DontWriteDFile0, MMCMake),

    % Don't write the `.d' file when making the `.opt' file because
    % we can't work out the full transitive implementation dependencies.
    globals.lookup_bool_option(Globals, make_optimization_interface,
        MakeOptInt),
    DontWriteDFile = bool.or(DontWriteDFile1, MakeOptInt),

    module_and_imports_get_module_name(ModuleAndImports0, ModuleName),
    (
        DontWriteDFile = yes,
        % The only time the TransOptDeps are required is when creating the
        % .trans_opt file. If DontWriteDFile is yes, then error check only
        % or type-check only is enabled, so we can't be creating the
        % .trans_opt file.
        MaybeTransOptDeps = no
    ;
        DontWriteDFile = no,
        maybe_read_dependency_file(Globals, ModuleName, MaybeTransOptDeps, !IO)
    ),

    % Errors in .opt and .trans_opt files result in software errors.
    maybe_grab_optfiles(Globals, ModuleAndImports0, Verbose, MaybeTransOptDeps,
        ModuleAndImports1, IntermodError, !IO),

    % We pay attention to IntermodError instead of _Error. XXX Is this right?
    module_and_imports_get_results(ModuleAndImports1, Items1,
        ItemSpecs, _Error),
    !:Specs = ItemSpecs ++ !.Specs,
    MaybeTimestamps = ModuleAndImports1 ^ mai_maybe_timestamps,

    globals.lookup_string_option(Globals, event_set_file_name,
        EventSetFileName),
    ( EventSetFileName = "" ->
        EventSetName = "",
        EventSpecMap1 = map.init,
        EventSetErrors = no
    ;
        read_event_set(EventSetFileName, EventSetName0, EventSpecMap0,
            EventSetSpecs, !IO),
        !:Specs = EventSetSpecs ++ !.Specs,
        EventSetErrors = contains_errors(Globals, EventSetSpecs),
        (
            EventSetErrors = no,
            EventSetName = EventSetName0,
            EventSpecMap1 = EventSpecMap0
        ;
            EventSetErrors = yes,
            EventSetName = "",
            EventSpecMap1 = map.init
        )
    ),

    invoke_module_qualify_items(Globals, Items1, Items2,
        EventSpecMap1, EventSpecMap2, ModuleName, EventSetFileName,
        Verbose, Stats, MQInfo0, MQUndefTypes, MQUndefModes, !Specs, !IO),

    mq_info_get_recompilation_info(MQInfo0, RecompInfo0),
    expand_equiv_types(Globals, ModuleName, Verbose, Stats, Items2, Items,
        EventSpecMap2, EventSpecMap, EqvMap, UsedModules,
        RecompInfo0, RecompInfo, ExpandErrors, !Specs, !IO),
    mq_info_set_recompilation_info(RecompInfo, MQInfo0, MQInfo),

    EventSet = event_set(EventSetName, EventSpecMap),
    make_hlds(Globals, ModuleName, Items, EventSet, MQInfo, EqvMap,
        UsedModules, Verbose, Stats, HLDS0, QualInfo,
        MakeHLDSUndefTypes, MakeHLDSUndefModes, FoundError, !Specs, !IO),

    bool.or_list([MQUndefTypes, EventSetErrors, ExpandErrors,
        MakeHLDSUndefTypes], UndefTypes),
    bool.or(MQUndefModes, MakeHLDSUndefModes, UndefModes),

    maybe_dump_hlds(HLDS0, 1, "initial", !DumpInfo, !IO),

    (
        DontWriteDFile = yes
    ;
        DontWriteDFile = no,
        module_info_get_all_deps(HLDS0, AllDeps),
        write_dependency_file(Globals, ModuleAndImports0, AllDeps,
            MaybeTransOptDeps, !IO),
        globals.lookup_bool_option(Globals,
            generate_mmc_make_module_dependencies, OutputMMCMakeDeps),
        (
            OutputMMCMakeDeps = yes,
            make_write_module_dep_file(Globals, ModuleAndImports0, !IO)
        ;
            OutputMMCMakeDeps = no
        )
    ),

    % Only stop on syntax errors in .opt files.
    (
        ( FoundError = yes
        ; IntermodError = yes
        )
    ->
        module_info_incr_errors(HLDS0, HLDS1)
    ;
        HLDS1 = HLDS0
    ).

%-----------------------------------------------------------------------------%

:- pred invoke_module_qualify_items(globals::in,
    list(item)::in, list(item)::out, event_spec_map::in, event_spec_map::out,
    module_name::in, string::in, bool::in, bool::in, mq_info::out,
    bool::out, bool::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

invoke_module_qualify_items(Globals, Items0, Items,
        EventSpecMap0, EventSpecMap, ModuleName, EventSpecFileName,
        Verbose, Stats, MQInfo, UndefTypes, UndefModes, !Specs, !IO) :-
    maybe_write_out_errors_no_module(Verbose, Globals, !Specs, !IO),
    maybe_write_string(Verbose, "% Module qualifying items...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    module_name_to_file_name(Globals, ModuleName, ".m",
        do_not_create_dirs, FileName, !IO),
    module_qualify_items(Items0, Items, EventSpecMap0, EventSpecMap,
        Globals, ModuleName, yes(FileName), EventSpecFileName, MQInfo,
        UndefTypes, UndefModes, [], QualifySpecs),
    !:Specs = QualifySpecs ++ !.Specs,
    maybe_write_out_errors_no_module(Verbose, Globals, !Specs, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%

:- pred maybe_grab_optfiles(globals::in, module_and_imports::in, bool::in,
    maybe(list(module_name))::in, module_and_imports::out, bool::out,
    io::di, io::uo) is det.

maybe_grab_optfiles(Globals, Imports0, Verbose, MaybeTransOptDeps,
        Imports, Error, !IO) :-
    globals.lookup_bool_option(Globals, intermodule_optimization,
        IntermodOpt),
    globals.lookup_bool_option(Globals, use_opt_files, UseOptInt),
    globals.lookup_bool_option(Globals, make_optimization_interface,
        MakeOptInt),
    globals.lookup_bool_option(Globals, transitive_optimization, TransOpt),
    globals.lookup_bool_option(Globals, make_transitive_opt_interface,
        MakeTransOptInt),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    (
        ( UseOptInt = yes
        ; IntermodOpt = yes
        ; IntermodAnalysis = yes
        ),
        MakeOptInt = no
    ->
        maybe_write_string(Verbose, "% Reading .opt files...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        grab_opt_files(Globals, Imports0, Imports1, Error1, !IO),
        maybe_write_string(Verbose, "% Done.\n", !IO)
    ;
        Imports1 = Imports0,
        Error1 = no
    ),
    ( MakeTransOptInt = yes ->
        (
            MaybeTransOptDeps = yes(TransOptDeps),
            % When creating the trans_opt file, only import the
            % trans_opt files which are lower in the ordering.
            grab_trans_opt_files(Globals, TransOptDeps, Imports1, Imports,
                Error2, !IO)
        ;
            MaybeTransOptDeps = no,
            Imports = Imports1,
            Error2 = no,
            module_and_imports_get_module_name(Imports, ModuleName),
            globals.lookup_bool_option(Globals, warn_missing_trans_opt_deps,
                WarnNoTransOptDeps),
            (
                WarnNoTransOptDeps = yes,
                Pieces = [words("Warning: cannot read trans-opt dependencies"),
                    words("for module"), sym_name(ModuleName), suffix("."), nl,
                    words("You need to remake the dependencies."), nl],
                Msg = error_msg(no, do_not_treat_as_first, 0,
                    [always(Pieces)]),
                Spec = error_spec(severity_warning, phase_read_files, [Msg]),
                % XXX _NumErrors
                write_error_spec(Spec, Globals, 0, _NumWarnings, 0, _NumErrors,
                    !IO)
            ;
                WarnNoTransOptDeps = no
            )
        )
    ; MakeOptInt = yes ->
        % If we're making the `.opt' file, then we can't read any `.trans_opt'
        % files, since `.opt' files aren't allowed to depend on `.trans_opt'
        % files.
        Imports = Imports1,
        Error2 = no
    ;
        (
            TransOpt = yes,
            % If transitive optimization is enabled, but we are not creating
            % the .opt or .trans opt file, then import the trans_opt files
            % for all the modules that are imported (or used), and for all
            % ancestor modules.
            TransOptFilesList =
                [Imports0 ^ mai_parent_deps,
                Imports0 ^ mai_int_deps,
                Imports0 ^ mai_impl_deps],
            list.condense(TransOptFilesList, TransOptFiles),
            grab_trans_opt_files(Globals, TransOptFiles, Imports1, Imports,
                Error2, !IO)
        ;
            TransOpt = no,
            Imports = Imports1,
            Error2 = no
        )
    ),
    bool.or(Error1, Error2, Error).

%-----------------------------------------------------------------------------%

:- pred expand_equiv_types(globals::in, module_name::in, bool::in, bool::in,
    list(item)::in, list(item)::out, event_spec_map::in, event_spec_map::out,
    eqv_map::out, used_modules::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out, bool::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

expand_equiv_types(Globals, ModuleName, Verbose, Stats, Items0, Items,
        EventSpecMap0, EventSpecMap, EqvMap, UsedModules,
        RecompInfo0, RecompInfo, FoundError, !Specs, !IO) :-
    maybe_write_out_errors_no_module(Verbose, Globals, !Specs, !IO),
    maybe_write_string(Verbose, "% Expanding equivalence types...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    equiv_type.expand_eqv_types(ModuleName, Items0, Items,
        EventSpecMap0, EventSpecMap, EqvMap, UsedModules,
        RecompInfo0, RecompInfo, ExpandSpecs),
    FoundError = contains_errors(Globals, ExpandSpecs),
    !:Specs = ExpandSpecs ++ !.Specs,
    maybe_write_out_errors_no_module(Verbose, Globals, !Specs, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%

:- pred make_hlds(globals::in, module_name::in, list(item)::in, event_set::in,
    mq_info::in, eqv_map::in, used_modules::in, bool::in, bool::in,
    module_info::out, make_hlds_qual_info::out,
    bool::out, bool::out, bool::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

make_hlds(Globals, ModuleName, Items, EventSet, MQInfo, EqvMap, UsedModules,
        Verbose, Stats, !:HLDS, QualInfo, UndefTypes, UndefModes,
        FoundSemanticError, !Specs, !IO) :-
    maybe_write_out_errors_no_module(Verbose, Globals, !Specs, !IO),
    maybe_write_string(Verbose, "% Converting parse tree to hlds...\n", !IO),
    module_name_to_file_name(Globals, ModuleName, ".hlds_dump",
        do_create_dirs, DumpBaseFileName, !IO),
    ParseTree = unit_module(ModuleName, Items),
    parse_tree_to_hlds(Globals, DumpBaseFileName, ParseTree, MQInfo, EqvMap,
        UsedModules, QualInfo, UndefTypes, UndefModes, !:HLDS, MakeSpecs),
    !:Specs = MakeSpecs ++ !.Specs,
    module_info_set_event_set(EventSet, !HLDS),
    io.get_exit_status(Status, !IO),
    SpecsErrors = contains_errors(Globals, !.Specs),
    (
        ( Status \= 0
        ; SpecsErrors = yes
        )
    ->
        FoundSemanticError = yes,
        io.set_exit_status(1, !IO)
    ;
        FoundSemanticError = no
    ),
    maybe_write_out_errors_no_module(Verbose, Globals, !Specs, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred maybe_write_dependency_graph(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_write_dependency_graph(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, show_dependency_graph, ShowDepGraph),
    (
        ShowDepGraph = yes,
        maybe_write_string(Verbose, "% Writing dependency graph...", !IO),
        module_info_get_name(!.HLDS, ModuleName),
        module_name_to_file_name(Globals, ModuleName, ".dependency_graph",
            do_create_dirs, FileName, !IO),
        io.open_output(FileName, Res, !IO),
        (
            Res = ok(FileStream),
            io.set_output_stream(FileStream, OutputStream, !IO),
            dependency_graph.write_dependency_graph(!HLDS, !IO),
            io.set_output_stream(OutputStream, _, !IO),
            io.close_output(FileStream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            Res = error(IOError),
            ErrorMsg = "unable to write dependency graph: " ++
                io.error_message(IOError),
            report_error(ErrorMsg, !IO)
        ),
        maybe_report_stats(Stats, !IO)
    ;
        ShowDepGraph = no
    ).

%-----------------------------------------------------------------------------%

    % Outputs the file <module_name>.prof, which contains the static
    % call graph in terms of label names, if the profiling flag is enabled.
    %
:- pred maybe_output_prof_call_graph(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_output_prof_call_graph(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, profile_calls, ProfileCalls),
    globals.lookup_bool_option(Globals, profile_time, ProfileTime),
    (
        ( ProfileCalls = yes
        ; ProfileTime = yes
        )
    ->
        maybe_write_string(Verbose,
            "% Outputing profiling call graph...", !IO),
        maybe_flush_output(Verbose, !IO),
        module_info_get_name(!.HLDS, ModuleName),
        module_name_to_file_name(Globals, ModuleName, ".prof", do_create_dirs,
            ProfFileName, !IO),
        io.open_output(ProfFileName, Res, !IO),
        (
            Res = ok(FileStream),
            io.set_output_stream(FileStream, OutputStream, !IO),
            dependency_graph.write_prof_dependency_graph(!HLDS, !IO),
            io.set_output_stream(OutputStream, _, !IO),
            io.close_output(FileStream, !IO)
        ;
            Res = error(IOError),
            ErrorMsg = "unable to write profiling static call graph: " ++
                io.error_message(IOError),
            report_error(ErrorMsg, !IO)
        ),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
%
% Library grade detection.
%

% Enable the compile-time trace flag "debug-detect-libgrades" to enable
% debugging messages for library grade detection in the very verbose output.

:- pred detect_libgrades(globals::in, maybe(list(string))::in,
    list(string)::out, io::di, io::uo) is det.

detect_libgrades(Globals, MaybeConfigMerStdLibDir, GradeOpts, !IO) :-
    globals.lookup_bool_option(Globals, detect_libgrades, Detect),
    (
        Detect = yes,
        globals.lookup_bool_option(Globals, verbose, Verbose),
        trace [io(!TIO), compile_time(flag("debug-detect-libgrades"))] (
            maybe_write_string(Verbose, "% Detecting library grades ...\n", !TIO)
        ),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        % NOTE: a standard library directory specified on the command line
        % overrides one set using the MERCURY_STDLIB_DIR variable.
        ( if
            % Was the standard library directory set on the command line?
            % 
            globals.lookup_maybe_string_option(Globals,
                mercury_standard_library_directory, MaybeStdLibDir),
            MaybeStdLibDir = yes(MerStdLibDir)
        then
            do_detect_libgrades(VeryVerbose, MerStdLibDir, GradeOpts, !IO)
        else if
            % Was the standard library directory set using the
            % MERCURY_STDLIB_DIR variable?
            MaybeConfigMerStdLibDir = yes([MerStdLibDir])
        then
            do_detect_libgrades(VeryVerbose, MerStdLibDir, GradeOpts, !IO)
        else
            GradeOpts = [] 
        ),
        trace [io(!TIO), compile_time(flag("debug-detect-libgrades"))] (
            maybe_write_string(Verbose, "% done.\n", !TIO)
        )
    ;
        Detect = no,
        GradeOpts = []
    ).

:- pred do_detect_libgrades(bool::in, string::in, list(string)::out,
    io::di, io::uo) is det.

do_detect_libgrades(VeryVerbose, StdLibDir, GradeOpts, !IO) :-
    ModulesDir = StdLibDir / "modules",
    dir.foldl2(do_detect_libgrade(VeryVerbose), ModulesDir,
        [], MaybeGradeOpts, !IO),
    (
        MaybeGradeOpts = ok(GradeOpts)
    ;
        MaybeGradeOpts = error(_, _),
        GradeOpts = []
    ).

:- pred do_detect_libgrade(bool::in, string::in, string::in, io.file_type::in,
    bool::out, list(string)::in, list(string)::out, io::di, io::uo) is det.

do_detect_libgrade(VeryVerbose, DirName, FileName, FileType, Continue,
        !GradeOpts, !IO) :-
    (
        FileType = directory,
        (
            % We do not generate .init files for the non-C grades so just
            % check for directories in StdLibDir / "modules" containing
            % the name of their base grade.
            %
            ( string.prefix(FileName, "csharp")
            ; string.prefix(FileName, "erlang")
            ; string.prefix(FileName, "java")
            )
        ->
            maybe_report_detected_libgrade(VeryVerbose, FileName, !IO),
            !:GradeOpts = ["--libgrade", FileName | !.GradeOpts]
        ;
            % For C grades, we check for the presence of the .init file for
            % mer_std to test whether the grade is present or not.
            %
            InitFile = DirName / FileName / "mer_std.init",
            io.check_file_accessibility(InitFile, [read], Result, !IO),
            (
                Result = ok,
                maybe_report_detected_libgrade(VeryVerbose, FileName, !IO),
                !:GradeOpts = ["--libgrade", FileName | !.GradeOpts]
            ;
                Result = error(_)
            )
        ),
        Continue = yes
    ;
        ( FileType = regular_file
        ; FileType = symbolic_link
        ; FileType = named_pipe
        ; FileType = socket
        ; FileType = character_device
        ; FileType = block_device
        ; FileType = message_queue
        ; FileType = semaphore
        ; FileType = shared_memory
        ; FileType = unknown
        ),
        Continue = yes
    ).
            
:- pred maybe_report_detected_libgrade(bool::in, string::in,
    io::di, io::uo) is det.

maybe_report_detected_libgrade(VeryVerbose, GradeStr, !IO) :-
    trace [io(!TIO), compile_time(flag("debug-detect-libgrades"))] (
        (
            VeryVerbose = yes,
            io.format("%% Detected library grade: %s\n", [s(GradeStr)], !TIO)
        ;
            VeryVerbose = no
        )
    ).

%-----------------------------------------------------------------------------%

:- pred gc_init(io::di, io::uo) is det.

% This version is only used if there is no matching foreign_proc version.
gc_init(!IO).

:- pragma foreign_proc("C",
    gc_init(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
#ifdef MR_BOEHM_GC
    /*
    ** Explicitly force the initial heap size to be at least 4 Mb.
    **
    ** This works around a bug in the Boehm collector (for versions up
    ** to at least 6.2) where the collector would sometimes abort with
    ** the message `unexpected mark stack overflow' (e.g. in grade hlc.gc
    ** on dec-alpha-osf3.2).
    **
    ** Doing this should also improve performance slightly by avoiding
    ** frequent garbage collection during start-up.
    */
    GC_expand_hp(4 * 1024 * 1024);
#endif
").

%-----------------------------------------------------------------------------%
:- end_module top_level.mercury_compile.
%-----------------------------------------------------------------------------%
