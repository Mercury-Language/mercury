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

    % main_for_make(Globals, Args, !IO) is called from
    % make.module_target.call_mercury_compile_main.
    %
:- pred main_for_make(globals::in, list(string)::in, io::di, io::uo) is det.

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
:- import_module hlds.hlds_defns.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.
:- import_module hlds.passes_aux.
:- import_module libs.check_libgrades.
:- import_module libs.compute_grade.
:- import_module libs.file_util.
:- import_module libs.handle_options.
:- import_module libs.op_mode.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module make.
:- import_module make.build.
:- import_module make.options_file.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.shared_utilities.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.check_raw_comp_unit.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.file_names.
:- import_module parse_tree.generate_dep_d_files.
:- import_module parse_tree.grab_modules.
:- import_module parse_tree.item_util.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_data_used_modules.
:- import_module parse_tree.prog_event.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.
:- import_module parse_tree.source_file_map.
:- import_module parse_tree.split_parse_tree_src.
:- import_module parse_tree.write_deps_file.
:- import_module parse_tree.write_module_interface_files.
:- import_module recompilation.
:- import_module recompilation.check.
:- import_module recompilation.usage.
:- import_module top_level.mercury_compile_erl_back_end.
:- import_module top_level.mercury_compile_front_end.
:- import_module top_level.mercury_compile_llds_back_end.
:- import_module top_level.mercury_compile_middle_passes.
:- import_module top_level.mercury_compile_mlds_back_end.

:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module dir.
:- import_module gc.
:- import_module getopt_io.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

real_main(!IO) :-
    gc_init(!IO),

    % All messages go to stderr.
    io.stderr_stream(StdErr, !IO),
    io.set_output_stream(StdErr, _, !IO),
    io.command_line_arguments(CmdLineArgs, !IO),

    trace [compile_time(flag("cmd_line_args")), io(!TIO)] (
        dump_args("REAL_MAIN", CmdLineArgs, !TIO)
    ),

    unlimit_stack(!IO),

    % Replace all @file arguments with the contents of the file.
    expand_at_file_arguments(CmdLineArgs, Res, !IO),
    (
        Res = ok(ExpandedCmdLineArgs),
        real_main_after_expansion(ExpandedCmdLineArgs, !IO)
    ;
        Res = error(E),
        io.set_exit_status(1, !IO),

        io.write_string(io.error_message(E), !IO),
        io.nl(!IO)
    ),
    trace [compile_time(flag("file_name_translations")), io(!TIO)] (
        write_translations_record_if_any(!TIO)
    ),
    trace [compile_time(flag("mai-stats")), io(!TIO)] (
        io.open_append("/tmp/MODULE_AND_IMPORTS", Result, !TIO),
        (
            Result = ok(Stream),
            write_mai_stats(Stream, !TIO),
            io.close_output(Stream, !TIO)
        ;
            Result = error(_)
        )
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
:- pred expand_file_into_arg_list(io.input_stream::in,
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

:- pred real_main_after_expansion(list(string)::in, io::di, io::uo) is det.

real_main_after_expansion(CmdLineArgs, !IO) :-
    % XXX Processing the options up to three times is not what you call
    % elegant.
    ( if CmdLineArgs = ["--arg-file", ArgFile | ExtraArgs] then
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
            unexpected($pred,
                "extra arguments with --arg-file: " ++ string(ExtraArgs))
        ),

        % read_args_file may attempt to look up options, so we need to
        % initialize the globals.
        read_args_file(ArgFile, MaybeArgs1,
            ArgsNonUndefSpecs, ArgsUndefSpecs, !IO),
        % Since the args file is supposed to be generated by the parent
        % `mmc --make' process, there shouldn't be any references to
        % undefined make variables in it. If there are, that process has
        % screwed up. We can't fix that bug unless the bug is reported,
        % which requires printing the error messages that it yields.
        AllSpecs = ArgsNonUndefSpecs ++ ArgsUndefSpecs,
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
    else
        % Find out which options files to read.
        % Don't report errors yet, as the errors may no longer exist
        % after we have read in options files.
        %
        % XXX Doing every task in handle_given_options is wasteful.
        % in the very likely case of their being an option file.
        % We should do just enough to find the setting of the two options
        % whose values we look up right here.
        handle_given_options(CmdLineArgs, OptionArgs, NonOptionArgs,
            _Errors0, ArgsGlobals, !IO),
        globals.lookup_accumulating_option(ArgsGlobals,
            options_search_directories, OptionSearchDirectories),
        globals.lookup_accumulating_option(ArgsGlobals, options_files,
            OptionsFiles),
        read_options_files_named_in_options_file_option(
            OptionSearchDirectories, OptionsFiles,
            Variables0, OptFileNonUndefSpecs, OptFileUndefSpecs, !IO),
        globals.lookup_bool_option(ArgsGlobals,
            warn_undefined_options_variables, WarnUndef),
        (
            WarnUndef = no,
            OptFileSpecs = OptFileNonUndefSpecs
        ;
            WarnUndef = yes,
            OptFileSpecs = OptFileNonUndefSpecs ++ OptFileUndefSpecs
        ),
        OptFileErrors = contains_errors(ArgsGlobals, OptFileSpecs),
        (
            OptFileErrors = no,
            maybe_dump_options_file(ArgsGlobals, Variables0, !IO),
            lookup_mmc_options(Variables0, MCFlags0, MCFlagsSpecs0, !IO),
            MCFlagsErrors0 = contains_errors(ArgsGlobals, MCFlagsSpecs0),
            (
                MCFlagsErrors0 = no,
                % Process the options again to find out which configuration
                % file to read.

                trace [compile_time(flag("cmd_line_args")), io(!TIO)] (
                    dump_args("MCFlags0", MCFlags0, !TIO),
                    dump_args("CmdLineArgs", CmdLineArgs, !TIO)
                ),

                handle_given_options(MCFlags0 ++ CmdLineArgs, _, _,
                    FlagsSpecs, FlagsArgsGlobals, !IO),
                (
                    FlagsSpecs = [_ | _],
                    usage_errors(FlagsArgsGlobals, FlagsSpecs, !IO),
                    DetectedGradeFlags = [],
                    Variables = options_variables_init,
                    AllSpecs = OptFileSpecs ++ MCFlagsSpecs0 ++ FlagsSpecs,
                    MaybeMCFlags = no
                ;
                    FlagsSpecs = [],
                    globals.lookup_maybe_string_option(FlagsArgsGlobals,
                        config_file, MaybeConfigFile),
                    (
                        MaybeConfigFile = yes(ConfigFile),
                        read_named_options_file(ConfigFile,
                            Variables0, Variables,
                            ConfigNonUndefSpecs, ConfigUndefSpecs, !IO),
                        (
                            WarnUndef = no,
                            ConfigSpecs = ConfigNonUndefSpecs
                        ;
                            WarnUndef = yes,
                            ConfigSpecs = ConfigNonUndefSpecs ++
                                ConfigUndefSpecs
                        ),
                        ConfigErrors = contains_errors(FlagsArgsGlobals,
                            ConfigSpecs),
                        (
                            ConfigErrors = no,
                            lookup_mmc_options(Variables, MCFlags1,
                                MCFlagsSpecs1, !IO),
                            AllSpecs0 = OptFileSpecs ++ MCFlagsSpecs0 ++
                                ConfigSpecs ++ MCFlagsSpecs1,
                            AllErrors0 = contains_errors(FlagsArgsGlobals,
                                AllSpecs0),
                            (
                                AllErrors0 = no,
                                MaybeMCFlags = yes(MCFlags1)
                            ;
                                AllErrors0 = yes,
                                MaybeMCFlags = no
                            ),
                            lookup_mercury_stdlib_dir(Variables,
                                MaybeMerStdLibDir, StdLibDirSpecs, !IO),
                            detect_libgrades(FlagsArgsGlobals,
                                MaybeMerStdLibDir, DetectedGradeFlags, !IO),
                            AllSpecs = AllSpecs0 ++ StdLibDirSpecs
                        ;
                            ConfigErrors = yes,
                            DetectedGradeFlags = [],
                            AllSpecs = OptFileSpecs ++
                                MCFlagsSpecs0 ++ ConfigSpecs,
                            MaybeMCFlags = no
                        )
                    ;
                        MaybeConfigFile = no,
                        DetectedGradeFlags = [],
                        Variables = options_variables_init,
                        lookup_mmc_options(Variables, MCFlags1,
                            MCFlagsSpecs1, !IO),
                        AllSpecs = OptFileSpecs ++ MCFlagsSpecs0 ++
                            MCFlagsSpecs1,
                        AllErrors = contains_errors(FlagsArgsGlobals,
                            AllSpecs),
                        (
                            AllErrors = no,
                            MaybeMCFlags = yes(MCFlags1)
                        ;
                            AllErrors = yes,
                            MaybeMCFlags = no
                        )
                    )
                )
            ;
                MCFlagsErrors0 = yes,
                Variables = options_variables_init,
                DetectedGradeFlags = [],
                AllSpecs = OptFileSpecs ++ MCFlagsSpecs0,
                MaybeMCFlags = no
            )
        ;
            OptFileErrors = yes,
            Variables = options_variables_init,
            DetectedGradeFlags = [],
            AllSpecs = OptFileSpecs,
            MaybeMCFlags = no
        )
    ),
    (
        MaybeMCFlags = yes(MCFlags),

        % NOTE: the order of the flags here is important. It must be:
        %
        %   (1) flags for detected library grades
        %   (2) flags from Mercury.config and any Mercury.options files
        %   (3) flags from any command line options
        %
        % Flags given later in this list will override those given earlier.
        %
        % XXX the relationship between --no-libgrade or --libgrade options set
        % via the DEFAULT_MCFLAGS variable and detected library grades is
        % currently not defined. It does not  matter at the moment, since
        % Mercury.config does not contain either of those two flags.
        AllFlags = DetectedGradeFlags ++ MCFlags ++ OptionArgs,
        trace [compile_time(flag("cmd_line_args")), io(!TIO)] (
            dump_args("AllFlags", AllFlags, !TIO)
        ),
        handle_given_options(AllFlags, _, _, Specs, ActualGlobals, !IO),

        % Now that we have constructed a globals, print out any errors and/or
        % warnings generated by the predicates in options_file.m.
        write_error_specs_ignore(ActualGlobals, AllSpecs, !IO),

        % When computing the option arguments to pass to `--make', only include
        % the command-line arguments, not the contents of DEFAULT_MCFLAGS.
        (
            Specs = [_ | _],
            usage_errors(ActualGlobals, Specs, !IO)
        ;
            Specs = [],
            main_after_setup(ActualGlobals, DetectedGradeFlags, Variables,
                OptionArgs, NonOptionArgs, !IO)
        )
    ;
        MaybeMCFlags = no,
        handle_given_options([], _, _, _Specs, DummyGlobals, !IO),
        % Even if we cannot construct a meaningful globals, print out
        % any errors and/or warnings generated by the predicates
        % in options_file.m.
        write_error_specs_ignore(DummyGlobals, AllSpecs, !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred maybe_dump_options_file(globals::in, options_variables::in,
    io::di, io::uo) is det.

maybe_dump_options_file(ArgsGlobals, Variables, !IO) :-
    lookup_bool_option(ArgsGlobals, dump_options_file, DumpOptionsFile),
    (
        DumpOptionsFile = no
    ;
        DumpOptionsFile = yes,
        dump_options_file(Variables, !IO)
    ).

%---------------------%

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
            maybe_write_string(Verbose, "% Detecting library grades ...\n",
                !TIO)
        ),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        % NOTE: a standard library directory specified on the command line
        % overrides one set using the MERCURY_STDLIB_DIR variable.
        ( if
            % Was the standard library directory set on the command line?
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
    dir.foldl2(do_detect_libgrade_using_init_file(VeryVerbose), ModulesDir,
        [], MaybeGradeOpts0, !IO),
    (
        MaybeGradeOpts0 = ok(GradeOpts0),
        LibsDir = StdLibDir / "lib",
        dir.foldl2(do_detect_libgrade_using_lib_file(VeryVerbose), LibsDir,
            GradeOpts0, MaybeGradeOpts, !IO),
        (
            MaybeGradeOpts = ok(GradeOpts)
        ;
            MaybeGradeOpts = error(_, _),
            GradeOpts = []
        )
    ;
        MaybeGradeOpts0 = error(_, _),
        GradeOpts = []
    ).

    % Test for the presence of an installed grade by looking for mer_std.init.
    % This works for all grades except the C# and Java grades.
    %
:- pred do_detect_libgrade_using_init_file(bool::in, string::in, string::in,
    io.file_type::in, bool::out, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

do_detect_libgrade_using_init_file(VeryVerbose, DirName, GradeFileName,
        GradeFileType, Continue, !GradeOpts, !IO) :-
    (
        GradeFileType = directory,
        InitFile = DirName / GradeFileName / "mer_std.init",
        io.check_file_accessibility(InitFile, [read], Result, !IO),
        (
            Result = ok,
            maybe_report_detected_libgrade(VeryVerbose, GradeFileName, !IO),
            !:GradeOpts = ["--libgrade", GradeFileName | !.GradeOpts]
        ;
            Result = error(_)
        ),
        Continue = yes
    ;
        ( GradeFileType = regular_file
        ; GradeFileType = symbolic_link
        ; GradeFileType = named_pipe
        ; GradeFileType = socket
        ; GradeFileType = character_device
        ; GradeFileType = block_device
        ; GradeFileType = message_queue
        ; GradeFileType = semaphore
        ; GradeFileType = shared_memory
        ; GradeFileType = unknown
        ),
        Continue = yes
    ).

    % Test for the presence of installed Java and C# grades by looking for
    % the standard library JAR or assembly respectively.
    %
:- pred do_detect_libgrade_using_lib_file(bool::in, string::in, string::in,
    io.file_type::in, bool::out, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

do_detect_libgrade_using_lib_file(VeryVerbose, DirName, GradeFileName,
        GradeFileType, Continue, !GradeOpts, !IO) :-
    (
        GradeFileType = directory,
        ( if
            csharp_or_java_libgrade_target(GradeFileName, LibFile)
        then
            TargetFile = DirName / GradeFileName / LibFile,
            io.check_file_accessibility(TargetFile, [read], Result, !IO),
            (
                Result = ok,
                maybe_report_detected_libgrade(VeryVerbose, GradeFileName,
                    !IO),
                !:GradeOpts = ["--libgrade", GradeFileName | !.GradeOpts]
            ;
                Result = error(_)
            )
        else
            true
        ),
        Continue = yes
    ;
        ( GradeFileType = regular_file
        ; GradeFileType = symbolic_link
        ; GradeFileType = named_pipe
        ; GradeFileType = socket
        ; GradeFileType = character_device
        ; GradeFileType = block_device
        ; GradeFileType = message_queue
        ; GradeFileType = semaphore
        ; GradeFileType = shared_memory
        ; GradeFileType = unknown
        ),
        Continue = yes
    ).

:- pred csharp_or_java_libgrade_target(string::in, string::out) is semidet.

csharp_or_java_libgrade_target(GradeFileName, LibFile) :-
    ( if string.prefix(GradeFileName, "csharp") then
        LibFile = "mer_std.dll"
    else if string.prefix(GradeFileName, "java") then
         LibFile = "mer_std.jar"
    else
        false
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

%---------------------------------------------------------------------------%

main_for_make(Globals, Args, !IO) :-
    main_after_setup(Globals, [], options_variables_init, [], Args, !IO).

%---------------------------------------------------------------------------%

:- pred main_after_setup(globals::in, list(string)::in, options_variables::in,
    list(string)::in, list(string)::in, io::di, io::uo) is det.

main_after_setup(Globals, DetectedGradeFlags, OptionVariables, OptionArgs,
        Args, !IO) :-
    globals.lookup_bool_option(Globals, version, Version),
    globals.lookup_bool_option(Globals, help, Help),

    % NOTE: --help takes precedence over any other modes of operation as we do
    % not wish to place unnecessary obstacles before users who want help.
    % --version takes precedence over the remaining modes of operation since
    % this behaviour is common in other compilers and command line tools and
    % will be in line with the expectations of at least some users.
    %
    ( if Help = yes then
        io.stdout_stream(Stdout, !IO),
        io.set_output_stream(Stdout, OldOutputStream, !IO),
        long_usage(!IO),
        io.set_output_stream(OldOutputStream, _, !IO)
    else if Version = yes then
        io.stdout_stream(Stdout, !IO),
        io.set_output_stream(Stdout, OldOutputStream, !IO),
        display_compiler_version(!IO),
        io.set_output_stream(OldOutputStream, _, !IO)
    else
        globals.get_op_mode(Globals, OpMode),
        HaveReadModuleMaps0 = init_have_read_module_maps,
        do_op_mode(Globals, OpMode, DetectedGradeFlags,
            OptionVariables, OptionArgs, Args,
            HaveReadModuleMaps0, _HaveReadModuleMaps, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred do_op_mode(globals::in, op_mode::in,
    list(string)::in, options_variables::in,
    list(string)::in, list(string)::in,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

do_op_mode(Globals, OpMode, DetectedGradeFlags, OptionVariables,
        OptionArgs, Args, !HaveReadModuleMaps, !IO) :-
    (
        OpMode = opm_top_make,
        make_process_compiler_args(Globals, DetectedGradeFlags,
            OptionVariables, OptionArgs, Args, !IO)
    ;
        OpMode = opm_top_generate_source_file_mapping,
        source_file_map.write_source_file_map(Globals, Args, !IO)
    ;
        OpMode = opm_top_generate_standalone_interface(StandaloneIntBasename),
        do_op_mode_standalone_interface(Globals, StandaloneIntBasename, !IO)
    ;
        OpMode = opm_top_query(OpModeQuery),
        do_op_mode_query(Globals, OpModeQuery, !IO)
    ;
        OpMode = opm_top_args(OpModeArgs),
        globals.lookup_bool_option(Globals, filenames_from_stdin,
            FileNamesFromStdin),
        ( if
            Args = [],
            FileNamesFromStdin = no
        then
            usage(!IO)
        else
            do_op_mode_args(Globals, OpModeArgs, FileNamesFromStdin,
                DetectedGradeFlags, OptionVariables, OptionArgs, Args,
                !HaveReadModuleMaps, !IO)
        )
    ).

:- pred do_op_mode_standalone_interface(globals::in, string::in,
    io::di, io::uo) is det.

do_op_mode_standalone_interface(Globals, StandaloneIntBasename, !IO) :-
    globals.get_target(Globals, Target),
    (
        ( Target = target_csharp
        ; Target = target_java
        ),
        NotRequiredMsg = [words("Error:"),
            quote("--generate-standalone-interface"),
            words("is not required for target language"),
            words(compilation_target_string(Target)), suffix("."), nl],
        write_error_pieces_plain(Globals, NotRequiredMsg, !IO),
        io.set_exit_status(1, !IO)
    ;
        Target = target_erlang,
        NYIMsg = [words("Sorry,"),
            quote("--generate-standalone-interface"),
            words("is not yet supported with target language"),
            words(compilation_target_string(Target)), suffix("."), nl],
        write_error_pieces_plain(Globals, NYIMsg, !IO),
        io.set_exit_status(1, !IO)
    ;
        Target = target_c,
        make_standalone_interface(Globals, StandaloneIntBasename, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred do_op_mode_query(globals::in, op_mode_query::in,
    io::di, io::uo) is det.

do_op_mode_query(Globals, OpModeQuery, !IO) :-
    (
        OpModeQuery = opmq_output_cc,
        globals.lookup_string_option(Globals, cc, CC),
        io.stdout_stream(StdOut, !IO),
        io.write_string(StdOut, CC ++ "\n", !IO)
    ;
        OpModeQuery = opmq_output_c_compiler_type,
        globals.lookup_string_option(Globals, c_compiler_type, CC_Type),
        io.stdout_stream(StdOut, !IO),
        io.write_string(StdOut, CC_Type ++ "\n", !IO)
    ;
        OpModeQuery = opmq_output_cflags,
        io.stdout_stream(StdOut, !IO),
        output_c_compiler_flags(Globals, StdOut, !IO),
        io.nl(StdOut, !IO)
    ;
        OpModeQuery = opmq_output_c_include_directory_flags,
        io.stdout_stream(StdOut, !IO),
        output_c_include_directory_flags(Globals, StdOut, !IO)
    ;
        OpModeQuery = opmq_output_csharp_compiler,
        globals.lookup_string_option(Globals, csharp_compiler, CSC),
        io.stdout_stream(StdOut, !IO),
        io.write_string(StdOut, CSC ++ "\n", !IO)
    ;
        OpModeQuery = opmq_output_csharp_compiler_type,
        globals.lookup_string_option(Globals, csharp_compiler_type, CSC_Type),
        io.stdout_stream(StdOut, !IO),
        io.write_string(StdOut, CSC_Type ++ "\n", !IO)
    ;
        OpModeQuery = opmq_output_grade_defines,
        io.stdout_stream(StdOut, !IO),
        output_c_grade_defines(Globals, StdOut, !IO)
    ;
        OpModeQuery = opmq_output_link_command,
        globals.lookup_string_option(Globals, link_executable_command,
            LinkCommand),
        io.stdout_stream(Stdout, !IO),
        io.write_string(Stdout, LinkCommand, !IO),
        io.nl(Stdout, !IO)
    ;
        OpModeQuery = opmq_output_shared_lib_link_command,
        globals.lookup_string_option(Globals, link_shared_lib_command,
            LinkCommand),
        io.stdout_stream(Stdout, !IO),
        io.write_string(Stdout, LinkCommand, !IO),
        io.nl(Stdout, !IO)
    ;
        OpModeQuery = opmq_output_library_link_flags,
        io.stdout_stream(StdOut, !IO),
        output_library_link_flags(Globals, StdOut, !IO)
    ;
        OpModeQuery = opmq_output_class_dir,
        io.stdout_stream(StdOut, !IO),
        get_class_dir_name(Globals, ClassName),
        io.write_string(StdOut, ClassName ++ "\n", !IO)
    ;
        OpModeQuery = opmq_output_grade_string,
        % When Mmake asks for the grade, it really wants the directory
        % component to use. This is consistent with scripts/canonical_grade.
        grade_directory_component(Globals, Grade),
        io.stdout_stream(Stdout, !IO),
        io.write_string(Stdout, Grade, !IO),
        io.nl(Stdout, !IO)
    ;
        OpModeQuery = opmq_output_libgrades,
        globals.lookup_accumulating_option(Globals, libgrades, LibGrades),
        (
            LibGrades = []
        ;
            LibGrades = [_ | _],
            io.stdout_stream(Stdout, !IO),
            io.write_list(Stdout, LibGrades, "\n", io.write_string, !IO),
            io.nl(Stdout, !IO)
        )
    ;
        OpModeQuery = opmq_output_target_arch,
        io.stdout_stream(StdOut, !IO),
        globals.lookup_string_option(Globals, target_arch, TargetArch),
        io.write_string(StdOut, TargetArch ++ "\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Do the modes of operation that process the argument list.
%

:- pred do_op_mode_args(globals::in, op_mode_args::in, bool::in,
    list(string)::in, options_variables::in,
    list(string)::in, list(string)::in,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

do_op_mode_args(Globals, OpModeArgs, FileNamesFromStdin, DetectedGradeFlags,
        OptionVariables, OptionArgs, Args, !HaveReadModuleMaps, !IO) :-
    (
        FileNamesFromStdin = yes,
        process_compiler_stdin_args(Globals, OpModeArgs,
            DetectedGradeFlags, OptionVariables, OptionArgs,
            cord.empty, ModulesToLinkCord, cord.empty, ExtraObjFilesCord,
            !HaveReadModuleMaps, !IO)
    ;
        FileNamesFromStdin = no,
        process_compiler_cmd_line_args(Globals, OpModeArgs,
            DetectedGradeFlags, OptionVariables, OptionArgs, Args,
            cord.empty, ModulesToLinkCord, cord.empty, ExtraObjFilesCord,
            !HaveReadModuleMaps, !IO)
    ),
    ModulesToLink = cord.list(ModulesToLinkCord),
    ExtraObjFiles = cord.list(ExtraObjFilesCord),

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
                % For Java, at the "link" step we just generate a shell
                % script; the actual linking will be done at runtime by
                % the Java interpreter.
                create_java_shell_script(Globals, MainModuleName,
                    Succeeded, !IO)
            ;
                ( Target = target_c
                ; Target = target_csharp
                ; Target = target_erlang
                ),
                compile_with_module_options(Globals, MainModuleName,
                    DetectedGradeFlags, OptionVariables, OptionArgs,
                    link_module_list(ModulesToLink, ExtraObjFiles),
                    Succeeded, !IO)
            ),
            maybe_set_exit_status(Succeeded, !IO)
        else
            true
        )
    else
        true
    ),
    maybe_print_delayed_error_messages(Globals, !IO),
    globals.lookup_bool_option(Globals, statistics, Statistics),
    (
        Statistics = yes,
        io.report_stats("full_memory_stats", !IO)
    ;
        Statistics = no
    ).

:- pred maybe_print_delayed_error_messages(globals::in, io::di, io::uo) is det.

maybe_print_delayed_error_messages(Globals, !IO) :-
    % Pick up the values of these flags, and then reset them
    % for the next module.
    globals.io_get_some_errors_were_context_limited(Limited, !IO),
    globals.io_set_some_errors_were_context_limited(
        no_errors_were_context_limited, !IO),
    globals.io_get_extra_error_info(ExtraErrorInfo, !IO),
    globals.io_set_extra_error_info(no_extra_error_info, !IO),

    % If we suppressed the printing of some errors, then tell the user
    % about this fact, because the absence of any errors being printed
    % during a failing compilation would otherwise be likely to be baffling.
    (
        Limited = no_errors_were_context_limited
    ;
        Limited = some_errors_were_context_limited,
        io.write_string("Some error messages were suppressed " ++
            "by `--limit-error-contexts' options.\n", !IO),
        io.write_string("You can see the suppressed messages " ++
            "if you recompile without these options.\n", !IO)
    ),

    % If we found some errors, but the user didn't enable the `-E'
    % (`--verbose-errors') option, give them a hint about it.
    (
        ExtraErrorInfo = no_extra_error_info
    ;
        ExtraErrorInfo = some_extra_error_info,
        globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
        (
            VerboseErrors = no,
            io.write_string("For more information, recompile with `-E'.\n",
                !IO)
        ;
            VerboseErrors = yes
        )
    ).

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
            ( pred(BuildGlobals::in, _::in, Succeeded0::out, X::in, X::out,
                    IO0::di, IO::uo) is det :-
                Compile(BuildGlobals, Succeeded0, IO0, IO)
            ),
        build_with_module_options_args(Globals, ModuleName, DetectedGradeFlags,
            OptionVariables, OptionArgs, [], Builder, Succeeded, unit, _, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred process_compiler_stdin_args(globals::in, op_mode_args::in,
    list(string)::in, options_variables::in, list(string)::in,
    cord(string)::in, cord(string)::out,
    cord(string)::in, cord(string)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

process_compiler_stdin_args(Globals, OpModeArgs, DetectedGradeFlags,
        OptionVariables, OptionArgs, !Modules, !ExtraObjFiles,
        !HaveReadModuleMaps, !IO) :-
    ( if is_empty(!.Modules) then
        true
    else
        garbage_collect(!IO)
    ),
    io.read_line_as_string(FileResult, !IO),
    (
        FileResult = ok(Line),
        Arg = string.rstrip(Line),
        process_compiler_arg(Globals, OpModeArgs, DetectedGradeFlags,
            OptionVariables, OptionArgs, Arg, ArgModules, ArgExtraObjFiles,
            !HaveReadModuleMaps, !IO),
        !:Modules = !.Modules ++ cord.from_list(ArgModules),
        !:ExtraObjFiles = !.ExtraObjFiles ++ cord.from_list(ArgExtraObjFiles),
        process_compiler_stdin_args(Globals, OpModeArgs, DetectedGradeFlags,
            OptionVariables, OptionArgs, !Modules, !ExtraObjFiles,
            !HaveReadModuleMaps, !IO)
    ;
        FileResult = eof
    ;
        FileResult = error(Error),
        io.error_message(Error, Msg),
        io.write_string("Error reading module name: ", !IO),
        io.write_string(Msg, !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred process_compiler_cmd_line_args(globals::in, op_mode_args::in,
    list(string)::in, options_variables::in,
    list(string)::in, list(string)::in,
    cord(string)::in, cord(string)::out, cord(string)::in, cord(string)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

process_compiler_cmd_line_args(_, _, _, _, _, [],
        !Modules, !ExtraObjFiles, !HaveReadModuleMaps, !IO).
process_compiler_cmd_line_args(Globals, OpModeArgs, DetectedGradeFlags,
        OptionVariables, OptionArgs, [Arg | Args],
        !Modules, !ExtraObjFiles, !HaveReadModuleMaps, !IO) :-
    process_compiler_arg(Globals, OpModeArgs, DetectedGradeFlags,
        OptionVariables, OptionArgs, Arg, ArgModules, ArgExtraObjFiles,
        !HaveReadModuleMaps, !IO),
    (
        Args = [_ | _],
        garbage_collect(!IO)
    ;
        Args = []
    ),
    !:Modules = !.Modules ++ cord.from_list(ArgModules),
    !:ExtraObjFiles = !.ExtraObjFiles ++ cord.from_list(ArgExtraObjFiles),
    process_compiler_cmd_line_args(Globals, OpModeArgs, DetectedGradeFlags,
        OptionVariables, OptionArgs, Args, !Modules, !ExtraObjFiles,
        !HaveReadModuleMaps, !IO).

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
:- pred process_compiler_arg(globals::in, op_mode_args::in,
    list(string)::in, options_variables::in,
    list(string)::in, string::in, list(string)::out, list(string)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

process_compiler_arg(Globals, OpModeArgs, DetectedGradeFlags, OptionVariables,
        OptionArgs, Arg, ModulesToLink, ExtraObjFiles,
        !HaveReadModuleMaps, !IO) :-
    FileOrModule = string_to_file_or_module(Arg),
    globals.lookup_bool_option(Globals, invoked_by_mmc_make, InvokedByMake),
    (
        InvokedByMake = no,
        ExtraOptions = [],
        DummyInput = unit,
        build_with_module_options_args(Globals,
            file_or_module_to_module_name(FileOrModule),
            DetectedGradeFlags, OptionVariables, OptionArgs, ExtraOptions,
            process_compiler_arg_build(OpModeArgs, FileOrModule, OptionArgs,
                !.HaveReadModuleMaps),
            _Succeeded, DummyInput, MaybeTuple, !IO),
        % XXX Why do we ignore _Succeeded? We know it is set to `yes'
        % by process_compiler_arg_build, but this could be overridden by
        % build_with_module_options_args if it finds *other* kinds of problems.
        % However, enabling the following line did not yield any aborts.
        % expect(unify(Succeeded, yes), $pred, "Succeeded != yes"),
        (
            MaybeTuple = yes(Tuple),
            Tuple = {ModulesToLink, ExtraObjFiles, !:HaveReadModuleMaps}
        ;
            MaybeTuple = no,
            ModulesToLink = [],
            ExtraObjFiles = []
        )
    ;
        InvokedByMake = yes,
        % `mmc --make' has already set up the options.
        do_process_compiler_arg(Globals, OpModeArgs, OptionArgs, FileOrModule,
            ModulesToLink, ExtraObjFiles, !HaveReadModuleMaps, !IO)
    ).

:- pred process_compiler_arg_build(op_mode_args::in, file_or_module::in,
    list(string)::in, have_read_module_maps::in,
    globals::in, list(string)::in, bool::out,
    unit::in, {list(string), list(string), have_read_module_maps}::out,
    io::di, io::uo) is det.

process_compiler_arg_build(OpModeArgs, FileOrModule, OptionArgs,
        HaveReadModuleMaps0, Globals, _, Succeeded,
        _DummyInput, {Modules, ExtraObjFiles, HaveReadModuleMaps},
        !IO) :-
    maybe_check_libraries_are_installed(Globals, LibgradeCheckSucceeded, !IO),
    (
        LibgradeCheckSucceeded = yes,
        do_process_compiler_arg(Globals, OpModeArgs, OptionArgs, FileOrModule,
            Modules, ExtraObjFiles,
            HaveReadModuleMaps0, HaveReadModuleMaps, !IO),
        Succeeded = yes
    ;
        LibgradeCheckSucceeded = no,
        Modules = [],
        ExtraObjFiles = [],
        Succeeded = no,
        HaveReadModuleMaps = HaveReadModuleMaps0
    ).

:- func version_numbers_return_timestamp(bool) = maybe_return_timestamp.

version_numbers_return_timestamp(no) = dont_return_timestamp.
version_numbers_return_timestamp(yes) = do_return_timestamp.

:- pred do_process_compiler_arg(globals::in, op_mode_args::in,
    list(string)::in, file_or_module::in, list(string)::out, list(string)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

do_process_compiler_arg(Globals0, OpModeArgs, OptionArgs, FileOrModule,
        ModulesToLink, ExtraObjFiles, !HaveReadModuleMaps, !IO) :-
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
        OpModeArgs = opma_generate_dependencies,
        (
            FileOrModule = fm_file(FileName),
            generate_dep_file_for_file(Globals0, FileName, !IO)
        ;
            FileOrModule = fm_module(ModuleName),
            generate_dep_file_for_module(Globals0, ModuleName, !IO)
        ),
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        OpModeArgs = opma_generate_dependency_file,
        (
            FileOrModule = fm_file(FileName),
            generate_d_file_for_file(Globals0, FileName, !IO)
        ;
            FileOrModule = fm_module(ModuleName),
            generate_d_file_for_module(Globals0, ModuleName, !IO)
        ),
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        OpModeArgs = opma_convert_to_mercury,
        read_module_or_file(Globals0, Globals, FileOrModule, ModuleName, _,
            dont_return_timestamp, _, ParseTreeSrc, Specs, Errors,
            !HaveReadModuleMaps, !IO),
        write_error_specs_ignore(Globals, Specs, !IO),
        ( if halt_at_module_error(Globals, Errors) then
            true
        else
            module_name_to_file_name(Globals, $pred, do_create_dirs,
                ext_other(other_ext(".ugly")),
                ModuleName, OutputFileName, !IO),
            output_parse_tree_src(Globals, OutputFileName, ParseTreeSrc, !IO)
        ),
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        OpModeArgs = opma_make_interface(InterfaceFile),
        do_process_compiler_arg_make_interface(Globals0, InterfaceFile,
            FileOrModule, !HaveReadModuleMaps, !IO),
        ModulesToLink = [],
        ExtraObjFiles = []
    ;
        OpModeArgs = opma_augment(OpModeAugment),
        find_modules_to_recompile(Globals0, Globals, FileOrModule,
            ModulesToRecompile, !HaveReadModuleMaps, !IO),
        ( if ModulesToRecompile = some_modules([]) then
            % XXX Currently smart recompilation is disabled if mmc is linking
            % the executable, because it doesn't know how to check whether
            % all the necessary intermediate files are present and up-to-date.
            ModulesToLink = [],
            ExtraObjFiles = []
        else
            read_augment_and_process_module(Globals, OpModeAugment, OptionArgs,
                FileOrModule, ModulesToRecompile,
                ModulesToLink, ExtraObjFiles, !HaveReadModuleMaps, !IO)
        )
    ).

:- pred do_process_compiler_arg_make_interface(globals::in,
    op_mode_interface_file::in, file_or_module::in,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

do_process_compiler_arg_make_interface(Globals0, InterfaceFile, FileOrModule,
        !HaveReadModuleMaps, !IO) :-
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
    read_module_or_file(Globals0, Globals, FileOrModule,
        ModuleName, FileName, ReturnTimestamp, MaybeTimestamp,
        ParseTreeSrc, ReadSpecs, ReadErrors, !HaveReadModuleMaps, !IO),
    ( if halt_at_module_error(Globals, ReadErrors) then
        write_error_specs_ignore(Globals, ReadSpecs, !IO)
    else
        split_into_compilation_units_perform_checks(ParseTreeSrc,
            RawCompUnits, ReadSpecs, ReadSplitSpecs),
        filter_interface_generation_specs(Globals, ReadSplitSpecs, Specs, !IO),
        write_error_specs_ignore(Globals, Specs, !IO),
        maybe_print_delayed_error_messages(Globals, !IO),
        (
            InterfaceFile = omif_int0,
            list.foldl2(
                write_private_interface_file_int0(Globals0,
                    FileName, ModuleName, MaybeTimestamp),
                RawCompUnits, !HaveReadModuleMaps, !IO)
        ;
            InterfaceFile = omif_int1_int2,
            list.foldl2(
                write_interface_file_int1_int2(Globals0,
                    FileName, ModuleName, MaybeTimestamp),
                RawCompUnits, !HaveReadModuleMaps, !IO)
        ;
            InterfaceFile = omif_int3,
            list.foldl(
                write_short_interface_file_int3(Globals0, FileName),
                RawCompUnits, !IO)
        )
    ).

:- pred find_modules_to_recompile(globals::in, globals::out,
    file_or_module::in, modules_to_recompile::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

find_modules_to_recompile(Globals0, Globals, FileOrModule, ModulesToRecompile,
        !HaveReadModuleMaps, !IO) :-
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
        find_smart_recompilation_target_files(Globals, FindTargetFiles),
        find_timestamp_files(Globals, FindTimestampFiles),
        recompilation.check.should_recompile(Globals, ModuleName,
            FindTargetFiles, FindTimestampFiles, ModulesToRecompile,
            !HaveReadModuleMaps, !IO)
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
    ( CompilationTarget = target_c, TargetOtherExt = other_ext(".c")
    ; CompilationTarget = target_csharp, TargetOtherExt = other_ext(".cs")
    ; CompilationTarget = target_java, TargetOtherExt = other_ext(".java")
    ; CompilationTarget = target_erlang, TargetOtherExt = other_ext(".erl")
    ),
    FindTargetFiles = usual_find_target_files(Globals, TargetOtherExt).

:- pred usual_find_target_files(globals::in, other_ext::in,
    module_name::in, list(file_name)::out, io::di, io::uo) is det.

usual_find_target_files(Globals, TargetOtherExt,
        ModuleName, TargetFiles, !IO) :-
    % XXX Should we check the generated header files?
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(TargetOtherExt), ModuleName, FileName, !IO),
    TargetFiles = [FileName].

:- pred find_timestamp_files(globals::in,
    find_timestamp_file_names::out(find_timestamp_file_names)) is det.

find_timestamp_files(Globals, FindTimestampFiles) :-
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        TimestampOtherExt = other_ext(".c_date")
    ;
        CompilationTarget = target_csharp,
        TimestampOtherExt = other_ext(".cs_date")
    ;
        CompilationTarget = target_java,
        TimestampOtherExt = other_ext(".java_date")
    ;
        CompilationTarget = target_erlang,
        TimestampOtherExt = other_ext(".erl_date")
    ),
    FindTimestampFiles = find_timestamp_files_2(Globals, TimestampOtherExt).

:- pred find_timestamp_files_2(globals::in, other_ext::in, module_name::in,
    list(file_name)::out, io::di, io::uo) is det.

find_timestamp_files_2(Globals, TimestampOtherExt,
        ModuleName, TimestampFiles, !IO) :-
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(TimestampOtherExt), ModuleName, FileName, !IO),
    TimestampFiles = [FileName].

%---------------------------------------------------------------------------%

:- pred read_augment_and_process_module(globals::in,
    op_mode_augment::in, list(string)::in, file_or_module::in,
    modules_to_recompile::in, list(string)::out, list(string)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

read_augment_and_process_module(Globals0, OpModeAugment, OptionArgs,
        FileOrModule, MaybeModulesToRecompile, ModulesToLink, ExtraObjFiles,
        !HaveReadModuleMaps, !IO) :-
    (
        ( OpModeAugment = opmau_make_opt_int
        ; OpModeAugment = opmau_make_trans_opt_int
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
        maybe_report_cmd_line(ReportCmdLineArgsDotErr, OptionArgs, [], !IO)
    ),

    read_module_or_file(Globals0, Globals, FileOrModule, ModuleName, FileName,
        do_return_timestamp, MaybeTimestamp, ParseTreeSrc, Specs0, Errors,
        !HaveReadModuleMaps, !IO),

    ( if halt_at_module_error(Globals, Errors) then
        write_error_specs_ignore(Globals, Specs0, !IO),
        ModulesToLink = [],
        ExtraObjFiles = []
    else
        split_into_compilation_units_perform_checks(ParseTreeSrc,
            RawCompUnits0, Specs0, Specs1),
        (
            MaybeModulesToRecompile = some_modules(ModulesToRecompile),
            ToRecompile =
                ( pred(RawCompUnit::in) is semidet :-
                    RawCompUnit =
                        raw_compilation_unit(RawCompUnitModuleName, _, _),
                    list.member(RawCompUnitModuleName, ModulesToRecompile)
                ),
            list.filter(ToRecompile, RawCompUnits0, RawCompUnitsToCompile)
        ;
            MaybeModulesToRecompile = all_modules,
            RawCompUnitsToCompile = RawCompUnits0
        ),
        RawCompUnitNames = set.list_to_set(
            list.map(raw_compilation_unit_project_name, RawCompUnits0)),
        set.delete(ModuleName, RawCompUnitNames, NestedCompUnitNames),

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
            globals.set_option(trace_stack_layout, bool(no),
                Globals, GlobalsNoTrace0),
            globals.set_trace_level_none(
                GlobalsNoTrace0, GlobalsNoTrace),
            GlobalsToUse = GlobalsNoTrace
        else
            GlobalsToUse = Globals
        ),
        augment_and_process_all_submodules(GlobalsToUse, OpModeAugment,
            FileName, ModuleName, MaybeTimestamp, NestedCompUnitNames,
            FindTimestampFiles, RawCompUnitsToCompile,
            Specs1, ModulesToLink, ExtraObjFiles, !HaveReadModuleMaps, !IO)
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

:- pred read_module_or_file(globals::in, globals::out, file_or_module::in,
    module_name::out, file_name::out,
    maybe_return_timestamp::in, maybe(timestamp)::out,
    parse_tree_src::out, list(error_spec)::out, read_module_errors::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

read_module_or_file(Globals0, Globals, FileOrModuleName,
        ModuleName, FileNameDotM, ReturnTimestamp, MaybeTimestamp,
        ParseTreeSrc, Specs, Errors, !HaveReadModuleMaps, !IO) :-
    (
        FileOrModuleName = fm_module(ModuleName),
        globals.lookup_bool_option(Globals0, verbose, Verbose),
        maybe_write_string(Verbose, "% Parsing module `", !IO),
        ModuleNameString = sym_name_to_string(ModuleName),
        maybe_write_string(Verbose, ModuleNameString, !IO),
        maybe_write_string(Verbose, "' and imported interfaces...\n", !IO),
        ( if
            % Avoid rereading the module if it was already read
            % by recompilation_version.m.
            find_read_module_src(!.HaveReadModuleMaps ^ hrmm_src, ModuleName,
                ReturnTimestamp, FileNameDotMPrime, MaybeTimestampPrime,
                ParseTreeSrcPrime, SpecsPrime, ErrorsPrime)
        then
            Globals = Globals0,
            % XXX When we have read the module before, it *could* have had
            % problems that should cause smart recompilation to be disabled.
            HaveReadModuleMapSrc0 = !.HaveReadModuleMaps ^ hrmm_src,
            map.delete(ModuleName,
                HaveReadModuleMapSrc0, HaveReadModuleMapSrc),
            !HaveReadModuleMaps ^ hrmm_src := HaveReadModuleMapSrc,
            FileNameDotM = FileNameDotMPrime,
            MaybeTimestamp = MaybeTimestampPrime,
            ParseTreeSrc = ParseTreeSrcPrime,
            Specs = SpecsPrime,
            Errors = ErrorsPrime
        else
            % We don't search `--search-directories' for source files
            % because that can result in the generated interface files
            % being created in the wrong directory.
            read_module_src(Globals0, "Reading module",
                do_not_ignore_errors, do_not_search,
                ModuleName, [], FileNameDotM,
                always_read_module(ReturnTimestamp), MaybeTimestamp,
                ParseTreeSrc, Specs, Errors, !IO),
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
        globals.lookup_bool_option(Globals, statistics, Stats),
        maybe_report_stats(Stats, !IO)
    ;
        FileOrModuleName = fm_file(FileName),
        FileNameDotM = FileName ++ ".m",
        globals.lookup_bool_option(Globals0, verbose, Verbose),
        maybe_write_string(Verbose, "% Parsing file `", !IO),
        maybe_write_string(Verbose, FileNameDotM, !IO),
        maybe_write_string(Verbose, "' and imported interfaces...\n", !IO),

        file_name_to_module_name(FileName, DefaultModuleName),
        ( if
            % Avoid rereading the module if it was already read
            % by recompilation_version.m.
            find_read_module_src(!.HaveReadModuleMaps ^ hrmm_src,
                DefaultModuleName, ReturnTimestamp, _, MaybeTimestampPrime,
                ParseTreeSrcPrime, SpecsPrime, ErrorsPrime)
        then
            Globals = Globals0,
            % XXX When we have read the module before, it *could* have had
            % problems that should cause smart recompilation to be disabled.
            HaveReadModuleMapSrc0 = !.HaveReadModuleMaps ^ hrmm_src,
            map.delete(ModuleName,
                HaveReadModuleMapSrc0, HaveReadModuleMapSrc),
            !HaveReadModuleMaps ^ hrmm_src := HaveReadModuleMapSrc,
            ModuleName = DefaultModuleName,
            MaybeTimestamp = MaybeTimestampPrime,
            ParseTreeSrc = ParseTreeSrcPrime,
            Specs = SpecsPrime,
            Errors = ErrorsPrime
        else
            % We don't search `--search-directories' for source files
            % because that can result in the generated interface files
            % being created in the wrong directory.
            read_module_src_from_file(Globals0, FileName, FileNameDotM,
                "Reading file", do_not_search,
                always_read_module(ReturnTimestamp), MaybeTimestamp,
                ParseTreeSrc, Specs, Errors, !IO),
            ParseTreeSrc = parse_tree_src(ModuleName, _, _),
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
        maybe_report_stats(Stats, !IO)
    ).

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
:- pred augment_and_process_all_submodules(globals::in,
    op_mode_augment::in, string::in, module_name::in,
    maybe(timestamp)::in, set(module_name)::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    list(raw_compilation_unit)::in, list(error_spec)::in,
    list(string)::out, list(string)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

augment_and_process_all_submodules(Globals, OpModeAugment,
        FileName, SourceFileModuleName, MaybeTimestamp, NestedSubModules,
        FindTimestampFiles, RawCompUnits, !.Specs,
        ModulesToLink, ExtraObjFiles, !HaveReadModuleMaps, !IO) :-
    list.map_foldl3(
        augment_and_process_module(Globals, OpModeAugment,
            FileName, SourceFileModuleName, MaybeTimestamp, NestedSubModules,
            FindTimestampFiles),
        RawCompUnits, ExtraObjFileLists, !Specs, !HaveReadModuleMaps, !IO),
    write_error_specs_ignore(Globals, !.Specs, !IO),
    list.map(module_to_link, RawCompUnits, ModulesToLink),
    list.condense(ExtraObjFileLists, ExtraObjFiles).

:- pred module_to_link(raw_compilation_unit::in, string::out) is det.

module_to_link(raw_compilation_unit(ModuleName, _, _), ModuleToLink) :-
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
:- pred augment_and_process_module(globals::in,
    op_mode_augment::in, file_name::in, module_name::in,
    maybe(timestamp)::in, set(module_name)::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    raw_compilation_unit::in, list(string)::out,
    list(error_spec)::in, list(error_spec)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

augment_and_process_module(Globals, OpModeAugment,
        SourceFileName, SourceFileModuleName, MaybeTimestamp,
        NestedSubModules0, FindTimestampFiles, RawCompUnit, ExtraObjFiles,
        !Specs, !HaveReadModuleMaps, !IO) :-
    RawCompUnit = raw_compilation_unit(ModuleName, ModuleNameContext,
        RawItemBlocks),
    check_interface_item_blocks_for_no_exports(Globals,
        ModuleName, ModuleNameContext, RawItemBlocks, !Specs),
    ( if ModuleName = SourceFileModuleName then
        NestedSubModules = NestedSubModules0
    else
        set.init(NestedSubModules)
    ),
    grab_qual_imported_modules_augment(Globals, SourceFileName,
        SourceFileModuleName, MaybeTimestamp, NestedSubModules, RawCompUnit,
        ModuleAndImports, !HaveReadModuleMaps, !IO),
    module_and_imports_get_aug_comp_unit(ModuleAndImports, _AugCompUnit,
        ImportedSpecs, Errors),
    !:Specs = ImportedSpecs ++ !.Specs,
    set.intersect(Errors, fatal_read_module_errors, FatalErrors),
    ( if set.is_empty(FatalErrors) then
        process_augmented_module(Globals, OpModeAugment, ModuleAndImports,
            NestedSubModules, FindTimestampFiles, ExtraObjFiles,
            no_prev_dump, _, !Specs, !HaveReadModuleMaps, !IO)
    else
        ExtraObjFiles = []
    ).

:- pred process_augmented_module(globals::in, op_mode_augment::in,
    module_and_imports::in, set(module_name)::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    list(string)::out, dump_info::in, dump_info::out,
    list(error_spec)::in, list(error_spec)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

process_augmented_module(Globals0, OpModeAugment, ModuleAndImports,
        NestedSubModules, FindTimestampFiles, ExtraObjFiles,
        !DumpInfo, !Specs, !HaveReadModuleMaps, !IO) :-
    (
        ( OpModeAugment = opmau_typecheck_only
        ; OpModeAugment = opmau_errorcheck_only
        ),
        Globals = Globals0,
        % If we are only typechecking or error checking, then we should not
        % modify any files; this includes writing to .d files.
        WriteDFile = do_not_write_d_file
    ;
        OpModeAugment = opmau_make_trans_opt_int,
        disable_warning_options(Globals0, Globals),
        WriteDFile = write_d_file
    ;
        OpModeAugment = opmau_generate_code(_),
        Globals = Globals0,
        WriteDFile = write_d_file
    ;
        OpModeAugment = opmau_make_opt_int,
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
    pre_hlds_pass(Globals, OpModeAugment, WriteDFile, ModuleAndImports, HLDS1,
        QualInfo, MaybeTimestampMap, UndefTypes, UndefModes, PreHLDSErrors,
        !DumpInfo, !Specs, !HaveReadModuleMaps, !IO),
    frontend_pass(OpModeAugment, QualInfo, UndefTypes, UndefModes,
        PreHLDSErrors, FrontEndErrors, HLDS1, HLDS20, !DumpInfo, !Specs, !IO),
    ( if
        PreHLDSErrors = no,
        FrontEndErrors = no,
        contains_errors(Globals, !.Specs) = no
    then
        globals.lookup_bool_option(Globals, verbose, Verbose),
        globals.lookup_bool_option(Globals, statistics, Stats),
        maybe_write_dependency_graph(Verbose, Stats, HLDS20, HLDS21, !IO),
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
                globals.set_option(optimize_unused_args, bool(no),
                    Globals, NoOptUnusedArgsGlobals),
                module_info_set_globals(NoOptUnusedArgsGlobals,
                    HLDS21, HLDS21a),
                maybe_unused_args(Verbose, Stats, _UnusedArgsInfos,
                    HLDS21a, _HLDS22, !Specs, !IO)
            ;
                UnusedArgs = no
            ),
            ExtraObjFiles = []
        ;
            OpModeAugment = opmau_make_opt_int,
            % Only run up to typechecking when making the .opt file.
            ExtraObjFiles = []
        ;
            OpModeAugment = opmau_make_trans_opt_int,
            output_trans_opt_file(HLDS21, !Specs, !DumpInfo, !IO),
            ExtraObjFiles = []
        ;
            OpModeAugment = opmau_make_analysis_registry,
            prepare_for_intermodule_analysis(Globals, Verbose, Stats,
                HLDS21, HLDS22, !IO),
            output_analysis_file(HLDS22, !Specs, !DumpInfo, !IO),
            ExtraObjFiles = []
        ;
            OpModeAugment = opmau_make_xml_documentation,
            xml_documentation(HLDS21, !IO),
            ExtraObjFiles = []
        ;
            OpModeAugment = opmau_generate_code(OpModeCodeGen),
            maybe_prepare_for_intermodule_analysis(Globals, Verbose, Stats,
                HLDS21, HLDS22, !IO),
            after_front_end_passes(Globals, OpModeCodeGen, NestedSubModules,
                FindTimestampFiles, MaybeTimestampMap, HLDS22,
                ExtraObjFiles, !Specs, !DumpInfo, !IO)
        )
    else
        % If the number of errors is > 0, make sure that the compiler
        % exits with a non-zero exit status.
        io.get_exit_status(ExitStatus, !IO),
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

:- type maybe_write_d_file
    --->    do_not_write_d_file
    ;       write_d_file.

:- pred pre_hlds_pass(globals::in, op_mode_augment::in, maybe_write_d_file::in,
    module_and_imports::in, module_info::out, make_hlds_qual_info::out,
    maybe(module_timestamp_map)::out, bool::out, bool::out, bool::out,
    dump_info::in, dump_info::out, list(error_spec)::in, list(error_spec)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

pre_hlds_pass(Globals, OpModeAugment, WriteDFile0, ModuleAndImports0, HLDS1,
        QualInfo, MaybeTimestampMap, UndefTypes, UndefModes,
        FoundSemanticError, !DumpInfo, !Specs, !HaveReadModuleMaps, !IO) :-
    globals.lookup_bool_option(Globals, statistics, Stats),
    globals.lookup_bool_option(Globals, verbose, Verbose),

    globals.lookup_bool_option(Globals, invoked_by_mmc_make, MMCMake),
    ( if
        % Don't write the `.d' file when making the `.opt' file because
        % we can't work out the full transitive implementation dependencies.
        ( MMCMake = yes
        ; OpModeAugment = opmau_make_opt_int
        )
    then
        WriteDFile = do_not_write_d_file
    else
        WriteDFile = WriteDFile0
    ),

    module_and_imports_get_module_name(ModuleAndImports0, ModuleName),
    (
        WriteDFile = do_not_write_d_file,
        MaybeTransOptDeps = no
    ;
        WriteDFile = write_d_file,
        % We need the MaybeTransOptDeps when creating the .trans_opt file.
        % However, we *also* need the MaybeTransOptDeps when writing out
        % .d files. In the absence of MaybeTransOptDeps, we will write out
        % a .d file that does not include the trans_opt_deps mmake rule,
        % which will require an "mmake depend" before the next rebuild.
        maybe_read_d_file_for_trans_opt_deps(Globals, ModuleName,
            MaybeTransOptDeps, !IO)
    ),

    % Errors in .opt and .trans_opt files result in software errors.
    maybe_grab_plain_and_trans_opt_files(Globals, OpModeAugment, Verbose,
        MaybeTransOptDeps, IntermodError, ModuleAndImports0, ModuleAndImports1,
        !HaveReadModuleMaps, !IO),

    % We pay attention to IntermodError instead of _Error. XXX Is this right?
    module_and_imports_get_aug_comp_unit(ModuleAndImports1, AugCompUnit1,
        ItemSpecs, _Error),
    !:Specs = ItemSpecs ++ !.Specs,
    module_and_imports_get_maybe_timestamp_map(ModuleAndImports1,
        MaybeTimestampMap),

    globals.lookup_string_option(Globals, event_set_file_name,
        EventSetFileName),
    ( if EventSetFileName = "" then
        EventSetName = "",
        EventSpecMap1 = map.init,
        EventSetErrors = no
    else
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

    pre_hlds_maybe_write_out_errors(Verbose, Globals, !Specs, !IO),
    maybe_write_string(Verbose, "% Module qualifying items...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    module_qualify_aug_comp_unit(Globals, AugCompUnit1, AugCompUnit2,
        EventSpecMap1, EventSpecMap2, EventSetFileName, MQInfo0,
        MQUndefTypes, MQUndefInsts, MQUndefModes, MQUndefTypeClasses,
        [], QualifySpecs),
    !:Specs = QualifySpecs ++ !.Specs,
    pre_hlds_maybe_write_out_errors(Verbose, Globals, !Specs, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO),

    mq_info_get_recompilation_info(MQInfo0, RecompInfo0),
    pre_hlds_maybe_write_out_errors(Verbose, Globals, !Specs, !IO),
    maybe_write_string(Verbose,
        "% Expanding equivalence types and insts...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    expand_eqv_types_insts(AugCompUnit2, AugCompUnit,
        EventSpecMap2, EventSpecMap, TypeEqvMap, UsedModules,
        RecompInfo0, RecompInfo, ExpandSpecs),
    ExpandErrors = contains_errors(Globals, ExpandSpecs),
    !:Specs = ExpandSpecs ++ !.Specs,
    pre_hlds_maybe_write_out_errors(Verbose, Globals, !Specs, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO),
    mq_info_set_recompilation_info(RecompInfo, MQInfo0, MQInfo),

    EventSet = event_set(EventSetName, EventSpecMap),
    make_hlds(Globals, AugCompUnit, EventSet, MQInfo, TypeEqvMap, UsedModules,
        Verbose, Stats, HLDS0, QualInfo,
        MakeHLDSFoundInvalidType, MakeHLDSFoundInvalidInstOrMode,
        FoundSemanticError, !Specs, !IO),
    maybe_write_definitions(Verbose, Stats, HLDS0, !IO),
    maybe_write_definition_line_counts(Verbose, Stats, HLDS0, !IO),
    maybe_write_definition_extents(Verbose, Stats, HLDS0, !IO),

    ( if
        MQUndefTypes = did_not_find_undef_type,
        MQUndefTypeClasses = did_not_find_undef_typeclass,
        EventSetErrors = no,
        ExpandErrors = no,
        MakeHLDSFoundInvalidType = did_not_find_invalid_type
    then
        UndefTypes = no
    else
        UndefTypes = yes
    ),
    ( if
        MQUndefInsts = did_not_find_undef_inst,
        MQUndefModes = did_not_find_undef_mode,
        MakeHLDSFoundInvalidInstOrMode = did_not_find_invalid_inst_or_mode
    then
        UndefModes = no
    else
        UndefModes = yes
    ),

    maybe_dump_hlds(HLDS0, 1, "initial", !DumpInfo, !IO),

    (
        WriteDFile = do_not_write_d_file
    ;
        WriteDFile = write_d_file,
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
    ( if
        ( FoundSemanticError = yes
        ; IntermodError = yes
        )
    then
        module_info_incr_errors(HLDS0, HLDS1)
    else
        HLDS1 = HLDS0
    ).

%---------------------%

    % maybe_read_d_file_for_trans_opt_deps(Globals, ModuleName,
    %   MaybeTransOptDeps, !IO):
    %
    % If transitive intermodule optimization has been enabled, then read
    % <ModuleName>.d to find the modules which <ModuleName>.trans_opt may
    % depend on. Otherwise return `no'.
    %
:- pred maybe_read_d_file_for_trans_opt_deps(globals::in, module_name::in,
    maybe(list(module_name))::out, io::di, io::uo) is det.

maybe_read_d_file_for_trans_opt_deps(Globals, ModuleName,
        MaybeTransOptDeps, !IO) :-
    globals.lookup_bool_option(Globals, transitive_optimization, TransOpt),
    (
        TransOpt = yes,
        globals.lookup_bool_option(Globals, verbose, Verbose),
        module_name_to_file_name(Globals, $pred, do_not_create_dirs,
            ext_other(other_ext(".d")), ModuleName, DependencyFileName, !IO),
        maybe_write_string(Verbose, "% Reading auto-dependency file `", !IO),
        maybe_write_string(Verbose, DependencyFileName, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        maybe_flush_output(Verbose, !IO),
        io.open_input(DependencyFileName, OpenResult, !IO),
        (
            OpenResult = ok(Stream),
            io.set_input_stream(Stream, OldStream, !IO),
            module_name_to_file_name(Globals, $pred, do_not_create_dirs,
                ext_other(other_ext(".trans_opt_date")),
                ModuleName, TransOptDateFileName, !IO),
            SearchPattern = TransOptDateFileName ++ " :",
            read_dependency_file_find_start(SearchPattern, FindResult, !IO),
            (
                FindResult = yes,
                read_dependency_file_get_modules(TransOptDeps, !IO),
                MaybeTransOptDeps = yes(TransOptDeps)
            ;
                FindResult = no,
                % error reading .d file
                MaybeTransOptDeps = no
            ),
            io.set_input_stream(OldStream, _, !IO),
            io.close_input(Stream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            OpenResult = error(IOError),
            maybe_write_string(Verbose, " failed.\n", !IO),
            maybe_flush_output(Verbose, !IO),
            io.error_message(IOError, IOErrorMessage),
            string.append_list(["error opening file `", DependencyFileName,
                "' for input: ", IOErrorMessage], Message),
            report_error(Message, !IO),
            MaybeTransOptDeps = no
        )
    ;
        TransOpt = no,
        MaybeTransOptDeps = no
    ).

    % Read lines from the dependency file (module.d) until one is found
    % which begins with SearchPattern.
    %
:- pred read_dependency_file_find_start(string::in, bool::out,
    io::di, io::uo) is det.

read_dependency_file_find_start(SearchPattern, Success, !IO) :-
    io.read_line_as_string(Result, !IO),
    (
        Result = ok(Line),
        ( if string.prefix(Line, SearchPattern) then
            % Have found the start.
            Success = yes
        else
            read_dependency_file_find_start(SearchPattern, Success, !IO)
        )
    ;
        ( Result = error(_)
        ; Result = eof
        ),
        Success = no
    ).

    % Read lines until one is found which does not contain whitespace
    % followed by a word which ends in .trans_opt. Remove the .trans_opt
    % ending from all the words which are read in and return the resulting
    % list of modules.
    %
:- pred read_dependency_file_get_modules(list(module_name)::out,
    io::di, io::uo) is det.

read_dependency_file_get_modules(TransOptDeps, !IO) :-
    io.read_line(Result, !IO),
    ( if
        Result = ok(CharList0),
        % Remove any whitespace from the beginning of the line,
        % then take all characters until another whitespace occurs.
        list.drop_while(char.is_whitespace, CharList0, CharList1),
        NotIsWhitespace = (pred(Char::in) is semidet :-
            not char.is_whitespace(Char)
        ),
        list.take_while(NotIsWhitespace, CharList1, CharList),
        string.from_char_list(CharList, FileName0),
        string.remove_suffix(FileName0, ".trans_opt", FileName)
    then
        ( if string.append("Mercury/trans_opts/", BaseFileName, FileName) then
            ModuleFileName = BaseFileName
        else
            ModuleFileName = FileName
        ),
        file_name_to_module_name(ModuleFileName, Module),
        read_dependency_file_get_modules(TransOptDeps0, !IO),
        TransOptDeps = [Module | TransOptDeps0]
    else
        TransOptDeps = []
    ).

%---------------------%

:- pred maybe_grab_plain_and_trans_opt_files(globals::in, op_mode_augment::in,
    bool::in, maybe(list(module_name))::in, bool::out,
    module_and_imports::in, module_and_imports::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

maybe_grab_plain_and_trans_opt_files(Globals, OpModeAugment, Verbose,
        MaybeTransOptDeps, Error, ModuleAndImports0, ModuleAndImports,
        !HaveReadModuleMaps, !IO) :-
    globals.lookup_bool_option(Globals, intermodule_optimization, IntermodOpt),
    globals.lookup_bool_option(Globals, use_opt_files, UseOptInt),
    globals.lookup_bool_option(Globals, transitive_optimization, TransOpt),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    ( if
        ( UseOptInt = yes
        ; IntermodOpt = yes
        ; IntermodAnalysis = yes
        ),
        OpModeAugment \= opmau_make_opt_int
    then
        maybe_write_string(Verbose, "% Reading .opt files...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        grab_plain_opt_and_int_for_opt_files(Globals, Error1,
            ModuleAndImports0, ModuleAndImports1, !HaveReadModuleMaps, !IO),
        maybe_write_string(Verbose, "% Done.\n", !IO)
    else
        ModuleAndImports1 = ModuleAndImports0,
        Error1 = no
    ),
    (
        OpModeAugment = opmau_make_trans_opt_int,
        (
            MaybeTransOptDeps = yes(TransOptDeps),
            % When creating the trans_opt file, only import the
            % trans_opt files which are lower in the ordering.
            grab_trans_opt_files(Globals, TransOptDeps, Error2,
                ModuleAndImports1, ModuleAndImports, !HaveReadModuleMaps, !IO)
        ;
            MaybeTransOptDeps = no,
            ModuleAndImports = ModuleAndImports1,
            Error2 = no,
            module_and_imports_get_module_name(ModuleAndImports, ModuleName),
            globals.lookup_bool_option(Globals, warn_missing_trans_opt_deps,
                WarnNoTransOptDeps),
            (
                WarnNoTransOptDeps = yes,
                Pieces = [words("Warning: cannot read trans-opt dependencies"),
                    words("for module"), qual_sym_name(ModuleName),
                    suffix("."), nl,
                    words("You need to remake the dependencies."), nl],
                Msg = error_msg(no, do_not_treat_as_first, 0,
                    [always(Pieces)]),
                Spec = error_spec($pred, severity_warning, phase_read_files,
                    [Msg]),
                write_error_spec_ignore(Globals, Spec, !IO)
            ;
                WarnNoTransOptDeps = no
            )
        )
    ;
        OpModeAugment = opmau_make_opt_int,
        % If we are making the `.opt' file, then we cannot read any
        % `.trans_opt' files, since `.opt' files aren't allowed to depend on
        % `.trans_opt' files.
        ModuleAndImports = ModuleAndImports1,
        Error2 = no
    ;
        ( OpModeAugment = opmau_make_analysis_registry
        ; OpModeAugment = opmau_make_xml_documentation
        ; OpModeAugment = opmau_typecheck_only
        ; OpModeAugment = opmau_errorcheck_only
        ; OpModeAugment = opmau_generate_code(_)
        ),
        (
            TransOpt = yes,
            % If transitive optimization is enabled, but we are not creating
            % the .opt or .trans opt file, then import the trans_opt files
            % for all the modules that are imported (or used), and for all
            % ancestor modules.
            module_and_imports_get_ancestors(ModuleAndImports0, Ancestors),
            module_and_imports_get_int_deps_set(ModuleAndImports0, IntDeps),
            module_and_imports_get_imp_deps_set(ModuleAndImports0, ImpDeps),
            TransOptFiles = set.union_list([Ancestors, IntDeps, ImpDeps]),
            set.to_sorted_list(TransOptFiles, TransOptFilesList),
            grab_trans_opt_files(Globals, TransOptFilesList, Error2,
                ModuleAndImports1, ModuleAndImports, !HaveReadModuleMaps, !IO)
        ;
            TransOpt = no,
            ModuleAndImports = ModuleAndImports1,
            Error2 = no
        )
    ),
    bool.or(Error1, Error2, Error).

%---------------------%

:- pred make_hlds(globals::in, aug_compilation_unit::in,
    event_set::in, mq_info::in, type_eqv_map::in, used_modules::in,
    bool::in, bool::in, module_info::out, make_hlds_qual_info::out,
    found_invalid_type::out, found_invalid_inst_or_mode::out, bool::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

make_hlds(Globals, AugCompUnit, EventSet, MQInfo, TypeEqvMap, UsedModules,
        Verbose, Stats, !:HLDS, QualInfo,
        FoundInvalidType, FoundInvalidInstOrMode,
        FoundSemanticError, !Specs, !IO) :-
    pre_hlds_maybe_write_out_errors(Verbose, Globals, !Specs, !IO),
    maybe_write_string(Verbose, "% Converting parse tree to hlds...\n", !IO),
    ModuleName = aug_compilation_unit_project_name(AugCompUnit),
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".hlds_dump")), ModuleName, DumpBaseFileName, !IO),
    parse_tree_to_hlds(AugCompUnit, Globals, DumpBaseFileName, MQInfo,
        TypeEqvMap, UsedModules, QualInfo,
        FoundInvalidType, FoundInvalidInstOrMode, !:HLDS, MakeSpecs),
    !:Specs = MakeSpecs ++ !.Specs,
    module_info_set_event_set(EventSet, !HLDS),
    io.get_exit_status(Status, !IO),
    SpecsErrors = contains_errors(Globals, !.Specs),
    ( if
        ( Status \= 0
        ; SpecsErrors = yes
        )
    then
        FoundSemanticError = yes,
        io.set_exit_status(1, !IO)
    else
        FoundSemanticError = no
    ),
    pre_hlds_maybe_write_out_errors(Verbose, Globals, !Specs, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%---------------------%

:- pred maybe_write_definitions(bool::in, bool::in,
    module_info::in, io::di, io::uo) is det.

maybe_write_definitions(Verbose, Stats, HLDS, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, show_definitions, ShowDefns),
    (
        ShowDefns = yes,
        maybe_write_string(Verbose, "% Writing definitions...", !IO),
        module_info_get_name(HLDS, ModuleName),
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".defns")), ModuleName, FileName, !IO),
        io.open_output(FileName, Res, !IO),
        (
            Res = ok(FileStream),
            hlds.hlds_defns.write_hlds_defns(FileStream, HLDS, !IO),
            io.close_output(FileStream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            Res = error(IOError),
            ErrorMsg = "unable to write definitions: " ++
                io.error_message(IOError),
            report_error(ErrorMsg, !IO)
        ),
        maybe_report_stats(Stats, !IO)
    ;
        ShowDefns = no
    ).

:- pred maybe_write_definition_line_counts(bool::in, bool::in,
    module_info::in, io::di, io::uo) is det.

maybe_write_definition_line_counts(Verbose, Stats, HLDS, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, show_definition_line_counts,
        LineCounts),
    (
        LineCounts = yes,
        maybe_write_string(Verbose,
            "% Writing definition line counts...", !IO),
        module_info_get_name(HLDS, ModuleName),
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".defn_line_counts")),
            ModuleName, FileName, !IO),
        io.open_output(FileName, Res, !IO),
        (
            Res = ok(FileStream),
            hlds.hlds_defns.write_hlds_defn_line_counts(FileStream, HLDS, !IO),
            io.close_output(FileStream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            Res = error(IOError),
            ErrorMsg = "unable to write definition line counts: " ++
                io.error_message(IOError),
            report_error(ErrorMsg, !IO)
        ),
        maybe_report_stats(Stats, !IO)
    ;
        LineCounts = no
    ).

:- pred maybe_write_definition_extents(bool::in, bool::in,
    module_info::in, io::di, io::uo) is det.

maybe_write_definition_extents(Verbose, Stats, HLDS, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, show_definition_extents, Extents),
    (
        Extents = yes,
        maybe_write_string(Verbose,
            "% Writing definition extents...", !IO),
        module_info_get_name(HLDS, ModuleName),
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".defn_extents")), ModuleName, FileName, !IO),
        io.open_output(FileName, Res, !IO),
        (
            Res = ok(FileStream),
            hlds.hlds_defns.write_hlds_defn_extents(FileStream, HLDS, !IO),
            io.close_output(FileStream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            Res = error(IOError),
            ErrorMsg = "unable to write definition extents: " ++
                io.error_message(IOError),
            report_error(ErrorMsg, !IO)
        ),
        maybe_report_stats(Stats, !IO)
    ;
        Extents = no
    ).

%---------------------------------------------------------------------------%

:- pred maybe_write_dependency_graph(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_write_dependency_graph(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, show_dependency_graph, ShowDepGraph),
    (
        ShowDepGraph = yes,
        maybe_write_string(Verbose, "% Writing dependency graph...", !IO),
        module_info_get_name(!.HLDS, ModuleName),
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".dependency_graph")),
            ModuleName, FileName, !IO),
        io.open_output(FileName, Res, !IO),
        (
            Res = ok(FileStream),
            io.set_output_stream(FileStream, OutputStream, !IO),
            write_dependency_graph(!HLDS, !IO),
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

%---------------------------------------------------------------------------%

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
    LocalModuleNames = set.list_to_set(SymNames),

    module_info_get_analysis_info(!.HLDS, AnalysisInfo0),
    prepare_intermodule_analysis(Globals, ModuleNames, LocalModuleNames,
        AnalysisInfo0, AnalysisInfo, !IO),
    module_info_set_analysis_info(AnalysisInfo, !HLDS),

    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%---------------------------------------------------------------------------%

:- pred after_front_end_passes(globals::in, op_mode_codegen::in,
    set(module_name)::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    maybe(module_timestamp_map)::in, module_info::in,
    list(string)::out, list(error_spec)::in, list(error_spec)::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

after_front_end_passes(Globals, OpModeCodeGen, NestedSubModules,
        FindTimestampFiles, MaybeTimestampMap, !.HLDS,
        ExtraObjFiles, !Specs, !DumpInfo, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),
    maybe_output_prof_call_graph(Verbose, Stats, !HLDS, !IO),
    middle_pass(!HLDS, !DumpInfo, !Specs, !IO),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    globals.get_target(Globals, Target),

    % Remove any existing `.used' file before writing the output file.
    % This avoids leaving the old `used' file lying around if compilation
    % is interrupted after the new output file is written but before the new
    % `.used' file is written.

    module_info_get_name(!.HLDS, ModuleName),
    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(other_ext(".used")), ModuleName, UsageFileName, !IO),
    io.remove_file(UsageFileName, _, !IO),

    globals.lookup_bool_option(Globals, halt_at_warn, HaltAtWarn),
    (
        HaltAtWarn = no,
        FrontEndErrors = contains_errors(Globals, !.Specs)
    ;
        HaltAtWarn = yes,
        FrontEndErrors = contains_errors_and_or_warnings(Globals, !.Specs)
    ),
    module_info_get_num_errors(!.HLDS, NumErrors),
    ( if
        FrontEndErrors = no,
        NumErrors = 0
    then
        (
            Target = target_csharp,
            mlds_backend(!.HLDS, _, MLDS, NewSpecs, !DumpInfo, !IO),
            !:Specs = NewSpecs ++ !.Specs,
            % mlds_to_csharp never goes beyond generating C# code.
            mlds_to_csharp(!.HLDS, MLDS, Succeeded, !IO),
            ExtraObjFiles = []
        ;
            Target = target_java,
            mlds_backend(!.HLDS, _, MLDS, NewSpecs, !DumpInfo, !IO),
            !:Specs = NewSpecs ++ !.Specs,
            mlds_to_java(!.HLDS, MLDS, TargetCodeSucceeded, !IO),
            (
                OpModeCodeGen = opmcg_target_code_only,
                Succeeded = TargetCodeSucceeded
            ;
                ( OpModeCodeGen = opmcg_target_and_object_code_only
                ; OpModeCodeGen = opmcg_target_object_and_executable
                ),
                (
                    TargetCodeSucceeded = no,
                    Succeeded = no
                ;
                    TargetCodeSucceeded = yes,
                    io.output_stream(OutputStream, !IO),
                    module_name_to_file_name(Globals, $pred,
                        do_not_create_dirs, ext_other(other_ext(".java")),
                        ModuleName, JavaFile, !IO),
                    compile_java_files(Globals, OutputStream, [JavaFile],
                        Succeeded, !IO),
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
            export.produce_header_file(!.HLDS, ExportDecls, ModuleName, !IO),
            (
                HighLevelCode = yes,
                mlds_backend(!.HLDS, _, MLDS, NewSpecs, !DumpInfo, !IO),
                !:Specs = NewSpecs ++ !.Specs,
                mlds_to_high_level_c(Globals, MLDS, TargetCodeSucceeded, !IO),
                (
                    OpModeCodeGen = opmcg_target_code_only,
                    Succeeded = TargetCodeSucceeded
                ;
                    ( OpModeCodeGen = opmcg_target_and_object_code_only
                    ; OpModeCodeGen = opmcg_target_object_and_executable
                    ),
                    (
                        TargetCodeSucceeded = no,
                        Succeeded = no
                    ;
                        TargetCodeSucceeded = yes,
                        module_name_to_file_name(Globals, $pred,
                            do_not_create_dirs, ext_other(other_ext(".c")),
                            ModuleName, C_File, !IO),
                        get_linked_target_type(Globals, TargetType),
                        get_object_code_type(Globals, TargetType, PIC),
                        pic_object_file_extension(Globals, PIC, ObjOtherExt),
                        module_name_to_file_name(Globals, $pred,
                            do_create_dirs, ext_other(ObjOtherExt),
                            ModuleName, O_File, !IO),
                        io.output_stream(OutputStream, !IO),
                        do_compile_c_file(Globals, OutputStream, PIC,
                            C_File, O_File, Succeeded, !IO),
                        maybe_set_exit_status(Succeeded, !IO)
                    )
                ),
                ExtraObjFiles = []
            ;
                HighLevelCode = no,
                llds_backend_pass(!HLDS, GlobalData, LLDS, !DumpInfo, !IO),
                % llds_output_pass looks up the target_code_only option
                % to see whether it should generate object code, using the
                % same logic as the HighLevelCode = yes case above.
                % XXX Move that logic here, for symmetry.
                llds_output_pass(OpModeCodeGen, !.HLDS, GlobalData, LLDS,
                    ModuleName, Succeeded, ExtraObjFiles, !IO)
            )
        ;
            Target = target_erlang,
            erlang_backend(!.HLDS, ELDS, !DumpInfo, !IO),
            % elds_to_erlang never goes beyond generating Erlang code.
            elds_to_erlang(!.HLDS, ELDS, Succeeded, !IO),
            ExtraObjFiles = []
        ),
        (
            Succeeded = yes,
            recompilation.usage.write_usage_file(!.HLDS, NestedSubModules,
                MaybeTimestampMap, !IO),
            FindTimestampFiles(ModuleName, TimestampFiles, !IO),
            list.foldl(touch_datestamp(Globals), TimestampFiles, !IO)
        ;
            Succeeded = no
            % An error should have been reported earlier.
        )
    else
        % If the number of errors is > 0, make sure that the compiler
        % exits with a non-zero exit status.
        io.get_exit_status(ExitStatus, !IO),
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
:- pred maybe_output_prof_call_graph(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_output_prof_call_graph(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, profile_calls, ProfileCalls),
    globals.lookup_bool_option(Globals, profile_time, ProfileTime),
    ( if
        ( ProfileCalls = yes
        ; ProfileTime = yes
        )
    then
        maybe_write_string(Verbose,
            "% Outputting profiling call graph...", !IO),
        maybe_flush_output(Verbose, !IO),
        module_info_get_name(!.HLDS, ModuleName),
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".prof")), ModuleName, ProfFileName, !IO),
        io.open_output(ProfFileName, Res, !IO),
        (
            Res = ok(FileStream),
            io.set_output_stream(FileStream, OutputStream, !IO),
            write_prof_dependency_graph(!HLDS, !IO),
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
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred dump_args(string::in, list(string)::in, io::di, io::uo) is det.

dump_args(Marker, Args, !IO) :-
    io.format("%s START\n", [s(Marker)], !IO),
    io.write_list(Args, "", dump_arg, !IO),
    io.format("%s END\n", [s(Marker)], !IO).

:- pred dump_arg(string::in, io::di, io::uo) is det.

dump_arg(Arg, !IO) :-
    io.format("<%s>\n", [s(Arg)], !IO).

%---------------------------------------------------------------------------%

:- pred halt_at_module_error(globals::in, read_module_errors::in) is semidet.

halt_at_module_error(Globals, Errors) :-
    set.is_non_empty(Errors),
    (
        globals.lookup_bool_option(Globals, halt_at_syntax_errors, HaltSyntax),
        HaltSyntax = yes
    ;
        set.intersect(Errors, fatal_read_module_errors, FatalErrors),
        set.is_non_empty(FatalErrors)
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

%---------------------------------------------------------------------------%
:- end_module top_level.mercury_compile_main.
%---------------------------------------------------------------------------%
