%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mercury_compile.m.
% Main authors: fjh, zs.

% This is the top-level of the Mercury compiler.

% This module invokes the different passes of the compiler as appropriate.
% The constraints on pass ordering are documented in
% compiler/notes/compiler_design.html.

%-----------------------------------------------------------------------------%

:- module top_level__mercury_compile.
:- interface.

:- import_module io.
:- import_module list.

    % This is the main entry point for the Mercury compiler.
    % It is called from top_level.main.
    %
:- pred real_main(io::di, io::uo) is det.

    % main(Args).
    %
:- pred main(list(string)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

    %
    % the main compiler passes (mostly in order of execution)
    %

    % semantic analysis
:- import_module parse_tree__prog_foreign.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__modules.
:- import_module parse_tree__source_file_map.
:- import_module parse_tree__module_qual.
:- import_module parse_tree__equiv_type.
:- import_module hlds__make_hlds.
:- import_module check_hlds__typecheck.
:- import_module check_hlds__purity.
:- import_module check_hlds__polymorphism.
:- import_module check_hlds__modes.
:- import_module check_hlds__mode_constraints.
:- import_module check_hlds__switch_detection.
:- import_module check_hlds__cse_detection.
:- import_module check_hlds__det_analysis.
:- import_module check_hlds__unique_modes.
:- import_module check_hlds__stratify.
:- import_module check_hlds__simplify.

    % high-level HLDS transformations
:- import_module check_hlds__check_typeclass.
:- import_module transform_hlds__intermod.
:- import_module transform_hlds__trans_opt.
:- import_module transform_hlds__equiv_type_hlds.
:- import_module transform_hlds__table_gen.
:- import_module transform_hlds__complexity.
:- import_module transform_hlds__lambda.
:- import_module transform_hlds__closure_analysis.
:- import_module transform_hlds__termination.
:- import_module transform_hlds__term_constr_main.
:- import_module transform_hlds__exception_analysis.
:- import_module transform_hlds__higher_order.
:- import_module transform_hlds__accumulator.
:- import_module transform_hlds__tupling.
:- import_module transform_hlds__untupling.
:- import_module transform_hlds__inlining.
:- import_module transform_hlds__loop_inv.
:- import_module transform_hlds__deforest.
:- import_module aditi_backend__aditi_builtin_ops.
:- import_module aditi_backend__dnf.
:- import_module aditi_backend__magic.
:- import_module transform_hlds__dead_proc_elim.
:- import_module transform_hlds__delay_construct.
:- import_module transform_hlds__unused_args.
:- import_module transform_hlds__unneeded_code.
:- import_module transform_hlds__lco.
:- import_module transform_hlds__size_prof.
:- import_module ll_backend__deep_profiling.

    % the LLDS back-end
:- import_module ll_backend__saved_vars.
:- import_module ll_backend__stack_opt.
:- import_module ll_backend__stack_alloc.
:- import_module ll_backend__follow_code.
:- import_module ll_backend__liveness.
:- import_module ll_backend__live_vars.
:- import_module ll_backend__store_alloc.
:- import_module ll_backend__code_gen.
:- import_module ll_backend__optimize.
:- import_module ll_backend__transform_llds.
:- import_module ll_backend__llds_out.
:- import_module ll_backend__continuation_info.
:- import_module ll_backend__stack_layout.
:- import_module ll_backend__global_data.
:- import_module ll_backend__dupproc.

    % the Aditi-RL back-end
:- import_module aditi_backend__rl_gen.
:- import_module aditi_backend__rl_opt.
:- import_module aditi_backend__rl_out.

    % the bytecode back-end
:- import_module bytecode_backend__bytecode_gen.
:- import_module bytecode_backend__bytecode.

    % the MLDS back-end
:- import_module ml_backend__add_trail_ops.         % HLDS -> HLDS
:- import_module ml_backend__add_heap_ops.          % HLDS -> HLDS
:- import_module ml_backend__mark_static_terms.     % HLDS -> HLDS
:- import_module ml_backend__mlds.                  % MLDS data structure
:- import_module ml_backend__ml_code_gen.
:- import_module ml_backend__rtti_to_mlds.          % HLDS/RTTI -> MLDS
:- import_module ml_backend__ml_elim_nested.        % MLDS -> MLDS
:- import_module ml_backend__ml_tailcall.           % MLDS -> MLDS
:- import_module ml_backend__ml_optimize.           % MLDS -> MLDS
:- import_module ml_backend__mlds_to_c.             % MLDS -> C
:- import_module ml_backend__mlds_to_java.          % MLDS -> Java
:- import_module ml_backend__mlds_to_ilasm.         % MLDS -> IL assembler
:- import_module ml_backend__maybe_mlds_to_gcc.     % MLDS -> GCC back-end
:- import_module ml_backend__ml_util.               % MLDS utility predicates

    % miscellaneous compiler modules
:- import_module aditi_backend__rl.
:- import_module aditi_backend__rl_dump.
:- import_module aditi_backend__rl_file.
:- import_module check_hlds__goal_path.
:- import_module hlds__arg_info.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_out.
:- import_module hlds__hlds_pred.
:- import_module hlds__passes_aux.
:- import_module ll_backend__layout.
:- import_module ll_backend__llds.
:- import_module make.
:- import_module make__options_file.
:- import_module make__util.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_util.
:- import_module recompilation.
:- import_module recompilation__check.
:- import_module recompilation__usage.
:- import_module transform_hlds__dependency_graph.

    % inter-module analysis framework
:- import_module analysis.
:- import_module transform_hlds__mmc_analysis.

    % compiler library modules
:- import_module backend_libs__base_typeclass_info.
:- import_module backend_libs__compile_target_code.
:- import_module backend_libs__export.
:- import_module backend_libs__foreign.
:- import_module backend_libs__name_mangle.
:- import_module backend_libs__proc_label.
:- import_module backend_libs__rtti.
:- import_module backend_libs__type_class_info.
:- import_module backend_libs__type_ctor_info.
:- import_module libs__globals.
:- import_module libs__handle_options.
:- import_module libs__options.
:- import_module libs__timestamp.
:- import_module libs__trace_params.

    % library modules
:- import_module assoc_list.
:- import_module benchmarking.
:- import_module bool.
:- import_module dir.
:- import_module gc.
:- import_module getopt_io.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module pprint.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

real_main(!IO) :-
    gc_init(!IO),

        % All messages go to stderr
    io__stderr_stream(StdErr, !IO),
    io__set_output_stream(StdErr, _, !IO),
    io__command_line_arguments(Args0, !IO),

    % read_args_file and globals__io_printing_usage may attempt
    % to look up options, so we need to initialize the globals.
    handle_options([], _, _, _, _, !IO),

    ( Args0 = ["--arg-file", ArgFile] ->
        %
        % All the configuration and options file options
        % are passed in the given file, which is created
        % by the parent `mmc --make' process.
        %

        options_file__read_args_file(ArgFile, MaybeArgs1, !IO),
        (
            MaybeArgs1 = yes(Args1),
            process_options(Args1, OptionArgs, NonOptionArgs, _, !IO),
            MaybeMCFlags = yes([])
        ;
            MaybeArgs1 = no,
            OptionArgs = [],
            NonOptionArgs = [],
            MaybeMCFlags = yes([])
        ),
        Variables = options_variables_init,
        Link = no
    ;
        %
        % Find out which options files to read.
        %
        handle_options(Args0, Errors0, OptionArgs, NonOptionArgs, Link, !IO),
        (
            Errors0 = [_ | _],
            usage_errors(Errors0, !IO),
            Variables = options_variables_init,
            MaybeMCFlags = no
        ;
            Errors0 = [],
            read_options_files(options_variables_init, MaybeVariables0, !IO),
            (
                MaybeVariables0 = yes(Variables0),
                lookup_mmc_options(Variables0, MaybeMCFlags0, !IO),
                (
                    MaybeMCFlags0 = yes(MCFlags0),
                    real_main_2(MCFlags0, MaybeMCFlags, Args0,
                        Variables0, Variables, !IO)

                ;
                    MaybeMCFlags0 = no,
                    Variables = options_variables_init,
                    MaybeMCFlags = no
                )
            ;
                MaybeVariables0 = no,
                Variables = options_variables_init,
                MaybeMCFlags = no
            )
        )
    ),
    (
        MaybeMCFlags = yes(MCFlags),
        handle_options(MCFlags ++ OptionArgs, Errors, _, _, _, !IO),

        %
        % When computing the option arguments to pass
        % to `--make', only include the command-line
        % arguments, not the contents of DEFAULT_MCFLAGS.
        %
        main_2(Errors, Variables, OptionArgs, NonOptionArgs, Link, !IO)
    ;
        MaybeMCFlags = no,
        io__set_exit_status(1, !IO)
    ).

:- pred real_main_2(list(string)::in, maybe(list(string))::out,
    list(string)::in, options_variables::in, options_variables::out,
    io::di, io::uo) is det.

real_main_2(MCFlags0, MaybeMCFlags, Args0, Variables0, Variables, !IO) :-
    %
    % Process the options again to find out
    % which configuration file to read.
    %
    handle_options(MCFlags0 ++ Args0, Errors, _, _, _, !IO),
    (
        Errors = [_ | _],
        usage_errors(Errors, !IO),
        Variables = options_variables_init,
        MaybeMCFlags = no
    ;
        Errors = [],
        globals__io_lookup_maybe_string_option(config_file,
            MaybeConfigFile, !IO),
        (
            MaybeConfigFile = yes(ConfigFile),
            read_options_file(ConfigFile, Variables0, MaybeVariables, !IO),
            (
                MaybeVariables = yes(Variables),
                lookup_mmc_options(Variables, MaybeMCFlags, !IO)
            ;
                MaybeVariables = no,
                MaybeMCFlags = no,
                Variables = options_variables_init
            )
        ;
            MaybeConfigFile = no,
            Variables = options_variables_init,
            lookup_mmc_options(Variables, MaybeMCFlags, !IO)
        )
    ).

main(Args, !IO) :-
    main_2([], options_variables_init, [], Args, no, !IO).

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

:- pred main_2(list(string)::in, options_variables::in, list(string)::in,
    list(string)::in, bool::in, io::di, io::uo) is det.

main_2(Errors @ [_ | _], _, _, _, _, !IO) :-
    usage_errors(Errors, !IO).
main_2([], OptionVariables, OptionArgs, Args, Link, !IO) :-
    globals__io_get_globals(Globals, !IO),
    globals__lookup_bool_option(Globals, version, Version),
    globals__lookup_bool_option(Globals, help, Help),
    globals__lookup_bool_option(Globals, generate_source_file_mapping,
        GenerateMapping),
    globals__lookup_bool_option(Globals, output_grade_string,
        OutputGrade),
    globals__lookup_bool_option(Globals, output_link_command,
        OutputLinkCommand),
    globals__lookup_bool_option(Globals, output_shared_lib_link_command,
        OutputShLibLinkCommand),
    globals__lookup_bool_option(Globals, filenames_from_stdin,
        FileNamesFromStdin),
    globals__lookup_bool_option(Globals, make, Make),
    ( Version = yes ->
        io__stdout_stream(Stdout, !IO),
        io__set_output_stream(Stdout, OldOutputStream, !IO),
        display_compiler_version(!IO),
        io__set_output_stream(OldOutputStream, _, !IO)
    ; Help = yes ->
        io__stdout_stream(Stdout, !IO),
        io__set_output_stream(Stdout, OldOutputStream, !IO),
        long_usage(!IO),
        io__set_output_stream(OldOutputStream, _, !IO)
    ; OutputGrade = yes ->
        % When Mmake asks for the grade, it really wants
        % the directory component to use. This is consistent
        % with scripts/canonical_grade.
        grade_directory_component(Globals, Grade),
        io__stdout_stream(Stdout, !IO),
        io__write_string(Stdout, Grade, !IO),
        io__write_string(Stdout, "\n", !IO)
    ; OutputLinkCommand = yes ->
        globals__lookup_string_option(Globals, link_executable_command,
            LinkCommand),
        io__stdout_stream(Stdout, !IO),
        io__write_string(Stdout, LinkCommand, !IO),
        io__write_string(Stdout, "\n", !IO)
    ; OutputShLibLinkCommand = yes ->
        globals__lookup_string_option(Globals, link_shared_lib_command,
            LinkCommand),
        io__stdout_stream(Stdout, !IO),
        io__write_string(Stdout, LinkCommand, !IO),
        io__write_string(Stdout, "\n", !IO)
    ; GenerateMapping = yes ->
        source_file_map__write_source_file_map(Args, !IO)
    ; Make = yes ->
        make__process_args(OptionVariables, OptionArgs, Args, !IO)
    ; Args = [], FileNamesFromStdin = no ->
        usage(!IO)
    ;
        process_all_args(OptionVariables, OptionArgs,
            Args, ModulesToLink, FactTableObjFiles, !IO),
        io__get_exit_status(ExitStatus, !IO),
        ( ExitStatus = 0 ->
            (
                Link = yes,
                ModulesToLink = [FirstModule | _]
            ->
                file_name_to_module_name(FirstModule,
                    MainModuleName),
                globals__get_target(Globals, Target),
                ( Target = java ->
                    % For Java, at the "link" step we just
                    % generate a shell script; the actual
                    % linking will be done at runtime by
                    % the Java interpreter.
                    create_java_shell_script(MainModuleName, Succeeded, !IO)
                ;
                    compile_with_module_options(MainModuleName,
                        OptionVariables, OptionArgs,
                        link_module_list(ModulesToLink, FactTableObjFiles),
                        Succeeded, !IO)
                ),
                maybe_set_exit_status(Succeeded, !IO)
            ;
                true
            )
        ;
            % If we found some errors, but the user didn't enable
            % the `-E' (`--verbose-errors') option, give them a
            % hint about it.  Of course, we should only output the
            % hint when we have further information to give the user.
            
            globals__lookup_bool_option(Globals, verbose_errors,
                VerboseErrors),
            globals__get_extra_error_info(Globals, ExtraErrorInfo), 
            (
                VerboseErrors = no,
                (
                    ExtraErrorInfo = yes,
                    io__write_string("For more information, " ++
                        "recompile with `-E'.\n", !IO)
                ;
                    ExtraErrorInfo = no
                )
            ;
                VerboseErrors = yes
            )
        ),
        globals__lookup_bool_option(Globals, statistics, Statistics),
        (
            Statistics = yes,
            io__report_stats("full_memory_stats", !IO)
        ;
            Statistics = no
        )
    ).

:- pred process_all_args(options_variables::in, list(string)::in,
    list(string)::in, list(string)::out, list(string)::out,
    io::di, io::uo) is det.

process_all_args(OptionVariables, OptionArgs, Args, ModulesToLink,
        FactTableObjFiles, !IO) :-
    % Because of limitations in the GCC back-end,
    % we can only call the GCC back-end once (per process),
    % to generate a single assembler file, rather than
    % calling it multiple times to generate individual
    % assembler files for each module.
    % So if we're generating code using the GCC back-end,
    % we need to call run_gcc_backend here at the top level.
    globals__io_get_globals(Globals, !IO),
    ( compiling_to_asm(Globals) ->
        ( Args = [FirstArg | OtherArgs] ->
            globals__lookup_bool_option(Globals, smart_recompilation, Smart),
            (
                Smart = yes,
                (
                    OtherArgs = [],
                    % With smart recompilation we need to delay
                    % starting the gcc backend to avoid overwriting
                    % the output assembler file even if
                    % recompilation is found to be unnecessary.
                    process_args(OptionVariables, OptionArgs, Args,
                        ModulesToLink, FactTableObjFiles, !IO)
                ;
                    OtherArgs = [_ | _],
                    Msg = "Sorry, not implemented: " ++
                        "`--target asm' with `--smart-recompilation' " ++
                        "with more than one module to compile.",
                    write_error_pieces_plain([words(Msg)], !IO),
                    io__set_exit_status(1, !IO),
                    ModulesToLink = [],
                    FactTableObjFiles = []
                )
            ;
                Smart = no,
                compile_using_gcc_backend(OptionVariables, OptionArgs,
                    string_to_file_or_module(FirstArg),
                    process_args_no_fact_table(OptionVariables, OptionArgs,
                        Args),
                    ModulesToLink, !IO),
                FactTableObjFiles = []
            )
        ;
            Msg = "Sorry, not implemented: `--target asm' " ++
                "with `--filenames-from-stdin",
            write_error_pieces_plain([words(Msg)], !IO),
            io__set_exit_status(1, !IO),
            ModulesToLink = [],
            FactTableObjFiles = []
        )
    ;
        % If we're NOT using the GCC back-end,
        % then we can just call process_args directly,
        % rather than via GCC.
        process_args(OptionVariables, OptionArgs, Args, ModulesToLink,
            FactTableObjFiles, !IO)
    ).

:- pred compiling_to_asm(globals::in) is semidet.

compiling_to_asm(Globals) :-
    globals__get_target(Globals, asm),
    % even if --target asm is specified,
    % it can be overridden by other options:
    OptionList = [convert_to_mercury, generate_dependencies, make_interface,
        make_short_interface, make_private_interface,
        make_optimization_interface, make_transitive_opt_interface,
        typecheck_only, errorcheck_only],
    BoolList = list__map((func(Opt) = Bool :-
        globals__lookup_bool_option(Globals, Opt, Bool)),
        OptionList),
    bool__or_list(BoolList) = no.

:- pred compile_using_gcc_backend(options_variables::in, list(string)::in,
    file_or_module::in,
    frontend_callback(list(string))::in(frontend_callback),
    list(string)::out, io::di, io::uo) is det.

compile_using_gcc_backend(OptionVariables, OptionArgs, FirstFileOrModule,
        CallBack, ModulesToLink, !IO) :-
    % The name of the assembler file that we generate
    % is based on name of the first module named
    % on the command line.  (Mmake requires this.)
    %
    % There's two cases:
    % (1) If the argument ends in ".m", we assume
    % that the argument is a file name.
    % To find the corresponding module name,
    % we would need to read in the file
    % (at least up to the first item);
    % this is needed to handle the case where
    % the module name does not match the file
    % name, e.g. file "browse.m" containing
    % ":- module mdb__browse." as its first item.
    % Rather than reading in the source file here,
    % we just pick a name
    % for the asm file based on the file name argument,
    % (e.g. "browse.s") and if necessary rename it later
    % (e.g. to "mdb.browse.s").
    %
    % (2) If the argument doesn't end in `.m',
    % then we assume it is a module name.
    % (Is it worth checking that the name doesn't
    % contain directory separators, and issuing
    % a warning or error in that case?)
    %
    (
        FirstFileOrModule = file(FirstFileName),
        file_name_to_module_name(FirstFileName, FirstModuleName)
    ;
        FirstFileOrModule = module(FirstModuleName)
    ),

    % Invoke run_gcc_backend.  It will call us back,
    % and then we'll continue with the normal work of
    % the compilation, which will be done by the callback
    % function (`process_args').
    maybe_mlds_to_gcc__run_gcc_backend(FirstModuleName, CallBack,
        ModulesToLink, !IO),

    % Now we know what the real module name was, so we
    % can rename the assembler file if needed (see above).
    ( ModulesToLink = [Module | _] ->
        file_name_to_module_name(Module, ModuleName),
        globals__io_lookup_bool_option(pic, Pic, !IO),
        AsmExt = (Pic = yes -> ".pic_s" ; ".s"),
        module_name_to_file_name(ModuleName, AsmExt, yes, AsmFile, !IO),
        ( ModuleName \= FirstModuleName ->
            module_name_to_file_name(FirstModuleName, AsmExt, no, FirstAsmFile,
                !IO),
            do_rename_file(FirstAsmFile, AsmFile, Result, !IO)
        ;
            Result = ok
        ),

        % Invoke the assembler to produce an object file,
        % if needed.
        globals__io_lookup_bool_option(target_code_only, TargetCodeOnly, !IO),
        (
            Result = ok,
            TargetCodeOnly = no
        ->
            io__output_stream(OutputStream, !IO),
            get_linked_target_type(TargetType, !IO),
            get_object_code_type(TargetType, PIC, !IO),
            compile_with_module_options(ModuleName, OptionVariables,
                OptionArgs,
                compile_target_code__assemble(OutputStream, PIC, ModuleName),
                AssembleOK, !IO),
            maybe_set_exit_status(AssembleOK, !IO)
        ;
            true
        )
    ;
        % This can happen if smart recompilation decided
        % that nothing needed to be compiled.
        true
    ).

:- pred do_rename_file(string::in, string::in, io__res::out,
    io::di, io::uo) is det.

do_rename_file(OldFileName, NewFileName, Result, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    maybe_write_string(Verbose, "% Renaming `", !IO),
    maybe_write_string(Verbose, OldFileName, !IO),
    maybe_write_string(Verbose, "' as `", !IO),
    maybe_write_string(Verbose, NewFileName, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    io__rename_file(OldFileName, NewFileName, Result0, !IO),
    ( Result0 = error(Error0) ->
        maybe_write_string(VeryVerbose, " failed.\n", !IO),
        maybe_flush_output(VeryVerbose, !IO),
        io__error_message(Error0, ErrorMsg0),
        % On some systems, we need to remove the existing file
        % first, if any.  So try again that way.
        maybe_write_string(VeryVerbose, "% Removing `", !IO),
        maybe_write_string(VeryVerbose, OldFileName, !IO),
        maybe_write_string(VeryVerbose, "'...", !IO),
        maybe_flush_output(VeryVerbose, !IO),
        io__remove_file(NewFileName, Result1, !IO),
        ( Result1 = error(Error1) ->
            maybe_write_string(Verbose, " failed.\n", !IO),
            maybe_flush_output(Verbose, !IO),
            io__error_message(Error1, ErrorMsg1),
            string__append_list(["can't rename file `", OldFileName,
                "' as `", NewFileName, "': ", ErrorMsg0,
                "; and can't remove file `", NewFileName, "': ", ErrorMsg1],
                Message),
            report_error(Message, !IO),
            Result = Result1
        ;
            maybe_write_string(VeryVerbose, " done.\n", !IO),
            maybe_write_string(VeryVerbose, "% Renaming `", !IO),
            maybe_write_string(VeryVerbose, OldFileName, !IO),
            maybe_write_string(VeryVerbose, "' as `", !IO),
            maybe_write_string(VeryVerbose, NewFileName, !IO),
            maybe_write_string(VeryVerbose, "' again...", !IO),
            maybe_flush_output(VeryVerbose, !IO),
            io__rename_file(OldFileName, NewFileName, Result2, !IO),
            ( Result2 = error(Error2) ->
                maybe_write_string(Verbose, " failed.\n", !IO),
                maybe_flush_output(Verbose, !IO),
                io__error_message(Error2, ErrorMsg),
                string__append_list(
                    ["can't rename file `", OldFileName, "' as `", NewFileName,
                    "': ", ErrorMsg], Message),
                report_error(Message, !IO)
            ;
                maybe_write_string(Verbose, " done.\n", !IO)
            ),
            Result = Result2
        )
    ;
        maybe_write_string(Verbose, " done.\n", !IO),
        Result = Result0
    ).

:- pred process_args_no_fact_table(options_variables::in, list(string)::in,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

process_args_no_fact_table(OptionVariables, OptionArgs, Args, ModulesToLink,
        !IO) :-
    process_args(OptionVariables, OptionArgs, Args, ModulesToLink,
        _FactTableObjFiles, !IO).

:- pred process_args(options_variables::in, list(string)::in, list(string)::in,
    list(string)::out, list(string)::out, io::di, io::uo) is det.

process_args(OptionVariables, OptionArgs, Args, ModulesToLink,
        FactTableObjFiles, !IO) :-
    globals__io_lookup_bool_option(filenames_from_stdin, FileNamesFromStdin,
        !IO),
    (
        FileNamesFromStdin = yes,
        process_stdin_arg_list(OptionVariables, OptionArgs,
            [], ModulesToLink, [], FactTableObjFiles, !IO)
    ;
        FileNamesFromStdin = no,
        process_arg_list(OptionVariables, OptionArgs,
            Args, ModulesToLink, FactTableObjFiles, !IO)
    ).

:- pred process_stdin_arg_list(options_variables::in, list(string)::in,
    list(string)::in, list(string)::out,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

process_stdin_arg_list(OptionVariables, OptionArgs, !Modules,
        !FactTableObjFiles, !IO) :-
    (
        !.Modules = [_ | _],
        garbage_collect(!IO)
    ;
        !.Modules = []
    ),
    io__read_line_as_string(FileResult, !IO),
    (
        FileResult = ok(Line),
        Arg = string.rstrip(Line),
        process_arg(OptionVariables, OptionArgs, Arg, Module,
            FactTableObjFileList, !IO),
        list__append(Module, !Modules),
        list__append(FactTableObjFileList, !FactTableObjFiles),
        process_stdin_arg_list(OptionVariables, OptionArgs,
            !Modules, !FactTableObjFiles, !IO)
    ;
        FileResult = eof
    ;
        FileResult = error(Error),
        io__error_message(Error, Msg),
        io__write_string("Error reading module name: ", !IO),
        io__write_string(Msg, !IO),
        io__set_exit_status(1, !IO)
    ).

:- pred process_arg_list(options_variables::in, list(string)::in,
    list(string)::in, list(string)::out, list(string)::out,
    io::di, io::uo) is det.

process_arg_list(OptionVariables, OptionArgs, Args, Modules, FactTableObjFiles,
        !IO) :-
    process_arg_list_2(OptionVariables, OptionArgs, Args, ModulesList,
        FactTableObjFileLists, !IO),
    list__condense(ModulesList, Modules),
    list__condense(FactTableObjFileLists, FactTableObjFiles).

:- pred process_arg_list_2(options_variables::in, list(string)::in,
    list(string)::in, list(list(string))::out, list(list(string))::out,
    io::di, io::uo) is det.

process_arg_list_2(_, _, [], [], [], !IO).
process_arg_list_2(OptionVariables, OptionArgs, [Arg | Args],
        [Modules | ModulesList],
        [FactTableObjFiles | FactTableObjFileLists], !IO) :-
    process_arg(OptionVariables, OptionArgs, Arg, Modules, FactTableObjFiles,
        !IO),
    (
        Args = [_ | _],
        garbage_collect(!IO)
    ;
        Args = []
    ),
    process_arg_list_2(OptionVariables, OptionArgs, Args, ModulesList,
        FactTableObjFileLists, !IO).

    % Figure out whether the argument is a module name or a file name.
    % Open the specified file or module, and process it.
    % Return the list of modules (including sub-modules,
    % if they were compiled to seperate object files)
    % that should be linked into the final executable.

:- pred process_arg(options_variables::in, list(string)::in, string::in,
    list(string)::out, list(string)::out, io::di, io::uo) is det.

process_arg(OptionVariables, OptionArgs, Arg, ModulesToLink, FactTableObjFiles,
        !IO) :-
    FileOrModule = string_to_file_or_module(Arg),
    globals__io_lookup_bool_option(invoked_by_mmc_make, InvokedByMake, !IO),
    (
        InvokedByMake = no,
        build_with_module_options(file_or_module_to_module_name(FileOrModule),
            OptionVariables, OptionArgs, [],
            process_arg_build(FileOrModule, OptionVariables, OptionArgs),
            _, [], MaybePair, !IO),
        (
            MaybePair = yes(ModulesToLink - FactTableObjFiles)
        ;
            MaybePair = no,
            ModulesToLink = [],
            FactTableObjFiles = []
        )
    ;
        InvokedByMake = yes,
        % `mmc --make' has already set up the options.
        process_arg_2(OptionVariables, OptionArgs, FileOrModule,
            ModulesToLink, FactTableObjFiles, !IO)
    ).

:- pred process_arg_build(file_or_module::in, options_variables::in,
    list(string)::in, list(string)::in, bool::out,
    list(string)::in, pair(list(string))::out, io::di, io::uo) is det.

process_arg_build(FileOrModule, OptionVariables, OptionArgs, _, yes,
        _, Modules - FactTableObjFiles, !IO) :-
    process_arg_2(OptionVariables, OptionArgs, FileOrModule, Modules,
        FactTableObjFiles, !IO).

:- pred process_arg_2(options_variables::in, list(string)::in,
    file_or_module::in, list(string)::out, list(string)::out,
    io::di, io::uo) is det.

process_arg_2(OptionVariables, OptionArgs, FileOrModule, ModulesToLink,
        FactTableObjFiles, !IO) :-
    globals__io_lookup_bool_option(generate_dependencies, GenerateDeps, !IO),
    (
        GenerateDeps = yes,
        ModulesToLink = [],
        FactTableObjFiles = [],
        (
            FileOrModule = file(FileName),
            generate_file_dependencies(FileName, !IO)
        ;
            FileOrModule = module(ModuleName),
            generate_module_dependencies(ModuleName, !IO)
        )
    ;
        GenerateDeps = no,
        process_module(OptionVariables, OptionArgs,
            FileOrModule, ModulesToLink, FactTableObjFiles, !IO)
    ).

:- type file_or_module
    --->    file(file_name)
    ;   module(module_name).

:- func string_to_file_or_module(string) = file_or_module.

string_to_file_or_module(String) = FileOrModule :-
    ( string__remove_suffix(String, ".m", FileName) ->
        % If the argument name ends in `.m', then we assume it is
        % a file name.
        FileOrModule = file(FileName)
    ;
        % If it doesn't end in `.m', then we assume it is
        % a module name.  (Is it worth checking that the
        % name doesn't contain directory separators, and issuing
        % a warning or error in that case?)
        file_name_to_module_name(String, ModuleName),
        FileOrModule = module(ModuleName)
    ).

:- func file_or_module_to_module_name(file_or_module) = module_name.

file_or_module_to_module_name(file(FileName)) = ModuleName :-
    % Assume the module name matches the file name.
    file_name_to_module_name(FileName, ModuleName).
file_or_module_to_module_name(module(ModuleName)) = ModuleName.

:- pred read_module(file_or_module::in, bool::in, module_name::out,
    file_name::out, maybe(timestamp)::out, item_list::out,
    module_error::out, read_modules::in, read_modules::out,
    io::di, io::uo) is det.

read_module(module(ModuleName), ReturnTimestamp, ModuleName, FileName,
        MaybeTimestamp, Items, Error, !ReadModules, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Parsing module `", !IO),
    mdbcomp__prim_data__sym_name_to_string(ModuleName, ModuleNameString),
    maybe_write_string(Verbose, ModuleNameString, !IO),
    maybe_write_string(Verbose, "' and imported interfaces...\n", !IO),
    (
        % Avoid rereading the module if it was already read
        % by recompilation_version.m.
        find_read_module(!.ReadModules, ModuleName, ".m", ReturnTimestamp,
            Items0, MaybeTimestamp0, Error0, FileName0)
    ->
        map__delete(!.ReadModules, ModuleName - ".m", !:ReadModules),
        FileName = FileName0,
        Items = Items0,
        Error = Error0,
        MaybeTimestamp = MaybeTimestamp0
    ;
        % We don't search `--search-directories' for source files
        % because that can result in the generated interface files
        % being created in the wrong directory.
        Search = no,
        read_mod(ModuleName, ".m", "Reading module", Search, ReturnTimestamp,
            Items, Error, FileName, MaybeTimestamp, !IO)
    ),
    globals__io_lookup_bool_option(statistics, Stats, !IO),
    maybe_report_stats(Stats, !IO).
read_module(file(FileName), ReturnTimestamp, ModuleName, SourceFileName,
        MaybeTimestamp, Items, Error, !ReadModules, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Parsing file `", !IO),
    maybe_write_string(Verbose, FileName, !IO),
    maybe_write_string(Verbose, "' and imported interfaces...\n", !IO),

    file_name_to_module_name(FileName, DefaultModuleName),
    (
        % Avoid rereading the module if it was already read
        % by recompilation_version.m.
        find_read_module(!.ReadModules, DefaultModuleName, ".m",
            ReturnTimestamp, Items0, MaybeTimestamp0, Error0, _)
    ->
        map__delete(!.ReadModules, ModuleName - ".m", !:ReadModules),
        ModuleName = DefaultModuleName,
        Items = Items0,
        Error = Error0,
        MaybeTimestamp = MaybeTimestamp0
    ;
        % We don't search `--search-directories' for source files
        % because that can result in the generated interface files
        % being created in the wrong directory.
        Search = no,
        read_mod_from_file(FileName, ".m", "Reading file", Search,
            ReturnTimestamp, Items, Error, ModuleName, MaybeTimestamp, !IO),

        %
        % XXX If the module name doesn't match the file name
        % the compiler won't be able to find the `.used'
        % file (the name of the `.used' file is derived from
        % the module name not the file name).
        % This will be fixed when mmake functionality
        % is moved into the compiler.
        %
        globals__io_lookup_bool_option(smart_recompilation, Smart, !IO),
        (
            Smart = yes,
            ModuleName \= DefaultModuleName
        ->
            globals__io_lookup_bool_option(warn_smart_recompilation, Warn,
                !IO),
            (
                Warn = yes,
                Msg1 = "Warning: module name does not " ++ "match file name: ",
                ModuleNameStr = describe_sym_name(ModuleName),
                Msg2 = FileName ++ " contains module `" ++ ModuleNameStr ++
                    "'.",
                Msg3 = "Smart recompilation will not work unless " ++
                    "a module name to file name mapping is created using " ++
                    "`mmc -f *.m'.",
                write_error_pieces_plain([words(Msg1), nl, words(Msg2), nl,
                    words(Msg3)], !IO),
                record_warning(!IO)
            ;
                Warn = no
            ),
            globals__io_set_option(smart_recompilation, bool(no), !IO)
        ;
            true
        )
    ),
    globals__io_lookup_bool_option(statistics, Stats, !IO),
    maybe_report_stats(Stats, !IO),
    string__append(FileName, ".m", SourceFileName).

:- pred process_module(options_variables::in, list(string)::in,
    file_or_module::in, list(string)::out, list(string)::out,
    io::di, io::uo) is det.

process_module(OptionVariables, OptionArgs, FileOrModule, ModulesToLink,
        FactTableObjFiles, !IO) :-
    globals__io_get_globals(Globals, !IO),
    globals__lookup_bool_option(Globals, halt_at_syntax_errors,
        HaltSyntax),
    globals__lookup_bool_option(Globals, make_interface, MakeInterface),
    globals__lookup_bool_option(Globals, make_short_interface,
        MakeShortInterface),
    globals__lookup_bool_option(Globals, make_private_interface,
        MakePrivateInterface),
    globals__lookup_bool_option(Globals, convert_to_mercury,
        ConvertToMercury),
    globals__lookup_bool_option(Globals, generate_item_version_numbers,
        GenerateVersionNumbers),
    (
        ( MakeInterface = yes ->
            ProcessModule = make_interface,
            ReturnTimestamp = GenerateVersionNumbers
        ; MakeShortInterface = yes ->
            ProcessModule = make_short_interface,
            ReturnTimestamp = no
        ; MakePrivateInterface = yes ->
            ProcessModule = make_private_interface,
            ReturnTimestamp = GenerateVersionNumbers
        ;
            fail
        )
    ->
        read_module(FileOrModule, ReturnTimestamp, ModuleName, FileName,
            MaybeTimestamp, Items, Error, map__init, _, !IO),
        ( halt_at_module_error(HaltSyntax, Error) ->
            true
        ;
            split_into_submodules(ModuleName, Items, SubModuleList, !IO),
            list__foldl(apply_process_module(ProcessModule,
                FileName, ModuleName, MaybeTimestamp), SubModuleList, !IO)
        ),
        ModulesToLink = [],
        FactTableObjFiles = []
    ;
        ConvertToMercury = yes
    ->
        read_module(FileOrModule, no, ModuleName, _, _,
            Items, Error, map__init, _, !IO),
        ( halt_at_module_error(HaltSyntax, Error) ->
            true
        ;
            module_name_to_file_name(ModuleName, ".ugly", yes, OutputFileName,
                !IO),
            convert_to_mercury(ModuleName, OutputFileName, Items, !IO)
        ),
        ModulesToLink = [],
        FactTableObjFiles = []
    ;
        globals__lookup_bool_option(Globals, smart_recompilation,
            Smart),
        globals__get_target(Globals, Target),
        (
            Smart = yes,
            (
                FileOrModule = module(ModuleName)
            ;
                FileOrModule = file(FileName),
                % XXX This won't work if the module name
                % doesn't match the file name -- such
                % modules will always be recompiled.
                %
                % This problem will be fixed when mmake
                % functionality is moved into the compiler.
                % The file_name->module_name mapping
                % will be explicitly recorded.
                file_name_to_module_name(FileName, ModuleName)
            ),

            find_smart_recompilation_target_files(ModuleName, Globals,
                FindTargetFiles),
            find_timestamp_files(ModuleName, Globals, FindTimestampFiles),
            recompilation__check__should_recompile(ModuleName, FindTargetFiles,
                FindTimestampFiles, ModulesToRecompile0, ReadModules, !IO),
            (
                Target = asm,
                ModulesToRecompile0 = some([_ | _])
            ->
                %
                % With `--target asm', if one module
                % needs to be recompiled, all need to be
                % recompiled because they are all compiled
                % into a single object file.
                %
                ModulesToRecompile = (all)
            ;
                ModulesToRecompile = ModulesToRecompile0
            )
        ;
            Smart = no,
            map__init(ReadModules),
            ModulesToRecompile = (all)
        ),
        ( ModulesToRecompile = some([]) ->
            % XXX Currently smart recompilation is disabled
            % if mmc is linking the executable because it
            % doesn't know how to check whether all the
            % necessary intermediate files are present
            % and up-to-date.
            ModulesToLink = [],
            FactTableObjFiles = []
        ;
            (
                Target = asm,
                Smart = yes
            ->
                % See the comment in process_all_args.
                compile_using_gcc_backend(OptionVariables,
                    OptionArgs, FileOrModule,
                    process_module_2_no_fact_table(FileOrModule,
                        ModulesToRecompile, ReadModules),
                    ModulesToLink, !IO),
                FactTableObjFiles = []
            ;
                process_module_2(FileOrModule, ModulesToRecompile, ReadModules,
                    ModulesToLink, FactTableObjFiles, !IO)
            )
        )
    ).

:- pred apply_process_module(
    pred(file_name, module_name, maybe(timestamp),
        pair(module_name, item_list), io, io)::
        in(pred(in, in, in, in, di, uo) is det),
    file_name::in, module_name::in, maybe(timestamp)::in,
    pair(module_name, item_list)::in, io::di, io::uo) is det.

apply_process_module(ProcessModule, FileName, ModuleName, MaybeTimestamp,
        SubModule, !IO) :-
    ProcessModule(FileName, ModuleName, MaybeTimestamp, SubModule, !IO).

:- pred process_module_2_no_fact_table(file_or_module::in,
    modules_to_recompile::in, read_modules::in, list(string)::out,
    io::di, io::uo) is det.

process_module_2_no_fact_table(FileOrModule, MaybeModulesToRecompile,
        ReadModules0, ModulesToLink, !IO) :-
    process_module_2(FileOrModule, MaybeModulesToRecompile, ReadModules0,
        ModulesToLink, _FactTableObjFiles, !IO).

:- pred process_module_2(file_or_module::in, modules_to_recompile::in,
    read_modules::in, list(string)::out, list(string)::out,
    io::di, io::uo) is det.

process_module_2(FileOrModule, MaybeModulesToRecompile, ReadModules0,
        ModulesToLink, FactTableObjFiles, !IO) :-
    read_module(FileOrModule, yes, ModuleName, FileName,
        MaybeTimestamp, Items, Error, ReadModules0, ReadModules, !IO),
    globals__io_lookup_bool_option(halt_at_syntax_errors, HaltSyntax, !IO),
    ( halt_at_module_error(HaltSyntax, Error) ->
        ModulesToLink = [],
        FactTableObjFiles = []
    ;
        split_into_submodules(ModuleName, Items, SubModuleList0, !IO),
        ( MaybeModulesToRecompile = some(ModulesToRecompile) ->
            ToRecompile = (pred((SubModule - _)::in) is semidet :-
                list__member(SubModule, ModulesToRecompile)
            ),
            list__filter(ToRecompile, SubModuleList0, SubModuleListToCompile)
        ;
            SubModuleListToCompile = SubModuleList0
        ),
        assoc_list__keys(SubModuleList0, NestedSubModules0),
        list__delete_all(NestedSubModules0, ModuleName, NestedSubModules),

        globals__io_get_globals(Globals, !IO),
        find_timestamp_files(ModuleName, Globals, FindTimestampFiles),
        
        globals.io_lookup_bool_option(trace_prof, TraceProf, !IO),
       
        ( 
            any_mercury_builtin_module(ModuleName),
            not (
                    mercury_profiling_builtin_module(ModuleName),
                    TraceProf = yes
            )
        ->
            % Some predicates in the builtin modules are missing
            % typeinfo arguments, which means that execution
            % tracing will not work on them. Predicates defined
            % there should never be part of an execution trace
            % anyway; they are effectively language primitives.
            % (They may still be parts of stack traces.)
            globals__io_lookup_bool_option(trace_stack_layout, TSL, !IO),
            globals__io_get_trace_level(TraceLevel, !IO),

            globals__io_set_option(trace_stack_layout, bool(no), !IO),
            globals__io_set_trace_level_none(!IO),

            compile_all_submodules(FileName, ModuleName, NestedSubModules,
                MaybeTimestamp, ReadModules, FindTimestampFiles,
                SubModuleListToCompile, ModulesToLink, FactTableObjFiles, !IO),

            globals__io_set_option(trace_stack_layout, bool(TSL), !IO),
            globals__io_set_trace_level(TraceLevel, !IO)
        ;
            compile_all_submodules(FileName, ModuleName, NestedSubModules,
                MaybeTimestamp, ReadModules, FindTimestampFiles,
                SubModuleListToCompile, ModulesToLink, FactTableObjFiles, !IO)
        )
    ).

    % For the MLDS->C and LLDS->C back-ends, we currently
    % compile each sub-module to its own C file.
    % XXX it would be better to do something like
    %
    %   list__map2_foldl(compile_to_llds, SubModuleList,
    %       LLDS_FragmentList),
    %   merge_llds_fragments(LLDS_FragmentList, LLDS),
    %   output_pass(LLDS_FragmentList)
    %
    % i.e. compile nested modules to a single C file.

:- pred compile_all_submodules(string::in, module_name::in,
    list(module_name)::in, maybe(timestamp)::in, read_modules::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    list(pair(module_name, item_list))::in, list(string)::out,
    list(string)::out, io::di, io::uo) is det.

compile_all_submodules(FileName, SourceFileModuleName, NestedSubModules,
        MaybeTimestamp, ReadModules, FindTimestampFiles,
        SubModuleList, ModulesToLink, FactTableObjFiles, !IO) :-
    list__map_foldl(compile(FileName, SourceFileModuleName, NestedSubModules,
            MaybeTimestamp, ReadModules, FindTimestampFiles),
        SubModuleList, FactTableObjFileLists, !IO),
    list__map(module_to_link, SubModuleList, ModulesToLink),
    list__condense(FactTableObjFileLists, FactTableObjFiles).

:- pred make_interface(file_name::in, module_name::in, maybe(timestamp)::in,
    pair(module_name, item_list)::in, io::di, io::uo) is det.

make_interface(SourceFileName, SourceFileModuleName, MaybeTimestamp,
        ModuleName - Items, !IO) :-
    make_interface(SourceFileName, SourceFileModuleName,
        ModuleName, MaybeTimestamp, Items, !IO).

:- pred make_short_interface(file_name::in, module_name::in,
    maybe(timestamp)::in, pair(module_name, item_list)::in,
    io::di, io::uo) is det.

make_short_interface(SourceFileName, _, _, ModuleName - Items, !IO) :-
    make_short_interface(SourceFileName, ModuleName, Items, !IO).

:- pred make_private_interface(file_name::in, module_name::in,
    maybe(timestamp)::in, pair(module_name, item_list)::in,
    io::di, io::uo) is det.

make_private_interface(SourceFileName, SourceFileModuleName,
        MaybeTimestamp, ModuleName - Items, !IO) :-
    make_private_interface(SourceFileName, SourceFileModuleName,
        ModuleName, MaybeTimestamp, Items, !IO).

:- pred halt_at_module_error(bool::in, module_error::in) is semidet.

halt_at_module_error(_, fatal_module_errors).
halt_at_module_error(HaltSyntax, some_module_errors) :- HaltSyntax = yes.

:- pred module_to_link(pair(module_name, item_list)::in, string::out) is det.

module_to_link(ModuleName - _Items, ModuleToLink) :-
    module_name_to_file_name(ModuleName, ModuleToLink).

:- type compile == pred(bool, io, io).
:- inst compile == (pred(out, di, uo) is det).

:- pred compile_with_module_options(module_name::in, options_variables::in,
    list(string)::in, compile::in(compile), bool::out, io::di, io::uo)
    is det.

compile_with_module_options(ModuleName, OptionVariables, OptionArgs,
        Compile, Succeeded, !IO) :-
    globals__io_lookup_bool_option(invoked_by_mmc_make, InvokedByMake, !IO),
    (
        InvokedByMake = yes,
        % `mmc --make' has already set up the options.
        Compile(Succeeded, !IO)
    ;
        InvokedByMake = no,
        build_with_module_options(ModuleName, OptionVariables, OptionArgs, [],
            (pred(_::in, Succeeded0::out, X::in, X::out,
                    IO0::di, IO::uo) is det :-
                Compile(Succeeded0, IO0, IO)
            ), Succeeded, unit, _, !IO)
    ).

%-----------------------------------------------------------------------------%

    % Return a closure which will work out what the target files
    % are for a module, so recompilation_check.m can check that
    % they are up-to-date which deciding whether compilation is
    % necessary.
    % Note that `--smart-recompilation' only works with
    % `--target-code-only', which is always set when the
    % compiler is invoked by mmake. Using smart recompilation
    % without using mmake is not a sensible thing to do.
    % handle_options.m will disable smart recompilation if
    % `--target-code-only' is not set.
:- pred find_smart_recompilation_target_files(module_name::in, globals::in,
    find_target_file_names::out(find_target_file_names)) is det.

find_smart_recompilation_target_files(TopLevelModuleName,
        Globals, FindTargetFiles) :-
    globals__get_target(Globals, CompilationTarget),
    (
        CompilationTarget = c,
        globals__lookup_bool_option(Globals, split_c_files, yes)
    ->
        FindTargetFiles = split_c_find_target_files
    ;
        ( CompilationTarget = c, TargetSuffix = ".c"
        ; CompilationTarget = il, TargetSuffix = ".il"
        ; CompilationTarget = java, TargetSuffix = ".java"
        ; CompilationTarget = asm, TargetSuffix = ".s"
        ),
        FindTargetFiles = usual_find_target_files(CompilationTarget,
            TargetSuffix, TopLevelModuleName)
    ).

:- pred split_c_find_target_files(module_name::in, list(file_name)::out,
    io::di, io::uo) is det.

split_c_find_target_files(ModuleName, [FileName], !IO) :-
    globals__io_lookup_string_option(object_file_extension, Obj, !IO),
    % We don't know how many chunks there should be, so just check
    % the first.
    module_name_to_split_c_file_name(ModuleName, 0, Obj, FileName, !IO).

:- pred usual_find_target_files(compilation_target::in, string::in,
    module_name::in, module_name::in, list(file_name)::out,
    io::di, io::uo) is det.

usual_find_target_files(CompilationTarget, TargetSuffix, TopLevelModuleName,
        ModuleName, TargetFiles, !IO) :-
    % XXX Should we check the generated header files?
    (
        CompilationTarget = asm,
        ModuleName \= TopLevelModuleName
    ->
        % With `--target asm' all the nested
        % sub-modules are placed in the `.s' file
        % of the top-level module.
        TargetFiles = []
    ;
        module_name_to_file_name(ModuleName, TargetSuffix, yes, FileName, !IO),
        TargetFiles = [FileName]
    ).

:- pred find_timestamp_files(module_name::in, globals::in,
    find_timestamp_file_names::out(find_timestamp_file_names)) is det.

find_timestamp_files(TopLevelModuleName, Globals, FindTimestampFiles) :-
    globals__lookup_bool_option(Globals, pic, Pic),
    globals__get_target(Globals, CompilationTarget),
    (
        CompilationTarget = c,
        TimestampSuffix = ".c_date"
    ;
        CompilationTarget = il,
        TimestampSuffix = ".il_date"
    ;
        CompilationTarget = java,
        TimestampSuffix = ".java_date"
    ;
        CompilationTarget = asm,
        TimestampSuffix = (Pic = yes -> ".pic_s_date" ; ".s_date")
    ),
    FindTimestampFiles = find_timestamp_files_2(CompilationTarget,
        TimestampSuffix, TopLevelModuleName).

:- pred find_timestamp_files_2(compilation_target::in, string::in,
    module_name::in, module_name::in, list(file_name)::out,
    io::di, io::uo) is det.

find_timestamp_files_2(CompilationTarget, TimestampSuffix,
        TopLevelModuleName, ModuleName, TimestampFiles, !IO) :-
    (
        CompilationTarget = asm,
        ModuleName \= TopLevelModuleName
    ->
        % With `--target asm' all the nested
        % sub-modules are placed in the `.s' file
        % of the top-level module.
        TimestampFiles = []
    ;
        module_name_to_file_name(ModuleName, TimestampSuffix, yes, FileName,
            !IO),
        TimestampFiles = [FileName]
    ).

%-----------------------------------------------------------------------------%

    % Given a fully expanded module (i.e. a module name and a list
    % of all the items in the module and any of its imports),
    % compile it.

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

:- pred compile(file_name::in, module_name::in, list(module_name)::in,
    maybe(timestamp)::in, read_modules::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    pair(module_name, item_list)::in, list(string)::out,
    io::di, io::uo) is det.

compile(SourceFileName, SourceFileModuleName, NestedSubModules0,
        MaybeTimestamp, ReadModules, FindTimestampFiles,
        ModuleName - Items, FactTableObjFiles, !IO) :-
    check_for_no_exports(Items, ModuleName, !IO),
    ( ModuleName = SourceFileModuleName ->
        NestedSubModules = NestedSubModules0
    ;
        NestedSubModules = []
    ),
    grab_imported_modules(SourceFileName, SourceFileModuleName, ModuleName,
        NestedSubModules, ReadModules, MaybeTimestamp, Items, Module, Error2,
        !IO),
    ( Error2 \= fatal_module_errors ->
        mercury_compile(Module, NestedSubModules, FindTimestampFiles,
            FactTableObjFiles, no_prev_dump, _, !IO)
    ;
        FactTableObjFiles = []
    ).

:- pred mercury_compile(module_imports::in, list(module_name)::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    list(string)::out, dump_info::in, dump_info::out, io::di, io::uo) is det.

mercury_compile(Module, NestedSubModules, FindTimestampFiles,
        FactTableObjFiles, !DumpInfo, !IO) :-
    module_imports_get_module_name(Module, ModuleName),
    % If we are only typechecking or error checking, then we should not
    % modify any files, this includes writing to .d files.
    globals__io_lookup_bool_option(typecheck_only, TypeCheckOnly, !IO),
    globals__io_lookup_bool_option(errorcheck_only, ErrorCheckOnly, !IO),
    bool__or(TypeCheckOnly, ErrorCheckOnly, DontWriteDFile),
    pre_hlds_pass(Module, DontWriteDFile, HLDS1, QualInfo, MaybeTimestamps,
        UndefTypes, UndefModes, Errors1, !DumpInfo, !IO),
    frontend_pass(QualInfo, UndefTypes, UndefModes, Errors1, Errors2,
        HLDS1, HLDS20, !DumpInfo, !IO),
    (
        Errors1 = no,
        Errors2 = no
    ->
        globals__io_lookup_bool_option(verbose, Verbose, !IO),
        globals__io_lookup_bool_option(statistics, Stats, !IO),
        maybe_write_dependency_graph(Verbose, Stats, HLDS20, HLDS21, !IO),
        maybe_generate_schemas(HLDS21, Verbose, Stats, !IO),
        globals__io_lookup_bool_option(make_optimization_interface,
            MakeOptInt, !IO),
        globals__io_lookup_bool_option(make_transitive_opt_interface,
            MakeTransOptInt, !IO),
        ( TypeCheckOnly = yes ->
            FactTableObjFiles = []
        ; ErrorCheckOnly = yes ->
            % we may still want to run `unused_args' so that we get
            % the appropriate warnings
            globals__io_lookup_bool_option(warn_unused_args, UnusedArgs, !IO),
            ( UnusedArgs = yes ->
                globals__io_set_option(optimize_unused_args, bool(no), !IO),
                maybe_unused_args(Verbose, Stats, HLDS21, HLDS22, !IO)
            ;
                HLDS22 = HLDS21
            ),
            % magic sets can report errors.
            maybe_transform_dnf(Verbose, Stats, HLDS22, HLDS23, !IO),
            maybe_magic(Verbose, Stats, HLDS23, _, !IO),
            FactTableObjFiles = []
        ; MakeOptInt = yes ->
            % only run up to typechecking when making the .opt file
            FactTableObjFiles = []
        ; MakeTransOptInt = yes ->
            output_trans_opt_file(HLDS21, !DumpInfo, !IO),
            FactTableObjFiles = []
        ;
            mercury_compile_after_front_end(NestedSubModules,
                FindTimestampFiles, MaybeTimestamps, ModuleName, HLDS21,
                FactTableObjFiles, !DumpInfo, !IO)
        )
    ;
        % If the number of errors is > 0, make sure that the compiler
        % exits with a non-zero exit status.
        io__get_exit_status(ExitStatus, !IO),
        ( ExitStatus = 0 ->
            io__set_exit_status(1, !IO)
        ;
            true
        ),
        FactTableObjFiles = []
    ).

:- pred mercury_compile_after_front_end(list(module_name)::in,
    find_timestamp_file_names::in(find_timestamp_file_names),
    maybe(module_timestamps)::in, module_name::in, module_info::in,
    list(string)::out, dump_info::in, dump_info::out, io::di, io::uo) is det.

mercury_compile_after_front_end(NestedSubModules, FindTimestampFiles,
        MaybeTimestamps, ModuleName, HLDS21, FactTableBaseFiles, !DumpInfo,
        !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),
    maybe_output_prof_call_graph(Verbose, Stats,
        HLDS21, HLDS25, !IO),
    middle_pass(ModuleName, HLDS25, HLDS50, !DumpInfo, !IO),
    globals__io_lookup_bool_option(highlevel_code, HighLevelCode, !IO),
    globals__io_lookup_bool_option(aditi_only, AditiOnly, !IO),
    globals__io_get_target(Target, !IO),
    globals__io_lookup_bool_option(target_code_only, TargetCodeOnly, !IO),

    %
    % Remove any existing `.used' file before writing the
    % output file file. This avoids leaving the old `used'
    % file lying around if compilation is interrupted after
    % the new output file is written but before the new
    % `.used' file is written.
    %
    module_name_to_file_name(ModuleName, ".used", no, UsageFileName, !IO),
    io__remove_file(UsageFileName, _, !IO),

    % magic sets can report errors.
    module_info_num_errors(HLDS50, NumErrors),
    ( NumErrors = 0 ->
        globals__io_lookup_bool_option(intermodule_analysis, IntermodAnalysis,
            !IO),
        (
            IntermodAnalysis = yes,
            module_info_analysis_info(HLDS50, AnalysisInfo),
            analysis__write_analysis_files(
                module_name_to_module_id(ModuleName), AnalysisInfo, !IO)
        ;
            IntermodAnalysis = no
        ),
        maybe_generate_rl_bytecode(Verbose, MaybeRLFile, HLDS50, HLDS51, !IO),
        (
            ( Target = c
            ; Target = asm
            )
        ->
            %
            % Produce the grade independent header file
            % <module>.mh containing function prototypes
            % for the `:- pragma export'ed procedures.
            %
            export__get_foreign_export_decls(HLDS50, ExportDecls),
            export__produce_header_file(ExportDecls, ModuleName, !IO)
        ;
            true
        ),
        ( AditiOnly = yes ->
            HLDS = HLDS51,
            FactTableBaseFiles = []
        ; Target = il ->
            HLDS = HLDS51,
            mlds_backend(HLDS, _, MLDS, !DumpInfo, !IO),
            (
                TargetCodeOnly = yes,
                mlds_to_il_assembler(MLDS, !IO)
            ;
                TargetCodeOnly = no,
                HasMain = mlds_has_main(MLDS),
                mlds_to_il_assembler(MLDS, !IO),
                io__output_stream(OutputStream, !IO),
                compile_target_code__il_assemble(OutputStream, ModuleName,
                    HasMain, Succeeded, !IO),
                maybe_set_exit_status(Succeeded, !IO)
            ),
            FactTableBaseFiles = []
        ; Target = java ->
            HLDS = HLDS51,
            mlds_backend(HLDS, _, MLDS, !DumpInfo, !IO),
            mlds_to_java(MLDS, !IO),
            (
                TargetCodeOnly = yes
            ;
                TargetCodeOnly = no,
                io__output_stream(OutputStream, !IO),
                module_name_to_file_name(ModuleName, ".java", no, JavaFile,
                    !IO),
                compile_target_code__compile_java_file(OutputStream, JavaFile,
                    Succeeded, !IO),
                maybe_set_exit_status(Succeeded, !IO)
            ),
            FactTableBaseFiles = []
        ; Target = asm ->
            % compile directly to assembler using the gcc back-end
            HLDS = HLDS51,
            mlds_backend(HLDS, _, MLDS, !DumpInfo, !IO),
            maybe_mlds_to_gcc(MLDS, MaybeRLFile, ContainsCCode, !IO),
            (
                TargetCodeOnly = yes
            ;
                TargetCodeOnly = no,
                % We don't invoke the assembler to produce an
                % object file yet -- that is done at
                % the top level.
                %
                % But if the module contained `pragma c_code',
                % then we will have compiled that to a
                % separate C file.  We need to invoke the
                % C compiler on that.
                %
                (
                    ContainsCCode = yes,
                    mercury_compile_asm_c_code(ModuleName, !IO)
                ;
                    ContainsCCode = no
                )
            ),
            FactTableBaseFiles = []
        ; HighLevelCode = yes ->
            HLDS = HLDS51,
            mlds_backend(HLDS, _, MLDS, !DumpInfo, !IO),
            mlds_to_high_level_c(MLDS, MaybeRLFile, !IO),
            (
                TargetCodeOnly = yes
            ;
                TargetCodeOnly = no,
                module_name_to_file_name(ModuleName, ".c", no, C_File, !IO),
                get_linked_target_type(TargetType, !IO),
                get_object_code_type(TargetType, PIC, !IO),
                maybe_pic_object_file_extension(PIC, Obj, !IO),
                module_name_to_file_name(ModuleName, Obj, yes, O_File, !IO),
                io__output_stream(OutputStream, !IO),
                compile_target_code__compile_c_file(OutputStream, PIC, C_File,
                    O_File, CompileOK, !IO),
                maybe_set_exit_status(CompileOK, !IO)
            ),
            FactTableBaseFiles = []
        ;
            backend_pass(HLDS51, HLDS, GlobalData, LLDS, !DumpInfo, !IO),
            output_pass(HLDS, GlobalData, LLDS, MaybeRLFile, ModuleName,
                _CompileErrors, FactTableBaseFiles, !IO)
        ),
        recompilation__usage__write_usage_file(HLDS, NestedSubModules,
            MaybeTimestamps, !IO),
        FindTimestampFiles(ModuleName, TimestampFiles, !IO),
        list__foldl(touch_datestamp, TimestampFiles, !IO)
    ;
        % If the number of errors is > 0, make sure that the compiler
        % exits with a non-zero exit status.
        io__get_exit_status(ExitStatus, !IO),
        ( ExitStatus = 0 ->
            io__set_exit_status(1, !IO)
        ;
            true
        ),
        FactTableBaseFiles = []
    ).

:- pred mercury_compile_asm_c_code(module_name::in, io::di, io::uo) is det.

mercury_compile_asm_c_code(ModuleName, !IO) :-
    get_linked_target_type(TargetType, !IO),
    get_object_code_type(TargetType, PIC, !IO),
    maybe_pic_object_file_extension(PIC, Obj, !IO),
    module_name_to_file_name(ModuleName, ".c", no, CCode_C_File, !IO),
    ForeignModuleName = foreign_language_module_name(ModuleName, c),
    module_name_to_file_name(ForeignModuleName, Obj, yes, CCode_O_File, !IO),
    io__output_stream(OutputStream, !IO),
    compile_target_code__compile_c_file(OutputStream, PIC,
        CCode_C_File, CCode_O_File, CompileOK, !IO),
    maybe_set_exit_status(CompileOK, !IO),
    % add this object file to the list
    % of extra object files to link in
    globals__io_lookup_accumulating_option(link_objects, LinkObjects, !IO),
    globals__io_set_option(link_objects,
        accumulating([CCode_O_File | LinkObjects]), !IO).

    % return `yes' iff this module defines the main/2 entry point.
:- func mlds_has_main(mlds) = has_main.

mlds_has_main(MLDS) =
    (
        MLDS = mlds(_, _, _, Defns, _, _),
        defns_contain_main(Defns)
    ->
        has_main
    ;
        no_main
    ).

:- pred get_linked_target_type(linked_target_type::out, io::di, io::uo) is det.

get_linked_target_type(LinkedTargetType, !IO) :-
    globals__io_lookup_bool_option(compile_to_shared_lib, MakeSharedLib, !IO),
    ( MakeSharedLib = yes ->
        LinkedTargetType = shared_library
    ;
        LinkedTargetType = executable
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred pre_hlds_pass(module_imports::in, bool::in, module_info::out,
    make_hlds_qual_info::out, maybe(module_timestamps)::out,
    bool::out, bool::out, bool::out, dump_info::in, dump_info::out,
    io::di, io::uo) is det.

pre_hlds_pass(ModuleImports0, DontWriteDFile0, HLDS1, QualInfo,
        MaybeTimestamps, UndefTypes, UndefModes, FoundError, !DumpInfo, !IO) :-
    globals__io_get_globals(Globals, !IO),
    globals__lookup_bool_option(Globals, statistics, Stats),
    globals__lookup_bool_option(Globals, verbose, Verbose),
    globals__lookup_bool_option(Globals, invoked_by_mmc_make, MMCMake),
    DontWriteDFile1 = DontWriteDFile0 `or` MMCMake,

    % Don't write the `.d' file when making the `.opt' file because
    % we can't work out the full transitive implementation dependencies.
    globals__lookup_bool_option(Globals, make_optimization_interface,
        MakeOptInt),
    DontWriteDFile = DontWriteDFile1 `or` MakeOptInt,

    module_imports_get_module_name(ModuleImports0, Module),
    (
        DontWriteDFile = yes,
        % The only time the TransOptDeps are required is when
        % creating the .trans_opt file.  If DontWriteDFile is yes,
        % then error check only or type-check only is enabled, so
        % we cant be creating the .trans_opt file.
        MaybeTransOptDeps = no
    ;
        DontWriteDFile = no,
        maybe_read_dependency_file(Module, MaybeTransOptDeps, !IO)
    ),

    % Errors in .opt and .trans_opt files result in software errors.
    maybe_grab_optfiles(ModuleImports0, Verbose, MaybeTransOptDeps,
        ModuleImports1, IntermodError, !IO),

    module_imports_get_items(ModuleImports1, Items1),
    MaybeTimestamps = ModuleImports1 ^ maybe_timestamps,

    module_qualify_items(Items1, Items2, Module, Verbose, Stats, MQInfo0, _,
        UndefTypes0, UndefModes0, !IO),

    mq_info_get_recompilation_info(MQInfo0, RecompInfo0),
    expand_equiv_types(Module, Items2, Verbose, Stats, Items, CircularTypes,
        EqvMap, RecompInfo0, RecompInfo, !IO),
    mq_info_set_recompilation_info(RecompInfo, MQInfo0, MQInfo),
    bool__or(UndefTypes0, CircularTypes, UndefTypes1),

    make_hlds(Module, Items, MQInfo, EqvMap, Verbose, Stats, HLDS0, QualInfo,
        UndefTypes2, UndefModes2, FoundError, !IO),

    bool__or(UndefTypes1, UndefTypes2, UndefTypes),
    bool__or(UndefModes0, UndefModes2, UndefModes),

    maybe_dump_hlds(HLDS0, 1, "initial", !DumpInfo, !IO),

    (
        DontWriteDFile = yes
    ;
        DontWriteDFile = no,
        module_info_get_all_deps(HLDS0, AllDeps),
        write_dependency_file(ModuleImports0, AllDeps, MaybeTransOptDeps, !IO),
        globals__lookup_bool_option(Globals,
            generate_mmc_make_module_dependencies, OutputMMCMakeDeps),
        (
            OutputMMCMakeDeps = yes,
            make__write_module_dep_file(ModuleImports0, !IO)
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

:- pred module_qualify_items(item_list::in, item_list::out, module_name::in,
    bool::in, bool::in, mq_info::out, int::out, bool::out, bool::out,
    io::di, io::uo) is det.

module_qualify_items(Items0, Items, ModuleName, Verbose, Stats, MQInfo,
        NumErrors, UndefTypes, UndefModes, !IO) :-
    maybe_write_string(Verbose, "% Module qualifying items...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    module_qual__module_qualify_items(Items0, Items, ModuleName, yes,
        MQInfo, NumErrors, UndefTypes, UndefModes, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred maybe_grab_optfiles(module_imports::in, bool::in,
    maybe(list(module_name))::in, module_imports::out, bool::out,
    io::di, io::uo) is det.

maybe_grab_optfiles(Imports0, Verbose, MaybeTransOptDeps, Imports, Error,
        !IO) :-
    globals__io_get_globals(Globals, !IO),
    globals__lookup_bool_option(Globals, intermodule_optimization,
        IntermodOpt),
    globals__lookup_bool_option(Globals, use_opt_files, UseOptInt),
    globals__lookup_bool_option(Globals, make_optimization_interface,
        MakeOptInt),
    globals__lookup_bool_option(Globals, transitive_optimization, TransOpt),
    globals__lookup_bool_option(Globals, make_transitive_opt_interface,
        MakeTransOptInt),
    (
        ( UseOptInt = yes
        ; IntermodOpt = yes
        ),
        MakeOptInt = no
    ->
        maybe_write_string(Verbose, "% Reading .opt files...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        intermod__grab_optfiles(Imports0, Imports1, Error1, !IO),
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
            trans_opt__grab_optfiles(TransOptDeps, Imports1, Imports, Error2,
                !IO)
        ;
            MaybeTransOptDeps = no,
            Imports = Imports1,
            Error2 = no,
            module_imports_get_module_name(Imports, ModuleName),
            globals__lookup_bool_option(Globals, warn_missing_trans_opt_deps,
                WarnNoTransOptDeps),
            (
                WarnNoTransOptDeps = yes,
                sym_name_to_string(ModuleName, ModuleString),
                Msg1 = "Warning: cannot read trans-opt dependencies " ++
                    "for module `" ++ ModuleString ++ "'.",
                Msg2 = "You need to remake the dependencies.",
                write_error_pieces_plain([words(Msg1), nl, words(Msg2)], !IO),
                record_warning(!IO)
            ;
                WarnNoTransOptDeps = no
            )
        )
    ; MakeOptInt = yes ->
        % If we're making the `.opt' file, then we can't
        % read any `.trans_opt' files, since `.opt' files
        % aren't allowed to depend on `.trans_opt' files.
        Imports = Imports1,
        Error2 = no
    ;
        (
            TransOpt = yes,
            % If transitive optimization is enabled, but we are
            % not creating the .opt or .trans opt file, then import
            % the trans_opt files for all the modules that are
            % imported (or used), and for all ancestor modules.
            list__condense([Imports0 ^ parent_deps,
                Imports0 ^ int_deps, Imports0 ^ impl_deps], TransOptFiles),
            trans_opt__grab_optfiles(TransOptFiles, Imports1, Imports, Error2,
                !IO)
        ;
            TransOpt = no,
            Imports = Imports1,
            Error2 = no
        )
    ),
    bool__or(Error1, Error2, Error).

:- pred expand_equiv_types(module_name::in, item_list::in, bool::in, bool::in,
    item_list::out, bool::out, eqv_map::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    io::di, io::uo) is det.

expand_equiv_types(ModuleName, Items0, Verbose, Stats, Items, CircularTypes,
    EqvMap, RecompInfo0, RecompInfo, !IO) :-
    maybe_write_string(Verbose, "% Expanding equivalence types...", !IO),
    maybe_flush_output(Verbose, !IO),
    equiv_type__expand_eqv_types(ModuleName, Items0, Items, CircularTypes,
        EqvMap, RecompInfo0, RecompInfo, !IO),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred make_hlds(module_name::in, item_list::in, mq_info::in,
    eqv_map::in, bool::in, bool::in, module_info::out,
    make_hlds_qual_info::out, bool::out, bool::out, bool::out, io::di, io::uo)
    is det.

make_hlds(Module, Items, MQInfo, EqvMap, Verbose, Stats, HLDS, QualInfo,
        UndefTypes, UndefModes, FoundSemanticError, !IO) :-
    maybe_write_string(Verbose, "% Converting parse tree to hlds...\n", !IO),
    Prog = module(Module, Items),
    parse_tree_to_hlds(Prog, MQInfo, EqvMap, HLDS, QualInfo,
        UndefTypes, UndefModes, !IO),
    module_info_num_errors(HLDS, NumErrors),
    io__get_exit_status(Status, !IO),
    (
        ( Status \= 0
        ; NumErrors > 0
        )
    ->
        FoundSemanticError = yes,
        io__set_exit_status(1, !IO)
    ;
        FoundSemanticError = no
    ),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred frontend_pass(make_hlds_qual_info::in, bool::in, bool::in,
    bool::in, bool::out, module_info::in, module_info::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

frontend_pass(QualInfo0, FoundUndefTypeError, FoundUndefModeError, !FoundError,
        !HLDS, !DumpInfo, !IO) :-
    % We can't continue after an undefined type error, since typecheck
    % would get internal errors.
    globals__io_get_globals(Globals, !IO),
    globals__lookup_bool_option(Globals, verbose, Verbose),
    (
        FoundUndefTypeError = yes,
        !:FoundError = yes,
        maybe_write_string(Verbose,
            "% Program contains undefined type error(s).\n", !IO),
        io__set_exit_status(1, !IO)
    ;
        FoundUndefTypeError = no,
        maybe_write_string(Verbose, "% Checking typeclasses...\n", !IO),
        check_typeclass__check_typeclasses(QualInfo0, QualInfo, !HLDS,
            FoundTypeclassError, !IO),
        maybe_dump_hlds(!.HLDS, 5, "typeclass", !DumpInfo, !IO),
        set_module_recomp_info(QualInfo, !HLDS),

        % We can't continue after a typeclass error, since typecheck
        % can get internal errors.
        (
            FoundTypeclassError = yes,
            !:FoundError = yes
        ;
            FoundTypeclassError = no,
            frontend_pass_no_type_error(FoundUndefModeError, !FoundError,
                !HLDS, !DumpInfo, !IO)
        )
    ).

:- pred frontend_pass_no_type_error(bool::in, bool::in, bool::out,
    module_info::in, module_info::out, dump_info::in, dump_info::out,
    io::di, io::uo) is det.

frontend_pass_no_type_error(FoundUndefModeError, !FoundError, !HLDS, !DumpInfo,
        !IO) :-
    globals__io_get_globals(Globals, !IO),
    globals__lookup_bool_option(Globals, verbose, Verbose),
    globals__lookup_bool_option(Globals, statistics, Stats),
    globals__lookup_bool_option(Globals, intermodule_optimization, Intermod),
    globals__lookup_bool_option(Globals, use_opt_files, UseOptFiles),
    globals__lookup_bool_option(Globals, make_optimization_interface,
        MakeOptInt),
    (
        ( Intermod = yes
        ; UseOptFiles = yes
        ),
        MakeOptInt = no
    ->
        % Eliminate unnecessary clauses from `.opt' files,
        % to speed up compilation. This must be done after
        % typeclass instances have been checked, since that
        % fills in which pred_ids are needed by instance decls.
        maybe_write_string(Verbose, "% Eliminating dead predicates... ", !IO),
        dead_pred_elim(!HLDS),
        maybe_write_string(Verbose, "done.\n", !IO),
        maybe_dump_hlds(!.HLDS, 10, "dead_pred_elim", !DumpInfo, !IO)
    ;
        true
    ),

    %
    % Next typecheck the clauses.
    %
    maybe_write_string(Verbose, "% Type-checking...\n", !IO),
    typecheck(!HLDS, FoundTypeError, ExceededTypeCheckIterationLimit, !IO),
    (
        FoundTypeError = yes,
        maybe_write_string(Verbose,
            "% Program contains type error(s).\n", !IO),
        io__set_exit_status(1, !IO)
    ;
        FoundTypeError = no,
        maybe_write_string(Verbose, "% Program is type-correct.\n", !IO)
    ),
    maybe_dump_hlds(!.HLDS, 15, "typecheck", !DumpInfo, !IO),

    % We can't continue after an undefined inst/mode error, since
    % propagate_types_into_proc_modes (in post_typecheck.m -- called by
    % purity.m) and mode analysis would get internal errors. Also mode analysis
    % can loop if there are cyclic insts/modes.
    %
    % We can't continue if the type inference iteration limit was exceeeded
    % because the code to resolve overloading in post_typecheck.m (called by
    % purity.m) could abort.
    ( FoundUndefModeError = yes ->
        !:FoundError = yes,
        maybe_write_string(Verbose,
            "% Program contains undefined inst " ++
            "or undefined mode error(s).\n", !IO),
        io__set_exit_status(1, !IO)
    ; ExceededTypeCheckIterationLimit = yes ->
        % FoundTypeError will always be true here, so we've already
        % printed a message about the program containing type errors.
        !:FoundError = yes,
        io__set_exit_status(1, !IO)
    ;
        puritycheck(Verbose, Stats, !HLDS, FoundTypeError,
            FoundPostTypecheckError, !IO),
        maybe_dump_hlds(!.HLDS, 20, "puritycheck", !DumpInfo, !IO),

        !:FoundError = !.FoundError `or` FoundTypeError,

        %
        % Stop here if `--typecheck-only' was specified.
        %
        globals__lookup_bool_option(Globals, typecheck_only, TypecheckOnly),
        (
            TypecheckOnly = yes
        ->
            true
        ;
            ( FoundTypeError = yes
            ; FoundPostTypecheckError = yes
            )
        ->
            % XXX It would be nice if we could go on and mode-check the
            % predicates which didn't have type errors, but we need to run
            % polymorphism before running mode analysis, and currently
            % polymorphism may get internal errors if any of the predicates
            % are not type-correct.

            !:FoundError = yes
        ;
            % Only write out the `.opt' file if there are no errors.
            (
                !.FoundError = no,
                FoundUndefModeError = no
            ->
                maybe_write_optfile(MakeOptInt, !HLDS, !DumpInfo, !IO)
            ;
                true
            ),
            % If our job was to write out the `.opt' file, then we're done.
            (
                MakeOptInt = yes
            ;
                MakeOptInt = no,
                % Now go ahead and do the rest of mode checking
                % and determinism analysis.
                frontend_pass_by_phases(!HLDS, FoundModeOrDetError, !DumpInfo,
                    !IO),
                !:FoundError = !.FoundError `or` FoundModeOrDetError
            )
        )
    ).

:- pred maybe_write_optfile(bool::in, module_info::in, module_info::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

maybe_write_optfile(MakeOptInt, !HLDS, !DumpInfo, !IO) :-
    globals__io_get_globals(Globals, !IO),
    globals__lookup_bool_option(Globals, intermodule_optimization, Intermod),
    globals__lookup_bool_option(Globals, intermod_unused_args, IntermodArgs),
    globals__lookup_accumulating_option(Globals, intermod_directories,
        IntermodDirs),
    globals__lookup_bool_option(Globals, use_opt_files, UseOptFiles),
    globals__lookup_bool_option(Globals, verbose, Verbose),
    globals__lookup_bool_option(Globals, statistics, Stats),
    globals__lookup_bool_option(Globals, termination, Termination),
    globals__lookup_bool_option(Globals, termination2, Termination2),
    globals__lookup_bool_option(Globals, analyse_exceptions,
        ExceptionAnalysis),
    globals__lookup_bool_option(Globals, analyse_closures,
        ClosureAnalysis),
    (
        MakeOptInt = yes,
        intermod__write_optfile(!HLDS, !IO),

        % If intermod_unused_args is being performed, run polymorphism,
        % mode analysis and determinism analysis, then run unused_args
        % to append the unused argument information to the `.opt.tmp'
        % file written above.
        (
            ( IntermodArgs = yes
            ; Termination = yes
            ; Termination2 = yes
            ; ExceptionAnalysis = yes
            )
        ->
            frontend_pass_by_phases(!HLDS, FoundModeError, !DumpInfo, !IO),
            ( FoundModeError = no ->
                (
                    % Closure analysis assumes that lambda expressions have
                    % been converted into separate predicates.
                    ClosureAnalysis = yes,
                    mercury_compile.process_lambdas(Verbose, Stats,
                        !HLDS, !IO),
                    mercury_compile.maybe_closure_analysis(Verbose, Stats,
                        !HLDS, !IO)
                ;
                    ClosureAnalysis = no
                ),
                (
                    ExceptionAnalysis = yes,
                    maybe_exception_analysis(Verbose, Stats, !HLDS, !IO)
                ;
                    ExceptionAnalysis = no
                ),
                (
                    IntermodArgs = yes,
                    maybe_unused_args(Verbose, Stats, !HLDS, !IO)
                ;
                    IntermodArgs = no
                ),
                (
                    Termination = yes,
                    maybe_termination(Verbose, Stats, !HLDS, !IO)
                ;
                    Termination = no
                ),
                (
                    Termination2 = yes,
                    maybe_termination2(Verbose, Stats, !HLDS, !IO)
                ;
                    Termination2 = no
                )
            ;
                io__set_exit_status(1, !IO)
            )
        ;
            true
        ),
        module_info_name(!.HLDS, ModuleName),
        module_name_to_file_name(ModuleName, ".opt", yes, OptName, !IO),
        update_interface(OptName, !IO),
        touch_interface_datestamp(ModuleName, ".optdate", !IO)
    ;
        MakeOptInt = no,
        % If there is a `.opt' file for this module the import
        % status of items in the `.opt' file needs to be updated.
        ( Intermod = yes ->
            UpdateStatus = yes
        ; UseOptFiles = yes ->
            module_info_name(!.HLDS, ModuleName),
            module_name_to_search_file_name(ModuleName, ".opt", OptName, !IO),
            search_for_file(IntermodDirs, OptName, Found, !IO),
            ( Found = ok(_) ->
                UpdateStatus = yes,
                io__seen(!IO)
            ;
                UpdateStatus = no
            )
        ;
            UpdateStatus = no
        ),
        (
            UpdateStatus = yes,
            intermod__adjust_pred_import_status(!HLDS, !IO)
        ;
            UpdateStatus = no
        )
    ).

:- pred output_trans_opt_file(module_info::in, dump_info::in, dump_info::out,
    io::di, io::uo) is det.

output_trans_opt_file(!.HLDS, !DumpInfo, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),
    globals__io_lookup_bool_option(analyse_closures, ClosureAnalysis, !IO),
    %
    % Closure analysis assumes that lambda expressions have
    % been converted into separate predicates.
    %
    (
        ClosureAnalysis = yes,
        process_lambdas(Verbose, Stats, !HLDS, !IO)
    ;
        ClosureAnalysis = no
    ),
    maybe_dump_hlds(!.HLDS, 110, "lambda", !DumpInfo, !IO),
    maybe_closure_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 117, "closure_analysis", !DumpInfo, !IO),
    maybe_exception_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 118, "exception_analysis", !DumpInfo, !IO),
    maybe_termination(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 120, "termination", !DumpInfo, !IO),
    maybe_termination2(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 121, "termination_2", !DumpInfo, !IO),
    trans_opt__write_optfile(!.HLDS, !IO).

:- pred frontend_pass_by_phases(module_info::in, module_info::out,
    bool::out, dump_info::in, dump_info::out, io::di, io::uo) is det.

frontend_pass_by_phases(!HLDS, FoundError, !DumpInfo, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),

    maybe_polymorphism(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 30, "polymorphism", !DumpInfo, !IO),

    maybe_mode_constraints(Verbose, Stats, !.HLDS, HHF_HLDS, !IO),
    maybe_dump_hlds(HHF_HLDS, 33, "mode_constraints", !DumpInfo, !IO),

    modecheck(Verbose, Stats, !HLDS, FoundModeError, UnsafeToContinue, !IO),
    maybe_dump_hlds(!.HLDS, 35, "modecheck", !DumpInfo, !IO),

    (
        UnsafeToContinue = yes,
        FoundError = yes
    ;
        UnsafeToContinue = no,
        detect_switches(Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(!.HLDS, 40, "switch_detect", !DumpInfo, !IO),

        detect_cse(Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(!.HLDS, 45, "cse", !DumpInfo, !IO),

        check_determinism(Verbose, Stats, !HLDS, FoundDetError, !IO),
        maybe_dump_hlds(!.HLDS, 50, "determinism", !DumpInfo, !IO),

        check_unique_modes(Verbose, Stats, !HLDS, FoundUniqError, !IO),
        maybe_dump_hlds(!.HLDS, 55, "unique_modes", !DumpInfo, !IO),

        check_stratification(Verbose, Stats, !HLDS, FoundStratError, !IO),
        maybe_dump_hlds(!.HLDS, 60, "stratification", !DumpInfo, !IO),

        simplify(yes, frontend, Verbose, Stats, process_all_nonimported_procs,
            !HLDS, !IO),
        maybe_dump_hlds(!.HLDS, 65, "frontend_simplify", !DumpInfo, !IO),

        % Work out whether we encountered any errors.
        io__get_exit_status(ExitStatus, !IO),
        (
            FoundModeError = no,
            FoundDetError = no,
            FoundUniqError = no,
            FoundStratError = no,
            % Strictly speaking, we shouldn't need to check the exit status.
            % But the values returned for FoundModeError etc. aren't always
            % correct.
            ExitStatus = 0
        ->
            FoundError = no
        ;
            FoundError = yes
        )
    ),
    maybe_dump_hlds(!.HLDS, 99, "front_end", !DumpInfo, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred middle_pass(module_name::in, module_info::in, module_info::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

middle_pass(ModuleName, !HLDS, !DumpInfo, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),

    maybe_read_experimental_complexity_file(!HLDS, !IO),

    tabling(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 105, "tabling", !DumpInfo, !IO),

    process_lambdas(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 110, "lambda", !DumpInfo, !IO),

    expand_equiv_types_hlds(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 115, "equiv_types", !DumpInfo, !IO),
    
    maybe_closure_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 117, "closure_analysis", !DumpInfo, !IO),

    % Uncomment the following code to check that unique mode analysis works
    % after simplification has been run. Currently it does not because common.m
    % does not preserve unique mode correctness (this test fails on about
    % five modules in the compiler and library). It is important that unique
    % mode analysis work most of the time after optimizations because
    % deforestation reruns it.
    %

    % check_unique_modes(Verbose, Stats, !HLDS,
    %   FoundUniqError, !IO),
    % ( FoundUniqError = yes ->
    %   error("unique modes failed")
    % ;
    %   true
    % ),
    
    % Exception analysis and termination analysis need to come before any
    % optimization passes that could benefit from the information that
    % they provide.
    %   
    maybe_exception_analysis(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 118, "exception_analysis", !DumpInfo, !IO),

    maybe_termination(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 120, "termination", !DumpInfo, !IO),

    maybe_termination2(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 121, "termination2", !DumpInfo, !IO),
    
    maybe_type_ctor_infos(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 125, "type_ctor_infos", !DumpInfo, !IO),

    % warn_dead_procs must come after type_ctor_infos, so that it handles
    % unification & comparison procedures correctly, but it must also come
    % before optimizations such as higher-order specialization and inlining,
    % which can make the original code for a procedure dead by
    % inlining/specializing all uses of it.
    maybe_warn_dead_procs(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 130, "warn_dead_procs", !DumpInfo, !IO),

    maybe_bytecodes(!.HLDS, ModuleName, Verbose, Stats, !DumpInfo, !IO),
    % stage number 31 is used by maybe_bytecodes

    maybe_untuple_arguments(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 133, "untupling", !DumpInfo, !IO),

    maybe_tuple_arguments(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 134, "tupling", !DumpInfo, !IO),

    maybe_higher_order(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 135, "higher_order", !DumpInfo, !IO),

    maybe_introduce_accumulators(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 140, "accum", !DumpInfo, !IO),

    maybe_do_inlining(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 145, "inlining", !DumpInfo, !IO),

    % Hoisting loop invariants first invokes pass 148, "mark_static".
    % "mark_static" is also run at stage 420.
    %
    maybe_loop_inv(Verbose, Stats, !HLDS, !DumpInfo, !IO),
    maybe_dump_hlds(!.HLDS, 150, "loop_inv", !DumpInfo, !IO),

    maybe_deforestation(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 155, "deforestation", !DumpInfo, !IO),

    maybe_delay_construct(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 160, "delay_construct", !DumpInfo, !IO),

    maybe_unused_args(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 165, "unused_args", !DumpInfo, !IO),

    maybe_unneeded_code(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 170, "unneeded_code", !DumpInfo, !IO),

    maybe_lco(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 175, "lco", !DumpInfo, !IO),

    maybe_transform_aditi_builtins(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 180, "aditi_builtins", !DumpInfo, !IO),

    % DNF transformations should be after inlining.
    maybe_transform_dnf(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 185, "dnf", !DumpInfo, !IO),

    % Magic sets should be the last thing done to Aditi procedures
    % before RL code generation, and must come immediately after DNF.
    % Note that if this pass is done, it will also invokes dead_proc_elim
    % (XXX which means dead_proc_elim may get done twice).
    maybe_magic(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 190, "magic", !DumpInfo, !IO),

    maybe_eliminate_dead_procs(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 195, "dead_procs", !DumpInfo, !IO),
    
    % If we are compiling in a deep profiling grade then now rerun simplify.
    % The reason for doing this now is that we want to take advantage of any
    % opportunities the other optimizations have provided for constant
    % propagation and we cannot do that once the term-size profiling or deep
    % profiling transformations have been applied. 
    %
    simplify(no, pre_prof_transforms, Verbose, Stats,
        process_all_nonimported_procs, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 197, "pre_prof_transform_simplify", !DumpInfo,
        !IO),

    % The term size profiling transformation should be after all
    % transformations that construct terms of non-zero size. (Deep profiling
    % does not construct non-zero size terms.)
    maybe_term_size_prof(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 200, "term_size_prof", !DumpInfo, !IO),

    % Deep profiling transformation should be done late in the piece
    % since it munges the code a fair amount and introduces strange
    % disjunctions that might confuse other hlds->hlds transformations.
    maybe_deep_profiling(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 205, "deep_profiling", !DumpInfo, !IO),

    % Experimental complexity transformation should be done late in the
    % piece for the same reason as deep profiling. At the moment, they are
    % exclusive.
    maybe_experimental_complexity(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 210, "complexity", !DumpInfo, !IO),

    maybe_dump_hlds(!.HLDS, 299, "middle_pass", !DumpInfo, !IO).

%-----------------------------------------------------------------------------%

:- pred maybe_generate_rl_bytecode(bool::in, maybe(rl_file)::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_generate_rl_bytecode(Verbose, MaybeRLFile, !ModuleInfo, !IO) :-
    globals__io_lookup_bool_option(aditi, Aditi, !IO),
    (
        Aditi = yes,
        module_info_get_do_aditi_compilation(!.ModuleInfo, AditiCompile),
        (
            AditiCompile = do_aditi_compilation,

            % Generate the RL procedures.
            maybe_write_string(Verbose, "% Generating RL...\n", !IO),
            maybe_flush_output(Verbose, !IO),
            rl_gen__module(!.ModuleInfo, RLProcs0, !IO),
            maybe_dump_rl(RLProcs0, !.ModuleInfo, "", "", !IO),

            % Optimize the RL procedures.
            rl_opt__procs(!.ModuleInfo, RLProcs0, RLProcs, !IO),
            maybe_dump_rl(RLProcs, !.ModuleInfo, "", ".opt", !IO),

            % Convert the RL procedures to bytecode.
            rl_out__generate_rl_bytecode(RLProcs, MaybeRLFile, !ModuleInfo,
                !IO)
        ;
            AditiCompile = no_aditi_compilation,
            globals__io_lookup_bool_option(aditi_only, AditiOnly, !IO),
            (
                AditiOnly = yes,

                % Always generate a `.rlo' file if compiling
                % with `--aditi-only'.
                RLProcs = [],
                rl_out__generate_rl_bytecode(RLProcs, MaybeRLFile, !ModuleInfo,
                    !IO)
            ;
                AditiOnly = no,
                MaybeRLFile = no
            )
        )
    ;
        Aditi = no,
        MaybeRLFile = no
    ).

:- pred generate_aditi_proc_info(module_info::in, list(rtti_data)::out) is det.

generate_aditi_proc_info(HLDS, AditiProcInfoRttiData) :-
    module_info_aditi_top_down_procs(HLDS, Procs),
    AditiProcInfoRttiData = list__map(
        (func(aditi_top_down_proc(proc(PredId, ProcId), _)) =
            rtti__make_aditi_proc_info(HLDS, PredId, ProcId)
        ), Procs).

%-----------------------------------------------------------------------------%

:- pred backend_pass(module_info::in, module_info::out,
    global_data::out, list(c_procedure)::out, dump_info::in, dump_info::out,
    io::di, io::uo) is det.

backend_pass(!HLDS, GlobalData, LLDS, !DumpInfo, !IO) :-
    module_info_name(!.HLDS, ModuleName),
    globals__io_lookup_bool_option(unboxed_float, UnboxFloat, !IO),
    globals__io_lookup_bool_option(common_data, DoCommonData, !IO),
    StaticCellInfo0 = init_static_cell_info(ModuleName, UnboxFloat,
        DoCommonData),
    global_data_init(StaticCellInfo0, GlobalData0),

    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),

    % map_args_to_regs affects the interface to a predicate,
    % so it must be done in one phase immediately before code generation

    map_args_to_regs(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 305, "args_to_regs", !DumpInfo, !IO),

    globals__io_lookup_bool_option(trad_passes, TradPasses, !IO),
    (
        TradPasses = no,
        backend_pass_by_phases(!HLDS, GlobalData0, GlobalData, LLDS, !DumpInfo,
            !IO)
    ;
        TradPasses = yes,
        backend_pass_by_preds(!HLDS, GlobalData0, GlobalData, LLDS, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred backend_pass_by_phases(module_info::in, module_info::out,
    global_data::in, global_data::out, list(c_procedure)::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

backend_pass_by_phases(!HLDS, !GlobalData, LLDS, !DumpInfo, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),

    maybe_saved_vars(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 310, "saved_vars_const", !DumpInfo, !IO),

    maybe_stack_opt(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 315, "saved_vars_cell", !DumpInfo, !IO),

    maybe_followcode(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 320, "followcode", !DumpInfo, !IO),

    simplify(no, ll_backend, Verbose, Stats,
        process_all_nonimported_nonaditi_procs, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 325, "ll_backend_simplify", !DumpInfo, !IO),

    compute_liveness(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 330, "liveness", !DumpInfo, !IO),

    compute_stack_vars(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 335, "stackvars", !DumpInfo, !IO),

    allocate_store_map(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 340, "store_map", !DumpInfo, !IO),

    maybe_goal_paths(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 345, "precodegen", !DumpInfo, !IO),

    generate_code(!.HLDS, Verbose, Stats, !GlobalData, LLDS1, !IO),

    maybe_generate_stack_layouts(!.HLDS, LLDS1, Verbose, Stats, !GlobalData,
        !IO),
    % maybe_dump_global_data(!.GlobalData, !IO),

    maybe_do_optimize(!.GlobalData, Verbose, Stats, LLDS1, LLDS, !IO).

:- pred backend_pass_by_preds(module_info::in, module_info::out,
    global_data::in, global_data::out, list(c_procedure)::out,
    io::di, io::uo) is det.

backend_pass_by_preds(!HLDS, !GlobalData, LLDS, !IO) :-
    module_info_predids(!.HLDS, PredIds),
    globals__io_lookup_bool_option(optimize_proc_dups, ProcDups, !IO),
    (
        ProcDups = no,
        OrderedPredIds = PredIds,
        MaybeDupProcMap = no
    ;
        ProcDups = yes,
        dependency_graph__build_pred_dependency_graph(!.HLDS,
            do_not_include_imported, DepInfo),
        hlds_dependency_info_get_dependency_ordering(DepInfo, PredSCCs),
        list__condense(PredSCCs, OrderedPredIds),
        MaybeDupProcMap = yes(map.init)
    ),
    backend_pass_by_preds_2(OrderedPredIds, !HLDS, !GlobalData,
        MaybeDupProcMap, LLDS, !IO).

:- pred backend_pass_by_preds_2(list(pred_id)::in,
    module_info::in, module_info::out, global_data::in, global_data::out,
    maybe(map(mdbcomp__prim_data__proc_label,
        mdbcomp__prim_data__proc_label))::in,
    list(c_procedure)::out, io::di, io::uo) is det.

backend_pass_by_preds_2([], !HLDS, !GlobalData, _, [], !IO).
backend_pass_by_preds_2([PredId | PredIds], !HLDS,
        !GlobalData, !.MaybeDupProcMap, Code, !IO) :-
    module_info_preds(!.HLDS, PredTable),
    map__lookup(PredTable, PredId, PredInfo),
    ProcIds = pred_info_non_imported_procids(PredInfo),
    (
        ( ProcIds = []
        ; hlds_pred__pred_info_is_aditi_relation(PredInfo)
        )
    ->
        ProcList = []
    ;
        globals__io_lookup_bool_option(verbose, Verbose, !IO),
        (
            Verbose = yes,
            io__write_string("% Generating code for ", !IO),
            hlds_out__write_pred_id(!.HLDS, PredId, !IO),
            io__write_string("\n", !IO)
        ;
            Verbose = no
        ),
        (
            PredModule = pred_info_module(PredInfo),
            PredName = pred_info_name(PredInfo),
            PredArity = pred_info_orig_arity(PredInfo),
            no_type_info_builtin(PredModule, PredName, PredArity)
        ->
            % These predicates should never be traced, since they do not obey
            % typeinfo_liveness. Since they may be opt_imported into other
            % modules, we must switch off the tracing of such preds on a
            % pred-by-pred basis; module-by-module wouldn't work.
            module_info_globals(!.HLDS, Globals0),
            globals__get_trace_level(Globals0, TraceLevel),
            globals__set_trace_level_none(Globals0, Globals1),
            module_info_set_globals(Globals1, !HLDS),
            copy(Globals1, Globals1Unique),
            globals__io_set_globals(Globals1Unique, !IO),
            backend_pass_by_preds_3(ProcIds, PredId, PredInfo, !HLDS,
                !GlobalData, IdProcList, !IO),
            module_info_globals(!.HLDS, Globals2),
            globals__set_trace_level(TraceLevel, Globals2, Globals),
            module_info_set_globals(Globals, !HLDS),
            copy(Globals, GlobalsUnique),
            globals__io_set_globals(GlobalsUnique, !IO)
        ;
            backend_pass_by_preds_3(ProcIds, PredId, PredInfo, !HLDS,
                !GlobalData, IdProcList, !IO)
        ),
        (
            !.MaybeDupProcMap = no,
            assoc_list__values(IdProcList, ProcList)
        ;
            !.MaybeDupProcMap = yes(DupProcMap0),
            eliminate_duplicate_procs(IdProcList, ProcList,
                DupProcMap0, DupProcMap),
            !:MaybeDupProcMap = yes(DupProcMap)
        )
    ),
    backend_pass_by_preds_2(PredIds, !HLDS, !GlobalData, !.MaybeDupProcMap,
        TailPredsCode, !IO),
    list__append(ProcList, TailPredsCode, Code).

:- pred backend_pass_by_preds_3(list(proc_id)::in, pred_id::in, pred_info::in,
    module_info::in, module_info::out, global_data::in, global_data::out,
    assoc_list(mdbcomp__prim_data__proc_label, c_procedure)::out,
    io::di, io::uo) is det.

backend_pass_by_preds_3([], _, _, !HLDS, !GlobalData, [], !IO).
backend_pass_by_preds_3([ProcId | ProcIds], PredId, PredInfo, !HLDS,
        !GlobalData, [ProcLabel - Proc | Procs], !IO) :-
    ProcLabel = make_proc_label(!.HLDS, PredId, ProcId),
    pred_info_procedures(PredInfo, ProcTable),
    map__lookup(ProcTable, ProcId, ProcInfo),
    backend_pass_by_preds_4(PredInfo, ProcInfo, _, ProcId, PredId, !HLDS,
        !GlobalData, Proc, !IO),
    backend_pass_by_preds_3(ProcIds, PredId, PredInfo, !HLDS, !GlobalData,
        Procs, !IO).

:- pred backend_pass_by_preds_4(pred_info::in, proc_info::in, proc_info::out,
    proc_id::in, pred_id::in, module_info::in, module_info::out,
    global_data::in, global_data::out, c_procedure::out, io::di, io::uo)
    is det.

backend_pass_by_preds_4(PredInfo, !ProcInfo, ProcId, PredId, !HLDS,
        !GlobalData, ProcCode, !IO) :-
    module_info_globals(!.HLDS, Globals),
    globals__lookup_bool_option(Globals, optimize_saved_vars_const,
        SavedVarsConst),
    (
        SavedVarsConst = yes,
        saved_vars_proc(PredId, ProcId, !ProcInfo, !HLDS, !IO)
    ;
        SavedVarsConst = no
    ),
    globals__lookup_bool_option(Globals, optimize_saved_vars_cell,
        SavedVarsCell),
    (
        SavedVarsCell = yes,
        stack_opt_cell(PredId, ProcId, !ProcInfo, !HLDS, !IO)
    ;
        SavedVarsCell = no
    ),
    globals__lookup_bool_option(Globals, follow_code, FollowCode),
    globals__lookup_bool_option(Globals, prev_code, PrevCode),
    (
        ( FollowCode = yes
        ; PrevCode = yes
        )
    ->
        move_follow_code_in_proc(PredId, ProcId, PredInfo, !ProcInfo, !HLDS)
    ;
        true
    ),
    simplify__find_simplifications(no, Globals, Simplifications0),

    globals.lookup_bool_option(Globals, profile_deep, DeepProf),
    globals.lookup_bool_option(Globals, record_term_sizes_as_words, TSWProf),
    globals.lookup_bool_option(Globals, record_term_sizes_as_cells, TSCProf),
    
    ProfTrans = bool.or_list([DeepProf, TSWProf, TSCProf]),
    ( 
        % Don't run constant propagation if any of the profiling
        % transformations has been applied.
        %
        % NOTE: Any changes here may also need to be made to
        %       mercury_compile.simplify.
        %
        ProfTrans = yes,
        Simplifications = list.delete_all(Simplifications0, constant_prop)
    ;
        ProfTrans = no,
        Simplifications = Simplifications0
    ),
      
    simplify__proc([do_once | Simplifications], PredId, ProcId,
        !HLDS, !ProcInfo, !IO),
    write_proc_progress_message("% Computing liveness in ", PredId, ProcId,
        !.HLDS, !IO),
    detect_liveness_proc(PredId, ProcId, !.HLDS, !ProcInfo, !IO),
    write_proc_progress_message("% Allocating stack slots in ", PredId,
        ProcId, !.HLDS, !IO),
    allocate_stack_slots_in_proc(PredId, ProcId, !.HLDS, !ProcInfo, !IO),
    write_proc_progress_message(
        "% Allocating storage locations for live vars in ",
        PredId, ProcId, !.HLDS, !IO),
    allocate_store_maps(final_allocation, PredId, !.HLDS, !ProcInfo),
    globals__io_get_trace_level(TraceLevel, !IO),
    ( given_trace_level_is_none(TraceLevel) = no ->
        write_proc_progress_message("% Calculating goal paths in ",
            PredId, ProcId, !.HLDS, !IO),
        goal_path__fill_slots(!.HLDS, !ProcInfo)
    ;
        true
    ),
    write_proc_progress_message("% Generating low-level (LLDS) code for ",
        PredId, ProcId, !.HLDS, !IO),
    generate_proc_code(PredInfo, !.ProcInfo, ProcId, PredId, !.HLDS,
        !GlobalData, ProcCode0),
    globals__lookup_bool_option(Globals, optimize, Optimize),
    (
        Optimize = yes,
        optimize__proc(!.GlobalData, ProcCode0, ProcCode, !IO)
    ;
        Optimize = no,
        ProcCode = ProcCode0
    ),
    ProcCode = c_procedure(_, _, PredProcId, Instructions, _, _, _),
    write_proc_progress_message(
        "% Generating call continuation information for ",
        PredId, ProcId, !.HLDS, !IO),
    continuation_info__maybe_process_proc_llds(Instructions, PredProcId,
        !.HLDS, !GlobalData).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred puritycheck(bool::in, bool::in, module_info::in, module_info::out,
    bool::in, bool::out, io::di, io::uo) is det.

puritycheck(Verbose, Stats, !HLDS, FoundTypeError, FoundPostTypecheckError,
        !IO) :-
    module_info_num_errors(!.HLDS, NumErrors0),
    puritycheck(FoundTypeError, FoundPostTypecheckError, !HLDS, !IO),
    module_info_num_errors(!.HLDS, NumErrors),
    ( NumErrors \= NumErrors0 ->
        maybe_write_string(Verbose,
            "% Program contains purity error(s).\n", !IO),
        io__set_exit_status(1, !IO)
    ;
        maybe_write_string(Verbose,
            "% Program is purity-correct.\n", !IO)
    ),
    maybe_report_stats(Stats, !IO).

:- pred modecheck(bool::in, bool::in, module_info::in, module_info::out,
    bool::out, bool::out, io::di, io::uo) is det.

modecheck(Verbose, Stats, !HLDS, FoundModeError, UnsafeToContinue, !IO) :-
    module_info_num_errors(!.HLDS, NumErrors0),
    maybe_benchmark_modes(
        (pred(H0::in, {H,U}::out, di, uo) is det -->
            modecheck(H0, H, U)
        ),
        "modecheck", !.HLDS, {!:HLDS, UnsafeToContinue}, !IO),
    module_info_num_errors(!.HLDS, NumErrors),
    ( NumErrors \= NumErrors0 ->
        FoundModeError = yes,
        maybe_write_string(Verbose, "% Program contains mode error(s).\n",
            !IO),
        io__set_exit_status(1, !IO)
    ;
        FoundModeError = no,
        maybe_write_string(Verbose, "% Program is mode-correct.\n", !IO)
    ),
    maybe_report_stats(Stats, !IO).

:- pred maybe_mode_constraints(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_mode_constraints(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(mode_constraints, ModeConstraints, !IO),
    (
        ModeConstraints = yes,
        maybe_write_string(Verbose, "% Dumping mode constraints...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        maybe_benchmark_modes(mode_constraints__process_module,
            "mode-constraints", !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        ModeConstraints = no
    ).

:- pred maybe_benchmark_modes(pred(T1, T2, io, io)::in(pred(in, out, di, uo)
    is det), string::in, T1::in, T2::out, io::di, io::uo) is det.

maybe_benchmark_modes(Pred, Stage, A0, A, !IO) :-
    globals__io_lookup_bool_option(benchmark_modes, BenchmarkModes, !IO),
    (
        BenchmarkModes = yes,
        globals__io_lookup_int_option(benchmark_modes_repeat, Repeats, !IO),
        io__format("%s %d ", [s(Stage), i(Repeats)], !IO),
        promise_equivalent_solutions [A, Time, !:IO] (
            do_io_benchmark(Pred, Repeats, A0, A - Time, !IO)
        ),
        io__format("%d ms\n", [i(Time)], !IO)
    ;
        BenchmarkModes = no,
        Pred(A0, A, !IO)
    ).

:- pred do_io_benchmark(pred(T1, T2, io, io)::in(pred(in, out, di, uo) is det),
    int::in, T1::in, pair(T2, int)::out, io::di, io::uo) is cc_multi.

do_io_benchmark(Pred, Repeats, A0, A - Time, !IO) :-
    benchmark_det_io(Pred, A0, A, !IO, Repeats, Time).

:- pred detect_switches(bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

detect_switches(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Detecting switches...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    detect_switches(!HLDS, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred detect_cse(bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

detect_cse(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(common_goal, CommonGoal, !IO),
    (
        CommonGoal = yes,
        maybe_write_string(Verbose,
            "% Detecting common deconstructions...\n", !IO),
        detect_cse(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        CommonGoal = no
    ).

:- pred check_determinism(bool::in, bool::in,
    module_info::in, module_info::out, bool::out, io::di, io::uo) is det.

check_determinism(Verbose, Stats, !HLDS, FoundError, !IO) :-
    module_info_num_errors(!.HLDS, NumErrors0),
    determinism_pass(!HLDS, !IO),
    module_info_num_errors(!.HLDS, NumErrors),
    ( NumErrors \= NumErrors0 ->
        FoundError = yes,
        maybe_write_string(Verbose,
            "% Program contains determinism error(s).\n", !IO),
        io__set_exit_status(1, !IO)
    ;
        FoundError = no,
        maybe_write_string(Verbose, "% Program is determinism-correct.\n", !IO)
    ),
    maybe_report_stats(Stats, !IO).

:- pred mercury_compile.maybe_closure_analysis(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

mercury_compile.maybe_closure_analysis(Verbose, Stats, !HLDS, !IO) :-
    globals.io_lookup_bool_option(analyse_closures, ClosureAnalysis,
        !IO),
    (
        ClosureAnalysis = yes,
        maybe_write_string(Verbose, "% Analysing closures...\n", !IO),
        closure_analysis.process_module(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        ClosureAnalysis = no
    ).

:- pred mercury_compile.maybe_exception_analysis(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

mercury_compile.maybe_exception_analysis(Verbose, Stats, !HLDS, !IO) :-
    globals.io_lookup_bool_option(analyse_exceptions, ExceptionAnalysis, !IO),
    (
        ExceptionAnalysis = yes,
        maybe_write_string(Verbose, "% Analysing exceptions...\n", !IO),
        exception_analysis.process_module(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        ExceptionAnalysis = no
    ).

:- pred maybe_termination(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_termination(Verbose, Stats, !HLDS, !IO) :-
    globals__io_get_globals(Globals, !IO),
    globals__lookup_bool_option(Globals, polymorphism, Polymorphism),
    globals__lookup_bool_option(Globals, termination, Termination),
    % Termination analysis requires polymorphism to be run,
    % since it does not handle complex unification
    (
        Polymorphism = yes,
        Termination = yes
    ->
        maybe_write_string(Verbose, "% Detecting termination...\n", !IO),
        termination__pass(!HLDS, !IO),
        maybe_write_string(Verbose, "% Termination checking done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        true
    ).

:- pred maybe_termination2(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_termination2(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(polymorphism, Polymorphism, !IO),
    globals__io_lookup_bool_option(termination2, Termination2, !IO),
    % Termination analysis requires polymorphism to be run,
    % as termination analysis does not handle complex unification.
    ( 
        Polymorphism = yes,
        Termination2 = yes
    ->
        maybe_write_string(Verbose, "% Detecting termination 2...\n", !IO),
        term_constr_main__pass(!HLDS, !IO),
        maybe_write_string(Verbose, "% Termination 2 checking done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        true
    ).

:- pred check_unique_modes(bool::in, bool::in,
    module_info::in, module_info::out, bool::out, io::di, io::uo) is det.

check_unique_modes(Verbose, Stats, !HLDS, FoundError, !IO) :-
    maybe_write_string(Verbose,
        "% Checking for backtracking over unique modes...\n", !IO),
    module_info_num_errors(!.HLDS, NumErrors0),
    unique_modes__check_module(!HLDS, !IO),
    module_info_num_errors(!.HLDS, NumErrors),
    ( NumErrors \= NumErrors0 ->
        FoundError = yes,
        maybe_write_string(Verbose,
            "% Program contains unique mode error(s).\n", !IO),
        io__set_exit_status(1, !IO)
    ;
        FoundError = no,
        maybe_write_string(Verbose, "% Program is unique-mode-correct.\n", !IO)
    ),
    maybe_report_stats(Stats, !IO).

:- pred check_stratification(bool::in, bool::in,
    module_info::in, module_info::out, bool::out, io::di, io::uo) is det.

check_stratification(Verbose, Stats, !HLDS, FoundError,
        !IO) :-
    module_info_stratified_preds(!.HLDS, StratifiedPreds),
    globals__io_lookup_bool_option(warn_non_stratification, Warn, !IO),
    (
        ( \+ set__empty(StratifiedPreds)
        ; Warn = yes
        )
    ->
        maybe_write_string(Verbose,
            "% Checking stratification...\n", !IO),
        io__get_exit_status(OldStatus, !IO),
        io__set_exit_status(0, !IO),
        stratify__check_stratification(!HLDS, !IO),
        io__get_exit_status(NewStatus, !IO),
        ( NewStatus \= 0 ->
            FoundError = yes,
            maybe_write_string(Verbose,
                "% Program contains stratification error(s).\n", !IO)
        ;
            FoundError = no,
            maybe_write_string(Verbose, "% done.\n", !IO),
            io__set_exit_status(OldStatus, !IO)
        ),
        maybe_report_stats(Stats, !IO)
    ;
        FoundError = no
    ).

:- pred maybe_warn_dead_procs(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_warn_dead_procs(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(warn_dead_procs, WarnDead, !IO),
    (
        WarnDead = yes,
        maybe_write_string(Verbose, "% Warning about dead procedures...\n",
            !IO),
        maybe_flush_output(Verbose, !IO),
        dead_proc_elim(warning_pass, !.HLDS, _HLDS1, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)

%       XXX The warning pass also does all the work of optimizing
%       away the dead procedures.  If we're optimizing, then
%       it would be nice if we could keep the HLDS that results.
%       However, because this pass gets run before type
%       specialization, dead code elimination at this point
%       incorrectly optimizes away procedures created for
%       `pragma type_spec' declarations.  So we can't use the
%       code below.  Instead we need to keep original HLDS.
%
%       %%% globals__io_lookup_bool_option(optimize_dead_procs,
%       %%%     OptimizeDead, !IO),
%       %%% ( OptimizeDead = yes ->
%       %%%     !:HLDS = HLDS1
%       %%% ;
%       %%%     true
%       %%% )
    ;
        WarnDead = no
    ).

    % This type indicates what stage of compilation we are running
    % the simplification pass at.  The exact simplifications we run
    % will depend upon this.
    % 
:- type simplify_pass
    --->    frontend
            % Immediately after the frontend passes.

    ;       post_untuple
            % After the untupling transformation has been applied. 

    ;       pre_prof_transforms
            % If deep/term-size profiling is enabled then immediately
            % before the source-to-source transformations that 
            % implement them.
    
    ;       ml_backend
            % The first stage of MLDS code generation.
    
    ;       ll_backend.
            % The first stage of LLDS code generation.

:- pred simplify(bool::in, simplify_pass::in, bool::in, bool::in,
    pred(task, module_info, module_info, io, io)::in(pred(task, in, out,
        di, uo) is det),
    module_info::in, module_info::out, io::di, io::uo) is det.

simplify(Warn, SimplifyPass, Verbose, Stats, Process, !HLDS, !IO) :-
    globals.io_get_globals(Globals, !IO),
    globals.lookup_bool_option(Globals, profile_deep, DeepProf),
    globals.lookup_bool_option(Globals, record_term_sizes_as_words, TSWProf),
    globals.lookup_bool_option(Globals, record_term_sizes_as_cells, TSCProf),
    %
    % We run the simplify pass before the profiling transformations,
    % only if those transformations are being applied - otherwise we
    % just leave things to the backend simplification passes.
    %
    IsProfPass = bool.or_list([DeepProf, TSWProf, TSCProf]),
    (
        SimplifyPass = pre_prof_transforms,
        IsProfPass = no
    ->
        true
    ;
        some [!Simplifications] (
            maybe_write_string(Verbose, "% Simplifying goals...\n", !IO),
            maybe_flush_output(Verbose, !IO),
           
            simplify.find_simplifications(Warn, Globals, !:Simplifications),
            (
                SimplifyPass = frontend
            ;
                SimplifyPass = post_untuple,
                list.cons(do_once, !Simplifications) 
            ;
                SimplifyPass = pre_prof_transforms,
                list.cons(do_once, !Simplifications)
            ;
                SimplifyPass = ml_backend,
                list.cons(do_once, !Simplifications)
            ;
                % Don't perform constant propagation if one of the
                % profiling transformations has been applied.
                %
                % NOTE: Any changes made here may also need to be made
                % to the relevant parts of backend_pass_by_preds_4/12.
                %
                SimplifyPass = ll_backend,
                (
                    IsProfPass = yes,
                    % XXX Why does find_simplifications return a list of
                    % them rather than a set?
                    list.delete_all(!.Simplifications, constant_prop,
                        !:Simplifications)
                ;
                    IsProfPass = no
                ),
                list.cons(do_once, !Simplifications)
            ),
            Process(update_pred_error(simplify.pred(!.Simplifications)), !HLDS,
                !IO),
    
            maybe_write_string(Verbose, "% done.\n", !IO),
            maybe_report_stats(Stats, !IO)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_mark_static_terms(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_mark_static_terms(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(static_ground_terms, StaticGroundTerms,
        !IO),
    (
        StaticGroundTerms = yes,
        maybe_write_string(Verbose, "% Marking static ground terms...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_proc(mark_static_terms),
            !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        StaticGroundTerms = no
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_add_trail_ops(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_add_trail_ops(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(use_trail, UseTrail, !IO),
    (
        UseTrail = yes,
        maybe_write_string(Verbose, "% Adding trailing operations...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_proc(add_trail_ops), !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        UseTrail = no
    ).

:- pred maybe_add_heap_ops(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_add_heap_ops(Verbose, Stats, !HLDS, !IO) :-
    globals__io_get_gc_method(GC, !IO),
    globals__io_lookup_bool_option(reclaim_heap_on_semidet_failure,
        SemidetReclaim, !IO),
    globals__io_lookup_bool_option(reclaim_heap_on_nondet_failure,
        NondetReclaim, !IO),
    (
        gc_is_conservative(GC) = yes
    ->
        % we can't do heap reclamation with conservative GC
        true
    ;
        SemidetReclaim = no,
        NondetReclaim = no
    ->
        true
    ;
        SemidetReclaim = yes,
        NondetReclaim = yes
    ->
        maybe_write_string(Verbose,
            "% Adding heap reclamation operations...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_proc(add_heap_ops), !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Msg = "Sorry, not implemented: `--high-level-code' and just one of " ++
            "`--reclaim-heap-on-semidet-failure' and " ++
            "`--reclaim-heap-on-nondet-failure'. " ++
            "Use `--(no-)reclaim-heap-on-failure' instead.",
        write_error_pieces_plain([words(Msg)], !IO),
        io__set_exit_status(1, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_write_dependency_graph(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_write_dependency_graph(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(show_dependency_graph, ShowDepGraph, !IO),
    (
        ShowDepGraph = yes,
        maybe_write_string(Verbose, "% Writing dependency graph...", !IO),
        module_info_name(!.HLDS, ModuleName),
        module_name_to_file_name(ModuleName, ".dependency_graph", yes,
            FileName, !IO),
        io__open_output(FileName, Res, !IO),
        ( Res = ok(FileStream) ->
            io__set_output_stream(FileStream, OutputStream, !IO),
            dependency_graph__write_dependency_graph(!HLDS, !IO),
            io__set_output_stream(OutputStream, _, !IO),
            io__close_output(FileStream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            report_error("unable to write dependency graph.", !IO)
        ),
        maybe_report_stats(Stats, !IO)
    ;
        ShowDepGraph = no
    ).

    % Outputs the file <module_name>.prof, which contains the static
    % call graph in terms of label names, if the profiling flag is enabled.
    %
:- pred maybe_output_prof_call_graph(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_output_prof_call_graph(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(profile_calls, ProfileCalls, !IO),
    globals__io_lookup_bool_option(profile_time, ProfileTime, !IO),
    (
        ( ProfileCalls = yes
        ; ProfileTime = yes
        )
    ->
        maybe_write_string(Verbose,
            "% Outputing profiling call graph...", !IO),
        maybe_flush_output(Verbose, !IO),
        module_info_name(!.HLDS, ModuleName),
        module_name_to_file_name(ModuleName, ".prof", yes, ProfFileName, !IO),
        io__open_output(ProfFileName, Res, !IO),
        ( Res = ok(FileStream) ->
            io__set_output_stream(FileStream, OutputStream, !IO),
            dependency_graph__write_prof_dependency_graph(!HLDS, !IO),
            io__set_output_stream(OutputStream, _, !IO),
            io__close_output(FileStream, !IO)
        ;
            report_error("unable to write profiling static call graph.", !IO)
        ),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_generate_schemas(module_info::in,
    bool::in, bool::in, io::di, io::uo) is det.

maybe_generate_schemas(ModuleInfo, Verbose, Stats, !IO) :-
    globals__io_lookup_bool_option(generate_schemas, Generate, !IO),
    (
        Generate = yes,
        maybe_write_string(Verbose, "% Writing schema file...", !IO),
        rl_out__generate_schema_file(ModuleInfo, !IO),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Generate = no
    ).

%-----------------------------------------------------------------------------%

:- pred tabling(bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

tabling(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Transforming tabled predicates...", !IO),
    maybe_flush_output(Verbose, !IO),
    table_gen__process_module(!HLDS, !IO),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%

:- pred process_lambdas(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

process_lambdas(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Transforming lambda expressions...", !IO),
    maybe_flush_output(Verbose, !IO),
    lambda__process_module(!HLDS),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%

:- pred expand_equiv_types_hlds(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

expand_equiv_types_hlds(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Fully expanding equivalence types...", !IO),
    maybe_flush_output(Verbose, !IO),
    equiv_type_hlds__replace_in_hlds(!HLDS),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%

:- pred maybe_polymorphism(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_polymorphism(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(polymorphism, Polymorphism, !IO),
    (
        Polymorphism = yes,
        maybe_write_string(Verbose,
            "% Transforming polymorphic unifications...", !IO),
        maybe_flush_output(Verbose, !IO),
        polymorphism__process_module(!HLDS, !IO),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Polymorphism = no,
        % The --no-polymorphism option really doesn't make much
        % sense anymore, because the polymorphism pass is necessary
        % for the proper mode analysis of code using existential
        % types.
        error("sorry, `--no-polymorphism' is no longer supported")
    ).

:- pred maybe_type_ctor_infos(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_type_ctor_infos(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(type_ctor_info, TypeCtorInfo, !IO),
    (
        TypeCtorInfo = yes,
        maybe_write_string(Verbose,
            "% Generating type_ctor_info structures...", !IO),
        maybe_flush_output(Verbose, !IO),
        type_ctor_info__generate_hlds(!HLDS),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        TypeCtorInfo = no
    ).

:- pred maybe_bytecodes(module_info::in, module_name::in,
    bool::in, bool::in, dump_info::in, dump_info::out, io::di, io::uo) is det.

maybe_bytecodes(HLDS0, ModuleName, Verbose, Stats, !DumpInfo, !IO) :-
    globals__io_lookup_bool_option(generate_bytecode, GenBytecode, !IO),
    (
        GenBytecode = yes,
        map_args_to_regs(Verbose, Stats, HLDS0, HLDS1, !IO),
        maybe_dump_hlds(HLDS1, 505, "bytecode_args_to_regs", !DumpInfo, !IO),
        maybe_write_string(Verbose, "% Generating bytecodes...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        bytecode_gen__module(HLDS1, Bytecode, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO),
        module_name_to_file_name(ModuleName, ".bytedebug", yes, BytedebugFile,
            !IO),
        maybe_write_string(Verbose, "% Writing bytecodes to `", !IO),
        maybe_write_string(Verbose, BytedebugFile, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        maybe_flush_output(Verbose, !IO),
        debug_bytecode_file(BytedebugFile, Bytecode, !IO),
        maybe_write_string(Verbose, " done.\n", !IO),
        module_name_to_file_name(ModuleName, ".mbc", yes, BytecodeFile, !IO),
        maybe_write_string(Verbose, "% Writing bytecodes to `", !IO),
        maybe_write_string(Verbose, BytecodeFile, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        maybe_flush_output(Verbose, !IO),
        output_bytecode_file(BytecodeFile, Bytecode, !IO),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        GenBytecode = no
    ).

:- pred maybe_untuple_arguments(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_untuple_arguments(Verbose, Stats, !HLDS, !IO) :-
    globals.io_lookup_bool_option(untuple, Untuple, !IO),
    (
        Untuple = yes,
        maybe_write_string(Verbose, "% Untupling...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        untuple_arguments(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        simplify(no, post_untuple, Verbose, Stats,
            process_all_nonimported_nonaditi_procs, !HLDS, !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Untuple = no
    ).

:- pred maybe_tuple_arguments(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_tuple_arguments(Verbose, Stats, !HLDS, !IO) :-
    globals.io_lookup_bool_option(tuple, Tuple, !IO),   
    (
        Tuple = yes,
        maybe_write_string(Verbose, "% Tupling...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        tuple_arguments(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Tuple = no
    ).

:- pred maybe_higher_order(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_higher_order(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(optimize_higher_order, HigherOrder, !IO),
    % --type-specialization implies --user-guided-type-specialization.
    globals__io_lookup_bool_option(user_guided_type_specialization, Types,
        !IO),

    % Always produce the specialized versions for which
    % `:- pragma type_spec' declarations exist, because
    % importing modules might call them.
    module_info_type_spec_info(!.HLDS, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(TypeSpecPreds, _, _, _),
    (
        ( HigherOrder = yes
        ; Types = yes
        ; \+ set__empty(TypeSpecPreds)
        )
    ->
        maybe_write_string(Verbose,
            "% Specializing higher-order and polymorphic predicates...\n",
            !IO),
        maybe_flush_output(Verbose, !IO),

        specialize_higher_order(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        true
    ).

:- pred maybe_do_inlining(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_do_inlining(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(allow_inlining, Allow, !IO),
    globals__io_lookup_bool_option(inline_simple, Simple, !IO),
    globals__io_lookup_bool_option(inline_single_use, SingleUse, !IO),
    globals__io_lookup_int_option(inline_compound_threshold, Threshold, !IO),
    (
        Allow = yes,
        ( Simple = yes
        ; SingleUse = yes
        ; Threshold > 0
        )
    ->
        maybe_write_string(Verbose, "% Inlining...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        inlining(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        true
    ).

:- pred maybe_deforestation(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_deforestation(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(deforestation, Deforest, !IO),

    % --constraint-propagation implies --local-constraint-propagation.
    globals__io_lookup_bool_option(local_constraint_propagation, Constraints,
        !IO),
    (
        ( Deforest = yes
        ; Constraints = yes
        )
    ->
        (
            Deforest = no,
            Constraints = no,
            error("mercury_compile__maybe_deforestation")
        ;
            Deforest = yes,
            Constraints = yes,
            Msg = "% Deforestation and constraint propagation...\n"
        ;
            Deforest = yes,
            Constraints = no,
            Msg = "% Deforestation...\n"
        ;
            Deforest = no,
            Constraints = yes,
            Msg = "% Constraint propagation...\n"
        ),
        maybe_write_string(Verbose, Msg, !IO),
        maybe_flush_output(Verbose, !IO),
        deforestation(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        true
    ).

:- pred maybe_transform_dnf(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_transform_dnf(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_do_aditi_compilation(!.HLDS, Aditi),
    ( Aditi = do_aditi_compilation ->
        maybe_write_string(Verbose,
            "% Disjunctive normal form transformation...", !IO),
        maybe_flush_output(Verbose, !IO),
        module_info_predids(!.HLDS, PredIds),
        set__init(AditiPreds0),
        list__foldl(add_aditi_procs(!.HLDS), PredIds, AditiPreds0, AditiPreds),
        dnf__transform_module(no, yes(AditiPreds), !HLDS),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        true
    ).

:- pred add_aditi_procs(module_info::in, pred_id::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

add_aditi_procs(HLDS0, PredId, AditiPreds0, AditiPreds) :-
    module_info_pred_info(HLDS0, PredId, PredInfo),
    ( hlds_pred__pred_info_is_aditi_relation(PredInfo) ->
        ProcIds = pred_info_procids(PredInfo),
        AddProc = (pred(ProcId::in, Preds0::in, Preds::out) is det :-
            set__insert(Preds0, proc(PredId, ProcId), Preds)
        ),
        list__foldl(AddProc, ProcIds, AditiPreds0, AditiPreds)
    ;
        AditiPreds = AditiPreds0
    ).

:- pred maybe_loop_inv(bool::in, bool::in,
    module_info::in, module_info::out, dump_info::in, dump_info::out,
    io::di, io::uo) is det.

maybe_loop_inv(Verbose, Stats, !HLDS, !DumpInfo, !IO) :-
    globals__io_lookup_bool_option(loop_invariants, LoopInv, !IO),
    (
        LoopInv = yes,
        % We run the mark_static pass because we need the construct_how flag
        % to be valid.
        %
        maybe_mark_static_terms(Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(!.HLDS, 148, "mark_static", !DumpInfo, !IO),

        maybe_write_string(Verbose, "% Hoisting loop invariants...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_module(hoist_loop_invariants),
            !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        LoopInv = no
    ).

:- pred maybe_delay_construct(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_delay_construct(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(delay_construct, DelayConstruct, !IO),
    (
        DelayConstruct = yes,
        maybe_write_string(Verbose,
            "% Delaying construction unifications ...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_proc_io(delay_construct_proc),
            !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        DelayConstruct = no
    ).

:- pred maybe_unused_args(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_unused_args(Verbose, Stats, !HLDS, !IO) :-
    globals__io_get_globals(Globals, !IO),
    globals__lookup_bool_option(Globals, intermod_unused_args, Intermod),
    globals__lookup_bool_option(Globals, optimize_unused_args, Optimize),
    globals__lookup_bool_option(Globals, warn_unused_args, Warn),
    (
        ( Optimize = yes
        ; Warn = yes
        ; Intermod = yes
        )
    ->
        maybe_write_string(Verbose, "% Finding unused arguments ...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        unused_args__process_module(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        true
    ).

:- pred maybe_unneeded_code(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_unneeded_code(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(unneeded_code, UnneededCode, !IO),
    (
        UnneededCode = yes,
        maybe_write_string(Verbose,
            "% Removing unneeded code from procedure bodies...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(
            update_module_io(unneeded_code__process_proc_msg), !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        UnneededCode = no
    ).

:- pred maybe_magic(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_magic(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_do_aditi_compilation(!.HLDS, Aditi),
    ( Aditi = do_aditi_compilation ->
        maybe_write_string(Verbose,
            "% Supplementary magic sets transformation...", !IO),
        maybe_flush_output(Verbose, !IO),
        magic__process_module(!HLDS, !IO),
        maybe_report_stats(Stats, !IO)
    ;
        true
    ).

:- pred maybe_eliminate_dead_procs(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_eliminate_dead_procs(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(optimize_dead_procs, Dead, !IO),
    (
        Dead = yes,
        maybe_write_string(Verbose, "% Eliminating dead procedures...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        dead_proc_elim(final_optimization_pass, !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Dead = no
    ).

:- pred maybe_term_size_prof(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_term_size_prof(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(record_term_sizes_as_words, AsWords, !IO),
    globals__io_lookup_bool_option(record_term_sizes_as_cells, AsCells, !IO),
    (
        AsWords = yes,
        AsCells = yes,
        error("mercury_compile__maybe_term_size_prof: as_words and as_cells")
    ;
        AsWords = yes,
        AsCells = no,
        MaybeTransform = yes(term_words)
    ;
        AsWords = no,
        AsCells = yes,
        MaybeTransform = yes(term_cells)
    ;
        AsWords = no,
        AsCells = no,
        MaybeTransform = no
    ),
    (
        MaybeTransform = yes(Transform),
        maybe_write_string(Verbose,
            "% Applying term size profiling transformation...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_nonaditi_procs(
            update_module_io(size_prof__process_proc_msg(Transform)),
            !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        MaybeTransform = no
    ).

:- pred maybe_read_experimental_complexity_file(
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_read_experimental_complexity_file(!HLDS, !IO) :-
    globals__io_lookup_string_option(experimental_complexity, FileName, !IO),
    globals__io_lookup_bool_option(record_term_sizes_as_words,
        RecordTermSizesAsWords, !IO),
    globals__io_lookup_bool_option(record_term_sizes_as_cells,
        RecordTermSizesAsCells, !IO),
    bool__or(RecordTermSizesAsWords, RecordTermSizesAsCells,
        RecordTermSizes),
    ( FileName = "" ->
%       While we could include the following sanity check, it is overly
%       strong. For example, a bootcheck in a term size profiling grade
%       would have to supply an --experimental-complexity option for
%       both the stage3 compiler and the test cases. Since the runtime
%       checks that all procedures mentioned in the files actually
%       exist and are transformed by the compiler, this would require
%       a different complexity experiment file for each test case,
%       which is impractical.
%       (
%           RecordTermSizes = yes,
%           report_error("term size profiling grades require " ++
%               "the --experimental-complexity option", !IO)
%       ;
%           RecordTermSizes = no
%       )
            true
    ;
        (
            RecordTermSizes = yes
        ;
            RecordTermSizes = no,
            report_error("the --experimental-complexity option " ++
                "requires a term size profiling grade", !IO)
        ),
        complexity__read_spec_file(FileName, MaybeNumProcMap, !IO),
        (
            MaybeNumProcMap = ok(NumProcs - ProcMap),
            module_info_set_maybe_complexity_proc_map(yes(NumProcs - ProcMap),
                !HLDS)
        ;
            MaybeNumProcMap = error(Msg),
            report_error(Msg, !IO)
        )
    ).

:- pred maybe_experimental_complexity(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_experimental_complexity(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_maybe_complexity_proc_map(!.HLDS, MaybeNumProcMap),
    (
        MaybeNumProcMap = no
    ;
        MaybeNumProcMap = yes(NumProcs - ProcMap),
        maybe_write_string(Verbose,
            "% Applying complexity experiment transformation...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_nonaditi_procs(
            update_module_io(complexity__process_proc_msg(NumProcs, ProcMap)),
            !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ).

:- pred maybe_deep_profiling(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_deep_profiling(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(profile_deep, ProfileDeep, !IO),
    (
        ProfileDeep = yes,
        maybe_write_string(Verbose,
            "% Applying deep profiling transformation...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        apply_deep_profiling_transformation(!HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        ProfileDeep = no
    ).

:- pred maybe_introduce_accumulators(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_introduce_accumulators(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(introduce_accumulators, Optimize, !IO),
    (
        Optimize = yes,
        maybe_write_string(Verbose,
            "% Attempting to introduce accumulators...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(
            update_module_io(accumulator__process_proc), !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Optimize = no
    ).

:- pred maybe_lco(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_lco(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(optimize_constructor_last_call, LCO, !IO),
    (
        LCO = yes,
        maybe_write_string(Verbose,
            "% Looking for LCO modulo constructor application ...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        lco_modulo_constructors(!HLDS),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        LCO = no
    ).

:- pred maybe_transform_aditi_builtins(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_transform_aditi_builtins(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_do_aditi_compilation(!.HLDS, Aditi),
    ( Aditi = do_aditi_compilation ->
        maybe_write_string(Verbose, "% Transforming away RL builtins...\n",
            !IO),
        maybe_flush_output(Verbose, !IO),
        transform_aditi_builtins(!HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

% The backend passes

:- pred map_args_to_regs(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

map_args_to_regs(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Mapping args to regs...", !IO),
    maybe_flush_output(Verbose, !IO),
    generate_arg_info(!HLDS),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred maybe_saved_vars(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_saved_vars(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(optimize_saved_vars_const, SavedVars, !IO),
    (
        SavedVars = yes,
        maybe_write_string(Verbose,
            "% Minimizing variable saves using constants...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_module_io(saved_vars_proc),
            !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        SavedVars = no
    ).

:- pred maybe_stack_opt(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_stack_opt(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(optimize_saved_vars_cell, SavedVars, !IO),
    (
        SavedVars = yes,
        maybe_write_string(Verbose,
            "% Minimizing variable saves using cells...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_module_io(stack_opt_cell),
            !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        SavedVars = no
    ).

:- pred maybe_followcode(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_followcode(Verbose, Stats, !HLDS, !IO) :-
    globals__io_lookup_bool_option(follow_code, FollowCode, !IO),
    globals__io_lookup_bool_option(prev_code, PrevCode, !IO),
    (
        ( FollowCode = yes
        ; PrevCode = yes
        )
    ->
        maybe_write_string(Verbose, "% Migrating branch code...", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_nonaditi_procs(update_module(
            move_follow_code_in_proc), !HLDS, !IO),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        true
    ).

:- pred compute_liveness(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

compute_liveness(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Computing liveness...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    process_all_nonimported_nonaditi_procs(
        update_proc_io(detect_liveness_proc), !HLDS, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred compute_stack_vars(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

compute_stack_vars(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Computing stack vars...", !IO),
    maybe_flush_output(Verbose, !IO),
    process_all_nonimported_nonaditi_procs(
        update_proc_io(allocate_stack_slots_in_proc), !HLDS, !IO),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred allocate_store_map(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

allocate_store_map(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Allocating store map...", !IO),
    maybe_flush_output(Verbose, !IO),
    process_all_nonimported_nonaditi_procs(
        update_proc_predid(allocate_store_maps(final_allocation)), !HLDS, !IO),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred maybe_goal_paths(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_goal_paths(Verbose, Stats, !HLDS, !IO) :-
    globals__io_get_trace_level(TraceLevel, !IO),
    ( given_trace_level_is_none(TraceLevel) = no ->
        maybe_write_string(Verbose, "% Calculating goal paths...", !IO),
        maybe_flush_output(Verbose, !IO),
        process_all_nonimported_procs(update_proc(goal_path__fill_slots),
            !HLDS, !IO),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        true
    ).

:- pred generate_code(module_info::in, bool::in, bool::in,
    global_data::in, global_data::out, list(c_procedure)::out,
    io::di, io::uo) is det.

generate_code(HLDS, Verbose, Stats, !GlobalData, LLDS, !IO) :-
    maybe_write_string(Verbose, "% Generating code...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    generate_code(HLDS, !GlobalData, LLDS, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred maybe_do_optimize(global_data::in, bool::in, bool::in,
    list(c_procedure)::in, list(c_procedure)::out, io::di, io::uo) is det.

maybe_do_optimize(GlobalData, Verbose, Stats, !LLDS, !IO) :-
    globals__io_lookup_bool_option(optimize, Optimize, !IO),
    (
        Optimize = yes,
        maybe_write_string(Verbose, "% Doing optimizations...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        optimize_main(GlobalData, !LLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        Optimize = no
    ).

:- pred maybe_generate_stack_layouts(module_info::in, list(c_procedure)::in,
    bool::in, bool::in, global_data::in, global_data::out, io::di, io::uo)
    is det.

maybe_generate_stack_layouts(HLDS, LLDS, Verbose, Stats, !GlobalData, !IO) :-
    maybe_write_string(Verbose,
        "% Generating call continuation information...", !IO),
    maybe_flush_output(Verbose, !IO),
    continuation_info__maybe_process_llds(LLDS, HLDS, !GlobalData),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Gather together the information from the HLDS, given the foreign
    % language we are going to use, that is used for the foreign language
    % interface.
    % This stuff mostly just gets passed directly to the LLDS unchanged, but
    % we do do a bit of code generation -- for example, we call
    % export__get_foreign_export_{decls,defns} here, which do the generation
    % of C code for `pragma export' declarations.
    %
:- pred get_c_interface_info(module_info::in, foreign_language::in,
    foreign_interface_info::out) is det.

get_c_interface_info(HLDS, UseForeignLanguage, Foreign_InterfaceInfo) :-
    module_info_name(HLDS, ModuleName),
    module_info_get_foreign_decl(HLDS, ForeignDecls),
    module_info_get_foreign_import_module(HLDS, ForeignImports),
    module_info_get_foreign_body_code(HLDS, ForeignBodyCode),
    foreign__filter_decls(UseForeignLanguage, ForeignDecls,
        WantedForeignDecls, _OtherDecls),
    foreign__filter_imports(UseForeignLanguage, ForeignImports,
        WantedForeignImports, _OtherImports),
    foreign__filter_bodys(UseForeignLanguage, ForeignBodyCode,
        WantedForeignBodys, _OtherBodys),
    export__get_foreign_export_decls(HLDS, Foreign_ExportDecls),
    export__get_foreign_export_defns(HLDS, Foreign_ExportDefns),

    Foreign_InterfaceInfo = foreign_interface_info(ModuleName,
        WantedForeignDecls, WantedForeignImports,
        WantedForeignBodys, Foreign_ExportDecls, Foreign_ExportDefns).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The LLDS output pass

:- pred output_pass(module_info::in, global_data::in, list(c_procedure)::in,
    maybe(rl_file)::in, module_name::in, bool::out, list(string)::out,
    io::di, io::uo) is det.

output_pass(HLDS, GlobalData0, Procs, MaybeRLFile, ModuleName, CompileErrors,
        FactTableObjFiles, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),
    %
    % Here we generate the LLDS representations for
    % various data structures used for RTTI, type classes,
    % and stack layouts.
    % XXX this should perhaps be part of backend_pass
    % rather than output_pass.
    %
    type_ctor_info__generate_rtti(HLDS, TypeCtorRttiData),
    base_typeclass_info__generate_rtti(HLDS, OldTypeClassInfoRttiData),
    generate_aditi_proc_info(HLDS, AditiProcInfoRttiData),
    globals__io_lookup_bool_option(new_type_class_rtti, NewTypeClassRtti, !IO),
    type_class_info__generate_rtti(HLDS, NewTypeClassRtti,
        NewTypeClassInfoRttiData),
    list__append(OldTypeClassInfoRttiData, NewTypeClassInfoRttiData,
        TypeClassInfoRttiData),
    list__map(llds__wrap_rtti_data, TypeCtorRttiData, TypeCtorTables),
    list__map(llds__wrap_rtti_data, TypeClassInfoRttiData, TypeClassInfos),
    list__map(llds__wrap_rtti_data, AditiProcInfoRttiData, AditiProcInfos),
    stack_layout__generate_llds(HLDS, GlobalData0, GlobalData, StackLayouts,
        LayoutLabels),
    %
    % Here we perform some optimizations on the LLDS data.
    % XXX this should perhaps be part of backend_pass
    % rather than output_pass.
    %
    % XXX We assume that the foreign language we use is C
    get_c_interface_info(HLDS, c, C_InterfaceInfo),
    global_data_get_all_proc_vars(GlobalData, GlobalVars),
    global_data_get_all_closure_layouts(GlobalData, ClosureLayouts),
    global_data_get_static_cell_info(GlobalData, StaticCellInfo),
    StaticCells = get_static_cells(StaticCellInfo),

    %
    % Next we put it all together and output it to one or more C files.
    %
    list__condense([StaticCells, ClosureLayouts, StackLayouts,
        TypeCtorTables, TypeClassInfos, AditiProcInfos], AllData),
    construct_c_file(HLDS, C_InterfaceInfo, Procs, GlobalVars, AllData, CFile,
        NumChunks, !IO),
    module_info_get_complexity_proc_infos(HLDS, ComplexityProcs),
    output_llds(ModuleName, CFile, ComplexityProcs, LayoutLabels, MaybeRLFile,
        Verbose, Stats, !IO),

    C_InterfaceInfo = foreign_interface_info(_, _, _, _, C_ExportDecls, _),
    export__produce_header_file(C_ExportDecls, ModuleName, !IO),

    %
    % Finally we invoke the C compiler to compile it.
    %
    globals__io_lookup_bool_option(target_code_only, TargetCodeOnly, !IO),
    (
        TargetCodeOnly = no,
        io__output_stream(OutputStream, !IO),
        c_to_obj(OutputStream, ModuleName, NumChunks, CompileOK, !IO),
        module_get_fact_table_files(HLDS, FactTableBaseFiles),
        list__map2_foldl(compile_fact_table_file(OutputStream),
            FactTableBaseFiles, FactTableObjFiles, FactTableCompileOKs, !IO),
        bool__and_list([CompileOK | FactTableCompileOKs], AllOk),
        maybe_set_exit_status(AllOk, !IO),
        bool__not(AllOk, CompileErrors)
    ;
        TargetCodeOnly = yes,
        CompileErrors = no,
        FactTableObjFiles = []
    ).

    % Split the code up into bite-size chunks for the C compiler.

:- pred construct_c_file(module_info::in, foreign_interface_info::in,
    list(c_procedure)::in, list(comp_gen_c_var)::in, list(comp_gen_c_data)::in,
    c_file::out, int::out, io::di, io::uo) is det.

construct_c_file(ModuleInfo, C_InterfaceInfo, Procedures, GlobalVars, AllData,
        CFile, ComponentCount, !IO) :-
    C_InterfaceInfo = foreign_interface_info(ModuleSymName, C_HeaderCode0,
        C_Includes, C_BodyCode0, _C_ExportDecls, C_ExportDefns),
    MangledModuleName = sym_name_mangle(ModuleSymName),
    string__append(MangledModuleName, "_module", ModuleName),
    globals__io_lookup_int_option(procs_per_c_function, ProcsPerFunc, !IO),
    get_c_body_code(C_BodyCode0, C_BodyCode),
    ( ProcsPerFunc = 0 ->
        % ProcsPerFunc = 0 really means infinity -
        % we store all the procs in a single function.
        ChunkedModules = [comp_gen_c_module(ModuleName, Procedures)]
    ;
        list__chunk(Procedures, ProcsPerFunc, ChunkedProcs),
        combine_chunks(ChunkedProcs, ModuleName, ChunkedModules)
    ),
    list__map_foldl(make_foreign_import_header_code, C_Includes,
        C_IncludeHeaderCode, !IO),

    % The lists are reversed because insertions into them are at the front.
    % We don't want to put C_LocalHeaderCode between Start and End, because
    % C_IncludeHeaderCode may include our own header file, which defines
    % the module's guard macro, which in turn #ifdefs out the stuff between
    % Start and End.
    list__filter(foreign_decl_code_is_local, list__reverse(C_HeaderCode0),
        C_LocalHeaderCode, C_ExportedHeaderCode),
    make_decl_guards(ModuleSymName, Start, End),
    C_HeaderCode = list__reverse(C_IncludeHeaderCode) ++
        C_LocalHeaderCode ++ [Start | C_ExportedHeaderCode] ++ [End],

    module_info_user_init_pred_c_names(ModuleInfo, UserInitPredCNames),
    module_info_user_final_pred_c_names(ModuleInfo, UserFinalPredCNames),

    CFile = c_file(ModuleSymName, C_HeaderCode, C_BodyCode, C_ExportDefns,
            GlobalVars, AllData, ChunkedModules, UserInitPredCNames,
            UserFinalPredCNames),
    list__length(C_BodyCode, UserCCodeCount),
    list__length(C_ExportDefns, ExportCount),
    list__length(GlobalVars, CompGenVarCount),
    list__length(AllData, CompGenDataCount),
    list__length(ChunkedModules, CompGenCodeCount),
    ComponentCount = UserCCodeCount + ExportCount + CompGenVarCount +
        CompGenDataCount + CompGenCodeCount.

:- pred foreign_decl_code_is_local(foreign_decl_code::in) is semidet.

foreign_decl_code_is_local(Decl) :-
    Decl = foreign_decl_code(_, foreign_decl_is_local, _, _).

:- pred make_decl_guards(sym_name::in, foreign_decl_code::out,
    foreign_decl_code::out) is det.

make_decl_guards(ModuleName, StartGuard, EndGuard) :-
    Define = decl_guard(ModuleName),
    Start = "#ifndef " ++ Define ++ "\n#define " ++ Define ++ "\n",
    End = "\n#endif",
    StartGuard = foreign_decl_code(c, foreign_decl_is_exported, Start,
        term__context_init),
    EndGuard = foreign_decl_code(c, foreign_decl_is_exported, End,
        term__context_init).

:- pred make_foreign_import_header_code(foreign_import_module::in,
    foreign_decl_code::out, io::di, io::uo) is det.

make_foreign_import_header_code(ForeignImportModule, Include, !IO) :-
    ForeignImportModule = foreign_import_module(Lang, ModuleName, Context),
    (
        Lang = c,
        module_name_to_search_file_name(ModuleName, ".mh", HeaderFileName, !IO),
        string__append_list(["#include """, HeaderFileName, """\n"],
            IncludeString),
        Include = foreign_decl_code(c, foreign_decl_is_exported,
            IncludeString, Context)
    ;
        Lang = csharp,
        error("sorry. :- import_module not yet implemented: " ++
            "`:- pragma foreign_import_module' for C#")
    ;
        Lang = managed_cplusplus,
        error("sorry. :- import_module not yet implemented: " ++
            "`:- pragma foreign_import_module' for Managed C++")
    ;
        Lang = il,
        error("sorry. :- import_module not yet implemented: " ++
            "`:- pragma foreign_import_module' for IL")
    ;
        Lang = java,
        error("sorry. :- import_module not yet implemented: " ++
            "`:- pragma foreign_import_module' for Java")
    ).

:- pred get_c_body_code(foreign_body_info::in, list(user_foreign_code)::out)
    is det.

get_c_body_code([], []).
get_c_body_code([foreign_body_code(Lang, Code, Context) | CodesAndContexts],
        [user_foreign_code(Lang, Code, Context) | C_Modules]) :-
    get_c_body_code(CodesAndContexts, C_Modules).

:- pred combine_chunks(list(list(c_procedure))::in, string::in,
    list(comp_gen_c_module)::out) is det.

combine_chunks(ChunkList, ModName, Modules) :-
    combine_chunks_2(ChunkList, ModName, 0, Modules).

:- pred combine_chunks_2(list(list(c_procedure))::in,
    string::in, int::in, list(comp_gen_c_module)::out) is det.

combine_chunks_2([], _ModName, _N, []).
combine_chunks_2([Chunk | Chunks], ModuleName, Num, [Module | Modules]) :-
    string__int_to_string(Num, NumString),
    string__append(ModuleName, NumString, ThisModuleName),
    Module = comp_gen_c_module(ThisModuleName, Chunk),
    Num1 = Num + 1,
    combine_chunks_2(Chunks, ModuleName, Num1, Modules).

:- pred output_llds(module_name::in, c_file::in,
    list(complexity_proc_info)::in, map(llds__label, llds__data_addr)::in,
    maybe(rl_file)::in, bool::in, bool::in, io::di, io::uo) is det.

output_llds(ModuleName, LLDS0, ComplexityProcs, StackLayoutLabels, MaybeRLFile,
        Verbose, Stats, !IO) :-
    maybe_write_string(Verbose, "% Writing output to `", !IO),
    module_name_to_file_name(ModuleName, ".c", yes, FileName, !IO),
    maybe_write_string(Verbose, FileName, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    transform_llds(LLDS0, LLDS, !IO),
    output_llds(LLDS, ComplexityProcs, StackLayoutLabels, MaybeRLFile, !IO),
    maybe_write_string(Verbose, " done.\n", !IO),
    maybe_flush_output(Verbose, !IO),
    maybe_report_stats(Stats, !IO).

:- pred c_to_obj(io__output_stream::in, module_name::in,
    int::in, bool::out, io::di, io::uo) is det.

c_to_obj(ErrorStream, ModuleName, NumChunks, Succeeded, !IO) :-
    globals__io_lookup_bool_option(split_c_files, SplitFiles, !IO),
    (
        SplitFiles = yes,
        compile_target_code__split_c_to_obj(ErrorStream, ModuleName,
            NumChunks, Succeeded, !IO)
    ;
        SplitFiles = no,
        get_linked_target_type(LinkedTargetType, !IO),
        get_object_code_type(LinkedTargetType, PIC, !IO),
        maybe_pic_object_file_extension(PIC, Obj, !IO),
        module_name_to_file_name(ModuleName, ".c", no, C_File, !IO),
        module_name_to_file_name(ModuleName, Obj, yes, O_File, !IO),
        compile_target_code__compile_c_file(ErrorStream, PIC, C_File, O_File,
            Succeeded, !IO)
    ).

:- pred compile_fact_table_file(io__output_stream::in, string::in,
    string::out, bool::out, io::di, io::uo) is det.

compile_fact_table_file(ErrorStream, BaseName, O_File, Succeeded, !IO) :-
    get_linked_target_type(LinkedTargetType, !IO),
    get_object_code_type(LinkedTargetType, PIC, !IO),
    maybe_pic_object_file_extension(PIC, Obj, !IO),
    string__append(BaseName, ".c", C_File),
    string__append(BaseName, Obj, O_File),
    compile_target_code__compile_c_file(ErrorStream, PIC, C_File, O_File,
        Succeeded, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The MLDS backend

:- pred mlds_backend(module_info::in, module_info::out, mlds::out,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

mlds_backend(!HLDS, MLDS, !DumpInfo, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),

    simplify(no, ml_backend, Verbose, Stats,
        process_all_nonimported_nonaditi_procs, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 405, "ml_backend_simplify", !DumpInfo, !IO),

    maybe_add_trail_ops(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 410, "add_trail_ops", !DumpInfo, !IO),

    maybe_add_heap_ops(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 415, "add_heap_ops", !DumpInfo, !IO),

    maybe_mark_static_terms(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 420, "mark_static", !DumpInfo, !IO),

    % We need to do map_args_to_regs, even though that module is meant
    % for the LLDS back-end, because with the MLDS back-end the arg_infos
    % that map_args_to_regs generates are used by continuation_info.m,
    % which is used by ml_unify_gen.m when outputting closure layout structs.
    map_args_to_regs(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 425, "args_to_regs", !DumpInfo, !IO),

    maybe_dump_hlds(!.HLDS, 499, "final", !DumpInfo, !IO),

    maybe_write_string(Verbose, "% Converting HLDS to MLDS...\n", !IO),
    ml_code_gen(!.HLDS, MLDS0, !IO),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO),
    maybe_dump_mlds(MLDS0, 0, "initial", !IO),

    maybe_write_string(Verbose, "% Generating RTTI data...\n", !IO),
    mlds_gen_rtti_data(!.HLDS, MLDS0, MLDS10),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO),
    maybe_dump_mlds(MLDS10, 10, "rtti", !IO),

    % Detection of tail calls needs to occur before the
    % chain_gc_stack_frame pass of ml_elim_nested,
    % because we need to unlink the stack frame from the
    % stack chain before tail calls.
    globals__io_lookup_bool_option(optimize_tailcalls, OptimizeTailCalls, !IO),
    (
        OptimizeTailCalls = yes,
        maybe_write_string(Verbose, "% Detecting tail calls...\n", !IO),
        ml_mark_tailcalls(MLDS10, MLDS20, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        OptimizeTailCalls = no,
        MLDS10 = MLDS20
    ),
    maybe_report_stats(Stats, !IO),
    maybe_dump_mlds(MLDS20, 20, "tailcalls", !IO),

    % Warning about non-tail calls needs to come after detection
    % of tail calls
    globals__io_lookup_bool_option(warn_non_tail_recursion, WarnTailCalls, !IO),
    (
        OptimizeTailCalls = yes,
        WarnTailCalls = yes
    ->
        maybe_write_string(Verbose,
            "% Warning about non-tail recursive calls...\n", !IO),
        ml_warn_tailcalls(MLDS20, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        true
    ),
    maybe_report_stats(Stats, !IO),

    % run the ml_optimize pass before ml_elim_nested,
    % so that we eliminate as many local variables as possible
    % before the ml_elim_nested transformations.
    % We also want to do tail recursion optimization before
    % ml_elim_nested, since this means that the stack-handling
    % code for accurate GC will go outside the loop rather than
    % inside the loop.
    %
    % However, we need to disable optimize_initializations,
    % because ml_elim_nested doesn't correctly handle
    % code containing initializations.
    globals__io_lookup_bool_option(optimize, Optimize, !IO),
    (
        Optimize = yes,
        globals__io_lookup_bool_option(optimize_initializations,
            OptimizeInitializations, !IO),
        globals__io_set_option(optimize_initializations, bool(no), !IO),
        maybe_write_string(Verbose, "% Optimizing MLDS...\n", !IO),
        ml_optimize__optimize(MLDS20, MLDS25, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),

        globals__io_set_option(optimize_initializations,
            bool(OptimizeInitializations), !IO)
    ;
        Optimize = no,
        MLDS25 = MLDS20
    ),
    maybe_report_stats(Stats, !IO),
    maybe_dump_mlds(MLDS25, 25, "optimize1", !IO),

    %
    % Note that we call ml_elim_nested twice --
    % the first time to chain the stack frames together, for accurate GC,
    % and the second time to flatten nested functions.
    % These two passes are quite similar,
    % but must be done separately.
    % Currently chaining the stack frames together for accurate GC
    % needs to be done first, because the code for doing that
    % can't handle the env_ptr references that the other pass
    % generates.
    %

    globals__io_get_gc_method(GC, !IO),
    ( GC = accurate ->
        maybe_write_string(Verbose,
            "% Threading GC stack frames...\n", !IO),
        ml_elim_nested(chain_gc_stack_frames, MLDS25, MLDS30, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        MLDS30 = MLDS25
    ),
    maybe_report_stats(Stats, !IO),
    maybe_dump_mlds(MLDS30, 30, "gc_frames", !IO),

    globals__io_lookup_bool_option(gcc_nested_functions, NestedFuncs, !IO),
    (
        NestedFuncs = no,
        maybe_write_string(Verbose, "% Flattening nested functions...\n", !IO),
        ml_elim_nested(hoist_nested_funcs, MLDS30, MLDS35, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        NestedFuncs = yes,
        MLDS35 = MLDS30
    ),
    maybe_report_stats(Stats, !IO),
    maybe_dump_mlds(MLDS35, 35, "nested_funcs", !IO),

    % run the ml_optimize pass again after ml_elim_nested,
    % to do optimize_initializations.  (It may also help pick
    % up some additional optimization opportunities for the
    % other optimizations in this pass.)
    (
        Optimize = yes,
        maybe_write_string(Verbose, "% Optimizing MLDS again...\n", !IO),
        ml_optimize__optimize(MLDS35, MLDS40, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        Optimize = no,
        MLDS40 = MLDS35
    ),
    maybe_report_stats(Stats, !IO),
    maybe_dump_mlds(MLDS40, 40, "optimize2", !IO),

    MLDS = MLDS40,
    maybe_dump_mlds(MLDS, 99, "final", !IO).

:- pred mlds_gen_rtti_data(module_info::in, mlds::in, mlds::out) is det.

mlds_gen_rtti_data(HLDS, MLDS0, MLDS) :-
    type_ctor_info__generate_rtti(HLDS, TypeCtorRtti),
    base_typeclass_info__generate_rtti(HLDS, TypeClassInfoRtti),

    generate_aditi_proc_info(HLDS, AditiProcInfoRtti),
    module_info_globals(HLDS, Globals),
    globals__lookup_bool_option(Globals, new_type_class_rtti, NewTypeClassRtti),
    type_class_info__generate_rtti(HLDS, NewTypeClassRtti,
        NewTypeClassInfoRttiData),
    list__condense([TypeCtorRtti, TypeClassInfoRtti,
        NewTypeClassInfoRttiData, AditiProcInfoRtti], RttiData),
    RttiDefns = rtti_data_list_to_mlds(HLDS, RttiData),
    MLDS0 = mlds(ModuleName, ForeignCode, Imports, Defns0, InitPreds,
        FinalPreds),
    list__append(RttiDefns, Defns0, Defns),
    MLDS = mlds(ModuleName, ForeignCode, Imports, Defns, InitPreds, FinalPreds).

% The `--high-level-C' MLDS output pass

:- pred mlds_to_high_level_c(mlds::in, maybe(rl_file)::in,
    io::di, io::uo) is det.

mlds_to_high_level_c(MLDS, MaybeRLFile, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),

    maybe_write_string(Verbose, "% Converting MLDS to C...\n", !IO),
    mlds_to_c__output_mlds(MLDS, MaybeRLFile, "", !IO),
    maybe_write_string(Verbose, "% Finished converting MLDS to C.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred mlds_to_java(mlds::in, io::di, io::uo) is det.

mlds_to_java(MLDS, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),

    maybe_write_string(Verbose, "% Converting MLDS to Java...\n", !IO),
    mlds_to_java__output_mlds(MLDS, !IO),
    maybe_write_string(Verbose, "% Finished converting MLDS to Java.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred maybe_mlds_to_gcc(mlds::in, maybe(rl_file)::in, bool::out,
    io::di, io::uo) is det.

maybe_mlds_to_gcc(MLDS, MaybeRLFile, ContainsCCode, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),

    maybe_write_string(Verbose,
        "% Passing MLDS to GCC and compiling to assembler...\n", !IO),
    maybe_mlds_to_gcc__compile_to_asm(MLDS, MaybeRLFile, ContainsCCode, !IO),
    maybe_write_string(Verbose, "% Finished compiling to assembler.\n", !IO),
    maybe_report_stats(Stats, !IO).

:- pred mlds_to_il_assembler(mlds::in, io::di, io::uo) is det.

mlds_to_il_assembler(MLDS, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),

    maybe_write_string(Verbose, "% Converting MLDS to IL...\n", !IO),
    mlds_to_ilasm__output_mlds(MLDS, !IO),
    maybe_write_string(Verbose, "% Finished converting MLDS to IL.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type dump_info
    --->    no_prev_dump
    ;       prev_dumped_hlds(string, module_info).

:- pred maybe_dump_hlds(module_info::in, int::in, string::in,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

maybe_dump_hlds(HLDS, StageNum, StageName, !DumpInfo, !IO) :-
    globals__io_lookup_accumulating_option(dump_hlds, DumpStages, !IO),
    StageNumStr = stage_num_str(StageNum),
    ( should_dump_stage(StageNum, StageNumStr, StageName, DumpStages) ->
        module_info_name(HLDS, ModuleName),
        module_name_to_file_name(ModuleName, ".hlds_dump", yes, BaseFileName,
            !IO),
        DumpFileName = BaseFileName ++ "." ++ StageNumStr ++ "-" ++ StageName,
        (
            !.DumpInfo = prev_dumped_hlds(PrevDumpFileName, PrevHLDS),
            HLDS = PrevHLDS
        ->
            CurDumpFileName = PrevDumpFileName,
            io__open_output(DumpFileName, Res, !IO),
            ( Res = ok(FileStream) ->
                io__write_string(FileStream, "This stage is identical " ++
                    "to the stage in " ++ PrevDumpFileName ++ ".\n", !IO),
                io__close_output(FileStream, !IO)
            ;
                globals__io_lookup_bool_option(verbose, Verbose, !IO),
                maybe_write_string(Verbose, "\n", !IO),
                Msg = "can't open file `" ++ DumpFileName ++ "' for output.",
                report_error(Msg, !IO)
            )
        ;
            dump_hlds(DumpFileName, HLDS, !IO),
            CurDumpFileName = DumpFileName
        ),
        !:DumpInfo = prev_dumped_hlds(CurDumpFileName, HLDS)
    ;
        true
    ).

:- func stage_num_str(int) = string.

stage_num_str(StageNum) = StageNumStr :-
    int_to_string(StageNum, StageNumStr0),
    ( string__length(StageNumStr0, 1) ->
        StageNumStr = "00" ++ StageNumStr0
    ; string__length(StageNumStr0, 2) ->
        StageNumStr = "0" ++ StageNumStr0
    ;
        StageNumStr = StageNumStr0
    ).

:- pred should_dump_stage(int::in, string::in, string::in, list(string)::in)
    is semidet.

should_dump_stage(StageNum, StageNumStr, StageName, DumpStages) :-
    list__member(DumpStage, DumpStages),
    (
        StageName = DumpStage
    ;
        "all" = DumpStage
    ;
        (
            DumpStage = StageNumStr
        ;
            string__append("0", DumpStage, StageNumStr)
        ;
            string__append("00", DumpStage, StageNumStr)
        )
    ;
        string__append(From, "+", DumpStage),
        string__to_int(From, FromInt),
        StageNum >= FromInt
    ).

:- pred dump_hlds(string::in, module_info::in, io::di, io::uo) is det.

dump_hlds(DumpFile, HLDS, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),
    maybe_write_string(Verbose, "% Dumping out HLDS to `", !IO),
    maybe_write_string(Verbose, DumpFile, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    io__open_output(DumpFile, Res, !IO),
    ( Res = ok(FileStream) ->
        io__set_output_stream(FileStream, OutputStream, !IO),
        hlds_out__write_hlds(0, HLDS, !IO),
        io__set_output_stream(OutputStream, _, !IO),
        io__close_output(FileStream, !IO),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        maybe_write_string(Verbose, "\n", !IO),
        Msg = "can't open file `" ++ DumpFile ++ "' for output.",
        report_error(Msg, !IO)
    ).

:- pred maybe_dump_mlds(mlds::in, int::in, string::in, io::di, io::uo) is det.

maybe_dump_mlds(MLDS, StageNum, StageName, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_accumulating_option(dump_mlds, DumpStages, !IO),
    globals__io_lookup_accumulating_option(verbose_dump_mlds,
        VerboseDumpStages, !IO),
    StageNumStr = stage_num_str(StageNum),
    ( should_dump_stage(StageNum, StageNumStr, StageName, DumpStages) ->
        maybe_write_string(Verbose, "% Dumping out MLDS as C...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        string__append_list(["_dump.", StageNumStr, "-", StageName],
            DumpSuffix),
        mlds_to_c__output_mlds(MLDS, no, DumpSuffix, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        true
    ),
    ( should_dump_stage(StageNum, StageNumStr, StageName, VerboseDumpStages) ->
        maybe_write_string(Verbose, "% Dumping out raw MLDS...\n", !IO),
        ModuleName = mlds__get_module_name(MLDS),
        module_name_to_file_name(ModuleName, ".mlds_dump", yes, BaseFileName,
            !IO),
        string__append_list([BaseFileName, ".", StageNumStr, "-", StageName],
            DumpFile),
        dump_mlds(DumpFile, MLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        true
    ).

:- pred dump_mlds(string::in, mlds::in, io::di, io::uo) is det.

dump_mlds(DumpFile, MLDS, !IO) :-
    globals__io_lookup_bool_option(verbose, Verbose, !IO),
    globals__io_lookup_bool_option(statistics, Stats, !IO),
    maybe_write_string(Verbose, "% Dumping out MLDS to `", !IO),
    maybe_write_string(Verbose, DumpFile, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),
    io__open_output(DumpFile, Res, !IO),
    ( Res = ok(FileStream) ->
        io__set_output_stream(FileStream, OutputStream, !IO),
        pprint__write(80, pprint__to_doc(MLDS), !IO),
        io__nl(!IO),
        io__set_output_stream(OutputStream, _, !IO),
        io__close_output(FileStream, !IO),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        maybe_write_string(Verbose, "\n", !IO),
        string__append_list(["can't open file `", DumpFile, "' for output."],
            ErrorMessage),
        report_error(ErrorMessage, !IO)
    ).

:- pred maybe_dump_rl(list(rl_proc)::in, module_info::in,
    string::in, string::in, io::di, io::uo) is det.

maybe_dump_rl(Procs, ModuleInfo, _StageNum, StageName, !IO) :-
    globals__io_lookup_bool_option(dump_rl, Dump, !IO),
    (
        Dump = yes,
        module_info_name(ModuleInfo, ModuleName0),
        mdbcomp__prim_data__sym_name_to_string(ModuleName0, ModuleName),
        string__append_list([ModuleName, ".rl_dump", StageName], DumpFile),
        globals__io_lookup_bool_option(verbose, Verbose, !IO),
        maybe_write_string(Verbose, "% Dumping out RL to `", !IO),
        maybe_write_string(Verbose, DumpFile, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        maybe_flush_output(Verbose, !IO),
        io__open_output(DumpFile, Res, !IO),
        ( Res = ok(FileStream) ->
            io__set_output_stream(FileStream, OutputStream, !IO),
            list__foldl(rl_dump__write_procedure(ModuleInfo), Procs, !IO),
            io__set_output_stream(OutputStream, _, !IO),
            io__close_output(FileStream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            maybe_write_string(Verbose, "\n", !IO),
            string__append_list(["can't open file `", DumpFile,
                "' for output."], ErrorMessage),
            report_error(ErrorMessage, !IO)
        )
    ;
        Dump = no
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
