%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: handle_options.m.
% Main authors: fjh, zs.
%
% This module does post-processing on the command-line options, after
% getopt has done its stuff.
%
% It also contains code for handling the --grade option.
%
%-----------------------------------------------------------------------------%

:- module libs.handle_options.
:- interface.

:- import_module libs.globals.
:- import_module libs.options.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Generate a dummy globals value based on the default values of the
    % options.
    %
:- pred generate_default_globals(globals::out, io::di, io::uo) is det.

    % handle_given_options(Args, OptionArgs, NonOptionArgs, Link,
    %   Errors, Globals, !IO).
    %
    % This predicate does NOT modify the exit status.
    %
:- pred handle_given_options(list(string)::in,
    list(string)::out, list(string)::out, bool::out, list(string)::out,
    globals::out, io::di, io::uo) is det.

    % separate_option_args(Args, OptionArgs, NonOptionArgs, !IO):
    %
    % Separate the list of arguments into option and non-option arguments.
    %
:- pred separate_option_args(list(string)::in,
    list(string)::out, list(string)::out, io::di, io::uo) is det.

    % Display the compiler version.
    %
:- pred display_compiler_version(io::di, io::uo) is det.

    % usage_error(Descr, Message)
    %
    % Display given list of error messages and then the usage message.
:- pred usage_errors(list(string)::in, io::di, io::uo) is det.

    % Display usage message.
    %
:- pred usage(io::di, io::uo) is det.

    % Display long usage message for help.
    %
:- pred long_usage(io::di, io::uo) is det.

    % Given the current set of options, figure out which grade to use.
    %
:- pred compute_grade(globals::in, string::out) is det.

    % The inverse of compute_grade: given a grade, set the appropriate options.
    %
:- pred convert_grade_option(string::in, option_table::in, option_table::out)
    is semidet.

    % Produce the grade component of grade-specific installation directories.
    %
:- pred grade_directory_component(globals::in, string::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module libs.compiler_util.
:- import_module libs.trace_params.
:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module char.
:- import_module dir.
:- import_module getopt_io.
:- import_module int.
:- import_module library.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

generate_default_globals(DefaultGlobals, !IO) :-
    handle_given_options([], _, _, _, _, DefaultGlobals, !IO).

handle_given_options(Args0, OptionArgs, Args, Link, Errors, !:Globals, !IO) :-
    % io.write_string("original arguments\n", !IO),
    % dump_arguments(Args0, !IO),
    process_given_options(Args0, OptionArgs, Args, Result, !IO),
    % io.write_string("final arguments\n", !IO),
    % dump_arguments(Args, !IO),
    convert_option_table_result_to_globals(Result, Errors, !:Globals, !IO),
    (
        Errors = [_ | _],
        Link = no
        % Do NOT set the exit status.  This predicate may be called before all
        % the options are known, so the errors may not be valid.
    ;
        Errors = [],
        globals.lookup_bool_option(!.Globals, generate_dependencies,
            GenerateDependencies),
        globals.lookup_bool_option(!.Globals, generate_dependency_file,
            GenerateDependencyFile),
        globals.lookup_bool_option(!.Globals, make_interface, MakeInterface),
        globals.lookup_bool_option(!.Globals, make_private_interface,
            MakePrivateInterface),
        globals.lookup_bool_option(!.Globals, make_short_interface,
            MakeShortInterface),
        globals.lookup_bool_option(!.Globals, make_optimization_interface,
            MakeOptimizationInt),
        globals.lookup_bool_option(!.Globals, make_transitive_opt_interface,
            MakeTransOptInt),
        globals.lookup_bool_option(!.Globals, make_analysis_registry,
            MakeAnalysisRegistry),
        globals.lookup_bool_option(!.Globals, make_xml_documentation,
            MakeXmlDocumentation),
        globals.lookup_bool_option(!.Globals, convert_to_mercury,
            ConvertToMercury),
        globals.lookup_bool_option(!.Globals, typecheck_only, TypecheckOnly),
        globals.lookup_bool_option(!.Globals, errorcheck_only, ErrorcheckOnly),
        globals.lookup_bool_option(!.Globals, target_code_only,
            TargetCodeOnly),
        globals.get_target(!.Globals, Target),
        GenerateIL = (if Target = target_il then yes else no),
        globals.lookup_bool_option(!.Globals, compile_only, CompileOnly),
        bool.or_list([GenerateDependencies, GenerateDependencyFile,
            MakeInterface, MakePrivateInterface, MakeShortInterface,
            MakeOptimizationInt, MakeTransOptInt, MakeAnalysisRegistry,
            MakeXmlDocumentation, ConvertToMercury, TypecheckOnly,
            ErrorcheckOnly, TargetCodeOnly, GenerateIL, CompileOnly],
            NotLink),
        bool.not(NotLink, Link),
        globals.lookup_bool_option(!.Globals, smart_recompilation, Smart),
        (
            Smart = yes,
            Link = yes
        ->
            % XXX Currently smart recompilation doesn't check that all the
            % files needed to link are present and up-to-date, so disable it.
            disable_smart_recompilation("linking", !Globals, !IO)
        ;
            true
        )
    ).

separate_option_args(Args0, OptionArgs, Args, !IO) :-
    process_given_options(Args0, OptionArgs, Args, _, !IO).

    % process_given_options(Args, OptionArgs, NonOptionArgs, MaybeOptionTable,
    %   !IO):
    %
    % Process the options, but don't do any post-processing.
    % This is mainly
    % useful for separating the list of arguments into option and non-option
    % arguments.
    %
:- pred process_given_options(list(string)::in,
    list(string)::out, list(string)::out, maybe_option_table(option)::out,
    io::di, io::uo) is det.

process_given_options(Args0, OptionArgs, Args, Result, !IO) :-
    OptionOps = option_ops(short_option, long_option,
        option_defaults, special_handler),
    getopt_io.process_options(OptionOps, Args0, OptionArgs, Args, Result,
        !IO).

:- pred dump_arguments(list(string)::in, io::di, io::uo) is det.

dump_arguments([], !IO).
dump_arguments([Arg | Args], !IO) :-
    io.write_string("<", !IO),
    io.write_string(Arg, !IO),
    io.write_string(">\n", !IO),
    dump_arguments(Args, !IO).

%-----------------------------------------------------------------------------%

    % Convert string-valued options into the appropriate enumeration types,
    % and process implications among the options (i.e. situations where setting
    % one option implies setting/unsetting another one).
    %
:- pred convert_option_table_result_to_globals(maybe_option_table(option)::in,
    list(string)::out, globals::out, io::di, io::uo) is det.

convert_option_table_result_to_globals(error(ErrorMessage), [ErrorMessage],
        Globals, !IO) :-
    generate_default_globals(Globals, !IO).
convert_option_table_result_to_globals(ok(OptionTable0), Errors,
        Globals, !IO) :-
    check_option_values(OptionTable0, OptionTable, Target, GC_Method,
        TagsMethod, TermNorm, Term2Norm, TraceLevel, TraceSuppress,
        SSTraceLevel, MaybeThreadSafe, C_CompilerType, CSharp_CompilerType,
        ReuseStrategy, MaybeILVersion,
        MaybeFeedbackInfo, HostEnvType, SystemEnvType, TargetEnvType,
        [], CheckErrors, !IO),
    (
        CheckErrors = [],
        convert_options_to_globals(OptionTable, Target, GC_Method,
            TagsMethod, TermNorm, Term2Norm, TraceLevel,
            TraceSuppress, SSTraceLevel, MaybeThreadSafe, C_CompilerType,
            CSharp_CompilerType, ReuseStrategy,
            MaybeILVersion, MaybeFeedbackInfo,
            HostEnvType, SystemEnvType, TargetEnvType,
            [], Errors, Globals, !IO)
    ;
        CheckErrors = [_ | _],
        Errors = CheckErrors,
        generate_default_globals(Globals, !IO)
    ).

:- pred check_option_values(option_table::in, option_table::out,
    compilation_target::out, gc_method::out, tags_method::out,
    termination_norm::out, termination_norm::out, trace_level::out,
    trace_suppress_items::out, ssdb_trace_level::out, may_be_thread_safe::out,
    c_compiler_type::out, csharp_compiler_type::out,
    reuse_strategy::out, maybe(il_version_number)::out,
    maybe(feedback_info)::out, env_type::out, env_type::out, env_type::out,
    list(string)::in, list(string)::out, io::di, io::uo) is det.

check_option_values(!OptionTable, Target, GC_Method, TagsMethod,
        TermNorm, Term2Norm, TraceLevel, TraceSuppress, SSTraceLevel,
        MaybeThreadSafe, C_CompilerType, CSharp_CompilerType,
        ReuseStrategy, MaybeILVersion, MaybeFeedbackInfo,
        HostEnvType, SystemEnvType, TargetEnvType, !Errors, !IO) :-
    map.lookup(!.OptionTable, target, Target0),
    (
        Target0 = string(TargetStr),
        convert_target(TargetStr, TargetPrime)
    ->
        Target = TargetPrime
    ;
        Target = target_c,     % dummy
        % XXX When the x86_64 backend is documented modify the line below.
        add_error("Invalid target option " ++
            "(must be `c', `il', `java', 'csharp', or `erlang')",
            !Errors)
    ),

    map.lookup(!.OptionTable, gc, GC_Method0),
    (
        GC_Method0 = string(GC_MethodStr),
        convert_gc_method(GC_MethodStr, GC_MethodPrime)
    ->
        GC_Method = GC_MethodPrime
    ;
        GC_Method = gc_none,   % dummy
        add_error("Invalid GC option (must be `none', " ++
            "`conservative', `boehm', `hgc', `mps', `accurate', " ++
            "or `automatic')",
            !Errors)
    ),

    map.lookup(!.OptionTable, tags, TagsMethod0),
    (
        TagsMethod0 = string(TagsMethodStr),
        convert_tags_method(TagsMethodStr, TagsMethodPrime)
    ->
        TagsMethod = TagsMethodPrime
    ;
        TagsMethod = tags_none,  % dummy
        add_error("Invalid tags option " ++
            "(must be `none', `low' or `high')", !Errors)
    ),

    map.lookup(!.OptionTable, fact_table_hash_percent_full, PercentFull),
    (
        PercentFull = int(Percent),
        Percent >= 1,
        Percent =< 100
    ->
        true
    ;
        add_error("Invalid argument to option " ++
            "`--fact-table-hash-percent-full'\n\t" ++
            "(must be an integer between 1 and 100)", !Errors)
    ),

    map.lookup(!.OptionTable, termination_norm, TermNorm0),
    (
        TermNorm0 = string(TermNormStr),
        convert_termination_norm(TermNormStr, TermNormPrime)
    ->
        TermNorm = TermNormPrime
    ;
        TermNorm = norm_simple,  % dummy
        add_error("Invalid argument to option " ++
            "`--termination-norm'\n\t(must be " ++
            "`simple', `total' or `num-data-elems').", !Errors)
    ),

    map.lookup(!.OptionTable, termination2_norm, Term2Norm0),
    (
        Term2Norm0 = string(Term2NormStr),
        convert_termination_norm(Term2NormStr, Term2NormPrime)
    ->
        Term2Norm = Term2NormPrime
    ;
        Term2Norm = norm_simple, % dummy
        add_error("Invalid argument to option " ++
            "`--termination2-norm'\n\t(must be" ++
            "`simple', `total' or `num-data-elems').", !Errors)
    ),

    map.lookup(!.OptionTable, force_disable_tracing, ForceDisableTracing),
    ( ForceDisableTracing = bool(yes) ->
        TraceLevel = trace_level_none
    ;
        map.lookup(!.OptionTable, trace_level, Trace),
        map.lookup(!.OptionTable, exec_trace, ExecTraceOpt),
        map.lookup(!.OptionTable, decl_debug, DeclDebugOpt),
        (
            Trace = string(TraceStr),
            ExecTraceOpt = bool(ExecTrace),
            DeclDebugOpt = bool(DeclDebug),
            convert_trace_level(TraceStr, ExecTrace, DeclDebug,
                MaybeTraceLevel)
        ->
            (
                MaybeTraceLevel = yes(TraceLevel)
            ;
                MaybeTraceLevel = no,
                TraceLevel = trace_level_none,  % dummy
                add_error("Specified trace level is not " ++
                    "compatible with grade", !Errors)
            )
        ;
            TraceLevel = trace_level_none,  % dummy
            add_error("Invalid argument to option `--trace'\n\t" ++
                "(must be `minimum', `shallow', `deep', " ++
                "`decl', `rep' or `default').", !Errors)
        )
    ),

    map.lookup(!.OptionTable, suppress_trace, Suppress),
    (
        Suppress = string(SuppressStr),
        convert_trace_suppress(SuppressStr, TraceSuppressPrime)
    ->
        TraceSuppress = TraceSuppressPrime
    ;
        TraceSuppress = default_trace_suppress, % dummy
        add_error("Invalid argument to option `--suppress-trace'.", !Errors)
    ),

    map.lookup(!.OptionTable, force_disable_ssdebug, ForceDisableSSDB),
    ( ForceDisableSSDB = bool(yes) ->
        SSTraceLevel = none
    ;
        map.lookup(!.OptionTable, ssdb_trace_level, SSTrace),
        map.lookup(!.OptionTable, source_to_source_debug, SSDB),
        ( 
            SSTrace = string(SSTraceStr),
            SSDB = bool(IsInSSDebugGrade),
            convert_ssdb_trace_level(SSTraceStr, IsInSSDebugGrade, SSTL)
        ->
            SSTraceLevel = SSTL
        ;
            SSTraceLevel = none,
            add_error("Invalid argument to option `--ssdb-trace'.", !Errors)
        )
    ),

    map.lookup(!.OptionTable, maybe_thread_safe_opt, MaybeThreadSafeOption),
    (
        MaybeThreadSafeOption = string(MaybeThreadSafeString),
        convert_maybe_thread_safe(MaybeThreadSafeString, MaybeThreadSafe0)
    ->
        MaybeThreadSafe = MaybeThreadSafe0
    ;
        MaybeThreadSafe = no, % dummy
        add_error("Invalid argument to option `--maybe-thread-safe'.", !Errors)
    ),

    map.lookup(!.OptionTable, dump_hlds_alias, DumpAliasOption),
    (
        DumpAliasOption = string(DumpAlias),
        DumpAlias = ""
    ->
        true
    ;
        DumpAliasOption = string(DumpAlias),
        convert_dump_alias(DumpAlias, AliasDumpOptions)
    ->
        map.set(dump_hlds_options, string(AliasDumpOptions), !OptionTable)
    ;
        add_error("Invalid argument to option `--hlds-dump-alias'.", !Errors)
    ),

    some [!DumpOptions] (
        lookup_string_option(!.OptionTable, dump_hlds_options, !:DumpOptions),

        % If we have not specified what we want, dump bare procedures
        % as default.
        ( !.DumpOptions = "" ->
            !:DumpOptions = "x"
        ;
            true
        ),

        % If we want structured insts in arg-modes, then we want arg-modes.
        (
            string.contains_char(!.DumpOptions, 'y'),
            not string.contains_char(!.DumpOptions, 'a')
        ->
            !:DumpOptions = "a" ++ !.DumpOptions
        ;
            true
        ),
        % If we want arg-modes, then we want the unifications they apply to.
        (
            string.contains_char(!.DumpOptions, 'a'),
            not string.contains_char(!.DumpOptions, 'u')
        ->
            !:DumpOptions = "u" ++ !.DumpOptions
        ;
            true
        ),
        % If we want any of the things that decorate predicates or the goals
        % inside predicates, then we want the predicates themselves.
        (
            ( string.contains_char(!.DumpOptions, 'A')
            ; string.contains_char(!.DumpOptions, 'B')
            ; string.contains_char(!.DumpOptions, 'D')
            ; string.contains_char(!.DumpOptions, 'G')
            ; string.contains_char(!.DumpOptions, 'P')
            ; string.contains_char(!.DumpOptions, 'R')
            ; string.contains_char(!.DumpOptions, 'S')
            ; string.contains_char(!.DumpOptions, 'b')
            ; string.contains_char(!.DumpOptions, 'c')
            ; string.contains_char(!.DumpOptions, 'd')
            ; string.contains_char(!.DumpOptions, 'f')
            ; string.contains_char(!.DumpOptions, 'g')
            ; string.contains_char(!.DumpOptions, 'i')
            ; string.contains_char(!.DumpOptions, 'l')
            ; string.contains_char(!.DumpOptions, 'm')
            ; string.contains_char(!.DumpOptions, 'n')
            ; string.contains_char(!.DumpOptions, 'p')
            ; string.contains_char(!.DumpOptions, 's')
            ; string.contains_char(!.DumpOptions, 't')
            ; string.contains_char(!.DumpOptions, 'u')
            ; string.contains_char(!.DumpOptions, 'z')
            ),
            not string.contains_char(!.DumpOptions, 'x')
        ->
            !:DumpOptions = "x" ++ !.DumpOptions
        ;
            true
        ),
        map.set(dump_hlds_options, string(!.DumpOptions), !OptionTable)
    ),

    map.lookup(!.OptionTable, c_compiler_type, C_CompilerType0),
    (
        C_CompilerType0 = string(C_CompilerTypeStr),
        convert_c_compiler_type(C_CompilerTypeStr, C_CompilerTypePrime)
    ->
        C_CompilerType = C_CompilerTypePrime
    ;
        C_CompilerType = cc_unknown,   % dummy
        add_error("Invalid argument to option `--c-compiler-type'\n" ++
            "\t(must be `gcc', `lcc', `clang', 'msvc', or `unknown').",
            !Errors)
    ),

    map.lookup(!.OptionTable, csharp_compiler_type, CSharp_CompilerType0),
    (
        CSharp_CompilerType0 = string(CSharp_CompilerTypeStr),
        convert_csharp_compiler_type(CSharp_CompilerTypeStr,
            CSharp_CompilerTypePrime)
    ->
        CSharp_CompilerType = CSharp_CompilerTypePrime
    ;
        CSharp_CompilerType = csharp_unknown,   % dummy
        add_error("Invalid argument to option `--csharp-compiler-type'\n" ++
            "\t(must be `microsoft', `mono', or `unknown').",
            !Errors),
        true
    ),

    map.lookup(!.OptionTable, structure_reuse_constraint, ReuseConstraint0),
    map.lookup(!.OptionTable, structure_reuse_constraint_arg,
        ReuseConstraintArg0),
    (
        ReuseConstraint0 = string(ReuseConstraintStr),
        ReuseConstraintArg0 = int(ReuseConstraintArgNum),
        convert_reuse_strategy(ReuseConstraintStr, ReuseConstraintArgNum,
            ReuseStrategyPrime)
    ->
        ReuseStrategy = ReuseStrategyPrime
    ;
        ReuseStrategy = same_cons_id,   % dummy
        add_error(
            "Invalid argument to option `--structure-reuse-constraint'\n" ++
            "\t(must be `same_cons_id' or `within_n_cells_difference').",
            !Errors)
    ),

    map.lookup(!.OptionTable, dotnet_library_version, DotNetLibVersionOpt),
    (
        DotNetLibVersionOpt = string(DotNetLibVersionStr),
        IsSep = (pred(('.')::in) is semidet),
        string.words_separator(IsSep, DotNetLibVersionStr) = [Mj, Mn, Bu, Rv],
        string.to_int(Mj, Major),
        string.to_int(Mn, Minor),
        string.to_int(Bu, Build),
        string.to_int(Rv, Revision)
    ->
        ILVersion = il_version_number(Major, Minor, Build, Revision),
        MaybeILVersion = yes(ILVersion)
    ;
        MaybeILVersion = no,
        add_error("Invalid argument to " ++
            "option `--dotnet-library-version'\n" ++
            "\t(must be of the form " ++
            "`MajorNum.MinorNum.BuildNum.RevisionNum').",
            !Errors),
        % The IL code generator cannot handle the IL version being unknown.
        map.det_update(errorcheck_only, bool(yes), !OptionTable)
    ),

    map.lookup(!.OptionTable, feedback_file, FeedbackFile0),
    (
        FeedbackFile0 = string(FeedbackFile),
        FeedbackFile \= ""
    ->
        read_feedback_file(FeedbackFile, FeedbackReadResult, !IO),
        (
            FeedbackReadResult = ok(FeedbackInfo),
            MaybeFeedbackInfo = yes(FeedbackInfo)
        ;
            FeedbackReadResult = error(Error),
            read_error_message_string(FeedbackFile, Error, ErrorMessage),
            add_error(ErrorMessage, !Errors),
            MaybeFeedbackInfo = no
        )
    ;
        % No feedback info.
        MaybeFeedbackInfo = no
    ),
    map.lookup(!.OptionTable, host_env_type, HostEnvType0),
    (
        HostEnvType0 = string(HostEnvTypeStr),
        convert_env_type(HostEnvTypeStr, HostEnvTypePrime)
    ->
        HostEnvType = HostEnvTypePrime
    ;
        HostEnvType = env_type_posix,   % dummy
        add_error(
            "Invalid argument to option `--host-env-type'\n" ++
            "\t(must be `posix', `cygwin', `msys' or `windows').",
            !Errors)
    ),
    map.lookup(!.OptionTable, system_env_type, SystemEnvType0),
    (
        SystemEnvType0 = string(SystemEnvTypeStr),
        ( if SystemEnvTypeStr = "" then
            SystemEnvTypePrime = HostEnvType
        else 
            convert_env_type(SystemEnvTypeStr, SystemEnvTypePrime) 
        )
    ->
        SystemEnvType = SystemEnvTypePrime
    ;
        SystemEnvType = env_type_posix,    % dummy
        add_error(
            "Invalid argument to option `--system-env-type'\n" ++
            "\t(must be `posix', `cygwin', `msys' or `windows').",
            !Errors)
    ),
    map.lookup(!.OptionTable, target_env_type, TargetEnvType0),
    (
        TargetEnvType0 = string(TargetEnvTypeStr),
        convert_env_type(TargetEnvTypeStr, TargetEnvTypePrime)
    ->
        TargetEnvType = TargetEnvTypePrime
    ;
        TargetEnvType = env_type_posix,   % dummy
        add_error(
            "Invalid argument to option `--target-env-type'\n" ++
            "\t(must be `posix', `cygwin', `msys' or `windows').",
            !Errors)
    ),

    ( HostEnvType = env_type_posix, CSharp_CompilerType = csharp_microsoft ->
        add_error(
            "`--host-env-type posix` is incompatible with\n" ++
            "`--csharp-compiler-type microsoft'.",
            !Errors)
    ;
        true
    ).

:- pred add_error(string::in, list(string)::in, list(string)::out) is det.

add_error(Error, Errors0, Errors) :-
    % We won't be appending enough errors for the quadratic complexity
    % of repeated appends to be a problem.
    list.append(Errors0, [Error], Errors).

    % NOTE: each termination analyser has its own norm setting.
    %
:- pred convert_options_to_globals(option_table::in,
    compilation_target::in, gc_method::in, tags_method::in,
    termination_norm::in, termination_norm::in, trace_level::in,
    trace_suppress_items::in, ssdb_trace_level::in, may_be_thread_safe::in,
    c_compiler_type::in, csharp_compiler_type::in,
    reuse_strategy::in, maybe(il_version_number)::in, maybe(feedback_info)::in,
    env_type::in, env_type::in, env_type::in,
    list(string)::in, list(string)::out,
    globals::out, io::di, io::uo) is det.

convert_options_to_globals(OptionTable0, Target, GC_Method, TagsMethod0,
        TermNorm, Term2Norm, TraceLevel, TraceSuppress, SSTraceLevel,
        MaybeThreadSafe, C_CompilerType, CSharp_CompilerType,
        ReuseStrategy, MaybeILVersion, MaybeFeedbackInfo,
        HostEnvType, SystemEnvType, TargetEnvType,
        !Errors, !:Globals, !IO) :-

    lookup_string_option(OptionTable0, install_command, InstallCmd),
    ( if InstallCmd = "" then
        FileInstallCmd = install_cmd_cp
    else
        lookup_string_option(OptionTable0, install_command_dir_option,
            InstallCmdDirOption),
        FileInstallCmd = install_cmd_user(InstallCmd,
            InstallCmdDirOption)
    ),

    globals_init(OptionTable0, Target, GC_Method, TagsMethod0,
        TermNorm, Term2Norm, TraceLevel, TraceSuppress, SSTraceLevel,
        MaybeThreadSafe, C_CompilerType, CSharp_CompilerType,
        ReuseStrategy, MaybeILVersion, MaybeFeedbackInfo,
        HostEnvType, SystemEnvType, TargetEnvType, FileInstallCmd,
        !:Globals),

    globals.lookup_string_option(!.Globals, event_set_file_name,
        EventSetFileName0),
    ( EventSetFileName0 = "" ->
        io.get_environment_var("MERCURY_EVENT_SET_FILE_NAME",
            MaybeEventSetFileName, !IO),
        (
            MaybeEventSetFileName = yes(EventSetFileName),
            globals.set_option(event_set_file_name,
                string(EventSetFileName), !Globals)
        ;
            MaybeEventSetFileName = no
        )
    ;
        true
    ),

    % Conservative GC implies --no-reclaim-heap-*
    ( gc_is_conservative(GC_Method) = yes ->
        globals.set_option(reclaim_heap_on_semidet_failure, bool(no),
            !Globals),
        globals.set_option(reclaim_heap_on_nondet_failure, bool(no),
            !Globals)
    ;
        true
    ),

    % --pregenerated-dist sets options so that the pre-generated C source
    % distribution can be compiled equally on 32-bit and 64-bit platforms.
    globals.lookup_bool_option(!.Globals, pregenerated_dist, PregeneratedDist),
    (
        PregeneratedDist = yes,
        globals.set_option(num_tag_bits, int(2), !Globals),
        globals.set_option(arg_pack_bits, int(32), !Globals),
        globals.set_option(unboxed_float, bool(no), !Globals),
        globals.set_option(single_prec_float, bool(no), !Globals),
        globals.set_option(allow_double_word_fields, bool(no), !Globals)
    ;
        PregeneratedDist = no
    ),

    % --tags none implies --num-tag-bits 0.
    (
        TagsMethod0 = tags_none,
        NumTagBits0 = 0
    ;
        ( TagsMethod0 = tags_low
        ; TagsMethod0 = tags_high
        ),
        globals.lookup_int_option(!.Globals, num_tag_bits, NumTagBits0)
    ),

    % If --tags low but --num-tag-bits not specified,
    % use the autoconf-determined value for --num-tag-bits
    % (the autoconf-determined value is passed from the `mc' script
    % using the undocumented --conf-low-tag-bits option).
    (
        TagsMethod0 = tags_low,
        NumTagBits0 = -1
    ->
        globals.lookup_int_option(!.Globals, conf_low_tag_bits,
            NumTagBits1)
    ;
        NumTagBits1 = NumTagBits0
    ),

    % If --num-tag-bits negative or unspecified, issue a warning
    % and assume --num-tag-bits 0.
    ( NumTagBits1 < 0 ->
        io.progname_base("mercury_compile", ProgName, !IO),
        io.format("%s: warning: --num-tag-bits invalid or unspecified\n",
            [s(ProgName)], !IO),
        io.format("%s: using --num-tag-bits 0 (tags disabled)\n",
            [s(ProgName)], !IO),
        record_warning(!.Globals, !IO),
        NumTagBits = 0
    ;
        NumTagBits = NumTagBits1
    ),

    globals.set_option(num_tag_bits, int(NumTagBits), !Globals),
    ( NumTagBits = 0 ->
        TagsMethod = tags_none,
        globals.set_tags_method(TagsMethod, !Globals)
    ;
        TagsMethod = TagsMethod0
    ),

    current_grade_supports_par_conj(!.Globals, GradeSupportsParConj),
    globals.lookup_bool_option(!.Globals, parallel, Parallel),
    globals.lookup_bool_option(!.Globals, threadscope, Threadscope),
    (
        GradeSupportsParConj = no,
        Threadscope = yes
    ->
        add_error("'threadscope' grade component requires a parallel grade",
            !Errors)
    ;
        true
    ),

    % Implicit parallelism requires feedback information, however this
    % error should only be shown in a parallel grade, otherwise implicit
    % parallelism should be disabled.
    globals.lookup_bool_option(!.Globals, implicit_parallelism,
        ImplicitParallelism),
    (
        ImplicitParallelism = yes,
        (
            GradeSupportsParConj = yes,
            globals.lookup_string_option(!.Globals, feedback_file,
                FeedbackFile),
            (
                FeedbackFile = ""
            ->
                add_error(
                    "'--implicit-parallelism' requires '--feedback-file'",
                    !Errors)
            ;
                true
            )
        ;
            % Report an error when used in parallel grades without parallel
            % conjunction support.  In non-parallel grades simply ignore
            % --implicit-parallelism.
            GradeSupportsParConj = no,
            (
                Parallel = yes,
                add_error(
                    "'--implicit-parallelism' requires a grade that " ++
                    "supports parallel conjunctions, use a low-level C " ++
                    "grade without trailing.",
                    !Errors)
            ;
                Parallel = no
            ),
            globals.set_option(implicit_parallelism, bool(no), !Globals)
        )
    ;
        ImplicitParallelism = no
    ),
    % Perform a simplification pass before the implicit parallelism pass to
    % ensure that the HLDS more-closely matches the feedback data.
    option_implies(implicit_parallelism, pre_implicit_parallelism_simplify,
        bool(yes), !Globals),

    % Loop control is not applicable in non-parallel grades.
    (
        GradeSupportsParConj = yes
    ;
        GradeSupportsParConj = no,
        globals.set_option(par_loop_control, bool(no), !Globals)
    ),

    % Generating IL implies:
    %   - gc_method `automatic' and no heap reclamation on failure
    %     Because GC is handled automatically by the .NET CLR
    %     implementation.
    %   - high-level code
    %     Because only the MLDS back-end supports
    %     compiling to IL, not the LLDS back-end.
    %   - high-level data
    %     Because it is more efficient,
    %     and better for interoperability.
    %     (In theory --low-level-data should work too,
    %     but there's no reason to bother supporting it.)
    %   - turning off nested functions
    %     Because IL doesn't support nested functions.
    %   - using copy-out for nondet output arguments
    %     For reasons explained in the paper "Compiling Mercury
    %     to the .NET Common Language Runtime"
    %   - using no tags
    %     Because IL doesn't provide any mechanism for tagging
    %     pointers.
    %   - boxing enums and disabling no_tag_types
    %     These are both required to ensure that we have a uniform
    %     representation (`object[]') for all data types,
    %     which is required to avoid type errors for code using
    %     abstract data types.
    %     XXX It should not be needed now that we have a general
    %     solution to the abstract equivalence type problem
    %     (intermodule optimization).
    %     But currently it is still needed, otherwise
    %         RTTI (e.g. construct, deconstruct) doesn't work
    %     for these types.
    %   - XXX it should also imply num_reserved_addresses = 1
    %     (we can use null pointers), but currently it doesn't,
    %     again because this causes problems with RTTI
    %   - no static ground terms
    %         XXX Previously static ground terms used to not work with
    %             --high-level-data. But this has been (mostly?) fixed now.
    %             So we should investigate re-enabling static ground terms.
    %         Currently mlds_to_il.m doesn't support them yet?
    %   - no library grade installation check with `mmc --make'.
    %   - cross compiling
    %     Because we use 32-bit integers which may be different to that of
    %     the host compiler.

    (
        Target = target_il,
        globals.set_gc_method(gc_automatic, !Globals),
        globals.set_option(gc, string("automatic"), !Globals),
        globals.set_option(reclaim_heap_on_nondet_failure, bool(no),
            !Globals),
        globals.set_option(reclaim_heap_on_semidet_failure, bool(no),
            !Globals),
        globals.set_option(highlevel_code, bool(yes), !Globals),
        globals.set_option(highlevel_data, bool(yes), !Globals),
        globals.set_option(gcc_nested_functions, bool(no), !Globals),
        globals.set_option(nondet_copy_out, bool(yes), !Globals),
        globals.set_option(num_tag_bits, int(0), !Globals),
        globals.set_option(unboxed_enums, bool(no), !Globals),
        globals.set_option(unboxed_no_tag_types, bool(no), !Globals),
        % globals.set_option(num_reserved_addresses, int(1), !Globals)
        globals.set_option(static_ground_cells, bool(no), !Globals),
        globals.set_option(libgrade_install_check, bool(no), !Globals),
        globals.set_option(cross_compiling, bool(yes), !Globals),

        % On the .NET backend we will be using a language independent
        % debugger not mdb.  Thus --debug has to imply --target-debug.
        ( given_trace_level_is_none(TraceLevel) = no ->
            globals.set_option(target_debug, bool(yes), !Globals)
        ;
            true
        )
    ;
        ( Target = target_c
        ; Target = target_csharp
        ; Target = target_java
        ; Target = target_x86_64
        ; Target = target_erlang
        )
    ),

    % Set --put-nondet-env-on-heap if --verifiable-code is specified,
    % unless both --il-funcptr-types and --il-refany-fields
    % are specified.
    globals.lookup_bool_option(!.Globals, il_funcptr_types,
        ILFuncPtrTypes),
    globals.lookup_bool_option(!.Globals, il_refany_fields,
        ILRefAnyFields),
    (
        ILFuncPtrTypes = yes,
        ILRefAnyFields = yes
    ->
        true
    ;
        option_implies(verifiable_code, put_nondet_env_on_heap, bool(yes),
            !Globals)
    ),

    % Generating Java implies
    %   - gc_method `automatic' and no heap reclamation on failure
    %     Because GC is handled automatically by the Java
    %     implementation.
    %   - high-level code
    %     Because only the MLDS back-end supports
    %     compiling to Java, not the LLDS back-end.
    %   - high-level data
    %     Because it is more efficient,
    %     and better for interoperability.
    %     (In theory --low-level-data should work too,
    %     but there's no reason to bother supporting it.)
    %   - unboxed floats
    %   - turning off nested functions
    %     Because Java doesn't support nested functions.
    %   - using copy-out for both det and nondet output arguments
    %     Because Java doesn't support pass-by-reference.
    %   - using no tags
    %     Because Java doesn't provide any mechanism for tagging
    %     pointers.
    %   - box no-tag types
    %         We require no-tag types to be boxed since in Java
    %         java.lang.Object is the only type that all other types
    %         can be successfully cast to and then cast back from.
    %   - store nondet environments on the heap
    %         Because Java has no way of allocating structs on the stack.
    %   - pretest-equality-cast-pointers
    %   - no library grade installation check with `mmc --make'.
    %   - cross compiling
    %     Because ints in Java are 32-bits wide which may be different to
    %     that of the host compiler.
    %
    % C# should be the same as Java, except that:
    %   - C# supports pass-by-reference, but for reasons explained in
    %     mlds_to_cs.m, we pretend it doesn't at the MLDS level

    (
        ( Target = target_java
        ; Target = target_csharp
        ),
        globals.set_gc_method(gc_automatic, !Globals),
        globals.set_option(gc, string("automatic"), !Globals),
        globals.set_option(reclaim_heap_on_nondet_failure, bool(no),
            !Globals),
        globals.set_option(reclaim_heap_on_semidet_failure, bool(no),
            !Globals),
        globals.set_option(highlevel_code, bool(yes), !Globals),
        globals.set_option(highlevel_data, bool(yes), !Globals),
        globals.set_option(unboxed_float, bool(yes), !Globals),
        globals.set_option(gcc_nested_functions, bool(no), !Globals),
        globals.set_option(nondet_copy_out, bool(yes), !Globals),
        globals.set_option(det_copy_out, bool(yes), !Globals),
        globals.set_option(num_tag_bits, int(0), !Globals),
        globals.set_option(unboxed_no_tag_types, bool(no), !Globals),
        globals.set_option(put_nondet_env_on_heap, bool(yes), !Globals),
        globals.set_option(pretest_equality_cast_pointers, bool(yes),
            !Globals),
        globals.set_option(libgrade_install_check, bool(no), !Globals),
        globals.set_option(cross_compiling, bool(yes), !Globals),
        (
            Target = target_csharp,
            globals.set_option(executable_file_extension, string(".exe"),
                !Globals)
        ;
            Target = target_java
        )
    ;
        ( Target = target_c
        ; Target = target_il
        ; Target = target_x86_64
        ; Target = target_erlang
        )
    ),

    % Generating Erlang implies
    %   - gc_method `automatic' and no heap reclamation on failure
    %     because GC is handled automatically by the Erlang
    %     implementation.
    %   - unboxed floats
    %   - delay-partial-instantiations
    %   - no-can-compare-constants-as-ints
    %   - can-compare-compound-values
    %   - lexically-compare-constructors
    %   - no library grade installation check with `mmc --make'
    %   - cross compiling
    %     Because Erlang has arbitrary precision integers which may
    %     different to that of the host compiler.

    (
        Target = target_erlang,
        globals.set_gc_method(gc_automatic, !Globals),
        globals.set_option(gc, string("automatic"), !Globals),
        globals.set_option(unboxed_float, bool(yes), !Globals),
        globals.set_option(reclaim_heap_on_nondet_failure, bool(no),
            !Globals),
        globals.set_option(reclaim_heap_on_semidet_failure, bool(no),
            !Globals),
        globals.set_option(delay_partial_instantiations, bool(yes),
            !Globals),
        globals.set_option(can_compare_constants_as_ints, bool(no),
            !Globals),
        globals.set_option(can_compare_compound_values, bool(yes),
            !Globals),
        globals.set_option(lexically_order_constructors, bool(yes),
            !Globals),
        globals.set_option(libgrade_install_check, bool(no), !Globals),
        globals.set_option(cross_compiling, bool(yes), !Globals),

        % The following options do not directly affect the Erlang backend,
        % however we need to ensure they are set to values that are consistent
        % with what the predicate grade_component_table/2 (below) expects.
        %
        globals.set_option(gcc_non_local_gotos, bool(no), !Globals),
        globals.set_option(gcc_global_registers, bool(no), !Globals),
        globals.set_option(asm_labels, bool(no), !Globals),
        globals.set_option(highlevel_code, bool(no), !Globals),
        globals.set_option(highlevel_data, bool(no), !Globals)
    ;
        ( Target = target_c
        ; Target = target_il
        ; Target = target_x86_64
        ; Target = target_java
        ; Target = target_csharp
        )
    ),

    % Generating high-level C code requires putting each commit
    % in its own function, to avoid problems with setjmp() and
    % non-volatile local variables.
    ( Target = target_c ->
        option_implies(highlevel_code, put_commit_in_own_func, bool(yes),
            !Globals)
    ;
        true
    ),

    % Generating x86_64 assembler implies '--no-use-local-vars'
    (
        Target = target_x86_64,
        globals.set_option(use_local_vars, bool(no), !Globals)
    ;
        ( Target = target_c
        ; Target = target_il
        ; Target = target_csharp
        ; Target = target_java
        ; Target = target_erlang
        )
    ),

    % Using trail segments implies the use of the trail.
    option_implies(trail_segments, use_trail, bool(yes), !Globals),

    %
    % Set up options for position independent code.
    %

    % Shared libraries always use `--linkage shared'.
    option_implies(compile_to_shared_lib, linkage, string("shared"),
        !Globals),
    option_implies(compile_to_shared_lib, mercury_linkage,
        string("shared"), !Globals),

    % --high-level-code disables the use of low-level gcc extensions
    option_implies(highlevel_code, gcc_non_local_gotos, bool(no),
        !Globals),
    option_implies(highlevel_code, gcc_global_registers, bool(no),
        !Globals),
    option_implies(highlevel_code, asm_labels, bool(no), !Globals),

    % --no-gcc-nested-functions implies --no-gcc-local-labels
    option_neg_implies(gcc_nested_functions, gcc_local_labels, bool(no),
        !Globals),

    % --no-mlds-optimize implies --no-optimize-tailcalls
    option_neg_implies(optimize, optimize_tailcalls, bool(no), !Globals),

    % --rebuild is just like --make but always rebuilds the files
    % without checking timestamps.
    option_implies(rebuild, make, bool(yes), !Globals),

    % If no --lib-linkage option has been specified, default to the
    % set of all possible linkages.
    globals.lookup_accumulating_option(!.Globals, lib_linkages,
        LibLinkages0),
    (
        LibLinkages0 = [],
        globals.set_option(lib_linkages,
            accumulating(["static", "shared"]), !Globals)
    ;
        LibLinkages0 = [_ | _]
    ),

    % make.m controls generating object code and linking itself,
    % so mercury_compile.m should only generate target code when
    % given a module to process.
    option_implies(make, compile_only, bool(yes), !Globals),
    option_implies(make, target_code_only, bool(yes), !Globals),

    % This is needed for library installation (the library grades
    % are built using `--use-grade-subdirs', and assume that
    % the interface files were built using `--use-subdirs').
    option_implies(make, use_subdirs, bool(yes), !Globals),
    option_implies(invoked_by_mmc_make, use_subdirs, bool(yes), !Globals),
    option_implies(invoked_by_mmc_make, make, bool(no), !Globals),

    % --make handles creation of the module dependencies itself,
    % and they don't need to be recreated when compiling to C.
    option_implies(invoked_by_mmc_make,
        generate_mmc_make_module_dependencies, bool(no), !Globals),

    % --libgrade-install-check only works with --make
    option_neg_implies(make, libgrade_install_check, bool(no), !Globals),

    % `--transitive-intermodule-optimization' and `--make' are
    % not compatible with each other.
    %
    globals.lookup_bool_option(!.Globals, transitive_optimization,
        TransOpt),
    (
        TransOpt = yes,
        globals.lookup_bool_option(!.Globals, make, UsingMMC_Make),
        globals.lookup_bool_option(!.Globals, invoked_by_mmc_make,
            InvokedByMMC_Make),
        ( UsingMMC_Make `bool.or` InvokedByMMC_Make = yes ->
            add_error("`--transitive-intermodule-optimization' is" ++
                " incompatible with `mmc --make'.", !Errors)
        ;
            true
        )
    ;
        TransOpt = no
    ),

    % `--intermodule-optimization' and `--intermodule-analysis' are
    % not compatible with each other.
    globals.lookup_bool_option(!.Globals, intermodule_optimization,
        InterModOpt),
    globals.lookup_bool_option(!.Globals, intermodule_analysis,
        InterModAnalysis),
    (
        InterModOpt = yes,
        InterModAnalysis = yes
    ->
        add_error("`--intermodule-optimization' is" ++
            " incompatible with `--intermodule-analysis'.", !Errors)
    ;
        true
    ),

    ( io.have_symlinks ->
        true
    ;
        globals.set_option(use_symlinks, bool(no), !Globals)
    ),

    globals.lookup_maybe_string_option(!.Globals,
        generate_standalone_interface, MaybeStandaloneInt),
    globals.lookup_bool_option(!.Globals,
        extra_initialization_functions, ExtraInitFunctions),
    (
        MaybeStandaloneInt = yes(_),
        ExtraInitFunctions = yes
    ->
        add_error("`--generate-standalone-interface' is" ++
            " incompatible with `--extra-initialization-functions'.",
            !Errors)
    ;
        true
    ),

    option_implies(structure_reuse_analysis, structure_sharing_analysis,
        bool(yes), !Globals),
    option_implies(verbose_check_termination, termination_check, bool(yes),
        !Globals),
    option_implies(termination_check, termination, bool(yes), !Globals),
    option_implies(termination_check, warn_missing_trans_opt_files,
        bool(yes), !Globals),
    option_implies(verbose_check_termination2, check_termination2,
        bool(yes), !Globals),
    option_implies(check_termination2, termination2, bool(yes), !Globals),
    option_implies(check_termination2, warn_missing_trans_opt_files,
        bool(yes), !Globals),
    option_implies(make_transitive_opt_interface, transitive_optimization,
        bool(yes), !Globals),
    option_implies(transitive_optimization, intermodule_optimization,
        bool(yes), !Globals),
    option_implies(use_trans_opt_files, use_opt_files, bool(yes),
        !Globals),

    % If we are doing full inter-module or transitive optimization,
    % we need to build all `.opt' or `.trans_opt' files.
    option_implies(intermodule_optimization, use_opt_files, bool(no),
        !Globals),
    option_implies(transitive_optimization, use_trans_opt_files, bool(no),
        !Globals),

    % XXX `--use-opt-files' is broken.
    % When inter-module optimization is enabled, error checking
    % without the extra information from the `.opt' files
    % is done when making the `.opt' file. With `--use-opt-files',
    % that doesn't happen.
    globals.set_option(use_opt_files, bool(no), !Globals),

    option_implies(smart_recompilation, generate_item_version_numbers,
        bool(yes), !Globals),
    option_implies(find_all_recompilation_reasons, verbose_recompilation,
        bool(yes), !Globals),

    % Disable `--smart-recompilation' for compilation options which either
    % do not produce a compiled output file or for which smart
    % recompilation will not work.
    %
    option_implies(generate_source_file_mapping, smart_recompilation,
        bool(no), !Globals),
    option_implies(generate_dependencies, smart_recompilation, bool(no),
        !Globals),
    option_implies(generate_dependency_file, smart_recompilation, bool(no),
        !Globals),
    option_implies(convert_to_mercury, smart_recompilation, bool(no),
        !Globals),
    option_implies(make_private_interface, smart_recompilation, bool(no),
        !Globals),
    option_implies(make_interface, smart_recompilation, bool(no),
        !Globals),
    option_implies(make_short_interface, smart_recompilation, bool(no),
        !Globals),
    option_implies(make_xml_documentation, smart_recompilation, bool(no),
        !Globals),
    option_implies(output_grade_string, smart_recompilation, bool(no),
        !Globals),
    option_implies(make_optimization_interface,
        smart_recompilation, bool(no), !Globals),
    option_implies(make_transitive_opt_interface,
        smart_recompilation, bool(no), !Globals),
    option_implies(make_analysis_registry,
        smart_recompilation, bool(no), !Globals),
    option_implies(errorcheck_only, smart_recompilation, bool(no),
        !Globals),
    option_implies(typecheck_only, smart_recompilation, bool(no),
        !Globals),

    % disable --line-numbers when building the `.int', `.opt', etc. files,
    % since including line numbers in those would cause unnecessary
    % recompilation
    option_implies(make_private_interface,          line_numbers, bool(no),
        !Globals),
    option_implies(make_interface,                  line_numbers, bool(no),
        !Globals),
    option_implies(make_short_interface,            line_numbers, bool(no),
        !Globals),
    option_implies(make_optimization_interface,     line_numbers, bool(no),
        !Globals),
    option_implies(make_transitive_opt_interface,   line_numbers, bool(no),
        !Globals),

    % We never use version number information in `.int3',
    % `.opt' or `.trans_opt' files.
    option_implies(make_short_interface, generate_item_version_numbers,
        bool(no), !Globals),

    % The combination of --make-xml-documentation and
    % --intermodule-optimization can causes spurious warnings about
    % missing .opt files if they haven't been built yet.
    %
    option_implies(make_xml_documentation, intermodule_optimization,
        bool(no), !Globals),

    % XXX Smart recompilation does not yet work with inter-module
    % optimization, but we still want to generate version numbers
    % in interface files for users of a library compiled with
    % inter-module optimization but not using inter-module
    % optimization themselves.
    globals.lookup_bool_option(!.Globals, smart_recompilation, Smart),
    maybe_disable_smart_recompilation(Smart, intermodule_optimization, yes,
        "`--intermodule-optimization'", !Globals, !IO),
    maybe_disable_smart_recompilation(Smart, use_opt_files, yes,
        "`--use-opt-files'", !Globals, !IO),

    % XXX Smart recompilation does not yet work with
    % `--no-target-code-only'. With `--no-target-code-only'
    % it becomes difficult to work out what all the target
    % files are and check whether they are up-to-date.
    % By default, mmake always enables `--target-code-only' and
    % processes the target code file itself, so this isn't a problem.
    maybe_disable_smart_recompilation(Smart, target_code_only, no,
        "`--no-target-code-only'", !Globals, !IO),

    option_implies(use_grade_subdirs, use_subdirs, bool(yes), !Globals),

    option_implies(very_verbose, verbose, bool(yes), !Globals),
    option_implies(verbose, verbose_commands, bool(yes), !Globals),
    globals.lookup_bool_option(!.Globals, very_verbose, VeryVerbose),
    globals.lookup_bool_option(!.Globals, statistics, Statistics),
    (
        VeryVerbose = yes,
        Statistics = yes
    ->
        globals.set_option(detailed_statistics, bool(yes), !Globals)
    ;
        true
    ),

    option_implies(debug_modes_minimal, debug_modes, bool(yes), !Globals),
    option_implies(debug_modes_verbose, debug_modes, bool(yes), !Globals),
    option_implies(debug_modes_statistics, debug_modes, bool(yes),
        !Globals),

    globals.lookup_int_option(!.Globals, debug_liveness, DebugLiveness),
    (
        DebugLiveness >= 0,
        convert_dump_alias("all", AllDumpOptions)
    ->
        % Programmers only enable --debug-liveness if they are interested
        % in the goal annotations put on goals by the various phases
        % of the liveness pass. The default dump options do not print
        % these annotations.
        globals.lookup_string_option(!.Globals, dump_hlds_options,
            DumpOptions0),
        string.append(DumpOptions0, AllDumpOptions, DumpOptions1),
        globals.set_option(dump_hlds_options, string(DumpOptions1),
            !Globals)
    ;
        true
    ),

    option_implies(debug_modes_verbose, debug_modes, bool(yes), !Globals),
    globals.lookup_int_option(!.Globals, debug_modes_pred_id,
        DebugModesPredId),
    ( DebugModesPredId > 0 ->
        globals.set_option(debug_modes, bool(yes), !Globals)
    ;
        true
    ),

    globals.lookup_accumulating_option(!.Globals,
        unneeded_code_debug_pred_name, DebugUnneededCodePredNames),
    (
        DebugUnneededCodePredNames = []
    ;
        DebugUnneededCodePredNames = [_ | _],
        globals.set_option(unneeded_code_debug, bool(yes), !Globals)
    ),

    globals.lookup_accumulating_option(!.Globals, debug_opt_pred_id,
        DebugOptPredIdStrs),
    globals.lookup_accumulating_option(!.Globals, debug_opt_pred_name,
        DebugOptPredNames),
    (
        ( DebugOptPredIdStrs = [_ | _]
        ; DebugOptPredNames = [_ | _]
        )
    ->
        globals.set_option(debug_opt, bool(yes), !Globals)
    ;
        true
    ),

    globals.lookup_bool_option(!.Globals, debug_intermodule_analysis,
        DebugIntermoduleAnalysis),
    analysis.enable_debug_messages(DebugIntermoduleAnalysis, !IO),

    globals.lookup_accumulating_option(!.Globals, dump_hlds_pred_id,
        DumpHLDSPredIds),
    (
        DumpHLDSPredIds = [_ | _],
        globals.lookup_string_option(!.Globals, dump_hlds_options,
            DumpOptions2),
        % Prevent the dumping of the mode and type tables.
        string.replace_all(DumpOptions2, "M", "", DumpOptions3),
        string.replace_all(DumpOptions3, "T", "", DumpOptions),
        globals.set_option(dump_hlds_options, string(DumpOptions),
            !Globals)
    ;
        DumpHLDSPredIds = []
    ),

    option_implies(debug_mode_constraints, prop_mode_constraints,
        bool(yes), !Globals),
    option_implies(prop_mode_constraints, mode_constraints, bool(yes),
        !Globals),
    option_implies(simple_mode_constraints, mode_constraints, bool(yes),
        !Globals),

    option_implies(frameopt_comments, auto_comments, bool(yes), !Globals),

    % Minimal model tabling is not compatible with high level code
    % or with trailing; see the comments in runtime/mercury_grade.h.

    globals.lookup_bool_option(!.Globals, use_trail, UseTrail),
    globals.lookup_bool_option(!.Globals, highlevel_code, HighLevelCode),
    globals.lookup_bool_option(!.Globals, use_minimal_model_stack_copy,
        UseMinimalModelStackCopy),
    globals.lookup_bool_option(!.Globals, use_minimal_model_own_stacks,
        UseMinimalModelOwnStacks),
    bool.or(UseMinimalModelStackCopy, UseMinimalModelOwnStacks,
        UseMinimalModel),
    (
        UseMinimalModelStackCopy = yes,
        UseMinimalModelOwnStacks = yes
    ->
        add_error("can't use both forms of minimal model tabling " ++
            "at once", !Errors)
    ;
        UseMinimalModel = yes,
        HighLevelCode = yes
    ->
        add_error("minimal model tabling is incompatible "
            ++ "with high level code", !Errors)
    ;
        UseMinimalModel = yes,
        UseTrail = yes
    ->
        add_error("minimal model tabling is incompatible " ++
            "with trailing", !Errors)
    ;
        true
    ),

    % Argument packing only works on C back-ends with low-level data.
    % In the future, we may want to use C bit-field syntax for high-level data.
    % For other back-ends, any RTTI code will need to be updated to cope with
    % packed arguments.
    %
    % Only C targets may store a constructor argument across two words.
    option_implies(highlevel_data, arg_pack_bits, int(0), !Globals),
    (
        Target = target_c,
        globals.lookup_int_option(!.Globals, arg_pack_bits, ArgPackBits0),
        globals.lookup_int_option(!.Globals, bits_per_word, BitsPerWord),
        % If --arg-pack-bits is negative then it means use all word bits.
        ( ArgPackBits0 < 0 ->
            ArgPackBits = BitsPerWord
        ; ArgPackBits0 > BitsPerWord ->
            io.progname_base("mercury_compile", ProgNameB, !IO),
            io.format("%s: warning: --arg-pack-bits invalid\n",
                [s(ProgNameB)], !IO),
            record_warning(!.Globals, !IO),
            ArgPackBits = BitsPerWord
        ;
            ArgPackBits = ArgPackBits0
        ),
        globals.set_option(arg_pack_bits, int(ArgPackBits), !Globals)
    ;
        ( Target = target_csharp
        ; Target = target_java
        ; Target = target_x86_64
        ; Target = target_il
        ; Target = target_erlang
        ),
        globals.set_option(arg_pack_bits, int(0), !Globals),
        globals.set_option(allow_double_word_fields, bool(no), !Globals)
    ),

    % We assume that single-precision floats do not need to be boxed.
    option_implies(single_prec_float, unboxed_float, bool(yes),
        !Globals),

    % We only use the float registers if floats would not fit into the
    % regular registers.
    option_implies(highlevel_code, use_float_registers, bool(no), !Globals),
    option_implies(unboxed_float, use_float_registers, bool(no), !Globals),

    % Changing this means that the code in make_hlds_passes.m that
    % handles the declarations for the global variables used by
    % mutables should also be updated.
    option_implies(highlevel_code, mutable_always_boxed, bool(no),
        !Globals),

    % Currently, multi-arm switches have been tested only for the LLDS
    % backend (which always generates C) and for the MLDS backend when
    % it is generating C, C# or Java code.
    (
        ( Target = target_c
        ; Target = target_csharp
        ; Target = target_java
        )
    ;
        ( Target = target_x86_64
        ; Target = target_il
        ; Target = target_erlang
        ),
        globals.set_option(allow_multi_arm_switches, bool(no), !Globals)
    ),

    (
        ( Target = target_csharp
        ; Target = target_java
        )
    ->
        % Switch off string hash switches until these backends implement
        % the hash operations.
        globals.set_option(string_hash_switch_size, int(999999), !Globals)
    ;
        true
    ),

    option_implies(target_debug, strip, bool(no), !Globals),

    % Profile for feedback requires coverage profiling.
    option_implies(profile_for_feedback, coverage_profiling, bool(yes),
        !Globals),

    % At the moment coverage profiling is not compatible with the tail
    % recursion preservation optimization used by the deep profiler.
    option_implies(coverage_profiling, deep_profile_tail_recursion,
        bool(no), !Globals),

    % Inlining happens before the deep profiling transformation, so if
    % we allowed inlining to happen, then we would lose all profiling
    % information about the inlined calls - this is not usually what we
    % want so we disable inlining with deep profiling by default.  The
    % user can re-enable it with the `--profile-optimized' option.  Leave
    % inlineing enabled when profiling for implicit parallelism.
    %
    option_implies(profile_for_feedback, prof_optimized, bool(yes), !Globals),
    globals.lookup_bool_option(!.Globals, prof_optimized, ProfOptimized),
    (
        ProfOptimized = no,
        option_implies(profile_deep, allow_inlining, bool(no), !Globals)
    ;
        ProfOptimized = yes
    ),

    % Perform a simplification pass before the profiling passes if one of
    % these profiling options is enabled.
    option_implies(profile_deep, pre_prof_transforms_simplify, bool(yes),
        !Globals),
    option_implies(record_term_sizes_as_words, pre_prof_transforms_simplify,
        bool(yes), !Globals),
    option_implies(record_term_sizes_as_cells, pre_prof_transforms_simplify,
        bool(yes), !Globals),

    globals.lookup_string_option(!.Globals, experimental_complexity,
        ExpComp),
    ( ExpComp = "" ->
        true
    ;
        globals.set_option(allow_inlining, bool(no), !Globals)
    ),

    % --decl-debug is an extension of --debug
    option_implies(decl_debug, exec_trace, bool(yes), !Globals),

    % --ssdb implies --link-ssdb-libs
    option_implies(source_to_source_debug,
        link_ssdb_libs, bool(yes), !Globals),

    % We need to be able to simulate exits for calls between where an
    % exception is thrown to where it is caught both in the debugger and
    % for deep profiling.
    option_implies(exec_trace, stack_trace, bool(yes), !Globals),
    option_implies(profile_deep, stack_trace, bool(yes), !Globals),

    % Deep profiling disables the optimization for pretesting whether
    % x == y in runtime/mercury_unify_compare_body.h, so compensate by
    % doing the same test in the unify and compare predicates generated by
    % the Mercury compiler.
    option_implies(profile_deep, should_pretest_equality, bool(yes),
        !Globals),

    % In debugging grades, we want to generate executables in which
    % one can do retries across I/O safely.
    option_implies(exec_trace, trace_table_io_all, bool(yes), !Globals),

    % --trace-table-io-all is compulsory application of --trace-table-io
    option_implies(trace_table_io_all, trace_table_io, bool(yes),
        !Globals),
    % --trace-table-io-require is compulsory application
    %   of --trace-table-io
    option_implies(trace_table_io_require, trace_table_io, bool(yes),
        !Globals),

    % Execution tracing requires
    %   - disabling optimizations that would change
    %     the trace being generated (except with --trace-optimized)
    %   - enabling some low level optimizations to ensure consistent
    %     paths across optimization levels
    %   - enabling stack layouts
    %   - enabling typeinfo liveness
    globals.lookup_bool_option(!.Globals, trace_optimized, TraceOptimized),
    ( given_trace_level_is_none(TraceLevel) = no ->
        (
            TraceOptimized = no,
            % The following options modify the structure
            % of the program, which makes it difficult to
            % relate the trace to the source code (although
            % it can be easily related to the transformed HLDS).
            globals.set_option(allow_inlining, bool(no), !Globals),
            globals.set_option(optimize_unused_args, bool(no), !Globals),
            globals.set_option(optimize_higher_order, bool(no), !Globals),
            globals.set_option(type_specialization, bool(no), !Globals),
            globals.set_option(user_guided_type_specialization,
                bool(no), !Globals),
            globals.set_option(deforestation, bool(no), !Globals),
            globals.set_option(constraint_propagation,
                bool(no), !Globals),
            globals.set_option(local_constraint_propagation,
                bool(no), !Globals),
            globals.set_option(optimize_duplicate_calls,
                bool(no), !Globals),
            globals.set_option(optimize_constructor_last_call,
                bool(no), !Globals),
            globals.set_option(optimize_saved_vars_cell,
                bool(no), !Globals),
            globals.set_option(loop_invariants, bool(no), !Globals),
            globals.set_option(untuple, bool(no), !Globals),
            globals.set_option(tuple, bool(no), !Globals),

            % For the IL backend we turn off optimize_peep
            % so that we don't optimize away references to the
            % local variables of a procedure.
            (
                Target = target_il,
                globals.set_option(optimize_peep, bool(no), !Globals)
            ;
                ( Target = target_c
                ; Target = target_csharp
                ; Target = target_java
                ; Target = target_x86_64
                ; Target = target_erlang
                )
            )
        ;
            TraceOptimized = yes
        ),

        % Disable hijacks if debugging is enabled. The code we now use
        % to restore the stacks for direct retries works only if the retry
        % does not "backtrack" over a hijacked nondet stack frame whose
        % hijack has not been undone. Note that code compiled without
        % debugging may still hijack nondet stack frames. Execution may
        % reemerge from the nondebugged region in one of two ways. If the
        % nondebugged code returns, then it will have undone hijack,
        % and the retry code will work. If the nondebugged code calls
        % debugged code, there will be a region on the stacks containing
        % no debugging information, and the retry command will refuse
        % to perform retries that go into or beyond this region.
        % Both cases preserve correctness.
        %
        % An alternative solution would be to store everything on the
        % nondet stack that may be hijacked in ordinary stack slots
        % on entry to every procedure, but that would be not only
        % more complex than simply disabling hijacks, it would be slower
        % as well, except in procedures that would have many nested
        % hijacks, and such code is extremely rare.
        globals.set_option(allow_hijacks, bool(no), !Globals),

        % The following option prevents useless variables from cluttering
        % the trace. Its explicit setting removes a source of variability
        % in the goal paths reported by tracing.
        globals.set_option(excess_assign, bool(yes), !Globals),

        % The explicit setting of the following option removes a source
        % of variability in the goal paths reported by tracing.
        globals.set_option(follow_code, bool(yes), !Globals),

        % The following option selects a special-case code generator
        % that cannot (yet) implement tracing.
        globals.set_option(middle_rec, bool(no), !Globals),

        % The following options cause the info required by tracing
        % to be generated.
        globals.set_option(trace_stack_layout, bool(yes), !Globals),
        globals.set_option(body_typeinfo_liveness, bool(yes), !Globals),

        % To support up-level printing, we need to save variables across
        % a call even if the call cannot succeed.
        globals.set_option(opt_no_return_calls, bool(no), !Globals),

        % The declarative debugger does not (yet) know about tail calls.
        ( trace_level_allows_tail_rec(TraceLevel) = no ->
            globals.set_option(exec_trace_tail_rec, bool(no), !Globals)
        ;
            true
        )
    ;
        % Since there will be no call and exit events, there is no point
        % in trying to turn them into tailcall events.
        globals.set_option(exec_trace_tail_rec, bool(no), !Globals)
    ),

    option_implies(profile_deep, procid_stack_layout, bool(yes), !Globals),
    globals.lookup_bool_option(!.Globals, profile_deep, ProfileDeep),
    (
        ProfileDeep = yes,
        (
            HighLevelCode = no,
            Target = target_c
        ->
            true
        ;
            add_error("deep profiling is incompatible " ++
                "with high level code", !Errors)
        ),
        globals.set_option(optimize_constructor_last_call,
            bool(no), !Globals),
        globals.lookup_bool_option(!.Globals,
            use_lots_of_ho_specialization, LotsOfHOSpec),
        (
            LotsOfHOSpec = yes,
            True = bool(yes),
            globals.set_option(optimize_higher_order, True, !Globals),
            globals.set_option(higher_order_size_limit, int(999999),
                !Globals)
        ;
            LotsOfHOSpec = no
        )
    ;
        ProfileDeep = no
    ),

    globals.lookup_bool_option(!.Globals, record_term_sizes_as_words,
        RecordTermSizesAsWords),
    globals.lookup_bool_option(!.Globals, record_term_sizes_as_cells,
        RecordTermSizesAsCells),
    (
        RecordTermSizesAsWords = yes,
        RecordTermSizesAsCells = yes
    ->
        add_error("we can't record term size " ++
            "as both words and cells", !Errors)
    ;
        ( RecordTermSizesAsWords = yes
        ; RecordTermSizesAsCells = yes
        )
    ->
        globals.set_option(optimize_constructor_last_call, bool(no),
            !Globals),
        % Term size profiling breaks the assumption that even word offsets from
        % the start of the cell are double-word aligned memory addresses.
        globals.set_option(allow_double_word_fields, bool(no),
            !Globals),
        (
            HighLevelCode = yes,
            add_error("term size profiling is incompatible "
                ++ "with high level code", !Errors)
        ;
            HighLevelCode = no
        )
    ;
        true
    ),

    (
        ( given_trace_level_is_none(TraceLevel) = yes
        ; HighLevelCode = no, Target = target_c
        ; Target = target_il
        )
    ->
        true
    ;
        add_error("debugging is available only in low level C grades",
            !Errors)
    ),

    % The pthreads headers on some architectures (Solaris, Linux)
    % don't work with -ansi.
    option_implies(parallel, ansi_c, bool(no), !Globals),

    option_neg_implies(inline_builtins, constant_propagation, bool(no),
        !Globals),

    % `--optimize-constant-propagation' effectively inlines builtins.
    %
    % We want to allow constant propagation in deep profiling grades,
    % so `--no-allow-inlining' should not cause it to be disabled.
    % (Other forms of inlining must be disabled for deep profiling.)
    %
    % `--no-allow-inlining' should imply
    % `--no-optimize-constant-propagation' otherwise,
    % e.g. when tracing is enabled.
    %
    (
        ProfileDeep = no,
        option_neg_implies(allow_inlining, constant_propagation, bool(no),
            !Globals)
    ;
        ProfileDeep = yes
    ),

    % --no-reorder-conj implies --no-deforestation,
    % --no-constraint-propagation and --no-local-constraint-propagation.
    option_neg_implies(reorder_conj, deforestation, bool(no), !Globals),
    option_neg_implies(reorder_conj, constraint_propagation, bool(no),
        !Globals),
    option_neg_implies(reorder_conj, local_constraint_propagation,
        bool(no), !Globals),

    % --stack-trace requires `procid' stack layouts
    option_implies(stack_trace, procid_stack_layout, bool(yes), !Globals),

    % `trace' stack layouts need `procid' stack layouts
    option_implies(trace_stack_layout, procid_stack_layout, bool(yes),
        !Globals),

    % --gc accurate for the LLDS back-end requires `agc' stack layouts,
    % typeinfo liveness, and needs hijacks, frameopt, and middle recursion
    % optimization to be switched off.
    % We also turn off optimization of stack slots for no_return calls,
    % because that optimization does not preserve agc typeinfo liveness.
    %
    % For the MLDS back-end, `--gc accurate' requires just typeinfo
    % liveness.
    %
    % XXX Currently we also need to disable heap reclamation on failure
    % if accurate GC is enabled.
    % There are two issues with heap reclamation on failure:
    % (i) For heap reclamation on failure to work at all,
    %      we also need at least some degree of liveness-accuracy.
    %      Otherwise a local variable may get initialized to point
    %      to the heap, then the heap is reset, then the memory
    %      is overwritten with new allocations, and then a collection
    %      occurs, at which point the local variable now points to
    %      a value of the wrong type.
    % (ii) The current method of handling saved heap pointers during GC
    %      means that we lose heap reclamation on failure after a
    %      GC occurs. A better method would be to just allocate a
    %      word of heap space at each choice point.
    %
    % XXX we also need to disable optimize-constructor-last-call
    % as currently the collector (and tracing code generator) knows
    % neither about the pre-constructed data structures nor the
    % references into them that this optimisation uses.
    %
    % XXX we also disable type specialization.
    % This is needed because type specialization may create
    % type class constraints of the form `c(t(T))'
    % (e.g. `enum(var(T))'' in library/sparse_bitset.m),
    % which the current RTTI system can't handle.
    %
    (
        GC_Method = gc_accurate,
        globals.set_option(agc_stack_layout, bool(yes), !Globals),
        globals.set_option(body_typeinfo_liveness, bool(yes), !Globals),
        globals.set_option(allow_hijacks, bool(no), !Globals),
        globals.set_option(optimize_frames, bool(no), !Globals),
        globals.set_option(opt_no_return_calls, bool(no), !Globals),
        globals.set_option(middle_rec, bool(no), !Globals),
        globals.set_option(
            reclaim_heap_on_semidet_failure, bool(no), !Globals),
        globals.set_option(
            reclaim_heap_on_nondet_failure, bool(no), !Globals),
        globals.set_option(optimize_constructor_last_call,
            bool(no), !Globals),
        globals.set_option(type_specialization, bool(no), !Globals),
        globals.set_option(user_guided_type_specialization,
            bool(no), !Globals)
    ;
        ( GC_Method = gc_automatic
        ; GC_Method = gc_none
        ; GC_Method = gc_boehm
        ; GC_Method = gc_boehm_debug
        ; GC_Method = gc_hgc
        ; GC_Method = gc_mps
        )
    ),

    % ml_gen_params_base and ml_declare_env_ptr_arg, in ml_code_util.m,
    % both assume (for accurate GC) that continuation environments
    % are always allocated on the stack, which means that things won't
    % if --gc accurate and --put-nondet-env-on-heap are both enabled.
    globals.lookup_bool_option(!.Globals, put_nondet_env_on_heap,
        PutNondetEnvOnHeap),
    (
        HighLevelCode = yes,
        GC_Method = gc_accurate,
        PutNondetEnvOnHeap = yes
    ->
        add_error("--gc accurate is incompatible with " ++
            "--put-nondet-env-on-heap", !Errors)
    ;
        true
    ),

    % `procid' and `agc' stack layouts need `basic' stack layouts
    option_implies(procid_stack_layout, basic_stack_layout, bool(yes),
        !Globals),
    option_implies(agc_stack_layout, basic_stack_layout, bool(yes),
        !Globals),

    % dupelim.m doesn't preserve label layout structures (e.g. it can
    % change the return address in a call to a different label whose code
    % is the same but which has a different label layout structure),
    % so we need to disable it when tracing.
    option_implies(procid_stack_layout, optimize_dups, bool(no), !Globals),
    % Likewise for accurate GC.
    option_implies(agc_stack_layout, optimize_dups, bool(no), !Globals),

    % stdlabel.m tries to perform operations that yield compiler aborts
    % if any stack layout information is present in the generated code.
    option_implies(basic_stack_layout, standardize_labels, bool(no),
        !Globals),

    % XXX deforestation and constraint propagation do not perform
    % folding on polymorphic predicates correctly with
    % --body-typeinfo-liveness.
    option_implies(body_typeinfo_liveness, deforestation, bool(no),
        !Globals),
    option_implies(body_typeinfo_liveness, constraint_propagation,
        bool(no), !Globals),

    % XXX if trailing is enabled, middle recursion optimization
    % can generate code which does not allocate a stack frame
    % even though stack slots are used to save and restore the
    % trail, if the code being optimized contains a construct which
    % might save/restore the trail state, i.e. an if-then-else,
    % negation, disjunction, or commit.
    option_implies(use_trail, middle_rec, bool(no), !Globals),

    % The cut-down stack frames used by middle recursion optimization
    % don't include return addresses. Since stack extension arranges for
    % the return to the old stack segments by overriding the return
    % address, stack extension via stack segments and middle recursion
    % optimization are incompatible.
    option_implies(stack_segments, middle_rec, bool(no), !Globals),

    % Stack copy minimal model tabling needs to be able to rewrite all
    % the redoips in a given nondet stack segments. If we allow hijacks,
    % some of these redoips may have been saved in ordinary framevars,
    % which means that tabling can't find them without label layout info.
    % Since we want to allow tabling in grades that do not have label
    % layout info, we disable hijacks instead.
    % XXX we should allow hijacks in table_builtin.m
    option_implies(use_minimal_model_stack_copy, allow_hijacks, bool(no),
        !Globals),

    % Stack copy minimal model tabling needs to generate extra code
    % at possibly negated contexts to handle the pneg stack and at commits
    % to handle the cut stack. The code below allows the generation of
    % these extra pieces of code to be disabled. The disabled program will
    % work only if the program doesn't actually use minimal model tabling,
    % which makes it useful only for performance testing.
    globals.lookup_bool_option(!.Globals,
        disable_minimal_model_stack_copy_pneg, DisablePneg),
    globals.lookup_bool_option(!.Globals,
        disable_minimal_model_stack_copy_cut, DisableCut),
    (
        UseMinimalModelStackCopy = yes,
        DisablePneg = no
    ->
        globals.set_option(use_minimal_model_stack_copy_pneg,
            bool(yes), !Globals)
    ;
        true
    ),
    (
        UseMinimalModelStackCopy = yes,
        DisableCut = no
    ->
        globals.set_option(use_minimal_model_stack_copy_cut,
            bool(yes), !Globals)
    ;
        true
    ),

    % --dump-hlds, --statistics, --parallel-liveness and
    % --parallel-code-gen require compilation by phases
    globals.lookup_accumulating_option(!.Globals, dump_hlds,
        DumpHLDSStages),
    globals.lookup_accumulating_option(!.Globals, dump_trace_counts,
        DumpTraceStages),
    globals.lookup_bool_option(!.Globals, parallel_liveness,
        ParallelLiveness),
    globals.lookup_bool_option(!.Globals, parallel_code_gen,
        ParallelCodeGen),
    (
        ( DumpHLDSStages = [_ | _]
        ; DumpTraceStages = [_ | _]
        ; Statistics = yes
        ; ParallelLiveness = yes
        ; ParallelCodeGen = yes
        )
    ->
        globals.set_option(trad_passes, bool(no), !Globals)
    ;
        true
    ),

    % If we are doing type-specialization, we may as well take advantage
    % of the declarations supplied by the programmer.
    option_implies(type_specialization, user_guided_type_specialization,
        bool(yes), !Globals),

    % The local constraint propagation transformation (constraint.m)
    % is a required part of the constraint propagation transformation
    % performed by deforest.m.
    option_implies(constraint_propagation, local_constraint_propagation,
        bool(yes), !Globals),

    % --intermod-unused-args implies --intermodule-optimization and
    % --optimize-unused-args.
    option_implies(intermod_unused_args, intermodule_optimization,
        bool(yes), !Globals),
    option_implies(intermod_unused_args, optimize_unused_args, bool(yes),
        !Globals),

    % --introduce-accumulators implies --excess-assign and
    % --common-struct.
    option_implies(introduce_accumulators, excess_assign, bool(yes),
        !Globals),
    option_implies(introduce_accumulators, common_struct, bool(yes),
        !Globals),

    % Don't do the unused_args optimization when making the
    % optimization interface.
    option_implies(make_optimization_interface, optimize_unused_args,
        bool(no), !Globals),

    % The results of trail usage analysis assume that trail usage
    % optimization is being done, i.e. that redundant trailing
    % operations are really being eliminated.
    option_implies(analyse_trail_usage, optimize_trail_usage,
        bool(yes), !Globals),

    % The information needed for generating the module ordering
    % is only available while generating the dependencies.
    option_implies(generate_module_order, generate_dependencies,
        bool(yes), !Globals),

    % The information needed for generating the imports graph
    % is only available while generating the dependencies.
    option_implies(imports_graph, generate_dependencies, bool(yes), !Globals),

    % We only generate the source file mapping if the module name
    % doesn't match the file name.
    option_implies(generate_source_file_mapping, warn_wrong_module_name,
        bool(no), !Globals),

    globals.lookup_string_option(!.Globals, fullarch, FullArch),

    % Add the standard library directory.
    globals.lookup_maybe_string_option(!.Globals,
        mercury_standard_library_directory, MaybeStdLibDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        globals.get_options(!.Globals, OptionTable2),
        globals.set_options(option_table_add_mercury_library_directory(
            OptionTable2, StdLibDir), !Globals),

        % Add `-L' and `-R' options for the location of the GC libraries.
        globals.lookup_accumulating_option(!.Globals,
            link_library_directories, LinkLibDirs0),
        globals.set_option(link_library_directories,
            accumulating([StdLibDir/"lib" | LinkLibDirs0]), !Globals),

        globals.lookup_accumulating_option(!.Globals,
            runtime_link_library_directories, Rpath0),
        globals.set_option(runtime_link_library_directories,
            accumulating([StdLibDir/"lib" | Rpath0]), !Globals)
    ;
        MaybeStdLibDir = no
    ),

    % Add the path to mercury_conf.h.
    globals.lookup_maybe_string_option(!.Globals,
        mercury_configuration_directory, MaybeConfDir),
    (
        MaybeConfDir = yes(ConfDir),
        globals.lookup_accumulating_option(!.Globals, c_include_directory,
            CIncludeDirs0),
        globals.set_option(c_include_directory,
            accumulating([ConfDir/"conf" | CIncludeDirs0]), !Globals)
    ;
        MaybeConfDir = no
    ),

    % Find the configuration file.
    globals.lookup_maybe_string_option(!.Globals, config_file,
        ConfigFile),
    % yes("") means `--config-file' was not passed on the command line.
    ( ConfigFile = yes("") ->
        (
            MaybeConfDir = yes(ConfDir1),
            globals.set_option(config_file, maybe_string(yes(
                ConfDir1/"conf"/"Mercury.config")), !Globals)
        ;
            MaybeConfDir = no,
            globals.set_option(config_file, maybe_string(no), !Globals)
        )
    ;
        true
    ),

    % Handle the `.opt', C and Erlang header, init file and library search
    % directories for installed libraries.  These couldn't be handled by
    % options.m because they are grade dependent.
    globals.lookup_accumulating_option(!.Globals,
        mercury_library_directories, MercuryLibDirs),
    grade_directory_component(!.Globals, GradeString),
    (
        MercuryLibDirs = [_ | _],
        ExtraLinkLibDirs = list.map(
            (func(MercuryLibDir) =
                MercuryLibDir/"lib"/GradeString
            ), MercuryLibDirs),

        globals.lookup_accumulating_option(!.Globals,
            link_library_directories, LinkLibDirs1),
        globals.set_option(link_library_directories,
            accumulating(LinkLibDirs1 ++ ExtraLinkLibDirs), !Globals),

        globals.lookup_accumulating_option(!.Globals,
            runtime_link_library_directories, Rpath),
        globals.set_option(runtime_link_library_directories,
            accumulating(Rpath ++ ExtraLinkLibDirs), !Globals),

        ExtraIncludeDirs = list.map(
            (func(MercuryLibDir) =
                MercuryLibDir/"lib"/GradeString/"inc"
            ), MercuryLibDirs),
        globals.lookup_accumulating_option(!.Globals, c_include_directory,
            CIncludeDirs),
        globals.set_option(c_include_directory,
            accumulating(ExtraIncludeDirs ++ CIncludeDirs), !Globals),
        globals.lookup_accumulating_option(!.Globals,
            erlang_include_directory, ErlangIncludeDirs),
        globals.set_option(erlang_include_directory,
            accumulating(ExtraIncludeDirs ++ ErlangIncludeDirs), !Globals),

        ExtraIntermodDirs = list.map(
            (func(MercuryLibDir) =
                dir.make_path_name(MercuryLibDir,
                    dir.make_path_name("ints", GradeString))
            ), MercuryLibDirs),
        globals.lookup_accumulating_option(!.Globals,
            intermod_directories, IntermodDirs0),
        globals.set_option(intermod_directories,
            accumulating(ExtraIntermodDirs ++ IntermodDirs0), !Globals),

        ExtraInitDirs = list.map(
            (func(MercuryLibDir) =
                MercuryLibDir / "modules" / GradeString
            ), MercuryLibDirs),

        globals.lookup_accumulating_option(!.Globals,
            init_file_directories, InitDirs1),
        globals.set_option(init_file_directories,
            accumulating(InitDirs1 ++ ExtraInitDirs), !Globals)
    ;
        MercuryLibDirs = []
    ),

    % If --use-search-directories-for-intermod is true, append the
    % search directories to the list of directories to search for
    % .opt files.
    globals.lookup_bool_option(!.Globals,
        use_search_directories_for_intermod, UseSearchDirs),
    (
        UseSearchDirs = yes,
        globals.lookup_accumulating_option(!.Globals,
            intermod_directories, IntermodDirs1),
        globals.lookup_accumulating_option(!.Globals,
            search_directories, SearchDirs),
        globals.set_option(intermod_directories,
            accumulating(IntermodDirs1 ++ SearchDirs), !Globals)
    ;
        UseSearchDirs = no
    ),

    globals.lookup_bool_option(!.Globals, use_grade_subdirs,
        UseGradeSubdirs),
    globals.lookup_accumulating_option(!.Globals,
        search_library_files_directories, SearchLibFilesDirs),
    globals.lookup_accumulating_option(!.Globals,
        intermod_directories, IntermodDirs2),
    ToGradeSubdir = (func(Dir) = Dir/"Mercury"/GradeString/FullArch),
    (
        UseGradeSubdirs = yes,
        % With `--use-grade-subdirs', `.opt', `.trans_opt' and
        % `.mih' files are placed in a directory named
        % `Mercury/<grade>/<fullarch>/Mercury/<ext>s'.
        % When searching for a `.opt' file, module_name_to_file_name
        % produces `Mercury/<ext>/<module>.ext' so that searches
        % for installed files work, so we need to add
        % `--intermod-directory Mercury/<grade>/<fullarch>'
        % to find the `.opt' files in the current directory.
        GradeSubdir = "Mercury"/GradeString/FullArch,

        % Directories listed with --search-library-files-directories need
        % to be treated in the same way as the current directory.
        SearchLibFilesGradeSubdirs = list.map(ToGradeSubdir,
            SearchLibFilesDirs),
        IntermodDirs3 = [GradeSubdir] ++ SearchLibFilesGradeSubdirs ++
            list.filter(isnt(unify(dir.this_directory)), IntermodDirs2)
    ;
        UseGradeSubdirs = no,
        IntermodDirs3 = SearchLibFilesDirs ++ IntermodDirs2
    ),
    globals.set_option(intermod_directories,
        accumulating(IntermodDirs3), !Globals),

    globals.lookup_accumulating_option(!.Globals,
        link_library_directories, LinkLibDirs2),
    globals.lookup_accumulating_option(!.Globals,
        init_file_directories, InitDirs2),
    (
        UseGradeSubdirs = yes,
        % With --use-grade-subdirs we need to search in
        % `Mercury/<grade>/<fullarch>/Mercury/lib' for libraries and
        % `Mercury/<grade>/<fullarch>/Mercury/inits' for init files,
        % for each directory listed with --search-library-files-directory.
        ToGradeLibDir = (func(Dir) = ToGradeSubdir(Dir)/"Mercury"/"lib"),
        SearchGradeLibDirs = list.map(ToGradeLibDir, SearchLibFilesDirs),
        LinkLibDirs = SearchGradeLibDirs ++ LinkLibDirs2,

        ToGradeInitDir = (func(Dir) =
            ToGradeSubdir(Dir)/"Mercury"/"inits"),
        SearchGradeInitDirs = list.map(ToGradeInitDir, SearchLibFilesDirs),
        InitDirs = SearchGradeInitDirs ++ InitDirs2
    ;
        UseGradeSubdirs = no,
        LinkLibDirs = SearchLibFilesDirs ++ LinkLibDirs2,
        InitDirs = SearchLibFilesDirs ++ InitDirs2
    ),
    globals.set_option(link_library_directories,
        accumulating(LinkLibDirs), !Globals),
    globals.set_option(init_file_directories,
        accumulating(InitDirs), !Globals),

    % When searching for a header (.mh, .mih, .hrl) file,
    % module_name_to_file_name uses the plain header name, so we need to
    % add the full path to the header files in the current directory,
    % and any directories listed with --search-library-files-directory.
    globals.lookup_bool_option(!.Globals, use_subdirs, UseSubdirs),
    (
        (
            UseGradeSubdirs = yes,
            ToMihsSubdir =
                (func(Dir) = ToGradeSubdir(Dir)/"Mercury"/"mihs"),
            ToHrlsSubdir =
                (func(Dir) = ToGradeSubdir(Dir)/"Mercury"/"hrls")
        ;
            UseGradeSubdirs = no,
            (
                UseSubdirs = yes,
                ToMihsSubdir = (func(Dir) = Dir/"Mercury"/"mihs"),
                ToHrlsSubdir = (func(Dir) = Dir/"Mercury"/"hrls")
            ;
                UseSubdirs = no,
                fail
            )
        )
    ->
        globals.lookup_accumulating_option(!.Globals, c_include_directory,
            CIncludeDirs1),
        MihsSubdir = ToMihsSubdir(dir.this_directory),
        SearchLibMihsSubdirs = list.map(ToMihsSubdir, SearchLibFilesDirs),
        SubdirCIncludeDirs = [dir.this_directory, MihsSubdir |
            SearchLibMihsSubdirs ++ CIncludeDirs1],
        globals.set_option(c_include_directory,
            accumulating(SubdirCIncludeDirs), !Globals),

        globals.lookup_accumulating_option(!.Globals,
            erlang_include_directory, ErlangIncludeDirs1),
        HrlsSubdir = ToHrlsSubdir(dir.this_directory),
        SubdirErlangIncludeDirs = [HrlsSubdir | ErlangIncludeDirs1],
        globals.set_option(erlang_include_directory,
            accumulating(SubdirErlangIncludeDirs), !Globals)
    ;
        true
    ),

    % --use-opt-files implies --no-warn-missing-opt-files since
    % we are expecting some to be missing.
    option_implies(use_opt_files, warn_missing_opt_files, bool(no),
        !Globals),

    % --warn-non-tail-recursion requires tail call optimization to be enabled.
    % It also doesn't work if you use --errorcheck-only.
    (
        HighLevelCode = no,
        option_requires(warn_non_tail_recursion, pessimize_tailcalls, bool(no),
            "--warn-non-tail-recursion is incompatible with " ++
            "--pessimize-tailcalls",
            !.Globals, !Errors)
    ;
        HighLevelCode = yes,
        option_requires(warn_non_tail_recursion, optimize_tailcalls, bool(yes),
            "--warn-non-tail-recursion requires --optimize-tailcalls",
            !.Globals, !Errors)
    ),
    option_requires(warn_non_tail_recursion, errorcheck_only, bool(no),
        "--warn-non-tail-recursion is incompatible with " ++
        "--errorcheck-only",
        !.Globals, !Errors),

    % The backend foreign languages depend on the target.
    (
        Target = target_c,
        BackendForeignLanguages = ["c"]
    ;
        Target = target_il,
        BackendForeignLanguages = ["il", "csharp"],
        set_option(optimize_constructor_last_call, bool(no), !Globals)
    ;
        Target = target_csharp,
        BackendForeignLanguages = ["csharp"]
    ;
        Target = target_java,
        BackendForeignLanguages = ["java"]
    ;
        Target = target_x86_64,
        BackendForeignLanguages = ["c"]
    ;
        Target = target_erlang,
        BackendForeignLanguages = ["erlang"],
        set_option(optimize_constructor_last_call, bool(no), !Globals),
        set_option(allow_multi_arm_switches, bool(no), !Globals)
    ),

    % Only set the backend foreign languages if they are unset.
    globals.lookup_accumulating_option(!.Globals,
        backend_foreign_languages, CurrentBackendForeignLanguage),
    (
        CurrentBackendForeignLanguage = [],
        globals.set_option(backend_foreign_languages,
            accumulating(BackendForeignLanguages), !Globals)
    ;
        CurrentBackendForeignLanguage = [_ | _]
    ),

    globals.lookup_int_option(!.Globals, compare_specialization,
        CompareSpec),
    ( CompareSpec < 0 ->
        % This indicates that the option was not set by the user;
        % we should set the option to the default value. This value
        % may be back end specific, since different back ends have
        % different performance tradeoffs.
        (
            HighLevelCode = no,
            globals.set_option(compare_specialization, int(13), !Globals)
        ;
            HighLevelCode = yes,
            globals.set_option(compare_specialization, int(14), !Globals)
        )
    ;
        true
    ),

    (
        % In the non-C backends, it may not be possible to cast a value
        % of a non-enum du type to an integer.
        Target = target_c,

        % To ensure that all constants in general du types are
        % allocated in one word, make_tags.m need to have at least one
        % tag bit left over after --reserve-tags possibly takes one.
        ( TagsMethod = tags_low
        ; TagsMethod = tags_high
        ),
        NumTagBits >= 2
    ->
        globals.set_option(can_compare_constants_as_ints, bool(yes),
            !Globals)
    ;
        globals.set_option(can_compare_constants_as_ints, bool(no),
            !Globals)
    ),

    (
        HighLevelCode = no,
        postprocess_options_lowlevel(!Globals)
    ;
        HighLevelCode = yes
    ),
    postprocess_options_libgrades(!Globals, !Errors),
    globals_init_mutables(!.Globals, !IO).

    % These option implications only affect the low-level (LLDS) code
    % generator.  They may in fact be harmful if set for the high-level
    % code generator, because sometimes the same option has different
    % meanings and implications in the two backends.
    %
:- pred postprocess_options_lowlevel(globals::in, globals::out) is det.

postprocess_options_lowlevel(!Globals) :-
    % --optimize-saved-vars-cell requires --use-local-vars for
    % acceptable performance.
    option_implies(optimize_saved_vars_cell, use_local_vars, bool(yes),
        !Globals),

    % --optimize-frames requires --optimize-labels and
    % --optimize-jumps
    option_implies(optimize_frames, optimize_labels, bool(yes), !Globals),
    option_implies(optimize_frames, optimize_jumps, bool(yes), !Globals),

    % --optimize-proc-dups is implemented only with --trad-passes.
    option_implies(optimize_proc_dups, trad_passes, bool(yes), !Globals),

    globals.lookup_bool_option(!.Globals, optimize_frames, OptFrames),
    globals.lookup_bool_option(!.Globals, use_local_vars, OptLocalVars),
    globals.lookup_int_option(!.Globals, optimize_repeat, OptRepeat),
    (
        ( OptFrames = yes
        ; OptLocalVars = yes
        ),
        OptRepeat < 1
    ->
        % The frame optimization and the local vars optimization depend on
        % the jump and label optimization having been done. They are turned
        % on above, but they still won't be executed unless optimize_repeat
        % is at least one.
        globals.set_option(optimize_repeat, int(1), !Globals)
    ;
        true
    ),

    % The setting of static_ground_floats is governed only by the settings
    % of unboxed_float and static_ground_cells.
    globals.lookup_bool_option(!.Globals, unboxed_float, UnboxedFloat),
    (
        UnboxedFloat = yes,
        % If we're using unboxed (MR_Word-sized) floats, floating point values
        % are always constants.
        StaticGroundFloats = yes
    ;
        UnboxedFloat = no,
        % If we're using boxed floats, then we can generate a static constant
        % variable to hold a float constant, and gcc doesn't mind us converting
        % from its address to MR_Word in a static initializer. In theory,
        % we should do this with --static-ground-terms. However, the code
        % generator does not yet handle the dynamic creation of boxed float
        % constants, and assumes that binding a variable to a constant
        % generates no code.
        StaticGroundFloats = yes
    ),
    globals.set_option(static_ground_floats, bool(StaticGroundFloats),
        !Globals),

    % The setting of static_code_addresses is governed only by the settings
    % of gcc_non_local_gotos and asm_labels.
    globals.lookup_bool_option(!.Globals, gcc_non_local_gotos, NonLocalGotos),
    globals.lookup_bool_option(!.Globals, asm_labels, AsmLabels),
    (
        NonLocalGotos = yes,
        AsmLabels = no
    ->
        % With non-local gotos but no asm labels, jumps to code addresses
        % in different c_modules must be done via global variables; the value
        % of these global variables is not constant (i.e. not computable at
        % load time), since they can't be initialized until we call
        % init_modules().
        StaticCodeAddrs = no
    ;
        StaticCodeAddrs = yes
    ),
    globals.set_option(static_code_addresses, bool(StaticCodeAddrs), !Globals).


    % option_implies(SourceBoolOption, ImpliedOption, ImpliedOptionValue):
    % If the SourceBoolOption is set to yes, then the ImpliedOption is set
    % to ImpliedOptionValue.
    %
:- pred option_implies(option::in, option::in, option_data::in,
    globals::in, globals::out) is det.

option_implies(SourceOption, ImpliedOption, ImpliedOptionValue, !Globals) :-
    globals.lookup_bool_option(!.Globals, SourceOption, SourceOptionValue),
    (
        SourceOptionValue = yes,
        globals.set_option(ImpliedOption, ImpliedOptionValue, !Globals)
    ;
        SourceOptionValue = no
    ).

    % option_neg_implies(SourceBoolOption, ImpliedOption, ImpliedOptionValue):
    % If the SourceBoolOption is set to no, then the ImpliedOption is set
    % to ImpliedOptionValue.
    %
:- pred option_neg_implies(option::in, option::in, option_data::in,
    globals::in, globals::out) is det.

option_neg_implies(SourceOption, ImpliedOption, ImpliedOptionValue,
        !Globals) :-
    globals.lookup_bool_option(!.Globals, SourceOption, SourceOptionValue),
    (
        SourceOptionValue = yes
    ;
        SourceOptionValue = no,
        globals.set_option(ImpliedOption, ImpliedOptionValue, !Globals)
    ).

    % option_requires(SourceBoolOption, RequiredOption, RequiredOptionValue,
    %   ErrorMsg, !Errors):
    % If the SourceBoolOption is set to yes, and RequiredOption is not set
    % to RequiredOptionValue, then add the given error message to the list.
    %
:- pred option_requires(option::in, option::in, option_data::in, string::in,
    globals::in, list(string)::in, list(string)::out) is det.

option_requires(SourceOption, RequiredOption, RequiredOptionValue,
        ErrorMessage, Globals, !Errors) :-
    globals.lookup_bool_option(Globals, SourceOption, SourceOptionValue),
    globals.lookup_option(Globals, RequiredOption, OptionValue),
    (
        SourceOptionValue = yes,
        OptionValue \= RequiredOptionValue
    ->
        add_error(ErrorMessage, !Errors)
    ;
        true
    ).

    % Smart recompilation does not yet work with all options (in particular
    % `--intermodule-optimization' and `--no-target-code-only'). Disable smart
    % recompilation if such an option is set, maybe issuing a warning.
    %
:- pred maybe_disable_smart_recompilation(bool::in, option::in, bool::in,
    string::in, globals::in, globals::out, io::di, io::uo) is det.

maybe_disable_smart_recompilation(Smart, ConflictingOption,
        ValueToDisableSmart, OptionDescr, !Globals, !IO) :-
    globals.lookup_bool_option(!.Globals, ConflictingOption, Value),
    (
        Smart = yes,
        Value = ValueToDisableSmart
    ->
        disable_smart_recompilation(OptionDescr, !Globals, !IO)
    ;
        true
    ).

:- pred disable_smart_recompilation(string::in, globals::in, globals::out,
    io::di, io::uo) is det.

disable_smart_recompilation(OptionDescr, !Globals, !IO) :-
    io_set_disable_smart_recompilation(yes, !IO),
    globals.set_option(smart_recompilation, bool(no), !Globals),
    globals.lookup_bool_option(!.Globals, warn_smart_recompilation, WarnSmart),
    (
        WarnSmart = yes,
        io.write_string("Warning: smart recompilation " ++
            "does not yet work with ", !IO),
        io.write_string(OptionDescr, !IO),
        io.write_string(".\n", !IO),
        globals.lookup_bool_option(!.Globals, halt_at_warn, Halt),
        (
            Halt = yes,
            io.set_exit_status(1, !IO)
        ;
            Halt = no
        )
    ;
        WarnSmart = no
    ).

usage_errors(Errors, !IO) :-
    io.progname_base("mercury_compile", ProgName, !IO),
    list.foldl(write_error_plain_with_progname(ProgName), Errors, !IO),
    io.set_exit_status(1, !IO),
    usage(!IO).

display_compiler_version(!IO) :-
    library.version(Version, Fullarch),
    io.write_strings([
        "Mercury Compiler, version ", Version, ", on ", Fullarch, "\n",
        "Copyright (C) 1993-2014 The University of Melbourne\n"
    ], !IO).

:- mutable(already_printed_usage, bool, no, ground,
    [untrailed, attach_to_io_state]).

usage(!IO) :-
    % usage is called from many places; ensure that we don't print the
    % duplicate copies of the message.
    get_already_printed_usage(AlreadyPrinted, !IO),
    (
        AlreadyPrinted = no,
        display_compiler_version(!IO),
        io.write_strings([
            "Usage: mmc [<options>] <arguments>\n",
            "Use `mmc --help' for more information.\n"
        ], !IO),
        set_already_printed_usage(yes, !IO)
    ;
        AlreadyPrinted = yes
    ).

long_usage(!IO) :-
    % long_usage is called from only one place, so can't print duplicate
    % copies of the long usage message. We can print both a short and along
    % usage message, but there is no simple way to avoid that.
    library.version(Version, Fullarch),
    io.write_strings(["Name: mmc -- Melbourne Mercury Compiler, version ",
        Version, ", on ", Fullarch, "\n"], !IO),
    io.write_string("Copyright: Copyright (C) 1993-2014 " ++
        "The University of Melbourne\n", !IO),
    io.write_string("Usage: mmc [<options>] <arguments>\n", !IO),
    io.write_string("Arguments:\n", !IO),
    io.write_string("\tArguments ending in `.m' " ++
        "are assumed to be source file names.\n", !IO),
    io.write_string("\tArguments that do not end in `.m' " ++
        "are assumed to be module names.\n", !IO),
    io.write_string("\tArguments in the form @file are replaced " ++
        "with the contents of the file.\n", !IO),
    io.write_string("Options:\n", !IO),
    options_help(!IO).

%-----------------------------------------------------------------------------%
%
% Code for postprocessing the library grade set
%

    % Apply some sanity checks to the library grade set and then apply any
    % library grade filters to that set.
    %
    % XXX we could do better with the sanity checks, currently we only
    % check that all the grade components are valid and that there are
    % no duplicate grade components.
    %
:- pred postprocess_options_libgrades(globals::in, globals::out,
    list(string)::in, list(string)::out) is det.

postprocess_options_libgrades(!Globals, !Errors) :-
    globals.lookup_accumulating_option(!.Globals, libgrades_include_components,
        IncludeComponentStrs),
    globals.lookup_accumulating_option(!.Globals, libgrades_exclude_components,
        OmitComponentStrs),
    list.foldl2(string_to_grade_component("included"),
        IncludeComponentStrs, [], IncludeComponents, !Errors),
    list.foldl2(string_to_grade_component("excluded"),
        OmitComponentStrs, [], OmitComponents, !Errors),
    some [!LibGrades] (
        globals.lookup_accumulating_option(!.Globals, libgrades, !:LibGrades),
        %
        % NOTE: the two calls to foldl2 here will preserve the original
        %       relative ordering of the library grades.
        %
        list.foldl2(filter_grade(must_contain, IncludeComponents),
            !.LibGrades, [], !:LibGrades, !Errors),
        list.foldl2(filter_grade(must_not_contain, OmitComponents),
            !.LibGrades, [], !:LibGrades, !Errors),
        globals.set_option(libgrades, accumulating(!.LibGrades), !Globals)
    ).

    % string_to_grade_component(OptionStr, Comp, !Comps, !Errors):
    %
    % If `Comp' is a string that represents a valid grade component
    % then add it to !Comps.  If it is not then emit an error message.
    % `OptionStr' should be the name of the command line option for
    % which the error is to be reported.
    %
:- pred string_to_grade_component(string::in, string::in,
    list(string)::in, list(string)::out,
    list(string)::in, list(string)::out) is det.

string_to_grade_component(FilterDesc, Comp, !Comps, !Errors) :-
    ( grade_component_table(Comp, _, _, _, _) ->
        !:Comps = [Comp | !.Comps]
    ;
        add_error("unknown " ++ FilterDesc ++ " library grade component: "
            ++ Comp, !Errors)
    ).

    % filter_grade(FilterPred, Components, GradeString, !Grades, !Errors):
    %
    % Convert `GradeString' into a list of grade component strings, and
    % then check whether the given grade should be filtered from the
    % library grade set by applying the closure `FilterPred(Components)',
    % to that list.  The grade is removed from the library grade set if
    % that application fails.
    %
    % Emits an error if `GradeString' cannot be converted into a list
    % of grade component strings.
    %
:- pred filter_grade(pred(list(string), list(string))
    ::in(pred(in, in) is semidet), list(string)::in,
    string::in, list(string)::in, list(string)::out,
    list(string)::in, list(string)::out) is det.

filter_grade(FilterPred, CondComponents, GradeString, !Grades, !Errors) :-
    grade_string_to_comp_strings(GradeString, MaybeGrade, !Errors),
    (
        MaybeGrade = yes(GradeComponents),
        ( FilterPred(CondComponents, GradeComponents) ->
            !:Grades = [GradeString | !.Grades]
        ;
            true
        )
    ;
        MaybeGrade = no
    ).

:- pred must_contain(list(string)::in, list(string)::in) is semidet.

must_contain(IncludeComponents, GradeComponents) :-
    all [Component] (
        list.member(Component, IncludeComponents)
    =>
        list.member(Component, GradeComponents)
    ).

:- pred must_not_contain(list(string)::in, list(string)::in) is semidet.

must_not_contain(OmitComponents, GradeComponents) :-
    all [Component] (
        list.member(Component, OmitComponents)
    =>
        not list.member(Component, GradeComponents)
    ).

    % Convert a grade string into a list of component strings.
    % Emit an invalid grade error if the conversion fails.
    %
:- pred grade_string_to_comp_strings(string::in,
    maybe(list(string))::out, list(string)::in, list(string)::out)
    is det.

grade_string_to_comp_strings(GradeString, MaybeGrade, !Errors) :-
    (
        split_grade_string(GradeString, ComponentStrs),
        StrToComp = (pred(Str::in, Str::out) is semidet :-
            grade_component_table(Str, _, _, _, _)
        ),
        list.map(StrToComp, ComponentStrs, Components0)
    ->
        list.sort_and_remove_dups(Components0, Components),
        ( list.length(Components0) > list.length(Components) ->
            add_error("invalid library grade: " ++ GradeString, !Errors),
            MaybeGrade = no
        ;
            MaybeGrade = yes(Components)
        )
    ;
        add_error("invalid library grade: " ++ GradeString, !Errors),
        MaybeGrade = no
    ).

%-----------------------------------------------------------------------------%

    % IMPORTANT: any changes here may require similar changes to other files,
    % see the list of files at the top of runtime/mercury_grade.h
    %
    % The grade_component type should have one constructor for each
    % dimension of the grade. It is used when converting the components
    % of the grade string to make sure the grade string doesn't contain
    % more than one value for each dimension (eg *.gc.agc).
    % Adding a value here will require adding clauses to the
    % grade_component_table.
    %
    % A --grade option causes all the grade dependent options to be
    % reset, and only those described by the grade string to be set.
    % The value to which a grade option should be reset should be given
    % in the grade_start_values table below.
    %
    % The ordering of the components here is the same as the order used in
    % scripts/canonical_grand.sh-subr, and any change here will require a
    % corresponding change there. The only place where the ordering actually
    % matters is for constructing the pathname for the grade of the library,
    % etc for linking (and installation).
    %
:- type grade_component
    --->    comp_gcc_ext        % gcc extensions etc. -- see
                                % grade_component_table
    ;       comp_par            % parallelism / multithreading
    ;       comp_par_threadscope
                                % Whether to support theadscope profiling of
                                % parallel grades.
    ;       comp_gc             % the kind of GC to use
    ;       comp_prof           % what profiling options to use
    ;       comp_term_size      % whether or not to record term sizes
    ;       comp_trail          % whether or not to use trailing
    ;       comp_minimal_model  % whether we set up for minimal model tabling
    ;       comp_pregen_spf     % whether to assume settings for the
                                % pregenerated C source distribution;
                                % and whether or not to use single precision
                                % floating point values.
    ;       comp_pic            % Do we need to reserve a register for
                                % PIC (position independent code)?
    ;       comp_lowlevel       % what to do to target code
    ;       comp_trace          % tracing/debugging options
    ;       comp_stack_extend   % automatic stack extension
    ;       comp_regions.       % Whether or not to use region-based memory
                                % management.

convert_grade_option(GradeString, Options0, Options) :-
    reset_grade_options(Options0, Options1),
    split_grade_string(GradeString, Components),
    set.init(NoComps),
    list.foldl2((pred(CompStr::in, Opts0::in, Opts::out,
            CompSet0::in, CompSet::out) is semidet :-
        grade_component_table(CompStr, Comp, CompOpts, MaybeTargets, _),

        % Check that the component isn't mentioned more than once.
        \+ set.member(Comp, CompSet0),
        set.insert(Comp, CompSet0, CompSet),
        add_option_list(CompOpts, Opts0, Opts1),

        % XXX Here the behaviour matches what used to happen and that is
        % to only set the target option iff there was only one possible target.
        % Is this a bug?
        ( MaybeTargets = yes([Target]) ->
            add_option_list([target - Target], Opts1, Opts)
        ;
            Opts = Opts1
        )
    ), Components, Options1, Options, NoComps, _FinalComps).

:- pred add_option_list(list(pair(option, option_data))::in, option_table::in,
    option_table::out) is det.

add_option_list(CompOpts, Opts0, Opts) :-
    list.foldl((pred(Opt::in, Opts1::in, Opts2::out) is det :-
        Opt = Option - Data,
        map.set(Option, Data, Opts1, Opts2)
    ), CompOpts, Opts0, Opts).

grade_directory_component(Globals, Grade) :-
    compute_grade(Globals, Grade0),
    % Strip out the `.picreg' part of the grade -- `.picreg' is
    % implied by the file names (.pic_o vs .o, `.a' vs `.so').
    %
    (
        string.sub_string_search(Grade0, ".picreg", PicRegIndex),
        string.split(Grade0, PicRegIndex, LeftPart, RightPart0),
        string.append(".picreg", RightPart, RightPart0)
    ->
        Grade = LeftPart ++ RightPart
    ;
        Grade = Grade0
    ).

compute_grade(Globals, Grade) :-
    globals.get_options(Globals, Options),
    compute_grade_components(Options, Components),
    (
        Components = [],
        Grade = "none"
    ;
        Components = [_ | _],
        construct_string(Components, Grade)
    ).

:- pred construct_string(list(pair(grade_component, string))::in, string::out)
    is det.

construct_string([], "").
construct_string([_ - Bit | Bits], Grade) :-
    (
        Bits = [_ | _],
        construct_string(Bits, Grade0),
        string.append_list([Bit, ".", Grade0], Grade)
    ;
        Bits = [],
        Grade = Bit
    ).

:- pred compute_grade_components(option_table::in,
    list(pair(grade_component, string))::out) is det.

compute_grade_components(Options, GradeComponents) :-
    solutions((pred(CompData::out) is nondet :-
        grade_component_table(Name, Comp, CompOpts, MaybeTargets,
            IncludeInGradeString),

        % For a possible component of the grade string include, it in the
        % actual grade string if all the option settings that it implies
        % are true.
        all [Opt, Value] (
            list.member(Opt - Value, CompOpts)
        =>
            map.search(Options, Opt, Value)
        ),

        % Don't include `.mm' or `.dmm' in grade strings because they are just
        % synonyms for `.mmsc' and `.dmmsc' respectively.
        IncludeInGradeString = yes,

        % When checking gcc_ext there exist grades which can have more than one
        % possible target, ensure that the target in the options table matches
        % one of the possible targets.
        (
            MaybeTargets = yes(Targets),
            list.member(Target, Targets),
            map.search(Options, target, Target)
        ;
            MaybeTargets = no
        ),
        CompData = Comp - Name
    ), GradeComponents).

    % grade_component_table(ComponetStr, Component, Options, MaybeTargets,
    %   IncludeGradeStr):
    %
    % `IncludeGradeStr' is `yes' if the component should be included
    % in the grade string.  It is `no' for those components that are
    % just synonyms for other comments, as .mm is for .mmsc.
    %
    % NOTE: .picreg components are handled separately.
    % (see compute_grade_components/3).
    %
:- pred grade_component_table(string, grade_component,
    list(pair(option, option_data)), maybe(list(option_data)), bool).
:- mode grade_component_table(in, out, out, out, out) is semidet.
:- mode grade_component_table(out, in, out, out, out) is multi.
:- mode grade_component_table(out, out, out, out, out) is multi.

    % Base components.
    % These specify the basic compilation model we use,
    % including the choice of back-end and the use of gcc extensions.
grade_component_table("none", comp_gcc_ext, [
        asm_labels              - bool(no),
        gcc_non_local_gotos     - bool(no),
        gcc_global_registers    - bool(no),
        highlevel_code          - bool(no),
        gcc_nested_functions    - bool(no),
        highlevel_data          - bool(no)],
        yes([string("c")]), yes).
grade_component_table("reg", comp_gcc_ext, [
        asm_labels              - bool(no),
        gcc_non_local_gotos     - bool(no),
        gcc_global_registers    - bool(yes),
        highlevel_code          - bool(no),
        gcc_nested_functions    - bool(no),
        highlevel_data          - bool(no)],
        yes([string("c")]), yes).
grade_component_table("jump", comp_gcc_ext, [
        asm_labels              - bool(no),
        gcc_non_local_gotos     - bool(yes),
        gcc_global_registers    - bool(no),
        highlevel_code          - bool(no),
        gcc_nested_functions    - bool(no),
        highlevel_data          - bool(no)],
        yes([string("c")]), yes).
grade_component_table("asm_jump", comp_gcc_ext, [
        asm_labels              - bool(yes),
        gcc_non_local_gotos     - bool(yes),
        gcc_global_registers    - bool(no),
        highlevel_code          - bool(no),
        gcc_nested_functions    - bool(no),
        highlevel_data          - bool(no)],
        yes([string("c")]), yes).
grade_component_table("fast", comp_gcc_ext, [
        asm_labels              - bool(no),
        gcc_non_local_gotos     - bool(yes),
        gcc_global_registers    - bool(yes),
        highlevel_code          - bool(no),
        gcc_nested_functions    - bool(no),
        highlevel_data          - bool(no)],
        yes([string("c")]), yes).
grade_component_table("asm_fast", comp_gcc_ext, [
        asm_labels              - bool(yes),
        gcc_non_local_gotos     - bool(yes),
        gcc_global_registers    - bool(yes),
        highlevel_code          - bool(no),
        gcc_nested_functions    - bool(no),
        highlevel_data          - bool(no)],
        yes([string("c")]), yes).
grade_component_table("hl", comp_gcc_ext, [
        asm_labels              - bool(no),
        gcc_non_local_gotos     - bool(no),
        gcc_global_registers    - bool(no),
        highlevel_code          - bool(yes),
        gcc_nested_functions    - bool(no),
        highlevel_data          - bool(yes)],
        yes([string("c"), string("asm")]), yes).
grade_component_table("hlc", comp_gcc_ext, [
        asm_labels              - bool(no),
        gcc_non_local_gotos     - bool(no),
        gcc_global_registers    - bool(no),
        highlevel_code          - bool(yes),
        gcc_nested_functions    - bool(no),
        highlevel_data          - bool(no)],
        yes([string("c"), string("asm")]), yes).
grade_component_table("hl_nest", comp_gcc_ext, [
        asm_labels              - bool(no),
        gcc_non_local_gotos     - bool(no),
        gcc_global_registers    - bool(no),
        highlevel_code          - bool(yes),
        gcc_nested_functions    - bool(yes),
        highlevel_data          - bool(yes)],
        yes([string("c"), string("asm")]), yes).
grade_component_table("hlc_nest", comp_gcc_ext, [
        asm_labels              - bool(no),
        gcc_non_local_gotos     - bool(no),
        gcc_global_registers    - bool(no),
        highlevel_code          - bool(yes),
        gcc_nested_functions    - bool(yes),
        highlevel_data          - bool(no)],
        yes([string("c"), string("asm")]), yes).
grade_component_table("il", comp_gcc_ext, [
        asm_labels              - bool(no),
        gcc_non_local_gotos     - bool(no),
        gcc_global_registers    - bool(no),
        highlevel_code          - bool(yes),
        gcc_nested_functions    - bool(no),
        highlevel_data          - bool(yes)],
        yes([string("il")]), yes).
grade_component_table("ilc", comp_gcc_ext, [
        asm_labels              - bool(no),
        gcc_non_local_gotos     - bool(no),
        gcc_global_registers    - bool(no),
        highlevel_code          - bool(yes),
        gcc_nested_functions    - bool(no),
        highlevel_data          - bool(no)],
        yes([string("il")]), yes).
grade_component_table("java", comp_gcc_ext, [
        asm_labels              - bool(no),
        gcc_non_local_gotos     - bool(no),
        gcc_global_registers    - bool(no),
        gcc_nested_functions    - bool(no),
        highlevel_code          - bool(yes),
        highlevel_data          - bool(yes)],
        yes([string("java")]), yes).
grade_component_table("csharp", comp_gcc_ext, [
        asm_labels              - bool(no),
        gcc_non_local_gotos     - bool(no),
        gcc_global_registers    - bool(no),
        gcc_nested_functions    - bool(no),
        highlevel_code          - bool(yes),
        highlevel_data          - bool(yes)],
        yes([string("csharp")]), yes).
grade_component_table("erlang", comp_gcc_ext, [
        asm_labels              - bool(no),
        gcc_non_local_gotos     - bool(no),
        gcc_global_registers    - bool(no),
        gcc_nested_functions    - bool(no),
        highlevel_code          - bool(no),
        highlevel_data          - bool(no)],
        yes([string("erlang")]), yes).

    % Parallelism/multithreading components.
grade_component_table("par", comp_par, [parallel - bool(yes)], no, yes).

    % Threadscope profiling in parallel grades.
grade_component_table("threadscope", comp_par_threadscope,
    [threadscope - bool(yes)], no, yes).

    % GC components.
grade_component_table("gc", comp_gc, [gc - string("boehm")], no, yes).
grade_component_table("gcd", comp_gc, [gc - string("boehm_debug")], no, yes).
grade_component_table("hgc", comp_gc, [gc - string("hgc")], no, yes).
grade_component_table("mps", comp_gc, [gc - string("mps")], no, yes).
grade_component_table("agc", comp_gc, [gc - string("accurate")], no, yes).

    % Profiling components.
grade_component_table("prof", comp_prof,
    [profile_time - bool(yes), profile_calls - bool(yes),
    profile_memory - bool(no), profile_deep - bool(no)], no, yes).
grade_component_table("proftime", comp_prof,
    [profile_time - bool(yes), profile_calls - bool(no),
    profile_memory - bool(no), profile_deep - bool(no)], no, yes).
grade_component_table("profcalls", comp_prof,
    [profile_time - bool(no), profile_calls - bool(yes),
    profile_memory - bool(no), profile_deep - bool(no)], no, yes).
grade_component_table("memprof", comp_prof,
    [profile_time - bool(no), profile_calls - bool(yes),
    profile_memory - bool(yes), profile_deep - bool(no)], no, yes).
grade_component_table("profall", comp_prof,
    [profile_time - bool(yes), profile_calls - bool(yes),
    profile_memory - bool(yes), profile_deep - bool(no)], no, yes).
grade_component_table("profdeep", comp_prof,
    [profile_time - bool(no), profile_calls - bool(no),
    profile_memory - bool(no), profile_deep - bool(yes)], no, yes).

    % Term size components.
grade_component_table("tsw", comp_term_size,
    [record_term_sizes_as_words - bool(yes),
    record_term_sizes_as_cells - bool(no)], no, yes).
grade_component_table("tsc", comp_term_size,
    [record_term_sizes_as_words - bool(no),
    record_term_sizes_as_cells - bool(yes)], no, yes).

    % Trailing components.
grade_component_table("tr", comp_trail,
    [use_trail - bool(yes), trail_segments - bool(no)], no, yes).
grade_component_table("trseg", comp_trail,
    [use_trail - bool(yes), trail_segments - bool(yes)], no, yes).

    % Minimal model tabling components.
    % NOTE: we do not include `.mm' and `.dmm' in grade strings
    % because they are just synonyms for `.mmsc' and `.dmmsc'.
    %
grade_component_table("mm", comp_minimal_model,
    [use_minimal_model_stack_copy - bool(yes),
    use_minimal_model_own_stacks - bool(no),
    minimal_model_debug - bool(no)], no, no).
grade_component_table("dmm", comp_minimal_model,
    [use_minimal_model_stack_copy - bool(yes),
    use_minimal_model_own_stacks - bool(no),
    minimal_model_debug - bool(yes)], no, no).
grade_component_table("mmsc", comp_minimal_model,
    [use_minimal_model_stack_copy - bool(yes),
    use_minimal_model_own_stacks - bool(no),
    minimal_model_debug - bool(no)], no, yes).
grade_component_table("dmmsc", comp_minimal_model,
    [use_minimal_model_stack_copy - bool(yes),
    use_minimal_model_own_stacks - bool(no),
    minimal_model_debug - bool(yes)], no, yes).
grade_component_table("mmos", comp_minimal_model,
    [use_minimal_model_stack_copy - bool(no),
    use_minimal_model_own_stacks - bool(yes),
    minimal_model_debug - bool(no)], no, yes).
grade_component_table("dmmos", comp_minimal_model,
    [use_minimal_model_stack_copy - bool(no),
    use_minimal_model_own_stacks - bool(yes),
    minimal_model_debug - bool(yes)], no, yes).

    % Settings for pre-generated source distribution
    % or single-precision floats.
grade_component_table("pregen", comp_pregen_spf,
    [pregenerated_dist - bool(yes)], no, yes).
grade_component_table("spf", comp_pregen_spf,
    [single_prec_float - bool(yes),
    unboxed_float - bool(yes)], no, yes).

    % Pic reg components.
grade_component_table("picreg", comp_pic, [pic_reg - bool(yes)], no, yes).

    % Debugging/Tracing components.
grade_component_table("decldebug", comp_trace,
    [exec_trace - bool(yes), decl_debug - bool(yes)], no, yes).
grade_component_table("debug", comp_trace,
    [exec_trace - bool(yes), decl_debug - bool(no)], no, yes).
grade_component_table("ssdebug", comp_trace,
    [source_to_source_debug - bool(yes)], no, yes).

    % Low (target) level debugging components.
grade_component_table("ll_debug", comp_lowlevel,
    [low_level_debug - bool(yes), target_debug - bool(yes)], no, yes).

    % Stack extension components.
grade_component_table("exts", comp_stack_extend,
    [extend_stacks_when_needed - bool(yes), stack_segments - bool(no)],
    no, yes).
grade_component_table("stseg", comp_stack_extend,
    [extend_stacks_when_needed - bool(no), stack_segments - bool(yes)],
    no, yes).

    % Region-based memory managment components
grade_component_table("rbmm", comp_regions,
    [use_regions - bool(yes),
    use_regions_debug - bool(no), use_regions_profiling - bool(no)],
    no, yes).
grade_component_table("rbmmd", comp_regions,
    [use_regions - bool(yes),
    use_regions_debug - bool(yes), use_regions_profiling - bool(no)],
    no, yes).
grade_component_table("rbmmp", comp_regions,
    [use_regions - bool(yes),
    use_regions_debug - bool(no), use_regions_profiling - bool(yes)],
    no, yes).
grade_component_table("rbmmdp", comp_regions,
    [use_regions - bool(yes),
    use_regions_debug - bool(yes), use_regions_profiling - bool(yes)],
    no, yes).

:- pred reset_grade_options(option_table::in, option_table::out) is det.

reset_grade_options(Options0, Options) :-
    solutions.aggregate(grade_start_values,
        (pred(Pair::in, Opts0::in, Opts::out) is det :-
            Pair = Option - Value,
            map.set(Option, Value, Opts0, Opts)
        ), Options0, Options).

:- pred grade_start_values(pair(option, option_data)::out) is multi.

grade_start_values(asm_labels - bool(no)).
grade_start_values(gcc_non_local_gotos - bool(no)).
grade_start_values(gcc_global_registers - bool(no)).
grade_start_values(highlevel_code - bool(no)).
grade_start_values(highlevel_data - bool(no)).
grade_start_values(gcc_nested_functions - bool(no)).
grade_start_values(parallel - bool(no)).
grade_start_values(threadscope - bool(no)).
grade_start_values(gc - string("none")).
grade_start_values(profile_deep - bool(no)).
grade_start_values(profile_time - bool(no)).
grade_start_values(profile_calls - bool(no)).
grade_start_values(profile_memory - bool(no)).
grade_start_values(use_trail - bool(no)).
grade_start_values(trail_segments - bool(no)).
grade_start_values(use_minimal_model_stack_copy - bool(no)).
grade_start_values(use_minimal_model_own_stacks - bool(no)).
grade_start_values(minimal_model_debug - bool(no)).
grade_start_values(pregenerated_dist - bool(no)).
grade_start_values(single_prec_float - bool(no)).
grade_start_values(pic_reg - bool(no)).
grade_start_values(exec_trace - bool(no)).
grade_start_values(decl_debug - bool(no)).
grade_start_values(source_to_source_debug - bool(no)).
grade_start_values(extend_stacks_when_needed - bool(no)).
grade_start_values(stack_segments - bool(no)).
grade_start_values(use_regions - bool(no)).
grade_start_values(use_regions_debug - bool(no)).
grade_start_values(use_regions_profiling - bool(no)).
grade_start_values(low_level_debug - bool(no)).

:- pred split_grade_string(string::in, list(string)::out) is semidet.

split_grade_string(GradeStr, Components) :-
    string.to_char_list(GradeStr, Chars),
    split_grade_string_2(Chars, Components).

:- pred split_grade_string_2(list(char)::in, list(string)::out) is semidet.

split_grade_string_2([], []).
split_grade_string_2(Chars, Components) :-
    Chars = [_ | _],
    list.takewhile(char_is_not('.'), Chars, ThisChars, RestChars0),
    string.from_char_list(ThisChars, ThisComponent),
    Components = [ThisComponent | RestComponents],
    (
        RestChars0 = [_ | RestChars], % Discard the `.'.
        split_grade_string_2(RestChars, RestComponents)
    ;
        RestChars0 = [],
        RestComponents = []
    ).

:- pred char_is_not(char::in, char::in) is semidet.

char_is_not(A, B) :-
    A \= B.

%-----------------------------------------------------------------------------%

% This predicate converts a symbolic name for a set of verbosity options
% (a "dump alias") into the string consisting of those options' characters.
%
% The meanings of the option characters are documented by doc/user_guide.texi
% and by compiler/hlds_out.m. The latter is more authoritative :-)
%
% You are welcome to add more aliases.

:- pred convert_dump_alias(string::in, string::out) is semidet.

% None of the 'all' aliases actually include all the options.
convert_dump_alias("ALL", "abcdEfgilmnprstuvzBCDIMPRSTUZ").
convert_dump_alias("allD", "abcdEfgilmnprstuvzBCDMPT").
convert_dump_alias("all", "abcdEfgilmnprstuvzBCMPSTZ").
convert_dump_alias("most", "bcdfgilmnprstvzP").
convert_dump_alias("cmp", "bdfgilmnprstuvzP").
convert_dump_alias("trans", "bcdglmnstuvz").
convert_dump_alias("mintrans", "bcdglmnstvz").
convert_dump_alias("codegen", "dfnprsu").
convert_dump_alias("vanessa", "ltuCIU").
convert_dump_alias("min", "ilv").
convert_dump_alias("paths", "cP").
convert_dump_alias("petdr", "din").
convert_dump_alias("detism", "divM").
convert_dump_alias("mm", "bdgvP").      % For debugging minimal model.
convert_dump_alias("osv", "bcdglmnpruvP").
    % for debugging --optimize-saved-vars-cell
convert_dump_alias("ctgc", "cdinpGDRS").
convert_dump_alias("vars", "npBis").    % Var instantiations, liveness etc.
convert_dump_alias("statevar", "gvCP").
convert_dump_alias("lco", "agiuvzD").
convert_dump_alias("poly", "vxX").

%-----------------------------------------------------------------------------%
:- end_module handle_options.
%-----------------------------------------------------------------------------%
