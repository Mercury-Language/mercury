%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: handle_options.m.
% Main authors: fjh, zs.
%
% This module does post-processing on the command-line options, after
% getopt has done its stuff.
%
%---------------------------------------------------------------------------%

:- module libs.handle_options.
:- interface.

:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % Generate a dummy globals value based on the default values of the
    % options.
    %
:- pred generate_default_globals(globals::out, io::di, io::uo) is det.

    % handle_given_options(Args, OptionArgs, NonOptionArgs, Specs,
    %   Globals, !IO).
    %
:- pred handle_given_options(list(string)::in,
    list(string)::out, list(string)::out, list(error_spec)::out,
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

    % usage_errors(Globals, Specs, !IO)
    %
    % Print the list of error messages, and then the usage message.
    %
:- pred usage_errors(globals::in, list(error_spec)::in, io::di, io::uo) is det.

    % Display usage message.
    %
:- pred usage(io::di, io::uo) is det.

    % Display long usage message for help.
    %
:- pred long_usage(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module libs.compiler_util.
:- import_module libs.compute_grade.
:- import_module libs.op_mode.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module mdbcomp.
:- import_module mdbcomp.feedback.

:- import_module bool.
:- import_module dir.
:- import_module getopt_io.
:- import_module int.
:- import_module library.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.

%---------------------------------------------------------------------------%

generate_default_globals(DefaultGlobals, !IO) :-
    handle_given_options([], _, _, _, DefaultGlobals, !IO).

handle_given_options(Args0, OptionArgs, Args, Specs, !:Globals, !IO) :-
    trace [compile_time(flag("debug_handle_given_options")), io(!TIO)] (
        io.nl(!TIO),
        io.write_string("original arguments\n", !TIO),
        dump_arguments(Args0, !TIO)
    ),
    process_given_options(Args0, OptionArgs, Args, Result, !IO),
    trace [compile_time(flag("debug_handle_given_options")), io(!TIO)] (
        io.nl(!TIO),
        io.write_string("final option arguments\n", !TIO),
        dump_arguments(OptionArgs, !TIO),

        io.nl(!TIO),
        io.write_string("final non-option arguments\n", !TIO),
        dump_arguments(Args, !TIO)
    ),
    convert_option_table_result_to_globals(Result, Specs, !:Globals, !IO),
    (
        Specs = [_ | _]
        % Do NOT set the exit status. This predicate may be called before all
        % the options are known, so the errors may not be valid.
    ;
        Specs = [],
        % XXX Why is this not tested for at the same time as the other reasons
        % for disabling smart recompilation?
        globals.get_op_mode(!.Globals, OpMode),
        globals.lookup_bool_option(!.Globals, smart_recompilation, Smart),
        ( if
            Smart = yes,
            OpMode = opm_top_args(opma_augment(
                opmau_generate_code(opmcg_target_object_and_executable)))
        then
            % XXX Currently smart recompilation doesn't check that all the
            % files needed to link are present and up-to-date, so disable it.
            disable_smart_recompilation("linking", !Globals, !IO)
        else
            true
        )
    ).

separate_option_args(Args0, OptionArgs, Args, !IO) :-
    process_given_options(Args0, OptionArgs, Args, _, !IO).

    % process_given_options(Args, OptionArgs, NonOptionArgs, MaybeOptionTable,
    %   !IO):
    %
    % Process the options, but don't do any post-processing. This is mainly
    % useful for separating the list of arguments into option and non-option
    % arguments.
    %
:- pred process_given_options(list(string)::in,
    list(string)::out, list(string)::out, maybe_option_table(option)::out,
    io::di, io::uo) is det.

process_given_options(Args0, OptionArgs, Args, Result, !IO) :-
    OptionOps = option_ops(short_option, long_option,
        option_defaults, special_handler),
    getopt_io.process_options(OptionOps, Args0, OptionArgs, Args, Result, !IO).

:- pred dump_arguments(list(string)::in, io::di, io::uo) is det.

dump_arguments([], !IO).
dump_arguments([Arg | Args], !IO) :-
    io.write_string("    <", !IO),
    io.write_string(Arg, !IO),
    io.write_string(">\n", !IO),
    dump_arguments(Args, !IO).

%---------------------------------------------------------------------------%

    % Convert string-valued options into the appropriate enumeration types,
    % and process implications among the options (i.e. situations where setting
    % one option implies setting/unsetting another one).
    %
:- pred convert_option_table_result_to_globals(maybe_option_table(option)::in,
    list(error_spec)::out, globals::out, io::di, io::uo) is det.

convert_option_table_result_to_globals(MaybeOptionTable0, !:Specs, Globals,
        !IO) :-
    (
        MaybeOptionTable0 = error(ErrorMessage),
        OptionTablePieces = [words(ErrorMessage)],
        OptionTableMsg = error_msg(no, do_not_treat_as_first, 0,
            [always(OptionTablePieces)]),
        OptionTableSpec = error_spec(severity_error, phase_options,
            [OptionTableMsg]),
        !:Specs = [OptionTableSpec],
        generate_default_globals(Globals, !IO)
    ;
        MaybeOptionTable0 = ok(OptionTable0),
        check_option_values(OptionTable0, OptionTable, Target, GC_Method,
            TagsMethod, TermNorm, Term2Norm, TraceLevel, TraceSuppress,
            SSTraceLevel, MaybeThreadSafe, C_CompilerType, CSharp_CompilerType,
            ReuseStrategy,
            MaybeFeedbackInfo, HostEnvType, SystemEnvType, TargetEnvType,
            LimitErrorContextsMap, !:Specs, !IO),
        decide_op_mode(OptionTable, OpMode, OtherOpModes),
        (
            OtherOpModes = []
        ;
            OtherOpModes = [_ | _],
            OpModeStrs = list.map(op_mode_to_option_string(OptionTable),
                [OpMode | OtherOpModes]),
            OpModePieces = [words("Error: only one of the following options"),
                words("may be given:")] ++
                list_to_quoted_pieces(OpModeStrs) ++ [suffix("."), nl],
            add_error(phase_options, OpModePieces, !Specs)
        ),
        (
            !.Specs = [],
            convert_options_to_globals(OptionTable, OpMode, Target,
                GC_Method, TagsMethod, TermNorm, Term2Norm,
                TraceLevel, TraceSuppress, SSTraceLevel,
                MaybeThreadSafe, C_CompilerType, CSharp_CompilerType,
                ReuseStrategy, MaybeFeedbackInfo,
                HostEnvType, SystemEnvType, TargetEnvType,
                LimitErrorContextsMap, !Specs, Globals, !IO)
        ;
            !.Specs = [_ | _],
            generate_default_globals(Globals, !IO)
        )
    ).

:- pred check_option_values(option_table::in, option_table::out,
    compilation_target::out, gc_method::out, tags_method::out,
    termination_norm::out, termination_norm::out, trace_level::out,
    trace_suppress_items::out, ssdb_trace_level::out, may_be_thread_safe::out,
    c_compiler_type::out, csharp_compiler_type::out,
    reuse_strategy::out,
    maybe(feedback_info)::out, env_type::out, env_type::out, env_type::out,
    limit_error_contexts_map::out,
    list(error_spec)::out, io::di, io::uo) is det.

check_option_values(!OptionTable, Target, GC_Method, TagsMethod,
        TermNorm, Term2Norm, TraceLevel, TraceSuppress, SSTraceLevel,
        MaybeThreadSafe, C_CompilerType, CSharp_CompilerType,
        ReuseStrategy, MaybeFeedbackInfo,
        HostEnvType, SystemEnvType, TargetEnvType, LimitErrorContextsMap,
        !:Specs, !IO) :-
    !:Specs = [],
    raw_lookup_string_option(!.OptionTable, target, TargetStr),
    ( if convert_target(TargetStr, TargetPrime) then
        Target = TargetPrime
    else
        Target = target_c,     % dummy
        TargetSpec =
            [words("Invalid argument"), quote(TargetStr), words("to the"),
            quote("--target"), words("option; must be")] ++
            list_to_quoted_pieces_or(["c", "java", "csharp", "erlang"]) ++
            [suffix("."), nl],
        add_error(phase_options, TargetSpec, !Specs)
    ),

    raw_lookup_string_option(!.OptionTable, gc, GC_MethodStr),
    ( if convert_gc_method(GC_MethodStr, GC_MethodPrime) then
        GC_Method = GC_MethodPrime
    else
        GC_Method = gc_none,   % dummy
        GCMethodSpec =
            [words("Invalid argument"), quote(GC_MethodStr),
            words("to the"), quote("--gc"), words("option; must be")] ++
            list_to_quoted_pieces_or(["none", "conservative", "boehm", "hgc",
                "accurate", "automatic"]) ++ [suffix("."), nl],
        add_error(phase_options, GCMethodSpec, !Specs)
    ),

    raw_lookup_string_option(!.OptionTable, tags, TagsMethodStr),
    ( if convert_tags_method(TagsMethodStr, TagsMethodPrime) then
        TagsMethod = TagsMethodPrime
    else
        TagsMethod = tags_none,  % dummy
        TagsMethodSpec =
            [words("Invalid argument"), quote(TagsMethodStr), words("to the"),
            quote("--tags"), words("option; must be")] ++
            list_to_quoted_pieces_or(["none", "low", "high"]) ++
            [suffix("."), nl],
        add_error(phase_options, TagsMethodSpec, !Specs)
    ),

    raw_lookup_int_option(!.OptionTable, fact_table_hash_percent_full,
        FactTablePercentFull),
    ( if
        FactTablePercentFull >= 1,
        FactTablePercentFull =< 100
    then
        true
    else
        FactTablePercentFullSpec =
            [words("Invalid argument"), int_fixed(FactTablePercentFull),
            words("to the"), quote("--fact-table-hash-percent-full"),
            words("option; must be an integer between 1 and 100."), nl],
        add_error(phase_options, FactTablePercentFullSpec, !Specs)
    ),

    raw_lookup_int_option(!.OptionTable, inform_incomplete_switch_threshold,
        IncompleteSwitchThreshold),
    ( if
        IncompleteSwitchThreshold >= 0,
        IncompleteSwitchThreshold =< 100
    then
        true
    else
        IncompleteSwitchThresholdSpec =
            [words("Invalid argument"), int_fixed(IncompleteSwitchThreshold),
            words("to the"), quote("--inform-incomplete-switch-threshold"),
            words("option; must be an integer between 0 and 100."), nl],
        add_error(phase_options, IncompleteSwitchThresholdSpec, !Specs)
    ),

    raw_lookup_string_option(!.OptionTable, termination_norm, TermNormStr),
    ( if convert_termination_norm(TermNormStr, TermNormPrime) then
        TermNorm = TermNormPrime
    else
        TermNorm = norm_simple,  % dummy
        TermNormSpec =
            [words("Invalid argument"), quote(TermNormStr), words("to the"),
            quote("--termination-norm"), words("option; must be")] ++
            list_to_quoted_pieces_or(["simple", "total", "num-data-elems"]) ++
            [suffix("."), nl],
        add_error(phase_options, TermNormSpec, !Specs)
    ),

    raw_lookup_string_option(!.OptionTable, termination2_norm, Term2NormStr),
    ( if convert_termination_norm(Term2NormStr, Term2NormPrime) then
        Term2Norm = Term2NormPrime
    else
        Term2Norm = norm_simple, % dummy
        Term2NormSpec =
            [words("Invalid argument"), quote(TermNormStr), words("to the"),
            quote("--termination2-norm"), words("option; must be")] ++
            list_to_quoted_pieces_or(["simple", "total", "num-data-elems"]) ++
            [suffix("."), nl],
        add_error(phase_options, Term2NormSpec, !Specs)
    ),

    raw_lookup_bool_option(!.OptionTable, force_disable_tracing,
        ForceDisableTracing),
    (
        ForceDisableTracing = yes,
        TraceLevel = trace_level_none
    ;
        ForceDisableTracing = no,
        raw_lookup_string_option(!.OptionTable, trace_level, Trace),
        raw_lookup_bool_option(!.OptionTable, exec_trace, ExecTrace),
        raw_lookup_bool_option(!.OptionTable, decl_debug, DeclDebug),
        ( if
            convert_trace_level(Trace, ExecTrace, DeclDebug, MaybeTraceLevel)
        then
            (
                MaybeTraceLevel = yes(TraceLevel)
            ;
                MaybeTraceLevel = no,
                TraceLevel = trace_level_none,  % dummy
                InconsistentTraceLevelSpec =
                    [words("The specified trace level"), quote(Trace),
                    words("is not compatible with the value of the"),
                    quote("--decl-debug"), words("option."), nl],
                add_error(phase_options, InconsistentTraceLevelSpec, !Specs)
            )
        else
            TraceLevel = trace_level_none,  % dummy
            BadTraceLevelSpec =
                [words("Invalid argument"), quote(Trace), words("to the"),
                quote("--trace"), words("option; must be")] ++
                list_to_quoted_pieces_or(["minimum", "shallow", "deep",
                    "decl", "rep", "default"]) ++ [suffix("."), nl],
            add_error(phase_options, BadTraceLevelSpec, !Specs)
        )
    ),

    raw_lookup_string_option(!.OptionTable, suppress_trace, SuppressStr),
    ( if convert_trace_suppress(SuppressStr, TraceSuppressPrime) then
        TraceSuppress = TraceSuppressPrime
    else
        TraceSuppress = default_trace_suppress, % dummy
        TraceSuppressSpec =
            [words("Invalid argument"), quote(SuppressStr),
            words("to the"), quote("--suppress-trace"), words("option."), nl],
        % The set of valid options is a given language, not a simple set
        % of words, so there is no simple, short suggestion we can print.
        add_error(phase_options, TraceSuppressSpec, !Specs)
    ),

    raw_lookup_bool_option(!.OptionTable, force_disable_ssdebug,
        ForceDisableSSDB),
    (
        ForceDisableSSDB = yes,
        SSTraceLevel = none
    ;
        ForceDisableSSDB = no,
        raw_lookup_string_option(!.OptionTable, ssdb_trace_level, SSTrace),
        raw_lookup_bool_option(!.OptionTable, source_to_source_debug, SSDB),
        ( if convert_ssdb_trace_level(SSTrace, SSDB, SSTL) then
            SSTraceLevel = SSTL
        else
            SSTraceLevel = none,
            SSDBSpec =
                [words("Invalid argument"), quote(SSTrace), words("to the"),
                quote("--ssdb-trace"), words("option; must be")] ++
                list_to_quoted_pieces_or(["default", "none",
                    "shallow", "deep"]) ++ [suffix("."), nl],
            add_error(phase_options, SSDBSpec, !Specs)
        )
    ),

    raw_lookup_string_option(!.OptionTable, maybe_thread_safe_opt,
        MaybeThreadSafeStr),
    ( if
        convert_maybe_thread_safe(MaybeThreadSafeStr, MaybeThreadSafePrime)
    then
        MaybeThreadSafe = MaybeThreadSafePrime
    else
        MaybeThreadSafe = no, % dummy
        % XXX This should either be a boolean option, or the two values
        % should have descriptive names, not `yes' and `no'.
        % XXX They should definitely use a bespoke type inside the compiler.
        MTSSpec =
            [words("Invalid argument"), quote(MaybeThreadSafeStr),
            words("to the"), quote("--maybe-thread-safe"), words("option;"),
            words("must be")] ++
            list_to_quoted_pieces_or(["no", "yes"]) ++ [suffix("."), nl],
        add_error(phase_options, MTSSpec, !Specs)
    ),

    raw_lookup_string_option(!.OptionTable, dump_hlds_alias, DumpAlias),
    ( if DumpAlias = "" then
        true
    else if convert_dump_alias(DumpAlias, AliasDumpOptions) then
        map.set(dump_hlds_options, string(AliasDumpOptions), !OptionTable)
    else
        DumpAliasSpec =
            [words("Invalid argument"), quote(DumpAlias),
            words("to the"), quote("D"), words("(also known as"),
            quote("--dump-hlds-alias"), suffix(")"), words("option."), nl],
        add_error(phase_options, DumpAliasSpec, !Specs)
    ),

    some [!DumpOptions] (
        lookup_string_option(!.OptionTable, dump_hlds_options, !:DumpOptions),

        % If we have not specified what we want, dump bare procedures
        % as default.
        ( if !.DumpOptions = "" then
            !:DumpOptions = "x"
        else
            true
        ),

        % If we want structured insts in arg-modes, then we want arg-modes.
        ( if
            string.contains_char(!.DumpOptions, 'y'),
            not string.contains_char(!.DumpOptions, 'a')
        then
            !:DumpOptions = "a" ++ !.DumpOptions
        else
            true
        ),
        % If we want arg-modes, then we want the unifications they apply to.
        ( if
            string.contains_char(!.DumpOptions, 'a'),
            not string.contains_char(!.DumpOptions, 'u')
        then
            !:DumpOptions = "u" ++ !.DumpOptions
        else
            true
        ),
        % If we want any of the things that decorate predicates or the goals
        % inside predicates, then we want the predicates themselves.
        ( if
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
        then
            !:DumpOptions = "x" ++ !.DumpOptions
        else
            true
        ),
        map.set(dump_hlds_options, string(!.DumpOptions), !OptionTable)
    ),

    raw_lookup_string_option(!.OptionTable, c_compiler_type,
        C_CompilerTypeStr),
    ( if convert_c_compiler_type(C_CompilerTypeStr, C_CompilerTypePrime) then
        C_CompilerType = C_CompilerTypePrime
    else
        C_CompilerType = cc_unknown,   % dummy
        CCTpec =
            [words("Invalid argument"), quote(C_CompilerTypeStr),
            words("to the"), quote("--c-compiler-type"), words("option;"),
            words("must be")] ++
            list_to_quoted_pieces_or(["gcc", "clang", "msvc", "unknown"]) ++
            [suffix("."), nl],
        add_error(phase_options, CCTpec, !Specs)
    ),

    raw_lookup_string_option(!.OptionTable, csharp_compiler_type,
        CSharp_CompilerTypeStr),
    ( if
        convert_csharp_compiler_type(CSharp_CompilerTypeStr,
            CSharp_CompilerTypePrime)
    then
        CSharp_CompilerType = CSharp_CompilerTypePrime
    else
        CSharp_CompilerType = csharp_unknown,   % dummy
        CSCSpec =
            [words("Invalid argument"), quote(CSharp_CompilerTypeStr),
            words("to the"), quote("--csharp-compiler-type"), words("option;"),
            words("must be")] ++
            list_to_quoted_pieces_or(["microsoft", "mono", "unknown"]) ++
            [suffix("."), nl],
        add_error(phase_options, CSCSpec, !Specs)
    ),

    raw_lookup_string_option(!.OptionTable, structure_reuse_constraint,
        ReuseConstraintStr),
    raw_lookup_int_option(!.OptionTable, structure_reuse_constraint_arg,
        ReuseConstraintArgNum),
    ( if
        convert_reuse_strategy(ReuseConstraintStr, ReuseConstraintArgNum,
            ReuseStrategyPrime)
    then
        ReuseStrategy = ReuseStrategyPrime
    else
        ReuseStrategy = same_cons_id,   % dummy
        ReuseConstrSpec =
            [words("Invalid argument"), quote(ReuseConstraintStr),
            words("to the"), quote("--structure-reuse-constraint"),
            words("option; must be")] ++
            list_to_quoted_pieces_or(["same_cons_id",
                "within_n_cells_difference"]) ++
            [suffix("."), nl],
        add_error(phase_options, ReuseConstrSpec, !Specs)
    ),

    raw_lookup_string_option(!.OptionTable, feedback_file, FeedbackFile),
    ( if FeedbackFile = "" then
        % No feedback info.
        MaybeFeedbackInfo = no
    else
        % When we are compiling a single module, we generally don't know
        % the name of the executable that the compiled module will end up in,
        % and in fact the compiled module may end up in more than one
        % executable. We therefore cannot require that we use feedback files
        % derived from any one profiled program.
        MaybeExpectedProfiledProgramName = no,
        read_feedback_file(FeedbackFile, MaybeExpectedProfiledProgramName,
            FeedbackReadResult, !IO),
        (
            FeedbackReadResult = ok(FeedbackInfo),
            MaybeFeedbackInfo = yes(FeedbackInfo)
        ;
            FeedbackReadResult = error(Error),
            feedback_read_error_message_string(FeedbackFile, Error,
                ErrorMessage),
            add_error(phase_options, [words(ErrorMessage)], !Specs),
            MaybeFeedbackInfo = no
        )
    ),

    raw_lookup_string_option(!.OptionTable, host_env_type, HostEnvTypeStr),
    ( if convert_env_type(HostEnvTypeStr, HostEnvTypePrime) then
        HostEnvType = HostEnvTypePrime
    else
        HostEnvType = env_type_posix,   % dummy
        HostEnvSpec =
            [words("Invalid argument"), quote(HostEnvTypeStr), words("to the"),
            quote("--host-env-type"), words("option; must be")] ++
            list_to_quoted_pieces_or(["posix", "cygwin", "msys", "windows"]) ++
            [suffix("."), nl],
        add_error(phase_options, HostEnvSpec, !Specs)
    ),
    raw_lookup_string_option(!.OptionTable, system_env_type, SystemEnvTypeStr),
    ( if
        ( if SystemEnvTypeStr = "" then
            SystemEnvTypePrime = HostEnvType
        else
            convert_env_type(SystemEnvTypeStr, SystemEnvTypePrime)
        )
    then
        SystemEnvType = SystemEnvTypePrime
    else
        SystemEnvType = env_type_posix,    % dummy
        SystemEnvSpec =
            [words("Invalid argument"), quote(SystemEnvTypeStr),
            words("to the"), quote("--system-env-type"), words("option;"),
            words("must be")] ++
            list_to_quoted_pieces_or(["posix", "cygwin", "msys", "windows"]) ++
            [suffix("."), nl],
        add_error(phase_options, SystemEnvSpec, !Specs)
    ),
    raw_lookup_string_option(!.OptionTable, target_env_type, TargetEnvTypeStr),
    ( if convert_env_type(TargetEnvTypeStr, TargetEnvTypePrime) then
        TargetEnvType = TargetEnvTypePrime
    else
        TargetEnvType = env_type_posix,   % dummy
        TargetEnvTypeSpec =
            [words("Invalid argument"), quote(TargetEnvTypeStr),
            words("to the"), quote("--target-env-type"), words("option;"),
            words("must be")] ++
            list_to_quoted_pieces_or(["posix", "cygwin", "msys", "windows"]) ++
            [suffix("."), nl],
        add_error(phase_options, TargetEnvTypeSpec, !Specs)
    ),

    ( if
        HostEnvType = env_type_posix,
        CSharp_CompilerType = csharp_microsoft
    then
        PosixCSMSpec =
            [words_quote("--host-env-type posix"),
            words("is incompatible with"),
            words_quote("--csharp-compiler-type microsoft"), suffix("."), nl],
        add_error(phase_options, PosixCSMSpec, !Specs)
    else
        true
    ),

    raw_lookup_accumulating_option(!.OptionTable, limit_error_contexts,
        LimitErrorContextsOptionStrs),
    convert_limit_error_contexts(LimitErrorContextsOptionStrs,
        BadLimitErrorContextsOptions, LimitErrorContextsMap),
    (
        BadLimitErrorContextsOptions = []
    ;
        BadLimitErrorContextsOptions = [BadLimitErrorContextsOption],
        LECSpec =
            [words("Invalid argument"), quote(BadLimitErrorContextsOption),
            words("to the"), quote("--limit-error-contexts"),
            words("option."), nl],
        add_error(phase_options, LECSpec, !Specs)
    ;
        BadLimitErrorContextsOptions = [_, _ | _],
        BadPieces = list_to_quoted_pieces(BadLimitErrorContextsOptions),
        LECSpec =
            [words("Invalid arguments")] ++ BadPieces ++
            [words("to the"), quote("--limit-error-contexts"),
            words("option."), nl],
        add_error(phase_options, LECSpec, !Specs)
    ).

    % NOTE: each termination analyser has its own norm setting.
    %
:- pred convert_options_to_globals(option_table::in, op_mode::in,
    compilation_target::in, gc_method::in, tags_method::in,
    termination_norm::in, termination_norm::in, trace_level::in,
    trace_suppress_items::in, ssdb_trace_level::in, may_be_thread_safe::in,
    c_compiler_type::in, csharp_compiler_type::in,
    reuse_strategy::in, maybe(feedback_info)::in,
    env_type::in, env_type::in, env_type::in, limit_error_contexts_map::in,
    list(error_spec)::in, list(error_spec)::out,
    globals::out, io::di, io::uo) is det.

convert_options_to_globals(OptionTable0, OpMode, Target,
        GC_Method, TagsMethod0, TermNorm, Term2Norm,
        TraceLevel, TraceSuppress, SSTraceLevel,
        MaybeThreadSafe, C_CompilerType, CSharp_CompilerType,
        ReuseStrategy, MaybeFeedbackInfo,
        HostEnvType, SystemEnvType, TargetEnvType, LimitErrorContextsMap,
        !Specs, !:Globals, !IO) :-

    lookup_string_option(OptionTable0, install_command, InstallCmd),
    ( if InstallCmd = "" then
        FileInstallCmd = install_cmd_cp
    else
        lookup_string_option(OptionTable0, install_command_dir_option,
            InstallCmdDirOption),
        FileInstallCmd = install_cmd_user(InstallCmd, InstallCmdDirOption)
    ),

    globals_init(OptionTable0, OpMode, Target, GC_Method, TagsMethod0,
        TermNorm, Term2Norm, TraceLevel, TraceSuppress, SSTraceLevel,
        MaybeThreadSafe, C_CompilerType, CSharp_CompilerType,
        ReuseStrategy, MaybeFeedbackInfo,
        HostEnvType, SystemEnvType, TargetEnvType, FileInstallCmd,
        LimitErrorContextsMap, !:Globals),

    % NOTE: this call must occur *before* any code below that implicitly
    % sets options based on the target language or GC method.
    check_grade_component_compatibility(!.Globals, Target, GC_Method, !Specs),

    globals.lookup_string_option(!.Globals, event_set_file_name,
        EventSetFileName0),
    ( if EventSetFileName0 = "" then
        io.get_environment_var("MERCURY_EVENT_SET_FILE_NAME",
            MaybeEventSetFileName, !IO),
        (
            MaybeEventSetFileName = yes(EventSetFileName),
            globals.set_option(event_set_file_name, string(EventSetFileName),
                !Globals)
        ;
            MaybeEventSetFileName = no
        )
    else
        true
    ),

    % Conservative GC implies --no-reclaim-heap-*
    GCIsConservative = gc_is_conservative(GC_Method),
    (
        GCIsConservative = yes,
        globals.set_option(reclaim_heap_on_semidet_failure, bool(no),
            !Globals),
        globals.set_option(reclaim_heap_on_nondet_failure, bool(no),
            !Globals)
    ;
        GCIsConservative = no
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

    % If --tags low but --num-tag-bits is not specified,
    % use the autoconf-determined value for --num-tag-bits.
    % (The autoconf-determined value is passed from the `mmc' script
    % using the deliberately undocumented --conf-low-tag-bits option.)
    ( if
        TagsMethod0 = tags_low,
        NumTagBits0 = -1
    then
        globals.lookup_int_option(!.Globals, conf_low_tag_bits, NumTagBits1)
    else
        NumTagBits1 = NumTagBits0
    ),

    % If --num-tag-bits is negative (which may or may not be its default
    % value of -1), issue a warning and assume --num-tag-bits 0.
    ( if NumTagBits1 < 0 then
        NumTagBits = 0,
        NumTagBitsSpec =
            [words("Warning: the value of the"), quote("--num-tag-bits"),
            words("option is either unspecified or invalid."), nl,
            words("Using"), quote("--num-tag-bits 0"), suffix(","),
            words("which disables tags."), nl],
        add_warning(phase_options, NumTagBitsSpec, !Specs)
    else
        NumTagBits = NumTagBits1
    ),

    globals.set_option(num_tag_bits, int(NumTagBits), !Globals),
    ( if NumTagBits = 0 then
        TagsMethod = tags_none,
        globals.set_tags_method(TagsMethod, !Globals)
    else
        TagsMethod = TagsMethod0
    ),

    current_grade_supports_par_conj(!.Globals, GradeSupportsParConj),
    globals.lookup_bool_option(!.Globals, parallel, Parallel),
    globals.lookup_bool_option(!.Globals, threadscope, Threadscope),
    ( if
        GradeSupportsParConj = no,
        Threadscope = yes
    then
        ThreadScopeSpec =
            [words("The"), quote("threadscope"), words("grade component"),
            words("requires a parallel grade."), nl],
        add_error(phase_options, ThreadScopeSpec, !Specs)
    else
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
            ( if FeedbackFile = "" then
                NoFeedbackFileSpec =
                    [words("The"), quote("--implicit-parallelism"),
                    words("option requires the use of"),
                    quote("--feedback-file"), suffix("."), nl],
                add_error(phase_options, NoFeedbackFileSpec, !Specs)
            else
                true
            )
        ;
            % Report an error when used in parallel grades without parallel
            % conjunction support. In non-parallel grades simply ignore
            % --implicit-parallelism.
            GradeSupportsParConj = no,
            (
                Parallel = yes,
                NoParConjSupportSpec =
                    [words("The"), quote("--implicit-parallelism"),
                    words("option requires a grade that"),
                    words("supports parallel conjunctions."),
                    words("Use a low-level C grade without trailing."), nl],
                add_error(phase_options, NoParConjSupportSpec, !Specs)
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

    (
        ( Target = target_java
        ; Target = target_csharp
        ),

        % Generating Java implies
        %   - gc_method `automatic' and no heap reclamation on failure
        %     Because GC is handled automatically by the Java implementation.
        %   - high-level code
        %     Because only the MLDS back-end supports compiling to Java.
        %   - high-level data
        %     Because it is more efficient, and better for interoperability.
        %     (In theory --low-level-data should work too, but there is
        %     no reason to bother supporting it.)
        %   - unboxed floats
        %   - turning off nested functions
        %     Because Java doesn't support nested functions.
        %   - using copy-out for both det and nondet output arguments
        %     Because Java doesn't support pass-by-reference.
        %   - using no tags
        %     Because Java doesn't provide any mechanism for tagging pointers.
        %   - box no-tag types
        %     We require no-tag types to be boxed since in Java,
        %     java.lang.Object is the only type that all other types
        %     can be successfully cast to and then cast back from.
        %   - store nondet environments on the heap
        %     Because Java has no way of allocating structs on the stack.
        %   - pretest-equality-cast-pointers
        %   - no library grade installation check with `mmc --make'.
        %
        % C# should be the same as Java, except that:
        %   - C# supports pass-by-reference, but for reasons explained in
        %     mlds_to_cs.m, we pretend it doesn't at the MLDS level

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
        (
            Target = target_csharp,
            globals.set_option(executable_file_extension, string(".exe"),
                !Globals)
        ;
            Target = target_java
        )
    ;
        Target = target_erlang,

        % Generating Erlang implies
        %   - gc_method `automatic' and no heap reclamation on failure
        %     Because GC is handled automatically by the Erlang implementation.
        %   - unboxed floats
        %   - delay-partial-instantiations
        %   - no-can-compare-constants-as-ints
        %   - can-compare-compound-values
        %   - lexically-compare-constructors
        %   - no library grade installation check with `mmc --make'
        %   - --no-optimize-tailcalls because Erlang implementations perform
        %     LCO.

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
        globals.set_option(optimize_tailcalls, bool(no), !Globals),

        % The following options do not directly affect the Erlang backend,
        % however we need to ensure they are set to values that are consistent
        % with what the predicate grade_component_table/2 (below) expects.
        globals.set_option(gcc_non_local_gotos, bool(no), !Globals),
        globals.set_option(gcc_global_registers, bool(no), !Globals),
        globals.set_option(asm_labels, bool(no), !Globals),
        globals.set_option(highlevel_code, bool(no), !Globals),
        globals.set_option(highlevel_data, bool(no), !Globals)
    ;
        Target = target_c,

        % Generating high-level C code requires putting each commit
        % in its own function, to avoid problems with setjmp() and
        % non-volatile local variables.
        option_implies(highlevel_code, put_commit_in_own_func, bool(yes),
            !Globals)
    ),

    % Using trail segments implies the use of the trail.
    option_implies(trail_segments, use_trail, bool(yes), !Globals),

    %
    % Set up options for position independent code.
    %

    % Shared libraries always use `--linkage shared'.
    option_implies(compile_to_shared_lib, linkage, string("shared"), !Globals),
    option_implies(compile_to_shared_lib, mercury_linkage,
        string("shared"), !Globals),

    % --high-level-code disables the use of low-level gcc extensions.
    option_implies(highlevel_code, gcc_non_local_gotos, bool(no), !Globals),
    option_implies(highlevel_code, gcc_global_registers, bool(no), !Globals),
    option_implies(highlevel_code, asm_labels, bool(no), !Globals),

    % --no-gcc-nested-functions implies --no-gcc-local-labels.
    option_neg_implies(gcc_nested_functions, gcc_local_labels, bool(no),
        !Globals),

    % --no-mlds-optimize implies --no-optimize-tailcalls.
    option_neg_implies(optimize, optimize_tailcalls, bool(no), !Globals),

    % If no --lib-linkage option has been specified, default to the
    % set of all possible linkages.
    globals.lookup_accumulating_option(!.Globals, lib_linkages, LibLinkages0),
    (
        LibLinkages0 = [],
        globals.set_option(lib_linkages,
            accumulating(["static", "shared"]), !Globals)
    ;
        LibLinkages0 = [_ | _]
    ),

    % This is needed for library installation (the library grades
    % are built using `--use-grade-subdirs', and assume that
    % the interface files were built using `--use-subdirs').
    globals.lookup_bool_option(!.Globals, invoked_by_mmc_make,
        InvokedByMMCMake),
    ( if
        ( OpMode = opm_top_make
        ; InvokedByMMCMake = yes
        )
    then
        globals.set_option(use_subdirs, bool(yes), !Globals)
    else
        true
    ),

    % --make handles creation of the module dependencies itself,
    % and they don't need to be recreated when compiling to C.
    option_implies(invoked_by_mmc_make,
        generate_mmc_make_module_dependencies, bool(no), !Globals),

    % --libgrade-install-check only works with --make.
    ( if OpMode = opm_top_make then
        true
    else
        globals.set_option(libgrade_install_check, bool(no), !Globals)
    ),

    % `--transitive-intermodule-optimization' and `--make' are
    % not compatible with each other.
    globals.lookup_bool_option(!.Globals, transitive_optimization, TransOpt),
    (
        TransOpt = yes,
        ( if
            ( InvokedByMMCMake = yes
            ; OpMode = opm_top_make
            )
        then
            TransOptMakeSpec =
                [words("The"), quote("--transitive-intermodule-optimization"),
                words("option is incompatible with the"), quote("--make"),
                words("option."), nl],
            add_error(phase_options, TransOptMakeSpec, !Specs)
        else
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
    ( if
        InterModOpt = yes,
        InterModAnalysis = yes
    then
        OptAnalysisSpec =
            [words("The"), quote("--intermodule-optimization"),
            words("option is incompatible with the"),
            quote("--intermodule-analysis"), words("option."), nl],
        add_error(phase_options, OptAnalysisSpec, !Specs)
    else
        true
    ),

    ( if io.have_symlinks then
        true
    else
        globals.set_option(use_symlinks, bool(no), !Globals)
    ),

    globals.lookup_maybe_string_option(!.Globals,
        generate_standalone_interface, MaybeStandaloneInt),
    globals.lookup_bool_option(!.Globals,
        extra_initialization_functions, ExtraInitFunctions),
    ( if
        MaybeStandaloneInt = yes(_),
        ExtraInitFunctions = yes
    then
        ExtraInitsSpec =
            [words("The"), quote("--generate-standalone-interface"),
            words("option is incompatible with the"),
            quote("--extra-initialization-functions"), words("option."), nl],
        add_error(phase_options, ExtraInitsSpec, !Specs)
    else
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
    ( if OpMode = opm_top_args(opma_augment(opmau_make_trans_opt_int)) then
        globals.set_option(transitive_optimization, bool(yes), !Globals)
    else
        true
    ),
    option_implies(transitive_optimization, intermodule_optimization,
        bool(yes), !Globals),
    option_implies(use_trans_opt_files, use_opt_files, bool(yes), !Globals),

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
    % XXX Should that be "with `--no-use-opt-files'"?
    globals.set_option(use_opt_files, bool(no), !Globals),

    option_implies(smart_recompilation, generate_item_version_numbers,
        bool(yes), !Globals),
    option_implies(find_all_recompilation_reasons, verbose_recompilation,
        bool(yes), !Globals),

    % Disable `--smart-recompilation' unless we are generating target code.
    ( if OpMode = opm_top_args(opma_augment(opmau_generate_code(_))) then
        true
    else
        globals.set_option(smart_recompilation, bool(no), !Globals)
    ),

    % Disable --line-numbers when building the `.int', `.opt', etc. files,
    % since including line numbers in those would cause unnecessary
    % recompilation.
    ( if
        ( OpMode = opm_top_args(opma_make_private_interface)
        ; OpMode = opm_top_args(opma_make_short_interface)
        ; OpMode = opm_top_args(opma_make_interface)
        ; OpMode = opm_top_args(opma_augment(opmau_make_opt_int))
        ; OpMode = opm_top_args(opma_augment(opmau_make_trans_opt_int))
        )
    then
        globals.set_option(line_numbers, bool(no), !Globals)
    else
        true
    ),

    % We never use version number information in `.int3',
    % `.opt' or `.trans_opt' files.
    ( if OpMode = opm_top_args(opma_make_short_interface) then
        globals.set_option(generate_item_version_numbers, bool(no), !Globals)
    else
        true
    ),

    % The combination of --make-xml-documentation and
    % --intermodule-optimization can causes spurious warnings about
    % missing .opt files if they haven't been built yet.
    ( if OpMode = opm_top_args(opma_augment(opmau_make_xml_documentation)) then
        globals.set_option(intermodule_optimization, bool(no), !Globals)
    else
        true
    ),

    % XXX Smart recompilation does not yet work with inter-module
    % optimization, but we still want to generate version numbers
    % in interface files for users of a library compiled with
    % inter-module optimization but not using inter-module
    % optimization themselves.
    globals.lookup_bool_option(!.Globals, smart_recompilation, Smart),
    (
        Smart = no
    ;
        Smart = yes,
        ( if
            globals.lookup_bool_option(!.Globals, intermodule_optimization,
                yes)
        then
            disable_smart_recompilation("`--intermodule-optimization'",
                !Globals, !IO)
        else
            true
        ),
        ( if globals.lookup_bool_option(!.Globals, use_opt_files, yes) then
            disable_smart_recompilation("`--use-opt-files'", !Globals, !IO)
        else
            true
        ),
        % XXX Smart recompilation does not yet work with
        % `--no-target-code-only'. With `--no-target-code-only'
        % it becomes difficult to work out what all the target files
        % are and check whether they are up-to-date. By default, mmake always
        % enables `--target-code-only' and processes the target code file
        % itself, so this isn't a problem.
        % XXX I (zs) don't believe that mmake sets --target-code-only anymore.
        ( if
            OpMode = opm_top_args(opma_augment(
                opmau_generate_code(opmcg_target_code_only)))
        then
            true
        else
            disable_smart_recompilation("`--no-target-code-only'",
                !Globals, !IO)
        )
    ),

    option_implies(use_grade_subdirs, use_subdirs, bool(yes), !Globals),

    option_implies(very_verbose, verbose, bool(yes), !Globals),
    option_implies(verbose, verbose_commands, bool(yes), !Globals),
    globals.lookup_bool_option(!.Globals, very_verbose, VeryVerbose),
    globals.lookup_bool_option(!.Globals, statistics, Statistics),
    ( if
        VeryVerbose = yes,
        Statistics = yes
    then
        globals.set_option(detailed_statistics, bool(yes), !Globals)
    else
        true
    ),

    option_implies(debug_modes_minimal, debug_modes, bool(yes), !Globals),
    option_implies(debug_modes_verbose, debug_modes, bool(yes), !Globals),
    option_implies(debug_modes_statistics, debug_modes, bool(yes), !Globals),

    globals.lookup_int_option(!.Globals, debug_liveness, DebugLiveness),
    ( if
        DebugLiveness >= 0,
        convert_dump_alias("all", AllDumpOptions)
    then
        % Programmers only enable --debug-liveness if they are interested
        % in the goal annotations put on goals by the various phases
        % of the liveness pass. The default dump options do not print
        % these annotations.
        globals.lookup_string_option(!.Globals, dump_hlds_options,
            DumpOptions0),
        DumpOptions1 = DumpOptions0 ++ AllDumpOptions,
        globals.set_option(dump_hlds_options, string(DumpOptions1), !Globals)
    else
        true
    ),

    option_implies(debug_modes_verbose, debug_modes, bool(yes), !Globals),
    globals.lookup_int_option(!.Globals, debug_modes_pred_id,
        DebugModesPredId),
    ( if DebugModesPredId > 0 then
        globals.set_option(debug_modes, bool(yes), !Globals)
    else
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
    ( if
        ( DebugOptPredIdStrs = [_ | _]
        ; DebugOptPredNames = [_ | _]
        )
    then
        globals.set_option(debug_opt, bool(yes), !Globals)
    else
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
        globals.set_option(dump_hlds_options, string(DumpOptions), !Globals)
    ;
        DumpHLDSPredIds = []
    ),

    option_implies(debug_mode_constraints, prop_mode_constraints, bool(yes),
        !Globals),
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
    ( if
        UseMinimalModelStackCopy = yes,
        UseMinimalModelOwnStacks = yes
    then
        DualMMSpec =
            [words("You cannot use both forms of minimal model tabling"),
            words("at once."), nl],
        add_error(phase_options, DualMMSpec, !Specs)
    else if
        UseMinimalModel = yes,
        HighLevelCode = yes
    then
        MMHLSpec =
            [words("Minimal model tabling is incompatible with"),
            words("high level code."), nl],
        add_error(phase_options, MMHLSpec, !Specs)
    else if
        UseMinimalModel = yes,
        UseTrail = yes
    then
        MMTrailSpec =
            [words("Minimal model tabling is incompatible with"),
            words("trailing."), nl],
        add_error(phase_options, MMTrailSpec, !Specs)
    else
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
        ( if ArgPackBits0 < 0 then
            ArgPackBits = BitsPerWord
        else if ArgPackBits0 > BitsPerWord then
            ArgPackBits = BitsPerWord,
            ArgPackBitsSpec =
                [words("Warning: cannot set the value of"),
                quote("--arg-pack-bits"),
                words("to value higher than the value of"),
                quote("--bits-per-word"), suffix("."),
                words("Reducing the effective value of"),
                quote("--arg-pack-bits"),
                words("to the maximum allowable value, which is"),
                int_fixed(BitsPerWord), suffix("."), nl],
            add_error(phase_options, ArgPackBitsSpec, !Specs)
        else
            ArgPackBits = ArgPackBits0
        ),
        globals.set_option(arg_pack_bits, int(ArgPackBits), !Globals)
    ;
        ( Target = target_csharp
        ; Target = target_java
        ; Target = target_erlang
        ),
        globals.set_option(arg_pack_bits, int(0), !Globals),
        globals.set_option(allow_double_word_fields, bool(no), !Globals)
    ),

    % We assume that single-precision floats do not need to be boxed.
    option_implies(single_prec_float, unboxed_float, bool(yes), !Globals),

    % We only use the float registers if floats would not fit into the
    % regular registers.
    option_implies(highlevel_code, use_float_registers, bool(no), !Globals),
    option_implies(unboxed_float, use_float_registers, bool(no), !Globals),

    % Currently, multi-arm switches have been tested only for the LLDS
    % backend (which always generates C) and for the MLDS backend when
    % it is generating C, C# or Java code.
    (
        ( Target = target_c
        ; Target = target_csharp
        ; Target = target_java
        )
    ;
        Target = target_erlang,
        globals.set_option(allow_multi_arm_switches, bool(no), !Globals)
    ),

    ( if
        ( Target = target_csharp
        ; Target = target_java
        )
    then
        % Switch off string hash switches until these backends implement
        % the hash operations.
        globals.set_option(string_hash_switch_size, int(999999), !Globals)
    else
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
    % want so we disable inlining with deep profiling by default.
    % The user can re-enable it with the `--profile-optimized' option.
    % Leave inlining enabled when profiling for implicit parallelism.
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

    globals.lookup_string_option(!.Globals, experimental_complexity, ExpComp),
    ( if ExpComp = "" then
        true
    else
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
    TraceLevelIsNone = given_trace_level_is_none(TraceLevel),
    (
        TraceLevelIsNone = no,
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
            globals.set_option(test_after_switch, bool(no), !Globals)
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
        TraceTailRec = trace_level_allows_tail_rec(TraceLevel),
        (
            TraceTailRec = no,
            globals.set_option(exec_trace_tail_rec, bool(no), !Globals)
        ;
            TraceTailRec = yes
        )
    ;
        TraceLevelIsNone = yes,
        % Since there will be no call and exit events, there is no point
        % in trying to turn them into tailcall events.
        globals.set_option(exec_trace_tail_rec, bool(no), !Globals)
    ),

    option_implies(profile_deep, procid_stack_layout, bool(yes), !Globals),
    globals.lookup_bool_option(!.Globals, profile_deep, ProfileDeep),
    (
        ProfileDeep = yes,
        ( if
            HighLevelCode = no,
            Target = target_c
        then
            true
        else
            DeepHLSpec =
                [words("Deep profiling is incompatible with"),
                words("high level code."), nl],
            add_error(phase_options, DeepHLSpec, !Specs)
        ),
        globals.set_option(optimize_constructor_last_call, bool(no), !Globals),
        globals.lookup_bool_option(!.Globals,
            use_lots_of_ho_specialization, LotsOfHOSpec),
        (
            LotsOfHOSpec = yes,
            globals.set_option(optimize_higher_order, bool(yes), !Globals),
            globals.set_option(higher_order_size_limit, int(999999), !Globals)
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
    ( if
        RecordTermSizesAsWords = yes,
        RecordTermSizesAsCells = yes
    then
        DualTermSizeSpec =
            [words("Cannot record term size as both words and cells."), nl],
        add_error(phase_options, DualTermSizeSpec, !Specs)
    else if
        ( RecordTermSizesAsWords = yes
        ; RecordTermSizesAsCells = yes
        )
    then
        globals.set_option(optimize_constructor_last_call, bool(no), !Globals),
        % Term size profiling breaks the assumption that even word offsets from
        % the start of the cell are double-word aligned memory addresses.
        globals.set_option(allow_double_word_fields, bool(no), !Globals),
        (
            HighLevelCode = yes,
            TermSizeHLSpec =
                [words("Term size profiling is incompatible with"),
                words("high level code."), nl],
            add_error(phase_options, TermSizeHLSpec, !Specs)
        ;
            HighLevelCode = no
        )
    else
        true
    ),

    ( if
        ( given_trace_level_is_none(TraceLevel) = yes
        ; HighLevelCode = no, Target = target_c
        )
    then
        true
    else
        TraceHLSpec =
            [words("Debugging is available only in low level C grades."), nl],
        add_error(phase_options, TraceHLSpec, !Specs)
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
    option_neg_implies(reorder_conj, local_constraint_propagation, bool(no),
        !Globals),

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
    % For the MLDS back-end, `--gc accurate' requires just typeinfo liveness.
    %
    % XXX Currently we also need to disable heap reclamation on failure
    % if accurate GC is enabled.
    % There are two issues with heap reclamation on failure:
    %
    % 1 For heap reclamation on failure to work at all, we also need
    %   at least some degree of liveness-accuracy. Otherwise, a local variable
    %   may get initialized to point to the heap, then the heap is reset,
    %   then the memory is overwritten with new allocations, and then
    %   a collection occurs, at which point the local variable now points to
    %   a value of the wrong type.
    % 2 The current method of handling saved heap pointers during GC means that
    %   we lose heap reclamation on failure after a GC occurs. A better method
    %   would be to just allocate a word of heap space at each choice point.
    %
    % XXX We also need to disable optimize-constructor-last-call as currently
    % the collector (and tracing code generator) knows neither about the
    % pre-constructed data structures nor the references into them
    % that this optimisation uses.
    %
    % XXX We also disable type specialization. This is needed because
    % type specialization may create type class constraints of the form
    % `c(t(T))' (e.g. `enum(var(T))'' in library/sparse_bitset.m),
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
        globals.set_option(optimize_constructor_last_call, bool(no), !Globals),
        globals.set_option(type_specialization, bool(no), !Globals),
        globals.set_option(user_guided_type_specialization, bool(no), !Globals)
    ;
        ( GC_Method = gc_automatic
        ; GC_Method = gc_none
        ; GC_Method = gc_boehm
        ; GC_Method = gc_boehm_debug
        ; GC_Method = gc_hgc
        )
    ),

    % ml_gen_params_base and ml_declare_env_ptr_arg, in ml_code_util.m,
    % both assume (for accurate GC) that continuation environments
    % are always allocated on the stack, which means that things won't work
    % if --gc accurate and --put-nondet-env-on-heap are both enabled.
    globals.lookup_bool_option(!.Globals, put_nondet_env_on_heap,
        PutNondetEnvOnHeap),
    ( if
        HighLevelCode = yes,
        GC_Method = gc_accurate,
        PutNondetEnvOnHeap = yes
    then
        AGCEnvSpec =
            [words_quote("--gc accurate"), words("is incompatible with"),
            words_quote("--put-nondet-env-on-heap"), suffix("."), nl],
        add_error(phase_options, AGCEnvSpec, !Specs)
    else
        true
    ),

    % `procid' and `agc' stack layouts need `basic' stack layouts
    option_implies(procid_stack_layout, basic_stack_layout, bool(yes),
        !Globals),
    option_implies(agc_stack_layout, basic_stack_layout, bool(yes), !Globals),

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
    option_implies(body_typeinfo_liveness, deforestation, bool(no), !Globals),
    option_implies(body_typeinfo_liveness, constraint_propagation, bool(no),
        !Globals),

    % XXX If trailing is enabled, middle recursion optimization
    % can generate code which does not allocate a stack frame
    % even though stack slots are used to save and restore the
    % trail, if the code being optimized contains a construct which
    % might save/restore the trail state, i.e. an if-then-else,
    % negation, disjunction, or commit.
    option_implies(use_trail, middle_rec, bool(no), !Globals),

    % The cut-down stack frames used by middle recursion optimization
    % don't include return addresses. Since stack extension arranges for
    % the return to the old stack segments by overriding the return address,
    % stack extension via stack segments and middle recursion optimization
    % are incompatible.
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
    ( if
        UseMinimalModelStackCopy = yes,
        DisablePneg = no
    then
        globals.set_option(use_minimal_model_stack_copy_pneg, bool(yes),
            !Globals)
    else
        true
    ),
    ( if
        UseMinimalModelStackCopy = yes,
        DisableCut = no
    then
        globals.set_option(use_minimal_model_stack_copy_cut, bool(yes),
            !Globals)
    else
        true
    ),

    % --dump-hlds, --statistics, --parallel-liveness and
    % --parallel-code-gen require compilation by phases.
    globals.lookup_accumulating_option(!.Globals, dump_hlds, DumpHLDSStages),
    globals.lookup_accumulating_option(!.Globals, dump_trace_counts,
        DumpTraceStages),
    globals.lookup_bool_option(!.Globals, parallel_liveness, ParallelLiveness),
    globals.lookup_bool_option(!.Globals, parallel_code_gen, ParallelCodeGen),
    ( if
        ( DumpHLDSStages = [_ | _]
        ; DumpTraceStages = [_ | _]
        ; Statistics = yes
        ; ParallelLiveness = yes
        ; ParallelCodeGen = yes
        )
    then
        globals.set_option(trad_passes, bool(no), !Globals)
    else
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
    option_implies(intermod_unused_args, intermodule_optimization, bool(yes),
        !Globals),
    option_implies(intermod_unused_args, optimize_unused_args, bool(yes),
        !Globals),

    % --introduce-accumulators implies --excess-assign and
    % --common-struct.
    option_implies(introduce_accumulators, excess_assign, bool(yes), !Globals),
    option_implies(introduce_accumulators, common_struct, bool(yes), !Globals),

    % Don't do the unused_args optimization when making the
    % optimization interface.
    ( if OpMode = opm_top_args(opma_augment(opmau_make_opt_int)) then
        globals.set_option(optimize_unused_args, bool(no), !Globals)
    else
        true
    ),

    % The results of trail usage analysis assume that trail usage
    % optimization is being done, i.e. that redundant trailing
    % operations are really being eliminated.
    option_implies(analyse_trail_usage, optimize_trail_usage, bool(yes),
        !Globals),

    % Without an existing source file mapping, there is no "right" module name.
    ( if OpMode = opm_top_generate_source_file_mapping then
        globals.set_option(warn_wrong_module_name, bool(no), !Globals)
    else
        true
    ),

    globals.lookup_string_option(!.Globals, target_arch, TargetArch),

    globals.lookup_string_option(!.Globals, mercury_linkage, MercuryLinkage),
    ( if MercuryLinkage = "static" then
        DefaultRuntimeLibraryDirs = no,
        globals.set_option(default_runtime_library_directory, bool(no),
            !Globals)
    else
        globals.lookup_bool_option(!.Globals,
            default_runtime_library_directory, DefaultRuntimeLibraryDirs)
    ),

    % Add the standard library directory.
    globals.lookup_maybe_string_option(!.Globals,
        mercury_standard_library_directory, MaybeStdLibDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        globals.get_options(!.Globals, OptionTable2),
        option_table_add_mercury_library_directory(StdLibDir,
            OptionTable2, OptionTable),
        globals.set_options(OptionTable, !Globals),

        % Add `-L' and `-R' options for the location of the GC libraries.
        globals.lookup_accumulating_option(!.Globals,
            link_library_directories, LinkLibDirs0),
        globals.set_option(link_library_directories,
            accumulating([StdLibDir/"lib" | LinkLibDirs0]), !Globals),

        (
            DefaultRuntimeLibraryDirs = yes,
            globals.lookup_accumulating_option(!.Globals,
                runtime_link_library_directories, Rpath0),
            globals.set_option(runtime_link_library_directories,
                accumulating([StdLibDir/"lib" | Rpath0]), !Globals)
        ;
            DefaultRuntimeLibraryDirs = no
        )
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
    ( if ConfigFile = yes("") then
        (
            MaybeConfDir = yes(ConfDir1),
            globals.set_option(config_file, maybe_string(yes(
                ConfDir1/"conf"/"Mercury.config")), !Globals)
        ;
            MaybeConfDir = no,
            globals.set_option(config_file, maybe_string(no), !Globals)
        )
    else
        true
    ),

    % Handle the `.opt', C and Erlang header, init file and library search
    % directories for installed libraries. These couldn't be handled by
    % options.m because they are grade dependent.
    globals.lookup_accumulating_option(!.Globals,
        mercury_library_directories, MercuryLibDirs),
    grade_directory_component(!.Globals, GradeString),
    (
        MercuryLibDirs = [_ | _],
        ExtraLinkLibDirs = list.map(
            ( func(MercuryLibDir) =
                MercuryLibDir/"lib"/GradeString
            ), MercuryLibDirs),

        globals.lookup_accumulating_option(!.Globals,
            link_library_directories, LinkLibDirs1),
        globals.set_option(link_library_directories,
            accumulating(LinkLibDirs1 ++ ExtraLinkLibDirs), !Globals),

        (
            DefaultRuntimeLibraryDirs = yes,
            globals.lookup_accumulating_option(!.Globals,
                runtime_link_library_directories, Rpath),
            globals.set_option(runtime_link_library_directories,
                accumulating(Rpath ++ ExtraLinkLibDirs), !Globals)
        ;
            DefaultRuntimeLibraryDirs = no
        ),

        ExtraIncludeDirs = list.map(
            ( func(MercuryLibDir) =
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
            ( func(MercuryLibDir) =
                dir.make_path_name(MercuryLibDir,
                    dir.make_path_name("ints", GradeString))
            ), MercuryLibDirs),
        globals.lookup_accumulating_option(!.Globals,
            intermod_directories, IntermodDirs0),
        globals.set_option(intermod_directories,
            accumulating(ExtraIntermodDirs ++ IntermodDirs0), !Globals),

        ExtraInitDirs = list.map(
            ( func(MercuryLibDir) =
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
    ToGradeSubdir = (func(Dir) = Dir/"Mercury"/GradeString/TargetArch),
    (
        UseGradeSubdirs = yes,
        % With `--use-grade-subdirs', `.opt', `.trans_opt' and
        % `.mih' files are placed in a directory named
        % `Mercury/<grade>/<target_arch>/Mercury/<ext>s'.
        % When searching for a `.opt' file, module_name_to_file_name
        % produces `Mercury/<ext>/<module>.ext' so that searches
        % for installed files work, so we need to add
        % `--intermod-directory Mercury/<grade>/<target_arch>'
        % to find the `.opt' files in the current directory.
        GradeSubdir = "Mercury"/GradeString/TargetArch,

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
        % `Mercury/<grade>/<target_arch>/Mercury/lib' for libraries and
        % `Mercury/<grade>/<target_arch>/Mercury/inits' for init files,
        % for each directory listed with --search-library-files-directory.
        ToGradeLibDir = (func(Dir) = ToGradeSubdir(Dir)/"Mercury"/"lib"),
        SearchGradeLibDirs = list.map(ToGradeLibDir, SearchLibFilesDirs),
        LinkLibDirs = SearchGradeLibDirs ++ LinkLibDirs2,

        ToGradeInitDir =
            (func(Dir) = ToGradeSubdir(Dir)/"Mercury"/"inits"),
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
    ( if
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
    then
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
    else
        true
    ),

    % --use-opt-files implies --no-warn-missing-opt-files since
    % we are expecting some to be missing.
    option_implies(use_opt_files, warn_missing_opt_files, bool(no),
        !Globals),

    % --warn-non-tail-recursion requires tail call optimization to be enabled.
    % It also doesn't work if you use --errorcheck-only.
    globals.lookup_bool_option(!.Globals, warn_non_tail_recursion_self,
        WarnNonTailRecSelf),
    globals.lookup_bool_option(!.Globals, warn_non_tail_recursion_mutual,
        WarnNonTailRecMutual),
    ( if
        ( WarnNonTailRecSelf = yes
        ; WarnNonTailRecMutual = yes
        )
    then
        globals.lookup_bool_option(!.Globals, pessimize_tailcalls,
            PessimizeTailCalls),
        globals.lookup_bool_option(!.Globals, optimize_tailcalls,
            OptimizeTailCalls),
        (
            PessimizeTailCalls = no
        ;
            PessimizeTailCalls = yes,
            PessimizeWords = "--warn-non-tail-recursion is incompatible" ++
                 " with --pessimize-tailcalls",
            add_error(phase_options, [words(PessimizeWords)], !Specs)
        ),
        (
            OptimizeTailCalls = yes
        ;
            OptimizeTailCalls = no,
            OptimizeWords =
                "--warn-non-tail-recursion requires --optimize-tailcalls",
            add_error(phase_options, [words(OptimizeWords)], !Specs)
        ),
        ( if OpMode = opm_top_args(opma_augment(opmau_errorcheck_only)) then
            ECOWords = "--warn-non-tail-recursion is incompatible"
                ++ " with --errorcheck-only",
            add_error(phase_options, [words(ECOWords)], !Specs)
        else
            true
        )
    else
        true
    ),

    % The backend foreign languages depend on the target.
    (
        Target = target_c,
        BackendForeignLanguages = ["c"]
    ;
        Target = target_csharp,
        BackendForeignLanguages = ["csharp"]
    ;
        Target = target_java,
        BackendForeignLanguages = ["java"]
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

    globals.lookup_int_option(!.Globals, compare_specialization, CompareSpec),
    ( if CompareSpec < 0 then
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
    else
        true
    ),

    ( if
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
    then
        globals.set_option(can_compare_constants_as_ints, bool(yes), !Globals)
    else
        globals.set_option(can_compare_constants_as_ints, bool(no), !Globals)
    ),

    (
        HighLevelCode = no,
        postprocess_options_lowlevel(!Globals)
    ;
        HighLevelCode = yes
    ),
    postprocess_options_libgrades(!Globals, !Specs),
    globals_init_mutables(!.Globals, !IO).

    % These option implications only affect the low-level (LLDS) code
    % generator. They may in fact be harmful if set for the high-level
    % code generator, because sometimes the same option has different
    % meanings and implications in the two backends.
    % XXX For each such dual-use option, we should add two others,
    % one for the LLDS and one for the MLDS backend, and make the original
    % option be a special option that sets the other two. This would leave
    % the user interface unchanged, but would let us handle the implications
    % for the two backends separately.
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
    ( if
        ( OptFrames = yes
        ; OptLocalVars = yes
        ),
        OptRepeat < 1
    then
        % The frame optimization and the local vars optimization depend on
        % the jump and label optimization having been done. They are turned
        % on above, but they still won't be executed unless optimize_repeat
        % is at least one.
        globals.set_option(optimize_repeat, int(1), !Globals)
    else
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
    ( if
        NonLocalGotos = yes,
        AsmLabels = no
    then
        % With non-local gotos but no asm labels, jumps to code addresses
        % in different c_modules must be done via global variables; the value
        % of these global variables is not constant (i.e. not computable at
        % load time), since they can't be initialized until we call
        % init_modules().
        StaticCodeAddrs = no
    else
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

%---------------------------------------------------------------------------%

usage_errors(Globals, Specs, !IO) :-
    io.progname_base("mercury_compile", ProgName, !IO),
    io.write_string(ProgName, !IO),
    io.write_string(":", !IO),
    io.nl(!IO),
    % If _NumErrors > 0, this will set the exit status to 1.
    % It will also set the exit status to 1 if  _NumWarnings > 0
    % and --halt-at-warn was specified.
    write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors, !IO).

display_compiler_version(!IO) :-
    library.version(Version, Fullarch),
    io.write_strings([
        "Mercury Compiler, version ", Version, ", on ", Fullarch, "\n",
        "Copyright (C) 1993-2012 The University of Melbourne\n",
        "Copyright (C) 2013-2017 The Mercury team\n"
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
    io.write_string("Copyright: Copyright (C) 1993-2012 " ++
        "The University of Melbourne\n", !IO),
    io.write_string("           Copyright (C) 2013-2017 " ++
        "The Mercury team\n", !IO),
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

%---------------------------------------------------------------------------%

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
convert_dump_alias("all", "abcdEfgilmnprstuvzBCMPTZ").
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

%---------------------------------------------------------------------------%

:- pred raw_lookup_bool_option(option_table::in, option::in, bool::out) is det.

raw_lookup_bool_option(OptionTable, Option, BoolValue) :-
    map.lookup(OptionTable, Option, OptionValue),
    ( if OptionValue = bool(BoolValuePrime) then
        BoolValue = BoolValuePrime
    else
        OptionStr = string.string(Option),
        unexpected($module, $pred, OptionStr ++ "is not a bool")
    ).

:- pred raw_lookup_int_option(option_table::in, option::in, int::out) is det.

raw_lookup_int_option(OptionTable, Option, IntValue) :-
    map.lookup(OptionTable, Option, OptionValue),
    ( if OptionValue = int(IntValuePrime) then
        IntValue = IntValuePrime
    else
        OptionStr = string.string(Option),
        unexpected($module, $pred, OptionStr ++ "is not an int")
    ).

:- pred raw_lookup_string_option(option_table::in, option::in,
    string::out) is det.

raw_lookup_string_option(OptionTable, Option, StringValue) :-
    map.lookup(OptionTable, Option, OptionValue),
    ( if OptionValue = string(StringValuePrime) then
        StringValue = StringValuePrime
    else
        OptionStr = string.string(Option),
        unexpected($module, $pred, OptionStr ++ "is not a string")
    ).

:- pred raw_lookup_accumulating_option(option_table::in, option::in,
    list(string)::out) is det.

raw_lookup_accumulating_option(OptionTable, Option, AccumulatingValue) :-
    map.lookup(OptionTable, Option, OptionValue),
    ( if OptionValue = accumulating(AccumulatingValuePrime) then
        AccumulatingValue = AccumulatingValuePrime
    else
        OptionStr = string.string(Option),
        unexpected($module, $pred, OptionStr ++ "is not accumulating")
    ).

%---------------------------------------------------------------------------%
:- end_module libs.handle_options.
%---------------------------------------------------------------------------%
