%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: check_options.m.
%
% This module checks the values of the command-line options.
%
%---------------------------------------------------------------------------%

:- module libs.check_options.
:- interface.

:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred check_option_values(option_table::in, option_table::out,
    compilation_target::out, word_size::out, gc_method::out,
    termination_norm::out, termination_norm::out, trace_level::out,
    trace_suppress_items::out, ssdb_trace_level::out, may_be_thread_safe::out,
    c_compiler_type::out, csharp_compiler_type::out,
    reuse_strategy::out,
    maybe(feedback_info)::out, env_type::out, env_type::out, env_type::out,
    limit_error_contexts_map::out, linked_target_ext_info_map::out,
    list(error_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % This predicate converts a symbolic name for a set of verbosity options
    % (a "dump alias") into the string consisting of those options' characters.
    %
    % The meanings of the option characters are documented by
    % doc/user_guide.texi and by compiler/hlds_out.m. The latter
    % is more authoritative :-)
    %
    % You are welcome to add more aliases.
    %
:- pred convert_dump_alias(string::in, string::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.color_schemes.
:- import_module libs.compiler_util.
:- import_module parse_tree.maybe_error.

:- import_module bool.
:- import_module getopt.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

check_option_values(!OptionTable, Target, WordSize, GC_Method,
        TermNorm, Term2Norm, TraceLevel, TraceSuppress, SSTraceLevel,
        MaybeThreadSafe, C_CompilerType, CSharp_CompilerType,
        ReuseStrategy, MaybeFeedbackInfo,
        HostEnvType, SystemEnvType, TargetEnvType,
        LimitErrorContextsMap, LinkExtMap, !:Specs, !IO) :-
    !:Specs = [],
    check_grade_options(!.OptionTable, Target, WordSize, GC_Method, !Specs),
    check_codegen_options(!.OptionTable,
        MaybeThreadSafe, ReuseStrategy, MaybeFeedbackInfo, !Specs, !IO),
    check_termination_options(!.OptionTable, TermNorm, Term2Norm, !Specs),
    check_debug_options(!.OptionTable,
        TraceLevel, TraceSuppress, SSTraceLevel, !Specs),
    check_system_env_options(!.OptionTable,
        C_CompilerType, CSharp_CompilerType,
        HostEnvType, SystemEnvType, TargetEnvType, !Specs),
    check_hlds_dump_options(!OptionTable, !Specs),
    check_diagnostics_options(!.OptionTable, LimitErrorContextsMap, !Specs),
    check_linked_target_extensions(!.OptionTable, LinkExtMap, !Specs),
    check_color_options(!OptionTable, !Specs, !IO).

:- pred check_grade_options(option_table::in,
    compilation_target::out, word_size::out, gc_method::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_grade_options(OptionTable, Target, WordSize, GC_Method, !Specs) :-
    lookup_string_option(OptionTable, target, TargetStr),
    ( if convert_target(TargetStr, TargetPrime) then
        Target = TargetPrime
    else
        Target = target_c,     % dummy
        TargetSpec =
            [words("Invalid argument"), quote(TargetStr), words("to the"),
            quote("--target"), words("option; must be")] ++
            quote_list_to_pieces("or", ["c", "java", "csharp"]) ++
            [suffix("."), nl],
        add_error(phase_options, TargetSpec, !Specs)
    ),

    lookup_int_option(OptionTable, bits_per_word, BitsPerWord),
    ( if BitsPerWord = 32 then
        WordSize = word_size_32
    else if BitsPerWord = 64 then
        WordSize = word_size_64
    else
        WordSize = word_size_64,    % dummy
        BitsPerWordStr = string.int_to_string(BitsPerWord),
        WordSizeSpec =
            [words("Invalid argument"), quote(BitsPerWordStr),
            words("to the"), quote("--bits-per-word"), words("option;"),
            words("must be either"), quote("32"), words("or"), quote("64"),
            suffix("."), nl],
        add_error(phase_options, WordSizeSpec, !Specs)
    ),

    lookup_string_option(OptionTable, gc, GC_MethodStr),
    ( if convert_gc_method(GC_MethodStr, GC_MethodPrime) then
        GC_Method = GC_MethodPrime
    else
        GC_Method = gc_none,   % dummy
        GCMethodSpec =
            [words("Invalid argument"), quote(GC_MethodStr),
            words("to the"), quote("--gc"), words("option; must be")] ++
            quote_list_to_pieces("or", ["none", "conservative", "boehm", "hgc",
                "accurate", "automatic"]) ++ [suffix("."), nl],
        add_error(phase_options, GCMethodSpec, !Specs)
    ).

:- pred check_codegen_options(option_table::in,
    may_be_thread_safe::out, reuse_strategy::out, maybe(feedback_info)::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

check_codegen_options(OptionTable, MaybeThreadSafe, ReuseStrategy,
        MaybeFeedbackInfo, !Specs, !IO) :-
    lookup_int_option(OptionTable, fact_table_hash_percent_full,
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

    lookup_string_option(OptionTable, maybe_thread_safe_opt,
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
            quote_list_to_pieces("or", ["no", "yes"]) ++ [suffix("."), nl],
        add_error(phase_options, MTSSpec, !Specs)
    ),

    lookup_string_option(OptionTable, structure_reuse_constraint,
        ReuseConstraintStr),
    lookup_int_option(OptionTable, structure_reuse_constraint_arg,
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
            quote_list_to_pieces("or", ["same_cons_id",
                "within_n_cells_difference"]) ++
            [suffix("."), nl],
        add_error(phase_options, ReuseConstrSpec, !Specs)
    ),

    lookup_string_option(OptionTable, feedback_file, FeedbackFile),
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
    ).

:- pred check_termination_options(option_table::in,
    termination_norm::out, termination_norm::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_termination_options(OptionTable, TermNorm, Term2Norm, !Specs) :-
    lookup_string_option(OptionTable, termination_norm, TermNormStr),
    ( if convert_termination_norm(TermNormStr, TermNormPrime) then
        TermNorm = TermNormPrime
    else
        TermNorm = norm_simple,  % dummy
        TermNormSpec =
            [words("Invalid argument"), quote(TermNormStr), words("to the"),
            quote("--termination-norm"), words("option; must be")] ++
            quote_list_to_pieces("or",
                ["simple", "total", "num-data-elems"]) ++
            [suffix("."), nl],
        add_error(phase_options, TermNormSpec, !Specs)
    ),

    lookup_string_option(OptionTable, termination2_norm, Term2NormStr),
    ( if convert_termination_norm(Term2NormStr, Term2NormPrime) then
        Term2Norm = Term2NormPrime
    else
        Term2Norm = norm_simple, % dummy
        Term2NormSpec =
            [words("Invalid argument"), quote(TermNormStr), words("to the"),
            quote("--termination2-norm"), words("option; must be")] ++
            quote_list_to_pieces("or",
                ["simple", "total", "num-data-elems"]) ++
            [suffix("."), nl],
        add_error(phase_options, Term2NormSpec, !Specs)
    ).

:- pred check_debug_options(option_table::in,
    trace_level::out, trace_suppress_items::out, ssdb_trace_level::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_debug_options(OptionTable, TraceLevel, TraceSuppress, SSTraceLevel,
        !Specs) :-
    lookup_bool_option(OptionTable, force_disable_tracing,
        ForceDisableTracing),
    (
        ForceDisableTracing = yes,
        TraceLevel = trace_level_none
    ;
        ForceDisableTracing = no,
        lookup_string_option(OptionTable, trace_level, Trace),
        lookup_bool_option(OptionTable, exec_trace, ExecTrace),
        lookup_bool_option(OptionTable, decl_debug, DeclDebug),
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
                quote_list_to_pieces("or", ["minimum", "shallow", "deep",
                    "decl", "rep", "default"]) ++ [suffix("."), nl],
            add_error(phase_options, BadTraceLevelSpec, !Specs)
        )
    ),

    lookup_string_option(OptionTable, suppress_trace, SuppressStr),
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

    lookup_bool_option(OptionTable, force_disable_ssdebug,
        ForceDisableSSDB),
    (
        ForceDisableSSDB = yes,
        SSTraceLevel = ssdb_none
    ;
        ForceDisableSSDB = no,
        lookup_string_option(OptionTable, ssdb_trace_level, SSTrace),
        lookup_bool_option(OptionTable, source_to_source_debug, SSDB),
        ( if convert_ssdb_trace_level(SSTrace, SSDB, SSTL) then
            SSTraceLevel = SSTL
        else
            SSTraceLevel = ssdb_none,
            SSDBSpec =
                [words("Invalid argument"), quote(SSTrace), words("to the"),
                quote("--ssdb-trace"), words("option; must be")] ++
                quote_list_to_pieces("or", ["default", "none",
                    "shallow", "deep"]) ++ [suffix("."), nl],
            add_error(phase_options, SSDBSpec, !Specs)
        )
    ).

:- pred check_system_env_options(option_table::in,
    c_compiler_type::out, csharp_compiler_type::out,
    env_type::out, env_type::out, env_type::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_system_env_options(OptionTable, C_CompilerType, CSharp_CompilerType,
        HostEnvType, SystemEnvType, TargetEnvType, !Specs) :-
    lookup_string_option(OptionTable, c_compiler_type, C_CompilerTypeStr),
    ( if convert_c_compiler_type(C_CompilerTypeStr, C_CompilerTypePrime) then
        C_CompilerType = C_CompilerTypePrime
    else
        C_CompilerType = cc_unknown,   % dummy
        ValidC_CompilerTypes = [
            "gcc",
            "clang",
            "msvc_x86",
            "msvc_x64",
            "unknown"
        ],
        CCTpec =
            [words("Invalid argument"), quote(C_CompilerTypeStr),
            words("to the"), quote("--c-compiler-type"), words("option;"),
            words("must be")] ++
            quote_list_to_pieces("or", ValidC_CompilerTypes) ++
            [suffix("."), nl],
        add_error(phase_options, CCTpec, !Specs)
    ),

    lookup_string_option(OptionTable, csharp_compiler_type,
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
            quote_list_to_pieces("or", ["microsoft", "mono", "unknown"]) ++
            [suffix("."), nl],
        add_error(phase_options, CSCSpec, !Specs)
    ),

    lookup_string_option(OptionTable, host_env_type, HostEnvTypeStr),
    ( if convert_env_type(HostEnvTypeStr, HostEnvTypePrime) then
        HostEnvType = HostEnvTypePrime
    else
        HostEnvType = env_type_posix,   % dummy
        HostEnvSpec =
            [words("Invalid argument"), quote(HostEnvTypeStr), words("to the"),
            quote("--host-env-type"), words("option; must be")] ++
            quote_list_to_pieces("or",
                ["posix", "cygwin", "msys", "windows"]) ++
            [suffix("."), nl],
        add_error(phase_options, HostEnvSpec, !Specs)
    ),
    lookup_string_option(OptionTable, system_env_type, SystemEnvTypeStr),
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
            quote_list_to_pieces("or",
                ["posix", "cygwin", "msys", "windows"]) ++
            [suffix("."), nl],
        add_error(phase_options, SystemEnvSpec, !Specs)
    ),
    lookup_string_option(OptionTable, target_env_type, TargetEnvTypeStr),
    ( if convert_env_type(TargetEnvTypeStr, TargetEnvTypePrime) then
        TargetEnvType = TargetEnvTypePrime
    else
        TargetEnvType = env_type_posix,   % dummy
        TargetEnvTypeSpec =
            [words("Invalid argument"), quote(TargetEnvTypeStr),
            words("to the"), quote("--target-env-type"), words("option;"),
            words("must be")] ++
            quote_list_to_pieces("or",
                ["posix", "cygwin", "msys", "windows"]) ++
            [suffix("."), nl],
        add_error(phase_options, TargetEnvTypeSpec, !Specs)
    ).

:- pred check_hlds_dump_options(option_table::in, option_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_hlds_dump_options(!OptionTable, !Specs) :-
    lookup_string_option(!.OptionTable, dump_hlds_alias, DumpAlias),
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
    ).

:- pred check_diagnostics_options(option_table::in,
    limit_error_contexts_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_diagnostics_options(OptionTable, LimitErrorContextsMap, !Specs) :-
    lookup_int_option(OptionTable, inform_incomplete_switch_threshold,
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

    lookup_accumulating_option(OptionTable, limit_error_contexts,
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
        BadPieces = quote_list_to_pieces("or", BadLimitErrorContextsOptions),
        LECSpec =
            [words("Invalid arguments")] ++ BadPieces ++
            [words("to the"), quote("--limit-error-contexts"),
            words("option."), nl],
        add_error(phase_options, LECSpec, !Specs)
    ).

:- pred check_linked_target_extensions(option_table::in,
    linked_target_ext_info_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_linked_target_extensions(OptionTable, !:LinkExtMap, !Specs) :-
    map.init(!:LinkExtMap),
    lookup_string_option(OptionTable, object_file_extension, ObjExt),
    lookup_string_option(OptionTable, pic_object_file_extension, PicObjExt),
    lookup_string_option(OptionTable, executable_file_extension, ExecExt),
    lookup_string_option(OptionTable, library_extension, LibExt),
    lookup_string_option(OptionTable, shared_library_extension, SharedLibExt),

    map.det_insert(".install",
        linked_target_ext_info("", ltk_library_install), !LinkExtMap),
    map.det_insert(".install_gs_gas",
        linked_target_ext_info("", ltk_library_install_gs_gas), !LinkExtMap),

    record_linked_target_extension(ObjExt,
        "the --object-file-extension option",
        ltk_object_file, !LinkExtMap, !Specs),
    ( if ObjExt = PicObjExt then
        true
    else
        record_linked_target_extension(PicObjExt,
            "the --pic-object-file-extension",
            ltk_pic_object_file, !LinkExtMap, !Specs)
    ),

    record_linked_target_extension(ExecExt,
        "the --executable-file-extension option",
        ltk_executable, !LinkExtMap, !Specs),

    record_linked_target_extension(LibExt,
        "the --library-extension option",
        ltk_static_library, !LinkExtMap, !Specs),
    ( if LibExt = SharedLibExt then
        true
    else
        record_linked_target_extension(SharedLibExt,
            "the --shared-library-extension option",
            ltk_shared_library, !LinkExtMap, !Specs)
    ),

    get_all_obj_extensions(ObjExt, AllObjExtA, MaybeAllObjExtB),
    record_linked_target_extension(AllObjExtA,
        "the build-all version of the --object-file-extension option",
        ltk_all_object_file, !LinkExtMap, !Specs),
    (
        MaybeAllObjExtB = no
    ;
        MaybeAllObjExtB = yes(AllObjExtB),
        record_linked_target_extension(AllObjExtB,
            "the build-all version of the --object-file-extension option",
            ltk_all_object_file, !LinkExtMap, !Specs)
    ),
    ( if ObjExt = PicObjExt then
        true
    else
        get_all_obj_extensions(PicObjExt, AllPicObjExtA, MaybeAllPicObjExtB),
        record_linked_target_extension(AllPicObjExtA,
            "the build-all version of the --pic-object-file-extension option",
            ltk_all_pic_object_file, !LinkExtMap, !Specs),
        (
            MaybeAllPicObjExtB = no
        ;
            MaybeAllPicObjExtB = yes(AllPicObjExtB),
            record_linked_target_extension(AllPicObjExtB,
                "the build-all version of the " ++
                    "--pic-object-file-extension option",
                ltk_all_pic_object_file, !LinkExtMap, !Specs)
        )
    ).

:- pred record_linked_target_extension(string::in, string::in,
    linked_target_kind::in,
    linked_target_ext_info_map::in, linked_target_ext_info_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

record_linked_target_extension(OptionExt, OptionName, LinkedTargetKind,
        !LinkExtMap, !Specs) :-
    OptionInfo = linked_target_ext_info(OptionName, LinkedTargetKind),
    map.search_insert(OptionExt, OptionInfo, MaybeOldOptionInfo, !LinkExtMap),
    (
        MaybeOldOptionInfo = no
    ;
        MaybeOldOptionInfo = yes(OldOptionInfo),
        OldOptionInfo =
            linked_target_ext_info(OldOptionName, OldLinkedTargetKind),
        ( if
            ( LinkedTargetKind = ltk_all_object_file
            ; LinkedTargetKind = ltk_all_pic_object_file
            )
        then
            true
        else
            ( if OldLinkedTargetKind = ltk_library_install then
                Pieces = [words("Error:"), quote(OptionExt),
                    words("may not be specified as the value of"),
                    quote(OptionName), suffix(","),
                    words("because that extension is reserved"),
                    words("for other purposes."), nl]
            else
                Pieces = [words("Error: the extension"), quote(OptionExt),
                    words("is specified as the value of both"),
                    quote(OldOptionName), words("and"),
                    quote(OptionName), suffix("."), nl]
            ),
            Spec = no_ctxt_spec($pred, severity_error,
                phase_options, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ).

:- pred get_all_obj_extensions(string::in, string::out, maybe(string)::out)
    is det.

get_all_obj_extensions(Ext, AllExtA, MaybeAllExtB) :-
    AllExtA = Ext ++ "s",
    ( if string.remove_prefix(".", Ext, NoDotExt) then
        AllExtB = ".all_" ++ NoDotExt ++ "s",
        MaybeAllExtB = yes(AllExtB)
    else
        MaybeAllExtB = no
    ).

    % Check whether the color scheme chosen by the user contains
    % any errors, and records the colors in the option table
    % if there are none.
    %
    % Note that handle_colors, which is invoked only *after* our caller
    % check_option_values has finished, will handle the separate question
    % of whether the use of colors is *enabled*.
    %
:- pred check_color_options(option_table::in, option_table::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

check_color_options(!OptionTable, !Specs, !IO) :-
    lookup_string_option(!.OptionTable, color_scheme_set_to, ColorScheme),
    lookup_string_option(!.OptionTable, color_scheme_set_by, SetBy),
    ( if
        (
            SetBy = "default",
            Source = [words("the default value of the"),
                quote("--color-scheme"), words("option")]
        ;
            SetBy = "envvar",
            Source = [words("the value of the"),
                quote("MERCURY_COLOR_SCHEME"), words("environment variable")]
        ;
            SetBy = "option",
            Source = [words("the value of the"),
                quote("--color-scheme"), words("option")]
        )
    then
        record_color_scheme_in_options(Source, ColorScheme, ColorSchemeSpecs,
            !OptionTable, !IO),
        !:Specs = ColorSchemeSpecs ++ !.Specs
    else
        unexpected($pred, "unexpected value in color_scheme_set_by option")
    ),
    MaybeConvertColorSpecs = convert_color_spec_options(!.OptionTable),
    (
        MaybeConvertColorSpecs = ok1(_)
    ;
        MaybeConvertColorSpecs = error1(ConvertColorSpecs),
        !:Specs = ConvertColorSpecs ++ !.Specs
    ).

%---------------------------------------------------------------------------%

% None of the 'all' aliases actually include all the options.
convert_dump_alias("ALL", "abcdfgilmnprstuvzBCDEIMPRSTUYZ").
convert_dump_alias("allD", "abcdfgilmnprstuvzBCDEMPTY").
convert_dump_alias("all", "abcdfgilmnprstuvzBCEMPTZ").
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
convert_dump_alias("du", "TL").

%---------------------------------------------------------------------------%
:- end_module libs.check_options.
%---------------------------------------------------------------------------%
