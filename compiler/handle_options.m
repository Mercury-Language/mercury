%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2025 The Mercury Team.
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
:- import_module libs.options.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.

:- import_module getopt.
:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % Generate a dummy globals value based on the default values of the
    % options.
    %
:- pred generate_default_globals(io.text_output_stream::in, option_table::in,
    globals::out, io::di, io::uo) is det.

    % handle_given_options(ProgressStream, DefaultOptionTable,
    %   EnvOptFileVariables, Args, OptionArgs, NonOptionArgs,
    %   Specs, Globals, !IO).
    %
:- pred handle_given_options(io.text_output_stream::in, option_table::in,
    maybe_stdlib_grades::in, maybe1(list(string))::in, list(string)::in,
    list(string)::out, list(string)::out, list(error_spec)::out, globals::out,
    io::di, io::uo) is det.

    % usage_errors(Globals, Specs, !IO)
    %
    % Print the list of error messages, and then the usage message.
    %
:- pred usage_errors(io.text_output_stream::in, globals::in,
    list(error_spec)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module analysis.operations.
:- import_module libs.check_libgrades.
:- import_module libs.check_options.
:- import_module libs.compiler_util.
:- import_module libs.compute_grade.
:- import_module libs.file_util.
:- import_module libs.op_mode.
:- import_module libs.optimization_options.
:- import_module libs.trace_params.
:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module parse_tree.file_names.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module cord.
:- import_module dir.
:- import_module int.
:- import_module io.environment.
:- import_module io.file.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

generate_default_globals(ProgressStream, DefaultOptionTable0,
        DefaultGlobals, !IO) :-
    map.set(default_globals, bool(yes),
        DefaultOptionTable0, DefaultOptionTable),
    MaybeStdLibGrades = stdlib_grades_unknown,
    % This value of MaybeEnvOptFileMerStdLibDir will cause the value of
    % the chosen_stdlib_dir option to be set to "no", while also avoiding
    % an error message being generated for that fact. The value of that
    % option will be filled in for real when we create the real,
    % nondefault globals.
    MaybeEnvOptFileMerStdLibDir = error1([]),
    handle_given_options(ProgressStream, DefaultOptionTable, MaybeStdLibGrades,
        MaybeEnvOptFileMerStdLibDir, [], _, _, _, DefaultGlobals, !IO).

handle_given_options(ProgressStream, DefaultOptionTable, MaybeStdLibGrades,
        MaybeEnvOptFileMerStdLibDir, Args0, OptionArgs, Args,
        Specs, !:Globals, !IO) :-
    trace [compile_time(flag("debug_handle_given_options")), io(!TIO)] (
        io.write_string(ProgressStream, "\noriginal arguments\n", !TIO),
        dump_arguments(ProgressStream, Args0, !TIO)
    ),
    process_given_options(DefaultOptionTable, Args0, OptionArgs, Args,
        MaybeError, OptionTable, OptOptions, !IO),
    trace [compile_time(flag("debug_handle_given_options")), io(!TIO)] (
        io.write_string(ProgressStream, "\nfinal option arguments\n", !TIO),
        dump_arguments(ProgressStream, OptionArgs, !TIO),
        io.write_string(ProgressStream, "\nfinal non-option arguments\n",
            !TIO),
        dump_arguments(ProgressStream, Args, !TIO)
    ),
    convert_option_table_result_to_globals(ProgressStream, DefaultOptionTable,
        MaybeStdLibGrades, MaybeError, OptionTable, OptOptions,
        MaybeEnvOptFileMerStdLibDir, Specs, !:Globals, !IO),
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
            OpMode = opm_top_args(OpModeArgs, _),
            OpModeArgs = opma_augment(opmau_front_and_middle(
                opfam_target_object_and_executable))
        then
            % XXX Currently smart recompilation doesn't check that all the
            % files needed to link are present and up-to-date, so disable it.
            disable_smart_recompilation(ProgressStream, "linking",
                !Globals, !IO)
        else
            true
        )
    ).

    % process_given_options(DefaultOptionTable, Args,
    %   OptionArgs, NonOptionArgs, MaybeOptionTable, !IO):
    %
    % Process the options, but don't do any post-processing. This is mainly
    % useful for separating the list of arguments into option and non-option
    % arguments.
    %
:- pred process_given_options(option_table::in, list(string)::in,
    list(string)::out, list(string)::out, maybe(option_error(option))::out,
    option_table::out, cord(optimization_option)::out, io::di, io::uo) is det.

process_given_options(DefaultOptionTable, RawArgs, OptionArgs, NonOptionArgs,
        MaybeError, OptionTable, OptOptionsCord, !IO) :-
    get_short_option(ShortOption),
    get_long_option(LongOption),
    OptionOps = option_ops_userdata(ShortOption, LongOption, special_handler),
    getopt.process_options_userdata_io(OptionOps, RawArgs,
        OptionArgs, NonOptionArgs, MaybeError, _OptionsSet,
        DefaultOptionTable, OptionTable, cord.init, OptOptionsCord, !IO).

:- pred dump_arguments(io.text_output_stream::in, list(string)::in,
    io::di, io::uo) is det.

dump_arguments(_, [], !IO).
dump_arguments(ProgressStream, [Arg | Args], !IO) :-
    io.format(ProgressStream, "    <%s>\n", [s(Arg)], !IO),
    dump_arguments(ProgressStream, Args, !IO).

%---------------------------------------------------------------------------%

    % Convert string-valued options into the appropriate enumeration types,
    % and process implications among the options (i.e. situations where setting
    % one option implies setting/unsetting another one).
    %
:- pred convert_option_table_result_to_globals(io.text_output_stream::in,
    option_table::in, maybe_stdlib_grades::in, maybe(option_error(option))::in,
    option_table::in, cord(optimization_option)::in, maybe1(list(string))::in,
    list(error_spec)::out, globals::out, io::di, io::uo) is det.

convert_option_table_result_to_globals(ProgressStream, DefaultOptionTable,
        MaybeStdLibGrades, MaybeError, OptionTable0, OptOptionsCord,
        MaybeEnvOptFileMerStdLibDir, !:Specs, Globals, !IO) :-
    (
        MaybeError = yes(Error),
        ErrorMessage = option_error_to_string(Error),
        OptionTableSpec = no_ctxt_spec($pred, severity_error,
            phase_options, [words(ErrorMessage)]),
        !:Specs = [OptionTableSpec],
        generate_default_globals(ProgressStream, DefaultOptionTable,
            Globals, !IO)
    ;
        MaybeError = no,
        OptOptions = cord.list(OptOptionsCord),
        process_optimization_options(OptionTable, OptOptions, OptTuple),
        check_option_values(OptionTable0, OptionTable,
            Target, WordSize, GC_Method, TermNorm, Term2Norm,
            TraceLevel, TraceSuppress, SSTraceLevel,
            MaybeThreadSafe, C_CompilerType, CSharp_CompilerType,
            ReuseStrategy, MaybeFeedbackInfo,
            HostEnvType, SystemEnvType, TargetEnvType, LimitErrorContextsMap,
            LinkExtMap, !:Specs, !IO),
        % Shared libraries always use `--linkage shared'.
        % Apply this implication here, since it hurts nothing and avoids
        % having to provide a setter for the linkage and mercury_linkage
        % fields of the globals.
        lookup_bool_option(OptionTable, shared_lib_not_executable,
            SharedLibNotExecutable),
        (
            SharedLibNotExecutable = yes,
            Linkage = sos_shared,
            MercuryLinkage = sos_shared
        ;
            SharedLibNotExecutable = no,
            convert_checked_linkage(OptionTable, only_globals_linkage,
                Linkage),
            convert_checked_linkage(OptionTable, only_globals_mercury_linkage,
                MercuryLinkage)
        ),

        % If no --lib-linkage option has been specified, default to the
        % set of all possible linkages.
        lookup_accumulating_option(OptionTable,
            only_globals_library_install_linkages, LibraryInstallLinkageStrs),
        (
            LibraryInstallLinkageStrs = [],
            set.list_to_set([sos_static, sos_shared], LibraryInstallLinkages)
        ;
            LibraryInstallLinkageStrs = [_ | _],
            convert_library_install_linkages(LibraryInstallLinkageStrs,
                set.init, LibraryInstallLinkages, !Specs)
        ),

        decide_op_mode(OptionTable, OpMode, OtherOpModes),
        (
            OtherOpModes = []
        ;
            OtherOpModes = [_ | _],
            OpModeStrs = list.map(op_mode_to_option_string(OptionTable),
                [OpMode | OtherOpModes]),
            OpModePieces = [words("Error: only one of the following options"),
                words("may be given:")] ++
                quote_list_to_pieces("and", OpModeStrs) ++ [suffix("."), nl],
            add_error(phase_options, OpModePieces, !Specs)
        ),
        lookup_bool_option(OptionTable, default_globals, DefaultGlobals),
        % If we are generating the default globals, then executing
        % the else-part would result in infinite recursion, which
        % may or may not cause unbounded stack growth that can cause
        % the machine to thrash itself to death. Ask me how I know :-(
        %
        % This problem will arise if even check_option_values returns
        % errors *even for the default option table*. I (zs) cannot remember
        % this happening with our usual everything-turned-off-by-default
        % option table, but check_option_values now also checks the values
        % of environment variables, and if they contain some errors, then
        % retrying with the empty list of command-line arguments won't cure
        % those errors. (We cannot unset the environment variable causing
        % the problem, because that won't work on Java.)
        ( if
            ( !.Specs = []
            ; DefaultGlobals = yes
            )
        then
            convert_options_to_globals(ProgressStream,
                DefaultOptionTable, OptionTable,
                MaybeStdLibGrades, MaybeEnvOptFileMerStdLibDir,
                OptTuple, OpMode, Target, WordSize, GC_Method,
                TermNorm, Term2Norm, TraceLevel, TraceSuppress, SSTraceLevel,
                MaybeThreadSafe, C_CompilerType, CSharp_CompilerType,
                Linkage, MercuryLinkage, LibraryInstallLinkages,
                ReuseStrategy, MaybeFeedbackInfo,
                HostEnvType, SystemEnvType, TargetEnvType,
                LimitErrorContextsMap, LinkExtMap, !Specs, Globals, !IO)
        else
            generate_default_globals(ProgressStream, DefaultOptionTable,
                Globals, !IO)
        )
    ).

:- pred convert_checked_linkage(option_table::in, option::in,
    static_or_shared::out) is det.

convert_checked_linkage(OptionTable, Option, StaticOrShared) :-
    lookup_string_option(OptionTable, Option, OptionValue),
    ( if convert_static_or_shared(OptionValue, StaticOrSharedPrime) then
        StaticOrShared = StaticOrSharedPrime
    else
        % This shouldn't happen, because the option value should have been
        % already checked by the special option handler.
        unexpected($pred, "neither static nor shared")
    ).

:- pred convert_library_install_linkages(list(string)::in,
    set(static_or_shared)::in, set(static_or_shared)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

convert_library_install_linkages([], !LibraryInstallLinkages, !Specs).
convert_library_install_linkages([Str | Strs],
        !LibraryInstallLinkages, !Specs) :-
    ( if convert_static_or_shared(Str, StaticOrSharedPrime) then
        set.insert(StaticOrSharedPrime, !LibraryInstallLinkages)
    else
        Pieces = [words("Error: the"), quote("--library-linkage"),
            words("option expects"), words("either"), quote("static"),
            words("or"), quote("shared"), words("as its argument;"),
            words("got"), quote(Str), words("instead."), nl],
        add_error(phase_options, Pieces, !Specs)
    ),
    convert_library_install_linkages(Strs, !LibraryInstallLinkages, !Specs).

%---------------------------------------------------------------------------%

    % NOTE: We take two termination_norm arguments because each
    % termination analyser (the old and the new) has its own norm setting.
    %
:- pred convert_options_to_globals(io.text_output_stream::in,
    option_table::in, option_table::in,
    maybe_stdlib_grades::in, maybe1(list(string))::in,
    opt_tuple::in, op_mode::in, compilation_target::in, word_size::in,
    gc_method::in, termination_norm::in, termination_norm::in,
    trace_level::in, trace_suppress_items::in, ssdb_trace_level::in,
    may_be_thread_safe::in, c_compiler_type::in, csharp_compiler_type::in,
    static_or_shared::in, static_or_shared::in, set(static_or_shared)::in,
    reuse_strategy::in, maybe(feedback_info)::in,
    env_type::in, env_type::in, env_type::in, limit_error_contexts_map::in,
    linked_target_ext_info_map::in,
    list(error_spec)::in, list(error_spec)::out,
    globals::out, io::di, io::uo) is det.

convert_options_to_globals(ProgressStream, DefaultOptionTable, OptionTable0,
        MaybeStdLibGrades, MaybeEnvOptFileMerStdLibDir, !.OptTuple,
        OpMode, Target, WordSize, GC_Method, TermNorm, Term2Norm,
        TraceLevel, TraceSuppress, SSTraceLevel,
        MaybeThreadSafe, C_CompilerType, CSharp_CompilerType,
        Linkage, MercuryLinkage, LibLinkages, ReuseStrategy, MaybeFeedbackInfo,
        HostEnvType, SystemEnvType, TargetEnvType, LimitErrorContextsMap,
        LinkExtMap, !Specs, !:Globals, !IO) :-
    OptTuple0 = !.OptTuple,
    OT_AllowInlining0 =             OptTuple0 ^ ot_allow_inlining,
    OT_EnableConstStructPoly0 =     OptTuple0 ^ ot_enable_const_struct_poly,
    OT_EnableConstStructUser0 =     OptTuple0 ^ ot_enable_const_struct_user,
    OT_OptCommonStructs0 =          OptTuple0 ^ ot_opt_common_structs,
    OT_PropConstraints0 =           OptTuple0 ^ ot_prop_constraints,
    OT_PropLocalConstraints0 =      OptTuple0 ^ ot_prop_local_constraints,
    OT_OptDupCalls0 =               OptTuple0 ^ ot_opt_dup_calls,
    OT_PropConstants0 =             OptTuple0 ^ ot_prop_constants,
    OT_ElimExcessAssigns0 =         OptTuple0 ^ ot_elim_excess_assigns,
    OT_MergeCodeAfterSwitch0 =      OptTuple0 ^ ot_merge_code_after_switch,
    OT_OptLoopInvariants0 =         OptTuple0 ^ ot_opt_loop_invariants,
    OT_OptSVCell0 =                 OptTuple0 ^ ot_opt_svcell,
    OT_OptFollowCode0 =             OptTuple0 ^ ot_opt_follow_code,
    OT_OptUnusedArgs0 =             OptTuple0 ^ ot_opt_unused_args,
    OT_OptUnusedArgsIntermod0 =     OptTuple0 ^ ot_opt_unused_args_intermod,
    OT_OptHigherOrder0 =            OptTuple0 ^ ot_opt_higher_order,
    OT_HigherOrderSizeLimit0 =      OptTuple0 ^ ot_higher_order_size_limit,
    OT_SpecTypes0 =                 OptTuple0 ^ ot_spec_types,
    OT_SpecTypesUserGuided0 =       OptTuple0 ^ ot_spec_types_user_guided,
    OT_IntroduceAccumulators0 =     OptTuple0 ^ ot_introduce_accumulators,
    OT_OptLCMC0 =                   OptTuple0 ^ ot_opt_lcmc,
    OT_Deforest0 =                  OptTuple0 ^ ot_deforest,
    OT_Tuple0 =                     OptTuple0 ^ ot_tuple,
    OT_Untuple0 =                   OptTuple0 ^ ot_untuple,
    OT_OptMiddleRec0 =              OptTuple0 ^ ot_opt_middle_rec,
    OT_AllowHijacks0 =              OptTuple0 ^ ot_allow_hijacks,
    OT_OptMLDSTailCalls0 =          OptTuple0 ^ ot_opt_mlds_tailcalls,
    OT_OptimizeMlds0 =              OptTuple0 ^ ot_optimize_mlds,
    OT_StdLabels0 =                 OptTuple0 ^ ot_standardize_labels,
    OT_OptDups0 =                   OptTuple0 ^ ot_opt_dups,
    OT_OptFrames0 =                 OptTuple0 ^ ot_opt_frames,
    OT_StringBinarySwitchSize0 =    OptTuple0 ^ ot_string_binary_switch_size,

    lookup_string_option(OptionTable0, install_method, InstallMethodStr),
    ( if (InstallMethodStr = "" ; InstallMethodStr = "external") then
        InstallMethod = install_method_external_cmd
    else if InstallMethodStr = "internal" then
        InstallMethod = install_method_internal_code
    else
        InstallMethodSpec = [words("Error: the value of the"),
            quote("--install-method"), words("option is"),
            quote(InstallMethodStr), suffix(","),
            words("but the only valid values are"),
            quote("external"), words("and"), quote("internal"),
            suffix("."), nl],
        add_error(phase_options, InstallMethodSpec, !Specs),
        InstallMethod = install_method_external_cmd % Dummy value
    ),

    lookup_string_option(OptionTable0, install_command, InstallCmd),
    ( if InstallCmd = "" then
        FileInstallCmd = install_cmd_cp
    else
        FileInstallCmd = install_cmd_user(InstallCmd)
    ),

    % The use_cur_dir we pass here as the subdir setting is a dummy;
    % the real value is set at the very end of this predicate,
    % We can do this because no code between here and there uses
    % the value of that field, and we *have* to do this because
    % the code between here and there *can* update the values of
    % the options from which the subdir setting is computed.
    globals_init(DefaultOptionTable, OptionTable0, !.OptTuple, OpMode,
        MaybeFeedbackInfo, FileInstallCmd, TraceSuppress, ReuseStrategy,
        LimitErrorContextsMap, LinkExtMap, C_CompilerType, CSharp_CompilerType,
        Linkage, MercuryLinkage, LibLinkages, MaybeStdLibGrades, Target,
        use_cur_dir, WordSize, GC_Method, TermNorm, Term2Norm,
        TraceLevel, SSTraceLevel, MaybeThreadSafe,
        HostEnvType, SystemEnvType, TargetEnvType, InstallMethod, !:Globals),

    globals.lookup_bool_option(!.Globals, experiment2, Experiment2),
    (
        Experiment2 = no
    ;
        Experiment2 = yes,
        globals.set_option(pack_everything, bool(yes), !Globals),
        globals.set_option(allow_double_word_ints, bool(yes), !Globals),
        globals.set_option(allow_packing_dummies, bool(yes), !Globals),
        globals.set_option(allow_packing_ints, bool(yes), !Globals),
        globals.set_option(allow_packing_chars, bool(yes), !Globals),
        globals.set_option(allow_packing_local_sectags, bool(yes), !Globals),
        globals.set_option(allow_packing_remote_sectags, bool(yes), !Globals)
    ),

    % NOTE: this call must occur *before* any code below that implicitly
    % sets options based on the target language or GC method.
    check_grade_component_compatibility(!.Globals, Target, GC_Method, !Specs),
    check_for_incompatibilities(!.Globals, OpMode, !Specs),

    handle_implications_of_pregen_target_spf(!Globals, Target,
        OT_StringBinarySwitchSize0, OT_StringBinarySwitchSize, !Specs),
    handle_implications_of_parallel(!Globals, !Specs),
    handle_gc_options(!Globals, GC_Method, OT_OptFrames0, OT_OptFrames,
        !Specs),
    handle_minimal_model_options(!Globals, AllowHijacksMMSC, !Specs),

    TraceEnabled = is_exec_trace_enabled_at_given_trace_level(TraceLevel),
    handle_debugging_options(Target, TraceLevel, TraceEnabled, SSTraceLevel,
        AllowSrcChangesDebug, !Globals, !Specs),
    maybe_update_event_set_file_name(!Globals, !IO),

    globals.lookup_bool_option(!.Globals, profile_deep, ProfileDeep),
    handle_profiling_options(!Globals, Target, ProfileDeep,
        AllowSrcChangesProf,
        OT_HigherOrderSizeLimit0, OT_HigherOrderSizeLimit, !Specs),
    handle_record_term_sizes_options(!Globals, AllowOptLCMCTermSize, !Specs),

    % Debugging and profiling both affect what stack layouts we need,
    % which is why this call is after both handle_debugging_options and
    % handle_profiling_options.
    handle_stack_layout_options(!Globals, OT_OptDups0, OT_OptDups,
        OT_StdLabels0, OT_StdLabels),

    % These three calls are in this order because
    %
    % - handle_op_mode_implications may set transitive_optimization,
    % - handle_option_to_option_implications may set
    %   intermodule_optimization if transitive_optimization is set,
    % - maybe_disable_smart_recompilation then needs the final value
    %   of intermodule_optimization.
    %
    % However, while it is hard to see, there is no conflict here.
    % handle_op_mode_implications sets transitive_optimization
    % only when the op_mode calls for making .trans_opt files,
    % while maybe_disable_smart_recompilation needs the value of
    % intermodule_optimization only when the op_mode calls for
    % generating target language code.
    handle_op_mode_implications(OpMode, !Globals),
    handle_option_to_option_implications(OpMode, !Globals),
    maybe_disable_smart_recompilation(ProgressStream, OpMode, !Globals, !IO),

    handle_chosen_stdlib_dir(MaybeEnvOptFileMerStdLibDir, !Globals, !Specs),
    handle_libgrades(ProgressStream, !Globals, !Specs, !IO),
    handle_subdir_setting(OpMode, !Globals),
    handle_directory_options(OpMode, !Globals),
    handle_target_compile_link_symlink_options(!Globals),
    handle_compiler_developer_options(!Globals, !IO),
    handle_compare_specialization(!Globals),
    handle_colors(!Globals),

    (
        OT_OptimizeMlds0 = do_not_optimize_mlds,
        % --no-mlds-optimize implies --no-optimize-tailcalls.
        OT_OptMLDSTailCalls = do_not_opt_mlds_tailcalls
    ;
        OT_OptimizeMlds0 = optimize_mlds,
        OT_OptMLDSTailCalls = OT_OptMLDSTailCalls0
    ),
    handle_non_tail_rec_warnings(!.Globals, OptTuple0, OT_OptMLDSTailCalls,
        !Specs),

    % The rest of the code of this predicate computes the various fields
    % of the optimization options tuple.

    ( if
        AllowSrcChangesDebug = allow_src_changes,
        AllowSrcChangesProf = allow_src_changes
    then
        OT_AllowInlining = OT_AllowInlining0
    else
        OT_AllowInlining = do_not_allow_inlining
    ),

    handle_const_struct(Target, OpMode, TraceLevel, TraceSuppress,
        OT_EnableConstStructPoly0, OT_EnableConstStructPoly,
        OT_EnableConstStructUser0, OT_EnableConstStructUser),

    % `--optimize-constant-propagation' effectively inlines builtins.
    %
    % We want to allow constant propagation in deep profiling grades,
    % so `--no-allow-inlining' should not cause it to be disabled.
    % (Other forms of inlining must be disabled for deep profiling.)
    %
    % `--no-allow-inlining' should imply
    % `--no-optimize-constant-propagation' otherwise,
    % e.g. when tracing is enabled.
    OT_InlineBuiltins0 = OptTuple0 ^ ot_inline_builtins,
    ( if
        OT_InlineBuiltins0 = inline_builtins,
        ( OT_AllowInlining = allow_inlining
        ; ProfileDeep = yes
        )
    then
        OT_PropConstants = OT_PropConstants0
    else
        OT_PropConstants = do_not_prop_constants
    ),

    % XXX deforestation and constraint propagation do not perform folding
    % on polymorphic predicates correctly with --body-typeinfo-liveness.
    globals.lookup_bool_option(!.Globals, body_typeinfo_liveness,
        BodyTypeInfoLiveness),
    globals.lookup_bool_option(!.Globals, reorder_conj, ReorderConj),
    ( if
        AllowSrcChangesDebug = allow_src_changes,
        % --no-reorder-conj implies --no-deforestation,
        ReorderConj = bool.yes,
        % XXX The folding done by deforestation on polymorphic predicates
        % does not respect --body-typeinfo-liveness.
        BodyTypeInfoLiveness = bool.no
    then
        OT_Deforest = OT_Deforest0
    else
        OT_Deforest = do_not_deforest
    ),

    globals.lookup_bool_option(!.Globals, stack_segments, StackSegments),
    ( if
        % The middle_rec special-case code generator cannot implement tracing.
        TraceEnabled = exec_trace_is_not_enabled,

        % The RTTI needed for accurate gc cannot handle the stack shapes
        % that it generates.
        GC_Method \= gc_accurate,

        % XXX If trailing is enabled, middle recursion optimization
        % can generate code which does not allocate a stack frame
        % even though stack slots are used to save and restore the trail,
        % if the code being optimized contains a construct which
        % might save/restore the trail state, i.e. an if-then-else,
        % negation, disjunction, or commit.
        globals.lookup_bool_option(!.Globals, use_trail, bool.no),

        % The cut-down stack frames used by middle recursion optimization
        % don't include return addresses. Since stack extension arranges for
        % the return to the old stack segments by overriding the return
        % address, stack extension via stack segments and middle recursion
        % optimization are incompatible.
        StackSegments = bool.no
    then
        OT_OptMiddleRec = OT_OptMiddleRec0
    else
        OT_OptMiddleRec = do_not_opt_middle_rec
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
    ( if
        TraceEnabled = exec_trace_is_not_enabled,
        GC_Method \= gc_accurate,
        AllowHijacksMMSC = bool.yes
    then
        OT_AllowHijacks = OT_AllowHijacks0
    else
        OT_AllowHijacks = do_not_allow_hijacks
    ),

    % XXX With accurate gc, we disable type specialization, because
    % type specialization may create type class constraints of the form
    % `c(t(T))' (e.g. `enum(var(T))'' in library/sparse_bitset.m),
    % which the current RTTI system can't handle.
    ( if
        AllowSrcChangesDebug = allow_src_changes,
        GC_Method \= gc_accurate
    then
        OT_SpecTypes = OT_SpecTypes0,
        (
            OT_SpecTypes = spec_types,
            % If we are doing type-specialization, we may as well take
            % advantage of the declarations supplied by the programmer.
            OT_SpecTypesUserGuided = spec_types_user_guided
        ;
            OT_SpecTypes = do_not_spec_types,
            OT_SpecTypesUserGuided = OT_SpecTypesUserGuided0
        )
    else
        OT_SpecTypes = do_not_spec_types,
        OT_SpecTypesUserGuided = do_not_spec_types_user_guided
    ),

    % The local constraint propagation transformation (constraint.m)
    % is a required part of the constraint propagation transformation
    % performed by deforest.m, so if propagating local constraints
    % is disallowed, we can't propagate any other constraints either.
    ( if
        AllowSrcChangesDebug = allow_src_changes,
        % --no-reorder-conj implies --no-local-constraint-propagation,
        % and hence also --no-constraint-propagation.
        ReorderConj = bool.yes
    then
        OT_PropLocalConstraints = OT_PropLocalConstraints0
    else
        OT_PropLocalConstraints = do_not_prop_local_constraints
    ),
    ( if
        % XXX The folding done by constraint propagation on polymorphic
        % predicates does not respect --body-typeinfo-liveness.
        BodyTypeInfoLiveness = bool.no,
        OT_PropLocalConstraints = prop_local_constraints
    then
        OT_PropConstraints = OT_PropConstraints0
    else
        OT_PropConstraints = do_not_prop_constraints
    ),

    % --introduce-accumulators implies --excess-assign and
    % --common-struct.
    (
        OT_IntroduceAccumulators0 = introduce_accumulators,
        OT_OptCommonStructs = opt_common_structs
    ;
        OT_IntroduceAccumulators0 = do_not_introduce_accumulators,
        OT_OptCommonStructs = OT_OptCommonStructs0
    ),
    ( if
        % Doing elim_excess_assigns prevents useless variables from
        % cluttering the trace. Its explicit setting removes a source
        % of variability in the goal paths reported by tracing.
        ( TraceEnabled = exec_trace_is_enabled
        ; OT_IntroduceAccumulators0 = introduce_accumulators
        )
    then
        OT_ElimExcessAssigns = elim_excess_assigns
    else
        OT_ElimExcessAssigns = OT_ElimExcessAssigns0
    ),
    ( if
        % Don't do the unused_args optimization when making the
        % optimization interface.
        ( AllowSrcChangesDebug = do_not_allow_src_changes
        ; OpMode = opm_top_args(opma_augment(opmau_make_plain_opt), _)
        )
    then
        OT_OptUnusedArgs = do_not_opt_unused_args
    else
        OT_OptUnusedArgs = OT_OptUnusedArgs0
    ),

    % --intermod-unused-args implies --intermodule-optimization and
    % --optimize-unused-args. The latter implies that it must be switched
    % off if --optimize-unused-args is disallowed,
    % XXX In the presence of --intermod-unused-args, we used to *ignore*
    % whether some code above disallowed --optimize-unused-args; we used
    % to turn it *back on*, and set --intermodule-optimization to yes anyway.
    ( if
        OT_OptUnusedArgsIntermod0 = opt_unused_args_intermod,
        OT_OptUnusedArgs = opt_unused_args
    then
        OT_OptUnusedArgsIntermod = OT_OptUnusedArgsIntermod0,
        globals.set_option(intermodule_optimization, bool(yes), !Globals)
    else
        OT_OptUnusedArgsIntermod = do_not_opt_unused_args_intermod
    ),

    % XXX With accurate gc, we need to disable optimize-constructor-last-call
    % as currently the collector (and tracing code generator) knows neither
    % about the pre-constructed data structures nor the references into them
    % that this optimisation uses.
    ( if
        AllowSrcChangesDebug = allow_src_changes,
        ProfileDeep = bool.no,
        AllowOptLCMCTermSize = bool.yes,
        GC_Method \= gc_accurate
    then
        OT_OptLCMC = OT_OptLCMC0
    else
        OT_OptLCMC = do_not_opt_lcmc
    ),

    (
        TraceEnabled = exec_trace_is_enabled,
        % The explicit setting of the following option removes a source
        % of variability in the goal paths reported by tracing.
        OT_OptFollowCode = opt_follow_code
    ;
        TraceEnabled = exec_trace_is_not_enabled,
        OT_OptFollowCode = OT_OptFollowCode0
    ),

    (
        AllowSrcChangesDebug = do_not_allow_src_changes,
        OT_OptDupCalls = do_not_opt_dup_calls,
        OT_OptSVCell = do_not_opt_svcell,
        OT_OptLoopInvariants = do_not_opt_loop_invariants,
        OT_OptHigherOrder = do_not_opt_higher_order,
        OT_Untuple = do_not_untuple,
        OT_Tuple = do_not_tuple,
        OT_MergeCodeAfterSwitch = do_not_merge_code_after_switch
    ;
        AllowSrcChangesDebug = allow_src_changes,
        OT_OptDupCalls = OT_OptDupCalls0,
        OT_OptSVCell = OT_OptSVCell0,
        OT_OptLoopInvariants = OT_OptLoopInvariants0,
        OT_OptHigherOrder = OT_OptHigherOrder0,
        OT_Tuple = OT_Tuple0,
        OT_Untuple = OT_Untuple0,
        OT_MergeCodeAfterSwitch = OT_MergeCodeAfterSwitch0
    ),

    !OptTuple ^ ot_allow_inlining :=            OT_AllowInlining,
    !OptTuple ^ ot_enable_const_struct_poly :=  OT_EnableConstStructPoly,
    !OptTuple ^ ot_enable_const_struct_user :=  OT_EnableConstStructUser,
    !OptTuple ^ ot_opt_common_structs :=        OT_OptCommonStructs,
    !OptTuple ^ ot_prop_constraints :=          OT_PropConstraints,
    !OptTuple ^ ot_prop_local_constraints :=    OT_PropLocalConstraints,
    !OptTuple ^ ot_opt_dup_calls :=             OT_OptDupCalls,
    !OptTuple ^ ot_prop_constants :=            OT_PropConstants,
    !OptTuple ^ ot_opt_svcell :=                OT_OptSVCell,
    !OptTuple ^ ot_opt_loop_invariants :=       OT_OptLoopInvariants,
    !OptTuple ^ ot_elim_excess_assigns :=       OT_ElimExcessAssigns,
    !OptTuple ^ ot_merge_code_after_switch :=   OT_MergeCodeAfterSwitch,
    !OptTuple ^ ot_opt_follow_code :=           OT_OptFollowCode,
    !OptTuple ^ ot_opt_unused_args :=           OT_OptUnusedArgs,
    !OptTuple ^ ot_opt_unused_args_intermod :=  OT_OptUnusedArgsIntermod,
    !OptTuple ^ ot_opt_higher_order :=          OT_OptHigherOrder,
    !OptTuple ^ ot_higher_order_size_limit :=   OT_HigherOrderSizeLimit,
    !OptTuple ^ ot_spec_types :=                OT_SpecTypes,
    !OptTuple ^ ot_spec_types_user_guided :=    OT_SpecTypesUserGuided,
    !OptTuple ^ ot_opt_lcmc :=                  OT_OptLCMC,
    !OptTuple ^ ot_deforest :=                  OT_Deforest,
    !OptTuple ^ ot_tuple :=                     OT_Tuple,
    !OptTuple ^ ot_untuple :=                   OT_Untuple,
    !OptTuple ^ ot_opt_middle_rec :=            OT_OptMiddleRec,
    !OptTuple ^ ot_allow_hijacks :=             OT_AllowHijacks,
    !OptTuple ^ ot_opt_mlds_tailcalls :=        OT_OptMLDSTailCalls,
    !OptTuple ^ ot_standardize_labels :=        OT_StdLabels,
    !OptTuple ^ ot_opt_dups :=                  OT_OptDups,
    !OptTuple ^ ot_opt_frames :=                OT_OptFrames,
    !OptTuple ^ ot_string_binary_switch_size := OT_StringBinarySwitchSize,

    globals.lookup_bool_option(!.Globals, highlevel_code, HighLevelCode),
    (
        HighLevelCode = no,
        postprocess_options_lowlevel(!Globals, !OptTuple)
    ;
        HighLevelCode = yes
    ),
    globals.set_opt_tuple(!.OptTuple, !Globals),
    globals_init_mutables(!.Globals, !IO).

%---------------------------------------------------------------------------%

    % Options updated:
    %   none
    %
:- pred check_for_incompatibilities(globals::in, op_mode::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_incompatibilities(Globals, OpMode, !Specs) :-
    % `--transitive-intermodule-optimization' and `--make' are
    % not compatible with each other.
    globals.lookup_bool_option(Globals, transitive_optimization, TransOpt),
    (
        TransOpt = bool.yes,
        ( if
            (
                OpMode = opm_top_args(_, InvokedByMMCMake),
                InvokedByMMCMake = op_mode_invoked_by_mmc_make
            ;
                OpMode = opm_top_make
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
        TransOpt = bool.no
    ),

    % `--intermodule-optimization' and `--intermodule-analysis' are
    % not compatible with each other.
    globals.lookup_bool_option(Globals, intermodule_optimization,
        InterModOpt),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        InterModAnalysis),
    ( if
        InterModOpt = bool.yes,
        InterModAnalysis = bool.yes
    then
        OptAnalysisSpec =
            [words("The"), quote("--intermodule-optimization"),
            words("option is incompatible with the"),
            quote("--intermodule-analysis"), words("option."), nl],
        add_error(phase_options, OptAnalysisSpec, !Specs)
    else
        true
    ),

    globals.lookup_bool_option(Globals, extra_init_functions,
        ExtraInitFunctions),
    ( if
        OpMode = opm_top_generate_standalone_interface(_),
        ExtraInitFunctions = bool.yes
    then
        ExtraInitsSpec =
            [words("The"), quote("--generate-standalone-interface"),
            words("option is incompatible with the"),
            quote("--extra-initialization-functions"), words("option."), nl],
        add_error(phase_options, ExtraInitsSpec, !Specs)
    else
        true
    ).

%---------------------%

    % Options updated:
    %   allow_double_word_fields
    %   allow_multi_arm_switches
    %   allow_packing_dummies
    %   allow_packing_ints
    %   arg_pack_bits
    %   asm_labels
    %   can_compare_constants_as_ints
    %   delay_partial_instantiations
    %   det_copy_out
    %   executable_file_extension
    %   gc
    %   gcc_global_registers
    %   gcc_non_local_gotos
    %   highlevel_code
    %   nondet_copy_out
    %   num_ptag_bits
    %   pretest_equality_cast_pointers
    %   put_commit_in_own_func
    %   reclaim_heap_on_nondet_failure
    %   reclaim_heap_on_semidet_failure
    %   single_prec_float
    %   structure_reuse_analysis
    %   structure_sharing_analysis
    %   unboxed_float
    %   unboxed_int64s
    %   unboxed_no_tag_types
    %   use_float_registers
    %
    % Other globals fields updated:
    %   backend_foreign_languages
    %
:- pred handle_implications_of_pregen_target_spf(globals::in, globals::out,
    compilation_target::in, int::in, int::out,
    list(error_spec)::in, list(error_spec)::out) is det.

handle_implications_of_pregen_target_spf(!Globals, Target,
        OT_StringBinarySwitchSize0, OT_StringBinarySwitchSize, !Specs) :-
    % --pregenerated-dist sets options so that the pre-generated C source
    % distribution can be compiled equally on 32-bit and 64-bit platforms.
    % Any changes here may require changes in runtime/mercury_conf_param.h.
    globals.lookup_bool_option(!.Globals, pregenerated_dist, PregeneratedDist),
    (
        PregeneratedDist = bool.yes,
        globals.set_word_size(word_size_32, !Globals),
        globals.set_option(num_ptag_bits, int(2), !Globals),
        globals.set_option(arg_pack_bits, int(32), !Globals),
        globals.set_option(unboxed_float, bool(no), !Globals),
        globals.set_option(unboxed_int64s, bool(no), !Globals),
        globals.set_option(single_prec_float, bool(no), !Globals),
        globals.set_option(allow_double_word_fields, bool(no), !Globals)
    ;
        PregeneratedDist = bool.no
    ),

    % We assume that single-precision floats do not need to be boxed.
    option_implies(single_prec_float, unboxed_float, bool(yes), !Globals),

    (
        Target = target_c,
        BackendForeignLanguages = ["c"],
        globals.lookup_int_option(!.Globals, num_ptag_bits, NumPtagBits0),
        ( if NumPtagBits0 = -1 then
            % The default value of NumPtagBits0 is -1. We replace it with the
            % autoconf-determined value for the number of low ptag bits,
            % which is passed to us from the `mmc' script using the
            % deliberately undocumented --conf-low-tag-bits option.
            globals.lookup_int_option(!.Globals, conf_low_ptag_bits,
                NumPtagBits)
        else
            % This non-default value may have been set above for
            % PregeneratedDist = yes, or it could have been set by the user.
            % The only legitimate reason for the latter is cross-compilation.
            NumPtagBits = NumPtagBits0
        ),
        globals.set_option(num_ptag_bits, int(NumPtagBits), !Globals),
        ( if (NumPtagBits = 2 ; NumPtagBits = 3) then
            true
        else
            NumPtagBitsSpec =
                [words("Error: the value of the"), quote("--num-ptag-bits"),
                words("option is"), int_fixed(NumPtagBits), suffix(","),
                words("but the only valid values are 2 and 3."), nl],
            add_error(phase_options, NumPtagBitsSpec, !Specs)
        ),

        globals.lookup_bool_option(!.Globals, highlevel_code, HighLevelCode),
        (
            HighLevelCode = no
        ;
            HighLevelCode = yes,
            % Generating high-level C code requires putting each commit
            % in its own function, to avoid problems with setjmp() and
            % non-volatile local variables. It also disables the use
            % of low-level gcc extensions.
            globals.set_option(put_commit_in_own_func, bool(yes), !Globals),
            globals.set_option(gcc_non_local_gotos, bool(no), !Globals),
            globals.set_option(gcc_global_registers, bool(no), !Globals),
            globals.set_option(asm_labels, bool(no), !Globals)
        ),

        % Since we have never targeted 16-bit platforms, the minimum number
        % of low ptag bits we ever configure is two.
        ( if NumPtagBits >= 2 then
            globals.set_option(can_compare_constants_as_ints, bool(yes),
                !Globals)
        else
            globals.set_option(can_compare_constants_as_ints, bool(no),
                !Globals)
        ),

        % Argument packing only works on back-ends which use low-level data,
        % i.e the C backend. For the other target languages, implementing
        % argument packing will require not just a lot of work on RTTI,
        % but also generalizing field addressing, to allow both single fields
        % and a group of adjacent fields packed into a single word to be
        % addressed via a mechanism other than an argument's name.
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
        globals.set_option(arg_pack_bits, int(ArgPackBits), !Globals),
        % Leave the value of allow_double_word_fields as set by the user.
        % Leave the value of allow_packing_dummies as set by the user.
        % Leave the value of allow_packing_ints as set by the user.

        OT_StringBinarySwitchSize = OT_StringBinarySwitchSize0
    ;
        ( Target = target_java
        ; Target = target_csharp
        ),
        globals.set_option(num_ptag_bits, int(0), !Globals),

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
        %   - unboxed 64-bit integers
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
        %   - no structure reuse
        %     Because mlds_to_java_stmt.m does not handle assign_if_in_heap.
        %
        % C# should be the same as Java, except that:
        %   - C# supports pass-by-reference, but for reasons explained in
        %     mlds_to_cs_stmt.m, we pretend at the MLDS level that it doesn't.

        globals.set_gc_method(gc_automatic, !Globals),
        globals.set_option(gc, string("automatic"), !Globals),
        globals.set_option(reclaim_heap_on_nondet_failure, bool(no),
            !Globals),
        globals.set_option(reclaim_heap_on_semidet_failure, bool(no),
            !Globals),
        globals.set_option(highlevel_code, bool(yes), !Globals),
        globals.set_option(gcc_non_local_gotos, bool(no), !Globals),
        globals.set_option(gcc_global_registers, bool(no), !Globals),
        globals.set_option(asm_labels, bool(no), !Globals),
        globals.set_option(unboxed_float, bool(yes), !Globals),
        globals.set_option(unboxed_int64s, bool(yes), !Globals),
        globals.set_option(nondet_copy_out, bool(yes), !Globals),
        globals.set_option(det_copy_out, bool(yes), !Globals),
        globals.set_option(unboxed_no_tag_types, bool(no), !Globals),
        globals.set_option(pretest_equality_cast_pointers, bool(yes),
            !Globals),
        globals.set_option(structure_reuse_analysis, bool(no), !Globals),
        globals.set_option(structure_sharing_analysis, bool(no), !Globals),

        (
            Target = target_csharp,
            BackendForeignLanguages = ["csharp"],
            globals.set_option(executable_file_extension, string(".exe"),
                !Globals)
        ;
            Target = target_java,
            BackendForeignLanguages = ["java"]
        ),

        % In the non-C backends, it may not be possible to cast a value
        % of a non-enum du type to an integer.
        globals.set_option(can_compare_constants_as_ints, bool(no), !Globals),

        globals.set_option(arg_pack_bits, int(0), !Globals),
        globals.set_option(allow_double_word_fields, bool(no), !Globals),
        globals.set_option(allow_packing_dummies, bool(no), !Globals),
        globals.set_option(allow_packing_ints, bool(no), !Globals),

        % Switch off string hash switches until these backends implement
        % the hash operations.
        OT_StringBinarySwitchSize = 999999
    ),

    % We only use the float registers if floats would not fit into the
    % regular registers.
    option_implies(unboxed_float, use_float_registers, bool(no), !Globals),
    option_implies(highlevel_code, use_float_registers, bool(no), !Globals),

    % Only set the backend foreign languages if they are unset.
    globals.lookup_accumulating_option(!.Globals,
        backend_foreign_languages, CurrentBackendForeignLanguage),
    (
        CurrentBackendForeignLanguage = [],
        globals.set_option(backend_foreign_languages,
            accumulating(BackendForeignLanguages), !Globals)
    ;
        CurrentBackendForeignLanguage = [_ | _]
    ).

%---------------------%

    % Options updated:
    %   ansi_c
    %   implicit_parallelism
    %   par_loop_control
    %   pre_implicit_parallelism_simplify
    %
:- pred handle_implications_of_parallel(globals::in, globals::out,
    list(error_spec)::in, list(error_spec)::out) is det.

handle_implications_of_parallel(!Globals, !Specs) :-
    current_grade_supports_par_conj(!.Globals, GradeSupportsParConj),
    globals.lookup_bool_option(!.Globals, parallel, Parallel),
    globals.lookup_bool_option(!.Globals, threadscope, Threadscope),
    ( if
        GradeSupportsParConj = bool.no,
        Threadscope = bool.yes
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
        ImplicitParallelism = bool.yes,
        (
            GradeSupportsParConj = bool.yes,
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
            GradeSupportsParConj = bool.no,
            (
                Parallel = yes,
                NoParConjSupportSpec =
                    [words("The"), quote("--implicit-parallelism"),
                    words("option requires a grade that"),
                    words("supports parallel conjunctions."),
                    words("Use a low-level C grade without trailing."), nl],
                add_error(phase_options, NoParConjSupportSpec, !Specs)
            ;
                Parallel = bool.no
            ),
            globals.set_option(implicit_parallelism, bool(no), !Globals)
        )
    ;
        ImplicitParallelism = bool.no
    ),
    % Perform a simplification pass before the implicit parallelism pass to
    % ensure that the HLDS more-closely matches the feedback data.
    option_implies(implicit_parallelism, pre_implicit_parallelism_simplify,
        bool(yes), !Globals),

    % Loop control is not applicable in non-parallel grades.
    (
        GradeSupportsParConj = bool.yes
    ;
        GradeSupportsParConj = bool.no,
        globals.set_option(par_loop_control, bool(no), !Globals)
    ),

    % The pthreads headers on some architectures (Solaris, Linux)
    % don't work with -ansi.
    % XXX We don't pass -ansi to the C compiler anymore.
    option_implies(parallel, ansi_c, bool(no), !Globals).

%---------------------%

    % Options updated:
    %   agc_stack_layout
    %   body_typeinfo_liveness
    %   opt_no_return_calls
    %   reclaim_heap_on_nondet_failure
    %   reclaim_heap_on_semidet_failure
    %
:- pred handle_gc_options(globals::in, globals::out, gc_method::in,
    maybe_opt_frames::in, maybe_opt_frames::out,
    list(error_spec)::in, list(error_spec)::out) is det.

handle_gc_options(!Globals, GC_Method, OT_OptFrames0, OT_OptFrames, !Specs) :-
    % --gc accurate for the LLDS back-end requires `agc' stack layouts,
    % typeinfo liveness, and needs hijacks, frameopt, and middle recursion
    % optimization to be switched off.
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
    (
        GC_Method = gc_accurate,
        globals.set_option(agc_stack_layout, bool(yes), !Globals),
        globals.set_option(body_typeinfo_liveness, bool(yes), !Globals),
        % We turn off optimization of stack slots for no_return calls,
        % because that optimization does not preserve agc typeinfo liveness.
        globals.set_option(opt_no_return_calls, bool(no), !Globals),

        globals.set_option(reclaim_heap_on_semidet_failure, bool(no),
            !Globals),
        globals.set_option(reclaim_heap_on_nondet_failure, bool(no),
            !Globals),

        OT_OptFrames = do_not_opt_frames,

        % ml_gen_params_base and ml_declare_env_ptr_arg, in ml_code_util.m,
        % both assume (for accurate GC) that continuation environments
        % are always allocated on the stack, which means that things won't
        % work if --gc accurate is enabled when targeting any language
        % other than C. (Java and C# put their environments on the heap.)
        globals.lookup_bool_option(!.Globals, highlevel_code,
            HighLevelCode),
        globals.get_target(!.Globals, Target),
        ( if
            HighLevelCode = bool.yes,
            Target \= target_c
        then
            AGCEnvSpec =
                [words("The only target language that"),
                words_quote("--gc accurate"), words("is incompatible with"),
                words("is C."), nl],
            add_error(phase_options, AGCEnvSpec, !Specs)
        else
            true
        )
    ;
        ( GC_Method = gc_automatic
        ; GC_Method = gc_none
        ; GC_Method = gc_boehm
        ; GC_Method = gc_boehm_debug
        ; GC_Method = gc_hgc
        ),
        OT_OptFrames = OT_OptFrames0,

        % Conservative GC implies --no-reclaim-heap-*
        GCIsConservative = gc_is_conservative(GC_Method),
        (
            GCIsConservative = bool.yes,
            globals.set_option(reclaim_heap_on_semidet_failure, bool(no),
                !Globals),
            globals.set_option(reclaim_heap_on_nondet_failure, bool(no),
                !Globals)
        ;
            GCIsConservative = bool.no,
            globals.lookup_bool_option(!.Globals, highlevel_code,
                HighLevelCode),
            globals.lookup_bool_option(!.Globals,
                reclaim_heap_on_semidet_failure, SemidetReclaim),
            globals.lookup_bool_option(!.Globals,
                reclaim_heap_on_nondet_failure, NondetReclaim),
            % The LLDS backend can tolerate SemidetReclaim \= NondetReclaim,
            % but the MLDS backend cannot.
            ( if
                HighLevelCode = yes,
                SemidetReclaim \= NondetReclaim
            then
                ReclaimPieces = [words("Sorry, not implemented:"),
                    quote("--high-level-code"), words("and just one of"),
                    quote("--reclaim-heap-on-semidet-failure"), words("and"),
                    quote("--reclaim-heap-on-nondet-failure"), suffix("."),
                    words("Use"), quote("--(no-)reclaim-heap-on-failure"),
                    words("instead."), nl],
                add_error(phase_options, ReclaimPieces, !Specs)
            else
                true
            )
        )
    ).

%---------------------%

    % Options updated:
    %   use_minimal_model_stack_copy_cut
    %   use_minimal_model_stack_copy_pneg
    %
:- pred handle_minimal_model_options(globals::in, globals::out,
    bool::out, list(error_spec)::in, list(error_spec)::out) is det.

handle_minimal_model_options(!Globals, AllowHijacksMMSC, !Specs) :-
    globals.lookup_bool_option(!.Globals, use_minimal_model_stack_copy,
        UseMinimalModelStackCopy),
    globals.lookup_bool_option(!.Globals, use_minimal_model_own_stacks,
        UseMinimalModelOwnStacks),
    ( if
        UseMinimalModelStackCopy = bool.yes,
        UseMinimalModelOwnStacks = bool.yes
    then
        DualMMSpec =
            [words("You cannot use both forms of minimal model tabling"),
            words("at once."), nl],
        add_error(phase_options, DualMMSpec, !Specs)
    else
        true
    ),
    bool.or(UseMinimalModelStackCopy, UseMinimalModelOwnStacks,
        UseMinimalModel),
    (
        UseMinimalModel = bool.yes,
        % Minimal model tabling is not compatible with any of high level code,
        % trailing, and parallelism; see comments in runtime/mercury_grade.h.
        globals.lookup_bool_option(!.Globals, highlevel_code, HighLevelCode),
        (
            HighLevelCode = yes,
            MMHLSpec =
                [words("Minimal model tabling is incompatible with"),
                words("high level code."), nl],
            add_error(phase_options, MMHLSpec, !Specs)
        ;
            HighLevelCode = no
        ),
        globals.lookup_bool_option(!.Globals, use_trail, UseTrail),
        (
            UseTrail = yes,
            MMTrailSpec =
                [words("Minimal model tabling is incompatible with"),
                words("trailing."), nl],
            add_error(phase_options, MMTrailSpec, !Specs)
        ;
            UseTrail = no
        ),
        globals.lookup_bool_option(!.Globals, parallel, Parallel),
        (
            Parallel = yes,
            MMParSpec =
                [words("Minimal model tabling is incompatible with"),
                words("parallel execution."), nl],
            add_error(phase_options, MMParSpec, !Specs)
        ;
            Parallel = no
        )
    ;
        UseMinimalModel = no
    ),

    % Stack copy minimal model tabling needs to be able to rewrite all
    % the redoips in a given nondet stack segments. If we allow hijacks,
    % some of these redoips may have been saved in ordinary framevars,
    % which means that tabling can't find them without label layout info.
    % Since we want to allow tabling in grades that do not have label
    % layout info, we disable hijacks instead.
    % XXX we should allow hijacks in table_builtin.m
    (
        UseMinimalModelStackCopy = bool.yes,
        AllowHijacksMMSC = bool.no
    ;
        UseMinimalModelStackCopy = bool.no,
        AllowHijacksMMSC = bool.yes
    ),

    % Stack copy minimal model tabling needs to generate extra code
    % at possibly negated contexts to handle the pneg stack and at commits
    % to handle the cut stack. The code below allows the generation of
    % these extra pieces of code to be disabled. The disabled program will
    % work only if the program doesn't actually use minimal model tabling,
    % which makes it useful only for performance testing.
    globals.lookup_bool_option(!.Globals, disable_mmsc_pneg, DisablePneg),
    globals.lookup_bool_option(!.Globals, disable_mmsc_cut, DisableCut),
    ( if
        UseMinimalModelStackCopy = bool.yes,
        DisablePneg = bool.no
    then
        globals.set_option(use_mmsc_pneg, bool(yes), !Globals)
    else
        true
    ),
    ( if
        UseMinimalModelStackCopy = bool.yes,
        DisableCut = bool.no
    then
        globals.set_option(use_mmsc_cut, bool(yes), !Globals)
    else
        true
    ).

%---------------------%

:- type maybe_allow_src_changes
    --->    do_not_allow_src_changes
    ;       allow_src_changes.

    % Options updated:
    %   body_typeinfo_liveness
    %   exec_trace
    %   exec_trace_tail_rec
    %   link_ssdb_libs
    %   opt_no_return_calls
    %   trace_stack_layout
    %   trace_table_io
    %   trace_table_io_all
    %
:- pred handle_debugging_options(compilation_target::in, trace_level::in,
    maybe_exec_trace_enabled::in, ssdb_trace_level::in,
    maybe_allow_src_changes::out, globals::in, globals::out,
    list(error_spec)::in, list(error_spec)::out) is det.

handle_debugging_options(Target, TraceLevel, TraceEnabled, SSTraceLevel,
        !:AllowSrcChanges, !Globals, !Specs) :-
    % --decl-debug is an extension of --debug
    option_implies(decl_debug, exec_trace, bool(yes), !Globals),

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
    (
        TraceEnabled = exec_trace_is_enabled,
        globals.lookup_bool_option(!.Globals, highlevel_code, HighLevelCode),
        ( if HighLevelCode = bool.no, Target = target_c then
            true
        else
            TraceHLSpec =
                [words("Debugging is available only in low level C grades."),
                nl],
            add_error(phase_options, TraceHLSpec, !Specs)
        ),
        globals.lookup_bool_option(!.Globals, parallel, Parallel),
        (
            Parallel = no
        ;
            Parallel = yes,
            ParSpec =
                [words("Debugging is not available in parallel grades."), nl],
            add_error(phase_options, ParSpec, !Specs)
        ),

        globals.lookup_bool_option(!.Globals, trace_optimized, TraceOptimized),
        (
            TraceOptimized = bool.no,
            % The options controlled by AllowSrcChanges modify
            % the structure of the program, which makes it difficult
            % to relate the trace to the source code (although
            % it can be easily related to the transformed HLDS).
            !:AllowSrcChanges = do_not_allow_src_changes
        ;
            TraceOptimized = bool.yes,
            !:AllowSrcChanges = allow_src_changes
        ),

        % The following options cause the info required by tracing
        % to be generated.
        globals.set_option(trace_stack_layout, bool(yes), !Globals),
        globals.set_option(body_typeinfo_liveness, bool(yes), !Globals),

        % To support up-level printing, we need to save variables across
        % a call even if the call cannot succeed.
        globals.set_option(opt_no_return_calls, bool(no), !Globals),

        % The declarative debugger does not (yet) know about tail calls.
        AllowTraceTailRec = trace_level_allows_tail_rec(TraceLevel),
        (
            AllowTraceTailRec = bool.no,
            % XXX Any code that checks the exec_trace_tail_rec option
            % should instead check the effective trace level for whatever
            % procedure it wants to operate on. However, at the moment,
            % the only code that looks at this option is mark_tail_calls.m,
            % which we invoke only in MLDS grades, which do not implement
            % execution tracing. So this question is moot (for now).
            globals.set_option(exec_trace_tail_rec, bool(no), !Globals)
        ;
            AllowTraceTailRec = bool.yes
        )
    ;
        TraceEnabled = exec_trace_is_not_enabled,
        !:AllowSrcChanges = allow_src_changes,

        % Since there will be no call and exit events, there is no point
        % in trying to turn them into tailcall events.
        globals.set_option(exec_trace_tail_rec, bool(no), !Globals)
    ),

    % Source-to-source debugging requires disabling many HLDS->HLDS
    % optimizations. This is so that the trace being generated relates to the
    % source code and also because the SSDB transformation cannot (yet) handle
    % the specialised predicates introduced by many optimizations.
    (
        ( SSTraceLevel = ssdb_shallow
        ; SSTraceLevel = ssdb_deep
        ),
        !:AllowSrcChanges = do_not_allow_src_changes
    ;
        SSTraceLevel = ssdb_none
        % Leave AllowSrcChanges alone.
    ),

    % --ssdb implies --link-ssdb-libs
    option_implies(source_to_source_debug, link_ssdb_libs, bool(yes),
        !Globals).

%---------------------%

    % Options updated:
    %   event_set_file_name
    %
:- pred maybe_update_event_set_file_name(globals::in, globals::out,
    io::di, io::uo) is det.

maybe_update_event_set_file_name(!Globals, !IO) :-
    globals.lookup_string_option(!.Globals, event_set_file_name,
        EventSetFileName0),
    ( if EventSetFileName0 = "" then
        io.environment.get_environment_var("MERCURY_EVENT_SET_FILE_NAME",
            MaybeEventSetFileName, !IO),
        (
            MaybeEventSetFileName = maybe.yes(EventSetFileName),
            globals.set_option(event_set_file_name, string(EventSetFileName),
                !Globals)
        ;
            MaybeEventSetFileName = maybe.no
        )
    else
        true
    ).

%---------------------%

    % Options updated:
    %   coverage_profiling
    %   deep_profile_tail_recursion
    %   pre_prof_transforms_simplify
    %   prof_optimized
    %
:- pred handle_profiling_options(globals::in, globals::out,
    compilation_target::in, bool::in,maybe_allow_src_changes::out,
    int::in, int::out, list(error_spec)::in, list(error_spec)::out) is det.

handle_profiling_options(!Globals, Target, ProfileDeep, !:AllowSrcChangesProf,
        OT_HigherOrderSizeLimit0, OT_HigherOrderSizeLimit, !Specs) :-
    % Profile for feedback requires coverage profiling.
    option_implies(profile_for_feedback, coverage_profiling, bool(yes),
        !Globals),

    % At the moment, coverage profiling is not compatible with the tail
    % recursion preservation optimization used by the deep profiler.
    option_implies(coverage_profiling, deep_profile_tail_recursion,
        bool(no), !Globals),

    % Perform a simplification pass before the profiling passes if one of
    % these profiling options is enabled.
    option_implies(profile_deep, pre_prof_transforms_simplify, bool(yes),
        !Globals),

    option_implies(profile_deep, procid_stack_layout, bool(yes), !Globals),
    (
        ProfileDeep = bool.yes,
        globals.lookup_bool_option(!.Globals, highlevel_code, HighLevelCode),
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

        % Deep profiling disables the optimization for pretesting whether
        % x == y in runtime/mercury_unify_compare_body.h, so compensate by
        % doing the same test in the unify and compare predicates generated by
        % the Mercury compiler.
        globals.set_option(should_pretest_equality, bool(yes), !Globals),

        % Inlining happens before the deep profiling transformation, so if
        % we allowed inlining to happen, then we would lose all profiling
        % information about the inlined calls - this is not usually what we
        % want so we disable inlining with deep profiling by default.
        % The user can re-enable it with the `--profile-optimized' option.
        % Leave inlining enabled when profiling for implicit parallelism.
        option_implies(profile_for_feedback, prof_optimized, bool(yes),
            !Globals),
        globals.lookup_bool_option(!.Globals, prof_optimized, ProfOptimized),
        (
            ProfOptimized = bool.no,
            !:AllowSrcChangesProf = do_not_allow_src_changes
        ;
            ProfOptimized = bool.yes,
            !:AllowSrcChangesProf = allow_src_changes
        ),

        globals.lookup_bool_option(!.Globals,
            use_lots_of_ho_specialization, LotsOfHOSpec),
        (
            LotsOfHOSpec = bool.yes,
            % We used to switch on --optimize-higher-order here,
            % even if it was disabled e.g. for execution tracing.
            % That was almost certainly a bug.
            %
            % Setting the limit here is ok, since the limit is irrelevant
            % if the only optimization that looks at it is not run.
            OT_HigherOrderSizeLimit = 999999
        ;
            LotsOfHOSpec = bool.no,
            OT_HigherOrderSizeLimit = OT_HigherOrderSizeLimit0
        )
    ;
        ProfileDeep = bool.no,
        !:AllowSrcChangesProf = allow_src_changes,
        OT_HigherOrderSizeLimit = OT_HigherOrderSizeLimit0
    ),

    globals.lookup_string_option(!.Globals, experimental_complexity, ExpComp),
    ( if ExpComp = "" then
        true
    else
        !:AllowSrcChangesProf = do_not_allow_src_changes,
        globals.lookup_bool_option(!.Globals, record_term_sizes_as_words,
            RecordTermSizesAsWords),
        globals.lookup_bool_option(!.Globals, record_term_sizes_as_cells,
            RecordTermSizesAsCells),
        ( if
            ( RecordTermSizesAsWords = bool.yes
            ; RecordTermSizesAsCells = bool.yes
            )
        then
            true
        else
            ExpCompSpec =
                [words("The --experimental-complexity option"),
                words("requires a term size profiling grade."), nl],
            add_error(phase_options, ExpCompSpec, !Specs)
        )
    ).

%---------------------%

    % Options updated:
    %   allow_double_word_fields
    %   pre_prof_transforms_simplify
    %
:- pred handle_record_term_sizes_options(globals::in, globals::out,
    bool::out, list(error_spec)::in, list(error_spec)::out) is det.

handle_record_term_sizes_options(!Globals, AllowOptLCMCTermSize, !Specs) :-
    globals.lookup_bool_option(!.Globals, record_term_sizes_as_words,
        RecordTermSizesAsWords),
    globals.lookup_bool_option(!.Globals, record_term_sizes_as_cells,
        RecordTermSizesAsCells),
    ( if
        RecordTermSizesAsWords = bool.yes,
        RecordTermSizesAsCells = bool.yes
    then
        DualTermSizeSpec =
            [words("Cannot record term size as both words and cells."), nl],
        add_error(phase_options, DualTermSizeSpec, !Specs),
        AllowOptLCMCTermSize = bool.yes
    else if
        ( RecordTermSizesAsWords = bool.yes
        ; RecordTermSizesAsCells = bool.yes
        )
    then
        globals.set_option(pre_prof_transforms_simplify, bool(yes), !Globals),
        AllowOptLCMCTermSize = bool.no,
        % Term size profiling breaks the assumption that even word offsets from
        % the start of the cell are double-word aligned memory addresses.
        %
        % XXX Actually, we do not (or should not) make that assumption as it
        % would also be violated by memory attribution profiling which also
        % allocates an extra word at the start of a cell.
        globals.lookup_bool_option(!.Globals, highlevel_code, HighLevelCode),
        globals.set_option(allow_double_word_fields, bool(no), !Globals),
        (
            HighLevelCode = bool.yes,
            TermSizeHLSpec =
                [words("Term size profiling is incompatible with"),
                words("high level code."), nl],
            add_error(phase_options, TermSizeHLSpec, !Specs)
        ;
            HighLevelCode = bool.no
        )
    else
        AllowOptLCMCTermSize = bool.yes
    ).

%---------------------%

    % Options updated:
    %   basic_stack_layout
    %   procid_stack_layout
    %
:- pred handle_stack_layout_options(globals::in, globals::out,
    maybe_opt_dups::in, maybe_opt_dups::out,
    maybe_standardize_labels::in, maybe_standardize_labels::out) is det.

handle_stack_layout_options(!Globals, OT_OptDups0, OT_OptDups,
        OT_StdLabels0, OT_StdLabels) :-
    % --stack-trace requires `procid' stack layouts.
    option_implies(stack_trace, procid_stack_layout, bool(yes), !Globals),

    % `trace' stack layouts need `procid' stack layouts.
    option_implies(trace_stack_layout, procid_stack_layout, bool(yes),
        !Globals),

    % `procid' and `agc' stack layouts need `basic' stack layouts
    option_implies(procid_stack_layout, basic_stack_layout, bool(yes),
        !Globals),
    option_implies(agc_stack_layout, basic_stack_layout, bool(yes), !Globals),

    % dupelim.m doesn't preserve label layout structures (e.g. it can
    % change the return address in a call to a different label whose code
    % is the same but which has a different label layout structure),
    % so we need to disable it when tracing. We also need to disable it
    % when using accurate GC, for the same reason.
    globals.lookup_bool_option(!.Globals, procid_stack_layout,
        ProcIdStackLayout),
    globals.lookup_bool_option(!.Globals, agc_stack_layout, AgcStackLayout),
    ( if (ProcIdStackLayout = yes ; AgcStackLayout = yes) then
        OT_OptDups = do_not_opt_dups
    else
        OT_OptDups = OT_OptDups0
    ),

    % stdlabel.m tries to perform operations that yield compiler aborts
    % if any stack layout information is present in the generated code.
    globals.lookup_bool_option(!.Globals, basic_stack_layout,
        BasicStackLayout),
    (
        BasicStackLayout = yes,
        OT_StdLabels = do_not_standardize_labels
    ;
        BasicStackLayout = no,
        OT_StdLabels = OT_StdLabels0
    ).

%---------------------%

    % Options updated:
    %   generate_item_version_numbers
    %   line_numbers
    %   smart_recompilation
    %   transitive_optimization
    %   warn_wrong_module_name
    %   warn_unused_interface_imports
    %   inform_generated_type_spec_pragmas
    %   detect_stdlib_grades
    %
:- pred handle_op_mode_implications(op_mode::in,
    globals::in, globals::out) is det.

handle_op_mode_implications(OpMode, !Globals) :-
    % Disable `--smart-recompilation' unless we are generating target code.
    globals.lookup_bool_option(!.Globals, smart_recompilation, Smart0),
    globals.lookup_bool_option(!.Globals, inform_generated_type_spec_pragmas,
        Inform0),
    (
        OpMode = opm_top_args(OpModeArgs, _),
        % Disable --line-numbers when building the `.int', `.opt', etc. files,
        % since including line numbers in those would cause unnecessary
        % recompilation.
        (
            OpModeArgs = opma_make_interface(OpModeArgsMI),
            turn_off_all_only_codegen_warnings(halt_at_warn_make_int,
                !Globals),
            (
                ( OpModeArgsMI = omif_int0
                ; OpModeArgsMI = omif_int1_int2
                ),
                % NOTE When generating interface files, --smart-recompilation
                % calls not for *doing* smart recompilation, but *preparing*
                % for smart recompilation, in the form of generating item
                % version numbers.
                globals.set_option(generate_item_version_numbers,
                    bool(Smart0), !Globals)
            ;
                OpModeArgsMI = omif_int3,
                % We never use version number information in `.int3',
                % `.opt' or `.trans_opt' files.
                globals.set_option(generate_item_version_numbers,
                    bool(no), !Globals)
            ),
            Smart = bool.no,
            Inform = bool.no
        ;
            OpModeArgs = opma_augment(OpModeAugment),
            (
                OpModeAugment = opmau_make_plain_opt,
                turn_off_all_only_codegen_warnings(halt_at_warn_make_opt,
                    !Globals),
                Smart = bool.no,
                Inform = bool.no
            ;
                OpModeAugment = opmau_make_trans_opt,
                globals.set_option(transitive_optimization, bool(yes),
                    !Globals),
                turn_off_all_only_codegen_warnings(halt_at_warn_make_opt,
                    !Globals),
                Smart = bool.no,
                Inform = bool.no
            ;
                ( OpModeAugment = opmau_make_analysis_registry
                ; OpModeAugment = opmau_make_xml_documentation
                ; OpModeAugment = opmau_typecheck_only
                ),
                turn_off_all_only_codegen_warnings(halt_at_warn, !Globals),
                Smart = bool.no,
                Inform = bool.no
            ;
                OpModeAugment = opmau_front_and_middle(OpModeFrontAndMiddle),
                (
                    OpModeFrontAndMiddle = opfam_errorcheck_only,
                    % We execute all the tests in tests/warnings with
                    % --errorcheck-only.
                    Smart = no
                ;
                    ( OpModeFrontAndMiddle = opfam_target_code_only
                    ; OpModeFrontAndMiddle = opfam_target_and_object_code_only
                    ; OpModeFrontAndMiddle = opfam_target_object_and_executable
                    ),
                    Smart = Smart0
                ),
                Inform = Inform0
            )
        ;
            ( OpModeArgs = opma_generate_dependencies(_)
            ; OpModeArgs = opma_generate_dependency_file
            ; OpModeArgs = opma_convert_to_mercury
            ),
            turn_off_all_only_codegen_warnings(halt_at_warn, !Globals),
            Smart = bool.no,
            Inform = bool.no
        )
    ;
        OpMode = opm_top_generate_source_file_mapping,
        Smart = bool.no,
        Inform = bool.no
    ;
        OpMode = opm_top_query(OpModeQuery),
        (
            ( OpModeQuery = opmq_output_library_install_grades
            ; OpModeQuery = opmq_output_stdlib_grades
            ),
            globals.set_option(detect_stdlib_grades, bool(yes), !Globals)
        ;
            ( OpModeQuery = opmq_output_cc
            ; OpModeQuery = opmq_output_c_compiler_type
            ; OpModeQuery = opmq_output_cflags
            ; OpModeQuery = opmq_output_c_include_directory_flags
            ; OpModeQuery = opmq_output_grade_defines
            ; OpModeQuery = opmq_output_csharp_compiler
            ; OpModeQuery = opmq_output_csharp_compiler_type
            ; OpModeQuery = opmq_output_java_class_dir
            ; OpModeQuery = opmq_output_link_command
            ; OpModeQuery = opmq_output_shared_lib_link_command
            ; OpModeQuery = opmq_output_library_link_flags
            ; OpModeQuery = opmq_output_grade_string
            ; OpModeQuery = opmq_output_stdlib_modules
            ; OpModeQuery = opmq_output_target_arch
            ; OpModeQuery = opmq_output_optimization_options(_)
            )
        ),
        Smart = bool.no,
        Inform = bool.no
    ;
        ( OpMode = opm_top_generate_standalone_interface(_)
        ; OpMode = opm_top_make
        ),
        Smart = bool.no,
        Inform = bool.no
    ),
    % We do this here instead of replacing all the "Smart = bool.no"s above
    % with a call to globals.set_option, because this way, we would get an
    % error message from the compiler if we added a new op_mode and failed to
    % explicitly consider what the value of Smart should be in its switch arm.
    ( if Smart = Smart0 then
        true
    else
        globals.set_option(smart_recompilation, bool(Smart), !Globals)
    ),
    % That consideration applies here as well.
    ( if Inform = Inform0 then
        true
    else
        globals.set_option(inform_generated_type_spec_pragmas, bool(Inform),
            !Globals)
    ).

:- pred turn_off_all_only_codegen_warnings(option::in,
    globals::in, globals::out) is det.

turn_off_all_only_codegen_warnings(HaltAtWarnSrcOpt, !Globals) :-
    some [!OptionTable]
    (
        globals.get_options(!.Globals, !:OptionTable),

        map.det_update(line_numbers, bool(no), !OptionTable),

        map.lookup(!.OptionTable, HaltAtWarnSrcOpt, HaltAtWarn),
        map.set(halt_at_warn, HaltAtWarn, !OptionTable),

        set_all_options_to(dodgy_code_warning_bool_options, bool(no),
            !OptionTable),
        set_all_options_to(slow_code_warning_bool_options, bool(no),
            !OptionTable),
        set_all_options_to(style_warning_bool_options, bool(no),
            !OptionTable),
        set_all_options_to(info_request_bool_options, bool(no),
            !OptionTable),

        globals.set_options(!.OptionTable, !Globals)
    ).

%---------------------%

    % Options updated:
    %   mode_constraints
    %   optimize_trail_usage
    %   prop_mode_constraints
    %   stack_trace
    %   structure_sharing_analysis
    %   termination
    %   termination2
    %   termination2_check
    %   termination_check
    %   use_opt_files
    %   use_trail
    %   use_trans_opt_files
    %   verbose_recompilation
    %   warn_missing_trans_opt_files
    %   warn_unused_interface_imports
    %
:- pred handle_option_to_option_implications(op_mode::in,
    globals::in, globals::out) is det.

handle_option_to_option_implications(OpMode, !Globals) :-
    % --make handles creation of the module dependencies itself,
    % and they don't need to be recreated when compiling to C.
    ( if
        OpMode = opm_top_args(_, InvokedByMMCMake),
        InvokedByMMCMake = op_mode_invoked_by_mmc_make
    then
        globals.set_option(generate_mmc_make_module_dependencies, bool(no),
            !Globals)
    else
        true
    ),

    option_implies(find_all_recompilation_reasons, verbose_recompilation,
        bool(yes), !Globals),

    option_implies(debug_mode_constraints, prop_mode_constraints, bool(yes),
        !Globals),
    option_implies(prop_mode_constraints, mode_constraints, bool(yes),
        !Globals),
    option_implies(simple_mode_constraints, mode_constraints, bool(yes),
        !Globals),

    % We need to be able to simulate exits for calls between where an
    % exception is thrown to where it is caught both in the debugger and
    % for deep profiling.
    option_implies(exec_trace, stack_trace, bool(yes), !Globals),
    option_implies(profile_deep, stack_trace, bool(yes), !Globals),

    % The results of trail usage analysis assume that trail usage
    % optimization is being done, i.e. that redundant trailing
    % operations are really being eliminated.
    option_implies(analyse_trail_usage, optimize_trail_usage, bool(yes),
        !Globals),

    option_implies(structure_reuse_analysis, structure_sharing_analysis,
        bool(yes), !Globals),

    option_implies(termination_check_verbose, termination_check, bool(yes),
        !Globals),
    option_implies(termination2_check_verbose, termination2_check,
        bool(yes), !Globals),
    option_implies(termination_check, termination_enable,
        bool(yes), !Globals),
    option_implies(termination2_check, termination2_enable,
        bool(yes), !Globals),
    % Note that setting warn_missing_trans_opt_files to yes can be
    % overridden below.
    option_implies(termination_check, warn_missing_trans_opt_files,
        bool(yes), !Globals),
    option_implies(termination2_check, warn_missing_trans_opt_files,
        bool(yes), !Globals),

    % If we are doing full inter-module or transitive optimization,
    % we need to build all `.opt' or `.trans_opt' files.
    option_implies(transitive_optimization, intermodule_optimization,
        bool(yes), !Globals),
    option_implies(transitive_optimization, use_trans_opt_files, bool(no),
        !Globals),
    option_implies(intermodule_optimization, use_opt_files, bool(no),
        !Globals),
    option_implies(use_trans_opt_files, use_opt_files, bool(yes), !Globals),

    % XXX `--use-opt-files' is broken.
    % When inter-module optimization is enabled, error checking
    % without the extra information from the `.opt' files
    % is done when making the `.opt' file. With `--use-opt-files',
    % that doesn't happen.
    % XXX Should that be "with `--no-use-opt-files'"?
    globals.set_option(use_opt_files, bool(no), !Globals),

    globals.lookup_bool_option(!.Globals, warn_unused_imports, UnusedImports),
    (
        UnusedImports = no
    ;
        UnusedImports = yes,
        % warn_unused_interface_imports does *part* of the job
        % of warn_unused_imports.
        globals.set_option(warn_unused_interface_imports, bool(no), !Globals)
    ).

    % --use-opt-files implies --no-warn-missing-opt-files since
    % we are expecting some to be missing.
    % XXX This rule will never fire while we set use_opt_files to "no" above.
    % option_implies(use_opt_files, warn_missing_opt_files, bool(no),
    %     !Globals).

%---------------------%

    % Options updated:
    %   none
    %
:- pred maybe_disable_smart_recompilation(io.text_output_stream::in,
    op_mode::in, globals::in, globals::out, io::di, io::uo) is det.

maybe_disable_smart_recompilation(ProgressStream, OpMode, !Globals, !IO) :-
    % XXX Smart recompilation does not yet work with intermodule
    % optimization, but we still want to generate version numbers
    % in interface files for users of a library compiled with
    % intermodule optimization but not using intermodule
    % optimization themselves.
    globals.lookup_bool_option(!.Globals, smart_recompilation, Smart),
    (
        Smart = bool.no
    ;
        Smart = bool.yes,
        ( if
            globals.lookup_bool_option(!.Globals, intermodule_optimization,
                yes)
        then
            disable_smart_recompilation(ProgressStream,
                "`--intermodule-optimization'", !Globals, !IO)
        else
            true
        ),
        ( if globals.lookup_bool_option(!.Globals, use_opt_files, yes) then
            disable_smart_recompilation(ProgressStream,
                "`--use-opt-files'", !Globals, !IO)
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
            OpMode = opm_top_args(OpModeArgs, _),
            OpModeArgs = opma_augment(
                opmau_front_and_middle(opfam_target_code_only))
        then
            true
        else
            disable_smart_recompilation(ProgressStream,
                "`--no-target-code-only'", !Globals, !IO)
        )
    ).

%---------------------%

    % Options updated:
    %   chosen_stdlib_dir
    %
:- pred handle_chosen_stdlib_dir(maybe1(list(string))::in,
    globals::in, globals::out,
    list(error_spec)::in, list(error_spec)::out) is det.

handle_chosen_stdlib_dir(MaybeEnvOptFileMerStdLibDir, !Globals, !Specs) :-
    % Was the standard library directory set on the command line?
    globals.lookup_maybe_string_option(!.Globals,
        mercury_standard_library_directory, MaybeOptionsStdLibDir),
    (
        MaybeOptionsStdLibDir = yes(ChosenStdLibDir),
        MaybeChosenStdLibDir = yes(ChosenStdLibDir)
    ;
        MaybeOptionsStdLibDir = no,
        % Was the standard library directory set using the
        % MERCURY_STDLIB_DIR variable in either the environment
        % or in an options_file?
        (
            MaybeEnvOptFileMerStdLibDir = ok1(EnvOptFileMerStdLibDirs),
            (
                EnvOptFileMerStdLibDirs = [],
                MaybeChosenStdLibDir = no,
                Pieces = [words("Error: the location of the directory"),
                    words("that holds the Mercury standard library"),
                    words("is not specified"), nl,

                    words("either by an"), quote("--mercury-stdlib-dir"),
                        words("option,"), nl,
                    words("or by an environment variable named"),
                        quote("MERCURY_STDLIB_DIR"), suffix(","), nl,
                    words("or by a make variable named"),
                        quote("MERCURY_STDLIB_DIR"),
                        words("in any specified options file, such as"),
                        quote("Mercury.config"), suffix("."), nl],
                Spec = no_ctxt_spec($pred, severity_error,
                    phase_options, Pieces),
                !:Specs = [Spec | !.Specs]
            ;
                EnvOptFileMerStdLibDirs = [ChosenStdLibDir],
                MaybeChosenStdLibDir = yes(ChosenStdLibDir)
            ;
                EnvOptFileMerStdLibDirs = [_, _ | _],
                MaybeChosenStdLibDir = no,
                % Note: we can be more precise about where MERCURY_STDLIB_DIR
                % came from only if we start recording the origin points of
                % the entries in the env_optfile_variables structure.
                Pieces = [words("Error: the definition of the"),
                    quote("MERCURY_STDLIB_DIR"), words("variable,"),
                    words("either in the environment"),
                    words("or in a specified options file,"),
                    words("contains more than one string."), nl],
                Spec = no_ctxt_spec($pred, severity_error,
                    phase_options, Pieces),
                !:Specs = [Spec | !.Specs]
            )
        ;
            MaybeEnvOptFileMerStdLibDir = error1(EnvOptFileSpecs),
            MaybeChosenStdLibDir = no,
            !:Specs = EnvOptFileSpecs ++ !.Specs
        )
    ),
    globals.set_option(chosen_stdlib_dir, maybe_string(MaybeChosenStdLibDir),
        !Globals).

%---------------------%

    % Options updated:
    %   libgrades
    %
    % Other globals fields updated:
    %   maybe_stdlib_grades
    %
:- pred handle_libgrades(io.text_output_stream::in, globals::in, globals::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

handle_libgrades(ProgressStream, !Globals, !Specs, !IO) :-
    globals.get_maybe_stdlib_grades(!.Globals, MaybeStdLibGrades0),
    (
        MaybeStdLibGrades0 = stdlib_grades_known(StdLibGradeSet0),
        set.to_sorted_list(StdLibGradeSet0, StdLibGrades)
    ;
        MaybeStdLibGrades0 = stdlib_grades_unknown,
        globals.lookup_bool_option(!.Globals, detect_stdlib_grades, Detect),
        (
            Detect = yes,
            detect_stdlib_grades(ProgressStream, !.Globals,
                MaybeStdLibGradeSet, !IO),
            (
                MaybeStdLibGradeSet = ok1(StdLibGradeSet),
                MaybeStdLibGrades = stdlib_grades_known(StdLibGradeSet),
                globals.set_maybe_stdlib_grades(MaybeStdLibGrades, !Globals),
                set.to_sorted_list(StdLibGradeSet, StdLibGrades)
            ;
                MaybeStdLibGradeSet = error1(_Specs),
                StdLibGrades = []
            )
        ;
            Detect = no,
            StdLibGrades = []
        )
    ),

    globals.lookup_accumulating_option(!.Globals,
        library_install_grades, LibGrades0),
    ( if LibGrades0 = ["stdlib" | SpecifiedLibGrades] then
        LibGrades = StdLibGrades ++ SpecifiedLibGrades,
        globals.set_option(library_install_grades, accumulating(LibGrades),
            !Globals)
    else
        true
    ),
    % Handle the libgrades_{include,exclude}_components options,
    handle_libgrade_component_incl_excl(!Globals, !Specs).

%---------------------%

    % Options updated:
    %   use_subdirs
    %
    % Other globals fields updated:
    %   subdir_setting
    %
:- pred handle_subdir_setting(op_mode::in, globals::in, globals::out) is det.

handle_subdir_setting(OpMode, !Globals) :-
    globals.lookup_bool_option(!.Globals, setting_only_use_grade_subdirs,
        UseGradeSubdirs),
    (
        UseGradeSubdirs = yes,
        UseSubdirs = bool.yes,
        globals.set_option(setting_only_use_subdirs, bool(UseSubdirs),
            !Globals),
        SubdirSetting = use_cur_ngs_gs_subdir
    ;
        UseGradeSubdirs = no,
        % This is needed for library installation (the library grades
        % are built using `--use-grade-subdirs', and assume that
        % the interface files were built using `--use-subdirs').
        % XXX I (zs) don't understand the above comment (which I moved,
        % but did not write).
        ( if
            (
                OpMode = opm_top_make
            ;
                OpMode = opm_top_args(_, InvokedByMMCMake),
                InvokedByMMCMake = op_mode_invoked_by_mmc_make
            )
        then
            UseSubdirs = bool.yes,
            globals.set_option(setting_only_use_subdirs, bool(UseSubdirs),
                !Globals),
            SubdirSetting = use_cur_ngs_subdir
        else
            globals.lookup_bool_option(!.Globals, setting_only_use_subdirs,
                UseSubdirs),
            (
                UseSubdirs = yes,
                SubdirSetting = use_cur_ngs_subdir
            ;
                UseSubdirs = no,
                SubdirSetting = use_cur_dir
            )
        )
    ),
    globals.set_subdir_setting(SubdirSetting, !Globals).

%---------------------%

    % Options updated:
    %   c_include_directory
    %   default_runtime_library_directory
    %   init_file_directories
    %   intermod_directories
    %   libgrade_install_check
    %   link_library_directories
    %   use_subdirs
    %
    % Other globals fields updated:
    %   ext_dirs_maps
    %
:- pred handle_directory_options(op_mode::in,
    globals::in, globals::out) is det.

handle_directory_options(OpMode, !Globals) :-
    % We only perform the library grade install check if we are
    % building a linked target using mmc --make or if we are building
    % a single source file linked target. (The library grade install
    % check is *not* compatible with the use of mmake.)
    ( if
        (
            OpMode = opm_top_make
        ;
            OpMode = opm_top_args(OpModeArgs, _),
            OpModeArgs = opma_augment(opmau_front_and_middle(
                opfam_target_object_and_executable))
        )
    then
        true
    else
        globals.set_option(libgrade_install_check, bool(no), !Globals)
    ),

    globals.get_mercury_linkage(!.Globals, MercuryLinkage),
    (
        MercuryLinkage = sos_static,
        UseDefaultRuntimeLibraryDirs = bool.no,
        globals.set_option(use_default_runtime_library_directory, bool(no),
            !Globals)
    ;
        MercuryLinkage = sos_shared,
        globals.lookup_bool_option(!.Globals,
            use_default_runtime_library_directory,
            UseDefaultRuntimeLibraryDirs)
    ),

    % Add the standard library directory.
    %
    % XXX We should be looking up the value of the chosen_stdlib_dir option,
    % not the mercury_standard_library_directory option. However, doing that
    % results in the failure of the foreign_decl_line_number and gh72_errors
    % test cases in tests/invalid.
    %
    % The symptom of those failures is an error message about not being able
    % to access the nonexistent directory /usr/local/mercury-DEV/lib/mercury
    % to check the set of installed grades. (That directory is the default
    % install prefix set by the configure script.)
    %
    % The reason why we get *only* those two failures (in a C grade) is that
    %
    % - in the test directories that want to generate (and then test)
    %   executables, mmake invokes mmc only to generate target code,
    %   with the C compiler then being invoked by mmake itself,
    %   which makes mmc's op_mode opmau_generate_code(opmcg_target_code_only),
    %
    % - in the test directories that want to check error messages,
    %   by default we invoke mmc with --errorcheck-only,
    %   which makes mmc's op_mode opmau_errorcheck_only, and
    %
    % - both of those op_modes will cause the if-then-else at the top of
    %   this predicate to set libgrade_install_check to "no".
    %
    % The reason why foreign_decl_line_number and gh72_errors fail is that
    % they both have --no-errorcheck-only as a module-specific mmc flag,
    % which causes that same if-then-else to leave libgrade_install_check
    % with its default value of "yes".
    %
    % I (zs) see two ways to avoid this problem. One way would be to decouple
    % the notions of
    %
    % - the location of the standard library for libgrade_install_checks
    %   during bootchecks in a workspace, and
    %
    % - the location where a "mmake install" in that workspace
    %   would put the the standard library
    %
    % by specifying a real, existing stdlib location (probably the stdlib
    % of the installed compiler) during bootchecks.
    %
    % The other way would be to decide that we don't want to do
    % libgrade_install_checks during bootchecks at all, except possibly
    % for a few tests that specifically want to test the
    % libgrade_install_check code itself. We would then specify
    % --no-libgrade-install-check as the default for the tests directory.
    %
    globals.lookup_maybe_string_option(!.Globals,
        mercury_standard_library_directory, MaybeStdLibDir),
    (
        MaybeStdLibDir = yes(StdLibDir),
        MerStdLibDirs = [StdLibDir],
        globals.get_options(!.Globals, OptionTable2),
        option_table_add_mercury_library_directory(StdLibDir,
            OptionTable2, OptionTable),
        globals.set_options(OptionTable, !Globals),

        % Add `-L' and `-R' options for the location of the GC libraries.
        globals.lookup_accumulating_option(!.Globals,
            link_library_directories, LinkLibDirs0),
        % XXX LEGACY
        globals.set_option(link_library_directories,
            accumulating([StdLibDir / "lib" | LinkLibDirs0]), !Globals),

        (
            UseDefaultRuntimeLibraryDirs = bool.yes,
            globals.lookup_accumulating_option(!.Globals,
                runtime_link_library_directories, Rpath0),
            % XXX LEGACY
            globals.set_option(runtime_link_library_directories,
                accumulating([StdLibDir / "lib" | Rpath0]), !Globals)
        ;
            UseDefaultRuntimeLibraryDirs = bool.no
        )
    ;
        MaybeStdLibDir = no,
        MerStdLibDirs = [],
        globals.set_option(libgrade_install_check, bool(no), !Globals)
    ),

    % Add the path to mercury_conf.h.
    globals.lookup_maybe_string_option(!.Globals,
        mercury_configuration_directory, MaybeConfDir),
    (
        MaybeConfDir = yes(ConfDir),
        globals.lookup_accumulating_option(!.Globals, c_include_directories,
            CIncludeDirs0),
        % XXX LEGACY
        globals.set_option(c_include_directories,
            accumulating([ConfDir/"conf" | CIncludeDirs0]), !Globals)
    ;
        MaybeConfDir = no
    ),

    % Handle the `.opt', C header, init file and library search
    % directories for installed libraries. These couldn't be handled by
    % options.m because they are grade dependent.
    globals.lookup_accumulating_option(!.Globals,
        mercury_library_directories, MercuryLibDirs),
    globals.get_grade_dir(!.Globals, Grade),
    (
        MercuryLibDirs = [_ | _],
        ExtraLinkLibDirs = list.map(
            ( func(MercuryLibDir) =
                MercuryLibDir/"lib"/Grade
            ), MercuryLibDirs),

        globals.lookup_accumulating_option(!.Globals,
            link_library_directories, LinkLibDirs1),
        globals.set_option(link_library_directories,
            accumulating(LinkLibDirs1 ++ ExtraLinkLibDirs), !Globals),

        (
            UseDefaultRuntimeLibraryDirs = bool.yes,
            globals.lookup_accumulating_option(!.Globals,
                runtime_link_library_directories, Rpath),
            % XXX LEGACY
            globals.set_option(runtime_link_library_directories,
                accumulating(Rpath ++ ExtraLinkLibDirs), !Globals)
        ;
            UseDefaultRuntimeLibraryDirs = bool.no
        ),

        ExtraIncludeDirs = list.map(
            ( func(MercuryLibDir) =
                MercuryLibDir/"lib"/Grade/"inc"
            ), MercuryLibDirs),
        globals.lookup_accumulating_option(!.Globals, c_include_directories,
            CIncludeDirs),
        % XXX LEGACY
        globals.set_option(c_include_directories,
            accumulating(ExtraIncludeDirs ++ CIncludeDirs), !Globals),

        ExtraIntermodDirs = list.map(
            ( func(MercuryLibDir) =
                dir.make_path_name(MercuryLibDir,
                    dir.make_path_name("ints", Grade))
            ), MercuryLibDirs),
        globals.lookup_accumulating_option(!.Globals,
            intermod_directories, IntermodDirs0),
        globals.set_option(intermod_directories,
            accumulating(ExtraIntermodDirs ++ IntermodDirs0), !Globals),

        ExtraInitDirs = list.map(
            ( func(MercuryLibDir) =
                MercuryLibDir / "modules" / Grade
            ), MercuryLibDirs),

        globals.lookup_accumulating_option(!.Globals,
            init_file_directories, InitDirs1),
        % XXX LEGACY
        globals.set_option(init_file_directories,
            accumulating(InitDirs1 ++ ExtraInitDirs), !Globals)
    ;
        MercuryLibDirs = []
    ),

    % If --use-search-directories-for-intermod is true, append the
    % search directories to the list of directories to search for
    % .opt files.
    globals.lookup_bool_option(!.Globals,
        use_search_directories_for_intermod, UseSearchDirsForIntermod),
    (
        UseSearchDirsForIntermod = bool.yes,
        globals.lookup_accumulating_option(!.Globals,
            intermod_directories, IntermodDirs1),
        globals.lookup_accumulating_option(!.Globals,
            search_directories, SearchDirs),
        % XXX LEGACY
        globals.set_option(intermod_directories,
            accumulating(IntermodDirs1 ++ SearchDirs), !Globals)
    ;
        UseSearchDirsForIntermod = bool.no
    ),

    globals.lookup_accumulating_option(!.Globals,
        search_library_files_directories, SearchLibFilesDirs),
    globals.lookup_accumulating_option(!.Globals,
        intermod_directories, IntermodDirs2),
    globals.lookup_string_option(!.Globals, target_arch, TargetArch),
    ToGradeSubdir = (func(Dir) = Dir/"Mercury"/Grade/TargetArch),
    globals.get_subdir_setting(!.Globals, SubdirSetting),
    (
        SubdirSetting = use_cur_ngs_gs_subdir,
        % With `--use-grade-subdirs', `.opt', `.trans_opt' and
        % `.mih' files are placed in a directory named
        % `Mercury/<grade>/<target_arch>/Mercury/<ext>s'.
        % When searching for a `.opt' file, module_name_to_file_name
        % produces `Mercury/<ext>/<module>.ext' so that searches
        % for installed files work, so we need to add
        % `--intermod-directory Mercury/<grade>/<target_arch>'
        % to find the `.opt' files in the current directory.
        GradeSubdir = "Mercury"/Grade/TargetArch,

        % Directories listed with --search-library-files-directories need
        % to be treated in the same way as the current directory.
        SearchLibFilesGradeSubdirs = list.map(ToGradeSubdir,
            SearchLibFilesDirs),
        IntermodDirs3 = [GradeSubdir] ++ SearchLibFilesGradeSubdirs ++
            list.negated_filter(unify(dir.this_directory), IntermodDirs2)
    ;
        ( SubdirSetting = use_cur_ngs_subdir
        ; SubdirSetting = use_cur_dir
        ),
        IntermodDirs3 = SearchLibFilesDirs ++ IntermodDirs2
    ),
    % XXX LEGACY
    globals.set_option(intermod_directories,
        accumulating(IntermodDirs3), !Globals),

    globals.lookup_accumulating_option(!.Globals,
        link_library_directories, LinkLibDirs2),
    globals.lookup_accumulating_option(!.Globals,
        init_file_directories, InitDirs2),
    (
        SubdirSetting = use_cur_ngs_gs_subdir,
        % With --use-grade-subdirs we need to search in
        % `Mercury/<grade>/<target_arch>/Mercury/lib' for libraries and
        % `Mercury/<grade>/<target_arch>/Mercury/inits' for init files,
        % for each directory listed with --search-library-files-directory.
        ToGradeLibDir = (func(Dir) = ToGradeSubdir(Dir) / "Mercury" / "lib"),
        SearchGradeLibDirs = list.map(ToGradeLibDir, SearchLibFilesDirs),
        LinkLibDirs = SearchGradeLibDirs ++ LinkLibDirs2,

        ToGradeInitDir =
            (func(Dir) = ToGradeSubdir(Dir) / "Mercury" / "inits"),
        SearchGradeInitDirs = list.map(ToGradeInitDir, SearchLibFilesDirs),
        InitDirs = SearchGradeInitDirs ++ InitDirs2
    ;
        ( SubdirSetting = use_cur_ngs_subdir
        ; SubdirSetting = use_cur_dir
        ),
        LinkLibDirs = SearchLibFilesDirs ++ LinkLibDirs2,
        InitDirs = SearchLibFilesDirs ++ InitDirs2
    ),
    % XXX LEGACY
    globals.set_option(link_library_directories,
        accumulating(LinkLibDirs), !Globals),
    globals.set_option(init_file_directories,
        accumulating(InitDirs), !Globals),

    % When searching for a header (.mh, .mih) file,
    % module_name_to_file_name uses the plain header name, so we need to
    % add the full path to the header files in the current directory,
    % and any directories listed with --search-library-files-directory.
    ( if
        require_complete_switch [SubdirSetting]
        (
            SubdirSetting = use_cur_ngs_gs_subdir,
            ToMihsSubdir =
                (func(Dir) = ToGradeSubdir(Dir)/"Mercury"/"mihs")
        ;
            SubdirSetting = use_cur_ngs_subdir,
            ToMihsSubdir = (func(Dir) = Dir/"Mercury"/"mihs")
        ;
            SubdirSetting = use_cur_dir,
            fail
        )
    then
        ToMhsSubdir = (func(Dir) = Dir/"Mercury"/"mhs"),
        globals.lookup_accumulating_option(!.Globals, c_include_directories,
            CIncludeDirs1),
        MhsSubdir = ToMhsSubdir(dir.this_directory),
        MihsSubdir = ToMihsSubdir(dir.this_directory),
        SearchLibMhsSubdirs = list.map(ToMhsSubdir, SearchLibFilesDirs),
        SearchLibMihsSubdirs = list.map(ToMihsSubdir, SearchLibFilesDirs),
        SubdirCIncludeDirs = [dir.this_directory, MhsSubdir, MihsSubdir |
            SearchLibMhsSubdirs ++ SearchLibMihsSubdirs ++ CIncludeDirs1],
        % XXX LEGACY
        globals.set_option(c_include_directories,
            accumulating(SubdirCIncludeDirs), !Globals)
    else
        true
    ),

    globals.lookup_accumulating_option(!.Globals,
        interface_dirs_same_subdir_setting, InterfaceSame),
    globals.lookup_accumulating_option(!.Globals,
        interface_dirs_indep_subdir_setting, InterfaceIndep),
    globals.lookup_accumulating_option(!.Globals,
        interface_dirs_installed_library, InterfaceInstalled),

    globals.lookup_accumulating_option(!.Globals,
        intermod_dirs_same_subdir_setting, IntermodSame),
    globals.lookup_accumulating_option(!.Globals,
        intermod_dirs_indep_subdir_setting, IntermodIndep),
    globals.lookup_accumulating_option(!.Globals,
        intermod_dirs_installed_library, IntermodInstalled),

    globals.lookup_accumulating_option(!.Globals,
        c_incl_dirs_same_subdir_setting, CInclSame),
    globals.lookup_accumulating_option(!.Globals,
        c_incl_dirs_indep_subdir_setting, CInclIndep),
    globals.lookup_accumulating_option(!.Globals,
        c_incl_dirs_installed_library, CInclInstalled),
    globals.lookup_accumulating_option(!.Globals,
        c_incl_dirs_external, CInclExternal),

    globals.lookup_accumulating_option(!.Globals,
        mer_lib_dirs_same_subdir_setting, MerLibSame),
    globals.lookup_accumulating_option(!.Globals,
        mer_lib_dirs_indep_subdir_setting, MerLibIndep),
    globals.lookup_accumulating_option(!.Globals,
        mer_lib_dirs_installed_library, MerLibInstalled),

    some [!InterfaceDirsMap, !IntermodDirsMap, !CInclDirsMap,
        !LibDirsMap, !StdLibDirsMap]
    (
        map.init(!:InterfaceDirsMap),
        map.init(!:IntermodDirsMap),
        map.init(!:CInclDirsMap),
        map.init(!:LibDirsMap),
        map.init(!:StdLibDirsMap),

        ext_cur_ngs_extension_dir(ext_cur_ngs_int_int0, _, ExtDirInt0),
        ext_cur_ngs_extension_dir(ext_cur_ngs_int_int1, _, ExtDirInt1),
        ext_cur_ngs_extension_dir(ext_cur_ngs_int_int2, _, ExtDirInt2),
        ext_cur_ngs_extension_dir(ext_cur_ngs_int_int3, _, ExtDirInt3),
        ext_cur_ngs_extension_dir(
            ext_cur_ngs_misc_module_dep, _, ExtDirModuleDep),
        make_proposed_search_path_ngs(SubdirSetting, ExtDirInt0,
            InterfaceSame, InterfaceIndep, InterfaceInstalled, InterfaceInt0),
        make_proposed_search_path_ngs(SubdirSetting, ExtDirInt1,
            InterfaceSame, InterfaceIndep, InterfaceInstalled, InterfaceInt1),
        make_proposed_search_path_ngs(SubdirSetting, ExtDirInt2,
            InterfaceSame, InterfaceIndep, InterfaceInstalled, InterfaceInt2),
        make_proposed_search_path_ngs(SubdirSetting, ExtDirInt3,
            InterfaceSame, InterfaceIndep, InterfaceInstalled, InterfaceInt3),
        make_proposed_search_path_ngs(SubdirSetting, ExtDirModuleDep,
            InterfaceSame, InterfaceIndep, InterfaceInstalled,
            InterfaceModuleDep),
        map.det_insert(ife_int0, InterfaceInt0, !InterfaceDirsMap),
        map.det_insert(ife_int1, InterfaceInt1, !InterfaceDirsMap),
        map.det_insert(ife_int2, InterfaceInt2, !InterfaceDirsMap),
        map.det_insert(ife_int3, InterfaceInt3, !InterfaceDirsMap),
        map.det_insert(ife_module_dep, InterfaceModuleDep, !InterfaceDirsMap),
        % XXX We should not look for .m files in installed libraries.
        InterfaceSrc = InterfaceSame ++ InterfaceIndep ++ InterfaceInstalled,
        map.det_insert(ife_src, InterfaceSrc, !InterfaceDirsMap),

        ext_cur_ngs_gs_extension_dir(
            ext_cur_ngs_gs_proposed_opt_plain, _, ExtDirPlainOpt),
        ext_cur_ngs_gs_extension_dir(
            ext_cur_ngs_gs_proposed_opt_trans, _, ExtDirTransOpt),
        ext_cur_ngs_gs_extension_dir(
            ext_cur_ngs_gs_an_analysis_date, _, ExtDirDate),
        ext_cur_ngs_gs_extension_dir(
            ext_cur_ngs_gs_an_analysis_status, _, ExtDirStatus),
        ext_cur_ngs_gs_max_ngs_extension_dir(
            ext_cur_ngs_gs_max_ngs_an_analysis, _, ExtDirAnalysis),
        ext_cur_ngs_gs_max_ngs_extension_dir(
            ext_cur_ngs_gs_max_ngs_an_imdg, _, ExtDirImdg),
        ext_cur_ngs_gs_max_ngs_extension_dir(
            ext_cur_ngs_gs_max_ngs_an_request, _, ExtDirRequest),
        make_proposed_search_path_gs(SubdirSetting, Grade, ExtDirPlainOpt,
            IntermodSame, IntermodIndep, IntermodInstalled, IntermodPlainOpt),
        make_proposed_search_path_gs(SubdirSetting, Grade, ExtDirTransOpt,
            IntermodSame, IntermodIndep, IntermodInstalled, IntermodTransOpt),
        make_proposed_search_path_gs(SubdirSetting, Grade, ExtDirDate,
            IntermodSame, IntermodIndep, IntermodInstalled, IntermodDate),
        make_proposed_search_path_gs(SubdirSetting, Grade, ExtDirStatus,
            IntermodSame, IntermodIndep, IntermodInstalled, IntermodStatus),
        make_proposed_search_path_gs(SubdirSetting, Grade, ExtDirAnalysis,
            IntermodSame, IntermodIndep, IntermodInstalled, IntermodAnalysis),
        make_proposed_search_path_gs(SubdirSetting, Grade, ExtDirImdg,
            IntermodSame, IntermodIndep, IntermodInstalled, IntermodImdg),
        make_proposed_search_path_gs(SubdirSetting, Grade, ExtDirRequest,
            IntermodSame, IntermodIndep, IntermodInstalled, IntermodRequest),
        map.det_insert(ime_opt_plain, IntermodPlainOpt, !IntermodDirsMap),
        map.det_insert(ime_opt_trans, IntermodTransOpt, !IntermodDirsMap),
        map.det_insert(ime_an_analysis_date, IntermodDate, !IntermodDirsMap),
        map.det_insert(ime_an_analysis_status, IntermodStatus,
            !IntermodDirsMap),
        map.det_insert(ime_an_analysis, IntermodAnalysis, !IntermodDirsMap),
        map.det_insert(ime_an_imdg, IntermodImdg, !IntermodDirsMap),
        map.det_insert(ime_an_request, IntermodRequest, !IntermodDirsMap),
        % XXX We should not look for .m files in installed libraries.
        IntermodSrc = IntermodSame ++ IntermodIndep ++ IntermodInstalled,
        map.det_insert(ime_src, IntermodSrc, !IntermodDirsMap),

        ext_cur_pgs_max_cur_extension_dir(
            ext_cur_pgs_max_cur_mh, _, ExtDirMh),
        ext_cur_ngs_gs_max_cur_extension_dir(
            ext_cur_ngs_gs_max_cur_mih, _, ExtDirMih),
        make_proposed_search_path_ngs(SubdirSetting, ExtDirMh,
            CInclSame, CInclIndep, CInclInstalled, CInclMh),
        make_proposed_search_path_gs(SubdirSetting, Grade, ExtDirMih,
            CInclSame, CInclIndep, CInclInstalled, CInclMih),
        map.det_insert(cie_mh, CInclMh, !CInclDirsMap),
        map.det_insert(cie_mih, CInclMih, !CInclDirsMap),
        CInclH0 = CInclMh ++ CInclMih ++ CInclExternal,
        list.remove_dups(CInclH0, CInclH),
        map.det_insert(cie_h, CInclH, !CInclDirsMap),

        ext_cur_gas_extension_dir(!.Globals, ext_cur_gas_lib_lib_opt, _,
            ExtDirA),
        make_proposed_search_path_gas(SubdirSetting, Grade, TargetArch,
            ExtDirA, MerLibSame, MerLibIndep, MerLibInstalled, LibA),
        map.det_insert(le_a, LibA, !LibDirsMap),

        ext_cur_gs_extension_dir(ext_cur_gs_lib_init, _, _, ExtDirInit),
        ext_cur_gs_extension_dir(ext_cur_gs_lib_jar, _, _, ExtDirJar),
        ext_cur_gs_extension_dir(ext_cur_gs_lib_cil_dll, _, _, ExtDirDll),
        make_proposed_search_path_gs(SubdirSetting, Grade, ExtDirInit,
            [], [], MerStdLibDirs, LibInit),
        make_proposed_search_path_gs(SubdirSetting, Grade, ExtDirJar,
            [], [], MerStdLibDirs, LibJar),
        make_proposed_search_path_gs(SubdirSetting, Grade, ExtDirDll,
            [], [], MerStdLibDirs, LibDll),
        map.det_insert(sle_init, LibInit, !StdLibDirsMap),
        map.det_insert(sle_jar, LibJar, !StdLibDirsMap),
        map.det_insert(sle_dll, LibDll, !StdLibDirsMap),

        ExtDirsMaps = ext_dirs_maps(!.InterfaceDirsMap, !.IntermodDirsMap,
            !.CInclDirsMap, !.LibDirsMap, !.StdLibDirsMap)
    ),
    globals.set_ext_dirs_maps(ExtDirsMaps, !Globals).

%---------------------%

:- pred make_proposed_search_path_gas(subdir_setting::in, dir_name::in,
    dir_name::in, dir_name::in,
    list(dir_name)::in, list(dir_name)::in, list(dir_name)::in,
    list(dir_name)::out) is det.

make_proposed_search_path_gas(SubdirSetting, Grade, TargetArch, ExtSubDir,
        SearchDirsSame, SearchDirsIndep, SearchDirsInstall, Dirs) :-
    % First, we search SearchDirsSame, which should be the workspace
    % directories that are guaranteed to share the same subdir_setting
    % as the current workspace directory. In each of these directories,
    % we use SubdirSetting to select the one right subdir.
    list.map(
        make_selected_proposed_dir_name_gas(SubdirSetting, Grade,
            TargetArch, ExtSubDir),
        SearchDirsSame, DirsSame),
    % Second, we search SearchDirsIndep, which should be the workspace
    % directories whose subdir_setting is independent of the subdir_setting
    % of this workspace directory. In these places, we have to search all three
    % of the subdirs where that independent subdir_setting can put the values
    % of this extension, searching them in the order grade-specific,
    % non-grade-specific, and then the current directory ("current" in this
    % case meaning one of the directories in SearchDirsIndep).
    list.map(
        make_all_proposed_dir_names_gas(Grade, TargetArch, ExtSubDir),
        SearchDirsIndep, DirsListIndep),
    % Third, we search SearchDirsInstall, which should be a list of library
    % install directories. In install directories, we look only in the
    % grade-specific subdir.
    list.map(
        make_selected_proposed_dir_name_gas(use_cur_ngs_gs_subdir, Grade,
            TargetArch, ExtSubDir),
        SearchDirsInstall, DirsInstall),
    list.condense(DirsListIndep, DirsIndep),
    Dirs = DirsSame ++ DirsIndep ++ DirsInstall.

:- pred make_proposed_search_path_gs(subdir_setting::in, dir_name::in,
    dir_name::in,
    list(dir_name)::in, list(dir_name)::in, list(dir_name)::in,
    list(dir_name)::out) is det.

make_proposed_search_path_gs(SubdirSetting, Grade, ExtSubDir,
        SearchDirsSame, SearchDirsIndep, SearchDirsInstall, Dirs) :-
    % We follow the search principles as make_proposed_search_path_gas above,
    % with the exception that the grade-specific directories are
    % not architecture-specific.
    list.map(
        make_selected_proposed_dir_name_gs(SubdirSetting, Grade, ExtSubDir),
        SearchDirsSame, DirsSame),
    list.map(
        make_all_proposed_dir_names_gs(Grade, ExtSubDir),
        SearchDirsIndep, DirsListIndep),
    list.map(
        make_selected_proposed_dir_name_gs(use_cur_ngs_gs_subdir, Grade,
            ExtSubDir),
        SearchDirsInstall, DirsInstall),
    list.condense(DirsListIndep, DirsIndep),
    Dirs = DirsSame ++ DirsIndep ++ DirsInstall.

:- pred make_proposed_search_path_ngs(subdir_setting::in, dir_name::in,
    list(dir_name)::in, list(dir_name)::in, list(dir_name)::in,
    list(dir_name)::out) is det.

make_proposed_search_path_ngs(SubdirSetting, ExtSubDir,
        SearchDirsSame, SearchDirsIndep, SearchDirsInstall, Dirs) :-
    % We follow the search principles as make_proposed_search_path_gas above,
    % with the difference that we never search any grade-specific directories,
    % since the files we are searching for are not grade specific.
    list.map(
        make_selected_proposed_dir_name_ngs(SubdirSetting, ExtSubDir),
        SearchDirsSame, DirsSame),
    list.map(
        make_all_proposed_dir_names_ngs(ExtSubDir),
        SearchDirsIndep, DirsListIndep),
    % Note that whether we pass use_cur_ngs_gs_subdir or use_cur_ngs_subdir
    % here does not matter; we will get the same result either way.
    list.map(
        make_selected_proposed_dir_name_ngs(use_cur_ngs_gs_subdir, ExtSubDir),
        SearchDirsInstall, DirsInstall),
    list.condense(DirsListIndep, DirsIndep),
    Dirs = DirsSame ++ DirsIndep ++ DirsInstall.

%---------------------%

    % Options updated:
    %   strip
    %   use_symlinks
    %
:- pred handle_target_compile_link_symlink_options(globals::in, globals::out)
    is det.

handle_target_compile_link_symlink_options(!Globals) :-
    option_implies(target_debug, strip, bool(no), !Globals),

    ( if io.file.have_symlinks then
        true
    else
        globals.set_option(use_symlinks, bool(no), !Globals)
    ).

%---------------------%

    % Options updated:
    %   auto_comments
    %   debug_modes
    %   debug_opt
    %   dump_hlds_options
    %   dump_hlds_options
    %   statistics
    %   trad_passes
    %   unneeded_code_debug
    %   verbose
    %   verbose_commands
    %   very_verbose
    %
:- pred handle_compiler_developer_options(globals::in, globals::out,
    io::di, io::uo) is det.

handle_compiler_developer_options(!Globals, !IO) :-
    option_implies(very_verbose, verbose, bool(yes), !Globals),
    option_implies(verbose, verbose_commands, bool(yes), !Globals),
    globals.lookup_bool_option(!.Globals, very_verbose, VeryVerbose),
    globals.lookup_bool_option(!.Globals, statistics, Statistics),
    ( if
        VeryVerbose = bool.yes,
        Statistics = bool.yes
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
        debug_unneeded_code_pred_name, DebugUnneededCodePredNames),
    (
        DebugUnneededCodePredNames = []
    ;
        DebugUnneededCodePredNames = [_ | _],
        globals.set_option(debug_unneeded_code, bool(yes), !Globals)
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
    (
        DebugIntermoduleAnalysis = no,
        set_analysis_debug_stream(maybe.no, !IO)
    ;
        DebugIntermoduleAnalysis = yes,
        % The next person to work actively on the analysis framework
        % can decide whether writing the framework's debug progress messages
        % to stderr is the right call, and if not, where those messages
        % should go.
        io.stderr_stream(AnalysisDebugStream, !IO),
        set_analysis_debug_stream(yes(AnalysisDebugStream), !IO)
    ),

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

    option_implies(frameopt_comments, auto_comments, bool(yes), !Globals),

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
        ; Statistics = bool.yes
        ; ParallelLiveness = bool.yes
        ; ParallelCodeGen = bool.yes
        )
    then
        globals.set_option(trad_passes, bool(no), !Globals)
    else
        true
    ).

%---------------------%

    % Options updated:
    %   compare_specialization
    %
:- pred handle_compare_specialization(globals::in, globals::out) is det.

handle_compare_specialization(!Globals) :-
    globals.lookup_int_option(!.Globals, compare_specialization, CompareSpec),
    ( if CompareSpec < 0 then
        % This indicates that the option was not set by the user;
        % we should set the option to the default value. This value
        % may be back end specific, since different back ends have
        % different performance tradeoffs.
        %
        % XXX Now that the quadratic code scheme for comparison predicates
        % has been made cheaper, these limits should almost certainly be
        % raised, but the *level* to which they should be raised requires
        % nontrivial benchmarking.
        globals.lookup_bool_option(!.Globals, highlevel_code, HighLevelCode),
        (
            HighLevelCode = bool.no,
            Limit0 = 13
        ;
            HighLevelCode = bool.yes,
            Limit0 = 14
        ),
        % The old constraint-based mode analysis code has not been updated
        % to handle switches in its input goals, so if it is enabled,
        % set compare_specialization to a level that suppresses the creation
        % of any switches.
        globals.lookup_bool_option(!.Globals, mode_constraints,
            ModeConstraints),
        (
            ModeConstraints = bool.no,
            Limit = Limit0
        ;
            ModeConstraints = bool.yes,
            Limit = 1
        ),
        globals.set_option(compare_specialization, int(Limit), !Globals)
    else
        true
    ).

%---------------------%

    % Options updated:
    %   use_color_diagnostics
    %
:- pred handle_colors(globals::in, globals::out) is det.

handle_colors(!Globals) :-
    % NOTE This predicate does not yet handle the issue of whether
    % the output is going to a tty or not. If and when it does want to do so,
    % it will first have to *figure out* where the output is going.
    % Given that
    %
    % - mmc --make first puts all diagnostics in .err files, but then
    % - copies the first N lines of those diagnostics to stderr,
    %
    % do we want to enable colors only in the first N lines of diagnostics?
    globals.lookup_bool_option(!.Globals,
        color_diagnostics_is_set, EnableIsSet),
    globals.lookup_bool_option(!.Globals,
        color_diagnostics_is_set_to, EnableValue),
    globals.lookup_bool_option(!.Globals,
        config_default_color_diagnostics, ConfigDefault),
    (
        EnableIsSet = yes,
        % If the user set the enable option, obey its value.
        UseColor = EnableValue
    ;
        EnableIsSet = no,
        % If the user dod not set the enable option, use the default.
        UseColor = ConfigDefault
    ),
    globals.set_option(use_color_diagnostics, bool(UseColor), !Globals).

%---------------------%

    % Options updated:
    %   none
    %
:- pred handle_non_tail_rec_warnings(globals::in, opt_tuple::in,
    maybe_opt_mlds_tailcalls::in,
    list(error_spec)::in, list(error_spec)::out) is det.

handle_non_tail_rec_warnings(Globals, OptTuple0, OT_OptMLDSTailCalls,
        !Specs) :-
    % --warn-non-tail-recursion requires tail call optimization to be enabled.
    % It also doesn't work if you use --errorcheck-only.
    globals.lookup_bool_option(Globals, warn_non_tail_recursion_self,
        WarnNonTailRecSelf),
    globals.lookup_bool_option(Globals, warn_non_tail_recursion_mutual,
        WarnNonTailRecMutual),
    ( if
        ( WarnNonTailRecSelf = bool.yes
        ; WarnNonTailRecMutual = bool.yes
        )
    then
        OT_PessimizeTailCalls0 = OptTuple0 ^ ot_pessimize_tailcalls,
        (
            OT_PessimizeTailCalls0 = do_not_pessimize_tailcalls
        ;
            OT_PessimizeTailCalls0 = pessimize_tailcalls,
            PessimizeWords = "--warn-non-tail-recursion is incompatible" ++
                 " with --pessimize-tailcalls",
            % XXX While these two options look diametrically opposed,
            % they are actually compatible, because pessimize_tailcalls
            % is implemented only for the LLDS backend, while the
            % optimize_tailcalls option is only for the MLDS backend.
            % (The LLDS backend's tail call optimization does NOT depend
            % on the value of that option.)
            add_error(phase_options, [words(PessimizeWords)], !Specs)
        ),
        (
            OT_OptMLDSTailCalls = opt_mlds_tailcalls
        ;
            OT_OptMLDSTailCalls = do_not_opt_mlds_tailcalls,
            % XXX This error message could be misleading. It is possible that
            % - the user *did* ask for MLDS tailcalls, but
            % - accidentally also specified --no-optimize, which would
            %   lead to code above turning MLDS tailcalls *off*.
            OptimizeWords =
                "--warn-non-tail-recursion requires --optimize-tailcalls",
            add_error(phase_options, [words(OptimizeWords)], !Specs)
        )
    else
        true
    ).

%---------------------%

    % Options updated:
    %   none
    %
:- pred handle_const_struct(compilation_target::in, op_mode::in,
    trace_level::in, trace_suppress_items::in,
    maybe_enable_const_struct_poly::in, maybe_enable_const_struct_poly::out,
    maybe_enable_const_struct_user::in, maybe_enable_const_struct_user::out)
    is det.

handle_const_struct(Target, OpMode, TraceLevel, TraceSuppress,
        OT_EnableConstStructPoly0, OT_EnableConstStructPoly,
        OT_EnableConstStructUser0, OT_EnableConstStructUser) :-
    (
        Target = target_c,
        OT_EnableConstStructPoly = OT_EnableConstStructPoly0,
        NeedProcBodies = trace_needs_proc_body_reps(TraceLevel, TraceSuppress),
        ( if
            (
                % We generate representations of procedure bodies for the
                % declarative debugger and for the profiler. When
                % traverse_primitives in browser/declarative_tree.m looks for
                % the Nth argument of variable X and X is built with code
                % such as X = ground_term_const(...), it crashes. It should be
                % taught not to do that, but in the meantime, we prevent the
                % situation from arising in the first place. (We never look
                % for the original sources of type infos and typeclass infos,
                % so we can use constant structures for them.)
                NeedProcBodies = yes
            ;
                % If we allowed the use of references to the const_struct_db,
                %
                % - those references would be dangling references
                %   if they were ever written to a .opt file, and
                %
                % - they would also confuse the termination analyzers,
                %   since they were written before the const_struct_db
                %   was implemented, and they have (yet) not been taught
                %   about it.
                OpMode = opm_top_args(opma_augment(Augment), _),
                ( Augment = opmau_make_plain_opt
                ; Augment = opmau_make_trans_opt
                )
            ;
                % If we are not allowed to use const structs for the
                % type_infos and typeclass_infos created by the polymorphism
                % pass, then we may not use them for user terms either.
                OT_EnableConstStructPoly0 = do_not_enable_const_struct_poly
            )
        then
            OT_EnableConstStructUser = do_not_enable_const_struct_user
        else
            OT_EnableConstStructUser = OT_EnableConstStructUser0
        )
    ;
        Target = target_java,
        OT_EnableConstStructPoly = OT_EnableConstStructPoly0,
        OT_EnableConstStructUser = do_not_enable_const_struct_user
    ;
        Target = target_csharp,
        OT_EnableConstStructPoly = do_not_enable_const_struct_poly,
        OT_EnableConstStructUser = do_not_enable_const_struct_user
    ).

%---------------------------------------------------------------------------%

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
:- pred postprocess_options_lowlevel(globals::in, globals::out,
    opt_tuple::in, opt_tuple::out) is det.

postprocess_options_lowlevel(!Globals, !OptTuple) :-
    % --optimize-saved-vars-cell requires --use-local-vars for
    % acceptable performance.
    SavedVarsCell = !.OptTuple ^ ot_opt_svcell,
    (
        SavedVarsCell = opt_svcell,
        !OptTuple ^ ot_use_local_vars := use_local_vars
    ;
        SavedVarsCell = do_not_opt_svcell
    ),

    % --optimize-frames requires --optimize-labels and
    % --optimize-jumps
    OptFrames = !.OptTuple ^ ot_opt_frames,
    (
        OptFrames = opt_frames,
        !OptTuple ^ ot_opt_labels := opt_labels,
        !OptTuple ^ ot_opt_jumps := opt_jumps
    ;
        OptFrames = do_not_opt_frames
    ),

    % --optimize-proc-dups is implemented only with --trad-passes.
    OptProcDups = !.OptTuple ^ ot_opt_proc_dups,
    (
        OptProcDups = opt_proc_dups,
        globals.set_option(trad_passes, bool(yes), !Globals)
    ;
        OptProcDups = do_not_opt_proc_dups
    ),

    UseLocalVars = !.OptTuple ^ ot_use_local_vars,
    OptRepeat = !.OptTuple ^ ot_opt_repeat,
    ( if
        ( OptFrames = opt_frames
        ; UseLocalVars = use_local_vars
        ),
        OptRepeat < 1
    then
        % The frame optimization and the local vars optimization depend on
        % the jump and label optimization having been done. They are turned
        % on above, but they still won't be executed unless optimize_repeat
        % is at least one.
        !OptTuple ^ ot_opt_repeat := 1
    else
        true
    ),

    % The setting of static_ground_floats is governed only by the settings
    % of unboxed_float and static_ground_cells.
    globals.lookup_bool_option(!.Globals, unboxed_float, UnboxedFloat),
    (
        UnboxedFloat = yes,
        % If we are using unboxed (MR_Word-sized) floats, floating point values
        % are always constants.
        !OptTuple ^ ot_use_static_ground_floats := use_static_ground_floats
    ;
        UnboxedFloat = no,
        % If we are using boxed floats, then we can generate a static constant
        % variable to hold a float constant, and gcc doesn't mind us converting
        % from its address to MR_Word in a static initializer. In theory,
        % we should do this with --static-ground-terms. However, the code
        % generator does not yet handle the dynamic creation of boxed float
        % constants, and assumes that binding a variable to a constant
        % generates no code.
        !OptTuple ^ ot_use_static_ground_floats := use_static_ground_floats
    ),

    % Ditto for 64-bit integers.
    globals.lookup_bool_option(!.Globals, unboxed_int64s, UnboxedInt64s),
    (
        UnboxedInt64s = yes,
        !OptTuple ^ ot_use_static_ground_int64s := use_static_ground_int64s
    ;
        UnboxedInt64s = no,
        !OptTuple ^ ot_use_static_ground_int64s := use_static_ground_int64s
    ),

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
        !OptTuple ^ ot_use_static_code_addresses
            := do_not_use_static_code_addresses
    else
        !OptTuple ^ ot_use_static_code_addresses := use_static_code_addresses
    ).

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
:- pragma consider_used(pred(option_neg_implies/5)).

option_neg_implies(SourceOption, ImpliedOption, ImpliedOptionValue,
        !Globals) :-
    globals.lookup_bool_option(!.Globals, SourceOption, SourceOptionValue),
    (
        SourceOptionValue = yes
    ;
        SourceOptionValue = no,
        globals.set_option(ImpliedOption, ImpliedOptionValue, !Globals)
    ).

:- pred disable_smart_recompilation(io.text_output_stream::in, string::in,
    globals::in, globals::out, io::di, io::uo) is det.

disable_smart_recompilation(ProgressStream, OptionDescr, !Globals, !IO) :-
    io_set_disable_smart_recompilation(disable_smart_recompilation, !IO),
    globals.set_option(smart_recompilation, bool(no), !Globals),
    globals.lookup_bool_option(!.Globals, warn_smart_recompilation, WarnSmart),
    (
        WarnSmart = yes,
        % Disabling smart recompilation is not a module-specific thing,
        % so we cannot direct the error message to a module-specific file.
        io.format(ProgressStream,
            "Warning: smart recompilation does not yet work with %s.\n",
            [s(OptionDescr)], !IO),
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

usage_errors(ProgressStream, Globals, Specs, !IO) :-
    io.progname_base("mercury_compile", ProgName, !IO),
    io.format(ProgressStream, "%s:\n", [s(ProgName)], !IO),
    write_error_specs(ProgressStream, Globals, Specs, !IO).

%---------------------------------------------------------------------------%
:- end_module libs.handle_options.
%---------------------------------------------------------------------------%
