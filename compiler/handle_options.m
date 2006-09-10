%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
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
%-----------------------------------------------------------------------------%

:- module libs.handle_options.
:- interface.

:- import_module libs.globals.
:- import_module libs.options.

:- import_module bool.
:- import_module getopt_io.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % handle_options(Args, Errors, OptionArgs, NonOptionArgs, Link).
    %
:- pred handle_options(list(string)::in, list(string)::out, list(string)::out,
    list(string)::out, bool::out, io::di, io::uo) is det.

    % process_options(Args, OptionArgs, NonOptionArgs, MaybeOptionTable).
    %
    % Process the options, but don't do any post-processing or
    % modify the globals. This is mainly useful for separating
    % the list of arguments into option and non-option arguments.
    %
:- pred process_options(list(string)::in, list(string)::out, list(string)::out,
    maybe_option_table(option)::out, io::di, io::uo) is det.

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
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_io_util.

:- import_module char.
:- import_module dir.
:- import_module int.
:- import_module library.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module std_util.
:- import_module string.
:- import_module svmap.

%-----------------------------------------------------------------------------%

handle_options(Args0, Errors, OptionArgs, Args, Link, !IO) :-
    % io.write_string("original arguments\n", !IO),
    % dump_arguments(Args0, !IO),
    process_options(Args0, OptionArgs, Args, Result, !IO),
    % io.write_string("final arguments\n", !IO),
    % dump_arguments(Args, !IO),
    postprocess_options(Result, Errors, !IO),
    (
        Errors = [_ | _],
        Link = no
    ;
        Errors = [],
        globals.io_lookup_bool_option(generate_dependencies,
            GenerateDependencies, !IO),
        globals.io_lookup_bool_option(generate_dependency_file,
            GenerateDependencyFile, !IO),
        globals.io_lookup_bool_option(make_interface, MakeInterface, !IO),
        globals.io_lookup_bool_option(make_private_interface,
            MakePrivateInterface, !IO),
        globals.io_lookup_bool_option(make_short_interface,
            MakeShortInterface, !IO),
        globals.io_lookup_bool_option(make_optimization_interface,
            MakeOptimizationInt, !IO),
        globals.io_lookup_bool_option(make_transitive_opt_interface,
            MakeTransOptInt, !IO),
        globals.io_lookup_bool_option(make_analysis_registry,
            MakeAnalysisRegistry, !IO),
        globals.io_lookup_bool_option(convert_to_mercury,
            ConvertToMercury, !IO),
        globals.io_lookup_bool_option(typecheck_only, TypecheckOnly, !IO),
        globals.io_lookup_bool_option(errorcheck_only, ErrorcheckOnly, !IO),
        globals.io_lookup_bool_option(target_code_only,
            TargetCodeOnly, !IO),
        globals.io_get_target(Target, !IO),
        GenerateIL = (if Target = target_il then yes else no),
        globals.io_lookup_bool_option(compile_only, CompileOnly, !IO),
        bool.or_list([GenerateDependencies, GenerateDependencyFile,
            MakeInterface, MakePrivateInterface, MakeShortInterface,
            MakeOptimizationInt, MakeTransOptInt, MakeAnalysisRegistry,
            ConvertToMercury, TypecheckOnly, ErrorcheckOnly, TargetCodeOnly,
            GenerateIL, CompileOnly],
            NotLink),
        bool.not(NotLink, Link),
        globals.io_lookup_bool_option(smart_recompilation, Smart, !IO),
        (
            Smart = yes,
            Link = yes
        ->
            % XXX Currently smart recompilation doesn't check that all the
            % files needed to link are present and up-to-date, so disable it.
            globals.io_get_globals(Globals0, !IO),
            disable_smart_recompilation("linking", Globals0, Globals1, !IO),
            unsafe_promise_unique(Globals1, Globals),
            globals.io_set_globals(Globals, !IO)
        ;
            true
        )
    ).

process_options(Args0, OptionArgs, Args, Result, !IO) :-
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
:- pred postprocess_options(maybe_option_table(option)::in,
    list(string)::out, io::di, io::uo) is det.

postprocess_options(error(ErrorMessage), [ErrorMessage], !IO).
postprocess_options(ok(OptionTable0), Errors, !IO) :-
    check_option_values(OptionTable0, OptionTable, Target, GC_Method,
        TagsMethod, TermNorm, Term2Norm, TraceLevel, TraceSuppress,
        MaybeThreadSafe, [], CheckErrors),
    (
        CheckErrors = [],
        postprocess_options_2(OptionTable, Target, GC_Method,
            TagsMethod, TermNorm, Term2Norm, TraceLevel,
            TraceSuppress, MaybeThreadSafe, [], Errors, !IO)
    ;
        CheckErrors = [_ | _],
        Errors = CheckErrors
    ).

:- pred check_option_values(option_table::in, option_table::out,
    compilation_target::out, gc_method::out, tags_method::out,
    termination_norm::out, termination_norm::out, trace_level::out,
    trace_suppress_items::out, may_be_thread_safe::out,
    list(string)::in, list(string)::out) is det.

check_option_values(!OptionTable, Target, GC_Method, TagsMethod,
        TermNorm, Term2Norm, TraceLevel, TraceSuppress, MaybeThreadSafe,
        !Errors) :-
    map.lookup(!.OptionTable, target, Target0),
    (
        Target0 = string(TargetStr),
        convert_target(TargetStr, TargetPrime)
    ->
        Target = TargetPrime
    ;
        Target = target_c,     % dummy
        add_error("Invalid target option " ++
            "(must be `c', `asm', `il', or `java')", !Errors)
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
            "`conservative', `boehm', `mps', `accurate', or `automatic')",
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
        convert_dump_alias(DumpAlias, DumpOptions)
    ->
        svmap.set(dump_hlds_options, string(DumpOptions), !OptionTable)
    ;
        add_error("Invalid argument to option `--hlds-dump-alias'.", !Errors)
    ).

:- pred add_error(string::in, list(string)::in, list(string)::out) is det.

add_error(Error, Errors0, Errors) :-
    % We won't be appending enough errors for the quadratic complexity
    % of repeated appends to be a problem.
    list.append(Errors0, [Error], Errors).

    % NOTE: each termination analyser has its own norm setting.
    %
:- pred postprocess_options_2(option_table::in, compilation_target::in,
    gc_method::in, tags_method::in, termination_norm::in,
    termination_norm::in, trace_level::in, trace_suppress_items::in,
    may_be_thread_safe::in, list(string)::in, list(string)::out,
    io::di, io::uo) is det.

postprocess_options_2(OptionTable0, Target, GC_Method, TagsMethod0,
        TermNorm, Term2Norm, TraceLevel, TraceSuppress, MaybeThreadSafe,
        !Errors, !IO) :-
    unsafe_promise_unique(OptionTable0, OptionTable1), % XXX
    globals_io_init(OptionTable1, Target, GC_Method, TagsMethod0,
        TermNorm, Term2Norm, TraceLevel, TraceSuppress, MaybeThreadSafe, !IO),

    some [!Globals] (
        globals.io_get_globals(!:Globals, !IO),

        % Conservative GC implies --no-reclaim-heap-*
        ( gc_is_conservative(GC_Method) = yes ->
            globals.set_option(reclaim_heap_on_semidet_failure, bool(no),
                !Globals),
            globals.set_option(reclaim_heap_on_nondet_failure, bool(no),
                !Globals)
        ;
            true
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

        % if --tags low but --num-tag-bits not specified,
        % use the autoconf-determined value for --num-tag-bits
        % (the autoconf-determined value is passed from the `mc' script
        % using the undocumented --conf-low-tag-bits option)
        (
            TagsMethod0 = tags_low,
            NumTagBits0 = -1
        ->
            globals.lookup_int_option(!.Globals, conf_low_tag_bits,
                NumTagBits1)
        ;
            NumTagBits1 = NumTagBits0
        ),

        % if --num-tag-bits negative or unspecified, issue a warning
        % and assume --num-tag-bits 0
        ( NumTagBits1 < 0 ->
            io.progname_base("mercury_compile", ProgName, !IO),
            report_warning(ProgName, !IO),
            report_warning(
                ": warning: --num-tag-bits invalid or unspecified\n", !IO),
            io.write_string(ProgName, !IO),
            report_warning(": using --num-tag-bits 0 (tags disabled)\n", !IO),
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

        globals.lookup_bool_option(!.Globals, highlevel_data, HighLevelData),
        globals.lookup_bool_option(!.Globals,
            automatic_intermodule_optimization, AutoIntermodOptimization),

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
        %   - intermodule optimization
        %     This is only required for high-level data and is needed
        %     so that abstract equivalence types can be expanded.  They
        %     need to be expanded because .NET requires that the structural
        %     representation of a type is known at all times.
        %   - dead procedure optimization
        %         intermodule optimization pulls in a lot of code which isn't
        %         needed, so ensure that this dead code is removed.

        ( Target = target_il ->
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
            globals.set_option(static_ground_terms, bool(no), !Globals),

            (
                HighLevelData = yes,
                AutoIntermodOptimization = yes
            ->
                globals.set_option(intermodule_optimization, bool(yes),
                    !Globals),
                globals.set_option(optimize_dead_procs, bool(yes), !Globals)
            ;
                true
            ),

            % On the .NET backend we will be using a language independent
            % debugger not mdb.  Thus --debug has to imply --target-debug.
            ( given_trace_level_is_none(TraceLevel) = no ->
                globals.set_option(target_debug, bool(yes), !Globals)
            ;
                true
            )
        ;
            true
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
        %   - no static ground terms
        %         XXX Previously static ground terms used to not work with
        %             --high-level-data. But this has been (mostly?) fixed now.
        %             So we should investigate re-enabling static ground terms.
        %   - intermodule optimization
        %     This is only required for high-level data and is needed
        %     so that abstract equivalence types can be expanded.  They
        %     need to be expanded because Java requires that the structural
        %     representation of a type is known at all times.
        %   - dead procedure optimization
        %         intermodule optimization pulls in a lot of code which isn't
        %         needed, so ensure that this dead code is removed.

        ( Target = target_java ->
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
            globals.set_option(det_copy_out, bool(yes), !Globals),
            globals.set_option(num_tag_bits, int(0), !Globals),
            globals.set_option(unboxed_no_tag_types, bool(no), !Globals),
            globals.set_option(static_ground_terms, bool(no), !Globals),
            globals.set_option(put_nondet_env_on_heap, bool(yes), !Globals),

            (
                AutoIntermodOptimization = yes,
                globals.set_option(intermodule_optimization, bool(yes),
                    !Globals),
                globals.set_option(optimize_dead_procs, bool(yes), !Globals)
            ;
                AutoIntermodOptimization = no
            )
        ;
            true
        ),
        % Generating assembler via the gcc back-end requires
        % using high-level code.
        ( Target = target_asm ->
            globals.set_option(highlevel_code, bool(yes), !Globals)
        ;
            true
        ),

        % Generating high-level C or asm code requires putting each commit
        % in its own function, to avoid problems with setjmp() and
        % non-volatile local variables.
        (
            ( Target = target_c
            ; Target = target_asm
            )
        ->
            option_implies(highlevel_code, put_commit_in_own_func, bool(yes),
                !Globals)
        ;
            true
        ),

        %
        % Set up options for position independent code.
        %

        % Shared libraries always use `--linkage shared'.
        option_implies(compile_to_shared_lib, pic, bool(yes), !Globals),
        option_implies(compile_to_shared_lib, linkage, string("shared"),
            !Globals),
        option_implies(compile_to_shared_lib, mercury_linkage,
            string("shared"), !Globals),

        % On x86, using PIC takes a register away from us.
        option_implies(pic, pic_reg, bool(yes), !Globals),

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

        option_implies(structure_reuse_analysis, structure_sharing_analysis,
            bool(yes), !Globals),
        option_implies(verbose_check_termination, check_termination,bool(yes),
            !Globals),
        option_implies(check_termination, termination, bool(yes), !Globals),
        option_implies(check_termination, warn_missing_trans_opt_files,
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
        %
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
                % Programmers only enable --debug-liveness if they are
                % interested in the goal annotations put on goals by
                % the various phases of the liveness pass. The default
                % dump options do not print these annotations.
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

        globals.lookup_int_option(!.Globals, debug_opt_pred_id,
            DebugOptPredId),
        globals.lookup_string_option(!.Globals, debug_opt_pred_name,
            DebugOptPredName),
        (
            ( DebugOptPredId > 0
            ; DebugOptPredName \= ""
            )
        ->
            globals.set_option(debug_opt, bool(yes), !Globals)
        ;
            true
        ),

        globals.lookup_bool_option(!.Globals, debug_intermodule_analysis,
            DebugIntermoduleAnalysis),
        analysis.enable_debug_messages(DebugIntermoduleAnalysis, !IO),

        globals.lookup_int_option(!.Globals, dump_hlds_pred_id,
            DumpHLDSPredId),
        ( DumpHLDSPredId >= 0 ->
            globals.lookup_string_option(!.Globals, dump_hlds_options,
                DumpOptions2),
            % Prevent the dumping of the mode and type tables.
            string.replace_all(DumpOptions2, "M", "", DumpOptions3),
            string.replace_all(DumpOptions3, "T", "", DumpOptions),
            globals.set_option(dump_hlds_options, string(DumpOptions),
                !Globals)
        ;
            true
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
        globals.lookup_bool_option(!.Globals, highlevel_code, HighLevel),
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
            HighLevel = yes
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

        option_implies(highlevel_code, mutable_always_boxed, bool(no),
            !Globals),

        option_implies(target_debug, strip, bool(no), !Globals),

        % Inlining happens before the deep profiling transformation, so if
        % we allowed inlining to happen, then we would lose all profiling
        % information about the inlined calls - this is not usually what we
        % want so we disable inlining with deep profiling by default.  The
        % user can re-enable it with the `--profile-optimized' option.
        % 
        globals.lookup_bool_option(!.Globals, prof_optimized, ProfOptimized),
        (
            ProfOptimized = yes
        ;
            ProfOptimized = no,
            option_implies(profile_deep, allow_inlining, bool(no), !Globals)
        ),

        globals.lookup_string_option(!.Globals, experimental_complexity,
            ExpComp),
        ( ExpComp = "" ->
            true
        ;
            globals.set_option(allow_inlining, bool(no), !Globals)
        ),

        % --decl-debug is an extension of --debug
        option_implies(decl_debug, exec_trace, bool(yes), !Globals),

        % We need to be able to simulate exits for calls between where an
        % exception is thrown to where it is caught both in the debugger and
        % for deep profiling.
        option_implies(exec_trace, stack_trace, bool(yes), !Globals),
        option_implies(profile_deep, stack_trace, bool(yes), !Globals),

        % The `.debug' grade implies --use-trail in most cases. The reason
        % for the implication is to avoid unnecessary proliferation in
        % the number of different grades.  If you're using --debug,
        % you've already taken a major performance hit, so you should
        % be able to afford the minor performance hit caused by --use-trail.
        %
        % There are two exceptions. First, --use-minimal-model doesn't work
        % with trails. Second, the only difference between debug and decldebug
        % is the latter's support for declarative debugging, which inherently
        % requires retries in the debugger. These retries don't reset the
        % trail unless the code that creates the trail entries has prepared for
        % retries, which usually isn't the case. In any case, the space
        % overhead of decldebug grades is high enough that we don't want the
        % space overhead of trailing (mostly for extra code) as well unless the
        % user has explicitly requested it.

        globals.lookup_bool_option(!.Globals, exec_trace, ExecTrace),
        globals.lookup_bool_option(!.Globals, decl_debug, DeclDebug),
        (
            ExecTrace = yes,
            DeclDebug = no,
            UseMinimalModel = no
        ->
            globals.set_option(use_trail, bool(yes), !Globals)
        ;
            true
        ),

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
        globals.lookup_bool_option(!.Globals, trace_optimized,
            TraceOptimized),
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
                ( Target = target_il ->
                    globals.set_option(optimize_peep, bool(no), !Globals)
                ;
                    true
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
            globals.set_option(opt_no_return_calls, bool(no), !Globals)
        ;
            true
        ),

        option_implies(profile_deep, procid_stack_layout, bool(yes), !Globals),
        globals.lookup_bool_option(!.Globals, profile_deep, ProfileDeep),
        (
            ProfileDeep = yes,
            (
                HighLevel = no,
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
            (
                HighLevel = yes,
                add_error("term size profiling is incompatible "
                    ++ "with high level code", !Errors)
            ;
                HighLevel = no
            )
        ;
            true
        ),

        (
            ( given_trace_level_is_none(TraceLevel) = yes
            ; HighLevel = no, Target = target_c
            ; Target = target_il
            )
        ->
            true
        ;
            add_error("debugging is available only in " ++
                "low level C grades", !Errors)
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
        % XXX for the MLDS back-end,
        % accurate GC currently also requires disabling the higher-order
        % specialization pass, since that pass creates procedures
        % which don't respect left-to-right scoping of type_info parameters,
        % i.e. in which a parameter X may have a type whose type_info var
        % (in the type_info_varmap) occurs to the right of X in the
        % procedure's parameter list.
        %
        % XXX we also disable type specialization.
        % This is needed because type specialization may create
        % type class constraints of the form `c(t(T))'
        % (e.g. `enum(var(T))'' in library/sparse_bitset.m),
        % which the current RTTI system can't handle.
        %
        ( GC_Method = gc_accurate ->
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

            option_implies(highlevel_code, optimize_higher_order,
                bool(no), !Globals),

            globals.set_option(type_specialization, bool(no), !Globals),
            globals.set_option(user_guided_type_specialization,
                bool(no), !Globals)
        ;
            true
        ),

        % ml_gen_params_base and ml_declare_env_ptr_arg, in ml_code_util.m,
        % both assume (for accurate GC) that continuation environments
        % are always allocated on the stack, which means that things won't
        % if --gc accurate and --put-nondet-env-on-heap are both enabled.
        globals.lookup_bool_option(!.Globals, put_nondet_env_on_heap,
            PutNondetEnvOnHeap),
        (
            HighLevel = yes,
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
            disable_minimal_model_stack_copy_cut,
            DisableCut),
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
        globals.lookup_accumulating_option(!.Globals, dump_hlds, DumpStages),
        globals.lookup_bool_option(!.Globals, parallel_liveness,
            ParallelLiveness),
        globals.lookup_bool_option(!.Globals, parallel_code_gen,
            ParallelCodeGen),
        (
            ( DumpStages = [_ | _]
            ; Statistics = yes
            ; ParallelLiveness = yes
            ; ParallelCodeGen = yes
            )
        ->
            globals.set_option(trad_passes, bool(no), !Globals)
        ;
            true
        ),

        % If we are doing type-specialization, we may as well take
        % advantage of the declarations supplied by the programmer.
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
        % optimization is being done, i.e that redundant trailing operations
        % are really being eliminated.
        option_implies(analyse_trail_usage, optimize_trail_usage,
            bool(yes), !Globals),

        % The information needed for generating the module ordering
        % is only available while generating the dependencies.
        option_implies(generate_module_order, generate_dependencies,
            bool(yes), !Globals),

        % We only generate the source file mapping if the module name
        % doesn't match the file name.
        option_implies(generate_source_file_mapping, warn_wrong_module_name,
            bool(no), !Globals),

        globals.lookup_string_option(!.Globals, fullarch, FullArch),

        %
        % Add the standard library directory.
        %
        globals.lookup_maybe_string_option(!.Globals,
            mercury_standard_library_directory, MaybeStdLibDir),
        (
            MaybeStdLibDir = yes(StdLibDir),
            globals.get_options(!.Globals, OptionTable2),
            globals.set_options(option_table_add_mercury_library_directory(
                OptionTable2, StdLibDir), !Globals),

            %
            % Add `-L' and `-R' options for the location
            % of the GC libraries.
            %
            globals.lookup_accumulating_option(!.Globals,
                link_library_directories, LinkLibDirs0),
            globals.set_option(link_library_directories,
                accumulating([StdLibDir/"lib" | LinkLibDirs0]),
                !Globals),

            globals.lookup_accumulating_option(!.Globals,
                runtime_link_library_directories, Rpath0),
            globals.set_option(runtime_link_library_directories,
                accumulating([StdLibDir/"lib" | Rpath0]), !Globals)

        ;
            MaybeStdLibDir = no
        ),

        %
        % Add the path to mercury_conf.h.
        %
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

        %
        % Find the configuration file.
        %
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

        %
        % Handle the `.opt', C header and library search directories.
        % These couldn't be handled by options.m because they are grade
        % dependent.
        %
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
                link_library_directories, LinkLibDirs),
            globals.set_option(link_library_directories,
                accumulating(LinkLibDirs ++ ExtraLinkLibDirs), !Globals),

            globals.lookup_accumulating_option(!.Globals,
                runtime_link_library_directories, Rpath),
            globals.set_option(runtime_link_library_directories,
                accumulating(Rpath ++ ExtraLinkLibDirs), !Globals),

            ExtraCIncludeDirs = list.map(
                (func(MercuryLibDir) =
                    MercuryLibDir/"lib"/GradeString/"inc"
                ), MercuryLibDirs),
            globals.lookup_accumulating_option(!.Globals, c_include_directory,
                CIncludeDirs),
            globals.set_option(c_include_directory,
                accumulating(ExtraCIncludeDirs ++ CIncludeDirs), !Globals),

            ExtraIntermodDirs = list.map(
                (func(MercuryLibDir) =
                    dir.make_path_name(MercuryLibDir,
                        dir.make_path_name("ints", GradeString))
                ), MercuryLibDirs),
            globals.lookup_accumulating_option(!.Globals,
                intermod_directories, IntermodDirs0),
            globals.set_option(intermod_directories,
                accumulating(ExtraIntermodDirs ++ IntermodDirs0), !Globals)
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
        (
            UseGradeSubdirs = yes,
            %
            % With `--use-grade-subdirs', `.opt', `.trans_opt' and
            % `.mih' files are placed in a directory named
            % `Mercury/<grade>/<fullarch>/Mercury/<ext>s'.
            % When searching for a `.opt' file, module_name_to_file_name
            % produces `Mercury/<ext>/<module>.ext' so that searches
            % for installed files work, so we need to add
            % `--intermod-directory Mercury/<grade>/<fullarch>'
            % to find the `.opt' files in the current directory.
            %
            globals.lookup_accumulating_option(!.Globals,
                intermod_directories, IntermodDirs2),
            GradeSubdirIntermodDirs =
                ["Mercury"/GradeString/FullArch |
                list.filter(isnt(unify(dir.this_directory)), IntermodDirs2)],
            globals.set_option(intermod_directories,
                accumulating(GradeSubdirIntermodDirs), !Globals)
        ;
            UseGradeSubdirs = no
        ),

        %
        % When searching for a header (.mh or .mih) file,
        % module_name_to_file_name uses the plain header name, so we need to
        % add the full path to the header files in the current directory.
        %
        globals.lookup_bool_option(!.Globals, use_subdirs, UseSubdirs),
        (
            ( UseGradeSubdirs = yes ->
                MihsSubdir = "Mercury"/GradeString/FullArch/"Mercury"/"mihs"
            ; UseSubdirs = yes ->
                MihsSubdir = "Mercury"/"mihs"
            ;
                fail
            )
        ->
            globals.lookup_accumulating_option(!.Globals, c_include_directory,
                CIncludeDirs1),
            SubdirCIncludeDirs =
                [dir.this_directory, MihsSubdir | CIncludeDirs1],
            globals.set_option(c_include_directory,
                accumulating(SubdirCIncludeDirs), !Globals)
        ;
            true
        ),

        % --use-opt-files implies --no-warn-missing-opt-files since
        % we are expecting some to be missing.
        option_implies(use_opt_files, warn_missing_opt_files, bool(no),
            !Globals),

        % --warn-non-tail-recursion requires both --high-level-code
        % and --optimize-tailcalls.  It also doesn't work if you use
        % --errorcheck-only.
        option_requires(warn_non_tail_recursion, highlevel_code, bool(yes),
            "--warn-non-tail-recursion requires --high-level-code",
            !.Globals, !Errors),
        option_requires(warn_non_tail_recursion, optimize_tailcalls, bool(yes),
            "--warn-non-tail-recursion requires --optimize-tailcalls",
            !.Globals, !Errors),
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
            BackendForeignLanguages = ["il", "csharp", "mc++"],
            set_option(optimize_constructor_last_call, bool(no), !Globals)
        ;
            Target = target_asm,
            % XXX This is wrong!  It should be asm.
            BackendForeignLanguages = ["c"]
        ;
            Target = target_java,
            BackendForeignLanguages = ["java"],
            set_option(optimize_constructor_last_call, bool(no), !Globals)
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
                HighLevel = no,
                globals.set_option(compare_specialization, int(13), !Globals)
            ;
                HighLevel = yes,
                globals.set_option(compare_specialization, int(14), !Globals)
            )
        ;
            true
        ),

        (
            % In the non-C backends, it may not be possible to cast a value
            % of a non-enum du type to an integer.
            ( Target = target_c
            ; Target = target_asm
            ),

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
            HighLevel = no,
            postprocess_options_lowlevel(!Globals)
        ;
            HighLevel = yes
        ),

        unsafe_promise_unique(!Globals),
        globals.io_set_globals(!.Globals, !IO)
    ).

    % These option implications only affect the low-level (LLDS) code
    % generator.  They may in fact be harmful if set for the high-level
    % code generator, because sometimes the same option has different
    % meanings and implications in the two backends.
    %
:- pred postprocess_options_lowlevel(globals::in, globals::out) is det.

postprocess_options_lowlevel(!Globals) :-
        % The low level code generator assumes that const(_) rvals are
        % really constant, and that create(_) rvals with constant
        % arguments can be materialized in an assignable rval without
        % further code. For float_consts, the former is true only if
        % either static_ground_terms or unboxed_floats is true, and
        % the latter cannot be true without static_ground_terms.
    option_neg_implies(highlevel_code, static_ground_terms, bool(yes),
        !Globals),

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
    globals.set_option(smart_recompilation, bool(no), !Globals),
    globals.lookup_bool_option(!.Globals, warn_smart_recompilation,
        WarnSmart),
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
    library.version(Version),
    io.write_strings([
        "Mercury Compiler, version ", Version, "\n",
        "Copyright (C) 1993-2006 The University of Melbourne\n"
    ], !IO).

usage(!IO) :-
    % usage is called from many places; ensure that we don't print the
    % duplicate copies of the message.
    globals.io_printing_usage(AlreadyPrinted, !IO),
    (
        AlreadyPrinted = no,
        display_compiler_version(!IO),
        io.write_strings([
            "Usage: mmc [<options>] <arguments>\n",
            "Use `mmc --help' for more information.\n"
        ], !IO)
    ;
        AlreadyPrinted = yes
    ).

long_usage(!IO) :-
    % long_usage is called from only one place, so can't print duplicate
    % copies of the long usage message. We can print both a short and along
    % usage message, but there is no simple way to avoid that.
    library.version(Version),
    io.write_strings(["Mercury Compiler, version ", Version, "\n"], !IO),
    io.write_string("Copyright (C) 1993-2006 " ++
        "The University of Melbourne\n", !IO),
    io.write_string("Usage: mmc [<options>] <arguments>\n", !IO),
    io.write_string("Arguments:\n", !IO),
    io.write_string("\tArguments ending in `.m' " ++
        "are assumed to be source file names.\n", !IO),
    io.write_string("\tArguments that do not end in `.m' " ++
        "are assumed to be module names.\n", !IO),
    io.write_string("Options:\n", !IO),
    options_help(!IO).

%-----------------------------------------------------------------------------%

    % IMPORTANT: any changes here may require similar changes to
    %   runtime/mercury_grade.h
    %   scripts/parse_grade_options.sh-subr
    %   scripts/canonical_grade.sh-subr
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
    % The ordering of the components here is the same as the order
    % used in scripts/ml.in, and any change here will require a
    % corresponding change there. The only place where the ordering
    % actually matters is for constructing the pathname for the
    % grade of the library, etc for linking (and installation).
:- type grade_component
    --->    comp_gcc_ext        % gcc extensions etc. -- see
                                % grade_component_table
    ;       comp_par            % parallelism / multithreading
    ;       comp_gc             % the kind of GC to use
    ;       comp_prof           % what profiling options to use
    ;       comp_term_size      % whether or not to record term sizes
    ;       comp_trail          % whether or not to use trailing
    ;       comp_tag            % whether or not to reserve a tag
    ;       comp_minimal_model  % whether we set up for minimal model tabling
    ;       comp_pic            % Do we need to reserve a register for
                                % PIC (position independent code)?
    ;       comp_trace          % tracing/debugging options
    ;       comp_stack_extend.  % automatic stack extension

convert_grade_option(GradeString, Options0, Options) :-
    reset_grade_options(Options0, Options1),
    split_grade_string(GradeString, Components),
    set.init(NoComps),
    list.foldl2((pred(CompStr::in, Opts0::in, Opts::out,
            CompSet0::in, CompSet::out) is semidet :-
        grade_component_table(CompStr, Comp, CompOpts, MaybeTargets,
            _),
            % Check that the component isn't mentioned
            % more than once.
        \+ set.member(Comp, CompSet0),
        set.insert(CompSet0, Comp, CompSet),
        add_option_list(CompOpts, Opts0, Opts1),

            % XXX Here the behaviour matches what used to happen
            % and that is to only set the target option iff there
            % was only one possible target.  Is this a bug?
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
        map.set(Opts1, Option, Data, Opts2)
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
        Components = [_|_],
        construct_string(Components, Grade)
    ).

:- pred construct_string(list(pair(grade_component, string))::in, string::out)
    is det.

construct_string([], "").
construct_string([_ - Bit|Bits], Grade) :-
    (
        Bits = [_|_],
        construct_string(Bits, Grade0),
        string.append_list([Bit, ".", Grade0], Grade)
    ;
        Bits = [],
        Grade = Bit
    ).

:- pred compute_grade_components(option_table::in,
    list(pair(grade_component, string))::out) is det.

compute_grade_components(Options, GradeComponents) :-
    solutions.solutions((pred(CompData::out) is nondet :-
        grade_component_table(Name, Comp, CompOpts, MaybeTargets,
            IncludeInGradeString),
            % For possible component of the grade string
            % include it in the actual grade string if all
            % the option setting that it implies are true.
            % ie
            %   all [Opt, Value] (
            %       member(Opt - Value, CompOpts) =>
            %       map.search(Options, Opt, Value)
            %   )
        \+ (
            list.member(Opt - Value, CompOpts),
            \+ map.search(Options, Opt, Value)
        ),
            % Don't include `.mm' or `.dmm' in grade strings
            % because they are just synonyms for `.mmsc' and
            % `.dmmsc' respectively.
            %
        IncludeInGradeString = yes,

            % When checking gcc_ext there exist grades which
            % can have more then one possible target, ensure that
            % the target in the options table matches one of the
            % possible targets.
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

    % Base components
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

    % Parallelism/multithreading components.
grade_component_table("par", comp_par, [parallel - bool(yes)], no, yes).

    % GC components
grade_component_table("gc", comp_gc, [gc - string("boehm")], no, yes).
grade_component_table("mps", comp_gc, [gc - string("mps")], no, yes).
grade_component_table("agc", comp_gc, [gc - string("accurate")], no, yes).

    % Profiling components
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

    % Term size components
grade_component_table("tsw", comp_term_size,
    [record_term_sizes_as_words - bool(yes),
    record_term_sizes_as_cells - bool(no)], no, yes).
grade_component_table("tsc", comp_term_size,
    [record_term_sizes_as_words - bool(no),
    record_term_sizes_as_cells - bool(yes)], no, yes).

    % Trailing components
grade_component_table("tr", comp_trail, [use_trail - bool(yes)], no, yes).

    % Tag reservation components
grade_component_table("rt", comp_tag, [reserve_tag - bool(yes)], no, yes).

    % Minimal model tabling components
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

    % Pic reg components
grade_component_table("picreg", comp_pic, [pic_reg - bool(yes)], no, yes).

    % Debugging/Tracing components
grade_component_table("decldebug", comp_trace,
    [exec_trace - bool(yes), decl_debug - bool(yes)], no, yes).
grade_component_table("debug", comp_trace,
    [exec_trace - bool(yes), decl_debug - bool(no)], no, yes).

    % Stack extension components
grade_component_table("exts", comp_stack_extend,
    [extend_stacks_when_needed - bool(yes)], no, yes).

:- pred reset_grade_options(option_table::in, option_table::out) is det.

reset_grade_options(Options0, Options) :-
    solutions.aggregate(grade_start_values,
        (pred(Pair::in, Opts0::in, Opts::out) is det :-
            Pair = Option - Value,
            map.set(Opts0, Option, Value, Opts)
        ), Options0, Options).

:- pred grade_start_values(pair(option, option_data)::out) is multi.

grade_start_values(asm_labels - bool(no)).
grade_start_values(gcc_non_local_gotos - bool(no)).
grade_start_values(gcc_global_registers - bool(no)).
grade_start_values(highlevel_code - bool(no)).
grade_start_values(highlevel_data - bool(no)).
grade_start_values(gcc_nested_functions - bool(no)).
grade_start_values(parallel - bool(no)).
grade_start_values(gc - string("none")).
grade_start_values(profile_deep - bool(no)).
grade_start_values(profile_time - bool(no)).
grade_start_values(profile_calls - bool(no)).
grade_start_values(profile_memory - bool(no)).
grade_start_values(use_trail - bool(no)).
grade_start_values(reserve_tag - bool(no)).
grade_start_values(use_minimal_model_stack_copy - bool(no)).
grade_start_values(use_minimal_model_own_stacks - bool(no)).
grade_start_values(minimal_model_debug - bool(no)).
grade_start_values(pic_reg - bool(no)).
grade_start_values(exec_trace - bool(no)).
grade_start_values(decl_debug - bool(no)).
grade_start_values(extend_stacks_when_needed - bool(no)).

:- pred split_grade_string(string::in, list(string)::out) is semidet.

split_grade_string(GradeStr, Components) :-
    string.to_char_list(GradeStr, Chars),
    split_grade_string_2(Chars, Components).

:- pred split_grade_string_2(list(char)::in, list(string)::out) is semidet.

split_grade_string_2([], []).
split_grade_string_2(Chars, Components) :-
    Chars = [_|_],
    list.takewhile(char_is_not('.'), Chars, ThisChars, RestChars0),
    string.from_char_list(ThisChars, ThisComponent),
    Components = [ThisComponent|RestComponents],
    (
        RestChars0 = [_|RestChars], % discard the `.'
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

convert_dump_alias("ALL", "abcdfgilmnprstuvBCDIMPRSTU").
convert_dump_alias("allD", "abcdfgilmnprstuvBCDMPT").
convert_dump_alias("all", "abcdfgilmnprstuvBCMPST").
convert_dump_alias("most", "bcdfgilmnprstuvP").
convert_dump_alias("trans", "bcdglmnstuv").
convert_dump_alias("codegen", "dfnprsu").
convert_dump_alias("vanessa", "ltuCIU").
convert_dump_alias("min", "ilv").
convert_dump_alias("paths", "cP").
convert_dump_alias("petdr", "din").
convert_dump_alias("mm", "bdgvP").      % for debugging minimal model
convert_dump_alias("osv", "bcdglmnpruvP").  % for debugging
                                            % --optimize-saved-vars-cell
convert_dump_alias("ctgc", "cdinpGDRS").

%-----------------------------------------------------------------------------%
:- end_module handle_options.
%-----------------------------------------------------------------------------%
