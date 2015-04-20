%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2011 The University of Melbourne.
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

:- module top_level.mercury_compile_front_end.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.
:- import_module hlds.passes_aux.
:- import_module parse_tree.error_util.

:- import_module bool.
:- import_module io.
:- import_module list.

:- pred frontend_pass(make_hlds_qual_info::in, bool::in, bool::in,
    bool::in, bool::out, module_info::in, module_info::out,
    dump_info::in, dump_info::out, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

    % This type indicates what stage of compilation we are running
    % the simplification pass at. The exact simplifications tasks we run
    % will depend upon this.
    %
:- type simplify_pass
    --->    simplify_pass_frontend
            % Immediately after the frontend passes.

    ;       simplify_pass_post_untuple
            % After the untupling transformation has been applied.

    ;       simplify_pass_pre_prof_transforms
            % If deep/term-size profiling is enabled then immediately
            % before the source-to-source transformations that
            % implement them.

    ;       simplify_pass_pre_implicit_parallelism
            % If implicit parallelism is anbled then perform simplification
            % before it is applied. This helps ensure that the HLDS matches
            % the feedback data.

    ;       simplify_pass_ml_backend
            % The first stage of MLDS code generation.

    ;       simplify_pass_ll_backend.
            % The first stage of LLDS code generation.

    % This predicate sets up and maybe runs the simplification pass.
    %
:- pred maybe_simplify(bool::in, simplify_pass::in, bool::in, bool::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.check_typeclass.
:- import_module check_hlds.cse_detection.
:- import_module check_hlds.det_analysis.
:- import_module check_hlds.implementation_defined_literals.
:- import_module check_hlds.inst_check.
:- import_module check_hlds.inst_user.
:- import_module check_hlds.mode_constraints.
:- import_module check_hlds.modes.
:- import_module check_hlds.oisu_check.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.post_typecheck.
:- import_module check_hlds.purity.
:- import_module check_hlds.simplify.
:- import_module check_hlds.simplify.simplify_proc.
:- import_module check_hlds.simplify.simplify_tasks.
:- import_module check_hlds.stratify.
:- import_module check_hlds.switch_detection.
:- import_module check_hlds.try_expand.
:- import_module check_hlds.type_constraints.
:- import_module check_hlds.typecheck.
:- import_module check_hlds.unique_modes.
:- import_module check_hlds.unused_imports.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_statistics.
:- import_module hlds.make_tags.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_data.
:- import_module top_level.mercury_compile_middle_passes.
:- import_module transform_hlds.dead_proc_elim.
:- import_module transform_hlds.intermod.

:- import_module benchmarking.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

frontend_pass(QualInfo0, FoundUndefTypeError, FoundUndefModeError, !FoundError,
        !HLDS, !DumpInfo, !Specs, !IO) :-
    % We can't continue after an undefined type error, since typecheck
    % would get internal errors.
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        FoundUndefTypeError = yes,
        % We can't continue after an undefined type error, because if we did,
        % typecheck could get internal errors.
        !:FoundError = yes,
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        maybe_write_string(Verbose,
            "% Program contains undefined type error(s).\n", !IO),
        io.set_exit_status(1, !IO)
    ;
        FoundUndefTypeError = no,
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),

        maybe_write_string(Verbose,
            "% Post-processing type definitions...\n", !IO),
        post_process_type_defns(!HLDS, PostTypeSpecs),
        PostTypeErrors = contains_errors(Globals, PostTypeSpecs),
        bool.or(PostTypeErrors, !FoundError),
        maybe_dump_hlds(!.HLDS, 3, "typedefn", !DumpInfo, !IO),

        maybe_write_string(Verbose, "% Checking typeclasses...\n", !IO),
        check_typeclasses(!HLDS, QualInfo0, QualInfo, [], TypeClassSpecs),
        !:Specs = PostTypeSpecs ++ TypeClassSpecs ++ !.Specs,
        maybe_dump_hlds(!.HLDS, 5, "typeclass", !DumpInfo, !IO),
        set_module_recomp_info(QualInfo, !HLDS),

        TypeClassErrors = contains_errors(Globals, TypeClassSpecs),
        (
            TypeClassErrors = yes,
            % We can't continue after a typeclass error, because if we did,
            % typecheck could get internal errors.
            !:FoundError = yes
        ;
            TypeClassErrors = no,
            frontend_pass_after_typeclass_check(FoundUndefModeError,
                !FoundError, !HLDS, !DumpInfo, !Specs, !IO)
        )
    ).

:- pred frontend_pass_after_typeclass_check(bool::in, bool::in, bool::out,
    module_info::in, module_info::out, dump_info::in, dump_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

frontend_pass_after_typeclass_check(FoundUndefModeError, !FoundError,
        !HLDS, !DumpInfo, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),
    globals.lookup_bool_option(Globals, intermodule_optimization, IntermodOpt),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    globals.lookup_bool_option(Globals, use_opt_files, UseOptFiles),
    globals.lookup_bool_option(Globals, make_optimization_interface,
        MakeOptInt),
    globals.lookup_bool_option(Globals, type_check_constraints,
        TypeCheckConstraints),
    (
        ( IntermodOpt = yes
        ; IntermodAnalysis = yes
        ; UseOptFiles = yes
        ),
        MakeOptInt = no
    ->
        % Eliminate unnecessary clauses from `.opt' files,
        % to speed up compilation. This must be done after
        % typeclass instances have been checked, since that
        % fills in which pred_ids are needed by instance decls.
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        maybe_write_string(Verbose, "% Eliminating dead predicates... ", !IO),
        dead_pred_elim(!HLDS),
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        maybe_write_string(Verbose, "done.\n", !IO),
        maybe_dump_hlds(!.HLDS, 10, "dead_pred_elim", !DumpInfo, !IO)
    ;
        true
    ),

    globals.lookup_bool_option(Globals, warn_insts_without_matching_type,
        WarnInstsWithNoMatchingType),
    (
        WarnInstsWithNoMatchingType = yes,
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        maybe_write_string(Verbose,
            "% Checking that insts have matching types... ", !IO),
        check_insts_have_matching_types(!HLDS, !Specs),
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        maybe_write_string(Verbose, "done.\n", !IO),
        maybe_dump_hlds(!.HLDS, 12, "warn_insts_without_matching_type",
            !DumpInfo, !IO)
    ;
        WarnInstsWithNoMatchingType = no
    ),

    % Next typecheck the clauses.
    maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
    maybe_write_string(Verbose, "% Type-checking...\n", !IO),
    maybe_write_string(Verbose, "% Type-checking clauses...\n", !IO),
    (
        TypeCheckConstraints = yes,
        typecheck_constraints(!HLDS, TypeCheckSpecs),
        ExceededTypeCheckIterationLimit = no
    ;
        TypeCheckConstraints = no,
        typecheck_module(!HLDS, TypeCheckSpecs,
            ExceededTypeCheckIterationLimit)
    ),
    !:Specs = TypeCheckSpecs ++ !.Specs,
    maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
    FoundTypeError = contains_errors(Globals, TypeCheckSpecs),
    (
        FoundTypeError = yes,
        maybe_write_string(Verbose,
            "% Program contains type error(s).\n", !IO)
    ;
        FoundTypeError = no,
        maybe_write_string(Verbose,
            "% Program is type-correct.\n", !IO)
    ),
    maybe_report_stats(Stats, !IO),
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
        io.set_exit_status(1, !IO)
    ; ExceededTypeCheckIterationLimit = yes ->
        % FoundTypeError will always be true here, so if Verbose = yes,
        % we've already printed a message about the program containing
        % type errors.
        !:FoundError = yes,
        io.set_exit_status(1, !IO)
    ;
        check_for_missing_type_defns(!.HLDS, MissingTypeDefnSpecs),
        !:Specs = !.Specs ++ MissingTypeDefnSpecs,
        SomeMissingTypeDefns = contains_errors(Globals, MissingTypeDefnSpecs),

        pretest_user_inst_table(!HLDS),
        post_typecheck_finish_preds(!HLDS, NumPostTypeCheckErrors,
            PostTypeCheckAlwaysSpecs, PostTypeCheckNoTypeErrorSpecs),
        % If the main part of typecheck detected some errors, then some of
        % the errors we detect during post-typecheck could be avalanche
        % messages. We get post_typecheck to put all such messages into
        % PostTypeCheckNoTypeErrorSpecs, and we report them only if did not
        % find any errors during typecheck.
        (
            FoundTypeError = no,
            PostTypeCheckSpecs = PostTypeCheckAlwaysSpecs
                ++ PostTypeCheckNoTypeErrorSpecs,
            !:Specs = !.Specs ++ PostTypeCheckSpecs
        ;
            FoundTypeError = yes,
            !:Specs = !.Specs ++ PostTypeCheckAlwaysSpecs
        ),
        (
            ( SomeMissingTypeDefns = yes
            ; NumPostTypeCheckErrors > 0
            )
        ->
            PostTypeCheckErrors = yes
        ;
            PostTypeCheckErrors = no
        ),
        maybe_dump_hlds(!.HLDS, 19, "post_typecheck", !DumpInfo, !IO),

        % Stop here if `--typecheck-only' was specified.
        globals.lookup_bool_option(Globals, typecheck_only, TypecheckOnly),
        (
            TypecheckOnly = yes,
            !:FoundError = bool.or(!.FoundError, PostTypeCheckErrors)
        ;
            TypecheckOnly = no,
            (
                ( FoundTypeError = yes
                ; PostTypeCheckErrors = yes
                )
            ->
                % XXX It would be nice if we could go on and mode-check the
                % predicates which didn't have type errors, but we need to run
                % polymorphism before running mode analysis, and currently
                % polymorphism may get internal errors if any of the predicates
                % are not type-correct.
                !:FoundError = yes
            ;
                puritycheck(Verbose, Stats, !HLDS, !Specs, !IO),
                maybe_dump_hlds(!.HLDS, 20, "puritycheck", !DumpInfo, !IO),

                % Substitute implementation-defined literals before clauses are
                % written out to `.opt' files.
                subst_implementation_defined_literals(Verbose, Stats, !HLDS,
                    !Specs, !IO),
                maybe_dump_hlds(!.HLDS, 25, "implementation_defined_literals",
                    !DumpInfo, !IO),

                % Only write out the `.opt' file if there are no errors.
                (
                    !.FoundError = no,
                    FoundUndefModeError = no
                ->
                    maybe_write_optfile(MakeOptInt, !HLDS, !DumpInfo, !Specs,
                        !IO)
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
                    frontend_pass_by_phases(!HLDS,
                        FoundModeOrDetError, !DumpInfo, !Specs, !IO),
                    !:FoundError = !.FoundError `or` FoundModeOrDetError
                )
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_write_optfile(bool::in, module_info::in, module_info::out,
    dump_info::in, dump_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

maybe_write_optfile(MakeOptInt, !HLDS, !DumpInfo, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, intermodule_optimization, IntermodOpt),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    globals.lookup_bool_option(Globals, intermod_unused_args, IntermodArgs),
    globals.lookup_accumulating_option(Globals, intermod_directories,
        IntermodDirs),
    globals.lookup_bool_option(Globals, use_opt_files, UseOptFiles),
    globals.lookup_bool_option(Globals, termination, Termination),
    globals.lookup_bool_option(Globals, termination2, Termination2),
    globals.lookup_bool_option(Globals, structure_sharing_analysis,
        SharingAnalysis),
    globals.lookup_bool_option(Globals, structure_reuse_analysis,
        ReuseAnalysis),
    globals.lookup_bool_option(Globals, analyse_exceptions,
        ExceptionAnalysis),
    globals.lookup_bool_option(Globals, analyse_closures,
        ClosureAnalysis),
    globals.lookup_bool_option(Globals, analyse_trail_usage,
        TrailingAnalysis),
    globals.lookup_bool_option(Globals, analyse_mm_tabling, TablingAnalysis),
    (
        MakeOptInt = yes,
        write_opt_file(!HLDS, !IO),

        % The following passes are only run with `--intermodule-optimisation'
        % to append their results to the `.opt.tmp' file. For
        % `--intermodule-analysis', analyses results should be recorded
        % using the intermodule analysis framework instead.
        %
        % If intermod_unused_args is being performed, run polymorphism,
        % mode analysis and determinism analysis before unused_args.
        (
            IntermodAnalysis = no,
            ( IntermodArgs = yes
            ; Termination = yes
            ; Termination2 = yes
            ; ExceptionAnalysis = yes
            ; TrailingAnalysis = yes
            ; TablingAnalysis = yes
            ; ClosureAnalysis = yes
            ; SharingAnalysis = yes
            ; ReuseAnalysis = yes
            )
        ->
            frontend_pass_by_phases(!HLDS, FoundModeError, !DumpInfo,
                !Specs, !IO),
            (
                FoundModeError = no,
                middle_pass_for_opt_file(!HLDS, !IO)
            ;
                FoundModeError = yes,
                io.set_exit_status(1, !IO)
            )
        ;
            true
        ),
        module_info_get_name(!.HLDS, ModuleName),
        module_name_to_file_name(Globals, ModuleName, ".opt",
            do_create_dirs, OptName, !IO),
        update_interface(Globals, OptName, !IO),
        touch_interface_datestamp(Globals, ModuleName, ".optdate", !IO)
    ;
        MakeOptInt = no,
        % If there is a `.opt' file for this module the import
        % status of items in the `.opt' file needs to be updated.
        ( IntermodOpt = yes ->
            UpdateStatus = yes
        ; IntermodAnalysis = yes ->
            UpdateStatus = yes
        ; UseOptFiles = yes ->
            module_info_get_name(!.HLDS, ModuleName),
            module_name_to_search_file_name(Globals, ModuleName, ".opt",
                OptName, !IO),
            search_for_file_returning_dir(do_not_open_file, IntermodDirs,
                OptName, Found, !IO),
            (
                Found = ok(_),
                UpdateStatus = yes
            ;
                Found = error(_),
                UpdateStatus = no
            )
        ;
            UpdateStatus = no
        ),
        (
            UpdateStatus = yes,
            intermod.adjust_pred_import_status(!HLDS)
        ;
            UpdateStatus = no
        )
    ).

%-----------------------------------------------------------------------------%

:- pred frontend_pass_by_phases(module_info::in, module_info::out,
    bool::out, dump_info::in, dump_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

frontend_pass_by_phases(!HLDS, FoundError, !DumpInfo, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),

    maybe_polymorphism(Verbose, Stats, !HLDS, !Specs, !IO),
    maybe_dump_hlds(!.HLDS, 30, "polymorphism", !DumpInfo, !IO),

    maybe_warn_about_unused_imports(Verbose, Stats, !HLDS, !Specs, !IO),
    maybe_dump_hlds(!.HLDS, 31, "unused_imports", !DumpInfo, !IO),

    % XXX Convert the mode constraints pass to use error_specs.
    maybe_mode_constraints(Verbose, Stats, !HLDS, !IO),
    maybe_dump_hlds(!.HLDS, 33, "mode_constraints", !DumpInfo, !IO),

    modecheck(Verbose, Stats, !HLDS, FoundModeError, SafeToContinue,
        !Specs, !IO),
    maybe_dump_hlds(!.HLDS, 35, "modecheck", !DumpInfo, !IO),

    (
        SafeToContinue = modes_unsafe_to_continue,
        FoundError = yes
    ;
        SafeToContinue = modes_safe_to_continue,
        detect_switches(Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(!.HLDS, 40, "switch_detect", !DumpInfo, !IO),

        detect_cse(Verbose, Stats, !HLDS, !IO),
        maybe_dump_hlds(!.HLDS, 45, "cse", !DumpInfo, !IO),

        check_determinism(Verbose, Stats, !HLDS, !Specs, !IO),
        maybe_dump_hlds(!.HLDS, 50, "determinism", !DumpInfo, !IO),

        check_unique_modes(Verbose, Stats, !HLDS, FoundUniqError, !Specs, !IO),
        maybe_dump_hlds(!.HLDS, 55, "unique_modes", !DumpInfo, !IO),

        check_stratification(Verbose, Stats, !HLDS, FoundStratError,
            !Specs, !IO),
        maybe_dump_hlds(!.HLDS, 60, "stratification", !DumpInfo, !IO),

        check_oisu_pragmas(Verbose, Stats, !HLDS, FoundOISUError,
            !Specs, !IO),
        maybe_dump_hlds(!.HLDS, 61, "oisu", !DumpInfo, !IO),

        process_try_goals(Verbose, Stats, !HLDS, FoundTryError, !Specs, !IO),
        maybe_dump_hlds(!.HLDS, 62, "try", !DumpInfo, !IO),

        maybe_simplify(yes, simplify_pass_frontend, Verbose, Stats,
            !HLDS, !Specs, !IO),
        maybe_dump_hlds(!.HLDS, 65, "frontend_simplify", !DumpInfo, !IO),

        maybe_proc_statistics(Verbose, Stats, "AfterFrontEnd", !HLDS,
            !Specs, !IO),

        % Work out whether we encountered any errors.
        module_info_get_num_errors(!.HLDS, NumErrors),
        io.get_exit_status(ExitStatus, !IO),
        (
            FoundModeError = no,
            FoundUniqError = no,
            FoundStratError = no,
            FoundOISUError = no,
            FoundTryError = no,
            NumErrors = 0,
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

:- pred puritycheck(bool::in, bool::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

puritycheck(Verbose, Stats, !HLDS, !Specs, !IO) :-
    maybe_write_string(Verbose, "% Purity-checking clauses...\n", !IO),
    puritycheck_module(!HLDS, [], PuritySpecs),
    !:Specs = PuritySpecs ++ !.Specs,
    module_info_get_globals(!.HLDS, Globals),
    PurityErrors = contains_errors(Globals, PuritySpecs),
    maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
    maybe_report_stats(Stats, !IO),
    (
        PurityErrors = yes,
        maybe_write_string(Verbose,
            "% Program contains purity error(s).\n", !IO)
    ;
        PurityErrors = no,
        maybe_write_string(Verbose,
            "% Program is purity-correct.\n", !IO)
    ),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%

:- pred subst_implementation_defined_literals(bool::in, bool::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

subst_implementation_defined_literals(Verbose, Stats, !HLDS, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
    maybe_write_string(Verbose,
        "% Substituting implementation-defined literals...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    subst_impl_defined_literals(!HLDS),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%

:- pred maybe_polymorphism(bool::in, bool::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

maybe_polymorphism(Verbose, Stats, !HLDS, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
    globals.lookup_bool_option(Globals, polymorphism, Polymorphism),
    (
        Polymorphism = yes,
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        (
            VeryVerbose = no,
            maybe_write_string(Verbose,
                "% Transforming polymorphic unifications...", !IO)
        ;
            VeryVerbose = yes,
            maybe_write_string(Verbose,
                "% Transforming polymorphic unifications...\n", !IO)
        ),
        maybe_flush_output(Verbose, !IO),
        polymorphism_process_module(!HLDS),
        (
            VeryVerbose = no,
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            VeryVerbose = yes,
            maybe_write_string(Verbose, "% done.\n", !IO)
        ),
        maybe_report_stats(Stats, !IO)
    ;
        Polymorphism = no,
        % The --no-polymorphism option really doesn't make much
        % sense anymore, because the polymorphism pass is necessary
        % for the proper mode analysis of code using existential
        % types.
        unexpected($module, $pred,
            "sorry, `--no-polymorphism' is no longer supported")
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_warn_about_unused_imports(bool::in, bool::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

maybe_warn_about_unused_imports(Verbose, Stats, !HLDS, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, warn_unused_imports,
        WarnUnusedImports),
    (
        WarnUnusedImports = yes,
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        maybe_write_string(Verbose, "% Checking for unused imports...", !IO),
        warn_about_unused_imports(!.HLDS, UnusedImportSpecs, !IO),
        !:Specs = UnusedImportSpecs ++ !.Specs,
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        maybe_write_string(Verbose, " done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        WarnUnusedImports = no
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_mode_constraints(bool::in, bool::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

maybe_mode_constraints(Verbose, Stats, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, mode_constraints, ModeConstraints),
    (
        ModeConstraints = yes,
        maybe_write_string(Verbose, "% Dumping mode constraints...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        maybe_benchmark_modes(mc_process_module,
            "mode-constraints", !HLDS, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        ModeConstraints = no
    ).

:- pred modecheck(bool::in, bool::in, module_info::in, module_info::out,
    bool::out, modes_safe_to_continue::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

modecheck(Verbose, Stats, !HLDS, FoundModeError, SafeToContinue,
        !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
    globals.lookup_bool_option(Globals, benchmark_modes, BenchmarkModes),
    (
        BenchmarkModes = yes,
        globals.lookup_int_option(Globals, benchmark_modes_repeat, Repeats),
        promise_equivalent_solutions [!:HLDS, SafeToContinue, ModeSpecs, Time]
        (
            benchmark_det(modecheck_module,
                !.HLDS, {!:HLDS, SafeToContinue, ModeSpecs}, Repeats, Time)
        ),
        io.format("BENCHMARK modecheck, %d repeats: %d ms\n",
            [i(Repeats), i(Time)], !IO)
    ;
        BenchmarkModes = no,
        modecheck_module(!.HLDS, {!:HLDS, SafeToContinue, ModeSpecs})
    ),
    !:Specs = ModeSpecs ++ !.Specs,
    FoundModeError = contains_errors(Globals, ModeSpecs),
    (
        FoundModeError = no,
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        maybe_write_string(Verbose, "% Program is mode-correct.\n", !IO)
    ;
        FoundModeError = yes,
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        maybe_write_string(Verbose, "% Program contains mode error(s).\n", !IO)
    ),
    maybe_report_stats(Stats, !IO).

:- pred maybe_benchmark_modes(
    pred(module_info, module_info, io, io)::in(pred(in, out, di, uo) is det),
    string::in, module_info::in, module_info::out, io::di, io::uo) is det.

maybe_benchmark_modes(Pred, Stage, !HLDS, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, benchmark_modes, BenchmarkModes),
    (
        BenchmarkModes = yes,
        globals.lookup_int_option(Globals, benchmark_modes_repeat, Repeats),
        io.format("%s %d ", [s(Stage), i(Repeats)], !IO),
        promise_equivalent_solutions [!:HLDS, Time, !:IO] (
            do_io_benchmark(Pred, Repeats, !.HLDS, !:HLDS - Time, !IO)
        ),
        io.format("%d ms\n", [i(Time)], !IO)
    ;
        BenchmarkModes = no,
        Pred(!HLDS, !IO)
    ).

:- pred do_io_benchmark(pred(T1, T2, io, io)::in(pred(in, out, di, uo) is det),
    int::in, T1::in, pair(T2, int)::out, io::di, io::uo) is cc_multi.

do_io_benchmark(Pred, Repeats, A0, A - Time, !IO) :-
    benchmark_det_io(Pred, A0, A, !IO, Repeats, Time).

%-----------------------------------------------------------------------------%

:- pred detect_switches(bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

detect_switches(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose, "% Detecting switches...\n", !IO),
    maybe_flush_output(Verbose, !IO),
    detect_switches_in_module(!HLDS),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%

:- pred detect_cse(bool::in, bool::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

detect_cse(Verbose, Stats, !HLDS, !IO) :-
    maybe_write_string(Verbose,
        "% Detecting common deconstructions...\n", !IO),
    detect_cse_in_module(!HLDS),
    maybe_write_string(Verbose, "% done.\n", !IO),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%

:- pred check_determinism(bool::in, bool::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

check_determinism(Verbose, Stats, !HLDS, !Specs, !IO) :-
    determinism_pass(!HLDS, DetismSpecs),
    !:Specs = DetismSpecs ++ !.Specs,
    module_info_get_globals(!.HLDS, Globals),
    FoundError = contains_errors(Globals, DetismSpecs),
    maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
    (
        FoundError = yes,
        maybe_write_string(Verbose,
            "% Program contains determinism error(s).\n", !IO)
    ;
        FoundError = no,
        maybe_write_string(Verbose, "% Program is determinism-correct.\n", !IO)
    ),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%

:- pred check_unique_modes(bool::in, bool::in,
    module_info::in, module_info::out, bool::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

check_unique_modes(Verbose, Stats, !HLDS, FoundError, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
    maybe_write_string(Verbose,
        "% Checking for backtracking over unique modes...\n", !IO),
    unique_modes_check_module(!HLDS, UniqueSpecs),
    !:Specs = UniqueSpecs ++ !.Specs,
    FoundError = contains_errors(Globals, UniqueSpecs),
    maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
    (
        FoundError = yes,
        maybe_write_string(Verbose,
            "% Program contains unique mode error(s).\n", !IO)
    ;
        FoundError = no,
        maybe_write_string(Verbose, "% Program is unique-mode-correct.\n", !IO)
    ),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%

:- pred check_stratification(bool::in, bool::in,
    module_info::in, module_info::out, bool::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

check_stratification(Verbose, Stats, !HLDS, FoundError, !Specs, !IO) :-
    module_info_get_must_be_stratified_preds(!.HLDS, MustBeStratifiedPreds),
    module_info_get_globals(!.HLDS, Globals),
    globals.lookup_bool_option(Globals, warn_non_stratification, Warn),
    (
        ( set.is_non_empty(MustBeStratifiedPreds)
        ; Warn = yes
        )
    ->
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        maybe_write_string(Verbose, "% Checking stratification...\n", !IO),
        check_module_for_stratification(!HLDS, StratifySpecs),
        !:Specs = StratifySpecs ++ !.Specs,
        FoundError = contains_errors(Globals, StratifySpecs),
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        (
            FoundError = yes,
            maybe_write_string(Verbose,
                "% Program contains stratification error(s).\n", !IO)
        ;
            FoundError = no,
            maybe_write_string(Verbose, "% done.\n", !IO)
        ),
        maybe_report_stats(Stats, !IO)
    ;
        FoundError = no
    ).

%-----------------------------------------------------------------------------%

:- pred check_oisu_pragmas(bool::in, bool::in,
    module_info::in, module_info::out, bool::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

check_oisu_pragmas(Verbose, Stats, !HLDS, FoundError, !Specs, !IO) :-
    module_info_get_oisu_map(!.HLDS, OISUMap),
    map.to_assoc_list(OISUMap, OISUPairs),
    module_info_get_name(!.HLDS, ModuleName),
    list.filter(type_ctor_is_defined_in_this_module(ModuleName),
        OISUPairs, ModuleOISUPairs),
    (
        ModuleOISUPairs = [_ | _],
        module_info_get_globals(!.HLDS, Globals),
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        maybe_write_string(Verbose,
            "% Checking oisu pragmas...\n", !IO),
        check_oisu_pragmas_for_module(ModuleOISUPairs, !HLDS, OISUSpecs),
        !:Specs = OISUSpecs ++ !.Specs,
        FoundError = contains_errors(Globals, OISUSpecs),
        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        (
            FoundError = yes,
            maybe_write_string(Verbose,
                "% Program contains oisu pragma error(s).\n", !IO)
        ;
            FoundError = no,
            maybe_write_string(Verbose, "% done.\n", !IO)
        ),
        maybe_report_stats(Stats, !IO)
    ;
        ModuleOISUPairs = [],
        FoundError = no
    ).

:- pred type_ctor_is_defined_in_this_module(module_name::in,
    pair(type_ctor, oisu_preds)::in) is semidet.

type_ctor_is_defined_in_this_module(ModuleName, TypeCtor - _) :-
    TypeCtor = type_ctor(TypeSymName, _TypeArity),
    TypeSymName = qualified(TypeModuleName, _TypeName),
    ModuleName = TypeModuleName.

%-----------------------------------------------------------------------------%

:- pred process_try_goals(bool::in, bool::in,
    module_info::in, module_info::out, bool::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

process_try_goals(Verbose, Stats, !HLDS, FoundError, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
    maybe_write_string(Verbose, "% Transforming try goals...\n", !IO),
    expand_try_goals_in_module(!HLDS, [], TryExpandSpecs),
    !:Specs = TryExpandSpecs ++ !.Specs,
    FoundError = contains_errors(Globals, TryExpandSpecs),
    maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
    (
        FoundError = yes,
        maybe_write_string(Verbose, "% Program contains error(s).\n", !IO)
    ;
        FoundError = no,
        maybe_write_string(Verbose, "% done.\n", !IO)
    ),
    maybe_report_stats(Stats, !IO).

%-----------------------------------------------------------------------------%

maybe_simplify(Warn, SimplifyPass, Verbose, Stats, !HLDS, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    some [!SimpList] (
        find_simplify_tasks(Warn, Globals, SimplifyTasks0),
        !:SimpList = simplify_tasks_to_list(SimplifyTasks0),
        (
            SimplifyPass = simplify_pass_frontend,
            list.cons(simptask_after_front_end, !SimpList)
        ;
            SimplifyPass = simplify_pass_post_untuple,
            list.cons(simptask_mark_code_model_changes, !SimpList)
        ;
            SimplifyPass = simplify_pass_pre_prof_transforms,

            % We run the simplify pass before the profiling transformations
            % only if those transformations are being applied; otherwise we
            % just leave things to the backend simplification passes.

            globals.lookup_bool_option(Globals, pre_prof_transforms_simplify,
                PreProfSimplify),
            (
                PreProfSimplify = yes,
                list.cons(simptask_mark_code_model_changes, !SimpList)
            ;
                PreProfSimplify = no,
                !:SimpList = []
            )
        ;
            SimplifyPass = simplify_pass_pre_implicit_parallelism,

            % We run the simplify pass before the implicit parallelism pass if
            % implicit parallelism is enabled.

            globals.lookup_bool_option(Globals,
                pre_implicit_parallelism_simplify, PreParSimplify),
            (
                PreParSimplify = yes,
                list.cons(simptask_mark_code_model_changes, !SimpList)
            ;
                PreParSimplify = no,
                !:SimpList = []
            )
        ;
            SimplifyPass = simplify_pass_ml_backend,
            list.cons(simptask_mark_code_model_changes, !SimpList)
        ;
            SimplifyPass = simplify_pass_ll_backend,
            % Don't perform constant propagation if one of the
            % profiling transformations has been applied.
            %
            % NOTE: Any changes made here may also need to be made
            % to the relevant parts of backend_pass_by_preds_4/12.
            globals.lookup_bool_option(Globals, constant_propagation,
                ConstProp),
            globals.lookup_bool_option(Globals, profile_deep, DeepProf),
            globals.lookup_bool_option(Globals, record_term_sizes_as_words,
                TSWProf),
            globals.lookup_bool_option(Globals, record_term_sizes_as_cells,
                TSCProf),
            (
                ConstProp = yes,
                DeepProf = no,
                TSWProf = no,
                TSCProf = no
            ->
                list.cons(simptask_constant_prop, !SimpList)
            ;
                !:SimpList = list.delete_all(!.SimpList, simptask_constant_prop)
            ),
            list.cons(simptask_mark_code_model_changes, !SimpList),
            list.cons(simptask_elim_removable_scopes, !SimpList)
        ),
        SimpList = !.SimpList
    ),
    (
        SimpList = [_ | _],

        maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),
        maybe_write_string(Verbose, "% Simplifying goals...\n", !IO),
        maybe_flush_output(Verbose, !IO),
        SimplifyTasks = list_to_simplify_tasks(SimpList),
        process_all_nonimported_preds_errors(
            update_pred_error(simplify_pred(SimplifyTasks)),
            !HLDS, [], SimplifySpecs, !IO),
        (
            SimplifyPass = simplify_pass_frontend,
            !:Specs = SimplifySpecs ++ !.Specs,
            maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO)
        ;
            ( SimplifyPass = simplify_pass_ll_backend
            ; SimplifyPass = simplify_pass_ml_backend
            ; SimplifyPass = simplify_pass_post_untuple
            ; SimplifyPass = simplify_pass_pre_prof_transforms
            ; SimplifyPass = simplify_pass_pre_implicit_parallelism
            )
        ),
        maybe_write_string(Verbose, "% done.\n", !IO),
        maybe_report_stats(Stats, !IO)
    ;
        SimpList = []
    ).

:- pred simplify_pred(simplify_tasks::in, pred_id::in,
    module_info::in, module_info::out, pred_info::in, pred_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

simplify_pred(SimplifyTasks0, PredId, !ModuleInfo, !PredInfo, !Specs) :-
    trace [io(!IO)] (
        write_pred_progress_message("% Simplifying ", PredId, !.ModuleInfo,
            !IO)
    ),
    ProcIds = pred_info_non_imported_procids(!.PredInfo),
    % Don't warn for compiler-generated procedures.
    ( is_unify_or_compare_pred(!.PredInfo) ->
        SimplifyTasks = SimplifyTasks0 ^ do_warn_simple_code := no
    ;
        SimplifyTasks = SimplifyTasks0
    ),
    ErrorSpecs0 = init_error_spec_accumulator,
    simplify_pred_procs(SimplifyTasks, PredId, ProcIds, !ModuleInfo,
        !PredInfo, ErrorSpecs0, ErrorSpecs),
    module_info_get_globals(!.ModuleInfo, Globals),
    SpecsList = error_spec_accumulator_to_list(ErrorSpecs),
    !:Specs = SpecsList ++ !.Specs,
    globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
    trace [io(!IO)] (
        maybe_report_stats(Statistics, !IO)
    ).

:- pred maybe_proc_statistics(bool::in, bool::in, string::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

maybe_proc_statistics(Verbose, Stats, Msg, !HLDS, !Specs, !IO) :-
    module_info_get_globals(!.HLDS, Globals),
    maybe_write_out_errors(Verbose, Globals, !HLDS, !Specs, !IO),

    globals.lookup_string_option(Globals, proc_size_statistics, StatsFileName),
    ( StatsFileName = "" ->
        % The user has not asked us to print these statistics.
        true
    ;
        io.open_append(StatsFileName, StatsFileNameResult, !IO),
        (
            StatsFileNameResult = ok(StatsFileStream),
            maybe_write_string(Verbose,
                "% Generating proc statistics...\n", !IO),
            write_proc_stats_for_module(StatsFileStream, Msg, !.HLDS, !IO),
            maybe_write_string(Verbose, "% done.\n", !IO),
            maybe_report_stats(Stats, !IO)
        ;
            StatsFileNameResult = error(StatsFileError),
            io.error_message(StatsFileError, StatsFileErrorMsg),
            maybe_write_string(Verbose,
                "% Cannot write proc statistics: ", !IO),
            maybe_write_string(Verbose, StatsFileErrorMsg, !IO),
            maybe_write_string(Verbose, "\n", !IO)
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module top_level.mercury_compile_front_end.
%-----------------------------------------------------------------------------%
