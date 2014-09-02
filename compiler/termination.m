%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2001, 2003-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: termination.m.
% Main author: crs.
% Significant modifications by zs.
%
% This termination analysis is based on the algorithm given by Gerhard Groeger
% and Lutz Plumer in their paper "Handling of Mutual Recursion in Automatic
% Termination Proofs for Logic Programs"  which was printed in JICSLP '92 (the
% proceedings of the Joint International Conference and Symposium on Logic
% Programming 1992) pages 336 - 350.
%
% Details about this implementation are covered in:
% Chris Speirs, Zoltan Somogyi, and Harald Sondergaard. Termination
% analysis for Mercury. In P. Van Hentenryck, editor, Static Analysis:
% Proceedings of the 4th International Symposium, Lecture Notes in Computer
% Science. Springer, 1997. A more detailed version is available for
% download from <http://www.cs.mu.oz.au/publications/tr_db/mu_97_09.ps.gz>
%
% The termination analysis may use a number of different norms to calculate
% the size of a term. These are set by using the --termination-norm string
% option.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.termination.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.error_util.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Perform termination analysis on the module.
    %
:- pred analyse_termination_in_module(module_info::in, module_info::out,
    list(error_spec)::out, io::di, io::uo) is det.

    % Write out a termination_info pragma for the predicate if it is exported,
    % it is not a builtin and it is not a predicate used to force type
    % specialization.
    %
:- pred write_pred_termination_info(module_info::in, pred_id::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.passes_aux.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.dependency_graph.
:- import_module transform_hlds.post_term_analysis.
:- import_module transform_hlds.term_errors.
:- import_module transform_hlds.term_norm.
:- import_module transform_hlds.term_pass1.
:- import_module transform_hlds.term_pass2.
:- import_module transform_hlds.term_util.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module unit.

%-----------------------------------------------------------------------------%

analyse_termination_in_module(!ModuleInfo, Specs, !IO) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_termination_norm(Globals, TermNorm),
    FunctorInfo = set_functor_info(!.ModuleInfo, TermNorm),
    globals.lookup_int_option(Globals, termination_error_limit, MaxErrors),
    globals.lookup_int_option(Globals, termination_path_limit, MaxPaths),
    PassInfo = pass_info(FunctorInfo, MaxErrors, MaxPaths),

    % Process builtin and compiler-generated predicates, and user-supplied
    % pragmas.
    module_info_get_valid_predids(PredIds, !ModuleInfo),
    check_preds(PredIds, !ModuleInfo, !IO),

    % Process all the SCCs of the call graph in a bottom-up order.
    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_dependency_info(!.ModuleInfo, DepInfo),
    hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),

    % Set the termination status of foreign_procs based on the foreign code
    % attributes.
    check_foreign_code_attributes(SCCs, !ModuleInfo, [], Specs1),

    % Ensure that termination pragmas for a procedure do not conflict with
    % termination pragmas for other procedures in the SCC.
    check_pragmas_are_consistent(SCCs, !ModuleInfo, Specs1, Specs),

    list.foldl2(analyse_termination_in_scc(PassInfo), SCCs, !ModuleInfo, !IO),

    % XXX update this once this analysis supports `--intermodule-analysis'
    globals.lookup_bool_option(Globals, make_optimization_interface,
        MakeOptInt),
    (
        MakeOptInt = yes,
        make_termination_opt_int(PredIds, !.ModuleInfo, !IO)
    ;
        MakeOptInt = no
    ),
    run_post_term_analysis(!.ModuleInfo, !IO).

%----------------------------------------------------------------------------%
%
% Handle foreign code attributes.
%

% Set the termination status for any procedures implemented using the foreign
% language interface. If the terminates/does_not_terminate attribute has been
% set then we set the termination status of the procedure accordingly.
% Otherwise the procedure is considered to be terminating if it does not call
% Mercury and non-terminating if it does.
%
% We also check that the foreign code attributes do not conflict with any
% termination pragmas that have been supplied for the procedure.

:- pred check_foreign_code_attributes(list(list(pred_proc_id))::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_foreign_code_attributes(SCCs, !ModuleInfo, !Specs) :-
    list.foldl2(check_foreign_code_attributes_2, SCCs, !ModuleInfo, !Specs).

:- pred check_foreign_code_attributes_2(list(pred_proc_id)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_foreign_code_attributes_2([], !ModuleInfo, !Specs) :-
    unexpected($module, $pred, "empty SCC").
check_foreign_code_attributes_2([PPId], !ModuleInfo, !Specs) :-
    some [!ProcInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, !:ProcInfo),
        (
            proc_info_get_goal(!.ProcInfo, Goal),
            Goal = hlds_goal(
                call_foreign_proc(Attributes, _, _, _, _, _, _),
                _GoalInfo)
        ->
            proc_info_get_maybe_termination_info(!.ProcInfo, MaybeTermination),
            proc_info_get_context(!.ProcInfo, Context),
            (
                MaybeTermination = no,
                ( attributes_imply_termination(Attributes) ->
                    TermStatus = yes(cannot_loop(unit)),
                    proc_info_set_maybe_termination_info(TermStatus, !ProcInfo)
                ;
                    TermErr = termination_error_context(
                        does_not_term_foreign(PPId), Context),
                    TermStatus = yes(can_loop([TermErr])),
                    proc_info_set_maybe_termination_info(TermStatus, !ProcInfo)
                )
            ;
                % If there was a `:- pragma terminates' declaration for this
                % procedure then check that the foreign code attributes do not
                % contradict this.
                MaybeTermination = yes(cannot_loop(_)),
                ( get_terminates(Attributes) = proc_does_not_terminate ->
                    TermErr = termination_error_context(
                        inconsistent_annotations, Context),
                    TermStatus = yes(can_loop([TermErr])),
                    % XXX intermod
                    proc_info_set_maybe_termination_info(TermStatus,
                        !ProcInfo),
                    ProcNamePieces = describe_one_proc_name(!.ModuleInfo,
                        should_module_qualify, PPId),
                    Pieces =
                        [words("Warning:") | ProcNamePieces] ++
                        [words("has a"), pragma_decl("terminates"),
                        words("declaration but also has the"),
                        quote("does_not_terminate"),
                        words("foreign code attribute set.")],
                    Msg = simple_msg(Context, [always(Pieces)]),
                    Spec = error_spec(severity_warning, phase_read_files,
                        [Msg]),
                    !:Specs = [Spec | !.Specs]
                ;
                    true
                )
            ;
                % In this case there was a `pragma does_not_terminate'
                % declaration - check that the foreign code attribute
                % does not contradict this.
                MaybeTermination = yes(can_loop(TermErrs0)),
                ( get_terminates(Attributes) = proc_terminates ->
                    TermErr = termination_error_context(
                        inconsistent_annotations, Context),
                    TermErrs = [TermErr | TermErrs0],
                    TermStatus =  yes(can_loop(TermErrs)),
                    % XXX intermod
                    proc_info_set_maybe_termination_info(TermStatus,
                        !ProcInfo),
                    ProcNamePieces = describe_one_proc_name(!.ModuleInfo,
                        should_module_qualify, PPId),
                    Pieces =
                        [words("Warning:") | ProcNamePieces] ++
                        [words("has a"), pragma_decl("does_not_terminate"),
                        words("declaration but also has the"),
                        quote("terminates"),
                        words("foreign code attribute set.")],
                    Msg = simple_msg(Context, [always(Pieces)]),
                    Spec = error_spec(severity_warning, phase_read_files,
                        [Msg]),
                    !:Specs = [Spec | !.Specs]
                ;
                    true
                )
            ),
            module_info_set_pred_proc_info(PPId, PredInfo, !.ProcInfo,
                !ModuleInfo)
        ;
            true
        )
    ).
check_foreign_code_attributes_2([_, _ | _], !ModuleInfo, !IO).

%----------------------------------------------------------------------------%
%
% Check termination pragmas for consistency.
%

% Check that any user-supplied termination information (from pragma
% terminates/does_not_terminate) is consistent for each SCC in the program.
%
% The information will not be consistent if:
% (1) One or more procs. in the SCC has a terminates pragma *and* one or more
%     procs. in the SCC has a does_not_terminate pragma.
% (2) One or more procs. in the SCC has a termination pragma (of either sort),
%     *and* the termination status of one or more procs. in the SCC is
%     unknown. (We check this after checking for the first case, so the
%     termination info. for the known procs. will be consistent.)
%
% In the first case set the termination for all procs. in the SCC to can_loop
% and emit a warning. In the second, set the termination of any procedures
% whose termination status is unknown to be the same as those whose
% termination status is known.

:- pred check_pragmas_are_consistent(list(list(pred_proc_id))::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_pragmas_are_consistent(SCCs, !ModuleInfo, !Specs) :-
    list.foldl2(check_scc_pragmas_are_consistent, SCCs, !ModuleInfo, !Specs).

:- pred check_scc_pragmas_are_consistent(list(pred_proc_id)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_scc_pragmas_are_consistent(SCC, !ModuleInfo, !Specs) :-
    list.filter(is_termination_known(!.ModuleInfo), SCC, SCCTerminationKnown,
        SCCTerminationUnknown),
    (
        SCCTerminationKnown = []
    ;
        SCCTerminationKnown = [KnownPPId | _],
        module_info_pred_proc_info(!.ModuleInfo, KnownPPId, _, KnownProcInfo),
        proc_info_get_maybe_termination_info(KnownProcInfo, MaybeKnownTerm),
        (
            MaybeKnownTerm = no,
            unexpected($module, $pred, "no termination info found")
        ;
            MaybeKnownTerm  = yes(KnownTermStatus)
        ),
        (
            check_procs_known_term(KnownTermStatus, SCCTerminationKnown,
                !.ModuleInfo)
        ->
            % Force any procedures in the SCC whose termination status is
            % unknown to have the same termination status as those that are
            % known.
            set_termination_infos(SCCTerminationUnknown, KnownTermStatus,
                !ModuleInfo)
        ;
            % There is a conflict between the user-supplied termination
            % information for two or more procedures in this SCC.
            % Emit a warning and then assume that they all loop.
            get_context_from_scc(SCCTerminationKnown, !.ModuleInfo,
                Context),
            NewTermStatus = can_loop([termination_error_context(
                inconsistent_annotations, Context)]),
            set_termination_infos(SCC, NewTermStatus, !ModuleInfo),

            PredIds = list.map((func(proc(PredId, _)) = PredId),
                SCCTerminationKnown),
            PredNamePieces = describe_several_pred_names(!.ModuleInfo,
                should_module_qualify, PredIds),
            Pieces =
                [words("Warning:") | PredNamePieces ] ++
                [words("are mutually recursive but some of their"),
                words( "termination pragmas are inconsistent.")],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_warning, phase_read_files, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

    % Check that all procedures in an SCC whose termination status is known
    % have the same termination status.
    %
:- pred check_procs_known_term(termination_info::in, list(pred_proc_id)::in,
    module_info::in) is semidet.

check_procs_known_term(_, [], _).
check_procs_known_term(Status, [PPId | PPIds], ModuleInfo) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_maybe_termination_info(ProcInfo, MaybeTerm),
    (
        MaybeTerm = no,
        unexpected($module, $pred, "no termination info for procedure")
    ;
        MaybeTerm = yes(PPIdStatus)
    ),
    (
        Status = cannot_loop(_),
        PPIdStatus = cannot_loop(_)
    ;
        Status = can_loop(_),
        PPIdStatus = can_loop(_)
    ),
    check_procs_known_term(Status, PPIds, ModuleInfo).

%-----------------------------------------------------------------------------%
%
% Run termination analysis on a single SCC.
%

    % For each SCC, we first find out the relationships among the sizes of the
    % arguments of the procedures of the SCC, and then attempt to prove
    % termination of the procedures.
    %
:- pred analyse_termination_in_scc(pass_info::in, list(pred_proc_id)::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

analyse_termination_in_scc(PassInfo, SCC, !ModuleInfo, !IO) :-
    IsArgSizeKnown = (pred(PPId::in) is semidet :-
        module_info_pred_proc_info(!.ModuleInfo, PPId, _, ProcInfo),
        proc_info_get_maybe_arg_size_info(ProcInfo, yes(_))
    ),
    list.filter(IsArgSizeKnown, SCC, _SCCArgSizeKnown, SCCArgSizeUnknown),
    (
        SCCArgSizeUnknown = [],
        ArgSizeErrors = [],
        TermErrors = []
    ;
        SCCArgSizeUnknown = [_|_],
        find_arg_sizes_in_scc(SCCArgSizeUnknown, PassInfo, ArgSizeResult,
            TermErrors, !ModuleInfo, !IO),
        (
            ArgSizeResult = arg_size_ok(Solutions, OutputSupplierMap),
            set_finite_arg_size_infos(Solutions, OutputSupplierMap,
                !ModuleInfo),
            ArgSizeErrors = []
        ;
            ArgSizeResult = arg_size_error(Errors),
            set_infinite_arg_size_infos(SCCArgSizeUnknown,
                infinite(Errors), !ModuleInfo),
            ArgSizeErrors = Errors
        )
    ),
    list.filter(is_termination_known(!.ModuleInfo), SCC,
        _SCCTerminationKnown, SCCTerminationUnknown),
    (
        % We may possibly have encountered inconsistent
        % terminates/does_not_terminate pragmas for this SCC, so we need to
        % report errors here as well.
        SCCTerminationUnknown = []
    ;
        SCCTerminationUnknown = [_ | _],
        IsFatal = (pred(ErrorAndContext::in) is semidet :-
            ErrorAndContext = termination_error_context(Error, _),
            is_fatal_error(Error) = yes
        ),
        list.filter(IsFatal, ArgSizeErrors, FatalErrors),
        BothErrors = TermErrors ++ FatalErrors,
        (
            BothErrors = [_ | _],
            % These errors prevent pass 2 from proving termination
            % in any case, so we may as well not prove it quickly.
            PassInfo = pass_info(_, MaxErrors, _),
            list.take_upto(MaxErrors, BothErrors, ReportedErrors),
            TerminationResult = can_loop(ReportedErrors)
        ;
            BothErrors = [],
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.lookup_int_option(Globals, termination_single_args,
                SingleArgs),
            prove_termination_in_scc(SCCTerminationUnknown,
                PassInfo, SingleArgs, TerminationResult, !ModuleInfo, !IO)
        ),
        set_termination_infos(SCCTerminationUnknown, TerminationResult,
            !ModuleInfo),
        (
            TerminationResult = can_loop(TerminationErrors),
            report_termination_errors(SCC, TerminationErrors,
                !ModuleInfo, !IO)
        ;
            TerminationResult = cannot_loop(_)
        )
    ).

%-----------------------------------------------------------------------------%

    % This predicate takes the results from solve_equations and inserts these
    % results into the module info.
    %
:- pred set_finite_arg_size_infos(list(pair(pred_proc_id, int))::in,
    used_args::in, module_info::in, module_info::out) is det.

set_finite_arg_size_infos([], _, !ModuleInfo).
set_finite_arg_size_infos([Soln | Solns], OutputSupplierMap, !ModuleInfo) :-
    Soln = PPId - Gamma,
    PPId = proc(PredId, ProcId),
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_procedures(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo),
    map.lookup(OutputSupplierMap, PPId, OutputSuppliers),
    ArgSizeInfo = finite(Gamma, OutputSuppliers),
    % XXX intermod
    proc_info_set_maybe_arg_size_info(yes(ArgSizeInfo), ProcInfo, ProcInfo1),
    map.set(ProcId, ProcInfo1, ProcTable0, ProcTable),
    pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
    map.set(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo),
    set_finite_arg_size_infos(Solns, OutputSupplierMap, !ModuleInfo).

:- pred set_infinite_arg_size_infos(list(pred_proc_id)::in,
    arg_size_info::in, module_info::in, module_info::out) is det.

set_infinite_arg_size_infos([], _, !ModuleInfo).
set_infinite_arg_size_infos([PPId | PPIds], ArgSizeInfo, !ModuleInfo) :-
    PPId = proc(PredId, ProcId),
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_procedures(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),
    % XXX intermod
    proc_info_set_maybe_arg_size_info(yes(ArgSizeInfo), ProcInfo0, ProcInfo),
    map.set(ProcId, ProcInfo, ProcTable0, ProcTable),
    pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
    map.set(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo),
    set_infinite_arg_size_infos(PPIds, ArgSizeInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred set_termination_infos(list(pred_proc_id)::in, termination_info::in,
    module_info::in, module_info::out) is det.

set_termination_infos([], _, !ModuleInfo).
set_termination_infos([PPId | PPIds], TerminationInfo, !ModuleInfo) :-
    PPId = proc(PredId, ProcId),
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_procedures(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),
    % XXX intermod
    proc_info_set_maybe_termination_info(yes(TerminationInfo),
        ProcInfo0, ProcInfo),
    map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
    pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo),
    set_termination_infos(PPIds, TerminationInfo, !ModuleInfo).

:- pred report_termination_errors(list(pred_proc_id)::in,
    termination_error_contexts::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

report_termination_errors(SCC, Errors, !ModuleInfo, !IO) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, termination_check, NormalErrors),
    globals.lookup_bool_option(Globals, verbose_check_termination,
        VerboseErrors),
    (
        IsCheckTerm = (pred(PPId::in) is semidet :-
            module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, _),
            \+ pred_info_is_imported(PredInfo),
            pred_info_get_markers(PredInfo, Markers),
            check_marker(Markers, marker_check_termination)
        ),
        list.filter(IsCheckTerm, SCC, CheckTermPPIds),
        CheckTermPPIds = [_ | _]
    ->
        % If any procedure in the SCC has a check_terminates pragma,
        % print out one error message for the whole SCC and indicate
        % an error.
        report_term_errors(SCC, Errors, !.ModuleInfo, !IO),
        io.set_exit_status(1, !IO),
        module_info_incr_errors(!ModuleInfo)
    ;
        IsNonImported = (pred(PPId::in) is semidet :-
            module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, _),
            \+ pred_info_is_imported(PredInfo)
        ),
        list.filter(IsNonImported, SCC, NonImportedPPIds),
        NonImportedPPIds = [_|_],

        % Don't emit non-termination warnings for the compiler generated
        % wrapper predicates for solver type initialisation predicates.
        % If they don't terminate there's nothing the user can do about it
        % anyway - the problem is with the initialisation predicate specified
        % by the user, not the wrapper.
        list.all_false(is_solver_init_wrapper_pred(!.ModuleInfo), SCC),

        % Only output warnings of non-termination for direct errors. If there
        % are no direct errors then output the indirect errors - this is
        % better than giving no reason at all. If verbose errors is enabled
        % then output both sorts of error.
        % (See term_errors.m for details of direct and indirect errors.)

        (
            VerboseErrors = yes,
            PrintErrors = Errors
        ;
            VerboseErrors = no,
            (
                NormalErrors = yes,
                IsNonSimple = (pred(ContextError::in) is semidet :-
                    ContextError = termination_error_context(Error, _Context),
                    is_indirect_error(Error) = no
                ),
                list.filter(IsNonSimple, Errors, PrintErrors0),
                % If there were no direct errors then use the indirect errors.
                % This prevents warnings that report termination could not be
                % proven for unknown reasons.
                (
                    PrintErrors0 = [],
                    PrintErrors = Errors
                ;
                    PrintErrors0 = [_ | _],
                    PrintErrors = PrintErrors0
                )
            ;
                NormalErrors = no,
                fail
            )
        )
    ->
        report_term_errors(SCC, PrintErrors, !.ModuleInfo, !IO)
    ;
        true
    ).

    % Succeeds iff the given PPId is a compiler generated wrapper
    % predicate for a solver type initialisation predicate.
    %
:- pred is_solver_init_wrapper_pred(module_info::in, pred_proc_id::in)
    is semidet.

is_solver_init_wrapper_pred(ModuleInfo, proc(PredId, _)) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_origin(PredInfo, PredOrigin),
    PredOrigin = origin_special_pred(SpecialPredId - _),
    SpecialPredId = spec_pred_init.

%----------------------------------------------------------------------------%

    % This predicate processes each predicate and sets the termination
    % property if possible. This is done as follows:  Set the termination
    % to yes if:
    % - there is a terminates pragma defined for the predicate
    % - there is a `check_termination' pragma defined for the predicate,
    %   and it is imported, and the compiler is not currently generating
    %   the intermodule optimization file.
    % - the predicate is a builtin predicate or is compiler generated (This
    %   also sets the termination constant and UsedArgs).
    %
    % Set the termination to dont_know if:
    % - there is a `does_not_terminate' pragma defined for this predicate.
    % - the predicate is imported and there is no other source of
    %   information about it (termination_info pragmas, terminates pragmas,
    %   check_termination pragmas, builtin/compiler generated).
    %
:- pred check_preds(list(pred_id)::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

check_preds([], !ModuleInfo, !IO).
check_preds([PredId | PredIds], !ModuleInfo, !IO) :-
    write_pred_progress_message("% Checking termination of ", PredId,
        !.ModuleInfo, !IO),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, make_optimization_interface,
        MakeOptInt),
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_import_status(PredInfo0, ImportStatus),
    pred_info_get_context(PredInfo0, Context),
    pred_info_get_procedures(PredInfo0, ProcTable0),
    pred_info_get_markers(PredInfo0, Markers),
    ProcIds = pred_info_procids(PredInfo0),
    (
        % It is possible for compiler generated/mercury builtin
        % predicates to be imported or locally defined, so they
        % must be covered here, separately.
        set_compiler_gen_terminates(PredInfo0, ProcIds, PredId,
            !.ModuleInfo, ProcTable0, ProcTable1)
    ->
        ProcTable2 = ProcTable1
    ;
        status_defined_in_this_module(ImportStatus) = yes
    ->
        % Since we cannot see the definition we consider procedures
        % defined using `:- external' to be imported.
        ( check_marker(Markers, marker_terminates) ->
            change_procs_termination_info(ProcIds, yes, cannot_loop(unit),
                ProcTable0, ProcTable2)
        ;
            ProcTable2 = ProcTable0
        )
    ;
        % Not defined in this module.

        % All of the predicates that are processed in this section are imported
        % in some way. With imported predicates, any 'check_termination'
        % pragmas will be checked by the compiler when it compiles the
        % relevant source file (that the predicate was imported from).
        % When making the intermodule optimizations, the check_termination
        % will not be checked when the relevant source file is compiled,
        % so it cannot be depended upon.
        (
            (
                check_marker(Markers, marker_terminates)
            ;
                MakeOptInt = no,
                check_marker(Markers, marker_check_termination)
            )
        ->
            change_procs_termination_info(ProcIds, yes, cannot_loop(unit),
                ProcTable0, ProcTable1)
        ;
            TerminationError = termination_error_context(imported_pred,
                Context),
            TerminationInfo = can_loop([TerminationError]),
            change_procs_termination_info(ProcIds, no, TerminationInfo,
                ProcTable0, ProcTable1)
        ),
        ArgSizeError = termination_error_context(imported_pred, Context),
        ArgSizeInfo = infinite([ArgSizeError]),
        change_procs_arg_size_info(ProcIds, no, ArgSizeInfo,
            ProcTable1, ProcTable2)
    ),
    ( check_marker(Markers, marker_does_not_terminate) ->
        RequestError = termination_error_context(
            does_not_term_pragma(PredId), Context),
        RequestTerminationInfo = can_loop([RequestError]),
        change_procs_termination_info(ProcIds, yes,
            RequestTerminationInfo, ProcTable2, ProcTable)
    ;
        ProcTable = ProcTable2
    ),
    pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
    map.set(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo),
    check_preds(PredIds, !ModuleInfo, !IO).

%----------------------------------------------------------------------------%

    % This predicate checks each ProcId in the list to see if it is a compiler
    % generated predicate, or a predicate from builtin.m or private_builtin.m.
    % If it is, then the compiler sets the termination property of the ProcIds
    % accordingly.
    %
    % We assume that user-defined special predicates terminate. This assumption
    % is checked later during the post_term_analysis pass.
    %
:- pred set_compiler_gen_terminates(pred_info::in, list(proc_id)::in,
    pred_id::in, module_info::in, proc_table::in, proc_table::out)
    is semidet.

set_compiler_gen_terminates(PredInfo, ProcIds, PredId, ModuleInfo,
        !ProcTable) :-
    (
        pred_info_is_builtin(PredInfo)
    ->
        set_builtin_terminates(ProcIds, PredId, PredInfo, ModuleInfo,
            !ProcTable)
    ;
        (
            ModuleName = pred_info_module(PredInfo),
            Name = pred_info_name(PredInfo),
            Arity = pred_info_orig_arity(PredInfo),
            special_pred_name_arity(SpecPredId0, Name, _, Arity),
            any_mercury_builtin_module(ModuleName)
        ->
            SpecialPredId = SpecPredId0
        ;
            pred_info_get_origin(PredInfo, Origin),
            Origin = origin_special_pred(SpecialPredId - _)
        )
    ->
        set_generated_terminates(ProcIds, SpecialPredId, !ProcTable)
    ;
        fail
    ).

:- pred set_generated_terminates(list(proc_id)::in, special_pred_id::in,
    proc_table::in, proc_table::out) is det.

set_generated_terminates([], _, !ProcTable).
set_generated_terminates([ProcId | ProcIds], SpecialPredId, !ProcTable) :-
    (
        ( SpecialPredId = spec_pred_unify
        ; SpecialPredId = spec_pred_compare
        ; SpecialPredId = spec_pred_index
        ),
        map.lookup(!.ProcTable, ProcId, ProcInfo0),
        proc_info_get_headvars(ProcInfo0, HeadVars),
        special_pred_id_to_termination(SpecialPredId, HeadVars,
            ArgSize, Termination),
        proc_info_set_maybe_arg_size_info(yes(ArgSize), ProcInfo0, ProcInfo1),
        proc_info_set_maybe_termination_info(yes(Termination),
            ProcInfo1, ProcInfo),
        map.det_update(ProcId, ProcInfo, !ProcTable)
    ;
        SpecialPredId = spec_pred_init
        % We don't need to do anything special for solver type initialisation
        % predicates. Leaving it up to the analyser may result in better
        % argument size information anyway.
    ),
    set_generated_terminates(ProcIds, SpecialPredId, !ProcTable).

    % XXX The ArgSize argument for unify predicates may not be correct
    % in the case where the type has user-defined equality.
    %
:- pred special_pred_id_to_termination(special_pred_id::in,
    prog_vars::in, arg_size_info::out, termination_info::out) is det.

special_pred_id_to_termination(SpecialPredId, HeadVars, ArgSize,
        Termination) :-
    (
        SpecialPredId = spec_pred_compare,
        term_util.make_bool_list(HeadVars, [no, no, no], OutList),
        ArgSize = finite(0, OutList),
        Termination = cannot_loop(unit)
    ;
        SpecialPredId = spec_pred_unify,
        term_util.make_bool_list(HeadVars, [yes, yes], OutList),
        ArgSize = finite(0, OutList),
        Termination = cannot_loop(unit)
    ;
        SpecialPredId = spec_pred_index,
        term_util.make_bool_list(HeadVars, [no, no], OutList),
        ArgSize = finite(0, OutList),
        Termination = cannot_loop(unit)
    ;
        SpecialPredId = spec_pred_init,
        unexpected($module, $pred, "initialise")
    ).

    % The list of proc_ids must refer to builtin predicates. This predicate
    % sets the termination information of builtin predicates.
    %
:- pred set_builtin_terminates(list(proc_id)::in, pred_id::in, pred_info::in,
    module_info::in, proc_table::in, proc_table::out) is det.

set_builtin_terminates([], _, _, _, !ProcTable).
set_builtin_terminates([ProcId | ProcIds], PredId, PredInfo, ModuleInfo,
        !ProcTable) :-
    map.lookup(!.ProcTable, ProcId, ProcInfo0),
    ( all_args_input_or_zero_size(ModuleInfo, PredInfo, ProcInfo0) ->
        % The size of the output arguments will all be 0, independent of the
        % size of the input variables.
        % UsedArgs should be set to yes([no, no, ...]).
        proc_info_get_headvars(ProcInfo0, HeadVars),
        term_util.make_bool_list(HeadVars, [], UsedArgs),
        ArgSizeInfo = yes(finite(0, UsedArgs))
    ;
        pred_info_get_context(PredInfo, Context),
        Error = is_builtin(PredId),
        ArgSizeError = termination_error_context(Error, Context),
        ArgSizeInfo = yes(infinite([ArgSizeError]))
    ),
    % XXX intermod
    proc_info_set_maybe_arg_size_info(ArgSizeInfo, ProcInfo0, ProcInfo1),
    proc_info_set_maybe_termination_info(yes(cannot_loop(unit)),
        ProcInfo1, ProcInfo),
    map.det_update(ProcId, ProcInfo, !ProcTable),
    set_builtin_terminates(ProcIds, PredId, PredInfo, ModuleInfo, !ProcTable).

%----------------------------------------------------------------------------%

    % This predicate sets the arg_size_info property of the given list of
    % procedures.
    %
    % change_procs_arg_size_info(ProcList, Override, TerminationInfo,
    %       ProcTable, ProcTable)
    %
    % If Override is yes, then this predicate overrides any existing arg_size
    % information. If Override is no, then it leaves the proc_info of a
    % procedure unchanged unless the proc_info had no arg_size information
    % (i.e. the maybe(arg_size_info) field was set to "no").
    %
:- pred change_procs_arg_size_info(list(proc_id)::in, bool::in,
    arg_size_info::in, proc_table::in, proc_table::out) is det.

change_procs_arg_size_info([], _, _, !ProcTable).
change_procs_arg_size_info([ProcId | ProcIds], Override, ArgSize,
        !ProcTable) :-
    map.lookup(!.ProcTable, ProcId, ProcInfo0),
    (
        (
            Override = yes
        ;
            proc_info_get_maybe_arg_size_info(ProcInfo0, no)
        )
    ->
        proc_info_set_maybe_arg_size_info(yes(ArgSize), ProcInfo0, ProcInfo),
        map.det_update(ProcId, ProcInfo, !ProcTable)
    ;
        true
    ),
    change_procs_arg_size_info(ProcIds, Override, ArgSize, !ProcTable).

    % change_procs_termination_info(ProcList, Override, TerminationInfo,
    %     !ProcTable):
    %
    % This predicate sets the termination_info property of the given list of
    % procedures.
    %
    % If Override is yes, then this predicate overrides any existing
    % termination information. If Override is no, then it leaves the proc_info
    % of a procedure unchanged unless the proc_info had no termination
    % information (i.e. the maybe(termination_info) field was set to "no").
    %
:- pred change_procs_termination_info(list(proc_id)::in, bool::in,
    termination_info::in, proc_table::in, proc_table::out) is det.

change_procs_termination_info([], _, _, !ProcTable).
change_procs_termination_info([ProcId | ProcIds], Override, Termination,
        !ProcTable) :-
    map.lookup(!.ProcTable, ProcId, ProcInfo0),
    (
        (
            Override = yes
        ;
            proc_info_get_maybe_termination_info(ProcInfo0, no)
        )
    ->
        % XXX intermod
        proc_info_set_maybe_termination_info(yes(Termination),
            ProcInfo0, ProcInfo),
        map.det_update(ProcId, ProcInfo, !ProcTable)
    ;
        true
    ),
    change_procs_termination_info(ProcIds, Override, Termination, !ProcTable).

%-----------------------------------------------------------------------------%

    % These predicates are used to add the termination_info pragmas to the
    % .opt and .trans_opt files.
    %
:- pred make_termination_opt_int(list(pred_id)::in, module_info::in,
    io::di, io::uo) is det.

make_termination_opt_int(PredIds, ModuleInfo, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(Globals, ModuleName, ".opt.tmp",
        do_not_create_dirs, OptFileName, !IO),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(Verbose, "% Appending termination_info pragmas to `",
        !IO),
    maybe_write_string(Verbose, OptFileName, !IO),
    maybe_write_string(Verbose, "'...", !IO),
    maybe_flush_output(Verbose, !IO),

    io.open_append(OptFileName, OptFileRes, !IO),
    (
        OptFileRes = ok(OptFile),
        io.set_output_stream(OptFile, OldStream, !IO),
        list.foldl(write_pred_termination_info(ModuleInfo), PredIds, !IO),
        io.set_output_stream(OldStream, _, !IO),
        io.close_output(OptFile, !IO),
        maybe_write_string(Verbose, " done.\n", !IO)
    ;
        OptFileRes = error(IOError),
        maybe_write_string(Verbose, " failed!\n", !IO),
        io.error_message(IOError, IOErrorMessage),
        io.write_strings(["Error opening file `",
            OptFileName, "' for output: ", IOErrorMessage], !IO),
        io.set_exit_status(1, !IO)
    ).

write_pred_termination_info(ModuleInfo, PredId, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_import_status(PredInfo, ImportStatus),
    module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),

    (
        (
            ImportStatus = status_exported
        ;
            ImportStatus = status_opt_exported
        ),
        \+ is_unify_or_compare_pred(PredInfo),

        % XXX These should be allowed, but the predicate declaration for
        % the specialized predicate is not produced before the termination
        % pragmas are read in, resulting in an undefined predicate error.
        \+ set.member(PredId, TypeSpecForcePreds)
    ->
        PredName = pred_info_name(PredInfo),
        ProcIds = pred_info_procids(PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        ModuleName = pred_info_module(PredInfo),
        pred_info_get_procedures(PredInfo, ProcTable),
        pred_info_get_context(PredInfo, Context),
        SymName = qualified(ModuleName, PredName),
        write_proc_termination_info(PredId, ProcIds, ProcTable,
            PredOrFunc, SymName, Context, !IO)
    ;
        true
    ).

:- pred write_proc_termination_info(pred_id::in, list(proc_id)::in,
    proc_table::in, pred_or_func::in, sym_name::in, prog_context::in,
    io::di, io::uo) is det.

write_proc_termination_info(_, [], _, _, _, _, !IO).
write_proc_termination_info(PredId, [ProcId | ProcIds], ProcTable,
        PredOrFunc, SymName, Context, !IO) :-
    map.lookup(ProcTable, ProcId, ProcInfo),
    proc_info_get_maybe_arg_size_info(ProcInfo, ArgSize),
    proc_info_get_maybe_termination_info(ProcInfo, Termination),
    proc_info_declared_argmodes(ProcInfo, ModeList),
    write_pragma_termination_info_components(PredOrFunc, SymName, ModeList,
        ArgSize, Termination, Context, !IO),
    write_proc_termination_info(PredId, ProcIds, ProcTable, PredOrFunc,
        SymName, Context, !IO).

%----------------------------------------------------------------------------%
:- end_module transform_hlds.termination.
%----------------------------------------------------------------------------%
