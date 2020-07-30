%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2001, 2003-2011 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
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
% download from the papers page of mercurylang.org.
%
% The termination analysis may use a number of different norms to calculate
% the size of a term. These are set by using the --termination-norm string
% option.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.termination.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Perform termination analysis on the module.
    %
:- pred analyse_termination_in_module(module_info::in, module_info::out,
    list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
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
:- import_module term.
:- import_module unit.

%-----------------------------------------------------------------------------%

analyse_termination_in_module(!ModuleInfo, !:Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_termination_norm(Globals, TermNorm),
    FunctorInfo = set_functor_info(!.ModuleInfo, TermNorm),
    globals.lookup_int_option(Globals, termination_error_limit, MaxErrors),
    globals.lookup_int_option(Globals, termination_path_limit, MaxPaths),
    PassInfo = pass_info(FunctorInfo, MaxErrors, MaxPaths),

    % Process builtin and compiler-generated predicates, and user-supplied
    % pragmas.
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    term_preprocess_preds(PredIds, !ModuleInfo),

    % Process all the SCCs of the call graph in a bottom-up order.
    module_info_ensure_dependency_info(!ModuleInfo, DepInfo),
    SCCs = dependency_info_get_bottom_up_sccs(DepInfo),

    % Set the termination status of foreign_procs based on the foreign code
    % attributes.
    !:Specs = [],
    check_foreign_code_attributes(SCCs, !ModuleInfo, !Specs),

    % Ensure that termination pragmas for a procedure do not conflict with
    % termination pragmas for other procedures in the SCC.
    check_pragmas_are_consistent(SCCs, !ModuleInfo, !Specs),

    list.foldl2(analyse_termination_in_scc(PassInfo), SCCs,
        !ModuleInfo, !Specs),
    run_post_term_analysis(!.ModuleInfo, PostSpecs),
    !:Specs = PostSpecs ++ !.Specs,

    module_info_get_proc_analysis_kinds(!.ModuleInfo, ProcAnalysisKinds0),
    set.insert(pak_termination, ProcAnalysisKinds0, ProcAnalysisKinds),
    module_info_set_proc_analysis_kinds(ProcAnalysisKinds, !ModuleInfo).

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

:- pred check_foreign_code_attributes(list(scc)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_foreign_code_attributes(SCCs, !ModuleInfo, !Specs) :-
    list.foldl2(check_foreign_code_attributes_in_scc, SCCs,
        !ModuleInfo, !Specs).

:- pred check_foreign_code_attributes_in_scc(scc::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_foreign_code_attributes_in_scc(SCC, !ModuleInfo, !Specs) :-
    set.to_sorted_list(SCC, PPIds),
    (
        PPIds = [],
        unexpected($pred, "empty SCC")
    ;
        PPIds = [PPId],
        module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, ProcInfo0),
        ( if
            proc_info_get_goal(ProcInfo0, Goal),
            Goal = hlds_goal(GoalExpr, _GoalInfo),
            GoalExpr = call_foreign_proc(Attributes, _, _, _, _, _, _)
        then
            check_foreign_code_attributes_of_proc(!.ModuleInfo, PPId,
                Attributes, ProcInfo0, ProcInfo, !Specs),
            module_info_set_pred_proc_info(PPId, PredInfo, ProcInfo,
                !ModuleInfo)
        else
            true
        )
    ;
        PPIds = [_, _ | _]
        % Foreign code proc cannot be mutually recursive.
    ).

:- pred check_foreign_code_attributes_of_proc(module_info::in,
    pred_proc_id::in, pragma_foreign_proc_attributes::in,
    proc_info::in, proc_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_foreign_code_attributes_of_proc(ModuleInfo, PPId, Attributes,
        !ProcInfo, !Specs) :-
    proc_info_get_maybe_termination_info(!.ProcInfo, MaybeTermination),
    proc_info_get_context(!.ProcInfo, Context),
    (
        MaybeTermination = no,
        ( if attributes_imply_termination(Attributes) then
            TermStatus = yes(cannot_loop(unit)),
            proc_info_set_maybe_termination_info(TermStatus, !ProcInfo)
        else
            TermErr = term_error(Context, does_not_term_foreign(PPId)),
            TermStatus = yes(can_loop([TermErr])),
            proc_info_set_maybe_termination_info(TermStatus, !ProcInfo)
        )
    ;
        % If there was a `:- pragma terminates' declaration for this procedure,
        % then check that the foreign code attributes do not contradict this.
        MaybeTermination = yes(cannot_loop(_)),
        ProcTerminates = get_terminates(Attributes),
        (
            ProcTerminates = proc_does_not_terminate,
            TermErr = term_error(Context, inconsistent_annotations),
            TermStatus = yes(can_loop([TermErr])),
            % XXX intermod
            proc_info_set_maybe_termination_info(TermStatus, !ProcInfo),
            ProcNamePieces = describe_one_proc_name(ModuleInfo,
                should_module_qualify, PPId),
            Pieces = [words("Warning:") | ProcNamePieces] ++ [words("has a"),
                pragma_decl("terminates"), words("declaration"),
                words("but also has the"), quote("does_not_terminate"),
                words("foreign code attribute set.")],
            Spec = simplest_spec($pred, severity_warning, phase_read_files,
                Context, Pieces),
            !:Specs = [Spec | !.Specs]
        ;
            ( ProcTerminates = proc_terminates
            ; ProcTerminates = depends_on_mercury_calls
            )
        )
    ;
        % If there was a `:- pragma does_not_terminate' declaration
        % for this procedure, then check that the foreign code attributes
        % do not contradict this.
        MaybeTermination = yes(can_loop(TermErrs0)),
        ProcTerminates = get_terminates(Attributes),
        (
            ProcTerminates = proc_terminates,
            TermErr = term_error(Context, inconsistent_annotations),
            TermErrs = [TermErr | TermErrs0],
            TermStatus = yes(can_loop(TermErrs)),
            % XXX intermod
            proc_info_set_maybe_termination_info(TermStatus, !ProcInfo),
            ProcNamePieces = describe_one_proc_name(ModuleInfo,
                should_module_qualify, PPId),
            Pieces = [words("Warning:") | ProcNamePieces] ++ [words("has a"),
                pragma_decl("does_not_terminate"), words("declaration"),
                words("but also has the"), quote("terminates"),
                words("foreign code attribute set.")],
            Spec = simplest_spec($pred, severity_warning, phase_read_files,
                Context, Pieces),
            !:Specs = [Spec | !.Specs]
        ;
            ( ProcTerminates = proc_does_not_terminate
            ; ProcTerminates = depends_on_mercury_calls
            )
        )
    ).

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

:- pred check_pragmas_are_consistent(list(scc)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_pragmas_are_consistent(SCCs, !ModuleInfo, !Specs) :-
    list.foldl2(check_scc_pragmas_are_consistent, SCCs, !ModuleInfo, !Specs).

:- pred check_scc_pragmas_are_consistent(scc::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_scc_pragmas_are_consistent(SCC, !ModuleInfo, !Specs) :-
    classify_termination_status(!.ModuleInfo,  set.to_sorted_list(SCC),
        set.init, KnownPredNamesIdsSet, set.init, KnownContextSet,
        set.init, KnownTermStatusSet, set.init, UnknownPPIdSet),
    KnownTermStatuses = set.to_sorted_list(KnownTermStatusSet),
    (
        KnownTermStatuses = []
        % We don't know the termination status of any procedure in the SCC.
    ;
        KnownTermStatuses = [KnownTermStatus],
        % We know the termination status of at least one procedure in the SCC,
        % and all the procedures that do have known termination statuses
        % have the same termination status.
        set.foldl(set_termination_info(KnownTermStatus),
            UnknownPPIdSet, !ModuleInfo)
    ;
        KnownTermStatuses = [_, _ | _],
        % We know the termination status of at least two procedures
        % in the SCC, and those procedures have two *different*
        % termination statuses.

        % Emit a warning, and then assume that they all loop.
        LeastContext = list.det_head(set.to_sorted_list(KnownContextSet)),
        NewTermStatus =
            can_loop([term_error(LeastContext, inconsistent_annotations)]),
        set.foldl(set_termination_info(NewTermStatus), SCC, !ModuleInfo),

        PredNamesIds = set.to_sorted_list(KnownPredNamesIdsSet),
        PredNamePieces = describe_several_pred_names(!.ModuleInfo,
            should_module_qualify, list.map(pair.snd, PredNamesIds)),
        Pieces =
            [words("Warning:") | PredNamePieces ] ++
            [words("are mutually recursive but some of their"),
            words( "termination pragmas are inconsistent.")],
        Spec = simplest_spec($pred, severity_warning, phase_read_files,
            LeastContext, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred classify_termination_status(module_info::in, list(pred_proc_id)::in,
    set(pair(string, pred_id))::in, set(pair(string, pred_id))::out, 
    set(prog_context)::in, set(prog_context)::out, 
    set(termination_info)::in, set(termination_info)::out,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

classify_termination_status(_, [],
        !KnownPredNamesIds, !KnownContexts,
        !KnownTermStatuses, !UnknownPPIds).
classify_termination_status(ModuleInfo, [PPId | PPIds],
        !KnownPredNamesIds, !KnownContexts,
        !KnownTermStatuses, !UnknownPPIds) :-
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, ProcInfo),
    proc_info_get_maybe_termination_info(ProcInfo, MaybeTermStatus),
    (
        MaybeTermStatus = yes(TermStatus),
        PredName = pred_info_name(PredInfo),
        PPId = proc(PredId, _ProcId),
        set.insert(PredName - PredId, !KnownPredNamesIds),
        % XXX We should be getting the context from the *Proc*Info.
        pred_info_get_context(PredInfo, Context),
        set.insert(Context, !KnownContexts),
        set.insert(TermStatus, !KnownTermStatuses)
    ;
        MaybeTermStatus = no,
        set.insert(PPId, !UnknownPPIds)
    ),
    classify_termination_status(ModuleInfo, PPIds,
        !KnownPredNamesIds, !KnownContexts,
        !KnownTermStatuses, !UnknownPPIds).

%-----------------------------------------------------------------------------%
%
% Run termination analysis on a single SCC.
%

    % For each SCC, we first find out the relationships among the sizes of the
    % arguments of the procedures of the SCC, and then attempt to prove
    % termination of the procedures.
    %
:- pred analyse_termination_in_scc(pass_info::in, scc::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

analyse_termination_in_scc(PassInfo, SCC, !ModuleInfo, !Specs) :-
    IsArgSizeKnown = (pred(PPId::in) is semidet :-
        module_info_pred_proc_info(!.ModuleInfo, PPId, _, ProcInfo),
        proc_info_get_maybe_arg_size_info(ProcInfo, yes(_))
    ),
    set.filter(IsArgSizeKnown, SCC, _SCCArgSizeKnown, SCCArgSizeUnknown),
    ( if set.is_empty(SCCArgSizeUnknown) then
        ArgSizeErrors = [],
        TermErrors = []
    else
        find_arg_sizes_in_scc(!.ModuleInfo, PassInfo, SCCArgSizeUnknown,
            ArgSizeResult, TermErrors),
        (
            ArgSizeResult = arg_size_ok(Solutions, OutputSupplierMap),
            set_finite_arg_size_infos(Solutions, OutputSupplierMap,
                !ModuleInfo),
            ArgSizeErrors = []
        ;
            ArgSizeResult = arg_size_error(Errors),
            set.foldl(set_infinite_arg_size_info(infinite(Errors)),
                SCCArgSizeUnknown, !ModuleInfo),
            ArgSizeErrors = Errors
        )
    ),
    set.filter(is_termination_known(!.ModuleInfo), SCC,
        _SCCTerminationKnown, SCCTerminationUnknown),
    ( if set.is_empty(SCCTerminationUnknown) then
        % We may possibly have encountered inconsistent
        % terminates/does_not_terminate pragmas for this SCC, so we need to
        % report errors here as well.
        % XXX That comment does not make sense here.
        true
    else
        IsFatal = (pred(Error::in) is semidet :-
            Error = term_error(_Context, ErrorKind),
            term_error_kind_is_fatal_error(ErrorKind) = yes
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
            prove_termination_in_scc(!.ModuleInfo, PassInfo,
                SCCTerminationUnknown, SingleArgs, TerminationResult)
        ),
        set.foldl(set_termination_info(TerminationResult),
            SCCTerminationUnknown, !ModuleInfo),
        (
            TerminationResult = can_loop(TerminationErrors),
            maybe_report_termination_errors(!.ModuleInfo, SCC,
                TerminationErrors, !Specs)
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
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo),
    map.lookup(OutputSupplierMap, PPId, OutputSuppliers),
    ArgSizeInfo = finite(Gamma, OutputSuppliers),
    % XXX intermod
    proc_info_set_maybe_arg_size_info(yes(ArgSizeInfo), ProcInfo, ProcInfo1),
    map.set(ProcId, ProcInfo1, ProcTable0, ProcTable),
    pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
    map.set(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo),
    set_finite_arg_size_infos(Solns, OutputSupplierMap, !ModuleInfo).

:- pred set_infinite_arg_size_info(arg_size_info::in, pred_proc_id::in,
    module_info::in, module_info::out) is det.

set_infinite_arg_size_info(ArgSizeInfo, PPId, !ModuleInfo) :-
    PPId = proc(PredId, ProcId),
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),
    % XXX intermod
    proc_info_set_maybe_arg_size_info(yes(ArgSizeInfo), ProcInfo0, ProcInfo),
    map.set(ProcId, ProcInfo, ProcTable0, ProcTable),
    pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
    map.set(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred set_termination_info(termination_info::in, pred_proc_id::in,
    module_info::in, module_info::out) is det.

set_termination_info(TerminationInfo, PPId, !ModuleInfo) :-
    PPId = proc(PredId, ProcId),
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),
    % XXX intermod
    proc_info_set_maybe_termination_info(yes(TerminationInfo),
        ProcInfo0, ProcInfo),
    map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
    pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred maybe_report_termination_errors(module_info::in, scc::in,
    list(term_error)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_report_termination_errors(ModuleInfo, SCC, Errors, !Specs) :-
    decide_what_term_errors_to_report(ModuleInfo, SCC, Errors,
        MaybeErrorsToReport),
    (
        MaybeErrorsToReport = no
    ;
        MaybeErrorsToReport = yes(ErrorsToReport),
        report_term_errors(ModuleInfo, SCC, ErrorsToReport, !Specs)
    ).

:- pred decide_what_term_errors_to_report(module_info::in, scc::in,
    list(term_error)::in, maybe(list(term_error))::out) is det.

decide_what_term_errors_to_report(ModuleInfo, SCC, Errors,
        MaybeErrorsToReport) :-
    % NOTE The code of this predicate follows the same logic as the
    % code of decide_what_term2_errors_to_report in term_constr_errors.m.
    % Although there are differences in the data types they operate on
    % and the options they consult, any changes here probably require
    % corresponding changes there as well.

    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, termination_check, NormalErrors),
    globals.lookup_bool_option(Globals, verbose_check_termination,
        VerboseErrors),
    ( if
        IsCheckTerm =
            ( pred(PPId::in) is semidet :-
                module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, _),
                not pred_info_is_imported(PredInfo),
                pred_info_get_markers(PredInfo, Markers),
                check_marker(Markers, marker_check_termination)
            ),
        set.filter(IsCheckTerm, SCC, CheckTermPPIds),
        set.is_non_empty(CheckTermPPIds)
    then
        % If any procedure in the SCC has a check_terminates pragma,
        % print out one error message for the whole SCC and indicate an error.
        MaybeErrorsToReport = yes(Errors)
    else if
        IsNonImported = (pred(PPId::in) is semidet :-
            module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, _),
            not pred_info_is_imported(PredInfo)
        ),
        set.filter(IsNonImported, SCC, NonImportedPPIds),
        set.is_non_empty(NonImportedPPIds)
    then
        (
            VerboseErrors = yes,
            % If verbose errors is enabled, then output both direct and
            % indirect errors.
            % (See term_errors.m for details of direct and indirect errors.)
            MaybeErrorsToReport = yes(Errors)
        ;
            VerboseErrors = no,
            (
                NormalErrors = yes,
                % We prefer reporting only the direct errors, but if there are
                % no direct errors, then output all the errors, which will then
                % all be indirect errors. This is better than giving no error
                % at all, which would lead to a message reporting that
                % termination could not be proven for "unknown reasons".
                IsDirect = (pred(Error::in) is semidet :-
                    Error = term_error(_, ErrorKind),
                    term_error_kind_is_direct_error(ErrorKind) = yes
                ),
                list.filter(IsDirect, Errors, DirectErrors),
                (
                    DirectErrors = [],
                    MaybeErrorsToReport = yes(Errors)
                ;
                    DirectErrors = [_ | _],
                    MaybeErrorsToReport = yes(DirectErrors)
                )
            ;
                NormalErrors = no,
                MaybeErrorsToReport = no
            )
        )
    else
        MaybeErrorsToReport = no
    ).

%----------------------------------------------------------------------------%

    % This predicate preprocesses each predicate and sets the termination
    % property if possible. This is done as follows.
    %
    % Set the termination to yes if:
    % - there is a terminates pragma defined for the predicate.
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
:- pred term_preprocess_preds(list(pred_id)::in,
    module_info::in, module_info::out) is det.

term_preprocess_preds([], !ModuleInfo).
term_preprocess_preds([PredId | PredIds], !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_op_mode(Globals, OpMode),
    ( if OpMode = opm_top_args(opma_augment(opmau_make_opt_int)) then
        MakeOptInt = yes
    else
        MakeOptInt = no
    ),
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_status(PredInfo0, PredStatus),
    pred_info_get_context(PredInfo0, Context),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    pred_info_get_markers(PredInfo0, Markers),
    ProcIds = pred_info_valid_procids(PredInfo0),
    ( if
        % It is possible for compiler generated/mercury builtin
        % predicates to be imported or locally defined, so they
        % must be covered here, separately.
        set_compiler_gen_terminates(PredInfo0, ProcIds, PredId,
            !.ModuleInfo, ProcTable0, ProcTable1)
    then
        ProcTable2 = ProcTable1
    else if
        pred_status_defined_in_this_module(PredStatus) = yes
    then
        % Since we cannot see their definition, we consider procedures
        % which have a `:- pragma external_{pred/func}' to be imported.
        ( if check_marker(Markers, marker_terminates) then
            change_procs_termination_info(ProcIds, yes, cannot_loop(unit),
                ProcTable0, ProcTable2)
        else
            ProcTable2 = ProcTable0
        )
    else
        % Not defined in this module.

        % All of the predicates that are processed in this section are imported
        % in some way. With imported predicates, any 'check_termination'
        % pragmas will be checked by the compiler when it compiles the
        % relevant source file (that the predicate was imported from).
        % When making the intermodule optimizations, the check_termination
        % will not be checked when the relevant source file is compiled,
        % so it cannot be depended upon.
        ( if
            (
                check_marker(Markers, marker_terminates)
            ;
                MakeOptInt = no,
                check_marker(Markers, marker_check_termination)
            )
        then
            change_procs_termination_info(ProcIds, yes, cannot_loop(unit),
                ProcTable0, ProcTable1)
        else
            TerminationError = term_error(Context, imported_pred),
            TerminationInfo = can_loop([TerminationError]),
            change_procs_termination_info(ProcIds, no, TerminationInfo,
                ProcTable0, ProcTable1)
        ),
        ArgSizeError = term_error(Context, imported_pred),
        ArgSizeInfo = infinite([ArgSizeError]),
        change_procs_arg_size_info(ProcIds, no, ArgSizeInfo,
            ProcTable1, ProcTable2)
    ),
    ( if check_marker(Markers, marker_does_not_terminate) then
        RequestError = term_error(Context, does_not_term_pragma(PredId)),
        RequestTerminationInfo = can_loop([RequestError]),
        change_procs_termination_info(ProcIds, yes,
            RequestTerminationInfo, ProcTable2, ProcTable)
    else
        ProcTable = ProcTable2
    ),
    pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
    map.set(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo),
    term_preprocess_preds(PredIds, !ModuleInfo).

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
    ( if
        pred_info_is_builtin(PredInfo)
    then
        set_builtin_terminates(ProcIds, PredId, PredInfo, ModuleInfo,
            !ProcTable)
    else if
        ( if
            ModuleName = pred_info_module(PredInfo),
            Name = pred_info_name(PredInfo),
            Arity = pred_info_orig_arity(PredInfo),
            special_pred_name_arity(SpecPredId0, Name, _, Arity),
            any_mercury_builtin_module(ModuleName)
        then
            SpecialPredId = SpecPredId0
        else
            pred_info_get_origin(PredInfo, Origin),
            Origin = origin_special_pred(SpecialPredId, _)
        )
    then
        set_generated_terminates(ProcIds, SpecialPredId, !ProcTable)
    else
        fail
    ).

:- pred set_generated_terminates(list(proc_id)::in, special_pred_id::in,
    proc_table::in, proc_table::out) is det.

set_generated_terminates([], _, !ProcTable).
set_generated_terminates([ProcId | ProcIds], SpecialPredId, !ProcTable) :-
    map.lookup(!.ProcTable, ProcId, ProcInfo0),
    proc_info_get_headvars(ProcInfo0, HeadVars),
    special_pred_id_to_termination(SpecialPredId, HeadVars,
        ArgSize, Termination),
    proc_info_set_maybe_arg_size_info(yes(ArgSize), ProcInfo0, ProcInfo1),
    proc_info_set_maybe_termination_info(yes(Termination),
        ProcInfo1, ProcInfo),
    map.det_update(ProcId, ProcInfo, !ProcTable),
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
    ( if all_args_input_or_zero_size(ModuleInfo, PredInfo, ProcInfo0) then
        % The size of the output arguments will all be 0, independent of the
        % size of the input variables.
        % UsedArgs should be set to yes([no, no, ...]).
        proc_info_get_headvars(ProcInfo0, HeadVars),
        term_util.make_bool_list(HeadVars, [], UsedArgs),
        ArgSizeInfo = yes(finite(0, UsedArgs))
    else
        pred_info_get_context(PredInfo, Context),
        Error = is_builtin(PredId),
        ArgSizeError = term_error(Context, Error),
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
    ( if
        ( Override = yes
        ; proc_info_get_maybe_arg_size_info(ProcInfo0, no)
        )
    then
        proc_info_set_maybe_arg_size_info(yes(ArgSize), ProcInfo0, ProcInfo),
        map.det_update(ProcId, ProcInfo, !ProcTable)
    else
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
    ( if
        (
            Override = yes
        ;
            proc_info_get_maybe_termination_info(ProcInfo0, no)
        )
    then
        % XXX intermod
        proc_info_set_maybe_termination_info(yes(Termination),
            ProcInfo0, ProcInfo),
        map.det_update(ProcId, ProcInfo, !ProcTable)
    else
        true
    ),
    change_procs_termination_info(ProcIds, Override, Termination, !ProcTable).

%-----------------------------------------------------------------------------%

%----------------------------------------------------------------------------%
:- end_module transform_hlds.termination.
%----------------------------------------------------------------------------%
