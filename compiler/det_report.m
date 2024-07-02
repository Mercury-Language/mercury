%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: det_report.m.
% Author: zs.
%
% This module handles reporting of determinism errors and warnings,
% as well as errors and warnings from some other related compiler passes
% such as simplify.
%
%---------------------------------------------------------------------------%

:- module check_hlds.det_report.
:- interface.

:- import_module check_hlds.det_util.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_sort.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- type seen_call_id
    --->    seen_call(pred_id, proc_id)
    ;       higher_order_call.

:- type cc_unify_context
    --->    ccuc_unify(unify_context)
    ;       ccuc_switch.

:- type failing_context
    --->    failing_context(
                prog_context,
                failing_goal
            ).

:- type failing_goal
    --->    incomplete_switch(prog_var)
    ;       fail_goal
    ;       test_goal(prog_var, prog_var)
    ;       deconstruct_goal(prog_var, cons_id)
    ;       call_goal(pred_id, proc_id)
    ;       generic_call_goal(generic_call)
    ;       negated_goal.

    % XXX if this declaration is left out the compiler incorrectly accepts the
    % code, but an exception is thrown when --smart-recompilation is used.
    %
:- type switch_context.

%---------------------------------------------------------------------------%

    % Check the determinism declarations of the specified procedures,
    % which should be all the valid procedures in this module.
    %
    % This is the main predicate exported by this module.
    %
:- pred check_determinism_of_procs(io.text_output_stream::in,
    list(pred_proc_id)::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % As above, but for just a single procedure. Used when a change in
    % the body of a procedure (by e.g. cse_detection.m) requires redoing
    % the procedure's mode- and determinism-analysis, in order to update
    % the relevant fields of hlds_goal_infos.
    %
:- pred check_determinism_of_proc(io.text_output_stream::in,
    pred_proc_id::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Check the determinism declarations of the specified procedures
    % whose bodies det_analysis.m did not analyze because they are
    % imported from another module (and whose bodies are therefore
    % not available, at least not without intermodule optimization).
    % However, we can still perform some checks on the mode declarations
    % themselves.
    %
    % Note that any errors with those declarations should also be caught
    % when the module that we are importing the declaration from is compiled.
    % We do the checks we do here to prevent later passes of *this* compiler
    % invocation being given data that violates our rules of static semantics.
    % (See tests/invalid_manual/gh118.m for an example.)
    %
:- pred check_determinism_of_imported_procs(io.text_output_stream::in,
    module_info::in, list(pred_proc_id)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Check a lambda goal with the specified declared and inferred
    % determinisms.
    %
:- pred det_check_lambda(determinism::in, determinism::in, hlds_goal::in,
    hlds_goal_info::in, instmap::in, det_info::in, det_info::out) is det.

    % det_diagnose_conj(Goals, InstMap0, Desired, FailingContexts, !DetInfo,
    %   Msgs):
    %
    % The conjunction Goals with initial instmap InstMap0 should have
    % determinism Desired, but doesn't. Find out what is wrong, and return
    % a list of messages giving the causes.
    %
    % det_diagnose_conj is used for both normal [sequential] conjunctions
    % and parallel conjunctions.
    %
:- pred det_diagnose_conj(list(hlds_goal)::in, instmap::in, determinism::in,
    list(switch_context)::in, det_info::in, det_info::out,
    list(error_msg_group)::out) is det.

    % Return a printable representation of the given promise_solutions_kind.
    %
:- func promise_solutions_kind_str(promise_solutions_kind) = string.

    % Describe the given list of failing contexts.
    %
:- func failing_contexts_description(module_info, var_table,
    list(failing_context)) = list(error_msg).

    % Describe a call we have seen.
    %
:- func det_report_seen_call_id(module_info, seen_call_id)
    = list(format_piece).

%---------------------------------------------------------------------------%

:- type options_to_restore.

    % Call this predicate before rerunning determinism analysis after an
    % optimization pass to disable all warnings. Errors will still be reported.
    %
:- pred disable_det_warnings(options_to_restore::out,
    globals::in, globals::out) is det.

:- pred restore_det_warnings(options_to_restore::in,
    globals::in, globals::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.mode_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.pred_name.
:- import_module hlds.status.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.write_error_spec.

:- import_module assoc_list.
:- import_module bag.
:- import_module bool.
:- import_module cord.
:- import_module getopt.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

check_determinism_of_procs(_, [], !ModuleInfo, !Specs).
check_determinism_of_procs(ProgressStream, [PredProcId | PredProcIds],
        !ModuleInfo, !Specs) :-
    check_determinism_of_proc(ProgressStream, PredProcId, !ModuleInfo, !Specs),
    check_determinism_of_procs(ProgressStream, PredProcIds,
        !ModuleInfo, !Specs).

check_determinism_of_proc(ProgressStream, PredProcId, !ModuleInfo, !Specs) :-
    module_info_pred_proc_info(!.ModuleInfo, PredProcId, PredInfo, ProcInfo),

    trace [compiletime(flag("debug-check-detism-progress")), io(!IO)] (
        proc_info_get_argmodes(ProcInfo, PredArgModes),
        proc_info_get_inst_varset(ProcInfo, InstVarSet),
        PredProcId = proc(PredId, _ProcId),
        PredModePieces = describe_one_pred_name_mode(!.ModuleInfo,
            output_mercury, InstVarSet, no, should_not_module_qualify, [],
            PredId, PredArgModes),
        PredStr = error_pieces_to_one_line_string(PredModePieces),
        io.format(ProgressStream, "check_determinism_of_proc %s\n",
            [s(PredStr)], !IO)
    ),

    % Report any mismatches between the declared and the actual determinisms
    % of the procedure.
    check_for_too_tight_or_loose_declared_determinism(PredProcId,
        PredInfo, ProcInfo, !ModuleInfo, !Specs),

    % Some determinisms are invalid for some kinds of predicates.
    % Check for and report any violations of these rules.
    make_reqscope_checks_if_needed(!.ModuleInfo, PredProcId,
        PredInfo, ProcInfo, !Specs),
    check_determinism_for_eval_method(ProcInfo, !Specs),
    check_determinism_if_pred_is_main(PredInfo, ProcInfo, !Specs),
    check_for_multisoln_func(!.ModuleInfo, PredProcId, PredInfo, ProcInfo,
        !Specs),
    check_io_state_proc_detism(!.ModuleInfo, PredProcId, PredInfo, ProcInfo,
        !Specs),
    check_exported_proc_detism(PredProcId, ProcInfo, !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%

check_determinism_of_imported_procs(_, _, [], !Specs).
check_determinism_of_imported_procs(ProgressStream, ModuleInfo,
        [PredProcId | PredProcIds], !Specs) :-
    check_determinism_of_imported_proc(ProgressStream, ModuleInfo,
        PredProcId, !Specs),
    check_determinism_of_imported_procs(ProgressStream, ModuleInfo,
        PredProcIds, !Specs).

:- pred check_determinism_of_imported_proc(io.text_output_stream::in,
    module_info::in, pred_proc_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_determinism_of_imported_proc(ProgressStream, ModuleInfo, PredProcId,
        !Specs) :-
    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo),

    trace [compiletime(flag("debug-check-detism-progress")), io(!IO)] (
        proc_info_get_argmodes(ProcInfo, PredArgModes),
        proc_info_get_inst_varset(ProcInfo, InstVarSet),
        PredProcId = proc(PredId, _ProcId),
        PredModePieces = describe_one_pred_name_mode(ModuleInfo,
            output_mercury, InstVarSet, no, should_not_module_qualify, [],
            PredId, PredArgModes),
        PredStr = error_pieces_to_one_line_string(PredModePieces),
        io.format(ProgressStream, "check_determinism_of_imported_proc %s\n",
            [s(PredStr)], !IO)
    ),

    check_for_multisoln_func(ModuleInfo, PredProcId, PredInfo, ProcInfo,
        !Specs),
    check_io_state_proc_detism(ModuleInfo, PredProcId, PredInfo, ProcInfo,
        !Specs).

%---------------------------------------------------------------------------%

:- pred check_for_too_tight_or_loose_declared_determinism(
    pred_proc_id::in, pred_info::in, proc_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_too_tight_or_loose_declared_determinism(PredProcId,
        PredInfo, ProcInfo, !ModuleInfo, !Specs) :-
    proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
    proc_info_get_inferred_determinism(ProcInfo, InferredDetism),
    (
        MaybeDetism = no
    ;
        MaybeDetism = yes(DeclaredDetism),
        compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
        (
            Cmp = first_detism_same_as
        ;
            Cmp = first_detism_looser_than,
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, warn_det_decls_too_lax,
                ShouldIssueWarning),
            globals.lookup_bool_option(Globals, warn_inferred_erroneous,
                WarnAboutInferredErroneous),
            pred_info_get_markers(PredInfo, Markers),
            ( if
                ShouldIssueWarning = yes,

                % Don't report warnings for class method implementations --
                % the determinism in the `:- typeclass' declaration will be
                % the loosest of all possible instances. This is similar to
                % the reason we don't report warnings for lambda expressions.
                not check_marker(Markers, marker_class_instance_method),

                % Don't report warnings for procedures with no clauses.
                not check_marker(Markers, marker_stub),

                % Don't report warnings for predicates for which the user
                % has written a pragma requesting no warnings.
                not check_marker(Markers, marker_no_detism_warning),

                % Don't report warnings for compiler-generated Unify, Compare
                % or Index procedures, since the user has no way to shut
                % these up. These can happen for the Unify pred for the unit
                % type, if such types are not boxed (as they are not
                % boxed for the IL backend).
                not is_unify_index_or_compare_pred(PredInfo),

                % Don't warn about predicates which are inferred erroneous
                % when the appropriate option is set. This is to avoid warnings
                % about unimplemented predicates.
                (
                    WarnAboutInferredErroneous = yes
                ;
                    WarnAboutInferredErroneous = no,
                    InferredDetism \= detism_erroneous
                ),

                % Only warn about predicates that are defined in this module.
                % This avoids warnings being emitted for opt_imported
                % predicates.
                pred_info_get_status(PredInfo, Status),
                pred_status_defined_in_this_module(Status) = yes
            then
                % The returned ReportMsg will contain the actual determinism
                % colored as incorrect. XXX Is this ok for a warning?
                report_determinism_problem(!.ModuleInfo, PredProcId,
                    "Warning", "could be tighter", [],
                    DeclaredDetism, InferredDetism, ReportMsg),
                ReportSpec = error_spec($pred, severity_warning,
                    phase_detism_check, [ReportMsg]),
                !:Specs = [ReportSpec | !.Specs]
            else
                true
            )
        ;
            ( Cmp = first_detism_tighter_than
            ; Cmp = first_detism_incomparable
            ),
            proc_info_get_goal(ProcInfo, Goal),
            proc_info_get_var_table(ProcInfo, VarTable),
            proc_info_get_initial_instmap(!.ModuleInfo, ProcInfo, InstMap0),
            det_info_init(!.ModuleInfo, PredProcId, VarTable,
                pess_extra_vars_report, [], DetInfo0),
            det_diagnose_goal_get_msgs(Goal, InstMap0, DeclaredDetism, [],
                DetInfo0, DetInfo, GoalMsgs),
            det_info_get_module_info(DetInfo, !:ModuleInfo),
            cse_nopull_msgs(ProcInfo, CseMsgs),
            DetailMsgs = GoalMsgs ++ CseMsgs,
            (
                DetailMsgs = [],
                ReasonPieces = []
            ;
                DetailMsgs = [_],
                ReasonPieces = [words("The reason for the difference"),
                    words("is the following."), nl]
            ;
                DetailMsgs = [_, _ | _],
                ReasonPieces = [words("The reasons for the difference"),
                    words("are the following."), nl]
            ),
            report_determinism_problem(!.ModuleInfo, PredProcId,
                "Error", "is not satisfied", ReasonPieces,
                DeclaredDetism, InferredDetism, ReportMsg),
            ReportSpec = error_spec($pred, severity_error, phase_detism_check,
                [ReportMsg | DetailMsgs]),
            !:Specs = [ReportSpec | !.Specs]
        )
    ).

:- pred cse_nopull_msgs(proc_info::in, list(error_msg)::out) is det.

cse_nopull_msgs(ProcInfo, Msgs) :-
    proc_info_get_cse_nopull_contexts(ProcInfo, CseNoPullContexts),
    list.sort(CseNoPullContexts, SortedCseNoPullContexts),
    (
        SortedCseNoPullContexts = [],
        Msgs = []
    ;
        SortedCseNoPullContexts = [FirstNoPullContext | _],
        Msgs = [msg(FirstNoPullContext, cse_nopull_pieces)]
    ).

:- func cse_nopull_pieces = list(format_piece).

cse_nopull_pieces =
    [words("It is possible that"),
    words("the cause of the declared determinism not being satisfied"),
    words("is the inability of determinism analysis to recognize that"),
    words("a disjunction (usually created by the compiler for a switch arm)"),
    words("is a switch on a *subterm* of a variable"),
    words("when the instantiation state of that variable"),
    words("is at least partially unique."),
    words("This is because converting such a disjunction to a switch"),
    words("requires replacing several unifications,"),
    words("one in each arm of the disjunction,"),
    words("that each unify the variable representing the subterm"),
    words("(e.g. the tail of a list) with the same function symbol,"),
    words("with just one unification before the disjunction,"),
    words("but due to limitations of the current modechecker,"),
    words("this transformation could destroy the uniqueness."), nl,
    words("In cases where this uniqueness is not needed,"),
    words("the programmer can fix the determinism error"),
    words("by performing this transformation manually."), nl].

%---------------------------------------------------------------------------%

:- pred make_reqscope_checks_if_needed(module_info::in,
    pred_proc_id::in, pred_info::in, proc_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

make_reqscope_checks_if_needed(ModuleInfo, PredProcId, PredInfo, ProcInfo,
        !Specs) :-
    pred_info_get_markers(PredInfo, Markers),
    ( if
        ( if
            check_marker(Markers, marker_has_incomplete_switch),
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, inform_incomplete_switch, yes)
        then
            % If this option is specified, it acts as an implicit
            % require_complete_switch scope around all switches.
            InformIncompleteSwitches = inform_incomplete_switches
        else if
            check_marker(Markers, marker_has_require_scope)
        then
            InformIncompleteSwitches = do_not_inform_incomplete_switches
        else
            fail
        )
    then
        proc_info_get_goal(ProcInfo, Goal),
        proc_info_get_var_table(ProcInfo, VarTable),
        proc_info_get_initial_instmap(ModuleInfo, ProcInfo, InstMap0),
        det_info_init(ModuleInfo, PredProcId, VarTable,
            pess_extra_vars_ignore, [], DetInfo0),
        reqscope_check_goal(Goal, InstMap0, InformIncompleteSwitches, no,
            [], DetInfo0, DetInfo),
        det_info_get_error_specs(DetInfo, RCSSpecs),
        !:Specs = RCSSpecs ++ !.Specs
    else
        true
    ).

:- func detism_decl_name(detism_decl) = string.

detism_decl_name(DetismDecl) = Name :-
    (
        DetismDecl = detism_decl_explicit,
        Name = "determinism declaration"
    ;
        DetismDecl = detism_decl_implicit,
        Name = "implicit determinism declaration"
    ;
        DetismDecl = detism_decl_none,
        Name = "nonexistent determinism declaration"
        % This shouldn't happen, but if it does, it is better to get an
        % error message that puts you on the right track than to get
        % a compiler abort.
        % unexpected($pred, "detism_decl_name: detism_decl_none")
    ).

:- pred get_valid_determinisms(tabled_eval_method::in, determinism::out)
    is nondet.

get_valid_determinisms(TabledMethod, Detism) :-
    determinism(Detism),
    valid_determinism_for_tabled_eval_method(TabledMethod, Detism) = yes.

    % Generate all the possible determinisms.
    %
:- pred determinism(determinism).
:- mode determinism(out) is multi.
:- mode determinism(in) is det.         % To ensure we don't forget any.

determinism(detism_det).
determinism(detism_semi).
determinism(detism_multi).
determinism(detism_non).
determinism(detism_cc_multi).
determinism(detism_cc_non).
determinism(detism_erroneous).
determinism(detism_failure).

%---------------------------------------------------------------------------%

:- pred check_determinism_for_eval_method(proc_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_determinism_for_eval_method(ProcInfo, !Specs) :-
    proc_info_get_inferred_determinism(ProcInfo, InferredDetism),
    proc_info_get_eval_method(ProcInfo, EvalMethod),
    ( if
        EvalMethod = eval_tabled(TabledMethod),
        valid_determinism_for_tabled_eval_method(TabledMethod, InferredDetism)
            = no
    then
        PragmaName = tabled_eval_method_to_pragma_name(TabledMethod),
        InferredDetismStr = determinism_to_string(InferredDetism),
        proc_info_get_context(ProcInfo, Context),
        MainPieces = [words("Error:")] ++
            color_as_subject([pragma_decl(PragmaName),
                words("declarations")]) ++
            [words("are")] ++
            color_as_incorrect([words("not allowed")]) ++
            [words("for procedures with determinism")] ++
            color_as_incorrect([quote(InferredDetismStr), suffix(".")]) ++
            [nl],

        solutions.solutions(get_valid_determinisms(TabledMethod), Detisms),
        DetismStrs = list.map(determinism_to_string, Detisms),
        list.sort(DetismStrs, SortedDetismStrs),
        DetismPieces = fixed_list_to_color_pieces(color_correct, "and",
            [suffix(".")], SortedDetismStrs),
        VerbosePieces =
            [words("This pragma is valid only for the following"),
            words(choose_number(Detisms, "determinism", "determinisms")),
            suffix(":") | DetismPieces] ++ [nl],
        ValidSpec = error_spec($pred, severity_error, phase_detism_check,
            [simple_msg(Context,
                [always(MainPieces),
                verbose_only(verbose_always, VerbosePieces)])]),
        !:Specs = [ValidSpec | !.Specs]
    else
        true
    ).

    % Check if the given evaluation method is allowed with
    % the given determinism.
    %
:- func valid_determinism_for_tabled_eval_method(tabled_eval_method,
    determinism) = bool.

valid_determinism_for_tabled_eval_method(TabledMethod, Detism) = Valid :-
    (
        ( TabledMethod = tabled_loop_check
        ; TabledMethod = tabled_memo(_)
        ),
        determinism_components(Detism, _, MaxSoln),
        (
            MaxSoln = at_most_zero,
            Valid = no
        ;
            ( MaxSoln = at_most_one
            ; MaxSoln = at_most_many
            ; MaxSoln = at_most_many_cc
            ),
            Valid = yes
        )
    ;
        TabledMethod = tabled_io(_, _),
        unexpected($pred, "called after tabling phase")
    ;
        TabledMethod = tabled_minimal(_),
        % The following reasons specify why a particular determinism is
        % incompatible with minimal model tabling.
        %
        % Reason 1:
        % Determinism analysis isn't yet smart enough to know whether
        % a cannot_fail execution path is guaranteed not to go through a call
        % to a predicate that is mutually recursive with this one, which
        % (if this predicate is minimal model) is the only way that the
        % predicate can be properly cannot_fail. The problem is that
        % in general, the mutually recursive predicate may be in
        % another module.
        %
        % Reason 2:
        % The transformation, as currently implemented, assumes that
        % it is possible to reach the call table tip, and generates HLDS
        % that refers to the introduced variable representing this tip.
        % This variable however, will be optimized away if the code
        % cannot succeed, causing a code generator abort.
        %
        % Reason 3:
        % Minimal model semantics requires computing to a fixpoint, and
        % this is incompatible with the notion of committed choice.
        %
        % Reason 4:
        % Doing the analysis required to ensure that a predicate can't have
        % more than one solution is much harder if the predicate concerned is
        % minimal_model. In theory, this analysis could be done, but it would
        % take a lot of programming, and since there is a simple workaround
        % (make the predicate nondet, and check the number of solutions at the
        % caller), this would not be cost-effective.
        (
            Detism = detism_det,
            Valid = no                  % Reason 1
        ;
            Detism = detism_semi,
            Valid = no                  % Reason 4
        ;
            Detism = detism_multi,      % Reason 1
            Valid = yes
        ;
            Detism = detism_non,
            Valid = yes
        ;
            Detism = detism_cc_multi,   % Reason 3
            Valid = no
        ;
            Detism = detism_cc_non,     % Reason 3
            Valid = no
        ;
            Detism = detism_erroneous,  % Reason 2
            Valid = no
        ;
            Detism = detism_failure,    % Reason 2
            Valid = no
        )
    ).

%---------------------------------------------------------------------------%

:- pred check_determinism_if_pred_is_main(pred_info::in, proc_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_determinism_if_pred_is_main(PredInfo, ProcInfo, !Specs) :-
    % Check that `main/2' has determinism `det' or `cc_multi',
    % as required by the language reference manual.
    proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
    ( if
        pred_info_name(PredInfo) = "main",
        pred_info_pred_form_arity(PredInfo) = pred_form_arity(2)
    then
        MainSNA = sym_name_arity(unqualified("main"), 2),
        MainSNAPiece = unqual_sym_name_arity(MainSNA),
        proc_info_get_context(ProcInfo, ProcContext),
        ( if pred_info_is_exported(PredInfo) then
            true
        else
            ExportPieces =
                [words("Error:")] ++ color_as_subject([MainSNAPiece]) ++
                [words("must be")] ++ color_as_correct([words("exported,")]) ++
                [words("but")] ++ color_as_incorrect([words("it is not.")]) ++
                [nl],
            ExportSpec = spec($pred, severity_error, phase_detism_check,
                ProcContext, ExportPieces),
            !:Specs = [ExportSpec | !.Specs]
        ),
        (
            MaybeDetism = no,
            DetismPieces =
                [words("Error:")] ++ color_as_subject([MainSNAPiece]) ++
                [words("must have a")] ++
                color_as_correct([words("declared determinism,")]) ++
                [words("but")] ++ color_as_correct([words("it does not.")]) ++
                [nl],
            DetismSpec = spec($pred, severity_error, phase_detism_check,
                ProcContext, DetismPieces),
            !:Specs = [DetismSpec | !.Specs]
        ;
            MaybeDetism = yes(DeclaredDetism),
            (
                ( DeclaredDetism = detism_det
                ; DeclaredDetism = detism_cc_multi
                )
            ;
                ( DeclaredDetism = detism_semi
                ; DeclaredDetism = detism_multi
                ; DeclaredDetism = detism_non
                ; DeclaredDetism = detism_cc_non
                ; DeclaredDetism = detism_erroneous
                ; DeclaredDetism = detism_failure
                ),
                DetismStr = determinism_to_string(DeclaredDetism),
                DetismPieces =
                    [words("Error:")] ++ color_as_subject([MainSNAPiece]) ++
                    [words("must be either")] ++
                    color_as_correct([quote("det")]) ++ [words("or")] ++
                    color_as_correct([quote("cc_multi"), suffix(";")]) ++
                    [words("it may not be")] ++
                    color_as_incorrect([quote(DetismStr), suffix(".")]) ++
                    [nl],
                DetismSpec = spec($pred, severity_error, phase_detism_check,
                    ProcContext, DetismPieces),
                !:Specs = [DetismSpec | !.Specs]
            )
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred check_for_multisoln_func(module_info::in, pred_proc_id::in,
    pred_info::in, proc_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_multisoln_func(ModuleInfo, PredProcId, PredInfo, ProcInfo, !Specs) :-
    proc_info_get_inferred_determinism(ProcInfo, InferredDetism),

    % Functions can only have more than one solution if it is a non-standard
    % mode. Otherwise, they would not be referentially transparent.
    % (Nondeterministic "functions" like C's `rand()' function are not
    % allowed.)
    ( if
        % If it is a mode for a function...
        pred_info_is_pred_or_func(PredInfo) = pf_function,
        % ... that can succeed more than once ...
        determinism_components(InferredDetism, _CanFail, NumSolns),
        NumSolns \= at_most_zero,
        NumSolns \= at_most_one,
        % ... but for which all the arguments are input ...
        proc_info_get_argmodes(ProcInfo, PredArgModes),
        pred_info_get_arg_types(PredInfo, PredArgTypes),
        pred_args_to_func_args(PredArgTypes, FuncArgTypes, _FuncResultType),
        pred_args_to_func_args(PredArgModes, FuncArgModes, _FuncResultMode),
        all_modes_are_fully_input(ModuleInfo, FuncArgTypes, FuncArgModes)
    then
        % ... then it is an error.
        proc_info_get_context(ProcInfo, FuncContext),
        proc_info_get_inst_varset(ProcInfo, InstVarSet),
        PredProcId = proc(PredId, _ProcId),
        PredModePieces = describe_one_pred_name_mode(ModuleInfo,
            output_mercury, InstVarSet, no, should_not_module_qualify, [],
            PredId, PredArgModes),
        InferredDetismStr = mercury_det_to_string(InferredDetism),
        MainPieces = [words("Error: invalid determinism for")] ++
            color_as_subject(PredModePieces ++ [suffix(":")]) ++ [nl,
            words("the primary mode of a function cannot be")] ++
            color_as_incorrect([quote((InferredDetismStr)), suffix(".")]) ++
            [nl],
        VerbosePieces = func_primary_mode_det_msg,
        Spec = error_spec($pred, severity_error, phase_detism_check,
            [simple_msg(FuncContext,
                [always(MainPieces),
                verbose_only(verbose_once, VerbosePieces)])]),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

:- func func_primary_mode_det_msg = list(format_piece).

func_primary_mode_det_msg = [words("In Mercury,"),
    words("a function is supposed to be a true mathematical function"),
    words("of its arguments; that is, the value of the function's result"),
    words("should be determined only by the values of its arguments."),
    words("(Allowing functions to have more than one result for the same"),
    words("arguments would break referential transparency.)"),
    words("Most likely, this procedure should be a predicate,"),
    words("not a function."), nl].

%---------------------------------------------------------------------------%

:- pred check_io_state_proc_detism(module_info::in, pred_proc_id::in,
    pred_info::in, proc_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_io_state_proc_detism(ModuleInfo, PredProcId, PredInfo, ProcInfo,
        !Specs) :-
    ( if
        proc_info_has_io_state_pair(ModuleInfo, ProcInfo, _InArg, _OutArg),
        proc_info_get_inferred_determinism(ProcInfo, ActualDetism),
        proc_info_get_declared_determinism(ProcInfo, MaybeDeclaredDetism),
        is_detism_ok_for_io(ActualDetism) = no,
        (
            MaybeDeclaredDetism = no,
            DetismToReport = ActualDetism
        ;
            MaybeDeclaredDetism = yes(DeclaredDetism),
            DeclaredDetismOk = is_detism_ok_for_io(DeclaredDetism),
            (
                DeclaredDetismOk = yes,
                % Do not generate the message below. The actual error
                % the programmer should fix is whatever issue is causing
                % the mismatch between DeclaredDetism and ActualDetism;
                % a reminder about ActualDetism not being allowed with I/O
                % would just be a nuisance, rather than being helpful.
                fail
            ;
                DeclaredDetismOk = no,
                % In this case, the programmer has the problems that
                % even the declared detism is not allowed with I/O.
                % This is what we report here. If the actual and declared
                % determinisms differ, that is a *different* problem,
                % which would get its own separate error message.
                DetismToReport = DeclaredDetism
            )
        )
    then
        pred_info_get_module_name(PredInfo, PredModuleName),
        module_info_get_name(ModuleInfo, ModuleName),
        % Almost all error messages this predicate will generate will refer
        % to a procedure that is local to the module being compiled,
        % and for these, printing the module name would only be clutter.
        % However, in rare cases such as the tests/invalid_manual/gh118
        % test case, we may need to generate a message about an imported
        % predicate, and for this case, the name of the module containing
        % the predicate that we complain about is crucial information.
        ( if ModuleName = PredModuleName then
            ShouldModuleQual = should_not_module_qualify
        else
            ShouldModuleQual = should_module_qualify
        ),
        ProcColonPieces = describe_one_proc_name_mode(ModuleInfo,
            output_mercury, yes(color_subject), ShouldModuleQual,
            [suffix(":")], PredProcId),
        % We used to print the second sentence (actually, only its first half)
        % if verbose error messages were enabled, but anyone who makes this
        % error will probably need that extra help, so don't make them
        % ask for it with -E.
        BadDetismPieces0 = [quote(determinism_to_string(DetismToReport))],
        BadDetismPieces = color_as_incorrect(BadDetismPieces0),
        GoodDetismPieces = color_as_correct([quote("det"), suffix(",")]) ++
            color_as_correct([quote("cc_multi")]) ++ [words("and")] ++
            color_as_correct([quote("erroneous"), suffix(",")]),
        Pieces = [words("In")] ++ ProcColonPieces ++ [nl,
            words("error:")] ++ BadDetismPieces ++
            [words("is not a valid determinism"),
            words("for a predicate that has I/O state arguments."),
            words("The valid determinisms for such predicates are")] ++
            GoodDetismPieces ++
            [words("since the I/O state can be neither duplicated"),
            words("nor destroyed."), nl],
        proc_info_get_context(ProcInfo, ProcContext),
        Spec = spec($pred, severity_error, phase_detism_check,
            ProcContext, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

:- func is_detism_ok_for_io(determinism) = bool.

is_detism_ok_for_io(Detism) = Ok :-
    (
        ( Detism = detism_det
        ; Detism = detism_cc_multi
        ; Detism = detism_erroneous
        ),
        Ok = yes
    ;
        ( Detism = detism_semi
        ; Detism = detism_multi
        ; Detism = detism_non
        ; Detism = detism_cc_non
        ; Detism = detism_failure
        ),
        Ok = no
    ).

%---------------------------------------------------------------------------%

    % Check to make sure that if this procedure is exported via a pragma
    % foreign_export declaration, then the determinism is not multi or nondet.
    % Pragma exported procs that have been declared to have these determinisms
    % should have been picked up in make_hlds, so this is just to catch those
    % whose determinisms need to be inferred.
    %
:- pred check_exported_proc_detism(pred_proc_id::in, proc_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_exported_proc_detism(PredProcId, ProcInfo, !ModuleInfo, !Specs) :-
    module_info_get_pragma_exported_procs(!.ModuleInfo, ExportedProcsCord0),
    ExportedProcs = cord.list(ExportedProcsCord0),
    ExportedProcsCord = cord.from_list(ExportedProcs),
    % So that any later conversion from cord to list will do nothing
    % except take off the cord wrapper.
    module_info_set_pragma_exported_procs(ExportedProcsCord, !ModuleInfo),
    proc_info_get_inferred_determinism(ProcInfo, Detism),
    PredProcId = proc(PredId, ProcId),
    ( if
        is_proc_pragma_exported(ExportedProcs, PredId, ProcId, ExportContext),
        ( Detism = detism_multi
        ; Detism = detism_non
        )
    then
        DetismStr = determinism_to_string(Detism),
        Pieces = [words("Error:"),
            pragma_decl("foreign_export"), words("declaration"),
            words("for a procedure whose determinism is")] ++
            color_as_incorrect([quote(DetismStr), suffix(".")]) ++ [nl],
        Spec = spec($pred, severity_error, phase_detism_check,
            ExportContext, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

:- pred is_proc_pragma_exported(list(pragma_exported_proc)::in,
    pred_id::in, proc_id::in, prog_context::out) is semidet.

is_proc_pragma_exported([ExportProc | ExportProcs], PredId, ProcId, Context) :-
    ( if ExportProc = pragma_exported_proc(_, PredId, ProcId, _, Context0) then
        Context = Context0
    else
        is_proc_pragma_exported(ExportProcs, PredId, ProcId, Context)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

det_check_lambda(DeclaredDetism, InferredDetism, Goal, GoalInfo, InstMap0,
        !DetInfo) :-
    compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
    (
        ( Cmp = first_detism_tighter_than
        ; Cmp = first_detism_incomparable
        ),
        det_info_get_pred_proc_id(!.DetInfo, PredProcId),
        Context = goal_info_get_context(GoalInfo),
        det_info_get_module_info(!.DetInfo, ModuleInfo),
        ProcColonPieces = describe_one_proc_name_mode(ModuleInfo,
            output_mercury, yes(color_subject), should_not_module_qualify,
            [suffix(":")], PredProcId),
        DeclaredStr = determinism_to_string(DeclaredDetism),
        InferredStr = determinism_to_string(InferredDetism),
        DeclaredPieces = color_as_correct([quote(DeclaredStr), suffix(",")]),
        InferredPieces = color_as_incorrect([quote(InferredStr), suffix(".")]),
        Pieces = [words("In")] ++ ProcColonPieces ++ [nl,
            words("determinism error in lambda expression."), nl] ++
            [words("Declared")] ++ DeclaredPieces ++
            [words("inferred")] ++ InferredPieces ++ [nl],
        det_diagnose_goal_get_msgs(Goal, InstMap0, DeclaredDetism, [],
            !DetInfo, GoalMsgs),
        Spec = error_spec($pred, severity_error, phase_detism_check,
            [msg(Context, Pieces) | GoalMsgs]),
        det_info_add_error_spec(Spec, !DetInfo)
    ;
        ( Cmp = first_detism_same_as
        ; Cmp = first_detism_looser_than
        )
        % We don't bother issuing warnings if the determinism was too loose;
        % that will often be the case, and should not be warned about.
    ).

:- pred report_determinism_problem(module_info::in, pred_proc_id::in,
    string::in, string::in, list(format_piece)::in,
    determinism::in, determinism::in, error_msg::out) is det.

report_determinism_problem(ModuleInfo, PredProcId, ErrorOrWarn, ProblemStr,
        ReasonPieces, DeclaredDetism, InferredDetism, Msg) :-
    module_info_proc_info(ModuleInfo, PredProcId, ProcInfo),
    proc_info_get_detism_decl(ProcInfo, DetismDecl),
    proc_info_get_context(ProcInfo, Context),
    ProcPieces = describe_one_proc_name_mode(ModuleInfo, output_mercury,
        yes(color_subject), should_not_module_qualify, [], PredProcId),
    DeclaredStr = determinism_to_string(DeclaredDetism),
    InferredStr = determinism_to_string(InferredDetism),
    DeclaredPieces = color_as_correct([quote(DeclaredStr), suffix(",")]),
    InferredPieces = color_as_incorrect([quote(InferredStr), suffix(".")]),
    Pieces = [words(ErrorOrWarn), suffix(":"), words("the"),
        words(detism_decl_name(DetismDecl)), words("for")] ++ ProcPieces ++
        color_as_incorrect([words(ProblemStr), suffix(".")]) ++ [nl] ++
        [words("Declared")] ++ DeclaredPieces ++
        [words("inferred")] ++ InferredPieces ++ [nl] ++
        ReasonPieces,
    Msg = msg(Context, Pieces).

%---------------------------------------------------------------------------%

    % The given goal should have determinism Desired, but doesn't.
    % Find out what is wrong, and return a list of messages giving the causes.
    %
:- pred det_diagnose_goal_get_msgs(hlds_goal::in, instmap::in, determinism::in,
    list(switch_context)::in, det_info::in, det_info::out,
    list(error_msg)::out) is det.

det_diagnose_goal_get_msgs(Goal, InstMap0, Desired, SwitchContexts, !DetInfo,
        Msgs) :-
    det_diagnose_goal(Goal, InstMap0, Desired, SwitchContexts, !DetInfo,
        MsgGroups),
    sort_error_msg_groups(MsgGroups, SortedMsgGroups),
    Msgs = flatten_error_msg_groups(SortedMsgGroups).

    % The given goal should have determinism Desired, but doesn't.
    % Find out what is wrong, and return a list of messages giving the causes.
    %
:- pred det_diagnose_goal(hlds_goal::in, instmap::in, determinism::in,
    list(switch_context)::in, det_info::in, det_info::out,
    list(error_msg_group)::out) is det.

det_diagnose_goal(Goal, InstMap0, Desired, SwitchContexts, !DetInfo,
        MsgGroups) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    Actual = goal_info_get_determinism(GoalInfo),
    compare_determinisms(Desired, Actual, CompareResult),
    (
        ( CompareResult = first_detism_tighter_than
        ; CompareResult = first_detism_incomparable
        ),
        det_diagnose_goal_expr(GoalExpr, GoalInfo, InstMap0, Desired, Actual,
            SwitchContexts, !DetInfo, MsgGroups)
    ;
        ( CompareResult = first_detism_same_as
        ; CompareResult = first_detism_looser_than
        ),
        MsgGroups = []
    ).

%---------------------------------------------------------------------------%

:- pred det_diagnose_goal_expr(hlds_goal_expr::in, hlds_goal_info::in,
    instmap::in, determinism::in, determinism::in, list(switch_context)::in,
    det_info::in, det_info::out, list(error_msg_group)::out) is det.

det_diagnose_goal_expr(GoalExpr, GoalInfo, InstMap0, Desired, Actual,
        SwitchContexts, !DetInfo, MsgGroups) :-
    (
        GoalExpr = unify(LHS, RHS, _, _, UnifyContext),
        Context = goal_info_get_context(GoalInfo),
        det_report_unify_context(is_first, is_last, UnifyContext, !.DetInfo,
            LHS, RHS, SurroundingContextPieces, GoalPieces),
        det_diagnose_primitive_goal(Desired, Actual, ProblemPieces),
        Pieces = SurroundingContextPieces ++
            [lower_case_next_if_not_first] ++ GoalPieces ++ ProblemPieces,
        MsgGroups = [error_msg_group(msg(Context, Pieces), [])]
    ;
        GoalExpr = plain_call(PredId, ProcId, _, _, CallContext, _),
        Context = goal_info_get_context(GoalInfo),
        det_report_call_context(CallContext, !.DetInfo, PredId, ProcId,
            AnyUnifyPieces, SurroundingContextPieces, GoalPieces),
        det_diagnose_primitive_goal(Desired, Actual, ProblemPieces),
        Pieces = AnyUnifyPieces ++ SurroundingContextPieces ++
            [lower_case_next_if_not_first] ++ GoalPieces ++ ProblemPieces,
        MsgGroups = [error_msg_group(msg(Context, Pieces), [])]
    ;
        GoalExpr = generic_call(GenericCall, _, _, _, _),
        Context = goal_info_get_context(GoalInfo),
        hlds_goal.generic_call_to_id(GenericCall, GenericCallId),
        GenericCallIdStr = generic_call_id_to_string(GenericCallId),
        GoalPieces = color_as_subject([words((GenericCallIdStr))]),
        det_diagnose_primitive_goal(Desired, Actual, ProblemPieces),
        Pieces = GoalPieces ++ ProblemPieces,
        MsgGroups = [error_msg_group(msg(Context, Pieces), [])]
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        Context = goal_info_get_context(GoalInfo),
        DesiredStr = determinism_to_string(Desired),
        Pieces = [words("Determinism declaration not satisfied."),
            words("Desired determinism is"), words(DesiredStr),
            suffix("."), nl],
        MsgGroups = [error_msg_group(msg(Context, Pieces), [])]
    ;
        GoalExpr = conj(_, Goals),
        det_diagnose_conj(Goals, InstMap0, Desired, SwitchContexts, !DetInfo,
            MsgGroups)
    ;
        GoalExpr = disj(Goals),
        % We use bags instead of sets because it is possible (though it is
        % *incredibly* rare) for more than one disjunct to have the same
        % context, and we don't want to mistake two possibly-successful
        % disjuncts that have the same context, which call for a message here,
        % with just one possibly-successful disjunct, which does not.
        det_diagnose_disj(Goals, InstMap0, Desired, Actual, SwitchContexts,
            !DetInfo, bag.init, DisjunctsWithSolnSet, SubMsgGroups),
        determinism_components(Desired, _, DesSolns),
        bag.to_list(DisjunctsWithSolnSet, DisjunctsWithSoln),
        ( if
            DesSolns \= at_most_many,
            DesSolns \= at_most_many_cc,
            DisjunctsWithSoln = [FirstContext | LaterContexts],
            LaterContexts = [_ | _]
        then
            det_diagnose_switch_context(!.DetInfo, SwitchContexts,
                NestingPieces),
            FirstDisjPieces = [lower_case_next_if_not_first,
                words("Disjunction has more than one disjunct"),
                words("with solutions."), nl],
            FirstMsg =
                msg(FirstContext, NestingPieces ++ FirstDisjPieces),
            LaterDisjPieces = [words("This later disjunct")] ++
                color_as_incorrect([words("may have a solution.")]) ++
                [nl],
            MakeLaterMsgs =
                ( func(LaterContext) = LaterMsg :-
                    LaterMsg = msg(LaterContext, LaterDisjPieces)
                ),
            list.sort(LaterContexts, SortedLaterContexts),
            LaterMsgs = list.map(MakeLaterMsgs, SortedLaterContexts),
            DisjMsgGroup = error_msg_group(FirstMsg, LaterMsgs),
            MsgGroups = [DisjMsgGroup | SubMsgGroups]
        else
            MsgGroups = SubMsgGroups
        )
    ;
        GoalExpr = switch(Var, SwitchCanFail, Cases),
        % The determinism of a switch is the worst of the determinism of each
        % of the cases. Also, if only a subset of the constructors are handled,
        % then it is semideterministic or worse - this is determined
        % in switch_detection.m and handled via the CanFail field.
        ( if
            SwitchCanFail = can_fail,
            determinism_components(Desired, cannot_fail, _)
        then
            Context = goal_info_get_context(GoalInfo),
            find_missing_cons_ids(!.DetInfo, yes(10), InstMap0, SwitchContexts,
                Var, Cases, NestingPieces, VarStr, MaybeMissingInfo),
            (
                MaybeMissingInfo = yes(MissingInfo),
                MissingInfo = missing_cons_id_info(_, _,
                    MainPieces, VerbosePieces),
                SwitchOnPieces = [lower_case_next_if_not_first,
                    words("The switch on")] ++
                    color_as_subject([quote(VarStr)]),
                NoCoverPieces = [words("does not cover")],
                append_prefix_and_maybe_verbose(yes(color_incorrect),
                    NestingPieces ++ SwitchOnPieces, NoCoverPieces,
                    MainPieces, VerbosePieces, Component)
            ;
                MaybeMissingInfo = no,
                NoCoverPieces = [lower_case_next_if_not_first,
                    words("The switch on"), fixed(VarStr)] ++
                    color_as_incorrect([words("can fail.")]) ++ [nl],
                Component = always(NestingPieces ++ NoCoverPieces)
            ),
            SwitchMsg = simple_msg(Context, [Component]),
            SwitchMsgGroups = [error_msg_group(SwitchMsg, [])]
        else
            SwitchMsgGroups = []
        ),
        det_info_get_var_table(!.DetInfo, VarTable),
        lookup_var_type(VarTable, Var, VarType),
        det_diagnose_switch_arms(Var, VarType, Cases, InstMap0,
            Desired, SwitchContexts, !DetInfo, SubMsgGroups),
        MsgGroups = SwitchMsgGroups ++ SubMsgGroups
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        determinism_components(Desired, _DesiredCanFail, DesiredSolns),
        Cond = hlds_goal(_CondGoal, CondInfo),
        CondDetism = goal_info_get_determinism(CondInfo),
        determinism_components(CondDetism, _CondCanFail, CondSolns),
        ( if
            CondSolns = at_most_many,
            DesiredSolns \= at_most_many
        then
            determinism_components(DesiredCond, can_fail, DesiredSolns),
            det_diagnose_goal(Cond, InstMap0, DesiredCond, SwitchContexts,
                !DetInfo, MsgGroupsCond)
        else
            MsgGroupsCond = []
        ),
        update_instmap(Cond, InstMap0, InstMap1),
        det_diagnose_goal(Then, InstMap1, Desired, SwitchContexts, !DetInfo,
            MsgGroupsThen),
        det_diagnose_goal(Else, InstMap0, Desired, SwitchContexts, !DetInfo,
            MsgGroupsElse),
        MsgGroups = MsgGroupsCond ++ MsgGroupsThen ++ MsgGroupsElse
    ;
        GoalExpr = negation(_),
        determinism_components(Desired, DesiredCanFail, DesiredSolns),
        determinism_components(Actual, ActualCanFail, ActualSolns),
        ( if
            DesiredCanFail = cannot_fail,
            ActualCanFail = can_fail
        then
            Context = goal_info_get_context(GoalInfo),
            Pieces = [words("Negated goal can succeed."), nl],
            MsgGroups = [error_msg_group(msg(Context, Pieces), [])]
        else if
            DesiredSolns = at_most_zero,
            ActualSolns \= at_most_zero
        then
            Context = goal_info_get_context(GoalInfo),
            Pieces = [words("Negated goal can fail."), nl],
            MsgGroups = [error_msg_group(msg(Context, Pieces), [])]
        else
            MsgGroups = []
        )
    ;
        GoalExpr = scope(_, SubGoal),
        SubGoal = hlds_goal(_, SubGoalInfo),
        Internal = goal_info_get_determinism(SubGoalInfo),
        ( if Actual = Internal then
            InternalDesired = Desired
        else
            determinism_components(Desired, CanFail, _),
            determinism_components(InternalDesired, CanFail, at_most_many)
        ),
        det_diagnose_goal(SubGoal, InstMap0, InternalDesired, SwitchContexts,
            !DetInfo, MsgGroups)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            det_diagnose_goal(MainGoal, InstMap0, Desired,
                SwitchContexts, !DetInfo, MainMsgGroups),
            det_diagnose_orelse_goals(OrElseGoals, InstMap0, Desired,
                SwitchContexts, !DetInfo, OrElseMsgGroups),
            MsgGroups = MainMsgGroups ++ OrElseMsgGroups
        ;
            ShortHand = try_goal(_, _, SubGoal),
            det_diagnose_goal(SubGoal, InstMap0, Desired, SwitchContexts,
                !DetInfo, MsgGroups)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

%---------------------------------------------------------------------------%

:- pred det_diagnose_primitive_goal(determinism::in, determinism::in,
    list(format_piece)::out) is det.

det_diagnose_primitive_goal(Desired, Actual,
        Pieces) :-
    determinism_components(Desired, DesiredCanFail, DesiredSolns),
    determinism_components(Actual, ActualCanFail, ActualSolns),
    compare_canfails(DesiredCanFail, ActualCanFail, CmpCanFail),
    (
        CmpCanFail = first_tighter_than,
        CanFailPieces = [words("can fail")]
    ;
        ( CmpCanFail = first_same_as
        ; CmpCanFail = first_looser_than
        ),
        CanFailPieces = []
    ),
    compare_solncounts(DesiredSolns, ActualSolns, CmpSolns),
    (
        CmpSolns = first_tighter_than,
        (
            DesiredSolns = at_most_one,
            SolnsPieces = [words("can succeed more than once")]
        ;
            ( DesiredSolns = at_most_zero
            ; DesiredSolns = at_most_many
            ; DesiredSolns = at_most_many_cc
            ),
            SolnsPieces = [words("can succeed")]
        )
    ;
        ( CmpSolns = first_same_as
        ; CmpSolns = first_looser_than
        ),
        SolnsPieces = []
    ),
    (
        CanFailPieces = [],
        SolnsPieces = [],
        ProblemPieces = []
    ;
        CanFailPieces = [],
        SolnsPieces = [_ | _],
        ProblemPieces = color_as_incorrect(SolnsPieces ++ [suffix(".")])
    ;
        CanFailPieces = [_ | _],
        SolnsPieces = [],
        ProblemPieces = color_as_incorrect(CanFailPieces ++ [suffix(".")])
    ;
        CanFailPieces = [_ | _],
        SolnsPieces = [_ | _],
        ProblemPieces = color_as_incorrect(CanFailPieces) ++
            [words("and")] ++
            color_as_incorrect(SolnsPieces ++ [suffix(".")])
    ),
    (
        ProblemPieces = [_ | _],
        Pieces = ProblemPieces
    ;
        ProblemPieces = [],
        DesiredPieces0 = [fixed(determinism_to_string(Desired)), suffix(",")],
        ActualPieces0 = [fixed(determinism_to_string(Actual)), suffix(".")],
        DesiredPieces = color_as_correct(DesiredPieces0),
        ActualPieces = color_as_incorrect(ActualPieces0),
        Pieces =
            [words("has unknown determinism problem;"), nl,
            words("desired determinism is")] ++ DesiredPieces ++ [nl] ++
            [words("while actual determinism is")] ++ ActualPieces ++ [nl]
    ).

det_diagnose_conj([], _InstMap0, _Desired, _SwitchContexts, !DetInfo, []).
det_diagnose_conj([Goal | Goals], InstMap0, Desired, SwitchContexts, !DetInfo,
        MsgGroups) :-
    det_diagnose_goal(Goal, InstMap0, Desired, SwitchContexts, !DetInfo,
        HeadMsgGroups),
    update_instmap(Goal, InstMap0, InstMap1),
    det_diagnose_conj(Goals, InstMap1, Desired, SwitchContexts, !DetInfo,
        TailMsgGroups),
    MsgGroups = HeadMsgGroups ++ TailMsgGroups.

:- pred det_diagnose_disj(list(hlds_goal)::in, instmap::in,
    determinism::in, determinism::in, list(switch_context)::in,
    det_info::in, det_info::out, bag(prog_context)::in, bag(prog_context)::out,
    list(error_msg_group)::out) is det.

det_diagnose_disj([], _InstMap0, _Desired, _Actual, _SwitchContexts,
        !DetInfo, !DisjunctsWithSoln, []).
det_diagnose_disj([Goal | Goals], InstMap0, Desired, Actual, SwitchContexts,
        !DetInfo, !DisjunctsWithSoln, MsgGroups) :-
    determinism_components(Actual, ActualCanFail, _),
    determinism_components(Desired, DesiredCanFail, DesiredSolns),
    ( if
        DesiredCanFail = cannot_fail,
        ActualCanFail = can_fail
    then
        % If the disjunction was declared to never fail, but we inferred that
        % it might fail, then we want to print an error message for every
        % disjunct that might fail.
        ClauseCanFail = cannot_fail
    else
        % Otherwise, either the disjunction is allowed to fail, or there is
        % at least one disjunct that we inferred won't fail, so we don't want
        % any error messages for the disjuncts that might fail.
        ClauseCanFail = can_fail
    ),
    determinism_components(ClauseDesired, ClauseCanFail, DesiredSolns),
    det_diagnose_goal(Goal, InstMap0, ClauseDesired, SwitchContexts, !DetInfo,
        HeadMsgGroups),
    Goal = hlds_goal(_, GoalInfo),
    ( if
        GoalDetism = goal_info_get_determinism(GoalInfo),
        determinism_components(GoalDetism, _, at_most_zero)
    then
        true
    else
        GoalContext = goal_info_get_context(GoalInfo),
        bag.insert(GoalContext, !DisjunctsWithSoln)
    ),
    det_diagnose_disj(Goals, InstMap0, Desired, Actual, SwitchContexts,
        !DetInfo, !DisjunctsWithSoln, TailMsgGroups),
    MsgGroups = HeadMsgGroups ++ TailMsgGroups.

:- pred det_diagnose_switch_arms(prog_var::in, mer_type::in, list(case)::in,
    instmap::in, determinism::in, list(switch_context)::in,
    det_info::in, det_info::out, list(error_msg_group)::out) is det.

det_diagnose_switch_arms(_Var, _VarType, [], _, _Desired, _SwitchContexts,
        !DetInfo, []).
det_diagnose_switch_arms(Var, VarType, [Case | Cases], InstMap0, Desired,
        SwitchContexts0, !DetInfo, MsgGroups) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    goal_to_conj_list(Goal, GoalSeq),
    find_switch_var_matches(GoalSeq, [Var], MainConsId, OtherConsIds,
        MainMatch, OtherMatches),
    NewSwitchContext = switch_context(Var, MainMatch, OtherMatches),
    SwitchContexts1 = [NewSwitchContext | SwitchContexts0],
    det_info_get_module_info(!.DetInfo, ModuleInfo0),
    bind_var_to_functors(Var, VarType, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
    det_info_set_module_info(ModuleInfo, !DetInfo),
    det_diagnose_goal(Goal, InstMap1, Desired, SwitchContexts1,
        !DetInfo, HeadMsgGroups),
    det_diagnose_switch_arms(Var, VarType, Cases, InstMap0, Desired,
        SwitchContexts0, !DetInfo, TailMsgGroups),
    MsgGroups = HeadMsgGroups ++ TailMsgGroups.

    % Given the list of conjuncts in a switch arm, find the unifications that
    % unify the switched-on variable or its synonyms with the arm's cons_ids.
    % The reason why we look for this is to get access to the argument
    % variables of that unification, in case code inside the arm has errors
    % in switches on those arguments. We don't collect argument variables
    % from unifications in which they are all local, since in that case
    % there is no chance of that happening, which means that printing
    % the argument variables in that case would not be helpful, and
    % would instead be only clutter.
    %
:- pred find_switch_var_matches(list(hlds_goal)::in, list(prog_var)::in,
    cons_id::in, list(cons_id)::in,
    switch_match::out, list(switch_match)::out) is det.

find_switch_var_matches([], _, MainConsId, OtherConsIds,
        MainMatch, OtherMatches) :-
    make_switch_match_no_args(MainConsId, MainMatch),
    list.map(make_switch_match_no_args, OtherConsIds, OtherMatches).
find_switch_var_matches([Conjunct | Conjuncts], !.SwitchVarSynonyms,
        MainConsId, OtherConsIds, MainMatch, OtherMatches) :-
    Conjunct = hlds_goal(GoalExpr, GoalInfo),
    ( if
        GoalExpr = unify(_, _, _, Unification, _),
        Unification = deconstruct(Var, MainConsId, ArgVars, _, _, _),
        list.member(Var, !.SwitchVarSynonyms),
        OtherConsIds = []
    then
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        ArgVarsSet = set_of_var.list_to_set(ArgVars),
        ( if
            set_of_var.intersect(NonLocals, ArgVarsSet, NonLocalArgVarsSet),
            set_of_var.is_non_empty(NonLocalArgVarsSet)
        then
            MaybeArgVars = yes(ArgVars)
        else
            MaybeArgVars = no
        ),
        MainMatch = switch_match(MainConsId, MaybeArgVars),
        OtherMatches = []
    else if
        GoalExpr = disj(Disjuncts),
        find_switch_var_submatches(Disjuncts, !.SwitchVarSynonyms,
            yes(MainConsId), OtherConsIds, yes(MainMatch0), OtherMatches0)
    then
        MainMatch = MainMatch0,
        OtherMatches = OtherMatches0
    else
        ( if
            GoalExpr = unify(_, _, _, Unification, _),
            Unification = assign(ToVar, FromVar),
            list.member(FromVar, !.SwitchVarSynonyms)
        then
            !:SwitchVarSynonyms = [ToVar | !.SwitchVarSynonyms]
        else
            true
        ),
        find_switch_var_matches(Conjuncts, !.SwitchVarSynonyms,
            MainConsId, OtherConsIds, MainMatch, OtherMatches)
    ).

    % If a conjunct in a switch arm is disjunction, check whether it is
    % the disjunction that specifies that this is the arm for MainConsId
    % and OtherConsIds. Once we have found a cons_id, we delete it from
    % the list of cons_ids the recursive call should look for. In the case of
    % the main cons_id, we do this by passing `no' as the MaybeMainConsId
    % argument; in the case of the other cons_ids, we do this by deleting it
    % from the OtherConsIds argument.
    %
    % If a call to this predicate succeeds, it should return switch_matches
    % for exactly the set of main and other cons_ids it was invoked with.
    %
:- pred find_switch_var_submatches(list(hlds_goal)::in, list(prog_var)::in,
    maybe(cons_id)::in, list(cons_id)::in,
    maybe(switch_match)::out, list(switch_match)::out) is semidet.

find_switch_var_submatches([], _, no, [], no, []).
find_switch_var_submatches([Disjunct | Disjuncts], SwitchVarSynonyms,
        MaybeMainConsId, OtherConsIds, MaybeMainMatch, OtherMatches) :-
    Disjunct = hlds_goal(GoalExpr, GoalInfo),
    GoalExpr = unify(_, _, _, Unification, _),
    Unification = deconstruct(Var, ConsId, ArgVars, _, _, _),
    list.member(Var, SwitchVarSynonyms),
    ( if
        MaybeMainConsId = yes(MainConsId),
        ConsId = MainConsId
    then
        find_switch_var_submatches(Disjuncts, SwitchVarSynonyms,
            no, OtherConsIds, no, OtherMatches),
        MaybeMainMatch = yes(switch_match(ConsId, yes(ArgVars)))
    else if
        list.delete_first(OtherConsIds, ConsId, LeftOverConsIds)
    then
        find_switch_var_submatches(Disjuncts, SwitchVarSynonyms,
            MaybeMainConsId, LeftOverConsIds, MaybeMainMatch, LeftOverMatches),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        set_of_var.list_to_set(ArgVars, ArgVarsSet),
        ( if
            set_of_var.intersect(NonLocals, ArgVarsSet, NonLocalArgVarsSet),
            set_of_var.is_non_empty(NonLocalArgVarsSet)
        then
            MaybeArgVars = yes(ArgVars)
        else
            MaybeArgVars = no
        ),
        OtherMatches = [switch_match(ConsId, MaybeArgVars) | LeftOverMatches]
    else
        fail
    ).

:- pred make_switch_match_no_args(cons_id::in, switch_match::out) is det.

make_switch_match_no_args(ConsId, Match) :-
    Match = switch_match(ConsId, no).

:- pred det_diagnose_orelse_goals(list(hlds_goal)::in, instmap::in,
    determinism::in, list(switch_context)::in, det_info::in, det_info::out,
    list(error_msg_group)::out) is det.

det_diagnose_orelse_goals([], _, _Desired, _SwitchContexts, !DetInfo, []).
det_diagnose_orelse_goals([Goal | Goals], InstMap0, Desired, SwitchContexts0,
        !DetInfo, MsgGroups) :-
    % XXX Once we start using STM in earnest, we should add something
    % representing "In orelse arm #n:" to the switch context.
    det_diagnose_goal(Goal, InstMap0, Desired, SwitchContexts0,
        !DetInfo, HeadMsgGroups),
    det_diagnose_orelse_goals(Goals, InstMap0, Desired, SwitchContexts0,
        !DetInfo, TailMsgGroups),
    MsgGroups = HeadMsgGroups ++ TailMsgGroups.

%---------------------------------------------------------------------------%
%
% There are two reasons why we may want to report that a switch is incomplete:
%
% - because the switch is wrapped in a require_complete_switch scope, and
% - because the --inform-incomplete-switch option is set.

    % This type says whether the --inform-incomplete-switch option is set.
    %
:- type maybe_inform_incomplete_switches
    --->    do_not_inform_incomplete_switches
    ;       inform_incomplete_switches.

    % This type says which of the above two reasons causes us to report
    % the given incomplete switch.
    %
:- type why_report_incomplete_switch
    --->    switch_required_to_be_complete
    ;       inform_incomplete_switch_option.

    % It is possible for *both* reasons to apply to the same incomplete switch.
    % In such cases, we want to generate only the error required by the scope,
    % and not the information message that the option calls for. We process
    % procedure bodies top down, so we process the scope goal before
    % the switch it wraps, so when we generate an error report for an
    % incomplete switch, we pass along its details when we process the
    % goal inside the scope (which may either be the incomplete switch,
    % or a conjunction of feature_lifted_by_cse deconstruction unifications
    % followed by the incomplete switch).
    %
:- type reported_switch
    --->    reported_switch(
                prog_context,
                prog_var,
                list(case)
            ).

    % Check that the switches in all require_complete_switch scopes are
    % actually complete. If they are not, add an error message to !DetInfo.
    %
    % If IIS = inform_incomplete_switches, do this for *all* switches.
    %
:- pred reqscope_check_goal(hlds_goal::in, instmap::in,
    maybe_inform_incomplete_switches::in, maybe(reported_switch)::in,
    list(switch_context)::in, det_info::in, det_info::out) is det.

reqscope_check_goal(Goal, InstMap0, IIS, MaybeReportedSwitch,
        SwitchContexts, !DetInfo) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = conj(_, Goals),
        reqscope_check_conj(Goals, InstMap0, IIS, MaybeReportedSwitch,
            SwitchContexts, !DetInfo)
    ;
        GoalExpr = disj(Goals),
        reqscope_check_disj(Goals, InstMap0, IIS, SwitchContexts, !DetInfo)
    ;
        GoalExpr = switch(Var, CanFail, Cases),
        (
            CanFail = cannot_fail
        ;
            CanFail = can_fail,
            Context = goal_info_get_context(GoalInfo),
            ( if
                (
                    IIS = do_not_inform_incomplete_switches
                ;
                    MaybeReportedSwitch = yes(ReportedSwitch),
                    ReportedSwitch = reported_switch(ReportedContext,
                        ReportedVar, ReportedCases),
                    ReportedContext = Context,
                    ReportedVar = Var,
                    ReportedCases = Cases
                )
            then
                % We have already reported an error for this incomplete switch.
                true
            else
                generate_incomplete_switch_spec(
                    inform_incomplete_switch_option, yes(10),
                    InstMap0, SwitchContexts, Var, Cases, Context, !DetInfo)
            )
        ),
        det_info_get_var_table(!.DetInfo, VarTable),
        lookup_var_type(VarTable, Var, VarType),
        reqscope_check_cases(Var, VarType, Cases, InstMap0,
            IIS, SwitchContexts, !DetInfo)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        reqscope_check_goal(Cond, InstMap0, IIS, no,
            SwitchContexts, !DetInfo),
        update_instmap(Cond, InstMap0, InstMap1),
        reqscope_check_goal(Then, InstMap1, IIS, no,
            SwitchContexts, !DetInfo),
        reqscope_check_goal(Else, InstMap0, IIS, no,
            SwitchContexts, !DetInfo)
    ;
        GoalExpr = negation(SubGoal),
        reqscope_check_goal(SubGoal, InstMap0, IIS, no,
            SwitchContexts, !DetInfo)
    ;
        GoalExpr = scope(Reason, SubGoal),
        reqscope_check_scope(SwitchContexts, Reason, SubGoal, GoalInfo,
            InstMap0, ScopeMaybeReportedSwitch, !DetInfo),
        reqscope_check_goal(SubGoal, InstMap0, IIS, ScopeMaybeReportedSwitch,
            SwitchContexts, !DetInfo)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            reqscope_check_goal(MainGoal, InstMap0, IIS, no,
                SwitchContexts, !DetInfo),
            reqscope_check_disj(OrElseGoals, InstMap0, IIS,
                SwitchContexts, !DetInfo)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            reqscope_check_goal(SubGoal, InstMap0, IIS, no,
                SwitchContexts, !DetInfo)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ;
        GoalExpr = unify(_, RHS, _, _, _),
        (
            ( RHS = rhs_var(_)
            ; RHS = rhs_functor(_, _, _)
            )
        ;
            RHS = rhs_lambda_goal(_Purity, _Groundness, _PorF,
                _LambdaNonLocals, ArgVarsModes, _Detism, LambdaGoal),
            det_info_get_module_info(!.DetInfo, ModuleInfo),
            lambda_update_instmap(ModuleInfo, ArgVarsModes,
                InstMap0, LambdaInstMap0),
            reqscope_check_goal(LambdaGoal, LambdaInstMap0, IIS, no,
                [], !DetInfo)
        )
    ;
        ( GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        )
    ).

:- pred reqscope_check_scope(list(switch_context)::in,
    scope_reason::in, hlds_goal::in, hlds_goal_info::in, instmap::in,
    maybe(reported_switch)::out, det_info::in, det_info::out) is det.

reqscope_check_scope(SwitchContexts, Reason, SubGoal, ScopeGoalInfo, InstMap0,
        MaybeReportedSwitch, !DetInfo) :-
    (
        Reason = require_detism(RequiredDetism),
        reqscope_check_goal_detism(RequiredDetism, SubGoal,
            require_detism_scope(ScopeGoalInfo), InstMap0, !DetInfo),
        MaybeReportedSwitch = no
    ;
        Reason = require_complete_switch(RequiredVar),
        % We must test the version of the subgoal that has not yet been
        % simplified, since simplification can convert a complete switch
        % into an incomplete switch by deleting an arm that contains
        % only `fail'.
        ( if
            is_scope_subgoal_a_sortof_switch(SubGoal,
                SwitchGoalContext, SwitchVar, CanFail, Cases),
            SwitchVar = RequiredVar
        then
            (
                CanFail = cannot_fail,
                MaybeReportedSwitch = no
            ;
                CanFail = can_fail,
                ScopeContext = goal_info_get_context(ScopeGoalInfo),
                generate_incomplete_switch_spec(
                    switch_required_to_be_complete, no, InstMap0,
                    SwitchContexts, RequiredVar, Cases, ScopeContext,
                    !DetInfo),
                ReportedSwitch =
                    reported_switch(SwitchGoalContext, SwitchVar, Cases),
                MaybeReportedSwitch = yes(ReportedSwitch)
            )
        else
            generate_error_not_switch_on_required_var(SwitchContexts,
                RequiredVar, "require_complete_switch", ScopeGoalInfo,
                !DetInfo),
            MaybeReportedSwitch = no
        )
    ;
        Reason = require_switch_arms_detism(RequiredVar, RequiredDetism),
        ( if
            is_scope_subgoal_a_sortof_switch(SubGoal,
                _SwitchContext, SwitchVar, _CanFail, Cases)
        then
            det_info_get_var_table(!.DetInfo, VarTable),
            lookup_var_type(VarTable, SwitchVar, SwitchVarType),
            reqscope_check_goal_detism_for_cases(RequiredDetism,
                SwitchVar, SwitchVarType, Cases, InstMap0, !DetInfo)
        else
            (
                RequiredDetism = detism_det,
                ScopeWord = "require_switch_arms_det"
            ;
                RequiredDetism = detism_semi,
                ScopeWord = "require_switch_arms_semidet"
            ;
                RequiredDetism = detism_multi,
                ScopeWord = "require_switch_arms_multi"
            ;
                RequiredDetism = detism_non,
                ScopeWord = "require_switch_arms_nondet"
            ;
                RequiredDetism = detism_cc_multi,
                ScopeWord = "require_switch_arms_cc_multi"
            ;
                RequiredDetism = detism_cc_non,
                ScopeWord = "require_switch_arms_cc_nondet"
            ;
                RequiredDetism = detism_erroneous,
                ScopeWord = "require_switch_arms_erroneous"
            ;
                RequiredDetism = detism_failure,
                ScopeWord = "require_switch_arms_failure"
            ),
            generate_error_not_switch_on_required_var(SwitchContexts,
                RequiredVar, ScopeWord, ScopeGoalInfo, !DetInfo)
        ),
        MaybeReportedSwitch = no
    ;
        Reason = loop_control(_, _, _),
        SubGoal = hlds_goal(_, SubGoalInfo),
        Detism = goal_info_get_determinism(SubGoalInfo),
        (
            ( Detism = detism_det
            ; Detism = detism_cc_multi
            )
        ;
            ( Detism = detism_semi
            ; Detism = detism_multi
            ; Detism = detism_non
            ; Detism = detism_cc_non
            ; Detism = detism_failure
            % Note: One day we should make exceptions in parallel
            % conjunctions work.
            ; Detism = detism_erroneous
            ),
            % Since loop control structures are generated only by the compiler,
            % it is reasonable to abort here, and it we don't want to present
            % the user with what would be a very confusing error message.
            unexpected($pred, "Loop control scope with strange determinism")
        ),
        MaybeReportedSwitch = no
    ;
        ( Reason = exist_quant(_, _)
        ; Reason = disable_warnings(_, _)
        ; Reason = commit(_)
        ; Reason = barrier(_)
        ; Reason = promise_purity(_)
        ; Reason = promise_solutions(_, _)
        ; Reason = from_ground_term(_, _)
        ; Reason = trace_goal(_, _, _, _, _)
        ),
        MaybeReportedSwitch = no
    ).

    % Is the given goal, which should be the subgoal of either a
    % require_complete_switch or require_switch_arms_detism scope,
    % a switch, or something that started out as a switch before
    % being modified by the compiler passes between switch detection
    % and here?
    %
:- pred is_scope_subgoal_a_sortof_switch(hlds_goal::in,
    prog_context::out, prog_var::out, can_fail::out, list(case)::out)
    is semidet.

is_scope_subgoal_a_sortof_switch(Goal, SwitchContext, SwitchVar,
        CanFail, Cases) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    require_complete_switch [GoalExpr]
    (
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        SwitchContext = goal_info_get_context(GoalInfo)
    ;
        GoalExpr = scope(Reason, SubGoal),
        % If the switch originally directly inside this scope
        % has some arms that have more solutions than they should
        % but the identity of those solutions does not matter,
        % the main determinism analysis algorithm will wrap the
        % switch with a commit scope. To diagnose the problem
        % that required this scope, we want to look through it.
        Reason = commit(_MaybeForcePruning),
        SubGoal = hlds_goal(SubGoalExpr, SubGoalInfo),
        SubGoalExpr = switch(SwitchVar, CanFail, Cases),
        SwitchContext = goal_info_get_context(SubGoalInfo)
    ;
        GoalExpr = conj(plain_conj, Conjuncts0),
        flatten_conj(Conjuncts0, Conjuncts),
        cse_lifted_then_sortof_switch(Conjuncts, SwitchContext, SwitchVar,
            CanFail, Cases)
    ;
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = disj(_)
        ; GoalExpr = if_then_else(_, _, _, _)
        ; GoalExpr = negation(_)
        ; GoalExpr = shorthand(_)
        ),
        fail
    ).

:- pred cse_lifted_then_sortof_switch(list(hlds_goal)::in,
    prog_context::out, prog_var::out, can_fail::out, list(case)::out)
    is semidet.

cse_lifted_then_sortof_switch(Conjuncts, SwitchContext, SwitchVar,
        CanFail, Cases) :-
    (
        Conjuncts = [Conjunct],
        is_scope_subgoal_a_sortof_switch(Conjunct,
            SwitchContext, SwitchVar, CanFail, Cases)
    ;
        Conjuncts = [Conjunct1, Conjunct2 | TailConjuncts],
        goal_has_feature(Conjunct1, feature_lifted_by_cse),
        cse_lifted_then_sortof_switch([Conjunct2 | TailConjuncts],
            SwitchContext, SwitchVar, CanFail, Cases)
    ).

:- pred generate_incomplete_switch_spec(why_report_incomplete_switch::in,
    maybe(int)::in, instmap::in, list(switch_context)::in,
    prog_var::in, list(case)::in, prog_context::in,
    det_info::in, det_info::out) is det.

generate_incomplete_switch_spec(Why, MaybeLimit, InstMap0, SwitchContexts,
        SwitchVar, Cases, Context, !DetInfo) :-
    find_missing_cons_ids(!.DetInfo, MaybeLimit, InstMap0, SwitchContexts,
        SwitchVar, Cases, NestingPieces, SwitchVarStr, MaybeMissingInfo),
    (
        MaybeMissingInfo = yes(MissingInfo),
        MissingInfo = missing_cons_id_info(NumPossibleConsIds,
            NumUncoveredConsIds, MainPieces, VerbosePieces),
        (
            Why = switch_required_to_be_complete,
            ErrorPieces = [lower_case_next_if_not_first,
                words("Error: the")] ++
                color_as_subject([words("switch on"), quote(SwitchVarStr)]) ++
                [words("is required to be complete,"),
                words("but it does not cover")],
            append_prefix_and_maybe_verbose(yes(color_incorrect),
                NestingPieces ++ ErrorPieces, [],
                MainPieces, VerbosePieces, Component),
            MaybeSeverityComponents = yes({severity_error, [Component]})
        ;
            Why = inform_incomplete_switch_option,
            det_info_get_module_info(!.DetInfo, ModuleInfo),
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_int_option(Globals,
                inform_incomplete_switch_threshold, Threshold),
            NumCoveredConsIds = NumPossibleConsIds - NumUncoveredConsIds,
            ( if NumCoveredConsIds * 100 >= NumPossibleConsIds * Threshold then
                % The number of covered cons_ids is above the threshold,
                % so it is reasonably likely to that the switch is *meant*
                % to be complete.
                NoCoverPieces = [lower_case_next_if_not_first,
                    words("The switch on"), quote(SwitchVarStr),
                    words("does not cover")],
                append_prefix_and_maybe_verbose(no, NestingPieces,
                    NoCoverPieces, MainPieces, VerbosePieces, Component),
                MaybeSeverityComponents =
                    yes({severity_informational, [Component]})
            else
                MaybeSeverityComponents = no
            )
        )
    ;
        MaybeMissingInfo = no,
        (
            Why = switch_required_to_be_complete,
            NoCoverPieces = [lower_case_next_if_not_first,
                words("Error: the")] ++
                color_as_subject([words("switch on"), quote(SwitchVarStr)]) ++
                [words("is required to be complete, but it is not."), nl],
            Component = always(NestingPieces ++ NoCoverPieces),
            MaybeSeverityComponents = yes({severity_error, [Component]})
        ;
            Why = inform_incomplete_switch_option,
            MaybeSeverityComponents = no
        )
    ),
    (
        MaybeSeverityComponents = yes({Severity, SpecComponents}),
        Msg = simple_msg(Context, SpecComponents),
        Spec = error_spec($pred, Severity, phase_detism_check, [Msg]),
        det_info_add_error_spec(Spec, !DetInfo)
    ;
        MaybeSeverityComponents = no
    ).

:- pred append_prefix_and_maybe_verbose(maybe(color_name)::in,
    list(format_piece)::in, list(format_piece)::in,
    list(format_piece)::in, list(format_piece)::in,
    error_msg_component::out) is det.

append_prefix_and_maybe_verbose(MaybeColor,
        NeutralPrefixPieces, ColorPrefixPieces0, MainPieces, VerbosePieces,
        Component) :-
    PrefixPieces = NeutralPrefixPieces ++
        maybe_color_pieces(MaybeColor, ColorPrefixPieces0),
    (
        VerbosePieces = [],
        Component = always(PrefixPieces ++ MainPieces)
    ;
        VerbosePieces = [_ | _],
        Component = verbose_and_nonverbose(
            PrefixPieces ++ VerbosePieces,
            PrefixPieces ++ MainPieces)
    ).

%---------------------------------------------------------------------------%

    % Are we checking the determinism of a goal for a require_{det,...} scope,
    % or for a require_switch_arms_{det,...} scope?
    %
:- type detism_check_kind
    --->    require_detism_scope(hlds_goal_info)
    ;       require_detism_switch_arm(prog_var, cons_id, list(cons_id)).

:- pred reqscope_check_goal_detism(determinism::in, hlds_goal::in,
    detism_check_kind::in, instmap::in, det_info::in, det_info::out) is det.

reqscope_check_goal_detism(RequiredDetism, Goal, CheckKind, InstMap0,
        !DetInfo) :-
    Goal = hlds_goal(_, GoalInfo),
    ActualDetism = goal_info_get_determinism(GoalInfo),
    compare_determinisms(ActualDetism, RequiredDetism, CompareResult),
    ( if
        (
            CheckKind = require_detism_scope(_),
            % For require_detism scopes, the programmer requires an exact
            % match.
            CompareResult = first_detism_same_as
        ;
            CheckKind = require_detism_switch_arm(_, _, _),
            % For require_switch_arms_detism scopes, the programmer requires
            % only that each switch arm's determinism must be at least as tight
            % as RequiredDetism.
            ( CompareResult = first_detism_tighter_than
            ; CompareResult = first_detism_same_as
            )
        )
    then
        true
    else
        ReqDetismStr = determinism_to_string(RequiredDetism),
        ActDetismStr = determinism_to_string(ActualDetism),
        ReqPieces = color_as_correct([quote(ReqDetismStr), suffix(",")]),
        ActPieces = color_as_incorrect([quote(ActDetismStr), suffix(".")]),
        (
            CheckKind = require_detism_scope(ScopeGoalInfo),
            % For require_detism scopes, the context of the require_detism
            % keyword is the most appropriate scope.
            Context = goal_info_get_context(ScopeGoalInfo),
            Pieces = [words("Error: the required determinism of the goal"),
                words("in this scope is")] ++ ReqPieces ++
                [words("but its actual determinism is")] ++ ActPieces ++ [nl]
        ;
            CheckKind =
                require_detism_switch_arm(SwitchVar, MainConsId, OtherConsIds),
            % For require_switch_arms_detism scopes, the context of the
            % require_switch_arms_detism keyword won't work, since it
            % won't tell the user *which* arm's determinism isn't right.
            % We have to use the context of the switch arm itself.
            Context = goal_info_get_context(GoalInfo),
            det_info_get_var_table(!.DetInfo, VarTable),
            SwitchVarName = var_table_entry_name(VarTable, SwitchVar),
            MainConsIdStr = cons_id_and_arity_to_string(MainConsId),
            OtherConsIdStrs =
                list.map(cons_id_and_arity_to_string, OtherConsIds),
            ConsIdsPieces =
                fixed_list_to_pieces("and", [MainConsIdStr | OtherConsIdStrs]),
            Pieces = [words("Error: the arms of the")] ++
                color_as_subject([words("switch on"), quote(SwitchVarName)]) ++
                [words("are required have a determinism that is"),
                words("acceptable in a")] ++
                color_as_correct([quote(ReqDetismStr)]) ++ [words("context,"),
                words("but the actual determinism"),
                words("of the arm for")] ++ ConsIdsPieces ++
                [words("is")] ++ ActPieces ++ [nl]
        ),
        Msg = msg(Context, Pieces),
        det_diagnose_goal_get_msgs(Goal, InstMap0, RequiredDetism, [],
            !DetInfo, SubMsgs),
        Spec = error_spec($pred, severity_error, phase_detism_check,
            [Msg | SubMsgs]),
        det_info_add_error_spec(Spec, !DetInfo)
    ).

:- pred reqscope_check_goal_detism_for_cases(determinism::in,
    prog_var::in, mer_type::in, list(case)::in, instmap::in,
    det_info::in, det_info::out) is det.

reqscope_check_goal_detism_for_cases(_RequiredDetism, _Var, _VarType,
        [], _InstMap0, !DetInfo).
reqscope_check_goal_detism_for_cases(RequiredDetism, Var, VarType,
        [Case | Cases], InstMap0, !DetInfo) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    det_info_get_module_info(!.DetInfo, ModuleInfo0),
    bind_var_to_functors(Var, VarType, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
    det_info_set_module_info(ModuleInfo, !DetInfo),
    CheckKind = require_detism_switch_arm(Var, MainConsId, OtherConsIds),
    reqscope_check_goal_detism(RequiredDetism, Goal, CheckKind, InstMap1,
        !DetInfo),

    reqscope_check_goal_detism_for_cases(RequiredDetism, Var, VarType,
        Cases, InstMap0, !DetInfo).

:- pred generate_error_not_switch_on_required_var(list(switch_context)::in,
    prog_var::in, string::in, hlds_goal_info::in,
    det_info::in, det_info::out) is det.

generate_error_not_switch_on_required_var(SwitchContexts, RequiredVar,
        ScopeWord, ScopeGoalInfo, !DetInfo) :-
    det_diagnose_switch_context(!.DetInfo, SwitchContexts, NestingPieces),
    det_info_get_var_table(!.DetInfo, VarTable),
    RequiredVarStr =
        mercury_var_to_string(VarTable, print_name_only, RequiredVar),
    ScopePieces = [words(ScopeWord), fixed("[" ++ RequiredVarStr ++ "]"),
        words("scope")],
    Pieces = NestingPieces ++ [lower_case_next_if_not_first,
        words("Error: the goal inside the")] ++
        color_as_subject(ScopePieces) ++ [words("is")] ++
        color_as_incorrect([words("not a switch on"),
            quote(RequiredVarStr), suffix(".")]) ++
        [nl],
    Context = goal_info_get_context(ScopeGoalInfo),
    Spec = spec($pred, severity_error, phase_detism_check,
        Context, Pieces),
    det_info_add_error_spec(Spec, !DetInfo).

%---------------------------------------------------------------------------%

:- pred reqscope_check_conj(list(hlds_goal)::in, instmap::in,
    maybe_inform_incomplete_switches::in, maybe(reported_switch)::in,
    list(switch_context)::in,
    det_info::in, det_info::out) is det.

reqscope_check_conj([], _InstMap0, _IIS,
        _MaybeReportedSwitch, _SwitchContexts, !DetInfo).
reqscope_check_conj([Goal | Goals], InstMap0, IIS,
        MaybeReportedSwitch, SwitchContexts, !DetInfo) :-
    reqscope_check_goal(Goal, InstMap0, IIS,
        MaybeReportedSwitch, SwitchContexts, !DetInfo),
    update_instmap(Goal, InstMap0, InstMap1),
    reqscope_check_conj(Goals, InstMap1, IIS,
        MaybeReportedSwitch, SwitchContexts, !DetInfo).

:- pred reqscope_check_disj(list(hlds_goal)::in, instmap::in,
    maybe_inform_incomplete_switches::in, list(switch_context)::in,
    det_info::in, det_info::out) is det.

reqscope_check_disj([], _InstMap0, _IIS, _SwitchContexts, !DetInfo).
reqscope_check_disj([Goal | Goals], InstMap0, IIS, SwitchContexts,
        !DetInfo) :-
    reqscope_check_goal(Goal, InstMap0, IIS, no, SwitchContexts, !DetInfo),
    reqscope_check_disj(Goals, InstMap0, IIS, SwitchContexts, !DetInfo).

:- pred reqscope_check_cases(prog_var::in, mer_type::in, list(case)::in,
    instmap::in, maybe_inform_incomplete_switches::in,
    list(switch_context)::in, det_info::in, det_info::out) is det.

reqscope_check_cases(_Var, _VarType, [], _InstMap0, _IIS,
        _SwitchContexts0, !DetInfo).
reqscope_check_cases(Var, VarType, [Case | Cases], InstMap0, IIS,
        SwitchContexts0, !DetInfo) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    goal_to_conj_list(Goal, GoalSeq),
    find_switch_var_matches(GoalSeq, [Var], MainConsId, OtherConsIds,
        MainMatch, OtherMatches),
    NewSwitchContext = switch_context(Var, MainMatch, OtherMatches),
    SwitchContexts1 = [NewSwitchContext | SwitchContexts0],
    det_info_get_module_info(!.DetInfo, ModuleInfo0),
    bind_var_to_functors(Var, VarType, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
    det_info_set_module_info(ModuleInfo, !DetInfo),
    reqscope_check_goal(Goal, InstMap1, IIS, no, SwitchContexts1, !DetInfo),
    reqscope_check_cases(Var, VarType, Cases, InstMap0, IIS,
        SwitchContexts0, !DetInfo).

:- pred lambda_update_instmap(module_info::in,
    assoc_list(prog_var, mer_mode)::in, instmap::in, instmap::out) is det.

lambda_update_instmap(_ModuleInfo, [], !InstMap).
lambda_update_instmap(ModuleInfo, [Var - Mode | VarsModes], !InstMap) :-
    mode_get_insts(ModuleInfo, Mode, InitInst, _FinalInst),
    instmap_set_var(Var, InitInst, !InstMap),
    lambda_update_instmap(ModuleInfo, VarsModes, !InstMap).

%---------------------------------------------------------------------------%

:- type missing_cons_id_info
    --->    missing_cons_id_info(
                % The number of cons_ids that the switch variable could
                % be bound to on entry to the switch. May be the number of
                % function symbols in the variable's type, or it may be
                % the number of cons_ids that appear in the variable's
                % initial bound(...) inst.
                int,

                % The number of these cons_ids that do NOT occur
                % in any of the cases.
                int,

                % The diagnostic listing the missing cons_ids.
                %
                % If there are more missing cons_ids than the
                % specified limit, we return two different versions
                % of the error message fragment. The first is the version
                % to use if the user doesn't want verbose errors,
                % the second is the version to use if he/she does want them.
                %
                % If the number of missing cons_ids is below the limit,
                % or if the caller did not specify the limit,
                % the second list will be empty.
                list(format_piece),
                list(format_piece)
            ).

:- pred find_missing_cons_ids(det_info::in, maybe(int)::in, instmap::in,
    list(switch_context)::in, prog_var::in, list(case)::in,
    list(format_piece)::out, string::out,
    maybe(missing_cons_id_info)::out) is det.

find_missing_cons_ids(DetInfo, MaybeLimit, InstMap0, SwitchContexts,
        Var, Cases, NestingPieces, VarStr, MaybeMissingInfo) :-
    det_diagnose_switch_context(DetInfo, SwitchContexts, NestingPieces),

    det_info_get_module_info(DetInfo, ModuleInfo),
    det_info_get_var_table(DetInfo, VarTable),
    VarStr = mercury_var_to_string(VarTable, print_name_only, Var),
    instmap_lookup_var(InstMap0, Var, VarInst),
    ( if
        det_info_get_var_table(DetInfo, VarTable),
        lookup_var_type(VarTable, Var, VarType),
        type_to_ctor_det(VarType, VarTypeCtor),
        module_info_get_type_table(ModuleInfo, TypeTable),
        ( if
            inst_is_bound_to_functors(ModuleInfo, VarInst, BoundInsts)
        then
            bound_insts_to_cons_ids(VarTypeCtor, BoundInsts, BoundConsIds),
            list.sort_and_remove_dups(BoundConsIds, SortedBoundConsIds),
            set_tree234.sorted_list_to_set(SortedBoundConsIds,
                BoundConsIdsSet),
            % We should insist that all insts used in predicate
            % declarations be specific to the type of the arguments
            % they apply to. However, I (zs) don't think we enforce this
            % yet with any consistency in the mode checker. The intersection
            % operation below should compensate for this, but we can invoke it
            % only for the switch variable's type is a du type, and not
            % a builtin type such as int or string.
            ( if
                search_type_ctor_defn(TypeTable, VarTypeCtor, TypeDefn),
                hlds_data.get_type_defn_body(TypeDefn, TypeBody),
                TypeBody = hlds_du_type(TypeBodyDu),
                TypeBodyDu = type_body_du(TypeConstructors, _, _, _, _)
            then
                SortedTypeConsIds =
                    constructor_cons_ids(VarTypeCtor,
                        one_or_more_to_list(TypeConstructors)),
                set_tree234.sorted_list_to_set(SortedTypeConsIds,
                    TypeConsIdsSet),
                set_tree234.intersect(TypeConsIdsSet, BoundConsIdsSet,
                    PossibleConsIdsSet)
            else
                PossibleConsIdsSet = BoundConsIdsSet
            )
        else
            search_type_ctor_defn(TypeTable, VarTypeCtor, TypeDefn),
            hlds_data.get_type_defn_body(TypeDefn, TypeBody),
            TypeBody = hlds_du_type(TypeBodyDu),
            TypeBodyDu = type_body_du(TypeConstructors, _, _, _, _),
            SortedTypeConsIds =
                constructor_cons_ids(VarTypeCtor,
                    one_or_more_to_list(TypeConstructors)),
            set_tree234.sorted_list_to_set(SortedTypeConsIds, TypeConsIdsSet),
            PossibleConsIdsSet = TypeConsIdsSet
        )
    then
        compute_covered_cons_ids(Cases, set_tree234.init, CoveredConsIdsSet),
        set_tree234.difference(PossibleConsIdsSet, CoveredConsIdsSet,
            UncoveredConsIdsSet),
        NumPossibleConsIds = set_tree234.count(PossibleConsIdsSet),
        NumUncoveredConsIds = set_tree234.count(UncoveredConsIdsSet),

        UncoveredConsIds = set_tree234.to_sorted_list(UncoveredConsIdsSet),
        list.map(strip_module_qualifier_from_cons_id,
            UncoveredConsIds, UnQualConsIds),
        % Just in case the stripping of qualifiers has affected the order.
        % It shouldn't, but this double insurance costs effectively nothing.
        list.sort(UnQualConsIds, SortedUnQualConsIds),

        (
            MaybeLimit = no,
            PrintedConsIds = SortedUnQualConsIds,
            NonPrintedConsIds = []
        ;
            MaybeLimit = yes(Limit),
            list.length(SortedUnQualConsIds, NumConsIds),
            ( if NumConsIds =< Limit then
                PrintedConsIds = SortedUnQualConsIds,
                NonPrintedConsIds = []
            else
                % We use Limit-1 lines for the printed missing cons_ids,
                % and one line for the message "and N more".
                %
                % If there are Limit missing cons_ids, the then-part above
                % will print them all. If there are Limit+1 missing cons_ids,
                % we will print Limit-1 of them here, followed by "and 2 more".
                % We never print "and 1 more", and we don't want to, since
                % anyone who read that would probably (justifiably) wonder
                % "why didn't the compiler use that line to print the one
                % unprinted missing cons_id?".
                list.split_upto(Limit - 1, SortedUnQualConsIds,
                    PrintedConsIds, NonPrintedConsIds)
            )
        ),
        (
            PrintedConsIds = [],
            MaybeMissingInfo = no
        ;
            PrintedConsIds = [_ | _],
            % If we invoked determinism analysis on this procedure, then
            % it must be type correct. Since users will know the type of the
            % switched-on variable, they will know which module defined it,
            % and hence which modules defined its function symbols.
            % Repeating the name of that module for each cons_id is
            % much more likely to be distracting clutter than helpful
            % information.
            WrapConsIdFunc = ( func(C) = [unqual_cons_id_and_maybe_arity(C)] ),
            PrintedConsIdPieces = list.map(WrapConsIdFunc, PrintedConsIds),
            (
                NonPrintedConsIds = [],
                MainPieces =
                    [nl_indent_delta(1)] ++
                    pieces_list_to_color_line_pieces(color_incorrect,
                        [suffix(".")], PrintedConsIdPieces) ++
                    [nl_indent_delta(-1)],
                VerbosePieces = []
            ;
                NonPrintedConsIds = [_ | _],
                NonPrintedConsIdPieces =
                    list.map(WrapConsIdFunc, NonPrintedConsIds),
                list.length(NonPrintedConsIds, NumNonPrintedConsIds),
                MainPieces =
                    [nl_indent_delta(1)] ++
                    pieces_list_to_color_line_pieces(color_incorrect,
                        [suffix(","), fixed("...")], PrintedConsIdPieces) ++
                    [nl_indent_delta(-1)] ++
                    color_as_incorrect([words("and"),
                        int_fixed(NumNonPrintedConsIds), words("more.")]) ++
                    [nl],
                ConsIdPieces = PrintedConsIdPieces ++ NonPrintedConsIdPieces,
                VerbosePieces =
                    [nl_indent_delta(1)] ++
                    pieces_list_to_color_line_pieces(color_incorrect,
                        [suffix(".")], ConsIdPieces) ++
                    [nl_indent_delta(-1)]
            ),
            MissingInfo = missing_cons_id_info(NumPossibleConsIds,
                NumUncoveredConsIds, MainPieces, VerbosePieces),
            MaybeMissingInfo = yes(MissingInfo)
        )
    else
        MaybeMissingInfo = no
    ).

:- pred compute_covered_cons_ids(list(case)::in,
    set_tree234(cons_id)::in, set_tree234(cons_id)::out) is det.

compute_covered_cons_ids([], !CoveredConsIds).
compute_covered_cons_ids([Case | Cases], !CoveredConsIds) :-
    Case = case(MainConsId, OtherConsIds, _Goal),
    set_tree234.insert(MainConsId, !CoveredConsIds),
    set_tree234.insert_list(OtherConsIds, !CoveredConsIds),
    compute_covered_cons_ids(Cases, !CoveredConsIds).

%---------------------------------------------------------------------------%

:- type switch_context
    --->    switch_context(
                % The variable being switched on.
                prog_var,

                % The match info for the first cons_id of this case.
                switch_match,

                % The match info for the other cons_ids of this case.
                list(switch_match)
            ).

    % A switch arm is for one or more cons_ids. A switch match can record,
    % for one of these cons_ids, the variables on the right hand side of
    % the unification that told switch detection that this disjunction
    % can succeed only if the switched-on variable is bound to one of these
    % cons_ids. For example, in a switch on X, if the switch arm is
    % for the cons_id f/2, this means that the switch arm should have
    % a unification of the form X = f(A, B). Likewise, if the switch arm
    % is for more than one cons_id, such as f/2 and g/1, then the switch arm
    % should contain a disjunction such as ( X = f(A, B) ; X = g(C) ).
    % The switch match data type records the argument variables of these
    % unifications PROVIDED that some of them are visible from the outside,
    % which means that later error messages (e.g. about missing arms in
    % switches on them) can refer to them.
    %
:- type switch_match
    --->    switch_match(cons_id, maybe(list(prog_var))).

:- pred det_diagnose_switch_context(det_info::in, list(switch_context)::in,
    list(format_piece)::out) is det.

det_diagnose_switch_context(_, [], []).
det_diagnose_switch_context(DetInfo, [SwitchContext | SwitchContexts],
        Pieces) :-
    det_info_get_var_table(DetInfo, VarTable),
    SwitchContext = switch_context(Var, MainMatch, OtherMatches),
    MainMatchStr = switch_match_to_string(VarTable, MainMatch),
    OtherMatchStrs =
        list.map(switch_match_to_string(VarTable), OtherMatches),
    MatchsStr = string.join_list(", ", [MainMatchStr | OtherMatchStrs]),
    VarStr = mercury_var_to_string(VarTable, print_name_only, Var),
    InnerPieces = [words("Inside the case"), words(MatchsStr),
        words("of the switch on"), fixed(VarStr), suffix(":"), nl],
    det_diagnose_switch_context(DetInfo, SwitchContexts, OuterPieces),
    % We construct the list of switch contexts so that inner contexts come
    % before outer contexts, but we want to print the contexts from the outside
    % towards the inside.
    Pieces = OuterPieces ++ [lower_case_next_if_not_first] ++ InnerPieces.

:- func switch_match_to_string(var_table, switch_match) = string.

switch_match_to_string(VarTable, switch_match(ConsId, MaybeArgVars)) =
    cons_id_and_vars_or_arity_to_string(VarTable, do_not_qualify_cons_id,
        ConsId, MaybeArgVars).

%---------------------------------------------------------------------------%

:- pred det_report_call_context(maybe(call_unify_context)::in, det_info::in,
    pred_id::in, proc_id::in, list(format_piece)::out,
    list(format_piece)::out, list(format_piece)::out) is det.

det_report_call_context(CallUnifyContext, DetInfo, PredId, ProcId,
        UnifyPieces, SurroundingUnifyContextPieces, GoalPieces) :-
    det_info_get_module_info(DetInfo, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_origin(PredInfo, Origin),

    % If the error was in a call to a type-specific unification predicate
    % (i.e. in the unification itself), then don't print out the predicate
    % name, just print out the context. If it wasn't, then print them
    % both out. (The latter can happen if there is a determinism error
    % in a function call inside some unification.)

    ( if Origin = origin_compiler(made_for_uci(spec_pred_unify, _)) then
        UnifyPieces = [],
        (
            CallUnifyContext = yes(call_unify_context(LHS, RHS, UC)),
            det_report_unify_context(is_first, is_last, UC, DetInfo,
                LHS, RHS, SurroundingUnifyContextPieces, GoalPieces)
        ;
            % This shouldn't happen; every call to a compiler generated
            % type-specific unification predicate should have a unify_context.
            CallUnifyContext = no,
            SurroundingUnifyContextPieces = [],
            GoalPieces = [words("Some weird unification"),
                words("(or explicit call to a"),
                words("type-specific unify predicate?)")]
        )
    else
        (
            CallUnifyContext = yes(call_unify_context(LHS, RHS, UC)),
            det_report_unify_context(is_first, is_not_last, UC,
                DetInfo, LHS, RHS, CallContextPieces, MainUnifyPieces),
            UnifyPieces = CallContextPieces ++
                MainUnifyPieces ++ [suffix(":"), nl]
        ;
            CallUnifyContext = no,
            UnifyPieces = []
        ),
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_declared_argmodes(ProcInfo, ArgModes),
        proc_info_get_inst_varset(ProcInfo, InstVarSet),
        PredPieces = describe_one_pred_name_mode(ModuleInfo, output_mercury,
            InstVarSet, no, should_module_qualify, [], PredId, ArgModes),
        SurroundingUnifyContextPieces = [],
        GoalPieces = [words("This call to")] ++ color_as_subject(PredPieces)
    ).

%---------------------------------------------------------------------------%

    % det_report_unify_context returns information about the context of an
    % error, i.e. where the error occurred.
    %
    % The first two arguments are boolean flags that specify whether this is
    % the first part of a sentence (in which case we start the error message
    % with a capital letter) and whether it is the last part (in which case we
    % omit the word "in" on the final "... in unification ...").
    %
    % We return SurroundingContextPieces and MainPieces separately. MainPieces
    % describes the unification whose unify_context is the third argument,
    % while SurroundingContextPieces describes the location of that
    % unification. Returning these two separately enables our caller,
    % if it so chooses, to color only MainPieces as incorrect.
    %
:- pred det_report_unify_context(is_first::in, is_last::in, unify_context::in,
    det_info::in, prog_var::in, unify_rhs::in,
    list(format_piece)::out, list(format_piece)::out) is det.

det_report_unify_context(!.First, Last, UnifyContext, DetInfo,
        LHSVar, RHS, SurroundingContextPieces, MainPieces) :-
    unify_context_first_to_pieces(!First, UnifyContext, _LastContextWord,
        [], SurroundingContextPieces),
    det_info_get_module_info(DetInfo, ModuleInfo),
    det_info_get_var_table(DetInfo, VarTable),
    (
        !.First = is_first,
        (
            Last = is_last,
            StartWords = "Unification"
        ;
            Last = is_not_last,
            StartWords = "In unification"
        )
    ;
        !.First = is_not_first,
        (
            Last = is_last,
            StartWords = "unification"
        ;
            Last = is_not_last,
            StartWords = "in unification"
        )
    ),
    lookup_var_entry(VarTable, LHSVar, LHSVarEntry),
    LHSVarRawName = LHSVarEntry ^ vte_name,
    ( if LHSVarRawName = "" then
        RHSStr = unify_rhs_to_string(ModuleInfo, VarTable,
            print_name_only, RHS),
        MainPieces = [words(StartWords), words("with")] ++
            color_as_subject([words(add_quotes(RHSStr))])
    else
        % LHSVarName may differ from LHSVarRawName; see
        % mercury_convert_var_name for details.
        LHSVarName = mercury_var_to_string(VarTable, print_name_only, LHSVar),
        ( if
            RHS = rhs_var(RHSVar),
            lookup_var_entry(VarTable, RHSVar, RHSVarEntry),
            RHSVarEntry ^ vte_name = ""
        then
            MainPieces = [words(StartWords), words("with")] ++
                color_as_subject([words(add_quotes(LHSVarName))])
        else
            RHSStr = unify_rhs_to_string(ModuleInfo, VarTable,
                print_name_only, RHS),
            MainPieces = [words(StartWords), words("of")] ++
                color_as_subject([words(add_quotes(LHSVarName))]) ++
                [words("and")] ++
                color_as_subject([words(add_quotes(RHSStr))])
        )
    ).

%---------------------------------------------------------------------------%

promise_solutions_kind_str(equivalent_solutions)
    = "promise_equivalent_solutions".
promise_solutions_kind_str(equivalent_solution_sets)
    = "promise_equivalent_solution_sets".
promise_solutions_kind_str(equivalent_solution_sets_arbitrary)
    = "arbitrary".

failing_contexts_description(ModuleInfo, VarTable, FailingContexts) =
    list.map(failing_context_description(ModuleInfo, VarTable),
        FailingContexts).

:- func failing_context_description(module_info, var_table,
    failing_context) = error_msg.

failing_context_description(ModuleInfo, VarTable, FailingContext) = Msg :-
    FailingContext = failing_context(Context, FailingGoal),
    (
        FailingGoal = incomplete_switch(Var),
        VarPiece = var_in_table_to_quote_piece(VarTable, Var),
        Pieces = [words("The")] ++
            color_as_subject([words("switch on"), VarPiece]) ++
            [words("is")] ++
            color_as_incorrect([words("incomplete.")]) ++ [nl]
    ;
        FailingGoal = fail_goal,
        Pieces = color_as_subject([words("Fail goal")]) ++
            color_as_incorrect([words("can fail.")]) ++ [nl]
    ;
        FailingGoal = test_goal(VarA, VarB),
        VarPieceA = var_in_table_to_quote_piece(VarTable, VarA),
        VarPieceB = var_in_table_to_quote_piece(VarTable, VarB),
        Pieces = [words("Unification of")] ++ color_as_subject([VarPieceA]) ++
            [words("and")] ++ color_as_subject([VarPieceB]) ++
            color_as_incorrect([words("can fail.")]) ++ [nl]
    ;
        FailingGoal = deconstruct_goal(Var, ConsId),
        VarPiece = var_in_table_to_quote_piece(VarTable, Var),
        Pieces = [words("Unification of")] ++ color_as_subject([VarPiece]) ++
            [words("with")] ++
            color_as_subject([qual_cons_id_and_maybe_arity(ConsId)]) ++
            color_as_incorrect([words("can fail.")]) ++ [nl]
    ;
        FailingGoal = call_goal(PredId, _ProcId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Name = pred_info_name(PredInfo),
        Pieces = [words("Call to")] ++ color_as_subject([fixed(Name)]) ++
            color_as_incorrect([words("can fail.")]) ++ [nl]
    ;
        FailingGoal = generic_call_goal(GenericCall),
        hlds_goal.generic_call_to_id(GenericCall, GenericCallId),
        GenericCallIdStr0 = generic_call_id_to_string(GenericCallId),
        GenericCallIdStr = capitalize_first(GenericCallIdStr0),
        Pieces = color_as_subject([words(GenericCallIdStr)]) ++
            color_as_incorrect([words("can fail.")]) ++ [nl]
    ;
        FailingGoal = negated_goal,
        Pieces = color_as_subject([words("Negated goal")]) ++
            color_as_incorrect([words("can fail.")]) ++ [nl]
    ),
    Msg = msg(Context, Pieces).

%---------------------------------------------------------------------------%

det_report_seen_call_id(ModuleInfo, SeenCall) = Pieces :-
    (
        SeenCall = seen_call(PredId, _),
        PredPieces = describe_one_pred_name(ModuleInfo, no,
            should_module_qualify, [], PredId),
        Pieces = [words("call to") | PredPieces]
    ;
        SeenCall = higher_order_call,
        Pieces = [words("higher-order call")]
    ).

%---------------------------------------------------------------------------%

:- type options_to_restore == assoc_list(option, option_data).

disable_det_warnings(OptionsToRestore, !Globals) :-
    globals.lookup_option(!.Globals, warn_simple_code, WarnSimple),
    globals.lookup_option(!.Globals, warn_det_decls_too_lax, WarnDeclsTooLax),
    globals.set_option(warn_simple_code, bool(no), !Globals),
    globals.set_option(warn_det_decls_too_lax, bool(no), !Globals),
    OptionsToRestore = [
        warn_simple_code - WarnSimple,
        warn_det_decls_too_lax - WarnDeclsTooLax
    ].

restore_det_warnings(OptionsToRestore, !Globals) :-
    list.foldl(restore_option, OptionsToRestore, !Globals).

:- pred restore_option(pair(option, option_data)::in,
    globals::in, globals::out) is det.

restore_option(Option - Value, !Globals) :-
    globals.set_option(Option, Value, !Globals).

%---------------------------------------------------------------------------%
:- end_module check_hlds.det_report.
%---------------------------------------------------------------------------%
