%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: det_report.m.
% Author: zs.
%
% This module handles reporting of determinism errors and warnings,
% as well as errors and warnings from some other related compiler passes
% such as simplify.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.det_report.
:- interface.

:- import_module check_hlds.det_util.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

    % Check all the determinism declarations in this module.
    % This is the main predicate exported by this module.
    %
:- pred global_checking_pass(pred_proc_list::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Check a lambda goal with the specified declared and inferred
    % determinisms.
    %
:- pred det_check_lambda(determinism::in, determinism::in, hlds_goal::in,
    hlds_goal_info::in, instmap::in, det_info::in, det_info::out) is det.

    % det_diagnose_conj(Goals, InstMap0, Desired, FailingContexts, DetInfo,
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
    list(error_msg)::out) is det.

    % Return a printable representation of the given promise_solutions_kind.
    %
:- func promise_solutions_kind_str(promise_solutions_kind) = string.

    % Return the name of the given variable in the given varset.
    %
:- func lookup_var_name_in_varset(prog_varset, prog_var) = string.

    % Describe the given list of failing contexts.
    %
:- func failing_contexts_description(module_info, prog_varset,
    list(failing_context)) = list(error_msg).

    % Describe a call we have seen.
    %
:- func det_report_seen_call_id(module_info, seen_call_id)
    = list(format_component).

%-----------------------------------------------------------------------------%

:- type options_to_restore.

    % Call this predicate before rerunning determinism analysis after an
    % optimization pass to disable all warnings. Errors will still be reported.
    %
:- pred disable_det_warnings(options_to_restore::out,
    globals::in, globals::out) is det.

:- pred restore_det_warnings(options_to_restore::in,
    globals::in, globals::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module getopt_io.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

global_checking_pass([], !ModuleInfo, !Specs).
global_checking_pass([Proc | Procs], !ModuleInfo, !Specs) :-
    Proc = proc(PredId, ProcId),
    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo),
    check_determinism(PredId, ProcId, PredInfo, ProcInfo, !ModuleInfo, !Specs),
    check_determinism_of_main(PredId, ProcId, PredInfo, ProcInfo, !Specs),
    check_for_multisoln_func(PredId, ProcId, PredInfo, ProcInfo, !.ModuleInfo,
        !Specs),
    global_checking_pass(Procs, !ModuleInfo, !Specs).

:- pred check_determinism(pred_id::in, proc_id::in, pred_info::in,
    proc_info::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_determinism(PredId, ProcId, PredInfo, ProcInfo, !ModuleInfo, !Specs) :-
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
            (
                ShouldIssueWarning = yes,

                % Don't report warnings for class method implementations --
                % the determinism in the `:- typeclass' declaration will be
                % the loosest of all possible instances. This is similar to
                % the reason we don't report warnings for lambda expressions.
                \+ check_marker(Markers, marker_class_instance_method),

                % Don't report warnings for procedures with no clauses.
                \+ check_marker(Markers, marker_stub),

                % Don't report warnings for predicates for which the user
                % has written a pragma requesting no warnings.
                \+ check_marker(Markers, marker_no_detism_warning),

                % Don't report warnings for compiler-generated Unify, Compare
                % or Index procedures, since the user has no way to shut
                % these up. These can happen for the Unify pred for the unit
                % type, if such types are not boxed (as they are not
                % boxed for the IL backend).
                \+ is_unify_or_compare_pred(PredInfo),

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
                pred_info_get_import_status(PredInfo, ImportStatus),
                status_defined_in_this_module(ImportStatus) = yes
            ->
                proc_info_get_detism_decl(ProcInfo, DetismDecl),
                MessagePieces = [words("warning:"),
                    words(detism_decl_name(DetismDecl)),
                    words("could be tighter."), nl
                ],
                report_determinism_problem(PredId, ProcId, !.ModuleInfo,
                    MessagePieces, DeclaredDetism, InferredDetism, ReportMsgs),
                ReportSpec = error_spec(severity_warning, phase_detism_check,
                    ReportMsgs),
                !:Specs = [ReportSpec | !.Specs]
            ;
                true
            )
        ;
            ( Cmp = first_detism_tighter_than
            ; Cmp = first_detism_incomparable
            ),
            proc_info_get_detism_decl(ProcInfo, DetismDecl),
            MessagePieces = [words("error:"),
                words(detism_decl_name(DetismDecl)),
                words("not satisfied."), nl],
            report_determinism_problem(PredId, ProcId, !.ModuleInfo,
                MessagePieces, DeclaredDetism, InferredDetism, ReportMsgs),
            proc_info_get_goal(ProcInfo, Goal),
            proc_info_get_vartypes(ProcInfo, VarTypes),
            proc_info_get_initial_instmap(ProcInfo, !.ModuleInfo, InstMap0),
            det_info_init(!.ModuleInfo, VarTypes, PredId, ProcId,
                pess_extra_vars_report, [], DetInfo0),
            det_diagnose_goal(Goal, InstMap0, DeclaredDetism, [],
                DetInfo0, DetInfo, GoalMsgs),
            det_info_get_module_info(DetInfo, !:ModuleInfo),
            sort_error_msgs(GoalMsgs, SortedGoalMsgs),
            ReportSpec = error_spec(severity_error, phase_detism_check,
                ReportMsgs ++ SortedGoalMsgs),
            !:Specs = [ReportSpec | !.Specs]
        )
    ),

    make_reqscope_checks_if_needed(!.ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo, !Specs),

    % Make sure the code model is valid given the eval method.
    proc_info_get_eval_method(ProcInfo, EvalMethod),
    Valid = valid_determinism_for_eval_method(EvalMethod, InferredDetism),
    (
        Valid = yes
    ;
        Valid = no,
        proc_info_get_context(ProcInfo, Context),
        MainPieces = [words("Error:"),
            pragma_decl(eval_method_to_pragma_name(EvalMethod)),
            words("declaration not allowed for procedure"),
            words("with determinism"),
            quote(determinism_to_string(InferredDetism)), suffix(".")],

        solutions.solutions(get_valid_dets(EvalMethod), Detisms),
        DetismStrs = list.map(determinism_to_string, Detisms),
        list.sort(DetismStrs, SortedDetismStrs),
        DetismPieces = list_to_pieces(SortedDetismStrs),
        VerbosePieces =
            [words("The pragma requested is only valid"),
            words("for the following"),
            words(choose_number(Detisms, "determinism", "determinisms")),
            suffix(":") |
            DetismPieces] ++ [suffix("."), nl],
        ValidSpec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context,
                [always(MainPieces),
                verbose_only(verbose_always, VerbosePieces)])]),
        !:Specs = [ValidSpec | !.Specs]
    ).

:- pred make_reqscope_checks_if_needed(module_info::in,
    pred_id::in, proc_id::in, pred_info::in, proc_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

make_reqscope_checks_if_needed(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo,
        !Specs) :-
    pred_info_get_markers(PredInfo, Markers),
    ( check_marker(Markers, marker_has_require_scope) ->
        proc_info_get_goal(ProcInfo, Goal),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap0),
        det_info_init(ModuleInfo, VarTypes, PredId, ProcId,
            pess_extra_vars_ignore, [], DetInfo0),
        reqscope_check_goal(Goal, InstMap0, DetInfo0, DetInfo),
        det_info_get_error_specs(DetInfo, RCSSpecs),
        !:Specs = RCSSpecs ++ !.Specs
    ;
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
        % unexpected($module, $pred, "detism_decl_name: detism_decl_none")
    ).

:- pred get_valid_dets(eval_method::in, determinism::out) is nondet.

get_valid_dets(EvalMethod, Detism) :-
    determinism(Detism),
    valid_determinism_for_eval_method(EvalMethod, Detism) = yes.

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

:- pred check_determinism_of_main(pred_id::in, proc_id::in,
    pred_info::in, proc_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_determinism_of_main(_PredId, _ProcId, PredInfo, ProcInfo, !Specs) :-
    % Check that `main/2' has determinism `det' or `cc_multi',
    % as required by the language reference manual.
    proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
    (
        pred_info_name(PredInfo) = "main",
        pred_info_orig_arity(PredInfo) = 2,
        pred_info_is_exported(PredInfo),
        MaybeDetism = yes(DeclaredDetism),
        \+ (
            DeclaredDetism = detism_det
        ;
            DeclaredDetism = detism_cc_multi
        )
    ->
        proc_info_get_context(ProcInfo, ProcContext),
        Pieces = [words("Error:"), sym_name_and_arity(unqualified("main") / 2),
            words("must be"), quote("det"), words("or"), quote("cc_multi"),
            suffix(".")],
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(ProcContext, [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ;
        true
    ).

:- pred check_for_multisoln_func(pred_id::in, proc_id::in, pred_info::in,
    proc_info::in, module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_multisoln_func(PredId, _ProcId, PredInfo, ProcInfo, ModuleInfo,
        !Specs) :-
    proc_info_get_inferred_determinism(ProcInfo, InferredDetism),

    % Functions can only have more than one solution if it is a non-standard
    % mode. Otherwise, they would not be referentially transparent.
    % (Nondeterministic "functions" like C's `rand()' function are not
    % allowed.)
    (
        % If it is a mode for a function...
        pred_info_is_pred_or_func(PredInfo) = pf_function,
        % ... that can succeed more than once ...
        determinism_components(InferredDetism, _CanFail, NumSolns),
        NumSolns \= at_most_zero,
        NumSolns \= at_most_one,
        % ... but for which all the arguments are input ...
        proc_info_get_argmodes(ProcInfo, PredArgModes),
        pred_args_to_func_args(PredArgModes, FuncArgModes, _FuncResultMode),
        (
            list.member(FuncArgMode, FuncArgModes)
        =>
            mode_is_fully_input(ModuleInfo, FuncArgMode)
        )
    ->
        % ... then it is an error.
        proc_info_get_context(ProcInfo, FuncContext),
        proc_info_get_inst_varset(ProcInfo, InstVarSet),
        PredModePieces = describe_one_pred_name_mode(ModuleInfo,
            should_not_module_qualify, PredId, InstVarSet, PredArgModes),
        MainPieces = [words("Error: invalid determinism for")]
            ++ PredModePieces ++ [suffix(":"), nl,
            words("the primary mode of a function cannot be"),
            quote(mercury_det_to_string(InferredDetism)), suffix(".")],
        VerbosePieces = func_primary_mode_det_msg,
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(FuncContext,
                [always(MainPieces),
                verbose_only(verbose_once, VerbosePieces)])]),
        !:Specs = [Spec | !.Specs]
    ;
        true
    ).

:- func func_primary_mode_det_msg = format_components.

func_primary_mode_det_msg = [
    words("In Mercury, a function is supposed to be a true mathematical"),
    words("function of its arguments; that is, the value of the function's"),
    words("result should be determined only by the values of its arguments."),
    words("(Allowing functions to have more than one result for the same"),
    words("arguments would break referential transparency.)"),
    words("Most likely, this procedure should be a predicate, not a function.")
    ].

det_check_lambda(DeclaredDetism, InferredDetism, Goal, GoalInfo, InstMap0,
        !DetInfo) :-
    compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
    (
        ( Cmp = first_detism_tighter_than
        ; Cmp = first_detism_incomparable
        ),
        det_info_get_pred_id(!.DetInfo, PredId),
        det_info_get_proc_id(!.DetInfo, ProcId),
        Context = goal_info_get_context(GoalInfo),
        det_info_get_module_info(!.DetInfo, ModuleInfo),
        PredPieces = describe_one_proc_name_mode(ModuleInfo,
            should_not_module_qualify, proc(PredId, ProcId)),
        Pieces =
            [words("In")] ++ PredPieces ++ [suffix(":"), nl,
            words("Determinism error in lambda expression."), nl,
            words("Declared"),
            quote(determinism_to_string(DeclaredDetism)), suffix(","),
            words("inferred"),
            quote(determinism_to_string(InferredDetism)), suffix("'.")],
        det_diagnose_goal(Goal, InstMap0, DeclaredDetism, [], !DetInfo,
            GoalMsgs),
        sort_error_msgs(GoalMsgs, SortedGoalMsgs),
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context, [always(Pieces)])] ++ SortedGoalMsgs),
        det_info_add_error_spec(Spec, !DetInfo)
    ;
        ( Cmp = first_detism_same_as
        ; Cmp = first_detism_looser_than
        )
        % We don't bother issuing warnings if the determinism was too loose;
        % that will often be the case, and should not be warned about.
    ).

:- pred report_determinism_problem(pred_id::in, proc_id::in, module_info::in,
    format_components::in, determinism::in, determinism::in,
    list(error_msg)::out) is det.

report_determinism_problem(PredId, ProcId, ModuleInfo, MessagePieces,
        DeclaredDetism, InferredDetism, Msgs) :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_get_context(ProcInfo, Context),
    ProcPieces = describe_one_proc_name_mode(ModuleInfo,
        should_not_module_qualify, proc(PredId, ProcId)),
    Pieces = [words("In")] ++ ProcPieces ++ [suffix(":"), nl] ++
        MessagePieces ++
        [
            nl,
            words("Declared"),
            quote(determinism_to_string(DeclaredDetism)), suffix(","),
            words("inferred"),
            quote(determinism_to_string(InferredDetism)), suffix(".")
        ],
    Msgs = [simple_msg(Context, [always(Pieces)])].

%-----------------------------------------------------------------------------%

    % The given goal should have determinism Desired, but doesn't.
    % Find out what is wrong, and return a list of messages giving the causes.
    %
:- pred det_diagnose_goal(hlds_goal::in, instmap::in, determinism::in,
    list(switch_context)::in, det_info::in, det_info::out,
    list(error_msg)::out) is det.

det_diagnose_goal(Goal, InstMap0, Desired, SwitchContexts, !DetInfo, Msgs) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    Actual = goal_info_get_determinism(GoalInfo),
    compare_determinisms(Desired, Actual, CompareResult),
    (
        ( CompareResult = first_detism_tighter_than
        ; CompareResult = first_detism_incomparable
        ),
        det_diagnose_goal_expr(GoalExpr, GoalInfo, InstMap0, Desired, Actual,
            SwitchContexts, !DetInfo, Msgs)
    ;
        ( CompareResult = first_detism_same_as
        ; CompareResult = first_detism_looser_than
        ),
        Msgs = []
    ).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_goal_expr(hlds_goal_expr::in, hlds_goal_info::in,
    instmap::in, determinism::in, determinism::in, list(switch_context)::in,
    det_info::in, det_info::out, list(error_msg)::out) is det.

det_diagnose_goal_expr(GoalExpr, GoalInfo, InstMap0, Desired, Actual,
        SwitchContexts, !DetInfo, Msgs) :-
    (
        GoalExpr = conj(_, Goals),
        det_diagnose_conj(Goals, InstMap0, Desired, SwitchContexts, !DetInfo,
            Msgs)
    ;
        GoalExpr = disj(Goals),
        det_diagnose_disj(Goals, InstMap0, Desired, Actual, SwitchContexts,
            !DetInfo, 0, DisjunctsWithSoln, Msgs1),
        determinism_components(Desired, _, DesSolns),
        (
            DesSolns \= at_most_many,
            DesSolns \= at_most_many_cc,
            DisjunctsWithSoln > 1
        ->
            Context = goal_info_get_context(GoalInfo),
            det_diagnose_switch_context(!.DetInfo, SwitchContexts,
                NestingPieces),
            DisjPieces =
                [words("Disjunction has multiple clauses with solutions.")],
            Pieces = NestingPieces ++ [lower_case_next_if_not_first]
                ++ DisjPieces,
            Msg = simple_msg(Context, [always(Pieces)]),
            Msgs = [Msg] ++ Msgs1
        ;
            Msgs = Msgs1
        )
    ;
        GoalExpr = switch(Var, SwitchCanFail, Cases),
        % The determinism of a switch is the worst of the determinism of each
        % of the cases. Also, if only a subset of the constructors are handled,
        % then it is semideterministic or worse - this is determined
        % in switch_detection.m and handled via the CanFail field.
        (
            SwitchCanFail = can_fail,
            determinism_components(Desired, cannot_fail, _)
        ->
            Context = goal_info_get_context(GoalInfo),
            det_diagnose_switch_context(!.DetInfo, SwitchContexts,
                NestingPieces),
            find_missing_cons_ids(!.DetInfo, InstMap0, Var, Cases,
                VarStr, MaybeMissingPieces),
            (
                MaybeMissingPieces = yes(MissingPieces),
                Pieces = [words("The switch on"), fixed(VarStr),
                    words("does not cover") | MissingPieces]
            ;
                MaybeMissingPieces = no,
                Pieces = [words("The switch on"), fixed(VarStr),
                    words("can fail.")]
            ),
            Msgs1 = [simple_msg(Context, [always(NestingPieces ++ Pieces)])]
        ;
            Msgs1 = []
        ),
        det_info_get_vartypes(!.DetInfo, VarTypes),
        lookup_var_type(VarTypes, Var, VarType),
        det_diagnose_switch_arms(Var, VarType, Cases, InstMap0,
            Desired, SwitchContexts, !DetInfo, Msgs2),
        Msgs = Msgs1 ++ Msgs2
    ;
        GoalExpr = plain_call(PredId, ProcId, _, _, CallContext, _),
        Context = goal_info_get_context(GoalInfo),
        det_report_call_context(Context, CallContext, !.DetInfo,
            PredId, ProcId, InitMsgs, StartingPieces),
        det_diagnose_primitive_goal(Desired, Actual, Context, StartingPieces,
            AtomicMsgs),
        Msgs = InitMsgs ++ AtomicMsgs
    ;
        GoalExpr = generic_call(GenericCall, _, _, _, _),
        Context = goal_info_get_context(GoalInfo),
        hlds_goal.generic_call_to_id(GenericCall, GenericCallId),
        StartingPieces = [words(generic_call_id_to_string(GenericCallId))],
        det_diagnose_primitive_goal(Desired, Actual, Context, StartingPieces,
            Msgs)
    ;
        GoalExpr = unify(LHS, RHS, _, _, UnifyContext),
        Context = goal_info_get_context(GoalInfo),
        det_report_unify_context(is_first, is_last, Context, UnifyContext,
            !.DetInfo, LHS, RHS, StartingPieces),
        det_diagnose_primitive_goal(Desired, Actual, Context, StartingPieces,
            Msgs)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        Context = goal_info_get_context(GoalInfo),
        DesiredStr = determinism_to_string(Desired),
        Pieces = [words("Determinism declaration not satisfied."),
            words("Desired determinism is " ++ DesiredStr ++ ".")],
        Msgs = [simple_msg(Context, [always(Pieces)])]
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        determinism_components(Desired, _DesiredCanFail, DesiredSolns),
        Cond = hlds_goal(_CondGoal, CondInfo),
        CondDetism = goal_info_get_determinism(CondInfo),
        determinism_components(CondDetism, _CondCanFail, CondSolns),
        (
            CondSolns = at_most_many,
            DesiredSolns \= at_most_many
        ->
            determinism_components(DesiredCond, can_fail, DesiredSolns),
            det_diagnose_goal(Cond, InstMap0, DesiredCond, SwitchContexts,
                !DetInfo, MsgsCond)
        ;
            MsgsCond = []
        ),
        update_instmap(Cond, InstMap0, InstMap1),
        det_diagnose_goal(Then, InstMap1, Desired, SwitchContexts, !DetInfo,
            MsgsThen),
        det_diagnose_goal(Else, InstMap0, Desired, SwitchContexts, !DetInfo,
            MsgsElse),
        Msgs = MsgsCond ++ MsgsThen ++ MsgsElse
    ;
        GoalExpr = negation(_),
        determinism_components(Desired, DesiredCanFail, DesiredSolns),
        determinism_components(Actual, ActualCanFail, ActualSolns),
        (
            DesiredCanFail = cannot_fail,
            ActualCanFail = can_fail
        ->
            Context = goal_info_get_context(GoalInfo),
            Pieces = [words("Negated goal can succeed.")],
            Msgs = [simple_msg(Context, [always(Pieces)])]
        ;
            DesiredSolns = at_most_zero,
            ActualSolns \= at_most_zero
        ->
            Context = goal_info_get_context(GoalInfo),
            Pieces = [words("Negated goal can fail.")],
            Msgs = [simple_msg(Context, [always(Pieces)])]
        ;
            Msgs = []
        )
    ;
        GoalExpr = scope(_, SubGoal),
        SubGoal = hlds_goal(_, SubGoalInfo),
        Internal = goal_info_get_determinism(SubGoalInfo),
        ( Actual = Internal ->
            InternalDesired = Desired
        ;
            determinism_components(Desired, CanFail, _),
            determinism_components(InternalDesired, CanFail, at_most_many)
        ),
        det_diagnose_goal(SubGoal, InstMap0, InternalDesired, SwitchContexts,
            !DetInfo, Msgs)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            det_diagnose_goal(MainGoal, InstMap0, Desired,
                SwitchContexts, !DetInfo, MainMsgs),
            det_diagnose_orelse_goals(OrElseGoals, InstMap0, Desired,
                SwitchContexts, !DetInfo, OrElseMsgs),
            Msgs = MainMsgs ++ OrElseMsgs
        ;
            ShortHand = try_goal(_, _, SubGoal),
            det_diagnose_goal(SubGoal, InstMap0, Desired, SwitchContexts,
                !DetInfo, Msgs)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($module, $pred, "bi_implication")
        )
    ).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_primitive_goal(determinism::in, determinism::in,
    prog_context::in, list(format_component)::in, list(error_msg)::out) is det.

det_diagnose_primitive_goal(Desired, Actual, Context, StartingPieces, Msgs) :-
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
            CanFailPieces = [_ | _],
            ConnectPieces = [words("and")]
        ;
            CanFailPieces = [],
            ConnectPieces = []
        ),
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
        ConnectPieces = [],
        SolnsPieces = []
    ),
    RawPieces = CanFailPieces ++ ConnectPieces ++ SolnsPieces,
    (
        RawPieces = [_ | _],
        Pieces = RawPieces ++ [suffix("."), nl]
    ;
        RawPieces = [],
        Pieces = [words("has unknown determinism problem;"), nl,
            words("desired determinism is"),
            fixed(determinism_to_string(Desired)), suffix(","), nl,
            words("while actual determinism is"),
            fixed(determinism_to_string(Actual)), suffix("."), nl]
    ),
    Msgs = [simple_msg(Context, [always(StartingPieces ++ Pieces)])].

det_diagnose_conj([], _InstMap0, _Desired, _SwitchContexts, !DetInfo, []).
det_diagnose_conj([Goal | Goals], InstMap0, Desired, SwitchContexts, !DetInfo,
        Msgs) :-
    det_diagnose_goal(Goal, InstMap0, Desired, SwitchContexts, !DetInfo,
        Msgs1),
    update_instmap(Goal, InstMap0, InstMap1),
    det_diagnose_conj(Goals, InstMap1, Desired, SwitchContexts, !DetInfo,
        Msgs2),
    Msgs = Msgs1 ++ Msgs2.

:- pred det_diagnose_disj(list(hlds_goal)::in, instmap::in,
    determinism::in, determinism::in, list(switch_context)::in,
    det_info::in, det_info::out, int::in, int::out, list(error_msg)::out)
    is det.

det_diagnose_disj([], _InstMap0, _Desired, _Actual, _SwitchContexts,
        !DetInfo, !DisjunctsWithSoln, []).
det_diagnose_disj([Goal | Goals], InstMap0, Desired, Actual, SwitchContexts,
        !DetInfo, !DisjunctsWithSoln, Msgs) :-
    determinism_components(Actual, ActualCanFail, _),
    determinism_components(Desired, DesiredCanFail, DesiredSolns),
    (
        DesiredCanFail = cannot_fail,
        ActualCanFail = can_fail
    ->
        % If the disjunction was declared to never fail, but we inferred that
        % it might fail, then we want to print an error message for every
        % disjunct that might fail.
        ClauseCanFail = cannot_fail
    ;
        % Otherwise, either the disjunction is allowed to fail, or there is
        % at least one disjunct that we inferred won't fail, so we don't want
        % any error messages for the disjuncts that might fail.
        ClauseCanFail = can_fail
    ),
    determinism_components(ClauseDesired, ClauseCanFail, DesiredSolns),
    det_diagnose_goal(Goal, InstMap0, ClauseDesired, SwitchContexts, !DetInfo,
        Msgs1),
    (
        Goal = hlds_goal(_, GoalInfo),
        GoalDetism = goal_info_get_determinism(GoalInfo),
        determinism_components(GoalDetism, _, at_most_zero)
    ->
        true
    ;
        !:DisjunctsWithSoln = !.DisjunctsWithSoln + 1
    ),
    det_diagnose_disj(Goals, InstMap0, Desired, Actual, SwitchContexts,
        !DetInfo, !DisjunctsWithSoln, Msgs2),
    Msgs = Msgs1 ++ Msgs2.

:- pred det_diagnose_switch_arms(prog_var::in, mer_type::in, list(case)::in,
    instmap::in, determinism::in, list(switch_context)::in,
    det_info::in, det_info::out, list(error_msg)::out) is det.

det_diagnose_switch_arms(_Var, _VarType, [], _, _Desired, _SwitchContexts,
        !DetInfo, []).
det_diagnose_switch_arms(Var, VarType, [Case | Cases], InstMap0, Desired,
        SwitchContexts0, !DetInfo, Msgs) :-
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
        !DetInfo, Msgs1),
    det_diagnose_switch_arms(Var, VarType, Cases, InstMap0, Desired,
        SwitchContexts0, !DetInfo, Msgs2),
    Msgs = Msgs1 ++ Msgs2.

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
    (
        GoalExpr = unify(_, _, _, Unification, _),
        Unification = deconstruct(Var, MainConsId, ArgVars, _, _, _),
        list.member(Var, !.SwitchVarSynonyms),
        OtherConsIds = []
    ->
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        ArgVarsSet = set_of_var.list_to_set(ArgVars),
        (
            set_of_var.intersect(NonLocals, ArgVarsSet, NonLocalArgVarsSet),
            set_of_var.is_non_empty(NonLocalArgVarsSet)
        ->
            MaybeArgVars = yes(ArgVars)
        ;
            MaybeArgVars = no
        ),
        MainMatch = switch_match(MainConsId, MaybeArgVars),
        OtherMatches = []
    ;
        GoalExpr = disj(Disjuncts),
        find_switch_var_submatches(Disjuncts, !.SwitchVarSynonyms,
            yes(MainConsId), OtherConsIds, yes(MainMatch0), OtherMatches0)
    ->
        MainMatch = MainMatch0,
        OtherMatches = OtherMatches0
    ;
        (
            GoalExpr = unify(_, _, _, Unification, _),
            Unification = assign(ToVar, FromVar),
            list.member(FromVar, !.SwitchVarSynonyms)
        ->
            !:SwitchVarSynonyms = [ToVar | !.SwitchVarSynonyms]
        ;
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
    (
        MaybeMainConsId = yes(MainConsId),
        ConsId = MainConsId
    ->
        find_switch_var_submatches(Disjuncts, SwitchVarSynonyms,
            no, OtherConsIds, no, OtherMatches),
        MaybeMainMatch = yes(switch_match(ConsId, yes(ArgVars)))
    ;
        list.delete_first(OtherConsIds, ConsId, LeftOverConsIds)
    ->
        find_switch_var_submatches(Disjuncts, SwitchVarSynonyms,
            MaybeMainConsId, LeftOverConsIds, MaybeMainMatch, LeftOverMatches),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        set_of_var.list_to_set(ArgVars, ArgVarsSet),
        (
            set_of_var.intersect(NonLocals, ArgVarsSet, NonLocalArgVarsSet),
            set_of_var.is_non_empty(NonLocalArgVarsSet)
        ->
            MaybeArgVars = yes(ArgVars)
        ;
            MaybeArgVars = no
        ),
        OtherMatches = [switch_match(ConsId, MaybeArgVars) | LeftOverMatches]
    ;
        fail
    ).

:- pred make_switch_match_no_args(cons_id::in, switch_match::out) is det.

make_switch_match_no_args(ConsId, Match) :-
    Match = switch_match(ConsId, no).

:- pred det_diagnose_orelse_goals(list(hlds_goal)::in, instmap::in,
    determinism::in, list(switch_context)::in, det_info::in, det_info::out,
    list(error_msg)::out) is det.

det_diagnose_orelse_goals([], _, _Desired, _SwitchContexts, !DetInfo, []).
det_diagnose_orelse_goals([Goal | Goals], InstMap0, Desired, SwitchContexts0,
        !DetInfo, Msgs) :-
    % XXX Once we start using STM in earnest, we should add something
    % representing "In orelse arm #n:" to the switch context.
    det_diagnose_goal(Goal, InstMap0, Desired, SwitchContexts0,
        !DetInfo, Msgs1),
    det_diagnose_orelse_goals(Goals, InstMap0, Desired, SwitchContexts0,
        !DetInfo, Msgs2),
    Msgs = Msgs1 ++ Msgs2.

%-----------------------------------------------------------------------------%

    % Check that the switches in all require_complete_switch scopes are
    % actually complete. If they are not, add an error message to !DetInfo.
    %
:- pred reqscope_check_goal(hlds_goal::in, instmap::in,
    det_info::in, det_info::out) is det.

reqscope_check_goal(Goal, InstMap0, !DetInfo) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = conj(_, Goals),
        reqscope_check_conj(Goals, InstMap0, !DetInfo)
    ;
        GoalExpr = disj(Goals),
        reqscope_check_disj(Goals, InstMap0, !DetInfo)
    ;
        GoalExpr = switch(Var, _, Cases),
        det_info_get_vartypes(!.DetInfo, VarTypes),
        lookup_var_type(VarTypes, Var, VarType),
        reqscope_check_switch(Var, VarType, Cases, InstMap0, !DetInfo)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        reqscope_check_goal(Cond, InstMap0, !DetInfo),
        update_instmap(Cond, InstMap0, InstMap1),
        reqscope_check_goal(Then, InstMap1, !DetInfo),
        reqscope_check_goal(Else, InstMap0, !DetInfo)
    ;
        GoalExpr = negation(SubGoal),
        reqscope_check_goal(SubGoal, InstMap0, !DetInfo)
    ;
        GoalExpr = scope(Reason, SubGoal),
        reqscope_check_scope(Reason, SubGoal, GoalInfo, InstMap0, !DetInfo),
        reqscope_check_goal(SubGoal, InstMap0, !DetInfo)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            reqscope_check_goal(MainGoal, InstMap0, !DetInfo),
            reqscope_check_disj(OrElseGoals, InstMap0, !DetInfo)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            reqscope_check_goal(SubGoal, InstMap0, !DetInfo)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($module, $pred, "bi_implication")
        )
    ;
        GoalExpr = unify(_, RHS, _, _, _),
        (
            ( RHS = rhs_var(_)
            ; RHS = rhs_functor(_, _, _)
            )
        ;
            RHS = rhs_lambda_goal(_Purity, _Groundness, _PorF, _EvalMethod,
                _LambdaNonLocals, Vars, Modes, _Detism, LambdaGoal),
            assoc_list.from_corresponding_lists(Vars, Modes, VarsModes),
            det_info_get_module_info(!.DetInfo, ModuleInfo),
            lambda_update_instmap(VarsModes, ModuleInfo,
                InstMap0, LambdaInstMap0),
            reqscope_check_goal(LambdaGoal, LambdaInstMap0, !DetInfo)
        )
    ;
        ( GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        )
    ).

:- pred reqscope_check_scope(scope_reason::in, hlds_goal::in,
    hlds_goal_info::in, instmap::in, det_info::in, det_info::out) is det.

reqscope_check_scope(Reason, SubGoal, ScopeGoalInfo, InstMap0, !DetInfo) :-
    (
        Reason = require_detism(RequiredDetism),
        reqscope_check_goal_detism(RequiredDetism, SubGoal,
            check_require_detism(ScopeGoalInfo), InstMap0, !DetInfo)
    ;
        Reason = require_complete_switch(RequiredVar),
        % We must test the version of the subgoal that has not yet been
        % simplified, since simplification can convert a complete switch
        % into an incomplete switch by deleting an arm that contains
        % only `fail'.
        SubGoal = hlds_goal(SubGoalExpr, _),
        (
            SubGoalExpr = switch(SwitchVar, CanFail, Cases),
            SwitchVar = RequiredVar
        ->
            (
                CanFail = cannot_fail
            ;
                CanFail = can_fail,
                find_missing_cons_ids(!.DetInfo, InstMap0, SwitchVar, Cases,
                    VarStr, MaybeMissingPieces),
                (
                    MaybeMissingPieces = yes(MissingPieces),
                    SwitchPieces = [words("Error: the switch on"),
                        quote(VarStr),
                        words("is required to be complete,"),
                        words("but it does not cover") | MissingPieces]
                ;
                    MaybeMissingPieces = no,
                    SwitchPieces = [words("Error: the switch on"),
                        quote(VarStr),
                        words("is required to be complete,"),
                        words("but it is not.")]
                ),
                Context = goal_info_get_context(ScopeGoalInfo),
                SwitchMsg = simple_msg(Context,
                    [always(SwitchPieces)]),
                SwitchSpec = error_spec(severity_error, phase_detism_check,
                    [SwitchMsg]),
                det_info_add_error_spec(SwitchSpec, !DetInfo)
            )
        ;
            generate_warning_for_switch_var_if_missing(RequiredVar, SubGoal,
                ScopeGoalInfo, !DetInfo)
        )
    ;
        Reason = require_switch_arms_detism(RequiredVar, RequiredDetism),
        SubGoal = hlds_goal(SubGoalExpr, _),
        (
            SubGoalExpr = switch(SwitchVar, _CanFail, Cases),
            SwitchVar = RequiredVar
        ->
            det_info_get_vartypes(!.DetInfo, VarTypes),
            lookup_var_type(VarTypes, SwitchVar, SwitchVarType),
            reqscope_check_goal_detism_for_cases(RequiredDetism,
                SwitchVar, SwitchVarType, Cases,
                check_require_switch_arms_detism, InstMap0, !DetInfo)
        ;
            generate_warning_for_switch_var_if_missing(RequiredVar, SubGoal,
                ScopeGoalInfo, !DetInfo)
        )
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
            unexpected($module, $pred,
                "Loop control scope with strange determinism")
        )
    ;
        ( Reason = exist_quant(_)
        ; Reason = commit(_)
        ; Reason = barrier(_)
        ; Reason = promise_purity(_)
        ; Reason = promise_solutions(_, _)
        ; Reason = from_ground_term(_, _)
        ; Reason = trace_goal(_, _, _, _, _)
        )
    ).

    % Are we checking the determinism of a goal for a require_{det,...} scope,
    % or for a require_switch_arms_{det,...} scope?
    %
:- type detism_check_kind
    --->    check_require_detism(hlds_goal_info)
    ;       check_require_switch_arms_detism.

:- pred reqscope_check_goal_detism(determinism::in, hlds_goal::in,
    detism_check_kind::in, instmap::in, det_info::in, det_info::out) is det.

reqscope_check_goal_detism(RequiredDetism, Goal, CheckKind, InstMap0,
        !DetInfo) :-
    Goal = hlds_goal(_, GoalInfo),
    ActualDetism = goal_info_get_determinism(GoalInfo),
    compare_determinisms(ActualDetism, RequiredDetism, CompareResult),
    (
        (
            CheckKind = check_require_detism(_),
            % For require_detism scopes, the programmer requires an exact
            % match.
            % keyword is the most appropriate scope.
            CompareResult = first_detism_same_as
        ;
            CheckKind = check_require_switch_arms_detism,
            % For require_switch_arms_detism scopes, the programmer requires
            % only that each switch arm's determinism must be at least as tight
            % as RequiredDetism.
            ( CompareResult = first_detism_tighter_than
            ; CompareResult = first_detism_same_as
            )
        )
    ->
        true
    ;
        (
            CheckKind = check_require_detism(ScopeGoalInfo),
            % For require_detism scopes, the context of the require_detism
            % keyword is the most appropriate scope.
            Context = goal_info_get_context(ScopeGoalInfo)
        ;
            CheckKind = check_require_switch_arms_detism,
            % For require_switch_arms_detism scopes, the context of the
            % require_switch_arms_detism keyword won't work, since it
            % won't tell the user *which* arm's determinism isn't right.
            % We have to use the context of the switch arm itself.
            Context = goal_info_get_context(GoalInfo)
        ),
        RequiredDetismStr = determinism_to_string(RequiredDetism),
        ActualDetismStr = determinism_to_string(ActualDetism),
        det_diagnose_goal(Goal, InstMap0, RequiredDetism, [], !DetInfo,
            SubMsgs),
        Pieces = [words("Error: required determinism is"),
            quote(RequiredDetismStr), suffix(","),
            words("but actual determinism is"),
            quote(ActualDetismStr), suffix("."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_detism_check, [Msg | SubMsgs]),
        det_info_add_error_spec(Spec, !DetInfo)
    ).

:- pred reqscope_check_goal_detism_for_cases(determinism::in,
    prog_var::in, mer_type::in, list(case)::in,
    detism_check_kind::in, instmap::in, det_info::in, det_info::out) is det.

reqscope_check_goal_detism_for_cases(_RequiredDetism, _Var, _VarType,
        [], _CheckKind, _InstMap0, !DetInfo).
reqscope_check_goal_detism_for_cases(RequiredDetism, Var, VarType,
        [Case | Cases], CheckKind, InstMap0, !DetInfo) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    det_info_get_module_info(!.DetInfo, ModuleInfo0),
    bind_var_to_functors(Var, VarType, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
    det_info_set_module_info(ModuleInfo, !DetInfo),

    reqscope_check_goal_detism(RequiredDetism, Goal, CheckKind, InstMap1,
        !DetInfo),
    reqscope_check_goal_detism_for_cases(RequiredDetism, Var, VarType,
        Cases, CheckKind, InstMap0, !DetInfo).

    % Emit a warning if the variable in the head of a require_complete_switch
    % or require_switch_arms_detism scope does not occur somewhere in the goal
    % inside the scope. Note that since the goal inside the scope does not
    % need to be a switch, the variable will not necessarily appear in the
    % non-locals set. We only emit the warning when the variable does
    % not occur at all.
    %
:- pred generate_warning_for_switch_var_if_missing(prog_var::in, hlds_goal::in,
    hlds_goal_info::in, det_info::in, det_info::out) is det.

generate_warning_for_switch_var_if_missing(RequiredVar, Goal, ScopeGoalInfo,
        !DetInfo) :-
    goal_vars(Goal, GoalVars),
    ( if set_of_var.member(GoalVars, RequiredVar) then
        true
    else
        det_get_proc_info(!.DetInfo, ProcInfo),
        proc_info_get_varset(ProcInfo, VarSet),
        VarStr = mercury_var_to_string(VarSet, no, RequiredVar),
        MissingRequiredPieces = [
            words("Warning: variable "), quote(VarStr),
            words("is the subject of a require_complete_switch scope"),
            words("but it does not occur in the sub-goal.")
        ],
        Context = goal_info_get_context(ScopeGoalInfo),
        MissingRequiredMsg = simple_msg(Context,
            [always(MissingRequiredPieces)]),
        MissingRequiredSpec = error_spec(severity_warning,
            phase_detism_check, [MissingRequiredMsg]),
        det_info_add_error_spec(MissingRequiredSpec, !DetInfo)
    ).

%-----------------------------------------------------------------------------%

:- pred reqscope_check_conj(list(hlds_goal)::in, instmap::in,
    det_info::in, det_info::out) is det.

reqscope_check_conj([], _InstMap0, !DetInfo).
reqscope_check_conj([Goal | Goals], InstMap0, !DetInfo) :-
    reqscope_check_goal(Goal, InstMap0, !DetInfo),
    update_instmap(Goal, InstMap0, InstMap1),
    reqscope_check_conj(Goals, InstMap1, !DetInfo).

:- pred reqscope_check_disj(list(hlds_goal)::in, instmap::in,
    det_info::in, det_info::out) is det.

reqscope_check_disj([], _InstMap0, !DetInfo).
reqscope_check_disj([Goal | Goals], InstMap0, !DetInfo) :-
    reqscope_check_goal(Goal, InstMap0, !DetInfo),
    reqscope_check_disj(Goals, InstMap0, !DetInfo).

:- pred reqscope_check_switch(prog_var::in, mer_type::in, list(case)::in,
    instmap::in, det_info::in, det_info::out) is det.

reqscope_check_switch(_Var, _VarType, [], _InstMap0, !DetInfo).
reqscope_check_switch(Var, VarType, [Case | Cases], InstMap0, !DetInfo) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    det_info_get_module_info(!.DetInfo, ModuleInfo0),
    bind_var_to_functors(Var, VarType, MainConsId, OtherConsIds,
        InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
    det_info_set_module_info(ModuleInfo, !DetInfo),
    reqscope_check_goal(Goal, InstMap1, !DetInfo),
    reqscope_check_switch(Var, VarType, Cases, InstMap0, !DetInfo).

:- pred lambda_update_instmap(assoc_list(prog_var, mer_mode)::in,
    module_info::in, instmap::in, instmap::out) is det.

lambda_update_instmap([], _ModuleInfo, !InstMap).
lambda_update_instmap([Var - Mode | VarsModes], ModuleInfo, !InstMap) :-
    mode_get_insts(ModuleInfo, Mode, InitInst, _FinalInst),
    instmap_set_var(Var, InitInst, !InstMap),
    lambda_update_instmap(VarsModes, ModuleInfo, !InstMap).

%-----------------------------------------------------------------------------%

:- pred find_missing_cons_ids(det_info::in, instmap::in, prog_var::in,
    list(case)::in, string::out, maybe(list(format_component))::out) is det.

find_missing_cons_ids(DetInfo, InstMap0, Var, Cases, VarStr,
        MaybeMissingPieces) :-
    det_get_proc_info(DetInfo, ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    VarStr = mercury_var_to_string(VarSet, no, Var),
    det_info_get_module_info(DetInfo, ModuleInfo),
    (
        (
            instmap_lookup_var(InstMap0, Var, VarInst),
            inst_is_bound_to_functors(ModuleInfo, VarInst, BoundInsts)
        ->
            det_info_get_vartypes(DetInfo, VarTypes),
            lookup_var_type(VarTypes, Var, VarType),
            type_to_ctor_det(VarType, VarTypeCtor),
            list.map(bound_inst_to_cons_id(VarTypeCtor),
                BoundInsts, ConsIds)
        ;
            det_lookup_var_type(ModuleInfo, ProcInfo, Var, TypeDefn),
            hlds_data.get_type_defn_body(TypeDefn, TypeBody),
            ConsTable = TypeBody ^ du_type_cons_tag_values,
            map.keys(ConsTable, ConsIds)
        )
    ->
        det_diagnose_missing_consids(ConsIds, Cases, MissingConsIds),
        cons_id_list_to_pieces(MissingConsIds, MissingPieces),
        MaybeMissingPieces = yes(MissingPieces)
    ;
        MaybeMissingPieces = no
    ).

:- pred det_diagnose_missing_consids(list(cons_id)::in, list(case)::in,
    list(cons_id)::out) is det.

det_diagnose_missing_consids(ConsIds, Cases, MissingConsIds) :-
    compute_covered_cons_ids(Cases, set_tree234.init, CoveredConsIds),
    find_uncovered_consids(ConsIds, CoveredConsIds, [], RevMissingConsIds),
    list.reverse(RevMissingConsIds, MissingConsIds).

:- pred find_uncovered_consids(list(cons_id)::in, set_tree234(cons_id)::in,
    list(cons_id)::in, list(cons_id)::out) is det.

find_uncovered_consids([], _, !RevMissingConsIds).
find_uncovered_consids([ConsId | ConsIds], CoveredConsIds,
        !RevMissingConsIds) :-
    ( set_tree234.contains(CoveredConsIds, ConsId) ->
        true
    ;
        !:RevMissingConsIds = [ConsId | !.RevMissingConsIds]
    ),
    find_uncovered_consids(ConsIds, CoveredConsIds, !RevMissingConsIds).

:- pred compute_covered_cons_ids(list(case)::in,
    set_tree234(cons_id)::in, set_tree234(cons_id)::out) is det.

compute_covered_cons_ids([], !CoveredConsIds).
compute_covered_cons_ids([Case | Cases], !CoveredConsIds) :-
    Case = case(MainConsId, OtherConsIds, _Goal),
    set_tree234.insert(MainConsId, !CoveredConsIds),
    set_tree234.insert_list(OtherConsIds, !CoveredConsIds),
    compute_covered_cons_ids(Cases, !CoveredConsIds).

:- pred cons_id_list_to_pieces(list(cons_id)::in,
    list(format_component)::out) is det.

cons_id_list_to_pieces([], []).
cons_id_list_to_pieces([ConsId | ConsIds], Pieces) :-
    ConsIdPiece = cons_id_and_maybe_arity(ConsId),
    (
        ConsIds = [_, _ | _],
        PiecesHead = [ConsIdPiece, suffix(",")]
    ;
        ConsIds = [_],
        PiecesHead = [ConsIdPiece, words("or")]
    ;
        ConsIds = [],
        PiecesHead = [ConsIdPiece, suffix(".")]
    ),
    cons_id_list_to_pieces(ConsIds, PiecesTail),
    Pieces = PiecesHead ++ PiecesTail.

%-----------------------------------------------------------------------------%

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
    % can succeed only the switched-on variable is bound to one of these
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
    list(format_component)::out) is det.

det_diagnose_switch_context(_, [], []).
det_diagnose_switch_context(DetInfo, [SwitchContext | SwitchContexts],
        Pieces) :-
    det_get_proc_info(DetInfo, ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    SwitchContext = switch_context(Var, MainMatch, OtherMatches),
    MainMatchStr = switch_match_to_string(VarSet, MainMatch),
    OtherMatchStrs = list.map(switch_match_to_string(VarSet), OtherMatches),
    MatchsStr = string.join_list(", ", [MainMatchStr | OtherMatchStrs]),
    VarStr = mercury_var_to_string(VarSet, no, Var),
    InnerPieces = [words("Inside the case"), words(MatchsStr),
        words("of the switch on"), fixed(VarStr), suffix(":"), nl],
    det_diagnose_switch_context(DetInfo, SwitchContexts, OuterPieces),
    % We construct the list of switch contexts so that inner contexts come
    % before outer contexts, but we want to print the contexts from the outside
    % towards the inside.
    Pieces = OuterPieces ++ [lower_case_next_if_not_first] ++ InnerPieces.

:- func switch_match_to_string(prog_varset, switch_match) = string.

switch_match_to_string(VarSet, switch_match(ConsId, MaybeArgVars)) =
    cons_id_and_vars_or_arity_to_string(do_not_qualify_cons_id, VarSet,
        ConsId, MaybeArgVars).

%-----------------------------------------------------------------------------%

:- pred det_report_call_context(prog_context::in,
    maybe(call_unify_context)::in, det_info::in, pred_id::in, proc_id::in,
    list(error_msg)::out, list(format_component)::out) is det.

det_report_call_context(Context, CallUnifyContext, DetInfo, PredId, ProcId,
        InitMsgs, StartingPieces) :-
    det_info_get_module_info(DetInfo, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_origin(PredInfo, Origin),

    % If the error was in a call to a type-specific unification predicate
    % (i.e. in the unification itself), then don't print out the predicate
    % name, just print out the context. If it wasn't, then print them
    % both out. (The latter can happen if there is a determinism error
    % in a function call inside some unification.)

    ( Origin = origin_special_pred(spec_pred_unify - _) ->
        InitMsgs = [],
        (
            CallUnifyContext = yes(call_unify_context(LHS, RHS, UC)),
            det_report_unify_context(is_first, is_last, Context, UC, DetInfo,
                LHS, RHS, StartingPieces)
        ;
            % This shouldn't happen; every call to a compiler generated
            % type-specific unification predicate should have a unify_context.
            CallUnifyContext = no,
            StartingPieces = [words("Some weird unification"
                ++ "(or explicit call to a type-specific unify predicate?)")]
        )
    ;
        (
            CallUnifyContext = yes(call_unify_context(LHS, RHS, UC)),
            det_report_unify_context(is_first, is_not_last, Context, UC,
                DetInfo, LHS, RHS, UnifyPieces0),
            UnifyPieces = UnifyPieces0 ++ [suffix(":")],
            UnifyMsg = simple_msg(Context, [always(UnifyPieces)]),
            InitMsgs = [UnifyMsg]
        ;
            CallUnifyContext = no,
            InitMsgs = []
        ),
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_declared_argmodes(ProcInfo, ArgModes),
        proc_info_get_inst_varset(ProcInfo, InstVarSet),
        PredPieces = describe_one_pred_name_mode(ModuleInfo,
            should_module_qualify, PredId, InstVarSet, ArgModes),
        StartingPieces = [words("Call to") | PredPieces]
    ).

%-----------------------------------------------------------------------------%

    % det_report_unify_context returns information about the context of an
    % error, i.e. where the error occurred.
    %
    % The first two arguments are boolean flags that specify whether this is
    % the first part of a sentence (in which case we start the error message
    % with a capital letter) and whether it is the last part (in which case we
    % omit the word "in" on the final "... in unification ...").
    %
:- pred det_report_unify_context(is_first::in, is_last::in, prog_context::in,
    unify_context::in, det_info::in, prog_var::in, unify_rhs::in,
    list(format_component)::out) is det.

det_report_unify_context(!.First, Last, _Context, UnifyContext, DetInfo,
        LHS, RHS, AllPieces) :-
    unify_context_first_to_pieces(!First, UnifyContext, [],
        UnifyContextPieces),
    det_get_proc_info(DetInfo, ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    det_info_get_module_info(DetInfo, ModuleInfo),
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
    ( varset.search_name(VarSet, LHS, _) ->
        (
            RHS = rhs_var(RV),
            \+ varset.search_name(VarSet, RV, _)
        ->
            Pieces = [words(StartWords), words("with"),
                words(add_quotes(mercury_var_to_string(VarSet, no, LHS)))]
        ;
            Pieces = [words(StartWords), words("of"),
                words(add_quotes(mercury_var_to_string(VarSet, no, LHS))),
                words("and"),
                words(add_quotes(
                    unify_rhs_to_string(RHS, ModuleInfo, VarSet, no)))]
        )
    ;
        Pieces = [words(StartWords), words("with"),
            words(add_quotes(
                unify_rhs_to_string(RHS, ModuleInfo, VarSet, no)))]
    ),
    AllPieces = UnifyContextPieces ++ Pieces.

%-----------------------------------------------------------------------------%

promise_solutions_kind_str(equivalent_solutions)
    = "promise_equivalent_solutions".
promise_solutions_kind_str(equivalent_solution_sets)
    = "promise_equivalent_solution_sets".
promise_solutions_kind_str(equivalent_solution_sets_arbitrary)
    = "arbitrary".

lookup_var_name_in_varset(VarSet, Var) =
    mercury_var_to_string(VarSet, no, Var).

failing_contexts_description(ModuleInfo, VarSet, FailingContexts) =
    list.map(failing_context_description(ModuleInfo, VarSet), FailingContexts).

:- func failing_context_description(module_info, prog_varset,
    failing_context) = error_msg.

failing_context_description(ModuleInfo, VarSet, FailingContext) = Msg :-
    FailingContext = failing_context(Context, FailingGoal),
    (
        FailingGoal = incomplete_switch(Var),
        VarStr = mercury_var_to_string(VarSet, no, Var),
        Pieces = [words("Switch on"), fixed(VarStr), words("is incomplete.")]
    ;
        FailingGoal = fail_goal,
        Pieces = [words("Fail goal can fail.")]
    ;
        FailingGoal = test_goal(Var1, Var2),
        Var1Str = mercury_var_to_string(VarSet, no, Var1),
        Var2Str = mercury_var_to_string(VarSet, no, Var2),
        Pieces = [words("Unification of"), fixed(Var1Str),
            words("and"), fixed(Var2Str), words("can fail.")]
    ;
        FailingGoal = deconstruct_goal(Var, ConsId),
        VarStr = mercury_var_to_string(VarSet, no, Var),
        Pieces = [words("Unification of"), fixed(VarStr), words("with"),
            cons_id_and_maybe_arity(ConsId), words("can fail.")]
    ;
        FailingGoal = call_goal(PredId, _ProcId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Name = pred_info_name(PredInfo),
        Pieces = [words("Call to"), fixed(Name), words("can fail.")]
    ;
        FailingGoal = generic_call_goal(GenericCall),
        hlds_goal.generic_call_to_id(GenericCall, GenericCallId),
        Pieces = [words(capitalize(generic_call_id_to_string(GenericCallId))),
            words("can fail.")]
    ;
        FailingGoal = negated_goal,
        Pieces = [words("Negated goal can fail.")]
    ),
    Msg = simple_msg(Context, [always(Pieces ++ [nl])]).

%-----------------------------------------------------------------------------%

det_report_seen_call_id(ModuleInfo, SeenCall) = Pieces :-
    (
        SeenCall = seen_call(PredId, _),
        PredPieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
            PredId),
        Pieces = [words("call to") | PredPieces]
    ;
        SeenCall = higher_order_call,
        Pieces = [words("higher-order call")]
    ).

%-----------------------------------------------------------------------------%

:- func det_report_context_lines(list(prog_context)) = string.

det_report_context_lines(Contexts) =
    det_report_context_lines_2(Contexts, is_first).

:- func det_report_context_lines_2(list(prog_context), is_first) = string.

det_report_context_lines_2([], _) = "".
det_report_context_lines_2([Context | Contexts], First) = Str :-
    term.context_line(Context, Line),
    (
        First = is_first,
        Punct = ""
    ;
        First = is_not_first,
        (
            Contexts = [_ | _],
            Punct = ", "
        ;
            Contexts = [],
            Punct = " and "
        )
    ),
    int_to_string(Line, This),
    Later = det_report_context_lines_2(Contexts, is_not_first),
    Str = Punct ++ This ++ Later.

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%
:- end_module check_hlds.det_report.
%-----------------------------------------------------------------------------%
