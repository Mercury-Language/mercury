%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2006 The University of Melbourne.
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
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- type seen_call_id
    --->    seen_call(pred_id, proc_id)
    ;       higher_order_call.

:- type cc_unify_context
    --->    ccuc_unify(unify_context)
    ;       ccuc_switch.

:- type failing_context == pair(prog_context, failing_goal).

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
:- pred global_checking_pass(pred_proc_list::in, module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Check a lambda goal with the specified declared and inferred
    % determinisms.
    %
:- pred det_check_lambda(determinism::in, determinism::in, hlds_goal::in,
    hlds_goal_info::in, det_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

    % det_diagnose_conj(Goals, Desired, FailingContexts, DetInfo, Msgs):
    %
    % The conjunction Goals should have determinism Desired, but doesn't.
    % Find out what is wrong, and return a list of messages giving the causes.
    %
    % det_diagnose_conj is used for both normal [sequential] conjunctions
    % and parallel conjunctions.
    %
:- pred det_diagnose_conj(list(hlds_goal)::in, determinism::in,
    list(switch_context)::in, det_info::in, list(error_msg)::out) is det.

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

:- type det_comparison
    --->    tighter
    ;       sameas
    ;       looser.

:- pred compare_determinisms(determinism::in, determinism::in,
    det_comparison::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module getopt_io.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

global_checking_pass([], _, !Specs).
global_checking_pass([proc(PredId, ProcId) | Rest], ModuleInfo, !Specs) :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo),
    check_determinism(PredId, ProcId, PredInfo, ProcInfo, ModuleInfo, !Specs),
    check_determinism_of_main(PredId, ProcId, PredInfo, ProcInfo,
        !Specs),
    check_for_multisoln_func(PredId, ProcId, PredInfo, ProcInfo, ModuleInfo,
        !Specs),
    global_checking_pass(Rest, ModuleInfo, !Specs).

:- pred check_determinism(pred_id::in, proc_id::in, pred_info::in,
    proc_info::in, module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_determinism(PredId, ProcId, PredInfo0, ProcInfo0, ModuleInfo, !Specs) :-
    proc_info_get_declared_determinism(ProcInfo0, MaybeDetism),
    proc_info_get_inferred_determinism(ProcInfo0, InferredDetism),
    (
        MaybeDetism = no
    ;
        MaybeDetism = yes(DeclaredDetism),
        compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
        (
            Cmp = sameas
        ;
            Cmp = looser,
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, warn_det_decls_too_lax,
                ShouldIssueWarning),
            globals.lookup_bool_option(Globals, warn_inferred_erroneous,
                WarnAboutInferredErroneous),
            pred_info_get_markers(PredInfo0, Markers),
            (
                ShouldIssueWarning = yes,

                % Don't report warnings for class method implementations --
                % the determinism in the `:- typeclass' declaration will be
                % the loosest of all possible instances. This is similar to
                % the reason we don't report warnings for lambda expressions.
                \+ check_marker(Markers, marker_class_instance_method),

                % Don't report warnings for procedures with no clauses.
                \+ check_marker(Markers, marker_stub),

                % Don't report warnings for compiler-generated Unify, Compare
                % or Index procedures, since the user has no way to shut
                % these up. These can happen for the Unify pred for the unit
                % type, if such types are not boxed (as they are not
                % boxed for the IL backend).
                \+ is_unify_or_compare_pred(PredInfo0),

                % Don't warn about predicates which are inferred erroneous
                % when the appropriate option is set. This is to avoid warnings
                % about unimplemented predicates.
                (
                    WarnAboutInferredErroneous = yes
                ;
                    WarnAboutInferredErroneous = no,
                    InferredDetism \= detism_erroneous
                )
            ->
                Message = "warning: determinism declaration " ++
                    "could be tighter.\n",
                report_determinism_problem(PredId, ProcId, ModuleInfo,
                    Message, DeclaredDetism, InferredDetism, ReportMsgs),
                ReportSpec = error_spec(severity_warning, phase_detism_check,
                    ReportMsgs),
                !:Specs = [ReportSpec | !.Specs]
            ;
                true
            )
        ;
            Cmp = tighter,
            Message = "error: determinism declaration not satisfied.\n",
            report_determinism_problem(PredId, ProcId, ModuleInfo, Message,
                DeclaredDetism, InferredDetism, ReportMsgs),
            proc_info_get_goal(ProcInfo0, Goal),
            proc_info_get_vartypes(ProcInfo0, VarTypes),
            det_info_init(ModuleInfo, VarTypes, PredId, ProcId, DetInfo),
            det_diagnose_goal(Goal, DeclaredDetism, [], DetInfo, GoalMsgs0),
            sort_error_msgs(GoalMsgs0, GoalMsgs),
            ReportSpec = error_spec(severity_error, phase_detism_check,
                ReportMsgs ++ GoalMsgs),
            !:Specs = [ReportSpec | !.Specs]
        )
    ),

    % Make sure the code model is valid given the eval method.
    proc_info_get_eval_method(ProcInfo0, EvalMethod),
    Valid = valid_determinism_for_eval_method(EvalMethod, InferredDetism),
    (
        Valid = yes
    ;
        Valid = no,
        proc_info_get_context(ProcInfo0, Context),
        MainPieces =
            [words("Error: `pragma "
                ++ eval_method_to_string(EvalMethod) ++ "'"),
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
                [always(MainPieces), verbose_only(VerbosePieces)])]),
        !:Specs = [ValidSpec | !.Specs]
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
        Pieces = [words("Error: main/2 must be `det' or `cc_multi'.")],
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
        pred_info_is_pred_or_func(PredInfo) = function,
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
                [always(MainPieces), verbose_only(VerbosePieces)])]),
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

det_check_lambda(DeclaredDetism, InferredDetism, Goal, GoalInfo, DetInfo,
        !Specs) :-
    compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
    ( 
        Cmp = tighter,
        det_info_get_pred_id(DetInfo, PredId),
        det_info_get_proc_id(DetInfo, ProcId),
        goal_info_get_context(GoalInfo, Context),
        det_info_get_module_info(DetInfo, ModuleInfo),
        PredPieces = describe_one_proc_name_mode(ModuleInfo,
            should_not_module_qualify, proc(PredId, ProcId)),
        Pieces =
            [words("In")] ++ PredPieces ++ [suffix(":"), nl,
            words("Determinism error in lambda expression."), nl,
            words("Declared"),
            quote(determinism_to_string(DeclaredDetism)), suffix(","),
            words("inferred"),
            quote(determinism_to_string(InferredDetism)), suffix("'.")],
        det_diagnose_goal(Goal, DeclaredDetism, [], DetInfo, GoalMsgs),
        sort_error_msgs(GoalMsgs, SortedGoalMsgs),
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context, [always(Pieces)])] ++ SortedGoalMsgs),
        !:Specs = [Spec | !.Specs]
    ;
        ( Cmp = sameas
        ; Cmp = looser
        )
        % We don't bother issuing warnings if the determinism was too loose;
        % that will often be the case, and should not be warned about.
    ).

:- pred report_determinism_problem(pred_id::in, proc_id::in, module_info::in,
    string::in, determinism::in, determinism::in, list(error_msg)::out) is det.

report_determinism_problem(PredId, ProcId, ModuleInfo, Message,
        DeclaredDetism, InferredDetism, Msgs) :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_get_context(ProcInfo, Context),
    ProcPieces = describe_one_proc_name_mode(ModuleInfo,
        should_not_module_qualify, proc(PredId, ProcId)),
    Pieces = [words("In")] ++ ProcPieces ++ [suffix(":"), nl,
        words(Message), nl,
        words("Declared"),
        quote(determinism_to_string(DeclaredDetism)), suffix(","),
        words("inferred"),
        quote(determinism_to_string(InferredDetism)), suffix(".")],
    Msgs = [simple_msg(Context, [always(Pieces)])].

%-----------------------------------------------------------------------------%

compare_determinisms(DeclaredDetism, InferredDetism, CmpDetism) :-
    determinism_components(DeclaredDetism, DeclaredCanFail, DeclaredSolns),
    determinism_components(InferredDetism, InferredCanFail, InferredSolns),
    compare_canfails(DeclaredCanFail, InferredCanFail, CmpCanFail),
    compare_solncounts(DeclaredSolns, InferredSolns, CmpSolns),

    % We can get e.g. tighter canfail and looser solncount
    % e.g. for a predicate declared multidet and inferred semidet.
    % Therefore the ordering of the following two tests is important:
    % we want errors to take precedence over warnings.

    ( ( CmpCanFail = tighter ; CmpSolns = tighter ) ->
        CmpDetism = tighter
    ; ( CmpCanFail = looser ; CmpSolns = looser ) ->
        CmpDetism = looser
    ;
        CmpDetism = sameas
    ).

:- pred compare_canfails(can_fail::in, can_fail::in, det_comparison::out)
    is det.

compare_canfails(cannot_fail, cannot_fail, sameas).
compare_canfails(cannot_fail, can_fail,    tighter).
compare_canfails(can_fail,    cannot_fail, looser).
compare_canfails(can_fail,    can_fail,    sameas).

:- pred compare_solncounts(soln_count::in, soln_count::in, det_comparison::out)
    is det.

compare_solncounts(at_most_zero,    at_most_zero,    sameas).
compare_solncounts(at_most_zero,    at_most_one,     tighter).
compare_solncounts(at_most_zero,    at_most_many_cc, tighter).
compare_solncounts(at_most_zero,    at_most_many,    tighter).

compare_solncounts(at_most_one,     at_most_zero,    looser).
compare_solncounts(at_most_one,     at_most_one,     sameas).
compare_solncounts(at_most_one,     at_most_many_cc, tighter).
compare_solncounts(at_most_one,     at_most_many,    tighter).

compare_solncounts(at_most_many_cc, at_most_zero,    looser).
compare_solncounts(at_most_many_cc, at_most_one,     looser).
compare_solncounts(at_most_many_cc, at_most_many_cc, sameas).
compare_solncounts(at_most_many_cc, at_most_many,    tighter).

compare_solncounts(at_most_many,    at_most_zero,    looser).
compare_solncounts(at_most_many,    at_most_one,     looser).
compare_solncounts(at_most_many,    at_most_many_cc, looser).
compare_solncounts(at_most_many,    at_most_many,    sameas).

%-----------------------------------------------------------------------------%

    % The given goal should have determinism Desired, but doesn't.
    % Find out what is wrong, and return a list of messages giving the causes.
    %
:- pred det_diagnose_goal(hlds_goal::in, determinism::in,
    list(switch_context)::in, det_info::in, list(error_msg)::out) is det.

det_diagnose_goal(Goal - GoalInfo, Desired, SwitchContext, DetInfo, Msgs) :-
    goal_info_get_determinism(GoalInfo, Actual),
    ( compare_determinisms(Desired, Actual, tighter) ->
        det_diagnose_goal_2(Goal, GoalInfo, Desired, Actual, SwitchContext,
            DetInfo, Msgs)
    ;
        Msgs = []
    ).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
    determinism::in, determinism::in, list(switch_context)::in,
    det_info::in, list(error_msg)::out) is det.

det_diagnose_goal_2(conj(_, Goals), _GoalInfo, Desired, _Actual, Context,
        DetInfo, Msgs) :-
    det_diagnose_conj(Goals, Desired, Context, DetInfo, Msgs).

det_diagnose_goal_2(disj(Goals), GoalInfo, Desired, Actual, SwitchContext,
        DetInfo, Msgs) :-
    det_diagnose_disj(Goals, Desired, Actual, SwitchContext, DetInfo, 0,
        ClausesWithSoln, Msgs1),
    determinism_components(Desired, _, DesSolns),
    (
        DesSolns \= at_most_many,
        DesSolns \= at_most_many_cc,
        ClausesWithSoln > 1
    ->
        goal_info_get_context(GoalInfo, Context),
        Pieces = [words("Disjunction has multiple clauses with solutions.")],
        Msg = simple_msg(Context, [always(Pieces)]),
        Msgs = [Msg] ++ Msgs1
    ;
        Msgs = Msgs1
    ).

    % The determinism of a switch is the worst of the determinism of each of
    % the cases. Also, if only a subset of the constructors are handled,
    % then it is semideterministic or worse - this is determined
    % in switch_detection.m and handled via the CanFail field.
    %
det_diagnose_goal_2(switch(Var, SwitchCanFail, Cases), GoalInfo,
        Desired, _Actual, SwitchContext, DetInfo, Msgs) :-
    (
        SwitchCanFail = can_fail,
        determinism_components(Desired, cannot_fail, _)
    ->
        goal_info_get_context(GoalInfo, Context),
        det_diagnose_switch_context(SwitchContext, DetInfo, NestingPieces),
        det_get_proc_info(DetInfo, ProcInfo),
        proc_info_get_varset(ProcInfo, VarSet),
        det_info_get_module_info(DetInfo, ModuleInfo),
        VarStr = mercury_var_to_string(VarSet, no, Var),
        (
            det_lookup_var_type(ModuleInfo, ProcInfo, Var, TypeDefn),
            hlds_data.get_type_defn_body(TypeDefn, TypeBody),
            ConsTable = TypeBody ^ du_type_cons_tag_values
        ->
            map.keys(ConsTable, ConsIds),
            det_diagnose_missing_consids(ConsIds, Cases, Missing),
            cons_id_list_to_pieces(Missing, MissingPieces),
            Pieces = [words("The switch on "), fixed(VarStr),
                words("does not cover") | MissingPieces]
        ;
            Pieces = [words("The switch on "), fixed(VarStr),
                words("can fail.")]
        ),
        Msgs1 = [simple_msg(Context, [always(NestingPieces ++ Pieces)])]
    ;
        Msgs1 = []
    ),
    det_diagnose_switch(Var, Cases, Desired, SwitchContext, DetInfo, Msgs2),
    Msgs = Msgs1 ++ Msgs2.

det_diagnose_goal_2(plain_call(PredId, ProcId, _, _, CallContext, _), GoalInfo,
        Desired, Actual, _, DetInfo, Msgs) :-
    goal_info_get_context(GoalInfo, Context),
    det_report_call_context(Context, CallContext, DetInfo, PredId, ProcId,
        InitMsgs, StartingPieces),
    det_diagnose_atomic_goal(Desired, Actual, Context, StartingPieces,
        AtomicMsgs),
    Msgs = InitMsgs ++ AtomicMsgs.

det_diagnose_goal_2(generic_call(GenericCall, _, _, _), GoalInfo,
        Desired, Actual, _, _DetInfo, Msgs) :-
    goal_info_get_context(GoalInfo, Context),
    report_generic_call_context(GenericCall, StartingPieces),
    det_diagnose_atomic_goal(Desired, Actual, Context, StartingPieces, Msgs).

det_diagnose_goal_2(unify(LHS, RHS, _, _, UnifyContext), GoalInfo,
        Desired, Actual, _, DetInfo, Msgs) :-
    goal_info_get_context(GoalInfo, Context),
    First = yes,
    Last = yes,
    det_report_unify_context(First, Last, Context, UnifyContext,
        DetInfo, LHS, RHS, StartingPieces),
    det_diagnose_atomic_goal(Desired, Actual, Context, StartingPieces, Msgs).

det_diagnose_goal_2(if_then_else(_Vars, Cond, Then, Else), _GoalInfo,
        Desired, _Actual, SwitchContext, DetInfo, Msgs) :-
    determinism_components(Desired, _DesiredCanFail, DesiredSolns),
    Cond = _CondGoal - CondInfo,
    goal_info_get_determinism(CondInfo, CondDetism),
    determinism_components(CondDetism, _CondCanFail, CondSolns),
    (
        CondSolns = at_most_many,
        DesiredSolns \= at_most_many
    ->
        determinism_components(DesiredCond, can_fail, DesiredSolns),
        det_diagnose_goal(Cond, DesiredCond, SwitchContext, DetInfo, Msgs1)
    ;
        Msgs1 = []
    ),
    det_diagnose_goal(Then, Desired, SwitchContext, DetInfo, Msgs2),
    det_diagnose_goal(Else, Desired, SwitchContext, DetInfo, Msgs3),
    Msgs = Msgs1 ++ Msgs2 ++ Msgs3.

det_diagnose_goal_2(negation(_), GoalInfo, Desired, Actual, _, _, Msgs) :-
    determinism_components(Desired, DesiredCanFail, DesiredSolns),
    determinism_components(Actual, ActualCanFail, ActualSolns),
    (
        DesiredCanFail = cannot_fail,
        ActualCanFail = can_fail
    ->
        goal_info_get_context(GoalInfo, Context),
        Pieces = [words("Negated goal can succeed.")],
        Msgs = [simple_msg(Context, [always(Pieces)])]
    ;
        DesiredSolns = at_most_zero,
        ActualSolns \= at_most_zero
    ->
        goal_info_get_context(GoalInfo, Context),
        Pieces = [words("Negated goal can fail.")],
        Msgs = [simple_msg(Context, [always(Pieces)])]
    ;
        Msgs = []
    ).

det_diagnose_goal_2(scope(_, Goal), _, Desired, Actual, SwitchContext, DetInfo,
        Msgs) :-
    Goal = _ - GoalInfo,
    goal_info_get_determinism(GoalInfo, Internal),
    ( Actual = Internal ->
        InternalDesired = Desired
    ;
        determinism_components(Desired, CanFail, _),
        determinism_components(InternalDesired, CanFail, at_most_many)
    ),
    det_diagnose_goal(Goal, InternalDesired, SwitchContext, DetInfo, Msgs).

det_diagnose_goal_2(call_foreign_proc(_, _, _, _, _, _, _), GoalInfo, Desired,
        _, _, _, Msgs) :-
    goal_info_get_context(GoalInfo, Context),
    DesiredStr = determinism_to_string(Desired),
    Pieces = [words("Determinism declaration not satisfied."),
        words("Desired determinism is " ++ DesiredStr ++ ".")],
    Msgs = [simple_msg(Context, [always(Pieces)])].

det_diagnose_goal_2(shorthand(_), _, _, _, _, _, []) :-
    % These should have been expanded out by now.
    unexpected(this_file, "det_diagnose_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

:- pred report_generic_call_context(generic_call::in,
    list(format_component)::out) is det.

report_generic_call_context(CallType, StartingPieces) :-
    hlds_goal.generic_call_id(CallType, CallId),
    StartingPieces = [words(call_id_to_string(CallId))].

%-----------------------------------------------------------------------------%

:- pred det_diagnose_atomic_goal(determinism::in, determinism::in,
    prog_context::in, list(format_component)::in, list(error_msg)::out) is det.

det_diagnose_atomic_goal(Desired, Actual, Context, StartingPieces, Msgs) :-
    determinism_components(Desired, DesiredCanFail, DesiredSolns),
    determinism_components(Actual, ActualCanFail, ActualSolns),
    compare_canfails(DesiredCanFail, ActualCanFail, CmpCanFail),
    ( CmpCanFail = tighter ->
        CanFailPieces = [words("can fail")]
    ;
        CanFailPieces = []
    ),
    compare_solncounts(DesiredSolns, ActualSolns, CmpSolns),
    ( CmpSolns = tighter ->
        (
            CanFailPieces = [_ | _],
            ConnectPieces = [words("and")]
        ;
            CanFailPieces = [],
            ConnectPieces = []
        ),
        ( DesiredSolns = at_most_one ->
            SolnsPieces = [words("can succeed more than once")]
        ;
            SolnsPieces = [words("can succeed")]
        )
    ;
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

det_diagnose_conj([], _Desired, _SwitchContext, _DetInfo, []).
det_diagnose_conj([Goal | Goals], Desired, SwitchContext, DetInfo, Msgs) :-
    det_diagnose_goal(Goal, Desired, SwitchContext, DetInfo, Msgs1),
    det_diagnose_conj(Goals, Desired, SwitchContext, DetInfo, Msgs2),
    Msgs = Msgs1 ++ Msgs2.

:- pred det_diagnose_disj(list(hlds_goal)::in,
    determinism::in, determinism::in, list(switch_context)::in,
    det_info::in, int::in, int::out, list(error_msg)::out) is det.

det_diagnose_disj([], _Desired, _Actual, _SwitchContext, _DetInfo,
        !ClausesWithSoln, []).
det_diagnose_disj([Goal | Goals], Desired, Actual, SwitchContext, DetInfo,
        !ClausesWithSoln, Msgs) :-
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
        % any error messages for the disjuncts that might fail
        ClauseCanFail = can_fail
    ),
    determinism_components(ClauseDesired, ClauseCanFail, DesiredSolns),
    det_diagnose_goal(Goal, ClauseDesired, SwitchContext, DetInfo, Msgs1),
    (
        Goal = _ - GoalInfo,
        goal_info_get_determinism(GoalInfo, GoalDetism),
        determinism_components(GoalDetism, _, at_most_zero)
    ->
        true
    ;
        !:ClausesWithSoln = !.ClausesWithSoln + 1
    ),
    det_diagnose_disj(Goals, Desired, Actual, SwitchContext, DetInfo,
        !ClausesWithSoln, Msgs2),
    Msgs = Msgs1 ++ Msgs2.

:- pred det_diagnose_switch(prog_var::in, list(case)::in, determinism::in,
    list(switch_context)::in, det_info::in, list(error_msg)::out) is det.

det_diagnose_switch(_Var, [], _Desired, _SwitchContext, _DetInfo, []).
det_diagnose_switch(Var, [case(ConsId, Goal) | Cases], Desired,
        SwitchContext0, DetInfo, Msgs) :-
    SwitchContext1 = [switch_context(Var, ConsId) | SwitchContext0],
    det_diagnose_goal(Goal, Desired, SwitchContext1, DetInfo, Msgs1),
    det_diagnose_switch(Var, Cases, Desired, SwitchContext0, DetInfo, Msgs2),
    Msgs = Msgs1 ++ Msgs2.

%-----------------------------------------------------------------------------%

:- pred det_diagnose_missing_consids(list(cons_id)::in, list(case)::in,
    list(cons_id)::out) is det.

det_diagnose_missing_consids([], _, []).
det_diagnose_missing_consids([ConsId | ConsIds], Cases, Missing) :-
    det_diagnose_missing_consids(ConsIds, Cases, Missing0),
    (
        list.member(Case, Cases),
        Case = case(ConsId, _)
    ->
        Missing = Missing0
    ;
        Missing = [ConsId | Missing0]
    ).

:- pred cons_id_list_to_pieces(list(cons_id)::in,
    list(format_component)::out) is det.

cons_id_list_to_pieces([], []).
cons_id_list_to_pieces([ConsId | ConsIds], Pieces) :-
    ConsIdStr = cons_id_to_string(ConsId),
    (
        ConsIds = [],
        PiecesHead = [fixed(ConsIdStr ++ ".")]
    ;
        ConsIds = [_],
        PiecesHead = [fixed(ConsIdStr), fixed("or")]
    ;
        ConsIds = [_, _ | _],
        PiecesHead = [fixed(ConsIdStr ++ ",")]
    ),
    cons_id_list_to_pieces(ConsIds, PiecesTail),
    Pieces = PiecesHead ++ PiecesTail.

%-----------------------------------------------------------------------------%

:- type switch_context
    --->    switch_context(prog_var, cons_id).

:- pred det_diagnose_switch_context(list(switch_context)::in, det_info::in,
    list(format_component)::out) is det.

det_diagnose_switch_context([], _, []).
det_diagnose_switch_context([SwitchContext | SwitchContexts], DetInfo,
        HeadPieces ++ TailPieces) :-
    det_get_proc_info(DetInfo, ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    SwitchContext = switch_context(Var, ConsId),
    ConsIdStr = cons_id_to_string(ConsId),
    VarStr = mercury_var_to_string(VarSet, no, Var),
    HeadPieces = [words("Inside the case"), fixed(ConsIdStr),
        words("of the switch on"), fixed(VarStr), words(":")],
    det_diagnose_switch_context(SwitchContexts, DetInfo, TailPieces).

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
    % name, just print out the context.  If it wasn't, then print them
    % both out. (The latter can happen if there is a determinism error
    % in a function call inside some unification.)

    ( Origin = origin_special_pred(spec_pred_unify - _) ->
        InitMsgs = [],
        (
            CallUnifyContext = yes(call_unify_context(LHS, RHS, UC)),
            First = yes,
            Last = yes,
            det_report_unify_context(First, Last, Context, UC, DetInfo,
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
            First = yes,
            Last = no,
            det_report_unify_context(First, Last, Context, UC, DetInfo,
                LHS, RHS, UnifyPieces0),
            UnifyPieces = UnifyPieces0 ++ [suffix(":")],
            UnifyMsg = simple_msg(Context, [always(UnifyPieces)]),
            InitMsgs = [UnifyMsg]
        ;
            CallUnifyContext = no,
            InitMsgs = []
        ),
        pred_info_get_procedures(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_declared_argmodes(ProcInfo, ArgModes),
        proc_info_get_inst_varset(ProcInfo, InstVarSet),
        PredPieces = describe_one_pred_name_mode(ModuleInfo,
            should_module_qualify, PredId, InstVarSet, ArgModes),
        StartingPieces = [words("call to") | PredPieces]
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
:- pred det_report_unify_context(bool::in, bool::in, prog_context::in,
    unify_context::in, det_info::in, prog_var::in, unify_rhs::in,
    list(format_component)::out) is det.

det_report_unify_context(!.First, Last, _Context, UnifyContext, DetInfo,
        LHS, RHS, AllPieces) :-
    unify_context_first_to_pieces(!First, UnifyContext, [], UnifyContextPieces),
    det_get_proc_info(DetInfo, ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    det_info_get_module_info(DetInfo, ModuleInfo),
    (
        !.First = yes,
        (
            Last = yes,
            StartWords = "Unification"
        ;
            Last = no,
            StartWords = "In unification"
        )
    ;
        !.First = no,
        (
            Last = yes,
            StartWords = "unification"
        ;
            Last = no,
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

failing_context_description(ModuleInfo, VarSet, Context - FailingGoal) = Msg :-
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
        ConsIdStr = cons_id_to_string(ConsId),
        Pieces = [words("Unification of"), fixed(VarStr),
            words("with"), fixed(ConsIdStr), words("can fail.")]
    ;
        FailingGoal = call_goal(PredId, _ProcId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        Name = pred_info_name(PredInfo),
        Pieces = [words("Call to"), fixed(Name), words("can fail.")]
    ;
        FailingGoal = generic_call_goal(GenericCall),
        hlds_goal.generic_call_id(GenericCall, CallId),
        Pieces = [words(capitalize(call_id_to_string(CallId))),
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

det_report_context_lines(Contexts) = det_report_context_lines_2(Contexts, yes).

:- func det_report_context_lines_2(list(prog_context), bool) = string.

det_report_context_lines_2([], _) = "".
det_report_context_lines_2([Context | Contexts], First) = Str :-
    term.context_line(Context, Line),
    ( First = yes ->
        Punct = ""
    ; Contexts = [] ->
        Punct = " and "
    ;
        Punct = ", "
    ),
    int_to_string(Line, This),
    Later = det_report_context_lines_2(Contexts, no),
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

:- func this_file = string.

this_file = "det_report.m".

%-----------------------------------------------------------------------------%
:- end_module det_report.
%-----------------------------------------------------------------------------%
