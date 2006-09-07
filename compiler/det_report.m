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
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module pair.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type context_det_msg
    --->    context_det_msg(prog_context, det_msg).

:- type det_msg
            % The following are warnings.

    --->    multidet_disj(list(prog_context))
    ;       det_disj(list(prog_context))
    ;       semidet_disj(list(prog_context))
    ;       zero_soln_disj(list(prog_context))
    ;       zero_soln_disjunct
    ;       ite_cond_cannot_fail
    ;       ite_cond_cannot_succeed
    ;       negated_goal_cannot_fail
    ;       negated_goal_cannot_succeed
    ;       goal_cannot_succeed
    ;       det_goal_has_no_outputs
    ;       warn_call_to_obsolete(pred_id)
            % Warning about calls to predicates for which there is
            % a `:- pragma obsolete' declaration.
    ;       warn_infinite_recursion
            % Warning about recursive calls which would cause infinite loops.
    ;       duplicate_call(seen_call_id, prog_context)
            % Multiple calls with the same input args.
    ;       unknown_format_string(sym_name, arity)
    ;       unknown_format_values(sym_name, arity)
    ;       bad_format(sym_name, arity, string)
    ;       nested_promise_eqv_solution_sets(prog_context)

            % The following are errors.

    ;       cc_unify_can_fail(hlds_goal_info, prog_var, mer_type,
                prog_varset, cc_unify_context)
    ;       cc_unify_in_wrong_context(hlds_goal_info, prog_var,
                mer_type, prog_varset, cc_unify_context, list(failing_context))
    ;       cc_pred_in_wrong_context(hlds_goal_info, determinism,
                pred_id, proc_id, prog_varset, list(failing_context))
    ;       higher_order_cc_pred_in_wrong_context(hlds_goal_info, determinism,
                prog_varset, list(failing_context))
    ;       error_in_lambda(determinism, determinism, % declared, inferred
                hlds_goal, hlds_goal_info, pred_id, proc_id)
    ;       par_conj_not_det(determinism, pred_id, proc_id,
                hlds_goal_info, list(hlds_goal))
    ;       pragma_c_code_without_det_decl(pred_id, proc_id)
    ;       has_io_state_but_not_det(pred_id, proc_id)
    ;       will_not_throw_with_erroneous(pred_id, proc_id)
    ;       export_model_non_proc(pred_id, proc_id, determinism)
            % Procedure with multi or nondet detism exported
            % via :- pragma export ...
    ;       arbitrary_without_promise
    ;       arbitrary_promise_overlap(prog_context, prog_varset, set(prog_var))
    ;       promise_solutions_missing_vars(promise_solutions_kind, prog_varset,
                set(prog_var))
    ;       promise_solutions_extra_vars(promise_solutions_kind, prog_varset,
                set(prog_var))
    ;       trace_goal_not_det(determinism).

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

%-----------------------------------------------------------------------------%

    % Check all the determinism declarations in this module.
    % This is the main predicate exported by this module.
    %
:- pred global_checking_pass(pred_proc_list::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

    % Check a lambda goal with the specified declared and inferred
    % determinisms.
    %
:- pred det_check_lambda(determinism::in, determinism::in, hlds_goal::in,
    hlds_goal_info::in, det_info::in, list(context_det_msg)::out) is det.

    % Print some determinism warning and/or error messages,
    % and update the module info accordingly.
    %
:- pred det_report_and_handle_msgs(list(context_det_msg)::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

    % Print some determinism warning and/or error messages,
    % and return the number of warnings and errors, so that code
    % somewhere elsewhere can update the module info.
    %
:- pred det_report_msgs(list(context_det_msg)::in, module_info::in,
    int::out, int::out, io::di, io::uo) is det.

:- type msg_modes
    --->    all_modes   % The warning should be reported only
                        % if it occurs in all modes of the predicate.

    ;       any_mode.   % The warning should be reported
                        % if it occurs in any mode of the predicate

    % Decide if the warning should be reported if it occurs in
    % any mode of the predicate, not only if it occurs in all modes.
    %
:- pred det_msg_is_any_mode_msg(det_msg::in, msg_modes::out) is det.

%-----------------------------------------------------------------------------%

:- type options_to_restore.

    % Call this predicate before rerunning determinism analysis after an
    % optimization pass to disable all warnings. Errors will still be reported.
    %
:- pred disable_det_warnings(options_to_restore::out, io::di, io::uo) is det.

:- pred restore_det_warnings(options_to_restore::in, io::di, io::uo) is det.

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

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module hlds.passes_aux.
:- import_module hlds.special_pred.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module getopt_io.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

global_checking_pass([], !ModuleInfo, !IO).
global_checking_pass([proc(PredId, ProcId) | Rest], !ModuleInfo, !IO) :-
    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo),
    check_determinism(PredId, ProcId, PredInfo, ProcInfo, !ModuleInfo, !IO),
    check_determinism_of_main(PredId, ProcId, PredInfo, ProcInfo, !ModuleInfo,
        !IO),
    check_for_multisoln_func(PredId, ProcId, PredInfo, ProcInfo, !ModuleInfo,
        !IO),
    global_checking_pass(Rest, !ModuleInfo, !IO).

:- pred check_determinism(pred_id::in, proc_id::in, pred_info::in,
    proc_info::in, module_info::in, module_info::out, io::di, io::uo) is det.

check_determinism(PredId, ProcId, PredInfo0, ProcInfo0, !ModuleInfo, !IO) :-
    proc_info_get_declared_determinism(ProcInfo0, MaybeDetism),
    proc_info_get_inferred_determinism(ProcInfo0, InferredDetism),
    (
        MaybeDetism = no,
        CmpSpecs = []
    ;
        MaybeDetism = yes(DeclaredDetism),
        compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
        (
            Cmp = sameas,
            CmpSpecs = []
        ;
            Cmp = looser,
            globals.io_lookup_bool_option(warn_det_decls_too_lax,
                ShouldIssueWarning, !IO),
            globals.io_lookup_bool_option(warn_inferred_erroneous,
                WarnAboutInferredErroneous, !IO),
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
                report_determinism_problem(PredId, ProcId, !.ModuleInfo,
                    Message, DeclaredDetism, InferredDetism, ReportMsgs),
                ReportSpec = error_spec(severity_warning, phase_detism_check,
                    ReportMsgs),
                CmpSpecs = [ReportSpec]
            ;
                CmpSpecs = []
            )
        ;
            Cmp = tighter,
            Message = "error: determinism declaration not satisfied.\n",
            report_determinism_problem(PredId, ProcId, !.ModuleInfo, Message,
                DeclaredDetism, InferredDetism, ReportMsgs),
            proc_info_get_goal(ProcInfo0, Goal),
            proc_info_get_vartypes(ProcInfo0, VarTypes),
            globals.io_get_globals(Globals, !IO),
            det_info_init(!.ModuleInfo, VarTypes, PredId, ProcId, Globals,
                DetInfo),
            det_diagnose_goal(Goal, DeclaredDetism, [], DetInfo, GoalMsgs0),
            sort_error_msgs(GoalMsgs0, GoalMsgs),
            ReportSpec = error_spec(severity_error, phase_detism_check,
                ReportMsgs ++ GoalMsgs),
            CmpSpecs = [ReportSpec]
        )
    ),

    % Make sure the code model is valid given the eval method.
    proc_info_get_eval_method(ProcInfo0, EvalMethod),
    ( valid_determinism_for_eval_method(EvalMethod, InferredDetism) = yes ->
        proc_info_set_eval_method(EvalMethod, ProcInfo0, ProcInfo),
        pred_info_get_procedures(PredInfo0, ProcTable0),
        map.det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
        pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo),
        ValidSpecs = []
    ;
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
        ( list.length(Detisms) = 1 ->
            Plural = ""
        ;
            Plural = "s"
        ),
        DetismPieces = list_to_pieces(SortedDetismStrs),
        VerbosePieces =
            [words("The pragma requested is only valid"),
            words("for the following determinism" ++ Plural ++ ":") |
            DetismPieces] ++ [suffix("."), nl],
        ValidSpecs = [error_spec(severity_error, phase_detism_check,
            [simple_msg(Context,
                [always(MainPieces), verbose_only(VerbosePieces)])])]
    ),
    write_error_specs(CmpSpecs ++ ValidSpecs, 0, _NumWarnings, 0, NumErrors,
        !IO),
    module_info_incr_num_errors(NumErrors, !ModuleInfo).

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
    pred_info::in, proc_info::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

check_determinism_of_main(_PredId, _ProcId, PredInfo, ProcInfo,
        !ModuleInfo, !IO) :-
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
        write_error_spec(Spec, 0, _NumWarnings, 0, NumErrors, !IO),
        module_info_incr_num_errors(NumErrors, !ModuleInfo)
    ;
        true
    ).

:- pred check_for_multisoln_func(pred_id::in, proc_id::in, pred_info::in,
    proc_info::in, module_info::in, module_info::out, io::di, io::uo) is det.

check_for_multisoln_func(PredId, _ProcId, PredInfo, ProcInfo,
        !ModuleInfo, !IO) :-
    proc_info_get_inferred_determinism(ProcInfo, InferredDetism),

    % Functions can only have more than one solution if it is a
    % non-standard mode.  Otherwise, they would not be referentially
    % transparent.  (Nondeterministic "functions" like C's `rand()'
    % function are not allowed.)
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
        \+ (
            list.member(FuncArgMode, FuncArgModes),
            \+ mode_is_fully_input(!.ModuleInfo, FuncArgMode)
        )
    ->
        % ... then it is an error.
        proc_info_get_context(ProcInfo, FuncContext),
        proc_info_get_inst_varset(ProcInfo, InstVarSet),
        PredModePieces = describe_one_pred_name_mode(!.ModuleInfo,
            should_not_module_qualify, PredId, InstVarSet, PredArgModes),
        MainPieces = [words("Error: invalid determinism for")]
            ++ PredModePieces ++ [suffix(":"), nl,
            words("the primary mode of a function cannot be"),
            quote(mercury_det_to_string(InferredDetism)), suffix(".")],
        VerbosePieces = func_primary_mode_det_msg,
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(FuncContext,
                [always(MainPieces), verbose_only(VerbosePieces)])]),
        write_error_spec(Spec, 0, _NumWarnings, 0, NumErrors, !IO),
        module_info_incr_num_errors(NumErrors, !ModuleInfo)
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
        Msgs) :-
    compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
    ( 
        Cmp = tighter,
        det_info_get_pred_id(DetInfo, PredId),
        det_info_get_proc_id(DetInfo, ProcId),
        goal_info_get_context(GoalInfo, Context),
        Msg = error_in_lambda(DeclaredDetism, InferredDetism, Goal, GoalInfo,
            PredId, ProcId),
        ContextMsg = context_det_msg(Context, Msg),
        Msgs = [ContextMsg]
    ;
        ( Cmp = sameas
        ; Cmp = looser
        ),
        % We don't bother issuing warnings if the determinism was too loose;
        % that will often be the case, and should not be warned about.
        Msgs = []
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
    % Find out what is wrong and print a report of the cause.
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
        VarStr = mercury_var_to_string(Var, VarSet, no),
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

    % det_diagnose_conj is used for both normal [sequential] conjunction
    % and parallel conjunction.
    %
:- pred det_diagnose_conj(list(hlds_goal)::in, determinism::in,
    list(switch_context)::in, det_info::in, list(error_msg)::out) is det.

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
    VarStr = mercury_var_to_string(Var, VarSet, no),
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
                words(add_quotes(mercury_var_to_string(LHS, VarSet, no)))]
        ;
            Pieces = [words(StartWords), words("of"),
                words(add_quotes(mercury_var_to_string(LHS, VarSet, no))),
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

:- type det_msg_type
    --->    simple_code_warning
    ;       call_warning
    ;       format_unknown
    ;       format_known_bad
    ;       det_error.

det_report_and_handle_msgs(Msgs, !ModuleInfo, !IO) :-
    (
        Msgs = []
        % fast path for the usual case
    ;
        Msgs = [_ | _],
        det_report_msgs(Msgs, !.ModuleInfo, WarnCnt, ErrCnt, !IO),
        globals.io_lookup_bool_option(halt_at_warn, HaltAtWarn, !IO),
        (
            (
                ErrCnt > 0
            ;
                WarnCnt > 0,
                HaltAtWarn = yes
            )
        ->
            io.set_exit_status(1, !IO),
            module_info_incr_errors(!ModuleInfo)
        ;
            true
        )
    ).

det_report_msgs(ContextMsgs, ModuleInfo, WarnCnt, ErrCnt, !IO) :-
    Specs0 = list.map(det_report_to_error_spec(ModuleInfo), ContextMsgs),
    % Programmers prefer reading messages in order of context.
    sort_error_specs(Specs0, Specs),
    write_error_specs(Specs, 0, WarnCnt, 0, ErrCnt, !IO).

:- func det_report_to_error_spec(module_info, context_det_msg) = error_spec.

det_report_to_error_spec(ModuleInfo, ContextDetMsg) = Spec :-
    ContextDetMsg = context_det_msg(Context, DetMsg),
    (
        (
            DetMsg = multidet_disj(DisjunctContexts),
            Pieces = [words("Warning: the disjunction with arms on lines"),
                words(det_report_context_lines(DisjunctContexts)),
                words("has no outputs, but can succeed more than once.")]
        ;
            DetMsg = det_disj(DisjunctContexts),
            Pieces = [words("Warning: the disjunction with arms on lines"),
                words(det_report_context_lines(DisjunctContexts)),
                words("will succeed exactly once.")]
        ;
            DetMsg = semidet_disj(DisjunctContexts),
            Pieces = [words("Warning: the disjunction with arms on lines"),
                words(det_report_context_lines(DisjunctContexts)),
                words("is semidet, yet it has an output.")]
        ;
            DetMsg = zero_soln_disj(DisjunctContexts),
            Pieces = [words("Warning: the disjunction with arms on lines"),
                words(det_report_context_lines(DisjunctContexts)),
                words("cannot succeed.")]
        ;
            DetMsg = zero_soln_disjunct,
            Pieces = [words("Warning: this disjunct"),
                words("will never have any solutions.")]
        ;
            DetMsg = ite_cond_cannot_fail,
            Pieces = [words("Warning: the condition of this if-then-else"),
                words("cannot fail.")]
        ;
            DetMsg = ite_cond_cannot_succeed,
            Pieces = [words("Warning: the condition of this if-then-else"),
                words("cannot succeed.")]
        ;
            DetMsg = negated_goal_cannot_fail,
            Pieces = [words("Warning: the negated goal cannot fail.")]
        ;
            DetMsg = negated_goal_cannot_succeed,
            Pieces = [words("Warning: the negated goal cannot succeed.")]
        ;
            DetMsg = warn_call_to_obsolete(PredId),
            % XXX warn_obsolete isn't really a simple code warning.
            % We should add a separate warning type for this.
            PredPieces = describe_one_pred_name(ModuleInfo,
                should_module_qualify, PredId),
            Pieces = [words("Warning: call to obsolete")] ++ PredPieces
                ++ [suffix(".")]
        ),
        Spec = error_spec(severity_warning, phase_detism_check,
            [simple_msg(Context,
                [option_is_set(warn_simple_code, yes, [always(Pieces)])])])
    ;
        (
            DetMsg = goal_cannot_succeed,
            MainPieces = [words("Warning: this goal cannot succeed.")],
            VerbosePieces =
                [words("The compiler will optimize away this goal,"),
                words("replacing it with `fail'."),
                words("To disable this optimization, use"),
                words("the `--fully-strict' option.")]
        ;
            DetMsg = det_goal_has_no_outputs,
            MainPieces = [words("Warning: det goal has no outputs.")],
            VerbosePieces =
                [words("The compiler will optimize away this goal,"),
                words("replacing it with `true'."),
                words("To disable this optimization, use"),
                words("the `--fully-strict' option.")]
        ;
            DetMsg = warn_infinite_recursion,
            % It would be better if we supplied more information than just
            % the line number, e.g. we should print the name of the containing
            % predicate.
            MainPieces = [words("Warning: recursive call will lead"),
                words("to infinite recursion.")],
            VerbosePieces = [words("If this recursive call is executed,"),
                words("the procedure will call itself"),
                words("with exactly the same input arguments,"),
                words("leading to infinite recursion.")]
        ),
        Spec = error_spec(severity_warning, phase_detism_check,
            [simple_msg(Context,
                [option_is_set(warn_simple_code, yes,
                    [always(MainPieces), verbose_only(VerbosePieces)])])])
    ;
        DetMsg = duplicate_call(SeenCall, PrevContext),
        CallPieces = det_report_seen_call_id(ModuleInfo, SeenCall),
        CurPieces = [words("Warning: redundant") | CallPieces] 
            ++ [suffix(".")],
        PrevPieces = [words("Here is the previous") | CallPieces]
            ++ [suffix(".")],
        Spec = error_spec(severity_warning, phase_detism_check,
            [simple_msg(Context,
                [option_is_set(warn_duplicate_calls, yes,
                    [always(CurPieces)])]),
            error_msg(yes(PrevContext), yes, 0,
                [option_is_set(warn_duplicate_calls, yes,
                    [always(PrevPieces)])])
            ])
    ;
        DetMsg = nested_promise_eqv_solution_sets(OuterContext),
        Pieces = [words("Error: "),
            words("`promise_equivalent_solution_sets' scope"),
            words("is nested inside another.")],
        OuterPieces = [words("This is the outer"),
            words("`promise_equivalent_solution_sets' scope.")],
        Spec = error_spec(severity_warning, phase_detism_check,
            [simple_msg(Context,
                [option_is_set(warn_simple_code, yes, [always(Pieces)])]),
            simple_msg(OuterContext,
                [option_is_set(warn_simple_code, yes, [always(OuterPieces)])])
            ])
    ;
        (
            DetMsg = unknown_format_string(SymName, Arity),
            Pieces = [words("Unknown format string in call to"),
                sym_name_and_arity(SymName / Arity), suffix(".")]
        ;
            DetMsg = unknown_format_values(SymName, Arity),
            Pieces = [words("Unknown format values in call to"),
                sym_name_and_arity(SymName / Arity), suffix(".")]
        ),
        Spec = error_spec(severity_warning, phase_detism_check,
            [simple_msg(Context,
                [option_is_set(warn_unknown_format_calls, yes,
                    [always(Pieces)])])])
    ;
        DetMsg = bad_format(SymName, Arity, Msg),
        Pieces = [words("Mismatched format and values in call to"),
            sym_name_and_arity(SymName / Arity), suffix(":"), nl, words(Msg)],
        Spec = error_spec(severity_warning, phase_detism_check,
            [simple_msg(Context,
                [option_is_set(warn_known_bad_format_calls, yes,
                    [always(Pieces)])])])
    ;
        DetMsg = cc_unify_can_fail(_GoalInfo, Var, Type, VarSet, GoalContext),
        (
            GoalContext = ccuc_switch,
            VarStr = mercury_var_to_string(Var, VarSet, no),
            Pieces0 = [words("In switch on variable"), quote(VarStr),
                suffix(":"), nl]
        ;
            GoalContext = ccuc_unify(UnifyContext),
            hlds_out.unify_context_to_pieces(UnifyContext, [], Pieces0)
        ),
        ( type_to_ctor_and_args(Type, TypeCtor, _TypeArgs) ->
            TypeCtor = type_ctor(TypeCtorName, TypeCtorArity),
            TypeCtorSymName = TypeCtorName / TypeCtorArity
        ;
            unexpected(this_file, "det_report_to_error_spec: " ++
                "cc_unify_can_fail: type_to_ctor_and_args failed")
        ),
        (
            Pieces0 = [],
            ErrorMsg = "Error:"
        ;
            Pieces0 = [_ | _],
            ErrorMsg = "error:"
        ),
        Pieces1 = [words(ErrorMsg),
            words("unification for non-canonical type"),
            sym_name_and_arity(TypeCtorSymName),
            words("is not guaranteed to succeed.")],
        VerbosePieces = [words("Since the type has a user-defined"),
            words("equality predicate, I must presume that"),
            words("there is more than one possible concrete"),
            words("representation for each abstract value"),
            words("of this type. The success of this unification"),
            words("might depend on the choice of concrete"),
            words("representation. Figuring out whether there is"),
            words("a solution to this unification would require"),
            words("backtracking over all possible"),
            words("representations, but I'm not going to do that"),
            words("implicitly. (If that's really what you want,"),
            words("you must do it explicitly.)")],
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context,
                [always(Pieces0 ++ Pieces1), verbose_only(VerbosePieces)])])
    ;
        DetMsg = cc_unify_in_wrong_context(_GoalInfo, Var, Type, VarSet,
            GoalContext, FailingContexts),
        (
            GoalContext = ccuc_switch,
            VarStr = mercury_var_to_string(Var, VarSet, no),
            Pieces0 = [words("In switch on variable `" ++ VarStr ++ "':"), nl]
        ;
            GoalContext = ccuc_unify(UnifyContext),
            unify_context_first_to_pieces(yes, _, UnifyContext, [], Pieces0)
        ),
        ( type_to_ctor_and_args(Type, TypeCtor, _TypeArgs) ->
            TypeCtorStr = hlds_out.type_ctor_to_string(TypeCtor)
        ;
            unexpected(this_file, "det_report_msg: " ++
                "cc_unify_in_wrong_context: type_to_ctor_and_args failed")
        ),
        (
            Pieces0 = [],
            ErrorMsg = "Error:"
        ;
            Pieces0 = [_ | _],
            ErrorMsg = "error:"
        ),
        Pieces1 = [words(ErrorMsg),
            words("unification for non-canonical type"),
            words("`" ++ TypeCtorStr ++ "'"),
            words("occurs in a context which requires all solutions."), nl],
        VerbosePieces = [words("Since the type has a user-defined"),
            words("equality predicate, I must presume that"),
            words("there is more than one possible concrete"),
            words("representation for each abstract value"),
            words("of this type. The results of this unification"),
            words("might depend on the choice of concrete"),
            words("representation. Finding all possible"),
            words("solutions to this unification would require"),
            words("backtracking over all possible"),
            words("representations, but I'm not going to do that"),
            words("implicitly. (If that's really what you want,"),
            words("you must do it explicitly.)")],
        ContextMsgs = failing_contexts_description(ModuleInfo, VarSet,
            FailingContexts),
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context,
                [always(Pieces0 ++ Pieces1), verbose_only(VerbosePieces)])]
            ++ ContextMsgs)
    ;
        DetMsg = cc_pred_in_wrong_context(_GoalInfo, Detism, PredId,
            _ModeId, VarSet, FailingContexts),
        PredPieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
            PredId),
        FirstPieces = [words("Error: call to")] ++ PredPieces ++
            [words("with determinism"), quote(mercury_det_to_string(Detism)),
            words("occurs in a context which requires all solutions."), nl],
        ContextMsgs = failing_contexts_description(ModuleInfo, VarSet,
            FailingContexts),
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context, [always(FirstPieces)])] ++ ContextMsgs)
    ;
        DetMsg = higher_order_cc_pred_in_wrong_context(_GoalInfo, Detism,
            VarSet, FailingContexts),
        FirstPieces = [words("Error: higher-order call to predicate with"),
            words("determinism"), quote(mercury_det_to_string(Detism)),
            words("occurs in a context which requires all solutions."), nl],
        ContextMsgs = failing_contexts_description(ModuleInfo, VarSet,
            FailingContexts),
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context, [always(FirstPieces)])] ++ ContextMsgs)
    ;
        DetMsg = error_in_lambda(DeclaredDetism, InferredDetism,
            Goal, _GoalInfo, PredId, ProcId),
        PredPieces = describe_one_proc_name_mode(ModuleInfo,
            should_not_module_qualify, proc(PredId, ProcId)),
        Pieces =
            [words("In")] ++ PredPieces ++ [suffix(":"), nl,
            words("Determinism error in lambda expression."), nl,
            words("Declared"),
            quote(determinism_to_string(DeclaredDetism)), suffix(","),
            words("inferred"),
            quote(determinism_to_string(InferredDetism)), suffix("'.")],
        module_info_get_globals(ModuleInfo, Globals),
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        det_info_init(ModuleInfo, VarTypes, PredId, ProcId, Globals, DetInfo),
        det_diagnose_goal(Goal, DeclaredDetism, [], DetInfo, GoalMsgs0),
        sort_error_msgs(GoalMsgs0, GoalMsgs),
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context, [always(Pieces)])] ++ GoalMsgs)
    ;
        DetMsg = par_conj_not_det(InferredDetism, PredId, ProcId,
            _GoalInfo, Goals),
        determinism_components(InferredDetism, CanFail, MaxSoln),
        ( CanFail \= cannot_fail ->
            First = "Error: parallel conjunct may fail."
        ; MaxSoln = at_most_many ->
            First = "Error: parallel conjunct may have multiple solutions."
        ;
            unexpected(this_file,
                "strange determinism error for parallel conjunction")
        ),
        Rest = "The current implementation supports only "
            ++ "single-solution non-failing parallel conjunctions.",
        Pieces = [words(First), words(Rest)],
        module_info_get_globals(ModuleInfo, Globals),
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        det_info_init(ModuleInfo, VarTypes, PredId, ProcId, Globals, DetInfo),
        det_diagnose_conj(Goals, detism_det, [], DetInfo, GoalMsgs0),
        sort_error_msgs(GoalMsgs0, GoalMsgs),
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context, [always(Pieces)])] ++ GoalMsgs)
    ;
        DetMsg = pragma_c_code_without_det_decl(PredId, ProcId),
        ProcPieces = describe_one_proc_name_mode(ModuleInfo,
            should_not_module_qualify, proc(PredId, ProcId)),
        Pieces = [words("In")] ++ ProcPieces ++ [suffix(":"), nl,
            words("error: `:- pragma c_code(...)' for a procedure"),
            words("without a determinism declaration.")],
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context, [always(Pieces)])])
    ;
        DetMsg = has_io_state_but_not_det(PredId, ProcId),
        ProcPieces = describe_one_proc_name_mode(ModuleInfo,
            should_not_module_qualify, proc(PredId, ProcId)),
        Pieces = [words("In")] ++ ProcPieces ++ [suffix(":"), nl,
            words("error: invalid determinism for a predicate"),
            words("with I/O state arguments.")],
        VerbosePieces = [words("Valid determinisms are "),
            words("det, cc_multi and erroneous.")],
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context,
                [always(Pieces), verbose_only(VerbosePieces)])])
    ;
        (
            DetMsg = will_not_throw_with_erroneous(PredId, ProcId),
            ProcPieces = describe_one_proc_name_mode(ModuleInfo,
                should_not_module_qualify, proc(PredId, ProcId)),
            Pieces = ProcPieces ++
                [words("has determinism erroneous but also has"),
                words("foreign clauses that have a"),
                fixed("`will_not_throw_exception' attribute."),
                words("This attribute cannot be applied"),
                words("to erroneous procedures.")]
        ;
            DetMsg = export_model_non_proc(_PredId, _ProcId, Detism),
            Pieces = [words("Error: "),
                fixed("`:- pragma export' declaration"),
                words("for a procedure that has a determinism of"),
                fixed(hlds_out.determinism_to_string(Detism) ++ ".")]
        ;
            DetMsg = arbitrary_without_promise,
            Pieces = [words("Error: "),
                words("this `arbitrary' scope is not nested inside"),
                words("a `promise_equivalent_solution_sets' scope.")]
        ),
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context, [always(Pieces)])])
    ;
        DetMsg = arbitrary_promise_overlap(PromiseContext, VarSet,
            OverlapVars),
        VarNames = list.map(lookup_var_name_in_varset(VarSet),
            set.to_sorted_list(OverlapVars)),
        (
            VarNames = [],
            unexpected(this_file, "det_report_msg: " ++
                "arbitrary_promise_overlap empty")
        ;
            VarNames = [_],
            VarStr = "the variable"
        ;
            VarNames = [_, _ | _],
            VarStr = "the following variables:"
        ),
        Pieces = [words("Error: "),
            words("this `arbitrary' scope and the"),
            words("`promise_equivalent_solution_sets' scope"),
            words("it is nested inside overlap on"), words(VarStr)]
            ++ list_to_pieces(VarNames) ++ [suffix(".")],
        PromisePieces = [words("This is the outer "),
            words("`promise_equivalent_solution_sets' scope.")],
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context, [always(Pieces)]),
            simple_msg(PromiseContext, [always(PromisePieces)])])
    ;
        DetMsg = promise_solutions_missing_vars(Kind, VarSet, Vars),
        VarNames = list.map(lookup_var_name_in_varset(VarSet),
            set.to_sorted_list(Vars)),
        KindStr = promise_solutions_kind_str(Kind),
        (
            VarNames = [],
            unexpected(this_file, "det_report_msg: " ++
                "promise_solutions_missing_vars empty")
        ;
            VarNames = [_],
            ListStr = "a variable that is not listed:"
        ;
            VarNames = [_, _ | _],
            ListStr = "some variables that are not listed:"
        ),
        Pieces = [words("Error: the"), words(add_quotes(KindStr)),
            words("goal binds"), words(ListStr)]
            ++ list_to_pieces(VarNames) ++ [suffix(".")],
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context, [always(Pieces)])])
    ;
        DetMsg = promise_solutions_extra_vars(Kind, VarSet, Vars),
        VarNames = list.map(lookup_var_name_in_varset(VarSet),
            set.to_sorted_list(Vars)),
        KindStr = promise_solutions_kind_str(Kind),
        (
            VarNames = [],
            unexpected(this_file,
                "det_report_msg: promise_solutions_extra_vars empty")
        ;
            VarNames = [_],
            ListStr = "an extra variable:"
        ;
            VarNames = [_, _ | _],
            ListStr = "some extra variables:"
        ),
        Pieces = [words("Error: the"), words(add_quotes(KindStr)),
            words("goal lists"), words(ListStr)] ++
            list_to_pieces(VarNames) ++ [suffix(".")],
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context, [always(Pieces)])])
    ;
        DetMsg = trace_goal_not_det(Detism),
        DetismStr = determinism_to_string(Detism),
        Pieces = [words("Error: trace goal has determinism"),
            quote(DetismStr), suffix(","),
            words("should be det or cc_multi.")],
        Spec = error_spec(severity_error, phase_detism_check,
            [simple_msg(Context, [always(Pieces)])])
    ).

det_msg_is_any_mode_msg(multidet_disj(_), all_modes).
det_msg_is_any_mode_msg(det_disj(_), all_modes).
det_msg_is_any_mode_msg(semidet_disj(_), all_modes).
det_msg_is_any_mode_msg(zero_soln_disj(_), all_modes).
det_msg_is_any_mode_msg(zero_soln_disjunct, all_modes).
det_msg_is_any_mode_msg(ite_cond_cannot_fail, all_modes).
det_msg_is_any_mode_msg(ite_cond_cannot_succeed, all_modes).
det_msg_is_any_mode_msg(negated_goal_cannot_fail, all_modes).
det_msg_is_any_mode_msg(negated_goal_cannot_succeed, all_modes).
det_msg_is_any_mode_msg(goal_cannot_succeed, all_modes).
det_msg_is_any_mode_msg(det_goal_has_no_outputs, all_modes).
det_msg_is_any_mode_msg(warn_call_to_obsolete(_), all_modes).
det_msg_is_any_mode_msg(warn_infinite_recursion, any_mode).
det_msg_is_any_mode_msg(duplicate_call(_, _), any_mode).
det_msg_is_any_mode_msg(unknown_format_string(_, _), any_mode).
det_msg_is_any_mode_msg(unknown_format_values(_, _), any_mode).
det_msg_is_any_mode_msg(bad_format(_, _, _), any_mode).
det_msg_is_any_mode_msg(cc_unify_can_fail(_, _, _, _, _), any_mode).
det_msg_is_any_mode_msg(cc_unify_in_wrong_context(_, _, _, _, _, _), any_mode).
det_msg_is_any_mode_msg(cc_pred_in_wrong_context(_, _, _, _, _, _), any_mode).
det_msg_is_any_mode_msg(higher_order_cc_pred_in_wrong_context(_, _, _, _),
    any_mode).
det_msg_is_any_mode_msg(error_in_lambda(_, _, _, _, _, _), any_mode).
det_msg_is_any_mode_msg(par_conj_not_det(_, _, _, _, _), any_mode).
det_msg_is_any_mode_msg(pragma_c_code_without_det_decl(_, _), any_mode).
det_msg_is_any_mode_msg(has_io_state_but_not_det(_, _), any_mode).
det_msg_is_any_mode_msg(will_not_throw_with_erroneous(_, _), any_mode).
det_msg_is_any_mode_msg(export_model_non_proc(_, _, _), any_mode).
det_msg_is_any_mode_msg(nested_promise_eqv_solution_sets(_), any_mode).
det_msg_is_any_mode_msg(arbitrary_without_promise, any_mode).
det_msg_is_any_mode_msg(arbitrary_promise_overlap(_, _, _), any_mode).
det_msg_is_any_mode_msg(promise_solutions_missing_vars(_, _, _), any_mode).
det_msg_is_any_mode_msg(promise_solutions_extra_vars(_, _, _), any_mode).
det_msg_is_any_mode_msg(trace_goal_not_det(_), any_mode).

:- func promise_solutions_kind_str(promise_solutions_kind) = string.

promise_solutions_kind_str(equivalent_solutions)
    = "promise_equivalent_solutions".
promise_solutions_kind_str(equivalent_solution_sets)
    = "promise_equivalent_solution_sets".
promise_solutions_kind_str(equivalent_solution_sets_arbitrary)
    = "arbitrary".

:- func lookup_var_name_in_varset(prog_varset, prog_var) = string.

lookup_var_name_in_varset(VarSet, Var) =
    mercury_var_to_string(Var, VarSet, no).

:- func failing_contexts_description(module_info, prog_varset,
    list(failing_context)) = list(error_msg).

failing_contexts_description(ModuleInfo, VarSet, FailingContexts) =
    list.map(failing_context_description(ModuleInfo, VarSet),
        FailingContexts).

:- func failing_context_description(module_info, prog_varset,
    failing_context) = error_msg.

failing_context_description(ModuleInfo, VarSet, Context - FailingGoal)
        = Msg :-
    (
        FailingGoal = incomplete_switch(Var),
        VarStr = mercury_var_to_string(Var, VarSet, no),
        Pieces = [words("Switch on"), fixed(VarStr), words("is incomplete.")]
    ;
        FailingGoal = fail_goal,
        Pieces = [words("Fail goal can fail.")]
    ;
        FailingGoal = test_goal(Var1, Var2),
        Var1Str = mercury_var_to_string(Var1, VarSet, no),
        Var2Str = mercury_var_to_string(Var2, VarSet, no),
        Pieces = [words("Unification of"), fixed(Var1Str),
            words("and"), fixed(Var2Str), words("can fail.")]
    ;
        FailingGoal = deconstruct_goal(Var, ConsId),
        VarStr = mercury_var_to_string(Var, VarSet, no),
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

:- func det_report_seen_call_id(module_info, seen_call_id)
    = list(format_component).

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

disable_det_warnings(OptionsToRestore, !IO) :-
    globals.io_lookup_option(warn_simple_code, WarnSimple, !IO),
    globals.io_lookup_option(warn_det_decls_too_lax,
        WarnDeclsTooLax, !IO),
    globals.io_set_option(warn_simple_code, bool(no), !IO),
    globals.io_set_option(warn_det_decls_too_lax, bool(no), !IO),
    OptionsToRestore = [
        warn_simple_code - WarnSimple,
        warn_det_decls_too_lax - WarnDeclsTooLax
    ].

restore_det_warnings(OptionsToRestore, !IO) :-
    list.foldl(restore_option, OptionsToRestore, !IO).

:- pred restore_option(pair(option, option_data)::in, io::di, io::uo) is det.

restore_option(Option - Value, !IO) :-
    globals.io_set_option(Option, Value, !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "det_report.m".

%-----------------------------------------------------------------------------%
:- end_module det_report.
%-----------------------------------------------------------------------------%
