%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: det_report.m.
% Author: zs.

% This module handles reporting of determinism errors and warnings,
% as well as errors and warnings from some other related compiler passes
% such as simplify.

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
    ;       warn_obsolete(pred_id)
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
                set(prog_var)).

:- type seen_call_id
    --->    seen_call(pred_id, proc_id)
    ;       higher_order_call.

:- type cc_unify_context
    --->    unify(unify_context)
    ;       switch.

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
        MaybeDetism = no
    ;
        MaybeDetism = yes(DeclaredDetism),
        compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
        (
            Cmp = sameas
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
                \+ check_marker(Markers, class_instance_method),

                % Don't report warnings for procedures with no clauses.
                \+ check_marker(Markers, stub),

                % Don't report warnings for compiler-generated Unify, Compare
                % or Index procedures, since the user has no way to shut
                % these up. These can happen for the Unify pred for the unit
                % type, if such types are not boxed (as they are not
                % boxed for the IL backend).
                \+ is_unify_or_compare_pred(PredInfo0),

                % Don't warn about predicates which are inferred erroneous
                % when the appropiate option is set. This is to avoid warnings
                % about unimplemented predicates.
                (
                    WarnAboutInferredErroneous = yes
                ;
                    WarnAboutInferredErroneous = no,
                    InferredDetism \= erroneous
                )
            ->
                Message = "warning: determinism declaration " ++
                    "could be tighter.\n",
                record_warning(!IO),
                report_determinism_problem(PredId, ProcId, !.ModuleInfo,
                    Message, DeclaredDetism, InferredDetism, ReportSpec),
                write_error_specs([ReportSpec], !IO)
            ;
                true
            )
        ;
            Cmp = tighter,
            module_info_incr_errors(!ModuleInfo),
            Message = "  error: determinism declaration not satisfied.\n",
            record_warning(!IO),
            report_determinism_problem(PredId, ProcId, !.ModuleInfo, Message,
                DeclaredDetism, InferredDetism, ReportSpec),
            proc_info_get_goal(ProcInfo0, Goal),
            proc_info_get_vartypes(ProcInfo0, VarTypes),
            globals.io_get_globals(Globals, !IO),
            det_info_init(!.ModuleInfo, VarTypes, PredId, ProcId, Globals,
                DetInfo),
            det_diagnose_goal(Goal, DeclaredDetism, [], DetInfo, _, [], Specs),
            write_error_specs([ReportSpec | Specs], !IO)
        )
    ),

    % Make sure the code model is valid given the eval method.
    proc_info_get_eval_method(ProcInfo0, EvalMethod),
    ( valid_determinism_for_eval_method(EvalMethod, InferredDetism) = yes ->
        proc_info_set_eval_method(EvalMethod, ProcInfo0, ProcInfo),
        pred_info_get_procedures(PredInfo0, ProcTable0),
        map.det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
        pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ;
        proc_info_get_context(ProcInfo0, Context),
        write_error_pieces(Context, 0,
            [words("Error: `pragma "
                ++ eval_method_to_one_string(EvalMethod) ++ "'"),
            words("declaration not allowed for procedure"),
            words("with determinism `"
                ++ determinism_to_string(InferredDetism) ++ "'.")], !IO),
        globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
        (
            VerboseErrors = yes,
            solutions.solutions(get_valid_dets(EvalMethod), Detisms),
            DetismStrs = list.map(determinism_to_string, Detisms),
            DetismPieces = list_to_pieces(DetismStrs),
            write_error_pieces_not_first_line(Context, 0,
                [words("The pragma requested is only valid"),
                words("for the following determinism(s):") |
                DetismPieces], !IO)
        ;
            VerboseErrors = no,
            globals.io_set_extra_error_info(yes, !IO)
        ),
        module_info_incr_errors(!ModuleInfo)
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

determinism(det).
determinism(semidet).
determinism(multidet).
determinism(nondet).
determinism(cc_multidet).
determinism(cc_nondet).
determinism(erroneous).
determinism(failure).

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
        DeclaredDetism \= det,
        DeclaredDetism \= cc_multidet
    ->
        proc_info_get_context(ProcInfo, Context1),
        write_error_pieces(Context1, 0,
            [words("Error: main/2 must be `det' or `cc_multi'.")], !IO),
        module_info_incr_errors(!ModuleInfo)
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
        % if it is a mode for a function...
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
        Pieces = [words("Error: invalid determinism for")]
            ++ PredModePieces ++ [suffix(":"), nl,
            words("the primary mode of a function cannot be"),
            words("`" ++ mercury_det_to_string(InferredDetism) ++ "'.")],
        write_error_pieces(FuncContext, 0, Pieces, !IO),
        globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
        (
            VerboseErrors = yes,
            ExtMsg = func_primary_mode_det_msg,
            write_error_pieces_not_first_line(FuncContext, 0,
                [words(ExtMsg)], !IO)
        ;
            VerboseErrors = no,
            globals.io_set_extra_error_info(yes, !IO)
        ),
        module_info_incr_errors(!ModuleInfo)
    ;
        true
    ).

:- func func_primary_mode_det_msg = string.

func_primary_mode_det_msg =
    "In Mercury, a function is supposed to be a true mathematical" ++
    "function of its arguments; that is, the value of the function's" ++
    "result should be determined only by the values of its arguments." ++
    "(Allowing functions to have more than one result for the same" ++
    "arguments would break referential transparency.)" ++
    "Most likely, this procedure should be a predicate, not a function.".

det_check_lambda(DeclaredDetism, InferredDetism, Goal, GoalInfo, DetInfo,
        Msgs) :-
    compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
    ( Cmp = tighter ->
        det_info_get_pred_id(DetInfo, PredId),
        det_info_get_proc_id(DetInfo, ProcId),
        goal_info_get_context(GoalInfo, Context),
        Msg = error_in_lambda(DeclaredDetism, InferredDetism, Goal, GoalInfo,
            PredId, ProcId),
        ContextMsg = context_det_msg(Context, Msg),
        Msgs = [ContextMsg]
    ;
        % We don't bother issuing warnings if the determinism was too loose;
        % that will often be the case, and should not be warned about.
        Msgs = []
    ).

:- pred report_determinism_problem(pred_id::in, proc_id::in, module_info::in,
    string::in, determinism::in, determinism::in,
    error_msg_spec::out(known_error_msg_spec)) is det.

report_determinism_problem(PredId, ProcId, ModuleInfo, Message,
        DeclaredDetism, InferredDetism, Spec) :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_get_context(ProcInfo, Context),
    ProcPieces = describe_one_proc_name_mode(ModuleInfo,
        should_not_module_qualify, proc(PredId, ProcId)),
    Pieces = [words("In")] ++ ProcPieces ++ [suffix(":"), nl,
        words(Message), nl,
        words("Declared `" ++ determinism_to_string(DeclaredDetism)
            ++ "', inferred `" ++ determinism_to_string(InferredDetism)
            ++ "'.")],
    Spec = error_msg_spec(no, Context, 0, Pieces).

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
    list(switch_context)::in, det_info::in, bool::out,
    list(error_msg_spec)::in(known_error_msg_specs),
    list(error_msg_spec)::out(known_error_msg_specs)) is det.

det_diagnose_goal(Goal - GoalInfo, Desired, SwitchContext, DetInfo, Diagnosed,
        !Specs) :-
    goal_info_get_determinism(GoalInfo, Actual),
    ( compare_determinisms(Desired, Actual, tighter) ->
        det_diagnose_goal_2(Goal, GoalInfo, Desired, Actual, SwitchContext,
            DetInfo, Diagnosed, !Specs)
    ;
        Diagnosed = no
    ).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
    determinism::in, determinism::in, list(switch_context)::in,
    det_info::in, bool::out,
    list(error_msg_spec)::in(known_error_msg_specs),
    list(error_msg_spec)::out(known_error_msg_specs)) is det.

det_diagnose_goal_2(conj(_, Goals), _GoalInfo, Desired, _Actual, Context,
        DetInfo, Diagnosed, !Specs) :-
    det_diagnose_conj(Goals, Desired, Context, DetInfo, Diagnosed, !Specs).

det_diagnose_goal_2(disj(Goals), GoalInfo, Desired, Actual, SwitchContext,
        DetInfo, Diagnosed, !Specs) :-
    det_diagnose_disj(Goals, Desired, Actual, SwitchContext, DetInfo, 0,
        ClausesWithSoln, Diagnosed1, !Specs),
    determinism_components(Desired, _, DesSolns),
    (
        DesSolns \= at_most_many,
        DesSolns \= at_most_many_cc,
        ClausesWithSoln > 1
    ->
        goal_info_get_context(GoalInfo, Context),
        Pieces = [words("Disjunction has multiple clauses with solutions.")],
        Spec = error_msg_spec(no, Context, 0, Pieces),
        inst_preserving_append(!.Specs, [Spec], !:Specs),
        Diagnosed = yes
    ;
        Diagnosed = Diagnosed1
    ).

    % The determinism of a switch is the worst of the determinism of each of
    % the cases. Also, if only a subset of the constructors are handled,
    % then it is semideterministic or worse - this is determined
    % in switch_detection.m and handled via the CanFail field.
    %
det_diagnose_goal_2(switch(Var, SwitchCanFail, Cases), GoalInfo,
        Desired, _Actual, SwitchContext, DetInfo, Diagnosed, !Specs) :-
    (
        SwitchCanFail = can_fail,
        determinism_components(Desired, cannot_fail, _)
    ->
        goal_info_get_context(GoalInfo, Context),
        det_diagnose_switch_context(Context, SwitchContext, DetInfo, !Specs),
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
        Spec = error_msg_spec(no, Context, 0, Pieces),
        inst_preserving_append(!.Specs, [Spec], !:Specs),
        Diagnosed1 = yes
    ;
        Diagnosed1 = no
    ),
    det_diagnose_switch(Var, Cases, Desired, SwitchContext, DetInfo,
        Diagnosed2, !Specs),
    bool.or(Diagnosed1, Diagnosed2, Diagnosed).

det_diagnose_goal_2(call(PredId, ProcId, _, _, CallContext, _), GoalInfo,
        Desired, Actual, _, DetInfo, yes, !Specs) :-
    goal_info_get_context(GoalInfo, Context),
    det_report_call_context(Context, CallContext, DetInfo, PredId, ProcId,
        InitSpecs, Spec),
    det_diagnose_atomic_goal(Desired, Actual, InitSpecs, Spec, !Specs).

det_diagnose_goal_2(generic_call(GenericCall, _, _, _), GoalInfo,
        Desired, Actual, _, _DetInfo, yes, !Specs) :-
    goal_info_get_context(GoalInfo, Context),
    report_generic_call_context(Context, GenericCall, Spec),
    det_diagnose_atomic_goal(Desired, Actual, [], Spec, !Specs).

det_diagnose_goal_2(unify(LHS, RHS, _, _, UnifyContext), GoalInfo,
        Desired, Actual, _, DetInfo, yes, !Specs) :-
    goal_info_get_context(GoalInfo, Context),
    First = yes,
    Last = yes,
    det_report_unify_context(First, Last, Context, UnifyContext,
        DetInfo, LHS, RHS, Spec),
    det_diagnose_atomic_goal(Desired, Actual, [], Spec, !Specs).

det_diagnose_goal_2(if_then_else(_Vars, Cond, Then, Else), _GoalInfo,
        Desired, _Actual, SwitchContext, DetInfo, Diagnosed, !Specs) :-
    determinism_components(Desired, _DesiredCanFail, DesiredSolns),
    Cond = _CondGoal - CondInfo,
    goal_info_get_determinism(CondInfo, CondDetism),
    determinism_components(CondDetism, _CondCanFail, CondSolns),
    (
        CondSolns = at_most_many,
        DesiredSolns \= at_most_many
    ->
        determinism_components(DesiredCond, can_fail, DesiredSolns),
        det_diagnose_goal(Cond, DesiredCond, SwitchContext, DetInfo,
            Diagnosed1, !Specs)
    ;
        Diagnosed1 = no
    ),
    det_diagnose_goal(Then, Desired, SwitchContext, DetInfo, Diagnosed2,
        !Specs),
    det_diagnose_goal(Else, Desired, SwitchContext, DetInfo, Diagnosed3,
        !Specs),
    bool.or(Diagnosed2, Diagnosed3, Diagnosed23),
    bool.or(Diagnosed1, Diagnosed23, Diagnosed).

det_diagnose_goal_2(not(_), GoalInfo, Desired, Actual, _, _, Diagnosed,
        !Specs) :-
    determinism_components(Desired, DesiredCanFail, DesiredSolns),
    determinism_components(Actual, ActualCanFail, ActualSolns),
    (
        DesiredCanFail = cannot_fail,
        ActualCanFail = can_fail
    ->
        goal_info_get_context(GoalInfo, Context),
        Pieces = [words("Negated goal can succeed.")],
        Spec = error_msg_spec(no, Context, 0, Pieces),
        inst_preserving_append(!.Specs, [Spec], !:Specs),
        Diagnosed = yes
    ;
        DesiredSolns = at_most_zero,
        ActualSolns \= at_most_zero
    ->
        goal_info_get_context(GoalInfo, Context),
        Pieces = [words("Negated goal can fail.")],
        Spec = error_msg_spec(no, Context, 0, Pieces),
        inst_preserving_append(!.Specs, [Spec], !:Specs),
        Diagnosed = yes
    ;
        Diagnosed = no
    ).

det_diagnose_goal_2(scope(_, Goal), _, Desired, Actual,
        SwitchContext, DetInfo, Diagnosed, !Specs) :-
    Goal = _ - GoalInfo,
    goal_info_get_determinism(GoalInfo, Internal),
    ( Actual = Internal ->
        InternalDesired = Desired
    ;
        determinism_components(Desired, CanFail, _),
        determinism_components(InternalDesired, CanFail, at_most_many)
    ),
    det_diagnose_goal(Goal, InternalDesired, SwitchContext, DetInfo, Diagnosed,
        !Specs).

det_diagnose_goal_2(foreign_proc(_, _, _, _, _, _), GoalInfo, Desired,
        _, _, _, yes, !Specs) :-
    goal_info_get_context(GoalInfo, Context),
    DesiredStr = determinism_to_string(Desired),
    Pieces = [words("Determinism declaration not satisfied."),
        words("Desired determinism is " ++ DesiredStr ++ ".")],
    Spec = error_msg_spec(no, Context, 0, Pieces),
    inst_preserving_append(!.Specs, [Spec], !:Specs).

det_diagnose_goal_2(shorthand(_), _, _, _, _, _, _, !Specs) :-
    % These should have been expanded out by now.
    unexpected(this_file, "det_diagnose_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

:- pred report_generic_call_context(prog_context::in, generic_call::in,
    error_msg_spec::out(known_error_msg_spec)) is det.

report_generic_call_context(Context, CallType, Spec) :-
    hlds_goal.generic_call_id(CallType, CallId),
    Pieces = [words(call_id_to_string(CallId))],
    Spec = error_msg_spec(no, Context, 0, Pieces).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_atomic_goal(determinism::in, determinism::in,
    list(error_msg_spec)::in(known_error_msg_specs),
    error_msg_spec::in(known_error_msg_spec),
    list(error_msg_spec)::in(known_error_msg_specs),
    list(error_msg_spec)::out(known_error_msg_specs)) is det.

det_diagnose_atomic_goal(Desired, Actual, InitSpecs, !.CurSpec, !Specs) :-
    determinism_components(Desired, DesiredCanFail, DesiredSolns),
    determinism_components(Actual, ActualCanFail, ActualSolns),
    compare_canfails(DesiredCanFail, ActualCanFail, CmpCanFail),
    ( CmpCanFail = tighter ->
        add_to_spec_at_end([words("can fail."), nl], !CurSpec),
        Diagnosed1 = yes
    ;
        Diagnosed1 = no
    ),
    compare_solncounts(DesiredSolns, ActualSolns, CmpSolns),
    ( CmpSolns = tighter ->
        ( DesiredSolns = at_most_one ->
            add_to_spec_at_end([words("can succeed more than once."), nl],
                !CurSpec)
        ;
            add_to_spec_at_end([words("can succeed."), nl], !CurSpec)
        ),
        Diagnosed2 = yes
    ;
        Diagnosed2 = no
    ),
    bool.or(Diagnosed1, Diagnosed2, Diagnosed),
    (
        Diagnosed = yes
    ;
        Diagnosed = no,
        Pieces = [words("has unknown determinism problem;"), nl,
            words("desired determinism is" ++
                determinism_to_string(Desired) ++ ","),
            words("while actual determinism is" ++
                determinism_to_string(Actual) ++ "."),
            nl],
        add_to_spec_at_end(Pieces, !CurSpec)
    ),
    inst_preserving_append(InitSpecs, [!.CurSpec], AllSpecs),
    inst_preserving_append(!.Specs, AllSpecs, !:Specs).

    % det_diagnose_conj is used for both normal [sequential] conjunction
    % and parallel conjunction.
    %
:- pred det_diagnose_conj(list(hlds_goal)::in, determinism::in,
    list(switch_context)::in, det_info::in, bool::out,
    list(error_msg_spec)::in(known_error_msg_specs),
    list(error_msg_spec)::out(known_error_msg_specs)) is det.

det_diagnose_conj([], _Desired, _SwitchContext, _DetInfo, no, !Specs).
det_diagnose_conj([Goal | Goals], Desired, SwitchContext, DetInfo,
        Diagnosed, !Specs) :-
    det_diagnose_goal(Goal, Desired, SwitchContext, DetInfo, Diagnosed1,
        !Specs),
    det_diagnose_conj(Goals, Desired, SwitchContext, DetInfo, Diagnosed2,
        !Specs),
    bool.or(Diagnosed1, Diagnosed2, Diagnosed).

:- pred det_diagnose_disj(list(hlds_goal)::in,
    determinism::in, determinism::in, list(switch_context)::in,
    det_info::in, int::in, int::out, bool::out,
    list(error_msg_spec)::in(known_error_msg_specs),
    list(error_msg_spec)::out(known_error_msg_specs)) is det.

det_diagnose_disj([], _Desired, _Actual, _SwitchContext, _DetInfo,
        !ClausesWithSoln, no, !Specs).
det_diagnose_disj([Goal | Goals], Desired, Actual, SwitchContext, DetInfo,
        !ClausesWithSoln, Diagnosed, !Specs) :-
    determinism_components(Actual, ActualCanFail, _),
    determinism_components(Desired, DesiredCanFail, DesiredSolns),
    (
        DesiredCanFail = cannot_fail,
        ActualCanFail = can_fail
    ->
        % if the disjunction was declared to never fail,
        % but we inferred that it might fail, then we
        % want to print an error message for every disjunct
        % that might fail
        ClauseCanFail = cannot_fail
    ;
        % otherwise, either the disjunction is allowed to
        % fail, or there is at least one disjunct that we
        % inferred won't fail, so we don't want any error
        % messages for the disjuncts that might fail
        ClauseCanFail = can_fail
    ),
    determinism_components(ClauseDesired, ClauseCanFail, DesiredSolns),
    det_diagnose_goal(Goal, ClauseDesired, SwitchContext, DetInfo,
        Diagnosed1, !Specs),
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
        !ClausesWithSoln, Diagnosed2, !Specs),
    bool.or(Diagnosed1, Diagnosed2, Diagnosed).

:- pred det_diagnose_switch(prog_var::in, list(case)::in, determinism::in,
    list(switch_context)::in, det_info::in, bool::out,
    list(error_msg_spec)::in(known_error_msg_specs),
    list(error_msg_spec)::out(known_error_msg_specs)) is det.

det_diagnose_switch(_Var, [], _Desired, _SwitchContext, _DetInfo, no, !Specs).
det_diagnose_switch(Var, [case(ConsId, Goal) | Cases], Desired,
        SwitchContext0, DetInfo, Diagnosed, !Specs) :-
    SwitchContext1 = [switch_context(Var, ConsId) | SwitchContext0],
    det_diagnose_goal(Goal, Desired, SwitchContext1, DetInfo, Diagnosed1,
        !Specs),
    det_diagnose_switch(Var, Cases, Desired, SwitchContext0, DetInfo,
        Diagnosed2, !Specs),
    bool.or(Diagnosed1, Diagnosed2, Diagnosed).

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
    list.append(PiecesHead, PiecesTail, Pieces).

%-----------------------------------------------------------------------------%

:- type switch_context
    --->    switch_context(prog_var, cons_id).

:- pred det_diagnose_switch_context(prog_context::in,
    list(switch_context)::in, det_info::in,
    list(error_msg_spec)::in(known_error_msg_specs),
    list(error_msg_spec)::out(known_error_msg_specs)) is det.

det_diagnose_switch_context(_Context, [], _, !Specs).
det_diagnose_switch_context(Context, [SwitchContext | SwitchContexts],
        DetInfo, !Specs) :-
    det_get_proc_info(DetInfo, ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    SwitchContext = switch_context(Var, ConsId),
    ConsIdStr = cons_id_to_string(ConsId),
    VarStr = mercury_var_to_string(Var, VarSet, no),
    Pieces = [words("Inside the case"), fixed(ConsIdStr),
        words("of the switch on"), fixed(VarStr), words(":")],
    Spec = error_msg_spec(no, Context, 0, Pieces),
    inst_preserving_append(!.Specs, [Spec], !:Specs),
    det_diagnose_switch_context(Context, SwitchContexts, DetInfo, !Specs).

%-----------------------------------------------------------------------------%

:- pred det_report_call_context(prog_context::in,
    maybe(call_unify_context)::in, det_info::in, pred_id::in, proc_id::in,
    list(error_msg_spec)::out(known_error_msg_specs),
    error_msg_spec::out(known_error_msg_spec)) is det.

det_report_call_context(Context, CallUnifyContext, DetInfo, PredId, ProcId,
        InitSpecs, Spec) :-
    det_info_get_module_info(DetInfo, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_origin(PredInfo, Origin),

    % If the error was in a call to a type-specific unification predicate
    % (i.e. in the unification itself), then don't print out the predicate
    % name, just print out the context.  If it wasn't, then print them
    % both out. (The latter can happen if there is a determinism error
    % in a function call inside some unification.)

    ( Origin = special_pred(spec_pred_unify - _) ->
        (
            CallUnifyContext = yes(call_unify_context(LHS, RHS, UC)),
            First = yes,
            Last = yes,
            det_report_unify_context(First, Last, Context, UC, DetInfo,
                LHS, RHS, Spec),
            InitSpecs = []
        ;
            % This shouldn't happen; every call to a compiler generated
            % type-specific unification predicate should have a unify_context.
            CallUnifyContext = no,
            Pieces = [words("Some weird unification"
                ++ "(or explicit call to a type-specific unify predicate?)")],
            Spec = error_msg_spec(no, Context, 0, Pieces),
            InitSpecs = []
        )
    ;
        (
            CallUnifyContext = yes(call_unify_context(LHS, RHS, UC)),
            First = yes,
            Last = no,
            det_report_unify_context(First, Last, Context, UC, DetInfo,
                LHS, RHS, UnifySpec0),
            add_to_spec_at_end([suffix(":")], UnifySpec0, UnifySpec),
            InitSpecs = [UnifySpec]
        ;
            CallUnifyContext = no,
            InitSpecs = []
        ),
        pred_info_get_procedures(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_declared_argmodes(ProcInfo, ArgModes),
        proc_info_get_inst_varset(ProcInfo, InstVarSet),
        PredPieces = describe_one_pred_name_mode(ModuleInfo,
            should_module_qualify, PredId, InstVarSet, ArgModes),
        CallPieces = [words("call to") | PredPieces],
        Spec = error_msg_spec(no, Context, 0, CallPieces)
    ).

%-----------------------------------------------------------------------------%

    % det_report_unify_context prints out information about the context of an
    % error, i.e. where the error occurred.
    %
    % The first two arguments are boolean flags that specify whether this is
    % the first part of a sentence (in which case we start the error message
    % with a capital letter) and whether it is the last part (in which case we
    % omit the word "in" on the final "... in unification ...").
    %
:- pred det_report_unify_context(bool::in, bool::in, prog_context::in,
    unify_context::in, det_info::in, prog_var::in, unify_rhs::in,
    error_msg_spec::out(known_error_msg_spec)) is det.

det_report_unify_context(!.First, Last, Context, UnifyContext, DetInfo,
        LHS, RHS, Spec) :-
    unify_context_to_pieces(!First, UnifyContext, [], UnifyContextPieces),
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
            RHS = var(RV),
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
    AllPieces = UnifyContextPieces ++ Pieces,
    Spec = error_msg_spec(no, Context, 0, AllPieces).

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

det_report_msgs(ContextMsgs0, ModuleInfo, WarnCnt, ErrCnt, !IO) :-
    globals.io_lookup_bool_option(warn_simple_code, WarnSimple, !IO),
    globals.io_lookup_bool_option(warn_duplicate_calls, WarnCalls, !IO),
    globals.io_lookup_bool_option(warn_unknown_format_calls,
        WarnUnknownFormat, !IO),
    globals.io_lookup_bool_option(warn_known_bad_format_calls,
        WarnKnownBadFormat, !IO),
        % Programmers prefer reading messages in order of context.
    list.sort(ContextMsgs0, ContextMsgs),
    det_report_msgs_2(ContextMsgs, WarnSimple, WarnCalls,
        WarnUnknownFormat, WarnKnownBadFormat, ModuleInfo,
        0, WarnCnt, 0, ErrCnt, !IO).

:- pred det_report_msgs_2(list(context_det_msg)::in, bool::in, bool::in,
    bool::in, bool::in, module_info::in, int::in, int::out, int::in, int::out,
    io::di, io::uo) is det.

det_report_msgs_2([], _, _, _, _, _ModuleInfo, !WarnCnt, !ErrCnt, !IO).
det_report_msgs_2([ContextMsg | ContextMsgs], WarnSimple, WarnCalls,
        WarnUnknownFormat, WarnKnownBadFormat, ModuleInfo, !WarnCnt,
        !ErrCnt, !IO) :-
    ContextMsg = context_det_msg(Context, Msg),
    det_msg_get_type(Msg, MsgType),
    (
        WarnSimple = no,
        MsgType = simple_code_warning
    ->
        true
    ;
        WarnCalls = no,
        MsgType = call_warning
    ->
        true
    ;
        WarnUnknownFormat = no,
        MsgType = format_unknown
    ->
        true
    ;
        WarnKnownBadFormat = no,
        MsgType = format_known_bad
    ->
        true
    ;
        det_report_msg(Msg, Context, ModuleInfo, !IO),
        (
            ( MsgType = simple_code_warning
            ; MsgType = call_warning
            ; MsgType = format_unknown
            ; MsgType = format_known_bad
            ),
            !:WarnCnt = !.WarnCnt + 1
        ;
            MsgType = det_error,
            !:ErrCnt = !.ErrCnt + 1
        )
    ),
    det_report_msgs_2(ContextMsgs, WarnSimple, WarnCalls,
        WarnUnknownFormat, WarnKnownBadFormat, ModuleInfo,
        !WarnCnt, !ErrCnt, !IO).

:- pred det_msg_get_type(det_msg::in, det_msg_type::out) is det.

det_msg_get_type(multidet_disj(_), simple_code_warning).
det_msg_get_type(det_disj(_), simple_code_warning).
det_msg_get_type(semidet_disj(_), simple_code_warning).
det_msg_get_type(zero_soln_disj(_), simple_code_warning).
det_msg_get_type(zero_soln_disjunct, simple_code_warning).
det_msg_get_type(ite_cond_cannot_fail, simple_code_warning).
det_msg_get_type(ite_cond_cannot_succeed, simple_code_warning).
det_msg_get_type(negated_goal_cannot_fail, simple_code_warning).
det_msg_get_type(negated_goal_cannot_succeed, simple_code_warning).
det_msg_get_type(goal_cannot_succeed, simple_code_warning).
det_msg_get_type(det_goal_has_no_outputs, simple_code_warning).
    % XXX warn_obsolete isn't really a simple code warning.
    % We should add a separate warning type for this.
det_msg_get_type(warn_obsolete(_), simple_code_warning).
det_msg_get_type(warn_infinite_recursion, simple_code_warning).
det_msg_get_type(duplicate_call(_, _), call_warning).
det_msg_get_type(unknown_format_string(_, _), format_unknown).
det_msg_get_type(unknown_format_values(_, _), format_unknown).
det_msg_get_type(bad_format(_, _, _), format_known_bad).
det_msg_get_type(cc_unify_can_fail(_, _, _, _, _), det_error).
det_msg_get_type(cc_unify_in_wrong_context(_, _, _, _, _, _), det_error).
det_msg_get_type(cc_pred_in_wrong_context(_, _, _, _, _, _), det_error).
det_msg_get_type(higher_order_cc_pred_in_wrong_context(_, _, _, _), det_error).
det_msg_get_type(error_in_lambda(_, _, _, _, _, _), det_error).
det_msg_get_type(par_conj_not_det(_, _, _, _, _), det_error).
det_msg_get_type(pragma_c_code_without_det_decl(_, _), det_error).
det_msg_get_type(has_io_state_but_not_det(_, _), det_error).
det_msg_get_type(will_not_throw_with_erroneous(_, _), det_error).
det_msg_get_type(export_model_non_proc(_, _, _), det_error).
det_msg_get_type(nested_promise_eqv_solution_sets(_), simple_code_warning).
det_msg_get_type(arbitrary_without_promise, det_error).
det_msg_get_type(arbitrary_promise_overlap(_, _, _), det_error).
det_msg_get_type(promise_solutions_missing_vars(_, _, _), det_error).
det_msg_get_type(promise_solutions_extra_vars(_, _, _), det_error).

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
det_msg_is_any_mode_msg(warn_obsolete(_), all_modes).
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

:- pred det_report_msg(det_msg::in, prog_context::in, module_info::in,
    io::di, io::uo) is det.

det_report_msg(multidet_disj(DisjunctContexts), Context, _, !IO) :-
    Pieces = [words("Warning: the disjunction with arms on lines"),
        words(det_report_context_lines(DisjunctContexts)),
        words("has no outputs, but can succeed more than once.")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(det_disj(DisjunctContexts), Context, _, !IO) :-
    Pieces = [words("Warning: the disjunction with arms on lines"),
        words(det_report_context_lines(DisjunctContexts)),
        words("will succeed exactly once.")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(semidet_disj(DisjunctContexts), Context, _, !IO) :-
    Pieces = [words("Warning: the disjunction with arms on lines"),
        words(det_report_context_lines(DisjunctContexts)),
        words("is semidet, yet it has an output.")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(zero_soln_disj(DisjunctContexts), Context, _, !IO) :-
    Pieces = [words("Warning: the disjunction with arms on lines"),
        words(det_report_context_lines(DisjunctContexts)),
        words("cannot succeed.")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(zero_soln_disjunct, Context, _, !IO) :-
    Pieces = [words("Warning: this disjunct"),
        words("will never have any solutions.")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(ite_cond_cannot_fail, Context, _, !IO) :-
    Pieces = [words("Warning: the condition of this if-then-else"),
        words("cannot fail.")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(ite_cond_cannot_succeed, Context, _, !IO) :-
    Pieces = [words("Warning: the condition of this if-then-else"),
        words("cannot succeed.")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(negated_goal_cannot_fail, Context, _, !IO) :-
    Pieces = [words("Warning: the negated goal cannot fail.")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(negated_goal_cannot_succeed, Context, _, !IO) :-
    Pieces = [words("Warning: the negated goal cannot succeed.")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(goal_cannot_succeed, Context, _, !IO) :-
    Pieces0 = [words("Warning: this goal cannot succeed.")],
    globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        Pieces1 = [words("The compiler will optimize away this goal,"),
            words("replacing it with `fail'."),
            words("To disable this optimization, use "),
            words("the `--fully-strict' option.")],
        Pieces = Pieces0 ++ Pieces1
    ;
        VerboseErrors = no,
        globals.io_set_extra_error_info(yes, !IO),
        Pieces = Pieces0
    ),
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(det_goal_has_no_outputs, Context, _, !IO) :-
    Pieces0 = [words("Warning: det goal has no outputs.")],
    globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        Pieces1 = [words("The compiler will optimize away this goal,"),
            words("replacing it with `true'."),
            words("To disable this optimization, use "),
            words("the `--fully-strict' option.")],
        Pieces = Pieces0 ++ Pieces1
    ;
        VerboseErrors = no,
        globals.io_set_extra_error_info(yes, !IO),
        Pieces = Pieces0
    ),
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(warn_obsolete(PredId), Context, ModuleInfo, !IO) :-
    PredPieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
        PredId),
    Pieces = [words("Warning: call to obsolete")] ++ PredPieces
        ++ [suffix(".")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(warn_infinite_recursion, Context, _ModuleInfo, !IO) :-
    % it would be better if we supplied more information than just
    % the line number, e.g. we should print the name of the containing
    % predicate.
    Pieces0 = [words("Warning: recursive call will lead"),
        words("to infinite recursion.")],
    globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        Pieces1 = [words("If this recursive call is executed,"),
            words("the procedure will call itself"),
            words("with exactly the same input arguments,"),
            words("leading to infinite recursion.")],
        Pieces = Pieces0 ++ Pieces1
    ;
        VerboseErrors = no,
        globals.io_set_extra_error_info(yes, !IO),
        Pieces = Pieces0
    ),
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(duplicate_call(SeenCall, PrevContext), Context, ModuleInfo,
        !IO) :-
    CallPieces = det_report_seen_call_id(ModuleInfo, SeenCall),
    CurPieces = [words("Warning: redundant") | CallPieces] ++ [suffix(".")],
    PrevPieces = [words("Here is the previous") | CallPieces] ++ [suffix(".")],
    write_error_pieces(Context, 0, CurPieces, !IO),
    write_error_pieces(PrevContext, 0, PrevPieces, !IO).
det_report_msg(unknown_format_string(SymName, Arity), Context, _, !IO) :-
    Pieces = [words("Unknown format string in call to"),
        sym_name_and_arity(SymName / Arity), suffix(".")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(unknown_format_values(SymName, Arity), Context, _, !IO) :-
    Pieces = [words("Unknown format values in call to"),
        sym_name_and_arity(SymName / Arity), suffix(".")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(bad_format(SymName, Arity, Msg), Context, _, !IO) :-
    Pieces = [words("Mismatched format and values in call to"),
        sym_name_and_arity(SymName / Arity), suffix(":"), nl, words(Msg)],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(cc_unify_can_fail(_GoalInfo, Var, Type, VarSet, GoalContext),
        Context, _ModuleInfo, !IO) :-
    (
        GoalContext = switch,
        VarStr = mercury_var_to_string(Var, VarSet, no),
        Pieces0 = [words("In switch on variable `" ++ VarStr ++ "':"), nl]
    ;
        GoalContext = unify(UnifyContext),
        hlds_out.unify_context_to_pieces(UnifyContext, [], Pieces0)
    ),
    ( type_to_ctor_and_args(Type, TypeCtor, _TypeArgs) ->
        TypeCtorStr = hlds_out.type_ctor_to_string(TypeCtor)
    ;
        unexpected(this_file, "det_report_msg: type_to_ctor_and_args failed")
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
        words("is not guaranteed to succeed.")],
    Pieces = Pieces0 ++ Pieces1,
    write_error_pieces(Context, 0, Pieces, !IO),
    globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
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
        write_error_pieces_not_first_line(Context, 0, VerbosePieces, !IO)
    ;
        VerboseErrors = no,
        globals.io_set_extra_error_info(yes, !IO)
    ).
det_report_msg(cc_unify_in_wrong_context(_GoalInfo, Var, Type, VarSet,
        GoalContext, FailingContexts), Context, ModuleInfo, !IO) :-
    (
        GoalContext = switch,
        VarStr = mercury_var_to_string(Var, VarSet, no),
        Pieces0 = [words("In switch on variable `" ++ VarStr ++ "':"), nl]
    ;
        GoalContext = unify(UnifyContext),
        hlds_out.unify_context_to_pieces(yes, _, UnifyContext, [], Pieces0)
    ),
    ( type_to_ctor_and_args(Type, TypeCtor, _TypeArgs) ->
        TypeCtorStr = hlds_out.type_ctor_to_string(TypeCtor)
    ;
        unexpected(this_file, "det_report_msg: type_to_ctor_and_args failed")
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
    FirstPieces = Pieces0 ++ Pieces1,
    FirstSpec = error_msg_spec(yes, Context, 0, FirstPieces),
    LaterSpecs = failing_contexts_description(ModuleInfo, VarSet,
        FailingContexts),
    globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
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
        VerboseSpec = error_msg_spec(no, Context, 0, VerbosePieces),
        write_error_specs([FirstSpec, VerboseSpec | LaterSpecs], !IO)
    ;
        VerboseErrors = no,
        globals.io_set_extra_error_info(yes, !IO),
        write_error_specs([FirstSpec | LaterSpecs], !IO)
    ).
det_report_msg(cc_pred_in_wrong_context(_GoalInfo, Detism, PredId,
        _ModeId, VarSet, FailingContexts), Context, ModuleInfo, !IO) :-
    PredPieces = describe_one_pred_name(ModuleInfo,
        should_not_module_qualify, PredId),
    DetStr = mercury_det_to_string(Detism),
    FirstPieces = [words("Error: call to")] ++ PredPieces ++
        [words("with determinism `" ++ DetStr ++ "'"),
        words("occurs in a context which requires all solutions."), nl],
    FirstSpec = error_msg_spec(yes, Context, 0, FirstPieces),
    LaterSpecs = failing_contexts_description(ModuleInfo, VarSet,
        FailingContexts),
    write_error_specs([FirstSpec | LaterSpecs], !IO).
det_report_msg(higher_order_cc_pred_in_wrong_context(_GoalInfo, Detism,
        VarSet, FailingContexts), Context, ModuleInfo, !IO) :-
    DetStr = mercury_det_to_string(Detism),
    FirstPieces = [words("Error: higher-order call to predicate with"),
        words("determinism `" ++ DetStr ++ "'"),
        words("occurs in a context which requires all solutions."), nl],
    FirstSpec = error_msg_spec(yes, Context, 0, FirstPieces),
    LaterSpecs = failing_contexts_description(ModuleInfo, VarSet,
        FailingContexts),
    write_error_specs([FirstSpec | LaterSpecs], !IO).
det_report_msg(error_in_lambda(DeclaredDetism, InferredDetism,
        Goal, _GoalInfo, PredId, ProcId), Context, ModuleInfo, !IO) :-
    PredPieces = describe_one_proc_name_mode(ModuleInfo,
        should_not_module_qualify, proc(PredId, ProcId)),
    Pieces =
        [words("In")] ++ PredPieces ++ [suffix(":"), nl,
        words("Determinism error in lambda expression."), nl,
        words("Declared `" ++ determinism_to_string(DeclaredDetism)
            ++ "', inferred `" ++ determinism_to_string(InferredDetism)
            ++ "'.")],
    ReportSpec = error_msg_spec(no, Context, 0, Pieces),
    globals.io_get_globals(Globals, !IO),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    det_info_init(ModuleInfo, VarTypes, PredId, ProcId, Globals, DetInfo),
    det_diagnose_goal(Goal, DeclaredDetism, [], DetInfo, _,
        [ReportSpec], Specs),
    write_error_specs(Specs, !IO).
det_report_msg(par_conj_not_det(InferredDetism, PredId, ProcId,
        _GoalInfo, Goals), Context, ModuleInfo, !IO) :-
    determinism_components(InferredDetism, CanFail, MaxSoln),
    ( CanFail \= cannot_fail ->
        First = "Error: parallel conjunct may fail."
    ; MaxSoln = at_most_many ->
        First = "Error: parallel conjunct may have multiple solutions."
    ;
        unexpected(this_file,
            "strange determinism error for parallel conjunction")
    ),
    Rest = "The current implementation supports only single-solution"
        ++ "non-failing parallel conjunctions.",
    ReportSpec = error_msg_spec(no, Context, 0, [words(First), words(Rest)]),
    globals.io_get_globals(Globals, !IO),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    det_info_init(ModuleInfo, VarTypes, PredId, ProcId, Globals, DetInfo),
    det_diagnose_conj(Goals, det, [], DetInfo, _, [ReportSpec], Specs),
    write_error_specs(Specs, !IO).
det_report_msg(pragma_c_code_without_det_decl(PredId, ProcId), Context,
        ModuleInfo, !IO) :-
    ProcPieces = describe_one_proc_name_mode(ModuleInfo,
        should_not_module_qualify, proc(PredId, ProcId)),
    Pieces = [words("In")] ++ ProcPieces ++ [suffix(":"), nl,
        words("error: `:- pragma c_code(...)' for a procedure"),
        words("without a determinism declaration.")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(has_io_state_but_not_det(PredId, ProcId), Context, ModuleInfo,
        !IO) :-
    ProcPieces = describe_one_proc_name_mode(ModuleInfo,
        should_not_module_qualify, proc(PredId, ProcId)),
    Pieces = [words("In")] ++ ProcPieces ++ [suffix(":"), nl,
        words("error: invalid determinism for a predicate"),
        words("with I/O state arguments.")],
    write_error_pieces(Context, 0, Pieces, !IO),
    globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        VerbosePieces = [words("Valid determinisms are "),
            words("det, cc_multi and erroneous.")],
        write_error_pieces_not_first_line(Context, 0, VerbosePieces, !IO)
    ;
        VerboseErrors = no,
        globals.io_set_extra_error_info(yes, !IO)
    ).
det_report_msg(will_not_throw_with_erroneous(PredId, ProcId), Context,
        ModuleInfo, !IO) :-
    ProcPieces = describe_one_proc_name_mode(ModuleInfo,
        should_not_module_qualify, proc(PredId, ProcId)),
    Pieces = ProcPieces ++
        [words("has determinism erroneous but also has"),
        words("foreign clauses that have a"),
        fixed("`will_not_throw_exception' attribute."),
        words("This attribute cannot be applied"),
        words("to erroneous procedures.")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(export_model_non_proc(_PredId, _ProcId, Detism), Context,
        _ModuleInfo, !IO) :-
    Pieces = [words("Error: "),
        fixed("`:- pragma export' declaration"),
        words("for a procedure that has a determinism of"),
        fixed(hlds_out.determinism_to_string(Detism) ++ ".")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(nested_promise_eqv_solution_sets(OuterContext), Context,
        _ModuleInfo, !IO) :-
    Pieces = [words("Error: "),
        words("`promise_equivalent_solution_sets' scope"),
        words("is nested inside another.")],
    write_error_pieces(Context, 0, Pieces, !IO),
    Pieces2 = [words("This is the outer "),
        words("`promise_equivalent_solution_sets' scope.")],
    write_error_pieces_not_first_line(OuterContext, 0, Pieces2, !IO).
det_report_msg(arbitrary_without_promise, Context, _ModuleInfo, !IO) :-
    Pieces = [words("Error: "),
        words("this `arbitrary' scope is not nested inside"),
        words("a `promise_equivalent_solution_sets' scope.")],
    write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(arbitrary_promise_overlap(PromiseContext, VarSet, OverlapVars),
        Context, _ModuleInfo, !IO) :-
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
        words("`promise_equivalent_solution_sets' scope it is nested inside"),
        words("overlap on"), words(VarStr)]
        ++ list_to_pieces(VarNames) ++ [suffix(".")],
    write_error_pieces(Context, 0, Pieces, !IO),
    Pieces2 = [words("This is the outer "),
        words("`promise_equivalent_solution_sets' scope.")],
    write_error_pieces_not_first_line(PromiseContext, 0, Pieces2, !IO).
det_report_msg(promise_solutions_missing_vars(Kind, VarSet, Vars), Context, _,
        !IO) :-
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
    error_util.write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(promise_solutions_extra_vars(Kind, VarSet, Vars), Context, _,
        !IO) :-
    VarNames = list.map(lookup_var_name_in_varset(VarSet),
        set.to_sorted_list(Vars)),
    KindStr = promise_solutions_kind_str(Kind),
    (
        VarNames = [],
        unexpected(this_file, "det_report_msg: " ++
            "promise_solutions_extra_vars empty")
    ;
        VarNames = [_],
        ListStr = "an extra variable:"
    ;
        VarNames = [_, _ | _],
        ListStr = "some extra variables:"
    ),
    Pieces = [words("Error: the"), words(add_quotes(KindStr)),
        words("goal lists"), words(ListStr)]
        ++ list_to_pieces(VarNames) ++ [suffix(".")],
    error_util.write_error_pieces(Context, 0, Pieces, !IO).

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

:- func failing_contexts_description(module_info::in, prog_varset::in,
    list(failing_context)::in) =
    (list(error_msg_spec)::out(known_error_msg_specs)) is det.

failing_contexts_description(ModuleInfo, VarSet, FailingContexts) =
    list.map(failing_context_description(ModuleInfo, VarSet),
        FailingContexts).

:- func failing_context_description(module_info::in, prog_varset::in,
    failing_context::in) = (error_msg_spec::out(known_error_msg_spec)) is det.

failing_context_description(ModuleInfo, VarSet, Context - FailingGoal)
        = Spec :-
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
    Spec = error_msg_spec(no, Context, 0, Pieces ++ [nl]).

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

:- pred inst_preserving_append(list(T)::in(list_skel(I)),
    list(T)::in(list_skel(I)), list(T)::out(list_skel(I))) is det.

inst_preserving_append([], L, L).
inst_preserving_append([H | T], B, [H | NT]) :-
    inst_preserving_append(T, B, NT).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "det_report.m".

%-----------------------------------------------------------------------------%
:- end_module det_report.
%-----------------------------------------------------------------------------%
