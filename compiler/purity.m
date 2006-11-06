%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File:     purity.m
% Authors:  scachte (Peter Schachte, main author and designer of purity system)
%           trd (modifications for impure functions)
% Purpose:  handle `impure' and `promise_pure' declarations;
%           finish off type checking.
%
% The main purpose of this module is check the consistency of the `impure' and
% `promise_pure' (etc.) declarations, and to thus report error messages if the
% program is not "purity-correct".  This includes treating procedures with
% different clauses for different modes as impure, unless promised pure.
%
% This module also calls post_typecheck.m to perform the final parts of
% type analysis, including resolution of predicate and function overloading
% (see the comments in that file).
%
% These actions cannot be done until after type inference is complete,
% so they need to be a separate "post-typecheck pass"; they are done
% here in combination with the purity-analysis pass for efficiency reasons.
%
% We also do elimination of double-negation in this pass.
% It needs to be done somewhere after quantification analysis and
% before mode analysis, and this is a convenient place to do it.
%
% This pass also converts calls to `private_builtin.unsafe_type_cast'
% into `generic_call(unsafe_cast, ...)' goals.
%
%-----------------------------------------------------------------------------%
%
% The aim of Mercury's purity system is to allow one to declare certain parts
% of one's program to be impure, thereby forbidding the compiler from making
% certain optimizations to that part of the code.  Since one can often
% implement a perfectly pure predicate or function in terms of impure
% predicates and functions, one is also allowed to promise to the compiler
% that a predicate *is* pure, despite calling impure predicates and
% functions.
%
% To keep purity/impurity consistent, it is required that every impure
% predicate/function be declared so.  A predicate is impure if:
%
%   1.  It's declared impure, or
%   2a. It's not promised pure, and
%   2b. It calls some impure predicates or functions.
%
% A predicate or function is declared impure by preceding the `pred' or
% `func' in its declaration with `impure'.  It is promised to be pure with a
%
%   :- pragma promise_pure(Name/Arity).
%
% directive.
%
% Calls to impure predicates may not be optimized away.  Neither may they be
% reodered relative to any other goals in a given conjunction; ie, an impure
% goal cleaves a conjunction into the stuff before it and the stuff after it.
% Both of these groups may be reordered separately, but no goal from either
% group may move into the other.  Similarly for disjunctions.
%
% Semipure goals are goals that are sensitive to the effects of impure goals.
% They may be reordered and optimized away just like pure goals, except that
% a semipure goal may behave differently after a call to an impure goal than
% before.  This means that semipure (as well as impure) predicates must not
% be tabled.  Further, duplicate semipure goals on different sides of an
% impure goal must not be optimized away.  In the current implementation, we
% simply do not optimize away duplicate semipure (or impure) goals at all.
%
% A predicate either has no purity declaration and so is assumed pure, or is
% declared semipure or impure, or is promised to be pure despite calling
% semipure or impure predicates.  This promise cannot be checked, so we must
% trust the programmer.
%
% See the language reference manual for more information on syntax and
% semantics.
%
% The current implementation now handles impure functions.
% They are limited to being used as part of an explicit unification
% with a purity indicator before the goal.
%
%   impure X = some_impure_func(Arg1, Arg2, ...)
%
% This eliminates any need to define some order of evaluation of nested
% impure functions.
%
% Of course it also eliminates the benefits of using functions to
% cut down on the number of variables introduced.  The main use of
% impure functions is to interface nicely with foreign language
% functions.
%
% Any non-variable arguments to the function are flattened into
% unification goals (see unravel_unifications in superhomogeneous.m)
% which are placed as pure goals before the function call itself.
%
% Wishlist:
%   It would be nice to use impure functions in DCG goals as well as
%   normal unifications.
%
%   It might be nice to allow
%       X = impure some_impure_fuc(Arg1, Arg2, ...)
%   syntax as well.  But there are advantages to having the
%   impure/semipure annotation in a regular position (on the left
%   hand side of a goal) too.  If this is implemented it should
%   probably be handled in prog_io, and turned into an impure
%   unify item.
%
%   It may also be nice to allow semipure function calls to occur
%   inline (since ordering is not an issue for them).
%
% To do:
%   Reconsider whether impure parallel conjuncts should be allowed.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.purity.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Purity check a whole module.  Also do the post-typecheck stuff described
    % above, and eliminate double negations and calls to
    % `private_builtin.unsafe_type_cast/2'.  The first argument specifies
    % whether there were any type errors (if so, we suppress some diagnostics
    % in post_typecheck.m because they are usually spurious).  The second
    % argument specifies whether post_typecheck.m detected any errors that
    % would cause problems for later passes (if so, we stop compilation after
    % this pass).
    %
:- pred puritycheck(bool::in, bool::out, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Rerun purity checking on a procedure after an optimization pass has
    % performed transformations which might affect the procedure's purity.
    % repuritycheck_proc makes sure that the goal_infos contain the correct
    % purity, and that the pred_info contains the promised_pure or
    % promised_semipure markers which might be needed if a promised pure
    % procedure was inlined into the procedure being checked.
    %
:- pred repuritycheck_proc(module_info::in, pred_proc_id::in, pred_info::in,
    pred_info::out) is det.

    % Give an error message for unifications marked impure/semipure
    % that are not function calls (e.g. impure X = 4).
    %
:- func impure_unification_expr_error(prog_context, purity) = error_spec.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.post_typecheck.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%
% Public Predicates
%

puritycheck(FoundTypeError, PostTypecheckError, !ModuleInfo, !Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, statistics, Statistics),
    globals.lookup_bool_option(Globals, verbose, Verbose),

    trace [io(!IO)] (
        maybe_write_string(Verbose, "% Purity-checking clauses...\n", !IO)
    ),
    finish_typecheck_and_check_preds_purity(FoundTypeError, PostTypecheckError,
        !ModuleInfo, !Specs),
    trace [io(!IO)] (
        maybe_report_stats(Statistics, !IO)
    ).

%-----------------------------------------------------------------------------%

    % Purity-check the code for all the predicates in a module.
    %
:- pred finish_typecheck_and_check_preds_purity(bool::in, bool::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

finish_typecheck_and_check_preds_purity(FoundTypeError, PostTypecheckError,
        !ModuleInfo, !Specs) :-
    module_info_predids(!.ModuleInfo, PredIds),

    % Only report error messages for unbound type variables if we didn't get
    % any type errors already; this avoids a lot of spurious diagnostics.
    ReportTypeErrors = bool.not(FoundTypeError),
    post_typecheck.finish_preds(PredIds, ReportTypeErrors, NumPostErrors,
        !ModuleInfo, !Specs),
    ( NumPostErrors > 0 ->
        PostTypecheckError = yes
    ;
        PostTypecheckError = no
    ),

    check_preds_purity(PredIds, !ModuleInfo, !Specs).

:- pred check_preds_purity(list(pred_id)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_preds_purity([], !ModuleInfo, !Specs).
check_preds_purity([PredId | PredIds], !ModuleInfo, !Specs) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    (
        ( pred_info_is_imported(PredInfo0)
        ; pred_info_is_pseudo_imported(PredInfo0)
        )
    ->
        PredInfo = PredInfo0
    ;
        trace [io(!IO)] (
            write_pred_progress_message("% Purity-checking ", PredId,
                !.ModuleInfo, !IO)
        ),
        puritycheck_pred(PredId, PredInfo0, PredInfo, !.ModuleInfo, !Specs),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ),

    % Finish processing of promise declarations.
    pred_info_get_goal_type(PredInfo, GoalType),
    ( GoalType = goal_type_promise(PromiseType) ->
        post_typecheck.finish_promise(PromiseType, PredId, !ModuleInfo, !Specs)
    ;
        true
    ),
    check_preds_purity(PredIds, !ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%
%
% Check purity of a single predicate.
%
% Purity checking is quite simple.  Since impurity /must/ be declared, we can
% perform a single pass checking that the actual purity of each predicate
% matches the declared (or implied) purity.  A predicate is just as pure as
% its least pure goal.  While we're doing this, we attach a `feature' to each
% goal that is not pure, including non-atomic goals, indicating its purity.
% This information must be maintained by later compilation passes, at least
% until after the last pass that may perform transformations that would not
% be correct for impure code.  As we check purity and attach impurity
% features, we also check that impure (semipure) atomic goals were marked in
% the source code as impure (semipure).  At this stage in the computation,
% this is indicated by already having the appropriate goal feature.  (During
% the translation from term to goal, calls have their purity attached to
% them, and in the translation from goal to hlds_goal, the attached purity is
% turned into the appropriate feature in the hlds_goal_info.)

:- pred puritycheck_pred(pred_id::in, pred_info::in, pred_info::out,
    module_info::in, list(error_spec)::in, list(error_spec)::out) is det.

puritycheck_pred(PredId, !PredInfo, ModuleInfo, !Specs) :-
    pred_info_get_purity(!.PredInfo, DeclPurity) ,
    pred_info_get_promised_purity(!.PredInfo, PromisedPurity),
    some [!ClausesInfo] (
        pred_info_clauses_info(!.PredInfo, !:ClausesInfo),
        clauses_info_clauses(Clauses0, !ClausesInfo),
        clauses_info_get_vartypes(!.ClausesInfo, VarTypes0),
        clauses_info_get_varset(!.ClausesInfo, VarSet0),
        RunPostTypecheck = yes,
        PurityInfo0 = purity_info(ModuleInfo, RunPostTypecheck,
            !.PredInfo, VarTypes0, VarSet0, [], dont_make_implicit_promises),
        compute_purity(Clauses0, Clauses, !.PredInfo, purity_pure, Purity,
            PurityInfo0, PurityInfo),
        PurityInfo = purity_info(_, _, !:PredInfo,
            VarTypes, VarSet, GoalSpecs, _),
        clauses_info_set_vartypes(VarTypes, !ClausesInfo),
        clauses_info_set_varset(VarSet, !ClausesInfo),
        clauses_info_set_clauses(Clauses, !ClausesInfo),
        pred_info_set_clauses_info(!.ClausesInfo, !PredInfo)
    ),
    WorstPurity = Purity,
    perform_pred_purity_checks(!.PredInfo, Purity, DeclPurity,
        PromisedPurity, PurityCheckResult),
    (
        PurityCheckResult = inconsistent_promise,
        Spec = error_inconsistent_promise(ModuleInfo, !.PredInfo, PredId,
            DeclPurity),
        PredSpecs = [Spec | GoalSpecs]
    ;
        PurityCheckResult = unnecessary_decl,
        Spec = warn_exaggerated_impurity_decl(ModuleInfo, !.PredInfo, PredId,
            DeclPurity, WorstPurity),
        PredSpecs = [Spec | GoalSpecs]
    ;
        PurityCheckResult = insufficient_decl,
        Spec = error_inferred_impure(ModuleInfo, !.PredInfo, PredId, Purity),
        PredSpecs = [Spec | GoalSpecs]
    ;
        PurityCheckResult = unnecessary_promise_pure,
        Spec = warn_unnecessary_promise_pure(ModuleInfo, !.PredInfo, PredId,
            PromisedPurity),
        PredSpecs = [Spec | GoalSpecs]
    ;
        PurityCheckResult = no_worries,
        PredSpecs = GoalSpecs
    ),
    !:Specs = PredSpecs ++ !.Specs.

repuritycheck_proc(ModuleInfo, proc(_PredId, ProcId), !PredInfo) :-
    pred_info_get_procedures(!.PredInfo, Procs0),
    map.lookup(Procs0, ProcId, ProcInfo0),
    proc_info_get_goal(ProcInfo0, Goal0),
    proc_info_get_vartypes(ProcInfo0, VarTypes0),
    proc_info_get_varset(ProcInfo0, VarSet0),
    RunPostTypeCheck = no,
    PurityInfo0 = purity_info(ModuleInfo, RunPostTypeCheck,
        !.PredInfo, VarTypes0, VarSet0, [], dont_make_implicit_promises),
    compute_goal_purity(Goal0, Goal, Bodypurity, _, PurityInfo0, PurityInfo),
    PurityInfo = purity_info(_, _, !:PredInfo, VarTypes, VarSet, _, _),
    proc_info_set_goal(Goal, ProcInfo0, ProcInfo1),
    proc_info_set_vartypes(VarTypes, ProcInfo1, ProcInfo2),
    proc_info_set_varset(VarSet, ProcInfo2, ProcInfo),
    map.det_update(Procs0, ProcId, ProcInfo, Procs),
    pred_info_set_procedures(Procs, !PredInfo),

    % A predicate should never become less pure after inlining, so update
    % any promises in the pred_info if the purity of the goal worsened
    % (for example if a promised pure predicate was inlined).

    pred_info_get_purity(!.PredInfo, OldPurity),
    pred_info_get_markers(!.PredInfo, Markers0),
    (
        less_pure(Bodypurity, OldPurity)
    ->
        (
            OldPurity = purity_pure,
            remove_marker(marker_promised_semipure, Markers0, Markers1),
            add_marker(marker_promised_pure, Markers1, Markers)
        ;
            OldPurity = purity_semipure,
            add_marker(marker_promised_semipure, Markers0, Markers)
        ;
            OldPurity = purity_impure,
            Markers = Markers0
        ),
        pred_info_set_markers(Markers, !PredInfo)
    ;
        less_pure(OldPurity, Bodypurity),
        [_] = pred_info_procids(!.PredInfo)
    ->
        % If there is only one procedure, update the purity in the pred_info
        % if the purity improved.
        %
        % XXX Storing the purity in the pred_info is the wrong thing to do,
        % because optimizations can make some procedures more pure than others.
        (
            Bodypurity = purity_pure,
            remove_marker(marker_is_impure, Markers0, Markers1),
            remove_marker(marker_is_semipure, Markers1, Markers)
        ;
            Bodypurity = purity_semipure,
            remove_marker(marker_is_impure, Markers0, Markers1),
            add_marker(marker_is_semipure, Markers1, Markers)
        ;
            Bodypurity = purity_impure,
            Markers = Markers0
        ),
        pred_info_set_markers(Markers, !PredInfo)
    ;
        true
    ).

    % Infer the purity of a single (non-foreign_proc) predicate.
    %
:- pred compute_purity(list(clause)::in, list(clause)::out,
    pred_info::in, purity::in, purity::out,
    purity_info::in, purity_info::out) is det.

compute_purity([], [], _, !Purity, !Info).
compute_purity([Clause0 | Clauses0], [Clause | Clauses], PredInfo, !Purity,
        !Info) :-
    Clause0 = clause(Ids, GoalExpr0 - GoalInfo0, Lang, Context),
    compute_expr_purity(GoalExpr0, GoalExpr, GoalInfo0, BodyPurity0, _, !Info),
    % If this clause doesn't apply to all modes of this procedure,
    % i.e. the procedure has different clauses for different modes,
    % then we must treat it as impure, unless the programmer has promised
    % that the clauses are semantically equivalent.
    %
    % The default impurity of foreign_proc procedures is handled when
    % processing the foreign_proc goal -- they are not counted as impure
    % here simply because they have different clauses for different modes.
    (
        (
            ProcIds = pred_info_procids(PredInfo),
            applies_to_all_modes(Clause0, ProcIds)
        ;
            pred_info_get_markers(PredInfo, Markers),
            check_marker(Markers, marker_promised_equivalent_clauses)
        ;
            pred_info_get_goal_type(PredInfo, GoalType),
            GoalType = goal_type_foreign
        )
    ->
        ClausePurity = purity_pure
    ;
        ClausePurity = purity_impure
    ),
    BodyPurity = worst_purity(BodyPurity0, ClausePurity),
    goal_info_set_purity(BodyPurity, GoalInfo0, GoalInfo),
    !:Purity = worst_purity(!.Purity, BodyPurity),
    Clause = clause(Ids, GoalExpr - GoalInfo, Lang, Context),
    compute_purity(Clauses0, Clauses, PredInfo, !Purity, !Info).

:- pred applies_to_all_modes(clause::in, list(proc_id)::in) is semidet.

applies_to_all_modes(clause(ClauseProcIds, _, _, _), ProcIds) :-
    (
        % An empty list here means that the clause applies to *all* procedures.
        ClauseProcIds = []
    ;
        % Otherwise the clause applies to the procids in the list.
        % Check if this is the same as the procids for this procedure.
        list.sort(ClauseProcIds, SortedIds),
        SortedIds = ProcIds
    ).

:- pred compute_expr_purity(hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, purity::out, contains_trace_goal::out,
    purity_info::in, purity_info::out) is det.

compute_expr_purity(conj(ConjType, Goals0), conj(ConjType, Goals), _,
        Purity, ContainsTrace, !Info) :-
    (
        ConjType = plain_conj,
        compute_goals_purity(Goals0, Goals, purity_pure, Purity,
            contains_no_trace_goal, ContainsTrace, !Info)
    ;
        ConjType = parallel_conj,
        compute_parallel_goals_purity(Goals0, Goals, purity_pure, Purity,
            contains_no_trace_goal, ContainsTrace, !Info)
    ).
compute_expr_purity(Goal0, Goal, GoalInfo, ActualPurity,
        contains_no_trace_goal, !Info) :-
    Goal0 = plain_call(PredId0, ProcId, Vars, BIState, UContext, Name0),
    RunPostTypecheck = !.Info ^ run_post_typecheck,
    PredInfo = !.Info ^ pred_info,
    ModuleInfo = !.Info ^ module_info,
    (
        RunPostTypecheck = yes,
        finally_resolve_pred_overloading(Vars, PredInfo, ModuleInfo,
            Name0, Name, PredId0, PredId),
        (
            % Convert any calls to private_builtin.unsafe_type_cast
            % into unsafe_type_cast generic calls.
            Name = qualified(mercury_private_builtin_module,
                "unsafe_type_cast"),
            Vars = [InputArg, OutputArg]
        ->
            Goal = generic_call(cast(unsafe_type_cast), [InputArg, OutputArg],
                [in_mode, out_mode], detism_det)
        ;
            Goal = plain_call(PredId, ProcId, Vars, BIState, UContext, Name)
        )
    ;
        RunPostTypecheck = no,
        PredId = PredId0,
        Goal = Goal0
    ),
    goal_info_get_purity(GoalInfo, DeclaredPurity),
    goal_info_get_context(GoalInfo, CallContext),
    perform_goal_purity_checks(CallContext, PredId,
        DeclaredPurity, ActualPurity, !Info).
compute_expr_purity(generic_call(GenericCall0, Args, Modes0, Det),
        GoalExpr, _GoalInfo, Purity, contains_no_trace_goal, !Info) :-
    (
        GenericCall0 = higher_order(_, Purity, _, _),
        GoalExpr = generic_call(GenericCall0, Args, Modes0, Det)
    ;
        GenericCall0 = class_method(_, _, _, _),
        Purity = purity_pure, % XXX this is wrong!
        GoalExpr = generic_call(GenericCall0, Args, Modes0, Det)
    ;
        ( GenericCall0 = cast(_)
        ; GenericCall0 = event_call(_)
        ),
        Purity = purity_pure,
        GoalExpr = generic_call(GenericCall0, Args, Modes0, Det)
    ).
compute_expr_purity(switch(Var, Canfail, Cases0),
        switch(Var, Canfail, Cases), _, Purity, ContainsTrace, !Info) :-
    compute_cases_purity(Cases0, Cases, purity_pure, Purity,
        contains_no_trace_goal, ContainsTrace, !Info).
compute_expr_purity(Unif0, GoalExpr, GoalInfo, ActualPurity,
        ContainsTrace, !Info) :-
    Unif0 = unify(Var, RHS0, Mode, Unification, UnifyContext),
    (
        RHS0 = rhs_lambda_goal(LambdaPurity, F, EvalMethod, H, Vars,
            Modes, K, Goal0 - Info0),
        compute_expr_purity(Goal0, Goal, Info0, GoalPurity, _, !Info),
        RHS = rhs_lambda_goal(LambdaPurity, F, EvalMethod, H, Vars,
            Modes, K, Goal - Info0),
        check_closure_purity(GoalInfo, LambdaPurity, GoalPurity, !Info),
        GoalExpr = unify(Var, RHS, Mode, Unification, UnifyContext),
        % the unification itself is always pure,
        % even if the lambda expression body is impure
        goal_info_get_purity(GoalInfo, DeclaredPurity),
        ( DeclaredPurity \= purity_pure ->
            goal_info_get_context(GoalInfo, Context),
            Spec = impure_unification_expr_error(Context, DeclaredPurity),
            purity_info_add_message(Spec, !Info)
        ;
            true
        ),
        ActualPurity = purity_pure,
        ContainsTrace = contains_no_trace_goal
    ;
        RHS0 = rhs_functor(ConsId, _, Args),
        RunPostTypecheck = !.Info ^ run_post_typecheck,
        (
            RunPostTypecheck = yes,
            ModuleInfo = !.Info ^ module_info,
            PredInfo0 = !.Info ^ pred_info,
            VarTypes0 = !.Info ^ vartypes,
            VarSet0 = !.Info ^ varset,
            post_typecheck.resolve_unify_functor(Var, ConsId, Args, Mode,
                Unification, UnifyContext, GoalInfo, ModuleInfo,
                PredInfo0, PredInfo, VarTypes0, VarTypes, VarSet0, VarSet,
                Goal1),
            !:Info = !.Info ^ vartypes := VarTypes,
            !:Info = !.Info ^ varset := VarSet,
            !:Info = !.Info ^ pred_info := PredInfo
        ;
            RunPostTypecheck = no,
            Goal1 = Unif0 - GoalInfo
        ),
        ( Goal1 = unify(_, _, _, _, _) - _ ->
            check_higher_order_purity(GoalInfo, ConsId, Var, Args,
                ActualPurity, !Info),
            ContainsTrace = contains_no_trace_goal,
            Goal = Goal1
        ;
            compute_goal_purity(Goal1, Goal, ActualPurity, ContainsTrace,
                !Info)
        ),
        Goal = GoalExpr - _
    ;
        RHS0 = rhs_var(_),
        GoalExpr = Unif0,
        ActualPurity = purity_pure,
        ContainsTrace = contains_no_trace_goal
    ).
compute_expr_purity(disj(Goals0), disj(Goals), _, Purity, ContainsTrace,
        !Info) :-
    compute_goals_purity(Goals0, Goals, purity_pure, Purity,
        contains_no_trace_goal, ContainsTrace, !Info).
compute_expr_purity(negation(Goal0), NotGoal, GoalInfo0, Purity, ContainsTrace,
        !Info) :-
    % Eliminate double negation.
    negate_goal(Goal0, GoalInfo0, NotGoal0),
    ( NotGoal0 = negation(Goal1) - _GoalInfo1 ->
        compute_goal_purity(Goal1, Goal, Purity, ContainsTrace, !Info),
        NotGoal = negation(Goal)
    ;
        compute_goal_purity(NotGoal0, NotGoal1, Purity, ContainsTrace, !Info),
        NotGoal1 = NotGoal - _
    ).
compute_expr_purity(scope(Reason, Goal0), scope(Reason, Goal),
        _, Purity, ContainsTrace, !Info) :-
    (
        Reason = exist_quant(_),
        compute_goal_purity(Goal0, Goal, Purity, ContainsTrace, !Info)
    ;
        Reason = promise_purity(Implicit, PromisedPurity),
        ImplicitPurity0 = !.Info ^ implicit_purity,
        (
            Implicit = make_implicit_promises,
            !:Info = !.Info ^ implicit_purity := Implicit
        ;
            Implicit = dont_make_implicit_promises
        ),
        compute_goal_purity(Goal0, Goal, _, ContainsTrace, !Info),
        !:Info = !.Info ^ implicit_purity := ImplicitPurity0,
        Purity = PromisedPurity
    ;
        Reason = promise_solutions(_, _),
        compute_goal_purity(Goal0, Goal, Purity, ContainsTrace, !Info)
    ;
        Reason = commit(_),
        compute_goal_purity(Goal0, Goal, Purity, ContainsTrace, !Info)
    ;
        Reason = barrier(_),
        compute_goal_purity(Goal0, Goal, Purity, ContainsTrace, !Info)
    ;
        Reason = from_ground_term(_),
        compute_goal_purity(Goal0, Goal, Purity, ContainsTrace, !Info)
    ;
        Reason = trace_goal(_, _, _, _),
        compute_goal_purity(Goal0, Goal, _SubPurity, _, !Info),
        Purity = purity_pure,
        ContainsTrace = contains_trace_goal
    ).
compute_expr_purity(if_then_else(Vars, Cond0, Then0, Else0),
        if_then_else(Vars, Cond, Then, Else), _, Purity, ContainsTrace,
        !Info) :-
    compute_goal_purity(Cond0, Cond, Purity1, ContainsTrace1, !Info),
    compute_goal_purity(Then0, Then, Purity2, ContainsTrace2, !Info),
    compute_goal_purity(Else0, Else, Purity3, ContainsTrace3, !Info),
    worst_purity(Purity1, Purity2) = Purity12,
    worst_purity(Purity12, Purity3) = Purity,
    (
        ( ContainsTrace1 = contains_trace_goal
        ; ContainsTrace2 = contains_trace_goal
        ; ContainsTrace3 = contains_trace_goal
        )
    ->
        ContainsTrace = contains_trace_goal
    ;
        ContainsTrace = contains_no_trace_goal
    ).
compute_expr_purity(ForeignProc0, ForeignProc, _, Purity,
        contains_no_trace_goal, !Info) :-
    ForeignProc0 = call_foreign_proc(_, _, _, _, _, _, _),
    Attributes = ForeignProc0 ^ foreign_attr,
    PredId = ForeignProc0 ^ foreign_pred_id,
    ModuleInfo = !.Info ^ module_info,
    LegacyBehaviour = get_legacy_purity_behaviour(Attributes),
    (
        LegacyBehaviour = yes,
        % Get the purity from the declaration, and set it here so that
        % it is correct for later use.

        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_purity(PredInfo, Purity),
        set_purity(Purity, Attributes, NewAttributes),
        ForeignProc = ForeignProc0 ^ foreign_attr := NewAttributes
    ;
        LegacyBehaviour = no,
        ForeignProc = ForeignProc0,
        Purity = get_purity(Attributes)
    ).
compute_expr_purity(shorthand(_), _, _, _, _, !Info) :-
    % These should have been expanded out by now.
    unexpected(this_file, "compute_expr_purity: unexpected shorthand").

:- pred check_higher_order_purity(hlds_goal_info::in, cons_id::in,
    prog_var::in, list(prog_var)::in, purity::out,
    purity_info::in, purity_info::out) is det.

check_higher_order_purity(GoalInfo, ConsId, Var, Args, ActualPurity, !Info) :-
    % Check that the purity of the ConsId matches the purity of the
    % variable's type.
    VarTypes = !.Info ^ vartypes,
    map.lookup(VarTypes, Var, TypeOfVar),
    (
        ConsId = cons(PName, _),
        type_is_higher_order_details(TypeOfVar, TypePurity, PredOrFunc,
            _EvalMethod, VarArgTypes)
    ->
        PredInfo = !.Info ^ pred_info,
        pred_info_get_typevarset(PredInfo, TVarSet),
        map.apply_to_list(Args, VarTypes, ArgTypes0),
        list.append(ArgTypes0, VarArgTypes, PredArgTypes),
        ModuleInfo = !.Info ^ module_info,
        CallerPredInfo = !.Info ^ pred_info,
        pred_info_get_markers(CallerPredInfo, CallerMarkers),
        (
            get_pred_id(calls_are_fully_qualified(CallerMarkers), PName,
                PredOrFunc, TVarSet, PredArgTypes, ModuleInfo, CalleePredId)
        ->
            module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
            pred_info_get_purity(CalleePredInfo, CalleePurity),
            check_closure_purity(GoalInfo, TypePurity, CalleePurity, !Info)
        ;
            % If we can't find the type of the function, it's because
            % typecheck couldn't give it one. Typechecking gives an error
            % in this case, we just keep silent.
            true
        )
    ;
        true
    ),

    % The unification itself is always pure,
    % even if it is a unification with an impure higher-order term.
    ActualPurity = purity_pure,

    % Check for a bogus purity annotation on the unification.
    goal_info_get_purity(GoalInfo, DeclaredPurity),
    (
        DeclaredPurity \= purity_pure,
        !.Info ^ implicit_purity = dont_make_implicit_promises
    ->
        goal_info_get_context(GoalInfo, Context),
        Spec = impure_unification_expr_error(Context, DeclaredPurity),
        purity_info_add_message(Spec, !Info)
    ;
        true
    ).

    % The possible results of a purity check.
:- type purity_check_result
    --->    no_worries                  % All is well.
    ;       insufficient_decl           % Purity decl is less than
                                        % required.
    ;       inconsistent_promise        % Promise is given
                                        % but decl is impure.
    ;       unnecessary_promise_pure    % Purity promise is given
                                        % but not required.
    ;       unnecessary_decl.           % Purity decl is more than is
                                        % required.

    % Peform purity checking of the actual and declared purity,
    % and check that promises are consistent.
    %
    % ActualPurity: The inferred purity of the pred
    % DeclaredPurity: The declared purity of the pred
    % InPragmaCCode: Is this a pragma c code?
    % Promised: Did we promise this pred as pure?
    %
:- pred perform_pred_purity_checks(pred_info::in, purity::in, purity::in,
    purity::in, purity_check_result::out) is det.

perform_pred_purity_checks(PredInfo, ActualPurity, DeclaredPurity,
        PromisedPurity, PurityCheckResult) :-
    (
        % The declared purity must match any promises.
        % (A promise of impure means no promise was made).
        PromisedPurity \= purity_impure,
        DeclaredPurity \= PromisedPurity
    ->
        PurityCheckResult = inconsistent_promise
    ;
        % You shouldn't promise pure unnecessarily. It's OK in the case
        % of foreign_procs though. There is also no point in warning about
        % compiler-generated predicates.
        PromisedPurity \= purity_impure,
        ActualPurity = PromisedPurity,
        not pred_info_pragma_goal_type(PredInfo),
        pred_info_get_origin(PredInfo, Origin),
        not (
            Origin = origin_transformed(_, _, _)
        ;
            Origin = origin_created(_)
        )
    ->
        PurityCheckResult = unnecessary_promise_pure
    ;
        % The purity should match the declaration.
        ActualPurity = DeclaredPurity
    ->
        PurityCheckResult = no_worries
    ;
        less_pure(ActualPurity, DeclaredPurity)
    ->
        ( PromisedPurity = purity_impure ->
            PurityCheckResult = insufficient_decl
        ;
            PurityCheckResult = no_worries
        )
    ;
        % We don't warn about exaggerated impurity decls in class methods
        % or instance methods --- it just means that the predicate provided
        % as an implementation was more pure than necessary.
        %
        % We don't warn about exaggerated impurity decls in c_code -- this is
        % just because we assume they are pure, but you can declare them
        % to be impure.
        %
        % We don't warn about exaggerated impurity declarations for "stub"
        % procedures, i.e. procedures which originally had no clauses.

        pred_info_get_markers(PredInfo, Markers),
        pred_info_get_goal_type(PredInfo, GoalType),
        (
            GoalType = goal_type_foreign
        ;
            GoalType = goal_type_clause_and_foreign
        ;
            check_marker(Markers, marker_class_method)
        ;
            check_marker(Markers, marker_class_instance_method)
        ;
            check_marker(Markers, marker_stub)
        )
    ->
        PurityCheckResult = no_worries
    ;
        PurityCheckResult = unnecessary_decl
    ).

    % Peform purity checking of the actual and declared purity,
    % and check that promises are consistent.
    %
    % ActualPurity: The inferred purity of the goal
    % DeclaredPurity: The declared purity of the goal
    %
:- pred perform_goal_purity_checks(prog_context::in, pred_id::in, purity::in,
    purity::out, purity_info::in, purity_info::out) is det.

perform_goal_purity_checks(Context, PredId, DeclaredPurity, ActualPurity,
        !Info) :-
    ModuleInfo = !.Info ^ module_info,
    PredInfo = !.Info ^ pred_info,
    ImplicitPurity = !.Info ^ implicit_purity,
    module_info_pred_info(ModuleInfo, PredId, CalleePredInfo),
    pred_info_get_purity(CalleePredInfo, ActualPurity),
    (
        % If implicit_purity = make_implicit_promises then
        % we don't report purity errors or warnings.
        ImplicitPurity = make_implicit_promises
    ->
        true
    ;
        % The purity of the callee should match the
        % purity declared at the call.
        ActualPurity = DeclaredPurity
    ->
        true
    ;
        % Don't require purity annotations on calls in
        % compiler-generated code.
        is_unify_or_compare_pred(PredInfo)
    ->
        true
    ;
        less_pure(ActualPurity, DeclaredPurity)
    ->
        Spec = error_missing_body_impurity_decl(ModuleInfo, PredId, Context),
        purity_info_add_message(Spec, !Info)
    ;
        % We don't warn about exaggerated impurity decls in class methods
        % or instance methods --- it just means that the predicate provided
        % as an implementation was more pure than necessary.

        pred_info_get_markers(PredInfo, Markers),
        (
            check_marker(Markers, marker_class_method)
        ;
            check_marker(Markers, marker_class_instance_method)
        )
    ->
        true
    ;
        Spec = warn_unnecessary_body_impurity_decl(ModuleInfo, PredId, Context,
            DeclaredPurity),
        purity_info_add_message(Spec, !Info)
    ).

:- pred compute_goal_purity(hlds_goal::in, hlds_goal::out, purity::out,
    contains_trace_goal::out, purity_info::in, purity_info::out) is det.

compute_goal_purity(Goal0 - GoalInfo0, Goal - GoalInfo, Purity, ContainsTrace,
        !Info) :-
    compute_expr_purity(Goal0, Goal, GoalInfo0, Purity, ContainsTrace, !Info),
    goal_info_set_purity(Purity, GoalInfo0, GoalInfo1),
    (
        ContainsTrace = contains_trace_goal,
        goal_info_add_feature(feature_contains_trace, GoalInfo1, GoalInfo)
    ;
        ContainsTrace = contains_no_trace_goal,
        goal_info_remove_feature(feature_contains_trace, GoalInfo1, GoalInfo)
    ).

    % Compute the purity of a list of hlds_goals.  Since the purity of a
    % disjunction is computed the same way as the purity of a conjunction,
    % we use the same code for both
    %
:- pred compute_goals_purity(list(hlds_goal)::in, list(hlds_goal)::out,
    purity::in, purity::out, contains_trace_goal::in, contains_trace_goal::out,
    purity_info::in, purity_info::out) is det.

compute_goals_purity([], [], !Purity, !ContainsTrace, !Info).
compute_goals_purity([Goal0 | Goals0], [Goal | Goals], !Purity, !ContainsTrace,
        !Info) :-
    compute_goal_purity(Goal0, Goal, GoalPurity, GoalContainsTrace, !Info),
    !:Purity = worst_purity(GoalPurity, !.Purity),
    !:ContainsTrace = worst_contains_trace(GoalContainsTrace, !.ContainsTrace),
    compute_goals_purity(Goals0, Goals, !Purity, !ContainsTrace, !Info).

:- pred compute_cases_purity(list(case)::in, list(case)::out,
    purity::in, purity::out, contains_trace_goal::in, contains_trace_goal::out,
    purity_info::in, purity_info::out) is det.

compute_cases_purity([], [], !Purity, !ContainsTrace, !Info).
compute_cases_purity([case(Ctor, Goal0) | Cases0], [case(Ctor, Goal) | Cases],
        !Purity, !ContainsTrace, !Info) :-
    compute_goal_purity(Goal0, Goal, GoalPurity, GoalContainsTrace, !Info),
    !:Purity = worst_purity(GoalPurity, !.Purity),
    !:ContainsTrace = worst_contains_trace(GoalContainsTrace, !.ContainsTrace),
    compute_cases_purity(Cases0, Cases, !Purity, !ContainsTrace, !Info).

:- pred compute_parallel_goals_purity(list(hlds_goal)::in,
    list(hlds_goal)::out, purity::in, purity::out, contains_trace_goal::in,
    contains_trace_goal::out, purity_info::in, purity_info::out) is det.

compute_parallel_goals_purity([], [], !Purity, !ContainsTrace, !Info).
compute_parallel_goals_purity([Goal0 | Goals0], [Goal | Goals], !Purity,
        !ContainsTrace, !Info) :-
    compute_goal_purity(Goal0, Goal, GoalPurity, GoalContainsTrace, !Info),
    (
        ( GoalPurity = purity_pure
        ; GoalPurity = purity_semipure
        )
    ;
        GoalPurity = purity_impure,
        Goal0 = _ - GoalInfo0,
        goal_info_get_context(GoalInfo0, Context),
        Spec = impure_parallel_conjunct_error(Context, GoalPurity),
        purity_info_add_message(Spec, !Info)
    ),
    !:Purity = worst_purity(GoalPurity, !.Purity),
    !:ContainsTrace = worst_contains_trace(GoalContainsTrace, !.ContainsTrace),
    compute_parallel_goals_purity(Goals0, Goals, !Purity, !ContainsTrace,
        !Info).

%-----------------------------------------------------------------------------%

:- pred check_closure_purity(hlds_goal_info::in, purity::in, purity::in,
    purity_info::in, purity_info::out) is det.

check_closure_purity(GoalInfo, DeclaredPurity, ActualPurity, !Info) :-
    ( ActualPurity `less_pure` DeclaredPurity ->
        goal_info_get_context(GoalInfo, Context),
        Spec = report_error_closure_purity(Context,
            DeclaredPurity, ActualPurity),
        purity_info_add_message(Spec, !Info)
    ;
        % We don't bother to warn if the DeclaredPurity is less pure than the
        % ActualPurity; that would lead to too many spurious warnings.
        true
    ).

%-----------------------------------------------------------------------------%

:- func pred_context(module_info, pred_info, pred_id) = list(format_component).

pred_context(ModuleInfo, _PredInfo, PredId) = Pieces :-
    PredPieces = describe_one_pred_name(ModuleInfo, should_not_module_qualify,
        PredId),
    Pieces = [words("In")] ++ PredPieces ++ [suffix(":"), nl].

:- func error_inconsistent_promise(module_info, pred_info, pred_id, purity)
    = error_spec.

error_inconsistent_promise(ModuleInfo, PredInfo, PredId, Purity) = Spec :-
    pred_info_context(PredInfo, Context),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
    purity_name(Purity, PurityName),
    PredContextPieces = pred_context(ModuleInfo, PredInfo, PredId),
    MainPieces = PredContextPieces ++
        [words("error: declared"), fixed(PurityName),
        words("but promised pure.")],
    VerbosePieces = [words("A pure"), fixed(PredOrFuncStr),
        words("that invokes impure or semipure code"),
        words("should be promised pure and should have"),
        words("no impurity declaration.")],
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_purity_check, [Msg]).

:- func warn_exaggerated_impurity_decl(module_info, pred_info, pred_id,
    purity, purity) = error_spec.

warn_exaggerated_impurity_decl(ModuleInfo, PredInfo, PredId,
        DeclPurity, ActualPurity) = Spec :-
    pred_info_context(PredInfo, Context),
    PredContextPieces = pred_context(ModuleInfo, PredInfo, PredId),
    purity_name(DeclPurity, DeclPurityName),
    purity_name(ActualPurity, ActualPurityName),
    Pieces = PredContextPieces ++
        [words("warning: declared"), fixed(DeclPurityName),
        words("but actually"), fixed(ActualPurityName ++ ".")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_warning, phase_purity_check, [Msg]).

:- func warn_unnecessary_promise_pure(module_info, pred_info, pred_id, purity)
    = error_spec.

warn_unnecessary_promise_pure(ModuleInfo, PredInfo, PredId, PromisedPurity)
        = Spec :-
    pred_info_context(PredInfo, Context),
    PredContextPieces = pred_context(ModuleInfo, PredInfo, PredId),
    (
        PromisedPurity = purity_pure,
        Pragma = "promise_pure",
        CodeStr = "impure or semipure"
    ;
        PromisedPurity = purity_semipure,
        Pragma = "promise_semipure",
        CodeStr = "impure"
    ;
        PromisedPurity = purity_impure,
        unexpected(this_file, "warn_unnecessary_promise_pure: promise_impure?")
    ),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    MainPieces = [words("warning: unnecessary"), quote(Pragma),
        words("pragma."), nl],
    VerbosePieces = [words("This"), p_or_f(PredOrFunc),
        words("does not invoke any"), fixed(CodeStr), words("code,"),
        words("so there is no need for a"), quote(Pragma), words("pragma."),
        nl],
    Msg = simple_msg(Context,
        [always(PredContextPieces), always(MainPieces),
            verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_warning, phase_purity_check, [Msg]).

:- func error_inferred_impure(module_info, pred_info, pred_id, purity)
    = error_spec.

error_inferred_impure(ModuleInfo, PredInfo, PredId, Purity) = Spec :-
    pred_info_context(PredInfo, Context),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
    PredContextPieces = pred_context(ModuleInfo, PredInfo, PredId),
    pred_info_get_purity(PredInfo, DeclaredPurity),
    purity_name(Purity, PurityName),
    purity_name(DeclaredPurity, DeclaredPurityName),

    Pieces1 = [words("purity error:"), fixed(PredOrFuncStr),
        words("is"), fixed(PurityName), suffix("."), nl],
    ( is_unify_or_compare_pred(PredInfo) ->
        Pieces2 = [words("It must be pure.")]
    ;
        Pieces2 = [words("It must be declared"), quote(PurityName),
            words("or promised"), fixed(DeclaredPurityName ++ "."), nl]
    ),
    Msg = simple_msg(Context,
        [always(PredContextPieces), always(Pieces1), always(Pieces2)]),
    Spec = error_spec(severity_error, phase_purity_check, [Msg]).

:- func error_missing_body_impurity_decl(module_info, pred_id, prog_context)
    = error_spec.

error_missing_body_impurity_decl(ModuleInfo, PredId, Context) = Spec :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_purity(PredInfo, Purity),
    purity_name(Purity, PurityName),
    PredPieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
        PredId),
    Pieces1 = [words("In call to "), fixed(PurityName)] ++
        PredPieces ++ [suffix(":"), nl],
    (
        PredOrFunc = predicate,
        Pieces2 = [words("purity error: call must be preceded by"),
            quote(PurityName), words("indicator."), nl]
    ;
        PredOrFunc = function,
        Pieces2 = [words("purity error: call must be in"),
            words("an explicit unification which is preceded by"),
            quote(PurityName), words("indicator."), nl]
    ),
    Msg = simple_msg(Context, [always(Pieces1), always(Pieces2)]),
    Spec = error_spec(severity_error, phase_purity_check, [Msg]).

:- func warn_unnecessary_body_impurity_decl(module_info, pred_id, prog_context,
    purity) = error_spec.

warn_unnecessary_body_impurity_decl(ModuleInfo, PredId, Context,
        DeclaredPurity) = Spec :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_purity(PredInfo, ActualPurity),
    purity_name(DeclaredPurity, DeclaredPurityName),
    purity_name(ActualPurity, ActualPurityName),
    PredPieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
        PredId),

    Pieces1 = [words("In call to")] ++ PredPieces ++ [suffix(":"), nl,
        words("warning: unnecessary"), quote(DeclaredPurityName),
        words("indicator."), nl],
    ( ActualPurity = purity_pure ->
        Pieces2 = [words("No purity indicator is necessary."), nl]
    ;
        Pieces2 = [words("A purity indicator of"), quote(ActualPurityName),
            words("is sufficient."), nl]
    ),
    Msg = simple_msg(Context, [always(Pieces1), always(Pieces2)]),
    Spec = error_spec(severity_warning, phase_purity_check, [Msg]).

:- func warn_redundant_promise_purity(prog_context, purity, purity)
    = error_spec.

warn_redundant_promise_purity(Context, PromisedPurity, InsidePurity) = Spec :-
    purity_name(PromisedPurity, PromisedPurityName),
    DeclName = "promise_" ++ PromisedPurityName,
    purity_name(InsidePurity, InsidePurityName),
    Pieces = [words("Warning: unnecessary"), quote(DeclName),
        words("goal."), nl,
        words("The purity inside is"), words(InsidePurityName), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_warning, phase_purity_check, [Msg]).

:- func report_error_closure_purity(prog_context, purity, purity) = error_spec.

report_error_closure_purity(Context, _DeclaredPurity, ActualPurity) = Spec :-
    purity_name(ActualPurity, ActualPurityName),
    Pieces = [words("Purity error in closure: closure body is"),
        fixed(ActualPurityName), suffix(","),
        words("but closure was not declared"),
        fixed(ActualPurityName), suffix("."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_purity_check, [Msg]).

impure_unification_expr_error(Context, Purity) = Spec :-
    purity_name(Purity, PurityName),
    Pieces = [words("Purity error: unification with expression"),
        words("was declared"), fixed(PurityName ++ ","),
        words("but expression was not a function call.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_purity_check, [Msg]).

:- func impure_parallel_conjunct_error(prog_context, purity) = error_spec.

impure_parallel_conjunct_error(Context, Purity) = Spec :-
    purity_name(Purity, PurityName),
    Pieces = [words("Purity error: parallel conjunct is"),
        fixed(PurityName ++ ","),
        words("but parallel conjuncts must be pure or semipure.")],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_purity_check, [Msg]).

%-----------------------------------------------------------------------------%

:- type purity_info
    --->    purity_info(
                % Fields not changed by purity checking.
                module_info         :: module_info,
                run_post_typecheck  :: bool,

                % Fields which may be changed.
                pred_info           :: pred_info,
                vartypes            :: vartypes,
                varset              :: prog_varset,
                messages            :: list(error_spec),
                implicit_purity     :: implicit_purity_promise
                                    % If this is make_implicit_promises then
                                    % purity annotations are optional in the
                                    % current scope and purity warnings/errors
                                    % should not be generated.
            ).

:- pred purity_info_add_message(error_spec::in,
    purity_info::in, purity_info::out) is det.

purity_info_add_message(Spec, Info0, Info) :-
    Info = Info0 ^ messages := [Spec | Info0 ^ messages].

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "purity.m".

%-----------------------------------------------------------------------------%
:- end_module purity.
%-----------------------------------------------------------------------------%
