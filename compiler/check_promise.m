%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module checks that exported promises refer only to exported entities.
%
%---------------------------------------------------------------------------%

:- module check_hlds.check_promise.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

    % Purity checking should call post_typecheck_finish_promise on every
    % predicate that implements a promise, with the third arg saying
    % what kind of promise it is.
    %
    % This predicate records the promise in the relevant promise table 
    % (the assertion table or the promise_ex table). It then removes the
    % predicate implementing the promise from the list of the pred ids
    % that future compiler passes should process.
    %
    % If the assertion is in the interface, we check that it doesn't refer
    % to any symbols which are local to that module.
    %
:- pred check_and_store_promise(pred_id::in, pred_info::in,
    promise_type::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.assertion.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module require.

%---------------------------------------------------------------------------%

check_and_store_promise(PredId, PredInfo, PromiseType,
        !ModuleInfo, !Specs) :-
    % Store the declaration in the appropriate table and get the goal
    % for the promise.
    store_promise(PredId, PredInfo, PromiseType, !ModuleInfo, Goal),

    % Remove the predicate from further processing.
    module_info_make_pred_id_invalid(PredId, !ModuleInfo),

    % If the promise is in the interface, then ensure that it doesn't refer
    % to any local symbols.
    ( if pred_info_is_exported(PredInfo) then
        check_in_interface_promise_goal(!.ModuleInfo, PredInfo, Goal, !Specs)
    else
        true
    ).

%---------------------%

    % Store promise declaration, normalise goal and return new module_info
    % and the goal for further processing.
    %
:- pred store_promise(pred_id::in, pred_info::in, promise_type::in,
    module_info::in, module_info::out, hlds_goal::out) is det.

store_promise(PredId, PredInfo, PromiseType, !ModuleInfo, Goal) :-
    (
        % Case for assertions.
        PromiseType = promise_type_true,
        module_info_get_assertion_table(!.ModuleInfo, AssertTable0),
        assertion_table_add_assertion(PredId, AssertionId,
            AssertTable0, AssertTable),
        module_info_set_assertion_table(AssertTable, !ModuleInfo),
        assertion.assert_id_goal(!.ModuleInfo, AssertionId, Goal),
        assertion.record_preds_used_in(Goal, AssertionId, !ModuleInfo)
    ;
        % Case for exclusivity.
        ( PromiseType = promise_type_exclusive
        ; PromiseType = promise_type_exclusive_exhaustive
        ),
        get_promise_ex_goal(PredInfo, Goal),
        predids_from_goal(Goal, PredIds),
        module_info_get_exclusive_table(!.ModuleInfo, Table0),
        list.foldl(exclusive_table_add(PredId), PredIds, Table0, Table),
        module_info_set_exclusive_table(Table, !ModuleInfo)
    ;
        % Case for exhaustiveness -- XXX not yet implemented.
        PromiseType = promise_type_exhaustive,
        get_promise_ex_goal(PredInfo, Goal)
    ).

    % Get the goal from a promise_ex declaration.
    %
:- pred get_promise_ex_goal(pred_info::in, hlds_goal::out) is det.

get_promise_ex_goal(PredInfo, Goal) :-
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
    get_clause_list_maybe_repeated(ClausesRep, Clauses),
    ( if Clauses = [Clause] then
        Goal0 = Clause ^ clause_body,
        assertion.normalise_goal(Goal0, Goal)
    else
        unexpected($module, $pred, "not a single clause")
    ).

%---------------------%

    % Ensure that an assertion which is defined in an interface doesn't
    % refer to any constructors, functions and predicates defined in the
    % implementation of that module.
    %
:- pred check_in_interface_promise_goal(module_info::in, pred_info::in,
    hlds_goal::in, list(error_spec)::in, list(error_spec)::out) is det.

check_in_interface_promise_goal(ModuleInfo, PredInfo, Goal, !Specs) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = plain_call(PredId, _, _, _, _,SymName),
        module_info_pred_info(ModuleInfo, PredId, CallPredInfo),
        pred_info_get_status(CallPredInfo, PredStatus),
        DefnInImplSection = pred_status_defined_in_impl_section(PredStatus),
        (
            DefnInImplSection = yes,
            Context = goal_info_get_context(GoalInfo),
            PredOrFunc = pred_info_is_pred_or_func(CallPredInfo),
            Arity = pred_info_orig_arity(CallPredInfo),
            IdPieces =
                [simple_call(simple_call_id(PredOrFunc, SymName, Arity))],
            report_assertion_interface_error(ModuleInfo, Context, IdPieces,
                !Specs)
        ;
            DefnInImplSection = no
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _)
    ;
        GoalExpr = unify(Var, RHS, _, _, _),
        Context = goal_info_get_context(GoalInfo),
        check_in_interface_promise_unify_rhs(ModuleInfo, PredInfo, Var, RHS,
            Context, !Specs)
    ;
        GoalExpr = call_foreign_proc(_, PredId, _, _, _, _, _),
        module_info_pred_info(ModuleInfo, PredId, PragmaPredInfo),
        pred_info_get_status(PragmaPredInfo, PredStatus),
        DefnInImplSection = pred_status_defined_in_impl_section(PredStatus),
        (
            DefnInImplSection = yes,
            Context = goal_info_get_context(GoalInfo),
            PredOrFunc = pred_info_is_pred_or_func(PragmaPredInfo),
            Name = pred_info_name(PragmaPredInfo),
            SymName = unqualified(Name),
            Arity = pred_info_orig_arity(PragmaPredInfo),
            IdPieces =
                [simple_call(simple_call_id(PredOrFunc, SymName, Arity))],
            report_assertion_interface_error(ModuleInfo, Context, IdPieces,
                !Specs)
        ;
            DefnInImplSection = no
        )
    ;
        GoalExpr = conj(_, Goals),
        check_in_interface_promise_goals(ModuleInfo, PredInfo, Goals, !Specs)
    ;
        GoalExpr = switch(_, _, _),
        unexpected($module, $pred, "assertion contains switch")
    ;
        GoalExpr = disj(Goals),
        check_in_interface_promise_goals(ModuleInfo, PredInfo, Goals, !Specs)
    ;
        GoalExpr = negation(SubGoal),
        check_in_interface_promise_goal(ModuleInfo, PredInfo, SubGoal, !Specs)
    ;
        GoalExpr = scope(_, SubGoal),
        check_in_interface_promise_goal(ModuleInfo, PredInfo, SubGoal, !Specs)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        check_in_interface_promise_goal(ModuleInfo, PredInfo, Cond, !Specs),
        check_in_interface_promise_goal(ModuleInfo, PredInfo, Then, !Specs),
        check_in_interface_promise_goal(ModuleInfo, PredInfo, Else, !Specs)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            check_in_interface_promise_goal(ModuleInfo, PredInfo, MainGoal,
                !Specs),
            check_in_interface_promise_goals(ModuleInfo, PredInfo, OrElseGoals,
                !Specs)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            check_in_interface_promise_goal(ModuleInfo, PredInfo, SubGoal,
                !Specs)
        ;
            ShortHand = bi_implication(LHS, RHS),
            check_in_interface_promise_goal(ModuleInfo, PredInfo, LHS, !Specs),
            check_in_interface_promise_goal(ModuleInfo, PredInfo, RHS, !Specs)
        )
    ).

:- pred check_in_interface_promise_unify_rhs(module_info::in, pred_info::in,
    prog_var::in, unify_rhs::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_in_interface_promise_unify_rhs(ModuleInfo, PredInfo, Var, RHS, Context,
        !Specs) :-
    (
        RHS = rhs_var(_)
    ;
        RHS = rhs_functor(ConsId, _, _),
        pred_info_get_clauses_info(PredInfo, ClausesInfo),
        clauses_info_get_vartypes(ClausesInfo, VarTypes),
        lookup_var_type(VarTypes, Var, Type),
        type_to_ctor_det(Type, TypeCtor),
        module_info_get_type_table(ModuleInfo, TypeTable),
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        get_type_defn_status(TypeDefn, TypeStatus),
        DefinedInImpl = type_status_defined_in_impl_section(TypeStatus),
        (
            DefinedInImpl = yes,
            IdPieces = [words("constructor"),
                qual_cons_id_and_maybe_arity(ConsId)],
            report_assertion_interface_error(ModuleInfo, Context, IdPieces,
                !Specs)
        ;
            DefinedInImpl = no
        )
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _, Goal),
        check_in_interface_promise_goal(ModuleInfo, PredInfo, Goal, !Specs)
    ).

:- pred check_in_interface_promise_goals(module_info::in, pred_info::in,
    list(hlds_goal)::in, list(error_spec)::in, list(error_spec)::out) is det.

check_in_interface_promise_goals(_ModuleInfo, _PredInfo, [], !Specs).
check_in_interface_promise_goals(ModuleInfo, PredInfo, [Goal0 | Goal0s],
        !Specs) :-
    check_in_interface_promise_goal(ModuleInfo, PredInfo, Goal0, !Specs),
    check_in_interface_promise_goals(ModuleInfo, PredInfo, Goal0s, !Specs).

%---------------------%

:- pred report_assertion_interface_error(module_info::in, prog_context::in,
    list(format_component)::in, list(error_spec)::in, list(error_spec)::out)
    is det.

report_assertion_interface_error(ModuleInfo, Context, IdPieces, !Specs) :-
    module_info_get_name(ModuleInfo, ModuleName),
    MainPieces =
        [words("In interface for module"), qual_sym_name(ModuleName),
        suffix(":"), nl,
        words("error: exported promise refers to")] ++ IdPieces ++
        [words("which is defined in the implementation section of module"),
        qual_sym_name(ModuleName), suffix("."), nl],
    VerbosePieces =
        [words("Either move the promise into the implementation section"),
        words("or move the definition into the interface."), nl],
    Msgs = [always(MainPieces), verbose_only(verbose_always, VerbosePieces)],
    Spec = error_spec(severity_error, phase_type_check,
        [simple_msg(Context, Msgs)]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
:- end_module check_hlds.check_promise.
%---------------------------------------------------------------------------%
