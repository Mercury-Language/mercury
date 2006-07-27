%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: prog_io_goal.m.
% Main authors: fjh, zs.

% This module defines the predicates that parse goals.

%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io_goal.
:- interface.

:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io_util.

:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

    % Convert a single term into a goal.
    %
:- pred parse_goal(term::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out) is det.

    % Convert a term, possibly starting with `some [Vars]', into
    % a list of the quantified variables, a list of quantified
    % state variables, and a goal. (If the term doesn't start
    % with `some [Vars]', we return empty lists of variables.)
    %
:- pred parse_some_vars_goal(term::in,
    maybe3(list(prog_var), list(prog_var), goal)::out,
    prog_varset::in, prog_varset::out) is det.

    % parse_pred_expression/3 converts the first argument to a :-/2
    % higher-order pred expression into a list of variables, a list
    % of their corresponding modes, and a determinism.
    %
:- pred parse_pred_expression(term::in, lambda_eval_method::out,
    list(prog_term)::out, list(mer_mode)::out, determinism::out) is semidet.

    % parse_dcg_pred_expression/3 converts the first argument to a -->/2
    % higher-order DCG pred expression into a list of arguments, a list
    % of their corresponding modes and the two DCG argument modes, and a
    % determinism.
    % This is a variant of the higher-order pred syntax:
    %   `(pred(Var1::Mode1, ..., VarN::ModeN, DCG0Mode, DCGMode)
    %       is Det --> Goal)'.
    %
:- pred parse_dcg_pred_expression(term::in, lambda_eval_method::out,
    list(prog_term)::out, list(mer_mode)::out, determinism::out) is semidet.

    % parse_func_expression/3 converts the first argument to a :-/2
    % higher-order func expression into a list of arguments, a list
    % of their corresponding modes, and a determinism.  The syntax
    % of a higher-order func expression is
    %   `(func(Var1::Mode1, ..., VarN::ModeN) = (VarN1::ModeN1) is Det
    %       :- Goal)'
    % or
    %   `(func(Var1, ..., VarN) = (VarN1) is Det :- Goal)'
    %       where the modes are assumed to be `in' for the
    %       function arguments and `out' for the result
    % or
    %   `(func(Var1, ..., VarN) = (VarN1) :- Goal)'
    %       where the modes are assumed as above, and the
    %       determinism is assumed to be det
    % or
    %   `(func(Var1, ..., VarN) = (VarN1)). '
    %
:- pred parse_func_expression(term::in, lambda_eval_method::out,
    list(prog_term)::out, list(mer_mode)::out, determinism::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
:- import_module char.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

parse_goal(Term, MaybeGoal, !VarSet) :-
    % We could do some error-checking here, but all errors are picked up
    % in either the type-checker or parser anyway.

    % First, get the goal context.
    (
        Term = term.functor(_, _, Context)
    ;
        Term = term.variable(_),
        term.context_init(Context)
    ),
    % We just check if it matches the appropriate pattern for one of the
    % builtins. If it doesn't match any of the builtins, then it's just
    % a predicate call.
    (
        % Check for builtins...
        Term = term.functor(term.atom(Name), Args, Context),
        parse_goal_2(Name, Args, Context, MaybeGoalPrime, !VarSet)
    ->
        MaybeGoal = MaybeGoalPrime
    ;
        % It's not a builtin.
        term.coerce(Term, ArgsTerm),
        % Check for predicate calls.
        ( sym_name_and_args(ArgsTerm, SymName, Args) ->
            GoalExpr = call_expr(SymName, Args, purity_pure)
        ;
            % A call to a free variable, or to a number or string.
            % Just translate it into a call to call/1 - the typechecker
            % will catch calls to numbers and strings.
            GoalExpr = call_expr(unqualified("call"), [ArgsTerm], purity_pure)
        ),
        Goal = GoalExpr - Context,
        MaybeGoal = ok1(Goal)
    ).

%-----------------------------------------------------------------------------%

:- pred parse_goal_2(string::in, list(term)::in, term.context::in,
    maybe1(goal)::out, prog_varset::in, prog_varset::out) is semidet.

    % Since (A -> B) has different semantics in standard Prolog
    % (A -> B ; fail) than it does in NU-Prolog or Mercury (A -> B ; true),
    % for the moment we'll just disallow it.
    % For consistency we also disallow if-then without the else.

parse_goal_2("true", [], Context, ok1(true_expr - Context), !VarSet).
parse_goal_2("fail", [], Context, ok1(fail_expr - Context), !VarSet).
parse_goal_2("=", [ATerm0, BTerm0], Context, MaybeGoal, !VarSet) :-
    term.coerce(ATerm0, ATerm),
    term.coerce(BTerm0, BTerm),
    MaybeGoal = ok1(unify_expr(ATerm, BTerm, purity_pure) - Context).
parse_goal_2(",", [ATerm, BTerm], Context, MaybeGoal, !VarSet) :-
    parse_goal(ATerm, MaybeAGoal, !VarSet),
    parse_goal(BTerm, MaybeBGoal, !VarSet),
    (
        MaybeAGoal = ok1(AGoal),
        MaybeBGoal = ok1(BGoal)
    ->
        MaybeGoal = ok1(conj_expr(AGoal, BGoal) - Context)
    ;
        AErrors = get_any_errors1(MaybeAGoal),
        BErrors = get_any_errors1(MaybeBGoal),
        MaybeGoal = error1(AErrors ++ BErrors)
    ).
parse_goal_2("&", [ATerm, BTerm], Context, MaybeGoal, !VarSet) :-
    parse_goal(ATerm, MaybeAGoal, !VarSet),
    parse_goal(BTerm, MaybeBGoal, !VarSet),
    (
        MaybeAGoal = ok1(AGoal),
        MaybeBGoal = ok1(BGoal)
    ->
        MaybeGoal = ok1(par_conj_expr(AGoal, BGoal) - Context)
    ;
        AErrors = get_any_errors1(MaybeAGoal),
        BErrors = get_any_errors1(MaybeBGoal),
        MaybeGoal = error1(AErrors ++ BErrors)
    ).
parse_goal_2(";", [ATerm, BTerm], Context, MaybeGoal, !VarSet) :-
    ( ATerm = term.functor(term.atom("->"), [XTerm, YTerm], _Context) ->
        parse_some_vars_goal(XTerm, MaybeXGoal, !VarSet),
        parse_goal(YTerm, MaybeYGoal, !VarSet),
        parse_goal(BTerm, MaybeBGoal, !VarSet),
        (
            MaybeXGoal = ok3(Vars, StateVars, XGoal),
            MaybeYGoal = ok1(YGoal),
            MaybeBGoal = ok1(BGoal)
        ->
            Goal = if_then_else_expr(Vars, StateVars, XGoal, YGoal, BGoal)
                - Context,
            MaybeGoal = ok1(Goal)
        ;
            XErrors = get_any_errors3(MaybeXGoal),
            YErrors = get_any_errors1(MaybeYGoal),
            BErrors = get_any_errors1(MaybeBGoal),
            MaybeGoal = error1(XErrors ++ YErrors ++ BErrors)
        )
    ;
        parse_goal(ATerm, MaybeAGoal, !VarSet),
        parse_goal(BTerm, MaybeBGoal, !VarSet),
        (
            MaybeAGoal = ok1(AGoal),
            MaybeBGoal = ok1(BGoal)
        ->
            MaybeGoal = ok1(disj_expr(AGoal, BGoal) - Context)
        ;
            AErrors = get_any_errors1(MaybeAGoal),
            BErrors = get_any_errors1(MaybeBGoal),
            MaybeGoal = error1(AErrors ++ BErrors)
        )
    ).
parse_goal_2("else", [IfTerm, CTerm], Context, MaybeGoal, !VarSet) :-
    IfTerm = term.functor(term.atom("if"),
        [term.functor(term.atom("then"), [ATerm, BTerm], _)], _),
    parse_some_vars_goal(ATerm, MaybeAGoal, !VarSet),
    parse_goal(BTerm, MaybeBGoal, !VarSet),
    parse_goal(CTerm, MaybeCGoal, !VarSet),
    (
        MaybeAGoal = ok3(Vars, StateVars, AGoal),
        MaybeBGoal = ok1(BGoal),
        MaybeCGoal = ok1(CGoal)
    ->
        Goal = if_then_else_expr(Vars, StateVars, AGoal, BGoal, CGoal)
            - Context,
        MaybeGoal = ok1(Goal)
    ;
        AErrors = get_any_errors3(MaybeAGoal),
        BErrors = get_any_errors1(MaybeBGoal),
        CErrors = get_any_errors1(MaybeCGoal),
        MaybeGoal = error1(AErrors ++ BErrors ++ CErrors)
    ).
parse_goal_2("not", [ATerm], Context, MaybeGoal, !VarSet) :-
    parse_goal(ATerm, MaybeAGoal, !VarSet),
    (
        MaybeAGoal = ok1(AGoal),
        MaybeGoal = ok1(not_expr(AGoal) - Context)
    ;
        MaybeAGoal = error1(_),
        MaybeGoal = MaybeAGoal
    ).
parse_goal_2("\\+", [ATerm], Context, MaybeGoal, !VarSet) :-
    parse_goal(ATerm, MaybeAGoal, !VarSet),
    (
        MaybeAGoal = ok1(AGoal),
        MaybeGoal = ok1(not_expr(AGoal) - Context)
    ;
        MaybeAGoal = error1(_),
        MaybeGoal = MaybeAGoal
    ).
parse_goal_2("all", [QVarsTerm, SubTerm], Context, MaybeGoal, !VarSet) :-
    % Extract any state variables in the quantifier.
    parse_quantifier_vars(QVarsTerm, MaybeStateVarsAndVars),
    parse_goal(SubTerm, MaybeSubGoal, !VarSet),
    (
        MaybeStateVarsAndVars = ok2(Vars0, StateVars0),
        MaybeSubGoal = ok1(SubGoal)
    ->
        list.map(term.coerce_var, Vars0, Vars),
        list.map(term.coerce_var, StateVars0, StateVars),
        SubGoal = SubGoalExpr - SubContext,
        (
            Vars = [],
            StateVars = [],
            GoalExpr = SubGoalExpr
        ;
            Vars = [],
            StateVars = [_ | _],
            GoalExpr = all_state_vars_expr(StateVars, SubGoal)
        ;
            Vars = [_ | _],
            StateVars = [],
            GoalExpr = all_expr(Vars, SubGoal)
        ;
            Vars = [_ | _], StateVars = [_ | _],
            GoalExpr = all_expr(Vars, all_state_vars_expr(StateVars, SubGoal)
                - SubContext)
        ),
        Goal = GoalExpr - Context,
        MaybeGoal = ok1(Goal)
    ;
        VarsErrors = get_any_errors2(MaybeStateVarsAndVars),
        SubGoalErrors = get_any_errors1(MaybeSubGoal),
        MaybeGoal = error1(VarsErrors ++ SubGoalErrors)
    ).
parse_goal_2("<=", [ATerm, BTerm], Context, MaybeGoal, !VarSet) :-
    parse_goal(ATerm, MaybeAGoal, !VarSet),
    parse_goal(BTerm, MaybeBGoal, !VarSet),
    (
        MaybeAGoal = ok1(AGoal),
        MaybeBGoal = ok1(BGoal)
    ->
        MaybeGoal = ok1(implies_expr(BGoal, AGoal) - Context)
    ;
        AErrors = get_any_errors1(MaybeAGoal),
        BErrors = get_any_errors1(MaybeBGoal),
        MaybeGoal = error1(AErrors ++ BErrors)
    ).
parse_goal_2("=>", [ATerm, BTerm], Context, MaybeGoal, !VarSet) :-
    parse_goal(ATerm, MaybeAGoal, !VarSet),
    parse_goal(BTerm, MaybeBGoal, !VarSet),
    (
        MaybeAGoal = ok1(AGoal),
        MaybeBGoal = ok1(BGoal)
    ->
        MaybeGoal = ok1(implies_expr(AGoal, BGoal) - Context)
    ;
        AErrors = get_any_errors1(MaybeAGoal),
        BErrors = get_any_errors1(MaybeBGoal),
        MaybeGoal = error1(AErrors ++ BErrors)
    ).
parse_goal_2("<=>", [ATerm, BTerm], Context, MaybeGoal, !VarSet) :-
    parse_goal(ATerm, MaybeAGoal, !VarSet),
    parse_goal(BTerm, MaybeBGoal, !VarSet),
    (
        MaybeAGoal = ok1(AGoal),
        MaybeBGoal = ok1(BGoal)
    ->
        MaybeGoal = ok1(equivalent_expr(AGoal, BGoal) - Context)
    ;
        AErrors = get_any_errors1(MaybeAGoal),
        BErrors = get_any_errors1(MaybeBGoal),
        MaybeGoal = error1(AErrors ++ BErrors)
    ).
parse_goal_2("some", [QVarsTerm, SubTerm], Context, MaybeGoal, !VarSet) :-
    % Extract any state variables in the quantifier.
    parse_quantifier_vars(QVarsTerm, MaybeStateVarsAndVars),
    parse_goal(SubTerm, MaybeSubGoal, !VarSet),
    (
        MaybeStateVarsAndVars = ok2(Vars0, StateVars0),
        MaybeSubGoal = ok1(SubGoal)
    ->
        list.map(term.coerce_var, Vars0, Vars),
        list.map(term.coerce_var, StateVars0, StateVars),
        SubGoal = SubGoalExpr - SubContext,
        (
            Vars = [],
            StateVars = [],
            GoalExpr = SubGoalExpr
        ;
            Vars = [],
            StateVars = [_ | _],
            GoalExpr = some_state_vars_expr(StateVars, SubGoal)
        ;
            Vars = [_ | _],
            StateVars = [],
            GoalExpr = some_expr(Vars, SubGoal)
        ;
            Vars = [_ | _],
            StateVars = [_ | _],
            GoalExpr = some_expr(Vars, some_state_vars_expr(StateVars, SubGoal)
                - SubContext)
        ),
        Goal = GoalExpr - Context,
        MaybeGoal = ok1(Goal)
    ;
        VarsErrors0 = get_any_errors2(MaybeStateVarsAndVars),
        VarsErrors = assoc_list.map_keys_only(
            string.append("in first argument of some: "), VarsErrors0),
        SubGoalErrors = get_any_errors1(MaybeSubGoal),
        MaybeGoal = error1(VarsErrors ++ SubGoalErrors)
    ).
parse_goal_2("trace", [ParamsTerm, SubTerm], Context, MaybeGoal, !VarSet) :-
    parse_trace_params(Context, ParamsTerm, MaybeParams),
    parse_goal(SubTerm, MaybeSubGoal, !VarSet),
    (
        MaybeParams = ok1(Params),
        MaybeSubGoal = ok1(SubGoal)
    ->
        convert_trace_params(Params, MaybeComponents),
        (
            MaybeComponents = ok4(CompileTime, RunTime, MaybeIO, MutVars),
            GoalExpr = trace_expr(CompileTime, RunTime, MaybeIO, MutVars,
                SubGoal),
            MaybeGoal = ok1(GoalExpr - Context)
        ;
            MaybeComponents = error4(Errors),
            MaybeGoal = error1(Errors)
        )
    ;
        ParamsErrors = get_any_errors1(MaybeParams),
        SubGoalErrors = get_any_errors1(MaybeSubGoal),
        MaybeGoal = error1(ParamsErrors ++ SubGoalErrors)
    ).
parse_goal_2("promise_equivalent_solutions", [VarsTerm, SubTerm], Context,
        MaybeGoal, !VarSet) :-
    parse_vars_and_state_vars(VarsTerm, MaybeVars),
    parse_goal(SubTerm, MaybeSubGoal, !VarSet),
    (
        MaybeVars = ok3(Vars0, DotSVars0, ColonSVars0),
        MaybeSubGoal = ok1(SubGoal)
    ->
        list.map(term.coerce_var, Vars0, Vars),
        list.map(term.coerce_var, DotSVars0, DotSVars),
        list.map(term.coerce_var, ColonSVars0, ColonSVars),
        MaybeGoal = ok1(promise_equivalent_solutions_expr(Vars,
            DotSVars, ColonSVars, SubGoal) - Context)
    ;
        VarsErrors = get_any_errors3(MaybeVars),
        SubGoalErrors = get_any_errors1(MaybeSubGoal),
        MaybeGoal = error1(VarsErrors ++ SubGoalErrors)
    ).
parse_goal_2("promise_equivalent_solution_sets", [VarsTerm, SubTerm], Context,
        MaybeGoal, !VarSet) :-
    parse_vars_and_state_vars(VarsTerm, MaybeVars),
    parse_goal(SubTerm, MaybeSubGoal, !VarSet),
    (
        MaybeVars = ok3(Vars0, DotSVars0, ColonSVars0),
        MaybeSubGoal = ok1(SubGoal)
    ->
        list.map(term.coerce_var, Vars0, Vars),
        list.map(term.coerce_var, DotSVars0, DotSVars),
        list.map(term.coerce_var, ColonSVars0, ColonSVars),
        MaybeGoal = ok1(promise_equivalent_solution_sets_expr(Vars,
            DotSVars, ColonSVars, SubGoal) - Context)
    ;
        VarsErrors = get_any_errors3(MaybeVars),
        SubGoalErrors = get_any_errors1(MaybeSubGoal),
        MaybeGoal = error1(VarsErrors ++ SubGoalErrors)
    ).
parse_goal_2("arbitrary", [VarsTerm, SubTerm], Context, MaybeGoal, !VarSet) :-
    parse_vars_and_state_vars(VarsTerm, MaybeVars),
    parse_goal(SubTerm, MaybeSubGoal, !VarSet),
    (
        MaybeVars = ok3(Vars0, DotSVars0, ColonSVars0),
        MaybeSubGoal = ok1(SubGoal)
    ->
        list.map(term.coerce_var, Vars0, Vars),
        list.map(term.coerce_var, DotSVars0, DotSVars),
        list.map(term.coerce_var, ColonSVars0, ColonSVars),
        MaybeGoal = ok1(promise_equivalent_solution_arbitrary_expr(Vars,
            DotSVars, ColonSVars, SubGoal) - Context)
    ;
        VarsErrors = get_any_errors3(MaybeVars),
        SubGoalErrors = get_any_errors1(MaybeSubGoal),
        MaybeGoal = error1(VarsErrors ++ SubGoalErrors)
    ).
parse_goal_2("promise_pure", [SubTerm], Context, MaybeGoal, !VarSet) :-
    parse_goal(SubTerm, MaybeSubGoal, !VarSet),
    (
        MaybeSubGoal = ok1(SubGoal),
        Goal = promise_purity_expr(dont_make_implicit_promises,
            purity_pure, SubGoal) - Context,
        MaybeGoal = ok1(Goal)
    ;
        MaybeSubGoal = error1(Errors),
        MaybeGoal = error1(Errors)
    ).
parse_goal_2("promise_semipure", [SubTerm], Context, MaybeGoal, !VarSet) :-
    parse_goal(SubTerm, MaybeSubGoal, !VarSet),
    (
        MaybeSubGoal = ok1(SubGoal),
        Goal = promise_purity_expr(dont_make_implicit_promises,
            purity_semipure, SubGoal) - Context,
        MaybeGoal = ok1(Goal)
    ;
        MaybeSubGoal = error1(Errors),
        MaybeGoal = error1(Errors)
    ).
parse_goal_2("promise_impure", [SubTerm], Context, MaybeGoal, !VarSet) :-
    parse_goal(SubTerm, MaybeSubGoal, !VarSet),
    (
        MaybeSubGoal = ok1(SubGoal),
        Goal = promise_purity_expr(dont_make_implicit_promises,
            purity_impure, SubGoal) - Context,
        MaybeGoal = ok1(Goal)
    ;
        MaybeSubGoal = error1(Errors),
        MaybeGoal = error1(Errors)
    ).
parse_goal_2("promise_pure_implicit", [SubTerm], Context, MaybeGoal,
        !VarSet) :-
    parse_goal(SubTerm, MaybeSubGoal, !VarSet),
    (
        MaybeSubGoal = ok1(SubGoal),
        Goal = promise_purity_expr(make_implicit_promises,
            purity_pure, SubGoal) - Context,
        MaybeGoal = ok1(Goal)
    ;
        MaybeSubGoal = error1(Errors),
        MaybeGoal = error1(Errors)
    ).
parse_goal_2("promise_semipure_implicit", [SubTerm], Context, MaybeGoal,
        !VarSet) :-
    parse_goal(SubTerm, MaybeSubGoal, !VarSet),
    (
        MaybeSubGoal = ok1(SubGoal),
        Goal = promise_purity_expr(make_implicit_promises,
            purity_semipure, SubGoal) - Context,
        MaybeGoal = ok1(Goal)
    ;
        MaybeSubGoal = error1(Errors),
        MaybeGoal = error1(Errors)
    ).
parse_goal_2("promise_impure_implicit", [SubTerm], Context, MaybeGoal,
        !VarSet) :-
    parse_goal(SubTerm, MaybeSubGoal, !VarSet),
    (
        MaybeSubGoal = ok1(SubGoal),
        Goal = promise_purity_expr(make_implicit_promises,
            purity_impure, SubGoal) - Context,
        MaybeGoal = ok1(Goal)
    ;
        MaybeSubGoal = error1(Errors),
        MaybeGoal = error1(Errors)
    ).
parse_goal_2("impure", [SubTerm], Context, MaybeGoal, !VarSet) :-
    parse_goal_with_purity(SubTerm, purity_impure, Context, MaybeGoal,
        !VarSet).
parse_goal_2("semipure", [SubTerm], Context, MaybeGoal, !VarSet) :-
    parse_goal_with_purity(SubTerm, purity_semipure, Context, MaybeGoal,
        !VarSet).
parse_goal_2("is", [ATerm0, BTerm0], Context, MaybeGoal, !VarSet) :-
    % The following is a temporary hack to handle `is' in the parser -
    % we ought to handle it in the code generation - but then `is/2' itself
    % is a bit of a hack.
    term.coerce(ATerm0, ATerm),
    term.coerce(BTerm0, BTerm),
    MaybeGoal = ok1(unify_expr(ATerm, BTerm, purity_pure) - Context).

:- pred parse_goal_with_purity(term::in, purity::in, context::in,
    maybe1(goal)::out, prog_varset::in, prog_varset::out) is det.

parse_goal_with_purity(Term, Purity, Context, MaybeGoal, !VarSet) :-
    parse_goal(Term, MaybeSubGoal, !VarSet),
    (
        MaybeSubGoal = ok1(SubGoal),
        SubGoal = SubGoalExpr - _SubContext,
        ( SubGoalExpr = call_expr(Pred, Args, purity_pure) ->
            MaybeGoal = ok1(call_expr(Pred, Args, Purity) - Context)
        ; SubGoalExpr = unify_expr(ProgTerm1, ProgTerm2, purity_pure) ->
            MaybeGoal = ok1(unify_expr(ProgTerm1, ProgTerm2, Purity) - Context)
        ;
            % Inappropriate placement of an impurity marker, so we treat
            % it like a predicate call. typecheck.m prints out something
            % descriptive for these errors.
            %
            % XXX we could return MaybeGoal = error1 here.
            purity_name(Purity, PurityString),
            term.coerce(Term, CoercedTerm),
            GoalExpr = call_expr(unqualified(PurityString), [CoercedTerm],
                purity_pure),
            MaybeGoal = ok1(GoalExpr - Context)
        )
    ;
        MaybeSubGoal = error1(_),
        MaybeGoal = MaybeSubGoal
    ).

%-----------------------------------------------------------------------------%

parse_some_vars_goal(Term, MaybeVarsAndGoal, !VarSet) :-
    ( Term = term.functor(term.atom("some"), [QVarsTerm, SubTerm], _Context) ->
        parse_quantifier_vars(QVarsTerm, MaybeVars),
        GoalTerm = SubTerm
    ;
        MaybeVars = ok2([], []),
        GoalTerm = Term
    ),
    parse_goal(GoalTerm, MaybeGoal, !VarSet),
    (
        MaybeVars = ok2(Vars0, StateVars0),
        MaybeGoal = ok1(Goal)
    ->
        list.map(term.coerce_var, Vars0, Vars),
        list.map(term.coerce_var, StateVars0, StateVars),
        MaybeVarsAndGoal = ok3(Vars, StateVars, Goal)
    ;
        VarsErrors0 = get_any_errors2(MaybeVars),
        VarsErrors = assoc_list.map_keys_only(
            string.append("in first argument of some: "), VarsErrors0),
        GoalErrors = get_any_errors1(MaybeGoal),
        MaybeVarsAndGoal = error3(VarsErrors ++ GoalErrors)
    ).

%-----------------------------------------------------------------------------%

:- type trace_component
    --->    trace_component_compiletime(trace_expr(trace_compiletime))
    ;       trace_component_runtime(trace_expr(trace_runtime))
    ;       trace_component_maybe_io(prog_var)
    ;       trace_component_mutable_var(trace_mutable_var).

:- pred parse_trace_params(context::in, term::in,
    maybe1(assoc_list(trace_component, term))::out) is det.

parse_trace_params(Context, Term, MaybeComponentsTerms) :-
    ( Term = term.functor(term.atom("[]"), [], _) ->
        MaybeComponentsTerms = ok1([])
    ; Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _) ->
        parse_trace_component(Term, HeadTerm, MaybeHeadComponent),
        parse_trace_params(Context, TailTerm, MaybeTailComponentsTerms),
        (
            MaybeHeadComponent = ok1(HeadComponent),
            MaybeTailComponentsTerms = ok1(TailComponentsTerms)
        ->
            MaybeComponentsTerms = ok1([HeadComponent |
                TailComponentsTerms])
        ;
            HeadErrors = get_any_errors1(MaybeHeadComponent),
            TailErrors = get_any_errors1(MaybeTailComponentsTerms),
            MaybeComponentsTerms = error1(HeadErrors ++ TailErrors)
        )
    ;
        (
            Term = term.functor(_, _, _),
            Msg = "invalid trace goal paramater",
            MaybeComponentsTerms = error1([Msg - Term])
        ;
            Term = term.variable(_),
            Msg = "expected trace goal paramater, found variable",
            ErrorTerm = term.functor(term.atom(""), [], Context),
            MaybeComponentsTerms = error1([Msg - ErrorTerm])
        )
    ).

:- pred parse_trace_component(term::in, term::in,
    maybe1(pair(trace_component, term))::out) is det.

parse_trace_component(ErrorTerm, Term, MaybeComponentTerm) :-
    (
        Term = term.functor(Functor, SubTerms, _),
        ( Functor = term.atom(Atom) ->
            (
                ( Atom = "compiletime"
                ; Atom = "compile_time"
                )
            ->
                ( SubTerms = [SubTerm] ->
                    parse_trace_tree(parse_trace_compiletime(Term), SubTerm,
                        MaybeCompileTime),
                    (
                        MaybeCompileTime = ok1(CompileTime),
                        Component = trace_component_compiletime(CompileTime),
                        MaybeComponentTerm = ok1(Component - Term)
                    ;
                        MaybeCompileTime = error1(Errors),
                        MaybeComponentTerm = error1(Errors)
                    )
                ;
                    Msg = Atom ++ " takes exactly one argument, " ++
                        "which should be a boolean expression " ++
                        "of compile-time tests",
                    MaybeComponentTerm = error1([Msg - Term])
                )
            ;
                ( Atom = "runtime"
                ; Atom = "run_time"
                )
            ->
                ( SubTerms = [SubTerm] ->
                    parse_trace_tree(parse_trace_runtime(Term), SubTerm,
                        MaybeRunTime),
                    (
                        MaybeRunTime = ok1(RunTime),
                        Component = trace_component_runtime(RunTime),
                        MaybeComponentTerm = ok1(Component - Term)
                    ;
                        MaybeRunTime = error1(Errors),
                        MaybeComponentTerm = error1(Errors)
                    )
                ;
                    Msg = Atom ++ " takes exactly one argument, " ++
                        "which should be a boolean expression " ++
                        "of run-time tests",
                    MaybeComponentTerm = error1([Msg - Term])
                )
            ;
                Atom = "io"
            ->
                ( SubTerms = [SubTerm] ->
                    (
                        SubTerm = term.functor(term.atom("!"),
                            [term.variable(Var)], _)
                    ->
                        term.coerce_var(Var, ProgVar),
                        Component = trace_component_maybe_io(ProgVar),
                        MaybeComponentTerm = ok1(Component - Term)
                    ;
                        Msg = "the argument of " ++ Atom ++ " should be" ++
                            " a state variable",
                        MaybeComponentTerm = error1([Msg - SubTerm])
                    )
                ;
                    Msg = Atom ++ " takes exactly one argument, " ++
                        "which should be a state variable name",
                    MaybeComponentTerm = error1([Msg - Term])
                )
            ;
                Atom = "state"
            ->
                ( SubTerms = [SubTermA, SubTermB] ->
                    ( SubTermA = term.functor(term.atom(MutableName), [], _) ->
                        MaybeMutable = ok1(MutableName)
                    ;
                        (
                            SubTermA = term.functor(_, _, _),
                            MutableErrorTerm = SubTermA
                        ;
                            SubTermA = term.variable(_),
                            MutableErrorTerm = Term
                        ),
                        MutableMsg = "the first argument of " ++ Atom ++
                            " should be the name of a mutable variable",
                        MaybeMutable = error1([MutableMsg - MutableErrorTerm])
                    ),
                    (
                        SubTermB = term.functor(term.atom("!"),
                            [term.variable(Var)], _)
                    ->
                        MaybeVar = ok1(Var)
                    ;
                        (
                            SubTermB = term.functor(_, _, _),
                            VarErrorTerm = SubTermB
                        ;
                            SubTermB = term.variable(_),
                            VarErrorTerm = Term
                        ),
                        VarMsg = "the second argument of " ++ Atom ++
                            " should be a state variable",
                        MaybeVar = error1([VarMsg - VarErrorTerm])
                    ),
                    (
                        MaybeMutable = ok1(FinalMutable),
                        MaybeVar = ok1(FinalVar)
                    ->
                        term.coerce_var(FinalVar, ProgVar),
                        MutableVar = trace_mutable_var(FinalMutable, ProgVar),
                        Component = trace_component_mutable_var(MutableVar),
                        MaybeComponentTerm = ok1(Component - Term)
                    ;
                        VarErrors = get_any_errors1(MaybeVar),
                        MutableErrors = get_any_errors1(MaybeMutable),
                        MaybeComponentTerm = error1(VarErrors ++ MutableErrors)
                    )
                ;
                    Msg = Atom ++ " takes exactly two arguments, " ++
                        "which should be the name of a mutable variable " ++
                        "and a state variable name",
                    MaybeComponentTerm = error1([Msg - Term])
                )
            ;
                Msg = "invalid trace goal paramater",
                MaybeComponentTerm = error1([Msg - Term])
            )
        ;
            Msg = "invalid trace goal paramater",
            MaybeComponentTerm = error1([Msg - Term])
        )
    ;
        Term = term.variable(_),
        Msg = "expected trace goal paramater, found variable",
        MaybeComponentTerm = error1([Msg - ErrorTerm])
    ).

:- pred parse_trace_tree(pred(term, maybe1(T))::in(pred(in, out) is det),
    term::in, maybe1(trace_expr(T))::out) is det.

parse_trace_tree(BaseParser, Term, MaybeTree) :-
    (
        Term = term.functor(term.atom(Atom), [LTerm, RTerm], _),
        (
            Atom = "or",
            Op = trace_or
        ;
            Atom = "and",
            Op = trace_and
        )
    ->
        parse_trace_tree(BaseParser, LTerm, MaybeLExpr),
        parse_trace_tree(BaseParser, RTerm, MaybeRExpr),
        (
            MaybeLExpr = ok1(LExpr),
            MaybeRExpr = ok1(RExpr)
        ->
            MaybeTree = ok1(trace_op(Op, LExpr, RExpr))
        ;
            LErrors = get_any_errors1(MaybeLExpr),
            RErrors = get_any_errors1(MaybeRExpr),
            MaybeTree = error1(LErrors ++ RErrors)
        )
    ;
        BaseParser(Term, MaybeBase),
        (
            MaybeBase = ok1(Base),
            MaybeTree = ok1(trace_base(Base))
        ;
            MaybeBase = error1(Errors),
            MaybeTree = error1(Errors)
        )
    ).

:- pred parse_trace_compiletime(term::in, term::in,
    maybe1(trace_compiletime)::out) is det.

parse_trace_compiletime(ErrorTerm, Term, MaybeCompiletime) :-
    (
        Term = term.functor(Functor, SubTerms, _),
        ( Functor = term.atom(Atom) ->
            ( Atom = "flag" ->
                ( SubTerms = [SubTerm] ->
                    ( SubTerm = term.functor(term.string(FlagName), [], _) ->
                        Compiletime = trace_flag(FlagName),
                        MaybeCompiletime = ok1(Compiletime)
                    ;
                        Msg = "compile_time paramater `flag'" ++
                            "takes a string as argument",
                        MaybeCompiletime = error1([Msg - Term])
                    )
                ;
                    Msg = "compile_time paramater `flag'" ++
                        "takes just one argument",
                    MaybeCompiletime = error1([Msg - Term])
                )
            ; Atom = "grade" ->
                ( SubTerms = [SubTerm] ->
                    (
                        SubTerm = term.functor(term.atom(GradeName), [], _),
                        GradeName = "debug"
                    ->
                        Compiletime = trace_grade(trace_grade_debug),
                        MaybeCompiletime = ok1(Compiletime)
                    ;
                        Msg = "compile_time paramater `grade' " ++
                            "takes just `debug' as argument (for now)",
                        MaybeCompiletime = error1([Msg - Term])
                    )
                ;
                    Msg = "compile_time paramater `grade'" ++
                        "takes just one argument",
                    MaybeCompiletime = error1([Msg - Term])
                )
            ; Atom = "travelevel" ->
                ( SubTerms = [SubTerm] ->
                    (
                        SubTerm = term.functor(term.atom(LevelName), [], _),
                        (
                            LevelName = "shallow",
                            Level = trace_level_shallow
                        ;
                            LevelName = "deep",
                            Level = trace_level_deep
                        )
                    ->
                        Compiletime = trace_trace_level(Level),
                        MaybeCompiletime = ok1(Compiletime)
                    ;
                        Msg = "compile_time paramater `tracelevel' " ++
                            "takes just `shallow' or `deep' as argument",
                        MaybeCompiletime = error1([Msg - Term])
                    )
                ;
                    Msg = "compile_time paramater `tracelevel'" ++
                        "takes just one argument",
                    MaybeCompiletime = error1([Msg - Term])
                )
            ;
                Msg = "invalid compile_time paramater",
                MaybeCompiletime = error1([Msg - Term])
            )
        ;
            Msg = "invalid compile_time paramater",
            MaybeCompiletime = error1([Msg - Term])
        )
    ;
        Term = term.variable(_),
        Msg = "expected compile_time paramater, found variable",
        MaybeCompiletime = error1([Msg - ErrorTerm])
    ).

:- pred parse_trace_runtime(term::in, term::in,
    maybe1(trace_runtime)::out) is det.

parse_trace_runtime(ErrorTerm, Term, MaybeRuntime) :-
    (
        Term = term.functor(Functor, SubTerms, _),
        ( Functor = term.atom(Atom) ->
            ( Atom = "env" ->
                ( SubTerms = [SubTerm] ->
                    (
                        SubTerm = term.functor(SubFunctor, [], _),
                        ( SubFunctor = term.string(EnvVarName)
                        ; SubFunctor = term.atom(EnvVarName)
                        )
                    ->
                        EnvVarChars = string.to_char_list(EnvVarName),
                        (
                            list.filter(env_var_is_acceptable_char,
                                EnvVarChars, _, [])
                        ->
                            Runtime = trace_envvar(EnvVarName),
                            MaybeRuntime = ok1(Runtime)
                        ;
                            Msg = "run_time paramater `env'" ++
                                "takes an identifier as argument",
                            MaybeRuntime = error1([Msg - SubTerm])
                        )
                    ;
                        Msg = "run_time paramater `env'" ++
                            "takes an identifier as argument",
                        MaybeRuntime = error1([Msg - Term])
                    )
                ;
                    Msg = "run_time paramater `env' takes just one argument",
                    MaybeRuntime = error1([Msg - Term])
                )
            ;
                Term = term.functor(_, _, _),
                Msg = "invalid run_time paramater; expected env(\"ENVVAR\")",
                MaybeRuntime = error1([Msg - Term])
            )
        ;
            Term = term.functor(_, _, _),
            Msg = "invalid run_time paramater; expected env(\"ENVVAR\")",
            MaybeRuntime = error1([Msg - Term])
        )
    ;
        Term = term.variable(_),
        Msg = "expected run_time paramater, found variable",
        MaybeRuntime = error1([Msg - ErrorTerm])
    ).

:- pred env_var_is_acceptable_char(char::in) is semidet.

env_var_is_acceptable_char(Char) :-
    % This definition must be consistent with the check applied in
    % util/mkinit.c.
    (
        char.is_alnum(Char)
    ;
        Char = '_'
    ).

:- pred convert_trace_params(assoc_list(trace_component, term)::in,
    maybe4(maybe(trace_expr(trace_compiletime)),
        maybe(trace_expr(trace_runtime)), maybe(prog_var),
        list(trace_mutable_var))::out) is det.

convert_trace_params(Components, MaybeParams) :-
    convert_trace_params_2(Components, no, no, no, [], [], MaybeParams).

:- pred convert_trace_params_2(assoc_list(trace_component, term)::in,
    maybe(trace_expr(trace_compiletime))::in,
    maybe(trace_expr(trace_runtime))::in,
    maybe(prog_var)::in, list(trace_mutable_var)::in,
    assoc_list(string, term)::in,
    maybe4(maybe(trace_expr(trace_compiletime)),
        maybe(trace_expr(trace_runtime)), maybe(prog_var),
        list(trace_mutable_var))::out) is det.

convert_trace_params_2([], MaybeCompileTime, MaybeRunTime, MaybeIO,
        MutableVars, Errors, MaybeParams) :-
    (
        Errors = [],
        MaybeParams = ok4(MaybeCompileTime, MaybeRunTime, MaybeIO, MutableVars)
    ;
        Errors = [_ | _],
        MaybeParams = error4(Errors)
    ).
convert_trace_params_2([Component - Term | ComponentsTerms],
        !.MaybeCompileTime, !.MaybeRunTime, !.MaybeIO, !.MutableVars,
        !.Errors, MaybeParams) :-
    (
        Component = trace_component_compiletime(CompileTime),
        (
            !.MaybeCompileTime = no,
            !:MaybeCompileTime = yes(CompileTime)
        ;
            !.MaybeCompileTime = yes(_),
            Msg = "duplicate compile_time trace parameter",
            !:Errors = !.Errors ++ [Msg - Term]
        )
    ;
        Component = trace_component_runtime(RunTime),
        (
            !.MaybeRunTime = no,
            !:MaybeRunTime = yes(RunTime)
        ;
            !.MaybeRunTime = yes(_),
            Msg = "duplicate run_time trace parameter",
            !:Errors = !.Errors ++ [Msg - Term]
        )
    ;
        Component = trace_component_maybe_io(IOStateVar),
        (
            !.MaybeIO = no,
            !:MaybeIO = yes(IOStateVar)
        ;
            !.MaybeIO = yes(_),
            Msg = "duplicate io trace parameter",
            !:Errors = !.Errors ++ [Msg - Term]
        )
    ;
        Component = trace_component_mutable_var(MutableVar),
        !:MutableVars = !.MutableVars ++ [MutableVar]
    ),
    convert_trace_params_2(ComponentsTerms, !.MaybeCompileTime,
        !.MaybeRunTime, !.MaybeIO, !.MutableVars, !.Errors, MaybeParams).

%-----------------------------------------------------------------------------%

:- pred parse_lambda_arg(term::in, prog_term::out, mer_mode::out) is semidet.

parse_lambda_arg(Term, ArgTerm, Mode) :-
    Term = term.functor(term.atom("::"), [ArgTerm0, ModeTerm], _),
    term.coerce(ArgTerm0, ArgTerm),
    convert_mode(allow_constrained_inst_var, ModeTerm, Mode0),
    constrain_inst_vars_in_mode(Mode0, Mode).

%-----------------------------------------------------------------------------%
% 
% Code for parsing pred/func expressions
%

parse_pred_expression(PredTerm, lambda_normal, Args, Modes, Det) :-
    PredTerm = term.functor(term.atom("is"), [PredArgsTerm, DetTerm], _),
    DetTerm = term.functor(term.atom(DetString), [], _),
    standard_det(DetString, Det),
    PredArgsTerm = term.functor(term.atom("pred"), PredArgsList, _),
    parse_pred_expr_args(PredArgsList, Args, Modes),
    inst_var_constraints_are_consistent_in_modes(Modes).

parse_dcg_pred_expression(PredTerm, lambda_normal, Args, Modes, Det) :-
    PredTerm = term.functor(term.atom("is"), [PredArgsTerm, DetTerm], _),
    DetTerm = term.functor(term.atom(DetString), [], _),
    standard_det(DetString, Det),
    PredArgsTerm = term.functor(term.atom("pred"), PredArgsList, _),
    parse_dcg_pred_expr_args(PredArgsList, Args, Modes),
    inst_var_constraints_are_consistent_in_modes(Modes).

parse_func_expression(FuncTerm, lambda_normal, Args, Modes, Det) :-
    % Parse a func expression with specified modes and determinism.
    FuncTerm = term.functor(term.atom("is"), [EqTerm, DetTerm], _),
    EqTerm = term.functor(term.atom("="), [FuncArgsTerm, RetTerm], _),
    DetTerm = term.functor(term.atom(DetString), [], _),
    standard_det(DetString, Det),
    FuncArgsTerm = term.functor(term.atom("func"), FuncArgsList, _),

    ( parse_pred_expr_args(FuncArgsList, Args0, Modes0) ->
        parse_lambda_arg(RetTerm, RetArg, RetMode),
        list.append(Args0, [RetArg], Args),
        list.append(Modes0, [RetMode], Modes),
        inst_var_constraints_are_consistent_in_modes(Modes)
    ;
        % The argument modes default to `in',
        % the return mode defaults to `out'.
        in_mode(InMode),
        out_mode(OutMode),
        list.length(FuncArgsList, NumArgs),
        list.duplicate(NumArgs, InMode, Modes0),
        RetMode = OutMode,
        list.append(Modes0, [RetMode], Modes),
        list.append(FuncArgsList, [RetTerm], Args1),
        list.map(term.coerce, Args1, Args)
    ).

parse_func_expression(FuncTerm, lambda_normal, Args, Modes, Det) :-
    % Parse a func expression with unspecified modes and determinism.
    FuncTerm = term.functor(term.atom("="), [FuncArgsTerm, RetTerm], _),
    FuncArgsTerm = term.functor(term.atom("func"), Args0, _),

    % The argument modes default to `in', the return mode defaults to `out',
    % and the determinism defaults to `det'.
    in_mode(InMode),
    out_mode(OutMode),
    list.length(Args0, NumArgs),
    list.duplicate(NumArgs, InMode, Modes0),
    RetMode = OutMode,
    Det = detism_det,
    list.append(Modes0, [RetMode], Modes),
    inst_var_constraints_are_consistent_in_modes(Modes),
    list.append(Args0, [RetTerm], Args1),
    list.map(term.coerce, Args1, Args).

:- pred parse_pred_expr_args(list(term)::in, list(prog_term)::out,
    list(mer_mode)::out) is semidet.

parse_pred_expr_args([], [], []).
parse_pred_expr_args([Term | Terms], [Arg | Args], [Mode | Modes]) :-
    parse_lambda_arg(Term, Arg, Mode),
    parse_pred_expr_args(Terms, Args, Modes).

    % parse_dcg_pred_expr_args is like parse_pred_expr_args except
    % that the last two elements of the list are the modes of the
    % two DCG arguments.
    %
:- pred parse_dcg_pred_expr_args(list(term)::in, list(prog_term)::out,
    list(mer_mode)::out) is semidet.

parse_dcg_pred_expr_args([DCGModeTermA, DCGModeTermB], [],
        [DCGModeA, DCGModeB]) :-
    convert_mode(allow_constrained_inst_var, DCGModeTermA, DCGModeA0),
    convert_mode(allow_constrained_inst_var, DCGModeTermB, DCGModeB0),
    constrain_inst_vars_in_mode(DCGModeA0, DCGModeA),
    constrain_inst_vars_in_mode(DCGModeB0, DCGModeB).
parse_dcg_pred_expr_args([Term | Terms], [Arg | Args], [Mode | Modes]) :-
    Terms = [_, _ | _],
    parse_lambda_arg(Term, Arg, Mode),
    parse_dcg_pred_expr_args(Terms, Args, Modes).

%-----------------------------------------------------------------------------%
