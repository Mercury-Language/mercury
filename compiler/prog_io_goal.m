%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_goal.m.
% Main authors: fjh, zs.
%
% This module defines the predicates that parse goals.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io_goal.
:- interface.

:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io_util.

:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

    % Convert a single term into a goal.
    %
:- pred parse_goal(term::in, list(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out) is det.

    % Convert a term, possibly starting with `some [Vars]', into
    % a list of the quantified variables, a list of quantified
    % state variables, and a goal. (If the term doesn't start
    % with `some [Vars]', we return empty lists of variables.)
    %
:- pred parse_some_vars_goal(term::in, list(format_component)::in,
    maybe3(list(prog_var), list(prog_var), goal)::out,
    prog_varset::in, prog_varset::out) is det.

    % parse_pred_expression/3 converts the first argument to a :-/2
    % higher-order pred expression into a list of variables, a list
    % of their corresponding modes, and a determinism.
    %
:- pred parse_pred_expression(term::in, ho_groundness::out,
    lambda_eval_method::out, list(prog_term)::out, list(mer_mode)::out,
    determinism::out) is semidet.

    % parse_dcg_pred_expression/3 converts the first argument to a -->/2
    % higher-order DCG pred expression into a list of arguments, a list
    % of their corresponding modes and the two DCG argument modes, and a
    % determinism.
    % This is a variant of the higher-order pred syntax:
    %   `(pred(Var1::Mode1, ..., VarN::ModeN, DCG0Mode, DCGMode)
    %       is Det --> Goal)'.
    %
    % For `any' insts replace `pred' with `any_pred'.
    %
:- pred parse_dcg_pred_expression(term::in, ho_groundness::out,
    lambda_eval_method::out, list(prog_term)::out, list(mer_mode)::out,
    determinism::out) is semidet.

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
    % For `any' insts replace `func' with `any_func'.
    %
:- pred parse_func_expression(term::in, ho_groundness::out,
    lambda_eval_method::out, list(prog_term)::out, list(mer_mode)::out,
    determinism::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
:- import_module char.
:- import_module maybe.
:- import_module pair.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

parse_goal(Term, ContextPieces, MaybeGoal, !VarSet) :-
    % We could do some error-checking here, but all errors are picked up
    % in either the type-checker or parser anyway.

    % First, get the goal context.
    (
        Term = term.functor(_, _, Context)
    ;
        Term = term.variable(_, Context)
    ),
    % We just check if it matches the appropriate pattern for one of the
    % builtins. If it doesn't match any of the builtins, then it's just
    % a predicate call.
    (
        % Check for builtins...
        Term = term.functor(term.atom(Name), Args, Context),
        parse_goal_2(Name, Args, Context, ContextPieces, MaybeGoalPrime,
            !VarSet)
    ->
        MaybeGoal = MaybeGoalPrime
    ;
        % It's not a builtin.
        term.coerce(Term, ArgsTerm),
        % Check for predicate calls.
        ( try_parse_sym_name_and_args(ArgsTerm, SymName, Args) ->
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
    list(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out) is semidet.

parse_goal_2(Functor, Args, Context, ContextPieces, MaybeGoal, !VarSet) :-
    % Since (A -> B) has different semantics in standard Prolog
    % (A -> B ; fail) than it does in NU-Prolog or Mercury (A -> B ; true),
    % for the moment we'll just disallow it.
    % For consistency we also disallow if-then without the else.

    % XXX We should update ContextPieces as we recurse down.
    (
        Functor = "true",
        Args = [],
        MaybeGoal = ok1(true_expr - Context)
    ;
        Functor = "fail",
        Args = [],
        MaybeGoal = ok1(fail_expr - Context)
    ;
        Functor = "=",
        Args = [ATerm0, BTerm0],
        term.coerce(ATerm0, ATerm),
        term.coerce(BTerm0, BTerm),
        MaybeGoal = ok1(unify_expr(ATerm, BTerm, purity_pure) - Context)
    ;
        Functor = ",",
        Args = [ATerm, BTerm],
        parse_goal(ATerm, ContextPieces, MaybeAGoal, !VarSet),
        parse_goal(BTerm, ContextPieces, MaybeBGoal, !VarSet),
        (
            MaybeAGoal = ok1(AGoal),
            MaybeBGoal = ok1(BGoal)
        ->
            MaybeGoal = ok1(conj_expr(AGoal, BGoal) - Context)
        ;
            ASpecs = get_any_errors1(MaybeAGoal),
            BSpecs = get_any_errors1(MaybeBGoal),
            MaybeGoal = error1(ASpecs ++ BSpecs)
        )
    ;
        Functor = "&",
        Args = [ATerm, BTerm],
        parse_goal(ATerm, ContextPieces, MaybeAGoal, !VarSet),
        parse_goal(BTerm, ContextPieces, MaybeBGoal, !VarSet),
        (
            MaybeAGoal = ok1(AGoal),
            MaybeBGoal = ok1(BGoal)
        ->
            MaybeGoal = ok1(par_conj_expr(AGoal, BGoal) - Context)
        ;
            ASpecs = get_any_errors1(MaybeAGoal),
            BSpecs = get_any_errors1(MaybeBGoal),
            MaybeGoal = error1(ASpecs ++ BSpecs)
        )
    ;
        Functor = ";",
        Args = [ATerm, BTerm],
        ( ATerm = term.functor(term.atom("->"), [XTerm, YTerm], _Context) ->
            parse_some_vars_goal(XTerm, ContextPieces, MaybeXGoal, !VarSet),
            parse_goal(YTerm, ContextPieces, MaybeYGoal, !VarSet),
            parse_goal(BTerm, ContextPieces, MaybeBGoal, !VarSet),
            (
                MaybeXGoal = ok3(Vars, StateVars, XGoal),
                MaybeYGoal = ok1(YGoal),
                MaybeBGoal = ok1(BGoal)
            ->
                Goal = if_then_else_expr(Vars, StateVars, XGoal, YGoal, BGoal)
                    - Context,
                MaybeGoal = ok1(Goal)
            ;
                XSpecs = get_any_errors3(MaybeXGoal),
                YSpecs = get_any_errors1(MaybeYGoal),
                BSpecs = get_any_errors1(MaybeBGoal),
                MaybeGoal = error1(XSpecs ++ YSpecs ++ BSpecs)
            )
        ;
            parse_goal(ATerm, ContextPieces, MaybeAGoal, !VarSet),
            parse_goal(BTerm, ContextPieces, MaybeBGoal, !VarSet),
            (
                MaybeAGoal = ok1(AGoal),
                MaybeBGoal = ok1(BGoal)
            ->
                MaybeGoal = ok1(disj_expr(AGoal, BGoal) - Context)
            ;
                ASpecs = get_any_errors1(MaybeAGoal),
                BSpecs = get_any_errors1(MaybeBGoal),
                MaybeGoal = error1(ASpecs ++ BSpecs)
            )
        )
    ;
        Functor = "else",
        Args = [IfTerm, CTerm],
        (
            IfTerm = term.functor(term.atom("if"),
                [term.functor(term.atom("then"), [ATerm, BTerm], _)], _)
        ->
            parse_some_vars_goal(ATerm, ContextPieces, MaybeAGoal, !VarSet),
            parse_goal(BTerm, ContextPieces, MaybeBGoal, !VarSet),
            parse_goal(CTerm, ContextPieces, MaybeCGoal, !VarSet),
            (
                MaybeAGoal = ok3(Vars, StateVars, AGoal),
                MaybeBGoal = ok1(BGoal),
                MaybeCGoal = ok1(CGoal)
            ->
                Goal = if_then_else_expr(Vars, StateVars, AGoal, BGoal, CGoal)
                    - Context,
                MaybeGoal = ok1(Goal)
            ;
                ASpecs = get_any_errors3(MaybeAGoal),
                BSpecs = get_any_errors1(MaybeBGoal),
                CSpecs = get_any_errors1(MaybeCGoal),
                MaybeGoal = error1(ASpecs ++ BSpecs ++ CSpecs)
            )
        ;
            % `else' can also be part of a `try' goal.
            parse_else_then_try_term(
                term.functor(term.atom("else"), [IfTerm, CTerm], Context),
                [], no, Context, ContextPieces, MaybeGoal, !VarSet)
        )
    ;
        Functor = "then",
        Args = [TryTerm, ThenTerm],
        parse_then_try_term(
            term.functor(atom("then"), [TryTerm, ThenTerm], Context),
            no, [], no, Context, ContextPieces, MaybeGoal, !VarSet)
    ;
        Functor = "catch",
        Args = [ElseThenTryTerm, CatchTerm],
        parse_catch_then_try_term(
            term.functor(atom("catch"), [ElseThenTryTerm, CatchTerm], Context),
            no, Context, ContextPieces, MaybeGoal, !VarSet)
    ;
        Functor = "catch_any",
        Args = [TermA, ArrowTerm],
        parse_catch_any_term(ArrowTerm, Context, ContextPieces,
            MaybeCatchAnyExpr, !VarSet),
        (
            MaybeCatchAnyExpr = ok1(CatchAnyExpr),
            ( TermA = term.functor(atom("catch"), _, _) ->
                parse_catch_then_try_term(TermA, yes(CatchAnyExpr),
                    Context, ContextPieces, MaybeGoal, !VarSet)
            ;
                parse_else_then_try_term(TermA, [], yes(CatchAnyExpr),
                    Context, ContextPieces, MaybeGoal, !VarSet)
            )
        ;
            MaybeCatchAnyExpr = error1(Specs),
            MaybeGoal = error1(Specs)
        )
    ;
        ( Functor = "not"
        ; Functor = "\\+"
        ),
        Args = [ATerm],
        parse_goal(ATerm, ContextPieces, MaybeAGoal, !VarSet),
        (
            MaybeAGoal = ok1(AGoal),
            MaybeGoal = ok1(not_expr(AGoal) - Context)
        ;
            MaybeAGoal = error1(_),
            MaybeGoal = MaybeAGoal
        )
    ;
        Functor = "all",
        Args = [QVarsTerm, SubTerm],
        % Extract any state variables in the quantifier.
        varset.coerce(!.VarSet, GenericVarSet),
        parse_quantifier_vars(QVarsTerm, GenericVarSet, ContextPieces,
            MaybeStateVarsAndVars),
        % XXX We should update ContextPieces, instead of supplying [].
        parse_goal(SubTerm, [], MaybeSubGoal, !VarSet),
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
                GoalExpr = all_expr(Vars,
                    all_state_vars_expr(StateVars, SubGoal)
                    - SubContext)
            ),
            Goal = GoalExpr - Context,
            MaybeGoal = ok1(Goal)
        ;
            VarsSpecs = get_any_errors2(MaybeStateVarsAndVars),
            SubGoalSpecs = get_any_errors1(MaybeSubGoal),
            MaybeGoal = error1(VarsSpecs ++ SubGoalSpecs)
        )
    ;
        Functor = "<=",
        Args = [ATerm, BTerm],
        parse_goal(ATerm, ContextPieces, MaybeAGoal, !VarSet),
        parse_goal(BTerm, ContextPieces, MaybeBGoal, !VarSet),
        (
            MaybeAGoal = ok1(AGoal),
            MaybeBGoal = ok1(BGoal)
        ->
            MaybeGoal = ok1(implies_expr(BGoal, AGoal) - Context)
        ;
            ASpecs = get_any_errors1(MaybeAGoal),
            BSpecs = get_any_errors1(MaybeBGoal),
            MaybeGoal = error1(ASpecs ++ BSpecs)
        )
    ;
        Functor = "=>",
        Args = [ATerm, BTerm],
        parse_goal(ATerm, ContextPieces, MaybeAGoal, !VarSet),
        parse_goal(BTerm, ContextPieces, MaybeBGoal, !VarSet),
        (
            MaybeAGoal = ok1(AGoal),
            MaybeBGoal = ok1(BGoal)
        ->
            MaybeGoal = ok1(implies_expr(AGoal, BGoal) - Context)
        ;
            ASpecs = get_any_errors1(MaybeAGoal),
            BSpecs = get_any_errors1(MaybeBGoal),
            MaybeGoal = error1(ASpecs ++ BSpecs)
        )
    ;
        Functor = "<=>",
        Args = [ATerm, BTerm],
        parse_goal(ATerm, ContextPieces, MaybeAGoal, !VarSet),
        parse_goal(BTerm, ContextPieces, MaybeBGoal, !VarSet),
        (
            MaybeAGoal = ok1(AGoal),
            MaybeBGoal = ok1(BGoal)
        ->
            MaybeGoal = ok1(equivalent_expr(AGoal, BGoal) - Context)
        ;
            ASpecs = get_any_errors1(MaybeAGoal),
            BSpecs = get_any_errors1(MaybeBGoal),
            MaybeGoal = error1(ASpecs ++ BSpecs)
        )
    ;
        Functor = "some",
        Args = [QVarsTerm, SubTerm],
        % Extract any state variables in the quantifier.
        UpdatedContextPieces = ContextPieces ++ [lower_case_next_if_not_first,
            words("In first argument of"), quote("some"), suffix(":")],
        varset.coerce(!.VarSet, GenericVarSet),
        parse_quantifier_vars(QVarsTerm, GenericVarSet, UpdatedContextPieces,
            MaybeStateVarsAndVars),
        parse_goal(SubTerm, ContextPieces, MaybeSubGoal, !VarSet),
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
                GoalExpr = some_expr(Vars,
                    some_state_vars_expr(StateVars, SubGoal)
                    - SubContext)
            ),
            Goal = GoalExpr - Context,
            MaybeGoal = ok1(Goal)
        ;
            VarsSpecs = get_any_errors2(MaybeStateVarsAndVars),
            SubGoalSpecs = get_any_errors1(MaybeSubGoal),
            MaybeGoal = error1(VarsSpecs ++ SubGoalSpecs)
        )
    ;
        Functor = "trace",
        Args = [ParamsTerm, SubTerm],
        varset.coerce(!.VarSet, GenericVarSet),
        parse_trace_params(GenericVarSet, Context, ParamsTerm, MaybeParams),
        parse_goal(SubTerm, ContextPieces, MaybeSubGoal, !VarSet),
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
                MaybeComponents = error4(Specs),
                MaybeGoal = error1(Specs)
            )
        ;
            ParamsSpecs = get_any_errors1(MaybeParams),
            SubGoalSpecs = get_any_errors1(MaybeSubGoal),
            MaybeGoal = error1(ParamsSpecs ++ SubGoalSpecs)
        )
    ;
        Functor = "atomic",
        Args = [ParamsTerm, SubTerm],
        varset.coerce(!.VarSet, GenericVarSet),
        parse_atomic_params(Context, ParamsTerm, GenericVarSet, MaybeParams),
        parse_atomic_subexpr(SubTerm, MaybeSubGoals, !VarSet),
        (
            MaybeParams = ok1(Params),
            MaybeSubGoals = ok2(MainGoal, OrElseGoals)
        ->
            convert_atomic_params(ParamsTerm, Params, MaybeComponents),
            (
                MaybeComponents = ok3(Outer, Inner, MaybeOutputVars),
                GoalExpr = atomic_expr(Outer, Inner, MaybeOutputVars, MainGoal,
                    OrElseGoals),
                MaybeGoal = ok1(GoalExpr - Context)
            ;
                MaybeComponents = error3(Specs),
                MaybeGoal = error1(Specs)
            )
        ;
            ParamsSpecs = get_any_errors1(MaybeParams),
            SubGoalSpecs = get_any_errors2(MaybeSubGoals),
            MaybeGoal = error1(ParamsSpecs ++ SubGoalSpecs)
        )
    ;
        ( Functor = "promise_equivalent_solutions"
        ; Functor = "promise_equivalent_solution_sets"
        ),
        Args = [VarsTerm, SubTerm],
        varset.coerce(!.VarSet, GenericVarSet),
        parse_vars_and_state_vars(VarsTerm, GenericVarSet, ContextPieces,
            MaybeVars),
        parse_goal(SubTerm, ContextPieces, MaybeSubGoal, !VarSet),
        (
            MaybeVars = ok4(Vars0, StateVars0, DotSVars0, ColonSVars0),
            MaybeSubGoal = ok1(SubGoal)
        ->
            list.map(term.coerce_var, Vars0, Vars),
            list.map(term.coerce_var, StateVars0, StateVars),
            list.map(term.coerce_var, DotSVars0, DotSVars),
            list.map(term.coerce_var, ColonSVars0, ColonSVars),
            (
                Functor = "promise_equivalent_solutions",
                MaybeGoal = ok1(promise_equivalent_solutions_expr(Vars,
                    StateVars, DotSVars, ColonSVars, SubGoal) - Context)
            ;
                Functor = "promise_equivalent_solution_sets",
                MaybeGoal = ok1(promise_equivalent_solution_sets_expr(Vars,
                    StateVars, DotSVars, ColonSVars, SubGoal) - Context)
            )
        ;
            VarsSpecs = get_any_errors4(MaybeVars),
            SubGoalSpecs = get_any_errors1(MaybeSubGoal),
            MaybeGoal = error1(VarsSpecs ++ SubGoalSpecs)
        )
    ;
        Functor = "arbitrary",
        Args = [VarsTerm, SubTerm],
        varset.coerce(!.VarSet, GenericVarSet),
        parse_vars_and_state_vars(VarsTerm, GenericVarSet, ContextPieces,
            MaybeVars),
        parse_goal(SubTerm, ContextPieces, MaybeSubGoal, !VarSet),
        (
            MaybeVars = ok4(Vars0, StateVars0, DotSVars0, ColonSVars0),
            MaybeSubGoal = ok1(SubGoal)
        ->
            list.map(term.coerce_var, Vars0, Vars),
            list.map(term.coerce_var, StateVars0, StateVars),
            list.map(term.coerce_var, DotSVars0, DotSVars),
            list.map(term.coerce_var, ColonSVars0, ColonSVars),
            MaybeGoal = ok1(promise_equivalent_solution_arbitrary_expr(Vars,
                StateVars, DotSVars, ColonSVars, SubGoal) - Context)
        ;
            VarsSpecs = get_any_errors4(MaybeVars),
            SubGoalSpecs = get_any_errors1(MaybeSubGoal),
            MaybeGoal = error1(VarsSpecs ++ SubGoalSpecs)
        )
    ;
        (
            Functor = "promise_pure",
            Purity = purity_pure
        ;
            Functor = "promise_semipure",
            Purity = purity_semipure
        ;
            Functor = "promise_impure",
            Purity = purity_impure
        ),
        Args = [SubTerm],
        parse_goal(SubTerm, ContextPieces, MaybeSubGoal, !VarSet),
        (
            MaybeSubGoal = ok1(SubGoal),
            Goal = promise_purity_expr(Purity, SubGoal) - Context,
            MaybeGoal = ok1(Goal)
        ;
            MaybeSubGoal = error1(Specs),
            MaybeGoal = error1(Specs)
        )
    ;
        (
            Functor = "require_det",
            Detism = detism_det
        ;
            Functor = "require_semidet",
            Detism = detism_semi
        ;
            Functor = "require_multi",
            Detism = detism_multi
        ;
            Functor = "require_nondet",
            Detism = detism_non
        ;
            Functor = "require_cc_multi",
            Detism = detism_cc_multi
        ;
            Functor = "require_cc_nondet",
            Detism = detism_cc_non
        ;
            Functor = "require_erroneous",
            Detism = detism_erroneous
        ;
            Functor = "require_failure",
            Detism = detism_failure
        ),
        Args = [SubTerm],
        parse_goal(SubTerm, ContextPieces, MaybeSubGoal, !VarSet),
        (
            MaybeSubGoal = ok1(SubGoal),
            Goal = require_detism_expr(Detism, SubGoal) - Context,
            MaybeGoal = ok1(Goal)
        ;
            MaybeSubGoal = error1(Specs),
            MaybeGoal = error1(Specs)
        )
    ;
        Functor = "require_complete_switch",
        Args = [VarsTerm, SubTerm],
        varset.coerce(!.VarSet, GenericVarSet),
        parse_vars(VarsTerm, GenericVarSet, ContextPieces, MaybeVars),
        parse_goal(SubTerm, ContextPieces, MaybeSubGoal, !VarSet),
        (
            MaybeVars = ok1(Vars0),
            MaybeSubGoal = ok1(SubGoal)
        ->
            parse_one_var_list(Vars0, SubGoal, ContextPieces, Functor,
                MaybeVar),
            (
                MaybeVar = ok1(Var),
                MaybeGoal = ok1(require_complete_switch_expr(Var, SubGoal)
                    - Context)
            ;
                MaybeVar = error1(RCSSpecs),
                MaybeGoal = error1(RCSSpecs)
            )
        ;
            VarsSpecs = get_any_errors1(MaybeVars),
            SubGoalSpecs = get_any_errors1(MaybeSubGoal),
            MaybeGoal = error1(VarsSpecs ++ SubGoalSpecs)
        )
    ;
        (
            Functor = "require_switch_arms_det",
            Detism = detism_det
        ;
            Functor = "require_switch_arms_semidet",
            Detism = detism_semi
        ;
            Functor = "require_switch_arms_multi",
            Detism = detism_multi
        ;
            Functor = "require_switch_arms_nondet",
            Detism = detism_non
        ;
            Functor = "require_switch_arms_cc_multi",
            Detism = detism_cc_multi
        ;
            Functor = "require_switch_arms_cc_nondet",
            Detism = detism_cc_non
        ;
            Functor = "require_switch_arms_erroneous",
            Detism = detism_erroneous
        ;
            Functor = "require_switch_arms_failure",
            Detism = detism_failure
        ),
        Args = [VarsTerm, SubTerm],
        varset.coerce(!.VarSet, GenericVarSet),
        parse_vars(VarsTerm, GenericVarSet, ContextPieces, MaybeVars),
        parse_goal(SubTerm, ContextPieces, MaybeSubGoal, !VarSet),
        (
            MaybeVars = ok1(Vars0),
            MaybeSubGoal = ok1(SubGoal)
        ->
            parse_one_var_list(Vars0, SubGoal, ContextPieces, Functor,
                MaybeVar),
            (
                MaybeVar = ok1(Var),
                MaybeGoal = ok1(require_switch_arms_detism_expr(Var,
                    Detism, SubGoal) - Context)
            ;
                MaybeVar = error1(RCSSpecs),
                MaybeGoal = error1(RCSSpecs)
            )
        ;
            VarsSpecs = get_any_errors1(MaybeVars),
            SubGoalSpecs = get_any_errors1(MaybeSubGoal),
            MaybeGoal = error1(VarsSpecs ++ SubGoalSpecs)
        )
    ;
        (
            Functor = "impure",
            Purity = purity_impure
        ;
            Functor = "semipure",
            Purity = purity_semipure
        ),
        Args = [SubTerm],
        parse_goal(SubTerm, ContextPieces, MaybeSubGoal, !VarSet),
        (
            MaybeSubGoal = ok1(SubGoal),
            SubGoal = SubGoalExpr - _SubContext,
            ( SubGoalExpr = call_expr(Pred, CallArgs, purity_pure) ->
                MaybeGoal = ok1(call_expr(Pred, CallArgs, Purity) - Context)
            ; SubGoalExpr = unify_expr(ProgTerm1, ProgTerm2, purity_pure) ->
                MaybeGoal = ok1(unify_expr(ProgTerm1, ProgTerm2, Purity)
                    - Context)
            ;
                % Inappropriate placement of an impurity marker, so we treat
                % it like a predicate call. typecheck.m prints out something
                % descriptive for these errors.
                %
                % XXX we could return MaybeGoal = error1 here.
                purity_name(Purity, PurityString),
                term.coerce(SubTerm, CoercedSubTerm),
                GoalExpr = call_expr(unqualified(PurityString),
                    [CoercedSubTerm], purity_pure),
                MaybeGoal = ok1(GoalExpr - Context)
            )
        ;
            MaybeSubGoal = error1(_),
            MaybeGoal = MaybeSubGoal
        )
    ;
        Functor = "event",
        Args = [SubTerm],
        parse_goal(SubTerm, ContextPieces, MaybeSubGoal, !VarSet),
        (
            MaybeSubGoal = ok1(SubGoal),
            ( SubGoal = call_expr(SymName, CallArgs, Purity) - SubContext ->
                (
                    SymName = unqualified(EventName),
                    Purity = purity_pure
                ->
                    Goal = event_expr(EventName, CallArgs) - Context,
                    MaybeGoal = ok1(Goal)
                ;
                    some [!Specs] (
                        !:Specs = [],
                        (
                            SymName = unqualified(_)
                        ;
                            SymName = qualified(_, _),
                            QualPieces = ContextPieces ++
                                [lower_case_next_if_not_first,
                                words("Error: event name"),
                                words("must not be qualified."), nl],
                            QualSpec = error_spec(severity_error,
                                phase_term_to_parse_tree,
                                [simple_msg(SubContext,
                                    [always(QualPieces)])]),
                            !:Specs = [QualSpec | !.Specs]
                        ),
                        (
                            Purity = purity_pure
                        ;
                            ( Purity = purity_semipure
                            ; Purity = purity_impure
                            ),
                            PurityPieces = ContextPieces ++
                                [lower_case_next_if_not_first,
                                words("Error: event cannot be"),
                                words("impure or semipure."), nl],
                            PuritySpec = error_spec(severity_error,
                                phase_term_to_parse_tree,
                                [simple_msg(SubContext,
                                    [always(PurityPieces)])]),
                            !:Specs = [PuritySpec | !.Specs]
                        ),
                        MaybeGoal = error1(!.Specs)
                    )
                )
            ;
                Pieces = ContextPieces ++ [lower_case_next_if_not_first,
                    words("Error: event prefix must not precede anything"),
                    words("other than a call."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(SubTerm), [always(Pieces)])]),
                MaybeGoal = error1([Spec])
            )
        ;
            MaybeSubGoal = error1(Specs),
            MaybeGoal = error1(Specs)
        )
    ;
        Functor = "is",
        Args = [ATerm0, BTerm0],
        % The following is a temporary hack to handle `is' in the parser -
        % we ought to handle it in the code generation - but then `is/2' itself
        % is a bit of a hack.
        term.coerce(ATerm0, ATerm),
        term.coerce(BTerm0, BTerm),
        MaybeGoal = ok1(unify_expr(ATerm, BTerm, purity_pure) - Context)
    ).

%-----------------------------------------------------------------------------%

:- pred parse_one_var_list(list(var(T))::in, goal::in,
    list(format_component)::in, string::in, maybe1(prog_var)::out) is det.

parse_one_var_list(Vars0, Goal, ContextPieces, ConstructName, MaybeVar) :-
    (
        Vars0 = [],
        Goal = _ - Context,
        Pieces = ContextPieces ++
            [words("Error: the first argument of"), words(ConstructName),
            words("must contain a variable."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        MaybeVar = error1([Spec])
    ;
        Vars0 = [Var0],
        term.coerce_var(Var0, Var),
        MaybeVar = ok1(Var)
    ;
        Vars0 = [_, _ | _],
        Goal = _ - Context,
        Pieces = ContextPieces ++
            [words("Error: the first argument of"), words(ConstructName),
            words("cannot contain more than one variable."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        MaybeVar = error1([Spec])
    ).

%-----------------------------------------------------------------------------%

parse_some_vars_goal(Term, ContextPieces, MaybeVarsAndGoal, !VarSet) :-
    ( Term = term.functor(term.atom("some"), [QVarsTerm, SubTerm], _Context) ->
        UpdatedContextPieces = ContextPieces ++ [lower_case_next_if_not_first,
            words("In first argument of"), quote("some"), suffix(":")],
        varset.coerce(!.VarSet, GenericVarSet),
        parse_quantifier_vars(QVarsTerm, GenericVarSet, UpdatedContextPieces,
            MaybeVars),
        GoalTerm = SubTerm
    ;
        MaybeVars = ok2([], []),
        GoalTerm = Term
    ),
    parse_goal(GoalTerm, ContextPieces, MaybeGoal, !VarSet),
    (
        MaybeVars = ok2(Vars0, StateVars0),
        MaybeGoal = ok1(Goal)
    ->
        list.map(term.coerce_var, Vars0, Vars),
        list.map(term.coerce_var, StateVars0, StateVars),
        MaybeVarsAndGoal = ok3(Vars, StateVars, Goal)
    ;
        VarsSpecs = get_any_errors2(MaybeVars),
        GoalSpecs = get_any_errors1(MaybeGoal),
        MaybeVarsAndGoal = error3(VarsSpecs ++ GoalSpecs)
    ).

%-----------------------------------------------------------------------------%

:- type trace_component
    --->    trace_component_compiletime(trace_expr(trace_compiletime))
    ;       trace_component_runtime(trace_expr(trace_runtime))
    ;       trace_component_maybe_io(prog_var)
    ;       trace_component_mutable_var(trace_mutable_var).

:- pred parse_trace_params(varset::in, context::in, term::in,
    maybe1(assoc_list(trace_component, term.context))::out) is det.

parse_trace_params(VarSet, Context, Term, MaybeComponentsContexts) :-
    ( Term = term.functor(term.atom("[]"), [], _) ->
        MaybeComponentsContexts = ok1([])
    ; Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _) ->
        parse_trace_component(VarSet, Term, HeadTerm,
            MaybeHeadComponentContext),
        parse_trace_params(VarSet, Context, TailTerm,
            MaybeTailComponentsContexts),
        (
            MaybeHeadComponentContext = ok1(HeadComponentContext),
            MaybeTailComponentsContexts = ok1(TailComponentsContexts)
        ->
            MaybeComponentsContexts =
                ok1([HeadComponentContext | TailComponentsContexts])
        ;
            HeadSpecs = get_any_errors1(MaybeHeadComponentContext),
            TailSpecs = get_any_errors1(MaybeTailComponentsContexts),
            MaybeComponentsContexts = error1(HeadSpecs ++ TailSpecs)
        )
    ;
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: invalid trace goal parameter"),
            quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeComponentsContexts = error1([Spec])
    ).

:- pred parse_trace_component(varset::in, term::in, term::in,
    maybe1(pair(trace_component, term.context))::out) is det.

parse_trace_component(VarSet, _ErrorTerm, Term, MaybeComponentContext) :-
    (
        Term = term.functor(Functor, SubTerms, Context),
        Functor = term.atom(Atom)
    ->
        (
            ( Atom = "compiletime"
            ; Atom = "compile_time"
            )
        ->
            ( SubTerms = [SubTerm] ->
                parse_trace_tree(parse_trace_compiletime(VarSet), SubTerm,
                    MaybeCompileTime),
                (
                    MaybeCompileTime = ok1(CompileTime),
                    Component = trace_component_compiletime(CompileTime),
                    MaybeComponentContext = ok1(Component - Context)
                ;
                    MaybeCompileTime = error1(Specs),
                    MaybeComponentContext = error1(Specs)
                )
            ;
                Pieces = [words("Error:"), fixed(Atom),
                    words("takes exactly one argument,"),
                    words("which should be a boolean expression"),
                    words("of compile-time tests."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeComponentContext = error1([Spec])
            )
        ;
            ( Atom = "runtime"
            ; Atom = "run_time"
            )
        ->
            ( SubTerms = [SubTerm] ->
                parse_trace_tree(parse_trace_runtime(VarSet), SubTerm,
                    MaybeRunTime),
                (
                    MaybeRunTime = ok1(RunTime),
                    Component = trace_component_runtime(RunTime),
                    MaybeComponentContext = ok1(Component - Context)
                ;
                    MaybeRunTime = error1(Specs),
                    MaybeComponentContext = error1(Specs)
                )
            ;
                Pieces = [words("Error:"), fixed(Atom),
                    words("takes exactly one argument,"),
                    words("which should be a boolean expression"),
                    words("of run-time tests."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeComponentContext = error1([Spec])
            )
        ;
            Atom = "io"
        ->
            ( SubTerms = [SubTerm] ->
                (
                    SubTerm = term.functor(term.atom("!"),
                        [term.variable(Var, _)], _)
                ->
                    term.coerce_var(Var, ProgVar),
                    Component = trace_component_maybe_io(ProgVar),
                    MaybeComponentContext = ok1(Component - Context)
                ;
                    Pieces = [words("Error: the argument of"), fixed(Atom),
                        words("should be a state variable."), nl],
                    Spec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(SubTerm),
                            [always(Pieces)])]),
                    MaybeComponentContext = error1([Spec])
                )
            ;
                Pieces = [words("Error:"), fixed(Atom),
                    words("takes exactly one argument,"),
                    words("which should be a state variable name."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeComponentContext = error1([Spec])
            )
        ;
            Atom = "state"
        ->
            ( SubTerms = [SubTermA, SubTermB] ->
                ( SubTermA = term.functor(term.atom(MutableName), [], _) ->
                    MaybeMutable = ok1(MutableName)
                ;
                    MutablePieces = [words("Error: the first argument of"),
                        fixed(Atom), words("should be"),
                        words("the name of a mutable variable."), nl],
                    MutableSpec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(SubTermA),
                            [always(MutablePieces)])]),
                    MaybeMutable = error1([MutableSpec])
                ),
                (
                    SubTermB = term.functor(term.atom("!"),
                        [term.variable(Var, _)], _)
                ->
                    MaybeVar = ok1(Var)
                ;
                    VarPieces = [words("Error: the second argument of"),
                        fixed(Atom), words("should be"),
                        words("a state variable name."), nl],
                    VarSpec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(SubTermB),
                            [always(VarPieces)])]),
                    MaybeVar = error1([VarSpec])
                ),
                (
                    MaybeMutable = ok1(FinalMutable),
                    MaybeVar = ok1(FinalVar)
                ->
                    term.coerce_var(FinalVar, ProgVar),
                    MutableVar = trace_mutable_var(FinalMutable, ProgVar),
                    Component = trace_component_mutable_var(MutableVar),
                    MaybeComponentContext = ok1(Component - Context)
                ;
                    VarSpecs = get_any_errors1(MaybeVar),
                    MutableSpecs = get_any_errors1(MaybeMutable),
                    MaybeComponentContext =
                        error1(VarSpecs ++ MutableSpecs)
                )
            ;
                Pieces = [words("Error:"), fixed(Atom),
                    words("takes exactly two arguments,"),
                    words("which should be"),
                    words("the name of a mutable variable"),
                    words("and a state variable name."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeComponentContext = error1([Spec])
            )
        ;
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: invalid trace goal parameter"),
                quote(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeComponentContext = error1([Spec])
        )
    ;
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: invalid trace goal parameter"),
            quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeComponentContext = error1([Spec])
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
            LSpecs = get_any_errors1(MaybeLExpr),
            RSpecs = get_any_errors1(MaybeRExpr),
            MaybeTree = error1(LSpecs ++ RSpecs)
        )
    ;
        Term = term.functor(term.atom("not"), [SubTerm], _)
    ->
        parse_trace_tree(BaseParser, SubTerm, MaybeSubExpr),
        (
            MaybeSubExpr = ok1(SubExpr)
        ->
            MaybeTree = ok1(trace_not(SubExpr))
        ;
            SubSpecs = get_any_errors1(MaybeSubExpr),
            MaybeTree = error1(SubSpecs)
        )
    ;
        BaseParser(Term, MaybeBase),
        (
            MaybeBase = ok1(Base),
            MaybeTree = ok1(trace_base(Base))
        ;
            MaybeBase = error1(Specs),
            MaybeTree = error1(Specs)
        )
    ).

:- pred parse_trace_compiletime(varset::in, term::in,
    maybe1(trace_compiletime)::out) is det.

parse_trace_compiletime(VarSet, Term, MaybeCompiletime) :-
    (
        Term = term.functor(Functor, SubTerms, TermContext),
        Functor = term.atom(Atom)
    ->
        ( Atom = "flag" ->
            ( SubTerms = [SubTerm] ->
                ( SubTerm = term.functor(term.string(FlagName), [], _) ->
                    Compiletime = trace_flag(FlagName),
                    MaybeCompiletime = ok1(Compiletime)
                ;
                    Pieces = [words("Error: compile_time parameter"),
                        quote("flag"),
                        words("takes a string as argument."), nl],
                    Spec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(TermContext, [always(Pieces)])]),
                    MaybeCompiletime = error1([Spec])
                )
            ;
                Pieces = [words("Error: compile_time parameter"),
                    quote("flag"), words("takes just one argument."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeCompiletime = error1([Spec])
            )
        ; Atom = "grade" ->
            ( SubTerms = [SubTerm] ->
                (
                    SubTerm = term.functor(term.atom(GradeName), [], _),
                    parse_trace_grade_name(GradeName, TraceGrade)
                ->
                    Compiletime = trace_grade(TraceGrade),
                    MaybeCompiletime = ok1(Compiletime)
                ;
                    solutions(valid_trace_grade_name, ValidGradeNames),
                    Pieces = [words("invalid grade test;"),
                        words("valid grade tests are")] ++
                        list_to_pieces(ValidGradeNames) ++
                        [suffix("."), nl],
                    Spec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(TermContext, [always(Pieces)])]),
                    MaybeCompiletime = error1([Spec])
                )
            ;
                Pieces = [words("Error: compile_time parameter"),
                    quote("grade"), words("takes just one argument."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeCompiletime = error1([Spec])
            )
        ; Atom = "tracelevel" ->
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
                    Pieces = [words("Error: compile_time parameter"),
                        quote("tracelevel"), words("takes just"),
                        quote("shallow"), words("or"), quote("deep"),
                        words("as argument."), nl],
                    Spec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(TermContext, [always(Pieces)])]),
                    MaybeCompiletime = error1([Spec])
                )
            ;
                Pieces = [words("Error: compile_time parameter"),
                    quote("tracelevel"),
                    words("takes just one argument."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeCompiletime = error1([Spec])
            )
        ;
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: invalid compile_time parameter"),
                quote(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeCompiletime = error1([Spec])
        )
    ;
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: invalid compile_time parameter"),
            quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeCompiletime = error1([Spec])
    ).

:- pred parse_trace_runtime(varset::in, term::in,
    maybe1(trace_runtime)::out) is det.

parse_trace_runtime(VarSet, Term, MaybeRuntime) :-
    (
        Term = term.functor(Functor, SubTerms, TermContext),
        Functor = term.atom(Atom)
    ->
        ( Atom = "env" ->
            ( SubTerms = [SubTerm] ->
                (
                    SubTerm = term.functor(SubFunctor, [], _),
                    ( SubFunctor = term.string(EnvVarName)
                    ; SubFunctor = term.atom(EnvVarName)
                    ),
                    EnvVarChars = string.to_char_list(EnvVarName),
                    list.filter(env_var_is_acceptable_char,
                        EnvVarChars, _, [])
                ->
                    Runtime = trace_envvar(EnvVarName),
                    MaybeRuntime = ok1(Runtime)
                ;
                    Pieces = [words("Error: run_time parameter"), quote("env"),
                        words("takes an identifier as argument."), nl],
                    Spec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(SubTerm),
                            [always(Pieces)])]),
                    MaybeRuntime = error1([Spec])
                )
            ;
                Pieces = [words("Error: run_time parameter"), quote("env"),
                    words("takes just one argument."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeRuntime = error1([Spec])
            )
        ;
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: invalid run_time parameter"),
                quote(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeRuntime = error1([Spec])
        )
    ;
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: invalid run_time parameter"),
            quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeRuntime = error1([Spec])
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

:- pred convert_trace_params(assoc_list(trace_component, term.context)::in,
    maybe4(maybe(trace_expr(trace_compiletime)),
        maybe(trace_expr(trace_runtime)), maybe(prog_var),
        list(trace_mutable_var))::out) is det.

convert_trace_params(Components, MaybeParams) :-
    convert_trace_params_2(Components, no, no, no, [], [], MaybeParams).

:- pred convert_trace_params_2(assoc_list(trace_component, term.context)::in,
    maybe(trace_expr(trace_compiletime))::in,
    maybe(trace_expr(trace_runtime))::in,
    maybe(prog_var)::in, list(trace_mutable_var)::in,
    list(error_spec)::in,
    maybe4(maybe(trace_expr(trace_compiletime)),
        maybe(trace_expr(trace_runtime)), maybe(prog_var),
        list(trace_mutable_var))::out) is det.

convert_trace_params_2([], MaybeCompileTime, MaybeRunTime, MaybeIO,
        MutableVars, Specs, MaybeParams) :-
    (
        Specs = [],
        MaybeParams = ok4(MaybeCompileTime, MaybeRunTime, MaybeIO, MutableVars)
    ;
        Specs = [_ | _],
        MaybeParams = error4(Specs)
    ).
convert_trace_params_2([Component - Context | ComponentsContexts],
        !.MaybeCompileTime, !.MaybeRunTime, !.MaybeIO, !.MutableVars,
        !.Specs, MaybeParams) :-
    (
        Component = trace_component_compiletime(CompileTime),
        (
            !.MaybeCompileTime = no,
            !:MaybeCompileTime = yes(CompileTime)
        ;
            !.MaybeCompileTime = yes(_),
            Pieces = [words("Duplicate compile_time trace parameter."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Component = trace_component_runtime(RunTime),
        (
            !.MaybeRunTime = no,
            !:MaybeRunTime = yes(RunTime)
        ;
            !.MaybeRunTime = yes(_),
            Pieces = [words("Duplicate run_time trace parameter."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Component = trace_component_maybe_io(IOStateVar),
        (
            !.MaybeIO = no,
            !:MaybeIO = yes(IOStateVar)
        ;
            !.MaybeIO = yes(_),
            Pieces = [words("Duplicate io trace parameter."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Component = trace_component_mutable_var(MutableVar),
        !:MutableVars = !.MutableVars ++ [MutableVar]
    ),
    convert_trace_params_2(ComponentsContexts, !.MaybeCompileTime,
        !.MaybeRunTime, !.MaybeIO, !.MutableVars, !.Specs, MaybeParams).

%-----------------------------------------------------------------------------%

:- pred parse_catch_any_term(term::in, term.context::in,
    list(format_component)::in, maybe1(catch_any_expr)::out,
    prog_varset::in, prog_varset::out) is semidet.

parse_catch_any_term(ArrowTerm, _Context, ContextPieces, MaybeCatchAny,
        !VarSet) :-
    ArrowTerm = term.functor(atom("->"), [VarTerm0, GoalTerm], TermContext),
    ( VarTerm0 = term.variable(Var0, _) ->
        term.coerce_var(Var0, Var),
        parse_goal(GoalTerm, ContextPieces, MaybeGoal, !VarSet),
        (
            MaybeGoal = ok1(Goal),
            CatchAny = catch_any_expr(Var, Goal),
            MaybeCatchAny = ok1(CatchAny)
        ;
            MaybeGoal = error1(Error),
            MaybeCatchAny = error1(Error)
        )
    ;
        Pieces = [words("Error: the argument of catch_any"),
            words("should be a variable."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(TermContext, [always(Pieces)])]),
        MaybeCatchAny = error1([Spec])
    ).

:- pred parse_catch_then_try_term(term::in, maybe(catch_any_expr)::in,
    term.context::in, list(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out) is semidet.

parse_catch_then_try_term(CatchElseThenTryTerm, MaybeCatchAnyExpr,
        Context, ContextPieces, MaybeGoal, !VarSet) :-
    CatchElseThenTryTerm = term.functor(atom("catch"), [TermA, TermB], _),
    parse_sub_catch_terms(TermB, Context, ContextPieces, MaybeCatches,
         !VarSet),
    (
        MaybeCatches = ok1(Catches),
        parse_else_then_try_term(TermA, Catches, MaybeCatchAnyExpr,
            Context, ContextPieces, MaybeGoal, !VarSet)
    ;
        MaybeCatches = error1(Error),
        MaybeGoal = error1(Error)
    ).

:- pred parse_sub_catch_terms(term::in, term.context::in,
    list(format_component)::in, maybe1(list(catch_expr))::out,
    prog_varset::in, prog_varset::out) is semidet.

parse_sub_catch_terms(Term, Context, ContextPieces, MaybeCatches, !VarSet) :-
    ( Term = functor(atom("catch"), [CatchArrowTerm, SubTerm], _) ->
        parse_catch_arrow_term(CatchArrowTerm, Context, ContextPieces,
            MaybeCatch, !VarSet),
        (
            MaybeCatch = ok1(Catch),
            parse_sub_catch_terms(SubTerm, Context, ContextPieces,
                MaybeCatches0, !VarSet),
            (
                MaybeCatches0 = ok1(Catches0),
                MaybeCatches = ok1([Catch | Catches0])
            ;
                MaybeCatches0 = error1(Error),
                MaybeCatches = error1(Error)
            )
        ;
            MaybeCatch = error1(Error),
            MaybeCatches = error1(Error)
        )
    ;
        parse_catch_arrow_term(Term, Context, ContextPieces, MaybeCatch,
            !VarSet),
        (
            MaybeCatch = ok1(Catch),
            MaybeCatches = ok1([Catch])
        ;
            MaybeCatch = error1(Error),
            MaybeCatches = error1(Error)
        )
    ).

:- pred parse_catch_arrow_term(term::in, term.context::in,
    list(format_component)::in, maybe1(catch_expr)::out,
    prog_varset::in, prog_varset::out) is semidet.

parse_catch_arrow_term(CatchArrowTerm, _Context, ContextPieces, MaybeCatch,
        !VarSet) :-
    CatchArrowTerm = term.functor(atom("->"), [PatternTerm0, GoalTerm], _),
    term.coerce(PatternTerm0, PatternTerm),
    parse_goal(GoalTerm, ContextPieces, MaybeGoal, !VarSet),
    (
        MaybeGoal = ok1(Goal),
        Catch = catch_expr(PatternTerm, Goal),
        MaybeCatch = ok1(Catch)
    ;
        MaybeGoal = error1(Error),
        MaybeCatch = error1(Error)
    ).

:- pred parse_else_then_try_term(term::in, list(catch_expr)::in,
    maybe(catch_any_expr)::in, term.context::in, list(format_component)::in,
    maybe1(goal)::out, prog_varset::in, prog_varset::out) is semidet.

parse_else_then_try_term(Term, CatchExprs, MaybeCatchAnyExpr,
        Context, ContextPieces, MaybeGoal, !VarSet) :-
    % `else' part may or may not exist in `try' goals.
    ( Term = term.functor(term.atom("else"), [ThenTerm, ElseTerm], _) ->
        parse_goal(ElseTerm, ContextPieces, MaybeElseGoal0, !VarSet),
        (
            MaybeElseGoal0 = ok1(ElseGoal),
            parse_then_try_term(ThenTerm, yes(ElseGoal), CatchExprs,
                MaybeCatchAnyExpr, Context, ContextPieces, MaybeGoal, !VarSet)
        ;
            MaybeElseGoal0 = error1(Specs),
            MaybeGoal = error1(Specs)
        )
    ;
        parse_then_try_term(Term, no, CatchExprs, MaybeCatchAnyExpr,
            Context, ContextPieces, MaybeGoal, !VarSet)
    ).

:- pred parse_then_try_term(term::in, maybe(goal)::in, list(catch_expr)::in,
    maybe(catch_any_expr)::in, term.context::in, list(format_component)::in,
    maybe1(goal)::out, prog_varset::in, prog_varset::out) is semidet.

parse_then_try_term(ThenTryTerm, MaybeElse, CatchExprs, MaybeCatchAnyExpr,
        Context, ContextPieces, MaybeGoal, !VarSet) :-
    ThenTryTerm = term.functor(term.atom("then"), [TryTerm, ThenTerm], _),
    TryTerm = term.functor(term.atom("try"), [ParamsTerm, TryGoalTerm], _),

    varset.coerce(!.VarSet, GenericVarSet),
    parse_try_params(GenericVarSet, Context, ParamsTerm, MaybeParams),
    parse_goal(TryGoalTerm, ContextPieces, MaybeTryGoal, !VarSet),
    parse_goal(ThenTerm, ContextPieces, MaybeThenGoal, !VarSet),
    (
        MaybeParams = ok1(Params),
        MaybeTryGoal = ok1(TryGoal),
        MaybeThenGoal = ok1(ThenGoal)
    ->
        convert_try_params(Params, MaybeComponents),
        (
            MaybeComponents = ok1(MaybeIO),
            GoalExpr = try_expr(MaybeIO, TryGoal, ThenGoal, MaybeElse,
                CatchExprs, MaybeCatchAnyExpr),
            MaybeGoal = ok1(GoalExpr - Context)
        ;
            MaybeComponents = error1(Specs),
            MaybeGoal = error1(Specs)
        )
    ;
        ParamsSpecs = get_any_errors1(MaybeParams),
        TryGoalSpecs = get_any_errors1(MaybeTryGoal),
        ThenGoalSpecs = get_any_errors1(MaybeThenGoal),
        MaybeGoal = error1(ParamsSpecs ++ TryGoalSpecs ++ ThenGoalSpecs)
    ).

:- type try_component
    --->    try_component_maybe_io(prog_var).

:- pred parse_try_params(varset::in, context::in, term::in,
    maybe1(assoc_list(try_component, term.context))::out) is det.

parse_try_params(VarSet, Context, Term, MaybeComponentsContexts) :-
    ( Term = term.functor(term.atom("[]"), [], _) ->
        MaybeComponentsContexts = ok1([])
    ; Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _) ->
        parse_try_component(VarSet, Term, HeadTerm,
            MaybeHeadComponentContext),
        parse_try_params(VarSet, Context, TailTerm,
            MaybeTailComponentsContexts),
        (
            MaybeHeadComponentContext = ok1(HeadComponentContext),
            MaybeTailComponentsContexts = ok1(TailComponentsContexts)
        ->
            MaybeComponentsContexts =
                ok1([HeadComponentContext | TailComponentsContexts])
        ;
            HeadSpecs = get_any_errors1(MaybeHeadComponentContext),
            TailSpecs = get_any_errors1(MaybeTailComponentsContexts),
            MaybeComponentsContexts = error1(HeadSpecs ++ TailSpecs)
        )
    ;
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: invalid try goal parameter"),
            quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeComponentsContexts = error1([Spec])
    ).

:- pred parse_try_component(varset::in, term::in, term::in,
    maybe1(pair(try_component, term.context))::out) is det.

parse_try_component(VarSet, _ErrorTerm, Term, MaybeComponentContext) :-
    (
        Term = term.functor(Functor, SubTerms, Context),
        Functor = term.atom(Atom)
    ->
        ( Atom = "io" ->
            ( SubTerms = [SubTerm] ->
                (
                    SubTerm = term.functor(term.atom("!"),
                        [term.variable(Var, _)], _)
                ->
                    term.coerce_var(Var, ProgVar),
                    Component = try_component_maybe_io(ProgVar),
                    MaybeComponentContext = ok1(Component - Context)
                ;
                    Pieces = [words("Error: the argument of"), fixed(Atom),
                        words("should be a state variable."), nl],
                    Spec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(SubTerm),
                            [always(Pieces)])]),
                    MaybeComponentContext = error1([Spec])
                )
            ;
                Pieces = [words("Error:"), fixed(Atom),
                    words("takes exactly one argument,"),
                    words("which should be a state variable name."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeComponentContext = error1([Spec])
            )
        ;
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: invalid try goal parameter"),
                quote(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeComponentContext = error1([Spec])
        )
    ;
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: invalid try goal parameter"),
            quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeComponentContext = error1([Spec])
    ).

:- pred convert_try_params(assoc_list(try_component, term.context)::in,
    maybe1(maybe(prog_var))::out) is det.

convert_try_params(Components, MaybeParams) :-
    convert_try_params_2(Components, no, [], MaybeParams).

:- pred convert_try_params_2(assoc_list(try_component, term.context)::in,
    maybe(prog_var)::in, list(error_spec)::in,
    maybe1(maybe(prog_var))::out) is det.

convert_try_params_2([], MaybeIO, Specs, MaybeParams) :-
    (
        Specs = [],
        MaybeParams = ok1(MaybeIO)
    ;
        Specs = [_ | _],
        MaybeParams = error1(Specs)
    ).
convert_try_params_2([Component - Context | ComponentsContexts],
        !.MaybeIO, !.Specs, MaybeParams) :-
    Component = try_component_maybe_io(IOStateVar),
    (
        !.MaybeIO = no,
        !:MaybeIO = yes(IOStateVar)
    ;
        !.MaybeIO = yes(_),
        Pieces = [words("Duplicate io try parameter."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    convert_try_params_2(ComponentsContexts, !.MaybeIO, !.Specs, MaybeParams).

%-----------------------------------------------------------------------------%

:- type atomic_component
    --->    atomic_component_inner(atomic_component_state)
    ;       atomic_component_outer(atomic_component_state)
    ;       atomic_component_vars(list(prog_var)).

:- pred parse_atomic_params(context::in, term::in, varset::in,
    maybe1(assoc_list(atomic_component, term.context))::out) is det.

parse_atomic_params(Context, Term, VarSet, MaybeComponentsContexts) :-
    ( Term = term.functor(term.atom("[]"), [], _) ->
        MaybeComponentsContexts = ok1([])
    ; Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _) ->
        parse_atomic_component(Term, HeadTerm, VarSet, MaybeHeadComponent),
        parse_atomic_params(Context, TailTerm, VarSet,
            MaybeTailComponentsContexts),
        (
            MaybeHeadComponent = ok1(HeadComponent),
            MaybeTailComponentsContexts = ok1(TailComponentsContexts)
        ->
            MaybeComponentsContexts =
                ok1([HeadComponent | TailComponentsContexts])
        ;
            HeadSpecs = get_any_errors1(MaybeHeadComponent),
            TailSpecs = get_any_errors1(MaybeTailComponentsContexts),
            MaybeComponentsContexts = error1(HeadSpecs ++ TailSpecs)
        )
    ;
        (
            Term = term.functor(_, _, TermContext),
            Pieces = [words("Invalid atomic goal parameter."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeComponentsContexts = error1([Spec])
        ;
            Term = term.variable(_, TermContext),
            Pieces = [words("Expected atomic goal parameter, found variable."),
                nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeComponentsContexts = error1([Spec])
        )
    ).

:- pred parse_atomic_subterm(string::in, term::in, term::in,
    maybe1(atomic_component_state)::out) is det.

parse_atomic_subterm(Name, ErrorTerm, Term, MaybeComponentState) :-
    (
        Term = term.functor(_, SubTerms, TermContext),
        ( parse_atomic_component_state_or_pair(SubTerms, ComponentState) ->
            MaybeComponentState = ok1(ComponentState)
        ;
            Pieces = [words("Error:"), words(Name),
                words("takes exactly one argument,"),
                words("which should be a state variable"),
                words("or a pair of variables."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeComponentState = error1([Spec])
        )
    ;
        Term = term.variable(_, _TermContext),
        Pieces = [words("Error: expected atomic goal parameter,"),
            words("found variable."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeComponentState = error1([Spec])
    ).

:- pred parse_atomic_component(term::in, term::in, varset::in,
    maybe1(pair(atomic_component, term.context))::out) is det.

parse_atomic_component(ErrorTerm, Term, VarSet, MaybeComponentContext) :-
    (
        Term = term.functor(Functor, SubTerms, Context),
        ( Functor = term.atom(Atom) ->
            % XXX Make parse_atomic_subterm do the postprocessing done here.
            ( Atom = "outer" ->
                parse_atomic_subterm(Atom, ErrorTerm, Term,
                    MaybeComponentSubTerm),
                (
                    MaybeComponentSubTerm = ok1(CompTerm),
                    Component = atomic_component_outer(CompTerm),
                    MaybeComponentContext = ok1(Component - Context)
                ;
                    MaybeComponentSubTerm = error1(Specs),
                    MaybeComponentContext = error1(Specs)
                )
            ; Atom = "inner" ->
                parse_atomic_subterm(Atom, ErrorTerm, Term,
                    MaybeComponentSubTerm),
                (
                    MaybeComponentSubTerm = ok1(CompTerm),
                    Component = atomic_component_inner(CompTerm),
                    MaybeComponentContext = ok1(Component - Context)
                ;
                    MaybeComponentSubTerm = error1(Specs),
                    MaybeComponentContext = error1(Specs)
                )
            ; Atom = "vars" ->
                ( SubTerms = [SubTerm] ->
                    ContextPieces = [words("In"), quote("vars"),
                        words("specifier of atomic scope:")],
                    parse_vars(SubTerm, VarSet, ContextPieces, MaybeVars),
                    (
                        MaybeVars = ok1(Vars),
                        list.map(term.coerce_var, Vars, ProgVars),
                        Component = atomic_component_vars(ProgVars),
                        MaybeComponentContext = ok1(Component - Context)
                    ;
                        MaybeVars = error1(Specs),
                        MaybeComponentContext = error1(Specs)
                    )
                ;
                    Pieces = [words(Atom), words("takes exact one argument,"),
                        words("which should be a list of variable names."),
                        nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(Context, [always(Pieces)])]),
                    MaybeComponentContext = error1([Spec])
                )
            ;
                Pieces = [words("Invalid atomic goal parameter."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeComponentContext = error1([Spec])
            )
        ;
            Pieces = [words("Invalid atomic goal parameter."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeComponentContext = error1([Spec])
        )
    ;
        Term = term.variable(_, _Context),
        Pieces = [words("Expected atomic goal parameter, found variable."),
            nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        MaybeComponentContext = error1([Spec])
    ).

:- pred parse_atomic_component_state_or_pair(list(term)::in,
    atomic_component_state::out) is semidet.

parse_atomic_component_state_or_pair(SubTerms, State) :-
    (
        SubTerms = [Term],
        Term = term.functor(term.atom("!"), [term.variable(Var, _)], _)
    ->
        term.coerce_var(Var, ProgVar),
        State = atomic_state_var(ProgVar)
    ;
        SubTerms = [TermA, TermB],
        TermA = term.variable(VarA, _),
        TermB = term.variable(VarB, _)
    ->
        term.coerce_var(VarA, ProgVarA),
        term.coerce_var(VarB, ProgVarB),
        State = atomic_var_pair(ProgVarA, ProgVarB)
    ;
        fail
    ).

% XXX reorder the predicates above

:- pred convert_atomic_params(term::in,
    assoc_list(atomic_component, term.context)::in,
    maybe3(atomic_component_state, atomic_component_state,
        maybe(list(prog_var)))::out) is det.

convert_atomic_params(ErrorTerm, ComponentsContexts, MaybeParams) :-
    convert_atomic_params_2(get_term_context(ErrorTerm), ComponentsContexts,
        no, no, no, [], MaybeParams).

:- pred convert_atomic_params_2(term.context::in,
    assoc_list(atomic_component, term.context)::in,
    maybe(atomic_component_state)::in,
    maybe(atomic_component_state)::in,
    maybe(list(prog_var))::in, list(error_spec)::in,
    maybe3(atomic_component_state, atomic_component_state,
        maybe(list(prog_var)))::out) is det.

convert_atomic_params_2(Context, [], MaybeOuter, MaybeInner, MaybeVars,
        Specs, MaybeParams) :-
    (
        Specs = [],
        (
            MaybeOuter = yes(Outer),
            MaybeInner = yes(Inner),
            MaybeParams = ok3(Outer, Inner, MaybeVars)
        ;
            MaybeOuter = yes(_),
            MaybeInner = no,
            Pieces = [words("Atomic goal is missing"),
                words("a specification of the inner STM state."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeParams = error3([Spec])
        ;
            MaybeOuter = no,
            MaybeInner = yes(_),
            Pieces = [words("Atomic goal is missing"),
                words("a specification of the outer STM state."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeParams = error3([Spec])
        ;
            MaybeOuter = no,
            MaybeInner = no,
            Pieces = [words("Atomic goal is missing"),
                words("a specification of both"),
                words("the outer and inner STM states."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeParams = error3([Spec])
        )
    ;
        Specs = [_ | _],
        MaybeParams = error3(Specs)
    ).
convert_atomic_params_2(Context,
        [Component - CompContext | ComponentsContexts],
        !.MaybeOuter, !.MaybeInner, !.MaybeVars, !.Specs, MaybeParams) :-
    (
        Component = atomic_component_outer(Outer),
        (
            !.MaybeOuter = no,
            !:MaybeOuter = yes(Outer)
        ;
            !.MaybeOuter = yes(_),
            % XXX We should specify the duplicate parameter.
            Pieces = [words("Duplicate outer atomic parameter."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(CompContext, [always(Pieces)])]),
            !:Specs = !.Specs ++ [Spec]
        )
    ;
        Component = atomic_component_inner(Inner),
        (
            !.MaybeInner = no,
            !:MaybeInner = yes(Inner)
        ;
            !.MaybeInner = yes(_),
            % XXX We should specify the duplicate parameter.
            Pieces = [words("Duplicate inner atomic parameter."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(CompContext, [always(Pieces)])]),
            !:Specs = !.Specs ++ [Spec]
        )
    ;
        Component = atomic_component_vars(Vars),
        (
            !.MaybeVars = no,
            !:MaybeVars = yes(Vars)
        ;
            !.MaybeVars = yes(_),
            % XXX We should specify the duplicate parameter.
            Pieces = [words("Duplicate atomic vars parameter."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(CompContext, [always(Pieces)])]),
            !:Specs = !.Specs ++ [Spec]
        )
    ),
    convert_atomic_params_2(Context, ComponentsContexts,
        !.MaybeOuter, !.MaybeInner, !.MaybeVars, !.Specs, MaybeParams).

:- pred parse_atomic_subexpr(term::in, maybe2(goal, goals)::out,
    prog_varset::in, prog_varset::out) is det.

parse_atomic_subexpr(Term, MaybeSubExpr, !VarSet) :-
    parse_atomic_subgoals_as_list(Term, MaybeGoalList, !VarSet),
    (
        MaybeGoalList = ok1(GoalList),
        (
            GoalList = [],
            Pieces = [words("Error: atomic scope must have a goal."), nl],
            Context = get_term_context(Term),
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeSubExpr = error2([Spec])
        ;
            GoalList = [MainSubGoalExpr | OrElseAlternativeSubExpr],
            MaybeSubExpr = ok2(MainSubGoalExpr, OrElseAlternativeSubExpr)
        )
    ;
        MaybeGoalList = error1(Specs),
        MaybeSubExpr = error2(Specs)
    ).

:- pred parse_atomic_subgoals_as_list(term::in, maybe1(list(goal))::out,
    prog_varset::in, prog_varset::out) is det.

parse_atomic_subgoals_as_list(Term, MaybeGoals, !VarSet) :-
    (
        Term = term.functor(term.atom("or_else"), [LeftGoal, RightGoal], _)
    ->
        parse_atomic_subgoals_as_list(LeftGoal, MaybeLeftGoalList, !VarSet),
        parse_atomic_subgoals_as_list(RightGoal, MaybeRightGoalList, !VarSet),
        (
            MaybeLeftGoalList = ok1(LeftGoalList),
            MaybeRightGoalList = ok1(RightGoalList)
        ->
            MaybeGoals = ok1(LeftGoalList ++ RightGoalList)
        ;
            LeftSpecs = get_any_errors1(MaybeLeftGoalList),
            RightSpecs = get_any_errors1(MaybeRightGoalList),
            MaybeGoals = error1(LeftSpecs ++ RightSpecs)
        )
    ;
        % XXX Provide better ContextPieces.
        ContextPieces = [],
        parse_goal(Term, ContextPieces, MaybeSubGoal, !VarSet),
        (
            MaybeSubGoal = ok1(SubGoal),
            MaybeGoals = ok1([SubGoal])
        ;
            MaybeSubGoal = error1(Specs),
            MaybeGoals = error1(Specs)
        )
    ).

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

parse_pred_expression(PredTerm, Groundness, lambda_normal, Args, Modes, Det) :-
    PredTerm = term.functor(term.atom("is"), [PredArgsTerm, DetTerm], _),
    DetTerm = term.functor(term.atom(DetString), [], _),
    standard_det(DetString, Det),
    PredArgsTerm = term.functor(term.atom(Name), PredArgsList, _),
    (
        Name = "pred",
        Groundness = ho_ground
    ;
        Name = "any_pred",
        Groundness = ho_any
    ),
    parse_pred_expr_args(PredArgsList, Args, Modes),
    inst_var_constraints_are_self_consistent_in_modes(Modes).

parse_dcg_pred_expression(PredTerm, Groundness, lambda_normal, Args, Modes,
        Det) :-
    PredTerm = term.functor(term.atom("is"), [PredArgsTerm, DetTerm], _),
    DetTerm = term.functor(term.atom(DetString), [], _),
    standard_det(DetString, Det),
    PredArgsTerm = term.functor(term.atom(Name), PredArgsList, _),
    (
        Name = "pred",
        Groundness = ho_ground
    ;
        Name = "any_pred",
        Groundness = ho_any
    ),
    parse_dcg_pred_expr_args(PredArgsList, Args, Modes),
    inst_var_constraints_are_self_consistent_in_modes(Modes).

parse_func_expression(FuncTerm, Groundness, lambda_normal, Args, Modes, Det) :-
    % Parse a func expression with specified modes and determinism.
    FuncTerm = term.functor(term.atom("is"), [EqTerm, DetTerm], _),
    EqTerm = term.functor(term.atom("="), [FuncArgsTerm, RetTerm], _),
    DetTerm = term.functor(term.atom(DetString), [], _),
    standard_det(DetString, Det),
    FuncArgsTerm = term.functor(term.atom(Name), FuncArgsList, _),
    (
        Name = "func",
        Groundness = ho_ground
    ;
        Name = "any_func",
        Groundness = ho_any
    ),

    ( parse_pred_expr_args(FuncArgsList, Args0, Modes0) ->
        parse_lambda_arg(RetTerm, RetArg, RetMode),
        list.append(Args0, [RetArg], Args),
        list.append(Modes0, [RetMode], Modes),
        inst_var_constraints_are_self_consistent_in_modes(Modes)
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

parse_func_expression(FuncTerm, Groundness, lambda_normal, Args, Modes, Det) :-
    % Parse a func expression with unspecified modes and determinism.
    FuncTerm = term.functor(term.atom("="), [FuncArgsTerm, RetTerm], _),
    FuncArgsTerm = term.functor(term.atom(Name), Args0, _),
    (
        Name = "func",
        Groundness = ho_ground
    ;
        Name = "any_func",
        Groundness = ho_any
    ),

    % The argument modes default to `in', the return mode defaults to `out',
    % and the determinism defaults to `det'.
    in_mode(InMode),
    out_mode(OutMode),
    list.length(Args0, NumArgs),
    list.duplicate(NumArgs, InMode, Modes0),
    RetMode = OutMode,
    Det = detism_det,
    list.append(Modes0, [RetMode], Modes),
    inst_var_constraints_are_self_consistent_in_modes(Modes),
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
:- end_module parse_tree.prog_io_goal.
%-----------------------------------------------------------------------------%
