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
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

    % Convert a single term into a goal.
    %
:- pred parse_goal(term::in, list(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out) is det.

    % Convert a term, possibly starting with `some [Vars]', into a list
    % of the quantified variables, a list of quantified state variables,
    % and a goal. (If the term doesn't start with `some [Vars]', we return
    % empty lists of variables.)
    %
    % Exported to superhomogeneous.m for parsing if-then-else expressions.
    %
:- pred parse_some_vars_goal(term::in, list(format_component)::in,
    maybe3(list(prog_var), list(prog_var), goal)::out,
    prog_varset::in, prog_varset::out) is det.

    % parse_pred_expression/3 converts the first argument of a :-/2
    % higher-order pred expression into a list of variables, a list
    % of their corresponding modes, and a determinism.
    %
    % Exported to superhomogeneous.m for parsing lambda expressions.
    %
:- pred parse_pred_expression(term::in, ho_groundness::out,
    lambda_eval_method::out, list(prog_term)::out, list(mer_mode)::out,
    determinism::out) is semidet.

    % parse_dcg_pred_expression/3 converts the first argument of a -->/2
    % higher-order DCG pred expression into a list of arguments, a list
    % of their corresponding modes and the two DCG argument modes, and a
    % determinism.
    % This is a variant of the higher-order pred syntax:
    %   `(pred(Var1::Mode1, ..., VarN::ModeN, DCG0Mode, DCGMode)
    %       is Det --> Goal)'.
    %
    % For `any' insts replace `pred' with `any_pred'.
    %
    % Exported to superhomogeneous.m for parsing lambda expressions.
    %
:- pred parse_dcg_pred_expression(term::in, ho_groundness::out,
    lambda_eval_method::out, list(prog_term)::out, list(mer_mode)::out,
    determinism::out) is semidet.

    % parse_func_expression/3 converts the first argument of a :-/2
    % higher-order func expression into a list of arguments, a list
    % of their corresponding modes, and a determinism. The syntax
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
    % Exported to superhomogeneous.m for parsing lambda expressions.
    %
:- pred parse_func_expression(term::in, ho_groundness::out,
    lambda_eval_method::out, list(prog_term)::out, list(mer_mode)::out,
    determinism::out) is semidet.

    % apply_purity_marker_to_maybe_goal(GoalTerm, Purity,
    %   MaybeGoal0, MaybeGoal):
    %
    % Given a GoalTerm which has a purity annotation for Purity in front of it,
    % which has been parsed as MaybeGoal0, marking the Goal0 in MaybeGoal0
    % as having the given purity, if it is a goal to which purity annotations
    % are applicable.
    %
:- pred apply_purity_marker_to_maybe_goal(term::in, purity::in,
    maybe1(goal)::in, maybe1(goal)::out) is det.

    % Functions to construct error messages. Exported to prog_io_dcg.m,
    % to allow DCG and non-DCG clauses to generate identical error messages
    % in analogous situations.
    %
:- func should_have_one_goal_prefix(list(format_component),
    term.context, string) = error_spec.
:- func should_have_two_terms_infix(list(format_component),
    term.context, string) = error_spec.
:- func should_have_two_goals_infix(list(format_component),
    term.context, string) = error_spec.
:- func should_have_one_x_one_goal_prefix(list(format_component),
    term.context, string, string) = error_spec.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_term.
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
:- import_module varset.

%-----------------------------------------------------------------------------%

parse_goal(Term, ContextPieces, MaybeGoal, !VarSet) :-
    % We could do some error-checking here, but all errors are picked up
    % in either the type-checker or parser anyway.

    % First, get the goal context.
    ( Term = term.functor(_, _, Context)
    ; Term = term.variable(_, Context)
    ),
    % We just check if it matches the appropriate pattern for one of the
    % builtins. If it doesn't match any of the builtins, then it's just
    % a predicate call.
    ( if
        % Check for builtins...
        Term = term.functor(term.atom(Name), Args, Context),
        parse_non_call_goal(Name, Args, Context, ContextPieces, MaybeGoalPrime,
            !VarSet)
    then
        MaybeGoal = MaybeGoalPrime
    else
        % It's not a builtin.
        term.coerce(Term, ArgsTerm),
        % Check for predicate calls.
        ( if try_parse_sym_name_and_args(ArgsTerm, SymName, Args) then
            Goal = call_expr(Context, SymName, Args, purity_pure)
        else
            % A call to a free variable, or to a number or string.
            % Just translate it into a call to call/1 - the typechecker
            % will catch calls to numbers and strings.
            Goal = call_expr(Context, unqualified("call"), [ArgsTerm],
                purity_pure)
        ),
        MaybeGoal = ok1(Goal)
    ).

%-----------------------------------------------------------------------------%

:- pred parse_non_call_goal(string::in, list(term)::in, term.context::in,
    list(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out) is semidet.

parse_non_call_goal(Functor, Args, Context, ContextPieces, MaybeGoal,
        !VarSet) :-
    % XXX We should update ContextPieces as we recurse down.
    % XXX Should reorder the disjuncts to match (a) the order of goal kinds
    % in the goal type definition, and (b) parse_non_call_dcg_goal.
    require_switch_arms_det [Functor]
    (
        (
            Functor = "true",
            Goal = true_expr(Context)
        ;
            Functor = "fail",
            Goal = fail_expr(Context)
        ),
        (
            Args = [],
            MaybeGoal = ok1(Goal)
        ;
            Args = [_ | _],
            Spec = should_have_no_args(ContextPieces, Context, Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        ( Functor = "="
        ; Functor = "is"
        ),
        ( if Args = [TermA0, TermB0] then
            term.coerce(TermA0, TermA),
            term.coerce(TermB0, TermB),
            MaybeGoal = ok1(unify_expr(Context, TermA, TermB, purity_pure))
        else
            Spec = should_have_two_terms_infix(ContextPieces, Context,
                Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = ",",
        ( if Args = [SubGoalTermA, SubGoalTermB] then
            parse_goal(SubGoalTermA, ContextPieces, MaybeSubGoalA, !VarSet),
            parse_goal(SubGoalTermB, ContextPieces, MaybeSubGoalB, !VarSet),
            ( if
                MaybeSubGoalA = ok1(SubGoalA),
                MaybeSubGoalB = ok1(SubGoalB)
            then
                MaybeGoal = ok1(conj_expr(Context, SubGoalA, SubGoalB))
            else
                Specs = get_any_errors1(MaybeSubGoalA) ++
                    get_any_errors1(MaybeSubGoalB),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_two_goals_infix(ContextPieces, Context,
                Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "&",
        ( if Args = [SubGoalTermA, SubGoalTermB] then
            parse_goal(SubGoalTermA, ContextPieces, MaybeSubGoalA, !VarSet),
            parse_goal(SubGoalTermB, ContextPieces, MaybeSubGoalB, !VarSet),
            ( if
                MaybeSubGoalA = ok1(SubGoalA),
                MaybeSubGoalB = ok1(SubGoalB)
            then
                MaybeGoal = ok1(par_conj_expr(Context, SubGoalA, SubGoalB))
            else
                Specs = get_any_errors1(MaybeSubGoalA) ++
                    get_any_errors1(MaybeSubGoalB),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_two_goals_infix(ContextPieces, Context,
                Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = ";",
        ( if Args = [TermA, TermB] then
            ( if TermA = term.functor(term.atom("->"), [TermX, TermY], _) then
                TermC = TermX,  % The condition.
                TermT = TermY,  % The then part.
                TermE = TermB,  % The else part.
                parse_some_vars_goal(TermC, ContextPieces, MaybeGoalC,
                    !VarSet),
                parse_goal(TermT, ContextPieces, MaybeGoalT, !VarSet),
                parse_goal(TermE, ContextPieces, MaybeGoalE, !VarSet),
                ( if
                    MaybeGoalC = ok3(Vars, StateVars, GoalC),
                    MaybeGoalT = ok1(GoalT),
                    MaybeGoalE = ok1(GoalE)
                then
                    Goal = if_then_else_expr(Context, Vars, StateVars,
                        GoalC, GoalT, GoalE),
                    MaybeGoal = ok1(Goal)
                else
                    Specs = get_any_errors3(MaybeGoalC) ++
                        get_any_errors1(MaybeGoalT) ++
                        get_any_errors1(MaybeGoalE),
                    MaybeGoal = error1(Specs)
                )
            else
                parse_goal(TermA, ContextPieces, MaybeGoalA, !VarSet),
                parse_goal(TermB, ContextPieces, MaybeGoalB, !VarSet),
                ( if
                    MaybeGoalA = ok1(GoalA),
                    MaybeGoalB = ok1(GoalB)
                then
                    MaybeGoal = ok1(disj_expr(Context, GoalA, GoalB))
                else
                    Specs = get_any_errors1(MaybeGoalA) ++
                        get_any_errors1(MaybeGoalB),
                    MaybeGoal = error1(Specs)
                )
            )
        else
            % XXX This generates an error message that is appropriate
            % for goals that are intended to be disjunctions. Should we
            % instead generate a message that also talks about if-then-elses
            % using (C->T;E) syntax? It would be more complete, but also
            % more complex, and therefore potentially more confusing
            % than helpful.
            % We do the same for ";" in parse_non_call_dcg_goal.
            Spec = should_have_two_goals_infix(ContextPieces, Context,
                Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "else",
        ( if Args = [TermA, TermB] then
            ( if
                TermA = term.functor(term.atom("if"),
                    [term.functor(term.atom("then"), [TermX, TermY], _)], _)
            then
                TermC = TermX,  % The condition.
                TermT = TermY,  % The then part.
                TermE = TermB,  % The else part.
                parse_some_vars_goal(TermC, ContextPieces, MaybeGoalC,
                    !VarSet),
                parse_goal(TermT, ContextPieces, MaybeGoalT, !VarSet),
                parse_goal(TermE, ContextPieces, MaybeGoalE, !VarSet),
                ( if
                    MaybeGoalC = ok3(Vars, StateVars, GoalC),
                    MaybeGoalT = ok1(GoalT),
                    MaybeGoalE = ok1(GoalE)
                then
                    Goal = if_then_else_expr(Context, Vars, StateVars,
                        GoalC, GoalT, GoalE),
                    MaybeGoal = ok1(Goal)
                else
                    Specs = get_any_errors3(MaybeGoalC) ++
                        get_any_errors1(MaybeGoalT) ++
                        get_any_errors1(MaybeGoalE),
                    MaybeGoal = error1(Specs)
                )
            else
                % `else' can also be part of a `try' goal.
                parse_else_then_try_term(
                    term.functor(term.atom("else"), [TermA, TermB], Context),
                    [], no, Context, ContextPieces, MaybeGoal, !VarSet)
            )
        else
            % XXX This generates an error message that is appropriate
            % for goals that are intended to be if-then-elses. Should we
            % instead generate a message that also talks about try goals?
            % It would be more complete, but also more complex, and therefore
            % more likely to be confusing than helpful, since try goals
            % are *much* rarer than if-then-elses.
            Pieces = [words("Error: the "), quote("else"), words("operator"),
                words("should occur in expressions of the form"),
                quote("( if goal then goal else goal )"), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "then",
        ( if Args = [TryTerm, ThenTerm] then
            parse_then_try_term(
                term.functor(atom("then"), [TryTerm, ThenTerm], Context),
                no, [], no, Context, ContextPieces, MaybeGoal, !VarSet)
        else
            % Since there was no "else" wrapped around this use of "then",
            % it is quite likely that this may have been intended to be
            % a try goal.
            % XXX Should we list all the things that may follow
            % the initial part of a try goal?
            Pieces = [words("Error: the "), quote("then"), words("operator,"),
                words("should be used either in an expression of the form"),
                quote("( if goal then goal else goal )"), suffix(","),
                words("or in an expression of the form"),
                quote("try [try_params] main_goal then success_goal"),
                suffix(","), words("optionally followed by"),
                quote("else failure_goal"), suffix(","),
                words("which in turn may be followed by zero or more"),
                quote("catch"), words("clauses, and optionally by a single"),
                quote("catch_any"), words("clause."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "catch",
        parse_catch_then_try_term_args(Args, no, Context, ContextPieces,
            MaybeGoal, !VarSet)
    ;
        Functor = "catch_any",
        ( if Args = [TermA, ArrowTerm] then
            parse_catch_any_term(ArrowTerm, Context, ContextPieces,
                MaybeCatchAnyExpr, !VarSet),
            (
                MaybeCatchAnyExpr = ok1(CatchAnyExpr),
                ( if TermA = term.functor(atom("catch"), TermAArgs, _) then
                    parse_catch_then_try_term_args(TermAArgs,
                        yes(CatchAnyExpr), Context, ContextPieces,
                        MaybeGoal, !VarSet)
                else
                    parse_else_then_try_term(TermA, [], yes(CatchAnyExpr),
                        Context, ContextPieces, MaybeGoal, !VarSet)
                )
            ;
                MaybeCatchAnyExpr = error1(Specs),
                MaybeGoal = error1(Specs)
            )
        else
            Pieces = [words("Error: the "), quote("catch_any"),
                words("operator should be preceded by"),
                words("a try expression, with a then-clause,"),
                words("optional else-clause and zero or more catch clauses,"),
                words("and should be followed by an expression of the form"),
                quote("variable -> goal"), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeGoal = error1([Spec])
        )
    ;
        ( Functor = "not"
        ; Functor = "\\+"
        ),
        ( if Args = [ATerm] then
            parse_goal(ATerm, ContextPieces, MaybeAGoal, !VarSet),
            (
                MaybeAGoal = ok1(AGoal),
                MaybeGoal = ok1(not_expr(Context, AGoal))
            ;
                MaybeAGoal = error1(_),
                MaybeGoal = MaybeAGoal
            )
        else
            Spec = should_have_one_goal_prefix(ContextPieces, Context,
                Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        (
            Functor = "some",
            QuantType = quant_some,
            VarsContextPieces = [lower_case_next_if_not_first,
                words("In first argument of"), quote("some"), suffix(":")]
        ;
            Functor = "all",
            QuantType = quant_all,
            VarsContextPieces = [lower_case_next_if_not_first,
                words("In first argument of"), quote("all"), suffix(":")]
        ),
        % Note that both versions of VarsContextPieces should be static data;
        % factoring out their common parts would destroy this property.
        ( if Args = [QVarsTerm, SubGoalTerm] then
            varset.coerce(!.VarSet, GenericVarSet),
            UpdatedContextPieces = ContextPieces ++ VarsContextPieces,
            parse_quantifier_vars(QVarsTerm, GenericVarSet,
                UpdatedContextPieces, MaybeStateVarsAndVars),
            % XXX We should update ContextPieces, instead of supplying [].
            parse_goal(SubGoalTerm, [], MaybeSubGoal, !VarSet),
            ( if
                MaybeStateVarsAndVars = ok2(Vars0, StateVars0),
                MaybeSubGoal = ok1(SubGoal)
            then
                list.map(term.coerce_var, Vars0, Vars),
                list.map(term.coerce_var, StateVars0, StateVars),
                (
                    StateVars = [],
                    Goal1 = SubGoal
                ;
                    StateVars = [_ | _],
                    Goal1 = quant_expr(QuantType, quant_state_vars, Context,
                        StateVars, SubGoal)
                ),
                (
                    Vars = [],
                    Goal = Goal1
                ;
                    Vars = [_ | _],
                    Goal = quant_expr(QuantType, quant_ordinary_vars, Context,
                        Vars, Goal1)
                ),
                MaybeGoal = ok1(Goal)
            else
                Specs = get_any_errors2(MaybeStateVarsAndVars) ++
                    get_any_errors1(MaybeSubGoal),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_one_x_one_goal_prefix(ContextPieces, Context,
                "a list of variables", Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        ( Functor = "<="
        ; Functor = "=>"
        ; Functor = "<=>"
        ),
        ( if Args = [TermA, TermB] then
            parse_goal(TermA, ContextPieces, MaybeGoalA, !VarSet),
            parse_goal(TermB, ContextPieces, MaybeGoalB, !VarSet),
            ( if
                MaybeGoalA = ok1(GoalA),
                MaybeGoalB = ok1(GoalB)
            then
                (
                    Functor = "<=",
                    Goal = implies_expr(Context, GoalB, GoalB)
                ;
                    Functor = "=>",
                    Goal = implies_expr(Context, GoalA, GoalB)
                ;
                    Functor = "<=>",
                    Goal = equivalent_expr(Context, GoalA, GoalB)
                ),
                MaybeGoal = ok1(Goal)
            else
                Specs = get_any_errors1(MaybeGoalA) ++
                    get_any_errors1(MaybeGoalB),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_two_goals_infix(ContextPieces, Context,
                Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "trace",
        ( if Args = [ParamsTerm, SubGoalTerm] then
            varset.coerce(!.VarSet, GenericVarSet),
            parse_trace_params(GenericVarSet, Context, ParamsTerm,
                MaybeParams),
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            ( if
                MaybeParams = ok1(Params),
                MaybeSubGoal = ok1(SubGoal)
            then
                convert_trace_params(Params, MaybeComponents),
                (
                    MaybeComponents = ok4(CompileTime, RunTime, MaybeIO,
                        MutVars),
                    Goal = trace_expr(Context, CompileTime, RunTime,
                        MaybeIO, MutVars, SubGoal),
                    MaybeGoal = ok1(Goal)
                ;
                    MaybeComponents = error4(Specs),
                    MaybeGoal = error1(Specs)
                )
            else
                Specs = get_any_errors1(MaybeParams) ++
                    get_any_errors1(MaybeSubGoal),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_one_x_one_goal_prefix(ContextPieces, Context,
                "a list of trace parameters", Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "atomic",
        ( if Args = [ParamsTerm, SubGoalsTerm] then
            varset.coerce(!.VarSet, GenericVarSet),
            parse_atomic_params(Context, ParamsTerm, GenericVarSet,
                MaybeParams),
            parse_atomic_subexpr(SubGoalsTerm, MaybeSubGoals, !VarSet),
            ( if
                MaybeParams = ok1(Params),
                MaybeSubGoals = ok2(MainGoal, OrElseGoals)
            then
                convert_atomic_params(ParamsTerm, Params, MaybeComponents),
                (
                    MaybeComponents = ok3(Outer, Inner, MaybeOutputVars),
                    Goal = atomic_expr(Context, Outer, Inner, MaybeOutputVars,
                        MainGoal, OrElseGoals),
                    MaybeGoal = ok1(Goal)
                ;
                    MaybeComponents = error3(Specs),
                    MaybeGoal = error1(Specs)
                )
            else
                Specs = get_any_errors1(MaybeParams) ++
                    get_any_errors2(MaybeSubGoals),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_one_x_one_goal_prefix(ContextPieces, Context,
                "a list of atomic parameters", Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        ( Functor = "promise_equivalent_solutions"
        ; Functor = "promise_equivalent_solution_sets"
        ),
        ( if Args = [VarsTerm, SubGoalTerm] then
            varset.coerce(!.VarSet, GenericVarSet),
            parse_vars_and_state_vars(VarsTerm, GenericVarSet, ContextPieces,
                MaybeVars),
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            ( if
                MaybeVars = ok4(Vars0, StateVars0, DotSVars0, ColonSVars0),
                MaybeSubGoal = ok1(SubGoal)
            then
                list.map(term.coerce_var, Vars0, Vars),
                list.map(term.coerce_var, StateVars0, StateVars),
                list.map(term.coerce_var, DotSVars0, DotSVars),
                list.map(term.coerce_var, ColonSVars0, ColonSVars),
                (
                    Functor = "promise_equivalent_solutions",
                    Goal = promise_equivalent_solutions_expr(Context, Vars,
                        StateVars, DotSVars, ColonSVars, SubGoal),
                    MaybeGoal = ok1(Goal)
                ;
                    Functor = "promise_equivalent_solution_sets",
                    Goal = promise_equivalent_solution_sets_expr(Context, Vars,
                        StateVars, DotSVars, ColonSVars, SubGoal),
                    MaybeGoal = ok1(Goal)
                )
            else
                Specs = get_any_errors4(MaybeVars) ++
                    get_any_errors1(MaybeSubGoal),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_one_x_one_goal_prefix(ContextPieces, Context,
                "a list of variables", Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "arbitrary",
        ( if Args = [VarsTerm, SubGoalTerm] then
            varset.coerce(!.VarSet, GenericVarSet),
            parse_vars_and_state_vars(VarsTerm, GenericVarSet, ContextPieces,
                MaybeVars),
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            ( if
                MaybeVars = ok4(Vars0, StateVars0, DotSVars0, ColonSVars0),
                MaybeSubGoal = ok1(SubGoal)
            then
                list.map(term.coerce_var, Vars0, Vars),
                list.map(term.coerce_var, StateVars0, StateVars),
                list.map(term.coerce_var, DotSVars0, DotSVars),
                list.map(term.coerce_var, ColonSVars0, ColonSVars),
                Goal = promise_equivalent_solution_arbitrary_expr(Context,
                    Vars, StateVars, DotSVars, ColonSVars, SubGoal),
                MaybeGoal = ok1(Goal)
            else
                Specs = get_any_errors4(MaybeVars) ++
                    get_any_errors1(MaybeSubGoal),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_one_x_one_goal_prefix(ContextPieces, Context,
                "a list of variables", Functor),
            MaybeGoal = error1([Spec])
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
        ( if Args = [SubGoalTerm] then
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            (
                MaybeSubGoal = ok1(SubGoal),
                Goal = promise_purity_expr(Context, Purity, SubGoal),
                MaybeGoal = ok1(Goal)
            ;
                MaybeSubGoal = error1(Specs),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_one_goal_prefix(ContextPieces, Context,
                Functor),
            MaybeGoal = error1([Spec])
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
        ( if Args = [SubGoalTerm] then
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            (
                MaybeSubGoal = ok1(SubGoal),
                Goal = require_detism_expr(Context, Detism, SubGoal),
                MaybeGoal = ok1(Goal)
            ;
                MaybeSubGoal = error1(Specs),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_one_goal_prefix(ContextPieces, Context,
                Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "require_complete_switch",
        ( if Args = [VarsTerm, SubGoalTerm] then
            varset.coerce(!.VarSet, GenericVarSet),
            parse_vars(VarsTerm, GenericVarSet, ContextPieces, MaybeVars),
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            ( if
                MaybeVars = ok1(Vars0),
                MaybeSubGoal = ok1(SubGoal)
            then
                parse_one_var_list(Vars0, SubGoal, ContextPieces, Functor,
                    MaybeVar),
                (
                    MaybeVar = ok1(Var),
                    Goal = require_complete_switch_expr(Context, Var, SubGoal),
                    MaybeGoal = ok1(Goal)
                ;
                    MaybeVar = error1(RCSSpecs),
                    MaybeGoal = error1(RCSSpecs)
                )
            else
                Specs = get_any_errors1(MaybeVars) ++
                    get_any_errors1(MaybeSubGoal),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_one_x_one_goal_prefix(ContextPieces, Context,
                "a variable in a singleton list", Functor),
            MaybeGoal = error1([Spec])
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
        ( if Args = [VarsTerm, SubGoalTerm] then
            varset.coerce(!.VarSet, GenericVarSet),
            parse_vars(VarsTerm, GenericVarSet, ContextPieces, MaybeVars),
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            ( if
                MaybeVars = ok1(Vars0),
                MaybeSubGoal = ok1(SubGoal)
            then
                parse_one_var_list(Vars0, SubGoal, ContextPieces, Functor,
                    MaybeVar),
                (
                    MaybeVar = ok1(Var),
                    Goal = require_switch_arms_detism_expr(Context, Var,
                        Detism, SubGoal),
                    MaybeGoal = ok1(Goal)
                ;
                    MaybeVar = error1(RCSSpecs),
                    MaybeGoal = error1(RCSSpecs)
                )
            else
                Specs = get_any_errors1(MaybeVars) ++
                    get_any_errors1(MaybeSubGoal),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_one_x_one_goal_prefix(ContextPieces, Context,
                "a variable in a singleton list", Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        (
            Functor = "impure",
            Purity = purity_impure
        ;
            Functor = "semipure",
            Purity = purity_semipure
        ),
        ( if Args = [SubGoalTerm] then
            parse_goal(SubGoalTerm, ContextPieces, MaybeGoal0, !VarSet),
            apply_purity_marker_to_maybe_goal(SubGoalTerm, Purity,
                MaybeGoal0, MaybeGoal)
        else
            Spec = should_have_one_goal_prefix(ContextPieces, Context,
                Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "event",
        ( if Args = [SubGoalTerm] then
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            (
                MaybeSubGoal = ok1(SubGoal),
                ( if
                    SubGoal = call_expr(SubContext, SymName, CallArgs, Purity)
                then
                    ( if
                        SymName = unqualified(EventName),
                        Purity = purity_pure
                    then
                        Goal = event_expr(Context, EventName, CallArgs),
                        MaybeGoal = ok1(Goal)
                    else
                        some [!Specs] (
                            !:Specs = [],
                            (
                                SymName = unqualified(_)
                            ;
                                SymName = qualified(_, _),
                                QualPieces = ContextPieces ++
                                    [lower_case_next_if_not_first,
                                    words("Error: the event name"),
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
                                    words("Error: an event cannot be"),
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
                else
                    Spec = should_have_one_call_prefix(ContextPieces, Context,
                        Functor),
                    MaybeGoal = error1([Spec])
                )
            ;
                MaybeSubGoal = error1(Specs),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_one_call_prefix(ContextPieces, Context,
                Functor),
            MaybeGoal = error1([Spec])
        )
    ).

%-----------------------------------------------------------------------------%

:- func should_have_no_args(list(format_component),
    term.context, string) = error_spec.

should_have_no_args(ContextPieces, Context, Functor) = Spec :-
    Pieces = ContextPieces ++
        [lower_case_next_if_not_first, words("Error:"),
        quote(Functor), words("should have no arguments."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

should_have_one_goal_prefix(ContextPieces, Context, Functor) = Spec :-
    Pieces = ContextPieces ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the prefix operator"), quote(Functor),
        words("should precede a single goal."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

should_have_two_terms_infix(ContextPieces, Context, Functor) = Spec :-
    Pieces = ContextPieces ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the infix operator"), quote(Functor),
        words("should have two terms as arguments."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

should_have_two_goals_infix(ContextPieces, Context, Functor) = Spec :-
    Pieces = ContextPieces ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the infix operator"), quote(Functor),
        words("should have two goals as arguments."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

should_have_one_x_one_goal_prefix(ContextPieces, Context, X, Functor) = Spec :-
    Pieces = ContextPieces ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the binary prefix operator"), quote(Functor),
        words("should precede"), words(X), words("and a goal."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

:- func should_have_one_call_prefix(list(format_component),
    term.context, string) = error_spec.

should_have_one_call_prefix(ContextPieces, Context, Functor) = Spec :-
    Pieces = ContextPieces ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the prefix operator"), quote(Functor),
        words("should precede a call."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

%-----------------------------------------------------------------------------%

apply_purity_marker_to_maybe_goal(GoalTerm, Purity, MaybeGoal0, MaybeGoal) :-
    (
        MaybeGoal0 = ok1(Goal0),
        (
            Goal0 = call_expr(Context, Pred, Args, Purity0),
            (
                Purity0 = purity_pure,
                Goal = call_expr(Context, Pred, Args, Purity)
            ;
                ( Purity0 = purity_semipure
                ; Purity0 = purity_impure
                ),
                Goal = bad_purity_goal(GoalTerm, Context, Purity)
            )
        ;
            Goal0 = unify_expr(Context, ProgTerm1, ProgTerm2, Purity0),
            (
                Purity0 = purity_pure,
                Goal = unify_expr(Context, ProgTerm1, ProgTerm2, Purity)
            ;
                ( Purity0 = purity_semipure
                ; Purity0 = purity_impure
                ),
                Goal = bad_purity_goal(GoalTerm, Context, Purity)
            )
        ;
            ( Goal0 = conj_expr(_, _, _)
            ; Goal0 = par_conj_expr(_, _, _)
            ; Goal0 = true_expr(_)
            ; Goal0 = disj_expr(_, _, _)
            ; Goal0 = fail_expr(_)
            ; Goal0 = quant_expr(_, _, _, _, _)
            ; Goal0 = promise_purity_expr(_, _, _)
            ; Goal0 = promise_equivalent_solutions_expr(_, _, _, _, _, _)
            ; Goal0 = promise_equivalent_solution_sets_expr(_, _, _, _, _, _)
            ; Goal0 = promise_equivalent_solution_arbitrary_expr(_, _, _,
                _, _, _)
            ; Goal0 = require_detism_expr(_, _, _)
            ; Goal0 = require_complete_switch_expr(_, _, _)
            ; Goal0 = require_switch_arms_detism_expr(_, _, _, _)
            ; Goal0 = trace_expr(_, _, _, _, _, _)
            ; Goal0 = atomic_expr(_, _, _, _, _, _)
            ; Goal0 = try_expr(_, _, _, _, _, _, _)
            ; Goal0 = implies_expr(_, _, _)
            ; Goal0 = equivalent_expr(_, _, _)
            ; Goal0 = not_expr(_, _)
            ; Goal0 = if_then_else_expr(_, _, _, _, _, _)
            ; Goal0 = event_expr(_, _, _)
            ),
            Goal = bad_purity_goal(GoalTerm, goal_get_context(Goal0), Purity)
        ),
        MaybeGoal = ok1(Goal)
    ;
        MaybeGoal0 = error1(Specs),
        MaybeGoal = error1(Specs)
    ).

    % bad_purity_goal(BadGoal, Purity):
    %
    % Given G, a term representing a goal that a semipure and impure prefix
    % is applied to even though such prefixes do not apply to it, return
    % the least-bad goal as the goal in that term. We return a predicate call
    % for which typechecking should print a descriptive error message.
    %
:- func bad_purity_goal(term, term.context, purity) = goal.

bad_purity_goal(GoalTerm0, Context, Purity) = Goal :-
    term.coerce(GoalTerm0, GoalTerm),
    purity_name(Purity, PurityString),
    Goal = call_expr(Context, unqualified(PurityString), [GoalTerm],
        purity_pure).

%-----------------------------------------------------------------------------%

:- pred parse_one_var_list(list(var(T))::in, goal::in,
    list(format_component)::in, string::in, maybe1(prog_var)::out) is det.

parse_one_var_list(Vars0, Goal, ContextPieces, ConstructName, MaybeVar) :-
    (
        Vars0 = [],
        Context = goal_get_context(Goal),
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
        Context = goal_get_context(Goal),
        Pieces = ContextPieces ++
            [words("Error: the first argument of"), words(ConstructName),
            words("cannot contain more than one variable."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        MaybeVar = error1([Spec])
    ).

%-----------------------------------------------------------------------------%

parse_some_vars_goal(Term, ContextPieces, MaybeVarsAndGoal, !VarSet) :-
    ( if
        Term = term.functor(term.atom("some"), [QVarsTerm, SubGoalTerm],
            _Context)
    then
        UpdatedContextPieces = ContextPieces ++ [lower_case_next_if_not_first,
            words("In first argument of"), quote("some"), suffix(":")],
        varset.coerce(!.VarSet, GenericVarSet),
        parse_quantifier_vars(QVarsTerm, GenericVarSet, UpdatedContextPieces,
            MaybeVars),
        GoalTerm = SubGoalTerm
    else
        MaybeVars = ok2([], []),
        GoalTerm = Term
    ),
    parse_goal(GoalTerm, ContextPieces, MaybeGoal, !VarSet),
    ( if
        MaybeVars = ok2(Vars0, StateVars0),
        MaybeGoal = ok1(Goal)
    then
        list.map(term.coerce_var, Vars0, Vars),
        list.map(term.coerce_var, StateVars0, StateVars),
        MaybeVarsAndGoal = ok3(Vars, StateVars, Goal)
    else
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
    ( if Term = term.functor(term.atom("[]"), [], _) then
        MaybeComponentsContexts = ok1([])
    else if Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _) then
        parse_trace_component(VarSet, Term, HeadTerm,
            MaybeHeadComponentContext),
        parse_trace_params(VarSet, Context, TailTerm,
            MaybeTailComponentsContexts),
        ( if
            MaybeHeadComponentContext = ok1(HeadComponentContext),
            MaybeTailComponentsContexts = ok1(TailComponentsContexts)
        then
            MaybeComponentsContexts =
                ok1([HeadComponentContext | TailComponentsContexts])
        else
            HeadSpecs = get_any_errors1(MaybeHeadComponentContext),
            TailSpecs = get_any_errors1(MaybeTailComponentsContexts),
            MaybeComponentsContexts = error1(HeadSpecs ++ TailSpecs)
        )
    else
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
    ( if
        Term = term.functor(Functor, SubTerms, Context),
        Functor = term.atom(Atom)
    then
        ( if
            ( Atom = "compiletime"
            ; Atom = "compile_time"
            )
        then
            ( if SubTerms = [SubTerm] then
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
            else
                Pieces = [words("Error:"), fixed(Atom),
                    words("takes exactly one argument,"),
                    words("which should be a boolean expression"),
                    words("of compile-time tests."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeComponentContext = error1([Spec])
            )
        else if
            ( Atom = "runtime"
            ; Atom = "run_time"
            )
        then
            ( if SubTerms = [SubTerm] then
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
            else
                Pieces = [words("Error:"), fixed(Atom),
                    words("takes exactly one argument,"),
                    words("which should be a boolean expression"),
                    words("of run-time tests."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeComponentContext = error1([Spec])
            )
        else if
            Atom = "io"
        then
            ( if SubTerms = [SubTerm] then
                ( if
                    SubTerm = term.functor(term.atom("!"),
                        [term.variable(Var, _)], _)
                then
                    term.coerce_var(Var, ProgVar),
                    Component = trace_component_maybe_io(ProgVar),
                    MaybeComponentContext = ok1(Component - Context)
                else
                    Pieces = [words("Error: the argument of"), fixed(Atom),
                        words("should be a state variable."), nl],
                    Spec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(SubTerm),
                            [always(Pieces)])]),
                    MaybeComponentContext = error1([Spec])
                )
            else
                Pieces = [words("Error:"), fixed(Atom),
                    words("takes exactly one argument,"),
                    words("which should be a state variable name."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeComponentContext = error1([Spec])
            )
        else if
            Atom = "state"
        then
            ( if SubTerms = [SubTermA, SubTermB] then
                ( if
                    SubTermA = term.functor(term.atom(MutableName), [], _)
                then
                    MaybeMutable = ok1(MutableName)
                else
                    MutablePieces = [words("Error: the first argument of"),
                        fixed(Atom), words("should be"),
                        words("the name of a mutable variable."), nl],
                    MutableSpec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(SubTermA),
                            [always(MutablePieces)])]),
                    MaybeMutable = error1([MutableSpec])
                ),
                ( if
                    SubTermB = term.functor(term.atom("!"),
                        [term.variable(Var, _)], _)
                then
                    MaybeVar = ok1(Var)
                else
                    VarPieces = [words("Error: the second argument of"),
                        fixed(Atom), words("should be"),
                        words("a state variable name."), nl],
                    VarSpec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(SubTermB),
                            [always(VarPieces)])]),
                    MaybeVar = error1([VarSpec])
                ),
                ( if
                    MaybeMutable = ok1(FinalMutable),
                    MaybeVar = ok1(FinalVar)
                then
                    term.coerce_var(FinalVar, ProgVar),
                    MutableVar = trace_mutable_var(FinalMutable, ProgVar),
                    Component = trace_component_mutable_var(MutableVar),
                    MaybeComponentContext = ok1(Component - Context)
                else
                    VarSpecs = get_any_errors1(MaybeVar),
                    MutableSpecs = get_any_errors1(MaybeMutable),
                    MaybeComponentContext =
                        error1(VarSpecs ++ MutableSpecs)
                )
            else
                Pieces = [words("Error:"), fixed(Atom),
                    words("takes exactly two arguments,"),
                    words("which should be"),
                    words("the name of a mutable variable"),
                    words("and a state variable name."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeComponentContext = error1([Spec])
            )
        else
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: invalid trace goal parameter"),
                quote(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeComponentContext = error1([Spec])
        )
    else
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
    ( if
        Term = term.functor(term.atom(Atom), [LTerm, RTerm], _),
        (
            Atom = "or",
            Op = trace_or
        ;
            Atom = "and",
            Op = trace_and
        )
    then
        parse_trace_tree(BaseParser, LTerm, MaybeLExpr),
        parse_trace_tree(BaseParser, RTerm, MaybeRExpr),
        ( if
            MaybeLExpr = ok1(LExpr),
            MaybeRExpr = ok1(RExpr)
        then
            MaybeTree = ok1(trace_op(Op, LExpr, RExpr))
        else
            LSpecs = get_any_errors1(MaybeLExpr),
            RSpecs = get_any_errors1(MaybeRExpr),
            MaybeTree = error1(LSpecs ++ RSpecs)
        )
    else if
        Term = term.functor(term.atom("not"), [SubTerm], _)
    then
        parse_trace_tree(BaseParser, SubTerm, MaybeSubExpr),
        ( if
            MaybeSubExpr = ok1(SubExpr)
        then
            MaybeTree = ok1(trace_not(SubExpr))
        else
            SubSpecs = get_any_errors1(MaybeSubExpr),
            MaybeTree = error1(SubSpecs)
        )
    else
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
    ( if
        Term = term.functor(Functor, SubTerms, TermContext),
        Functor = term.atom(Atom)
    then
        ( if Atom = "flag" then
            ( if SubTerms = [SubTerm] then
                ( if SubTerm = term.functor(term.string(FlagName), [], _) then
                    Compiletime = trace_flag(FlagName),
                    MaybeCompiletime = ok1(Compiletime)
                else
                    Pieces = [words("Error: compile_time parameter"),
                        quote("flag"),
                        words("takes a string as argument."), nl],
                    Spec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(TermContext, [always(Pieces)])]),
                    MaybeCompiletime = error1([Spec])
                )
            else
                Pieces = [words("Error: compile_time parameter"),
                    quote("flag"), words("takes just one argument."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeCompiletime = error1([Spec])
            )
        else if Atom = "grade" then
            ( if SubTerms = [SubTerm] then
                ( if
                    SubTerm = term.functor(term.atom(GradeName), [], _),
                    parse_trace_grade_name(GradeName, TraceGrade)
                then
                    Compiletime = trace_grade(TraceGrade),
                    MaybeCompiletime = ok1(Compiletime)
                else
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
            else
                Pieces = [words("Error: compile_time parameter"),
                    quote("grade"), words("takes just one argument."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeCompiletime = error1([Spec])
            )
        else if Atom = "tracelevel" then
            ( if SubTerms = [SubTerm] then
                ( if
                    SubTerm = term.functor(term.atom(LevelName), [], _),
                    (
                        LevelName = "shallow",
                        Level = trace_level_shallow
                    ;
                        LevelName = "deep",
                        Level = trace_level_deep
                    )
                then
                    Compiletime = trace_trace_level(Level),
                    MaybeCompiletime = ok1(Compiletime)
                else
                    Pieces = [words("Error: compile_time parameter"),
                        quote("tracelevel"), words("takes just"),
                        quote("shallow"), words("or"), quote("deep"),
                        words("as argument."), nl],
                    Spec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(TermContext, [always(Pieces)])]),
                    MaybeCompiletime = error1([Spec])
                )
            else
                Pieces = [words("Error: compile_time parameter"),
                    quote("tracelevel"),
                    words("takes just one argument."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeCompiletime = error1([Spec])
            )
        else
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: invalid compile_time parameter"),
                quote(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeCompiletime = error1([Spec])
        )
    else
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
    ( if
        Term = term.functor(Functor, SubTerms, TermContext),
        Functor = term.atom(Atom)
    then
        ( if Atom = "env" then
            ( if SubTerms = [SubTerm] then
                ( if
                    SubTerm = term.functor(SubFunctor, [], _),
                    ( SubFunctor = term.string(EnvVarName)
                    ; SubFunctor = term.atom(EnvVarName)
                    ),
                    EnvVarChars = string.to_char_list(EnvVarName),
                    list.filter(env_var_is_acceptable_char,
                        EnvVarChars, _, [])
                then
                    Runtime = trace_envvar(EnvVarName),
                    MaybeRuntime = ok1(Runtime)
                else
                    Pieces = [words("Error: run_time parameter"), quote("env"),
                        words("takes an identifier as argument."), nl],
                    Spec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(SubTerm),
                            [always(Pieces)])]),
                    MaybeRuntime = error1([Spec])
                )
            else
                Pieces = [words("Error: run_time parameter"), quote("env"),
                    words("takes just one argument."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(TermContext, [always(Pieces)])]),
                MaybeRuntime = error1([Spec])
            )
        else
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: invalid run_time parameter"),
                quote(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeRuntime = error1([Spec])
        )
    else
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
    ( char.is_alnum(Char)
    ; Char = '_'
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
    prog_varset::in, prog_varset::out) is det.

parse_catch_any_term(ArrowTerm, _Context, ContextPieces, MaybeCatchAny,
        !VarSet) :-
    ( if ArrowTerm = term.functor(atom("->"), [VarTerm0, GoalTerm], _) then
        ( if VarTerm0 = term.variable(Var0, _) then
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
        else
            Pieces = [words("Error: the left operand of the"),
                quote("->"), words("operator inside the scope"),
                words("of a"), quote("catch_any"), words("operator"),
                words("should be a variable."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ArrowTerm), [always(Pieces)])]),
            MaybeCatchAny = error1([Spec])
        )
    else
        Pieces = [words("Error: the "), quote("catch_any"), words("operator"),
            words("should be followed by an expression of the form"),
            quote("variable -> goal"), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ArrowTerm), [always(Pieces)])]),
        MaybeCatchAny = error1([Spec])
    ).

:- pred parse_catch_then_try_term_args(list(term)::in,
    maybe(catch_any_expr)::in, term.context::in, list(format_component)::in,
    maybe1(goal)::out, prog_varset::in, prog_varset::out) is det.

parse_catch_then_try_term_args(CatchTermArgs, MaybeCatchAnyExpr,
        Context, ContextPieces, MaybeGoal, !VarSet) :-
    ( if CatchTermArgs = [TermA, TermB] then
        parse_sub_catch_terms(TermB, Context, ContextPieces, MaybeCatches,
             !VarSet),
        (
            MaybeCatches = ok1(Catches),
            parse_else_then_try_term(TermA, Catches, MaybeCatchAnyExpr,
                Context, ContextPieces, MaybeGoal, !VarSet)
        ;
            MaybeCatches = error1(Error),
            MaybeGoal = error1(Error)
        )
    else
        Pieces = [words("Error: the "), quote("catch"), words("operator"),
            words("should be preceded by a try expression of the form"),
            quote("try [try_params] main_goal then else_goal"), suffix(","),
            words("and followed by an expression of the form"),
            quote("catch_pattern -> catch_goal"), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        MaybeGoal = error1([Spec])
    ).

:- pred parse_sub_catch_terms(term::in, term.context::in,
    list(format_component)::in, maybe1(list(catch_expr))::out,
    prog_varset::in, prog_varset::out) is det.

parse_sub_catch_terms(Term, Context, ContextPieces, MaybeCatches, !VarSet) :-
    ( if Term = functor(atom("catch"), [CatchArrowTerm, SubTerm], _) then
        parse_catch_arrow_term(CatchArrowTerm, Context, ContextPieces,
            HeadMaybeCatch, !VarSet),
        parse_sub_catch_terms(SubTerm, Context, ContextPieces,
            TailMaybeCatches, !VarSet),
        ( if
            HeadMaybeCatch = ok1(HeadCatch),
            TailMaybeCatches = ok1(TailCatches)
        then
            MaybeCatches = ok1([HeadCatch | TailCatches])
        else
            Specs = get_any_errors1(HeadMaybeCatch) ++
                get_any_errors1(TailMaybeCatches),
            MaybeCatches = error1(Specs)
        )
    else
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
    prog_varset::in, prog_varset::out) is det.

parse_catch_arrow_term(ArrowTerm, _Context, ContextPieces, MaybeCatch,
        !VarSet) :-
    ( if ArrowTerm = term.functor(atom("->"), [PatternTerm0, GoalTerm], _) then
        term.coerce(PatternTerm0, PatternTerm),
        parse_goal(GoalTerm, ContextPieces, MaybeGoal, !VarSet),
        (
            MaybeGoal = ok1(Goal),
            Catch = catch_expr(PatternTerm, Goal),
            MaybeCatch = ok1(Catch)
        ;
            MaybeGoal = error1(Error),
            MaybeCatch = error1(Error)
        )
    else
        Pieces = [words("Error: the "), quote("catch"), words("operator"),
            words("should be followed by an expression of the form"),
            quote("catch_pattern -> catch_goal"), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ArrowTerm), [always(Pieces)])]),
        MaybeCatch = error1([Spec])
    ).

:- pred parse_else_then_try_term(term::in, list(catch_expr)::in,
    maybe(catch_any_expr)::in, term.context::in, list(format_component)::in,
    maybe1(goal)::out, prog_varset::in, prog_varset::out) is det.

parse_else_then_try_term(Term, CatchExprs, MaybeCatchAnyExpr,
        Context, ContextPieces, MaybeGoal, !VarSet) :-
    % `else' part may or may not exist in `try' goals.
    ( if Term = term.functor(term.atom("else"), [ThenTerm, ElseTerm], _) then
        parse_goal(ElseTerm, ContextPieces, MaybeElseGoal0, !VarSet),
        (
            MaybeElseGoal0 = ok1(ElseGoal),
            parse_then_try_term(ThenTerm, yes(ElseGoal), CatchExprs,
                MaybeCatchAnyExpr, Context, ContextPieces, MaybeGoal, !VarSet)
        ;
            MaybeElseGoal0 = error1(Specs),
            MaybeGoal = error1(Specs)
        )
    else
        parse_then_try_term(Term, no, CatchExprs, MaybeCatchAnyExpr,
            Context, ContextPieces, MaybeGoal, !VarSet)
    ).

:- pred parse_then_try_term(term::in, maybe(goal)::in, list(catch_expr)::in,
    maybe(catch_any_expr)::in, term.context::in, list(format_component)::in,
    maybe1(goal)::out, prog_varset::in, prog_varset::out) is det.

parse_then_try_term(ThenTryTerm, MaybeElse, CatchExprs, MaybeCatchAnyExpr,
        Context, ContextPieces, MaybeGoal, !VarSet) :-
    ( if
        ThenTryTerm = term.functor(term.atom("then"), [TryTerm, ThenTerm], _),
        TryTerm = term.functor(term.atom("try"), [ParamsTerm, TryGoalTerm],
            TryContext)
    then
        varset.coerce(!.VarSet, GenericVarSet),
        parse_try_params(GenericVarSet, Context, ParamsTerm, MaybeParams),
        parse_goal(TryGoalTerm, ContextPieces, MaybeTryGoal, !VarSet),
        parse_goal(ThenTerm, ContextPieces, MaybeThenGoal, !VarSet),
        ( if
            MaybeParams = ok1(Params),
            MaybeTryGoal = ok1(TryGoal),
            MaybeThenGoal = ok1(ThenGoal)
        then
            convert_try_params(Params, MaybeComponents),
            (
                MaybeComponents = ok1(MaybeIO),
                Goal = try_expr(TryContext, MaybeIO, TryGoal, ThenGoal,
                    MaybeElse, CatchExprs, MaybeCatchAnyExpr),
                MaybeGoal = ok1(Goal)
            ;
                MaybeComponents = error1(Specs),
                MaybeGoal = error1(Specs)
            )
        else
            Specs = get_any_errors1(MaybeParams) ++
                get_any_errors1(MaybeTryGoal) ++
                get_any_errors1(MaybeThenGoal),
            MaybeGoal = error1(Specs)
        )
    else
        Pieces = [words("Error: a"), quote("try"), words("goal"),
            words("should have the form"),
            quote("try [try_params] main_goal then success_goal"), suffix(","),
            words("optionally followed by"),
            quote("else failue_goal"), suffix(","),
            words("which in turn may be followed by zero or more"),
            quote("catch"), words("clauses, and optionally by a single"),
            quote("catch_any"), words("clause."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ThenTryTerm), [always(Pieces)])]),
        MaybeGoal = error1([Spec])
    ).

:- type try_component
    --->    try_component_maybe_io(prog_var).

:- pred parse_try_params(varset::in, context::in, term::in,
    maybe1(assoc_list(try_component, term.context))::out) is det.

parse_try_params(VarSet, Context, Term, MaybeComponentsContexts) :-
    ( if Term = term.functor(term.atom("[]"), [], _) then
        MaybeComponentsContexts = ok1([])
    else if Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _) then
        parse_try_param(VarSet, Term, HeadTerm,
            MaybeHeadComponentContext),
        parse_try_params(VarSet, Context, TailTerm,
            MaybeTailComponentsContexts),
        ( if
            MaybeHeadComponentContext = ok1(HeadComponentContext),
            MaybeTailComponentsContexts = ok1(TailComponentsContexts)
        then
            MaybeComponentsContexts =
                ok1([HeadComponentContext | TailComponentsContexts])
        else
            Specs = get_any_errors1(MaybeHeadComponentContext) ++
                get_any_errors1(MaybeTailComponentsContexts),
            MaybeComponentsContexts = error1(Specs)
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: the"), quote("try"), words("operator"),
            words("should be followed by a list of try parameters;"),
            quote(TermStr), words("is not a list."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeComponentsContexts = error1([Spec])
    ).

:- pred parse_try_param(varset::in, term::in, term::in,
    maybe1(pair(try_component, term.context))::out) is det.

parse_try_param(VarSet, _ErrorTerm, Term, MaybeComponentContext) :-
    ( if
        Term = term.functor(Functor, SubTerms, Context),
        Functor = term.atom(Atom)
    then
        ( if Atom = "io" then
            ( if SubTerms = [SubTerm] then
                ( if
                    SubTerm = term.functor(term.atom("!"),
                        [term.variable(Var, _)], _)
                then
                    term.coerce_var(Var, ProgVar),
                    Component = try_component_maybe_io(ProgVar),
                    MaybeComponentContext = ok1(Component - Context)
                else
                    Pieces = [words("Error: the argument of"), fixed(Atom),
                        words("should be a state variable."), nl],
                    Spec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(SubTerm),
                            [always(Pieces)])]),
                    MaybeComponentContext = error1([Spec])
                )
            else
                Pieces = [words("Error:"), fixed(Atom),
                    words("takes exactly one argument,"),
                    words("which should be a state variable name."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeComponentContext = error1([Spec])
            )
        else
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: invalid try goal parameter"),
                quote(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeComponentContext = error1([Spec])
        )
    else
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
    ( if Term = term.functor(term.atom("[]"), [], _) then
        MaybeComponentsContexts = ok1([])
    else if Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _) then
        parse_atomic_component(Term, HeadTerm, VarSet, MaybeHeadComponent),
        parse_atomic_params(Context, TailTerm, VarSet,
            MaybeTailComponentsContexts),
        ( if
            MaybeHeadComponent = ok1(HeadComponent),
            MaybeTailComponentsContexts = ok1(TailComponentsContexts)
        then
            MaybeComponentsContexts =
                ok1([HeadComponent | TailComponentsContexts])
        else
            HeadSpecs = get_any_errors1(MaybeHeadComponent),
            TailSpecs = get_any_errors1(MaybeTailComponentsContexts),
            MaybeComponentsContexts = error1(HeadSpecs ++ TailSpecs)
        )
    else
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
        ( if
            parse_atomic_component_state_or_pair(SubTerms, ComponentState)
        then
            MaybeComponentState = ok1(ComponentState)
        else
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
        ( if Functor = term.atom(Atom) then
            % XXX Make parse_atomic_subterm do the postprocessing done here.
            ( if Atom = "outer" then
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
            else if Atom = "inner" then
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
            else if Atom = "vars" then
                ( if SubTerms = [SubTerm] then
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
                else
                    Pieces = [words(Atom), words("takes exact one argument,"),
                        words("which should be a list of variable names."),
                        nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(Context, [always(Pieces)])]),
                    MaybeComponentContext = error1([Spec])
                )
            else
                Pieces = [words("Invalid atomic goal parameter."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeComponentContext = error1([Spec])
            )
        else
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
    ( if
        SubTerms = [Term],
        Term = term.functor(term.atom("!"), [term.variable(Var, _)], _)
    then
        term.coerce_var(Var, ProgVar),
        State = atomic_state_var(ProgVar)
    else if
        SubTerms = [TermA, TermB],
        TermA = term.variable(VarA, _),
        TermB = term.variable(VarB, _)
    then
        term.coerce_var(VarA, ProgVarA),
        term.coerce_var(VarB, ProgVarB),
        State = atomic_var_pair(ProgVarA, ProgVarB)
    else
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

:- pred parse_atomic_subexpr(term::in, maybe2(goal, list(goal))::out,
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
    ( if
        Term = term.functor(term.atom("or_else"), [LeftGoal, RightGoal], _)
    then
        parse_atomic_subgoals_as_list(LeftGoal, MaybeLeftGoalList, !VarSet),
        parse_atomic_subgoals_as_list(RightGoal, MaybeRightGoalList, !VarSet),
        ( if
            MaybeLeftGoalList = ok1(LeftGoalList),
            MaybeRightGoalList = ok1(RightGoalList)
        then
            MaybeGoals = ok1(LeftGoalList ++ RightGoalList)
        else
            LeftSpecs = get_any_errors1(MaybeLeftGoalList),
            RightSpecs = get_any_errors1(MaybeRightGoalList),
            MaybeGoals = error1(LeftSpecs ++ RightSpecs)
        )
    else
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
% Code for parsing pred/func expressions.
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

    ( if parse_pred_expr_args(FuncArgsList, Args0, Modes0) then
        parse_lambda_arg(RetTerm, RetArg, RetMode),
        Args = Args0 ++ [RetArg],
        Modes = Modes0 ++ [RetMode],
        inst_var_constraints_are_self_consistent_in_modes(Modes)
    else
        % The argument modes default to `in',
        % the return mode defaults to `out'.
        in_mode(InMode),
        out_mode(OutMode),
        list.length(FuncArgsList, NumArgs),
        list.duplicate(NumArgs, InMode, InModes),
        Modes = InModes ++ [OutMode],
        Args1 = FuncArgsList ++ [RetTerm],
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
    list.duplicate(NumArgs, InMode, InModes),
    Det = detism_det,
    Modes = InModes ++ [OutMode],
    inst_var_constraints_are_self_consistent_in_modes(Modes),
    Args1 = Args0 ++ [RetTerm],
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
