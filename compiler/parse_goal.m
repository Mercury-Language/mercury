%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_goal.m.
% Main authors: fjh, zs.
%
% This module defines the predicates that parse goals.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_goal.
:- interface.

:- import_module parse_tree.error_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module cord.
:- import_module list.
:- import_module term.

%---------------------------------------------------------------------------%

    % Convert a single term into a goal.
    %
:- pred parse_goal(term::in, cord(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out) is det.

    % Convert a term, possibly starting with `some [Vars]', into a list
    % of the quantified variables, a list of quantified state variables,
    % and a goal. (If the term doesn't start with `some [Vars]', we return
    % empty lists of variables.)
    %
    % Exported to superhomogeneous.m for parsing if-then-else expressions.
    %
:- pred parse_some_vars_goal(term::in, cord(format_component)::in,
    maybe3(list(prog_var), list(prog_var), goal)::out,
    prog_varset::in, prog_varset::out) is det.

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

    % Functions to construct error messages. Exported to parse_dcg_goal.m,
    % to allow DCG and non-DCG clauses to generate identical error messages
    % in analogous situations.
    %
:- func should_have_one_goal_prefix(cord(format_component),
    term.context, string) = error_spec.
:- func should_have_two_terms_infix(cord(format_component),
    term.context, string) = error_spec.
:- func should_have_two_goals_infix(cord(format_component),
    term.context, string) = error_spec.
:- func should_have_one_x_one_goal_prefix(cord(format_component),
    term.context, string, string) = error_spec.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_vars.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
:- import_module bag.
:- import_module char.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module solutions.
:- import_module string.
:- import_module unit.
:- import_module varset.

%---------------------------------------------------------------------------%

parse_goal(Term, ContextPieces, MaybeGoal, !VarSet) :-
    % We could do some error-checking here, but all errors are picked up
    % in either the type-checker or parser anyway.
    %
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
        Context = get_term_context(Term),
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

%---------------------------------------------------------------------------%

:- pred parse_non_call_goal(string::in, list(term)::in, term.context::in,
    cord(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out) is semidet.

parse_non_call_goal(Functor, Args, Context, ContextPieces, MaybeGoal,
        !VarSet) :-
    % We parse goals in non-DCG contexts here, while we parse goals
    % in DCG contexts in parse_non_call_dcg_goal in parse_dcg_goal.m.
    %
    % Since many kinds of goals can occur in both kinds of contexts,
    % the code handling those kinds of goals should be kept as identical
    % as possible in the two places. For ease of maintenance, the switch arms
    % handling the common goal types should also be kept in the same order.
    % The list below documents the order in both predicates. If you add
    % code to handle a new goal type either here or there, please update
    % this comment.
    %
    % In both parse_non_call_goal and parse_non_call_dcg_goal:
    %
    %   impure/1, semipure/1
    %   promise_pure/1, promise_semipure/1, promise_impure/1
    %   disable_warnings/2
    %   not/1, \+/1
    %   some/2, all/2
    %   ,/2
    %   &/2
    %   ;/2                     (disjunction, or C->T;E style if-then-else)
    %   else                    (if C then T else E style if-then-else;
    %                               or try goal in parse_non_call_goal)
    %
    % Only in parse_non_call_goal, after the common goal types:
    %
    %   then/2                  (try goal)
    %   catch/2                 (try goal)
    %   catch_any/2             (try goal)
    %   <=/2, =>/2, <=>/2       (implication, bi-implication)
    %   trace/2                 (trace goals)
    %   atomic/2                (atomic goals)
    %   prom_eqv_{solns,sets}/2 (determinism cast)
    %   arbitrary/2             (determinism cast)
    %   req_{det,...}/2         (determinism check)
    %   req_compl_switch/2      (determinism check)
    %   req_sw_arms_{det,...}/2 (determinism check)
    %   event/2                 (debugger event)
    %   true/0, fail/0          (empty conjunction/disjunction)
    %   =/2, is/2               (unification)
    %
    % Only in parse_non_call_dcg_goal, after the common goal types:
    %
    %   if/2                    (if C then T, with implicit "else")
    %   {}/1                    (wrapping non-DCG goal)
    %   []/0                    (consuming nothing from dcg)
    %   [|]/2                   (consuming something from dcg)
    %   =/1                     (reading current dcg var)
    %   :=/1                    (writing to next dcg var)
    %
    % For the goal types that can occur in both DCG and non-DCG contexts,
    % the code handling them here and in parse_non_call_dcg_goal is often
    % very similar. These goal types are all compound, and they tend to differ
    % only in whether they call parse_goal or parse_dcg_goal to handle their
    % subgoals. However, factoring out the commonalities would nevertheless
    % not be a good idea, for two reasons. Both relate to the fact that
    % the common code would have to make a runtime decision between
    % calling parse_goal and parse_dcg_goal on terms representing subgoals,
    % and would have to pass around the arguments needed by parse_dcg_goal
    % to make the latter choice possible.
    %
    % The first reason is simply that with this extra generality,
    % the common code would not be significantly shorter OR simpler
    % than the two specialized pieces of code put together.
    %
    % The second reason is that the extra tests and parameter passing
    % would slow down the code in both contexts. For infrequently occurring
    % kinds of goals, this wouldn't matter, but conjunctions and if-then-elses
    % occur very frequently, and we don't want to pay any extra cost
    % for parsing them.

    % XXX We should update ContextPieces at every call to parse a goal
    % component that is not itself a goal.

    require_switch_arms_det [Functor]
    (
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
        ( Functor = "disable_warning"
        ; Functor = "disable_warnings"
        ),
        ( if Args = [WarningsTerm, SubGoalTerm] then
            varset.coerce(!.VarSet, GenericVarSet),
            parse_warnings(GenericVarSet, WarningsTerm, Functor,
                ContextPieces, 1, MaybeWarnings),
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            ( if
                MaybeWarnings = ok1(Warnings),
                MaybeSubGoal = ok1(SubGoal)
            then
                WarningsContext = get_term_context(WarningsTerm),
                % XXX Should we generate a warning if the name of a warning
                % to disable is listed more than once?
                bag.insert_list(Warnings, bag.init, WarningsBag),
                bag.to_assoc_list(WarningsBag, WarningsCounts),
                generate_warnings_for_duplicate_warnings(WarningsContext,
                    ContextPieces, WarningsCounts,
                    NonDuplicateWarnings, DuplicateSpecs),
                (
                    DuplicateSpecs = [],
                    (
                        NonDuplicateWarnings = [HeadWarning | TailWarnings],
                        Goal = disable_warnings_expr(Context,
                            HeadWarning, TailWarnings, SubGoal),
                        MaybeGoal = ok1(Goal)
                    ;
                        NonDuplicateWarnings = [],
                        Pieces = cord.list(ContextPieces) ++
                            [lower_case_next_if_not_first, words("Error:"),
                            words("a"), fixed(Functor), words("scope"),
                            words("must list at least one warning."), nl],
                        Spec = error_spec(severity_error,
                            phase_term_to_parse_tree,
                            [simple_msg(WarningsContext, [always(Pieces)])]),
                        MaybeGoal = error1([Spec])
                    )
                ;
                    DuplicateSpecs = [_ | _],
                    MaybeGoal = error1(DuplicateSpecs)
                )
            else
                Specs = get_any_errors1(MaybeWarnings) ++
                    get_any_errors1(MaybeSubGoal),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_one_x_one_goal_prefix(ContextPieces, Context,
                "a list of warnings to disable", Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        ( Functor = "not"   % Negation (NU-Prolog syntax).
        ; Functor = "\\+"   % Negation (Prolog syntax).
        ),
        ( if Args = [SubGoalTerm] then
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            (
                MaybeSubGoal = ok1(SubGoal),
                MaybeGoal = ok1(not_expr(Context, SubGoal))
            ;
                MaybeSubGoal = error1(_),
                MaybeGoal = MaybeSubGoal
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
            VarsTailPieces = [lower_case_next_if_not_first,
                words("In first argument of"), quote("some"), suffix(":")]
        ;
            Functor = "all",
            QuantType = quant_all,
            VarsTailPieces = [lower_case_next_if_not_first,
                words("In first argument of"), quote("all"), suffix(":")]
        ),
        ( if Args = [QVarsTerm, SubGoalTerm] then
            varset.coerce(!.VarSet, GenericVarSet),
            VarsContextPieces = ContextPieces ++
                cord.from_list(VarsTailPieces),
            parse_vars_state_vars(QVarsTerm, GenericVarSet, VarsContextPieces,
                MaybeVars),
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            ( if
                MaybeVars = ok1(plain_state_vars(Vars0, StateVars0)),
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
                Specs = get_any_errors1(MaybeVars) ++
                    get_any_errors1(MaybeSubGoal),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_one_x_one_goal_prefix(ContextPieces, Context,
                "a list of variables", Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = ",",
        % Although we do almost exactly the same thing for "&" as for ",",
        % we handle them separately, because "," is FAR more common than "&",
        % and keeping its processing efficient is important enough to warrant
        % a small amount of code duplication.
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
        ( if Args = [SubGoalTermA, SubGoalTermB] then
            ( if
                SubGoalTermA = term.functor(term.atom("->"),
                    [CondGoalTerm, ThenGoalTerm], _)
            then
                ElseGoalTerm = SubGoalTermB,
                parse_some_vars_goal(CondGoalTerm, ContextPieces,
                    MaybeCondGoal, !VarSet),
                parse_goal(ThenGoalTerm, ContextPieces, MaybeThenGoal,
                    !VarSet),
                parse_goal(ElseGoalTerm, ContextPieces, MaybeElseGoal,
                    !VarSet),
                ( if
                    MaybeCondGoal = ok3(Vars, StateVars, CondGoal),
                    MaybeThenGoal = ok1(ThenGoal),
                    MaybeElseGoal = ok1(ElseGoal)
                then
                    Goal = if_then_else_expr(Context, Vars, StateVars,
                        CondGoal, ThenGoal, ElseGoal),
                    MaybeGoal = ok1(Goal)
                else
                    Specs = get_any_errors3(MaybeCondGoal) ++
                        get_any_errors1(MaybeThenGoal) ++
                        get_any_errors1(MaybeElseGoal),
                    MaybeGoal = error1(Specs)
                )
            else
                parse_goal(SubGoalTermA, ContextPieces, MaybeSubGoalA,
                    !VarSet),
                parse_goal(SubGoalTermB, ContextPieces, MaybeSubGoalB,
                    !VarSet),
                ( if
                    MaybeSubGoalA = ok1(SubGoalA),
                    MaybeSubGoalB = ok1(SubGoalB)
                then
                    MaybeGoal = ok1(disj_expr(Context, SubGoalA, SubGoalB))
                else
                    Specs = get_any_errors1(MaybeSubGoalA) ++
                        get_any_errors1(MaybeSubGoalB),
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
        % If-then-else (NU-Prolog syntax).
        ( if Args = [CondThenTerm, ElseGoalTerm] then
            ( if
                CondThenTerm = term.functor(term.atom("if"),
                    [term.functor(term.atom("then"),
                        [CondGoalTerm, ThenGoalTerm], _)],
                    CondContext)
            then
                parse_some_vars_goal(CondGoalTerm, ContextPieces,
                    MaybeCondGoal, !VarSet),
                parse_goal(ThenGoalTerm, ContextPieces,
                    MaybeThenGoal, !VarSet),
                parse_goal(ElseGoalTerm, ContextPieces,
                    MaybeElseGoal, !VarSet),
                ( if
                    MaybeCondGoal = ok3(Vars, StateVars, CondGoal),
                    MaybeThenGoal = ok1(ThenGoal),
                    MaybeElseGoal = ok1(ElseGoal)
                then
                    Goal = if_then_else_expr(CondContext, Vars, StateVars,
                        CondGoal, ThenGoal, ElseGoal),
                    MaybeGoal = ok1(Goal)
                else
                    Specs = get_any_errors3(MaybeCondGoal) ++
                        get_any_errors1(MaybeThenGoal) ++
                        get_any_errors1(MaybeElseGoal),
                    MaybeGoal = error1(Specs)
                )
            else if
                CondThenTerm = term.functor(term.atom("if"),
                    [term.functor(term.atom("->"),
                        [_CondGoalTerm, _ThenGoalTerm], ArrowContext)],
                    _CondContext)
            then
                Pieces = [words("Error: malformed if-then-else;"),
                    words("replace the"), quote("->"),
                    words("with"), quote("then"), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(ArrowContext, [always(Pieces)])]),
                MaybeGoal = error1([Spec])
            else if
                CondThenTerm = term.functor(term.atom("->"),
                    [_CondGoalTerm, _ThenGoalTerm], ArrowContext)
            then
                Pieces = [words("Error: malformed if-then-else;"),
                    words("replace the"), quote("->"),
                    words("with"), quote("then"), suffix(","),
                    words("and add an"), quote("if"),
                    words("before the condition."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(ArrowContext, [always(Pieces)])]),
                MaybeGoal = error1([Spec])
            else
                % `else' can also be part of a `try' goal.
                FullTerm = term.functor(term.atom("else"), Args, Context),
                parse_else_then_try_term(FullTerm, [], no,
                    Context, ContextPieces, MaybeGoal, !VarSet)
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
        Functor = "if",
        % If-then-else (NU-Prolog syntax) with a missing "else".
        ( if
            Args = [term.functor(term.atom("then"),
                [_CondGoalTerm, ThenGoalTerm], ThenContext)]
        then
            ( if
                ThenGoalTerm = term.functor(term.atom(";"),
                    [_, _], SemiColonContext)
            then
                Pieces = [words("Error: malformed if-then-else;"),
                    words("replace the"), quote(";"),
                    words("with"), quote("else"), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(SemiColonContext, [always(Pieces)])])
            else
                Pieces = [words("Error: malformed if-then-else;"),
                    words("this"), quote("then"),
                    words("has no"), quote("else"), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(ThenContext, [always(Pieces)])])
            )
        else
            Pieces = [words("Error: malformed if-then-else;"),
                words("this"), quote("if"),
                words("has no"), quote("then"), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])])
        ),
        MaybeGoal = error1([Spec])
    ;
        Functor = "then",
        ( if Args = [TryTerm, ThenTerm] then
            ( if
                ThenTerm = term.functor(term.atom(";"), [_, _],
                    SemiColonContext)
            then
                % The term we are parsing is not a valid try goal.
                % It is much more likely to be a malformed if-then-else
                % than a malformed try goal, so generate an error message
                % that is more informative in the common case.
                Pieces = [words("Error: malformed if-then-else;"),
                    words("replace the"), quote(";"),
                    words("with"), quote("else"), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(SemiColonContext, [always(Pieces)])]),
                MaybeGoal = error1([Spec])
            else
                parse_then_try_term(
                    term.functor(atom("then"), [TryTerm, ThenTerm], Context),
                    no, [], no, Context, ContextPieces, MaybeGoal, !VarSet)
            )
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
            parse_vars_state_dot_colon_vars(VarsTerm, GenericVarSet,
                ContextPieces, MaybeVars),
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            ( if
                MaybeVars = ok1(plain_state_dot_colon_vars(Vars0,
                    StateVars0, DotSVars0, ColonSVars0)),
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
                Specs = get_any_errors1(MaybeVars) ++
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
            parse_vars_state_dot_colon_vars(VarsTerm, GenericVarSet,
                ContextPieces, MaybeVars),
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            ( if
                MaybeVars = ok1(plain_state_dot_colon_vars(Vars0,
                    StateVars0, DotSVars0, ColonSVars0)),
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
                Specs = get_any_errors1(MaybeVars) ++
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
            term.coerce(VarsTerm, ProgVarsTerm),
            parse_vars_state_dot_colon_vars(ProgVarsTerm, !.VarSet,
                ContextPieces, MaybePSDCVars),
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            ( if
                MaybePSDCVars = ok1(PSDCVars0),
                MaybeSubGoal = ok1(SubGoal)
            then
                parse_one_plain_or_dot_var(PSDCVars0, SubGoal,
                    ContextPieces, Functor, MaybePODVar),
                (
                    MaybePODVar = ok1(PODVar),
                    Goal = require_complete_switch_expr(Context, PODVar,
                        SubGoal),
                    MaybeGoal = ok1(Goal)
                ;
                    MaybePODVar = error1(RCSSpecs),
                    MaybeGoal = error1(RCSSpecs)
                )
            else
                Specs = get_any_errors1(MaybePSDCVars) ++
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
            term.coerce(VarsTerm, ProgVarsTerm),
            parse_vars_state_dot_colon_vars(ProgVarsTerm, !.VarSet,
                ContextPieces, MaybePSDCVars),
            parse_goal(SubGoalTerm, ContextPieces, MaybeSubGoal, !VarSet),
            ( if
                MaybePSDCVars = ok1(PSDCVars0),
                MaybeSubGoal = ok1(SubGoal)
            then
                parse_one_plain_or_dot_var(PSDCVars0, SubGoal,
                    ContextPieces, Functor, MaybePODVar),
                (
                    MaybePODVar = ok1(PODVar),
                    Goal = require_switch_arms_detism_expr(Context, PODVar,
                        Detism, SubGoal),
                    MaybeGoal = ok1(Goal)
                ;
                    MaybePODVar = error1(RCSSpecs),
                    MaybeGoal = error1(RCSSpecs)
                )
            else
                Specs = get_any_errors1(MaybePSDCVars) ++
                    get_any_errors1(MaybeSubGoal),
                MaybeGoal = error1(Specs)
            )
        else
            Spec = should_have_one_x_one_goal_prefix(ContextPieces, Context,
                "a variable in a singleton list", Functor),
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
                                QualPieces = cord.list(ContextPieces) ++
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
                                PurityPieces = cord.list(ContextPieces) ++
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
    ;
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
    ).

%---------------------------------------------------------------------------%

:- func should_have_no_args(cord(format_component),
    term.context, string) = error_spec.

should_have_no_args(ContextPieces, Context, Functor) = Spec :-
    Pieces = cord.list(ContextPieces) ++
        [lower_case_next_if_not_first, words("Error:"),
        quote(Functor), words("should have no arguments."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

should_have_one_goal_prefix(ContextPieces, Context, Functor) = Spec :-
    Pieces = cord.list(ContextPieces) ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the prefix operator"), quote(Functor),
        words("should precede a single goal."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

should_have_two_terms_infix(ContextPieces, Context, Functor) = Spec :-
    Pieces = cord.list(ContextPieces) ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the infix operator"), quote(Functor),
        words("should have two terms as arguments."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

should_have_two_goals_infix(ContextPieces, Context, Functor) = Spec :-
    Pieces = cord.list(ContextPieces) ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the infix operator"), quote(Functor),
        words("should have two goals as arguments."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

should_have_one_x_one_goal_prefix(ContextPieces, Context, X, Functor) = Spec :-
    Pieces = cord.list(ContextPieces) ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the binary prefix operator"), quote(Functor),
        words("should precede"), words(X), words("and a goal."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

:- func should_have_one_call_prefix(cord(format_component),
    term.context, string) = error_spec.

should_have_one_call_prefix(ContextPieces, Context, Functor) = Spec :-
    Pieces = cord.list(ContextPieces) ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the prefix operator"), quote(Functor),
        words("should precede a call."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(Context, [always(Pieces)])]).

%---------------------------------------------------------------------------%

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
            ; Goal0 = disable_warnings_expr(_, _, _, _)
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

%---------------------------------------------------------------------------%

:- pred parse_one_plain_or_dot_var(
    plain_state_dot_colon_vars(prog_var_type)::in, goal::in,
    cord(format_component)::in, string::in, maybe1(plain_or_dot_var)::out)
    is det.

parse_one_plain_or_dot_var(PSDCVars, Goal, ContextPieces, ConstructName,
        MaybePODVar) :-
    PSDCVars = plain_state_dot_colon_vars(PlainVars, StateVars,
        DotVars, ColonVars),
    Context = goal_get_context(Goal),
    (
        StateVars = [],
        MaybeStateVars = ok1(unit)
    ;
        StateVars = [_ | _],
        StatePieces = cord.list(ContextPieces) ++
            [words("Error: the first argument of"), words(ConstructName),
            words("may not contain a state variable pair."), nl],
        StateSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(StatePieces)])]),
        MaybeStateVars = error1([StateSpec])
    ),
    (
        ColonVars = [],
        MaybeColonVars = ok1(unit)
    ;
        ColonVars = [_ | _],
        ColonPieces = cord.list(ContextPieces) ++
            [words("Error: the first argument of"), words(ConstructName),
            words("may not contain a reference to the next value"),
            words("of a state variable."), nl],
        ColonSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(ColonPieces)])]),
        MaybeColonVars = error1([ColonSpec])
    ),
    (
        (
            PlainVars = [],
            MaybeMaybePlainVar = ok1(no)
        ;
            PlainVars = [PlainVar0],
            MaybeMaybePlainVar = ok1(yes(PlainVar0))
        )
    ;
        PlainVars = [_, _ | _],
        PlainPieces = cord.list(ContextPieces) ++
            [words("Error: the first argument of"), words(ConstructName),
            words("may not contain more than one variable."), nl],
        PlainSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(PlainPieces)])]),
        MaybeMaybePlainVar = error1([PlainSpec])
    ),
    (
        (
            DotVars = [],
            MaybeMaybeDotVar = ok1(no)
        ;
            DotVars = [DotVar0],
            MaybeMaybeDotVar = ok1(yes(DotVar0))
        )
    ;
        DotVars = [_, _ | _],
        DotPieces = cord.list(ContextPieces) ++
            [words("Error: the first argument of"), words(ConstructName),
            words("may not contain more than one variable."), nl],
        DotSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(DotPieces)])]),
        MaybeMaybeDotVar = error1([DotSpec])
    ),
    ( if
        MaybeStateVars = ok1(_),
        MaybeColonVars = ok1(_),
        MaybeMaybePlainVar = ok1(MaybePlainVar),
        MaybeMaybeDotVar = ok1(MaybeDotVar)
    then
        (
            MaybePlainVar = no,
            MaybeDotVar = no,
            Pieces = cord.list(ContextPieces) ++
                [words("Error: the first argument of"), words(ConstructName),
                words("must contain a variable."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybePODVar = error1([Spec])
        ;
            MaybePlainVar = yes(PlainVar),
            MaybeDotVar = no,
            MaybePODVar = ok1(podv_plain(PlainVar))
        ;
            MaybePlainVar = no,
            MaybeDotVar = yes(DotVar),
            MaybePODVar = ok1(podv_dot(DotVar))
        ;
            MaybePlainVar = yes(_),
            MaybeDotVar = yes(_),
            Pieces = cord.list(ContextPieces) ++
                [words("Error: the first argument of"), words(ConstructName),
                words("may not contain more than one variable."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybePODVar = error1([Spec])
        )
    else
        Specs =
            get_any_errors1(MaybeStateVars) ++
            get_any_errors1(MaybeColonVars) ++
            get_any_errors1(MaybeMaybePlainVar) ++
            get_any_errors1(MaybeMaybeDotVar),
        MaybePODVar = error1(Specs)
    ).

%---------------------------------------------------------------------------%

parse_some_vars_goal(Term, ContextPieces, MaybeVarsAndGoal, !VarSet) :-
    % We parse existentially quantified goals in non-DCG contexts here,
    % while we parse them in DCG contexts in parse_some_vars_dcg_goal
    % in parse_dcg_goal.m.
    ( if
        Term = term.functor(term.atom("some"), [VarsTerm, SubGoalTerm],
            _Context)
    then
        varset.coerce(!.VarSet, GenericVarSet),
        VarsTailPieces = [lower_case_next_if_not_first,
            words("In first argument of"), quote("some"), suffix(":")],
        VarsContextPieces = ContextPieces ++ cord.from_list(VarsTailPieces),
        parse_vars_state_vars(VarsTerm, GenericVarSet, VarsContextPieces,
            MaybeVars),
        GoalTerm = SubGoalTerm
    else
        MaybeVars = ok1(plain_state_vars([], [])),
        GoalTerm = Term
    ),
    parse_goal(GoalTerm, ContextPieces, MaybeGoal, !VarSet),
    ( if
        MaybeVars = ok1(plain_state_vars(Vars0, StateVars0)),
        MaybeGoal = ok1(Goal)
    then
        list.map(term.coerce_var, Vars0, Vars),
        list.map(term.coerce_var, StateVars0, StateVars),
        MaybeVarsAndGoal = ok3(Vars, StateVars, Goal)
    else
        Specs = get_any_errors1(MaybeVars) ++
            get_any_errors1(MaybeGoal),
        MaybeVarsAndGoal = error3(Specs)
    ).

%---------------------------------------------------------------------------%

:- pred parse_warnings(varset::in, term::in, string::in,
    cord(format_component)::in, int::in, maybe1(list(goal_warning))::out)
    is det.

parse_warnings(VarSet, Term, ScopeFunctor, ContextPieces, WarningNum,
        MaybeWarnings) :-
    ( if Term = term.functor(term.atom("[]"), [], _) then
        MaybeWarnings = ok1([])
    else if Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _) then
        parse_warning(VarSet, HeadTerm, ScopeFunctor, ContextPieces,
            WarningNum, MaybeHeadWarning),
        parse_warnings(VarSet, TailTerm, ScopeFunctor, ContextPieces,
            WarningNum + 1, MaybeTailWarnings),
        ( if
            MaybeHeadWarning = ok1(HeadWarning),
            MaybeTailWarnings = ok1(TailWarnings)
        then
            MaybeWarnings = ok1([HeadWarning | TailWarnings])
        else
            Specs = get_any_errors1(MaybeHeadWarning) ++
                get_any_errors1(MaybeTailWarnings),
            MaybeWarnings = error1(Specs)
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = cord.list(ContextPieces) ++
            [lower_case_next_if_not_first, words("Error:"),
            words("the"), quote(ScopeFunctor), words("keyword should be"),
            words("followed by a list of warnings to disable."),
            words("The term"), quote(TermStr), words("is not a list."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeWarnings = error1([Spec])
    ).

:- pred parse_warning(varset::in, term::in, string::in,
    cord(format_component)::in, int::in, maybe1(goal_warning)::out) is det.

parse_warning(VarSet, Term, ScopeFunctor, ContextPieces, WarningNum,
        MaybeWarning) :-
    ( if 
        Term = term.functor(term.atom(WarningFunctor), [], _),
        (
            WarningFunctor = "non_tail_recursive_calls",
            Warning = goal_warning_non_tail_recursive_calls
        ;
            WarningFunctor = "singleton_vars",
            Warning = goal_warning_singleton_vars
        )
    then
        MaybeWarning = ok1(Warning)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = cord.list(ContextPieces) ++
            [lower_case_next_if_not_first, words("Error:"),
            words("the"), nth_fixed(WarningNum), words("argument of the"),
            quote(ScopeFunctor), words("keyword should be"),
            words("the name of a warning to disable."),
            words("The term"), quote(TermStr),
            words("is not the name of a warning."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeWarning = error1([Spec])
    ).

:- pred generate_warnings_for_duplicate_warnings(prog_context::in,
    cord(format_component)::in, assoc_list(goal_warning, int)::in,
    list(goal_warning)::out, list(error_spec)::out) is det.

generate_warnings_for_duplicate_warnings(_, _, [], [], []).
generate_warnings_for_duplicate_warnings(Context, ContextPieces,
        [WarningCount | WarningsCounts], NonDupWarnings, DupSpecs) :-
    generate_warnings_for_duplicate_warnings(Context, ContextPieces,
        WarningsCounts, TailNonDupWarnings, TailDupSpecs),
    WarningCount = Warning - Count,
    ( if Count > 1 then
        WarningStr = goal_warning_to_string(Warning),
        ( if Count > 2 then
            MoreThanOnce = "more than once"
        else
            MoreThanOnce = ""
        ),
        Pieces = cord.list(ContextPieces) ++
            [lower_case_next_if_not_first, words("Error:"),
            words("the warning"), fixed(WarningStr),
            words("is duplicated"), words(MoreThanOnce),
            words("in the list of warnings to disable."), nl],
        Spec = error_spec(severity_warning, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),

        NonDupWarnings = TailNonDupWarnings,
        DupSpecs = [Spec | TailDupSpecs]
    else
        NonDupWarnings = [Warning | TailNonDupWarnings],
        DupSpecs = TailDupSpecs
    ).

%---------------------------------------------------------------------------%

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
            Specs = get_any_errors1(MaybeHeadComponentContext) ++
                get_any_errors1(MaybeTailComponentsContexts),
            MaybeComponentsContexts = error1(Specs)
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
                    Specs = get_any_errors1(MaybeVar) ++
                        get_any_errors1(MaybeMutable),
                    MaybeComponentContext = error1(Specs)
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
            Specs = get_any_errors1(MaybeLExpr) ++
                get_any_errors1(MaybeRExpr),
            MaybeTree = error1(Specs)
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
        else if ( Atom = "tracelevel" ; Atom = "trace_level" ) then
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
                quote(TermStr), suffix("."), nl,
                words("The acceptable compile_time paramaters"),
                words("have one of the following forms:"), nl,
                quote("flag(""name of --trace-flag parameter"")"), nl,
                quote("grade(""grade name"")"), nl,
                quote("tracelevel(shallow)"), nl,
                quote("tracelevel(deep)"), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(TermContext, [always(Pieces)])]),
            MaybeCompiletime = error1([Spec])
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: invalid compile_time parameter"),
            quote(TermStr), suffix("."), nl,
            words("The acceptable compile_time paramaters"),
            words("have one of the following forms:"), nl,
            quote("flag(""name of --trace-flag parameter"")"), nl,
            quote("grade(""grade name"")"), nl,
            quote("tracelevel(shallow)"), nl,
            quote("tracelevel(deep)"), nl],
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
                quote(TermStr), suffix("."), nl,
                words("The only acceptable run_time paramaters have the form"),
                quote("env(""name of an environment variable"")"), nl],
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
            Pieces = [words("Error: duplicate compile_time trace parameter."),
                nl],
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
            Pieces = [words("Error: duplicate run_time trace parameter."), nl],
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
            Pieces = [words("Error: duplicate io trace parameter."), nl],
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

%---------------------------------------------------------------------------%

:- pred parse_catch_any_term(term::in, term.context::in,
    cord(format_component)::in, maybe1(catch_any_expr)::out,
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
    maybe(catch_any_expr)::in, term.context::in, cord(format_component)::in,
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
    cord(format_component)::in, maybe1(list(catch_expr))::out,
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
    cord(format_component)::in, maybe1(catch_expr)::out,
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
    maybe(catch_any_expr)::in, term.context::in, cord(format_component)::in,
    maybe1(goal)::out, prog_varset::in, prog_varset::out) is det.

parse_else_then_try_term(Term, CatchExprs, MaybeCatchAnyExpr,
        Context, ContextPieces, MaybeGoal, !VarSet) :-
    % `else' part may or may not exist in `try' goals.
    ( if Term = term.functor(term.atom("else"), [ThenTerm, ElseTerm], _) then
        parse_goal(ElseTerm, ContextPieces, MaybeElseGoal0, !VarSet),
        (
            MaybeElseGoal0 = ok1(ElseGoal),
            parse_then_try_term(ThenTerm, yes(ElseGoal), CatchExprs,
                MaybeCatchAnyExpr, Context, ContextPieces, MaybeTryGoal,
                !VarSet),
            ( if
                MaybeTryGoal = error1(_),
                ThenTerm = term.functor(term.atom("then"), [_, _], ThenContext)
            then
                Pieces = [words("Error: malformed if-then-else;"),
                    words("this"), quote("then"),
                    words("is missing its"), quote("if"), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(ThenContext, [always(Pieces)])]),
                MaybeGoal = error1([Spec])
            else
                MaybeGoal = MaybeTryGoal
            )
        ;
            MaybeElseGoal0 = error1(Specs),
            MaybeGoal = error1(Specs)
        )
    else
        parse_then_try_term(Term, no, CatchExprs, MaybeCatchAnyExpr,
            Context, ContextPieces, MaybeGoal, !VarSet)
    ).

:- pred parse_then_try_term(term::in, maybe(goal)::in, list(catch_expr)::in,
    maybe(catch_any_expr)::in, term.context::in, cord(format_component)::in,
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
            quote("else failure_goal"), suffix(","),
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
        Pieces = [words("Error: duplicate io try parameter."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    convert_try_params_2(ComponentsContexts, !.MaybeIO, !.Specs, MaybeParams).

%---------------------------------------------------------------------------%

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
            Specs = get_any_errors1(MaybeHeadComponent) ++
                get_any_errors1(MaybeTailComponentsContexts),
            MaybeComponentsContexts = error1(Specs)
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
                    ContextPieces = cord.from_list([words("In"), quote("vars"),
                        words("specifier of atomic scope:")]),
                    parse_possibly_repeated_vars(SubTerm, VarSet,
                        ContextPieces, MaybeVars),
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
            Pieces = [words("Error: duplicate outer atomic parameter."), nl],
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
            Pieces = [words("Error: duplicate inner atomic parameter."), nl],
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
            Pieces = [words("Error: duplicate atomic vars parameter."), nl],
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
            Specs = get_any_errors1(MaybeLeftGoalList) ++
                get_any_errors1(MaybeRightGoalList),
            MaybeGoals = error1(Specs)
        )
    else
        % XXX Provide better ContextPieces.
        ContextPieces = cord.init,
        parse_goal(Term, ContextPieces, MaybeSubGoal, !VarSet),
        (
            MaybeSubGoal = ok1(SubGoal),
            MaybeGoals = ok1([SubGoal])
        ;
            MaybeSubGoal = error1(Specs),
            MaybeGoals = error1(Specs)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_goal.
%---------------------------------------------------------------------------%
