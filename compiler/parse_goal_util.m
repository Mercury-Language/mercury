%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% Copyright (C) 2016-2024 The Mercury team.
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

:- module parse_tree.parse_goal_util.
:- interface.

:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module cord.
:- import_module list.
:- import_module term.

%---------------------------------------------------------------------------%

:- func should_have_no_args(cord(format_piece),
    term.context, string) = error_spec.
:- func should_have_one_goal_prefix(cord(format_piece),
    term.context, string) = error_spec.
:- func should_have_two_terms_infix(cord(format_piece),
    term.context, string) = error_spec.
:- func should_have_two_goals_infix(cord(format_piece),
    term.context, string) = error_spec.
:- func should_have_one_x_one_goal_prefix(cord(format_piece),
    term.context, string, string) = error_spec.
:- func should_have_one_call_prefix(cord(format_piece),
    term.context, string) = error_spec.

    % apply_purity_marker_to_maybe_goal(GoalTerm, Purity,
    %   MaybeGoal0, MaybeGoal):
    %
    % Given a GoalTerm which has a purity annotation for Purity in front of it,
    % which has been parsed as MaybeGoal0, mark the Goal0 in MaybeGoal0
    % as having the given purity, if it is a goal to which purity annotations
    % are applicable.
    %
:- pred apply_purity_marker_to_maybe_goal(term::in, purity::in,
    maybe2(goal, list(warning_spec))::in,
    maybe2(goal, list(warning_spec))::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_misc.

:- import_module maybe.
:- import_module term_context.

%---------------------------------------------------------------------------%

should_have_no_args(ContextPieces, Context, Functor) = Spec :-
    Pieces = cord.list(ContextPieces) ++
        [lower_case_next_if_not_first, words("Error:")] ++
        color_as_subject([quote(Functor)]) ++
        color_as_incorrect([words("should have no arguments.")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

should_have_one_goal_prefix(ContextPieces, Context, Functor) = Spec :-
    Pieces = cord.list(ContextPieces) ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the prefix operator")] ++
        color_as_subject([quote(Functor)]) ++
        color_as_incorrect([words("should precede a single goal.")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

should_have_two_terms_infix(ContextPieces, Context, Functor) = Spec :-
    Pieces = cord.list(ContextPieces) ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the infix operator")] ++
        color_as_subject([quote(Functor)]) ++
        color_as_incorrect([words("should have two terms as arguments.")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

should_have_two_goals_infix(ContextPieces, Context, Functor) = Spec :-
    Pieces = cord.list(ContextPieces) ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the infix operator")] ++
        color_as_subject([quote(Functor)]) ++
        color_as_incorrect([words("should have two goals as arguments.")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

should_have_one_x_one_goal_prefix(ContextPieces, Context, X, Functor) = Spec :-
    Pieces = cord.list(ContextPieces) ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the binary prefix operator")] ++
        color_as_subject([quote(Functor)]) ++
        color_as_incorrect([words("should precede"), words(X),
            words("and a goal.")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

should_have_one_call_prefix(ContextPieces, Context, Functor) = Spec :-
    Pieces = cord.list(ContextPieces) ++
        [lower_case_next_if_not_first, words("Error:"),
        words("the prefix operator")] ++
        color_as_subject([quote(Functor)]) ++
        color_as_incorrect([words("should precede a call.")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

%---------------------------------------------------------------------------%

apply_purity_marker_to_maybe_goal(GoalTerm, Purity, MaybeGoal0, MaybeGoal) :-
    (
        MaybeGoal0 = ok2(Goal0, WarningSpecs0),
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
            ; Goal0 = disj_expr(_, _, _, _)
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
            Goal = bad_purity_goal(GoalTerm, get_goal_context(Goal0), Purity)
        ),
        MaybeGoal = ok2(Goal, WarningSpecs0)
    ;
        MaybeGoal0 = error2(Specs),
        MaybeGoal = error2(Specs)
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
:- end_module parse_tree.parse_goal_util.
%---------------------------------------------------------------------------%
