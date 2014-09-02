%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2001, 2003-2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_dcg.m.
% Main authors: fjh, zs.
%
% This module handles the parsing of clauses in Definite Clause Grammar
% notation.
%
% XXX This module performs no error checking.
% XXX It may be an idea to recode this as a state variable transformation:
% roughly     Head --> G1, G2, {G3}, G4.
% becomes     Head(!DCG) :- G1(!DCG), G2(!DCG), G3, G4(!DCG).
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io_dcg.
:- interface.

:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io_util.

:- import_module list.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- pred parse_dcg_clause(module_name::in, varset::in, term::in, term::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

    % parse_dcg_pred_goal(GoalTerm, MaybeGoal, DCGVarInitial, DCGVarFinal,
    %   !Varset):
    %
    % Parses `GoalTerm' and expands it as a DCG goal.
    % `DCGVarInitial' is the first DCG variable,
    % and `DCGVarFinal' is the final DCG variable.
    %
:- pred parse_dcg_pred_goal(term::in, list(format_component)::in,
    maybe1(goal)::out, prog_var::out, prog_var::out,
    prog_varset::in, prog_varset::out) is det.

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_io_goal.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.prog_out.

:- import_module counter.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

parse_dcg_clause(ModuleName, VarSet0, DCG_Head, DCG_Body, DCG_Context,
        SeqNum, MaybeItem) :-
    varset.coerce(VarSet0, ProgVarSet0),
    new_dcg_var(ProgVarSet0, ProgVarSet1, counter.init(0), Counter0,
        DCG_0_Var),
    % XXX Should this be [words("In DCG clause body:")]?
    BodyContextPieces = [],
    parse_dcg_goal(DCG_Body, BodyContextPieces, MaybeBody,
        ProgVarSet1, ProgVarSet, Counter0, _Counter, DCG_0_Var, DCG_Var),
    (
        MaybeBody = ok1(Body),
        HeadContextPieces = [words("In DCG clause head:")],
        parse_implicitly_qualified_sym_name_and_args(ModuleName, DCG_Head,
            VarSet0, HeadContextPieces, HeadResult),
        process_dcg_clause(HeadResult, ProgVarSet, DCG_0_Var, DCG_Var, Body,
            DCG_Context, SeqNum, MaybeItem)
    ;
        MaybeBody = error1(Specs),
        MaybeItem = error1(Specs)
    ).

%-----------------------------------------------------------------------------%

parse_dcg_pred_goal(GoalTerm, ContextPieces, MaybeGoal,
        DCGVar0, DCGVar, !VarSet) :-
    new_dcg_var(!VarSet, counter.init(0), Counter0, DCGVar0),
    parse_dcg_goal(GoalTerm, ContextPieces, MaybeGoal, !VarSet,
        Counter0, _Counter, DCGVar0, DCGVar).

%-----------------------------------------------------------------------------%

    % Used to allocate fresh variables needed for the DCG expansion.
    %
:- pred new_dcg_var(prog_varset::in, prog_varset::out,
    counter::in, counter::out, prog_var::out) is det.

new_dcg_var(!VarSet, !Counter, DCG_0_Var) :-
    counter.allocate(N, !Counter),
    string.int_to_string(N, StringN),
    string.append("DCG_", StringN, VarName),
    varset.new_var(DCG_0_Var, !VarSet),
    varset.name_var(DCG_0_Var, VarName, !VarSet).

%-----------------------------------------------------------------------------%

    % Expand a DCG goal.
    %
:- pred parse_dcg_goal(term::in, list(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

parse_dcg_goal(Term, ContextPieces, MaybeGoal, !VarSet, !Counter, !Var) :-
    % First, figure out the context for the goal.
    (
        Term = term.functor(_, _, Context)
    ;
        Term = term.variable(_, Context)
    ),
    % Next, parse it.
    (
        term.coerce(Term, ProgTerm),
        try_parse_sym_name_and_args(ProgTerm, SymName, Args0)
    ->
        % First check for the special cases:
        (
            SymName = unqualified(Functor),
            list.map(term.coerce, Args0, Args1),
            parse_dcg_goal_2(Functor, Args1, Context, ContextPieces,
                MaybeGoalPrime, !VarSet, !Counter, !Var)
        ->
            MaybeGoal = MaybeGoalPrime
        ;
            % It's the ordinary case of non-terminal. Create a fresh var
            % as the DCG output var from this goal, and append the DCG argument
            % pair to the non-terminal's argument list.
            new_dcg_var(!VarSet, !Counter, Var),
            Args = Args0 ++
                [term.variable(!.Var, Context), term.variable(Var, Context)],
            Goal = call_expr(SymName, Args, purity_pure) - Context,
            MaybeGoal = ok1(Goal),
            !:Var = Var
        )
    ;
        % A call to a free variable, or to a number or string.
        % Just translate it into a call to call/3 - the typechecker
        % will catch calls to numbers and strings.
        new_dcg_var(!VarSet, !Counter, Var),
        term.coerce(Term, ProgTerm),
        Goal = call_expr(unqualified("call"),
            [ProgTerm, variable(!.Var, Context), variable(Var, Context)],
            purity_pure) - Context,
        MaybeGoal = ok1(Goal),
        !:Var = Var
    ).

    % parse_dcg_goal_2(Functor, Args, Context, ContextPieces, Goal,
    %   !VarSet, !Counter, !Var):
    %
    % We use !VarSet to allocate fresh DCG variables. We use !Counter
    % to keep track of the number to give to the next DCG variable
    % (so that we can give it a semi-meaningful name "DCG_<N>" for use
    % in error messages, debugging, etc.). We use !Var to keep track of
    % the current DCG variable.
    %
    % Since (A -> B) has different semantics in standard Prolog
    % (A -> B ; fail) than it does in NU-Prolog or Mercury (A -> B ; true),
    % for the moment we'll just disallow it.
    %
:- pred parse_dcg_goal_2(string::in, list(term)::in, prog_context::in,
    list(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is semidet.

    % XXX We should update ContextPieces as we recurse.
parse_dcg_goal_2("{}", [G0 | Gs], Context, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !Var) :-
    % Ordinary goal inside { curly braces }.
    % The parser treats '{}/N' terms as tuples, so we need
    % to undo the parsing of the argument conjunction here.
    list_to_conjunction(Context, G0, Gs, G),
    parse_goal(G, ContextPieces, MaybeGoal, !VarSet).
parse_dcg_goal_2("impure", [G], _, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !Var) :-
    parse_dcg_goal_with_purity(G, purity_impure, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !Var).
parse_dcg_goal_2("semipure", [G], _, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !Var) :-
    parse_dcg_goal_with_purity(G, purity_semipure, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !Var).
parse_dcg_goal_2("promise_pure", [G], Context, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !Var) :-
    parse_dcg_goal(G, ContextPieces, MaybeGoal0, !VarSet, !Counter, !Var),
    (
        MaybeGoal0 = ok1(Goal0),
        Goal = promise_purity_expr(purity_pure, Goal0) - Context,
        MaybeGoal = ok1(Goal)
    ;
        MaybeGoal0 = error1(Specs),
        MaybeGoal = error1(Specs)
    ).
parse_dcg_goal_2("promise_semipure", [G], Context, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !Var) :-
    parse_dcg_goal(G, ContextPieces, MaybeGoal0, !VarSet, !Counter, !Var),
    (
        MaybeGoal0 = ok1(Goal0),
        Goal = promise_purity_expr(purity_semipure, Goal0) - Context,
        MaybeGoal = ok1(Goal)
    ;
        MaybeGoal0 = error1(Specs),
        MaybeGoal = error1(Specs)
    ).
parse_dcg_goal_2("promise_impure", [G], Context, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !Var) :-
    parse_dcg_goal(G, ContextPieces, MaybeGoal0, !VarSet, !Counter, !Var),
    (
        MaybeGoal0 = ok1(Goal0),
        Goal = promise_purity_expr(purity_impure, Goal0) - Context,
        MaybeGoal = ok1(Goal)
    ;
        MaybeGoal0 = error1(Specs),
        MaybeGoal = error1(Specs)
    ).
parse_dcg_goal_2("[]", [], Context, _CP, MaybeGoal, !VarSet, !Counter,
        Var0, Var) :-
    % Empty list - just unify the input and output DCG args.
    new_dcg_var(!VarSet, !Counter, Var),
    Goal = unify_expr(
        term.variable(Var0, Context), term.variable(Var, Context), purity_pure)
        - Context,
    MaybeGoal = ok1(Goal).
parse_dcg_goal_2("[|]", [X, Xs], Context, _CP, MaybeGoal, !VarSet, !Counter,
        Var0, Var) :-
    % Non-empty list of terminals. Append the DCG output arg as the new tail
    % of the list, and unify the result with the DCG input arg.
    new_dcg_var(!VarSet, !Counter, Var),
    ConsTerm0 = term.functor(term.atom("[|]"), [X, Xs], Context),
    term.coerce(ConsTerm0, ConsTerm),
    term_list_append_term(ConsTerm, term.variable(Var, Context), Term),
    Goal = unify_expr(variable(Var0, Context), Term, purity_pure) - Context,
    MaybeGoal = ok1(Goal).
parse_dcg_goal_2("=", [A0], Context, _CP, MaybeGoal, !VarSet, !Counter,
        Var, Var) :-
    % Call to '='/1 - unify argument with DCG input arg.
    term.coerce(A0, A),
    Goal = unify_expr(A, variable(Var, Context), purity_pure) - Context,
    MaybeGoal = ok1(Goal).
parse_dcg_goal_2(":=", [A0], Context, _CP, MaybeGoal, !VarSet, !Counter,
        _Var0, Var) :-
    % Call to ':='/1 - unify argument with DCG output arg.
    new_dcg_var(!VarSet, !Counter, Var),
    term.coerce(A0, A),
    Goal = unify_expr(A, variable(Var, Context), purity_pure) - Context,
    MaybeGoal = ok1(Goal).
parse_dcg_goal_2("if", Args, Context, ContextPieces, MaybeGoal,
        !VarSet, !Counter, Var0, Var) :-
    Args = [term.functor(term.atom("then"), [CondTerm, ThenTerm], _)],
    % If-then (NU-Prolog syntax).
    parse_dcg_if_then(CondTerm, ThenTerm, Context, ContextPieces,
        MaybeVarsCond, MaybeThen, !VarSet, !Counter, Var0, Var),
    (
        MaybeVarsCond = ok3(Vars, StateVars, Cond),
        MaybeThen = ok1(Then)
    ->
        ( Var = Var0 ->
            Else = true_expr - Context
        ;
            Unify = unify_expr(variable(Var, Context), variable(Var0, Context),
                purity_pure),
            Else = Unify - Context
        ),
        Goal = if_then_else_expr(Vars, StateVars, Cond, Then, Else) - Context,
        MaybeGoal = ok1(Goal)
    ;
        CondSpecs = get_any_errors3(MaybeVarsCond),
        ThenSpecs = get_any_errors1(MaybeThen),
        MaybeGoal = error1(CondSpecs ++ ThenSpecs)
    ).
parse_dcg_goal_2(",", [ATerm, BTerm], Context, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !Var) :-
    % Conjunction.
    parse_dcg_goal(ATerm, ContextPieces, MaybeAGoal, !VarSet, !Counter, !Var),
    parse_dcg_goal(BTerm, ContextPieces, MaybeBGoal, !VarSet, !Counter, !Var),
    (
        MaybeAGoal = ok1(AGoal),
        MaybeBGoal = ok1(BGoal)
    ->
        Goal = conj_expr(AGoal, BGoal) - Context,
        MaybeGoal = ok1(Goal)
    ;
        ASpecs = get_any_errors1(MaybeAGoal),
        BSpecs = get_any_errors1(MaybeBGoal),
        MaybeGoal = error1(ASpecs ++ BSpecs)
    ).
parse_dcg_goal_2("&", [ATerm, BTerm], Context, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !Var) :-
    parse_dcg_goal(ATerm, ContextPieces, MaybeAGoal, !VarSet, !Counter, !Var),
    parse_dcg_goal(BTerm, ContextPieces, MaybeBGoal, !VarSet, !Counter, !Var),
    (
        MaybeAGoal = ok1(AGoal),
        MaybeBGoal = ok1(BGoal)
    ->
        Goal = par_conj_expr(AGoal, BGoal) - Context,
        MaybeGoal = ok1(Goal)
    ;
        ASpecs = get_any_errors1(MaybeAGoal),
        BSpecs = get_any_errors1(MaybeBGoal),
        MaybeGoal = error1(ASpecs ++ BSpecs)
    ).
parse_dcg_goal_2(";", [ATerm, BTerm], Context, ContextPieces, MaybeGoal,
        !VarSet, !Counter, Var0, Var) :-
    % Disjunction or if-then-else (Prolog syntax).
    ( ATerm = term.functor(term.atom("->"), [CondTerm, ThenTerm], _Context) ->
        parse_dcg_if_then_else(CondTerm, ThenTerm, BTerm, Context,
            ContextPieces, MaybeGoal, !VarSet, !Counter, Var0, Var)
    ;
        parse_dcg_goal(ATerm, ContextPieces, MaybeAGoal0,
            !VarSet, !Counter, Var0, VarA),
        parse_dcg_goal(BTerm, ContextPieces, MaybeBGoal0,
            !VarSet, !Counter, Var0, VarB),
        (
            MaybeAGoal0 = ok1(AGoal0),
            MaybeBGoal0 = ok1(BGoal0)
        ->
            ( VarA = Var0, VarB = Var0 ->
                Var = Var0,
                Goal = disj_expr(AGoal0, BGoal0) - Context
            ; VarA = Var0 ->
                Var = VarB,
                Unify = unify_expr(
                    term.variable(Var, Context), term.variable(VarA, Context),
                    purity_pure),
                append_to_disjunct(AGoal0, Unify, Context, AGoal),
                Goal = disj_expr(AGoal, BGoal0) - Context
            ; VarB = Var0 ->
                Var = VarA,
                Unify = unify_expr(
                    term.variable(Var, Context), term.variable(VarB, Context),
                    purity_pure),
                append_to_disjunct(BGoal0, Unify, Context, BGoal),
                Goal = disj_expr(AGoal0, BGoal) - Context
            ;
                Var = VarB,
                prog_util.rename_in_goal(VarA, VarB, AGoal0, AGoal),
                Goal = disj_expr(AGoal, BGoal0) - Context
            ),
            MaybeGoal = ok1(Goal)
        ;
            Var = VarA,         % Dummy; the value shouldn't matter.
            ASpecs = get_any_errors1(MaybeAGoal0),
            BSpecs = get_any_errors1(MaybeBGoal0),
            MaybeGoal = error1(ASpecs ++ BSpecs)
        )
    ).
parse_dcg_goal_2("else", [IfTerm, ElseTerm], _, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !Var) :-
    % If-then-else (NU-Prolog syntax).
    IfTerm = term.functor(term.atom("if"),
        [term.functor(term.atom("then"), [CondTerm, ThenTerm], _)], Context),
    parse_dcg_if_then_else(CondTerm, ThenTerm, ElseTerm, Context,
        ContextPieces, MaybeGoal, !VarSet, !Counter, !Var).
parse_dcg_goal_2("not", [ATerm], Context, ContextPieces, MaybeGoal,
        !VarSet, !Counter, Var0, Var0) :-
    % Negation (NU-Prolog syntax).
    parse_dcg_goal(ATerm, ContextPieces, MaybeAGoal, !VarSet, !Counter,
        Var0, _),
    (
        MaybeAGoal = ok1(AGoal),
        Goal = not_expr(AGoal) - Context,
        MaybeGoal = ok1(Goal)
    ;
        MaybeAGoal = error1(Specs),
        MaybeGoal = error1(Specs)
    ).
parse_dcg_goal_2("\\+", [ATerm], Context, ContextPieces, MaybeGoal,
        !VarSet, !Counter, Var0, Var0) :-
    % Negation (Prolog syntax).
    parse_dcg_goal(ATerm, ContextPieces, MaybeAGoal, !VarSet, !Counter,
        Var0, _),
    (
        MaybeAGoal = ok1(AGoal),
        Goal = not_expr(AGoal) - Context,
        MaybeGoal = ok1(Goal)
    ;
        MaybeAGoal = error1(Specs),
        MaybeGoal = error1(Specs)
    ).
parse_dcg_goal_2("all", [QVarsTerm, SubTerm], Context, ContextPieces,
        MaybeGoal, !VarSet, !Counter, !Var) :-
    % Universal quantification.
    % Extract any state variables in the quantifier.
    varset.coerce(!.VarSet, GenericVarSet),
    parse_quantifier_vars(QVarsTerm, GenericVarSet, ContextPieces,
        MaybeStateVarsAndVars),
    parse_dcg_goal(SubTerm, ContextPieces, MaybeSubGoal,
        !VarSet, !Counter, !Var),
    (
        MaybeStateVarsAndVars = ok2(Vars0, StateVars0),
        MaybeSubGoal = ok1(SubGoal)
    ->
        list.map(term.coerce_var, StateVars0, StateVars),
        list.map(term.coerce_var, Vars0, Vars),
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
            Vars = [_ | _],
            StateVars = [_ | _],
            GoalExpr = all_expr(Vars, all_state_vars_expr(StateVars, SubGoal)
                - SubContext)
        ),
        Goal = GoalExpr - Context,
        MaybeGoal = ok1(Goal)
    ;
        VarsSpecs = get_any_errors2(MaybeStateVarsAndVars),
        SubGoalSpecs = get_any_errors1(MaybeSubGoal),
        MaybeGoal = error1(VarsSpecs ++ SubGoalSpecs)
    ).
parse_dcg_goal_2("some", [QVarsTerm, SubTerm], Context, ContextPieces,
        MaybeGoal, !VarSet, !Counter, !Var) :-
    % Existential quantification.
    % Extract any state variables in the quantifier.
    varset.coerce(!.VarSet, GenericVarSet),
    parse_quantifier_vars(QVarsTerm, GenericVarSet, ContextPieces,
        MaybeStateVarsAndVars),
    parse_dcg_goal(SubTerm, ContextPieces, MaybeSubGoal,
        !VarSet, !Counter, !Var),
    (
        MaybeStateVarsAndVars = ok2(Vars0, StateVars0),
        MaybeSubGoal = ok1(SubGoal)
    ->
        list.map(term.coerce_var, StateVars0, StateVars),
        list.map(term.coerce_var, Vars0, Vars),
        SubGoal = SubGoalExpr - SubContext,
        (
            Vars = [],
            StateVars = [],
            SubGoalExpr = GoalExpr
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
        VarsSpecs = get_any_errors2(MaybeStateVarsAndVars),
        SubGoalSpecs = get_any_errors1(MaybeSubGoal),
        MaybeGoal = error1(VarsSpecs ++ SubGoalSpecs)
    ).

:- pred parse_dcg_goal_with_purity(term::in, purity::in,
    list(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

parse_dcg_goal_with_purity(G, Purity, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !Var) :-
    parse_dcg_goal(G, ContextPieces, MaybeGoal1, !VarSet, !Counter, !Var),
    (
        MaybeGoal1 = ok1(Goal1),
        ( Goal1 = call_expr(Pred, Args, purity_pure) - Context ->
            Goal = call_expr(Pred, Args, Purity) - Context
        ; Goal1 = unify_expr(ProgTerm1, ProgTerm2, purity_pure) - Context ->
            Goal = unify_expr(ProgTerm1, ProgTerm2, Purity) - Context
        ;
            % Inappropriate placement of an impurity marker, so we treat
            % it like a predicate call. typecheck.m prints out something
            % descriptive for these errors.
            Goal1 = _ - Context,
            purity_name(Purity, PurityString),
            term.coerce(G, G1),
            Goal = call_expr(unqualified(PurityString), [G1], purity_pure)
                - Context
        ),
        MaybeGoal = ok1(Goal)
    ;
        MaybeGoal1 = error1(Specs),
        MaybeGoal = error1(Specs)
    ).

:- pred append_to_disjunct(goal::in, goal_expr::in, prog_context::in,
    goal::out) is det.

append_to_disjunct(Disjunct0, Goal, Context, Disjunct) :-
    ( Disjunct0 = disj_expr(A0, B0) - Context2 ->
        append_to_disjunct(A0, Goal, Context, A),
        append_to_disjunct(B0, Goal, Context, B),
        Disjunct = disj_expr(A, B) - Context2
    ;
        Disjunct = conj_expr(Disjunct0, Goal - Context) - Context
    ).

:- pred parse_some_vars_dcg_goal(term::in, list(format_component)::in,
    maybe3(list(prog_var), list(prog_var), goal)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

parse_some_vars_dcg_goal(Term, ContextPieces, MaybeVarsGoal,
        !VarSet, !Counter, !Var) :-
    ( Term = term.functor(term.atom("some"), [VarsTerm, SubTerm], _Context) ->
        % XXX We should update ContextPieces.
        varset.coerce(!.VarSet, GenericVarSet),
        parse_quantifier_vars(VarsTerm, GenericVarSet, ContextPieces,
            MaybeVars),
        GoalTerm = SubTerm
    ;
        MaybeVars = ok2([], []),
        GoalTerm = Term
    ),
    parse_dcg_goal(GoalTerm, ContextPieces, MaybeGoal, !VarSet, !Counter,
        !Var),
    (
        MaybeVars = ok2(Vars0, StateVars0),
        MaybeGoal = ok1(Goal)
    ->
        list.map(term.coerce_var, Vars0, Vars),
        list.map(term.coerce_var, StateVars0, StateVars),
        MaybeVarsGoal = ok3(Vars, StateVars, Goal)
    ;
        VarsSpecs = get_any_errors2(MaybeVars),
        GoalSpecs = get_any_errors1(MaybeGoal),
        MaybeVarsGoal = error3(VarsSpecs ++ GoalSpecs)
    ).

    % Parse the "if" and the "then" part of an if-then or an if-then-else.
    % If the condition is a DCG goal, but then "then" part is not,
    % then we need to translate
    %   ( a -> { b } ; c )
    % as
    %   ( a(DCG_1, DCG_2) ->
    %       b,
    %       DCG_3 = DCG_2
    %   ;
    %       c(DCG_1, DCG_3)
    %   )
    % rather than
    %   ( a(DCG_1, DCG_2) ->
    %       b
    %   ;
    %       c(DCG_1, DCG_2)
    %   )
    % so that the implicit quantification of DCG_2 is correct.
    %
:- pred parse_dcg_if_then(term::in, term::in, prog_context::in,
    list(format_component)::in,
    maybe3(list(prog_var), list(prog_var), goal)::out,
    maybe1(goal)::out, prog_varset::in, prog_varset::out,
    counter::in, counter::out, prog_var::in, prog_var::out) is det.

parse_dcg_if_then(CondTerm, ThenTerm, Context, ContextPieces,
        MaybeVarsCond, MaybeThen, !VarSet, !Counter, Var0, Var) :-
    parse_some_vars_dcg_goal(CondTerm, ContextPieces, MaybeVarsCond,
        !VarSet, !Counter, Var0, Var1),
    parse_dcg_goal(ThenTerm, ContextPieces, MaybeThen1,
        !VarSet, !Counter, Var1, Var2),
    (
        Var0 \= Var1,
        Var1 = Var2
    ->
        (
            MaybeThen1 = ok1(Then1),
            new_dcg_var(!VarSet, !Counter, Var),
            Unify = unify_expr(
                term.variable(Var, Context), term.variable(Var2, Context),
                purity_pure),
            Then = conj_expr(Then1, Unify - Context) - Context,
            MaybeThen = ok1(Then)
        ;
            MaybeThen1 = error1(_),
            MaybeThen = MaybeThen1,
            Var = Var2                  % Dummy; the value shouldn't matter.
        )
    ;
        MaybeThen = MaybeThen1,
        Var = Var2
    ).

:- pred parse_dcg_if_then_else(term::in, term::in, term::in, prog_context::in,
    list(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

parse_dcg_if_then_else(CondTerm, ThenTerm, ElseTerm, Context, ContextPieces,
        MaybeGoal, !VarSet, !Counter, Var0, Var) :-
    parse_dcg_if_then(CondTerm, ThenTerm, Context, ContextPieces,
        MaybeVarsCond, MaybeThen1, !VarSet, !Counter, Var0, VarThen),
    parse_dcg_goal(ElseTerm, ContextPieces, MaybeElse1,
        !VarSet, !Counter, Var0, VarElse),
    (
        MaybeVarsCond = ok3(Vars, StateVars, Cond),
        MaybeThen1 = ok1(Then1),
        MaybeElse1 = ok1(Else1)
    ->
        ( VarThen = Var0, VarElse = Var0 ->
            Var = Var0,
            Then = Then1,
            Else = Else1
        ; VarThen = Var0 ->
            Var = VarElse,
            Unify = unify_expr(
                term.variable(Var, Context), term.variable(VarThen, Context),
                purity_pure),
            Then = conj_expr(Then1, Unify - Context) - Context,
            Else = Else1
        ; VarElse = Var0 ->
            Var = VarThen,
            Then = Then1,
            Unify = unify_expr(
                term.variable(Var, Context), term.variable(VarElse, Context),
                purity_pure),
            Else = conj_expr(Else1, Unify - Context) - Context
        ;
            % We prefer to substitute the then part since it is likely to be
            % smaller than the else part, since the else part may have a deeply
            % nested chain of if-then-elses.

            % parse_dcg_if_then guarantees that if VarThen \= Var0, then the
            % then part introduces a new DCG variable (i.e. VarThen does not
            % appear in the condition). We therefore don't need to do the
            % substitution in the condition.

            Var = VarElse,
            prog_util.rename_in_goal(VarThen, VarElse, Then1, Then),
            Else = Else1
        ),
        Goal = if_then_else_expr(Vars, StateVars, Cond, Then, Else)
            - Context,
        MaybeGoal = ok1(Goal)
    ;
        CondSpecs = get_any_errors3(MaybeVarsCond),
        ThenSpecs = get_any_errors1(MaybeThen1),
        ElseSpecs = get_any_errors1(MaybeElse1),
        MaybeGoal = error1(CondSpecs ++ ThenSpecs ++ ElseSpecs),
        Var = Var0              % Dummy; the value shouldn't matter.
    ).

    % term_list_append_term(ListTerm, Term, Result):
    %
    % If ListTerm is a term representing a proper list, this predicate
    % will append the term Term onto the end of the list.
    %
:- pred term_list_append_term(term(T)::in, term(T)::in, term(T)::out)
    is semidet.

term_list_append_term(List0, Term, List) :-
    ( List0 = term.functor(term.atom("[]"), [], _Context) ->
        List = Term
    ;
        List0 = term.functor(term.atom("[|]"), [Head, Tail0], Context),
        term_list_append_term(Tail0, Term, Tail),
        List = term.functor(term.atom("[|]"), [Head, Tail], Context)
    ).

:- pred process_dcg_clause(maybe_functor::in, prog_varset::in, prog_var::in,
    prog_var::in, goal::in, prog_context::in, int::in, maybe1(item)::out)
    is det.

process_dcg_clause(MaybeFunctor, VarSet, Var0, Var, Body, Context,
        SeqNum, MaybeItem) :-
    (
        MaybeFunctor = ok2(Name, Args0),
        list.map(term.coerce, Args0, Args1),
        Args = Args1 ++ [variable(Var0, Context), variable(Var, Context)],
        ItemClause = item_clause_info(user, VarSet, pf_predicate, Name, Args,
            Body, Context, SeqNum),
        Item = item_clause(ItemClause),
        MaybeItem = ok1(Item)
    ;
        MaybeFunctor = error2(Specs),
        MaybeItem = error1(Specs)
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_io_dcg.
%-----------------------------------------------------------------------------%
