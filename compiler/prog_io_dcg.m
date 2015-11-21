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
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io_iom.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- pred parse_dcg_clause(module_name::in, varset::in, term::in, term::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

    % parse_dcg_pred_goal(GoalTerm, MaybeGoal, DCGVar0, DCGVar, !VarSet):
    %
    % Parses `GoalTerm' and expands it as a DCG goal.
    % `DCGVar0' is the initial DCG variable, and `DCGVar' is the final one.
    %
:- pred parse_dcg_pred_goal(term::in, list(format_component)::in,
    maybe1(goal)::out, prog_var::out, prog_var::out,
    prog_varset::in, prog_varset::out) is det.

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_io_goal.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_util.

:- import_module counter.
:- import_module string.

%-----------------------------------------------------------------------------%

parse_dcg_clause(ModuleName, VarSet0, DCG_Head, DCG_Body, Context, SeqNum,
        MaybeIOM) :-
    varset.coerce(VarSet0, ProgVarSet0),
    new_dcg_var(ProgVarSet0, ProgVarSet1, counter.init(0), Counter0,
        DCGVar0),
    % XXX Should this be [words("In DCG clause body:")]?
    BodyContextPieces = [],
    parse_dcg_goal(DCG_Body, BodyContextPieces, MaybeBody,
        ProgVarSet1, ProgVarSet, Counter0, _Counter, DCGVar0, DCGVar),
    (
        MaybeBody = ok1(Body),
        HeadContextPieces = [words("In DCG clause head:")],
        parse_implicitly_qualified_sym_name_and_args(ModuleName, DCG_Head,
            VarSet0, HeadContextPieces, MaybeFunctor),
        (
            MaybeFunctor = ok2(Name, Args0),
            list.map(term.coerce, Args0, Args1),
            Args = Args1 ++
                [term.variable(DCGVar0, Context),
                term.variable(DCGVar, Context)],
            ItemClause = item_clause_info(Name, pf_predicate, Args,
                item_origin_user, ProgVarSet, Body, Context, SeqNum),
            Item = item_clause(ItemClause),
            MaybeIOM = ok1(iom_item(Item))
        ;
            MaybeFunctor = error2(Specs),
            MaybeIOM = error1(Specs)
        )
    ;
        MaybeBody = error1(Specs),
        MaybeIOM = error1(Specs)
    ).

%-----------------------------------------------------------------------------%

parse_dcg_pred_goal(GoalTerm, ContextPieces, MaybeGoal,
        DCGVar0, DCGVar, !VarSet) :-
    new_dcg_var(!VarSet, counter.init(0), Counter0, DCGVar0),
    parse_dcg_goal(GoalTerm, ContextPieces, MaybeGoal, !VarSet,
        Counter0, _Counter, DCGVar0, DCGVar).

%-----------------------------------------------------------------------------%

    % Expand a DCG goal.
    %
:- pred parse_dcg_goal(term::in, list(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

parse_dcg_goal(Term, ContextPieces, MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    % First, figure out the context for the goal.
    ( Term = term.functor(_, _, Context)
    ; Term = term.variable(_, Context)
    ),
    % Next, parse it.
    ( if
        term.coerce(Term, ProgTerm),
        try_parse_sym_name_and_args(ProgTerm, SymName, Args0)
    then
        % First check for the special cases:
        ( if
            SymName = unqualified(Functor),
            list.map(term.coerce, Args0, Args1),
            parse_non_call_dcg_goal(Functor, Args1, Context, ContextPieces,
                MaybeGoalPrime, !VarSet, !Counter, !DCGVar)
        then
            MaybeGoal = MaybeGoalPrime
        else
            % It's the ordinary case of nonterminal. Create a fresh var
            % as the DCG output var from this goal, and append the DCG argument
            % pair to the nonterminal's argument list.
            InVar = !.DCGVar,
            new_dcg_var(!VarSet, !Counter, OutVar),
            !:DCGVar = OutVar,
            Args = Args0 ++
                [term.variable(InVar, Context),
                term.variable(OutVar, Context)],
            Goal = call_expr(Context, SymName, Args, purity_pure),
            MaybeGoal = ok1(Goal)
        )
    else
        % A call to a free variable, or to a number or string.
        % Just translate it into a call to call/3 - the typechecker
        % will catch calls to numbers and strings.
        InVar = !.DCGVar,
        new_dcg_var(!VarSet, !Counter, OutVar),
        !:DCGVar = OutVar,
        term.coerce(Term, ProgTerm),
        Goal = call_expr(Context, unqualified("call"),
            [ProgTerm, variable(InVar, Context), variable(OutVar, Context)],
            purity_pure),
        MaybeGoal = ok1(Goal)
    ).

    % parse_non_call_dcg_goal(Functor, Args, Context, ContextPieces, Goal,
    %   !VarSet, !Counter, !DCGVar):
    %
    % We use !VarSet to allocate fresh DCG variables. We use !Counter
    % to keep track of the number to give to the next DCG variable
    % (so that we can give it a semi-meaningful name "DCG_<N>" for use
    % in error messages, debugging, etc.). We use !DCGVar to keep track of
    % the current DCG variable.
    %
:- pred parse_non_call_dcg_goal(string::in, list(term)::in, prog_context::in,
    list(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is semidet.

parse_non_call_dcg_goal(Functor, Args, Context, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !DCGVar) :-
    % XXX We should update ContextPieces as we recurse.
    (
        Functor = "{}",
        Args = [HeadGoal | TailGoals],
        % Ordinary goal inside { curly braces }.
        % The parser treats '{}/N' terms as tuples, so we need to undo
        % the parsing of the argument conjunction here.
        one_or_more_to_conjunction(Context, one_or_more(HeadGoal, TailGoals),
            SubGoal),
        parse_goal(SubGoal, ContextPieces, MaybeGoal, !VarSet)
    ;
        (
            Functor = "impure",
            Purity = purity_impure
        ;
            Functor = "semipure",
            Purity = purity_semipure
        ),
        Args = [SubGoalTerm],
        parse_dcg_goal(SubGoalTerm, ContextPieces, MaybeGoal0,
            !VarSet, !Counter, !DCGVar),
        apply_purity_marker_to_maybe_goal(SubGoalTerm, Purity,
            MaybeGoal0, MaybeGoal)
    ;
        (
            Functor = "promise_pure",
            PromisedPurity = purity_pure
        ;
            Functor = "promise_semipure",
            PromisedPurity = purity_semipure
        ;
            Functor = "promise_impure",
            PromisedPurity = purity_impure
        ),
        Args = [SubGoalTerm],
        parse_dcg_goal(SubGoalTerm, ContextPieces, MaybeGoal0,
            !VarSet, !Counter, !DCGVar),
        (
            MaybeGoal0 = ok1(Goal0),
            Goal = promise_purity_expr(Context, PromisedPurity, Goal0),
            MaybeGoal = ok1(Goal)
        ;
            MaybeGoal0 = error1(Specs),
            MaybeGoal = error1(Specs)
        )
    ;
        Functor = "[]",
        Args = [],
        % Empty list - just unify the input and output DCG args.
        InVar = !.DCGVar,
        new_dcg_var(!VarSet, !Counter, OutVar),
        !:DCGVar = OutVar,
        Goal = unify_expr(Context,
            term.variable(InVar, Context), term.variable(OutVar, Context),
            purity_pure),
        MaybeGoal = ok1(Goal)
    ;
        Functor = "[|]",
        Args = [X, Xs],
        % Non-empty list of terminals. Append the DCG output arg as the
        % new tail of the list, and unify the result with the DCG input arg.
        InVar = !.DCGVar,
        new_dcg_var(!VarSet, !Counter, OutVar),
        !:DCGVar = OutVar,
        ConsTerm0 = term.functor(term.atom("[|]"), [X, Xs], Context),
        term.coerce(ConsTerm0, ConsTerm),
        term_list_append_term(ConsTerm, term.variable(OutVar, Context), Term),
        Goal = unify_expr(Context, variable(InVar, Context), Term,
            purity_pure),
        MaybeGoal = ok1(Goal)
    ;
        Functor = "=",
        Args = [Arg0],
        % Call to '='/1 - unify argument with DCG input arg.
        term.coerce(Arg0, Arg),
        Goal = unify_expr(Context, Arg, variable(!.DCGVar, Context),
            purity_pure),
        MaybeGoal = ok1(Goal)
    ;
        Functor = ":=",
        Args = [Arg0],
        % Call to ':='/1 - unify argument with DCG output arg.
        new_dcg_var(!VarSet, !Counter, OutVar),
        !:DCGVar = OutVar,
        term.coerce(Arg0, Arg),
        Goal = unify_expr(Context, Arg, variable(OutVar, Context),
            purity_pure),
        MaybeGoal = ok1(Goal)
    ;
        Functor = "if",
        Args = [term.functor(term.atom("then"),
            [CondGoalTerm, ThenGoalTerm], _)],
        % If-then (NU-Prolog syntax).
        InVar = !.DCGVar,
        parse_dcg_if_then(CondGoalTerm, ThenGoalTerm, Context, ContextPieces,
            MaybeVarsCondGoal, MaybeThenGoal, !VarSet, !Counter, !DCGVar),
        OutVar = !.DCGVar,
        ( if
            MaybeVarsCondGoal = ok3(Vars, StateVars, CondGoal),
            MaybeThenGoal = ok1(ThenGoal)
        then
            ( if OutVar = InVar then
                ElseGoal = true_expr(Context)
            else
                ElseGoal = unify_expr(Context,
                    term.variable(OutVar, Context),
                    term.variable(InVar, Context),
                    purity_pure)
            ),
            Goal = if_then_else_expr(Context, Vars, StateVars,
                CondGoal, ThenGoal, ElseGoal),
            MaybeGoal = ok1(Goal)
        else
            CondSpecs = get_any_errors3(MaybeVarsCondGoal),
            ThenSpecs = get_any_errors1(MaybeThenGoal),
            MaybeGoal = error1(CondSpecs ++ ThenSpecs)
        )
    ;
        Functor = ",",
        Args = [SubGoalTermA, SubGoalTermB],
        % Conjunction.
        parse_dcg_goal(SubGoalTermA, ContextPieces, MaybeSubGoalA,
            !VarSet, !Counter, !DCGVar),
        parse_dcg_goal(SubGoalTermB, ContextPieces, MaybeSubGoalB,
            !VarSet, !Counter, !DCGVar),
        ( if
            MaybeSubGoalA = ok1(SubGoalA),
            MaybeSubGoalB = ok1(SubGoalB)
        then
            Goal = conj_expr(Context, SubGoalA, SubGoalB),
            MaybeGoal = ok1(Goal)
        else
            SpecsA = get_any_errors1(MaybeSubGoalA),
            SpecsB = get_any_errors1(MaybeSubGoalB),
            MaybeGoal = error1(SpecsA ++ SpecsB)
        )
    ;
        Functor = "&",
        Args = [SubGoalTermA, SubGoalTermB],
        parse_dcg_goal(SubGoalTermA, ContextPieces, MaybeSubGoalA,
            !VarSet, !Counter, !DCGVar),
        parse_dcg_goal(SubGoalTermB, ContextPieces, MaybeSubGoalB,
            !VarSet, !Counter, !DCGVar),
        ( if
            MaybeSubGoalA = ok1(SubGoalA),
            MaybeSubGoalB = ok1(SubGoalB)
        then
            Goal = par_conj_expr(Context, SubGoalA, SubGoalB),
            MaybeGoal = ok1(Goal)
        else
            SpecsA = get_any_errors1(MaybeSubGoalA),
            SpecsB = get_any_errors1(MaybeSubGoalB),
            MaybeGoal = error1(SpecsA ++ SpecsB)
        )
    ;
        Functor = ";",
        Args = [SubGoalTermA, SubGoalTermB],
        % Disjunction or if-then-else (Prolog syntax).
        ( if
            SubGoalTermA = term.functor(term.atom("->"),
                [CondGoalTerm, ThenGoalTerm], _Context)
        then
            parse_dcg_if_then_else(CondGoalTerm, ThenGoalTerm, SubGoalTermB,
                Context, ContextPieces, MaybeGoal, !VarSet, !Counter, !DCGVar)
        else
            Var0 = !.DCGVar,
            parse_dcg_goal(SubGoalTermA, ContextPieces, MaybeSubGoalA0,
                !VarSet, !Counter, Var0, VarA),
            parse_dcg_goal(SubGoalTermB, ContextPieces, MaybeSubGoalB0,
                !VarSet, !Counter, Var0, VarB),
            ( if
                MaybeSubGoalA0 = ok1(SubGoalA0),
                MaybeSubGoalB0 = ok1(SubGoalB0)
            then
                ( if VarA = Var0, VarB = Var0 then
                    Var = Var0,
                    Goal = disj_expr(Context, SubGoalA0, SubGoalB0)
                else if VarA = Var0 then
                    Var = VarB,
                    Unify = unify_expr(Context,
                        term.variable(Var, Context),
                        term.variable(VarA, Context),
                        purity_pure),
                    append_to_disjunct(Unify, Context, SubGoalA0, SubGoalA),
                    Goal = disj_expr(Context, SubGoalA, SubGoalB0)
                else if VarB = Var0 then
                    Var = VarA,
                    Unify = unify_expr(Context,
                        term.variable(Var, Context),
                        term.variable(VarB, Context),
                        purity_pure),
                    append_to_disjunct(Unify, Context, SubGoalB0, SubGoalB),
                    Goal = disj_expr(Context, SubGoalA0, SubGoalB)
                else
                    Var = VarB,
                    prog_util.rename_in_goal(VarA, VarB, SubGoalA0, SubGoalA),
                    Goal = disj_expr(Context, SubGoalA, SubGoalB0)
                ),
                !:DCGVar = Var,
                MaybeGoal = ok1(Goal)
            else
                !:DCGVar = VarA,    % Dummy; the value shouldn't matter.
                SpecsA = get_any_errors1(MaybeSubGoalA0),
                SpecsB = get_any_errors1(MaybeSubGoalB0),
                MaybeGoal = error1(SpecsA ++ SpecsB)
            )
        )
    ;
        Functor = "else",
        Args = [CondThenTerm, ElseGoalTerm],
        % If-then-else (NU-Prolog syntax).
        CondThenTerm = term.functor(term.atom("if"),
            [term.functor(term.atom("then"),
                [CondGoalTerm, ThenGoalTerm], _)],
            CondContext),
        parse_dcg_if_then_else(CondGoalTerm, ThenGoalTerm, ElseGoalTerm,
            CondContext, ContextPieces, MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        ( Functor = "not"   % Negation (NU-Prolog syntax).
        ; Functor = "\\+"   % Negation (Prolog syntax).
        ),
        Args = [SubGoalTermA],
        parse_dcg_goal(SubGoalTermA, ContextPieces, MaybeSubGoalA,
            !VarSet, !Counter, !.DCGVar, _),
        (
            MaybeSubGoalA = ok1(SubGoalA),
            Goal = not_expr(Context, SubGoalA),
            MaybeGoal = ok1(Goal)
        ;
            MaybeSubGoalA = error1(Specs),
            MaybeGoal = error1(Specs)
        )
    ;
        % Existential or universal quantification.
        (
            Functor = "some",
            QuantType = quant_some,
            VarsContextPieces = [lower_case_next_if_not_first,
                words("In first argument of"), quote("all"), suffix(":")]
        ;
            Functor = "all",
            QuantType = quant_all,
            VarsContextPieces = [lower_case_next_if_not_first,
                words("In first argument of"), quote("all"), suffix(":")]
        ),
        Args = [QVarsTerm, SubGoalTerm],
        varset.coerce(!.VarSet, GenericVarSet),
        % Note that both versions of VarsContextPieces should be static data;
        % factoring out their common parts would destroy this property.
        UpdatedContextPieces = ContextPieces ++ VarsContextPieces,
        parse_quantifier_vars(QVarsTerm, GenericVarSet, UpdatedContextPieces,
            MaybeStateVarsAndVars),
        parse_dcg_goal(SubGoalTerm, ContextPieces, MaybeSubGoal,
            !VarSet, !Counter, !DCGVar),
        ( if
            MaybeStateVarsAndVars = ok2(Vars0, StateVars0),
            MaybeSubGoal = ok1(SubGoal)
        then
            list.map(term.coerce_var, StateVars0, StateVars),
            list.map(term.coerce_var, Vars0, Vars),
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
            VarsSpecs = get_any_errors2(MaybeStateVarsAndVars),
            SubGoalSpecs = get_any_errors1(MaybeSubGoal),
            MaybeGoal = error1(VarsSpecs ++ SubGoalSpecs)
        )
    ).

:- pred parse_some_vars_dcg_goal(term::in, list(format_component)::in,
    maybe3(list(prog_var), list(prog_var), goal)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

parse_some_vars_dcg_goal(Term, ContextPieces, MaybeVarsGoal,
        !VarSet, !Counter, !DCGVar) :-
    ( if
        Term = term.functor(term.atom("some"), [VarsTerm, SubGoalTerm],
            _Context)
    then
        % XXX We should update ContextPieces.
        varset.coerce(!.VarSet, GenericVarSet),
        parse_quantifier_vars(VarsTerm, GenericVarSet, ContextPieces,
            MaybeVars),
        GoalTerm = SubGoalTerm
    else
        MaybeVars = ok2([], []),
        GoalTerm = Term
    ),
    parse_dcg_goal(GoalTerm, ContextPieces, MaybeGoal, !VarSet, !Counter,
        !DCGVar),
    ( if
        MaybeVars = ok2(Vars0, StateVars0),
        MaybeGoal = ok1(Goal)
    then
        list.map(term.coerce_var, Vars0, Vars),
        list.map(term.coerce_var, StateVars0, StateVars),
        MaybeVarsGoal = ok3(Vars, StateVars, Goal)
    else
        VarsSpecs = get_any_errors2(MaybeVars),
        GoalSpecs = get_any_errors1(MaybeGoal),
        MaybeVarsGoal = error3(VarsSpecs ++ GoalSpecs)
    ).

    % Parse the "if" and the "then" part of an if-then or an if-then-else.
    % If the condition is a DCG goal, but then "then" part is not,
    % then we need to translate
    %   ( if a then
    %       { b }
    %   else
    %       c
    %   )
    % as
    %   ( if a(DCG_1, DCG_2) then
    %       b,
    %       DCG_3 = DCG_2
    %   else
    %       c(DCG_1, DCG_3)
    %   )
    % rather than
    %   ( if a(DCG_1, DCG_2) then
    %       b
    %   else
    %       c(DCG_1, DCG_2)
    %   )
    % so that the implicit quantification of DCG_2 is correct.
    %
:- pred parse_dcg_if_then(term::in, term::in, prog_context::in,
    list(format_component)::in,
    maybe3(list(prog_var), list(prog_var), goal)::out,
    maybe1(goal)::out, prog_varset::in, prog_varset::out,
    counter::in, counter::out, prog_var::in, prog_var::out) is det.

parse_dcg_if_then(CondGoalTerm, ThenGoalTerm, Context, ContextPieces,
        MaybeVarsCond, MaybeThen, !VarSet, !Counter, Var0, Var) :-
    parse_some_vars_dcg_goal(CondGoalTerm, ContextPieces, MaybeVarsCond,
        !VarSet, !Counter, Var0, Var1),
    parse_dcg_goal(ThenGoalTerm, ContextPieces, MaybeThen1,
        !VarSet, !Counter, Var1, Var2),
    ( if
        Var0 \= Var1,
        Var1 = Var2
    then
        (
            MaybeThen1 = ok1(Then1),
            new_dcg_var(!VarSet, !Counter, Var),
            Unify = unify_expr(Context,
                term.variable(Var, Context), term.variable(Var2, Context),
                purity_pure),
            Then = conj_expr(Context, Then1, Unify),
            MaybeThen = ok1(Then)
        ;
            MaybeThen1 = error1(_),
            MaybeThen = MaybeThen1,
            Var = Var2                  % Dummy; the value shouldn't matter.
        )
    else
        MaybeThen = MaybeThen1,
        Var = Var2
    ).

:- pred parse_dcg_if_then_else(term::in, term::in, term::in, prog_context::in,
    list(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

parse_dcg_if_then_else(CondGoalTerm, ThenGoalTerm, ElseGoalTerm,
        Context, ContextPieces, MaybeGoal, !VarSet, !Counter, Var0, Var) :-
    parse_dcg_if_then(CondGoalTerm, ThenGoalTerm, Context, ContextPieces,
        MaybeVarsCond, MaybeThen1, !VarSet, !Counter, Var0, VarThen),
    parse_dcg_goal(ElseGoalTerm, ContextPieces, MaybeElse1,
        !VarSet, !Counter, Var0, VarElse),
    ( if
        MaybeVarsCond = ok3(Vars, StateVars, Cond),
        MaybeThen1 = ok1(Then1),
        MaybeElse1 = ok1(Else1)
    then
        ( if VarThen = Var0, VarElse = Var0 then
            Var = Var0,
            Then = Then1,
            Else = Else1
        else if VarThen = Var0 then
            Var = VarElse,
            Unify = unify_expr(Context,
                term.variable(Var, Context), term.variable(VarThen, Context),
                purity_pure),
            Then = conj_expr(Context, Then1, Unify),
            Else = Else1
        else if VarElse = Var0 then
            Var = VarThen,
            Then = Then1,
            Unify = unify_expr(Context,
                term.variable(Var, Context), term.variable(VarElse, Context),
                purity_pure),
            Else = conj_expr(Context, Else1, Unify)
        else
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
        Goal = if_then_else_expr(Context, Vars, StateVars, Cond, Then, Else),
        MaybeGoal = ok1(Goal)
    else
        CondSpecs = get_any_errors3(MaybeVarsCond),
        ThenSpecs = get_any_errors1(MaybeThen1),
        ElseSpecs = get_any_errors1(MaybeElse1),
        MaybeGoal = error1(CondSpecs ++ ThenSpecs ++ ElseSpecs),
        Var = Var0              % Dummy; the value shouldn't matter.
    ).

%-----------------------------------------------------------------------------%
%
% Utility predicates.
%
%-----------------------------------------------------------------------------%

    % Used to allocate fresh variables needed for the DCG expansion.
    %
:- pred new_dcg_var(prog_varset::in, prog_varset::out,
    counter::in, counter::out, prog_var::out) is det.

new_dcg_var(!VarSet, !Counter, DCGVar) :-
    counter.allocate(N, !Counter),
    string.int_to_string(N, StringN),
    string.append("DCG_", StringN, VarName),
    varset.new_var(DCGVar, !VarSet),
    varset.name_var(DCGVar, VarName, !VarSet).

:- pred append_to_disjunct(goal::in, prog_context::in, goal::in, goal::out)
    is det.

append_to_disjunct(AddedGoal, Context, Disjunct0, Disjunct) :-
    ( if
        Disjunct0 = disj_expr(Disjunct0Context, SubDisjunctA0, SubDisjunctB0)
    then
        append_to_disjunct(AddedGoal, Context, SubDisjunctA0, SubDisjunctA),
        append_to_disjunct(AddedGoal, Context, SubDisjunctB0, SubDisjunctB),
        Disjunct = disj_expr(Disjunct0Context, SubDisjunctA, SubDisjunctB)
    else
        Disjunct = conj_expr(Context, Disjunct0, AddedGoal)
    ).

    % term_list_append_term(ListTerm, Term, Result):
    %
    % If ListTerm is a term representing a proper list, this predicate
    % will append the term Term onto the end of the list.
    %
:- pred term_list_append_term(term(T)::in, term(T)::in, term(T)::out)
    is semidet.

term_list_append_term(List0, Term, List) :-
    ( if List0 = term.functor(term.atom("[]"), [], _Context) then
        List = Term
    else
        List0 = term.functor(term.atom("[|]"), [Head, Tail0], Context),
        term_list_append_term(Tail0, Term, Tail),
        List = term.functor(term.atom("[|]"), [Head, Tail], Context)
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_io_dcg.
%-----------------------------------------------------------------------------%
