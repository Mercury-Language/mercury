%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2001, 2003-2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_dcg_goal.m.
% Main authors: fjh, zs.
%
% This module handles the parsing of clauses in Definite Clause Grammar
% notation.
%
% XXX This module performs no error checking.
%
% XXX It may be an idea to recode this as a state variable transformation:
% roughly     Head --> G1, G2, {G3}, G4.
% becomes     Head(!DCG) :- G1(!DCG), G2(!DCG), G3, G4(!DCG).
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_dcg_goal.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module cord.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- pred parse_dcg_clause(module_name::in, varset::in, term::in, term::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

    % parse_dcg_pred_goal(GoalTerm, MaybeGoal, DCGVar0, DCGVar, !VarSet):
    %
    % Parses `GoalTerm' and expands it as a DCG goal.
    % `DCGVar0' is the initial DCG variable, and `DCGVar' is the final one.
    %
:- pred parse_dcg_pred_goal(term::in, cord(format_component)::in,
    maybe1(goal)::out, prog_var::out, prog_var::out,
    prog_varset::in, prog_varset::out) is det.

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.parse_goal.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.parse_vars.
:- import_module parse_tree.prog_util.

:- import_module counter.
:- import_module list.
:- import_module one_or_more.
:- import_module string.

%---------------------------------------------------------------------------%

parse_dcg_clause(ModuleName, VarSet0, DCG_Head, DCG_Body, Context, SeqNum,
        MaybeIOM) :-
    varset.coerce(VarSet0, ProgVarSet0),
    new_dcg_var(ProgVarSet0, ProgVarSet1, counter.init(0), Counter0,
        DCGVar0),
    BodyContextPieces = cord.init,
    parse_dcg_goal(DCG_Body, BodyContextPieces, MaybeBodyGoal,
        ProgVarSet1, ProgVarSet, Counter0, _Counter, DCGVar0, DCGVar),

    HeadContextPieces = cord.singleton(words("In DCG clause head:")),
    parse_implicitly_qualified_sym_name_and_args(ModuleName, DCG_Head,
        VarSet0, HeadContextPieces, MaybeFunctor),
    (
        MaybeFunctor = ok2(Name, Args0),
        list.map(term.coerce, Args0, Args1),
        Args = Args1 ++
            [term.variable(DCGVar0, Context),
            term.variable(DCGVar, Context)],
        ItemClause = item_clause_info(Name, pf_predicate, Args,
            item_origin_user, ProgVarSet, MaybeBodyGoal, Context, SeqNum),
        Item = item_clause(ItemClause),
        MaybeIOM = ok1(iom_item(Item))
    ;
        MaybeFunctor = error2(FunctorSpecs),
        Specs = FunctorSpecs ++ get_any_errors1(MaybeBodyGoal),
        MaybeIOM = error1(Specs)
    ).

%---------------------------------------------------------------------------%

parse_dcg_pred_goal(GoalTerm, ContextPieces, MaybeGoal,
        DCGVar0, DCGVar, !VarSet) :-
    new_dcg_var(!VarSet, counter.init(0), Counter0, DCGVar0),
    parse_dcg_goal(GoalTerm, ContextPieces, MaybeGoal, !VarSet,
        Counter0, _Counter, DCGVar0, DCGVar).

%---------------------------------------------------------------------------%

    % Expand a DCG goal.
    %
:- pred parse_dcg_goal(term::in, cord(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

parse_dcg_goal(Term, ContextPieces, MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    Context = get_term_context(Term),
    term.coerce(Term, ProgTerm),
    ( if
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
            % It's the ordinary case of nonterminal. Turn the term into a call,
            % and add the current and next DCG variables to the end of the
            % argument list.
            make_dcg_call(SymName, Args0, Context, Goal,
                !VarSet, !Counter, !DCGVar),
            MaybeGoal = ok1(Goal)
        )
    else
        % A call to a free variable, or to a number or string.
        % Just translate it into a call to call/3 - the typechecker will catch
        % calls to numbers and strings.
        SymName = unqualified("call"),
        Args = [ProgTerm],
        make_dcg_call(SymName, Args, Context, Goal,
            !VarSet, !Counter, !DCGVar),
        MaybeGoal = ok1(Goal)
    ).

:- pred make_dcg_call(sym_name::in, list(prog_term)::in, prog_context::in,
    goal::out, prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

make_dcg_call(SymName, Args0, Context, Goal, !VarSet, !Counter, !DCGVar) :-
    InVar = !.DCGVar,
    new_dcg_var(!VarSet, !Counter, OutVar),
    !:DCGVar = OutVar,
    Args = Args0 ++
        [term.variable(InVar, Context),
        term.variable(OutVar, Context)],
    Goal = call_expr(Context, SymName, Args, purity_pure).

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
    cord(format_component)::in, maybe1(goal)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is semidet.

parse_non_call_dcg_goal(Functor, Args, Context, ContextPieces, MaybeGoal,
        !VarSet, !Counter, !DCGVar) :-
    % We parse goals in DCG contexts here, while we parse goals in non-DCG
    % contexts in parse_non_call_goal in parse_goal.m.
    %
    % Since many kinds of goals can occur in both kinds of contexts,
    % the code handling those kinds of goals should be kept as identical
    % as possible in the two places. For ease of maintenance, the switch arms
    % handling the common goal types should also be kept in the same order.
    % The top of parse_non_call_goal contains a comment the order in which
    % the various goal types are handled by both predicates. If you add
    % code to handle a new goal type here, please update that comment.
    %
    % That comment also records why we don't want to factor out much of the
    % common code between these two predicates.

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
            parse_dcg_goal(SubGoalTerm, ContextPieces, MaybeGoal0,
                !VarSet, !Counter, !DCGVar),
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
            PromisedPurity = purity_pure
        ;
            Functor = "promise_semipure",
            PromisedPurity = purity_semipure
        ;
            Functor = "promise_impure",
            PromisedPurity = purity_impure
        ),
        ( if Args = [SubGoalTerm] then
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
        else
            Spec = should_have_one_goal_prefix(ContextPieces, Context,
                Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        ( Functor = "not"   % Negation (NU-Prolog syntax).
        ; Functor = "\\+"   % Negation (Prolog syntax).
        ),
        ( if Args = [SubGoalTerm] then
            parse_dcg_goal(SubGoalTerm, ContextPieces, MaybeSubGoal,
                !VarSet, !Counter, !.DCGVar, _),
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
        % Existential or universal quantification.
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
            parse_vars_state_vars(QVarsTerm, GenericVarSet,
                VarsContextPieces, MaybeVars),
            parse_dcg_goal(SubGoalTerm, ContextPieces, MaybeSubGoal,
                !VarSet, !Counter, !DCGVar),
            ( if
                MaybeVars = ok1(plain_state_vars(Vars0, StateVars0)),
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
            % Disjunction or if-then-else (Prolog syntax).
            ( if
                SubGoalTermA = term.functor(term.atom("->"),
                    [CondGoalTerm, ThenGoalTerm], _Context)
            then
                parse_dcg_if_then_else(CondGoalTerm, ThenGoalTerm,
                    SubGoalTermB, Context, ContextPieces, MaybeGoal,
                    !VarSet, !Counter, !DCGVar)
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
                        append_to_disjunct(Unify, Context,
                            SubGoalA0, SubGoalA),
                        Goal = disj_expr(Context, SubGoalA, SubGoalB0)
                    else if VarB = Var0 then
                        Var = VarA,
                        Unify = unify_expr(Context,
                            term.variable(Var, Context),
                            term.variable(VarB, Context),
                            purity_pure),
                        append_to_disjunct(Unify, Context,
                            SubGoalB0, SubGoalB),
                        Goal = disj_expr(Context, SubGoalA0, SubGoalB)
                    else
                        Var = VarB,
                        prog_util.rename_in_goal(VarA, VarB,
                            SubGoalA0, SubGoalA),
                        Goal = disj_expr(Context, SubGoalA, SubGoalB0)
                    ),
                    !:DCGVar = Var,
                    MaybeGoal = ok1(Goal)
                else
                    !:DCGVar = VarA,    % Dummy; the value shouldn't matter.
                    Specs = get_any_errors1(MaybeSubGoalA0) ++
                        get_any_errors1(MaybeSubGoalB0),
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
            % We do the same for ";" in parse_non_call_goal.
            Spec = should_have_two_goals_infix(ContextPieces, Context,
                Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "else",
        % If-then-else (NU-Prolog syntax).
        ( if
            Args = [CondThenTerm, ElseGoalTerm],
            CondThenTerm = term.functor(term.atom("if"),
                [term.functor(term.atom("then"),
                    [CondGoalTerm, ThenGoalTerm], _)],
                CondContext)
        then
            parse_dcg_if_then_else(CondGoalTerm, ThenGoalTerm, ElseGoalTerm,
                CondContext, ContextPieces, MaybeGoal,
                !VarSet, !Counter, !DCGVar)
        else
            Pieces = [words("Error: the "), quote("else"), words("operator"),
                words("should occur in expressions of the form"),
                quote("( if goal then goal else goal )"), suffix("."), nl],
            Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                Context, Pieces),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "if",
        % If-then (NU-Prolog syntax).
        ( if
            Args = [term.functor(term.atom("then"),
                [CondGoalTerm, ThenGoalTerm], _)]
        then
            InVar = !.DCGVar,
            parse_dcg_if_then(CondGoalTerm, ThenGoalTerm,
                Context, ContextPieces, MaybeVarsCondGoal, MaybeThenGoal,
                !VarSet, !Counter, !DCGVar),
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
                Specs = get_any_errors3(MaybeVarsCondGoal) ++
                    get_any_errors1(MaybeThenGoal),
                MaybeGoal = error1(Specs)
            )
        else
            Pieces = [words("Error: the "), quote("else"), words("operator"),
                words("should occur in expressions of the form"),
                quote("( if goal then goal else goal )"), suffix("."), nl],
            Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                Context, Pieces),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "{}",
        (
            Args = [],
            Pieces = [words("Error: there should be at least one goal"),
                words("between the curly braces."), nl],
            Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                Context, Pieces),
            MaybeGoal = error1([Spec])
        ;
            Args = [HeadGoal | TailGoals],
            % Ordinary goal inside { curly braces }.
            % The parser treats '{}/N' terms as tuples, so we need to undo
            % the parsing of the argument conjunction here.
            one_or_more_to_conjunction(Context,
                one_or_more(HeadGoal, TailGoals), SubGoal),
            parse_goal(SubGoal, ContextPieces, MaybeGoal, !VarSet)
        )
    ;
        Functor = "[]",
        (
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
            Args = [_ | _],
            Pieces = [words("Error: in DCG clauses,"),
                words("the"), quote("[]"), words("operator"),
                words("may only be used to match the input"),
                words("against a list of zero items,"),
                words("and must therefore be used with arity 0."), nl],
            Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                Context, Pieces),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "[|]",
        ( if Args = [X, Xs] then
            % Translate
            %   [A, B, C]
            % to
            %   DCGIn = [A, B, C | DCGOut].
            InVar = !.DCGVar,
            new_dcg_var(!VarSet, !Counter, OutVar),
            !:DCGVar = OutVar,
            ConsTerm0 = term.functor(term.atom("[|]"), [X, Xs], Context),
            term.coerce(ConsTerm0, ConsTerm),
            OutVarTerm = term.variable(OutVar, Context),
            ( if term_list_append_term(ConsTerm, OutVarTerm, Term) then
                Goal = unify_expr(Context, variable(InVar, Context), Term,
                    purity_pure),
                MaybeGoal = ok1(Goal)
            else
                Pieces = [words("Error: there is no"),
                    quote("[]"), words("at the end of the list."), nl],
                Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                    Context, Pieces),
                MaybeGoal = error1([Spec])
            )
        else
            Pieces = [words("Error: in DCG clauses,"),
                words("the"), quote("[|]"), words("operator"),
                words("may only be used to match the input"),
                words("against a list of one or more items,"),
                words("and must therefore be used with arity 2."), nl],
            Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                Context, Pieces),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = "=",
        ( if Args = [Arg0] then
            % Call to '='/1 - unify argument with DCG input arg.
            term.coerce(Arg0, Arg),
            DCGVarTerm = variable(!.DCGVar, Context),
            Goal = unify_expr(Context, Arg, DCGVarTerm, purity_pure),
            MaybeGoal = ok1(Goal)
        else
            Spec = should_have_two_terms_infix(ContextPieces, Context,
                Functor),
            MaybeGoal = error1([Spec])
        )
    ;
        Functor = ":=",
        ( if Args = [Arg0] then
            % ":=(Term)" is a DCG output unification, which unifies Term
            % with the implicit DCG output argument, while ignoring
            % the input DCG argument.
            new_dcg_var(!VarSet, !Counter, OutVar),
            !:DCGVar = OutVar,
            term.coerce(Arg0, Arg),
            OutVarTerm = variable(OutVar, Context),
            Goal = unify_expr(Context, Arg, OutVarTerm, purity_pure),
            MaybeGoal = ok1(Goal)
        else
            % "^ field_list := Term" is a DCG field update, which replaces
            % the named field in the implicit DCG argument.
            %
            % Such field updates are represented as calls in the parse tree.
            % The field update is implemented by goal_expr_to_goal.m when
            % we convert the parse tree to the HLDS. That code will also
            % generate the error message if Args has the wrong shape.
            SymName = unqualified(":="),
            list.map(term.coerce, Args, ProgArgs),
            make_dcg_call(SymName, ProgArgs, Context, Goal,
                !VarSet, !Counter, !DCGVar),
            MaybeGoal = ok1(Goal)
        )
    ).

:- pred parse_some_vars_dcg_goal(term::in, cord(format_component)::in,
    maybe3(list(prog_var), list(prog_var), goal)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

parse_some_vars_dcg_goal(Term, ContextPieces, MaybeVarsGoal,
        !VarSet, !Counter, !DCGVar) :-
    % We parse existentially quantified goals in DCG contexts here,
    % while we parse them in non-DCG contexts in parse_some_vars_goal
    % in parse_goal.m.
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
    parse_dcg_goal(GoalTerm, ContextPieces, MaybeGoal, !VarSet, !Counter,
        !DCGVar),
    ( if
        MaybeVars = ok1(plain_state_vars(Vars0, StateVars0)),
        MaybeGoal = ok1(Goal)
    then
        list.map(term.coerce_var, Vars0, Vars),
        list.map(term.coerce_var, StateVars0, StateVars),
        MaybeVarsGoal = ok3(Vars, StateVars, Goal)
    else
        Specs = get_any_errors1(MaybeVars) ++ get_any_errors1(MaybeGoal),
        MaybeVarsGoal = error3(Specs)
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
    cord(format_component)::in,
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
    cord(format_component)::in, maybe1(goal)::out,
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
        Specs = get_any_errors3(MaybeVarsCond) ++
            get_any_errors1(MaybeThen1) ++
            get_any_errors1(MaybeElse1),
        MaybeGoal = error1(Specs),
        Var = Var0              % Dummy; the value shouldn't matter.
    ).

%---------------------------------------------------------------------------%
%
% Utility predicates.
%
%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_dcg_goal.
%---------------------------------------------------------------------------%
