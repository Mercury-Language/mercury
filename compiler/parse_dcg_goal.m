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
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module cord.
:- import_module list.
:- import_module maybe.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- pred parse_dcg_clause(maybe(module_name)::in, varset::in,
    term::in, term::in, prog_context::in, item_seq_num::in,
    maybe1(item_clause_info)::out) is det.

    % parse_dcg_pred_goal(GoalTerm, MaybeGoal, DCGVar0, DCGVar, !VarSet):
    %
    % Parses `GoalTerm' and expands it as a DCG goal.
    % `DCGVar0' is the initial DCG variable, and `DCGVar' is the final one.
    %
:- pred parse_dcg_pred_goal(term::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out, prog_var::out, prog_var::out,
    prog_varset::in, prog_varset::out) is det.

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.parse_goal.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.parse_vars.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module counter.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

parse_dcg_clause(MaybeModuleName, VarSet0, DCG_HeadTerm, DCG_BodyTerm,
        Context, SeqNum, MaybeClause) :-
    varset.coerce(VarSet0, ProgVarSet0),
    new_dcg_var(ProgVarSet0, ProgVarSet1, counter.init(0), Counter0,
        DCGVar0),
    BodyContextPieces = cord.init,
    parse_dcg_goal(DCG_BodyTerm, BodyContextPieces, MaybeBodyGoal,
        ProgVarSet1, ProgVarSet, Counter0, _Counter, DCGVar0, DCGVar),

    HeadContextPieces = cord.singleton(words("In DCG clause head:")),
    (
        MaybeModuleName = no,
        parse_sym_name_and_args(VarSet0,
            HeadContextPieces, DCG_HeadTerm, MaybeFunctor)
    ;
        MaybeModuleName = yes(ModuleName),
        parse_implicitly_qualified_sym_name_and_args(ModuleName, VarSet0,
            HeadContextPieces, DCG_HeadTerm, MaybeFunctor)
    ),
    (
        MaybeFunctor = ok2(SymName, ArgTerms0),
        list.map(term.coerce, ArgTerms0, ArgTerms1),
        ArgTerms = ArgTerms1 ++
            [term.variable(DCGVar0, Context),
            term.variable(DCGVar, Context)],
        ItemClause = item_clause_info(pf_predicate, SymName, ArgTerms,
            ProgVarSet, MaybeBodyGoal, Context, SeqNum),
        MaybeClause = ok1(ItemClause)
    ;
        MaybeFunctor = error2(FunctorSpecs),
        Specs = FunctorSpecs ++ get_any_errors_warnings2(MaybeBodyGoal),
        MaybeClause = error1(Specs)
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
:- pred parse_dcg_goal(term::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

parse_dcg_goal(Term, ContextPieces, MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    ( if
         % Check for builtins...
        Term = term.functor(term.atom(Functor), ArgTerms, Context),
        string_dcg_goal_kind(Functor, GoalKind)
    then
        parse_non_call_dcg_goal(GoalKind, ArgTerms, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    else
        % It is not a builtin.
        Context = get_term_context(Term),
        term.coerce(Term, ProgTerm),
        ( if try_parse_sym_name_and_args(ProgTerm, SymName, ArgTerms0) then
            % It is the ordinary case of a nonterminal. Turn the term
            % into a call, and add the current and next DCG variables
            % to the end of the argument list.
            make_dcg_call(SymName, ArgTerms0, Context, Goal,
                !VarSet, !Counter, !DCGVar),
            MaybeGoal = ok2(Goal, [])
        else
            % A call to a free variable, or to a number or string.
            % Just translate it into a call to call/3 - the typechecker
            % will catch calls to numbers and strings.
            SymName = unqualified("call"),
            make_dcg_call(SymName, [ProgTerm], Context, Goal,
                !VarSet, !Counter, !DCGVar),
            MaybeGoal = ok2(Goal, [])
        )
    ).

:- pred make_dcg_call(sym_name::in, list(prog_term)::in, prog_context::in,
    goal::out, prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

make_dcg_call(SymName, ArgTerms0, Context, Goal, !VarSet, !Counter, !DCGVar) :-
    InVar = !.DCGVar,
    new_dcg_var(!VarSet, !Counter, OutVar),
    !:DCGVar = OutVar,
    ArgTerms = ArgTerms0 ++
        [term.variable(InVar, Context),
        term.variable(OutVar, Context)],
    Goal = call_expr(Context, SymName, ArgTerms, purity_pure).

%---------------------------------------------------------------------------%

:- type dcg_goal_kind
    --->    dgk_impure
    ;       dgk_semipure
    ;       dgk_promise_impure
    ;       dgk_promise_semipure
    ;       dgk_promise_pure
    ;       dgk_not
    ;       dgk_not_prolog
    ;       dgk_some
    ;       dgk_all
    ;       dgk_conj
    ;       dgk_par_conj
    ;       dgk_semicolon
    ;       dgk_else
    ;       dgk_if
    ;       dgk_braces
    ;       dgk_nil
    ;       dgk_cons
    ;       dgk_equal
    ;       dgk_colon_equal.

:- inst dcg_goal_kind_purity for dcg_goal_kind/0
    --->    dgk_impure
    ;       dgk_semipure.

:- inst dcg_goal_kind_promise_purity for dcg_goal_kind/0
    --->    dgk_promise_impure
    ;       dgk_promise_semipure
    ;       dgk_promise_pure.

:- inst dcg_goal_kind_not for dcg_goal_kind/0
    --->    dgk_not
    ;       dgk_not_prolog.

:- inst dcg_goal_kind_some_all for dcg_goal_kind/0
    --->    dgk_some
    ;       dgk_all.

:- inst dcg_goal_kind_conj for dcg_goal_kind/0
    --->    dgk_conj.

:- inst dcg_goal_kind_par_conj for dcg_goal_kind/0
    --->    dgk_par_conj.

:- pred string_dcg_goal_kind(string, dcg_goal_kind).
:- mode string_dcg_goal_kind(in, out) is semidet.
:- mode string_dcg_goal_kind(out, in) is det.

string_dcg_goal_kind(Functor, GoalKind) :-
    ( Functor = "impure",           GoalKind = dgk_impure
    ; Functor = "semipure",         GoalKind = dgk_semipure
    ; Functor = "promise_impure"  , GoalKind = dgk_promise_impure
    ; Functor = "promise_semipure", GoalKind = dgk_promise_semipure
    ; Functor = "promise_pure",     GoalKind = dgk_promise_pure
    ; Functor = "not",              GoalKind = dgk_not
    ; Functor = "\\+",              GoalKind = dgk_not_prolog
    ; Functor = "some",             GoalKind = dgk_some
    ; Functor = "all",              GoalKind = dgk_all
    ; Functor = ",",                GoalKind = dgk_conj
    ; Functor = "&",                GoalKind = dgk_par_conj
    ; Functor = ";",                GoalKind = dgk_semicolon
    ; Functor = "else",             GoalKind = dgk_else
    ; Functor = "if",               GoalKind = dgk_if
    ; Functor = "{}",               GoalKind = dgk_braces
    ; Functor = "[]",               GoalKind = dgk_nil
    ; Functor = "[|]",              GoalKind = dgk_cons
    ; Functor = "=",                GoalKind = dgk_equal
    ; Functor = ":=",               GoalKind = dgk_colon_equal
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
:- pred parse_non_call_dcg_goal(dcg_goal_kind::in, list(term)::in,
    prog_context::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

parse_non_call_dcg_goal(GoalKind, Args, Context, ContextPieces, MaybeGoal,
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
    %
    % XXX We should update ContextPieces at every call to parse a goal
    % component that is not itself a goal.
    (
        ( GoalKind = dgk_impure
        ; GoalKind = dgk_semipure
        ),
        parse_dcg_goal_impure_semipure(GoalKind, Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        ( GoalKind = dgk_promise_pure
        ; GoalKind = dgk_promise_semipure
        ; GoalKind = dgk_promise_impure
        ),
        parse_dcg_goal_promise_purity(GoalKind, Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        ( GoalKind = dgk_not
        ; GoalKind = dgk_not_prolog
        ),
        parse_dcg_goal_not(GoalKind, Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        ( GoalKind = dgk_some
        ; GoalKind = dgk_all
        ),
        % Existential or universal quantification.
        parse_dcg_goal_some_all(GoalKind, Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        GoalKind = dgk_conj,
        parse_dcg_goal_conj(GoalKind, Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        GoalKind = dgk_par_conj,
        parse_dcg_goal_conj(GoalKind, Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        GoalKind = dgk_semicolon,
        parse_dcg_goal_semicolon(Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        GoalKind = dgk_else,
        % If-then-else (NU-Prolog syntax).
        parse_dcg_goal_else(Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        GoalKind = dgk_if,
        % If-then (NU-Prolog syntax).
        parse_dcg_goal_if(Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        GoalKind = dgk_braces,
        parse_dcg_goal_braces(Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        GoalKind = dgk_nil,
        parse_dcg_goal_nil(Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        GoalKind = dgk_cons,
        parse_dcg_goal_cons(Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        GoalKind = dgk_equal,
        parse_dcg_goal_equal(Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ;
        GoalKind = dgk_colon_equal,
        parse_dcg_goal_colon_equal(Args, Context, ContextPieces,
            MaybeGoal, !VarSet, !Counter, !DCGVar)
    ).

%---------------------%

:- pred parse_dcg_goal_impure_semipure(dcg_goal_kind::in(dcg_goal_kind_purity),
    list(term)::in, prog_context::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.
:- pragma inline(pred(parse_dcg_goal_impure_semipure/11)).

parse_dcg_goal_impure_semipure(GoalKind, ArgTerms, Context, ContextPieces,
        MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    ( GoalKind = dgk_impure,   Purity = purity_impure
    ; GoalKind = dgk_semipure, Purity = purity_semipure
    ),
    ( if ArgTerms = [SubGoalTerm] then
        parse_dcg_goal(SubGoalTerm, ContextPieces, MaybeGoal0,
            !VarSet, !Counter, !DCGVar),
        apply_purity_marker_to_maybe_goal(SubGoalTerm, Purity,
            MaybeGoal0, MaybeGoal)
    else
        string_dcg_goal_kind(Functor, GoalKind),
        Spec = should_have_one_goal_prefix(ContextPieces, Context, Functor),
        MaybeGoal = error2([Spec])
    ).

%---------------------%

:- pred parse_dcg_goal_promise_purity(
    dcg_goal_kind::in(dcg_goal_kind_promise_purity),
    list(term)::in, prog_context::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.
:- pragma inline(pred(parse_dcg_goal_promise_purity/11)).

parse_dcg_goal_promise_purity(GoalKind, ArgTerms, Context, ContextPieces,
        MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    ( GoalKind = dgk_promise_pure,     PromisedPurity = purity_pure
    ; GoalKind = dgk_promise_semipure, PromisedPurity = purity_semipure
    ; GoalKind = dgk_promise_impure,   PromisedPurity = purity_impure
    ),
    ( if ArgTerms = [SubGoalTerm] then
        parse_dcg_goal(SubGoalTerm, ContextPieces, MaybeGoal0,
            !VarSet, !Counter, !DCGVar),
        (
            MaybeGoal0 = ok2(Goal0, GoalWarningSpecs),
            Goal = promise_purity_expr(Context, PromisedPurity, Goal0),
            MaybeGoal = ok2(Goal, GoalWarningSpecs)
        ;
            MaybeGoal0 = error2(Specs),
            MaybeGoal = error2(Specs)
        )
    else
        string_dcg_goal_kind(Functor, GoalKind),
        Spec = should_have_one_goal_prefix(ContextPieces, Context, Functor),
        MaybeGoal = error2([Spec])
    ).

%---------------------%

:- pred parse_dcg_goal_not(dcg_goal_kind::in(dcg_goal_kind_not),
    list(term)::in, prog_context::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.
:- pragma inline(pred(parse_dcg_goal_not/11)).

parse_dcg_goal_not(GoalKind, ArgTerms, Context, ContextPieces,
        MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    ( if ArgTerms = [SubGoalTerm] then
        parse_dcg_goal(SubGoalTerm, ContextPieces, MaybeSubGoal,
            !VarSet, !Counter, !.DCGVar, _),
        (
            MaybeSubGoal = ok2(SubGoal, GoalWarningSpecs),
            Goal = not_expr(Context, SubGoal),
            MaybeGoal = ok2(Goal, GoalWarningSpecs)
        ;
            MaybeSubGoal = error2(_),
            MaybeGoal = MaybeSubGoal
        )
    else
        string_dcg_goal_kind(Functor, GoalKind),
        Spec = should_have_one_goal_prefix(ContextPieces, Context, Functor),
        MaybeGoal = error2([Spec])
    ).

%---------------------%

:- inst some_or_all for string/0
    --->    "some"
    ;       "all".

:- pred parse_dcg_goal_some_all(dcg_goal_kind::in(dcg_goal_kind_some_all),
    list(term)::in,
    prog_context::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.
:- pragma inline(pred(parse_dcg_goal_some_all/11)).

parse_dcg_goal_some_all(GoalKind, ArgTerms, Context, ContextPieces,
        MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    (
        GoalKind = dgk_some,
        QuantType = quant_some,
        VarsTailPieces = [lower_case_next_if_not_first,
            words("In first argument of"), quote("some"), suffix(":")]
    ;
        GoalKind = dgk_all,
        QuantType = quant_all,
        VarsTailPieces = [lower_case_next_if_not_first,
            words("In first argument of"), quote("all"), suffix(":")]
    ),
    ( if ArgTerms = [QVarsTerm, SubGoalTerm] then
        varset.coerce(!.VarSet, GenericVarSet),
        VarsContextPieces = ContextPieces ++
            cord.from_list(VarsTailPieces),
        parse_vars_state_vars(QVarsTerm, GenericVarSet,
            VarsContextPieces, MaybeVars),
        parse_dcg_goal(SubGoalTerm, ContextPieces, MaybeSubGoal,
            !VarSet, !Counter, !DCGVar),
        ( if
            MaybeVars = ok1(plain_state_vars(Vars0, StateVars0)),
            MaybeSubGoal = ok2(SubGoal, SubGoalWarningSpecs)
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
            MaybeGoal = ok2(Goal, SubGoalWarningSpecs)
        else
            Specs = get_any_errors1(MaybeVars) ++
                get_any_errors_warnings2(MaybeSubGoal),
            MaybeGoal = error2(Specs)
        )
    else
        string_dcg_goal_kind(Functor, GoalKind),
        Spec = should_have_one_x_one_goal_prefix(ContextPieces, Context,
            "a list of variables", Functor),
        MaybeGoal = error2([Spec])
    ).

%---------------------%

    % Although we do almost exactly the same thing for "&" as for ",",
    % we handle them in separate modes, because "," is FAR more common
    % than "&", and keeping its processing efficient is important enough
    % to warrant a small amount of code target language code duplication.
    %
:- pred parse_dcg_goal_conj(dcg_goal_kind, list(term),
    prog_context, cord(format_piece), maybe2(goal, list(warning_spec)),
    prog_varset, prog_varset, counter, counter, prog_var, prog_var).
:- mode parse_dcg_goal_conj(in(dcg_goal_kind_conj), in, in, in, out,
    in, out, in, out, in, out) is det.
:- mode parse_dcg_goal_conj(in(dcg_goal_kind_par_conj), in, in, in, out,
    in, out, in, out, in, out) is det.
:- pragma inline(pred(parse_dcg_goal_conj/11)).

parse_dcg_goal_conj(GoalKind, ArgTerms, Context, ContextPieces,
        MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    string_dcg_goal_kind(Functor, GoalKind),
    ( if ArgTerms = [TermA, TermB] then
        parse_dcg_goal_conjunction(Functor, TermA, TermB, Context,
            ContextPieces, cord.init, ConjunctsCord, [], Warnings, [], Errors,
            !VarSet, !Counter, !DCGVar),
        (
            Errors = [],
            Conjuncts = cord.list(ConjunctsCord),
            (
                Conjuncts = [],
                unexpected($pred, "no Conjuncts")
            ;
                Conjuncts = [ConjunctA | ConjunctsB]
            ),
            (
                GoalKind = dgk_conj,
                Goal = conj_expr(Context, ConjunctA, ConjunctsB)
            ;
                GoalKind = dgk_par_conj,
                Goal = par_conj_expr(Context, ConjunctA, ConjunctsB)
            ),
            MaybeGoal = ok2(Goal, Warnings)
        ;
            Errors = [_ | _],
            MaybeGoal = error2(Errors)
        )
    else
        Spec = should_have_two_goals_infix(ContextPieces, Context, Functor),
        MaybeGoal = error2([Spec])
    ).

:- pred parse_dcg_goal_conjunction(string::in, term::in, term::in,
    prog_context::in, cord(format_piece)::in,
    cord(goal)::in, cord(goal)::out,
    list(warning_spec)::in, list(warning_spec)::out,
    list(error_spec)::in, list(error_spec)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

parse_dcg_goal_conjunction(Functor, TermA, TermB, Context,
        ContextPieces, !ConjunctsCord, !Warnings, !Errors,
        !VarSet, !Counter, !DCGVar) :-
    parse_dcg_goal(TermA, ContextPieces, MaybeGoalA,
        !VarSet, !Counter, !DCGVar),
    (
        MaybeGoalA = ok2(GoalA, WarningsA),
        cord.snoc(GoalA, !ConjunctsCord),
        !:Warnings = WarningsA ++ !.Warnings
    ;
        MaybeGoalA = error2(SpecsA),
        !:Errors = SpecsA ++ !.Errors
    ),
    ( if
        TermB = term.functor(term.atom(Functor), ArgTermsB, Context),
        ArgTermsB = [TermBA, TermBB]
    then
        parse_dcg_goal_conjunction(Functor, TermBA, TermBB, Context,
            ContextPieces, !ConjunctsCord, !Warnings, !Errors,
            !VarSet, !Counter, !DCGVar)
    else
        parse_dcg_goal(TermB, ContextPieces, MaybeGoalB,
            !VarSet, !Counter, !DCGVar),
        (
            MaybeGoalB = ok2(GoalB, WarningsB),
            cord.snoc(GoalB, !ConjunctsCord),
            !:Warnings = WarningsB ++ !.Warnings
        ;
            MaybeGoalB = error2(SpecsB),
            !:Errors = SpecsB ++ !.Errors
        )
    ).

%---------------------%

:- pred parse_dcg_goal_semicolon(list(term)::in,
    prog_context::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.
:- pragma inline(pred(parse_dcg_goal_semicolon/10)).

parse_dcg_goal_semicolon(ArgTerms, Context, ContextPieces,
        MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    ( if ArgTerms = [SubGoalTermA, SubGoalTermB] then
        % Disjunction or if-then-else (Prolog syntax).
        ( if
            SubGoalTermA = term.functor(term.atom("->"),
                [CondGoalTerm, ThenGoalTerm], _Context)
        then
            parse_dcg_if_then_else(CondGoalTerm, ThenGoalTerm,
                SubGoalTermB, Context, ContextPieces, MaybeGoal,
                !VarSet, !Counter, !DCGVar)
        else
            InitDCGVar = !.DCGVar,
            parse_dcg_goal_disjunction(InitDCGVar, ContextPieces,
                SubGoalTermA, SubGoalTermB, no, MaybeFirstDiffDCGVar,
                cord.init, DisjunctsDCGVarsCord, [], Warnings, [], ErrorSpecs,
                !VarSet, !Counter),
            (
                ErrorSpecs = [],
                DisjunctsDCGVars = cord.list(DisjunctsDCGVarsCord),
                (
                    MaybeFirstDiffDCGVar = no,
                    % !DCGVar is unchanged in all disjuncts, so it should stay
                    % as InitDCGVar after the disjunction.
                    assoc_list.keys(DisjunctsDCGVars, Disjuncts)
                ;
                    MaybeFirstDiffDCGVar = yes(FirstDiffDCGVar),
                    !:DCGVar = FirstDiffDCGVar,
                    bring_disjuncts_up_to(InitDCGVar, FirstDiffDCGVar, Context,
                        DisjunctsDCGVars, [], RevDisjuncts),
                    list.reverse(RevDisjuncts, Disjuncts)
                ),
                (
                    ( Disjuncts = []
                    ; Disjuncts = [_]
                    ),
                    unexpected($pred, "less than two disjuncts")
                ;
                    Disjuncts = [Disjunct1, Disjunct2 | Disjuncts3plus]
                ),
                Goal = disj_expr(Context, Disjunct1, Disjunct2,
                    Disjuncts3plus),
                MaybeGoal = ok2(Goal, Warnings)
            ;
                ErrorSpecs = [_ | _],
                % The final value of !DCGVar shouldn't matter.
                MaybeGoal = error2(ErrorSpecs)
            )
        )
    else
        % XXX This generates an error message that is appropriate for goals
        % that are intended to be disjunctions. Should we instead generate
        % a message that also talks about if-then-elses using (C->T;E) syntax?
        % It would be more complete, but also more complex, and therefore
        % potentially more confusing than helpful.
        % We do the same for ";" in parse_non_call_goal.
        Spec = should_have_two_goals_infix(ContextPieces, Context, ";"),
        MaybeGoal = error2([Spec])
    ).

:- pred parse_dcg_goal_disjunction(prog_var::in, cord(format_piece)::in,
    term::in, term::in, maybe(prog_var)::in, maybe(prog_var)::out,
    cord(pair(goal, prog_var))::in, cord(pair(goal, prog_var))::out,
    list(warning_spec)::in, list(warning_spec)::out,
    list(error_spec)::in, list(error_spec)::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out) is det.

parse_dcg_goal_disjunction(DCGVar0, ContextPieces, TermA, TermB,
        !MaybeFirstDiffDCGVar, !DisjunctsDCGVarsCord, !Warnings, !Specs,
        !VarSet, !Counter) :-
    parse_dcg_goal(TermA, ContextPieces, MaybeGoalA,
        !VarSet, !Counter, DCGVar0, DCGVarA),
    (
        MaybeGoalA = ok2(DisjunctA, WarningsA),
        maybe_record_non_initial_dcg_var(DCGVar0, DCGVarA,
            !MaybeFirstDiffDCGVar),
        append_disjunct_dcg_var_to_cord(DCGVarA, DisjunctA,
            !DisjunctsDCGVarsCord),
        % The order of the warnings does not matter.
        !:Warnings = WarningsA ++ !.Warnings
    ;
        MaybeGoalA = error2(SpecsA),
        !:Specs = !.Specs ++ SpecsA
    ),
    ( if
        TermB = term.functor(term.atom(";"), ArgTermsB, _Context),
        ArgTermsB = [TermBA, TermBB],
        not (
            TermBA = term.functor(term.atom("->"), [_, _], _)
        )
    then
        parse_dcg_goal_disjunction(DCGVar0, ContextPieces, TermBA, TermBB,
            !MaybeFirstDiffDCGVar, !DisjunctsDCGVarsCord, !Warnings, !Specs,
            !VarSet, !Counter)
    else
        parse_dcg_goal(TermB, ContextPieces, MaybeGoalB,
            !VarSet, !Counter, DCGVar0, DCGVarB),
        (
            MaybeGoalB = ok2(DisjunctB, WarningsB),
            maybe_record_non_initial_dcg_var(DCGVar0, DCGVarB,
                !MaybeFirstDiffDCGVar),
            append_disjunct_dcg_var_to_cord(DCGVarB, DisjunctB,
                !DisjunctsDCGVarsCord),
            !:Warnings = !.Warnings ++ WarningsB
        ;
            MaybeGoalB = error2(SpecsB),
            !:Specs = !.Specs ++ SpecsB
        )
    ).

    % maybe_record_non_initial_dcg_var(DCGVar0, DCGVarEndBranch,
    %   !MaybeFirstDiffDCGVar):
    %
    % If the current branch updated the dcg variable from DCGVar0 to
    % DCGVarEndBranch, *and* if it is the first branch to have done so,
    % then record DCGVarEndBranch as !:MaybeFirstDiffDCGVar.
    %
:- pred maybe_record_non_initial_dcg_var(prog_var::in, prog_var::in,
    maybe(prog_var)::in, maybe(prog_var)::out) is det.

maybe_record_non_initial_dcg_var(DCGVar0, DCGVarEndBranch,
        !MaybeFirstDiffDCGVar) :-
    ( if DCGVar0 = DCGVarEndBranch then
        true
    else
        (
            !.MaybeFirstDiffDCGVar = yes(_)
        ;
            !.MaybeFirstDiffDCGVar = no,
            !:MaybeFirstDiffDCGVar = yes(DCGVarEndBranch)
        )
    ).

:- pred append_disjunct_dcg_var_to_cord(prog_var::in, goal::in,
    cord(pair(goal, prog_var))::in, cord(pair(goal, prog_var))::out) is det.

append_disjunct_dcg_var_to_cord(DCGVar, Goal, !DisjunctsDCGVarsCord) :-
    % We flatten disjunctions, for reasons explained in the comment
    % at the top of tests/hard_coded/flatten_disjunctions.m.
    ( if Goal = disj_expr(_Ctxt, Disjunct1, Disjunct2, Disjuncts3plus) then
        append_disjunct_dcg_var_to_cord(DCGVar, Disjunct1,
            !DisjunctsDCGVarsCord),
        append_disjunct_dcg_var_to_cord(DCGVar, Disjunct2,
            !DisjunctsDCGVarsCord),
        list.foldl(append_disjunct_dcg_var_to_cord(DCGVar), Disjuncts3plus,
            !DisjunctsDCGVarsCord)
    else
        cord.snoc(Goal - DCGVar, !DisjunctsDCGVarsCord)
    ).

    % Given a disjunction in which the initial and final values of the
    % DCG variable are InitDCGVar and FinalDCGVar respectively,
    % ensure that all disjuncts end up with FinalDCGVar, whatever
    % the updates (if any) were within each disjunct.
    %
:- pred bring_disjuncts_up_to(prog_var::in, prog_var::in, prog_context::in,
    assoc_list(goal, prog_var)::in, list(goal)::in, list(goal)::out) is det.

bring_disjuncts_up_to(_InitDCGVar, _FinalDCGVar, _Context, [], !Disjuncts).
bring_disjuncts_up_to(InitDCGVar, FinalDCGVar, Context,
        [RevDisjunctDCGVar | RevDisjunctsDCGVars], !Disjuncts) :-
    bring_disjunct_up_to(InitDCGVar, FinalDCGVar, Context,
        RevDisjunctDCGVar, !Disjuncts),
    bring_disjuncts_up_to(InitDCGVar, FinalDCGVar, Context,
        RevDisjunctsDCGVars, !Disjuncts).

:- pred bring_disjunct_up_to(prog_var::in, prog_var::in, prog_context::in,
    pair(goal, prog_var)::in, list(goal)::in, list(goal)::out) is det.

bring_disjunct_up_to(InitDCGVar, FinalDCGVar, Context,
        RevDisjunctDCGVar, !Disjuncts) :-
    RevDisjunctDCGVar = Disjunct0 - DisjunctEndDCGVar,
    ( if DisjunctEndDCGVar = FinalDCGVar then
        Disjunct = Disjunct0
    else if DisjunctEndDCGVar = InitDCGVar then
        Unify = unify_expr(Context,
            term.variable(FinalDCGVar, Context),
            term.variable(InitDCGVar, Context),
            purity_pure),
        Disjunct = conj_expr(Context, Disjunct0, [Unify])
    else
        prog_util.rename_in_goal(DisjunctEndDCGVar, FinalDCGVar,
            Disjunct0, Disjunct)
    ),
    !:Disjuncts = [Disjunct | !.Disjuncts].

%---------------------%

:- pred parse_dcg_goal_else(list(term)::in,
    prog_context::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.
:- pragma inline(pred(parse_dcg_goal_else/10)).

parse_dcg_goal_else(ArgTerms, Context, ContextPieces,
        MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    ( if
        ArgTerms = [CondThenTerm, ElseGoalTerm],
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
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, Context, Pieces),
        MaybeGoal = error2([Spec])
    ).

%---------------------%

:- pred parse_dcg_goal_if(list(term)::in,
    prog_context::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.
:- pragma inline(pred(parse_dcg_goal_if/10)).

parse_dcg_goal_if(ArgTerms, Context, ContextPieces,
        MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    ( if
        ArgTerms = [term.functor(term.atom("then"),
            [CondGoalTerm, ThenGoalTerm], _)]
    then
        InVar = !.DCGVar,
        parse_dcg_if_then(CondGoalTerm, ThenGoalTerm, Context, ContextPieces,
            MaybeVarsCondGoal, MaybeThenGoal, !VarSet, !Counter, !DCGVar),
        OutVar = !.DCGVar,
        ( if
            MaybeVarsCondGoal =
                ok4(Vars, StateVars, CondGoal, CondWarningSpecs),
            MaybeThenGoal = ok2(ThenGoal, ThenWarningSpecs)
        then
            WarningSpecs = CondWarningSpecs ++ ThenWarningSpecs,
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
            MaybeGoal = ok2(Goal, WarningSpecs)
        else
            Specs = get_any_errors_warnings4(MaybeVarsCondGoal) ++
                get_any_errors_warnings2(MaybeThenGoal),
            MaybeGoal = error2(Specs)
        )
    else
        Pieces = [words("Error: the "), quote("else"), words("operator"),
            words("should occur in expressions of the form"),
            quote("( if goal then goal else goal )"), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeGoal = error2([Spec])
    ).

%---------------------%

:- pred parse_dcg_goal_braces(list(term)::in,
    prog_context::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.
:- pragma inline(pred(parse_dcg_goal_braces/10)).

parse_dcg_goal_braces(ArgTerms, Context, ContextPieces,
        MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    (
        ArgTerms = [],
        Pieces = [words("Error: there should be at least one goal"),
            words("between the curly braces."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeGoal = error2([Spec])
    ;
        ArgTerms = [HeadTerm | TailTerm],
        % Ordinary goal inside { curly braces }.
        % The parser treats '{}/N' terms as tuples, so we need to undo
        % the parsing of the argument conjunction here.
        one_or_more_to_conjunction(Context, one_or_more(HeadTerm, TailTerm),
            SubGoal),
        parse_goal(SubGoal, ContextPieces, MaybeGoal, !VarSet)
    ).

%---------------------%

:- pred parse_dcg_goal_nil(list(term)::in,
    prog_context::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.
:- pragma inline(pred(parse_dcg_goal_nil/10)).

parse_dcg_goal_nil(ArgTerms, Context, _ContextPieces,
        MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    (
        ArgTerms = [],
        % Empty list - just unify the input and output DCG args.
        InVar = !.DCGVar,
        new_dcg_var(!VarSet, !Counter, OutVar),
        !:DCGVar = OutVar,
        Goal = unify_expr(Context,
            term.variable(InVar, Context), term.variable(OutVar, Context),
            purity_pure),
        MaybeGoal = ok2(Goal, [])
    ;
        ArgTerms = [_ | _],
        Pieces = [words("Error: in DCG clauses,"),
            words("the"), quote("[]"), words("operator"),
            words("may only be used to match the input"),
            words("against a list of zero items,"),
            words("and must therefore be used with arity 0."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeGoal = error2([Spec])
    ).

%---------------------%

:- pred parse_dcg_goal_cons(list(term)::in,
    prog_context::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.
:- pragma inline(pred(parse_dcg_goal_cons/10)).

parse_dcg_goal_cons(ArgTerms, Context, _ContextPieces,
        MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    ( if ArgTerms = [X, Xs] then
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
            MaybeGoal = ok2(Goal, [])
        else
            Pieces = [words("Error: there is no"),
                quote("[]"), words("at the end of the list."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, Pieces),
            MaybeGoal = error2([Spec])
        )
    else
        Pieces = [words("Error: in DCG clauses,"),
            words("the"), quote("[|]"), words("operator"),
            words("may only be used to match the input"),
            words("against a list of one or more items,"),
            words("and must therefore be used with arity 2."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeGoal = error2([Spec])
    ).

%---------------------%

:- pred parse_dcg_goal_equal(list(term)::in,
    prog_context::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.
:- pragma inline(pred(parse_dcg_goal_equal/10)).

parse_dcg_goal_equal(ArgTerms, Context, ContextPieces,
        MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    ( if ArgTerms = [ArgTerm0] then
        % Call to '='/1 - unify argument with DCG input arg.
        term.coerce(ArgTerm0, ArgTerm),
        DCGVarTerm = variable(!.DCGVar, Context),
        Goal = unify_expr(Context, ArgTerm, DCGVarTerm, purity_pure),
        MaybeGoal = ok2(Goal, [])
    else
        Spec = should_have_two_terms_infix(ContextPieces, Context, "="),
        MaybeGoal = error2([Spec])
    ).

%---------------------%

:- pred parse_dcg_goal_colon_equal(list(term)::in,
    prog_context::in, cord(format_piece)::in,
    maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.
:- pragma inline(pred(parse_dcg_goal_colon_equal/10)).

parse_dcg_goal_colon_equal(ArgTerms, Context, _ContextPieces,
        MaybeGoal, !VarSet, !Counter, !DCGVar) :-
    ( if ArgTerms = [ArgTerm0] then
        % ":=(Term)" is a DCG output unification, which unifies Term
        % with the implicit DCG output argument, while ignoring
        % the input DCG argument.
        new_dcg_var(!VarSet, !Counter, OutVar),
        !:DCGVar = OutVar,
        term.coerce(ArgTerm0, ArgTerm),
        OutVarTerm = variable(OutVar, Context),
        Goal = unify_expr(Context, ArgTerm, OutVarTerm, purity_pure),
        MaybeGoal = ok2(Goal, [])
    else
        % "^ field_list := Term" is a DCG field update, which replaces
        % the named field in the implicit DCG argument.
        %
        % Such field updates are represented as calls in the parse tree.
        % The field update is implemented by goal_expr_to_goal.m when
        % we convert the parse tree to the HLDS. That code will also
        % generate the error message if ArgTerms has the wrong shape.
        SymName = unqualified(":="),
        list.map(term.coerce, ArgTerms, ProgArgTerms),
        make_dcg_call(SymName, ProgArgTerms, Context, Goal,
            !VarSet, !Counter, !DCGVar),
        MaybeGoal = ok2(Goal, [])
    ).

%---------------------------------------------------------------------------%

:- pred parse_some_vars_dcg_goal(term::in, cord(format_piece)::in,
    maybe4(list(prog_var), list(prog_var), goal, list(warning_spec))::out,
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
        MaybeGoal = ok2(Goal, GoalWarningSpecs)
    then
        list.map(term.coerce_var, Vars0, Vars),
        list.map(term.coerce_var, StateVars0, StateVars),
        MaybeVarsGoal = ok4(Vars, StateVars, Goal, GoalWarningSpecs)
    else
        Specs = get_any_errors1(MaybeVars) ++
            get_any_errors_warnings2(MaybeGoal),
        MaybeVarsGoal = error4(Specs)
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
    cord(format_piece)::in,
    maybe4(list(prog_var), list(prog_var), goal, list(warning_spec))::out,
    maybe2(goal, list(warning_spec))::out, prog_varset::in, prog_varset::out,
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
            MaybeThen1 = ok2(Then1, ThenWarningSpecs),
            new_dcg_var(!VarSet, !Counter, Var),
            Unify = unify_expr(Context,
                term.variable(Var, Context), term.variable(Var2, Context),
                purity_pure),
            Then = conj_expr(Context, Then1, [Unify]),
            MaybeThen = ok2(Then, ThenWarningSpecs)
        ;
            MaybeThen1 = error2(_),
            MaybeThen = MaybeThen1,
            Var = Var2                  % Dummy; the value shouldn't matter.
        )
    else
        MaybeThen = MaybeThen1,
        Var = Var2
    ).

:- pred parse_dcg_if_then_else(term::in, term::in, term::in, prog_context::in,
    cord(format_piece)::in, maybe2(goal, list(warning_spec))::out,
    prog_varset::in, prog_varset::out, counter::in, counter::out,
    prog_var::in, prog_var::out) is det.

parse_dcg_if_then_else(CondGoalTerm, ThenGoalTerm, ElseGoalTerm,
        Context, ContextPieces, MaybeGoal, !VarSet, !Counter, Var0, Var) :-
    parse_dcg_if_then(CondGoalTerm, ThenGoalTerm, Context, ContextPieces,
        MaybeVarsCond, MaybeThen1, !VarSet, !Counter, Var0, VarThen),
    parse_dcg_goal(ElseGoalTerm, ContextPieces, MaybeElse1,
        !VarSet, !Counter, Var0, VarElse),
    ( if
        MaybeVarsCond = ok4(Vars, StateVars, Cond, CondWarningSpecs),
        MaybeThen1 = ok2(Then1, ThenWarningSpecs),
        MaybeElse1 = ok2(Else1, ElseWarningSpecs)
    then
        WarningSpecs = CondWarningSpecs ++
            ThenWarningSpecs ++ ElseWarningSpecs,
        ( if VarThen = Var0, VarElse = Var0 then
            Var = Var0,
            Then = Then1,
            Else = Else1
        else if VarThen = Var0 then
            Var = VarElse,
            Unify = unify_expr(Context,
                term.variable(Var, Context), term.variable(VarThen, Context),
                purity_pure),
            Then = conj_expr(Context, Then1, [Unify]),
            Else = Else1
        else if VarElse = Var0 then
            Var = VarThen,
            Then = Then1,
            Unify = unify_expr(Context,
                term.variable(Var, Context), term.variable(VarElse, Context),
                purity_pure),
            Else = conj_expr(Context, Else1, [Unify])
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
        MaybeGoal = ok2(Goal, WarningSpecs)
    else
        Specs = get_any_errors_warnings4(MaybeVarsCond) ++
            get_any_errors_warnings2(MaybeThen1) ++
            get_any_errors_warnings2(MaybeElse1),
        MaybeGoal = error2(Specs),
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
    string.format("DCG_%d", [i(N)], VarName),
    varset.new_named_var(VarName, DCGVar, !VarSet).

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
