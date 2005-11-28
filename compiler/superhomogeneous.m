%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: superhomogeneous.m.
% Main author: fjh.

% This module performs the conversion of clause bodies
% to superhomogeneous form.

%-----------------------------------------------------------------------------%

:- module hlds__make_hlds__superhomogeneous.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.make_hlds.state_var.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type arg_context
    --->    head(pred_or_func, arity)
            % the arguments in the head of the clause

    ;       call(call_id)
            % the arguments in a call to a predicate

    ;       functor(            % the arguments in a functor
                cons_id,
                unify_main_context,
                unify_sub_contexts
            ).

    % `insert_arg_unifications' takes a list of variables,
    % a list of terms to unify them with, and a goal, and
    % inserts the appropriate unifications onto the front of
    % the goal.  It calls `unravel_unification' to ensure
    % that each unification gets reduced to superhomogeneous form.
    % It also gets passed an `arg_context', which indicates
    % where the terms came from.
    %
    % We never insert unifications of the form X = X.
    %
:- pred insert_arg_unifications(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

:- pred insert_arg_unifications_with_supplied_contexts(list(prog_var)::in,
    list(prog_term)::in, assoc_list(int, arg_context)::in, prog_context::in,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

    % append_arg_unifications is the same as insert_arg_unifications,
    % except that the unifications are added after the goal rather
    % than before the goal.
    %
:- pred append_arg_unifications(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in, hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

:- pred unravel_unification(prog_term::in, prog_term::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, purity::in,
    hlds_goal::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

    % make_fresh_arg_vars(Args, VarSet0, Vars, VarSet, !SInfo, !IO):
    %   `Vars' is a list of distinct variables corresponding to
    %   the terms in `Args'.  For each term in `Args', if
    %   the term is a variable V which is distinct from the
    %   variables already produced, then the corresponding
    %   variable in `Vars' is just V, otherwise a fresh variable
    %   is allocated from `VarSet0'.   `VarSet' is the resulting
    %   varset after all the necessary variables have been allocated.
    %   !SInfo and !IO are required to handle state variables.
    %
    %   For efficiency, the list `Vars' is constructed backwards
    %   and then reversed to get the correct order.
    %
:- pred make_fresh_arg_vars(list(prog_term)::in, list(prog_var)::out,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

:- pred make_fresh_arg_var(prog_term::in, prog_var::out, list(prog_var)::in,
    prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module check_hlds.purity.
:- import_module hlds.goal_util.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.field_access.
:- import_module hlds.make_hlds.qual_info.
:- import_module libs.compiler_util.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_io_dcg.
:- import_module parse_tree.prog_io_goal.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module svvarset.
:- import_module svset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

insert_arg_unifications(HeadVars, Args0, Context, ArgContext,
        !Goal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    (
        HeadVars = []
    ;
        HeadVars = [_ | _],
        !.Goal = _ - GoalInfo0,
        goal_to_conj_list(!.Goal, Goals0),
        substitute_state_var_mappings(Args0, Args, !VarSet, !SInfo, !IO),
        insert_arg_unifications_2(HeadVars, Args, Context, ArgContext,
            0, Goals0, Goals, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        goal_info_set_context(Context, GoalInfo0, GoalInfo),
        conj_list_to_goal(Goals, GoalInfo, !:Goal)
    ).

:- pred insert_arg_unifications_2(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in, int::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

insert_arg_unifications_2([], [_ | _], _, _, _, _, _, _, _, !ModuleInfo,
        !QualInfo, !SInfo, !IO) :-
    unexpected(this_file, "insert_arg_unifications_2: length mismatch").
insert_arg_unifications_2([_ | _], [], _, _, _, _, _, _, _, !ModuleInfo,
        !QualInfo, !SInfo, !IO) :-
    unexpected(this_file, "insert_arg_unifications_2: length mismatch").
insert_arg_unifications_2([], [], _, _, _, !Goals, !VarSet, !ModuleInfo,
        !QualInfo, !SInfo, !IO).
insert_arg_unifications_2([Var | Vars], [Arg | Args], Context, ArgContext,
        N0, !Goals, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    N1 = N0 + 1,
    insert_arg_unification(Var, Arg, Context, ArgContext, N1,
        !VarSet, ArgUnifyConj, !ModuleInfo, !QualInfo, !SInfo, !IO),
    (
        ArgUnifyConj = [],
        % Allow the recursive call to be tail recursive.
        insert_arg_unifications_2(Vars, Args, Context, ArgContext,
            N1, !Goals, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO)
    ;
        ArgUnifyConj = [_ | _],
        insert_arg_unifications_2(Vars, Args, Context, ArgContext,
            N1, !Goals, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        list__append(ArgUnifyConj, !.Goals, !:Goals)
    ).

insert_arg_unifications_with_supplied_contexts(ArgVars, ArgTerms0, ArgContexts,
        Context, !Goal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    (
        ArgVars = []
    ;
        ArgVars = [_ | _],
        !.Goal = _ - GoalInfo0,
        goal_to_conj_list(!.Goal, GoalList0),
        substitute_state_var_mappings(ArgTerms0, ArgTerms, !VarSet, !SInfo,
            !IO),
        insert_arg_unifications_with_supplied_contexts_2(ArgVars, ArgTerms,
            ArgContexts, Context, GoalList0, GoalList, !VarSet,
            !ModuleInfo, !QualInfo, !SInfo, !IO),
        goal_info_set_context(Context, GoalInfo0, GoalInfo),
        conj_list_to_goal(GoalList, GoalInfo, !:Goal)
    ).

:- pred insert_arg_unifications_with_supplied_contexts_2(list(prog_var)::in,
    list(prog_term)::in, assoc_list(int, arg_context)::in, prog_context::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

insert_arg_unifications_with_supplied_contexts_2(Vars, Terms, ArgContexts,
        Context, !Goals, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    (
        Vars = [],
        Terms = [],
        ArgContexts = []
    ->
        true
    ;
        Vars = [Var | VarsTail],
        Terms = [Term | TermsTail],
        ArgContexts = [ArgNumber - ArgContext | ArgContextsTail]
    ->
        insert_arg_unification(Var, Term, Context, ArgContext, ArgNumber,
            !VarSet, UnifyConj, !ModuleInfo, !QualInfo, !SInfo, !IO),
        insert_arg_unifications_with_supplied_contexts_2(VarsTail, TermsTail,
            ArgContextsTail, Context, !Goals, !VarSet, !ModuleInfo, !QualInfo,
            !SInfo, !IO),
        list__append(UnifyConj, !.Goals, !:Goals)
    ;
        unexpected(this_file, "insert_arg_unifications_with_supplied_contexts")
    ).

:- pred insert_arg_unification(prog_var::in, prog_term::in, prog_context::in,
    arg_context::in, int::in, prog_varset::in, prog_varset::out,
    list(hlds_goal)::out, module_info::in, module_info::out,
    qual_info::in, qual_info::out, svar_info::in, svar_info::out,
    io::di, io::uo) is det.

insert_arg_unification(Var, Arg, Context, ArgContext, N1, !VarSet,
        ArgUnifyConj, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    ( Arg = term__variable(Var) ->
        % Skip unifications of the form `X = X'
        ArgUnifyConj = []
    ;
        arg_context_to_unify_context(ArgContext, N1, UnifyMainContext,
            UnifySubContext),
        unravel_unification(term__variable(Var), Arg, Context,
            UnifyMainContext, UnifySubContext, purity_pure, Goal,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        goal_to_conj_list(Goal, ArgUnifyConj)
    ).

append_arg_unifications(HeadVars, Args0, Context, ArgContext,
        !Goal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    (
        HeadVars = []
    ;
        HeadVars = [_ | _],
        !.Goal = _ - GoalInfo,
        goal_to_conj_list(!.Goal, List0),
        substitute_state_var_mappings(Args0, Args, !VarSet,
            !SInfo, !IO),
        append_arg_unifications_2(HeadVars, Args, Context, ArgContext,
            0, List0, List, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        conj_list_to_goal(List, GoalInfo, !:Goal)
    ).

:- pred append_arg_unifications_2(list(prog_var)::in, list(prog_term)::in,
    prog_context::in, arg_context::in, int::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

append_arg_unifications_2([], [_|_], _, _, _, _, _, _, _, !ModuleInfo,
        !QualInfo, !SInfo, !IO) :-
    unexpected(this_file, "append_arg_unifications_2: length mismatch").
append_arg_unifications_2([_|_], [], _, _, _, _, _, _, _, !ModuleInfo,
        !QualInfo, !SInfo, !IO) :-
    unexpected(this_file, "append_arg_unifications_2: length mismatch").
append_arg_unifications_2([], [], _, _, _, !List, !VarSet, !ModuleInfo,
        !QualInfo, !SInfo, !IO).
append_arg_unifications_2([Var|Vars], [Arg|Args], Context, ArgContext,
        N0, !List, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    N1 = N0 + 1,
    append_arg_unification(Var, Arg, Context, ArgContext, N1, ConjList,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
    list__append(!.List, ConjList, !:List),
    append_arg_unifications_2(Vars, Args, Context, ArgContext, N1,
        !List, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO).

:- pred append_arg_unification(prog_var::in, prog_term::in, prog_context::in,
    arg_context::in, int::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

append_arg_unification(Var, Arg, Context, ArgContext, N1, ConjList,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    ( Arg = term__variable(Var) ->
        % skip unifications of the form `X = X'
        ConjList = []
    ;
        arg_context_to_unify_context(ArgContext, N1, UnifyMainContext,
            UnifySubContext),
        unravel_unification(term__variable(Var), Arg, Context,
            UnifyMainContext, UnifySubContext, purity_pure, Goal,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        goal_to_conj_list(Goal, ConjList)
    ).

:- pred arg_context_to_unify_context(arg_context::in, int::in,
    unify_main_context::out, unify_sub_contexts::out) is det.

arg_context_to_unify_context(head(PredOrFunc, Arity), ArgNum,
        ArgContext, []) :-
    ( PredOrFunc = function, ArgNum = Arity ->
        % it's the function result term in the head
        ArgContext = head_result
    ;
        % it's a head argument
        ArgContext = head(ArgNum)
    ).
arg_context_to_unify_context(call(PredId), ArgNum, call(PredId, ArgNum), []).
arg_context_to_unify_context(functor(ConsId, MainContext, SubContexts), ArgNum,
    MainContext, [ConsId - ArgNum | SubContexts]).

%-----------------------------------------------------------------------------%

make_fresh_arg_vars(Args, Vars, !VarSet, !SInfo, !IO) :-
    make_fresh_arg_vars_2(Args, [], Vars1, !VarSet, !SInfo, !IO),
    list__reverse(Vars1, Vars).

:- pred make_fresh_arg_vars_2(list(prog_term)::in, list(prog_var)::in,
    list(prog_var)::out, prog_varset::in,prog_varset::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

make_fresh_arg_vars_2([], Vars, Vars, !VarSet, !SInfo, !IO).
make_fresh_arg_vars_2([Arg | Args], Vars0, Vars, !VarSet, !SInfo, !IO) :-
    make_fresh_arg_var(Arg, Var, Vars0, !VarSet, !SInfo, !IO),
    make_fresh_arg_vars_2(Args, [Var | Vars0], Vars, !VarSet, !SInfo, !IO).

make_fresh_arg_var(Arg0, Var, Vars0, !VarSet, !SInfo, !IO) :-
    substitute_state_var_mapping(Arg0, Arg, !VarSet, !SInfo, !IO),
    (
        Arg = term__variable(ArgVar),
        \+ list__member(ArgVar, Vars0)
    ->
        Var = ArgVar
    ;
        varset__new_var(!.VarSet, Var, !:VarSet)
    ).

%-----------------------------------------------------------------------------%

    %
    % XXX We could do better on the error messages for
    % lambda expressions and field extraction and update expressions.
    %

unravel_unification(LHS0, RHS0, Context, MainContext, SubContext,
        Purity, Goal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    substitute_state_var_mapping(LHS0, LHS, !VarSet, !SInfo, !IO),
    substitute_state_var_mapping(RHS0, RHS, !VarSet, !SInfo, !IO),
    unravel_unification_2(LHS, RHS, Context, MainContext, SubContext,
        Purity, Goal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO).

:- pred unravel_unification_2(prog_term::in, prog_term::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, purity::in,
    hlds_goal::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

    % `X = Y' needs no unravelling.

unravel_unification_2(term__variable(X), term__variable(Y), Context,
        MainContext, SubContext, Purity, Goal, !VarSet,
        !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    make_atomic_unification(X, var(Y), Context, MainContext, SubContext,
        Purity, Goal, !QualInfo).

    % If we find a unification of the form
    %   X = f(A1, A2, A3)
    % we replace it with
    %   X = f(NewVar1, NewVar2, NewVar3),
    %   NewVar1 = A1,
    %   NewVar2 = A2,
    %   NewVar3 = A3.
    % In the trivial case `X = c', no unravelling occurs.

unravel_unification_2(term__variable(X), RHS, Context, MainContext, SubContext,
        Purity, Goal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    RHS = term__functor(F, Args1, FunctorContext),
    substitute_state_var_mappings(Args1, Args, !VarSet, !SInfo, !IO),
    (
        % Handle explicit type qualification.
        (
            F = term__atom("with_type")
        ;
            F = term__atom(":")
        ),
        Args = [RVal, DeclType0]
    ->
        % DeclType0 is a prog_term, but it is really a type so we coerce it
        % to a generic term before parsing it.
        term__coerce(DeclType0, DeclType1),
        parse_type(DeclType1, DeclTypeResult),
        (
            DeclTypeResult = ok(DeclType),
            varset__coerce(!.VarSet, DeclVarSet),
            process_type_qualification(X, DeclType, DeclVarSet,
                Context, !ModuleInfo, !QualInfo, !IO)
        ;
            DeclTypeResult = error(Msg, ErrorTerm),
            % The varset is a prog_varset even though it contains the names
            % of type variables in ErrorTerm, which is a generic term.
            GenericVarSet = varset__coerce(!.VarSet),
            TermStr = mercury_term_to_string(ErrorTerm, GenericVarSet, no),
            Pieces = [words("In explicit type qualification:"),
                    words(Msg),
                    suffix(":"),
                    fixed("`" ++ TermStr ++ "'.")],
            write_error_pieces(Context, 0, Pieces, !IO),
            io.set_exit_status(1, !IO)
        ),
        unravel_unification(term__variable(X), RVal, Context, MainContext,
            SubContext, Purity, Goal, !VarSet, !ModuleInfo, !QualInfo,
            !SInfo, !IO)
    ;
        % Handle unification expressions.
        F = term__atom("@"),
        Args = [LVal, RVal]
    ->
        unravel_unification(term__variable(X), LVal, Context,
            MainContext, SubContext, Purity, Goal1,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        unravel_unification(term__variable(X), RVal, Context,
            MainContext, SubContext, Purity, Goal2,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        goal_info_init(GoalInfo),
        goal_to_conj_list(Goal1, ConjList1),
        goal_to_conj_list(Goal2, ConjList2),
        list__append(ConjList1, ConjList2, ConjList),
        conj_list_to_goal(ConjList, GoalInfo, Goal)
    ;
        % Handle higher-order pred and func expressions.
        parse_rule_term(Context, RHS, HeadTerm0, GoalTerm1),
        term__coerce(HeadTerm0, HeadTerm1),
        parse_purity_annotation(HeadTerm1, LambdaPurity, HeadTerm),
        (
            parse_pred_expression(HeadTerm, EvalMethod0, Vars0, Modes0, Det0)
        ->
            PredOrFunc = predicate,
            EvalMethod = EvalMethod0,
            Vars1 = Vars0,
            Modes1 = Modes0,
            Det1 = Det0
        ;
            parse_func_expression(HeadTerm, EvalMethod, Vars1, Modes1, Det1),
            PredOrFunc = function
        )
    ->
        add_clause__qualify_lambda_mode_list(Modes1, Modes, Context,
            !QualInfo, !IO),
        Det = Det1,
        term__coerce(GoalTerm1, GoalTerm),
        parse_goal(GoalTerm, ParsedGoal, !VarSet),
        build_lambda_expression(X, Purity, LambdaPurity, PredOrFunc,
            EvalMethod, Vars1, Modes, Det, ParsedGoal, Context, MainContext,
            SubContext, Goal, !VarSet, !ModuleInfo, !QualInfo, !.SInfo, !IO)
    ;
        % handle higher-order dcg pred expressions -
        % same semantics as higher-order pred expressions,
        % but has two extra arguments, and the goal is expanded
        % as a DCG goal.
        F = term__atom("-->"),
        Args = [PredTerm0, GoalTerm0],
        term__coerce(PredTerm0, PredTerm1),
        parse_purity_annotation(PredTerm1, DCGLambdaPurity, PredTerm),
        parse_dcg_pred_expression(PredTerm, EvalMethod, Vars0, Modes0, Det)
    ->
        add_clause__qualify_lambda_mode_list(Modes0, Modes, Context, !QualInfo,
            !IO),
        term__coerce(GoalTerm0, GoalTerm),
        parse_dcg_pred_goal(GoalTerm, ParsedGoal, DCG0, DCGn, !VarSet),
        list__append(Vars0, [term__variable(DCG0), term__variable(DCGn)],
            Vars1),
        build_lambda_expression(X, Purity, DCGLambdaPurity, predicate,
            EvalMethod, Vars1, Modes, Det, ParsedGoal, Context, MainContext,
            SubContext, Goal0, !VarSet, !ModuleInfo, !QualInfo, !.SInfo, !IO),
        Goal0 = GoalExpr - GoalInfo0,
        add_goal_info_purity_feature(Purity, GoalInfo0, GoalInfo),
        Goal = GoalExpr - GoalInfo
    ;
        % handle if-then-else expressions
        (
            F = term__atom("else"),
            IfThenTerm = term__functor(
                term__atom("if"),
                [term__functor(term__atom("then"), [IfTerm0, ThenTerm], _)],
                _),
            Args = [IfThenTerm, ElseTerm]
        ;
            F = term__atom(";"),
            Args = [term__functor(term__atom("->"), [IfTerm0, ThenTerm], _),
                ElseTerm]
        ),
        term__coerce(IfTerm0, IfTerm),
        parse_some_vars_goal(IfTerm, Vars, StateVars, IfParseTree, !VarSet)
    ->
        BeforeSInfo = !.SInfo,
        prepare_for_if_then_else_expr(StateVars, !VarSet, !SInfo),

        map__init(EmptySubst),
        transform_goal(IfParseTree, EmptySubst, IfGoal, !VarSet,
            !ModuleInfo, !QualInfo, !SInfo, !IO),

        finish_if_then_else_expr_condition(BeforeSInfo, !SInfo),

        unravel_unification(term__variable(X), ThenTerm,
            Context, MainContext, SubContext, Purity, ThenGoal,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),

        finish_if_then_else_expr_then_goal(StateVars, BeforeSInfo, !SInfo),

        unravel_unification(term__variable(X), ElseTerm,
            Context, MainContext, SubContext, Purity,
            ElseGoal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),

        IfThenElse = if_then_else(StateVars ++ Vars, IfGoal, ThenGoal,
            ElseGoal),
        goal_info_init(Context, GoalInfo),
        Goal = IfThenElse - GoalInfo
    ;
        % handle field extraction expressions
        F = term__atom("^"),
        Args = [InputTerm, FieldNameTerm],
        parse_field_list(FieldNameTerm, FieldNameResult),
        FieldNameResult = ok(FieldNames)
    ->
        make_fresh_arg_var(InputTerm, InputTermVar, [], !VarSet, !SInfo, !IO),
        expand_get_field_function_call(Context, MainContext, SubContext,
            FieldNames, X, InputTermVar, Purity, !VarSet, Functor, _, Goal0,
            !ModuleInfo, !QualInfo, !SInfo, !IO),

        ArgContext = functor(Functor, MainContext, SubContext),
        insert_arg_unifications([InputTermVar], [InputTerm],
            FunctorContext, ArgContext, Goal0, Goal,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO)
    ;
        % handle field update expressions
        F = term__atom(":="),
        Args = [FieldDescrTerm, FieldValueTerm],
        FieldDescrTerm = term__functor(term__atom("^"),
            [InputTerm, FieldNameTerm], _),
        parse_field_list(FieldNameTerm, FieldNameResult),
        FieldNameResult = ok(FieldNames)
    ->
        make_fresh_arg_var(InputTerm, InputTermVar, [], !VarSet, !SInfo, !IO),
        make_fresh_arg_var(FieldValueTerm, FieldValueVar, [InputTermVar],
            !VarSet, !SInfo, !IO),

        expand_set_field_function_call(Context, MainContext, SubContext,
            FieldNames, FieldValueVar, InputTermVar, X, !VarSet,
            Functor, InnerFunctor - FieldSubContext, Goal0,
            !ModuleInfo, !QualInfo, !SInfo, !IO),

        TermArgContext = functor(Functor, MainContext, SubContext),
        TermArgNumber = 1,
        FieldArgContext = functor(InnerFunctor, MainContext, FieldSubContext),
        FieldArgNumber = 2,
        ArgContexts = [TermArgNumber - TermArgContext,
            FieldArgNumber - FieldArgContext],
        insert_arg_unifications_with_supplied_contexts(
            [InputTermVar, FieldValueVar],
            [InputTerm, FieldValueTerm], ArgContexts, Context,
            Goal0, Goal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO)
    ;
        % handle the usual case
        parse_qualified_term(RHS, RHS, "", MaybeFunctor),
        (
            MaybeFunctor = ok(FunctorName, FunctorArgs),
            list__length(FunctorArgs, Arity),
            ConsId = cons(FunctorName, Arity)
        ;
            % float, int or string constant
            %   - any errors will be caught by typechecking
            MaybeFunctor = error(_, _),
            list__length(Args, Arity),
            ConsId = make_functor_cons_id(F, Arity),
            FunctorArgs = Args
        ),
        (
            FunctorArgs = [],
            make_atomic_unification(X, functor(ConsId, no, []), Context,
                MainContext, SubContext, Purity, Goal0, !QualInfo),
            Goal0 = GoalExpr - GoalInfo0,
            add_goal_info_purity_feature(Purity, GoalInfo0, GoalInfo),
            % We could attach the from_ground_term feature to Goal,
            % but there would be no gain from doing so, whereas the
            % increase would lead to a slight increase in memory and time
            % requirements.
            Goal = GoalExpr - GoalInfo
        ;
            FunctorArgs = [_ | _],
            make_fresh_arg_vars(FunctorArgs, HeadVars, !VarSet, !SInfo, !IO),
            make_atomic_unification(X, functor(ConsId, no, HeadVars), Context,
                MainContext, SubContext, Purity, Goal0, !QualInfo),
            ArgContext = functor(ConsId, MainContext, SubContext),
            % Should this be insert_... rather than append_...?
            % No, because that causes efficiency problems
            % with type-checking :-(
            % But for impure unifications, we need to do
            % this, because mode reordering can't reorder
            % around the functor unification.
            ( Purity = purity_pure ->
                append_arg_unifications(HeadVars, FunctorArgs, FunctorContext,
                    ArgContext, Goal0, Goal2, !VarSet,
                    !ModuleInfo, !QualInfo, !SInfo, !IO)
            ;
                Goal0 = GoalExpr0 - GoalInfo0,
                add_goal_info_purity_feature(Purity, GoalInfo0, GoalInfo1),
                Goal1 = GoalExpr0 - GoalInfo1,
                insert_arg_unifications(HeadVars, FunctorArgs, FunctorContext,
                    ArgContext, Goal1, Goal2, !VarSet,
                    !ModuleInfo, !QualInfo, !SInfo, !IO)
            ),
            % This "optimization" is disabled, because the extra cost of
            % traversing the scope goals in typechecking is more than the
            % savings from the reduction in delays/wakeups in modechecking.
            (
                semidet_fail,
                ground_terms(FunctorArgs)
            ->
                % This insertion of the `scope' goal is undone by the code
                % handling `scope' goals in modecheck_goal_expr in modes.m.

                Goal2 = _GoalExpr2 - GoalInfo,
                GoalExpr = scope(from_ground_term(X), Goal2),
                Goal = GoalExpr - GoalInfo
            ;
                Goal = Goal2
            )
        )
    ).

    % Handle `f(...) = X' in the same way as `X = f(...)'.

unravel_unification_2(term__functor(F, As, FC), term__variable(Y), C, MC, SC,
        Purity, Goal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    unravel_unification(term__variable(Y), term__functor(F, As, FC), C, MC, SC,
        Purity, Goal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO).

    % If we find a unification of the form `f1(...) = f2(...)',
    % then we replace it with `Tmp = f1(...), Tmp = f2(...)',
    % and then process it according to the rule above.
    % Note that we can't simplify it yet, because we might simplify
    % away type errors.

unravel_unification_2(term__functor(LeftF, LeftAs, LeftC),
        term__functor(RightF, RightAs, RightC),
        Context, MainContext, SubContext,
        Purity, Goal, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    varset__new_var(!.VarSet, TmpVar, !:VarSet),
    unravel_unification(term__variable(TmpVar),
        term__functor(LeftF, LeftAs, LeftC),
        Context, MainContext, SubContext,
        Purity, Goal0, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
    unravel_unification(term__variable(TmpVar),
        term__functor(RightF, RightAs, RightC),
        Context, MainContext, SubContext,
        Purity, Goal1, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
    goal_info_init(GoalInfo),
    goal_to_conj_list(Goal0, ConjList0),
    goal_to_conj_list(Goal1, ConjList1),
    list__append(ConjList0, ConjList1, ConjList),
    conj_list_to_goal(ConjList, GoalInfo, Goal).

:- pred ground_term(term(T)::in) is semidet.

ground_term(term__functor(_, Terms, _)) :-
    ground_terms(Terms).

:- pred ground_terms(list(term(T))::in) is semidet.

ground_terms([]).
ground_terms([Term | Terms]) :-
    ground_term(Term),
    ground_terms(Terms).

%-----------------------------------------------------------------------------%
%
% Code for building lambda expressions
%

:- pred build_lambda_expression(prog_var::in, purity::in, purity::in,
    pred_or_func::in, lambda_eval_method::in, list(prog_term)::in,
    list(mer_mode)::in, determinism::in, goal::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, hlds_goal::out,
    prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, io::di, io::uo) is det.

build_lambda_expression(X, UnificationPurity, LambdaPurity, PredOrFunc,
        EvalMethod, Args0, Modes, Det, ParsedGoal, Context, MainContext,
        SubContext, Goal, !VarSet, !ModuleInfo, !QualInfo, !.SInfo, !IO) :-
    %
    % In the parse tree, the lambda arguments can be any terms, but
    % in the HLDS they must be distinct variables.  So we introduce
    % fresh variables for the lambda arguments, and add appropriate
    % unifications.
    %
    % For example, we convert from:
    %
    %       X = (func(f(A, B), c) = D :- Body )
    %
    % to:
    %
    %       X = (func(H1, H2) = H3 :-
    %           some [A, B] (
    %               H1 = f(A, B),
    %               H2 = c,
    %               Body,
    %               H3 = D
    %       )
    %
    % Note that the quantification is important here.  That's why we
    % need to introduce the explicit `some [...]'.  Variables in the
    % argument positions are lambda-quantified, so when we move them to
    % the body, we need to make them explicitly existentially quantified
    % to avoid capturing any variables of the same name that occur
    % outside this scope.
    %
    % Also, note that any introduced unifications that construct the
    % output arguments for the lambda expression, need to occur *after*,
    % the body of the lambda expression.  This is in case the body of
    % the lambda expression is impure, in which case the mode analyser
    % cannot reorder the unifications; this results in a mode error.
    %
    % XXX the mode analyser *should* be able to reorder such unifications,
    %     especially ones that the compiler introduced itself.
    %
    % For predicates, all variables occurring in the lambda arguments are
    % locally quantified to the lambda goal.  For functions, we need to
    % be careful because variables in arguments should similarly be
    % quantified, but variables in the function return value term (and
    % not in the arguments) should *not* be locally quantified.
    %
    % Create fresh variables, transform the goal to HLDS, and add
    % unifications with the fresh variables.  We use varset.new_vars
    % rather than make_fresh_arg_vars, since for functions we need to
    % ensure that the variable corresponding to the function result term
    % is a new variable, to avoid the function result term becoming
    % lambda-quantified.
    %
    (
        illegal_state_var_func_result(PredOrFunc, Args0, StateVar)
    ->
        report_illegal_func_svar_result(Context, !.VarSet, StateVar, !IO),
        true_goal(Goal)
    ;
        lambda_args_contain_bang_state_var(Args0, StateVar)
    ->
        report_illegal_bang_svar_lambda_arg(Context, !.VarSet, StateVar, !IO),
        true_goal(Goal)
    ;
        prepare_for_lambda(!SInfo),
        substitute_state_var_mappings(Args0, Args, !VarSet, !SInfo, !IO),

        list.length(Args, NumArgs),
        svvarset.new_vars(NumArgs, LambdaVars, !VarSet),
        %
        % Partition the arguments (and their corresponding lambda variables)
        % into two sets: those that are not output, i.e. input and unused,
        % and those that are output.
        %
        (
            partition_args_and_lambda_vars(!.ModuleInfo, Args, LambdaVars,
                Modes, NonOutputArgs0, OutputArgs0, NonOutputLambdaVars0,
                OutputLambdaVars0)
        ->
            NonOutputArgs       = NonOutputArgs0,
            OutputArgs          = OutputArgs0,
            NonOutputLambdaVars = NonOutputLambdaVars0,
            OutputLambdaVars    = OutputLambdaVars0
        ;
            unexpected(this_file,
                "Mismatched lists in build_lambda_expression.")
        ),
        
        map.init(Substitution),
        ArgContext = head(PredOrFunc, NumArgs),
        %
        % Create the unifications that need to come before the body of
        % the lambda expression; those corresponding to args whose mode
        % is input or unused.
        %
        hlds_goal.true_goal(HeadBefore0),
        insert_arg_unifications(NonOutputLambdaVars, NonOutputArgs,
            Context, ArgContext, HeadBefore0, HeadBefore, !VarSet,
            !ModuleInfo, !QualInfo, !SInfo, !IO),
        %
        % Create the unifications that need to come after the body of
        % the lambda expression; those corresponding to args whose mode
        % is output.
        %
        hlds_goal.true_goal(HeadAfter0),
        insert_arg_unifications(OutputLambdaVars, OutputArgs,
            Context, ArgContext, HeadAfter0, HeadAfter, !VarSet,
            !ModuleInfo, !QualInfo, !SInfo, !IO),

        prepare_for_body(FinalSVarMap, !VarSet, !SInfo),

        transform_goal(ParsedGoal, Substitution,
            Body, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
        %
        % Fix up any state variable unifications.
        %
        finish_goals(Context, FinalSVarMap, [HeadBefore, Body, HeadAfter],
            HLDS_Goal0, !.SInfo),
        %
        % Figure out which variables we need to explicitly existentially
        % quantify.
        %
        (
            PredOrFunc = predicate,
            QuantifiedArgs = Args
        ;
            PredOrFunc = function,
            pred_args_to_func_args(Args, QuantifiedArgs, _ReturnValTerm)
        ),
        term.vars_list(QuantifiedArgs, QuantifiedVars0),
        list.sort_and_remove_dups(QuantifiedVars0, QuantifiedVars),

        goal_info_init(Context, GoalInfo),
        HLDS_Goal = scope(exist_quant(QuantifiedVars), HLDS_Goal0) - GoalInfo,
        %
        % We set the lambda nonlocals here to anything that could
        % possibly be nonlocal.  Quantification will reduce this down to
        % the proper set of nonlocal arguments.
        %
        some [!LambdaGoalVars] (
            goal_util.goal_vars(HLDS_Goal, !:LambdaGoalVars),
            svset.delete_list(LambdaVars, !LambdaGoalVars),
            svset.delete_list(QuantifiedVars, !LambdaGoalVars),
            LambdaNonLocals = set.to_sorted_list(!.LambdaGoalVars)
        ),

        LambdaGoal = lambda_goal(LambdaPurity, PredOrFunc, EvalMethod,
            modes_are_ok, LambdaNonLocals, LambdaVars, Modes, Det, HLDS_Goal),
        make_atomic_unification(X, LambdaGoal, Context, MainContext,
            SubContext, UnificationPurity, Goal, !QualInfo)
    ).

    % Partition the lists of arguments and variables into lists
    % of non-output and output arguments and variables.
    %
 :- pred partition_args_and_lambda_vars(
    module_info::in, list(prog_term)::in,
    list(prog_var)::in, list(mer_mode)::in,
    list(prog_term)::out, list(prog_term)::out,
    list(prog_var)::out, list(prog_var)::out) is semidet.

partition_args_and_lambda_vars(_, [], [], [], [], [], [], []).
partition_args_and_lambda_vars(ModuleInfo, [ Arg | Args ],
            [ LambdaVar | LambdaVars ],
            [Mode | Modes], InputArgs, OutputArgs, 
            InputLambdaVars, OutputLambdaVars) :-
        partition_args_and_lambda_vars(ModuleInfo, Args, LambdaVars, Modes,
            InputArgs0, OutputArgs0, InputLambdaVars0, OutputLambdaVars0),
        %
        % Calling mode_is_output/2 directly will cause the compiler to
        % abort if the mode is undefined, so we first check for this.
        % If the mode is undefined, it doesn't really matter which 
        % partitions we place the arguements/lambda vars into because
        % mode analysis will fail anyway.
        %
        ( mode_is_undefined(ModuleInfo, Mode) ->
            InputArgs        = [ Arg | InputArgs0],
            OutputArgs       = OutputArgs0,
            InputLambdaVars  = [ LambdaVar | InputLambdaVars0 ],
            OutputLambdaVars = OutputLambdaVars0
        ;
            ( mode_is_output(ModuleInfo, Mode) ->
                InputArgs        = InputArgs0,
                OutputArgs       = [Arg | OutputArgs0],
                InputLambdaVars  = InputLambdaVars0,
                OutputLambdaVars = [ LambdaVar | OutputLambdaVars0 ]
            ;
                InputArgs        = [ Arg | InputArgs0],
                OutputArgs       = OutputArgs0,
                InputLambdaVars  = [ LambdaVar | InputLambdaVars0 ],
                OutputLambdaVars = OutputLambdaVars0
            )
        ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "superhomogeneous.m".

%-----------------------------------------------------------------------------%
:- end_module superhomogeneous.
%-----------------------------------------------------------------------------%
