%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2014-2021, 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: superhomogeneous.m.
% Main author of the original version of this module: fjh.
% Main author of the current version of this module: zs.
%
% This module performs the conversion of clause bodies
% to superhomogeneous form.
%
% XXX The code in this module should follow a consistent naming convention
% to distinguish
%
% - variables that refer to goals in the term we are parsing from
% - variables that refer to goals in the HLDS we are building.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.superhomogeneous.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.make_hlds.state_var.
:- import_module hlds.make_hlds.superhomogeneous_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % insert_arg_unifications(XVarsArgTerms0, Context, ArgContext, Goal0, Goal,
    %   !SVarState, !UrInfo):
    % insert_arg_unifications_with_contexts(XVarsArgTermsArgNumsContexts0,
    %   Context, Goal0, Goal, !SVarState, !UrInfo):
    %
    % These two predicates both take as input
    %
    % - a list containing variables and the terms to unify them with,
    %   (XVarsArgTerms0 and XVarsArgTermsArgNumsContexts0 respectively),
    % - information about where they came from (Context and ArgContext), and
    % - a goal (Goal0).
    %
    % They reduce the requested unifications to superhomogeneous form,
    % and then insert them in front of Goal0, returning the result as Goal.
    %
    % We never insert unifications of the form X = X.
    %
    % We use these predicates to implement the unifications implicit
    % in both clause heads and in calls to functions and predicates,
    % between the terms that appear there and the corresponding argument
    % variables.
    %
:- pred insert_arg_unifications(list(unify_var_term)::in,
    prog_context::in, arg_context::in, hlds_goal::in, hlds_goal::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.
:- pred insert_arg_unifications_with_contexts(
    list(unify_var_term_num_context)::in,
    prog_context::in, hlds_goal::in, hlds_goal::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

    % unravel_unification(XTerm, YTerm, Context, MainContext, SubContext,
    %   Purity, Goal, !SVarState, !UrInfo):
    %
    % Reduce the explicit unification XTerm = YTerm, whose context and purity
    % are given by Context, MainContext, SubContext and Purity,
    % to superhomogeneous form, and return it as Goal.
    %
:- pred unravel_unification(prog_term::in, prog_term::in, prog_context::in,
    unify_main_context::in, list(unify_sub_context)::in, purity::in,
    hlds_goal::out, svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.from_ground_term_util.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_goal.
:- import_module hlds.make_hlds.field_access.
:- import_module hlds.make_hlds.goal_expr_to_goal.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.make_hlds.superhomogeneous_lambda.
:- import_module libs.
:- import_module libs.globals.  % for get_maybe_from_ground_term_threshold
:- import_module libs.options.  % for warn_suspected_occurs_check_failure
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_goal.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_type_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module integer.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type ancestor_var_map == map(prog_var, prog_context).

%---------------------------------------------------------------------------%

insert_arg_unifications(XVarsArgTerms0, Context, ArgContext, Goal0, Goal,
        !SVarState, !UrInfo) :-
    substitute_state_var_mappings_unify_var_term(XVarsArgTerms0, XVarsArgTerms,
        !SVarState, !UrInfo),
    map.init(AncestorVarMap),
    do_arg_unifications(XVarsArgTerms, Context, ArgContext,
        construct_bottom_up, 1, AncestorVarMap, Expansions,
        !SVarState, !UrInfo),
    Goal0 = hlds_goal(_, GoalInfo0),
    insert_expansions_before_goal_top_not_fgti(!.UrInfo, GoalInfo0, Expansions,
        Goal0, Goal).

insert_arg_unifications_with_contexts(XVarsArgTermsArgNumsContexts0,
        Context, Goal0, Goal, !SVarState, !UrInfo) :-
    substitute_state_var_mappings_unify_var_term_num_context(
        XVarsArgTermsArgNumsContexts0, XVarsArgTermsArgNumsContexts,
        !SVarState, !UrInfo),
    map.init(AncestorVarMap),
    do_arg_unifications_with_contexts(XVarsArgTermsArgNumsContexts,
        Context, construct_bottom_up, AncestorVarMap, Expansions,
        !SVarState, !UrInfo),
    Goal0 = hlds_goal(_, GoalInfo0),
    insert_expansions_before_goal_top_not_fgti(!.UrInfo, GoalInfo0, Expansions,
        Goal0, Goal).

%---------------------------------------------------------------------------%

:- pred substitute_state_var_mappings_unify_var_term(
    list(unify_var_term)::in, list(unify_var_term)::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

substitute_state_var_mappings_unify_var_term([], [], !SVarState, !UrInfo).
substitute_state_var_mappings_unify_var_term([UVT0 | UVTs0], [UVT | UVTs],
        !SVarState, !UrInfo) :-
    UVT0 = unify_var_term(Var, Arg0),
    replace_any_dot_colon_state_var_in_term(Arg0, Arg, !SVarState, !UrInfo),
    UVT = unify_var_term(Var, Arg),
    substitute_state_var_mappings_unify_var_term(UVTs0, UVTs,
        !SVarState, !UrInfo).

:- pred substitute_state_var_mappings_unify_var_term_num_context(
    list(unify_var_term_num_context)::in,
        list(unify_var_term_num_context)::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

substitute_state_var_mappings_unify_var_term_num_context([], [],
        !SVarState, !UrInfo).
substitute_state_var_mappings_unify_var_term_num_context(
        [UVTNC0 | UVTNCs0], [UVTNC | UVTNCs], !SVarState, !UrInfo) :-
    UVTNC0 = unify_var_term_num_context(Var, Arg0, ArgNum, ArgContext),
    replace_any_dot_colon_state_var_in_term(Arg0, Arg, !SVarState, !UrInfo),
    UVTNC = unify_var_term_num_context(Var, Arg, ArgNum, ArgContext),
    substitute_state_var_mappings_unify_var_term_num_context(UVTNCs0, UVTNCs,
        !SVarState, !UrInfo).

%---------------------------------------------------------------------------%

:- pred do_arg_unifications(list(unify_var_term)::in,
    prog_context::in, arg_context::in,
    goal_order::in, int::in, ancestor_var_map::in, list(expansion)::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

do_arg_unifications([], _Context, _ArgContext, _Order, _ArgNum,
        _AncestorVarMap, [], !SVarState, !UrInfo).
do_arg_unifications([unify_var_term(XVar, YTerm) | XVarsYTerms],
        Context, ArgContext, Order, ArgNum,
        AncestorVarMap, [Expansion | Expansions], !SVarState, !UrInfo) :-
    do_arg_unification(XVar, YTerm, Context, ArgContext, Order, ArgNum,
        AncestorVarMap, Expansion, !SVarState, !UrInfo),
    do_arg_unifications(XVarsYTerms, Context, ArgContext, Order, ArgNum + 1,
        AncestorVarMap, Expansions, !SVarState, !UrInfo).

:- pred do_arg_unifications_with_contexts(list(unify_var_term_num_context)::in,
    prog_context::in, goal_order::in, ancestor_var_map::in,
    list(expansion)::out, svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

do_arg_unifications_with_contexts([], _Context, _Order, _AncestorVarMap, [],
        !SVarState, !UrInfo).
do_arg_unifications_with_contexts(
        [HeadXVarYTermArgContext | TailXVarsYTermsArgContexts],
        Context, Order, AncestorVarMap, Expansions, !SVarState, !UrInfo) :-
    HeadXVarYTermArgContext = unify_var_term_num_context(HeadXVar,
        HeadYTerm, HeadArgNumber, HeadArgContext),
    do_arg_unification(HeadXVar, HeadYTerm, Context, HeadArgContext, Order,
        HeadArgNumber, AncestorVarMap, HeadExpansion, !SVarState, !UrInfo),
    do_arg_unifications_with_contexts(TailXVarsYTermsArgContexts,
        Context, Order, AncestorVarMap, TailExpansions, !SVarState, !UrInfo),
    Expansions = [HeadExpansion | TailExpansions].

:- pred do_arg_unifications_with_fresh_vars(list(prog_term)::in,
    prog_context::in, arg_context::in, goal_order::in, int::in,
    list(prog_var)::in, ancestor_var_map::in,
    list(prog_var)::out, list(expansion)::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

do_arg_unifications_with_fresh_vars([], _Context, _ArgContext,
        _Order, _ArgNum, _SeenXVars, _AncestorVarMap,
        [], [], !SVarState, !UrInfo).
do_arg_unifications_with_fresh_vars([YTerm | YTerms], Context, ArgContext,
        Order, ArgNum, !.SeenXVars, AncestorVarMap,
        [XVar | XVars], [Expansion | Expansions], !SVarState, !UrInfo) :-
    make_fresh_arg_var_no_svar(YTerm, XVar, !.SeenXVars, !UrInfo),
    !:SeenXVars = [XVar | !.SeenXVars],
    do_arg_unification(XVar, YTerm, Context, ArgContext, Order,
        ArgNum, AncestorVarMap, Expansion,
        !SVarState, !UrInfo),
    do_arg_unifications_with_fresh_vars(YTerms, Context, ArgContext, Order,
        ArgNum + 1, !.SeenXVars, AncestorVarMap, XVars, Expansions,
        !SVarState, !UrInfo).

:- pred do_arg_unification(prog_var::in, prog_term::in,
    prog_context::in, arg_context::in,
    goal_order::in, int::in, ancestor_var_map::in, expansion::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

do_arg_unification(XVar, YTerm, Context, ArgContext, Order, ArgNum,
        AncestorVarMap, Expansion, !SVarState, !UrInfo) :-
    % It is the caller's job to make sure that if needed, then both
    % XVar and the top level of YTerm have already been through
    % state var mapping expansion.
    perform_occurs_check(AncestorVarMap, XVar, !UrInfo),
    (
        YTerm = term.variable(YVar, YVarContext),
        ( if XVar = YVar then
            % Skip unifications of the form `XVar = XVar'.
            GoalCord = cord.init
        else
            arg_context_to_unify_context(ArgContext, ArgNum,
                MainContext, SubContext),
            create_atomic_complicated_unification(XVar, rhs_var(YVar),
                YVarContext, MainContext, SubContext, purity_pure, Goal),
            GoalCord = cord.singleton(Goal)
        ),
        Expansion = expansion(not_fgti, GoalCord)
    ;
        YTerm = term.functor(YFunctor, YArgTerms, YFunctorContext),
        arg_context_to_unify_context(ArgContext, ArgNum,
            MainContext, SubContext),
        unravel_var_functor_unification(XVar, YFunctor, YArgTerms,
            YFunctorContext, Context, MainContext, SubContext, purity_pure,
            Order, AncestorVarMap, Expansion, !SVarState, !UrInfo)
    ).

%---------------------------------------------------------------------------%

unravel_unification(XTerm, YTerm, Context, MainContext, SubContext, Purity,
        Goal, !SVarState, !UrInfo) :-
    (
        Purity = purity_pure,
        Order = deconstruct_top_down
    ;
        ( Purity = purity_semipure
        ; Purity = purity_impure
        ),
        Order = construct_bottom_up
    ),
    do_unravel_unification(XTerm, YTerm, Context, MainContext, SubContext,
        Purity, Order, Expansion, !SVarState, !UrInfo),
    goal_info_init(Context, GoalInfo),
    expansion_to_goal_wrap_if_fgti(!.UrInfo, GoalInfo, Expansion, Goal).

%---------------------------------------------------------------------------%
%
% Unravel unifications of the form XTerm = YTerm.
%

    % As of 2025 aug 27, this predicate is called from only one call site,
    % in unravel_unification.
    %
:- pred do_unravel_unification(prog_term::in, prog_term::in,
    prog_context::in, unify_main_context::in, list(unify_sub_context)::in,
    purity::in, goal_order::in, expansion::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

do_unravel_unification(XTerm0, YTerm0, Context, MainContext, SubContext,
        Purity, Order, Expansion, !SVarState, !UrInfo) :-
    replace_any_dot_colon_state_var_in_term(XTerm0, XTerm,
        !SVarState, !UrInfo),
    replace_any_dot_colon_state_var_in_term(YTerm0, YTerm,
        !SVarState, !UrInfo),
    classify_unravel_unification(XTerm, YTerm,
        Context, MainContext, SubContext, Purity, Order,
        map.init, Expansion, !SVarState, !UrInfo).

    % As of 2025 aug 27, this predicate is called from only one call site,
    % in do_unravel_unification.
    %
:- pred classify_unravel_unification(prog_term::in, prog_term::in,
    prog_context::in, unify_main_context::in, list(unify_sub_context)::in,
    purity::in, goal_order::in, ancestor_var_map::in, expansion::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

classify_unravel_unification(XTerm, YTerm, Context, MainContext, SubContext,
        Purity, Order, AncestorVarMap, Expansion, !SVarState, !UrInfo) :-
    (
        XTerm = term.variable(XVar, _),
        YTerm = term.variable(YVar, _),
        % `X = Y' needs no unravelling.
        create_atomic_complicated_unification(XVar, rhs_var(YVar),
            Context, MainContext, SubContext, Purity, Goal),
        Expansion = expansion(not_fgti, cord.singleton(Goal))
    ;
        XTerm = term.variable(XVar, _),
        YTerm = term.functor(YFunctor, YArgTerms, YFunctorContext),
        unravel_var_functor_unification(XVar, YFunctor, YArgTerms,
            YFunctorContext, Context, MainContext, SubContext,
            Purity, Order, AncestorVarMap, Expansion, !SVarState, !UrInfo)
    ;
        XTerm = term.functor(XFunctor, XArgTerms, XFunctorContext),
        YTerm = term.variable(YVar, _),
        unravel_var_functor_unification(YVar, XFunctor, XArgTerms,
            XFunctorContext, Context, MainContext, SubContext,
            Purity, Order, AncestorVarMap, Expansion, !SVarState, !UrInfo)
    ;
        XTerm = term.functor(XFunctor, XArgTerms, XFunctorContext),
        YTerm = term.functor(YFunctor, YArgTerms, YFunctorContext),
        % If we find a unification of the form `f1(...) = f2(...)',
        % then we replace it with `Tmp = f1(...), Tmp = f2(...)',
        % and then process it according to the rules above.
        % Note that we can't simplify it yet, e.g. by pairwise unifying
        % the args of XTerm and YTerm, because we might simplify away
        % type errors.
        create_new_unravel_var(TmpVar, !UrInfo),
        % TmpVar cannot occur in either XTerm or YTerm, so adding it
        % to AncestorVarMap would not result in any hits, and would only
        % slow down lookups.
        unravel_var_functor_unification(TmpVar, XFunctor, XArgTerms,
            XFunctorContext, Context, MainContext, SubContext,
            Purity, Order, AncestorVarMap, ExpansionX, !SVarState, !UrInfo),
        unravel_var_functor_unification(TmpVar, YFunctor, YArgTerms,
            YFunctorContext, Context, MainContext, SubContext,
            Purity, Order, AncestorVarMap, ExpansionY, !SVarState, !UrInfo),
        goal_info_init(Context, GoalInfo),
        expansion_to_goal_cord_wrap_if_fgti(!.UrInfo, GoalInfo, ExpansionX,
            MaybeWrappedGoalCordX),
        expansion_to_goal_cord_wrap_if_fgti(!.UrInfo, GoalInfo, ExpansionY,
            MaybeWrappedGoalCordY),
        GoalCord = MaybeWrappedGoalCordX ++ MaybeWrappedGoalCordY,
        Expansion = expansion(not_fgti, GoalCord)
    ).

%---------------------------------------------------------------------------%
%
% Unravel unifications of the form XVar = YTerm.
%

    % As of 2025 aug 27, this predicate is called onlt from call sites
    % in maybe_unravel_special_var_functor_unification.
    %
:- pred do_unravel_var_unification(prog_var::in, prog_term::in,
    prog_context::in, unify_main_context::in, list(unify_sub_context)::in,
    purity::in, goal_order::in, expansion::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

do_unravel_var_unification(XVar, YTerm0, Context, MainContext, SubContext,
        Purity, Order, Expansion, !SVarState, !UrInfo) :-
    replace_any_dot_colon_state_var_in_term(YTerm0, YTerm,
        !SVarState, !UrInfo),
    classify_unravel_var_unification(XVar, YTerm,
        Context, MainContext, SubContext, Purity, Order,
        map.init, Expansion, !SVarState, !UrInfo).

    % As of 2025 aug 27, this predicate is called from
    % do_unravel_var_unification and from
    % maybe_unravel_special_var_functor_unification.
    %
:- pred classify_unravel_var_unification(prog_var::in, prog_term::in,
    prog_context::in, unify_main_context::in, list(unify_sub_context)::in,
    purity::in, goal_order::in, ancestor_var_map::in, expansion::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

classify_unravel_var_unification(XVar, YTerm, Context, MainContext, SubContext,
        Purity, Order, AncestorVarMap, Expansion, !SVarState, !UrInfo) :-
    (
        % `X = Y' needs no unravelling.
        YTerm = term.variable(YVar, _),
        perform_occurs_check(AncestorVarMap, YVar, !UrInfo),
        create_atomic_complicated_unification(XVar, rhs_var(YVar),
            Context, MainContext, SubContext, Purity, Goal),
        Expansion = expansion(not_fgti, cord.singleton(Goal))
    ;
        YTerm = term.functor(YFunctor, YArgTerms, YFunctorContext),
        unravel_var_functor_unification(XVar, YFunctor, YArgTerms,
            YFunctorContext, Context, MainContext, SubContext,
            Purity, Order, AncestorVarMap, Expansion, !SVarState, !UrInfo)
    ).

%---------------------------------------------------------------------------%

    % Given an unification of the form
    %   X = f(ArgTerm1, ArgTerm2, ArgTerm3)
    % we replace it with
    %   X = f(NewVar1, NewVar2, NewVar3),
    %   NewVar1 = ArgTerm1,
    %   NewVar2 = ArgTerm2,
    %   NewVar3 = ArgTerm3.
    % In the trivial case `X = c', no unravelling occurs.
    %
:- pred unravel_var_functor_unification(prog_var::in, term.const::in,
    list(prog_term)::in, term.context::in,
    prog_context::in, unify_main_context::in, list(unify_sub_context)::in,
    purity::in, goal_order::in, ancestor_var_map::in, expansion::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

unravel_var_functor_unification(XVar, YFunctor, YArgTerms0, YFunctorContext,
        Context, MainContext, SubContext,
        Purity, Order, AncestorVarMap, Expansion, !SVarState, !UrInfo) :-
    replace_any_dot_colon_state_var_in_terms(YArgTerms0, YArgTerms,
        !SVarState, !UrInfo),
    ( if
        YFunctor = term.atom(YAtom),
        maybe_unravel_special_var_functor_unification(XVar, YAtom, YArgTerms,
            YFunctorContext, Context, MainContext, SubContext, Purity,
            Order, ExpansionPrime, !SVarState, !UrInfo)
    then
        Expansion = ExpansionPrime
    else
        % Handle the usual case.
        ( if
            % The condition of this if-then-else is based on the logic of
            % try_parse_sym_name_and_args, but specialized to this location,
            % so that we can do state var expansion only if we need to.
            YFunctor = term.atom(YAtom),
            ( if
                YAtom = ".",
                YArgTerms = [ModuleNameTerm, NameArgsTerm]
            then
                NameArgsTerm = term.functor(term.atom(Name), NameArgTerms, _),
                try_parse_sym_name(ModuleNameTerm, ModuleName),
                FunctorName = qualified(ModuleName, Name),
                % We have done state variable name expansion at the top
                % level of Args, but not at the level of NameArgTerms.
                replace_any_dot_colon_state_var_in_terms(NameArgTerms,
                    MaybeQualifiedYArgTermsPrime, !SVarState, !UrInfo)
            else
                FunctorName = string_to_sym_name_sep(YAtom, "__"),
                MaybeQualifiedYArgTermsPrime = YArgTerms
            )
        then
            MaybeQualifiedYArgTerms = MaybeQualifiedYArgTermsPrime,
            list.length(MaybeQualifiedYArgTerms, Arity),
            ConsId = du_data_ctor(du_ctor(FunctorName, Arity,
                cons_id_dummy_type_ctor))
        else
            % If YFunctor is a numeric or string constant, it *should*
            % have no arguments. If it nevertheless does, we still record
            % its arguments, and let the error be caught later during
            % typechecking.
            parse_ordinary_cons_id(YFunctor, YArgTerms, YFunctorContext,
                ConsId, !UrInfo),
            MaybeQualifiedYArgTerms = YArgTerms
        ),
        build_var_cons_id_unification(XVar, ConsId, MaybeQualifiedYArgTerms,
            YFunctorContext, Context, MainContext, SubContext, Purity,
            AncestorVarMap, Expansion, !SVarState, !UrInfo)
    ).

    % See whether YAtom indicates a term with special syntax.
    %
    % XXX We could do better on the error messages for lambda expressions
    % and field extraction and update expressions.
    %
:- pred maybe_unravel_special_var_functor_unification(prog_var::in,
    string::in, list(prog_term)::in, term.context::in,
    prog_context::in, unify_main_context::in, list(unify_sub_context)::in,
    purity::in, goal_order::in, expansion::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is semidet.

maybe_unravel_special_var_functor_unification(XVar, YAtom, YArgTerms,
        YFunctorContext, Context, MainContext, SubContext, Purity, Order,
        Expansion, !SVarState, !UrInfo) :-
    % Switch on YAtom.
    % We cannot require each of the switch arms to be det. One reason is that
    % some of the keywords we are looking for contain just one character,
    % and *could* be a reference to a character constant (since the lexer
    % doesn't distinguish between seeing e.g. ':' with and without the quotes).
    % However, each arm should wrap a require_det scope around all the goals
    % that we execute when we have decided that the term we are parsing *is*
    % in fact supposed to be the construct we are looking for.
    (
        % Handle explicit type qualification.
        ( YAtom = "with_type"
        ; YAtom = ":"
        ),
        ( if YArgTerms = [RValTerm, DeclTypeTerm0] then
            require_det (
                % DeclType0 is a prog_term, but it is really a type,
                % so we coerce it to a generic term before parsing it.
                term.coerce(DeclTypeTerm0, DeclTypeTerm1),
                ContextPieces =
                    cord.singleton(words("In explicit type qualification:")),
                VarSet0 = !.UrInfo ^ ui_varset,
                varset.coerce(VarSet0, GenericVarSet),
                parse_type(no_allow_ho_inst_info(wnhii_type_qual),
                    GenericVarSet, ContextPieces, DeclTypeTerm1,
                    DeclTypeResult),
                (
                    DeclTypeResult = ok1(DeclType),
                    varset.coerce(VarSet0, DeclVarSet),
                    QualInfo0 = !.UrInfo ^ ui_qual_info,
                    process_type_qualification(XVar, DeclType, DeclVarSet,
                        YFunctorContext, QualInfo0, QualInfo,
                        [], TypeQualSpecs),
                    !UrInfo ^ ui_qual_info := QualInfo,
                    % Note that most of time, TypeQualSpecs will be [].
                    add_unravel_specs(TypeQualSpecs, !UrInfo)
                ;
                    DeclTypeResult = error1(DeclTypeSpecs),
                    % The varset is a prog_varset even though it contains
                    % the names of type variables in ErrorTerm, which is
                    % a generic term.
                    add_unravel_specs(DeclTypeSpecs, !UrInfo)
                ),
                do_unravel_var_unification(XVar, RValTerm,
                    Context, MainContext, SubContext, Purity, Order, Expansion,
                    !SVarState, !UrInfo)
            )
        else if YAtom = ":", YArgTerms = [] then
            % This may be the character ':'.
            fail
        else
            % The code below is disabled, as per the discussion on m-rev
            % that started on 2016 may 5.
            fail
%           Pieces = [words("Error: the type qualification operator"),
%               quote(YAtom), words("can be used only in expressions"),
%               words("of the form"), quote("<term> " ++ YAtom ++ " <type>"),
%               suffix("."), nl],
%           Msg = simple_msg(YFunctorContext, [always(Pieces)]),
%           Spec = error_spec(severity_error, phase_pt2h, [Msg]),
%           !:Specs = [Spec | !.Specs],
%           qual_info_set_found_syntax_error(yes, !QualInfo),
%           Expansion = expansion(not_fgti, cord.empty)
        )
    ;
        % Handle unification expressions.
        YAtom = "@",
        (
            YArgTerms = [],
            % This may be the character '@'.
            fail
        ;
            YArgTerms = [LVal, RVal],
            require_det (
                do_unravel_var_unification(XVar, LVal, Context,
                    MainContext, SubContext, Purity, Order, ExpansionL,
                    !SVarState, !UrInfo),
                do_unravel_var_unification(XVar, RVal, Context,
                    MainContext, SubContext, Purity, Order, ExpansionR,
                    !SVarState, !UrInfo),
                ExpansionL = expansion(_, GoalCordL),
                ExpansionR = expansion(_, GoalCordR),
                Expansion = expansion(not_fgti, GoalCordL ++ GoalCordR)
            )
        ;
            ( YArgTerms = [_]
            ; YArgTerms = [_, _, _ | _]
            ),
            % The code below is disabled, as per the discussion on m-rev
            % that started on 2016 may 5.
            fail
%           Pieces = [words("Error: the unification expression operator"),
%               quote(YAtom), words("can be used only in expressions"),
%               words("of the form"), quote("<term> " ++ YAtom ++ " <term>"),
%               suffix("."), nl],
%           Msg = simple_msg(YFunctorContext, [always(Pieces)]),
%           Spec = error_spec(severity_error, phase_pt2h, [Msg]),
%           !:Specs = [Spec | !.Specs],
%           qual_info_set_found_syntax_error(yes, !QualInfo),
%           Expansion = expansion(not_fgti, cord.empty)
        )
    ;
        % Handle coerce expressions.
        YAtom = "coerce",
        YArgTerms = [RValTerm0],
        require_det (
            (
                RValTerm0 = term.variable(RValTermVar, _),
                RValGoalCord = cord.empty
            ;
                RValTerm0 = term.functor(_, _, _),
                replace_any_dot_colon_state_var_in_term(RValTerm0, RValTerm,
                    !SVarState, !UrInfo),
                make_fresh_arg_var_no_svar(RValTerm0, RValTermVar, [],
                    !UrInfo),
                do_unravel_var_unification(RValTermVar, RValTerm, Context,
                    MainContext, SubContext, Purity, Order, RValTermExpansion,
                    !SVarState, !UrInfo),
                RValTermExpansion = expansion(_, RValGoalCord)
            ),
            CoerceGoalExpr = generic_call(cast(subtype_coerce),
                [RValTermVar, XVar], [in_mode, out_mode],
                arg_reg_types_unset, detism_det),
            goal_info_init(Context, CoerceGoalInfo),
            CoerceGoal = hlds_goal(CoerceGoalExpr, CoerceGoalInfo),
            CoerceGoalCord = cord.singleton(CoerceGoal),
            Expansion = expansion(not_fgti, RValGoalCord ++ CoerceGoalCord)
        )
    ;
        % Handle if-then-else expressions.
        (
            YAtom = "else",
            YArgTerms = [CondThenTerm0, ElseTerm0],
            CondThenTerm0 = term.functor(term.atom("if"),
                [term.functor(term.atom("then"), [CondTerm0, ThenTerm0], _)],
                    _)
        ;
            YAtom = ";",
            YArgTerms = [CondThenTerm0, ElseTerm0],
            CondThenTerm0 = term.functor(term.atom("->"),
                [CondTerm0, ThenTerm0], _)
        ),

        require_det (
            term.coerce(CondTerm0, CondTerm),
            ContextPieces = cord.init,
            VarSet0 = !.UrInfo ^ ui_varset,
            parse_some_vars_goal(CondTerm, ContextPieces, MaybeVarsCond,
                VarSet0, VarSet),
            !UrInfo ^ ui_varset := VarSet,
            (
                MaybeVarsCond =
                    ok4(Vars, StateVars, CondParseTree, CondWarningSpecs),
                add_unravel_specs(CondWarningSpecs, !UrInfo),
                BeforeSVarState = !.SVarState,
                svar_prepare_for_local_state_vars(Context, StateVars,
                    BeforeSVarState, BeforeInsideSVarState, !UrInfo),

                map.init(EmptyRenaming),
                transform_parse_tree_goal_to_hlds(loc_inside_atomic_goal,
                    EmptyRenaming, CondParseTree, CondGoal,
                    BeforeInsideSVarState, AfterCondInsideSVarState, !UrInfo),

                replace_any_dot_colon_state_var_in_term(ThenTerm0, ThenTerm,
                    AfterCondInsideSVarState, AfterThenInsideSVarState0,
                    !UrInfo),
                map.init(AncestorVarMap),
                classify_unravel_var_unification(XVar, ThenTerm,
                    Context, MainContext, SubContext,
                    Purity, Order, AncestorVarMap, ThenExpansion,
                    AfterThenInsideSVarState0, AfterThenInsideSVarState,
                    !UrInfo),
                goal_info_init(get_term_context(ThenTerm), ThenGoalInfo),
                expansion_to_goal_wrap_if_fgti(!.UrInfo, ThenGoalInfo,
                    ThenExpansion, ThenGoal0),

                svar_finish_local_state_vars(!.UrInfo, StateVars,
                    BeforeSVarState, AfterThenInsideSVarState,
                    AfterThenSVarState),

                replace_any_dot_colon_state_var_in_term(ElseTerm0, ElseTerm,
                    BeforeSVarState, AfterElseSVarState0, !UrInfo),
                classify_unravel_var_unification(XVar, ElseTerm,
                    Context, MainContext, SubContext,
                    Purity, Order, AncestorVarMap, ElseExpansion,
                    AfterElseSVarState0, AfterElseSVarState, !UrInfo),
                goal_info_init(get_term_context(ElseTerm), ElseGoalInfo),
                expansion_to_goal_wrap_if_fgti(!.UrInfo, ElseGoalInfo,
                    ElseExpansion, ElseGoal0),

                svar_finish_if_then_else(loc_inside_atomic_goal, Context,
                    StateVars, ThenGoal0, ThenGoal, ElseGoal0, ElseGoal,
                    BeforeSVarState, AfterCondInsideSVarState,
                    AfterThenSVarState, AfterElseSVarState,
                    AfterITESVarState, !UrInfo),
                !:SVarState = AfterITESVarState,

                GoalExpr = if_then_else(StateVars ++ Vars,
                    CondGoal, ThenGoal, ElseGoal),
                goal_info_init(Context, GoalInfo),
                Goal = hlds_goal(GoalExpr, GoalInfo),
                Expansion = expansion(not_fgti, cord.singleton(Goal))
            ;
                MaybeVarsCond = error4(VarsCondSpecs),
                add_unravel_specs(VarsCondSpecs, !UrInfo),
                Expansion = expansion(not_fgti,
                    cord.singleton(true_goal_with_context(Context)))
            )
        )
    ;
        % Handle field extraction expressions.
        YAtom = "^",
        (
            YArgTerms = [],
            % This may be the character '^'.
            fail
        ;
            YArgTerms = [InputTerm0, FieldNameTerm],
            FieldNameContextPieces = [words("On the right hand side"),
                words("of the"), quote("^"), words("operator"),
                words("in a field selection expression:")],
            VarSet0 = !.UrInfo ^ ui_varset,
            parse_field_list(FieldNameTerm, VarSet0,
                FieldNameContextPieces, MaybeFieldNames),
            (
                MaybeFieldNames = ok1(FieldNames),
                require_det (
                    replace_any_dot_colon_state_var_in_term(InputTerm0,
                        InputTerm, !SVarState, !UrInfo),
                    make_fresh_arg_var_no_svar(InputTerm, InputTermVar, [],
                        !UrInfo),
                    expand_get_field_function_call(Context, MainContext,
                        SubContext, FieldNames, XVar, InputTermVar, Purity,
                        Functor, _, GetGoal, !SVarState, !UrInfo),

                    ArgContext = ac_functor(Functor, MainContext, SubContext),
                    map.init(AncestorVarMap),
                    do_arg_unification(InputTermVar, InputTerm,
                        YFunctorContext, ArgContext, Order,
                        1, AncestorVarMap, InputArgExpansion,
                        !SVarState, !UrInfo),
                    goal_info_init(Context, GoalInfo),
                    insert_expansion_before_goal_top_not_fgti(!.UrInfo,
                        GoalInfo, InputArgExpansion, GetGoal, Goal),
                    Expansion = expansion(not_fgti, cord.singleton(Goal))
                )
            ;
                MaybeFieldNames = error1(_FieldNamesSpecs),
                % The code below is disabled, as per the discussion
                % on m-rev that started on 2016 may 5.
                fail
%               !:Specs = FieldNamesSpecs ++ !.Specs,
%               qual_info_set_found_syntax_error(yes, !QualInfo),
%               Expansion = expansion(not_fgti, cord.empty)
            )
        ;
            ( YArgTerms = [_]
            ; YArgTerms = [_, _, _ | _]
            ),
            % The code below is disabled, as per the discussion on m-rev
            % that started on 2016 may 5.
            fail
%           Pieces = [words("Error: the field access operator"), quote(YAtom),
%               words("can be used only in expressions of the form"),
%               quote("<term> ^ <fieldname>"), suffix("."), nl],
%           Msg = simple_msg(YFunctorContext, [always(Pieces)]),
%           Spec = error_spec(severity_error, phase_pt2h, [Msg]),
%           !:Specs = [Spec | !.Specs],
%           qual_info_set_found_syntax_error(yes, !QualInfo),
%           Expansion = expansion(not_fgti, cord.empty)
        )
    ;
        % Handle field update expressions.
        YAtom = ":=",
        ( if
            YArgTerms = [FieldDescrTerm, FieldValueTerm0],
            FieldDescrTerm = term.functor(term.atom("^"),
                [InputTerm0, FieldNameTerm], _)
        then
            FieldNameContextPieces = [words("On the right hand side"),
                words("of the"), quote("^"), words("operator"),
                words("in a field update expression:")],
            VarSet0 = !.UrInfo ^ ui_varset,
            parse_field_list(FieldNameTerm, VarSet0,
                FieldNameContextPieces, MaybeFieldNames),
            (
                MaybeFieldNames = ok1(FieldNames),
                require_det (
                    replace_any_dot_colon_state_var_in_term(InputTerm0,
                        InputTerm, !SVarState, !UrInfo),
                    make_fresh_arg_var_no_svar(InputTerm, InputTermVar, [],
                        !UrInfo),
                    replace_any_dot_colon_state_var_in_term(FieldValueTerm0,
                        FieldValueTerm, !SVarState, !UrInfo),
                    make_fresh_arg_var_no_svar(FieldValueTerm, FieldValueVar,
                        [InputTermVar], !UrInfo),

                    expand_set_field_function_call(Context, MainContext,
                        SubContext, FieldNames, FieldValueVar,
                        InputTermVar, XVar,
                        Functor, InnerFunctor - FieldSubContext, SetGoal,
                        !SVarState, !UrInfo),

                    TermArgNumber = 1,
                    TermArgContext = ac_functor(Functor,
                        MainContext, SubContext),
                    InputVTNC = unify_var_term_num_context(InputTermVar,
                        InputTerm, TermArgNumber, TermArgContext),

                    FieldArgNumber = 2,
                    FieldArgContext = ac_functor(InnerFunctor, MainContext,
                        FieldSubContext),
                    FieldVTNC = unify_var_term_num_context(FieldValueVar,
                        FieldValueTerm, FieldArgNumber, FieldArgContext),

                    map.init(AncestorVarMap),
                    do_arg_unifications_with_contexts([InputVTNC, FieldVTNC],
                        Context, Order, AncestorVarMap,
                        InputFieldArgExpansions, !SVarState, !UrInfo),

                    goal_info_init(Context, GoalInfo),
                    insert_expansions_before_goal_top_not_fgti(!.UrInfo,
                        GoalInfo, InputFieldArgExpansions, SetGoal, Goal),
                    Expansion = expansion(not_fgti, cord.singleton(Goal))
                )
            ;
                MaybeFieldNames = error1(_FieldNamesSpecs),
                % The code below is disabled, as per the discussion
                % on m-rev that started on 2016 may 5.
                fail
%               !:Specs = FieldNamesSpecs ++ !.Specs,
%               qual_info_set_found_syntax_error(yes, !QualInfo),
%               Expansion = expansion(not_fgti, cord.empty)
            )
        else
            % The code below is disabled, as per the discussion on m-rev
            % that started on 2016 may 5.
            fail
%           Pieces = [words("Error: the field update operator"), quote(YAtom),
%               words("can be used only in expressions of the form"),
%               quote("<term> ^ <fieldname> := <newfieldvalueterm>"),
%               suffix("."), nl],
%           Msg = simple_msg(YFunctorContext, [always(Pieces)]),
%           Spec = error_spec(severity_error, phase_pt2h, [Msg]),
%           !:Specs = [Spec | !.Specs],
%           qual_info_set_found_syntax_error(yes, !QualInfo),
%           Expansion = expansion(not_fgti, cord.empty)
        )
    ;
        (
            YAtom = ":-",
            LambdaBodyKind = lambda_body_ordinary
        ;
            YAtom = "-->",
            LambdaBodyKind = lambda_body_dcg
        ),
        % A lambda expression with a body goal. It may or may not have purity
        % marker.
        ( if YArgTerms = [PurityPFArgsDetTerm, BodyGoalTerm] then
            require_det(
                parse_lambda_expr(XVar, Purity,
                    Context, MainContext, SubContext,
                    PurityPFArgsDetTerm, yes({LambdaBodyKind, BodyGoalTerm}),
                    Expansion, !.SVarState, !UrInfo)
            )
        else
            HeadForm = "<lambda expression head> ",
            BodyForm = " <lambda expression body>",
            Form = HeadForm ++ YAtom ++ BodyForm,
            Pieces =
                [words("Error: the clause neck operator"), quote(YAtom)] ++
                color_as_incorrect([words("can be used only"),
                    words("in expressions of the form")]) ++
                color_as_correct([quote(Form), suffix(".")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_pt2h,
                YFunctorContext, Pieces),
            add_unravel_spec(Spec, !UrInfo),
            record_unravel_found_syntax_error(!UrInfo),
            Expansion = expansion(not_fgti, cord.empty)
        )
%   ;
%       ( YAtom = "impure"
%       ; YAtom = "semipure"
%       ),
%       % This could be a lambda expression without a body goal
%       % but with a purity marker. However, since it could also be
%       % a marker in front of an ordinary, non-lambda unification,
%       % we cannot insist on parsing it as a language expression.
    ;
        YAtom = "is",
        % A lambda expression without a body goal or a purity marker,
        % but with a declared determinism.
        require_det (
            YTerm = term.functor(term.atom(YAtom), YArgTerms, YFunctorContext),
            parse_lambda_expr(XVar, Purity, Context, MainContext, SubContext,
                YTerm, no, Expansion, !.SVarState, !UrInfo)
        )
    ;
        YAtom = "=",
        % A lambda expression without a body goal or a purity marker,
        % and without a declared determinism. This can happen only if
        % the lambda expression is a function, in which case its top functor
        % will be "=", and the top functor of the left operand of the "="
        % will be "func" or "any_func". (If it isn't, then we are looking at
        % a plain old unification that does NOT involve a lambda expression.)
        ( if
            YArgTerms = [FuncArgsTerm, _ReturnArgModeTerm],
            FuncArgsTerm = term.functor(term.atom(FuncTermFunctor), _, _),
            ( FuncTermFunctor = "func"
            ; FuncTermFunctor = "any_func"
            )
        then
            require_det (
                YTerm = term.functor(term.atom(YAtom), YArgTerms,
                    YFunctorContext),
                parse_lambda_expr(XVar, Purity,
                    Context, MainContext, SubContext, YTerm, no, Expansion,
                    !.SVarState, !UrInfo)
            )
        else
            fail
        )
    ).

%---------------------------------------------------------------------------%

:- pred parse_ordinary_cons_id(term.const::in, list(prog_term)::in,
    term.context::in, cons_id::out,
    unravel_info::in, unravel_info::out) is det.

parse_ordinary_cons_id(Functor, ArgTerms, Context, ConsId, !UrInfo) :-
    (
        Functor = term.atom(Name),
        list.length(ArgTerms, Arity),
        DuCtor = du_ctor(unqualified(Name), Arity, cons_id_dummy_type_ctor),
        ConsId = du_data_ctor(DuCtor)
    ;
        Functor = term.integer(Base, Integer, Signedness, Size),
%       expect(unify(ArgTerms, []), $pred,
%           "parse_simple_term has given an integer arguments"),
        parse_integer_cons_id(Base, Integer, Signedness, Size, Context,
            MaybeConsId),
        (
            MaybeConsId = ok1(ConsId)
        ;
            MaybeConsId = error1(ConsIdSpecs),
            add_unravel_specs(ConsIdSpecs, !UrInfo),
            % This is a dummy.
            ConsId = some_int_const(int_const(0))
        )
    ;
        Functor = term.float(Float),
%       expect(unify(ArgTerms, []), $pred,
%           "parse_simple_term has given a float arguments"),
        ConsId = float_const(Float)
    ;
        Functor = term.string(String),
%       expect(unify(ArgTerms, []), $pred,
%           "parse_simple_term has given a string arguments"),
        ConsId = string_const(String)
    ;
        Functor = term.implementation_defined(Name),
%       expect(unify(ArgTerms, []), $pred,
%           "parse_simple_term has given an implementation_defined arguments"),
        ( if
            ( Name = "line",   IDCKind = idc_line
            ; Name = "file",   IDCKind = idc_file
            ; Name = "module", IDCKind = idc_module
            ; Name = "pred",   IDCKind = idc_pred
            ; Name = "grade",  IDCKind = idc_grade
            )
        then
            ConsId = impl_defined_const(IDCKind)
        else
            ErrorTerm = functor(Functor, ArgTerms, Context),
            VarSet = !.UrInfo ^ ui_varset,
            TermStr = describe_error_term(VarSet, ErrorTerm),
            Pieces = [words("Error:"),
                words("unexpected implementation defined literal")] ++
                color_as_incorrect([quote(TermStr), suffix(".")]) ++ [nl,
                words("The only valid implementation defined literals are")] ++
                color_as_correct([quote("$line"), suffix(","),
                    quote("$file"), suffix(","),
                    quote("$module"), suffix(","), quote("$pred")]) ++
                [words("and")] ++
                color_as_correct([quote("$grade"), suffix(".")]) ++ [nl],
            Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
            add_unravel_spec(Spec, !UrInfo),
            % This is a dummy.
            ConsId = impl_defined_const(idc_line)
        )
    ).

%---------------------------------------------------------------------------%

:- pred build_var_cons_id_unification(prog_var::in, cons_id::in,
    list(prog_term)::in, term.context::in, prog_context::in,
    unify_main_context::in, list(unify_sub_context)::in, purity::in,
    ancestor_var_map::in, expansion::out, svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

build_var_cons_id_unification(XVar, ConsId, MaybeQualifiedYArgTerms,
        YFunctorContext, Context, MainContext, SubContext, Purity,
        !.AncestorVarMap, Expansion, !SVarState, !UrInfo) :-
    % Our caller has done state variable name expansion
    % at the top level of MaybeQualifiedYArgTerms.
    (
        MaybeQualifiedYArgTerms = [],
        RHS = rhs_functor(ConsId, is_not_exist_constr, []),
        QualInfo0 = !.UrInfo ^ ui_qual_info,
        make_atomic_unification(XVar, RHS, YFunctorContext,
            MainContext, SubContext, Purity, FunctorGoal, QualInfo0, QualInfo),
        !UrInfo ^ ui_qual_info := QualInfo,
        goal_set_purity(Purity, FunctorGoal, Goal),
        Expansion = expansion(fgti_var_size(XVar, 1), cord.singleton(Goal))
    ;
        MaybeQualifiedYArgTerms = [_ | _],
        ArgContext = ac_functor(ConsId, MainContext, SubContext),
        maybe_add_to_ancestor_var_map(!.UrInfo, XVar, ConsId, Context,
            !AncestorVarMap),
        (
            Purity = purity_pure,
            % If we can, we want to add the unifications for the arguments
            % AFTER the unification of the top level function symbol, because
            % otherwise we get efficiency problems during type checking.
            do_arg_unifications_with_fresh_vars(MaybeQualifiedYArgTerms,
                YFunctorContext, ArgContext, deconstruct_top_down, 1,
                [], !.AncestorVarMap, YVars, ArgExpansions,
                !SVarState, !UrInfo),
            RHS = rhs_functor(ConsId, is_not_exist_constr, YVars),
            QualInfo0 = !.UrInfo ^ ui_qual_info,
            make_atomic_unification(XVar, RHS, YFunctorContext,
                MainContext, SubContext, Purity, FunctorGoal,
                QualInfo0, QualInfo),
            !UrInfo ^ ui_qual_info := QualInfo,
            goal_info_init(Context, GoalInfo),
            append_expansions_after_goal_top_ftgi(!.UrInfo, GoalInfo, XVar,
                FunctorGoal, 1, ArgExpansions, Expansion)
        ;
            ( Purity = purity_semipure
            ; Purity = purity_impure
            ),
            % For impure unifications, we need to put the unifications
            % for the arguments BEFORE the unification of the top level
            % function symbol, because mode reordering can't reorder
            % code around that unification.
            do_arg_unifications_with_fresh_vars(MaybeQualifiedYArgTerms,
                YFunctorContext, ArgContext, construct_bottom_up, 1,
                [], !.AncestorVarMap, YVars, ArgExpansions,
                !SVarState, !UrInfo),
            RHS = rhs_functor(ConsId, is_not_exist_constr, YVars),
            QualInfo0 = !.UrInfo ^ ui_qual_info,
            make_atomic_unification(XVar, RHS, YFunctorContext,
                MainContext, SubContext, Purity, FunctorGoal,
                QualInfo0, QualInfo),
            !UrInfo ^ ui_qual_info := QualInfo,
            goal_info_init(Context, GoalInfo),
            insert_expansions_before_goal_top_not_fgti(!.UrInfo, GoalInfo,
                ArgExpansions, FunctorGoal, Goal0),
            goal_set_purity(Purity, Goal0, Goal),
            Expansion = expansion(not_fgti, cord.singleton(Goal))
        )
    ).

    % Add the variable on the left side of the var-functor unification
    % XVar = ConsId(...) to the ancestor var map *if* it can be part of
    % an occurs check violation we want to report.
    %
    % - The occurs check cannot be violated if ConsId is a constant.
    %
    % - If ConsId cannot actually be a data constructor, then this unification
    %   cannot be part of an occurs check violation we want to report.
    %   There are four possibilities:
    %
    %   1 ConsId(...) is a full application of a function, which returns
    %     a piece of data. Even if XVar occurs somewhere inside the
    %     arguments of ConsId, checking whether XVar is equal to the
    %     value computed from it is a perfectly legitimate test.
    %
    %   2 ConsId(...) is a partial application of a function or a predicate,
    %     and XVar's type is the higher order type matching the type
    %     of this closure. This case *would* be a perfectly legitimate
    %     equality test like case 1, were it not for the fact that unification
    %     of higher order values is not allowed (because it is an undecidable
    %     problem). This should therefore be detected as a type error.
    %
    %   3 ConsId(...) is a partial application of a function or a predicate,
    %     and XVar's type is not the higher order type matching the type
    %     of this closure. This is a more straightforward type error.
    %
    %   4 ConsId is not a function or a predicate. This is a straightforward
    %     "unknown function symbol" error.
    %
    %   In case 1, any warning about occurs check violation would be
    %   misleading. In cases 2, 3 and 4, it would be redundant, since those
    %   cases all involve an error which is not really about the occurs check.
    %
:- pred maybe_add_to_ancestor_var_map(unravel_info::in, prog_var::in,
    cons_id::in, prog_context::in,
    ancestor_var_map::in, ancestor_var_map::out) is det.

maybe_add_to_ancestor_var_map(UrInfo, XVar, ConsId, Context,
        !AncestorVarMap) :-
    ( if
        % The only two kinds of cons_ids that may (a) appear in user
        % written code, as opposed to compiler-generated code, and
        % (b) may have nonzero arities, are cons and tuple_cons.
        % However, the cons_ids of tuples are represented by tuple_cons
        % only *after* resolve_unify_functor.m has been run as part of
        % the post_typecheck pass. Until then, they have the form
        % recognized by the second disjunct.
        ConsId = du_data_ctor(DuCtor),
        DuCtor = du_ctor(SymName, Arity, _TypeCtor),
        Arity > 0,
        (
            ModuleInfo = UrInfo ^ ui_module_info,
            module_info_get_cons_table(ModuleInfo, ConsTable),
            is_known_data_cons(ConsTable, DuCtor)
        ;
            SymName = unqualified("{}")
        )
    then
        map.search_insert(XVar, Context, _OldContext, !AncestorVarMap)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred insert_expansion_before_goal_top_not_fgti(unravel_info::in,
    hlds_goal_info::in, expansion::in, hlds_goal::in, hlds_goal::out) is det.

insert_expansion_before_goal_top_not_fgti(UrInfo, GoalInfo, Expansion,
        BaseGoal, Goal) :-
    goal_to_conj_list(BaseGoal, BaseGoals),
    expansion_to_goal_cord_wrap_if_fgti(UrInfo, GoalInfo, Expansion,
        ExpansionGoalCord),
    ExpansionGoals = cord.list(ExpansionGoalCord),
    conj_list_to_goal(ExpansionGoals ++ BaseGoals, GoalInfo, Goal).

:- pred insert_expansions_before_goal_top_not_fgti(unravel_info::in,
    hlds_goal_info::in, list(expansion)::in,
    hlds_goal::in, hlds_goal::out) is det.

insert_expansions_before_goal_top_not_fgti(UrInfo, GoalInfo, Expansions,
        BaseGoal, Goal) :-
    goal_to_conj_list(BaseGoal, BaseGoals),
    list.map(expansion_to_goal_cord_wrap_if_fgti(UrInfo, GoalInfo), Expansions,
        ExpansionGoalCords),
    ExpansionGoals = cord.cord_list_to_list(ExpansionGoalCords),
    conj_list_to_goal(ExpansionGoals ++ BaseGoals, GoalInfo, Goal).

%---------------------------------------------------------------------------%

:- pred append_expansions_after_goal_top_ftgi(unravel_info::in,
    hlds_goal_info::in, prog_var::in, hlds_goal::in, int::in,
    list(expansion)::in, expansion::out) is det.

append_expansions_after_goal_top_ftgi(UrInfo, GoalInfo, TermVar,
        BaseGoal, BaseGoalSize, ArgExpansions, Expansion) :-
    append_expansions_after_goal_top_ftgi_loop(ArgExpansions, yes, AllFGTI,
        BaseGoalSize, TotalSize),
    (
        AllFGTI = no,
        list.map(expansion_to_goal_cord_wrap_if_fgti(UrInfo, GoalInfo),
            ArgExpansions, ArgGoalCords),
        ArgGoalsCord = cord.cord_list_to_cord(ArgGoalCords),
        % XXX If BaseGoal can be a plain_conj, then we should expand it here.
        GoalCord = cord.cons(BaseGoal, ArgGoalsCord),
        Expansion = expansion(not_fgti, GoalCord)
    ;
        AllFGTI = yes,
        list.map(project_expansion_goals, ArgExpansions, ArgGoalCords),
        ArgGoalsCord = cord.cord_list_to_cord(ArgGoalCords),
        % XXX If BaseGoal can be a plain_conj, then we should expand it here.
        GoalCord = cord.cons(BaseGoal, ArgGoalsCord),
        Expansion = expansion(fgti_var_size(TermVar, TotalSize), GoalCord)
    ).

:- pred append_expansions_after_goal_top_ftgi_loop(list(expansion)::in,
    bool::in, bool::out, int::in, int::out) is det.

append_expansions_after_goal_top_ftgi_loop([], !AllFGTI, !TotalSize).
append_expansions_after_goal_top_ftgi_loop([Expansion | Expansions],
        !AllFGTI, !TotalSize) :-
    Expansion = expansion(MaybeFGTI, _),
    (
        MaybeFGTI = not_fgti,
        !:AllFGTI = no
    ;
        MaybeFGTI = fgti_var_size(_, Size),
        !:TotalSize = !.TotalSize + Size
    ),
    append_expansions_after_goal_top_ftgi_loop(Expansions,
        !AllFGTI, !TotalSize).

:- pred project_expansion_goals(expansion::in, cord(hlds_goal)::out)
    is det.

project_expansion_goals(expansion(_, GoalCord), GoalCord).

%---------------------------------------------------------------------------%

:- pred expansion_to_goal_wrap_if_fgti(unravel_info::in,
    hlds_goal_info::in, expansion::in, hlds_goal::out) is det.

expansion_to_goal_wrap_if_fgti(UrInfo, GoalInfo, Expansion, Goal) :-
    Expansion = expansion(MaybeFGTI, ExpansionGoalCord),
    ExpansionGoals = cord.list(ExpansionGoalCord),
    (
        ExpansionGoals = [],
        Goal = hlds_goal(true_goal_expr, GoalInfo)
    ;
        ExpansionGoals = [ExpansionGoal0],
        ExpansionGoal0 = hlds_goal(ExpansionGoalExpr, ExpansionGoalInfo0),
        Context = goal_info_get_context(GoalInfo),
        goal_info_set_context(Context, ExpansionGoalInfo0, ExpansionGoalInfo),
        Goal = hlds_goal(ExpansionGoalExpr, ExpansionGoalInfo)
    ;
        ExpansionGoals = [_, _ | _],
        ( if
            MaybeFGTI = fgti_var_size(TermVar, Size),
            Size >= UrInfo ^ ui_fgt_threshold
        then
            goal_info_set_nonlocals(set_of_var.make_singleton(TermVar),
                GoalInfo, MarkedGoalInfo),
            mark_nonlocals_in_ground_term_initial(ExpansionGoals, MarkedGoals),
            ConjGoalExpr = conj(plain_conj, MarkedGoals),
            ConjGoal = hlds_goal(ConjGoalExpr, MarkedGoalInfo),
            Reason = from_ground_term(TermVar, from_ground_term_initial),
            GoalExpr = scope(Reason, ConjGoal),
            Goal = hlds_goal(GoalExpr, MarkedGoalInfo)
        else
            GoalExpr = conj(plain_conj, ExpansionGoals),
            Goal = hlds_goal(GoalExpr, GoalInfo)
        )
    ).

:- pred expansion_to_goal_cord_wrap_if_fgti(unravel_info::in,
    hlds_goal_info::in, expansion::in, cord(hlds_goal)::out) is det.

expansion_to_goal_cord_wrap_if_fgti(UrInfo, GoalInfo, Expansion,
        MaybeWrappedGoalCord) :-
    Expansion = expansion(MaybeFGTI, GoalCord),
    ( if
        MaybeFGTI = fgti_var_size(TermVar, Size),
        Size >= UrInfo ^ ui_fgt_threshold
    then
        Goals = cord.list(GoalCord),
        goal_info_set_nonlocals(set_of_var.make_singleton(TermVar),
            GoalInfo, MarkedGoalInfo),
        mark_nonlocals_in_ground_term_initial(Goals, MarkedGoals),
        ConjGoalExpr = conj(plain_conj, MarkedGoals),
        ConjGoal = hlds_goal(ConjGoalExpr, MarkedGoalInfo),
        Reason = from_ground_term(TermVar, from_ground_term_initial),
        ScopeGoalExpr = scope(Reason, ConjGoal),
        ScopeGoal = hlds_goal(ScopeGoalExpr, MarkedGoalInfo),
        MaybeWrappedGoalCord = cord.singleton(ScopeGoal)
    else
        MaybeWrappedGoalCord = GoalCord
    ).

:- pred mark_nonlocals_in_ground_term_initial(
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

mark_nonlocals_in_ground_term_initial([], []).
mark_nonlocals_in_ground_term_initial([Goal0 | Goals0], [Goal | Goals]) :-
    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    ( if
        GoalExpr = unify(LHSVar, RHS, _, _, _),
        RHS = rhs_functor(_, _, RHSVars)
    then
        set_of_var.list_to_set([LHSVar | RHSVars], NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    else
        unexpected($pred, "wrong shape goal")
    ),
    mark_nonlocals_in_ground_term_initial(Goals0, Goals).

%---------------------------------------------------------------------------%

:- pred perform_occurs_check(ancestor_var_map::in, prog_var::in,
    unravel_info::in, unravel_info::out) is det.

perform_occurs_check(AncestorVarMap, Var, !UrInfo) :-
    ( if map.search(AncestorVarMap, Var, AncestorContext) then
        % We *could* put the value of WarnOccursCheck into UrInfo, but
        % occurs check violations are very rare, so the gain would be
        % negligible. While the cost of copying an extra field on each
        % update of !UrInfo would be small, it would not buy us anything
        % useful.
        ModuleInfo = !.UrInfo ^ ui_module_info,
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals,
            warn_suspected_occurs_check_failure, WarnOccursCheck),
        (
            WarnOccursCheck = no
        ;
            WarnOccursCheck = yes,
            VarSet = !.UrInfo ^ ui_varset,
            varset.lookup_name(VarSet, Var, VarName),
            Pieces = [words("Warning: the")] ++
                color_as_subject([words("variable"), quote(VarName)]) ++
                [words("is")] ++
                color_as_incorrect(
                    [words("unified with a term containing itself.")]) ++
                [nl],
            Severity = severity_warning(warn_suspected_occurs_check_failure),
            Spec = spec($pred, Severity, phase_pt2h, AncestorContext, Pieces),
            add_unravel_spec(Spec, !UrInfo)
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred arg_context_to_unify_context(arg_context::in, int::in,
    unify_main_context::out, list(unify_sub_context)::out) is det.

arg_context_to_unify_context(ArgContext, ArgNum, MainContext, SubContexts) :-
    (
        ArgContext = ac_head(PredOrFunc, PredFormArity),
        ( if
            PredOrFunc = pf_function,
            PredFormArity = pred_form_arity(PredFormArityInt),
            ArgNum = PredFormArityInt
        then
            % It is the function result term in the head.
            MainContext = umc_head_result
        else
            % It is a non-function-result head argument.
            MainContext = umc_head(ArgNum)
        ),
        SubContexts = []
    ;
        ArgContext = ac_call(PredId),
        MainContext = umc_call(PredId, ArgNum),
        SubContexts = []
    ;
        ArgContext = ac_functor(ConsId, MainContext, SubContexts0),
        SubContext = unify_sub_context(ConsId, ArgNum),
        SubContexts = [SubContext | SubContexts0]
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.superhomogeneous.
%---------------------------------------------------------------------------%
