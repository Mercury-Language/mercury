%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_type_unify.m.
%
% Type unification and type subsumption.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_type_unify.
:- interface.

:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module term.

%---------------------------------------------------------------------------%
%
% Type unification.
%

    % Unify (with occurs check) two types with respect to a type substitution
    % and update the type bindings. The third argument is a list of type
    % variables which cannot be bound (i.e. head type variables).
    %
    % No kind checking is done, since it is assumed that kind errors
    % will be picked up elsewhere.
    %
:- pred type_unify(mer_type::in, mer_type::in, list(tvar)::in, tsubst::in,
    tsubst::out) is semidet.

:- pred type_unify_list(list(mer_type)::in, list(mer_type)::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

%---------------------------------------------------------------------------%
%
% Type subsumption.
%

    % type_subsumes(TypeA, TypeB, Subst) succeeds iff TypeA subsumes
    % (is more general than) TypeB, producing a type substitution
    % which when applied to TypeA will give TypeB.
    %
:- pred type_subsumes(mer_type::in, mer_type::in, tsubst::out) is semidet.

    % Same as type_subsumes, but aborts instead of failing.
    %
:- pred type_subsumes_det(mer_type::in, mer_type::in, tsubst::out) is det.

    % type_list_subsumes(TypesA, TypesB, Subst) succeeds iff the list TypesA
    % subsumes (is more general than) TypesB, producing a type substitution
    % which, when applied to TypesA, will give TypesB.
    %
:- pred type_list_subsumes(list(mer_type)::in, list(mer_type)::in, tsubst::out)
    is semidet.

    % Same as type_list_subsumes, but aborts instead of failing.
    %
:- pred type_list_subsumes_det(list(mer_type)::in, list(mer_type)::in,
    tsubst::out) is det.

    % arg_type_list_subsumes(TVarSet, ExistQVars, ArgTypes, HeadTypeParams,
    %   CalleeTVarSet, CalleeExistQVars, CalleeArgTypes):
    % XXX This comment has suffered bit rot.
    %
    % Check that the argument types of the called predicate, function or
    % constructor subsume the types of the arguments of the call. This checks
    % that none of the existentially quantified type variables of the callee
    % are bound.
    %
:- pred arg_type_list_subsumes(tvarset::in, existq_tvars::in,
    list(mer_type)::in, list(tvar)::in,
    tvarset::in, tvar_kind_map::in, existq_tvars::in, list(mer_type)::in)
    is semidet.

    % Check whether two lists of types are identical up to renaming.
    %
:- pred identical_up_to_renaming(list(mer_type)::in, list(mer_type)::in)
    is semidet.

%---------------------------------------------------------------------------%

    % compute_caller_callee_type_substitution(CalleeArgTypes, CallerArgTypes,
    %   ExternalTypeParams, CalleeExistQTVars, TypeSubn):
    %
    % Work out a type substitution to map the callee's argument types
    % into the caller's.
    %
    % Note that this predicate belongs here, and not in prog_type_subst.m,
    % because it *computes* a substitution, while prog_type_subst.m's job
    % is *applying* substitutions.
    %
:- pred compute_caller_callee_type_substitution(list(mer_type)::in,
    list(mer_type)::in, list(tvar)::in, list(tvar)::in, tsubst::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_subst.

:- import_module require.

%---------------------------------------------------------------------------%

type_unify(X, Y, HeadTypeParams, !Bindings) :-
    ( if X = type_variable(VarX, _) then
        type_unify_var(VarX, Y, HeadTypeParams, !Bindings)
    else if Y = type_variable(VarY, _) then
        type_unify_var(VarY, X, HeadTypeParams, !Bindings)
    else if type_unify_nonvar(X, Y, HeadTypeParams, !Bindings) then
        true
    else
        % Some special cases are not handled above. We handle them separately
        % here.
        type_unify_special(X, Y, HeadTypeParams, !Bindings)
    ).

:- pred type_unify_var(tvar::in, mer_type::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_var(VarX, TypeY, HeadTypeParams, !Bindings) :-
    ( if TypeY = type_variable(VarY, KindY) then
        type_unify_var_var(VarX, VarY, KindY, HeadTypeParams, !Bindings)
    else if map.search(!.Bindings, VarX, BindingOfX) then
        % VarX has a binding. Y is not a variable.
        type_unify(BindingOfX, TypeY, HeadTypeParams, !Bindings)
    else
        % VarX has no binding, so bind it to TypeY.
        not type_occurs(TypeY, VarX, !.Bindings),
        not list.member(VarX, HeadTypeParams),
        map.det_insert(VarX, TypeY, !Bindings)
    ).

:- pred type_unify_var_var(tvar::in, tvar::in, kind::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_var_var(X, Y, Kind, HeadTypeParams, !Bindings) :-
    ( if list.member(Y, HeadTypeParams) then
        type_unify_head_type_param(X, Y, Kind, HeadTypeParams, !Bindings)
    else if list.member(X, HeadTypeParams) then
        type_unify_head_type_param(Y, X, Kind, HeadTypeParams, !Bindings)
    else if map.search(!.Bindings, X, BindingOfX) then
        ( if map.search(!.Bindings, Y, BindingOfY) then
            % Both X and Y already have bindings - just unify the
            % types they are bound to.
            type_unify(BindingOfX, BindingOfY, HeadTypeParams, !Bindings)
        else
            % Y hasn't been bound yet.
            apply_rec_subst_to_type(!.Bindings, BindingOfX, SubstBindingOfX),
            ( if SubstBindingOfX = type_variable(Y, _) then
                true
            else
                not type_occurs(SubstBindingOfX, Y, !.Bindings),
                map.det_insert(Y, SubstBindingOfX, !Bindings)
            )
        )
    else
        % Neither X nor Y is a head type param. X had not been bound yet.
        ( if map.search(!.Bindings, Y, BindingOfY) then
            apply_rec_subst_to_type(!.Bindings, BindingOfY, SubstBindingOfY),
            ( if SubstBindingOfY = type_variable(X, _) then
                true
            else
                not type_occurs(SubstBindingOfY, X, !.Bindings),
                map.det_insert(X, SubstBindingOfY, !Bindings)
            )
        else
            % Both X and Y are unbound type variables - bind one to the other.
            ( if X = Y then
                true
            else
                map.det_insert(X, type_variable(Y, Kind), !Bindings)
            )
        )
    ).

:- pred type_unify_head_type_param(tvar::in, tvar::in, kind::in,
    list(tvar)::in, tsubst::in, tsubst::out) is semidet.

type_unify_head_type_param(Var, HeadVar, Kind, HeadTypeParams, !Bindings) :-
    ( if map.search(!.Bindings, Var, BindingOfVar) then
        BindingOfVar = type_variable(Var2, _),
        type_unify_head_type_param(Var2, HeadVar, Kind, HeadTypeParams,
            !Bindings)
    else
        ( if Var = HeadVar then
            true
        else
            not list.member(Var, HeadTypeParams),
            map.det_insert(Var, type_variable(HeadVar, Kind), !Bindings)
        )
    ).

    % Unify two types, neither of which are variables. Two special cases
    % which are not handled here are apply_n types and kinded types.
    % Those are handled below.
    %
:- pred type_unify_nonvar(mer_type::in, mer_type::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_nonvar(TypeX, TypeY, HeadTypeParams, !Bindings) :-
    (
        TypeX = defined_type(SymName, ArgTypesX, _),
        TypeY = defined_type(SymName, ArgTypesY, _),
        % Instead of insisting that the names are equal and the arg lists
        % unify, we should consider attempting to expand equivalence types
        % first. That would require the type table to be passed in to the
        % unification algorithm, though.
        type_unify_list(ArgTypesX, ArgTypesY, HeadTypeParams, !Bindings)
    ;
        TypeX = builtin_type(BuiltinType),
        TypeY = builtin_type(BuiltinType)
    ;
        TypeX = higher_order_type(PorF, ArgTypesX, _, Purity, EvalMethod),
        TypeY = higher_order_type(PorF, ArgTypesY, _, Purity, EvalMethod),
        type_unify_list(ArgTypesX, ArgTypesY, HeadTypeParams, !Bindings)
    ;
        TypeX = tuple_type(ArgTypesX, _),
        TypeY = tuple_type(ArgTypesY, _),
        type_unify_list(ArgTypesX, ArgTypesY, HeadTypeParams, !Bindings)
    ).

    % Handle apply_n types and kinded types.
    %
:- pred type_unify_special(mer_type::in, mer_type::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_special(TypeX, TypeY, HeadTypeParams, !Bindings) :-
    ( if TypeX = apply_n_type(VarX, ArgTypesX, _) then
        type_unify_apply(TypeY, VarX, ArgTypesX, HeadTypeParams, !Bindings)
    else if TypeY = apply_n_type(VarY, ArgTypesY, _) then
        type_unify_apply(TypeX, VarY, ArgTypesY, HeadTypeParams, !Bindings)
    else if TypeX = kinded_type(RawX, _) then
        ( if TypeY = kinded_type(RawY, _) then
            type_unify(RawX, RawY, HeadTypeParams, !Bindings)
        else
            type_unify(RawX, TypeY, HeadTypeParams, !Bindings)
        )
    else if TypeY = kinded_type(RawY, _) then
        type_unify(TypeX, RawY, HeadTypeParams, !Bindings)
    else
        fail
    ).

    % The idea here is that we try to strip off arguments from Y starting
    % from the end and unify each with the corresponding argument of X.
    % If we reach an atomic type before the arguments run out, we fail.
    % If we reach a variable before the arguments run out, we unify it
    % with what remains of the apply_n expression. If we manage to unify
    % all of the arguments, we unify the apply_n variable with what remains
    % of the other expression.
    %
    % Note that Y is not a variable, since that case would have been caught
    % by type_unify.
    %
:- pred type_unify_apply(mer_type::in, tvar::in, list(mer_type)::in,
    list(tvar)::in, tsubst::in, tsubst::out) is semidet.

type_unify_apply(TypeY, VarX, ArgTypesX0, HeadTypeParams, !Bindings) :-
    (
        TypeY = defined_type(NameY, ArgTypesY0, KindY0),
        type_unify_args(ArgTypesX0, ArgTypesY0, ArgTypesY, KindY0, KindY,
            HeadTypeParams, !Bindings),
        type_unify_var(VarX, defined_type(NameY, ArgTypesY, KindY),
            HeadTypeParams, !Bindings)
    ;
        TypeY = builtin_type(_),
        ArgTypesX0 = [],
        type_unify_var(VarX, TypeY, HeadTypeParams, !Bindings)
    ;
        TypeY = higher_order_type(_, _, _, _, _),
        ArgTypesX0 = [],
        type_unify_var(VarX, TypeY, HeadTypeParams, !Bindings)
    ;
        TypeY = tuple_type(ArgTypesY0, KindY0),
        type_unify_args(ArgTypesX0, ArgTypesY0, ArgTypesY, KindY0, KindY,
            HeadTypeParams, !Bindings),
        type_unify_var(VarX, tuple_type(ArgTypesY, KindY), HeadTypeParams,
            !Bindings)
    ;
        TypeY = apply_n_type(VarY, ArgTypesY0, Kind0),
        list.length(ArgTypesX0, NArgTypesX0),
        list.length(ArgTypesY0, NArgTypesY0),
        compare(Result, NArgTypesX0, NArgTypesY0),
        (
            Result = (<),
            type_unify_args(ArgTypesX0, ArgTypesY0, ArgTypesY, Kind0, Kind,
                HeadTypeParams, !Bindings),
            type_unify_var(VarX, apply_n_type(VarY, ArgTypesY, Kind),
                HeadTypeParams, !Bindings)
        ;
            Result = (=),
            % We know here that the list of remaining args will be empty.
            type_unify_args(ArgTypesX0, ArgTypesY0, _, Kind0, Kind,
                HeadTypeParams, !Bindings),
            type_unify_var_var(VarX, VarY, Kind, HeadTypeParams, !Bindings)
        ;
            Result = (>),
            type_unify_args(ArgTypesY0, ArgTypesX0, ArgTypesX, Kind0, Kind,
                HeadTypeParams, !Bindings),
            type_unify_var(VarY, apply_n_type(VarX, ArgTypesX, Kind),
                HeadTypeParams, !Bindings)
        )
    ;
        TypeY = kinded_type(RawY, _),
        type_unify_apply(RawY, VarX, ArgTypesX0, HeadTypeParams, !Bindings)
    ;
        TypeY = builtin_type(_),
        % XXX I (zs) am not sure *why* it is ok to fail here.
        fail
    ).

:- pred type_unify_args(list(mer_type)::in, list(mer_type)::in,
    list(mer_type)::out, kind::in, kind::out, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_args(ArgTypesX, ArgTypesY0, ArgTypesY,
        KindY0, KindY, HeadTypeParams, !Bindings) :-
    list.reverse(ArgTypesX, RevArgTypesX),
    list.reverse(ArgTypesY0, RevArgTypesY0),
    type_unify_rev_args(RevArgTypesX, RevArgTypesY0, RevArgTypesY,
        KindY0, KindY, HeadTypeParams, !Bindings),
    list.reverse(RevArgTypesY, ArgTypesY).

:- pred type_unify_rev_args(list(mer_type)::in, list(mer_type)::in,
    list(mer_type)::out, kind::in, kind::out, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_rev_args([], ArgTypesY, ArgTypesY, KindY, KindY, _, !Bindings).
type_unify_rev_args([ArgTypeX | ArgTypesX], [ArgTypeY0 | ArgTypesY0],
        ArgTypesY, KindY0, KindY, HeadTypeParams, !Bindings) :-
    type_unify(ArgTypeX, ArgTypeY0, HeadTypeParams, !Bindings),
    KindY1 = kind_arrow(get_type_kind(ArgTypeY0), KindY0),
    type_unify_rev_args(ArgTypesX, ArgTypesY0, ArgTypesY,
        KindY1, KindY, HeadTypeParams, !Bindings).

type_unify_list([], [], _HeadTypeParams, !Bindings).
type_unify_list([X | Xs], [Y | Ys], HeadTypeParams, !Bindings) :-
    type_unify(X, Y, HeadTypeParams, !Bindings),
    type_unify_list(Xs, Ys, HeadTypeParams, !Bindings).

    % type_occurs(Type, Var, Subst) succeeds iff Type contains Var,
    % perhaps indirectly via the substitution. (The variable must not
    % be mapped by the substitution.)
    %
:- pred type_occurs(mer_type::in, tvar::in, tsubst::in) is semidet.

type_occurs(TypeX, Y, Bindings) :-
    require_complete_switch [TypeX]
    (
        TypeX = type_variable(X, _),
        ( if X = Y then
            true
        else
            map.search(Bindings, X, BindingOfX),
            type_occurs(BindingOfX, Y, Bindings)
        )
    ;
        TypeX = defined_type(_, ArgTypes, _),
        type_occurs_list(ArgTypes, Y, Bindings)
    ;
        TypeX = higher_order_type(_, ArgTypes, _, _, _),
        type_occurs_list(ArgTypes, Y, Bindings)
    ;
        TypeX = tuple_type(ArgTypes, _),
        type_occurs_list(ArgTypes, Y, Bindings)
    ;
        TypeX = apply_n_type(X, ArgTypes, _),
        (
            X = Y
        ;
            type_occurs_list(ArgTypes, Y, Bindings)
        ;
            map.search(Bindings, X, BindingOfX),
            type_occurs(BindingOfX, Y, Bindings)
        )
    ;
        TypeX = kinded_type(TypeX1, _),
        type_occurs(TypeX1, Y, Bindings)
    ;
        TypeX = builtin_type(_),
        fail
    ).

:- pred type_occurs_list(list(mer_type)::in, tvar::in, tsubst::in) is semidet.

type_occurs_list([X | Xs], Y,  Bindings) :-
    (
        type_occurs(X, Y, Bindings)
    ;
        type_occurs_list(Xs, Y, Bindings)
    ).

%---------------------------------------------------------------------------%

type_subsumes(TypeA, TypeB, TypeSubst) :-
    % TypeA subsumes TypeB iff TypeA can be unified with TypeB
    % without binding any of the type variables in TypeB.
    type_vars_in_type(TypeB, TypeBVars),
    map.init(TypeSubst0),
    type_unify(TypeA, TypeB, TypeBVars, TypeSubst0, TypeSubst).

type_subsumes_det(TypeA, TypeB, TypeSubst) :-
    ( if type_subsumes(TypeA, TypeB, TypeSubstPrime) then
        TypeSubst = TypeSubstPrime
    else
        unexpected($pred, "type_subsumes failed")
    ).

type_list_subsumes(TypesA, TypesB, TypeSubst) :-
    % TypesA subsumes TypesB iff TypesA can be unified with TypesB
    % without binding any of the type variables in TypesB.
    type_vars_in_types(TypesB, TypesBVars),
    map.init(TypeSubst0),
    type_unify_list(TypesA, TypesB, TypesBVars, TypeSubst0, TypeSubst).

type_list_subsumes_det(TypesA, TypesB, TypeSubst) :-
    ( if type_list_subsumes(TypesA, TypesB, TypeSubstPrime) then
        TypeSubst = TypeSubstPrime
    else
        unexpected($pred, "type_list_subsumes failed")
    ).

arg_type_list_subsumes(TVarSet, ExistQVars, ActualArgTypes, HeadTypeParams,
        CalleeTVarSet, PredKindMap, PredExistQVars, PredArgTypes) :-
    % Rename the type variables in the callee's argument types.
    tvarset_merge_renaming(TVarSet, CalleeTVarSet, _TVarSet1, Renaming),
    apply_variable_renaming_to_tvar_kind_map(Renaming, PredKindMap,
        ParentKindMap),
    apply_variable_renaming_to_type_list(Renaming, PredArgTypes,
        ParentArgTypes),
    apply_variable_renaming_to_tvar_list(Renaming, PredExistQVars,
        ParentExistQVars),

    % Check that the types of the candidate predicate/function
    % subsume the actual argument types.
    % [This is the right thing to do even for calls to
    % existentially typed preds, because we're using the
    % type variables from the callee's pred decl (obtained
    % from the pred_info via pred_info_get_arg_types) not the types
    % inferred from the callee's clauses (and stored in the
    % clauses_info and proc_info) -- the latter
    % might not subsume the actual argument types.]

    (
        ExistQVars = [],
        type_list_subsumes(ParentArgTypes, ActualArgTypes, ParentToActualSubst)
    ;
        ExistQVars = [_ | _],
        % For calls to existentially type preds, we may need to bind
        % type variables in the caller, not just those in the callee.
        type_unify_list(ParentArgTypes, ActualArgTypes, HeadTypeParams,
            map.init, ParentToActualSubst)
    ),

    % Check that the type substitution did not bind any existentially
    % typed variables to non-ground types.
    (
        ParentExistQVars = []
        % Optimize common case.
    ;
        ParentExistQVars = [_ | _],
        apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualSubst,
            ParentExistQVars, ActualExistQTypes),
        all [T] (
            list.member(T, ActualExistQTypes)
        =>
            T = type_variable(_, _)
        )

        % It might make sense to also check that the type substitution
        % did not bind any existentially typed variables to universally
        % quantified type variables in the caller's argument types.
    ).

identical_up_to_renaming(TypesList1, TypesList2) :-
    % They are identical up to renaming if they subsume each other.
    type_list_subsumes(TypesList1, TypesList2, _),
    type_list_subsumes(TypesList2, TypesList1, _).

%---------------------------------------------------------------------------%

compute_caller_callee_type_substitution(CalleeArgTypes, CallerArgTypes,
        ExternalTypeParams, CalleeExistQVars, TypeSubn) :-
    (
        CalleeExistQVars = [],
        ( if type_list_subsumes(CalleeArgTypes, CallerArgTypes, TypeSubn0) then
            TypeSubn = TypeSubn0
        else
            % The callee's arg types should always be unifiable with the
            % caller's, otherwise there is a type error that should have
            % been detected by typechecking. But polymorphism.m introduces
            % type-incorrect code -- e.g. compare(Res, EnumA, EnumB) gets
            % converted into builtin_compare_int(Res, EnumA, EnumB), which
            % is a type error, since it assumes that an enumeration is an int.
            % In those cases, we don't need to worry about the type
            % substitution. (Perhaps it would be better if polymorphism
            % introduced calls to unsafe_type_cast/2 for such cases.)
            map.init(TypeSubn)
        )
    ;
        CalleeExistQVars = [_ | _],
        % For calls to existentially type preds, we may need to bind
        % type variables in the caller, as well as in the callee.
        ( if
            map.init(TypeSubn0),
            type_unify_list(CalleeArgTypes, CallerArgTypes, ExternalTypeParams,
                TypeSubn0, TypeSubn1)
        then
            TypeSubn = TypeSubn1
        else
            unexpected($pred, "type unification failed")
        )
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_type_unify.
%---------------------------------------------------------------------------%
