%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2021, 2023-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_unify_var_functor.m.
% Main author: fjh.
%
% This file contains the code to typecheck unifications of the form
% X = f(Y1, ..., Yn).
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_unify_var_functor.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred typecheck_unify_var_functor(unify_context::in, prog_context::in,
    goal_id::in, prog_var::in, cons_id::in, list(prog_var)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.typecheck_cons_infos.
:- import_module check_hlds.typecheck_error_undef.
:- import_module check_hlds.typecheck_error_unify.
:- import_module check_hlds.typecheck_error_util.
:- import_module check_hlds.typecheck_util.
:- import_module hlds.hlds_class.
:- import_module hlds.type_util.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.vartypes.

:- import_module maybe.
:- import_module require.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

typecheck_unify_var_functor(UnifyContext, Context, GoalId,
        LHSVar, ConsId, ArgVars, TypeAssignSet0, TypeAssignSet, !Info) :-
    % Get the list of possible constructors that match this functor/arity.
    % If there aren't any, report an undefined constructor error.
    list.length(ArgVars, Arity),
    typecheck_info_construct_all_cons_infos(!.Info, ConsId, Arity, GoalId,
        ConsInfoResult),
    (
        ConsInfoResult = cons_info_builtin_const(BuiltinType, BuiltinTypeName),
        typecheck_unify_var_functor_builtin(UnifyContext, Context,
            LHSVar, ConsId, BuiltinType, BuiltinTypeName,
            TypeAssignSet0, TypeAssignSet, !Info)
    ;
        ConsInfoResult = cons_info_tuple(ConsTypeInfo),
        typecheck_unify_var_functor_cons_infos(UnifyContext, Context, LHSVar,
            ConsId, Arity, ArgVars, [ConsTypeInfo],
            TypeAssignSet0, TypeAssignSet, !Info)
    ;
        ConsInfoResult = cons_info_du_ctor(DuCtor, ConsTypeInfos, ConsErrors),
        (
            ConsTypeInfos = [],
            TypeAssignSet = TypeAssignSet0,
            typecheck_info_get_error_clause_context(!.Info, ClauseContext),
            GoalContext = type_error_in_unify(UnifyContext),
            % Note that ConsErrors may be [], but the fact that there are
            % no ConsTypeInfos is itself an error.
            Spec = report_error_undef_du_ctor(ClauseContext, GoalContext,
                Context, DuCtor, ConsErrors),
            typecheck_info_add_error(Spec, !Info)
        ;
            ConsTypeInfos = [_ | _],
            typecheck_unify_var_functor_cons_infos(UnifyContext, Context,
                LHSVar, ConsId, Arity, ArgVars, ConsTypeInfos,
                TypeAssignSet0, TypeAssignSet, !Info)
        )
    ;
        ( ConsInfoResult = cons_info_field_access_func
        ; ConsInfoResult = cons_info_comp_gen_cons_id
        ),
        TypeAssignSet = TypeAssignSet0,
        typecheck_info_get_error_clause_context(!.Info, ClauseContext),
        GoalContext = type_error_in_unify(UnifyContext),
        Spec = report_error_undef_non_du_ctor(ClauseContext, GoalContext,
            Context, ConsId),
        typecheck_info_add_error(Spec, !Info)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred typecheck_unify_var_functor_builtin(unify_context::in,
    prog_context::in, prog_var::in, cons_id::in, builtin_type::in, string::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_unify_var_functor_builtin(UnifyContext, Context, LHSVar, ConsId,
        BuiltinType, BuiltinTypeName, TypeAssignSet0, TypeAssignSet, !Info) :-
    ( if BuiltinType = builtin_type_int(int_type_int) then
        typecheck_info_add_nosuffix_integer_var(LHSVar, !Info)
    else
        true
    ),
    ConsType = builtin_type(BuiltinType),
    list.foldl(
        type_assign_check_functor_type_builtin(ConsType, LHSVar),
        TypeAssignSet0, [], TypeAssignSet1),
    (
        TypeAssignSet1 = [_ | _],
        TypeAssignSet = TypeAssignSet1
    ;
        TypeAssignSet1 = [],
        % If we encountered an error, continue checking with the
        % original type assign set.
        TypeAssignSet = TypeAssignSet0,
        (
            TypeAssignSet0 = []
            % The error did not originate here, so generating an error
            % message here would be misleading.
        ;
            TypeAssignSet0 = [_ | _],
            varset.init(ConsTypeVarSet),
            ConsTypeInfo = cons_type_info(ConsTypeVarSet, [], ConsType, [],
                empty_hlds_constraint_db,
                source_builtin_type(BuiltinTypeName)),
            ConsIdSpec = report_error_unify_var_functor_result(!.Info,
                UnifyContext, Context, LHSVar, [ConsTypeInfo],
                ConsId, 0, TypeAssignSet0),
            typecheck_info_add_error(ConsIdSpec, !Info)
        )
    ).

:- pred type_assign_check_functor_type_builtin(mer_type::in,
    prog_var::in, type_assign::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_check_functor_type_builtin(ConsType, Y, TypeAssign0,
        !TypeAssignSet) :-
    % Unify the type of Var with the type of the constructor.
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    search_insert_var_type(Y, ConsType, MaybeTypeY, VarTypes0, VarTypes),
    (
        MaybeTypeY = yes(TypeY),
        ( if
            type_assign_unify_type(ConsType, TypeY, TypeAssign0, TypeAssign)
        then
            % The constraints are empty here because none are added by
            % unification with a functor.
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        else
            true
        )
    ;
        MaybeTypeY = no,
        % The constraints are empty here because none are added by
        % unification with a functor.
        type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
        !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred typecheck_unify_var_functor_cons_infos(unify_context::in,
    prog_context::in, prog_var::in, cons_id::in, arity::in, list(prog_var)::in,
    list(cons_type_info)::in(non_empty_list),
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_unify_var_functor_cons_infos(UnifyContext, Context, LHSVar,
        ConsId, Arity, ArgVars, ConsTypeInfos,
        TypeAssignSet0, TypeAssignSet, !Info) :-
    (
        ConsTypeInfos = [_]
    ;
        ConsTypeInfos = [_, _ | _],
        Sources = list.map(project_cons_type_info_source, ConsTypeInfos),
        Symbol = overloaded_func(ConsId, Sources),
        typecheck_info_add_overloaded_symbol(Symbol, Context, !Info)
    ),

    % Produce the ConsTypeAssignSet, which is essentially the
    % cross-product of the ConsTypeInfos and the TypeAssignSet0.
    get_cons_type_assigns_for_cons_defns(ConsTypeInfos, TypeAssignSet0,
        [], ConsTypeAssignSet),
    ( if
        ConsTypeAssignSet = [],
        TypeAssignSet0 = [_ | _]
    then
        % This should never happen, since undefined ctors
        % should be caught by the check just above.
        unexpected($pred, "undefined cons?")
    else
        true
    ),

    % Check that the type of the functor matches the type of the variable.
    typecheck_var_functor_types(LHSVar, ConsTypeAssignSet,
        [], ArgsTypeAssignSet),
    ( if
        ArgsTypeAssignSet = [],
        ConsTypeAssignSet = [_ | _]
    then
        ConsIdSpec = report_error_unify_var_functor_result(!.Info,
            UnifyContext, Context, LHSVar, ConsTypeInfos, ConsId, Arity,
            TypeAssignSet0),
        typecheck_info_add_error(ConsIdSpec, !Info)
    else
        true
    ),

    % Check that the type of the arguments of the functor matches
    % their expected type for this functor.
    typecheck_functor_arg_types(!.Info, ArgVars, ArgsTypeAssignSet,
        [], TypeAssignSet1),
    (
        TypeAssignSet1 = [_ | _],
        TypeAssignSet = TypeAssignSet1
    ;
        TypeAssignSet1 = [],
        % If we encountered an error, continue checking with the
        % original type assign set.
        TypeAssignSet = TypeAssignSet0,
        (
            ArgsTypeAssignSet = []
            % The error did not originate here, so generating an error
            % message here would be misleading.
        ;
            ArgsTypeAssignSet = [_ | _],
            ArgSpec = report_error_unify_var_functor_args(!.Info,
                UnifyContext, Context, LHSVar, ConsTypeInfos,
                ConsId, ArgVars, ArgsTypeAssignSet),
            typecheck_info_add_error(ArgSpec, !Info)
        )
    ).

%---------------------------------------------------------------------------%

:- type cons_type_assign
    --->    cons_type_assign(
                type_assign,
                mer_type,
                list(mer_type),
                cons_type_info_source
            ).

:- type cons_type_assign_set == list(cons_type_assign).

    % typecheck_unify_var_functor_get_ctors_for_type_assigns(ConsTypeInfos,
    %   TypeAssignSet, !ConsTypeAssignSet):
    %
    % Iterate over all the different possible pairings of all the
    % constructor definitions and all the type assignments.
    % For each constructor definition in ConsTypeInfos and type assignment
    % in TypeAssignSet, produce
    %
    %   TypeAssign - cons_type(Type, ArgTypes)
    %   cons_type_assign(TypeAssign, ConsType, ArgTypes, Source).
    %
    % ArgTypes->ConsType is one of the possible types for the constructor
    % in ConsTypeInfos, and where TypeAssign is the type assignment
    % renamed apart from the types of the constructors.
    %
    % This predicate iterates over the cons_type_infos;
    % get_cons_type_assigns_for_cons_defn iterates over the type_assigns.
    %
:- pred get_cons_type_assigns_for_cons_defns(list(cons_type_info)::in,
    type_assign_set::in,
    cons_type_assign_set::in, cons_type_assign_set::out) is det.

get_cons_type_assigns_for_cons_defns([], _, !ConsTypeAssignSet).
get_cons_type_assigns_for_cons_defns([ConsTypeInfo | ConsTypeInfos],
        TypeAssigns, !ConsTypeAssignSet) :-
    get_cons_type_assigns_for_cons_defn(ConsTypeInfo, TypeAssigns,
        !ConsTypeAssignSet),
    get_cons_type_assigns_for_cons_defns(ConsTypeInfos, TypeAssigns,
        !ConsTypeAssignSet).

:- pred get_cons_type_assigns_for_cons_defn(cons_type_info::in,
    type_assign_set::in,
    cons_type_assign_set::in, cons_type_assign_set::out) is det.

get_cons_type_assigns_for_cons_defn(_, [], !ConsTypeAssignSet).
get_cons_type_assigns_for_cons_defn(ConsTypeInfo, [TypeAssign | TypeAssigns],
        !ConsTypeAssignSet) :-
    get_cons_type_assign(ConsTypeInfo, TypeAssign, ConsTypeAssign),
    !:ConsTypeAssignSet = [ConsTypeAssign | !.ConsTypeAssignSet],
    get_cons_type_assigns_for_cons_defn(ConsTypeInfo, TypeAssigns,
        !ConsTypeAssignSet).

    % Given an cons_type_info, construct a type for the constructor
    % and a list of types of the arguments, suitably renamed apart
    % from the current type_assign's typevarset. Return them in a
    % cons_type_assign with the updated-for-the-renaming type_assign.
    %
:- pred get_cons_type_assign(cons_type_info::in, type_assign::in,
    cons_type_assign::out) is det.

get_cons_type_assign(ConsTypeInfo, TypeAssign0, ConsTypeAssign) :-
    ConsTypeInfo = cons_type_info(ConsTypeVarSet, ConsExistQVars0,
        ConsType0, ArgTypes0, ClassConstraintDb0, Source),

    % Rename apart the type vars in the type of the constructor
    % and the types of its arguments.
    % (Optimize the common case of a non-polymorphic type.)
    ( if varset.is_empty(ConsTypeVarSet) then
        ConsType = ConsType0,
        ArgTypes = ArgTypes0,
        TypeAssign2 = TypeAssign0,
        ConstraintDbToAdd = ClassConstraintDb0
    else
        type_assign_rename_apart(TypeAssign0, ConsTypeVarSet,
            TypeAssign1, Renaming),
        apply_renaming_to_type(Renaming, ConsType0, ConsType),
        apply_renaming_to_types(Renaming, ArgTypes0, ArgTypes),
        apply_renaming_to_tvars(Renaming,
            ConsExistQVars0, ConsExistQVars),
        apply_renaming_to_constraint_db(Renaming,
            ClassConstraintDb0, ConstraintDbToAdd),
        type_assign_get_existq_tvars(TypeAssign1, ExistQTVars0),
        ExistQTVars = ConsExistQVars ++ ExistQTVars0,
        type_assign_set_existq_tvars(ExistQTVars, TypeAssign1, TypeAssign2)
    ),

    % Add the constraints for this functor to the current constraint set.
    % Note that there can still be (ground) constraints even if the varset
    % is empty.
    %
    % For functors which are data constructors, the fact that we don't take
    % the dual corresponds to assuming that they will be used as deconstructors
    % rather than as constructors.
    type_assign_get_constraint_db(TypeAssign2, OldConstraintDb),
    merge_hlds_constraint_dbs(ConstraintDbToAdd, OldConstraintDb,
        ClassConstraintDb),
    type_assign_set_constraint_db(ClassConstraintDb,
        TypeAssign2, TypeAssign),
    ConsTypeAssign = cons_type_assign(TypeAssign, ConsType, ArgTypes, Source).

%---------------------------------------------------------------------------%

    % typecheck_var_functor_type(Var, ConsTypeAssignSet, !ArgsTypeAssignSet):
    %
    % For each possible cons type assignment in `ConsTypeAssignSet',
    % for each possible constructor type,
    % check that the type of `Var' matches this type.
    % If it does, add the type binding to !ArgsTypeAssignSet.
    %
:- pred typecheck_var_functor_types(prog_var::in, cons_type_assign_set::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_var_functor_types(_, [], !ArgsTypeAssignSet).
typecheck_var_functor_types(Var, [ConsTypeAssign | ConsTypeAssigns],
        !ArgsTypeAssignSet) :-
    typecheck_var_functor_type(Var, ConsTypeAssign, !ArgsTypeAssignSet),
    typecheck_var_functor_types(Var, ConsTypeAssigns, !ArgsTypeAssignSet).

:- pred typecheck_var_functor_type(prog_var::in, cons_type_assign::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_var_functor_type(Var, ConsTypeAssign0, !ArgsTypeAssignSet) :-
    ConsTypeAssign0 = cons_type_assign(TypeAssign0, ConsType, ConsArgTypes,
        Source0),

    % Unify the type of Var with the type of the constructor.
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    search_insert_var_type(Var, ConsType, MaybeOldVarType,
        VarTypes0, VarTypes),
    (
        MaybeOldVarType = yes(OldVarType),
        % VarTypes wasn't updated, so don't need to update its containing
        % type assign either.
        ( if
            type_assign_unify_type(ConsType, OldVarType,
                TypeAssign0, TypeAssign)
        then
            % The constraints are empty here because none are added by
            % unification with a functor.
            ArgsTypeAssign = args_type_assign(TypeAssign,
                ConsArgTypes, empty_hlds_constraint_db, atas_cons(Source0)),
            !:ArgsTypeAssignSet = [ArgsTypeAssign | !.ArgsTypeAssignSet]
        else
            true
        )
    ;
        MaybeOldVarType = no,
        type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
        % The constraints are empty here because none are added by
        % unification with a functor.
        ArgsTypeAssign = args_type_assign(TypeAssign,
            ConsArgTypes, empty_hlds_constraint_db, atas_cons(Source0)),
        !:ArgsTypeAssignSet = [ArgsTypeAssign | !.ArgsTypeAssignSet]
    ).

%---------------------------------------------------------------------------%

    % typecheck_functor_arg_types(Info, ArgVars, ArgsTypeAssigns, ...):
    %
    % For each possible cons type assignment in `ConsTypeAssignSet',
    % for each possible constructor argument types,
    % check that the types of `ArgVars' match these types.
    %
:- pred typecheck_functor_arg_types(typecheck_info::in, list(prog_var)::in,
    args_type_assign_set::in,
    type_assign_set::in, type_assign_set::out) is det.

typecheck_functor_arg_types(_, _, [], !TypeAssignSet).
typecheck_functor_arg_types(Info, ArgVars, [ArgsTypeAssign | ArgsTypeAssigns],
        !TypeAssignSet) :-
    ArgsTypeAssign = args_type_assign(TypeAssign, ArgTypes, _, _),
    type_assign_vars_have_types(Info, TypeAssign, ArgVars, ArgTypes,
        !TypeAssignSet),
    typecheck_functor_arg_types(Info, ArgVars, ArgsTypeAssigns,
        !TypeAssignSet).

    % type_assign_vars_have_types(Info, TypeAssign, ArgVars, Types,
    %   TypeAssignSet0, TypeAssignSet):
    % Let TAs = { TA | TA is an extension of TypeAssign for which
    %   the types of the ArgVars unify with their respective Types },
    % list.append(TAs, TypeAssignSet0, TypeAssignSet).
    %
:- pred type_assign_vars_have_types(typecheck_info::in, type_assign::in,
    list(prog_var)::in, list(mer_type)::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_vars_have_types(_, TypeAssign, [], [],
        TypeAssignSet, [TypeAssign | TypeAssignSet]).
type_assign_vars_have_types(_, _, [], [_ | _], _, _) :-
    unexpected($pred, "length mismatch").
type_assign_vars_have_types(_, _, [_ | _], [], _, _) :-
    unexpected($pred, "length mismatch").
type_assign_vars_have_types(Info, TypeAssign0,
        [ArgVar | ArgVars], [Type | Types], TypeAssignSet0, TypeAssignSet) :-
    acc_type_assign_if_var_can_have_type(TypeAssign0, ArgVar, Type,
        [], TypeAssignSet1),
    type_assigns_vars_have_types(Info, TypeAssignSet1,
        ArgVars, Types, TypeAssignSet0, TypeAssignSet).

    % type_assigns_vars_have_types(Info, TypeAssigns, ArgVars, Types,
    %       TypeAssignSet0, TypeAssignSet):
    % Let TAs = { TA | TA is an extension of a member of TypeAssigns for which
    %   the types of the ArgVars unify with their respective Types },
    % list.append(TAs, TypeAssignSet0, TypeAssignSet).
    %
:- pred type_assigns_vars_have_types(typecheck_info::in,
    type_assign_set::in, list(prog_var)::in, list(mer_type)::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assigns_vars_have_types(_, [], _, _, !TypeAssignSet).
type_assigns_vars_have_types(Info, [TypeAssign | TypeAssigns],
        ArgVars, Types, !TypeAssignSet) :-
    type_assign_vars_have_types(Info, TypeAssign, ArgVars, Types,
        !TypeAssignSet),
    type_assigns_vars_have_types(Info, TypeAssigns, ArgVars, Types,
        !TypeAssignSet).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_unify_var_functor.
%---------------------------------------------------------------------------%
