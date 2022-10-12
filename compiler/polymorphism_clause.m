%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012, 2014 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
% File: polymorphism_clause.m.
% Main authors: fjh and zs.
%
% This module handles the part of the polymorphism transformation
% that involves transforming clauses, specifically clause heads;
% clauses bodies, i.e. goals, are transformed by polymorphism_goal.m.
%
% The polymorphism transformation is described by the comment at the top of
% polymorphism.m.
%---------------------------------------------------------------------------%

:- module check_hlds.polymorphism_clause.
:- interface.

:- import_module check_hlds.polymorphism_info.
:- import_module hlds.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- pred polymorphism_process_clause_info(pred_info::in,
    poly_arg_vector(mer_mode)::out, clauses_info::in, clauses_info::out,
    poly_info::in, poly_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.polymorphism_goal.
:- import_module check_hlds.polymorphism_type_class_info.
:- import_module check_hlds.polymorphism_type_info.
:- import_module hlds.const_struct.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_goal.
:- import_module hlds.pred_name.
:- import_module hlds.quantification.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

polymorphism_process_clause_info(PredInfo0, ExtraArgModes,
        !ClausesInfo, !Info) :-
    !.ClausesInfo = clauses_info(VarSet, ExplicitVarTypes,
        _VarTable, _RttiVarMaps, TVarNameMap, HeadVars0, ClausesRep0,
        ItemNumbers, HaveForeignClauses, HadSyntaxErrors),
    setup_headvars(PredInfo0, HeadVars0, HeadVars, ExtraArgModes,
        UnconstrainedTVars, ExtraTypeInfoHeadVars,
        ExistTypeClassInfoHeadVars, !Info),
    ( if pred_info_is_imported(PredInfo0) then
        % We get here only if we need only the *interface* of this predicate,
        % not its code. If PredInfo0 is *opt*-imported, then the call to
        % pred_info_is_imported will fail, and we get to the else branch
        % instead.
        ClausesRep = ClausesRep0
    else
        get_clause_list_for_replacement(ClausesRep0, Clauses0),
        list.map_foldl(
            polymorphism_process_clause(PredInfo0, HeadVars0, HeadVars,
                UnconstrainedTVars, ExtraTypeInfoHeadVars,
                ExistTypeClassInfoHeadVars),
            Clauses0, Clauses, !Info),
        set_clause_list(Clauses, ClausesRep)
    ),
    % Set the new values of the fields in clauses_info.
    poly_info_get_var_table(!.Info, VarTable),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps),
    % The VarSet and ExplicitVarTypes fields are used
    % only while adding the clauses and doing typechecking.
    !:ClausesInfo = clauses_info(VarSet, ExplicitVarTypes,
        VarTable, RttiVarMaps, TVarNameMap, HeadVars, ClausesRep,
        ItemNumbers, HaveForeignClauses, HadSyntaxErrors).

%---------------------------------------------------------------------------%

    % XXX document me
    %
    % XXX the following code ought to be rewritten to handle
    % existential/universal type_infos and type_class_infos
    % in a more consistent manner.
    %
:- pred setup_headvars(pred_info::in, proc_arg_vector(prog_var)::in,
    proc_arg_vector(prog_var)::out, poly_arg_vector(mer_mode)::out,
    list(tvar)::out, list(prog_var)::out, list(prog_var)::out,
    poly_info::in, poly_info::out) is det.

setup_headvars(PredInfo, !HeadVars, !:ExtraArgModes,
        AllUnconstrainedTVars, AllExtraHeadTypeInfoVars,
        ExistHeadTypeClassInfoVars, !Info) :-
    pred_info_get_origin(PredInfo, Origin),
    !:ExtraArgModes = poly_arg_vector_init,
    ( if
        Origin = origin_user(OriginUser),
        OriginUser = user_made_instance_method(_, InstanceMethodConstraints)
    then
        setup_instance_method_headvars(PredInfo, InstanceMethodConstraints,
            ClassContext, InstanceTVars,
            InstanceUnconstrainedTVars, InstanceUnconstrainedTypeInfoVars,
            !HeadVars, !ExtraArgModes, !Info)
    else
        pred_info_get_class_context(PredInfo, ClassContext),
        InstanceTVars = [],
        InstanceUnconstrainedTVars = [],
        InstanceUnconstrainedTypeInfoVars = []
    ),

    % Grab the appropriate fields from the pred_info.
    pred_info_get_arg_types(PredInfo, ArgTypeVarSet, ExistQVars, ArgTypes),

    % Insert extra head variables to hold the address of the type_infos
    % and typeclass_infos. We insert one variable for each unconstrained
    % type variable (for the type_info) and one variable for each
    % constraint (for the typeclass_info).
    %
    % The order of these variables is important, and must match the order
    % specified at the top of this file.

    % Make a fresh variable for each class constraint, returning a list of
    % variables that appear in the constraints, along with the location of
    % the type infos for them. For the existential constraints, we want
    % the rtti_varmaps to contain the internal view of the types (that is,
    % with type variables bound) so we may need to look up the actual
    % constraints in the constraint map. For the universal constraints there
    % is no distinction between the internal views and the external view, so
    % we just use the constraints from the class context.
    ClassContext = constraints(UnivConstraints, ExistConstraints),
    constraint_list_get_tvars(UnivConstraints, UnivConstrainedTVars),
    constraint_list_get_tvars(ExistConstraints, ExistConstrainedTVars),
    poly_info_get_constraint_map(!.Info, ConstraintMap),
    get_improved_exists_head_constraints(ConstraintMap, ExistConstraints,
        ActualExistConstraints),
    ( if
        pred_info_get_markers(PredInfo, PredMarkers),
        check_marker(PredMarkers, marker_class_method)
    then
        % For class methods we record the type_info_locns even for the
        % existential constraints. It is easier to do it here than when we
        % are expanding class method bodies, and we know there won't be any
        % references to the type_info after the instance method call so
        % recording them now won't be a problem.
        RecordExistQLocns = do_record_type_info_locns
    else
        RecordExistQLocns = do_not_record_type_info_locns
    ),
    make_typeclass_info_head_vars(RecordExistQLocns, ActualExistConstraints,
        ExistHeadTypeClassInfoVars, !Info),
    make_typeclass_info_head_vars(do_record_type_info_locns, UnivConstraints,
        UnivHeadTypeClassInfoVars, !Info),

    type_vars_in_types(ArgTypes, HeadTypeVars),
    list.delete_elems(HeadTypeVars, UnivConstrainedTVars,
        UnconstrainedTVars0),
    list.delete_elems(UnconstrainedTVars0, ExistConstrainedTVars,
        UnconstrainedTVars1),

    % Typeinfos for the instance tvars have already been introduced by
    % setup_instance_method_headvars.
    list.delete_elems(UnconstrainedTVars1, InstanceTVars, UnconstrainedTVars2),
    list.remove_dups(UnconstrainedTVars2, UnconstrainedTVars),

    (
        ExistQVars = [],
        % Optimize common case.
        UnconstrainedUnivTVars = UnconstrainedTVars,
        UnconstrainedExistTVars = [],
        ExistHeadTypeInfoVars = []
    ;
        ExistQVars = [_ | _],
        list.delete_elems(UnconstrainedTVars, ExistQVars,
            UnconstrainedUnivTVars),
        list.delete_elems(UnconstrainedTVars, UnconstrainedUnivTVars,
            UnconstrainedExistTVars),
        make_head_vars(ArgTypeVarSet, UnconstrainedExistTVars,
            ExistHeadTypeInfoVars, !Info)
    ),

    make_head_vars(ArgTypeVarSet, UnconstrainedUnivTVars, UnivHeadTypeInfoVars,
        !Info),
    ExtraHeadTypeInfoVars = UnivHeadTypeInfoVars ++ ExistHeadTypeInfoVars,

    AllExtraHeadTypeInfoVars =
        InstanceUnconstrainedTypeInfoVars ++ ExtraHeadTypeInfoVars,
    list.condense([InstanceUnconstrainedTVars, UnconstrainedUnivTVars,
        UnconstrainedExistTVars], AllUnconstrainedTVars),

    proc_arg_vector_set_univ_type_infos(UnivHeadTypeInfoVars, !HeadVars),
    proc_arg_vector_set_exist_type_infos(ExistHeadTypeInfoVars, !HeadVars),
    proc_arg_vector_set_univ_typeclass_infos(UnivHeadTypeClassInfoVars,
        !HeadVars),
    proc_arg_vector_set_exist_typeclass_infos(ExistHeadTypeClassInfoVars,
        !HeadVars),

    % Figure out the modes of the introduced type_info and typeclass_info
    % arguments.

    in_mode(In),
    out_mode(Out),
    list.length(UnconstrainedUnivTVars, NumUnconstrainedUnivTVars),
    list.length(UnconstrainedExistTVars, NumUnconstrainedExistTVars),
    list.length(UnivHeadTypeClassInfoVars, NumUnivClassInfoVars),
    list.length(ExistHeadTypeClassInfoVars, NumExistClassInfoVars),
    list.duplicate(NumUnconstrainedUnivTVars, In, UnivTypeInfoModes),
    list.duplicate(NumUnconstrainedExistTVars, Out, ExistTypeInfoModes),
    list.duplicate(NumUnivClassInfoVars, In, UnivTypeClassInfoModes),
    list.duplicate(NumExistClassInfoVars, Out, ExistTypeClassInfoModes),
    poly_arg_vector_set_univ_type_infos(UnivTypeInfoModes, !ExtraArgModes),
    poly_arg_vector_set_exist_type_infos(ExistTypeInfoModes, !ExtraArgModes),
    poly_arg_vector_set_univ_typeclass_infos(UnivTypeClassInfoModes,
        !ExtraArgModes),
    poly_arg_vector_set_exist_typeclass_infos(ExistTypeClassInfoModes,
        !ExtraArgModes),

    % Add the locations of the typeinfos for unconstrained, universally
    % quantified type variables to the initial rtti_varmaps. Also add the
    % locations of typeclass_infos.
    some [!RttiVarMaps] (
        poly_info_get_rtti_varmaps(!.Info, !:RttiVarMaps),

        list.map(var_as_type_info_locn, UnivHeadTypeInfoVars, UnivTypeLocns),
        list.foldl_corresponding(rtti_det_insert_type_info_locn,
            UnconstrainedUnivTVars, UnivTypeLocns, !RttiVarMaps),

        list.map(var_as_type_info_locn, ExistHeadTypeInfoVars, ExistTypeLocns),
        list.foldl_corresponding(rtti_det_insert_type_info_locn,
            UnconstrainedExistTVars, ExistTypeLocns, !RttiVarMaps),

        list.map(var_as_type_info_locn,
            InstanceUnconstrainedTypeInfoVars, InstanceUnconstrainedTypeLocns),
        list.foldl_corresponding(rtti_det_insert_type_info_locn,
            InstanceUnconstrainedTVars, InstanceUnconstrainedTypeLocns,
            !RttiVarMaps),

        list.foldl(rtti_reuse_typeclass_info_var, UnivHeadTypeClassInfoVars,
            !RttiVarMaps),

        poly_info_set_rtti_varmaps(!.RttiVarMaps, !Info)
    ).

    % For class method implementations, do_call_class_method in
    % runtime/mercury_ho_call.c takes the type_infos and typeclass_infos
    % from the typeclass_info and pastes them onto the front of the
    % argument list. We need to match that order here.
    %
:- pred setup_instance_method_headvars(pred_info::in,
    instance_method_constraints::in, prog_constraints::out, list(tvar)::out,
    list(tvar)::out, list(prog_var)::out,
    proc_arg_vector(prog_var)::in, proc_arg_vector(prog_var)::out,
    poly_arg_vector(mer_mode)::in, poly_arg_vector(mer_mode)::out,
    poly_info::in, poly_info::out) is det.

setup_instance_method_headvars(PredInfo, InstanceMethodConstraints,
        ClassContext, InstanceTVars, InstanceUnconstrainedTVars,
        InstanceUnconstrainedTypeInfoVars, !HeadVars, !ExtraArgModes, !Info) :-
    InstanceMethodConstraints = instance_method_constraints(_,
        InstanceTypes, InstanceConstraints, ClassContext),

    type_vars_in_types(InstanceTypes, InstanceTVars),
    get_unconstrained_tvars(InstanceTVars, InstanceConstraints,
        InstanceUnconstrainedTVars),
    pred_info_get_arg_types(PredInfo, ArgTypeVarSet, _, _),
    make_head_vars(ArgTypeVarSet, InstanceUnconstrainedTVars,
        InstanceUnconstrainedTypeInfoVars, !Info),
    make_typeclass_info_head_vars(do_record_type_info_locns,
        InstanceConstraints, InstanceHeadTypeClassInfoVars, !Info),

    proc_arg_vector_set_instance_type_infos(InstanceUnconstrainedTypeInfoVars,
        !HeadVars),
    proc_arg_vector_set_instance_typeclass_infos(InstanceHeadTypeClassInfoVars,
         !HeadVars),

    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    list.foldl(rtti_reuse_typeclass_info_var,
        InstanceHeadTypeClassInfoVars, RttiVarMaps0, RttiVarMaps),
    poly_info_set_rtti_varmaps(RttiVarMaps, !Info),

    in_mode(InMode),
    list.duplicate(list.length(InstanceUnconstrainedTypeInfoVars),
        InMode, InstanceUnconstrainedTypeInfoModes),
    list.duplicate(list.length(InstanceHeadTypeClassInfoVars),
        InMode, InstanceHeadTypeClassInfoModes),
    poly_arg_vector_set_instance_type_infos(
        InstanceUnconstrainedTypeInfoModes, !ExtraArgModes),
    poly_arg_vector_set_instance_typeclass_infos(
        InstanceHeadTypeClassInfoModes, !ExtraArgModes).

%---------------------------------------------------------------------------%

:- pred make_head_vars(tvarset::in, list(tvar)::in, list(prog_var)::out,
    poly_info::in, poly_info::out) is det.

make_head_vars(_, [], [], !Info).
make_head_vars(TypeVarSet, [TypeVar | TypeVars], [TypeInfoVar | TypeInfoVars],
        !Info) :-
    poly_info_get_tvar_kind_map(!.Info, TVarKindMap),
    get_tvar_kind(TVarKindMap, TypeVar, Kind),
    Type = type_variable(TypeVar, Kind),
    new_type_info_var(Type, type_info, TypeInfoVar, !Info),
    ( if varset.search_name(TypeVarSet, TypeVar, TypeVarName) then
        VarName = "TypeInfo_for_" ++ TypeVarName,
        poly_info_get_var_table(!.Info, VarTable0),
        update_var_name(TypeInfoVar, VarName, VarTable0, VarTable),
        poly_info_set_var_table(VarTable, !Info)
    else
        true
    ),
    make_head_vars(TypeVarSet, TypeVars, TypeInfoVars, !Info).

:- pred var_as_type_info_locn(prog_var::in, type_info_locn::out) is det.

var_as_type_info_locn(Var, type_info(Var)).

:- pred get_improved_exists_head_constraints(constraint_map::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

get_improved_exists_head_constraints(ConstraintMap,  ExistConstraints,
        ActualExistConstraints) :-
    list.length(ExistConstraints, NumExistConstraints),
    ( if
        search_hlds_constraint_list(ConstraintMap, unproven,
            goal_id_for_head_constraints, NumExistConstraints,
            ActualExistConstraintsPrime)
    then
        ActualExistConstraints = ActualExistConstraintsPrime
    else
        % Some predicates, for example typeclass methods and predicates for
        % which we inferred the type, don't have constraint map entries for
        % the head constraints. In these cases we can just use the external
        % constraints, since there can't be any difference between them and
        % the internal ones.
        ActualExistConstraints = ExistConstraints
    ).

%---------------------------------------------------------------------------%

:- pred polymorphism_process_clause(pred_info::in,
    proc_arg_vector(prog_var)::in, proc_arg_vector(prog_var)::in,
    list(tvar)::in, list(prog_var)::in, list(prog_var)::in,
    clause::in, clause::out, poly_info::in, poly_info::out) is det.

polymorphism_process_clause(PredInfo0, OldHeadVars, NewHeadVars,
        UnconstrainedTVars, ExtraTypeInfoHeadVars,
        ExistTypeClassInfoHeadVars, !Clause, !Info) :-
    Goal0 = !.Clause ^ clause_body,

    % Process any polymorphic calls inside the goal.
    empty_cache_maps(!Info),
    poly_info_set_num_reuses(0, !Info),
    polymorphism_process_goal(Goal0, Goal1, !Info),

    % Generate code to construct the typeclass_infos and type_infos
    % for existentially quantified type vars.
    produce_clause_existq_tvars(PredInfo0, OldHeadVars,
        UnconstrainedTVars, ExtraTypeInfoHeadVars,
        ExistTypeClassInfoHeadVars, Goal1, Goal2, !Info),

    pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars),
    requantify_clause_goal_if_needed(NewHeadVars, ExistQVars,
        Goal2, Goal, !Info),
    !Clause ^ clause_body := Goal.

    % Generate code to produce the values of type_infos and typeclass_infos
    % for existentially quantified type variables in the head.
    %
    % XXX The following code ought to be rewritten to handle
    % existential/universal type_infos and type_class_infos
    % in a more consistent manner.
    %
:- pred produce_clause_existq_tvars(pred_info::in,
    proc_arg_vector(prog_var)::in, list(tvar)::in,
    list(prog_var)::in, list(prog_var)::in, hlds_goal::in, hlds_goal::out,
    poly_info::in, poly_info::out) is det.

produce_clause_existq_tvars(PredInfo, HeadVars, UnconstrainedTVars,
        TypeInfoHeadVars, ExistTypeClassInfoHeadVars, Goal0, Goal, !Info) :-
    poly_info_get_var_table(!.Info, VarTable0),
    poly_info_get_constraint_map(!.Info, ConstraintMap),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    pred_info_get_tvar_kind_map(PredInfo, KindMap),
    pred_info_get_class_context(PredInfo, PredClassContext),

    % Generate code to produce values for any existentially quantified
    % typeclass_info variables in the head.
    PredExistConstraints = PredClassContext ^ exist_constraints,
    get_improved_exists_head_constraints(ConstraintMap, PredExistConstraints,
        ActualExistConstraints),
    ExistQVarsForCall = [],
    Goal0 = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    make_typeclass_info_vars(ActualExistConstraints, ExistQVarsForCall,
        Context, ExistTypeClassVarsMCAs, ExtraTypeClassGoals, !Info),
    assoc_list.keys(ExistTypeClassVarsMCAs, ExistTypeClassVars),
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    list.foldl(rtti_reuse_typeclass_info_var, ExistTypeClassVars,
        RttiVarMaps0, RttiVarMaps),
    poly_info_set_rtti_varmaps(RttiVarMaps, !Info),
    make_complicated_unify_assigns(ExistTypeClassInfoHeadVars,
        ExistTypeClassVars, ExtraTypeClassUnifyGoals),

    % Figure out the bindings for any unconstrained existentially quantified
    % type variables in the head.
    var_table_count(VarTable0, NumVarsInDb0),
    ( if
        NumVarsInDb0 = 0
    then
        % This can happen for compiler generated procedures.
        map.init(PredToActualTypeSubst)
    else if
        HeadVarList = proc_arg_vector_to_list(HeadVars),
        lookup_var_types(VarTable0, HeadVarList, ActualArgTypes),
        type_list_subsumes(ArgTypes, ActualArgTypes, ArgTypeSubst)
    then
        PredToActualTypeSubst = ArgTypeSubst
    else
        % This can happen for unification procedures of equivalence types
        % error("polymorphism.m: type_list_subsumes failed")
        map.init(PredToActualTypeSubst)
    ),

    % Apply the type bindings to the unconstrained type variables to give
    % the actual types, and then generate code to initialize the type_infos
    % for those types.
    apply_subst_to_tvar_list(KindMap, PredToActualTypeSubst,
        UnconstrainedTVars, ActualTypes),
    polymorphism_do_make_type_info_vars(ActualTypes, Context,
        TypeInfoVarsMCAs, ExtraTypeInfoGoals, !Info),
    assoc_list.keys(TypeInfoVarsMCAs, TypeInfoVars),
    make_complicated_unify_assigns(TypeInfoHeadVars, TypeInfoVars,
        ExtraTypeInfoUnifyGoals),
    list.condense([[Goal0 | ExtraTypeClassGoals], ExtraTypeClassUnifyGoals,
        ExtraTypeInfoGoals, ExtraTypeInfoUnifyGoals], GoalList),
    conj_list_to_goal(GoalList, GoalInfo, Goal).

    % If the pred we are processing is a polymorphic predicate, or contains
    % polymorphically-typed goals, we may need to recompute the set of
    % nonlocals variables of each goal so that it includes the extra type_info
    % variables and typeclass_info variables that we added to the headvars,
    % or to the arguments of existentially typed predicate calls,
    % function calls and deconstruction unifications.
    %
    % Type(class)-infos added for ground types passed to predicate calls,
    % function calls and existentially typed construction unifications
    % do not require requantification because they are local to the conjunction
    % containing the type(class)-info construction and the goal which uses the
    % type(class)-info. The nonlocals for those goals are adjusted by the code
    % which creates/alters them. However, reusing a type_info changes it
    % from being local to nonlocal.
    %
:- pred requantify_clause_goal_if_needed(proc_arg_vector(prog_var)::in,
    existq_tvars::in, hlds_goal::in, hlds_goal::out,
    poly_info::in, poly_info::out) is det.

requantify_clause_goal_if_needed(HeadVars, ExistQVars, Goal0, Goal, !Info) :-
    ( if
        % Optimize a common case.
        ExistQVars = [],
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        rtti_varmaps_no_tvars(RttiVarMaps0),
        poly_info_get_num_reuses(!.Info, NumReuses),
        NumReuses = 0,
        poly_info_get_must_requantify(!.Info, MustRequantify),
        MustRequantify = no_must_requantify
    then
        Goal = Goal0
    else
        poly_info_get_var_table(!.Info, VarTable0),
        poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        OutsideVars = proc_arg_vector_to_set(HeadVars),
        implicitly_quantify_goal_general(ord_nl_maybe_lambda,
            set_to_bitset(OutsideVars), _Warnings, Goal0, Goal,
            VarTable0, VarTable, RttiVarMaps0, RttiVarMaps),
        poly_info_set_var_table_rtti(VarTable, RttiVarMaps, !Info)
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.polymorphism_clause.
%---------------------------------------------------------------------------%
