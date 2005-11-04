%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: typeclasses.m
% Main author: mark (including code by fjh and dgj)
%
% The module implements context reduction, which is the part of type checking
% which implements the type class system.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module check_hlds.typeclasses.

:- interface.

:- import_module check_hlds.typecheck_info.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.

:- import_module io.

    % perform_context_reduction(OrigTypeAssignSet, !Info) is true
    % iff either
    % (a) !:Info is the typecheck_info that results from performing
    % context reduction on the type_assigns in !.Info, or
    % (b) if there is no valid context reduction, then !:Info is !.Info
    % with the type assign set replaced by OrigTypeAssignSet (see below).
    %
    % Context reduction is the process of eliminating redundant constraints
    % from the constraints in the type_assign and adding the proof of the
    % constraint's redundancy to the proofs in the same type_assign. There
    % are three ways in which a constraint may be redundant:
    %
    % - if a constraint occurs in the pred/func declaration for this
    %   predicate or function, then it is redundant
    %   (in this case, the proof is trivial, so there is no need
    %   to record it in the proof map)
    % - if a constraint is present in the set of constraints and all
    %   of the "superclass" constraints for the constraints are all
    %   present, then all the superclass constraints are eliminated
    % - if there is an instance declaration that may be applied, the
    %   constraint is replaced by the constraints from that instance
    %   declaration
    %
    % In addition, context reduction removes repeated constraints.
    %
    % During context reduction we also try to "improve" the type binding
    % in the given type_assign (that is, binding the type variables in
    % such a way that the satisfiability of the constraints is not
    % changed).  This is done by applying improvement rules inside the
    % fixpoint loop.  The improvement rules are those which are induced
    % by functional dependencies attached to typeclass declarations.
    %
    % If context reduction fails on a type_assign, that type_assign is
    % removed from the type_assign_set. Context reduction fails if there is
    % a constraint where the type of (at least) one of the arguments to
    % the constraint has its top level functor bound, but there is no
    % instance declaration for that type.
    %
    % If all type_assigns from the typecheck_info are rejected, than an
    % appropriate error message is given, the type_assign_set is
    % restored to the original one given by OrigTypeAssignSet,
    % but without any typeclass constraints.
    % The reason for this is to avoid reporting the same error at
    % subsequent calls to perform_context_reduction.
    %
:- pred perform_context_reduction(type_assign_set::in,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

    % Apply context reduction to the list of class constraints by applying
    % the instance rules or superclass rules, building up proofs for
    % redundant constraints.
    %
:- pred reduce_context_by_rule_application(class_table::in,
    instance_table::in, superclass_table::in, head_type_params::in,
    tsubst::in, tsubst::out, tvarset::in, tvarset::out,
    constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out,
    hlds_constraints::in, hlds_constraints::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module check_hlds.typecheck_errors.
:- import_module hlds.hlds_module.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module multi_map.
:- import_module set.
:- import_module std_util.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

perform_context_reduction(OrigTypeAssignSet, !Info, !IO) :-
    checkpoint("before context reduction", !Info, !IO),
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_class_table(ModuleInfo, ClassTable),
    module_info_get_superclass_table(ModuleInfo, SuperClassTable),
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
    list__filter_map(
        reduce_type_assign_context(ClassTable, SuperClassTable, InstanceTable),
        TypeAssignSet0, TypeAssignSet),
    (
        % Check that this context reduction hasn't eliminated
        % all the type assignments.
        TypeAssignSet0 = [_ | _],
        TypeAssignSet = []
    ->
        report_unsatisfiable_constraints(TypeAssignSet0, !Info, !IO),
        DeleteConstraints = (pred(TA0::in, TA::out) is det :-
            type_assign_get_typeclass_constraints(TA0, Constraints0),
            Constraints = (Constraints0
                    ^ unproven := [])
                    ^ redundant := multi_map.init,
            type_assign_set_typeclass_constraints(Constraints, TA0, TA)
        ),
        list__map(DeleteConstraints, OrigTypeAssignSet, NewTypeAssignSet),
        typecheck_info_set_type_assign_set(NewTypeAssignSet, !Info)
    ;
        typecheck_info_set_type_assign_set(TypeAssignSet, !Info)
    ).

:- pred reduce_type_assign_context(class_table::in, superclass_table::in,
    instance_table::in, type_assign::in, type_assign::out) is semidet.

reduce_type_assign_context(ClassTable, SuperClassTable, InstanceTable,
        !TypeAssign) :-
    type_assign_get_head_type_params(!.TypeAssign, HeadTypeParams),
    type_assign_get_type_bindings(!.TypeAssign, Bindings0),
    type_assign_get_typeclass_constraints(!.TypeAssign, Constraints0),
    type_assign_get_typevarset(!.TypeAssign, TVarSet0),
    type_assign_get_constraint_proofs(!.TypeAssign, Proofs0),
    type_assign_get_constraint_map(!.TypeAssign, ConstraintMap0),

    typeclasses__reduce_context_by_rule_application(ClassTable,
        InstanceTable, SuperClassTable, HeadTypeParams,
        Bindings0, Bindings, TVarSet0, TVarSet, Proofs0, Proofs,
        ConstraintMap0, ConstraintMap, Constraints0, Constraints),
    check_satisfiability(Constraints ^ unproven, HeadTypeParams),

    type_assign_set_type_bindings(Bindings, !TypeAssign),
    type_assign_set_typeclass_constraints(Constraints, !TypeAssign),
    type_assign_set_typevarset(TVarSet, !TypeAssign),
    type_assign_set_constraint_proofs(Proofs, !TypeAssign),
    type_assign_set_constraint_map(ConstraintMap, !TypeAssign).

typeclasses__reduce_context_by_rule_application(ClassTable, InstanceTable,
        SuperClassTable, HeadTypeParams, !Bindings, !TVarSet, !Proofs,
        !ConstraintMap, !Constraints) :-
    typeclasses__reduce_context_by_rule_application_2(ClassTable,
        InstanceTable, SuperClassTable, HeadTypeParams, !Bindings,
        !TVarSet, !Proofs, !ConstraintMap, !Constraints,
        !.Constraints ^ unproven, _).

:- pred typeclasses__reduce_context_by_rule_application_2(class_table::in,
    instance_table::in, superclass_table::in, head_type_params::in,
    tsubst::in, tsubst::out, tvarset::in, tvarset::out,
    constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out,
    hlds_constraints::in, hlds_constraints::out,
    list(hlds_constraint)::in, list(hlds_constraint)::out) is det.

typeclasses__reduce_context_by_rule_application_2(ClassTable, InstanceTable,
        SuperClassTable, HeadTypeParams, !Bindings, !TVarSet, !Proofs,
        !ConstraintMap, !Constraints, !Seen) :-
    apply_rec_subst_to_constraints(!.Bindings, !Constraints),
    apply_improvement_rules(ClassTable, InstanceTable, HeadTypeParams,
        !.Constraints, !TVarSet, !Bindings, AppliedImprovementRule),

    % We want to make sure that any changes to the bindings are reflected
    % in the constraints, so that the full effect of the improvement rules
    % applies as soon as possible. We therefore apply the bindings to the
    % constraints (but only if the bindings have actually changed since
    % they were last applied).
    (
        AppliedImprovementRule = yes,
        apply_rec_subst_to_constraints(!.Bindings, !Constraints)
    ;
        AppliedImprovementRule = no
    ),

    eliminate_assumed_constraints(!ConstraintMap, !Constraints,
        EliminatedAssumed),
    apply_instance_rules(ClassTable, InstanceTable, !TVarSet, !Proofs,
        !ConstraintMap, !Seen, !Constraints, AppliedInstanceRule),
    % XXX Kind inference: we assume that all tvars have kind `star'.
    map__init(KindMap),
    apply_class_rules(SuperClassTable, !.TVarSet, KindMap, !Proofs,
        !ConstraintMap, !Constraints, AppliedClassRule),
    (
        AppliedImprovementRule = no,
        EliminatedAssumed = no,
        AppliedInstanceRule = no,
        AppliedClassRule = no
    ->
        % We have reached fixpoint.
        sort_and_merge_dups(!Constraints)
    ;
        typeclasses__reduce_context_by_rule_application_2(ClassTable,
            InstanceTable, SuperClassTable, HeadTypeParams, !Bindings,
            !TVarSet, !Proofs, !ConstraintMap, !Constraints, !Seen)
    ).

:- pred sort_and_merge_dups(hlds_constraints::in, hlds_constraints::out)
    is det.

sort_and_merge_dups(!Constraints) :-
    % Should we also sort and merge the other fields?
    Unproven0 = !.Constraints ^ unproven,
    list__sort(compare_hlds_constraints, Unproven0, Unproven1),
    merge_adjacent_constraints(Unproven1, Unproven),
    !:Constraints = !.Constraints ^ unproven := Unproven.

:- pred merge_adjacent_constraints(list(hlds_constraint)::in,
    list(hlds_constraint)::out) is det.

merge_adjacent_constraints([], []).
merge_adjacent_constraints([C | Cs], Constraints) :-
    merge_adjacent_constraints_2(C, Cs, Constraints).

:- pred merge_adjacent_constraints_2(hlds_constraint::in,
    list(hlds_constraint)::in, list(hlds_constraint)::out) is det.

merge_adjacent_constraints_2(C0, [], [C0]).
merge_adjacent_constraints_2(C0, [C1 | Cs], Constraints) :-
    ( merge_constraints(C0, C1, C) ->
        merge_adjacent_constraints_2(C, Cs, Constraints)
    ;
        merge_adjacent_constraints_2(C1, Cs, Constraints0),
        Constraints = [C0 | Constraints0]
    ).

    % merge_constraints(A, B, C) succeeds if A and B represent equivalent
    % constraints. In this case, C is the equivalent constraint with the
    % list of ids being the union of the ids of A and B.
    %
:- pred merge_constraints(hlds_constraint::in, hlds_constraint::in,
    hlds_constraint::out) is semidet.

merge_constraints(constraint(IdsA, Name, Types), constraint(IdsB, Name, Types),
        constraint(Ids, Name, Types)) :-
    list__append(IdsA, IdsB, Ids0),
    list__sort_and_remove_dups(Ids0, Ids).

:- pred apply_improvement_rules(class_table::in, instance_table::in,
    head_type_params::in, hlds_constraints::in, tvarset::in, tvarset::out,
    tsubst::in, tsubst::out, bool::out) is det.

apply_improvement_rules(ClassTable, InstanceTable, HeadTypeParams, Constraints,
        !TVarSet, !Bindings, Changed) :-
    % XXX Should we sort and merge the constraints here?
    do_class_improvement(ClassTable, HeadTypeParams, Constraints, !Bindings,
        Changed1),
    % XXX Do we really need to modify the varset? See the comment above
    % find_matching_instance_rule.
    do_instance_improvement(ClassTable, InstanceTable, HeadTypeParams,
        Constraints, !TVarSet, !Bindings, Changed2),
    Changed = bool__or(Changed1, Changed2).

:- pred do_class_improvement(class_table::in, head_type_params::in,
    hlds_constraints::in, tsubst::in, tsubst::out, bool::out) is det.

do_class_improvement(ClassTable, HeadTypeParams, Constraints, !Bindings,
        Changed) :-
    Redundant = Constraints ^ redundant,
    Assumed = Constraints ^ assumed,
    multi_map__keys(Redundant, ClassIds),
    list__foldl2(
        do_class_improvement_2(ClassTable, HeadTypeParams, Redundant, Assumed),
        ClassIds, !Bindings, no, Changed).

:- pred do_class_improvement_2(class_table::in, head_type_params::in,
    redundant_constraints::in, list(hlds_constraint)::in, class_id::in,
    tsubst::in, tsubst::out, bool::in, bool::out) is det.

do_class_improvement_2(ClassTable, HeadTypeParams, RedundantConstraints,
        Assumed, ClassId, !Bindings, !Changed) :-
    map__lookup(ClassTable, ClassId, ClassDefn),
    FunDeps = ClassDefn ^ class_fundeps,
    map__lookup(RedundantConstraints, ClassId, Constraints),
    do_class_improvement_by_pairs(Constraints, FunDeps, HeadTypeParams,
        !Bindings, !Changed),
    list__filter(has_class_id(ClassId), Assumed, ThisClassAssumed),
    do_class_improvement_by_assumed(ThisClassAssumed, Constraints, FunDeps,
        HeadTypeParams, !Bindings, !Changed).

:- pred has_class_id(class_id::in, hlds_constraint::in) is semidet.

has_class_id(class_id(Name, Arity), constraint(_, Name, Args)) :-
    list__length(Args, Arity).

    % Try to find an opportunity for improvement for each (unordered)
    % pair of constraints from the list.
    %
:- pred do_class_improvement_by_pairs(list(hlds_constraint)::in,
    hlds_class_fundeps::in, head_type_params::in, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_class_improvement_by_pairs([], _, _, !Bindings, !Changed).
do_class_improvement_by_pairs([Constraint | Constraints], FunDeps,
        HeadTypeParams, !Bindings, !Changed) :-
    do_class_improvement_by_pairs_2(Constraint, Constraints, FunDeps,
        HeadTypeParams, !Bindings, !Changed),
    do_class_improvement_by_pairs(Constraints, FunDeps, HeadTypeParams,
        !Bindings, !Changed).

:- pred do_class_improvement_by_pairs_2(hlds_constraint::in,
    list(hlds_constraint)::in, hlds_class_fundeps::in, head_type_params::in,
    tsubst::in, tsubst::out, bool::in, bool::out) is det.

do_class_improvement_by_pairs_2(_, [], _, _, !Bindings, !Changed).
do_class_improvement_by_pairs_2(Constraint, [HeadConstraint | TailConstraints],
        FunDeps, HeadTypeParams, !Bindings, !Changed) :-
    do_class_improvement_pair(Constraint, HeadConstraint, FunDeps,
        HeadTypeParams, !Bindings, !Changed),
    do_class_improvement_by_pairs_2(Constraint, TailConstraints, FunDeps,
        HeadTypeParams, !Bindings, !Changed).

    % Try to find an opportunity for improvement for each pair of
    % constraints where one comes from the assumed constraints and the
    % other comes from the redundant constraints.
    %
:- pred do_class_improvement_by_assumed(list(hlds_constraint)::in,
    list(hlds_constraint)::in, hlds_class_fundeps::in, head_type_params::in,
    tsubst::in, tsubst::out, bool::in, bool::out) is det.

do_class_improvement_by_assumed(Assumed, Constraints, FunDeps, HeadTypeParams,
        !Bindings, !Changed) :-
    list__foldl2(
        do_class_improvement_by_assumed_2(Constraints, FunDeps,
            HeadTypeParams),
        Assumed, !Bindings, !Changed).

:- pred do_class_improvement_by_assumed_2(list(hlds_constraint)::in,
    hlds_class_fundeps::in, head_type_params::in, hlds_constraint::in,
    tsubst::in, tsubst::out, bool::in, bool::out) is det.

do_class_improvement_by_assumed_2([], _, _, _, !Bindings, !Changed).
do_class_improvement_by_assumed_2([Constraint | Constraints], FunDeps,
        HeadTypeParams, Assumed, !Bindings, !Changed) :-
    do_class_improvement_pair(Constraint, Assumed, FunDeps, HeadTypeParams,
        !Bindings, !Changed),
    do_class_improvement_by_assumed_2(Constraints, FunDeps, HeadTypeParams,
        Assumed, !Bindings, !Changed).

    % Try to find an opportunity for improvement for this pair of
    % constraints, using each fundep in turn.
    %
:- pred do_class_improvement_pair(hlds_constraint::in, hlds_constraint::in,
    hlds_class_fundeps::in, head_type_params::in, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_class_improvement_pair(_, _, [], _, !Bindings, !Changed).
do_class_improvement_pair(ConstraintA, ConstraintB, [FunDep | FunDeps],
        HeadTypeParams, !Bindings, !Changed) :-
    do_class_improvement_fundep(ConstraintA, ConstraintB, FunDep,
        HeadTypeParams, !Bindings, !Changed),
    do_class_improvement_pair(ConstraintA, ConstraintB, FunDeps,
        HeadTypeParams, !Bindings, !Changed).

:- pred do_class_improvement_fundep(hlds_constraint::in, hlds_constraint::in,
    hlds_class_fundep::in, head_type_params::in, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_class_improvement_fundep(ConstraintA, ConstraintB, FunDep, HeadTypeParams,
        !Bindings, !Changed) :-
    ConstraintA = constraint(_, _, TypesA),
    ConstraintB = constraint(_, _, TypesB),
    FunDep = fundep(Domain, Range),
    (
        % We already know that the name/arity of the constraints match,
        % since we have partitioned them already.
        lists_match_on_elements(Domain, TypesA, TypesB),
        \+ lists_match_on_elements(Range, TypesA, TypesB),

        % The unification can fail if type parameters in the declaration
        % would be bound by the improvement rule. This means that the
        % declaration is not as specific as it could be, but that is not
        % a problem for us.
        unify_on_elements(Range, TypesA, TypesB, HeadTypeParams, !Bindings)
    ->
        !:Changed = yes
    ;
        true
    ).

:- pred do_instance_improvement(class_table::in, instance_table::in,
    head_type_params::in, hlds_constraints::in, tvarset::in, tvarset::out,
    tsubst::in, tsubst::out, bool::out) is det.

do_instance_improvement(ClassTable, InstanceTable, HeadTypeParams, Constraints,
        !TVarSet, !Bindings, Changed) :-
    RedundantConstraints = Constraints ^ redundant,
    map__keys(RedundantConstraints, ClassIds),
    list__foldl3(
        do_instance_improvement_2(ClassTable, InstanceTable,
            HeadTypeParams, RedundantConstraints),
        ClassIds, !TVarSet, !Bindings, no, Changed).

:- pred do_instance_improvement_2(class_table::in, instance_table::in,
    head_type_params::in, redundant_constraints::in, class_id::in,
    tvarset::in, tvarset::out, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_instance_improvement_2(ClassTable, InstanceTable, HeadTypeParams,
        RedundantConstraints, ClassId, !TVarSet, !Bindings, !Changed) :-
    map__lookup(ClassTable, ClassId, ClassDefn),
    FunDeps = ClassDefn ^ class_fundeps,
    map__lookup(InstanceTable, ClassId, InstanceDefns),
    map__lookup(RedundantConstraints, ClassId, Constraints),
    list__foldl3(
        do_instance_improvement_3(Constraints, FunDeps, HeadTypeParams),
        InstanceDefns, !TVarSet, !Bindings, !Changed).

:- pred do_instance_improvement_3(list(hlds_constraint)::in,
    hlds_class_fundeps::in, head_type_params::in, hlds_instance_defn::in,
    tvarset::in, tvarset::out, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_instance_improvement_3(Constraints, FunDeps, HeadTypeParams, InstanceDefn,
        !TVarSet, !Bindings, !Changed) :-
    InstanceTVarSet = InstanceDefn ^ instance_tvarset,
    InstanceTypes0 = InstanceDefn ^ instance_types,
    tvarset_merge_renaming(!.TVarSet, InstanceTVarSet, NewTVarSet, Renaming),
    apply_variable_renaming_to_type_list(Renaming, InstanceTypes0,
        InstanceTypes),
    list__foldl2(
        do_instance_improvement_4(FunDeps, InstanceTypes, HeadTypeParams),
        Constraints, !Bindings, no, Changed0),
    (
        Changed0 = yes,
        !:TVarSet = NewTVarSet,
        !:Changed = yes
    ;
        Changed0 = no
    ).

:- pred do_instance_improvement_4(hlds_class_fundeps::in, list(mer_type)::in,
    head_type_params::in, hlds_constraint::in, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_instance_improvement_4(FunDeps, InstanceTypes, HeadTypeParams, Constraint,
        !Bindings, !Changed) :-
    list__foldl2(
        do_instance_improvement_fundep(Constraint, InstanceTypes,
            HeadTypeParams),
        FunDeps, !Bindings, !Changed).

:- pred do_instance_improvement_fundep(hlds_constraint::in, list(mer_type)::in,
    head_type_params::in, hlds_class_fundep::in, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_instance_improvement_fundep(Constraint, InstanceTypes0, HeadTypeParams,
        FunDep, !Bindings, !Changed) :-
    Constraint = constraint(_, _, ConstraintTypes),
    FunDep = fundep(Domain, Range),
    (
        % We already know that the name/arity of the constraints match,
        % since we have partitioned them already.
        subsumes_on_elements(Domain, InstanceTypes0, ConstraintTypes, Subst),
        apply_rec_subst_to_type_list(Subst, InstanceTypes0, InstanceTypes),
        \+ lists_match_on_elements(Range, InstanceTypes, ConstraintTypes),

        % The unification can fail if type parameters in the declaration
        % would be bound by the improvement rule. This means that the
        % declaration is not as specific as it could be, but that is not
        % a problem for us.
        unify_on_elements(Range, InstanceTypes, ConstraintTypes,
            HeadTypeParams, !Bindings)
    ->
        !:Changed = yes
    ;
        true
    ).

    % For each index in the set, check that the types in the corresponding
    % positions in the lists are identical.
    %
:- pred lists_match_on_elements(set(hlds_class_argpos)::in, list(mer_type)::in,
    list(mer_type)::in) is semidet.

lists_match_on_elements(Elements, TypesA, TypesB) :-
    RTypesA = restrict_list_elements(Elements, TypesA),
    RTypesB = restrict_list_elements(Elements, TypesB),
    RTypesA = RTypesB.

    % For each index in the set, unify the types in the corresponding
    % positions in the lists and add to the current bindings.
    %
:- pred unify_on_elements(set(hlds_class_argpos)::in, list(mer_type)::in,
    list(mer_type)::in, head_type_params::in, tsubst::in, tsubst::out)
    is semidet.

unify_on_elements(Elements, TypesA, TypesB, HeadTypeParams, !Bindings) :-
    RTypesA = restrict_list_elements(Elements, TypesA),
    RTypesB = restrict_list_elements(Elements, TypesB),
    type_unify_list(RTypesA, RTypesB, HeadTypeParams, !Bindings).

    % Analogous to type_list_subsumes except that it only checks those
    % elements of the list specified by the set of indices.
    %
:- pred subsumes_on_elements(set(hlds_class_argpos)::in, list(mer_type)::in,
    list(mer_type)::in, tsubst::out) is semidet.

subsumes_on_elements(Elements, TypesA, TypesB, Subst) :-
    RTypesA = restrict_list_elements(Elements, TypesA),
    RTypesB = restrict_list_elements(Elements, TypesB),
    prog_type__vars_list(RTypesB, RTypesBVars),
    map__init(Subst0),
    type_unify_list(RTypesA, RTypesB, RTypesBVars, Subst0, Subst).

:- pred eliminate_assumed_constraints(constraint_map::in, constraint_map::out,
    hlds_constraints::in, hlds_constraints::out, bool::out) is det.

eliminate_assumed_constraints(!ConstraintMap, !Constraints, Changed) :-
    !.Constraints = constraints(Unproven0, Assumed, Redundant),
    eliminate_assumed_constraints_2(Assumed, !ConstraintMap,
        Unproven0, Unproven, Changed),
    !:Constraints = constraints(Unproven, Assumed, Redundant).

:- pred eliminate_assumed_constraints_2(list(hlds_constraint)::in,
    constraint_map::in, constraint_map::out,
    list(hlds_constraint)::in, list(hlds_constraint)::out,
    bool::out) is det.

eliminate_assumed_constraints_2(_, !ConstraintMap, [], [], no).
eliminate_assumed_constraints_2(AssumedCs, !ConstraintMap, [C | Cs], NewCs,
        Changed) :-
    eliminate_assumed_constraints_2(AssumedCs, !ConstraintMap, Cs, NewCs0,
        Changed0),
    (
        some [A] (
            list__member(A, AssumedCs),
            matching_constraints(A, C)
        )
    ->
        update_constraint_map(C, !ConstraintMap),
        NewCs = NewCs0,
        Changed = yes
    ;
        NewCs = [C | NewCs0],
        Changed = Changed0
    ).

:- pred apply_instance_rules(class_table::in, instance_table::in,
    tvarset::in, tvarset::out,
    constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out,
    list(hlds_constraint)::in, list(hlds_constraint)::out,
    hlds_constraints::in, hlds_constraints::out, bool::out) is det.

apply_instance_rules(ClassTable, InstanceTable, !TVarSet, !Proofs,
        !ConstraintMap, !Seen, !Constraints, Changed) :-
    !.Constraints = constraints(Unproven0, Assumed, Redundant0),
    apply_instance_rules_2(ClassTable, InstanceTable, !TVarSet, !Proofs,
        !ConstraintMap, Redundant0, Redundant, !Seen,
        Unproven0, Unproven, Changed),
    !:Constraints = constraints(Unproven, Assumed, Redundant).

:- pred apply_instance_rules_2(class_table::in, instance_table::in,
    tvarset::in, tvarset::out,
    constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out,
    redundant_constraints::in, redundant_constraints::out,
    list(hlds_constraint)::in, list(hlds_constraint)::out,
    list(hlds_constraint)::in, list(hlds_constraint)::out, bool::out) is det.

apply_instance_rules_2(_, _, !TVarSet, !Proofs, !ConstraintMap, !Redundant,
        !Seen, [], [], no).
apply_instance_rules_2(ClassTable, InstanceTable, !TVarSet, !Proofs,
        !ConstraintMap, !Redundant, !Seen, [C | Cs], Constraints, Changed) :-
    C = constraint(_, ClassName, Types),
    list__length(Types, Arity),
    map__lookup(InstanceTable, class_id(ClassName, Arity), Instances),
    InitialTVarSet = !.TVarSet,
    (
        find_matching_instance_rule(Instances, C, !TVarSet, !Proofs,
            NewConstraints0)
    ->
        update_constraint_map(C, !ConstraintMap),
        % Remove any constraints we've already seen.
        % This ensures we don't get into an infinite loop.
        list__filter(matches_no_constraint(!.Seen), NewConstraints0,
            NewConstraints),
        update_redundant_constraints(ClassTable, !.TVarSet,
            NewConstraints, !Redundant),
        % Put the new constraints at the front of the list.
        !:Seen = NewConstraints ++ !.Seen,
        Changed1 = yes
    ;
        % Put the old constraint at the front of the list.
        NewConstraints = [C],
        !:TVarSet = InitialTVarSet,
        Changed1 = no
    ),
    apply_instance_rules_2(ClassTable, InstanceTable, !TVarSet, !Proofs,
        !ConstraintMap, !Redundant, !Seen, Cs, TailConstraints, Changed2),
    bool__or(Changed1, Changed2, Changed),
    list__append(NewConstraints, TailConstraints, Constraints).

:- pred matches_no_constraint(list(hlds_constraint)::in, hlds_constraint::in)
    is semidet.

matches_no_constraint(Seen, Constraint) :-
    \+ ( some [S] (
        list__member(S, Seen),
        matching_constraints(S, Constraint)
    )).

    % We take the first matching instance rule that we can find; any
    % overlapping instance declarations will have been caught earlier.
    %
    % This pred also catches tautological constraints since the
    % NewConstraints will be [].
    %
    % XXX Surely we shouldn't need to rename the variables and return
    % a new varset: this substitution should have been worked out before,
    % as these varsets would already have been merged.
    %
:- pred find_matching_instance_rule(list(hlds_instance_defn)::in,
    hlds_constraint::in, tvarset::in, tvarset::out,
    constraint_proof_map::in, constraint_proof_map::out,
    list(hlds_constraint)::out) is semidet.

find_matching_instance_rule(Instances, Constraint, !TVarSet, !Proofs,
        NewConstraints) :-
    % Start a counter so we remember which instance decl we have used.
    find_matching_instance_rule_2(Instances, 1, Constraint, !TVarSet,
        !Proofs, NewConstraints).

:- pred find_matching_instance_rule_2(list(hlds_instance_defn)::in, int::in,
    hlds_constraint::in, tvarset::in, tvarset::out,
    constraint_proof_map::in, constraint_proof_map::out,
    list(hlds_constraint)::out) is semidet.

find_matching_instance_rule_2([Instance | Instances], InstanceNum0, Constraint,
        !TVarSet, !Proofs, NewConstraints) :-
    Constraint = constraint(_Ids, _Name, Types),
    ProgConstraints0 = Instance ^ instance_constraints,
    InstanceTypes0 = Instance ^ instance_types,
    InstanceTVarSet = Instance ^ instance_tvarset,
    tvarset_merge_renaming(!.TVarSet, InstanceTVarSet, NewTVarSet, Renaming),
    apply_variable_renaming_to_type_list(Renaming, InstanceTypes0,
        InstanceTypes),
    (
        type_list_subsumes(InstanceTypes, Types, Subst)
    ->
        !:TVarSet = NewTVarSet,
        apply_variable_renaming_to_prog_constraint_list(Renaming,
            ProgConstraints0, ProgConstraints1),
        apply_rec_subst_to_prog_constraint_list(Subst,
            ProgConstraints1, ProgConstraints),
        init_hlds_constraint_list(ProgConstraints, NewConstraints),

        NewProof = apply_instance(InstanceNum0),
        retrieve_prog_constraint(Constraint, ProgConstraint),
        map__set(!.Proofs, ProgConstraint, NewProof, !:Proofs)
    ;
        InstanceNum = InstanceNum0 + 1,
        find_matching_instance_rule_2(Instances, InstanceNum,
            Constraint, !TVarSet, !Proofs, NewConstraints)
    ).

    % To reduce a constraint using class declarations, we search the
    % superclass relation to find a path from the inferred constraint to
    % another (declared or inferred) constraint.
    %
:- pred apply_class_rules(superclass_table::in, tvarset::in, tvar_kind_map::in,
    constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out,
    hlds_constraints::in, hlds_constraints::out, bool::out) is det.

apply_class_rules(SuperClassTable, TVarSet, KindMap, !Proofs, !ConstraintMap,
        !Constraints, Changed) :-
    !.Constraints = constraints(Unproven0, Assumed, _),
    apply_class_rules_2(Assumed, SuperClassTable, TVarSet, KindMap,
        !Proofs, !ConstraintMap, Unproven0, Unproven, Changed),
    !:Constraints = !.Constraints ^ unproven := Unproven.

:- pred apply_class_rules_2(list(hlds_constraint)::in, superclass_table::in,
    tvarset::in, tvar_kind_map::in, constraint_proof_map::in,
    constraint_proof_map::out, constraint_map::in, constraint_map::out,
    list(hlds_constraint)::in, list(hlds_constraint)::out,
    bool::out) is det.

apply_class_rules_2(_, _, _, _, !Proofs, !ConstraintMap, [], [], no).
apply_class_rules_2(AssumedConstraints, SuperClassTable, TVarSet, KindMap,
        !Proofs, !ConstraintMap, [Constraint0 | Constraints0],
        Constraints, Changed) :-
    Parents = [],
    retrieve_prog_constraint(Constraint0, ProgConstraint0),

    % The head_type_params argument contains all the variables from the
    % original constraint that we are trying to prove. (These are the type
    % variables that must not be bound as we search through the superclass
    % relation).
    constraint_get_tvars(ProgConstraint0, HeadTypeParams),
    (
        eliminate_constraint_by_class_rules(ProgConstraint0, _, _,
            AssumedConstraints, SuperClassTable, HeadTypeParams,
            TVarSet, KindMap, Parents, !Proofs)
    ->
        update_constraint_map(Constraint0, !ConstraintMap),
        apply_class_rules_2(AssumedConstraints, SuperClassTable,
            TVarSet, KindMap, !Proofs, !ConstraintMap,
            Constraints0, Constraints, _),
        Changed = yes
    ;
        apply_class_rules_2(AssumedConstraints, SuperClassTable,
            TVarSet, KindMap, !Proofs, !ConstraintMap,
            Constraints0, TailConstraints, Changed),
        Constraints = [Constraint0 | TailConstraints]
    ).

    % eliminate_constraint_by_class_rules eliminates a class constraint
    % by applying the superclass relation. A list of "parent" constraints
    % is also passed in --- these are the constraints that we are
    % (recursively) in the process of checking, and is used to ensure that
    % we don't get into a cycle in the relation.
    %
:- pred eliminate_constraint_by_class_rules(prog_constraint::in,
    prog_constraint::out, tsubst::out, list(hlds_constraint)::in,
    superclass_table::in, head_type_params::in, tvarset::in,
    tvar_kind_map::in, list(prog_constraint)::in,
    constraint_proof_map::in, constraint_proof_map::out) is semidet.

eliminate_constraint_by_class_rules(C, SubstC, SubClassSubst,
        AssumedConstraints, SuperClassTable, HeadTypeParams, TVarSet,
        KindMap, ParentConstraints, Proofs0, Proofs) :-

    % Make sure we aren't in a cycle in the superclass relation.
    \+ list__member(C, ParentConstraints),

    C = constraint(SuperClassName, SuperClassTypes),
    list__length(SuperClassTypes, SuperClassArity),
    SuperClassId = class_id(SuperClassName, SuperClassArity),
    multi_map__search(SuperClassTable, SuperClassId, SubClasses),

    % Convert all the subclass_details into prog_constraints by doing the
    % appropriate variable renaming and applying the type variable bindings.
    % If the unification of the type variables for a particular constraint
    % fails then that constraint is eliminated because it cannot contribute
    % to proving the constraint we are trying to prove.
    list__filter_map(
        subclass_details_to_constraint(TVarSet, KindMap, SuperClassTypes),
        SubClasses, SubClassConstraints),
    (
        % Do the first level of search. We search for an assumed constraint
        % which unifies with any of the subclass constraints.
        varset__vars(TVarSet, XXXHeadTypeParams),
        list.find_first_map(
            match_assumed_constraint(XXXHeadTypeParams, SubClassConstraints),
            AssumedConstraints, SubClass - SubClassSubst0)
    ->
        SubClassSubst = SubClassSubst0,
        apply_rec_subst_to_prog_constraint(SubClassSubst, C, SubstC),
        map__set(Proofs0, SubstC, superclass(SubClass), Proofs)
    ;
        NewParentConstraints = [C | ParentConstraints],

        % Recursively search the rest of the superclass relation.
        SubClassSearch = (pred(Constraint::in, CnstrtAndProof::out)
                is semidet :-
            eliminate_constraint_by_class_rules(Constraint,
                SubstConstraint, SubClassSubst0,
                AssumedConstraints, SuperClassTable,
                HeadTypeParams, TVarSet, KindMap,
                NewParentConstraints, Proofs0, SubProofs),
            CnstrtAndProof = {SubstConstraint, SubClassSubst0, SubProofs}
        ),
        % XXX this could (and should) be more efficient.
        % (i.e. by manually doing a "cut").
        find_first_map(SubClassSearch, SubClassConstraints,
            {NewSubClass, SubClassSubst, NewProofs}),
        apply_rec_subst_to_prog_constraint(SubClassSubst, C, SubstC),
        map__set(NewProofs, SubstC, superclass(NewSubClass), Proofs)
    ).

:- pred match_assumed_constraint(head_type_params::in,
    list(prog_constraint)::in, hlds_constraint::in,
    pair(prog_constraint, tsubst)::out) is semidet.

match_assumed_constraint(HeadTypeParams, SubClassConstraints,
        AssumedConstraint, Match) :-
    find_first_map(
        match_assumed_constraint_2(HeadTypeParams, AssumedConstraint),
        SubClassConstraints, Match).

:- pred match_assumed_constraint_2(head_type_params::in, hlds_constraint::in,
    prog_constraint::in, pair(prog_constraint, tsubst)::out) is semidet.

match_assumed_constraint_2(HeadTypeParams, AssumedConstraint,
        SubClassConstraint, Match) :-
    AssumedConstraint = constraint(_, AssumedConstraintClass,
        AssumedConstraintTypes),
    SubClassConstraint = constraint(AssumedConstraintClass,
        SubClassConstraintTypes),
    map__init(EmptySub),
    type_unify_list(SubClassConstraintTypes, AssumedConstraintTypes,
        HeadTypeParams, EmptySub, AssumedConstraintSub),
    retrieve_prog_constraint(AssumedConstraint, MatchingProgConstraint),
    Match = MatchingProgConstraint - AssumedConstraintSub.

    % subclass_details_to_constraint will fail iff the call to
    % type_unify_list fails.
    %
:- pred subclass_details_to_constraint(tvarset::in, tvar_kind_map::in,
    list(mer_type)::in, subclass_details::in, prog_constraint::out) is semidet.

subclass_details_to_constraint(TVarSet, KindMap0, SuperClassTypes,
        SubClassDetails, SubC) :-
    SubClassDetails = subclass_details(SuperVars0, SubID, SubVars0,
        SuperVarSet),

    % Rename the variables from the typeclass declaration into those
    % of the current pred.
    tvarset_merge_renaming(TVarSet, SuperVarSet, _NewTVarSet, Renaming),
    apply_variable_renaming_to_tvar_kind_map(Renaming, KindMap0, KindMap),
    apply_variable_renaming_to_tvar_list(Renaming, SubVars0, SubVars),
    apply_variable_renaming_to_type_list(Renaming, SuperVars0, SuperVars),

    % Work out what the (renamed) vars from the typeclass declaration
    % are bound to here.
    type_unify_list(SuperVars, SuperClassTypes, [], map__init, Bindings),
    SubID = class_id(SubName, _SubArity),
    apply_rec_subst_to_tvar_list(KindMap, Bindings, SubVars,
        SubClassTypes),
    SubC = constraint(SubName, SubClassTypes).

    % check_satisfiability(Constraints, HeadTypeParams):
    %
    % Check that all of the constraints are satisfiable. Fail if any are
    % definitely not satisfiable.
    %
    % We disallow ground constraints for which there are no matching instance
    % rules, even though the module system means that it would make sense
    % to allow them: even if there is no instance declaration visible
    % in the current module, there may be one visible in the caller. The reason
    % we disallow them is that in practice allowing this causes type inference
    % to let too many errors slip through, with the error diagnosis being
    % too far removed from the real cause of the error. Note that ground
    % constraints *are* allowed if you declare them, since we removed declared
    % constraints before checking satisfiability.
    %
    % Similarly, for constraints on head type params (universally quantified
    % type vars in this pred's type decl, or existentially quantified type vars
    % in type decls for callees), we know that the head type params can
    % never get bound. This means that if the constraint wasn't an assumed
    % constraint and can't be eliminated by instance rule or class rule
    % application, then we can report an error now, rather than later.
    % (For non-head-type-param type variables, we need to wait, in case
    % the type variable gets bound to a type for which there is a valid
    % instance declaration.)
    %
    % So a constraint is considered satisfiable iff it contains at least one
    % type variable that is not in the head type params.
    %
:- pred check_satisfiability(list(hlds_constraint)::in, head_type_params::in)
    is semidet.

check_satisfiability(Constraints, HeadTypeParams) :-
    all [Constraint] (
        list__member(Constraint, Constraints)
    =>
        (
            Constraint = constraint(_Ids, _ClassName, Types),
            type_list_contains_var(Types, TVar),
            not list__member(TVar, HeadTypeParams)
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.typeclasses.
%-----------------------------------------------------------------------------%
