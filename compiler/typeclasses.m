%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2009, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: typeclasses.m.
% Main author: mark (including code by fjh and dgj)
%
% The module implements context reduction, which is the part of type checking
% which implements the type class system.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.typeclasses.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_info.
:- import_module hlds.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

%-----------------------------------------------------------------------------%

    % perform_context_reduction(..., !Info) is true iff either
    % (a) !:Info is the typecheck_info that results from performing
    % context reduction on the type_assigns in !.Info, or
    % (b) if there is no valid context reduction, then an appropriate
    % error message is given. To avoid reporting the same error at
    % subsequent calls, !:Info is !.Info with all unproven constraints
    % removed from the type assign set.
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
    % such a way that the satisfiability of the constraints is not changed).
    % This is done by applying improvement rules inside the fixpoint loop.
    % The improvement rules are those which are induced by functional
    % dependencies attached to typeclass declarations.
    %
    % If context reduction fails on a type_assign, that type_assign is
    % removed from the type_assign_set. Context reduction fails if there is
    % a constraint where the type of (at least) one of the arguments to
    % the constraint has its top level functor bound, but there is no
    % instance declaration for that type.
    %
:- pred perform_context_reduction(prog_context::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

    % Apply context reduction to the list of class constraints by applying
    % the instance rules or superclass rules, building up proofs for
    % redundant constraints.
    %
:- pred reduce_context_by_rule_application(class_table::in, instance_table::in,
    external_type_params::in,
    tsubst::in, tsubst::out, tvarset::in, tvarset::out,
    constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out,
    hlds_constraints::in, hlds_constraints::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module check_hlds.typecheck_debug.
:- import_module check_hlds.typecheck_errors.
:- import_module hlds.hlds_module.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_unify.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

perform_context_reduction(Context, TypeAssignSet0, TypeAssignSet, !Info) :-
    typecheck_info_get_error_clause_context(!.Info, ClauseContext),
    ModuleInfo = ClauseContext ^ tecc_module_info,
    trace [compiletime(flag("type_checkpoint")), io(!IO)] (
        VarSet = ClauseContext ^ tecc_varset,
        type_checkpoint("before context reduction", !.Info, VarSet,
            TypeAssignSet0, !IO)
    ),
    module_info_get_class_table(ModuleInfo, ClassTable),
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    list.foldl2(
        reduce_type_assign_context(ClassTable, InstanceTable),
        TypeAssignSet0,
        cord.init, TypeAssignSetCord1, [], UnsatTypeAssignSet),
    TypeAssignSet1 = cord.list(TypeAssignSetCord1),
    ( if
        % Check that this context reduction hasn't eliminated
        % all the type assignments. Put the usually-failing test first.
        TypeAssignSet1 = [],
        TypeAssignSet0 = [_ | _]
    then
        Spec = report_unsatisfiable_constraints(ClauseContext, Context,
            UnsatTypeAssignSet),
        typecheck_info_add_error(Spec, !Info),
        DeleteConstraints =
            ( pred(TA0::in, TA::out) is det :-
                % Make a new hlds_constraints structure for the type assign,
                % with the same assumed constraints but all unproven
                % constraints deleted.
                type_assign_get_typeclass_constraints(TA0, Constraints0),
                type_assign_get_typevarset(TA0, TVarSet),
                make_hlds_constraints(ClassTable, TVarSet, [],
                    Constraints0 ^ hcs_assumed, Constraints),
                type_assign_set_typeclass_constraints(Constraints, TA0, TA)
            ),
        list.map(DeleteConstraints, TypeAssignSet0, TypeAssignSet)
    else
        TypeAssignSet = TypeAssignSet1
    ).

:- pred reduce_type_assign_context(class_table::in, instance_table::in,
    type_assign::in, cord(type_assign)::in, cord(type_assign)::out,
    list(type_assign)::in, list(type_assign)::out) is det.

reduce_type_assign_context(ClassTable, InstanceTable, !.TypeAssign,
        !TypeAssignCord, !UnsatTypeAssignSet) :-
    type_assign_get_typeclass_constraints(!.TypeAssign, Constraints0),
    ( if
        % Optimize the common case of no typeclass constraints at all.
        Constraints0 =
            hlds_constraints(Unproven0, Assumed0, Redundant0, Ancestors0),
        Unproven0 = [],
        Assumed0 = [],
        map.is_empty(Redundant0),
        map.is_empty(Ancestors0)
    then
        !:TypeAssignCord = cord.snoc(!.TypeAssignCord, !.TypeAssign)
    else
        type_assign_get_existq_tvars(!.TypeAssign, ExistQTVars),
        type_assign_get_typevarset(!.TypeAssign, TVarSet0),
        type_assign_get_type_bindings(!.TypeAssign, Bindings0),
        type_assign_get_constraint_proof_map(!.TypeAssign, ProofMap0),
        type_assign_get_constraint_map(!.TypeAssign, ConstraintMap0),

        reduce_context_by_rule_application(ClassTable, InstanceTable,
            ExistQTVars, Bindings0, Bindings, TVarSet0, TVarSet,
            ProofMap0, ProofMap, ConstraintMap0, ConstraintMap,
            Constraints0, Constraints),

        type_assign_set_reduce_results(TVarSet, Bindings, Constraints,
            ProofMap, ConstraintMap, !TypeAssign),

        Unproven = Constraints ^ hcs_unproven,
        ( if all_constraints_are_satisfiable(Unproven, ExistQTVars) then
            !:TypeAssignCord = cord.snoc(!.TypeAssignCord, !.TypeAssign)
        else
            % Remember the unsatisfiable type_assign_set so we can produce more
            % specific error messages.
            !:UnsatTypeAssignSet = [!.TypeAssign | !.UnsatTypeAssignSet]
        )
    ).

    % all_constraints_are_satisfiable(Constraints, ExistQTVars):
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
:- pred all_constraints_are_satisfiable(list(hlds_constraint)::in,
    list(tvar)::in) is semidet.

all_constraints_are_satisfiable([], _).
all_constraints_are_satisfiable([Constraint | Constraints], ExistQTVars) :-
    Constraint = hlds_constraint(_Ids, _ClassName, ArgTypes),
    some [TVar] (
        type_list_contains_var(ArgTypes, TVar),
        not list.member(TVar, ExistQTVars)
    ),
    all_constraints_are_satisfiable(Constraints, ExistQTVars).

reduce_context_by_rule_application(ClassTable, InstanceTable, ExistQTVars,
        !Bindings, !TVarSet, !ProofMap, !ConstraintMap, !Constraints) :-
    reduce_context_by_rule_application_2(ClassTable, InstanceTable,
        ExistQTVars, !Bindings, !TVarSet, !ProofMap, !ConstraintMap,
        !Constraints, !.Constraints ^ hcs_unproven, _).

:- pred reduce_context_by_rule_application_2(class_table::in,
    instance_table::in, external_type_params::in,
    tsubst::in, tsubst::out, tvarset::in, tvarset::out,
    constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out,
    hlds_constraints::in, hlds_constraints::out,
    list(hlds_constraint)::in, list(hlds_constraint)::out) is det.

reduce_context_by_rule_application_2(ClassTable, InstanceTable, ExistQTVars,
        !Bindings, !TVarSet, !ProofMap, !ConstraintMap, !Constraints, !Seen) :-
    apply_rec_subst_to_constraints(!.Bindings, !Constraints),
    apply_improvement_rules(ClassTable, InstanceTable, ExistQTVars,
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
    apply_instance_rules(ClassTable, InstanceTable, !TVarSet, !ProofMap,
        !ConstraintMap, !Seen, !Constraints, AppliedInstanceRule),
    apply_class_rules(!ProofMap, !ConstraintMap, !Constraints,
        AppliedClassRule),
    ( if
        AppliedImprovementRule = no,
        EliminatedAssumed = no,
        AppliedInstanceRule = no,
        AppliedClassRule = no
    then
        % We have reached fixpoint.
        sort_and_merge_dups(!Constraints)
    else
        disable_warning [suspicious_recursion] (
            reduce_context_by_rule_application_2(ClassTable, InstanceTable,
                ExistQTVars, !Bindings, !TVarSet, !ProofMap,
                !ConstraintMap, !Constraints, !Seen)
        )
    ).

:- pred sort_and_merge_dups(hlds_constraints::in, hlds_constraints::out)
    is det.

sort_and_merge_dups(!Constraints) :-
    % Should we also sort and merge the other fields?
    Unproven0 = !.Constraints ^ hcs_unproven,
    list.sort(compare_hlds_constraints, Unproven0, Unproven1),
    merge_adjacent_constraints(Unproven1, Unproven),
    !Constraints ^ hcs_unproven := Unproven.

:- pred merge_adjacent_constraints(list(hlds_constraint)::in,
    list(hlds_constraint)::out) is det.

merge_adjacent_constraints([], []).
merge_adjacent_constraints([C | Cs], Constraints) :-
    merge_adjacent_constraints_2(C, Cs, Constraints).

:- pred merge_adjacent_constraints_2(hlds_constraint::in,
    list(hlds_constraint)::in, list(hlds_constraint)::out) is det.

merge_adjacent_constraints_2(C0, [], [C0]).
merge_adjacent_constraints_2(C0, [C1 | Cs], Constraints) :-
    ( if merge_constraints(C0, C1, C) then
        merge_adjacent_constraints_2(C, Cs, Constraints)
    else
        merge_adjacent_constraints_2(C1, Cs, Constraints0),
        Constraints = [C0 | Constraints0]
    ).

    % merge_constraints(A, B, C) succeeds if A and B represent equivalent
    % constraints. In this case, C is the equivalent constraint with the
    % list of ids being the union of the ids of A and B.
    %
:- pred merge_constraints(hlds_constraint::in, hlds_constraint::in,
    hlds_constraint::out) is semidet.

merge_constraints(ConstraintA, ConstraintB, Constraint) :-
    ConstraintA = hlds_constraint(IdsA, ClassName, ArgTypes),
    ConstraintB = hlds_constraint(IdsB, ClassName, ArgTypes),
    list.append(IdsA, IdsB, Ids0),
    list.sort_and_remove_dups(Ids0, Ids),
    Constraint = hlds_constraint(Ids, ClassName, ArgTypes).

:- pred apply_improvement_rules(class_table::in, instance_table::in,
    external_type_params::in, hlds_constraints::in, tvarset::in, tvarset::out,
    tsubst::in, tsubst::out, bool::out) is det.

apply_improvement_rules(ClassTable, InstanceTable, ExternalTypeParams,
        Constraints, !TVarSet, !Bindings, Changed) :-
    % XXX Should we sort and merge the constraints here?
    do_class_improvement(ClassTable, ExternalTypeParams, Constraints,
        !Bindings, Changed1),
    % XXX Do we really need to modify the varset? See the comment above
    % find_matching_instance_rule.
    do_instance_improvement(ClassTable, InstanceTable, ExternalTypeParams,
        Constraints, !TVarSet, !Bindings, Changed2),
    Changed = bool.or(Changed1, Changed2).

:- pred do_class_improvement(class_table::in, external_type_params::in,
    hlds_constraints::in, tsubst::in, tsubst::out, bool::out) is det.

do_class_improvement(ClassTable, ExternalTypeParams, Constraints, !Bindings,
        Changed) :-
    Redundant = Constraints ^ hcs_redundant,
    map.keys(Redundant, ClassIds),
    list.foldl2(
        do_class_improvement_2(ClassTable, ExternalTypeParams, Redundant),
        ClassIds, !Bindings, no, Changed).

:- pred do_class_improvement_2(class_table::in, external_type_params::in,
    redundant_constraints::in, class_id::in, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_class_improvement_2(ClassTable, ExternalTypeParams, RedundantConstraints,
        ClassId, !Bindings, !Changed) :-
    map.lookup(ClassTable, ClassId, ClassDefn),
    FunDeps = ClassDefn ^ classdefn_fundeps,
    map.lookup(RedundantConstraints, ClassId, ConstraintSet),
    set.to_sorted_list(ConstraintSet, ConstraintList),
    do_class_improvement_by_pairs(ConstraintList, FunDeps, ExternalTypeParams,
        !Bindings, !Changed).

    % Try to find an opportunity for improvement for each (unordered)
    % pair of constraints from the list.
    %
:- pred do_class_improvement_by_pairs(list(hlds_constraint)::in,
    hlds_class_fundeps::in, external_type_params::in, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_class_improvement_by_pairs([], _, _, !Bindings, !Changed).
do_class_improvement_by_pairs([Constraint | Constraints], FunDeps,
        ExternalTypeParams, !Bindings, !Changed) :-
    do_class_improvement_by_pairs_2(Constraint, Constraints, FunDeps,
        ExternalTypeParams, !Bindings, !Changed),
    do_class_improvement_by_pairs(Constraints, FunDeps, ExternalTypeParams,
        !Bindings, !Changed).

:- pred do_class_improvement_by_pairs_2(hlds_constraint::in,
    list(hlds_constraint)::in, hlds_class_fundeps::in,
    external_type_params::in, tsubst::in, tsubst::out, bool::in, bool::out)
    is det.

do_class_improvement_by_pairs_2(_, [], _, _, !Bindings, !Changed).
do_class_improvement_by_pairs_2(Constraint, [HeadConstraint | TailConstraints],
        FunDeps, ExternalTypeParams, !Bindings, !Changed) :-
    do_class_improvement_pair(Constraint, HeadConstraint, FunDeps,
        ExternalTypeParams, !Bindings, !Changed),
    do_class_improvement_by_pairs_2(Constraint, TailConstraints, FunDeps,
        ExternalTypeParams, !Bindings, !Changed).

    % Try to find an opportunity for improvement for this pair of
    % constraints, using each fundep in turn.
    %
:- pred do_class_improvement_pair(hlds_constraint::in, hlds_constraint::in,
    hlds_class_fundeps::in, external_type_params::in, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_class_improvement_pair(_, _, [], _, !Bindings, !Changed).
do_class_improvement_pair(ConstraintA, ConstraintB, [FunDep | FunDeps],
        ExternalTypeParams, !Bindings, !Changed) :-
    do_class_improvement_fundep(ConstraintA, ConstraintB, FunDep,
        ExternalTypeParams, !Bindings, !Changed),
    do_class_improvement_pair(ConstraintA, ConstraintB, FunDeps,
        ExternalTypeParams, !Bindings, !Changed).

:- pred do_class_improvement_fundep(hlds_constraint::in, hlds_constraint::in,
    hlds_class_fundep::in, external_type_params::in, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_class_improvement_fundep(ConstraintA, ConstraintB, FunDep,
        ExternalTypeParams, !Bindings, !Changed) :-
    ConstraintA = hlds_constraint(_IdsA, _ClassNameA, TypesA),
    ConstraintB = hlds_constraint(_IdsB, _ClassNameB, TypesB),
    FunDep = fundep(Domain, Range),
    ( if
        % We already know that the name/arity of the constraints match,
        % since we have partitioned them already.
        lists_match_on_elements(Domain, TypesA, TypesB),
        not lists_match_on_elements(Range, TypesA, TypesB),

        % The unification can fail if type parameters in the declaration
        % would be bound by the improvement rule. This means that the
        % declaration is not as specific as it could be, but that is not
        % a problem for us.
        unify_on_elements(Range, TypesA, TypesB, ExternalTypeParams, !Bindings)
    then
        !:Changed = yes
    else
        true
    ).

:- pred do_instance_improvement(class_table::in, instance_table::in,
    external_type_params::in, hlds_constraints::in, tvarset::in, tvarset::out,
    tsubst::in, tsubst::out, bool::out) is det.

do_instance_improvement(ClassTable, InstanceTable, ExternalTypeParams,
        Constraints, !TVarSet, !Bindings, Changed) :-
    RedundantConstraints = Constraints ^ hcs_redundant,
    map.keys(RedundantConstraints, ClassIds),
    list.foldl3(
        do_instance_improvement_2(ClassTable, InstanceTable,
            ExternalTypeParams, RedundantConstraints),
        ClassIds, !TVarSet, !Bindings, no, Changed).

:- pred do_instance_improvement_2(class_table::in, instance_table::in,
    external_type_params::in, redundant_constraints::in, class_id::in,
    tvarset::in, tvarset::out, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_instance_improvement_2(ClassTable, InstanceTable, ExternalTypeParams,
        RedundantConstraints, ClassId, !TVarSet, !Bindings, !Changed) :-
    map.lookup(ClassTable, ClassId, ClassDefn),
    FunDeps = ClassDefn ^ classdefn_fundeps,
    map.lookup(InstanceTable, ClassId, InstanceDefns),
    map.lookup(RedundantConstraints, ClassId, ConstraintSet),
    set.to_sorted_list(ConstraintSet, ConstraintList),
    list.foldl3(
        do_instance_improvement_3(ConstraintList, FunDeps, ExternalTypeParams),
        InstanceDefns, !TVarSet, !Bindings, !Changed).

:- pred do_instance_improvement_3(list(hlds_constraint)::in,
    hlds_class_fundeps::in, external_type_params::in, hlds_instance_defn::in,
    tvarset::in, tvarset::out, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_instance_improvement_3(Constraints, FunDeps, ExternalTypeParams,
        InstanceDefn, !TVarSet, !Bindings, !Changed) :-
    InstanceTVarSet = InstanceDefn ^ instdefn_tvarset,
    InstanceTypes0 = InstanceDefn ^ instdefn_types,
    tvarset_merge_renaming(!.TVarSet, InstanceTVarSet, NewTVarSet, Renaming),
    apply_variable_renaming_to_type_list(Renaming, InstanceTypes0,
        InstanceTypes),
    list.foldl2(
        do_instance_improvement_4(FunDeps, InstanceTypes, ExternalTypeParams),
        Constraints, !Bindings, no, Changed0),
    (
        Changed0 = yes,
        !:TVarSet = NewTVarSet,
        !:Changed = yes
    ;
        Changed0 = no
    ).

:- pred do_instance_improvement_4(hlds_class_fundeps::in, list(mer_type)::in,
    external_type_params::in, hlds_constraint::in, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_instance_improvement_4(FunDeps, InstanceTypes, ExternalTypeParams,
        Constraint, !Bindings, !Changed) :-
    list.foldl2(
        do_instance_improvement_fundep(Constraint, InstanceTypes,
            ExternalTypeParams),
        FunDeps, !Bindings, !Changed).

:- pred do_instance_improvement_fundep(hlds_constraint::in, list(mer_type)::in,
    external_type_params::in, hlds_class_fundep::in, tsubst::in, tsubst::out,
    bool::in, bool::out) is det.

do_instance_improvement_fundep(Constraint, InstanceTypes0, ExternalTypeParams,
        FunDep, !Bindings, !Changed) :-
    Constraint = hlds_constraint(_Ids, _ClassName, ConstraintTypes),
    FunDep = fundep(Domain, Range),
    ( if
        % We already know that the name/arity of the constraints match,
        % since we have partitioned them already.
        subsumes_on_elements(Domain, InstanceTypes0, ConstraintTypes, Subst),
        apply_rec_subst_to_type_list(Subst, InstanceTypes0, InstanceTypes),

        % Improvement occurs iff the instance range types are not more
        % general than the constraint range types. If they *are* more
        % general, we stop here.
        not subsumes_on_elements(Range, InstanceTypes, ConstraintTypes, _),

        % The unification can fail if type parameters in the declaration
        % would be bound by the improvement rule. This means that the
        % declaration is not as specific as it could be, but that is not
        % a problem for us.
        unify_on_elements(Range, InstanceTypes, ConstraintTypes,
            ExternalTypeParams, !Bindings)
    then
        !:Changed = yes
    else
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
    list(mer_type)::in, external_type_params::in, tsubst::in, tsubst::out)
    is semidet.

unify_on_elements(Elements, TypesA, TypesB, ExternalTypeParams, !Bindings) :-
    RTypesA = restrict_list_elements(Elements, TypesA),
    RTypesB = restrict_list_elements(Elements, TypesB),
    type_unify_list(RTypesA, RTypesB, ExternalTypeParams, !Bindings).

    % Analogous to type_list_subsumes except that it only checks those
    % elements of the list specified by the set of indices.
    %
:- pred subsumes_on_elements(set(hlds_class_argpos)::in, list(mer_type)::in,
    list(mer_type)::in, tsubst::out) is semidet.

subsumes_on_elements(Elements, TypesA, TypesB, Subst) :-
    RTypesA = restrict_list_elements(Elements, TypesA),
    RTypesB = restrict_list_elements(Elements, TypesB),
    type_vars_in_types(RTypesB, RTypesBVars),
    map.init(Subst0),
    type_unify_list(RTypesA, RTypesB, RTypesBVars, Subst0, Subst).

:- pred eliminate_assumed_constraints(constraint_map::in, constraint_map::out,
    hlds_constraints::in, hlds_constraints::out, bool::out) is det.

eliminate_assumed_constraints(!ConstraintMap, !Constraints, Changed) :-
    !.Constraints = hlds_constraints(Unproven0, Assumed, Redundant, Ancestors),
    eliminate_assumed_constraints_2(Assumed, !ConstraintMap,
        Unproven0, Unproven, Changed),
    !:Constraints = hlds_constraints(Unproven, Assumed, Redundant, Ancestors).

:- pred eliminate_assumed_constraints_2(list(hlds_constraint)::in,
    constraint_map::in, constraint_map::out,
    list(hlds_constraint)::in, list(hlds_constraint)::out,
    bool::out) is det.

eliminate_assumed_constraints_2(_, !ConstraintMap, [], [], no).
eliminate_assumed_constraints_2(AssumedCs, !ConstraintMap, [C | Cs], NewCs,
        Changed) :-
    eliminate_assumed_constraints_2(AssumedCs, !ConstraintMap, Cs, NewCs0,
        Changed0),
    ( if
        some [A] (
            list.member(A, AssumedCs),
            matching_constraints(A, C)
        )
    then
        update_constraint_map(C, !ConstraintMap),
        NewCs = NewCs0,
        Changed = yes
    else
        NewCs = [C | NewCs0],
        Changed = Changed0
    ).

:- pred apply_instance_rules(class_table::in, instance_table::in,
    tvarset::in, tvarset::out,
    constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out,
    list(hlds_constraint)::in, list(hlds_constraint)::out,
    hlds_constraints::in, hlds_constraints::out, bool::out) is det.

apply_instance_rules(ClassTable, InstanceTable, !TVarSet, !ProofMap,
        !ConstraintMap, !Seen, !Constraints, Changed) :-
    !.Constraints = hlds_constraints(Unproven0, Assumed,
        Redundant0, Ancestors),
    apply_instance_rules_2(ClassTable, InstanceTable, !TVarSet, !ProofMap,
        !ConstraintMap, Redundant0, Redundant, !Seen,
        Unproven0, Unproven, Changed),
    !:Constraints = hlds_constraints(Unproven, Assumed, Redundant, Ancestors).

:- pred apply_instance_rules_2(class_table::in, instance_table::in,
    tvarset::in, tvarset::out,
    constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out,
    redundant_constraints::in, redundant_constraints::out,
    list(hlds_constraint)::in, list(hlds_constraint)::out,
    list(hlds_constraint)::in, list(hlds_constraint)::out, bool::out) is det.

apply_instance_rules_2(_, _, !TVarSet, !ProofMap, !ConstraintMap, !Redundant,
        !Seen, [], [], no).
apply_instance_rules_2(ClassTable, InstanceTable, !TVarSet, !ProofMap,
        !ConstraintMap, !Redundant, !Seen, [C | Cs], Constraints, Changed) :-
    C = hlds_constraint(_Ids, ClassName, ArgTypes),
    list.length(ArgTypes, Arity),
    map.lookup(InstanceTable, class_id(ClassName, Arity), Instances),
    InitialTVarSet = !.TVarSet,
    ( if
        find_matching_instance_rule(Instances, C, !TVarSet, !ProofMap,
            NewConstraints0)
    then
        update_constraint_map(C, !ConstraintMap),
        % Remove any constraints we've already seen.
        % This ensures we don't get into an infinite loop.
        list.filter(matches_no_constraint(!.Seen),
            NewConstraints0, NewConstraints),
        update_redundant_constraints(ClassTable, !.TVarSet,
            NewConstraints, !Redundant),
        % Put the new constraints at the front of the list.
        !:Seen = NewConstraints ++ !.Seen,
        Changed1 = yes
    else
        % Put the old constraint at the front of the list.
        NewConstraints = [C],
        !:TVarSet = InitialTVarSet,
        Changed1 = no
    ),
    apply_instance_rules_2(ClassTable, InstanceTable, !TVarSet, !ProofMap,
        !ConstraintMap, !Redundant, !Seen, Cs, TailConstraints, Changed2),
    bool.or(Changed1, Changed2, Changed),
    list.append(NewConstraints, TailConstraints, Constraints).

:- pred matches_no_constraint(list(hlds_constraint)::in, hlds_constraint::in)
    is semidet.

matches_no_constraint(Seen, Constraint) :-
    not (
        some [S] (
            list.member(S, Seen),
            matching_constraints(S, Constraint)
        )
    ).

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

find_matching_instance_rule(Instances, Constraint, !TVarSet, !ProofMap,
        NewConstraints) :-
    % Start a counter so we remember which instance decl we have used.
    find_matching_instance_rule_2(Instances, 1, Constraint, !TVarSet,
        !ProofMap, NewConstraints).

:- pred find_matching_instance_rule_2(list(hlds_instance_defn)::in, int::in,
    hlds_constraint::in, tvarset::in, tvarset::out,
    constraint_proof_map::in, constraint_proof_map::out,
    list(hlds_constraint)::out) is semidet.

find_matching_instance_rule_2([Instance | Instances], CurInstanceNum,
        Constraint, !TVarSet, !ProofMap, NewConstraints) :-
    Constraint = hlds_constraint(_Ids, _ClassName, ArgTypes),
    ProgConstraints0 = Instance ^ instdefn_constraints,
    InstanceTypes0 = Instance ^ instdefn_types,
    InstanceTVarSet = Instance ^ instdefn_tvarset,
    tvarset_merge_renaming(!.TVarSet, InstanceTVarSet, NewTVarSet, Renaming),
    apply_variable_renaming_to_type_list(Renaming, InstanceTypes0,
        InstanceTypes),
    ( if type_list_subsumes(InstanceTypes, ArgTypes, Subst) then
        !:TVarSet = NewTVarSet,
        apply_variable_renaming_to_prog_constraint_list(Renaming,
            ProgConstraints0, ProgConstraints1),
        apply_rec_subst_to_prog_constraint_list(Subst,
            ProgConstraints1, ProgConstraints),
        init_hlds_constraint_list(ProgConstraints, NewConstraints),

        NewProof = apply_instance(instance_id(CurInstanceNum)),
        retrieve_prog_constraint(Constraint, ProgConstraint),
        map.set(ProgConstraint, NewProof, !ProofMap)
    else
        find_matching_instance_rule_2(Instances, CurInstanceNum + 1,
            Constraint, !TVarSet, !ProofMap, NewConstraints)
    ).

    % To reduce a constraint using class declarations, we search the
    % ancestors in the hlds_constraints to find a path from the inferred
    % constraint to another (declared or inferred) constraint.
    %
:- pred apply_class_rules(constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out,
    hlds_constraints::in, hlds_constraints::out, bool::out) is det.

apply_class_rules(!ProofMap, !ConstraintMap, !Constraints, Changed) :-
    !.Constraints = hlds_constraints(Unproven0, _, _, Ancestors),
    apply_class_rules_2(Ancestors, !ProofMap, !ConstraintMap,
        Unproven0, Unproven, Changed),
    !Constraints ^ hcs_unproven := Unproven.

:- pred apply_class_rules_2(ancestor_constraints::in,
    constraint_proof_map::in, constraint_proof_map::out,
    constraint_map::in, constraint_map::out,
    list(hlds_constraint)::in, list(hlds_constraint)::out, bool::out) is det.

apply_class_rules_2(_, !ProofMap, !ConstraintMap, [], [], no).
apply_class_rules_2(Ancestors, !ProofMap, !ConstraintMap,
        [Constraint0 | Constraints0], Constraints, Changed) :-
    retrieve_prog_constraint(Constraint0, ProgConstraint0),
    ( if
        map.search(Ancestors, ProgConstraint0, Descendants)
    then
        update_constraint_map(Constraint0, !ConstraintMap),
        add_superclass_proofs(ProgConstraint0, Descendants, !ProofMap),
        apply_class_rules_2(Ancestors, !ProofMap, !ConstraintMap,
            Constraints0, Constraints, _),
        Changed = yes
    else
        apply_class_rules_2(Ancestors, !ProofMap, !ConstraintMap,
            Constraints0, TailConstraints, Changed),
        Constraints = [Constraint0 | TailConstraints]
    ).

:- pred add_superclass_proofs(prog_constraint::in, list(prog_constraint)::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

add_superclass_proofs(_, [], !ProofMap).
add_superclass_proofs(Constraint, [Descendant | Descendants], !ProofMap) :-
    map.set(Constraint, superclass(Descendant), !ProofMap),
    add_superclass_proofs(Descendant, Descendants, !ProofMap).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.typeclasses.
%-----------------------------------------------------------------------------%
