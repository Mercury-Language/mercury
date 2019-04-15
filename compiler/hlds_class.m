%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_lass.m.
% Main authors: fjh, conway.
%
% This module defines the part of the HLDS that deals with type classes
% and type class constraints.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_class.
:- interface.

:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module int.
:- import_module require.
:- import_module varset.

%---------------------------------------------------------------------------%

:- interface.

:- type class_table == map(class_id, hlds_class_defn).

    % Information about a single `typeclass' declaration.
    %
:- type hlds_class_defn
    --->    hlds_class_defn(
                classdefn_status            :: typeclass_status,

                % SuperClasses.
                classdefn_supers            :: list(prog_constraint),

                % Functional dependencies.
                classdefn_fundeps           :: hlds_class_fundeps,

                % All ancestors which have fundeps on them.
                classdefn_fundep_ancestors  :: list(prog_constraint),

                % ClassVars.
                classdefn_vars              :: list(tvar),

                % Kinds of class_vars.
                classdefn_kinds             :: tvar_kind_map,

                % The interface from the original declaration, used by
                % intermod.m to % write out the interface for a local typeclass
                % to the `.opt' file.
                classdefn_interface         :: class_interface,

                % Methods.
                classdefn_hlds_interface    :: hlds_class_interface,

                % VarNames.
                classdefn_tvarset           :: tvarset,

                % Location of declaration.
                classdefn_context           :: prog_context
            ).

    % In the HLDS, functional dependencies are represented using
    % argument positions (counting from 1) rather than type variables.
    % We know that there will be a one-one correspondence since
    % typeclass parameters must be distinct variables, and using
    % argument positions is more efficient.
    %
:- type hlds_class_fundeps == list(hlds_class_fundep).
:- type hlds_class_fundep
    --->    fundep(
                domain      :: set(hlds_class_argpos),
                range       :: set(hlds_class_argpos)
            ).

:- type hlds_class_argpos == int.

:- func restrict_list_elements(set(hlds_class_argpos), list(T)) = list(T).

:- type hlds_class_interface == list(pred_proc_id).

    % For each class, we keep track of a list of its instances, since there
    % can be more than one instance of each class. Each visible instance
    % is assigned a unique identifier (integers beginning from one).
    % The position in the list of instances corresponds to the instance_id.
    %
:- type instance_table == map(class_id, list(hlds_instance_defn)).

:- type instance_id == int.

    % Information about a single `instance' declaration.
    % The order of fields is intended to put the list of hlds_instance_defns
    % of a class into a stable order when the hlds_instance_defns are sorted.
    % ("Stable" meaning that changing *only* the order of the instance
    % declarations in a source module should leave the contents of the
    % .opt file unchanged.)
    %
:- type hlds_instance_defn
    --->    hlds_instance_defn(
                % Module of the instance declaration.
                instdefn_module         :: module_name,

                % The class types. The original types field is used only
                % for error checking.
                instdefn_types          :: list(mer_type),
                instdefn_orig_types     :: list(mer_type),

                % Import status of the instance declaration.
                % XXX This can be set to abstract_imported even if
                % the instance is NOT imported.
                instdefn_status         :: instance_status,

                % Context of declaration.
                instdefn_context        :: prog_context,

                % Constraints on the instance declaration.
                instdefn_constraints    :: list(prog_constraint),

                % Methods
                instdefn_body           :: instance_body,

                % After check_typeclass, we will know the pred_ids and proc_ids
                % of all the methods.
                instdefn_hlds_interface :: maybe(hlds_class_interface),

                % VarNames
                instdefn_tvarset        :: tvarset,

                % "Proofs" of how to build the typeclass_infos for the
                % superclasses of this class (that is, the constraints
                % on the class declaration), for this instance.
                instdefn_proofs         :: constraint_proof_map
            ).

    % Return the value of the MR_typeclass_info_num_extra_instance_args field
    % in the base_typeclass_info of the given instance.
    %
:- pred num_extra_instance_args(hlds_instance_defn::in, int::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

restrict_list_elements(Elements, List) = RestrictedList :-
    set.to_sorted_list(Elements, SortedElements),
    restrict_list_elements_2(SortedElements, 1, List, RestrictedList).

:- pred restrict_list_elements_2(list(hlds_class_argpos)::in,
    hlds_class_argpos::in, list(T)::in, list(T)::out) is det.

restrict_list_elements_2(_, _, [], []).
restrict_list_elements_2([], _, [_ | _], []).
restrict_list_elements_2([Posn | Posns], Index, [X | Xs], RestrictedXs) :-
    ( if Index = Posn then
        restrict_list_elements_2(Posns, Index + 1, Xs, TailRestrictedXs),
        RestrictedXs = [X | TailRestrictedXs]
    else if Index < Posn then
        restrict_list_elements_2([Posn | Posns], Index + 1, Xs, RestrictedXs)
    else
        restrict_list_elements_2(Posns, Index + 1, [X | Xs], RestrictedXs)
    ).

num_extra_instance_args(InstanceDefn, NumExtra) :-
    InstanceDefn = hlds_instance_defn(_InstanceModule,
        InstanceTypes, _OrigInstanceTypes, _ImportStatus, _TermContext,
        InstanceConstraints, _Body, _PredProcIds, _Varset, _SuperClassProofs),
    type_vars_list(InstanceTypes, TypeVars),
    get_unconstrained_tvars(TypeVars, InstanceConstraints, Unconstrained),
    list.length(InstanceConstraints, NumConstraints),
    list.length(Unconstrained, NumUnconstrained),
    NumExtra = NumConstraints + NumUnconstrained.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- interface.

    % Identifiers for constraints which are unique across a given type_assign.
    % Integers in these values refer to the position in the list of constraints
    % at that location, beginning from 1.
    %
    % Only identifiers for constraints appearing directly on a goal are needed
    % at the moment, so there is no way to represent the appropriate identifier
    % for the superclass of such a constraint.
    %
    % XXX A more robust and efficient solution would be to allocate
    % unique integers to the constraints as they are encountered, and
    % store the allocated integer in the relevant hlds_goal_expr.
    %
:- type constraint_id
    --->    constraint_id(
                % Assumed or unproven.
                constraint_type,

                % The id of the atomic goal which is constrained.
                goal_id,

                % The position of the constraint.
                int
            ).

:- type constraint_type
    --->    unproven
    ;       assumed.

    % The identifier of a constraint is stored along with the constraint.
    % Each value of this type may have more than one identifier because
    % if two constraints in a context are equivalent, then we merge them
    % together in order to not have to prove the same constraint twice.
    %
:- type hlds_constraint
    --->    hlds_constraint(
                list(constraint_id),
                class_name,
                list(mer_type)
            ).

:- type hlds_constraints
    --->    hlds_constraints(
                % Unproven constraints. These are the constraints that we must
                % prove (that is, universal constraints from the goal being
                % checked, or existential constraints on the head).
                hcs_unproven    :: list(hlds_constraint),

                % Assumed constraints. These are constraints we can use in
                % proofs (that is, existential constraints from the goal being
                % checked, or universal constraints on the head).
                hcs_assumed     :: list(hlds_constraint),

                % Constraints that are known to be redundant. This includes
                % constraints that have already been proved as well as
                % constraints that are ancestors of other unproven, assumed
                % or redundant constraints. Not all such constraints are
                % included, only those which may be used for the purposes
                % of improvement.
                hcs_redundant   :: redundant_constraints,

                % Ancestors of assumed constraints.
                hcs_ancestors   :: ancestor_constraints
            ).

    % Redundant constraints are partitioned by class, which helps us
    % process them more efficiently.
    %
:- type redundant_constraints == map(class_id, set(hlds_constraint)).

    % Constraints which are ancestors of assumed constraints, along with the
    % list of constraints (following the class hierarchy) which leads to
    % the assumed constraint. The assumed constraint is the last item in the
    % list.
    %
    % Note that if there are two possible lists for the same constraint, we
    % always keep the shorter one.
    %
:- type ancestor_constraints == map(prog_constraint, list(prog_constraint)).

    % During type checking we fill in a constraint_map which gives
    % the constraint that corresponds to each identifier. This is used
    % by the polymorphism translation to retrieve details of constraints.
    %
:- type constraint_map == map(constraint_id, prog_constraint).

    % `Proof' of why a constraint is redundant.
:- type constraint_proof
    --->    apply_instance(instance_id)
            % Apply the instance decl with the given identifier. Note that
            % we don't store the actual hlds_instance_defn for two reasons:
            %
            % - That would require storing a renamed version of the
            %   constraint_proofs for *every* use of an instance declaration.
            %   This wouldn't even get GCed for a long time because it
            %   would be stored in the pred_info.
            %
            % - The superclass proofs stored in the hlds_instance_defn would
            %   need to store all the constraint_proofs for all its ancestors.
            %   This would require the class relation to be topologically
            %   sorted before checking the instance declarations.

    ;       superclass(prog_constraint).
            % The constraint is redundant because of the following class's
            % superclass declaration.

    % The constraint_proof_map is a map which for each type class constraint
    % records how/why that constraint was satisfied. This information is used
    % to determine how to construct the typeclass_info for that constraint.
    %
:- type constraint_proof_map == map(prog_constraint, constraint_proof).

:- pred empty_hlds_constraints(hlds_constraints::out) is det.

:- pred init_hlds_constraint_list(list(prog_constraint)::in,
    list(hlds_constraint)::out) is det.

:- pred make_head_hlds_constraints(class_table::in, tvarset::in,
    prog_constraints::in, hlds_constraints::out) is det.

:- pred make_body_hlds_constraints(class_table::in, tvarset::in, goal_id::in,
    prog_constraints::in, hlds_constraints::out) is det.

    % make_hlds_constraints(ClassTable, TVarSet, UnprovenConstraints,
    %   AssumedConstraints, Constraints):
    %
    % ClassTable is the class_table for the module. TVarSet is the tvarset
    % for the predicate this class context is for. UnprovenConstraints is
    % a list of constraints which will need to be proven (that is, universal
    % constraints in the body or existential constraints in the head).
    % AssumedConstraints is a list of constraints that may be used in proofs
    % (that is, existential constraints in the body or universal constraints
    % in the head).
    %
:- pred make_hlds_constraints(class_table::in, tvarset::in,
    list(hlds_constraint)::in, list(hlds_constraint)::in,
    hlds_constraints::out) is det.

:- pred make_hlds_constraint_list(list(prog_constraint)::in,
    constraint_type::in, goal_id::in, list(hlds_constraint)::out) is det.

:- pred merge_hlds_constraints(hlds_constraints::in, hlds_constraints::in,
    hlds_constraints::out) is det.

:- pred retrieve_prog_constraints(hlds_constraints::in, prog_constraints::out)
    is det.

:- pred retrieve_prog_constraint_list(list(hlds_constraint)::in,
    list(prog_constraint)::out) is det.

:- pred retrieve_prog_constraint(hlds_constraint::in, prog_constraint::out)
    is det.

:- pred matching_constraints(hlds_constraint::in, hlds_constraint::in)
    is semidet.

:- pred compare_hlds_constraints(hlds_constraint::in, hlds_constraint::in,
    comparison_result::out) is det.

:- pred update_constraint_map(hlds_constraint::in, constraint_map::in,
    constraint_map::out) is det.

:- pred update_redundant_constraints(class_table::in, tvarset::in,
    list(hlds_constraint)::in,
    redundant_constraints::in, redundant_constraints::out) is det.

:- pred lookup_hlds_constraint_list(constraint_map::in, constraint_type::in,
    goal_id::in, int::in, list(prog_constraint)::out) is det.

:- pred search_hlds_constraint_list(constraint_map::in, constraint_type::in,
    goal_id::in, int::in, list(prog_constraint)::out) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

empty_hlds_constraints(Constraints) :-
    Constraints = hlds_constraints([], [], map.init, map.init).

init_hlds_constraint_list(ProgConstraints, Constraints) :-
    list.map(init_hlds_constraint, ProgConstraints, Constraints).

:- pred init_hlds_constraint(prog_constraint::in, hlds_constraint::out) is det.

init_hlds_constraint(Constraint, HLDSConstraint) :-
    Constraint = constraint(ClassName, ArgTypes),
    HLDSConstraint = hlds_constraint([], ClassName, ArgTypes).

make_head_hlds_constraints(ClassTable, TVarSet, ProgConstraints,
        Constraints) :-
    ProgConstraints = constraints(UnivConstraints, ExistConstraints),
    GoalId = goal_id_for_head_constraints,
    make_hlds_constraint_list(UnivConstraints, assumed, GoalId,
        AssumedConstraints),
    make_hlds_constraint_list(ExistConstraints, unproven, GoalId,
        UnprovenConstraints),
    make_hlds_constraints(ClassTable, TVarSet, UnprovenConstraints,
        AssumedConstraints, Constraints).

make_body_hlds_constraints(ClassTable, TVarSet, GoalId, ProgConstraints,
        Constraints) :-
    ProgConstraints = constraints(UnivConstraints, ExistConstraints),
    make_hlds_constraint_list(UnivConstraints, unproven, GoalId,
        UnprovenConstraints),
    make_hlds_constraint_list(ExistConstraints, assumed, GoalId,
        AssumedConstraints),
    make_hlds_constraints(ClassTable, TVarSet, UnprovenConstraints,
        AssumedConstraints, Constraints).

make_hlds_constraints(ClassTable, TVarSet, Unproven, Assumed, Constraints) :-
    list.foldl(update_redundant_constraints_2(ClassTable, TVarSet),
        Unproven, map.init, Redundant0),
    list.foldl(update_redundant_constraints_2(ClassTable, TVarSet),
        Assumed, Redundant0, Redundant),
    list.foldl(update_ancestor_constraints(ClassTable, TVarSet),
        Assumed, map.init, Ancestors),
    Constraints = hlds_constraints(Unproven, Assumed, Redundant, Ancestors).

make_hlds_constraint_list(ProgConstraints, ConstraintType, GoalId,
        Constraints) :-
    make_hlds_constraint_list_2(ProgConstraints, ConstraintType, GoalId,
        1, Constraints).

:- pred make_hlds_constraint_list_2(list(prog_constraint)::in,
    constraint_type::in, goal_id::in, int::in, list(hlds_constraint)::out)
    is det.

make_hlds_constraint_list_2([], _, _, _, []).
make_hlds_constraint_list_2([ProgConstraint | ProgConstraints], ConstraintType,
        GoalId, CurArgNum, [HLDSConstraint | HLDSConstraints]) :-
    ProgConstraint = constraint(ClassName, ArgTypes),
    Id = constraint_id(ConstraintType, GoalId, CurArgNum),
    HLDSConstraint = hlds_constraint([Id], ClassName, ArgTypes),
    make_hlds_constraint_list_2(ProgConstraints, ConstraintType,
        GoalId, CurArgNum + 1, HLDSConstraints).

merge_hlds_constraints(ConstraintsA, ConstraintsB, Constraints) :-
    ConstraintsA = hlds_constraints(UnprovenA, AssumedA,
        RedundantA, AncestorsA),
    ConstraintsB = hlds_constraints(UnprovenB, AssumedB,
        RedundantB, AncestorsB),
    Unproven = UnprovenA ++ UnprovenB,
    Assumed = AssumedA ++ AssumedB,
    map.union(set.union, RedundantA, RedundantB, Redundant),
    map.union(pick_shorter_list, AncestorsA, AncestorsB, Ancestors),
    Constraints = hlds_constraints(Unproven, Assumed, Redundant, Ancestors).

:- pred pick_shorter_list(list(T)::in, list(T)::in, list(T)::out) is det.

pick_shorter_list(As, Bs, Cs) :-
    ( if is_shorter(As, Bs) then
        Cs = As
    else
        Cs = Bs
    ).

:- pred is_shorter(list(T)::in, list(T)::in) is semidet.

is_shorter([], _).
is_shorter([_ | As], [_ | Bs]) :-
    is_shorter(As, Bs).

retrieve_prog_constraints(Constraints, ProgConstraints) :-
    Constraints = hlds_constraints(Unproven, Assumed, _, _),
    retrieve_prog_constraint_list(Unproven, UnivProgConstraints),
    retrieve_prog_constraint_list(Assumed, ExistProgConstraints),
    ProgConstraints = constraints(UnivProgConstraints, ExistProgConstraints).

retrieve_prog_constraint_list(Constraints, ProgConstraints) :-
    list.map(retrieve_prog_constraint, Constraints, ProgConstraints).

retrieve_prog_constraint(Constraint, ProgConstraint) :-
    Constraint = hlds_constraint(_Ids, ClassName, ArgTypes),
    ProgConstraint = constraint(ClassName, ArgTypes).

matching_constraints(ConstraintA, ConstraintB) :-
    ConstraintA = hlds_constraint(_IdsA, ClassName, ArgTypes),
    ConstraintB = hlds_constraint(_IdsB, ClassName, ArgTypes).

compare_hlds_constraints(ConstraintA, ConstraintB, CmpRes) :-
    ConstraintA = hlds_constraint(_IdsA, ClassNameA, ArgTypesA),
    ConstraintB = hlds_constraint(_IdsB, ClassNameB, ArgTypesB),
    compare(NameCmpRes, ClassNameA, ClassNameB),
    (
        NameCmpRes = (=),
        compare(CmpRes, ArgTypesA, ArgTypesB)
    ;
        ( NameCmpRes = (<)
        ; NameCmpRes = (>)
        ),
        CmpRes = NameCmpRes
    ).

update_constraint_map(HLDSConstraint, !ConstraintMap) :-
    HLDSConstraint = hlds_constraint(Ids, ClassName, ArgTypes),
    ProgConstraint = constraint(ClassName, ArgTypes),
    list.foldl(update_constraint_map_2(ProgConstraint), Ids, !ConstraintMap).

:- pred update_constraint_map_2(prog_constraint::in, constraint_id::in,
    constraint_map::in, constraint_map::out) is det.

update_constraint_map_2(ProgConstraint, ConstraintId, !ConstraintMap) :-
    map.set(ConstraintId, ProgConstraint, !ConstraintMap).

update_redundant_constraints(ClassTable, TVarSet, Constraints, !Redundant) :-
    list.foldl(update_redundant_constraints_2(ClassTable, TVarSet),
        Constraints, !Redundant).

:- pred update_redundant_constraints_2(class_table::in, tvarset::in,
    hlds_constraint::in, redundant_constraints::in,
    redundant_constraints::out) is det.

update_redundant_constraints_2(ClassTable, TVarSet, Constraint, !Redundant) :-
    Constraint = hlds_constraint(_Ids, ClassName, ArgTypes),
    list.length(ArgTypes, Arity),
    ClassId = class_id(ClassName, Arity),
    map.lookup(ClassTable, ClassId, ClassDefn),
    ClassAncestors0 = ClassDefn ^ classdefn_fundep_ancestors,
    list.map(init_hlds_constraint, ClassAncestors0, ClassAncestors),
    (
        % Optimize the simple case.
        ClassAncestors = []
    ;
        ClassAncestors = [_ | _],
        ClassTVarSet = ClassDefn ^ classdefn_tvarset,
        ClassParams = ClassDefn ^ classdefn_vars,

        % We can ignore the resulting tvarset, since any new variables
        % will become bound when the arguments are bound. (This follows
        % from the fact that constraints on class declarations can only use
        % variables that appear in the head of the declaration.)

        tvarset_merge_renaming(TVarSet, ClassTVarSet, _, Renaming),
        apply_variable_renaming_to_constraint_list(Renaming,
            ClassAncestors, RenamedAncestors),
        apply_variable_renaming_to_tvar_list(Renaming, ClassParams,
            RenamedParams),
        map.from_corresponding_lists(RenamedParams, ArgTypes, Subst),
        apply_subst_to_constraint_list(Subst, RenamedAncestors, Ancestors),
        list.foldl(add_redundant_constraint, Ancestors, !Redundant)
    ).

:- pred add_redundant_constraint(hlds_constraint::in,
    redundant_constraints::in, redundant_constraints::out) is det.

add_redundant_constraint(Constraint, !Redundant) :-
    Constraint = hlds_constraint(_Ids, ClassName, ArgTypes),
    list.length(ArgTypes, Arity),
    ClassId = class_id(ClassName, Arity),
    ( if map.search(!.Redundant, ClassId, Constraints0) then
        set.insert(Constraint, Constraints0, Constraints),
        map.det_update(ClassId, Constraints, !Redundant)
    else
        Constraints = set.make_singleton_set(Constraint),
        map.det_insert(ClassId, Constraints, !Redundant)
    ).

lookup_hlds_constraint_list(ConstraintMap, ConstraintType, GoalId, Count,
        Constraints) :-
    ( if
        search_hlds_constraint_list_2(ConstraintMap, ConstraintType, GoalId,
            Count, [], ConstraintsPrime)
    then
        Constraints = ConstraintsPrime
    else
        unexpected($pred, "not found")
    ).

search_hlds_constraint_list(ConstraintMap, ConstraintType, GoalId, Count,
        Constraints) :-
    search_hlds_constraint_list_2(ConstraintMap, ConstraintType, GoalId,
        Count, [], Constraints).

:- pred search_hlds_constraint_list_2(constraint_map::in, constraint_type::in,
    goal_id::in, int::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is semidet.

search_hlds_constraint_list_2(ConstraintMap, ConstraintType, GoalId, Count,
        !Constraints) :-
    ( if Count = 0 then
        true
    else
        ConstraintId = constraint_id(ConstraintType, GoalId, Count),
        map.search(ConstraintMap, ConstraintId, Constraint),
        !:Constraints = [Constraint | !.Constraints],
        search_hlds_constraint_list_2(ConstraintMap, ConstraintType,
            GoalId, Count - 1, !Constraints)
    ).

%---------------------------------------------------------------------------%

    % Search the superclasses of the given constraint for a potential proof,
    % and add it to the map if no better proof exists already.
    %
:- pred update_ancestor_constraints(class_table::in, tvarset::in,
    hlds_constraint::in, ancestor_constraints::in, ancestor_constraints::out)
    is det.

update_ancestor_constraints(ClassTable, TVarSet, HLDSConstraint, !Ancestors) :-
    retrieve_prog_constraint(HLDSConstraint, Constraint),
    update_ancestor_constraints_2(ClassTable, TVarSet, [], Constraint,
        !Ancestors).

:- pred update_ancestor_constraints_2(class_table::in, tvarset::in,
    list(prog_constraint)::in, prog_constraint::in,
    ancestor_constraints::in, ancestor_constraints::out) is det.

update_ancestor_constraints_2(ClassTable, TVarSet, Descendants0, Constraint,
        !Ancestors) :-
    Constraint = constraint(ClassName, ArgTypes),
    list.length(ArgTypes, Arity),
    ClassId = class_id(ClassName, Arity),
    map.lookup(ClassTable, ClassId, ClassDefn),

    % We can ignore the resulting tvarset, since any new variables
    % will become bound when the arguments are bound. (This follows
    % from the fact that constraints on class declarations can only use
    % variables that appear in the head of the declaration.)

    tvarset_merge_renaming(TVarSet, ClassDefn ^ classdefn_tvarset, _,
        Renaming),
    apply_variable_renaming_to_prog_constraint_list(Renaming,
        ClassDefn ^ classdefn_supers, RenamedSupers),
    apply_variable_renaming_to_tvar_list(Renaming, ClassDefn ^ classdefn_vars,
        RenamedParams),
    map.from_corresponding_lists(RenamedParams, ArgTypes, Subst),
    apply_subst_to_prog_constraint_list(Subst, RenamedSupers, Supers),

    Descendants = [Constraint | Descendants0],
    list.foldl(update_ancestor_constraints_3(ClassTable, TVarSet, Descendants),
        Supers, !Ancestors).

:- pred update_ancestor_constraints_3(class_table::in, tvarset::in,
    list(prog_constraint)::in, prog_constraint::in,
    ancestor_constraints::in, ancestor_constraints::out) is det.

update_ancestor_constraints_3(ClassTable, TVarSet, Descendants, Constraint,
        !Ancestors) :-
    ( if
        map.search(!.Ancestors, Constraint, OldDescendants),
        is_shorter(OldDescendants, Descendants)
    then
        % We don't want to update the ancestors because we already have a
        % better path. The same will apply for all superclasses, so we
        % don't traverse any further.
        true
    else
        map.set(Constraint, Descendants, !Ancestors),
        update_ancestor_constraints_2(ClassTable, TVarSet, Descendants,
            Constraint, !Ancestors)
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_class.
%---------------------------------------------------------------------------%
