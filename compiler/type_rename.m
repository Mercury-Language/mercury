%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: type_name.m.
%
% Predicates for doing renamings and substitutions on HLDS data structures.
%
%-----------------------------------------------------------------------------%

:- module hlds.type_rename.
:- interface.

:- import_module hlds.hlds_class.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred apply_renaming_to_constraint(tvar_renaming::in,
    hlds_constraint::in, hlds_constraint::out) is det.

:- pred apply_subst_to_constraint(tsubst::in, hlds_constraint::in,
    hlds_constraint::out) is det.

:- pred apply_rec_subst_to_constraint(tsubst::in, hlds_constraint::in,
    hlds_constraint::out) is det.

%-------------%

:- pred apply_renaming_to_constraints(tvar_renaming::in,
    list(hlds_constraint)::in, list(hlds_constraint)::out) is det.

:- pred apply_subst_to_constraints(tsubst::in, list(hlds_constraint)::in,
    list(hlds_constraint)::out) is det.

:- pred apply_rec_subst_to_constraints(tsubst::in,
    list(hlds_constraint)::in, list(hlds_constraint)::out) is det.

%-------------%

:- pred apply_renaming_to_constraint_db(tvar_renaming::in,
    hlds_constraint_db::in, hlds_constraint_db::out) is det.

:- pred apply_subst_to_constraint_db(tsubst::in,
    hlds_constraint_db::in, hlds_constraint_db::out) is det.

:- pred apply_rec_subst_to_constraint_db(tsubst::in,
    hlds_constraint_db::in, hlds_constraint_db::out) is det.

%-------------%

:- pred apply_renaming_to_constraint_proof_map(tvar_renaming::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

:- pred apply_subst_to_constraint_proof_map(tsubst::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

:- pred apply_rec_subst_to_constraint_proof_map(tsubst::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

%-------------%

:- pred apply_renaming_to_constraint_map(tvar_renaming::in,
    constraint_map::in, constraint_map::out) is det.

:- pred apply_subst_to_constraint_map(tsubst::in,
    constraint_map::in, constraint_map::out) is det.

:- pred apply_rec_subst_to_constraint_map(tsubst::in,
    constraint_map::in, constraint_map::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type_subst.

:- import_module map.
:- import_module set.

%-----------------------------------------------------------------------------%

apply_renaming_to_constraint(Renaming, !Constraint) :-
    !.Constraint = hlds_constraint(Ids, ClassName, ArgTypes0),
    apply_renaming_to_types(Renaming, ArgTypes0, ArgTypes),
    !:Constraint = hlds_constraint(Ids, ClassName, ArgTypes).

apply_subst_to_constraint(Subst, !Constraint) :-
    !.Constraint = hlds_constraint(Ids, ClassName, ArgTypes0),
    apply_subst_to_types(Subst, ArgTypes0, ArgTypes),
    !:Constraint = hlds_constraint(Ids, ClassName, ArgTypes).

apply_rec_subst_to_constraint(Subst, !Constraint) :-
    !.Constraint = hlds_constraint(Ids, ClassName, ArgTypes0),
    apply_rec_subst_to_types(Subst, ArgTypes0, ArgTypes),
    !:Constraint = hlds_constraint(Ids, ClassName, ArgTypes).

%-----------------------------------------------------------------------------%

apply_renaming_to_constraints(Renaming, !Constraints) :-
    list.map(apply_renaming_to_constraint(Renaming), !Constraints).

apply_subst_to_constraints(Subst, !Constraints) :-
    list.map(apply_subst_to_constraint(Subst), !Constraints).

apply_rec_subst_to_constraints(Subst, !Constraints) :-
    list.map(apply_rec_subst_to_constraint(Subst), !Constraints).

%-----------------------------------------------------------------------------%

apply_renaming_to_constraint_db(Renaming, !ConstraintDb) :-
    !.ConstraintDb = hlds_constraint_db(Unproven0, Assumed0,
        Redundant0, Ancestors0),
    % Most of the time, !.Constraints contains nothing. Even when some
    % of its fields are not empty, some others may be.
    ( if
        Unproven0 = [],
        Assumed0 = [],
        map.is_empty(Redundant0),
        map.is_empty(Ancestors0)
    then
        true
    else
        apply_renaming_to_constraints(Renaming, Unproven0, Unproven),
        apply_renaming_to_constraints(Renaming, Assumed0, Assumed),
        ( if map.is_empty(Redundant0) then
            Redundant = Redundant0
        else
            Pred =
                ( pred(C0::in, C::out) is det :-
                    set.to_sorted_list(C0, L0),
                    apply_renaming_to_constraints(Renaming, L0, L),
                    set.list_to_set(L, C)
                ),
            map.map_values_only(Pred, Redundant0, Redundant)
        ),
        ( if map.is_empty(Ancestors0) then
            Ancestors = Ancestors0
        else
            map.keys(Ancestors0, AncestorsKeys0),
            map.values(Ancestors0, AncestorsValues0),
            apply_renaming_to_prog_constraints(Renaming,
                AncestorsKeys0, AncestorsKeys),
            list.map(apply_renaming_to_prog_constraints(Renaming),
                AncestorsValues0, AncestorsValues),
            map.from_corresponding_lists(AncestorsKeys, AncestorsValues,
                Ancestors)
        ),
        !:ConstraintDb =
            hlds_constraint_db(Unproven, Assumed, Redundant, Ancestors)
    ).

apply_subst_to_constraint_db(Subst, !ConstraintDb) :-
    !.ConstraintDb = hlds_constraint_db(Unproven0, Assumed0,
        Redundant0, Ancestors0),
    apply_subst_to_constraints(Subst, Unproven0, Unproven),
    apply_subst_to_constraints(Subst, Assumed0, Assumed),
    Pred =
        ( pred(C0::in, C::out) is det :-
            set.to_sorted_list(C0, L0),
            apply_subst_to_constraints(Subst, L0, L),
            set.list_to_set(L, C)
        ),
    map.map_values_only(Pred, Redundant0, Redundant),
    map.keys(Ancestors0, AncestorsKeys0),
    map.values(Ancestors0, AncestorsValues0),
    apply_subst_to_prog_constraints(Subst, AncestorsKeys0, AncestorsKeys),
    list.map(apply_subst_to_prog_constraints(Subst),
        AncestorsValues0, AncestorsValues),
    map.from_corresponding_lists(AncestorsKeys, AncestorsValues, Ancestors),
    !:ConstraintDb = hlds_constraint_db(Unproven, Assumed,
        Redundant, Ancestors).

apply_rec_subst_to_constraint_db(Subst, !ConstraintDb) :-
    !.ConstraintDb = hlds_constraint_db(Unproven0, Assumed0,
        Redundant0, Ancestors0),
    apply_rec_subst_to_constraints(Subst, Unproven0, Unproven),
    apply_rec_subst_to_constraints(Subst, Assumed0, Assumed),
    Pred =
        ( pred(C0::in, C::out) is det :-
            set.to_sorted_list(C0, L0),
            apply_rec_subst_to_constraints(Subst, L0, L),
            set.list_to_set(L, C)
        ),
    map.map_values_only(Pred, Redundant0, Redundant),
    map.keys(Ancestors0, AncestorsKeys0),
    map.values(Ancestors0, AncestorsValues0),
    apply_rec_subst_to_prog_constraints(Subst, AncestorsKeys0, AncestorsKeys),
    list.map(apply_rec_subst_to_prog_constraints(Subst),
        AncestorsValues0, AncestorsValues),
    map.from_corresponding_lists(AncestorsKeys, AncestorsValues, Ancestors),
    !:ConstraintDb = hlds_constraint_db(Unproven, Assumed,
        Redundant, Ancestors).

%-----------------------------------------------------------------------------%

apply_renaming_to_constraint_proof_map(Renaming,
        ProofMap0, ProofMap) :-
    ( if map.is_empty(ProofMap0) then
        % Optimize the simple case.
        ProofMap = ProofMap0
    else
        map.keys(ProofMap0, Keys0),
        map.values(ProofMap0, Values0),
        apply_renaming_to_prog_constraints(Renaming, Keys0, Keys),
        list.map(rename_constraint_proof(Renaming), Values0, Values),
        map.from_corresponding_lists(Keys, Values, ProofMap)
    ).

    % Apply a type variable renaming to a class constraint proof.
    %
:- pred rename_constraint_proof(tvar_renaming::in,
    constraint_proof::in, constraint_proof::out) is det.

rename_constraint_proof(TSubst, Proof0, Proof) :-
    (
        Proof0 = apply_instance(_Num),
        Proof = Proof0
    ;
        Proof0 = superclass(ClassConstraint0),
        apply_renaming_to_prog_constraint(TSubst,
            ClassConstraint0, ClassConstraint),
        Proof = superclass(ClassConstraint)
    ).

apply_subst_to_constraint_proof_map(Subst, ProofMap0, ProofMap) :-
    map.foldl(apply_subst_to_constraint_proof_map_2(Subst), ProofMap0,
        map.init, ProofMap).

:- pred apply_subst_to_constraint_proof_map_2(tsubst::in,
    prog_constraint::in, constraint_proof::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

apply_subst_to_constraint_proof_map_2(Subst, Constraint0, Proof0, !ProofMap) :-
    apply_subst_to_prog_constraint(Subst, Constraint0, Constraint),
    (
        Proof0 = apply_instance(_),
        Proof = Proof0
    ;
        Proof0 = superclass(Super0),
        apply_subst_to_prog_constraint(Subst, Super0, Super),
        Proof = superclass(Super)
    ),
    map.set(Constraint, Proof, !ProofMap).

apply_rec_subst_to_constraint_proof_map(Subst, ProofMap0, ProofMap) :-
    map.foldl(apply_rec_subst_to_constraint_proof_map_2(Subst), ProofMap0,
        map.init, ProofMap).

:- pred apply_rec_subst_to_constraint_proof_map_2(tsubst::in,
    prog_constraint::in, constraint_proof::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

apply_rec_subst_to_constraint_proof_map_2(Subst, Constraint0, Proof0,
        !ProofMap) :-
    apply_rec_subst_to_prog_constraint(Subst, Constraint0, Constraint),
    (
        Proof0 = apply_instance(_),
        Proof = Proof0
    ;
        Proof0 = superclass(Super0),
        apply_rec_subst_to_prog_constraint(Subst, Super0, Super),
        Proof = superclass(Super)
    ),
    map.set(Constraint, Proof, !ProofMap).

%-----------------------------------------------------------------------------%

apply_renaming_to_constraint_map(Renaming, !ConstraintMap) :-
    map.map_values_only(apply_renaming_to_prog_constraint(Renaming),
        !ConstraintMap).

apply_subst_to_constraint_map(Subst, !ConstraintMap) :-
    map.map_values_only(apply_subst_to_prog_constraint(Subst), !ConstraintMap).

apply_rec_subst_to_constraint_map(Subst, !ConstraintMap) :-
    map.map_values_only(apply_rec_subst_to_prog_constraint(Subst),
        !ConstraintMap).

%-----------------------------------------------------------------------------%
:- end_module hlds.type_rename.
%-----------------------------------------------------------------------------%
