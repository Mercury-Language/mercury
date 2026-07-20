%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2015, 2018, 2020-2021, 2024-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: type_assign.m.
% Main author: fjh (when this code was in typecheck_info.m, or earlier).
%
% This module defines the type_assign and args_type_assign types, plus some
% useful predicates that work with those types.
%
%---------------------------------------------------------------------------%

:- module check_hlds.type_assign.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.vartypes.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%
%
% The type_assign data structure.
%

:- type type_assign
    --->    type_assign(
                ta_var_types            :: vartypes,
                ta_type_varset          :: tvarset,

                % Existentially quantified type variables.
                ta_existq_tvars         :: list(tvar),

                % Type bindings.
                ta_type_bindings        :: tsubst,

                % The list of coerce constraints collected so far.
                ta_coerce_constraints   :: list(coerce_constraint),

                % The set of class constraints collected so far.
                ta_class_constraints    :: hlds_constraint_db,

                % For each constraint found to be redundant, why is it so?
                ta_constraint_proof_map :: constraint_proof_map,

                % Maps constraint identifiers to the actual constraints.
                ta_constraint_map       :: constraint_map
            ).

:- type coerce_constraint
    --->    coerce_constraint(
                % One or both sides should contain a type variable.
                coerce_from             :: mer_type,
                coerce_to               :: mer_type,
                coerce_context          :: prog_context,
                coerce_var              :: prog_var,
                coerce_status           :: coerce_constraint_status,
                % This list should be nonempty for the status values
                % unsatisfiable and not_yet_resolved.
                coerce_fails            :: list(coerce_fail)
            ).

:- type coerce_constraint_status
    --->    need_to_check
    ;       unsatisfiable
    ;       not_yet_resolved
    ;       satisfied_but_redundant.

    % The purpose of this type is let report_invalid_coerce_from_to
    % describe *in specific detail* the cause of the coercion failure.
    %
    % Each function symbol in this list represents one point
    % in the operation of the typecheck_coerce_between_types predicate
    % and its subcontractors where it could fail.
    %
    % XXX While the current set of failure points works for typechecking
    % I don't think this is the ideal set for explaining coercion failures
    % to users. Specifically, at the moment we check whether both types
    % are du types quite late; I think we should check this VERY early.
    %
    % NOTE: there is no point in discussing the details of what we
    % record for each kind of failure until we decide on what set of failures
    % we want.
    %
    % XXX I (zs) am not sure whether all these kinds of coerce fails
    % can actually reach report_invalid_coerce_from_to. This is because
    % I cannot rule out either of the following the possibilities.
    %
    % - A failure point lower down in the call tree will never be reached
    %   because some code higher up in the call tree will find a related
    %   but not identical failure point first, preventing execution from
    %   reaching the lower failure point.
    %
    % - Some coercion failures may occur during typechecking while the
    %   specific types bound to some type variables are not yet known,
    %   but they will go away once the bindings of those type variables
    %   become known.
    %
    % XXX Also, I am not sure that these names are the ones that
    % best describe each kind of coercion failure.
:- type coerce_fail
    --->    different_base_types(
                % The two base type_ctors must be different.
                dbt_from_type           :: mer_type,
                dbt_from_base_type_ctor :: type_ctor,
                dbt_to_type             :: mer_type,
                dbt_to_base_type_ctor   :: type_ctor
            )
    ;       unknown_or_nonground_type(
                existq_tvars            :: list(tvar),
                % If both these arguments are yes(...), then
                % type_is_ground_except_vars must fail for at least one
                % of the wrapped types, when that predicate is passed
                % the value in the existq_tvars field.
                maybe_from_type         :: maybe(mer_type),
                maybe_to_type           :: maybe(mer_type)
            )
    ;       incompatible_types(
                % The two types have different principal function symbols.
                it_from_type            :: mer_type,
                it_to_type              :: mer_type
            )
    ;       cannot_coerce_type_vars(
                % At least one of these must be a type_var.
                cctv_from_type          :: mer_type,
                cctv_to_type            :: mer_type
            )
    ;       cannot_unify_type_vars(
                cutv_from_type          :: mer_type,
                cutv_to_type            :: mer_type
            )
    ;       non_du_type_ctor(
                % The from-type and the to-type have the same base type_ctor,
                % but this base type_ctor is not a du type.
                ndtc_from_type          :: mer_type,
                ndtc_to_type            :: mer_type
            )
    ;       should_be_invariant_arg(
                % The from-type and to-type are different, even though
                % they should be identical.
                %
                % XXX We *really* need some extra info here
                % that we use to explain to users the *reason*
                % these should be identical.
                sbia_from_type          :: mer_type,
                sbia_to_type            :: mer_type
            ).

:- pred type_assign_get_var_types(type_assign::in,
    vartypes::out) is det.
:- pred type_assign_get_typevarset(type_assign::in,
    tvarset::out) is det.
:- pred type_assign_get_existq_tvars(type_assign::in,
    list(tvar)::out) is det.
:- pred type_assign_get_type_bindings(type_assign::in,
    tsubst::out) is det.
:- pred type_assign_get_coerce_constraints(type_assign::in,
    list(coerce_constraint)::out) is det.
:- pred type_assign_get_constraint_db(type_assign::in,
    hlds_constraint_db::out) is det.
:- pred type_assign_get_constraint_proof_map(type_assign::in,
    constraint_proof_map::out) is det.
:- pred type_assign_get_constraint_map(type_assign::in,
    constraint_map::out) is det.

:- pred type_assign_set_var_types(vartypes::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_typevarset(tvarset::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_existq_tvars(list(tvar)::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_type_bindings(tsubst::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_coerce_constraints(list(coerce_constraint)::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_constraint_db(hlds_constraint_db::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_constraint_proof_map(constraint_proof_map::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_constraint_map(constraint_map::in,
    type_assign::in, type_assign::out) is det.

:- pred type_assign_set_reduce_results(tvarset::in, tsubst::in,
    hlds_constraint_db::in, constraint_proof_map::in, constraint_map::in,
    type_assign::in, type_assign::out) is det.

%---------------------------------------------------------------------------%
%
% The type_assign_set data structure.
%

:- type type_assign_set == list(type_assign).

:- pred type_assign_set_init(tvarset::in, vartypes::in, list(tvar)::in,
    hlds_constraint_db::in, type_assign_set::out) is det.

    % type_assign_set_get_final_info(TypeAssignSet, OldExternalTypeParams,
    %   OldExistQVars, OldExplicitVarTypes, NewTypeVarSet, New* ...,
    %   TypeRenaming, ExistTypeRenaming):
    %
    % Extracts the final inferred types from TypeAssignSet.
    %
    % OldExternalTypeParams should be the type variables from the head of the
    % predicate. XXX How about type variables from existentially quantified
    % types returned by predicates called in the body?
    % OldExistQVars should be the declared existentially quantified
    % type variables (if any).
    % OldExplicitVarTypes is the vartypes map containing the explicit
    % type qualifications.
    % New* is the newly inferred types, in NewTypeVarSet.
    % TypeRenaming is a map to rename things from the old TypeVarSet
    % to the NewTypeVarSet.
    % ExistTypeRenaming is a map (which should be applied *before*
    % applying TypeRenaming) to rename existential type variables
    % in OldExistQVars.
    %
:- pred type_assign_set_get_final_info(type_assign_set::in,
    list(tvar)::in, existq_tvars::in, vartypes::in, tvarset::out,
    existq_tvars::out, vartypes::out, univ_exist_constraints::out,
    constraint_proof_map::out, constraint_map::out,
    tvar_renaming::out, tvar_renaming::out) is det.

%---------------------------------------------------------------------------%
%
% The args_type_assign data structure.
%

:- type args_type_assign_source
    --->    atas_pred(pred_id)
            % If the argument types come from a plain call, then their source
            % should be "atas_pred(PredId)" where PredId is the callee.
    ;       atas_higher_order_call(prog_var)
            % If the argument types come from a higher order call, then
            % their source should be "atas_higher_order_call(Var)" where Var
            % is the higher-order variable being called.
    ;       atas_cons(cons_type_info_source)
            % If the argument types come from a ConsId, then their source
            % should be "atas_cons(ConsTypeInfoSource)", where
            % ConsTypeInfoSource is the description of the source of ConsId.
            %
            % Note that the ConsId may refer to a pred_id when the program
            % (tries to) partially apply a predicate or function.
    ;       atas_ensure_have_a_type.
            % If the argument types come from the compiler try to
            % ensure that a variable has *a* type, then the source should be
            % atas_ensure_have_a_type.
            %
            % Since ensure_vars_have_a_type does not impose any expectations
            % on the type of a variable, while atas_ensure_have_a_type
            % may occur in args_type_assigns, it should *not* occur
            % in any args_type_assigns involved in argument type errors.

:- type args_type_assign
    --->    args_type_assign(
                % Type assignment.
                ata_caller_arg_assign       :: type_assign,

                % Types of callee/cons_id args, renamed apart.
                % XXX Renamed apart ... from what?
                ata_expected_arg_types      :: list(mer_type),

                % Constraints from callee/cons_id, renamed apart.
                % XXX Renamed apart ... from what?
                ata_expected_constraints    :: hlds_constraint_db,

                % The source of the expected arg types and their constraints.
                ata_source                  :: args_type_assign_source
            ).

:- func get_caller_arg_assign(args_type_assign) = type_assign.
:- func get_expected_arg_types(args_type_assign) = list(mer_type).
:- func get_expected_constraints(args_type_assign) = hlds_constraint_db.

%---------------------------------------------------------------------------%
%
% The args_type_assign_set data structure.
%

:- type args_type_assign_set == list(args_type_assign).

    % XXX document me
    %
:- func args_type_assign_set_to_type_assign_set(args_type_assign_set)
    = type_assign_set.

%---------------------------------------------------------------------------%
%
% The cons_type_info data structure.
%

% XXX Values of type cons_type_info are not held in the typecheck_info,
% though values of type cons_type_info_source are. Values of this type
% are currently computed on demand, though they should be stored precomputed
% for each data constructor for each type in the HLDS.
:- type cons_type_info
    --->    cons_type_info(
                % Type variables.
                cti_varset          :: tvarset,

                % Existentially quantified type vars.
                cti_existq_tvars    :: existq_tvars,

                % Constructor type.
                cti_result_type     :: mer_type,

                % Types of the arguments.
                cti_arg_types       :: list(mer_type),

                % Constraints introduced by this constructor (e.g. if it is
                % actually a function, or if it is an existentially quantified
                % data constructor).
                cti_constraints     :: hlds_constraint_db,

                cti_source          :: cons_type_info_source
            ).

:- type cons_type_info_source
    --->    source_type(type_ctor, du_ctor)
    ;       source_builtin_type(string)
    ;       source_field_access(field_access_type, type_ctor, du_ctor, string)
            % get or set, type, user cons_id, field name
    ;       source_apply(string)
    ;       source_pred(pred_id).

:- func project_cons_type_info_source(cons_type_info) = cons_type_info_source.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.type_rename.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_subst.

:- import_module map.
:- import_module require.
:- import_module set.
:- import_module varset.

%---------------------------------------------------------------------------%

type_assign_get_var_types(TA, X) :-
    X = TA ^ ta_var_types.
type_assign_get_typevarset(TA, X) :-
    X = TA ^ ta_type_varset.
type_assign_get_existq_tvars(TA, X) :-
    X = TA ^ ta_existq_tvars.
type_assign_get_type_bindings(TA, X) :-
    X = TA ^ ta_type_bindings.
type_assign_get_coerce_constraints(TA, X) :-
    X = TA ^ ta_coerce_constraints.
type_assign_get_constraint_db(TA, X) :-
    X = TA ^ ta_class_constraints.
type_assign_get_constraint_proof_map(TA, X) :-
    X = TA ^ ta_constraint_proof_map.
type_assign_get_constraint_map(TA, X) :-
    X = TA ^ ta_constraint_map.

type_assign_set_var_types(X, !TA) :-
    !TA ^ ta_var_types := X.
type_assign_set_typevarset(X, !TA) :-
    !TA ^ ta_type_varset := X.
type_assign_set_existq_tvars(X, !TA) :-
    !TA ^ ta_existq_tvars := X.
type_assign_set_type_bindings(X, !TA) :-
    !TA ^ ta_type_bindings := X.
type_assign_set_coerce_constraints(X, !TA) :-
    !TA ^ ta_coerce_constraints := X.
type_assign_set_constraint_db(X, !TA) :-
    !TA ^ ta_class_constraints := X.
type_assign_set_constraint_proof_map(X, !TA) :-
    !TA ^ ta_constraint_proof_map := X.
type_assign_set_constraint_map(X, !TA) :-
    !TA ^ ta_constraint_map := X.

type_assign_set_reduce_results(TVarSet, Bindings, Constraints, ProofMap,
        ConstraintMap, TypeAssign0, TypeAssign) :-
    TypeAssign0 = type_assign(VarTypes, _, ExternalTypeParams, _,
        Coercions, _, _, _),
    TypeAssign = type_assign(VarTypes, TVarSet, ExternalTypeParams, Bindings,
        Coercions, Constraints, ProofMap, ConstraintMap).

%---------------------------------------------------------------------------%

type_assign_set_init(TypeVarSet, VarTypes, ExternalTypeParams, Constraints,
        TypeAssignSet) :-
    map.init(TypeBindings),
    Coercions = [],
    map.init(ProofMap),
    map.init(ConstraintMap),
    TypeAssignSet = [type_assign(VarTypes, TypeVarSet, ExternalTypeParams,
        TypeBindings, Coercions, Constraints, ProofMap, ConstraintMap)].

type_assign_set_get_final_info(TypeAssignSet,
        OldExternalTypeParams, OldExistQVars,
        OldExplicitVarTypes, NewTypeVarSet, NewExternalTypeParams,
        NewVarTypes, NewTypeConstraints, NewConstraintProofMap,
        NewConstraintMap, OldToNewTRename, ExistTypeRenaming) :-
    (
        TypeAssignSet = [TypeAssign | _]
        % XXX Why are we using only the first TypeAssign?
    ;
        TypeAssignSet = [],
        unexpected($pred, "TypeAssignSet = []")
    ),

    TypeAssign = type_assign(VarTypes0, OldTypeVarSet, ExternalTypeParams,
        TypeBindings, _Coercions0, HLDSTypeConstraints, ConstraintProofMap0,
        ConstraintMap0),

    ( if map.is_empty(TypeBindings) then
        VarTypes1 = VarTypes0,
        ConstraintProofMap = ConstraintProofMap0,
        ConstraintMap1 = ConstraintMap0,
        vartypes_types(VarTypes1, Types1),
        type_vars_in_types(Types1, TypeVars1)
    else
        transform_foldl_var_types(expand_types(TypeBindings),
            VarTypes0, VarTypes1, set.init, TypeVarsSet1),
        set.to_sorted_list(TypeVarsSet1, TypeVars1),
        apply_rec_subst_to_constraint_proof_map(TypeBindings,
            ConstraintProofMap0, ConstraintProofMap),
        apply_rec_subst_to_constraint_map(TypeBindings,
            ConstraintMap0, ConstraintMap1)
    ),

    % When inferring the typeclass constraints, the universal constraints
    % here may be assumed (if this is the last pass) but will not have been
    % eliminated during context reduction, hence they will not yet be
    % in the constraint map. Since they may be required, put them in now.
    %
    % Additionally, existential constraints are assumed so don't need to be
    % eliminated during context reduction, so they need to be put in the
    % constraint map now.
    %
    % XXX The definition of hlds_constraint_db states that the first two
    % fields contain *unproven* and *assumed* constraints respectively.
    % While unproven constraints are *usually* universal and assumed
    % constraints are *usually* existential (both in the goal being checked),
    % this is NOT GUARANTEED TO BE ALWAYS THE CASE.
    HLDSTypeConstraints = hlds_constraint_db(HLDSUnivConstraints,
        HLDSExistConstraints, _, _),
    list.foldl(update_constraint_map, HLDSUnivConstraints,
        ConstraintMap1, ConstraintMap2),
    list.foldl(update_constraint_map, HLDSExistConstraints,
        ConstraintMap2, ConstraintMap),

    % Figure out how we should rename the existential types
    % in the type declaration (if any).
    get_existq_tvar_renaming(OldExternalTypeParams, OldExistQVars,
        TypeBindings, ExistTypeRenaming),

    % We used to just use the OldTypeVarSet that we got from the type
    % assignment.
    %
    % However, that caused serious efficiency problems, because the
    % typevarsets get bigger and bigger with each inference step. Instead,
    % we now construct a new typevarset NewTypeVarSet which contains
    % only the variables we want, and we rename the type variables so that
    % they fit into this new typevarset.

    % First, find the set (sorted list) of type variables that we need.
    % This must include any type variables in the inferred types, the
    % explicit type qualifications, and any existentially typed variables
    % that will remain in the declaration.
    %
    % There may also be some type variables in the ExternalTypeParams
    % which do not occur in the type of any variable (e.g. this can happen
    % in the case of code containing type errors). We'd better keep those,
    % too, to avoid errors when we apply the TRename to the ExternalTypeParams.
    % (XXX should we do the same for TypeConstraints and ConstraintProofMap
    % too?)
    vartypes_types(OldExplicitVarTypes, ExplicitTypes),
    type_vars_in_types(ExplicitTypes, ExplicitTypeVars0),
    map.keys(ExistTypeRenaming, ExistQVarsToBeRenamed),
    list.delete_elems(OldExistQVars,
        ExistQVarsToBeRenamed, ExistQVarsToRemain),
    list.condense([ExistQVarsToRemain, ExternalTypeParams,
        TypeVars1, ExplicitTypeVars0], TypeVars2),
    list.sort_and_remove_dups(TypeVars2, TypeVars),

    % Next, create a new typevarset with the same number of variables.
    varset.squash(OldTypeVarSet, TypeVars, NewTypeVarSet, OldToNewTRename),

    % Finally, if necessary, rename the types and type class constraints
    % to use the new typevarset type variables.
    retrieve_univ_exist_constraints(HLDSTypeConstraints, TypeConstraints),
    ( if map.is_empty(OldToNewTRename) then
        NewVarTypes = VarTypes1,
        NewExternalTypeParams = ExternalTypeParams,
        NewTypeConstraints = TypeConstraints,
        NewConstraintProofMap = ConstraintProofMap,
        NewConstraintMap = ConstraintMap
    else
        rename_vars_in_vartypes(OldToNewTRename, VarTypes1, NewVarTypes),
        map.apply_to_list(ExternalTypeParams, OldToNewTRename,
            NewExternalTypeParams),
        apply_renaming_to_univ_exist_constraints(OldToNewTRename,
            TypeConstraints, NewTypeConstraints),
        apply_renaming_to_constraint_proof_map(OldToNewTRename,
            ConstraintProofMap, NewConstraintProofMap),
        apply_renaming_to_constraint_map(OldToNewTRename,
            ConstraintMap, NewConstraintMap)
    ).

    % Fully expand the types of the variables by applying the type bindings.
    % We also accumulate the set of type variables we have seen so far,
    % since doing so saves having to do a separate traversal for that.
    %
:- pred expand_types(tsubst::in, mer_type::in, mer_type::out,
    set(tvar)::in, set(tvar)::out) is det.

expand_types(TypeSubst, Type0, Type, !TypeVarsSet) :-
    apply_rec_subst_to_type(TypeSubst, Type0, Type),
    type_vars_in_type(Type, TypeVars),
    set.insert_list(TypeVars, !TypeVarsSet).

    % We rename any existentially quantified type variables which get mapped
    % to other type variables, unless they are mapped to universally quantified
    % type variables from the head of the predicate.
    %
:- pred get_existq_tvar_renaming(list(tvar)::in, existq_tvars::in, tsubst::in,
    tvar_renaming::out) is det.

get_existq_tvar_renaming(OldExternalTypeParams, ExistQVars, TypeBindings,
        ExistTypeRenaming) :-
    list.foldl(get_existq_tvar_renaming_2(OldExternalTypeParams, TypeBindings),
        ExistQVars, map.init, ExistTypeRenaming).

:- pred get_existq_tvar_renaming_2(existq_tvars::in, tsubst::in,
    tvar::in, tvar_renaming::in, tvar_renaming::out) is det.

get_existq_tvar_renaming_2(OldExternalTypeParams, TypeBindings, TVar,
        !Renaming) :-
    ( if
        tvar_maps_to_tvar(TypeBindings, TVar, NewTVar),
        NewTVar \= TVar,
        not list.member(NewTVar, OldExternalTypeParams)
    then
        map.det_insert(TVar, NewTVar, !Renaming)
    else
        true
    ).

:- pred tvar_maps_to_tvar(tsubst::in, tvar::in, tvar::out) is semidet.

tvar_maps_to_tvar(TypeBindings, TVar0, TVar) :-
    ( if map.search(TypeBindings, TVar0, Type) then
        Type = type_variable(TVar1, _),
        tvar_maps_to_tvar(TypeBindings, TVar1, TVar)
    else
        TVar = TVar0
    ).

%---------------------------------------------------------------------------%

get_caller_arg_assign(ArgsTypeAssign) =
    ArgsTypeAssign ^ ata_caller_arg_assign.
get_expected_arg_types(ArgsTypeAssign) =
    ArgsTypeAssign ^ ata_expected_arg_types.
get_expected_constraints(ArgsTypeAssign) =
    ArgsTypeAssign ^ ata_expected_constraints.

%---------------------------------------------------------------------------%

args_type_assign_set_to_type_assign_set([]) = [].
args_type_assign_set_to_type_assign_set([ArgsTypeAssign | ArgsTypeAssigns]) =
    [args_type_assign_to_type_assign(ArgsTypeAssign) |
    args_type_assign_set_to_type_assign_set(ArgsTypeAssigns)].

:- func args_type_assign_to_type_assign(args_type_assign) = type_assign.

args_type_assign_to_type_assign(ArgsTypeAssign) = TypeAssign :-
    ArgsTypeAssign = args_type_assign(TypeAssign0, _, ConstraintDb0, _),
    type_assign_get_constraint_db(TypeAssign0, OldConstraintDb),
    type_assign_get_type_bindings(TypeAssign0, Bindings),
    apply_rec_subst_to_constraint_db(Bindings, ConstraintDb0, ConstraintDb),
    merge_hlds_constraint_dbs(ConstraintDb, OldConstraintDb, NewConstraintDb),
    type_assign_set_constraint_db(NewConstraintDb,
        TypeAssign0, TypeAssign).

%-----------------------------------------------------------------------------%

project_cons_type_info_source(CTI) = CTI ^ cti_source.

%---------------------------------------------------------------------------%
:- end_module check_hlds.type_assign.
%---------------------------------------------------------------------------%
