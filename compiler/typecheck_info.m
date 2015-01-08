%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: typecheck_info.m.
% Main author: fjh.
%
% This module defines the typecheck_info and type_assign types, plus some
% useful predicates that work with those types.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.typecheck_info.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module map.

%-----------------------------------------------------------------------------%
%
% The typecheck_info data structure's initializer and finalizer.
%

:- type typecheck_info.

:- pred typecheck_info_init(module_info::in, pred_id::in, bool::in,
    tvarset::in, prog_varset::in, vartypes::in, head_type_params::in,
    hlds_constraints::in, import_status::in, pred_markers::in,
    list(error_spec)::in, typecheck_info::out) is det.

    % typecheck_info_get_final_info(Info, OldHeadTypeParams, OldExistQVars,
    %   OldExplicitVarTypes, NewTypeVarSet, New* ..., TypeRenaming,
    %   ExistTypeRenaming):
    %
    % Extracts the final inferred types from Info.
    %
    % OldHeadTypeParams should be the type variables from the head of the
    % predicate.
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
:- pred typecheck_info_get_final_info(typecheck_info::in, list(tvar)::in,
    existq_tvars::in, vartypes::in, tvarset::out, existq_tvars::out,
    vartypes::out, prog_constraints::out,
    constraint_proof_map::out, constraint_map::out,
    tvar_renaming::out, tvar_renaming::out) is det.

%-----------------------------------------------------------------------------%
%
% Basic access predicates for typecheck_info, and the purpose-specific
% types that some of them return.
%

:- type overloaded_symbol_map == map(overloaded_symbol, list(prog_context)).

:- type overloaded_symbol
    --->    overloaded_pred(
                simple_call_id,
                list(pred_id)
            )
    ;       overloaded_func(
                cons_id,
                list(cons_type_info_source)
            ).

:- type type_error_clause_context
    --->    type_error_clause_context(
                % The outermost context for a type error is the clause
                % that was being typechecked when the error was found.

                % The tecc_pred_id field gives the identity of the predicate,
                % and the tecc_module_info field allows us to convert that
                % into the information we actually need about the predicate,
                % such as its name.
                tecc_module_info                :: module_info,
                tecc_pred_id                    :: pred_id,

                % The markers of the pred being checked. The code that
                % needs to know this to generate good error messages could
                % get the pred_info using the two fields above and then
                % look up the pred_markers in there, but we have them anyway
                % when we construct the typecheck_info, and the space it takes
                % up is not worth worrying about.
                tecc_pred_markers               :: pred_markers,

                % Which clause of the predicate are we checking?
                tecc_clause_num                 :: int,

                % The context of the clause, which will be the context
                % of its head.
                tecc_clause_context             :: prog_context,

                % Variable names in the clause being checked.
                tecc_varset                     :: prog_varset
            ).

:- pred typecheck_info_get_module_info(typecheck_info::in,
    module_info::out) is det.
:- pred typecheck_info_get_error_clause_context(typecheck_info::in,
    type_error_clause_context::out) is det.
:- pred typecheck_info_get_type_assign_set(typecheck_info::in,
    type_assign_set::out) is det.
:- pred typecheck_info_get_ambiguity_error_limit(typecheck_info::in,
    int::out) is det.

:- pred typecheck_info_get_pred_id(typecheck_info::in,
    pred_id::out) is det.
:- pred typecheck_info_get_calls_are_fully_qualified(typecheck_info::in,
    is_fully_qualified::out) is det.
:- pred typecheck_info_get_is_field_access_function(typecheck_info::in,
    maybe(import_status)::out) is det.
:- pred typecheck_info_get_non_overload_errors(typecheck_info::in,
    list(error_spec)::out) is det.
:- pred typecheck_info_get_overload_error(typecheck_info::in,
    maybe(error_spec)::out) is det.
:- pred typecheck_info_get_overloaded_symbol_map(typecheck_info::in,
    overloaded_symbol_map::out) is det.
:- pred typecheck_info_get_ambiguity_warn_limit(typecheck_info::in,
    int::out) is det.

:- pred typecheck_info_set_type_assign_set(type_assign_set::in,
    typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_overload_error(maybe(error_spec)::in,
    typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_overloaded_symbol_map(overloaded_symbol_map::in,
    typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_non_overload_errors(list(error_spec)::in,
    typecheck_info::in, typecheck_info::out) is det.

%-----------------------------------------------------------------------------%
%
% Utility predicates for typecheck_info.
%

:- pred typecheck_info_get_module_name(typecheck_info::in, module_name::out)
    is det.
:- pred typecheck_info_get_preds(typecheck_info::in, predicate_table::out)
    is det.
:- pred typecheck_info_get_types(typecheck_info::in, type_table::out) is det.
:- pred typecheck_info_get_ctors(typecheck_info::in, cons_table::out) is det.

:- pred typecheck_info_add_overloaded_symbol(overloaded_symbol::in,
    prog_context::in, typecheck_info::in, typecheck_info::out) is det.

:- pred typecheck_info_add_error(error_spec::in,
    typecheck_info::in, typecheck_info::out) is det.

:- pred typecheck_info_get_all_errors(typecheck_info::in,
    list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%
% The type_assign and type_assign_set data structures.
%

:- type type_assign_set ==  list(type_assign).

:- type type_assign
    --->    type_assign(
                ta_var_types            :: vartypes,
                ta_type_varset          :: tvarset,

                % Universally quantified type variables.
                ta_head_type_params     :: head_type_params,

                % Type bindings.
                ta_type_bindings        :: tsubst,

                % The set of class constraints collected so far.
                ta_class_constraints    :: hlds_constraints,

                % For each constraint found to be redundant, why is it so?
                ta_constraint_proof_map :: constraint_proof_map,

                % Maps constraint identifiers to the actual constraints.
                ta_constraint_map       :: constraint_map
            ).

%-----------------------------------------------------------------------------%
%
% Access predicates for type_assign.
%

:- pred type_assign_get_var_types(type_assign::in,
    vartypes::out) is det.
:- pred type_assign_get_typevarset(type_assign::in,
    tvarset::out) is det.
:- pred type_assign_get_head_type_params(type_assign::in,
    head_type_params::out) is det.
:- pred type_assign_get_type_bindings(type_assign::in,
    tsubst::out) is det.
:- pred type_assign_get_typeclass_constraints(type_assign::in,
    hlds_constraints::out) is det.
:- pred type_assign_get_constraint_proof_map(type_assign::in,
    constraint_proof_map::out) is det.
:- pred type_assign_get_constraint_map(type_assign::in,
    constraint_map::out) is det.

:- pred type_assign_set_var_types(vartypes::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_typevarset(tvarset::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_head_type_params(head_type_params::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_type_bindings(tsubst::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_typeclass_constraints(hlds_constraints::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_constraint_proof_map(constraint_proof_map::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_constraint_map(constraint_map::in,
    type_assign::in, type_assign::out) is det.

:- pred type_assign_set_reduce_results(tsubst::in, tvarset::in,
    hlds_constraints::in, constraint_proof_map::in, constraint_map::in,
    type_assign::in, type_assign::out) is det.

%-----------------------------------------------------------------------------%

:- type args_type_assign_set == list(args_type_assign).

:- type args_type_assign
    --->    args_type_assign(
                % Type assignment.
                ata_caller_arg_assign   :: type_assign,

                % Types of callee args, renamed apart.
                ata_callee_arg_types    :: list(mer_type),

                % Constraints from callee, renamed apart.
                ata_callee_constraints  :: hlds_constraints
            ).

:- func get_caller_arg_assign(args_type_assign) = type_assign.
:- func get_callee_arg_types(args_type_assign) = list(mer_type).
:- func get_callee_constraints(args_type_assign) = hlds_constraints.

    % XXX document me
    %
:- func convert_args_type_assign_set(args_type_assign_set) = type_assign_set.

    % Same as convert_args_type_assign_set, but aborts when the args are
    % non-empty.
    %
:- func convert_args_type_assign_set_check_empty_args(args_type_assign_set) =
    type_assign_set.

%-----------------------------------------------------------------------------%

:- type cons_type_info
    --->    cons_type_info(
                % Type variables.
                cti_varset          :: tvarset,

                % Existentially quantified type vars.
                cti_exit_tvars      :: existq_tvars,

                % Constructor type.
                cti_result_type     :: mer_type,

                % Types of the arguments.
                cti_arg_types       :: list(mer_type),

                % Constraints introduced by this constructor (e.g. if it is
                % actually a function, or if it is an existentially quantified
                % data constructor).
                cti_constraints     :: hlds_constraints,

                cti_source          :: cons_type_info_source
            ).

:- type cons_type_info_source
    --->    source_type(type_ctor)
    ;       source_builtin_type(string)
    ;       source_get_field_access(type_ctor)
    ;       source_set_field_access(type_ctor)
    ;       source_apply(string)
    ;       source_pred(pred_id).

:- func project_cons_type_info_source(cons_type_info) = cons_type_info_source.

%-----------------------------------------------------------------------------%

:- pred write_type_assign_set(type_assign_set::in, prog_varset::in,
    io::di, io::uo) is det.

:- func type_assign_set_to_pieces(type_assign_set, maybe(int), prog_varset)
    = list(format_component).

:- pred write_args_type_assign_set(args_type_assign_set::in, prog_varset::in,
    io::di, io::uo) is det.

:- func args_type_assign_set_to_pieces(args_type_assign_set, maybe(int),
    prog_varset) = list(format_component).

%-----------------------------------------------------------------------------%

    % Used for debugging typechecking.
    %
:- pred type_checkpoint(string::in, typecheck_info::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module int.
:- import_module require.
:- import_module string.
:- import_module set.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type typecheck_sub_info
    --->    typecheck_sub_info(
                % Are calls from the body of the predicate we are checking
                % guaranteed to be already fully qualified? In some cases,
                % such as when the body was read in from a .opt file,
                % they will be.
                tcsi_calls_are_fully_qualified  :: is_fully_qualified,

                % Is the pred we are checking a field access function? If so,
                % there should only be a field access function application
                % in the body, not predicate or function calls or constructor
                % applications, and we will need to know the predicate's
                % import status, so we don't generate errors when the function
                % is opt-imported into other modules.
                tcsi_is_field_access_function   :: maybe(import_status),

                % The list of errors found so far (if any), with one exception:
                % any errors about overloading are in the overload_error field.
                tcsi_non_overload_errors        :: list(error_spec),

                % Have we already generated a warning or error message about
                % highly ambiguous overloading? If yes, this has the message.
                tcsi_overload_error             :: maybe(error_spec),

                % The symbols used by the current predicate that have
                % more than one accessible definition, mapped to the unsorted
                % list of the locations that refer to them.
                tcsi_overloaded_symbol_map      :: overloaded_symbol_map,

                % The value of the option --typecheck-ambiguity-error-limit.
                tcsi_ambiguity_error_limit      :: int
            ).

:- type typecheck_info
    --->    typecheck_info(
                % The four most frequently used parts of the conceptual
                % typecheck_info are here in this cell. The less frequently
                % used parts are in the typecheck_sub_info, or (if they are
                % needed for the generation of type error messages) in the
                % type_error_clause_context.

                tci_sub_info                    :: typecheck_sub_info,

                % The part of the information needed to generate good error
                % messages for type errors that remains valid during the
                % typechecking of an entire clause. As for the Information
                % needed to generate good error messages that changes as
                % we traverse the body of a clause, that is passed around
                % separately in typecheck.m.
                tci_error_clause_context        :: type_error_clause_context,

                % This is the main piece of information that we are computing
                % and which gets updated as we go along.
                %
                % XXX We could pass the type assign set around *without*
                % putting it into the typecheck_info. Since it would avoid
                % most memory allocations for updated typecheck_infos, this
                % change could lead to a speedup.
                tci_type_assign_set             :: type_assign_set,

                % The value of the option --typecheck-ambiguity-warn-limit.
                tci_ambiguity_warn_limit        :: int
            ).

%-----------------------------------------------------------------------------%

typecheck_info_init(ModuleInfo, PredId, IsFieldAccessFunction,
        TypeVarSet, ClauseVarSet, VarTypes, HeadTypeParams, Constraints,
        Status, PredMarkers, NonOverloadErrors, Info) :-
    CallsAreFullyQualified = calls_are_fully_qualified(PredMarkers),
    (
        IsFieldAccessFunction = no,
        MaybeFieldAccessFunction = no
    ;
        IsFieldAccessFunction = yes,
        MaybeFieldAccessFunction = yes(Status)
    ),
    map.init(TypeBindings),
    map.init(Proofs),
    map.init(ConstraintMap),
    OverloadErrors = no,
    map.init(OverloadedSymbols),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_int_option(Globals, typecheck_ambiguity_warn_limit,
        WarnLimit),
    globals.lookup_int_option(Globals, typecheck_ambiguity_error_limit,
        ErrorLimit),
    TypeAssignSet = [type_assign(VarTypes, TypeVarSet, HeadTypeParams,
        TypeBindings, Constraints, Proofs, ConstraintMap)],
    SubInfo = typecheck_sub_info(CallsAreFullyQualified,
        MaybeFieldAccessFunction,
        NonOverloadErrors, OverloadErrors, OverloadedSymbols, ErrorLimit),
    ClauseContext = type_error_clause_context(ModuleInfo, PredId,
        PredMarkers, 0, term.context_init, ClauseVarSet),
    Info = typecheck_info(SubInfo, ClauseContext, TypeAssignSet, WarnLimit).

typecheck_info_get_final_info(Info, OldHeadTypeParams, OldExistQVars,
        OldExplicitVarTypes, NewTypeVarSet, NewHeadTypeParams,
        NewVarTypes, NewTypeConstraints, NewConstraintProofMap,
        NewConstraintMap, TSubst, ExistTypeRenaming) :-
    typecheck_info_get_type_assign_set(Info, TypeAssignSet),
    (
        TypeAssignSet = [TypeAssign | _],
        type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
        type_assign_get_typevarset(TypeAssign, OldTypeVarSet),
        type_assign_get_var_types(TypeAssign, VarTypes0),
        type_assign_get_type_bindings(TypeAssign, TypeBindings),
        type_assign_get_typeclass_constraints(TypeAssign, HLDSTypeConstraints),
        type_assign_get_constraint_proof_map(TypeAssign, ConstraintProofMap0),
        type_assign_get_constraint_map(TypeAssign, ConstraintMap0),

        ( map.is_empty(TypeBindings) ->
            VarTypes1 = VarTypes0,
            ConstraintProofMap = ConstraintProofMap0,
            ConstraintMap1 = ConstraintMap0,
            vartypes_types(VarTypes1, Types1),
            type_vars_list(Types1, TypeVars1)
        ;
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

        HLDSTypeConstraints = hlds_constraints(HLDSUnivConstraints,
            HLDSExistConstraints, _, _),
        list.foldl(update_constraint_map, HLDSUnivConstraints,
            ConstraintMap1, ConstraintMap2),
        list.foldl(update_constraint_map, HLDSExistConstraints,
            ConstraintMap2, ConstraintMap),

        % Figure out how we should rename the existential types
        % in the type declaration (if any).

        get_existq_tvar_renaming(OldHeadTypeParams, OldExistQVars,
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
        % There may also be some type variables in the HeadTypeParams
        % which do not occur in the type of any variable (e.g. this can happen
        % in the case of code containing type errors). We'd better keep those,
        % too, to avoid errors when we apply the TSubst to the HeadTypeParams.
        % (XXX should we do the same for TypeConstraints and ConstraintProofMap
        % too?)

        vartypes_types(OldExplicitVarTypes, ExplicitTypes),
        type_vars_list(ExplicitTypes, ExplicitTypeVars0),
        map.keys(ExistTypeRenaming, ExistQVarsToBeRenamed),
        list.delete_elems(OldExistQVars, ExistQVarsToBeRenamed,
            ExistQVarsToRemain),
        list.condense([ExistQVarsToRemain, HeadTypeParams,
            TypeVars1, ExplicitTypeVars0], TypeVars2),
        list.sort_and_remove_dups(TypeVars2, TypeVars),

        % Next, create a new typevarset with the same number of variables.
        varset.squash(OldTypeVarSet, TypeVars, NewTypeVarSet, TSubst),

        % Finally, if necessary, rename the types and type class constraints
        % to use the new typevarset type variables.
        retrieve_prog_constraints(HLDSTypeConstraints, TypeConstraints),
        ( map.is_empty(TSubst) ->
            NewVarTypes = VarTypes1,
            NewHeadTypeParams = HeadTypeParams,
            NewTypeConstraints = TypeConstraints,
            NewConstraintProofMap = ConstraintProofMap,
            NewConstraintMap = ConstraintMap
        ;
            transform_var_types(apply_variable_renaming_to_type(TSubst),
                VarTypes1, NewVarTypes),
            map.apply_to_list(HeadTypeParams, TSubst, NewHeadTypeParams),
            apply_variable_renaming_to_prog_constraints(TSubst,
                TypeConstraints, NewTypeConstraints),
            apply_variable_renaming_to_constraint_proof_map(TSubst,
                ConstraintProofMap, NewConstraintProofMap),
            apply_variable_renaming_to_constraint_map(TSubst,
                ConstraintMap, NewConstraintMap)
        )
    ;
        TypeAssignSet = [],
        unexpected($module, $pred, "TypeAssignSet = []")
    ).

    % Fully expand the types of the variables by applying the type bindings.
    % We also accumulate the set of type variables we have seen so far,
    % since doing so saves having to do a separate traversal for that.
    %
:- pred expand_types(tsubst::in, mer_type::in, mer_type::out,
    set(tvar)::in, set(tvar)::out) is det.

expand_types(TypeSubst, Type0, Type, !TypeVarsSet) :-
    apply_rec_subst_to_type(TypeSubst, Type0, Type),
    type_vars(Type, TypeVars),
    set.insert_list(TypeVars, !TypeVarsSet).

    % We rename any existentially quantified type variables which get mapped
    % to other type variables, unless they are mapped to universally quantified
    % type variables from the head of the predicate.
    %
:- pred get_existq_tvar_renaming(list(tvar)::in, existq_tvars::in, tsubst::in,
    tvar_renaming::out) is det.

get_existq_tvar_renaming(OldHeadTypeParams, ExistQVars, TypeBindings,
        ExistTypeRenaming) :-
    list.foldl(get_existq_tvar_renaming_2(OldHeadTypeParams, TypeBindings),
        ExistQVars, map.init, ExistTypeRenaming).

:- pred get_existq_tvar_renaming_2(existq_tvars::in, tsubst::in,
    tvar::in, tvar_renaming::in, tvar_renaming::out) is det.

get_existq_tvar_renaming_2(OldHeadTypeParams, TypeBindings, TVar, !Renaming) :-
    (
        tvar_maps_to_tvar(TypeBindings, TVar, NewTVar),
        NewTVar \= TVar,
        \+ list.member(NewTVar, OldHeadTypeParams)
    ->
        map.det_insert(TVar, NewTVar, !Renaming)
    ;
        true
    ).

:- pred tvar_maps_to_tvar(tsubst::in, tvar::in, tvar::out) is semidet.

tvar_maps_to_tvar(TypeBindings, TVar0, TVar) :-
    ( map.search(TypeBindings, TVar0, Type) ->
        Type = type_variable(TVar1, _),
        tvar_maps_to_tvar(TypeBindings, TVar1, TVar)
    ;
        TVar = TVar0
    ).

%-----------------------------------------------------------------------------%

typecheck_info_get_error_clause_context(Info, X) :-
    X = Info ^ tci_error_clause_context.
typecheck_info_get_type_assign_set(Info, X) :-
    X = Info ^ tci_type_assign_set.
typecheck_info_get_ambiguity_warn_limit(Info, X) :-
    X = Info ^ tci_ambiguity_warn_limit.

typecheck_info_get_module_info(Info, X) :-
    X = Info ^ tci_error_clause_context ^ tecc_module_info.
typecheck_info_get_pred_id(Info, X) :-
    X = Info ^ tci_error_clause_context ^ tecc_pred_id.

typecheck_info_get_calls_are_fully_qualified(Info, X) :-
    X = Info ^ tci_sub_info ^ tcsi_calls_are_fully_qualified.
typecheck_info_get_is_field_access_function(Info, X) :-
    X = Info ^ tci_sub_info ^ tcsi_is_field_access_function.
typecheck_info_get_non_overload_errors(Info, X) :-
    X = Info ^ tci_sub_info ^ tcsi_non_overload_errors.
typecheck_info_get_overload_error(Info, X) :-
    X = Info ^ tci_sub_info ^ tcsi_overload_error.
typecheck_info_get_overloaded_symbol_map(Info, X) :-
    X = Info ^ tci_sub_info ^ tcsi_overloaded_symbol_map.
typecheck_info_get_ambiguity_error_limit(Info, X) :-
    X = Info ^ tci_sub_info ^ tcsi_ambiguity_error_limit.

typecheck_info_set_type_assign_set(X, !Info) :-
    !Info ^ tci_type_assign_set := X.

typecheck_info_set_non_overload_errors(X, !Info) :-
    !Info ^ tci_sub_info ^ tcsi_non_overload_errors := X.
typecheck_info_set_overload_error(X, !Info) :-
    !Info ^ tci_sub_info ^ tcsi_overload_error := X.
typecheck_info_set_overloaded_symbol_map(X, !Info) :-
    !Info ^ tci_sub_info ^ tcsi_overloaded_symbol_map := X.

% Access statistics from before the change on 2015 jan 9.
%
%  i      read      same      diff   same%
%  0   7325016         0         0              module_info
%  1        62         0    519838   0.000%     called_pred_id
%  2        62    878563   3641386  19.437%     arg_num
%  3    240136    773992   2491358  23.703%     context
%  4        86        53   1245023   0.004%     unify_context
%  5   9555205        22   5883791   0.000%     type_assign_set
%  6   2421093         0         0              ambiguity_warn_limit
%  7    681078         0         0              pred_id
%  8       129         0         0              import_status
%  9   1704110         0         0              pred_markers
% 10    856891         0         0              is_field_access_function
% 11        43         0         0              varset
% 12    401290         0       124   0.000%     non_overload_errors
% 13    401218         0       164   0.000%     overload_error
% 14    188296         0    188132   0.000%     overloaded_symbols 
% 15       204         0         0              ambiguity_error_limit

% Access statistics from during the change on 2015 jan 9.
% (The final commit changed the set of fields and getters/setters
% still further.)
%
%  i      read      same      diff   same%
%  0   1046873         0         0          error_clause_stats
%  1  10228916        23   6337890   0.00%  type_assign_set
%  2   2550889         0         0          ambiguity_warn_limit
%  3   7802441         0         0          module_info
%  4    735390         0         0          pred_id
%  5   1821662         0         0          pred_markers
%  6         0         0         0          varset
%  7       129         0         0          pred_import_status
%  8    914248         0         0          is_field_access_function
%  9    431258         0       125   0.00%  non_overload_errors
% 10    431185         0       164   0.00%  overload_error
% 11    192779         0    192575   0.00%  overloaded_symbol_map
% 12       204         0         0          ambiguity_error_limit

%-----------------------------------------------------------------------------%

typecheck_info_get_module_name(Info, Name) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_name(ModuleInfo, Name).
typecheck_info_get_preds(Info, Preds) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, Preds).
typecheck_info_get_types(Info, Types) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_type_table(ModuleInfo, Types).
typecheck_info_get_ctors(Info, Ctors) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_cons_table(ModuleInfo, Ctors).

typecheck_info_add_overloaded_symbol(Symbol, Context, !Info) :-
    typecheck_info_get_overloaded_symbol_map(!.Info, OverloadedSymbolMap0),
    ( map.search(OverloadedSymbolMap0, Symbol, OldContexts) ->
        Contexts = [Context | OldContexts],
        map.det_update(Symbol, Contexts,
            OverloadedSymbolMap0, OverloadedSymbolMap)
    ;
        Contexts = [Context],
        map.det_insert(Symbol, Contexts,
            OverloadedSymbolMap0, OverloadedSymbolMap)
    ),
    typecheck_info_set_overloaded_symbol_map(OverloadedSymbolMap, !Info).

typecheck_info_add_error(Error, !Info) :-
    typecheck_info_get_non_overload_errors(!.Info, Errors0),
    Errors = [Error | Errors0],
    typecheck_info_set_non_overload_errors(Errors, !Info).

typecheck_info_get_all_errors(Info, Errors) :-
    typecheck_info_get_non_overload_errors(Info, Errors0),
    typecheck_info_get_overload_error(Info, MaybeOverloadError),
    (
        MaybeOverloadError = no,
        Errors = Errors0
    ;
        MaybeOverloadError = yes(OverloadError),
        Errors = [OverloadError | Errors0]
    ).

%-----------------------------------------------------------------------------%

type_assign_get_var_types(TA, X) :-
    X = TA ^ ta_var_types.
type_assign_get_typevarset(TA, X) :-
    X = TA ^ ta_type_varset.
type_assign_get_head_type_params(TA, X) :-
    X = TA ^ ta_head_type_params.
type_assign_get_type_bindings(TA, X) :-
    X = TA ^ ta_type_bindings.
type_assign_get_typeclass_constraints(TA, X) :-
    X = TA ^ ta_class_constraints.
type_assign_get_constraint_proof_map(TA, X) :-
    X = TA ^ ta_constraint_proof_map.
type_assign_get_constraint_map(TA, X) :-
    X = TA ^ ta_constraint_map.

type_assign_set_var_types(X, !TA) :-
    !TA ^ ta_var_types := X.
type_assign_set_typevarset(X, !TA) :-
    !TA ^ ta_type_varset := X.
type_assign_set_head_type_params(X, !TA) :-
    !TA ^ ta_head_type_params := X.
type_assign_set_type_bindings(X, !TA) :-
    !TA ^ ta_type_bindings := X.
type_assign_set_typeclass_constraints(X, !TA) :-
    !TA ^ ta_class_constraints := X.
type_assign_set_constraint_proof_map(X, !TA) :-
    !TA ^ ta_constraint_proof_map := X.
type_assign_set_constraint_map(X, !TA) :-
    !TA ^ ta_constraint_map := X.

type_assign_set_reduce_results(Bindings, TVarSet, Constraints, ProofMap,
        ConstraintMap, !TA) :-
    % This should allocate just one new type_assign, whereas separate calls
    % to the predicates above to set each of these fields would allocate
    % several.
    !TA ^ ta_type_bindings := Bindings,
    !TA ^ ta_type_varset := TVarSet,
    !TA ^ ta_class_constraints := Constraints,
    !TA ^ ta_constraint_proof_map := ProofMap,
    !TA ^ ta_constraint_map := ConstraintMap.

%-----------------------------------------------------------------------------%

convert_args_type_assign_set([]) = [].
convert_args_type_assign_set([ArgsTypeAssign | ArgsTypeAssigns]) =
    [convert_args_type_assign(ArgsTypeAssign) |
    convert_args_type_assign_set(ArgsTypeAssigns)].

convert_args_type_assign_set_check_empty_args([]) = [].
convert_args_type_assign_set_check_empty_args([ArgTypeAssign | ArgTypeAssigns])
        = Result :-
    ArgTypeAssign = args_type_assign(_, Args, _),
    (
        Args = [],
        Result =
            [convert_args_type_assign(ArgTypeAssign) |
            convert_args_type_assign_set_check_empty_args(ArgTypeAssigns)]
    ;
        Args = [_ | _],
        % This should never happen, since the arguments should all have been
        % processed at this point.
        unexpected($module, $pred, "Args != []")
    ).

:- func convert_args_type_assign(args_type_assign) = type_assign.

convert_args_type_assign(args_type_assign(TypeAssign0, _, Constraints0))
        = TypeAssign :-
    type_assign_get_typeclass_constraints(TypeAssign0, OldConstraints),
    type_assign_get_type_bindings(TypeAssign0, Bindings),
    apply_rec_subst_to_constraints(Bindings, Constraints0, Constraints),
    merge_hlds_constraints(Constraints, OldConstraints, NewConstraints),
    type_assign_set_typeclass_constraints(NewConstraints,
        TypeAssign0, TypeAssign).

get_caller_arg_assign(ArgsTypeAssign) =
    ArgsTypeAssign ^ ata_caller_arg_assign.
get_callee_arg_types(ArgsTypeAssign) =
    ArgsTypeAssign ^ ata_callee_arg_types.
get_callee_constraints(ArgsTypeAssign) =
    ArgsTypeAssign ^ ata_callee_constraints.

project_cons_type_info_source(CTI) = CTI ^ cti_source.

%-----------------------------------------------------------------------------%

:- func varnums = bool.

varnums = yes.

:- func inc_maybe_seq(maybe(int)) = maybe(int).

inc_maybe_seq(no) = no.
inc_maybe_seq(yes(N)) = yes(N + 1).

write_type_assign_set([], _, !IO).
write_type_assign_set([TypeAssign | TypeAssigns], VarSet, !IO) :-
    io.write_string("\t", !IO),
    write_type_assign(TypeAssign, VarSet, !IO),
    io.write_string("\n", !IO),
    write_type_assign_set(TypeAssigns, VarSet, !IO).

type_assign_set_to_pieces([], _, _) = [].
type_assign_set_to_pieces([TypeAssign | TypeAssigns], MaybeSeq, VarSet) =
    type_assign_to_pieces(TypeAssign, MaybeSeq, VarSet) ++
    type_assign_set_to_pieces(TypeAssigns, inc_maybe_seq(MaybeSeq), VarSet).

write_args_type_assign_set([], _, !IO).
write_args_type_assign_set([ArgTypeAssign | ArgTypeAssigns], VarSet, !IO) :-
    % XXX Why does this simply pick the TypeAssign part of the ArgTypeAssign,
    % instead of invoking convert_args_type_assign?
    ArgTypeAssign = args_type_assign(TypeAssign, _ArgTypes, _Cnstrs),
    io.write_string("\t", !IO),
    write_type_assign(TypeAssign, VarSet, !IO),
    io.write_string("\n", !IO),
    write_args_type_assign_set(ArgTypeAssigns, VarSet, !IO).

args_type_assign_set_to_pieces([], _, _) = [].
args_type_assign_set_to_pieces([ArgTypeAssign | ArgTypeAssigns], MaybeSeq,
        VarSet) = Pieces :-
    % XXX Why does this simply pick the TypeAssign part of the ArgTypeAssign,
    % instead of invoking convert_args_type_assign?
    ArgTypeAssign = args_type_assign(TypeAssign, _ArgTypes, _Cnstrs),
    Pieces = type_assign_to_pieces(TypeAssign, MaybeSeq, VarSet) ++
        args_type_assign_set_to_pieces(ArgTypeAssigns, inc_maybe_seq(MaybeSeq),
            VarSet).

:- pred write_type_assign(type_assign::in, prog_varset::in, io::di, io::uo)
    is det.

write_type_assign(TypeAssign, VarSet, !IO) :-
    type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
    type_assign_get_var_types(TypeAssign, VarTypes),
    type_assign_get_typeclass_constraints(TypeAssign, Constraints),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TypeVarSet),
    vartypes_vars(VarTypes, Vars),
    (
        HeadTypeParams = []
    ;
        HeadTypeParams = [_ | _],
        io.write_string("some [", !IO),
        mercury_output_vars(TypeVarSet, varnums, HeadTypeParams, !IO),
        io.write_string("]\n\t", !IO)
    ),
    write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings, TypeVarSet,
        no, !IO),
    write_type_assign_hlds_constraints(Constraints, TypeBindings, TypeVarSet,
        !IO),
    io.write_string("\n", !IO).

:- func type_assign_to_pieces(type_assign, maybe(int), prog_varset)
    = list(format_component).

type_assign_to_pieces(TypeAssign, MaybeSeq, VarSet) = Pieces :-
    (
        MaybeSeq = yes(N),
        SeqPieces0 = [words("Type assignment"), int_fixed(N), suffix(":"), nl],
        ( N > 1 ->
            SeqPieces = [blank_line | SeqPieces0]
        ;
            SeqPieces = SeqPieces0
        )
    ;
        MaybeSeq = no,
        SeqPieces = []
    ),
    type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
    type_assign_get_var_types(TypeAssign, VarTypes),
    type_assign_get_typeclass_constraints(TypeAssign, Constraints),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TypeVarSet),
    vartypes_vars(VarTypes, Vars),
    (
        HeadTypeParams = [],
        HeadPieces = []
    ;
        HeadTypeParams = [_ | _],
        VarsStr = mercury_vars_to_string(TypeVarSet, varnums, HeadTypeParams),
        HeadPieces = [words("some [" ++ VarsStr ++ "]"), nl]
    ),
    TypePieces = type_assign_types_to_pieces(Vars, VarSet, VarTypes,
        TypeBindings, TypeVarSet, no),
    ConstraintPieces = type_assign_hlds_constraints_to_pieces(Constraints,
        TypeBindings, TypeVarSet),
    Pieces = SeqPieces ++ HeadPieces ++ TypePieces ++ ConstraintPieces ++ [nl].

:- pred write_type_assign_types(list(prog_var)::in, prog_varset::in,
    vartypes::in, tsubst::in, tvarset::in, bool::in, io::di, io::uo) is det.

write_type_assign_types([], _, _, _, _, FoundOne, !IO) :-
    (
        FoundOne = no,
        io.write_string("(No variables were assigned a type)", !IO)
    ;
        FoundOne = yes
    ).
write_type_assign_types([Var | Vars], VarSet, VarTypes, TypeBindings,
        TypeVarSet, FoundOne, !IO) :-
    ( search_var_type(VarTypes, Var, Type) ->
        (
            FoundOne = yes,
            io.write_string("\n\t", !IO)
        ;
            FoundOne = no
        ),
        mercury_output_var(VarSet, varnums, Var, !IO),
        io.write_string(": ", !IO),
        write_type_with_bindings(Type, TypeVarSet, TypeBindings, !IO),
        write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings,
            TypeVarSet, yes, !IO)
    ;
        write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings,
            TypeVarSet, FoundOne, !IO)
    ).

:- func type_assign_types_to_pieces(list(prog_var), prog_varset,
    vartypes, tsubst, tvarset, bool) = list(format_component).

type_assign_types_to_pieces([], _, _, _, _, FoundOne) = Pieces :-
    (
        FoundOne = no,
        Pieces = [words("(No variables were assigned a type)")]
    ;
        FoundOne = yes,
        Pieces = []
    ).
type_assign_types_to_pieces([Var | Vars], VarSet, VarTypes, TypeBindings,
        TypeVarSet, FoundOne) = Pieces :-
    ( search_var_type(VarTypes, Var, Type) ->
        (
            FoundOne = yes,
            PrefixPieces = [nl]
        ;
            FoundOne = no,
            PrefixPieces = []
        ),
        VarStr = mercury_var_to_string(VarSet, varnums, Var),
        TypeStr = type_with_bindings_to_string(Type, TypeVarSet, TypeBindings),
        AssignPieces = [fixed(VarStr), suffix(":"), words(TypeStr)],
        TailPieces = type_assign_types_to_pieces(Vars, VarSet, VarTypes,
            TypeBindings, TypeVarSet, yes),
        Pieces = PrefixPieces ++ AssignPieces ++ TailPieces
    ;
        Pieces = type_assign_types_to_pieces(Vars, VarSet, VarTypes,
            TypeBindings, TypeVarSet, FoundOne)
    ).

:- pred write_type_assign_hlds_constraints(hlds_constraints::in,
    tsubst::in, tvarset::in, io::di, io::uo) is det.

write_type_assign_hlds_constraints(Constraints, TypeBindings, TypeVarSet,
        !IO) :-
    Constraints =
        hlds_constraints(ConstraintsToProve, AssumedConstraints, _, _),
    write_type_assign_constraints("&", AssumedConstraints,
        TypeBindings, TypeVarSet, no, !IO),
    write_type_assign_constraints("<=", ConstraintsToProve,
        TypeBindings, TypeVarSet, no, !IO).

:- func type_assign_hlds_constraints_to_pieces(hlds_constraints,
    tsubst, tvarset) = list(format_component).

type_assign_hlds_constraints_to_pieces(Constraints, TypeBindings, TypeVarSet)
        = Pieces1 ++ Pieces2 :-
    Constraints =
        hlds_constraints(ConstraintsToProve, AssumedConstraints, _, _),
    PiecesList1 = type_assign_constraints_to_pieces_list("&",
        AssumedConstraints, TypeBindings, TypeVarSet, no),
    PiecesList2 = type_assign_constraints_to_pieces_list("<=",
        ConstraintsToProve, TypeBindings, TypeVarSet, no),
    Pieces1 = component_list_to_line_pieces(PiecesList1, []),
    Pieces2 = component_list_to_line_pieces(PiecesList2, []).

:- pred write_type_assign_constraints(string::in, list(hlds_constraint)::in,
    tsubst::in, tvarset::in, bool::in, io::di, io::uo) is det.

write_type_assign_constraints(_, [], _, _, _, !IO).
write_type_assign_constraints(Operator, [Constraint | Constraints],
        TypeBindings, TypeVarSet, FoundOne, !IO) :-
    (
        FoundOne = no,
        io.write_strings(["\n\t", Operator, " "], !IO)
    ;
        FoundOne = yes,
        io.write_string(",\n\t   ", !IO)
    ),
    apply_rec_subst_to_constraint(TypeBindings, Constraint, BoundConstraint),
    retrieve_prog_constraint(BoundConstraint, ProgConstraint),
    mercury_output_constraint(TypeVarSet, varnums, ProgConstraint, !IO),
    write_type_assign_constraints(Operator, Constraints, TypeBindings,
        TypeVarSet, yes, !IO).

:- func type_assign_constraints_to_pieces_list(string, list(hlds_constraint),
    tsubst, tvarset, bool) = list(list(format_component)).

type_assign_constraints_to_pieces_list(_, [], _, _, _) = [].
type_assign_constraints_to_pieces_list(Operator, [Constraint | Constraints],
        TypeBindings, TypeVarSet, FoundOne) = [ThisPieces] ++ TailPieceLists :-
    (
        FoundOne = no,
        Prefix = Operator ++ " "
    ;
        FoundOne = yes,
        Prefix = "   "
    ),
    apply_rec_subst_to_constraint(TypeBindings, Constraint, BoundConstraint),
    retrieve_prog_constraint(BoundConstraint, ProgConstraint),
    ThisPieces = [fixed(Prefix ++
        mercury_constraint_to_string(TypeVarSet, ProgConstraint))],
    TailPieceLists = type_assign_constraints_to_pieces_list(Operator,
        Constraints, TypeBindings, TypeVarSet, yes).

    % write_type_with_bindings writes out a type after applying the
    % type bindings.
    %
:- pred write_type_with_bindings(mer_type::in, tvarset::in, tsubst::in,
    io::di, io::uo) is det.

write_type_with_bindings(Type0, TypeVarSet, TypeBindings, !IO) :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_builtin_qualifiers_from_type(Type1, Type),
    mercury_output_type(TypeVarSet, yes, Type, !IO).

:- func type_with_bindings_to_string(mer_type, tvarset, tsubst) = string.

type_with_bindings_to_string(Type0, TypeVarSet, TypeBindings) = Str :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_builtin_qualifiers_from_type(Type1, Type),
    Str = mercury_type_to_string(TypeVarSet, no, Type).

%-----------------------------------------------------------------------------%

type_checkpoint(Msg, Info, !IO) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_types, DoCheckPoint),
    (
        DoCheckPoint = yes,
        do_type_checkpoint(Msg, Info, !IO)
    ;
        DoCheckPoint = no
    ).

:- pred do_type_checkpoint(string::in, typecheck_info::in, io::di, io::uo)
    is det.

do_type_checkpoint(Msg, Info, !IO) :-
    io.write_string("At ", !IO),
    io.write_string(Msg, !IO),
    io.write_string(": ", !IO),
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
    maybe_report_stats(Statistics, !IO),
    io.write_string("\n", !IO),
    typecheck_info_get_type_assign_set(Info, TypeAssignSet),
    (
        Statistics = yes,
        TypeAssignSet = [TypeAssign | _]
    ->
        type_assign_get_var_types(TypeAssign, VarTypes),
        checkpoint_vartypes_stats("\t`var -> type' map", VarTypes, !IO),
        type_assign_get_type_bindings(TypeAssign, TypeBindings),
        checkpoint_tree_stats("\t`type var -> type' map", TypeBindings, !IO)
    ;
        true
    ),
    typecheck_info_get_error_clause_context(Info, ClauseContext),
    VarSet = ClauseContext ^ tecc_varset,
    write_type_assign_set(TypeAssignSet, VarSet, !IO).

:- pred checkpoint_tree_stats(string::in, map(_K, _V)::in, io::di, io::uo)
    is det.

checkpoint_tree_stats(Description, Tree, !IO) :-
    map.count(Tree, Count),
    io.write_string(Description, !IO),
    io.write_string(": count = ", !IO),
    io.write_int(Count, !IO),
    io.write_string("\n", !IO).

:- pred checkpoint_vartypes_stats(string::in, vartypes::in, io::di, io::uo)
    is det.

checkpoint_vartypes_stats(Description, Tree, !IO) :-
    vartypes_count(Tree, Count),
    io.write_string(Description, !IO),
    io.write_string(": count = ", !IO),
    io.write_int(Count, !IO),
    io.write_string("\n", !IO).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_info.
%-----------------------------------------------------------------------------%
