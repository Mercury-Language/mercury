%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: type_assign.m.
% Main author: fjh (when this code was in typecheck_info.m, or earlier).
%
% This module defines the type_assign and args_type_assign types, plus some
% useful predicates that work with those types.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.type_assign.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%
%
% The type_assign data structure.
%

:- type type_assign
    --->    type_assign(
                ta_var_types            :: vartypes,
                ta_type_varset          :: tvarset,

                % Universally quantified type variables.
                ta_external_type_params :: external_type_params,

                % Type bindings.
                ta_type_bindings        :: tsubst,

                % The set of class constraints collected so far.
                ta_class_constraints    :: hlds_constraints,

                % For each constraint found to be redundant, why is it so?
                ta_constraint_proof_map :: constraint_proof_map,

                % Maps constraint identifiers to the actual constraints.
                ta_constraint_map       :: constraint_map
            ).

:- pred type_assign_get_var_types(type_assign::in,
    vartypes::out) is det.
:- pred type_assign_get_typevarset(type_assign::in,
    tvarset::out) is det.
:- pred type_assign_get_external_type_params(type_assign::in,
    external_type_params::out) is det.
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
:- pred type_assign_set_external_type_params(external_type_params::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_type_bindings(tsubst::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_typeclass_constraints(hlds_constraints::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_constraint_proof_map(constraint_proof_map::in,
    type_assign::in, type_assign::out) is det.
:- pred type_assign_set_constraint_map(constraint_map::in,
    type_assign::in, type_assign::out) is det.

:- pred type_assign_set_reduce_results(tvarset::in, tsubst::in,
    hlds_constraints::in, constraint_proof_map::in, constraint_map::in,
    type_assign::in, type_assign::out) is det.

%-----------------------------------------------------------------------------%
%
% The type_assign_set data structure.
%

:- type type_assign_set == list(type_assign).

:- pred type_assign_set_init(tvarset::in, vartypes::in,
    external_type_params::in, hlds_constraints::in, type_assign_set::out)
    is det.

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
    existq_tvars::out, vartypes::out, prog_constraints::out,
    constraint_proof_map::out, constraint_map::out,
    tvar_renaming::out, tvar_renaming::out) is det.

%-----------------------------------------------------------------------------%
%
% The args_type_assign data structure.
%

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

%-----------------------------------------------------------------------------%
%
% The args_type_assign_set data structure.
%

:- type args_type_assign_set == list(args_type_assign).

    % XXX document me
    %
:- func convert_args_type_assign_set(args_type_assign_set) = type_assign_set.

    % Same as convert_args_type_assign_set, but aborts when the args are
    % non-empty.
    %
:- func convert_args_type_assign_set_check_empty_args(args_type_assign_set) =
    type_assign_set.

%-----------------------------------------------------------------------------%
%
% Functions and predicates to help debug the typechecker.
%

:- func type_assign_set_to_pieces(type_assign_set, maybe(int), prog_varset)
    = list(format_component).

:- func args_type_assign_set_to_pieces(args_type_assign_set, maybe(int),
    prog_varset) = list(format_component).

:- pred type_checkpoint(string::in, module_info::in, prog_varset::in,
    type_assign_set::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

type_assign_get_var_types(TA, X) :-
    X = TA ^ ta_var_types.
type_assign_get_typevarset(TA, X) :-
    X = TA ^ ta_type_varset.
type_assign_get_external_type_params(TA, X) :-
    X = TA ^ ta_external_type_params.
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
type_assign_set_external_type_params(X, !TA) :-
    !TA ^ ta_external_type_params := X.
type_assign_set_type_bindings(X, !TA) :-
    !TA ^ ta_type_bindings := X.
type_assign_set_typeclass_constraints(X, !TA) :-
    !TA ^ ta_class_constraints := X.
type_assign_set_constraint_proof_map(X, !TA) :-
    !TA ^ ta_constraint_proof_map := X.
type_assign_set_constraint_map(X, !TA) :-
    !TA ^ ta_constraint_map := X.

type_assign_set_reduce_results(TVarSet, Bindings, Constraints, ProofMap,
        ConstraintMap, TypeAssign0, TypeAssign) :-
    TypeAssign0 = type_assign(VarTypes, _, ExternalTypeParams, _, _, _, _),
    TypeAssign = type_assign(VarTypes, TVarSet, ExternalTypeParams, Bindings,
        Constraints, ProofMap, ConstraintMap).

%-----------------------------------------------------------------------------%

type_assign_set_init(TypeVarSet, VarTypes, ExternalTypeParams, Constraints,
        TypeAssignSet) :-
    map.init(TypeBindings),
    map.init(ProofMap),
    map.init(ConstraintMap),
    TypeAssignSet = [type_assign(VarTypes, TypeVarSet, ExternalTypeParams,
        TypeBindings, Constraints, ProofMap, ConstraintMap)].

type_assign_set_get_final_info(TypeAssignSet,
        OldExternalTypeParams, OldExistQVars,
        OldExplicitVarTypes, NewTypeVarSet, NewExternalTypeParams,
        NewVarTypes, NewTypeConstraints, NewConstraintProofMap,
        NewConstraintMap, TSubst, ExistTypeRenaming) :-
    (
        TypeAssignSet = [TypeAssign | _]
        % XXX Why are we using only the first TypeAssign?
    ;
        TypeAssignSet = [],
        unexpected($module, $pred, "TypeAssignSet = []")
    ),

    TypeAssign = type_assign(VarTypes0, OldTypeVarSet, ExternalTypeParams,
        TypeBindings, HLDSTypeConstraints, ConstraintProofMap0,
        ConstraintMap0),

    ( if map.is_empty(TypeBindings) then
        VarTypes1 = VarTypes0,
        ConstraintProofMap = ConstraintProofMap0,
        ConstraintMap1 = ConstraintMap0,
        vartypes_types(VarTypes1, Types1),
        type_vars_list(Types1, TypeVars1)
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
    HLDSTypeConstraints = hlds_constraints(HLDSUnivConstraints,
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
    % too, to avoid errors when we apply the TSubst to the ExternalTypeParams.
    % (XXX should we do the same for TypeConstraints and ConstraintProofMap
    % too?)
    vartypes_types(OldExplicitVarTypes, ExplicitTypes),
    type_vars_list(ExplicitTypes, ExplicitTypeVars0),
    map.keys(ExistTypeRenaming, ExistQVarsToBeRenamed),
    list.delete_elems(OldExistQVars, ExistQVarsToBeRenamed,
        ExistQVarsToRemain),
    list.condense([ExistQVarsToRemain, ExternalTypeParams,
        TypeVars1, ExplicitTypeVars0], TypeVars2),
    list.sort_and_remove_dups(TypeVars2, TypeVars),

    % Next, create a new typevarset with the same number of variables.
    varset.squash(OldTypeVarSet, TypeVars, NewTypeVarSet, TSubst),

    % Finally, if necessary, rename the types and type class constraints
    % to use the new typevarset type variables.
    retrieve_prog_constraints(HLDSTypeConstraints, TypeConstraints),
    ( if map.is_empty(TSubst) then
        NewVarTypes = VarTypes1,
        NewExternalTypeParams = ExternalTypeParams,
        NewTypeConstraints = TypeConstraints,
        NewConstraintProofMap = ConstraintProofMap,
        NewConstraintMap = ConstraintMap
    else
        apply_variable_renaming_to_vartypes(TSubst, VarTypes1, NewVarTypes),
        map.apply_to_list(ExternalTypeParams, TSubst, NewExternalTypeParams),
        apply_variable_renaming_to_prog_constraints(TSubst,
            TypeConstraints, NewTypeConstraints),
        apply_variable_renaming_to_constraint_proof_map(TSubst,
            ConstraintProofMap, NewConstraintProofMap),
        apply_variable_renaming_to_constraint_map(TSubst,
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
    type_vars(Type, TypeVars),
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

%-----------------------------------------------------------------------------%

get_caller_arg_assign(ArgsTypeAssign) =
    ArgsTypeAssign ^ ata_caller_arg_assign.
get_callee_arg_types(ArgsTypeAssign) =
    ArgsTypeAssign ^ ata_callee_arg_types.
get_callee_constraints(ArgsTypeAssign) =
    ArgsTypeAssign ^ ata_callee_constraints.

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

convert_args_type_assign(ArgsTypeAssign) = TypeAssign :-
    ArgsTypeAssign = args_type_assign(TypeAssign0, _, Constraints0),
    type_assign_get_typeclass_constraints(TypeAssign0, OldConstraints),
    type_assign_get_type_bindings(TypeAssign0, Bindings),
    apply_rec_subst_to_constraints(Bindings, Constraints0, Constraints),
    merge_hlds_constraints(Constraints, OldConstraints, NewConstraints),
    type_assign_set_typeclass_constraints(NewConstraints,
        TypeAssign0, TypeAssign).

%-----------------------------------------------------------------------------%

type_assign_set_to_pieces([], _, _) = [].
type_assign_set_to_pieces([TypeAssign | TypeAssigns], MaybeSeq, VarSet) =
    type_assign_to_pieces(TypeAssign, MaybeSeq, VarSet) ++
    type_assign_set_to_pieces(TypeAssigns, inc_maybe_seq(MaybeSeq), VarSet).

args_type_assign_set_to_pieces([], _, _) = [].
args_type_assign_set_to_pieces([ArgTypeAssign | ArgTypeAssigns], MaybeSeq,
        VarSet) = Pieces :-
    % XXX Why does this simply pick the TypeAssign part of the ArgTypeAssign,
    % instead of invoking convert_args_type_assign?
    ArgTypeAssign = args_type_assign(TypeAssign, _ArgTypes, _Cnstrs),
    Pieces = type_assign_to_pieces(TypeAssign, MaybeSeq, VarSet) ++
        args_type_assign_set_to_pieces(ArgTypeAssigns, inc_maybe_seq(MaybeSeq),
            VarSet).

%-----------------------------------------------------------------------------%

:- func type_assign_to_pieces(type_assign, maybe(int), prog_varset)
    = list(format_component).

type_assign_to_pieces(TypeAssign, MaybeSeq, VarSet) = Pieces :-
    (
        MaybeSeq = yes(N),
        SeqPieces0 = [words("Type assignment"), int_fixed(N), suffix(":"), nl],
        ( if N > 1 then
            SeqPieces = [blank_line | SeqPieces0]
        else
            SeqPieces = SeqPieces0
        )
    ;
        MaybeSeq = no,
        SeqPieces = []
    ),
    type_assign_get_external_type_params(TypeAssign, ExternalTypeParams),
    type_assign_get_var_types(TypeAssign, VarTypes),
    type_assign_get_typeclass_constraints(TypeAssign, Constraints),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TypeVarSet),
    vartypes_vars(VarTypes, Vars),
    (
        ExternalTypeParams = [],
        HeadPieces = []
    ;
        ExternalTypeParams = [_ | _],
        VarsStr =
            mercury_vars_to_string(TypeVarSet, varnums, ExternalTypeParams),
        HeadPieces = [words("some [" ++ VarsStr ++ "]"), nl]
    ),
    TypePieces = type_assign_types_to_pieces(Vars, VarSet, VarTypes,
        TypeBindings, TypeVarSet, no),
    ConstraintPieces = type_assign_hlds_constraints_to_pieces(Constraints,
        TypeBindings, TypeVarSet),
    Pieces = SeqPieces ++ HeadPieces ++ TypePieces ++ ConstraintPieces ++ [nl].

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
    ( if search_var_type(VarTypes, Var, Type) then
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
    else
        Pieces = type_assign_types_to_pieces(Vars, VarSet, VarTypes,
            TypeBindings, TypeVarSet, FoundOne)
    ).

:- func type_with_bindings_to_string(mer_type, tvarset, tsubst) = string.

type_with_bindings_to_string(Type0, TypeVarSet, TypeBindings) = Str :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_builtin_qualifiers_from_type(Type1, Type),
    Str = mercury_type_to_string(TypeVarSet, print_name_only, Type).

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

:- func inc_maybe_seq(maybe(int)) = maybe(int).

inc_maybe_seq(no) = no.
inc_maybe_seq(yes(N)) = yes(N + 1).

:- func varnums = var_name_print.

varnums = print_name_and_num.

%-----------------------------------------------------------------------------%

type_checkpoint(Msg, ModuleInfo, VarSet, TypeAssignSet, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_types, DoCheckPoint),
    (
        DoCheckPoint = yes,
        do_type_checkpoint(Msg, ModuleInfo, VarSet, TypeAssignSet, !IO)
    ;
        DoCheckPoint = no
    ).

:- pred do_type_checkpoint(string::in, module_info::in, prog_varset::in,
    type_assign_set::in, io::di, io::uo) is det.

do_type_checkpoint(Msg, ModuleInfo, VarSet, TypeAssignSet, !IO) :-
    io.write_string("At ", !IO),
    io.write_string(Msg, !IO),
    io.write_string(": ", !IO),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
    maybe_report_stats(Statistics, !IO),
    io.write_string("\n", !IO),
    ( if
        Statistics = yes,
        TypeAssignSet = [TypeAssign | _]
    then
        type_assign_get_var_types(TypeAssign, VarTypes),
        vartypes_count(VarTypes, VarTypesCount),
        io.format("\t`var -> type' map: count = %d\n",
            [i(VarTypesCount)], !IO),
        type_assign_get_type_bindings(TypeAssign, TypeBindings),
        map.count(TypeBindings, TypeBindingsCount),
        io.format("\t`type var -> type' map: count = %d\n",
            [i(TypeBindingsCount)], !IO)
    else
        true
    ),
    write_type_assign_set(TypeAssignSet, VarSet, !IO).

:- pred write_type_assign_set(type_assign_set::in, prog_varset::in,
    io::di, io::uo) is det.

write_type_assign_set([], _, !IO).
write_type_assign_set([TypeAssign | TypeAssigns], VarSet, !IO) :-
    io.write_string("\t", !IO),
    write_type_assign(TypeAssign, VarSet, !IO),
    io.write_string("\n", !IO),
    write_type_assign_set(TypeAssigns, VarSet, !IO).

:- pred write_type_assign(type_assign::in, prog_varset::in, io::di, io::uo)
    is det.

write_type_assign(TypeAssign, VarSet, !IO) :-
    type_assign_get_external_type_params(TypeAssign, ExternalTypeParams),
    type_assign_get_var_types(TypeAssign, VarTypes),
    type_assign_get_typeclass_constraints(TypeAssign, Constraints),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TypeVarSet),
    vartypes_vars(VarTypes, Vars),
    (
        ExternalTypeParams = []
    ;
        ExternalTypeParams = [_ | _],
        io.write_string("some [", !IO),
        mercury_output_vars(TypeVarSet, varnums, ExternalTypeParams, !IO),
        io.write_string("]\n\t", !IO)
    ),
    write_type_assign_types(VarSet, TypeVarSet, VarTypes, TypeBindings,
        no, Vars, !IO),
    write_type_assign_hlds_constraints(TypeVarSet, TypeBindings, Constraints,
        !IO),
    io.write_string("\n", !IO).

:- pred write_type_assign_types(prog_varset::in, tvarset::in, vartypes::in,
    tsubst::in, bool::in, list(prog_var)::in, io::di, io::uo) is det.

write_type_assign_types(_, _, _, _, FoundOne, [], !IO) :-
    (
        FoundOne = no,
        io.write_string("(No variables were assigned a type)", !IO)
    ;
        FoundOne = yes
    ).
write_type_assign_types(VarSet, TypeVarSet, VarTypes, TypeBindings,
        FoundOne, [Var | Vars], !IO) :-
    ( if search_var_type(VarTypes, Var, Type) then
        (
            FoundOne = yes,
            io.write_string("\n\t", !IO)
        ;
            FoundOne = no
        ),
        mercury_output_var(VarSet, varnums, Var, !IO),
        io.write_string(": ", !IO),
        write_type_with_bindings(TypeVarSet, TypeBindings, Type, !IO),
        write_type_assign_types(VarSet, TypeVarSet, VarTypes, TypeBindings,
            yes, Vars, !IO)
    else
        write_type_assign_types(VarSet, TypeVarSet, VarTypes, TypeBindings,
            FoundOne, Vars, !IO)
    ).

    % write_type_with_bindings writes out a type after applying the
    % type bindings.
    %
:- pred write_type_with_bindings(tvarset::in, tsubst::in, mer_type::in,
    io::di, io::uo) is det.

write_type_with_bindings(TypeVarSet, TypeBindings, Type0, !IO) :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_builtin_qualifiers_from_type(Type1, Type),
    mercury_output_type(TypeVarSet, print_name_and_num, Type, !IO).

:- pred write_type_assign_hlds_constraints(tvarset::in, tsubst::in,
    hlds_constraints::in, io::di, io::uo) is det.

write_type_assign_hlds_constraints(TypeVarSet, TypeBindings, Constraints,
        !IO) :-
    Constraints =
        hlds_constraints(ConstraintsToProve, AssumedConstraints, _, _),
    write_type_assign_constraints(TypeVarSet, TypeBindings,
        "&", AssumedConstraints, no, !IO),
    write_type_assign_constraints(TypeVarSet, TypeBindings,
        "<=", ConstraintsToProve, no, !IO).

:- pred write_type_assign_constraints(tvarset::in, tsubst::in,
    string::in, list(hlds_constraint)::in, bool::in, io::di, io::uo) is det.

write_type_assign_constraints(_, _, _, [], _, !IO).
write_type_assign_constraints(TypeVarSet, TypeBindings, Operator,
        [Constraint | Constraints], FoundOne, !IO) :-
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
    write_type_assign_constraints(TypeVarSet, TypeBindings, Operator,
        Constraints, yes, !IO).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.type_assign.
%-----------------------------------------------------------------------------%
