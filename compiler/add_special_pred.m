%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: add_special_pred.m.
%
% This module handles the declaration of unify, compare and (if needed)
% index predicates for the types defined or imported by the module
% being compiled.
%
%---------------------------------------------------------------------------%

:- module hlds.add_special_pred.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

%---------------------------------------------------------------------------%

:- pred add_special_pred_decl_defns_for_type_maybe_lazily(type_ctor::in,
    hlds_type_defn::in, module_info::in, module_info::out) is det.

    % This predicate defines unify and compare predicates for the given type.
    % This can be any type when invoked from the _maybe_lazily version.
    % When invoked from make_hlds_passes.m, which is the only place outside
    % this module from where it should be invoked, it is used to generate
    % unify and compare predicates for some builtin types, using an abstract
    % type body to signal the fact that these types are builtins.
    %
:- pred add_special_pred_decl_defns_for_type_eagerly(tvarset::in,
    mer_type::in, type_ctor::in, hlds_type_body::in, type_status::in,
    prog_context::in, module_info::in, module_info::out) is det.

    % add_special_pred_decl_defn(SpecialPredId, TVarSet, Type, TypeCtor,
    %   TypeBody, TypeStatus, TypeContext, !ModuleInfo).
    %
    % Add declarations and clauses for a special predicate.
    % This is used by unify_proc.m to add a unification predicate
    % for an imported type for which special predicates are being
    % generated only when a unification procedure is requested
    % during mode analysis.
    %
:- pred add_special_pred_decl_defn(special_pred_id::in, tvarset::in,
    mer_type::in, type_ctor::in, hlds_type_body::in, type_status::in,
    prog_context::in, module_info::in, module_info::out) is det.

    % add_special_pred_decl(SpecialPredId, TVarSet, Type, TypeCtor,
    %   TypeStatus, TypeContext, !ModuleInfo).
    %
    % Add declarations for a special predicate.
    % This is used by higher_order.m when specializing an in-in
    % unification for an imported type for which unification procedures
    % are generated lazily.
    %
:- pred add_special_pred_decl(special_pred_id::in,
    tvarset::in, mer_type::in, type_ctor::in, type_status::in,
    prog_context::in, module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%

    % add_lazily_generated_unify_pred(TypeCtor, UnifyPredId_for_Type,
    %   !ModuleInfo):
    %
    % For most imported unification procedures, we delay generating
    % declarations and clauses until we know whether they are actually needed
    % because there is a complicated unification involving the type.
    % This predicate is exported for use by higher_order.m when it is
    % specializing calls to unify/2.
    %
:- pred add_lazily_generated_unify_pred(type_ctor::in, pred_id::out,
    module_info::in, module_info::out) is det.

    % add_lazily_generated_compare_pred_decl(TypeCtor, ComparePredId_for_Type,
    %   !ModuleInfo):
    %
    % Add declarations, but not clauses, for a compare or index predicate.
    %
:- pred add_lazily_generated_compare_pred_decl(type_ctor::in, pred_id::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.post_typecheck.
:- import_module check_hlds.unify_proc.
:- import_module hlds.add_pred.
:- import_module hlds.hlds_clauses.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.pred_name.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

add_special_pred_decl_defns_for_type_maybe_lazily(TypeCtor, TypeDefn,
        !ModuleInfo) :-
    get_type_defn_body(TypeDefn, TypeBody),
    get_type_defn_status(TypeDefn, TypeStatus),
    ( if
        special_pred_is_generated_lazily(!.ModuleInfo, TypeCtor, TypeBody,
            TypeStatus)
    then
        true
    else
        get_type_defn_tvarset(TypeDefn, TVarSet),
        get_type_defn_kind_map(TypeDefn, KindMap),
        get_type_defn_tparams(TypeDefn, TypeParams),
        prog_type.var_list_to_type_list(KindMap, TypeParams, ArgTypes),
        construct_type(TypeCtor, ArgTypes, Type),
        get_type_defn_context(TypeDefn, Context),
        add_special_pred_decl_defns_for_type_eagerly(TVarSet, Type, TypeCtor,
            TypeBody, TypeStatus, Context, !ModuleInfo)
    ).

add_special_pred_decl_defns_for_type_eagerly(TVarSet, Type, TypeCtor, TypeBody,
        TypeStatus, Context, !ModuleInfo) :-
    % When we see an abstract type declaration, we do not declare an index
    % predicate for that type, since the actual type definition may later
    % turn out not to require one. If the type does turn out to need
    % an index predicate, its declaration will be generated together with
    % its implementation.
    ( if
        can_generate_special_pred_clauses_for_type(!.ModuleInfo, TypeCtor,
            TypeBody)
    then
        add_special_pred_decl_defn(spec_pred_unify, TVarSet, Type, TypeCtor,
            TypeBody, TypeStatus, Context, !ModuleInfo),
        ThisModule = type_status_defined_in_this_module(TypeStatus),
        (
            ThisModule = yes,
            add_special_pred_decl_defn(spec_pred_compare, TVarSet, Type,
                TypeCtor, TypeBody, TypeStatus, Context, !ModuleInfo)
        ;
            ThisModule = no,
            % Never add clauses for comparison predicates
            % for imported types -- they will never be used.
            module_info_get_special_pred_maps(!.ModuleInfo, SpecialPredMaps),
            ( if
                search_special_pred_maps(SpecialPredMaps, spec_pred_compare,
                    TypeCtor, _)
            then
                true
            else
                add_special_pred_decl(spec_pred_compare, TVarSet, Type,
                    TypeCtor, TypeStatus, Context, !ModuleInfo)
            )
        )
    else
        add_special_pred_decl(spec_pred_unify, TVarSet, Type,
            TypeCtor, TypeStatus, Context, !ModuleInfo),
        add_special_pred_decl(spec_pred_compare, TVarSet, Type,
            TypeCtor, TypeStatus, Context, !ModuleInfo)
    ).

add_special_pred_decl_defn(SpecialPredId, TVarSet, Type0, TypeCtor, TypeBody,
        TypeStatus0, Context, !ModuleInfo) :-
    Type = adjust_types_with_special_preds_in_private_builtin(Type0),
    adjust_special_pred_status(SpecialPredId, TypeStatus0, PredStatus),
    module_info_get_special_pred_maps(!.ModuleInfo, SpecialPredMaps0),
    ( if
        search_special_pred_maps(SpecialPredMaps0, SpecialPredId, TypeCtor, _)
    then
        true
    else
        % XXX STATUS
        PredStatus = pred_status(PredOldStatus),
        TypeStatus = type_status(PredOldStatus),
        add_special_pred_decl(SpecialPredId, TVarSet, Type, TypeCtor,
            TypeStatus, Context, !ModuleInfo)
    ),
    module_info_get_special_pred_maps(!.ModuleInfo, SpecialPredMaps1),
    lookup_special_pred_maps(SpecialPredMaps1, SpecialPredId, TypeCtor,
        PredId),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    % If the type was imported, then the special preds for that type
    % should be imported too.
    % XXX There are several different shades of "imported", and in this case,
    % the above comment *should* go into detail about them.
    % XXX Why isn't the status set correctly by add_special_pred_decl
    % in the first place?
    ( if
        ( PredStatus = pred_status(status_imported(_))
        ; PredStatus = pred_status(status_pseudo_imported)
        )
    then
        pred_info_set_status(PredStatus, PredInfo0, PredInfo1)
    else if
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu ^ du_type_canonical = noncanon(_),
        pred_info_get_status(PredInfo0, OldPredStatus),
        OldPredStatus = pred_status(status_pseudo_imported),
        pred_status_is_imported(PredStatus) = no
    then
        % We can only get here with --no-special-preds if the old status
        % is from an abstract declaration of the type.
        % XXX The --no-special-preds option does not exist anymore.
        % Since the compiler did not then know that the type definition
        % will specify a user-defined equality predicate, it set up
        % the status as pseudo_imported in order to prevent the
        % generation of code for mode 0 of the unify predicate
        % for the type. However, for types with user-defined equality,
        % we *do* want to generate code for mode 0 of unify,
        % so we fix the status.
        pred_info_set_status(PredStatus, PredInfo0, PredInfo1)
    else
        PredInfo1 = PredInfo0
    ),
    SpecDefnInfo = spec_pred_defn_info(SpecialPredId, PredId,
        TVarSet, Type, TypeCtor, TypeBody, TypeStatus0, Context),
    add_clauses_for_special_pred(SpecDefnInfo, PredInfo1, !ModuleInfo).

:- pred add_clauses_for_special_pred(spec_pred_defn_info::in,
    pred_info::in, module_info::in, module_info::out) is det.

add_clauses_for_special_pred(SpecDefnInfo, !.PredInfo, !ModuleInfo) :-
    generate_clauses_for_special_pred(SpecDefnInfo, ClausesInfo, !ModuleInfo),
    pred_info_set_clauses_info(ClausesInfo, !PredInfo),
    pred_info_get_markers(!.PredInfo, Markers0),
    add_marker(marker_calls_are_fully_qualified, Markers0, Markers),
    pred_info_set_markers(Markers, !PredInfo),
    PredId = SpecDefnInfo ^ spdi_pred_id,
    module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo).

    % These types need to have the builtin qualifier removed
    % so that their special predicates type check.
    %
    % XXX TYPE_REPN Check that this operation is idempotent. Otherwise,
    % storing only Type and not Type0 in spec_pred_defn_info won't work.
:- func adjust_types_with_special_preds_in_private_builtin(mer_type)
    = mer_type.

adjust_types_with_special_preds_in_private_builtin(Type) = NormalizedType :-
    ( if
        type_to_ctor_and_args(Type, TypeCtor, []),
        is_builtin_type_special_preds_defined_in_mercury(TypeCtor, Name)
    then
        construct_type(type_ctor(unqualified(Name), 0), [], NormalizedType)
    else
        NormalizedType = Type
    ).

%---------------------------------------------------------------------------%

add_special_pred_decl(SpecialPredId, TVarSet, Type, TypeCtor, TypeStatus,
        Context, !ModuleInfo) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    special_pred_interface(SpecialPredId, Type, ArgTypes, ArgModes, Det),
    PredName = uci_pred_name(SpecialPredId, TypeCtor),
    PredArity = get_special_pred_id_arity(SpecialPredId),
    PredFormArity = pred_form_arity(PredArity),
    % All current special_preds are predicates.
    clauses_info_init(pf_predicate, PredFormArity,
        init_clause_item_numbers_comp_gen, ClausesInfo0),
    Origin = origin_special_pred(SpecialPredId, TypeCtor),
    adjust_special_pred_status(SpecialPredId, TypeStatus, PredStatus),
    MaybeCurUserDecl = maybe.no,
    GoalType = goal_not_for_promise(np_goal_type_none),
    map.init(Proofs),
    map.init(ConstraintMap),
    init_markers(Markers),
    % XXX If/when we have "comparable" or "unifiable" typeclasses,
    % this context might not be empty.
    ClassContext = constraints([], []),
    ExistQVars = [],
    map.init(VarNameRemap),
    % XXX Why are we passing the name of the *current* module here,
    % when it could be different from the module that defines TypeCtor?
    pred_info_init(pf_predicate, ModuleName, PredName, PredFormArity, Context,
        Origin, PredStatus, MaybeCurUserDecl, GoalType, Markers, ArgTypes,
        TVarSet, ExistQVars, ClassContext, Proofs, ConstraintMap,
        ClausesInfo0, VarNameRemap, PredInfo0),
    SeqNum = item_no_seq_num,
    varset.init(InstVarSet),
    ArgLives = no,
    % Should not be any inst vars here so it is ok to use a fresh inst_varset.
    % Before the simplification pass, HasParallelConj is not meaningful.
    HasParallelConj = has_no_parallel_conj,
    add_new_proc(Context, SeqNum, PredArity,
        InstVarSet, ArgModes, yes(ArgModes), ArgLives,
        detism_decl_implicit, yes(Det), address_is_not_taken,
        HasParallelConj, PredInfo0, PredInfo, _ProcId),

    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    predicate_table_insert(PredInfo, PredId, PredicateTable0, PredicateTable),
    module_info_set_predicate_table(PredicateTable, !ModuleInfo),
    module_info_get_special_pred_maps(!.ModuleInfo, SpecialPredMaps0),
    (
        SpecialPredId = spec_pred_unify,
        UnifyMap0 = SpecialPredMaps0 ^ spm_unify_map,
        map.det_insert(TypeCtor, PredId, UnifyMap0, UnifyMap),
        SpecialPredMaps = SpecialPredMaps0 ^ spm_unify_map := UnifyMap
    ;
        SpecialPredId = spec_pred_index,
        IndexMap0 = SpecialPredMaps0 ^ spm_index_map,
        map.det_insert(TypeCtor, PredId, IndexMap0, IndexMap),
        SpecialPredMaps = SpecialPredMaps0 ^ spm_index_map := IndexMap
    ;
        SpecialPredId = spec_pred_compare,
        CompareMap0 = SpecialPredMaps0 ^ spm_compare_map,
        map.det_insert(TypeCtor, PredId, CompareMap0, CompareMap),
        SpecialPredMaps = SpecialPredMaps0 ^ spm_compare_map := CompareMap
    ),
    module_info_set_special_pred_maps(SpecialPredMaps, !ModuleInfo).

:- pred adjust_special_pred_status(special_pred_id::in,
    type_status::in, pred_status::out) is det.

adjust_special_pred_status(SpecialPredId, TypeStatus, !:PredStatus) :-
    ( if
        ( TypeStatus = type_status(status_opt_imported)
        ; TypeStatus = type_status(status_abstract_imported)
        )
    then
        !:PredStatus = pred_status(status_imported(import_locn_interface))
    else if
        TypeStatus = type_status(status_abstract_exported)
    then
        !:PredStatus = pred_status(status_exported)
    else
        TypeStatus = type_status(OldStatus),
        !:PredStatus = pred_status(OldStatus)
    ),

    % Unification predicates are special - they are
    % "pseudo"-imported/exported (only mode 0 is imported/exported).
    ( if SpecialPredId = spec_pred_unify then
        ( if !.PredStatus = pred_status(status_imported(_)) then
            !:PredStatus = pred_status(status_pseudo_imported)
        else if !.PredStatus = pred_status(status_exported) then
            !:PredStatus = pred_status(status_pseudo_exported)
        else
            true
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

add_lazily_generated_unify_pred(TypeCtor, PredId, !ModuleInfo) :-
    ( if type_ctor_is_tuple(TypeCtor) then
        collect_type_defn_for_tuple(TypeCtor, Type, TVarSet, TypeBody,
            Context)
    else
        collect_type_defn(!.ModuleInfo, TypeCtor, Type, TVarSet, TypeBody,
            Context)
    ),
    ( if
        can_generate_special_pred_clauses_for_type(!.ModuleInfo,
            TypeCtor, TypeBody)
    then
        % If the unification predicate had a status other than TypeStatus,
        % it should already have been generated.
        % XXX STATUS this is not an appropriate status for a type.
        TypeStatus = type_status(status_pseudo_imported),
        DeclMaybeDefn = decl_and_clauses
    else
        TypeStatus = type_status(status_imported(import_locn_implementation)),
        DeclMaybeDefn = declaration_only
    ),
    add_lazily_generated_special_pred(spec_pred_unify, DeclMaybeDefn,
        TVarSet, Type, TypeCtor, TypeBody, Context, TypeStatus, PredId,
        !ModuleInfo).

add_lazily_generated_compare_pred_decl(TypeCtor, PredId, !ModuleInfo) :-
    collect_type_defn(!.ModuleInfo, TypeCtor, Type, TVarSet, TypeBody,
        Context),

    % If the comparison predicate had a status other than TypeStatus,
    % it should already have been generated.
    % XXX STATUS This is NOT the same TypeStatus as in the similarly marked
    % piece of code above.
    TypeStatus = type_status(status_imported(import_locn_implementation)),
    add_lazily_generated_special_pred(spec_pred_compare, declaration_only,
        TVarSet, Type, TypeCtor, TypeBody, Context, TypeStatus, PredId,
        !ModuleInfo).

%---------------------------------------------------------------------------%

:- type decl_maybe_defn
    --->    declaration_only
    ;       decl_and_clauses.

:- pred add_lazily_generated_special_pred(special_pred_id::in,
    decl_maybe_defn::in, tvarset::in, mer_type::in, type_ctor::in,
    hlds_type_body::in, prog_context::in, type_status::in, pred_id::out,
    module_info::in, module_info::out) is det.

add_lazily_generated_special_pred(SpecialId, Item, TVarSet, Type, TypeCtor,
        TypeBody, Context, TypeStatus, PredId, !ModuleInfo) :-
    % Add the declaration and maybe clauses.
    (
        Item = decl_and_clauses,
        add_special_pred_decl_defn(SpecialId, TVarSet, Type, TypeCtor,
            TypeBody, TypeStatus, Context, !ModuleInfo)
    ;
        Item = declaration_only,
        add_special_pred_decl(SpecialId, TVarSet, Type, TypeCtor,
            TypeStatus, Context, !ModuleInfo)
    ),

    module_info_get_special_pred_maps(!.ModuleInfo, SpecialPredMaps),
    lookup_special_pred_maps(SpecialPredMaps, SpecialId, TypeCtor, PredId),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),

    % The clauses are generated with all type information computed,
    % so just go on to post_typecheck.
    (
        Item = decl_and_clauses,
        PredInfo1 = PredInfo0
    ;
        Item = declaration_only,
        setup_vartypes_in_clauses_for_imported_pred(PredInfo0, PredInfo1)
    ),
    propagate_checked_types_into_pred_modes(!.ModuleInfo, ErrorProcs,
        _InstForTypeSpecs, PredInfo1, PredInfo),
    expect(unify(ErrorProcs, []), $pred, "ErrorProcs != []"),

    % Call polymorphism to introduce type_info arguments for polymorphic types.
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo),

    % Note that this will not work if the generated clauses call a polymorphic
    % predicate which requires type_infos to be added. Such calls can be
    % generated by generate_clause_info, but unification predicates which
    % contain such calls are never generated lazily.
    polymorphism_process_generated_pred(PredId, !ModuleInfo).

%---------------------------------------------------------------------------%

:- pred collect_type_defn(module_info::in, type_ctor::in, mer_type::out,
    tvarset::out, hlds_type_body::out, prog_context::out) is det.

collect_type_defn(ModuleInfo, TypeCtor, Type, TVarSet, TypeBody, Context) :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_tvarset(TypeDefn, TVarSet),
    hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
    hlds_data.get_type_defn_kind_map(TypeDefn, KindMap),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    hlds_data.get_type_defn_context(TypeDefn, Context),

    expect(
        special_pred_is_generated_lazily(ModuleInfo, TypeCtor, TypeBody,
            TypeStatus),
        $pred, "not generated lazily"),
    prog_type.var_list_to_type_list(KindMap, TypeParams, TypeArgs),
    construct_type(TypeCtor, TypeArgs, Type).

:- pred collect_type_defn_for_tuple(type_ctor::in, mer_type::out,
    tvarset::out, hlds_type_body::out, prog_context::out) is det.

collect_type_defn_for_tuple(TypeCtor, Type, TVarSet, TypeBody, Context) :-
    TypeCtor = type_ctor(_, TupleArity),

    % Build a hlds_type_body for the tuple constructor, which will
    % be used by generate_clause_info.
    varset.init(TVarSet0),
    varset.new_vars(TupleArity, TupleArgTVars, TVarSet0, TVarSet),
    prog_type.var_list_to_type_list(map.init, TupleArgTVars,
        TupleArgTypes),

    % Tuple constructors can't be existentially quantified.
    MaybeExistConstraints = no_exist_constraints,

    make_tuple_args_and_repns(Context, TupleArgTypes, CtorArgs, CtorArgRepns),

    Ordinal = 0u32,
    CtorSymName = unqualified("{}"),
    Ctor = ctor(Ordinal, MaybeExistConstraints, CtorSymName,
        CtorArgs, TupleArity, Context),
    CtorRepn = ctor_repn(Ordinal, MaybeExistConstraints, CtorSymName,
        remote_args_tag(remote_args_only_functor), CtorArgRepns,
        TupleArity, Context),

    map.from_assoc_list(["{}" - one_or_more(CtorRepn, [])], ConsCtorMap),
    DirectArgCtors = no,
    Repn = du_type_repn([CtorRepn], ConsCtorMap, no_cheaper_tag_test,
        du_type_kind_general, DirectArgCtors),
    MaybeSuperType = not_a_subtype,
    MaybeCanonical = canon,
    IsForeign = no,
    TypeBodyDu = type_body_du(one_or_more(Ctor, []), MaybeSuperType,
        MaybeCanonical, yes(Repn), IsForeign),
    TypeBody = hlds_du_type(TypeBodyDu),
    construct_type(TypeCtor, TupleArgTypes, Type),

    term.context_init(Context).

:- pred make_tuple_args_and_repns(prog_context::in, list(mer_type)::in,
    list(constructor_arg)::out, list(constructor_arg_repn)::out) is det.

make_tuple_args_and_repns(Context, ArgTypes, CtorArgs, CtorArgRepns) :-
    make_tuple_args_and_repns_loop(Context, ArgTypes, 0,
        CtorArgs, CtorArgRepns).

:- pred make_tuple_args_and_repns_loop(prog_context::in, list(mer_type)::in,
    int::in, list(constructor_arg)::out, list(constructor_arg_repn)::out)
    is det.

make_tuple_args_and_repns_loop(_Context, [], _ArgNum, [], []).
make_tuple_args_and_repns_loop(Context, [ArgType | ArgTypes], ArgNum,
        [CtorArg | CtorArgs], [CtorArgRepn | CtorArgRepns]) :-
    CtorArg = ctor_arg(no, ArgType, Context),
    ArgPosWidth = apw_full(arg_only_offset(ArgNum), cell_offset(ArgNum)),
    CtorArgRepn = ctor_arg_repn(no, ArgType, ArgPosWidth, Context),
    make_tuple_args_and_repns_loop(Context, ArgTypes, ArgNum + 1,
        CtorArgs, CtorArgRepns).

%---------------------------------------------------------------------------%
:- end_module hlds.add_special_pred.
%---------------------------------------------------------------------------%
