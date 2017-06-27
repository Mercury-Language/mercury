%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: add_special_pred.m.
%
% This submodule of make_hlds handles the creation of unify, compare and
% (if needed) index and init predicates for the types defined or imported
% by the module being compiled.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_special_pred.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

%-----------------------------------------------------------------------------%

    % do_add_special_pred_for_real(SpecialPredId, TVarSet, Type, TypeCtor,
    %   TypeBody, TypeContext, TypeStatus, !ModuleInfo).
    %
    % Add declarations and clauses for a special predicate.
    % This is used by unify_proc.m to add a unification predicate
    % for an imported type for which special predicates are being
    % generated only when a unification procedure is requested
    % during mode analysis.
    %
:- pred do_add_special_pred_for_real(special_pred_id::in, tvarset::in,
    mer_type::in, type_ctor::in, hlds_type_body::in, prog_context::in,
    type_status::in, module_info::in, module_info::out) is det.

    % do_add_special_pred_decl_for_real(SpecialPredId, TVarSet,
    %   Type, TypeCtor, TypeContext, TypeStatus, !ModuleInfo).
    %
    % Add declarations for a special predicate.
    % This is used by higher_order.m when specializing an in-in
    % unification for an imported type for which unification procedures
    % are generated lazily.
    %
:- pred do_add_special_pred_decl_for_real(special_pred_id::in,
    tvarset::in, mer_type::in, type_ctor::in, prog_context::in,
    type_status::in, module_info::in, module_info::out) is det.

:- pred add_special_preds(tvarset::in, mer_type::in, type_ctor::in,
    hlds_type_body::in, prog_context::in, type_status::in,
    module_info::in, module_info::out) is det.

    % XXX should be used only for a temporary workaround in make_hlds_passes.m
    %
:- pred eagerly_add_special_preds(tvarset::in, mer_type::in, type_ctor::in,
    hlds_type_body::in, prog_context::in, type_status::in,
    module_info::in, module_info::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.unify_proc.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module varset.

%----------------------------------------------------------------------------%

    % The only place that the index predicate for a type can ever
    % be called from is the compare predicate for that type.
    % The only types whose compare predicates call the type's index
    % predicate are discriminated union types which
    %
    % - do not have user-defined equality (any compiler-generated compare
    %   predicates for types with user-defined equality generate a runtime
    %   abort),
    %
    % - are not enums (comparison predicates for enums just do an integer
    %   comparison), and
    %
    % - have more than one constructor (for types with only one
    %   constructor, the comparison predicate just deconstructs the
    %   arguments and compares them).
    %
    % The compare predicate for an equivalence type never calls the index
    % predicate for that type; it calls the compare predicate of the
    % expanded type instead.
    %
    % When we see an abstract type declaration, we do not declare an index
    % predicate for that type, since the actual type definition may later
    % turn out not to require one. If the type does turn out to need
    % an index predicate, its declaration will be generated together with
    % its implementation.
    %
    % We also do not declare index predicates for types with hand defined
    % RTTI, since such types do not have index predicates.
    %
    % Note: this predicate should include index in the list of special
    % predicates to be defined only for the kinds of types which do not
    % lead unify_proc.generate_index_clauses to abort.
    %
add_special_preds(TVarSet, Type, TypeCtor, TypeBody, Context, TypeStatus,
        !ModuleInfo) :-
    ( if
        special_pred_is_generated_lazily(!.ModuleInfo, TypeCtor, TypeBody,
            TypeStatus)
    then
        true
    else
        eagerly_add_special_preds(TVarSet, Type, TypeCtor, TypeBody, Context,
            TypeStatus, !ModuleInfo)
    ).

eagerly_add_special_preds(TVarSet, Type, TypeCtor, TypeBody, Context,
        TypeStatus, !ModuleInfo) :-
    ( if
        can_generate_special_pred_clauses_for_type(!.ModuleInfo, TypeCtor,
            TypeBody)
    then
        add_special_pred(spec_pred_unify, TVarSet, Type, TypeCtor, TypeBody,
            Context, TypeStatus, !ModuleInfo),
        ThisModule = type_status_defined_in_this_module(TypeStatus),
        (
            ThisModule = yes,
            ( if
                Ctors = TypeBody ^ du_type_ctors,
                TypeBody ^ du_type_kind = du_type_kind_general,
                TypeBody ^ du_type_usereq = no,
                module_info_get_globals(!.ModuleInfo, Globals),
                globals.lookup_int_option(Globals, compare_specialization,
                    CompareSpec),
                list.length(Ctors, CtorCount),
                CtorCount > CompareSpec
            then
                SpecialPredIds = [spec_pred_index, spec_pred_compare]
            else
                SpecialPredIds = [spec_pred_compare]
            ),
            add_special_pred_list(SpecialPredIds, TVarSet, Type, TypeCtor,
                TypeBody, Context, TypeStatus, !ModuleInfo)
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
                    TypeCtor, Context, TypeStatus, !ModuleInfo)
            )
        )
    else
        SpecialPredIds = [spec_pred_unify, spec_pred_compare],
        add_special_pred_decl_list(SpecialPredIds, TVarSet, Type,
            TypeCtor, Context, TypeStatus, !ModuleInfo)
    ).

:- pred add_special_pred_list(list(special_pred_id)::in, tvarset::in,
    mer_type::in, type_ctor::in, hlds_type_body::in, prog_context::in,
    type_status::in, module_info::in, module_info::out) is det.

add_special_pred_list([], _, _, _, _, _, _, !ModuleInfo).
add_special_pred_list([SpecialPredId | SpecialPredIds], TVarSet, Type,
        TypeCtor, TypeBody, Context, TypeStatus, !ModuleInfo) :-
    add_special_pred(SpecialPredId, TVarSet, Type,
        TypeCtor, TypeBody, Context, TypeStatus, !ModuleInfo),
    add_special_pred_list(SpecialPredIds, TVarSet, Type,
        TypeCtor, TypeBody, Context, TypeStatus, !ModuleInfo).

:- pred add_special_pred(special_pred_id::in, tvarset::in, mer_type::in,
    type_ctor::in, hlds_type_body::in, prog_context::in, type_status::in,
    module_info::in, module_info::out) is det.

add_special_pred(SpecialPredId, TVarSet, Type, TypeCtor, TypeBody, Context,
        TypeStatus0, !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, special_preds, GenSpecialPreds),
    (
        GenSpecialPreds = yes,
        do_add_special_pred_for_real(SpecialPredId, TVarSet,
            Type, TypeCtor, TypeBody, Context, TypeStatus0, !ModuleInfo)
    ;
        GenSpecialPreds = no,
        (
            SpecialPredId = spec_pred_unify,
            % XXX STATUS
            add_special_pred_unify_status(TypeBody, TypeStatus0, TypeStatus),
            do_add_special_pred_for_real(SpecialPredId, TVarSet,
                Type, TypeCtor, TypeBody, Context, TypeStatus, !ModuleInfo)
        ;
            SpecialPredId = spec_pred_index
        ;
            SpecialPredId = spec_pred_compare,
            (
                TypeBody = hlds_du_type(_, _, _, _, MaybeUserEq, _, _, _, _),
                (
                    MaybeUserEq = yes(_),
                    % The compiler generated comparison procedure prints
                    % an error message, since comparisons of types with
                    % user-defined equality are not allowed.
                    % We get the runtime system to invoke this procedure
                    % instead of printing the error message itself, because
                    % it is easier to generate a good error message in Mercury
                    % code than in C code.
                    do_add_special_pred_for_real(SpecialPredId, TVarSet, Type,
                        TypeCtor, TypeBody, Context, TypeStatus0, !ModuleInfo)
                ;
                    MaybeUserEq = no
                )
            ;
                ( TypeBody = hlds_eqv_type(_)
                ; TypeBody = hlds_foreign_type(_)
                ; TypeBody = hlds_solver_type(_)
                ; TypeBody = hlds_abstract_type(_)
                )
            )
        )
    ).

do_add_special_pred_for_real(SpecialPredId, TVarSet, Type0, TypeCtor,
        TypeBody, Context, TypeStatus0, !ModuleInfo) :-
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
        do_add_special_pred_decl_for_real(SpecialPredId, TVarSet,
            Type, TypeCtor, Context, TypeStatus, !ModuleInfo)
    ),
    module_info_get_special_pred_maps(!.ModuleInfo, SpecialPredMaps1),
    lookup_special_pred_maps(SpecialPredMaps1, SpecialPredId, TypeCtor,
        PredId),
    module_info_get_preds(!.ModuleInfo, Preds0),
    map.lookup(Preds0, PredId, PredInfo0),
    % If the type was imported, then the special preds for that
    % type should be imported too.
    ( if
        ( PredStatus = pred_status(status_imported(_))
        ; PredStatus = pred_status(status_pseudo_imported)
        )
    then
        pred_info_set_status(PredStatus, PredInfo0, PredInfo1)
    else if
        TypeBody ^ du_type_usereq = yes(_),
        pred_info_get_status(PredInfo0, OldPredStatus),
        OldPredStatus = pred_status(status_pseudo_imported),
        pred_status_is_imported(PredStatus) = no
    then
        % We can only get here with --no-special-preds if the old
        % status is from an abstract declaration of the type.
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
    generate_clause_info(SpecialPredId, Type, TypeBody,
        Context, !.ModuleInfo, ClausesInfo),
    pred_info_set_clauses_info(ClausesInfo, PredInfo1, PredInfo2),
    pred_info_get_markers(PredInfo2, Markers2),
    add_marker(marker_calls_are_fully_qualified, Markers2, Markers),
    pred_info_set_markers(Markers, PredInfo2, PredInfo3),
    pred_info_set_origin(origin_special_pred(SpecialPredId, TypeCtor),
        PredInfo3, PredInfo),
    map.det_update(PredId, PredInfo, Preds0, Preds),
    module_info_set_preds(Preds, !ModuleInfo).

    % These types need to have the builtin qualifier removed
    % so that their special predicates type check.
    %
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

:- pred add_special_pred_decl_list(list(special_pred_id)::in, tvarset::in,
    mer_type::in, type_ctor::in, prog_context::in,
    type_status::in, module_info::in, module_info::out) is det.

add_special_pred_decl_list([], _, _, _, _, _, !ModuleInfo).
add_special_pred_decl_list([SpecialPredId | SpecialPredIds], TVarSet, Type,
        TypeCtor, Context, TypeStatus, !ModuleInfo) :-
    add_special_pred_decl(SpecialPredId, TVarSet, Type,
        TypeCtor, Context, TypeStatus, !ModuleInfo),
    add_special_pred_decl_list(SpecialPredIds, TVarSet, Type,
        TypeCtor, Context, TypeStatus, !ModuleInfo).

:- pred add_special_pred_decl(special_pred_id::in, tvarset::in, mer_type::in,
    type_ctor::in, prog_context::in, type_status::in,
    module_info::in, module_info::out) is det.

add_special_pred_decl(SpecialPredId, TVarSet, Type, TypeCtor,
        Context, TypeStatus, !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, special_preds, GenSpecialPreds),
    (
        GenSpecialPreds = yes,
        do_add_special_pred_decl_for_real(SpecialPredId,
            TVarSet, Type, TypeCtor, Context, TypeStatus, !ModuleInfo)
    ;
        GenSpecialPreds = no,
        (
            SpecialPredId = spec_pred_unify,
            do_add_special_pred_decl_for_real(SpecialPredId, TVarSet,
                Type, TypeCtor, Context, TypeStatus, !ModuleInfo)
        ;
            SpecialPredId = spec_pred_compare
        ;
            SpecialPredId = spec_pred_index
        )
    ).

do_add_special_pred_decl_for_real(SpecialPredId, TVarSet, Type, TypeCtor,
        Context, TypeStatus, !ModuleInfo) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    special_pred_interface(SpecialPredId, Type, ArgTypes, ArgModes, Det),
    PredBaseName = special_pred_name(SpecialPredId, TypeCtor),
    PredName = unqualified(PredBaseName),
    Arity = get_special_pred_id_arity(SpecialPredId),
    % XXX we probably shouldn't hardcode this as predicate but since
    % all current special_preds are predicates at the moment it doesn't
    % matter.
    clauses_info_init(pf_predicate, Arity, init_clause_item_numbers_comp_gen,
        ClausesInfo0),
    Origin = origin_special_pred(SpecialPredId, TypeCtor),
    adjust_special_pred_status(SpecialPredId, TypeStatus, PredStatus),
    CurUserDecl = maybe.no,
    map.init(Proofs),
    map.init(ConstraintMap),
    init_markers(Markers),
    % XXX If/when we have "comparable" or "unifiable" typeclasses,
    % this context might not be empty.
    ClassContext = constraints([], []),
    ExistQVars = [],
    map.init(VarNameRemap),
    pred_info_init(ModuleName, PredName, Arity, pf_predicate, Context,
        Origin, PredStatus, CurUserDecl, goal_type_none, Markers, ArgTypes,
        TVarSet, ExistQVars, ClassContext, Proofs, ConstraintMap,
        ClausesInfo0, VarNameRemap, PredInfo0),
    ItemNumber = -1,
    varset.init(InstVarSet),
    ArgLives = no,
    % Should not be any inst vars here so it is ok to use a fresh inst_varset.
    % Before the simplification pass, HasParallelConj is not meaningful.
    HasParallelConj = has_no_parallel_conj,
    do_add_new_proc(Context, ItemNumber, Arity,
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

:- pred add_special_pred_unify_status(hlds_type_body::in,
    type_status::in, type_status::out) is det.

add_special_pred_unify_status(TypeBody, TypeStatus0, TypeStatus) :-
    % XXX STATUS should return pred_status, not type_status
    (
        TypeBody = hlds_du_type(_, _, _, _, MaybeUserEq, _, _, _, _),
        (
            MaybeUserEq = yes(_),
            % If the type has user-defined equality, then we create a real
            % unify predicate for it, whose body calls the user-specified
            % predicate. The compiler's usual type checking algorithm
            % will handle any necessary disambiguation from predicates
            % with the same name but different argument types, and the
            % usual mode checking algorithm will select the right mode
            % of the chosen predicate.
            TypeStatus = TypeStatus0
        ;
            MaybeUserEq = no,
            TypeStatus = type_status(status_pseudo_imported)
        )
    ;
        ( TypeBody = hlds_eqv_type(_)
        ; TypeBody = hlds_foreign_type(_)
        ; TypeBody = hlds_solver_type(_)
        ; TypeBody = hlds_abstract_type(_)
        ),
        TypeStatus = type_status(status_pseudo_imported)
    ).

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

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_special_pred.
%----------------------------------------------------------------------------%
