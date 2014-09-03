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
:- import_module hlds.hlds_pred.
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
    import_status::in, module_info::in, module_info::out) is det.

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
    import_status::in, module_info::in, module_info::out) is det.

:- pred add_special_preds(tvarset::in, mer_type::in, type_ctor::in,
    hlds_type_body::in, prog_context::in, import_status::in,
    module_info::in, module_info::out) is det.

    % XXX should be used only for a temporary workaround in make_hlds_passes.m
    %
:- pred eagerly_add_special_preds(tvarset::in, mer_type::in, type_ctor::in,
    hlds_type_body::in, prog_context::in, import_status::in,
    module_info::in, module_info::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module check_hlds.unify_proc.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.
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
    % What we do here for uu types does not matter much, since such types
    % are not yet supported.
    %
    % Note: this predicate should include index in the list of special
    % predicates to be defined only for the kinds of types which do not
    % lead unify_proc.generate_index_clauses to abort.
    %
add_special_preds(TVarSet, Type, TypeCtor, Body, Context, Status,
        !ModuleInfo) :-
    (
        special_pred_is_generated_lazily(!.ModuleInfo, TypeCtor, Body, Status)
    ->
        true
    ;
        eagerly_add_special_preds(TVarSet, Type, TypeCtor, Body, Context,
            Status, !ModuleInfo)
    ).

eagerly_add_special_preds(TVarSet, Type, TypeCtor, Body, Context, Status,
        !ModuleInfo) :-
    (
        can_generate_special_pred_clauses_for_type(!.ModuleInfo, TypeCtor,
            Body)
    ->
        add_special_pred(spec_pred_unify, TVarSet, Type, TypeCtor, Body,
            Context, Status, !ModuleInfo),
        ThisModule = status_defined_in_this_module(Status),
        (
            ThisModule = yes,
            (
                Ctors = Body ^ du_type_ctors,
                Body ^ du_type_kind = du_type_kind_general,
                Body ^ du_type_usereq = no,
                module_info_get_globals(!.ModuleInfo, Globals),
                globals.lookup_int_option(Globals, compare_specialization,
                    CompareSpec),
                list.length(Ctors, CtorCount),
                CtorCount > CompareSpec
            ->
                SpecialPredIds = [spec_pred_index, spec_pred_compare]
            ;
                SpecialPredIds = [spec_pred_compare]
            ),
            add_special_pred_list(SpecialPredIds, TVarSet, Type, TypeCtor,
                Body, Context, Status, !ModuleInfo)
        ;
            ThisModule = no,
            % Never add clauses for comparison predicates
            % for imported types -- they will never be used.
            module_info_get_special_pred_map(!.ModuleInfo, SpecialPreds),
            ( map.contains(SpecialPreds, spec_pred_compare - TypeCtor) ->
                true
            ;
                add_special_pred_decl(spec_pred_compare, TVarSet, Type,
                    TypeCtor, Body, Context, Status, !ModuleInfo)
            )
        ),
        ( type_is_solver_type_with_auto_init(!.ModuleInfo, Type) ->
            add_special_pred(spec_pred_init, TVarSet, Type, TypeCtor, Body,
                Context, Status, !ModuleInfo)
        ;
            true
        )
    ;
        ( type_is_solver_type_with_auto_init(!.ModuleInfo, Type) ->
            SpecialPredIds = [spec_pred_unify, spec_pred_compare,
                spec_pred_init]
        ;
            SpecialPredIds = [spec_pred_unify, spec_pred_compare]
        ),
        add_special_pred_decl_list(SpecialPredIds, TVarSet, Type,
            TypeCtor, Body, Context, Status, !ModuleInfo)
    ).

:- pred add_special_pred_list(list(special_pred_id)::in, tvarset::in,
    mer_type::in, type_ctor::in, hlds_type_body::in, prog_context::in,
    import_status::in, module_info::in, module_info::out) is det.

add_special_pred_list([], _, _, _, _, _, _, !ModuleInfo).
add_special_pred_list([SpecialPredId | SpecialPredIds], TVarSet, Type,
        TypeCtor, Body, Context, Status, !ModuleInfo) :-
    add_special_pred(SpecialPredId, TVarSet, Type,
        TypeCtor, Body, Context, Status, !ModuleInfo),
    add_special_pred_list(SpecialPredIds, TVarSet, Type,
        TypeCtor, Body, Context, Status, !ModuleInfo).

:- pred add_special_pred(special_pred_id::in, tvarset::in, mer_type::in,
    type_ctor::in, hlds_type_body::in, prog_context::in, import_status::in,
    module_info::in, module_info::out) is det.

add_special_pred(SpecialPredId, TVarSet, Type, TypeCtor, TypeBody, Context,
        Status0, !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, special_preds, GenSpecialPreds),
    (
        GenSpecialPreds = yes,
        do_add_special_pred_for_real(SpecialPredId, TVarSet,
            Type, TypeCtor, TypeBody, Context, Status0, !ModuleInfo)
    ;
        GenSpecialPreds = no,
        (
            SpecialPredId = spec_pred_unify,
            add_special_pred_unify_status(TypeBody, Status0, Status),
            do_add_special_pred_for_real(SpecialPredId, TVarSet,
                Type, TypeCtor, TypeBody, Context, Status, !ModuleInfo)
        ;
            SpecialPredId = spec_pred_index
        ;
            SpecialPredId = spec_pred_compare,
            (
                % The compiler generated comparison procedure prints an error
                % message, since comparisons of types with user-defined
                % equality are not allowed.
                % We get the runtime system to invoke this procedure instead
                % of printing the error message itself, because it is easier
                % to generate a good error message in Mercury code than in
                % C code.
                TypeBody ^ du_type_usereq = yes(_)
            ->
                do_add_special_pred_for_real(SpecialPredId, TVarSet, Type,
                    TypeCtor, TypeBody, Context, Status0, !ModuleInfo)
            ;
                true
            )
        ;
            SpecialPredId = spec_pred_init,
            ( type_is_solver_type_with_auto_init(!.ModuleInfo, Type) ->
                do_add_special_pred_for_real(SpecialPredId, TVarSet, Type,
                    TypeCtor, TypeBody, Context, Status0, !ModuleInfo)
            ;
                unexpected($module, $pred,
                    "attempt to add initialise pred for non-solver type " ++
                    "or solver type without automatic initialisation.")
            )
        )
    ).

do_add_special_pred_for_real(SpecialPredId, TVarSet, Type0, TypeCtor,
        TypeBody, Context, Status0, !ModuleInfo) :-
    Type = adjust_types_with_special_preds_in_private_builtin(Type0),
    adjust_special_pred_status(SpecialPredId, Status0, Status),
    module_info_get_special_pred_map(!.ModuleInfo, SpecialPredMap0),
    ( map.contains(SpecialPredMap0, SpecialPredId - TypeCtor) ->
        true
    ;
        do_add_special_pred_decl_for_real(SpecialPredId, TVarSet,
            Type, TypeCtor, Context, Status, !ModuleInfo)
    ),
    module_info_get_special_pred_map(!.ModuleInfo, SpecialPredMap1),
    map.lookup(SpecialPredMap1, SpecialPredId - TypeCtor, PredId),
    module_info_get_preds(!.ModuleInfo, Preds0),
    map.lookup(Preds0, PredId, PredInfo0),
    % If the type was imported, then the special preds for that
    % type should be imported too.
    (
        ( Status = status_imported(_)
        ; Status = status_pseudo_imported
        )
    ->
        pred_info_set_import_status(Status, PredInfo0, PredInfo1)
    ;
        TypeBody ^ du_type_usereq = yes(_),
        pred_info_get_import_status(PredInfo0, OldStatus),
        OldStatus = status_pseudo_imported,
        status_is_imported(Status) = no
    ->
        % We can only get here with --no-special-preds if the old
        % status is from an abstract declaration of the type.
        % Since the compiler did not then know that the type definition
        % will specify a user-defined equality predicate, it set up
        % the status as pseudo_imported in order to prevent the
        % generation of code for mode 0 of the unify predicate
        % for the type. However, for types with user-defined equality,
        % we *do* want to generate code for mode 0 of unify,
        % so we fix the status.
        pred_info_set_import_status(Status, PredInfo0, PredInfo1)
    ;
        PredInfo1 = PredInfo0
    ),
    generate_clause_info(SpecialPredId, Type, TypeBody,
        Context, !.ModuleInfo, ClausesInfo),
    pred_info_set_clauses_info(ClausesInfo, PredInfo1, PredInfo2),
    pred_info_get_markers(PredInfo2, Markers2),
    add_marker(marker_calls_are_fully_qualified, Markers2, Markers),
    pred_info_set_markers(Markers, PredInfo2, PredInfo3),
    pred_info_set_origin(origin_special_pred(SpecialPredId - TypeCtor),
        PredInfo3, PredInfo),
    map.det_update(PredId, PredInfo, Preds0, Preds),
    module_info_set_preds(Preds, !ModuleInfo).

    % These types need to have the builtin qualifier removed
    % so that their special predicates type check.
    %
:- func adjust_types_with_special_preds_in_private_builtin(mer_type)
    = mer_type.

adjust_types_with_special_preds_in_private_builtin(Type) = NormalizedType :-
    ( type_to_ctor_and_args(Type, TypeCtor, []) ->
        ( is_builtin_type_special_preds_defined_in_mercury(TypeCtor, Name) ->
            construct_type(type_ctor(unqualified(Name), 0), [], NormalizedType)
        ;
            NormalizedType = Type
        )
    ;
        NormalizedType = Type
    ).

:- pred add_special_pred_decl_list(list(special_pred_id)::in, tvarset::in,
    mer_type::in, type_ctor::in, hlds_type_body::in, prog_context::in,
    import_status::in, module_info::in, module_info::out) is det.

add_special_pred_decl_list([], _, _, _, _, _, _, !ModuleInfo).
add_special_pred_decl_list([SpecialPredId | SpecialPredIds], TVarSet, Type,
        TypeCtor, TypeBody, Context, Status, !ModuleInfo) :-
    add_special_pred_decl(SpecialPredId, TVarSet, Type,
        TypeCtor, TypeBody, Context, Status, !ModuleInfo),
    add_special_pred_decl_list(SpecialPredIds, TVarSet, Type,
        TypeCtor, TypeBody, Context, Status, !ModuleInfo).

:- pred add_special_pred_decl(special_pred_id::in, tvarset::in, mer_type::in,
    type_ctor::in, hlds_type_body::in, prog_context::in, import_status::in,
    module_info::in, module_info::out) is det.

add_special_pred_decl(SpecialPredId, TVarSet, Type, TypeCtor, TypeBody,
        Context, Status0, !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, special_preds, GenSpecialPreds),
    (
        GenSpecialPreds = yes,
        do_add_special_pred_decl_for_real(SpecialPredId,
            TVarSet, Type, TypeCtor, Context, Status0, !ModuleInfo)
    ;
        GenSpecialPreds = no,
        (
            SpecialPredId = spec_pred_unify,
            add_special_pred_unify_status(TypeBody, Status0, Status),
            do_add_special_pred_decl_for_real(SpecialPredId, TVarSet,
                Type, TypeCtor, Context, Status, !ModuleInfo)
        ;
            SpecialPredId = spec_pred_compare
        ;
            SpecialPredId = spec_pred_index
        ;
            SpecialPredId = spec_pred_init
        )
    ).

do_add_special_pred_decl_for_real(SpecialPredId, TVarSet, Type, TypeCtor,
        Context, Status0, !ModuleInfo) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    special_pred_interface(SpecialPredId, Type, ArgTypes, ArgModes, Det),
    Name = special_pred_name(SpecialPredId, TypeCtor),
    (
        SpecialPredId = spec_pred_init,
        TypeCtor = type_ctor(TypeSymName, _TypeArity),
        sym_name_get_module_name_default(TypeSymName, ModuleName,
            TypeModuleName),
        PredName = qualified(TypeModuleName, Name)
    ;
        ( SpecialPredId = spec_pred_unify
        ; SpecialPredId = spec_pred_index
        ; SpecialPredId = spec_pred_compare
        ),
        PredName = unqualified(Name)
    ),
    Arity = get_special_pred_id_arity(SpecialPredId),
    % XXX we probably shouldn't hardcode this as predicate but since
    % all current special_preds are predicates at the moment it doesn't
    % matter.
    clauses_info_init(pf_predicate, Arity, init_clause_item_numbers_comp_gen,
        ClausesInfo0),
    Origin = origin_special_pred(SpecialPredId - TypeCtor),
    adjust_special_pred_status(SpecialPredId, Status0, Status),
    map.init(Proofs),
    map.init(ConstraintMap),
    init_markers(Markers),
    % XXX If/when we have "comparable" or "unifiable" typeclasses,
    % this context might not be empty.
    ClassContext = constraints([], []),
    ExistQVars = [],
    map.init(VarNameRemap),
    pred_info_init(ModuleName, PredName, Arity, pf_predicate, Context,
        Origin, Status, goal_type_none, Markers, ArgTypes, TVarSet, ExistQVars,
        ClassContext, Proofs, ConstraintMap, ClausesInfo0, VarNameRemap,
        PredInfo0),
    ArgLives = no,
    varset.init(InstVarSet),
    % Should not be any inst vars here so it is ok to use a fresh inst_varset.
    % Before the simplification pass, HasParallelConj is not meaningful.
    HasParallelConj = has_no_parallel_conj,
    do_add_new_proc(InstVarSet, Arity, ArgModes, yes(ArgModes), ArgLives,
        detism_decl_implicit, yes(Det), Context, address_is_not_taken,
        HasParallelConj, PredInfo0, PredInfo, _ProcId),

    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    predicate_table_insert(PredInfo, PredId, PredicateTable0, PredicateTable),
    module_info_set_predicate_table(PredicateTable, !ModuleInfo),
    module_info_get_special_pred_map(!.ModuleInfo, SpecialPredMap0),
    map.set(SpecialPredId - TypeCtor, PredId, SpecialPredMap0, SpecialPredMap),
    module_info_set_special_pred_map(SpecialPredMap, !ModuleInfo).

:- pred add_special_pred_unify_status(hlds_type_body::in, import_status::in,
    import_status::out) is det.

add_special_pred_unify_status(TypeBody, Status0, Status) :-
    ( TypeBody ^ du_type_usereq = yes(_) ->
        % If the type has user-defined equality, then we create a real unify
        % predicate for it, whose body calls the user-specified predicate.
        % The compiler's usual type checking algorithm will handle any
        % necessary disambiguation from predicates with the same name
        % but different argument types, and the usual mode checking algorithm
        % will select the right mode of the chosen predicate.
        Status = Status0
    ;
        Status = status_pseudo_imported
    ).

:- pred adjust_special_pred_status(special_pred_id::in,
    import_status::in, import_status::out) is det.

adjust_special_pred_status(SpecialPredId, !Status) :-
    (
        ( !.Status = status_opt_imported
        ; !.Status = status_abstract_imported
        )
    ->
        !:Status = status_imported(import_locn_interface)
    ;
        !.Status = status_abstract_exported
    ->
        !:Status = status_exported
    ;
        true
    ),

    % Unification predicates are special - they are
    % "pseudo"-imported/exported (only mode 0 is imported/exported).
    ( SpecialPredId = spec_pred_unify ->
        ( !.Status = status_imported(_) ->
            !:Status = status_pseudo_imported
        ; !.Status = status_exported ->
            !:Status = status_pseudo_exported
        ;
            true
        )
    ;
        true
    ).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_special_pred.
%----------------------------------------------------------------------------%
