%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: unused_types.m.
% Main author: zs.
%
% Generate warnings about any types that
%
% - are not used in their defining modules, and
% - are not exported from that module (even to submodules),
%   so they cannot be used in other modules either.
%
% To enable the warnings use `--warn-unused-types'.
%
%---------------------------------------------------------------------------%

:- module check_hlds.unused_types.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred warn_about_unused_types(module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.

%---------------------------------------------------------------------------%

warn_about_unused_types(ModuleInfo, !Specs) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, warn_unused_types, WarnUnusedTypes),
    (
        WarnUnusedTypes = no
    ;
        WarnUnusedTypes = yes,
        module_info_get_type_table(ModuleInfo, TypeTable),
        get_all_type_ctor_defns(TypeTable, TypeCtorsDefns),
        collect_should_be_used_type_ctors(TypeCtorsDefns,
            [], ShouldBeUsedTypeCtors),
        set_tree234.list_to_set(ShouldBeUsedTypeCtors,
            ShouldBeUsedTypeCtorsSet),
        module_info_get_predicate_table(ModuleInfo, PredTable),
        predicate_table_get_pred_id_table(PredTable, PredIdTable),
        map.values(PredIdTable, PredInfos),
        % We process *all* pred_infos, not just the valid pred_infos,
        % because a type_ctor is used even if the only references to it are
        % in the argument list of a predicate that has been marked invalid
        % due to a type error. This happens e.g. in the module
        % tests/invalid/coerce_type_error.m.
        record_type_ctors_used_in_preds(PredInfos,
            ShouldBeUsedTypeCtorsSet, UnusedTypeCtorSet0),
        ( if set_tree234.is_empty(UnusedTypeCtorSet0) then
            UnusedTypeCtorSet1 = UnusedTypeCtorSet0
        else
            module_info_get_type_repn_dec(ModuleInfo, DecisionData),
            FEEInfos = DecisionData ^ trdd_foreign_exports,
            list.foldl(record_type_ctors_used_in_foreign_export_enum_info,
                FEEInfos, UnusedTypeCtorSet0, UnusedTypeCtorSet1)
        ),
        ( if set_tree234.is_empty(UnusedTypeCtorSet1) then
            UnusedTypeCtorSet = UnusedTypeCtorSet1
        else
            % This call must be last predicate we invoke here
            % to mark type_ctors as used. The reason is explained
            % in the comment on made_for_uci predicates
            % in record_type_ctors_used_in_preds below.
            record_type_ctors_used_in_type_defns(UnusedTypeCtorSet1,
                TypeCtorsDefns, UnusedTypeCtorSet1, UnusedTypeCtorSet)
        ),
        set_tree234.foldl(report_unused_type_ctor(TypeTable),
            UnusedTypeCtorSet, !Specs)
    ).

%---------------------------------------------------------------------------%

:- pred collect_should_be_used_type_ctors(
    assoc_list(type_ctor, hlds_type_defn)::in,
    list(type_ctor)::in, list(type_ctor)::out) is det.

collect_should_be_used_type_ctors([], !ShouldBeUsedTypeCtors).
collect_should_be_used_type_ctors([Pair | Pairs], !ShouldBeUsedTypeCtors) :-
    Pair = TypeCtor - TypeDefn,
    get_type_defn_status(TypeDefn, TypeStatus),
    get_type_defn_body(TypeDefn, TypeBody),
    ( if
        % Is the type local to the module? The answer is "yes" only if
        % - the type is defined in the implementation section, AND
        % - the type is not exported even to submodules.
        type_status_defined_in_impl_section(TypeStatus) = yes,
        type_status_is_exported(TypeStatus) = no,
        % XXX We should check whether equivalence types are unused as well,
        % but we can do that only if we keep track of which equivalence types
        % were expanded by equiv_type.m. We do not yet do that.
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu ^ du_type_is_foreign_type = no
    then
        !:ShouldBeUsedTypeCtors = [TypeCtor | !.ShouldBeUsedTypeCtors]
    else
        true
    ),
    collect_should_be_used_type_ctors(Pairs, !ShouldBeUsedTypeCtors).

%---------------------------------------------------------------------------%

:- pred record_type_ctors_used_in_preds(list(pred_info)::in,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out) is det.

record_type_ctors_used_in_preds([], !ShouldBeUsedTypeCtors).
record_type_ctors_used_in_preds([PredInfo | PredInfos],
        !ShouldBeUsedTypeCtors) :-
    record_type_ctors_used_in_pred(PredInfo, !ShouldBeUsedTypeCtors),
    ( if set_tree234.is_empty(!.ShouldBeUsedTypeCtors) then
        % We already know that all local type_ctors are used,
        % so there is no point in continuing to try to find *more*
        % used local type_ctors.
        true
    else
        record_type_ctors_used_in_preds(PredInfos, !ShouldBeUsedTypeCtors)
    ).

:- pred record_type_ctors_used_in_pred(pred_info::in,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out) is det.

record_type_ctors_used_in_pred(PredInfo, !ShouldBeUsedTypeCtors) :-
    pred_info_get_origin(PredInfo, PredOrigin),
    (
        PredOrigin = origin_user(_),
        Process = yes
    ;
        PredOrigin = origin_compiler(CompilerMade),
        (
            CompilerMade = made_for_uci(_, _),
            % We want to ignore unify/compare/index predicates because
            % *every* type is used in its own uci predicate.
            %
            % Of course, a uci predicate can also refer to the type
            % constructors that occur in the data constructors of its
            % definition. There is a dilemma here:
            %
            % - if type constructor TC_A is used, then we want
            %   to count a reference its uci predicates make to another type
            %   constructor, call it TC_B, as marking it used.
            %
            % - if type constructor TC_A is NOT used, then we DO NOT want
            %   to count a reference its uci predicates make to TC_B
            %   as marking it used.
            %
            % We do not yet know which category this uci predicate falls into.
            % However, just before execution gets to the end of the
            % warn_unused_types predicate, we *will* know. And at that time,
            % we process the definitions of just the *used* type_ctors,
            % and the set of type_ctors referred to in their data constructors
            % is (by construction) identical to the set of type_ctors
            % referenced by their uci predicates.
            %
            % Note that this design allows two or more types to marked
            % as unused even if they are mutually recursive, meaning that
            % they are "used" in each other's definitions.
            Process = no
        ;
            CompilerMade = made_for_tabling(_, _),
            % We ignore these predicates because any type constructor they use
            % will also be used by the predicate they help table. Therefore
            % processing them would be a waste of time.
            Process = no
        ;
            ( CompilerMade = made_for_solver_repn(_, _)
            ; CompilerMade = made_for_mutable(_, _, _)
            ; CompilerMade = made_for_initialise(_, _)
            ; CompilerMade = made_for_finalise(_, _)
            ),
            Process = yes
        ;
            ( CompilerMade = made_for_deforestation(_, _)
            ),
            % Such predicates should not exist yet.
            unexpected($pred, "later pass predicate")
        )
    ;
        (
            PredOrigin = origin_pred_transform(PredTransform, _, _),
            (
                PredTransform = pred_transform_pragma_type_spec(_),
                % A type-specialized predicate can show up here,
                % but all the type_ctors it uses must be definition
                % also be used by the original, non-specialized predicate.
                Process = no
            ;
                ( PredTransform = pred_transform_distance_granularity(_)
                ; PredTransform = pred_transform_table_generator
                ; PredTransform = pred_transform_ssdebug(_)
                ; PredTransform = pred_transform_structure_reuse
                ),
                % These transforms are done by later passes.
                unexpected($pred, "later transform")
            )
        ;
            PredOrigin = origin_proc_transform(ProcTransform, _, _, _),
            (
                ProcTransform = proc_transform_user_type_spec(_, _),
                % A type-specialized predicate can show up here,
                % but all the type_ctors it uses must be definition
                % also be used by the original, non-specialized predicate.
                Process = no
            ;
                ( ProcTransform = proc_transform_higher_order_spec(_)
                ; ProcTransform = proc_transform_accumulator(_, _)
                ; ProcTransform = proc_transform_unused_args(_)
                ; ProcTransform = proc_transform_loop_inv(_, _)
                ; ProcTransform = proc_transform_tuple(_, _)
                ; ProcTransform = proc_transform_untuple(_, _)
                ; ProcTransform = proc_transform_dep_par_conj(_)
                ; ProcTransform = proc_transform_par_loop_ctrl
                ; ProcTransform = proc_transform_lcmc(_, _)
                ; ProcTransform = proc_transform_stm_expansion
                ; ProcTransform = proc_transform_io_tabling
                ; ProcTransform = proc_transform_direct_arg_in_out
                ),
                % These transforms are done by later passes.
                unexpected($pred, "later transform")
            )
        ),
        Process = no
    ),
    (
        Process = no
    ;
        Process = yes,
        pred_info_get_clauses_info(PredInfo, ClausesInfo),
        VarTable = ClausesInfo ^ cli_var_table,
        foldl_var_table_values(record_type_ctors_used_in_vte, VarTable,
            !ShouldBeUsedTypeCtors)
    ).

:- pred record_type_ctors_used_in_vte(var_table_entry::in,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out) is det.

record_type_ctors_used_in_vte(VarTableEntry, !ShouldBeUsedTypeCtors) :-
    VarTableEntry = vte(_, Type, _),
    record_type_ctors_used_in_type(Type, !ShouldBeUsedTypeCtors).

%---------------------------------------------------------------------------%

:- pred record_type_ctors_used_in_type_defns(set_tree234(type_ctor)::in,
    assoc_list(type_ctor, hlds_type_defn)::in,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out) is det.

record_type_ctors_used_in_type_defns(_, [], !UnusedTypeCtorSet).
record_type_ctors_used_in_type_defns(PredUnusedTypeCtorSet,
        [TypeCtorDefn | TypeCtorsDefns], !UnusedTypeCtorSet) :-
    TypeCtorDefn = TypeCtor - TypeDefn,
    get_type_defn_status(TypeDefn, TypeStatus),
    ( if
        ( type_status_defined_in_this_module(TypeStatus) = no
        ; set_tree234.contains(PredUnusedTypeCtorSet, TypeCtor)
        )
    then
        true
    else
        get_type_defn_body(TypeDefn, TypeBody),
        (
            TypeBody = hlds_du_type(TypeBodyDu),
            TypeBodyDu = type_body_du(OoMCtors, _SortedOoMCtors,
                MaybeSubType, _MaybeCanon, _Repn, _IsForeign),
            OoMCtors = one_or_more(HeadCtor, TailCtors),
            record_type_ctors_used_in_data_ctor(HeadCtor, !UnusedTypeCtorSet),
            list.foldl(record_type_ctors_used_in_data_ctor, TailCtors,
                !UnusedTypeCtorSet),
            (
                MaybeSubType = not_a_subtype
            ;
                MaybeSubType = subtype_of(SuperType),
                record_type_ctors_used_in_type(SuperType, !UnusedTypeCtorSet)
            )
        ;
            TypeBody = hlds_eqv_type(EqvType),
            record_type_ctors_used_in_type(EqvType, !UnusedTypeCtorSet)
        ;
            TypeBody = hlds_solver_type(TypeDetailsSolver),
            TypeDetailsSolver = type_details_solver(Details, _MaybeCanon),
            Details = solver_type_details(RepnType, _, _, _),
            record_type_ctors_used_in_type(RepnType, !UnusedTypeCtorSet)
        ;
            ( TypeBody = hlds_foreign_type(_)
            ; TypeBody = hlds_abstract_type(_)
            )
        )
    ),
    record_type_ctors_used_in_type_defns(PredUnusedTypeCtorSet,
        TypeCtorsDefns, !UnusedTypeCtorSet).

:- pred record_type_ctors_used_in_data_ctor(constructor::in,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out) is det.

record_type_ctors_used_in_data_ctor(Ctor, !UnusedTypeCtorSet) :-
    Ctor = ctor(_Ordinal, _MaybeExist, _SymName, CtorArgs, _NumArgs, _Ctxt),
    list.foldl(record_type_ctors_used_in_data_ctor_arg, CtorArgs,
        !UnusedTypeCtorSet).

:- pred record_type_ctors_used_in_data_ctor_arg(constructor_arg::in,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out) is det.

record_type_ctors_used_in_data_ctor_arg(CtorArg, !UnusedTypeCtorSet) :-
    CtorArg = ctor_arg(_MaybeFieldName, ArgType, _Context),
    record_type_ctors_used_in_type(ArgType, !UnusedTypeCtorSet).

%---------------------------------------------------------------------------%

:- pred record_type_ctors_used_in_foreign_export_enum_info(
    item_foreign_export_enum_info::in,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out) is det.

record_type_ctors_used_in_foreign_export_enum_info(FEEInfo,
        !UnusedTypeCtorSet) :-
    TypeCtor = FEEInfo ^ fee_type_ctor,
    set_tree234.delete(TypeCtor, !UnusedTypeCtorSet).

%---------------------------------------------------------------------------%

:- pred record_type_ctors_used_in_types(list(mer_type)::in,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out) is det.

record_type_ctors_used_in_types([], !ShouldBeUsedTypeCtors).
record_type_ctors_used_in_types([Type | Types], !ShouldBeUsedTypeCtors) :-
    record_type_ctors_used_in_type(Type, !ShouldBeUsedTypeCtors),
    record_type_ctors_used_in_types(Types, !ShouldBeUsedTypeCtors).

:- pred record_type_ctors_used_in_type(mer_type::in,
    set_tree234(type_ctor)::in, set_tree234(type_ctor)::out) is det.

record_type_ctors_used_in_type(Type, !ShouldBeUsedTypeCtors) :-
    (
        ( Type = type_variable(_TVar, _Kind)
        ; Type = builtin_type(_)
        )
    ;
        Type = defined_type(SymName, ArgTypes, _Kind),
        list.length(ArgTypes, Arity),
        TypeCtor = type_ctor(SymName, Arity),
        set_tree234.delete(TypeCtor, !ShouldBeUsedTypeCtors),
        record_type_ctors_used_in_types(ArgTypes, !ShouldBeUsedTypeCtors)
    ;
        ( Type = tuple_type(ArgTypes, _Kind)
        ; Type = higher_order_type(_PorF, ArgTypes, _HOInstInfo, _Purity)
        ; Type = apply_n_type(_TVar, ArgTypes, _Kind)
        ),
        record_type_ctors_used_in_types(ArgTypes, !ShouldBeUsedTypeCtors)
    ;
        Type = kinded_type(SubType, _Kind),
        record_type_ctors_used_in_type(SubType, !ShouldBeUsedTypeCtors)
    ).

%---------------------------------------------------------------------------%

:- pred report_unused_type_ctor(type_table::in, type_ctor::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unused_type_ctor(TypeTable, TypeCtor, !Specs) :-
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    get_type_defn_context(TypeDefn, Context),
    Pieces =
        [words("Warning: type constructor"), unqual_type_ctor(TypeCtor),
        words("is")] ++
        color_as_incorrect([words("unused.")]) ++
        [nl],
    Severity = severity_warning(warn_unused_types),
    Spec = spec($pred, Severity, phase_type_check, Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
:- end_module check_hlds.unused_types.
%---------------------------------------------------------------------------%
