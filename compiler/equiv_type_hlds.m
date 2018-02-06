%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: equiv_type_hlds.m.
% Main author: stayl.
%
% Expand all types in the module_info using all equivalence type definitions,
% even those local to (transitively) imported modules.
%
% This is necessary to avoid problems with back-ends that don't support
% equivalence types properly (or at all).
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.equiv_type_hlds.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

%-----------------------------------------------------------------------------%

:- pred replace_in_hlds(module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_inst_mode.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module recompilation.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

replace_in_hlds(!ModuleInfo) :-
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    foldl2_over_type_ctor_defns(add_type_to_eqv_map, TypeTable0,
        map.init, TypeEqvMap, set.init, EqvExportTypes),
    set.fold(mark_eqv_exported_types, EqvExportTypes, TypeTable0, TypeTable1),

    module_info_get_maybe_recompilation_info(!.ModuleInfo, MaybeRecompInfo0),
    module_info_get_name(!.ModuleInfo, ModuleName),
    map_foldl_over_type_ctor_defns(
        replace_in_type_defn(ModuleName, TypeEqvMap),
        TypeTable1, TypeTable, MaybeRecompInfo0, MaybeRecompInfo),
    module_info_set_type_table(TypeTable, !ModuleInfo),
    module_info_set_maybe_recompilation_info(MaybeRecompInfo, !ModuleInfo),

    module_info_get_inst_table(!.ModuleInfo, Insts0),
    InstCache0 = map.init,
    replace_in_inst_table(TypeEqvMap, Insts0, Insts, InstCache0, InstCache),
    module_info_set_inst_table(Insts, !ModuleInfo),

    module_info_get_cons_table(!.ModuleInfo, ConsTable0),
    replace_in_cons_table(TypeEqvMap, ConsTable0, ConsTable),
    module_info_set_cons_table(ConsTable, !ModuleInfo),

    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    list.foldl2(replace_in_pred(TypeEqvMap), PredIds, !ModuleInfo,
        InstCache, _).

%-----------------------------------------------------------------------------%

:- pred add_type_to_eqv_map(type_ctor::in, hlds_type_defn::in,
    type_eqv_map::in, type_eqv_map::out,
    set(type_ctor)::in, set(type_ctor)::out) is det.

add_type_to_eqv_map(TypeCtor, Defn, !TypeEqvMap, !EqvExportTypes) :-
    hlds_data.get_type_defn_body(Defn, Body),
    (
        Body = hlds_eqv_type(EqvType),
        hlds_data.get_type_defn_tvarset(Defn, TVarSet),
        hlds_data.get_type_defn_tparams(Defn, Params),
        hlds_data.get_type_defn_status(Defn, TypeStatus),
        map.det_insert(TypeCtor, eqv_type_body(TVarSet, Params, EqvType),
            !TypeEqvMap),
        IsExported = type_status_is_exported(TypeStatus),
        (
            IsExported = yes,
            add_type_ctors_to_set(EqvType, !EqvExportTypes)
        ;
            IsExported = no
        )
    ;
        ( Body = hlds_du_type(_, _, _, _)
        ; Body = hlds_foreign_type(_)
        ; Body = hlds_solver_type(_)
        ; Body = hlds_abstract_type(_)
        )
    ).

:- pred add_type_ctors_to_set(mer_type::in,
    set(type_ctor)::in, set(type_ctor)::out) is det.

add_type_ctors_to_set(Type, !Set) :-
    ( if type_to_ctor_and_args(Type, TypeCtor, Args) then
        set.insert(TypeCtor, !Set),
        list.foldl(add_type_ctors_to_set, Args, !Set)
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- pred mark_eqv_exported_types(type_ctor::in, type_table::in, type_table::out)
    is det.

mark_eqv_exported_types(TypeCtor, !TypeTable) :-
    ( if search_type_ctor_defn(!.TypeTable, TypeCtor, TypeDefn0) then
        set_type_defn_in_exported_eqv(yes, TypeDefn0, TypeDefn),
        replace_type_ctor_defn(TypeCtor, TypeDefn, !TypeTable)
    else
        % We can get here for builtin `types' such as func. Since their unify
        % and compare preds are in the runtime system, not generated by the
        % compiler, marking them as exported in the compiler is moot.
        true
    ).

%-----------------------------------------------------------------------------%

:- pred replace_in_type_defn(module_name::in, type_eqv_map::in, type_ctor::in,
    hlds_type_defn::in, hlds_type_defn::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out) is det.

replace_in_type_defn(ModuleName, TypeEqvMap, TypeCtor, !Defn,
        !MaybeRecompInfo) :-
    hlds_data.get_type_defn_tvarset(!.Defn, TVarSet0),
    hlds_data.get_type_defn_body(!.Defn, Body0),
    TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
    TypeCtorItem = type_ctor_to_item_name(TypeCtor),
    maybe_start_recording_expanded_items(ModuleName, TypeCtorSymName,
        !.MaybeRecompInfo, EquivTypeInfo0),
    (
        Body0 = hlds_du_type(Ctors0, MaybeCanonical, MaybeRepn0, MaybeForeign),
        equiv_type.replace_in_ctors(TypeEqvMap, Ctors0, Ctors,
            TVarSet0, TVarSet1, EquivTypeInfo0, EquivTypeInfo1),
        (
            MaybeRepn0 = no,
            MaybeRepn = no,
            TVarSet = TVarSet1,
            EquivTypeInfo = EquivTypeInfo1
        ;
            MaybeRepn0 = yes(Repn0),
            Repn0 = du_type_repn(ConsIdToTagMap,
                CtorRepns0, _CtorNameToRepnMap0, CheaperTagTest,
                DuKind, DirectArgCtors),
            list.map_foldl3(replace_in_ctor_repn(TypeEqvMap),
                CtorRepns0, CtorRepns, map.init, CtorNameToRepnMap,
                TVarSet1, TVarSet, EquivTypeInfo1, EquivTypeInfo),
            Repn = du_type_repn(ConsIdToTagMap,
                CtorRepns, CtorNameToRepnMap, CheaperTagTest,
                DuKind, DirectArgCtors),
            MaybeRepn = yes(Repn)
        ),
        Body = hlds_du_type(Ctors, MaybeCanonical, MaybeRepn, MaybeForeign)
    ;
        Body0 = hlds_eqv_type(Type0),
        equiv_type.replace_in_type(TypeEqvMap, Type0, Type, _,
            TVarSet0, TVarSet, EquivTypeInfo0, EquivTypeInfo),
        Body = hlds_eqv_type(Type)
    ;
        Body0 = hlds_foreign_type(_),
        EquivTypeInfo = EquivTypeInfo0,
        Body = Body0,
        TVarSet = TVarSet0
    ;
        Body0 = hlds_solver_type(DetailsSolver0),
        DetailsSolver0 = type_details_solver(SolverTypeDetails0, UserEq),
        SolverTypeDetails0 = solver_type_details(RepnType0,
            GroundInst, AnyInst, MutableItems),
        equiv_type.replace_in_type(TypeEqvMap, RepnType0, RepnType, _,
            TVarSet0, TVarSet, EquivTypeInfo0, EquivTypeInfo),
        SolverTypeDetails = solver_type_details(RepnType,
            GroundInst, AnyInst, MutableItems),
        DetailsSolver = type_details_solver(SolverTypeDetails, UserEq),
        Body = hlds_solver_type(DetailsSolver)
    ;
        Body0 = hlds_abstract_type(_),
        EquivTypeInfo = EquivTypeInfo0,
        Body = Body0,
        TVarSet = TVarSet0
    ),
    ItemId = item_id(type_body_item, TypeCtorItem),
    finish_recording_expanded_items(ItemId, EquivTypeInfo, !MaybeRecompInfo),
    hlds_data.set_type_defn_body(Body, !Defn),
    hlds_data.set_type_defn_tvarset(TVarSet, !Defn).

:- pred replace_in_ctor_repn(type_eqv_map::in,
    constructor_repn::in, constructor_repn::out,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out,
    tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out) is det.

replace_in_ctor_repn(TypeEqvMap, CtorRepn0, CtorRepn, !CtorNameToRepnMap,
        !TVarSet, !EquivTypeInfo) :-
    CtorRepn0 = ctor_repn(MaybeExistConstraints0, CtorName, Tag,
        CtorArgRepns0, Arity, Context),
    list.map_foldl2(replace_in_ctor_arg_repn(TypeEqvMap),
        CtorArgRepns0, CtorArgRepns, !TVarSet, !EquivTypeInfo),
    (
        MaybeExistConstraints0 = no_exist_constraints,
        MaybeExistConstraints = no_exist_constraints
    ;
        MaybeExistConstraints0 = exist_constraints(
            cons_exist_constraints(ExistQVars, Constraints0)),
        replace_in_prog_constraint_list(TypeEqvMap, Constraints0, Constraints,
            !TVarSet, !EquivTypeInfo),
        MaybeExistConstraints = exist_constraints(
            cons_exist_constraints(ExistQVars, Constraints))
    ),
    CtorRepn = ctor_repn(MaybeExistConstraints, CtorName, Tag,
        CtorArgRepns, Arity, Context),
    insert_ctor_repn_into_map(CtorRepn, !CtorNameToRepnMap).

:- pred replace_in_ctor_arg_repn(type_eqv_map::in,
    constructor_arg_repn::in, constructor_arg_repn::out,
    tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out) is det.

replace_in_ctor_arg_repn(TypeEqvMap, CtorArgRepn0, CtorArgRepn,
        !TVarSet, !EquivTypeInfo) :-
    CtorArgRepn0 = ctor_arg_repn(Name, Type0, Width, Context),
    replace_in_type(TypeEqvMap, Type0, Type, Changed,
        !TVarSet, !EquivTypeInfo),
    (
        Changed = no,
        CtorArgRepn = CtorArgRepn0
    ;
        Changed = yes,
        CtorArgRepn = ctor_arg_repn(Name, Type, Width, Context)
    ).

%-----------------------------------------------------------------------------%

:- pred replace_in_inst_table(type_eqv_map::in,
    inst_table::in, inst_table::out, inst_cache::in, inst_cache::out) is det.

replace_in_inst_table(TypeEqvMap, !InstTable, !Cache) :-
% We currently have no syntax for typed user-defined insts,
% so this is unnecessary.
%
%   inst_table_get_user_insts(!.InstTable, UserInsts0),
%   map.map_values(
%       (pred(_::in, Defn0::in, Defn::out) is det :-
%           Body0 = Defn0 ^ inst_body,
%           (
%               Body0 = abstract_inst,
%               Defn = Defn0
%           ;
%               Body0 = eqv_inst(Inst0),
%               % XXX We don't have a valid tvarset here.
%               TVarSet0 = varset.init.
%               replace_in_inst(TypeEqvMap, Inst0, Inst, TVarSet0, _)
%               ...
%           ),
%           ...
%       ), UserInsts0, UserInsts),
%   inst_table_set_user_insts(!.InstTable, UserInsts, !:InstTable),

    inst_table_get_unify_insts(!.InstTable, UnifyInsts0),
    inst_table_get_merge_insts(!.InstTable, MergeInsts0),
    inst_table_get_ground_insts(!.InstTable, GroundInsts0),
    inst_table_get_any_insts(!.InstTable, AnyInsts0),
    inst_table_get_shared_insts(!.InstTable, SharedInsts0),
    inst_table_get_mostly_uniq_insts(!.InstTable, MostlyUniqInsts0),

    unify_insts_to_sorted_pairs(UnifyInsts0, UnifyInstPairs0),
    merge_insts_to_sorted_pairs(MergeInsts0, MergeInstPairs0),
    ground_insts_to_sorted_pairs(GroundInsts0, GroundInstPairs0),
    any_insts_to_sorted_pairs(AnyInsts0, AnyInstPairs0),
    shared_insts_to_sorted_pairs(SharedInsts0, SharedInstPairs0),
    mostly_uniq_insts_to_sorted_pairs(MostlyUniqInsts0, MostlyUniqInstPairs0),

    replace_in_one_inst_table(
        replace_in_unify_inst_info, replace_in_maybe_inst_det,
        TypeEqvMap, UnifyInstPairs0, UnifyInstPairs, !Cache),
    replace_in_one_inst_table(
        replace_in_merge_inst_info, replace_in_maybe_inst,
        TypeEqvMap, MergeInstPairs0, MergeInstPairs, !Cache),
    replace_in_one_inst_table(
        replace_in_ground_inst_info, replace_in_maybe_inst_det,
        TypeEqvMap, GroundInstPairs0, GroundInstPairs, !Cache),
    replace_in_one_inst_table(
        replace_in_any_inst_info, replace_in_maybe_inst_det,
        TypeEqvMap, AnyInstPairs0, AnyInstPairs, !Cache),
    replace_in_one_inst_table(
        replace_in_inst_name_no_tvarset, replace_in_maybe_inst,
        TypeEqvMap, SharedInstPairs0, SharedInstPairs, !Cache),
    replace_in_one_inst_table(
        replace_in_inst_name_no_tvarset, replace_in_maybe_inst,
        TypeEqvMap, MostlyUniqInstPairs0, MostlyUniqInstPairs, !.Cache, _),

    unify_insts_from_sorted_pairs(UnifyInstPairs, UnifyInsts),
    merge_insts_from_sorted_pairs(MergeInstPairs, MergeInsts),
    ground_insts_from_sorted_pairs(GroundInstPairs, GroundInsts),
    any_insts_from_sorted_pairs(AnyInstPairs, AnyInsts),
    shared_insts_from_sorted_pairs(SharedInstPairs, SharedInsts),
    mostly_uniq_insts_from_sorted_pairs(MostlyUniqInstPairs, MostlyUniqInsts),

    inst_table_set_unify_insts(UnifyInsts, !InstTable),
    inst_table_set_merge_insts(MergeInsts, !InstTable),
    inst_table_set_ground_insts(GroundInsts, !InstTable),
    inst_table_set_any_insts(AnyInsts, !InstTable),
    inst_table_set_shared_insts(SharedInsts, !InstTable),
    inst_table_set_mostly_uniq_insts(MostlyUniqInsts, !InstTable).

%-----------------------------------------------------------------------------%
%
% In almost all cases, the expansion of equivalence types leaves inst names
% and insts themselves unchanged. The code below is optimized for that,
% in that we preserve the sortedness of the association lists as far as we can.
%
% We could also check whether *any* element of the original SortedElements0
% has changed, and if not, return the original Map0. The reason why we don't
% is (a), for programs with few entries in the inst tables, it would yield
% only a negligible improvement (there is nothing to optimize), while for
% programs with lots of entries in the inst tables, it is much more likely
% that *some* expansions *do* change some elements, so the overhead of keeping
% track of the absence/presence of changes won't be paid back.
%
% Expanding equivalence types should virtually never make two existing insts
% the same, but in some rare circumstances it might, so we have to handle that.
%

:- pred replace_in_one_inst_table(
    pred(type_eqv_map, K, K, bool, inst_cache, inst_cache)::
        (pred(in, in, out, out, in, out) is det),
    pred(type_eqv_map, V, V, bool, inst_cache, inst_cache)::
        (pred(in, in, out, out, in, out) is det),
    type_eqv_map::in, assoc_list(K, V)::in, assoc_list(K, V)::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_one_inst_table(ReplaceKey, ReplaceValue, TypeEqvMap,
        SortedElements0, SortedElements, !Cache) :-
    replace_in_one_inst_table_elements(ReplaceKey, ReplaceValue, TypeEqvMap,
        SortedElements0, [], RevSortedElements1, [], UnSortedElements, !Cache),
    (
        UnSortedElements = [],
        list.reverse(RevSortedElements1, SortedElements)
    ;
        UnSortedElements = [_ | _],
        list.reverse(RevSortedElements1, SortedElements1),
        list.sort_and_remove_dups(UnSortedElements, NowSortedElements),
        list.merge_and_remove_dups(SortedElements1, NowSortedElements,
            SortedElements)
    ).

:- pred replace_in_one_inst_table_elements(
    pred(type_eqv_map, K, K, bool, inst_cache, inst_cache)::
        (pred(in, in, out, out, in, out) is det),
    pred(type_eqv_map, V, V, bool, inst_cache, inst_cache)::
        (pred(in, in, out, out, in, out) is det),
    type_eqv_map::in, assoc_list(K, V)::in,
    assoc_list(K, V)::in, assoc_list(K, V)::out,
    assoc_list(K, V)::in, assoc_list(K, V)::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_one_inst_table_elements(_ReplaceKey, _ReplaceValue, _EqvMap, [],
        !RevSortedElements, !UnSortedElements, !Cache).
replace_in_one_inst_table_elements(ReplaceKey, ReplaceValue, TypeEqvMap,
        [Element0 | Elements0],
        !RevSortedElements, !UnSortedElements, !Cache) :-
    Element0 = Key0 - Value0,
    ReplaceKey(TypeEqvMap, Key0, Key, KeyChanged, !Cache),
    ReplaceValue(TypeEqvMap, Value0, Value, ValueChanged, !Cache),
    (
        KeyChanged = no,
        (
            ValueChanged = no,
            Element = Element0
        ;
            ValueChanged = yes,
            Element = Key0 - Value
        ),
        !:RevSortedElements = [Element | !.RevSortedElements]
    ;
        KeyChanged = yes,
        Element = Key - Value,
        !:UnSortedElements = [Element | !.UnSortedElements]
    ),
    replace_in_one_inst_table_elements(ReplaceKey, ReplaceValue, TypeEqvMap,
        Elements0, !RevSortedElements, !UnSortedElements, !Cache).

:- pred replace_in_unify_inst_info(type_eqv_map::in,
    unify_inst_info::in, unify_inst_info::out, bool::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_unify_inst_info(TypeEqvMap, UnifyInstInfo0, UnifyInstInfo, Changed,
        !Cache) :-
    UnifyInstInfo0 = unify_inst_info(Live, Real, InstA0, InstB0),
    % XXX We don't have a valid tvarset here.
    varset.init(TVarSet0),
    replace_in_inst(TypeEqvMap, InstA0, InstA, ChangedA, TVarSet0, TVarSet1,
        !Cache),
    replace_in_inst(TypeEqvMap, InstB0, InstB, ChangedB, TVarSet1, _TVarSet,
        !Cache),
    Changed = ChangedA `or` ChangedB,
    ( Changed = yes, UnifyInstInfo = unify_inst_info(Live, Real, InstA, InstB)
    ; Changed = no, UnifyInstInfo = UnifyInstInfo0
    ).

:- pred replace_in_merge_inst_info(type_eqv_map::in,
    merge_inst_info::in, merge_inst_info::out, bool::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_merge_inst_info(TypeEqvMap, MergeInstInfo0, MergeInstInfo, Changed,
        !Cache) :-
    MergeInstInfo0 = merge_inst_info(InstA0, InstB0),
    % XXX We don't have a valid tvarset here.
    varset.init(TVarSet0),
    replace_in_inst(TypeEqvMap, InstA0, InstA, ChangedA, TVarSet0, TVarSet1,
        !Cache),
    replace_in_inst(TypeEqvMap, InstB0, InstB, ChangedB, TVarSet1, _TVarSet,
        !Cache),
    Changed = ChangedA `or` ChangedB,
    ( Changed = yes, MergeInstInfo = merge_inst_info(InstA, InstB)
    ; Changed = no, MergeInstInfo = MergeInstInfo0
    ).

:- pred replace_in_ground_inst_info(type_eqv_map::in,
    ground_inst_info::in, ground_inst_info::out, bool::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_ground_inst_info(TypeEqvMap, GroundInstInfo0, GroundInstInfo,
        Changed, !Cache) :-
    GroundInstInfo0 = ground_inst_info(InstName0, Uniq, Live, Real),
    replace_in_inst_name_no_tvarset(TypeEqvMap, InstName0, InstName, Changed,
        !Cache),
    (
        Changed = yes,
        GroundInstInfo = ground_inst_info(InstName, Uniq, Live, Real)
    ;
        Changed = no,
        GroundInstInfo = GroundInstInfo0
    ).

:- pred replace_in_any_inst_info(type_eqv_map::in,
    any_inst_info::in, any_inst_info::out, bool::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_any_inst_info(TypeEqvMap, AnyInstInfo0, AnyInstInfo,
        Changed, !Cache) :-
    AnyInstInfo0 = any_inst_info(InstName0, Uniq, Live, Real),
    replace_in_inst_name_no_tvarset(TypeEqvMap, InstName0, InstName, Changed,
        !Cache),
    ( Changed = yes, AnyInstInfo = any_inst_info(InstName, Uniq, Live, Real)
    ; Changed = no, AnyInstInfo = AnyInstInfo0
    ).

:- pred replace_in_maybe_inst(type_eqv_map::in,
    maybe_inst::in, maybe_inst::out, bool::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_maybe_inst(TypeEqvMap, MaybeInst0, MaybeInst, Changed, !Cache) :-
    (
        MaybeInst0 = inst_unknown,
        MaybeInst = inst_unknown,
        Changed = no
    ;
        MaybeInst0 = inst_known(Inst0),
        % XXX We don't have a valid tvarset here.
        varset.init(TVarSet),
        replace_in_inst(TypeEqvMap, Inst0, Inst, Changed, TVarSet, _, !Cache),
        (
            Changed = no,
            MaybeInst = MaybeInst0
        ;
            Changed = yes,
            MaybeInst = inst_known(Inst)
        )
    ).

:- pred replace_in_maybe_inst_det(type_eqv_map::in,
    maybe_inst_det::in, maybe_inst_det::out, bool::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_maybe_inst_det(TypeEqvMap, MaybeInstDet0, MaybeInstDet, Changed,
        !Cache) :-
    (
        MaybeInstDet0 = inst_det_unknown,
        MaybeInstDet = inst_det_unknown,
        Changed = no
    ;
        MaybeInstDet0 = inst_det_known(Inst0, Det),
        % XXX We don't have a valid tvarset here.
        varset.init(TVarSet),
        replace_in_inst(TypeEqvMap, Inst0, Inst, Changed, TVarSet, _, !Cache),
        (
            Changed = no,
            MaybeInstDet = MaybeInstDet0
        ;
            Changed = yes,
            MaybeInstDet = inst_det_known(Inst, Det)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred replace_in_cons_table(type_eqv_map::in,
    cons_table::in, cons_table::out) is det.

replace_in_cons_table(TypeEqvMap, !ConsTable) :-
    replace_cons_defns_in_cons_table(replace_in_cons_defn(TypeEqvMap),
        !ConsTable).

:- pred replace_in_cons_defn(type_eqv_map::in,
    hlds_cons_defn::in, hlds_cons_defn::out) is det.

replace_in_cons_defn(TypeEqvMap, ConsDefn0, ConsDefn) :-
    ConsDefn0 = hlds_cons_defn(TypeCtor, TVarSet0, TypeParams, KindMap,
        MaybeExistConstraints, ConstructorArgs0, Context),
    list.map_foldl(replace_in_constructor_arg(TypeEqvMap),
        ConstructorArgs0, ConstructorArgs, TVarSet0, TVarSet),
    ConsDefn = hlds_cons_defn(TypeCtor, TVarSet, TypeParams, KindMap,
        MaybeExistConstraints, ConstructorArgs, Context).

:- pred replace_in_constructor_arg(type_eqv_map::in,
    constructor_arg::in, constructor_arg::out,
    tvarset::in, tvarset::out) is det.

replace_in_constructor_arg(TypeEqvMap, CtorArg0, CtorArg, !TVarSet) :-
    CtorArg0 = ctor_arg(MaybeFieldName, Type0, Context),
    replace_in_type(TypeEqvMap, Type0, Type, Changed, !TVarSet, no, _),
    (
        Changed = yes,
        CtorArg = ctor_arg(MaybeFieldName, Type, Context)
    ;
        Changed = no,
        CtorArg = CtorArg0
    ).

%-----------------------------------------------------------------------------%

:- pred replace_in_pred(type_eqv_map::in, pred_id::in,
    module_info::in, module_info::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_pred(TypeEqvMap, PredId, !ModuleInfo, !Cache) :-
    some [!PredInfo, !EquivTypeInfo] (
        module_info_get_name(!.ModuleInfo, ModuleName),
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        module_info_get_maybe_recompilation_info(!.ModuleInfo,
            MaybeRecompInfo0),

        PredName = pred_info_name(!.PredInfo),
        maybe_start_recording_expanded_items(ModuleName,
            qualified(ModuleName, PredName), MaybeRecompInfo0,
            !:EquivTypeInfo),

        pred_info_get_arg_types(!.PredInfo, ArgTVarSet0, ExistQVars,
            ArgTypes0),
        equiv_type.replace_in_type_list(TypeEqvMap, ArgTypes0, ArgTypes,
            _, ArgTVarSet0, ArgTVarSet1, !EquivTypeInfo),

        % The constraint_proofs aren't used after polymorphism,
        % so they don't need to be processed.
        pred_info_get_class_context(!.PredInfo, ClassContext0),
        equiv_type.replace_in_prog_constraints(TypeEqvMap, ClassContext0,
            ClassContext, ArgTVarSet1, ArgTVarSet, !EquivTypeInfo),
        pred_info_set_class_context(ClassContext, !PredInfo),
        pred_info_set_arg_types(ArgTVarSet, ExistQVars, ArgTypes, !PredInfo),

        ItemId = item_id(pred_or_func_to_item_type(
            pred_info_is_pred_or_func(!.PredInfo)),
            item_name(qualified(pred_info_module(!.PredInfo), PredName),
                pred_info_orig_arity(!.PredInfo))),
        finish_recording_expanded_items(ItemId, !.EquivTypeInfo,
            MaybeRecompInfo0, MaybeRecompInfo),
        module_info_set_maybe_recompilation_info(MaybeRecompInfo, !ModuleInfo),

        pred_info_get_proc_table(!.PredInfo, ProcMap0),
        map.map_values_foldl3(replace_in_proc(TypeEqvMap), ProcMap0, ProcMap,
            !ModuleInfo, !PredInfo, !Cache),
        pred_info_set_proc_table(ProcMap, !PredInfo),
        module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo)
    ).

:- pred replace_in_proc(type_eqv_map::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out, pred_info::in, pred_info::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_proc(TypeEqvMap, !ProcInfo, !ModuleInfo, !PredInfo, !Cache) :-
    some [!TVarSet] (
        pred_info_get_typevarset(!.PredInfo, !:TVarSet),

        proc_info_get_argmodes(!.ProcInfo, ArgModes0),
        replace_in_modes(TypeEqvMap, ArgModes0, ArgModes, _, !TVarSet, !Cache),
        proc_info_set_argmodes(ArgModes, !ProcInfo),

        proc_info_get_maybe_declared_argmodes(!.ProcInfo, MaybeDeclModes0),
        (
            MaybeDeclModes0 = yes(DeclModes0),
            replace_in_modes(TypeEqvMap, DeclModes0, DeclModes, _, !TVarSet,
                !Cache),
            proc_info_set_maybe_declared_argmodes(yes(DeclModes), !ProcInfo)
        ;
            MaybeDeclModes0 = no
        ),

        proc_info_get_vartypes(!.ProcInfo, VarTypes0),
        transform_foldl_var_types(hlds_replace_in_type(TypeEqvMap),
            VarTypes0, VarTypes, !TVarSet),
        proc_info_set_vartypes(VarTypes, !ProcInfo),

        proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
        rtti_varmaps_types(RttiVarMaps0, AllTypes),
        list.foldl2(
            (pred(OldType::in, !.TMap::in, !:TMap::out,
                    !.TVarSet::in, !:TVarSet::out) is det :-
                hlds_replace_in_type(TypeEqvMap, OldType, NewType, !TVarSet),
                map.set(OldType, NewType, !TMap)
            ), AllTypes, map.init, TypeMap, !TVarSet),
        rtti_varmaps_transform_types(map.lookup(TypeMap),
            RttiVarMaps0, RttiVarMaps),
        proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),

        proc_info_get_goal(!.ProcInfo, Goal0),
        ReplaceInfo0 = replace_info(!.ModuleInfo, !.PredInfo, !.ProcInfo,
            !.TVarSet, !.Cache, no),
        replace_in_goal(TypeEqvMap, Goal0, Goal, Changed,
            ReplaceInfo0, ReplaceInfo),
        ReplaceInfo = replace_info(!:ModuleInfo, !:PredInfo, !:ProcInfo,
            !:TVarSet, _XXX, Recompute),
        (
            Changed = yes,
            proc_info_set_goal(Goal, !ProcInfo)
        ;
            Changed = no
        ),
        (
            Recompute = yes,
            requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),
            recompute_instmap_delta_proc(
                do_not_recompute_atomic_instmap_deltas, !ProcInfo, !ModuleInfo)
        ;
            Recompute = no
        ),
        pred_info_set_typevarset(!.TVarSet, !PredInfo)
    ).

%-----------------------------------------------------------------------------%

    % Replace equivalence types in a given type.
    %
:- pred hlds_replace_in_type(type_eqv_map::in, mer_type::in, mer_type::out,
    tvarset::in, tvarset::out) is det.

hlds_replace_in_type(TypeEqvMap, Type0, Type, !VarSet) :-
    hlds_replace_in_type_2(TypeEqvMap, [], Type0, Type, _Changed, !VarSet).

:- pred hlds_replace_in_type_2(type_eqv_map::in, list(type_ctor)::in,
    mer_type::in, mer_type::out, bool::out,
    tvarset::in, tvarset::out) is det.

hlds_replace_in_type_2(TypeEqvMap, TypeCtorsAlreadyExpanded,
        Type0, Type, Changed, !VarSet) :-
    (
        ( Type0 = type_variable(_, _)
        ; Type0 = builtin_type(_)
        ),
        Type = Type0,
        Changed = no
    ;
        Type0 = defined_type(SymName, TypeArgs0, Kind),
        hlds_replace_in_type_list_2(TypeEqvMap, TypeCtorsAlreadyExpanded,
            TypeArgs0, TypeArgs, no, ArgsChanged, !VarSet),
        Arity = list.length(TypeArgs),
        TypeCtor = type_ctor(SymName, Arity),
        hlds_replace_type_ctor(TypeEqvMap, TypeCtorsAlreadyExpanded, Type0,
            TypeCtor, TypeArgs, Kind, Type, ArgsChanged, Changed, !VarSet)
    ;
        Type0 = higher_order_type(PorF, ArgTypes0, HOInstInfo, Purity,
            EvalMethod),
        % XXX replace in HOInstInfo
        HOInstInfoChanged = no,
        hlds_replace_in_type_list_2(TypeEqvMap, TypeCtorsAlreadyExpanded,
            ArgTypes0, ArgTypes, HOInstInfoChanged, Changed, !VarSet),
        (
            Changed = yes,
            Type = higher_order_type(PorF, ArgTypes, HOInstInfo, Purity,
                EvalMethod)
        ;
            Changed = no,
            Type = Type0
        )
    ;
        Type0 = tuple_type(Args0, Kind),
        hlds_replace_in_type_list_2(TypeEqvMap, TypeCtorsAlreadyExpanded,
            Args0, Args, no, Changed, !VarSet),
        (
            Changed = yes,
            Type = tuple_type(Args, Kind)
        ;
            Changed = no,
            Type = Type0
        )
    ;
        Type0 = apply_n_type(Var, Args0, Kind),
        hlds_replace_in_type_list_2(TypeEqvMap, TypeCtorsAlreadyExpanded,
            Args0, Args, no, Changed, !VarSet),
        (
            Changed = yes,
            Type = apply_n_type(Var, Args, Kind)
        ;
            Changed = no,
            Type = Type0
        )
    ;
        Type0 = kinded_type(RawType0, Kind),
        hlds_replace_in_type_2(TypeEqvMap, TypeCtorsAlreadyExpanded,
            RawType0, RawType, Changed, !VarSet),
        (
            Changed = yes,
            Type = kinded_type(RawType, Kind)
        ;
            Changed = no,
            Type = Type0
        )
    ).

:- pred hlds_replace_in_type_list_2(type_eqv_map::in, list(type_ctor)::in,
    list(mer_type)::in, list(mer_type)::out, bool::in, bool::out,
    tvarset::in, tvarset::out) is det.

hlds_replace_in_type_list_2(_EqvMap, _Seen, [], [], !Changed, !VarSet).
hlds_replace_in_type_list_2(TypeEqvMap, Seen, [Type0 | Types0], [Type | Types],
        !Changed, !VarSet) :-
    hlds_replace_in_type_2(TypeEqvMap, Seen, Type0, Type, TypeChanged,
        !VarSet),
    bool.or(!.Changed, TypeChanged, !:Changed),
    hlds_replace_in_type_list_2(TypeEqvMap, Seen, Types0, Types,
        !Changed, !VarSet).

:- pred hlds_replace_type_ctor(type_eqv_map::in, list(type_ctor)::in,
    mer_type::in, type_ctor::in, list(mer_type)::in, kind::in, mer_type::out,
    bool::in, bool::out, tvarset::in, tvarset::out) is det.

hlds_replace_type_ctor(TypeEqvMap, TypeCtorsAlreadyExpanded0, Type0,
        TypeCtor, ArgTypes, Kind, Type, !Changed, !VarSet) :-
    ( if list.member(TypeCtor, TypeCtorsAlreadyExpanded0) then
        AlreadyExpanded = yes
    else
        AlreadyExpanded = no
    ),
    ( if
        map.search(TypeEqvMap, TypeCtor, EqvTypeBody),
        EqvTypeBody = eqv_type_body(EqvVarSet, Params0, Body0),

        % Don't merge in the variable names from the type declaration to avoid
        % creating multiple variables with the same name so that
        % `varset.create_name_var_map' can be used on the resulting tvarset.
        % make_hlds uses `varset.create_name_var_map' to match up type
        % variables in `:- pragma type_spec' declarations and explicit type
        % qualifications with the type variables in the predicate's
        % declaration.

        tvarset_merge_renaming_without_names(!.VarSet, EqvVarSet, !:VarSet,
            Renaming),
        AlreadyExpanded = no
    then
        map.apply_to_list(Params0, Renaming, Params),
        apply_variable_renaming_to_type(Renaming, Body0, Body1),
        map.from_corresponding_lists(Params, ArgTypes, Subst),
        apply_subst_to_type(Subst, Body1, Body),
        TypeCtorsAlreadyExpanded = [TypeCtor | TypeCtorsAlreadyExpanded0],
        hlds_replace_in_type_2(TypeEqvMap, TypeCtorsAlreadyExpanded,
            Body, Type, _BodyChanged, !VarSet),
        !:Changed = yes
    else
        (
            !.Changed = yes,
            TypeCtor = type_ctor(SymName, _Arity),
            Type = defined_type(SymName, ArgTypes, Kind)
        ;
            !.Changed = no,
            Type = Type0
        )
    ).

%-----------------------------------------------------------------------------%

% Note that we go out of our way to avoid duplicating unchanged
% insts and modes. This means we don't need to hash-cons those insts
% to avoid losing sharing.

:- pred replace_in_modes(type_eqv_map::in,
    list(mer_mode)::in, list(mer_mode)::out, bool::out,
    tvarset::in, tvarset::out, inst_cache::in, inst_cache::out) is det.

replace_in_modes(_EqvMap, [], [], no, !TVarSet, !Cache).
replace_in_modes(TypeEqvMap, List0 @ [Mode0 | Modes0], List, Changed,
        !TVarSet, !Cache) :-
    replace_in_mode(TypeEqvMap, Mode0, Mode, Changed0, !TVarSet, !Cache),
    replace_in_modes(TypeEqvMap, Modes0, Modes, Changed1, !TVarSet, !Cache),
    Changed = Changed0 `or` Changed1,
    ( Changed = yes, List = [Mode | Modes]
    ; Changed = no, List = List0
    ).

:- pred replace_in_mode(type_eqv_map::in,
    mer_mode::in, mer_mode::out, bool::out,
    tvarset::in, tvarset::out, inst_cache::in, inst_cache::out) is det.

replace_in_mode(TypeEqvMap, Mode0, Mode, Changed, !TVarSet, !Cache) :-
    (
        Mode0 = from_to_mode(Init0, Final0),
        replace_in_inst(TypeEqvMap, Init0,  Init,  ChangedA, !TVarSet, !Cache),
        replace_in_inst(TypeEqvMap, Final0, Final, ChangedB, !TVarSet, !Cache),
        Changed = ChangedA `or` ChangedB,
        ( Changed = yes, Mode = from_to_mode(Init, Final)
        ; Changed = no, Mode = Mode0
        )
    ;
        Mode0 = user_defined_mode(Name, Insts0),
        replace_in_insts(TypeEqvMap, Insts0, Insts, Changed, !TVarSet, !Cache),
        ( Changed = yes, Mode = user_defined_mode(Name, Insts)
        ; Changed = no, Mode = Mode0
        )
    ).

:- pred replace_in_unify_modes(type_eqv_map::in,
    list(unify_mode)::in, list(unify_mode)::out, bool::out,
    replace_info::in, replace_info::out) is det.

replace_in_unify_modes(_EqvMap, [], [], no, !Info).
replace_in_unify_modes(TypeEqvMap, List0 @ [UnifyMode0 | UnifyModes0], List,
        Changed, !Info) :-
    replace_in_unify_mode(TypeEqvMap, UnifyMode0, UnifyMode,
        Changed0, !Info),
    replace_in_unify_modes(TypeEqvMap, UnifyModes0, UnifyModes,
        Changed1, !Info),
    Changed = Changed0 `or` Changed1,
    ( Changed = yes, List = [UnifyMode | UnifyModes]
    ; Changed = no, List = List0
    ).

:- pred replace_in_unify_mode(type_eqv_map::in,
    unify_mode::in, unify_mode::out, bool::out,
    replace_info::in, replace_info::out) is det.

replace_in_unify_mode(TypeEqvMap, UnifyMode0, UnifyMode, Changed, !Info) :-
    UnifyMode0 = unify_modes_lhs_rhs(LHSFromToInsts0, RHSFromToInsts0),
    some [!TVarSet, !Cache] (
        !:TVarSet = !.Info ^ ethri_tvarset,
        !:Cache = !.Info ^ ethri_inst_cache,
        replace_in_from_to_insts(TypeEqvMap, LHSFromToInsts0, LHSFromToInsts,
            ChangedA, !TVarSet, !Cache),
        replace_in_from_to_insts(TypeEqvMap, RHSFromToInsts0, RHSFromToInsts,
            ChangedB, !TVarSet, !Cache),
        Changed = ChangedA `or` ChangedB,
        (
            Changed = yes,
            !Info ^ ethri_tvarset := !.TVarSet,
            !Info ^ ethri_inst_cache := !.Cache,
            UnifyMode = unify_modes_lhs_rhs(LHSFromToInsts, RHSFromToInsts)
        ;
            Changed = no,
            UnifyMode = UnifyMode0
        )
    ).

:- pred replace_in_from_to_insts(type_eqv_map::in,
    from_to_insts::in, from_to_insts::out, bool::out,
    tvarset::in, tvarset::out, inst_cache::in, inst_cache::out) is det.

replace_in_from_to_insts(TypeEqvMap, FromToInsts0, FromToInsts, Changed,
        !TVarSet, !Cache) :-
    FromToInsts0 = from_to_insts(Init0, Final0),
    replace_in_inst(TypeEqvMap, Init0,  Init,  ChangedA, !TVarSet, !Cache),
    replace_in_inst(TypeEqvMap, Final0, Final, ChangedB, !TVarSet, !Cache),
    Changed = ChangedA `or` ChangedB,
    ( Changed = yes, FromToInsts = from_to_insts(Init, Final)
    ; Changed = no, FromToInsts = FromToInsts0
    ).

:- pred replace_in_inst(type_eqv_map::in, mer_inst::in, mer_inst::out,
    bool::out, tvarset::in, tvarset::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_inst(TypeEqvMap, Inst0, Inst, Changed, !TVarSet, !Cache) :-
    % The call to replace_in_inst_2 can allocate a *lot* of cells if the
    % inst is complex, as it will be for an inst describing a large term.
    % The fact that we traverse the inst twice if ContainsType = yes
    % shouldn't be a problem, since we expect that ContainsType = no
    % almost all the time.

    ContainsType = type_may_occur_in_inst(Inst0),
    (
        ContainsType = yes,
        replace_in_inst_2(TypeEqvMap, Inst0, Inst1, Changed, !TVarSet, !Cache),
        (
            Changed = yes,
            % Doing this when the inst has not changed is too slow,
            % and makes the cache potentially very large.
            hash_cons_inst(Inst1, Inst, !Cache)
        ;
            Changed = no,
            Inst = Inst1
        )
    ;
        ContainsType = no,
        Inst = Inst0,
        Changed = no
    ).

%-----------------------------------------------------------------------------%

    % Return true if any type may occur inside the given inst.
    %
    % The logic here should be a conservative approximation of the code
    % of replace_in_inst_2.
    %
    % The no_inline pragma is there to allow lookup_inst_may_occur
    % and record_inst_may_occur to be inlined in type_may_occur_in_inst
    % WITHOUT any possibility, however remote, of them being inlined
    % in procedures outside this module. Since the table they deal with
    % is local to this module, such cross-module inlining would not work.
    %
:- func type_may_occur_in_inst(mer_inst) = bool.
:- pragma no_inline(type_may_occur_in_inst/1).

type_may_occur_in_inst(Inst) = MayOccur :-
    (
        ( Inst = free
        ; Inst = ground(_, none_or_default_func)
        ; Inst = any(_, none_or_default_func)
        ; Inst = not_reached
        ; Inst = inst_var(_)
        ),
        MayOccur = no
    ;
        % The last three entries here are conservative approximations;
        % e.g. the _PredInstInfo may contain a reference to a type.
        ( Inst = free(_)
        ; Inst = ground(_, higher_order(_PredInstInfo))
        ; Inst = any(_, higher_order(_PredInstInfo))
        ; Inst = defined_inst(_)
        ),
        MayOccur = yes
    ;
        Inst = bound(_, InstResults, BoundInsts),
        (
            InstResults = inst_test_results_fgtc,
            MayOccur = no
        ;
            (
                InstResults = inst_test_results(_, _, _, _, TypeResult, _)
            ;
                InstResults = inst_test_no_results,
                TypeResult = inst_result_contains_types_unknown
            ),
            (
                TypeResult = inst_result_contains_types_known(TypeCtors),
                ( if set.is_empty(TypeCtors) then
                    MayOccur = no
                else
                    MayOccur = yes
                )
            ;
                TypeResult = inst_result_contains_types_unknown,
                % XXX Do we still need this cache, or are things now
                % fast enough without it?
                promise_pure (
                    semipure lookup_inst_may_occur(Inst, Found, OldMayOccur),
                    (
                        Found = yes,
                        MayOccur = OldMayOccur
                    ;
                        Found = no,
                        MayOccur = type_may_occur_in_bound_insts(BoundInsts),
                        impure record_inst_may_occur(Inst, MayOccur)
                    )
                )
            )
        )
    ;
        Inst = abstract_inst(_, ArgInsts),
        MayOccur = type_may_occur_in_insts(ArgInsts)
    ;
        Inst = constrained_inst_vars(_, CInst),
        MayOccur = type_may_occur_in_inst(CInst)
    ).

    % Return true if any type may occur inside any of the given bound insts.
    %
    % The logic here should be a conservative approximation of the code
    % of replace_in_bound_insts.
    %
:- func type_may_occur_in_bound_insts(list(bound_inst)) = bool.

type_may_occur_in_bound_insts([]) = no.
type_may_occur_in_bound_insts([BoundInst | BoundInsts]) = MayOccur :-
    BoundInst = bound_functor(_, Insts),
    MayOccurInInsts = type_may_occur_in_insts(Insts),
    (
        MayOccurInInsts = yes,
        MayOccur = yes
    ;
        MayOccurInInsts = no,
        MayOccur = type_may_occur_in_bound_insts(BoundInsts)
    ).

    % Return true if any type may occur inside any of the given insts.
    %
    % The logic here should be a conservative approximation of the code
    % of replace_in_insts.
    %
:- func type_may_occur_in_insts(list(mer_inst)) = bool.

type_may_occur_in_insts([]) = no.
type_may_occur_in_insts([Inst | Insts]) = MayOccur :-
    MayOccurInInst = type_may_occur_in_inst(Inst),
    (
        MayOccurInInst = yes,
        MayOccur = yes
    ;
        MayOccurInInst = no,
        MayOccur = type_may_occur_in_insts(Insts)
    ).

%-----------------------------------------------------------------------------%
%
% The expansion of terms by the superhomogeneous transformation generates code
% that looks like this:
%
%   V1 = [],
%   V2 = e1,
%   V3 = [V2 | V1],
%   V4 = e2,
%   V5 = [V3 | V4]
%
% The insts on those unifications will contain insts from earlier unifications.
% For example, the inst on the unification building V5 will give V5 an inst
% that contains the insts of V3 and V4.
%
% If there are N elements in a list, testing the insts of the N variables
% representing the N cons cells in the list would ordinarily take O(N^2) steps.
% Since N could be very large, this is disastrous.
%
% We avoid quadratic performance by caching the results of recent calls
% to type_may_occur_in_inst for insts that are susceptible to this problem.
% This way, the test on the inst of e.g. V5 will find the results of the tests
% on the insts of V3 and V4 already available. This reduces the overall
% complexity of testing the insts of those N variables to O(n).
%
% The downsides of this cache include the costs of the lookups, and
% the fact that it keeps the cached insts alive.

:- pragma foreign_decl("C",
"
typedef struct {
    MR_Word     tice_inst_addr;
    MR_Word     tice_may_occur;
} TypeInInstCacheEntry;

#define TYPE_IN_INST_CACHE_SIZE 1307

/*
** Every entry should be implicitly initialized to zeros. Since zero is
** not a valid address for an inst, uninitialized entries cannot be mistaken
** for filled-in entries.
*/

static  TypeInInstCacheEntry  type_in_inst_cache[TYPE_IN_INST_CACHE_SIZE];
").

    % Look up Inst in the cache. If it is there, return Found = yes
    % and set MayOccur. Otherwise, return Found = no.
    %
:- semipure pred lookup_inst_may_occur(mer_inst::in,
    bool::out, bool::out) is det.

:- pragma foreign_proc("C",
    lookup_inst_may_occur(Inst::in, Found::out, MayOccur::out),
    [will_not_call_mercury, promise_semipure],
"
    MR_Unsigned hash;

    hash = (MR_Unsigned) Inst;
    hash = hash >> MR_LOW_TAG_BITS;
    hash = hash % TYPE_IN_INST_CACHE_SIZE;

    if (type_in_inst_cache[hash].tice_inst_addr == Inst) {
        Found = MR_BOOL_YES;
        MayOccur = type_in_inst_cache[hash].tice_may_occur;
    } else {
        Found = MR_BOOL_NO;
    }
").

lookup_inst_may_occur(_, no, no) :-
    semipure semipure_true.

    % Record the result for Inst in the cache.
    %
:- impure pred record_inst_may_occur(mer_inst::in, bool::in) is det.

:- pragma foreign_proc("C",
    record_inst_may_occur(Inst::in, MayOccur::in),
    [will_not_call_mercury],
"
    MR_Unsigned hash;

    hash = (MR_Unsigned) Inst;
    hash = hash >> MR_LOW_TAG_BITS;
    hash = hash % TYPE_IN_INST_CACHE_SIZE;
    /* We overwrite any existing entry in the slot. */
    type_in_inst_cache[hash].tice_inst_addr = Inst;
    type_in_inst_cache[hash].tice_may_occur = MayOccur;
").

record_inst_may_occur(_, _) :-
    impure impure_true.

%-----------------------------------------------------------------------------%

:- pred replace_in_inst_2(type_eqv_map::in,
    mer_inst::in, mer_inst::out, bool::out,
    tvarset::in, tvarset::out, inst_cache::in, inst_cache::out) is det.

replace_in_inst_2(TypeEqvMap, Inst0, Inst, Changed, !TVarSet, !Cache) :-
    (
        ( Inst0 = free
        ; Inst0 = ground(_, none_or_default_func)
        ; Inst0 = any(_, none_or_default_func)
        ; Inst0 = not_reached
        ; Inst0 = inst_var(_)
        ),
        Inst = Inst0,
        Changed = no
    ;
        Inst0 = any(Uniq, higher_order(PredInstInfo0)),
        PredInstInfo0 = pred_inst_info(PorF, Modes0, MaybeArgRegs, Det),
        replace_in_modes(TypeEqvMap, Modes0, Modes, Changed, !TVarSet, !Cache),
        (
            Changed = yes,
            PredInstInfo = pred_inst_info(PorF, Modes, MaybeArgRegs, Det),
            Inst = any(Uniq, higher_order(PredInstInfo))
        ;
            Changed = no,
            Inst = Inst0
        )
    ;
        Inst0 = free(Type0),
        equiv_type.replace_in_type(TypeEqvMap, Type0, Type, Changed, !TVarSet,
            no, _),
        ( Changed = yes, Inst = free(Type)
        ; Changed = no, Inst = Inst0
        )
    ;
        Inst0 = bound(Uniq, InstResults0, BoundInsts0),
        (
            InstResults0 = inst_test_results(GroundnessResult, AnyResult,
                InstNamesResult, InstVarsResult, TypeResult, PropagatedResult),
            (
                TypeResult = inst_result_contains_types_unknown,
                NeedReplace = yes(InstResults0)
            ;
                TypeResult = inst_result_contains_types_known(TypeCtors),
                % XXX We could test the intersection of TypeCtors with
                % the key set of TypeEqvMap. That test would be more expensive,
                % but could possibly avoid a costly traversal of BoundInsts0.
                ( if set.is_empty(TypeCtors) then
                    NeedReplace = no
                else
                    NeedReplace = yes(inst_test_results(GroundnessResult,
                        AnyResult, InstNamesResult, InstVarsResult,
                        inst_result_contains_types_unknown, PropagatedResult))
                )
            )
        ;
            InstResults0 = inst_test_results_fgtc,
            NeedReplace = no
        ;
            InstResults0 = inst_test_no_results,
            NeedReplace = yes(InstResults0)
        ),
        (
            NeedReplace = no,
            Changed = no,
            Inst = Inst0
        ;
            NeedReplace = yes(InstResults),
            replace_in_bound_insts(TypeEqvMap, BoundInsts0, BoundInsts,
                Changed, !TVarSet, !Cache),
            % We could try to figure out the set of type_ctors in BoundInsts,
            % but that info may never be needed again.
            ( Changed = yes, Inst = bound(Uniq, InstResults, BoundInsts)
            ; Changed = no, Inst = Inst0
            )
        )
    ;
        Inst0 = ground(Uniq, higher_order(PredInstInfo0)),
        PredInstInfo0 = pred_inst_info(PorF, Modes0, MaybeArgRegs, Det),
        replace_in_modes(TypeEqvMap, Modes0, Modes, Changed, !TVarSet, !Cache),
        (
            Changed = yes,
            PredInstInfo = pred_inst_info(PorF, Modes, MaybeArgRegs, Det),
            Inst = ground(Uniq, higher_order(PredInstInfo))
        ;
            Changed = no,
            Inst = Inst0
        )
    ;
        Inst0 = constrained_inst_vars(Vars, CInst0),
        replace_in_inst(TypeEqvMap, CInst0, CInst, Changed, !TVarSet, !Cache),
        ( Changed = yes, Inst = constrained_inst_vars(Vars, CInst)
        ; Changed = no, Inst = Inst0
        )
    ;
        Inst0 = defined_inst(InstName0),
        replace_in_inst_name(TypeEqvMap, InstName0, InstName, Changed,
            !TVarSet, !Cache),
        ( Changed = yes, Inst = defined_inst(InstName)
        ; Changed = no, Inst = Inst0
        )
    ;
        Inst0 = abstract_inst(Name, ArgInsts0),
        replace_in_insts(TypeEqvMap, ArgInsts0, ArgInsts, Changed,
            !TVarSet, !Cache),
        ( Changed = yes, Inst = abstract_inst(Name, ArgInsts)
        ; Changed = no, Inst = Inst0
        )
    ).

:- pred replace_in_inst_name_no_tvarset(type_eqv_map::in,
    inst_name::in, inst_name::out, bool::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_inst_name_no_tvarset(TypeEqvMap, InstName0, InstName, Changed,
        !Cache) :-
    varset.init(TVarSet0),
    replace_in_inst_name(TypeEqvMap, InstName0, InstName, Changed,
        TVarSet0, _TVarSet, !Cache).

:- pred replace_in_inst_name(type_eqv_map::in, inst_name::in, inst_name::out,
    bool::out, tvarset::in, tvarset::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_inst_name(TypeEqvMap, InstName0, InstName, Changed,
        !TVarSet, !Cache) :-
    (
        InstName0 = user_inst(Name, Insts0),
        replace_in_insts(TypeEqvMap, Insts0, Insts, Changed, !TVarSet, !Cache),
        ( Changed = yes, InstName = user_inst(Name, Insts)
        ; Changed = no, InstName = InstName0
        )
    ;
        InstName0 = unify_inst(Live, Real, InstA0, InstB0),
        replace_in_inst(TypeEqvMap, InstA0, InstA, ChangedA, !TVarSet, !Cache),
        replace_in_inst(TypeEqvMap, InstB0, InstB, ChangedB, !TVarSet, !Cache),
        Changed = ChangedA `or` ChangedB,
        ( Changed = yes, InstName = unify_inst(Live, Real, InstA, InstB)
        ; Changed = no, InstName = InstName0
        )
    ;
        InstName0 = merge_inst(InstA0, InstB0),
        replace_in_inst(TypeEqvMap, InstA0, InstA, ChangedA, !TVarSet, !Cache),
        replace_in_inst(TypeEqvMap, InstB0, InstB, ChangedB, !TVarSet, !Cache),
        Changed = ChangedA `or` ChangedB,
        ( Changed = yes, InstName = merge_inst(InstA, InstB)
        ; Changed = no, InstName = InstName0
        )
    ;
        InstName0 = ground_inst(Name0, Live, Uniq, Real),
        replace_in_inst_name(TypeEqvMap, Name0, Name, Changed,
            !TVarSet, !Cache),
        ( Changed = yes, InstName = ground_inst(Name, Live, Uniq, Real)
        ; Changed = no, InstName = InstName0
        )
    ;
        InstName0 = any_inst(Name0, Live, Uniq, Real),
        replace_in_inst_name(TypeEqvMap, Name0, Name, Changed,
            !TVarSet, !Cache),
        ( Changed = yes, InstName = any_inst(Name, Live, Uniq, Real)
        ; Changed = no, InstName = InstName0
        )
    ;
        InstName0 = shared_inst(Name0),
        replace_in_inst_name(TypeEqvMap, Name0, Name, Changed,
            !TVarSet, !Cache),
        ( Changed = yes, InstName = shared_inst(Name)
        ; Changed = no, InstName = InstName0
        )
    ;
        InstName0 = mostly_uniq_inst(Name0),
        replace_in_inst_name(TypeEqvMap, Name0, Name, Changed,
            !TVarSet, !Cache),
        ( Changed = yes, InstName = mostly_uniq_inst(Name)
        ; Changed = no, InstName = InstName0
        )
    ;
        InstName0 = typed_ground(Uniq, Type0),
        replace_in_type(TypeEqvMap, Type0, Type, Changed, !TVarSet, no, _),
        ( Changed = yes, InstName = typed_ground(Uniq, Type)
        ; Changed = no, InstName = InstName0
        )
    ;
        InstName0 = typed_inst(Type0, Name0),
        replace_in_type(TypeEqvMap, Type0, Type, TypeChanged, !TVarSet, no, _),
        replace_in_inst_name(TypeEqvMap, Name0, Name, Changed0,
            !TVarSet, !Cache),
        Changed = TypeChanged `or` Changed0,
        ( Changed = yes, InstName = typed_inst(Type, Name)
        ; Changed = no, InstName = InstName0
        )
    ).

:- pred replace_in_bound_insts(type_eqv_map::in, list(bound_inst)::in,
    list(bound_inst)::out, bool::out, tvarset::in, tvarset::out,
    inst_cache::in, inst_cache::out) is det.

replace_in_bound_insts(_EqvMap, [], [], no, !TVarSet, !Cache).
replace_in_bound_insts(TypeEqvMap,
        List0 @ [bound_functor(ConsId, Insts0) | BoundInsts0],
        List, Changed, !TVarSet, !Cache) :-
    replace_in_insts(TypeEqvMap, Insts0, Insts, InstsChanged,
        !TVarSet, !Cache),
    replace_in_bound_insts(TypeEqvMap, BoundInsts0, BoundInsts,
        BoundInstsChanged, !TVarSet, !Cache),
    Changed = InstsChanged `or` BoundInstsChanged,
    ( Changed = yes, List = [bound_functor(ConsId, Insts) | BoundInsts]
    ; Changed = no, List = List0
    ).

:- pred replace_in_insts(type_eqv_map::in,
    list(mer_inst)::in, list(mer_inst)::out, bool::out,
    tvarset::in, tvarset::out, inst_cache::in, inst_cache::out) is det.

replace_in_insts(_EqvMap, [], [], no, !TVarSet, !Cache).
replace_in_insts(TypeEqvMap, List0 @ [Inst0 | Insts0], List, Changed,
        !TVarSet, !Cache) :-
    replace_in_inst(TypeEqvMap, Inst0, Inst, Changed0, !TVarSet, !Cache),
    replace_in_insts(TypeEqvMap, Insts0, Insts, Changed1, !TVarSet, !Cache),
    Changed = Changed0 `or` Changed1,
    ( Changed = yes, List = [Inst | Insts]
    ; Changed = no, List = List0
    ).

    % We hash-cons (actually map-cons) insts created by this pass
    % to avoid losing sharing.
    %
:- type inst_cache == map(mer_inst, mer_inst).

:- pred hash_cons_inst(mer_inst::in, mer_inst::out,
    inst_cache::in, inst_cache::out) is det.

hash_cons_inst(Inst0, Inst, !Cache) :-
    ( if map.search(!.Cache, Inst0, Inst1) then
        Inst = Inst1
    else
        Inst = Inst0,
        !:Cache = map.det_insert(!.Cache, Inst, Inst)
    ).

%-----------------------------------------------------------------------------%

:- type replace_info
    --->    replace_info(
                ethri_module_info   :: module_info,
                ethri_pred_info     :: pred_info,
                ethri_proc_info     :: proc_info,
                ethri_tvarset       :: tvarset,
                ethri_inst_cache    :: inst_cache,
                ethri_recompute     :: bool
            ).

:- pred replace_in_goal(type_eqv_map::in,
    hlds_goal::in, hlds_goal::out, bool::out,
    replace_info::in, replace_info::out) is det.

replace_in_goal(TypeEqvMap, Goal0, Goal, Changed, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    replace_in_goal_expr(TypeEqvMap, GoalExpr0, GoalExpr, Changed0, !Info),

    InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
    TVarSet0 = !.Info ^ ethri_tvarset,
    Cache0 = !.Info ^ ethri_inst_cache,
    instmap_delta_map_foldl(
        (pred(_::in, Inst0::in, Inst::out,
                {Changed1, TVarSet1, Cache1}::in,
                {Changed1 `or` InstChanged, TVarSet2, Cache2}::out) is det :-
            replace_in_inst(TypeEqvMap, Inst0, Inst, InstChanged,
                TVarSet1, TVarSet2, Cache1, Cache2)
        ), InstMapDelta0, InstMapDelta,
        {Changed0, TVarSet0, Cache0}, {Changed, TVarSet, Cache}),
    (
        Changed = yes,
        !Info ^ ethri_tvarset := TVarSet,
        !Info ^ ethri_inst_cache := Cache,
        goal_info_set_instmap_delta(InstMapDelta, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        Changed = no,
        Goal = Goal0
    ).

:- pred replace_in_goals(type_eqv_map::in,
    list(hlds_goal)::in, list(hlds_goal)::out, bool::out,
    replace_info::in, replace_info::out) is det.

replace_in_goals(_EqvMap, [], [], no, !Acc).
replace_in_goals(TypeEqvMap, List0 @ [H0 | T0], List, Changed, !Acc) :-
    replace_in_goals(TypeEqvMap, T0, T, Changed0, !Acc),
    replace_in_goal(TypeEqvMap, H0, H, Changed1, !Acc),
    Changed = Changed0 `or` Changed1,
    ( Changed = yes, List = [H | T]
    ; Changed = no, List = List0
    ).

:- pred replace_in_goal_expr(type_eqv_map::in,
    hlds_goal_expr::in, hlds_goal_expr::out, bool::out,
    replace_info::in, replace_info::out) is det.

replace_in_goal_expr(TypeEqvMap, GoalExpr0, GoalExpr, Changed, !Info) :-
    (
        GoalExpr0 = conj(ConjType, Goals0),
        replace_in_goals(TypeEqvMap, Goals0, Goals, Changed, !Info),
        ( Changed = yes, GoalExpr = conj(ConjType, Goals)
        ; Changed = no, GoalExpr = GoalExpr0
        )
    ;
        GoalExpr0 = disj(Goals0),
        replace_in_goals(TypeEqvMap, Goals0, Goals, Changed, !Info),
        ( Changed = yes, GoalExpr = disj(Goals)
        ; Changed = no, GoalExpr = GoalExpr0
        )
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        replace_in_cases(TypeEqvMap, Cases0, Cases, Changed, !Info),
        ( Changed = yes, GoalExpr = switch(Var, CanFail, Cases)
        ; Changed = no, GoalExpr = GoalExpr0
        )
    ;
        GoalExpr0 = negation(NegGoal0),
        replace_in_goal(TypeEqvMap, NegGoal0, NegGoal, Changed, !Info),
        ( Changed = yes, GoalExpr = negation(NegGoal)
        ; Changed = no, GoalExpr = GoalExpr0
        )
    ;
        GoalExpr0 = scope(Reason, SomeGoal0),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            % The code in modes.m sets the kind to from_ground_term_construct
            % only when SomeGoal0 does not have anything to expand.
            GoalExpr = GoalExpr0,
            Changed = no
        else
            replace_in_goal(TypeEqvMap, SomeGoal0, SomeGoal, Changed, !Info),
            ( Changed = yes, GoalExpr = scope(Reason, SomeGoal)
            ; Changed = no, GoalExpr = GoalExpr0
            )
        )
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        replace_in_goal(TypeEqvMap, Cond0, Cond, Changed1, !Info),
        replace_in_goal(TypeEqvMap, Then0, Then, Changed2, !Info),
        replace_in_goal(TypeEqvMap, Else0, Else, Changed3, !Info),
        Changed = Changed1 `or` Changed2 `or` Changed3,
        ( Changed = yes, GoalExpr = if_then_else(Vars, Cond, Then, Else)
        ; Changed = no, GoalExpr = GoalExpr0
        )
    ;
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        GoalExpr = GoalExpr0,
        Changed = no
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        TVarSet0 = !.Info ^ ethri_tvarset,
        replace_in_foreign_arg_list(TypeEqvMap, GoalExpr0 ^ foreign_args,
            Args, ChangedArgs, TVarSet0, TVarSet1, no, _),
        replace_in_foreign_arg_list(TypeEqvMap, GoalExpr0 ^ foreign_extra_args,
            ExtraArgs, ChangedExtraArgs, TVarSet1, TVarSet, no, _),
        Changed = ChangedArgs `or` ChangedExtraArgs,
        (
            Changed = yes,
            !Info ^ ethri_tvarset := TVarSet,
            GoalExpr = (GoalExpr0 ^ foreign_args := Args)
                ^ foreign_extra_args := ExtraArgs
        ;
            Changed = no,
            GoalExpr = GoalExpr0
        )
    ;
        GoalExpr0 = generic_call(Details, Args, Modes0, MaybeArgRegs, Detism),
        TVarSet0 = !.Info ^ ethri_tvarset,
        Cache0 = !.Info ^ ethri_inst_cache,
        replace_in_modes(TypeEqvMap, Modes0, Modes, Changed, TVarSet0, TVarSet,
            Cache0, Cache),
        (
            Changed = yes,
            !Info ^ ethri_tvarset := TVarSet,
            !Info ^ ethri_inst_cache := Cache,
            GoalExpr = generic_call(Details, Args, Modes, MaybeArgRegs, Detism)
        ;
            Changed = no,
            GoalExpr = GoalExpr0
        )
    ;
        GoalExpr0 = unify(Var, _, _, _, _),
        module_info_get_type_table(!.Info ^ ethri_module_info, TypeTable),
        proc_info_get_vartypes(!.Info ^ ethri_proc_info, VarTypes),
        proc_info_get_rtti_varmaps(!.Info ^ ethri_proc_info, RttiVarMaps),
        lookup_var_type(VarTypes, Var, VarType),
        TypeCtorCat = classify_type(!.Info ^ ethri_module_info, VarType),
        ( if
            % If this goal constructs a type_info for an equivalence type,
            % we need to expand that to make the type_info for the expanded
            % type. It is simpler to just recreate the type_info from scratch.

            GoalExpr0 ^ unify_kind = construct(_, ConsId, _, _, _, _, _),
            ConsId = type_info_cell_constructor(TypeCtor),
            TypeCtorCat = ctor_cat_system(cat_system_type_info),
            search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
            hlds_data.get_type_defn_body(TypeDefn, Body),
            Body = hlds_eqv_type(_)
        then
            Changed = yes,
            ModuleInfo0 = !.Info ^ ethri_module_info,
            PredInfo0 = !.Info ^ ethri_pred_info,
            ProcInfo0 = !.Info ^ ethri_proc_info,
            TVarSet0 = !.Info ^ ethri_tvarset,
            pred_info_set_typevarset(TVarSet0, PredInfo0, PredInfo1),
            create_poly_info(ModuleInfo0, PredInfo1, ProcInfo0, PolyInfo0),
            rtti_varmaps_var_info(RttiVarMaps, Var, VarInfo),
            (
                VarInfo = type_info_var(TypeInfoType0),
                TypeInfoType = TypeInfoType0
            ;
                ( VarInfo = typeclass_info_var(_)
                ; VarInfo = non_rtti_var
                ),
                unexpected($module, $pred, "info not found")
            ),
            polymorphism_make_type_info_var(TypeInfoType,
                term.context_init, TypeInfoVar, Goals0, PolyInfo0, PolyInfo),
            poly_info_extract(PolyInfo, PolySpecs, PredInfo1, PredInfo,
                ProcInfo0, ProcInfo, ModuleInfo),
            expect(unify(PolySpecs, []), $module, $pred,
                "got errors while making type_info_var"),
            pred_info_get_typevarset(PredInfo, TVarSet),
            !Info ^ ethri_module_info := ModuleInfo,
            !Info ^ ethri_pred_info := PredInfo,
            !Info ^ ethri_proc_info := ProcInfo,
            !Info ^ ethri_tvarset := TVarSet,

            rename_vars_in_goals(need_not_rename,
                map.from_assoc_list([TypeInfoVar - Var]), Goals0, Goals),
            ( if Goals = [hlds_goal(GoalExpr1, _)] then
                GoalExpr = GoalExpr1
            else
                GoalExpr = conj(plain_conj, Goals)
            ),
            !Info ^ ethri_recompute := yes
        else if
            % Check for a type_ctor_info for an equivalence type. We can just
            % remove these because after the code above to fix up type_infos
            % for equivalence types they can't be used.

            GoalExpr0 ^ unify_kind = construct(_, ConsId, _, _, _, _, _),
            ConsId = type_info_cell_constructor(TypeCtor),
            TypeCtorCat = ctor_cat_system(cat_system_type_ctor_info),
            search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
            hlds_data.get_type_defn_body(TypeDefn, Body),
            Body = hlds_eqv_type(_)
        then
            Changed = yes,
            GoalExpr = conj(plain_conj, []),
            !Info ^ ethri_recompute := yes
        else
            GoalExpr0 ^ unify_mode = unify_modes_lhs_rhs(LMode0, RMode0),
            TVarSet0 = !.Info ^ ethri_tvarset,
            Cache0 = !.Info ^ ethri_inst_cache,
            replace_in_from_to_insts(TypeEqvMap, LMode0, LMode, Changed1,
                TVarSet0, TVarSet1, Cache0, Cache1),
            replace_in_from_to_insts(TypeEqvMap, RMode0, RMode, Changed2,
                TVarSet1, TVarSet, Cache1, Cache),
            !Info ^ ethri_tvarset := TVarSet,
            !Info ^ ethri_inst_cache := Cache,
            replace_in_unification(TypeEqvMap,
                GoalExpr0 ^ unify_kind, Unification, Changed3, !Info),
            Changed = Changed1 `or` Changed2 `or` Changed3,
            (
                Changed = yes,
                GoalExpr1 = GoalExpr0 ^ unify_mode :=
                    unify_modes_lhs_rhs(LMode, RMode),
                GoalExpr = GoalExpr1 ^ unify_kind := Unification
            ;
                Changed = no,
                GoalExpr = GoalExpr0
            )
        )
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner,
                MaybeOutputVars, MainGoal0, OrElseGoals0, OrElseInners),
            replace_in_goal(TypeEqvMap, MainGoal0, MainGoal, Changed1, !Info),
            replace_in_goals(TypeEqvMap, OrElseGoals0, OrElseGoals, Changed2,
                !Info),
            Changed = Changed1 `or` Changed2,
            (
                Changed = yes,
                ShortHand = atomic_goal(GoalType, Outer, Inner,
                    MaybeOutputVars, MainGoal, OrElseGoals, OrElseInners),
                GoalExpr = shorthand(ShortHand)
            ;
                Changed = no,
                GoalExpr = GoalExpr0
            )
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            replace_in_goal(TypeEqvMap, SubGoal0, SubGoal, Changed, !Info),
            (
                Changed = yes,
                ShortHand = try_goal(MaybeIO, ResultVar, SubGoal),
                GoalExpr = shorthand(ShortHand)
            ;
                Changed = no,
                GoalExpr = GoalExpr0
            )
        ;
            ShortHand0 = bi_implication(_, _),
            unexpected($module, $pred, "bi_implication")
        )
    ).

:- pred replace_in_case(type_eqv_map::in, case::in, case::out, bool::out,
    replace_info::in, replace_info::out) is det.

replace_in_case(TypeEqvMap, Case0, Case, Changed, !Info) :-
    Case0 = case(MainConsId, OtherConsIds, CaseGoal0),
    replace_in_goal(TypeEqvMap, CaseGoal0, CaseGoal, Changed, !Info),
    ( Changed = yes, Case = case(MainConsId, OtherConsIds, CaseGoal)
    ; Changed = no, Case = Case0
    ).

:- pred replace_in_cases(type_eqv_map::in, list(case)::in, list(case)::out,
    bool::out, replace_info::in, replace_info::out) is det.

replace_in_cases(_EqvMap, [], [], no, !Acc).
replace_in_cases(TypeEqvMap, List0 @ [H0 | T0], List, Changed, !Acc) :-
    replace_in_cases(TypeEqvMap, T0, T, Changed0, !Acc),
    replace_in_case(TypeEqvMap, H0, H, Changed1, !Acc),
    Changed = Changed0 `or` Changed1,
    ( Changed = yes, List = [H | T]
    ; Changed = no, List = List0
    ).

:- pred replace_in_unification(type_eqv_map::in,
    unification::in, unification::out, bool::out,
    replace_info::in, replace_info::out) is det.

replace_in_unification(TypeEqvMap, Uni0, Uni, Changed, !Info) :-
    (
        ( Uni0 = assign(_, _)
        ; Uni0 = simple_test(_, _)
        ),
        Uni = Uni0,
        Changed = no
    ;
        Uni0 = complicated_unify(UnifyMode0, B, C),
        replace_in_unify_mode(TypeEqvMap, UnifyMode0, UnifyMode,
            Changed, !Info),
        ( Changed = yes, Uni = complicated_unify(UnifyMode, B, C)
        ; Changed = no, Uni = Uni0
        )
    ;
        Uni0 = construct(_, _, _, _, _, _, _),
        ArgModes0 = Uni0 ^ construct_arg_modes,
        replace_in_unify_modes(TypeEqvMap, ArgModes0, ArgModes,
            Changed, !Info),
        ( Changed = yes, Uni = Uni0 ^ construct_arg_modes := ArgModes
        ; Changed = no, Uni = Uni0
        )
    ;
        Uni0 = deconstruct(_, _, _, _, _, _),
        UnifyModes0 = Uni0 ^ deconstruct_arg_modes,
        replace_in_unify_modes(TypeEqvMap, UnifyModes0, UnifyModes,
            Changed, !Info),
        ( Changed = yes, Uni = Uni0 ^ deconstruct_arg_modes := UnifyModes
        ; Changed = no, Uni = Uni0
        )
    ).

%-----------------------------------------------------------------------------%

    % Replace equivalence types in a given type.
    % The bool output is `yes' if anything changed.
    %
:- pred replace_in_foreign_arg(type_eqv_map::in,
    foreign_arg::in, foreign_arg::out, bool::out, tvarset::in, tvarset::out,
    eqv_expanded_info::in, eqv_expanded_info::out) is det.

replace_in_foreign_arg(TypeEqvMap, Arg0, Arg, Changed, !VarSet, !Info) :-
    Arg0 = foreign_arg(Var, NameMode, Type0, BoxPolicy),
    replace_in_type(TypeEqvMap, Type0, Type, Changed, !VarSet, !Info),
    ( Changed = yes, Arg = foreign_arg(Var, NameMode, Type, BoxPolicy)
    ; Changed = no, Arg = Arg0
    ).

:- pred replace_in_foreign_arg_list(type_eqv_map::in,
    list(foreign_arg)::in, list(foreign_arg)::out, bool::out,
    tvarset::in, tvarset::out, eqv_expanded_info::in, eqv_expanded_info::out)
    is det.

replace_in_foreign_arg_list(_EqvMap, [], [], no, !VarSet, !Info).
replace_in_foreign_arg_list(TypeEqvMap, List0 @ [A0 | As0], List,
        Changed, !VarSet, !Info) :-
    replace_in_foreign_arg(TypeEqvMap, A0, A, Changed0, !VarSet, !Info),
    replace_in_foreign_arg_list(TypeEqvMap, As0, As, Changed1, !VarSet, !Info),
    Changed = Changed0 `or` Changed1,
    ( Changed = yes, List = [A | As]
    ; Changed = no, List = List0
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.equiv_type_hlds.
%-----------------------------------------------------------------------------%
