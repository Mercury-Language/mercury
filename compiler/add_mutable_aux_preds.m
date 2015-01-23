%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_mutable_aux_preds.
:- interface.

:- import_module hlds.hlds_module.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Add predmode declarations for the four primitive operations.
    %
:- pred add_mutable_unsafe_get_pred_decl(module_name::in, string::in,
    mer_type::in, mer_inst::in, item_status::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred add_mutable_unsafe_set_pred_decl(module_name::in, string::in,
    mer_type::in, mer_inst::in, item_status::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred add_mutable_lock_pred_decl(module_name::in, string::in,
    item_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.
:- pred add_mutable_unlock_pred_decl(module_name::in, string::in,
    item_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Add a predmode declaration for the mutable initialisation predicate.
    %
:- pred add_mutable_init_pred_decl(module_name::in, string::in,
    item_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Add a predmode declaration for the mutable pre-initialisation
    % predicate. For normal mutables, this initialises the mutex protecting
    % the mutable. For thread-local mutables, this allocates an index
    % into an array of thread-local mutable values.
    %
:- pred add_mutable_pre_init_pred_decl(module_name::in, string::in,
    item_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

:- pred add_pred_decl_info_for_mutable_aux_pred(item_pred_decl_info::in,
    module_name::in, string::in, mutable_pred_kind::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_mutable.

:- import_module varset.
:- import_module require.

%-----------------------------------------------------------------------------%

add_mutable_unsafe_get_pred_decl(ModuleName, Name, Type, Inst, Status, Context,
        !ModuleInfo, !Specs) :-
    PredName = mutable_unsafe_get_pred_sym_name(ModuleName, Name),
    ArgTypesAndModes = [type_and_mode(Type, out_mode(Inst))],
    add_mutable_aux_pred_decl(ModuleName, Name, mutable_pred_unsafe_get,
        PredName, ArgTypesAndModes, purity_semipure, Status, Context,
        !ModuleInfo, !Specs).

add_mutable_unsafe_set_pred_decl(ModuleName, Name, Type, Inst, Status, Context,
        !ModuleInfo, !Specs) :-
    PredName = mutable_unsafe_set_pred_sym_name(ModuleName, Name),
    ArgTypesAndModes = [type_and_mode(Type, in_mode(Inst))],
    add_mutable_aux_pred_decl(ModuleName, Name, mutable_pred_unsafe_set,
        PredName, ArgTypesAndModes, purity_impure, Status, Context,
        !ModuleInfo, !Specs).

add_mutable_lock_pred_decl(ModuleName, Name, Status, Context,
        !ModuleInfo, !Specs) :-
    PredName = mutable_lock_pred_sym_name(ModuleName, Name),
    ArgTypesAndModes = [],
    add_mutable_aux_pred_decl(ModuleName, Name, mutable_pred_lock,
        PredName, ArgTypesAndModes, purity_impure, Status, Context,
        !ModuleInfo, !Specs).

add_mutable_unlock_pred_decl(ModuleName, Name, Status, Context,
        !ModuleInfo, !Specs) :-
    PredName = mutable_unlock_pred_sym_name(ModuleName, Name),
    ArgTypesAndModes = [],
    add_mutable_aux_pred_decl(ModuleName, Name, mutable_pred_unlock,
        PredName, ArgTypesAndModes, purity_impure, Status, Context,
        !ModuleInfo, !Specs).

add_mutable_init_pred_decl(ModuleName, Name, Status, Context,
        !ModuleInfo, !Specs) :-
    PredName = mutable_init_pred_sym_name(ModuleName, Name),
    ArgTypesAndModes = [],
    add_mutable_aux_pred_decl(ModuleName, Name, mutable_pred_pre_init,
        PredName, ArgTypesAndModes, purity_impure, Status, Context,
        !ModuleInfo, !Specs).

add_mutable_pre_init_pred_decl(ModuleName, Name, Status, Context,
        !ModuleInfo, !Specs) :-
    PredName = mutable_pre_init_pred_sym_name(ModuleName, Name),
    ArgTypesAndModes = [],
    add_mutable_aux_pred_decl(ModuleName, Name, mutable_pred_pre_init,
        PredName, ArgTypesAndModes, purity_impure, Status, Context,
        !ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%

:- pred add_mutable_aux_pred_decl(module_name::in, string::in,
    mutable_pred_kind::in, sym_name::in, list(type_and_mode)::in, purity::in,
    item_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_mutable_aux_pred_decl(ModuleName, Name, Kind, PredName, ArgTypesAndModes,
        Purity, Status, Context, !ModuleInfo, !Specs) :-
    PredOrigin = origin_mutable(ModuleName, Name, Kind),
    TypeVarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    marker_list_to_markers([marker_mutable_access_pred], Markers),
    module_add_pred_or_func(PredOrigin, TypeVarSet, InstVarSet, ExistQVars,
        pf_predicate, PredName, ArgTypesAndModes, yes(detism_det),
        Purity, Constraints, Markers, Context, Status, _, !ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%

add_pred_decl_info_for_mutable_aux_pred(ItemPredDecl, ModuleName, Name, Kind,
        Status, !ModuleInfo, !Specs) :-
    PredOrigin = origin_mutable(ModuleName, Name, Kind),
    ItemPredDecl = item_pred_decl_info(_Origin, TypeVarSet, InstVarSet,
        ExistQVars, PredOrFunc, PredName, TypesAndModes, WithType, WithInst,
        MaybeDet, _Cond, Purity, Constraints, Context, _SeqNum),
    expect(unify(TypeVarSet, varset.init), $module, $pred,
        "TypeVarSet != varset.init"),
    expect(unify(InstVarSet, varset.init), $module, $pred,
        "InstVarSet != varset.init"),
    expect(unify(ExistQVars, []), $module, $pred, "ExistQVars != []"),
    expect(unify(PredOrFunc, pf_predicate), $module, $pred,
        "PredOrFunc != pf_predicate"),
    expect(unify(WithType, no), $module, $pred, "WithType != no"),
    expect(unify(WithInst, no), $module, $pred, "WithInst != no"),
    expect(unify(MaybeDet, yes(detism_det)), $module, $pred,
        "MaybeDet != yes(detism_det)"),
    expect(unify(Constraints, constraints([], [])), $module, $pred,
        "Constraints != constraints([], [])"),
    marker_list_to_markers([marker_mutable_access_pred], Markers),
    module_add_pred_or_func(PredOrigin, TypeVarSet, InstVarSet, ExistQVars,
        PredOrFunc, PredName, TypesAndModes, MaybeDet, Purity, Constraints,
        Markers, Context, Status, _, !ModuleInfo, !Specs).

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_mutable_aux_preds.
%-----------------------------------------------------------------------------%
