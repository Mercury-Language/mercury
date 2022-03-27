%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines an ADT that records the types of the variables
% in a predicate or procedure.
%
%---------------------------------------------------------------------------%

:- module hlds.vartypes.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

:- type vartypes.

:- pred init_vartypes(vartypes::out) is det.

:- pred vartypes_is_empty(vartypes::in) is semidet.

:- pred vartypes_count(vartypes::in, int::out) is det.

:- pred vartypes_select(set(prog_var)::in, vartypes::in, vartypes::out) is det.

:- pred vartypes_optimize(vartypes::in, vartypes::out) is det.

:- pred add_var_type(prog_var::in, mer_type::in,
    vartypes::in, vartypes::out) is det.
:- pred update_var_type(prog_var::in, mer_type::in,
    vartypes::in, vartypes::out) is det.

:- pred search_insert_var_type(prog_var::in, mer_type::in,
    maybe(mer_type)::out, vartypes::in, vartypes::out) is det.

:- pred is_in_vartypes(vartypes::in, prog_var::in) is semidet.

:- pred search_var_type(vartypes::in, prog_var::in, mer_type::out) is semidet.

:- func lookup_var_type_func(vartypes, prog_var) = mer_type.
:- pred lookup_var_type(vartypes::in, prog_var::in, mer_type::out) is det.
:- pred lookup_var_types(vartypes::in, list(prog_var)::in,
    list(mer_type)::out) is det.

:- pred vartypes_vars(vartypes::in, list(prog_var)::out) is det.
:- pred vartypes_types(vartypes::in, list(mer_type)::out) is det.

:- pred vartypes_to_sorted_assoc_list(vartypes::in,
    assoc_list(prog_var, mer_type)::out) is det.

:- pred vartypes_from_corresponding_lists(list(prog_var)::in,
    list(mer_type)::in, vartypes::out) is det.

:- pred vartypes_from_sorted_assoc_list(assoc_list(prog_var, mer_type)::in,
    vartypes::out) is det.

:- pred vartypes_add_corresponding_lists(list(prog_var)::in,
    list(mer_type)::in, vartypes::in, vartypes::out) is det.

:- pred delete_var_type(prog_var::in,
    vartypes::in, vartypes::out) is det.
:- pred delete_var_types(list(prog_var)::in,
    vartypes::in, vartypes::out) is det.
:- pred delete_sorted_var_types(list(prog_var)::in,
    vartypes::in, vartypes::out) is det.

:- pred apply_variable_renaming_to_vartypes(tvar_renaming::in,
    vartypes::in, vartypes::out) is det.

:- pred apply_subst_to_vartypes(tsubst::in, vartypes::in, vartypes::out)
    is det.

:- pred apply_rec_subst_to_vartypes(tsubst::in, vartypes::in, vartypes::out)
    is det.

:- pred transform_foldl_var_types(
    pred(mer_type, mer_type, T, T)::in(pred(in, out, in, out) is det),
    vartypes::in, vartypes::out, T::in, T::out) is det.

:- pred foldl_var_types(pred(mer_type, T, T)::in(pred(in, in, out) is det),
    vartypes::in, T::in, T::out) is det.

:- type prog_var_set_types
    --->    prog_var_set_types(prog_varset, vartypes).

:- type maybe_vartypes
    --->    varset_vartypes(tvarset, vartypes)
    ;       no_varset_vartypes.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_type_subst.

:- import_module map.

:- type vartypes == map(prog_var, mer_type).

%---------------------------------------------------------------------------%

init_vartypes(VarTypes) :-
    map.init(VarTypes).

vartypes_is_empty(VarTypes) :-
    map.is_empty(VarTypes).

vartypes_count(VarTypes, Count) :-
    map.count(VarTypes, Count).

vartypes_select(SelectedVars, !VarTypes) :-
    map.select(!.VarTypes, SelectedVars, !:VarTypes).

vartypes_optimize(!VarTypes) :-
    map.optimize(!VarTypes).

add_var_type(Var, Type, !VarTypes) :-
    map.det_insert(Var, Type, !VarTypes).

update_var_type(Var, Type, !VarTypes) :-
    map.det_update(Var, Type, !VarTypes).

search_insert_var_type(Var, NewType, MaybeOldType, !VarTypes) :-
    map.search_insert(Var, NewType, MaybeOldType, !VarTypes).

is_in_vartypes(VarTypes, Var) :-
    map.contains(VarTypes, Var).

search_var_type(VarTypes, Var, Type) :-
    map.search(VarTypes, Var, Type).

lookup_var_type_func(VarTypes, Var) = Type :-
    lookup_var_type(VarTypes, Var, Type).

lookup_var_type(VarTypes, Var, Type) :-
    map.lookup(VarTypes, Var, Type).

lookup_var_types(_VarTypes, [], []).
lookup_var_types(VarTypes, [Var | Vars], [Type | Types]) :-
    lookup_var_type(VarTypes, Var, Type),
    lookup_var_types(VarTypes, Vars, Types).

vartypes_vars(VarTypes, Vars) :-
    map.keys(VarTypes, Vars).

vartypes_types(VarTypes, Types) :-
    map.values(VarTypes, Types).

vartypes_to_sorted_assoc_list(VarTypes, AssocList) :-
    map.to_sorted_assoc_list(VarTypes, AssocList).

vartypes_from_corresponding_lists(Vars, Types, VarTypes) :-
    map.from_corresponding_lists(Vars, Types, VarTypes).

vartypes_from_sorted_assoc_list(AssocList, VarTypes) :-
    map.from_sorted_assoc_list(AssocList, VarTypes).

vartypes_add_corresponding_lists(Vars, Types, !VarTypes) :-
    map.det_insert_from_corresponding_lists(Vars, Types, !VarTypes).

delete_var_type(Var, !VarTypes) :-
    map.delete(Var, !VarTypes).

delete_var_types(Vars, !VarTypes) :-
    map.delete_list(Vars, !VarTypes).

delete_sorted_var_types(SortedVars, !VarTypes) :-
    map.delete_sorted_list(SortedVars, !VarTypes).

apply_variable_renaming_to_vartypes(Renaming, !VarTypes) :-
    transform_var_types(apply_variable_renaming_to_type(Renaming), !VarTypes).

apply_subst_to_vartypes(Subst, !VarTypes) :-
    transform_var_types(apply_subst_to_type(Subst), !VarTypes).

apply_rec_subst_to_vartypes(Subst, !VarTypes) :-
    transform_var_types(apply_rec_subst_to_type(Subst), !VarTypes).

:- pred transform_var_types(pred(mer_type, mer_type)::in(pred(in, out) is det),
    vartypes::in, vartypes::out) is det.

transform_var_types(Transform, !VarTypes) :-
    map.map_values_only(Transform, !VarTypes).

transform_foldl_var_types(Transform, !VarTypes, !Acc) :-
    map.map_values_foldl(Transform, !VarTypes, !Acc).

foldl_var_types(Pred, VarTypes, !Acc) :-
    map.foldl_values(Pred, VarTypes, !Acc).

%---------------------------------------------------------------------------%
:- end_module hlds.vartypes.
%---------------------------------------------------------------------------%
