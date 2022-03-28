%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines an ADT that records for each variable in a pred_info
% or proc_info
%
% - the variable's name, if any;
% - the variable's type, and
% - whether that type is a dummy type.
%
%---------------------------------------------------------------------------%

:- module hlds.var_table.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module list.
:- import_module maybe.
:- import_module set.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type var_table.

:- type var_table_entry
    --->    vte(
                vte_name        :: string,
                vte_type        :: mer_type,
                vte_is_dummy    :: is_dummy_type
            ).

:- pred make_var_table(module_info::in, prog_varset::in, vartypes::in,
    var_table::out) is det.

:- pred init_var_table(var_table::out) is det.

:- pred var_table_is_empty(var_table::in) is semidet.

:- pred var_table_count(var_table::in, int::out) is det.

:- pred var_table_select(set(prog_var)::in,
    var_table::in, var_table::out) is det.

:- pred var_table_optimize(var_table::in, var_table::out) is det.

:- pred add_var_entry(prog_var::in, var_table_entry::in,
    var_table::in, var_table::out) is det.
:- pred update_var_entry(prog_var::in, var_table_entry::in,
    var_table::in, var_table::out) is det.

:- pred search_insert_var_entry(prog_var::in, var_table_entry::in,
    maybe(var_table_entry)::out, var_table::in, var_table::out) is det.

:- pred is_in_var_table(var_table::in, prog_var::in) is semidet.

:- pred search_var_entry(var_table::in, prog_var::in,
    var_table_entry::out) is semidet.

:- func lookup_var_entry_func(var_table, prog_var) = var_table_entry.
:- pred lookup_var_entry(var_table::in, prog_var::in,
    var_table_entry::out) is det.
:- pred lookup_var_entries(var_table::in, list(prog_var)::in,
    list(var_table_entry)::out) is det.

:- func var_entry_name(prog_var, var_table_entry) = string.
:- func var_entry_name_and_number(prog_var, var_table_entry) = string.
:- func var_table_entry_name(var_table, prog_var) = string.
:- func var_table_entry_name_and_number(var_table, prog_var) = string.

:- func lookup_var_type_func(var_table, prog_var) = mer_type.

:- pred var_table_vars(var_table::in, list(prog_var)::out) is det.
:- pred var_table_entries(var_table::in, list(var_table_entry)::out) is det.

:- pred var_table_to_sorted_assoc_list(var_table::in,
    assoc_list(prog_var, var_table_entry)::out) is det.

:- pred var_table_from_corresponding_lists(list(prog_var)::in,
    list(var_table_entry)::in, var_table::out) is det.

:- pred var_table_from_sorted_assoc_list(
    assoc_list(prog_var, var_table_entry)::in, var_table::out) is det.

:- pred var_table_add_corresponding_lists(list(prog_var)::in,
    list(var_table_entry)::in, var_table::in, var_table::out) is det.

:- pred delete_var_entry(prog_var::in,
    var_table::in, var_table::out) is det.
:- pred delete_var_entries(list(prog_var)::in,
    var_table::in, var_table::out) is det.
:- pred delete_sorted_var_entries(list(prog_var)::in,
    var_table::in, var_table::out) is det.

:- pred apply_variable_renaming_to_var_table(tvar_renaming::in,
    var_table::in, var_table::out) is det.

:- pred apply_subst_to_var_table(tsubst::in,
    var_table::in, var_table::out) is det.

:- pred apply_rec_subst_to_var_table(tsubst::in,
    var_table::in, var_table::out) is det.

:- pred transform_foldl_var_table(
    pred(var_table_entry, var_table_entry, T, T)::
        in(pred(in, out, in, out) is det),
    var_table::in, var_table::out, T::in, T::out) is det.

:- pred foldl_var_table(
    pred(var_table_entry, T, T)::in(pred(in, in, out) is det),
    var_table::in, T::in, T::out) is det.

:- type prog_var_set_types
    --->    prog_var_set_types(prog_varset, var_table).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module parse_tree.prog_type_subst.

:- import_module map.
:- import_module pair.
:- import_module string.
:- import_module term.

:- type var_table == map(prog_var, var_table_entry).

%---------------------------------------------------------------------------%

make_var_table(ModuleInfo, VarSet, VarTypes, VarTable) :-
    vartypes_to_sorted_assoc_list(VarTypes, VarTypesAL),
    make_var_table_loop(ModuleInfo, VarSet, VarTypesAL, [], RevVarTableAL),
    map.from_rev_sorted_assoc_list(RevVarTableAL, VarTable).

:- pred make_var_table_loop(module_info::in, prog_varset::in,
    assoc_list(prog_var, mer_type)::in,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::out) is det.

make_var_table_loop(_, _, [], !RevVarTableAL).
make_var_table_loop(ModuleInfo, VarSet, [Var - Type | VarsTypes],
        !RevVarTableAL) :-
    ( if varset.search_name(VarSet, Var, NamePrime) then
        Name = NamePrime
    else
        Name = ""
    ),
    IsDummy = is_type_a_dummy(ModuleInfo, Type),
    Entry = vte(Name, Type, IsDummy),
    !:RevVarTableAL = [Var - Entry | !.RevVarTableAL],
    make_var_table_loop(ModuleInfo, VarSet, VarsTypes, !RevVarTableAL).

init_var_table(VarTable) :-
    map.init(VarTable).

var_table_is_empty(VarTable) :-
    map.is_empty(VarTable).

var_table_count(VarTable, Count) :-
    map.count(VarTable, Count).

var_table_select(SelectedVars, !VarTable) :-
    map.select(!.VarTable, SelectedVars, !:VarTable).

var_table_optimize(!VarTable) :-
    map.optimize(!VarTable).

add_var_entry(Var, Entry, !VarTable) :-
    map.det_insert(Var, Entry, !VarTable).

update_var_entry(Var, Entry, !VarTable) :-
    map.det_update(Var, Entry, !VarTable).

search_insert_var_entry(Var, NewEntry, MaybeOldEntry, !VarTable) :-
    map.search_insert(Var, NewEntry, MaybeOldEntry, !VarTable).

is_in_var_table(VarTable, Var) :-
    map.contains(VarTable, Var).

search_var_entry(VarTable, Var, Entry) :-
    map.search(VarTable, Var, Entry).

lookup_var_entry_func(VarTable, Var) = Entry :-
    lookup_var_entry(VarTable, Var, Entry).

lookup_var_entry(VarTable, Var, Entry) :-
    map.lookup(VarTable, Var, Entry).

lookup_var_entries(_VarTable, [], []).
lookup_var_entries(VarTable, [Var | Vars], [Entry | Entries]) :-
    lookup_var_entry(VarTable, Var, Entry),
    lookup_var_entries(VarTable, Vars, Entries).

var_entry_name(Var, Entry) = Name :-
    Name0 = Entry ^ vte_name,
    ( if Name0 = "" then
        term.var_to_int(Var, VarNum),
        Name = "V_" ++ string.int_to_string(VarNum)
    else
        Name = Name0
    ).

var_entry_name_and_number(Var, Entry) = Name :-
    Name0 = Entry ^ vte_name,
    term.var_to_int(Var, VarNum),
    ( if Name0 = "" then
        string.format("V_%d", [i(VarNum)], Name)
    else
        string.format("%s_%d", [s(Name0), i(VarNum)], Name)
    ).

var_table_entry_name(VarTable, Var) = Name :-
    lookup_var_entry(VarTable, Var, Entry),
    Name = var_entry_name(Var, Entry).

var_table_entry_name_and_number(VarTable, Var) = Name :-
    lookup_var_entry(VarTable, Var, Entry),
    Name = var_entry_name_and_number(Var, Entry).

lookup_var_type_func(VarTable, Var) = Type :-
    lookup_var_entry(VarTable, Var, Entry),
    Type = Entry ^ vte_type.

var_table_vars(VarTable, Vars) :-
    map.keys(VarTable, Vars).

var_table_entries(VarTable, Entries) :-
    map.values(VarTable, Entries).

var_table_to_sorted_assoc_list(VarTable, AssocList) :-
    map.to_sorted_assoc_list(VarTable, AssocList).

var_table_from_corresponding_lists(Vars, Types, VarTable) :-
    map.from_corresponding_lists(Vars, Types, VarTable).

var_table_from_sorted_assoc_list(AssocList, VarTable) :-
    map.from_sorted_assoc_list(AssocList, VarTable).

var_table_add_corresponding_lists(Vars, Entries, !VarTable) :-
    map.det_insert_from_corresponding_lists(Vars, Entries, !VarTable).

delete_var_entry(Var, !VarTable) :-
    map.delete(Var, !VarTable).

delete_var_entries(Vars, !VarTable) :-
    map.delete_list(Vars, !VarTable).

delete_sorted_var_entries(SortedVars, !VarTable) :-
    map.delete_sorted_list(SortedVars, !VarTable).

apply_variable_renaming_to_var_table(Renaming, !VarTable) :-
    transform_var_table(apply_variable_renaming_to_type_in_vte(Renaming),
        !VarTable).

:- pred apply_variable_renaming_to_type_in_vte(tvar_renaming::in,
    var_table_entry::in, var_table_entry::out) is det.

apply_variable_renaming_to_type_in_vte(Renaming, Entry0, Entry) :-
    Type0 = Entry0 ^ vte_type,
    apply_variable_renaming_to_type(Renaming, Type0, Type),
    Entry = Entry0 ^ vte_type := Type.

apply_subst_to_var_table(Subst, !VarTable) :-
    transform_var_table(apply_subst_to_type_in_vte(Subst), !VarTable).

:- pred apply_subst_to_type_in_vte(tsubst::in,
    var_table_entry::in, var_table_entry::out) is det.

apply_subst_to_type_in_vte(Subst, Entry0, Entry) :-
    Type0 = Entry0 ^ vte_type,
    apply_subst_to_type(Subst, Type0, Type),
    Entry = Entry0 ^ vte_type := Type.

apply_rec_subst_to_var_table(Subst, !VarTable) :-
    transform_var_table(apply_rec_subst_to_type_in_vte(Subst), !VarTable).

:- pred apply_rec_subst_to_type_in_vte(tsubst::in,
    var_table_entry::in, var_table_entry::out) is det.

apply_rec_subst_to_type_in_vte(Subst, Entry0, Entry) :-
    Type0 = Entry0 ^ vte_type,
    apply_rec_subst_to_type(Subst, Type0, Type),
    Entry = Entry0 ^ vte_type := Type.

:- pred transform_var_table(
    pred(var_table_entry, var_table_entry)::in(pred(in, out) is det),
    var_table::in, var_table::out) is det.

transform_var_table(Transform, !VarTable) :-
    map.map_values_only(Transform, !VarTable).

transform_foldl_var_table(Transform, !VarTable, !Acc) :-
    map.map_values_foldl(Transform, !VarTable, !Acc).

foldl_var_table(Pred, VarTable, !Acc) :-
    map.foldl_values(Pred, VarTable, !Acc).

%---------------------------------------------------------------------------%
:- end_module hlds.var_table.
%---------------------------------------------------------------------------%
