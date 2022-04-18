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

:- module parse_tree.var_table.
:- interface.

:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.vartypes.

:- import_module assoc_list.
:- import_module counter.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

:- type var_table.

:- type var_table_map == map(prog_var, var_table_entry).
:- type var_table_entry
    --->    vte(
                vte_name        :: string,
                vte_type        :: mer_type,
                vte_is_dummy    :: is_dummy_type
            ).

    % These predicates are exported for use by hlds_pred.m. No other module
    % should call them.
    %
:- pred construct_var_table(counter::in, var_table_map::in,
    var_table::out) is det.
:- pred deconstruct_var_table(var_table::in,
    counter::out, var_table_map::out) is det.

:- pred init_var_table(var_table::out) is det.

:- pred var_table_map_to_var_table(var_table_map::in, var_table::out) is det.

:- pred var_table_is_empty(var_table::in) is semidet.

:- pred var_table_count(var_table::in, int::out) is det.

:- pred var_table_max_var_num(var_table::in, int::out) is det.

:- pred var_table_select(set(prog_var)::in,
    var_table::in, var_table::out) is det.

:- pred var_table_optimize(var_table::in, var_table::out) is det.

:- pred add_var_entry(var_table_entry::in, prog_var::out,
    var_table::in, var_table::out) is det.

:- pred add_prefix_number_var_entry(string::in, mer_type::in,
    is_dummy_type::in, prog_var::out, var_table::in, var_table::out) is det.

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

:- func lookup_var_type_func(var_table, prog_var) = mer_type.
:- pred lookup_var_type(var_table::in, prog_var::in, mer_type::out) is det.
:- pred lookup_var_types(var_table::in, list(prog_var)::in,
    list(mer_type)::out) is det.

:- func var_entry_name(prog_var, var_table_entry) = string.
:- func var_entry_name_default(prog_var, var_table_entry, string) = string.
:- func var_entry_name_and_number(prog_var, var_table_entry) = string.
:- func var_entry_name_and_number_default(prog_var, var_table_entry, string)
    = string.
:- func var_table_entry_name(var_table, prog_var) = string.
:- func var_table_entry_name_default(var_table, prog_var, string) = string.
:- func var_table_entry_name_and_number(var_table, prog_var) = string.
:- func var_table_entry_name_and_number_default(var_table, prog_var, string)
    = string.

:- pred var_table_vars(var_table::in, list(prog_var)::out) is det.
:- pred var_table_entries(var_table::in, list(var_table_entry)::out) is det.

:- pred var_table_to_sorted_assoc_list(var_table::in,
    assoc_list(prog_var, var_table_entry)::out) is det.

:- pred var_table_from_corresponding_lists(list(prog_var)::in,
    list(var_table_entry)::in, var_table::out) is det.

:- pred var_table_from_sorted_assoc_list(
    assoc_list(prog_var, var_table_entry)::in, var_table::out) is det.
:- pred var_table_from_rev_sorted_assoc_list(
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

%---------------------------------------------------------------------------%

    % Transitional type for compiler passes that need only type information.
    %
:- type var_type_source
    --->    vts_vartypes(vartypes)
    ;       vts_var_table(var_table).

:- pred lookup_var_type_in_source(var_type_source::in, prog_var::in,
    mer_type::out) is det.
:- pred lookup_var_types_in_source(var_type_source::in, list(prog_var)::in,
    list(mer_type)::out) is det.

%---------------------------------------------------------------------------%

    % Transitional type for compiler passes that need only name information.
    %
:- type var_name_source
    --->    vns_varset(prog_varset)
    ;       vns_var_table(var_table).

:- pred search_var_name_in_source(var_name_source::in, prog_var::in,
    string::out) is semidet.
:- pred lookup_var_name_in_source(var_name_source::in, prog_var::in,
    string::out) is det.

%---------------------------------------------------------------------------%

    % Transitional type for compiler passes that need both name and
    % type information.
    %
:- type var_db
    --->    var_db_varset_vartypes(prog_var_set_types)
    ;       var_db_var_table(var_table).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_type_subst.

:- import_module int.
:- import_module pair.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type var_table
    --->    var_table(
                vt_counter  :: counter,
                vt_map      :: var_table_map
            ).

construct_var_table(Counter, Map, var_table(Counter, Map)).
deconstruct_var_table(var_table(Counter, Map), Counter, Map).

%---------------------------------------------------------------------------%

init_var_table(VarTable) :-
    counter.init(1, Counter),
    map.init(VarTableMap),
    VarTable = var_table(Counter, VarTableMap).

%---------------------------------------------------------------------------%

var_table_map_to_var_table(VarTableMap, VarTable) :-
    ( if map.max_key(VarTableMap) = MaxVar then
        NextAllocVarNum = var_to_int(MaxVar) + 1
    else
        NextAllocVarNum = 1
    ),
    counter.init(NextAllocVarNum, Counter),
    VarTable = var_table(Counter, VarTableMap).

%---------------------------------------------------------------------------%

var_table_is_empty(VarTable) :-
    map.is_empty(VarTable ^ vt_map).

var_table_count(VarTable, Count) :-
    map.count(VarTable ^ vt_map, Count).

var_table_max_var_num(VarTable, MaxVarNum) :-
    counter.allocate(NextVarNum, VarTable ^ vt_counter, _),
    MaxVarNum = NextVarNum - 1.

%---------------------------------------------------------------------------%

var_table_select(SelectedVars, !VarTable) :-
    !.VarTable = var_table(Counter, VarTableMap0),
    map.select(VarTableMap0, SelectedVars, VarTableMap),
    !:VarTable = var_table(Counter, VarTableMap).

var_table_optimize(!VarTable) :-
    !.VarTable = var_table(Counter, VarTableMap0),
    map.optimize(VarTableMap0, VarTableMap),
    !:VarTable = var_table(Counter, VarTableMap).

%---------------------------------------------------------------------------%

add_var_entry(Entry, Var, !VarTable) :-
    !.VarTable = var_table(Counter0, VarTableMap0),
    counter.allocate(VarNum, Counter0, Counter),
    Var = force_construct_var(VarNum),
    map.det_insert(Var, Entry, VarTableMap0, VarTableMap),
    !:VarTable = var_table(Counter, VarTableMap).

add_prefix_number_var_entry(Prefix, Type, IsDummy, Var, !VarTable) :-
    !.VarTable = var_table(Counter0, VarTableMap0),
    counter.allocate(VarNum, Counter0, Counter),
    Var = force_construct_var(VarNum),
    string.format("%s_%d", [s(Prefix), i(VarNum)], Name),
    Entry = vte(Name, Type, IsDummy),
    map.det_insert(Var, Entry, VarTableMap0, VarTableMap),
    !:VarTable = var_table(Counter, VarTableMap).

update_var_entry(Var, Entry, !VarTable) :-
    !.VarTable = var_table(Counter, VarTableMap0),
    map.det_update(Var, Entry, VarTableMap0, VarTableMap),
    !:VarTable = var_table(Counter, VarTableMap).

search_insert_var_entry(Var, NewEntry, MaybeOldEntry, !VarTable) :-
    !.VarTable = var_table(Counter, VarTableMap0),
    map.search_insert(Var, NewEntry, MaybeOldEntry,
        VarTableMap0, VarTableMap),
    !:VarTable = var_table(Counter, VarTableMap).

%---------------------------------------------------------------------------%

is_in_var_table(VarTable, Var) :-
    map.contains(VarTable ^ vt_map, Var).

search_var_entry(VarTable, Var, Entry) :-
    map.search(VarTable ^ vt_map, Var, Entry).

lookup_var_entry_func(VarTable, Var) = Entry :-
    lookup_var_entry(VarTable, Var, Entry).

lookup_var_entry(VarTable, Var, Entry) :-
    map.lookup(VarTable ^ vt_map, Var, Entry).

lookup_var_entries(_VarTable, [], []).
lookup_var_entries(VarTable, [Var | Vars], [Entry | Entries]) :-
    lookup_var_entry(VarTable, Var, Entry),
    lookup_var_entries(VarTable, Vars, Entries).

lookup_var_type_func(VarTable, Var) = Type :-
    lookup_var_type(VarTable, Var, Type).

lookup_var_type(VarTable, Var, Type) :-
    lookup_var_entry(VarTable, Var, Entry),
    Type = Entry ^ vte_type.

lookup_var_types(_VarTable, [], []).
lookup_var_types(VarTable, [Var | Vars], [Type | Types]) :-
    lookup_var_type(VarTable, Var, Type),
    lookup_var_types(VarTable, Vars, Types).

%---------------------------------------------------------------------------%

var_entry_name(Var, Entry) = Name :-
    Name0 = Entry ^ vte_name,
    ( if Name0 = "" then
        term.var_to_int(Var, VarNum),
        string.format("V_%d", [i(VarNum)], Name)
    else
        Name = Name0
    ).

var_entry_name_default(Var, Entry, DefaultPrefix) = Name :-
    Name0 = Entry ^ vte_name,
    ( if Name0 = "" then
        term.var_to_int(Var, VarNum),
        string.format("%s_%d", [s(DefaultPrefix), i(VarNum)], Name)
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

var_entry_name_and_number_default(Var, Entry, DefaultPrefix) = Name :-
    Name0 = Entry ^ vte_name,
    term.var_to_int(Var, VarNum),
    ( if Name0 = "" then
        string.format("%s_%d", [s(DefaultPrefix), i(VarNum)], Name)
    else
        string.format("%s_%d", [s(Name0), i(VarNum)], Name)
    ).

%---------------------%

var_table_entry_name(VarTable, Var) = Name :-
    lookup_var_entry(VarTable, Var, Entry),
    Name = var_entry_name(Var, Entry).

var_table_entry_name_default(VarTable, Var, DefaultPrefix) = Name :-
    lookup_var_entry(VarTable, Var, Entry),
    Name = var_entry_name_default(Var, Entry, DefaultPrefix).

var_table_entry_name_and_number(VarTable, Var) = Name :-
    lookup_var_entry(VarTable, Var, Entry),
    Name = var_entry_name_and_number(Var, Entry).

var_table_entry_name_and_number_default(VarTable, Var, DefaultPrefix) = Name :-
    lookup_var_entry(VarTable, Var, Entry),
    Name = var_entry_name_and_number_default(Var, Entry, DefaultPrefix).

%---------------------------------------------------------------------------%

var_table_vars(VarTable, Vars) :-
    map.keys(VarTable ^ vt_map, Vars).

var_table_entries(VarTable, Entries) :-
    map.values(VarTable ^ vt_map, Entries).

%---------------------------------------------------------------------------%

var_table_to_sorted_assoc_list(VarTable, AssocList) :-
    map.to_sorted_assoc_list(VarTable ^ vt_map, AssocList).

var_table_from_corresponding_lists(Vars, Types, VarTable) :-
    map.from_corresponding_lists(Vars, Types, VarTableMap),
    var_table_map_to_var_table(VarTableMap, VarTable).

var_table_from_sorted_assoc_list(AssocList, VarTable) :-
    map.from_sorted_assoc_list(AssocList, VarTableMap),
    var_table_map_to_var_table(VarTableMap, VarTable).

var_table_from_rev_sorted_assoc_list(RevAssocList, VarTable) :-
    map.from_rev_sorted_assoc_list(RevAssocList, VarTableMap),
    var_table_map_to_var_table(VarTableMap, VarTable).

var_table_add_corresponding_lists(Vars, Entries, !VarTable) :-
    !.VarTable = var_table(Counter, VarTableMap0),
    map.det_insert_from_corresponding_lists(Vars, Entries,
        VarTableMap0, VarTableMap),
    !:VarTable = var_table(Counter, VarTableMap).

%---------------------------------------------------------------------------%

delete_var_entry(Var, !VarTable) :-
    !.VarTable = var_table(Counter, VarTableMap0),
    map.delete(Var, VarTableMap0, VarTableMap),
    !:VarTable = var_table(Counter, VarTableMap).

delete_var_entries(Vars, !VarTable) :-
    !.VarTable = var_table(Counter, VarTableMap0),
    map.delete_list(Vars, VarTableMap0, VarTableMap),
    !:VarTable = var_table(Counter, VarTableMap).

delete_sorted_var_entries(SortedVars, !VarTable) :-
    !.VarTable = var_table(Counter, VarTableMap0),
    map.delete_sorted_list(SortedVars, VarTableMap0, VarTableMap),
    !:VarTable = var_table(Counter, VarTableMap).

%---------------------------------------------------------------------------%

apply_variable_renaming_to_var_table(Renaming, !VarTable) :-
    transform_var_table(apply_variable_renaming_to_type_in_vte(Renaming),
        !VarTable).

:- pred apply_variable_renaming_to_type_in_vte(tvar_renaming::in,
    var_table_entry::in, var_table_entry::out) is det.

apply_variable_renaming_to_type_in_vte(Renaming, Entry0, Entry) :-
    Type0 = Entry0 ^ vte_type,
    apply_variable_renaming_to_type(Renaming, Type0, Type),
    Entry = Entry0 ^ vte_type := Type.

%---------------------%

apply_subst_to_var_table(Subst, !VarTable) :-
    transform_var_table(apply_subst_to_type_in_vte(Subst), !VarTable).

:- pred apply_subst_to_type_in_vte(tsubst::in,
    var_table_entry::in, var_table_entry::out) is det.

apply_subst_to_type_in_vte(Subst, Entry0, Entry) :-
    Type0 = Entry0 ^ vte_type,
    apply_subst_to_type(Subst, Type0, Type),
    Entry = Entry0 ^ vte_type := Type.

%---------------------%

apply_rec_subst_to_var_table(Subst, !VarTable) :-
    transform_var_table(apply_rec_subst_to_type_in_vte(Subst), !VarTable).

:- pred apply_rec_subst_to_type_in_vte(tsubst::in,
    var_table_entry::in, var_table_entry::out) is det.

apply_rec_subst_to_type_in_vte(Subst, Entry0, Entry) :-
    Type0 = Entry0 ^ vte_type,
    apply_rec_subst_to_type(Subst, Type0, Type),
    Entry = Entry0 ^ vte_type := Type.

%---------------------%

:- pred transform_var_table(
    pred(var_table_entry, var_table_entry)::in(pred(in, out) is det),
    var_table::in, var_table::out) is det.

transform_var_table(Transform, !VarTable) :-
    !.VarTable = var_table(Counter, VarTableMap0),
    map.map_values_only(Transform, VarTableMap0, VarTableMap),
    !:VarTable = var_table(Counter, VarTableMap).

%---------------------------------------------------------------------------%

transform_foldl_var_table(Transform, !VarTable, !Acc) :-
    !.VarTable = var_table(Counter, VarTableMap0),
    map.map_values_foldl(Transform, VarTableMap0, VarTableMap, !Acc),
    !:VarTable = var_table(Counter, VarTableMap).

foldl_var_table(Pred, VarTable, !Acc) :-
    map.foldl_values(Pred, VarTable ^ vt_map, !Acc).

%---------------------------------------------------------------------------%

lookup_var_type_in_source(VarTypeSrc, Var, Type) :-
    (
        VarTypeSrc = vts_vartypes(VarTypes),
        vartypes.lookup_var_type(VarTypes, Var, Type)
    ;
        VarTypeSrc = vts_var_table(VarTable),
        var_table.lookup_var_type(VarTable, Var, Type)
    ).

lookup_var_types_in_source(VarTypeSrc, Vars, Types) :-
    (
        VarTypeSrc = vts_vartypes(VarTypes),
        vartypes.lookup_var_types(VarTypes, Vars, Types)
    ;
        VarTypeSrc = vts_var_table(VarTable),
        var_table.lookup_var_types(VarTable, Vars, Types)
    ).

%---------------------%

search_var_name_in_source(VarNameSrc, Var, Name) :-
    (
        VarNameSrc = vns_varset(VarSet),
        varset.search_name(VarSet, Var, Name)
    ;
        VarNameSrc = vns_var_table(VarTable),
        var_table.lookup_var_entry(VarTable, Var, Entry),
        Name = Entry ^ vte_name,
        Name \= ""
    ).

lookup_var_name_in_source(VarNameSrc, Var, Name) :-
    (
        VarNameSrc = vns_varset(VarSet),
        varset.lookup_name(VarSet, Var, Name)
    ;
        VarNameSrc = vns_var_table(VarTable),
        var_table.lookup_var_entry(VarTable, Var, Entry),
        Name = var_entry_name(Var, Entry)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.var_table.
%---------------------------------------------------------------------------%
