%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: var_table_hlds.m.
% Main author: zs.
%
% This module defines operations on var_tables that require access
% to the HLDS. (The var_table type itself, and most other operations on it,
% are defined in var_table.m, which is in the parse_tree package, which
% does not have access to the HLDS.)
%
%---------------------------------------------------------------------------%

:- module hlds.var_table_hlds.

:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.vartypes.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module list.

%---------------------------------------------------------------------------%

    % Create a var_table from a varset/vartypes pair.
    %
:- pred make_var_table(module_info::in, prog_varset::in, vartypes::in,
    var_table::out) is det.

    % Split up a var_table into a varset/vartypes pair.
    %
:- pred split_var_table(var_table::in, prog_varset::out, vartypes::out) is det.

    % Create a var_table from a varset and a list of variables
    % with their types.
    %
:- pred vars_types_to_var_table(module_info::in, prog_varset::in,
    assoc_list(prog_var, mer_type)::in, var_table::out) is det.
:- pred corresponding_vars_types_to_var_table(module_info::in, prog_varset::in,
    list(prog_var)::in, list(mer_type)::in, var_table::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_type.

:- import_module counter.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

make_var_table(ModuleInfo, VarSet, VarTypes, VarTable) :-
    vartypes_to_sorted_assoc_list(VarTypes, VarTypesAL),
    make_var_table_loop(ModuleInfo, VarSet, 1, VarTypesAL,
        [], RevVarTableAL0),
    (
        RevVarTableAL0 = [],
        LastAllocVarNum0 = 0
    ;
        RevVarTableAL0 = [Var - _ | _],
        LastAllocVarNum0 = var_to_int(Var)
    ),
    MaxVarInVarSet = varset.max_var(VarSet),
    MaxVarNumInVarSet = var_to_int(MaxVarInVarSet),
    ( if MaxVarNumInVarSet > LastAllocVarNum0 then
        LastAllocVarNum = MaxVarNumInVarSet,
        extend_var_table_loop(VarSet, LastAllocVarNum0 + 1, LastAllocVarNum,
            RevVarTableAL0, RevVarTableAL)
    else
        LastAllocVarNum = LastAllocVarNum0,
        RevVarTableAL = RevVarTableAL0
    ),
    counter.init(LastAllocVarNum + 1, Counter),
    map.from_rev_sorted_assoc_list(RevVarTableAL, VarTableMap),
    construct_var_table(Counter, VarTableMap, VarTable).

:- pred make_var_table_loop(module_info::in, prog_varset::in, int::in,
    assoc_list(prog_var, mer_type)::in,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::out) is det.

make_var_table_loop(_, _, _, [], !RevVarTableAL).
make_var_table_loop(ModuleInfo, VarSet, CurVarNum, [Var - Type | VarsTypes0],
        !RevVarTableAL) :-
    VarNum = term.var_to_int(Var),
    ( if CurVarNum = VarNum then
        ( if varset.search_name(VarSet, Var, NamePrime) then
            Name = NamePrime
        else
            Name = ""
        ),
        IsDummy = is_type_a_dummy(ModuleInfo, Type),
        Entry = vte(Name, Type, IsDummy),
        !:RevVarTableAL = [Var - Entry | !.RevVarTableAL],
        VarsTypes = VarsTypes0
    else if CurVarNum < VarNum then
        record_untyped_var(VarSet, CurVarNum, !RevVarTableAL),
        % We did not Process Var in this iteration.
        VarsTypes = [Var - Type | VarsTypes0]
    else
        unexpected($pred, "CurVarNum > VarNum")
    ),
    make_var_table_loop(ModuleInfo, VarSet, CurVarNum + 1,
        VarsTypes, !RevVarTableAL).

:- pred extend_var_table_loop(prog_varset::in, int::in, int::in,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::out) is det.

extend_var_table_loop(VarSet, CurVarNum, MaxVarNum, !RevVarTableAL) :-
    ( if CurVarNum =< MaxVarNum then
        record_untyped_var(VarSet, CurVarNum, !RevVarTableAL),
        extend_var_table_loop(VarSet, CurVarNum + 1, MaxVarNum, !RevVarTableAL)
    else
        true
    ).

    % Record a variable number that was allocated in the varset,
    % but whose type was not recorded.
    %
    % Before we started using var_tables, a lookup of such a variable
    % would succeed in the varset but fail in the vartypes.
    %
    % The var_table we are constructing will have valid info for the name,
    % but dummy info for the type.
    %
:- pred record_untyped_var(prog_varset::in, int::in,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::out) is det.

record_untyped_var(VarSet, VarNum, !RevVarTableAL) :-
    Var = force_construct_var(VarNum),
    ( if varset.search_name(VarSet, Var, NamePrime) then
        Name = NamePrime
    else
        Name = ""
    ),
    VarEntry = vte(Name, void_type, is_dummy_type),
    !:RevVarTableAL = [Var - VarEntry | !.RevVarTableAL].

%---------------------------------------------------------------------------%

split_var_table(VarTable, VarSet, VarTypes) :-
    deconstruct_var_table(VarTable, Counter, VarTableMap),
    map.to_sorted_assoc_list(VarTableMap, VarsEntries),
    split_var_table_loop(VarsEntries, [], RevVarTypes, [], RevVarNames),
    vartypes_from_rev_sorted_assoc_list(RevVarTypes, VarTypes),
    map.from_rev_sorted_assoc_list(RevVarNames, VarNameMap),
    (
        RevVarTypes = [],
        LastVarNum = 0
    ;
        RevVarTypes = [Var - _ | _],
        LastVarNum = var_to_int(Var)
    ),
    counter.allocate(NextVarNum, Counter, _),
    expect(unify(LastVarNum + 1, NextVarNum), $pred,
        "LastVarNum + 1 != NextVarNum"),
    construct_varset(LastVarNum, VarNameMap, VarSet).

:- pred split_var_table_loop(assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, mer_type)::in, assoc_list(prog_var, mer_type)::out,
    assoc_list(prog_var, string)::in, assoc_list(prog_var, string)::out)
    is det.

split_var_table_loop([], !RevVarTypes, !RevVarNames).
split_var_table_loop([Var - Entry | VarsEntries],
        !RevVarTypes, !RevVarNames) :-
    Entry = vte(Name, Type, _IsDummy),
    !:RevVarTypes = [Var - Type | !.RevVarTypes],
    ( if Name = "" then
        true
    else
        !:RevVarNames = [Var - Name | !.RevVarNames]
    ),
    split_var_table_loop(VarsEntries, !RevVarTypes, !RevVarNames).

%---------------------------------------------------------------------------%

vars_types_to_var_table(ModuleInfo, VarSet, VarsTypes, VarTable) :-
    vars_types_to_vars_entries(ModuleInfo, VarSet, VarsTypes, [], VarsEntries),
    list.sort(VarsEntries, SortedVarsEntries),
    var_table_from_sorted_assoc_list(SortedVarsEntries, VarTable).

:- pred vars_types_to_vars_entries(module_info::in, prog_varset::in,
    assoc_list(prog_var, mer_type)::in,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::out) is det.

vars_types_to_vars_entries(_, _, [], !VarsEntries).
vars_types_to_vars_entries(ModuleInfo, VarSet, [Var - Type | VarsTypes],
        !VarsEntries) :-
    ( if varset.search_name(VarSet, Var, Name0) then
        Name = Name0
    else
        Name = ""
    ),
    IsDummy = is_type_a_dummy(ModuleInfo, Type),
    Entry = vte(Name, Type, IsDummy),
    !:VarsEntries = [Var - Entry | !.VarsEntries],
    vars_types_to_vars_entries(ModuleInfo, VarSet, VarsTypes, !VarsEntries).

%---------------------------------------------------------------------------%

corresponding_vars_types_to_var_table(ModuleInfo, VarSet, Vars, Types,
        VarTable) :-
    corresponding_vars_types_to_vars_entries(ModuleInfo, VarSet, Vars, Types,
        [], VarsEntries),
    list.sort(VarsEntries, SortedVarsEntries),
    var_table_from_sorted_assoc_list(SortedVarsEntries, VarTable).

:- pred corresponding_vars_types_to_vars_entries(module_info::in,
    prog_varset::in, list(prog_var)::in, list(mer_type)::in,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::out) is det.

corresponding_vars_types_to_vars_entries(_, _, [], [], !VarsEntries).
corresponding_vars_types_to_vars_entries(_, _, [], [_ | _], !VarsEntries) :-
    unexpected($pred, "length mismatch").
corresponding_vars_types_to_vars_entries(_, _, [_ | _], [], !VarsEntries) :-
    unexpected($pred, "length mismatch").
corresponding_vars_types_to_vars_entries(ModuleInfo, VarSet,
        [Var | Vars], [Type | Types], !VarsEntries) :-
    ( if varset.search_name(VarSet, Var, Name0) then
        Name = Name0
    else
        Name = ""
    ),
    IsDummy = is_type_a_dummy(ModuleInfo, Type),
    Entry = vte(Name, Type, IsDummy),
    !:VarsEntries = [Var - Entry | !.VarsEntries],
    corresponding_vars_types_to_vars_entries(ModuleInfo, VarSet, Vars, Types,
        !VarsEntries).

%---------------------------------------------------------------------------%
:- end_module hlds.var_table_hlds.
%---------------------------------------------------------------------------%
