%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module's job is to make it possible for compiler passes to operate
% on variables using either
%
% - var_tables, which most of the compiler uses to represent information
%   about variables, or
%
% - prog_varsets and/or vartypes, which a few compiler passes near the start
%   of the compilation process use for the same purpose.
%
%---------------------------------------------------------------------------%

:- module parse_tree.var_db.
:- interface.

:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.var_table.
:- import_module parse_tree.vartypes.

:- import_module list.

%---------------------------------------------------------------------------%

    % A source for information about the types of variables.
    %
:- type var_type_source
    --->    vts_vartypes(vartypes)
    ;       vts_var_table(var_table).

:- pred lookup_var_type_in_source(var_type_source::in, prog_var::in,
    mer_type::out) is det.
:- pred lookup_var_types_in_source(var_type_source::in, list(prog_var)::in,
    list(mer_type)::out) is det.

%---------------------------------------------------------------------------%

    % A source for information about the names of variables.
    %
:- type var_name_source
    --->    vns_varset(prog_varset)
    ;       vns_var_table(var_table).

:- pred search_var_name_in_source(var_name_source::in, prog_var::in,
    string::out) is semidet.
:- pred lookup_var_name_in_source(var_name_source::in, prog_var::in,
    string::out) is det.

%---------------------------------------------------------------------------%

    % A source for information about both the names and types of variables.
    %
:- type var_db
    --->    var_db_varset_vartypes(prog_var_set_types)
    ;       var_db_var_table(var_table).

:- pred add_entry_to_var_db(var_table_entry::in, prog_var::out,
    var_db::in, var_db::out) is det.
:- pred add_prefix_number_var_entry_to_var_set_vartypes(string::in,
    mer_type::in, is_dummy_type::in, prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.
:- pred add_prefix_number_var_entry_to_var_db(string::in,
    mer_type::in, is_dummy_type::in, prog_var::out,
    var_db::in, var_db::out) is det.

:- pred lookup_var_type_in_db(var_db::in, prog_var::in,
    mer_type::out) is det.
:- pred lookup_var_types_in_db(var_db::in, list(prog_var)::in,
    list(mer_type)::out) is det.

:- pred set_var_name_in_db(prog_var::in, string::in,
    var_db::in, var_db::out) is det.

:- pred var_db_count(var_db::in, int::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module string.
:- import_module term.
:- import_module varset.

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
        var_table.search_var_name(VarTable, Var, Name)
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

%---------------------%

add_entry_to_var_db(Entry, Var, !VarDb) :-
    (
        !.VarDb = var_db_varset_vartypes(VarSetTypes0),
        VarSetTypes0 = prog_var_set_types(VarSet0, VarTypes0),
        Entry = vte(Name, Type, _IsDummy),
        ( if Name = "" then
            varset.new_var(Var, VarSet0, VarSet)
        else
            varset.new_named_var(Name, Var, VarSet0, VarSet)
        ),
        vartypes.add_var_type(Var, Type, VarTypes0, VarTypes),
        VarSetTypes = prog_var_set_types(VarSet, VarTypes),
        !:VarDb = var_db_varset_vartypes(VarSetTypes)
    ;
        !.VarDb = var_db_var_table(VarTable0),
        add_var_entry(Entry, Var, VarTable0, VarTable),
        !:VarDb = var_db_var_table(VarTable)
    ).

add_prefix_number_var_entry_to_var_set_vartypes(Prefix, Type, _IsDummy, Var,
        !VarSet, !VarTypes) :-
    varset.new_var(Var, !VarSet),
    term.var_to_int(Var, VarNum),
    string.format("%s_%d", [s(Prefix), i(VarNum)], Name),
    varset.name_var(Var, Name, !VarSet),
    vartypes.add_var_type(Var, Type, !VarTypes).

add_prefix_number_var_entry_to_var_db(Prefix, Type, IsDummy, Var, !VarDb) :-
    (
        !.VarDb = var_db_varset_vartypes(VarSetTypes0),
        VarSetTypes0 = prog_var_set_types(VarSet0, VarTypes0),
        add_prefix_number_var_entry_to_var_set_vartypes(Prefix, Type, IsDummy,
            Var, VarSet0, VarSet, VarTypes0, VarTypes),
        VarSetTypes = prog_var_set_types(VarSet, VarTypes),
        !:VarDb = var_db_varset_vartypes(VarSetTypes)
    ;
        !.VarDb = var_db_var_table(VarTable0),
        add_prefix_number_var_entry(Prefix, Type, IsDummy, Var,
            VarTable0, VarTable),
        !:VarDb = var_db_var_table(VarTable)
    ).

%---------------------%

lookup_var_type_in_db(VarDb, Var, Type) :-
    (
        VarDb = var_db_varset_vartypes(prog_var_set_types(_, VarTypes)),
        vartypes.lookup_var_type(VarTypes, Var, Type)
    ;
        VarDb = var_db_var_table(VarTable),
        var_table.lookup_var_type(VarTable, Var, Type)
    ).

lookup_var_types_in_db(VarDb, Vars, Types) :-
    (
        VarDb = var_db_varset_vartypes(prog_var_set_types(_, VarTypes)),
        vartypes.lookup_var_types(VarTypes, Vars, Types)
    ;
        VarDb = var_db_var_table(VarTable),
        var_table.lookup_var_types(VarTable, Vars, Types)
    ).

%---------------------%

set_var_name_in_db(Var, Name, !VarDb) :-
    (
        !.VarDb = var_db_varset_vartypes(VarSetVarTypes0),
        VarSetVarTypes0 = prog_var_set_types(VarSet0, VarTypes),
        varset.name_var(Var, Name, VarSet0, VarSet),
        VarSetVarTypes = prog_var_set_types(VarSet, VarTypes),
        !:VarDb = var_db_varset_vartypes(VarSetVarTypes)
    ;
        !.VarDb = var_db_var_table(VarTable0),
        update_var_name(Var, Name, VarTable0, VarTable),
        !:VarDb = var_db_var_table(VarTable)
    ).

%---------------------%

var_db_count(VarDb, Count) :-
    (
        VarDb = var_db_varset_vartypes(prog_var_set_types(VarSet, _)),
        Count = varset.num_allocated(VarSet)
    ;
        VarDb = var_db_var_table(VarTable),
        var_table_count(VarTable, Count)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.var_db.
%---------------------------------------------------------------------------%
