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

:- pred lookup_var_type_in_db(var_db::in, prog_var::in,
    mer_type::out) is det.
:- pred lookup_var_types_in_db(var_db::in, list(prog_var)::in,
    list(mer_type)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

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

%---------------------------------------------------------------------------%
:- end_module parse_tree.var_db.
%---------------------------------------------------------------------------%
