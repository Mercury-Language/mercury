%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: prog_mutable.m.
% Main authors: rafe, juliensf.

% Utility predicates for dealing with mutable declarations.

%-----------------------------------------------------------------------------%

:- module parse_tree.prog_mutable.
:- interface.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module string.

%-----------------------------------------------------------------------------%

    % Create a predmode declaration for the semipure mutable get predicate.
    % (This is the default get predicate.)
    %
:- func std_get_pred_decl(module_name, string, mer_type, mer_inst) = item.

    % Create a predmode declaration for the impure mutable set predicate.
    % (This is the default set predicate.)
    %
:- func std_set_pred_decl(module_name, string, mer_type, mer_inst) = item.

    % Create a predmode declaration for a get predicate for a constant mutable.
    % (This is only created if the `constant' attribute is given.)
    %
:- func constant_get_pred_decl(module_name, string, mer_type, mer_inst) = item.

    % Create a predmode declaration for a set predicate for a constant mutable;
    % this predicate is designed to be used only from the mutable's
    % initialization predicate.
    % (This is created only if the `constant' attribute is given.)
    %
:- func constant_set_pred_decl(module_name, string, mer_type, mer_inst) = item.

    % Create a predmode declaration for a get predicate using the I/O state.
    % (This is created only if the `attach_to_io_state' attribute is given.)
    %
:- func io_get_pred_decl(module_name, string, mer_type, mer_inst) = item.

    % Create a predmode declaration for a set predicate using the I/O state.
    % (This is created only if the `attach_to_io_state' attribute is given.)
    %
:- func io_set_pred_decl(module_name, string, mer_type, mer_inst) = item.

    % Create a predmode declaration for the mutable initialisation predicate.
    %
:- func mutable_init_pred_decl(module_name, string) = item.

:- func mutable_get_pred_sym_name(sym_name, string) = sym_name.

:- func mutable_set_pred_sym_name(sym_name, string) = sym_name.

    % We need a set predicate even for constant mutables. The reason is that
    % the init predicate needs to do two things: execute arbitrary Mercury code
    % (call functions etc) to generate the initial (and for constant mutables,
    % also final) value of the mutable, and then store this value in persistent
    % storage. However, even we could create an item that contains both
    % Mercury code and backend (e.g. C) code (which is currently not possible),
    % this would require the second part to be a foreign_proc goal. Such goals
    % include a reference to the predicate they implement. That predicate
    % would be equivalent to the set predicate.
    %
    % Avoiding the need for a set predicate would require significant changes
    % to the structures of items. It is much simpler to use a predicate and
    % give it a name that makes it clear people shouldn't use it.
    %
:- func mutable_secret_set_pred_sym_name(sym_name, string) = sym_name.

:- func mutable_init_pred_sym_name(sym_name, string) = sym_name.

:- func mutable_c_var_name(sym_name, string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module libs.compiler_util.

:- import_module list.
:- import_module maybe.
:- import_module varset.

%-----------------------------------------------------------------------------%

std_get_pred_decl(ModuleName, Name, Type, Inst) = GetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    GetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_get_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, out_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(det),
        true /* condition */, purity_semipure, Constraints).

std_set_pred_decl(ModuleName, Name, Type, Inst) = SetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    SetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_set_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, in_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(det),
        true /* condition */, purity_impure, Constraints).

constant_get_pred_decl(ModuleName, Name, Type, Inst) = GetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    GetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_get_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, out_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(det),
        true /* condition */, purity_pure, Constraints).

constant_set_pred_decl(ModuleName, Name, Type, Inst) = SetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    SetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_secret_set_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, in_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(det),
        true /* condition */, purity_impure, Constraints).

io_get_pred_decl(ModuleName, Name, Type, Inst) = GetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    GetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_get_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, out_mode(Inst)),
        type_and_mode(io_state_type, di_mode),
        type_and_mode(io_state_type, uo_mode)],
        no /* with_type */, no /* with_inst */, yes(det),
        true /* condition */, purity_pure, Constraints).

io_set_pred_decl(ModuleName, Name, Type, Inst) = SetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    SetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_set_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, in_mode(Inst)),
        type_and_mode(io_state_type, di_mode),
        type_and_mode(io_state_type, uo_mode)],
        no /* with_type */, no /* with_inst */, yes(det),
        true /* condition */, purity_pure, Constraints).

mutable_init_pred_decl(ModuleName, Name) = InitPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    InitPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars,
        predicate, mutable_init_pred_sym_name(ModuleName, Name),
        [], no /* with_type */, no /* with_inst */, yes(det),
        true /* condition */, purity_impure, Constraints).

%-----------------------------------------------------------------------------%

mutable_get_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "get_" ++ Name).

mutable_set_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "set_" ++ Name).

mutable_secret_set_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "secret_initialization_only_set_" ++ Name).

mutable_init_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "initialise_mutable_" ++ Name).

mutable_c_var_name(ModuleName, Name) = MangledCVarName :-
    RawCVarName       = "mutable_variable_" ++ Name,
    QualifiedCVarName = qualified(ModuleName, RawCVarName),
    MangledCVarName   = sym_name_mangle(QualifiedCVarName).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "prog_mutable.m".

%-----------------------------------------------------------------------------%
:- end_module prog_mutable.
%-----------------------------------------------------------------------------%
