%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main authors: rafe, juliensf
%
% Utility predicates for dealing with mutable declarations.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_mutable.

:- interface.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module string.

%-----------------------------------------------------------------------------%

    % Create a predmode declaration for a non-pure mutable get predicate.
    % (This is the default get predicate.)
    %
:- func nonpure_get_pred_decl(module_name, string, (type), (inst)) = item.

    % Create a predmode declaration for a non-pure mutable set predicate.
    % (This is the default set predicate.)
    %
:- func nonpure_set_pred_decl(module_name, string, (type), (inst)) = item.

    % Create a predmode declaration for a pure mutable get predicate.
    % (This is only created if the `pure' mutable attribute is given.)
    %
:- func pure_get_pred_decl(module_name, string, (type), (inst)) = item.

    % Create a predmode declaration for a pure mutable set predicate.
    % (This is only create the `pure' mutable attribute is give.)
    %
:- func pure_set_pred_decl(module_name, string, (type), (inst)) = item.

    % Create a predmode declaration for the mutable initialisation
    % predicate.
    %
:- func init_pred_decl(module_name, string) = item.

    % Create the foreign_decl for the mutable.
    %
:- func get_global_foreign_decl(string) = item.

    % Create the foreign_code that defines the mutable.
    %
:- func get_global_foreign_defn(string) = item.

:- func mutable_get_pred_sym_name(sym_name, string) = sym_name.

:- func mutable_set_pred_sym_name(sym_name, string) = sym_name.

:- func mutable_init_pred_sym_name(sym_name, string) = sym_name.

:- func mutable_c_var_name(sym_name, string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.globals.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_mode.
:- import_module list.
:- import_module std_util.
:- import_module varset.

%-----------------------------------------------------------------------------%

nonpure_get_pred_decl(ModuleName, Name, Type, Inst) = GetPredDecl :-
    VarSet = varset__init,
    InstVarSet = varset__init,
    ExistQVars = [],
    Constraints = constraints([], []),
    GetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_get_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, out_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(det),
        true /* condition */, (semipure), Constraints).

nonpure_set_pred_decl(ModuleName, Name, Type, Inst) = SetPredDecl :-
    VarSet = varset__init,
    InstVarSet = varset__init,
    ExistQVars = [],
    Constraints = constraints([], []),
    SetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_set_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, in_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(det),
        true /* condition */, (impure), Constraints).

pure_get_pred_decl(ModuleName, Name, Type, Inst) = GetPredDecl :-
    VarSet = varset__init,
    InstVarSet = varset__init,
    ExistQVars = [],
    Constraints = constraints([], []),
    GetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_get_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, out_mode(Inst)),
        type_and_mode(io_state_type, di_mode),
        type_and_mode(io_state_type, uo_mode)],
        no /* with_type */, no /* with_inst */, yes(det),
        true /* condition */, pure, Constraints).

pure_set_pred_decl(ModuleName, Name, Type, Inst) = SetPredDecl :-
    VarSet = varset__init,
    InstVarSet = varset__init,
    ExistQVars = [],
    Constraints = constraints([], []),
    SetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_set_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, in_mode(Inst)),
        type_and_mode(io_state_type, di_mode),
        type_and_mode(io_state_type, uo_mode)],
        no /* with_type */, no /* with_inst */, yes(det),
        true /* condition */, pure, Constraints).

    % Return the type io.state.
    % XXX Perhaps this should be in prog_type?
    %
:- func io_state_type = (type).

io_state_type = defined(qualified(unqualified("io"), "state"), [], star).

init_pred_decl(ModuleName, Name) = InitPredDecl :-
    VarSet = varset__init,
    InstVarSet = varset__init,
    ExistQVars = [],
    Constraints = constraints([], []),
    InitPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars,
        predicate, mutable_init_pred_sym_name(ModuleName, Name),
        [], no /* with_type */, no /* with_inst */, yes(det),
        true /* condition */, (impure), Constraints).

%-----------------------------------------------------------------------------%

get_global_foreign_decl(TargetMutableName) =
    pragma(compiler(mutable_decl),
        foreign_decl(c, foreign_decl_is_exported,
            "extern MR_Word " ++ TargetMutableName ++ ";")).

get_global_foreign_defn(TargetMutableName) =
    pragma(compiler(mutable_decl),
        foreign_code(c, "MR_Word " ++ TargetMutableName ++ ";")).

%-----------------------------------------------------------------------------%

mutable_get_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "get_" ++ Name).

mutable_set_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "set_" ++ Name).

mutable_init_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "initialise_mutable_" ++ Name).

mutable_c_var_name(ModuleName, Name) = MangledCVarName :-
    RawCVarName       = "mutable_variable_" ++ Name,
    QualifiedCVarName = qualified(ModuleName, RawCVarName),
    MangledCVarName   = sym_name_mangle(QualifiedCVarName).

%-----------------------------------------------------------------------------%
:- end_module prog_mutable.
%-----------------------------------------------------------------------------%
