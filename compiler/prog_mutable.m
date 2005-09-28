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

:- func prog_mutable.get_pred_decl(module_name, string, (type), (inst))
	= item.

:- func prog_mutable.set_pred_decl(module_name, string, (type), (inst))
	= item.

:- func prog_mutable.init_pred_decl(module_name, string) = item.

    % XXX We should probably mangle Name for safety...
    %
:- func mutable_get_pred_sym_name(sym_name, string) = sym_name.

:- func mutable_set_pred_sym_name(sym_name, string) = sym_name.

:- func mutable_init_pred_sym_name(sym_name, string) = sym_name.

:- func mutable_c_var_name(sym_name, string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_mode.
:- import_module list.
:- import_module std_util.
:- import_module varset.

%-----------------------------------------------------------------------------%

get_pred_decl(ModuleName, Name, Type, Inst) = GetPredDecl :-
	VarSet = varset__init,
	InstVarSet = varset__init,
	ExistQVars = [],
	Constraints = constraints([], []),
	GetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
	    mutable_get_pred_sym_name(ModuleName, Name),
	    [type_and_mode(Type, out_mode(Inst))],
	    no /* with_type */, no /* with_inst */, yes(det),
	    true /* condition */, (semipure), Constraints).
        
set_pred_decl(ModuleName, Name, Type, Inst) = SetPredDecl :-
	VarSet = varset__init,
	InstVarSet = varset__init,
	ExistQVars = [],
	Constraints = constraints([], []),
	SetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
            mutable_set_pred_sym_name(ModuleName, Name),
            [type_and_mode(Type, in_mode(Inst))],
            no /* with_type */, no /* with_inst */, yes(det),
            true /* condition */, (impure), Constraints).

init_pred_decl(ModuleName, Name) = InitPredDecl :-
	VarSet = varset__init,
	InstVarSet = varset__init,
	ExistQVars = [],
	Constraints = constraints([], []),
	InitPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars,
		predicate, mutable_init_pred_sym_name(ModuleName, Name),
    		[], no /* with_type */, no /* with_inst */, yes(det),
    		true /* condition */, (impure), Constraints).

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
