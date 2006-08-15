%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: prog_mutable.m.
% Main authors: rafe, juliensf.
%
% This module defines utility predicates for dealing with mutable
% declarations.  It also contains a description of the source-to-source
% transformation used for implementing mutables.
%
%-----------------------------------------------------------------------------%
%
% Mutables are implemented as a source-to-source transformation on the
% parse tree.  The basic transformation is as follows:
%
%   :- mutable(<varname>, <vartype>, <initvalue>, <varinst>, [attributes]).
% 
% ===>
%   
%   :- pragma foreign_decl("C", "extern <CType> mutable_<varname>;").
%   :- pragma foreign_code("C", "<CType> mutable_<varname>;");
%
% NOTES: 
%
%        * The name of the C global corresponding to mutable_<varname> may be
%          mangled.
%
%        * <CType> is chosen on a backend-specific basis.  If the value stored
%          in the mutable is always boxed it is `MR_Word' otherwise it may
%          be some native type, `MR_Integer', `MR_Float' etc.
%
%   :- initialise initialise_mutable_<varname>/0.
%
%   :- impure pred initialise_mutable_<varname> is det.
%   
%   initialise_mutable_<varname> :-
%       impure set_<varname>(<initval>).
% 
%   :- semipure pred get_<varname>(<vartype>::out(<varinst>)) is det.
%   :- pragma foreign_proc("C",
%       get_<varname>(X::out(<varinst>)),
%       [promise_semipure, will_not_call_mercury],
%   "
%       X = mutable_<varname>;
%   ");
%   
%   :- impure pred set_<varname>(<vartype>::in(<varinst>)) is det.
%   :- pragma foreign_proc("C",
%       set_<varname>(X::in(<varinst>)),
%       [will_not_call_mercury],
%   "
%       MR_trail_current_value(&mutable_<varname>);
%       mutable_<varname> = X; 
%   ").
%
% NOTE: mutables *are* trailed by default.  The `untrailed' attribute just
%       causes the call to MR_trail_current_value to be omitted.
%
% If the `attach_to_io_state' attribute is specified we also generate:
%
%   :- pred get_varname(<vartype>::out(<varinst>), io::di, io::uo) is det.
%   :- pred set_varname(<vartype>::in(<varinst>),  io::di, io::uo) is det.
%
%   :- pragma foreign_proc("C",
%       get_varname(X::out(<varinst), IO0::di, IO::uo),
%       [promise_pure, will_not_call_mercury, tabled_for_io],
%   "
%       X = mutable_<varname>;
%       IO = IO0;
%   ").
%  
%   :- pragma foreign_proc("C",
%       set_varname(X::in(<varinst>), IO0::di, IO::uo),
%       [promise_pure, will_not_call_mercury, tabled_for_io],
%   "
%       mutable_<varname> = X;
%       IO = IO0;
%   ").
% 
% NOTE: we could implement the above in terms of the impure get and set
%       predicates.  The reason we don't is so that we can use I/O 
%       tabling.
%       XXX If tabling of impure actions is ever implemented we should\
%           revisit this.
% 
% For constant mutables (those with the `constant' attribute), the 
% transformation is a little different:
%
%   :- mutable(<varname>, <vartype>, <initvalue>, <varinst>, [constant]).
%
% ===>
%
%   (The declarations for the global are as above.)
%
%   :- pred get_<varname>(<vartype>::out(<varinst>)) is det.
%   :- pragma foreign_proc("C",
%       get_<varname>(X::out(<varinst>)),
%       [will_not_call_mercury, promise_pure, thread_safe],
%   "
%       X = mutable_<varname>;
%   ").
% 
% In order to initialise constant mutables we generate the following:
%
%   :- impure pred secret_initialization_only_set_<varname>(
%       <vartype>::in(<varinst>)) is det.
% 
%   :- pragma foreign_proc("C",
%       secret_initialization_only_set_<varname>(X::in(<varinst>)),
%       [will_not_call_mercury],
%   "
%       mutable_<varname> = X;
%   ").
%
%   :- initialise secret_initialization_only_set_<varname>/0.
%
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
    % storage. However, even if we could create an item that contains both
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
        no /* with_type */, no /* with_inst */, yes(detism_det),
        true /* condition */, purity_semipure, Constraints).

std_set_pred_decl(ModuleName, Name, Type, Inst) = SetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    SetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_set_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, in_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(detism_det),
        true /* condition */, purity_impure, Constraints).

constant_get_pred_decl(ModuleName, Name, Type, Inst) = GetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    GetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_get_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, out_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(detism_det),
        true /* condition */, purity_pure, Constraints).

constant_set_pred_decl(ModuleName, Name, Type, Inst) = SetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    SetPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_secret_set_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, in_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(detism_det),
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
        no /* with_type */, no /* with_inst */, yes(detism_det),
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
        no /* with_type */, no /* with_inst */, yes(detism_det),
        true /* condition */, purity_pure, Constraints).

mutable_init_pred_decl(ModuleName, Name) = InitPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    ArgDecls = [],
    WithType = no,
    WithInst = no,
    Condition = true,
    InitPredDecl = pred_or_func(VarSet, InstVarSet, ExistQVars, predicate,
        mutable_init_pred_sym_name(ModuleName, Name), ArgDecls,
        WithType, WithInst, yes(detism_det), Condition,
        purity_impure, Constraints).

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
