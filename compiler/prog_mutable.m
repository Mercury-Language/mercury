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
% This module defines utility predicates for dealing with mutable declarations.
% It also contains a description of the source-to-source transformation
% used for implementing mutables.
%
%-----------------------------------------------------------------------------%
%
% Mutables are implemented as a source-to-source transformation on the
% parse tree.  For non-constant mutables the transformation is as follows: 
%
%   :- mutable(<varname>, <vartype>, <initvalue>, <varinst>, [attributes]).
% 
% ===>
%   
%   :- pragma foreign_decl("C", "
%           extern <CType> mutable_<varname>;
%           #ifdef MR_THREAD_SAFE
%               extern MercuryLock mutable_<varname>_lock;
%           #endif
%
%   ").
%   
%   :- pragma foreign_code("C", "
%           <CType> mutable_<varname>;
%           #ifdef MR_THREAD_SAFE
%               MercuryLock mutable_<varname>_lock;
%           #endif
%   ").
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
%       impure initialise_mutex_for_mutable_<varname>,
%       impure set_<varname>(<initval>).
% 
%   :- impure pred initialise_mutex_for_mutable_<varname> is det.
%   :- pragma foreign_proc("C",
%       initialise_mutex_for_mutable_<varname>,
%       [will_not_call_mercury],
%   "
%       #ifdef MR_THREAD_SAFE
%           pthread_init_mutex(&mutable_<varname>_lock, MR_MUTEX_ATTR);
%       #endif
%   ").
%
% Operations on mutables are defined in terms of the following four 
% predicates.  Note that they are all marked `thread_safe' in order to
% avoid having to acquire the global lock.
%
%   :- impure pred unsafe_set_<varname>(<vartype>::in(<varinst>)) is det.
%   :- pragma foreign_proc("C",
%       unsafe_set_<varname)(X::in(<varinst>)),
%       [will_not_call_mercury, thread_safe],
%   "
%       mutable_<varname> = X;
%   ").
%
%   :- semipure pred unsafe_get_<varname>(<vartype>::out(<varinst>)) is det.
%   :- pragma foreign_proc("C",
%       unsafe_get_varname(X::in(<varinst>)),
%       [promise_semipure, will_not_all_mercury, thread_safe],
%   "
%        X = mutable_<varname>;
%   ").   
%
%   :- impure lock_<varname> is det.
%   :- pramga foreign_proc("C",
%       lock_<varname>,
%       [will_not_call_mercury, promise_pure],
%   "   
%       #ifdef MR_THREAD_SAFE
%          MR_LOCK(&mutable_<varname>_lock, \"lock_<varname>/0\");      
%       #endif
%   ").
%
%   :- impure unlock_<varname> is det.
%   :- pramga foreign_proc("C",
%       unlock_<varname>,
%       [will_not_call_mercury, promise_pure],
%   "   
%       #ifdef MR_THREAD_SAFE
%          MR_UNLOCK(&mutable_<varname>_lock, \"unlock_<varname>/0\");      
%       #endif
%   ").
%
% The other operations are all defined in Mercury using the above predicates:
%
% :- impure pred set_<varname>(<vartype>::in(<varinst>)) is det.
%
% set_<varname>(X) :-
%   impure lock_<varname>,
%   impure unsafe_set_<varname>(X),
%   impure unlock_<varname>.
%
% :- semipure pred get_<varname>(<vartype>::out(<varinst>)) is det.
% 
% get_<varname>(X) :-
%   promise_semipure (
%       impure lock_<varname>
%       semipure unsafe_get_<varname>(X),
%       impure unlock_<varname>
%  ).
%
% etc.
% 
% For constant mutables the transformation is:
%
%   :- mutable(<varname>, <vartype>, <initvalue>, <varinst>, [constant]).
%
% ===>
%
%   :- pragma foreign_decl("C", "extern <CType> mutable_<varname>;").
%   :- pragma foreign_code("C", "<CType> mutable_<varname>;").
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
%   :- initialise initialise_mutable_<varname>/0.
%
%   :- impure pred initialise_mutable_<varname> is det.
%
%   initialise_mutable_<varname> :-
%       impure secret_initialization_only_set_<varname>(<initval>).
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_mutable.
:- interface.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module string.

%-----------------------------------------------------------------------------%

    % Create predmode declarations for the four primitive operations.
    %
:- func unsafe_get_pred_decl(module_name, string, mer_type, mer_inst) = item.
:- func unsafe_set_pred_decl(module_name, string, mer_type, mer_inst) = item.
:- func lock_pred_decl(module_name, string) = item.
:- func unlock_pred_decl(module_name, string) = item.

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

    % Create a predmode declaration for the mutable mutex initialisation
    % predicate.
    %
:- func mutable_init_mutex_pred_decl(module_name, string) = item.

    % Names of the primtive operations.
    %
:- func mutable_lock_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_unlock_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_unsafe_get_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_unsafe_set_pred_sym_name(sym_name, string) = sym_name.

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
:- func mutable_secret_set_pred_sym_name(module_name, string) = sym_name.

:- func mutable_init_pred_sym_name(module_name, string) = sym_name.

:- func mutable_init_mutex_pred_sym_name(module_name, string) = sym_name.

:- func mutable_c_var_name(module_name, string) = string.

    % Returns the name of the mutex associated a given mutable.  The
    % input to this function is the name of the mutable in the target
    % language, i.e it is the result of a call to mutable_c_var_name/2
    % or one of the specified foreign names for the mutable.
    %
:- func mutable_mutex_var_name(string) = string.

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
%
% Predmode declarations for primitive operations
%

unsafe_get_pred_decl(ModuleName, Name, Type, Inst) = UnsafeGetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    Origin = compiler(mutable_decl),
    UnsafeGetPredDecl = item_pred_or_func(Origin, VarSet, InstVarSet,
        ExistQVars,
        predicate,
        mutable_unsafe_get_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, out_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(detism_det),
        cond_true /* condition */, purity_semipure, Constraints).

unsafe_set_pred_decl(ModuleName, Name, Type, Inst) = UnsafeSetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    Origin = compiler(mutable_decl),
    UnsafeSetPredDecl = item_pred_or_func(Origin, VarSet, InstVarSet,
        ExistQVars,
        predicate,
        mutable_unsafe_set_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, in_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(detism_det),
        cond_true /* condition */, purity_impure, Constraints).

lock_pred_decl(ModuleName, Name) = LockPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    Origin = compiler(mutable_decl),
    LockPredDecl = item_pred_or_func(Origin, VarSet, InstVarSet, ExistQVars,
        predicate,
        mutable_lock_pred_sym_name(ModuleName, Name),
        [],
        no /* with_type */, no /* with_inst */, yes(detism_det),
        cond_true /* condition */, purity_impure, Constraints).

unlock_pred_decl(ModuleName, Name) = UnlockPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    Origin = compiler(mutable_decl),
    UnlockPredDecl = item_pred_or_func(Origin, VarSet, InstVarSet, ExistQVars,
        predicate,
        mutable_unlock_pred_sym_name(ModuleName, Name),
        [],
        no /* with_type */, no /* with_inst */, yes(detism_det),
        cond_true /* condition */, purity_impure, Constraints).

%-----------------------------------------------------------------------------%

std_get_pred_decl(ModuleName, Name, Type, Inst) = GetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    Origin = compiler(mutable_decl),
    GetPredDecl = item_pred_or_func(Origin, VarSet, InstVarSet, ExistQVars,
        predicate,
        mutable_get_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, out_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(detism_det),
        cond_true /* condition */, purity_semipure, Constraints).

std_set_pred_decl(ModuleName, Name, Type, Inst) = SetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    Origin = compiler(mutable_decl),
    SetPredDecl = item_pred_or_func(Origin, VarSet, InstVarSet, ExistQVars,
        predicate,
        mutable_set_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, in_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(detism_det),
        cond_true /* condition */, purity_impure, Constraints).

constant_get_pred_decl(ModuleName, Name, Type, Inst) = GetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    Origin = compiler(mutable_decl),
    GetPredDecl = item_pred_or_func(Origin, VarSet, InstVarSet, ExistQVars,
        predicate,
        mutable_get_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, out_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(detism_det),
        cond_true /* condition */, purity_pure, Constraints).

constant_set_pred_decl(ModuleName, Name, Type, Inst) = SetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    Origin = compiler(mutable_decl),
    SetPredDecl = item_pred_or_func(Origin, VarSet, InstVarSet, ExistQVars,
        predicate,
        mutable_secret_set_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, in_mode(Inst))],
        no /* with_type */, no /* with_inst */, yes(detism_det),
        cond_true /* condition */, purity_impure, Constraints).

io_get_pred_decl(ModuleName, Name, Type, Inst) = GetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    Origin = compiler(mutable_decl),
    GetPredDecl = item_pred_or_func(Origin, VarSet, InstVarSet, ExistQVars,
        predicate,
        mutable_get_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, out_mode(Inst)),
        type_and_mode(io_state_type, di_mode),
        type_and_mode(io_state_type, uo_mode)],
        no /* with_type */, no /* with_inst */, yes(detism_det),
        cond_true /* condition */, purity_pure, Constraints).

io_set_pred_decl(ModuleName, Name, Type, Inst) = SetPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    Origin = compiler(mutable_decl),
    SetPredDecl = item_pred_or_func(Origin, VarSet, InstVarSet, ExistQVars,
        predicate,
        mutable_set_pred_sym_name(ModuleName, Name),
        [type_and_mode(Type, in_mode(Inst)),
        type_and_mode(io_state_type, di_mode),
        type_and_mode(io_state_type, uo_mode)],
        no /* with_type */, no /* with_inst */, yes(detism_det),
        cond_true /* condition */, purity_pure, Constraints).

mutable_init_pred_decl(ModuleName, Name) = InitPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    ArgDecls = [],
    WithType = no,
    WithInst = no,
    Condition = cond_true,
    Origin = compiler(mutable_decl),
    InitPredDecl = item_pred_or_func(Origin, VarSet, InstVarSet, ExistQVars,
        predicate, mutable_init_pred_sym_name(ModuleName, Name), ArgDecls,
        WithType, WithInst, yes(detism_det), Condition,
        purity_impure, Constraints).

mutable_init_mutex_pred_decl(ModuleName, Name) = InitMutexPredDecl :-
    VarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    Constraints = constraints([], []),
    ArgDecls = [],
    WithType = no,
    WithInst = no,
    Condition = cond_true,
    Origin = compiler(mutable_decl),
    InitMutexPredDecl = item_pred_or_func(Origin, VarSet, InstVarSet,
        ExistQVars, predicate,
        mutable_init_mutex_pred_sym_name(ModuleName, Name),
        ArgDecls, WithType, WithInst, yes(detism_det), Condition,
        purity_impure, Constraints).

%-----------------------------------------------------------------------------%

mutable_lock_pred_sym_name(ModuleName, Name) = 
    qualified(ModuleName, "lock_" ++ Name).

mutable_unlock_pred_sym_name(ModuleName, Name) = 
    qualified(ModuleName, "unlock_" ++ Name).

mutable_unsafe_get_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "unsafe_get_" ++ Name).

mutable_unsafe_set_pred_sym_name(ModuleName, Name) = 
    qualified(ModuleName, "unsafe_set_" ++ Name).

mutable_get_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "get_" ++ Name).

mutable_set_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "set_" ++ Name).

mutable_secret_set_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "secret_initialization_only_set_" ++ Name).

mutable_init_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "initialise_mutable_" ++ Name).

mutable_init_mutex_pred_sym_name(ModuleName, Name) = 
    qualified(ModuleName, "initialise_mutex_for_mutable_" ++ Name).

mutable_c_var_name(ModuleName, Name) = MangledCVarName :-
    RawCVarName       = "mutable_variable_" ++ Name,
    QualifiedCVarName = qualified(ModuleName, RawCVarName),
    MangledCVarName   = sym_name_mangle(QualifiedCVarName).

mutable_mutex_var_name(TargetMutableVarName) = MutexVarName :-
    MutexVarName = TargetMutableVarName ++ "_lock". 

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "prog_mutable.m".

%-----------------------------------------------------------------------------%
:- end_module prog_mutable.
%-----------------------------------------------------------------------------%
