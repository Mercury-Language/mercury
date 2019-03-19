%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2011 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
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
% parse tree. The transformation depends on the compilation target.
%
%-----------------------------------------------------------------------------%
%
% C BACKENDS
%
% For non-constant mutables the transformation is as follows:
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
%        * <CType> is chosen on a backend-specific basis. If the value stored
%          in the mutable is always boxed it is `MR_Word' otherwise it may
%          be some native type, `MR_Integer', `MR_Float' etc.
%
%   :- initialise initialise_mutable_<varname>/0.
%
%   :- impure pred initialise_mutable_<varname> is det.
%
%   initialise_mutable_<varname> :-
%       impure pre_initialise_mutable_<varname>,
%       impure X = <initval>,
%       impure set_<varname>(X).
%
%   :- impure pred pre_initialise_mutable_<varname> is det.
%   :- pragma foreign_proc("C",
%       pre_initialise_mutable_<varname>,
%       [will_not_call_mercury],
%   "
%       #ifdef MR_THREAD_SAFE
%           pthread_init_mutex(&mutable_<varname>_lock, MR_MUTEX_ATTR);
%       #endif
%   ").
%
% Operations on mutables are defined in terms of the following four predicates.
% Note that they are all marked `thread_safe' in order to avoid having
% to acquire the global lock.
%
%   :- impure pred unsafe_set_<varname>(<vartype>::in(<varinst>)) is det.
%   :- pragma foreign_proc("C",
%       unsafe_set_<varname>(X::in(<varinst>)),
%       [will_not_call_mercury, thread_safe],
%   "
%       mutable_<varname> = X;
%   ").
%
%   :- semipure pred unsafe_get_<varname>(<vartype>::out(<varinst>)) is det.
%   :- pragma foreign_proc("C",
%       unsafe_get_<varname>(X::out(<varinst>)),
%       [promise_semipure, will_not_call_mercury, thread_safe],
%   "
%        X = mutable_<varname>;
%   ").
%
%   :- impure lock_<varname> is det.
%   :- pragma foreign_proc("C",
%       lock_<varname>,
%       [will_not_call_mercury, promise_pure],
%   "
%       #ifdef MR_THREAD_SAFE
%          MR_LOCK(&mutable_<varname>_lock, \"lock_<varname>/0\");
%       #endif
%   ").
%
%   :- impure unlock_<varname> is det.
%   :- pragma foreign_proc("C",
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
%   :- impure pred set_<varname>(<vartype>::in(<varinst>)) is det.
%
%   set_<varname>(X) :-
%       impure lock_<varname>,
%       impure unsafe_set_<varname>(X),
%       impure unlock_<varname>.
%
%   :- semipure pred get_<varname>(<vartype>::out(<varinst>)) is det.
%
%   get_<varname>(X) :-
%       promise_semipure (
%           impure lock_<varname>
%           semipure unsafe_get_<varname>(X),
%           impure unlock_<varname>
%       ).
%
% etc.
%
% For thread-local mutables the transformation is as above, with the following
% differences:
%
%   :- mutable(<varname>, <vartype>, <initvalue>, <varinst>, [thread_local]).
%
% ===>
%
%   :- pragma foreign_decl("C", "extern MR_Unsigned mutable_<varname>;").
%   :- pragma foreign_code("C", "MR_Unsigned mutable_<varname>;").
%
%   :- pragma foreign_proc("C",
%       pre_initialise_mutable_<varname>,
%       [will_not_call_mercury],
%   "
%       mutable_<varname> = MR_new_thread_local_mutable_index();
%   ").
%
%   :- pragma foreign_proc("C",
%       unsafe_set_<varname>(X::in(<varinst>)),
%       [will_not_call_mercury, thread_safe],
%   "
%       MR_set_thread_local_mutable(<type>, X, mutable_<varname>);
%   ").
%
%   :- pragma foreign_proc("C",
%       unsafe_get_<varname>(X::out(<varinst>)),
%       [promise_semipure, will_not_call_mercury, thread_safe],
%   "
%        MR_get_thread_local_mutable(<type>, X, mutable_<varname>);
%   ").
%
%   :- pragma foreign_proc("C",
%       lock_<varname>,
%       [will_not_call_mercury, promise_pure],
%   "
%       /* blank */
%   ").
%
%   :- pragma foreign_proc("C",
%       unlock_<varname>,
%       [will_not_call_mercury, promise_pure],
%   "
%       /* blank */
%   ").
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
%       impure X = <initval>,
%       impure secret_initialization_only_set_<varname>(X).
%
%-----------------------------------------------------------------------------%
%
% JAVA BACKEND
%
% For non-constant mutables the transformation is as follows:
%
%   :- mutable(<varname>, <vartype>, <initvalue>, <varinst>, [attributes]).
%
% ===>
%
%   :- pragma foreign_code("Java", "
%       static <JType> mutable_<varname>;
%   ").
%
%   :- initialise initialise_mutable_<varname>/0.
%
%   :- impure pred initialise_mutable_<varname> is det.
%
%   initialise_mutable_<varname> :-
%       impure X = <initval>,
%       impure set_<varname>(X).
%
% <JType> is either `int' or `java.lang.Object' (all other types).
%
% Operations on mutables are defined in terms of the following two predicates.
% They are actually "safe": by the Java specification, 32-bit variables are
% loaded/stored atomically. Doubles and longs may be treated as two 32-bit
% variables, but Mercury does not expose them yet. The predicates are named so
% to minimise the differences with the C backends.
%
%   :- impure pred unsafe_set_<varname>(<vartype>::in(<varinst>)) is det.
%   :- pragma foreign_proc("Java",
%       unsafe_set_<varname>(X::in(<varinst>)),
%       [will_not_call_mercury, thread_safe],
%   "
%       mutable_<varname> = X;
%   ").
%
%   :- semipure pred unsafe_get_<varname>(<vartype>::out(<varinst>)) is det.
%   :- pragma foreign_proc("Java",
%       unsafe_get_<varname>(X::out(<varinst>)),
%       [promise_semipure, will_not_call_mercury, thread_safe],
%   "
%       X = mutable_<varname>;
%   ").
%
% If mutable_<varname> has the type `java.lang.Object' a cast is required
% after the code above, to cast X to the correct type. This is handled by
% the MLDS code generator.
%
% For thread-local mutables the transformation is as follows:
%
%   :- mutable(<varname>, <vartype>, <initvalue>, <varinst>, [attributes]).
%
% ===>
%
%   :- pragma foreign_code("Java", "
%       static java.lang.ThreadLocal<JType> mutable_<varname> =
%           new java.lang.InheritableThreadLocal<JType>();
%   ").
%
%   :- pragma foreign_proc("Java",
%       unsafe_set_<varname>(X::in(<varinst>)),
%       [will_not_call_mercury, thread_safe],
%   "
%       mutable_<varname>.set(X);
%   ").
%
%   :- pragma foreign_proc("Java",
%       unsafe_get_<varname>(X::out(<varinst>)),
%       [promise_semipure, will_not_call_mercury, thread_safe],
%   "
%       X = mutable_<varname>.get();
%   ").
%
% <JType> is `java.lang.Integer' or `java.lang.Object'.
%
% The above prediates are called by these predicates, again to minimise
% differences with the C backends:
%
%   :- impure pred set_<varname>(<vartype>::in(<varinst>)) is det.
%
%   set_<varname>(X) :-
%       impure unsafe_set_<varname>(X).
%
%   :- semipure pred get_<varname>(<vartype>::out(<varinst>)) is det.
%
%   get_<varname>(X) :-
%       semipure unsafe_get_<varname>(X).
%
% For constant mutables the transformation is:
%
%   :- mutable(<varname>, <vartype>, <initvalue>, <varinst>, [constant]).
%
% ===>
%
%   :- pragma foreign_code("Java", "
%       static <JType> mutable_<varname>;
%   ").
%
%   :- pred get_<varname>(<vartype>::out(<varinst>)) is det.
%   :- pragma foreign_proc("Java",
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
%   :- pragma foreign_proc("Java",
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
%       impure X = <initval>,
%       impure secret_initialization_only_set_<varname>(X).
%
%-----------------------------------------------------------------------------%
%
% C# BACKEND
%
% The C# implementation is analogous to the Java implementation, except for
% thread-local mutables, which are transformed as follows:
%
%   :- mutable(<varname>, <vartype>, <initvalue>, <varinst>, [attributes]).
%
% ===>
%
%   :- pragma foreign_code("C#", "
%       private static int mutable_<varname>;
%   ").
%
%   :- initialise initialise_mutable_<varname>/0.
%
%   :- impure pred initialise_mutable_<varname> is det.
%
%   initialise_mutable_<varname> :-
%       impure pre_initialise_mutable_<varname>,
%       impure X = <initvalue>,
%       impure set_<varname>(X).
%
%   :- pragma foreign_proc("C#",
%       pre_initialise_mutable_<varname>,
%       [will_not_call_mercury],
%   "
%       mutable_<varname> = runtime.ThreadLocalMutables.new_index();
%   ").
%
%   :- pragma foreign_proc("C#",
%       unsafe_set_<varname>(X::in(<varinst>)),
%       [will_not_call_mercury, thread_safe],
%   "
%       runtime.ThreadLocalMutables.set(mutable_<varname>, X);
%   ").
%
%   :- pragma foreign_proc("C#",
%       unsafe_get_<varname>(X::out(<varinst>)),
%       [promise_semipure, will_not_call_mercury, thread_safe],
%   "
%       X = runtime.ThreadLocalMutables.get(mutable_<varname>);
%   ").
%
%-----------------------------------------------------------------------------%
%
% ERLANG BACKEND
%
% Every Erlang "process" has an associated process dictionary, which we can use
% to implement mutables. However, since a process dictionary is local to the
% process, it would not work (in multi-process/multi-threaded programs) to just
% have each process work with its own process dictionary. Therefore, at
% initialisation time we start up a global server process to hold the mutable
% values. Other processes can get and set mutables by communicating messages
% with this global server.
%
% In the transformations below, <varname> is a key derived from the name of the
% mutable and the module name. The module name must be included.
%
% For non-constant mutables:
%
%   :- mutable(<varname>, <vartype>, <initvalue>, <varinst>, [attributes]).
%
% ===>
%
%   :- initialise initialise_mutable_<varname>/0.
%
%   :- impure pred initialise_mutable_<varname> is det.
%
%   initialise_mutable_<varname> :-
%       impure X = <initval>,
%       impure set_<varname>(X).
%
%   :- impure pred set_<varname>(<vartype>::in(<varinst>)) is det.
%   :- pragma foreign_proc("Erlang",
%       set_<varname>(X::in(<varinst>)),
%       [will_not_call_mercury, thread_safe],
%   "
%       'ML_erlang_global_server' ! {set_mutable, <varname>, X}
%   ").
%
%   :- semipure pred get_<varname>(<vartype>::out(<varinst>)) is det.
%   :- pragma foreign_proc("Erlang",
%       get_<varname>(X::out(<varinst>)),
%       [promise_semipure, will_not_call_mercury, thread_safe],
%   "
%       'ML_erlang_global_server' ! {get_mutable, <varname>, self()}},
%       receive
%           {get_mutable_ack, Value} ->
%               X = value
%       end
%   ").
%
% For constant mutables:
%
%   :- mutable(<varname>, <vartype>, <initvalue>, <varinst>, [constant]).
%
% ===>
%
%   :- pred get_<varname>(<vartype>::out(<varinst>)) is det.
%   :- pragma foreign_proc("Erlang",
%       get_<varname>(X::out(<varinst>)),
%       [will_not_call_mercury, promise_pure, thread_safe],
%   "
%       'ML_erlang_global_server' ! {get_mutable, <varname>, self()}},
%       receive
%           {get_mutable_ack, Value} ->
%               X = value
%       end
%   ").
%
% In order to initialise constant mutables we generate the following:
%
%   :- impure pred secret_initialization_only_set_<varname>(
%       <vartype>::in(<varinst>)) is det.
%
%   :- pragma foreign_proc("Erlang",
%       secret_initialization_only_set_<varname>(X::in(<varinst>)),
%       [will_not_call_mercury],
%   "
%       'ML_erlang_global_server' ! {set_mutable, <varname>, X}
%   ").
%
%   :- initialise initialise_mutable_<varname>/0.
%
%   :- impure pred initialise_mutable_<varname> is det.
%
%   initialise_mutable_<varname> :-
%       impure X = <initval>,
%       impure secret_initialization_only_set_<varname>(X).
%
% The transformation for thread_local mutables has not been decided (we need a
% way for spawned processes to inherit all the thread-local mutable values of
% its parent process, but a child process in Erlang does not automatically
% inherit its parent process's process dictionary).
%
% Trailed mutabled are not supported because the Erlang backend doesn't
% support trailing.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_mutable.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

%-----------------------------------------------------------------------------%

    % Names of the primitive operations.
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

:- func mutable_pre_init_pred_sym_name(module_name, string) = sym_name.

:- func mutable_c_var_name(module_name, string) = string.

    % Returns the name of the mutex associated a given mutable. The input
    % to this function is the name of the mutable in the target language,
    % i.e. it is the result of a call to mutable_c_var_name/2 or one of the
    % specified foreign names for the mutable.
    %
:- func mutable_mutex_var_name(string) = string.

%-----------------------------------------------------------------------------%

    % Create a predmode declaration for the semipure mutable get predicate.
    % (This is the default get predicate.)
    %
:- func std_get_pred_decl(module_name, string, mer_type, mer_inst,
    prog_context) = item_pred_decl_info.

    % Create a predmode declaration for the impure mutable set predicate.
    % (This is the default set predicate.)
    %
:- func std_set_pred_decl(module_name, string, mer_type, mer_inst,
    prog_context) = item_pred_decl_info.

    % Create a predmode declaration for a get predicate for a constant mutable.
    % (This is only created if the `constant' attribute is given.)
    %
:- func constant_get_pred_decl(module_name, string, mer_type, mer_inst,
    prog_context) = item_pred_decl_info.

    % Create a predmode declaration for a set predicate for a constant mutable;
    % this predicate is designed to be used only from the mutable's
    % initialization predicate.
    % (This is created only if the `constant' attribute is given.)
    %
:- func constant_set_pred_decl(module_name, string, mer_type, mer_inst,
    prog_context) = item_pred_decl_info.

    % Create a predmode declaration for a get predicate using the I/O state.
    % (This is created only if the `attach_to_io_state' attribute is given.)
    %
:- func io_get_pred_decl(module_name, string, mer_type, mer_inst,
    prog_context) = item_pred_decl_info.

    % Create a predmode declaration for a set predicate using the I/O state.
    % (This is created only if the `attach_to_io_state' attribute is given.)
    %
:- func io_set_pred_decl(module_name, string, mer_type, mer_inst,
    prog_context) = item_pred_decl_info.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_mode.

:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module varset.

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

mutable_pre_init_pred_sym_name(ModuleName, Name) =
    qualified(ModuleName, "pre_initialise_mutable_" ++ Name).

mutable_c_var_name(ModuleName, Name) = MangledCVarName :-
    RawCVarName = "mutable_variable_" ++ Name,
    QualifiedCVarName0 = qualified(ModuleName, RawCVarName),
    ( if mercury_std_library_module_name(ModuleName) then
        QualifiedCVarName =
            add_outermost_qualifier("mercury", QualifiedCVarName0)
    else
        QualifiedCVarName = QualifiedCVarName0
    ),
    MangledCVarName = sym_name_mangle(QualifiedCVarName).

mutable_mutex_var_name(TargetMutableVarName) = MutexVarName :-
    MutexVarName = TargetMutableVarName ++ "_lock".

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

std_get_pred_decl(ModuleName, Name, Type, Inst, Context) =
    make_mutable_get_pred_decl(ModuleName, Name, Type, Inst,
        get_set_pred_std, Context).

constant_get_pred_decl(ModuleName, Name, Type, Inst, Context) =
    make_mutable_get_pred_decl(ModuleName, Name, Type, Inst,
        get_set_pred_constant, Context).

io_get_pred_decl(ModuleName, Name, Type, Inst, Context) =
    make_mutable_get_pred_decl(ModuleName, Name, Type, Inst,
        get_set_pred_io, Context).

%-----------------------------------------------------------------------------%

std_set_pred_decl(ModuleName, Name, Type, Inst, Context) =
    make_mutable_set_pred_decl(ModuleName, Name, Type, Inst,
        get_set_pred_std, Context).

constant_set_pred_decl(ModuleName, Name, Type, Inst, Context) =
    make_mutable_set_pred_decl(ModuleName, Name, Type, Inst,
        get_set_pred_constant, Context).

io_set_pred_decl(ModuleName, Name, Type, Inst, Context) =
    make_mutable_set_pred_decl(ModuleName, Name, Type, Inst,
        get_set_pred_io, Context).

%-----------------------------------------------------------------------------%

:- type get_set_pred_kind
    --->    get_set_pred_std
    ;       get_set_pred_constant
    ;       get_set_pred_io.

:- func make_mutable_get_pred_decl(module_name, string, mer_type, mer_inst,
    get_set_pred_kind, prog_context) = item_pred_decl_info.

make_mutable_get_pred_decl(ModuleName, Name, Type, Inst, PredKind, Context)
        = GetPredDecl :-
    Attrs = item_compiler_attributes(do_allow_export, is_mutable),
    Origin = item_origin_compiler(Attrs),
    % XXX The real origin is ico_mutable_decl(Name, PredKind))
    TypeVarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    SymName = mutable_get_pred_sym_name(ModuleName, Name),
    MainArgTypesAndModes = [type_and_mode(Type, out_mode(Inst))],
    (
        PredKind = get_set_pred_std,
        ArgTypesAndModes = MainArgTypesAndModes,
        Purity = purity_semipure
    ;
        PredKind = get_set_pred_constant,
        ArgTypesAndModes = MainArgTypesAndModes,
        Purity = purity_pure
    ;
        PredKind = get_set_pred_io,
        ArgTypesAndModes = MainArgTypesAndModes ++ io_state_pair,
        Purity = purity_pure
    ),
    WithType = no,
    WithInst = no,
    Constraints = constraints([], []),
    GetPredDecl = item_pred_decl_info(SymName, pf_predicate, ArgTypesAndModes,
        WithType, WithInst, yes(detism_det), Origin, TypeVarSet, InstVarSet,
        ExistQVars, Purity, Constraints, Context, -1).

:- func make_mutable_set_pred_decl(module_name, string, mer_type, mer_inst,
    get_set_pred_kind, prog_context) = item_pred_decl_info.

make_mutable_set_pred_decl(ModuleName, Name, Type, Inst, PredKind, Context)
        = SetPredDecl :-
    Attrs = item_compiler_attributes(do_allow_export, is_mutable),
    Origin = item_origin_compiler(Attrs),
    % XXX The real origin is ico_mutable_decl(Name, PredKind))
    TypeVarSet = varset.init,
    InstVarSet = varset.init,
    ExistQVars = [],
    MainArgTypesAndModes = [type_and_mode(Type, in_mode(Inst))],
    (
        PredKind = get_set_pred_std,
        SymName = mutable_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = MainArgTypesAndModes,
        Purity = purity_impure
    ;
        PredKind = get_set_pred_constant,
        SymName = mutable_secret_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = MainArgTypesAndModes,
        Purity = purity_impure
    ;
        PredKind = get_set_pred_io,
        SymName = mutable_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = MainArgTypesAndModes ++ io_state_pair,
        Purity = purity_pure
    ),
    Constraints = constraints([], []),
    WithType = no,
    WithInst = no,
    SetPredDecl = item_pred_decl_info(SymName, pf_predicate, ArgTypesAndModes,
        WithType, WithInst, yes(detism_det), Origin, TypeVarSet, InstVarSet,
        ExistQVars, Purity, Constraints, Context, -1).

:- func io_state_pair = list(type_and_mode).

io_state_pair =
    [type_and_mode(io_state_type, di_mode),
    type_and_mode(io_state_type, uo_mode)].

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_mutable.
%-----------------------------------------------------------------------------%
