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

:- import_module list.

%-----------------------------------------------------------------------------%
%
% The names we construct for the auxiliary predicates of a mutable.
%

:- func mutable_lock_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_unlock_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_unsafe_get_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_unsafe_set_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_get_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_set_pred_sym_name(sym_name, string) = sym_name.
:- func mutable_secret_set_pred_sym_name(module_name, string) = sym_name.
:- func mutable_init_pred_sym_name(module_name, string) = sym_name.
:- func mutable_pre_init_pred_sym_name(module_name, string) = sym_name.

%-----------------------------------------------------------------------------%

    % This predicate decides which of the publicly visible auxiliary predicates
    % we should generate for a mutable.
    %
    % This same decisions for the private aux predicates are made by
    % compute_needed_private_mutable_aux_preds in add_mutable_aux_preds.m.
    %
:- pred compute_needed_public_mutable_aux_preds(mutable_var_attributes::in,
    list(mutable_pred_kind)::out) is det.

    % make_mutable_aux_pred_decl(ModuleName, MutableName, Type, Inst, Context,
    %   Kind, PredDecl):
    %
    % Create the predicate declaration for the given Kind of mutable auxiliry
    % predicate for a mutable with the given MutableName, which has the
    % given Type, Inst and Context.
    %
:- pred make_mutable_aux_pred_decl(module_name::in, string::in,
    mer_type::in, mer_inst::in, prog_context::in, mutable_pred_kind::in,
    item_pred_decl_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_mode.

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

compute_needed_public_mutable_aux_preds(MutAttrs, PublicAuxPreds) :-
    % The logic we use here is duplicated in define_main_get_set_preds
    % in add_mutable_aux_preds.m. The comment there explains why.
    IsConstant = mutable_var_constant(MutAttrs),
    AttachToIO = mutable_var_attach_to_io_state(MutAttrs),
    (
        IsConstant = mutable_constant,
        % We create the "get" access predicate, which is pure since
        % it always returns the same value, but we must also create
        % a secret "set" predicate for use by the initialization code.
        GetSetPreds =
            [mutable_pred_constant_get, mutable_pred_constant_secret_set]
    ;
        IsConstant = mutable_not_constant,
        % Create the standard, non-pure access predicates. These are
        % always created for non-constant mutables, even if the
        % `attach_to_io_state' attribute has been specified.
        StdGetSetPreds = [mutable_pred_std_get, mutable_pred_std_set],

        % If requested, create pure access predicates using
        % the I/O state as well.
        (
            AttachToIO = mutable_dont_attach_to_io_state,
            GetSetPreds = StdGetSetPreds
        ;
            AttachToIO = mutable_attach_to_io_state,
            IOGetSetPreds = [mutable_pred_io_get, mutable_pred_io_set],
            GetSetPreds = StdGetSetPreds ++ IOGetSetPreds
        )
    ),
    PublicAuxPreds = GetSetPreds.

%-----------------------------------------------------------------------------%

make_mutable_aux_pred_decl(ModuleName, Name, Type, Inst, Context, Kind,
        PredDecl) :-
    (
        Kind = mutable_pred_pre_init,
        PredSymName = mutable_pre_init_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [],
        AllowExport = do_not_allow_export,
        Purity = purity_impure
    ;
        Kind = mutable_pred_init,
        PredSymName = mutable_init_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [],
        AllowExport = do_not_allow_export,
        Purity = purity_impure
    ;
        Kind = mutable_pred_lock,
        PredSymName = mutable_lock_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [],
        AllowExport = do_not_allow_export,
        Purity = purity_impure
    ;
        Kind = mutable_pred_unlock,
        PredSymName = mutable_unlock_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [],
        AllowExport = do_not_allow_export,
        Purity = purity_impure
    ;
        Kind = mutable_pred_unsafe_get,
        PredSymName = mutable_unsafe_get_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, out_mode(Inst))],
        AllowExport = do_not_allow_export,
        Purity = purity_semipure
    ;
        Kind = mutable_pred_unsafe_set,
        PredSymName = mutable_unsafe_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, in_mode(Inst))],
        AllowExport = do_not_allow_export,
        Purity = purity_impure
    ;
        Kind = mutable_pred_std_get,
        PredSymName = mutable_get_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, out_mode(Inst))],
        AllowExport = do_allow_export,
        Purity = purity_semipure
    ;
        Kind = mutable_pred_std_set,
        PredSymName = mutable_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, in_mode(Inst))],
        AllowExport = do_allow_export,
        Purity = purity_impure
    ;
        Kind = mutable_pred_constant_get,
        PredSymName = mutable_get_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, out_mode(Inst))],
        AllowExport = do_allow_export,
        Purity = purity_pure
    ;
        Kind = mutable_pred_constant_secret_set,
        PredSymName = mutable_secret_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, in_mode(Inst))],
        AllowExport = do_allow_export,
        Purity = purity_impure
    ;
        Kind = mutable_pred_io_get,
        PredSymName = mutable_get_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, out_mode(Inst))]
            ++ io_state_pair,
        AllowExport = do_allow_export,
        Purity = purity_pure
    ;
        Kind = mutable_pred_io_set,
        PredSymName = mutable_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_and_mode(Type, in_mode(Inst))]
            ++ io_state_pair,
        AllowExport = do_allow_export,
        Purity = purity_pure
    ),
    WithType = maybe.no,
    WithMode = maybe.no,
    MaybeIsMutable = is_mutable(ModuleName, Name, Kind),
    CompilerAttrs = item_compiler_attributes(AllowExport, MaybeIsMutable),
    MaybeAttrs = item_origin_compiler(CompilerAttrs),
    varset.init(TypeVarSet),
    varset.init(InstVarSet),
    ExistQVars = [],
    Constraints = constraints([], []),
    SeqNum = -1,
    PredDecl = item_pred_decl_info(PredSymName, pf_predicate, ArgTypesAndModes,
        WithType, WithMode, yes(detism_det), MaybeAttrs,
        TypeVarSet, InstVarSet, ExistQVars, Purity, Constraints,
        Context, SeqNum).

:- func io_state_pair = list(type_and_mode).

io_state_pair =
    [type_and_mode(io_state_type, di_mode),
    type_and_mode(io_state_type, uo_mode)].

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_mutable.
%-----------------------------------------------------------------------------%
