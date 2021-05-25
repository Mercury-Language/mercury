%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module creates both the public predicates users use to access mutables,
% and the private auxiliary predicates needed to make that possible.
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

:- module hlds.make_hlds.add_mutable_aux_preds.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_foreign.

:- import_module list.

%---------------------------------------------------------------------------%

:- type item_fpe == item_pragma_info(pragma_info_foreign_proc_export).

    % Check the mutables that are local to this module for errors,
    % and if they are ok, construct and return
    %
    % - the predicate declarations of the auxiliary predicates they need,
    %
    % - the definitions of those auxiliary predicates, in the form of
    %   clauses (for some) or foreign procs (for the others),
    %
    % - the foreign_body_codes that contain the definitions of the
    %   global variables that contain the mutable,
    %
    % - any foreign_decl_codes that contain declarations of those
    %   global variables, if needed by the target language,
    %
    % Also, export the initialization predicates to the target language,
    % and arrange for them to be called before main/2.
    %
:- pred implement_mutables_if_local(module_info::in,
    sec_list(item_mutable_info)::in,
    sec_list(item_pred_decl_info)::out,
    ims_list(item_clause_info)::out, ims_list(item_foreign_proc)::out,
    list(foreign_decl_code)::out, list(foreign_body_code)::out,
    list(item_fpe)::out, pred_target_names::in, pred_target_names::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module hlds.error_msg_inst.
:- import_module hlds.hlds_inst_mode.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.file_names.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_mutable.

:- import_module bool.
:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type sec_cord(T) == cord(sec_sub_list(T)).

implement_mutables_if_local(ModuleInfo, SecList, PredDecls,
        ClauseInfos, ForeignProcs, ForeignDeclCodes, ForeignBodyCodes,
        FPEInfos, !PredTargetNames, !Specs) :-
    implement_mutables_loop(ModuleInfo, SecList,
        [], PredDecls, cord.init, ClauseInfoCord, cord.init, ForeignProcCord,
        cord.init, ForeignDeclCodeCord, cord.init, ForeignBodyCodeCord,
        cord.init, FPEInfoCord, !PredTargetNames, !Specs),
    ClauseInfos = cord.list(ClauseInfoCord),
    ForeignProcs = cord.list(ForeignProcCord),
    ForeignDeclCodes = cord.list(ForeignDeclCodeCord),
    ForeignBodyCodes = cord.list(ForeignBodyCodeCord),
    FPEInfos = cord.list(FPEInfoCord).

:- pred implement_mutables_loop(module_info::in,
    sec_list(item_mutable_info)::in,
    sec_list(item_pred_decl_info)::in, sec_list(item_pred_decl_info)::out,
    ims_cord(item_clause_info)::in, ims_cord(item_clause_info)::out,
    ims_cord(item_foreign_proc)::in, ims_cord(item_foreign_proc)::out,
    cord(foreign_decl_code)::in, cord(foreign_decl_code)::out,
    cord(foreign_body_code)::in, cord(foreign_body_code)::out,
    cord(item_fpe)::in, cord(item_fpe)::out,
    pred_target_names::in, pred_target_names::out,
    list(error_spec)::in, list(error_spec)::out) is det.

implement_mutables_loop(_ModuleInfo, [],
        !PredDecls, !ClauseInfoCord, !ForeignProcCord,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord,
        !FPEInfoCord, !PredTargetNames, !Specs).
implement_mutables_loop(ModuleInfo, [SecSubList | SecSubLists],
        !PredDecls, !ClauseInfoCord, !ForeignProcCord,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord,
        !FPEInfoCord, !PredTargetNames, !Specs) :-
    SecSubList = sec_sub_list(SectionInfo, ItemMutables),
    SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
    (
        ItemMercuryStatus = item_defined_in_this_module(_),
        list.foldl8(
            check_and_implement_mutable(ModuleInfo, SectionInfo),
            ItemMutables, !PredDecls,
            cord.init, ClauseInfoSubCord, cord.init, ForeignProcSubCord,
            !ForeignDeclCodeCord, !ForeignBodyCodeCord,
            !FPEInfoCord, !PredTargetNames, !Specs),
        ( if cord.is_empty(ClauseInfoSubCord) then
            true
        else
            ClauseInfoSubList =
                ims_sub_list(ItemMercuryStatus, cord.list(ClauseInfoSubCord)),
            cord.snoc(ClauseInfoSubList, !ClauseInfoCord)
        ),
        ( if cord.is_empty(ForeignProcSubCord) then
            true
        else
            ForeignProcSubList =
                ims_sub_list(ItemMercuryStatus, cord.list(ForeignProcSubCord)),
            cord.snoc(ForeignProcSubList, !ForeignProcCord)
        )
    ;
        ItemMercuryStatus = item_defined_in_other_module(_)
        % We don't implement the `mutable' declaration unless it is defined
        % in this module. If we did not have this check, we would duplicate
        % the definition of the global variable storing the mutable
        % in any submodules of the module that actually defined the mutable.
    ),
    implement_mutables_loop(ModuleInfo, SecSubLists,
        !PredDecls, !ClauseInfoCord, !ForeignProcCord,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord,
        !FPEInfoCord, !PredTargetNames, !Specs).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% XXX CLEANUP put the predicates in this module in a logical order
% in a separate diff once review is done.

:- pred check_and_implement_mutable(module_info::in,
    sec_info::in, item_mutable_info::in,
    sec_list(item_pred_decl_info)::in, sec_list(item_pred_decl_info)::out,
    cord(item_clause_info)::in, cord(item_clause_info)::out,
    cord(item_foreign_proc)::in, cord(item_foreign_proc)::out,
    cord(foreign_decl_code)::in, cord(foreign_decl_code)::out,
    cord(foreign_body_code)::in, cord(foreign_body_code)::out,
    cord(item_fpe)::in, cord(item_fpe)::out,
    pred_target_names::in, pred_target_names::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_and_implement_mutable(ModuleInfo, SecInfo, ItemMutable,
        !PredDecls, !ClauseInfoCord, !ForeignProcCord,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord,
        !FPEInfoCord, !PredTargetNames, !Specs) :-
    check_mutable(ModuleInfo, ItemMutable, !Specs),
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, Type, _OrigInst, Inst,
        _InitTerm, _VarSetMutable, MutAttrs, Context, _SeqNum),
    module_info_get_globals(ModuleInfo, Globals),
    get_mutable_target_params(Globals, MutAttrs, TargetParams),
    PublicPredKinds = TargetParams ^ mtp_public_aux_preds,
    PrivatePredKinds = TargetParams ^ mtp_private_aux_preds,
    NeededPredKinds = PublicPredKinds ++ PrivatePredKinds,
    module_info_get_name(ModuleInfo, ModuleName),
    list.map(
        make_mutable_aux_pred_decl(ModuleName, MutableName, Type, Inst,
            Context),
        NeededPredKinds, NeededPredDecls),
    !:PredDecls = [sec_sub_list(SecInfo, NeededPredDecls) | !.PredDecls],
    add_aux_pred_defns_for_mutable(ModuleInfo, ItemMutable,
        !ClauseInfoCord, !ForeignProcCord,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord,
        !FPEInfoCord, !PredTargetNames).

%---------------------------------------------------------------------------%

:- pred add_aux_pred_defns_for_mutable(module_info::in, item_mutable_info::in,
    cord(item_clause_info)::in, cord(item_clause_info)::out,
    cord(item_foreign_proc)::in, cord(item_foreign_proc)::out,
    cord(foreign_decl_code)::in, cord(foreign_decl_code)::out,
    cord(foreign_body_code)::in, cord(foreign_body_code)::out,
    cord(item_fpe)::in, cord(item_fpe)::out,
    pred_target_names::in, pred_target_names::out) is det.

add_aux_pred_defns_for_mutable(ModuleInfo, ItemMutable,
        !ClauseInfoCord, !ForeignProcCord,
        !ForeignDeclCodes, !ForeignBodyCodes,
        !FPEInfoCord, !PredTargetNames) :-
    % The transformation here is documented in the comments at the
    % beginning of prog_mutable.m.
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, Type, _OrigInst, _Inst,
        _InitTerm, _VarSetMutable, MutAttrs, Context, _SeqNum),
    module_info_get_globals(ModuleInfo, Globals),
    get_mutable_target_params(Globals, MutAttrs, TargetParams),
    Lang = TargetParams ^ mtp_target_lang,

    MutAttrs = mutable_var_attributes(_LangMap, Const),

    % Work out what name to give the global in the target language.
    module_info_get_name(ModuleInfo, ModuleName),
    decide_mutable_target_var_name(ModuleName, MutableName, MutAttrs,
        Lang, TargetMutableName),
    % We need ModuleInfo after all type definitions have been processed
    % as an input, because the target-language-specific name of the type
    % of the global variable we are defining here depends on whether
    % there are any foreign_type declarations for Type.
    (
        Lang = lang_c,
        define_mutable_global_var_c(ModuleInfo, TargetMutableName, Type,
            Const, Context, !ForeignDeclCodes, !ForeignBodyCodes)
    ;
        Lang = lang_csharp,
        define_mutable_global_var_csharp(TargetMutableName, Type,
            Const, Context, !ForeignBodyCodes)
    ;
        Lang = lang_java,
        define_mutable_global_var_java(TargetMutableName, Type,
            Const, Context, !ForeignBodyCodes)
    ),

    define_aux_preds_for_mutable(ModuleInfo, TargetParams, ItemMutable,
        TargetMutableName, ClauseInfos, ForeignProcs, FPEInfo,
        !PredTargetNames),
    !:ClauseInfoCord = !.ClauseInfoCord ++ cord.from_list(ClauseInfos),
    !:ForeignProcCord = !.ForeignProcCord ++ cord.from_list(ForeignProcs),
    cord.snoc(FPEInfo, !FPEInfoCord).

%---------------------------------------------------------------------------%
%
% Define the global holding the mutable.
%

    % Define the global variable used to hold the mutable on the C backend,
    % and if needed, the mutex controlling access to it.
    %
:- pred define_mutable_global_var_c(module_info::in, string::in, mer_type::in,
    mutable_maybe_constant::in, prog_context::in,
    cord(foreign_decl_code)::in, cord(foreign_decl_code)::out,
    cord(foreign_body_code)::in, cord(foreign_body_code)::out) is det.

define_mutable_global_var_c(ModuleInfo, TargetMutableName, Type,
        Const, Context, !ForeignDeclCodes, !ForeignBodyCodes) :-
    % The declaration we construct will be included in the .mh files. Since
    % these are grade independent, we need to output both the high- and
    % low-level C declarations for the global used to implement the mutable,
    % and make the choice conditional on whether MR_HIGHLEVEL_CODE is defined.
    IsThreadLocal = mutable_var_thread_local(Const),
    (
        IsThreadLocal = mutable_not_thread_local,
        % The only difference between the high- and low-level C backends
        % is that in the latter, mutables are *always* boxed, whereas
        % in the former they may not be.
        HighLevelTypeName = global_foreign_type_name(ModuleInfo,
            bp_native_if_possible, lang_c, Type),
        LowLevelTypeName = global_foreign_type_name(ModuleInfo,
            bp_always_boxed, lang_c, Type),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        (
            HighLevelCode = no,
            TypeName = LowLevelTypeName
        ;
            HighLevelCode = yes,
            TypeName = HighLevelTypeName
        )
    ;
        IsThreadLocal = mutable_thread_local,
        % For thread-local mutables, the variable holds an index into an array.
        TypeName = "MR_Unsigned",
        HighLevelTypeName = TypeName,
        LowLevelTypeName  = TypeName
    ),

    % Constant mutables do not require mutexes, as their values are never
    % updated. Thread-local mutables do not require mutexes either.
    ( if
        ( Const = mutable_is_constant
        ; IsThreadLocal = mutable_thread_local
        )
    then
        LockDeclStrs = [],
        LockDefnStrs = []
    else
        MutexVarName = mutable_mutex_var_name(TargetMutableName),
        LockDeclStrs = [
            "#ifdef MR_THREAD_SAFE\n",
            "    extern MercuryLock ", MutexVarName, ";\n",
            "#endif\n"
        ],
        LockDefnStrs = [
            "#ifdef MR_THREAD_SAFE\n",
            "    MercuryLock ", MutexVarName, ";\n",
            "#endif\n"
        ]
    ),

    DeclBody = string.append_list([
        "#ifdef MR_HIGHLEVEL_CODE\n",
        "    extern ", HighLevelTypeName, " ", TargetMutableName, ";\n",
        "#else\n",
        "    extern ", LowLevelTypeName, " ", TargetMutableName, ";\n",
        "#endif\n" | LockDeclStrs]),
    ForeignDeclCode = foreign_decl_code(lang_c, foreign_decl_is_exported,
        floi_literal(DeclBody), Context),
    cord.snoc(ForeignDeclCode, !ForeignDeclCodes),

    DefnBody = string.append_list([
        TypeName, " ", TargetMutableName, ";\n" | LockDefnStrs]),
    ForeignBodyCode = foreign_body_code(lang_c,
        floi_literal(DefnBody), Context),
    cord.snoc(ForeignBodyCode, !ForeignBodyCodes).

    % Define the global variable used to hold the mutable on the C# backend.
    %
:- pred define_mutable_global_var_csharp(string::in, mer_type::in,
    mutable_maybe_constant::in, prog_context::in,
    cord(foreign_body_code)::in, cord(foreign_body_code)::out) is det.

define_mutable_global_var_csharp(TargetMutableName, Type, Const, Context,
        !ForeignBodyCodes) :-
    IsThreadLocal = mutable_var_thread_local(Const),
    (
        IsThreadLocal = mutable_not_thread_local,
        ( if Type = int_type then
            TypeStr = "int"
        else
            TypeStr = "object"
        )
    ;
        IsThreadLocal = mutable_thread_local,
        TypeStr = "int"
    ),
    DefnBody = "static " ++ TypeStr ++ " " ++ TargetMutableName ++ ";\n",
    DefnForeignBodyCode =
        foreign_body_code(lang_csharp, floi_literal(DefnBody), Context),
    cord.snoc(DefnForeignBodyCode, !ForeignBodyCodes).

    % Define the global variable used to hold the mutable on the Java backend.
    %
:- pred define_mutable_global_var_java(string::in, mer_type::in,
    mutable_maybe_constant::in, prog_context::in,
    cord(foreign_body_code)::in, cord(foreign_body_code)::out) is det.

define_mutable_global_var_java(TargetMutableName, Type, Const, Context,
        !ForeignBodyCodes) :-
    IsThreadLocal = mutable_var_thread_local(Const),
    (
        IsThreadLocal = mutable_not_thread_local,
        % Synchronization is only required for double and long values,
        % which Mercury does not expose. We could also use the volatile
        % keyword. (Java Language Specification, 2nd Ed., 17.4).
        ( if Type = int_type then
            TypeStr = "int"
        else
            TypeStr = "java.lang.Object"
        ),
        DefnBody = "static " ++ TypeStr ++ " " ++ TargetMutableName ++ ";\n"
    ;
        IsThreadLocal = mutable_thread_local,
        ( if Type = int_type then
            TypeStr = "java.lang.Integer"
        else
            TypeStr = "java.lang.Object"
        ),
        DefnBody = string.append_list([
            "static java.lang.ThreadLocal<", TypeStr, "> ",
            TargetMutableName,
            " = new java.lang.InheritableThreadLocal<", TypeStr, ">();\n"
        ])
    ),
    DefnForeignBodyCode =
        foreign_body_code(lang_java, floi_literal(DefnBody), Context),
    cord.snoc(DefnForeignBodyCode, !ForeignBodyCodes).

%---------------------------------------------------------------------------%

:- pred define_aux_preds_for_mutable(module_info::in,
    mutable_target_params::in, item_mutable_info::in, string::in,
    list(item_clause_info)::out, list(item_foreign_proc)::out,
    item_pragma_info(pragma_info_foreign_proc_export)::out,
    pred_target_names::in, pred_target_names::out) is det.

define_aux_preds_for_mutable(ModuleInfo, TargetParams, ItemMutable,
        TargetMutableName, ClauseInfos, ForeignProcs, PragmaFPEInfo,
        !PredTargetNames) :-
    TargetParams = mutable_target_params(Lang, BoxPolicy,
        _PreInit, _LockUnlock, _UnsafeAccess,
        PrivatePredKinds, PublicPredKinds),
    NeededPredKinds = PublicPredKinds ++ PrivatePredKinds,

    % Set up the default attributes for the foreign_procs used for the
    % access predicates.
    Attrs0 = default_attributes(Lang),
    (
        Lang = lang_c,
        set_box_policy(BoxPolicy, Attrs0, Attrs1),
        set_may_call_mercury(proc_will_not_call_mercury, Attrs1, Attrs)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        ),
        % The mutable variable name is not module-qualified, and so
        % it must not be exported to `.opt' files. We could add the
        % qualification but it would be better to move the mutable code
        % generation into the backends first.
        % (Would we really want to opt-export mutable variables anyway?)
        set_may_export_body(yes(proc_may_not_export_body), Attrs0, Attrs)
    ),

    % The logic of this code should match the logic of
    % add_mutable_aux_pred_decls, though there is one difference of order:
    % we define the init predicate last, though it is declared second
    % just after the pre_init predicate. This is because the definition
    % needs information we gather during the definition of the other
    % predicates.

    some [!PredKinds]
    (
        module_info_get_name(ModuleInfo, ModuleName),
        set.list_to_set(NeededPredKinds, !:PredKinds),
        ( if set.remove(mutable_pred_pre_init, !PredKinds) then
            define_pre_init_pred(ModuleName, TargetParams, ItemMutable,
                TargetMutableName, Attrs, CallPreInitExpr, PreInitForeignProc),
            MaybeCallPreInitExpr = yes(CallPreInitExpr),
            PreInitForeignProcs = [PreInitForeignProc]
        else
            MaybeCallPreInitExpr = no,
            PreInitForeignProcs = []
        ),
        ( if
            set.remove(mutable_pred_lock, !PredKinds),
            set.remove(mutable_pred_unlock, !PredKinds)
        then
            define_lock_unlock_preds(ModuleName, TargetParams, ItemMutable,
                TargetMutableName, Attrs, LockUnlockExprs,
                LockUnlockForeignProcs),
            MaybeLockUnlockExprs = yes(LockUnlockExprs)
        else
            MaybeLockUnlockExprs = no,
            LockUnlockForeignProcs = []
        ),
        ( if
            set.remove(mutable_pred_unsafe_get, !PredKinds),
            set.remove(mutable_pred_unsafe_set, !PredKinds)
        then
            define_unsafe_get_set_preds(ModuleInfo, TargetParams,
                ItemMutable, TargetMutableName, Attrs,
                UnsafeGetSetExprs, UnsafeGetSetForeignProcs),
            MaybeUnsafeGetSetExprs = yes(UnsafeGetSetExprs)
        else
            MaybeUnsafeGetSetExprs = no,
            UnsafeGetSetForeignProcs = []
        ),

        % We do this after defining (a) the lock and unlock predicates and
        % (b) the unsafe get and set predicates, since they give us
        % (a) MaybeLockUnlockExprs and (b) MaybeUnsafeGetSetExprs respectively.
        define_main_get_set_preds(ModuleName, TargetParams, ItemMutable,
            TargetMutableName, Attrs, MaybeLockUnlockExprs,
            MaybeUnsafeGetSetExprs, InitSetPredName,
            GetSetClauseInfos, GetSetForeignProcs, !PredKinds),

        ( if set.remove(mutable_pred_init, !PredKinds) then
            % We do this after defining (a) the preinit predicate and
            % (b) the main get and set predicates, since they give us
            % (a) MaybeCallPreInitExpr and (b) InitSetPredName respectively.
            define_init_pred(ModuleName, Lang, ItemMutable, InitSetPredName,
                MaybeCallPreInitExpr, InitClauseInfo, PragmaFPEInfo,
                !PredTargetNames)
        else
            unexpected($pred, "mutable does not need init predicate")
        ),

        expect(set.is_empty(!.PredKinds), $pred, "!.PredKinds is not empty"),

        ClauseInfos = GetSetClauseInfos ++ [InitClauseInfo],
        ForeignProcs = PreInitForeignProcs ++ LockUnlockForeignProcs ++
            GetSetForeignProcs ++ UnsafeGetSetForeignProcs
    ).

    % Define the pre_init predicates, if needed by the init predicate.
    %
:- pred define_pre_init_pred(module_name::in, mutable_target_params::in,
    item_mutable_info::in, string::in, pragma_foreign_proc_attributes::in,
    goal::out, item_foreign_proc::out) is det.

define_pre_init_pred(ModuleName, TargetParams, ItemMutable, TargetMutableName,
        Attrs, CallPreInitExpr, ForeignProc) :-
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, _Type, _OrigInst, _Inst,
        _InitTerm, _VarSetMutable, MutAttrs, Context, _SeqNum),
    MutAttrs = mutable_var_attributes(_LangMap, Const),
    (
        Const = mutable_is_not_constant(_, _)
    ;
        Const = mutable_is_constant,
        unexpected($pred, "need_pre_init_pred, but mutable_is_constant")
    ),
    IsThreadLocal = mutable_var_thread_local(Const),
    Lang = TargetParams ^ mtp_target_lang,
    PreInitPredName = mutable_pre_init_pred_sym_name(ModuleName, MutableName),
    (
        Lang = lang_c,
        (
            IsThreadLocal = mutable_not_thread_local,
            PreInitCode = string.append_list([
                "#ifdef MR_THREAD_SAFE\n",
                "   pthread_mutex_init(&",
                        mutable_mutex_var_name(TargetMutableName),
                        ", MR_MUTEX_ATTR);\n",
                "#endif\n"
            ])
        ;
            IsThreadLocal = mutable_thread_local,
            PreInitCode = TargetMutableName ++
                " = MR_new_thread_local_mutable_index();\n"
        )
    ;
        Lang = lang_csharp,
        PreInitCode = TargetMutableName ++
            " = runtime.ThreadLocalMutables.new_index();\n"
    ;
        Lang = lang_java,
        unexpected($pred, "preinit for java")
    ),
    PreInitFCInfo = pragma_info_foreign_proc(Attrs,
        PreInitPredName,
        pf_predicate,
        [],             % Args
        varset.init,    % ProgVarSet
        varset.init,    % InstVarSet
        fp_impl_ordinary(PreInitCode, yes(Context))
    ),
    ForeignProc =
        item_pragma_info(PreInitFCInfo, Context, item_no_seq_num),
    CallPreInitExpr = call_expr(Context, PreInitPredName, [], purity_impure).

    % Define the lock and unlock predicates, if needed.
    %
:- pred define_lock_unlock_preds(module_name::in, mutable_target_params::in,
    item_mutable_info::in, string::in, pragma_foreign_proc_attributes::in,
    {goal, goal}::out, list(item_foreign_proc)::out) is det.

define_lock_unlock_preds(ModuleName, TargetParams, ItemMutable,
        TargetMutableName, Attrs, LockUnlockExprs, ForeignProcs) :-
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, _Type, _OrigInst, _Inst,
        _InitTerm, _VarSetMutable, MutAttrs, Context, _SeqNum),
    MutAttrs = mutable_var_attributes(_LangMap, Const),
    (
        Const = mutable_is_not_constant(_, _)
    ;
        Const = mutable_is_constant,
        unexpected($pred, "need_lock_unlock_preds, but mutable_is_constant")
    ),
    IsThreadLocal = mutable_var_thread_local(Const),
    Lang = TargetParams ^ mtp_target_lang,
    (
        Lang = lang_c,
        set_thread_safe(proc_thread_safe, Attrs, LockAndUnlockAttrs),
        MutableMutexVarName = mutable_mutex_var_name(TargetMutableName),
        (
            IsThreadLocal = mutable_not_thread_local,
            % XXX The second argument of both calls should be the name of
            % the Mercury predicate, with chars escaped as appropriate.
            LockForeignProcBody = string.append_list([
                "#ifdef MR_THREAD_SAFE\n",
                "  MR_LOCK(&" ++ MutableMutexVarName ++ ",
                    \"" ++ MutableMutexVarName ++ "\");\n" ++
                "#endif\n"
            ]),
            UnlockForeignProcBody = string.append_list([
                "#ifdef MR_THREAD_SAFE\n",
                "  MR_UNLOCK(&" ++ MutableMutexVarName ++ ",
                    \"" ++ MutableMutexVarName ++ "\");\n" ++
                "#endif\n"
            ])
        ;
            IsThreadLocal = mutable_thread_local,
            LockForeignProcBody = "",
            UnlockForeignProcBody = ""
        ),
        LockPredName =
            mutable_lock_pred_sym_name(ModuleName, MutableName),
        UnlockPredName =
            mutable_unlock_pred_sym_name(ModuleName, MutableName),
        LockFCInfo = pragma_info_foreign_proc(LockAndUnlockAttrs,
            LockPredName,
            pf_predicate,
            [],
            varset.init,    % ProgVarSet
            varset.init,    % InstVarSet
            fp_impl_ordinary(LockForeignProcBody, yes(Context))
        ),
        UnlockFCInfo = pragma_info_foreign_proc(LockAndUnlockAttrs,
            UnlockPredName,
            pf_predicate,
            [],
            varset.init,    % ProgVarSet
            varset.init,    % InstVarSet
            fp_impl_ordinary(UnlockForeignProcBody, yes(Context))
        ),
        LockForeignProc =
            item_pragma_info(LockFCInfo, Context, item_no_seq_num),
        UnlockForeignProc =
            item_pragma_info(UnlockFCInfo, Context, item_no_seq_num),
        ForeignProcs = [LockForeignProc, UnlockForeignProc],
        CallLockExpr0 =
            call_expr(Context, LockPredName, [], purity_impure),
        CallUnlockExpr0 =
            call_expr(Context, UnlockPredName, [], purity_impure),
        LockUnlockExprs = {CallLockExpr0, CallUnlockExpr0}
    ;
        Lang = lang_csharp,
        unexpected($pred, "lock_unlock for csharp")
    ;
        Lang = lang_java,
        unexpected($pred, "lock_unlock for java")
    ).

    % Define the unsafe get and set predicates, if needed.
    %
:- pred define_unsafe_get_set_preds(module_info::in, mutable_target_params::in,
    item_mutable_info::in, string::in, pragma_foreign_proc_attributes::in,
    {goal, goal}::out, list(item_foreign_proc)::out) is det.

define_unsafe_get_set_preds(ModuleInfo, TargetParams, ItemMutable,
        TargetMutableName, Attrs, UnsafeGetSetExprs, ForeignProcs) :-
    module_info_get_name(ModuleInfo, ModuleName),
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, Type, _OrigInst, Inst,
        _InitTerm, _VarSetMutable, MutAttrs, Context, _SeqNum),
    MutAttrs = mutable_var_attributes(_LangMap, Const),
    (
        Const = mutable_is_not_constant(_, _)
    ;
        Const = mutable_is_constant,
        unexpected($pred,
            "need_unsafe_get_set_preds, but mutable_is_constant")
    ),
    IsThreadLocal = mutable_var_thread_local(Const),
    Lang = TargetParams ^ mtp_target_lang,
    BoxPolicy = TargetParams ^ mtp_box_policy,
    varset.new_named_var("X", X, varset.init, VarSetOnlyX),

    set_thread_safe(proc_thread_safe, Attrs, ThreadSafeAttrs),
    set_purity(purity_semipure, ThreadSafeAttrs, UnsafeGetAttrs),
    UnsafeSetAttrs = ThreadSafeAttrs,   % defaults to purity_impure

    (
        Lang = lang_c,
        TrailedMutable = mutable_var_trailed(Const),
        (
            TrailedMutable = mutable_untrailed,
            TrailCode = ""
        ;
            TrailedMutable = mutable_trailed,
            % We have already checked that we are in a
            % trailing grade.
            TrailCode = "MR_trail_current_value(&" ++
                TargetMutableName ++ ");\n"
        ),
        (
            IsThreadLocal = mutable_not_thread_local,
            UnsafeGetCode = "X = " ++ TargetMutableName ++ ";\n",
            UnsafeSetCode = TargetMutableName ++ " = X;\n"
        ;
            IsThreadLocal = mutable_thread_local,
            TypeName =
                global_foreign_type_name(ModuleInfo, BoxPolicy, Lang, Type),
            UnsafeGetCode = "MR_get_thread_local_mutable(" ++
                TypeName ++ ", X, " ++ TargetMutableName ++ ");\n",
            UnsafeSetCode = "MR_set_thread_local_mutable(" ++
                TypeName ++ ", X, " ++ TargetMutableName ++ ");\n"
        )
    ;
        Lang = lang_csharp,
        % We generate an error for trailed mutables in pass 2, but we
        % still continue on to pass 3 even in the presence of such errors.
        TrailCode = "",
        (
            IsThreadLocal = mutable_not_thread_local,
            UnsafeGetCode = "\tX = " ++ TargetMutableName ++ ";\n",
            UnsafeSetCode = "\t" ++ TargetMutableName ++ " = X;\n"
        ;
            IsThreadLocal = mutable_thread_local,
            ( if Type = int_type then
                Cast = "(int) "
            else
                Cast = ""
            ),
            UnsafeGetCode = "\tX = " ++ Cast ++
                "runtime.ThreadLocalMutables.get(" ++
                TargetMutableName ++ ");\n",
            UnsafeSetCode = "\truntime.ThreadLocalMutables.set(" ++
                TargetMutableName ++ ", X);\n"
        )
    ;
        Lang = lang_java,
        % We generate an error for trailed mutables in pass 2, but we
        % still continue on to pass 3 even in the presence of such errors.
        TrailCode = "",
        (
            IsThreadLocal = mutable_not_thread_local,
            UnsafeGetCode = "\tX = " ++ TargetMutableName ++ ";\n",
            UnsafeSetCode = "\t" ++ TargetMutableName ++ " = X;\n"
        ;
            IsThreadLocal = mutable_thread_local,
            UnsafeGetCode = "\tX = " ++ TargetMutableName ++ ".get();\n",
            UnsafeSetCode = "\t" ++ TargetMutableName ++ ".set(X);\n"
        )
    ),

    UnsafeGetPredName =
        mutable_unsafe_get_pred_sym_name(ModuleName, MutableName),
    UnsafeSetPredName =
        mutable_unsafe_set_pred_sym_name(ModuleName, MutableName),
    UnsafeGetFCInfo = pragma_info_foreign_proc(UnsafeGetAttrs,
        UnsafeGetPredName,
        pf_predicate,
        [pragma_var(X, "X", out_mode(Inst), BoxPolicy)],
        VarSetOnlyX,    % ProgVarSet
        varset.init,    % InstVarSet
        fp_impl_ordinary(UnsafeGetCode, yes(Context))
    ),
    UnsafeSetFCInfo = pragma_info_foreign_proc(UnsafeSetAttrs,
        UnsafeSetPredName,
        pf_predicate,
        [pragma_var(X, "X", in_mode(Inst), BoxPolicy)],
        VarSetOnlyX,    % ProgVarSet
        varset.init,    % InstVarSet
        fp_impl_ordinary(TrailCode ++ UnsafeSetCode, yes(Context))
    ),
    UnsafeGetForeignProc =
        item_pragma_info(UnsafeGetFCInfo, Context, item_no_seq_num),
    UnsafeSetForeignProc =
        item_pragma_info(UnsafeSetFCInfo, Context, item_no_seq_num),
    ForeignProcs = [UnsafeGetForeignProc, UnsafeSetForeignProc],

    CallUnsafeGetExpr0 = call_expr(Context, UnsafeGetPredName,
        [variable(X, Context)], purity_semipure),
    CallUnsafeSetExpr0 = call_expr(Context, UnsafeSetPredName,
        [variable(X, Context)], purity_impure),
    UnsafeGetSetExprs = {CallUnsafeGetExpr0, CallUnsafeSetExpr0}.

    % Define one of the following sets of predicates:
    %
    % 1: the standard get predicate and the constant set predicate; or
    % 2: the standard get and set predicates; or
    % 3: the standard get and set predicates and the io get and set predicate.
    %
    % We define set 1 if the mutable is constant, and one of 2 or 3
    % if it is not, depending on whether the mutable is attached to
    % the I/O state.
    %
    % We do this *after* creating the lock and unlock predicates
    % and the unsafe get and set predicates, since they give us
    % MaybeLockUnlockExprs and MaybeUnsafeGetSetExprs.
    %
:- pred define_main_get_set_preds(module_name::in, mutable_target_params::in,
    item_mutable_info::in, string::in, pragma_foreign_proc_attributes::in,
    maybe({goal, goal})::in, maybe({goal, goal})::in, sym_name::out,
    list(item_clause_info)::out, list(item_foreign_proc)::out,
    set(mutable_pred_kind)::in, set(mutable_pred_kind)::out) is det.

define_main_get_set_preds(ModuleName, TargetParams, ItemMutable,
        TargetMutableName, Attrs, MaybeLockUnlockExprs, MaybeUnsafeGetSetExprs,
        InitSetPredName, ClauseInfos, ForeignProcs, !PredKinds) :-
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, _Type, _OrigInst, Inst,
        _InitTerm, _VarSetMutable, MutAttrs, Context, _SeqNum),
    Lang = TargetParams ^ mtp_target_lang,
    BoxPolicy = TargetParams ^ mtp_box_policy,
    varset.new_named_var("X", X, varset.init, VarSetOnlyX),

    % Due to the nontrivial flows of information between the code pieces
    % that construct the code of each aux pred, this code duplicates
    % the logic of compute_needed_public_mutable_aux_preds. We ensure that we
    % define the same predicates that compute_needed_public_mutable_aux_preds
    % has said we need (which is also the set that gets declared) by
    %
    % - checking that the aux predicates we define here are in the set of
    %   mutable_pred_kinds computed by compute_needed_public_mutable_aux_preds,
    %   and then
    % - having our caller check that there are no mutable_pred_kinds that
    %   it says we need but which we have *not* defined.
    MutAttrs = mutable_var_attributes(_LangMap, Const),
    (
        Const = mutable_is_constant,
        ConstantGetPredName =
            mutable_get_pred_sym_name(ModuleName, MutableName),
        ConstantSecretSetPredName =
            mutable_secret_set_pred_sym_name(ModuleName, MutableName),
        InitSetPredName = ConstantSecretSetPredName,

        set_purity(purity_pure, Attrs, ConstantGetAttrs0),
        set_thread_safe(proc_thread_safe,
            ConstantGetAttrs0, ConstantGetAttrs),
        ConstantSetAttrs = Attrs,
        (
            ( Lang = lang_c
            ; Lang = lang_csharp
            ; Lang = lang_java
            ),
            ConstantGetCode = "X = " ++ TargetMutableName ++ ";\n",
            ConstantSetCode = TargetMutableName ++ " = X;\n"
        ),
        ConstantGetFCInfo = pragma_info_foreign_proc(ConstantGetAttrs,
            ConstantGetPredName,
            pf_predicate,
            [pragma_var(X, "X", out_mode(Inst), BoxPolicy)],
            VarSetOnlyX,    % ProgVarSet
            varset.init,    % InstVarSet
            fp_impl_ordinary(ConstantGetCode, yes(Context))
        ),
        % NOTE: we don't need to trail the set action, since it is
        % executed only once at initialization time.
        ConstantSetFCInfo = pragma_info_foreign_proc(ConstantSetAttrs,
            ConstantSecretSetPredName,
            pf_predicate,
            [pragma_var(X, "X", in_mode(Inst), BoxPolicy)],
            VarSetOnlyX,    % ProgVarSet
            varset.init,    % InstVarSet
            fp_impl_ordinary(ConstantSetCode, yes(Context))
        ),
        ConstantGetForeignProc =
            item_pragma_info(ConstantGetFCInfo, Context, item_no_seq_num),
        ConstantSetForeignProc =
            item_pragma_info(ConstantSetFCInfo, Context, item_no_seq_num),
        ClauseInfos = [],
        ForeignProcs = [ConstantGetForeignProc, ConstantSetForeignProc],
        set.det_remove(mutable_pred_constant_get, !PredKinds),
        set.det_remove(mutable_pred_constant_secret_set, !PredKinds)
    ;
        Const = mutable_is_not_constant(AttachToIO, _Trail),
        StdGetPredName = mutable_get_pred_sym_name(ModuleName, MutableName),
        StdSetPredName = mutable_set_pred_sym_name(ModuleName, MutableName),
        InitSetPredName = StdSetPredName,
        (
            ( Lang = lang_c
            ; Lang = lang_csharp
            ; Lang = lang_java
            ),
            (
                MaybeUnsafeGetSetExprs =
                    yes({CallUnsafeGetExpr, CallUnsafeSetExpr})
            ;
                MaybeUnsafeGetSetExprs = no,
                unexpected($pred,
                    "mutable_not_constant but MaybeUnsafeGetSetExprs = no")
            ),
            (
                MaybeLockUnlockExprs = no,
                ImpureGetExpr = CallUnsafeGetExpr,
                ImpureSetExpr = CallUnsafeSetExpr
            ;
                MaybeLockUnlockExprs = yes({CallLockExpr, CallUnlockExpr}),
                ImpureGetExpr = conj_expr(Context, CallLockExpr,
                    conj_expr(Context, CallUnsafeGetExpr, CallUnlockExpr)),
                ImpureSetExpr = conj_expr(Context, CallLockExpr,
                    conj_expr(Context, CallUnsafeSetExpr, CallUnlockExpr))
            ),

            StdPredArgs = [variable(X, Context)],
            StdGetPredExpr = promise_purity_expr(Context, purity_semipure,
                ImpureGetExpr),
            StdSetPredExpr = ImpureSetExpr,

            StdGetClauseInfo = item_clause_info(pf_predicate, StdGetPredName,
                StdPredArgs, VarSetOnlyX, ok2(StdGetPredExpr, []),
                Context, item_no_seq_num),
            StdSetClauseInfo = item_clause_info(pf_predicate, StdSetPredName,
                StdPredArgs, VarSetOnlyX, ok2(StdSetPredExpr, []),
                Context, item_no_seq_num),
            StdClauseInfos = [StdGetClauseInfo, StdSetClauseInfo],
            set.det_remove(mutable_pred_std_get, !PredKinds),
            set.det_remove(mutable_pred_std_set, !PredKinds)
        ),
        (
            AttachToIO = mutable_dont_attach_to_io_state,
            ClauseInfos = StdClauseInfos
        ;
            AttachToIO = mutable_attach_to_io_state,
            some [!VarSet] (
                !:VarSet = VarSetOnlyX,
                varset.new_named_var("IO0", IO0, !VarSet),
                varset.new_named_var("IO", IO, !VarSet),
                VarSetXandIOs = !.VarSet
            ),
            IOGetPredName = StdGetPredName,
            IOSetPredName = StdSetPredName,
            IOPredArgs = [variable(X, Context),
                variable(IO0, Context), variable(IO, Context)],

            % It is important to have CopyIOExpr INSIDE the promise_pure scope
            % for the set predicate. If it were outside, then the scope
            % would not bind any variables, and since it is promised pure,
            % the compiler would be allowed to delete it. The problem does not
            % arise for the get predicate, since ImpureGetExpr binds X.
            CopyIOExpr = unify_expr(Context,
                variable(IO0, Context), variable(IO, Context),
                purity_impure),
            IOGetPredExpr = conj_expr(Context, ImpureGetExpr, CopyIOExpr),
            IOSetPredExpr = conj_expr(Context, ImpureSetExpr, CopyIOExpr),
            PureIOGetPredExpr =
                promise_purity_expr(Context, purity_pure, IOGetPredExpr),
            PureIOSetPredExpr =
                promise_purity_expr(Context, purity_pure, IOSetPredExpr),

            IOGetClauseInfo = item_clause_info(pf_predicate,
                IOGetPredName, IOPredArgs, VarSetXandIOs,
                ok2(PureIOGetPredExpr, []), Context, item_no_seq_num),
            IOSetClauseInfo = item_clause_info(pf_predicate,
                IOSetPredName, IOPredArgs, VarSetXandIOs,
                ok2(PureIOSetPredExpr, []), Context, item_no_seq_num),
            ClauseInfos = StdClauseInfos ++ [IOGetClauseInfo, IOSetClauseInfo],
            set.det_remove(mutable_pred_io_get, !PredKinds),
            set.det_remove(mutable_pred_io_set, !PredKinds)
        ),
        ForeignProcs = []
    ).

    % Define the init predicate, and arrange for it to be called
    % at initialization time.
    %
:- pred define_init_pred(module_name::in, foreign_language::in,
    item_mutable_info::in, sym_name::in, maybe(goal)::in,
    item_clause_info::out,
    item_pragma_info(pragma_info_foreign_proc_export)::out,
    pred_target_names::in, pred_target_names::out) is det.

define_init_pred(ModuleName, Lang, ItemMutable, InitSetPredName,
        MaybeCallPreInitExpr, InitClauseInfo, PragmaFPEInfo,
        !PredTargetNames) :-
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, _Type, _OrigInst, _Inst,
        InitTerm, VarSetMutable, _MutAttrs, Context, SeqNum),
    varset.new_named_var("X", X, VarSetMutable, VarSetMutableX),
    VarX = variable(X, Context),

    UnifyExpr = unify_expr(Context, VarX, InitTerm, purity_impure),
    CallSetExpr = call_expr(Context, InitSetPredName, [VarX], purity_impure),
    UnifyCallSetExpr = conj_expr(Context, UnifyExpr, CallSetExpr),
    (
        MaybeCallPreInitExpr = no,
        InitPredExpr = UnifyCallSetExpr
    ;
        MaybeCallPreInitExpr = yes(CallPreInitExpr),
        InitPredExpr = conj_expr(Context, CallPreInitExpr, UnifyCallSetExpr)
    ),
    InitPredName = mutable_init_pred_sym_name(ModuleName, MutableName),
    % See the comments for parse_mutable_decl_info for the reason
    % why we _must_ pass VarSetMutableX here.
    InitClauseInfo = item_clause_info(pf_predicate, InitPredName, [],
        VarSetMutableX, ok2(InitPredExpr, []), Context, item_no_seq_num),

    % The compiler introduces initialise declarations that call impure
    % predicates as part of the source-to-source transformation for mutable
    % variables. These predicates *must* be impure in order to prevent the
    % compiler optimizing them away.
    Attrs = item_compiler_attributes(compiler_origin_mutable(ModuleName,
        MutableName, mutable_pred_init)),
    Origin = item_origin_compiler(Attrs),
    Arity = 0,
    (
        SeqNum = item_seq_num(SeqNumInt)
    ;
        SeqNum = item_no_seq_num,
        unexpected($pred, "item_no_seq_num")
    ),
    new_user_init_pred(ModuleName, SeqNumInt, InitPredName, Arity,
        TargetName, !PredTargetNames),
    PredNameModesPF = proc_pf_name_modes(pf_predicate, InitPredName, []),
    FPEInfo = pragma_info_foreign_proc_export(Origin, Lang,
        PredNameModesPF, TargetName),
    PragmaFPEInfo = item_pragma_info(FPEInfo, Context, item_no_seq_num).

%---------------------------------------------------------------------------%

    % Decide what the name of the underlying global used to implement the
    % mutable should be. If there is a foreign_name attribute for the target
    % language, then use that, otherwise construct one based on the
    % Mercury name for the mutable.
    %
:- pred decide_mutable_target_var_name(module_name::in, string::in,
    mutable_var_attributes::in, foreign_language::in, string::out) is det.

decide_mutable_target_var_name(ModuleName, MutableName, MutAttrs,
        ForeignLanguage, TargetMutableName) :-
    MutAttrs = mutable_var_attributes(LangMap, _Const),
    ( if map.search(LangMap, ForeignLanguage, TargetMutableName0) then
        TargetMutableName = TargetMutableName0
    else
        TargetMutableName = mutable_var_name(ModuleName, MutableName)
    ).

    % The first argument global_foreign_type_name says whether the mutable
    % should always be boxed or not. The only difference between the high- and
    % low-level C backends is that in the latter mutables are *always* boxed,
    % whereas in the former they may not be. The other backends that support
    % mutables are all native_if_possible.
    %
:- func global_foreign_type_name(module_info, box_policy, foreign_language,
    mer_type) = string.

global_foreign_type_name(ModuleInfo, BoxPolicy, Lang, Type) = String :-
    (
        BoxPolicy = bp_always_boxed,
        String = "MR_Word"
    ;
        BoxPolicy = bp_native_if_possible,
        String = exported_type_to_string(ModuleInfo, Lang, Type)
    ).

%---------------------------------------------------------------------------%

    % Generate a name for the target language variable that holds
    % the current the state of the mutable.
    %
    % This variable name should be acceptable in all our current backends.
    %
:- func mutable_var_name(module_name, string) = string.

mutable_var_name(ModuleName, Name) = MangledVarName :-
    RawVarName = "mutable_variable_" ++ Name,
    QualifiedVarName0 = qualified(ModuleName, RawVarName),
    ( if mercury_std_library_module_name(ModuleName) then
        QualifiedVarName =
            add_outermost_qualifier("mercury", QualifiedVarName0)
    else
        QualifiedVarName = QualifiedVarName0
    ),
    MangledVarName = sym_name_mangle(QualifiedVarName).

    % Returns the name of the mutex associated a given mutable. The input
    % to this function is the name of the mutable in the target language,
    % i.e. it is the result of a call to mutable_c_var_name/2 or one of the
    % specified foreign names for the mutable.
    %
:- func mutable_mutex_var_name(string) = string.

mutable_mutex_var_name(TargetMutableVarName) = MutexVarName :-
    MutexVarName = TargetMutableVarName ++ "_lock".

%---------------------------------------------------------------------------%

:- type need_pre_init_pred
    --->    dont_need_pre_init_pred
    ;       need_pre_init_pred.

:- type need_lock_unlock_preds
    --->    dont_need_lock_unlock_preds
    ;       need_lock_unlock_preds.

:- type need_unsafe_get_set_preds
    --->    dont_need_unsafe_get_set_preds
    ;       need_unsafe_get_set_preds.

:- type mutable_target_params
    --->    mutable_target_params(
                mtp_target_lang         :: foreign_language,
                mtp_box_policy          :: box_policy,
                mtp_need_pre_init       :: need_pre_init_pred,
                mtp_need_locking        :: need_lock_unlock_preds,
                mtp_need_unsafe_get_set :: need_unsafe_get_set_preds,
                mtp_private_aux_preds   :: list(mutable_pred_kind),
                mtp_public_aux_preds    :: list(mutable_pred_kind)
            ).

%---------------------------------------------------------------------------%

    % This predicate decides which auxiliary predicates we need
    % to implement a mutable. The rest of this module just implements
    % the decisions made here, which are recorded in the mutable_target_params.
    %
:- pred get_mutable_target_params(globals::in, mutable_var_attributes::in,
    mutable_target_params::out) is det.

get_mutable_target_params(Globals, MutAttrs, TargetParams) :-
    % The set of predicates we need depends on
    % - the compilation target, since we use different implementations
    %   of mutables on different backends, and
    % - on the properties of the mutable itself.
    globals.get_target(Globals, CompilationTarget),
    (
        CompilationTarget = target_c,
        Lang = lang_c,
        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        (
            HighLevelCode = no,
            BoxPolicy = bp_always_boxed
        ;
            HighLevelCode = yes,
            BoxPolicy = bp_native_if_possible
        )
    ;
        CompilationTarget = target_csharp,
        Lang = lang_csharp,
        BoxPolicy = bp_native_if_possible
    ;
        CompilationTarget = target_java,
        Lang = lang_java,
        BoxPolicy = bp_native_if_possible
    ),
    MutAttrs = mutable_var_attributes(_LangMap, Const),
    (
        Const = mutable_is_constant,
        PreInit = dont_need_pre_init_pred,
        LockUnlock = dont_need_lock_unlock_preds,
        UnsafeAccess = dont_need_unsafe_get_set_preds
    ;
        Const = mutable_is_not_constant(_, _),
        (
            CompilationTarget = target_c,
            PreInit = need_pre_init_pred,
            LockUnlock = need_lock_unlock_preds,
            UnsafeAccess = need_unsafe_get_set_preds
        ;
            CompilationTarget = target_csharp,
            IsThreadLocal = mutable_var_thread_local(Const),
            (
                IsThreadLocal = mutable_thread_local,
                PreInit = need_pre_init_pred
            ;
                IsThreadLocal = mutable_not_thread_local,
                PreInit = dont_need_pre_init_pred
            ),
            LockUnlock = dont_need_lock_unlock_preds,
            UnsafeAccess = need_unsafe_get_set_preds
        ;
            CompilationTarget = target_java,
            PreInit = dont_need_pre_init_pred,
            LockUnlock = dont_need_lock_unlock_preds,
            UnsafeAccess = need_unsafe_get_set_preds
        )
    ),
    compute_needed_private_mutable_aux_preds(PreInit, LockUnlock, UnsafeAccess,
        PrivatePredKinds),
    compute_needed_public_mutable_aux_preds(MutAttrs, PublicPredKinds),
    TargetParams = mutable_target_params(Lang, BoxPolicy,
        PreInit, LockUnlock, UnsafeAccess, PrivatePredKinds, PublicPredKinds).

    % This predicate decides which of the private auxiliary predicates
    % we should generate for a mutable.
    %
    % This same decisions for the public aux predicates are made by
    % compute_needed_public_mutable_aux_preds in prog_mutable.m.
    %
:- pred compute_needed_private_mutable_aux_preds(need_pre_init_pred::in,
    need_lock_unlock_preds::in, need_unsafe_get_set_preds::in,
    list(mutable_pred_kind)::out) is det.

compute_needed_private_mutable_aux_preds(PreInit, LockUnlock, UnsafeAccess,
        PrivateAuxPreds) :-
    % The logic of this code should match the logic of the
    % define_aux_preds_for_mutable predicate above.

    % Create the mutable initialisation predicate.
    InitPreds = [mutable_pred_init],

    % Create the pre-initialisation predicate,
    % if needed by the initialisation predicate.
    (
        PreInit = dont_need_pre_init_pred,
        PreInitPreds = []
    ;
        PreInit = need_pre_init_pred,
        PreInitPreds = [mutable_pred_pre_init]
    ),

    % Create the primitive access and locking predicates, if needed.
    (
        UnsafeAccess = dont_need_unsafe_get_set_preds,
        UnsafeAccessPreds = []
    ;
        UnsafeAccess = need_unsafe_get_set_preds,
        UnsafeAccessPreds = [mutable_pred_unsafe_get, mutable_pred_unsafe_set]
    ),
    (
        LockUnlock = dont_need_lock_unlock_preds,
        LockUnlockPreds = []
    ;
        LockUnlock = need_lock_unlock_preds,
        LockUnlockPreds = [mutable_pred_lock, mutable_pred_unlock]
    ),
    PrivateAuxPreds = InitPreds ++ PreInitPreds ++
        UnsafeAccessPreds ++ LockUnlockPreds.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred check_mutable(module_info::in, item_mutable_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_mutable(ModuleInfo, ItemMutable, !Specs) :-
    ItemMutable = item_mutable_info(_MutableName,
        _OrigType, _Type, OrigInst, Inst,
        _InitTerm, _VarSetMutable, MutAttrs, Context, _SeqNum),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, CompilationTarget),
    % NOTE We currently support the foreign_name attribute for all targets,
    % but we did not do so when we supported Erlang.
    (
        ( CompilationTarget = target_c
        ; CompilationTarget = target_java
        ; CompilationTarget = target_csharp
        )
    ),

    % If the mutable is to be trailed, then we need to be in a trailing grade.
    MutAttrs = mutable_var_attributes(_LangMap, Const),
    globals.lookup_bool_option(Globals, use_trail, UseTrail),
    ( if
        mutable_var_trailed(Const) = mutable_trailed,
        UseTrail = no
    then
        TrailPieces = [words("Error: trailed"), decl("mutable"),
            words("declaration in non-trailing grade."), nl],
        TrailSpec = simplest_spec($pred, severity_error,
            phase_parse_tree_to_hlds, Context, TrailPieces),
        !:Specs = [TrailSpec | !.Specs]
    else
        true
    ),

    % Check that the inst in the mutable declaration is a valid inst
    % for a mutable declaration.
    % It is okay to pass a dummy varset in here since any attempt
    % to use inst variables in a mutable declaration should already
    % been dealt with when the mutable declaration was parsed.
    DummyInstVarSet = varset.init,
    check_mutable_inst(ModuleInfo, Context, DummyInstVarSet, [], Inst,
        [], ExpandedInstSpecs),
    (
        ExpandedInstSpecs = []
    ;
        ExpandedInstSpecs = [_ | _],
        % We found some insts in Inst that are not allowed in mutables.
        %
        % Inst has been processed by equiv_type.m, which replaces named insts
        % with the definition of the named inst. When we check it, the error
        % messages we generate for any errors in it will lack information
        % about what nested sequence of named inst definitions the errors is
        % inside. We therefore compute the error messages on the original
        % inst as well.
        %
        % If ExpandedInstSpecs is nonempty, then UnexpandedInstSpecs should
        % be nonempty as well, but we prepare for it to be empty just in case.
        check_mutable_inst(ModuleInfo, Context, DummyInstVarSet, [], OrigInst,
            [], UnexpandedInstSpecs),
        (
            UnexpandedInstSpecs = [],
            % Printing error messages without the proper context is better than
            % not printing error messages at all, once we have discovered
            % an error.
            !:Specs = ExpandedInstSpecs ++ !.Specs
        ;
            UnexpandedInstSpecs = [_ | _],
            !:Specs = UnexpandedInstSpecs ++ !.Specs
        )
    ).

%---------------------------------------------------------------------------%

    % Add an error to !Specs for each part of the inst that isn't allowed
    % inside a mutable declaration.
    %
:- pred check_mutable_inst(module_info::in, prog_context::in,
    inst_varset::in, list(inst_ctor)::in, mer_inst::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_mutable_inst(ModuleInfo, Context, InstVarSet, ParentInsts, Inst,
        !Specs) :-
    (
        ( Inst = any(Uniq, _)
        ; Inst = ground(Uniq, _)
        ),
        check_mutable_inst_uniqueness(ModuleInfo, Context, InstVarSet,
            ParentInsts, Inst, Uniq, !Specs)
    ;
        Inst = bound(Uniq, _, BoundInsts),
        check_mutable_inst_uniqueness(ModuleInfo, Context, InstVarSet,
            ParentInsts, Inst, Uniq, !Specs),
        check_mutable_bound_insts(ModuleInfo, Context, InstVarSet,
            ParentInsts, BoundInsts, !Specs)
    ;
        Inst = defined_inst(InstName),
        (
            InstName = user_inst(UserInstSymName, UserInstArgs),
            list.length(UserInstArgs, UserInstArity),
            UserInstCtor = inst_ctor(UserInstSymName, UserInstArity),
            ( if
                list.member(UserInstCtor, ParentInsts)
            then
                true
            else if
                UserInstSymName =
                    qualified(UserInstModuleName, UserInstBaseName),
                UserInstModuleName = mercury_public_builtin_module,
                UserInstArity = 0,
                ( UserInstBaseName = "dead"
                ; UserInstBaseName = "mostly_dead"
                )
            then
                FreePieces = [words("may not appear in"),
                    decl("mutable"), words("declarations.")],
                UnqualInstName =
                    user_inst(unqualified(UserInstBaseName), UserInstArgs),
                UnqualInst = defined_inst(UnqualInstName),
                invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet,
                    ParentInsts, UnqualInst, FreePieces, !Specs)
            else
                check_mutable_insts(ModuleInfo, Context, InstVarSet,
                    ParentInsts, UserInstArgs, !Specs),

                module_info_get_inst_table(ModuleInfo, InstTable),
                inst_table_get_user_insts(InstTable, UserInstTable),
                ( if map.search(UserInstTable, UserInstCtor, InstDefn) then
                    InstDefn = hlds_inst_defn(DefnInstVarSet, _Params,
                        InstBody, _MMTC, _Context, _Status),
                    InstBody = eqv_inst(EqvInst),
                    DefnParentInsts = [UserInstCtor | ParentInsts],
                    check_mutable_inst(ModuleInfo, Context, DefnInstVarSet,
                        DefnParentInsts, EqvInst, !Specs)
                else
                    UndefinedPieces = [words("is not defined.")],
                    invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet,
                        ParentInsts, Inst, UndefinedPieces, !Specs)
                )
            )
        ;
            ( InstName = unify_inst(_, _, _, _)
            ; InstName = merge_inst(_, _)
            ; InstName = ground_inst(_, _, _, _)
            ; InstName = any_inst(_, _, _, _)
            ; InstName = shared_inst(_)
            ; InstName = mostly_uniq_inst(_)
            ; InstName = typed_inst(_, _)
            ; InstName = typed_ground(_, _)
            ),
            unexpected($pred, "non-user inst")
        )
    ;
        ( Inst = free
        ; Inst = free(_)
        ),
        FreePieces = [words("may not appear in"),
            decl("mutable"), words("declarations.")],
        invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet, ParentInsts,
            Inst, FreePieces, !Specs)
    ;
        Inst = constrained_inst_vars(_, _),
        ConstrainedPieces = [words("is constrained, and thus"),
            words("may not appear in"), decl("mutable"),
            words("declarations.")],
        invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet, ParentInsts,
            Inst, ConstrainedPieces, !Specs)
    ;
        Inst = abstract_inst(_, _),
        AbstractPieces = [words("is abstract, and thus"),
            words("may not appear in"), decl("mutable"),
            words("declarations.")],
        invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet, ParentInsts,
            Inst, AbstractPieces, !Specs)
    ;
        Inst = inst_var(_)
        % The parser ensures that the inst in the mutable declaration does
        % not have any variables. Any variables we encounter here must be
        % a parameter from a named inst that the top level inst refers to
        % either directly or indirectly.
    ;
        Inst = not_reached,
        unexpected($pred, "not_reached")
    ).

:- pred check_mutable_bound_insts(module_info::in, prog_context::in,
    inst_varset::in, list(inst_ctor)::in, list(bound_inst)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_mutable_bound_insts(_ModuleInfo, _Context, _InstVarSet, _ParentInsts,
        [], !Specs).
check_mutable_bound_insts(ModuleInfo, Context, InstVarSet, ParentInsts,
        [BoundInst | BoundInsts], !Specs) :-
    BoundInst = bound_functor(_ConsId, ArgInsts),
    check_mutable_insts(ModuleInfo, Context, InstVarSet, ParentInsts,
        ArgInsts, !Specs),
    check_mutable_bound_insts(ModuleInfo, Context, InstVarSet, ParentInsts,
        BoundInsts, !Specs).

:- pred check_mutable_insts(module_info::in, prog_context::in,
    inst_varset::in, list(inst_ctor)::in, list(mer_inst)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_mutable_insts(_ModuleInfo, _Context, _InstVarSet, _ParentInsts,
        [], !Specs).
check_mutable_insts(ModuleInfo, Context, InstVarSet, ParentInsts,
        [Inst | Insts], !Specs) :-
    check_mutable_inst(ModuleInfo, Context, InstVarSet, ParentInsts,
        Inst, !Specs),
    check_mutable_insts(ModuleInfo, Context, InstVarSet, ParentInsts,
        Insts, !Specs).

%---------------------%

:- pred check_mutable_inst_uniqueness(module_info::in, prog_context::in,
    inst_varset::in, list(inst_ctor)::in, mer_inst::in, uniqueness::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_mutable_inst_uniqueness(ModuleInfo, Context, InstVarSet, ParentInsts,
        Inst, Uniq, !Specs) :-
    (
        Uniq = shared
    ;
        (
            Uniq = unique,
            UniqStr = "unique"
        ;
            Uniq = mostly_unique,
            UniqStr = "mostly_unique"
        ;
            Uniq = clobbered,
            UniqStr = "clobbered"
        ;
            Uniq = mostly_clobbered,
            UniqStr = "mostly_clobbered"
        ),
        ( if Inst = ground(Uniq, _) then
            UniqPieces = [words("may not appear in"),
                decl("mutable"), words("declarations.")]
        else
            UniqPieces = [words("has uniqueness"), quote(UniqStr), suffix(","),
                words("which may not appear in"),
                decl("mutable"), words("declarations.")]
        ),
        invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet, ParentInsts,
            Inst, UniqPieces, !Specs)
    ).

:- pred invalid_inst_in_mutable(module_info::in, prog_context::in,
    inst_varset::in, list(inst_ctor)::in, mer_inst::in,
    list(format_component)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

invalid_inst_in_mutable(ModuleInfo, Context, InstVarSet, ParentInsts, Inst,
        ProblemPieces, !Specs) :-
    named_parents_to_pieces(ParentInsts, ParentPieces),
    InstPieces = error_msg_inst(ModuleInfo, InstVarSet,
        dont_expand_named_insts, quote_short_inst,
        [], [nl_indent_delta(1)], [nl_indent_delta(-1)], Inst),
    Pieces = [words("Error:") | ParentPieces] ++
        [words("the inst") | InstPieces] ++ ProblemPieces ++ [nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

:- pred named_parents_to_pieces(list(inst_ctor)::in,
    list(format_component)::out) is det.

named_parents_to_pieces([], []).
named_parents_to_pieces([InstCtor | InstCtors], Pieces) :-
    named_parent_to_pieces(InstCtor, HeadPieces),
    named_parents_to_pieces(InstCtors, TailPieces),
    Pieces = HeadPieces ++ TailPieces.

:- pred named_parent_to_pieces(inst_ctor::in,
    list(format_component)::out) is det.

named_parent_to_pieces(InstCtor, Pieces) :-
    InstCtor = inst_ctor(InstName, InstArity),
    Pieces = [words("in the expansion of the named inst"),
        qual_sym_name_arity(sym_name_arity(InstName, InstArity)),
        suffix(":"), nl].

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_mutable_aux_preds.
%---------------------------------------------------------------------------%
