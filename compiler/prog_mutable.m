%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2011 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_mutable.m.
% Main authors: rafe, juliensf, zs.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_mutable.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_item.

:- import_module cord.
:- import_module list.

:- func mutable_public_aux_pred_decls(module_name, item_mutable_info)
    = list(item_pred_decl_info).

%---------------------%

:- type module_params
    --->    module_params(
                mp_globals          :: globals,
                mp_module_name      :: sym_name,
                mp_type_name_func   :: type_foreign_name_func
            ).

    % This function should lookup the name of the given Mercury type
    % in the given foreign target language, even if the Mercury type
    % is defined using a foreign_type pragma. This means that our caller
    % needs to embed in this function access to a version of the type table
    % from *after* all type definitions have been processed.
    %
:- type type_foreign_name_func ==
    (func(foreign_language, mer_type) = string).

    % Implement the given mutable by constructing
    %
    % - the predicate declarations of the auxiliary predicates it needs,
    %
    % - the definitions of those auxiliary predicates, in the form of
    %   clauses (for some) or foreign procs (for the others),
    %
    % - recording the foreign_body_codes that contain the definitions of the
    %   global variables that contain the mutable,
    %
    % - recording any foreign_decl_codes that contain declarations of those
    %   global variables, if needed by the target language,
    %
    % Also, record the initialization predicates to the target language,
    % to let our ancestors arrange for them to be called before main/2.
    %
:- pred implement_mutable(module_params::in, item_mutable_info::in,
    list(item_pred_decl_info)::out,
    list(item_clause_info)::out, list(item_foreign_proc)::out,
    item_fproc_export::out,
    cord(foreign_decl_code)::in, cord(foreign_decl_code)::out,
    cord(foreign_body_code)::in, cord(foreign_body_code)::out,
    pred_target_names::in, pred_target_names::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.file_names.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_mode.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

mutable_public_aux_pred_decls(ModuleName, ItemMutable) = PublicAuxPredDecls :-
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, Type, _OrigInst, Inst, _Value, _Varset, MutAttrs,
        Context, _SeqNum),
    compute_needed_public_mutable_aux_preds(MutAttrs, PublicAuxPreds),
    list.map(
        make_mutable_aux_pred_decl(ModuleName, MutableName, Type, Inst,
            Context),
        PublicAuxPreds, PublicAuxPredDecls).

%---------------------------------------------------------------------------%

implement_mutable(ModuleParams, ItemMutable,
        PredDecls, ClauseInfos, ForeignProcs, FPEInfo,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord, !PredTargetNames) :-
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, Type, _OrigInst, Inst,
        _InitTerm, _VarSetMutable, MutAttrs, Context, _SeqNum),
    Globals = ModuleParams ^ mp_globals,
    get_mutable_target_params(Globals, MutAttrs, TargetParams),
    PublicPredKinds = TargetParams ^ mtp_public_aux_preds,
    PrivatePredKinds = TargetParams ^ mtp_private_aux_preds,
    NeededPredKinds = PublicPredKinds ++ PrivatePredKinds,
    ModuleName = ModuleParams ^ mp_module_name,
    list.map(
        make_mutable_aux_pred_decl(ModuleName, MutableName, Type, Inst,
            Context),
        NeededPredKinds, PredDecls),
    implement_defns_for_mutable(ModuleParams, ItemMutable,
        ClauseInfos, ForeignProcs, FPEInfo,
        !ForeignDeclCodeCord, !ForeignBodyCodeCord, !PredTargetNames).

%---------------------------------------------------------------------------%

:- pred implement_defns_for_mutable(module_params::in, item_mutable_info::in,
    list(item_clause_info)::out, list(item_foreign_proc)::out,
    item_fproc_export::out,
    cord(foreign_decl_code)::in, cord(foreign_decl_code)::out,
    cord(foreign_body_code)::in, cord(foreign_body_code)::out,
    pred_target_names::in, pred_target_names::out) is det.

implement_defns_for_mutable(ModuleParams, ItemMutable,
        ClauseInfos, ForeignProcs, FPEInfo,
        !ForeignDeclCodes, !ForeignBodyCodes, !PredTargetNames) :-
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, Type, _OrigInst, _Inst,
        _InitTerm, _VarSetMutable, MutAttrs, Context, _SeqNum),
    Globals = ModuleParams ^ mp_globals,
    get_mutable_target_params(Globals, MutAttrs, TargetParams),
    Lang = TargetParams ^ mtp_target_lang,
    define_mutable_global_var(ModuleParams, Lang, MutableName, Type, MutAttrs,
        Context, TargetMutableName, !ForeignDeclCodes, !ForeignBodyCodes),
    define_aux_preds_for_mutable(ModuleParams, TargetParams, ItemMutable,
        TargetMutableName, ClauseInfos, ForeignProcs, FPEInfo,
        !PredTargetNames).

%---------------------------------------------------------------------------%
%
% Define the global holding the mutable.
%

:- pred define_mutable_global_var(module_params::in, foreign_language::in,
    string::in, mer_type::in, mutable_var_attributes::in, prog_context::in,
    string::out,
    cord(foreign_decl_code)::in, cord(foreign_decl_code)::out,
    cord(foreign_body_code)::in, cord(foreign_body_code)::out) is det.

define_mutable_global_var(ModuleParams, Lang, MutableName, Type, MutAttrs,
        Context, TargetMutableName, !ForeignDeclCodes, !ForeignBodyCodes) :-
    MutAttrs = mutable_var_attributes(LangMap, Const),
    ModuleName = ModuleParams ^ mp_module_name,
    mutable_target_var_name(ModuleName, MutableName, LangMap, Lang,
        TargetMutableName),
    (
        Lang = lang_c,
        define_mutable_global_var_c(ModuleParams, TargetMutableName, Type,
            Const, Context, ForeignDeclCode, ForeignBodyCode),
        cord.snoc(ForeignDeclCode, !ForeignDeclCodes)
    ;
        Lang = lang_csharp,
        define_mutable_global_var_csharp(TargetMutableName, Type,
            Const, Context, ForeignBodyCode)
    ;
        Lang = lang_java,
        define_mutable_global_var_java(TargetMutableName, Type,
            Const, Context, ForeignBodyCode)
    ),
    cord.snoc(ForeignBodyCode, !ForeignBodyCodes).

    % Decide what the name of the underlying global used to implement the
    % mutable should be. If there is a foreign_name attribute for the target
    % language, then use that, otherwise construct one based on the
    % Mercury name for the mutable.
    %
    % The variable name should be acceptable in all our current backends.
    %
:- pred mutable_target_var_name(module_name::in, string::in,
    map(foreign_language, string)::in, foreign_language::in,
    string::out) is det.

mutable_target_var_name(ModuleName, MutableName, LangMap, Lang,
        TargetMutableVarName) :-
    ( if map.search(LangMap, Lang, TargetVarName) then
        TargetMutableVarName = TargetVarName
    else
        RawVarName = "mutable_variable_" ++ MutableName,
        QualifiedVarName0 = qualified(ModuleName, RawVarName),
        ( if mercury_std_library_module_name(ModuleName) then
            QualifiedVarName =
                add_outermost_qualifier("mercury", QualifiedVarName0)
        else
            QualifiedVarName = QualifiedVarName0
        ),
        TargetMutableVarName = sym_name_mangle(QualifiedVarName)
    ).

    % Define the global variable used to hold the mutable on the C backend,
    % and if needed, the mutex controlling access to it.
    %
:- pred define_mutable_global_var_c(module_params::in, string::in,
    mer_type::in, mutable_maybe_constant::in, prog_context::in,
    foreign_decl_code::out, foreign_body_code::out) is det.

define_mutable_global_var_c(ModuleParams, TargetMutableName, Type,
        Const, Context, ForeignDeclCode, ForeignBodyCode) :-
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
        HighLevelTypeName = global_foreign_type_name(ModuleParams,
            bp_native_if_possible, lang_c, Type),
        LowLevelTypeName = global_foreign_type_name(ModuleParams,
            bp_always_boxed, lang_c, Type),
        Globals = ModuleParams ^ mp_globals,
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
    DefnBody = string.append_list([
        TypeName, " ", TargetMutableName, ";\n" | LockDefnStrs]),
    ForeignBodyCode =
        foreign_body_code(lang_c, floi_literal(DefnBody), Context).

    % Define the global variable used to hold the mutable on the C# backend.
    %
:- pred define_mutable_global_var_csharp(string::in, mer_type::in,
    mutable_maybe_constant::in, prog_context::in,
    foreign_body_code::out) is det.

define_mutable_global_var_csharp(TargetMutableName, Type, Const, Context,
        ForeignBodyCode) :-
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
    ForeignBodyCode =
        foreign_body_code(lang_csharp, floi_literal(DefnBody), Context).

    % Define the global variable used to hold the mutable on the Java backend.
    %
:- pred define_mutable_global_var_java(string::in, mer_type::in,
    mutable_maybe_constant::in, prog_context::in,
    foreign_body_code::out) is det.

define_mutable_global_var_java(TargetMutableName, Type, Const, Context,
        ForeignBodyCode) :-
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
    ForeignBodyCode =
        foreign_body_code(lang_java, floi_literal(DefnBody), Context).

%---------------------------------------------------------------------------%

:- pred define_aux_preds_for_mutable(module_params::in,
    mutable_target_params::in, item_mutable_info::in, string::in,
    list(item_clause_info)::out, list(item_foreign_proc)::out,
    item_pragma_info(pragma_info_foreign_proc_export)::out,
    pred_target_names::in, pred_target_names::out) is det.

define_aux_preds_for_mutable(ModuleParams, TargetParams, ItemMutable,
        TargetMutableName, ClauseInfos, ForeignProcs, PragmaFPEInfo,
        !PredTargetNames) :-
    % The transformation we implement here is documented in notes/mutables.
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, Type, _OrigInst, Inst,
        _InitTerm, _VarSetMutable, MutAttrs, Context, _SeqNum),
    MutAttrs = mutable_var_attributes(_LangMap, Const),
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
        ModuleName = ModuleParams ^ mp_module_name,
        set.list_to_set(NeededPredKinds, !:PredKinds),
        ( if set.remove(mutable_pred_pre_init, !PredKinds) then
            define_pre_init_pred(ModuleName, TargetParams, MutableName,
                Const, Context, TargetMutableName, Attrs,
                CallPreInitExpr, PreInitForeignProc),
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
            define_lock_unlock_preds(ModuleName, TargetParams, MutableName,
                Const, Context, TargetMutableName, Attrs,
                LockUnlockExprs, LockUnlockForeignProcs),
            MaybeLockUnlockExprs = yes(LockUnlockExprs)
        else
            MaybeLockUnlockExprs = no,
            LockUnlockForeignProcs = []
        ),
        ( if
            set.remove(mutable_pred_unsafe_get, !PredKinds),
            set.remove(mutable_pred_unsafe_set, !PredKinds)
        then
            define_unsafe_get_set_preds(ModuleParams, TargetParams,
                MutableName, Type, Inst, Const, Context, TargetMutableName,
                Attrs, UnsafeGetSetExprs, UnsafeGetSetForeignProcs),
            MaybeUnsafeGetSetExprs = yes(UnsafeGetSetExprs)
        else
            MaybeUnsafeGetSetExprs = no,
            UnsafeGetSetForeignProcs = []
        ),

        % We do this after defining (a) the lock and unlock predicates and
        % (b) the unsafe get and set predicates, since they give us
        % (a) MaybeLockUnlockExprs and (b) MaybeUnsafeGetSetExprs respectively.
        define_main_get_set_preds(ModuleName, TargetParams, MutableName,
            Inst, Const, Context, TargetMutableName, Attrs,
            MaybeLockUnlockExprs, MaybeUnsafeGetSetExprs, InitSetPredName,
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
    string::in, mutable_maybe_constant::in, prog_context::in, string::in,
    pragma_foreign_proc_attributes::in, goal::out, item_foreign_proc::out)
    is det.

define_pre_init_pred(ModuleName, TargetParams, MutableName, Const, Context,
        TargetMutableName, Attrs, CallPreInitExpr, ForeignProc) :-
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
    string::in, mutable_maybe_constant::in, prog_context::in, string::in,
    pragma_foreign_proc_attributes::in,
    {goal, goal}::out, list(item_foreign_proc)::out) is det.

define_lock_unlock_preds(ModuleName, TargetParams, MutableName, Const, Context,
        TargetMutableName, Attrs, LockUnlockExprs, ForeignProcs) :-
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
:- pred define_unsafe_get_set_preds(module_params::in,
    mutable_target_params::in, string::in, mer_type::in, mer_inst::in,
    mutable_maybe_constant::in, prog_context::in, string::in,
    pragma_foreign_proc_attributes::in,
    {goal, goal}::out, list(item_foreign_proc)::out) is det.

define_unsafe_get_set_preds(ModuleParams, TargetParams, MutableName,
        Type, Inst, Const, Context, TargetMutableName, Attrs,
        UnsafeGetSetExprs, ForeignProcs) :-
    ModuleName = ModuleParams ^ mp_module_name,
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
                global_foreign_type_name(ModuleParams, BoxPolicy, Lang, Type),
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
    string::in, mer_inst::in, mutable_maybe_constant::in, prog_context::in,
    string::in, pragma_foreign_proc_attributes::in,
    maybe({goal, goal})::in, maybe({goal, goal})::in, sym_name::out,
    list(item_clause_info)::out, list(item_foreign_proc)::out,
    set(mutable_pred_kind)::in, set(mutable_pred_kind)::out) is det.

define_main_get_set_preds(ModuleName, TargetParams, MutableName, Inst, Const,
        Context, TargetMutableName, Attrs,
        MaybeLockUnlockExprs, MaybeUnsafeGetSetExprs,
        InitSetPredName, ClauseInfos, ForeignProcs, !PredKinds) :-
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
        Const = mutable_is_not_constant(_, Local),
        (
            CompilationTarget = target_c,
            PreInit = need_pre_init_pred,
            LockUnlock = need_lock_unlock_preds,
            UnsafeAccess = need_unsafe_get_set_preds
        ;
            CompilationTarget = target_csharp,
            (
                Local = mutable_is_thread_local,
                PreInit = need_pre_init_pred
            ;
                Local = mutable_is_not_thread_local(_),
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

    % This predicate decides which of the publicly visible auxiliary predicates
    % we should generate for a mutable.
    %
    % This same decisions for the private aux predicates are made by
    % compute_needed_private_mutable_aux_preds in add_mutable_aux_preds.m.
    %
:- pred compute_needed_public_mutable_aux_preds(mutable_var_attributes::in,
    list(mutable_pred_kind)::out) is det.

compute_needed_public_mutable_aux_preds(MutAttrs, PublicAuxPreds) :-
    % The logic we use here is duplicated in define_main_get_set_preds
    % in add_mutable_aux_preds.m. The comment there explains why.
    MutAttrs = mutable_var_attributes(_ForeignNames, Const),
    (
        Const = mutable_is_constant,
        % We create the "get" access predicate, which is pure since
        % it always returns the same value, but we must also create
        % a secret "set" predicate for use by the initialization code.
        GetSetPreds =
            [mutable_pred_constant_get, mutable_pred_constant_secret_set]
    ;
        Const = mutable_is_not_constant(AttachToIO, _Local),
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

%---------------------------------------------------------------------------%

    % Returns the name of the mutex associated eith a given mutable.
    % The input to this function is the name of the mutable in the target
    % language.
    %
:- func mutable_mutex_var_name(string) = string.

mutable_mutex_var_name(TargetMutableVarName) = MutexVarName :-
    MutexVarName = TargetMutableVarName ++ "_lock".

%---------------------------------------------------------------------------%

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

make_mutable_aux_pred_decl(ModuleName, Name, Type, Inst, Context, Kind,
        PredDecl) :-
    (
        Kind = mutable_pred_pre_init,
        PredSymName = mutable_pre_init_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [],
        Purity = purity_impure
    ;
        Kind = mutable_pred_init,
        PredSymName = mutable_init_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [],
        Purity = purity_impure
    ;
        Kind = mutable_pred_lock,
        PredSymName = mutable_lock_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [],
        Purity = purity_impure
    ;
        Kind = mutable_pred_unlock,
        PredSymName = mutable_unlock_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [],
        Purity = purity_impure
    ;
        Kind = mutable_pred_unsafe_get,
        PredSymName = mutable_unsafe_get_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_out(Type, Inst)],
        Purity = purity_semipure
    ;
        Kind = mutable_pred_unsafe_set,
        PredSymName = mutable_unsafe_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_in(Type, Inst)],
        Purity = purity_impure
    ;
        Kind = mutable_pred_std_get,
        PredSymName = mutable_get_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_out(Type, Inst)],
        Purity = purity_semipure
    ;
        Kind = mutable_pred_std_set,
        PredSymName = mutable_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_in(Type, Inst)],
        Purity = purity_impure
    ;
        Kind = mutable_pred_constant_get,
        PredSymName = mutable_get_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_out(Type, Inst)],
        Purity = purity_pure
    ;
        Kind = mutable_pred_constant_secret_set,
        PredSymName = mutable_secret_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_in(Type, Inst)],
        Purity = purity_impure
    ;
        Kind = mutable_pred_io_get,
        PredSymName = mutable_get_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_out(Type, Inst) | io_state_pair],
        Purity = purity_pure
    ;
        Kind = mutable_pred_io_set,
        PredSymName = mutable_set_pred_sym_name(ModuleName, Name),
        ArgTypesAndModes = [type_in(Type, Inst) | io_state_pair],
        Purity = purity_pure
    ),
    make_aux_pred_decl(ModuleName, Name, PredSymName, ArgTypesAndModes, Purity,
        Kind, Context, PredDecl).

%---------------------------------------------------------------------------%

:- func type_in(mer_type, mer_inst) = type_and_mode.

type_in(Type, Inst) = type_and_mode(Type, in_mode(Inst)).

:- func type_out(mer_type, mer_inst) = type_and_mode.

type_out(Type, Inst) = type_and_mode(Type, out_mode(Inst)).

:- func io_state_pair = list(type_and_mode).

io_state_pair =
    [type_and_mode(io_state_type, di_mode),
    type_and_mode(io_state_type, uo_mode)].

:- pred make_aux_pred_decl(module_name::in, string::in, sym_name::in,
    list(type_and_mode)::in, purity::in, mutable_pred_kind::in,
    prog_context::in, item_pred_decl_info::out) is det.

make_aux_pred_decl(ModuleName, Name, PredSymName, ArgTypesAndModes, Purity,
        Kind, Context, PredDecl) :-
    WithType = maybe.no,
    WithMode = maybe.no,
    Origin = compiler_origin_mutable(ModuleName, Name, Kind),
    CompilerAttrs = item_compiler_attributes(Origin),
    MaybeAttrs = item_origin_compiler(CompilerAttrs),
    varset.init(TypeVarSet),
    varset.init(InstVarSet),
    ExistQVars = [],
    Constraints = constraints([], []),
    SeqNum = item_no_seq_num,
    PredDecl = item_pred_decl_info(PredSymName, pf_predicate, ArgTypesAndModes,
        WithType, WithMode, yes(detism_det), MaybeAttrs,
        TypeVarSet, InstVarSet, ExistQVars, Purity, Constraints,
        Context, SeqNum).

%---------------------------------------------------------------------------%

    % The BoxPolicy says whether the mutable should always be boxed or not.
    % The only difference between the high- and low-level C backends is that
    % in the latter mutables are *always* boxed, whereas in the former
    % they may not be. The other backends that support mutables are all
    % native_if_possible.
    %
:- func global_foreign_type_name(module_params, box_policy, foreign_language,
    mer_type) = string.

global_foreign_type_name(ModuleParams, BoxPolicy, Lang, Type) = String :-
    (
        BoxPolicy = bp_always_boxed,
        String = "MR_Word"
    ;
        BoxPolicy = bp_native_if_possible,
        TypeNameFunc = ModuleParams ^ mp_type_name_func,
        String = TypeNameFunc(Lang, Type)
    ).

%---------------------------------------------------------------------------%
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

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_mutable.
%---------------------------------------------------------------------------%
