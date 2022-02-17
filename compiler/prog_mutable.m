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
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_item.

:- import_module cord.
:- import_module list.

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

%---------------------%

:- func declare_mutable_aux_preds_for_int0(module_name, item_mutable_info)
    = list(item_pred_decl_info).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.file_names.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_mode.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

implement_mutable(ModuleParams, ItemMutable,
        PredDecls, ClauseInfos, ForeignProcs, FPEInfo,
        !ForeignDeclCodes, !ForeignBodyCodes, !PredTargetNames) :-
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, Type, _OrigInst, _Inst,
        _InitTerm, _VarSetMutable, MutAttrs, Context, _SeqNum),
    Globals = ModuleParams ^ mp_globals,
    get_target_params(Globals, TargetParams),
    Lang = TargetParams ^ tp_target_lang,
    define_mutable_global_var(ModuleParams, Lang, MutableName, Type, MutAttrs,
        Context, TargetMutableName, !ForeignDeclCodes, !ForeignBodyCodes),
    declare_and_define_mutable_aux_preds(ModuleParams, TargetParams,
        ItemMutable, TargetMutableName,
        PredDecls, ClauseInfos, ForeignProcs, FPEInfo, !PredTargetNames).

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

declare_mutable_aux_preds_for_int0(ModuleName, ItemMutable)
        = PublicAuxPredDecls :-
    % The logic here is a version of the logic behind
    % declare_and_define_mutable_aux_preds below, but restricted
    % to the kinds of items that we may want to put into .int0 files.
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, Type, _OrigInst, Inst, _Value, _Varset, MutAttrs,
        Context, _SeqNum),
    MutAttrs = mutable_var_attributes(_LangMap, Const),
    (
        Const = mutable_is_constant,
        declare_constant_get_set_preds(ModuleName, MutableName, Type, Inst,
            Context, _InitSetPredName, GetPredDecl, _SetPredDecl),
        % We create the "get" access predicate, which is pure since
        % it always returns the same value, but we must also create
        % a secret "set" predicate for use by the initialization code.
        PublicAuxPredDecls = [GetPredDecl]
        % We used to also put SetPredDecl into the .int0 file,
        % but since the set predicate for constant mutables
        % is supposed to be a secret, that seems... counterproductive.
    ;
        Const = mutable_is_not_constant(AttachToIO, _Local),
        declare_nonconstant_get_set_preds(ModuleName, MutableName, Type, Inst,
            AttachToIO, Context, _InitSetPredName,
            GetPredDecl, SetPredDecl, IOPredDecls),
        % _AttachToIO = mutable_dont_attach_to_io_state => IOPredDecls = [] 
        PublicAuxPredDecls = [GetPredDecl, SetPredDecl | IOPredDecls]
    ).

%---------------------------------------------------------------------------%

:- pred declare_and_define_mutable_aux_preds(module_params::in,
    mutable_target_params::in, item_mutable_info::in, string::in,
    list(item_pred_decl_info)::out,
    list(item_clause_info)::out, list(item_foreign_proc)::out,
    item_fproc_export::out,
    pred_target_names::in, pred_target_names::out) is det.

declare_and_define_mutable_aux_preds(ModuleParams, TargetParams, ItemMutable,
        TargetMutableName, PredDecls, ClauseInfos, ForeignProcs,
        PragmaFPEInfo, !PredTargetNames) :-
    % The transformation we implement here is documented in notes/mutables.
    % The logic here is an expanded version of the logic behind
    % declare_mutable_aux_preds_for_int0 above.
    ItemMutable = item_mutable_info(MutableName,
        _OrigType, Type, _OrigInst, Inst,
        _InitTerm, _VarSetMutable, MutAttrs, Context, _SeqNum),
    TargetParams = mutable_target_params(Lang, BoxPolicy),

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
        % qualification, but do we really want to opt-export
        % mutable variables anyway?
        set_may_export_body(yes(proc_may_not_export_body), Attrs0, Attrs)
    ),

    ModuleName = ModuleParams ^ mp_module_name,
    MutAttrs = mutable_var_attributes(_LangMap, Const),
    (
        Const = mutable_is_constant,
        declare_constant_get_set_preds(ModuleName, MutableName, Type, Inst,
            Context, InitSetPredName, GetPredDecl, SetPredDecl),
        define_constant_get_set_preds(ModuleName, TargetParams, MutableName,
            Inst, Context, TargetMutableName, Attrs, GetSetForeignProcs),
        NonInitPredDecls = [GetPredDecl, SetPredDecl],
        GetSetClauseInfos = [],
        PreInitForeignProcs = [],
        MaybeCallPreInitExpr = maybe.no,
        LockUnlockForeignProcs = [],
        UnsafeGetSetForeignProcs = []
    ;
        Const = mutable_is_not_constant(AttachToIO, Local),
        declare_nonconstant_get_set_preds(ModuleName, MutableName, Type, Inst,
            AttachToIO, Context, InitSetPredName,
            GetPredDecl, SetPredDecl, IOPredDecls),
        % We call define_nonconstant_get_set_preds later,
        % after we have computed the input it needs.
        do_we_need_pre_init_lock_unlock(Lang, Local, PreInit, LockUnlock),
        (
            PreInit = dont_need_pre_init_pred,
            PreInitPredDecls = [],
            PreInitForeignProcs = [],
            MaybeCallPreInitExpr = maybe.no
        ;
            PreInit = need_pre_init_pred,
            declare_pre_init_pred(ModuleName, MutableName, Context,
                PreInitPredDecl),
            PreInitPredDecls = [PreInitPredDecl],
            define_pre_init_pred(ModuleName, TargetParams, MutableName,
                Local, Context, TargetMutableName, Attrs,
                CallPreInitExpr, PreInitForeignProc),
            PreInitForeignProcs = [PreInitForeignProc],
            MaybeCallPreInitExpr = yes(CallPreInitExpr)
        ),
        (
            LockUnlock = dont_need_lock_unlock_preds,
            LockUnlockPredDecls = [],
            LockUnlockForeignProcs = [],
            MaybeLockUnlockExprs = maybe.no
        ;
            LockUnlock = need_lock_unlock_preds,
            declare_lock_unlock_preds(ModuleName, MutableName,
                Context, LockPredDecl, UnlockPredDecl),
            define_lock_unlock_preds(ModuleName, TargetParams, MutableName,
                Local, Context, TargetMutableName, Attrs,
                LockUnlockExprs, LockUnlockForeignProcs),
            LockUnlockPredDecls = [LockPredDecl, UnlockPredDecl],
            MaybeLockUnlockExprs = yes(LockUnlockExprs)
        ),
        declare_unsafe_get_set_preds(ModuleName, MutableName, Type, Inst,
            Context, UnsafeGetPredDecl, UnsafeSetPredDecl),
        define_unsafe_get_set_preds(ModuleParams, TargetParams,
            MutableName, Type, Inst, Local, Context, TargetMutableName,
            Attrs, UnsafeGetExpr, UnsafeSetExpr, UnsafeGetSetForeignProcs),
        % We do this after defining (a) the lock and unlock predicates
        % (if any), and (b) the unsafe get and set predicates, since they
        % give us (a) MaybeLockUnlockExprs and (b) Unsafe{Get,Set}Expr
        % respectively.
        define_nonconstant_get_set_preds(ModuleName, TargetParams,
            MutableName, AttachToIO, Context,
            MaybeLockUnlockExprs, UnsafeGetExpr, UnsafeSetExpr,
            GetSetClauseInfos),
        GetSetForeignProcs = [],
        NonInitPredDecls = [GetPredDecl, SetPredDecl] ++ IOPredDecls ++
            PreInitPredDecls ++ LockUnlockPredDecls ++
            [UnsafeGetPredDecl, UnsafeSetPredDecl]
    ),
    declare_init_pred(ModuleName, MutableName, Context, InitPredDecl),
    % We do this after (a) defining the preinit predicate (if any) and
    % (b) declaring the main get and set predicates, since they give us
    % (a) MaybeCallPreInitExpr and (b) InitSetPredName respectively.
    define_init_pred(ModuleName, Lang, ItemMutable, InitSetPredName,
        MaybeCallPreInitExpr, InitClauseInfo, PragmaFPEInfo, !PredTargetNames),
    PredDecls = [InitPredDecl | NonInitPredDecls],
    ClauseInfos = [InitClauseInfo | GetSetClauseInfos],
    ForeignProcs = PreInitForeignProcs ++ LockUnlockForeignProcs ++
        GetSetForeignProcs ++ UnsafeGetSetForeignProcs.

:- type need_pre_init_pred
    --->    dont_need_pre_init_pred
    ;       need_pre_init_pred.

:- type need_lock_unlock_preds
    --->    dont_need_lock_unlock_preds
    ;       need_lock_unlock_preds.

:- pred do_we_need_pre_init_lock_unlock(foreign_language::in,
    mutable_maybe_thread_local::in,
    need_pre_init_pred::out, need_lock_unlock_preds::out) is det.

do_we_need_pre_init_lock_unlock(Lang, Local, PreInit, LockUnlock) :-
    (
        Lang = lang_c,
        PreInit = need_pre_init_pred,
        LockUnlock = need_lock_unlock_preds
    ;
        Lang = lang_csharp,
        (
            Local = mutable_is_thread_local,
            PreInit = need_pre_init_pred
        ;
            Local = mutable_is_not_thread_local(_),
            PreInit = dont_need_pre_init_pred
        ),
        LockUnlock = dont_need_lock_unlock_preds
    ;
        Lang = lang_java,
        PreInit = dont_need_pre_init_pred,
        LockUnlock = dont_need_lock_unlock_preds
    ).

%---------------------------------------------------------------------------%

:- pred declare_pre_init_pred(module_name::in, string::in,
    prog_context::in, item_pred_decl_info::out) is det.

declare_pre_init_pred(ModuleName, MutableName, Context, PreInitPredDecl) :-
    PreInitPredName = mutable_pre_init_pred_name(ModuleName, MutableName),
    make_aux_pred_decl(ModuleName, MutableName, PreInitPredName,
        [], purity_impure, mutable_pred_pre_init, Context, PreInitPredDecl).

    % Define the pre_init predicates, if needed by the init predicate.
    %
:- pred define_pre_init_pred(module_name::in, mutable_target_params::in,
    string::in, mutable_maybe_thread_local::in, prog_context::in, string::in,
    pragma_foreign_proc_attributes::in, goal::out, item_foreign_proc::out)
    is det.

define_pre_init_pred(ModuleName, TargetParams, MutableName, Local, Context,
        TargetMutableName, Attrs, CallPreInitExpr, ForeignProc) :-
    Lang = TargetParams ^ tp_target_lang,
    PreInitPredName = mutable_pre_init_pred_name(ModuleName, MutableName),
    (
        Lang = lang_c,
        (
            Local = mutable_is_not_thread_local(_),
            PreInitCode = string.append_list([
                "#ifdef MR_THREAD_SAFE\n",
                "   pthread_mutex_init(&",
                        mutable_mutex_var_name(TargetMutableName),
                        ", MR_MUTEX_ATTR);\n",
                "#endif\n"
            ])
        ;
            Local = mutable_is_thread_local,
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

%---------------------------------------------------------------------------%

:- pred declare_lock_unlock_preds(module_name::in, string::in,
    prog_context::in,
    item_pred_decl_info::out, item_pred_decl_info::out) is det.

declare_lock_unlock_preds(ModuleName, MutableName, Context,
        LockPredDecl, UnlockPredDecl) :-
    LockPredName = mutable_lock_pred_name(ModuleName, MutableName),
    UnlockPredName = mutable_unlock_pred_name(ModuleName, MutableName),
    make_aux_pred_decl(ModuleName, MutableName, LockPredName,
        [], purity_impure, mutable_pred_lock, Context, LockPredDecl),
    make_aux_pred_decl(ModuleName, MutableName, UnlockPredName,
        [], purity_impure, mutable_pred_unlock, Context, UnlockPredDecl).

    % Define the lock and unlock predicates, if needed.
    %
:- pred define_lock_unlock_preds(module_name::in, mutable_target_params::in,
    string::in, mutable_maybe_thread_local::in, prog_context::in, string::in,
    pragma_foreign_proc_attributes::in,
    {goal, goal}::out, list(item_foreign_proc)::out) is det.

define_lock_unlock_preds(ModuleName, TargetParams, MutableName, Local, Context,
        TargetMutableName, Attrs, LockUnlockExprs, ForeignProcs) :-
    Lang = TargetParams ^ tp_target_lang,
    (
        Lang = lang_c,
        set_thread_safe(proc_thread_safe, Attrs, LockAndUnlockAttrs),
        MutableMutexVarName = mutable_mutex_var_name(TargetMutableName),
        (
            Local = mutable_is_not_thread_local(_),
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
            Local = mutable_is_thread_local,
            LockForeignProcBody = "",
            UnlockForeignProcBody = ""
        ),
        LockPredName =
            mutable_lock_pred_name(ModuleName, MutableName),
        UnlockPredName =
            mutable_unlock_pred_name(ModuleName, MutableName),
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

%---------------------------------------------------------------------------%

:- pred declare_unsafe_get_set_preds(module_name::in, string::in,
    mer_type::in, mer_inst::in, prog_context::in,
    item_pred_decl_info::out, item_pred_decl_info::out) is det.

declare_unsafe_get_set_preds(ModuleName, MutableName, Type, Inst, Context,
        UnsafeGetPredDecl, UnsafeSetPredDecl) :-
    GetArg = type_out(Type, Inst),
    SetArg = type_in(Type, Inst),
    UnsafeGetPredName = mutable_unsafe_get_pred_name(ModuleName, MutableName),
    UnsafeSetPredName = mutable_unsafe_set_pred_name(ModuleName, MutableName),
    make_aux_pred_decl(ModuleName, MutableName, UnsafeGetPredName,
        [GetArg], purity_semipure,
        mutable_pred_std_get, Context, UnsafeGetPredDecl),
    make_aux_pred_decl(ModuleName, MutableName, UnsafeSetPredName,
        [SetArg], purity_impure,
        mutable_pred_std_set, Context, UnsafeSetPredDecl).

    % Define the unsafe get and set predicates, if needed.
    %
:- pred define_unsafe_get_set_preds(module_params::in,
    mutable_target_params::in, string::in, mer_type::in, mer_inst::in,
    mutable_maybe_thread_local::in, prog_context::in, string::in,
    pragma_foreign_proc_attributes::in,
    goal::out, goal::out, list(item_foreign_proc)::out) is det.

define_unsafe_get_set_preds(ModuleParams, TargetParams, MutableName,
        Type, Inst, Local, Context, TargetMutableName, Attrs,
        UnsafeGetExpr, UnsafeSetExpr, ForeignProcs) :-
    ModuleName = ModuleParams ^ mp_module_name,
    Lang = TargetParams ^ tp_target_lang,
    BoxPolicy = TargetParams ^ tp_box_policy,
    varset.new_named_var("X", X, varset.init, VarSetOnlyX),
    set_thread_safe(proc_thread_safe, Attrs, ThreadSafeAttrs),
    set_purity(purity_semipure, ThreadSafeAttrs, UnsafeGetAttrs),
    UnsafeSetAttrs = ThreadSafeAttrs,   % defaults to purity_impure
    (
        Lang = lang_c,
        Trailed = mutable_thread_local_trailed(Local),
        (
            Trailed = mutable_untrailed,
            TrailCode = ""
        ;
            Trailed = mutable_trailed,
            % We have already checked that we are in a
            % trailing grade.
            TrailCode = "MR_trail_current_value(&" ++
                TargetMutableName ++ ");\n"
        ),
        (
            Local = mutable_is_not_thread_local(_Trail),
            UnsafeGetCode = "X = " ++ TargetMutableName ++ ";\n",
            UnsafeSetCode = TargetMutableName ++ " = X;\n"
        ;
            Local = mutable_is_thread_local,
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
            Local = mutable_is_not_thread_local(_Trail),
            UnsafeGetCode = "\tX = " ++ TargetMutableName ++ ";\n",
            UnsafeSetCode = "\t" ++ TargetMutableName ++ " = X;\n"
        ;
            Local = mutable_is_thread_local,
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
            Local = mutable_is_not_thread_local(_Trail),
            UnsafeGetCode = "\tX = " ++ TargetMutableName ++ ";\n",
            UnsafeSetCode = "\t" ++ TargetMutableName ++ " = X;\n"
        ;
            Local = mutable_is_thread_local,
            UnsafeGetCode = "\tX = " ++ TargetMutableName ++ ".get();\n",
            UnsafeSetCode = "\t" ++ TargetMutableName ++ ".set(X);\n"
        )
    ),
    UnsafeGetPredName =
        mutable_unsafe_get_pred_name(ModuleName, MutableName),
    UnsafeSetPredName =
        mutable_unsafe_set_pred_name(ModuleName, MutableName),
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
    UnsafeGetExpr = call_expr(Context, UnsafeGetPredName,
        [variable(X, Context)], purity_semipure),
    UnsafeSetExpr = call_expr(Context, UnsafeSetPredName,
        [variable(X, Context)], purity_impure).

%---------------------------------------------------------------------------%

:- pred declare_constant_get_set_preds(module_name::in, string::in,
    mer_type::in, mer_inst::in, prog_context::in, sym_name::out,
    item_pred_decl_info::out, item_pred_decl_info::out) is det.

declare_constant_get_set_preds(ModuleName, MutableName, Type, Inst,
        Context, InitSetPredName, GetPredDecl, SetPredDecl) :-
    GetArg = type_out(Type, Inst),
    SetArg = type_in(Type, Inst),
    GetPredName = mutable_get_pred_name(ModuleName, MutableName),
    SetPredName = mutable_secret_set_pred_name(ModuleName, MutableName),
    InitSetPredName = SetPredName,
    make_aux_pred_decl(ModuleName, MutableName, GetPredName,
        [GetArg], purity_pure,
        mutable_pred_constant_get, Context, GetPredDecl),
    make_aux_pred_decl(ModuleName, MutableName, SetPredName,
        [SetArg], purity_impure,
        mutable_pred_constant_secret_set, Context, SetPredDecl).

:- pred define_constant_get_set_preds(module_name::in,
    mutable_target_params::in, string::in, mer_inst::in, prog_context::in,
    string::in, pragma_foreign_proc_attributes::in,
    list(item_foreign_proc)::out) is det.

define_constant_get_set_preds(ModuleName, TargetParams, MutableName, Inst,
        Context, TargetMutableName, Attrs, ForeignProcs) :-
    Lang = TargetParams ^ tp_target_lang,
    BoxPolicy = TargetParams ^ tp_box_policy,
    varset.new_named_var("X", X, varset.init, VarSetOnlyX),
    ConstantGetPredName = mutable_get_pred_name(ModuleName, MutableName),
    ConstantSetPredName =
        mutable_secret_set_pred_name(ModuleName, MutableName),
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
        ConstantSetPredName,
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
    ForeignProcs = [ConstantGetForeignProc, ConstantSetForeignProc].

%---------------------------------------------------------------------------%

:- pred declare_nonconstant_get_set_preds(module_name::in, string::in,
    mer_type::in, mer_inst::in, mutable_attach_to_io_state::in,
    prog_context::in, sym_name::out,
    item_pred_decl_info::out, item_pred_decl_info::out,
    list(item_pred_decl_info)::out) is det.

declare_nonconstant_get_set_preds(ModuleName, MutableName, Type, Inst,
        AttachToIO, Context, InitSetPredName,
        GetPredDecl, SetPredDecl, IOPredDecls) :-
    GetArg = type_out(Type, Inst),
    SetArg = type_in(Type, Inst),
    GetPredName = mutable_get_pred_name(ModuleName, MutableName),
    SetPredName = mutable_set_pred_name(ModuleName, MutableName),
    InitSetPredName = SetPredName,
    make_aux_pred_decl(ModuleName, MutableName, GetPredName,
        [GetArg], purity_semipure,
        mutable_pred_std_get, Context, GetPredDecl),
    make_aux_pred_decl(ModuleName, MutableName, SetPredName,
        [SetArg], purity_impure,
        mutable_pred_std_set, Context, SetPredDecl),
    (
        AttachToIO = mutable_dont_attach_to_io_state,
        IOPredDecls = []
    ;
        AttachToIO = mutable_attach_to_io_state,
        IOGetPredName = mutable_get_pred_name(ModuleName, MutableName),
        IOSetPredName = mutable_set_pred_name(ModuleName, MutableName),
        make_aux_pred_decl(ModuleName, MutableName, IOGetPredName,
            [GetArg | io_state_pair], purity_pure,
            mutable_pred_io_get, Context, IOGetPredDecl),
        make_aux_pred_decl(ModuleName, MutableName, IOSetPredName,
            [SetArg | io_state_pair], purity_pure,
            mutable_pred_io_set, Context, IOSetPredDecl),
        IOPredDecls = [IOGetPredDecl, IOSetPredDecl]
    ).

    % Define the standard get and set predicates for a nonconstant mutable.
    % Define the io get and set predicates as well if the mutable
    % is attached to the I/O state.
    %
:- pred define_nonconstant_get_set_preds(module_name::in,
    mutable_target_params::in, string::in,
    mutable_attach_to_io_state::in, prog_context::in,
    maybe({goal, goal})::in, goal::in, goal::in,
    list(item_clause_info)::out) is det.

define_nonconstant_get_set_preds(ModuleName, TargetParams, MutableName,
        AttachToIO, Context,
        MaybeLockUnlockExprs, CallUnsafeGetExpr, CallUnsafeSetExpr,
        ClauseInfos) :-
    Lang = TargetParams ^ tp_target_lang,
    varset.new_named_var("X", X, varset.init, VarSetOnlyX),
    StdGetPredName = mutable_get_pred_name(ModuleName, MutableName),
    StdSetPredName = mutable_set_pred_name(ModuleName, MutableName),
    (
        ( Lang = lang_c
        ; Lang = lang_csharp
        ; Lang = lang_java
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
            Context, item_no_seq_num)
    ),
    (
        AttachToIO = mutable_dont_attach_to_io_state,
        ClauseInfos = [StdGetClauseInfo, StdSetClauseInfo]
    ;
        AttachToIO = mutable_attach_to_io_state,
        varset.new_named_var("IO0", IO0, VarSetOnlyX, VarSetXandIOs0),
        varset.new_named_var("IO", IO, VarSetXandIOs0, VarSetXandIOs),
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
        ClauseInfos = [StdGetClauseInfo, StdSetClauseInfo,
            IOGetClauseInfo, IOSetClauseInfo]
    ).

%---------------------------------------------------------------------------%

:- pred declare_init_pred(module_name::in, string::in, prog_context::in,
    item_pred_decl_info::out) is det.

declare_init_pred(ModuleName, MutableName, Context, InitPredDecl) :-
    InitPredName = mutable_init_pred_name(ModuleName, MutableName),
    make_aux_pred_decl(ModuleName, MutableName, InitPredName, [],
        purity_impure, mutable_pred_init, Context, InitPredDecl).

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
    InitPredName = mutable_init_pred_name(ModuleName, MutableName),
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
    new_user_init_or_final_pred_target_name(ModuleName, "init", SeqNum,
        InitPredName, user_arity(0), TargetName, !PredTargetNames),
    PredNameModesPF = proc_pf_name_modes(pf_predicate, InitPredName, []),
    FPEInfo = pragma_info_foreign_proc_export(Origin, Lang,
        PredNameModesPF, TargetName),
    PragmaFPEInfo = item_pragma_info(FPEInfo, Context, item_no_seq_num).

%---------------------------------------------------------------------------%

:- type mutable_target_params
    --->    mutable_target_params(
                tp_target_lang          :: foreign_language,
                tp_box_policy           :: box_policy
            ).

%---------------------------------------------------------------------------%

    % This predicate decides which auxiliary predicates we need
    % to implement a mutable. The rest of this module just implements
    % the decisions made here, which are recorded in the mutable_target_params.
    %
:- pred get_target_params(globals::in, mutable_target_params::out) is det.

get_target_params(Globals, TargetParams) :-
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
    TargetParams = mutable_target_params(Lang, BoxPolicy).

%---------------------------------------------------------------------------%

    % Returns the name of the mutex associated eith a given mutable.
    % The input to this function is the name of the mutable in the target
    % language.
    %
:- func mutable_mutex_var_name(string) = string.

mutable_mutex_var_name(TargetMutableVarName) = MutexVarName :-
    MutexVarName = TargetMutableVarName ++ "_lock".

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

make_aux_pred_decl(ModuleName, MutableName, PredSymName, ArgTypesAndModes,
        Purity, Kind, Context, PredDecl) :-
    WithType = maybe.no,
    WithMode = maybe.no,
    Origin = compiler_origin_mutable(ModuleName, MutableName, Kind),
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

:- func mutable_lock_pred_name(sym_name, string) = sym_name.
:- func mutable_unlock_pred_name(sym_name, string) = sym_name.
:- func mutable_unsafe_get_pred_name(sym_name, string) = sym_name.
:- func mutable_unsafe_set_pred_name(sym_name, string) = sym_name.
:- func mutable_get_pred_name(sym_name, string) = sym_name.
:- func mutable_set_pred_name(sym_name, string) = sym_name.
:- func mutable_secret_set_pred_name(module_name, string) = sym_name.
:- func mutable_init_pred_name(module_name, string) = sym_name.
:- func mutable_pre_init_pred_name(module_name, string) = sym_name.

mutable_lock_pred_name(ModuleName, MutableName) =
    qualified(ModuleName, "lock_" ++ MutableName).

mutable_unlock_pred_name(ModuleName, MutableName) =
    qualified(ModuleName, "unlock_" ++ MutableName).

mutable_unsafe_get_pred_name(ModuleName, MutableName) =
    qualified(ModuleName, "unsafe_get_" ++ MutableName).

mutable_unsafe_set_pred_name(ModuleName, MutableName) =
    qualified(ModuleName, "unsafe_set_" ++ MutableName).

mutable_get_pred_name(ModuleName, MutableName) =
    qualified(ModuleName, "get_" ++ MutableName).

mutable_set_pred_name(ModuleName, MutableName) =
    qualified(ModuleName, "set_" ++ MutableName).

mutable_secret_set_pred_name(ModuleName, MutableName) =
    qualified(ModuleName, "secret_initialization_only_set_" ++ MutableName).

mutable_init_pred_name(ModuleName, MutableName) =
    qualified(ModuleName, "initialise_mutable_" ++ MutableName).

mutable_pre_init_pred_name(ModuleName, MutableName) =
    qualified(ModuleName, "pre_initialise_mutable_" ++ MutableName).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_mutable.
%---------------------------------------------------------------------------%
