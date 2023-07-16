%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_rename_classes.m.
%
% The MLDS->Java code generator (specifically, mlds_to_java_file.m)
% uses the predicates in this module to shorten any "excessively-long"
% class names. The length matters because class names become part of
% file names, and file systems have limits on their length.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_rename_classes.
:- interface.

:- import_module ml_backend.mlds.

:- import_module map.

:- type class_name_renaming
    --->    class_name_renaming(
                cnr_module      :: mlds_module_name,
                cnr_renaming    :: map(mlds_class_name, mlds_class_name)
            ).

:- pred rename_class_names_in_class_defn(class_name_renaming::in,
    mlds_class_defn::in, mlds_class_defn::out) is det.

:- pred rename_class_names_in_env_defn(class_name_renaming::in,
    mlds_env_defn::in, mlds_env_defn::out) is det.

:- pred rename_class_names_in_function_defn(class_name_renaming::in,
    mlds_function_defn::in, mlds_function_defn::out) is det.

:- pred rename_class_names_in_global_var_defn(class_name_renaming::in,
    mlds_global_var_defn::in, mlds_global_var_defn::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%
%
% Rename in class definitions.
%

rename_class_names_in_class_defn(Renaming, ClassDefn0, ClassDefn) :-
    ClassDefn0 = mlds_class_defn(ClassName, Arity, Context, Flags, ClassKind,
        Imports, Inherits, Implements, TypeParams,
        MemberFields0, MemberClasses0, MemberMethods0, Ctors0),
    list.map(rename_class_names_in_field_var_defn(Renaming),
        MemberFields0, MemberFields),
    list.map(rename_class_names_in_class_defn(Renaming),
        MemberClasses0, MemberClasses),
    list.map(rename_class_names_in_function_defn(Renaming),
        MemberMethods0, MemberMethods),
    list.map(rename_class_names_in_function_defn(Renaming), Ctors0, Ctors),
    ClassDefn = mlds_class_defn(ClassName, Arity, Context, Flags, ClassKind,
        Imports, Inherits, Implements, TypeParams,
        MemberFields, MemberClasses, MemberMethods, Ctors).

rename_class_names_in_env_defn(Renaming, EnvDefn0, EnvDefn) :-
    EnvDefn0 = mlds_env_defn(ClassName, Context, MemberFields0),
    list.map(rename_class_names_in_field_var_defn(Renaming),
        MemberFields0, MemberFields),
    EnvDefn = mlds_env_defn(ClassName, Context, MemberFields).

:- pred rename_class_names_in_field_var_defn(class_name_renaming::in,
    mlds_field_var_defn::in, mlds_field_var_defn::out) is det.

rename_class_names_in_field_var_defn(Renaming, FieldVarDefn0, FieldVarDefn) :-
    FieldVarDefn0 = mlds_field_var_defn(FieldVarName, Context, Flags,
        Type0, Initializer0, GCStmt),
    rename_class_names_in_type(Renaming, Type0, Type),
    rename_class_names_in_initializer(Renaming, Initializer0, Initializer),
    FieldVarDefn = mlds_field_var_defn(FieldVarName, Context, Flags,
        Type, Initializer, GCStmt).

%---------------------------------------------------------------------------%
%
% Rename in function definitions.
%

rename_class_names_in_function_defn(Renaming, FuncDefn0, FuncDefn) :-
    FuncDefn0 = mlds_function_defn(Name, Context, Flags, MaybePredProcId,
        FuncParams0, FuncBody0, EnvVarNames, MaybeRequireTailrecInfo),
    rename_class_names_in_func_params(Renaming, FuncParams0, FuncParams),
    (
        FuncBody0 = body_defined_here(Stmt0),
        rename_class_names_in_stmt(Renaming, Stmt0, Stmt),
        FuncBody = body_defined_here(Stmt)
    ;
        FuncBody0 = body_external,
        FuncBody = body_external
    ),
    FuncDefn = mlds_function_defn(Name, Context, Flags, MaybePredProcId,
        FuncParams, FuncBody, EnvVarNames, MaybeRequireTailrecInfo).

:- pred rename_class_names_in_stmt(class_name_renaming::in,
    mlds_stmt::in, mlds_stmt::out) is det.

rename_class_names_in_stmt(Renaming, !Stmt) :-
    (
        !.Stmt = ml_stmt_block(LocalVarDefns0, FuncDefns0, SubStmts0, Context),
        list.map(rename_class_names_in_local_var_defn(Renaming),
            LocalVarDefns0, LocalVarDefns),
        list.map(rename_class_names_in_function_defn(Renaming),
            FuncDefns0, FuncDefns),
        list.map(rename_class_names_in_stmt(Renaming), SubStmts0, SubStmts),
        !:Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts, Context)
    ;
        !.Stmt = ml_stmt_while(Kind, Rval0, SubStmt0, LoopLocalVars, Context),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        rename_class_names_in_stmt(Renaming, SubStmt0, SubStmt),
        !:Stmt = ml_stmt_while(Kind, Rval, SubStmt, LoopLocalVars, Context)
    ;
        !.Stmt = ml_stmt_if_then_else(Rval0, Then0, MaybeElse0, Context),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        rename_class_names_in_stmt(Renaming, Then0, Then),
        (
            MaybeElse0 = yes(Else0),
            rename_class_names_in_stmt(Renaming, Else0, Else),
            MaybeElse = yes(Else)
        ;
            MaybeElse0 = no,
            MaybeElse = no
        ),
        !:Stmt = ml_stmt_if_then_else(Rval, Then, MaybeElse, Context)
    ;
        !.Stmt = ml_stmt_switch(Type0, Rval0, SwitchRange, Cases0, Default0,
            Context),
        rename_class_names_in_type(Renaming, Type0, Type),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        list.map(rename_class_names_in_switch_case(Renaming), Cases0, Cases),
        rename_class_names_in_switch_default(Renaming, Default0, Default),
        !:Stmt = ml_stmt_switch(Type, Rval, SwitchRange, Cases, Default,
            Context)
    ;
        !.Stmt = ml_stmt_label(_, _)
    ;
        !.Stmt = ml_stmt_goto(_, _)
    ;
        !.Stmt = ml_stmt_computed_goto(Rval0, Labels, Context),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        !:Stmt = ml_stmt_computed_goto(Rval, Labels, Context)
    ;
        !.Stmt = ml_stmt_call(Signature0, Rval0, Rvals0, RetLvals0,
            CallKind, Context),
        Signature0 = mlds_func_signature(ArgTypes0, RetTypes0),
        list.map(rename_class_names_in_type(Renaming), ArgTypes0, ArgTypes),
        list.map(rename_class_names_in_type(Renaming), RetTypes0, RetTypes),
        Signature = mlds_func_signature(ArgTypes, RetTypes),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        list.map(rename_class_names_in_rval(Renaming), Rvals0, Rvals),
        list.map(rename_class_names_in_lval(Renaming), RetLvals0, RetLvals),
        !:Stmt = ml_stmt_call(Signature, Rval, Rvals, RetLvals,
            CallKind, Context)
    ;
        !.Stmt = ml_stmt_return(Rvals0, Context),
        list.map(rename_class_names_in_rval(Renaming), Rvals0, Rvals),
        !:Stmt = ml_stmt_return(Rvals, Context)
    ;
        !.Stmt = ml_stmt_try_commit(Lval0, BodyStmt0, HandlerStmt0, Context),
        rename_class_names_in_lval(Renaming, Lval0, Lval),
        rename_class_names_in_stmt(Renaming, BodyStmt0, BodyStmt),
        rename_class_names_in_stmt(Renaming, HandlerStmt0, HandlerStmt),
        !:Stmt = ml_stmt_try_commit(Lval, BodyStmt, HandlerStmt, Context)
    ;
        !.Stmt = ml_stmt_do_commit(Rval0, Context),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        !:Stmt = ml_stmt_do_commit(Rval, Context)
    ;
        !.Stmt = ml_stmt_atomic(AtomicStmt0, Context),
        rename_class_names_in_atomic(Renaming, AtomicStmt0, AtomicStmt),
        !:Stmt = ml_stmt_atomic(AtomicStmt, Context)
    ).

:- pred rename_class_names_in_local_var_defn(class_name_renaming::in,
    mlds_local_var_defn::in, mlds_local_var_defn::out) is det.

rename_class_names_in_local_var_defn(Renaming, LocalVarDefn0, LocalVarDefn) :-
    LocalVarDefn0 = mlds_local_var_defn(LocalVarName, Context,
        Type0, Initializer0, GCStmt),
    rename_class_names_in_type(Renaming, Type0, Type),
    rename_class_names_in_initializer(Renaming, Initializer0, Initializer),
    LocalVarDefn = mlds_local_var_defn(LocalVarName, Context,
        Type, Initializer, GCStmt).

:- pred rename_class_names_in_switch_case(class_name_renaming::in,
    mlds_switch_case::in, mlds_switch_case::out) is det.

rename_class_names_in_switch_case(Renaming, !Case) :-
    !.Case = mlds_switch_case(FirstMatchCond, LaterMatchConds, Stmt0),
    % The rvals in the match conditions shouldn't need renaming.
    rename_class_names_in_stmt(Renaming, Stmt0, Stmt),
    !:Case = mlds_switch_case(FirstMatchCond, LaterMatchConds, Stmt).

:- pred rename_class_names_in_switch_default(class_name_renaming::in,
    mlds_switch_default::in, mlds_switch_default::out) is det.

rename_class_names_in_switch_default(Renaming, !Default) :-
    (
        !.Default = default_is_unreachable
    ;
        !.Default = default_do_nothing
    ;
        !.Default = default_case(Stmt0),
        rename_class_names_in_stmt(Renaming, Stmt0, Stmt),
        !:Default = default_case(Stmt)
    ).

:- pred rename_class_names_in_atomic(class_name_renaming::in,
    mlds_atomic_statement::in, mlds_atomic_statement::out) is det.

rename_class_names_in_atomic(Renaming, !Stmt) :-
    (
        !.Stmt = assign(Lval0, Rval0),
        rename_class_names_in_lval(Renaming, Lval0, Lval),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        !:Stmt = assign(Lval, Rval)
    ;
        !.Stmt = assign_if_in_heap(Lval0, Rval0),
        rename_class_names_in_lval(Renaming, Lval0, Lval),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        !:Stmt = assign_if_in_heap(Lval, Rval)
    ;
        !.Stmt = delete_object(Rval0),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        !:Stmt = delete_object(Rval)
    ;
        !.Stmt = new_object(TargetLval0, MaybeTag, ExplicitSecTag, Type0,
            MaybeSize, MaybeCtorName, ArgRvalsTypes0, MayUseAtomic, AllocId),
        rename_class_names_in_lval(Renaming, TargetLval0, TargetLval),
        rename_class_names_in_type(Renaming, Type0, Type),
        list.map(rename_class_names_in_typed_rval(Renaming),
            ArgRvalsTypes0, ArgRvalsTypes),
        !:Stmt = new_object(TargetLval, MaybeTag, ExplicitSecTag, Type,
            MaybeSize, MaybeCtorName, ArgRvalsTypes, MayUseAtomic, AllocId)
    ;
        !.Stmt = inline_target_code(Lang, Components0),
        (
            Lang = ml_target_java,
            list.map(rename_class_names_in_target_code_component(Renaming),
                Components0, Components),
            !:Stmt = inline_target_code(Lang, Components)
        ;
            ( Lang = ml_target_c
            ; Lang = ml_target_csharp
            )
        )
    ;
        ( !.Stmt = comment(_)
        ; !.Stmt = gc_check
        ; !.Stmt = mark_hp(_)
        ; !.Stmt = restore_hp(_)
        ; !.Stmt = trail_op(_)
        ; !.Stmt = outline_foreign_proc(_, _, _, _)
        )
    ).

:- pred rename_class_names_in_lval(class_name_renaming::in,
    mlds_lval::in, mlds_lval::out) is det.

rename_class_names_in_lval(Renaming, !Lval) :-
    (
        !.Lval = ml_field(Tag, PtrRval0, PtrType0, FieldId0, FieldType0),
        rename_class_names_in_rval(Renaming, PtrRval0, PtrRval),
        rename_class_names_in_type(Renaming, PtrType0, PtrType),
        rename_class_names_in_field_id(Renaming, FieldId0, FieldId),
        rename_class_names_in_type(Renaming, FieldType0, FieldType),
        !:Lval = ml_field(Tag, PtrRval, PtrType, FieldId, FieldType)
    ;
        !.Lval = ml_mem_ref(Rval0, Type0),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        rename_class_names_in_type(Renaming, Type0, Type),
        !:Lval = ml_mem_ref(Rval, Type)
    ;
        !.Lval = ml_target_global_var_ref(_)
    ;
        !.Lval = ml_global_var(GlobalVar, Type0),
        rename_class_names_in_type(Renaming, Type0, Type),
        !:Lval = ml_global_var(GlobalVar, Type)
    ;
        !.Lval = ml_local_var(LocalVar, Type0),
        rename_class_names_in_type(Renaming, Type0, Type),
        !:Lval = ml_local_var(LocalVar, Type)
    ).

:- pred rename_class_names_in_field_id(class_name_renaming::in,
    mlds_field_id::in, mlds_field_id::out) is det.

rename_class_names_in_field_id(Renaming, !FieldId) :-
    (
        !.FieldId = ml_field_offset(Rval0),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        !:FieldId = ml_field_offset(Rval)
    ;
        !.FieldId = ml_field_named(Name, Type0),
        rename_class_names_in_type(Renaming, Type0, Type),
        !:FieldId = ml_field_named(Name, Type)
    ).

:- pred rename_class_names_in_typed_rval(class_name_renaming::in,
    mlds_typed_rval::in, mlds_typed_rval::out) is det.

rename_class_names_in_typed_rval(Renaming, !TypedRval) :-
    !.TypedRval = ml_typed_rval(Rval0, Type0),
    rename_class_names_in_rval(Renaming, Rval0, Rval),
    rename_class_names_in_type(Renaming, Type0, Type),
    !:TypedRval = ml_typed_rval(Rval, Type).

:- pred rename_class_names_in_rval(class_name_renaming::in,
    mlds_rval::in, mlds_rval::out) is det.

rename_class_names_in_rval(Renaming, !Rval) :-
    (
        !.Rval = ml_lval(Lval0),
        rename_class_names_in_lval(Renaming, Lval0, Lval),
        !:Rval = ml_lval(Lval)
    ;
        !.Rval = ml_mkword(Tag, Rval0),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        !:Rval = ml_mkword(Tag, Rval)
    ;
        !.Rval = ml_const(RvalConst0),
        rename_class_names_in_rval_const(Renaming, RvalConst0, RvalConst),
        !:Rval = ml_const(RvalConst)
    ;
        !.Rval = ml_box(Type0, SubRval0),
        rename_class_names_in_type(Renaming, Type0, Type),
        rename_class_names_in_rval(Renaming, SubRval0, SubRval),
        !:Rval = ml_box(Type, SubRval)
    ;
        !.Rval = ml_unbox(Type0, SubRval0),
        rename_class_names_in_type(Renaming, Type0, Type),
        rename_class_names_in_rval(Renaming, SubRval0, SubRval),
        !:Rval = ml_unbox(Type, SubRval)
    ;
        !.Rval = ml_cast(Type0, SubRval0),
        rename_class_names_in_type(Renaming, Type0, Type),
        rename_class_names_in_rval(Renaming, SubRval0, SubRval),
        !:Rval = ml_cast(Type, SubRval)
    ;
        !.Rval = ml_unop(Op, SubRval0),
        rename_class_names_in_rval(Renaming, SubRval0, SubRval),
        !:Rval = ml_unop(Op, SubRval)
    ;
        !.Rval = ml_binop(Op, SubRvalA0, SubRvalB0),
        rename_class_names_in_rval(Renaming, SubRvalA0, SubRvalA),
        rename_class_names_in_rval(Renaming, SubRvalB0, SubRvalB),
        !:Rval = ml_binop(Op, SubRvalA, SubRvalB)
    ;
        !.Rval = ml_mem_addr(Lval0),
        rename_class_names_in_lval(Renaming, Lval0, Lval),
        !:Rval = ml_mem_addr(Lval)
    ;
        !.Rval = ml_scalar_common(_)
    ;
        !.Rval = ml_scalar_common_addr(_)
    ;
        !.Rval = ml_vector_common_row_addr(VectorCommon, RowRval0),
        rename_class_names_in_rval(Renaming, RowRval0, RowRval),
        !:Rval = ml_vector_common_row_addr(VectorCommon, RowRval)
    ;
        !.Rval = ml_self(Type0),
        rename_class_names_in_type(Renaming, Type0, Type),
        !:Rval = ml_self(Type)
    ).

:- pred rename_class_names_in_rval_const(class_name_renaming::in,
    mlds_rval_const::in, mlds_rval_const::out) is det.

rename_class_names_in_rval_const(Renaming, !Const) :-
    (
        !.Const = mlconst_foreign(Lang, String, Type0),
        rename_class_names_in_type(Renaming, Type0, Type),
        !:Const = mlconst_foreign(Lang, String, Type)
    ;
        !.Const = mlconst_null(Type0),
        rename_class_names_in_type(Renaming, Type0, Type),
        !:Const = mlconst_null(Type)
    ;
        ( !.Const = mlconst_true
        ; !.Const = mlconst_false
        ; !.Const = mlconst_int(_)
        ; !.Const = mlconst_uint(_)
        ; !.Const = mlconst_int8(_)
        ; !.Const = mlconst_uint8(_)
        ; !.Const = mlconst_int16(_)
        ; !.Const = mlconst_uint16(_)
        ; !.Const = mlconst_int32(_)
        ; !.Const = mlconst_uint32(_)
        ; !.Const = mlconst_int64(_)
        ; !.Const = mlconst_uint64(_)
        ; !.Const = mlconst_char(_)
        ; !.Const = mlconst_enum(_, _)
        ; !.Const = mlconst_float(_)
        ; !.Const = mlconst_string(_)
        ; !.Const = mlconst_multi_string(_)
        ; !.Const = mlconst_named_const(_, _)
        ; !.Const = mlconst_code_addr(_)
        ; !.Const = mlconst_data_addr_local_var(_)
        ; !.Const = mlconst_data_addr_global_var(_, _)
        ; !.Const = mlconst_data_addr_rtti(_, _)
        ; !.Const = mlconst_data_addr_tabling(_, _)
        )
    ).

:- pred rename_class_names_in_target_code_component(class_name_renaming::in,
    target_code_component::in, target_code_component::out) is det.

rename_class_names_in_target_code_component(Renaming, !Component) :-
    (
        ( !.Component = user_target_code(_, _)
        ; !.Component = raw_target_code(_)
        ; !.Component = target_code_alloc_id(_)
        ; !.Component = target_code_function_name(_)
        )
    ;
        !.Component = target_code_input(Rval0),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        !:Component = target_code_input(Rval)
    ;
        !.Component = target_code_output(Lval0),
        rename_class_names_in_lval(Renaming, Lval0, Lval),
        !:Component = target_code_output(Lval)
    ;
        !.Component = target_code_type(Type0),
        rename_class_names_in_type(Renaming, Type0, Type),
        !:Component = target_code_type(Type)
    ).

%---------------------------------------------------------------------------%
%
% Rename in global variable definitions.
%

rename_class_names_in_global_var_defn(Renaming,
        GlobalVarDefn0, GlobalVarDefn) :-
    GlobalVarDefn0 = mlds_global_var_defn(GlobalVarName, Context, Flags,
        Type0, Initializer0, GCStmt),
    rename_class_names_in_type(Renaming, Type0, Type),
    rename_class_names_in_initializer(Renaming, Initializer0, Initializer),
    GlobalVarDefn = mlds_global_var_defn(GlobalVarName, Context, Flags,
        Type, Initializer, GCStmt).

%---------------------------------------------------------------------------%
%
% General utility predicates that are useful in implementing more than one
% of the exported predicates.
%

:- pred rename_class_names_in_type(class_name_renaming::in,
    mlds_type::in, mlds_type::out) is det.

rename_class_names_in_type(Renaming, !Type) :-
    (
        !.Type = mlds_mercury_array_type(Type0),
        rename_class_names_in_type(Renaming, Type0, Type),
        !:Type = mlds_mercury_array_type(Type)
    ;
        !.Type = mlds_cont_type(RetTypes0),
        list.map(rename_class_names_in_type(Renaming), RetTypes0, RetTypes),
        !:Type = mlds_cont_type(RetTypes)
    ;
        !.Type = mlds_class_type(ClassId0),
        ClassId0 = mlds_class_id(QualClassName0, Arity, ClassKind),
        QualClassName0 = qual_class_name(ModuleName, QualKind, ClassName0),
        ( if
            Renaming = class_name_renaming(ModuleName, RenamingMap),
            map.search(RenamingMap, ClassName0, ClassName)
        then
            QualClassName = qual_class_name(ModuleName, QualKind, ClassName),
            ClassId = mlds_class_id(QualClassName, Arity, ClassKind),
            !:Type = mlds_class_type(ClassId)
        else
            true
        )
    ;
        !.Type = mlds_enum_class_type(ClassId0),
        ClassId0 = mlds_enum_class_id(ModuleName, ClassName0, Arity),
        ( if
            Renaming = class_name_renaming(ModuleName, RenamingMap),
            map.search(RenamingMap, ClassName0, ClassName)
        then
            ClassId = mlds_enum_class_id(ModuleName, ClassName, Arity),
            !:Type = mlds_enum_class_type(ClassId)
        else
            true
        )
    ;
        !.Type = mlds_env_type(EnvId0),
        EnvId0 = mlds_env_id(ModuleName, ClassName0),
        ( if
            Renaming = class_name_renaming(ModuleName, RenamingMap),
            map.search(RenamingMap, ClassName0, ClassName)
        then
            EnvId = mlds_env_id(ModuleName, ClassName),
            !:Type = mlds_env_type(EnvId)
        else
            true
        )
    ;
        !.Type = mlds_struct_type(StructId0),
        StructId0 = mlds_struct_id(ModuleName, ClassName0),
        ( if
            Renaming = class_name_renaming(ModuleName, RenamingMap),
            map.search(RenamingMap, ClassName0, ClassName)
        then
            StructId = mlds_struct_id(ModuleName, ClassName),
            !:Type = mlds_struct_type(StructId)
        else
            true
        )
    ;
        !.Type = mlds_array_type(Type0),
        rename_class_names_in_type(Renaming, Type0, Type),
        !:Type = mlds_array_type(Type)
    ;
        !.Type = mlds_mostly_generic_array_type(Types0),
        list.map(rename_class_names_in_type(Renaming), Types0, Types),
        !:Type = mlds_mostly_generic_array_type(Types)
    ;
        !.Type = mlds_ptr_type(Type0),
        rename_class_names_in_type(Renaming, Type0, Type),
        !:Type = mlds_ptr_type(Type)
    ;
        !.Type = mlds_func_type(FuncParams0),
        rename_class_names_in_func_params(Renaming, FuncParams0, FuncParams),
        !:Type = mlds_func_type(FuncParams)
    ;
        ( !.Type = mercury_nb_type(_, _)
        ; !.Type = mlds_commit_type
        ; !.Type = mlds_native_bool_type
        ; !.Type = mlds_builtin_type_int(_)
        ; !.Type = mlds_builtin_type_float
        ; !.Type = mlds_builtin_type_string
        ; !.Type = mlds_builtin_type_char
        ; !.Type = mlds_foreign_type(_)
        ; !.Type = mlds_generic_type
        ; !.Type = mlds_generic_env_ptr_type
        ; !.Type = mlds_type_info_type
        ; !.Type = mlds_pseudo_type_info_type
        ; !.Type = mlds_rtti_type(_)
        ; !.Type = mlds_tabling_type(_)
        ; !.Type = mlds_unknown_type
        )
    ).

%---------------------%

:- pred rename_class_names_in_initializer(class_name_renaming::in,
    mlds_initializer::in, mlds_initializer::out) is det.

rename_class_names_in_initializer(Renaming, !Initializer) :-
    (
        !.Initializer = init_obj(Rval0),
        rename_class_names_in_rval(Renaming, Rval0, Rval),
        !:Initializer = init_obj(Rval)
    ;
        !.Initializer = init_struct(Type0, Initializers0),
        rename_class_names_in_type(Renaming, Type0, Type),
        list.map(rename_class_names_in_initializer(Renaming), Initializers0,
            Initializers),
        !:Initializer = init_struct(Type, Initializers)
    ;
        !.Initializer = init_array(Initializers0),
        list.map(rename_class_names_in_initializer(Renaming), Initializers0,
            Initializers),
        !:Initializer = init_array(Initializers)
    ;
        !.Initializer = no_initializer
    ).

%---------------------%

:- pred rename_class_names_in_func_params(class_name_renaming::in,
    mlds_func_params::in, mlds_func_params::out) is det.

rename_class_names_in_func_params(Renaming, !FuncParams) :-
    !.FuncParams = mlds_func_params(Arguments0, RetTypes0),
    list.map(rename_class_names_in_argument(Renaming), Arguments0, Arguments),
    list.map(rename_class_names_in_type(Renaming), RetTypes0, RetTypes),
    !:FuncParams = mlds_func_params(Arguments, RetTypes).

:- pred rename_class_names_in_argument(class_name_renaming::in,
    mlds_argument::in, mlds_argument::out) is det.

rename_class_names_in_argument(Renaming, !Argument) :-
    !.Argument = mlds_argument(Name, Type0, GCStmt),
    rename_class_names_in_type(Renaming, Type0, Type),
    !:Argument = mlds_argument(Name, Type, GCStmt).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_rename_classes.
%---------------------------------------------------------------------------%
