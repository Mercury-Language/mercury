%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Code to create wrapper classes for the implementation of function pointers
% in Java.
%
% As there is no way to take the address of a method in Java, we must create a
% wrapper for that method which implements a common interface. We are then able
% to pass that class around as a java.lang.Object.
%
% XXX This implementation will not handle taking the address of instance
% methods. This is not currently a problem as they will never be generated
% by the MLDS back-end.
%
% XXX This implementation will not correctly handle the case which occurs where
% there are two or more overloaded MLDS functions (that we take the address of)
% with the same name and arity but different argument types, both in the same
% module. This is due to the fact that the names of the generated wrapper
% classes are based purely on the method name.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_java_wrap.
:- interface.

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_java_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module pair.

%---------------------------------------------------------------------------%

:- pred generate_addr_wrapper_class(mlds_module_name::in,
    pair(arity, list(mlds_code_addr))::in, mlds_class_defn::out,
    map(mlds_code_addr, code_addr_wrapper)::in,
    map(mlds_code_addr, code_addr_wrapper)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_type_gen.
:- import_module parse_tree.java_names.

:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

generate_addr_wrapper_class(MLDS_ModuleName, Arity - CodeAddrs, ClassDefn,
        !AddrOfMap) :-
    % Create a name for this wrapper class based on the fully qualified method
    % (predicate) name.
    ClassName = "addrOf" ++ string.from_int(Arity),

    % If the class is wrapping more than one method, then add a member variable
    % which says which predicate to call, and a constructor function to
    % initialise that variable.
    (
        CodeAddrs = [],
        unexpected($pred, "no addresses")
    ;
        CodeAddrs = [_],
        FieldVarDefns = [],
        CtorDefns = []
    ;
        CodeAddrs = [_, _ | _],
        Context = term.context_init,

        % Create the member variable.
        CtorArgName = lvn_field_var_as_local(fvn_ptr_num),
        FieldVarDefn = mlds_field_var_defn(
            fvn_env_field_from_local_var(CtorArgName), Context,
            ml_gen_const_member_data_decl_flags, mlds_native_int_type,
            no_initializer, gc_no_stmt),
        FieldVarDefns = [FieldVarDefn],

        % Create the constructor function.
        QualClassName =
            qual_class_name(MLDS_ModuleName, module_qual, ClassName),
        ClassType =
            mlds_class_type(mlds_class_id(QualClassName, 0, mlds_class)),

        FieldName =
            qual_field_var_name(MLDS_ModuleName, type_qual, fvn_ptr_num),
        FieldId = ml_field_named(FieldName, ClassType),
        FieldLval = ml_field(no, ml_self(ClassType), ClassType,
            FieldId, mlds_native_int_type),

        CtorArgs = [mlds_argument(CtorArgName, mlds_native_int_type,
            gc_no_stmt)],
        CtorReturnValues = [],

        CtorArgLval = ml_local_var(CtorArgName, mlds_native_int_type),
        CtorArgRval = ml_lval(CtorArgLval),
        CtorStmt = ml_stmt_atomic(assign(FieldLval, CtorArgRval), Context),

        CtorFunctionName = mlds_function_export("<constructor>"),
        CtorFlags = mlds_function_decl_flags(func_public, per_instance),
        Params = mlds_func_params(CtorArgs, CtorReturnValues),
        EnvVarNames = set.init,
        CtorDefn = mlds_function_defn(CtorFunctionName, Context, CtorFlags,
            no, Params, body_defined_here(CtorStmt), EnvVarNames, no),
        CtorDefns = [CtorDefn]
    ),

    % Create a method that calls the original predicates.
    generate_call_method(Arity, CodeAddrs, MethodDefn),

    ( if is_specialised_method_ptr_arity(Arity) then
        InterfaceName = "MethodPtr" ++ string.from_int(Arity)
    else
        InterfaceName = "MethodPtrN"
    ),
    InterfaceModuleName = mercury_module_name_to_mlds(
        java_mercury_runtime_package_name),
    Interface =
        qual_class_name(InterfaceModuleName, module_qual, InterfaceName),

    % Create class components.
    ClassImports = [],
    ClassInherits = inherits_nothing,
    InterfaceId = mlds_interface_id(Interface, 0, mlds_interface),
    ClassImplements = [InterfaceId],
    TypeParams = [],

    % Put it all together.
    ClassContext = term.context_init,
    ClassFlags = mlds_class_decl_flags(class_private, sealed, const),
    ClassDefn = mlds_class_defn(ClassName, 0, ClassContext, ClassFlags,
        mlds_class, ClassImports, ClassInherits, ClassImplements,
        TypeParams, FieldVarDefns, [], [MethodDefn], CtorDefns),

    add_to_address_map(ClassName, CodeAddrs, !AddrOfMap).

:- pred generate_call_method(arity::in, list(mlds_code_addr)::in,
    mlds_function_defn::out) is det.

generate_call_method(Arity, CodeAddrs, MethodDefn) :-
    % Create the arguments to the call method. For low arities, the method
    % takes n arguments directly. For higher arities, the arguments are
    % passed in as an array.
    ( if is_specialised_method_ptr_arity(Arity) then
        list.map2(create_generic_arg, 1 .. Arity, ArgNames, MethodArgs),
        InputArgs = cmi_separate(ArgNames)
    else
        ArgName = lvn_comp_var(lvnc_args),
        ArgType = mlds_array_type(mlds_generic_type),
        Arg = mlds_argument(ArgName, ArgType, gc_no_stmt),
        MethodArgs = [Arg],
        InputArgs = cmi_array(ArgName)
    ),

    % Create a statement to call each of the original methods.
    list.map(generate_call_statement_for_addr(InputArgs), CodeAddrs,
        CodeAddrStmts),

    Context = term.context_init,

    % If there is more than one original method, then we need to switch on the
    % ptr_num member variable.
    (
        CodeAddrStmts = [],
        unexpected($pred, "no statements")
    ;
        CodeAddrStmts = [Stmt]
    ;
        CodeAddrStmts = [_, _ | _],
        MaxCase = list.length(CodeAddrs) - 1,
        MakeCase =
            ( func(I, CaseStmt) = Case :-
                MatchCond = match_value(ml_const(mlconst_int(I))),
                Case = mlds_switch_case(MatchCond, [], CaseStmt)
            ),
        Cases = list.map_corresponding(MakeCase, 0 .. MaxCase, CodeAddrStmts),

        SwitchVarName = lvn_field_var_as_local(fvn_ptr_num),
        SwitchVarRval =
            ml_lval(ml_local_var(SwitchVarName, mlds_native_int_type)),
        SwitchRange = mlds_switch_range(0, MaxCase),
        Stmt = ml_stmt_switch(mlds_native_int_type, SwitchVarRval,
            SwitchRange, Cases, default_is_unreachable, Context)
    ),

    % Create new method name.
    PredId = hlds_pred.initial_pred_id,
    ProcId = initial_proc_id,
    PredLabel = mlds_special_pred_label("call", no, "", 0),
    ProcLabel = mlds_proc_label(PredLabel, ProcId),
    FuncLabel = mlds_func_label(ProcLabel, proc_func),
    PlainFuncName = mlds_plain_func_name(FuncLabel, PredId),
    MethodName = mlds_function_name(PlainFuncName),

    % Create return type.
    MethodRetType = mlds_generic_type,
    MethodRets = [MethodRetType],

    % Put it all together.
    MethodFlags = ml_gen_member_decl_flags,
    MethodParams = mlds_func_params(MethodArgs, MethodRets),
    MethodMaybeId = no,
    MethodEnvVarNames = set.init,
    MethodDefn = mlds_function_defn(MethodName, Context, MethodFlags,
        MethodMaybeId, MethodParams, body_defined_here(Stmt),
        MethodEnvVarNames, no).

:- pred create_generic_arg(int::in, mlds_local_var_name::out,
    mlds_argument::out) is det.

create_generic_arg(I, ArgName, Arg) :-
    ArgName = lvn_comp_var(lvnc_arg(I)),
    Arg = mlds_argument(ArgName, mlds_generic_type, gc_no_stmt).

:- type call_method_inputs
    --->    cmi_separate(list(mlds_local_var_name))
    ;       cmi_array(mlds_local_var_name).

:- pred generate_call_statement_for_addr(call_method_inputs::in,
    mlds_code_addr::in, mlds_stmt::out) is det.

generate_call_statement_for_addr(InputArgs, CodeAddr, Stmt) :-
    CodeAddr = mlds_code_addr(_QualFuncLabel, OrigFuncSignature),
    OrigFuncSignature = mlds_func_signature(OrigArgTypes, OrigRetTypes),

    % Create the arguments to pass to the original method.
    (
        InputArgs = cmi_separate(ArgNames),
        list.map_corresponding(generate_call_method_nth_arg,
            OrigArgTypes, ArgNames, CallArgs)
    ;
        InputArgs = cmi_array(ArrayVarName),
        generate_call_method_args_from_array(OrigArgTypes, ArrayVarName, 0,
            [], CallArgs)
    ),


    % Create a temporary variable to store the result of the call to the
    % original method.
    ReturnVarName = lvn_comp_var(lvnc_return_value),
    (
        OrigRetTypes = [],
        ReturnVarType = mlds_generic_type
    ;
        OrigRetTypes = [CallRetType],
        ReturnVarType = CallRetType
    ;
        OrigRetTypes = [_, _ | _],
        ReturnVarType = mlds_array_type(mlds_generic_type)
    ),
    ReturnLval = ml_local_var(ReturnVarName, ReturnVarType),

    Context = term.context_init,
    GCStmt = gc_no_stmt,  % The Java back-end does its own GC.
    ReturnVarDefn = mlds_local_var_defn(ReturnVarName, Context,
        ReturnVarType, no_initializer, GCStmt),

    % Create the call to the original method.
    CallRval = ml_const(mlconst_code_addr(CodeAddr)),

    % If the original method has a return type of void, then we obviously
    % cannot assign its return value to "return_value". Thus, in this
    % case, the value returned by the call method will just be the value
    % that "return_value" was initialised to.
    (
        OrigRetTypes = [],
        CallRetLvals = []
    ;
        OrigRetTypes = [_ | _],
        CallRetLvals = [ReturnLval]
    ),
    CallStmt = ml_stmt_call(OrigFuncSignature, CallRval, CallArgs,
        CallRetLvals, ordinary_call, Context),

    % Create a return statement that returns the result of the call to the
    % original method, boxed as a java.lang.Object.
    ReturnRval = ml_box(ReturnVarType, ml_lval(ReturnLval)),
    ReturnStmt = ml_stmt_return([ReturnRval], Context),

    Stmt = ml_stmt_block([ReturnVarDefn], [], [CallStmt, ReturnStmt], Context).

:- pred generate_call_method_nth_arg(mlds_type::in,
    mlds_local_var_name::in, mlds_rval::out) is det.

generate_call_method_nth_arg(Type, MethodArgVariable, CallArg) :-
    Rval = ml_lval(ml_local_var(MethodArgVariable, mlds_generic_type)),
    CallArg = ml_unbox(Type, Rval).

:- pred generate_call_method_args_from_array(list(mlds_type)::in,
    mlds_local_var_name::in, int::in,
    list(mlds_rval)::in, list(mlds_rval)::out) is det.

generate_call_method_args_from_array([], _, _, Args, Args).
generate_call_method_args_from_array([Type | Types], ArrayVar, Counter,
        Args0, Args) :-
    ArrayRval = ml_lval(ml_local_var(ArrayVar, mlds_native_int_type)),
    IndexRval = ml_const(mlconst_int(Counter)),
    ElemType = array_elem_scalar(scalar_elem_generic),
    Rval = ml_binop(array_index(ElemType), ArrayRval, IndexRval),
    UnBoxedRval = ml_unbox(Type, Rval),
    Args1 = Args0 ++ [UnBoxedRval],
    generate_call_method_args_from_array(Types, ArrayVar, Counter + 1,
        Args1, Args).

:- pred add_to_address_map(string::in, list(mlds_code_addr)::in,
    map(mlds_code_addr, code_addr_wrapper)::in,
    map(mlds_code_addr, code_addr_wrapper)::out) is det.

add_to_address_map(ClassName, CodeAddrs, !AddrOfMap) :-
    FlippedClassName = flip_initial_case(ClassName),
    (
        CodeAddrs = [],
        unexpected($pred, "no addresses")
    ;
        CodeAddrs = [CodeAddr],
        Wrapper = code_addr_wrapper(FlippedClassName, no),
        map.det_insert(CodeAddr, Wrapper, !AddrOfMap)
    ;
        CodeAddrs = [_, _ | _],
        add_to_address_map_2(FlippedClassName, CodeAddrs, 0, !AddrOfMap)
    ).

:- pred add_to_address_map_2(string::in, list(mlds_code_addr)::in, int::in,
    map(mlds_code_addr, code_addr_wrapper)::in,
    map(mlds_code_addr, code_addr_wrapper)::out) is det.

add_to_address_map_2(_, [], _, !AddrOfMap).
add_to_address_map_2(FlippedClassName, [CodeAddr | CodeAddrs], I,
        !AddrOfMap) :-
    Wrapper = code_addr_wrapper(FlippedClassName, yes(I)),
    map.det_insert(CodeAddr, Wrapper, !AddrOfMap),
    add_to_address_map_2(FlippedClassName, CodeAddrs, I + 1, !AddrOfMap).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_java_wrap.
%---------------------------------------------------------------------------%
