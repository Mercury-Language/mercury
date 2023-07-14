%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output the code that exports Mercury functions and types to C.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_c_export.
:- interface.

:- import_module libs.
:- import_module libs.indent.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c_util.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred mlds_output_pragma_export_defn(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in,
    mlds_module_name::in, mlds_pragma_export::in, io::di, io::uo) is det.

:- pred mlds_output_export_enums(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, list(mlds_exported_enum)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module backend_libs.rtti.
:- import_module libs.globals.
:- import_module ml_backend.mlds_to_c_func.
:- import_module ml_backend.mlds_to_c_name.
:- import_module ml_backend.mlds_to_c_type.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.

mlds_output_pragma_export_defn(Opts, Stream, Indent,
        ModuleName, PragmaExport, !IO) :-
    PragmaExport = ml_pragma_export(Lang, _ExportName, MLDS_Name,
        MLDS_Signature, _UnivQTVars, Context),
    expect(unify(Lang, lang_c), $pred,
        "foreign_export to language other than C."),
    io.nl(Stream, !IO),
    mlds_output_pragma_export_func_name(Opts, Stream, Indent,
        ModuleName, PragmaExport, !IO),
    io.nl(Stream, !IO),
    c_output_context(Stream, Opts ^ m2co_foreign_line_numbers, Context, !IO),
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    c_output_context(Stream, Opts ^ m2co_foreign_line_numbers, Context, !IO),
    io.format(Stream, "%s", [s(IndentStr)], !IO),
    mlds_output_pragma_export_defn_body(Opts, Stream, MLDS_Name,
        MLDS_Signature, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- pred mlds_output_pragma_export_func_name(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, mlds_module_name::in,
    mlds_pragma_export::in, io::di, io::uo) is det.

mlds_output_pragma_export_func_name(Opts, Stream, Indent,
        ModuleName, Export, !IO) :-
    Export = ml_pragma_export(Lang, ExportName, _MLDSName, Signature,
        _UnivQTVars, Context),
    expect(unify(Lang, lang_c), $pred, "export to language other than C."),
    FuncName = mlds_function_export(ExportName),
    QualFuncName = qual_function_name(ModuleName, FuncName),
    c_output_context(Stream, Opts ^ m2co_foreign_line_numbers, Context, !IO),
    write_indent2(Stream, Indent, !IO),
    % For functions exported using `pragma foreign_export',
    % we use the default C calling convention.
    CallingConvention = "",
    mlds_output_func_decl_ho(Opts, Stream,
        export_type_to_prefix_suffix_ignore_opts, CallingConvention,
        Indent, Context, QualFuncName, Signature, !IO).

:- pred export_type_to_prefix_suffix_ignore_opts(mlds_to_c_opts::in,
    mlds_type::in, string::out, string::out) is det.

export_type_to_prefix_suffix_ignore_opts(_Opts, MLDS_Type,
        TypePrefix, TypeSuffix) :-
    export_type_to_prefix_suffix(MLDS_Type, TypePrefix, TypeSuffix).

:- func export_type_to_string_for_c(mlds_type) = string.

export_type_to_string_for_c(MLDS_Type) = TypeStr :-
    export_type_to_prefix_suffix(MLDS_Type, TypePrefix, TypeSuffix),
    ( if TypeSuffix = "" then
        TypeStr = TypePrefix
    else
        unexpected($pred, "TypeSuffix is not empty")
    ).

    % This predicate always return an empty string as the type suffix.
    % We nevertheless return a suffix, because mlds_output_func_decl_ho,
    % which is called above, requires a predicate that returns a suffix.
    % (Some of its *other* callers give it predicates that *can* return
    % a nonempty suffix.)
    %
:- pred export_type_to_prefix_suffix(mlds_type::in,
    string::out, string::out) is det.

export_type_to_prefix_suffix(MLDS_Type, TypePrefix, TypeSuffix) :-
    (
        MLDS_Type = mlds_mercury_array_type(_ElemType),
        TypePrefix = "MR_ArrayPtr"
    ;
        MLDS_Type = mercury_nb_type(MerType, _),
        % XXX We should not need to call this function.
        TypePrefix = maybe_foreign_type_to_c_string(MerType, no)
    ;
        ( MLDS_Type = mlds_cont_type(_)
        ; MLDS_Type = mlds_commit_type
        ; MLDS_Type = mlds_class_type(_)
        ; MLDS_Type = mlds_enum_class_type(_)
        ; MLDS_Type = mlds_env_type(_)
        ; MLDS_Type = mlds_array_type(_)
        ; MLDS_Type = mlds_mostly_generic_array_type(_)
        ; MLDS_Type = mlds_func_type(_)
        ; MLDS_Type = mlds_generic_type
        ; MLDS_Type = mlds_generic_env_ptr_type
        ; MLDS_Type = mlds_type_info_type
        ; MLDS_Type = mlds_pseudo_type_info_type
        ; MLDS_Type = mlds_rtti_type(_)
        ),
        TypePrefix = "MR_Word"
    ;
        MLDS_Type = mlds_native_bool_type,
        TypePrefix = "MR_bool"
    ;
        MLDS_Type = mlds_builtin_type_int(IntType),
        TypePrefix =
            exported_builtin_type_to_c_string(builtin_type_int(IntType))
    ;
        MLDS_Type = mlds_builtin_type_float,
        TypePrefix = exported_builtin_type_to_c_string(builtin_type_float)
    ;
        MLDS_Type = mlds_builtin_type_string,
        TypePrefix = exported_builtin_type_to_c_string(builtin_type_string)
    ;
        MLDS_Type = mlds_builtin_type_char,
        TypePrefix = exported_builtin_type_to_c_string(builtin_type_char)
    ;
        MLDS_Type = mlds_foreign_type(ForeignType),
        (
            ForeignType = c(c_type(TypePrefix))
        ;
            ForeignType = java(_),
            unexpected($pred, "java foreign_type")
        ;
            ForeignType = csharp(_),
            unexpected($pred, "csharp foreign_type")
        )
    ;
        MLDS_Type = mlds_ptr_type(BaseType),
        BaseTypeStr = export_type_to_string_for_c(BaseType),
        string.format("%s *", [s(BaseTypeStr)], TypePrefix)
    ;
        MLDS_Type = mlds_tabling_type(_),
        % These types should never occur in procedures exported to C,
        % so the fact that we could generate a more accurate type
        % should not matter.
        TypePrefix = "MR_Word"
    ;
        MLDS_Type = mlds_unknown_type,
        unexpected($pred, "unknown_type")
    ),
    TypeSuffix = "".

    % Output the definition body for a pragma foreign_export.
    %
:- pred mlds_output_pragma_export_defn_body(mlds_to_c_opts::in,
    io.text_output_stream::in, qual_function_name::in, mlds_func_params::in,
    io::di, io::uo) is det.

mlds_output_pragma_export_defn_body(Opts, Stream, FuncName, Signature, !IO) :-
    Signature = mlds_func_params(Parameters, ReturnTypes),

    % Declare local variables corresponding to any foreign_type parameters.
    IsCForeignType =
        ( pred(Arg::in) is semidet :-
            Arg = mlds_argument(_Name, Type, _GCStmt),
            Type = mlds_foreign_type(c(_))
        ),
    IsCForeignTypePtr =
        ( pred(Arg::in) is semidet :-
            Arg = mlds_argument(_Name, Type, _GCStmt),
            Type = mlds_ptr_type(mlds_foreign_type(c(_)))
        ),
    CForeignTypeInputs = list.filter(IsCForeignType, Parameters),
    CForeignTypeOutputs = list.filter(IsCForeignTypePtr, Parameters),

    CForeignTypeInputDecls = list.map(pragma_input_arg_to_decl(Opts),
        CForeignTypeInputs),
    CForeignTypeOutputDecls = list.map(pragma_output_arg_to_decl(Opts),
        CForeignTypeOutputs),

    % Generate code to box any non-word-sized foreign_type input parameters;
    % these need to be converted to a uniform size before passing them
    % to Mercury code.
    InputUnboxStrs =
        list.map(pragma_input_arg_to_box_string, CForeignTypeInputs),
    % Generate code to unbox any foreign_type output parameters,
    % since we are returning those parameters to C code.
    OutputBoxStrs =
        list.map(pragma_output_arg_to_unbox_string, CForeignTypeOutputs),

    list.foldl(io.write_string(Stream), CForeignTypeInputDecls, !IO),
    list.foldl(io.write_string(Stream), CForeignTypeOutputDecls, !IO),

    % The structure of each path through this switch is
    %
    % - Declare a local variable or two for the return value, if there is one.
    % - Call the Mercury procedure which is being exported.
    % - If there is a return value, return it after any needed unboxing.
    (
        ReturnTypes = [],
        list.foldl(io.write_string(Stream), InputUnboxStrs, !IO),
        io.write_string(Stream, "\t", !IO),
        mlds_output_pragma_export_call(Opts, Stream, FuncName,
            Parameters, !IO),
        list.foldl(io.write_string(Stream), OutputBoxStrs, !IO)
    ;
        ReturnTypes = [ReturnType],
        ReturnTypeStr = export_type_to_string_for_c(ReturnType),
        ( if ReturnType = mlds_foreign_type(c(_)) then
            io.format(Stream, "\t%s ret_value;\n", [s(ReturnTypeStr)], !IO),
            BoxedReturnTypeStr = type_to_string_for_c(Opts, ReturnType),
            io.format(Stream, "\t%s boxed_ret_value;\n",
                [s(BoxedReturnTypeStr)], !IO),
            list.foldl(io.write_string(Stream), InputUnboxStrs, !IO),
            io.write_string(Stream, "\tboxed_ret_value = ", !IO),
            mlds_output_pragma_export_call(Opts, Stream, FuncName,
                Parameters, !IO),
            list.foldl(io.write_string(Stream), OutputBoxStrs, !IO),
            io.format(Stream,
                "\tMR_MAYBE_UNBOX_FOREIGN_TYPE(%s, %s, %s);\n",
                [s(ReturnTypeStr), s("boxed_ret_value"), s("ret_value")], !IO),
            io.write_string(Stream, "\treturn ret_value;\n", !IO)
        else
            io.format(Stream, "\t%s ret_value;\n", [s(ReturnTypeStr)], !IO),
            list.foldl(io.write_string(Stream), InputUnboxStrs, !IO),
            io.format(Stream, "\tret_value = (%s)", [s(ReturnTypeStr)], !IO),
            mlds_output_pragma_export_call(Opts, Stream, FuncName,
                Parameters, !IO),
            list.foldl(io.write_string(Stream), OutputBoxStrs, !IO),
            io.write_string(Stream, "\treturn ret_value;\n", !IO)
        )
    ;
        ReturnTypes = [_, _ | _],
        % Since C does not support functions that return more than one value,
        % this arm is just for MLDS dumps when compiling to non-C targets.
        % So we do not need to worry about boxing/unboxing foreign types here.
        % NOTE Yet we do write out InputUnboxStrs and OutputBoxStrs.
        %
        list.foldl(io.write_string(Stream), InputUnboxStrs, !IO),
        ReturnTypeStrs = list.map(export_type_to_string_for_c, ReturnTypes),
        ReturnTypesStr = return_list_to_string_for_c(ReturnTypeStrs),
        io.format(Stream, "\treturn (%s);", [s(ReturnTypesStr)], !IO),
        list.foldl(io.write_string(Stream), OutputBoxStrs, !IO)
    ).

%---------------------%

:- func pragma_input_arg_to_decl(mlds_to_c_opts, mlds_argument) = string.

pragma_input_arg_to_decl(Opts, Arg) = DeclStr :-
    Arg = mlds_argument(LocalVarName, Type, _GCStmt),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    BoxedLocalVarNameStr = local_var_name_to_string_for_c(BoxedLocalVarName),
    type_to_prefix_suffix_for_c(Opts, Type, no_size, TypePrefix, TypeSuffix),
    string.format("\t%s %s%s;\n",
        [s(TypePrefix), s(BoxedLocalVarNameStr), s(TypeSuffix)], DeclStr).

:- func pragma_output_arg_to_decl(mlds_to_c_opts, mlds_argument) = string.

pragma_output_arg_to_decl(Opts, Arg) = DeclStr :-
    Arg = mlds_argument(LocalVarName, PtrType, _GCStmt),
    Type = pointed_to_type(PtrType),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    BoxedLocalVarNameStr = local_var_name_to_string_for_c(BoxedLocalVarName),
    type_to_prefix_suffix_for_c(Opts, Type, no_size, TypePrefix, TypeSuffix),
    string.format("\t%s %s%s;\n",
        [s(TypePrefix), s(BoxedLocalVarNameStr), s(TypeSuffix)], DeclStr).

%---------------------%

:- func pragma_input_arg_to_box_string(mlds_argument) = string.

pragma_input_arg_to_box_string(Arg) = BoxStr :-
    Arg = mlds_argument(LocalVarName, Type, _GCStmt),
    TypeStr = export_type_to_string_for_c(Type),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    LocalVarNameStr = local_var_name_to_string_for_c(LocalVarName),
    BoxedLocalVarNameStr = local_var_name_to_string_for_c(BoxedLocalVarName),
    string.format("\tMR_MAYBE_BOX_FOREIGN_TYPE(%s, %s, %s);\n",
        [s(TypeStr), s(LocalVarNameStr), s(BoxedLocalVarNameStr)], BoxStr).

:- func pragma_output_arg_to_unbox_string(mlds_argument) = string.

pragma_output_arg_to_unbox_string(Arg) = UnboxStr :-
    Arg = mlds_argument(LocalVarName, PtrType, _GCStmt),
    Type = pointed_to_type(PtrType),
    TypeStr = export_type_to_string_for_c(Type),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    BoxedLocalVarNameStr = local_var_name_to_string_for_c(BoxedLocalVarName),
    LocalVarNameStr = local_var_name_to_string_for_c(LocalVarName),
    string.format("\tMR_MAYBE_UNBOX_FOREIGN_TYPE(%s, %s, *%s);\n",
        [s(TypeStr), s(BoxedLocalVarNameStr), s(LocalVarNameStr)], UnboxStr).

%---------------------%

:- func pointed_to_type(mlds_type) = mlds_type.

pointed_to_type(PtrType) =
    ( if PtrType = mlds_ptr_type(Type) then
        Type
    else
        unexpected($pred, "not pointer")
    ).

:- pred get_boxed_local_var_name(mlds_local_var_name::in,
    mlds_local_var_name::out) is det.

get_boxed_local_var_name(VarName, BoxedVarName) :-
    ( if VarName = lvn_prog_var(Name, Seq) then
        BoxedVarName = lvn_prog_var_boxed(Name, Seq)
    else
        NameStr = ml_local_var_name_to_string(VarName),
        BoxedVarName = lvn_comp_var(lvnc_non_prog_var_boxed(NameStr))
    ).

%---------------------%

:- pred mlds_output_pragma_export_call(mlds_to_c_opts::in,
    io.text_output_stream::in, qual_function_name::in, list(mlds_argument)::in,
    io::di, io::uo) is det.

mlds_output_pragma_export_call(Opts, Stream, FuncName, Parameters, !IO) :-
    FuncNameStr = qual_function_name_to_string_for_c(FuncName),
    ParameterStrs = list.map(pragma_export_arg_to_string(Opts), Parameters),
    ParametersStr = string.join_list(", ", ParameterStrs),
    io.format(Stream, "%s(%s);\n", [s(FuncNameStr), s(ParametersStr)], !IO).

    % Output a fully qualified name preceded by a cast.
    %
:- func pragma_export_arg_to_string(mlds_to_c_opts, mlds_argument) = string.

pragma_export_arg_to_string(Opts, Arg) = ArgStr :-
    Arg = mlds_argument(LocalVarName, Type, _GCStmt),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    ( if Type = mlds_foreign_type(c(_)) then
        % This is a foreign_type input. Pass in the already-boxed value.
        ArgStr = local_var_name_to_string_for_c(BoxedLocalVarName)
    else if Type = mlds_ptr_type(mlds_foreign_type(c(_))) then
        % This is a foreign_type output. Pass in the address of the
        % local variable which will hold the boxed value.
        ArgStr = "&" ++ local_var_name_to_string_for_c(BoxedLocalVarName)
    else
        % Otherwise, no boxing or unboxing is needed.
        % Just cast the argument to the right type.
        CastPrefix = cast_to_prefix_string_for_c(Opts, Type),
        LocalVarNameStr = local_var_name_to_string_for_c(LocalVarName),
        ArgStr = CastPrefix ++ LocalVarNameStr
    ).

%---------------------------------------------------------------------------%

mlds_output_export_enums(Opts, Stream, Indent, ExportedEnums, !IO) :-
    list.foldl(mlds_output_export_enum(Opts, Stream, Indent),
        ExportedEnums, !IO).

:- pred mlds_output_export_enum(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, mlds_exported_enum::in, io::di, io::uo) is det.

mlds_output_export_enum(Opts, Stream, _Indent, ExportedEnum, !IO) :-
    ExportedEnum = mlds_exported_enum(Lang, Context, _TypeCtor,
        ExportConstants),
    (
        Lang = lang_c,
        c_output_context(Stream, Opts ^ m2co_foreign_line_numbers,
            Context, !IO),
        list.foldl(mlds_output_exported_enum_constant_hashdef(Stream),
            ExportConstants, !IO)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        )
    ).

:- pred mlds_output_exported_enum_constant_hashdef(io.text_output_stream::in,
    mlds_exported_enum_constant::in, io::di, io::uo) is det.

mlds_output_exported_enum_constant_hashdef(Stream, ExportedConstant, !IO) :-
    ExportedConstant = mlds_exported_enum_constant(Name, Initializer),
    ( if
        Initializer = init_obj(ml_const(mlconst_enum(N, _)))
    then
        io.format(Stream, "#define %s %d\n", [s(Name), i(N)], !IO)
    else if
        Initializer = init_obj(ml_const(mlconst_foreign(Lang, Value, _)))
    then
        expect(unify(Lang, lang_c), $pred,
            "mlconst_foreign for language other than C."),
        io.format(Stream, "#define %s %s\n", [s(Name), s(Value)], !IO)
    else
        unexpected($pred, "tag for export enumeration is not enum or foreign")
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_export.
%---------------------------------------------------------------------------%
