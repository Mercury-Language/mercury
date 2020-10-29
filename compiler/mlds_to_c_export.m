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

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c_util.
:- import_module ml_backend.mlds_to_target_util.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred mlds_output_pragma_export_defn(mlds_to_c_opts::in,
    mlds_module_name::in, indent::in, mlds_pragma_export::in,
    io::di, io::uo) is det.

:- pred mlds_output_export_enums(mlds_to_c_opts::in, indent::in,
    list(mlds_exported_enum)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module backend_libs.rtti.
:- import_module libs.
:- import_module libs.globals.
:- import_module ml_backend.mlds_to_c_func.
:- import_module ml_backend.mlds_to_c_name.
:- import_module ml_backend.mlds_to_c_type.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module char.
:- import_module maybe.
:- import_module require.
:- import_module term.

mlds_output_pragma_export_defn(Opts, ModuleName, Indent, PragmaExport, !IO) :-
    PragmaExport = ml_pragma_export(Lang, _ExportName, MLDS_Name,
        MLDS_Signature, _UnivQTVars, Context),
    expect(unify(Lang, lang_c), $pred,
        "foreign_export to language other than C."),
    mlds_output_pragma_export_func_name(Opts, ModuleName, Indent,
        PragmaExport, !IO),
    io.write_string("\n", !IO),
    c_output_context(Opts ^ m2co_foreign_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("{\n", !IO),
    c_output_context(Opts ^ m2co_foreign_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    mlds_output_pragma_export_defn_body(Opts, MLDS_Name, MLDS_Signature, !IO),
    io.write_string("}\n", !IO).

:- pred mlds_output_pragma_export_func_name(mlds_to_c_opts::in,
    mlds_module_name::in, indent::in, mlds_pragma_export::in,
    io::di, io::uo) is det.

mlds_output_pragma_export_func_name(Opts, ModuleName, Indent, Export, !IO) :-
    Export = ml_pragma_export(Lang, ExportName, _MLDSName, Signature,
        _UnivQTVars, Context),
    expect(unify(Lang, lang_c), $pred, "export to language other than C."),
    FuncName = mlds_function_export(ExportName),
    QualFuncName = qual_function_name(ModuleName, FuncName),
    c_output_context(Opts ^ m2co_foreign_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    % For functions exported using `pragma foreign_export',
    % we use the default C calling convention.
    CallingConvention = "",
    mlds_output_func_decl_ho(Opts, Indent, QualFuncName, Context,
        CallingConvention, Signature,
        mlds_output_pragma_export_type_ignore_opts(prefix),
        mlds_output_pragma_export_type_ignore_opts(suffix), !IO).

:- pred mlds_output_pragma_export_type_prefix_suffix(mlds_type::in,
    io::di, io::uo) is det.

mlds_output_pragma_export_type_prefix_suffix(Type, !IO) :-
    mlds_output_pragma_export_type(prefix, Type, !IO),
    mlds_output_pragma_export_type(suffix, Type, !IO).

:- type locn
    --->    prefix
    ;       suffix.

:- pred mlds_output_pragma_export_type_ignore_opts(locn::in,
    mlds_to_c_opts::in, mlds_type::in, io::di, io::uo) is det.

mlds_output_pragma_export_type_ignore_opts(PrefixSuffix, _Opts, MLDS_Type,
        !IO) :-
    mlds_output_pragma_export_type(PrefixSuffix, MLDS_Type, !IO).

:- pred mlds_output_pragma_export_type(locn::in,
    mlds_type::in, io::di, io::uo) is det.

mlds_output_pragma_export_type(PrefixSuffix, MLDS_Type, !IO) :-
    (
        PrefixSuffix = suffix
    ;
        PrefixSuffix = prefix,
        (
            MLDS_Type = mlds_mercury_array_type(_ElemType),
            io.write_string("MR_ArrayPtr", !IO)
        ;
            MLDS_Type = mercury_nb_type(MerType, _),
            % XXX We should not need to call this function.
            TypeStr = maybe_foreign_type_to_c_string(MerType, no),
            io.write_string(TypeStr, !IO)
        ;
            ( MLDS_Type = mlds_cont_type(_)
            ; MLDS_Type = mlds_commit_type
            ; MLDS_Type = mlds_class_type(_)
            ; MLDS_Type = mlds_array_type(_)
            ; MLDS_Type = mlds_mostly_generic_array_type(_)
            ; MLDS_Type = mlds_func_type(_)
            ; MLDS_Type = mlds_generic_type
            ; MLDS_Type = mlds_generic_env_ptr_type
            ; MLDS_Type = mlds_type_info_type
            ; MLDS_Type = mlds_pseudo_type_info_type
            ; MLDS_Type = mlds_rtti_type(_)
            ),
            io.write_string("MR_Word", !IO)
        ;
            MLDS_Type = mlds_native_bool_type,
            io.write_string("MR_bool", !IO)
        ;
            MLDS_Type = mlds_builtin_type_int(IntType),
            TypeStr =
                exported_builtin_type_to_c_string(builtin_type_int(IntType)),
            io.write_string(TypeStr, !IO)
        ;
            MLDS_Type = mlds_builtin_type_float,
            TypeStr = exported_builtin_type_to_c_string(builtin_type_float),
            io.write_string(TypeStr, !IO)
        ;
            MLDS_Type = mlds_builtin_type_string,
            TypeStr = exported_builtin_type_to_c_string(builtin_type_string),
            io.write_string(TypeStr, !IO)
        ;
            MLDS_Type = mlds_builtin_type_char,
            TypeStr = exported_builtin_type_to_c_string(builtin_type_char),
            io.write_string(TypeStr, !IO)
        ;
            MLDS_Type = mlds_foreign_type(ForeignType),
            (
                ForeignType = c(c_type(Name)),
                io.write_string(Name, !IO)
            ;
                ForeignType = java(_),
                unexpected($pred, "java foreign_type")
            ;
                ForeignType = csharp(_),
                unexpected($pred, "csharp foreign_type")
            )
        ;
            MLDS_Type = mlds_ptr_type(Type),
            mlds_output_pragma_export_type(prefix, Type, !IO),
            io.write_string(" *", !IO)
        ;
            MLDS_Type = mlds_tabling_type(_),
            % These types should never occur in procedures exported to C,
            % so the fact that we could generate a more accurate type
            % should not matter.
            io.write_string("MR_Word", !IO)
        ;
            MLDS_Type = mlds_unknown_type,
            unexpected($pred, "unknown_type")
        )
    ).

    % Output the definition body for a pragma foreign_export.
    %
:- pred mlds_output_pragma_export_defn_body(mlds_to_c_opts::in,
    qual_function_name::in, mlds_func_params::in, io::di, io::uo) is det.

mlds_output_pragma_export_defn_body(Opts, FuncName, Signature, !IO) :-
    Signature = mlds_func_params(Parameters, RetTypes),

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
    io.write_list(CForeignTypeInputs, "",
        mlds_output_pragma_export_input_defns(Opts), !IO),
    io.write_list(CForeignTypeOutputs, "",
        mlds_output_pragma_export_output_defns(Opts), !IO),

    % Declare a local variable or two for the return value, if needed.
    (
        RetTypes = [RetType1],
        ( if RetType1 = mlds_foreign_type(c(_)) then
            io.write_string("\t", !IO),
            mlds_output_pragma_export_type_prefix_suffix(RetType1, !IO),
            io.write_string(" ret_value;\n", !IO),
            io.write_string("\t", !IO),
            mlds_output_type(Opts, RetType1, !IO),
            io.write_string(" boxed_ret_value;\n", !IO)
        else
            io.write_string("\t", !IO),
            mlds_output_pragma_export_type_prefix_suffix(RetType1, !IO),
            io.write_string(" ret_value;\n", !IO)
        )
    ;
        RetTypes = []
    ;
        RetTypes = [_, _ | _]
    ),

    % Generate code to box any non-word-sized foreign_type input parameters;
    % these need to be converted to a uniform size before passing them
    % to Mercury code.
    io.write_list(CForeignTypeInputs, "", mlds_output_pragma_input_arg, !IO),

    % Generate code to actually call the Mercury procedure which
    % is being exported
    (
        RetTypes = [],
        io.write_string("\t", !IO),
        mlds_output_pragma_export_call(Opts, FuncName, Parameters, !IO)
    ;
        RetTypes = [RetType2],
        ( if RetType2 = mlds_foreign_type(c(_)) then
            io.write_string("\tboxed_ret_value = ", !IO)
        else
            io.write_string("\tret_value = (", !IO),
            mlds_output_pragma_export_type_prefix_suffix(RetType2, !IO),
            io.write_string(")", !IO)
        ),
        mlds_output_pragma_export_call(Opts, FuncName, Parameters, !IO)
    ;
        RetTypes = [_, _ | _],
        % This is just for MLDS dumps when compiling to non-C targets.
        % So we do not need to worry about boxing/unboxing foreign types here.
        io.write_string("\treturn (", !IO),
        mlds_output_return_list(RetTypes,
            mlds_output_pragma_export_type_prefix_suffix, !IO),
        io.write_string(") ", !IO)
    ),

    % Generate code to unbox any foreign_type output parameters,
    % since we are returning those parameters to C code.
    io.write_list(CForeignTypeOutputs, "", mlds_output_pragma_output_arg, !IO),

    % Generate the final statement to unbox and return the return value,
    % if needed.
    (
        RetTypes = []
    ;
        RetTypes = [RetType3],
        ( if RetType3 = mlds_foreign_type(c(_)) then
            io.write_string("\tMR_MAYBE_UNBOX_FOREIGN_TYPE(", !IO),
            mlds_output_pragma_export_type_prefix_suffix(RetType3, !IO),
            io.write_string(", boxed_ret_value, ret_value);\n", !IO)
        else
            true
        ),
        io.write_string("\treturn ret_value;\n", !IO)
    ;
        RetTypes = [_, _ | _]
    ).

:- pred mlds_output_pragma_input_arg(mlds_argument::in,
    io::di, io::uo) is det.

mlds_output_pragma_input_arg(Arg, !IO) :-
    Arg = mlds_argument(LocalVarName, Type, _GCStmt),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    io.write_string("\tMR_MAYBE_BOX_FOREIGN_TYPE(", !IO),
    mlds_output_pragma_export_type_prefix_suffix(Type, !IO),
    io.write_string(", ", !IO),
    mlds_output_local_var_name(LocalVarName, !IO),
    io.write_string(", ", !IO),
    mlds_output_local_var_name(BoxedLocalVarName, !IO),
    io.write_string(");\n", !IO).

:- pred mlds_output_pragma_output_arg(mlds_argument::in,
    io::di, io::uo) is det.

mlds_output_pragma_output_arg(Arg, !IO) :-
    Arg = mlds_argument(LocalVarName, Type, _GCStmt),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    io.write_string("\tMR_MAYBE_UNBOX_FOREIGN_TYPE(", !IO),
    mlds_output_pragma_export_type_prefix_suffix(pointed_to_type(Type), !IO),
    io.write_string(", ", !IO),
    mlds_output_local_var_name(BoxedLocalVarName, !IO),
    io.write_string(", *", !IO),
    mlds_output_local_var_name(LocalVarName, !IO),
    io.write_string(");\n", !IO).

:- pred mlds_output_pragma_export_input_defns(mlds_to_c_opts::in,
    mlds_argument::in, io::di, io::uo) is det.

mlds_output_pragma_export_input_defns(Opts, Arg, !IO) :-
    Arg = mlds_argument(LocalVarName, Type, _GCStmt),
    io.write_string("\t", !IO),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    mlds_output_type_prefix(Opts, Type, !IO),
    io.write_char(' ', !IO),
    mlds_output_local_var_name(BoxedLocalVarName, !IO),
    mlds_output_type_suffix_no_size(Opts, Type, !IO),
    io.write_string(";\n", !IO).

:- pred mlds_output_pragma_export_output_defns(mlds_to_c_opts::in,
    mlds_argument::in, io::di, io::uo) is det.

mlds_output_pragma_export_output_defns(Opts, Arg, !IO) :-
    Arg = mlds_argument(LocalVarName, Type, _GCStmt),
    io.write_string("\t", !IO),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    PointedToType = pointed_to_type(Type),
    mlds_output_type_prefix(Opts, PointedToType, !IO),
    io.write_char(' ', !IO),
    mlds_output_local_var_name(BoxedLocalVarName, !IO),
    mlds_output_type_suffix_no_size(Opts, PointedToType, !IO),
    io.write_string(";\n", !IO).

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

:- pred mlds_output_pragma_export_call(mlds_to_c_opts::in,
    qual_function_name::in, list(mlds_argument)::in, io::di, io::uo) is det.

mlds_output_pragma_export_call(Opts, FuncName, Parameters, !IO) :-
    mlds_output_fully_qualified_function_name(FuncName, !IO),
    io.write_string("(", !IO),
    io.write_list(Parameters, ", ", mlds_output_pragma_export_arg(Opts), !IO),
    io.write_string(");\n", !IO).

    % Output a fully qualified name preceded by a cast.
    %
:- pred mlds_output_pragma_export_arg(mlds_to_c_opts::in,
    mlds_argument::in, io::di, io::uo) is det.

mlds_output_pragma_export_arg(Opts, Arg, !IO) :-
    Arg = mlds_argument(LocalVarName, Type, _GCStmt),
    get_boxed_local_var_name(LocalVarName, BoxedLocalVarName),
    ( if Type = mlds_foreign_type(c(_)) then
        % This is a foreign_type input. Pass in the already-boxed value.
        mlds_output_local_var_name(BoxedLocalVarName, !IO)
    else if Type = mlds_ptr_type(mlds_foreign_type(c(_))) then
        % This is a foreign_type output. Pass in the address of the
        % local variable which will hold the boxed value.
        io.write_string("&", !IO),
        mlds_output_local_var_name(BoxedLocalVarName, !IO)
    else
        % Otherwise, no boxing or unboxing is needed.
        % Just cast the argument to the right type.
        mlds_output_cast(Opts, Type, !IO),
        mlds_output_local_var_name(LocalVarName, !IO)
    ).

%---------------------------------------------------------------------------%

mlds_output_export_enums(Opts, Indent, ExportedEnums, !IO) :-
    list.foldl(mlds_output_export_enum(Opts, Indent), ExportedEnums, !IO).

:- pred mlds_output_export_enum(mlds_to_c_opts::in, indent::in,
    mlds_exported_enum::in, io::di, io::uo) is det.

mlds_output_export_enum(Opts, _Indent, ExportedEnum, !IO) :-
    ExportedEnum = mlds_exported_enum(Lang, Context, _TypeCtor,
        ExportConstants),
    (
        Lang = lang_c,
        c_output_context(Opts ^ m2co_foreign_line_numbers, Context, !IO),
        list.foldl(mlds_output_exported_enum_constant, ExportConstants, !IO)
    ;
        ( Lang = lang_csharp
        ; Lang = lang_java
        )
    ).

:- pred mlds_output_exported_enum_constant(mlds_exported_enum_constant::in,
    io::di, io::uo) is det.

mlds_output_exported_enum_constant(ExportedConstant, !IO) :-
    ExportedConstant = mlds_exported_enum_constant(Name, Initializer),
    io.write_string("#define ", !IO),
    io.write_string(Name, !IO),
    io.write_string(" ", !IO),
    ( if
        Initializer = init_obj(ml_const(mlconst_enum(Value, _)))
    then
        io.write_int(Value, !IO)
    else if
        Initializer = init_obj(ml_const(mlconst_foreign(Lang, Value, _)))
    then
        expect(unify(Lang, lang_c), $pred,
            "mlconst_foreign for language other than C."),
        io.write_string(Value, !IO)
    else
        unexpected($pred, "tag for export enumeration is not enum or foreign")
    ),
    io.nl(!IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_export.
%---------------------------------------------------------------------------%
