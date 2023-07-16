%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS constructs exported to Java.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_java_export.
:- interface.

:- import_module libs.
:- import_module libs.indent.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_java_util.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % Exports are converted into forwarding methods that are given the
    % specified name. These simply call the exported procedure.
    %
    % NOTE: the forwarding methods must be declared public as they might
    % be referred to within foreign_procs that are inlined across module
    % boundaries.
    %
:- pred output_exports_for_java(java_out_info::in, io.text_output_stream::in,
    indent::in, list(mlds_pragma_export)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_exported_enums_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_exported_enum)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module libs.globals.
:- import_module ml_backend.ml_type_gen.   % for ml_gen_type_name
:- import_module ml_backend.mlds_to_java_data.
:- import_module ml_backend.mlds_to_java_func.
:- import_module ml_backend.mlds_to_java_name.
:- import_module ml_backend.mlds_to_java_type.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

output_exports_for_java(Info, Stream, Indent, Exports, !IO) :-
    list.foldl(output_export_for_java(Info, Stream, Indent), Exports, !IO).

:- pred output_export_for_java(java_out_info::in, io.text_output_stream::in,
    indent::in, mlds_pragma_export::in, io::di, io::uo) is det.

output_export_for_java(Info0, Stream, Indent, Export, !IO) :-
    Export = ml_pragma_export(Lang, ExportName, _, MLDS_Signature,
        UnivQTVars, _),
    expect(unify(Lang, lang_java), $pred,
        "foreign_export for language other than Java."),
    MLDS_Signature = mlds_func_params(Parameters, ReturnTypes),
    Info = (Info0 ^ joi_output_generics := do_output_generics)
                  ^ joi_univ_tvars := UnivQTVars,

    IndentStr = indent2_string(Indent),
    UnivQTVarsStr = generic_tvars_to_string(UnivQTVars),
    PadStr = (if UnivQTVarsStr = "" then "" else " "),
    io.format(Stream, "%spublic static%s%s\n",
        [s(IndentStr), s(PadStr), s(UnivQTVarsStr)], !IO),

    write_indent2(Stream, Indent, !IO),
    (
        ReturnTypes = [],
        io.write_string(Stream, "void", !IO)
    ;
        ReturnTypes = [RetType],
        output_type_for_java(Info, Stream, RetType, !IO)
    ;
        ReturnTypes = [_, _ | _],
        % For multiple outputs, we return an array of objects.
        io.write_string(Stream, "java.lang.Object []", !IO)
    ),
    io.write_string(Stream, " " ++ ExportName, !IO),
    ( if
        some [Param] (
            list.member(Param, Parameters),
            has_ptr_type(Param)
        )
    then
        (
            ( ReturnTypes = []
            ; ReturnTypes = [_]
            ),
            output_export_ref_out(Info, Stream, Indent, Export, !IO)
        ;
            ReturnTypes = [_, _ | _],
            unexpected($pred, "multiple return values")
        )
    else
        output_export_no_ref_out(Info, Stream, Indent, Export, !IO)
    ).

:- pred output_export_no_ref_out(java_out_info::in, io.text_output_stream::in,
    indent::in, mlds_pragma_export::in, io::di, io::uo) is det.

output_export_no_ref_out(Info, Stream, Indent, Export, !IO) :-
    Export = ml_pragma_export(_Lang, _ExportName, QualFuncName, MLDS_Signature,
        _UnivQTVars, _Context),
    Indent1 = Indent + 1,
    IndentStr = indent2_string(Indent),
    Indent1Str = indent2_string(Indent1),

    MLDS_Signature = mlds_func_params(Parameters, ReturnTypes),
    output_params_for_java(Info, Stream, Indent1, Parameters, !IO),
    io.nl(Stream, !IO),

    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    io.format(Stream, "%s", [s(Indent1Str)], !IO),
    (
        ReturnTypes = []
    ;
        ReturnTypes = [RetType],
        % The cast is required when the exported method uses generics but the
        % underlying method does not use generics (i.e. returns Object).
        io.format(Stream, "return (%s) ",
            [s(type_to_string_for_java(Info, RetType))], !IO)
    ;
        ReturnTypes = [_, _ | _],
        io.write_string(Stream, "return ", !IO)
    ),
    write_export_call_for_java(Stream, QualFuncName, Parameters, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- pred output_export_ref_out(java_out_info::in, io.text_output_stream::in,
    indent::in, mlds_pragma_export::in, io::di, io::uo) is det.

output_export_ref_out(Info, Stream, Indent, Export, !IO) :-
    Export = ml_pragma_export(_Lang, _ExportName, QualFuncName, MLDS_Signature,
        _UnivQTVars, _Context),
    Indent1 = Indent + 1,
    IndentStr = indent2_string(Indent),
    Indent1Str = indent2_string(Indent1),

    MLDS_Signature = mlds_func_params(Parameters, ReturnTypes),
    list.filter(has_ptr_type, Parameters, RefParams, NonRefParams),

    output_export_params_ref_out(Info, Stream, Indent, Parameters, !IO),
    io.nl(Stream, !IO),
    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    io.format(Stream, "%sjava.lang.Object[] results = ", [s(Indent1Str)], !IO),
    write_export_call_for_java(Stream, QualFuncName, NonRefParams, !IO),

    ( if ReturnTypes = [] then
        FirstRefArg = 0
    else if ReturnTypes = [mlds_native_bool_type] then
        % Semidet procedure.
        FirstRefArg = 1
    else
        unexpected($pred, "unexpected ReturnTypes")
    ),
    list.foldl2(assign_ref_output(Info, Stream, Indent1), RefParams,
        FirstRefArg, _, !IO),
    (
        FirstRefArg = 0
    ;
        FirstRefArg = 1,
        io.format(Stream, 
            "%sreturn ((java.lang.Boolean) results[0]).booleanValue();\n",
            [s(Indent1Str)], !IO)
    ),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- pred output_export_params_ref_out(java_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_argument)::in,
    io::di, io::uo) is det.

output_export_params_ref_out(Info, Stream, Indent, Parameters, !IO) :-
    io.write_string(Stream, "(", !IO),
    (
        Parameters = []
    ;
        Parameters = [_ | _],
        io.nl(Stream, !IO),
        write_out_list(output_export_param_ref_out(Info, Indent + 1),
            ",\n", Parameters, Stream, !IO)
    ),
    io.write_string(Stream, ")", !IO).

:- pred output_export_param_ref_out(java_out_info::in,
    indent::in, mlds_argument::in, io.text_output_stream::in,
    io::di, io::uo) is det.

output_export_param_ref_out(Info, Indent, Argument, Stream, !IO) :-
    Argument = mlds_argument(VarName, Type, _),
    IndentStr = indent2_string(Indent),
    VarNameStr = local_var_name_to_string_for_java(VarName),
    ( if Type = mlds_ptr_type(InnerType) then
        boxed_type_to_string_for_java(Info, InnerType, InnerTypeStr),
        io.format(Stream, "%sjmercury.runtime.Ref<%s> %s",
            [s(IndentStr), s(InnerTypeStr), s(VarNameStr)], !IO)
    else
        TypeStr = type_to_string_for_java(Info, Type),
        io.format(Stream, "%s%s %s",
            [s(IndentStr), s(TypeStr), s(VarNameStr)], !IO)
    ).

:- pred write_export_call_for_java(io.text_output_stream::in,
    qual_function_name::in, list(mlds_argument)::in, io::di, io::uo) is det.

write_export_call_for_java(Stream, QualFuncName, Parameters, !IO) :-
    QualFuncName = qual_function_name(ModuleName, FuncName),
    Qualifier = qualifier_to_string_for_java(ModuleName, module_qual),
    FuncNameStr = function_name_to_string_for_java(FuncName),
    ParamVars =
        list.map((func(mlds_argument(VarName, _, _)) = VarName), Parameters),
    ParamVarStrs = list.map(local_var_name_to_string_for_java, ParamVars),
    ParamVarsStr = string.join_list(", ", ParamVarStrs),
    io.format(Stream, "%s.%s(%s);\n",
        [s(Qualifier), s(FuncNameStr), s(ParamVarsStr)], !IO).

:- pred assign_ref_output(java_out_info::in, io.text_output_stream::in,
    indent::in, mlds_argument::in, int::in, int::out, io::di, io::uo) is det.

assign_ref_output(Info, Stream, Indent, Arg, N, N + 1, !IO) :-
    IndentStr = indent2_string(Indent),
    Arg = mlds_argument(VarName, Type, _),
    VarNameStr = local_var_name_to_string_for_java(VarName),
    ( if Type = mlds_ptr_type(InnerType) then
        boxed_type_to_string_for_java(Info, InnerType, TypeName)
    else
        boxed_type_to_string_for_java(Info, Type, TypeName)
    ),
    io.format(Stream, "%s%s.val = (%s) results[%d];\n",
        [s(IndentStr), s(VarNameStr), s(TypeName), i(N)], !IO).

:- pred has_ptr_type(mlds_argument::in) is semidet.

has_ptr_type(mlds_argument(_, mlds_ptr_type(_), _)).

%---------------------------------------------------------------------------%

output_exported_enums_for_java(Info, Stream, Indent, ExportedEnums, !IO) :-
    list.foldl(output_exported_enum_for_java(Info, Stream, Indent),
        ExportedEnums, !IO).

:- pred output_exported_enum_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, mlds_exported_enum::in,
    io::di, io::uo) is det.

output_exported_enum_for_java(Info, Stream, Indent, ExportedEnum, !IO) :-
    ExportedEnum = mlds_exported_enum(Lang, _, TypeCtor, ExportedConstants),
    (
        Lang = lang_java,
        ml_gen_type_name(TypeCtor, EnumModule, EnumName, EnumArity),
        EnumId = mlds_enum_class_id(EnumModule, EnumName, EnumArity),
        MLDS_Type = mlds_enum_class_type(EnumId),
        list.foldl(
            output_exported_enum_constant_for_java(Info, Stream, Indent,
                MLDS_Type),
            ExportedConstants, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_csharp
        )
    ).

:- pred output_exported_enum_constant_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, mlds_type::in,
    mlds_exported_enum_constant::in, io::di, io::uo) is det.

output_exported_enum_constant_for_java(Info, Stream, Indent, MLDS_Type,
        ExportedConstant, !IO) :-
    IndentStr = indent2_string(Indent),
    ExportedConstant = mlds_exported_enum_constant(Name, Initializer),
    io.format(Stream, "%spublic static final %s %s = ",
        [s(IndentStr), s(type_to_string_for_java(Info, MLDS_Type)), s(Name)],
        !IO),
    output_initializer_body_for_java(Info, Stream, not_at_start_of_line,
        Indent + 1, Initializer, no, ";", !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_java_export.
%---------------------------------------------------------------------------%
