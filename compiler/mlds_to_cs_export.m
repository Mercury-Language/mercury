%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS constructs exported to C#.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_cs_export.
:- interface.

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_cs_util.
:- import_module ml_backend.mlds_to_target_util.

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
:- pred output_exports_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in,
    list(mlds_pragma_export)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_exported_enums_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in,
    list(mlds_exported_enum)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.indent.
:- import_module ml_backend.ml_type_gen.    % for ml_gen_type_name
:- import_module ml_backend.mlds_to_cs_data.
:- import_module ml_backend.mlds_to_cs_func.
:- import_module ml_backend.mlds_to_cs_name.
:- import_module ml_backend.mlds_to_cs_type.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

output_exports_for_csharp(Info, Stream, Indent, Exports, !IO) :-
    list.foldl(output_export_for_csharp(Info, Stream, Indent), Exports, !IO).

:- pred output_export_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in,
    mlds_pragma_export::in, io::di, io::uo) is det.

output_export_for_csharp(Info, Stream, Indent, Export, !IO) :-
    Export = ml_pragma_export(Lang, ExportName, QualFuncName, MLDS_Signature,
        _UnivQTVars, _),
    expect(unify(Lang, lang_csharp), $pred,
        "foreign_export for language other than C#."),
    list.filter(is_out_argument, Parameters, OutArgs, InArgs),
    MLDS_Signature = mlds_func_params(Parameters, ReturnTypes),
    ParamsStr = params_to_string_for_csharp(Info, Indent + 1, Parameters),

    IndentStr = indent2_string(Indent),

    io.format(Stream, "%spublic static\n", [s(IndentStr)], !IO),
    % XXX C# has generics.
    % output_generic_tvars(UnivQTVars, !IO),
    (
        ReturnTypes = [],
        (
            OutArgs = [],
            ExportCallStr = export_call_to_string_for_csharp(QualFuncName,
                InArgs),
            io.format(Stream, "%svoid %s%s\n",
                [s(IndentStr), s(ExportName), s(ParamsStr)], !IO),
            io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
            io.format(Stream, "  %s%s;\n",
                [s(IndentStr), s(ExportCallStr)], !IO),
            io.format(Stream, "%s}\n", [s(IndentStr)], !IO)
        ;
            OutArgs = [FirstOutArg | RestOutArgs],
            FirstOutArg = mlds_argument(FirstOutArgName, _, _),
            FirstOutArgNameStr =
                local_var_name_to_ll_string_for_csharp(FirstOutArgName),
            ExportCallStr = export_call_to_string_for_csharp(QualFuncName,
                InArgs ++ RestOutArgs),
            io.format(Stream, "%svoid %s%s\n",
                [s(IndentStr), s(ExportName), s(ParamsStr)], !IO),
            io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
            io.format(Stream, "  %s%s = %s;\n",
                [s(IndentStr), s(FirstOutArgNameStr), s(ExportCallStr)], !IO),
            io.format(Stream, "%s}\n", [s(IndentStr)], !IO)
        )
    ;
        ReturnTypes = [RetType],
        RetTypeStr = type_to_string_for_csharp(Info, RetType),
        ExportCallStr = export_call_to_string_for_csharp(QualFuncName,
            InArgs ++ OutArgs),
        io.format(Stream, "%s%s %s%s\n",
            [s(IndentStr), s(RetTypeStr), s(ExportName), s(ParamsStr)], !IO),
        io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
        io.format(Stream, "%s  return (%s) %s;\n",
            [s(IndentStr), s(RetTypeStr), s(ExportCallStr)], !IO),
        io.format(Stream, "%s}\n", [s(IndentStr)], !IO)
    ;
        ReturnTypes = [_, _ | _],
        unexpected($pred, "multiple return values in export method")
    ).

:- pred is_out_argument(mlds_argument::in) is semidet.

is_out_argument(mlds_argument(_, Type, _)) :-
    Type = mlds_ptr_type(_).

:- func export_call_to_string_for_csharp(qual_function_name,
    list(mlds_argument)) = string.

export_call_to_string_for_csharp(QualFuncName, Args) = CallStr :-
    QualFuncName = qual_function_name(ModuleName, FuncName),
    Qualifier = qualifier_to_nll_string_for_csharp(ModuleName, module_qual),
    FuncNameStr = function_name_to_ll_string_for_csharp(FuncName),
    ArgStrs = list.map(maybe_out_argument_name_for_csharp, Args),
    ArgsStr = string.join_list(", ", ArgStrs),
    string.format("%s.%s(%s)", [s(Qualifier), s(FuncNameStr), s(ArgsStr)],
        CallStr).

:- func maybe_out_argument_name_for_csharp(mlds_argument) = string.

maybe_out_argument_name_for_csharp(Arg) = ArgStr :-
    Arg = mlds_argument(Name, Type, _),
    NameStr = local_var_name_to_ll_string_for_csharp(Name),
    ( if Type = mlds_ptr_type(_) then
        string.format("out %s", [s(NameStr)], ArgStr)
    else
        ArgStr = NameStr
    ).

%---------------------------------------------------------------------------%

output_exported_enums_for_csharp(Info, Stream, Indent, ExportedEnums, !IO) :-
    list.foldl(output_exported_enum_for_csharp(Info, Stream, Indent),
        ExportedEnums, !IO).

:- pred output_exported_enum_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_exported_enum::in,
    io::di, io::uo) is det.

output_exported_enum_for_csharp(Info, Stream, Indent, ExportedEnum, !IO) :-
    ExportedEnum = mlds_exported_enum(Lang, _, TypeCtor, ExportedConstants),
    (
        Lang = lang_csharp,
        ml_gen_type_name(TypeCtor, ClassName, ClassArity),
        ClassId = mlds_class_id(ClassName, ClassArity, mlds_enum),
        MLDS_Type = mlds_class_type(ClassId),
        list.foldl(
            output_exported_enum_constant_for_csharp(Info, Stream, Indent,
                MLDS_Type),
            ExportedConstants, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_java
        )
    ).

:- pred output_exported_enum_constant_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, int::in, mlds_type::in,
    mlds_exported_enum_constant::in, io::di, io::uo) is det.

output_exported_enum_constant_for_csharp(Info, Stream, Indent, MLDS_Type,
        ExportedConstant, !IO) :-
    IndentStr = indent2_string(Indent),
    ExportedConstant = mlds_exported_enum_constant(Name, Initializer),
    TypeStr = type_to_string_for_csharp(Info, MLDS_Type),
    io.format(Stream, "%spublic static readonly %s %s = ",
        [s(IndentStr), s(TypeStr), s(Name)], !IO),
    output_initializer_body_for_csharp(Info, Stream, not_at_start_of_line,
        Indent + 1, Initializer, no, ";", !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_export.
%---------------------------------------------------------------------------%
