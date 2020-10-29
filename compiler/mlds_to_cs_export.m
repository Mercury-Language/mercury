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
:- pred output_exports_for_csharp(csharp_out_info::in, indent::in,
    list(mlds_pragma_export)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_exported_enums_for_csharp(csharp_out_info::in, indent::in,
    list(mlds_exported_enum)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.globals.
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
:- import_module term.

%---------------------------------------------------------------------------%

output_exports_for_csharp(Info, Indent, Exports, !IO) :-
    list.foldl(output_export_for_csharp(Info, Indent), Exports, !IO).

:- pred output_export_for_csharp(csharp_out_info::in, indent::in,
    mlds_pragma_export::in, io::di, io::uo) is det.

output_export_for_csharp(Info, Indent, Export, !IO) :-
    Export = ml_pragma_export(Lang, ExportName, MLDS_Name, MLDS_Signature,
        _UnivQTVars, _),
    MLDS_Signature = mlds_func_params(Parameters, ReturnTypes),
    expect(unify(Lang, lang_csharp), $pred,
        "foreign_export for language other than C#."),

    output_n_indents(Indent, !IO),
    io.write_string("public static ", !IO),
    % XXX C# has generics.
    % output_generic_tvars(UnivQTVars, !IO),
    io.nl(!IO),
    output_n_indents(Indent, !IO),

    (
        ReturnTypes = [],
        io.write_string("void ", !IO)
    ;
        ReturnTypes = [RetType],
        output_type_for_csharp(Info, RetType, !IO),
        io.write_string(" ", !IO)
    ;
        ReturnTypes = [_, _ | _],
        unexpected($pred, "multiple return values in export method")
    ),
    io.write_string(ExportName, !IO),
    output_params_for_csharp(Info, Indent + 1, Parameters, !IO),
    io.nl(!IO),
    output_n_indents(Indent, !IO),
    io.write_string("{\n", !IO),
    output_n_indents(Indent + 1, !IO),
    list.filter(is_out_argument, Parameters, OutArgs, InArgs),
    (
        ReturnTypes = [],
        (
            OutArgs = [],
            RestOutArgs = []
        ;
            OutArgs = [FirstOutArg | RestOutArgs],
            FirstOutArg = mlds_argument(FirstOutArgName, _, _),
            output_local_var_name_for_csharp(FirstOutArgName, !IO),
            io.write_string(" = ", !IO)
        )
    ;
        ReturnTypes = [RetTypeB | _],
        % The cast is required when the exported method uses generics, but
        % the underlying method does not use generics (i.e. returns Object).
        io.write_string("return (", !IO),
        output_type_for_csharp(Info, RetTypeB, !IO),
        io.write_string(") ", !IO),
        RestOutArgs = OutArgs
    ),
    write_export_call_for_csharp(MLDS_Name, InArgs ++ RestOutArgs, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred is_out_argument(mlds_argument::in) is semidet.

is_out_argument(mlds_argument(_, Type, _)) :-
    Type = mlds_ptr_type(_).

:- pred write_export_call_for_csharp(qual_function_name::in,
    list(mlds_argument)::in, io::di, io::uo) is det.

write_export_call_for_csharp(QualFuncName, Parameters, !IO) :-
    QualFuncName = qual_function_name(ModuleName, FuncName),
    output_qual_name_prefix_cs(ModuleName, module_qual, !IO),
    output_function_name_for_csharp(FuncName, !IO),
    io.write_char('(', !IO),
    io.write_list(Parameters, ", ", write_argument_name_for_csharp, !IO),
    io.write_string(");\n", !IO).

:- pred write_argument_name_for_csharp(mlds_argument::in,
    io::di, io::uo) is det.

write_argument_name_for_csharp(Arg, !IO) :-
    Arg = mlds_argument(Name, Type, _),
    ( if Type = mlds_ptr_type(_) then
        io.write_string("out ", !IO)
    else
        true
    ),
    output_local_var_name_for_csharp(Name, !IO).

%---------------------------------------------------------------------------%

output_exported_enums_for_csharp(Info, Indent, ExportedEnums, !IO) :-
    list.foldl(output_exported_enum_for_csharp(Info, Indent),
        ExportedEnums, !IO).

:- pred output_exported_enum_for_csharp(csharp_out_info::in, indent::in,
    mlds_exported_enum::in, io::di, io::uo) is det.

output_exported_enum_for_csharp(Info, Indent, ExportedEnum, !IO) :-
    ExportedEnum = mlds_exported_enum(Lang, _, TypeCtor, ExportedConstants),
    (
        Lang = lang_csharp,
        ml_gen_type_name(TypeCtor, ClassName, ClassArity),
        ClassId = mlds_class_id(ClassName, ClassArity, mlds_enum),
        MLDS_Type = mlds_class_type(ClassId),
        list.foldl(
            output_exported_enum_constant_for_csharp(Info, Indent, MLDS_Type),
            ExportedConstants, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_java
        )
    ).

:- pred output_exported_enum_constant_for_csharp(csharp_out_info::in,
    indent::in, mlds_type::in, mlds_exported_enum_constant::in,
    io::di, io::uo) is det.

output_exported_enum_constant_for_csharp(Info, Indent, MLDS_Type,
        ExportedConstant, !IO) :-
    ExportedConstant = mlds_exported_enum_constant(Name, Initializer),
    output_n_indents(Indent, !IO),
    io.write_string("public static readonly ", !IO),
    output_type_for_csharp(Info, MLDS_Type, !IO),
    io.write_string(" ", !IO),
    io.write_string(Name, !IO),
    io.write_string(" = ", !IO),
    output_initializer_body_for_csharp(Info, not_at_start_of_line,
        Indent + 1, Initializer, no, ";", !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_export.
%---------------------------------------------------------------------------%
