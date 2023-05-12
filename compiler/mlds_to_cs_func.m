%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS function declarations and definitions in C#.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_cs_func.
:- interface.

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_cs_util.
:- import_module ml_backend.mlds_to_target_util.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred output_function_defn_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, output_aux::in,
    mlds_function_defn::in, io::di, io::uo) is det.

:- func params_to_string_for_csharp(csharp_out_info, indent,
    list(mlds_argument)) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.            % for pred_proc_id
:- import_module libs.
:- import_module libs.indent.
:- import_module ml_backend.mlds_to_cs_name.
:- import_module ml_backend.mlds_to_cs_stmt.
:- import_module ml_backend.mlds_to_cs_type.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

output_function_defn_for_csharp(Info, Stream, Indent, OutputAux,
        FunctionDefn, !IO) :-
    % Put a blank line before each function definition.
    io.nl(Stream, !IO),
    IndentStr = indent2_string(Indent),
    FunctionDefn = mlds_function_defn(FuncName, Context, Flags,
        MaybePredProcId, Params, MaybeBody, _EnvVarNames,
        _MaybeRequireTailrecInfo),
    (
        MaybeBody = body_external,
        % This is just a function declaration, with no body.
        % C# doesn't support separate declarations and definitions,
        % so just output the declaration as a comment.
        % (Note that the actual definition of an external procedure
        % must be given in `pragma foreign_code' in the same module.)
        PreStr =  IndentStr ++ "/* external:\n",
        PostStr = IndentStr ++ "*/\n"
    ;
        MaybeBody = body_defined_here(_),
        PreStr = "",
        PostStr = ""
    ),
    io.write_string(Stream, PreStr, !IO),
    (
        MaybePredProcId = no
    ;
        MaybePredProcId = yes(PredProcId),
        maybe_output_pred_proc_id_comment(Stream, Info ^ csoi_auto_comments,
            IndentStr, PredProcId, !IO)
    ),
    Flags = mlds_function_decl_flags(Access, PerInstance),
    ( Access = func_public,         AccessPrefix = "public "
    ; Access = func_private,        AccessPrefix = "private "
    ),
    ( PerInstance = per_instance,   PerInstancePrefix = ""
    ; PerInstance = one_copy,       PerInstancePrefix = "static "
    ),
    io.format(Stream, "%s%s%s",
        [s(IndentStr), s(AccessPrefix), s(PerInstancePrefix)], !IO),
    output_func_for_csharp(Info, Stream, Indent, FuncName, OutputAux, Context,
        Params, MaybeBody, !IO),
    io.write_string(Stream, PostStr, !IO).

%---------------------------------------------------------------------------%

:- pred output_func_for_csharp(csharp_out_info::in, io.text_output_stream::in,
    indent::in, mlds_function_name::in, output_aux::in, prog_context::in,
    mlds_func_params::in, mlds_function_body::in, io::di, io::uo) is det.

output_func_for_csharp(Info, Stream, Indent, FuncName, OutputAux, Context,
        Signature, MaybeBody, !IO) :-
    (
        MaybeBody = body_defined_here(Body),
        PrintLineNumbers = Info ^ csoi_line_numbers,
        IndentStr = indent2_string(Indent),
        output_func_decl_for_csharp(Info, Stream, Indent, FuncName, OutputAux,
            Signature, !IO),
        cs_output_context(Stream, PrintLineNumbers, Context, !IO),
        io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
        FuncInfo = func_info_csj(Signature),
        output_stmt_for_csharp(Info, Stream, Indent + 1, FuncInfo, Body,
            _ExitMethods, !IO),
        % XXX What is this printed context for? Its scope is limited
        % to just one close brace.
        cs_output_context(Stream, PrintLineNumbers, Context, !IO),
        io.format(Stream, "%s}\n", [s(IndentStr)], !IO),    % end the function
        cs_output_default_context(Stream, PrintLineNumbers, !IO)
    ;
        MaybeBody = body_external
    ).

:- pred output_func_decl_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_function_name::in,
    output_aux::in, mlds_func_params::in, io::di, io::uo) is det.

output_func_decl_for_csharp(Info, Stream, Indent, FuncName, OutputAux,
        Signature, !IO) :-
    Signature = mlds_func_params(Parameters, RetTypes),
    ( if
        OutputAux = oa_cname(ClassName, ClassArity),
        FuncName = mlds_function_export("<constructor>")
    then
        ClassNameStr =
            unqual_class_name_to_ll_string_for_csharp(ClassName, ClassArity),
        ParamsStr = params_to_string_for_csharp(Info, Indent, Parameters),
        io.format(Stream, "%s%s\n", [s(ClassNameStr), s(ParamsStr)], !IO)
    else
        FuncNameStr = function_name_to_ll_string_for_csharp(FuncName),
        get_return_type_and_out_params_for_csharp(Info, RetTypes,
            RetTypeStr, OutParamTypes),
        list.map_foldl(make_out_param, OutParamTypes, OutParams, 2, _),
        ParamsStr = params_to_string_for_csharp(Info, Indent,
            Parameters ++ OutParams),
        io.format(Stream, "%s %s%s\n",
            [s(RetTypeStr), s(FuncNameStr), s(ParamsStr)], !IO)
    ).

:- pred make_out_param(mlds_type::in, mlds_argument::out,
    int::in, int::out) is det.

make_out_param(Type, Argument, Num, Num + 1) :-
    VarName = lvn_comp_var(lvnc_out_param(Num)),
    Argument = mlds_argument(VarName, mlds_ptr_type(Type), gc_no_stmt).

:- pred get_return_type_and_out_params_for_csharp(csharp_out_info::in,
    mlds_return_types::in, string::out, list(mlds_type)::out) is det.

get_return_type_and_out_params_for_csharp(Info, RetTypes,
        RetTypeStr, OutParams) :-
    (
        RetTypes = [],
        RetTypeStr = "void",
        OutParams = []
    ;
        RetTypes = [RetType | OutParams],
        % The first return value is returned directly. Any further return
        % values are returned via out parameters.
        RetTypeStr = type_to_string_for_csharp(Info, RetType)
    ).

params_to_string_for_csharp(Info, Indent, Params) = Str :-
    (
        Params = [],
        Str = "()"
    ;
        Params = [_ | _],
        Indent1Str = indent2_string(Indent + 1),
        ParamStrs =
            list.map(param_to_string_for_csharp(Info, Indent1Str), Params),
        ParamsStr = string.join_list(",\n", ParamStrs),
        string.format("(\n%s)", [s(ParamsStr)], Str)
    ).

:- func param_to_string_for_csharp(csharp_out_info, string, mlds_argument)
    = string.

param_to_string_for_csharp(Info, IndentStr, Arg) = Str :-
    Arg = mlds_argument(VarName, Type, _GCStmt),
    TypeStr = type_to_string_for_csharp(Info, Type),
    VarNameStr = local_var_name_to_ll_string_for_csharp(VarName),
    ( if Type = mlds_ptr_type(_) then
        string.format("%sout %s %s",
            [s(IndentStr), s(TypeStr), s(VarNameStr)], Str)
    else
        string.format("%s%s %s",
            [s(IndentStr), s(TypeStr), s(VarNameStr)], Str)
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_func.
%---------------------------------------------------------------------------%
