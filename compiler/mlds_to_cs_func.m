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

:- pred output_function_defn_for_csharp(csharp_out_info::in, indent::in,
    output_aux::in, mlds_function_defn::in, io::di, io::uo) is det.

:- pred output_params_for_csharp(csharp_out_info::in, indent::in,
    list(mlds_argument)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.            % for pred_proc_id
:- import_module ml_backend.mlds_to_cs_name.
:- import_module ml_backend.mlds_to_cs_stmt.
:- import_module ml_backend.mlds_to_cs_type.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module term.

%---------------------------------------------------------------------------%

output_function_defn_for_csharp(Info, Indent, OutputAux, FunctionDefn, !IO) :-
    output_n_indents(Indent, !IO),
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
        PreStr = "/* external:\n",
        PostStr = "*/\n"
    ;
        MaybeBody = body_defined_here(_),
        PreStr = "",
        PostStr = ""
    ),
    io.write_string(PreStr, !IO),
    output_function_decl_flags_for_csharp(Info, Flags, !IO),
    (
        MaybePredProcId = no
    ;
        MaybePredProcId = yes(PredProcId),
        maybe_output_pred_proc_id_comment(Info ^ csoi_auto_comments,
            PredProcId, !IO)
    ),
    output_func_for_csharp(Info, Indent, FuncName, OutputAux, Context,
        Params, MaybeBody, !IO),
    io.write_string(PostStr, !IO).

%---------------------------------------------------------------------------%

:- pred output_function_decl_flags_for_csharp(csharp_out_info::in,
    mlds_function_decl_flags::in, io::di, io::uo) is det.

output_function_decl_flags_for_csharp(Info, Flags, !IO) :-
    Flags = mlds_function_decl_flags(Access, PerInstance),
    output_access_for_csharp(Info, Access, !IO),
    output_per_instance_for_csharp(PerInstance, !IO).

:- pred output_access_for_csharp(csharp_out_info::in, function_access::in,
    io::di, io::uo) is det.

output_access_for_csharp(_Info, Access, !IO) :-
    (
        Access = func_public,
        io.write_string("public ", !IO)
    ;
        Access = func_private,
        io.write_string("private ", !IO)
    ).

:- pred output_per_instance_for_csharp(per_instance::in,
    io::di, io::uo) is det.

output_per_instance_for_csharp(PerInstance, !IO) :-
    (
        PerInstance = per_instance
    ;
        PerInstance = one_copy,
        io.write_string("static ", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred output_func_for_csharp(csharp_out_info::in, indent::in,
    mlds_function_name::in, output_aux::in, prog_context::in,
    mlds_func_params::in, mlds_function_body::in, io::di, io::uo) is det.

output_func_for_csharp(Info, Indent, FuncName, OutputAux, Context, Signature,
        MaybeBody, !IO) :-
    (
        MaybeBody = body_defined_here(Body),
        output_func_decl_for_csharp(Info, Indent, FuncName, OutputAux,
            Signature, !IO),
        io.write_string("\n", !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("{\n", !IO),
        FuncInfo = func_info_csj(Signature),
        output_stmt_for_csharp(Info, Indent + 1, FuncInfo, Body,
            _ExitMethods, !IO),
        indent_line_after_context(Info ^ csoi_line_numbers, Context,
            Indent, !IO),
        io.write_string("}\n", !IO),    % end the function
        cs_output_default_context(Info ^ csoi_line_numbers, !IO)
    ;
        MaybeBody = body_external
    ).

:- pred output_func_decl_for_csharp(csharp_out_info::in, indent::in,
    mlds_function_name::in, output_aux::in, mlds_func_params::in,
    io::di, io::uo) is det.

output_func_decl_for_csharp(Info, Indent, FuncName, OutputAux, Signature,
        !IO) :-
    Signature = mlds_func_params(Parameters, RetTypes),
    ( if
        OutputAux = oa_cname(ClassName, ClassArity),
        FuncName = mlds_function_export("<constructor>")
    then
        output_class_name_arity_for_csharp(ClassName, ClassArity, !IO),
        OutParams = []
    else
        output_return_types_for_csharp(Info, RetTypes, RestRetTypes, !IO),
        io.write_char(' ', !IO),
        output_function_name_for_csharp(FuncName, !IO),
        list.map_foldl(make_out_param, RestRetTypes, OutParams, 2, _)
    ),
    output_params_for_csharp(Info, Indent, Parameters ++ OutParams, !IO).

:- pred make_out_param(mlds_type::in, mlds_argument::out, int::in, int::out)
    is det.

make_out_param(Type, Argument, Num, Num + 1) :-
    VarName = lvn_comp_var(lvnc_out_param(Num)),
    Argument = mlds_argument(VarName, mlds_ptr_type(Type), gc_no_stmt).

:- pred output_return_types_for_csharp(csharp_out_info::in,
    mlds_return_types::in, list(mlds_type)::out, io::di, io::uo) is det.

output_return_types_for_csharp(Info, RetTypes, OutParams, !IO) :-
    (
        RetTypes = [],
        io.write_string("void", !IO),
        OutParams = []
    ;
        RetTypes = [RetType | OutParams],
        % The first return value is returned directly. Any further return
        % values are returned via out parameters.
        output_type_for_csharp(Info, RetType, !IO)
    ).

output_params_for_csharp(Info, Indent, Parameters, !IO) :-
    io.write_char('(', !IO),
    (
        Parameters = []
    ;
        Parameters = [_ | _],
        io.nl(!IO),
        io.write_list(Parameters, ",\n",
            output_param_for_csharp(Info, Indent + 1), !IO)
    ),
    io.write_char(')', !IO).

:- pred output_param_for_csharp(csharp_out_info::in, indent::in,
    mlds_argument::in, io::di, io::uo) is det.

output_param_for_csharp(Info, Indent, Arg, !IO) :-
    Arg = mlds_argument(Name, Type, _GCStmt),
    output_n_indents(Indent, !IO),
    ( if Type = mlds_ptr_type(_) then
        io.write_string("out ", !IO)
    else
        true
    ),
    output_type_for_csharp(Info, Type, !IO),
    io.write_char(' ', !IO),
    output_local_var_name_for_csharp(Name, !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_func.
%---------------------------------------------------------------------------%
