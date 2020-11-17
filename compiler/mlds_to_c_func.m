%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output function declarations and definitions.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_c_func.
:- interface.

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c_util.
:- import_module ml_backend.mlds_to_c_type.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred mlds_output_function_decls(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, mlds_module_name::in,
    list(mlds_function_defn)::in, io::di, io::uo) is det.

:- pred mlds_output_function_decl_opts(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, mlds_module_name::in,
    mlds_function_defn::in, io::di, io::uo) is det.

:- pred mlds_output_func_decl_ho(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, qual_function_name::in, prog_context::in, string::in,
    mlds_func_params::in,
    output_type::in(output_type), output_type::in(output_type),
    io::di, io::uo) is det.

:- pred mlds_output_function_defns(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, mlds_module_name::in,
    list(mlds_function_defn)::in, io::di, io::uo) is det.

:- pred mlds_output_function_defn(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, mlds_module_name::in,
    mlds_function_defn::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_pred.            % for pred_proc_id.
:- import_module libs.
:- import_module libs.globals.
:- import_module ml_backend.mlds_to_c_name.
:- import_module ml_backend.mlds_to_c_stmt.
:- import_module parse_tree.parse_tree_out_info.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module maybe.
:- import_module term.

%---------------------------------------------------------------------------%

mlds_output_function_decls(_, _, _, _, [], !IO).
mlds_output_function_decls(Opts, Stream, Indent, ModuleName,
        [FuncDefn | FuncDefns], !IO) :-
    io.nl(Stream, !IO),
    mlds_output_function_decl_opts(Opts, Stream, Indent, ModuleName,
        FuncDefn, !IO),
    mlds_output_function_decls(Opts, Stream, Indent, ModuleName,
        FuncDefns, !IO).

mlds_output_function_decl_opts(Opts, Stream, Indent, ModuleName,
        FunctionDefn, !IO) :-
    FunctionDefn = mlds_function_defn(FuncName, Context, Flags,
        MaybePredProcId, Params, MaybeBody,
        _EnvVarNames, _MaybeRequireTailrecInfo),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Stream, Indent, !IO),
    mlds_output_function_decl_flags(Opts, Stream, Flags, MaybeBody, !IO),
    QualFuncName = qual_function_name(ModuleName, FuncName),

    (
        MaybePredProcId = no
    ;
        MaybePredProcId = yes(PredProcId),
        mlds_output_pred_proc_id(Opts, Stream, PredProcId, !IO)
    ),
    mlds_output_func_decl(Opts, Stream, Indent, QualFuncName, Context,
        Params, !IO),
    io.write_string(Stream, ";\n", !IO).

%---------------------%

:- pred mlds_output_func_decl(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, qual_function_name::in, prog_context::in, mlds_func_params::in,
    io::di, io::uo) is det.

mlds_output_func_decl(Opts, Stream, Indent, QualifiedName, Context,
        Signature, !IO) :-
    CallingConvention = "MR_CALL ",
    mlds_output_func_decl_ho(Opts, Stream, Indent, QualifiedName, Context,
        CallingConvention, Signature,
        mlds_output_type_prefix, mlds_output_type_suffix_no_size, !IO).

mlds_output_func_decl_ho(Opts, Stream, Indent, QualFuncName, Context,
        CallingConvention, Signature, OutputPrefix, OutputSuffix, !IO) :-
    Signature = mlds_func_params(Parameters0, RetTypes),
    (
        RetTypes = [],
        io.write_string(Stream, "void", !IO)
    ;
        RetTypes = [RetType],
        OutputPrefix(Opts, Stream, RetType, !IO)
    ;
        RetTypes = [_, _ | _],
        mlds_output_return_list(Stream,
            mlds_output_prefix_suffix(Opts, OutputPrefix, OutputSuffix),
            RetTypes, !IO)
    ),
    io.write_char(Stream, ' ', !IO),
    io.write_string(Stream, CallingConvention, !IO),
    io.nl(Stream, !IO),
    mlds_output_fully_qualified_function_name(Stream, QualFuncName, !IO),
    StdDecl = Opts ^ m2co_std_func_decl,
    (
        StdDecl = no,
        Parameters = Parameters0
    ;
        StdDecl = yes,
        list.map_foldl(standardize_param_names, Parameters0, Parameters, 1, _)
    ),
    mlds_output_params(Opts, Stream, OutputPrefix, OutputSuffix, Indent,
        Context, Parameters, !IO),
    (
        RetTypes = []
    ;
        RetTypes = [RetType2],
        OutputSuffix(Opts, Stream, RetType2, !IO)
    ;
        RetTypes = [_, _ | _]
    ).

:- pred standardize_param_names(mlds_argument::in, mlds_argument::out,
    int::in, int::out) is det.

standardize_param_names(!Argument, !ArgNum) :-
    VarName = lvn_comp_var(lvnc_param(!.ArgNum)),
    !.Argument = mlds_argument(_VarName0, Type, GCStmt),
    !:Argument = mlds_argument(VarName, Type, GCStmt),
    !:ArgNum = !.ArgNum + 1.

:- pred mlds_output_prefix_suffix(mlds_to_c_opts::in,
    output_type::in(output_type), output_type::in(output_type), mlds_type::in,
    io.text_output_stream::in, io::di, io::uo) is det.

mlds_output_prefix_suffix(Opts, OutputPrefix, OutputSuffix, Value,
        Stream, !IO) :-
    OutputPrefix(Opts, Stream, Value, !IO),
    OutputSuffix(Opts, Stream, Value, !IO).

:- pred mlds_output_params(mlds_to_c_opts::in, io.text_output_stream::in,
    output_type::in(output_type), output_type::in(output_type), indent::in,
    prog_context::in, list(mlds_argument)::in, io::di, io::uo) is det.

mlds_output_params(Opts, Stream, OutputPrefix, OutputSuffix, Indent, Context,
        Parameters, !IO) :-
    io.write_char(Stream, '(', !IO),
    (
        Parameters = [],
        io.write_string(Stream, "void", !IO)
    ;
        Parameters = [_ | _],
        io.nl(Stream, !IO),
        write_out_list(
            mlds_output_param(Opts, OutputPrefix, OutputSuffix,
                Indent + 1, Context),
            ",\n", Parameters, Stream, !IO)
    ),
    io.write_char(Stream, ')', !IO).

:- pred mlds_output_param(mlds_to_c_opts::in,
    output_type::in(output_type), output_type::in(output_type), indent::in,
    prog_context::in, mlds_argument::in,
    io.text_output_stream::in, io::di, io::uo) is det.

mlds_output_param(Opts, OutputPrefix, OutputSuffix, Indent,
        Context, Arg, Stream, !IO) :-
    Arg = mlds_argument(LocalVarName, Type, GCStmt),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Stream, Indent, !IO),
    OutputPrefix(Opts, Stream, Type, !IO),
    io.write_char(Stream, ' ', !IO),
    mlds_output_local_var_name(Stream, LocalVarName, !IO),
    OutputSuffix(Opts, Stream, Type, !IO),
    mlds_output_gc_statement(Opts, Stream, Indent, GCStmt, "\n", !IO).

%---------------------------------------------------------------------------%

mlds_output_function_defns(_, _, _, _, [], !IO).
mlds_output_function_defns(Opts, Stream, Indent, ModuleName,
        [FuncDefn | FuncDefns], !IO) :-
    mlds_output_function_defn(Opts, Stream, Indent, ModuleName,
        FuncDefn, !IO),
    mlds_output_function_defns(Opts, Stream, Indent, ModuleName,
        FuncDefns, !IO).

mlds_output_function_defn(Opts, Stream, Indent, ModuleName,
        FunctionDefn, !IO) :-
    io.nl(Stream, !IO),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Stream, Indent, !IO),
    FunctionDefn = mlds_function_defn(FuncName, Context, Flags,
        MaybePredProcId, Params, MaybeBody,
        _EnvVarNames, _MaybeRequireTailrecInfo),
    mlds_output_function_decl_flags(Opts, Stream, Flags, MaybeBody, !IO),
    (
        MaybePredProcId = no
    ;
        MaybePredProcId = yes(PredProcId),
        mlds_output_pred_proc_id(Opts, Stream, PredProcId, !IO)
    ),
    QualFuncName = qual_function_name(ModuleName, FuncName),
    mlds_output_func(Opts, Stream, Indent, QualFuncName, Context, Params,
        MaybeBody, !IO).

%---------------------------------------------------------------------------%
%
% Code to output function declarations/definitions.
%

:- pred mlds_output_pred_proc_id(mlds_to_c_opts::in, io.text_output_stream::in,
    pred_proc_id::in, io::di, io::uo) is det.

mlds_output_pred_proc_id(Opts, Stream, proc(PredId, ProcId), !IO) :-
    Comments = Opts ^ m2co_auto_comments,
    (
        Comments = yes,
        io.write_string(Stream, "// pred_id: ", !IO),
        pred_id_to_int(PredId, PredIdNum),
        io.write_int(Stream, PredIdNum, !IO),
        io.write_string(Stream, ", proc_id: ", !IO),
        proc_id_to_int(ProcId, ProcIdNum),
        io.write_int(Stream, ProcIdNum, !IO),
        io.nl(Stream, !IO)
    ;
        Comments = no
    ).

:- pred mlds_output_func(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, qual_function_name::in, prog_context::in,
    mlds_func_params::in, mlds_function_body::in, io::di, io::uo) is det.

mlds_output_func(Opts, Stream, Indent, QualFuncName, Context, Params,
        FunctionBody, !IO) :-
    mlds_output_func_decl(Opts, Stream, Indent, QualFuncName, Context,
        Params, !IO),
    (
        FunctionBody = body_external,
        io.write_string(Stream, ";\n", !IO)
    ;
        FunctionBody = body_defined_here(Body),
        io.write_string(Stream, "\n", !IO),

        c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Stream, Indent, !IO),
        io.write_string(Stream, "{\n", !IO),

        ProfileTime = Opts ^ m2co_profile_time,
        (
            ProfileTime = yes,
            mlds_output_time_profile_instr(Opts, Stream, Context, Indent + 1,
                QualFuncName, !IO)
        ;
            ProfileTime = no
        ),

        Signature = mlds_get_func_signature(Params),
        FuncInfo = func_info_c(QualFuncName, Signature),
        mlds_output_statement(Opts, Stream, Indent + 1, FuncInfo, Body, !IO),

        c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Stream, Indent, !IO),
        io.write_string(Stream, "}\n", !IO)    % end the function
    ).

%---------------------------------------------------------------------------%
%
% Code to output declaration specifiers.
%

:- pred mlds_output_function_decl_flags(mlds_to_c_opts::in,
    io.text_output_stream::in, mlds_function_decl_flags::in,
    mlds_function_body::in, io::di, io::uo) is det.

mlds_output_function_decl_flags(Opts, Stream, Flags, MaybeBody, !IO) :-
    Flags = mlds_function_decl_flags(Access, PerInstance),
    Comments = Opts ^ m2co_auto_comments,
    (
        Comments = yes,
        mlds_output_access_comment(Stream, Access, !IO),
        mlds_output_per_instance_comment(Stream, PerInstance, !IO)
    ;
        Comments = no
    ),
    ( if
        Access = func_private,
        % Do not output "static" for functions that do not have a body.
        MaybeBody = body_defined_here(_)
    then
        io.write_string(Stream, "static ", !IO)
    else
        true
    ).

:- pred mlds_output_access_comment(io.text_output_stream::in,
    function_access::in, io::di, io::uo) is det.

mlds_output_access_comment(Stream, func_public, !IO) :-
    io.write_string(Stream, "/* public: */ ", !IO).
mlds_output_access_comment(Stream, func_private, !IO) :-
    io.write_string(Stream, "/* private: */ ", !IO).

:- pred mlds_output_per_instance_comment(io.text_output_stream::in,
    per_instance::in, io::di, io::uo) is det.

mlds_output_per_instance_comment(_, per_instance, !IO).
mlds_output_per_instance_comment(Stream, one_copy, !IO) :-
    io.write_string(Stream, "/* one_copy */ ", !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_func.
%---------------------------------------------------------------------------%
