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

:- import_module libs.
:- import_module libs.indent.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c_type.
:- import_module ml_backend.mlds_to_c_util.
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
    type_prefix_suffix::in(type_prefix_suffix), string::in,
    indent::in, prog_context::in, qual_function_name::in, mlds_func_params::in,
    io::di, io::uo) is det.

:- type maybe_blank_line
    --->    no_blank_line_start
    ;       blank_line_start.

:- pred mlds_output_function_defns(mlds_to_c_opts::in, maybe_blank_line::in,
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
:- import_module libs.globals.
:- import_module ml_backend.mlds_to_c_name.
:- import_module ml_backend.mlds_to_c_stmt.
:- import_module ml_backend.mlds_to_target_util.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module maybe.
:- import_module string.

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
    QualFuncName = qual_function_name(ModuleName, FuncName),
    IndentStr = indent2_string(Indent),
    FlagsPrefix = function_decl_flags_to_prefix_for_c(Opts, Flags, MaybeBody),

    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    (
        MaybePredProcId = no,
        io.format(Stream, "%s%s", [s(IndentStr), s(FlagsPrefix)], !IO)
    ;
        MaybePredProcId = yes(PredProcId),
        CommentPrefix = pred_proc_id_comment_prefix(Opts, PredProcId),
        io.format(Stream, "%s%s%s",
            [s(IndentStr), s(FlagsPrefix), s(CommentPrefix)], !IO)
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
    mlds_output_func_decl_ho(Opts, Stream, type_to_prefix_suffix_for_c_no_size,
        CallingConvention, Indent, Context, QualifiedName, Signature, !IO).

mlds_output_func_decl_ho(Opts, Stream, GetTypePrefixSuffix, CallingConvention,
        Indent, Context, QualFuncName, Signature, !IO) :-
    Signature = mlds_func_params(Parameters0, ReturnTypes),
    StdDecl = Opts ^ m2co_std_func_decl,
    (
        StdDecl = no,
        Parameters = Parameters0
    ;
        StdDecl = yes,
        list.map_foldl(standardize_param_names, Parameters0, Parameters, 1, _)
    ),
    IndentStr = indent2_string(Indent),
    QualFuncNameStr = qual_function_name_to_string_for_c(QualFuncName),
    (
        ReturnTypes = [],
        io.format(Stream, "%svoid %s\n",
            [s(IndentStr), s(CallingConvention)], !IO),
        io.format(Stream, "%s%s", [s(IndentStr), s(QualFuncNameStr)], !IO),
        mlds_output_params_in_parens(Opts, Stream, GetTypePrefixSuffix,
            Indent, Context, Parameters, !IO)
    ;
        ReturnTypes = [ReturnType],
        GetTypePrefixSuffix(Opts, ReturnType,
            ReturnTypePrefix, ReturnTypeSuffix),
        io.format(Stream, "%s%s %s\n",
            [s(IndentStr), s(ReturnTypePrefix), s(CallingConvention)], !IO),
        io.format(Stream, "%s%s", [s(IndentStr), s(QualFuncNameStr)], !IO),
        mlds_output_params_in_parens(Opts, Stream, GetTypePrefixSuffix,
            Indent, Context, Parameters, !IO),
        % NOTE In a --grade hlc.gc bootcheck on 2023 may 16,
        % ReturnTypeSuffix was always empty.
        io.write_string(Stream, ReturnTypeSuffix, !IO)
    ;
        ReturnTypes = [_, _ | _],
        mlds_output_return_list(Stream,
            mlds_output_prefix_suffix(Opts, GetTypePrefixSuffix),
            ReturnTypes, !IO),
        io.format(Stream, "%s%s", [s(IndentStr), s(QualFuncNameStr)], !IO),
        mlds_output_params_in_parens(Opts, Stream, GetTypePrefixSuffix,
            Indent, Context, Parameters, !IO)
    ).

:- pred standardize_param_names(mlds_argument::in, mlds_argument::out,
    int::in, int::out) is det.

standardize_param_names(!Argument, !ArgNum) :-
    VarName = lvn_comp_var(lvnc_param(!.ArgNum)),
    !.Argument = mlds_argument(_VarName0, Type, GCStmt),
    !:Argument = mlds_argument(VarName, Type, GCStmt),
    !:ArgNum = !.ArgNum + 1.

:- pred mlds_output_prefix_suffix(mlds_to_c_opts::in,
    type_prefix_suffix::in(type_prefix_suffix), mlds_type::in,
    io.text_output_stream::in, io::di, io::uo) is det.

mlds_output_prefix_suffix(Opts, GetTypePrefixSuffix, Type, Stream, !IO) :-
    GetTypePrefixSuffix(Opts, Type, TypePrefix, TypeSuffix),
    io.format(Stream, "%s%s", [s(TypePrefix), s(TypeSuffix)], !IO).

:- pred mlds_output_params_in_parens(mlds_to_c_opts::in,
    io.text_output_stream::in, type_prefix_suffix::in(type_prefix_suffix),
    indent::in, prog_context::in, list(mlds_argument)::in,
    io::di, io::uo) is det.

mlds_output_params_in_parens(Opts, Stream, GetTypePrefixSuffix,
        Indent, Context, Args, !IO) :-
    (
        Args = [],
        io.write_string(Stream, "(void)", !IO)
    ;
        Args = [HeadArg | TailArgs],
        io.write_string(Stream, "(\n", !IO),
        mlds_output_params_list(Opts, Stream, GetTypePrefixSuffix, Indent + 1,
            Context, HeadArg, TailArgs, !IO),
        io.write_char(Stream, ')', !IO)
    ).

:- pred mlds_output_params_list(mlds_to_c_opts::in, io.text_output_stream::in,
    type_prefix_suffix::in(type_prefix_suffix), indent::in, prog_context::in,
    mlds_argument::in, list(mlds_argument)::in, io::di, io::uo) is det.

mlds_output_params_list(Opts, Stream, GetTypePrefixSuffix, Indent, Context,
        HeadArg, TailArgs, !IO) :-
    HeadArg = mlds_argument(LocalVarName, Type, GCStmt),
    IndentStr = indent2_string(Indent),
    GetTypePrefixSuffix(Opts, Type, TypePrefix, TypeSuffix),
    LocalVarNameStr = local_var_name_to_string_for_c(LocalVarName),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%s%s %s%s",
        [s(IndentStr), s(TypePrefix), s(LocalVarNameStr), s(TypeSuffix)], !IO),
    % This call outputs nothing except with gc_accurate, which is
    % not likely to be used in practice any time soon. We therefore
    % don't really care how well any non-empty output this call may generate
    % fits into the rest of our output.
    mlds_output_gc_statement(Opts, Stream, Indent, GCStmt, "\n", !IO),
    (
        TailArgs = []
        % Leave the cursor at the end of a line containing the last param.
    ;
        TailArgs = [HeadTailArg | TailTailArgs],
        io.write_string(Stream, ",\n", !IO),
        mlds_output_params_list(Opts, Stream, GetTypePrefixSuffix,
            Indent, Context, HeadTailArg, TailTailArgs, !IO)
    ).

%---------------------------------------------------------------------------%

mlds_output_function_defns(_, _, _, _, _, [], !IO).
mlds_output_function_defns(Opts, BlankLine, Stream, Indent, ModuleName,
        [FuncDefn | FuncDefns], !IO) :-
    (
        BlankLine = no_blank_line_start
    ;
        BlankLine = blank_line_start,
        io.nl(Stream, !IO)
    ),
    mlds_output_function_defn(Opts, Stream, Indent,
        ModuleName, FuncDefn, !IO),
    mlds_output_function_defns(Opts, blank_line_start, Stream, Indent,
        ModuleName, FuncDefns, !IO).

mlds_output_function_defn(Opts, Stream, Indent, ModuleName,
        FunctionDefn, !IO) :-
    FunctionDefn = mlds_function_defn(FuncName, Context, Flags,
        MaybePredProcId, Params, MaybeBody,
        _EnvVarNames, _MaybeRequireTailrecInfo),
    IndentStr = indent2_string(Indent),
    FlagsPrefix = function_decl_flags_to_prefix_for_c(Opts, Flags, MaybeBody),

    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    (
        MaybePredProcId = no,
        io.format(Stream, "%s%s", [s(IndentStr), s(FlagsPrefix)], !IO)
    ;
        MaybePredProcId = yes(PredProcId),
        CommentPrefix = pred_proc_id_comment_prefix(Opts, PredProcId),
        io.format(Stream, "%s%s%s",
            [s(IndentStr), s(FlagsPrefix), s(CommentPrefix)], !IO)
    ),
    QualFuncName = qual_function_name(ModuleName, FuncName),
    mlds_output_func(Opts, Stream, Indent, QualFuncName, Context, Params,
        MaybeBody, !IO).

%---------------------------------------------------------------------------%
%
% Code to output function declarations/definitions.
%

:- func pred_proc_id_comment_prefix(mlds_to_c_opts, pred_proc_id) = string.

pred_proc_id_comment_prefix(Opts, PredProcId) = CommentPrefix :-
    Comments = Opts ^ m2co_auto_comments,
    (
        Comments = yes,
        PredProcId = proc(PredId, ProcId),
        PredIdInt = pred_id_to_int(PredId),
        ProcIdInt = proc_id_to_int(ProcId),
        string.format("/* pred_id: %d, proc_id: %d */ ",
            [i(PredIdInt), i(ProcIdInt)], CommentPrefix)
    ;
        Comments = no,
        CommentPrefix = ""
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
        FunctionBody = body_defined_here(BodyStmt),
        io.write_string(Stream, "\n", !IO),

        LineNumbers = Opts ^ m2co_line_numbers,
        ProfileTime = Opts ^ m2co_profile_time,
        Signature = mlds_get_func_signature(Params),
        FuncInfo = func_info_c(QualFuncName, Signature),
        ( if
            LineNumbers = no,
            ProfileTime = no,
            BodyStmt = ml_stmt_block(_, _, _, _)
        then
            % The entire output of this call will have braces around it.
            % mlds_output_statement puts them there to create a scope
            % for the block, but they also work to wrap the function.
            mlds_output_statement(Opts, Stream, Indent, FuncInfo,
                BodyStmt, !IO)
        else
            c_output_context(Stream, LineNumbers, Context, !IO),
            IndentStr = indent2_string(Indent),
            % start of the function
            io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
            (
                ProfileTime = yes,
                mlds_output_time_profile_instr(Opts, Stream, Context,
                    Indent + 1, QualFuncName, !IO)
            ;
                ProfileTime = no
            ),
            mlds_output_statement(Opts, Stream, Indent + 1, FuncInfo,
                BodyStmt, !IO),
            c_output_context(Stream, LineNumbers, Context, !IO),
            % end of the function
            io.format(Stream, "%s}\n", [s(IndentStr)], !IO)
        )
    ).

%---------------------------------------------------------------------------%
%
% Code to output declaration specifiers.
%

:- func function_decl_flags_to_prefix_for_c(mlds_to_c_opts,
    mlds_function_decl_flags, mlds_function_body) = string.

function_decl_flags_to_prefix_for_c(Opts, Flags, MaybeBody) = FlagsPrefix :-
    Flags = mlds_function_decl_flags(Access, PerInstance),
    ( if
        Access = func_private,
        % Do not output "static" for functions that do not have a body.
        MaybeBody = body_defined_here(_)
    then
        MaybeStaticPrefix = "static "
    else
        MaybeStaticPrefix = ""
    ),
    Comments = Opts ^ m2co_auto_comments,
    (
        Comments = yes,
        (
            Access = func_public,
            (
                PerInstance = per_instance,
                AccessPerInstancePrefix = "/* public */ "
            ;
                PerInstance = one_copy,
                AccessPerInstancePrefix = "/* public one_copy */ "
            )
        ;
            Access = func_private,
            (
                PerInstance = per_instance,
                AccessPerInstancePrefix = "/* private */ "
            ;
                PerInstance = one_copy,
                AccessPerInstancePrefix = "/* private one_copy */ "
            )
        ),
        FlagsPrefix = AccessPerInstancePrefix ++ MaybeStaticPrefix
    ;
        Comments = no,
        FlagsPrefix = MaybeStaticPrefix
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_func.
%---------------------------------------------------------------------------%
