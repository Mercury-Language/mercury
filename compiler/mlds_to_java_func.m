%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS function declarations and definitions in Java.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_java_func.
:- interface.

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_java_util.
:- import_module ml_backend.mlds_to_target_util.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred output_func_decl_for_java(java_out_info::in, indent::in,
    mlds_function_name::in, output_aux::in, mlds_func_params::in,
    io::di, io::uo) is det.

:- pred output_params_for_java(java_out_info::in, indent::in,
    list(mlds_argument)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_function_defn_for_java(java_out_info::in, indent::in,
    output_aux::in, mlds_function_defn::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module ml_backend.mlds_to_java_name.
:- import_module ml_backend.mlds_to_java_stmt.
:- import_module ml_backend.mlds_to_java_type.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module maybe.
:- import_module term.

%---------------------------------------------------------------------------%

output_func_decl_for_java(Info, Indent, FuncName, OutputAux, Signature, !IO) :-
    Signature = mlds_func_params(Parameters, RetTypes),
    ( if
        OutputAux = oa_cname(ClassName, ClassArity),
        FuncName = mlds_function_export("<constructor>")
    then
        output_class_name_arity_for_java(ClassName, ClassArity, !IO)
    else
        output_return_types_for_java(Info, RetTypes, !IO),
        io.nl(!IO),
        output_n_indents(Indent, !IO),
        output_function_name_for_java(FuncName, !IO)
    ),
    output_params_for_java(Info, Indent, Parameters, !IO).

:- pred output_return_types_for_java(java_out_info::in, mlds_return_types::in,
    io::di, io::uo) is det.

output_return_types_for_java(Info, RetTypes, !IO) :-
    (
        RetTypes = [],
        io.write_string("void", !IO)
    ;
        RetTypes = [RetType],
        output_type_for_java(Info, RetType, !IO)
    ;
        RetTypes = [_, _ | _],
        % For multiple outputs, we return an array of objects.
        io.write_string("java.lang.Object []", !IO)
    ).

output_params_for_java(Info, Indent, Parameters, !IO) :-
    io.write_char('(', !IO),
    (
        Parameters = []
    ;
        Parameters = [_ | _],
        io.nl(!IO),
        io.write_list(Parameters, ",\n", output_param(Info, Indent + 1), !IO)
    ),
    io.write_char(')', !IO).

:- pred output_param(java_out_info::in, indent::in, mlds_argument::in,
    io::di, io::uo) is det.

output_param(Info, Indent, Arg, !IO) :-
    Arg = mlds_argument(VarName, Type, _GCStmt),
    output_n_indents(Indent, !IO),
    output_type_for_java(Info, Type, !IO),
    io.write_char(' ', !IO),
    output_local_var_name_for_java(VarName, !IO).

%---------------------------------------------------------------------------%

output_function_defn_for_java(Info, Indent, OutputAux, FunctionDefn, !IO) :-
    FunctionDefn = mlds_function_defn(Name, Context, Flags, MaybePredProcId,
        Params, MaybeBody, _EnvVarNames, _MaybeRequireTailrecInfo),
    io.nl(!IO),
    (
        MaybeBody = body_external,
        % This is just a function declaration, with no body.
        % Java doesn't support separate declarations and definitions,
        % so just output the declaration as a comment.
        % (Note that the actual definition of an external procedure
        % must be given in `pragma java_code' in the same module.)
        %
        % XXX For now, we print only the name of the function.
        % We would like to print the whole declaration in a comment,
        % but that does not work. For some argument types in the function
        % declaration, we may print a comment before the Java type
        % (see type_to_string_for_java). This would yield nested comments,
        % which Java does not allow.
        indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("// external: ", !IO),
        output_function_name_for_java(Name, !IO),
        io.nl(!IO)
    ;
        MaybeBody = body_defined_here(_),
        indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        output_function_decl_flags_for_java(Info, Flags, !IO),
        (
            MaybePredProcId = no
        ;
            MaybePredProcId = yes(PredProcid),
            maybe_output_pred_proc_id_comment(Info ^ joi_auto_comments,
                PredProcid, !IO)
        ),
        output_func_for_java(Info, Indent, Name, OutputAux, Context,
            Params, MaybeBody, !IO)
    ).

:- pred output_func_for_java(java_out_info::in, indent::in,
    mlds_function_name::in, output_aux::in, prog_context::in,
    mlds_func_params::in, mlds_function_body::in, io::di, io::uo) is det.

output_func_for_java(Info, Indent, FuncName, OutputAux, Context, Signature,
        MaybeBody, !IO) :-
    output_func_decl_for_java(Info, Indent, FuncName, OutputAux,
        Signature, !IO),
    io.write_string("\n", !IO),
    (
        MaybeBody = body_external
        % The signature above will be printed inside a comment.
    ;
        MaybeBody = body_defined_here(Body),
        FuncInfo = func_info_csj(Signature),
        % Do not place redundant brackets around a block.
        ( if Body = ml_stmt_block(_, _, _, _) then
            output_statement_for_java(Info, Indent, FuncInfo, Body,
                _ExitMethods, !IO),
            indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
                Context, Indent, !IO)
        else
            io.write_string("{\n", !IO),
            output_statement_for_java(Info, Indent + 1, FuncInfo, Body,
                _ExitMethods, !IO),
            indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
                Context, Indent, !IO),
            io.write_string("}\n", !IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred output_function_decl_flags_for_java(java_out_info::in,
    mlds_function_decl_flags::in, io::di, io::uo) is det.

output_function_decl_flags_for_java(Info, Flags, !IO) :-
    Flags = mlds_function_decl_flags(Access, PerInstance),
    output_access_for_java(Info, Access, !IO),
    output_per_instance_for_java(PerInstance, !IO).

:- pred output_access_for_java(java_out_info::in, function_access::in,
    io::di, io::uo) is det.

output_access_for_java(_Info, Access, !IO) :-
    (
        Access = func_public,
        io.write_string("public ", !IO)
    ;
        Access = func_private,
        io.write_string("private ", !IO)
    ).

:- pred output_per_instance_for_java(per_instance::in, io::di, io::uo) is det.

output_per_instance_for_java(PerInstance, !IO) :-
    (
        PerInstance = per_instance
    ;
        PerInstance = one_copy,
        io.write_string("static ", !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_java_func.
%---------------------------------------------------------------------------%
