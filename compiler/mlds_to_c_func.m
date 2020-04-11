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

:- pred mlds_output_function_decls(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, list(mlds_function_defn)::in,
    io::di, io::uo) is det.

:- pred mlds_output_function_decl_opts(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, mlds_function_defn::in, io::di, io::uo) is det.

:- pred mlds_output_func_decl_ho(mlds_to_c_opts::in, indent::in,
    qual_function_name::in, prog_context::in, string::in,
    mlds_func_params::in,
    output_type::in(output_type), output_type::in(output_type),
    io::di, io::uo) is det.

:- pred mlds_output_function_defns(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, list(mlds_function_defn)::in,
    io::di, io::uo) is det.

:- pred mlds_output_function_defn(mlds_to_c_opts::in, indent::in,
    mlds_module_name::in, mlds_function_defn::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_pred.            % for pred_proc_id.
:- import_module libs.
:- import_module libs.globals.
:- import_module ml_backend.ml_type_gen.    % for ml_gen_type_name
:- import_module ml_backend.mlds_to_c_name.
:- import_module ml_backend.mlds_to_c_stmt.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module maybe.
:- import_module solutions.
:- import_module term.

%---------------------------------------------------------------------------%

mlds_output_function_decls(_Opts, _Indent, _ModuleName, [], !IO).
mlds_output_function_decls(Opts, Indent, ModuleName,
        [FuncDefn | FuncDefns], !IO) :-
    io.nl(!IO),
    mlds_output_function_decl_opts(Opts, Indent, ModuleName, FuncDefn, !IO),
    mlds_output_function_decls(Opts, Indent, ModuleName, FuncDefns, !IO).

mlds_output_function_decl_opts(Opts, Indent, ModuleName, FunctionDefn, !IO) :-
    FunctionDefn = mlds_function_defn(FuncName, Context, Flags,
        MaybePredProcId, Params, MaybeBody,
        _EnvVarNames, _MaybeRequireTailrecInfo),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    mlds_output_function_decl_flags(Opts, Flags, MaybeBody, !IO),
    QualFuncName = qual_function_name(ModuleName, FuncName),

    (
        MaybePredProcId = no
    ;
        MaybePredProcId = yes(PredProcId),
        mlds_output_pred_proc_id(Opts, PredProcId, !IO)
    ),
    mlds_output_func_decl(Opts, Indent, QualFuncName, Context, Params, !IO),
    io.write_string(";\n", !IO).

%---------------------%

:- pred mlds_output_type_forward_decls(mlds_to_c_opts::in, indent::in,
    list(mlds_type)::in, io::di, io::uo) is det.

mlds_output_type_forward_decls(Opts, Indent, ParamTypes, !IO) :-
    % Output forward declarations for all struct types
    % that are contained in the parameter types.
    solutions.aggregate(mlds_type_list_contains_type(ParamTypes),
        mlds_output_type_forward_decl(Opts, Indent), !IO).

    % mlds_type_list_contains_type(Types, SubType):
    %
    % True iff the type SubType occurs (directly or indirectly) in the
    % specified list of Types.
    %
:- pred mlds_type_list_contains_type(list(mlds_type)::in, mlds_type::out)
    is nondet.

mlds_type_list_contains_type(Types, SubType) :-
    list.member(Type, Types),
    mlds_type_contains_type(Type, SubType).

    % mlds_type_contains_type(Type, SubType):
    %
    % True iff the type Type contains the type SubType.
    %
:- pred mlds_type_contains_type(mlds_type::in, mlds_type::out) is multi.

mlds_type_contains_type(Type, Type).
mlds_type_contains_type(mlds_mercury_array_type(Type), Type).
mlds_type_contains_type(mlds_array_type(Type), Type).
mlds_type_contains_type(mlds_ptr_type(Type), Type).
mlds_type_contains_type(mlds_func_type(Parameters), Type) :-
    Parameters = mlds_func_params(Arguments, RetTypes),
    ( list.member(mlds_argument(_Name, Type, _GCStmt), Arguments)
    ; list.member(Type, RetTypes)
    ).

:- pred mlds_output_type_forward_decl(mlds_to_c_opts::in, indent::in,
    mlds_type::in, io::di, io::uo) is det.

mlds_output_type_forward_decl(Opts, Indent, Type, !IO) :-
    ( if
        (
            Type = mlds_class_type(ClassId),
            ClassId = mlds_class_id(_Name, _Arity, Kind),
            Kind \= mlds_enum,
            ClassType = Type
        ;
            Type = mercury_nb_type(MercuryType, ctor_cat_user(_)),
            type_to_ctor(MercuryType, TypeCtor),
            ml_gen_type_name(TypeCtor, ClassName, ClassArity),
            ClassId = mlds_class_id(ClassName, ClassArity, mlds_class),
            ClassType = mlds_class_type(ClassId)
        )
    then
        output_n_indents(Indent, !IO),
        mlds_output_type(Opts, ClassType, !IO),
        io.write_string(";\n", !IO)
    else
        true
    ).

%---------------------%

:- pred mlds_output_func_decl(mlds_to_c_opts::in, indent::in,
    qual_function_name::in, prog_context::in, mlds_func_params::in,
    io::di, io::uo) is det.

mlds_output_func_decl(Opts, Indent, QualifiedName, Context, Signature, !IO) :-
    CallingConvention = "MR_CALL ",
    mlds_output_func_decl_ho(Opts, Indent, QualifiedName, Context,
        CallingConvention, Signature,
        mlds_output_type_prefix, mlds_output_type_suffix_no_size, !IO).

mlds_output_func_decl_ho(Opts, Indent, QualFuncName, Context,
        CallingConvention, Signature, OutputPrefix, OutputSuffix, !IO) :-
    Signature = mlds_func_params(Parameters0, RetTypes),
    (
        RetTypes = [],
        io.write_string("void", !IO)
    ;
        RetTypes = [RetType],
        OutputPrefix(Opts, RetType, !IO)
    ;
        RetTypes = [_, _ | _],
        mlds_output_return_list(RetTypes,
            mlds_output_prefix_suffix(Opts, OutputPrefix, OutputSuffix), !IO)
    ),
    io.write_char(' ', !IO),
    io.write_string(CallingConvention, !IO),
    io.nl(!IO),
    mlds_output_fully_qualified_function_name(QualFuncName, !IO),
    StdDecl = Opts ^ m2co_std_func_decl,
    (
        StdDecl = no,
        Parameters = Parameters0
    ;
        StdDecl = yes,
        list.map_foldl(standardize_param_names, Parameters0, Parameters, 1, _)
    ),
    mlds_output_params(Opts, OutputPrefix, OutputSuffix, Indent,
        Context, Parameters, !IO),
    (
        RetTypes = [RetType2],
        OutputSuffix(Opts, RetType2, !IO)
    ;
        RetTypes = []
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
    io::di, io::uo) is det.

mlds_output_prefix_suffix(Opts, OutputPrefix, OutputSuffix, Value, !IO) :-
    OutputPrefix(Opts, Value, !IO),
    OutputSuffix(Opts, Value, !IO).

:- pred mlds_output_params(mlds_to_c_opts::in, output_type::in(output_type),
    output_type::in(output_type), indent::in,
    prog_context::in, list(mlds_argument)::in, io::di, io::uo) is det.

mlds_output_params(Opts, OutputPrefix, OutputSuffix, Indent, Context,
        Parameters, !IO) :-
    io.write_char('(', !IO),
    (
        Parameters = [],
        io.write_string("void", !IO)
    ;
        Parameters = [_ | _],
        io.nl(!IO),
        io.write_list(Parameters, ",\n",
            mlds_output_param(Opts, OutputPrefix, OutputSuffix,
                Indent + 1, Context),
            !IO)
    ),
    io.write_char(')', !IO).

:- pred mlds_output_param(mlds_to_c_opts::in, output_type::in(output_type),
    output_type::in(output_type), indent::in,
    prog_context::in, mlds_argument::in, io::di, io::uo) is det.

mlds_output_param(Opts, OutputPrefix, OutputSuffix, Indent,
        Context, Arg, !IO) :-
    Arg = mlds_argument(LocalVarName, Type, GCStmt),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    OutputPrefix(Opts, Type, !IO),
    io.write_char(' ', !IO),
    mlds_output_local_var_name(LocalVarName, !IO),
    OutputSuffix(Opts, Type, !IO),
    mlds_output_gc_statement(Opts, Indent, GCStmt, "\n", !IO).

%---------------------------------------------------------------------------%

mlds_output_function_defns(_Opts, _Indent, _ModuleName, [], !IO).
mlds_output_function_defns(Opts, Indent, ModuleName,
        [FuncDefn | FuncDefns], !IO) :-
    mlds_output_function_defn(Opts, Indent, ModuleName, FuncDefn, !IO),
    mlds_output_function_defns(Opts, Indent, ModuleName, FuncDefns, !IO).

mlds_output_function_defn(Opts, Indent, ModuleName, FunctionDefn, !IO) :-
    io.nl(!IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    FunctionDefn = mlds_function_defn(FuncName, Context, Flags,
        MaybePredProcId, Params, MaybeBody,
        _EnvVarNames, _MaybeRequireTailrecInfo),
    mlds_output_function_decl_flags(Opts, Flags, MaybeBody, !IO),
    (
        MaybePredProcId = no
    ;
        MaybePredProcId = yes(PredProcId),
        mlds_output_pred_proc_id(Opts, PredProcId, !IO)
    ),
    QualFuncName = qual_function_name(ModuleName, FuncName),
    mlds_output_func(Opts, Indent, QualFuncName, Context, Params,
        MaybeBody, !IO).

%---------------------------------------------------------------------------%
%
% Code to output function declarations/definitions.
%

:- pred mlds_output_pred_proc_id(mlds_to_c_opts::in, pred_proc_id::in,
    io::di, io::uo) is det.

mlds_output_pred_proc_id(Opts, proc(PredId, ProcId), !IO) :-
    Comments = Opts ^ m2co_auto_comments,
    (
        Comments = yes,
        io.write_string("// pred_id: ", !IO),
        pred_id_to_int(PredId, PredIdNum),
        io.write_int(PredIdNum, !IO),
        io.write_string(", proc_id: ", !IO),
        proc_id_to_int(ProcId, ProcIdNum),
        io.write_int(ProcIdNum, !IO),
        io.nl(!IO)
    ;
        Comments = no
    ).

:- pred mlds_output_func(mlds_to_c_opts::in, indent::in,
    qual_function_name::in, prog_context::in,
    mlds_func_params::in, mlds_function_body::in, io::di, io::uo) is det.

mlds_output_func(Opts, Indent, QualFuncName, Context, Params,
        FunctionBody, !IO) :-
    mlds_output_func_decl(Opts, Indent, QualFuncName, Context, Params, !IO),
    (
        FunctionBody = body_external,
        io.write_string(";\n", !IO)
    ;
        FunctionBody = body_defined_here(Body),
        io.write_string("\n", !IO),

        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("{\n", !IO),

        ProfileTime = Opts ^ m2co_profile_time,
        (
            ProfileTime = yes,
            mlds_output_time_profile_instr(Opts, Context, Indent + 1,
                QualFuncName, !IO)
        ;
            ProfileTime = no
        ),

        Signature = mlds_get_func_signature(Params),
        FuncInfo = func_info_c(QualFuncName, Signature),
        mlds_output_statement(Opts, Indent + 1, FuncInfo, Body, !IO),

        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("}\n", !IO)    % end the function
    ).

%---------------------------------------------------------------------------%
%
% Code to output declaration specifiers.
%

:- pred mlds_output_function_decl_flags(mlds_to_c_opts::in,
    mlds_function_decl_flags::in, mlds_function_body::in,
    io::di, io::uo) is det.

mlds_output_function_decl_flags(Opts, Flags, MaybeBody, !IO) :-
    Flags = mlds_function_decl_flags(Access, PerInstance),
    Comments = Opts ^ m2co_auto_comments,
    (
        Comments = yes,
        mlds_output_access_comment(Access, !IO),
        mlds_output_per_instance_comment(PerInstance, !IO)
    ;
        Comments = no
    ),
    ( if
        Access = func_private,
        % Do not output "static" for functions that do not have a body.
        MaybeBody = body_defined_here(_)
    then
        io.write_string("static ", !IO)
    else
        true
    ).

:- pred mlds_output_access_comment(function_access::in, io::di, io::uo) is det.

mlds_output_access_comment(func_public, !IO) :-
    io.write_string("/* public: */ ", !IO).
mlds_output_access_comment(func_private, !IO) :-
    io.write_string("/* private: */ ", !IO).

:- pred mlds_output_per_instance_comment(per_instance::in,
    io::di, io::uo) is det.

mlds_output_per_instance_comment(per_instance, !IO).
mlds_output_per_instance_comment(one_copy, !IO) :-
    io.write_string("/* one_copy */ ", !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_func.
%---------------------------------------------------------------------------%
