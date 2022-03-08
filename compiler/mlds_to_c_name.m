%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output the names of various entities.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_c_name.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.

%---------------------------------------------------------------------------%

:- func should_module_qualify_global_var_name(mlds_global_var_name) = bool.

:- pred mlds_output_maybe_qualified_global_var_name(io.text_output_stream::in,
    mlds_module_name::in, mlds_global_var_name::in, io::di, io::uo) is det.

:- func global_var_name(global_var_ref) = string.

:- func mlds_tabling_data_name(mlds_proc_label, proc_tabling_struct_id)
    = string.

%---------------------------------------------------------------------------%

:- pred mlds_output_fully_qualified_function_name(io.text_output_stream::in,
    qual_function_name::in, io::di, io::uo) is det.

:- pred mlds_output_fully_qualified_proc_label(io.text_output_stream::in,
    qual_proc_label::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mlds_output_class_name_arity(io.text_output_stream::in,
    mlds_class_name::in, arity::in, io::di, io::uo) is det.

:- pred mlds_output_fully_qualified_field_var_name(io.text_output_stream::in,
    qual_field_var_name::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mlds_output_local_var_name(io.text_output_stream::in,
    mlds_local_var_name::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_qual_name_prefix_c(io.text_output_stream::in,
    mlds_module_name::in, io::di, io::uo) is det.

:- func qual_name_prefix_c(mlds_module_name) = string.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.prog_foreign.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

%---------------------------------------------------------------------------%

should_module_qualify_global_var_name(GlobalVarName) = ShouldModuleQual :-
    (
        GlobalVarName = gvn_rtti_var(RttiId),
        ShouldModuleQual = module_qualify_name_of_rtti_id(RttiId)
    ;
        ( GlobalVarName = gvn_const_var(_, _)
        ; GlobalVarName = gvn_tabling_var(_, _)
        ),
        ShouldModuleQual = no
    ;
        GlobalVarName = gvn_dummy_var,
        % The reference is to private_builtin.dummy_var, which is not in the
        % current module (unless we are compiling private_builtin.m).
        ShouldModuleQual = yes
    ).

mlds_output_maybe_qualified_global_var_name(Stream, ModuleName, GlobalVarName,
        !IO) :-
    ShouldModuleQual = should_module_qualify_global_var_name(GlobalVarName),
    (
        ShouldModuleQual = no
    ;
        ShouldModuleQual = yes,
        output_qual_name_prefix_c(Stream, ModuleName, !IO)
    ),
    mlds_output_global_var_name(Stream, GlobalVarName, !IO).

:- pred mlds_output_global_var_name(io.text_output_stream::in,
    mlds_global_var_name::in, io::di, io::uo) is det.

mlds_output_global_var_name(Stream, GlobalVarName, !IO) :-
    (
        GlobalVarName = gvn_const_var(ConstVar, Num),
        MangledGlobalVarName =
            name_mangle(ml_global_const_var_name_to_string(ConstVar, Num)),
        io.write_string(Stream, MangledGlobalVarName, !IO)
    ;
        GlobalVarName = gvn_rtti_var(RttiId),
        rtti.id_to_c_identifier(RttiId, RttiAddrName),
        io.write_string(Stream, RttiAddrName, !IO)
    ;
        GlobalVarName = gvn_tabling_var(ProcLabel, TablingId),
        io.write_string(Stream, mlds_tabling_data_name(ProcLabel, TablingId),
            !IO)
    ;
        GlobalVarName = gvn_dummy_var,
        io.write_string(Stream, "dummy_var", !IO)
    ).

% The calls to env_var_is_acceptable_char in parse_goal.m ensure that
% EnvVarName is acceptable as part of a C identifier.
% The prefix must be identical to envvar_prefix in util/mkinit.c
% and c_global_var_name in llds_out.m.
global_var_name(env_var_ref(EnvVarName)) = "mercury_envvar_" ++ EnvVarName.

mlds_tabling_data_name(ProcLabel, TablingId) =
    tabling_info_id_str(TablingId) ++ "_for_" ++
        mlds_proc_label_to_string(mlds_std_tabling_proc_label(ProcLabel)).

%---------------------------------------------------------------------------%

mlds_output_fully_qualified_function_name(Stream, QualFuncName, !IO) :-
    QualFuncName = qual_function_name(ModuleName, FuncName),
    ( if
        (
            % Do not module-qualify main/2.
            FuncName = mlds_function_name(PlainFuncName),
            PlainFuncName = mlds_plain_func_name(FuncLabel, _),
            FuncLabel = mlds_func_label(ProcLabel, _MaybeSeqNum),
            ProcLabel = mlds_proc_label(PredLabel, _ProcId),
            PredLabel = mlds_user_pred_label(pf_predicate, no, "main", 2,
                model_det, no)
        ;
            % We do not module qualify pragma foreign_export names.
            FuncName = mlds_function_export(_)
        )
    then
        true
    else
        output_qual_name_prefix_c(Stream, ModuleName, !IO)
    ),
    mlds_output_function_name(Stream, FuncName, !IO).

:- pred mlds_output_function_name(io.text_output_stream::in,
    mlds_function_name::in, io::di, io::uo) is det.

mlds_output_function_name(Stream, FunctionName, !IO) :-
    % XXX We should avoid appending the modenum and seqnum
    % if they are not needed.
    (
        FunctionName = mlds_function_name(PlainFuncName),
        PlainFuncName = mlds_plain_func_name(FuncLabel, _PredId),
        FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
        ProcLabel = mlds_proc_label(PredLabel, ProcId),
        proc_id_to_int(ProcId, ModeNum),
        FuncIdSuffix = mlds_maybe_aux_func_id_to_suffix(MaybeAux),
        io.format(Stream, "%s_%d%s",
            [s(mlds_pred_label_to_string(PredLabel)),
            i(ModeNum), s(FuncIdSuffix)], !IO)
    ;
        FunctionName = mlds_function_export(Name),
        io.write_string(Stream, Name, !IO)
    ).

%---------------------%

mlds_output_fully_qualified_proc_label(Stream, QualProcLabel, !IO) :-
    QualProcLabel = qual_proc_label(ModuleName, Name),
    Name = mlds_proc_label(PredLabel, _ProcId),
    ( if
        % Do not module-qualify main/2.
        PredLabel = mlds_user_pred_label(pf_predicate, no, "main", 2,
            model_det, no)
    then
        true
    else
        output_qual_name_prefix_c(Stream, ModuleName, !IO)
    ),
    mlds_output_proc_label(Stream, Name, !IO).

:- pred mlds_output_proc_label(io.text_output_stream::in, mlds_proc_label::in,
    io::di, io::uo) is det.

mlds_output_proc_label(Stream, mlds_proc_label(PredLabel, ProcId), !IO) :-
    proc_id_to_int(ProcId, ModeNum),
    io.format(Stream, "%s_%d",
        [s(mlds_pred_label_to_string(PredLabel)), i(ModeNum)], !IO).

:- func mlds_proc_label_to_string(mlds_proc_label) = string.

mlds_proc_label_to_string(mlds_proc_label(PredLabel, ProcId)) =
    mlds_pred_label_to_string(PredLabel) ++ "_"
        ++ string.int_to_string(proc_id_to_int(ProcId)).

%---------------------%

    % mlds_pred_label_to_string should be kept in sync with
    % browser/name_mangle.m.
    %
:- func mlds_pred_label_to_string(mlds_pred_label) = string.

mlds_pred_label_to_string(PredLabel) = Str :-
    (
        PredLabel = mlds_user_pred_label(PredOrFunc, MaybeDefiningModule,
            Name, PredArity, _CodeModel, _NonOutputFunc),
        (
            PredOrFunc = pf_predicate,
            Suffix = "p",
            UserArity = PredArity
        ;
            PredOrFunc = pf_function,
            Suffix = "f",
            UserArity = PredArity - 1
        ),
        MangledName = name_mangle(Name),
        (
            MaybeDefiningModule = yes(DefiningModule),
            Str = string.format("%s_%d_%s_in__%s",
                [s(MangledName), i(UserArity), s(Suffix),
                s(sym_name_mangle(DefiningModule))])
        ;
            MaybeDefiningModule = no,
            Str = string.format("%s_%d_%s",
                [s(MangledName), i(UserArity), s(Suffix)])
        )
    ;
        PredLabel = mlds_special_pred_label(PredName, MaybeTypeModule,
            TypeName, TypeArity),
        MangledPredName = name_mangle(PredName),
        MangledTypeName = name_mangle(TypeName),
        (
            MaybeTypeModule = yes(TypeModule),
            MangledTypeModule = sym_name_mangle(TypeModule),
            % XXX The old mlds_output_pred_label predicate, which used to be
            % called from mlds_output_function_name and mlds_output_proc_label
            % (calls which have been replaced by calls to this function)
            % used the equivalent of the format string "%s____%s%s_%d",
            % which was almost certainly a bug.
            %
            % However, since the MaybeTypeModule field is yes(_) *only* when
            % we are generating code in module A for a unify/compare/index
            % predicate for a type defined in module B where A != B,
            % that predicate *will* be private to module A, so we can call it
            % by any name we want in the C code we generate, provided it is
            % consistent.
            Str = string.format("%s__%s__%s_%d",
                [s(MangledPredName), s(MangledTypeModule),
                s(MangledTypeName), i(TypeArity)])
        ;
            MaybeTypeModule = no,
            Str = string.format("%s__%s_%d",
                [s(MangledPredName), s(MangledTypeName), i(TypeArity)])
        )
    ).

%---------------------------------------------------------------------------%

mlds_output_class_name_arity(Stream, ClassName, Arity, !IO) :-
    % XXX We should avoid appending the arity if it is not needed.
    MangledClassName = name_mangle(ClassName),
    io.format(Stream, "%s_%d", [s(MangledClassName), i(Arity)], !IO).

mlds_output_fully_qualified_field_var_name(Stream, QualFieldVarName, !IO) :-
    QualFieldVarName = qual_field_var_name(ModuleName, _, FieldVarName),
    output_qual_name_prefix_c(Stream, ModuleName, !IO),
    mlds_output_field_var_name(Stream, FieldVarName, !IO).

:- pred mlds_output_field_var_name(io.text_output_stream::in,
    mlds_field_var_name::in, io::di, io::uo) is det.

mlds_output_field_var_name(Stream, FieldVarName, !IO) :-
    MangledFieldVarName =
        name_mangle(ml_field_var_name_to_string(FieldVarName)),
    io.write_string(Stream, MangledFieldVarName, !IO).

%---------------------------------------------------------------------------%

mlds_output_local_var_name(Stream, LocalVarName, !IO) :-
    MangledLocalVarName =
        name_mangle(ml_local_var_name_to_string(LocalVarName)),
    io.write_string(Stream, MangledLocalVarName, !IO).

%---------------------------------------------------------------------------%

output_qual_name_prefix_c(Stream, ModuleName, !IO) :-
    SymName = mlds_module_name_to_sym_name(ModuleName),
    MangledModuleName = sym_name_mangle(SymName),
    io.format(Stream, "%s__", [s(MangledModuleName)], !IO).

qual_name_prefix_c(ModuleName) = ModuleNamePrefix :-
    SymName = mlds_module_name_to_sym_name(ModuleName),
    MangledModuleName = sym_name_mangle(SymName),
    ModuleNamePrefix = MangledModuleName ++ "__".

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_name.
%---------------------------------------------------------------------------%
