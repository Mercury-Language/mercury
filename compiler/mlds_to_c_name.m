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

%---------------------------------------------------------------------------%

:- func global_var_ref_to_string(global_var_ref) = string.

:- func tabling_struct_id_to_string(mlds_proc_label, proc_tabling_struct_id)
    = string.

%---------------------------------------------------------------------------%

:- func should_module_qualify_global_var_name(mlds_global_var_name) = bool.

:- func maybe_qual_global_var_name_to_string_for_c(mlds_module_name,
    mlds_global_var_name) = string.

%---------------------------------------------------------------------------%

:- func local_var_name_to_string_for_c(mlds_local_var_name) = string.

%---------------------------------------------------------------------------%

:- func field_var_name_to_string_for_c(mlds_field_var_name) = string.
:- func qual_field_var_name_to_string_for_c(qual_field_var_name) = string.

%---------------------------------------------------------------------------%

:- func class_name_arity_to_string_for_c(mlds_class_name, arity) = string.

%---------------------------------------------------------------------------%

:- func qual_function_name_to_string_for_c(qual_function_name) = string.

%---------------------------------------------------------------------------%

:- func qual_proc_label_to_string_for_c(qual_proc_label) = string.

%---------------------------------------------------------------------------%

:- func qualifier_to_string_for_c(mlds_module_name) = string.

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

% The calls to env_var_is_acceptable_char in parse_goal.m ensure that
% EnvVarName is acceptable as part of a C identifier.
% The prefix must be identical to envvar_prefix in util/mkinit.c
% and c_global_var_name in llds_out.m.
global_var_ref_to_string(env_var_ref(EnvVarName)) =
    "mercury_envvar_" ++ EnvVarName.

tabling_struct_id_to_string(ProcLabel, TablingId) = TablingName :-
    TablingIdStr = tabling_info_id_str(TablingId),
    TablingProcLabel = mlds_std_tabling_proc_label(ProcLabel),
    TablingProcLabelStr = proc_label_to_string_for_c(TablingProcLabel),
    string.format("%s_for_%s", [s(TablingIdStr), s(TablingProcLabelStr)],
        TablingName).

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

maybe_qual_global_var_name_to_string_for_c(ModuleName, GlobalVarName)
        = MaybeQualGlobalVarNameStr :-
    ShouldModuleQual = should_module_qualify_global_var_name(GlobalVarName),
    GlobalVarNameStr = global_var_name_to_string_for_c(GlobalVarName),
    (
        ShouldModuleQual = no,
        MaybeQualGlobalVarNameStr = GlobalVarNameStr
    ;
        ShouldModuleQual = yes,
        Qualifier = qualifier_to_string_for_c(ModuleName),
        string.format("%s__%s", [s(Qualifier), s(GlobalVarNameStr)],
            MaybeQualGlobalVarNameStr)
    ).

:- func global_var_name_to_string_for_c(mlds_global_var_name) = string.

global_var_name_to_string_for_c(GlobalVarName) = GlobalVarNameStr :-
    (
        GlobalVarName = gvn_const_var(ConstVar, Num),
        GlobalVarNameStr =
            name_mangle(ml_global_const_var_name_to_string(ConstVar, Num))
    ;
        GlobalVarName = gvn_rtti_var(RttiId),
        rtti.id_to_c_identifier(RttiId, RttiAddrName),
        GlobalVarNameStr = RttiAddrName
    ;
        GlobalVarName = gvn_tabling_var(ProcLabel, TablingId),
        GlobalVarNameStr = tabling_struct_id_to_string(ProcLabel, TablingId)
    ;
        GlobalVarName = gvn_dummy_var,
        GlobalVarNameStr = "dummy_var"
    ).

%---------------------------------------------------------------------------%

local_var_name_to_string_for_c(LocalVarName) = LocalVarNameStr :-
    LocalVarNameStr = name_mangle(ml_local_var_name_to_string(LocalVarName)).

%---------------------------------------------------------------------------%

field_var_name_to_string_for_c(FieldVarName) = FieldVarNameStr :-
    % ZZZ is ml_field_var_name_to_string etc used by non-c backends?
    FieldVarNameStr = name_mangle(ml_field_var_name_to_string(FieldVarName)).

qual_field_var_name_to_string_for_c(QualFieldVarName) = QualFieldVarNameStr :-
    QualFieldVarName = qual_field_var_name(ModuleName, _, FieldVarName),
    Qualifier = qualifier_to_string_for_c(ModuleName),
    FieldVarNameStr = field_var_name_to_string_for_c(FieldVarName),
    string.format("%s__%s", [s(Qualifier), s(FieldVarNameStr)],
        QualFieldVarNameStr).

%---------------------------------------------------------------------------%

class_name_arity_to_string_for_c(ClassName, Arity) = ClassNameStr :-
    % XXX We should avoid appending the arity if it is not needed.
    MangledClassName = name_mangle(ClassName),
    string.format("%s_%d", [s(MangledClassName), i(Arity)], ClassNameStr).

%---------------------------------------------------------------------------%

qual_function_name_to_string_for_c(QualFuncName) = QualFuncNameStr :-
    QualFuncName = qual_function_name(ModuleName, FuncName),
    FuncNameStr = function_name_to_string_for_c(FuncName),
    ( if
        (
            % Do not module-qualify main/2.
            FuncName = mlds_function_name(PlainFuncName),
            PlainFuncName = mlds_plain_func_name(FuncLabel, _),
            FuncLabel = mlds_func_label(ProcLabel, _MaybeSeqNum),
            ProcLabel = mlds_proc_label(PredLabel, _ProcId),
            PredLabel = mlds_user_pred_label(pf_predicate, no, "main",
                pred_form_arity(2), model_det, no)
        ;
            % We do not module qualify pragma foreign_export names.
            FuncName = mlds_function_export(_)
        )
    then
        QualFuncNameStr = FuncNameStr
    else
        Qualifier = qualifier_to_string_for_c(ModuleName),
        string.format("%s__%s", [s(Qualifier), s(FuncNameStr)],
            QualFuncNameStr)
    ).

:- func function_name_to_string_for_c(mlds_function_name) = string.

function_name_to_string_for_c(FuncName) = FuncNameStr :-
    % XXX We should avoid appending the modenum and seqnum
    % if they are not needed.
    (
        FuncName = mlds_function_name(PlainFuncName),
        PlainFuncName = mlds_plain_func_name(FuncLabel, _PredId),
        FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
        ProcLabel = mlds_proc_label(PredLabel, ProcId),
        PredLabelStr = pred_label_to_string_for_c(PredLabel),
        proc_id_to_int(ProcId, ModeNum),
        FuncIdSuffix = mlds_maybe_aux_func_id_to_suffix(MaybeAux),
        string.format("%s_%d%s",
            [s(PredLabelStr), i(ModeNum), s(FuncIdSuffix)], FuncNameStr)
    ;
        FuncName = mlds_function_export(FuncNameStr)
    ).

%---------------------%

qual_proc_label_to_string_for_c(QualProcLabel) = QualProcLabelStr :-
    QualProcLabel = qual_proc_label(ModuleName, ProcLabel),
    ProcLabel = mlds_proc_label(PredLabel, _ProcId),
    ProcLabelStr = proc_label_to_string_for_c(ProcLabel),
    ( if
        % Do not module-qualify main/2.
        PredLabel = mlds_user_pred_label(pf_predicate, no, "main",
            pred_form_arity(2), model_det, no)
    then
        QualProcLabelStr = ProcLabelStr
    else
        Qualifier = qualifier_to_string_for_c(ModuleName),
        string.format("%s__%s", [s(Qualifier), s(ProcLabelStr)],
            QualProcLabelStr)
    ).

%---------------------%

:- func proc_label_to_string_for_c(mlds_proc_label) = string.

proc_label_to_string_for_c(ProcLabel) = ProcLabelStr :-
    ProcLabel = mlds_proc_label(PredLabel, ProcId),
    PredLabelStr = pred_label_to_string_for_c(PredLabel),
    proc_id_to_int(ProcId, ModeNum),
    string.format("%s_%d", [s(PredLabelStr), i(ModeNum)], ProcLabelStr).

    % mlds_pred_label_to_string should be kept in sync with
    % browser/name_mangle.m.
    %
:- func pred_label_to_string_for_c(mlds_pred_label) = string.

pred_label_to_string_for_c(PredLabel) = Str :-
    (
        PredLabel = mlds_user_pred_label(PredOrFunc, MaybeDefiningModule,
            Name, PredFormArity, _CodeModel, _NonOutputFunc),
        PredFormArity = pred_form_arity(PredFormArityInt),
        (
            PredOrFunc = pf_predicate,
            Suffix = "p",
            UserArityInt = PredFormArityInt
        ;
            PredOrFunc = pf_function,
            Suffix = "f",
            UserArityInt = PredFormArityInt - 1
        ),
        MangledName = name_mangle(Name),
        (
            MaybeDefiningModule = yes(DefiningModule),
            Str = string.format("%s_%d_%s_in__%s",
                [s(MangledName), i(UserArityInt), s(Suffix),
                s(sym_name_mangle(DefiningModule))])
        ;
            MaybeDefiningModule = no,
            Str = string.format("%s_%d_%s",
                [s(MangledName), i(UserArityInt), s(Suffix)])
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

qualifier_to_string_for_c(ModuleName) = Qualifier :-
    SymName = mlds_module_name_to_sym_name(ModuleName),
    Qualifier = sym_name_mangle(SymName).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_name.
%---------------------------------------------------------------------------%
