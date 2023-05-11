%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010-2012 The University of Melbourne.
% Copyright (C) 2013-2018, 2020 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output the names of various entities.
%
% XXX Much of this code will not work when we start enforcing names properly.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_cs_name.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_cs_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module io.

%---------------------------------------------------------------------------%

:- func strip_mercury_and_mangle_sym_name_for_csharp(sym_name) = string.

%---------------------------------------------------------------------------%

    % Although the C# spec does not limit identifier lengths, the Microsoft
    % compiler restricts identifiers to 511 characters and Mono restricts
    % identifiers to 512 characters.
    % This assumes the identifier contains only ASCII characters.
:- func limit_identifier_length(string) = string.
    % ZZZ
:- pred write_identifier_string_for_csharp(io.text_output_stream::in,
    string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- func global_var_name_to_nll_string_for_csharp(mlds_global_var_name)
    = string.
:- func global_var_name_to_ll_string_for_csharp(mlds_global_var_name) = string.
:- pred output_global_var_name_for_csharp(io.text_output_stream::in,
    mlds_global_var_name::in, io::di, io::uo) is det.

/*
ZZZ
:- func maybe_qualified_global_var_name_to_string_for_csharp(csharp_out_info,
    qual_global_var_name) = string.
*/
:- pred output_maybe_qualified_global_var_name_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, qual_global_var_name::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- func local_var_name_to_nll_string_for_csharp(mlds_local_var_name) = string.
:- func local_var_name_to_ll_string_for_csharp(mlds_local_var_name) = string.
:- pred output_local_var_name_for_csharp(io.text_output_stream::in,
    mlds_local_var_name::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- func field_var_name_to_nll_string_for_csharp(mlds_field_var_name) = string.
:- func field_var_name_to_ll_string_for_csharp(mlds_field_var_name) = string.
:- pred output_field_var_name_for_csharp(io.text_output_stream::in,
    mlds_field_var_name::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- func unqual_class_name_to_nll_string_for_csharp(mlds_class_name, arity)
    = string.
:- func unqual_class_name_to_ll_string_for_csharp(mlds_class_name, arity)
    = string.
:- pred output_unqual_class_name_for_csharp(io.text_output_stream::in,
    mlds_class_name::in, arity::in, io::di, io::uo) is det.

:- func qual_class_name_to_nll_string_for_csharp(qual_class_name, arity)
    = string.
:- func qual_class_name_to_ll_string_for_csharp(qual_class_name, arity)
    = string.

:- pred output_class_name_arity_for_csharp(io.text_output_stream::in,
    mlds_class_name::in, arity::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- func function_name_to_nll_string_for_csharp(mlds_function_name) = string.
:- func function_name_to_ll_string_for_csharp(mlds_function_name) = string.
:- pred output_function_name_for_csharp(io.text_output_stream::in,
    mlds_function_name::in, io::di, io::uo) is det.

:- pred output_maybe_qualified_function_name_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, qual_function_name::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- func proc_label_to_nll_string_for_csharp(string, mlds_proc_label) = string.
:- func proc_label_to_ll_string_for_csharp(string, mlds_proc_label) = string.
:- pred output_proc_label_for_csharp(io.text_output_stream::in, string::in,
    mlds_proc_label::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- func qualifier_to_string_for_csharp(mlds_module_name, mlds_qual_kind)
    = string.
:- pred output_qual_name_prefix_cs(io.text_output_stream::in,
    mlds_module_name::in, mlds_qual_kind::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.            % for pred_proc_id
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_foreign.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

strip_mercury_and_mangle_sym_name_for_csharp(SymName0) = MangledSymName :-
    % XXX could use global::mercury. instead of stripping it
    ( if strip_outermost_qualifier(SymName0, "mercury", StrippedSymName0) then
        SymName = StrippedSymName0
    else
        SymName = SymName0
    ),
    mangle_sym_name_for_csharp(SymName, module_qual, "__", MangledSymName).

%---------------------------------------------------------------------------%

limit_identifier_length(Str0) = Str :-
    Length = string.length(Str0),
    ( if Length > 511 then
        Left = string.left(Str0, 251),
        Middle = c_util.hex_hash32(Str0),
        Right = string.right(Str0, 250),
        string.format("%s_%s_%s", [s(Left), s(Middle), s(Right)], Str)
    else
        Str = Str0
    ).

write_identifier_string_for_csharp(Stream, Str0, !IO) :-
    io.write_string(Stream, limit_identifier_length(Str0), !IO).

%---------------------------------------------------------------------------%

global_var_name_to_nll_string_for_csharp(GlobalVarName) = GlobalVarNameStr :-
    (
        GlobalVarName = gvn_const_var(ConstVar, Num),
        GlobalVarNameStr = ml_global_const_var_name_to_string(ConstVar, Num)
    ;
        GlobalVarName = gvn_rtti_var(RttiId),
        rtti.id_to_c_identifier(RttiId, RttiAddrName),
        GlobalVarNameStr = RttiAddrName
    ;
        GlobalVarName = gvn_tabling_var(_ProcLabel, _Id),
        unexpected($pred, "NYI: gvn_tabling_ref")
    ;
        GlobalVarName = gvn_dummy_var,
        GlobalVarNameStr = "dummy_var"
    ).

global_var_name_to_ll_string_for_csharp(GlobalVarName) = GlobalVarNameStr :-
    GlobalVarNameStr0
        = global_var_name_to_nll_string_for_csharp(GlobalVarName),
    GlobalVarNameStr = limit_identifier_length(GlobalVarNameStr0).

output_global_var_name_for_csharp(Stream, GlobalVarName, !IO) :-
    GlobalVarNameStr = global_var_name_to_ll_string_for_csharp(GlobalVarName),
    io.write_string(Stream, GlobalVarNameStr, !IO).

/*
maybe_qualified_global_var_name_to_string_for_csharp(Info, QualGlobalVarName)
        = MaybeQualGlobalVarNameStr :-
    QualGlobalVarName = qual_global_var_name(GlobalVarModule, GlobalVarName),
    % ZZZ what is length limited
    GlobalVarNameStr = global_var_name_to_string_for_csharp(GlobalVarName),
    % Don't module qualify names which are defined in the current module.
    % This avoids unnecessary verbosity.
    CurrentModule = Info ^ csoi_module_name,
    ( if GlobalVarModule = CurrentModule then
        MaybeQualGlobalVarNameStr = GlobalVarNameStr
    else
        output_qual_name_prefix_cs(Stream, ModuleName, module_qual, !IO),
        string.format("%s.%s", [s(QualStr), s(GlobalVarNameStr)],
            MaybeQualGlobalVarNameStr)
    ),
    output_global_var_name_for_csharp(Stream, GlobalVarName, !IO).
*/

output_maybe_qualified_global_var_name_for_csharp(Info, Stream,
        QualGlobalVarName, !IO) :-
    % Don't module qualify names which are defined in the current module.
    % This avoids unnecessary verbosity.
    QualGlobalVarName = qual_global_var_name(ModuleName, GlobalVarName),
    CurrentModuleName = Info ^ csoi_module_name,
    ( if ModuleName = CurrentModuleName then
        true
    else
        output_qual_name_prefix_cs(Stream, ModuleName, module_qual, !IO)
    ),
    output_global_var_name_for_csharp(Stream, GlobalVarName, !IO).

%---------------------------------------------------------------------------%

local_var_name_to_nll_string_for_csharp(LocalVarName) = LocalVarNameStr :-
    RawString = ml_local_var_name_to_string(LocalVarName),
    MangledString = name_mangle(RawString),
    LocalVarNameStr = make_valid_csharp_symbol_name(MangledString).

local_var_name_to_ll_string_for_csharp(LocalVarName) = LocalVarNameStr :-
    LocalVarNameStr0 = local_var_name_to_nll_string_for_csharp(LocalVarName),
    LocalVarNameStr = limit_identifier_length(LocalVarNameStr0).

output_local_var_name_for_csharp(Stream, LocalVarName, !IO) :-
    LocalVarNameStr = local_var_name_to_ll_string_for_csharp(LocalVarName),
    io.write_string(Stream, LocalVarNameStr, !IO).

%---------------------------------------------------------------------------%

field_var_name_to_nll_string_for_csharp(FieldVarName) = FieldVarNameStr :-
    RawString = ml_field_var_name_to_string(FieldVarName),
    MangledString = name_mangle(RawString),
    FieldVarNameStr = make_valid_csharp_symbol_name(MangledString).

field_var_name_to_ll_string_for_csharp(FieldVarName) = FieldVarNameStr :-
    FieldVarNameStr0 = field_var_name_to_nll_string_for_csharp(FieldVarName),
    FieldVarNameStr = limit_identifier_length(FieldVarNameStr0).

output_field_var_name_for_csharp(Stream, FieldVarName, !IO) :-
    FieldVarNameStr = field_var_name_to_ll_string_for_csharp(FieldVarName),
    io.write_string(Stream, FieldVarNameStr, !IO).

%---------------------------------------------------------------------------%

unqual_class_name_to_nll_string_for_csharp(Name, Arity) = ClassNameStr :-
    MangledName = name_mangle_no_leading_digit(Name),
    % By convention, class names should start with a capital letter.
    UppercaseMangledName = flip_initial_case(MangledName),
    string.format("%s_%d", [s(UppercaseMangledName), i(Arity)], ClassNameStr).

unqual_class_name_to_ll_string_for_csharp(Name, Arity) = ClassNameStr :-
    ClassNameStr0 = unqual_class_name_to_nll_string_for_csharp(Name, Arity),
    ClassNameStr = limit_identifier_length(ClassNameStr0).

output_unqual_class_name_for_csharp(Stream, Name, Arity, !IO) :-
    ClassNameStr0 = unqual_class_name_to_nll_string_for_csharp(Name, Arity),
    ClassNameStr = limit_identifier_length(ClassNameStr0),
    io.write_string(Stream, ClassNameStr, !IO).

qual_class_name_to_nll_string_for_csharp(QualName, Arity) = QualClassNameStr :-
    QualName = qual_class_name(MLDS_ModuleName, QualKind, ClassName),
    ( if
        SymName = mlds_module_name_to_sym_name(MLDS_ModuleName),
        SymName = csharp_mercury_runtime_package_name
    then
        % Don't mangle runtime class names.
        string.format("runtime.%s", [s(ClassName)], QualClassNameStr)
    else
        % XXX maybe duplicated code
        Qualifier = qualifier_to_string_for_csharp(MLDS_ModuleName, QualKind),
        UnqualClassNameStr =
            unqual_class_name_to_nll_string_for_csharp(ClassName, Arity),
        string.format("%s.%s", [s(Qualifier), s(UnqualClassNameStr)],
            QualClassNameStr)
    ).

qual_class_name_to_ll_string_for_csharp(QualName, Arity) = QualClassNameStr :-
    QualClassNameStr0 =
        qual_class_name_to_nll_string_for_csharp(QualName, Arity),
    QualClassNameStr = limit_identifier_length(QualClassNameStr0).

% ZZZ ll
output_class_name_arity_for_csharp(Stream, ClassName, ClassArity, !IO) :-
    ClassNameStr0 =
        unqual_class_name_to_nll_string_for_csharp(ClassName, ClassArity),
    ClassNameStr = limit_identifier_length(ClassNameStr0),
    io.write_string(Stream, ClassNameStr, !IO).

%---------------------------------------------------------------------------%

function_name_to_nll_string_for_csharp(FunctionName) = FunctionNameStr :-
    (
        FunctionName = mlds_function_name(PlainFuncName),
        PlainFuncName = mlds_plain_func_name(FuncLabel, _PredId),
        FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
        ProcLabel = mlds_proc_label(PredLabel, ProcId),
        PredLabelStr = pred_label_to_string_for_csharp(PredLabel),
        proc_id_to_int(ProcId, ModeNum),
        MaybeAuxSuffix = mlds_maybe_aux_func_id_to_suffix(MaybeAux),
        string.format("%s_%d%s",
            [s(PredLabelStr), i(ModeNum), s(MaybeAuxSuffix)], FunctionNameStr)
    ;
        FunctionName = mlds_function_export(FunctionNameStr)
    ).

function_name_to_ll_string_for_csharp(FunctionName) = FunctionNameStr :-
    FunctionNameStr0 = function_name_to_nll_string_for_csharp(FunctionName),
    FunctionNameStr = limit_identifier_length(FunctionNameStr0).

output_function_name_for_csharp(Stream, FunctionName, !IO) :-
    FunctionNameStr = function_name_to_ll_string_for_csharp(FunctionName),
    io.write_string(Stream, FunctionNameStr, !IO).

output_maybe_qualified_function_name_for_csharp(Info, Stream,
        QualFuncName, !IO) :-
    % Don't module qualify names which are defined in the current module.
    % This avoids unnecessary verbosity.
    QualFuncName = qual_function_name(ModuleName, FuncName),
    CurrentModuleName = Info ^ csoi_module_name,
    ( if ModuleName = CurrentModuleName then
        true
    else
        output_qual_name_prefix_cs(Stream, ModuleName, module_qual, !IO)
    ),
    output_function_name_for_csharp(Stream, FuncName, !IO).

%---------------------------------------------------------------------------%

:- func pred_label_to_string_for_csharp(mlds_pred_label) = string.

pred_label_to_string_for_csharp(PredLabel) = PredLabelStr :-
    (
        PredLabel = mlds_user_pred_label(PredOrFunc, MaybeDefiningModule,
            Name, PredFormArity, _, _),
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
        MangledName = name_mangle_no_leading_digit(Name),
        (
            MaybeDefiningModule = yes(DefiningModule),
            DefiningModuleStr = sym_name_mangle(DefiningModule),
            string.format("%s_%d_%s_in__%s",
                [s(MangledName), i(UserArityInt), s(Suffix),
                    s(DefiningModuleStr)],
                PredLabelStr)
        ;
            MaybeDefiningModule = no,
            string.format("%s_%d_%s",
                [s(MangledName), i(UserArityInt), s(Suffix)], PredLabelStr)
        )
    ;
        PredLabel = mlds_special_pred_label(PredName, MaybeTypeModule,
            TypeName, TypeArity),
        MangledPredName = name_mangle_no_leading_digit(PredName),
        MangledTypeName = name_mangle(TypeName),
        (
            MaybeTypeModule = yes(TypeModule),
            TypeModuleStr = sym_name_mangle(TypeModule),
            string.format("%s__%s__%s_%d",
                [s(TypeModuleStr), s(MangledPredName), s(MangledTypeName),
                    i(TypeArity)],
                PredLabelStr)
        ;
            MaybeTypeModule = no,
            string.format("%s__%s_%d",
                [s(MangledPredName), s(MangledTypeName), i(TypeArity)],
                PredLabelStr)
        )
    ).

%---------------------------------------------------------------------------%

proc_label_to_nll_string_for_csharp(Suffix, ProcLabel) = ProcLabelStr :-
    ProcLabel = mlds_proc_label(PredLabel, ProcId),
    PredLabelStr = pred_label_to_string_for_csharp(PredLabel),
    proc_id_to_int(ProcId, ModeNum),
    string.format("%s_%d%s", [s(PredLabelStr), i(ModeNum), s(Suffix)],
        ProcLabelStr).

proc_label_to_ll_string_for_csharp(Suffix, ProcLabel) = ProcLabelStr :-
    ProcLabelStr0 = proc_label_to_nll_string_for_csharp(Suffix, ProcLabel),
    ProcLabelStr = limit_identifier_length(ProcLabelStr0).

output_proc_label_for_csharp(Stream, Suffix, ProcLabel, !IO) :-
    ProcLabelStr = proc_label_to_ll_string_for_csharp(Suffix, ProcLabel),
    io.write_string(Stream, ProcLabelStr, !IO).

%---------------------------------------------------------------------------%

qualifier_to_string_for_csharp(MLDS_ModuleName, QualKind) = Str :-
    mlds_module_name_to_package_name(MLDS_ModuleName) = OuterName,
    mlds_module_name_to_sym_name(MLDS_ModuleName) = InnerName,

    % The part of the qualifier that corresponds to a top-level class.
    % Remove the outermost mercury qualifier.
    MangledOuterName = strip_mercury_and_mangle_sym_name_for_csharp(OuterName),

    % The later parts of the qualifier correspond to nested classes.
    ( if OuterName = InnerName then
        Str = MangledOuterName
    else
        remove_sym_name_prefix(InnerName, OuterName, Suffix),
        mangle_sym_name_for_csharp(Suffix, convert_qual_kind(QualKind), ".",
            MangledSuffix),
        string.format("%s.%s", [s(MangledOuterName), s(MangledSuffix)], Str)
    ).

output_qual_name_prefix_cs(Stream, ModuleName, QualKind, !IO) :-
    Qualifier = qualifier_to_string_for_csharp(ModuleName, QualKind),
    % ZZZ limit_identifier_length
    io.write_string(Stream, Qualifier, !IO),
    io.write_string(Stream, ".", !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_name.
%---------------------------------------------------------------------------%
