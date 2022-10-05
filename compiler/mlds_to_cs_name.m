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

:- pred output_maybe_qualified_global_var_name_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, qual_global_var_name::in,
    io::di, io::uo) is det.

:- pred output_global_var_name_for_csharp(io.text_output_stream::in,
    mlds_global_var_name::in, io::di, io::uo) is det.

:- pred global_var_name_to_string_for_csharp(mlds_global_var_name::in,
    string::out) is det.

%---------------------------------------------------------------------------%

:- pred output_maybe_qualified_function_name_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, qual_function_name::in, io::di, io::uo) is det.

:- pred output_function_name_for_csharp(io.text_output_stream::in,
    mlds_function_name::in, io::di, io::uo) is det.

:- pred mlds_output_proc_label(io.text_output_stream::in, string::in,
    mlds_proc_label::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred qual_class_name_to_string_for_csharp(qual_class_name::in, arity::in,
    string::out) is det.

:- pred output_unqual_class_name_for_csharp(io.text_output_stream::in,
    mlds_class_name::in, arity::in, io::di, io::uo) is det.

:- pred output_class_name_arity_for_csharp(io.text_output_stream::in,
    mlds_class_name::in, arity::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_field_var_name_for_csharp(io.text_output_stream::in,
    mlds_field_var_name::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_local_var_name_for_csharp(io.text_output_stream::in,
    mlds_local_var_name::in, io::di, io::uo) is det.

:- pred local_var_name_to_string_for_csharp(mlds_local_var_name::in,
    string::out) is det.

%---------------------------------------------------------------------------%

:- pred output_qual_name_prefix_cs(io.text_output_stream::in,
    mlds_module_name::in, mlds_qual_kind::in, io::di, io::uo) is det.

:- func strip_mercury_and_mangle_sym_name_for_csharp(sym_name) = string.

%---------------------------------------------------------------------------%

:- pred write_identifier_string_for_csharp(io.text_output_stream::in,
    string::in, io::di, io::uo) is det.

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

output_global_var_name_for_csharp(Stream, DataName, !IO) :-
    global_var_name_to_string_for_csharp(DataName, DataNameStr),
    write_identifier_string_for_csharp(Stream, DataNameStr, !IO).

global_var_name_to_string_for_csharp(GlobalVarName, String) :-
    (
        GlobalVarName = gvn_const_var(ConstVar, Num),
        String = ml_global_const_var_name_to_string(ConstVar, Num)
    ;
        GlobalVarName = gvn_rtti_var(RttiId),
        rtti.id_to_c_identifier(RttiId, RttiAddrName),
        String = RttiAddrName
    ;
        GlobalVarName = gvn_tabling_var(_ProcLabel, _Id),
        unexpected($pred, "NYI: gvn_tabling_ref")
    ;
        GlobalVarName = gvn_dummy_var,
        String = "dummy_var"
    ).

%---------------------------------------------------------------------------%

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

output_function_name_for_csharp(Stream, FunctionName, !IO) :-
    function_name_to_string_for_csharp(FunctionName, FunctionNameStr),
    write_identifier_string_for_csharp(Stream, FunctionNameStr, !IO).

:- pred function_name_to_string_for_csharp(mlds_function_name::in, string::out)
    is det.

function_name_to_string_for_csharp(FunctionName, String) :-
    (
        FunctionName = mlds_function_name(PlainFuncName),
        PlainFuncName = mlds_plain_func_name(FuncLabel, _PredId),
        FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
        ProcLabel = mlds_proc_label(PredLabel, ProcId),
        pred_label_to_string_for_csharp(PredLabel, PredLabelStr),
        proc_id_to_int(ProcId, ModeNum),
        MaybeAuxSuffix = mlds_maybe_aux_func_id_to_suffix(MaybeAux),
        string.format("%s_%d%s",
            [s(PredLabelStr), i(ModeNum), s(MaybeAuxSuffix)], String)
    ;
        FunctionName = mlds_function_export(Name),
        String = Name
    ).

:- pred pred_label_to_string_for_csharp(mlds_pred_label::in, string::out)
    is det.

pred_label_to_string_for_csharp(PredLabel, String) :-
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
                String)
        ;
            MaybeDefiningModule = no,
            string.format("%s_%d_%s",
                [s(MangledName), i(UserArityInt), s(Suffix)], String)
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
                String)
        ;
            MaybeTypeModule = no,
            string.format("%s__%s_%d",
                [s(MangledPredName), s(MangledTypeName), i(TypeArity)],
                String)
        )
    ).

mlds_output_proc_label(Stream, Suffix, mlds_proc_label(PredLabel, ProcId),
        !IO) :-
    pred_label_to_string_for_csharp(PredLabel, PredLabelStr),
    proc_id_to_int(ProcId, ModeNum),
    string.format("%s_%d%s", [s(PredLabelStr), i(ModeNum), s(Suffix)], String),
    write_identifier_string_for_csharp(Stream, String, !IO).

%---------------------------------------------------------------------------%

qual_class_name_to_string_for_csharp(QualName, Arity, String) :-
    QualName = qual_class_name(MLDS_ModuleName, QualKind, ClassName),
    ( if
        SymName = mlds_module_name_to_sym_name(MLDS_ModuleName),
        SymName = csharp_mercury_runtime_package_name
    then
        % Don't mangle runtime class names.
        String = "runtime." ++ ClassName
    else
        % XXX maybe duplicated code
        qualifier_to_string_for_csharp(MLDS_ModuleName, QualKind, QualString),
        unqual_class_name_to_string_for_csharp(ClassName, Arity, UnqualString),
        String = QualString ++ "." ++ UnqualString
    ).

output_unqual_class_name_for_csharp(Stream, Name, Arity, !IO) :-
    unqual_class_name_to_string_for_csharp(Name, Arity, String),
    write_identifier_string_for_csharp(Stream, String, !IO).

:- pred unqual_class_name_to_string_for_csharp(mlds_class_name::in, arity::in,
    string::out) is det.

unqual_class_name_to_string_for_csharp(Name, Arity, String) :-
    MangledName = name_mangle_no_leading_digit(Name),
    % By convention, class names should start with a capital letter.
    UppercaseMangledName = flip_initial_case(MangledName),
    String = UppercaseMangledName ++ "_" ++ string.from_int(Arity).

output_class_name_arity_for_csharp(Stream, ClassName, ClassArity, !IO) :-
    class_name_arity_to_string_for_csharp(ClassName, ClassArity, Str),
    write_identifier_string_for_csharp(Stream, Str, !IO).

:- pred class_name_arity_to_string_for_csharp(mlds_class_name::in, arity::in,
    string::out) is det.

class_name_arity_to_string_for_csharp(ClassName, ClassArity, String) :-
    unqual_class_name_to_string_for_csharp(ClassName, ClassArity, String).

%---------------------------------------------------------------------------%

output_field_var_name_for_csharp(Stream, VarName, !IO) :-
    field_var_name_to_string_for_csharp(VarName, VarNameStr),
    write_identifier_string_for_csharp(Stream, VarNameStr, !IO).

:- pred field_var_name_to_string_for_csharp(mlds_field_var_name::in,
    string::out) is det.

field_var_name_to_string_for_csharp(LocalVarName, String) :-
    RawString = ml_field_var_name_to_string(LocalVarName),
    MangledString = name_mangle(RawString),
    String = make_valid_csharp_symbol_name(MangledString).

%---------------------------------------------------------------------------%

output_local_var_name_for_csharp(Stream, VarName, !IO) :-
    local_var_name_to_string_for_csharp(VarName, VarNameStr),
    write_identifier_string_for_csharp(Stream, VarNameStr, !IO).

local_var_name_to_string_for_csharp(LocalVarName, String) :-
    RawString = ml_local_var_name_to_string(LocalVarName),
    MangledString = name_mangle(RawString),
    String = make_valid_csharp_symbol_name(MangledString).

%---------------------------------------------------------------------------%

output_qual_name_prefix_cs(Stream, ModuleName, QualKind, !IO) :-
    qualifier_to_string_for_csharp(ModuleName, QualKind, QualifierString),
    io.write_string(Stream, QualifierString, !IO),
    io.write_string(Stream, ".", !IO).

:- pred qualifier_to_string_for_csharp(mlds_module_name::in,
    mlds_qual_kind::in, string::out) is det.

qualifier_to_string_for_csharp(MLDS_ModuleName, QualKind, String) :-
    mlds_module_name_to_package_name(MLDS_ModuleName) = OuterName,
    mlds_module_name_to_sym_name(MLDS_ModuleName) = InnerName,

    % The part of the qualifier that corresponds to a top-level class.
    % Remove the outermost mercury qualifier.
    MangledOuterName = strip_mercury_and_mangle_sym_name_for_csharp(OuterName),

    % The later parts of the qualifier correspond to nested classes.
    ( if OuterName = InnerName then
        MangledSuffix = ""
    else
        remove_sym_name_prefix(InnerName, OuterName, Suffix),
        mangle_sym_name_for_csharp(Suffix, convert_qual_kind(QualKind), ".",
            MangledSuffix0),
        MangledSuffix = "." ++ MangledSuffix0
    ),

    String = MangledOuterName ++ MangledSuffix.

strip_mercury_and_mangle_sym_name_for_csharp(SymName) = MangledSymName :-
    % XXX could use global::mercury. instead of stripping it
    ( if strip_outermost_qualifier(SymName, "mercury", StrippedSymName) then
        mangle_sym_name_for_csharp(StrippedSymName, module_qual, "__",
            MangledSymName)
    else
        mangle_sym_name_for_csharp(SymName, module_qual, "__",
            MangledSymName)
    ).

%---------------------------------------------------------------------------%

write_identifier_string_for_csharp(Stream, String, !IO) :-
    % Although the C# spec does not limit identifier lengths, the Microsoft
    % compiler restricts identifiers to 511 characters and Mono restricts
    % identifiers to 512 characters.
    % This assumes the identifier contains only ASCII characters.
    Length = string.length(String),
    ( if Length > 511 then
        Left = string.left(String, 251),
        Middle = c_util.hex_hash32(String),
        Right = string.right(String, 250),
        io.format(Stream, "%s_%s_%s", [s(Left), s(Middle), s(Right)], !IO)
    else
        io.write_string(Stream, String, !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_name.
%---------------------------------------------------------------------------%
