%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output the names of various entities.
%
% XXX Much of this code will not work when we start enforcing names properly.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_java_name.
:- interface.

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_java_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred output_maybe_qualified_global_var_name_for_java(java_out_info::in,
    io.text_output_stream::in, qual_global_var_name::in,
    io::di, io::uo) is det.

:- pred output_global_var_name_for_java(io.text_output_stream::in,
    mlds_global_var_name::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_maybe_qualified_function_name_for_java(java_out_info::in,
    io.text_output_stream::in, qual_function_name::in, io::di, io::uo) is det.

:- pred output_function_name_for_java(io.text_output_stream::in,
    mlds_function_name::in, io::di, io::uo) is det.

:- pred mlds_output_proc_label_for_java(io.text_output_stream::in,
    mlds_proc_label::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred qual_class_name_to_string_for_java(qual_class_name::in, arity::in,
    string::out) is det.

:- pred output_unqual_class_name_for_java(io.text_output_stream::in,
    mlds_class_name::in, arity::in, io::di, io::uo) is det.

:- pred output_class_name_arity_for_java(io.text_output_stream::in,
    mlds_class_name::in, arity::in, io::di, io::uo) is det.

:- pred output_field_var_name_for_java(io.text_output_stream::in,
    mlds_field_var_name::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_local_var_name_for_java(io.text_output_stream::in,
    mlds_local_var_name::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred output_qual_name_prefix_java(io.text_output_stream::in,
    mlds_module_name::in, mlds_qual_kind::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.           % for pred_proc_id.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_foreign.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

output_maybe_qualified_global_var_name_for_java(Info, Stream,
    QualGlobalVarName, !IO) :-
    % Don't module qualify names which are defined in the current module.
    % This avoids unnecessary verbosity.
    QualGlobalVarName = qual_global_var_name(ModuleName, GlobalVarName),
    CurrentModuleName = Info ^ joi_module_name,
    ( if ModuleName = CurrentModuleName then
        true
    else
        output_qual_name_prefix_java(Stream, ModuleName, module_qual, !IO)
    ),
    output_global_var_name_for_java(Stream, GlobalVarName, !IO).

output_global_var_name_for_java(Stream, GlobalVarName, !IO) :-
    (
        GlobalVarName = gvn_const_var(ConstVar, Num),
        NameStr = ml_global_const_var_name_to_string(ConstVar, Num),
        output_valid_mangled_name_for_java(Stream, NameStr, !IO)
    ;
        GlobalVarName = gvn_rtti_var(RttiId),
        rtti.id_to_c_identifier(RttiId, RttiAddrName),
        io.write_string(Stream, RttiAddrName, !IO)
    ;
        GlobalVarName = gvn_tabling_var(ProcLabel, Id),
        Prefix = tabling_info_id_str(Id) ++ "_",
        io.write_string(Stream, Prefix, !IO),
        mlds_output_proc_label_for_java(Stream,
            mlds_std_tabling_proc_label(ProcLabel), !IO)
    ;
        GlobalVarName = gvn_dummy_var,
        io.write_string(Stream, "dummy_var", !IO)
    ).

%---------------------------------------------------------------------------%

output_maybe_qualified_function_name_for_java(Info, Stream, QualFuncName,
        !IO) :-
    % Don't module qualify names which are defined in the current module.
    % This avoids unnecessary verbosity.
    QualFuncName = qual_function_name(ModuleName, FuncName),
    CurrentModuleName = Info ^ joi_module_name,
    ( if ModuleName = CurrentModuleName then
        true
    else
        output_qual_name_prefix_java(Stream, ModuleName, module_qual, !IO)
    ),
    output_function_name_for_java(Stream, FuncName, !IO).

output_function_name_for_java(Stream, FunctionName, !IO) :-
    (
        FunctionName = mlds_function_name(PlainFuncName),
        PlainFuncName = mlds_plain_func_name(FuncLabel, _PredId),
        FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
        ProcLabel = mlds_proc_label(PredLabel, ProcId),
        output_pred_label_for_java(Stream, PredLabel, !IO),
        proc_id_to_int(ProcId, ModeNum),
        io.format(Stream, "_%d", [i(ModeNum)], !IO),
        io.write_string(Stream,
            mlds_maybe_aux_func_id_to_suffix(MaybeAux), !IO)
    ;
        FunctionName = mlds_function_export(Name),
        io.write_string(Stream, Name, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred output_pred_label_for_java(io.text_output_stream::in,
    mlds_pred_label::in, io::di, io::uo) is det.

output_pred_label_for_java(Stream, PredLabel, !IO) :-
    (
        PredLabel = mlds_user_pred_label(PredOrFunc, MaybeDefiningModule, Name,
            PredArity, _, _),
        (
            PredOrFunc = pf_predicate,
            Suffix = "p",
            OrigArity = PredArity
        ;
            PredOrFunc = pf_function,
            Suffix = "f",
            OrigArity = PredArity - 1
        ),
        MangledName = name_mangle_no_leading_digit(Name),
        io.format(Stream, "%s_%d_%s",
            [s(MangledName), i(OrigArity), s(Suffix)], !IO),
        (
            MaybeDefiningModule = yes(DefiningModule),
            io.write_string(Stream, "_in__", !IO),
            output_module_name(Stream, DefiningModule, !IO)
        ;
            MaybeDefiningModule = no
        )
    ;
        PredLabel = mlds_special_pred_label(PredName, MaybeTypeModule,
            TypeName, TypeArity),
        MangledPredName = name_mangle_no_leading_digit(PredName),
        MangledTypeName = name_mangle(TypeName),
        io.write_string(Stream, MangledPredName, !IO),
        io.write_string(Stream, "__", !IO),
        (
            MaybeTypeModule = yes(TypeModule),
            output_module_name(Stream, TypeModule, !IO),
            io.write_string(Stream, "__", !IO)
        ;
            MaybeTypeModule = no
        ),
        io.format(Stream, "%s_%d", [s(MangledTypeName), i(TypeArity)], !IO)
    ).

mlds_output_proc_label_for_java(Stream, mlds_proc_label(PredLabel, ProcId),
        !IO) :-
    output_pred_label_for_java(Stream, PredLabel, !IO),
    proc_id_to_int(ProcId, ModeNum),
    io.format(Stream, "_%d", [i(ModeNum)], !IO).

%---------------------------------------------------------------------------%

qual_class_name_to_string_for_java(QualClassName, Arity, String) :-
    QualClassName = qual_class_name(MLDS_ModuleName, QualKind, ClassName),
    ( if
        SymName = mlds_module_name_to_sym_name(MLDS_ModuleName),
        SymName = java_mercury_runtime_package_name
    then
        % Don't mangle runtime class names.
        String = "jmercury.runtime." ++ ClassName
    else
        qualifier_to_string_for_java(MLDS_ModuleName, QualKind, QualString),
        unqual_class_name_to_string_for_java(ClassName, Arity, UnqualString),
        String = QualString ++ "." ++ UnqualString
    ).

output_unqual_class_name_for_java(Stream, Name, Arity, !IO) :-
    unqual_class_name_to_string_for_java(Name, Arity, String),
    io.write_string(Stream, String, !IO).

output_class_name_arity_for_java(Stream, ClassName, ClassArity, !IO) :-
    output_unqual_class_name_for_java(Stream, ClassName, ClassArity, !IO).

:- pred unqual_class_name_to_string_for_java(mlds_class_name::in, arity::in,
    string::out) is det.

unqual_class_name_to_string_for_java(Name, Arity, String) :-
    MangledName = name_mangle_no_leading_digit(Name),
    % By convention, class names should start with a capital letter.
    UppercaseMangledName = flip_initial_case(MangledName),
    String = UppercaseMangledName ++ "_" ++ string.from_int(Arity).

output_field_var_name_for_java(Stream, FieldVarName, !IO) :-
    NameStr = ml_field_var_name_to_string(FieldVarName),
    output_valid_mangled_name_for_java(Stream, NameStr, !IO).

%---------------------------------------------------------------------------%

output_local_var_name_for_java(Stream, LocalVarName, !IO) :-
    NameStr = ml_local_var_name_to_string(LocalVarName),
    output_valid_mangled_name_for_java(Stream, NameStr, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

output_qual_name_prefix_java(Stream, ModuleName, QualKind, !IO) :-
    qualifier_to_string_for_java(ModuleName, QualKind, QualifierString),
    io.write_string(Stream, QualifierString, !IO),
    io.write_string(Stream, ".", !IO).

:- pred qualifier_to_string_for_java(mlds_module_name::in, mlds_qual_kind::in,
    string::out) is det.

qualifier_to_string_for_java(MLDS_ModuleName, QualKind, String) :-
    mlds_module_name_to_package_name(MLDS_ModuleName) = OuterName,
    mlds_module_name_to_sym_name(MLDS_ModuleName) = InnerName,

    % The part of the qualifier that corresponds to a top-level Java class.
    mangle_sym_name_for_java(OuterName, module_qual, "__", MangledOuterName),

    % The later parts of the qualifier correspond to nested Java classes.
    ( if OuterName = InnerName then
        MangledSuffix = ""
    else
        remove_sym_name_prefix(InnerName, OuterName, Suffix),
        mangle_sym_name_for_java(Suffix, convert_qual_kind(QualKind), ".",
            MangledSuffix0),
        MangledSuffix = "." ++ MangledSuffix0
    ),

    String = MangledOuterName ++ MangledSuffix.

%---------------------------------------------------------------------------%

:- pred output_module_name(io.text_output_stream::in, mercury_module_name::in,
    io::di, io::uo) is det.

output_module_name(Stream, ModuleName, !IO) :-
    io.write_string(Stream, sym_name_mangle(ModuleName), !IO).

:- pred output_valid_mangled_name_for_java(io.text_output_stream::in,
    string::in, io::di, io::uo) is det.

output_valid_mangled_name_for_java(Stream, Name, !IO) :-
    MangledName = name_mangle(Name),
    JavaSafeName = make_valid_java_symbol_name(MangledName),
    io.write_string(Stream, JavaSafeName, !IO).

%---------------------------------------------------------------------------%
%
% Code to mangle names, enforce Java code conventions regarding class names
% etc.
% XXX None of this stuff works as it should. The idea is that class names
% should start with an uppercase letter, while method names and package
% specifiers should start with a lowercase letter.
% The current implementation of the MLDS makes this rather harder to achieve
% than it might initially seem. The current position is that coding conventions
% are only enforced on library modules.
% This is needed as Java compilers don't take too well to compiling
% classes named `char',`int', `float' etc.
%

    % XXX This won't work if we start using the Java coding conventions
    % for all names. At the moment it only affects library modules.
    %
:- pred enforce_java_names(string::in, string::out) is det.
:- pragma consider_used(pred(enforce_java_names/2)).

enforce_java_names(Name, JavaName) :-
    % If the Name contains one or more dots (`.'), then capitalize
    % the first letter after the last dot.
    reverse_string(Name, RevName),
    ( if string.sub_string_search(RevName, ".", Pos) then
        string.split(RevName, Pos, Head0, Tail0),
        reverse_string(Tail0, Tail),
        reverse_string(Head0, Head1),
        string.capitalize_first(Head1, Head),
        string.append(Tail, Head, JavaName)
    else
        JavaName = Name
    ).

:- pred reverse_string(string::in, string::out) is det.

reverse_string(String0, String) :-
    string.to_char_list(String0, String1),
    string.from_rev_char_list(String1, String).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_java_name.
%---------------------------------------------------------------------------%
