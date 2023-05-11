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

%---------------------------------------------------------------------------%

:- func global_var_name_to_string_for_java(mlds_global_var_name) = string.

:- func maybe_qualified_global_var_name_to_string_for_java(java_out_info,
    qual_global_var_name) = string.

%---------------------------------------------------------------------------%

:- func local_var_name_to_string_for_java(mlds_local_var_name) = string.

%---------------------------------------------------------------------------%

:- func field_var_name_to_string_for_java(mlds_field_var_name) = string.

%---------------------------------------------------------------------------%

:- func unqual_class_name_to_string_for_java(mlds_class_name, arity) = string.

:- func qual_class_name_to_string_for_java(qual_class_name, arity) = string.

%---------------------------------------------------------------------------%

:- func function_name_to_string_for_java(mlds_function_name) = string.

:- func maybe_qualified_function_name_to_string_for_java(java_out_info,
    qual_function_name) = string.

%---------------------------------------------------------------------------%

:- func proc_label_to_string_for_java(mlds_proc_label) = string.

%---------------------------------------------------------------------------%

:- func qualifier_to_string_for_java(mlds_module_name, mlds_qual_kind)
    = string.

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

global_var_name_to_string_for_java(GlobalVarName) = GlobalVarNameStr :-
    (
        GlobalVarName = gvn_const_var(ConstVar, Num),
        NameStr = ml_global_const_var_name_to_string(ConstVar, Num),
        GlobalVarNameStr = make_valid_mangled_name_for_java(NameStr)
    ;
        GlobalVarName = gvn_rtti_var(RttiId),
        rtti.id_to_c_identifier(RttiId, RttiAddrName),
        GlobalVarNameStr = RttiAddrName
    ;
        GlobalVarName = gvn_tabling_var(ProcLabel, TablingStructId),
        TablingProcLabel = mlds_std_tabling_proc_label(ProcLabel),
        TablingProcLabelStr = proc_label_to_string_for_java(TablingProcLabel),
        string.format("%s_%s",
            [s(tabling_info_id_str(TablingStructId)), s(TablingProcLabelStr)],
            GlobalVarNameStr)
    ;
        GlobalVarName = gvn_dummy_var,
        GlobalVarNameStr = "dummy_var"
    ).

maybe_qualified_global_var_name_to_string_for_java(Info, QualGlobalVarName)
        = MaybeQualGlobalVarNameStr :-
    QualGlobalVarName = qual_global_var_name(GlobalVarModule, GlobalVarName),
    GlobalVarNameStr = global_var_name_to_string_for_java(GlobalVarName),
    % Don't module qualify the names of global variables that are defined
    % in the current module. This avoids unnecessary verbosity.
    CurrentModule = Info ^ joi_module_name,
    ( if GlobalVarModule = CurrentModule then
        MaybeQualGlobalVarNameStr = GlobalVarNameStr
    else
        QualStr = qualifier_to_string_for_java(GlobalVarModule, module_qual),
        string.format("%s.%s", [s(QualStr), s(GlobalVarNameStr)],
            MaybeQualGlobalVarNameStr)
    ).

%---------------------------------------------------------------------------%

local_var_name_to_string_for_java(LocalVarName) = MangledNameStr :-
    NameStr = ml_local_var_name_to_string(LocalVarName),
    MangledNameStr = make_valid_mangled_name_for_java(NameStr).

%---------------------------------------------------------------------------%

field_var_name_to_string_for_java(FieldVarName) = MangledNameStr :-
    NameStr = ml_field_var_name_to_string(FieldVarName),
    MangledNameStr = make_valid_mangled_name_for_java(NameStr).

%---------------------------------------------------------------------------%

unqual_class_name_to_string_for_java(Name, Arity) = Str :-
    MangledName = name_mangle_no_leading_digit(Name),
    % By convention, class names should start with a capital letter.
    UppercaseMangledName = flip_initial_case(MangledName),
    string.format("%s_%d", [s(UppercaseMangledName), i(Arity)], Str).

qual_class_name_to_string_for_java(QualClassName, Arity) = QualClassNameStr :-
    QualClassName = qual_class_name(MLDS_ModuleName, QualKind, ClassName),
    ( if
        SymName = mlds_module_name_to_sym_name(MLDS_ModuleName),
        SymName = java_mercury_runtime_package_name
    then
        % Don't mangle runtime class names.
        QualClassNameStr = "jmercury.runtime." ++ ClassName
    else
        QualStr = qualifier_to_string_for_java(MLDS_ModuleName, QualKind),
        UnqualClassNameStr =
            unqual_class_name_to_string_for_java(ClassName, Arity),
        string.format("%s.%s", [s(QualStr), s(UnqualClassNameStr)],
            QualClassNameStr)
    ).

%---------------------------------------------------------------------------%

function_name_to_string_for_java(FunctionName) = FunctionNameStr :-
    (
        FunctionName = mlds_function_name(PlainFuncName),
        PlainFuncName = mlds_plain_func_name(FuncLabel, _PredId),
        FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
        ProcLabel = mlds_proc_label(PredLabel, ProcId),
        PredLabelStr = pred_label_to_string_for_java(PredLabel),
        proc_id_to_int(ProcId, ModeNum),
        MaybeAuxSuffix = mlds_maybe_aux_func_id_to_suffix(MaybeAux),
        string.format("%s_%d%s",
            [s(PredLabelStr), i(ModeNum), s(MaybeAuxSuffix)], FunctionNameStr)
    ;
        FunctionName = mlds_function_export(FunctionNameStr)
    ).

maybe_qualified_function_name_to_string_for_java(Info, QualFuncName)
        = MaybeQualFuncNameStr :-
    QualFuncName = qual_function_name(FuncModule, FuncName),
    FuncNameStr = function_name_to_string_for_java(FuncName),
    % Don't module qualify the names of functions that are defined
    % in the current module. This avoids unnecessary verbosity.
    CurrentModule = Info ^ joi_module_name,
    ( if FuncModule = CurrentModule then
        MaybeQualFuncNameStr = FuncNameStr
    else
        QualStr = qualifier_to_string_for_java(FuncModule, module_qual),
        string.format("%s.%s", [s(QualStr), s(FuncNameStr)],
            MaybeQualFuncNameStr)
    ).

%---------------------------------------------------------------------------%

proc_label_to_string_for_java(ProcLabel) = ProcLabelStr :-
    ProcLabel = mlds_proc_label(PredLabel, ProcId),
    PredLabelStr = pred_label_to_string_for_java(PredLabel),
    proc_id_to_int(ProcId, ModeNum),
    string.format("%s_%d", [s(PredLabelStr), i(ModeNum)], ProcLabelStr).

%---------------------------------------------------------------------------%

:- func pred_label_to_string_for_java(mlds_pred_label) = string.

pred_label_to_string_for_java(PredLabel) = PredLabelStr :-
    (
        PredLabel = mlds_user_pred_label(PredOrFunc, MaybeDefiningModule, Name,
            PredFormArity, _, _),
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
            string.format("%s_%d_%s_in__%s",
                [s(MangledName), i(UserArityInt), s(Suffix),
                s(sym_name_mangle(DefiningModule))], PredLabelStr)
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
            MangledTypeModule = sym_name_mangle(TypeModule),
            string.format("%s__%s__%s_%d",
                [s(MangledPredName), s(MangledTypeModule),
                s(MangledTypeName), i(TypeArity)], PredLabelStr)
        ;
            MaybeTypeModule = no,
            string.format("%s__%s_%d",
                [s(MangledPredName), s(MangledTypeName), i(TypeArity)],
                PredLabelStr)
        )
    ).

%---------------------------------------------------------------------------%

qualifier_to_string_for_java(MLDS_ModuleName, QualKind) = Qualifier :-
    mlds_module_name_to_package_name(MLDS_ModuleName) = OuterName,
    mlds_module_name_to_sym_name(MLDS_ModuleName) = InnerName,

    % The part of the qualifier that corresponds to a top-level Java class.
    mangle_sym_name_for_java(OuterName, module_qual, "__", MangledOuterName),

    % The later parts of the qualifier correspond to nested Java classes.
    ( if OuterName = InnerName then
        Qualifier = MangledOuterName
    else
        remove_sym_name_prefix(InnerName, OuterName, Suffix),
        mangle_sym_name_for_java(Suffix, convert_qual_kind(QualKind), ".",
            MangledSuffix),
        string.format("%s.%s", [s(MangledOuterName), s(MangledSuffix)],
            Qualifier)
    ).

%---------------------------------------------------------------------------%

:- func make_valid_mangled_name_for_java(string) = string.

make_valid_mangled_name_for_java(Name) = JavaSafeName :-
    MangledName = name_mangle(Name),
    JavaSafeName = make_valid_java_symbol_name(MangledName).

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
    %
    % XXX This code looks like it has *major* efficiency problems.
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
