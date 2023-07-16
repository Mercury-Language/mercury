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
% The functions in this module which return a string containing a single
% identifier
%
% - limit the length of that string if their name contains to_ll_string
%   (ll being short for length-limited), but
% - do not limit the length if their name contains to_nll_string
%   (nll being short for non-length-limited).
%
% The reason for length limiting was originally documented with this comment:
%
%   Although the C# spec does not limit identifier lengths, the Microsoft
%   compiler restricts identifiers to 511 characters and Mono restricts
%   identifiers to 512 characters.
%   This assumes the identifier contains only ASCII characters.
%
% Note that strings that contain anything other than a single identifier
% should *not* have their length limited in this way, because the function
% that enforces this limit, limit_identifier_length, may do the wrong thing
% when given such a string. Specifically, if the string is longer than the
% limit but the identifier(s) in it are all within the limit, it may
% scramble one of those identifiers in a way that other references to that
% same identifier would not. This is then very likely to lead to a mismatch
% between the form of the identifier used in its definition versus some of
% its uses.
%
% This consideration means that module qualifiers, which (in the case of
% nested modules) may naturally contain more than one component, should
% *not* be length limited as a whole. We could apply length limitation
% to each component of the module qualifier individually, but we do not
% do so. This should not be a problem, because module names are decided
% by humans, and no sane human would use a >512 character string as
% *one* component of a module name. (The length limitation is an issue
% for *machine*-generated names, such as typeclass instances, whose names
% contain a description of the entire instance type vector.)
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

%---------------------------------------------------------------------------%

:- func strip_mercury_and_mangle_sym_name_for_csharp(sym_name) = string.

%---------------------------------------------------------------------------%

:- func limit_identifier_length(string) = string.

%---------------------------------------------------------------------------%

:- func global_var_name_to_nll_string_for_csharp(mlds_global_var_name)
    = string.
:- func global_var_name_to_ll_string_for_csharp(mlds_global_var_name) = string.

:- func maybe_qualified_global_var_name_to_string_for_csharp(csharp_out_info,
    qual_global_var_name) = string.

%---------------------------------------------------------------------------%

:- func local_var_name_to_nll_string_for_csharp(mlds_local_var_name) = string.
:- func local_var_name_to_ll_string_for_csharp(mlds_local_var_name) = string.

%---------------------------------------------------------------------------%

:- func field_var_name_to_nll_string_for_csharp(mlds_field_var_name) = string.
:- func field_var_name_to_ll_string_for_csharp(mlds_field_var_name) = string.

%---------------------------------------------------------------------------%

:- func unqual_class_name_to_nll_string_for_csharp(mlds_class_name, arity)
    = string.
:- func unqual_class_name_to_ll_string_for_csharp(mlds_class_name, arity)
    = string.

:- func qual_class_name_to_nll_string_for_csharp(qual_class_name, arity)
    = string.
:- func qual_class_name_to_ll_string_for_csharp(qual_class_name, arity)
    = string.

    % The nrt part of the name is short for "not runtime";
    % it says that the mlds_module_name argument must not refer to
    % csharp_mercury_runtime_package_name.
    %
:- func qual_nrt_name_to_nll_string_for_csharp(mlds_module_name, string, arity)
    = string.

%---------------------------------------------------------------------------%

:- func function_name_to_nll_string_for_csharp(mlds_function_name) = string.
:- func function_name_to_ll_string_for_csharp(mlds_function_name) = string.

:- func maybe_qualified_function_name_to_ll_string_for_csharp(csharp_out_info,
    qual_function_name) = string.

%---------------------------------------------------------------------------%

:- func proc_label_to_nll_string_for_csharp(string, mlds_proc_label) = string.
:- func proc_label_to_ll_string_for_csharp(string, mlds_proc_label) = string.

%---------------------------------------------------------------------------%

:- func qualifier_to_nll_string_for_csharp(mlds_module_name, mlds_qual_kind)
    = string.

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

maybe_qualified_global_var_name_to_string_for_csharp(Info, QualGlobalVarName)
        = MaybeQualGlobalVarNameStr :-
    QualGlobalVarName = qual_global_var_name(GlobalVarModule, GlobalVarName),
    GlobalVarNameStr = global_var_name_to_ll_string_for_csharp(GlobalVarName),
    % Don't module qualify names which are defined in the current module.
    % This avoids unnecessary verbosity.
    CurrentModule = Info ^ csoi_module_name,
    ( if GlobalVarModule = CurrentModule then
        MaybeQualGlobalVarNameStr = GlobalVarNameStr
    else
        QualStr =
            qualifier_to_nll_string_for_csharp(GlobalVarModule, module_qual),
        string.format("%s.%s", [s(QualStr), s(GlobalVarNameStr)],
            MaybeQualGlobalVarNameStr)
    ).

%---------------------------------------------------------------------------%

local_var_name_to_nll_string_for_csharp(LocalVarName) = LocalVarNameStr :-
    RawString = ml_local_var_name_to_string(LocalVarName),
    MangledString = name_mangle(RawString),
    LocalVarNameStr = make_valid_csharp_symbol_name(MangledString).

local_var_name_to_ll_string_for_csharp(LocalVarName) = LocalVarNameStr :-
    LocalVarNameStr0 = local_var_name_to_nll_string_for_csharp(LocalVarName),
    LocalVarNameStr = limit_identifier_length(LocalVarNameStr0).

%---------------------------------------------------------------------------%

field_var_name_to_nll_string_for_csharp(FieldVarName) = FieldVarNameStr :-
    RawString = ml_field_var_name_to_string(FieldVarName),
    MangledString = name_mangle(RawString),
    FieldVarNameStr = make_valid_csharp_symbol_name(MangledString).

field_var_name_to_ll_string_for_csharp(FieldVarName) = FieldVarNameStr :-
    FieldVarNameStr0 = field_var_name_to_nll_string_for_csharp(FieldVarName),
    FieldVarNameStr = limit_identifier_length(FieldVarNameStr0).

%---------------------------------------------------------------------------%

unqual_class_name_to_nll_string_for_csharp(Name, Arity) = ClassNameStr :-
    MangledName = name_mangle_no_leading_digit(Name),
    % By convention, class names should start with a capital letter.
    UppercaseMangledName = flip_initial_case(MangledName),
    string.format("%s_%d", [s(UppercaseMangledName), i(Arity)], ClassNameStr).

unqual_class_name_to_ll_string_for_csharp(Name, Arity) = ClassNameStr :-
    ClassNameStr0 = unqual_class_name_to_nll_string_for_csharp(Name, Arity),
    ClassNameStr = limit_identifier_length(ClassNameStr0).

qual_class_name_to_nll_string_for_csharp(QualName, Arity) = QualClassNameStr :-
    QualName = qual_class_name(ModuleName, QualKind, ClassName),
    SymName = mlds_module_name_to_sym_name(ModuleName),
    ( if SymName = csharp_mercury_runtime_package_name then
        % Don't mangle runtime class names.
        string.format("runtime.%s", [s(ClassName)], QualClassNameStr)
    else
        % XXX maybe duplicated code
        QualStr = qualifier_to_nll_string_for_csharp(ModuleName, QualKind),
        UnqualClassNameStr =
            unqual_class_name_to_nll_string_for_csharp(ClassName, Arity),
        string.format("%s.%s", [s(QualStr), s(UnqualClassNameStr)],
            QualClassNameStr)
    ).

qual_class_name_to_ll_string_for_csharp(QualName, Arity) = QualClassNameStr :-
    QualClassNameStr0 =
        qual_class_name_to_nll_string_for_csharp(QualName, Arity),
    QualClassNameStr = limit_identifier_length(QualClassNameStr0).

qual_nrt_name_to_nll_string_for_csharp(ModuleName, Name, Arity)
        = QualNameStr :-
    QualStr = qualifier_to_nll_string_for_csharp(ModuleName, module_qual),
    UnqualNameStr = unqual_class_name_to_nll_string_for_csharp(Name, Arity),
    string.format("%s.%s", [s(QualStr), s(UnqualNameStr)],
        QualNameStr).

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

maybe_qualified_function_name_to_ll_string_for_csharp(Info, QualFuncName)
        = MaybeQualFuncNameStr :-
    QualFuncName = qual_function_name(FuncModule, FuncName),
    FuncNameStr = function_name_to_ll_string_for_csharp(FuncName),
    % Don't module qualify names which are defined in the current module.
    % This avoids unnecessary verbosity.
    CurrentModule = Info ^ csoi_module_name,
    ( if FuncModule = CurrentModule then
        MaybeQualFuncNameStr = FuncNameStr
    else
        QualStr = qualifier_to_nll_string_for_csharp(FuncModule, module_qual),
        string.format("%s.%s", [s(QualStr), s(FuncNameStr)],
            MaybeQualFuncNameStr)
    ).

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

%---------------------------------------------------------------------------%

qualifier_to_nll_string_for_csharp(ModuleName, QualKind) = Str :-
    mlds_module_name_to_package_name(ModuleName) = OuterName,
    mlds_module_name_to_sym_name(ModuleName) = InnerName,

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

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_name.
%---------------------------------------------------------------------------%
