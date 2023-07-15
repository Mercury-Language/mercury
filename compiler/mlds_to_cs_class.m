%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output class declarations and definitions in C#.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_cs_class.
:- interface.

:- import_module libs.
:- import_module libs.indent.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_cs_util.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred output_class_defn_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_class_defn::in,
    io::di, io::uo) is det.

:- pred output_enum_class_defn_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_enum_class_defn::in,
    io::di, io::uo) is det.

:- pred output_env_defn_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_env_defn::in,
    io::di, io::uo) is det.

:- pred output_struct_defn_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_struct_defn::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.mlds_to_cs_data.
:- import_module ml_backend.mlds_to_cs_func.
:- import_module ml_backend.mlds_to_cs_name.
:- import_module ml_backend.mlds_to_cs_type.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

output_class_defn_for_csharp(Info0, Stream, Indent, ClassDefn, !IO) :-
    ClassDefn = mlds_class_defn(ClassName, ClassArity, _Context, Flags, Kind,
        _Imports, Inherits, Implements, TypeParams,
        MemberFields, MemberClasses, MemberMethods, Ctors),
    expect(unify(MemberMethods, []), $pred, "MemberMethods != []"),
    Info  = Info0 ^ csoi_univ_tvars := TypeParams,

    IndentStr = indent2_string(Indent),
    get_class_decl_flags_for_csharp(Kind, Flags, Serializable,
        AccessPrefix, PerInstancePrefix, OverridePrefix, ConstnessPrefix,
        ClassKindStr),
    ClassNameStr =
        unqual_class_name_to_ll_string_for_csharp(ClassName, ClassArity),
    OutputGenerics = Info ^ csoi_output_generics,
    (
        OutputGenerics = do_output_generics,
        GenericTypeParamsStr = generic_tvars_to_string(TypeParams)
    ;
        OutputGenerics = do_not_output_generics,
        GenericTypeParamsStr = ""
    ),
    (
        Serializable = no
    ;
        Serializable = yes,
        io.format(Stream, "%s[System.Serializable]\n", [s(IndentStr)], !IO)
    ),
    io.format(Stream, "%s%s%s%s%s%s %s%s\n",
        [s(IndentStr), s(AccessPrefix), s(PerInstancePrefix),
        s(OverridePrefix), s(ConstnessPrefix), s(ClassKindStr),
        s(ClassNameStr), s(GenericTypeParamsStr)], !IO),
    SuperClassNames = get_superclass_names(Info, Inherits, Implements),
    (
        SuperClassNames = []
    ;
        SuperClassNames = [_ | _],
        SuperClassesStr = string.join_list(", ", SuperClassNames),
        io.format(Stream, "%s  : %s\n",
            [s(IndentStr), s(SuperClassesStr)], !IO)
    ),

    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    Indent1 = Indent + 1,
    list.foldl(
        output_field_var_defn_for_csharp(Info, Stream, Indent1),
        MemberFields, !IO),
    list.foldl(
        output_class_defn_for_csharp(Info, Stream, Indent1),
        MemberClasses, !IO),
    io.nl(Stream, !IO),

    CtorsAux = oa_cname(ClassName, ClassArity),
    list.foldl(
        output_function_defn_for_csharp(Info, Stream, Indent1, CtorsAux),
        Ctors, !IO),
    io.format(Stream, "%s}\n\n", [s(IndentStr)], !IO).

output_enum_class_defn_for_csharp(Info0, Stream, Indent, EnumDefn, !IO) :-
    EnumDefn = mlds_enum_class_defn(ClassName, ClassArity, _Context,
        Inherits, Implements, TypeParams, _ValueField, EnumConsts, Ctors),
    Info  = Info0 ^ csoi_univ_tvars := TypeParams,

    IndentStr = indent2_string(Indent),
    AccessPrefix = access_prefix_for_csharp(class_public),
    PerInstancePrefix = per_instance_prefix_for_csharp(per_instance),
    OverridePrefix = overrideability_prefix_for_csharp(overridable),
    ConstnessPrefix = constness_prefix_for_csharp(modifiable),
    ClassNameStr =
        unqual_class_name_to_ll_string_for_csharp(ClassName, ClassArity),
    OutputGenerics = Info ^ csoi_output_generics,
    (
        OutputGenerics = do_output_generics,
        GenericTypeParamsStr = generic_tvars_to_string(TypeParams)
    ;
        OutputGenerics = do_not_output_generics,
        GenericTypeParamsStr = ""
    ),
    io.format(Stream, "%s[System.Serializable]\n", [s(IndentStr)], !IO),
    io.format(Stream, "%s%s%s%s%senum %s%s\n",
        [s(IndentStr), s(AccessPrefix), s(PerInstancePrefix),
        s(OverridePrefix), s(ConstnessPrefix),
        s(ClassNameStr), s(GenericTypeParamsStr)], !IO),
    expect(unify(Inherits, inherits_nothing), $pred,
        "Inherits != inherits_nothing"),
    expect(unify(Implements, no), $pred, "Implements != no"),

    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    Indent1 = Indent + 1,
    Indent2Str = indent2_string(Indent1),
    list.foldl(output_enum_constant_for_csharp(Info, Stream, Indent2Str),
        EnumConsts, !IO),

    CtorsAux = oa_cname(ClassName, ClassArity),
    list.foldl(
        output_function_defn_for_csharp(Info, Stream, Indent1, CtorsAux),
        Ctors, !IO),
    io.format(Stream, "%s}\n\n", [s(IndentStr)], !IO).

output_env_defn_for_csharp(Info, Stream, Indent, EnvDefn, !IO) :-
    EnvDefn = mlds_env_defn(EnvName, _Context, MemberFields),
    IndentStr = indent2_string(Indent),
    Indent1 = Indent + 1,
    Indent1Str = indent2_string(Indent1),
    EnvNameStr = unqual_class_name_to_ll_string_for_csharp(EnvName, 0),
    EnvTypeStr = type_to_string_for_csharp(Info, mlds_generic_env_ptr_type),

    io.format(Stream, "%s[System.Serializable]\n", [s(IndentStr)], !IO),
    io.format(Stream, "%sprivate class %s\n",
        [s(IndentStr), s(EnvNameStr)], !IO),
    io.format(Stream, "%s: %s\n", [s(Indent1Str), s(EnvTypeStr)], !IO),
    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    list.foldl(
        output_field_var_defn_for_csharp(Info, Stream, Indent1),
        MemberFields, !IO),
    io.format(Stream, "%s}\n\n", [s(IndentStr)], !IO).

output_struct_defn_for_csharp(Info, Stream, Indent, StructDefn, !IO) :-
    StructDefn = mlds_struct_defn(StructName, _Context,
        MemberFields, MaybeCtor),
    (
        MaybeCtor = no,
        unexpected($pred, "MaybeCtor = no")
    ;
        MaybeCtor = yes(Ctor)
    ),
    Indent1 = Indent + 1,
    IndentStr = indent2_string(Indent),
    StructNameStr = unqual_class_name_to_ll_string_for_csharp(StructName, 0),

    io.format(Stream, "%sprivate struct %s\n",
        [s(IndentStr), s(StructNameStr)], !IO),
    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    list.foldl(
        output_field_var_defn_for_csharp(Info, Stream, Indent1),
        MemberFields, !IO),
    io.nl(Stream, !IO),
    output_function_defn_for_csharp(Info, Stream, Indent1, 
        oa_cname(StructName, 0), Ctor, !IO),
    io.format(Stream, "%s}\n\n", [s(IndentStr)], !IO).

%---------------------------------------------------------------------------%

    % Output superclass that this class extends and interfaces implemented.
    % C# does not support multiple inheritance, so more than one superclass
    % is an error.
    % XXX This predicate does not output anything.
    %
:- func get_superclass_names(csharp_out_info,
    mlds_class_inherits, list(mlds_interface_id)) = list(string).

get_superclass_names(Info, Inherits, Interfaces) = Supers :-
    Supers0 = list.map(interface_to_string_for_csharp, Interfaces),
    (
        Inherits = inherits_nothing,
        Supers = Supers0
    ;
        Inherits = inherits_class(BaseClassId),
        BaseClassType = mlds_class_type(BaseClassId),
        BaseClassTypeName = type_to_string_for_csharp(Info, BaseClassType),
        Supers = [BaseClassTypeName | Supers0]
    ).

:- func interface_to_string_for_csharp(mlds_interface_id) = string.

interface_to_string_for_csharp(InterfaceId) = String :-
    InterfaceId = mlds_interface_id(QualClassName, Arity, _),
    QualClassName = qual_class_name(ModuleQualifier, _QualKind, ClassName),
    SymName = mlds_module_name_to_sym_name(ModuleQualifier),
    mangle_sym_name_for_csharp(SymName, module_qual, ".", ModuleNameStr),

    % Check if the interface is one of the ones in the runtime system.
    % If it is, we don't need to output the arity.
    ( if interface_is_special_for_csharp(ClassName) then
        String = string.format("%s.%s", [s(ModuleNameStr), s(ClassName)])
    else
        String = string.format("%s.%s%d",
            [s(ModuleNameStr), s(ClassName), i(Arity)])
    ).

    % Succeeds iff a given string matches the unqualified interface name
    % of a interface in Mercury's C# runtime system.
    %
:- pred interface_is_special_for_csharp(string::in) is semidet.

interface_is_special_for_csharp("MercuryType").

%---------------------------------------------------------------------------%

:- pred output_field_var_defn_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_field_var_defn::in,
    io::di, io::uo) is det.

output_field_var_defn_for_csharp(Info, Stream, Indent, FieldVarDefn, !IO) :-
    FieldVarDefn = mlds_field_var_defn(FieldVarName, _Context, Flags,
        Type, Initializer, _),
    IndentStr = indent2_string(Indent),
    get_field_var_decl_flags_for_csharp(Flags, AccessPrefix, PerInstancePrefix,
        ConstnessPrefix),
    TypeStr = type_to_string_for_csharp(Info, Type),
    FieldVarNameStr = field_var_name_to_ll_string_for_csharp(FieldVarName),
    io.format(Stream, "%s%s%s%s%s %s",
        [s(IndentStr), s(AccessPrefix), s(PerInstancePrefix),
        s(ConstnessPrefix), s(TypeStr), s(FieldVarNameStr)], !IO),
    output_initializer_for_csharp(Info, Stream, oa_none, Indent + 1,
        Type, Initializer, ";", !IO).

%---------------------------------------------------------------------------%

:- pred output_enum_constant_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, string::in, mlds_enum_const_defn::in,
    io::di, io::uo) is det.

output_enum_constant_for_csharp(Info, Stream, IndentStr, EnumConstDefn, !IO) :-
    EnumConstDefn = mlds_enum_const_defn(VarName, _Context, EnumConst),
    VarNameStr = field_var_name_to_ll_string_for_csharp(VarName),
    (
        EnumConst = mlds_enum_const_uint(EnumUInt),
        io.format(Stream, "%s%s = %u,\n",
            [s(IndentStr), s(VarNameStr), u(EnumUInt)], !IO)
    ;
        EnumConst = mlds_enum_const_foreign(Lang, EnumNameStr, Type),
        % The name might require mangling.
        expect(unify(Lang, lang_csharp), $pred, "Lang != lang_csharp"),
        TypeStr = type_to_string_for_csharp(Info, Type),
        io.format(Stream, "%s%s = (%s) %s,\n",
            [s(IndentStr), s(VarNameStr), s(TypeStr), s(EnumNameStr)], !IO)
    ).

%---------------------------------------------------------------------------%
%
% Code to output declaration specifiers.
%

:- pred get_field_var_decl_flags_for_csharp(mlds_field_var_decl_flags::in,
    string::out, string::out, string::out) is det.

get_field_var_decl_flags_for_csharp(Flags, AccessPrefix, PerInstancePrefix,
        ConstnessPrefix) :-
    Flags = mlds_field_var_decl_flags(PerInstance, Constness),
    AccessPrefix = "public ",
    PerInstancePrefix = per_instance_prefix_for_csharp(PerInstance),
    ConstnessPrefix = constness_prefix_for_csharp(Constness).

:- pred get_class_decl_flags_for_csharp(mlds_class_kind::in,
    mlds_class_decl_flags::in, bool::out,
    string::out, string::out, string::out, string::out, string::out) is det.

get_class_decl_flags_for_csharp(Kind, Flags, Serializable,
        AccessPrefix, PerInstancePrefix, OverridePrefix, ConstnessPrefix,
        KindStr) :-
    Flags = mlds_class_decl_flags(Access, Overridability0, Constness),
    (
        % `static' not wanted on classes generated for Mercury types.
        Kind = mlds_class,
        KindStr = "class",
        Serializable = yes,
        PerInstance = per_instance,
        Overridability = Overridability0
    ;
        Kind = mlds_interface,
        KindStr = "interface",
        Serializable = no,
        PerInstance = one_copy,
        Overridability = Overridability0
    ),
    AccessPrefix = access_prefix_for_csharp(Access),
    PerInstancePrefix = per_instance_prefix_for_csharp(PerInstance),
    OverridePrefix = overrideability_prefix_for_csharp(Overridability),
    ConstnessPrefix = constness_prefix_for_csharp(Constness).

%---------------------------------------------------------------------------%
%
% Every prefix string that is not empty should end with a space.
%

:- func access_prefix_for_csharp(class_access) = string.

access_prefix_for_csharp(Access) = AccessPrefix :-
    (
        Access = class_public,
        AccessPrefix = "public "
    ;
        Access = class_private,
        AccessPrefix = "private "
    ).

:- func per_instance_prefix_for_csharp(per_instance) = string.

per_instance_prefix_for_csharp(PerInstance) = PerInstancePrefix :-
    (
        PerInstance = per_instance,
        PerInstancePrefix = ""
    ;
        PerInstance = one_copy,
        PerInstancePrefix = "static "
    ).

:- func overrideability_prefix_for_csharp(overridability) = string.

overrideability_prefix_for_csharp(Overridability) = OverridePrefix :-
    (
        Overridability = sealed,
        OverridePrefix = "sealed "
    ;
        Overridability = overridable,
        OverridePrefix = ""
    ).

:- func constness_prefix_for_csharp(constness) = string.

constness_prefix_for_csharp(Constness) = ConstnessPrefix :-
    (
        Constness = const,
        ConstnessPrefix = "readonly "
    ;
        Constness = modifiable,
        ConstnessPrefix = ""
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_class.
%---------------------------------------------------------------------------%
