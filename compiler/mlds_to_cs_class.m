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

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_cs_util.
:- import_module ml_backend.mlds_to_target_util.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred output_class_defn_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_class_defn::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.indent.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.mlds_to_cs_data.
:- import_module ml_backend.mlds_to_cs_func.
:- import_module ml_backend.mlds_to_cs_name.
:- import_module ml_backend.mlds_to_cs_type.
:- import_module parse_tree.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module list.
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
    (
        ( Kind = mlds_class
        ; Kind = mlds_interface
        ; Kind = mlds_struct
        ),
        list.foldl(
            output_field_var_defn_for_csharp(Info, Stream, Indent + 1),
            MemberFields, !IO),
        list.foldl(
            output_class_defn_for_csharp(Info, Stream, Indent + 1),
            MemberClasses, !IO),
        io.nl(Stream, !IO)
    ;
        Kind = mlds_enum,
        list.filter(field_var_defn_is_enum_const,
            MemberFields, EnumConstMemberFields),
        % XXX Why +2?
        Indent2Str = indent2_string(Indent + 2),
        list.foldl(output_enum_constant_for_csharp(Info, Stream, Indent2Str),
            EnumConstMemberFields, !IO)

    ),
    CtorsAux = oa_cname(ClassName, ClassArity),
    list.foldl(
        output_function_defn_for_csharp(Info, Stream, Indent + 1, CtorsAux),
        Ctors, !IO),
    io.format(Stream, "%s}\n\n", [s(IndentStr)], !IO).

    % Output superclass that this class extends and interfaces implemented.
    % C# does not support multiple inheritance, so more than one superclass
    % is an error.
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
    ;
        Inherits = inherits_generic_env_ptr_type,
        EnvPtrTypeName =
            type_to_string_for_csharp(Info, mlds_generic_env_ptr_type),
        Supers = [EnvPtrTypeName | Supers0]
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
    io.text_output_stream::in, string::in, mlds_field_var_defn::in, 
    io::di, io::uo) is det.

output_enum_constant_for_csharp(Info, Stream, IndentStr, FieldVarDefn, !IO) :-
    FieldVarDefn = mlds_field_var_defn(FieldVarName, _Context, _Flags,
        _Type, Initializer, _GCStmt),
    (
        Initializer = init_obj(Rval),
        % The name might require mangling.
        FieldVarNameStr = field_var_name_to_ll_string_for_csharp(FieldVarName),
        ( if
            Rval = ml_const(mlconst_enum(N, _))
        then
            io.format(Stream, "%s%s = %d,\n",
                [s(IndentStr), s(FieldVarNameStr), i(N)], !IO)
        else if
            Rval = ml_const(mlconst_foreign(lang_csharp, ConstStr, Type))
        then
            TypeStr = type_to_string_for_csharp(Info, Type),
            io.format(Stream, "%s%s = (%s) %s,\n",
                [s(IndentStr), s(FieldVarNameStr), s(TypeStr), s(ConstStr)],
                !IO)
        else
            unexpected($pred, string(Rval))
        )
    ;
        ( Initializer = no_initializer
        ; Initializer = init_struct(_, _)
        ; Initializer = init_array(_)
        ),
        unexpected($pred, string(Initializer))
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
        (
            % `static' keyword not allowed on enumerations.
            Kind = mlds_enum,
            KindStr = "enum"
        ;
            % `static' not wanted on classes generated for Mercury types.
            Kind = mlds_class,
            KindStr = "class"
        ),
        Serializable = yes,
        PerInstance = per_instance,
        Overridability = Overridability0
    ;
        % `static' and `sealed' not wanted or allowed on structs.
        Kind = mlds_struct,
        KindStr = "struct",
        Serializable = no,
        PerInstance = per_instance,
        Overridability = overridable
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
