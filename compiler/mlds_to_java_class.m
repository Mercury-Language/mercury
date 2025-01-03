%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018, 2020, 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output class declarations and definitions in Java.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_java_class.
:- interface.

:- import_module libs.
:- import_module libs.indent.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_java_util.

:- import_module io.
:- import_module map.

%---------------------------------------------------------------------------%

:- pred output_class_defn_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, mlds_class_defn::in,
    io::di, io::uo) is det.

:- pred output_enum_class_defn_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, mlds_enum_class_defn::in,
    io::di, io::uo) is det.

:- pred output_env_defn_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, mlds_env_defn::in,
    io::di, io::uo) is det.

:- pred output_struct_defn_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, mlds_struct_defn::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Rename class names which are too long. Each class results in a separate
    % `.class' file, so a long class name may exceed filesystem limits.
    % The long names tend to be automatically generated by the compiler.
    %
:- pred maybe_shorten_long_class_name(
    mlds_class_defn::in, mlds_class_defn::out,
    map(mlds_class_name, mlds_class_name)::in,
    map(mlds_class_name, mlds_class_name)::out) is det.
:- pred maybe_shorten_long_env_name(
    mlds_env_defn::in, mlds_env_defn::out,
    map(mlds_class_name, mlds_class_name)::in,
    map(mlds_class_name, mlds_class_name)::out) is det.

%---------------------------------------------------------------------------%

    % Succeeds iff a given string matches the unqualified interface name
    % of a interface in Mercury's Java runtime system.
    %
:- pred interface_is_special_for_java(string::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.mlds_to_java_data.
:- import_module ml_backend.mlds_to_java_func.
:- import_module ml_backend.mlds_to_java_name.
:- import_module ml_backend.mlds_to_java_type.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module uint.

%---------------------------------------------------------------------------%
%
% Code to output classes.
%

output_class_defn_for_java(Info0, Stream, Indent, ClassDefn, !IO) :-
    ClassDefn = mlds_class_defn(ClassName, ClassArity, Context, Flags,
        _Imports, Inherits, Implements, TypeParams,
        MemberFields, MemberClasses, MemberMethods, Ctors),
    indent_line_after_context(Stream, Info0 ^ joi_line_numbers,
        marker_comment, Context, Indent, !IO),
    output_class_decl_flags_for_java(Info0, Stream, Flags, !IO),

    Info1 = Info0 ^ joi_univ_tvars := TypeParams,
    % Use generics in the output of this predicate if this class represents
    % a Mercury type. Note that we do NOT return Info1 or Info to our caller.
    ( if list.member(ml_java_mercury_type_interface, Implements) then
        Info = Info1 ^ joi_output_generics := do_output_generics
    else
        Info = Info1
    ),

    ClassNameStr = unqual_class_name_to_string_for_java(ClassName, ClassArity),
    io.format(Stream, "class %s", [s(ClassNameStr)], !IO),
    OutputGenerics = Info ^ joi_output_generics,
    (
        OutputGenerics = do_output_generics,
        output_generic_tvars(Stream, TypeParams, !IO)
    ;
        OutputGenerics = do_not_output_generics
    ),
    io.nl(Stream, !IO),

    Indent1 = Indent + 1u,
    output_inherits_list(Info, Stream, Indent1, Inherits, !IO),
    output_implements_list(Stream, Indent1, Implements, !IO),
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    list.foldl(
        output_field_var_defn_for_java(Info, Stream, Indent1),
        MemberFields, !IO),
    list.foldl(
        output_class_defn_for_java(Info, Stream, Indent1),
        MemberClasses, !IO),
    list.foldl(
        output_function_defn_for_java(Info, Stream, Indent1, oa_none),
        MemberMethods, !IO),
    io.nl(Stream, !IO),
    list.foldl(
        output_function_defn_for_java(Info, Stream, Indent1,
            oa_cname(ClassName, ClassArity)),
        Ctors, !IO),
    io.format(Stream, "%s}\n\n", [s(IndentStr)], !IO).

output_enum_class_defn_for_java(Info0, Stream, Indent, EnumDefn, !IO) :-
    EnumDefn = mlds_enum_class_defn(ClassName, ClassArity, Context,
        Inherits, Implements, TypeParams, _ValueField, EnumConsts, Ctors),

    Info1 = Info0 ^ joi_univ_tvars := TypeParams,
    % Use generics in the output of this predicate if this class represents
    % a Mercury type. Note that we do NOT return Info1 or Info to our caller.
    ( if
        Implements = yes(Interface),
        Interface = ml_java_mercury_type_interface
    then
        InterfaceStr = interface_to_string_for_java(Interface),
        Info = Info1 ^ joi_output_generics := do_output_generics
    else
        unexpected($pred, "no ml_java_mercury_type_interface")
    ),

    indent_line_after_context(Stream, Info0 ^ joi_line_numbers,
        marker_comment, Context, Indent, !IO),
    ClassNameStr = unqual_class_name_to_string_for_java(ClassName, ClassArity),
    io.format(Stream, "public static class %s", [s(ClassNameStr)], !IO),
    output_generic_tvars(Stream, TypeParams, !IO),
    io.nl(Stream, !IO),

    IndentStr = indent2_string(Indent),
    Indent1 = Indent + 1u,
    output_enum_inherits_list(Info, Stream, Indent1, Inherits, !IO),
    io.format(Stream, "%simplements %s\n",
        [s(IndentStr), s(InterfaceStr)], !IO),
    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    list.foldl(
        output_enum_constant_for_java(Stream, Indent1, ClassName, ClassArity),
        EnumConsts, !IO),
    io.nl(Stream, !IO),
    output_enum_ctor_for_java(Stream, Indent1, ClassName, ClassArity, !IO),
    ClassAux = oa_cname(ClassName, ClassArity),
    list.foldl(
        output_function_defn_for_java(Info, Stream, Indent1, ClassAux),
        Ctors, !IO),
    io.format(Stream, "%s}\n\n", [s(IndentStr)], !IO).

output_env_defn_for_java(Info, Stream, Indent, EnvDefn, !IO) :-
    EnvDefn = mlds_env_defn(EnvName, Context, MemberFields),
    IndentStr = indent2_string(Indent),
    Indent1 = Indent + 1u,
    Indent1Str = indent2_string(Indent1),
    EnvNameStr = unqual_class_name_to_string_for_java(EnvName, 0),
    BaseTypeStr = type_to_string_for_java(Info, mlds_generic_env_ptr_type),

    indent_line_after_context(Stream, Info ^ joi_line_numbers,
        marker_comment, Context, Indent, !IO),
    io.format(Stream, "private static class %s\n", [s(EnvNameStr)], !IO),
    io.format(Stream, "%sextends %s\n", [s(Indent1Str), s(BaseTypeStr)], !IO),
    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    list.foldl(
        output_field_var_defn_for_java(Info, Stream, Indent1),
        MemberFields, !IO),
    io.format(Stream, "%s}\n\n", [s(IndentStr)], !IO).

output_struct_defn_for_java(Info, Stream, Indent, StructDefn, !IO) :-
    StructDefn = mlds_struct_defn(StructName, Context,
        MemberFields, MaybeCtor),
    (
        MaybeCtor = no,
        unexpected($pred, "MaybeCtor = no")
    ;
        MaybeCtor = yes(Ctor)
    ),
    StructNameStr = unqual_class_name_to_string_for_java(StructName, 0),
    Indent1 = Indent + 1u,
    IndentStr = indent2_string(Indent),

    indent_line_after_context(Stream, Info ^ joi_line_numbers,
        marker_comment, Context, Indent, !IO),
    io.format(Stream, "private static final class %s\n",
        [s(StructNameStr)], !IO),
    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    list.foldl(
        output_field_var_defn_for_java(Info, Stream, Indent1),
        MemberFields, !IO),
    io.nl(Stream, !IO),
    output_function_defn_for_java(Info, Stream, Indent1,
        oa_cname(StructName, 0), Ctor, !IO),
    io.format(Stream, "%s}\n\n", [s(IndentStr)], !IO).

%---------------------------------------------------------------------------%

    % Output superclass that this class extends. Java does not support
    % multiple inheritance, so more than one superclass is an error.
    %
:- pred output_inherits_list(java_out_info::in, io.text_output_stream::in,
    indent::in, mlds_class_inherits::in, io::di, io::uo) is det.

output_inherits_list(Info, Stream, Indent, Inherits, !IO) :-
    (
        Inherits = inherits_nothing
    ;
        Inherits = inherits_class(BaseClassId),
        IndentStr = indent2_string(Indent),
        BaseType = mlds_class_type(BaseClassId),
        BaseTypeStr = type_to_string_for_java(Info, BaseType),
        io.format(Stream, "%sextends %s\n",
            [s(IndentStr), s(BaseTypeStr)], !IO)
    ).

:- pred output_enum_inherits_list(java_out_info::in, io.text_output_stream::in,
    indent::in, mlds_enum_class_inherits::in, io::di, io::uo) is det.

output_enum_inherits_list(Info, Stream, Indent, Inherits, !IO) :-
    (
        Inherits = inherits_nothing
    ;
        Inherits = inherits_class(BaseClassId),
        BaseType = mlds_class_type(BaseClassId),
        IndentStr = indent2_string(Indent),
        BaseTypeStr = type_to_string_for_java(Info, BaseType),
        io.format(Stream, "%sextends %s\n",
            [s(IndentStr), s(BaseTypeStr)], !IO)
    ).

    % Output list of interfaces that this class implements.
    %
:- pred output_implements_list(io.text_output_stream::in, indent::in,
    list(mlds_interface_id)::in, io::di, io::uo) is det.

output_implements_list(Stream, Indent, InterfaceList, !IO)  :-
    (
        InterfaceList = []
    ;
        InterfaceList = [_ | _],
        IndentStr = indent2_string(Indent),
        InterfaceStrs = list.map(interface_to_string_for_java, InterfaceList),
        InterfacesStr = string.join_list(", ", InterfaceStrs),
        io.format(Stream, "%simplements %s\n",
            [s(IndentStr), s(InterfacesStr)], !IO)
    ).

:- func interface_to_string_for_java(mlds_interface_id) = string.

interface_to_string_for_java(Interface) = String :-
    Interface = mlds_interface_id(ModuleName, ClassName),
    SymName = mlds_module_name_to_sym_name(ModuleName),
    mangle_sym_name_for_java(SymName, module_qual, ".", ModuleNameStr),
    % Check if the interface is one of the ones in the runtime system.
    % If it is, we don't need to output the arity.
    ( if interface_is_special_for_java(ClassName) then
        string.format("%s.%s", [s(ModuleNameStr), s(ClassName)], String)
    else
        Arity = 0,
        string.format("%s.%s%d", [s(ModuleNameStr), s(ClassName), i(Arity)],
            String)
    ).

%---------------------------------------------------------------------------%
%
% Code for generating enumerations.
%
% Enumerations are a bit different from normal classes, because although
% the code generator generates them as classes, it treats them as integers.
% Here we treat them as objects (instantiations of the classes) rather than
% just as integers.

    % Output a (Java) constructor for the class representing the enumeration.
    %
:- pred output_enum_ctor_for_java(io.text_output_stream::in, indent::in,
    mlds_class_name::in, arity::in, io::di, io::uo) is det.

output_enum_ctor_for_java(Stream, Indent, ClassName, ClassArity, !IO) :-
    IndentStr = indent2_string(Indent),
    Indent1Str = indent2_string(Indent + 1u),
    UnqualClassnameStr =
        unqual_class_name_to_string_for_java(ClassName, ClassArity),
    io.format(Stream, "%sprivate %s(int val) {\n",
        [s(IndentStr), s(UnqualClassnameStr)], !IO),
    % Call the MercuryEnum constructor, which will set the MR_value field.
    io.format(Stream, "%ssuper(val);\n", [s(Indent1Str)], !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- pred output_enum_constant_for_java(io.text_output_stream::in,
    indent::in, mlds_class_name::in, arity::in, mlds_enum_const_defn::in,
    io::di, io::uo) is det.

output_enum_constant_for_java(Stream, Indent, ClassName, ClassArity,
        EnumConstDefn, !IO) :-
    EnumConstDefn = mlds_enum_const_defn(FieldVarName, _Context, EnumConst),
    % Make a static instance of the constant. The MLDS doesn't retain enum
    % constructor names (that shouldn't be hard to change now) so it is
    % easier to derive the name of the constant later by naming them after
    % the integer values.
    IndentStr = indent2_string(Indent),
    UnqualClassNameStr =
        unqual_class_name_to_string_for_java(ClassName, ClassArity),
    FieldVarNameStr = field_var_name_to_string_for_java(FieldVarName),
    (
        EnumConst = mlds_enum_const_uint(EnumUInt),
        io.format(Stream,
            "%spublic static final %s K%u = new %s(%u); /* %s */\n",
            [s(IndentStr), s(UnqualClassNameStr), u(EnumUInt),
            s(UnqualClassNameStr), u(EnumUInt), s(FieldVarNameStr)], !IO)
    ;
        EnumConst = mlds_enum_const_foreign(_Lang, _EnumNameStr, _Type),
        % Foreign enums are not currently supported by the Java backend.
        unexpected($pred, "not mlconst_enum")
    ).

%---------------------------------------------------------------------------%

:- pred output_field_var_decl_for_java(java_out_info::in,
    io.text_output_stream::in, mlds_field_var_name::in, mlds_type::in,
    io::di, io::uo) is det.

output_field_var_decl_for_java(Info, Stream, FieldVarName, Type, !IO) :-
    TypeStr = type_to_string_for_java(Info, Type),
    FieldVarNameStr = field_var_name_to_string_for_java(FieldVarName),
    io.format(Stream, "%s %s", [s(TypeStr), s(FieldVarNameStr)], !IO).

:- pred output_field_var_defn_for_java(java_out_info::in,
    io.text_output_stream::in, indent::in, mlds_field_var_defn::in,
    io::di, io::uo) is det.

output_field_var_defn_for_java(Info, Stream, Indent, FieldVarDefn, !IO) :-
    FieldVarDefn = mlds_field_var_defn(FieldVarName, Context, Flags, Type,
        Initializer, _),
    indent_line_after_context(Stream, Info ^ joi_line_numbers, marker_comment,
        Context, Indent, !IO),
    output_field_var_decl_flags_for_java(Stream, Flags, !IO),
    output_field_var_decl_for_java(Info, Stream, FieldVarName, Type, !IO),
    output_initializer_for_java(Info, Stream, oa_none, Indent + 1u,
        Type, Initializer, ";", !IO).

%---------------------------------------------------------------------------%
%
% Code to output declaration specifiers.
%

:- pred output_field_var_decl_flags_for_java(io.text_output_stream::in,
    mlds_field_var_decl_flags::in, io::di, io::uo) is det.

output_field_var_decl_flags_for_java(Stream, Flags, !IO) :-
    Flags = mlds_field_var_decl_flags(PerInstance, Constness),
    io.write_string(Stream, "public ", !IO),
    (
        PerInstance = per_instance
    ;
        PerInstance = one_copy,
        io.write_string(Stream, "static ", !IO)
    ),
    output_overridability_constness_for_java(Stream, overridable,
        Constness, !IO).

:- pred output_class_decl_flags_for_java(java_out_info::in,
    io.text_output_stream::in, mlds_class_decl_flags::in,
    io::di, io::uo) is det.

output_class_decl_flags_for_java(_Info, Stream, Flags, !IO) :-
    Flags = mlds_class_decl_flags(Access, Overrability, Constness),
    (
        Access = class_public,
        io.write_string(Stream, "public ", !IO)
    ;
        Access = class_private,
        io.write_string(Stream, "private ", !IO)
    ),
    % PerInstance = one_copy,
    io.write_string(Stream, "static ", !IO),
    output_overridability_constness_for_java(Stream, Overrability,
        Constness, !IO).

:- pred output_overridability_constness_for_java(io.text_output_stream::in,
    overridability::in, constness::in, io::di, io::uo) is det.

output_overridability_constness_for_java(Stream, Overridability,
        Constness, !IO) :-
    ( if
        ( Overridability = sealed
        ; Constness = const
        )
    then
        io.write_string(Stream, "final ", !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%
%
% Code to rename long class names.
%

maybe_shorten_long_class_name(!ClassDefn, !Renaming) :-
    !.ClassDefn = mlds_class_defn(ClassName0, _ClassArity, _Context, Flags,
        _Imports, _Inherits, _Implements, _TypeParams,
        _MemberFields, _MemberClasses, _MemberMethods, _Ctors),
    Flags = mlds_class_decl_flags(Access, _Overridability, _Constness),
    (
        % We only rename private classes for now.
        Access = class_private,
        ClassName = shorten_class_name(ClassName0),
        ( if ClassName = ClassName0 then
            true
        else
            !ClassDefn ^ mcd_class_name := ClassName,
            map.det_insert(ClassName0, ClassName, !Renaming)
        )
    ;
        Access = class_public
    ).

maybe_shorten_long_env_name(!EnvDefn, !Renaming) :-
    !.EnvDefn = mlds_env_defn(EnvName0, Context, MemberFields),
    % All environment definitions are private.
    EnvName = shorten_class_name(EnvName0),
    ( if EnvName = EnvName0 then
        true
    else
        !:EnvDefn = mlds_env_defn(EnvName, Context, MemberFields),
        map.det_insert(EnvName0, EnvName, !Renaming)
    ).

:- func shorten_class_name(string) = string.

shorten_class_name(ClassName0) = ClassName :-
    MangledClassName0 = name_mangle_no_leading_digit(ClassName0),
    ( if string.length(MangledClassName0) < 100 then
        ClassName = ClassName0
    else
        % The new name must not require name mangling, as then the name may
        % again be too long. We replace all non-alphanumeric or underscore
        % characters by underscores. The s_ prefix avoids having f_ as the
        % prefix which is used to indicate a mangled name.
        Left = string.left(ClassName0, 44),
        Middle = c_util.hex_hash32(ClassName0),
        Right = string.right(ClassName0, 44),
        GenName = string.format("s_%s_%s_%s", [s(Left), s(Middle), s(Right)]),
        GenList = string.to_char_list(GenName),
        FilterList = list.map(replace_non_alphanum_underscore, GenList),
        ClassName = string.from_char_list(FilterList)
    ).

:- func replace_non_alphanum_underscore(char) = char.

replace_non_alphanum_underscore(Char) =
    ( if char.is_alnum_or_underscore(Char) then
        Char
    else
        '_'
    ).

%---------------------------------------------------------------------------%

interface_is_special_for_java("MercuryType").
interface_is_special_for_java("MethodPtr").
interface_is_special_for_java("MethodPtr1").
interface_is_special_for_java("MethodPtr2").
interface_is_special_for_java("MethodPtr3").
interface_is_special_for_java("MethodPtr4").
interface_is_special_for_java("MethodPtr5").
interface_is_special_for_java("MethodPtr6").
interface_is_special_for_java("MethodPtr7").
interface_is_special_for_java("MethodPtr8").
interface_is_special_for_java("MethodPtr9").
interface_is_special_for_java("MethodPtr10").
interface_is_special_for_java("MethodPtr11").
interface_is_special_for_java("MethodPtr12").
interface_is_special_for_java("MethodPtr13").
interface_is_special_for_java("MethodPtr14").
interface_is_special_for_java("MethodPtr15").
interface_is_special_for_java("MethodPtrN").

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_java_class.
%---------------------------------------------------------------------------%
