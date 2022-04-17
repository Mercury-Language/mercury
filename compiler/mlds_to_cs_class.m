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
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.mlds_to_cs_data.
:- import_module ml_backend.mlds_to_cs_func.
:- import_module ml_backend.mlds_to_cs_name.
:- import_module ml_backend.mlds_to_cs_type.
:- import_module parse_tree.
:- import_module parse_tree.java_names.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

output_class_defn_for_csharp(!.Info, Stream, Indent, ClassDefn, !IO) :-
    output_n_indents(Stream, Indent, !IO),
    ClassDefn = mlds_class_defn(ClassName, ClassArity, _Context, Flags, Kind,
        _Imports, Inherits, Implements, TypeParams,
        MemberFields, MemberClasses, MemberMethods, Ctors),
    expect(unify(MemberMethods, []), $pred, "MemberMethods != []"),
    (
        (
            % `static' keyword not allowed on enumerations.
            Kind = mlds_enum
        ;
            % `static' not wanted on classes generated for Mercury types.
            Kind = mlds_class
        ),
        io.write_string(Stream, "[System.Serializable]\n", !IO),
        output_n_indents(Stream, Indent, !IO)
    ;
        ( Kind = mlds_struct
        ; Kind = mlds_interface
        )
    ),
    output_class_decl_flags_for_csharp(!.Info, Stream, Flags, Kind, !IO),

    !Info ^ csoi_univ_tvars := TypeParams,

    output_class_kind_for_csharp(Stream, Kind, !IO),
    output_unqual_class_name_for_csharp(Stream, ClassName, ClassArity, !IO),
    OutputGenerics = !.Info ^ csoi_output_generics,
    (
        OutputGenerics = do_output_generics,
        output_generic_tvars(Stream, TypeParams, !IO)
    ;
        OutputGenerics = do_not_output_generics
    ),
    io.nl(Stream, !IO),

    output_supers_list(!.Info, Stream, Indent + 1, Inherits, Implements, !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "{\n", !IO),
    (
        ( Kind = mlds_class
        ; Kind = mlds_interface
        ; Kind = mlds_struct
        ),
        list.foldl(
            output_field_var_defn_for_csharp(!.Info, Stream, Indent + 1),
            MemberFields, !IO),
        list.foldl(
            output_class_defn_for_csharp(!.Info, Stream, Indent + 1),
            MemberClasses, !IO)
    ;
        Kind = mlds_enum,
        list.filter(field_var_defn_is_enum_const,
            MemberFields, EnumConstMemberFields),
        % XXX Why +2?
        output_enum_constants_for_csharp(!.Info, Stream, Indent + 2,
            EnumConstMemberFields, !IO)
    ),
    io.nl(Stream, !IO),
    list.foldl(
        output_function_defn_for_csharp(!.Info, Stream, Indent + 1,
            oa_cname(ClassName, ClassArity)),
        Ctors, !IO),
    output_n_indents(Stream, Indent, !IO),
    io.write_string(Stream, "}\n\n", !IO).

:- pred output_class_kind_for_csharp(io.text_output_stream::in,
    mlds_class_kind::in, io::di, io::uo) is det.

output_class_kind_for_csharp(Stream, Kind, !IO) :-
    (
        Kind = mlds_interface,
        io.write_string(Stream, "interface ", !IO)
    ;
        Kind = mlds_class,
        io.write_string(Stream, "class ", !IO)
    ;
        Kind = mlds_struct,
        io.write_string(Stream, "struct ", !IO)
    ;
        Kind = mlds_enum,
        io.write_string(Stream, "enum ", !IO)
    ).

    % Output superclass that this class extends and interfaces implemented.
    % C# does not support multiple inheritance, so more than one superclass
    % is an error.
    %
:- pred output_supers_list(csharp_out_info::in, io.text_output_stream::in,
    indent::in, mlds_class_inherits::in, list(mlds_interface_id)::in,
    io::di, io::uo) is det.

output_supers_list(Info, Stream, Indent, Inherits, Interfaces, !IO) :-
    list.map(interface_to_string, Interfaces, AfterColonStrings0),
    (
        Inherits = inherits_nothing,
        AfterColonStrings = AfterColonStrings0
    ;
        Inherits = inherits_class(BaseClassId),
        BaseClassType = mlds_class_type(BaseClassId),
        type_to_string_for_csharp(Info, BaseClassType, BaseClassString,
            _ArrayDims),
        AfterColonStrings = [BaseClassString | AfterColonStrings0]
    ;
        Inherits = inherits_generic_env_ptr_type,
        type_to_string_for_csharp(Info, mlds_generic_env_ptr_type,
            EnvPtrTypeString, _ArrayDims),
        AfterColonStrings = [EnvPtrTypeString | AfterColonStrings0]
    ),
    (
        AfterColonStrings = []
    ;
        AfterColonStrings = [_ | _],
        AfterColonString = string.join_list(", ", AfterColonStrings),
        output_n_indents(Stream, Indent, !IO),
        io.format(Stream, ": %s\n",  [s(AfterColonString)], !IO)
    ).

:- pred interface_to_string(mlds_interface_id::in, string::out) is det.

interface_to_string(InterfaceId, String) :-
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
    output_n_indents(Stream, Indent, !IO),
    FieldVarDefn = mlds_field_var_defn(FieldVarName, _Context, Flags,
        Type, Initializer, _),
    output_field_var_decl_flags_for_csharp(Stream, Flags, !IO),
    output_field_var_decl_for_csharp(Info, Stream, FieldVarName, Type, !IO),
    output_initializer_for_csharp(Info, Stream, oa_none, Indent + 1,
        Type, Initializer, ";", !IO).

:- pred output_field_var_decl_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, mlds_field_var_name::in, mlds_type::in,
    io::di, io::uo) is det.

output_field_var_decl_for_csharp(Info, Stream, FieldVarName, Type, !IO) :-
    output_type_for_csharp(Info, Type, Stream, !IO),
    io.write_char(Stream, ' ', !IO),
    output_field_var_name_for_csharp(Stream, FieldVarName, !IO).

%---------------------------------------------------------------------------%

:- pred output_enum_constants_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, list(mlds_field_var_defn)::in,
    io::di, io::uo) is det.

output_enum_constants_for_csharp(Info, Stream, Indent, EnumConsts, !IO) :-
    write_out_list(output_enum_constant_for_csharp(Info, Indent),
        "\n", EnumConsts, Stream, !IO),
    io.nl(Stream, !IO).

:- pred output_enum_constant_for_csharp(csharp_out_info::in,
    indent::in, mlds_field_var_defn::in, io.text_output_stream::in,
    io::di, io::uo) is det.

output_enum_constant_for_csharp(Info, Indent, FieldVarDefn, Stream, !IO) :-
    FieldVarDefn = mlds_field_var_defn(FieldVarName, _Context, _Flags,
        _Type, Initializer, _GCStmt),
    (
        Initializer = init_obj(Rval),
        % The name might require mangling.
        output_n_indents(Stream, Indent, !IO),
        output_field_var_name_for_csharp(Stream, FieldVarName, !IO),
        io.write_string(Stream, " = ", !IO),
        ( if
            Rval = ml_const(mlconst_enum(N, _))
        then
            io.write_int(Stream, N, !IO)
        else if
            Rval = ml_const(mlconst_foreign(lang_csharp, String, Type))
        then
            io.write_string(Stream, "(", !IO),
            output_type_for_csharp(Info, Type, Stream, !IO),
            io.write_string(Stream, ") ", !IO),
            io.write_string(Stream, String, !IO)
        else
            unexpected($pred, string(Rval))
        ),
        io.write_string(Stream, ",", !IO)
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

:- pred output_field_var_decl_flags_for_csharp(io.text_output_stream::in,
    mlds_field_var_decl_flags::in, io::di, io::uo) is det.

output_field_var_decl_flags_for_csharp(Stream, Flags, !IO) :-
    io.write_string(Stream, "public ", !IO),
    output_per_instance_for_csharp(Stream, Flags ^ mfvdf_per_instance, !IO),
    output_constness_for_csharp(Stream, Flags ^ mfvdf_constness, !IO).

:- pred output_class_decl_flags_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, mlds_class_decl_flags::in, mlds_class_kind::in,
    io::di, io::uo) is det.

output_class_decl_flags_for_csharp(_Info, Stream, Flags, Kind, !IO) :-
    Flags = mlds_class_decl_flags(Access, Overridability0, Constness),
    (
        (
            % `static' keyword not allowed on enumerations.
            Kind = mlds_enum
        ;
            % `static' not wanted on classes generated for Mercury types.
            Kind = mlds_class
        ),
        PerInstance = per_instance,
        Overridability = Overridability0
    ;
        % `static' and `sealed' not wanted or allowed on structs.
        Kind = mlds_struct,
        PerInstance = per_instance,
        Overridability = overridable
    ;
        Kind = mlds_interface,
        PerInstance = one_copy,
        Overridability = Overridability0
    ),
    (
        Access = class_public,
        io.write_string(Stream, "public ", !IO)
    ;
        Access = class_private,
        io.write_string(Stream, "private ", !IO)
    ),
    output_per_instance_for_csharp(Stream, PerInstance, !IO),
    (
        Overridability = sealed,
        io.write_string(Stream, "sealed ", !IO)
    ;
        Overridability = overridable
    ),
    output_constness_for_csharp(Stream, Constness, !IO).

:- pred output_per_instance_for_csharp(io.text_output_stream::in,
    per_instance::in, io::di, io::uo) is det.

output_per_instance_for_csharp(Stream, PerInstance, !IO) :-
    (
        PerInstance = per_instance
    ;
        PerInstance = one_copy,
        io.write_string(Stream, "static ", !IO)
    ).

:- pred output_constness_for_csharp(io.text_output_stream::in, constness::in,
    io::di, io::uo) is det.

output_constness_for_csharp(Stream, Constness, !IO) :-
    (
        Constness = const,
        io.write_string(Stream, "readonly ", !IO)
    ;
        Constness = modifiable
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_class.
%---------------------------------------------------------------------------%
