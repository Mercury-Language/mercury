%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mlds_to_cs.m.
% Main authors: wangp.
%
% Convert MLDS to C# code.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.mlds_to_cs.
:- interface.

:- import_module hlds.hlds_module.
:- import_module ml_backend.mlds.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred output_csharp_mlds(module_info::in, mlds::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

    % XXX needed for c_util.output_quoted_string,
    %     c_util.output_quoted_multi_string, and
    %     c_util.make_float_literal.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.rtti.
:- import_module hlds.hlds_pred.           % for pred_proc_id.
:- import_module libs.compiler_util.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- use_module ml_backend.java_util.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_type_gen.   % for ml_gen_type_name
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds.
:- import_module ml_backend.rtti_to_mlds.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.file_names.    % for mercury_std_library_name.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module digraph.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

output_csharp_mlds(ModuleInfo, MLDS, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    ModuleName = mlds_get_module_name(MLDS),
    module_name_to_file_name(Globals, ModuleName, ".cs", do_create_dirs,
        SourceFile, !IO),
    Indent = 0,
    output_to_file(Globals, SourceFile,
        output_csharp_src_file(ModuleInfo, Indent, MLDS), !IO).

%-----------------------------------------------------------------------------%
%
% Utility predicates for various purposes.
%

    % Succeeds iff this definition is a data definition which defines RTTI.
    %
:- pred defn_is_rtti_data(mlds_defn::in) is semidet.

defn_is_rtti_data(Defn) :-
    Defn = mlds_defn(_Name, _Context, _Flags, Body),
    Body = mlds_data(Type, _, _),
    Type = mlds_rtti_type(_).

    % Succeeds iff this definition is a data definition.
    %
:- pred defn_is_data(mlds_defn::in) is semidet.

defn_is_data(Defn) :-
    Defn = mlds_defn(_Name, _Context, _Flags, Body),
    Body = mlds_data(_, _, _).

    % Succeeds iff a given string matches the unqualified interface name
    % of a interface in Mercury's C# runtime system.
    %
:- pred interface_is_special(string::in) is semidet.

interface_is_special("MercuryType").

%-----------------------------------------------------------------------------%
%
% Code to generate the `.cs' file.
%

:- pred output_csharp_src_file(module_info::in, indent::in, mlds::in,
    io::di, io::uo) is det.

output_csharp_src_file(ModuleInfo, Indent, MLDS, !IO) :-
    % Run further transformations on the MLDS.
    MLDS = mlds(ModuleName, AllForeignCode, Imports, GlobalData, Defns0,
        InitPreds, _FinalPreds, ExportedEnums),
    ml_global_data_get_all_global_defns(GlobalData,
        _ScalarCellGroupMap, _VectorCellGroupMap, GlobalDefns),
    Defns = GlobalDefns ++ Defns0,

    % Get the foreign code for C#
    % XXX We should not ignore _RevImports.
    ForeignCode = mlds_get_csharp_foreign_code(AllForeignCode),
    ForeignCode = mlds_foreign_code(RevForeignDecls, _RevImports,
        RevBodyCode, ExportDefns),
    ForeignDecls = list.reverse(RevForeignDecls),
    ForeignBodyCode = list.reverse(RevBodyCode),

    % Output transformed MLDS as C# source.
    module_info_get_globals(ModuleInfo, Globals),
    Info = init_csharp_out_info(ModuleInfo),
    output_src_start(Globals, Info, Indent, ModuleName, Imports, ForeignDecls,
        Defns, !IO),
    io.write_list(ForeignBodyCode, "\n", output_csharp_body_code(Info, Indent),
        !IO),

    list.filter(defn_is_rtti_data, Defns, RttiDefns, NonRttiDefns),

    io.write_string("\n// RttiDefns\n", !IO),
    output_defns(Info, Indent + 1, alloc_only, RttiDefns, !IO),
    output_rtti_assignments(Info, Indent + 1, RttiDefns, !IO),

    list.filter(defn_is_data, NonRttiDefns, DataDefns, NonDataDefns),
    io.write_string("\n// DataDefns\n", !IO),
    output_data_decls(Info, Indent + 1, DataDefns, !IO),
    output_init_data_method(Info, Indent + 1, DataDefns, !IO),

    % Scalar common data must appear after the previous data definitions,
    % and the vector common data after that.
    io.write_string("\n// Scalar common data\n", !IO),
    % output_scalar_common_data(Info, Indent + 1, ScalarCellGroupMap, !IO),

    io.write_string("\n// Vector common data\n", !IO),
    % output_vector_common_data(Info, Indent + 1, VectorCellGroupMap, !IO),

    io.write_string("\n// NonDataDefns\n", !IO),
    output_defns(Info, Indent + 1, none, NonDataDefns, !IO),

    io.write_string("\n// ExportDefns\n", !IO),
    output_exports(Info, Indent + 1, ExportDefns, !IO),

    io.write_string("\n// ExportedEnums\n", !IO),
    output_exported_enums(Info, Indent + 1, ExportedEnums, !IO),

    % io.write_string("\n// FinalPreds\n", !IO),
    % output_finals(Indent + 1, FinalPreds, !IO),

    io.write_string("\n// EnvVarNames\n", !IO),
    output_env_vars(Indent + 1, NonRttiDefns, !IO),

    StaticCtorCalls = ["MR_init_rtti", "MR_init_data" | InitPreds],
    output_static_constructor(ModuleName, Indent + 1, StaticCtorCalls, !IO),

    output_src_end(Indent, ModuleName, !IO).

%-----------------------------------------------------------------------------%
%
% Code for working with `foreign_code'.
%

:- pred output_csharp_decl(csharp_out_info::in, indent::in,
    foreign_decl_code::in, io::di, io::uo) is det.

output_csharp_decl(Info, Indent, DeclCode, !IO) :-
    DeclCode = foreign_decl_code(Lang, _IsLocal, Code, Context),
    (
        Lang = lang_csharp,
        indent_line(Info, mlds_make_context(Context), Indent, !IO),
        io.write_string(Code, !IO),
        io.nl(!IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_java
        ; Lang = lang_il
        ; Lang = lang_erlang
        ),
        sorry(this_file, "foreign decl other than C#")
    ).

:- pred output_csharp_body_code(csharp_out_info::in, indent::in,
    user_foreign_code::in, io::di, io.state::uo) is det.

output_csharp_body_code(Info, Indent, UserForeignCode, !IO) :-
    UserForeignCode = user_foreign_code(Lang, Code, Context),
    % Only output C# code.
    (
        Lang = lang_csharp,
        indent_line(Info, mlds_make_context(Context), Indent, !IO),
        io.write_string(Code, !IO),
        io.nl(!IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_java
        ; Lang = lang_il
        ; Lang = lang_erlang
        ),
        sorry(this_file, "foreign code other than C#")
    ).

:- func mlds_get_csharp_foreign_code(map(foreign_language, mlds_foreign_code))
    = mlds_foreign_code.

mlds_get_csharp_foreign_code(AllForeignCode) = ForeignCode :-
    ( map.search(AllForeignCode, lang_csharp, ForeignCode0) ->
        ForeignCode = ForeignCode0
    ;
        ForeignCode = mlds_foreign_code([], [], [], [])
    ).

%-----------------------------------------------------------------------------%
%
% Code for handling `pragma foreign_export'.
%

    % Exports are converted into forwarding methods that are given the
    % specified name.  These simply call the exported procedure.
    %
    % NOTE: the forwarding methods must be declared public as they might
    % be referred to within foreign_procs that are inlined across module
    % boundaries.
    %
:- pred output_exports(csharp_out_info::in, indent::in,
    list(mlds_pragma_export)::in, io::di, io::uo) is det.

output_exports(Info, Indent, Exports, !IO) :-
    list.foldl(output_export(Info, Indent), Exports, !IO).

:- pred output_export(csharp_out_info::in, indent::in, mlds_pragma_export::in,
    io::di, io::uo) is det.

output_export(Info0, Indent, Export, !IO) :-
    Export = ml_pragma_export(Lang, ExportName, _, MLDS_Signature,
        _UnivQTVars, _),
    expect(unify(Lang, lang_csharp), this_file,
        "foreign_export for language other than C#."),

    indent_line(Indent, !IO),
    io.write_string("public static ", !IO),
    % XXX C# has generics
    % output_generic_tvars(UnivQTVars, !IO),
    io.nl(!IO),
    indent_line(Indent, !IO),

    MLDS_Signature = mlds_func_params(_Parameters, ReturnTypes),
    Info = Info0,
    (
        ReturnTypes = [],
        io.write_string("void", !IO)
    ;
        ReturnTypes = [RetType],
        output_type(Info, RetType, !IO)
    ;
        ReturnTypes = [_, _ | _],
        % For multiple outputs, we return an array of objects.
        % XXX C# has output parameters
        io.write_string("object []", !IO)
    ),
    io.write_string(" " ++ ExportName, !IO),
    output_export_no_ref_out(Info, Indent, Export, !IO).

:- pred output_export_no_ref_out(csharp_out_info::in, indent::in,
    mlds_pragma_export::in, io::di, io::uo) is det.

output_export_no_ref_out(Info, Indent, Export, !IO) :-
    Export = ml_pragma_export(_Lang, _ExportName, MLDS_Name, MLDS_Signature,
        _UnivQTVars, _MLDS_Context),
    MLDS_Signature = mlds_func_params(Parameters, ReturnTypes),
    output_params(Info, Indent + 1, Parameters, !IO),
    io.nl(!IO),
    indent_line(Indent, !IO),
    io.write_string("{\n", !IO),
    indent_line(Indent + 1, !IO),
    (
        ReturnTypes = []
    ;
        ReturnTypes = [RetType],
        % The cast is required when the exported method uses generics but the
        % underlying method does not use generics (i.e. returns Object).
        io.write_string("return (", !IO),
        output_type(Info, RetType, !IO),
        io.write_string(") ", !IO)
    ;
        ReturnTypes = [_, _ | _],
        io.write_string("return ", !IO)
    ),
    write_export_call(MLDS_Name, Parameters, !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred write_export_call(mlds_qualified_entity_name::in,
    list(mlds_argument)::in, io::di, io::uo) is det.

write_export_call(MLDS_Name, Parameters, !IO) :-
    output_fully_qualified_thing(MLDS_Name, output_name, !IO),
    io.write_char('(', !IO),
    io.write_list(Parameters, ", ", write_argument_name, !IO),
    io.write_string(");\n", !IO).

:- pred write_argument_name(mlds_argument::in, io::di, io::uo) is det.

write_argument_name(Arg, !IO) :-
    Arg = mlds_argument(Name, _, _),
    output_name(Name, !IO).

%-----------------------------------------------------------------------------%
%
% Code for handling `pragma foreign_export_enum'.
%

:- pred output_exported_enums(csharp_out_info::in, indent::in,
    list(mlds_exported_enum)::in, io::di, io::uo) is det.

output_exported_enums(Info, Indent, ExportedEnums, !IO) :-
    list.foldl(output_exported_enum(Info, Indent), ExportedEnums, !IO).

:- pred output_exported_enum(csharp_out_info::in, indent::in,
    mlds_exported_enum::in, io::di, io::uo) is det.

output_exported_enum(Info, Indent, ExportedEnum, !IO) :-
    ExportedEnum = mlds_exported_enum(Lang, _, TypeCtor, ExportedConstants0),
    (
        Lang = lang_csharp,
        ml_gen_type_name(TypeCtor, ClassName, ClassArity),
        MLDS_Type = mlds_class_type(ClassName, ClassArity, mlds_enum),
        % We reverse the list so the constants are printed out in order.
        list.reverse(ExportedConstants0, ExportedConstants),
        list.foldl(output_exported_enum_constant(Info, Indent, MLDS_Type),
            ExportedConstants, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_java
        ; Lang = lang_il
        ; Lang = lang_erlang
        )
    ).

:- pred output_exported_enum_constant(csharp_out_info::in, indent::in,
    mlds_type::in, mlds_exported_enum_constant::in, io::di, io::uo) is det.

output_exported_enum_constant(Info, Indent, MLDS_Type, ExportedConstant,
        !IO) :-
    ExportedConstant = mlds_exported_enum_constant(Name, Initializer),
    indent_line(Indent, !IO),
    io.write_string("public static readonly ", !IO),
    output_type(Info, MLDS_Type, !IO),
    io.write_string(" ", !IO),
    io.write_string(Name, !IO),
    io.write_string(" = ", !IO),
    output_initializer_body(Info, Initializer, no, !IO),
    io.write_string(";\n", !IO).

%-----------------------------------------------------------------------------%
%
% Code to output globals for environment variables.
%

:- pred output_env_vars(indent::in, list(mlds_defn)::in, io::di, io::uo)
    is det.

output_env_vars(Indent, NonRttiDefns, !IO) :-
    list.foldl(collect_env_var_names, NonRttiDefns, set.init, EnvVarNamesSet),
    EnvVarNames = set.to_sorted_list(EnvVarNamesSet),
    (
        EnvVarNames = []
    ;
        EnvVarNames = [_ | _],
        list.foldl(output_env_var_definition(Indent), EnvVarNames, !IO)
    ).

:- pred collect_env_var_names(mlds_defn::in,
    set(string)::in, set(string)::out) is det.

collect_env_var_names(Defn, !EnvVarNames) :-
    Defn = mlds_defn(_, _, _, EntityDefn),
    (
        EntityDefn = mlds_data(_, _, _)
    ;
        EntityDefn = mlds_function(_, _, _, _, EnvVarNames),
        set.union(EnvVarNames, !EnvVarNames)
    ;
        EntityDefn = mlds_class(_)
    ).

:- pred output_env_var_definition(indent::in, string::in, io::di, io::uo)
    is det.

output_env_var_definition(Indent, EnvVarName, !IO) :-
    % We use int because the generated code compares against zero, and changing
    % that is more trouble than it's worth as it affects the C backends.
    indent_line(Indent, !IO),
    io.write_string("private static int mercury_envvar_", !IO),
    io.write_string(EnvVarName, !IO),
    io.write_string(" =\n", !IO),
    indent_line(Indent + 1, !IO),
    io.write_string("System.Environment.GetEnvironmentVariable(\"", !IO),
    io.write_string(EnvVarName, !IO),
    io.write_string("\") == null ? 0 : 1;\n", !IO).

%-----------------------------------------------------------------------------%
%
% Code to output the start and end of a source file.
%

:- pred output_src_start(globals::in, csharp_out_info::in, indent::in,
    mercury_module_name::in, mlds_imports::in, list(foreign_decl_code)::in,
    list(mlds_defn)::in, io::di, io::uo) is det.

output_src_start(Globals, Info, Indent, MercuryModuleName, _Imports,
        ForeignDecls, Defns, !IO) :-
    output_auto_gen_comment(Globals, MercuryModuleName, !IO),
    indent_line(Indent, !IO),
    io.write_string("/* :- module ", !IO),
    prog_out.write_sym_name(MercuryModuleName, !IO),
    io.write_string(". */\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("namespace mercury {\n\n", !IO),

    io.write_list(ForeignDecls, "\n", output_csharp_decl(Info, Indent), !IO),
    io.write_string("public static class ", !IO),
    mangle_sym_name_for_csharp(MercuryModuleName, module_qual, "__",
        ClassName),
    io.write_string(ClassName, !IO),
    io.write_string(" {\n", !IO),

    % output_debug_class_init(MercuryModuleName, "start", !IO),

    % Check if this module contains a `main' predicate and if it does insert
    % a `main' method in the resulting source file that calls the `main'
    % predicate.
    ( defns_contain_main(Defns) ->
        write_main_driver(Indent + 1, ClassName, !IO)
    ;
        true
    ).

    % C# only allows a single static constructor so we just call the real
    % methods that we generated earlier.
    %
:- pred output_static_constructor(mercury_module_name::in, indent::in,
    list(string)::in, io::di, io::uo) is det.

output_static_constructor(MercuryModuleName, Indent, StaticConstructors,
        !IO) :-
    indent_line(Indent, !IO),
    io.write_string("static ", !IO),
    mangle_sym_name_for_csharp(MercuryModuleName, module_qual, "__",
        ClassName),
    io.write_string(ClassName, !IO),
    io.write_string("() {\n", !IO),
    WriteCall = (pred(MethodName::in, !.IO::di, !:IO::uo) is det :-
        indent_line(Indent + 1, !IO),
        io.write_string(MethodName, !IO),
        io.write_string("();\n", !IO)
    ),
    list.foldl(WriteCall, StaticConstructors, !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred write_main_driver(indent::in, string::in, io::di, io::uo) is det.

write_main_driver(Indent, ClassName, !IO) :-
    indent_line(Indent, !IO),
    io.write_string("public static void Main", !IO),
    io.write_string("(string[] args)\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("{\n", !IO),

    % XXX handle command line arguments and exit status
    Body = [
        "   " ++ ClassName ++ ".main_2_p_0();"
    ],
    list.foldl(write_indented_line(Indent + 1), Body, !IO),

    indent_line(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred write_indented_line(indent::in, string::in, io::di, io::uo) is det.

write_indented_line(Indent, Line, !IO) :-
    indent_line(Indent, !IO),
    io.write_string(Line, !IO),
    io.nl(!IO).

:- pred output_src_end(indent::in, mercury_module_name::in, io::di, io::uo)
    is det.

output_src_end(Indent, ModuleName, !IO) :-
    io.write_string("}\n\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("// :- end_module ", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(".\n", !IO).

    % Output a comment saying that the file was automatically
    % generated and give details such as the compiler version.
    %
:- pred output_auto_gen_comment(globals::in, mercury_module_name::in,
    io::di, io::uo) is det.

output_auto_gen_comment(Globals, ModuleName, !IO)  :-
    library.version(Version),
    module_name_to_file_name(Globals, ModuleName, ".m", do_not_create_dirs,
        SourceFileName, !IO),
    io.write_string("//\n//\n// Automatically generated from ", !IO),
    io.write_string(SourceFileName, !IO),
    io.write_string(" by the Mercury Compiler,\n", !IO),
    io.write_string("// version ", !IO),
    io.write_string(Version, !IO),
    io.nl(!IO),
    io.write_string("//\n", !IO),
    io.write_string("//\n", !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
%
% Code to output declarations and definitions.
%

    % Options to adjust the behaviour of the output predicates.
    %
:- type output_aux
    --->    none
            % Nothing special.

    ;       cname(mlds_entity_name)
            % Pass down the class name if a definition is a constructor; this
            % is needed since the class name is not available for a constructor
            % in the MLDS.

    ;       alloc_only
            % When writing out RTTI structure definitions, initialise members
            % with allocated top-level structures but don't fill in the fields
            % yet.

    ;       force_init.
            % Used to force local variables to be initialised even if an
            % initialiser is not provided.

:- pred output_defns(csharp_out_info::in, indent::in, output_aux::in,
    list(mlds_defn)::in, io::di, io::uo) is det.

output_defns(Info, Indent, OutputAux, Defns, !IO) :-
    list.foldl(output_defn(Info, Indent, OutputAux), Defns, !IO).

:- pred output_defn(csharp_out_info::in, indent::in, output_aux::in,
    mlds_defn::in, io::di, io::uo) is det.

output_defn(Info, Indent, OutputAux, Defn, !IO) :-
    Defn = mlds_defn(Name, Context, Flags, DefnBody),
    indent_line(Info, Context, Indent, !IO),
    ( DefnBody = mlds_function(_, _, body_external, _, _) ->
        % This is just a function declaration, with no body.
        % C# doesn't support separate declarations and definitions,
        % so just output the declaration as a comment.
        % (Note that the actual definition of an external procedure
        % must be given in `pragma foreign_code' in the same module.)
        io.write_string("/* external:\n", !IO),
        output_decl_flags(Info, Flags, !IO),
        output_defn_body(Info, Indent, Name, OutputAux, Context, DefnBody,
            !IO),
        io.write_string("*/\n", !IO)
    ;
        (
            DefnBody = mlds_class(ClassDefn),
            Kind = ClassDefn ^ mcd_kind,
            (
                % `static' keyword not allowed on enumerations.
                Kind = mlds_enum
            ;
                % `static' not wanted on classes generated for Mercury types.
                Kind = mlds_class
            )
        ->
            OverrideFlags = set_per_instance(Flags, per_instance)
        ;
            OverrideFlags = Flags
        ),
        output_decl_flags(Info, OverrideFlags, !IO),
        output_defn_body(Info, Indent, Name, OutputAux, Context, DefnBody,
            !IO)
    ).

:- pred output_defn_body(csharp_out_info::in, indent::in, mlds_entity_name::in,
    output_aux::in, mlds_context::in, mlds_entity_defn::in, io::di, io::uo)
    is det.

output_defn_body(Info, Indent, UnqualName, OutputAux, Context, Entity, !IO) :-
    (
        Entity = mlds_data(Type, Initializer, _),
        output_data_defn(Info, UnqualName, OutputAux, Type, Initializer,
            !IO)
    ;
        Entity = mlds_function(MaybePredProcId, Signature, MaybeBody,
            _Attributes, _EnvVarNames),
        output_maybe(MaybePredProcId, output_pred_proc_id(Info), !IO),
        output_func(Info, Indent, UnqualName, OutputAux, Context,
            Signature, MaybeBody, !IO)
    ;
        Entity = mlds_class(ClassDefn),
        output_class(Info, Indent, UnqualName, ClassDefn, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Code to output classes.
%

:- pred output_class(csharp_out_info::in, indent::in, mlds_entity_name::in,
    mlds_class_defn::in, io::di, io::uo) is det.

output_class(!.Info, Indent, UnqualName, ClassDefn, !IO) :-
    (
        UnqualName = entity_type(ClassNamePrime, ArityPrime),
        ClassName = ClassNamePrime,
        Arity = ArityPrime
    ;
        ( UnqualName = entity_data(_)
        ; UnqualName = entity_function(_, _, _, _)
        ; UnqualName = entity_export(_)
        ),
        unexpected(this_file, "output_class: name is not entity_type.")
    ),
    ClassDefn = mlds_class_defn(Kind, _Imports, BaseClasses, Implements,
        TypeParams, Ctors, AllMembers),

    !Info ^ oi_univ_tvars := TypeParams,

    output_class_kind(Kind, !IO),
    output_unqual_class_name(ClassName, Arity, !IO),
    OutputGenerics = !.Info ^ oi_output_generics,
    (
        OutputGenerics = do_output_generics,
        output_generic_tvars(TypeParams, !IO)
    ;
        OutputGenerics = do_not_output_generics
    ),
    io.nl(!IO),

    output_supers_list(!.Info, Indent + 1, BaseClasses, Implements, !IO),
    indent_line(Indent, !IO),
    io.write_string("{\n", !IO),
    output_class_body(!.Info, Indent + 1, Kind, UnqualName, AllMembers, !IO),
    io.nl(!IO),
    output_defns(!.Info, Indent + 1, cname(UnqualName), Ctors, !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n\n", !IO).

:- pred output_class_kind(mlds_class_kind::in, io::di, io::uo) is det.

output_class_kind(Kind, !IO) :-
    (
        Kind = mlds_interface,
        io.write_string("interface ", !IO)
    ;
        ( Kind = mlds_class
        ; Kind = mlds_package
        ; Kind = mlds_struct
        ),
        io.write_string("class ", !IO)
    ;
        Kind = mlds_enum,
        io.write_string("enum ", !IO)
    ).

:- pred output_generic_tvars(list(tvar)::in, io::di, io::uo) is det.

output_generic_tvars(Vars, !IO) :-
    (
        Vars = []
    ;
        Vars = [_ | _],
        io.write_string("<", !IO),
        io.write_list(Vars, ", ", output_generic_tvar, !IO),
        io.write_string(">", !IO)
    ).

:- pred output_generic_tvar(tvar::in, io::di, io::uo) is det.

output_generic_tvar(Var, !IO) :-
    generic_tvar_to_string(Var, VarName),
    io.write_string(VarName, !IO).

:- pred generic_tvar_to_string(tvar::in, string::out) is det.

generic_tvar_to_string(Var, VarName) :-
    varset.lookup_name(varset.init, Var, "MR_tvar_", VarName).

    % Output superclass that this class extends and interfaces implemented.
    % C# does not support multiple inheritance, so more than one superclass is
    % an error.
    %
:- pred output_supers_list(csharp_out_info::in, indent::in,
    list(mlds_class_id)::in, list(mlds_interface_id)::in,
    io::di, io::uo) is det.

output_supers_list(Info, Indent, BaseClasses, Interfaces, !IO) :-
    list.map(interface_to_string, Interfaces, Strings0),
    (
        BaseClasses = [],
        Strings = Strings0
    ;
        BaseClasses = [BaseClass],
        type_to_string(Info, BaseClass, BaseClassString, _ArrayDims),
        Strings = [BaseClassString | Strings0]
    ;
        BaseClasses = [_, _ | _],
        unexpected(this_file,
            "output_supers_list: multiple inheritance not supported")
    ),
    (
        Strings = []
    ;
        Strings = [_ | _],
        indent_line(Indent, !IO),
        io.write_string(": ", !IO),
        io.write_list(Strings, ", ", io.write_string, !IO),
        io.nl(!IO)
    ).

:- pred interface_to_string(mlds_interface_id::in, string::out) is det.

interface_to_string(Interface, String) :-
    (
        Interface = mlds_class_type(qual(ModuleQualifier, _QualKind, Name),
            Arity, _)
    ->
        SymName = mlds_module_name_to_sym_name(ModuleQualifier),
        mangle_sym_name_for_csharp(SymName, module_qual, ".", ModuleName),

        % Check if the interface is one of the ones in the runtime system.
        % If it is, we don't need to output the arity.
        ( interface_is_special(Name) ->
            String = string.format("%s.%s", [s(ModuleName), s(Name)])
        ;
            String = string.format("%s.%s%d", [s(ModuleName), s(Name),
                i(Arity)])
        )
    ;
        unexpected(this_file, "interface_to_string: interface was not a class")
    ).

:- pred output_class_body(csharp_out_info::in, indent::in, mlds_class_kind::in,
    mlds_entity_name::in, list(mlds_defn)::in, io::di, io::uo) is det.

output_class_body(Info, Indent, Kind, UnqualName, AllMembers, !IO) :-
    (
        Kind = mlds_class,
        output_defns(Info, Indent, none, AllMembers, !IO)
    ;
        Kind = mlds_package,
        unexpected(this_file, "cannot use package as a type.")
    ;
        Kind = mlds_interface,
        output_defns(Info, Indent, none, AllMembers, !IO)
    ;
        Kind = mlds_struct,
        % XXX C# is not Java
        unexpected(this_file,
            "output_class_body: structs not supported in Java.")
    ;
        Kind = mlds_enum,
        list.filter(defn_is_const, AllMembers, EnumConsts),
        output_enum_constants(Info, Indent + 1, UnqualName, EnumConsts, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Additional code for generating enumerations.
%

:- pred defn_is_const(mlds_defn::in) is semidet.

defn_is_const(Defn) :-
    Defn = mlds_defn(_Name, _Context, Flags, _DefnBody),
    constness(Flags) = const.

:- pred output_enum_constants(csharp_out_info::in, indent::in,
    mlds_entity_name::in, list(mlds_defn)::in, io::di, io::uo) is det.

output_enum_constants(Info, Indent, EnumName, EnumConsts, !IO) :-
    io.write_list(EnumConsts, "\n",
        output_enum_constant(Info, Indent, EnumName), !IO),
    io.nl(!IO).

:- pred output_enum_constant(csharp_out_info::in, indent::in,
    mlds_entity_name::in, mlds_defn::in, io::di, io::uo) is det.

output_enum_constant(_Info, Indent, _EnumName, Defn, !IO) :-
    Defn = mlds_defn(Name, _Context, _Flags, DefnBody),
    ( DefnBody = mlds_data(_Type, Initializer, _GCStatement) ->
        (
            Initializer = init_obj(Rval),
            ( Rval = ml_const(mlconst_enum(N, _)) ->
                % The name might require mangling.
                indent_line(Indent, !IO),
                output_name(Name, !IO),
                io.format(" = %d,", [i(N)], !IO)
            ;
                unexpected(this_file, "output_enum_constant: not mlconst_enum")
            )
        ;
            ( Initializer = no_initializer
            ; Initializer = init_struct(_, _)
            ; Initializer = init_array(_)
            ),
            unexpected(this_file, "output_enum_constant: not mlconst_enum")
        )
    ;
        unexpected(this_file,
            "output_enum_constant: definition body was not data.")
    ).

%-----------------------------------------------------------------------------%
%
% Code to output data declarations/definitions.
%

:- pred output_data_decls(csharp_out_info::in, indent::in, list(mlds_defn)::in,
    io::di, io::uo) is det.

output_data_decls(_, _, [], !IO).
output_data_decls(Info, Indent, [Defn | Defns], !IO) :-
    Defn = mlds_defn(Name, _Context, Flags, DefnBody),
    ( DefnBody = mlds_data(Type, _Initializer, _GCStatement) ->
        indent_line(Indent, !IO),
        % We can't honour `final' here as the variable is assigned separately.
        % XXX does this make any sense for C#?
        NonFinalFlags = set_finality(Flags, overridable),
        output_decl_flags(Info, NonFinalFlags, !IO),
        output_data_decl(Info, Name, Type, !IO),
        io.write_string(";\n", !IO)
    ;
        unexpected(this_file, "output_data_decls: not data")
    ),
    output_data_decls(Info, Indent, Defns, !IO).

:- pred output_data_decl(csharp_out_info::in, mlds_entity_name::in,
    mlds_type::in, io::di, io::uo) is det.

output_data_decl(Info, Name, Type, !IO) :-
    output_type(Info, Type, !IO),
    io.write_char(' ', !IO),
    output_name(Name, !IO).

:- pred output_init_data_method(csharp_out_info::in, indent::in,
    list(mlds_defn)::in, io::di, io::uo) is det.

output_init_data_method(Info, Indent, Defns, !IO) :-
    indent_line(Indent, !IO),
    io.write_string("private static void MR_init_data() {\n", !IO),
    output_init_data_statements(Info, Indent + 1, Defns, !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_init_data_statements(csharp_out_info::in, indent::in,
    list(mlds_defn)::in, io::di, io::uo) is det.

output_init_data_statements(_, _, [], !IO).
output_init_data_statements(Info, Indent, [Defn | Defns], !IO) :-
    Defn = mlds_defn(Name, _Context, _Flags, DefnBody),
    ( DefnBody = mlds_data(Type, Initializer, _GCStatement) ->
        indent_line(Indent, !IO),
        output_name(Name, !IO),
        output_initializer(Info, none, Type, Initializer, !IO),
        io.write_string(";\n", !IO)
    ;
        unexpected(this_file, "output_init_data_statements: not mlds_data")
    ),
    output_init_data_statements(Info, Indent, Defns, !IO).

:- pred output_data_defn(csharp_out_info::in, mlds_entity_name::in,
    output_aux::in, mlds_type::in, mlds_initializer::in, io::di, io::uo)
    is det.

output_data_defn(Info, Name, OutputAux, Type, Initializer, !IO) :-
    output_data_decl(Info, Name, Type, !IO),
    output_initializer(Info, OutputAux, Type, Initializer, !IO),
    io.write_string(";\n", !IO).

%-----------------------------------------------------------------------------%
%
% Code to output common data.
%

:- pred output_scalar_common_data(csharp_out_info::in, indent::in,
    ml_scalar_cell_map::in, io::di, io::uo) is det.

output_scalar_common_data(Info, Indent, ScalarCellGroupMap, !IO) :-
    % Elements of scalar data arrays may reference elements in higher-numbered
    % arrays, or elements of the same array, so we must initialise them
    % separately in a static initialisation block, and we must ensure that
    % elements which are referenced by other elements are initialised first.
    map.foldl3(output_scalar_defns(Info, Indent), ScalarCellGroupMap,
        digraph.init, Graph, map.init, Map, !IO),

    ( digraph.tsort(Graph, SortedScalars0) ->
        indent_line(Indent, !IO),
        io.write_string("static {\n", !IO),
        list.reverse(SortedScalars0, SortedScalars),
        list.foldl(output_scalar_init(Info, Indent + 1, Map),
            SortedScalars, !IO),
        indent_line(Indent, !IO),
        io.write_string("}\n", !IO)
    ;
        unexpected(this_file,
            "output_scalar_common_data: digraph.tsort failed")
    ).

:- pred output_scalar_defns(csharp_out_info::in, indent::in,
    ml_scalar_common_type_num::in, ml_scalar_cell_group::in,
    digraph(mlds_scalar_common)::in, digraph(mlds_scalar_common)::out,
    map(mlds_scalar_common, mlds_initializer)::in,
    map(mlds_scalar_common, mlds_initializer)::out, io::di, io::uo) is det.

output_scalar_defns(Info, Indent, TypeNum, CellGroup, !Graph, !Map, !IO) :-
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    CellGroup = ml_scalar_cell_group(Type, _InitArraySize, _Counter, _Members,
        RowInitsCord),
    ArrayType = mlds_array_type(Type),
    RowInits = cord.list(RowInitsCord),

    indent_line(Indent, !IO),
    io.write_string("private static final ", !IO),
    output_type(Info, Type, !IO),
    io.format("[] MR_scalar_common_%d = ", [i(TypeRawNum)], !IO),
    output_initializer_alloc_only(Info, init_array(RowInits), yes(ArrayType),
        !IO),
    io.write_string(";\n", !IO),

    MLDS_ModuleName = Info ^ oi_module_name,
    list.foldl3(add_scalar_inits(MLDS_ModuleName, Type, TypeNum),
        RowInits, 0, _, !Graph, !Map).

:- pred add_scalar_inits(mlds_module_name::in, mlds_type::in,
    ml_scalar_common_type_num::in, mlds_initializer::in, int::in, int::out,
    digraph(mlds_scalar_common)::in, digraph(mlds_scalar_common)::out,
    map(mlds_scalar_common, mlds_initializer)::in,
    map(mlds_scalar_common, mlds_initializer)::out) is det.

add_scalar_inits(MLDS_ModuleName, Type, TypeNum, Initializer,
        RowNum, RowNum + 1, !Graph, !Map) :-
    Scalar = ml_scalar_common(MLDS_ModuleName, Type, TypeNum, RowNum),
    svmap.det_insert(Scalar, Initializer, !Map),
    digraph.add_vertex(Scalar, _Key, !Graph),
    add_scalar_deps(Scalar, Initializer, !Graph).

:- pred add_scalar_deps(mlds_scalar_common::in, mlds_initializer::in,
    digraph(mlds_scalar_common)::in, digraph(mlds_scalar_common)::out) is det.

add_scalar_deps(FromScalar, Initializer, !Graph) :-
    (
        Initializer = init_obj(Rval),
        add_scalar_deps_rval(FromScalar, Rval, !Graph)
    ;
        Initializer = init_struct(_Type, Initializers),
        list.foldl(add_scalar_deps(FromScalar), Initializers, !Graph)
    ;
        Initializer = init_array(Initializers),
        list.foldl(add_scalar_deps(FromScalar), Initializers, !Graph)
    ;
        Initializer = no_initializer
    ).

:- pred add_scalar_deps_rval(mlds_scalar_common::in, mlds_rval::in,
    digraph(mlds_scalar_common)::in, digraph(mlds_scalar_common)::out) is det.

add_scalar_deps_rval(FromScalar, Rval, !Graph) :-
    (
        ( Rval = ml_mkword(_, RvalA)
        ; Rval = ml_unop(_, RvalA)
        ; Rval = ml_vector_common_row(_, RvalA)
        ),
        add_scalar_deps_rval(FromScalar, RvalA, !Graph)
    ;
        Rval = ml_binop(_, RvalA, RvalB),
        add_scalar_deps_rval(FromScalar, RvalA, !Graph),
        add_scalar_deps_rval(FromScalar, RvalB, !Graph)
    ;
        Rval = ml_const(RvalConst),
        add_scalar_deps_rval_const(FromScalar, RvalConst, !Graph)
    ;
        Rval = ml_scalar_common(ToScalar),
        digraph.add_vertices_and_edge(FromScalar, ToScalar, !Graph)
    ;
        Rval = ml_self(_)
    ;
        ( Rval = ml_lval(_Lval)
        ; Rval = ml_mem_addr(_Lval)
        ),
        unexpected(this_file, "add_scalar_deps_rval: lval")
    ).

:- pred add_scalar_deps_rval_const(mlds_scalar_common::in, mlds_rval_const::in,
    digraph(mlds_scalar_common)::in, digraph(mlds_scalar_common)::out) is det.

add_scalar_deps_rval_const(FromScalar, RvalConst, !Graph) :-
    (
        RvalConst = mlconst_data_addr(data_addr(_, DataName)),
        (
            DataName = mlds_scalar_common_ref(ToScalar),
            digraph.add_vertices_and_edge(FromScalar, ToScalar, !Graph)
        ;
            ( DataName = mlds_data_var(_)
            ; DataName = mlds_rtti(_)
            ; DataName = mlds_module_layout
            ; DataName = mlds_proc_layout(_)
            ; DataName = mlds_internal_layout(_, _)
            ; DataName = mlds_tabling_ref(_, _)
            )
        )
    ;
        ( RvalConst = mlconst_true
        ; RvalConst = mlconst_false
        ; RvalConst = mlconst_int(_)
        ; RvalConst = mlconst_enum(_, _)
        ; RvalConst = mlconst_char(_)
        ; RvalConst = mlconst_float(_)
        ; RvalConst = mlconst_string(_)
        ; RvalConst = mlconst_multi_string(_)
        ; RvalConst = mlconst_foreign(_, _, _)
        ; RvalConst = mlconst_named_const(_)
        ; RvalConst = mlconst_code_addr(_)
        ; RvalConst = mlconst_null(_)
        )
    ).

:- pred output_scalar_init(csharp_out_info::in, indent::in,
    map(mlds_scalar_common, mlds_initializer)::in, mlds_scalar_common::in,
    io::di, io::uo) is det.

output_scalar_init(Info, Indent, Map, Scalar, !IO) :-
    map.lookup(Map, Scalar, Initializer),
    Scalar = ml_scalar_common(_, Type, TypeNum, RowNum),
    TypeNum = ml_scalar_common_type_num(TypeRawNum),
    indent_line(Indent, !IO),
    io.format("MR_scalar_common_%d[%d] = ", [i(TypeRawNum), i(RowNum)], !IO),
    output_initializer_body(Info, Initializer, yes(Type), !IO),
    io.write_string(";\n", !IO).

:- pred output_vector_common_data(csharp_out_info::in, indent::in,
    ml_vector_cell_map::in, io::di, io::uo) is det.

output_vector_common_data(Info, Indent, VectorCellGroupMap, !IO) :-
    map.foldl(output_vector_cell_group(Info, Indent), VectorCellGroupMap, !IO).

:- pred output_vector_cell_group(csharp_out_info::in, indent::in,
    ml_vector_common_type_num::in, ml_vector_cell_group::in,
    io::di, io::uo) is det.

output_vector_cell_group(Info, Indent, TypeNum, CellGroup, !IO) :-
    TypeNum = ml_vector_common_type_num(TypeRawNum),
    CellGroup = ml_vector_cell_group(Type, ClassDefn, _FieldIds, _NextRow,
        RowInits),
    output_defn(Info, Indent, none, ClassDefn, !IO),

    indent_line(Indent, !IO),
    io.write_string("private static final ", !IO),
    output_type(Info, Type, !IO),
    io.format(" MR_vector_common_%d[] = {\n", [i(TypeRawNum)], !IO),
    indent_line(Indent + 1, !IO),
    output_initializer_body_list(Info, cord.list(RowInits), !IO),
    io.nl(!IO),
    indent_line(Indent, !IO),
    io.write_string("};\n", !IO).

%-----------------------------------------------------------------------------%

    % We need to provide initializers for local variables to avoid problems
    % with undefined variables.
    %
:- func get_type_initializer(mlds_type) = string.

get_type_initializer(Type) = Initializer :-
    (
        Type = mercury_type(_, CtorCat, _),
        (
            ( CtorCat = ctor_cat_builtin(cat_builtin_int)
            ; CtorCat = ctor_cat_builtin(cat_builtin_float)
            ; CtorCat = ctor_cat_enum(_)
            ; CtorCat = ctor_cat_user(cat_user_direct_dummy)
            ),
            Initializer = "0"
        ;
            CtorCat = ctor_cat_builtin(cat_builtin_char),
            Initializer = "'\\u0000'"
        ;
            ( CtorCat = ctor_cat_builtin(cat_builtin_string)
            ; CtorCat = ctor_cat_system(_)
            ; CtorCat = ctor_cat_higher_order
            ; CtorCat = ctor_cat_tuple
            ; CtorCat = ctor_cat_builtin_dummy  % XXX might need to be 0
            ; CtorCat = ctor_cat_variable
            ; CtorCat = ctor_cat_void
            ; CtorCat = ctor_cat_user(cat_user_notag)
            ; CtorCat = ctor_cat_user(cat_user_general)
            ),
            Initializer = "null"
        )
    ;
        ( Type = mlds_native_int_type
        ; Type = mlds_native_float_type
        ),
        Initializer = "0"
    ;
        Type = mlds_native_char_type,
        Initializer = "'\\u0000'"
    ;
        Type = mlds_native_bool_type,
        Initializer = "false"
    ;
        ( Type = mlds_mercury_array_type(_)
        ; Type = mlds_cont_type(_)
        ; Type = mlds_commit_type
        ; Type = mlds_class_type(_, _, _)
        ; Type = mlds_array_type(_)
        ; Type = mlds_ptr_type(_)
        ; Type = mlds_func_type(_)
        ; Type = mlds_generic_type
        ; Type = mlds_generic_env_ptr_type
        ; Type = mlds_type_info_type
        ; Type = mlds_pseudo_type_info_type
        ; Type = mlds_rtti_type(_)
        ; Type = mlds_tabling_type(_)
        ),
        Initializer = "null"
    ;
        Type = mlds_foreign_type(ForeignType),
        (
            % XXX Value types must be initialised differently to reference
            % types. Here we support a "valuetype" prefix in foreign types,
            % even though it is not valid C# syntax. In the future, we may
            % want to introduce a foreign_type attribute instead.
            ForeignType = csharp(csharp_type(CsharpType)),
            ( string.append("valuetype ", Name, CsharpType) ->
                Initializer = "new " ++ Name ++ "()"
            ;
                Initializer = "null"
            )
        ;
            ( ForeignType = il(_)
            ; ForeignType = c(_)
            ; ForeignType = java(_)
            ; ForeignType = erlang(_)
            ),
            unexpected(this_file,
                "get_type_initializer: wrong foreign language type")
        )
    ;
        Type = mlds_unknown_type,
        unexpected(this_file,
            "get_type_initializer: variable has unknown_type")
    ).

:- pred output_maybe(maybe(T)::in,
    pred(T, io, io)::pred(in, di, uo) is det, io::di, io::uo) is det.

output_maybe(MaybeValue, OutputAction, !IO) :-
    (
        MaybeValue = yes(Value),
        OutputAction(Value, !IO)
    ;
        MaybeValue = no
    ).

%-----------------------------------------------------------------------------%

:- pred output_initializer(csharp_out_info::in, output_aux::in, mlds_type::in,
    mlds_initializer::in, io::di, io::uo) is det.

output_initializer(Info, OutputAux, Type, Initializer, !IO) :-
    NeedsInit = needs_initialization(Initializer),
    (
        NeedsInit = yes,
        io.write_string(" = ", !IO),
        % Due to cyclic references, we need to separate the allocation and
        % initialisation steps of RTTI structures.  If InitStyle is alloc_only
        % then we output an initializer to allocate a structure without filling
        % in the fields.
        (
            ( OutputAux = none
            ; OutputAux = cname(_)
            ; OutputAux = force_init
            ),
            output_initializer_body(Info, Initializer, yes(Type), !IO)
        ;
            OutputAux = alloc_only,
            output_initializer_alloc_only(Info, Initializer, yes(Type), !IO)
        )
    ;
        NeedsInit = no,
        (
            OutputAux = force_init,
            % Local variables need to be initialised to avoid warnings.
            io.write_string(" = ", !IO),
            io.write_string(get_type_initializer(Type), !IO)
        ;
            ( OutputAux = none
            ; OutputAux = cname(_)
            ; OutputAux = alloc_only
            )
        )
    ).

:- func needs_initialization(mlds_initializer) = bool.

needs_initialization(no_initializer) = no.
needs_initialization(init_obj(_)) = yes.
needs_initialization(init_struct(_, _)) = yes.
needs_initialization(init_array(_)) = yes.

:- pred output_initializer_alloc_only(csharp_out_info::in, mlds_initializer::in,
    maybe(mlds_type)::in, io::di, io::uo) is det.

output_initializer_alloc_only(Info, Initializer, MaybeType, !IO) :-
    (
        Initializer = no_initializer,
        unexpected(this_file, "output_initializer_alloc_only: no_initializer")
    ;
        Initializer = init_obj(_),
        unexpected(this_file, "output_initializer_alloc_only: init_obj")
    ;
        Initializer = init_struct(StructType, FieldInits),
        io.write_string("new ", !IO),
        (
            StructType = mercury_type(_Type, CtorCat, _),
            type_category_is_array(CtorCat) = is_array
        ->
            Size = list.length(FieldInits),
            io.format("object[%d]", [i(Size)], !IO)
        ;
            output_type(Info, StructType, !IO),
            io.write_string("()", !IO)
        )
    ;
        Initializer = init_array(ElementInits),
        Size = list.length(ElementInits),
        io.write_string("new ", !IO),
        (
            MaybeType = yes(Type),
            type_to_string(Info, Type, String, ArrayDims),
            io.write_string(String, !IO),
            % Replace the innermost array dimension by the known size.
            ( list.split_last(ArrayDims, Heads, 0) ->
                output_array_dimensions(Heads ++ [Size], !IO)
            ;
                unexpected(this_file,
                    "output_initializer_alloc_only: missing array dimension")
            )
        ;
            MaybeType = no,
            % XXX we need to know the type here
            io.format("/* XXX init_array */ object[%d]", [i(Size)], !IO)
        )
    ).

:- pred output_initializer_body(csharp_out_info::in, mlds_initializer::in,
    maybe(mlds_type)::in, io::di, io::uo) is det.

output_initializer_body(Info, Initializer, MaybeType, !IO) :-
    (
        Initializer = no_initializer,
        unexpected(this_file, "output_initializer_body: no_initializer")
    ;
        Initializer = init_obj(Rval),
        output_rval(Info, Rval, !IO)
    ;
        Initializer = init_struct(StructType, FieldInits),
        io.write_string("new ", !IO),
        output_type(Info, StructType, !IO),
        IsArray = type_is_array(StructType),
        io.write_string(if IsArray = is_array then " {" else "(", !IO),
        output_initializer_body_list(Info, FieldInits, !IO),
        io.write_char(if IsArray = is_array then '}' else ')', !IO)
    ;
        Initializer = init_array(ElementInits),
        io.write_string("new ", !IO),
        (
            MaybeType = yes(Type),
            output_type(Info, Type, !IO)
        ;
            MaybeType = no,
            % XXX we need to know the type here
            io.write_string("/* XXX init_array */ object[]", !IO)
        ),
        io.write_string(" {\n\t\t", !IO),
        output_initializer_body_list(Info, ElementInits, !IO),
        io.write_string("}", !IO)
    ).

:- pred output_initializer_body_list(csharp_out_info::in,
    list(mlds_initializer)::in, io::di, io::uo) is det.

output_initializer_body_list(Info, Inits, !IO) :-
    io.write_list(Inits, ",\n\t\t",
        (pred(Init::in, !.IO::di, !:IO::uo) is det :-
            output_initializer_body(Info, Init, no, !IO)),
        !IO).

%-----------------------------------------------------------------------------%
%
% Code to output RTTI data assignments.
%

:- pred output_rtti_assignments(csharp_out_info::in, indent::in,
    list(mlds_defn)::in, io::di, io::uo) is det.

output_rtti_assignments(Info, Indent, Defns, !IO) :-
    indent_line(Indent, !IO),
    io.write_string("static void MR_init_rtti() {\n", !IO),
    OrderedDefns = order_mlds_rtti_defns(Defns),
    list.foldl(output_rtti_defns_assignments(Info, Indent + 1),
        OrderedDefns, !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_rtti_defns_assignments(csharp_out_info::in, indent::in,
    list(mlds_defn)::in, io::di, io::uo) is det.

output_rtti_defns_assignments(Info, Indent, Defns, !IO) :-
    % Separate cliques.
    indent_line(Indent, !IO),
    io.write_string("//\n", !IO),
    list.foldl(output_rtti_defn_assignments(Info, Indent),
        Defns, !IO).

:- pred output_rtti_defn_assignments(csharp_out_info::in, indent::in,
    mlds_defn::in, io::di, io::uo) is det.

output_rtti_defn_assignments(Info, Indent, Defn, !IO) :-
    Defn = mlds_defn(Name, _Context, _Flags, DefnBody),
    (
        DefnBody = mlds_data(_Type, Initializer, _),
        output_rtti_defn_assignments_2(Info, Indent, Name, Initializer, !IO)
    ;
        ( DefnBody = mlds_function(_, _, _, _, _)
        ; DefnBody = mlds_class(_)
        ),
        unexpected(this_file,
            "output_rtti_defn_assignments: expected mlds_data")
    ).

:- pred output_rtti_defn_assignments_2(csharp_out_info::in, indent::in,
    mlds_entity_name::in, mlds_initializer::in, io::di, io::uo) is det.

output_rtti_defn_assignments_2(Info, Indent, Name, Initializer, !IO) :-
    (
        Initializer = no_initializer
    ;
        Initializer = init_obj(_),
        % Not encountered in practice.
        unexpected(this_file, "output_rtti_defn_assignments_2: init_obj")
    ;
        Initializer = init_struct(StructType, FieldInits),
        IsArray = type_is_array(StructType),
        (
            IsArray = not_array,
            indent_line(Indent, !IO),
            output_name(Name, !IO),
            io.write_string(".init(", !IO),
            output_initializer_body_list(Info, FieldInits, !IO),
            io.write_string(");\n", !IO)
        ;
            IsArray = is_array,
            % Not encountered in practice.
            unexpected(this_file, "output_rtti_defn_assignments_2: is_array")
        )
    ;
        Initializer = init_array(ElementInits),
        list.foldl2(output_rtti_array_assignments(Info, Indent, Name),
            ElementInits, 0, _Index, !IO)
    ).

:- pred output_rtti_array_assignments(csharp_out_info::in, indent::in,
    mlds_entity_name::in, mlds_initializer::in, int::in, int::out,
    io::di, io::uo) is det.

output_rtti_array_assignments(Info, Indent, Name, ElementInit,
        Index, Index + 1, !IO) :-
    indent_line(Indent, !IO),
    output_name(Name, !IO),
    io.write_string("[", !IO),
    io.write_int(Index, !IO),
    io.write_string("] = ", !IO),
    output_initializer_body(Info, ElementInit, no, !IO),
    io.write_string(";\n", !IO).

%-----------------------------------------------------------------------------%
%
% Code to output function declarations/definitions.
%

:- pred output_pred_proc_id(csharp_out_info::in, pred_proc_id::in,
    io::di, io::uo) is det.

output_pred_proc_id(Info, proc(PredId, ProcId), !IO) :-
    AutoComments = Info ^ oi_auto_comments,
    (
        AutoComments = yes,
        io.write_string("// pred_id: ", !IO),
        pred_id_to_int(PredId, PredIdNum),
        io.write_int(PredIdNum, !IO),
        io.write_string(", proc_id: ", !IO),
        proc_id_to_int(ProcId, ProcIdNum),
        io.write_int(ProcIdNum, !IO),
        io.nl(!IO)
    ;
        AutoComments = no
    ).

:- pred output_func(csharp_out_info::in, indent::in, mlds_entity_name::in,
    output_aux::in, mlds_context::in,
    mlds_func_params::in, mlds_function_body::in, io::di, io::uo) is det.

output_func(Info, Indent, Name, OutputAux, Context, Signature, MaybeBody,
        !IO) :-
    (
        MaybeBody = body_defined_here(Body),
        output_func_decl(Info, Indent, Name, OutputAux, Signature, !IO),
        io.write_string("\n", !IO),
        indent_line(Info, Context, Indent, !IO),
        io.write_string("{\n", !IO),
        FuncInfo = func_info(Signature),
        output_statement(Info, Indent + 1, FuncInfo, Body, _ExitMethods, !IO),
        indent_line(Info, Context, Indent, !IO),
        io.write_string("}\n", !IO)    % end the function
    ;
        MaybeBody = body_external
    ).

:- pred output_func_decl(csharp_out_info::in, indent::in, mlds_entity_name::in,
    output_aux::in, mlds_func_params::in, io::di, io::uo) is det.

output_func_decl(Info, Indent, Name, OutputAux, Signature, !IO) :-
    Signature = mlds_func_params(Parameters, RetTypes),
    (
        OutputAux = cname(CtorName),
        Name = entity_export("<constructor>")
    ->
        output_name(CtorName, !IO)
    ;
        output_return_types(Info, RetTypes, !IO),
        io.write_char(' ', !IO),
        output_name(Name, !IO)
    ),
    output_params(Info, Indent, Parameters, !IO).

:- pred output_return_types(csharp_out_info::in, mlds_return_types::in,
    io::di, io::uo) is det.

output_return_types(Info, RetTypes, !IO) :-
    (
        RetTypes = [],
        io.write_string("void", !IO)
    ;
        RetTypes = [RetType],
        output_type(Info, RetType, !IO)
    ;
        RetTypes = [_, _ | _],
        % For multiple outputs, we return an array of objects.
        % XXX C# has output parameters
        io.write_string("object []", !IO)
    ).

:- pred output_params(csharp_out_info::in, indent::in, mlds_arguments::in,
    io::di, io::uo) is det.

output_params(Info, Indent, Parameters, !IO) :-
    io.write_char('(', !IO),
    (
        Parameters = []
    ;
        Parameters = [_ | _],
        io.nl(!IO),
        io.write_list(Parameters, ",\n", output_param(Info, Indent + 1), !IO)
    ),
    io.write_char(')', !IO).

:- pred output_param(csharp_out_info::in, indent::in, mlds_argument::in,
    io::di, io::uo) is det.

output_param(Info, Indent, Arg, !IO) :-
    Arg = mlds_argument(Name, Type, _GCStatement),
    indent_line(Indent, !IO),
    output_type(Info, Type, !IO),
    io.write_char(' ', !IO),
    output_name(Name, !IO).

%-----------------------------------------------------------------------------%
%
% Code to output names of various entities.
%
% XXX Much of the code in this section will not work when we start enforcing
% names properly.
%

:- pred output_maybe_qualified_name(csharp_out_info::in,
    mlds_qualified_entity_name::in, io::di, io::uo) is det.

output_maybe_qualified_name(Info, QualifiedName, !IO) :-
    % Don't module qualify names which are defined in the current module.
    % This avoids unnecessary verbosity, and is also necessary in the case
    % of local variables and function parameters, which must not be qualified.
    QualifiedName = qual(ModuleName, _QualKind, Name),
    CurrentModuleName = Info ^ oi_module_name,
    ( ModuleName = CurrentModuleName ->
        output_name(Name, !IO)
    ;
        output_fully_qualified_thing(QualifiedName, output_name, !IO)
    ).

:- pred output_fully_qualified_thing(mlds_fully_qualified_name(T)::in,
    pred(T, io, io)::pred(in, di, uo) is det, io::di, io::uo) is det.

output_fully_qualified_thing(QualName, OutputFunc, !IO) :-
    QualName = qual(MLDS_ModuleName, QualKind, UnqualName),
    qualifier_to_string(MLDS_ModuleName, QualKind, QualifierString),
    io.write_string(QualifierString, !IO),
    io.write_string(".", !IO),
    OutputFunc(UnqualName, !IO).

:- pred qualifier_to_string(mlds_module_name::in, mlds_qual_kind::in,
    string::out) is det.

qualifier_to_string(MLDS_ModuleName, QualKind, String) :-
    mlds_module_name_to_package_name(MLDS_ModuleName) = OuterName,
    mlds_module_name_to_sym_name(MLDS_ModuleName) = InnerName,

    % The part of the qualifier that corresponds to a top-level class.
    % Remove the outermost mercury qualifier.
    ( strip_outermost_qualifier(OuterName, "mercury", StrippedOuterName) ->
        mangle_sym_name_for_csharp(StrippedOuterName, module_qual, "__",
            MangledOuterName)
    ;
        mangle_sym_name_for_csharp(OuterName, module_qual, "__",
            MangledOuterName)
    ),

    % The later parts of the qualifier correspond to nested classes.
    ( OuterName = InnerName ->
        MangledSuffix = ""
    ;
        remove_sym_name_prefixes(InnerName, OuterName, Suffix),
        mangle_sym_name_for_csharp(Suffix, convert_qual_kind(QualKind), ".",
            MangledSuffix0),
        MangledSuffix = "." ++ MangledSuffix0
    ),

    String = MangledOuterName ++ MangledSuffix.

:- pred remove_sym_name_prefixes(sym_name::in, sym_name::in, sym_name::out)
    is det.

remove_sym_name_prefixes(SymName0, Prefix, SymName) :-
    (
        SymName0 = qualified(Qual, Name),
        ( Qual = Prefix ->
            SymName = unqualified(Name)
        ;
            remove_sym_name_prefixes(Qual, Prefix, SymName1),
            SymName = qualified(SymName1, Name)
        )
    ;
        SymName0 = unqualified(_),
        unexpected(this_file, "remove_sym_name_prefixes: prefix not found")
    ).

:- func convert_qual_kind(mlds_qual_kind) = csj_qual_kind.

convert_qual_kind(module_qual) = module_qual.
convert_qual_kind(type_qual) = type_qual.

:- pred output_module_name(mercury_module_name::in, io::di, io::uo) is det.

output_module_name(ModuleName, !IO) :-
    io.write_string(sym_name_mangle(ModuleName), !IO).

:- pred output_unqual_class_name(mlds_class_name::in, arity::in,
    io::di, io::uo) is det.

output_unqual_class_name(Name, Arity, !IO) :-
    unqual_class_name_to_string(Name, Arity, String),
    io.write_string(String, !IO).

:- pred unqual_class_name_to_string(mlds_class_name::in, arity::in,
    string::out) is det.

unqual_class_name_to_string(Name, Arity, String) :-
    MangledName = name_mangle_no_leading_digit(Name),
    % By convention, class names should start with a capital letter.
    UppercaseMangledName = flip_initial_case(MangledName),
    String = UppercaseMangledName ++ "_" ++ string.from_int(Arity).

:- pred qual_class_name_to_string(mlds_class::in, arity::in, string::out)
    is det.

qual_class_name_to_string(QualName, Arity, String) :-
    QualName = qual(MLDS_ModuleName, QualKind, ClassName),
    (
        SymName = mlds_module_name_to_sym_name(MLDS_ModuleName),
        SymName = csharp_mercury_runtime_package_name
    ->
        % Don't mangle runtime class names.
        String = "runtime." ++ ClassName
    ;
        qualifier_to_string(MLDS_ModuleName, QualKind, QualString),
        unqual_class_name_to_string(ClassName, Arity, UnqualString),
        String = QualString ++ "." ++ UnqualString
    ).

:- pred output_name(mlds_entity_name::in, io::di, io::uo) is det.

output_name(entity_type(Name, Arity), !IO) :-
    output_unqual_class_name(Name, Arity, !IO).
output_name(entity_data(DataName), !IO) :-
    output_data_name(DataName, !IO).
output_name(entity_function(PredLabel, ProcId, MaybeSeqNum, _PredId), !IO) :-
    output_pred_label(PredLabel, !IO),
    proc_id_to_int(ProcId, ModeNum),
    io.format("_%d", [i(ModeNum)], !IO),
    (
        MaybeSeqNum = yes(SeqNum),
        io.format("_%d", [i(SeqNum)], !IO)
    ;
        MaybeSeqNum = no
    ).
output_name(entity_export(Name), !IO) :-
    io.write_string(Name, !IO).

:- pred output_pred_label(mlds_pred_label::in, io::di, io::uo) is det.

output_pred_label(mlds_user_pred_label(PredOrFunc, MaybeDefiningModule, Name,
        PredArity, _, _), !IO) :-
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
    io.format("%s_%d_%s", [s(MangledName), i(OrigArity), s(Suffix)], !IO),
    (
        MaybeDefiningModule = yes(DefiningModule),
        io.write_string("_in__", !IO),
        output_module_name(DefiningModule, !IO)
    ;
        MaybeDefiningModule = no
    ).

output_pred_label(mlds_special_pred_label(PredName, MaybeTypeModule, TypeName,
        TypeArity), !IO) :-
    MangledPredName = name_mangle_no_leading_digit(PredName),
    MangledTypeName = name_mangle(TypeName),
    io.write_string(MangledPredName, !IO),
    io.write_string("__", !IO),
    (
        MaybeTypeModule = yes(TypeModule),
        output_module_name(TypeModule, !IO),
        io.write_string("__", !IO)
    ;
        MaybeTypeModule = no
    ),
    io.format("%s_%d", [s(MangledTypeName), i(TypeArity)], !IO).

:- pred output_data_name(mlds_data_name::in, io::di, io::uo) is det.

output_data_name(mlds_data_var(VarName), !IO) :-
    output_mlds_var_name(VarName, !IO).

output_data_name(mlds_scalar_common_ref(Common), !IO) :-
    Common = ml_scalar_common(_ModuleName, _Type,
        ml_scalar_common_type_num(TypeNum), RowNum),
    io.format("MR_scalar_common_%d[%d]", [i(TypeNum), i(RowNum)], !IO).

output_data_name(mlds_rtti(RttiId), !IO) :-
    rtti.id_to_c_identifier(RttiId, RttiAddrName),
    io.write_string(RttiAddrName, !IO).
output_data_name(mlds_module_layout, !IO) :-
    unexpected(this_file, "NYI: mlds_module_layout").
output_data_name(mlds_proc_layout(_ProcLabel), !IO) :-
    unexpected(this_file, "NYI: mlds_proc_layout").
output_data_name(mlds_internal_layout(_ProcLabel, _FuncSeqNum), !IO) :-
    unexpected(this_file, "NYI: mlds_internal_layout").
output_data_name(mlds_tabling_ref(ProcLabel, Id), !IO) :-
    Prefix = tabling_info_id_str(Id) ++ "_",
    io.write_string(Prefix, !IO),
    mlds_output_proc_label(mlds_std_tabling_proc_label(ProcLabel), !IO).

:- pred output_mlds_var_name(mlds_var_name::in, io::di, io::uo) is det.

output_mlds_var_name(mlds_var_name(Name, no), !IO) :-
    output_valid_mangled_name(Name, !IO).
output_mlds_var_name(mlds_var_name(Name, yes(Num)), !IO) :-
    output_mangled_name(string.format("%s_%d", [s(Name), i(Num)]), !IO).

%-----------------------------------------------------------------------------%
%
% Code to output types.
%

:- pred output_type(csharp_out_info::in, mlds_type::in, io::di, io::uo) is det.

output_type(Info, MLDS_Type, !IO) :-
    output_type(Info, MLDS_Type, [], !IO).

:- pred output_type(csharp_out_info::in, mlds_type::in, list(int)::in,
    io::di, io::uo) is det.

output_type(Info, MLDS_Type, ArrayDims0, !IO) :-
    type_to_string(Info, MLDS_Type, String, ArrayDims),
    io.write_string(String, !IO),
    output_array_dimensions(ArrayDims ++ ArrayDims0, !IO).

:- pred output_array_dimensions(list(int)::in, io::di, io::uo) is det.

output_array_dimensions(ArrayDims, !IO) :-
    list.map(array_dimension_to_string, ArrayDims, Strings),
    list.foldr(io.write_string, Strings, !IO).

    % type_to_string(Info, MLDS_Type, String, ArrayDims)
    %
    % Generate the Java name for a type.  ArrayDims are the array dimensions to
    % be written after the type name, if any, in reverse order to that of Java
    % syntax where a non-zero integer represents a known array size and zero
    % represents an unknown array size.
    %
    % e.g. ArrayDims = [0, 3] represents the Java array `Object[3][]',
    % which should be read as `(Object[])[3]'.
    %
    % XXX yet to check this for C#
    %
:- pred type_to_string(csharp_out_info::in, mlds_type::in,
    string::out, list(int)::out) is det.

type_to_string(Info, MLDS_Type, String, ArrayDims) :-
    (
        MLDS_Type = mercury_type(Type, CtorCat, _),
        (
            % We need to handle type_info (etc.) types specially --
            % they get mapped to types in the runtime rather than
            % in private_builtin.
            hand_defined_type(Type, CtorCat, SubstituteName, ArrayDims0)
        ->
            String = SubstituteName,
            ArrayDims = ArrayDims0
        ;
            % io.state and store.store
            CtorCat = ctor_cat_builtin_dummy
        ->
            String = "/* builtin_dummy */ object",
            ArrayDims = []
        ;
            Type = c_pointer_type
        ->
            % The c_pointer type is used in the c back-end as a generic way
            % to pass foreign types to automatically generated Compare and
            % Unify code. When compiling to C# we must instead use
            % object.
            String = "/* c_pointer */ object",
            ArrayDims = []
        ;
            mercury_type_to_string(Info, Type, CtorCat, String, ArrayDims)
        )
    ;
        MLDS_Type = mlds_mercury_array_type(ElementType),
        ( ElementType = mercury_type(_, ctor_cat_variable, _) ->
            String = "System.Array",
            ArrayDims = []
        ;
            % For primitive element types we use arrays of the primitive type.
            % For non-primitive element types, we just use
            % `object []'. We used to use more specific types,
            % but then to create an array of the right type we need to use
            % reflection to determine the class of a representative element.
            % That doesn't work if the representative element is of a foreign
            % type, and has the value null.
            ( csharp_builtin_type(ElementType, _) ->
                type_to_string(Info, ElementType, String, ArrayDims0),
                ArrayDims = [0 | ArrayDims0]
            ;
                String = "object",
                ArrayDims = [0]
            )
        )
    ;
        MLDS_Type = mlds_native_int_type,
        String = "int",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_float_type,
        String = "double",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_bool_type,
        String = "bool",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_char_type,
        String = "char",
        ArrayDims = []
    ;
        MLDS_Type = mlds_foreign_type(ForeignType),
        (
            ForeignType = csharp(csharp_type(CsharpType)),
            ( string.append("valuetype ", Name, CsharpType) ->
                String = Name
            ;
                String = CsharpType
            ),
            ArrayDims = []
        ;
            ForeignType = c(_),
            unexpected(this_file, "output_type: c foreign_type")
        ;
            ForeignType = il(_),
            unexpected(this_file, "output_type: il foreign_type")
        ;
            ForeignType = java(_),
            unexpected(this_file, "output_type: java foreign_type")
        ;
            ForeignType = erlang(_),
            unexpected(this_file, "output_type: erlang foreign_type")
        )
    ;
        MLDS_Type = mlds_class_type(Name, Arity, _ClassKind),
        qual_class_name_to_string(Name, Arity, String),
        ArrayDims = []
    ;
        MLDS_Type = mlds_ptr_type(Type),
        % XXX Should we report an error here, if the type pointed to
        % is not a class type?
        type_to_string(Info, Type, String, ArrayDims)
    ;
        MLDS_Type = mlds_array_type(Type),
        type_to_string(Info, Type, String, ArrayDims0),
        ArrayDims = [0 | ArrayDims0]
    ;
        MLDS_Type = mlds_func_type(mlds_func_params(Args, RetTypes)),
        ArgTypes = list.map(func(mlds_argument(_, Type, _)) = Type, Args),
        String = method_ptr_type_to_string(Info, ArgTypes, RetTypes),
        ArrayDims = []
    ;
        MLDS_Type = mlds_generic_type,
        String = "/* generic_type */ object",
        ArrayDims = []
    ;
        MLDS_Type = mlds_generic_env_ptr_type,
        String = "/* env_ptr */ object",
        ArrayDims = []
    ;
        MLDS_Type = mlds_type_info_type,
        String = "runtime.TypeInfo",
        ArrayDims = []
    ;
        MLDS_Type = mlds_pseudo_type_info_type,
        String = "runtime.PseudoTypeInfo",
        ArrayDims = []
    ;
        MLDS_Type = mlds_cont_type(_),
        % XXX can we do better than this for C#?
        String = "/* cont_type */ object",
        ArrayDims = []
    ;
        MLDS_Type = mlds_commit_type,
        String = "runtime.Commit",
        ArrayDims = []
    ;
        MLDS_Type = mlds_rtti_type(RttiIdMaybeElement),
        rtti_id_maybe_element_csharp_type(RttiIdMaybeElement, String, IsArray),
        (
            IsArray = is_array,
            ArrayDims = [0]
        ;
            IsArray = not_array,
            ArrayDims = []
        )
    ;
        MLDS_Type = mlds_tabling_type(TablingId),
        % XXX C# is not Java
        tabling_id_java_type(TablingId, String, IsArray),
        (
            IsArray = is_array,
            ArrayDims = [0]
        ;
            IsArray = not_array,
            ArrayDims = []
        )
    ;
        MLDS_Type = mlds_unknown_type,
        unexpected(this_file, "output_type: unknown type")
    ).

:- pred mercury_type_to_string(csharp_out_info::in, mer_type::in,
    type_ctor_category::in, string::out, list(int)::out) is det.

mercury_type_to_string(Info, Type, CtorCat, String, ArrayDims) :-
    (
        CtorCat = ctor_cat_builtin(cat_builtin_char),
        String = "char",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int),
        String = "int",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        String = "string",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        String = "double",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_void,
        String = "builtin.Void_0",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_variable,
        % XXX C# has generics
        String = "object",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_tuple,
        String = "/* tuple */ object",
        ArrayDims = [0]
    ;
        CtorCat = ctor_cat_higher_order,
        String = "/* closure */ object",
        ArrayDims = [0]
    ;
        CtorCat = ctor_cat_system(_),
        mercury_type_to_string(Info, Type, ctor_cat_user(cat_user_general),
            String, ArrayDims)
    ;
        ( CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ),
        mercury_user_type_to_string(Info, Type, CtorCat, String, ArrayDims)
    ).

:- pred mercury_user_type_to_string(csharp_out_info::in, mer_type::in,
    type_ctor_category::in, string::out, list(int)::out) is det.

mercury_user_type_to_string(Info, Type, CtorCat, String, ArrayDims) :-
    ( type_to_ctor_and_args(Type, TypeCtor, ArgsTypes) ->
        ml_gen_type_name(TypeCtor, ClassName, ClassArity),
        ( CtorCat = ctor_cat_enum(_) ->
            MLDS_Type = mlds_class_type(ClassName, ClassArity, mlds_enum)
        ;
            MLDS_Type = mlds_class_type(ClassName, ClassArity, mlds_class)
        ),
        type_to_string(Info, MLDS_Type, TypeString, ArrayDims),
        OutputGenerics = Info ^ oi_output_generics,
        (
            OutputGenerics = do_output_generics,
            generic_args_types_to_string(Info, ArgsTypes, GenericsString),
            String = TypeString ++ GenericsString
        ;
            OutputGenerics = do_not_output_generics,
            String = TypeString
        )
    ;
        unexpected(this_file, "output_mercury_user_type: not a user type")
    ).

:- pred generic_args_types_to_string(csharp_out_info::in, list(mer_type)::in,
    string::out) is det.

generic_args_types_to_string(Info, ArgsTypes, String) :-
    (
        ArgsTypes = [],
        String = ""
    ;
        ArgsTypes = [_ | _],
        ToString = (pred(ArgType::in, ArgTypeString::out) is det :-
            ModuleInfo = Info ^ oi_module_info,
            MLDS_ArgType = mercury_type_to_mlds_type(ModuleInfo, ArgType),
            boxed_type_to_string(Info, MLDS_ArgType, ArgTypeString)
        ),
        list.map(ToString, ArgsTypes, ArgsTypesStrings),
        ArgsTypesString = string.join_list(", ", ArgsTypesStrings),
        String = "<" ++ ArgsTypesString ++ ">"
    ).

    % Return is_array if the corresponding C# type is an array type.
    %
:- func type_is_array(mlds_type) = is_array.

type_is_array(Type) = IsArray :-
    ( Type = mlds_array_type(_) ->
        IsArray = is_array
    ; Type = mlds_mercury_array_type(_) ->
        IsArray = is_array
    ; Type = mercury_type(_, CtorCat, _) ->
        IsArray = type_category_is_array(CtorCat)
    ; Type = mlds_rtti_type(RttiIdMaybeElement) ->
        rtti_id_maybe_element_csharp_type(RttiIdMaybeElement,
            _TypeName, IsArray)
    ;
        IsArray = not_array
    ).

    % Return is_array if the corresponding C# type is an array type.
    %
:- func type_category_is_array(type_ctor_category) = is_array.

type_category_is_array(CtorCat) = IsArray :-
    (
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_system(cat_system_type_info)
        ; CtorCat = ctor_cat_system(cat_system_type_ctor_info)
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_user(_)
        ),
        IsArray = not_array
    ;
        ( CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_system(cat_system_typeclass_info)
        ; CtorCat = ctor_cat_system(cat_system_base_typeclass_info)
        ),
        IsArray = is_array
    ).

    % hand_defined_type(Type, CtorCat, SubstituteName, ArrayDims):
    %
    % We need to handle type_info (etc.) types specially -- they get mapped
    % to types in the runtime rather than in private_builtin.
    %
:- pred hand_defined_type(mer_type::in, type_ctor_category::in, string::out,
    list(int)::out) is semidet.

hand_defined_type(Type, CtorCat, SubstituteName, ArrayDims) :-
    (
        CtorCat = ctor_cat_system(cat_system_type_info),
        SubstituteName = "runtime.TypeInfo_Struct",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_system(cat_system_type_ctor_info),
        SubstituteName = "runtime.TypeCtorInfo_Struct",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_system(cat_system_typeclass_info),
        SubstituteName = "/* typeclass_info */ object",
        ArrayDims = [0]
    ;
        CtorCat = ctor_cat_system(cat_system_base_typeclass_info),
        SubstituteName = "/* base_typeclass_info */ object",
        ArrayDims = [0]
    ;
        CtorCat = ctor_cat_user(cat_user_general),
        ( Type = type_desc_type ->
            SubstituteName = "runtime.TypeInfo_Struct"
        ; Type = pseudo_type_desc_type ->
            SubstituteName = "runtime.PseudoTypeInfo"
        ; Type = type_ctor_desc_type ->
            SubstituteName = "runtime.TypeCtorInfo_Struct"
        ;
            fail
        ),
        ArrayDims = []
    ).

:- pred boxed_type_to_string(csharp_out_info::in, mlds_type::in, string::out)
    is det.

boxed_type_to_string(Info, Type, String) :-
    type_to_string(Info, Type, String0, ArrayDims),
    list.map(array_dimension_to_string, ArrayDims, RevBrackets),
    list.reverse(RevBrackets, Brackets),
    string.append_list([String0 | Brackets], String).

:- pred array_dimension_to_string(int::in, string::out) is det.

array_dimension_to_string(N, String) :-
    ( N = 0 ->
        String = "[]"
    ;
        String = string.format("[%d]", [i(N)])
    ).

%-----------------------------------------------------------------------------%
%
% Code to output declaration specifiers.
%

:- pred output_decl_flags(csharp_out_info::in, mlds_decl_flags::in,
    io::di, io::uo) is det.

output_decl_flags(Info, Flags, !IO) :-
    output_access(Info, access(Flags), !IO),
    output_per_instance(per_instance(Flags), !IO),
    output_virtuality(Info, virtuality(Flags), !IO),
    output_finality(finality(Flags), !IO),
    output_constness(Info, constness(Flags), !IO),
    output_abstractness(abstractness(Flags), !IO).

:- pred output_access(csharp_out_info::in, access::in, io::di, io::uo) is det.

output_access(Info, Access, !IO) :-
    (
        Access = acc_public,
        io.write_string("public ", !IO)
    ;
        Access = acc_private,
        io.write_string("private ", !IO)
    ;
        Access = acc_protected,
        io.write_string("protected ", !IO)
    ;
        Access = acc_default,
        maybe_output_comment(Info, "default", !IO)
    ;
        Access = acc_local
    ).

:- pred output_per_instance(per_instance::in, io::di, io::uo) is det.

output_per_instance(PerInstance, !IO) :-
    (
        PerInstance = per_instance
    ;
        PerInstance = one_copy,
        io.write_string("static ", !IO)
    ).

:- pred output_virtuality(csharp_out_info::in, virtuality::in,
    io::di, io::uo) is det.

output_virtuality(Info, Virtual, !IO) :-
    (
        Virtual = virtual,
        maybe_output_comment(Info, "virtual", !IO)
    ;
        Virtual = non_virtual
    ).

:- pred output_finality(finality::in, io::di, io::uo) is det.

output_finality(Finality, !IO) :-
    (
        Finality = final,
        io.write_string("readonly ", !IO)
    ;
        Finality = overridable
    ).

:- pred output_constness(csharp_out_info::in, constness::in,
    io::di, io::uo) is det.

output_constness(Info, Constness, !IO) :-
    (
        Constness = const,
        maybe_output_comment(Info, "const", !IO)
    ;
        Constness = modifiable
    ).

:- pred output_abstractness(abstractness::in, io::di, io::uo) is det.

output_abstractness(Abstractness, !IO) :-
    (
        Abstractness = abstract,
        io.write_string("abstract ", !IO)
    ;
        Abstractness = concrete
    ).

:- pred maybe_output_comment(csharp_out_info::in, string::in,
    io::di, io::uo) is det.

maybe_output_comment(Info, Comment, !IO) :-
    AutoComments = Info ^ oi_auto_comments,
    (
        AutoComments = yes,
        io.write_string("/* ", !IO),
        io.write_string(Comment, !IO),
        io.write_string(" */", !IO)
    ;
        AutoComments = no
    ).

%-----------------------------------------------------------------------------%
%
% Code to output statements.
%

    % These types are used by many of the output_stmt style predicates to
    % return information about the statement's control flow,
    % i.e. about the different ways in which the statement can exit.
    % In general we only output the current statement if the previous
    % statement could complete normally (fall through).
    % We keep a set of exit methods since some statements (like an
    % if-then-else) could potentially break, and also fall through.
:- type exit_methods == set.set(exit_method).

:- type exit_method
    --->    can_break
    ;       can_continue
    ;       can_return
    ;       can_throw
    ;       can_fall_through.   % Where the instruction can complete
                                % normally and execution can continue
                                % with the following statement.

:- type code_addr_wrapper
    --->    code_addr_wrapper(
                caw_class           :: string,
                caw_ptr_num         :: maybe(int)
            ).

:- type func_info
    --->    func_info(
                func_info_params    :: mlds_func_params
            ).

:- pred output_statements(csharp_out_info::in, indent::in, func_info::in,
    list(statement)::in, exit_methods::out, io::di, io::uo) is det.

output_statements(_, _, _, [], ExitMethods, !IO) :-
    ExitMethods = set.make_singleton_set(can_fall_through).
output_statements(Info, Indent, FuncInfo, [Statement | Statements],
        ExitMethods, !IO) :-
    output_statement(Info, Indent, FuncInfo, Statement,
        StmtExitMethods, !IO),
    ( set.member(can_fall_through, StmtExitMethods) ->
        output_statements(Info, Indent, FuncInfo, Statements,
            StmtsExitMethods, !IO),
        ExitMethods0 = StmtExitMethods `set.union` StmtsExitMethods,
        ( set.member(can_fall_through, StmtsExitMethods) ->
            ExitMethods = ExitMethods0
        ;
            % If the last statement could not complete normally
            % the current block can no longer complete normally.
            ExitMethods = ExitMethods0 `set.delete` can_fall_through
        )
    ;
        % Don't output any more statements from the current list since
        % the preceeding statement cannot complete.
        ExitMethods = StmtExitMethods
    ).

:- pred output_statement(csharp_out_info::in, indent::in,
    func_info::in, statement::in, exit_methods::out, io::di, io::uo) is det.

output_statement(Info, Indent, FuncInfo,
        statement(Statement, Context), ExitMethods, !IO) :-
    output_context(Info, Context, !IO),
    output_stmt(Info, Indent, FuncInfo, Statement, Context,
        ExitMethods, !IO).

:- pred output_stmt(csharp_out_info::in, indent::in, func_info::in,
    mlds_stmt::in, mlds_context::in, exit_methods::out, io::di, io::uo) is det.

output_stmt(Info, Indent, FuncInfo, Statement, Context, ExitMethods, !IO) :-
    (
        Statement = ml_stmt_block(Defns, Statements),
        indent_line(Indent, !IO),
        io.write_string("{\n", !IO),
        (
            Defns = [_ | _],
            output_defns(Info, Indent + 1, force_init, Defns, !IO),
            io.write_string("\n", !IO)
        ;
            Defns = []
        ),
        output_statements(Info, Indent + 1, FuncInfo, Statements,
            ExitMethods, !IO),
        indent_line(Info, Context, Indent, !IO),
        io.write_string("}\n", !IO)
    ;
        Statement = ml_stmt_while(Kind, Cond, BodyStatement),
        Kind = may_loop_zero_times,
        indent_line(Indent, !IO),
        io.write_string("while (", !IO),
        output_rval(Info, Cond, !IO),
        io.write_string(")\n", !IO),
        % The contained statement is reachable iff the while statement is
        % reachable and the condition expression is not a constant expression
        % whose value is false.
        ( Cond = ml_const(mlconst_false) ->
            indent_line(Indent, !IO),
            io.write_string("{  /* Unreachable code */  }\n", !IO),
            ExitMethods = set.make_singleton_set(can_fall_through)
        ;
            output_statement(Info, Indent + 1, FuncInfo, BodyStatement,
                StmtExitMethods, !IO),
            ExitMethods = while_exit_methods(Cond, StmtExitMethods)
        )
    ;
        Statement = ml_stmt_while(Kind, Cond, BodyStatement),
        Kind = loop_at_least_once,
        indent_line(Indent, !IO),
        io.write_string("do\n", !IO),
        output_statement(Info, Indent + 1, FuncInfo, BodyStatement,
            StmtExitMethods, !IO),
        indent_line(Info, Context, Indent, !IO),
        io.write_string("while (", !IO),
        output_rval(Info, Cond, !IO),
        io.write_string(");\n", !IO),
        ExitMethods = while_exit_methods(Cond, StmtExitMethods)
    ;
        Statement = ml_stmt_if_then_else(Cond, Then0, MaybeElse),
        % We need to take care to avoid problems caused by the dangling else
        % ambiguity.
        (
            % For examples of the form
            %
            %   if (...)
            %       if (...)
            %           ...
            %   else
            %       ...
            %
            % we need braces around the inner `if', otherwise they wouldn't
            % parse they way we want them to.

            MaybeElse = yes(_),
            Then0 = statement(ml_stmt_if_then_else(_, _, no), ThenContext)
        ->
            Then = statement(ml_stmt_block([], [Then0]), ThenContext)
        ;
            Then = Then0
        ),

        indent_line(Indent, !IO),
        io.write_string("if (", !IO),
        output_rval(Info, Cond, !IO),
        io.write_string(")\n", !IO),
        output_statement(Info, Indent + 1, FuncInfo, Then,
            ThenExitMethods, !IO),
        (
            MaybeElse = yes(Else),
            indent_line(Info, Context, Indent, !IO),
            io.write_string("else\n", !IO),
            output_statement(Info, Indent + 1, FuncInfo, Else,
                ElseExitMethods, !IO),
            % An if-then-else statement can complete normally iff the
            % then-statement can complete normally or the else-statement
            % can complete normally.
            ExitMethods = ThenExitMethods `set.union` ElseExitMethods
        ;
            MaybeElse = no,
            % An if-then statement can complete normally iff it is reachable.
            ExitMethods = ThenExitMethods `set.union`
                set.make_singleton_set(can_fall_through)
        )
    ;
        Statement = ml_stmt_switch(_Type, Val, _Range, Cases, Default),
        indent_line(Info, Context, Indent, !IO),
        io.write_string("switch (", !IO),
        output_rval(Info, Val, !IO),
        io.write_string(") {\n", !IO),
        output_switch_cases(Info, Indent + 1, FuncInfo, Context, Cases,
            Default, ExitMethods, !IO),
        indent_line(Info, Context, Indent, !IO),
        io.write_string("}\n", !IO)
    ;
        Statement = ml_stmt_label(_),
        % XXX C# is not Java
        unexpected(this_file, "output_stmt: labels not supported in Java.")
    ;
        Statement = ml_stmt_goto(goto_label(_)),
        % XXX C# is not Java
        unexpected(this_file, "output_stmt: gotos not supported in Java.")
    ;
        Statement = ml_stmt_goto(goto_break),
        indent_line(Indent, !IO),
        io.write_string("break;\n", !IO),
        ExitMethods = set.make_singleton_set(can_break)
    ;
        Statement = ml_stmt_goto(goto_continue),
        indent_line(Indent, !IO),
        io.write_string("continue;\n", !IO),
        ExitMethods = set.make_singleton_set(can_continue)
    ;
        Statement = ml_stmt_computed_goto(_, _),
        % XXX C# is not Java
        unexpected(this_file,
            "output_stmt: computed gotos not supported in Java.")
    ;
        Statement = ml_stmt_call(Signature, FuncRval, MaybeObject, CallArgs,
            Results, IsTailCall),
        Signature = mlds_func_signature(ArgTypes, RetTypes),
        indent_line(Indent, !IO),
        io.write_string("{\n", !IO),
        indent_line(Info, Context, Indent + 1, !IO),
        (
            Results = []
        ;
            Results = [Lval],
            output_lval(Info, Lval, !IO),
            io.write_string(" = ", !IO)
        ;
            Results = [_, _ | _],
            % for multiple return values,
            % we generate the following code:
            %   { object [] result = <func>(<args>);
            %     <output1> = (<type1>) result[0];
            %     <output2> = (<type2>) result[1];
            %     ...
            %   }
            %
            io.write_string("object [] result = ", !IO)
        ),
        ( FuncRval = ml_const(mlconst_code_addr(_)) ->
            % This is a standard method call.
            (
                MaybeObject = yes(Object),
                output_bracketed_rval(Info, Object, !IO),
                io.write_string(".", !IO)
            ;
                MaybeObject = no
            ),
            % This is a standard function call.
            output_call_rval(Info, FuncRval, !IO),
            io.write_string("(", !IO),
            io.write_list(CallArgs, ", ", output_rval(Info), !IO),
            io.write_string(")", !IO)
        ;
            % This is a call using a method pointer.
            %
            % Here we do downcasting, as a call will always return
            % something of type object
            %
            % XXX This is a hack, I can't see any way to do this downcasting
            % nicely, as it needs to effectively be wrapped around the method
            % call itself, so it acts before this predicate's solution to
            % multiple return values, see above.

            (
                RetTypes = []
            ;
                RetTypes = [RetType],
                boxed_type_to_string(Info, RetType, RetTypeString),
                io.format("((%s) ", [s(RetTypeString)], !IO)
            ;
                RetTypes = [_, _ | _],
                io.write_string("((object[]) ", !IO)
            ),
            (
                MaybeObject = yes(Object),
                output_bracketed_rval(Info, Object, !IO),
                io.write_string(".", !IO)
            ;
                MaybeObject = no
            ),

%             list.length(CallArgs, Arity),
%             list.length(RetTypes, NumRetTypes),
            io.format("((%s) ", [s(method_ptr_type_to_string(Info, ArgTypes, RetTypes))], !IO),
            output_bracketed_rval(Info, FuncRval, !IO),
            io.write_string(")(", !IO),
            output_boxed_args(Info, CallArgs, ArgTypes, !IO),
            % Closes brackets, and calls unbox methods for downcasting.
            % XXX This is a hack, see the above comment.
            io.write_string(")", !IO),
            (
                RetTypes = []
            ;
                RetTypes = [_RetType2],
                io.write_string(")", !IO)
            ;
                RetTypes = [_, _ | _],
                io.write_string(")", !IO)
            )
        ),
        io.write_string(";\n", !IO),

        ( Results = [_, _ | _] ->
            % Copy the results from the "result" array into the Result
            % lvals (unboxing them as we go).
            output_assign_results(Info, Results, RetTypes, 0, Indent + 1,
                Context, !IO)
        ;
            true
        ),
        % XXX Is this needed? If present, it causes compiler errors for a
        %     couple of files in the benchmarks directory.  -mjwybrow
        %
        % ( IsTailCall = tail_call, Results = [] ->
        %   indent_line(Context, Indent + 1, !IO),
        %   io.write_string("return;\n", !IO)
        % ;
        %   true
        % ),

        % This is to tell the C# compiler that a call to an erroneous procedure
        % will not fall through. --pw
        (
            IsTailCall = ordinary_call
        ;
            IsTailCall = tail_call
        ;
            IsTailCall = no_return_call,
            indent_line(Indent  + 1, !IO),
            io.write_string("throw new runtime.UnreachableDefault();\n", !IO)
        ),

        indent_line(Indent, !IO),
        io.write_string("}\n", !IO),
        ExitMethods = set.make_singleton_set(can_fall_through)
    ;
        Statement = ml_stmt_return(Results),
        (
            Results = [],
            indent_line(Indent, !IO),
            io.write_string("return;\n", !IO)
        ;
            Results = [Rval],
            indent_line(Indent, !IO),
            io.write_string("return ", !IO),
            output_rval(Info, Rval, !IO),
            io.write_string(";\n", !IO)
        ;
            Results = [_, _ | _],
            FuncInfo = func_info(Params),
            Params = mlds_func_params(_Args, ReturnTypes),
            TypesAndResults = assoc_list.from_corresponding_lists(
                ReturnTypes, Results),
            io.write_string("return new object[] {\n", !IO),
            indent_line(Indent + 1, !IO),
            Separator = ",\n" ++ duplicate_char(' ', (Indent + 1) * 2),
            io.write_list(TypesAndResults, Separator,
                (pred((Type - Result)::in, !.IO::di, !:IO::uo) is det :-
                    output_boxed_rval(Info, Type, Result, !IO)),
                !IO),
            io.write_string("\n", !IO),
            indent_line(Indent, !IO),
            io.write_string("};\n", !IO)
        ),
        ExitMethods = set.make_singleton_set(can_return)
    ;
        Statement = ml_stmt_do_commit(Ref),
        indent_line(Indent, !IO),
        output_rval(Info, Ref, !IO),
        io.write_string(" = new runtime.Commit();\n", !IO),
        indent_line(Indent, !IO),
        io.write_string("throw ", !IO),
        output_rval(Info, Ref, !IO),
        io.write_string(";\n", !IO),
        ExitMethods = set.make_singleton_set(can_throw)
    ;
        Statement = ml_stmt_try_commit(_Ref, Stmt, Handler),
        indent_line(Indent, !IO),
        io.write_string("try\n", !IO),
        indent_line(Indent, !IO),
        io.write_string("{\n", !IO),
        output_statement(Info, Indent + 1, FuncInfo, Stmt,
            TryExitMethods0, !IO),
        indent_line(Indent, !IO),
        io.write_string("}\n", !IO),
        indent_line(Indent, !IO),
        io.write_string("catch (runtime.Commit commit_variable)\n",
            !IO),
        indent_line(Indent, !IO),
        io.write_string("{\n", !IO),
        indent_line(Indent + 1, !IO),
        output_statement(Info, Indent + 1, FuncInfo, Handler,
            CatchExitMethods, !IO),
        indent_line(Indent, !IO),
        io.write_string("}\n", !IO),
        ExitMethods = (TryExitMethods0 `set.delete` can_throw)
            `set.union`  CatchExitMethods
    ;
        Statement = ml_stmt_atomic(AtomicStatement),
        output_atomic_stmt(Info, Indent, AtomicStatement, Context, !IO),
        ExitMethods = set.make_singleton_set(can_fall_through)
    ).

%-----------------------------------------------------------------------------%
%
% Extra code for handling while-loops.
%

:- func while_exit_methods(mlds_rval, exit_methods) = exit_methods.

while_exit_methods(Cond, BlockExitMethods) = ExitMethods :-
    % A while statement cannot complete normally if its condition
    % expression is a constant expression with value true, and it
    % doesn't contain a reachable break statement that exits the
    % while statement.
    (
        % XXX This is not a sufficient way of testing for a Java
        % "constant expression", though determining these accurately
        % is a little difficult to do here.
        Cond = ml_const(mlconst_true),
        not set.member(can_break, BlockExitMethods)
    ->
        % Cannot complete normally
        ExitMethods0 = BlockExitMethods `set.delete` can_fall_through
    ;
        ExitMethods0 = BlockExitMethods `set.insert` can_fall_through
    ),
    ExitMethods = (ExitMethods0 `set.delete` can_continue)
        `set.delete` can_break.

%-----------------------------------------------------------------------------%
%
% Extra code for handling function calls/returns.
%

:- pred output_boxed_args(csharp_out_info::in, list(mlds_rval)::in,
    list(mlds_type)::in, io::di, io::uo) is det.

output_boxed_args(_, [], [], !IO).
output_boxed_args(_, [_ | _], [], !IO) :-
    unexpected(this_file, "output_boxed_args: length mismatch.").
output_boxed_args(_, [], [_ | _], !IO) :-
    unexpected(this_file, "output_boxed_args: length mismatch.").
output_boxed_args(Info, [CallArg | CallArgs], [CallArgType | CallArgTypes],
        !IO) :-
    output_boxed_rval(Info, CallArgType, CallArg, !IO),
    (
        CallArgs = []
    ;
        CallArgs = [_ | _],
        io.write_string(", ", !IO),
        output_boxed_args(Info, CallArgs, CallArgTypes, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Code for handling multiple return values.
%

% When returning multiple values,
% we generate the following code:
%   { object [] result = <func>(<args>);
%     <output1> = (<type1>) result[0];
%     <output2> = (<type2>) result[1];
%     ...
%   }
%

    % This procedure generates the assignments to the outputs.
    %
:- pred output_assign_results(csharp_out_info::in, list(mlds_lval)::in,
    list(mlds_type)::in, int::in, indent::in, mlds_context::in,
    io::di, io::uo) is det.

output_assign_results(_, [], [], _, _, _, !IO).
output_assign_results(Info, [Lval | Lvals], [Type | Types], ResultIndex,
        Indent, Context, !IO) :-
    indent_line(Info, Context, Indent, !IO),
    output_lval(Info, Lval, !IO),
    io.write_string(" = ", !IO),
    output_unboxed_result(Info, Type, ResultIndex, !IO),
    io.write_string(";\n", !IO),
    output_assign_results(Info, Lvals, Types, ResultIndex + 1,
        Indent, Context, !IO).
output_assign_results(_, [_ | _], [], _, _, _, _, _) :-
    unexpected(this_file, "output_assign_results: list length mismatch.").
output_assign_results(_, [], [_ | _], _, _, _, _, _) :-
    unexpected(this_file, "output_assign_results: list length mismatch.").

:- pred output_unboxed_result(csharp_out_info::in, mlds_type::in, int::in,
    io::di, io::uo) is det.

output_unboxed_result(Info, Type, ResultIndex, !IO) :-
    io.write_string("(", !IO),
    output_type(Info, Type, !IO),
    io.write_string(") ", !IO),
    io.format("result[%d]", [i(ResultIndex)], !IO).

%-----------------------------------------------------------------------------%
%
% Extra code for outputting switch statements.
%

:- pred output_switch_cases(csharp_out_info::in, indent::in, func_info::in,
    mlds_context::in, list(mlds_switch_case)::in, mlds_switch_default::in,
    exit_methods::out, io::di, io::uo) is det.

output_switch_cases(Info, Indent, FuncInfo, Context,
        [], Default, ExitMethods, !IO) :-
    output_switch_default(Info, Indent, FuncInfo, Context, Default,
        ExitMethods, !IO).
output_switch_cases(Info, Indent, FuncInfo, Context,
        [Case | Cases], Default, ExitMethods, !IO) :-
    output_switch_case(Info, Indent, FuncInfo, Context, Case,
        CaseExitMethods0, !IO),
    output_switch_cases(Info, Indent, FuncInfo, Context, Cases, Default,
        CasesExitMethods, !IO),
    ( set.member(can_break, CaseExitMethods0) ->
        CaseExitMethods = (CaseExitMethods0 `set.delete` can_break)
            `set.insert` can_fall_through
    ;
        CaseExitMethods = CaseExitMethods0
    ),
    ExitMethods = CaseExitMethods `set.union` CasesExitMethods.

:- pred output_switch_case(csharp_out_info::in, indent::in, func_info::in,
    mlds_context::in, mlds_switch_case::in, exit_methods::out,
    io::di, io::uo) is det.

output_switch_case(Info, Indent, FuncInfo, Context, Case, ExitMethods, !IO) :-
    Case = mlds_switch_case(FirstCond, LaterConds, Statement),
    output_case_cond(Info, Indent, Context, FirstCond, !IO),
    list.foldl(output_case_cond(Info, Indent, Context), LaterConds, !IO),
    output_statement(Info, Indent + 1, FuncInfo, Statement,
        StmtExitMethods, !IO),
    ( set.member(can_fall_through, StmtExitMethods) ->
        indent_line(Info, Context, Indent + 1, !IO),
        io.write_string("break;\n", !IO),
        ExitMethods = (StmtExitMethods `set.insert` can_break)
            `set.delete` can_fall_through
    ;
        % Don't output `break' since it would be unreachable.
        ExitMethods = StmtExitMethods
    ).

:- pred output_case_cond(csharp_out_info::in, indent::in, mlds_context::in,
    mlds_case_match_cond::in, io::di, io::uo) is det.

output_case_cond(Info, Indent, Context, Match, !IO) :-
    (
        Match = match_value(Val),
        indent_line(Info, Context, Indent, !IO),
        io.write_string("case ", !IO),
        output_rval(Info, Val, !IO),
        io.write_string(":\n", !IO)
    ;
        Match = match_range(_, _),
        unexpected(this_file,
            "output_case_cond: cannot match ranges in C# cases")
    ).

:- pred output_switch_default(csharp_out_info::in, indent::in, func_info::in,
    mlds_context::in, mlds_switch_default::in, exit_methods::out,
    io::di, io::uo) is det.

output_switch_default(Info, Indent, FuncInfo, Context, Default,
        ExitMethods, !IO) :-
    (
        Default = default_do_nothing,
        ExitMethods = set.make_singleton_set(can_fall_through)
    ;
        Default = default_case(Statement),
        indent_line(Info, Context, Indent, !IO),
        io.write_string("default:\n", !IO),
        output_statement(Info, Indent + 1, FuncInfo, Statement, ExitMethods,
            !IO),
        indent_line(Info, Context, Indent, !IO),
        io.write_string("break;\n", !IO)
    ;
        Default = default_is_unreachable,
        indent_line(Info, Context, Indent, !IO),
        io.write_string("default: /*NOTREACHED*/\n", !IO),
        indent_line(Info, Context, Indent + 1, !IO),
        io.write_string("throw new runtime.UnreachableDefault();\n",
            !IO),
        ExitMethods = set.make_singleton_set(can_throw)
    ).

%-----------------------------------------------------------------------------%
%
% Code for outputting atomic statements.
%

:- pred output_atomic_stmt(csharp_out_info::in, indent::in,
    mlds_atomic_statement::in, mlds_context::in, io::di, io::uo) is det.

output_atomic_stmt(Info, Indent, AtomicStmt, Context, !IO) :-
    (
        AtomicStmt = comment(Comment),
        % XXX We should escape any "*/"'s in the Comment. We should also split
        % the comment into lines and indent each line appropriately.
        indent_line(Indent, !IO),
        io.write_string("/* ", !IO),
        io.write_string(Comment, !IO),
        io.write_string(" */\n", !IO)
    ;
        AtomicStmt = assign(Lval, Rval),
        indent_line(Indent, !IO),
        output_lval(Info, Lval, !IO),
        io.write_string(" = ", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(";\n", !IO)
    ;
        AtomicStmt = assign_if_in_heap(_, _),
        sorry(this_file, "output_atomic_stmt: assign_if_in_heap")
    ;
        AtomicStmt = delete_object(_Lval),
        unexpected(this_file, "delete_object not supported in C#.")
    ;
        AtomicStmt = new_object(Target, _MaybeTag, ExplicitSecTag, Type,
            _MaybeSize, MaybeCtorName, Args, ArgTypes, _MayUseAtomic),
        (
            ExplicitSecTag = yes,
            unexpected(this_file, "output_atomic_stmt: explicit secondary tag")
        ;
            ExplicitSecTag = no
        ),

        indent_line(Indent, !IO),
        io.write_string("{\n", !IO),
        indent_line(Info, Context, Indent + 1, !IO),
        output_lval(Info, Target, !IO),
        io.write_string(" = new ", !IO),
        % Generate class constructor name.
        (
            MaybeCtorName = yes(QualifiedCtorId),
            \+ (
                Type = mercury_type(MerType, CtorCat, _),
                hand_defined_type(MerType, CtorCat, _, _)
            )
        ->
            output_type(Info, Type, !IO),
            io.write_char('.', !IO),
            QualifiedCtorId = qual(_ModuleName, _QualKind, CtorDefn),
            CtorDefn = ctor_id(CtorName, CtorArity),
            output_unqual_class_name(CtorName, CtorArity, !IO)
        ;
            output_type(Info, Type, !IO)
        ),
        IsArray = type_is_array(Type),
        (
            IsArray = is_array,
            % The new object will be an array, so we need to initialise it
            % using array literals syntax.
            io.write_string(" {", !IO),
            output_init_args(Info, Args, ArgTypes, !IO),
            io.write_string("};\n", !IO)
        ;
            IsArray = not_array,
            % Generate constructor arguments.
            io.write_string("(", !IO),
            output_init_args(Info, Args, ArgTypes, !IO),
            io.write_string(");\n", !IO)
        ),
        indent_line(Indent, !IO),
        io.write_string("}\n", !IO)
    ;
        AtomicStmt = gc_check,
        unexpected(this_file, "gc_check not implemented.")
    ;
        AtomicStmt = mark_hp(_Lval),
        unexpected(this_file, "mark_hp not implemented.")
    ;
        AtomicStmt = restore_hp(_Rval),
        unexpected(this_file, "restore_hp not implemented.")
    ;
        AtomicStmt = trail_op(_TrailOp),
        unexpected(this_file, "trail_ops not implemented.")
    ;
        AtomicStmt = inline_target_code(TargetLang, Components),
        (
            TargetLang = ml_target_csharp,
            indent_line(Indent, !IO),
            list.foldl(output_target_code_component(Info), Components, !IO)
        ;
            ( TargetLang = ml_target_c
            ; TargetLang = ml_target_gnu_c
            ; TargetLang = ml_target_asm
            ; TargetLang = ml_target_il
            ; TargetLang = ml_target_java
            ),
            unexpected(this_file,
                "inline_target_code only works for lang_java")
        )
    ;
        AtomicStmt = outline_foreign_proc(_TargetLang, _Vs, _Lvals, _Code),
        unexpected(this_file, "foreign language interfacing not implemented")
    ).

%-----------------------------------------------------------------------------%

:- pred output_target_code_component(csharp_out_info::in,
    target_code_component::in, io::di, io::uo) is det.

output_target_code_component(Info, TargetCode, !IO) :-
    (
        TargetCode = user_target_code(CodeString, MaybeUserContext, _Attrs),
        (
            MaybeUserContext = yes(ProgContext),
            output_context(Info, mlds_make_context(ProgContext), !IO)
        ;
            MaybeUserContext = no
        ),
        io.write_string(CodeString, !IO)
    ;
        TargetCode = raw_target_code(CodeString, _Attrs),
        io.write_string(CodeString, !IO)
    ;
        TargetCode = target_code_input(Rval),
        output_rval(Info, Rval, !IO)
    ;
        TargetCode = target_code_output(Lval),
        output_lval(Info, Lval, !IO)
    ;
        TargetCode = target_code_type(Type),
        % XXX enable generics here
        output_type(Info, Type, !IO)
    ;
        TargetCode = target_code_name(Name),
        output_maybe_qualified_name(Info, Name, !IO)
    ).

%-----------------------------------------------------------------------------%

    % Output initial values of an object's fields as arguments for the
    % object's class constructor.
    %
:- pred output_init_args(csharp_out_info::in, list(mlds_rval)::in,
    list(mlds_type)::in, io::di, io::uo) is det.

output_init_args(_, [], [], !IO).
output_init_args(_, [_ | _], [], _, _) :-
    unexpected(this_file, "output_init_args: length mismatch.").
output_init_args(_, [], [_ | _], _, _) :-
    unexpected(this_file, "output_init_args: length mismatch.").
output_init_args(Info, [Arg | Args], [_ArgType | ArgTypes], !IO) :-
    output_rval(Info, Arg, !IO),
    (
        Args = []
    ;
        Args = [_ | _],
        io.write_string(", ", !IO)
    ),
    output_init_args(Info, Args, ArgTypes, !IO).

%-----------------------------------------------------------------------------%
%
% Code to output expressions.
%

:- pred output_lval(csharp_out_info::in, mlds_lval::in, io::di, io::uo) is det.

output_lval(Info, Lval, !IO) :-
    (
        Lval = ml_field(_MaybeTag, PtrRval, FieldId, FieldType, _),
        (
            FieldId = ml_field_offset(OffsetRval),
            (
                ( FieldType = mlds_generic_type
                ; FieldType = mercury_type(type_variable(_, _), _, _)
                )
            ->
                true
            ;
                % The field type for field(_, _, offset(_), _, _) lvals
                % must be something that maps to MR_Box.
                unexpected(this_file, "unexpected field type.")
            ),
            % XXX We shouldn't need this cast here, but there are cases where
            % it is needed and the MLDS doesn't seem to generate it.
            io.write_string("((object[]) ", !IO),
            output_rval(Info, PtrRval, !IO),
            io.write_string(")[", !IO),
            output_rval(Info, OffsetRval, !IO),
            io.write_string("]", !IO)
        ;
            FieldId = ml_field_named(FieldName, CtorType),
            (
                FieldName = qual(_, _, UnqualFieldName),
                UnqualFieldName = "data_tag"
            ->
                % If the field we are trying to access is just a `data_tag'
                % then it is a member of the base class.
                output_bracketed_rval(Info, PtrRval, !IO),
                io.write_string(".", !IO)
            ;
                PtrRval = ml_self(_)
            ->
                % Suppress type cast on `this' keyword.  This makes a
                % difference when assigning to `final' member variables in
                % constructor functions.
                output_rval(Info, PtrRval, !IO),
                io.write_string(".", !IO)
            ;
                % Otherwise the field we are trying to access may be
                % in a derived class. Objects are manipulated as instances
                % of their base class, so we need to downcast to the derived
                % class to access some fields.
                io.write_string("((", !IO),
                output_type(Info, CtorType, !IO),
                io.write_string(") ", !IO),
                output_bracketed_rval(Info, PtrRval, !IO),
                io.write_string(").", !IO)
            ),
            FieldName = qual(_, _, UnqualFieldName),
            output_valid_mangled_name(UnqualFieldName, !IO)
        )
    ;
        Lval = ml_mem_ref(Rval, _Type),
        output_bracketed_rval(Info, Rval, !IO)
    ;
        Lval = ml_global_var_ref(GlobalVarRef),
        GlobalVarRef = env_var_ref(EnvVarName),
        io.write_string("mercury_envvar_", !IO),
        io.write_string(EnvVarName, !IO)
    ;
        Lval = ml_var(qual(ModName, QualKind, Name), _),
        % Rewrite references to constants in private_builtin.m to the
        % mercury_dotnet.cs file.
        Name = mlds_var_name(NameStr, _),
        (
            SymName = mlds_module_name_to_sym_name(ModName),
            strip_outermost_qualifier(SymName, "mercury",
                mercury_private_builtin_module)
        ->
            ( string.prefix(NameStr, "MR_TYPECTOR_REP_") ->
                io.write_string("runtime.TypeCtorRep.", !IO),
                io.write_string(NameStr, !IO)
            ; string.prefix(NameStr, "MR_SECTAG_") ->
                io.write_string("runtime.Sectag_Locn.", !IO),
                io.write_string(NameStr, !IO)
            ; NameStr = "MR_PREDICATE" ->
                io.write_string("runtime.Constants.MR_PREDICATE", !IO)
            ; NameStr = "MR_FUNCTION" ->
                io.write_string("runtime.Constants.MR_FUNCTION", !IO)
            ;
                QualName = qual(ModName, QualKind,
                    entity_data(mlds_data_var(Name))),
                output_maybe_qualified_name(Info, QualName, !IO)
            )
        ;
            QualName = qual(ModName, QualKind,
                entity_data(mlds_data_var(Name))),
            output_maybe_qualified_name(Info, QualName, !IO)
        )
    ).

:- pred output_mangled_name(string::in, io::di, io::uo) is det.

output_mangled_name(Name, !IO) :-
    MangledName = name_mangle(Name),
    io.write_string(MangledName, !IO).

:- pred output_valid_mangled_name(string::in, io::di, io::uo) is det.

output_valid_mangled_name(Name, !IO) :-
    MangledName = name_mangle(Name),
    JavaSafeName = valid_csharp_symbol_name(MangledName),
    io.write_string(JavaSafeName, !IO).

:- pred output_call_rval(csharp_out_info::in, mlds_rval::in, io::di, io::uo)
    is det.

output_call_rval(Info, Rval, !IO) :-
    (
        Rval = ml_const(Const),
        Const = mlconst_code_addr(CodeAddr)
    ->
        IsCall = yes,
        mlds_output_code_addr(Info, CodeAddr, IsCall, !IO)
    ;
        output_bracketed_rval(Info, Rval, !IO)
    ).

:- pred output_bracketed_rval(csharp_out_info::in, mlds_rval::in, io::di, io::uo)
    is det.

output_bracketed_rval(Info, Rval, !IO) :-
    (
        % If it's just a variable name, then we don't need parentheses.
        ( Rval = ml_lval(ml_var(_,_))
        ; Rval = ml_const(mlconst_code_addr(_))
        )
    ->
        output_rval(Info, Rval, !IO)
    ;
        io.write_char('(', !IO),
        output_rval(Info, Rval, !IO),
        io.write_char(')', !IO)
    ).

:- pred output_rval(csharp_out_info::in, mlds_rval::in, io::di, io::uo) is det.

output_rval(Info, Rval, !IO) :-
    (
        Rval = ml_lval(Lval),
        output_lval(Info, Lval, !IO)
    ;
        Rval = ml_mkword(_, _),
        unexpected(this_file, "output_rval: tags not supported in C#")
    ;
        Rval = ml_const(Const),
        output_rval_const(Info, Const, !IO)
    ;
        Rval = ml_unop(Op, RvalA),
        output_unop(Info, Op, RvalA, !IO)
    ;
        Rval = ml_binop(Op, RvalA, RvalB),
        output_binop(Info, Op, RvalA, RvalB, !IO)
    ;
        Rval = ml_mem_addr(_Lval),
        unexpected(this_file, "output_rval: mem_addr(_) not supported")
    ;
        Rval = ml_scalar_common(_),
        % This reference is not the same as a mlds_data_addr const.
        unexpected(this_file, "output_rval: ml_scalar_common")
    ;
        Rval = ml_vector_common_row(VectorCommon, RowRval),
        output_vector_common_row_rval(Info, VectorCommon, RowRval, !IO)
    ;
        Rval = ml_self(_),
        io.write_string("this", !IO)
    ).

:- pred output_unop(csharp_out_info::in, mlds_unary_op::in, mlds_rval::in,
    io::di, io::uo) is det.

output_unop(Info, Unop, Expr, !IO) :-
    (
        Unop = cast(Type),
        output_cast_rval(Info, Type, Expr, !IO)
    ;
        Unop = box(Type),
        output_boxed_rval(Info, Type, Expr, !IO)
    ;
        Unop = unbox(Type),
        output_unboxed_rval(Info, Type, Expr, !IO)
    ;
        Unop = std_unop(StdUnop),
        output_std_unop(Info, StdUnop, Expr, !IO)
    ).

:- pred output_cast_rval(csharp_out_info::in, mlds_type::in, mlds_rval::in,
    io::di, io::uo) is det.

output_cast_rval(Info, Type, Expr, !IO) :-
    % rtti_to_mlds.m generates casts from int to runtime.PseudoTypeInfo, but
    % for C# we need to treat these as constructions, not casts.
    % Similarly for conversions from TypeCtorInfo to TypeInfo.
    (
        Type = mlds_pseudo_type_info_type,
        Expr = ml_const(mlconst_int(N))
    ->
        maybe_output_comment(Info, "cast", !IO),
        ( have_preallocated_pseudo_type_var(N) ->
            io.write_string("runtime.PseudoTypeInfo.K", !IO),
            io.write_int(N, !IO)
        ;
            io.write_string("new runtime.PseudoTypeInfo(", !IO),
            output_rval(Info, Expr, !IO),
            io.write_string(")", !IO)
        )
    ;
        ( Type = mercury_type(_, ctor_cat_system(cat_system_type_info), _)
        ; Type = mlds_type_info_type
        )
    ->
        % XXX We really should be able to tell if we are casting a
        % TypeCtorInfo or a TypeInfo. Julien says that's probably going to
        % be rather difficult as the compiler doesn't keep track of where
        % type_ctor_infos are acting as type_infos properly.
        maybe_output_comment(Info, "cast", !IO),
        io.write_string("runtime.TypeInfo_Struct.maybe_new(",
            !IO),
        output_rval(Info, Expr, !IO),
        io.write_string(")", !IO)
    ;
        csharp_builtin_type(Type, "int")
    ->
        io.write_string("(int) ", !IO),
        output_rval(Info, Expr, !IO)
    ;
        io.write_string("(", !IO),
        output_type(Info, Type, !IO),
        io.write_string(") ", !IO),
        output_rval(Info, Expr, !IO)
    ).

:- pred have_preallocated_pseudo_type_var(int::in) is semidet.

have_preallocated_pseudo_type_var(N) :-
    % Corresponds to static members in class PseudoTypeInfo.
    N >= 1,
    N =< 5.

:- pred output_boxed_rval(csharp_out_info::in, mlds_type::in, mlds_rval::in,
     io::di, io::uo) is det.

output_boxed_rval(Info, _Type, Expr, !IO) :-
    % C# does implicit boxing.
    output_rval(Info, Expr, !IO).
    /*
    ( csharp_builtin_type(Type, _JavaName, JavaBoxedName, _) ->
        % valueOf may return cached instances instead of creating new objects.
        io.write_string(JavaBoxedName, !IO),
        io.write_string(".valueOf(", !IO),
        output_rval(Info, Expr, !IO),
        io.write_string(")", !IO)
    ;
        io.write_string("((object) (", !IO),
        output_rval(Info, Expr, !IO),
        io.write_string("))", !IO)
    ).
    */

:- pred output_unboxed_rval(csharp_out_info::in, mlds_type::in, mlds_rval::in,
    io::di, io::uo) is det.

output_unboxed_rval(Info, Type, Expr, !IO) :-
    io.write_string("((", !IO),
    output_type(Info, Type, !IO),
    io.write_string(") ", !IO),
    output_rval(Info, Expr, !IO),
    io.write_string(")", !IO).

:- pred csharp_builtin_type(mlds_type::in, string::out) is semidet.

csharp_builtin_type(Type, "int") :-
    Type = mlds_native_int_type.
csharp_builtin_type(Type, "int") :-
    Type = mercury_type(builtin_type(builtin_type_int), _, _).
csharp_builtin_type(Type, "double") :-
    Type = mlds_native_float_type.
csharp_builtin_type(Type, "double") :-
    Type = mercury_type(builtin_type(builtin_type_float), _, _).
csharp_builtin_type(Type, "char") :-
    Type = mlds_native_char_type.
csharp_builtin_type(Type, "char") :-
    Type = mercury_type(builtin_type(builtin_type_char), _, _).
csharp_builtin_type(Type, "bool") :-
    Type = mlds_native_bool_type.
csharp_builtin_type(Type, "int") :-
    % The test for defined/3 is logically redundant since all dummy
    % types are defined types, but enables the compiler to infer that
    % this disjunction is a switch.
    Type = mercury_type(defined_type(_, _, _), TypeCtorCat, _),
    TypeCtorCat = ctor_cat_builtin_dummy.

:- pred output_std_unop(csharp_out_info::in, builtin_ops.unary_op::in,
    mlds_rval::in, io::di, io::uo) is det.

    % There are no tags, so all the tagging operators are no-ops, except for
    % `tag', which always returns zero (a tag of zero means there's no tag).
    %
output_std_unop(Info, UnaryOp, Expr, !IO) :-
    ( UnaryOp = tag ->
        io.write_string("/* tag */  0", !IO)
    ;
        % XXX C# is not Java
        java_util.java_unary_prefix_op(UnaryOp, UnaryOpString),
        io.write_string(UnaryOpString, !IO),
        io.write_string("(", !IO),
        output_rval(Info, Expr, !IO),
        io.write_string(")", !IO)
    ).

:- pred output_binop(csharp_out_info::in, binary_op::in, mlds_rval::in,
    mlds_rval::in, io::di, io::uo) is det.

output_binop(Info, Op, X, Y, !IO) :-
    ( Op = array_index(_Type) ->
        output_bracketed_rval(Info, X, !IO),
        io.write_string("[", !IO),
        output_rval(Info, Y, !IO),
        io.write_string("]", !IO)
    % XXX C# is not Java
    ; java_util.java_string_compare_op(Op, OpStr) ->
        ( OpStr = "==" ->
            output_rval(Info, X, !IO),
            io.write_string(".Equals(", !IO),
            output_rval(Info, Y, !IO),
            io.write_string(")", !IO)
        ;
            io.write_string("(", !IO),
            output_rval(Info, X, !IO),
            io.write_string(".CompareTo(", !IO),
            output_rval(Info, Y, !IO),
            io.write_string(") ", !IO),
            io.write_string(OpStr, !IO),
            io.write_string(" 0)", !IO)
        )
    ;
        io.write_string("(", !IO),
        output_rval(Info, X, !IO),
        io.write_string(" ", !IO),
        output_binary_op(Op, !IO),
        io.write_string(" ", !IO),
        output_rval(Info, Y, !IO),
        io.write_string(")", !IO)
    ).

:- pred output_binary_op(binary_op::in, io::di, io::uo) is det.

output_binary_op(Op, !IO) :-
    % XXX why are these separated into three predicates?
    % XXX C# is not Java
    ( java_util.java_binary_infix_op(Op, OpStr) ->
        io.write_string(OpStr, !IO)
    ; java_util.java_float_compare_op(Op, OpStr) ->
        io.write_string(OpStr, !IO)
    ; java_util.java_float_op(Op, OpStr) ->
        io.write_string(OpStr, !IO)
    ;
        unexpected(this_file, "output_binary_op: invalid binary operator")
    ).

:- pred output_rval_const(csharp_out_info::in, mlds_rval_const::in,
    io::di, io::uo) is det.

output_rval_const(Info, Const, !IO) :-
    (
        Const = mlconst_true,
        io.write_string("true", !IO)
    ;
        Const = mlconst_false,
        io.write_string("false", !IO)
    ;
        Const = mlconst_int(N),
        output_int_const(N, !IO)
    ;
        Const = mlconst_char(N),
        io.write_string("((char) ", !IO),
        output_int_const(N, !IO),
        io.write_string(")", !IO)
    ;
        Const = mlconst_enum(N, EnumType),
        % Explicit cast required.
        output_cast_rval(Info, EnumType, ml_const(mlconst_int(N)), !IO)
    ;
        Const = mlconst_foreign(Lang, Value, _Type),
        expect(unify(Lang, lang_csharp), this_file,
            "output_rval_const: language other than C#."),
        % XXX Should we parenthesize this?
        io.write_string(Value, !IO)
    ;
        Const = mlconst_float(FloatVal),
        c_util.output_float_literal(FloatVal, !IO)
    ;
        Const = mlconst_string(String),
        io.write_string("""", !IO),
        c_util.output_quoted_string_lang(literal_csharp, String, !IO),
        io.write_string("""", !IO)
    ;
        Const = mlconst_multi_string(String),
        io.write_string("""", !IO),
        c_util.output_quoted_multi_string_lang(literal_csharp, String, !IO),
        io.write_string("""", !IO)
    ;
        Const = mlconst_named_const(NamedConst),
        io.write_string(NamedConst, !IO)
    ;
        Const = mlconst_code_addr(CodeAddr),
        IsCall = no,
        mlds_output_code_addr(Info, CodeAddr, IsCall, !IO)
    ;
        Const = mlconst_data_addr(DataAddr),
        mlds_output_data_addr(DataAddr, !IO)
    ;
        Const = mlconst_null(Type),
        Initializer = get_type_initializer(Type),
        io.write_string(Initializer, !IO)
    ).

:- pred output_int_const(int::in, io::di, io::uo) is det.

output_int_const(N, !IO) :-
    % The Mercury compiler could be using 64-bit integers but Java has 32-bit
    % ints.  A literal 0xffffffff in a source file would be interpreted by a
    % 64-bit Mercury compiler as 4294967295.  If it is written out in decimal a
    % Java compiler would rightly complain because the integer is too large to
    % fit in a 32-bit int.  However, it won't complain if the literal is
    % expressed in hexadecimal (nor as the negative decimal -1).
    % XXX check this for C#
    ( N < 0 ->
        io.write_int(N, !IO)
    ;
        N >> 32 = 0,
        N /\ 0x80000000 = 0x80000000
    ->
        % The bit pattern fits in 32 bits, but is too large to write as a
        % positive decimal.  This branch is unreachable on a 32-bit compiler.
        io.format("0x%x", [i(N /\ 0xffffffff)], !IO)
    ;
        io.write_int(N, !IO)
    ).

:- pred output_vector_common_row_rval(csharp_out_info::in,
    mlds_vector_common::in, mlds_rval::in, io::di, io::uo) is det.

output_vector_common_row_rval(Info, VectorCommon, RowRval, !IO) :-
    VectorCommon = ml_vector_common(_ModuleName, _Type,
        ml_vector_common_type_num(TypeNum), StartRowNum, _NumRows),
    io.format("MR_vector_common_%d[%d + ", [i(TypeNum), i(StartRowNum)], !IO),
    output_rval(Info, RowRval, !IO),
    io.write_string("]", !IO).

%-----------------------------------------------------------------------------%

:- pred mlds_output_code_addr(csharp_out_info::in, mlds_code_addr::in, bool::in,
    io::di, io::uo) is det.

mlds_output_code_addr(Info, CodeAddr, IsCall, !IO) :-
    ( CodeAddr = code_addr_proc(_, Sig)
    ; CodeAddr = code_addr_internal(_, _, Sig)
    ),
    Sig = mlds_func_signature(ArgTypes, RetTypes),
    (
        IsCall = no,
        % Not a function call, so we are taking the address of the
        % wrapper for that function (method).
        TypeString = method_ptr_type_to_string(Info, ArgTypes, RetTypes),
        io.format("(%s) ", [s(TypeString)], !IO)
    ;
        IsCall = yes
    ),
    (
        CodeAddr = code_addr_proc(Label, _Sig),
        output_fully_qualified_thing(Label, mlds_output_proc_label, !IO)
    ;
        CodeAddr = code_addr_internal(Label, SeqNum, _Sig),
        output_fully_qualified_thing(Label, mlds_output_proc_label, !IO),
        io.write_string("_", !IO),
        io.write_int(SeqNum, !IO)
    ).

:- func method_ptr_type_to_string(csharp_out_info, mlds_arg_types,
    mlds_return_types) = string.

method_ptr_type_to_string(Info, ArgTypes, RetTypes) = String :-
    Arity = list.length(ArgTypes),
    (
        RetTypes = [],
        VoidRet = "_r0",
        RetTypesStrings = []
    ;
        RetTypes = [R1 | Rs],
        VoidRet = "",
        (
            Rs = [],
            boxed_type_to_string(Info, R1, R1_string),
            RetTypesStrings = [R1_string]
        ;
            Rs = [_ | _],
            RetTypesStrings = ["object[]"] % for now
        )
    ),
    list.map(boxed_type_to_string(Info), ArgTypes, ArgTypesStrings),
    TypesString = string.join_list(", ", ArgTypesStrings ++ RetTypesStrings),
    String = "runtime.MethodPtr" ++ string.from_int(Arity) ++
        VoidRet ++ "<" ++ TypesString ++ ">".

:- pred mlds_output_proc_label(mlds_proc_label::in, io::di, io::uo) is det.

mlds_output_proc_label(mlds_proc_label(PredLabel, ProcId), !IO) :-
    output_pred_label(PredLabel, !IO),
    proc_id_to_int(ProcId, ModeNum),
    io.format("_%d", [i(ModeNum)], !IO).

:- pred mlds_output_data_addr(mlds_data_addr::in, io::di, io::uo) is det.

mlds_output_data_addr(data_addr(ModuleQualifier, DataName), !IO) :-
    SymName = mlds_module_name_to_sym_name(ModuleQualifier),
    % XXX could use global::mercury. instead of stripping it
    ( strip_outermost_qualifier(SymName, "mercury", StrippedSymName) ->
        mangle_sym_name_for_csharp(StrippedSymName, module_qual, "__",
            ModuleName)
    ;
        mangle_sym_name_for_csharp(SymName, module_qual, "__", ModuleName)
    ),
    io.write_string(ModuleName, !IO),
    io.write_string(".", !IO),
    output_data_name(DataName, !IO).

%-----------------------------------------------------------------------------%
%
% Miscellaneous stuff to handle indentation and generation of
% source context annotations.
%

:- mutable(last_context, prog_context, context_init, ground,
    [untrailed, attach_to_io_state]).

:- pred output_context(csharp_out_info::in, mlds_context::in,
    io::di, io::uo) is det.

output_context(Info, Context, !IO) :-
    LineNumbers = Info ^ oi_line_numbers,
    (
        LineNumbers = yes,
        ProgContext = mlds_get_prog_context(Context),
        get_last_context(LastContext, !IO),
        term.context_file(ProgContext, File),
        term.context_line(ProgContext, Line),
        (
            ProgContext \= LastContext,
            Line > 0,
            File \= ""
        ->
            % Java doesn't have an equivalent of #line directives.
            % \u is treated as a Unicode escape even with comments.
            % XXX update for C#
            io.write_string("// ", !IO),
            string.replace_all(File, "\\u", "\\\\u", SafePath),
            io.write_string(SafePath, !IO),
            io.write_string(":", !IO),
            io.write_int(Line, !IO),
            io.nl(!IO),
            set_last_context(ProgContext, !IO)
        ;
            true
        )
    ;
        LineNumbers = no
    ).

:- pred indent_line(csharp_out_info::in, mlds_context::in, indent::in,
    io::di, io::uo) is det.

indent_line(Info, Context, N, !IO) :-
    output_context(Info, Context, !IO),
    indent_line(N, !IO).

    % A value of type `indent' records the number of levels of indentation
    % to indent the next piece of code. Currently we output two spaces
    % for each level of indentation.
    % XXX There is a small amount of code duplication with mlds_to_c.m here.
:- type indent == int.

:- pred indent_line(indent::in, io::di, io::uo) is det.

indent_line(N, !IO) :-
    ( N =< 0 ->
        true
    ;
        io.write_string("  ", !IO),
        indent_line(N - 1, !IO)
    ).

%-----------------------------------------------------------------------------%

:- type csharp_out_info
    --->    csharp_out_info(
                % These are static.
                oi_module_info      :: module_info,
                oi_auto_comments    :: bool,
                oi_line_numbers     :: bool,
                oi_module_name      :: mlds_module_name,

                % These are dynamic.
                oi_output_generics  :: output_generics,
                oi_univ_tvars       :: list(tvar)
            ).

:- type output_generics
    --->    do_output_generics
    ;       do_not_output_generics.

:- func init_csharp_out_info(module_info) = csharp_out_info.

init_csharp_out_info(ModuleInfo) = Info :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, auto_comments, AutoComments),
    globals.lookup_bool_option(Globals, line_numbers, LineNumbers),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    Info = csharp_out_info(ModuleInfo, AutoComments, LineNumbers,
        MLDS_ModuleName, do_not_output_generics, []).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "mlds_to_cs.m".

%-----------------------------------------------------------------------------%
