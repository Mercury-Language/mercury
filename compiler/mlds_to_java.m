%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2011 The University of Melbourne.
% Copyright (C) 2013-2016 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mlds_to_java.m.
% Main authors: juliensf, mjwybrow, fjh, wangp.
%
% Convert MLDS to Java code.
%
% DONE:
%   det and semidet predicates
%   multiple output arguments
%   boxing and unboxing
%   conjunctions
%   disjunctions
%   if-then-else's
%   enumerations
%   discriminated unions
%   higher order functions
%   multidet and nondet predicates
%   test tests/benchmarks/*.m
%   generate optimized tailcalls
%   RTTI generation
%   handle foreign code written in Java
%   Support for Java in mmc --make
%   Support for nested modules
%
% TODO:
% - Support nested modules
%   (The problem with current code generation scheme for nested modules
%   is that Java does not allow the name of a class to be the same
%   as the name of its enclosing package. That should work now, but
%   javac doesn't like the filenames we give for submodules.)
%
% - Generate names of classes etc. correctly.
%
% - General code cleanup
%
% - handle static ground terms(?)
%
% - support foreign_import_module for Java
%
% - handle foreign code written in C
%
% NOTES:
% To avoid namespace conflicts all Java names must be fully qualified,
% e.g. The classname `String' must be qualified as `java.lang.String'
% to avoid conflicting with `mercury.String'.
%
% There is currently some code threaded through the output predicates (usually
% a variable called `ExitMethods') which keeps track of, and removes
% unreachable code. Ideally this would be done as an MLDS->MLDS transformation,
% preferably in a separate module. Unfortunately this is not possible
% due to the fact that the back-end generates `break' statements for cases
% in switches as they are output, meaning that we can't remove them in
% a pass over the MLDS.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.mlds_to_java.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module ml_backend.mlds.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- pred output_java_mlds(module_info::in, mlds::in, bool::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

    % XXX needed for c_util.output_quoted_string,
    %     c_util.output_quoted_multi_string, and
    %     c_util.make_float_literal.
:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.rtti.
:- import_module hlds.hlds_pred.           % for pred_proc_id.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_code_util.  % for ml_gen_local_var_decl_flags.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_type_gen.   % for ml_gen_type_name
:- import_module ml_backend.ml_util.
:- import_module ml_backend.rtti_to_mlds.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.file_names.    % for mercury_std_library_name.
:- import_module parse_tree.java_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module char.
:- import_module cord.
:- import_module digraph.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module uint.
:- import_module varset.

%-----------------------------------------------------------------------------%

output_java_mlds(ModuleInfo, MLDS, Succeeded, !IO) :-
    % Note that the Java file name that we use for modules in the
    % Mercury standard library do not include a "mercury." prefix;
    % that's why we don't call mercury_module_name_to_mlds here.
    module_info_get_globals(ModuleInfo, Globals),
    ModuleName = mlds_get_module_name(MLDS),
    module_name_to_file_name(Globals, ModuleName, ".java", do_create_dirs,
        JavaSourceFile, !IO),
    Indent = 0,
    output_to_file(Globals, JavaSourceFile,
        output_java_src_file(ModuleInfo, Indent, MLDS), Succeeded, !IO).

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

    % Succeeds iff this type is a enumeration.
    %
:- pred type_is_enum(mlds_type::in) is semidet.

type_is_enum(Type) :-
    Type = mercury_type(_, Builtin, _),
    Builtin = ctor_cat_enum(_).

    % Succeeds iff the Rval represents an enumeration object in the Java
    % backend. We need to check both Rvals that are variables and Rvals
    % that are casts. We need to know this in order to append the field name
    % to the object so we can access the value of the enumeration object.
    %
:- pred rval_is_enum_object(mlds_rval::in) is semidet.

rval_is_enum_object(Rval) :-
    Rval = ml_lval(Lval),
    (
        Lval = ml_var(_, VarType),
        type_is_enum(VarType)
    ;
        Lval = ml_field(_, _, _, FieldType, _),
        type_is_enum(FieldType)
    ).

    % Succeeds iff a given string matches the unqualified interface name
    % of a interface in Mercury's Java runtime system.
    %
:- pred interface_is_special(string::in) is semidet.

interface_is_special("MercuryType").
interface_is_special("MethodPtr").
interface_is_special("MethodPtr1").
interface_is_special("MethodPtr2").
interface_is_special("MethodPtr3").
interface_is_special("MethodPtr4").
interface_is_special("MethodPtr5").
interface_is_special("MethodPtr6").
interface_is_special("MethodPtr7").
interface_is_special("MethodPtr8").
interface_is_special("MethodPtr9").
interface_is_special("MethodPtr10").
interface_is_special("MethodPtr11").
interface_is_special("MethodPtr12").
interface_is_special("MethodPtr13").
interface_is_special("MethodPtr14").
interface_is_special("MethodPtr15").
interface_is_special("MethodPtrN").

%-----------------------------------------------------------------------------%
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
% XXX It might be nice if the name mangling code was taken out of which
% ever LLDS module it's hiding in and put in a separate one.
%

    % XXX This won't work if we start using the Java coding conventions
    % for all names. At the moment it only affects library modules.
    %
:- pred enforce_java_names(string::in, string::out) is det.
:- pragma consider_used(enforce_java_names/2).

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

%-----------------------------------------------------------------------------%
%
% Code to output imports.
%

:- pred output_imports(mlds_imports::in, io::di, io::uo) is det.

output_imports(Imports, !IO) :-
    list.foldl(output_import, Imports, !IO).

:- pred output_import(mlds_import::in, io::di, io::uo) is det.

output_import(Import, !IO) :-
    (
        Import = mercury_import(ImportType, ImportName),
        (
            ImportType = user_visible_interface,
            unexpected($module, $pred,
                "import_type `user_visible_interface' in Java backend")
        ;
            ImportType = compiler_visible_interface
        )
    ;
        Import = foreign_import(_),
        unexpected($module, $pred, "foreign import in Java backend")
    ),
    SymName = mlds_module_name_to_sym_name(ImportName),
    mangle_sym_name_for_java(SymName, module_qual, "__", ClassFile),
    % There are issues related to using import statements and Java's naming
    % conventions. To avoid these problems, we output dependencies as comments
    % only. This is ok, since we always use fully qualified names anyway.
    io.write_strings(["// import ", ClassFile, ";\n"], !IO).

%-----------------------------------------------------------------------------%
%
% Code to generate the `.java' file.
%

:- pred output_java_src_file(module_info::in, indent::in, mlds::in,
    io::di, io::uo) is det.

output_java_src_file(ModuleInfo, Indent, MLDS, !IO) :-
    % Run further transformations on the MLDS.
    MLDS = mlds(ModuleName, AllForeignCode, Imports, GlobalData, Defns0,
        InitPreds, FinalPreds, ExportedEnums),
    ml_global_data_get_all_global_defns(GlobalData,
        ScalarCellGroupMap, VectorCellGroupMap, _AllocIdMap, GlobalDefns),

    % Do NOT enforce the outermost "mercury" qualifier here. This module name
    % is compared with other module names in the MLDS, to avoid unnecessary
    % module qualification.
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),

    % Find and build list of all methods which would have their addresses
    % taken to be used as a function pointer.
    some [!CodeAddrs] (
        !:CodeAddrs = [],
        find_pointer_addressed_methods(GlobalDefns, !CodeAddrs),
        find_pointer_addressed_methods(Defns0, !CodeAddrs),

        map.values(ScalarCellGroupMap, ScalarCellGroups),
        ScalarCellRows = list.map(func(G) = G ^ mscg_rows, ScalarCellGroups),
        list.foldl(find_pointer_addressed_methods_in_scalars,
            ScalarCellRows, !CodeAddrs),

        make_code_addr_map(!.CodeAddrs, multi_map.init, CodeAddrsMap),
        map.to_assoc_list(CodeAddrsMap, CodeAddrsAssocList)
    ),

    % Create wrappers in MLDS for all pointer addressed methods.
    list.map_foldl(generate_addr_wrapper_class(MLDS_ModuleName),
        CodeAddrsAssocList, WrapperClassDefns, map.init, AddrOfMap),
    Defns1 = GlobalDefns ++ WrapperClassDefns ++ Defns0,

    % Rename classes with excessively long names.
    shorten_long_class_names(MLDS_ModuleName, Defns1, Defns),

    % Get the foreign code for Java
    % XXX We should not ignore _RevImports.
    ForeignCode = mlds_get_java_foreign_code(AllForeignCode),
    ForeignCode = mlds_foreign_code(ForeignDeclCodes, ForeignBodyCodes,
        _Imports, ExportDefns),

    % Output transformed MLDS as Java source.
    %
    % The order is important here, because Java requires static constants
    % be defined before they can be used in static initializers.
    % We start with the Java foreign code declarations, since for
    % library/private_builtin.m they contain static constants
    % that will get used in the RTTI definitions.
    module_info_get_globals(ModuleInfo, Globals),
    module_source_filename(Globals, ModuleName, SourceFileName, !IO),
    Info = init_java_out_info(ModuleInfo, SourceFileName, AddrOfMap),
    output_src_start(Info, Indent, ModuleName, Imports, ForeignDeclCodes,
        Defns, !IO),
    io.write_list(ForeignBodyCodes, "\n", output_java_body_code(Info, Indent),
        !IO),

    list.filter(defn_is_rtti_data, Defns, RttiDefns, NonRttiDefns),

    io.write_string("\n// RttiDefns\n", !IO),
    output_defns(Info, Indent + 1, alloc_only, RttiDefns, !IO),
    output_rtti_assignments(Info, Indent + 1, RttiDefns, !IO),

    list.filter(defn_is_data, NonRttiDefns, DataDefns, NonDataDefns),
    io.write_string("\n// DataDefns\n", !IO),
    output_data_decls(Info, Indent + 1, DataDefns, !IO),
    output_data_assignments(Info, Indent + 1, DataDefns, !IO),

    % Scalar common data must appear after the previous data definitions,
    % and the vector common data after that.
    io.write_string("\n// Scalar common data\n", !IO),
    output_scalar_common_data(Info, Indent + 1, ScalarCellGroupMap, !IO),

    io.write_string("\n// Vector common data\n", !IO),
    output_vector_common_data(Info, Indent + 1, VectorCellGroupMap, !IO),

    io.write_string("\n// NonDataDefns\n", !IO),
    output_defns(Info, Indent + 1, none, NonDataDefns, !IO),

    io.write_string("\n// ExportDefns\n", !IO),
    output_exports(Info, Indent + 1, ExportDefns, !IO),

    io.write_string("\n// ExportedEnums\n", !IO),
    output_exported_enums(Info, Indent + 1, ExportedEnums, !IO),

    io.write_string("\n// InitPreds\n", !IO),
    output_inits(Indent + 1, InitPreds, !IO),

    io.write_string("\n// FinalPreds\n", !IO),
    output_finals(Indent + 1, FinalPreds, !IO),

    io.write_string("\n// EnvVarNames\n", !IO),
    output_env_vars(Indent + 1, NonRttiDefns, !IO),

    output_src_end(Indent, ModuleName, !IO).
    % XXX Need to handle non-Java foreign code at this point.

%-----------------------------------------------------------------------------%
%
% Code for working with Java `foreign_code'.
%

:- pred output_java_decl(java_out_info::in, indent::in, foreign_decl_code::in,
    io::di, io::uo) is det.

output_java_decl(Info, Indent, DeclCode, !IO) :-
    DeclCode = foreign_decl_code(Lang, _IsLocal, LiteralOrInclude, Context),
    (
        Lang = lang_java,
        output_java_foreign_literal_or_include(Info, Indent,
            LiteralOrInclude, Context, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_csharp
        ; Lang = lang_erlang
        ),
        sorry($module, $pred, "foreign decl other than Java")
    ).

:- pred output_java_body_code(java_out_info::in, indent::in,
    foreign_body_code::in, io::di, io.state::uo) is det.

output_java_body_code(Info, Indent, ForeignBodyCode, !IO) :-
    ForeignBodyCode = foreign_body_code(Lang, LiteralOrInclude, Context),
    % Only output Java code.
    (
        Lang = lang_java,
        output_java_foreign_literal_or_include(Info, Indent, LiteralOrInclude,
            Context, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_csharp
        ; Lang = lang_erlang
        ),
        sorry($module, $pred, "foreign code other than Java")
    ).

:- pred output_java_foreign_literal_or_include(java_out_info::in,
    indent::in, foreign_literal_or_include::in, prog_context::in,
    io::di, io::uo) is det.

output_java_foreign_literal_or_include(Info, Indent, LiteralOrInclude,
        Context, !IO) :-
    (
        LiteralOrInclude = floi_literal(Code),
        write_string_with_context_block(Info, Indent, Code, Context, !IO)
    ;
        LiteralOrInclude = floi_include_file(IncludeFile),
        SourceFileName = Info ^ joi_source_filename,
        make_include_file_path(SourceFileName, IncludeFile, IncludePath),
        output_context(Info ^ joi_foreign_line_numbers, marker_begin_block,
            context(IncludePath, 1), !IO),
        write_include_file_contents_cur_stream(IncludePath, !IO),
        io.nl(!IO),
        % We don't have the true end context readily available.
        output_context(Info ^ joi_foreign_line_numbers, marker_end_block,
            Context, !IO)
    ).

    % Get the foreign code for Java.
    %
:- func mlds_get_java_foreign_code(map(foreign_language, mlds_foreign_code))
    = mlds_foreign_code.

mlds_get_java_foreign_code(AllForeignCode) = ForeignCode :-
    ( if map.search(AllForeignCode, lang_java, ForeignCode0) then
        ForeignCode = ForeignCode0
    else
        ForeignCode = mlds_foreign_code([], [], [], [])
    ).

%-----------------------------------------------------------------------------%
%
% Code for handling `pragma foreign_export' for Java.
%

    % Exports are converted into forwarding methods that are given the
    % specified name. These simply call the exported procedure.
    %
    % NOTE: the forwarding methods must be declared public as they might
    % be referred to within foreign_procs that are inlined across module
    % boundaries.
    %
:- pred output_exports(java_out_info::in, indent::in,
    list(mlds_pragma_export)::in, io::di, io::uo) is det.

output_exports(Info, Indent, Exports, !IO) :-
    list.foldl(output_export(Info, Indent), Exports, !IO).

:- pred output_export(java_out_info::in, indent::in, mlds_pragma_export::in,
    io::di, io::uo) is det.

output_export(Info0, Indent, Export, !IO) :-
    Export = ml_pragma_export(Lang, ExportName, _, MLDS_Signature,
        UnivQTVars, _),
    expect(unify(Lang, lang_java), $module, $pred,
        "foreign_export for language other than Java."),

    indent_line(Indent, !IO),
    io.write_string("public static ", !IO),
    output_generic_tvars(UnivQTVars, !IO),
    io.nl(!IO),
    indent_line(Indent, !IO),

    MLDS_Signature = mlds_func_params(Parameters, ReturnTypes),
    Info = (Info0 ^ joi_output_generics := do_output_generics)
                  ^ joi_univ_tvars := UnivQTVars,
    (
        ReturnTypes = [],
        io.write_string("void", !IO)
    ;
        ReturnTypes = [RetType],
        output_type(Info, RetType, !IO)
    ;
        ReturnTypes = [_, _ | _],
        % For multiple outputs, we return an array of objects.
        io.write_string("java.lang.Object []", !IO)
    ),
    io.write_string(" " ++ ExportName, !IO),
    ( if
        list.member(Param, Parameters),
        has_ptr_type(Param)
    then
        (
            ( ReturnTypes = []
            ; ReturnTypes = [_]
            ),
            output_export_ref_out(Info, Indent, Export, !IO)
        ;
            ReturnTypes = [_, _ | _],
            unexpected($module, $pred, "multiple return values")
        )
    else
        output_export_no_ref_out(Info, Indent, Export, !IO)
    ).

:- pred output_export_no_ref_out(java_out_info::in, indent::in,
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

:- pred output_export_ref_out(java_out_info::in, indent::in,
    mlds_pragma_export::in, io::di, io::uo) is det.

output_export_ref_out(Info, Indent, Export, !IO) :-
    Export = ml_pragma_export(_Lang, _ExportName, MLDS_Name, MLDS_Signature,
        _UnivQTVars, _MLDS_Context),
    MLDS_Signature = mlds_func_params(Parameters, ReturnTypes),
    list.filter(has_ptr_type, Parameters, RefParams, NonRefParams),

    output_export_params_ref_out(Info, Indent, Parameters, !IO),
    io.nl(!IO),
    indent_line(Indent, !IO),
    io.write_string("{\n", !IO),
    indent_line(Indent + 1, !IO),
    io.write_string("java.lang.Object[] results = ", !IO),
    write_export_call(MLDS_Name, NonRefParams, !IO),

    ( if ReturnTypes = [] then
        FirstRefArg = 0
    else if ReturnTypes = [mlds_native_bool_type] then
        % Semidet procedure.
        FirstRefArg = 1
    else
        unexpected($module, $pred, "unexpected ReturnTypes")
    ),
    list.foldl2(assign_ref_output(Info, Indent + 1), RefParams,
        FirstRefArg, _, !IO),
    (
        FirstRefArg = 0
    ;
        FirstRefArg = 1,
        indent_line(Indent + 1, !IO),
        Stmt = "return ((java.lang.Boolean) results[0]).booleanValue();\n",
        io.write_string(Stmt, !IO)
    ),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_export_params_ref_out(java_out_info::in, indent::in,
    list(mlds_argument)::in, io::di, io::uo) is det.

output_export_params_ref_out(Info, Indent, Parameters, !IO) :-
    io.write_string("(", !IO),
    (
        Parameters = []
    ;
        Parameters = [_ | _],
        io.nl(!IO),
        io.write_list(Parameters, ",\n",
            output_export_param_ref_out(Info, Indent + 1), !IO)
    ),
    io.write_string(")", !IO).

:- pred output_export_param_ref_out(java_out_info::in, indent::in,
    mlds_argument::in, io::di, io::uo) is det.

output_export_param_ref_out(Info, Indent, Argument, !IO) :-
    Argument = mlds_argument(VarName, Type, _),
    indent_line(Indent, !IO),
    ( if Type = mlds_ptr_type(InnerType) then
        boxed_type_to_string(Info, InnerType, InnerTypeString),
        io.format("jmercury.runtime.Ref<%s> ", [s(InnerTypeString)], !IO)
    else
        output_type(Info, Type, !IO),
        io.write_string(" ", !IO)
    ),
    output_mlds_var_name_for_java(VarName, !IO).

:- pred write_export_call(mlds_qualified_entity_name::in,
    list(mlds_argument)::in, io::di, io::uo) is det.

write_export_call(MLDS_Name, Parameters, !IO) :-
    output_fully_qualified_thing(MLDS_Name, output_entity_name_for_java, !IO),
    io.write_char('(', !IO),
    io.write_list(Parameters, ", ", write_argument_name, !IO),
    io.write_string(");\n", !IO).

:- pred write_argument_name(mlds_argument::in, io::di, io::uo) is det.

write_argument_name(Arg, !IO) :-
    Arg = mlds_argument(VarName, _, _),
    output_mlds_var_name_for_java(VarName, !IO).

:- pred assign_ref_output(java_out_info::in, indent::in, mlds_argument::in,
    int::in, int::out, io::di, io::uo) is det.

assign_ref_output(Info, Indent, Arg, N, N + 1, !IO) :-
    Arg = mlds_argument(VarName, Type, _),
    indent_line(Indent, !IO),
    output_mlds_var_name_for_java(VarName, !IO),
    ( if Type = mlds_ptr_type(InnerType) then
        boxed_type_to_string(Info, InnerType, TypeString)
    else
        boxed_type_to_string(Info, Type, TypeString)
    ),
    io.format(".val = (%s) results[%d];\n", [s(TypeString), i(N)], !IO).

:- pred has_ptr_type(mlds_argument::in) is semidet.

has_ptr_type(mlds_argument(_, mlds_ptr_type(_), _)).

%-----------------------------------------------------------------------------%
%
% Code for handling `pragma foreign_export_enum' for Java.
%

:- pred output_exported_enums(java_out_info::in, indent::in,
    list(mlds_exported_enum)::in, io::di, io::uo) is det.

output_exported_enums(Info, Indent, ExportedEnums, !IO) :-
    list.foldl(output_exported_enum(Info, Indent), ExportedEnums, !IO).

:- pred output_exported_enum(java_out_info::in, indent::in,
    mlds_exported_enum::in, io::di, io::uo) is det.

output_exported_enum(Info, Indent, ExportedEnum, !IO) :-
    ExportedEnum = mlds_exported_enum(Lang, _, TypeCtor, ExportedConstants0),
    (
        Lang = lang_java,
        ml_gen_type_name(TypeCtor, ClassName, ClassArity),
        MLDS_Type = mlds_class_type(ClassName, ClassArity, mlds_enum),
        % We reverse the list so the constants are printed out in order.
        list.reverse(ExportedConstants0, ExportedConstants),
        list.foldl(output_exported_enum_constant(Info, Indent, MLDS_Type),
            ExportedConstants, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_csharp
        ; Lang = lang_erlang
        )
    ).

:- pred output_exported_enum_constant(java_out_info::in, indent::in,
    mlds_type::in, mlds_exported_enum_constant::in, io::di, io::uo) is det.

output_exported_enum_constant(Info, Indent, MLDS_Type, ExportedConstant,
        !IO) :-
    ExportedConstant = mlds_exported_enum_constant(Name, Initializer),
    indent_line(Indent, !IO),
    io.write_string("public static final ", !IO),
    output_type(Info, MLDS_Type, !IO),
    io.write_string(" ", !IO),
    io.write_string(Name, !IO),
    io.write_string(" = ", !IO),
    output_initializer_body(Info, Initializer, no, !IO),
    io.write_string(";\n", !IO).

%-----------------------------------------------------------------------------%
%
% Code to search MLDS for all uses of function pointers.
%

    % Returns code-address information (function label and signature)
    % for each method/function which has its address taken in the MLDS.
    %
:- pred find_pointer_addressed_methods(list(mlds_defn)::in,
    list(mlds_code_addr)::in, list(mlds_code_addr)::out) is det.

find_pointer_addressed_methods([], !CodeAddrs).
find_pointer_addressed_methods([Defn | Defns], !CodeAddrs) :-
    Defn  = mlds_defn(_Name, _Context, _Flags, Body),
    method_ptrs_in_entity_defn(Body, !CodeAddrs),
    find_pointer_addressed_methods(Defns, !CodeAddrs).

:- pred find_pointer_addressed_methods_in_scalars(cord(mlds_initializer)::in,
    list(mlds_code_addr)::in, list(mlds_code_addr)::out) is det.

find_pointer_addressed_methods_in_scalars(Cord, !CodeAddrs) :-
    cord.foldl_pred(method_ptrs_in_initializer, Cord, !CodeAddrs).

:- pred method_ptrs_in_entity_defn(mlds_entity_defn::in,
    list(mlds_code_addr)::in, list(mlds_code_addr)::out) is det.

method_ptrs_in_entity_defn(Defn, !CodeAddrs) :-
    (
        Defn = mlds_function(_MaybeID, _Params, Body, _Attributes, _EnvVars,
            _MaybeRequireTailrecInfo),
        (
            Body = body_defined_here(Statement),
            method_ptrs_in_statement(Statement, !CodeAddrs)
        ;
            Body = body_external
        )
    ;
        Defn = mlds_data(_Type, Initializer, _GCStatement),
        method_ptrs_in_initializer(Initializer, !CodeAddrs)
    ;
        Defn = mlds_class(ClassDefn),
        ClassDefn = mlds_class_defn(_, _, _, _, _, Ctors, Members),
        method_ptrs_in_defns(Ctors, !CodeAddrs),
        method_ptrs_in_defns(Members, !CodeAddrs)
    ).

:- pred method_ptrs_in_statements(list(statement)::in,
    list(mlds_code_addr)::in, list(mlds_code_addr)::out) is det.

method_ptrs_in_statements([], !CodeAddrs).
method_ptrs_in_statements([Statement | Statements], !CodeAddrs) :-
    method_ptrs_in_statement(Statement, !CodeAddrs),
    method_ptrs_in_statements(Statements, !CodeAddrs).

:- pred method_ptrs_in_statement(statement::in,
    list(mlds_code_addr)::in, list(mlds_code_addr)::out) is det.

method_ptrs_in_statement(statement(Stmt, _Context), !CodeAddrs) :-
    method_ptrs_in_stmt(Stmt, !CodeAddrs).

:- pred method_ptrs_in_stmt(mlds_stmt::in,
    list(mlds_code_addr)::in, list(mlds_code_addr)::out) is det.

method_ptrs_in_stmt(ml_stmt_block(Defns, Statements), !CodeAddrs) :-
    method_ptrs_in_defns(Defns, !CodeAddrs),
    method_ptrs_in_statements(Statements, !CodeAddrs).
method_ptrs_in_stmt(ml_stmt_while(_Kind, Rval, Statement), !CodeAddrs) :-
    method_ptrs_in_rval(Rval, !CodeAddrs),
    method_ptrs_in_statement(Statement, !CodeAddrs).
method_ptrs_in_stmt(ml_stmt_if_then_else(Rval, StatementThen,
        MaybeStatementElse), !CodeAddrs) :-
    method_ptrs_in_rval(Rval, !CodeAddrs),
    method_ptrs_in_statement(StatementThen, !CodeAddrs),
    (
        MaybeStatementElse = yes(StatementElse),
        method_ptrs_in_statement(StatementElse, !CodeAddrs)
    ;
        MaybeStatementElse = no
    ).
method_ptrs_in_stmt(ml_stmt_switch(_Type, Rval, _Range, Cases, Default),
        !CodeAddrs) :-
    method_ptrs_in_rval(Rval, !CodeAddrs),
    method_ptrs_in_switch_cases(Cases, !CodeAddrs),
    method_ptrs_in_switch_default(Default, !CodeAddrs).
method_ptrs_in_stmt(ml_stmt_label(_), _, _) :-
    unexpected($module, $pred, "labels not supported in Java.").
method_ptrs_in_stmt(ml_stmt_goto(goto_break), !CodeAddrs).
method_ptrs_in_stmt(ml_stmt_goto(goto_continue), !CodeAddrs).
method_ptrs_in_stmt(ml_stmt_goto(goto_label(_)), _, _) :-
    unexpected($module, $pred, "goto label not supported in Java.").
method_ptrs_in_stmt(ml_stmt_computed_goto(_, _), _, _) :-
    unexpected($module, $pred, "computed gotos not supported in Java.").
method_ptrs_in_stmt(ml_stmt_try_commit(_Lval, StatementGoal,
        StatementHandler), !CodeAddrs) :-
    % We don't check "_Lval" here as we expect it to be a local variable
    % of type mlds_commit_type.
    method_ptrs_in_statement(StatementGoal, !CodeAddrs),
    method_ptrs_in_statement(StatementHandler, !CodeAddrs).
method_ptrs_in_stmt(ml_stmt_do_commit(_Rval), !CodeAddrs).
    % We don't check "_Rval" here as we expect it to be a local variable
    % of type mlds_commit_type.
method_ptrs_in_stmt(ml_stmt_return(Rvals), !CodeAddrs) :-
    method_ptrs_in_rvals(Rvals, !CodeAddrs).
method_ptrs_in_stmt(CallStmt, !CodeAddrs) :-
    CallStmt = ml_stmt_call(_FuncSig, _Rval, _MaybeThis, Rvals, _ReturnVars,
        _IsTailCall, _Markers),
    % We don't check "_Rval" - it may be a code address but is a
    % standard call rather than a function pointer use.
    method_ptrs_in_rvals(Rvals, !CodeAddrs).
method_ptrs_in_stmt(ml_stmt_atomic(AtomicStatement), !CodeAddrs) :-
    ( if
        AtomicStatement = new_object(Lval, _MaybeTag, _Bool,
            _Type, _MemRval, _MaybeCtorName, Rvals, _Types, _MayUseAtomic,
            _AllocId)
    then
        % We don't need to check "_MemRval" since this just stores
        % the amount of memory needed for the new object.
        method_ptrs_in_lval(Lval, !CodeAddrs),
        method_ptrs_in_rvals(Rvals, !CodeAddrs)
    else if
        AtomicStatement = assign(Lval, Rval)
    then
        method_ptrs_in_lval(Lval, !CodeAddrs),
        method_ptrs_in_rval(Rval, !CodeAddrs)
    else
        true
    ).

:- pred method_ptrs_in_switch_default(mlds_switch_default::in,
    list(mlds_code_addr)::in, list(mlds_code_addr)::out) is det.

method_ptrs_in_switch_default(default_is_unreachable, !CodeAddrs).
method_ptrs_in_switch_default(default_do_nothing, !CodeAddrs).
method_ptrs_in_switch_default(default_case(Statement), !CodeAddrs) :-
    method_ptrs_in_statement(Statement, !CodeAddrs).

:- pred method_ptrs_in_switch_cases(list(mlds_switch_case)::in,
    list(mlds_code_addr)::in, list(mlds_code_addr)::out) is det.

method_ptrs_in_switch_cases([], !CodeAddrs).
method_ptrs_in_switch_cases([Case | Cases], !CodeAddrs) :-
    Case = mlds_switch_case(_FirstCond, _LaterConds, Statement),
    method_ptrs_in_statement(Statement, !CodeAddrs),
    method_ptrs_in_switch_cases(Cases, !CodeAddrs).

:- pred method_ptrs_in_defns(list(mlds_defn)::in, list(mlds_code_addr)::in,
    list(mlds_code_addr)::out) is det.

method_ptrs_in_defns([], !CodeAddrs).
method_ptrs_in_defns([Defn | Defns], !CodeAddrs) :-
    method_ptrs_in_defn(Defn, !CodeAddrs),
    method_ptrs_in_defns(Defns, !CodeAddrs).

:- pred method_ptrs_in_defn(mlds_defn::in, list(mlds_code_addr)::in,
    list(mlds_code_addr)::out) is det.

method_ptrs_in_defn(mlds_defn(_Name, _Context, _Flags, Body), !CodeAddrs) :-
    method_ptrs_in_entity_defn(Body, !CodeAddrs).

:- pred method_ptrs_in_initializer(mlds_initializer::in,
    list(mlds_code_addr)::in, list(mlds_code_addr)::out) is det.

method_ptrs_in_initializer(no_initializer, !CodeAddrs).
method_ptrs_in_initializer(init_struct(_Type, Initializers),
        !CodeAddrs) :-
    method_ptrs_in_initializers(Initializers, !CodeAddrs).
method_ptrs_in_initializer(init_array(Initializers), !CodeAddrs) :-
    method_ptrs_in_initializers(Initializers, !CodeAddrs).
method_ptrs_in_initializer(init_obj(Rval), !CodeAddrs) :-
    method_ptrs_in_rval(Rval, !CodeAddrs).

:- pred method_ptrs_in_initializers(list(mlds_initializer)::in,
    list(mlds_code_addr)::in, list(mlds_code_addr)::out) is det.

method_ptrs_in_initializers([], !CodeAddrs).
method_ptrs_in_initializers([Initializer | Initializers], !CodeAddrs) :-
    method_ptrs_in_initializer(Initializer, !CodeAddrs),
    method_ptrs_in_initializers(Initializers, !CodeAddrs).

:- pred method_ptrs_in_rvals(list(mlds_rval)::in, list(mlds_code_addr)::in,
    list(mlds_code_addr)::out) is det.

method_ptrs_in_rvals([], !CodeAddrs).
method_ptrs_in_rvals([Rval | Rvals], !CodeAddrs) :-
    method_ptrs_in_rval(Rval, !CodeAddrs),
    method_ptrs_in_rvals(Rvals, !CodeAddrs).

:- pred method_ptrs_in_rval(mlds_rval::in, list(mlds_code_addr)::in,
    list(mlds_code_addr)::out) is det.

method_ptrs_in_rval(Rval, !CodeAddrs) :-
    (
        Rval = ml_lval(Lval),
        method_ptrs_in_lval(Lval, !CodeAddrs)
    ;
        Rval = ml_mkword(_Tag, SubRval),
        method_ptrs_in_rval(SubRval, !CodeAddrs)
    ;
        Rval = ml_const(RvalConst),
        (
            RvalConst = mlconst_code_addr(CodeAddr),
            !:CodeAddrs = [CodeAddr | !.CodeAddrs]
        ;
            ( RvalConst = mlconst_true
            ; RvalConst = mlconst_false
            ; RvalConst = mlconst_int(_)
            ; RvalConst = mlconst_uint(_)
            ; RvalConst = mlconst_char(_)
            ; RvalConst = mlconst_enum(_, _)
            ; RvalConst = mlconst_foreign(_, _, _)
            ; RvalConst = mlconst_float(_)
            ; RvalConst = mlconst_string(_)
            ; RvalConst = mlconst_multi_string(_)
            ; RvalConst = mlconst_named_const(_)
            ; RvalConst = mlconst_data_addr(_)
            ; RvalConst = mlconst_null(_)
            )
        )
    ;
        Rval = ml_unop(_UnaryOp, RvalA),
        method_ptrs_in_rval(RvalA, !CodeAddrs)
    ;
        Rval = ml_binop(_BinaryOp, RvalA, RvalB),
        method_ptrs_in_rval(RvalA, !CodeAddrs),
        method_ptrs_in_rval(RvalB, !CodeAddrs)
    ;
        Rval = ml_vector_common_row(_, RowRval),
        method_ptrs_in_rval(RowRval, !CodeAddrs)
    ;
        ( Rval = ml_scalar_common(_)
        ; Rval = ml_mem_addr(_Address)
        ; Rval = ml_self(_Type)
        )
    ).

:- pred method_ptrs_in_lval(mlds_lval::in, list(mlds_code_addr)::in,
    list(mlds_code_addr)::out) is det.

    % Here, "_Rval" is the address of a variable so we don't check it.
method_ptrs_in_lval(ml_mem_ref(_Rval, _Type), !CodeAddrs).
    % Here, "_Rval" is a pointer to a cell on the heap, and doesn't need
    % to be considered.
method_ptrs_in_lval(ml_field(_MaybeTag, _Rval, _FieldId, _FieldType, _PtrType),
        !CodeAddrs).
method_ptrs_in_lval(ml_var(_Variable, _Type), !CodeAddrs).
method_ptrs_in_lval(ml_global_var_ref(_), !CodeAddrs).

%-----------------------------------------------------------------------------%
%
% Code to output wrapper classes for the implementation of function pointers
% in Java.
%
% As there is no way to take the address of a method in Java, we must create a
% wrapper for that method which implements a common interface. We are then able
% to pass that class around as a java.lang.Object.
%
% XXX This implementation will not handle taking the address of instance
% methods. This is not currently a problem as they will never be generated
% by the MLDS back-end.
%
% XXX This implementation will not correctly handle the case which occurs where
% there are two or more overloaded MLDS functions (that we take the address of)
% with the same name and arity but different argument types, both in the same
% module. This is due to the fact that the names of the generated wrapper
% classes are based purely on the method name.

:- type call_method_inputs
    --->    cmi_separate(list(mlds_var_name))
    ;       cmi_array(mlds_var_name).

:- pred make_code_addr_map(list(mlds_code_addr)::in,
    multi_map(arity, mlds_code_addr)::in,
    multi_map(arity, mlds_code_addr)::out) is det.

make_code_addr_map([], !Map).
make_code_addr_map([CodeAddr | CodeAddrs], !Map) :-
    (
        CodeAddr = code_addr_proc(_ProcLabel, OrigFuncSignature)
    ;
        CodeAddr = code_addr_internal(_ProcLabel, _SeqNum, OrigFuncSignature)
    ),
    OrigFuncSignature = mlds_func_signature(OrigArgTypes, _OrigRetTypes),
    list.length(OrigArgTypes, Arity),
    multi_map.set(Arity, CodeAddr, !Map),
    make_code_addr_map(CodeAddrs, !Map).

:- pred generate_addr_wrapper_class(mlds_module_name::in,
    pair(arity, list(mlds_code_addr))::in, mlds_defn::out,
    map(mlds_code_addr, code_addr_wrapper)::in,
    map(mlds_code_addr, code_addr_wrapper)::out) is det.

generate_addr_wrapper_class(MLDS_ModuleName, Arity - CodeAddrs, ClassDefn,
        !AddrOfMap) :-
    % Create a name for this wrapper class based on the fully qualified method
    % (predicate) name.
    ClassName = "addrOf" ++ string.from_int(Arity),

    % If the class is wrapping more than one method then add a member variable
    % which says which predicate to call, and a constructor function to
    % initialise that variable.
    (
        CodeAddrs = [_],
        DataDefns = [],
        CtorDefns = []
    ;
        CodeAddrs = [_, _ | _],
        Context = mlds_make_context(term.context_init),

        % Create the member variable.
        DataDefn = mlds_defn(
            entity_data(mlds_data_var(mlds_var_name("ptr_num", no))),
            Context, ml_gen_const_member_decl_flags,
            mlds_data(mlds_native_int_type, no_initializer, gc_no_stmt)),
        DataDefns = [DataDefn],

        % Create the constructor function.
        QualClassName = qual(MLDS_ModuleName, module_qual, ClassName),
        ClassType = mlds_class_type(QualClassName, 0, mlds_class),

        FieldName = qual(MLDS_ModuleName, type_qual, "ptr_num"),
        FieldId = ml_field_named(FieldName, ClassType),
        FieldLval = ml_field(no, ml_self(ClassType), FieldId,
            mlds_native_int_type, ClassType),

        CtorArgName = mlds_var_name("ptr_num", no),
        CtorArgs = [mlds_argument(CtorArgName, mlds_native_int_type,
            gc_no_stmt)],
        CtorReturnValues = [],

        CtorArgLval = ml_var(qual(MLDS_ModuleName, type_qual, CtorArgName),
            mlds_native_int_type),
        CtorArgRval = ml_lval(CtorArgLval),
        CtorStatement = statement(
            ml_stmt_atomic(assign(FieldLval, CtorArgRval)), Context),

        Attributes = [],
        EnvVarNames = set.init,
        Ctor = mlds_function(no, mlds_func_params(CtorArgs, CtorReturnValues),
            body_defined_here(CtorStatement), Attributes, EnvVarNames, no),
        CtorFlags = init_decl_flags(acc_public, per_instance, non_virtual,
            overridable, modifiable, concrete),
        CtorDefn = mlds_defn(entity_export("<constructor>"), Context,
            CtorFlags, Ctor),
        CtorDefns = [CtorDefn]
    ;
        CodeAddrs = [],
        unexpected($module, $pred, "no addresses")
    ),

    % Create a method that calls the original predicates.
    generate_call_method(MLDS_ModuleName, Arity, CodeAddrs, MethodDefn),

    ( if Arity =< max_specialised_method_ptr_arity then
        InterfaceName = "MethodPtr" ++ string.from_int(Arity)
    else
        InterfaceName = "MethodPtrN"
    ),
    InterfaceModuleName = mercury_module_name_to_mlds(
        java_mercury_runtime_package_name),
    Interface = qual(InterfaceModuleName, module_qual, InterfaceName),

    % Create class components.
    ClassImports = [],
    ClassExtends = [],
    InterfaceDefn = mlds_class_type(Interface, 0, mlds_interface),
    ClassImplements = [InterfaceDefn],
    TypeParams = [],

    % Put it all together.
    ClassMembers  = DataDefns ++ [MethodDefn],
    ClassEntityName = entity_type(ClassName, 0),
    ClassContext  = mlds_make_context(term.context_init),
    ClassFlags    = addr_wrapper_decl_flags,
    ClassBodyDefn = mlds_class_defn(mlds_class, ClassImports, ClassExtends,
        ClassImplements, TypeParams, CtorDefns, ClassMembers),
    ClassBody     = mlds_class(ClassBodyDefn),
    ClassDefn = mlds_defn(ClassEntityName, ClassContext, ClassFlags,
        ClassBody),

    add_to_address_map(ClassName, CodeAddrs, !AddrOfMap).

    % The highest arity for which there is a specialised MethodPtr<n>
    % interface.
    %
:- func max_specialised_method_ptr_arity = int.

max_specialised_method_ptr_arity = 15.

:- pred generate_call_method(mlds_module_name::in, arity::in,
    list(mlds_code_addr)::in, mlds_defn::out) is det.

generate_call_method(MLDS_ModuleName, Arity, CodeAddrs, MethodDefn) :-
    % Create the arguments to the call method. For low arities the method
    % takes n arguments directly. For higher arities the arguments are passed
    % in as an array.
    ( if Arity =< max_specialised_method_ptr_arity then
        list.map2(create_generic_arg, 1 .. Arity, ArgNames, MethodArgs),
        InputArgs = cmi_separate(ArgNames)
    else
        ArgName = mlds_var_name("args", no),
        ArgType = mlds_array_type(mlds_generic_type),
        Arg = mlds_argument(ArgName, ArgType, gc_no_stmt),
        MethodArgs = [Arg],
        InputArgs = cmi_array(ArgName)
    ),

    % Create a statement to call each of the original methods.
    list.map(generate_call_statement_for_addr(InputArgs), CodeAddrs,
        CodeAddrStatements),

    Context = mlds_make_context(term.context_init),

    % If there is more than one original method then we need to switch on the
    % ptr_num member variable.
    (
        CodeAddrStatements = [Statement]
    ;
        CodeAddrStatements = [_, _ | _],
        MaxCase = list.length(CodeAddrs) - 1,
        MakeCase = (func(I, CaseStatement) = Case :-
            MatchCond = match_value(ml_const(mlconst_int(I))),
            Case = mlds_switch_case(MatchCond, [], CaseStatement)
        ),
        Cases = list.map_corresponding(MakeCase, 0 .. MaxCase,
            CodeAddrStatements),

        SwitchVarName = mlds_var_name("ptr_num", no),
        SwitchVar = qual(MLDS_ModuleName, module_qual, SwitchVarName),
        SwitchVarRval = ml_lval(ml_var(SwitchVar, mlds_native_int_type)),
        SwitchRange = mlds_switch_range(0, MaxCase),
        Switch = ml_stmt_switch(mlds_native_int_type, SwitchVarRval,
            SwitchRange, Cases, default_is_unreachable),
        Statement = statement(Switch, Context)
    ;
        CodeAddrStatements = [],
        unexpected($module, $pred, "no statements")
    ),

    % Create new method name.
    PredID = hlds_pred.initial_pred_id,
    ProcID = initial_proc_id,
    Label = mlds_special_pred_label("call", no, "", 0),
    MethodName = entity_function(Label, ProcID, no, PredID),

    % Create return type.
    MethodRetType = mlds_generic_type,
    MethodRets = [MethodRetType],

    % Put it all together.
    MethodParams = mlds_func_params(MethodArgs, MethodRets),
    MethodMaybeID = no,
    MethodAttribs = [],
    MethodEnvVarNames = set.init,
    MethodBody   = mlds_function(MethodMaybeID, MethodParams,
        body_defined_here(Statement), MethodAttribs, MethodEnvVarNames, no),
    MethodFlags  = ml_gen_member_decl_flags,
    MethodDefn   = mlds_defn(MethodName, Context, MethodFlags, MethodBody).

:- pred create_generic_arg(int::in, mlds_var_name::out, mlds_argument::out)
    is det.

create_generic_arg(I, ArgName, Arg) :-
    ArgName = mlds_var_name("arg" ++ string.from_int(I), no),
    Arg = mlds_argument(ArgName, mlds_generic_type, gc_no_stmt).

:- pred generate_call_statement_for_addr(call_method_inputs::in,
    mlds_code_addr::in, statement::out) is det.

generate_call_statement_for_addr(InputArgs, CodeAddr, Statement) :-
    (
        CodeAddr = code_addr_proc(ProcLabel, OrigFuncSignature)
    ;
        CodeAddr = code_addr_internal(ProcLabel, _SeqNum, OrigFuncSignature)
    ),
    OrigFuncSignature = mlds_func_signature(OrigArgTypes, OrigRetTypes),
    ModuleName = ProcLabel ^ mod_name,

    % Create the arguments to pass to the original method.
    (
        InputArgs = cmi_separate(ArgNames),
        list.map_corresponding(generate_call_method_nth_arg(ModuleName),
            OrigArgTypes, ArgNames, CallArgs)
    ;
        InputArgs = cmi_array(ArrayVarName),
        ArrayVar = qual(ModuleName, module_qual, ArrayVarName),
        generate_call_method_args_from_array(OrigArgTypes, ArrayVar, 0,
            [], CallArgs)
    ),

    % Create a temporary variable to store the result of the call to the
    % original method.
    ReturnVarName = mlds_var_name("return_value", no),
    ReturnVar = qual(ModuleName, module_qual, ReturnVarName),

    % Create a declaration for this variable.
    (
        OrigRetTypes = [],
        ReturnVarType = mlds_generic_type
    ;
        OrigRetTypes = [CallRetType],
        ReturnVarType = CallRetType
    ;
        OrigRetTypes = [_, _ | _],
        ReturnVarType = mlds_array_type(mlds_generic_type)
    ),
    ReturnLval = ml_var(ReturnVar, ReturnVarType),
    ReturnEntityName = entity_data(mlds_data_var(ReturnVarName)),

    ReturnDecFlags = ml_gen_local_var_decl_flags,
    GCStatement = gc_no_stmt,  % The Java back-end does its own GC.
    ReturnEntityDefn = mlds_data(ReturnVarType, no_initializer, GCStatement),
    Context = mlds_make_context(term.context_init),
    ReturnVarDefn = mlds_defn(ReturnEntityName, Context, ReturnDecFlags,
        ReturnEntityDefn),
    MethodDefns = [ReturnVarDefn],

    % Create the call to the original method.
    CallRval = ml_const(mlconst_code_addr(CodeAddr)),

    % If the original method has a return type of void, then we obviously
    % cannot assign its return value to "return_value". Thus, in this
    % case the value returned by the call method will just be the value
    % which "return_value" was initialised to.
    (
        OrigRetTypes = [],
        CallRetLvals = []
    ;
        OrigRetTypes = [_ | _],
        CallRetLvals = [ReturnLval]
    ),
    Call = ml_stmt_call(OrigFuncSignature, CallRval, no, CallArgs,
        CallRetLvals, ordinary_call, set.init),
    CallStatement = statement(Call, Context),

    % Create a return statement that returns the result of the call to the
    % original method, boxed as a java.lang.Object.
    ReturnRval = ml_unop(box(ReturnVarType), ml_lval(ReturnLval)),
    Return = ml_stmt_return([ReturnRval]),
    ReturnStatement = statement(Return, Context),

    Block = ml_stmt_block(MethodDefns, [CallStatement, ReturnStatement]),
    Statement = statement(Block, Context).

:- pred generate_call_method_nth_arg(mlds_module_name::in, mlds_type::in,
    mlds_var_name::in, mlds_rval::out) is det.

generate_call_method_nth_arg(ModuleName, Type, MethodArgVariable, CallArg) :-
    CallArgLabel = qual(ModuleName, module_qual, MethodArgVariable),
    Rval = ml_lval(ml_var(CallArgLabel, mlds_generic_type)),
    CallArg = ml_unop(unbox(Type), Rval).

:- pred generate_call_method_args_from_array(list(mlds_type)::in,
    mlds_var::in, int::in, list(mlds_rval)::in, list(mlds_rval)::out) is det.

generate_call_method_args_from_array([], _, _, Args, Args).
generate_call_method_args_from_array([Type | Types], ArrayVar, Counter,
        Args0, Args) :-
    ArrayRval = ml_lval(ml_var(ArrayVar, mlds_native_int_type)),
    IndexRval = ml_const(mlconst_int(Counter)),
    ElemType = array_elem_scalar(scalar_elem_generic),
    Rval = ml_binop(array_index(ElemType), ArrayRval, IndexRval),
    UnBoxedRval = ml_unop(unbox(Type), Rval),
    Args1 = Args0 ++ [UnBoxedRval],
    generate_call_method_args_from_array(Types, ArrayVar, Counter + 1,
        Args1, Args).

:- func addr_wrapper_decl_flags = mlds_decl_flags.

addr_wrapper_decl_flags = MLDS_DeclFlags :-
    Access = acc_private,
    PerInstance = one_copy,
    Virtuality = non_virtual,
    Overridability = sealed,
    Constness = const,
    Abstractness = concrete,
    MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
        Virtuality, Overridability, Constness, Abstractness).

:- pred add_to_address_map(string::in, list(mlds_code_addr)::in,
    map(mlds_code_addr, code_addr_wrapper)::in,
    map(mlds_code_addr, code_addr_wrapper)::out) is det.

add_to_address_map(ClassName, CodeAddrs, !AddrOfMap) :-
    FlippedClassName = flip_initial_case(ClassName),
    (
        CodeAddrs = [CodeAddr],
        Wrapper = code_addr_wrapper(FlippedClassName, no),
        map.det_insert(CodeAddr, Wrapper, !AddrOfMap)
    ;
        CodeAddrs = [_, _ | _],
        add_to_address_map_2(FlippedClassName, CodeAddrs, 0, !AddrOfMap)
    ;
        CodeAddrs = [],
        unexpected($module, $pred, "no addresses")
    ).

:- pred add_to_address_map_2(string::in, list(mlds_code_addr)::in, int::in,
    map(mlds_code_addr, code_addr_wrapper)::in,
    map(mlds_code_addr, code_addr_wrapper)::out) is det.

add_to_address_map_2(_, [], _, !AddrOfMap).
add_to_address_map_2(FlippedClassName, [CodeAddr | CodeAddrs], I,
        !AddrOfMap) :-
    Wrapper = code_addr_wrapper(FlippedClassName, yes(I)),
    map.det_insert(CodeAddr, Wrapper, !AddrOfMap),
    add_to_address_map_2(FlippedClassName, CodeAddrs, I + 1, !AddrOfMap).

%-----------------------------------------------------------------------------%
%
% Code to rename long class names.
%

:- type class_name_renaming
    --->    class_name_renaming(
                cnr_module      :: mlds_module_name,
                cnr_renaming    :: map(mlds_class_name, mlds_class_name)
            ).

    % Rename class names which are too long. Each class results in a separate
    % `.class' file, so a long class name may exceed filesystem limits.
    % The long names tend to be automatically generated by the compiler.
    %
:- pred shorten_long_class_names(mlds_module_name::in,
    list(mlds_defn)::in, list(mlds_defn)::out) is det.

shorten_long_class_names(ModuleName, Defns0, Defns) :-
    list.map_foldl(maybe_shorten_long_class_name, Defns0, Defns1,
        map.init, RenamingMap),
    ( if map.is_empty(RenamingMap) then
        Defns = Defns1
    else
        Renaming = class_name_renaming(ModuleName, RenamingMap),
        list.map(rename_class_names_defn(Renaming), Defns1, Defns)
    ).

:- pred maybe_shorten_long_class_name(mlds_defn::in, mlds_defn::out,
    map(mlds_class_name, mlds_class_name)::in,
    map(mlds_class_name, mlds_class_name)::out) is det.

maybe_shorten_long_class_name(!Defn, !Renaming) :-
    Access = access(!.Defn ^ md_decl_flags),
    (
        % We only rename private classes for now.
        Access = acc_private,
        EntityName0 = !.Defn ^ md_entity_name,
        (
            EntityName0 = entity_type(ClassName0, Arity),
            ClassName = shorten_class_name(ClassName0),
            ( if ClassName = ClassName0 then
                true
            else
                EntityName = entity_type(ClassName, Arity),
                !Defn ^ md_entity_name := EntityName,
                map.det_insert(ClassName0, ClassName, !Renaming)
            )
        ;
            ( EntityName0 = entity_function(_, _, _, _)
            ; EntityName0 = entity_data(_)
            ; EntityName0 = entity_export(_)
            )
        )
    ;
        ( Access = acc_public
        ; Access = acc_protected
        ; Access = acc_default
        ; Access = acc_local
        )
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
        Right = string.right(ClassName0, 44),
        Hash = string.hash(ClassName0) /\ 0xffffffff,
        GenName = string.format("s_%s_%08x_%s", [s(Left), i(Hash), s(Right)]),
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

:- pred rename_class_names_defn(class_name_renaming::in,
    mlds_defn::in, mlds_defn::out) is det.

rename_class_names_defn(Renaming, !Defn) :-
    EntityDefn0 = !.Defn ^ md_entity_defn,
    (
        EntityDefn0 = mlds_data(Type0, Initializer0, GCStatement),
        rename_class_names_type(Renaming, Type0, Type),
        rename_class_names_initializer(Renaming, Initializer0, Initializer),
        EntityDefn = mlds_data(Type, Initializer, GCStatement)
    ;
        EntityDefn0 = mlds_function(MaybePPId, FuncParams0, FuncBody0,
            Attributes, EnvVarNames, MaybeRequireTailrecInfo),
        rename_class_names_func_params(Renaming, FuncParams0, FuncParams),
        (
            FuncBody0 = body_defined_here(Statement0),
            rename_class_names_statement(Renaming, Statement0, Statement),
            FuncBody = body_defined_here(Statement)
        ;
            FuncBody0 = body_external,
            FuncBody = body_external
        ),
        EntityDefn = mlds_function(MaybePPId, FuncParams, FuncBody,
            Attributes, EnvVarNames, MaybeRequireTailrecInfo)
    ;
        EntityDefn0 = mlds_class(mlds_class_defn(ClassKind, Imports, Inherits,
            Implements, TypeParams, Ctors0, Members0)),
        list.map(rename_class_names_defn(Renaming), Ctors0, Ctors),
        list.map(rename_class_names_defn(Renaming), Members0, Members),
        EntityDefn = mlds_class(mlds_class_defn(ClassKind, Imports, Inherits,
            Implements, TypeParams, Ctors, Members))
    ),
    !Defn ^ md_entity_defn := EntityDefn.

:- pred rename_class_names_type(class_name_renaming::in,
    mlds_type::in, mlds_type::out) is det.

rename_class_names_type(Renaming, !Type) :-
    (
        !.Type = mlds_mercury_array_type(Type0),
        rename_class_names_type(Renaming, Type0, Type),
        !:Type = mlds_mercury_array_type(Type)
    ;
        !.Type = mlds_cont_type(RetTypes0),
        list.map(rename_class_names_type(Renaming), RetTypes0, RetTypes),
        !:Type = mlds_cont_type(RetTypes)
    ;
        !.Type = mlds_class_type(Name0, Arity, ClassKind),
        Name0 = qual(ModuleName, QualKind, UnqualName0),
        ( if
            Renaming = class_name_renaming(ModuleName, RenamingMap),
            map.search(RenamingMap, UnqualName0, UnqualName)
        then
            Name = qual(ModuleName, QualKind, UnqualName),
            !:Type = mlds_class_type(Name, Arity, ClassKind)
        else
            true
        )
    ;
        !.Type = mlds_array_type(Type0),
        rename_class_names_type(Renaming, Type0, Type),
        !:Type = mlds_array_type(Type)
    ;
        !.Type = mlds_mostly_generic_array_type(Types0),
        list.map(rename_class_names_type(Renaming), Types0, Types),
        !:Type = mlds_mostly_generic_array_type(Types)
    ;
        !.Type = mlds_ptr_type(Type0),
        rename_class_names_type(Renaming, Type0, Type),
        !:Type = mlds_ptr_type(Type)
    ;
        !.Type = mlds_func_type(FuncParams0),
        rename_class_names_func_params(Renaming, FuncParams0, FuncParams),
        !:Type = mlds_func_type(FuncParams)
    ;
        ( !.Type = mercury_type(_, _, _)
        ; !.Type = mlds_commit_type
        ; !.Type = mlds_native_bool_type
        ; !.Type = mlds_native_int_type
        ; !.Type = mlds_native_uint_type
        ; !.Type = mlds_native_float_type
        ; !.Type = mlds_native_char_type
        ; !.Type = mlds_foreign_type(_)
        ; !.Type = mlds_generic_type
        ; !.Type = mlds_generic_env_ptr_type
        ; !.Type = mlds_type_info_type
        ; !.Type = mlds_pseudo_type_info_type
        ; !.Type = mlds_rtti_type(_)
        ; !.Type = mlds_tabling_type(_)
        ; !.Type = mlds_unknown_type
        )
    ).

:- pred rename_class_names_initializer(class_name_renaming::in,
    mlds_initializer::in, mlds_initializer::out) is det.

rename_class_names_initializer(Renaming, !Initializer) :-
    (
        !.Initializer = init_obj(Rval0),
        rename_class_names_rval(Renaming, Rval0, Rval),
        !:Initializer = init_obj(Rval)
    ;
        !.Initializer = init_struct(Type0, Initializers0),
        rename_class_names_type(Renaming, Type0, Type),
        list.map(rename_class_names_initializer(Renaming), Initializers0,
            Initializers),
        !:Initializer = init_struct(Type, Initializers)
    ;
        !.Initializer = init_array(Initializers0),
        list.map(rename_class_names_initializer(Renaming), Initializers0,
            Initializers),
        !:Initializer = init_array(Initializers)
    ;
        !.Initializer = no_initializer
    ).

:- pred rename_class_names_func_params(class_name_renaming::in,
    mlds_func_params::in, mlds_func_params::out) is det.

rename_class_names_func_params(Renaming, !FuncParams) :-
    !.FuncParams = mlds_func_params(Arguments0, RetTypes0),
    list.map(rename_class_names_argument(Renaming), Arguments0, Arguments),
    list.map(rename_class_names_type(Renaming), RetTypes0, RetTypes),
    !:FuncParams = mlds_func_params(Arguments, RetTypes).

:- pred rename_class_names_argument(class_name_renaming::in,
    mlds_argument::in, mlds_argument::out) is det.

rename_class_names_argument(Renaming, !Argument) :-
    !.Argument = mlds_argument(Name, Type0, GCStatement),
    rename_class_names_type(Renaming, Type0, Type),
    !:Argument = mlds_argument(Name, Type, GCStatement).

:- pred rename_class_names_statement(class_name_renaming::in,
    statement::in, statement::out) is det.

rename_class_names_statement(Renaming, !Statement) :-
    !.Statement = statement(Stmt0, Context),
    rename_class_names_stmt(Renaming, Stmt0, Stmt),
    !:Statement = statement(Stmt, Context).

:- pred rename_class_names_stmt(class_name_renaming::in,
    mlds_stmt::in, mlds_stmt::out) is det.

rename_class_names_stmt(Renaming, !Stmt) :-
    (
        !.Stmt = ml_stmt_block(Defns0, Statements0),
        list.map(rename_class_names_defn(Renaming), Defns0, Defns),
        list.map(rename_class_names_statement(Renaming),
            Statements0, Statements),
        !:Stmt = ml_stmt_block(Defns, Statements)
    ;
        !.Stmt = ml_stmt_while(Kind, Rval0, Statement0),
        rename_class_names_rval(Renaming, Rval0, Rval),
        rename_class_names_statement(Renaming, Statement0, Statement),
        !:Stmt = ml_stmt_while(Kind, Rval, Statement)
    ;
        !.Stmt = ml_stmt_if_then_else(Rval0, Statement0, MaybeElse0),
        rename_class_names_rval(Renaming, Rval0, Rval),
        rename_class_names_statement(Renaming, Statement0, Statement),
        (
            MaybeElse0 = yes(Else0),
            rename_class_names_statement(Renaming, Else0, Else),
            MaybeElse = yes(Else)
        ;
            MaybeElse0 = no,
            MaybeElse = no
        ),
        !:Stmt = ml_stmt_if_then_else(Rval, Statement, MaybeElse)
    ;
        !.Stmt = ml_stmt_switch(Type0, Rval0, SwitchRange, Cases0, Default0),
        rename_class_names_type(Renaming, Type0, Type),
        rename_class_names_rval(Renaming, Rval0, Rval),
        list.map(rename_class_names_switch_case(Renaming), Cases0, Cases),
        rename_class_names_switch_default(Renaming, Default0, Default),
        !:Stmt = ml_stmt_switch(Type, Rval, SwitchRange, Cases, Default)
    ;
        !.Stmt = ml_stmt_label(_)
    ;
        !.Stmt = ml_stmt_goto(_)
    ;
        !.Stmt = ml_stmt_computed_goto(Rval0, Labels),
        rename_class_names_rval(Renaming, Rval0, Rval),
        !:Stmt = ml_stmt_computed_goto(Rval, Labels)
    ;
        !.Stmt = ml_stmt_call(Signature0, Rval0, MaybeThis, Rvals0, RetLvals0,
            CallKind, Markers),
        Signature0 = mlds_func_signature(ArgTypes0, RetTypes0),
        list.map(rename_class_names_type(Renaming), ArgTypes0, ArgTypes),
        list.map(rename_class_names_type(Renaming), RetTypes0, RetTypes),
        Signature = mlds_func_signature(ArgTypes, RetTypes),
        rename_class_names_rval(Renaming, Rval0, Rval),
        list.map(rename_class_names_rval(Renaming), Rvals0, Rvals),
        list.map(rename_class_names_lval(Renaming), RetLvals0, RetLvals),
        !:Stmt = ml_stmt_call(Signature, Rval, MaybeThis, Rvals, RetLvals,
            CallKind, Markers)
    ;
        !.Stmt = ml_stmt_return(Rvals0),
        list.map(rename_class_names_rval(Renaming), Rvals0, Rvals),
        !:Stmt = ml_stmt_return(Rvals)
    ;
        !.Stmt = ml_stmt_try_commit(Lval0, StatementA0, StatementB0),
        rename_class_names_lval(Renaming, Lval0, Lval),
        rename_class_names_statement(Renaming, StatementA0, StatementA),
        rename_class_names_statement(Renaming, StatementB0, StatementB),
        !:Stmt = ml_stmt_try_commit(Lval, StatementA, StatementB)
    ;
        !.Stmt = ml_stmt_do_commit(Rval0),
        rename_class_names_rval(Renaming, Rval0, Rval),
        !:Stmt = ml_stmt_do_commit(Rval)
    ;
        !.Stmt = ml_stmt_atomic(AtomicStatement0),
        rename_class_names_atomic(Renaming, AtomicStatement0, AtomicStatement),
        !:Stmt = ml_stmt_atomic(AtomicStatement)
    ).

:- pred rename_class_names_switch_case(class_name_renaming::in,
    mlds_switch_case::in, mlds_switch_case::out) is det.

rename_class_names_switch_case(Renaming, !Case) :-
    !.Case = mlds_switch_case(FirstMatchCond, LaterMatchConds, Statement0),
    % The rvals in the match conditions shouldn't need renaming.
    rename_class_names_statement(Renaming, Statement0, Statement),
    !:Case = mlds_switch_case(FirstMatchCond, LaterMatchConds, Statement).

:- pred rename_class_names_switch_default(class_name_renaming::in,
    mlds_switch_default::in, mlds_switch_default::out) is det.

rename_class_names_switch_default(Renaming, !Default) :-
    (
        !.Default = default_is_unreachable
    ;
        !.Default = default_do_nothing
    ;
        !.Default = default_case(Statement0),
        rename_class_names_statement(Renaming, Statement0, Statement),
        !:Default = default_case(Statement)
    ).

:- pred rename_class_names_atomic(class_name_renaming::in,
    mlds_atomic_statement::in, mlds_atomic_statement::out) is det.

rename_class_names_atomic(Renaming, !Statement) :-
    (
        !.Statement = assign(Lval0, Rval0),
        rename_class_names_lval(Renaming, Lval0, Lval),
        rename_class_names_rval(Renaming, Rval0, Rval),
        !:Statement = assign(Lval, Rval)
    ;
        !.Statement = assign_if_in_heap(Lval0, Rval0),
        rename_class_names_lval(Renaming, Lval0, Lval),
        rename_class_names_rval(Renaming, Rval0, Rval),
        !:Statement = assign_if_in_heap(Lval, Rval)
    ;
        !.Statement = delete_object(Rval0),
        rename_class_names_rval(Renaming, Rval0, Rval),
        !:Statement = delete_object(Rval)
    ;
        !.Statement = new_object(TargetLval0, MaybeTag, ExplicitSecTag, Type0,
            MaybeSize, MaybeCtorName, Args0, ArgTypes0, MayUseAtomic, AllocId),
        rename_class_names_lval(Renaming, TargetLval0, TargetLval),
        rename_class_names_type(Renaming, Type0, Type),
        list.map(rename_class_names_rval(Renaming), Args0, Args),
        list.map(rename_class_names_type(Renaming), ArgTypes0, ArgTypes),
        !:Statement = new_object(TargetLval, MaybeTag, ExplicitSecTag, Type,
            MaybeSize, MaybeCtorName, Args, ArgTypes, MayUseAtomic, AllocId)
    ;
        !.Statement = inline_target_code(Lang, Components0),
        (
            Lang = ml_target_java,
            list.map(rename_class_names_target_code_component(Renaming),
                Components0, Components),
            !:Statement = inline_target_code(Lang, Components)
        ;
            ( Lang = ml_target_c
            ; Lang = ml_target_gnu_c
            ; Lang = ml_target_csharp
            )
        )
    ;
        ( !.Statement = comment(_)
        ; !.Statement = gc_check
        ; !.Statement = mark_hp(_)
        ; !.Statement = restore_hp(_)
        ; !.Statement = trail_op(_)
        ; !.Statement = outline_foreign_proc(_, _, _, _)
        )
    ).

:- pred rename_class_names_lval(class_name_renaming::in,
    mlds_lval::in, mlds_lval::out) is det.

rename_class_names_lval(Renaming, !Lval) :-
    (
        !.Lval = ml_field(Tag, Address0, FieldId0, FieldType0, PtrType0),
        rename_class_names_rval(Renaming, Address0, Address),
        rename_class_names_field_id(Renaming, FieldId0, FieldId),
        rename_class_names_type(Renaming, FieldType0, FieldType),
        rename_class_names_type(Renaming, PtrType0, PtrType),
        !:Lval = ml_field(Tag, Address, FieldId, FieldType, PtrType)
    ;
        !.Lval = ml_mem_ref(Rval0, Type0),
        rename_class_names_rval(Renaming, Rval0, Rval),
        rename_class_names_type(Renaming, Type0, Type),
        !:Lval = ml_mem_ref(Rval, Type)
    ;
        !.Lval = ml_global_var_ref(_)
    ;
        !.Lval = ml_var(Var, Type0),
        rename_class_names_type(Renaming, Type0, Type),
        !:Lval = ml_var(Var, Type)
    ).

:- pred rename_class_names_field_id(class_name_renaming::in,
    mlds_field_id::in, mlds_field_id::out) is det.

rename_class_names_field_id(Renaming, !FieldId) :-
    (
        !.FieldId = ml_field_offset(Rval0),
        rename_class_names_rval(Renaming, Rval0, Rval),
        !:FieldId = ml_field_offset(Rval)
    ;
        !.FieldId = ml_field_named(Name, Type0),
        rename_class_names_type(Renaming, Type0, Type),
        !:FieldId = ml_field_named(Name, Type)
    ).

:- pred rename_class_names_rval(class_name_renaming::in,
    mlds_rval::in, mlds_rval::out) is det.

rename_class_names_rval(Renaming, !Rval) :-
    (
        !.Rval = ml_lval(Lval0),
        rename_class_names_lval(Renaming, Lval0, Lval),
        !:Rval = ml_lval(Lval)
    ;
        !.Rval = ml_mkword(Tag, Rval0),
        rename_class_names_rval(Renaming, Rval0, Rval),
        !:Rval = ml_mkword(Tag, Rval)
    ;
        !.Rval = ml_const(RvalConst0),
        rename_class_names_rval_const(Renaming, RvalConst0, RvalConst),
        !:Rval = ml_const(RvalConst)
    ;
        !.Rval = ml_unop(Op0, Rval0),
        rename_class_names_unary_op(Renaming, Op0, Op),
        rename_class_names_rval(Renaming, Rval0, Rval),
        !:Rval = ml_unop(Op, Rval)
    ;
        !.Rval = ml_binop(Op, RvalA0, RvalB0),
        rename_class_names_rval(Renaming, RvalA0, RvalA),
        rename_class_names_rval(Renaming, RvalB0, RvalB),
        !:Rval = ml_binop(Op, RvalA, RvalB)
    ;
        !.Rval = ml_mem_addr(Lval0),
        rename_class_names_lval(Renaming, Lval0, Lval),
        !:Rval = ml_mem_addr(Lval)
    ;
        !.Rval = ml_scalar_common(_)
    ;
        !.Rval = ml_vector_common_row(VectorCommon, RowRval0),
        rename_class_names_rval(Renaming, RowRval0, RowRval),
        !:Rval = ml_vector_common_row(VectorCommon, RowRval)
    ;
        !.Rval = ml_self(Type0),
        rename_class_names_type(Renaming, Type0, Type),
        !:Rval = ml_self(Type)
    ).

:- pred rename_class_names_rval_const(class_name_renaming::in,
    mlds_rval_const::in, mlds_rval_const::out) is det.

rename_class_names_rval_const(Renaming, !Const) :-
    (
        !.Const = mlconst_foreign(Lang, String, Type0),
        rename_class_names_type(Renaming, Type0, Type),
        !:Const = mlconst_foreign(Lang, String, Type)
    ;
        !.Const = mlconst_null(Type0),
        rename_class_names_type(Renaming, Type0, Type),
        !:Const = mlconst_null(Type)
    ;
        ( !.Const = mlconst_true
        ; !.Const = mlconst_false
        ; !.Const = mlconst_int(_)
        ; !.Const = mlconst_uint(_)
        ; !.Const = mlconst_char(_)
        ; !.Const = mlconst_enum(_, _)
        ; !.Const = mlconst_float(_)
        ; !.Const = mlconst_string(_)
        ; !.Const = mlconst_multi_string(_)
        ; !.Const = mlconst_named_const(_)
        ; !.Const = mlconst_code_addr(_)
        ; !.Const = mlconst_data_addr(_)
        )
    ).

:- pred rename_class_names_unary_op(class_name_renaming::in,
    mlds_unary_op::in, mlds_unary_op::out) is det.

rename_class_names_unary_op(Renaming, !Op) :-
    (
        !.Op = box(Type0),
        rename_class_names_type(Renaming, Type0, Type),
        !:Op = box(Type)
    ;
        !.Op = unbox(Type0),
        rename_class_names_type(Renaming, Type0, Type),
        !:Op = unbox(Type)
    ;
        !.Op = cast(Type0),
        rename_class_names_type(Renaming, Type0, Type),
        !:Op = cast(Type)
    ;
        !.Op = std_unop(_)
    ).

:- pred rename_class_names_target_code_component(class_name_renaming::in,
    target_code_component::in, target_code_component::out) is det.

rename_class_names_target_code_component(Renaming, !Component) :-
    (
        !.Component = user_target_code(_, _)
    ;
        !.Component = raw_target_code(_)
    ;
        !.Component = target_code_alloc_id(_)
    ;
        !.Component = target_code_input(Rval0),
        rename_class_names_rval(Renaming, Rval0, Rval),
        !:Component = target_code_input(Rval)
    ;
        !.Component = target_code_output(Lval0),
        rename_class_names_lval(Renaming, Lval0, Lval),
        !:Component = target_code_output(Lval)
    ;
        !.Component = target_code_type(Type0),
        rename_class_names_type(Renaming, Type0, Type),
        !:Component = target_code_type(Type)
    ;
        !.Component = target_code_name(QualName0),
        QualName0 = qual(ModuleName, Kind, EntityName0),
        (
            EntityName0 = entity_type(ClassName0, Arity),
            ( if
                Renaming = class_name_renaming(ModuleName, RenamingMap),
                map.search(RenamingMap, ClassName0, ClassName)
            then
                EntityName = entity_type(ClassName, Arity),
                QualName = qual(ModuleName, Kind, EntityName),
                !:Component = target_code_name(QualName)
            else
                true
            )
        ;
            ( EntityName0 = entity_function(_, _, _, _)
            ; EntityName0 = entity_data(_)
            ; EntityName0 = entity_export(_)
            )
        )
    ).

%-----------------------------------------------------------------------------%
%
% Code to output calls to module initialisers.
%

:- pred output_inits(int::in, list(string)::in, io::di, io::uo) is det.

output_inits(Indent, InitPreds, !IO) :-
    (
        InitPreds = []
    ;
        InitPreds = [_ | _],
        % We call the initialisation predicates from a static initialisation
        % block.
        indent_line(Indent, !IO),
        io.write_string("static {\n", !IO),
        list.foldl(output_init_2(Indent + 1), InitPreds, !IO),
        indent_line(Indent, !IO),
        io.write_string("}\n", !IO)
    ).

:- pred output_init_2(int::in, string::in, io::di, io::uo) is det.

output_init_2(Indent, InitPred, !IO) :-
    indent_line(Indent, !IO),
    io.write_string(InitPred, !IO),
    io.write_string("();\n", !IO).

%-----------------------------------------------------------------------------%
%
% Code to output module finalisers.
%

:- pred output_finals(indent::in, list(string)::in, io::di, io::uo) is det.

output_finals(Indent, FinalPreds, !IO) :-
    (
        FinalPreds = []
    ;
        FinalPreds = [_ | _],
        indent_line(Indent, !IO),
        io.write_string("static {\n", !IO),
        indent_line(Indent + 1, !IO),
        io.write_string("jmercury.runtime.JavaInternal.register_finaliser(\n",
            !IO),
        indent_line(Indent + 2, !IO),
        io.write_string("new java.lang.Runnable() {\n", !IO),
        indent_line(Indent + 3, !IO),
        io.write_string("public void run() {\n", !IO),
        list.foldl(output_final_pred_call(Indent + 4), FinalPreds, !IO),
        indent_line(Indent + 3, !IO),
        io.write_string("}\n", !IO),
        indent_line(Indent + 2, !IO),
        io.write_string("}\n", !IO),
        indent_line(Indent + 1, !IO),
        io.write_string(");\n", !IO),
        indent_line(Indent, !IO),
        io.write_string("}\n", !IO)
    ).

:- pred output_final_pred_call(indent::in, string::in, io::di, io::uo) is det.

output_final_pred_call(Indent, FinalPred, !IO) :-
    indent_line(Indent, !IO),
    io.write_string(FinalPred, !IO),
    io.write_string("();\n", !IO).

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
        EntityDefn = mlds_function(_, _, _, _, EnvVarNames, _),
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
    io.write_string("java.lang.System.getenv(\"", !IO),
    io.write_string(EnvVarName, !IO),
    io.write_string("\") == null ? 0 : 1;\n", !IO).

%-----------------------------------------------------------------------------%
%
% Code to output the start and end of a source file.
%

:- pred output_src_start(java_out_info::in, indent::in,
    mercury_module_name::in, mlds_imports::in, list(foreign_decl_code)::in,
    list(mlds_defn)::in, io::di, io::uo) is det.

output_src_start(Info, Indent, MercuryModuleName, Imports, ForeignDecls, Defns,
        !IO) :-
    output_auto_gen_comment(Info, !IO),
    indent_line(Indent, !IO),
    io.write_string("/* :- module ", !IO),
    prog_out.write_sym_name(MercuryModuleName, !IO),
    io.write_string(". */\n\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("package jmercury;\n", !IO),

    output_imports(Imports, !IO),
    io.write_list(ForeignDecls, "\n", output_java_decl(Info, Indent), !IO),
    io.write_string("public class ", !IO),
    mangle_sym_name_for_java(MercuryModuleName, module_qual, "__", ClassName),
    io.write_string(ClassName, !IO),
    io.write_string(" {\n", !IO),

    output_debug_class_init(MercuryModuleName, "start", !IO),

    % Check if this module contains a `main' predicate and if it does insert
    % a `main' method in the resulting Java class that calls the `main'
    % predicate.
    ( if defns_contain_main(Defns) then
        write_main_driver(Indent + 1, ClassName, !IO)
    else
        true
    ).

:- pred write_main_driver(indent::in, string::in, io::di, io::uo) is det.

write_main_driver(Indent, ClassName, !IO) :-
    indent_line(Indent, !IO),
    io.write_string("public static void main", !IO),
    io.write_string("(java.lang.String[] args)\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("{\n", !IO),

    % Save the progname and command line arguments in the class variables
    % of `jmercury.runtime.JavaInternal', as well as setting the default
    % exit status.
    Body = [
        "jmercury.runtime.JavaInternal.progname = """ ++ ClassName ++ """;",
        "jmercury.runtime.JavaInternal.args = args;",
        "jmercury.runtime.JavaInternal.exit_status = 0;",
        "library.ML_std_library_init();",
        "benchmarking.ML_initialise();",
        "Runnable run_main = new Runnable() {",
        "    public void run() {",
        "        " ++ ClassName ++ ".main_2_p_0();",
        "    }",
        "};",
        "jmercury.runtime.JavaInternal.runMain(run_main);",
        "io.flush_output_3_p_0(io.stdout_stream_0_f_0());",
        "io.flush_output_3_p_0(io.stderr_stream_0_f_0());",
        "java.lang.System.exit(jmercury.runtime.JavaInternal.exit_status);"
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
    output_debug_class_init(ModuleName, "end", !IO),
    io.write_string("}\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("// :- end_module ", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(".\n", !IO).

:- pred output_debug_class_init(mercury_module_name::in, string::in,
    io::di, io::uo) is det.

output_debug_class_init(ModuleName, State, !IO) :-
    list.foldl(io.write_string, [
        "  static {\n",
        "    if (System.getenv(""MERCURY_DEBUG_CLASS_INIT"") != null) {\n",
        "      System.out.println(""[", sym_name_mangle(ModuleName),
        " ", State, " init]"");\n",
        "    }\n",
        "  }\n"
    ], !IO).

    % Output a Java comment saying that the file was automatically
    % generated and give details such as the compiler version.
    %
:- pred output_auto_gen_comment(java_out_info::in, io::di, io::uo) is det.

output_auto_gen_comment(Info, !IO)  :-
    library.version(Version, Fullarch),
    SourceFileName = Info ^ joi_source_filename,
    io.write_string("//\n//\n// Automatically generated from ", !IO),
    io.write_string(SourceFileName, !IO),
    io.write_string(" by the Mercury Compiler,\n", !IO),
    io.write_string("// version ", !IO),
    io.write_string(Version, !IO),
    io.nl(!IO),
    io.write_string("// configured for ", !IO),
    io.write_string(Fullarch, !IO),
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

:- pred output_defns(java_out_info::in, indent::in, output_aux::in,
    list(mlds_defn)::in, io::di, io::uo) is det.

output_defns(Info, Indent, OutputAux, Defns, !IO) :-
    list.foldl(output_defn(Info, Indent, OutputAux), Defns, !IO).

:- pred output_defn(java_out_info::in, indent::in, output_aux::in,
    mlds_defn::in, io::di, io::uo) is det.

output_defn(Info, Indent, OutputAux, Defn, !IO) :-
    Defn = mlds_defn(Name, Context, Flags, DefnBody),
    indent_line(Info ^ joi_line_numbers, marker_comment, Context, Indent, !IO),
    ( if DefnBody = mlds_function(_, _, body_external, _, _, _) then
        % This is just a function declaration, with no body.
        % Java doesn't support separate declarations and definitions,
        % so just output the declaration as a comment.
        % (Note that the actual definition of an external procedure
        % must be given in `pragma java_code' in the same module.)
        io.write_string("/* external:\n", !IO),
        output_decl_flags(Info, Flags, !IO),
        output_defn_body(Info, Indent, Name, OutputAux, Context, DefnBody,
            !IO),
        io.write_string("*/\n", !IO)
    else
        output_decl_flags(Info, Flags, !IO),
        output_defn_body(Info, Indent, Name, OutputAux, Context, DefnBody,
            !IO)
    ).

:- pred output_defn_body(java_out_info::in, indent::in, mlds_entity_name::in,
    output_aux::in, mlds_context::in, mlds_entity_defn::in, io::di, io::uo)
    is det.

output_defn_body(Info, Indent, UnqualName, OutputAux, Context, Entity, !IO) :-
    (
        Entity = mlds_data(Type, Initializer, _),
        output_data_defn(Info, UnqualName, OutputAux, Type, Initializer,
            !IO)
    ;
        Entity = mlds_function(MaybePredProcId, Signature, MaybeBody,
            _Attributes, _EnvVarNames, _MaybeRequireTailrecInfo),
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

:- pred output_class(java_out_info::in, indent::in, mlds_entity_name::in,
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
        unexpected($module, $pred, "name is not entity_type")
    ),
    ClassDefn = mlds_class_defn(Kind, _Imports, BaseClasses, Implements,
        TypeParams, Ctors, AllMembers),

    !Info ^ joi_univ_tvars := TypeParams,

    % Use generics in the output if this class represents a Mercury type.
    ( if list.member(ml_java_mercury_type_interface, Implements) then
        !Info ^ joi_output_generics := do_output_generics
    else
        true
    ),

    output_class_kind(Kind, !IO),
    output_unqual_class_name_for_java(ClassName, Arity, !IO),
    OutputGenerics = !.Info ^ joi_output_generics,
    (
        OutputGenerics = do_output_generics,
        output_generic_tvars(TypeParams, !IO)
    ;
        OutputGenerics = do_not_output_generics
    ),
    io.nl(!IO),

    output_extends_list(!.Info, Indent + 1, BaseClasses, !IO),
    output_implements_list(Indent + 1, Implements, !IO),
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
        ; Kind = mlds_enum
        ; Kind = mlds_struct
        ),
        io.write_string("class ", !IO)
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

    % Output superclass that this class extends. Java does not support
    % multiple inheritance, so more than one superclass is an error.
    %
:- pred output_extends_list(java_out_info::in, indent::in,
    list(mlds_class_id)::in, io::di, io::uo) is det.

output_extends_list(_, _, [], !IO).
output_extends_list(Info, Indent, [SuperClass], !IO) :-
    indent_line(Indent, !IO),
    io.write_string("extends ", !IO),
    output_type(Info, SuperClass, !IO),
    io.nl(!IO).
output_extends_list(_, _, [_, _ | _], _, _) :-
    unexpected($module, $pred, "multiple inheritance not supported in Java").

    % Output list of interfaces that this class implements.
    %
:- pred output_implements_list(indent::in, list(mlds_interface_id)::in,
    io::di, io::uo) is det.

output_implements_list(Indent, InterfaceList, !IO)  :-
    (
        InterfaceList = []
    ;
        InterfaceList = [_ | _],
        indent_line(Indent, !IO),
        io.write_string("implements ", !IO),
        io.write_list(InterfaceList, ",", output_interface, !IO),
        io.nl(!IO)
    ).

:- pred output_interface(mlds_interface_id::in, io::di, io::uo) is det.

output_interface(Interface, !IO) :-
    ( if
        Interface = mlds_class_type(qual(ModuleQualifier, QualKind, Name),
            Arity, _)
    then
        SymName = mlds_module_name_to_sym_name(ModuleQualifier),
        mangle_sym_name_for_java(SymName, convert_qual_kind(QualKind),
            ".", ModuleName),
        io.format("%s.%s", [s(ModuleName), s(Name)], !IO),

        % Check if the interface is one of the ones in the runtime system.
        % If it is, we don't need to output the arity.
        ( if interface_is_special(Name) then
            true
        else
            io.format("%d", [i(Arity)], !IO)
        )
    else
        unexpected($module, $pred, "interface was not a class")
    ).

:- pred output_class_body(java_out_info::in, indent::in, mlds_class_kind::in,
    mlds_entity_name::in, list(mlds_defn)::in, io::di, io::uo) is det.

output_class_body(Info, Indent, Kind, UnqualName, AllMembers, !IO) :-
    (
        Kind = mlds_class,
        output_defns(Info, Indent, none, AllMembers, !IO)
    ;
        Kind = mlds_package,
        unexpected($module, $pred, "cannot use package as a type")
    ;
        Kind = mlds_interface,
        output_defns(Info, Indent, none, AllMembers, !IO)
    ;
        Kind = mlds_struct,
        unexpected($module, $pred, "structs not supported in Java")
    ;
        Kind = mlds_enum,
        list.filter(defn_is_const, AllMembers, EnumConsts),
        output_enum_constants(Info, Indent + 1, UnqualName, EnumConsts, !IO),
        io.nl(!IO),
        output_enum_ctor(Indent + 1, UnqualName, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Additional code for generating enumerations.
%
% Enumerations are a bit different from normal classes because although the
% ml code generator generates them as classes, it treats them as integers.
% Here we treat them as objects (instantiations of the classes) rather than
% just as integers.

:- pred defn_is_const(mlds_defn::in) is semidet.

defn_is_const(Defn) :-
    Defn = mlds_defn(_Name, _Context, Flags, _DefnBody),
    constness(Flags) = const.

    % Output a (Java) constructor for the class representing the enumeration.
    %
:- pred output_enum_ctor(indent::in, mlds_entity_name::in, io::di, io::uo)
    is det.

output_enum_ctor(Indent, UnqualName, !IO) :-
    indent_line(Indent, !IO),
    io.write_string("private ", !IO),
    output_entity_name_for_java(UnqualName, !IO),
    io.write_string("(int val) {\n", !IO),
    indent_line(Indent + 1, !IO),
    % Call the MercuryEnum constructor, which will set the MR_value field.
    io.write_string("super(val);\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_enum_constants(java_out_info::in, indent::in,
    mlds_entity_name::in, list(mlds_defn)::in, io::di, io::uo) is det.

output_enum_constants(Info, Indent, EnumName, EnumConsts, !IO) :-
    io.write_list(EnumConsts, "\n",
        output_enum_constant(Info, Indent, EnumName), !IO),
    io.nl(!IO).

:- pred output_enum_constant(java_out_info::in, indent::in,
    mlds_entity_name::in, mlds_defn::in, io::di, io::uo) is det.

output_enum_constant(_Info, Indent, EnumName, Defn, !IO) :-
    Defn = mlds_defn(Name, _Context, _Flags, DefnBody),
    ( if DefnBody = mlds_data(_Type, Initializer, _GCStatement) then
        % Make a static instance of the constant. The MLDS doesn't retain enum
        % constructor names (that shouldn't be hard to change now) so it is
        % easier to derive the name of the constant later by naming them after
        % the integer values.
        (
            Initializer = init_obj(Rval),
            ( if Rval = ml_const(mlconst_enum(N, _)) then
                indent_line(Indent, !IO),
                io.write_string("public static final ", !IO),
                output_entity_name_for_java(EnumName, !IO),
                io.format(" K%d = new ", [i(N)], !IO),
                output_entity_name_for_java(EnumName, !IO),
                io.format("(%d); ", [i(N)], !IO),

                io.write_string(" /* ", !IO),
                output_entity_name_for_java(Name, !IO),
                io.write_string(" */", !IO)
            else
                unexpected($module, $pred, "not mlconst_enum")
            )
        ;
            ( Initializer = no_initializer
            ; Initializer = init_struct(_, _)
            ; Initializer = init_array(_)
            ),
            unexpected($module, $pred, "not mlconst_enum")
        )
    else
        unexpected($module, $pred, "definition body was not data")
    ).

%-----------------------------------------------------------------------------%
%
% Code to output data declarations/definitions.
%

:- pred output_data_decls(java_out_info::in, indent::in, list(mlds_defn)::in,
    io::di, io::uo) is det.

output_data_decls(_, _, [], !IO).
output_data_decls(Info, Indent, [Defn | Defns], !IO) :-
    Defn = mlds_defn(Name, _Context, Flags, DefnBody),
    ( if DefnBody = mlds_data(Type, _Initializer, _GCStatement) then
        indent_line(Indent, !IO),
        output_decl_flags(Info, Flags, !IO),
        output_data_decl(Info, Name, Type, !IO),
        io.write_string(";\n", !IO)
    else
        unexpected($module, $pred, "not data")
    ),
    output_data_decls(Info, Indent, Defns, !IO).

:- pred output_data_decl(java_out_info::in, mlds_entity_name::in,
    mlds_type::in, io::di, io::uo) is det.

output_data_decl(Info, Name, Type, !IO) :-
    output_type(Info, Type, !IO),
    io.write_char(' ', !IO),
    output_entity_name_for_java(Name, !IO).

:- pred output_data_assignments(java_out_info::in, indent::in,
    list(mlds_defn)::in, io::di, io::uo) is det.

output_data_assignments(Info, Indent, Defns, !IO) :-
    % Divide into small methods to avoid running into the maximum method size
    % limit.
    list.chunk(Defns, 1000, DefnChunks),
    list.foldl2(output_init_data_method(Info, Indent),
        DefnChunks, 0, NumChunks, !IO),

    % Call the individual methods.
    indent_line(Indent, !IO),
    io.write_string("static {\n", !IO),
    int.fold_up(output_call_init_data_method(Indent + 1), 0, NumChunks - 1,
        !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_init_data_method(java_out_info::in, indent::in,
    list(mlds_defn)::in, int::in, int::out, io::di, io::uo) is det.

output_init_data_method(Info, Indent, Defns, Chunk, Chunk + 1, !IO) :-
    indent_line(Indent, !IO),
    io.format("private static void MR_init_data_%d() {\n", [i(Chunk)], !IO),
    output_init_data_statements(Info, Indent + 1, Defns, !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_init_data_statements(java_out_info::in, indent::in,
    list(mlds_defn)::in, io::di, io::uo) is det.

output_init_data_statements(_, _, [], !IO).
output_init_data_statements(Info, Indent, [Defn | Defns], !IO) :-
    Defn = mlds_defn(Name, _Context, _Flags, DefnBody),
    ( if DefnBody = mlds_data(Type, Initializer, _GCStatement) then
        indent_line(Indent, !IO),
        output_entity_name_for_java(Name, !IO),
        output_initializer(Info, none, Type, Initializer, !IO),
        io.write_string(";\n", !IO)
    else
        unexpected($module, $pred, "not mlds_data")
    ),
    output_init_data_statements(Info, Indent, Defns, !IO).

:- pred output_call_init_data_method(indent::in, int::in, io::di, io::uo)
    is det.

output_call_init_data_method(Indent, I, !IO) :-
    indent_line(Indent, !IO),
    io.format("MR_init_data_%d();\n", [i(I)], !IO).

:- pred output_data_defn(java_out_info::in, mlds_entity_name::in,
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

:- pred output_scalar_common_data(java_out_info::in, indent::in,
    ml_scalar_cell_map::in, io::di, io::uo) is det.

output_scalar_common_data(Info, Indent, ScalarCellGroupMap, !IO) :-
    % Elements of scalar data arrays may reference elements in higher-numbered
    % arrays, or elements of the same array, so we must initialise them
    % separately in a static initialisation block, and we must ensure that
    % elements which are referenced by other elements are initialised first.
    map.foldl3(output_scalar_defns(Info, Indent), ScalarCellGroupMap,
        digraph.init, Graph, map.init, Map, !IO),

    ( if digraph.tsort(Graph, SortedScalars0) then
        % Divide into small methods to avoid running into the maximum method
        % size limit.
        list.reverse(SortedScalars0, SortedScalars),
        list.chunk(SortedScalars, 1000, ScalarChunks),
        list.foldl2(output_scalar_init_method(Info, Indent, Map),
            ScalarChunks, 0, NumChunks, !IO),

        % Call the individual methods.
        indent_line(Indent, !IO),
        io.write_string("static {\n", !IO),
        int.fold_up(output_call_scalar_init_method(Indent + 1),
            0, NumChunks - 1, !IO),
        indent_line(Indent, !IO),
        io.write_string("}\n", !IO)
    else
        unexpected($module, $pred, "digraph.tsort failed")
    ).

:- pred output_scalar_defns(java_out_info::in, indent::in,
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

    MLDS_ModuleName = Info ^ joi_module_name,
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
    map.det_insert(Scalar, Initializer, !Map),
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
        unexpected($module, $pred, "lval or mem_addr")
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
        ; RvalConst = mlconst_uint(_)
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

:- pred output_scalar_init_method(java_out_info::in, indent::in,
    map(mlds_scalar_common, mlds_initializer)::in,
    list(mlds_scalar_common)::in, int::in, int::out, io::di, io::uo) is det.

output_scalar_init_method(Info, Indent, Map, Scalars,
        ChunkNum, ChunkNum + 1, !IO) :-
    indent_line(Indent, !IO),
    io.format("private static void MR_init_scalars_%d() {\n",
        [i(ChunkNum)], !IO),
    list.foldl(output_scalar_init(Info, Indent + 1, Map), Scalars, !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_scalar_init(java_out_info::in, indent::in,
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

:- pred output_call_scalar_init_method(int::in, int::in, io::di, io::uo)
    is det.

output_call_scalar_init_method(Indent, ChunkNum, !IO) :-
    indent_line(Indent, !IO),
    io.format("MR_init_scalars_%d();\n", [i(ChunkNum)], !IO).

:- pred output_vector_common_data(java_out_info::in, indent::in,
    ml_vector_cell_map::in, io::di, io::uo) is det.

output_vector_common_data(Info, Indent, VectorCellGroupMap, !IO) :-
    map.foldl(output_vector_cell_group(Info, Indent), VectorCellGroupMap, !IO).

:- pred output_vector_cell_group(java_out_info::in, indent::in,
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
    % with Java's rules for definite assignment. This mirrors the default
    % Java initializers for class and instance variables.
    %
:- func get_java_type_initializer(mlds_type) = string.

get_java_type_initializer(Type) = Initializer :-
    (
        Type = mercury_type(_, CtorCat, _),
        (
            ( CtorCat = ctor_cat_builtin(cat_builtin_int)
            ; CtorCat = ctor_cat_builtin(cat_builtin_uint)
            ; CtorCat = ctor_cat_builtin(cat_builtin_float)
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
            ; CtorCat = ctor_cat_enum(_)
            ; CtorCat = ctor_cat_builtin_dummy
            ; CtorCat = ctor_cat_variable
            ; CtorCat = ctor_cat_void
            ; CtorCat = ctor_cat_user(_)
            ),
            Initializer = "null"
        )
    ;
        ( Type = mlds_native_int_type
        ; Type = mlds_native_uint_type
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
        Type = mlds_foreign_type(ForeignLangType),
        ( if
            java_primitive_foreign_language_type(ForeignLangType, _, _,
                _, Initializer0)
        then
            Initializer = Initializer0
        else
            Initializer = "null"
        )
    ;
        ( Type = mlds_mercury_array_type(_)
        ; Type = mlds_cont_type(_)
        ; Type = mlds_commit_type
        ; Type = mlds_class_type(_, _, _)
        ; Type = mlds_array_type(_)
        ; Type = mlds_mostly_generic_array_type(_)
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
        Type = mlds_unknown_type,
        unexpected($module, $pred, "variable has unknown_type")
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

:- pred output_initializer(java_out_info::in, output_aux::in, mlds_type::in,
    mlds_initializer::in, io::di, io::uo) is det.

output_initializer(Info, OutputAux, Type, Initializer, !IO) :-
    NeedsInit = needs_initialization(Initializer),
    (
        NeedsInit = yes,
        io.write_string(" = ", !IO),
        % Due to cyclic references, we need to separate the allocation and
        % initialisation steps of RTTI structures. If InitStyle is alloc_only
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
            io.write_string(get_java_type_initializer(Type), !IO)
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

:- pred output_initializer_alloc_only(java_out_info::in, mlds_initializer::in,
    maybe(mlds_type)::in, io::di, io::uo) is det.

output_initializer_alloc_only(Info, Initializer, MaybeType, !IO) :-
    (
        Initializer = no_initializer,
        unexpected($module, $pred, "no_initializer")
    ;
        Initializer = init_obj(_),
        unexpected($module, $pred, "init_obj")
    ;
        Initializer = init_struct(StructType, FieldInits),
        io.write_string("new ", !IO),
        ( if
            StructType = mercury_type(_Type, CtorCat, _),
            type_category_is_array(CtorCat) = is_array
        then
            Size = list.length(FieldInits),
            io.format("java.lang.Object[%d]", [i(Size)], !IO)
        else
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
            ( if list.split_last(ArrayDims, Heads, 0) then
                output_array_dimensions(Heads ++ [Size], !IO)
            else
                unexpected($module, $pred, "missing array dimension")
            )
        ;
            MaybeType = no,
            % XXX we need to know the type here
            io.format("/* XXX init_array */ Object[%d]", [i(Size)], !IO)
        )
    ).

:- pred output_initializer_body(java_out_info::in, mlds_initializer::in,
    maybe(mlds_type)::in, io::di, io::uo) is det.

output_initializer_body(Info, Initializer, MaybeType, !IO) :-
    (
        Initializer = no_initializer,
        unexpected($module, $pred, "no_initializer")
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
            io.write_string("/* XXX init_array */ Object[]", !IO)
        ),
        io.write_string(" {\n\t\t", !IO),
        output_initializer_body_list(Info, ElementInits, !IO),
        io.write_string("}", !IO)
    ).

:- pred output_initializer_body_list(java_out_info::in,
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

:- pred output_rtti_assignments(java_out_info::in, indent::in,
    list(mlds_defn)::in, io::di, io::uo) is det.

output_rtti_assignments(Info, Indent, Defns, !IO) :-
    (
        Defns = []
    ;
        Defns = [_ | _],
        OrderedDefns = order_mlds_rtti_defns(Defns),
        indent_line(Indent, !IO),
        io.write_string("static {\n", !IO),
        list.foldl(output_rtti_defns_assignments(Info, Indent + 1),
            OrderedDefns, !IO),
        indent_line(Indent, !IO),
        io.write_string("}\n", !IO)
    ).

:- pred output_rtti_defns_assignments(java_out_info::in, indent::in,
    list(mlds_defn)::in, io::di, io::uo) is det.

output_rtti_defns_assignments(Info, Indent, Defns, !IO) :-
    % Separate cliques.
    indent_line(Indent, !IO),
    io.write_string("//\n", !IO),
    list.foldl(output_rtti_defn_assignments(Info, Indent),
        Defns, !IO).

:- pred output_rtti_defn_assignments(java_out_info::in, indent::in,
    mlds_defn::in, io::di, io::uo) is det.

output_rtti_defn_assignments(Info, Indent, Defn, !IO) :-
    Defn = mlds_defn(Name, _Context, _Flags, DefnBody),
    (
        DefnBody = mlds_data(_Type, Initializer, _),
        output_rtti_defn_assignments_2(Info, Indent, Name, Initializer, !IO)
    ;
        ( DefnBody = mlds_function(_, _, _, _, _, _)
        ; DefnBody = mlds_class(_)
        ),
        unexpected($module, $pred, "expected mlds_data")
    ).

:- pred output_rtti_defn_assignments_2(java_out_info::in, indent::in,
    mlds_entity_name::in, mlds_initializer::in, io::di, io::uo) is det.

output_rtti_defn_assignments_2(Info, Indent, Name, Initializer, !IO) :-
    (
        Initializer = no_initializer
    ;
        Initializer = init_obj(_),
        % Not encountered in practice.
        unexpected($module, $pred, "init_obj")
    ;
        Initializer = init_struct(StructType, FieldInits),
        IsArray = type_is_array(StructType),
        (
            IsArray = not_array,
            indent_line(Indent, !IO),
            output_entity_name_for_java(Name, !IO),
            io.write_string(".init(", !IO),
            output_initializer_body_list(Info, FieldInits, !IO),
            io.write_string(");\n", !IO)
        ;
            IsArray = is_array,
            % Not encountered in practice.
            unexpected($module, $pred, "is_array")
        )
    ;
        Initializer = init_array(ElementInits),
        list.foldl2(output_rtti_array_assignments(Info, Indent, Name),
            ElementInits, 0, _Index, !IO)
    ).

:- pred output_rtti_array_assignments(java_out_info::in, indent::in,
    mlds_entity_name::in, mlds_initializer::in, int::in, int::out,
    io::di, io::uo) is det.

output_rtti_array_assignments(Info, Indent, Name, ElementInit,
        Index, Index + 1, !IO) :-
    indent_line(Indent, !IO),
    output_entity_name_for_java(Name, !IO),
    io.write_string("[", !IO),
    io.write_int(Index, !IO),
    io.write_string("] = ", !IO),
    output_initializer_body(Info, ElementInit, no, !IO),
    io.write_string(";\n", !IO).

%-----------------------------------------------------------------------------%
%
% Code to output function declarations/definitions.
%

:- pred output_pred_proc_id(java_out_info::in, pred_proc_id::in,
    io::di, io::uo) is det.

output_pred_proc_id(Info, proc(PredId, ProcId), !IO) :-
    AutoComments = Info ^ joi_auto_comments,
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

:- pred output_func(java_out_info::in, indent::in, mlds_entity_name::in,
    output_aux::in, mlds_context::in,
    mlds_func_params::in, mlds_function_body::in, io::di, io::uo) is det.

output_func(Info, Indent, Name, OutputAux, Context, Signature, MaybeBody,
        !IO) :-
    (
        MaybeBody = body_defined_here(Body),
        output_func_decl(Info, Indent, Name, OutputAux, Signature, !IO),
        io.write_string("\n", !IO),
        indent_line(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("{\n", !IO),
        FuncInfo = func_info(Signature),
        output_statement(Info, Indent + 1, FuncInfo, Body, _ExitMethods, !IO),
        indent_line(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("}\n", !IO)    % end the function
    ;
        MaybeBody = body_external
    ).

:- pred output_func_decl(java_out_info::in, indent::in, mlds_entity_name::in,
    output_aux::in, mlds_func_params::in, io::di, io::uo) is det.

output_func_decl(Info, Indent, Name, OutputAux, Signature, !IO) :-
    Signature = mlds_func_params(Parameters, RetTypes),
    ( if
        OutputAux = cname(CtorName),
        Name = entity_export("<constructor>")
    then
        output_entity_name_for_java(CtorName, !IO)
    else
        output_return_types(Info, RetTypes, !IO),
        io.write_char(' ', !IO),
        output_entity_name_for_java(Name, !IO)
    ),
    output_params(Info, Indent, Parameters, !IO).

:- pred output_return_types(java_out_info::in, mlds_return_types::in,
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
        io.write_string("java.lang.Object []", !IO)
    ).

:- pred output_params(java_out_info::in, indent::in, mlds_arguments::in,
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

:- pred output_param(java_out_info::in, indent::in, mlds_argument::in,
    io::di, io::uo) is det.

output_param(Info, Indent, Arg, !IO) :-
    Arg = mlds_argument(VarName, Type, _GCStatement),
    indent_line(Indent, !IO),
    output_type(Info, Type, !IO),
    io.write_char(' ', !IO),
    output_mlds_var_name_for_java(VarName, !IO).

%-----------------------------------------------------------------------------%
%
% Code to output names of various entities.
%
% XXX Much of the code in this section will not work when we start enforcing
% names properly.
%

:- pred output_maybe_qualified_name(java_out_info::in,
    mlds_qualified_entity_name::in, io::di, io::uo) is det.

output_maybe_qualified_name(Info, QualifiedName, !IO) :-
    % Don't module qualify names which are defined in the current module.
    % This avoids unnecessary verbosity, and is also necessary in the case
    % of local variables and function parameters, which must not be qualified.
    QualifiedName = qual(ModuleName, _QualKind, Name),
    CurrentModuleName = Info ^ joi_module_name,
    ( if ModuleName = CurrentModuleName then
        output_entity_name_for_java(Name, !IO)
    else
        output_fully_qualified_thing(QualifiedName,
            output_entity_name_for_java, !IO)
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

    % The part of the qualifier that corresponds to a top-level Java class.
    mangle_sym_name_for_java(OuterName, module_qual, "__", MangledOuterName),

    % The later parts of the qualifier correspond to nested Java classes.
    ( if OuterName = InnerName then
        MangledSuffix = ""
    else
        remove_sym_name_prefixes(InnerName, OuterName, Suffix),
        mangle_sym_name_for_java(Suffix, convert_qual_kind(QualKind), ".",
            MangledSuffix0),
        MangledSuffix = "." ++ MangledSuffix0
    ),

    String = MangledOuterName ++ MangledSuffix.

:- pred remove_sym_name_prefixes(sym_name::in, sym_name::in, sym_name::out)
    is det.

remove_sym_name_prefixes(SymName0, Prefix, SymName) :-
    (
        SymName0 = qualified(Qual, Name),
        ( if Qual = Prefix then
            SymName = unqualified(Name)
        else
            remove_sym_name_prefixes(Qual, Prefix, SymName1),
            SymName = qualified(SymName1, Name)
        )
    ;
        SymName0 = unqualified(_),
        unexpected($module, $pred, "prefix not found")
    ).

:- func convert_qual_kind(mlds_qual_kind) = csj_qual_kind.

convert_qual_kind(module_qual) = module_qual.
convert_qual_kind(type_qual) = type_qual.

:- pred output_module_name(mercury_module_name::in, io::di, io::uo) is det.

output_module_name(ModuleName, !IO) :-
    io.write_string(sym_name_mangle(ModuleName), !IO).

:- pred output_unqual_class_name_for_java(mlds_class_name::in, arity::in,
    io::di, io::uo) is det.

output_unqual_class_name_for_java(Name, Arity, !IO) :-
    unqual_class_name_to_string_for_java(Name, Arity, String),
    io.write_string(String, !IO).

:- pred unqual_class_name_to_string_for_java(mlds_class_name::in, arity::in,
    string::out) is det.

unqual_class_name_to_string_for_java(Name, Arity, String) :-
    MangledName = name_mangle_no_leading_digit(Name),
    % By convention, class names should start with a capital letter.
    UppercaseMangledName = flip_initial_case(MangledName),
    String = UppercaseMangledName ++ "_" ++ string.from_int(Arity).

:- pred qual_class_name_to_string_for_java(mlds_class::in, arity::in,
    string::out) is det.

qual_class_name_to_string_for_java(QualName, Arity, String) :-
    QualName = qual(MLDS_ModuleName, QualKind, ClassName),
    ( if
        SymName = mlds_module_name_to_sym_name(MLDS_ModuleName),
        SymName = java_mercury_runtime_package_name
    then
        % Don't mangle runtime class names.
        String = "jmercury.runtime." ++ ClassName
    else
        qualifier_to_string(MLDS_ModuleName, QualKind, QualString),
        unqual_class_name_to_string_for_java(ClassName, Arity, UnqualString),
        String = QualString ++ "." ++ UnqualString
    ).

:- pred output_entity_name_for_java(mlds_entity_name::in, io::di, io::uo)
    is det.

output_entity_name_for_java(EntityName, !IO) :-
    (
        EntityName = entity_type(Name, Arity),
        output_unqual_class_name_for_java(Name, Arity, !IO)
    ;
        EntityName = entity_data(DataName),
        output_data_name_for_java(DataName, !IO)
    ;
        EntityName = entity_function(PredLabel, ProcId, MaybeSeqNum, _PredId),
        output_pred_label(PredLabel, !IO),
        proc_id_to_int(ProcId, ModeNum),
        io.format("_%d", [i(ModeNum)], !IO),
        (
            MaybeSeqNum = yes(SeqNum),
            io.format("_%d", [i(SeqNum)], !IO)
        ;
            MaybeSeqNum = no
        )
    ;
        EntityName = entity_export(Name),
        io.write_string(Name, !IO)
    ).

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

:- pred output_data_name_for_java(mlds_data_name::in, io::di, io::uo) is det.

output_data_name_for_java(DataName, !IO) :-
    (
        DataName = mlds_data_var(VarName),
        output_mlds_var_name_for_java(VarName, !IO)
    ;
        DataName = mlds_scalar_common_ref(Common),
        Common = ml_scalar_common(_ModuleName, _Type,
            ml_scalar_common_type_num(TypeNum), RowNum),
        io.format("MR_scalar_common_%d[%d]", [i(TypeNum), i(RowNum)], !IO)
    ;
        DataName = mlds_rtti(RttiId),
        rtti.id_to_c_identifier(RttiId, RttiAddrName),
        io.write_string(RttiAddrName, !IO)
    ;
        DataName = mlds_module_layout,
        unexpected($module, $pred, "NYI: mlds_module_layout")
    ;
        DataName = mlds_proc_layout(_ProcLabel),
        unexpected($module, $pred, "NYI: mlds_proc_layout")
    ;
        DataName = mlds_internal_layout(_ProcLabel, _FuncSeqNum),
        unexpected($module, $pred, "NYI: mlds_internal_layout")
    ;
        DataName = mlds_tabling_ref(ProcLabel, Id),
        Prefix = tabling_info_id_str(Id) ++ "_",
        io.write_string(Prefix, !IO),
        mlds_output_proc_label(mlds_std_tabling_proc_label(ProcLabel), !IO)
    ).


:- pred output_mlds_var_name_for_java(mlds_var_name::in, io::di, io::uo)
    is det.

output_mlds_var_name_for_java(mlds_var_name(Name, no), !IO) :-
    output_valid_mangled_name(Name, !IO).
output_mlds_var_name_for_java(mlds_var_name(Name, yes(Num)), !IO) :-
    output_mangled_name(string.format("%s_%d", [s(Name), i(Num)]), !IO).

%-----------------------------------------------------------------------------%
%
% Code to output types.
%

:- pred output_type(java_out_info::in, mlds_type::in, io::di, io::uo) is det.

output_type(Info, MLDS_Type, !IO) :-
    output_type(Info, MLDS_Type, [], !IO).

:- pred output_type(java_out_info::in, mlds_type::in, list(int)::in,
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
    % Generate the Java name for a type. ArrayDims are the array dimensions to
    % be written after the type name, if any, in reverse order to that of Java
    % syntax where a non-zero integer represents a known array size and zero
    % represents an unknown array size.
    %
    % e.g. ArrayDims = [0, 3] represents the Java array `Object[3][]',
    % which should be read as `(Object[])[3]'.
    %
:- pred type_to_string(java_out_info::in, mlds_type::in,
    string::out, list(int)::out) is det.

type_to_string(Info, MLDS_Type, String, ArrayDims) :-
    (
        MLDS_Type = mercury_type(Type, CtorCat, _),
        ( if
            % We need to handle type_info (etc.) types specially --
            % they get mapped to types in the runtime rather than
            % in private_builtin.
            hand_defined_type(Type, CtorCat, SubstituteName, ArrayDims0)
        then
            String = SubstituteName,
            ArrayDims = ArrayDims0
        else if
            % io.state and store.store
            CtorCat = ctor_cat_builtin_dummy
        then
            String = "/* builtin_dummy */ java.lang.Object",
            ArrayDims = []
        else if
            Type = c_pointer_type
        then
            % The c_pointer type is used in the c back-end as a generic way
            % to pass foreign types to automatically generated Compare and
            % Unify code. When compiling to Java we must instead use
            % java.lang.Object.
            String = "/* c_pointer */ java.lang.Object",
            ArrayDims = []
        else
            mercury_type_to_string(Info, Type, CtorCat, String, ArrayDims)
        )
    ;
        MLDS_Type = mlds_mercury_array_type(ElementType),
        ( if ElementType = mercury_type(_, ctor_cat_variable, _) then
            % We can't use `java.lang.Object []', since we want a generic type
            % that is capable of holding any kind of array, including e.g.
            % `int []'. Java doesn't have any equivalent of .NET's System.Array
            % class, so we just use the universal base `java.lang.Object'.
            String = "/* Array */ java.lang.Object",
            ArrayDims = []
        else
            % For primitive element types we use arrays of the primitive type.
            % For non-primitive element types, we just use
            % `java.lang.Object []'. We used to use more specific types,
            % but then to create an array of the right type we need to use
            % reflection to determine the class of a representative element.
            % That doesn't work if the representative element is of a foreign
            % type, and has the value null.
            ( if java_builtin_type(ElementType, _, _, _) then
                type_to_string(Info, ElementType, String, ArrayDims0),
                ArrayDims = [0 | ArrayDims0]
            else
                String = "java.lang.Object",
                ArrayDims = [0]
            )
        )
    ;
        MLDS_Type = mlds_native_int_type,
        String = "int",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_uint_type,
        String = "int", % Java lacks unsigned integers.
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_float_type,
        String = "double",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_bool_type,
        String = "boolean",
        ArrayDims = []
    ;
        MLDS_Type = mlds_native_char_type,
        % Java `char' not large enough for code points so we must use `int'.
        String = "int",
        ArrayDims = []
    ;
        MLDS_Type = mlds_foreign_type(ForeignType),
        (
            ForeignType = java(java_type(Name)),
            String = Name,
            ArrayDims = []
        ;
            ForeignType = c(_),
            unexpected($module, $pred, "c foreign_type")
        ;
            ForeignType = csharp(_),
            unexpected($module, $pred, "csharp foreign_type")
        ;
            ForeignType = erlang(_),
            unexpected($module, $pred, "erlang foreign_type")
        )
    ;
        MLDS_Type = mlds_class_type(Name, Arity, _ClassKind),
        qual_class_name_to_string_for_java(Name, Arity, String),
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
        MLDS_Type = mlds_mostly_generic_array_type(_Type),
        Type = mlds_generic_type,
        type_to_string(Info, Type, String, ArrayDims0),
        ArrayDims = [0 | ArrayDims0]
    ;
        MLDS_Type = mlds_func_type(_FuncParams),
        String = "jmercury.runtime.MethodPtr",
        ArrayDims = []
    ;
        MLDS_Type = mlds_generic_type,
        String = "/* generic_type */ java.lang.Object",
        ArrayDims = []
    ;
        MLDS_Type = mlds_generic_env_ptr_type,
        String = "/* env_ptr */ java.lang.Object",
        ArrayDims = []
    ;
        MLDS_Type = mlds_type_info_type,
        String = "jmercury.runtime.TypeInfo",
        ArrayDims = []
    ;
        MLDS_Type = mlds_pseudo_type_info_type,
        String = "jmercury.runtime.PseudoTypeInfo",
        ArrayDims = []
    ;
        MLDS_Type = mlds_cont_type(_),
        % XXX Should this actually be a class that extends MethodPtr?
        String = "jmercury.runtime.MethodPtr",
        ArrayDims = []
    ;
        MLDS_Type = mlds_commit_type,
        String = "jmercury.runtime.Commit",
        ArrayDims = []
    ;
        MLDS_Type = mlds_rtti_type(RttiIdMaybeElement),
        rtti_id_maybe_element_java_type(RttiIdMaybeElement, String, IsArray),
        (
            IsArray = is_array,
            ArrayDims = [0]
        ;
            IsArray = not_array,
            ArrayDims = []
        )
    ;
        MLDS_Type = mlds_tabling_type(TablingId),
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
        unexpected($module, $pred, "unknown type")
    ).

:- pred mercury_type_to_string(java_out_info::in, mer_type::in,
    type_ctor_category::in, string::out, list(int)::out) is det.

mercury_type_to_string(Info, Type, CtorCat, String, ArrayDims) :-
    (
        CtorCat = ctor_cat_builtin(cat_builtin_char),
        % Java `char' not large enough for code points so we must use `int'.
        String = "int",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int),
        String = "int",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_uint),
        String = "int",     % Java has no unsigned types.
        ArrayDims = []
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        String = "java.lang.String",
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
        ( if
            Info ^ joi_output_generics = do_output_generics,
            Type = type_variable(TVar, kind_star),
            list.member(TVar, Info ^ joi_univ_tvars)
        then
            generic_tvar_to_string(TVar, String)
        else
            String = "java.lang.Object"
        ),
        ArrayDims = []
    ;
        CtorCat = ctor_cat_tuple,
        String = "/* tuple */ java.lang.Object",
        ArrayDims = [0]
    ;
        CtorCat = ctor_cat_higher_order,
        String = "/* closure */ java.lang.Object",
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

:- pred mercury_user_type_to_string(java_out_info::in, mer_type::in,
    type_ctor_category::in, string::out, list(int)::out) is det.

mercury_user_type_to_string(Info, Type, CtorCat, String, ArrayDims) :-
    type_to_ctor_and_args_det(Type, TypeCtor, ArgsTypes),
    ml_gen_type_name(TypeCtor, ClassName, ClassArity),
    ( if CtorCat = ctor_cat_enum(_) then
        MLDS_Type = mlds_class_type(ClassName, ClassArity, mlds_enum)
    else
        MLDS_Type = mlds_class_type(ClassName, ClassArity, mlds_class)
    ),
    type_to_string(Info, MLDS_Type, TypeString, ArrayDims),
    OutputGenerics = Info ^ joi_output_generics,
    (
        OutputGenerics = do_output_generics,
        generic_args_types_to_string(Info, ArgsTypes, GenericsString),
        String = TypeString ++ GenericsString
    ;
        OutputGenerics = do_not_output_generics,
        String = TypeString
    ).

:- pred generic_args_types_to_string(java_out_info::in, list(mer_type)::in,
    string::out) is det.

generic_args_types_to_string(Info, ArgsTypes, String) :-
    (
        ArgsTypes = [],
        String = ""
    ;
        ArgsTypes = [_ | _],
        ToString = (pred(ArgType::in, ArgTypeString::out) is det :-
            ModuleInfo = Info ^ joi_module_info,
            MLDS_ArgType = mercury_type_to_mlds_type(ModuleInfo, ArgType),
            boxed_type_to_string(Info, MLDS_ArgType, ArgTypeString)
        ),
        list.map(ToString, ArgsTypes, ArgsTypesStrings),
        ArgsTypesString = string.join_list(", ", ArgsTypesStrings),
        String = "<" ++ ArgsTypesString ++ ">"
    ).

    % Return is_array if the corresponding Java type is an array type.
    %
:- func type_is_array(mlds_type) = is_array.

type_is_array(Type) = IsArray :-
    ( if Type = mlds_array_type(_) then
        IsArray = is_array
    else if Type = mlds_mercury_array_type(_) then
        IsArray = is_array
    else if Type = mercury_type(_, CtorCat, _) then
        IsArray = type_category_is_array(CtorCat)
    else if Type = mlds_rtti_type(RttiIdMaybeElement) then
        rtti_id_maybe_element_java_type(RttiIdMaybeElement,
            _JavaTypeName, IsArray)
    else
        IsArray = not_array
    ).

    % Return is_array if the corresponding Java type is an array type.
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
        SubstituteName = "jmercury.runtime.TypeInfo_Struct",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_system(cat_system_type_ctor_info),
        SubstituteName = "jmercury.runtime.TypeCtorInfo_Struct",
        ArrayDims = []
    ;
        CtorCat = ctor_cat_system(cat_system_typeclass_info),
        SubstituteName = "/* typeclass_info */ java.lang.Object",
        ArrayDims = [0]
    ;
        CtorCat = ctor_cat_system(cat_system_base_typeclass_info),
        SubstituteName = "/* base_typeclass_info */ java.lang.Object",
        ArrayDims = [0]
    ;
        CtorCat = ctor_cat_user(cat_user_general),
        ( if Type = type_desc_type then
            SubstituteName = "jmercury.runtime.TypeInfo_Struct"
        else if Type = pseudo_type_desc_type then
            SubstituteName = "jmercury.runtime.PseudoTypeInfo"
        else if Type = type_ctor_desc_type then
            SubstituteName = "jmercury.runtime.TypeCtorInfo_Struct"
        else
            fail
        ),
        ArrayDims = []
    ).

:- pred boxed_type_to_string(java_out_info::in, mlds_type::in, string::out)
    is det.

boxed_type_to_string(Info, Type, String) :-
    ( if java_builtin_type(Type, _, JavaBoxedName, _) then
        String = JavaBoxedName
    else
        type_to_string(Info, Type, String0, ArrayDims),
        list.map(array_dimension_to_string, ArrayDims, RevBrackets),
        list.reverse(RevBrackets, Brackets),
        string.append_list([String0 | Brackets], String)
    ).

:- pred array_dimension_to_string(int::in, string::out) is det.

array_dimension_to_string(N, String) :-
    ( if N = 0 then
        String = "[]"
    else
        String = string.format("[%d]", [i(N)])
    ).

%-----------------------------------------------------------------------------%
%
% Code to output declaration specifiers.
%

:- pred output_decl_flags(java_out_info::in, mlds_decl_flags::in,
    io::di, io::uo) is det.

output_decl_flags(Info, Flags, !IO) :-
    output_access(Info, access(Flags), !IO),
    output_per_instance(per_instance(Flags), !IO),
    output_virtuality(Info, virtuality(Flags), !IO),
    output_overridability_constness(overridability(Flags), constness(Flags),
        !IO),
    output_abstractness(abstractness(Flags), !IO).

:- pred output_access(java_out_info::in, access::in, io::di, io::uo) is det.

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

:- pred output_virtuality(java_out_info::in, virtuality::in,
    io::di, io::uo) is det.

output_virtuality(Info, Virtual, !IO) :-
    (
        Virtual = virtual,
        maybe_output_comment(Info, "virtual", !IO)
    ;
        Virtual = non_virtual
    ).

:- pred output_overridability_constness(overridability::in, constness::in,
    io::di, io::uo) is det.

output_overridability_constness(Overridability, Constness, !IO) :-
    ( if
        ( Overridability = sealed
        ; Constness = const
        )
    then
        io.write_string("final ", !IO)
    else
        true
    ).

:- pred output_abstractness(abstractness::in, io::di, io::uo) is det.

output_abstractness(Abstractness, !IO) :-
    (
        Abstractness = abstract,
        io.write_string("abstract ", !IO)
    ;
        Abstractness = concrete
    ).

:- pred maybe_output_comment(java_out_info::in, string::in,
    io::di, io::uo) is det.

maybe_output_comment(Info, Comment, !IO) :-
    AutoComments = Info ^ joi_auto_comments,
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

    ;       can_fall_through.
            % Where the instruction can complete normally and execution
            % can continue with the following statement.

:- type code_addr_wrapper
    --->    code_addr_wrapper(
                caw_class           :: string,
                caw_ptr_num         :: maybe(int)
            ).

:- type func_info
    --->    func_info(
                func_info_params    :: mlds_func_params
            ).

:- func mod_name(mlds_fully_qualified_name(T)) = mlds_module_name.

mod_name(qual(ModuleName, _, _)) = ModuleName.

:- pred output_statements(java_out_info::in, indent::in, func_info::in,
    list(statement)::in, exit_methods::out, io::di, io::uo) is det.

output_statements(_, _, _, [], ExitMethods, !IO) :-
    ExitMethods = set.make_singleton_set(can_fall_through).
output_statements(Info, Indent, FuncInfo, [Statement | Statements],
        ExitMethods, !IO) :-
    output_statement(Info, Indent, FuncInfo, Statement,
        StmtExitMethods, !IO),
    ( if set.member(can_fall_through, StmtExitMethods) then
        output_statements(Info, Indent, FuncInfo, Statements,
            StmtsExitMethods, !IO),
        ExitMethods0 = StmtExitMethods `set.union` StmtsExitMethods,
        ( if set.member(can_fall_through, StmtsExitMethods) then
            ExitMethods = ExitMethods0
        else
            % If the last statement could not complete normally
            % the current block can no longer complete normally.
            ExitMethods = ExitMethods0 `set.delete` can_fall_through
        )
    else
        % Don't output any more statements from the current list since
        % the previous statement cannot complete.
        ExitMethods = StmtExitMethods
    ).

:- pred output_statement(java_out_info::in, indent::in,
    func_info::in, statement::in, exit_methods::out, io::di, io::uo) is det.

output_statement(Info, Indent, FuncInfo,
        statement(Statement, Context), ExitMethods, !IO) :-
    ProgContext = mlds_get_prog_context(Context),
    output_context(Info ^ joi_line_numbers, marker_comment, ProgContext, !IO),
    output_stmt(Info, Indent, FuncInfo, Statement, Context,
        ExitMethods, !IO).

:- pred output_stmt(java_out_info::in, indent::in, func_info::in,
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
        indent_line(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
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
        ( if Cond = ml_const(mlconst_false) then
            indent_line(Indent, !IO),
            io.write_string("{  /* Unreachable code */  }\n", !IO),
            ExitMethods = set.make_singleton_set(can_fall_through)
        else
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
        indent_line(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("while (", !IO),
        output_rval(Info, Cond, !IO),
        io.write_string(");\n", !IO),
        ExitMethods = while_exit_methods(Cond, StmtExitMethods)
    ;
        Statement = ml_stmt_if_then_else(Cond, Then0, MaybeElse),
        % We need to take care to avoid problems caused by the dangling else
        % ambiguity.
        ( if
            % For examples of the form
            %
            %   if (...)
            %       if (...)
            %           ...
            %   else
            %       ...
            %
            % we need braces around the inner `if', otherwise they wouldn't
            % parse they way we want them to: Java would match the `else'
            % with the inner `if' rather than the outer `if'.

            MaybeElse = yes(_),
            Then0 = statement(ml_stmt_if_then_else(_, _, no), ThenContext)
        then
            Then = statement(ml_stmt_block([], [Then0]), ThenContext)
        else
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
            indent_line(Info ^ joi_line_numbers, marker_comment,
                Context, Indent, !IO),
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
        indent_line(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("switch (", !IO),
        output_rval_maybe_with_enum(Info, Val, !IO),
        io.write_string(") {\n", !IO),
        output_switch_cases(Info, Indent + 1, FuncInfo, Context, Cases,
            Default, ExitMethods, !IO),
        indent_line(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("}\n", !IO)
    ;
        Statement = ml_stmt_label(_),
        unexpected($module, $pred, "labels not supported in Java.")
    ;
        Statement = ml_stmt_goto(goto_label(_)),
        unexpected($module, $pred, "gotos not supported in Java.")
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
        unexpected($module, $pred, "computed gotos not supported in Java.")
    ;
        Statement = ml_stmt_call(Signature, FuncRval, MaybeObject, CallArgs,
            Results, _IsTailCall, _Markers),
        Signature = mlds_func_signature(ArgTypes, RetTypes),
        indent_line(Indent, !IO),
        io.write_string("{\n", !IO),
        indent_line(Info ^ joi_line_numbers, marker_comment,
            Context, Indent + 1, !IO),
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
            %   { java.lang.Object [] result = <func>(<args>);
            %     <output1> = (<type1>) result[0];
            %     <output2> = (<type2>) result[1];
            %     ...
            %   }
            %
            io.write_string("java.lang.Object [] result = ", !IO)
        ),
        ( if FuncRval = ml_const(mlconst_code_addr(_)) then
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
        else
            % This is a call using a method pointer.
            %
            % Here we do downcasting, as a call will always return
            % something of type java.lang.Object
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
                io.write_string("((java.lang.Object[]) ", !IO)
            ),
            (
                MaybeObject = yes(Object),
                output_bracketed_rval(Info, Object, !IO),
                io.write_string(".", !IO)
            ;
                MaybeObject = no
            ),

            list.length(CallArgs, Arity),
            ( if Arity =< max_specialised_method_ptr_arity then
                io.write_string("((jmercury.runtime.MethodPtr", !IO),
                io.write_int(Arity, !IO),
                io.write_string(") ", !IO),
                output_bracketed_rval(Info, FuncRval, !IO),
                io.write_string(").call___0_0(", !IO),
                output_boxed_args(Info, CallArgs, ArgTypes, !IO)
            else
                io.write_string("((jmercury.runtime.MethodPtrN) ", !IO),
                output_bracketed_rval(Info, FuncRval, !IO),
                io.write_string(").call___0_0(", !IO),
                output_args_as_array(Info, CallArgs, ArgTypes, !IO)
            ),

            % Closes brackets, and calls unbox methods for downcasting.
            % XXX This is a hack, see the above comment.
            io.write_string(")", !IO),
            (
                RetTypes = []
            ;
                RetTypes = [RetType2],
                ( if java_builtin_type(RetType2, _, _, UnboxMethod) then
                    io.write_string(").", !IO),
                    io.write_string(UnboxMethod, !IO),
                    io.write_string("()", !IO)
                else
                    io.write_string(")", !IO)
                )
            ;
                RetTypes = [_, _ | _],
                io.write_string(")", !IO)
            )
        ),
        io.write_string(";\n", !IO),

        ( if Results = [_, _ | _] then
            % Copy the results from the "result" array into the Result
            % lvals (unboxing them as we go).
            output_assign_results(Info, Results, RetTypes, 0, Indent + 1,
                Context, !IO)
        else
            true
        ),
        % XXX Is this needed? If present, it causes compiler errors for a
        % couple of files in the benchmarks directory. -mjwybrow
        %
        % ( if IsTailCall = tail_call, Results = [] then
        %   indent_line(Context, Indent + 1, !IO),
        %   io.write_string("return;\n", !IO)
        % else
        %   true
        % ),
        %
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
            io.write_string("return new java.lang.Object[] {\n", !IO),
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
        io.write_string(" = new jmercury.runtime.Commit();\n", !IO),
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
        io.write_string("catch (jmercury.runtime.Commit commit_variable)\n",
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
    ( if
        % XXX This is not a sufficient way of testing for a Java
        % "constant expression", though determining these accurately
        % is a little difficult to do here.
        Cond = ml_const(mlconst_true),
        not set.member(can_break, BlockExitMethods)
    then
        % Cannot complete normally
        ExitMethods0 = BlockExitMethods `set.delete` can_fall_through
    else
        ExitMethods0 = BlockExitMethods `set.insert` can_fall_through
    ),
    ExitMethods = (ExitMethods0 `set.delete` can_continue)
        `set.delete` can_break.

%-----------------------------------------------------------------------------%
%
% Extra code for handling function calls/returns.
%

:- pred output_args_as_array(java_out_info::in, list(mlds_rval)::in,
    list(mlds_type)::in, io::di, io::uo) is det.

output_args_as_array(Info, CallArgs, CallArgTypes, !IO) :-
    io.write_string("new java.lang.Object[] { ", !IO),
    output_boxed_args(Info, CallArgs, CallArgTypes, !IO),
    io.write_string("} ", !IO).

:- pred output_boxed_args(java_out_info::in, list(mlds_rval)::in,
    list(mlds_type)::in, io::di, io::uo) is det.

output_boxed_args(_, [], [], !IO).
output_boxed_args(_, [_ | _], [], !IO) :-
    unexpected($module, $pred, "length mismatch").
output_boxed_args(_, [], [_ | _], !IO) :-
    unexpected($module, $pred, "length mismatch").
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
%   { java.lang.Object [] result = <func>(<args>);
%     <output1> = (<type1>) result[0];
%     <output2> = (<type2>) result[1];
%     ...
%   }
%

    % This procedure generates the assignments to the outputs.
    %
:- pred output_assign_results(java_out_info::in, list(mlds_lval)::in,
    list(mlds_type)::in, int::in, indent::in, mlds_context::in,
    io::di, io::uo) is det.

output_assign_results(_, [], [], _, _, _, !IO).
output_assign_results(Info, [Lval | Lvals], [Type | Types], ResultIndex,
        Indent, Context, !IO) :-
    indent_line(Info ^ joi_line_numbers, marker_comment, Context, Indent, !IO),
    output_lval(Info, Lval, !IO),
    io.write_string(" = ", !IO),
    output_unboxed_result(Info, Type, ResultIndex, !IO),
    io.write_string(";\n", !IO),
    output_assign_results(Info, Lvals, Types, ResultIndex + 1,
        Indent, Context, !IO).
output_assign_results(_, [_ | _], [], _, _, _, _, _) :-
    unexpected($module, $pred, "list length mismatch").
output_assign_results(_, [], [_ | _], _, _, _, _, _) :-
    unexpected($module, $pred, "list length mismatch").

:- pred output_unboxed_result(java_out_info::in, mlds_type::in, int::in,
    io::di, io::uo) is det.

output_unboxed_result(Info, Type, ResultIndex, !IO) :-
    ( if java_builtin_type(Type, _, JavaBoxedName, UnboxMethod) then
        io.write_string("((", !IO),
        io.write_string(JavaBoxedName, !IO),
        io.write_string(") ", !IO),
        io.format("result[%d]).%s()", [i(ResultIndex), s(UnboxMethod)], !IO)
    else
        io.write_string("(", !IO),
        output_type(Info, Type, !IO),
        io.write_string(") ", !IO),
        io.format("result[%d]", [i(ResultIndex)], !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Extra code for outputting switch statements.
%

:- pred output_switch_cases(java_out_info::in, indent::in, func_info::in,
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
    ( if set.member(can_break, CaseExitMethods0) then
        CaseExitMethods = (CaseExitMethods0 `set.delete` can_break)
            `set.insert` can_fall_through
    else
        CaseExitMethods = CaseExitMethods0
    ),
    ExitMethods = CaseExitMethods `set.union` CasesExitMethods.

:- pred output_switch_case(java_out_info::in, indent::in, func_info::in,
    mlds_context::in, mlds_switch_case::in, exit_methods::out,
    io::di, io::uo) is det.

output_switch_case(Info, Indent, FuncInfo, Context, Case, ExitMethods, !IO) :-
    Case = mlds_switch_case(FirstCond, LaterConds, Statement),
    output_case_cond(Info, Indent, Context, FirstCond, !IO),
    list.foldl(output_case_cond(Info, Indent, Context), LaterConds, !IO),
    output_statement(Info, Indent + 1, FuncInfo, Statement,
        StmtExitMethods, !IO),
    ( if set.member(can_fall_through, StmtExitMethods) then
        indent_line(Info ^ joi_line_numbers, marker_comment,
            Context, Indent + 1, !IO),
        io.write_string("break;\n", !IO),
        ExitMethods = (StmtExitMethods `set.insert` can_break)
            `set.delete` can_fall_through
    else
        % Don't output `break' since it would be unreachable.
        ExitMethods = StmtExitMethods
    ).

:- pred output_case_cond(java_out_info::in, indent::in, mlds_context::in,
    mlds_case_match_cond::in, io::di, io::uo) is det.

output_case_cond(Info, Indent, Context, Match, !IO) :-
    (
        Match = match_value(Val),
        indent_line(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("case ", !IO),
        ( if Val = ml_const(mlconst_enum(N, _)) then
            io.write_int(N, !IO)
        else
            output_rval(Info, Val, !IO)
        ),
        io.write_string(":\n", !IO)
    ;
        Match = match_range(_, _),
        unexpected($module, $pred, "cannot match ranges in Java cases")
    ).

:- pred output_switch_default(java_out_info::in, indent::in, func_info::in,
    mlds_context::in, mlds_switch_default::in, exit_methods::out,
    io::di, io::uo) is det.

output_switch_default(Info, Indent, FuncInfo, Context, Default,
        ExitMethods, !IO) :-
    (
        Default = default_do_nothing,
        ExitMethods = set.make_singleton_set(can_fall_through)
    ;
        Default = default_case(Statement),
        indent_line(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("default:\n", !IO),
        output_statement(Info, Indent + 1, FuncInfo, Statement, ExitMethods,
            !IO)
    ;
        Default = default_is_unreachable,
        indent_line(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("default: /*NOTREACHED*/\n", !IO),
        indent_line(Info ^ joi_line_numbers, marker_comment,
            Context, Indent + 1, !IO),
        io.write_string("throw new jmercury.runtime.UnreachableDefault();\n",
            !IO),
        ExitMethods = set.make_singleton_set(can_throw)
    ).

%-----------------------------------------------------------------------------%
%
% Code for outputting atomic statements.
%

:- pred output_atomic_stmt(java_out_info::in, indent::in,
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
        sorry($module, $pred, "assign_if_in_heap")
    ;
        AtomicStmt = delete_object(_Lval),
        unexpected($module, $pred, "delete_object not supported in Java.")
    ;
        AtomicStmt = new_object(Target, _MaybeTag, ExplicitSecTag, Type,
            _MaybeSize, MaybeCtorName, Args, ArgTypes, _MayUseAtomic,
            _AllocId),
        (
            ExplicitSecTag = yes,
            unexpected($module, $pred, "explicit secondary tag")
        ;
            ExplicitSecTag = no
        ),

        indent_line(Indent, !IO),
        io.write_string("{\n", !IO),
        indent_line(Info ^ joi_line_numbers, marker_comment,
            Context, Indent + 1, !IO),
        output_lval(Info, Target, !IO),
        io.write_string(" = new ", !IO),
        % Generate class constructor name.
        ( if
            MaybeCtorName = yes(QualifiedCtorId),
            not (
                Type = mercury_type(MerType, CtorCat, _),
                hand_defined_type(MerType, CtorCat, _, _)
            )
        then
            output_type(Info, Type, !IO),
            io.write_char('.', !IO),
            QualifiedCtorId = qual(_ModuleName, _QualKind, CtorDefn),
            CtorDefn = ctor_id(CtorName, CtorArity),
            output_unqual_class_name_for_java(CtorName, CtorArity, !IO)
        else
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
        unexpected($module, $pred, "gc_check not implemented.")
    ;
        AtomicStmt = mark_hp(_Lval),
        unexpected($module, $pred, "mark_hp not implemented.")
    ;
        AtomicStmt = restore_hp(_Rval),
        unexpected($module, $pred, "restore_hp not implemented.")
    ;
        AtomicStmt = trail_op(_TrailOp),
        unexpected($module, $pred, "trail_ops not implemented.")
    ;
        AtomicStmt = inline_target_code(TargetLang, Components),
        (
            TargetLang = ml_target_java,
            indent_line(Indent, !IO),
            list.foldl(output_target_code_component(Info), Components, !IO)
        ;
            ( TargetLang = ml_target_c
            ; TargetLang = ml_target_gnu_c
            ; TargetLang = ml_target_csharp
            ),
            unexpected($module, $pred,
                "inline_target_code only works for lang_java")
        )
    ;
        AtomicStmt = outline_foreign_proc(_TargetLang, _Vs, _Lvals, _Code),
        unexpected($module, $pred,
            "foreign language interfacing not implemented")
    ).

%-----------------------------------------------------------------------------%

:- pred output_target_code_component(java_out_info::in,
    target_code_component::in, io::di, io::uo) is det.

output_target_code_component(Info, TargetCode, !IO) :-
    (
        TargetCode = user_target_code(CodeString, MaybeUserContext),
        (
            MaybeUserContext = yes(ProgContext),
            write_string_with_context_block(Info, 0, CodeString,
                ProgContext, !IO)
        ;
            MaybeUserContext = no,
            io.write_string(CodeString, !IO)
        )
    ;
        TargetCode = raw_target_code(CodeString),
        io.write_string(CodeString, !IO)
    ;
        TargetCode = target_code_input(Rval),
        output_rval(Info, Rval, !IO)
    ;
        TargetCode = target_code_output(Lval),
        output_lval(Info, Lval, !IO)
    ;
        TargetCode = target_code_type(Type),
        InfoGenerics = Info ^ joi_output_generics := do_output_generics,
        output_type(InfoGenerics, Type, !IO)
    ;
        TargetCode = target_code_name(Name),
        output_maybe_qualified_name(Info, Name, !IO)
    ;
        TargetCode = target_code_alloc_id(_),
        unexpected($module, $pred, "target_code_alloc_id not implemented")
    ).

%-----------------------------------------------------------------------------%

    % Output initial values of an object's fields as arguments for the
    % object's class constructor.
    %
:- pred output_init_args(java_out_info::in, list(mlds_rval)::in,
    list(mlds_type)::in, io::di, io::uo) is det.

output_init_args(_, [], [], !IO).
output_init_args(_, [_ | _], [], _, _) :-
    unexpected($module, $pred, "length mismatch.").
output_init_args(_, [], [_ | _], _, _) :-
    unexpected($module, $pred, "length mismatch.").
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

:- pred output_lval(java_out_info::in, mlds_lval::in, io::di, io::uo) is det.

output_lval(Info, Lval, !IO) :-
    (
        Lval = ml_field(_MaybeTag, PtrRval, FieldId, FieldType, _),
        (
            FieldId = ml_field_offset(OffsetRval),
            ( if
                ( FieldType = mlds_generic_type
                ; FieldType = mercury_type(type_variable(_, _), _, _)
                )
            then
                true
            else
                % The field type for field(_, _, offset(_), _, _) lvals
                % must be something that maps to MR_Box.
                unexpected($module, $pred, "unexpected field type")
            ),
            % XXX We shouldn't need this cast here, but there are cases where
            % it is needed and the MLDS doesn't seem to generate it.
            io.write_string("((java.lang.Object[]) ", !IO),
            output_rval(Info, PtrRval, !IO),
            io.write_string(")[", !IO),
            output_rval(Info, OffsetRval, !IO),
            io.write_string("]", !IO)
        ;
            FieldId = ml_field_named(FieldName, CtorType),
            ( if
                FieldName = qual(_, _, UnqualFieldName),
                UnqualFieldName = "data_tag"
            then
                % If the field we are trying to access is just a `data_tag'
                % then it is a member of the base class.
                output_bracketed_rval(Info, PtrRval, !IO),
                io.write_string(".", !IO)
            else if
                PtrRval = ml_self(_)
            then
                % Suppress type cast on `this' keyword. This makes a difference
                % when assigning to `final' member variables in constructor
                % functions.
                output_rval(Info, PtrRval, !IO),
                io.write_string(".", !IO)
            else
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
        QualName = qual(ModName, QualKind, entity_data(mlds_data_var(Name))),
        output_maybe_qualified_name(Info, QualName, !IO)
    ).

:- pred output_mangled_name(string::in, io::di, io::uo) is det.

output_mangled_name(Name, !IO) :-
    MangledName = name_mangle(Name),
    io.write_string(MangledName, !IO).

:- pred output_valid_mangled_name(string::in, io::di, io::uo) is det.

output_valid_mangled_name(Name, !IO) :-
    MangledName = name_mangle(Name),
    JavaSafeName = valid_java_symbol_name(MangledName),
    io.write_string(JavaSafeName, !IO).

:- pred output_call_rval(java_out_info::in, mlds_rval::in, io::di, io::uo)
    is det.

output_call_rval(Info, Rval, !IO) :-
    ( if
        Rval = ml_const(Const),
        Const = mlconst_code_addr(CodeAddr)
    then
        IsCall = yes,
        mlds_output_code_addr(Info, CodeAddr, IsCall, !IO)
    else
        output_bracketed_rval(Info, Rval, !IO)
    ).

:- pred output_bracketed_rval(java_out_info::in, mlds_rval::in, io::di, io::uo)
    is det.

output_bracketed_rval(Info, Rval, !IO) :-
    ( if
        % If it's just a variable name, then we don't need parentheses.
        ( Rval = ml_lval(ml_var(_,_))
        ; Rval = ml_const(mlconst_code_addr(_))
        )
    then
        output_rval(Info, Rval, !IO)
    else
        io.write_char('(', !IO),
        output_rval(Info, Rval, !IO),
        io.write_char(')', !IO)
    ).

:- pred output_rval(java_out_info::in, mlds_rval::in, io::di, io::uo) is det.

output_rval(Info, Rval, !IO) :-
    (
        Rval = ml_lval(Lval),
        output_lval(Info, Lval, !IO)
    ;
        Rval = ml_mkword(_, _),
        unexpected($module, $pred, "tags not supported in Java")
    ;
        Rval = ml_const(Const),
        output_rval_const(Info, Const, !IO)
    ;
        Rval = ml_unop(UnOp, RvalA),
        output_unop(Info, UnOp, RvalA, !IO)
    ;
        Rval = ml_binop(BinOp, RvalA, RvalB),
        output_binop(Info, BinOp, RvalA, RvalB, !IO)
    ;
        Rval = ml_mem_addr(_Lval),
        unexpected($module, $pred, "mem_addr(_) not supported")
    ;
        Rval = ml_scalar_common(_),
        % This reference is not the same as a mlds_data_addr const.
        unexpected($module, $pred, "ml_scalar_common")
    ;
        Rval = ml_vector_common_row(VectorCommon, RowRval),
        output_vector_common_row_rval(Info, VectorCommon, RowRval, !IO)
    ;
        Rval = ml_self(_),
        io.write_string("this", !IO)
    ).

:- pred output_unop(java_out_info::in, mlds_unary_op::in, mlds_rval::in,
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

:- pred output_cast_rval(java_out_info::in, mlds_type::in, mlds_rval::in,
    io::di, io::uo) is det.

output_cast_rval(Info, Type, Expr, !IO) :-
    % rtti_to_mlds.m generates casts from int to
    % jmercury.runtime.PseudoTypeInfo, but for Java
    % we need to treat these as constructions, not casts.
    % Similarly for conversions from TypeCtorInfo to TypeInfo.
    ( if
        Type = mlds_pseudo_type_info_type,
        Expr = ml_const(mlconst_int(N))
    then
        maybe_output_comment(Info, "cast", !IO),
        ( if have_preallocated_pseudo_type_var(N) then
            io.write_string("jmercury.runtime.PseudoTypeInfo.K", !IO),
            io.write_int(N, !IO)
        else
            io.write_string("new jmercury.runtime.PseudoTypeInfo(", !IO),
            output_rval(Info, Expr, !IO),
            io.write_string(")", !IO)
        )
    else if
        ( Type = mercury_type(_, ctor_cat_system(cat_system_type_info), _)
        ; Type = mlds_type_info_type
        )
    then
        % XXX We really should be able to tell if we are casting a
        % TypeCtorInfo or a TypeInfo. Julien says that's probably going to
        % be rather difficult as the compiler doesn't keep track of where
        % type_ctor_infos are acting as type_infos properly.
        maybe_output_comment(Info, "cast", !IO),
        io.write_string("jmercury.runtime.TypeInfo_Struct.maybe_new(",
            !IO),
        output_rval(Info, Expr, !IO),
        io.write_string(")", !IO)
    else if
        java_builtin_type(Type, "int", _, _)
    then
        io.write_string("(int) ", !IO),
        output_rval_maybe_with_enum(Info, Expr, !IO)
    else
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

:- pred output_boxed_rval(java_out_info::in, mlds_type::in, mlds_rval::in,
     io::di, io::uo) is det.

output_boxed_rval(Info, Type, Expr, !IO) :-
    ( if java_builtin_type(Type, _JavaName, JavaBoxedName, _) then
        % valueOf may return cached instances instead of creating new objects.
        io.write_string(JavaBoxedName, !IO),
        io.write_string(".valueOf(", !IO),
        output_rval(Info, Expr, !IO),
        io.write_string(")", !IO)
    else
        io.write_string("((java.lang.Object) (", !IO),
        output_rval(Info, Expr, !IO),
        io.write_string("))", !IO)
    ).

:- pred output_unboxed_rval(java_out_info::in, mlds_type::in, mlds_rval::in,
    io::di, io::uo) is det.

output_unboxed_rval(Info, Type, Expr, !IO) :-
    ( if java_builtin_type(Type, _, JavaBoxedName, UnboxMethod) then
        io.write_string("((", !IO),
        io.write_string(JavaBoxedName, !IO),
        io.write_string(") ", !IO),
        output_bracketed_rval(Info, Expr, !IO),
        io.write_string(").", !IO),
        io.write_string(UnboxMethod, !IO),
        io.write_string("()", !IO)
    else
        io.write_string("((", !IO),
        output_type(Info, Type, !IO),
        io.write_string(") ", !IO),
        output_rval(Info, Expr, !IO),
        io.write_string(")", !IO)
    ).

    % java_builtin_type(MLDS_Type, JavaUnboxedType, JavaBoxedType,
    %   UnboxMethod):
    %
    % For a given Mercury type, check if this corresponds to a Java type
    % which has both unboxed (builtin) and boxed (class) versions, and if so,
    % return their names, and the name of the method to get the unboxed value
    % from the boxed type.
    %
:- pred java_builtin_type(mlds_type::in, string::out, string::out, string::out)
    is semidet.

java_builtin_type(MLDS_Type, JavaUnboxedType, JavaBoxedType, UnboxMethod) :-
    require_complete_switch [MLDS_Type] (
        MLDS_Type = mlds_native_bool_type,
        JavaUnboxedType = "boolean",
        JavaBoxedType = "java.lang.Boolean",
        UnboxMethod = "booleanValue"
    ;
        % NOTE: Java's `char' type is not large enough for code points so we
        % must use `int'.  Java has no unsigned types so we represent them
        % as `int'.
        ( MLDS_Type = mlds_native_char_type
        ; MLDS_Type = mlds_native_int_type
        ; MLDS_Type = mlds_native_uint_type
        ),
        JavaUnboxedType = "int",
        JavaBoxedType = "java.lang.Integer",
        UnboxMethod = "intValue"
    ;
        MLDS_Type = mlds_native_float_type,
        JavaUnboxedType = "double",
        JavaBoxedType = "java.lang.Double",
        UnboxMethod = "doubleValue"
    ;
        MLDS_Type = mercury_type(MerType, TypeCtorCat, _),
        require_complete_switch [MerType] (
            MerType = builtin_type(BuiltinType),
            require_complete_switch [BuiltinType] (
                % The rationale for the handling of `char' and `uint' here is
                % the same as for the mlds_native types above.
                ( BuiltinType = builtin_type_char
                ; BuiltinType = builtin_type_int
                ; BuiltinType = builtin_type_uint
                ),
                JavaUnboxedType = "int",
                JavaBoxedType = "java.lang.Integer",
                UnboxMethod = "intValue"
            ;
                BuiltinType = builtin_type_float,
                JavaUnboxedType = "double",
                JavaBoxedType = "java.lang.Double",
                UnboxMethod = "doubleValue"
            ;
                BuiltinType = builtin_type_string,
                fail
            )
        ;
            MerType = defined_type(_, _, _),
            require_complete_switch [TypeCtorCat] (
                % io.state and store.store(S) are dummy variables for which we
                % pass an arbitrary integer. For this reason they should have
                % the Java type `int'.
                TypeCtorCat = ctor_cat_builtin_dummy,
                JavaUnboxedType = "int",
                JavaBoxedType = "java.lang.Integer",
                UnboxMethod = "intValue"
            ;
                ( TypeCtorCat = ctor_cat_builtin(_)
                ; TypeCtorCat = ctor_cat_higher_order
                ; TypeCtorCat = ctor_cat_tuple
                ; TypeCtorCat = ctor_cat_enum(_)
                ; TypeCtorCat = ctor_cat_variable
                ; TypeCtorCat = ctor_cat_system(_)
                ; TypeCtorCat = ctor_cat_void
                ; TypeCtorCat = ctor_cat_user(_)
                ),
                fail
            )
        ;
            ( MerType = type_variable(_, _)
            ; MerType = tuple_type(_, _)
            ; MerType = higher_order_type(_, _, _, _, _)
            ; MerType = apply_n_type(_, _, _)
            ; MerType = kinded_type(_, _)
            ),
            fail
        )
    ;
        % Handle foreign types that map on to Java's primitive types specially
        % since we want to avoid boxing them where possible for performance
        % reasons.
        MLDS_Type = mlds_foreign_type(ForeignLangType),
        java_primitive_foreign_language_type(ForeignLangType, JavaUnboxedType,
            JavaBoxedType, UnboxMethod, _DefaultValue)
    ;
        ( MLDS_Type = mlds_mercury_array_type(_)
        ; MLDS_Type = mlds_cont_type(_)
        ; MLDS_Type = mlds_commit_type
        ; MLDS_Type = mlds_class_type(_, _, _)
        ; MLDS_Type = mlds_array_type(_)
        ; MLDS_Type = mlds_mostly_generic_array_type(_)
        ; MLDS_Type = mlds_ptr_type(_)
        ; MLDS_Type = mlds_func_type(_)
        ; MLDS_Type = mlds_generic_type
        ; MLDS_Type = mlds_generic_env_ptr_type
        ; MLDS_Type = mlds_type_info_type
        ; MLDS_Type = mlds_pseudo_type_info_type
        ; MLDS_Type = mlds_rtti_type(_)
        ; MLDS_Type = mlds_tabling_type(_)
        ),
        fail
    ;
        MLDS_Type = mlds_unknown_type,
        unexpected($file, $pred, "unknown typed")
    ).

:- pred java_primitive_foreign_language_type(foreign_language_type::in,
    string::out, string::out, string::out, string::out) is semidet.

java_primitive_foreign_language_type(ForeignLangType, PrimitiveType,
        BoxedType, UnboxMethod, DefaultValue) :-
    require_complete_switch [ForeignLangType]
    (
        ForeignLangType = java(java_type(JavaForeignType))
    ;
        ForeignLangType = c(_),
        unexpected($file, $pred, "foreign_type for C")
    ;
        ForeignLangType = csharp(_),
        unexpected($file, $pred, "foreign_type for C#")
    ;
        ForeignLangType = erlang(_),
        unexpected($file, $pred, "foreign_type for Erlang")
    ),
    PrimitiveType = string.strip(JavaForeignType),
    (
        PrimitiveType = "byte",
        BoxedType = "java.lang.Byte",
        UnboxMethod = "byteValue",
        DefaultValue = "0"
    ;
        PrimitiveType = "short",
        BoxedType = "java.lang.Short",
        UnboxMethod = "shortValue",
        DefaultValue = "0"
    ;
        PrimitiveType = "int",
        BoxedType = "java.lang.Integer",
        UnboxMethod = "intValue",
        DefaultValue = "0"
    ;
        PrimitiveType = "long",
        BoxedType = "java.lang.Long",
        UnboxMethod = "longValue",
        DefaultValue = "0"
    ;
        PrimitiveType = "float",
        BoxedType = "java.lang.Float",
        UnboxMethod = "floatValue",
        DefaultValue = "0"
    ;
        PrimitiveType = "double",
        BoxedType = "java.lang.Double",
        UnboxMethod = "doubleValue",
        DefaultValue = "0"
    ;
        PrimitiveType = "boolean",
        BoxedType = "java.lang.Boolean",
        UnboxMethod = "booleanValue",
        DefaultValue = "false"
    ;
        PrimitiveType = "char",
        BoxedType = "java.lang.Character",
        UnboxMethod = "charValue",
        DefaultValue = "'\\u0000'"
    ).

:- pred output_std_unop(java_out_info::in, builtin_ops.unary_op::in,
    mlds_rval::in, io::di, io::uo) is det.

    % For the Java back-end, there are no tags, so all the tagging operators
    % are no-ops, except for `tag', which always returns zero (a tag of zero
    % means there's no tag).
    %
output_std_unop(Info, UnaryOp, Expr, !IO) :-
    (
        UnaryOp = tag,
        io.write_string("/* tag */  0", !IO)
    ;
        ( UnaryOp = mktag,     UnaryOpStr = "/* mktag */ "
        ; UnaryOp = unmktag,   UnaryOpStr = "/* unmktag */ "
        ; UnaryOp = strip_tag, UnaryOpStr = "/* strip_tag */ "
        ; UnaryOp = mkbody,    UnaryOpStr = "/* mkbody */ "
        ; UnaryOp = unmkbody,   UnaryOpStr = "/* unmkbody */ "
        ; UnaryOp = bitwise_complement, UnaryOpStr = "~"
        ; UnaryOp = logical_not, UnaryOpStr = "!"
        ; UnaryOp = hash_string,  UnaryOpStr = "mercury.String.hash_1_f_0"
        ; UnaryOp = hash_string2, UnaryOpStr = "mercury.String.hash2_1_f_0"
        ; UnaryOp = hash_string3, UnaryOpStr = "mercury.String.hash3_1_f_0"
        ; UnaryOp = hash_string4, UnaryOpStr = "mercury.String.hash4_1_f_0"
        ; UnaryOp = hash_string5, UnaryOpStr = "mercury.String.hash5_1_f_0"
        ; UnaryOp = hash_string6, UnaryOpStr = "mercury.String.hash6_1_f_0"
        ; UnaryOp = uint_bitwise_complement, UnaryOpStr = "~"
        ),
        io.write_string(UnaryOpStr, !IO),
        io.write_string("(", !IO),
        output_rval(Info, Expr, !IO),
        io.write_string(")", !IO)
    ).

:- pred output_binop(java_out_info::in, binary_op::in, mlds_rval::in,
    mlds_rval::in, io::di, io::uo) is det.

output_binop(Info, Op, X, Y, !IO) :-
    (
        Op = array_index(_Type),
        output_bracketed_rval(Info, X, !IO),
        io.write_string("[", !IO),
        output_rval(Info, Y, !IO),
        io.write_string("]", !IO)
    ;
        Op = str_eq,
        output_rval(Info, X, !IO),
        io.write_string(".equals(", !IO),
        output_rval(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        ( Op = str_ne, OpStr = "!="
        ; Op = str_lt, OpStr = "<"
        ; Op = str_gt, OpStr = ">"
        ; Op = str_le, OpStr = "<="
        ; Op = str_ge, OpStr = ">="
        ),
        io.write_string("(", !IO),
        output_rval(Info, X, !IO),
        io.write_string(".compareTo(", !IO),
        output_rval(Info, Y, !IO),
        io.write_string(") ", !IO),
        io.write_string(OpStr, !IO),
        io.write_string(" 0)", !IO)
    ;
        Op = str_cmp,
        io.write_string("(", !IO),
        output_rval(Info, X, !IO),
        io.write_string(".compareTo(", !IO),
        output_rval(Info, Y, !IO),
        io.write_string(")) ", !IO)
    ;
        Op = pointer_equal_conservative,
        io.write_string("(", !IO),
        output_rval(Info, X, !IO),
        io.write_string(" == ", !IO),
        output_rval(Info, Y, !IO),
        io.write_string(") ", !IO)
    ;
        % XXX Should we abort for some of these?
        ( Op = int_add
        ; Op = int_sub
        ; Op = int_mul
        ; Op = int_div
        ; Op = int_mod
        ; Op = unchecked_left_shift
        ; Op = unchecked_right_shift
        ; Op = bitwise_and
        ; Op = bitwise_or
        ; Op = bitwise_xor
        ; Op = logical_and
        ; Op = logical_or
        ; Op = eq
        ; Op = ne
        ; Op = body
        ; Op = string_unsafe_index_code_unit
        ; Op = offset_str_eq(_)
        ; Op = int_lt
        ; Op = int_gt
        ; Op = int_le
        ; Op = int_ge
        ; Op = unsigned_le
        ; Op = float_plus
        ; Op = float_minus
        ; Op = float_times
        ; Op = float_divide
        ; Op = float_eq
        ; Op = float_ne
        ; Op = float_lt
        ; Op = float_gt
        ; Op = float_le
        ; Op = float_ge
        ; Op = float_word_bits
        ; Op = float_from_dword
        ; Op = compound_eq
        ; Op = compound_lt
        ),
        ( if rval_is_enum_object(X) then
            io.write_string("(", !IO),
            output_rval(Info, X, !IO),
            io.write_string(".MR_value ", !IO),
            output_binary_op(Op, !IO),
            io.write_string(" ", !IO),
            output_rval(Info, Y, !IO),
            io.write_string(".MR_value)", !IO)
        else
            io.write_string("(", !IO),
            output_rval(Info, X, !IO),
            io.write_string(" ", !IO),
            output_binary_op(Op, !IO),
            io.write_string(" ", !IO),
            output_rval(Info, Y, !IO),
            io.write_string(")", !IO)
        )
    ;
        ( Op = uint_eq
        ; Op = uint_ne
        ; Op = uint_add
        ; Op = uint_sub
        ; Op = uint_mul
        ; Op = uint_bitwise_and
        ; Op = uint_bitwise_or
        ; Op = uint_bitwise_xor
        ; Op = uint_unchecked_left_shift
        ; Op = uint_unchecked_right_shift
        ),
        io.write_string("(", !IO),
        output_rval(Info, X, !IO),
        io.write_string(" ", !IO),
        output_binary_op(Op, !IO),
        io.write_string(" ", !IO),
        output_rval(Info, Y, !IO),
        io.write_string(")", !IO)
    ;
        ( Op = uint_lt
        ; Op = uint_gt
        ; Op = uint_le
        ; Op = uint_ge
        ),
        io.write_string("(((", !IO),
        output_rval(Info, X, !IO),
        io.write_string(") & 0xffffffffL) ", !IO),
        output_binary_op(Op, !IO),
        io.write_string(" ((", !IO),
        output_rval(Info, Y, !IO),
        io.write_string(") & 0xffffffffL))", !IO)
    ;
        ( Op = uint_div
        ; Op = uint_mod
        ),
        io.write_string("((int)(((", !IO),
        output_rval(Info, X, !IO),
        io.write_string(") & 0xffffffffL) ", !IO),
        output_binary_op(Op, !IO),
        io.write_string(" ((", !IO),
        output_rval(Info, Y, !IO),
        io.write_string(") & 0xffffffffL)))", !IO)
    ).

    % Output an Rval and if the Rval is an enumeration object append the string
    % ".MR_value", so we can access its value field.
    %
    % XXX Note that this is necessary in some places, but not in others.
    % For example, it is important to do so for switch statements, as the
    % argument of a switch _must_ be an integer in Java. However, adding
    % the .MR_value to assignments breaks some casting... At some point, we
    % need to go through all the places where output_rval and
    % output_rval_maybe_with_enum are called and make sure the correct one
    % is being used.
    %
:- pred output_rval_maybe_with_enum(java_out_info::in, mlds_rval::in,
    io::di, io::uo) is det.

output_rval_maybe_with_enum(Info, Rval, !IO) :-
    output_rval(Info, Rval, !IO),
    ( if rval_is_enum_object(Rval) then
        io.write_string(".MR_value", !IO)
    else
        true
    ).

:- pred output_binary_op(binary_op::in, io::di, io::uo) is det.

output_binary_op(Op, !IO) :-
    (
        ( Op = int_add, OpStr = "+"
        ; Op = int_sub, OpStr = "-"
        ; Op = int_mul, OpStr = "*"
        ; Op = int_div, OpStr = "/"
        ; Op = int_mod, OpStr = "%"
        ; Op = unchecked_left_shift, OpStr = "<<"
        ; Op = unchecked_right_shift, OpStr = ">>"
        ; Op = bitwise_and, OpStr = "&"
        ; Op = bitwise_or, OpStr = "|"
        ; Op = bitwise_xor, OpStr = "^"
        ; Op = logical_and, OpStr = "&&"
        ; Op = logical_or, OpStr = "||"
        ; Op = eq, OpStr = "=="
        ; Op = ne, OpStr = "!="
        ; Op = int_lt, OpStr = "<"
        ; Op = int_gt, OpStr = ">"
        ; Op = int_le, OpStr = "<="
        ; Op = int_ge, OpStr = ">="

        ; Op = uint_eq, OpStr = "=="
        ; Op = uint_ne, OpStr = "!="
        % NOTE: unsigned comparisons require special handling in Java.
        % See output_binop/6 above.
        ; Op = uint_lt, OpStr = "<"
        ; Op = uint_gt, OpStr = ">"
        ; Op = uint_le, OpStr = "<="
        ; Op = uint_ge, OpStr = ">="

        ; Op = uint_add, OpStr = "+"
        ; Op = uint_sub, OpStr = "-"
        ; Op = uint_mul, OpStr = "*"

        % NOTE: unsigned div and mod require special handling in Java.
        % See output_binop/6 above.
        ; Op = uint_div, OpStr = "/"
        ; Op = uint_mod, OpStr = "%"

        ; Op = uint_bitwise_and, OpStr = "&"
        ; Op = uint_bitwise_or, OpStr = "|"
        ; Op = uint_bitwise_xor, OpStr = "^"
        ; Op = uint_unchecked_left_shift, OpStr = "<<"
        ; Op = uint_unchecked_right_shift, OpStr = ">>>"

        ; Op = float_eq, OpStr = "=="
        ; Op = float_ne, OpStr = "!="
        ; Op = float_le, OpStr = "<="
        ; Op = float_ge, OpStr = ">="
        ; Op = float_lt, OpStr = "<"
        ; Op = float_gt, OpStr = ">"

        ; Op = float_plus, OpStr = "+"
        ; Op = float_minus, OpStr = "-"
        ; Op = float_times, OpStr = "*"
        ; Op = float_divide, OpStr = "/"
        ),
        io.write_string(OpStr, !IO)
    ;
        ( Op = array_index(_)
        ; Op = body
        ; Op = float_from_dword
        ; Op = float_word_bits
        ; Op = offset_str_eq(_)
        ; Op = str_cmp
        ; Op = str_eq
        ; Op = str_ge
        ; Op = str_gt
        ; Op = str_le
        ; Op = str_lt
        ; Op = str_ne
        ; Op = string_unsafe_index_code_unit
        ; Op = pointer_equal_conservative
        ; Op = unsigned_le
        ; Op = compound_eq
        ; Op = compound_lt
        ),
        unexpected($module, $pred, "invalid binary operator")
    ).

:- pred output_rval_const(java_out_info::in, mlds_rval_const::in,
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
        Const = mlconst_uint(U),
        % Java does not have unsigned integer literals.
        % XXX perhaps we should output this in hexadecimal?
        output_int_const(uint.cast_to_int(U), !IO)
    ;
        Const = mlconst_char(N),
        io.write_string("(", !IO),
        output_int_const(N, !IO),
        io.write_string(")", !IO)
    ;
        Const = mlconst_enum(N, EnumType),
        output_type(Info, EnumType, !IO),
        io.write_string(".K", !IO),
        output_int_const(N, !IO)
    ;
        Const = mlconst_foreign(Lang, Value, _Type),
        expect(unify(Lang, lang_java), $module, $pred,
            "language other than Java."),
        % XXX Should we parenthesize this?
        io.write_string(Value, !IO)
    ;
        Const = mlconst_float(FloatVal),
        c_util.output_float_literal_cur_stream(FloatVal, !IO)
    ;
        Const = mlconst_string(String),
        io.write_string("""", !IO),
        c_util.output_quoted_string_lang_cur_stream(literal_java,
            String, !IO),
        io.write_string("""", !IO)
    ;
        Const = mlconst_multi_string(String),
        io.write_string("""", !IO),
        c_util.output_quoted_multi_string_lang_cur_stream(literal_java,
            String, !IO),
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
        Initializer = get_java_type_initializer(Type),
        io.write_string(Initializer, !IO)
    ).

:- pred output_int_const(int::in, io::di, io::uo) is det.

output_int_const(N, !IO) :-
    % The Mercury compiler could be using 64-bit integers but Java has 32-bit
    % ints. A literal 0xffffffff in a source file would be interpreted by a
    % 64-bit Mercury compiler as 4294967295. If it is written out in decimal,
    % a Java compiler would rightly complain because the integer is too large
    % to fit in a 32-bit int. However, it won't complain if the literal is
    % expressed in hexadecimal (nor as the negative decimal -1).
    ( if N < 0 then
        io.write_int(N, !IO)
    else if
        N >> 32 = 0,
        N /\ 0x80000000 = 0x80000000
    then
        % The bit pattern fits in 32 bits, but is too large to write as a
        % positive decimal. This branch is unreachable on a 32-bit compiler.
        io.format("0x%x", [i(N /\ 0xffffffff)], !IO)
    else
        io.write_int(N, !IO)
    ).

:- pred output_vector_common_row_rval(java_out_info::in,
    mlds_vector_common::in, mlds_rval::in, io::di, io::uo) is det.

output_vector_common_row_rval(Info, VectorCommon, RowRval, !IO) :-
    VectorCommon = ml_vector_common(_ModuleName, _Type,
        ml_vector_common_type_num(TypeNum), StartRowNum, _NumRows),
    io.format("MR_vector_common_%d[%d + ", [i(TypeNum), i(StartRowNum)], !IO),
    output_rval(Info, RowRval, !IO),
    io.write_string("]", !IO).

%-----------------------------------------------------------------------------%

:- pred mlds_output_code_addr(java_out_info::in, mlds_code_addr::in, bool::in,
    io::di, io::uo) is det.

mlds_output_code_addr(Info, CodeAddr, IsCall, !IO) :-
    (
        IsCall = no,
        % Not a function call, so we are taking the address of the
        % wrapper for that function (method).
        io.write_string("new ", !IO),
        AddrOfMap = Info ^ joi_addrof_map,
        map.lookup(AddrOfMap, CodeAddr, CodeAddrWrapper),
        CodeAddrWrapper = code_addr_wrapper(ClassName, MaybePtrNum),
        io.write_string(ClassName, !IO),
        io.write_string("_0(", !IO),
        (
            MaybePtrNum = yes(PtrNum),
            io.write_int(PtrNum, !IO)
        ;
            MaybePtrNum = no
        ),
        io.write_string(")", !IO)
    ;
        IsCall = yes,
        (
            CodeAddr = code_addr_proc(Label, _Sig),
            output_fully_qualified_thing(Label, mlds_output_proc_label, !IO)
        ;
            CodeAddr = code_addr_internal(Label, SeqNum, _Sig),
            output_fully_qualified_thing(Label, mlds_output_proc_label, !IO),
            io.write_string("_", !IO),
            io.write_int(SeqNum, !IO)
        )
    ).

:- pred mlds_output_proc_label(mlds_proc_label::in, io::di, io::uo) is det.

mlds_output_proc_label(mlds_proc_label(PredLabel, ProcId), !IO) :-
    output_pred_label(PredLabel, !IO),
    proc_id_to_int(ProcId, ModeNum),
    io.format("_%d", [i(ModeNum)], !IO).

:- pred mlds_output_data_addr(mlds_data_addr::in, io::di, io::uo) is det.

mlds_output_data_addr(data_addr(ModuleQualifier, DataName), !IO) :-
    SymName = mlds_module_name_to_sym_name(ModuleQualifier),
    mangle_sym_name_for_java(SymName, module_qual, "__", ModuleName),
    io.write_string(ModuleName, !IO),
    io.write_string(".", !IO),
    output_data_name_for_java(DataName, !IO).

%-----------------------------------------------------------------------------%
%
% Miscellaneous stuff to handle indentation and generation of
% source context annotations.
%

:- mutable(last_context, prog_context, context_init, ground,
    [untrailed, attach_to_io_state]).

:- type context_marker
    --->    marker_begin_block
            % The beginning of some Java foreign code whose errors
            % should be reported with Mercury line numbers.

    ;       marker_end_block
            % The end of such a block.

    ;       marker_comment.
            % This marks mercury generated code for which Java's line numbers
            % should be used, it's just a comment for the Mercury developers.

:- pred output_context(bool::in, context_marker::in,
    prog_context::in, io::di, io::uo) is det.

output_context(OutputLineNumbers, Marker, ProgContext, !IO) :-
    (
        OutputLineNumbers = yes,
        get_last_context(LastContext, !IO),
        term.context_file(ProgContext, File),
        term.context_line(ProgContext, Line),
        ( if
            % It is safe to ignore marker comments when the comment isn't
            % useful. All other marker types must be emitted in all cases.
            (
                Marker = marker_comment
            =>
                (
                    ProgContext \= LastContext,
                    Line > 0,
                    File \= ""
                )
            )
        then
            % Java doesn't have an equivalent of #line directives.
            % We use the token MER_LINE to allow us to filter these lines out
            % of the file when mangling javac's output.
            % \u is treated as a Unicode escape even with comments.
            string.replace_all(File, "\\u", "\\\\u", SafePath),
            % Do not modify this format string without modifying
            % mfilterjavac/mfilterjavac.m
            io.format("// %s %s:%d\n",
                [s(marker_string(Marker)), s(SafePath), i(Line)], !IO),
            set_last_context(ProgContext, !IO)
        else
            true
        )
    ;
        OutputLineNumbers = no
    ).

    % Do not modify these strings without modifying util/mfilterjavac.m
    %
:- func marker_string(context_marker) = string.

marker_string(marker_begin_block) = "MER_FOREIGN_BEGIN".
marker_string(marker_end_block) = "MER_FOREIGN_END".
marker_string(marker_comment) = "".

:- pred indent_line_prog_context(bool::in, context_marker::in,
    prog_context::in, indent::in, io::di, io::uo) is det.

indent_line_prog_context(OutputLineNumbers, Marker, Context, N, !IO) :-
    output_context(OutputLineNumbers, Marker, Context, !IO),
    indent_line(N, !IO).

:- pred indent_line(bool::in, context_marker::in, mlds_context::in,
    indent::in, io::di, io::uo) is det.

indent_line(OutputLineNumbers, Marker, Context, N, !IO) :-
    ProgContext = mlds_get_prog_context(Context),
    indent_line_prog_context(OutputLineNumbers, Marker, ProgContext, N, !IO).

    % A value of type `indent' records the number of levels of indentation
    % to indent the next piece of code. Currently we output two spaces
    % for each level of indentation.
    % XXX There is a small amount of code duplication with mlds_to_c.m here.
:- type indent == int.

:- pred indent_line(indent::in, io::di, io::uo) is det.

indent_line(N, !IO) :-
    ( if N =< 0 then
        true
    else
        io.write_string("  ", !IO),
        indent_line(N - 1, !IO)
    ).

:- pred write_string_with_context_block(java_out_info::in, indent::in,
    string::in, prog_context::in, io::di, io::uo) is det.

write_string_with_context_block(Info, Indent, Code, Context, !IO) :-
    indent_line_prog_context(Info ^ joi_foreign_line_numbers,
        marker_begin_block, Context, Indent, !IO),
    io.write_string(Code, !IO),
    io.nl(!IO),
    % The num_lines(Code) call is supposed to count the number of lines
    % occupied by Code in the source file. The result will be incorrect if
    % there were any escape sequences representing CR or LF characters --
    % they are expanded out in Code.
    Context = context(File, Lines0),
    ContextEnd = context(File, Lines0 + num_lines(Code)),
    indent_line_prog_context(Info ^ joi_foreign_line_numbers,
        marker_end_block, ContextEnd, Indent, !IO).

:- func num_lines(string) = int.

num_lines(String) = Num :-
    % The initial "previous" character may be anything other than \r.
    string.foldl2(count_new_lines, String, 1, Num, 'x', _).

    % Increment the line count !N whenever we see CR or LF or CRLF,
    % ensuring that the latter counts as only ONE newline.
    %
:- pred count_new_lines(char::in, int::in, int::out, char::in, char::out)
    is det.

count_new_lines(C, !N, Prev, C) :-
    ( if
        (
            C = '\r'
        ;
            (
                C = '\n',
                Prev \= '\r'
            )
        )
    then
        !:N = !.N + 1
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- type java_out_info
    --->    java_out_info(
                % These are static.
                joi_module_info     :: module_info,
                joi_auto_comments   :: bool,
                joi_line_numbers    :: bool,
                joi_foreign_line_numbers :: bool,
                joi_module_name     :: mlds_module_name,
                joi_source_filename :: string,
                joi_addrof_map      :: map(mlds_code_addr, code_addr_wrapper),

                % These are dynamic.
                joi_output_generics :: output_generics,
                joi_univ_tvars      :: list(tvar)
            ).

:- type output_generics
    --->    do_output_generics
    ;       do_not_output_generics.

:- func init_java_out_info(module_info, string,
    map(mlds_code_addr, code_addr_wrapper)) = java_out_info.

init_java_out_info(ModuleInfo, SourceFileName, AddrOfMap) = Info :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, auto_comments, AutoComments),
    globals.lookup_bool_option(Globals, line_numbers, LineNumbers),
    globals.lookup_bool_option(Globals, line_numbers_around_foreign_code,
        ForeignLineNumbers),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    Info = java_out_info(ModuleInfo, AutoComments,
        LineNumbers, ForeignLineNumbers, MLDS_ModuleName, SourceFileName,
        AddrOfMap, do_not_output_generics, []).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_java.
%-----------------------------------------------------------------------------%
