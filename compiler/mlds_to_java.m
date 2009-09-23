%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2009 The University of Melbourne.
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
%   (The problem with current code generation scheme for nested
%   modules is that Java does not allow the name of a class to
%   be the same as the name of its enclosing package.
%   That should work now, but javac doesn't like the filenames
%   we give for submodules.)
%
% - Support for Java in Mmake.
%
% - Generate names of classes etc. correctly (mostly same as IL backend)
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

:- import_module hlds.hlds_module.
:- import_module ml_backend.mlds.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred output_java_mlds(module_info::in, mlds::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

    % XXX needed for c_util.output_quoted_string,
    %     c_util.output_quoted_multi_string, and
    %     c_util.make_float_literal.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.rtti.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_pred.           % for pred_proc_id.
:- import_module libs.compiler_util.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.java_util.
:- import_module ml_backend.ml_code_util.  % for ml_gen_local_var_decl_flags.
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
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
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

%-----------------------------------------------------------------------------%

output_java_mlds(ModuleInfo, MLDS, !IO) :-
    % Note that the Java file name that we use for modules in the
    % Mercury standard library do not include a "mercury." prefix;
    % that's why we don't call mercury_module_name_to_mlds here.
    ModuleName = mlds_get_module_name(MLDS),
    module_name_to_file_name(ModuleName, ".java", do_create_dirs,
        JavaSourceFile, !IO),
    Indent = 0,
    output_to_file(JavaSourceFile,
        output_java_src_file(ModuleInfo, Indent, MLDS), !IO).

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

    % Succeeds iff this type is a enumeration.
    %
:- pred type_is_enum(mlds_type::in) is semidet.

type_is_enum(Type) :-
    Type = mercury_type(_, Builtin, _),
    Builtin = ctor_cat_enum(_).

    % Succeeds iff this type is something that the Java backend will represent
    % as an object i.e. something created using the new operator.
    %
:- pred type_is_object(mlds_type::in) is semidet.

type_is_object(Type) :-
    Type = mercury_type(_, CtorCat, _),
    type_category_is_object(CtorCat) = yes.

:- func type_category_is_object(type_ctor_category) = bool.

type_category_is_object(CtorCat) = IsObject :-
    (
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_void
        ),
        IsObject = no
    ;
        ( CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_user(_)
        ),
        IsObject = yes
    ).

    % Given an lval, return its type.
    %
:- func mlds_lval_type(mlds_lval) = mlds_type.

mlds_lval_type(ml_var(_, VarType)) = VarType.
mlds_lval_type(ml_field(_, _, _, FieldType, _)) = FieldType.
mlds_lval_type(ml_mem_ref(_, PtrType)) =
    ( PtrType = mlds_ptr_type(Type) ->
        Type
    ;
        unexpected(this_file, "mlds_lval_type: mem_ref of non-pointer")
    ).
mlds_lval_type(ml_global_var_ref(_)) = _ :-
    sorry(this_file, "mlds_lval_type: global_var_ref NYI").

    % Succeeds iff the Rval represents an integer constant.
    %
:- pred rval_is_int_const(mlds_rval::in) is semidet.

rval_is_int_const(Rval) :-
    Rval = ml_const(mlconst_int(_)).

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
% XXX None of this stuff works as it should. The idea is that class
% names should start with an uppercase letter, while method names and
% package specifiers should start with a lowercase letter.
% The current implementation of the MLDS makes this rather harder to achieve
% than it might initially seem.  The current position is that coding
% conventions are only enforced on library modules.
% This is needed as Java compilers don't take too well to compiling
% classes named `char',`int', `float' etc.
% XXX It might be nice if the name mangling code was taken out of which
% ever LLDS module it's hiding in and put in a separate one.
%

    % XXX This won't work if we start using the Java coding conventions
    % for all names. At the moment it only affects library modules.
    %
:- pred enforce_java_names(string::in, string::out) is det.

enforce_java_names(Name, JavaName) :-
    % If the Name contains one or more dots (`.'), then capitalize
    % the first letter after the last dot.
    reverse_string(Name, RevName),
    ( string.sub_string_search(RevName, ".", Pos) ->
        string.split(RevName, Pos, Head0, Tail0),
        reverse_string(Tail0, Tail),
        reverse_string(Head0, Head1),
        string.capitalize_first(Head1, Head),
        string.append(Tail, Head, JavaName)
    ;
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
            unexpected(this_file,
                "import_type `user_visible_interface' in Java backend")
        ;
            ImportType = compiler_visible_interface
        )
    ;
        Import = foreign_import(_),
        unexpected(this_file, "foreign import in Java backend")
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
        ScalarCellGroupMap, VectorCellGroupMap, GlobalDefns),
    expect(map.is_empty(ScalarCellGroupMap), this_file,
        "output_java_src_file: nonempty ScalarCellGroupMap"),
    expect(map.is_empty(VectorCellGroupMap), this_file,
        "output_java_src_file: nonempty VectorCellGroupMap"),

    % Do NOT enforce the outermost "mercury" qualifier here.  This module
    % name is compared with other module names in the MLDS, to avoid
    % unnecessary module qualification.
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),

    % Find and build list of all methods which would have their addresses
    % taken to be used as a function pointer.
    find_pointer_addressed_methods(GlobalDefns, [], CodeAddrs0),
    find_pointer_addressed_methods(Defns0, CodeAddrs0, CodeAddrs1),
    CodeAddrs = list.sort_and_remove_dups(CodeAddrs1),

    % Create wrappers in MLDS for all pointer addressed methods.
    list.map(generate_addr_wrapper_class, CodeAddrs, WrapperDefns),
    Defns1 = GlobalDefns ++ WrapperDefns ++ Defns0,

    % Rename classes with excessively long names.
    shorten_long_class_names(MLDS_ModuleName, Defns1, Defns),

    % Get the foreign code for Java
    % XXX We should not ignore _RevImports.
    ForeignCode = mlds_get_java_foreign_code(AllForeignCode),
    ForeignCode = mlds_foreign_code(RevForeignDecls, _RevImports,
        RevBodyCode, ExportDefns),
    ForeignDecls = list.reverse(RevForeignDecls),
    ForeignBodyCode = list.reverse(RevBodyCode),

    % Output transformed MLDS as Java source.
    %
    % The order is important here, because Java requires static constants
    % be defined before they can be used in static initializers.
    % We start with the Java foreign code declarations, since for
    % library/private_builtin.m they contain static constants
    % that will get used in the RTTI definitions.
    output_src_start(Indent, ModuleName, Imports, ForeignDecls, Defns, !IO),
    io.write_list(ForeignBodyCode, "\n", output_java_body_code(Indent), !IO),
    list.filter(defn_is_rtti_data, Defns, RttiDefns, NonRttiDefns),
    io.write_string("\n// RttiDefns\n", !IO),
    output_defns(Indent + 1, ModuleInfo, MLDS_ModuleName, alloc_only,
        RttiDefns, !IO),
    output_rtti_assignments(Indent + 1, ModuleInfo, MLDS_ModuleName,
        RttiDefns, !IO),
    io.write_string("\n// NonRttiDefns\n", !IO),
    output_defns(Indent + 1, ModuleInfo, MLDS_ModuleName, none,
        NonRttiDefns, !IO),
    io.write_string("\n// ExportDefns\n", !IO),
    output_exports(Indent + 1, ModuleInfo, MLDS_ModuleName, none,
        ExportDefns, !IO),
    io.write_string("\n// ExportedEnums\n", !IO),
    output_exported_enums(Indent + 1, ModuleInfo, ExportedEnums, !IO),
    io.write_string("\n// InitPreds\n", !IO),
    output_inits(Indent + 1, ModuleInfo, InitPreds, !IO),
    io.write_string("\n// FinalPreds\n", !IO),
    output_finals(Indent + 1, ModuleInfo, FinalPreds, !IO),
    io.write_string("\n// EnvVarNames\n", !IO),
    output_env_vars(Indent + 1, NonRttiDefns, !IO),
    output_src_end(Indent, ModuleName, !IO).
    % XXX Need to handle non-Java foreign code at this point.

%-----------------------------------------------------------------------------%
%
% Code for working with Java `foreign_code'.
%

:- pred output_java_decl(indent::in, foreign_decl_code::in, io::di, io::uo)
    is det.

output_java_decl(Indent, DeclCode, !IO) :-
    DeclCode = foreign_decl_code(Lang, _IsLocal, Code, Context),
    (
        Lang = lang_java,
        indent_line(mlds_make_context(Context), Indent, !IO),
        io.write_string(Code, !IO),
        io.nl(!IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_csharp
        ; Lang = lang_il
        ; Lang = lang_erlang
        ),
        sorry(this_file, "foreign decl other than Java")
    ).

:- pred output_java_body_code(indent::in, user_foreign_code::in, io::di,
    io.state::uo) is det.

output_java_body_code(Indent, user_foreign_code(Lang, Code, Context), !IO) :-
    % Only output Java code.
    (
        Lang = lang_java,
        indent_line(mlds_make_context(Context), Indent, !IO),
        io.write_string(Code, !IO),
        io.nl(!IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_csharp
        ; Lang = lang_il
        ; Lang = lang_erlang
        ),
        sorry(this_file, "foreign code other than Java")
    ).

    % Get the foreign code for Java.
    %
:- func mlds_get_java_foreign_code(map(foreign_language, mlds_foreign_code))
    = mlds_foreign_code.

mlds_get_java_foreign_code(AllForeignCode) = ForeignCode :-
    ( map.search(AllForeignCode, lang_java, ForeignCode0) ->
        ForeignCode = ForeignCode0
    ;
        ForeignCode = mlds_foreign_code([], [], [], [])
    ).

%-----------------------------------------------------------------------------%
%
% Code for handling `pragma foreign_export' for Java
%

    % Exports are converted into forwarding methods that are given the
    % specified name.  These simply call the exported procedure.
    %
    % NOTE: the forwarding methods must be declared public as they might
    % be referred to within foreign_procs that are inlined across module
    % boundaries.
    %
:- pred output_exports(indent::in, module_info::in, mlds_module_name::in,
    output_aux::in, list(mlds_pragma_export)::in, io::di, io::uo) is det.

output_exports(_, _, _, _, [], !IO).
output_exports(Indent, ModuleInfo, MLDS_ModuleName, OutputAux,
        [Export | Exports], !IO) :-
    Export = ml_pragma_export(Lang, ExportName, MLDS_Name, MLDS_Signature,
        MLDS_Context),
    expect(unify(Lang, lang_java), this_file,
        "foreign_export for language other than Java."),
    indent_line(Indent, !IO),
    io.write_string("public static ", !IO),
    MLDS_Signature = mlds_func_params(Parameters, ReturnTypes),
    (
        ReturnTypes = [],
        io.write_string("void", !IO)
    ;
        ReturnTypes = [RetType],
        output_type(normal_style, RetType, !IO)
    ;
        ReturnTypes = [_, _ | _],
        % For multiple outputs, we return an array of objects.
        io.write_string("java.lang.Object []", !IO)
    ),
    io.write_string(" " ++ ExportName, !IO),
    output_params(Indent + 1, MLDS_ModuleName, MLDS_Context, Parameters, !IO),
    io.nl(!IO),
    indent_line(Indent, !IO),
    io.write_string("{\n", !IO),
    indent_line(Indent + 1, !IO),
    (
        ReturnTypes = []
    ;
        ReturnTypes = [_ | _],
        io.write_string("return ", !IO)
    ),
    output_fully_qualified_name(MLDS_Name, !IO),
    io.write_char('(', !IO),
    WriteCallArg = (pred(Arg::in, !.IO::di, !:IO::uo) is det :-
        Arg = mlds_argument(Name, _, _),
        output_name(Name, !IO)
    ),
    io.write_list(Parameters, ", ", WriteCallArg, !IO),
    io.write_string(");\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO),
    output_exports(Indent, ModuleInfo, MLDS_ModuleName, OutputAux, Exports,
        !IO).

%-----------------------------------------------------------------------------%
%
% Code for handling `pragma foreign_export_enum' for Java
%

:- pred output_exported_enums(indent::in, module_info::in,
    list(mlds_exported_enum)::in, io::di, io::uo) is det.

output_exported_enums(Indent, ModuleInfo, ExportedEnums, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    list.foldl(output_exported_enum(Indent, ModuleInfo, MLDS_ModuleName),
        ExportedEnums, !IO).

:- pred output_exported_enum(indent::in, module_info::in, mlds_module_name::in,
    mlds_exported_enum::in, io::di, io::uo) is det.

output_exported_enum(Indent, ModuleInfo, MLDS_ModuleName, ExportedEnum, !IO) :-
    ExportedEnum = mlds_exported_enum(Lang, _, TypeCtor, ExportedConstants0),
    (
        Lang = lang_java,
        ml_gen_type_name(TypeCtor, ClassName, ClassArity),
        MLDS_Type = mlds_class_type(ClassName, ClassArity, mlds_enum),
        % We reverse the list so the constants are printed out in order.
        list.reverse(ExportedConstants0, ExportedConstants),
        list.foldl(output_exported_enum_constant(Indent, ModuleInfo,
            MLDS_ModuleName, MLDS_Type), ExportedConstants, !IO)
    ;
        ( Lang = lang_c
        ; Lang = lang_csharp
        ; Lang = lang_il
        ; Lang = lang_erlang
        )
    ).

:- pred output_exported_enum_constant(indent::in, module_info::in,
    mlds_module_name::in, mlds_type::in, mlds_exported_enum_constant::in,
    io::di, io::uo) is det.

output_exported_enum_constant(Indent, ModuleInfo, MLDS_ModuleName, MLDS_Type,
        ExportedConstant, !IO) :-
    ExportedConstant = mlds_exported_enum_constant(Name, Initializer),
    indent_line(Indent, !IO),
    io.write_string("public static final ", !IO),
    output_type(normal_style, MLDS_Type, !IO),
    io.write_string(" ", !IO),
    io.write_string(Name, !IO),
    io.write_string(" = ", !IO),
    output_type(normal_style, MLDS_Type, !IO),
    io.write_string(".K", !IO),
    % XXX this will break with `:- pragma foreign_enum'
    output_initializer_body(ModuleInfo, Initializer, no, MLDS_ModuleName, !IO),
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

:- pred method_ptrs_in_entity_defn(mlds_entity_defn::in,
    list(mlds_code_addr)::in, list(mlds_code_addr)::out) is det.

method_ptrs_in_entity_defn(mlds_function(_MaybeID, _Params, Body,
        _Attributes, _EnvVars), !CodeAddrs) :-
    (
        Body = body_defined_here(Statement),
        method_ptrs_in_statement(Statement, !CodeAddrs)
    ;
        Body = body_external
    ).
method_ptrs_in_entity_defn(mlds_data(_Type, Initializer, _GCStatement),
        !CodeAddrs) :-
    method_ptrs_in_initializer(Initializer, !CodeAddrs).
method_ptrs_in_entity_defn(mlds_class(ClassDefn), !CodeAddrs) :-
    ClassDefn = mlds_class_defn(_, _, _, _, Ctors, Members),
    method_ptrs_in_defns(Ctors, !CodeAddrs),
    method_ptrs_in_defns(Members, !CodeAddrs).

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
method_ptrs_in_stmt(ml_stmt_while(Rval, Statement, _Bool), !CodeAddrs) :-
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
    unexpected(this_file,
        "method_ptrs_in_stmt: labels not supported in Java.").
method_ptrs_in_stmt(ml_stmt_goto(goto_break), !CodeAddrs).
method_ptrs_in_stmt(ml_stmt_goto(goto_continue), !CodeAddrs).
method_ptrs_in_stmt(ml_stmt_goto(goto_label(_)), _, _) :-
    unexpected(this_file,
        "method_ptrs_in_stmt: goto label not supported in Java.").
method_ptrs_in_stmt(ml_stmt_computed_goto(_, _), _, _) :-
    unexpected(this_file,
        "method_ptrs_in_stmt: computed gotos not supported in Java.").
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
        _IsTailCall),
    % We don't check "_Rval" - it may be a code address but is a
    % standard call rather than a function pointer use.
    method_ptrs_in_rvals(Rvals, !CodeAddrs).
method_ptrs_in_stmt(ml_stmt_atomic(AtomicStatement), !CodeAddrs) :-
    (
        AtomicStatement = new_object(Lval, _MaybeTag, _Bool,
            _Type, _MemRval, _MaybeCtorName, Rvals, _Types, _MayUseAtomic)
    ->
        % We don't need to check "_MemRval" since this just stores
        % the amount of memory needed for the new object.
        method_ptrs_in_lval(Lval, !CodeAddrs),
        method_ptrs_in_rvals(Rvals, !CodeAddrs)
    ; AtomicStatement = assign(Lval, Rval) ->
        method_ptrs_in_lval(Lval, !CodeAddrs),
        method_ptrs_in_rval(Rval, !CodeAddrs)
    ;
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

method_ptrs_in_rval(ml_lval(Lval), !CodeAddrs) :-
    method_ptrs_in_lval(Lval, !CodeAddrs).
method_ptrs_in_rval(ml_mkword(_Tag, Rval), !CodeAddrs) :-
    method_ptrs_in_rval(Rval, !CodeAddrs).
method_ptrs_in_rval(ml_const(RvalConst), !CodeAddrs) :-
    (
        RvalConst = mlconst_code_addr(CodeAddr),
        !:CodeAddrs = [CodeAddr | !.CodeAddrs]
    ;
        ( RvalConst = mlconst_true
        ; RvalConst = mlconst_false
        ; RvalConst = mlconst_int(_)
        ; RvalConst = mlconst_foreign(_, _, _)
        ; RvalConst = mlconst_float(_)
        ; RvalConst = mlconst_string(_)
        ; RvalConst = mlconst_multi_string(_)
        ; RvalConst = mlconst_named_const(_)
        ; RvalConst = mlconst_data_addr(_)
        ; RvalConst = mlconst_null(_)
        )
    ).
method_ptrs_in_rval(ml_unop(_UnaryOp, Rval), !CodeAddrs) :-
    method_ptrs_in_rval(Rval, !CodeAddrs).
method_ptrs_in_rval(ml_binop(_BinaryOp, RvalA, RvalB), !CodeAddrs) :-
    method_ptrs_in_rval(RvalA, !CodeAddrs),
    method_ptrs_in_rval(RvalB, !CodeAddrs).
method_ptrs_in_rval(ml_scalar_common(_), !CodeAddrs).
method_ptrs_in_rval(ml_vector_common_row(_, RowRval), !CodeAddrs) :-
    method_ptrs_in_rval(RowRval, !CodeAddrs).
method_ptrs_in_rval(ml_mem_addr(_Address), !CodeAddrs).
method_ptrs_in_rval(ml_self(_Type), !CodeAddrs).

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
% XXX This implementation will not corectly handle the case which occurs where
% there are two or more overloaded MLDS functions (that we take the address of)
% with the same name and arity but different argument types, both in the same
% module. This is due to the fact that the names of the generated wrapper
% classes are based purely on the method name.

    % Generate the MLDS wrapper class for a given code_addr.
    %
:- pred generate_addr_wrapper_class(mlds_code_addr::in, mlds_defn::out) is det.

generate_addr_wrapper_class(CodeAddr, ClassDefn) :-
    % Create a method that calls the original predicate.
    generate_call_method(CodeAddr, MethodDefn, Arity),

    ( Arity =< max_specialised_method_ptr_arity ->
        InterfaceName = "MethodPtr" ++ string.from_int(Arity)
    ;
        InterfaceName = "MethodPtrN"
    ),
    InterfaceModuleName = mercury_module_name_to_mlds(
        mercury_runtime_package_name),
    Interface = qual(InterfaceModuleName, module_qual, InterfaceName),

    % Create class components.
    ClassImports = [],
    ClassExtends = [],
    InterfaceDefn = mlds_class_type(Interface, 0, mlds_interface),
    ClassImplements = [InterfaceDefn],

    % Create a name for this wrapper class based on the fully qualified method
    % (predicate) name.
    create_addr_wrapper_name(CodeAddr, MangledClassEntityName),

    % XXX We should fill in the Context properly. This would probably involve
    % also returning context information for each "code_addr" returned by the
    % "method_ptrs_*" predicates above.
    Context = mlds_make_context(term.context_init),

    % Put it all together.
    ClassMembers  = [MethodDefn],
    ClassCtors    = [],
    ClassName     = entity_type(MangledClassEntityName, 0),
    ClassContext  = Context,
    ClassFlags    = addr_wrapper_decl_flags,
    ClassBodyDefn = mlds_class_defn(mlds_class, ClassImports,
        ClassExtends, ClassImplements, ClassCtors, ClassMembers),
    ClassBody     = mlds_class(ClassBodyDefn),
    ClassDefn = mlds_defn(ClassName, ClassContext, ClassFlags, ClassBody).

:- pred create_addr_wrapper_name(mlds_code_addr::in, string::out) is det.

create_addr_wrapper_name(CodeAddr, MangledClassEntityName) :-
    (
        CodeAddr = code_addr_proc(ProcLabel, _FuncSig),
        MaybeSeqNum = no
    ;
        CodeAddr = code_addr_internal(ProcLabel, SeqNum, _FuncSig),
        MaybeSeqNum = yes(SeqNum)
    ),
    ProcLabel = qual(ModuleQualifier, QualKind,
        mlds_proc_label(PredLabel, ProcID)),
    PredName = make_pred_name_string(PredLabel, ProcID, MaybeSeqNum),

    % Create a name for this wrapper class based on the fully qualified method
    % (predicate) name.
    ModuleQualifierSym = mlds_module_name_to_sym_name(ModuleQualifier),
    mangle_sym_name_for_java(ModuleQualifierSym, convert_qual_kind(QualKind),
        "__", ModuleNameStr),
    ClassEntityName = "addrOf__" ++ ModuleNameStr ++ "__" ++ PredName,
    MangledClassEntityName = name_mangle_no_leading_digit(ClassEntityName).

    % The highest arity for which there is a specialised MethodPtr<n> interface.
    %
:- func max_specialised_method_ptr_arity = int.

max_specialised_method_ptr_arity = 15.

    % Generates a call methods which calls the original method we have
    % created the wrapper for.
    %
:- pred generate_call_method(mlds_code_addr::in, mlds_defn::out, int::out)
    is det.

generate_call_method(CodeAddr, MethodDefn, Arity) :-
    (
        CodeAddr = code_addr_proc(ProcLabel, OrigFuncSignature)
    ;
        CodeAddr = code_addr_internal(ProcLabel, _SeqNum, OrigFuncSignature)
    ),
    OrigFuncSignature = mlds_func_signature(OrigArgTypes, OrigRetTypes),
    % XXX We should fill in the Context properly.
    Context = mlds_make_context(term.context_init),
    ModuleName = ProcLabel ^ mod_name,
    PredID = hlds_pred.initial_pred_id,
    ProcID = initial_proc_id,

    % Create new method name.
    Label = mlds_special_pred_label("call", no, "", 0),
    MethodName = entity_function(Label, ProcID, no, PredID),

    list.length(OrigArgTypes, Arity),

    % Create method arguments and call arguments.  For low arities we have
    % specialised MethodPtr interfaces which contain a method which takes n
    % arguments directly.  For higher arities the arguments are passed in as an
    % array.
    ( Arity =< max_specialised_method_ptr_arity ->
        list.map2_foldl(generate_call_method_nth_arg(ModuleName),
            OrigArgTypes, MethodArgs, CallArgs, 1, _Arity)
    ;
        MethodArgVariable = mlds_var_name("args", no),
        MethodArgType = mlds_argument(
            entity_data(mlds_data_var(MethodArgVariable)),
            mlds_array_type(mlds_generic_type), gc_no_stmt),
        MethodArgs = [MethodArgType],
        CallArgLabel = qual(ModuleName, module_qual, MethodArgVariable),
        generate_call_method_array_args(OrigArgTypes, CallArgLabel, 0, [],
            CallArgs)
    ),

    % Create return type.
    MethodRetType = mlds_generic_type,
    MethodRets = [MethodRetType],

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
        CallRetLvals, ordinary_call),
    CallStatement = statement(Call, Context),

    % Create a return statement that returns the result of the call to the
    % original method, boxed as a java.lang.Object.
    ReturnRval = ml_unop(box(ReturnVarType), ml_lval(ReturnLval)),
    Return = ml_stmt_return([ReturnRval]),
    ReturnStatement = statement(Return, Context),

    Block = ml_stmt_block(MethodDefns, [CallStatement, ReturnStatement]),
    Statements = statement(Block, Context),

    % Put it all together.
    MethodParams = mlds_func_params(MethodArgs, MethodRets),
    MethodMaybeID = no,
    MethodAttribs = [],
    MethodEnvVarNames = set.init,
    MethodBody   = mlds_function(MethodMaybeID, MethodParams,
        body_defined_here(Statements), MethodAttribs, MethodEnvVarNames),
    MethodFlags  = ml_gen_special_member_decl_flags,
    MethodDefn   = mlds_defn(MethodName, Context, MethodFlags, MethodBody).

:- pred generate_call_method_nth_arg(mlds_module_name::in, mlds_type::in,
    mlds_argument::out, mlds_rval::out, int::in, int::out) is det.

generate_call_method_nth_arg(ModuleName, Type, MethodArg, CallArg, I, I + 1) :-
    MethodArgVariable = mlds_var_name("arg" ++ string.from_int(I), no),
    MethodArg = mlds_argument(entity_data(mlds_data_var(MethodArgVariable)),
        mlds_generic_type, gc_no_stmt),
    CallArgLabel = qual(ModuleName, module_qual, MethodArgVariable),
    Rval = ml_lval(ml_var(CallArgLabel, mlds_generic_type)),
    CallArg = ml_unop(unbox(Type), Rval).

:- pred generate_call_method_array_args(list(mlds_type)::in, mlds_var::in,
    int::in, list(mlds_rval)::in, list(mlds_rval)::out) is det.

generate_call_method_array_args([], _, _, Args, Args).
generate_call_method_array_args([Type | Types], ArrayVar, Counter,
        Args0, Args) :-
    ArrayRval = ml_lval(ml_var(ArrayVar, mlds_native_int_type)),
    IndexRval = ml_const(mlconst_int(Counter)),
    ElemType = array_elem_scalar(scalar_elem_generic),
    Rval = ml_binop(array_index(ElemType), ArrayRval, IndexRval),
    UnBoxedRval = ml_unop(unbox(Type), Rval),
    Args1 = Args0 ++ [UnBoxedRval],
    generate_call_method_array_args(Types, ArrayVar, Counter + 1, Args1, Args).

:- func make_pred_name_string(mlds_pred_label, proc_id,
    maybe(mlds_func_sequence_num)) = string.

make_pred_name_string(PredLabel, ProcId, MaybeSeqNum) = NameStr :-
    PredLabelStr = pred_label_string(PredLabel),
    proc_id_to_int(ProcId, ModeNum),
    NameStr0 = PredLabelStr ++ "_" ++ string.int_to_string(ModeNum),
    (
        MaybeSeqNum = yes(SeqNum),
        NameStr = NameStr0 ++ "_" ++ string.int_to_string(SeqNum)
    ;
        MaybeSeqNum = no,
        NameStr = NameStr0
    ).

:- func pred_label_string(mlds_pred_label) = string.

pred_label_string(mlds_user_pred_label(PredOrFunc, MaybeDefiningModule, Name,
        PredArity, _CodeModel, _NonOutputFunc)) = PredLabelStr :-
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
    PredLabelStr0 = MangledName ++ "_" ++ string.int_to_string(OrigArity)
        ++ "_" ++ Suffix,
    (
        MaybeDefiningModule = yes(DefiningModule),
        MangledModuleName = sym_name_mangle(DefiningModule),
        PredLabelStr = PredLabelStr0 ++ "_in__" ++ MangledModuleName
    ;
        MaybeDefiningModule = no,
        PredLabelStr = PredLabelStr0
    ).
pred_label_string(mlds_special_pred_label(PredName, MaybeTypeModule, TypeName,
        TypeArity)) = PredLabelStr :-
    MangledPredName = name_mangle_no_leading_digit(PredName),
    MangledTypeName = name_mangle(TypeName),
    PredLabelStr0 = MangledPredName ++ "__",
    (
        MaybeTypeModule = yes(TypeModule),
        MangledModuleName = sym_name_mangle(TypeModule),
        PredLabelStr1 = PredLabelStr0 ++ "__" ++ MangledModuleName
    ;
        MaybeTypeModule = no,
        PredLabelStr1 = PredLabelStr0
    ),
    PredLabelStr = PredLabelStr1 ++ MangledTypeName ++ "_" ++
        string.int_to_string(TypeArity).

:- func addr_wrapper_decl_flags = mlds_decl_flags.

addr_wrapper_decl_flags = MLDS_DeclFlags :-
    Access = acc_private,
    PerInstance = one_copy,
    Virtuality = non_virtual,
    Finality = final,
    Constness = const,
    Abstractness = concrete,
    MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
        Virtuality, Finality, Constness, Abstractness).

%-----------------------------------------------------------------------------%
%
% Code to rename long class names.
%

:- type class_name_renaming
    --->    class_name_renaming(
                cnr_module      :: mlds_module_name,
                cnr_renaming    :: map(mlds_class_name, mlds_class_name)
            ).

    % Rename class names which are too long.  Each class results in a separate
    % `.class' file, so a long class name may exceed filesystem limits.
    % The long names tend to be automatically generated by the compiler.
    %
:- pred shorten_long_class_names(mlds_module_name::in,
    list(mlds_defn)::in, list(mlds_defn)::out) is det.

shorten_long_class_names(ModuleName, Defns0, Defns) :-
    list.map_foldl(maybe_shorten_long_class_name, Defns0, Defns1,
        map.init, RenamingMap),
    ( map.is_empty(RenamingMap) ->
        Defns = Defns1
    ;
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
            ( ClassName \= ClassName0 ->
                EntityName = entity_type(ClassName, Arity),
                !Defn ^ md_entity_name := EntityName,
                svmap.det_insert(ClassName0, ClassName, !Renaming)
            ;
                true
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
    ( string.length(MangledClassName0) < 100 ->
        ClassName = ClassName0
    ;
        % The new name must not require name mangling, as then the name may
        % again be too long.  We replace all non-alphanumeric or underscore
        % characters by underscores.  The s_ prefix avoids having f_ as the
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
    ( char.is_alnum_or_underscore(Char) ->
        Char
    ;
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
            Attributes, EnvVarNames),
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
            Attributes, EnvVarNames)
    ;
        EntityDefn0 = mlds_class(mlds_class_defn(ClassKind, Imports, Inherits,
            Implements, Ctors0, Members0)),
        list.map(rename_class_names_defn(Renaming), Ctors0, Ctors),
        list.map(rename_class_names_defn(Renaming), Members0, Members),
        EntityDefn = mlds_class(mlds_class_defn(ClassKind, Imports, Inherits,
            Implements, Ctors, Members))
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
        (
            Renaming = class_name_renaming(ModuleName, RenamingMap),
            map.search(RenamingMap, UnqualName0, UnqualName)
        ->
            Name = qual(ModuleName, QualKind, UnqualName),
            !:Type = mlds_class_type(Name, Arity, ClassKind)
        ;
            true
        )
    ;
        !.Type = mlds_array_type(Type0),
        rename_class_names_type(Renaming, Type0, Type),
        !:Type = mlds_array_type(Type)
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
        !.Stmt = ml_stmt_while(Rval0, Statement0, AtLeastOnce),
        rename_class_names_rval(Renaming, Rval0, Rval),
        rename_class_names_statement(Renaming, Statement0, Statement),
        !:Stmt = ml_stmt_while(Rval, Statement, AtLeastOnce)
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
            CallKind),
        Signature0 = mlds_func_signature(ArgTypes0, RetTypes0),
        list.map(rename_class_names_type(Renaming), ArgTypes0, ArgTypes),
        list.map(rename_class_names_type(Renaming), RetTypes0, RetTypes),
        Signature = mlds_func_signature(ArgTypes, RetTypes),
        rename_class_names_rval(Renaming, Rval0, Rval),
        list.map(rename_class_names_rval(Renaming), Rvals0, Rvals),
        list.map(rename_class_names_lval(Renaming), RetLvals0, RetLvals),
        !:Stmt = ml_stmt_call(Signature, Rval, MaybeThis, Rvals, RetLvals,
            CallKind)
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
        !.Statement = new_object(TargetLval0, MaybeTag, HasSecTag, Type0,
            MaybeSize, MaybeCtorName, Args0, ArgTypes0, MayUseAtomic),
        rename_class_names_lval(Renaming, TargetLval0, TargetLval),
        rename_class_names_type(Renaming, Type0, Type),
        list.map(rename_class_names_rval(Renaming), Args0, Args),
        list.map(rename_class_names_type(Renaming), ArgTypes0, ArgTypes),
        !:Statement = new_object(TargetLval, MaybeTag, HasSecTag, Type,
            MaybeSize, MaybeCtorName, Args, ArgTypes, MayUseAtomic)
    ;
        ( !.Statement = comment(_)
        ; !.Statement = gc_check
        ; !.Statement = mark_hp(_)
        ; !.Statement = restore_hp(_)
        ; !.Statement = trail_op(_)
        ; !.Statement = inline_target_code(_, _)
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

%-----------------------------------------------------------------------------%
%
% Code to output calls to module initialisers.
%

:- pred output_inits(int::in, module_info::in, list(string)::in,
    io::di, io::uo) is det.

output_inits(Indent, _ModuleInfo, InitPreds, !IO) :-
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

:- pred output_finals(indent::in, module_info::in, list(string)::in,
    io::di, io::uo) is det.

output_finals(Indent, _ModuleInfo, FinalPreds, !IO) :-
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
    io.write_string("java.lang.System.getenv(\"", !IO),
    io.write_string(EnvVarName, !IO),
    io.write_string("\") == null ? 0 : 1;\n", !IO).

%-----------------------------------------------------------------------------%
%
% Code to output the start and end of a source file.
%

:- pred output_src_start(indent::in, mercury_module_name::in,
    mlds_imports::in, list(foreign_decl_code)::in, list(mlds_defn)::in,
    io::di, io::uo) is det.

output_src_start(Indent, MercuryModuleName, Imports, ForeignDecls, Defns,
        !IO) :-
    output_auto_gen_comment(MercuryModuleName, !IO),
    indent_line(Indent, !IO),
    io.write_string("/* :- module ", !IO),
    prog_out.write_sym_name(MercuryModuleName, !IO),
    io.write_string(". */\n\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("package jmercury;\n", !IO),

    output_imports(Imports, !IO),
    io.write_list(ForeignDecls, "\n", output_java_decl(Indent), !IO),
    io.write_string("public class ", !IO),
    mangle_sym_name_for_java(MercuryModuleName, module_qual, "__", ClassName),
    io.write_string(ClassName, !IO),
    io.write_string(" {\n", !IO),

    output_debug_class_init(MercuryModuleName, "start", !IO),

    % Check if this module contains a `main' predicate and if it does insert
    % a `main' method in the resulting Java class that calls the `main'
    % predicate.
    ( defns_contain_main(Defns) ->
        write_main_driver(Indent + 1, ClassName, !IO)
    ;
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
        "benchmarking.ML_initialise();",
        "try {",
        "   " ++ ClassName ++ ".main_2_p_0();",
        "   jmercury.runtime.JavaInternal.run_finalisers();",
        "} catch (jmercury.runtime.Exception e) {",
        "   exception.ML_report_uncaught_exception(",
        "       (univ.Univ_0) e.exception);",
        "   if (System.getenv(""MERCURY_SUPPRESS_STACK_TRACE"") == null) {",
        "       e.printStackTrace(System.err);",
        "   }",
        "   if (jmercury.runtime.JavaInternal.exit_status == 0) {",
        "       jmercury.runtime.JavaInternal.exit_status = 1;",
        "   }",
        "}",
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
:- pred output_auto_gen_comment(mercury_module_name::in, io::di, io::uo)
    is det.

output_auto_gen_comment(ModuleName, !IO)  :-
    library.version(Version),
    module_name_to_file_name(ModuleName, ".m", do_not_create_dirs,
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

    ;       alloc_only.
            % When writing out RTTI structure definitions, initialise members
            % with allocated top-level structures but don't fill in the fields
            % yet.

:- pred output_defns(indent::in, module_info::in, mlds_module_name::in,
    output_aux::in, list(mlds_defn)::in, io::di, io::uo) is det.

output_defns(Indent, ModuleInfo, ModuleName, OutputAux, Defns, !IO) :-
    OutputDefn = output_defn(Indent, ModuleInfo, ModuleName, OutputAux),
    list.foldl(OutputDefn, Defns, !IO).

:- pred output_defn(indent::in, module_info::in, mlds_module_name::in,
    output_aux::in, mlds_defn::in, io::di, io::uo) is det.

output_defn(Indent, ModuleInfo, ModuleName, OutputAux, Defn, !IO) :-
    Defn = mlds_defn(Name, Context, Flags, DefnBody),
    indent_line(Context, Indent, !IO),
    ( DefnBody = mlds_function(_, _, body_external, _, _) ->
        % This is just a function declaration, with no body.
        % Java doesn't support separate declarations and definitions,
        % so just output the declaration as a comment.
        % (Note that the actual definition of an external procedure
        % must be given in `pragma java_code' in the same module.)
        io.write_string("/* external:\n", !IO),
        output_decl_flags(Flags, !IO),
        output_defn_body(Indent, ModuleInfo,
            qual(ModuleName, module_qual, Name), OutputAux, Context, DefnBody,
            !IO),
        io.write_string("*/\n", !IO)
    ;
        output_decl_flags(Flags, !IO),
        output_defn_body(Indent, ModuleInfo,
            qual(ModuleName, module_qual, Name), OutputAux, Context, DefnBody,
            !IO)
    ).

:- pred output_defn_body(indent::in, module_info::in,
    mlds_qualified_entity_name::in, output_aux::in,
    mlds_context::in, mlds_entity_defn::in, io::di, io::uo) is det.

output_defn_body(_, ModuleInfo, Name, OutputAux, _,
        mlds_data(Type, Initializer, _), !IO) :-
    output_data_defn(ModuleInfo, Name, OutputAux, Type, Initializer, !IO).
output_defn_body(Indent, ModuleInfo, Name, OutputAux, Context,
        mlds_function(MaybePredProcId, Signature, MaybeBody,
            _Attributes, _EnvVarNames), !IO) :-
    output_maybe(MaybePredProcId, output_pred_proc_id, !IO),
    output_func(Indent, ModuleInfo, Name, OutputAux, Context,
        Signature, MaybeBody, !IO).
output_defn_body(Indent, ModuleInfo, Name, _, Context,
        mlds_class(ClassDefn), !IO) :-
    output_class(Indent, ModuleInfo, Name, Context, ClassDefn, !IO).

%-----------------------------------------------------------------------------%
%
% Code to output classes.
%

:- pred output_class(indent::in, module_info::in,
    mlds_qualified_entity_name::in, mlds_context::in, mlds_class_defn::in,
    io::di, io::uo) is det.

output_class(Indent, ModuleInfo, Name, _Context, ClassDefn, !IO) :-
    Name = qual(ModuleName, _QualKind, UnqualName),
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
        Ctors, AllMembers),
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
    ),
    output_class_name_and_arity(ClassName, Arity, !IO),
    io.nl(!IO),
    output_extends_list(Indent + 1, BaseClasses, !IO),
    output_implements_list(Indent + 1, Implements, !IO),
    indent_line(Indent, !IO),
    io.write_string("{\n", !IO),
    output_class_body(Indent + 1, ModuleInfo, Kind, Name, AllMembers,
        ModuleName, !IO),
    io.nl(!IO),
    output_defns(Indent + 1, ModuleInfo, ModuleName, cname(UnqualName), Ctors,
        !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n\n", !IO).

    % Output superclass that this class extends. Java does not support
    % multiple inheritance, so more than one superclass is an error.
    %
:- pred output_extends_list(indent::in, list(mlds_class_id)::in,
    io::di, io::uo) is det.

output_extends_list(_, [], !IO).
output_extends_list(Indent, [SuperClass], !IO) :-
    indent_line(Indent, !IO),
    io.write_string("extends ", !IO),
    output_type(normal_style, SuperClass, !IO),
    io.nl(!IO).
output_extends_list(_, [_, _ | _], _, _) :-
    unexpected(this_file,
        "output_extends_list: multiple inheritance not supported in Java").

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
    (
        Interface = mlds_class_type(qual(ModuleQualifier, QualKind, Name),
            Arity, _)
    ->
        SymName = mlds_module_name_to_sym_name(ModuleQualifier),
        mangle_sym_name_for_java(SymName, convert_qual_kind(QualKind),
            ".", ModuleName),
        io.format("%s.%s", [s(ModuleName), s(Name)], !IO),
        %
        % Check if the interface is one of the ones in the runtime
        % system.  If it is we don't need to output the arity.
        %
        ( interface_is_special(Name) ->
            true
        ;
            io.format("%d", [i(Arity)], !IO)
        )
    ;
        unexpected(this_file, "output_interface: interface was not a class.")
    ).

:- pred output_class_body(indent::in, module_info::in, mlds_class_kind::in,
    mlds_qualified_entity_name::in, list(mlds_defn)::in,
    mlds_module_name::in, io::di, io::uo) is det.

output_class_body(Indent, ModuleInfo, mlds_class, _, AllMembers, ModuleName,
        !IO) :-
    output_defns(Indent, ModuleInfo, ModuleName, none, AllMembers, !IO).

output_class_body(_Indent, _, mlds_package, _Name, _AllMembers, _, _, _) :-
    unexpected(this_file, "cannot use package as a type.").

output_class_body(Indent, ModuleInfo, mlds_interface, _, AllMembers,
        ModuleName, !IO) :-
    output_defns(Indent, ModuleInfo, ModuleName, none, AllMembers, !IO).

output_class_body(_Indent, _, mlds_struct, _, _AllMembers, _, _, _) :-
    unexpected(this_file, "output_class_body: structs not supported in Java.").

output_class_body(Indent, ModuleInfo, mlds_enum, Name, AllMembers, _, !IO) :-
    list.filter(defn_is_const, AllMembers, EnumConsts),
    Name = qual(ModuleName, _QualKind, UnqualName),
    output_enum_constants(Indent + 1, ModuleInfo, ModuleName, UnqualName,
        EnumConsts, !IO),
    io.nl(!IO),
    output_enum_ctor(Indent + 1, UnqualName, !IO).

%-----------------------------------------------------------------------------%
%
% Additional code for generating enumerations
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
    output_name(UnqualName, !IO),
    io.write_string("(int val) {\n", !IO),
    indent_line(Indent + 1, !IO),
    % Call the MercuryEnum constructor, which will set the MR_value field.
    io.write_string("super(val);\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_enum_constants(indent::in, module_info::in,
    mlds_module_name::in, mlds_entity_name::in, list(mlds_defn)::in,
    io::di, io::uo) is det.

output_enum_constants(Indent, ModuleInfo, EnumModuleName, EnumName,
        EnumConsts, !IO) :-
    io.write_list(EnumConsts, "\n",
        output_enum_constant(Indent, ModuleInfo, EnumModuleName, EnumName),
        !IO),
    io.nl(!IO).

:- pred output_enum_constant(indent::in, module_info::in,
    mlds_module_name::in, mlds_entity_name::in, mlds_defn::in,
    io::di, io::uo) is det.

output_enum_constant(Indent, ModuleInfo, EnumModuleName, EnumName, Defn,
        !IO) :-
    Defn = mlds_defn(Name, _Context, _Flags, DefnBody),
    (
        DefnBody = mlds_data(_Type, Initializer, _GCStatement)
    ->
        % Make a static instance of the constant.  The MLDS doesn't retain enum
        % constructor names so it's easier to derive the name of the constant
        % later by naming them after the integer values.
        % XXX this will break with `:- pragma foreign_enum'
        indent_line(Indent, !IO),
        io.write_string("public static final ", !IO),
        output_name(EnumName, !IO),
        io.write_string(" K", !IO),
        output_initializer_body(ModuleInfo, Initializer, no, EnumModuleName,
            !IO),
        io.write_string(" = new ", !IO),
        output_name(EnumName, !IO),
        io.write_string("(", !IO),
        output_initializer_body(ModuleInfo, Initializer, no, EnumModuleName,
            !IO),
        io.write_string(");", !IO),

        io.write_string(" /* ", !IO),
        output_name(Name, !IO),
        io.write_string(" */", !IO)
    ;
        unexpected(this_file,
            "output_enum_constant: definition body was not data.")
    ).

%-----------------------------------------------------------------------------%
%
% Code to output data declarations/definitions
%

:- pred output_data_decl(mlds_qualified_entity_name::in, mlds_type::in,
    io::di, io::uo) is det.

output_data_decl(qual(_, _, Name), Type, !IO) :-
    output_type(normal_style, Type, !IO),
    io.write_char(' ', !IO),
    output_name(Name, !IO).

:- pred output_data_defn(module_info::in, mlds_qualified_entity_name::in,
    output_aux::in, mlds_type::in, mlds_initializer::in, io::di, io::uo) is det.

output_data_defn(ModuleInfo, Name, OutputAux, Type, Initializer, !IO) :-
    output_data_decl(Name, Type, !IO),
    output_initializer(ModuleInfo, Name ^ mod_name, OutputAux, Type,
        Initializer, !IO),
    io.write_string(";\n", !IO).

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
            ; CtorCat = ctor_cat_builtin(cat_builtin_char)
            ; CtorCat = ctor_cat_builtin(cat_builtin_float)
            ),
            Initializer = "0"
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
        ; Type = mlds_native_float_type
        ; Type = mlds_native_char_type
        ),
        Initializer = "0"
    ;
        Type = mlds_native_bool_type,
        Initializer = "false"
    ;
        ( Type = mlds_mercury_array_type(_)
        ; Type = mlds_cont_type(_)
        ; Type = mlds_commit_type
        ; Type = mlds_foreign_type(_)
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

:- pred output_initializer(module_info::in, mlds_module_name::in,
    output_aux::in, mlds_type::in, mlds_initializer::in, io::di, io::uo)
    is det.

output_initializer(ModuleInfo, ModuleName, OutputAux, Type, Initializer, !IO) :-
    io.write_string(" = ", !IO),
    NeedsInit = needs_initialization(Initializer),
    (
        NeedsInit = yes,
        % Due to cyclic references, we need to separate the allocation and
        % initialisation steps of RTTI structures.  If InitStyle is alloc_only
        % then we output an initializer to allocate a structure without filling
        % in the fields.
        (
            ( OutputAux = none
            ; OutputAux = cname(_)
            ),
            output_initializer_body(ModuleInfo, Initializer, yes(Type),
                ModuleName, !IO)
        ;
            OutputAux = alloc_only,
            output_initializer_alloc_only(ModuleInfo, Initializer, yes(Type),
                ModuleName, !IO)
        )
    ;
        NeedsInit = no,
        % If we are not provided with an initializer we just, supply the
        % default java values -- note: this is strictly only necessary for
        % local variables, but it's not going to hurt anything else.
        %
        io.write_string(get_java_type_initializer(Type), !IO)
    ).

:- func needs_initialization(mlds_initializer) = bool.

needs_initialization(no_initializer) = no.
needs_initialization(init_obj(_)) = yes.
needs_initialization(init_struct(_, _)) = yes.
needs_initialization(init_array(_)) = yes.

:- pred output_initializer_alloc_only(module_info::in, mlds_initializer::in,
    maybe(mlds_type)::in, mlds_module_name::in, io::di, io::uo) is det.

output_initializer_alloc_only(_ModuleInfo, Initializer, MaybeType, _ModuleName,
        !IO) :-
    (
        Initializer = no_initializer,
        unexpected(this_file, "output_initializer_alloc_only: no_initializer")
    ;
        Initializer = init_obj(_),
        unexpected(this_file, "output_initializer_alloc_only: init_obj")
    ;
        Initializer = init_struct(StructType, _FieldInits),
        io.write_string("new ", !IO),
        output_type(normal_style, StructType, !IO),
        io.write_string("()", !IO)
    ;
        Initializer = init_array(ElementInits),
        Size = list.length(ElementInits),
        io.write_string("new ", !IO),
        (
            MaybeType = yes(Type),
            output_type(sized_array(Size), Type, !IO)
        ;
            MaybeType = no,
            % XXX we need to know the type here
            io.write_string("/* XXX init_array */ Object", !IO),
            output_array_brackets(sized_array(Size), !IO)
        )
    ).

:- pred output_initializer_body(module_info::in, mlds_initializer::in,
    maybe(mlds_type)::in, mlds_module_name::in, io::di, io::uo) is det.

output_initializer_body(_ModuleInfo, no_initializer, _, _, _, _) :-
    unexpected(this_file, "output_initializer_body: no_initializer").
output_initializer_body(ModuleInfo, init_obj(Rval), MaybeType, ModuleName,
        !IO) :-
    (
        MaybeType = yes(Type),
        type_is_object(Type),
        rval_is_int_const(Rval)
    ->
        % If it is a enumeration object make a reference to a static instance.
        output_type(normal_style, Type, !IO),
        io.write_string(".K", !IO),
        output_rval_maybe_with_enum(ModuleInfo, Rval, ModuleName, !IO)
    ;
        MaybeType = yes(Type)
    ->
        % If it is an non-enumeration object, insert appropriate cast.
        % XXX The logic of this is a bit wrong. Fixing it would eliminate
        % some of the unecessary casting that happens.
        io.write_string("(", !IO),
        output_type(normal_style, Type, !IO),
        io.write_string(") ", !IO),
        output_rval(ModuleInfo, Rval, ModuleName, !IO)
    ;
        output_rval_maybe_with_enum(ModuleInfo, Rval, ModuleName, !IO)
    ).

output_initializer_body(ModuleInfo, init_struct(StructType, FieldInits),
        _MaybeType, ModuleName, !IO) :-
    io.write_string("new ", !IO),
    output_type(normal_style, StructType, !IO),
    IsArray = type_is_array(StructType),
    io.write_string(if IsArray = is_array then " {" else "(", !IO),
    output_initializer_body_list(ModuleInfo, ModuleName, FieldInits, !IO),
    io.write_char(if IsArray = is_array then '}' else ')', !IO).
output_initializer_body(ModuleInfo, init_array(ElementInits), MaybeType,
        ModuleName, !IO) :-
    io.write_string("new ", !IO),
    (
        MaybeType = yes(Type),
        output_type(normal_style, Type, !IO)
    ;
        MaybeType = no,
        % XXX we need to know the type here
        io.write_string("/* XXX init_array */ Object[]", !IO)
    ),
    io.write_string(" {\n\t\t", !IO),
    output_initializer_body_list(ModuleInfo, ModuleName, ElementInits, !IO),
    io.write_string("}", !IO).

:- pred output_initializer_body_list(module_info::in, mlds_module_name::in,
    list(mlds_initializer)::in, io::di, io::uo) is det.

output_initializer_body_list(ModuleInfo, ModuleName, Inits, !IO) :-
    io.write_list(Inits, ",\n\t\t",
        (pred(Init::in, !.IO::di, !:IO::uo) is det :-
            output_initializer_body(ModuleInfo, Init, no, ModuleName, !IO)),
        !IO).

%-----------------------------------------------------------------------------%
%
% Code to output RTTI data assignments
%

:- pred output_rtti_assignments(indent::in, module_info::in,
    mlds_module_name::in, list(mlds_defn)::in, io::di, io::uo) is det.

output_rtti_assignments(Indent, ModuleInfo, ModuleName, Defns, !IO) :-
    (
        Defns = []
    ;
        Defns = [_ | _],
        OrderedDefns = order_mlds_rtti_defns(Defns),
        indent_line(Indent, !IO),
        io.write_string("static {\n", !IO),
        list.foldl(
            output_rtti_defns_assignments(Indent + 1, ModuleInfo, ModuleName),
            OrderedDefns, !IO),
        indent_line(Indent, !IO),
        io.write_string("}\n", !IO)
    ).

:- pred output_rtti_defns_assignments(indent::in, module_info::in,
    mlds_module_name::in, list(mlds_defn)::in, io::di, io::uo) is det.

output_rtti_defns_assignments(Indent, ModuleInfo, ModuleName, Defns, !IO) :-
    % Separate cliques.
    indent_line(Indent, !IO),
    io.write_string("//\n", !IO),
    list.foldl(
        output_rtti_defn_assignments(Indent, ModuleInfo, ModuleName),
        Defns, !IO).

:- pred output_rtti_defn_assignments(indent::in, module_info::in,
    mlds_module_name::in, mlds_defn::in, io::di, io::uo) is det.

output_rtti_defn_assignments(Indent, ModuleInfo, ModuleName, Defn, !IO) :-
    Defn = mlds_defn(Name, _Context, _Flags, DefnBody),
    (
        DefnBody = mlds_data(Type, Initializer, _),
        output_rtti_defn_assignments_2(Indent, ModuleInfo, ModuleName, Name,
            Type, Initializer, !IO)
    ;
        ( DefnBody = mlds_function(_, _, _, _, _)
        ; DefnBody = mlds_class(_)
        ),
        unexpected(this_file,
            "output_rtti_defn_assignments: expected mlds_data")
    ).

:- pred output_rtti_defn_assignments_2(indent::in, module_info::in,
    mlds_module_name::in, mlds_entity_name::in, mlds_type::in,
    mlds_initializer::in, io::di, io::uo) is det.

output_rtti_defn_assignments_2(Indent, ModuleInfo, ModuleName, Name, _Type,
        Initializer, !IO) :-
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
            output_initializer_body_list(ModuleInfo, ModuleName, FieldInits,
                !IO),
            io.write_string(");\n", !IO)
        ;
            IsArray = is_array,
            % Not encountered in practice.
            unexpected(this_file, "output_rtti_defn_assignments_2: is_array")
        )
    ;
        Initializer = init_array(ElementInits),
        list.foldl2(output_rtti_array_assignments(Indent, ModuleInfo,
            ModuleName, Name), ElementInits, 0, _Index, !IO)
    ).

:- pred output_rtti_array_assignments(indent::in, module_info::in,
    mlds_module_name::in, mlds_entity_name::in,
    mlds_initializer::in, int::in, int::out, io::di, io::uo) is det.

output_rtti_array_assignments(Indent, ModuleInfo, ModuleName, Name,
        ElementInit, Index, Index + 1, !IO) :-
    indent_line(Indent, !IO),
    output_name(Name, !IO),
    io.write_string("[", !IO),
    io.write_int(Index, !IO),
    io.write_string("] = ", !IO),
    output_initializer_body(ModuleInfo, ElementInit, no, ModuleName, !IO),
    io.write_string(";\n", !IO).

%-----------------------------------------------------------------------------%
%
% Code to output function declarations/definitions
%

:- pred output_pred_proc_id(pred_proc_id::in, io::di, io::uo) is det.

output_pred_proc_id(proc(PredId, ProcId), !IO) :-
    globals.io_lookup_bool_option(auto_comments, AddComments, !IO),
    (
        AddComments = yes,
        io.write_string("// pred_id: ", !IO),
        pred_id_to_int(PredId, PredIdNum),
        io.write_int(PredIdNum, !IO),
        io.write_string(", proc_id: ", !IO),
        proc_id_to_int(ProcId, ProcIdNum),
        io.write_int(ProcIdNum, !IO),
        io.nl(!IO)
    ;
        AddComments = no
    ).

:- pred output_func(indent::in, module_info::in,
    mlds_qualified_entity_name::in, output_aux::in, mlds_context::in,
    mlds_func_params::in, mlds_function_body::in, io::di, io::uo) is det.

output_func(Indent, ModuleInfo, Name, OutputAux, Context, Signature, MaybeBody,
        !IO) :-
    (
        MaybeBody = body_defined_here(Body),
        output_func_decl(Indent, Name, OutputAux, Context, Signature, !IO),
        io.write_string("\n", !IO),
        indent_line(Context, Indent, !IO),
        io.write_string("{\n", !IO),
        FuncInfo = func_info(Name, Signature),
        output_statement(Indent + 1, ModuleInfo, FuncInfo, Body, _ExitMethods,
            !IO),
        indent_line(Context, Indent, !IO),
        io.write_string("}\n", !IO)    % end the function
    ;
        MaybeBody = body_external
    ).

:- pred output_func_decl(indent::in, mlds_qualified_entity_name::in,
    output_aux::in, mlds_context::in, mlds_func_params::in, io::di, io::uo)
    is det.

output_func_decl(Indent, QualifiedName, cname(CtorName), Context, Signature,
        !IO) :-
    Signature = mlds_func_params(Parameters, _RetTypes),
    output_name(CtorName, !IO),
    output_params(Indent, QualifiedName ^ mod_name, Context, Parameters, !IO).

output_func_decl(Indent, QualifiedName, OutputAux, Context, Signature, !IO) :-
    ( OutputAux = none
    ; OutputAux = alloc_only
    ),
    Signature = mlds_func_params(Parameters, RetTypes),
    (
        RetTypes = [],
        io.write_string("void", !IO)
    ;
        RetTypes = [RetType],
        output_type(normal_style, RetType, !IO)
    ;
        RetTypes = [_, _ | _],
        % For multiple outputs, we return an array of objects.
        io.write_string("java.lang.Object []", !IO)
    ),
    io.write_char(' ', !IO),
    QualifiedName = qual(ModuleName, _QualKind, Name),
    output_name(Name, !IO),
    output_params(Indent, ModuleName, Context, Parameters, !IO).

:- pred output_params(indent::in, mlds_module_name::in, mlds_context::in,
    mlds_arguments::in, io::di, io::uo) is det.

output_params(Indent, ModuleName, Context, Parameters, !IO) :-
    io.write_char('(', !IO),
    (
        Parameters = []
    ;
        Parameters = [_ | _],
        io.nl(!IO),
        io.write_list(Parameters, ",\n",
            output_param(Indent + 1, ModuleName, Context), !IO)
    ),
    io.write_char(')', !IO).

:- pred output_param(indent::in, mlds_module_name::in, mlds_context::in,
    mlds_argument::in, io::di, io::uo) is det.

output_param(Indent, _ModuleName, Context, Arg, !IO) :-
    Arg = mlds_argument(Name, Type, _GCStatement),
    indent_line(Context, Indent, !IO),
    output_type(normal_style, Type, !IO),
    io.write_char(' ', !IO),
    output_name(Name, !IO).

%-----------------------------------------------------------------------------%
%
% Code to output names of various entities
% XXX Much of the code in this section will not work when we start enforcing
% names properly.

:- pred output_maybe_qualified_name(mlds_qualified_entity_name::in,
    mlds_module_name::in, io::di, io::uo) is det.

output_maybe_qualified_name(QualifiedName, CurrentModuleName, !IO) :-
    % Don't module qualify names which are defined in the current module.
    % This avoids unnecessary verbosity, and is also necessary in the case
    % of local variables and function parameters, which must not be qualified.
    QualifiedName = qual(ModuleName, _QualKind, Name),
    ( ModuleName = CurrentModuleName ->
        output_name(Name, !IO)
    ;
        output_fully_qualified_thing(QualifiedName, output_name, !IO)
    ).

:- pred output_fully_qualified_name(mlds_qualified_entity_name::in,
    io::di, io::uo) is det.

output_fully_qualified_name(QualifiedName, !IO) :-
    output_fully_qualified_thing(QualifiedName, output_name, !IO).

:- pred output_fully_qualified_proc_label(mlds_qualified_proc_label::in,
    io::di, io::uo) is det.

output_fully_qualified_proc_label(QualifiedName, !IO) :-
    output_fully_qualified_thing(QualifiedName, mlds_output_proc_label, !IO).

:- pred output_fully_qualified_thing(mlds_fully_qualified_name(T)::in,
    pred(T, io, io)::pred(in, di, uo) is det, io::di, io::uo) is det.

output_fully_qualified_thing(qual(MLDS_ModuleName, QualKind, Name), OutputFunc,
        !IO) :-
    % XXX These functions are named wrongly for Java.
    mlds_module_name_to_package_name(MLDS_ModuleName) = OuterName,
    mlds_module_name_to_sym_name(MLDS_ModuleName) = InnerName,

    % Write the part of the qualifier that corresponds to a top-level Java
    % class.
    mangle_sym_name_for_java(OuterName, module_qual, "__", MangledOuterName),
    io.write_string(MangledOuterName, !IO),

    % Write the later parts of the qualifier correspond to nested Java classes.
    ( OuterName = InnerName ->
        true
    ;
        io.write_string(".", !IO),
        remove_sym_name_prefixes(InnerName, OuterName, Suffix),
        mangle_sym_name_for_java(Suffix, convert_qual_kind(QualKind), ".",
            MangledSuffix),
        io.write_string(MangledSuffix, !IO)
    ),

    % Write the qualified thing.
    io.write_string(".", !IO),
    OutputFunc(Name, !IO).

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

:- func convert_qual_kind(mlds_qual_kind) = java_qual_kind.

convert_qual_kind(module_qual) = module_qual.
convert_qual_kind(type_qual) = type_qual.

:- pred output_module_name(mercury_module_name::in, io::di, io::uo) is det.

output_module_name(ModuleName, !IO) :-
    io.write_string(sym_name_mangle(ModuleName), !IO).

:- pred output_class_name_and_arity(mlds_class_name::in, arity::in,
    io::di, io::uo) is det.

output_class_name_and_arity(Name, Arity, !IO) :-
    output_class_name(Name, !IO),
    io.format("_%d", [i(Arity)], !IO).

:- pred output_class_name(mlds_class_name::in, io::di, io::uo) is det.

output_class_name(Name, !IO) :-
    MangledName = name_mangle_no_leading_digit(Name),
    % By convention, class names should start with a capital letter.
    UppercaseMangledName = flip_initial_case(MangledName),
    io.write_string(UppercaseMangledName, !IO).

:- pred output_name(mlds_entity_name::in, io::di, io::uo) is det.

output_name(entity_type(Name, Arity), !IO) :-
    output_class_name_and_arity(Name, Arity, !IO).
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
output_data_name(mlds_scalar_common_ref(_), !IO) :-
    unexpected(this_file, "NYI: mlds_scalar_common_ref").
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
% Code to output types
%

:- type output_style
    --->    normal_style
    ;       sized_array(int).
            % If writing an array type, include the integer within the
            % square brackets.

:- pred output_type(output_style::in, mlds_type::in, io::di, io::uo) is det.

output_type(Style, mercury_type(Type, CtorCat, _), !IO) :-
    (
        % We need to handle type_info (etc.) types specially -- they get mapped
        % to types in the runtime rather than in private_builtin.
        hand_defined_type(CtorCat, SubstituteName)
    ->
        io.write_string(SubstituteName, !IO)
    ;
        % io.state and store.store
        CtorCat = ctor_cat_builtin_dummy
    ->
        io.write_string("/* builtin_dummy */ java.lang.Object", !IO)
    ;
        Type = c_pointer_type
    ->
        % The c_pointer type is used in the c back-end as a generic way
        % to pass foreign types to automatically generated Compare and Unify
        % code. When compiling to Java we must instead use java.lang.Object.
        io.write_string("/* c_pointer */ java.lang.Object", !IO)
    ;
        output_mercury_type(Style, Type, CtorCat, !IO)
    ).

output_type(Style, mlds_mercury_array_type(ElementType), !IO) :-
    ( ElementType = mercury_type(_, ctor_cat_variable, _) ->
        % We can't use `java.lang.Object []', since we want a generic type
        % that is capable of holding any kind of array, including e.g.
        % `int []'. Java doesn't have any equivalent of .NET's System.Array
        % class, so we just use the universal base `java.lang.Object'.
        io.write_string("/* Array */ java.lang.Object", !IO)
    ;
        % For primitive element types we use arrays of the primitive type.
        % For non-primitive element types, we just use `java.lang.Object []'.
        % We used to use more specific types, but then to create an array of
        % the right type we need to use reflection to determine the class of a
        % representative element.  That doesn't work if the representative
        % element is of a foreign type, and has the value null.
        ( java_builtin_type(ElementType, _, _, _) ->
            output_type(Style, ElementType, !IO)
        ;
            io.write_string("/* ", !IO),
            output_type(Style, ElementType, !IO),
            io.write_string("[] */ java.lang.Object", !IO)
        ),
        output_array_brackets(Style, !IO)
    ).
output_type(_, mlds_native_int_type, !IO) :-
    io.write_string("int", !IO).
output_type(_, mlds_native_float_type, !IO) :-
    io.write_string("double", !IO).
output_type(_, mlds_native_bool_type, !IO) :-
    io.write_string("boolean", !IO).
output_type(_, mlds_native_char_type, !IO)  :-
    io.write_string("char", !IO).
output_type(_, mlds_foreign_type(ForeignType), !IO) :-
    (
        ForeignType = java(java_type(Name)),
        maybe_output_comment("foreign_type", !IO),
        io.write_string(Name, !IO)
    ;
        ForeignType = c(_),
        unexpected(this_file, "output_type: c foreign_type")
    ;
        ForeignType = il(_),
        unexpected(this_file, "output_type: il foreign_type")
    ;
        ForeignType = erlang(_),
        unexpected(this_file, "output_type: erlang foreign_type")
    ).
output_type(_, mlds_class_type(Name, Arity, _ClassKind), !IO) :-
    (
        Name = qual(ModuleName, _, ClassName),
        SymName = mlds_module_name_to_sym_name(ModuleName),
        SymName = mercury_runtime_package_name
    ->
        % Don't mangle runtime class names.
        io.write_string("jmercury.runtime.", !IO),
        io.write_string(ClassName, !IO)
    ;
        % We used to treat enumerations specially here, outputting
        % them as "int", but now we do the same for all classes.
        output_fully_qualified_thing(Name, output_class_name, !IO),
        io.format("_%d", [i(Arity)], !IO)
    ).
output_type(Style, mlds_ptr_type(Type), !IO) :-
    % XXX should we report an error here, if the type pointed to
    % is not a class type?
    output_type(Style, Type, !IO).
output_type(Style, mlds_array_type(Type), !IO) :-
    output_type(Style, Type, !IO),
    output_array_brackets(Style, !IO).
output_type(_, mlds_func_type(_FuncParams), !IO) :-
    io.write_string("jmercury.runtime.MethodPtr", !IO).
output_type(_, mlds_generic_type, !IO) :-
    io.write_string("java.lang.Object", !IO).
output_type(_, mlds_generic_env_ptr_type, !IO) :-
    io.write_string("/* env_ptr */ java.lang.Object", !IO).
output_type(_, mlds_type_info_type, !IO) :-
    io.write_string("jmercury.runtime.TypeInfo", !IO).
output_type(_, mlds_pseudo_type_info_type, !IO) :-
    io.write_string("jmercury.runtime.PseudoTypeInfo", !IO).
output_type(_, mlds_cont_type(_), !IO) :-
    % XXX Should this actually be a class that extends MethodPtr?
    io.write_string("jmercury.runtime.MethodPtr", !IO).
output_type(_, mlds_commit_type, !IO) :-
    io.write_string("jmercury.runtime.Commit", !IO).
output_type(Style, mlds_rtti_type(RttiIdMaybeElement), !IO) :-
    rtti_id_maybe_element_java_type(RttiIdMaybeElement, JavaTypeName, IsArray),
    io.write_string(JavaTypeName, !IO),
    (
        IsArray = is_array,
        output_array_brackets(Style, !IO)
    ;
        IsArray = not_array
    ).
output_type(Style, mlds_tabling_type(TablingId), !IO) :-
    tabling_id_java_type(TablingId, JavaTypeName, IsArray),
    io.write_string(JavaTypeName, !IO),
    (
        IsArray = is_array,
        output_array_brackets(Style, !IO)
    ;
        IsArray = not_array
    ).
output_type(_, mlds_unknown_type, !IO) :-
    unexpected(this_file, "output_type: unknown type").

:- pred output_mercury_type(output_style::in, mer_type::in,
    type_ctor_category::in, io::di, io::uo) is det.

output_mercury_type(Style, Type, CtorCat, !IO) :-
    (
        CtorCat = ctor_cat_builtin(cat_builtin_char),
        io.write_string("char", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int),
        io.write_string("int", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        io.write_string("java.lang.String", !IO)
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        io.write_string("double", !IO)
    ;
        CtorCat = ctor_cat_void,
        io.write_string("builtin.Void_0", !IO)
    ;
        CtorCat = ctor_cat_variable,
        io.write_string("java.lang.Object", !IO)
    ;
        CtorCat = ctor_cat_tuple,
        io.write_string("/* tuple */ java.lang.Object", !IO),
        output_array_brackets(Style, !IO)
    ;
        CtorCat = ctor_cat_higher_order,
        io.write_string("/* closure */ java.lang.Object", !IO),
        output_array_brackets(Style, !IO)
    ;
        CtorCat = ctor_cat_system(_),
        output_mercury_user_type(Style, Type, ctor_cat_user(cat_user_general),
            !IO)
    ;
        ( CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ),
        output_mercury_user_type(Style, Type, CtorCat, !IO)
    ).

:- pred output_mercury_user_type(output_style::in, mer_type::in,
    type_ctor_category::in, io::di, io::uo) is det.

output_mercury_user_type(Style, Type, CtorCat, !IO) :-
    ( type_to_ctor_and_args(Type, TypeCtor, _ArgsTypes) ->
        ml_gen_type_name(TypeCtor, ClassName, ClassArity),
        ( CtorCat = ctor_cat_enum(_) ->
            MLDS_Type = mlds_class_type(ClassName, ClassArity, mlds_enum)
        ;
            MLDS_Type = mlds_class_type(ClassName, ClassArity, mlds_class)
        ),
        output_type(Style, MLDS_Type, !IO)
    ;
        unexpected(this_file, "output_mercury_user_type: not a user type")
    ).

:- pred output_array_brackets(output_style::in, io::di, io::uo) is det.

output_array_brackets(Style, !IO) :-
    io.write_string("[", !IO),
    (
        Style = normal_style
    ;
        Style = sized_array(Size),
        io.write_int(Size, !IO)
    ),
    io.write_string("]", !IO).

    % Return is_array if the corresponding Java type is an array type.
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
        rtti_id_maybe_element_java_type(RttiIdMaybeElement,
            _JavaTypeName, IsArray)
    ;
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

    % hand_defined_type(Type, SubstituteName):
    %
    % We need to handle type_info (etc.) types specially -- they get mapped
    % to types in the runtime rather than in private_builtin.
    %
:- pred hand_defined_type(type_ctor_category::in, string::out) is semidet.

hand_defined_type(ctor_cat_system(Kind), SubstituteName) :-
    (
        Kind = cat_system_type_info,
        SubstituteName = "jmercury.runtime.TypeInfo_Struct"
    ;
        Kind = cat_system_type_ctor_info,
        SubstituteName = "jmercury.runtime.TypeCtorInfo_Struct"
    ;
        Kind = cat_system_typeclass_info,
        SubstituteName = "/* typeclass_info */ java.lang.Object[]"
    ;
        Kind = cat_system_base_typeclass_info,
        SubstituteName = "/* base_typeclass_info */ java.lang.Object[]"
    ).

%-----------------------------------------------------------------------------%
%
% Code to output declaration specifiers
%

:- pred output_decl_flags(mlds_decl_flags::in, io::di, io::uo) is det.

output_decl_flags(Flags, !IO) :-
    output_access(access(Flags), !IO),
    output_per_instance(per_instance(Flags), !IO),
    output_virtuality(virtuality(Flags), !IO),
    output_finality(finality(Flags), !IO),
    output_constness(constness(Flags), !IO),
    output_abstractness(abstractness(Flags), !IO).

:- pred output_access(access::in, io::di, io::uo) is det.

output_access(acc_public, !IO) :-
    io.write_string("public ", !IO).
output_access(acc_private, !IO) :-
    io.write_string("private ", !IO).
output_access(acc_protected, !IO) :-
    io.write_string("protected ", !IO).
output_access(acc_default, !IO) :-
    maybe_output_comment("default", !IO).
output_access(acc_local, !IO).

:- pred output_per_instance(per_instance::in, io::di, io::uo) is det.

output_per_instance(per_instance, !IO).
output_per_instance(one_copy, !IO) :- io.write_string("static ", !IO).

:- pred output_virtuality(virtuality::in, io::di, io::uo) is det.

output_virtuality(virtual, !IO) :- maybe_output_comment("virtual", !IO).
output_virtuality(non_virtual, !IO).

:- pred output_finality(finality::in, io::di, io::uo) is det.

output_finality(final, !IO) :- io.write_string("final ", !IO).
output_finality(overridable, !IO).

:- pred output_constness(constness::in, io::di, io::uo) is det.

output_constness(const, !IO) :- maybe_output_comment("const", !IO).
output_constness(modifiable, !IO).

:- pred output_abstractness(abstractness::in, io::di, io::uo) is det.

output_abstractness(abstract, !IO) :- io.write_string("abstract ", !IO).
output_abstractness(concrete, !IO).

:- pred maybe_output_comment(string::in, io::di, io::uo) is det.

maybe_output_comment(Comment, !IO) :-
    globals.io_lookup_bool_option(auto_comments, AddComments, !IO),
    (
        AddComments = yes,
        io.write_string("/* ", !IO),
        io.write_string(Comment, !IO),
        io.write_string(" */", !IO)
    ;
        AddComments = no
    ).

%-----------------------------------------------------------------------------%
%
% Code to output statements
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

:- type func_info
    --->    func_info(
                func_info_name      :: mlds_qualified_entity_name,
                func_info_params    :: mlds_func_params
            ).

:- func mod_name(mlds_fully_qualified_name(T)) = mlds_module_name.

mod_name(qual(ModuleName, _, _)) = ModuleName.

:- pred output_statements(indent::in, module_info::in, func_info::in,
    list(statement)::in, exit_methods::out, io::di, io::uo) is det.

output_statements(_, _, _, [], set.make_singleton_set(can_fall_through), !IO).
output_statements(Indent, ModuleInfo, FuncInfo, [Statement | Statements],
        ExitMethods, !IO) :-
    output_statement(Indent, ModuleInfo, FuncInfo, Statement, StmtExitMethods,
        !IO),
    ( set.member(can_fall_through, StmtExitMethods) ->
        output_statements(Indent, ModuleInfo, FuncInfo, Statements,
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

:- pred output_statement(indent::in, module_info::in, func_info::in,
    statement::in, exit_methods::out, io::di, io::uo) is det.

output_statement(Indent, ModuleInfo, FuncInfo,
        statement(Statement, Context), ExitMethods, !IO) :-
    output_context(Context, !IO),
    output_stmt(Indent, ModuleInfo, FuncInfo, Statement, Context, ExitMethods,
        !IO).

:- pred output_stmt(indent::in, module_info::in, func_info::in, mlds_stmt::in,
    mlds_context::in, exit_methods::out, io::di, io::uo) is det.

    % sequence
    %
output_stmt(Indent, ModuleInfo, FuncInfo, ml_stmt_block(Defns, Statements),
        Context, ExitMethods, !IO) :-
    indent_line(Indent, !IO),
    io.write_string("{\n", !IO),
    (
        Defns = [_ | _],
        ModuleName = FuncInfo ^ func_info_name ^ mod_name,
        output_defns(Indent + 1, ModuleInfo, ModuleName, none, Defns, !IO),
        io.write_string("\n", !IO)
    ;
        Defns = []
    ),
    output_statements(Indent + 1, ModuleInfo, FuncInfo, Statements,
        ExitMethods, !IO),
    indent_line(Context, Indent, !IO),
    io.write_string("}\n", !IO).

    % iteration
    %
output_stmt(Indent, ModuleInfo, FuncInfo, ml_stmt_while(Cond, Statement, no),
        _, ExitMethods, !IO) :-
    indent_line(Indent, !IO),
    io.write_string("while (", !IO),
    output_rval(ModuleInfo, Cond, FuncInfo ^ func_info_name ^ mod_name, !IO),
    io.write_string(")\n", !IO),
    % The contained statement is reachable iff the while statement is
    % reachable and the condition expression is not a constant expression
    % whose value is false.
    ( Cond = ml_const(mlconst_false) ->
        indent_line(Indent, !IO),
        io.write_string("{  /* Unreachable code */  }\n", !IO),
        ExitMethods = set.make_singleton_set(can_fall_through)
    ;
        output_statement(Indent + 1, ModuleInfo, FuncInfo, Statement,
            StmtExitMethods, !IO),
        ExitMethods = while_exit_methods(Cond, StmtExitMethods)
    ).
output_stmt(Indent, ModuleInfo, FuncInfo, ml_stmt_while(Cond, Statement, yes),
        Context, ExitMethods, !IO) :-
    indent_line(Indent, !IO),
    io.write_string("do\n", !IO),
    output_statement(Indent + 1, ModuleInfo, FuncInfo, Statement,
        StmtExitMethods, !IO),
    indent_line(Context, Indent, !IO),
    io.write_string("while (", !IO),
    output_rval(ModuleInfo, Cond, FuncInfo ^ func_info_name ^ mod_name, !IO),
    io.write_string(");\n", !IO),
    ExitMethods = while_exit_methods(Cond, StmtExitMethods).

    % selection (if-then-else)
    %
output_stmt(Indent, ModuleInfo, FuncInfo,
        ml_stmt_if_then_else(Cond, Then0, MaybeElse),
        Context, ExitMethods, !IO) :-
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
        % we need braces around the inner `if', otherwise they wouldn't parse
        % they way we want them to: Java would match the `else' with the
        % inner `if' rather than the outer `if'.

        MaybeElse = yes(_),
        Then0 = statement(ml_stmt_if_then_else(_, _, no), ThenContext)
    ->
        Then = statement(ml_stmt_block([], [Then0]), ThenContext)
    ;
        Then = Then0
    ),

    indent_line(Indent, !IO),
    io.write_string("if (", !IO),
    output_rval(ModuleInfo, Cond, FuncInfo ^ func_info_name ^ mod_name, !IO),
    io.write_string(")\n", !IO),
    output_statement(Indent + 1, ModuleInfo, FuncInfo, Then, ThenExitMethods,
        !IO),
    (
        MaybeElse = yes(Else),
        indent_line(Context, Indent, !IO),
        io.write_string("else\n", !IO),
        output_statement(Indent + 1, ModuleInfo, FuncInfo, Else,
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
    ).

    % selection (switch)
    %
output_stmt(Indent, ModuleInfo, FuncInfo,
        ml_stmt_switch(_Type, Val, _Range, Cases, Default),
        Context, ExitMethods, !IO) :-
    indent_line(Context, Indent, !IO),
    io.write_string("switch (", !IO),
    output_rval_maybe_with_enum(ModuleInfo, Val,
        FuncInfo ^ func_info_name ^ mod_name, !IO),
    io.write_string(") {\n", !IO),
    output_switch_cases(Indent + 1, ModuleInfo, FuncInfo, Context, Cases,
        Default, ExitMethods, !IO),
    indent_line(Context, Indent, !IO),
    io.write_string("}\n", !IO).

    % transfer of control
    %
output_stmt(_, _, _, ml_stmt_label(_), _, _, _, _)  :-
    unexpected(this_file, "output_stmt: labels not supported in Java.").
output_stmt(_, _, _, ml_stmt_goto(goto_label(_)), _, _, _, _) :-
    unexpected(this_file, "output_stmt: gotos not supported in Java.").
output_stmt(Indent, _, _FuncInfo, ml_stmt_goto(goto_break), _Context,
        ExitMethods, !IO) :-
    indent_line(Indent, !IO),
    io.write_string("break;\n", !IO),
    ExitMethods = set.make_singleton_set(can_break).
output_stmt(Indent, _, _FuncInfo, ml_stmt_goto(goto_continue), _Context,
        ExitMethods, !IO) :-
    indent_line(Indent, !IO),
    io.write_string("continue;\n", !IO),
    ExitMethods = set.make_singleton_set(can_continue).
output_stmt(_, _, _, ml_stmt_computed_goto(_, _), _, _, _, _) :-
    unexpected(this_file,
        "output_stmt: computed gotos not supported in Java.").

    % function call/return
    %
output_stmt(Indent, ModuleInfo, CallerFuncInfo, Call, Context, ExitMethods,
        !IO) :-
    Call = ml_stmt_call(Signature, FuncRval, MaybeObject, CallArgs, Results,
        _IsTailCall),
    Signature = mlds_func_signature(ArgTypes, RetTypes),
    ModuleName = CallerFuncInfo ^ func_info_name ^ mod_name,
    indent_line(Indent, !IO),
    io.write_string("{\n", !IO),
    indent_line(Context, Indent + 1, !IO),
    (
        Results = []
    ;
        Results = [Lval],
        output_lval(ModuleInfo, Lval, ModuleName, !IO),
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
    ( FuncRval = ml_const(mlconst_code_addr(_)) ->
        % This is a standard method call.
        (
            MaybeObject = yes(Object),
            output_bracketed_rval(ModuleInfo, Object, ModuleName, !IO),
            io.write_string(".", !IO)
        ;
            MaybeObject = no
        ),
        % This is a standard function call.
        output_call_rval(ModuleInfo, FuncRval, ModuleName, !IO),
        io.write_string("(", !IO),
        io.write_list(CallArgs, ", ",
            (pred(CallArg::in, !.IO::di, !:IO::uo) is det :-
                output_rval(ModuleInfo, CallArg, ModuleName, !IO)), !IO),
        io.write_string(")", !IO)
    ;
        % This is a call using a method pointer.
        %
        % Here we do downcasting, as a call will always return
        % something of type java.lang.Object
        %
        % XXX This is a hack, I can't see any way to do this downcasting
        % nicely, as it needs to effectively be wrapped around the method call
        % itself, so it acts before this predicate's solution to multiple
        % return values, see above.
        %
        (
            RetTypes = []
        ;
            RetTypes = [RetType],
            ( java_builtin_type(RetType, _, JavaBoxedName, _) ->
                io.write_string("((", !IO),
                io.write_string(JavaBoxedName, !IO),
                io.write_string(") ", !IO)
            ;
                io.write_string("((", !IO),
                output_type(normal_style, RetType, !IO),
                io.write_string(") ", !IO)
            )
        ;
            RetTypes = [_, _ | _],
            io.write_string("((java.lang.Object[]) ", !IO)
        ),
        (
            MaybeObject = yes(Object),
            output_bracketed_rval(ModuleInfo, Object, ModuleName, !IO),
            io.write_string(".", !IO)
        ;
            MaybeObject = no
        ),

        list.length(CallArgs, Arity),
        ( Arity =< max_specialised_method_ptr_arity ->
            io.write_string("((jmercury.runtime.MethodPtr", !IO),
            io.write_int(Arity, !IO),
            io.write_string(") ", !IO),
            output_bracketed_rval(ModuleInfo, FuncRval, ModuleName, !IO),
            io.write_string(").call___0_0(", !IO),
            output_boxed_args(ModuleInfo, CallArgs, ArgTypes, ModuleName, !IO)
        ;
            io.write_string("((jmercury.runtime.MethodPtrN) ", !IO),
            output_bracketed_rval(ModuleInfo, FuncRval, ModuleName, !IO),
            io.write_string(").call___0_0(", !IO),
            output_args_as_array(ModuleInfo, CallArgs, ArgTypes, ModuleName,
                !IO)
        ),

        % Closes brackets, and calls unbox methods for downcasting.
        % XXX This is a hack, see the above comment.
        io.write_string(")", !IO),
        (
            RetTypes = []
        ;
            RetTypes = [RetType2],
            ( java_builtin_type(RetType2, _, _, UnboxMethod) ->
                io.write_string(").", !IO),
                io.write_string(UnboxMethod, !IO),
                io.write_string("()", !IO)
            ;
                io.write_string(")", !IO)
            )
        ;
            RetTypes = [_, _ | _],
            io.write_string(")", !IO)
        )
    ),
    io.write_string(";\n", !IO),

    ( Results = [_, _ | _] ->
        % Copy the results from the "result" array into the Result
        % lvals (unboxing them as we go).
        output_assign_results(ModuleInfo, Results, RetTypes, 0, ModuleName,
            Indent + 1, Context, !IO)
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
    %
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO),
    ExitMethods = set.make_singleton_set(can_fall_through).

output_stmt(Indent, ModuleInfo, FuncInfo, ml_stmt_return(Results), _,
        ExitMethods, !IO) :-
    (
        Results = [],
        indent_line(Indent, !IO),
        io.write_string("return;\n", !IO)
    ;
        Results = [Rval],
        indent_line(Indent, !IO),
        io.write_string("return ", !IO),
        output_rval(ModuleInfo, Rval, FuncInfo ^ func_info_name ^ mod_name,
            !IO),
        io.write_string(";\n", !IO)
    ;
        Results = [_, _ | _],
        FuncInfo = func_info(FuncName, Params),
        Params = mlds_func_params(_Args, ReturnTypes),
        TypesAndResults = assoc_list.from_corresponding_lists(
            ReturnTypes, Results),
        io.write_string("return new java.lang.Object[] {\n", !IO),
        indent_line(Indent + 1, !IO),
        Separator = ",\n" ++ duplicate_char(' ', (Indent + 1) * 2),
        io.write_list(TypesAndResults, Separator,
            (pred((Type - Result)::in, !.IO::di, !:IO::uo) is det :-
                output_boxed_rval(ModuleInfo, Type, Result,
                    FuncName ^ mod_name, !IO)),
            !IO),
        io.write_string("\n", !IO),
        indent_line(Indent, !IO),
        io.write_string("};\n", !IO)
    ),
    ExitMethods = set.make_singleton_set(can_return).

output_stmt(Indent, ModuleInfo, FuncInfo, DoCommitStmt, _, ExitMethods,
        !IO) :-
    DoCommitStmt = ml_stmt_do_commit(Ref),
    indent_line(Indent, !IO),
    output_rval(ModuleInfo, Ref, FuncInfo ^ func_info_name ^ mod_name, !IO),
    io.write_string(" = new jmercury.runtime.Commit();\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("throw ", !IO),
    output_rval(ModuleInfo, Ref, FuncInfo ^ func_info_name ^ mod_name, !IO),
    io.write_string(";\n", !IO),
    ExitMethods = set.make_singleton_set(can_throw).

output_stmt(Indent, ModuleInfo, FuncInfo, TryCommitStmt, _, ExitMethods,
        !IO) :-
    TryCommitStmt = ml_stmt_try_commit(_Ref, Stmt, Handler),
    indent_line(Indent, !IO),
    io.write_string("try\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("{\n", !IO),
    output_statement(Indent + 1, ModuleInfo, FuncInfo, Stmt, TryExitMethods0,
        !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("catch (jmercury.runtime.Commit commit_variable)\n", !IO),
    indent_line(Indent, !IO),
    io.write_string("{\n", !IO),
    indent_line(Indent + 1, !IO),
    output_statement(Indent + 1, ModuleInfo, FuncInfo, Handler,
        CatchExitMethods, !IO),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO),
    ExitMethods = (TryExitMethods0 `set.delete` can_throw)
        `set.union`  CatchExitMethods.

    % exception handling
    %

    % XXX not yet implemented

    % atomic statements
    %
output_stmt(Indent, ModuleInfo, FuncInfo, AtomicStmt, Context, ExitMethods,
        !IO) :-
    AtomicStmt = ml_stmt_atomic(AtomicStatement),
    output_atomic_stmt(Indent, ModuleInfo, FuncInfo, AtomicStatement, Context,
        !IO),
    ExitMethods = set.make_singleton_set(can_fall_through).
    % Returns a set of exit_methods that describes whether the while
    % statement can complete normally.

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

:- pred output_args_as_array(module_info::in, list(mlds_rval)::in,
    list(mlds_type)::in, mlds_module_name::in, io::di, io::uo) is det.

output_args_as_array(ModuleInfo, CallArgs, CallArgTypes, ModuleName, !IO) :-
    io.write_string("new java.lang.Object[] { ", !IO),
    output_boxed_args(ModuleInfo, CallArgs, CallArgTypes, ModuleName, !IO),
    io.write_string("} ", !IO).

:- pred output_boxed_args(module_info::in, list(mlds_rval)::in,
    list(mlds_type)::in, mlds_module_name::in, io::di, io::uo) is det.

output_boxed_args(_, [], [], _, !IO).
output_boxed_args(_, [_ | _], [], _, _, _) :-
    unexpected(this_file, "output_boxed_args: length mismatch.").
output_boxed_args(_, [], [_ | _], _, _, _) :-
    unexpected(this_file, "output_boxed_args: length mismatch.").
output_boxed_args(ModuleInfo, [CallArg | CallArgs],
        [CallArgType | CallArgTypes], ModuleName, !IO) :-
    output_boxed_rval(ModuleInfo, CallArgType, CallArg, ModuleName, !IO),
    (
        CallArgs = []
    ;
        CallArgs = [_ | _],
        io.write_string(", ", !IO),
        output_boxed_args(ModuleInfo, CallArgs, CallArgTypes, ModuleName, !IO)
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
:- pred output_assign_results(module_info::in, list(mlds_lval)::in,
    list(mlds_type)::in, int::in, mlds_module_name::in, indent::in,
    mlds_context::in, io::di, io::uo) is det.

output_assign_results(_, [], [], _, _, _, _, !IO).
output_assign_results(ModuleInfo, [Lval | Lvals], [Type | Types], ResultIndex,
        ModuleName, Indent, Context, !IO) :-
    indent_line(Context, Indent, !IO),
    output_lval(ModuleInfo, Lval, ModuleName, !IO),
    io.write_string(" = ", !IO),
    output_unboxed_result(Type, ResultIndex, !IO),
    io.write_string(";\n", !IO),
    output_assign_results(ModuleInfo, Lvals, Types, ResultIndex + 1,
        ModuleName, Indent, Context, !IO).
output_assign_results(_, [_ | _], [], _, _, _, _, _, _) :-
    unexpected(this_file, "output_assign_results: list length mismatch.").
output_assign_results(_, [], [_ | _], _, _, _, _, _, _) :-
    unexpected(this_file, "output_assign_results: list lenght mismatch.").

:- pred output_unboxed_result(mlds_type::in, int::in, io::di, io::uo) is det.

output_unboxed_result(Type, ResultIndex, !IO) :-
    ( java_builtin_type(Type, _, JavaBoxedName, UnboxMethod) ->
        io.write_string("((", !IO),
        io.write_string(JavaBoxedName, !IO),
        io.write_string(") ", !IO),
        io.format("result[%d]).%s()", [i(ResultIndex), s(UnboxMethod)], !IO)
    ;
        io.write_string("(", !IO),
        output_type(normal_style, Type, !IO),
        io.write_string(") ", !IO),
        io.format("result[%d]", [i(ResultIndex)], !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Extra code for outputting switch statements
%

:- pred output_switch_cases(indent::in, module_info::in, func_info::in,
    mlds_context::in, list(mlds_switch_case)::in, mlds_switch_default::in,
    exit_methods::out, io::di, io::uo) is det.

output_switch_cases(Indent, ModuleInfo, FuncInfo, Context, [], Default,
        ExitMethods, !IO) :-
    output_switch_default(Indent, ModuleInfo, FuncInfo, Context, Default,
        ExitMethods, !IO).
output_switch_cases(Indent, ModuleInfo, FuncInfo, Context, [Case | Cases],
        Default, ExitMethods, !IO) :-
    output_switch_case(Indent, ModuleInfo, FuncInfo, Context, Case,
        CaseExitMethods0, !IO),
    output_switch_cases(Indent, ModuleInfo, FuncInfo, Context, Cases, Default,
        CasesExitMethods, !IO),
    ( set.member(can_break, CaseExitMethods0) ->
        CaseExitMethods = (CaseExitMethods0 `set.delete` can_break)
            `set.insert` can_fall_through
    ;
        CaseExitMethods = CaseExitMethods0
    ),
    ExitMethods = CaseExitMethods `set.union` CasesExitMethods.

:- pred output_switch_case(indent::in, module_info::in, func_info::in,
    mlds_context::in, mlds_switch_case::in, exit_methods::out,
    io::di, io::uo) is det.

output_switch_case(Indent, ModuleInfo, FuncInfo, Context, Case, ExitMethods,
        !IO) :-
    Case = mlds_switch_case(FirstCond, LaterConds, Statement),
    ModuleName = FuncInfo ^ func_info_name ^ mod_name,
    output_case_cond(Indent, ModuleInfo, ModuleName, Context, FirstCond, !IO),
    list.foldl(output_case_cond(Indent, ModuleInfo, ModuleName, Context),
        LaterConds, !IO),
    output_statement(Indent + 1, ModuleInfo, FuncInfo, Statement,
        StmtExitMethods, !IO),
    ( set.member(can_fall_through, StmtExitMethods) ->
        indent_line(Context, Indent + 1, !IO),
        io.write_string("break;\n", !IO),
        ExitMethods = (StmtExitMethods `set.insert` can_break)
            `set.delete` can_fall_through
    ;
        % Don't output `break' since it would be unreachable.
        ExitMethods = StmtExitMethods
    ).

:- pred output_case_cond(indent::in, module_info::in, mlds_module_name::in,
    mlds_context::in, mlds_case_match_cond::in, io::di, io::uo) is det.

output_case_cond(Indent, ModuleInfo, ModuleName, Context, match_value(Val),
        !IO) :-
    indent_line(Context, Indent, !IO),
    io.write_string("case ", !IO),
    output_rval(ModuleInfo, Val, ModuleName, !IO),
    io.write_string(":\n", !IO).
output_case_cond(_Indent, _ModuleInfo, _ModuleName, _Context,
        match_range(_, _), _, _) :-
    unexpected(this_file,
        "output_case_cond: cannot match ranges in Java cases").

:- pred output_switch_default(indent::in, module_info::in, func_info::in,
    mlds_context::in, mlds_switch_default::in, exit_methods::out,
    io::di, io::uo) is det.

output_switch_default(_Indent, _ModuleInfo, _FuncInfo, _Context,
        default_do_nothing, ExitMethods, !IO) :-
    ExitMethods = set.make_singleton_set(can_fall_through).
output_switch_default(Indent, ModuleInfo, FuncInfo, Context,
        default_case(Statement), ExitMethods, !IO) :-
    indent_line(Context, Indent, !IO),
    io.write_string("default:\n", !IO),
    output_statement(Indent + 1, ModuleInfo, FuncInfo, Statement, ExitMethods,
        !IO).
output_switch_default(Indent, _ModuleInfo, _FuncInfo, Context,
        default_is_unreachable, ExitMethods, !IO) :-
    indent_line(Context, Indent, !IO),
    io.write_string("default: /*NOTREACHED*/\n", !IO),
    indent_line(Context, Indent + 1, !IO),
    io.write_string("throw new jmercury.runtime.UnreachableDefault();\n", !IO),
    ExitMethods = set.make_singleton_set(can_throw).

%-----------------------------------------------------------------------------%
%
% Code for outputting atomic statements.
%

:- pred output_atomic_stmt(indent::in, module_info::in, func_info::in,
    mlds_atomic_statement::in, mlds_context::in, io::di, io::uo) is det.

    % comments
    %
output_atomic_stmt(Indent, _ModuleInfo, _FuncInfo, comment(Comment), _, !IO) :-
    % XXX We should escape any "*/"'s in the Comment. We should also split
    % the comment into lines and indent each line appropriately.
    indent_line(Indent, !IO),
    io.write_string("/* ", !IO),
    io.write_string(Comment, !IO),
    io.write_string(" */\n", !IO).

    % assignment
    %
output_atomic_stmt(Indent, ModuleInfo, FuncInfo, assign(Lval, Rval), _, !IO) :-
    ModuleName = FuncInfo ^ func_info_name ^ mod_name,
    indent_line(Indent, !IO),
    output_lval(ModuleInfo, Lval, ModuleName, !IO),
    io.write_string(" = ", !IO),
    (
        LvalType = mlds_lval_type(Lval),
        type_is_object(LvalType)
    ->
        ( rval_is_int_const(Rval) ->
            % If it is a enumeration object make a reference to a static
            % instance.
            output_type(normal_style, LvalType, !IO),
            io.write_string(".K", !IO),
            output_rval(ModuleInfo, Rval, ModuleName, !IO)
        ;
            output_rval(ModuleInfo, Rval, ModuleName, !IO)
        )
    ;
        output_rval_maybe_with_enum(ModuleInfo, Rval, ModuleName, !IO)
    ),
    io.write_string(";\n", !IO).

output_atomic_stmt(_, _, _, assign_if_in_heap(_, _), _, !IO) :-
    sorry(this_file, "output_atomic_stmt: assign_if_in_heap").

    % heap management
    %
output_atomic_stmt(_Indent, _, _FuncInfo, delete_object(_Lval), _, _, _) :-
    unexpected(this_file, "delete_object not supported in Java.").

output_atomic_stmt(Indent, ModuleInfo, FuncInfo, NewObject, Context, !IO) :-
    NewObject = new_object(Target, _MaybeTag, HasSecTag, Type,
        _MaybeSize, MaybeCtorName, Args, ArgTypes, _MayUseAtomic),
    ModuleName = FuncInfo ^ func_info_name ^ mod_name,

    indent_line(Indent, !IO),
    io.write_string("{\n", !IO),
    indent_line(Context, Indent + 1, !IO),
    output_lval(ModuleInfo, Target, ModuleName, !IO),
    io.write_string(" = new ", !IO),

    % Generate class constructor name.
    (
        MaybeCtorName = yes(QualifiedCtorId),
        \+ (
            Type = mercury_type(_, CtorCat, _),
            hand_defined_type(CtorCat, _)
        )
    ->
        output_type(normal_style, Type, !IO),
        io.write_char('.', !IO),
        QualifiedCtorId = qual(_ModuleName, _QualKind, CtorDefn),
        CtorDefn = ctor_id(CtorName, CtorArity),
        output_class_name_and_arity(CtorName, CtorArity, !IO)
    ;
        output_type(normal_style, Type, !IO)
    ),
    IsArray = type_is_array(Type),
    (
        IsArray = is_array,
        % The new object will be an array, so we need to initialise it
        % using array literals syntax.
        io.write_string(" {", !IO),
        output_init_args(ModuleInfo, Args, ArgTypes, 0, HasSecTag, ModuleName,
            !IO),
        io.write_string("};\n", !IO)
    ;
        IsArray = not_array,
        % Generate constructor arguments.
        io.write_string("(", !IO),
        output_init_args(ModuleInfo, Args, ArgTypes, 0, HasSecTag, ModuleName,
            !IO),
        io.write_string(");\n", !IO)
    ),
    indent_line(Indent, !IO),
    io.write_string("}\n", !IO).

output_atomic_stmt(_Indent, _, _FuncInfo, gc_check, _, _, _) :-
    unexpected(this_file, "gc_check not implemented.").

output_atomic_stmt(_Indent, _, _FuncInfo, mark_hp(_Lval), _, _, _) :-
    unexpected(this_file, "mark_hp not implemented.").

output_atomic_stmt(_Indent, _, _FuncInfo, restore_hp(_Rval), _, _, _) :-
    unexpected(this_file, "restore_hp not implemented.").

    % Trail management.
    %
output_atomic_stmt(_Indent, _, _FuncInfo, trail_op(_TrailOp), _, _, _) :-
    unexpected(this_file, "trail_ops not implemented.").

    % Foreign language interfacing.
    %
output_atomic_stmt(Indent, ModuleInfo, FuncInfo,
        inline_target_code(TargetLang, Components), Context, !IO) :-
    (
        TargetLang = ml_target_java,
        indent_line(Indent, !IO),
        ModuleName = FuncInfo ^ func_info_name ^ mod_name,
        list.foldl(
            output_target_code_component(ModuleInfo, ModuleName, Context),
            Components, !IO)
    ;
        ( TargetLang = ml_target_c
        ; TargetLang = ml_target_gnu_c
        ; TargetLang = ml_target_asm
        ; TargetLang = ml_target_il
        ),
        unexpected(this_file, "inline_target_code only works for lang_java")
    ).

output_atomic_stmt(_Indent, _, _FuncInfo,
        outline_foreign_proc(_TargetLang, _Vs, _Lvals, _Code),
        _Context, _, _)  :-
    unexpected(this_file, "foreign language interfacing not implemented").

%-----------------------------------------------------------------------------%

:- pred output_target_code_component(module_info::in, mlds_module_name::in,
    mlds_context::in, target_code_component::in, io::di, io::uo) is det.

output_target_code_component(ModuleInfo, ModuleName, _Context, TargetCode,
        !IO) :-
    (
        TargetCode = user_target_code(CodeString, MaybeUserContext, _Attrs),
        (
            MaybeUserContext = yes(ProgContext),
            output_context(mlds_make_context(ProgContext), !IO)
        ;
            MaybeUserContext = no
        ),
        io.write_string(CodeString, !IO)
    ;
        TargetCode = raw_target_code(CodeString, _Attrs),
        io.write_string(CodeString, !IO)
    ;
        TargetCode = target_code_input(Rval),
        output_rval(ModuleInfo, Rval, ModuleName, !IO)
    ;
        TargetCode = target_code_output(Lval),
        output_lval(ModuleInfo, Lval, ModuleName, !IO)
    ;
        TargetCode = target_code_type(Type),
        output_type(normal_style, Type, !IO)
    ;
        TargetCode = target_code_name(Name),
        output_maybe_qualified_name(Name, ModuleName, !IO)
    ).

%-----------------------------------------------------------------------------%

    % Output initial values of an object's fields as arguments for the
    % object's class constructor.
    %
:- pred output_init_args(module_info::in, list(mlds_rval)::in,
    list(mlds_type)::in, int::in, bool::in, mlds_module_name::in,
    io::di, io::uo) is det.

output_init_args(_, [], [], _, _, _, !IO).
output_init_args(_, [_ | _], [], _, _, _, _, _) :-
    unexpected(this_file, "output_init_args: length mismatch.").
output_init_args(_, [], [_ | _], _, _, _, _, _) :-
    unexpected(this_file, "output_init_args: length mismatch.").
output_init_args(ModuleInfo, [Arg | Args], [_ArgType | ArgTypes], ArgNum,
        HasSecTag, ModuleName, !IO) :-
    (
        ArgNum = 0,
        HasSecTag = yes
    ->
        % This first argument is a `data_tag', It is set by
        % the class constructor so this argument can be discarded.
        true
    ;
        output_rval(ModuleInfo, Arg, ModuleName, !IO),
        (
            Args = []
        ;
            Args = [_ | _],
            io.write_string(", ", !IO)
        )
    ),
    output_init_args(ModuleInfo, Args, ArgTypes, ArgNum + 1, HasSecTag,
        ModuleName, !IO).

%-----------------------------------------------------------------------------%
%
% Code to output expressions
%

:- pred output_lval(module_info::in, mlds_lval::in, mlds_module_name::in,
    io::di, io::uo) is det.

output_lval(ModuleInfo, Lval, ModuleName, !IO) :-
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
            io.write_string("((java.lang.Object[]) ", !IO),
            output_rval(ModuleInfo, PtrRval, ModuleName, !IO),
            io.write_string(")[", !IO),
            output_rval(ModuleInfo, OffsetRval, ModuleName, !IO),
            io.write_string("]", !IO)
        ;
            FieldId = ml_field_named(FieldName, CtorType),
            (
                FieldName = qual(_, _, UnqualFieldName),
                UnqualFieldName = "data_tag"
            ->
                % If the field we are trying to access is just a `data_tag'
                % then it is a member of the base class.
                output_bracketed_rval(ModuleInfo, PtrRval, ModuleName, !IO),
                io.write_string(".", !IO)
            ;
                % Otherwise the field we are trying to access may be
                % in a derived class. Objects are manipulated as instances
                % of their base class, so we need to downcast to the derived
                % class to access some fields.

                io.write_string("((", !IO),
                output_type(normal_style, CtorType, !IO),
                io.write_string(") ", !IO),
                output_bracketed_rval(ModuleInfo, PtrRval, ModuleName, !IO),
                % The actual variable.
                io.write_string(").", !IO)
            ),
            FieldName = qual(_, _, UnqualFieldName),
            output_valid_mangled_name(UnqualFieldName, !IO)
        )
    ;
        Lval = ml_mem_ref(Rval, _Type),
        output_bracketed_rval(ModuleInfo, Rval, ModuleName, !IO)
    ;
        Lval = ml_global_var_ref(GlobalVarRef),
        GlobalVarRef = env_var_ref(EnvVarName),
        io.write_string("mercury_envvar_", !IO),
        io.write_string(EnvVarName, !IO)
    ;
        Lval = ml_var(qual(ModName, QualKind, Name), _),
        QualName = qual(ModName, QualKind, entity_data(mlds_data_var(Name))),
        output_maybe_qualified_name(QualName, ModuleName, !IO)
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

:- pred output_call_rval(module_info::in, mlds_rval::in, mlds_module_name::in,
    io::di, io::uo) is det.

output_call_rval(ModuleInfo, Rval, ModuleName, !IO) :-
    (
        Rval = ml_const(Const),
        Const = mlconst_code_addr(CodeAddr)
    ->
        IsCall = yes,
        mlds_output_code_addr(CodeAddr, IsCall, !IO)
    ;
        output_bracketed_rval(ModuleInfo, Rval, ModuleName, !IO)
    ).

:- pred output_bracketed_rval(module_info::in, mlds_rval::in,
    mlds_module_name::in, io::di, io::uo) is det.

output_bracketed_rval(ModuleInfo, Rval, ModuleName, !IO) :-
    (
        % If it's just a variable name, then we don't need parentheses.
        ( Rval = ml_lval(ml_var(_,_))
        ; Rval = ml_const(mlconst_code_addr(_))
        )
    ->
        output_rval(ModuleInfo, Rval, ModuleName, !IO)
    ;
        io.write_char('(', !IO),
        output_rval(ModuleInfo, Rval, ModuleName, !IO),
        io.write_char(')', !IO)
    ).

:- pred output_rval(module_info::in, mlds_rval::in, mlds_module_name::in,
    io::di, io::uo) is det.

output_rval(ModuleInfo, Rval, ModuleName, !IO) :-
    (
        Rval = ml_lval(Lval),
        output_lval(ModuleInfo, Lval, ModuleName, !IO)
    ;
        Rval = ml_scalar_common(_),
        unexpected(this_file, "output_rval: ml_scalar_common")
    ;
        Rval = ml_vector_common_row(_, _),
        unexpected(this_file, "output_rval: ml_vector_common_row")
    ;
        Rval = ml_mkword(_, _),
        unexpected(this_file, "output_rval: tags not supported in Java")
    ;
        Rval = ml_const(Const),
        output_rval_const(Const, !IO)
    ;
        Rval = ml_unop(Op, RvalA),
        output_unop(ModuleInfo, Op, RvalA, ModuleName, !IO)
    ;
        Rval = ml_binop(Op, RvalA, RvalB),
        output_binop(ModuleInfo, Op, RvalA, RvalB, ModuleName, !IO)
    ;
        Rval = ml_mem_addr(_Lval),
        unexpected(this_file, "output_rval: mem_addr(_) not supported")
    ;
        Rval = ml_self(_),
        io.write_string("this", !IO)
    ).

:- pred output_unop(module_info::in, mlds_unary_op::in, mlds_rval::in,
    mlds_module_name::in, io::di, io::uo) is det.

output_unop(ModuleInfo, Unop, Expr, ModuleName, !IO) :-
    (
        Unop = cast(Type),
        output_cast_rval(ModuleInfo, Type, Expr, ModuleName, !IO)
    ;
        Unop = box(Type),
        output_boxed_rval(ModuleInfo, Type, Expr, ModuleName, !IO)
    ;
        Unop = unbox(Type),
        output_unboxed_rval(ModuleInfo, Type, Expr, ModuleName, !IO)
    ;
        Unop = std_unop(StdUnop),
        output_std_unop(ModuleInfo, StdUnop, Expr, ModuleName, !IO)
    ).

:- pred output_cast_rval(module_info::in, mlds_type::in, mlds_rval::in,
    mlds_module_name::in, io::di, io::uo) is det.

output_cast_rval(ModuleInfo, Type, Expr, ModuleName, !IO) :-
    % rtti_to_mlds.m generates casts from int to
    % jmercury.runtime.PseudoTypeInfo, but for Java
    % we need to treat these as constructions, not casts.
    % Similarly for conversions from TypeCtorInfo to TypeInfo.
    (
        Type = mlds_pseudo_type_info_type,
        Expr = ml_const(mlconst_int(N))
    ->
        maybe_output_comment("cast", !IO),
        ( have_preallocated_pseudo_type_var(N) ->
            io.write_string("jmercury.runtime.PseudoTypeInfo.K", !IO),
            io.write_int(N, !IO)
        ;
            io.write_string("new jmercury.runtime.PseudoTypeInfo(", !IO),
            output_rval(ModuleInfo, Expr, ModuleName, !IO),
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
        maybe_output_comment("cast", !IO),
        io.write_string("jmercury.runtime.TypeInfo_Struct.maybe_new(",
            !IO),
        output_rval(ModuleInfo, Expr, ModuleName, !IO),
        io.write_string(")", !IO)
    ;
        type_is_object(Type),
        Expr = ml_const(mlconst_int(N))
    ->
        % If it is a enumeration object make a reference to a static instance.
        output_type(normal_style, Type, !IO),
        io.write_string(".K", !IO),
        io.write_int(N, !IO)
    ;
        io.write_string("(", !IO),
        output_type(normal_style, Type, !IO),
        io.write_string(") ", !IO),
        ( java_builtin_type(Type, "int", _, _) ->
            output_rval_maybe_with_enum(ModuleInfo, Expr, ModuleName, !IO)
        ;
            output_rval(ModuleInfo, Expr, ModuleName, !IO)
        )
    ).

:- pred have_preallocated_pseudo_type_var(int::in) is semidet.

have_preallocated_pseudo_type_var(N) :-
    % Corresponds to static members in class PseudoTypeInfo.
    N >= 1,
    N =< 5.

:- pred output_boxed_rval(module_info::in, mlds_type::in, mlds_rval::in,
     mlds_module_name::in, io::di, io::uo) is det.

output_boxed_rval(ModuleInfo, Type, Expr, ModuleName, !IO) :-
    ( java_builtin_type(Type, _JavaName, JavaBoxedName, _) ->
        io.write_string("new ", !IO),
        io.write_string(JavaBoxedName, !IO),
        io.write_string("(", !IO),
        output_rval(ModuleInfo, Expr, ModuleName, !IO),
        io.write_string(")", !IO)
    ;
        io.write_string("((java.lang.Object) (", !IO),
        output_rval(ModuleInfo, Expr, ModuleName, !IO),
        io.write_string("))", !IO)
    ).

:- pred output_unboxed_rval(module_info::in, mlds_type::in, mlds_rval::in,
    mlds_module_name::in, io::di, io::uo) is det.

output_unboxed_rval(ModuleInfo, Type, Expr, ModuleName, !IO) :-
    ( java_builtin_type(Type, _, JavaBoxedName, UnboxMethod) ->
        io.write_string("((", !IO),
        io.write_string(JavaBoxedName, !IO),
        io.write_string(") ", !IO),
        output_bracketed_rval(ModuleInfo, Expr, ModuleName, !IO),
        io.write_string(").", !IO),
        io.write_string(UnboxMethod, !IO),
        io.write_string("()", !IO)
    ;
        io.write_string("((", !IO),
        output_type(normal_style, Type, !IO),
        io.write_string(") ", !IO),
        output_rval(ModuleInfo, Expr, ModuleName, !IO),
        io.write_string(")", !IO)
    ).

    % java_builtin_type(MLDS_Type, JavaUnboxedType, JavaBoxedType, UnboxMethod):
    %
    % For a given Mercury type, check if this corresponds to a Java type
    % which has both unboxed (builtin) and boxed (class) versions, and if so,
    % return their names, and the name of the method to get the unboxed value
    % from the boxed type.
    %
:- pred java_builtin_type(mlds_type::in, string::out, string::out, string::out)
    is semidet.

java_builtin_type(Type, "int", "java.lang.Integer", "intValue") :-
    Type = mlds_native_int_type.
java_builtin_type(Type, "int", "java.lang.Integer", "intValue") :-
    Type = mercury_type(builtin_type(builtin_type_int), _, _).
java_builtin_type(Type, "double", "java.lang.Double", "doubleValue") :-
    Type = mlds_native_float_type.
java_builtin_type(Type, "double", "java.lang.Double", "doubleValue") :-
    Type = mercury_type(builtin_type(builtin_type_float), _, _).
java_builtin_type(Type, "char", "java.lang.Character", "charValue") :-
    Type = mlds_native_char_type.
java_builtin_type(Type, "char", "java.lang.Character", "charValue") :-
    Type = mercury_type(builtin_type(builtin_type_char), _, _).
java_builtin_type(Type, "boolean", "java.lang.Boolean", "booleanValue") :-
    Type = mlds_native_bool_type.

    % io.state and store.store(S) are dummy variables
    % for which we pass an arbitrary integer. For this
    % reason they should have the Java type `int'.
    %
java_builtin_type(Type, "int", "java.lang.Integer", "intValue") :-
    % The test for defined/3 is logically redundant since all dummy
    % types are defined types, but enables the compiler to infer that
    % this disjunction is a switch.
    Type = mercury_type(defined_type(_, _, _), TypeCtorCat, _),
    TypeCtorCat = ctor_cat_builtin_dummy.

:- pred output_std_unop(module_info::in, builtin_ops.unary_op::in,
    mlds_rval::in, mlds_module_name::in, io::di, io::uo) is det.

    % For the Java back-end, there are no tags, so all the tagging operators
    % are no-ops, except for `tag', which always returns zero (a tag of zero
    % means there's no tag).
    %
output_std_unop(ModuleInfo, UnaryOp, Expr, ModuleName, !IO) :-
    ( UnaryOp = tag ->
        io.write_string("/* tag */  0", !IO)
    ;
        java_unary_prefix_op(UnaryOp, UnaryOpString),
        io.write_string(UnaryOpString, !IO),
        io.write_string("(", !IO),
        output_rval(ModuleInfo, Expr, ModuleName, !IO),
        io.write_string(")", !IO)
    ).

:- pred output_binop(module_info::in, binary_op::in, mlds_rval::in,
    mlds_rval::in, mlds_module_name::in, io::di, io::uo) is det.

output_binop(ModuleInfo, Op, X, Y, ModuleName, !IO) :-
    ( Op = array_index(_Type) ->
        output_bracketed_rval(ModuleInfo, X, ModuleName, !IO),
        io.write_string("[", !IO),
        output_rval(ModuleInfo, Y, ModuleName, !IO),
        io.write_string("]", !IO)
    ; java_string_compare_op(Op, OpStr) ->
        io.write_string("(", !IO),
        output_rval(ModuleInfo, X, ModuleName, !IO),
        io.write_string(".compareTo(", !IO),
        output_rval(ModuleInfo, Y, ModuleName, !IO),
        io.write_string(") ", !IO),
        io.write_string(OpStr, !IO),
        io.write_string(" 0)", !IO)
    ;
        ( java_float_compare_op(Op, OpStr1) ->
            OpStr = OpStr1
        ; java_float_op(Op, OpStr2) ->
            OpStr = OpStr2
        ;
            fail
        )
    ->
        io.write_string("(", !IO),
        output_rval_maybe_with_enum(ModuleInfo, X, ModuleName, !IO),
        io.write_string(" ", !IO),
        io.write_string(OpStr, !IO),
        io.write_string(" ", !IO),
        output_rval_maybe_with_enum(ModuleInfo, Y, ModuleName, !IO),
        io.write_string(")", !IO)
    ;
        io.write_string("(", !IO),
        output_rval_maybe_with_enum(ModuleInfo, X, ModuleName, !IO),
        io.write_string(" ", !IO),
        output_binary_op(Op, !IO),
        io.write_string(" ", !IO),
        output_rval_maybe_with_enum(ModuleInfo, Y, ModuleName, !IO),
        io.write_string(")", !IO)
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
:- pred output_rval_maybe_with_enum(module_info::in, mlds_rval::in,
    mlds_module_name::in, io::di, io::uo) is det.

output_rval_maybe_with_enum(ModuleInfo, Rval, ModuleName, !IO) :-
    output_rval(ModuleInfo, Rval, ModuleName, !IO),
    ( rval_is_enum_object(Rval) ->
        io.write_string(".MR_value", !IO)
    ;
        true
    ).

:- pred output_binary_op(binary_op::in, io::di, io::uo) is det.

output_binary_op(Op, !IO) :-
    ( java_binary_infix_op(Op, OpStr) ->
        io.write_string(OpStr, !IO)
    ;
        unexpected(this_file,
            "output_binary_op: invalid binary operator")
    ).

:- pred output_rval_const(mlds_rval_const::in, io::di, io::uo) is det.

output_rval_const(Const, !IO) :-
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
        Const = mlconst_foreign(Lang, Value, _Type),
        expect(unify(Lang, lang_java), this_file,
            "output_rval_const: language other than Java."),
        % XXX Should we parenthesize this?
        io.write_string(Value, !IO)
    ;
        Const = mlconst_float(FloatVal),
        c_util.output_float_literal(FloatVal, !IO)
    ;
        Const = mlconst_string(String),
        io.write_string("""", !IO),
        c_util.output_quoted_string_lang(literal_java, String, !IO),
        io.write_string("""", !IO)
    ;
        Const = mlconst_multi_string(String),
        io.write_string("""", !IO),
        c_util.output_quoted_multi_string_lang(literal_java, String, !IO),
        io.write_string("""", !IO)
    ;
        Const = mlconst_named_const(NamedConst),
        io.write_string(NamedConst, !IO)
    ;
        Const = mlconst_code_addr(CodeAddr),
        IsCall = no,
        mlds_output_code_addr(CodeAddr, IsCall, !IO)
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
    % ints.  A literal 0xffffffff in a source file would be interpreted by a
    % 64-bit Mercury compiler as 4294967295.  If it is written out in decimal a
    % Java compiler would rightly complain because the integer is too large to
    % fit in a 32-bit int.  However, it won't complain if the literal is
    % expressed in hexadecimal (nor as the negative decimal -1).
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

%-----------------------------------------------------------------------------%

:- pred mlds_output_code_addr(mlds_code_addr::in, bool::in, io::di,
    io::uo) is det.

mlds_output_code_addr(CodeAddr, IsCall, !IO) :-
    (
        IsCall = no,
        % Not a function call, so we are taking the address of the
        % wrapper for that function (method).
        io.write_string("new ", !IO),
        create_addr_wrapper_name(CodeAddr, MangledClassEntityName0),
        MangledClassEntityName = shorten_class_name(MangledClassEntityName0),
        io.write_string(flip_initial_case(MangledClassEntityName), !IO),
        io.write_string("_0()", !IO)
    ;
        IsCall = yes,
        (
            CodeAddr = code_addr_proc(Label, _Sig),
            output_fully_qualified_proc_label(Label, !IO)
        ;
            CodeAddr = code_addr_internal(Label, SeqNum, _Sig),
            output_fully_qualified_proc_label(Label, !IO),
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
    output_data_name(DataName, !IO).

%-----------------------------------------------------------------------------%
%
% Miscellaneous stuff to handle indentation and generation of
% source context annotations.
%

:- mutable(last_context, prog_context, context_init, ground,
    [untrailed, attach_to_io_state]).

:- pred output_context(mlds_context::in, io::di, io::uo) is det.

output_context(Context, !IO) :-
    globals.io_lookup_bool_option(line_numbers, LineNumbers, !IO),
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
            io.write_string("// ", !IO),
            io.write_string(File, !IO),
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

:- pred indent_line(mlds_context::in, indent::in, io::di, io::uo) is det.

indent_line(Context, N, !IO) :-
    output_context(Context, !IO),
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

:- func this_file = string.

this_file = "mlds_to_java.m".

%-----------------------------------------------------------------------------%
