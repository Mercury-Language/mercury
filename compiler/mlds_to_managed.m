%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module:   mlds_to_managed
% Main author:  trd, petdr.
%
% Generate code for the foreign language interface to C# and managed C++.
%

:- module ml_backend__mlds_to_managed.
:- interface.

:- import_module libs.globals.
:- import_module ml_backend.mlds.

:- import_module io.

:- inst managed_lang == bound(csharp; managed_cplusplus).

    % Convert the MLDS to the specified foreign language and write
    % it to a file.
    %
:- pred output_managed_code(foreign_language::in(managed_lang), mlds::in,
    io::di, io::uo) is det.

    % Print the header comments of the output module.
    %
:- pred output_src_start(mercury_module_name::in, io::di, io::uo) is det.

    % Print the footer commments of the output module.
    %
:- pred output_src_end(mercury_module_name::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.foreign.
:- import_module backend_libs.rtti.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_pred. % for `pred_proc_id'.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.ilasm.
:- import_module ml_backend.ilds.
:- import_module ml_backend.il_peephole.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.mlds_to_il.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module term.

output_managed_code(Lang, MLDS, !IO) :-
    MLDS = mlds(ModuleName, _ForeignCode, _Imports, _Defns,
        _InitPreds, _FinalPreds),
    output_src_start(ModuleName, !IO),
    io__nl(!IO),
    generate_code(Lang, MLDS, !IO),
    output_src_end(ModuleName, !IO).

%-----------------------------------------------------------------------------%

output_src_start(ModuleName, !IO) :-
    library__version(Version),
    sym_name_to_string(ModuleName, ModuleNameStr),
    io__write_strings(
        ["//\n// Automatically generated from `",
        ModuleNameStr,
        ".m' by the\n",
        "// Mercury compiler, version ",
        Version,
        ".\n",
        "// Do not edit.\n",
        "\n\n"], !IO).

output_src_end(ModuleName, !IO) :-
    io__write_string("// End of module: ", !IO),
    prog_out__write_sym_name(ModuleName, !IO),
    io__write_string(". \n", !IO).

%-----------------------------------------------------------------------------%

:- pred generate_code(foreign_language::in(managed_lang), mlds::in,
    io::di, io::uo) is det.

generate_code(Lang, MLDS, !IO) :-
    MLDS = mlds(ModuleName, AllForeignCode, Imports, Defns,
        _InitPreds, _FinalPreds),
    ClassName = class_name(mercury_module_name_to_mlds(ModuleName),
        wrapper_class_name),
    io__nl(!IO),

    % Output any generic header code specific to the target language.
    output_language_specific_header_code(Lang, ModuleName, Imports, !IO),

    % Get the foreign code for the required language.
    ForeignCode = map__lookup(AllForeignCode, Lang),
    generate_foreign_header_code(Lang, ModuleName, ForeignCode, !IO),

    % Output the namespace.
    generate_namespace_details(Lang, ClassName, NameSpaceFmtStr, Namespace),
    io__write_list(Namespace, "\n",
        (pred(N::in, di, uo) is det -->
            io__format(NameSpaceFmtStr, [s(N)])
    ), !IO),

    (
        Lang = csharp,
        io__write_strings(["\npublic class " ++ wrapper_class_name, "{\n"],
            !IO)
    ;
        Lang = managed_cplusplus,
        io__write_strings(["\n__gc public class " ++ wrapper_class_name,
            "{\n", "public:\n"], !IO)
    ),

    % Output the contents of pragma foreign_code declarations.
    generate_foreign_code(Lang,
        mercury_module_name_to_mlds(ModuleName), ForeignCode, !IO),

    io__write_string("\n", !IO),

    % Output the contents of foreign_proc declarations.
    % Put each one inside a method.
    list__foldl(generate_method_code(Lang,
        mercury_module_name_to_mlds(ModuleName)), Defns, !IO),

    io__write_string("};\n", !IO),

    % Close the namespace braces.
    io__write_list(Namespace, "\n",
        (pred(_N::in, di, uo) is det -->
            io__write_string("}")
    ), !IO),
    io__nl(!IO).

:- pred output_language_specific_header_code(
    foreign_language::in(managed_lang), mercury_module_name::in,
    mlds__imports::in, io::di, io::uo) is det.

output_language_specific_header_code(csharp, _ModuleName, _Imports, !IO) :-
    get_il_data_rep(DataRep, !IO),
    ( DataRep = il_data_rep(yes, _) ->
        io__write_string("#define MR_HIGHLEVEL_DATA\n", !IO)
    ;
        true
    ),

    % XXX We may be able to drop the mercury namespace soon,
    % as there doesn't appear to be any llds generated code
    % in the C# code anymore.
    io__write_string("using mercury;\n\n", !IO),

    globals__io_lookup_bool_option(sign_assembly, SignAssembly, !IO),
    (
        SignAssembly = yes,
        io__write_string("[assembly:System.Reflection." ++
            "AssemblyKeyFileAttribute(\"mercury.sn\")]\n", !IO)
    ;
        SignAssembly = no
    ).
output_language_specific_header_code(managed_cplusplus, ModuleName, Imports,
        !IO) :-
    get_il_data_rep(DataRep, !IO),
    ( DataRep = il_data_rep(yes, _) ->
        io__write_string("#define MR_HIGHLEVEL_DATA\n", !IO)
    ;
        true
    ),

    io__write_string("#using <mscorlib.dll>\n", !IO),

    ( mercury_std_library_module_name(ModuleName) ->
        io__write_strings([
            "#using ""mercury_dotnet.dll""\n",
            "#using ""mercury_il.dll""\n"
            %,
            %"#using ""private_builtin.dll""\n",
            %"#using ""builtin.dll""\n"
            ], !IO)
    ;
        true
    ),

    list__map(
        (pred(Import::in, Result::out) is det :-
            ( Import = mercury_import(_, Name) ->
                ( is_std_lib_module(Name, StdLibName) ->
                    ( mercury_std_library_module_name(ModuleName) ->
                        Str = StdLibName
                    ;
                        Str = "mercury"
                    )
                ;
                    SymName = mlds_module_name_to_sym_name(Name),
                    sym_name_to_string(SymName, ".", Str)
                ),
                Result = [Str]
            ;
                Result = []
            )
        ), Imports, ImportListList),
    ActualImports = remove_dups(condense(ImportListList)),

    list__foldl((pred(I::in, di, uo) is det -->
            io__write_string("#using """),
            io__write_string(I),
            io__write_string(".dll""\n")
        ), ActualImports, !IO),

    sym_name_to_string(ModuleName, ModuleNameStr),
    io__write_strings([
        "#using """, ModuleNameStr, ".dll""\n",
        "#include ""mercury_mcpp.h""\n",

        % XXX We have to use the mercury namespace, as
        % llds_out still generates some of the code used
        % in the MC++ interface, and so it doesn't have
        % "mercury::" namespace qualifiers.
        "using namespace mercury;\n",
        "\n"], !IO),

    globals__io_lookup_bool_option(sign_assembly, SignAssembly, !IO),
    (
        SignAssembly = yes,
        io__write_string("[assembly:System::Reflection::" ++
            "AssemblyKeyFileAttribute(\"mercury.sn\")];\n", !IO)
    ;
        SignAssembly = no
    ).

:- pred generate_foreign_header_code(foreign_language::in(managed_lang),
    module_name::in, mlds__foreign_code::in, io::di, io::uo) is det.

generate_foreign_header_code(Lang, ModuleName, ForeignCode, !IO) :-
    ForeignCode = mlds__foreign_code(RevHeaderCode, RevImports,
        _RevBodyCode, _ExportDefns),
    % Only MC++ can declare which assemblies it refers to in its
    % source file.  C# declares which assemblies it refers to via
    % command line arguments to the C# compiler.
    (
        Lang = managed_cplusplus,
        Imports = list__reverse(RevImports),
        list__foldl(
            (pred(ForeignImport::in, di, uo) is det -->
                module_name_to_search_file_name(
                    foreign_import_module_name(ForeignImport, ModuleName),
                        ".dll", FileName),
                io__write_strings(["#using """, FileName, """\n"])
            ), Imports, !IO)
    ;
        Lang = csharp
    ),

    HeaderCode = list__reverse(RevHeaderCode),
    io__write_list(HeaderCode, "\n",
        % XXX Ignoring _IsLocal may not be the right thing to do.
        (pred(foreign_decl_code(CodeLang, _IsLocal, Code, Context)::in,
                di, uo) is det -->
            output_context(Lang, Context),
            ( { CodeLang = Lang } ->
                io__write_string(Code), io__nl
            ;
                { sorry(this_file, "wrong foreign code") }
            ),
            output_reset_context(Lang)
        ), !IO).

:- pred generate_namespace_details(foreign_language::in(managed_lang),
    ilds__class_name::in, string::out, list(string)::out) is det.

generate_namespace_details(Lang, ClassName, NameSpaceFmtStr, Namespace) :-
    % XXX We should consider what happens if we need to mangle
    % the namespace name.
    (
        Lang = csharp,
        NameExt = "__csharp_code",
        NameSpaceFmtStr = "namespace @%s {"
    ;
        Lang = managed_cplusplus,
        NameExt = "__cpp_code",
        NameSpaceFmtStr = "namespace %s {"
    ),

    Namespace0 = get_class_namespace(ClassName),
    ( list__reverse(Namespace0) = [Head | Tail] ->
        Namespace = list__reverse([Head ++ NameExt | Tail])
    ;
        Namespace = Namespace0
    ).

:- pred generate_foreign_code(foreign_language::in(managed_lang),
    mlds_module_name::in, mlds__foreign_code::in,
    io::di, io::uo) is det.

generate_foreign_code(Lang, _ModuleName, ForeignCode, !IO) :-
    ForeignCode = mlds__foreign_code(_RevHeaderCode, _RevImports,
        RevBodyCode, _ExportDefns),
    BodyCode = list__reverse(RevBodyCode),
    io__write_list(BodyCode, "\n",
        (pred(user_foreign_code(CodeLang, Code, Context)::in,
                di, uo) is det -->
            output_context(Lang, Context),
            ( { Lang = CodeLang } ->
                io__write_string(Code), io__nl
            ;
                { sorry(this_file, "wrong foreign code") }
            ),
            output_reset_context(Lang)
    ), !IO).

:- pred generate_method_code(foreign_language::in(managed_lang),
    mlds_module_name::in, mlds__defn::in, io::di, io::uo) is det.

generate_method_code(_, _, defn(export(_), _, _, _), !IO).
generate_method_code(_, _, defn(data(_), _, _, _), !IO).
generate_method_code(_, _, defn(type(_, _), _, _, _), !IO).
generate_method_code(Lang, _ModuleName, Defn, !IO) :-
    Defn = defn(function(PredLabel, ProcId, MaybeSeqNum, _PredId),
        _Context, _DeclFlags, Entity),
    (
        % XXX we ignore the attributes
        Entity = mlds__function(_, Params, defined_here(Statement),
            _Attributes),
        has_foreign_languages(Statement, Langs),
        list__member(Lang, Langs)
    ->
        get_il_data_rep(DataRep, !IO),
        Params = mlds__func_params(Inputs, Outputs),
        (
            Outputs = [],
            ReturnType = void
        ;
            Outputs = [MLDSReturnType],
            mlds_type_to_ilds_type(DataRep, MLDSReturnType) =
                ilds__type(_, SimpleType),
            ReturnType = simple_type(SimpleType)
        ;
            Outputs = [_, _ | _],
            % C# and MC++ don't support multiple return values
            sorry(this_file, "multiple return values")
        ),

        predlabel_to_id(PredLabel, ProcId, MaybeSeqNum, Id),
        (
            Lang = csharp,
            io__write_string("public static ", !IO)
        ;
            Lang = managed_cplusplus,
            io__write_string("static ", !IO)
        ),
        write_il_ret_type_as_foreign_type(Lang, ReturnType, !IO),

        io__write_string(" ", !IO),

        io__write_string(Id, !IO),
        io__write_string("(", !IO),
        io__write_list(Inputs, ", ",
                write_input_arg_as_foreign_type(Lang), !IO),
        io__write_string(")", !IO),
        io__nl(!IO),

        io__write_string("{\n", !IO),
        write_statement(Lang, Inputs, Statement, !IO),
        io__write_string("}\n", !IO)
    ;
        true
    ).

:- pred write_statement(foreign_language::in(managed_lang),
    mlds__arguments::in, statement::in, io::di, io::uo) is det.

write_statement(Lang, Args, statement(Statement, Context), !IO) :-
    (
        % XXX petdr
        Statement = atomic(outline_foreign_proc(Lang, OutlineArgs,
            _Lvals, Code))
    ->
        list__foldl(write_outline_arg_init(Lang), OutlineArgs, !IO),
        output_context(Lang, get_prog_context(Context), !IO),
        io__write_string(Code, !IO),
        io__nl(!IO),
        output_reset_context(Lang, !IO),
        list__foldl(write_outline_arg_final(Lang), OutlineArgs, !IO)
    ;
        Statement = block(Defns, Statements)
    ->
        io__write_list(Defns, "", write_defn_decl(Lang), !IO),
        io__write_string("{\n", !IO),
        io__write_list(Statements, "", write_statement(Lang, Args), !IO),
        io__write_string("\n}\n", !IO)
    ;
        Statement = return(Rvals)
    ->
        ( Rvals = [Rval] ->
            io__write_string("return ", !IO),
            write_rval(Lang, Rval, !IO),
            io__write_string(";\n", !IO)
        ;
            sorry(this_file, "multiple return values")
        )
    ;
        Statement = atomic(assign(LVal, RVal))
    ->
        write_lval(Lang, LVal, !IO),
        io__write_string(" = ", !IO),
        write_rval(Lang, RVal, !IO),
        io__write_string(";\n", !IO)
    ;
        functor(Statement, SFunctor, _Arity),
        sorry(this_file, "foreign code output for " ++ SFunctor)
    ).

:- pred write_outline_arg_init(foreign_language::in(managed_lang),
    outline_arg::in, io::di, io::uo) is det.

write_outline_arg_init(Lang, in(Type, VarName, Rval), !IO) :-
    write_parameter_type(Lang, Type, !IO),
    io__write_string(" ", !IO),
    io__write_string(VarName, !IO),
    io__write_string(" = ", !IO),
    write_rval(Lang, Rval, !IO),
    io__write_string(";\n", !IO).
write_outline_arg_init(Lang, out(Type, VarName, _Lval), !IO) :-
    write_parameter_type(Lang, Type, !IO),
    io__write_string(" ", !IO),
    io__write_string(VarName, !IO),
    % In C# give output variables a default value to avoid warnings.
    (
        Lang = csharp,
        io__write_string(" = ", !IO),
        write_parameter_initializer(Lang, Type, !IO)
    ;
        Lang = managed_cplusplus
    ),
    io__write_string(";\n", !IO).
write_outline_arg_init(_Lang, unused, !IO).

:- pred write_outline_arg_final(foreign_language::in(managed_lang),
    outline_arg::in, io::di, io::uo) is det.

write_outline_arg_final(_Lang, in(_, _, _), !IO).
write_outline_arg_final(Lang, out(_Type, VarName, Lval), !IO) :-
    write_lval(Lang, Lval, !IO),
    io__write_string(" = ", !IO),
    io__write_string(VarName, !IO),
    io__write_string(";\n", !IO).
write_outline_arg_final(_Lang, unused, !IO).

:- pred write_declare_and_assign_local(foreign_language::in(managed_lang),
    mlds__argument::in, io::di, io::uo) is det.

write_declare_and_assign_local(Lang, argument(Name, Type, _GcCode), !IO) :-
    ( Name = data(var(VarName0)) ->
        VarName = VarName0
    ;
        unexpected(this_file, "not a variable name")
    ),

    % A pointer type is an output type.
    ( Type = mlds__ptr_type(OutputType) ->
        ( is_anonymous_variable(VarName) ->
            true
        ;
            write_parameter_type(Lang, OutputType, !IO),
            io__write_string(" ", !IO),
            write_mlds_var_name_for_local(VarName, !IO),

            % In C# give output types a default value to
            % avoid warnings.
            (
                Lang = csharp,
                io__write_string(" = ", !IO),
                write_parameter_initializer(Lang, OutputType, !IO)
            ;
                Lang = managed_cplusplus
            ),
            io__write_string(";\n", !IO)
        )
    ;
        write_parameter_type(Lang, Type, !IO),
        io__write_string(" ", !IO),
        write_mlds_var_name_for_local(VarName, !IO),
        io__write_string(" = ", !IO),
        write_mlds_var_name_for_parameter(VarName, !IO),
        io__write_string(";\n", !IO)
    ).

:- pred write_assign_local_to_output(foreign_language::in(managed_lang),
    mlds__argument::in, io::di, io::uo) is det.

write_assign_local_to_output(Lang, argument(Name, Type, _GcCode), !IO) :-
    ( Name = data(var(VarName0)) ->
        VarName = VarName0
    ;
        unexpected(this_file, "not a variable name")
    ),

    % A pointer type is an output type.
    (
        Type = mlds__ptr_type(_OutputType),
        not is_anonymous_variable(VarName)
    ->
        (
            Lang = csharp
        ;
            Lang = managed_cplusplus,
            io__write_string("*", !IO)
        ),
        write_mlds_var_name_for_parameter(VarName, !IO),
        io__write_string(" = ", !IO),
        write_mlds_var_name_for_local(VarName, !IO),
        io__write_string(";\n", !IO)
    ;
        true
    ).

:- pred is_anonymous_variable(var_name::in) is semidet.

is_anonymous_variable(var_name(Name, _)) :-
    string__prefix(Name, "_").

%-----------------------------------------------------------------------------%

:- pred output_context(foreign_language::in(managed_lang), prog_context::in,
    io::di, io::uo) is det.

output_context(_Lang, Context, !IO) :-
    term__context_file(Context, File),
    term__context_line(Context, Line),
    c_util__set_line_num(File, Line, !IO).

:- pred output_reset_context(foreign_language::in(managed_lang),
    io::di, io::uo) is det.

output_reset_context(_i, !IO) :-
    c_util__reset_line_num(!IO).

:- pred write_rval(foreign_language::in(managed_lang), mlds_rval::in,
    io::di, io::uo) is det.

write_rval(Lang, lval(Lval), !IO) :-
    write_lval(Lang, Lval, !IO).
write_rval(_Lang, mkword(_Tag, _Rval), !IO) :-
    sorry(this_file, "mkword rval").
write_rval(Lang, const(RvalConst), !IO) :-
    write_rval_const(Lang, RvalConst, !IO).
write_rval(Lang, unop(Unop, Rval), !IO) :-
    (
        Unop = std_unop(StdUnop),
        c_util__unary_prefix_op(StdUnop, UnopStr)
    ->
        io__write_string(UnopStr, !IO),
        io__write_string("(", !IO),
        write_rval(Lang, Rval, !IO),
        io__write_string(")", !IO)
    ;
        Unop = cast(Type)
    ->
        io__write_string("(", !IO),
        write_parameter_type(Lang, Type, !IO),
        io__write_string(") ", !IO),
        write_rval(Lang, Rval, !IO)
    ;
        sorry(this_file, "box or unbox unop")
    ).
write_rval(Lang, binop(Binop, Rval1, Rval2), !IO) :-
    ( c_util__binary_infix_op(Binop, BinopStr) ->
        io__write_string("(", !IO),
        write_rval(Lang, Rval1, !IO),
        io__write_string(") ", !IO),
        io__write_string(BinopStr, !IO),
        io__write_string(" (", !IO),
        write_rval(Lang, Rval2, !IO),
        io__write_string(")", !IO)
    ;
        sorry(this_file, "binop rval")
    ).
write_rval(_Lang, mem_addr(_), !IO) :-
    sorry(this_file, "mem_addr rval").
write_rval(_Lang, self(_), !IO) :-
    sorry(this_file, "self rval").

:- pred write_rval_const(foreign_language::in(managed_lang),
    mlds_rval_const::in, io::di, io::uo) is det.

write_rval_const(_Lang, true, !IO) :-
    io__write_string("1", !IO).
write_rval_const(_Lang, false, !IO) :-
    io__write_string("0", !IO).
write_rval_const(_Lang, int_const(I), !IO) :-
    io__write_int(I, !IO).
write_rval_const(_Lang, float_const(F), !IO) :-
    io__write_float(F, !IO).
    % XXX We don't quote this correctly.
write_rval_const(_Lang, string_const(S), !IO) :-
    io__write_string("""", !IO),
    c_util__output_quoted_string(S, !IO),
    io__write_string("""", !IO).
write_rval_const(_Lang, multi_string_const(L, S), !IO) :-
    io__write_string("""", !IO),
    c_util__output_quoted_multi_string(L, S, !IO),
    io__write_string("""", !IO).
write_rval_const(Lang, code_addr_const(CodeAddrConst), !IO) :-
    (
        CodeAddrConst = proc(ProcLabel, _FuncSignature),
        mangle_mlds_proc_label(ProcLabel, no, ClassName, MangledName),
        write_class_name(Lang, ClassName, !IO),
        write_field_selector(Lang, !IO),
        io__write_string(MangledName, !IO)
    ;
        CodeAddrConst = internal(ProcLabel, SeqNum, _FuncSignature),
        mangle_mlds_proc_label(ProcLabel, yes(SeqNum), ClassName, MangledName),
        write_class_name(Lang, ClassName, !IO),
        write_field_selector(Lang, !IO),
        io__write_string(MangledName, !IO)
    ).
write_rval_const(_Lang, data_addr_const(_), !IO) :-
    sorry(this_file, "data_addr_const rval").
write_rval_const(Lang, null(_), !IO) :-
    (
        Lang = csharp,
        io__write_string("null", !IO)
    ;
        Lang = managed_cplusplus,
        io__write_string("NULL", !IO)
    ).

:- pred write_lval(foreign_language::in(managed_lang), mlds_lval::in,
    io::di, io::uo) is det.

write_lval(Lang, field(_, Rval, named_field(FieldId, _Type), _, _), !IO) :-
    io__write_string("(", !IO),
    write_rval(Lang, Rval, !IO),
    io__write_string(")", !IO),
    write_field_selector(Lang, !IO),
    FieldId = qual(_, _, FieldName),
    io__write_string(FieldName, !IO).
write_lval(Lang, field(_, Rval, offset(OffSet), _, _), !IO) :-
    io__write_string("(", !IO),
    write_rval(Lang, Rval, !IO),
    io__write_string(")", !IO),
    io__write_string("[", !IO),
    write_rval(Lang, OffSet, !IO),
    io__write_string("]", !IO).
write_lval(Lang, mem_ref(Rval, _), !IO) :-
    (
        Lang = managed_cplusplus,
        io__write_string("*", !IO)
    ;
        Lang = csharp
    ),
    write_rval(Lang, Rval, !IO).
write_lval(_Lang, var(Var, _VarType), !IO) :-
    Var = qual(_, _, VarName),
    write_mlds_var_name_for_parameter(VarName, !IO).

:- pred write_field_selector(foreign_language::in(managed_lang),
    io::di, io::uo) is det.

write_field_selector(csharp, !IO) :-
    io__write_string(".", !IO).
write_field_selector(managed_cplusplus, !IO) :-
    io__write_string("->", !IO).

:- pred write_defn_decl(foreign_language::in(managed_lang), mlds__defn::in,
    io::di, io::uo) is det.

write_defn_decl(Lang, Defn, !IO) :-
    Defn = mlds__defn(Name, _Context, _Flags, DefnBody),
    (
        DefnBody = data(Type, _Initializer, _GC_TraceCode),
        Name = data(var(VarName))
    ->
        write_parameter_type(Lang, Type, !IO),
        io__write_string(" ", !IO),
        write_mlds_var_name_for_parameter(VarName, !IO),
        io__write_string(";\n", !IO)
    ;
        % XXX We should implement others.
        sorry(this_file, "data_addr_const rval")
    ).

:- pred write_parameter_type(foreign_language::in(managed_lang),
    mlds_type::in, io::di, io::uo) is det.

write_parameter_type(Lang, Type, !IO) :-
    get_il_data_rep(DataRep, !IO),
    ILType = mlds_type_to_ilds_type(DataRep, Type),
    write_il_type_as_foreign_type(Lang, ILType, !IO).

:- pred write_input_arg_as_foreign_type(foreign_language::in(managed_lang),
    mlds__argument::in, io::di, io::uo) is det.

write_input_arg_as_foreign_type(Lang, Arg, !IO) :-
    Arg = mlds__argument(EntityName, Type, _GC_TraceCode),
    get_il_data_rep(DataRep, !IO),
    write_il_type_as_foreign_type(Lang, mlds_type_to_ilds_type(DataRep, Type),
        !IO),
    io__write_string(" ", !IO),
    ( EntityName = data(var(VarName)) ->
        write_mlds_var_name_for_parameter(VarName, !IO)
    ;
        error("found a variable in a list")
    ).

:- pred write_parameter_initializer(foreign_language::in(managed_lang),
    mlds_type::in, io::di, io::uo) is det.

write_parameter_initializer(managed_cplusplus, _Type, !IO) :-
    unexpected(this_file, "initializer for MC++").
write_parameter_initializer(csharp, Type, !IO) :-
    get_il_data_rep(DataRep, !IO),
    ILType = mlds_type_to_ilds_type(DataRep, Type),
    ILType = type(_, ILSimpleType),
    write_csharp_initializer(ILSimpleType, !IO).

:- pred write_il_ret_type_as_foreign_type(foreign_language::in(managed_lang),
    ret_type::in, io::di, io::uo) is det.

write_il_ret_type_as_foreign_type(_Lang, void, !IO) :-
    io__write_string("void", !IO).
write_il_ret_type_as_foreign_type(Lang, simple_type(T), !IO) :-
    write_il_simple_type_as_foreign_type(Lang, T, !IO).

:- pred write_il_type_as_foreign_type(foreign_language::in(managed_lang),
    ilds__type::in, io::di, io::uo) is det.

write_il_type_as_foreign_type(Lang, ilds__type(Modifiers, SimpleType), !IO) :-
    io__write_list(Modifiers, " ",
        write_il_type_modifier_as_foreign_type(Lang), !IO),
    write_il_simple_type_as_foreign_type(Lang, SimpleType, !IO).

:- pred write_il_type_modifier_as_foreign_type(
    foreign_language::in(managed_lang), ilds__type_modifier::in,
    io::di, io::uo) is det.

write_il_type_modifier_as_foreign_type(_Lang, const, !IO) :-
    io__write_string("const", !IO).
write_il_type_modifier_as_foreign_type(_Lang, readonly, !IO) :-
    io__write_string("readonly", !IO).
write_il_type_modifier_as_foreign_type(_Lang, volatile, !IO) :-
    io__write_string("volatile", !IO).

    % XXX Need to revisit this and choose types appropriately.
    %
:- pred write_il_simple_type_as_foreign_type(
    foreign_language::in(managed_lang),
    simple_type::in, io::di, io::uo) is det.

write_il_simple_type_as_foreign_type(csharp, Type, !IO) :-
    write_il_simple_type_as_foreign_type_cs(Type, !IO).
write_il_simple_type_as_foreign_type(managed_cplusplus, Type, !IO) :-
    write_il_simple_type_as_foreign_type_mcpp(Type, !IO).

:- pred write_il_simple_type_as_foreign_type_cs(
    simple_type::in, io::di, io::uo) is det.

write_il_simple_type_as_foreign_type_cs(int8, !IO) :-
    io__write_string("sbyte", !IO).
write_il_simple_type_as_foreign_type_cs(int16, !IO) :-
    io__write_string("short", !IO).
write_il_simple_type_as_foreign_type_cs(int32, !IO) :-
    io__write_string("int", !IO).
write_il_simple_type_as_foreign_type_cs(int64, !IO) :-
    io__write_string("long", !IO).
write_il_simple_type_as_foreign_type_cs(uint8, !IO) :-
    io__write_string("byte", !IO).
write_il_simple_type_as_foreign_type_cs(uint16, !IO) :-
    io__write_string("ushort", !IO).
write_il_simple_type_as_foreign_type_cs(uint32, !IO) :-
    io__write_string("uint", !IO).
write_il_simple_type_as_foreign_type_cs(uint64, !IO) :-
    io__write_string("ulong", !IO).
write_il_simple_type_as_foreign_type_cs(native_int, !IO) :-
    io__write_string("int", !IO).
write_il_simple_type_as_foreign_type_cs(native_uint, !IO) :-
    io__write_string("uint", !IO).
write_il_simple_type_as_foreign_type_cs(float32, !IO) :-
    io__write_string("float", !IO).
write_il_simple_type_as_foreign_type_cs(float64, !IO) :-
    io__write_string("double", !IO).
write_il_simple_type_as_foreign_type_cs(native_float, !IO) :-
    io__write_string("float", !IO).
write_il_simple_type_as_foreign_type_cs(bool, !IO) :-
    io__write_string("bool", !IO).
write_il_simple_type_as_foreign_type_cs(char, !IO) :-
    io__write_string("char", !IO).
write_il_simple_type_as_foreign_type_cs(string, !IO) :-
    io__write_string("string", !IO).
write_il_simple_type_as_foreign_type_cs(object, !IO) :-
    io__write_string("object", !IO).
write_il_simple_type_as_foreign_type_cs(refany, !IO) :-
    io__write_string("mercury.MR_RefAny", !IO).
write_il_simple_type_as_foreign_type_cs(class(ClassName), !IO) :-
    write_class_name(csharp, ClassName, !IO).
write_il_simple_type_as_foreign_type_cs(valuetype(ClassName), !IO) :-
    write_class_name(csharp, ClassName, !IO).
write_il_simple_type_as_foreign_type_cs(interface(_ClassName), !IO) :-
    sorry(this_file, "interfaces").
write_il_simple_type_as_foreign_type_cs('[]'(Type, Bounds), !IO) :-
    write_il_type_as_foreign_type(csharp, Type, !IO),
    io__write_string("[]", !IO),
    (
        Bounds = []
    ;
        Bounds = [_ | _],
        sorry(this_file, "arrays with bounds")
    ).
write_il_simple_type_as_foreign_type_cs('&'(Type), !IO) :-
    % XXX Is this always right?
    io__write_string("ref ", !IO),
    write_il_type_as_foreign_type(csharp, Type, !IO).
write_il_simple_type_as_foreign_type_cs('*'(Type), !IO) :-
    write_il_type_as_foreign_type(csharp, Type, !IO),
    io__write_string(" *", !IO).

:- pred write_il_simple_type_as_foreign_type_mcpp(
    simple_type::in, io::di, io::uo) is det.

write_il_simple_type_as_foreign_type_mcpp(int8, !IO) :-
    io__write_string("mercury::MR_Integer8", !IO).
write_il_simple_type_as_foreign_type_mcpp(int16, !IO) :-
    io__write_string("mercury::MR_Integer16", !IO).
write_il_simple_type_as_foreign_type_mcpp(int32, !IO) :-
    io__write_string("mercury::MR_Integer", !IO).
write_il_simple_type_as_foreign_type_mcpp(int64, !IO) :-
    io__write_string("mercury::MR_Integer64", !IO).
write_il_simple_type_as_foreign_type_mcpp(uint8, !IO) :-
    io__write_string("unsigned int", !IO).
write_il_simple_type_as_foreign_type_mcpp(uint16, !IO) :-
    io__write_string("unsigned int", !IO).
write_il_simple_type_as_foreign_type_mcpp(uint32, !IO) :-
    io__write_string("unsigned int", !IO).
write_il_simple_type_as_foreign_type_mcpp(uint64, !IO) :-
    io__write_string("unsigned int", !IO).
write_il_simple_type_as_foreign_type_mcpp(native_int, !IO) :-
    io__write_string("mercury::MR_Integer", !IO).
write_il_simple_type_as_foreign_type_mcpp(native_uint, !IO) :-
    io__write_string("unsigned int", !IO).
write_il_simple_type_as_foreign_type_mcpp(float32, !IO) :-
    io__write_string("float", !IO).
write_il_simple_type_as_foreign_type_mcpp(float64, !IO) :-
    io__write_string("mercury::MR_Float", !IO).
write_il_simple_type_as_foreign_type_mcpp(native_float, !IO) :-
    io__write_string("mercury::MR_Float", !IO).
write_il_simple_type_as_foreign_type_mcpp(bool, !IO) :-
    io__write_string("mercury::MR_Bool", !IO).
write_il_simple_type_as_foreign_type_mcpp(char, !IO) :-
    io__write_string("mercury::MR_Char", !IO).
write_il_simple_type_as_foreign_type_mcpp(string, !IO) :-
    io__write_string("mercury::MR_String", !IO).
write_il_simple_type_as_foreign_type_mcpp(object, !IO) :-
    io__write_string("mercury::MR_Box", !IO).
write_il_simple_type_as_foreign_type_mcpp(refany, !IO) :-
    io__write_string("mercury::MR_RefAny", !IO).
write_il_simple_type_as_foreign_type_mcpp(class(ClassName), !IO) :-
    ( ClassName = il_generic_class_name ->
        io__write_string("mercury::MR_Box", !IO)
    ;
        io__write_string("public class ", !IO),
        write_class_name(managed_cplusplus, ClassName, !IO),
        io__write_string(" *", !IO)
    ).
write_il_simple_type_as_foreign_type_mcpp(valuetype(ClassName), !IO) :-
    io__write_string("__value class ", !IO),
    write_class_name(managed_cplusplus, ClassName, !IO).
        % XXX this is not the right syntax
write_il_simple_type_as_foreign_type_mcpp(interface(ClassName), !IO) :-
    io__write_string("interface ", !IO),
    write_class_name(managed_cplusplus, ClassName, !IO),
    io__write_string(" *", !IO).
        % XXX this needs more work
write_il_simple_type_as_foreign_type_mcpp('[]'(_Type, _Bounds), !IO) :-
    io__write_string("mercury::MR_Word", !IO).
write_il_simple_type_as_foreign_type_mcpp('&'(Type), !IO) :-
    io__write_string("MR_Ref(", !IO),
    write_il_type_as_foreign_type(managed_cplusplus, Type, !IO),
    io__write_string(")", !IO).
write_il_simple_type_as_foreign_type_mcpp('*'(Type), !IO) :-
    write_il_type_as_foreign_type(managed_cplusplus, Type, !IO),
    io__write_string(" *", !IO).

:- pred write_csharp_initializer(simple_type::in, io::di, io::uo) is det.

write_csharp_initializer(int8, !IO) :-
    io__write_string("0", !IO).
write_csharp_initializer(int16, !IO) :-
    io__write_string("0", !IO).
write_csharp_initializer(int32, !IO) :-
    io__write_string("0", !IO).
write_csharp_initializer(int64, !IO) :-
    io__write_string("0", !IO).
write_csharp_initializer(uint8, !IO) :-
    io__write_string("0", !IO).
write_csharp_initializer(uint16, !IO) :-
    io__write_string("0", !IO).
write_csharp_initializer(uint32, !IO) :-
    io__write_string("0", !IO).
write_csharp_initializer(uint64, !IO) :-
    io__write_string("0", !IO).
write_csharp_initializer(native_int, !IO) :-
    io__write_string("0", !IO).
write_csharp_initializer(native_uint, !IO) :-
    io__write_string("0", !IO).
write_csharp_initializer(float32, !IO) :-
    io__write_string("0.0", !IO).
write_csharp_initializer(float64, !IO) :-
    io__write_string("0.0", !IO).
write_csharp_initializer(native_float, !IO) :-
    io__write_string("0.0", !IO).
write_csharp_initializer(bool, !IO) :-
    io__write_string("false", !IO).
write_csharp_initializer(char, !IO) :-
    io__write_string("'\\0'", !IO).
write_csharp_initializer(string, !IO) :-
    io__write_string("null", !IO).
write_csharp_initializer(object, !IO) :-
    io__write_string("null", !IO).
write_csharp_initializer(refany, !IO) :-
    io__write_string("null", !IO).
write_csharp_initializer(class(_ClassName), !IO) :-
    io__write_string("null", !IO).
write_csharp_initializer(interface(_ClassName), !IO) :-
    io__write_string("null", !IO).
write_csharp_initializer('[]'(_Type, _Bounds), !IO) :-
    io__write_string("null", !IO).
write_csharp_initializer('&'(_Type), !IO) :-
    io__write_string("null", !IO).
write_csharp_initializer('*'(_Type), !IO) :-
    io__write_string("null", !IO).
write_csharp_initializer(valuetype(ClassName), !IO) :-
    io__write_string("new ", !IO),
    write_class_name(csharp, ClassName, !IO),
    io__write_string("()", !IO).

:- pred write_class_name(foreign_language::in(managed_lang),
    structured_name::in, io::di, io::uo) is det.

write_class_name(Lang, structured_name(_Asm, DottedName, NestedClasses),
        !IO) :-
    (
        Lang = csharp,
        Sep = "."
    ;
        Lang = managed_cplusplus,
        Sep = "::"
    ),
    io__write_list(DottedName ++ NestedClasses, Sep, io__write_string, !IO).

:- pred write_mlds_var_name_for_local(mlds__var_name::in,
    io::di, io::uo) is det.

write_mlds_var_name_for_local(var_name(Name, _MaybeNum), !IO) :-
    io__write_string(Name, !IO).

:- pred write_mlds_var_name_for_parameter(mlds__var_name::in,
    io::di, io::uo) is det.

write_mlds_var_name_for_parameter(var_name(Name, MaybeNum), !IO) :-
    io__write_string(Name, !IO),
    (
        MaybeNum = yes(Num),
        io__write_string("_", !IO),
        io__write_int(Num, !IO)
    ;
        MaybeNum = no
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "mlds_to_managed.m".

%-----------------------------------------------------------------------------%
