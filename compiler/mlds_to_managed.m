%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: mlds_to_managed.m.
% Main author: trd, petdr.
%
% Generate code for the foreign language interface to C#.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.mlds_to_managed.
:- interface.

:- import_module libs.globals.
:- import_module ml_backend.mlds.

:- import_module io.

%-----------------------------------------------------------------------------%

    % Convert the MLDS to C# and write it to a file.
    %
:- pred output_csharp_code(globals::in, mlds::in, io::di, io::uo) is det.

    % Print the header comments of the output module.
    %
:- pred output_src_start(mercury_module_name::in, io::di, io::uo) is det.

    % Print the footer commments of the output module.
    %
:- pred output_src_end(mercury_module_name::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.c_util.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.ilds.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_to_il.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.

:- import_module bool.
:- import_module deconstruct.
:- import_module library.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

output_csharp_code(Globals, MLDS, !IO) :-
    MLDS = mlds(ModuleName, AllForeignCode, _Imports, GlobalData, Defns0,
        _InitPreds, _FinalPreds, _ExportedEnums),
    ml_global_data_get_all_global_defns(GlobalData,
        ScalarCellGroupMap, VectorCellGroupMap, _AllocIdMap, GlobalDefns),
    expect(map.is_empty(ScalarCellGroupMap), $module, $pred,
        "nonempty ScalarCellGroupMap"),
    expect(map.is_empty(VectorCellGroupMap), $module, $pred,
        "nonempty VectorCellGroupMap"),
    Defns = GlobalDefns ++ Defns0,

    output_src_start(ModuleName, !IO),

    ClassName = class_name(mercury_module_name_to_mlds(ModuleName),
        wrapper_class_name),

    output_csharp_header_code(Globals, !IO),

    % Get the foreign code for the required language.
    ForeignCode = map.lookup(AllForeignCode, lang_csharp),
    generate_foreign_header_code(Globals, ForeignCode, !IO),

    % Output the namespace.
    generate_namespace_details(ClassName, NameSpaceFmtStr, Namespace),
    io.write_list(Namespace, "\n",
        (pred(N::in, !.IO::di, !:IO::uo) is det :-
            io.format(NameSpaceFmtStr, [s(N)], !IO)
    ), !IO),

    io.write_strings(["\npublic class " ++ wrapper_class_name, "{\n"], !IO),

    % Output the contents of pragma foreign_code declarations.
    generate_foreign_code(Globals, ForeignCode, !IO),

    io.nl(!IO),

    % Output the contents of foreign_proc declarations.
    % Put each one inside a method.
    get_il_data_rep(Globals, DataRep),
    list.foldl(generate_method_code(Globals, DataRep), Defns, !IO),

    io.write_string("};\n", !IO),

    % Close the namespace braces.
    io.write_list(Namespace, "\n",
        (pred(_N::in, !.IO::di, !:IO::uo) is det :-
            io.write_string("}", !IO)
    ), !IO),
    io.nl(!IO),

    output_src_end(ModuleName, !IO).

%-----------------------------------------------------------------------------%

output_src_start(ModuleName, !IO) :-
    library.version(Version, Fullarch),
    io.write_strings(
        ["//\n// Automatically generated from `",
        sym_name_to_string(ModuleName),
        ".m' by the\n",
        "// Mercury compiler, version ", Version, ",\n",
        "// configured for ", Fullarch, ".\n",
        "// Do not edit.\n",
        "\n\n"], !IO).

output_src_end(ModuleName, !IO) :-
    io.write_string("// End of module: ", !IO),
    prog_out.write_sym_name(ModuleName, !IO),
    io.write_string(". \n", !IO).

%-----------------------------------------------------------------------------%

:- pred output_csharp_header_code(globals::in, io::di, io::uo) is det.

output_csharp_header_code(Globals, !IO) :-
    get_il_data_rep(Globals, DataRep),
    ( DataRep = il_data_rep(yes, _) ->
        io.write_string("#define MR_HIGHLEVEL_DATA\n", !IO)
    ;
        true
    ),

    % XXX We may be able to drop the mercury namespace soon,
    % as there doesn't appear to be any llds generated code
    % in the C# code anymore.
    io.write_string("using mercury;\n\n", !IO),

    globals.lookup_bool_option(Globals, il_sign_assembly, SignAssembly),
    (
        SignAssembly = yes,
        io.write_string("[assembly:System.Reflection." ++
            "AssemblyKeyFileAttribute(\"mercury.sn\")]\n", !IO)
    ;
        SignAssembly = no
    ).

:- pred generate_foreign_header_code(globals::in, mlds_foreign_code::in,
    io::di, io::uo) is det.

generate_foreign_header_code(Globals, ForeignCode, !IO) :-
    ForeignCode = mlds_foreign_code(RevHeaderCode, _RevImports,
        _RevBodyCode, _ExportDefns),

    HeaderCode = list.reverse(RevHeaderCode),
    io.write_list(HeaderCode, "\n",
        % XXX Ignoring _IsLocal may not be the right thing to do.
        (pred(foreign_decl_code(CodeLang, _IsLocal, Code, Context)::in,
                !.IO::di, !:IO::uo) is det :-
            output_context(Globals, Context, !IO),
            ( CodeLang = lang_csharp ->
                io.write_string(Code, !IO),
                io.nl(!IO)
            ;
                sorry($module, $pred, "wrong foreign code")
            ),
            output_reset_context(Globals, !IO)
        ), !IO).

:- pred generate_namespace_details(ilds.class_name::in, string::out,
    list(string)::out) is det.

generate_namespace_details(ClassName, NameSpaceFmtStr, Namespace) :-
    % XXX We should consider what happens if we need to mangle
    % the namespace name.
    %
    % XXX Generating the left brace here and the right brace somewhere else
    % seems bad design. -zs
    NameSpaceFmtStr = "namespace @%s {",

    Namespace0 = get_class_namespace(ClassName),
    ( list.reverse(Namespace0) = [Head | Tail] ->
        Namespace = list.reverse([Head ++ "__csharp_code" | Tail])
    ;
        Namespace = Namespace0
    ).

:- pred generate_foreign_code(globals::in, mlds_foreign_code::in,
    io::di, io::uo) is det.

generate_foreign_code(Globals, ForeignCode, !IO) :-
    ForeignCode = mlds_foreign_code(_RevHeaderCode, _RevImports,
        RevBodyCode, _ExportDefns),
    BodyCode = list.reverse(RevBodyCode),
    io.write_list(BodyCode, "\n",
        (pred(user_foreign_code(CodeLang, Code, Context)::in,
                !.IO::di, !:IO::uo) is det :-
            output_context(Globals, Context, !IO),
            ( CodeLang = lang_csharp ->
                io.write_string(Code, !IO),
                io.nl(!IO)
            ;
                sorry($module, $pred, "wrong foreign code")
            ),
            output_reset_context(Globals, !IO)
    ), !IO).

:- pred generate_method_code(globals::in, il_data_rep::in, mlds_defn::in,
    io::di, io::uo) is det.

generate_method_code(Globals, DataRep, Defn, !IO) :-
    Defn = mlds_defn(EntityName, _Context, _DeclFlags, Entity),
    (
        ( EntityName = entity_export(_)
        ; EntityName = entity_data(_)
        ; EntityName = entity_type(_, _)
        )
    ;
        EntityName = entity_function(PredLabel, ProcId, MaybeSeqNum, _PredId),
        (
            % XXX we ignore the attributes
            Entity = mlds_function(_, Params, body_defined_here(Statement),
                _Attributes, EnvVarNames),
            has_foreign_languages(Statement, Langs),
            list.member(lang_csharp, Langs)
        ->
            expect(set.empty(EnvVarNames), $module, $pred, "EnvVarNames"),
            Params = mlds_func_params(Inputs, Outputs),
            (
                Outputs = [],
                ReturnType = void
            ;
                Outputs = [MLDSReturnType],
                mlds_type_to_ilds_type(DataRep, MLDSReturnType) =
                    il_type(_, SimpleType),
                ReturnType = simple_type(SimpleType)
            ;
                Outputs = [_, _ | _],
                % C# doesn't support multiple return values
                sorry($module, $pred, "multiple return values")
            ),

            predlabel_to_csharp_id(PredLabel, ProcId, MaybeSeqNum, Id),
            io.write_string("public static ", !IO),
            write_il_ret_type_as_foreign_type(ReturnType, !IO),

            io.write_string(" ", !IO),

            io.write_string(Id, !IO),
            io.write_string("(", !IO),
            io.write_list(Inputs, ", ",
                write_input_arg_as_foreign_type(DataRep), !IO),
            io.write_string(")", !IO),
            io.nl(!IO),

            io.write_string("{\n", !IO),
            write_statement(Globals, DataRep, Inputs, Statement, !IO),
            io.write_string("}\n", !IO)
        ;
            true
        )
    ).

:- pred write_statement(globals::in, il_data_rep::in, mlds_arguments::in,
    statement::in, io::di, io::uo) is det.

write_statement(Globals, DataRep, Args, statement(Statement, Context), !IO) :-
    (
        % XXX petdr
        Statement = ml_stmt_atomic(ForeignProc),
        ForeignProc = outline_foreign_proc(lang_csharp, OutlineArgs, _, Code)
    ->
        list.foldl(write_outline_arg_init(DataRep), OutlineArgs, !IO),
        output_context(Globals, mlds_get_prog_context(Context), !IO),
        io.write_string(Code, !IO),
        io.nl(!IO),
        output_reset_context(Globals, !IO),
        list.foldl(write_outline_arg_final(DataRep), OutlineArgs, !IO)
    ;
        Statement = ml_stmt_block(Defns, Statements)
    ->
        io.write_list(Defns, "", write_defn_decl(DataRep), !IO),
        io.write_string("{\n", !IO),
        io.write_list(Statements, "", write_statement(Globals, DataRep, Args),
            !IO),
        io.write_string("\n}\n", !IO)
    ;
        Statement = ml_stmt_return(Rvals)
    ->
        ( Rvals = [Rval] ->
            io.write_string("return ", !IO),
            write_rval(DataRep, Rval, !IO),
            io.write_string(";\n", !IO)
        ;
            sorry($module, $pred, "multiple return values")
        )
    ;
        Statement = ml_stmt_atomic(assign(LVal, RVal))
    ->
        write_lval(DataRep, LVal, !IO),
        io.write_string(" = ", !IO),
        write_rval(DataRep, RVal, !IO),
        io.write_string(";\n", !IO)
    ;
        functor(Statement, canonicalize, SFunctor, _Arity),
        sorry($module, $pred, "foreign code output for " ++ SFunctor)
    ).

:- pred write_outline_arg_init(il_data_rep::in, outline_arg::in,
    io::di, io::uo) is det.

write_outline_arg_init(DataRep, OutlineArg, !IO) :-
    (
        OutlineArg = ola_in(Type, VarName, Rval),
        write_parameter_type(DataRep, Type, !IO),
        io.write_string(" ", !IO),
        io.write_string(VarName, !IO),
        io.write_string(" = ", !IO),
        write_rval(DataRep, Rval, !IO),
        io.write_string(";\n", !IO)
    ;
        OutlineArg = ola_out(Type, VarName, _Lval),
        write_parameter_type(DataRep, Type, !IO),
        io.write_string(" ", !IO),
        io.write_string(VarName, !IO),
        % In C# give output variables a default value to avoid warnings.
        io.write_string(" = ", !IO),
        write_parameter_initializer(DataRep, Type, !IO),
        io.write_string(";\n", !IO)
    ;
        OutlineArg = ola_unused
    ).

:- pred write_outline_arg_final(il_data_rep::in, outline_arg::in,
    io::di, io::uo) is det.

write_outline_arg_final(DataRep, OutlineArg, !IO) :-
    (
        OutlineArg = ola_in(_, _, _)
    ;
        OutlineArg = ola_out(_Type, VarName, Lval),
        write_lval(DataRep, Lval, !IO),
        io.write_string(" = ", !IO),
        io.write_string(VarName, !IO),
        io.write_string(";\n", !IO)
    ;
        OutlineArg = ola_unused
    ).

:- pred write_assign_local_to_output(mlds_argument::in, io::di, io::uo) is det.

write_assign_local_to_output(mlds_argument(Name, Type, _GcCode), !IO) :-
    ( Name = entity_data(mlds_data_var(VarName0)) ->
        VarName = VarName0
    ;
        unexpected($module, $pred, "not a variable name")
    ),

    % A pointer type is an output type.
    (
        Type = mlds_ptr_type(_OutputType),
        not is_anonymous_variable(VarName)
    ->
        write_mlds_var_name_for_parameter(VarName, !IO),
        io.write_string(" = ", !IO),
        write_mlds_var_name_for_local(VarName, !IO),
        io.write_string(";\n", !IO)
    ;
        true
    ).

:- pred is_anonymous_variable(mlds_var_name::in) is semidet.

is_anonymous_variable(mlds_var_name(Name, _)) :-
    string.prefix(Name, "_").

%-----------------------------------------------------------------------------%

:- pred output_context(globals::in, prog_context::in, io::di, io::uo) is det.

output_context(Globals, Context, !IO) :-
    term.context_file(Context, File),
    term.context_line(Context, Line),
    c_util.set_line_num(Globals, File, Line, !IO).

:- pred output_reset_context(globals::in, io::di, io::uo) is det.

output_reset_context(Globals, !IO) :-
    c_util.reset_line_num(Globals, !IO).

:- pred write_rval(il_data_rep::in, mlds_rval::in, io::di, io::uo) is det.

write_rval(DataRep, Rval, !IO) :-
    (
        Rval = ml_lval(Lval),
        write_lval(DataRep, Lval, !IO)
    ;
        Rval = ml_mkword(_Tag, _Rval),
        sorry($module, $pred, "mkword rval")
    ;
        Rval = ml_const(RvalConst),
        write_rval_const(RvalConst, !IO)
    ;
        Rval = ml_unop(Unop, RvalA),
        (
            Unop = std_unop(StdUnop),
            c_util.unary_prefix_op(StdUnop, UnopStr)
        ->
            io.write_string(UnopStr, !IO),
            io.write_string("(", !IO),
            write_rval(DataRep, RvalA, !IO),
            io.write_string(")", !IO)
        ;
            Unop = cast(Type)
        ->
            io.write_string("(", !IO),
            write_parameter_type(DataRep, Type, !IO),
            io.write_string(") ", !IO),
            write_rval(DataRep, RvalA, !IO)
        ;
            sorry($module, $pred, "box or unbox unop")
        )
    ;
        Rval = ml_binop(Binop, RvalA, RvalB),
        c_util.binop_category_string(Binop, Category, BinopStr),
        ( Category = int_or_bool_binary_infix_binop ->
            io.write_string("(", !IO),
            write_rval(DataRep, RvalA, !IO),
            io.write_string(") ", !IO),
            io.write_string(BinopStr, !IO),
            io.write_string(" (", !IO),
            write_rval(DataRep, RvalB, !IO),
            io.write_string(")", !IO)
        ;
            sorry($module, $pred, "binop rval")
        )
    ;
        Rval = ml_scalar_common(_),
        sorry($module, $pred, "scalar_common rval")
    ;
        Rval = ml_vector_common_row(_, _),
        sorry($module, $pred, "vector_common_row rval")
    ;
        Rval = ml_mem_addr(_),
        sorry($module, $pred, "mem_addr rval")
    ;
        Rval = ml_self(_),
        sorry($module, $pred, "self rval")
    ).

:- pred write_rval_const(mlds_rval_const::in, io::di, io::uo) is det.

write_rval_const(mlconst_true, !IO) :-
    io.write_string("1", !IO).
write_rval_const(mlconst_false, !IO) :-
    io.write_string("0", !IO).
write_rval_const(Const, !IO) :-
    ( Const = mlconst_int(I)
    ; Const = mlconst_enum(I, _)
    ; Const = mlconst_char(I)
    ),
    io.write_int(I, !IO).
write_rval_const(mlconst_foreign(_Lang, _Value, _Type), !IO) :-
    sorry($module, $pred, "mlconst_foreign for managed languages").
write_rval_const(mlconst_float(F), !IO) :-
    io.write_float(F, !IO).
    % XXX We don't quote this correctly.
write_rval_const(mlconst_string(S), !IO) :-
    io.write_string("""", !IO),
    c_util.output_quoted_string(S, !IO),
    io.write_string("""", !IO).
write_rval_const(mlconst_multi_string(S), !IO) :-
    io.write_string("""", !IO),
    c_util.output_quoted_multi_string(S, !IO),
    io.write_string("""", !IO).
write_rval_const(mlconst_named_const(NamedConst), !IO) :-
    io.write_string(NamedConst, !IO).
write_rval_const(mlconst_code_addr(CodeAddrConst), !IO) :-
    (
        CodeAddrConst = code_addr_proc(ProcLabel, _FuncSignature),
        mangle_mlds_proc_label(ProcLabel, no, ClassName, MangledName),
        write_class_name(ClassName, !IO),
        io.write_string(".", !IO),
        io.write_string(MangledName, !IO)
    ;
        CodeAddrConst = code_addr_internal(ProcLabel, SeqNum, _FuncSignature),
        mangle_mlds_proc_label(ProcLabel, yes(SeqNum), ClassName, MangledName),
        write_class_name(ClassName, !IO),
        io.write_string(".", !IO),
        io.write_string(MangledName, !IO)
    ).
write_rval_const(mlconst_data_addr(_), !IO) :-
    sorry($module, $pred, "data_addr_const rval").
write_rval_const(mlconst_null(_), !IO) :-
    io.write_string("null", !IO).

:- pred write_lval(il_data_rep::in, mlds_lval::in, io::di, io::uo) is det.

write_lval(DataRep, Lval, !IO) :-
    (
        Lval = ml_field(_, Rval, FieldId, _, _),
        (
            FieldId = ml_field_offset(OffSet),
            io.write_string("(", !IO),
            write_rval(DataRep, Rval, !IO),
            io.write_string(")", !IO),
            io.write_string("[", !IO),
            write_rval(DataRep, OffSet, !IO),
            io.write_string("]", !IO)
        ;
            FieldId = ml_field_named(FQFieldName, _Type),
            io.write_string("(", !IO),
            write_rval(DataRep, Rval, !IO),
            io.write_string(")", !IO),
            io.write_string(".", !IO),
            FQFieldName = qual(_, _, FieldName),
            io.write_string(FieldName, !IO)
        )
    ;
        Lval = ml_mem_ref(Rval, _),
        write_rval(DataRep, Rval, !IO)
    ;
        Lval = ml_global_var_ref(_),
        sorry($module, $pred, "global_var_ref NYI")
    ;
        Lval = ml_var(Var, _VarType),
        Var = qual(_, _, VarName),
        write_mlds_var_name_for_parameter(VarName, !IO)
    ).

:- pred write_defn_decl(il_data_rep::in, mlds_defn::in, io::di, io::uo) is det.

write_defn_decl(DataRep, Defn, !IO) :-
    Defn = mlds_defn(Name, _Context, _Flags, DefnBody),
    (
        DefnBody = mlds_data(Type, _Initializer, _GCStatement),
        Name = entity_data(mlds_data_var(VarName))
    ->
        write_parameter_type(DataRep, Type, !IO),
        io.write_string(" ", !IO),
        write_mlds_var_name_for_parameter(VarName, !IO),
        io.write_string(";\n", !IO)
    ;
        % XXX We should implement others.
        sorry($module, $pred, "data_addr_const rval")
    ).

:- pred write_parameter_type(il_data_rep::in, mlds_type::in, io::di, io::uo)
    is det.

write_parameter_type(DataRep, Type, !IO) :-
    ILType = mlds_type_to_ilds_type(DataRep, Type),
    write_il_type_as_foreign_type(ILType, !IO).

:- pred write_input_arg_as_foreign_type(il_data_rep::in, mlds_argument::in,
    io::di, io::uo) is det.

write_input_arg_as_foreign_type(DataRep, Arg, !IO) :-
    Arg = mlds_argument(EntityName, Type, _GCStatement),
    write_il_type_as_foreign_type(mlds_type_to_ilds_type(DataRep, Type), !IO),
    io.write_string(" ", !IO),
    ( EntityName = entity_data(mlds_data_var(VarName)) ->
        write_mlds_var_name_for_parameter(VarName, !IO)
    ;
        unexpected($module, $pred, "found a variable in a list")
    ).

:- pred write_parameter_initializer(il_data_rep::in, mlds_type::in,
    io::di, io::uo) is det.

write_parameter_initializer(DataRep, Type, !IO) :-
    ILType = mlds_type_to_ilds_type(DataRep, Type),
    ILType = il_type(_, ILSimpleType),
    write_csharp_initializer(ILSimpleType, !IO).

:- pred write_il_ret_type_as_foreign_type(ret_type::in, io::di, io::uo) is det.

write_il_ret_type_as_foreign_type(void, !IO) :-
    io.write_string("void", !IO).
write_il_ret_type_as_foreign_type(simple_type(T), !IO) :-
    write_il_simple_type_as_foreign_type(T, !IO).

:- pred write_il_type_as_foreign_type(il_type::in, io::di, io::uo) is det.

write_il_type_as_foreign_type(il_type(Modifiers, SimpleType), !IO) :-
    io.write_list(Modifiers, " ",
        write_il_type_modifier_as_foreign_type, !IO),
    write_il_simple_type_as_foreign_type(SimpleType, !IO).

:- pred write_il_type_modifier_as_foreign_type(ilds.type_modifier::in,
    io::di, io::uo) is det.

write_il_type_modifier_as_foreign_type(const, !IO) :-
    io.write_string("const", !IO).
write_il_type_modifier_as_foreign_type(readonly, !IO) :-
    io.write_string("readonly", !IO).
write_il_type_modifier_as_foreign_type(volatile, !IO) :-
    io.write_string("volatile", !IO).

    % XXX Need to revisit this and choose types appropriately.
    %
:- pred write_il_simple_type_as_foreign_type(simple_type::in,
    io::di, io::uo) is det.

write_il_simple_type_as_foreign_type(int8, !IO) :-
    io.write_string("sbyte", !IO).
write_il_simple_type_as_foreign_type(int16, !IO) :-
    io.write_string("short", !IO).
write_il_simple_type_as_foreign_type(int32, !IO) :-
    io.write_string("int", !IO).
write_il_simple_type_as_foreign_type(int64, !IO) :-
    io.write_string("long", !IO).
write_il_simple_type_as_foreign_type(uint8, !IO) :-
    io.write_string("byte", !IO).
write_il_simple_type_as_foreign_type(uint16, !IO) :-
    io.write_string("ushort", !IO).
write_il_simple_type_as_foreign_type(uint32, !IO) :-
    io.write_string("uint", !IO).
write_il_simple_type_as_foreign_type(uint64, !IO) :-
    io.write_string("ulong", !IO).
write_il_simple_type_as_foreign_type(native_int, !IO) :-
    io.write_string("int", !IO).
write_il_simple_type_as_foreign_type(native_uint, !IO) :-
    io.write_string("uint", !IO).
write_il_simple_type_as_foreign_type(float32, !IO) :-
    io.write_string("float", !IO).
write_il_simple_type_as_foreign_type(float64, !IO) :-
    io.write_string("double", !IO).
write_il_simple_type_as_foreign_type(native_float, !IO) :-
    io.write_string("float", !IO).
write_il_simple_type_as_foreign_type(bool, !IO) :-
    io.write_string("bool", !IO).
write_il_simple_type_as_foreign_type(char, !IO) :-
    io.write_string("char", !IO).
write_il_simple_type_as_foreign_type(string, !IO) :-
    io.write_string("string", !IO).
write_il_simple_type_as_foreign_type(object, !IO) :-
    io.write_string("object", !IO).
write_il_simple_type_as_foreign_type(refany, !IO) :-
    io.write_string("mercury.MR_RefAny", !IO).
write_il_simple_type_as_foreign_type(class(ClassName), !IO) :-
    write_class_name(ClassName, !IO).
write_il_simple_type_as_foreign_type(valuetype(ClassName), !IO) :-
    write_class_name(ClassName, !IO).
write_il_simple_type_as_foreign_type(interface(_ClassName), !IO) :-
    sorry($module, $pred, "interfaces").
write_il_simple_type_as_foreign_type('[]'(Type, Bounds), !IO) :-
    write_il_type_as_foreign_type(Type, !IO),
    io.write_string("[]", !IO),
    (
        Bounds = []
    ;
        Bounds = [_ | _],
        sorry($module, $pred, "arrays with bounds")
    ).
write_il_simple_type_as_foreign_type('&'(Type), !IO) :-
    % XXX Is this always right?
    io.write_string("ref ", !IO),
    write_il_type_as_foreign_type(Type, !IO).
write_il_simple_type_as_foreign_type('*'(Type), !IO) :-
    write_il_type_as_foreign_type(Type, !IO),
    io.write_string(" *", !IO).

:- pred write_csharp_initializer(simple_type::in, io::di, io::uo) is det.

write_csharp_initializer(int8, !IO) :-
    io.write_string("0", !IO).
write_csharp_initializer(int16, !IO) :-
    io.write_string("0", !IO).
write_csharp_initializer(int32, !IO) :-
    io.write_string("0", !IO).
write_csharp_initializer(int64, !IO) :-
    io.write_string("0", !IO).
write_csharp_initializer(uint8, !IO) :-
    io.write_string("0", !IO).
write_csharp_initializer(uint16, !IO) :-
    io.write_string("0", !IO).
write_csharp_initializer(uint32, !IO) :-
    io.write_string("0", !IO).
write_csharp_initializer(uint64, !IO) :-
    io.write_string("0", !IO).
write_csharp_initializer(native_int, !IO) :-
    io.write_string("0", !IO).
write_csharp_initializer(native_uint, !IO) :-
    io.write_string("0", !IO).
write_csharp_initializer(float32, !IO) :-
    io.write_string("0.0", !IO).
write_csharp_initializer(float64, !IO) :-
    io.write_string("0.0", !IO).
write_csharp_initializer(native_float, !IO) :-
    io.write_string("0.0", !IO).
write_csharp_initializer(bool, !IO) :-
    io.write_string("false", !IO).
write_csharp_initializer(char, !IO) :-
    io.write_string("'\\0'", !IO).
write_csharp_initializer(string, !IO) :-
    io.write_string("null", !IO).
write_csharp_initializer(object, !IO) :-
    io.write_string("null", !IO).
write_csharp_initializer(refany, !IO) :-
    io.write_string("null", !IO).
write_csharp_initializer(class(_ClassName), !IO) :-
    io.write_string("null", !IO).
write_csharp_initializer(interface(_ClassName), !IO) :-
    io.write_string("null", !IO).
write_csharp_initializer('[]'(_Type, _Bounds), !IO) :-
    io.write_string("null", !IO).
write_csharp_initializer('&'(_Type), !IO) :-
    io.write_string("null", !IO).
write_csharp_initializer('*'(_Type), !IO) :-
    io.write_string("null", !IO).
write_csharp_initializer(valuetype(ClassName), !IO) :-
    io.write_string("new ", !IO),
    write_class_name(ClassName, !IO),
    io.write_string("()", !IO).

:- pred write_class_name(structured_name::in, io::di, io::uo) is det.

write_class_name(structured_name(_Asm, DottedName, NestedClasses), !IO) :-
    io.write_list(DottedName ++ NestedClasses, ".", io.write_string, !IO).

:- pred write_mlds_var_name_for_local(mlds_var_name::in,
    io::di, io::uo) is det.

write_mlds_var_name_for_local(mlds_var_name(Name, _MaybeNum), !IO) :-
    io.write_string(Name, !IO).

:- pred write_mlds_var_name_for_parameter(mlds_var_name::in,
    io::di, io::uo) is det.

write_mlds_var_name_for_parameter(mlds_var_name(Name, MaybeNum), !IO) :-
    io.write_string(Name, !IO),
    (
        MaybeNum = yes(Num),
        io.write_string("_", !IO),
        io.write_int(Num, !IO)
    ;
        MaybeNum = no
    ).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_managed.
%-----------------------------------------------------------------------------%
