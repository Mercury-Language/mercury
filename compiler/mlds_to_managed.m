%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: 	mlds_to_managed 
% Main author: 	trd, petdr.
%
% Generate code for the foreign language interface to C# and managed C++.
%

:- module ml_backend__mlds_to_managed.
:- interface.

:- import_module ml_backend__mlds.
:- import_module libs__globals.
:- import_module io.

:- inst managed_lang == bound(csharp; managed_cplusplus).

	% Convert the MLDS to the specified foreign language and write
	% it to a file.
:- pred output_managed_code(foreign_language, mlds, io__state, io__state).
:- mode output_managed_code(in(managed_lang), in, di, uo) is det.

	% Print the header comments of the output module
:- pred output_src_start(mercury_module_name, io__state, io__state).
:- mode output_src_start(in, di, uo) is det.

	% Print the footer commments of the output module
:- pred output_src_end(mercury_module_name, io__state, io__state).
:- mode output_src_end(in, di, uo) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs__globals, libs__options, hlds__passes_aux.
:- import_module backend_libs__builtin_ops, backend_libs__c_util.
:- import_module parse_tree__modules, libs__tree.
:- import_module hlds__hlds_pred. % for `pred_proc_id'.
:- import_module parse_tree__prog_data, parse_tree__prog_out.
:- import_module backend_libs__foreign, backend_libs__rtti.
:- import_module check_hlds__type_util, hlds__error_util.

:- import_module ml_backend__ilds, ml_backend__ilasm, ml_backend__il_peephole.
:- import_module ml_backend__ml_util, ml_backend__ml_code_util.

:- import_module bool, int, map, string, list, assoc_list, term, std_util.
:- import_module library, require, counter.

:- import_module ml_backend__mlds_to_il.

output_managed_code(Lang, MLDS) -->
	{ MLDS = mlds(ModuleName, _ForeignCode, _Imports, _Defns) },
	output_src_start(ModuleName), 
	io__nl,

	generate_code(Lang, MLDS),

	output_src_end(ModuleName).

%-----------------------------------------------------------------------------%

output_src_start(ModuleName) -->
	{ library__version(Version) },
	{ prog_out__sym_name_to_string(ModuleName, ModuleNameStr) },
	io__write_strings(
		["//\n// Automatically generated from `", 
		ModuleNameStr,
		".m' by the\n",
		"// Mercury compiler, version ", 
		Version,
		".\n",
		"// Do not edit.\n",
		"\n\n"]).

output_src_end(ModuleName) -->
	io__write_string("// End of module: "),
	prog_out__write_sym_name(ModuleName),
	io__write_string(". \n").

%-----------------------------------------------------------------------------%

:- pred generate_code(foreign_language, mlds, io__state, io__state).
:- mode generate_code(in(managed_lang), in, di, uo) is det.

generate_code(Lang, MLDS) -->

	{ MLDS = mlds(ModuleName, AllForeignCode, Imports, Defns) },
	{ ClassName = class_name(mercury_module_name_to_mlds(ModuleName), 
			wrapper_class_name) },

	io__nl,

		% Output any generic header code specific to the target
		% language.
	output_language_specific_header_code(Lang, ModuleName, Imports),

		% Get the foreign code for the required language.
	{ ForeignCode = map__lookup(AllForeignCode, Lang) },
	generate_foreign_header_code(Lang,
			mercury_module_name_to_mlds(ModuleName), ForeignCode),

		% Output the namespace.
	{ generate_namespace_details(Lang, ClassName,
			NameSpaceFmtStr, Namespace) },
	io__write_list(Namespace, "\n", 
		(pred(N::in, di, uo) is det -->
			io__format(NameSpaceFmtStr, [s(N)])
	)),

	( { Lang = csharp },
		io__write_strings([
			"\npublic class " ++ wrapper_class_name,
			"{\n"])
	; { Lang = managed_cplusplus },
		io__write_strings([
			"\n__gc public class " ++ wrapper_class_name,
			"{\n",
			"public:\n"])
	),

		% Output the contents of pragma foreign_code declarations.
	generate_foreign_code(Lang,
			mercury_module_name_to_mlds(ModuleName), ForeignCode),

	io__write_string("\n"),

		% Output the contents of foreign_proc declarations.
		% Put each one inside a method.
	list__foldl(generate_method_code(Lang,
			mercury_module_name_to_mlds(ModuleName)), Defns),

	io__write_string("};\n"),

		% Close the namespace braces.
	io__write_list(Namespace, "\n", 
		(pred(_N::in, di, uo) is det -->
			io__write_string("}")
	)),

	io__nl.

:- pred output_language_specific_header_code(foreign_language::in(managed_lang),
		mercury_module_name::in, mlds__imports::in,
		io::di, io::uo) is det.

output_language_specific_header_code(csharp, _ModuleName, _Imports) -->
	io__write_strings([
		% XXX We may be able to drop the mercury namespace soon,
		% as there doesn't appear to be any llds generated code
		% in the C# code anymore.
		"using mercury;\n",
		"\n"]),

	globals__io_lookup_bool_option(sign_assembly, SignAssembly),
	( { SignAssembly = yes },
		io__write_string(
"[assembly:System.Reflection.AssemblyKeyFileAttribute(\"mercury.sn\")]\n")
	; { SignAssembly = no },
		[]
	).
output_language_specific_header_code(managed_cplusplus, ModuleName, Imports) -->
	get_il_data_rep(DataRep),
	( { DataRep = il_data_rep(yes, _) } ->
		io__write_string("#define MR_HIGHLEVEL_DATA\n")
	;
		[]
	),

	io__write_string("#using <mscorlib.dll>\n"),

	( { mercury_std_library_module_name(ModuleName) } ->
		io__write_strings([
			"#using ""mercury_mcpp.dll""\n",
			"#using ""mercury_il.dll""\n"
			%,
			%"#using ""private_builtin.dll""\n",
			%"#using ""builtin.dll""\n"
			])
	;
		[]
	),

	{ list__map(
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
			    prog_out__sym_name_to_string(SymName, ".", Str)
			),
			Result = [Str]
		    ;
			Result = []
		    )
		), Imports, ImportListList) },
	{ ActualImports = remove_dups(condense(ImportListList)) },

	list__foldl((pred(I::in, di, uo) is det -->
			io__write_string("#using """),
			io__write_string(I),
			io__write_string(".dll""\n")
		), ActualImports),

	{ prog_out__sym_name_to_string(ModuleName, ModuleNameStr) },
	io__write_strings([
		"#using """, ModuleNameStr, ".dll""\n",
		"#include ""mercury_mcpp.h""\n",

		% XXX We have to use the mercury namespace, as
		% llds_out still generates some of the code used
		% in the MC++ interface, and so it doesn't have
		% "mercury::" namespace qualifiers.
		"using namespace mercury;\n",

		% XXX this supresses problems caused by
		% references to float.  If you don't do this,
		% you'll get link errors.  Revisit this when the
		% .NET implementation has matured.
		"extern ""C"" int _fltused=0;\n",
		"\n"]),

	globals__io_lookup_bool_option(sign_assembly, SignAssembly),
	( { SignAssembly = yes },
		io__write_string(
"[assembly:System::Reflection::AssemblyKeyFileAttribute(\"mercury.sn\")];\n")
	; { SignAssembly = no },
		[]
	).


	% XXX we don't handle `:- pragma foreign_import_module'.
:- pred generate_foreign_header_code(foreign_language::in(managed_lang),
		mlds_module_name::in, mlds__foreign_code::in,
		io__state::di, io__state::uo) is det.

generate_foreign_header_code(Lang, _ModuleName, 
		mlds__foreign_code(RevHeaderCode, _RevImports, _RevBodyCode,
			_ExportDefns)) -->
	{ HeaderCode = list__reverse(RevHeaderCode) },
	io__write_list(HeaderCode, "\n", 
		(pred(foreign_decl_code(CodeLang, Code, Context)::in,
				di, uo) is det -->
			output_context(Lang, Context),
			( { CodeLang = Lang } ->
				io__write_string(Code)
			;
				{ sorry(this_file, "wrong foreign code") }
			),
			output_reset_context(Lang)
		)).

:- pred generate_namespace_details(foreign_language::in(managed_lang),
		ilds__class_name::in,
		string::out, list(string)::out) is det.

generate_namespace_details(Lang, ClassName, NameSpaceFmtStr, Namespace) :-
	% XXX we should consider what happens if we need to mangle
	% the namespace name.
	( Lang = csharp,
		NameExt = "__csharp_code",
		NameSpaceFmtStr = "namespace @%s {"
	; Lang = managed_cplusplus,
		NameExt = "__cpp_code",
		NameSpaceFmtStr = "namespace %s {"
	),

	Namespace0 = get_class_namespace(ClassName),
	( list__reverse(Namespace0) = [Head | Tail] ->
		Namespace = list__reverse([Head ++ NameExt | Tail])
	;
		Namespace = Namespace0
	).

	% XXX we don't handle `:- pragma foreign_import_module'.
:- pred generate_foreign_code(foreign_language::in(managed_lang),
		mlds_module_name::in, mlds__foreign_code::in,
		io__state::di, io__state::uo) is det.

generate_foreign_code(Lang, _ModuleName, 
		mlds__foreign_code(_RevHeaderCode, _RevImports, RevBodyCode,
			_ExportDefns)) -->
	{ BodyCode = list__reverse(RevBodyCode) },
	io__write_list(BodyCode, "\n", 
		(pred(user_foreign_code(CodeLang, Code, Context)::in,
				di, uo) is det -->
			output_context(Lang, Context),
			( { Lang = CodeLang } ->
				io__write_string(Code)
			;
				{ sorry(this_file, "wrong foreign code") }
			),
			output_reset_context(Lang)
	)).

:- pred generate_method_code(foreign_language::in(managed_lang),
		mlds_module_name::in, mlds__defn::in,
		io__state::di, io__state::uo) is det.

generate_method_code(_, _, defn(export(_), _, _, _)) --> [].
generate_method_code(_, _, defn(data(_), _, _, _)) --> [].
generate_method_code(_, _, defn(type(_, _), _, _, _)) --> [].
generate_method_code(Lang, _ModuleName, 
		defn(function(PredLabel, ProcId, MaybeSeqNum, _PredId), 
	_Context, _DeclFlags, Entity)) -->

	( 
			% XXX we ignore the attributes
		{ Entity = mlds__function(_, Params, defined_here(Statement),
			_Attributes) },
		{ has_foreign_languages(Statement, Langs) },
		{ list__member(Lang, Langs) }
	->
		get_il_data_rep(DataRep),
		{ Params = mlds__func_params(Inputs, Outputs) },
		{ Outputs = [] ->
			ReturnType = void
		; Outputs = [MLDSReturnType] ->
			mlds_type_to_ilds_type(DataRep, MLDSReturnType) = 
				ilds__type(_, SimpleType),
			ReturnType = simple_type(SimpleType)
		;
			% C# and MC++ don't support multiple return values
			sorry(this_file, "multiple return values")
		},


		{ predlabel_to_id(PredLabel, ProcId, MaybeSeqNum, Id) },
		( { Lang = csharp },
			io__write_string("public static ")
		; { Lang = managed_cplusplus },
			io__write_string("static ")
		),
		write_il_ret_type_as_foreign_type(Lang, ReturnType),

		io__write_string(" "),

		io__write_string(Id),
		io__write_string("("),
		io__write_list(Inputs, ", ",
				write_input_arg_as_foreign_type(Lang)),
		io__write_string(")"),
		io__nl,

		io__write_string("{\n"),
		write_statement(Lang, Inputs, Statement),
		io__write_string("}\n")
	;
		[]
	).

:- pred write_statement(foreign_language::in(managed_lang), 
		mlds__arguments::in, mlds__statement::in,
		io__state::di, io__state::uo) is det.
write_statement(Lang, Args, statement(Statement, Context)) -->
	( 
			% XXX petdr
		{ Statement = atomic(outline_foreign_proc(Lang, OutlineArgs,
				_Lvals, Code)) }
	->
		list__foldl(write_outline_arg_init(Lang), OutlineArgs),
		output_context(Lang, get_prog_context(Context)),
		io__write_string(Code),
		io__nl,
		output_reset_context(Lang),
		list__foldl(write_outline_arg_final(Lang), OutlineArgs)
	;
		{ Statement = block(Defns, Statements) }
	->
		io__write_list(Defns, "", write_defn_decl(Lang)),
		io__write_string("{\n"),
		io__write_list(Statements, "", write_statement(Lang, Args)),
		io__write_string("\n}\n")
	;
		{ Statement = return(Rvals) }
	->
		( { Rvals = [Rval] } ->
			io__write_string("return "),
			write_rval(Lang, Rval),
			io__write_string(";\n")
		;
			{ sorry(this_file, "multiple return values") }
		)
	;
		{ Statement = atomic(assign(LVal, RVal)) } 
	->
		write_lval(Lang, LVal),
		io__write_string(" = "),
		write_rval(Lang, RVal),
		io__write_string(";\n")
	;
		{ functor(Statement, SFunctor, _Arity) },
		{ sorry(this_file, "foreign code output for " ++ SFunctor) }
	).

:- pred write_outline_arg_init(foreign_language::in(managed_lang),
		outline_arg::in, io::di, io::uo) is det.

write_outline_arg_init(Lang, in(Type, VarName, Rval)) -->
	write_parameter_type(Lang, Type),
	io__write_string(" "),
	io__write_string(VarName),
	io__write_string(" = "),
	write_rval(Lang, Rval),
	io__write_string(";\n").
write_outline_arg_init(Lang, out(Type, VarName, _Lval)) -->
	write_parameter_type(Lang, Type),
	io__write_string(" "),
	io__write_string(VarName),
	% In C# give output variables a default value to avoid warnings.
	( { Lang = csharp } ->
		io__write_string(" = "),
		write_parameter_initializer(Lang, Type)
	;
		[]
	),
	io__write_string(";\n").
write_outline_arg_init(_Lang, unused) --> [].

:- pred write_outline_arg_final(foreign_language::in(managed_lang),
		outline_arg::in, io::di, io::uo) is det.

write_outline_arg_final(_Lang, in(_, _, _)) --> [].
write_outline_arg_final(Lang, out(_Type, VarName, Lval)) -->
	write_lval(Lang, Lval),
	io__write_string(" = "),
	io__write_string(VarName),
	io__write_string(";\n").
write_outline_arg_final(_Lang, unused) --> [].


:- pred write_declare_and_assign_local(foreign_language::in(managed_lang),
		mlds__argument::in, io::di, io::uo) is det.

write_declare_and_assign_local(Lang, argument(Name, Type, _GcCode)) -->
	{ Name = data(var(VarName0)) ->
		VarName = VarName0
	;
		unexpected(this_file, "not a variable name")
	},

	% A pointer type is an output type.
	( { Type = mlds__ptr_type(OutputType) } ->
		( { is_anonymous_variable(VarName) } ->
			[]
		;
			write_parameter_type(Lang, OutputType),
			io__write_string(" "),
			write_mlds_var_name_for_local(VarName),

			% In C# give output types a default value to
			% avoid warnings.
			( { Lang = csharp } ->
				io__write_string(" = "),
				write_parameter_initializer(Lang,
						OutputType)
			;
				[]
			),

			io__write_string(";\n")
		)
	;
		write_parameter_type(Lang, Type),
		io__write_string(" "),
		write_mlds_var_name_for_local(VarName),
		io__write_string(" = "),
		write_mlds_var_name_for_parameter(VarName),
		io__write_string(";\n")
	).

:- pred write_assign_local_to_output(foreign_language::in(managed_lang),
		mlds__argument::in, io::di, io::uo) is det.

write_assign_local_to_output(Lang, argument(Name, Type, _GcCode)) -->
	{ Name = data(var(VarName0)) ->
		VarName = VarName0
	;
		unexpected(this_file, "not a variable name")
	},

		% A pointer type is an output type.
	( 
		{ Type = mlds__ptr_type(_OutputType) },
		{ not is_anonymous_variable(VarName) }
	->
		( { Lang = csharp },
			[]
		; { Lang = managed_cplusplus },
			io__write_string("*")
		),
		write_mlds_var_name_for_parameter(VarName),
		io__write_string(" = "),
		write_mlds_var_name_for_local(VarName),
		io__write_string(";\n")
	;
		[]
	).

:- pred is_anonymous_variable(var_name::in) is semidet.

is_anonymous_variable(var_name(Name, _)) :-
	string__prefix(Name, "_").

%------------------------------------------------------------------------------%

:- pred output_context(foreign_language::in(managed_lang), prog_context::in,
		io__state::di, io__state::uo) is det.

output_context(_Lang, Context) -->
	{ term__context_file(Context, File) },
	{ term__context_line(Context, Line) },
	c_util__set_line_num(File, Line).

:- pred output_reset_context(foreign_language::in(managed_lang),
		io__state::di, io__state::uo) is det.

output_reset_context(_) -->
	c_util__reset_line_num.


:- pred write_rval(foreign_language, mlds__rval, io__state, io__state).
:- mode write_rval(in(managed_lang), in, di, uo) is det.

write_rval(Lang, lval(Lval)) -->
	write_lval(Lang, Lval).
write_rval(_Lang, mkword(_Tag, _Rval)) -->
	{ sorry(this_file, "mkword rval") }.
write_rval(Lang, const(RvalConst)) -->
	write_rval_const(Lang, RvalConst).
write_rval(Lang, unop(Unop, Rval)) -->
	( 
		{ Unop = std_unop(StdUnop) },
		{ c_util__unary_prefix_op(StdUnop, UnopStr) }
	->
		io__write_string(UnopStr),
		io__write_string("("),
		write_rval(Lang, Rval),
		io__write_string(")")
	;
		{ Unop = cast(Type) }
	->
		io__write_string("("),
		write_parameter_type(Lang, Type),
		io__write_string(") "),
		write_rval(Lang, Rval)
	;
		{ sorry(this_file, "box or unbox unop") }
	).
write_rval(Lang, binop(Binop, Rval1, Rval2)) -->
	( 
		{ c_util__binary_infix_op(Binop, BinopStr) }
	->
		io__write_string("("),
		write_rval(Lang, Rval1),
		io__write_string(") "),
		io__write_string(BinopStr),
		io__write_string(" ("),
		write_rval(Lang, Rval2),
		io__write_string(")")
	;
		{ sorry(this_file, "binop rval") }
	).

write_rval(_Lang, mem_addr(_)) -->
	{ sorry(this_file, "mem_addr rval") }.
	
write_rval(_Lang, self(_)) -->
	{ sorry(this_file, "self rval") }.
	
:- pred write_rval_const(foreign_language, mlds__rval_const, io, io).
:- mode write_rval_const(in(managed_lang), in, di, uo) is det.

write_rval_const(_Lang, true) --> io__write_string("1").
write_rval_const(_Lang, false) --> io__write_string("0").
write_rval_const(_Lang, int_const(I)) --> io__write_int(I).
write_rval_const(_Lang, float_const(F)) --> io__write_float(F).
	% XXX We don't quote this correctly.
write_rval_const(_Lang, string_const(S)) --> 
	io__write_string(""""),
	c_util__output_quoted_string(S),
	io__write_string("""").
write_rval_const(_Lang, multi_string_const(L, S)) --> 
	io__write_string(""""),
	c_util__output_quoted_multi_string(L, S),
	io__write_string("""").
write_rval_const(Lang, code_addr_const(CodeAddrConst)) --> 
	(
		{ CodeAddrConst = proc(ProcLabel, _FuncSignature) },
		{ mangle_mlds_proc_label(ProcLabel, no, ClassName,
				MangledName) },
		write_class_name(Lang, ClassName),
		write_field_selector(Lang),
		io__write_string(MangledName)
	;
		{ CodeAddrConst = internal(ProcLabel, SeqNum,
				_FuncSignature) },
		{ mangle_mlds_proc_label(ProcLabel, yes(SeqNum), ClassName,
				MangledName) },
		write_class_name(Lang, ClassName),
		write_field_selector(Lang),
		io__write_string(MangledName)
	).
write_rval_const(_Lang, data_addr_const(_)) --> 
	{ sorry(this_file, "data_addr_const rval") }.
write_rval_const(Lang, null(_)) --> 
	( { Lang = csharp },
		io__write_string("null")
	; { Lang = managed_cplusplus },
		io__write_string("NULL")
	).

:- pred write_lval(foreign_language, mlds__lval, io__state, io__state).
:- mode write_lval(in(managed_lang), in, di, uo) is det.

write_lval(Lang, field(_, Rval, named_field(FieldId, _Type), _, _)) -->
	io__write_string("("),
	write_rval(Lang, Rval),
	io__write_string(")"),
	write_field_selector(Lang),
	{ FieldId = qual(_, FieldName) },
	io__write_string(FieldName).
write_lval(Lang, field(_, Rval, offset(OffSet), _, _)) -->
	io__write_string("("),
	write_rval(Lang, Rval),
	io__write_string(")"),
	io__write_string("["),
	write_rval(Lang, OffSet),
	io__write_string("]").
write_lval(Lang, mem_ref(Rval, _)) -->
	( { Lang = managed_cplusplus },
		io__write_string("*")
	; { Lang = csharp },
		[]
	),
	write_rval(Lang, Rval).
write_lval(_Lang, var(Var, _VarType)) -->
	{ Var = qual(_, VarName) },
	write_mlds_var_name_for_parameter(VarName).

:- pred write_field_selector(foreign_language::in(managed_lang),
		io__state::di, io__state::uo) is det.

write_field_selector(csharp) -->
	io__write_string(".").
write_field_selector(managed_cplusplus) -->
	io__write_string("->").

:- pred write_defn_decl(foreign_language, mlds__defn, io__state, io__state).
:- mode write_defn_decl(in(managed_lang), in, di, uo) is det.

write_defn_decl(Lang, Defn) -->
	{ Defn = mlds__defn(Name, _Context, _Flags, DefnBody) },
	(
		{ DefnBody = data(Type, _Initializer, _GC_TraceCode) },
		{ Name = data(var(VarName)) }
	->
		write_parameter_type(Lang, Type),
		io__write_string(" "),
		write_mlds_var_name_for_parameter(VarName),
		io__write_string(";\n")
	;
		% XXX we should implement others
		{ sorry(this_file, "data_addr_const rval") }
	).

:- pred write_parameter_type(foreign_language, mlds__type, io, io).
:- mode write_parameter_type(in(managed_lang), in, di, uo) is det.

write_parameter_type(Lang, Type) -->
	get_il_data_rep(DataRep),
	{ ILType = mlds_type_to_ilds_type(DataRep, Type) },
	write_il_type_as_foreign_type(Lang, ILType).

:- pred write_input_arg_as_foreign_type(foreign_language::in(managed_lang),
		mlds__argument::in, io__state::di, io__state::uo) is det.
write_input_arg_as_foreign_type(Lang, Arg) --> 
	{ Arg = mlds__argument(EntityName, Type, _GC_TraceCode) },
	get_il_data_rep(DataRep),
	write_il_type_as_foreign_type(Lang, 
			mlds_type_to_ilds_type(DataRep, Type)),
	io__write_string(" "),
	( { EntityName = data(var(VarName)) } ->
		write_mlds_var_name_for_parameter(VarName)
	;
		{ error("found a variable in a list") }
	).

:- pred write_parameter_initializer(foreign_language, mlds__type, io, io).
:- mode write_parameter_initializer(in(managed_lang), in, di, uo) is det.

write_parameter_initializer(managed_cplusplus, _Type) -->
	{ unexpected(this_file, "initializer for MC++") }.
write_parameter_initializer(csharp, Type) -->
	get_il_data_rep(DataRep),
	{ ILType = mlds_type_to_ilds_type(DataRep, Type) },
	{ ILType = type(_, ILSimpleType) },
	write_csharp_initializer(ILSimpleType).

:- pred write_il_ret_type_as_foreign_type(foreign_language::in(managed_lang),
		ret_type::in, io__state::di, io__state::uo) is det.

write_il_ret_type_as_foreign_type(_Lang, void) -->
	io__write_string("void").
write_il_ret_type_as_foreign_type(Lang, simple_type(T)) --> 
	write_il_simple_type_as_foreign_type(Lang, T).

:- pred write_il_type_as_foreign_type(foreign_language::in(managed_lang),
		ilds__type::in, io__state::di, io__state::uo) is det.

write_il_type_as_foreign_type(Lang, ilds__type(Modifiers, SimpleType)) -->
	io__write_list(Modifiers, " ", 
		write_il_type_modifier_as_foreign_type(Lang)),
	write_il_simple_type_as_foreign_type(Lang, SimpleType).

:- pred write_il_type_modifier_as_foreign_type(
	foreign_language::in(managed_lang), ilds__type_modifier::in,
	io__state::di, io__state::uo) is det.

write_il_type_modifier_as_foreign_type(_Lang, const) --> 
	io__write_string("const").
write_il_type_modifier_as_foreign_type(_Lang, readonly) --> 
	io__write_string("readonly").
write_il_type_modifier_as_foreign_type(_Lang, volatile) --> 
	io__write_string("volatile").

	% XXX need to revisit this and choose types appropriately
:- pred write_il_simple_type_as_foreign_type(foreign_language::in(managed_lang),
		simple_type::in, io__state::di, io__state::uo) is det.

write_il_simple_type_as_foreign_type(csharp, int8) --> 
	io__write_string("sbyte").
write_il_simple_type_as_foreign_type(csharp, int16) --> 
	io__write_string("short").
write_il_simple_type_as_foreign_type(csharp, int32) --> 
	io__write_string("int").
write_il_simple_type_as_foreign_type(csharp, int64) --> 
	io__write_string("long").
write_il_simple_type_as_foreign_type(csharp, uint8) --> 
	io__write_string("byte").
write_il_simple_type_as_foreign_type(csharp, uint16) --> 
	io__write_string("ushort").
write_il_simple_type_as_foreign_type(csharp, uint32) --> 
	io__write_string("uint").
write_il_simple_type_as_foreign_type(csharp, uint64) --> 
	io__write_string("ulong").
write_il_simple_type_as_foreign_type(csharp, native_int) --> 
	io__write_string("int").
write_il_simple_type_as_foreign_type(csharp, native_uint) --> 
	io__write_string("uint").
write_il_simple_type_as_foreign_type(csharp, float32) --> 
	io__write_string("float").
write_il_simple_type_as_foreign_type(csharp, float64) --> 
	io__write_string("double").
write_il_simple_type_as_foreign_type(csharp, native_float) --> 
	io__write_string("float").
write_il_simple_type_as_foreign_type(csharp, bool) --> 
	io__write_string("bool").
write_il_simple_type_as_foreign_type(csharp, char) --> 
	io__write_string("char").
write_il_simple_type_as_foreign_type(csharp, string) --> 
	io__write_string("string").
write_il_simple_type_as_foreign_type(csharp, object) --> 
	io__write_string("object").
write_il_simple_type_as_foreign_type(csharp, refany) --> 
	io__write_string("mercury.MR_RefAny").
write_il_simple_type_as_foreign_type(csharp, class(ClassName)) --> 
	write_class_name(csharp, ClassName).
write_il_simple_type_as_foreign_type(csharp, valuetype(ClassName)) --> 
	write_class_name(csharp, ClassName).
write_il_simple_type_as_foreign_type(csharp, interface(_ClassName)) --> 
	{ sorry(this_file, "interfaces") }.
write_il_simple_type_as_foreign_type(csharp, '[]'(Type, Bounds)) --> 
	write_il_type_as_foreign_type(csharp, Type),
	io__write_string("[]"),
	( { Bounds = [] } ->
		[]
	;
		{ sorry(this_file, "arrays with bounds") }
	).
write_il_simple_type_as_foreign_type(csharp, '&'(Type)) --> 
		% XXX is this always right?
	io__write_string("ref "),
	write_il_type_as_foreign_type(csharp, Type).
write_il_simple_type_as_foreign_type(csharp, '*'(Type)) --> 
	write_il_type_as_foreign_type(csharp, Type),
	io__write_string(" *").

write_il_simple_type_as_foreign_type(managed_cplusplus, int8) --> 
	io__write_string("mercury::MR_Integer8").
write_il_simple_type_as_foreign_type(managed_cplusplus, int16) --> 
	io__write_string("mercury::MR_Integer16").
write_il_simple_type_as_foreign_type(managed_cplusplus, int32) --> 
	io__write_string("mercury::MR_Integer").
write_il_simple_type_as_foreign_type(managed_cplusplus, int64) --> 
	io__write_string("mercury::MR_Integer64").
write_il_simple_type_as_foreign_type(managed_cplusplus, uint8) --> 
	io__write_string("unsigned int").
write_il_simple_type_as_foreign_type(managed_cplusplus, uint16) --> 
	io__write_string("unsigned int").
write_il_simple_type_as_foreign_type(managed_cplusplus, uint32) --> 
	io__write_string("unsigned int").
write_il_simple_type_as_foreign_type(managed_cplusplus, uint64) --> 
	io__write_string("unsigned int").
write_il_simple_type_as_foreign_type(managed_cplusplus, native_int) --> 
	io__write_string("mercury::MR_Integer").
write_il_simple_type_as_foreign_type(managed_cplusplus, native_uint) --> 
	io__write_string("unsigned int").
write_il_simple_type_as_foreign_type(managed_cplusplus, float32) --> 
	io__write_string("float").
write_il_simple_type_as_foreign_type(managed_cplusplus, float64) --> 
	io__write_string("mercury::MR_Float").
write_il_simple_type_as_foreign_type(managed_cplusplus, native_float) --> 
	io__write_string("mercury::MR_Float").
write_il_simple_type_as_foreign_type(managed_cplusplus, bool) --> 
	io__write_string("mercury::MR_Bool").
write_il_simple_type_as_foreign_type(managed_cplusplus, char) --> 
	io__write_string("mercury::MR_Char").
write_il_simple_type_as_foreign_type(managed_cplusplus, string) --> 
	io__write_string("mercury::MR_String").
write_il_simple_type_as_foreign_type(managed_cplusplus, object) --> 
	io__write_string("mercury::MR_Box").
write_il_simple_type_as_foreign_type(managed_cplusplus, refany) --> 
	io__write_string("mercury::MR_RefAny").
write_il_simple_type_as_foreign_type(managed_cplusplus, class(ClassName)) --> 
	( { ClassName = il_generic_class_name } ->
		io__write_string("mercury::MR_Box")
	;
		io__write_string("public class "),
		write_class_name(managed_cplusplus, ClassName),
		io__write_string(" *")
	).
write_il_simple_type_as_foreign_type(managed_cplusplus,
		valuetype(ClassName)) --> 
	io__write_string("__value class "),
	write_class_name(managed_cplusplus, ClassName).
		% XXX this is not the right syntax
write_il_simple_type_as_foreign_type(managed_cplusplus,
		interface(ClassName)) --> 
	io__write_string("interface "),
	write_class_name(managed_cplusplus, ClassName),
	io__write_string(" *").
		% XXX this needs more work
write_il_simple_type_as_foreign_type(managed_cplusplus,
		'[]'(_Type, _Bounds)) --> 
	io__write_string("mercury::MR_Word").
write_il_simple_type_as_foreign_type(managed_cplusplus, '&'(Type)) --> 
	io__write_string("MR_Ref("),
	write_il_type_as_foreign_type(managed_cplusplus, Type),
	io__write_string(")").
write_il_simple_type_as_foreign_type(managed_cplusplus, '*'(Type)) --> 
	write_il_type_as_foreign_type(managed_cplusplus, Type),
	io__write_string(" *").

:- pred write_csharp_initializer(simple_type::in, io::di, io::uo) is det.

write_csharp_initializer(int8) --> io__write_string("0").
write_csharp_initializer(int16) --> io__write_string("0").
write_csharp_initializer(int32) --> io__write_string("0").
write_csharp_initializer(int64) --> io__write_string("0").
write_csharp_initializer(uint8) --> io__write_string("0").
write_csharp_initializer(uint16) --> io__write_string("0").
write_csharp_initializer(uint32) --> io__write_string("0").
write_csharp_initializer(uint64) --> io__write_string("0").
write_csharp_initializer(native_int) --> io__write_string("0").
write_csharp_initializer(native_uint) --> io__write_string("0").
write_csharp_initializer(float32) --> io__write_string("0.0").
write_csharp_initializer(float64) --> io__write_string("0.0").
write_csharp_initializer(native_float) --> io__write_string("0.0").
write_csharp_initializer(bool) --> io__write_string("false").
write_csharp_initializer(char) --> io__write_string("'\\0'").
write_csharp_initializer(string) --> io__write_string("null").
write_csharp_initializer(object) --> io__write_string("null").
write_csharp_initializer(refany) --> io__write_string("null").
write_csharp_initializer(class(_ClassName)) --> io__write_string("null").
write_csharp_initializer(interface(_ClassName)) --> io__write_string("null").
write_csharp_initializer('[]'(_Type, _Bounds)) --> io__write_string("null").
write_csharp_initializer('&'(_Type)) --> io__write_string("null").
write_csharp_initializer('*'(_Type)) --> io__write_string("null").
write_csharp_initializer(valuetype(ClassName)) --> 
	io__write_string("new "),
	write_class_name(csharp, ClassName),
	io__write_string("()").

:- pred write_class_name(foreign_language::in(managed_lang),
		structured_name::in, io__state::di, io__state::uo) is det.
write_class_name(Lang, structured_name(_Asm, DottedName, NestedClasses)) -->
	{ Lang = csharp,
		Sep = "."
	; Lang = managed_cplusplus,
		Sep = "::"
	},
	io__write_list(DottedName ++ NestedClasses, Sep, io__write_string).

:- pred write_mlds_var_name_for_local(mlds__var_name::in,
		io__state::di, io__state::uo) is det.

write_mlds_var_name_for_local(var_name(Name, _MaybeNum)) -->
	io__write_string(Name).

:- pred write_mlds_var_name_for_parameter(mlds__var_name::in,
	io__state::di, io__state::uo) is det.
write_mlds_var_name_for_parameter(var_name(Name, MaybeNum)) -->
	io__write_string(Name),
	( { MaybeNum = yes(Num) } ->
		io__write_string("_"),
		io__write_int(Num)
	;
		[]
	).

:- func this_file = string.
this_file = "mlds_to_managed.m".

:- end_module ml_backend__mlds_to_managed.
