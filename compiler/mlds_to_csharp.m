%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% mlds_to_csharp - Generate C# code for the foreign language interface.
% Main author: trd.
%
% This code converts the MLDS representation of foreign language code into C#

:- module mlds_to_csharp.
:- interface.

:- import_module mlds.
:- import_module io.

	% Convert the MLDS to C# and write it to a file.

:- pred mlds_to_csharp__output_csharp_code(mlds, io__state, io__state).
:- mode mlds_to_csharp__output_csharp_code(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module globals, options, passes_aux.
:- import_module builtin_ops, c_util, modules, tree.
:- import_module hlds_pred. % for `pred_proc_id'.
:- import_module prog_data, prog_out.
:- import_module rtti, type_util, error_util.

:- import_module ilds, ilasm, il_peephole.
:- import_module ml_util, ml_code_util.
:- use_module llds. /* for user_c_code */

:- import_module bool, int, map, string, list, assoc_list, term, std_util.
:- import_module library, require, counter.

:- import_module mlds_to_il.

%-----------------------------------------------------------------------------%


%-----------------------------------------------------------------------------%

	%
	% Generate the `__csharp_code.cs' file which contains the c sharp
	% code.
	%
output_csharp_code(MLDS) -->
	{ MLDS = mlds(ModuleName, _ForeignCode, _Imports, _Defns) },
	output_src_start(ModuleName), 
	io__nl,

	generate_csharp_code(MLDS),

	output_src_end(ModuleName).

:- pred output_src_start(mercury_module_name, io__state, io__state).
:- mode output_src_start(in, di, uo) is det.

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

:- pred output_src_end(mercury_module_name, io__state, io__state).
:- mode output_src_end(in, di, uo) is det.

output_src_end(ModuleName) -->
	io__write_string("// End of module: "),
	prog_out__write_sym_name(ModuleName),
	io__write_string(". \n").

%-----------------------------------------------------------------------------%

	% XXX we don't output contexts for any of this.
:- pred generate_csharp_code(mlds, io__state, io__state).
:- mode generate_csharp_code(in, di, uo) is det.
generate_csharp_code(MLDS) -->

	{ MLDS = mlds(ModuleName, AllForeignCode, _Imports, Defns) },
	{ ClassName = class_name(mercury_module_name_to_mlds(ModuleName), 
			wrapper_class_name) },

	io__nl,
	io__write_strings([
		% XXX We may be able to drop the mercury namespace soon,
		% as there doesn't appear to be any llds generated code in
		% the C# code anymore.
		"using mercury;\n",
		"\n"]),

		% Get the foreign code for C#
	{ ForeignCode = map__lookup(AllForeignCode, csharp) },
	generate_foreign_header_code(mercury_module_name_to_mlds(ModuleName),
		ForeignCode),

	globals__io_lookup_bool_option(sign_assembly, SignAssembly),
	( { SignAssembly = yes },
		io__write_string("[assembly:System.Reflection.AssemblyKeyFileAttribute(\"mercury.sn\")]\n")
	; { SignAssembly = no },
		[]
	),

	{ Namespace0 = get_class_namespace(ClassName) },
	{ list__reverse(Namespace0) = [Head | Tail] ->
		Namespace = list__reverse([Head ++ "__csharp_code" | Tail])
	;
		Namespace = Namespace0
	},

		% XXX we should consider what happens if we need to mangle
		% the namespace name.
	io__write_list(Namespace, "\n", 
		(pred(N::in, di, uo) is det -->
			io__format("namespace @%s {", [s(N)])
	)),

	io__write_strings([
		"\npublic class " ++ wrapper_class_name,
		"{\n"]),

		% Output the contents of pragma foreign_code declarations.
	generate_foreign_code(mercury_module_name_to_mlds(ModuleName),
		ForeignCode),

	io__write_string("\n"),

		% Output the contents of foreign_proc declarations.
		% Put each one inside a method.
	list__foldl(generate_method_csharp_code(
		mercury_module_name_to_mlds(ModuleName)), Defns),

	io__write_string("};\n"),

		% Close the namespace braces.
	io__write_list(Namespace, "\n", 
		(pred(_N::in, di, uo) is det -->
			io__write_string("}")
	)),

	io__nl.


	% XXX we don't handle export decls.
:- pred generate_foreign_code(mlds_module_name, mlds__foreign_code,
		io__state, io__state).
:- mode generate_foreign_code(in, in, di, uo) is det.
generate_foreign_code(_ModuleName, 
		mlds__foreign_code(_RevHeaderCode, RevBodyCode,
			_ExportDefns)) -->
	{ BodyCode = list__reverse(RevBodyCode) },
	io__write_list(BodyCode, "\n", 
		(pred(llds__user_foreign_code(Lang, Code, _Context)::in,
				di, uo) is det -->
			( { Lang = csharp } ->
				io__write_string(Code)
			;
				{ sorry(this_file, 
					"foreign code other than MC++") }
			)					
	)).

	% XXX we don't handle export decls.
:- pred generate_foreign_header_code(mlds_module_name, mlds__foreign_code,
		io__state, io__state).
:- mode generate_foreign_header_code(in, in, di, uo) is det.
generate_foreign_header_code(_ModuleName, 
		mlds__foreign_code(RevHeaderCode, _RevBodyCode,
			_ExportDefns)) -->
	{ HeaderCode = list__reverse(RevHeaderCode) },
	io__write_list(HeaderCode, "\n", 
		(pred(llds__foreign_decl_code(Lang, Code, _Context)::in,
			di, uo) is det -->
			( { Lang = csharp } ->
				io__write_string(Code)
			;
				{ sorry(this_file, 
					"foreign code other than MC++") }
			)					
	)).

:- pred generate_method_csharp_code(mlds_module_name, mlds__defn,
		io__state, io__state).
:- mode generate_method_csharp_code(in, in, di, uo) is det.

	% XXX we don't handle export
generate_method_csharp_code(_, defn(export(_), _, _, _)) --> [].
generate_method_csharp_code(_, defn(data(_), _, _, _)) --> [].
generate_method_csharp_code(_, defn(type(_, _), _, _, _)) --> [].
generate_method_csharp_code(_ModuleName, 
		defn(function(PredLabel, ProcId, MaybeSeqNum, _PredId), 
	_Context, _DeclFlags, Entity)) -->

	( 
			% XXX we ignore the attributes
		{ Entity = mlds__function(_, Params, defined_here(Statement),
			_Attributes) },
		{ has_foreign_languages(Statement, Langs) },
		{ list__member(csharp, Langs) }
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
			% C# and IL don't support multiple return values
			sorry(this_file, "multiple return values")
		},


		{ predlabel_to_id(PredLabel, ProcId, MaybeSeqNum, Id) },
		io__write_string("public static "),
		write_il_ret_type_as_csharp_type(ReturnType),

		io__write_string(" "),

		io__write_string(Id),
		io__write_string("("),
		io__write_list(Inputs, ", ", write_input_arg_as_csharp_type),
		io__write_string(")"),
		io__nl,

		io__write_string("{\n"),
		write_csharp_statement(Statement),
		io__write_string("}\n")
	;
		[]
	).

:- pred write_csharp_statement(mlds__statement, io__state, io__state).
:- mode write_csharp_statement(in, di, uo) is det.
write_csharp_statement(statement(Statement, _Context)) -->
	( 
		{ Statement = atomic(outline_foreign_proc(csharp,
			_Lvals, Code)) } 
	->
		io__write_string(Code),
		io__nl
	;
		{ Statement = block(Defns, Statements) }
	->
		io__write_list(Defns, "", write_csharp_defn_decl),
		io__write_string("{\n"),
		io__write_list(Statements, "", write_csharp_statement),
		io__write_string("\n}\n")
	;
		{ Statement = return(Rvals) }
	->
		( { Rvals = [Rval] } ->
			io__write_string("return "),
			write_csharp_rval(Rval),
			io__write_string(";\n")
		;
			{ sorry(this_file, "multiple return values") }
		)
	;
		{ functor(Statement, SFunctor, _Arity) },
		{ sorry(this_file, "csharp output for " ++ SFunctor) }
	).

%-------------------------------------------------------------------
% code below here is not used.
%-------------------------------------------------------------------

	% XXX we ignore contexts
:- pred write_csharp_code_component(mlds__target_code_component, 
	io__state, io__state).
:- mode write_csharp_code_component(in, di, uo) is det.
write_csharp_code_component(user_target_code(Code, _MaybeContext, _Attrrs)) -->
	io__write_string(Code).
write_csharp_code_component(raw_target_code(Code, _Attrs)) -->
	io__write_string(Code).
		% XXX we don't handle name yet.
write_csharp_code_component(name(_)) --> [].
write_csharp_code_component(target_code_input(Rval)) -->
	write_csharp_rval(Rval).
write_csharp_code_component(target_code_output(Lval)) -->
	write_csharp_lval(Lval).

:- pred write_csharp_rval(mlds__rval, io__state, io__state).
:- mode write_csharp_rval(in, di, uo) is det.
write_csharp_rval(lval(Lval)) -->
	write_csharp_lval(Lval).
write_csharp_rval(mkword(_Tag, _Rval)) -->
	{ sorry(this_file, "mkword rval") }.
write_csharp_rval(const(RvalConst)) -->
	write_csharp_rval_const(RvalConst).
write_csharp_rval(unop(Unop, Rval)) -->
	( 
		{ Unop = std_unop(StdUnop) },
		{ c_util__unary_prefix_op(StdUnop, UnopStr) }
	->
		io__write_string(UnopStr),
		io__write_string("("),
		write_csharp_rval(Rval),
		io__write_string(")")
	;
		{ Unop = cast(Type) }
	->
		io__write_string("("),
		write_csharp_parameter_type(Type),
		io__write_string(") "),
		write_csharp_rval(Rval)
	;
		{ sorry(this_file, "box or unbox unop") }
	).
write_csharp_rval(binop(Binop, Rval1, Rval2)) -->
	( 
		{ c_util__binary_infix_op(Binop, BinopStr) }
	->
		io__write_string("("),
		write_csharp_rval(Rval1),
		io__write_string(") "),
		io__write_string(BinopStr),
		io__write_string(" ("),
		write_csharp_rval(Rval2),
		io__write_string(")")
	;
		{ sorry(this_file, "binop rval") }
	).

write_csharp_rval(mem_addr(_)) -->
	{ sorry(this_file, "mem_addr rval") }.
	
write_csharp_rval(self(_)) -->
	{ sorry(this_file, "self rval") }.
	
:- pred write_csharp_rval_const(mlds__rval_const, io__state, io__state).
:- mode write_csharp_rval_const(in, di, uo) is det.
write_csharp_rval_const(true) --> io__write_string("1").
write_csharp_rval_const(false) --> io__write_string("0").
write_csharp_rval_const(int_const(I)) --> io__write_int(I).
write_csharp_rval_const(float_const(F)) --> io__write_float(F).
	% XXX We don't quote this correctly.
write_csharp_rval_const(string_const(S)) --> 
	io__write_string(""""),
	c_util__output_quoted_string(S),
	io__write_string("""").
write_csharp_rval_const(multi_string_const(L, S)) --> 
	io__write_string(""""),
	c_util__output_quoted_multi_string(L, S),
	io__write_string("""").
write_csharp_rval_const(code_addr_const(CodeAddrConst)) --> 
	(
		{ CodeAddrConst = proc(ProcLabel, _FuncSignature) },
		{ mangle_mlds_proc_label(ProcLabel, no, ClassName,
			MangledName) },
		write_csharp_class_name(ClassName),
		io__write_string("."),
		io__write_string(MangledName)
	;
		{ CodeAddrConst = internal(ProcLabel, SeqNum,
			_FuncSignature) },
		{ mangle_mlds_proc_label(ProcLabel, yes(SeqNum), ClassName,
			MangledName) },
		write_csharp_class_name(ClassName),
		io__write_string("."),
		io__write_string(MangledName)
	).



write_csharp_rval_const(data_addr_const(_)) --> 
	{ sorry(this_file, "data_addr_const rval") }.
write_csharp_rval_const(null(_)) --> 
	io__write_string("null").

:- pred write_csharp_lval(mlds__lval, io__state, io__state).
:- mode write_csharp_lval(in, di, uo) is det.
write_csharp_lval(field(_, Rval, named_field(FieldId, _Type), _, _)) -->
	io__write_string("("),
	write_csharp_rval(Rval),
	io__write_string(")"),
	io__write_string("."),
	{ FieldId = qual(_, FieldName) },
	io__write_string(FieldName).

write_csharp_lval(field(_, Rval, offset(OffSet), _, _)) -->
	io__write_string("("),
	write_csharp_rval(Rval),
	io__write_string(")"),
	io__write_string("["),
	write_csharp_rval(OffSet),
	io__write_string("]").

write_csharp_lval(mem_ref(Rval, _)) -->
	io__write_string("*"),
	write_csharp_rval(Rval).
write_csharp_lval(var(Var, _VarType)) -->
	{ Var = qual(_, VarName) },
	write_mlds_var_name_for_parameter(VarName).

:- pred write_csharp_defn_decl(mlds__defn, io__state, io__state).
:- mode write_csharp_defn_decl(in, di, uo) is det.
write_csharp_defn_decl(Defn) -->
	{ Defn = mlds__defn(Name, _Context, _Flags, DefnBody) },
	(
		{ DefnBody = data(Type, _Initializer) },
		{ Name = data(var(VarName)) }
	->
		write_csharp_parameter_type(Type),
		io__write_string(" "),
		write_mlds_var_name_for_parameter(VarName),
		io__write_string(";\n")
	;
		% XXX we should implement others
		{ sorry(this_file, "data_addr_const rval") }
	).

:- pred write_csharp_parameter_type(mlds__type, io__state, io__state).
:- mode write_csharp_parameter_type(in, di, uo) is det.
write_csharp_parameter_type(Type) -->
	get_il_data_rep(DataRep),
	{ ILType = mlds_type_to_ilds_type(DataRep, Type) },
	write_il_type_as_csharp_type(ILType).

:- pred type_is_byref_type(mlds__type, mlds__type).
:- mode type_is_byref_type(in, out) is semidet.
type_is_byref_type(Type, InnerType) :-
	Type = mlds__ptr_type(InnerType).

:- pred write_il_ret_type_as_csharp_type(ret_type::in,
	io__state::di, io__state::uo) is det.
write_il_ret_type_as_csharp_type(void) --> io__write_string("void").
write_il_ret_type_as_csharp_type(simple_type(T)) --> 
	write_il_simple_type_as_csharp_type(T).

	% XXX need to revisit this and choose types appropriately
:- pred write_il_simple_type_as_csharp_type(simple_type::in,
	io__state::di, io__state::uo) is det.
write_il_simple_type_as_csharp_type(int8) --> 
	io__write_string("sbyte").
write_il_simple_type_as_csharp_type(int16) --> 
	io__write_string("short").
write_il_simple_type_as_csharp_type(int32) --> 
	io__write_string("int").
write_il_simple_type_as_csharp_type(int64) --> 
	io__write_string("long").
write_il_simple_type_as_csharp_type(uint8) --> 
	io__write_string("byte").
write_il_simple_type_as_csharp_type(uint16) --> 
	io__write_string("ushort").
write_il_simple_type_as_csharp_type(uint32) --> 
	io__write_string("uint").
write_il_simple_type_as_csharp_type(uint64) --> 
	io__write_string("ulong").
write_il_simple_type_as_csharp_type(native_int) --> 
	io__write_string("int").
write_il_simple_type_as_csharp_type(native_uint) --> 
	io__write_string("uint").
write_il_simple_type_as_csharp_type(float32) --> 
	io__write_string("float").
write_il_simple_type_as_csharp_type(float64) --> 
	io__write_string("double").
write_il_simple_type_as_csharp_type(native_float) --> 
	io__write_string("float").
write_il_simple_type_as_csharp_type(bool) --> 
	io__write_string("bool").
write_il_simple_type_as_csharp_type(char) --> 
	io__write_string("char").
write_il_simple_type_as_csharp_type(refany) --> 
	io__write_string("mercury.MR_RefAny").
write_il_simple_type_as_csharp_type(class(ClassName)) --> 
	write_csharp_class_name(ClassName).
write_il_simple_type_as_csharp_type(value_class(_ClassName)) --> 
	{ sorry(this_file, "value classes") }.
write_il_simple_type_as_csharp_type(interface(_ClassName)) --> 
	{ sorry(this_file, "interfaces") }.
write_il_simple_type_as_csharp_type('[]'(Type, Bounds)) --> 
	write_il_type_as_csharp_type(Type),
	io__write_string("[]"),
	( { Bounds = [] } ->
		[]
	;
		{ sorry(this_file, "arrays with bounds") }
	).
write_il_simple_type_as_csharp_type('&'(Type)) --> 
		% XXX is this always right?
	io__write_string("ref "),
	write_il_type_as_csharp_type(Type).
write_il_simple_type_as_csharp_type('*'(Type)) --> 
	write_il_type_as_csharp_type(Type),
	io__write_string(" *").

:- pred write_csharp_class_name(structured_name::in, io__state::di,
	io__state::uo) is det.
write_csharp_class_name(structured_name(_Assembly, DottedName, NestedClasses)) -->
	io__write_list(DottedName ++ NestedClasses, ".", io__write_string).

:- pred write_il_type_as_csharp_type(ilds__type::in,
	io__state::di, io__state::uo) is det.
write_il_type_as_csharp_type(ilds__type(Modifiers, SimpleType)) -->
	io__write_list(Modifiers, " ", 
		write_il_type_modifier_as_csharp_type),
	write_il_simple_type_as_csharp_type(SimpleType).

:- pred write_il_type_modifier_as_csharp_type(ilds__type_modifier::in,
	io__state::di, io__state::uo) is det.
write_il_type_modifier_as_csharp_type(const) --> 
	io__write_string("const").
write_il_type_modifier_as_csharp_type(readonly) --> 
	io__write_string("readonly").
write_il_type_modifier_as_csharp_type(volatile) --> 
	io__write_string("volatile").

:- pred write_input_arg_as_csharp_type(
	pair(mlds__entity_name, mlds__type)::in,
	io__state::di, io__state::uo) is det.
write_input_arg_as_csharp_type(EntityName - Type) --> 
	get_il_data_rep(DataRep),
	write_il_type_as_csharp_type(mlds_type_to_ilds_type(DataRep, Type)),
	io__write_string(" "),
	( { EntityName = data(var(VarName)) } ->
		write_mlds_var_name_for_parameter(VarName)
	;
		{ error("found a variable in a list") }
	).

:- pred write_mlds_var_name_for_local(mlds__var_name::in,
	io__state::di, io__state::uo) is det.
write_mlds_var_name_for_local(var_name(Name, MaybeNum)) -->
	io__write_string(Name),
	( { MaybeNum = yes(Num) } ->
		io__write_string("_"),
		io__write_int(Num)
	;
		[]
	).

:- pred write_mlds_var_name_for_parameter(mlds__var_name::in,
	io__state::di, io__state::uo) is det.
write_mlds_var_name_for_parameter(var_name(Name, _)) -->
	io__write_string(Name).

:- func this_file = string.
this_file = "mlds_to_csharp.m".

:- end_module mlds_to_csharp.
