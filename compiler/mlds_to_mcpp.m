%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% mlds_to_mcpp - Generate Managed C++ code for the foreign language
% interface.
% Main author: trd.
%
% To-do:
%
% [ ] Fix the output of contexts so that the context gets reset after the
%     user code.
% [ ] Output contexts in more places, currently we don't do all of them.
%
% This code converts the MLDS representation of foreign language code into MC++ 

:- module mlds_to_mcpp.
:- interface.

:- import_module mlds.
:- import_module io.

	% Convert the MLDS to MC++ and write it to a file.
:- pred mlds_to_mcpp__output_mcpp_code(mlds, io__state, io__state).
:- mode mlds_to_mcpp__output_mcpp_code(in, di, uo) is det.

	% Print the header comments of the output module
:- pred output_src_start(mercury_module_name, io__state, io__state).
:- mode output_src_start(in, di, uo) is det.

	% Print the footer commments of the output module
:- pred output_src_end(mercury_module_name, io__state, io__state).
:- mode output_src_end(in, di, uo) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module globals, options, passes_aux.
:- import_module builtin_ops, c_util, modules, tree.
:- import_module hlds_pred. % for `pred_proc_id'.
:- import_module prog_data, prog_out, llds_out.
:- import_module rtti, type_util, error_util.

:- import_module ilds, ilasm, il_peephole.
:- import_module ml_util, ml_code_util.
:- import_module mlds_to_c. /* to output C code for .cpp files */
:- use_module llds. /* for user_c_code */

:- import_module bool, int, map, string, list, assoc_list, term, std_util.
:- import_module library, require, counter.

:- import_module mlds_to_il.

%-----------------------------------------------------------------------------%

output_mcpp_code(MLDS) -->
	{ MLDS = mlds(ModuleName, _ForeignCode, _Imports, _Defns) },
	output_src_start(ModuleName), 
	io__nl,

	generate_mcplusplus_code(MLDS),

	output_src_end(ModuleName).

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

:- pred generate_mcplusplus_code(mlds, io__state, io__state).
:- mode generate_mcplusplus_code(in, di, uo) is det.
generate_mcplusplus_code(MLDS) -->

	{ MLDS = mlds(ModuleName, AllForeignCode, _Imports, Defns) },
	{ prog_out__sym_name_to_string(ModuleName, ModuleNameStr) },
	{ ClassName = class_name(mercury_module_name_to_mlds(ModuleName),
			wrapper_class_name) },

	io__nl,
	io__write_strings([
		"#using <mscorlib.dll>\n",
		"#include ""mercury_mcpp.h""\n",
		"#using ""mercury_mcpp.dll""\n",
		"#using ""mercury_il.dll""\n",
		"#using """, ModuleNameStr, ".dll""\n",

		% XXX We have to use the mercury namespace, as
		% llds_out still generates some of the code used in the
		% MC++ interface, and so it doesn't have "mercury::"
		% namespace qualifiers.
		"using namespace mercury;\n",

		% XXX this supresses problems caused by references to 
		% float.  If you don't do this, you'll get link errors.
		% Revisit this when the .NET implementation has matured.
		"extern ""C"" int _fltused=0;\n",
		"\n"]),

	globals__io_lookup_bool_option(sign_assembly, SignAssembly),
	( { SignAssembly = yes },
		io__write_string("[assembly:System::Reflection::AssemblyKeyFileAttribute(\"mercury.sn\")];\n")
	; { SignAssembly = no },
		[]
	),

	{ Namespace0 = get_class_namespace(ClassName) },
	{ list__reverse(Namespace0) = [Head | Tail] ->
		Namespace = list__reverse([Head ++ "__cpp_code" | Tail])
	;
		Namespace = Namespace0
	},

	io__write_list(Namespace, "\n", 
		(pred(N::in, di, uo) is det -->
			io__format("namespace %s {", [s(N)])
	)),

		% Get the foreign code for MC++
	{ ForeignCode = map__lookup(AllForeignCode, managed_cplusplus) },
	generate_foreign_header_code(mercury_module_name_to_mlds(ModuleName),
		ForeignCode),

	io__write_strings([
		"\n__gc public class " ++ wrapper_class_name,
		"{\n",
		"public:\n"]),


		% Output the contents of foreign_code declarations.
	generate_foreign_code(mercury_module_name_to_mlds(ModuleName),
		ForeignCode),

		% Output the contents of foreign_proc declarations. 
		% Put each one inside a method.
	list__foldl(generate_method_mcpp_code(
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
		(pred(llds__user_foreign_code(Lang, Code, Context)::in,
				di, uo) is det -->
			( { Lang = managed_cplusplus } ->
				mlds_to_c__output_context(mlds__make_context(
					Context)),
				io__write_string(Code)
			;
				% ignore it if it isn't MC++
				[]
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
			( { Lang = managed_cplusplus } ->
				io__write_string(Code)
			;
				% ignore it if it isn't MC++
				[]
			)					
	)).

:- pred generate_method_mcpp_code(mlds_module_name, mlds__defn,
		io__state, io__state).
:- mode generate_method_mcpp_code(in, in, di, uo) is det.

	% XXX we don't handle export
generate_method_mcpp_code(_, defn(export(_), _, _, _)) --> [].
generate_method_mcpp_code(_, defn(data(_), _, _, _)) --> [].
generate_method_mcpp_code(_, defn(type(_, _), _, _, _)) --> [].
generate_method_mcpp_code(ModuleName, 
		defn(function(PredLabel, ProcId, MaybeSeqNum, _PredId), 
	_Context, _DeclFlags, Entity)) -->
	( 
			% XXX we ignore the attributes
		{ Entity = mlds__function(_, Params,
			defined_here(Statement), _) },
		( 
			{ has_inline_target_code_statement(Statement) }
		;
			{ has_foreign_languages(Statement, Langs) },
			{ list__member(managed_cplusplus, Langs) }
		)
	->
		get_il_data_rep(DataRep),
		{ ILSignature = params_to_il_signature(DataRep, ModuleName,
			Params) },
		{ predlabel_to_id(PredLabel, ProcId, MaybeSeqNum, Id) },
		io__write_string("static "),
		{ ILSignature = signature(_CallConv, ReturnType, ILArgs) },
		write_il_ret_type_as_managed_cpp_type(ReturnType),

		io__write_string(" "),

		io__write_string(Id),
		io__write_string("("),
		io__write_list(ILArgs, ", ", write_il_arg_as_managed_cpp_type),
		io__write_string(")"),
		io__nl,

		io__write_string("{\n"),
		write_managed_cpp_statement(Statement),
		io__write_string("}\n")
	;
		[]
	).

	% In order to implement the C interface, you need to
	% implement:
	%	call/6 (for calling continuations)
	%	return/1 (for returning succeeded)
	% 	block/2 (because the code is wrapped in a block, and
	%		because local variables are declared for
	%		"succeeded")
	% 	target_code/2 (where the actual code is put)
	%	assign/2 (to assign to the environment)
	%	newobj/7 (to create an environment)
	%
	% Unfortunately currently some of the "raw_target_code" is
	% C specific and won't translate well into managed C++.
	% Probably the best solution to this is to introduce some new
	% code components.
	%
	% Note that for the managed C++ backend there is a problem.
	% #import doesn't import classes in namespaces properly (yet), so we
	% can't #import .dlls that define environments.  So even if we
	% implement newobj/7, we will get errors.  
	% The work-around for this is to make sure ml_elim_nested
	% doesn't introduce environments where they aren't needed,
	% so we don't generally have to allocate anything but the local
	% environment (which is defined locally).

:- pred write_managed_cpp_statement(mlds__statement, 
	io__state, io__state).
:- mode write_managed_cpp_statement(in, di, uo) is det.
write_managed_cpp_statement(Statement) -->
	get_il_data_rep(ILDataRep),
	( 
			% XXX this ignores the language target.
		{ Statement = statement(atomic(inline_target_code(
			_Lang, CodeComponents)), _) } 
	->
		io__write_list(CodeComponents, "\n", 
			write_managed_cpp_code_component)
	;
		{ Statement = statement(block(Defns, Statements), _) }
	->
		io__write_list(Defns, "", write_managed_cpp_defn_decl),
		io__write_string("{\n"),
		io__write_list(Statements, "", write_managed_cpp_statement),
		io__write_string("}\n")
	;
		{ Statement = statement(
			call(_Sig, Function, _This, Args, Results, _IsTail), 
				_Context) }
	->
		% XXX this doesn't work for continuations because 
		% a) I don't know how to call a function pointer in
		%    managed C++.
		% b) Function pointers are represented as integers,
		%    and we don't do any casting for them.
		% The nondet interface might need to be reworked in
		% this case.
		% The workaround at the moment is to make sure we don't
		% actually generate calls to continuations in managed
		% C++, instead we generate a nested function that is
		% implemented in IL that does the continuation call, and
		% just call the nested function instead.  Sneaky, eh?
		( { Results = [] } ->
			[]
		; { Results = [Lval] } ->
			write_managed_cpp_lval(Lval),
			io__write_string(" = ")
		;
			{ sorry(this_file, "multiple return values") }
		),
		write_managed_cpp_rval(Function),
		io__write_string("("),
		io__write_list(Args, ", ", write_managed_cpp_rval),
		io__write_string(");\n")
	;
		{ Statement = statement(return(Rvals), _) }
	->
		( { Rvals = [Rval] } ->
			io__write_string("return "),
			write_managed_cpp_rval(Rval),
			io__write_string(";\n")
		;
			{ sorry(this_file, "multiple return values") }
		)
	;
		{ Statement = statement(atomic(assign(Lval, Rval)), _) }
	->
		write_managed_cpp_lval(Lval),
		io__write_string(" = "),
		write_managed_cpp_rval(Rval),
		io__write_string(";\n")
	;

			% XXX This is not fully implemented
		{ Statement = statement(atomic(
			new_object(Target, _MaybeTag, Type, _MaybeSize, 
				_MaybeCtorName, _Args, _ArgTypes)), _) },
		{ ClassName = mlds_type_to_ilds_class_name(ILDataRep, Type) }
	->
		write_managed_cpp_lval(Target),
		io__write_string(" = new "),
		write_managed_cpp_class_name(ClassName),
		io__write_string("();\n")
	;
		{ Statement = statement(atomic(Atomic), _) }
	->
		{ functor(Atomic, AtomicFunctor, Arity) },
		io__write_string("// unimplemented: atomic "), 
		io__write_string(AtomicFunctor), 
		io__write_string("/"), 
		io__write(Arity),
		io__nl

	;
		{ Statement = statement(S, _) },
		{ functor(S, SFunctor, Arity) },
		io__write_string("// unimplemented: "), 
		io__write_string(SFunctor), 
		io__write_string("/"), 
		io__write(Arity),
		io__nl
	).

	% XXX we ignore contexts
:- pred write_managed_cpp_code_component(mlds__target_code_component, 
	io__state, io__state).
:- mode write_managed_cpp_code_component(in, di, uo) is det.
write_managed_cpp_code_component(user_target_code(Code, _MaybeContext,
		_Attrs)) -->
	io__write_string(Code).
write_managed_cpp_code_component(raw_target_code(Code, _Attrs)) -->
	io__write_string(Code).
		% XXX we don't handle name yet.
write_managed_cpp_code_component(name(_)) --> [].
write_managed_cpp_code_component(target_code_input(Rval)) -->
	write_managed_cpp_rval(Rval).
write_managed_cpp_code_component(target_code_output(Lval)) -->
	write_managed_cpp_lval(Lval).

:- pred write_managed_cpp_rval(mlds__rval, io__state, io__state).
:- mode write_managed_cpp_rval(in, di, uo) is det.
write_managed_cpp_rval(lval(Lval)) -->
	write_managed_cpp_lval(Lval).
write_managed_cpp_rval(mkword(_Tag, _Rval)) -->
	io__write_string(" /* mkword rval -- unimplemented */ ").
write_managed_cpp_rval(const(RvalConst)) -->
	write_managed_cpp_rval_const(RvalConst).
write_managed_cpp_rval(unop(Unop, Rval)) -->
	( 
		{ Unop = std_unop(StdUnop) },
		{ c_util__unary_prefix_op(StdUnop, UnopStr) }
	->
		io__write_string(UnopStr),
		io__write_string("("),
		write_managed_cpp_rval(Rval),
		io__write_string(")")
	;
		{ Unop = cast(Type) }
	->
		io__write_string("("),
		write_managed_cpp_type(Type),
		io__write_string(") "),
		write_managed_cpp_rval(Rval)
	;
		io__write_string(" /* XXX box or unbox unop -- unimplemented */ "),
		write_managed_cpp_rval(Rval)
	).
write_managed_cpp_rval(binop(Binop, Rval1, Rval2)) -->
	( 
		{ c_util__binary_infix_op(Binop, BinopStr) }
	->
		io__write_string("("),
		write_managed_cpp_rval(Rval1),
		io__write_string(") "),
		io__write_string(BinopStr),
		io__write_string(" ("),
		write_managed_cpp_rval(Rval2),
		io__write_string(")")
	;
		io__write_string(" /* binop rval -- unimplemented */ ")
	).

write_managed_cpp_rval(mem_addr(_)) -->
	io__write_string(" /* mem_addr rval -- unimplemented */ ").

write_managed_cpp_rval(self(_)) -->
	io__write_string(" /* self rval -- unimplemented */ ").
	
:- pred write_managed_cpp_rval_const(mlds__rval_const, io__state, io__state).
:- mode write_managed_cpp_rval_const(in, di, uo) is det.
write_managed_cpp_rval_const(true) --> io__write_string("1").
write_managed_cpp_rval_const(false) --> io__write_string("0").
write_managed_cpp_rval_const(int_const(I)) --> io__write_int(I).
write_managed_cpp_rval_const(float_const(F)) --> io__write_float(F).
	% XXX We don't quote this correctly.
write_managed_cpp_rval_const(string_const(S)) --> 
	io__write_string(""""),
	io__write_string(S),
	io__write_string("""").
write_managed_cpp_rval_const(multi_string_const(_L, _S)) --> 
	io__write_string(" /* multi_string_const rval -- unimplemented */ ").
write_managed_cpp_rval_const(code_addr_const(CodeAddrConst)) --> 
	(
		{ CodeAddrConst = proc(ProcLabel, _FuncSignature) },
		{ mangle_mlds_proc_label(ProcLabel, no, ClassName,
			MangledName) },
		write_managed_cpp_class_name(ClassName),
		io__write_string("::"),
		io__write_string(MangledName)
	;
		{ CodeAddrConst = internal(ProcLabel, SeqNum,
			_FuncSignature) },
		{ mangle_mlds_proc_label(ProcLabel, yes(SeqNum), ClassName,
			MangledName) },
		write_managed_cpp_class_name(ClassName),
		io__write_string("::"),
		io__write_string(MangledName)
	).



write_managed_cpp_rval_const(data_addr_const(_)) --> 
	io__write_string(" /* data_addr_const rval -- unimplemented */ ").
write_managed_cpp_rval_const(null(_)) --> 
	io__write_string("0").

:- pred write_managed_cpp_lval(mlds__lval, io__state, io__state).
:- mode write_managed_cpp_lval(in, di, uo) is det.
write_managed_cpp_lval(field(_, Rval, named_field(FieldId, _Type), _, _)) -->
	io__write_string("("),
	write_managed_cpp_rval(Rval),
	io__write_string(")"),
	io__write_string("->"),
	{ FieldId = qual(_, FieldName) },
	io__write_string(FieldName).

write_managed_cpp_lval(field(_, Rval, offset(OffSet), _, _)) -->
	io__write_string("("),
	write_managed_cpp_rval(Rval),
	io__write_string(")"),
	io__write_string("["),
	write_managed_cpp_rval(OffSet),
	io__write_string("]").

write_managed_cpp_lval(mem_ref(Rval, _)) -->
	io__write_string("*"),
	write_managed_cpp_rval(Rval).
write_managed_cpp_lval(var(Var, _VarType)) -->
	{ mangle_mlds_var(Var, Id) },
	io__write_string(Id).

:- pred write_managed_cpp_defn_decl(mlds__defn, io__state, io__state).
:- mode write_managed_cpp_defn_decl(in, di, uo) is det.
write_managed_cpp_defn_decl(Defn) -->
	{ Defn = mlds__defn(Name, _Context, _Flags, DefnBody) },
	( { DefnBody = data(Type, _Initializer) },
  	  { Name = data(var(VarName)) }
	->
		write_managed_cpp_type(Type),
		io__write_string(" "),
		write_mlds_varname(VarName),
		io__write_string(";\n")
	;
		io__write_string("// unimplemented defn decl\n")
	).

:- pred write_mlds_varname(mlds__var_name, io__state, io__state).
:- mode write_mlds_varname(in, di, uo) is det.
write_mlds_varname(var_name(Var, yes(Num))) -->
	io__format("%s_%d", [s(Var), i(Num)]).
write_mlds_varname(var_name(Var, no)) -->
	io__write_string(Var).

:- pred write_managed_cpp_type(mlds__type, io__state, io__state).
:- mode write_managed_cpp_type(in, di, uo) is det.
write_managed_cpp_type(Type) -->
	get_il_data_rep(DataRep),
	write_il_type_as_managed_cpp_type(
		mlds_type_to_ilds_type(DataRep, Type)).

	% XXX this could be more efficient
:- pred has_inline_target_code_statement(mlds__statement).
:- mode has_inline_target_code_statement(in) is semidet.
has_inline_target_code_statement(Statement) :-
	GetTargetCode = (pred(SubStatement::out) is nondet :-
		statement_contains_statement(Statement, SubStatement),
		SubStatement = statement(atomic(inline_target_code(_, _)), _) 
		),
	solutions(GetTargetCode, [_|_]).



:- pred write_il_ret_type_as_managed_cpp_type(ret_type::in,
	io__state::di, io__state::uo) is det.
write_il_ret_type_as_managed_cpp_type(void) --> io__write_string("void").
write_il_ret_type_as_managed_cpp_type(simple_type(T)) --> 
	write_il_simple_type_as_managed_cpp_type(T).

	% XXX need to revisit this and choose types appropriately
:- pred write_il_simple_type_as_managed_cpp_type(simple_type::in,
	io__state::di, io__state::uo) is det.
write_il_simple_type_as_managed_cpp_type(int8) --> 
	io__write_string("mercury::MR_Integer8").
write_il_simple_type_as_managed_cpp_type(int16) --> 
	io__write_string("mercury::MR_Integer16").
write_il_simple_type_as_managed_cpp_type(int32) --> 
	io__write_string("mercury::MR_Integer").
write_il_simple_type_as_managed_cpp_type(int64) --> 
	io__write_string("mercury::MR_Integer64").
write_il_simple_type_as_managed_cpp_type(uint8) --> 
	io__write_string("unsigned int").
write_il_simple_type_as_managed_cpp_type(uint16) --> 
	io__write_string("unsigned int").
write_il_simple_type_as_managed_cpp_type(uint32) --> 
	io__write_string("unsigned int").
write_il_simple_type_as_managed_cpp_type(uint64) --> 
	io__write_string("unsigned int").
write_il_simple_type_as_managed_cpp_type(native_int) --> 
	io__write_string("mercury::MR_Integer").
write_il_simple_type_as_managed_cpp_type(native_uint) --> 
	io__write_string("unsigned int").
write_il_simple_type_as_managed_cpp_type(float32) --> 
	io__write_string("float").
write_il_simple_type_as_managed_cpp_type(float64) --> 
	io__write_string("mercury::MR_Float").
write_il_simple_type_as_managed_cpp_type(native_float) --> 
	io__write_string("mercury::MR_Float").
write_il_simple_type_as_managed_cpp_type(bool) --> 
	io__write_string("mercury::MR_Integer").
write_il_simple_type_as_managed_cpp_type(char) --> 
	io__write_string("mercury::MR_Char").
write_il_simple_type_as_managed_cpp_type(refany) --> 
	io__write_string("mercury::MR_RefAny").
write_il_simple_type_as_managed_cpp_type(class(ClassName)) --> 
	( { ClassName = il_generic_class_name } ->
		io__write_string("mercury::MR_Box")
	;
		io__write_string("public class "),
		write_managed_cpp_class_name(ClassName),
		io__write_string(" *")
	).
		% XXX this is not the right syntax
write_il_simple_type_as_managed_cpp_type(value_class(ClassName)) --> 
	io__write_string("value class "),
	write_managed_cpp_class_name(ClassName),
	io__write_string(" *").
		% XXX this is not the right syntax
write_il_simple_type_as_managed_cpp_type(interface(ClassName)) --> 
	io__write_string("interface "),
	write_managed_cpp_class_name(ClassName),
	io__write_string(" *").
		% XXX this needs more work
write_il_simple_type_as_managed_cpp_type('[]'(_Type, _Bounds)) --> 
	io__write_string("mercury::MR_Word").
write_il_simple_type_as_managed_cpp_type('&'(Type)) --> 
	io__write_string("MR_Ref("),
	write_il_type_as_managed_cpp_type(Type),
	io__write_string(")").
write_il_simple_type_as_managed_cpp_type('*'(Type)) --> 
	write_il_type_as_managed_cpp_type(Type),
	io__write_string(" *").

:- pred write_managed_cpp_class_name(structured_name::in, io__state::di,
	io__state::uo) is det.
write_managed_cpp_class_name(structured_name(_Assembly, DottedName,
		NestedClasses)) -->
	io__write_list(DottedName ++ NestedClasses, "::", io__write_string).

:- pred write_il_type_as_managed_cpp_type(ilds__type::in,
	io__state::di, io__state::uo) is det.
write_il_type_as_managed_cpp_type(ilds__type(Modifiers, SimpleType)) -->
	io__write_list(Modifiers, " ", 
		write_il_type_modifier_as_managed_cpp_type),
	write_il_simple_type_as_managed_cpp_type(SimpleType).

:- pred write_il_type_modifier_as_managed_cpp_type(ilds__type_modifier::in,
	io__state::di, io__state::uo) is det.
write_il_type_modifier_as_managed_cpp_type(const) --> 
	io__write_string("const").
write_il_type_modifier_as_managed_cpp_type(readonly) --> 
	io__write_string("readonly").
write_il_type_modifier_as_managed_cpp_type(volatile) --> 
	io__write_string("volatile").

:- pred write_il_arg_as_managed_cpp_type(pair(ilds__type,
	maybe(ilds__id))::in, io__state::di, io__state::uo) is det.
write_il_arg_as_managed_cpp_type(Type - MaybeId) --> 
	write_il_type_as_managed_cpp_type(Type),
	( { MaybeId = yes(Id) } ->
		io__write_string(" "),
		io__write_string(Id)
	;
		% XXX should make up a name!
		{ sorry(this_file, "unnamed arguments in method parameters") }
	).


:- func this_file = string.
this_file = "mlds_to_mcpp.m".

