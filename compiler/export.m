%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module defines predicates to produce the functions which are
% exported to a foreign language via a `pragma export' declaration.

% Note: any changes here might also require similar changes to the handling
% of `pragma import' declarations, which are handled in make_hlds.m.

% Main authors: dgj.

%-----------------------------------------------------------------------------%

:- module backend_libs__export.

:- interface.

:- import_module parse_tree__prog_data, hlds__hlds_module.
:- import_module backend_libs__foreign.
:- import_module io.

	% From the module_info, get a list of foreign_export_decls,
	% each of which holds information about the declaration
	% of a foreign function named in a `pragma export' declaration,
	% which is used to allow a call to be made to a Mercury
	% procedure from the foreign language.
:- pred export__get_foreign_export_decls(module_info, foreign_export_decls).
:- mode export__get_foreign_export_decls(in, out) is det.

	% From the module_info, get a list of foreign_export_defns,
	% each of which is a string containing the foreign code
	% for defining a foreign function named in a `pragma export' decl.
:- pred export__get_foreign_export_defns(module_info, foreign_export_defns).
:- mode export__get_foreign_export_defns(in, out) is det.

	% Produce an interface file containing declarations for the
	% exported foreign functions (if required in this foreign
	% language).
:- pred export__produce_header_file(foreign_export_decls, module_name,
					io__state, io__state).
:- mode export__produce_header_file(in, in, di, uo) is det.


%-----------------------------------------------------------------------------%

% Utilities for generating C code which interfaces with Mercury.  
% The {MLDS,LLDS}->C backends and fact tables use this code.

	% Generate C code to convert an rval (represented as a string), from
	% a C type to a mercury C type (ie. convert strings and floats to
	% words) and return the resulting C code as a string.
:- pred convert_type_to_mercury(string, type, string).
:- mode convert_type_to_mercury(in, in, out) is det.

	% Generate C code to convert an rval (represented as a string), from
	% a mercury C type to a C type. (ie. convert words to strings and
	% floats if required) and return the resulting C code as a string.
:- pred convert_type_from_mercury(string, type, string).
:- mode convert_type_from_mercury(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__foreign.
:- import_module parse_tree__modules.
:- import_module hlds__hlds_pred, check_hlds__type_util.
:- import_module backend_libs__code_model.
:- import_module ll_backend__code_gen, ll_backend__code_util.
:- import_module ll_backend__llds_out.
:- import_module libs__globals, libs__options.

:- import_module term, varset.
:- import_module library, map, int, string, std_util, assoc_list, require.
:- import_module list, bool.

%-----------------------------------------------------------------------------%

export__get_foreign_export_decls(HLDS, C_ExportDecls) :-
	module_info_get_predicate_table(HLDS, PredicateTable),
	predicate_table_get_preds(PredicateTable, Preds),
	module_info_get_pragma_exported_procs(HLDS, ExportedProcs),
	module_info_globals(HLDS, Globals),
	export__get_foreign_export_decls_2(Preds, ExportedProcs, Globals,
		HLDS, C_ExportDecls).

:- pred export__get_foreign_export_decls_2(pred_table,
		list(pragma_exported_proc), globals,
		module_info, list(foreign_export_decl)).
:- mode export__get_foreign_export_decls_2(in, in, in, in, out) is det.

export__get_foreign_export_decls_2(_Preds, [], _, _, []).
export__get_foreign_export_decls_2(Preds, [E|ExportedProcs], Globals, Module,
		C_ExportDecls) :-
	E = pragma_exported_proc(PredId, ProcId, C_Function, _Ctxt),
	get_export_info(Preds, PredId, ProcId, Globals, Module, _HowToDeclare,
		C_RetType, _DeclareReturnVal, _FailureAction, _SuccessAction,
		HeadArgInfoTypes),
	get_argument_declarations(HeadArgInfoTypes, no, Module, ArgDecls),
	C_ExportDecl = foreign_export_decl(c, C_RetType, C_Function, ArgDecls),
	export__get_foreign_export_decls_2(Preds, ExportedProcs, Globals,
		Module, C_ExportDecls0),
	C_ExportDecls = [C_ExportDecl | C_ExportDecls0].

%-----------------------------------------------------------------------------%

export__get_foreign_export_defns(Module, ExportedProcsCode) :-
	module_info_get_pragma_exported_procs(Module, ExportedProcs),
	module_info_get_predicate_table(Module, PredicateTable),
	predicate_table_get_preds(PredicateTable, Preds),
	export__to_c(Preds, ExportedProcs, Module, ExportedProcsCode).

	% For each exported procedure, produce a C function.
	% The code we generate is in the form
	%
	% MR_declare_entry(<label of called proc>); /* or MR_declare_static */
	%
	% #if SEMIDET
	%   MR_bool
	% #elif FUNCTION
	%   MR_Word
	% #else
	%   void
	% #endif
	% <function name>(MR_Word Mercury__Argument1, 
	%			MR_Word *Mercury__Argument2...)
	%			/* Word for input, Word* for output */
	% {
	% #if MR_NUM_REAL_REGS > 0
	%	MR_Word c_regs[MR_NUM_REAL_REGS];
	% #endif
	% #if FUNCTION
	%	MR_Word retval;
	% #endif
	% #if MR_THREAD_SAFE
	% 	MR_bool must_finalize_engine;
	% #endif 
	% #if MR_DEEP_PROFILING
	%	MR_CallSiteDynamic *saved_call_site_addr
	%				= MR_current_callback_site;
	%	MR_CallSiteDynamic *saved_csd;
	% #endif
	%
	%		/* save the registers that our C caller may be using */
	%	MR_save_regs_to_mem(c_regs);
	%
	%		/* 
	%		** start a new Mercury engine inside this POSIX 
	%		** thread, if necessary (the C code may be 
	%		** multi-threaded itself).
	%		*/
	%
	% #if MR_THREAD_SAFE
	% 	must_finalize_engine = MR_init_thread(MR_use_now);
	% #endif 
	%
	% #if MR_DEEP_PROFILING
	%	saved_csd = MR_current_call_site_dynamic;
	%	MR_setup_callback(MR_ENTRY(<label of called proc>));
	% #endif
	%		/* 
	%		** restore Mercury's registers that were saved as
	%		** we entered C from Mercury.  For single threaded
	%		** programs the process must always start in Mercury
	%		** so that we can MR_init_engine() etc.  For
	%		** multi-threaded MR_init_thread (above) takes care
	%		** of making a new engine if required.
	%		*/
	%	MR_restore_registers();
	%	<copy input arguments from Mercury__Arguments into registers>
	%		/* save the registers which may be clobbered      */
	%		/* by the C function call MR_call_engine().       */
	%	MR_save_transient_registers();
	%
	%	(void) MR_call_engine(MR_ENTRY(<label of called proc>),
	%			MR_FALSE);
	%
	%		/* restore the registers which may have been      */
	%		/* clobbered by the return from the C function    */
	%		/* MR_call_engine()				  */
	%	MR_restore_transient_registers();
	% #if MR_DEEP_PROFILING
	% 	MR_current_call_site_dynamic = saved_csd;
	%	MR_current_callback_site = saved_call_site_addr;
	% #endif
	% #if SEMIDET
	%	if (!MR_r1) {
	%		MR_restore_regs_from_mem(c_regs);
	%		return MR_FALSE;
	%	}
	% #elif FUNCTION
	%	<copy return value register into retval>
	% #endif
	%	<copy output args from registers into *Mercury__Arguments>
	% #if MR_THREAD_SAFE
	% 	if (must_finalize_engine) {
	% 		MR_finalize_thread_engine();
	% 	}
	% #endif 
	%	MR_restore_regs_from_mem(c_regs);
	% #if SEMIDET
	%	return MR_TRUE;
	% #elif FUNCTION
	%	return retval;
	% #endif
	% }
:- pred export__to_c(pred_table, list(pragma_exported_proc), module_info,
		list(string)).
:- mode export__to_c(in, in, in, out) is det.

export__to_c(_Preds, [], _Module, []).
export__to_c(Preds, [E|ExportedProcs], Module, ExportedProcsCode) :-
	E = pragma_exported_proc(PredId, ProcId, C_Function, _Ctxt),
	module_info_globals(Module, Globals),
	get_export_info(Preds, PredId, ProcId, Globals, Module, DeclareString,
		C_RetType, MaybeDeclareRetval, MaybeFail, MaybeSucceed,
		ArgInfoTypes),
	get_argument_declarations(ArgInfoTypes, yes, Module, ArgDecls),

		% work out which arguments are input, and which are output,
		% and copy to/from the mercury registers.
	get_input_args(ArgInfoTypes, 0, InputArgs),
	copy_output_args(ArgInfoTypes, 0, OutputArgs),
	
	code_util__make_proc_label(Module, PredId, ProcId, ProcLabel),
	llds_out__get_proc_label(ProcLabel, yes, ProcLabelString),

	string__append_list([
		"\n",
				DeclareString, "(", ProcLabelString, ");\n",
				"\n",
				C_RetType, "\n", 
				C_Function, "(", ArgDecls, ")\n{\n",
				"#if MR_NUM_REAL_REGS > 0\n",
				"\tMR_Word c_regs[MR_NUM_REAL_REGS];\n",
				"#endif\n",
				"#if MR_THREAD_SAFE\n",
				"\tMR_bool must_finalize_engine;\n", 
				"#endif\n",
		"#if MR_DEEP_PROFILING\n",
		"\tMR_CallSiteDynList **saved_cur_callback;\n",
		"\tMR_CallSiteDynamic *saved_cur_csd;\n",
		"#endif\n",
				MaybeDeclareRetval,
				"\n",
				"\tMR_save_regs_to_mem(c_regs);\n", 
				"#if MR_THREAD_SAFE\n",
				"\tmust_finalize_engine = MR_init_thread(MR_use_now);\n", 
				"#endif\n",
		"#if MR_DEEP_PROFILING\n",
		"\tsaved_cur_callback = MR_current_callback_site;\n",
		"\tsaved_cur_csd = MR_current_call_site_dynamic;\n",
		"\tMR_setup_callback(MR_ENTRY(", ProcLabelString, "));\n",
		"#endif\n",
				"\tMR_restore_registers();\n", 
				InputArgs,
				"\tMR_save_transient_registers();\n",
				"\t(void) MR_call_engine(MR_ENTRY(",
					ProcLabelString, "), MR_FALSE);\n",
				"\tMR_restore_transient_registers();\n",
		"#if MR_DEEP_PROFILING\n",
		"\tMR_current_call_site_dynamic = saved_cur_csd;\n",
		"\tMR_current_callback_site = saved_cur_callback;\n",
		"#endif\n",
				MaybeFail,
				OutputArgs,
				"#if MR_THREAD_SAFE\n",
				"\tif (must_finalize_engine) {\n", 
				"\t\t MR_finalize_thread_engine();\n", 
				"\t}\n", 
				"#endif\n",
				"\tMR_restore_regs_from_mem(c_regs);\n", 
				MaybeSucceed,
				"}\n\n"],
				Code),

	export__to_c(Preds, ExportedProcs, Module, TheRest),
	ExportedProcsCode = [Code|TheRest].


	% get_export_info(Preds, PredId, ProcId, Globals, DeclareString,
	%		C_RetType, MaybeDeclareRetval, MaybeFail, MaybeSuccess,
	%		ArgInfoTypes):
	%	For a given procedure, figure out the information about
	%	that procedure that is needed to export it:
	%	- how to declare the procedure's entry label,
	%	- the C return type, and the C declaration for the variable
	%	  holding the return value (if any),
	%	- the actions on success and failure, and
	%	- the argument locations/modes/types.

:- pred get_export_info(pred_table, pred_id, proc_id, globals, module_info,
			string, string, string, string, string,
			assoc_list(arg_info, type)).
:- mode get_export_info(in, in, in, in, in,
		out, out, out, out, out, out) is det.

get_export_info(Preds, PredId, ProcId, Globals, Module,
		HowToDeclareLabel, C_RetType, MaybeDeclareRetval,
		MaybeFail, MaybeSucceed, ArgInfoTypes) :-
	map__lookup(Preds, PredId, PredInfo),
	pred_info_import_status(PredInfo, Status),
	(
		( procedure_is_exported(PredInfo, ProcId)
		; status_defined_in_this_module(Status, no)
		  % for --split-c-files, we need to treat
		  % all procedures as if they were exported
		; globals__lookup_bool_option(Globals, split_c_files, yes)
		)
	->
		HowToDeclareLabel = "MR_declare_entry"
	;
		HowToDeclareLabel = "MR_declare_static"
	),
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId, ProcInfo),
	proc_info_arg_info(ProcInfo, ArgInfos),
	pred_info_arg_types(PredInfo, ArgTypes),
	proc_info_interface_code_model(ProcInfo, CodeModel),
	assoc_list__from_corresponding_lists(ArgInfos, ArgTypes,
		ArgInfoTypes0),

	% figure out what the C return type should be,
	% and build the `return' instructions (if any)
	( CodeModel = model_det,
		(
			PredOrFunc = function,
			pred_args_to_func_args(ArgInfoTypes0, ArgInfoTypes1,
				arg_info(RetArgLoc, RetArgMode) - RetType),
			RetArgMode = top_out,
			\+ type_util__is_dummy_argument_type(RetType)
		->
			C_RetType = to_type_string(c, Module, RetType),
			argloc_to_string(RetArgLoc, RetArgString0),
			convert_type_from_mercury(RetArgString0, RetType,
				RetArgString),
			string__append_list(["\t", C_RetType,
					" return_value;\n"],
						MaybeDeclareRetval),
			string__append_list(["\treturn_value = ", RetArgString,
						";\n"], MaybeFail),
			string__append_list(["\treturn return_value;\n"],
				MaybeSucceed),
			ArgInfoTypes2 = ArgInfoTypes1
		;
			C_RetType = "void",
			MaybeDeclareRetval = "",
			MaybeFail = "",
			MaybeSucceed = "",
			ArgInfoTypes2 = ArgInfoTypes0
		)
	; CodeModel = model_semi,
		% we treat semidet functions the same as semidet predicates,
		% which means that for Mercury functions the Mercury return
		% value becomes the last argument, and the C return value
		% is a bool that is used to indicate success or failure.
		C_RetType = "MR_bool",
		MaybeDeclareRetval = "",
		string__append_list([
			"\tif (!MR_r1) {\n",
			"\t\tMR_restore_regs_from_mem(c_regs);\n",
			"\treturn MR_FALSE;\n",
			"\t}\n"
				], MaybeFail),
		MaybeSucceed = "\treturn MR_TRUE;\n",
		ArgInfoTypes2 = ArgInfoTypes0
	; CodeModel = model_non,
		% we should probably check this earlier, e.g. in make_hlds.m,
		% but better we catch this error late than never...
		C_RetType = "\n#error ""cannot export nondet procedure""\n",
		MaybeDeclareRetval = "",
		MaybeFail = "",
		MaybeSucceed = "",
		ArgInfoTypes2 = ArgInfoTypes0
	),
	list__filter(export__include_arg, ArgInfoTypes2, ArgInfoTypes).

	% export__include_arg(ArgInfoType):
	%	Succeeds iff the specified argument should be included in
	%	the arguments of the exported C function.
	%
:- pred export__include_arg(pair(arg_info, type)::in) is semidet.
export__include_arg(arg_info(_Loc, Mode) - Type) :-
	Mode \= top_unused,
	\+ type_util__is_dummy_argument_type(Type).

	% get_argument_declarations(Args, NameThem, DeclString):
	% build a string to declare the argument types (and if
	% NameThem = yes, the argument names) of a C function.

:- pred get_argument_declarations(assoc_list(arg_info, type), bool,
		module_info, string).
:- mode get_argument_declarations(in, in, in, out) is det.

get_argument_declarations([], _, _, "void").
get_argument_declarations([X|Xs], NameThem, Module, Result) :-
	get_argument_declarations_2([X|Xs], 0, NameThem, Module, Result).

:- pred get_argument_declarations_2(assoc_list(arg_info, type), int, bool,
				module_info, string).
:- mode get_argument_declarations_2(in, in, in, in, out) is det.

get_argument_declarations_2([], _, _, _, "").
get_argument_declarations_2([AT|ATs], Num0, NameThem, Module, Result) :-
	AT = ArgInfo - Type,
	Num is Num0 + 1,
	get_argument_declaration(ArgInfo, Type, Num, NameThem, Module,
			TypeString, ArgName),
	(
		ATs = []
	->
		string__append(TypeString, ArgName, Result)
	;
		get_argument_declarations_2(ATs, Num, NameThem, Module,
			TheRest),
		string__append_list([TypeString, ArgName, ", ", TheRest],
			Result)
	).
	
:- pred get_argument_declaration(arg_info, type, int, bool, module_info, 
		string, string).
:- mode get_argument_declaration(in, in, in, in, in, out, out) is det.

get_argument_declaration(ArgInfo, Type, Num, NameThem, Module,
		TypeString, ArgName) :-
	ArgInfo = arg_info(_Loc, Mode),
	( NameThem = yes ->
		string__int_to_string(Num, NumString),
		string__append(" Mercury__argument", NumString, ArgName)
	;
		ArgName = ""
	),
	TypeString0 = to_type_string(c, Module, Type),
	(
		Mode = top_out
	->
			% output variables are passed as pointers
		string__append(TypeString0, " *", TypeString)
	;
		TypeString = TypeString0
	).

:- pred get_input_args(assoc_list(arg_info, type), int, string).
:- mode get_input_args(in, in, out) is det.

get_input_args([], _, "").
get_input_args([AT|ATs], Num0, Result) :-
	AT = ArgInfo - Type,
	ArgInfo = arg_info(ArgLoc, Mode),
	Num is Num0 + 1,
	(
		Mode = top_in,

		string__int_to_string(Num, NumString),
		string__append("Mercury__argument", NumString, ArgName0),
		convert_type_to_mercury(ArgName0, Type, ArgName),
		argloc_to_string(ArgLoc, ArgLocString),
		string__append_list(
			["\t", ArgLocString, " = ", ArgName, ";\n" ],
			InputArg)
	;
		Mode = top_out,
		InputArg = ""
	;
		Mode = top_unused,
		InputArg = ""
	),
	get_input_args(ATs, Num, TheRest),
	string__append(InputArg, TheRest, Result).

:- pred copy_output_args(assoc_list(arg_info, type), int, string).
:- mode copy_output_args(in, in, out) is det.

copy_output_args([], _, "").
copy_output_args([AT|ATs], Num0, Result) :-
	AT = ArgInfo - Type,
	ArgInfo = arg_info(ArgLoc, Mode),
	Num is Num0 + 1,
	(
		Mode = top_in,
		OutputArg = ""
	;
		Mode = top_out,

		string__int_to_string(Num, NumString),
		string__append("Mercury__argument", NumString, ArgName),
		argloc_to_string(ArgLoc, ArgLocString0),
		convert_type_from_mercury(ArgLocString0, Type, ArgLocString),
		string__append_list(
			["\t*", ArgName, " = ", ArgLocString, ";\n" ],
			OutputArg)
	;
		Mode = top_unused,
		OutputArg = ""
	),
	copy_output_args(ATs, Num, TheRest),
	string__append(OutputArg, TheRest, Result).
	
	% convert an argument location (currently just a register number)
	% to a string representing a C code fragment that names it.
:- pred argloc_to_string(arg_loc, string).
:- mode argloc_to_string(in, out) is det.

argloc_to_string(RegNum, RegName) :-
	string__int_to_string(RegNum, RegNumString),
	( 
			% XXX We should handle float registers
			% XXX This magic number can't be good
		RegNum > 32 
	->
		string__append_list(["MR_r(", RegNumString, ")"], RegName)
	;
		string__append("MR_r", RegNumString, RegName)
	).

convert_type_to_mercury(Rval, Type, ConvertedRval) :-	
	(
        	Type = term__functor(term__atom("string"), [], _)
	->
		string__append("(MR_Word) ", Rval, ConvertedRval)
	;
        	Type = term__functor(term__atom("float"), [], _)
	->
		string__append_list(["MR_float_to_word(", Rval, ")" ],
			ConvertedRval)
	;
        	Type = term__functor(term__atom("character"), [], _)
	->
		% We need to explicitly cast to UnsignedChar
		% to avoid problems with C compilers for which `char'
		% is signed.
		string__append("(UnsignedChar) ", Rval, ConvertedRval)
	;
		ConvertedRval = Rval
	).

convert_type_from_mercury(Rval, Type, ConvertedRval) :-	
	(
        	Type = term__functor(term__atom("string"), [], _)
	->
		string__append("(MR_String) ", Rval, ConvertedRval)
	;
        	Type = term__functor(term__atom("float"), [], _)
	->
		string__append_list(["MR_word_to_float(", Rval, ")" ],
			ConvertedRval)
	;
		ConvertedRval = Rval
	).

%-----------------------------------------------------------------------------%

% Should this predicate go in llds_out.m?

export__produce_header_file([], _) --> [].
export__produce_header_file(C_ExportDecls, ModuleName) -->
	{ C_ExportDecls = [_|_] },
	module_name_to_file_name(ModuleName, ".h", yes, FileName),
	io__open_output(FileName, Result),
	(
		{ Result = ok(FileStream) }
	->
		io__set_output_stream(FileStream, OutputStream),
		module_name_to_file_name(ModuleName, ".m", no, SourceFileName),
		{ library__version(Version) },
		io__write_strings(["/*\n** Automatically generated from `", 
			SourceFileName,
			"' by the\n** Mercury compiler, version ", Version,
			".\n** Do not edit.\n*/\n"]),
		{ llds_out__sym_name_mangle(ModuleName, MangledModuleName) },
		{ string__to_upper(MangledModuleName, UppercaseModuleName) },
		{ string__append(UppercaseModuleName, "_H", GuardMacroName) },
		io__write_strings([
			"#ifndef ", GuardMacroName, "\n",
			"#define ", GuardMacroName, "\n",
			"\n",
			"#ifdef __cplusplus\n",
			"extern ""C"" {\n",
			"#endif\n",
			"\n",
			"#ifndef MERCURY_HDR_EXCLUDE_IMP_H\n",
			"#include ""mercury_imp.h""\n",
			"#endif\n",
			"#ifdef MR_DEEP_PROFILING\n",
			"#include ""mercury_deep_profiling.h""\n",
			"#endif\n",
			"\n"]),
		export__produce_header_file_2(C_ExportDecls),
		io__write_strings([
			"\n",
			"#ifdef __cplusplus\n",
			"}\n",
			"#endif\n",
			"\n",
			"#endif /* ", GuardMacroName, " */\n"]),
		io__set_output_stream(OutputStream, _),
		io__close_output(OutputStream)
	;
		io__progname_base("export.m", ProgName),
		io__write_string("\n"),
		io__write_string(ProgName),
		io__write_string(": can't open `"),
		io__write_string(FileName),
		io__write_string("' for output\n"),
		io__set_exit_status(1)
	).

:- pred export__produce_header_file_2(foreign_export_decls, 
		io__state, io__state).
:- mode export__produce_header_file_2(in, di, uo) is det.
export__produce_header_file_2([]) --> [].
export__produce_header_file_2([E|ExportedProcs]) -->
	{ E = foreign_export_decl(Lang, C_RetType, C_Function, ArgDecls) },
	( 
		{ Lang = c }
	->
			% output the function header
		io__write_string(C_RetType),
		io__write_string(" "),
		io__write_string(C_Function),
		io__write_string("("),
		io__write_string(ArgDecls),
		io__write_string(");\n")
	;
		{ error("export__produce_header_file_2: foreign languages other than C unimplemented") }
	),
	export__produce_header_file_2(ExportedProcs).

%-----------------------------------------------------------------------------%
