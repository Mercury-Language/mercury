%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module defines predicates to produce the functions which are
% exported to C via a pragma(export, ...) declaration.

% Main authors: dgj.

%-----------------------------------------------------------------------------%

:- module export.

:- interface.

:- import_module hlds_module, prog_data.
:- import_module io, list, term.

	% From the module_info, get a list of functions, each of which allows
	% a call to be made to a Mercury procedure from C
:- pred export__get_pragma_exported_procs(module_info, list(string)).
:- mode export__get_pragma_exported_procs(in, out) is det.

	% Produce a header file containing prototypes for the exported C
	% functions
:- pred export__produce_header_file(module_info, string, io__state, io__state).
:- mode export__produce_header_file(in, in, di, uo) is det.

	% Convert the term, which represents a type, to a string corresponding
	% to its C type. (Defaults to Word).
:- pred export__term_to_type_string(term, string).
:- mode export__term_to_type_string(in, out) is det.

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

:- implementation.

:- import_module code_gen, code_util, hlds_pred, llds, llds_out.
:- import_module library, map, int, string, std_util, assoc_list, require.
:- import_module bool.

export__get_pragma_exported_procs(Module, ExportedProcsCode) :-
	module_info_get_pragma_exported_procs(Module, ExportedProcs),
	module_info_get_predicate_table(Module, PredicateTable),
	predicate_table_get_preds(PredicateTable, Preds),
	export__to_c(Preds, ExportedProcs, Module, ExportedProcsCode).

	% For each exported procedure, produce a C function.
	% The code we generate is in the form
	%
	% #if SEMIDET
	%   bool
	% #elif FUNCTION
	%   Word
	% #else
	%   void
	% #endif
	% <function name>(Word Mercury__Argument1, Word *Mercury__Argument2...)
	%			/* Word for input, Word* for output */
	% {
	% #if NUM_REAL_REGS > 0
	%	Word c_regs[NUM_REAL_REGS];
	% #endif
	% #if FUNCTION
	%	Word retval;
	% #endif
	%
	%		/* save the registers that our C caller may be using */
	%	save_regs_to_mem(c_regs);
	%
	%		/* restore Mercury's registers that were saved as */
	%		/* we entered C from Mercury (the process must    */
	%		/* always start in Mercury so that we can 	  */
	%		/* init_engine() etc.)				  */
	%	restore_registers();
	%	<copy input arguments from Mercury__Arguments into registers>
	%		/* save the registers which may be clobbered      */
	%		/* by the C function call call_engine().          */
	%	save_transient_registers();
	%	{
	%	Declare_entry(<label of called proc>);
	%	call_engine(ENTRY(<label of called proc>);
	%	}
	%		/* restore the registers which may have been      */
	%		/* clobbered by the return from the C function    */
	%		/* call_engine()				  */
	%	restore_transient_registers();
	% #if SEMIDET
	%	if (!r1) {
	%		restore_regs_from_mem(c_regs);
	%		return FALSE;
	%	}
	% #elif FUNCTION
	%	<copy return value register into retval>
	% #endif
	%	<copy output args from registers into *Mercury__Arguments>
	%	restore_regs_from_mem(c_regs);
	% #if SEMIDET
	%	return TRUE;
	% #elif FUNCTION
	%	return retval;
	% #endif
	% }
:- pred export__to_c(pred_table, list(pragma_exported_proc), module_info,
		list(string)).
:- mode export__to_c(in, in, in, out) is det.

export__to_c(_Preds, [], _Module, []).
export__to_c(Preds, [E|ExportedProcs], Module, ExportedProcsCode) :-
	E = pragma_exported_proc(PredId, ProcId, C_Function),
	get_export_info(Preds, PredId, ProcId,
		C_RetType, MaybeDeclareRetval, MaybeFail, MaybeSucceed,
		ArgInfoTypes),
	get_argument_declarations(ArgInfoTypes, yes, ArgDecls),

		% work out which arguments are input, and which are output,
		% and copy to/from the mercury registers.
	get_input_args(ArgInfoTypes, 0, InputArgs),
	copy_output_args(ArgInfoTypes, 0, OutputArgs),
	
	code_util__make_proc_label(Module, PredId, ProcId, ProcLabel),
	get_proc_label(ProcLabel, ProcLabelString),

	string__append_list([	"\n",
				C_RetType, "\n", 
				C_Function, "(", ArgDecls, ")\n{\n",
				"#if NUM_REAL_REGS > 0\n",
				"\tWord c_regs[NUM_REAL_REGS];\n",
				"#endif\n",
				MaybeDeclareRetval,
				"\n",
				"\tsave_regs_to_mem(c_regs);\n", 
				"\trestore_registers();\n", 
				InputArgs,
				"\tsave_transient_registers();\n",
				"\t{\n\tDeclare_entry(",
				ProcLabelString,
				");\n",
				"\tcall_engine(ENTRY(",
				ProcLabelString,
				"));\n\t}\n",
				"\trestore_transient_registers();\n",
				MaybeFail,
				OutputArgs,
				"\trestore_regs_from_mem(c_regs);\n", 
				MaybeSucceed,
				"}\n\n"],
				Code),

	export__to_c(Preds, ExportedProcs, Module, TheRest),
	ExportedProcsCode = [Code|TheRest].


	% get_export_info(Preds, PredId, ProcId,
	%		C_RetType, MaybeDeclareRetval, MaybeFail, MaybeSuccess,
	%		ArgInfoTypes):
	%	Figure out the C return type, the actions on success
	%	and failure, and the argument locations/modes/types
	%	for a given procedure.
:- pred get_export_info(pred_table, pred_id, proc_id,
			string, string, string, string,
			assoc_list(arg_info, type)).
:- mode get_export_info(in, in, in, out, out, out, out, out) is det.

get_export_info(Preds, PredId, ProcId, C_RetType,
		MaybeDeclareRetval, MaybeFail, MaybeSucceed, ArgInfoTypes) :-
	map__lookup(Preds, PredId, PredInfo),
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId, ProcInfo),
	proc_info_arg_info(ProcInfo, ArgInfos),
	pred_info_arg_types(PredInfo, _TVarSet, ArgTypes),
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
			RetArgMode = top_out
		->
			export__term_to_type_string(RetType, C_RetType),
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
			ArgInfoTypes = ArgInfoTypes1
		;
			C_RetType = "void",
			MaybeDeclareRetval = "",
			MaybeFail = "",
			MaybeSucceed = "",
			ArgInfoTypes = ArgInfoTypes0
		)
	; CodeModel = model_semi,
		% we treat semidet functions the same as semidet predicates,
		% which means that for Mercury functions the Mercury return
		% value becomes the last argument, and the C return value
		% is a bool that is used to indicate success or failure.
		C_RetType = "bool",
		MaybeDeclareRetval = "",
		string__append_list([
			"\tif (!r1) {\n",
			"\t\trestore_regs_from_mem(c_regs);\n",
			"\treturn FALSE;\n",
			"\t}\n"
				], MaybeFail),
		MaybeSucceed = "\treturn TRUE;\n",
		ArgInfoTypes = ArgInfoTypes0
	; CodeModel = model_non,
		% we should probably check this earlier, e.g. in make_hlds.m,
		% but better we catch this error late than never...
		C_RetType = "\n#error ""cannot export nondet procedure""\n",
		MaybeDeclareRetval = "",
		MaybeFail = "",
		MaybeSucceed = "",
		ArgInfoTypes = ArgInfoTypes0
	).

	% get_argument_declarations(Args, NameThem, DeclString):
	% build a string to declare the argument types (and if
	% NameThem = yes, the argument names) of a C function.

:- pred get_argument_declarations(assoc_list(arg_info, type), bool, string).
:- mode get_argument_declarations(in, in, out) is det.

get_argument_declarations([], _, "void").
get_argument_declarations([X|Xs], NameThem, Result) :-
	get_argument_declarations_2([X|Xs], 0, NameThem, Result).

:- pred get_argument_declarations_2(assoc_list(arg_info, type), int, bool,
				string).
:- mode get_argument_declarations_2(in, in, in, out) is det.

get_argument_declarations_2([], _, _, "").
get_argument_declarations_2([AT|ATs], Num0, NameThem, Result) :-
	AT = ArgInfo - Type,
	ArgInfo = arg_info(_Loc, Mode),
	Num is Num0 + 1,
	( NameThem = yes ->
		string__int_to_string(Num, NumString),
		string__append(" Mercury__argument", NumString, ArgName)
	;
		ArgName = ""
	),
	export__term_to_type_string(Type, TypeString0),
	(
		Mode = top_out
	->
			% output variables are passed as pointers
		string__append(TypeString0, " *", TypeString)
	;
		TypeString = TypeString0
	),
	(
		ATs = []
	->
		string__append(TypeString, ArgName, Result)
	;
		get_argument_declarations_2(ATs, Num, NameThem, TheRest),
		string__append_list([TypeString, ArgName, ", ", TheRest], 
			Result)
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
		string__append_list(["r(", RegNumString, ")"], RegName)
	;
		string__append("r", RegNumString, RegName)
	).

convert_type_to_mercury(Rval, Type, ConvertedRval) :-	
	(
        	Type = term__functor(term__atom("string"), [], _)
	->
		string__append("(Word) ", Rval, ConvertedRval)
	;
        	Type = term__functor(term__atom("float"), [], _)
	->
		string__append_list(["float_to_word(", Rval, ")" ],
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
		string__append("(String) ", Rval, ConvertedRval)
	;
        	Type = term__functor(term__atom("float"), [], _)
	->
		string__append_list(["word_to_float(", Rval, ")" ],
			ConvertedRval)
	;
		ConvertedRval = Rval
	).


export__produce_header_file(Module, ModuleName) -->
	{ module_info_get_pragma_exported_procs(Module, ExportedProcs) },
	(
		{ ExportedProcs = [_|_] }
	->
		{ module_info_get_predicate_table(Module, PredicateTable) },
		{ predicate_table_get_preds(PredicateTable, Preds) },
		{ string__append(ModuleName, ".h", FileName) },
		io__tell(FileName, Result),
		(
			{ Result = ok }
		->
			{ library__version(Version) },
			io__write_strings(
				["/*\n** Automatically generated from `", 
				ModuleName,
				".m' by the\n** Mercury compiler, version ", 
				Version,
				".  Do not edit.\n*/\n"]),
			{ string__to_upper(ModuleName, UpperModuleName) },
			{ string__append(UpperModuleName, "_H", UpperFileName) },
			io__write_strings([
					"#ifndef ", UpperFileName, "\n",
					"#define ", UpperFileName, "\n",
					"\n",
					"#ifdef __cplusplus\n",
					"extern ""C"" {\n",
					"#endif\n",
					"\n",
					"#include ""imp.h""\n",
					"\n"]),
			export__produce_header_file_2(Preds, ExportedProcs),
			io__write_strings([
					"\n",
					"#ifdef __cplusplus\n",
					"}\n",
					"#endif\n",
					"\n",
					"#endif /* ", UpperFileName, " */\n"]),
			io__told
		;
			io__progname_base("export.m", ProgName),
			io__write_string("\n"),
			io__write_string(ProgName),
			io__write_string(": can't open `"),
			io__write_string(FileName),
			io__write_string("' for output\n"),
			io__set_exit_status(1)
		)
	;
		[]
	).

:- pred export__produce_header_file_2(pred_table, list(pragma_exported_proc),
	io__state, io__state).
:- mode export__produce_header_file_2(in, in, di, uo) is det.
export__produce_header_file_2(_Preds, []) --> [].
export__produce_header_file_2(Preds, [E|ExportedProcs]) -->
	{ E = pragma_exported_proc(PredId, ProcId, C_Function) },
	{ get_export_info(Preds, PredId, ProcId, C_RetType,
		_DeclareReturnVal, _FailureAction, _SuccessAction,
		HeadArgInfoTypes) },
	{ get_argument_declarations(HeadArgInfoTypes, no, ArgDecls) },

		% output the function header
	io__write_string(C_RetType),
	io__write_string(" "),
	io__write_string(C_Function),
	io__write_string("("),
	io__write_string(ArgDecls),
	io__write_string(");\n"),

	export__produce_header_file_2(Preds, ExportedProcs).

	% Convert a term representation of a variable type to a string which
	% represents the C type of the variable
	% Apart from special cases, local variables become Words
export__term_to_type_string(Type, Result) :-
	( Type = term__functor(term__atom("int"), [], _) ->
		Result = "Integer"
	; Type = term__functor(term__atom("float"), [], _) ->
		Result = "Float"
	; Type = term__functor(term__atom("string"), [], _) ->
		Result = "String"
	; Type = term__functor(term__atom("character"), [], _) ->
		Result = "Char"
	;
		Result = "Word"
	).

