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

:- import_module hlds_module.
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

:- implementation.

:- import_module code_gen, code_util, hlds_pred, llds, llds_out.
:- import_module library, map, int, string, std_util, assoc_list, require.

export__get_pragma_exported_procs(Module, ExportedProcsCode) :-
	module_info_get_pragma_exported_procs(Module, ExportedProcs),
	module_info_get_predicate_table(Module, PredicateTable),
	predicate_table_get_preds(PredicateTable, Preds),
	export__to_c(Preds, ExportedProcs, Module, ExportedProcsCode).

	% For each exported procedure, produce a C function.
	% The code we generate is in the form
	%
	% void
	% <function name>(Word Mercury__Argument1, Word *Mercury__Argument2...)
	%			/* Word for input, Word* for output */
	% {
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
	%		/* restore the registers which we saved before    */
	%		/* the C function call                            */
	%	restore_transient_registers();
	%	<copy output args from registers into *Mercury__Arguments>
	% #ifndef CONSERVATIVE_GC
	%               /* save the registers before returning to C.      */
	%		/* However, the only register of importance that  */
	%		/* may have changed during the execution of the   */
	%		/* Mercury code is the heap pointer. If we are    */
	%		/* using the convervative garbage collector, the  */
	%		/* heap pointer is not important, so we don't     */
	%		/* bother at all.                                 */
	%	save_registers();
	% #endif
	% }
:- pred export__to_c(pred_table, list(pragma_exported_proc), module_info,
		list(string)).
:- mode export__to_c(in, in, in, out) is det.

export__to_c(_Preds, [], _Module, []).
export__to_c(Preds, [E|ExportedProcs], Module, ExportedProcsCode) :-
	E = pragma_exported_proc(PredId, ProcId, C_Function),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId, ProcInfo),
	proc_info_arg_info(ProcInfo, ArgInfos),
	proc_info_headvars(ProcInfo, HeadVars), 
	proc_info_vartypes(ProcInfo, VarTypes),
	make_arg_info_type_list(HeadVars, ArgInfos, VarTypes, 
		ArgInfoTypes),

	get_argument_declarations(ArgInfoTypes, ArgDecls),

		% work out which arguments are input, and which are output,
		% and copy to/from the mercury registers.
	get_input_args(ArgInfoTypes, 0, InputArgs),
	copy_output_args(ArgInfoTypes, 0, OutputArgs),
	
	code_util__make_proc_label(Module, PredId, ProcId, ProcLabel),
	get_proc_label(ProcLabel, ProcLabelString),

	string__append_list([	"\nvoid\n", 
				C_Function, 
				"(", 
				ArgDecls, 
				")\n{\n",
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
				OutputArgs,
				"#ifndef CONSERVATIVE_GC\n",
				"\tsave_registers();\n",
				"#endif\n",
				"}\n\n"],
				Code),

	export__to_c(Preds, ExportedProcs, Module, TheRest),
	ExportedProcsCode = [Code|TheRest].


	% Match the arg_info with the type of the corresponding variable
:- pred make_arg_info_type_list(list(var), list(arg_info), map(var, type), 
		assoc_list(arg_info, type)).
:- mode make_arg_info_type_list(in, in, in, out) is det.
make_arg_info_type_list([], [], _VarTypes, []).
make_arg_info_type_list([_|_], [], _VarTypes, []) :-
	error("list length mismatch in make_var_arg_info_type_list/4").
make_arg_info_type_list([], [_|_], _VarTypes, []) :-
	error("list length mismatch in make_var_arg_info_type_list/4").
make_arg_info_type_list([V|Vs], [A|As], VarTypes, [AT|ATs]) :-
	map__lookup(VarTypes, V, Type),
	AT =  A - Type,
	make_arg_info_type_list(Vs, As, VarTypes, ATs).


:- pred get_argument_declarations(assoc_list(arg_info, type), string).
:- mode get_argument_declarations(in, out) is det.

get_argument_declarations([], "void").
get_argument_declarations([X|Xs], Result) :-
	get_argument_declarations_2([X|Xs], 0, Result).

:- pred get_argument_declarations_2(assoc_list(arg_info, type), int, string).
:- mode get_argument_declarations_2(in, in, out) is det.

get_argument_declarations_2([], _, "").
get_argument_declarations_2([AT|ATs], Num0, Result) :-
	AT = ArgInfo - Type,
	ArgInfo = arg_info(_Loc, Mode),
	Num is Num0 + 1,
	string__int_to_string(Num, NumString),
	string__append("Mercury__argument", NumString, ArgName),
	export__term_to_type_string(Type, TypeString0),
	(
		Mode = top_out
	->
			% output variables are passed as pointers
		string__append(TypeString0, " *", TypeString)
	;
		string__append(TypeString0, " ", TypeString)
	),
	(
		ATs = []
	->
		string__append(TypeString, ArgName, Result)
	;
		get_argument_declarations_2(ATs, Num, TheRest),
		string__append_list([TypeString, ArgName, ", ", TheRest], 
			Result)
	).

:- pred get_input_args(assoc_list(arg_info, type), int, string).
:- mode get_input_args(in, in, out) is det.

get_input_args([], _, "").
get_input_args([AT|ATs], Num0, Result) :-
	AT = ArgInfo - Type,
	ArgInfo = arg_info(Register, Mode),
	Num is Num0 + 1,
	(
		Mode = top_in,

		string__int_to_string(Register, RegString),
		string__int_to_string(Num, NumString),
		string__append("Mercury__argument", NumString, ArgName0),
		convert_type_to_mercury(ArgName0, Type, ArgName),

		( 
				% XXX We should handle floats
				% XXX This magic number can't be good
			Register > 32 
		->
			string__append_list(["r(", RegString, ")"], RegName)
		;
			string__append("r", RegString, RegName)
		),
		string__append_list(["\t", RegName, " = ", ArgName, ";\n" ],
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
	ArgInfo = arg_info(Register, Mode),
	Num is Num0 + 1,
	(
		Mode = top_in,
		OutputArg = ""
	;
		Mode = top_out,

		string__int_to_string(Num, NumString),
		string__append("Mercury__argument", NumString, ArgName),
		string__int_to_string(Register, RegString),

		( 
				% XXX We should handle float registers
				% XXX This magic number can't be good
			Register > 32 
		->
			string__append_list(["r(", RegString, ")"], RegName0)
		;
			string__append("r", RegString, RegName0)
		),
		convert_type_from_mercury(RegName0, Type, RegName),
		string__append_list(["\t*", ArgName, " = ", RegName, ";\n" ],
			OutputArg)
	;
		Mode = top_unused,
		OutputArg = ""
	),
	copy_output_args(ATs, Num, TheRest),
	string__append(OutputArg, TheRest, Result).
	

	% Convert an rval (represented as a string), from a C type to
	% a mercury C type. (ie. convert strings and floats to words).
:- pred convert_type_to_mercury(string, type, string).
:- mode convert_type_to_mercury(in, in, out) is det.

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
		ConvertedRval = Rval
	).

	% Convert an rval (represented as a string), from a mercury C type to
	% a C type. (ie. convert words to strings and floats if required).
:- pred convert_type_from_mercury(string, type, string).
:- mode convert_type_from_mercury(in, in, out) is det.

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
			io__write_string("#include ""imp.h""\n\n"),
			{ string__to_upper(ModuleName, UpperModuleName) },
			{ string__append(UpperModuleName, "_H", UpperFileName) },
			io__write_strings([
						"#ifndef ",
						UpperFileName,
						"\n",
						"#define ",
						UpperFileName,
						"\n"
						]),
			export__produce_header_file_2(Preds, ExportedProcs),
			io__write_string("#endif\n"),
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
	{ map__lookup(Preds, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ProcId, ProcInfo) },
	{ proc_info_arg_info(ProcInfo, ArgInfos) },

	{ proc_info_headvars(ProcInfo, HeadVars) },
	{ proc_info_vartypes(ProcInfo, VarTypes) },

	{ make_arg_info_type_list(HeadVars, ArgInfos, VarTypes, 
		HeadArgInfoTypes) },

	{ get_argument_declarations(HeadArgInfoTypes, ArgDecls) },

		% output the function header
	io__write_string("void\n"),
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

