%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: fact_table.m.
% Main author: dmo.

% This module handles compilation of fact tables contained in external
% files that have been declared with a `pragma fact_table' declaration.
%
% The facts are processed one by one. Each fact is read in and type and mode 
% checked. If there are no modes with input arguments, the data is written
% out to arrays of C structures as each fact is processed.  If there are input
% modes, the input arguments for each mode are written out to a temporary
% sort file -- one sort file per input mode.  The output arguments are also
% included in the sort file for the primary input mode.  (At the moment,
% the primary input mode is the one with the lowest ProcID number, however
% this may change in the future to select the mode that is likely to give
% the biggest increase in efficiency by being the primary mode).
%
% After all the facts have been read, the sort files are sorted by the Unix
% `sort' program.  They are then scanned for duplicate input keys to infer
% the determinisms of each mode.
%
% The sort files are then read back in one by one and hash tables are created
% for each input mode.  While the sort file for the primary input mode is
% being read, the output arguments are also read back in and output as C
% arrays in another temporary file.  (This file is concatenated to the end
% of the fact table C file after all the hash tables have been created.)
% This means that the output data for identical keys in the primary input
% mode will be grouped together allowing the code that accesses this mode
% to be just pick the next item in the data array when backtracking.

% The inferred determinism for each mode is added to the proc_info.  If a
% determinism has been declared for the procedure it will be tested against
% the inferred determinism later on in det_report.m.  

% XXX All combinations of `in' and `out' arguments are now supported for all
% determinisms.  Only the builtin `string', `int' and `float' types are
% supported at the moment.

% XXX Cross compilation is not supported for fact tables that are indexed on
% floats.


:- module fact_table.

:- interface.

:- import_module io, list.
:- import_module prog_data, hlds_pred, hlds_module.

	% compile the fact table into a separate .c file.
	% fact_table_compile_facts(PredName, Arity, FileName, PredInfo0, 
	% 	PredInfo, Context, ModuleInfo, C_HeaderCode, PrimaryProcID)
:- pred fact_table_compile_facts(sym_name, arity, string, pred_info, pred_info,
		prog_context, module_info, string, proc_id,
		io__state, io__state).
:- mode fact_table_compile_facts(in, in, in, in, out, in, in, out, out,
		di, uo) is det.

	% generate c code to lookup a fact table in a given mode
	% fact_table_generate_c_code(PredName, PragmaVars, ProcID, 
	%	PrimaryProcID, ProcInfo, ArgTypes, C_ProcCode, C_ExtraCode).
	% C_ProcCode is the C code for the procedure,
	% C_ExtraCode is extra C code that should be included in the module
	%
	% XXX   model_non pragma c was not supported by the compiler
	% when this code was written.  To get around this, the C_ProcCode
	% generated for model_non code pops off the stack frame that is
	% automatically created by the compiler and jumps to the code contained
	% in C_ExtraCode.  C_ExtraCode declares the required labels and creates
	% a new stack frame with the required number of framevars.  It then 
	% does all the work required to lookup the fact table.
	% This should really be rewritten to work using model_non pragma c
	% now that model_non pragma c is implemented.
:- pred fact_table_generate_c_code(sym_name, list(pragma_var), proc_id,
		proc_id, proc_info, list(type), module_info, string, string,
		io__state, io__state).
:- mode fact_table_generate_c_code(in, in, in, in, in, in, in, out, out,
		di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

% Standard library modules
:- import_module int, map, std_util, assoc_list, char, require, library, bool.
:- import_module float, math, getopt, string.
:- import_module parser, term, term_io.

% Parse tree modules
:- import_module prog_util, prog_io, prog_out, modules.
% HLDS modules
:- import_module hlds_out, hlds_data, mode_util, inst_match.
% LLDS back-end modules
:- import_module arg_info, llds, llds_out, code_util, export, foreign.
% Modules shared between different back-ends.
:- import_module passes_aux, code_model.
% Misc
:- import_module globals, options.

:- type fact_result
	--->	ok ; error.

	% proc_stream contains information about an open sort file for
	% a particular procedure.
:- type proc_stream
	--->	proc_stream(
			proc_id, 		% ID of procedure
			io__output_stream 	% Sort file stream
		).

:- type hash_entry
	--->	hash_entry(
			fact_arg,	% lookup key
			hash_index, 	% pointer to next hash table or index
					% to fact data
			int		% position of next entry with same
					% hash value
		).

	% Data structure used to build up a hash table before writing it out
	% as a C array.
:- type hash_table
	--->	hash_table(
			int,			% size of hash table
			map(int, hash_entry)
		).

:- type hash_index
	--->	fact(int)		% index into fact table
	;	hash_table(int, string).% hash table for next arg

:- type fact_arg == const.

	% sort_file_line contains the information read in from a sort file
	% after sorting.
:- type sort_file_line
	--->	sort_file_line(
			list(fact_arg),	% input arguments
			int,		% index of fact in original file
			list(fact_arg)	% output arguments
		).

:- type fact_table_mode_type
	--->	all_in		% modes of all arguments are input
	;	all_out		% modes of all arguments are output
	;	in_out		% modes are a mixture of input and output
	;	other		% some arguments have modes that are 
				% not in or out
	;	unknown.

:- type inferred_determinism
	--->	inferred(determinism)	% determinism has been inferred
	;	not_yet		 	% determinism has not yet been inferred
	;	error.		 	% an error occurred trying to infer 
					%determinism

:- type fact_arg_info
	--->	fact_arg_info(
			type,	% type of the argument
			bool,	% is an input argument for some mode
			bool	% is an output argument for some mode
		).

	% Maximum size of each array in the fact data table.  GCC doesn't cope
	% very well with huge arrays so we break the fact data table into a
	% number of smaller arrays, each with a maximum size given by this 
	% predicate, and create an array of pointers to these arrays to access
	% the data.  The size should be a power of 2 to make the generated
	% code more efficient.
:- pred fact_table_size(int::out, io__state::di, io__state::uo) is det.

fact_table_size(FactTableSize) -->
	globals__io_lookup_int_option(fact_table_max_array_size,
		FactTableSize).

%---------------------------------------------------------------------------%

fact_table_compile_facts(PredName, Arity, FileName, PredInfo0, PredInfo, 
		Context, ModuleInfo, C_HeaderCode, PrimaryProcID) -->
    io__see(FileName, Result0),
    (
	{ Result0 = ok },
	{ module_info_name(ModuleInfo, ModuleName) },
	fact_table_file_name(ModuleName, FileName, ".c", OutputFileName),
	io__open_output(OutputFileName, Result1),
	(
	    { Result1 = ok(OutputStream) },
	    { pred_info_arg_types(PredInfo0, Types) },
	    { init_fact_arg_infos(Types, FactArgInfos0) },
	    infer_determinism_pass_1(PredInfo0, PredInfo1, Context, ModuleInfo,
	    	CheckProcs, ExistsAllInMode, WriteHashTables, WriteDataTable,
	    	FactArgInfos0, FactArgInfos, Result2),
	    write_fact_table_header(PredName, PredInfo1, FileName,
		FactArgInfos, OutputStream, C_HeaderCode0, StructName, Result3),
	    ( { Result2 = ok, Result3 = ok } ->
		open_sort_files(CheckProcs, ProcStreams),
		( { WriteDataTable = yes } ->
		    ( { CheckProcs = [] } ->
			{ MaybeOutput = yes(OutputStream - StructName) },
			    % opening brace for first fact data array
			write_new_data_array(OutputStream, StructName, 0),
			{ WriteDataAfterSorting = no }
		    ; 
			{ MaybeOutput = no },
			{ WriteDataAfterSorting = yes }
		    )
		;
			{ MaybeOutput = no },
			{ WriteDataAfterSorting = no }
		),
		compile_facts(PredName, Arity, PredInfo1, ModuleInfo,
		    FactArgInfos, ProcStreams, MaybeOutput, 0, NumFacts),
		io__seen,
		(
		    { MaybeOutput = yes(_) },
			    % closing brace for last fact data array
		    write_closing_brace(OutputStream),
		    write_fact_table_pointer_array(NumFacts, StructName, 
			OutputStream, C_HeaderCode2)
		;
		    { MaybeOutput = no },
		    { C_HeaderCode2 = "" }

		),
		{ pred_info_procedures(PredInfo1, ProcTable0) },
		infer_determinism_pass_2(ProcStreams, ProcFiles,
		    ExistsAllInMode, ProcTable0, ProcTable),
		{ pred_info_set_procedures(PredInfo1, ProcTable, PredInfo) },
		io__make_temp(DataFileName),
		write_fact_table_arrays(ProcFiles, DataFileName, StructName, 
		    ProcTable, ModuleInfo, NumFacts, FactArgInfos,
		    WriteHashTables, WriteDataAfterSorting, OutputStream,
		    C_HeaderCode1, PrimaryProcID),
		write_fact_table_numfacts(PredName, NumFacts, OutputStream, 
		    C_HeaderCode3),
		{ string__append_list([C_HeaderCode0, C_HeaderCode1, 
		    C_HeaderCode2, C_HeaderCode3], C_HeaderCode) }
	    ;
	    	% Either there are no modes declared for this fact table or
	    	% the `:- pred' or `:- func' declaration had some types that
	    	% are not supported in fact tables so there is no point trying
	    	% to type-check all the facts.
	    	{ PredInfo = PredInfo0 },
	    	{ C_HeaderCode = C_HeaderCode0 },
	    	{ invalid_proc_id(PrimaryProcID) },
	    	{ WriteDataAfterSorting = no },
	    	{ DataFileName = "" }
	    ),
	    io__close_output(OutputStream),
	    maybe_append_data_table(WriteDataAfterSorting, OutputFileName, 
	    	DataFileName)
	;
	    { Result1 = error(ErrorCode) },
	    { io__error_message(ErrorCode, ErrorMessage) },
	    prog_out__write_context(Context),
	    io__write_strings([ 
	    	"Error opening file `", 
	    	OutputFileName, 
	    	"' for output: ",
	    	ErrorMessage,
	    	".\n" ]),
	    io__set_exit_status(1),
	    { PredInfo = PredInfo0 },
	    { C_HeaderCode = "" },
	    { invalid_proc_id(PrimaryProcID) }
	)
    ;
	{ Result0 = error(ErrorCode) },
	{ io__error_message(ErrorCode, ErrorMessage) },

		% Context is the location of the pragma fact_table decl
	prog_out__write_context(Context),
	io__write_strings([ 
	    "Error opening file `", 
	    FileName, 
	    "' for input: ",
	    ErrorMessage,
	    ".\n" ]),
	io__set_exit_status(1),
	{ PredInfo = PredInfo0 },
	{ C_HeaderCode = "" },
	{ invalid_proc_id(PrimaryProcID) }
    ).

%---------------------------------------------------------------------------%

	% read in facts one by one and check and compile them
:- pred compile_facts(sym_name, arity, pred_info, module_info,
		list(fact_arg_info), list(proc_stream),
		maybe(pair(io__output_stream, string)), int, int,
		io__state, io__state).
:- mode compile_facts(in, in, in, in, in, in, in, in, out, di, uo) is det.

compile_facts(PredName, Arity, PredInfo, ModuleInfo, FactArgInfos, ProcStreams,
		MaybeOutput, NumFacts0, NumFacts) -->
	parser__read_term(Result0),
	(
		{ Result0 = eof},
		{ NumFacts = NumFacts0 }
	;
		{ Result0 = error(Message, LineNum) },
		io__input_stream_name(FileName),
		{ term__context_init(FileName, LineNum, Context) },
		prog_out__write_context(Context),
		io__write_strings([Message, "\n"]),
		io__set_exit_status(1),
		{ NumFacts = NumFacts0 }
	;
		{ Result0 = term(_VarSet, Term) },
		fact_table_size(FactTableSize),
		( { 0 is NumFacts0 mod FactTableSize } ->
			globals__io_lookup_bool_option(very_verbose,
				VeryVerbose),
			( { VeryVerbose = yes } ->
				io__format("%% Read fact %d\n", [i(NumFacts0)])
			;
				[]
			)
		;
			[]
		),

		check_fact_term(PredName, Arity, PredInfo, ModuleInfo, Term,
			FactArgInfos, ProcStreams, MaybeOutput, NumFacts0,
			Result1),
		{
			Result1 = ok,
			NumFacts1 is NumFacts0 + 1
		;
			Result1 = error,
			NumFacts1 = NumFacts0
		},
		compile_facts(PredName, Arity, PredInfo, ModuleInfo,
			FactArgInfos, ProcStreams, MaybeOutput, NumFacts1,
			NumFacts)
	).

	% do syntactic and semantic checks on a fact term
:- pred check_fact_term(sym_name, arity, pred_info, module_info, prog_term,
		list(fact_arg_info), list(proc_stream),
		maybe(pair(io__output_stream, string)), int, fact_result,
		io__state, io__state).
:- mode check_fact_term(in, in, in, in, in, in, in, in, in, out, di, uo)
		is det.

check_fact_term(_, _, _, _, term__variable(_V), _, _, _, _, error) -->
	io__get_line_number(LineNum),
	io__input_stream_name(FileName),
	prog_out__write_context(term__context(FileName, LineNum)),
	io__write_string("Error: term is not a fact.\n"),
	io__set_exit_status(1).


check_fact_term(PredName, Arity0, PredInfo, ModuleInfo,
	term__functor(Const, Terms0, Context), FactArgInfos, ProcStreams, 
	MaybeOutput, FactNum, Result) -->
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ unqualify_name(PredName, PredString) },
	(
	    { Const = term__atom(TopLevel) }
	->
	    (
		(
		    { PredOrFunc = predicate },
		    { TopLevel = PredString },
		    { Terms = Terms0 },
		    { Arity = Arity0 }
		;
		    { PredOrFunc = function },
		    { TopLevel = "=" },
		    { Terms0 = [ FuncHeadTerm , FuncResultTerm ] },
		    { FuncHeadTerm = term__functor(
		    	term__atom(PredString), Terms1, _) },
		    { list__append(Terms1, [FuncResultTerm], Terms) },
		    { Arity is Arity0 + 1 }
		)
	    ->
		% Check that arity of the fact is correct
		{ list__length(Terms, Len) },
		(
		    { Len = Arity }
		->
		    { pred_info_arg_types(PredInfo, Types) },
		    check_fact_type_and_mode(Types, Terms, 0, PredOrFunc, 
			Context, Result),
		    { pred_info_procedures(PredInfo, ProcTable) },
		    { string__int_to_string(FactNum, FactNumStr) },
		    write_sort_file_lines(ProcStreams, ProcTable, Terms, 
			ModuleInfo, FactNumStr, FactArgInfos, yes),

		    % If there are no in_out modes to the predicate, we need
		    % to write out the facts at this point.  If there are
		    % input modes, the facts are written out later on after 
		    % being sorted on the first input mode.
		    (
			{ MaybeOutput = yes(OutputStream - StructName) },
			{ TermToArg = lambda([Term::in,FactArg::out] is semidet,
				Term = term__functor(FactArg, _, _)) },
			{ list__map(TermToArg, Terms, FactArgs) }
		    ->
			write_fact_data(FactNum, FactArgs, StructName,
				OutputStream)
		    ;
		    	% If list__map above fails, don't do anything here.
		    	% The error will have already been reported in 
		    	% check_fact_type_and_mode.
		    	[]
		    )
		;
		    prog_out__write_context(Context),
		    io__write_string(
			"Error: fact has wrong number of arguments.\n"),
		    prog_out__write_context(Context),
		    { string__format(
		      "  Expecting %d arguments, but fact has %d arguments.\n",
		    	[i(Arity), i(Len)], ErrString) },
		    io__write_string(ErrString),
		    io__set_exit_status(1),
		    { Result = error}
		)
	    ;
		prog_out__write_context(Context),
		io__write_string("Error: invalid clause for "),
		hlds_out__write_pred_or_func(PredOrFunc),
		io__write_strings([ " `", PredString, "/"]),
		io__write_int(Arity0),
		io__write_string("' .\n"),
		io__set_exit_status(1),
		{ Result = error }
	    )
	;
	    prog_out__write_context(Context),
	    io__write_string("Error: term is not a fact.\n"),
	    io__set_exit_status(1),
	    { Result = error }
	).

% Check that the mode of the fact is correct.  All terms must be ground and be
% a constant of the correct type.  Only string, int and float are supported at
% the moment.
:- pred check_fact_type_and_mode(list(type), list(prog_term), int,
		pred_or_func, prog_context, fact_result, io__state, io__state).
:- mode check_fact_type_and_mode(in, in, in, in, in, out,
	di, uo) is det.

check_fact_type_and_mode(_, [], _, _, _, ok) --> [].
check_fact_type_and_mode(Types0, [Term | Terms], ArgNum0, PredOrFunc,
		Context0, Result) -->
	{ ArgNum is ArgNum0 + 1 },
	(
		{ Term = term__variable(_) },
		prog_out__write_context(Context0),
		io__write_string("Error: non-ground term in fact.\n"),
		io__set_exit_status(1),
		{ Result = error}
	;
		{ Term = term__functor(term__atom(_), Items, Context) },
		(
			{ Items = [_ | _] },
			prog_out__write_context(Context),
			io__write_string("Error: compound types are not "),
			io__write_string("supported in fact tables.\n"),
			io__set_exit_status(1),
			{ Result = error}
		;
			{ Items = [] },
			prog_out__write_context(Context),
			io__write_string("Error: enumeration types are not "),
			io__write_string("yet supported in fact tables.\n"),
			io__set_exit_status(1),
			{ Result = error}
		)
	;
		% We know that string, integer and float constants are ground, 
		% but we still need to check that they are the right type for 
		% this argument.
		{ Term = term__functor(term__string(_), _, Context) },
		(
			{ Types0 = [Type | Types] },
			{ Type = term__functor(term__atom("string"), [], _) }
		->
			check_fact_type_and_mode(Types, Terms, ArgNum, 
				PredOrFunc, Context0, Result)
		;
			report_type_error(Context, ArgNum, Terms, PredOrFunc),
			{ Result = error}
		)
	;
		{ Term = term__functor(term__integer(_), _, Context) },
		(
			{ Types0 = [Type | Types] },
			{ Type = term__functor(term__atom("int"), [], _) }
		->
			check_fact_type_and_mode(Types, Terms, ArgNum, 
				PredOrFunc, Context0, Result)
		;
			report_type_error(Context, ArgNum, Terms, PredOrFunc),
			{ Result = error}
		)
	;
		{ Term = term__functor(term__float(_), _, Context) },
		(
			{ Types0 = [Type | Types] },
			{ Type = term__functor(term__atom("float"), [], _) }
		->
			check_fact_type_and_mode(Types, Terms, ArgNum, 
				PredOrFunc, Context0, Result)
		;
			report_type_error(Context, ArgNum, Terms, PredOrFunc),
			{ Result = error}
		)
	).

:- pred report_type_error(prog_context, int, list(prog_term), pred_or_func, 
		io__state, io__state).
:- mode report_type_error(in, in, in, in, di, uo) is det.

report_type_error(Context, ArgNum, RemainingTerms, PredOrFunc) -->
	prog_out__write_context(Context),
	(
		% Report a different error message for the return value of a 
		% function.
		{ PredOrFunc = function },
		{ RemainingTerms = [] }
	->
		io__write_string("Type error in return value of function.\n")
	;
		io__write_string("Type error in argument "),
		io__write_int(ArgNum),
		io__write_string(".\n")
	),
	io__set_exit_status(1).

%---------------------------------------------------------------------------%

:- pred write_fact_table_header(sym_name, pred_info, string,
		list(fact_arg_info), io__output_stream, string, string,
		fact_result, io__state, io__state).
:- mode write_fact_table_header(in, in, in, in, in, out, out, out,
		di, uo) is det.

write_fact_table_header(PredName, PredInfo, FileName, FactArgInfos,
		OutputStream, C_HeaderCode, StructName, Result) -->
	{ library__version(Version) },
	io__write_strings(OutputStream,
		["/*\n** Automatically generated from `", FileName,
		"' by the\n** Mercury compiler, version ", Version,
		".  Do not edit.\n*/\n\n"]),
	io__write_string(OutputStream, "#include ""mercury_imp.h""\n\n"),

	{ make_fact_table_identifier(PredName, Identifier) },
	{ string__append_list(["mercury__", Identifier, "_fact_table"],
		StructName) },

	% Define a struct for a fact table entry.
	{ pred_info_context(PredInfo, Context) },  % location of :- pred decl
	write_fact_table_struct(FactArgInfos, 1, Context, StructContents, 
		Result),
	{ StructContents = "" ->
		StructDef = ""
	;
		string__append_list(
			["struct ", StructName, "_struct {\n", StructContents, 
			"};\n\n"], StructDef)
	},
	io__write_string(OutputStream, StructDef),

	% Define a struct for a hash table entry.
	{ HashDef = "
#ifndef MERCURY_FACT_TABLE_HASH_TABLES
#define MERCURY_FACT_TABLE_HASH_TABLES

struct MR_fact_table_hash_table_s {
	MR_Integer size;		/* size of the hash table */
	struct MR_fact_table_hash_entry_s *table;	/* the actual table */
};

struct MR_fact_table_hash_table_f {
	MR_Integer size;		/* size of the hash table */
	struct MR_fact_table_hash_entry_f *table;	/* the actual table */
};

struct MR_fact_table_hash_table_i {
	MR_Integer size;		/* size of the hash table */
	struct MR_fact_table_hash_entry_i *table;	/* the actual table */
};

/* hash table for string keys */
struct MR_fact_table_hash_entry_s {
	MR_ConstString key;   /* lookup key */
	const MR_Word *index; /* index into fact table data array	     */
		          /* or pointer to hash table for next argument      */
#if TAGBITS < 2
	short type;	  /* 0 if entry empty, 1 if entry is a pointer to the*/
			  /* data table, 2 if entry is a pointer to another  */
			  /* hash table 				     */
#endif
	int next;	  /* location of next entry with the same hash value */
};

/* hash table for float keys */
struct MR_fact_table_hash_entry_f {
	MR_Float key;
	const MR_Word *index;
#if TAGBITS < 2
	short type;
#endif
	int next;
};

/* hash table for int keys */
struct MR_fact_table_hash_entry_i {
	MR_Integer key;
	const MR_Word *index;
#if TAGBITS < 2
	short type;
#endif
	int next;
};


#if TAGBITS >= 2
	#define MR_FACT_TABLE_MAKE_TAGGED_INDEX(i,t)   \
		MR_mkword(MR_mktag(t), MR_mkbody(i))
	#define MR_FACT_TABLE_MAKE_TAGGED_POINTER(p,t) \
		MR_mkword(MR_mktag(t), p)
	#define MR_FACT_TABLE_HASH_ENTRY_TYPE(p)       \
		MR_tag((MR_Word)((p).index))
	#define MR_FACT_TABLE_HASH_INDEX(w)            \
		MR_unmkbody(w)
	#define MR_FACT_TABLE_HASH_POINTER(w)          \
		MR_body(w,MR_tag(w))
#else
	#define MR_FACT_TABLE_MAKE_TAGGED_INDEX(i,t)   \
		((const MR_Word *) i), (t)
	#define MR_FACT_TABLE_MAKE_TAGGED_POINTER(p,t) \
		((const MR_Word *) p), (t)
	#define MR_FACT_TABLE_HASH_ENTRY_TYPE(p)       ((p).type)
	#define MR_FACT_TABLE_HASH_INDEX(w)            (w)
	#define MR_FACT_TABLE_HASH_POINTER(w)          (w)
#endif

#endif /* not MERCURY_FACT_TABLE_HASH_TABLES */
" }, 

	io__write_string(OutputStream, HashDef),
	{ string__append_list([ StructDef, HashDef ], C_HeaderCode) }.

	% Create a struct for the fact table consisting of any arguments
	% that are output in some mode.
	% Also ensure that are arguments are either string, float or int.
:- pred write_fact_table_struct(list(fact_arg_info), int, prog_context, 
		string, fact_result, io__state, io__state).
:- mode write_fact_table_struct(in, in, in, out, out, di, uo) is det.

write_fact_table_struct([], _, _, "", ok) --> [].
write_fact_table_struct([Info | Infos], I, Context, StructContents, Result) -->
	{ Info = fact_arg_info(Type, _IsInput, IsOutput) },
	(
		{ Type = term__functor(term__atom("string"), [], _) }
	->
		{ I1 is I + 1 },
		write_fact_table_struct(Infos, I1, Context,
			StructContents1, Result),
		{
			IsOutput = yes,
			string__format("\tMR_ConstString V_%d;\n", [i(I)], 
				StructContents0),
			string__append(StructContents0, StructContents1, 
				StructContents)
		;
			IsOutput = no,
			StructContents = StructContents1
		}
	;
		{ Type = term__functor(term__atom("int"), [], _) }
	->
		{ I1 is I + 1 },
		write_fact_table_struct(Infos, I1, Context,
			StructContents1, Result),
		{
			IsOutput = yes,
			string__format("\tMR_Integer V_%d;\n", [i(I)], 
				StructContents0),
			string__append(StructContents0, StructContents1, 
				StructContents)
		;
			IsOutput = no,
			StructContents = StructContents1
		}
	;
		{ Type = term__functor(term__atom("float"), [], _) }
	->
		{ I1 is I + 1 },
		write_fact_table_struct(Infos, I1, Context,
			StructContents1, Result),
		{
			IsOutput = yes,
			string__format("\tMR_Float V_%d;\n", [i(I)], 
				StructContents0),
			string__append(StructContents0, StructContents1, 
				StructContents)
		;
			IsOutput = no,
			StructContents = StructContents1
		}
	;
		% Report an error for types other than string, int and float.
		% Context is the `:- pred' or `:- func' declaration where the 
		% types are declared.
		prog_out__write_context(Context),
		io__write_string("Error: invalid type in fact table:\n"),
		prog_out__write_context(Context),
		io__write_string("  only `string', `int' and `float' types "),
		io__write_string("are allowed in fact tables.\n"),
		io__set_exit_status(1),
		{ Result = error },
		{ StructContents = "" }
	).

%---------------------------------------------------------------------------%
	% Initialise list of fact argument information.
	% Input and output flags are initialised to `no' and filled in 
	% correctly by infer_determinism_pass_1.
:- pred init_fact_arg_infos(list(type), list(fact_arg_info)).
:- mode init_fact_arg_infos(in, out) is det.

init_fact_arg_infos([], []).
init_fact_arg_infos([Type | Types], [Info | Infos]) :-
	Info = fact_arg_info(Type, no, no),
	init_fact_arg_infos(Types, Infos).

:- pred fill_in_fact_arg_infos(list(mode), module_info, list(fact_arg_info), 
		list(fact_arg_info)).
:- mode fill_in_fact_arg_infos(in, in, in, out) is det.

fill_in_fact_arg_infos([], _, [], []).
fill_in_fact_arg_infos([_|_], _, [], _) :- 
	error("fill_in_fact_arg_infos: too many argmodes").
fill_in_fact_arg_infos([], _, [_|_], _) :- 
	error("fill_in_fact_arg_infos: too many fact_arg_infos").
fill_in_fact_arg_infos([Mode | Modes], ModuleInfo, [Info0 | Infos0],
		[Info | Infos]) :-
	Info0 = fact_arg_info(Type, IsInput, _IsOutput),
	( mode_is_fully_input(ModuleInfo, Mode) ->
		% XXX Info = fact_arg_info(Type, yes, IsOutput)

		% XXX currently the first input mode requires _all_ arguments to
		% be written in the fact data table so it can do lookups on
		% backtracking.  This may change if it is found to be less
		% efficient than doing these lookups via the hash table.
		Info = fact_arg_info(Type, yes, yes)

	; mode_is_fully_output(ModuleInfo, Mode) ->
		Info = fact_arg_info(Type, IsInput, yes)
	;
		% this is a mode error that will be reported by 
		% infer_proc_determinism_pass_1
		Info = Info0
	),
	fill_in_fact_arg_infos(Modes, ModuleInfo, Infos0, Infos).


%---------------------------------------------------------------------------%

	% First pass of determinism inference.
	% (out, out, ..., out) procs are multidet and (in, in, .., in) procs are
	% semidet.  Return a list of procs containing both in's and out's.
	% These need further analysis later in pass 2.
:- pred infer_determinism_pass_1(pred_info, pred_info,prog_context,
		module_info, list(proc_id), bool, bool, bool,
		list(fact_arg_info), list(fact_arg_info),
		fact_result, io__state, io__state).
:- mode infer_determinism_pass_1(in, out, in, in, out, out, out, out, in, out,
		out, di, uo) is det.

infer_determinism_pass_1(PredInfo0, PredInfo, Context, ModuleInfo, CheckProcs,
		ExistsAllInMode, WriteHashTables, WriteDataTable,
		FactArgInfos0, FactArgInfos, Result) -->
	{ pred_info_procedures(PredInfo0, ProcTable0) },
	{ pred_info_procids(PredInfo0, ProcIDs) },
	( { ProcIDs = [] } ->
		% There are no declared modes so report an error.
		{ pred_info_name(PredInfo0, PredString) },
		{ pred_info_arity(PredInfo0, Arity) },
		prog_out__write_context(Context),
		io__format("Error: no modes declared for fact table `%s/%d'.\n",
			[s(PredString), i(Arity)]),
		io__set_exit_status(1),
		{ PredInfo = PredInfo0 },
		{ CheckProcs = [] },
		{ ExistsAllInMode = no },
		{ WriteHashTables = no },
		{ WriteDataTable = no },
		{ FactArgInfos = FactArgInfos0 },
		{ Result = error }
	;
		infer_proc_determinism_pass_1(ProcIDs, ProcTable0, ProcTable,
			ModuleInfo, [], CheckProcs0, MaybeAllInProc,
			WriteHashTables, WriteDataTable, FactArgInfos0,
			FactArgInfos),

		% If there is an all_in procedure, it needs to be put on the
		% end of the list so a sort file is created for it.  This
		% is required when building the hash table, not for 
		% determinism inference.
		{
			MaybeAllInProc = yes(ProcID),
			CheckProcs1 = [ProcID | CheckProcs0],
			ExistsAllInMode = yes
		;
			MaybeAllInProc = no,
			CheckProcs1 = CheckProcs0,
			ExistsAllInMode = no
		},

		% need to get order right for CheckProcs because first procedure
		% in list is used to derive the primary lookup key.
		{ list__reverse(CheckProcs1, CheckProcs) },
		{ pred_info_set_procedures(PredInfo0, ProcTable, PredInfo) },
		{ Result = ok }
	).

:- pred infer_proc_determinism_pass_1(list(proc_id), proc_table, proc_table,
		module_info, list(proc_id), list(proc_id), maybe(proc_id),
		bool, bool, list(fact_arg_info), list(fact_arg_info),
		io__state, io__state).
:- mode infer_proc_determinism_pass_1(in, in, out, in, in, out, out, out, out,
		in, out, di, uo) is det.

infer_proc_determinism_pass_1([], ProcTable, ProcTable, _, CheckProcs,
		CheckProcs, no, no, no, FactArgInfos, FactArgInfos) --> [].
infer_proc_determinism_pass_1([ProcID | ProcIDs], ProcTable0, ProcTable,
			ModuleInfo, CheckProcs0, CheckProcs, MaybeAllInProc, 
			WriteHashTables, WriteDataTable, FactArgInfos0,
			FactArgInfos) -->
	{ map__lookup(ProcTable0, ProcID, ProcInfo0) },
	{ proc_info_argmodes(ProcInfo0, ArgModes) },
	{ fill_in_fact_arg_infos(ArgModes, ModuleInfo, FactArgInfos0, 
		FactArgInfos1) },
	{ fact_table_mode_type(ArgModes, ModuleInfo, ModeType) },
	(
		{ ModeType = all_in },
		{ InferredDetism = inferred(semidet) },
		{ CheckProcs1 = CheckProcs0 },
		{ WriteHashTables0 = yes },
		{ WriteDataTable0 = no },
		{ MaybeAllInProc0 = yes(ProcID) }
	;
		{ ModeType = all_out },
		{ proc_info_declared_determinism(ProcInfo0, MaybeDet) },
		{
			(
				MaybeDet = yes(cc_multidet)
			;
				MaybeDet = yes(cc_nondet)
			)
		->
			InferredDetism = inferred(cc_multidet)
		;
			InferredDetism = inferred(multidet)
		},
		{ CheckProcs1 = CheckProcs0 },
		{ WriteHashTables0 = no },
		{ WriteDataTable0 = yes },
		{ MaybeAllInProc0 = no }
	;
		{ ModeType = in_out },

			% Don't have enough info to infer determinism yet.
			% Put it off till the second pass.
		{ InferredDetism = not_yet },
			% add to list and check in pass 2
		{ CheckProcs1 = [ProcID | CheckProcs0] },
		{ WriteHashTables0 = yes },
		{ WriteDataTable0 = yes },
		{ MaybeAllInProc0 = no }
	;
		{ ModeType = other }, 		% mode error
		{ InferredDetism = error },
		{ proc_info_context(ProcInfo0, Context) },
		prog_out__write_context(Context),
		io__write_string(
			"Error: only `in' and `out' modes are currently "),
		io__write_string("supported in fact tables.\n"),
		io__set_exit_status(1),
		{ CheckProcs1 = CheckProcs0 },
		{ WriteHashTables0 = no },
		{ WriteDataTable0 = no },
		{ MaybeAllInProc0 = no }
	;
		{ ModeType = unknown },		 % mode error
		{ InferredDetism = error },
		{ proc_info_context(ProcInfo0, Context) },
		prog_out__write_context(Context),
		io__write_string("Error: mode list for procedure is empty.\n"),
		io__set_exit_status(1),
		{ CheckProcs1 = CheckProcs0 },
		{ WriteHashTables0 = no },
		{ WriteDataTable0 = no },
		{ MaybeAllInProc0 = no }
	),
	{
		InferredDetism = inferred(Determinism)
	->
		proc_info_set_inferred_determinism(ProcInfo0, Determinism, 
			ProcInfo),
		map__det_update(ProcTable0, ProcID, ProcInfo, ProcTable1)
	;
		ProcTable1 = ProcTable0
	},
	infer_proc_determinism_pass_1(ProcIDs, ProcTable1, ProcTable, 
		ModuleInfo, CheckProcs1, CheckProcs, MaybeAllInProc1,
		WriteHashTables1, WriteDataTable1, FactArgInfos1, FactArgInfos),
	{
		MaybeAllInProc0 = yes(_),
		MaybeAllInProc = MaybeAllInProc0
	;
		MaybeAllInProc0 = no,
		MaybeAllInProc = MaybeAllInProc1
	},
	{ bool__or(WriteHashTables0, WriteHashTables1, WriteHashTables) },
	{ bool__or(WriteDataTable0, WriteDataTable1, WriteDataTable) }.

% Return the fact_table_mode_type for a procedure.  
:- pred fact_table_mode_type(list(mode), module_info, fact_table_mode_type).
:- mode fact_table_mode_type(in, in, out) is det.

fact_table_mode_type([], _, unknown).
fact_table_mode_type([Mode | Modes], ModuleInfo, ModeType) :-
	( mode_is_fully_input(ModuleInfo, Mode) ->
		ModeType0 = all_in
	; mode_is_fully_output(ModuleInfo, Mode) ->
		ModeType0 = all_out
	;
		ModeType0 = other
	),
	( ModeType0 = other ->
		ModeType = other
	;
		fact_table_mode_type(Modes, ModuleInfo, ModeType1),
		( ModeType1 = unknown ->
			ModeType = ModeType0
		; ModeType1 = other ->
			ModeType = other
		; ModeType1 = ModeType0 ->
			ModeType = ModeType0
		;
			ModeType = in_out
		)
	).

%---------------------------------------------------------------------------%

	% open_sort_files(ProcIDs, ProcStreams)
	% Open a temporary sort file for each proc_id in ProcIDs.
	% Return a list of proc_streams for all the files opened.
:- pred open_sort_files(list(proc_id), list(proc_stream),
		io__state, io__state).
:- mode open_sort_files(in, out, di, uo) is det.

open_sort_files([], []) --> [].
open_sort_files([ProcID | ProcIDs], ProcStreams) -->
	io__make_temp(SortFileName),
	io__open_output(SortFileName, Result),
	(
		{ Result = ok(Stream) },
		open_sort_files(ProcIDs, ProcStreams0),
		{ ProcStreams = [proc_stream(ProcID, Stream) | ProcStreams0] }
	;
		{ Result = error(ErrorCode) },
		{ ProcStreams = [] },
		{ io__error_message(ErrorCode, Message) },
		io__write_strings([ 
		    "Error opening file `", 
		    SortFileName, 
		    "' for output: ",
		    Message,
		    ".\n" ]),
		io__set_exit_status(1)
	).

	% write_sort_file_lines(ProcStreams, ProcTable, Terms)
	% Write out a line to each sort file for this fact.
	% The line is made up of the input arguments of the procedure (the key)
	% followed by the position of the fact in the original input table.
	%
	% Note lines written out here need to be read back in by 
	% read_sort_file_line so if any changes are made here, corresponding
	% changes should be made there too.
:- pred write_sort_file_lines(list(proc_stream), proc_table, list(prog_term), 
		module_info, string, list(fact_arg_info), bool,
		io__state, io__state).
:- mode write_sort_file_lines(in, in, in, in, in, in, in, di, uo) is det.

write_sort_file_lines([], _, _, _, _, _, _) --> [].
write_sort_file_lines([proc_stream(ProcID, Stream) | ProcStreams], ProcTable,
		Terms, ModuleInfo, FactNumStr, FactArgInfos, IsPrimary) -->
	{ map__lookup(ProcTable, ProcID, ProcInfo) },
	{ proc_info_argmodes(ProcInfo, ArgModes) },
	{ assoc_list__from_corresponding_lists(ArgModes, Terms, ModeTerms) },
	{ make_sort_file_key(ModeTerms, ModuleInfo, Key) },
	{
		IsPrimary = yes,
		assoc_list__from_corresponding_lists(FactArgInfos, Terms, 
			InfoTerms),
		make_fact_data_string(InfoTerms, DataString)
	;
		IsPrimary = no,
		DataString = ""
	},
	io__write_strings(Stream, [Key, "~", FactNumStr, "~", DataString,"\n"]),
	write_sort_file_lines(ProcStreams, ProcTable, Terms, ModuleInfo,
		FactNumStr, [],no).

%---------------------------------------------------------------------------%

	% Create a key for the fact table entry.
	% Arguments are separated by ":".
	% Colons in string literals are replaced by "\c", tildes are replaced
	% by "\t", newlines are replaced by "\n" and backslashes by "\\".
	% This ensures that each possible set of arguments maps to a unique key
	% and guarantees that duplicate keys will be adjacent after sorting
	% with the sort program.  The tilde ('~') character is used in the
	% sort file to separate the sort key from the data.

:- pred make_sort_file_key(assoc_list(mode, prog_term), module_info, string).
:- mode make_sort_file_key(in, in, out) is det.

make_sort_file_key([], _, "").
make_sort_file_key([(Mode - Term) | ModeTerms], ModuleInfo, Key) :-
	(
		mode_is_fully_input(ModuleInfo, Mode),
		Term = term__functor(Const, [], _Context)
	->
		make_key_part(Const, KeyPart),
		make_sort_file_key(ModeTerms, ModuleInfo, Key0),
		string__append(":", Key0, Key1), % field separator
		string__append(KeyPart, Key1, Key)
	;
		make_sort_file_key(ModeTerms, ModuleInfo, Key)
	).

	% like make_sort_file_key but for the output arguments of the fact
:- pred make_fact_data_string(assoc_list(fact_arg_info, prog_term), string).
:- mode make_fact_data_string(in, out) is det.

make_fact_data_string([], "").
make_fact_data_string([fact_arg_info(_, _, IsOutput) - Term | InfoTerms],
		String) :-
	(
		IsOutput = yes,
		Term = term__functor(Const, [], _)
	->
		make_key_part(Const, KeyPart),
		make_fact_data_string(InfoTerms, String0),
		string__append_list([KeyPart, ":", String0], String)
	;
		make_fact_data_string(InfoTerms, String)
	).

:- pred make_key_part(const, string).
:- mode make_key_part(in, out) is det.

make_key_part(term__atom(_), _):- 
	error("make_key_part: enumerated types are not supported yet.").
make_key_part(term__integer(I), K) :- 
	% convert int to base 36 to reduce the size of the I/O.
	string__int_to_base_string(I, 36, K).
make_key_part(term__float(F), K) :- 
	string__float_to_string(F, K).
make_key_part(term__string(S), K) :-
	string__to_char_list(S, Cs0),
	key_from_chars(Cs0, Cs),
	string__from_char_list(Cs, K).

	% escape all backslashes with a backslash and replace all
	% newlines with "\n", colons with "\c" and tildes with "\t".
:- pred key_from_chars(list(char), list(char)).
:- mode key_from_chars(in, out) is det.

key_from_chars(Cs, ECs) :-
	key_from_chars_2(Cs, [], ECs0),
	list__reverse(ECs0, ECs).

:- pred key_from_chars_2(list(char), list(char), list(char)).
:- mode key_from_chars_2(in, in, out) is det.

key_from_chars_2([], ECs, ECs).
key_from_chars_2([C | Cs], ECs0, ECs) :-
	(
		C = ('\\')
	->
		ECs1 = ['\\', '\\' | ECs0]
	;
		C = (':')
	->
		ECs1 = ['c', '\\' | ECs0]
	;
		C = ('~')
	->
		ECs1 = ['t', '\\' |ECs0]
	;
		C = ('\n')
	->
		ECs1 = ['n', '\\' | ECs0]
	;
		ECs1 = [C | ECs0]
	),
	key_from_chars_2(Cs, ECs1, ECs).

%---------------------------------------------------------------------------%

	% infer_determinism_pass_2(ProcStreams, ProcFiles,
	% 		ProcTable0, ProcTable),
	% Close each sort file then run `sort' on it to see if the keys are
	% unique.  If they are, the procedure is semidet, otherwise it is 
	% nondet.  Return a list of (proc_id, filename) pairs and the updated
	% proc_table.
:- pred infer_determinism_pass_2(list(proc_stream),
		assoc_list(proc_id, string), bool, proc_table, proc_table,
		io__state, io__state).
:- mode infer_determinism_pass_2(in, out, in, in, out, di, uo) is det.

infer_determinism_pass_2([], [], _, ProcTable, ProcTable) --> [].
infer_determinism_pass_2([proc_stream(ProcID, Stream) | ProcStreams], 
		[(ProcID - FileName) | ProcFiles], ExistsAllInMode, 
		ProcTable0, ProcTable) -->
	{ map__lookup(ProcTable0, ProcID, ProcInfo0) },
	io__output_stream_name(Stream, FileName),
	io__close_output(Stream),
	{ make_command_string(string__format(
		"sort -o %s %s && cut -d'~' -f1 %s | sort -cu >/dev/null 2>&1",
		[s(FileName), s(FileName), s(FileName)]), double, Command) },
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Invoking system command `"),
	maybe_write_string(Verbose, Command),
	maybe_write_string(Verbose, "'..."),
	io__call_system(Command, Result),
	maybe_write_string(Verbose, "done.\n"),
	(
	    { Result = ok(ExitStatus) },

	    % sort -cu returns 0 if file is sorted and contains
	    % no duplicate keys, >=1 if duplicate keys exist 
	    (
		{ 
		    ExitStatus = 0
		;
		    % this is an all_in mode so it is semidet.
		    ExistsAllInMode = yes,
		    ProcStreams = []
		}
	    ->
		    % no duplicate keys => procedure is semidet
		{ Determinism = semidet }
	    ;
		{ ExitStatus >= 1}
	    ->
		    % duplicate keys => procedure is nondet
		{ proc_info_declared_determinism(ProcInfo0, MaybeDet) },
		{
		    (
			MaybeDet = yes(cc_multidet)
		    ;
			MaybeDet = yes(cc_nondet)
		    )
		->
		    Determinism = cc_nondet
		;
		    Determinism = nondet
		}
	    ;
		io__progname_base("mercury_compile", ProgName),
		io__write_strings([
		    ProgName,
		    ": an error occurred in the `sort' program\n",
		    "  during fact table determinism inference.\n"
		    ]),
		io__set_exit_status(1),
		{ Determinism = erroneous }
	    )
	;
	    { Result = error(ErrorCode) },
	    { io__error_message(ErrorCode, ErrorMessage) },
	    io__progname_base("mercury_compile", ProgName),
	    io__write_strings([ 
		ProgName,
		": error executing system command `sort':\n  ", 
		ErrorMessage, 
		".\n" ]),
	    io__set_exit_status(1),
	    { Determinism = erroneous }
	),
	{ proc_info_set_inferred_determinism(ProcInfo0, Determinism, 
	    ProcInfo)},
	{ map__det_update(ProcTable0, ProcID, ProcInfo, ProcTable1) },
	infer_determinism_pass_2(ProcStreams, ProcFiles, ExistsAllInMode,
		ProcTable1, ProcTable).

%---------------------------------------------------------------------------%

		% write out the fact table data arrays and hash tables
:- pred write_fact_table_arrays(assoc_list(proc_id, string), string, string, 
		proc_table, module_info, int, list(fact_arg_info), bool, bool, 
		io__output_stream, string, proc_id, io__state, io__state).
:- mode write_fact_table_arrays(in, in, in, in, in, in, in, in, in, in, out, 
		out, di, uo) is det.

write_fact_table_arrays(ProcFiles0, DataFileName, StructName, ProcTable, 
		ModuleInfo, NumFacts, FactArgInfos, WriteHashTables,
		WriteDataTable, OutputStream, C_HeaderCode, PrimaryProcID) -->
	(
		% no sort files => there was only and all_out mode
		%	=> nothing left to be done here
	    { ProcFiles0 = [] },
	    { C_HeaderCode = "" },
			% This won't get used anyway.
	    { hlds_pred__initial_proc_id(PrimaryProcID) }
	;
	    { ProcFiles0 = [(PrimaryProcID - FileName) | ProcFiles1] },
	    (
		{ WriteHashTables = yes },
		{
			% If there we need to build secondary hash tables 
			% (i.e. if there's >1 input mode) we need to create
			% a ``FactMap'' while writing out the primary table.
		    ProcFiles1 = [_|_],
		    CreateFactMap = yes
		;
		    ProcFiles1 = [],
		    CreateFactMap = no
		},
		write_primary_hash_table(PrimaryProcID, FileName, DataFileName, 
		    StructName, ProcTable, ModuleInfo, OutputStream,
		    FactArgInfos, WriteDataTable, NumFacts, CreateFactMap,
		    Result0, FactMap, C_HeaderCode0),
		( 
		    { Result0 = ok },
		    write_secondary_hash_tables(ProcFiles1, StructName,
			ProcTable, ModuleInfo, OutputStream, FactMap,
			FactArgInfos, "", C_HeaderCode1),
		    { string__append(C_HeaderCode0, C_HeaderCode1,
			C_HeaderCode) }
		;
		    { Result0 = error },
		    { C_HeaderCode = C_HeaderCode0 }
		)
	    ;
		{ WriteHashTables = no },
		{ C_HeaderCode = "" }
	    )
	).

	% write out the data for the fact table
:- pred write_fact_table_data(int, list(list(fact_arg)), string,
		io__output_stream, io__state, io__state).
:- mode write_fact_table_data(in, in, in, in, di, uo) is det.

write_fact_table_data(_, [], _, _) --> [].
write_fact_table_data(FactNum, [Args | ArgsList], StructName, OutputStream) -->
	write_fact_data(FactNum, Args, StructName, OutputStream),
	{ NextFactNum is FactNum + 1 },
	write_fact_table_data(NextFactNum, ArgsList, StructName, OutputStream).


	% Write out the data for a single fact, starting a new array if
	% necessary.  Note:  this predicate will not write the declaration
	% or opening brace for the first array or the closing brace of the last
	% array.
:- pred write_fact_data(int, list(fact_arg), string, io__output_stream,
		io__state, io__state).
:- mode write_fact_data(in, in, in, in, di, uo) is det.

write_fact_data(FactNum, Args, StructName, OutputStream) -->
	fact_table_size(FactTableSize),
	( { 0 is FactNum mod FactTableSize } ->
		( { FactNum = 0 } ->
			[]
		;
			write_closing_brace(OutputStream),
			write_new_data_array(OutputStream,
				StructName, FactNum)
		),
		globals__io_lookup_bool_option(very_verbose, VeryVerbose),
		( { VeryVerbose = yes } ->
			io__format("%% Writing fact %d\n", [i(FactNum)])
		;
			[]
		)
	;
		[]
	),
	io__write_string(OutputStream, "\t{ "),
	write_fact_args(Args, OutputStream),
	io__write_string(OutputStream, " },\n").

	% Write out the declaration of a new data array followed by " = {\n"
:- pred write_new_data_array(io__output_stream, string, int,
		io__state, io__state).
:- mode write_new_data_array(in, in, in, di, uo) is det.

write_new_data_array(OutputStream, StructName, FactNum) -->
	io__format(OutputStream, "const struct %s_struct %s%d[] = {\n", 
		[s(StructName), s(StructName), i(FactNum)]).

	% Write out the closing brace of an array.
:- pred write_closing_brace(io__output_stream, io__state, io__state).
:- mode write_closing_brace(in, di, uo) is det.

write_closing_brace(OutputStream) -->
	io__write_string(OutputStream, "};\n\n").

:- pred write_fact_args(list(fact_arg), io__output_stream,
		io__state, io__state).
:- mode write_fact_args(in, in, di, uo) is det.

write_fact_args([], _) --> [].
write_fact_args([Arg | Args], OutputStream) -->
	(
		{ Arg = term__string(String) },
		io__set_output_stream(OutputStream, OldStream),
		io__write_string(""""),
		output_c_quoted_string(String),
		io__write_string(""", "),
		io__set_output_stream(OldStream, _)
	;
		{ Arg = term__integer(Int) },
		io__write_int(OutputStream, Int),
		io__write_string(OutputStream, ", ")
	;
		{ Arg = term__float(Float) },
		io__write_float(OutputStream, Float),
		io__write_string(OutputStream, ", ")
	;
		{ Arg = term__atom(_) },
		{ error("write_fact_terms: unsupported type") }
	),
	write_fact_args(Args, OutputStream).

	% If a data table has been created in a separate file, append it to the
	% end of the main output file and then delete it.
:- pred maybe_append_data_table(bool, string, string, io__state, io__state).
:- mode maybe_append_data_table(in, in, in, di, uo) is det.

maybe_append_data_table(no, _, _) --> [].
maybe_append_data_table(yes, OutputFileName, DataFileName) -->
	{ make_command_string(string__format("cat %s >>%s", 
		[s(DataFileName), s(OutputFileName)]), forward, Command) },
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Invoking system command `"),
	maybe_write_string(Verbose, Command),
	maybe_write_string(Verbose, ",..."),
	io__call_system(Command, Result),
	maybe_write_string(Verbose, "done.\n"),
	(
		{ Result = ok(ExitStatus) },
		( { ExitStatus = 0 } ->
			[]
		;
			io__write_strings([
				"An error occurred while concatenating\n",
				"  fact table output files.\n" ]),
			io__set_exit_status(1)
		)
	;
		{ Result = error(ErrorCode) },
		{ io__error_message(ErrorCode, ErrorMessage) },
		io__progname_base("mercury_compile", ProgName),
		io__write_strings([
			ProgName,
			": error executing system command `cat':\n  ",
			ErrorMessage,
			".\n" ]),
		io__set_exit_status(1)
	),
	delete_temporary_file(DataFileName).

	% Write hash tables for the primary key.
	% Create a map from indices in the original input table to the table
	% sorted on the primary key.
	% Write out the data table if required.
:- pred write_primary_hash_table(proc_id, string, string, string, proc_table, 
		module_info, io__output_stream, list(fact_arg_info), bool,
		int, bool, fact_result, map(int, int), string,
		io__state, io__state).
:- mode write_primary_hash_table(in, in, in, in, in, in, in, in, in, in, in,
		out, out, out, di, uo) is det.

write_primary_hash_table(ProcID, FileName, DataFileName, StructName, ProcTable,
		ModuleInfo, OutputStream, FactArgInfos, WriteDataTable,
		NumFacts, CreateFactMap, Result, FactMap, C_HeaderCode) -->
	{ map__init(FactMap0) },
	io__see(FileName, Result0),
	(
		{ Result0 = ok },
		(
			{ WriteDataTable  = yes },
			io__open_output(DataFileName, Result1),
			(
				{ Result1 = ok(DataStream) },
				{ MaybeDataStream = yes(DataStream) },
				    % opening brace for first fact data array
				write_new_data_array(DataStream, StructName, 0),
				{ Result2 = ok }
			;
				{ Result1 = error(ErrorCode1) },
				{ io__error_message(ErrorCode1, ErrorMessage1)},
				io__write_strings([
					"Error opening file `",
					DataFileName,
					"' for output: ",
					ErrorMessage1,
					".\n" ]),
				io__set_exit_status(1),
				{ MaybeDataStream = no },
				{ Result2 = error }
			)
		;
			{ WriteDataTable = no },
			{ MaybeDataStream = no },
			{ Result2 = ok }
		),
		(
			{ Result2 = ok },
			{ proc_id_to_int(ProcID, ProcInt) },
			{ string__format("%s_hash_table_%d_", 
				[s(StructName), i(ProcInt)], HashTableName) },
			{ string__format(
			  "extern struct MR_fact_table_hash_table_i %s0;\n",
				[s(HashTableName)], C_HeaderCode0) },
				% Note: the type declared here is not
				% necessarily correct.  The type is declared
				% just to stop the C compiler emitting warnings.
			{ map__lookup(ProcTable, ProcID, ProcInfo) },
			{ proc_info_argmodes(ProcInfo, ArgModes) },
			read_sort_file_line(FactArgInfos, ArgModes, ModuleInfo,
				MaybeFirstFact),
			( 
				{ MaybeFirstFact = yes(FirstFact) },
				build_hash_table(0, 0, HashTableName,
					StructName, 0, ArgModes, ModuleInfo,
					FactArgInfos, yes, OutputStream,
					FirstFact, MaybeDataStream,
					CreateFactMap, FactMap0, FactMap),
				{ Result = ok }
			;
				{ MaybeFirstFact = no },
				{ Result = error },
				{ FactMap = FactMap0 }
			)
		;
			{ Result2 = error },
			{ Result = error },
			{ FactMap = FactMap0 },
			{ C_HeaderCode0 = "" }
		),
		(
			{ MaybeDataStream = yes(DataStream1) },
				% closing brace for last fact data array
			write_closing_brace(DataStream1),
			write_fact_table_pointer_array(NumFacts, StructName,
				DataStream1, C_HeaderCode1),
			io__close_output(DataStream1),
			{ string__append(C_HeaderCode0, C_HeaderCode1,
				C_HeaderCode) }
		;
			{ MaybeDataStream = no },
			{ C_HeaderCode = C_HeaderCode0 }
		),
		io__seen,
		delete_temporary_file(FileName)
	;
		{ Result0 = error(ErrorCode0) },
		{ io__error_message(ErrorCode0, ErrorMessage0) },
		io__write_strings([ 
			"Error opening file `", 
			FileName, 
			"' for input: ",
			ErrorMessage0,
			".\n" ]),
		io__set_exit_status(1),
		{ Result = error },
		{ FactMap = FactMap0 },
		{ C_HeaderCode = "" }
	).

	% Build hash tables for non-primary input procs.
:- pred write_secondary_hash_tables(assoc_list(proc_id, string), string,
		proc_table, module_info, io__output_stream, map(int, int),
		list(fact_arg_info), string, string, io__state, io__state).
:- mode write_secondary_hash_tables(in, in, in, in, in, in, in, in, out,
		di, uo) is det.

write_secondary_hash_tables([], _, _, _, _, _, _, C_HeaderCode, C_HeaderCode)
		--> [].
write_secondary_hash_tables([ProcID - FileName | ProcFiles], StructName,
		ProcTable, ModuleInfo, OutputStream, FactMap, FactArgInfos,
		C_HeaderCode0, C_HeaderCode) -->
	io__see(FileName, Result0),
	(
		{ Result0 = ok },
		{ proc_id_to_int(ProcID, ProcInt) },
		{ string__format("%s_hash_table_%d_",
			[s(StructName), i(ProcInt)], HashTableName) },
		{ string__format(
			"extern struct MR_fact_table_hash_table_i %s0;\n",
			[s(HashTableName)], C_HeaderCode1) },
			% Note: the type declared here is not
			% necessarily correct.  The type is declared
			% just to stop the C compiler emitting warnings.
		{ string__append(C_HeaderCode1, C_HeaderCode0,
			C_HeaderCode2) },
		{ map__lookup(ProcTable, ProcID, ProcInfo) },
		{ proc_info_argmodes(ProcInfo, ArgModes) },
		read_sort_file_line(FactArgInfos, ArgModes, ModuleInfo,
			MaybeFirstFact),
		(
			{ MaybeFirstFact = yes(FirstFact) },
			build_hash_table(0, 0, HashTableName, StructName, 0,
				ArgModes, ModuleInfo, FactArgInfos, bool:no,
				OutputStream, FirstFact, no, no, FactMap, _),
			io__seen,
			delete_temporary_file(FileName),
			write_secondary_hash_tables(ProcFiles, StructName,
				ProcTable, ModuleInfo, OutputStream, FactMap,
				FactArgInfos, C_HeaderCode2, C_HeaderCode)
		;
			{ MaybeFirstFact = no },
			io__seen,
			{ C_HeaderCode = C_HeaderCode2 }
		)
	;
		{ Result0 = error(ErrorCode0) },
		{ io__error_message(ErrorCode0, ErrorMessage0) },
		io__write_strings([ 
			"Error opening file `", 
			FileName, 
			"' for input: ",
			ErrorMessage0,
			".\n" ]),
		io__set_exit_status(1),
		{ C_HeaderCode = C_HeaderCode0 }
	).

:- pred read_sort_file_line(list(fact_arg_info), list(mode), module_info,
		maybe(sort_file_line), io__state, io__state).
:- mode read_sort_file_line(in, in, in, out, di, uo) is det.

read_sort_file_line(FactArgInfos, ArgModes, ModuleInfo, MaybeSortFileLine) -->
	io__read_line(Result),
	(
		{ Result = ok(LineChars) },
		{ string__from_char_list(LineChars, LineString) },
		{ split_sort_file_line(FactArgInfos, ArgModes, ModuleInfo,
			LineString, SortFileLine) },
		{ MaybeSortFileLine = yes(SortFileLine) }
	;
		{ Result = eof },
		{ MaybeSortFileLine = no }
	;
		{ Result = error(ErrorCode) },
		{ io__error_message(ErrorCode, ErrorMessage) },
		io__input_stream_name(FileName),
		io__write_strings([
			"Error reading file `",
			FileName,
			"': ",
			ErrorMessage,
			".\n" ]),
		io__set_exit_status(1),
		{ MaybeSortFileLine = no }
	).


	% Build and write out a top level hash table and all the lower level
	% tables connected to it.
:- pred build_hash_table(int, int, string, string, int, list(mode),
		module_info, list(fact_arg_info), bool, io__output_stream,
		sort_file_line, maybe(io__output_stream), bool, map(int, int),
		map(int, int), io__state, io__state).
:- mode build_hash_table(in, in, in, in, in, in, in, in, in, in, in, in, in,
		in, out, di, uo) is det.

build_hash_table(FactNum, InputArgNum, HashTableName, StructName, TableNum,
		ArgModes, ModuleInfo, Infos, IsPrimaryTable, OutputStream,
		FirstFact, MaybeDataStream, CreateFactMap, FactMap0, FactMap)
		-->
	build_hash_table_2(FactNum, InputArgNum, HashTableName, StructName,
		TableNum, ArgModes, ModuleInfo, Infos, IsPrimaryTable,
		OutputStream, yes(FirstFact), MaybeDataStream, CreateFactMap,
		FactMap0, FactMap, [], HashList),
	{ list__length(HashList, Len) },
	calculate_hash_table_size(Len, HashSize),
	{ hash_table_init(HashSize, HashTable0) },
	{ hash_table_from_list(HashList, HashSize, HashTable0, HashTable) },
	write_hash_table(HashTableName, TableNum, HashTable, OutputStream).

:- pred build_hash_table_2(int, int, string, string, int, list(mode),
		module_info, list(fact_arg_info), bool, io__output_stream,
		maybe(sort_file_line), maybe(io__output_stream), bool,
		map(int, int), map(int, int), list(hash_entry),
		list(hash_entry), io__state, io__state).
:- mode build_hash_table_2(in, in, in, in, in, in, in, in, in, in, in, in, in,
		in, out, in, out, di, uo) is det.

build_hash_table_2(_, _, _, _, _, _, _, _, _, _, no, _, _, FactMap, FactMap,
		HashList, HashList) --> [].
build_hash_table_2(FactNum, InputArgNum, HashTableName, StructName, TableNum0,
		ArgModes, ModuleInfo, Infos, IsPrimaryTable, OutputStream,
		yes(FirstFact), MaybeDataStream, CreateFactMap,
		FactMap0, FactMap, HashList0, HashList) -->
	top_level_collect_matching_facts(FirstFact, MatchingFacts,
		MaybeNextFact, Infos, ArgModes, ModuleInfo),
	{
		CreateFactMap = yes,
		update_fact_map(FactNum, MatchingFacts, FactMap0, FactMap1)
	;
		CreateFactMap = no,
		FactMap1 = FactMap0
	},
	(
		{ MaybeDataStream = yes(DataStream) },
		{ list__map(lambda([X::in, Y::out] is det, 
			X = sort_file_line(_, _, Y)), MatchingFacts,
			OutputData) },
		write_fact_table_data(FactNum, OutputData, StructName,
			DataStream)
	;
		{ MaybeDataStream = no }
	),
	do_build_hash_table(FactNum, InputArgNum, HashTableName,
		TableNum0, TableNum1, IsPrimaryTable, OutputStream,
		MatchingFacts, FactMap1, HashList0, HashList1),
	{ list__length(MatchingFacts, Len) },
	{ NextFactNum is FactNum + Len },
	build_hash_table_2(NextFactNum, InputArgNum, HashTableName, StructName,
		TableNum1, ArgModes, ModuleInfo, Infos, IsPrimaryTable,
		OutputStream, MaybeNextFact, MaybeDataStream, CreateFactMap,
		FactMap1, FactMap, HashList1, HashList).

	% Build a lower level hash table.  The main difference to
	% build_hash_table (above) is that ``sort file lines'' are read from 
	% a list rather than from the actual sort file.
:- pred build_hash_table_lower_levels(int, int, string, int, int,
		bool, io__output_stream, list(sort_file_line),
		map(int, int), io__state, io__state).
:- mode build_hash_table_lower_levels(in, in, in, in, out, in, in, in,
		in, di, uo) is det.

build_hash_table_lower_levels(FactNum, InputArgNum, HashTableName,
		TableNum0, TableNum, IsPrimaryTable, OutputStream,
		Facts, FactMap) -->
	build_hash_table_lower_levels_2(FactNum, InputArgNum, 
		HashTableName, TableNum0, TableNum, IsPrimaryTable,
		OutputStream, Facts, FactMap, [], HashList),
	{ list__length(HashList, Len) },
	calculate_hash_table_size(Len, HashSize),
	{ hash_table_init(HashSize, HashTable0) },
	{ hash_table_from_list(HashList, HashSize, HashTable0, HashTable) },
	write_hash_table(HashTableName, TableNum0, HashTable, OutputStream).


:- pred build_hash_table_lower_levels_2(int, int, string, int, int,
		bool, io__output_stream, list(sort_file_line),
		map(int, int), list(hash_entry), list(hash_entry),
		io__state, io__state).
:- mode build_hash_table_lower_levels_2(in, in, in, in, out, in, in, in,
		in, in, out, di, uo) is det.

build_hash_table_lower_levels_2(_, _, _, TableNum, TableNum, _, _, [],
		_, HashList, HashList) --> [].
build_hash_table_lower_levels_2(FactNum, InputArgNum, HashTableName,
		TableNum0, TableNum, IsPrimaryTable, OutputStream,
		[Fact | Facts0], FactMap, HashList0, HashList) -->
	{ lower_level_collect_matching_facts(Fact, Facts0, MatchingFacts,
		Facts1, InputArgNum) },
	do_build_hash_table(FactNum, InputArgNum, HashTableName,
		TableNum0, TableNum1, IsPrimaryTable, OutputStream,
		MatchingFacts, FactMap, HashList0, HashList1),
	{ list__length(MatchingFacts, Len) },
	{ NextFactNum is FactNum + Len },
	build_hash_table_lower_levels_2(NextFactNum, InputArgNum,
		HashTableName, TableNum1, TableNum, IsPrimaryTable,
		OutputStream, Facts1, FactMap, HashList1, HashList).


	% This is where most of the actual work is done in building up the
	% hash table.
:- pred do_build_hash_table(int, int, string, int, int, bool,
		io__output_stream, list(sort_file_line), map(int, int),
		list(hash_entry), list(hash_entry), io__state, io__state).
:- mode do_build_hash_table(in, in, in, in, out, in, in, in, in, in,
		out, di, uo) is det.

do_build_hash_table(FactNum, InputArgNum, HashTableName, TableNum0,
		TableNum, IsPrimaryTable, OutputStream, Facts, FactMap,
		HashList0, HashList) -->
	(
		{ Facts = [] },
		{ error("do_build_hash_table: no facts") }
	;
		{ Facts = [Fact | Facts1] },
		{ fact_get_arg_and_index(Fact, InputArgNum, Arg, Index) },
		{
			IsPrimaryTable = yes,
			HashIndex = FactNum
		;
			IsPrimaryTable = no,
			map__lookup(FactMap, Index, HashIndex)
		},
		(
			{ Facts1 = [] }
		->
			% If only one matching index, insert a pointer to the
			% fact table entry into the current hash table.
			{ HashList = [hash_entry(Arg, fact(HashIndex), -1) | 
				HashList0] },
			{ TableNum = TableNum0 }
		;
			% see if there are any more input arguments
			{ NextInputArgNum is InputArgNum + 1 },
			{ Fact = sort_file_line(InputArgs, _, _) },
			{ N is NextInputArgNum + 1 },
			{ list__drop(N, InputArgs, _) }
		->
			{ TableNum1 is TableNum0 + 1 },
			build_hash_table_lower_levels(FactNum, NextInputArgNum,
				HashTableName, TableNum1, TableNum,
				IsPrimaryTable, OutputStream, Facts, FactMap),
			{ HashList = [hash_entry(Arg, 
				hash_table(TableNum1, HashTableName), -1) |
				HashList0] }
		;
			{ IsPrimaryTable = no }
		->
			% insert all matching indexes into the hash table
			{ hash_list_insert_many(HashList0, Facts,
				IsPrimaryTable, FactMap, FactNum,
				InputArgNum, HashList) },
			{ TableNum = TableNum0 }
		;
			% insert only the first matching index into the hash
			% table
			{ HashList = [hash_entry(Arg, fact(HashIndex), -1) |
				HashList0] },
			{ TableNum = TableNum0 }
		)
	).


	% Read lines from the sort file that that have the same first input
	% argument as Fact.  Places these lines into MatchingFacts.  The
	% first fact in MatchingFacts is always Fact.  If an extra fact is
	% read in following the matching facts, it is placed in MaybeNextFact.
:- pred top_level_collect_matching_facts(sort_file_line, list(sort_file_line),
		maybe(sort_file_line), list(fact_arg_info), list(mode),
		module_info, io__state, io__state).
:- mode top_level_collect_matching_facts(in, out, out, in, in, in, di, uo)
		is det.

top_level_collect_matching_facts(Fact, MatchingFacts, MaybeNextFact, Infos,
		ArgModes, ModuleInfo) -->
	top_level_collect_matching_facts_2(Fact, [], MatchingFacts0,
		MaybeNextFact, Infos, ArgModes, ModuleInfo),
	{ list__reverse(MatchingFacts0, MatchingFacts1) },
	{ MatchingFacts = [Fact | MatchingFacts1] }.


:- pred top_level_collect_matching_facts_2(sort_file_line,
		list(sort_file_line), list(sort_file_line),
		maybe(sort_file_line), list(fact_arg_info), list(mode),
		module_info, io__state, io__state).
:- mode top_level_collect_matching_facts_2(in, in, out, out, in, in, in,
		di, uo) is det.

top_level_collect_matching_facts_2(Fact, MatchingFacts0, MatchingFacts,
		MaybeNextFact, Infos, ArgModes, ModuleInfo) -->
	read_sort_file_line(Infos, ArgModes, ModuleInfo, MaybeSortFileLine),
	(
		{ MaybeSortFileLine = yes(Fact1) },
		(
			{ Fact1 = sort_file_line([Arg1 | _], _, _) },
			{ Fact  = sort_file_line([Arg  | _], _, _) }
		->
			( { Arg = Arg1 } ->
				top_level_collect_matching_facts_2(Fact,
					[Fact1 | MatchingFacts0], 
					MatchingFacts, MaybeNextFact, Infos,
					ArgModes, ModuleInfo)
			;
				{ MatchingFacts = MatchingFacts0 },
				{ MaybeNextFact = yes(Fact1) }
			)
		;
			{ error(
			    "top_level_collect_matching_facts: no input args") }
		)
	;
		{ MaybeSortFileLine = no },
		{ MatchingFacts = MatchingFacts0 },
		{ MaybeNextFact = no }
	).


	% Same as above, but reads facts from a list instead of from the 
	% sort file.
:- pred lower_level_collect_matching_facts(sort_file_line,
		list(sort_file_line), list(sort_file_line),
		list(sort_file_line), int).
:- mode lower_level_collect_matching_facts(in, in, out, out, in) is det.

lower_level_collect_matching_facts(Fact, Facts0, Matching, Remaining,
		InputArgNum) :-
	lower_level_collect_matching_facts_2(Fact, Facts0, [], Matching0,
		Remaining, InputArgNum),
	list__reverse(Matching0, Matching1),
	Matching = [Fact | Matching1].

:- pred lower_level_collect_matching_facts_2(sort_file_line,
	list(sort_file_line), list(sort_file_line), list(sort_file_line),
	list(sort_file_line), int).
:- mode lower_level_collect_matching_facts_2(in, in, in, out, out, in) is det.

lower_level_collect_matching_facts_2(_, [], Matching, Matching, [], _).
lower_level_collect_matching_facts_2(Fact, [Fact0 | Facts0], Matching0,
		Matching, Remaining, InputArgNum) :-
	Fact0 = sort_file_line(InputArgs0, _, _),
	Fact  = sort_file_line(InputArgs,  _, _),
	(
		list__drop(InputArgNum, InputArgs0, [Arg0 | _]),
		list__drop(InputArgNum, InputArgs,  [Arg  | _])
	->
		( Arg = Arg0 ->
			lower_level_collect_matching_facts_2(Fact, Facts0,
				[Fact0 | Matching0], Matching, Remaining,
				InputArgNum)
		;
			Matching = Matching0,
			Remaining = [Fact0 | Facts0]
		)
	;
		error(
		    "lower_level_collect_matching_facts: not enough input args")
	).

:- pred update_fact_map(int, list(sort_file_line), map(int, int), 
		map(int, int)).
:- mode update_fact_map(in, in, in, out) is det.

update_fact_map(_, [], FactMap, FactMap).
update_fact_map(FactNum, [Fact | Facts], FactMap0, FactMap) :-
	Fact = sort_file_line(_, Index, _),
	map__set(FactMap0, Index, FactNum, FactMap1),
	NextFactNum is FactNum + 1,
	update_fact_map(NextFactNum, Facts, FactMap1, FactMap).

%---------------------------------------------------------------------------%

	% Break up a string into the components of a sort file line
:- pred split_sort_file_line(list(fact_arg_info), list(mode), module_info,
		string, sort_file_line) is det.
:- mode split_sort_file_line(in, in, in, in, out) is det.

split_sort_file_line(FactArgInfos, ArgModes, ModuleInfo, Line0, SortFileLine)
		:-
	(
		string__sub_string_search(Line0, "~", Pos0),
		string__split(Line0, Pos0, InputArgsString, Line1),
		string__first_char(Line1, _, Line2),
		string__sub_string_search(Line2, "~", Pos1),
		string__split(Line2, Pos1, IndexString, Line3),
		string__first_char(Line3, _, Line4),
		string__remove_suffix(Line4, "\n", OutputArgsString),
		string__to_int(IndexString, Index0)
	->
		split_key_to_arg_strings(InputArgsString, InputArgStrings),
		get_input_args_list(FactArgInfos, ArgModes, ModuleInfo,
			InputArgStrings, InputArgs),
		split_key_to_arg_strings(OutputArgsString, OutputArgStrings),
		( 
			% Only extract the output arguments if they have
			% actually been written to this sort file.
			OutputArgStrings = [_|_],
			get_output_args_list(FactArgInfos, OutputArgStrings,
				OutputArgs)
		; 
			OutputArgStrings = [],
			OutputArgs = []
		),
		SortFileLine = sort_file_line(InputArgs, Index0, OutputArgs)
	;
		error("fact_table.m: sort file format incorrect")
	).

	% Split up a string containing a sort file key into a list of strings
	% containing the key arguments.  Arguments in the key are separated
	% by `:'.
:- pred split_key_to_arg_strings(string::in, list(string)::out) is det.

split_key_to_arg_strings(Key0, ArgStrings) :-
	(
		Key0 = ""
	->
		ArgStrings = []
	;
		(
			string__sub_string_search(Key0, ":", Pos),
			string__split(Key0, Pos, ArgString, Key1),
			string__first_char(Key1, _, Key2)
		->
			split_key_to_arg_strings(Key2, ArgStrings0),
			ArgStrings = [ArgString | ArgStrings0]
		;
			error("split_key_to_arg_strings: sort file key format is incorrect")
		)
	).

:- pred get_input_args_list(list(fact_arg_info), list(mode), module_info,
		list(string), list(fact_arg)).
:- mode get_input_args_list(in, in, in, in, out) is det.

get_input_args_list([], [], _, _, []).
get_input_args_list([_|_], [], _, _, _) :-
	error("get_input_args_list: too many fact_arg_infos").
get_input_args_list([], [_|_], _, _, _) :-
	error("get_input_args_list: too many argmodes").
get_input_args_list([Info | Infos], [Mode | Modes], ModuleInfo, ArgStrings0,
		Args) :-
	( mode_is_fully_input(ModuleInfo, Mode) ->
		(
			ArgStrings0 = [ArgString | ArgStrings],
			Info = fact_arg_info(Type, _, _),
			convert_key_string_to_arg(ArgString, Type, Arg),
			get_input_args_list(Infos, Modes, ModuleInfo,
				ArgStrings, Args0),
			Args = [Arg | Args0]
		;
			ArgStrings0 = [],
			error("get_input_args_list: not enough ArgStrings")
		)
	;
		% This argument is not input so skip it and try the next one.
		get_input_args_list(Infos, Modes, ModuleInfo, ArgStrings0, Args)
	).

:- pred get_output_args_list(list(fact_arg_info), list(string),
		list(fact_arg)).
:- mode get_output_args_list(in, in, out) is det.

get_output_args_list([], _, []).
get_output_args_list([Info | Infos], ArgStrings0, Args) :-
	(
		Info = fact_arg_info(Type, _, yes)
	->
		% this is an output argument (for some mode of the predicate)
		(
			ArgStrings0 = [ArgString | ArgStrings],
			convert_key_string_to_arg(ArgString, Type, Arg),
			get_output_args_list(Infos, ArgStrings, Args0),
			Args = [Arg | Args0]
		;
			ArgStrings0 = [],
			error("get_output_args_list: not enough ArgStrings")
		)
	;
		% not an output argument
		get_output_args_list(Infos, ArgStrings0, Args)
	).


:- pred convert_key_string_to_arg(string, type, fact_arg).
:- mode convert_key_string_to_arg(in, in, out) is det.

convert_key_string_to_arg(ArgString, Type, Arg) :-
	(
		Type = term__functor(term__atom("int"), [], _)
	->
		(
			string__base_string_to_int(36, ArgString, I)
		->
			Arg = term__integer(I)
		;
			error("convert_key_string_to_arg: could not convert string to int")
		)
	;
		Type = term__functor(term__atom("string"), [], _)
	->
		string__to_char_list(ArgString, Cs0),
		remove_sort_file_escapes(Cs0, [], Cs1),
		list__reverse(Cs1, Cs),
		string__from_char_list(Cs, S),
		Arg = term__string(S)
	;
		Type = term__functor(term__atom("float"), [], _)
	->
		(
			string__to_float(ArgString, F)
		->
			Arg = term__float(F)
		;
			error("convert_key_string_to_arg: could not convert string to float")
		)
	;
		error("convert_key_string_to_arg: unsupported type")
	).

	% remove the escape characters put in the string by make_sort_file_key
:- pred remove_sort_file_escapes(list(char), list(char), list(char)).
:- mode remove_sort_file_escapes(in, in, out) is det.

remove_sort_file_escapes([], Cs, Cs).
remove_sort_file_escapes([C0 | Cs0], In, Out) :- 
	( C0 = ('\\') ->
		(
			Cs0 = [C1 | Cs1],
			( C1 = ('\\') ->
				C = ('\\')
			; C1 = ('c') ->
				C = (':')
			; C1 = ('t') ->
				C = ('~')
			; C1 = ('n') ->
				C = ('\n')
			;
				error("remove_sort_file_escapes: something went wrong")
			),
			remove_sort_file_escapes(Cs1, [C | In], Out)
		;
			Cs0 = [],
			error("remove_sort_file_escapes: something went wrong")
		)
	;
		remove_sort_file_escapes(Cs0, [C0 | In], Out)
	).


:- pred fact_get_arg_and_index(sort_file_line, int, fact_arg, int).
:- mode fact_get_arg_and_index(in, in, out, out) is det.

fact_get_arg_and_index(Fact, InputArgNum, Arg, Index) :-
	Fact = sort_file_line(InputArgs, Index, _),
	(
		list__drop(InputArgNum, InputArgs, [Arg0 | _])
	->
		Arg = Arg0
	;
		error("fact_get_arg_and_index: not enough input args")
	).


%---------------------------------------------------------------------------%

	% Select a prime number > NumEntries * 100 / PercentFull
	% The prime number is selected from a list of primes each of which is
	% close to a power of 2 between 2^1 and 2^31.
:- pred calculate_hash_table_size(int, int, io__state, io__state).
:- mode calculate_hash_table_size(in, out, di, uo) is det.

calculate_hash_table_size(NumEntries, HashTableSize) -->
	globals__io_lookup_int_option(fact_table_hash_percent_full,
		PercentFull),
	{ Primes = [ 2, 3, 5, 11, 17, 37, 67, 131, 257, 521, 1031, 2053,
		4099, 8209, 16411, 32771, 65537, 131101, 262147,
		524309, 1048627, 2097257, 4194493, 8388949, 16777903,
		33555799, 67108879, 134217757, 268435459, 536870923,
		1073741827, 2147483647 ] },
	{ N is (NumEntries * 100) // PercentFull },
	{ calculate_hash_table_size_2(N, Primes, HashTableSize) }.

:- pred calculate_hash_table_size_2(int, list(int), int).
:- mode calculate_hash_table_size_2(in, in, out) is det.

calculate_hash_table_size_2(_, [], _) :- 
	error("hash table too large (max size 2147483647)").
calculate_hash_table_size_2(N, [P | Ps], H) :-
	(
		P > N
	->
		H = P
	;
		calculate_hash_table_size_2(N, Ps, H)
	).

	% Insert an entry in a hash table.
	% If a collision occurrs, find an empty hash slot to place the data in
	% and put a pointer to the new slot in the Next field of the old one.
	% This technique is called ``open-addressing''.
:- pred hash_table_insert(hash_table, hash_entry, int, hash_table).
:- mode hash_table_insert(in, in, in, out) is det.

hash_table_insert(HashTable0, Entry, HashSize, HashTable) :-
	Entry = hash_entry(Key, Index, _),
	fact_table_hash(HashSize, Key, HashVal),
	(
		hash_table_search(HashTable0, HashVal, _)
	->
		hash_table_insert_2(HashTable0, HashVal, _, Index, Key, 
			HashTable)
	;
		hash_table_set(HashTable0, HashVal, hash_entry(Key, Index, -1),
			HashTable)
	).

:- pred hash_table_insert_2(hash_table, int, int, hash_index, fact_arg, 
	hash_table).
:- mode hash_table_insert_2(in, in, out, in, in, out) is det.

hash_table_insert_2(HashTable0, HashVal, FreeVal, Index0, Key0, HashTable) :-
	(
		hash_table_search(HashTable0, HashVal, 
			hash_entry(Key1, Index1, Next))
	->
		(
			Next = -1
		->
			get_free_hash_slot(HashTable0, HashVal, FreeVal),
			hash_table_set(HashTable0, FreeVal, 
				hash_entry(Key0, Index0, -1), HashTable1),
			hash_table_set(HashTable1, HashVal, 
				hash_entry(Key1, Index1, FreeVal), HashTable)
		;
			hash_table_insert_2(HashTable0, Next, FreeVal, Index0, 
				Key0, HashTable)
		)
	;
		% shouldn't ever get here
		error("hash_table_insert_2: hash table entry empty")
	).

	% Probe through the hash table to find a free slot.
	% This will eventually terminate because the hash table size is selected
	% to be larger than the number of entries that need to go in it.
:- pred get_free_hash_slot(hash_table, int, int).
:- mode get_free_hash_slot(in, in, out) is det.

get_free_hash_slot(HashTable, Start, Free) :-
	HashTable = hash_table(Size, _),
	Max is Size - 1,
	get_free_hash_slot_2(HashTable, Start, Max, Free).

:- pred get_free_hash_slot_2(hash_table, int, int, int).
:- mode get_free_hash_slot_2(in, in, in, out) is det.

get_free_hash_slot_2(HashTable, Start, Max, Free) :-
	Next is (Start + 1) mod Max,
	(
		hash_table_search(HashTable, Next, _)
	->
		get_free_hash_slot_2(HashTable, Next, Max, Free)
	;
		Free = Next
	).

	% Hash computation predicate.
	% Note:  if you change this predicate, you will also need to change
	% the C code that is output to compute the hash value at runtime.
	% This C code is generated in `generate_hash_code'.
:- pred fact_table_hash(int::in, fact_arg::in, int::out) is det.

fact_table_hash(HashSize, Key, HashVal) :-
	(
		Key = term__string(String)
	->
		string__to_char_list(String, Cs),
		list__map(lambda([C::in, I::out] is det, char__to_int(C, I)),
			Cs, Ns)
	;
		Key = term__integer(Int)
	->
		int__abs(Int, N),
		Ns = [N]
	;
		Key = term__float(Float)
	->
		% XXX This method of hashing floats may not work cross-compiling
		% between architectures that have different floating-point
		% representations.
		int__abs(float__hash(Float), N),
		Ns = [N]
	;
		error("fact_table_hash: unsupported type in key")
	),
	fact_table_hash_2(HashSize, Ns, 0, HashVal).

:- pred fact_table_hash_2(int::in, list(int)::in, int::in, int::out) is det.

fact_table_hash_2(_, [], HashVal, HashVal).
fact_table_hash_2(HashSize, [N | Ns], HashVal0, HashVal) :-
	HashVal1 is (N + 31 * HashVal0) mod HashSize,
	fact_table_hash_2(HashSize, Ns, HashVal1, HashVal).

:- pred hash_list_insert_many(list(hash_entry), list(sort_file_line), bool,
		map(int, int), int, int, list(hash_entry)).
:- mode hash_list_insert_many(in, in, in, in, in, in, out) is det.

hash_list_insert_many(HashList, [], _, _, _, _, HashList).
hash_list_insert_many(HashList0, [Fact | Facts], IsPrimaryTable, FactMap,
		FactNum, InputArgNum, HashList) :-
	fact_get_arg_and_index(Fact, InputArgNum, Arg, Index),
	( 
		IsPrimaryTable = yes,
		HashIndex = FactNum
	; 
		IsPrimaryTable = no,
		map__lookup(FactMap, Index, HashIndex)
	),
	hash_list_insert_many([hash_entry(Arg,fact(HashIndex),-1) | HashList0],
		Facts, IsPrimaryTable, FactMap, FactNum, InputArgNum, HashList).

:- pred hash_table_init(int::in, hash_table::out) is det.

hash_table_init(Size, HashTable) :-
	map__init(Map),
	HashTable = hash_table(Size, Map).

:- pred hash_table_from_list(list(hash_entry), int, hash_table, hash_table).
:- mode hash_table_from_list(in, in, in, out) is det.

hash_table_from_list([], _, HashTable, HashTable).
hash_table_from_list([Entry | Entrys], HashSize, HashTable0, HashTable) :-
	hash_table_insert(HashTable0, Entry, HashSize, HashTable1),
	hash_table_from_list(Entrys, HashSize, HashTable1, HashTable).


:- pred hash_table_search(hash_table, int, hash_entry).
:- mode hash_table_search(in, in, out) is semidet.

hash_table_search(HashTable, Index, Value) :-
	HashTable = hash_table(_, Map),
	map__search(Map, Index, Value).

:- pred hash_table_set(hash_table, int, hash_entry, hash_table).
:- mode hash_table_set(in, in, in, out) is det.

hash_table_set(HashTable0, Index, Value, HashTable) :-
	HashTable0 = hash_table(Size, Map0),
	map__set(Map0, Index, Value, Map),
	HashTable = hash_table(Size, Map).

%--------------------------------------------------------------------------%

	% write out the C code for a hash table
:- pred write_hash_table(string, int, hash_table, io__output_stream,
		io__state, io__state).
:- mode write_hash_table(in, in, in, in, di, uo) is det.

write_hash_table(BaseName, TableNum, HashTable, OutputStream) -->
	{ get_hash_table_type(HashTable, TableType) },
	{ string__format("struct MR_fact_table_hash_entry_%c %s%d_data[]",
		[c(TableType), s(BaseName), i(TableNum)], HashTableDataName) },
	io__set_output_stream(OutputStream, OldOutputStream),
	io__write_strings([HashTableDataName, " = {\n"]),
	{ HashTable = hash_table(Size, _) },
	{ MaxIndex is Size - 1 },
	write_hash_table_2(HashTable, 0, MaxIndex),
	io__write_string("};\n\n"),
	io__format("

struct MR_fact_table_hash_table_%c %s%d = {
	%d,
	%s%d_data
};
",
		[c(TableType), s(BaseName), i(TableNum), i(Size), 
		s(BaseName), i(TableNum)]),
	io__set_output_stream(OldOutputStream, _).

:- pred write_hash_table_2(hash_table, int, int, io__state, io__state).
:- mode write_hash_table_2(in, in, in, di, uo) is det.

write_hash_table_2(HashTable, CurrIndex, MaxIndex) -->
	(
		{ CurrIndex > MaxIndex }
	->
		[]
	;
		io__write_string("\t{ "),
		(
			{ hash_table_search(HashTable, CurrIndex, 
				hash_entry(Key, Index, Next)) }
		->
			(
				{ Key = term__string(String) }
			->
				io__write_string(""""),
				output_c_quoted_string(String),
				io__write_string("""")
			;
				{ Key = term__integer(Int) }
			->
				io__write_int(Int)
			;
				{ Key = term__float(Float) }
			->
				io__write_float(Float)
			;
				{ error("write_hash_table: unsupported type") }
			),
			(
				{ Index = fact(I) },
				io__format(
				", MR_FACT_TABLE_MAKE_TAGGED_INDEX(%d, 1), ",
				    [i(I)])
			;
				{ Index = hash_table(I, H) },
				io__format(
			", MR_FACT_TABLE_MAKE_TAGGED_POINTER(&%s%d, 2), ",
				    [s(H), i(I)])
			),
			io__write_int(Next)
		;
			io__write_string(
			"0, MR_FACT_TABLE_MAKE_TAGGED_POINTER(NULL, 0), -1 ")
		),
		io__write_string("},\n"),
		{ NextIndex is CurrIndex + 1 },
		write_hash_table_2(HashTable, NextIndex, MaxIndex)
	).

	% Return 's' for string, 'i' for int, 'f' for float, 'a' for atom.
	% Don't call this with an empty hash table.
:- pred get_hash_table_type(hash_table::in, char::out) is det.

get_hash_table_type(HashTable, TableType) :-
	HashTable = hash_table(_Size, Map),
	(
		map__is_empty(Map)
	->
		error("get_has_table_type: empty hash table")
	;
		get_hash_table_type_2(Map, 0, TableType)
	).

:- pred get_hash_table_type_2(map(int, hash_entry)::in, int::in, char::out)
		is det.

get_hash_table_type_2(Map, Index, TableType) :-
	(
		map__search(Map, Index, Entry)
	->
		Entry = hash_entry(Key, _, _),
		(
			Key = term__string(_)
		->
			TableType = 's'
		;
			Key = term__integer(_)
		->
			TableType = 'i'
		;
			Key = term__float(_)
		->
			TableType = 'f'
		;
			Key = term__atom(_)
		->
			TableType = 'a'
		;
			error("get_hash_table_type: invalid term")
		)
	;
		NextIndex is Index + 1,
		get_hash_table_type_2(Map, NextIndex, TableType)
	).

%---------------------------------------------------------------------------%

	% write out the array of pointers to the fact table arrays.
:- pred write_fact_table_pointer_array(int, string, io__output_stream, string,
		io__state, io__state).
:- mode write_fact_table_pointer_array(in, in, in, out, di, uo) is det.

write_fact_table_pointer_array(NumFacts, StructName, OutputStream, 
		C_HeaderCode) -->
	{ string__append_list(
		["const struct ", StructName, "_struct *", StructName, "[]"], 
		PointerArrayName) },
	{ string__append_list(["extern ", PointerArrayName, ";\n"], 
		C_HeaderCode) },
	io__write_strings(OutputStream, [PointerArrayName, " = {\n"]),
	write_fact_table_pointer_array_2(0, NumFacts, StructName, OutputStream),
	io__write_string(OutputStream, "};\n").

:- pred write_fact_table_pointer_array_2(int, int, string, io__output_stream,
		io__state, io__state).
:- mode write_fact_table_pointer_array_2(in, in, in, in, di, uo) is det.

write_fact_table_pointer_array_2(CurrFact, NumFacts, StructName, OutputStream)
	-->
	(
		{ CurrFact >= NumFacts }
	->
		[]
	;
		io__format(OutputStream, "\t%s%d,\n",
			[s(StructName), i(CurrFact)]),
		fact_table_size(FactTableSize),
		{ NextFact is CurrFact + FactTableSize },
		write_fact_table_pointer_array_2(NextFact, NumFacts, StructName,
			OutputStream)
	).


:- pred write_fact_table_numfacts(sym_name, int, io__output_stream,
		string, io__state, io__state).
:- mode write_fact_table_numfacts(in, in, in, out, di, uo) is det.

write_fact_table_numfacts(PredName, NumFacts, OutputStream, C_HeaderCode) --> 
	io__set_output_stream(OutputStream, OldOutputStream),

	% Write out the size of the fact table.
	{ make_fact_table_identifier(PredName, Identifier) },
	io__write_strings([
		"const MR_Integer mercury__",
		Identifier,
		"_fact_table_num_facts = "]),
	io__write_int(NumFacts),
	io__write_string(";\n\n"),

	{ string__append_list(
		[
			"extern const MR_Integer mercury__",
			Identifier,
			"_fact_table_num_facts;\n"
		],
		C_HeaderCode) },

	io__set_output_stream(OldOutputStream, _).

%---------------------------------------------------------------------------%
:- pred make_fact_table_identifier(sym_name::in, string::out) is det.

make_fact_table_identifier(SymName, Identifier) :-
	llds_out__sym_name_mangle(SymName, Identifier).

%---------------------------------------------------------------------------%

	% Delete a file.  Report an error message if something goes wrong.
:- pred delete_temporary_file(string::in, io__state::di, io__state::uo) is det.

delete_temporary_file(FileName) -->
	io__remove_file(FileName, Result),
	(
		{ Result = ok }
	;
		{ Result = error(ErrorCode) },
		{ io__error_message(ErrorCode, ErrorMessage) },
		io__progname_base("mercury_compile", ProgName),
		io__write_strings([
			ProgName,
			": error deleting file `",
			FileName,
			"':\n  ",
			ErrorMessage,
			".\n"]),
		io__set_exit_status(1)
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

fact_table_generate_c_code(PredName, PragmaVars, ProcID, PrimaryProcID, 
		ProcInfo, ArgTypes, ModuleInfo, ProcCode, ExtraCode) -->
	fact_table_size(FactTableSize),
	{ proc_info_argmodes(ProcInfo, ArgModes) },
	{ proc_info_interface_determinism(ProcInfo, Determinism) },
	{ fact_table_mode_type(ArgModes, ModuleInfo, ModeType) },
	{ make_fact_table_identifier(PredName, Identifier) },
	{
		ModeType = all_out,
		Determinism = multidet
	->
		generate_multidet_code(Identifier, PragmaVars, ProcID,
			ArgTypes, ModuleInfo, FactTableSize,
			ProcCode, ExtraCode)
	;
		ModeType = all_out,
		Determinism = cc_multidet
	->
		generate_cc_multi_code(Identifier, PragmaVars, ProcCode),
		ExtraCode = ""
	;
		ModeType = all_in,
		Determinism = semidet
	->
		generate_all_in_code(Identifier, PragmaVars, ProcID,
			ArgTypes, ModuleInfo, FactTableSize, ProcCode),
		ExtraCode = ""
	;
		ModeType = in_out,
		( Determinism = semidet ; Determinism = cc_nondet )
	->
		generate_semidet_in_out_code(Identifier, PragmaVars, ProcID,
			ArgTypes, ModuleInfo, FactTableSize, ProcCode),
		ExtraCode = ""
	;
		ModeType = in_out,
		Determinism = nondet,
		ProcID = PrimaryProcID
	->
		generate_primary_nondet_code(Identifier, PragmaVars,
			ProcID, ArgTypes, ModuleInfo, FactTableSize,
			ProcCode, ExtraCode)
	;
		ModeType = in_out,
		Determinism = nondet,
		ProcID \= PrimaryProcID
	->
		generate_secondary_nondet_code(Identifier, PragmaVars,
			ProcID, ArgTypes, ModuleInfo, FactTableSize,
			ProcCode, ExtraCode)
	;
		% There is a determinism error in this procedure which will be 
		% reported later on when the inferred determinism is compared
		% to the declared determinism.  So all we need to do here is
		% return some C code that does nothing.

		% List the variables in the C code to stop the compiler giving
		% a warning about them not being there.
		pragma_vars_to_names_string(PragmaVars, NamesString),
		string__format("/* %s */", [s(NamesString)], ProcCode),
		ExtraCode = ""
	}.


%---------------------------------------------------------------------------%

	% XXX this should be changed to use the new model_non pragma c_code
:- pred generate_multidet_code(string, list(pragma_var), proc_id, 
		list(type), module_info, int, string, string).
:- mode generate_multidet_code(in, in, in, in, in, in, out, out) is det.

generate_multidet_code(PredName, PragmaVars, ProcID, ArgTypes,
	    ModuleInfo, FactTableSize, ProcCode, ExtraCode) :-
	generate_nondet_proc_code(PragmaVars, PredName, ProcID, ExtraCodeLabel,
		ProcCode),

	ExtraCodeTemplate = "

MR_define_extern_entry(%s);
MR_declare_label(%s_i1);

MR_BEGIN_MODULE(%s_module)
	MR_init_entry(%s);
	MR_init_label(%s_i1);
MR_BEGIN_CODE
MR_define_entry(%s);
	MR_mkframe(""%s/%d"", 1, MR_LABEL(%s_i1));
	MR_framevar(1) = (MR_Integer) 0;
	MR_GOTO(MR_LABEL(%s_i1));
MR_define_label(%s_i1);
	if (MR_framevar(1) >= %s) MR_fail();
	{
		/* declare argument vars */
%s
		MR_Word ind = MR_framevar(1), tmp;
		/* lookup fact table */
%s
		/* save output args to registers */
%s
	}
	MR_framevar(1)++;
	MR_succeed();
MR_END_MODULE

extern MR_ModuleFunc %s_module;

/*
INIT mercury_sys_init_%s_module
*/
void mercury_sys_init_%s_module(void);
void mercury_sys_init_%s_module(void) {
	%s_module();
}

	",

	string__append_list(["mercury__", PredName, "_fact_table_num_facts"],
		NumFactsVar),
	list__length(PragmaVars, Arity), 
	generate_argument_vars_code(PragmaVars, ArgTypes,
		ModuleInfo, ArgDeclCode, _InputCode, OutputCode, _, _, _),
	generate_fact_lookup_code(PredName, PragmaVars, ArgTypes, ModuleInfo, 1,
		FactTableSize, FactLookupCode),

	string__format(ExtraCodeTemplate, [
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(PredName),
			i(Arity),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(NumFactsVar),
			s(ArgDeclCode),
			s(FactLookupCode),
			s(OutputCode),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel)
		],
		ExtraCode).

:- pred generate_nondet_proc_code(list(pragma_var)::in, string::in, proc_id::in,
		string::out, string::out) is det.

generate_nondet_proc_code(PragmaVars, PredName, ProcID, ExtraCodeLabel,
		ProcCode) :-
	ProcCodeTemplate =  "

	/*
	** Mention arguments %s to stop the compiler giving a warning.
	**
	** Pop off the nondet stack frame that the pragma c_code generates
	** then jump to the code where the work is actually done.
	*/

	MR_maxfr = MR_prevfr_slot(MR_curfr);
	MR_curfr = MR_succfr_slot(MR_curfr);
	{
		MR_declare_entry(%s);
		MR_GOTO(MR_ENTRY(%s));
	}
	",

	list__length(PragmaVars, Arity),
	proc_id_to_int(ProcID, ProcInt),
	string__format("mercury__%s_%d_%d_xx", 
		[s(PredName), i(Arity), i(ProcInt)], ExtraCodeLabel),
	pragma_vars_to_names_string(PragmaVars, NamesString),
	string__format(ProcCodeTemplate, [s(NamesString), s(ExtraCodeLabel),
		s(ExtraCodeLabel)], ProcCode).


	% pragma_vars_to_names_string(PragmaVars, NamesString),
	% create a string containing the names of the pragma vars separated by
	% a space.
:- pred pragma_vars_to_names_string(list(pragma_var), string).
:- mode pragma_vars_to_names_string(in, out) is det.

pragma_vars_to_names_string([], "").
pragma_vars_to_names_string([pragma_var(_, Name, _) | PVars], NamesString) :-
	pragma_vars_to_names_string(PVars, NamesString0),
	string__append_list([Name, ", ", NamesString0], NamesString).

%---------------------------------------------------------------------------%

	% for cc_multi output mode, just return the first fact in the table
:- pred generate_cc_multi_code(string, list(pragma_var), string).
:- mode generate_cc_multi_code(in, in, out) is det.

generate_cc_multi_code(PredName, PragmaVars, ProcCode) :-
	string__append_list(["mercury__", PredName, "_fact_table"], StructName),
	generate_cc_multi_code_2(PragmaVars, StructName, 1, "", ProcCode).

:- pred generate_cc_multi_code_2(list(pragma_var), string, int, string,
		string).
:- mode generate_cc_multi_code_2(in, in, in, in, out) is det.

generate_cc_multi_code_2([], _, _, ProcCode, ProcCode).
generate_cc_multi_code_2([pragma_var(_, VarName, _)|PragmaVars], StructName,
		ArgNum, ProcCode0, ProcCode) :-
	string__format("\t\t%s = %s[0][0].V_%d;\n", [s(VarName), s(StructName),
		i(ArgNum)], ProcCode1),
	string__append(ProcCode1, ProcCode0, ProcCode2),
	NextArgNum is ArgNum + 1,
	generate_cc_multi_code_2(PragmaVars, StructName, NextArgNum, ProcCode2,
		ProcCode).

%---------------------------------------------------------------------------%

	% generate semidet code for all_in mode
:- pred generate_all_in_code(string, list(pragma_var), proc_id, list(type),
		module_info, int, string).
:- mode generate_all_in_code(in, in, in, in, in, in, out) is det.

generate_all_in_code(PredName, PragmaVars, ProcID, ArgTypes, ModuleInfo,
		FactTableSize, ProcCode) :-
	generate_decl_code(PredName, ProcID, DeclCode),

	proc_id_to_int(ProcID, ProcInt),
	string__format("%s_%d", [s(PredName), i(ProcInt)], LabelName), 
	generate_hash_code(PragmaVars, ArgTypes, ModuleInfo, LabelName, 0,
		PredName, 1, FactTableSize, HashCode),

	SuccessCodeTemplate = "
		success_code_%s:
			SUCCESS_INDICATOR = TRUE;
			goto skip_%s;
		failure_code_%s:
			SUCCESS_INDICATOR = FALSE;
		skip_%s:
			;
	",
	string__format(SuccessCodeTemplate, [s(LabelName), s(LabelName),
		s(LabelName), s(LabelName)], SuccessCode),

	string__append_list([
		"\t{\n", DeclCode, HashCode, SuccessCode, "\t}\n"], ProcCode).

%---------------------------------------------------------------------------%

	% Generate code for semidet and cc_nondet in_out modes.
	% Lookup key in hash table and if found return first match.
	% If not found, fail.
:- pred generate_semidet_in_out_code(string, list(pragma_var), proc_id,
		list(type), module_info, int, string).
:- mode generate_semidet_in_out_code(in, in, in, in, in, in, out) is det.

generate_semidet_in_out_code(PredName, PragmaVars, ProcID, ArgTypes,
		ModuleInfo, FactTableSize, ProcCode):-
	generate_decl_code(PredName, ProcID, DeclCode),

	proc_id_to_int(ProcID, ProcInt),
	string__format("%s_%d", [s(PredName), i(ProcInt)], LabelName), 
	generate_hash_code(PragmaVars, ArgTypes, ModuleInfo, LabelName, 0,
		PredName, 1, FactTableSize, HashCode),

	SuccessCodeTemplate = "
		success_code_%s:
			SUCCESS_INDICATOR = TRUE;
	",
	string__format(SuccessCodeTemplate, [s(LabelName)], SuccessCode),

	generate_fact_lookup_code(PredName, PragmaVars, ArgTypes, ModuleInfo, 1,
		FactTableSize, FactLookupCode),

	FailCodeTemplate = "
			goto skip_%s;
		failure_code_%s:
			SUCCESS_INDICATOR = FALSE;
		skip_%s:
			;
	",
	string__format(FailCodeTemplate, [s(LabelName), s(LabelName),
		s(LabelName)], FailCode),

	string__append_list(["\t{\n", DeclCode, HashCode, SuccessCode,
		FactLookupCode, FailCode, "\t}\n"], ProcCode).

%---------------------------------------------------------------------------%
	% Some code generation procedures used by various modes.

:- pred generate_decl_code(string::in, proc_id::in, string::out) is det.

generate_decl_code(Name, ProcID, DeclCode) :-
	DeclCodeTemplate = "
			MR_Integer hashval, hashsize;
			MR_Word ind;
			void *current_table;
			char keytype = '\\0';
			MR_Word current_key, tmp;

			/*
			** Initialise current_table to the top level hash table
			** for this ProcID.
			*/
			current_table = 
				&mercury__%s_fact_table_hash_table_%d_0;

	",
	proc_id_to_int(ProcID, ProcInt),
	string__format(DeclCodeTemplate, [s(Name), i(ProcInt)], DeclCode).

	% generate code to calculate hash values and lookup the hash tables
:- pred generate_hash_code(list(pragma_var), list(type), module_info, string,
		int, string, int, int, string).
:- mode generate_hash_code(in, in, in, in, in, in, in, in, out) is det.

generate_hash_code([], [], _, _, _, _, _, _, "").
generate_hash_code([], [_|_], _, _, _, _, _, _, _) :- 
	error("generate_hash_code").
generate_hash_code([_|_], [], _, _, _, _, _, _, _) :- 
	error("generate_hash_code").
generate_hash_code([pragma_var(_, Name, Mode)|PragmaVars], [Type | Types],
		ModuleInfo, LabelName, LabelNum, PredName, ArgNum,
		FactTableSize, C_Code) :-
	NextArgNum is ArgNum + 1,
	( mode_is_fully_input(ModuleInfo, Mode) ->
		(
			Type = term__functor(term__atom("int"), [], _)
		->
			generate_hash_int_code(Name, LabelName, LabelNum,
				PredName, PragmaVars, Types, ModuleInfo,
				NextArgNum, FactTableSize, C_Code0)
		;
			Type = term__functor(term__atom("float"), [], _)
		->
			generate_hash_float_code(Name, LabelName, LabelNum,
				PredName, PragmaVars, Types, ModuleInfo,
				NextArgNum, FactTableSize, C_Code0)
		;
			Type = term__functor(term__atom("string"), [], _)
		->
			generate_hash_string_code(Name, LabelName, LabelNum,
				PredName, PragmaVars, Types, ModuleInfo,
				NextArgNum, FactTableSize, C_Code0)
		;
			error("generate_hash_code: unsupported type")
		),
		NextLabelNum is LabelNum + 1,
		generate_hash_code(PragmaVars, Types, ModuleInfo, LabelName,
			NextLabelNum, PredName, NextArgNum, FactTableSize,
			C_Code1),
		string__append(C_Code0, C_Code1, C_Code)
	;
		% skip non-input arguments
		generate_hash_code(PragmaVars, Types, ModuleInfo, LabelName,
			LabelNum, PredName, NextArgNum, FactTableSize,
			C_Code)
	).

:- pred generate_hash_int_code(string::in, string::in, int::in, string::in,
		list(pragma_var)::in, list(type)::in, module_info::in,
		int::in, int::in, string::out)
		is det.

generate_hash_int_code(Name, LabelName, LabelNum, PredName, PragmaVars,
		Types, ModuleInfo, ArgNum, FactTableSize, C_Code) :-
	generate_hash_lookup_code(Name, LabelName, LabelNum, "%s == %s", 'i',
		yes, PredName, PragmaVars, Types, ModuleInfo, ArgNum,
		FactTableSize, HashLookupCode),
	C_Code_Template = "

		/* calculate hash value for an integer */

		hashsize = ((struct MR_fact_table_hash_table_i *)current_table)
			->size;

		hashval = (%s >= 0 ? %s : -%s) %% hashsize;

		current_key = %s;

		/* lookup the hash table */
		%s

	",
	string__format(C_Code_Template, [s(Name), s(Name), s(Name), s(Name),
		s(HashLookupCode)], C_Code).

:- pred generate_hash_float_code(string::in, string::in, int::in, string::in,
		list(pragma_var)::in, list(type)::in, module_info::in,
		int::in, int::in, string::out)
		is det.

generate_hash_float_code(Name, LabelName, LabelNum, PredName, PragmaVars,
		Types, ModuleInfo, ArgNum, FactTableSize, C_Code) :-
	generate_hash_lookup_code(Name, LabelName, LabelNum, "%s == %s", 'f',
		yes, PredName, PragmaVars, Types, ModuleInfo, ArgNum,
		FactTableSize, HashLookupCode),
	C_Code_Template = "

		/* calculate hash value for a float */

		hashsize = ((struct MR_fact_table_hash_table_f *)current_table)
			->size;

		hashval = MR_hash_float(%s);
		hashval = (hashval >= 0 ? hashval : -hashval) %% hashsize;

		current_key = MR_float_to_word(%s);

		/* lookup the hash table */
		%s

	",
	string__format(C_Code_Template, [s(Name), s(Name), s(HashLookupCode)],
		C_Code).

:- pred generate_hash_string_code(string::in, string::in, int::in, string::in,
		list(pragma_var)::in, list(type)::in, module_info::in,
		int::in, int::in, string::out)
		is det.

generate_hash_string_code(Name, LabelName, LabelNum, PredName, PragmaVars,
		Types, ModuleInfo, ArgNum, FactTableSize, C_Code) :-
	generate_hash_lookup_code(Name, LabelName, LabelNum, 
		"strcmp(%s, %s) == 0", 's', yes, PredName, PragmaVars,
		Types, ModuleInfo, ArgNum, FactTableSize, HashLookupCode),
	C_Code_Template = "

		hashsize = ((struct MR_fact_table_hash_table_s *)current_table)
			->size;

		/* calculate hash value for a string */
		{
			char *p;
			hashval = 0;
			for (p = %s ; *p != '\\0' ; p++)
				hashval = (*p + 31 * hashval) %% hashsize;
		}

		current_key = (MR_Word) %s;

		/* lookup the hash table */
		%s

	",
	string__format(C_Code_Template, [s(Name), s(Name), s(HashLookupCode)],
		C_Code).


	% Generate code to lookup the key in the hash table.
	% KeyType should be 's', 'i' or 'f' for string, int or float, 
	% respectively.  CompareTemplate should be a template for testing for
	% equality for the type given, e.g. "%s == %s" for ints,
	% "strcmp(%s, %s) == 0" for strings.
:- pred generate_hash_lookup_code(string::in, string::in, int::in, string::in,
		char::in, bool::in, string::in, list(pragma_var)::in,
		list(type)::in, module_info::in, int::in, int::in, string::out)
		is det.

generate_hash_lookup_code(VarName, LabelName, LabelNum, CompareTemplate,
		KeyType, CheckKeys, PredName, PragmaVars, Types,
		ModuleInfo, ArgNum, FactTableSize, HashLookupCode) :-
	string__format(
	"((struct MR_fact_table_hash_table_%c *)current_table)->table[hashval]",
		[c(KeyType)], HashTableEntry),
	string__append(HashTableEntry, ".key", HashTableKey),
	string__format(CompareTemplate, [s(HashTableKey), s(VarName)],
		CompareString),

	HashLookupCodeTemplate = "

		do {
			if (MR_FACT_TABLE_HASH_ENTRY_TYPE(%s) != 0 && %s)
			{
				ind = (MR_Word) %s.index;
				goto found_%s_%d;
			}
		} while ((hashval = %s.next) != -1);

		/* key not found */
		goto failure_code_%s;

	found_%s_%d:

		if (MR_FACT_TABLE_HASH_ENTRY_TYPE(%s) == 1) {
			ind = MR_FACT_TABLE_HASH_INDEX(ind);

			/* check that any remaining input arguments match */
			%s
			keytype = '%c';
			hashval = %s.next;
			goto success_code_%s;
		}

		current_table = (void *) MR_FACT_TABLE_HASH_POINTER(ind);

	",
	( CheckKeys = yes ->
		string__append_list(["mercury__", PredName, "_fact_table"],
			FactTableName),
		generate_test_condition_code(FactTableName, PragmaVars, Types,
			ModuleInfo, ArgNum, yes, FactTableSize, CondCode),
		( CondCode \= "" ->
			TestCodeTemplate = 
				"if (%s\t\t\t) goto failure_code_%s;\n",
			string__format(TestCodeTemplate, 
				[s(CondCode), s(LabelName)], TestCode)
		;
			TestCode = ""
		)
	;
		TestCode = ""
	),
	
	string__format(HashLookupCodeTemplate, [s(HashTableEntry),
		s(CompareString), s(HashTableEntry), s(LabelName), i(LabelNum),
		s(HashTableEntry), s(LabelName), s(LabelName), i(LabelNum),
		s(HashTableEntry), s(TestCode), c(KeyType),
		s(HashTableEntry), s(LabelName)],
		HashLookupCode).



	% Generate code to lookup the fact table with a given index
:- pred generate_fact_lookup_code(string, list(pragma_var), list(type),
		module_info, int, int, string).
:- mode generate_fact_lookup_code(in, in, in, in, in, in, out) is det.

generate_fact_lookup_code(_, [], [], _, _, _, "").
generate_fact_lookup_code(_, [_|_], [], _, _, _, _) :-
	error("generate_fact_lookup_code: too many pragma vars").
generate_fact_lookup_code(_, [], [_|_], _, _, _, _) :-
	error("generate_fact_lookup_code: too many types").
generate_fact_lookup_code(PredName, [pragma_var(_, VarName, Mode)|PragmaVars],
		[Type | Types], ModuleInfo, ArgNum, FactTableSize, C_Code) :-
	NextArgNum is ArgNum + 1,
	( mode_is_fully_output(ModuleInfo, Mode) ->
	    TableEntryTemplate = 
		"mercury__%s_fact_table[ind/%d][ind%%%d].V_%d",
	    string__format(TableEntryTemplate, [s(PredName), 
		i(FactTableSize), i(FactTableSize), i(ArgNum)],
		    TableEntry),
	    ( Type = term__functor(term__atom("string"), [], _) ->
		mode_get_insts(ModuleInfo, Mode, _, FinalInst),
		( inst_is_not_partly_unique(ModuleInfo, FinalInst) ->
		    % Cast MR_ConstString -> MR_Word -> MR_String to avoid 
		    % gcc warning "assignment discards `const'".
		    Template = 
			"\t\tMR_make_aligned_string(%s, (MR_String) (MR_Word) %s);\n",
		    string__format(Template, [s(VarName), s(TableEntry)],
		    	C_Code0)
		;
		    % Unique modes need to allow destructive update so we
		    % need to make a copy of the string on the heap.
		    Template = 
"		MR_incr_hp_atomic(tmp,
			(strlen(%s) + sizeof(MR_Word)) / sizeof(MR_Word));
		%s = (MR_String) tmp;
		strcpy(%s, %s);
",
		    string__format(Template, [s(TableEntry), s(VarName),
			s(VarName), s(TableEntry)], C_Code0)
		)
	    ;
		Template = "\t\t%s = %s;\n",
		string__format(Template, [s(VarName), s(TableEntry)],
		    C_Code0)
	    ),
	    generate_fact_lookup_code(PredName, PragmaVars, Types,
		ModuleInfo, NextArgNum, FactTableSize, C_Code1),
	    string__append(C_Code0, C_Code1, C_Code)
	;
		% skip non-output arguments
	    generate_fact_lookup_code(PredName, PragmaVars, Types,
		ModuleInfo, NextArgNum, FactTableSize, C_Code)
	).
%---------------------------------------------------------------------------%

	% Code for lookup in nondet modes.

	% Generate code for the nondet mode with the primary key

	% XXX this should change to use the new model_non pragma c_code when
	% it has been implemented.
:- pred generate_primary_nondet_code(string, list(pragma_var), proc_id, 
		list(type), module_info, int, string, string).
:- mode generate_primary_nondet_code(in, in, in, in, in, in, out, out)
		is det.

generate_primary_nondet_code(PredName, PragmaVars, ProcID, ArgTypes,
		ModuleInfo, FactTableSize, ProcCode, ExtraCode) :-
	generate_nondet_proc_code(PragmaVars, PredName, ProcID, ExtraCodeLabel,
		ProcCode),

	ExtraCodeTemplate = "

MR_define_extern_entry(%s);
MR_declare_label(%s_i1);

MR_BEGIN_MODULE(%s_module)
	MR_init_entry(%s);
	MR_init_label(%s_i1);
MR_BEGIN_CODE
MR_define_entry(%s);
	MR_mkframe(""%s/%d"", %d, MR_LABEL(%s_i1));
	{
		/* create argument vars */
%s
		/* declare local variables */
%s
 		/* copy registers to input arg vars */
%s
		/* copy registers to framevars */
%s
		/* lookup hash table */
%s
	success_code_%s:
		/* lookup fact table */
%s
		/* save output args to registers */
%s
		MR_framevar(1) = ind + 1;
		MR_succeed();
	failure_code_%s:
		MR_fail();
	}
MR_define_label(%s_i1);
	if (MR_framevar(1) >= %s) 
		MR_fail();
	{
		/* create argument vars */
%s
		int ind = MR_framevar(1);
		/* copy framevars to registers */
%s		
 		/* copy registers to input arg vars */
%s
		/* test fact table entry */
%s
		/* lookup fact table */
%s
		/* save output args to registers */
%s
	}
	MR_framevar(1)++;
	MR_succeed();
MR_END_MODULE

extern MR_ModuleFunc %s_module;

/*
INIT mercury_sys_init_%s_module
*/
void mercury_sys_init_%s_module(void);
void mercury_sys_init_%s_module(void) {
	%s_module();
}

	",

	generate_argument_vars_code(PragmaVars, ArgTypes,
		ModuleInfo, ArgDeclCode, InputCode, OutputCode, SaveRegsCode,
		GetRegsCode, NumFrameVars),
	generate_decl_code(PredName, ProcID, DeclCode),
	proc_id_to_int(ProcID, ProcInt),
	string__format("%s_%d", [s(PredName), i(ProcInt)], LabelName), 
	generate_hash_code(PragmaVars, ArgTypes, ModuleInfo, LabelName, 0,
		PredName, 1, FactTableSize, HashCode),
	generate_fact_lookup_code(PredName, PragmaVars, ArgTypes, ModuleInfo, 1,
		FactTableSize, FactLookupCode),
	generate_fact_test_code(PredName, PragmaVars, ArgTypes, ModuleInfo,
		FactTableSize, FactTestCode),

	string__append_list(["mercury__", PredName, "_fact_table_num_facts"],
		NumFactsVar),
	list__length(PragmaVars, Arity),

	string__format(ExtraCodeTemplate, [
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(PredName),
			i(Arity),
			i(NumFrameVars),
			s(ExtraCodeLabel),
			s(ArgDeclCode),
			s(DeclCode),
			s(InputCode),
			s(SaveRegsCode),
			s(HashCode),
			s(LabelName),
			s(FactLookupCode),
			s(OutputCode),
			s(LabelName),
			s(ExtraCodeLabel),
			s(NumFactsVar),
			s(ArgDeclCode),
			s(GetRegsCode),
			s(InputCode),
			s(FactTestCode),
			s(FactLookupCode),
			s(OutputCode),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel)
		],
		ExtraCode).

	% generate code to create argument variables and assign them to 
	% registers
:- pred generate_argument_vars_code(list(pragma_var), list(type),
		module_info, string, string, string, string, string, int).
:- mode generate_argument_vars_code(in, in, in, out, out, out, out, out,
		out) is det.

generate_argument_vars_code(PragmaVars, Types, ModuleInfo, DeclCode, InputCode,
		OutputCode, SaveRegsCode, GetRegsCode, NumInputArgs) :-
	list__map(lambda([X::in, Y::out] is det, X = pragma_var(_,_,Y)),
		PragmaVars, Modes),
	make_arg_infos(Types, Modes, model_non, ModuleInfo, ArgInfos),
	generate_argument_vars_code_2(PragmaVars, ArgInfos, Types, ModuleInfo,
		DeclCode, InputCode, OutputCode, SaveRegsCode, GetRegsCode, 1,
		NumInputArgs).

:- pred generate_argument_vars_code_2(list(pragma_var), list(arg_info),
		list(type), module_info, string,
		string, string, string, string, int, int).
:- mode generate_argument_vars_code_2(in, in, in, in, out, out, out, out, out,
		in, out) is det.

generate_argument_vars_code_2(PragmaVars0, ArgInfos0, Types0, Module, DeclCode,
		InputCode, OutputCode, SaveRegsCode, GetRegsCode,
		NumInputArgs0, NumInputArgs) :-
	(
		PragmaVars0 = [],
		ArgInfos0 = [],
		Types0 = []
	->
		DeclCode = "",
		InputCode = "",
		OutputCode = "",
		SaveRegsCode = "",
		GetRegsCode = "",
		NumInputArgs = NumInputArgs0
	;
		PragmaVars0 = [pragma_var(_, VarName, _) | PragmaVars],
		ArgInfos0 = [arg_info(Loc, ArgMode) | ArgInfos],
		Types0 = [Type | Types]
	->
		generate_arg_decl_code(VarName, Type, Module, DeclCode0),
		( ArgMode = top_in ->
			NumInputArgs1 is NumInputArgs0 + 1,
			generate_arg_input_code(VarName, Type, Loc,
				NumInputArgs1, InputCode0, SaveRegsCode0,
				GetRegsCode0),
			OutputCode0 = ""
		; ArgMode = top_out ->
			generate_arg_output_code(VarName, Type, Loc,
				OutputCode0),
			InputCode0 = "",
			SaveRegsCode0 = "",
			GetRegsCode0 = "",
			NumInputArgs1 = NumInputArgs0
		;
			error("generate_argument_vars_code: invalid mode")
		),
		generate_argument_vars_code_2(PragmaVars, ArgInfos, Types,
			Module, DeclCode1, InputCode1, OutputCode1,
			SaveRegsCode1, GetRegsCode1, NumInputArgs1,
			NumInputArgs),
		string__append(DeclCode0, DeclCode1, DeclCode),
		string__append(InputCode0, InputCode1, InputCode),
		string__append(OutputCode0, OutputCode1, OutputCode),
		string__append(SaveRegsCode0, SaveRegsCode1, SaveRegsCode),
		string__append(GetRegsCode0, GetRegsCode1, GetRegsCode)
	;
		error("generate_argument_vars_code: list length mismatch")
	).

:- pred generate_arg_decl_code(string::in, (type)::in, module_info::in,
		string::out) is det.

generate_arg_decl_code(Name, Type, Module, DeclCode) :-
	C_Type = to_type_string(c, Module, Type),
	string__format("\t\t%s %s;\n", [s(C_Type), s(Name)], DeclCode).

:- pred generate_arg_input_code(string::in, (type)::in, int::in, int::in,
		string::out, string::out, string::out) is det.

generate_arg_input_code(Name, Type, RegNum, FrameVarNum, InputCode,
		SaveRegCode, GetRegCode) :-
	get_reg_name(RegNum, RegName),
	convert_type_from_mercury(RegName, Type, Converted),
	Template = "\t\t%s = %s;\n",
	string__format(Template, [s(Name), s(Converted)], InputCode),
	string__format("\t\tMR_framevar(%d) = %s;\n",
		[i(FrameVarNum), s(RegName)], SaveRegCode),
	string__format("\t\t%s = MR_framevar(%d);\n",
		[s(RegName), i(FrameVarNum)], GetRegCode).

:- pred generate_arg_output_code(string::in, (type)::in, int::in, 
		string::out) is det.

generate_arg_output_code(Name, Type, RegNum, OutputCode) :-
	get_reg_name(RegNum, RegName),
	convert_type_to_mercury(Name, Type, Converted),
	Template = "\t\t%s = %s;\n",
	string__format(Template, [s(RegName), s(Converted)], OutputCode).

:- pred get_reg_name(int::in, string::out) is det.

get_reg_name(RegNum, RegName) :-
	code_util__arg_loc_to_register(RegNum, Lval),
	( Lval = reg(RegType, N) ->
		llds_out__reg_to_string(RegType, N, RegName)
	;
		error("get_reg_name: lval is not a register")
	).

	% Generate code to test that the fact found matches the input arguments.
	% This is only required for generate_primary_nondet_code.  Other
	% procedures can test the key in the hash table against the
	% input arguments.
:- pred generate_fact_test_code(string, list(pragma_var), list(type),
		module_info, int, string).
:- mode generate_fact_test_code(in, in, in, in, in, out) is det.

generate_fact_test_code(PredName, PragmaVars, ArgTypes, ModuleInfo,
		FactTableSize, FactTestCode) :-
	string__append_list(["mercury__", PredName, "_fact_table"],
		FactTableName),
	generate_test_condition_code(FactTableName, PragmaVars, ArgTypes,
		ModuleInfo, 1, yes, FactTableSize, CondCode),
	string__append_list(["\t\tif(", CondCode, "\t\t) MR_fail();\n"],
		FactTestCode).

:- pred generate_test_condition_code(string, list(pragma_var), list(type),
		module_info, int, bool, int, string).
:- mode generate_test_condition_code(in, in, in, in, in, in, in, out) is det.

generate_test_condition_code(_, [], [], _, _, _, _, "").
generate_test_condition_code(_, [_|_], [], _, _, _, _, "") :-
	error("generate_test_condition_code: too many PragmaVars").
generate_test_condition_code(_, [], [_|_], _, _, _, _, "") :-
	error("generate_test_condition_code: too many ArgTypes").
generate_test_condition_code(FactTableName, [PragmaVar|PragmaVars], 
		[Type|Types], ModuleInfo, ArgNum, IsFirstInputArg0,
		FactTableSize, CondCode) :-
	PragmaVar = pragma_var(_, Name, Mode),
	( mode_is_fully_input(ModuleInfo, Mode) ->
		(
			Type = term__functor(term__atom("string"), [], _)
		->
			Template = "strcmp(%s[ind/%d][ind%%%d].V_%d, %s) != 0\n"
		;
			Template = "%s[ind/%d][ind%%%d].V_%d != %s\n"
		),
		string__format(Template, [s(FactTableName), i(FactTableSize),
			i(FactTableSize), i(ArgNum), s(Name)], CondCode0),
		(
			IsFirstInputArg0 = no
		->
			string__append("\t\t|| ", CondCode0, CondCode1)
		;
			CondCode0 = CondCode1
		),
		IsFirstInputArg = no
	;
		CondCode1 = "",
		IsFirstInputArg = IsFirstInputArg0
	),
	NextArgNum is ArgNum + 1,
	generate_test_condition_code(FactTableName, PragmaVars, Types,
		ModuleInfo, NextArgNum, IsFirstInputArg, FactTableSize,
		CondCode2),
	string__append(CondCode1, CondCode2, CondCode).


	% Generate code for a nondet mode using a secondary key.

	% XXX this should change to use the new model_non pragma c_code when
	% it has been implemented.
:- pred generate_secondary_nondet_code(string, list(pragma_var), proc_id, 
		list(type), module_info, int, string, string).
:- mode generate_secondary_nondet_code(in, in, in, in, in, in, out, out)
		is det.

generate_secondary_nondet_code(PredName, PragmaVars, ProcID, ArgTypes,
		ModuleInfo, FactTableSize, ProcCode, ExtraCode) :-
	generate_nondet_proc_code(PragmaVars, PredName, ProcID, ExtraCodeLabel,
		ProcCode),

	ExtraCodeTemplate = "

MR_define_extern_entry(%s);
MR_declare_label(%s_i1);

MR_BEGIN_MODULE(%s_module)
	MR_init_entry(%s);
	MR_init_label(%s_i1);
MR_BEGIN_CODE
MR_define_entry(%s);
	MR_mkframe(""%s/%d"", 4, MR_LABEL(%s_i1));
	{
		/* create argument vars */
%s
		/* declare local variables */
%s
 		/* copy registers to input arg vars */
%s
		/* lookup hash table */
%s
	success_code_%s:
		/* lookup fact table */
%s
		/* save output args to registers */
%s
		if (hashval == -1) MR_succeed_discard();
		MR_framevar(1) = hashval;
		MR_framevar(2) = (MR_Word) current_table;
		MR_framevar(3) = (MR_Word) keytype;
		MR_framevar(4) = current_key;
		MR_succeed();
	failure_code_%s:
		MR_fail();
	}
MR_define_label(%s_i1);
	{
		/* create argument vars */
%s
		MR_Integer hashval = MR_framevar(1);
		MR_Word ind;
		void *current_table = (void *) MR_framevar(2);
		char keytype = (char) MR_framevar(3);

		/* lookup hash table */
		switch(keytype) 
		{
			case 's':
%s
				break;
			case 'i':
%s
				break;
			case 'f':
%s
				break;
			default:
				MR_fatal_error(""fact table hash lookup: nondet stack corrupted?"");
		}
	success_code_%s:
		/* lookup fact table */
%s
		/* save output args to registers */
%s
		if (hashval == -1) MR_succeed_discard();
		MR_framevar(1) = hashval;
		MR_succeed();
	failure_code_%s:
		MR_fail();
	}
MR_END_MODULE

extern MR_ModuleFunc %s_module;

/*
INIT mercury_sys_init_%s_module
*/
void mercury_sys_init_%s_module(void);
void mercury_sys_init_%s_module(void) {
	%s_module();
}

	",

	generate_argument_vars_code(PragmaVars, ArgTypes,
		ModuleInfo, ArgDeclCode, InputCode, OutputCode, _SaveRegsCode,
		_GetRegsCode, _NumFrameVars),
	generate_decl_code(PredName, ProcID, DeclCode),
	proc_id_to_int(ProcID, ProcInt),
	string__format("%s_%d", [s(PredName), i(ProcInt)], LabelName), 
	string__append(LabelName, "_2", LabelName2),
	generate_hash_code(PragmaVars, ArgTypes, ModuleInfo, LabelName, 0,
		PredName, 1, FactTableSize, HashCode),

	generate_hash_lookup_code("(char *) MR_framevar(4)", LabelName2, 0,
		"strcmp(%s, %s) == 0", 's', no, "", [], [], ModuleInfo, 0, 0,
		StringHashLookupCode),
	generate_hash_lookup_code("MR_framevar(4)", LabelName2, 1, "%s == %s",	
		'i', no, "", [], [], ModuleInfo, 0, 0, IntHashLookupCode),
	generate_hash_lookup_code("MR_word_to_float(MR_framevar(4))",
		LabelName2, 2, "%s == %s", 'f', no, "", [], [], ModuleInfo,
		0, 0, FloatHashLookupCode),
	generate_fact_lookup_code(PredName, PragmaVars, ArgTypes, ModuleInfo, 1,
		FactTableSize, FactLookupCode),
	list__length(PragmaVars, Arity),

	string__format(ExtraCodeTemplate, [
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(PredName),
			i(Arity),
			s(ExtraCodeLabel),
			s(ArgDeclCode),
			s(DeclCode),
			s(InputCode),
			s(HashCode),
			s(LabelName),
			s(FactLookupCode),
			s(OutputCode),
			s(LabelName),
			s(ExtraCodeLabel),
			s(ArgDeclCode),
			s(StringHashLookupCode),
			s(IntHashLookupCode),
			s(FloatHashLookupCode),
			s(LabelName2),
			s(FactLookupCode),
			s(OutputCode),
			s(LabelName2),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel),
			s(ExtraCodeLabel)
		],
		ExtraCode).

