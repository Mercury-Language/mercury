%-----------------------------------------------------------------------------%
% Copyright (C) 1996 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: fact_table.m.
% Main author: dmo.

% This module handles compilation of fact tables contained in external
% files that have been declared with a `pragma fact_table' declaration.
% The facts are processed one by one. Each fact is read in, type and mode 
% checked, and then output as an element in an array of C structures.  
% Determinism for each procedure in the predicate is inferred and added to 
% the proc_info.  If a determinism has been declared for the procedure it 
% will be tested against the inferred determinism later on in det_report.m.  

% XXX The C file produced does not yet contain any functions for accessing
% the fact table.  These will be added soon.

:- module fact_table.

:- interface.

:- import_module io, string, list.
:- import_module prog_data, hlds_pred.

	% compile the fact table into a separate .o file.
:- pred fact_table_compile_facts(sym_name, arity, string, pred_info, pred_info,
		term__context, io__state, io__state).
:- mode fact_table_compile_facts(in, in, in, in, out, in, di, uo) is det.

	% generate c code to lookup a table in a given mode
:- pred fact_table_generate_c_code(sym_name, list(pragma_var), string).
:- mode fact_table_generate_c_code(in, in, out) is det.


:- implementation.

:- import_module int, map, std_util, assoc_list, char, require, library.
:- import_module parser, prog_out, term_io, hlds_out, hlds_data.

fact_table_compile_facts(PredName, Arity, FileName, PredInfo0, PredInfo, 
		Context) -->
    io__see(FileName, Result0),
    (
	{ Result0 = ok },
	{ string__append(FileName, ".c", OutputFileName) },
	io__open_output(OutputFileName, Result1),
	(
	    { Result1 = ok(OutputStream) },
	    write_fact_table_header(PredName, Arity, PredInfo0, FileName, 
		    OutputStream, Result2),
	    (
	    	{ Result2 = ok },
		infer_determinism_pass_1(PredInfo0, PredInfo1, CheckProcs),
		open_sort_files(CheckProcs, FileName, ProcStreams),
		compile_facts(PredName, Arity, PredInfo1, ProcStreams, 
			    OutputStream),
		{ pred_info_procedures(PredInfo1, ProcTable0) },
		infer_determinism_pass_2(ProcStreams, ProcTable0, ProcTable),
		{ pred_info_set_procedures(PredInfo1, ProcTable, PredInfo) },
		io__write_string(OutputStream, "};\n")
	    ;
	    	{ Result2 = error },
	    	{ PredInfo = PredInfo0 }
	    	% The `:- pred' or `:- func' declaration had some types that
	    	% are not supported in fact tables so there is no point trying
	    	% to type-check all the facts
	    ),
	    io__close_output(OutputStream)
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
	    { PredInfo = PredInfo0 }
	),
	io__seen
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
	{ PredInfo = PredInfo0 }
    ).


	% read in facts one by one and check and compile them
:- pred compile_facts(sym_name, arity, pred_info, 
		assoc_list(proc_id, io__output_stream), io__output_stream, 
		io__state, io__state).
:- mode compile_facts(in, in, in, in, in, di, uo) is det.

compile_facts(PredName, Arity, PredInfo, ProcStreams, OutputStream) -->
	parser__read_term(Result0),
	(
		{ Result0 = eof}
	;
		{ Result0 = error(Message, LineNum) },
		io__input_stream_name(FileName),
		prog_out__write_context(term__context(FileName, LineNum)),
		io__write_strings([Message, "\n"]),
		io__set_exit_status(1)
	;
		{ Result0 = term(VarSet, Term) },
		check_fact_term(PredName, Arity, PredInfo, VarSet, Term, 
			ProcStreams, OutputStream, Result1),
		(
			{ Result1 = ok }
		;
			{ Result1 = error }
		),
		compile_facts(PredName, Arity, PredInfo, ProcStreams, 
			OutputStream)
	).

:- type fact_result
	--->	ok ; error.

	% do syntactic and semantic checks on a fact term
:- pred check_fact_term(sym_name, arity, pred_info, varset, term, 
		assoc_list(proc_id, io__output_stream), io__output_stream, 
		fact_result, io__state, io__state).
:- mode check_fact_term(in, in, in, in, in, in, in, out, di, uo) is det.

check_fact_term(_, _, _, _, term__variable(_V), _, _, error) -->
	io__get_line_number(LineNum),
	io__input_stream_name(FileName),
	prog_out__write_context(term__context(FileName, LineNum)),
	io__write_string("Error: term is not a fact.\n"),
	io__set_exit_status(1).


check_fact_term(PredName, Arity0, PredInfo, _VarSet, 
		term__functor(Const, Terms0, Context), ProcStreams, 
		OutputStream, Result) -->
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	(
	    { PredName = unqualified(PredString) }
	;
	    { PredName = qualified(_, PredString) }
	),
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
		    { FuncHeadTerm = term__functor(term__atom(PredString), 
			    Terms1, _) },
		    { list__append(Terms1, [FuncResultTerm], Terms) },
		    { Arity is Arity0 + 1 }
		)
	    ->
		% Check that arity of the fact is correct
		{ list__length(Terms, Len) },
		(
		    { Len = Arity }
		->
		    io__write_string(OutputStream, "\t{ "),
		    { pred_info_arg_types(PredInfo, _, Types) },
		    check_fact_type_and_mode(Types, Terms, Context,
			    OutputStream, Result),
		    io__write_string(OutputStream, "},\n"),
		    { pred_info_procedures(PredInfo, ProcTable) },
		    write_sort_file_lines(ProcStreams, ProcTable, 
			    Terms)
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
:- pred check_fact_type_and_mode(list(type), list(term), term__context,
		io__output_stream, fact_result, io__state, io__state).
:- mode check_fact_type_and_mode(in, in, in, in, out, di, uo) is det.

check_fact_type_and_mode(_, [], _, _, ok) --> [].
check_fact_type_and_mode(Types0, [Term | Terms], Context0, OutputStream, Result)
	-->
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
		% We know that strings, integers and floats are ground, but we 
		% still need to check that they are the right type for this 
		% argument.
		{ Term = term__functor(term__string(String), _, Context) },
		(
			{ Types0 = [Type | Types] },
			{ Type = term__functor(term__atom("string"), _, _) }
		->
			io__write_string(OutputStream, """"),
			io__write_string(OutputStream, String),
			io__write_string(OutputStream, """, "),
			check_fact_type_and_mode(Types, Terms, Context0, 
				OutputStream, Result)
		;
			prog_out__write_context(Context),
			io__write_string("Type error in fact argument.\n"),
			io__set_exit_status(1),
			{ Result = error}
		)
	;
		{ Term = term__functor(term__integer(Int), _, Context) },
		(
			{ Types0 = [Type | Types] },
			{ Type = term__functor(term__atom("int"), _, _) }
		->
			io__write_int(OutputStream, Int),
			io__write_string(OutputStream, ", "),
			check_fact_type_and_mode(Types, Terms, Context0, 
				OutputStream, Result)
		;
			prog_out__write_context(Context),
			io__write_string("Type error in fact argument.\n"),
			io__set_exit_status(1),
			{ Result = error}
		)
	;
		{ Term = term__functor(term__float(Float), _, Context) },
		(
			{ Types0 = [Type | Types] },
			{ Type = term__functor(term__atom("float"), _, _) }
		->
			io__write_float(OutputStream, Float),
			io__write_string(OutputStream, ", "),
			check_fact_type_and_mode(Types, Terms, Context0, 
				OutputStream, Result)
		;
			prog_out__write_context(Context),
			io__write_string("Type error in fact argument.\n"),
			io__set_exit_status(1),
			{ Result = error}
		)
	).

:- pred write_fact_table_header(sym_name, arity, pred_info, 
		string, io__output_stream, fact_result, io__state, io__state).
:- mode write_fact_table_header(in, in, in, in, in, out, di, uo) is det.

write_fact_table_header(_PredName, _Arity, PredInfo, FileName, OutputStream, 
		Result) -->
	{ library__version(Version) },
	io__write_strings(OutputStream,
		["/*\n** Automatically generated from `", FileName,
		"' by the\n** Mercury compiler, version ", Version,
		".  Do not edit.\n*/\n\n"]),
	io__write_string(OutputStream, "#include ""imp.h""\n\n"),
	io__write_string(OutputStream, "struct fact_table {\n"),
	{ pred_info_arg_types(PredInfo, _, Types) },
	{ pred_info_context(PredInfo, Context) },  % location of :- pred decl
	write_fact_table_struct(Types, 1, Context, OutputStream, Result),
	io__write_string(OutputStream, "};\n\n"),
	io__write_string(OutputStream, 
		"static struct fact_table table[] = {\n").

:- pred write_fact_table_struct(list(type), int, term__context, 
		io__output_stream, fact_result, io__state, io__state).
:- mode write_fact_table_struct(in, in, in, in, out, di, uo) is det.

write_fact_table_struct([], _, _, _, ok) --> [].
write_fact_table_struct([Type | Types], I, Context, OutputStream, Result) -->
	(
		{ Type = term__functor(term__atom("string"), [], _) }
	->
		% XXX using "const String" causes some unwanted warning messages
		% from mgnuc so I'm using "const char *" for now.
		io__write_string(OutputStream, "\tconst char * V_"),
		io__write_int(OutputStream, I),
		io__write_string(OutputStream, ";\n"),
		{ I1 is I + 1 },
		write_fact_table_struct(Types, I1, Context, OutputStream, 
			Result)
	;
		{ Type = term__functor(term__atom("int"), [], _) }
	->
		io__write_string(OutputStream, "\tInteger V_"),
		io__write_int(OutputStream, I),
		io__write_string(OutputStream, ";\n"),
		{ I1 is I + 1 },
		write_fact_table_struct(Types, I1, Context, OutputStream, 
			Result)
	;
		{ Type = term__functor(term__atom("float"), [], _) }
	->
		io__write_string(OutputStream, "\tFloat V_"),
		io__write_int(OutputStream, I),
		io__write_string(OutputStream, ";\n"),
		{ I1 is I + 1 },
		write_fact_table_struct(Types, I1, Context, OutputStream, 
			Result)
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
		{ Result = error }
	).

	% First pass of determinism inference.
	% (out, out, ..., out) procs are multidet and (in, in, .., in) procs are
	% semidet.  Return a list of procs containing both in's and out's.
	% These need further analysis later in pass 2.
:- pred infer_determinism_pass_1(pred_info, pred_info, list(proc_id), 
		io__state, io__state).
:- mode infer_determinism_pass_1(in, out, out, di, uo) is det.

infer_determinism_pass_1(PredInfo0, PredInfo, CheckProcs) --> 
	{ pred_info_procedures(PredInfo0, ProcTable0) },
	{ pred_info_procids(PredInfo0, ProcIDs) },
	infer_proc_determinism_pass_1(ProcIDs, ProcTable0, ProcTable, 
			[], CheckProcs),
	{ pred_info_set_procedures(PredInfo0, ProcTable, PredInfo) }.

:- type fact_table_mode_type
	--->	all_in		% modes of all arguments are input
	;	all_out		% modes of all arguments are output
	;	in_out		% modes are a mixture of input and output
	;	other		% some arguments contain modes that are 
				% not in or out
	;	unknown.

:- type inferred_determinism
	--->	yes(determinism) % determinism has been inferred
	;	no		 % determinism has not yet been inferred
	;	error.		 % an error occurred trying to infer determinism

:- pred infer_proc_determinism_pass_1(list(proc_id), proc_table, proc_table,
		list(proc_id), list(proc_id), io__state, io__state).
:- mode infer_proc_determinism_pass_1(in, in, out, in, out, di, uo) is det.

infer_proc_determinism_pass_1([], ProcTable, ProcTable, CheckProcs, CheckProcs)
		--> [].
infer_proc_determinism_pass_1([ProcID | ProcIDs], ProcTable0, ProcTable, 
			CheckProcs0, CheckProcs) -->
	{ map__lookup(ProcTable0, ProcID, ProcInfo0) },
	{ proc_info_argmodes(ProcInfo0, ArgModes) },
	{ fact_table_mode_type(ArgModes, ModeType) },
	(
		{ ModeType = all_in },
		{ InferredDetism = yes(semidet) },
		{ CheckProcs1 = CheckProcs0 }
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
			InferredDetism = yes(cc_multidet)
		;
			InferredDetism = yes(multidet)
		},
		{ CheckProcs1 = CheckProcs0 }
	;
		{ ModeType = in_out },

			% Don't have enough info to infer determinism yet.
			% Put it off till the second pass.
		{ InferredDetism = no },
			% add to list and check in pass 2
		{ CheckProcs1 = [ProcID | CheckProcs0] }
	;
		{ ModeType = other }, 		% mode error
		{ InferredDetism = error },
		{ proc_info_context(ProcInfo0, Context) },
		prog_out__write_context(Context),
		io__write_string("Error: only in and out modes are currently "),
		io__write_string("supported in fact tables.\n"),
		io__set_exit_status(1),
		{ CheckProcs1 = CheckProcs0 }
	;
		{ModeType = unknown },		 % mode error
		{ InferredDetism = error },
		{ proc_info_context(ProcInfo0, Context) },
		prog_out__write_context(Context),
		io__write_string("Error: mode list for procedure is empty.\n"),
		io__set_exit_status(1),
		{ CheckProcs1 = CheckProcs0 }
	),
	{
		InferredDetism = yes(Determinism)
	->
		proc_info_set_inferred_determinism(ProcInfo0, Determinism, 
			ProcInfo),
		map__set(ProcTable0, ProcID, ProcInfo, ProcTable1)
	;
		ProcTable1 = ProcTable0
	},
	infer_proc_determinism_pass_1(ProcIDs, ProcTable1, ProcTable, 
		CheckProcs1, CheckProcs).

% Return the fact_table_mode_type for a procedure.  At the moment the only
% recognised modes are mercury_builtin:in and mercury_builtin:out.
% It should really find all modes defined as ground->ground and free->ground
% and treat them the same as in and out, respectively.
:- pred fact_table_mode_type(list(mode), fact_table_mode_type).
:- mode fact_table_mode_type(in, out) is det.

fact_table_mode_type([], unknown).
fact_table_mode_type([Mode | Modes], ModeType) :-
	(
		Mode = ((_) -> (_)),
		ModeType0 = other
	;
		Mode = user_defined_mode(SymName, Insts),
		(
			SymName = unqualified(_Name),
			ModeType0 = other
		;
			SymName = qualified(ModSpec, Name),
			(
				Insts = [],
				ModSpec = "mercury_builtin"
			->
				(
					Name = "in"
				->
					ModeType0 = all_in
				;
					Name = "out"
				->
					ModeType0 = all_out
				;
					ModeType0 = other
				)
			;
				ModeType0 = other
			)
		)
	),
	(
		ModeType0 = other
	->
		ModeType = other
	;
		fact_table_mode_type(Modes, ModeType1),
		(
			ModeType1 = unknown
		->
			ModeType = ModeType0
		;
			ModeType1 = other
		->
			ModeType = other
		;
			ModeType1 = ModeType0
		->
			ModeType = ModeType0
		;
			ModeType = in_out
		)
	).

	% open_sort_files(ProcIDs, BaseName, ProcStreams)
	% Open a temporary sort file for each proc_id in ProcIDs.
	% Return the (proc_id - io__output_stream) pairs in ProcStreams.
	% File names are derived from BaseName.ProcID.
:- pred open_sort_files(list(proc_id), string, 
		assoc_list(proc_id, io__output_stream), io__state, io__state).
:- mode open_sort_files(in, in, out, di, uo) is det.

open_sort_files([], _, []) --> [].
open_sort_files([ProcID | ProcIDs], BaseName, ProcStreams) -->
	{ string__format("%s.tmp.%d", [s(BaseName), i(ProcID)], SortFileName) },
	io__open_output(SortFileName, Result),
	(
		{ Result = ok(Stream) },
		open_sort_files(ProcIDs, BaseName, ProcStreams0),
		{ ProcStreams = [(ProcID - Stream) | ProcStreams0] }
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
	% The line is made up of the input arguments of the procedure.
:- pred write_sort_file_lines(assoc_list(proc_id, io__output_stream), 
		proc_table, list(term), io__state, io__state).
:- mode write_sort_file_lines(in, in, in, di, uo) is det.

write_sort_file_lines([], _, _) --> [].
write_sort_file_lines([(ProcID - Stream) | ProcStreams], ProcTable, Terms) -->
	{ map__lookup(ProcTable, ProcID, ProcInfo) },
	{ proc_info_argmodes(ProcInfo, ArgModes) },
	{ assoc_list__from_corresponding_lists(ArgModes, Terms, ModeTerms) },
	{ make_sort_file_key(ModeTerms, Key) },
	io__write_strings(Stream, [Key, "\n"]),
	write_sort_file_lines(ProcStreams, ProcTable, Terms).

	% Create a key for the fact table entry.
	% Arguments are separated by ":".
	% Colons in string literals are replaced by "\:".
	% Newlines are replaced by "\n" and backslashes by "\\"
:- pred make_sort_file_key(assoc_list(mode, term), string).
:- mode make_sort_file_key(in, out) is det.

make_sort_file_key([], "").
make_sort_file_key([(Mode - Term) | ModeTerms], Key) :-
	(
		Mode = user_defined_mode(SymName, []),
		SymName = qualified("mercury_builtin", "in"),
		Term = term__functor(Const, [], _Context)
	->
		make_key_part(Const, KeyPart),
		make_sort_file_key(ModeTerms, Key0),
		string__append(":", Key0, Key1), % field separator
		string__append(KeyPart, Key1, Key)
	;
		make_sort_file_key(ModeTerms, Key)
	).

:- pred make_key_part(const, string).
:- mode make_key_part(in, out) is det.

make_key_part(term__atom(_), _):- 
	error("make_key_part: enumerated types are not supported yet.").
make_key_part(term__integer(I), K) :- 
	string__int_to_string(I, K).
make_key_part(term__float(F), K) :- 
	string__float_to_string(F, K).
make_key_part(term__string(S), K) :-
	string__to_char_list(S, Cs0),
	key_from_chars(Cs0, Cs),
	string__from_char_list(Cs, K).

	% escape all backslashes and colons with a backslash and replace all
	% newlines with "\n".
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
		ECs1 = [':', '\\' | ECs0]
	;
		C = ('\n')
	->
		ECs1 = ['n', '\\' | ECs0]
	;
		ECs1 = [C | ECs0]
	),
	key_from_chars_2(Cs, ECs1, ECs).

	% infer_determinism_pass_2(ProcStreams, ProcTable0, ProcTable),
	% Close each sort file then run `sort' on it to see if the keys are
	% unique.  If they are, the procedure is semidet, otherwise it is 
	% nondet.
:- pred infer_determinism_pass_2(assoc_list(proc_id, io__output_stream),
		proc_table, proc_table, io__state, io__state).
:- mode infer_determinism_pass_2(in, in, out, di, uo) is det.

infer_determinism_pass_2([], ProcTable, ProcTable) --> [].
infer_determinism_pass_2([(ProcID - Stream) | ProcStreams], ProcTable0, 
		ProcTable) -->
	{ map__lookup(ProcTable0, ProcID, ProcInfo0) },
	io__output_stream_name(Stream, FileName),
	io__close_output(Stream),
	{ string__format("sort %s | sort -cu >/dev/null 2>&1", [s(FileName)],
		Command) },
	io__call_system(Command, Result),
	(
		{ Result = ok(ExitStatus) },

			% sort -cu returns 0 if file is sorted and contains
			% no duplicate keys, >=1 if duplicate keys exist 
		(
			{ ExitStatus = 0 }
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
			io__write_strings([
				"An error occurred in the `sort' program\n",
				"  during fact table determinism inference.\n"
				]),
			io__set_exit_status(1),
			{ Determinism = erroneous }
		)
	;
		{ Result = error(ErrorCode) },
		{ io__error_message(ErrorCode, ErrorMessage) },
		io__write_strings([ 
			"Error executing system command `sort': ", 
			ErrorMessage, 
			".\n" ]),
		io__set_exit_status(1),
		{ Determinism = erroneous }
	),
	{ proc_info_set_inferred_determinism(ProcInfo0, Determinism, ProcInfo)},
	{ map__set(ProcTable0, ProcID, ProcInfo, ProcTable1) },
	infer_determinism_pass_2(ProcStreams, ProcTable1, ProcTable).





:- pred write_fact_table_functions(io__output_stream, io__state, io__state).
:- mode write_fact_table_functions(in, di, uo) is det.

write_fact_table_functions(_OutputStream) --> []. % not finished yet.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

fact_table_generate_c_code(_PredName, _PragmaVars, C_Code) :-
	C_Code = "fprintf(stderr, ""Fact Tables not yet working\\n"");".
