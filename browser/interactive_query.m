%---------------------------------------------------------------------------%
% Copyright (C) 1999-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% file: interactive_query.m
% author: fjh
% A module to invoke interactive queries using dynamic linking.
%
% This module reads in a query, writes out Mercury code for it to `query.m',
% invokes the Mercury compiler mmc to compile `query.m' to `libquery.so',
% dynamically loads in the object code for the module `query'
% from the file `libquery.so', looks up the address of the
% procedure query/2 in that module, and then calls that procedure.

:- module mdb__interactive_query.
:- interface.
:- import_module io, list.

:- pred query(query_type::in, imports::in, options::in,
		io__input_stream::in, io__output_stream::in,
		state::di, state::uo) is det.

% query_external/7 is the same as query/7 but for the use of the external 
% debugger.
:- pred query_external(query_type::in, imports::in, options::in,
		io__input_stream::in, io__output_stream::in,
		state::di, state::uo) is det.

:- type query_type ---> normal_query ; cc_query ; io_query.
:- type imports == list(string).
:- type options == string.

:- implementation.
:- import_module std_util, bool, string, term, varset, term_io, parser.
:- import_module mdb__dl, mdb__name_mangle.
:- import_module mdb__util.

:- pragma export(query(in, in, in, in, in, di, uo), "ML_query").

:- type prog ---> prog(query_type, imports, term, varset).

query(QueryType, Imports, Options, MDB_Stdin, MDB_Stdout) -->
	% write_import_list(Imports),
	util__trace_getline(query_prompt(QueryType), Result,
			MDB_Stdin, MDB_Stdout),
	( { Result = eof },
		io__nl(MDB_Stdout)
	; { Result = error(Error) },
		{ io__error_message(Error, Msg) },
		io__write_string(MDB_Stdout, Msg), io__nl(MDB_Stdout),
		query(QueryType, Imports, Options, MDB_Stdin, MDB_Stdout)
	; { Result = ok(Line) },
		{ parser__read_term_from_string("", Line, _, ReadTerm) },
		query_2(QueryType, Imports, Options, MDB_Stdin, MDB_Stdout,
				ReadTerm)
	).


:- pred query_2(query_type::in, imports::in, options::in,
		io__input_stream::in, io__output_stream::in,
		read_term(generic)::in, state::di, state::uo) is det.

query_2(QueryType, Imports, Options, MDB_Stdin, MDB_Stdout, ReadTerm) -->
	( { ReadTerm = eof },
		io__nl(MDB_Stdout)
	; { ReadTerm = error(Msg, _Line) },
		io__write_string(MDB_Stdout, Msg),
		io__nl(MDB_Stdout),
		query(QueryType, Imports, Options, MDB_Stdin, MDB_Stdout)
	; { ReadTerm = term(VarSet, Term) },
		% io__write_string("Read term: "),
		% term_io__write_term(Term, VarSet),
		% io__write_string("\n"),
		(if { Term = term__functor(term__atom("quit"), [], _) } then
			io__nl(MDB_Stdout)
		else if { Term = term__functor(term__atom("options"),
				[term__functor(term__string(NewOptions),
					[], _)], _) } then
			print(MDB_Stdout, "Compilation options: "),
			print(MDB_Stdout, NewOptions),
			io__nl(MDB_Stdout),
			query(QueryType, Imports, NewOptions,
				MDB_Stdin, MDB_Stdout)
		else if { term_to_list(Term, ModuleList) } then
			{ list__append(Imports, ModuleList, NewImports) },
			write_import_list(MDB_Stdout, NewImports),
			query(QueryType, NewImports, Options,
				MDB_Stdin, MDB_Stdout)
		else
			run_query(Options,
				prog(QueryType, Imports, Term, VarSet)),
			query(QueryType, Imports, Options,
				MDB_Stdin, MDB_Stdout)
		)
	).


% Type of the terms sent to the socket during an interactive query session 
% under the control of the external debugger.
:- type interactive_query_response 
	--->	iq_ok
	;	iq_imported(imports)
	;	iq_quit
	;	iq_eof
	;	iq_error(string)
	.

:- pragma export(query_external(in, in, in, in, in, di, uo), 
	"ML_query_external").

query_external(QueryType, Imports, Options, SocketIn, SocketOut) -->
	io__set_input_stream(SocketIn, OldStdin),
	term_io__read_term(Result),
	io__set_input_stream(OldStdin, _),
	( { Result = eof },
		send_term_to_socket(iq_eof, SocketOut)
	; { Result = error(ErrorMsg, _Line) },
		send_term_to_socket(iq_error(ErrorMsg), SocketOut),
		query_external(QueryType, Imports, Options, SocketIn, SocketOut)
	; { Result = term(VarSet, Term) },
		(if { Term = term__functor(term__atom("quit"), [], _) } then
			send_term_to_socket(iq_quit, SocketOut)
		else if { Term = term__functor(term__atom("options"),
				[term__functor(term__string(NewOptions),
					[], _)], _) } then
			send_term_to_socket(iq_ok, SocketOut),
			query_external(QueryType, Imports, NewOptions,
				SocketIn, SocketOut)
		else if { term_to_list(Term, ModuleList) } then
			{ list__append(Imports, ModuleList, NewImports) },
			send_term_to_socket(iq_imported(NewImports), SocketOut),
			query_external(QueryType, NewImports, Options,
				SocketIn, SocketOut)
		else
			run_query(Options,
				prog(QueryType, Imports, Term, VarSet)),
			send_term_to_socket(iq_ok, SocketOut),
			query_external(QueryType, Imports, Options,
				SocketIn, SocketOut)
		)
	).

:- pred send_term_to_socket(interactive_query_response, io__output_stream,
	io__state, io__state).
:- mode send_term_to_socket(in, in, di, uo) is det.
send_term_to_socket(Term, SocketStream) -->
	write(SocketStream, Term),
	print(SocketStream, ".\n"),
	flush_output(SocketStream).

:- func query_prompt(query_type) = string.
query_prompt(normal_query) = "?- ".
query_prompt(cc_query) = "?- ".
query_prompt(io_query) = "run <-- ".

:- pred term_to_list(term, list(string)).
:- mode term_to_list(in, out) is semidet.
term_to_list(term__functor(term__atom("[]"), [], _), []).
term_to_list(term__functor(term__atom("[|]"),
		[term__functor(term__atom(Module), [], _C1), Rest], _C2),
		[Module | Modules]) :-
	term_to_list(Rest, Modules).

:- pred run_query(options, prog, io__state, io__state).
:- mode run_query(in, in, di, uo) is det.
run_query(Options, Program) -->
	{ SourceFile = "query.m" },
	io__get_environment_var("MERCURY_OPTIONS", MAYBE_MERCURY_OPTIONS),
	(if { MAYBE_MERCURY_OPTIONS = yes(MERCURY_OPTIONS) } then	
		io__set_environment_var("MERCURY_OPTIONS", ""),
		write_prog_to_file(Program, SourceFile),
		compile_file(Options, Succeeded),
		(if { Succeeded = yes } then
			dynamically_load_and_run
		else
			{ true }
		),
		io__set_environment_var("MERCURY_OPTIONS", MERCURY_OPTIONS)
	else
		print("Unable to unset MERCURY_OPTIONS environment variable")
	).

%-----------------------------------------------------------------------------%
%
% print the program to a file
%

:- pred write_prog_to_file(prog, string, io__state, io__state).
:- mode write_prog_to_file(in, in, di, uo) is det.

write_prog_to_file(Program, FileName) -->
	open_output_file(FileName, Stream),
	io__set_output_stream(Stream, OldStream),
	write_prog_to_stream(Program),
	io__set_output_stream(OldStream, _),
	io__close_output(Stream).

:- pred open_output_file(string::in, io__output_stream::out,
		io__state::di, io__state::uo) is det.

open_output_file(File, Stream) -->
	io__open_output(File, Result),
	( { Result = ok(Stream0) },
		{ Stream = Stream0 }
	; { Result = error(Error) },
		io__progname("interactive", Progname),
		{ io__error_message(Error, ErrorMessage) },
		{ string__append_list([
			Progname, ": ",
			"error opening file `", File, "' for output:\n\t",
			ErrorMessage, "\n"],
			Message) },
		io__write_string(Message),
		% XXX we really ought to throw an exception here;
		%     instead, we just return a bogus stream (stdout)
		io__stdout_stream(Stream)
	).

:- pred write_prog_to_stream(prog::in, io__state::di, io__state::uo) is det.

write_prog_to_stream(prog(QueryType, Imports, Term, VarSet)) -->
	io__write_string("
			:- module query.
			:- interface.
			:- import_module io.
			:- pred run(io__state::di, io__state::uo) is cc_multi.
			:- implementation.
			"),
	io__output_stream(Out),
	write_import_list(Out, ["std_util" | Imports]),
	io__write_string("
			:- pragma source_file(""<stdin>"").
			run -->
	"),
	( { QueryType = normal_query },
		{ term__vars(Term, Vars0) },
		{ list__remove_dups(Vars0, Vars) },
/*
	For a normal query, we generate code that looks like this:

		run -->
			unsorted_aggregate(
				(pred(res(A,B,C)::out) is nondet :-
					query(A,B,C)),
				(pred(res(A,B,C)::in, di, uo) is cc_multi -->
					print("A = "), print_cc(A), print(","),
					print("B = "), print_cc(B), print(","),
					print("C = "), print_cc(C), print(","),
					print("true ;\n"))
			),
			print(""fail.\n""),
			print(""No (more) solutions.\n"").

		:- type res(A, B, C) ---> res(A, B, C).

		% :- mode query(out, out, out) is nondet.
		query(res(A, B, C)) :-
				...
*/
		io__write_string("
				unsorted_aggregate(
					(pred(res"),
		write_args(Vars, VarSet),
		io__write_string("::out) is nondet :-
						query"),
		write_args(Vars, VarSet),
		io__write_string("),"),
		io__write_string("(pred(res"),
		write_args(Vars, VarSet),
		io__write_string("::in, di, uo) is cc_multi -->
						"),
		list__foldl(write_code_to_print_one_var(VarSet), Vars),
		io__write_string("
					io__write_string(""true ;\n""))
				),
				io__write_string(""fail.\n""),
				io__write_string(""No (more) solutions.\n"").

			:- type res"),
		write_args(Vars, VarSet),
		io__write_string(" ---> res"),
		write_args(Vars, VarSet),
		io__write_string(".\n"),

/******
		io__write_string("
			:- mode query"),
		( { Vars \= [] } ->
			{ list__length(Vars, NumVars) },
			{ list__duplicate(NumVars, "out", Modes) },
			io__write_string("("),
			io__write_list(Modes, ", ", io__write_string),
			io__write_string(")")
		;
			[]
		),
		io__write_string(" is nondet."),
******/

		io__write_string("
			query"),
		write_args(Vars, VarSet),
		io__write_string(" :- "),
		write_line_directive,
		term_io__write_term(VarSet, Term),
		io__write_string(" .\n")
	; { QueryType = cc_query },
		%
		% For a cc_query, we generate code that looks like this:
		%
		%	run --> if { query(A, B, C) } then 
		%			print("A = "), print(A), print(", "),
		%			print("B = "), print(B), print(", "),
		%			print("C = "), print(C), print(", "),
		%			print("Yes.\n"))
		%		else
		%			print("No solution.\n").
		%
		%	query(A, B, C) :- ...
		%

		{ term__vars(Term, Vars0) },
		{ list__remove_dups(Vars0, Vars) },
		io__write_string("(if { query"),
		write_args(Vars, VarSet),
		io__write_string(" } then\n"),
		list__foldl(write_code_to_print_one_var(VarSet), Vars),
		io__write_string("
					io__write_string(""true.\\n"")
				else
					io__write_string(""No solution.\\n"")
				).
		"),
		io__write_string("query"),
		write_args(Vars, VarSet),
		io__write_string(" :-\n"),
		write_line_directive,
		term_io__write_term(VarSet, Term),
		io__write_string(" .\n")
	; { QueryType = io_query },
		%
		% For an io_query, we just spit the code straight out:
		%
		%	run --> ...
		%
		write_line_directive,
		term_io__write_term(VarSet, Term),
		io__write_string(" .\n")
	).

:- pred write_line_directive(io__state::di, io__state::uo) is det.

write_line_directive -->
	io__write_string("\n#"),
	io__get_line_number(LineNum),
	io__write_int(LineNum),
	io__nl.

:- pred write_code_to_print_one_var(varset::in, var::in,
		io__state::di, io__state::uo) is det.

write_code_to_print_one_var(VarSet, Var) -->
	io__write_string("io__write_string("""),
	term_io__write_variable(Var, VarSet),
	io__write_string(" = ""), io__write_cc("),
	term_io__write_variable(Var, VarSet),
	print("), io__write_string("", ""), ").

:- pred write_args(list(var)::in, varset::in,
		io__state::di, io__state::uo) is det.

write_args(Vars, VarSet) -->
	( { Vars \= [] } ->
		io__write_string("("),
		io__write_list(Vars, ", ", write_one_var(VarSet)),
		io__write_string(")")
	;
		[]
	).

:- pred write_one_var(varset::in, var::in,
		io__state::di, io__state::uo) is det.

write_one_var(VarSet, Var) -->
	term_io__write_variable(Var, VarSet).

:- pred write_import_list(io__output_stream::in, imports::in,
		io__state::di, io__state::uo) is det.

write_import_list(Out, Imports) -->
	io__write_string(Out, ":- import_module "),
	io__write_list(Out, Imports, ", ", term_io__quote_atom),
	io__write_string(Out, ".\n").

%-----------------------------------------------------------------------------%
%
% invoke the Mercury compile to compile the file to a shared object
%

:- pred compile_file(options, bool, state, state).
:- mode compile_file(in, out, di, uo) is det.

compile_file(Options, Succeeded) -->
	%
	% We use the following options:
	%	--pic-reg
	%		needed for shared libraries / dynamic linking
	%	--infer-all
	%		for inferring the type etc. of query/N
	%	-O0 --no-c-optimize
	%		to improve compilation speed
	%	--no-warn-det-decls-too-lax
	%	--no-warn-simple-code
	%		to avoid spurious warnings in the automatically
	%		generated parts of the query predicate
	%
	{ string__append_list([
		"mmc --grade ", grade_option, " ",
		"--infer-all ",
		"--pic-reg ", "-O0 --no-c-optimize ",
		"--no-warn-simple-code --no-warn-det-decls-too-lax ",
		"-c ", Options,
		" query.m"], Command) },
	invoke_system_command(Command, Succeeded0),
	( { Succeeded0 = yes } ->
		% Figure out the location of the object file,
		% which will depend on whether --use-subdirs was specified.
		% XXX This code doesn't properly handle the case when the
		% option is later overridden, e.g. if the options are
		% `--use-subdirs --no-use-subdirs'.  But that case
		% is not very likely to occur in practice...
		{ string__sub_string_search(Options, "--use-subdirs", _) ->
			QueryObject = "Mercury/os/query.o"
		;
			QueryObject = "query.o"
		},
			
		% We use the following options:
		%	--make-shared-lib
		%		needed so we can dynamically load in the
		%		code
		%	--allow-undefined
		%		needed to allow the query to reference
		%		symbols defined in the program
		%	--trace
		{ string__append_list([
			"ml --grade ", grade_option,
			" --trace",
			" --allow-undefined",
			" --make-shared-lib ", Options,
			" -o libquery.so ", QueryObject], Command2) },
		invoke_system_command(Command2, Succeeded)
	;
		{ Succeeded = no }
	).

:- func grade_option = string.
%
% `grade_option' returns MR_GRADE_OPT,
% which is defined in runtime/mercury_grade.h.
% This is a string containing the grade that the current
% executable was compiled in, in a form suitable for
% passing as a `--grade' option to mmc or ml.
%
:- pragma c_header_code("
	#include ""mercury_grade.h""
	#include ""mercury_string.h""
").
:- pragma c_code(grade_option = (GradeOpt::out),
	[thread_safe, will_not_call_mercury],
	"MR_make_aligned_string(GradeOpt, (MR_String) MR_GRADE_OPT);").

:- func verbose = bool.
verbose = no.

:- pred invoke_system_command(string, bool, state, state).
:- mode invoke_system_command(in, out, di, uo) is det.

invoke_system_command(Command, Succeeded) -->
	(if { verbose = yes } then
		io__write_string("% Invoking system command `"),
		io__write_string(Command),
		io__write_string("'...\n"),
		io__flush_output
	else
		[]
	),
	io__call_system(Command, Result),
	(if { Result = ok(0) } then
		( if { verbose = yes } then print("% done.\n") else [] ),
		{ Succeeded = yes }
	else if { Result = ok(_) } then
		print("Compilation error(s) occurred.\n"),
		{ Succeeded = no }
	else
		print("Error: unable to invoke the compiler.\n"),
		{ Succeeded = no }
	).

%-----------------------------------------------------------------------------%
%
% dynamically load the shared object and execute the query
%

:- pred dynamically_load_and_run(io__state::di, io__state::uo) is det.

dynamically_load_and_run -->
	%
	% Load in the object code for the module `query' from
	% the file `libquery.so'.
	%
	dl__open("./libquery.so", lazy, local, MaybeHandle),
	(	
		{ MaybeHandle = error(Msg) },
		print("dlopen failed: "), print(Msg), nl
	;
		{ MaybeHandle = ok(Handle) },
		%
		% Look up the address of the first mode (mode number 0)
		% of the predicate run/2 in the module query.
		%
		{ QueryProc = mercury_proc(predicate, unqualified("query"),
					"run", 2, 0) },
		dl__mercury_sym(Handle, QueryProc, MaybeQuery),
		(
			{ MaybeQuery = error(Msg) },
			print("dlsym failed: "), print(Msg), nl
		;
			{ MaybeQuery = ok(QueryPred0) },
			%
			% Cast the higher-order term that we obtained
			% to the correct higher-order inst.
			%
			{ QueryPred = inst_cast(QueryPred0) },
			%
			% Call the procedure whose address
			% we just obtained.
			%
			call(QueryPred)
		),
		%
		% unload the object code in the libquery.so file
		%
		dl__close(Handle, Result),
		(
			{ Result = error(CloseMsg) },
			print("dlclose failed: "), print(CloseMsg), nl
		;
			{ Result = ok }
		)
	).

%
% dl__mercury_sym returns a higher-order term with inst `ground'.
% We need to cast it to the right higher-order inst, namely
% `pred(di, uo) is det' before we can actually call it.
% The function inst_cast/1 defined below does that.
%

:- type io_pred == pred(io__state, io__state).
:- inst io_pred == (pred(di, uo) is det).

:- func inst_cast(io_pred) = io_pred.
:- mode inst_cast(in) = out(io_pred) is det.

:- pragma c_code(inst_cast(X::in) = (Y::out(io_pred)),
	[will_not_call_mercury, thread_safe], "Y = X").

%-----------------------------------------------------------------------------%
