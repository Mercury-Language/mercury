%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% File: io.nu.nl.
% Main author: fjh.
%
% This file provides an implementation of some of the predicates
% declared in io.nl using non-logical NU-Prolog code.
%
%-----------------------------------------------------------------------------%

% program startup code
	
	% `io__exit_on_abort' is used as a global flag to control the
	% behaviour of main/1 when it is re-entered after a call to abort.
	% When we enter main/1, we check whether this is because
	% we have just restarted after a call to abort/0, and if so
	% we halt.  (abort/0 is called by error/1; see the
	% NU-Prolog manual entry for abort/0.)
	%
	% `io__inhibit_user_main' is used to ensure that
	% when the user redefines main/2 by loading their
	% file into the interpreter and then does an abort, we
	% execute io__main to invoke the NU-Prolog command loop,
	% rather than executing the users program.

:- dynamic io__exit_on_abort/0.
:- dynamic io__inhibit_user_main/0.
:- dynamic io__main_has_been_executed/0.

:- dynamic io__save_progname/1.
:- dynamic io__save_args/1.
:- dynamic io__save_exit_status/1.

:- pred main(list(atom)).
:- mode main(in) is det.

main(Args) :-
	( io__exit_on_abort ->
		write('Mercury aborting.'), nl,
		exit(1)
	;
		assert(io__exit_on_abort),
		( io__inhibit_user_main ->
			io__run(Args)
		;
			run(Args)
		),
		io__save_exit_status(ExitStatus),
		exit(ExitStatus)
	).

:- pred r(string).
:- mode r(in) is det.

r(ArgString) :-
	string__to_int_list(ArgString, ArgStringCodes),
	convert_args(ArgStringCodes, Args),
	run(Args).

:- pred convert_args(list(int), list(atom)).
:- mode convert_args(in, out) is det.

convert_args([], []).
convert_args([C|Cs], Args) :-
	( isspace(C) ->
		convert_args(Cs, Args)
	;
		convert_args_2(Cs, [C], Args)
	).

:- pred isspace(int).
:- mode isspace(in) is semidet.

isspace(0' ).

:- pred convert_args_2(list(int), list(int), list(atom)).
:- mode convert_args_2(in, in, out) is det.

convert_args_2([], Word, [Arg]) :-
	name(Arg, Word).
convert_args_2([C|Cs], Word, Args) :-
	( isspace(C) ->
		name(Arg, Word),
		Args = [Arg | Args1],
		convert_args(Cs, Args1)
	;
		append(Word, [C], Word1),
		convert_args_2(Cs, Word1, Args)
	).

:- pred run(list(atom)).
:- mode run(in) is det.

run(Args) :-
	io__init(Args),
	io__init_state(IOState0),
	io__call(main, IOState0, IOState),
	io__final_state(IOState).

:- pred io__run(list(atom)).
:- mode io__run(in) is det.

io__run(Args) :-
	io__init(Args),
	io__init_state(IOState0),
	io__call(io__main, IOState0, IOState),
	io__final_state(IOState).

:- pred io__init(list(atom)).
:- mode io__init(in) is det.

io__init(Args) :-
	putprop(io__saved_state, depth, 0),
	atoms_to_strings(Args, ArgStrings),
	retractall(io__save_progname(_)),
	retractall(io__save_args(_)),
	retractall(io__save_exit_status(_)),
	( ArgStrings = [Progname | Rest] ->
		assert(io__save_progname(Progname)),
		assert(io__save_args(Rest))
	;
		assert(io__save_args([]))
	),
	assert(io__save_exit_status(0)).


:- pred atoms_to_strings(list(atom), list(string)).
:- mode atoms_to_strings(in, out) is det.

atoms_to_strings([],[]).
atoms_to_strings([A|As],[S|Ss]) :-
	name(A,IntList),
	string__to_int_list(S,IntList),
	atoms_to_strings(As,Ss).

	% The following definition of main/2 is a default
	% and will normally be overridden by the users main/2.
	% It just invokes the NU-Prolog command loop.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

main --> io__main.

:- pred io__main(io__state, io__state).
:- mode io__main(di, uo) is det.

io__main -->
	{ io__main_has_been_executed ->
		true
	;
		write('Mercury Interpreter 0.1'), nl,
		assert(io__main_has_been_executed)
	},
	{ retractall(io__inhibit_user_main) },
	{ assert(io__inhibit_user_main) },
	{ retractall(io__exit_on_abort) },
	{ nuprolog ->
		'$mainloop'
	;
		version,
		break
	}.

:- pred io__call(pred).

/******
io__call(Goal, IOState0, IOState) :-
	findall(Goal - IOState1, call(Goal, IOState0, IOState1), Solutions),
	io__call_2(Goal, Solutions, IOState).

:- pred io__call_2(pred, list(_), io__state).
io__call_2(Goal, Solutions, IOState) :-
	(Solutions = [] ->
		nl(user_error),
		write(user_error, 'io.nl: error: goal `'),
		functor(Goal, F, N),
		write(user_error, F),
		write(user_error, '/'),
		write(user_error, N),
		write(user_error, ''' failed.'),
		nl(user_error),
		abort
	; Solutions = [SingleSolution - IOState0] ->
		Goal = SingleSolution,
		IOState = IOState0
	;
		nl(user_error),
		write(user_error, 'io.nl: error: goal `'),
		functor(Goal, F, N),
		write(user_error, F),
		write(user_error, '/'),
		write(user_error, N),
		write(user_error, ''' not deterministic.'),
		nl(user_error),
		abort
	).
*****/

io__call(Goal, IOState0, IOState) :-
	( call(Goal, IOState0, IOState) ->
		true
	;
		nl(user_error),
		write(user_error, 'io.nl: error: goal `'),
		functor(Goal, F, N),
		write(user_error, F),
		write(user_error, '/'),
		write(user_error, N),
		write(user_error, ''' failed.'),
		nl(user_error),
		abort
	).

% io__gc_call - call a goal, and do a manual garbage collection

io__gc_call(Goal) -->
	io__update_state,
	(
		io__call(Goal),
		io__update_state,
		{ 
		  getprop(io__saved_state, depth, Depth),
		  Depth1 is Depth + 1,
		  putprop(io__saved_state, depth, Depth1),
		  putprop(io__saved_state, Depth, Goal),
		  fail
		}
	;
		{ 
		  getprop(io__saved_state, depth, Depth1),
		  Depth is Depth1 - 1,
		  putprop(io__saved_state, depth, Depth),
		  getprop(io__saved_state, Depth, Goal),
		  remprop(io__saved_state, Depth)
		}
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% input predicates

io__read_char_code(Stream, Code, IO_0, IO) :-
	IO_0 = io__state(A, PutBack0, C, D, E),
 	(
		map__search(PutBack0, Stream, PutBackChars),
		PutBackChars = [Char | Chars]
 	->
		( Chars = [] ->
			map__det_remove(PutBack0, Stream, _, PutBack)
		;
			map__det_update(PutBack0, Stream, Chars, PutBack)
		),
		IO = io__state(A, PutBack, C, D, E),
		char_to_int(Char, Code)
 	;
		get0(Stream, Code),
		IO = IO_0
 	).
	%%% io__update_state.

io__putback_char(Stream, Char, IO_0, IO) :-
	IO_0 = io__state(A, PutBack0, C, D, E),
	( map__search(PutBack0, Stream, Chars) ->
		map__det_update(PutBack0, Stream, [Char | Chars], PutBack)
	;
		map__det_insert(PutBack0, Stream, [Char], PutBack)
	),
	IO = io__state(A, PutBack, C, D, E).

io__read_anything(S, Result) -->
	{ read(S, Term) },
	{ eof(Term) ->
		Result = eof
	;
		Result = ok(Term)
	}.

io__read_anything(Result) -->
	{ read(Term) },
	{ eof(Term) ->
		Result = eof
	;
		Result = ok(Term)
	}.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% output predicates - to the specified stream

io__write_int(S, I) -->
	{ write(S, I) },
	io__update_state.

io__write_char(S, C) -->
	{ write(S, C) },
	io__update_state.

io__write_float(S, F) -->
	{ write(S, F) },
	io__update_state.

io__write_string(Stream, String) -->
	{ (format(Stream, "~s", [String]), fail ; true) },
	io__update_state.
	
io__write_anything(S, I) -->
	{ write(S, I) },
	io__update_state.

io__flush_output(Stream) -->
	{ flushOutput(Stream) },
	io__update_state.

%-----------------------------------------------------------------------------%

% output predicates - to the current output stream

io__write_char(Char) -->
	io__output_stream(Stream),
	io__write_char(Stream, Char).

io__write_int(Int) -->
	io__output_stream(Stream),
	io__write_int(Stream, Int).

io__write_string(String) -->
	io__output_stream(Stream),
	io__write_string(Stream, String).

io__write_float(Float) -->
	io__output_stream(Stream),
	io__write_float(Stream, Float).

io__write_anything(Term) -->
	io__output_stream(Stream),
	io__write_anything(Stream, Term).

io__flush_output -->
	io__output_stream(Stream),
	io__flush_output(Stream).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

io__stdin_stream(user_input) --> [].

io__stdout_stream(user_output) --> [].

io__stderr_stream(user_error) --> [].

%-----------------------------------------------------------------------------%

io__input_stream(Stream) -->
	{ currentInput(Stream) }.

io__output_stream(Stream) -->
	{ currentOutput(Stream) }.

io__set_input_stream(NewStream, OldStream) -->
	{ currentInput(OldStream) },
	{ setInput(NewStream) },
	io__update_state.

io__set_output_stream(NewStream, OldStream) -->
	{ currentOutput(OldStream) },
	{ setOutput(NewStream) },
	io__update_state.

%-----------------------------------------------------------------------------%

io__do_open_input(FileName, Result, Stream) -->
	io__do_open(FileName, read, Result, Stream).

io__do_open_output(FileName, Result, Stream) -->
	io__do_open(FileName, write, Result, Stream).

io__do_open_append(FileName, Result, Stream) -->
	io__do_open(FileName, append, Result, Stream).

io__do_open(File, Mode, Result, Stream) -->
	{ string__to_int_list(File, FileCodes) },
	{ name(FileName, FileCodes) },
	( { open(FileName, Mode, Stream0) } ->
		{ Result = 0 },
		{ Stream = Stream0 }
	;
		{ Result = -1 },
		{ Stream = garbage }
	),
	io__update_state.

io__close_input(Stream) -->
	io__do_close(Stream).

io__close_output(Stream) -->
	io__do_close(Stream).

io__do_close(Stream) -->
	{ close(Stream) -> true ; true },
	io__delete_stream_name(Stream),
	io__update_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

io__get_line_number(LineNumber) -->
	{ currentInput(Stream) },
	io__get_line_number(Stream, LineNumber).

io__get_line_number(Stream, LineNumber) -->
	{ lineCount(Stream, LineNumber0) },
	=(io__state(_, PutBack, _, _, _)),
	{ map__search(PutBack, Stream, Chars) ->
		io__adjust_line_num(Chars, LineNumber0, LineNumber)
	;
		LineNumber = LineNumber0
	}.

:- pred io__adjust_line_num(list(char)::in, int::in, int::out) is det.

io__adjust_line_num([], N, N).
io__adjust_line_num([C | Cs], N0, N) :-
	( C = '\n' ->
		N1 is N0 - 1
	;
		N1 = N0
	),
	io__adjust_line_num(Cs, N1, N).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% These predicates are used to enforce correct usage
	% of io__states. io__update_state uses destructive assignment
	% to ensure that once an io state has been used it can't be
	% used again.

:- pred io__init_state(io__state).
io__init_state(IO_State) :-
	io__init_state(current, IO_State).

:- pred io__update_state(io__state, io__state).
io__update_state(IOState0, IOState) :-
	% using require/2 here causes rampant memory usage
	% because the strings get allocated every time
	( var(IOState0) ->
		error("io.nl: I/O predicate called with free io__state")
	;
		true
	),
	%%% ( IOState0 = io__state(_, _, current) ->
	%%% 	true
	%%% ;
	%%% 	error("io.nl: cannot retry I/O operation")
	%%% ),
	%%% IOState0 = io__state(Names, Globals, _),
	%%% $replacn(2, IOState0, old),
	%%% IOState = io__state(Names, Globals, current).
	IOState = IOState0.

:- pred io__final_state(io__state).
io__final_state(IOState) :-
	io__update_state(IOState, _).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% global state predicates

io__progname(DefaultName, Name) --> 
	{ io__save_progname(N) ->
		Name0 = N
	;
		Name0 = DefaultName
	},
	{ dir__basename(Name0, Name) }.

io__command_line_arguments(Args) --> 
	{ io__save_args(Args) }.
	
io__get_exit_status(ExitStatus) --> 
	{ io__save_exit_status(ExitStatus) }.

io__set_exit_status(ExitStatus) --> 
	{ retractall(io__save_exit_status(_)) },
	{ assert(io__save_exit_status(ExitStatus)) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% memory management predicates

io__preallocate_heap_space(N) -->
	{ preallocate_heap(N) },
	io__update_state.

% preallocate_heap(N) preallocates approximately n kilobytes of heap space.
% This is necessary in NU-Prolog to avoid
%	"Panic: growing stacks has required shifting the heap."

preallocate_heap(KBytes) :-
	(
		N is KBytes // 16,	
		preallocate_heap_2(N),
		fail
	;
		true
	).

% allocates N * 4096 * 4 bytes of heap space.

preallocate_heap_2(N) :-
	( N = 0 ->
		true
	;
		functor(_, f, 4095),
		N1 is N - 1,
		preallocate_heap_2(N1)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% miscellaneous predicates

io__call_system_code(Command, Status) -->
	{ system(Command, Status0) ->
		Status = Status0
	;
		Status = -1
	},
	io__update_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
