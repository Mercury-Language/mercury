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
	% `io__inhibit_user_main_predicate' is used to ensure that
	% when the user redefines main_predicate by loading their
	% file into the interpreter and then does an abort, we
	% execute io__main_predicate to invoke the NU-Prolog command loop,
	% rather than executing the users program.

:- dynamic io__exit_on_abort/0.
:- dynamic io__inhibit_user_main_predicate/0.
:- dynamic io__main_has_been_executed/0.

:- dynamic io__save_progname/1.

:- pred main(list(atom)).
:- mode main(in) is det.

main(Args) :-
	( io__exit_on_abort ->
		write('Mercury aborting.\n'),
		exit(1)
	;
		assert(io__exit_on_abort),
		( io__inhibit_user_main_predicate ->
			io__run(Args)
		;
			run(Args)
		),
		exit(0)
	).

:- pred r(string).
:- mode r(in) is det.

r(ArgString) :-
	convert_args(ArgString, Args),
	run(Args).

:- pred convert_args(string, list(atom)).
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

:- pred convert_args_2(string, string, list(atom)).
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
	io__init(Args, ArgStrings),
	io__init_state(IOState0),
	io__call(main_predicate(ArgStrings), IOState0, IOState),
	io__final_state(IOState).

:- pred io__run(list(atom)).
:- mode io__run(in) is det.

io__run(Args) :-
	io__init(Args, ArgStrings),
	io__init_state(IOState0),
	io__call(io__main_predicate(ArgStrings), IOState0, IOState),
	io__final_state(IOState).

:- pred io__init(list(atom), list(string)).
:- mode io__init(in, out) is det.

io__init(Args, ArgStrings) :-
	putprop(io__saved_state, depth, 0),
	atoms_to_strings(Args, ArgStrings),
	( ArgStrings = [Progname | _] ->
		retractall(io__save_progname(_)),
		assert(io__save_progname(Progname))
	;
		true
	).

:- pred atoms_to_strings(list(atom), list(string)).
:- mode atoms_to_strings(in, out) is det.

atoms_to_strings([],[]).
atoms_to_strings([A|As],[S|Ss]) :-
	name(A,S),
	atoms_to_strings(As,Ss).

	% The following definition of main_predicate/3 is a default
	% and will normally be overridden by the users main_predicate/3.
	% It just invokes the NU-Prolog command loop.

:- pred main_predicate(list(string), io__state, io__state).
:- mode main_predicate(in, di, uo) is det.

main_predicate(Args) -->
	io__main_predicate(Args).

:- pred io__main_predicate(list(string), io__state, io__state).
:- mode io__main_predicate(in, di, uo) is det.

io__main_predicate(_) -->
	{ io__main_has_been_executed ->
		true
	;
		write('Mercury Interpreter 0.1\n'),
		assert(io__main_has_been_executed)
	},
	{ retractall(io__inhibit_user_main_predicate) },
	{ assert(io__inhibit_user_main_predicate) },
	{ retractall(io__exit_on_abort) },
	{ $mainloop }.

:- pred io__call(pred).

/******
io__call(Goal, IOState0, IOState) :-
	findall(Goal - IOState1, call(Goal, IOState0, IOState1), Solutions),
	io__call_2(Goal, Solutions, IOState).

:- pred io__call_2(pred, list(_), io__state).
io__call_2(Goal, Solutions, IOState) :-
	(Solutions = [] ->
		write(user_error, '\nio.nl: error: goal `'),
		functor(Goal, F, N),
		write(user_error, F),
		write(user_error, '/'),
		write(user_error, N),
		write(user_error, '\' failed.\n'),
		abort
	; Solutions = [SingleSolution - IOState0] ->
		Goal = SingleSolution,
		IOState = IOState0
	;
		write(user_error, '\nio.nl: error: goal `'),
		functor(Goal, F, N),
		write(user_error, F),
		write(user_error, '/'),
		write(user_error, N),
		write(user_error, '\' not deterministic.\n'),
		abort
	).
*****/

io__call(Goal, IOState0, IOState) :-
	( call(Goal, IOState0, IOState) ->
		true
	;
		write(user_error, '\nio.nl: error: goal `'),
		functor(Goal, F, N),
		write(user_error, F),
		write(user_error, '/'),
		write(user_error, N),
		write(user_error, '\' failed.\n'),
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

io__read_char_code(S, C) -->
	{ get0(S, C) },
	io__update_state.

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
	{ name(FileName, File) },
	( { open(FileName, Mode, Stream0) } ->
		{ Result = 0 },
		{ Stream = Stream0 },
		io__insert_stream_name(Stream, File)
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
	{ lineCount(Stream, LineNumber) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% These predicates are used to enforce correct usage
	% of io__states. io__update_state uses destructive assignment
	% to ensure that once an io state has been used it can't be
	% used again.

:- pred io__init_state(io__state).
io__init_state(io__state(Names, PutBack, Globals, current)) :-
	map__init(PutBack),
	type_to_univ("<globals>", Globals),
	map__init(Names0),
	map__insert(Names0, user_input, "<standard input>", Names1),
	map__insert(Names1, user_output, "<standard output>", Names2),
	map__insert(Names2, user_error, "<standard error>", Names).

:- pred io__update_state(io__state, io__state).
io__update_state(IOState0, IOState) :-
	% using require/2 here causes rampant memory usage
	% because the strings get allocated every time
	( var(IOState0) ->
		error("\nio.nl: I/O predicate called with free io__state")
	;
		true
	),
	%%% ( IOState0 = io__state(_, _, current) ->
	%%% 	true
	%%% ;
	%%% 	error("\nio.nl: cannot retry I/O operation")
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
