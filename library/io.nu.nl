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

	% When we enter main/1, we check whether this is because
	% we have just restarted after a call to abort/0, and if so
	% we halt.  (abort/0 is called by error/1; see the
	% NU-Prolog manual entry for abort/0.)

:- dynamic io__main_started/0.
:- dynamic io__save_progname/1.

:- pred main(list(atom)).
:- mode main(in) is det.
main(Args) :-
	( io__main_started ->
		exit(1)
	;
		assert(io__main_started),
		run(Args),
		exit(0)
	).


:- pred run(list(atom)).
:- mode run(in) is det.
run(Args) :-
	putprop(io__saved_state, depth, 0),
	atoms_to_strings(Args, ArgStrings),
	( ArgStrings = [Progname | _] ->
		retractall(io__save_progname(_)),
		assert(io__save_progname(Progname))
	;
		true
	),
	io__init_state(IOState0),
	io__call(main_predicate(ArgStrings), IOState0, IOState),
	io__final_state(IOState).

	% The following definition of main_predicate/3 is a default
	% and will normally be overridden by the users main_predicate/3.
	% It just invokes the NU-Prolog command loop.

:- pred main_predicate(list(string), io__state, io__state).
:- mode main_predicate(in, di, uo) is det.

main_predicate(_) -->
	{ retractall(io__main_started) },	% Fix handling of `abort'.
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

:- pred atoms_to_strings(list(atom), list(string)).
atoms_to_strings([],[]).
atoms_to_strings([A|As],[S|Ss]) :-
	name(A,S),
	atoms_to_strings(As,Ss).

io__progname(DefaultName, Name) --> 
	{ io__save_progname(N) ->
		Name0 = N
	;
		Name0 = DefaultName
	},
	{ dir__basename(Name0, Name) }.
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

io__get_line_number(LineNumber) -->
	{ currentInput(Stream) },
	{ lineCount(Stream, LineNumber) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

io__input_stream(Stream) -->
	{ currentInput(Stream) }.

io__output_stream(Stream) -->
	{ currentOutput(Stream) }.

io__set_input_stream(NewStream, OldStream) -->
	{ currentInput(OldStream) },
	{ see(NewStream) },
	io__update_state.

io__set_output_stream(NewStream, OldStream) -->
	{ currentOutput(OldStream) },
	{ setOutput(NewStream) },
	io__update_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Declarative versions of Prolog's see/1 and seen/0.

io__see(File, Result) -->
	{ name(FileName, File) },
	( { see(FileName) } ->
		{ Result = ok },
		io__input_stream(Stream),
		io__insert_stream_name(Stream, File)
	;
		{ Result = error }
	),
	io__update_state.

io__seen -->
	io__input_stream(Stream),
	io__delete_stream_name(Stream),
	{ seen },
	io__update_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Declarative versions of Prolog's tell/1 and told/0.

io__tell(File, Result) -->
	{ name(FileName, File) },
	( { tell(FileName) } ->
		{ Result = ok },
		io__output_stream(Stream),
		io__insert_stream_name(Stream, File)
	;
		{ Result = error }
	),
	io__update_state.

io__told -->
	io__output_stream(Stream),
	io__delete_stream_name(Stream),
	{ told },
	io__update_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred io__delete_stream_name(io__stream, io__state, io__state).
:- mode io__delete_stream_name(in, di, uo).

io__delete_stream_name(Stream, io__state(StreamNames0, Globals, S),
		io__state(StreamNames, Globals, S)) :-
	map__delete(StreamNames0, Stream, StreamNames).

:- pred io__insert_stream_name(io__stream, string, io__state, io__state).
:- mode io__insert_stream_name(in, string, di, uo).

io__insert_stream_name(Stream, Name, io__state(StreamNames0, Globals, S),
		io__state(StreamNames, Globals, S)) :-
	map__insert(StreamNames0, Stream, Name, StreamNames).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

io__write_int(S, I) -->
	{ write(S, I) },
	io__update_state.

%-----------------------------------------------------------------------------%

io__read_char(S, C, R) -->
	{ get0(S, C0) },
	(
		{ C0 = -1 }
	->
		{ R = eof, C = 1 }
	;
		{ R = ok, char_to_int(C, C0) }
	),
	io__update_state.

%-----------------------------------------------------------------------------%

io__read_line(S, Str, R) -->
	io__read_char(S, C, R0),
	(
		{ R0 = ok }
	->
		(
			{ C = '\n' }
		->
			{ Str = ['\n'] },
			{ R = R0 }
		;
			{ Str = [C|Str0] },
			io__read_line(S, Str0, R)
		)
	;
		{ Str = [] },
		{ R = R0 }
	).

%-----------------------------------------------------------------------------%

io__write_char(S, C) -->
	{ write(S, C) },
	io__update_state.

%-----------------------------------------------------------------------------%

io__write_float(S, F) -->
	{ write(S, F) },
	io__update_state.

%-----------------------------------------------------------------------------%

io__write_string(Stream, String) -->
	{ (format(Stream, "~s", [String]), fail ; true) },
	io__update_state.
	
%-----------------------------------------------------------------------------%

io__write_anything(S, I) -->
	{ write(S, I) },
	io__update_state.

%-----------------------------------------------------------------------------%

io__flush_output(Stream) -->
	{ flushOutput(Stream) },
	io__update_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% These predicates are used to enforce correct usage
	% of io__states. io__update_state uses destructive assignment
	% to ensure that once an io state has been used it can't be
	% used again.

:- pred io__init_state(io__state).
io__init_state(io__state(Names, Globals, current)) :-
	map__init(Names0),
	type_to_univ("<globals>", Globals),
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

:- pred io__get_globals(univ, io__state, io__state).
:- mode io__get_globals(out, di, uo).

io__get_globals(Globals, IOState, IOState) :-
	IOState = io__state(_StreamNames, Globals, _S).

:- pred io__set_globals(univ, io__state, io__state).
:- mode io__set_globals(in, di, uo).

io__set_globals(Globals, io__state(StreamNames, _, S),
		io__state(StreamNames, Globals, S)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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

io__call_system(Command, Result) -->
	{ system(Command, Status) ->
		Result = ok(Status)
	;
		Result = error
	},
	io__update_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
