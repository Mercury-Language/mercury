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
	io__call(Goal),
	io__update_state.

%-----------------------------------------------------------------------------%

	% When we enter main/1, we check whether this is because
	% we have just restarted after a call to abort/0, and if so
	% we halt.  (abort/0 is called by error/1; see the
	% NU-Prolog manual entry for abort/0.)

:- dynamic main_started/0.

:- pred main(list(atom)).
main(Args) :-
	( main_started ->
		exit(1)
	;
		assert(main_started),
		run(Args)
	).

:- pred run(list(atom)).
run(Args) :-
	atoms_to_strings(Args, ArgStrings),
	io__init_state(IOState0),
	io__call(main_predicate(ArgStrings), IOState0, IOState),
	io__final_state(IOState).

:- pred io__call(pred).
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

:- pred atoms_to_strings(list(atom), list(string)).
atoms_to_strings([],[]).
atoms_to_strings(A.As,S.Ss) :-
	name(A,S),
	atoms_to_strings(As,Ss).

	% XXX A NU-Prolog bug means that the program name is
	% not available, so we have to use the default name.

io__progname(DefaultName, DefaultName) --> [].
	
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
	{ tell(NewStream) },
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
:- mode io__delete_stream_name(input, di, uo).

io__delete_stream_name(Stream, io__state(StreamNames0, S),
		io__state(StreamNames, S)) :-
	map__delete(StreamNames0, Stream, StreamNames).

:- pred io__insert_stream_name(io__stream, string, io__state, io__state).
:- mode io__insert_stream_name(input, string, di, uo).

io__insert_stream_name(Stream, Name, io__state(StreamNames0, S),
		io__state(StreamNames, S)) :-
	map__insert(StreamNames0, Stream, Name, StreamNames).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

io__write_int(S, I) -->
	{ write(S, I) },
	io__update_state.

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
io__init_state(io__state(Names, current)) :-
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
	%%% ( IOState0 = io__state(_, current) ->
	%%% 	true
	%%% ;
	%%% 	error("\nio.nl: cannot retry I/O operation")
	%%% ),
	%%% IOState0 = io__state(Names, _),
	%%% $replacn(2, IOState0, old),
	%%% IOState = io__state(Names, current).
	IOState = IOState0.

:- pred io__final_state(io__state).
io__final_state(IOState) :-
	io__update_state(IOState, _).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
