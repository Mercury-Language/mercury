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
	{ io__call(Goal) }.

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
	atoms_to_strings(Args,ArgStrings),
	save_progname(ArgStrings),
	io__call(main_predicate(ArgStrings)).

:- pred io__call(pred).
io__call(Goal) :-
	io__init_state(IOState0),
	findall(Goal, ( call(Goal, IOState0, IOState1),
			io__final_state(IOState1) ), Solutions),
	io__call_2(Goal, Solutions).

:- pred io__call_2(pred, list(_)).
io__call_2(Goal, Solutions) :-
	(Solutions = [] ->
		write('\nio.nl: error: goal "'),
		print(Goal),
		write('." failed.\n')
	; Solutions = [SingleSolution] ->
		Goal = SingleSolution
	;
		write('\nio.nl: error: goal "'),
		print(Goal),
		write('." not deterministic.\n')
	).

:- pred atoms_to_strings(list(atom), list(string)).
atoms_to_strings([],[]).
atoms_to_strings(A.As,S.Ss) :-
	name(A,S),
	atoms_to_strings(As,Ss).

% ?- dynamic(io__progname/1).
% save_progname(Progname._) :-
% 	assert(io__progname(Progname, X, X)).

	% !! this is wrong, but is necessary to avoid bugs in nit & NU-Prolog.
save_progname(_).
io__progname("<argv[0] not available>", X, X).
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- io__get_line_number(_, IO0, _) when IO0.
io__get_line_number(LineNumber) -->
	{ currentInput(Stream) },
	{ lineCount(Stream, LineNumber) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Declarative versions of Prolog's see/1 and seen/0.

:- io__see(File, _, IO0, _) when ground(File) and IO0.
io__see(File, Result) -->
	{
		name(FileName, File),
		(if see(FileName) then
			Result = ok
		else
			Result = error
		)
	},
	io__update_state.

:- io__seen(IO0, _) when IO0.
io__seen -->
	{ seen },
	io__update_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Declarative versions of Prolog's tell/1 and told/0.

:- io__tell(File, _, IO0, _) when ground(File) and IO0.
io__tell(File, Result) -->
	{
		name(FileName, File),
		(if tell(FileName) then
			Result = ok
		else
			Result = error
		)
	},
	io__update_state.

:- io__told(IO0, _) when IO0.
io__told -->
	{ told },
	io__update_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- io__write_int(I, IO0, _) when I and IO0.
io__write_int(I) -->
	{ write(I) },
	io__update_state.

%-----------------------------------------------------------------------------%

:- io__write_char(C, IO0, _) when C and IO0.
io__write_char(C) -->
	{ write(C) },
	io__update_state.

%-----------------------------------------------------------------------------%

:- io__write_float(F, IO0, _) when F and IO0.
io__write_float(F) -->
	{ write(F) },
	io__update_state.

%-----------------------------------------------------------------------------%

:- io__write_string(S, IO0, _) when ground(S) and IO0.
io__write_string(S) -->
	{ write_string(S) },
	io__update_state.
	
write_string(S) :-
	format("~s",[S]).

%-----------------------------------------------------------------------------%

:- io__write_anything(I, IO0, _) when I and IO0.
io__write_anything(I) -->
	{ write(I) },
	io__update_state.

%-----------------------------------------------------------------------------%

io__flush_output -->
	{ 
	  currentOutput(Stream),
	  flushOutput(Stream)
	},
	io__update_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% These predicates are used to enforce correct usage
	% of io__states. io__update_state uses destructive assignment
	% to ensure that once an io state has been used it can't be
	% used again.

:- pred io__init_state(io__state).
io__init_state(io__state(current)).

:- pred io__update_state(io__state, io__state).
io__update_state(IOState0, IOState) :-
	require(IOState0 = io__state(current),
		"\nio.nl: cannot retry I/O operation"),
	$replacn(1, IOState0, old),
	IOState = io__state(current).

:- pred io__final_state(io__state).
io__final_state(IOState) :-
	io__update_state(IOState, _).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
