%-----------------------------------------------------------------------------%
%
% File: portray.nl
% Main author: fjh.
%
% This file contains a definition for portray/1 that only
% displays the top-level functors of big terms.
% This is useful for debugging, since otherwise the NU-Prolog
% debugger prints out screenfuls of crap as you step through
% a program.  It also contains a definition for spyHook/2 that displays
% the term in full, so that you can get the full details by
% typing "|" in the debugger.
% Note that you need to compile it to a .no file -
% loading it as a .np file doesn't work.
%
%-----------------------------------------------------------------------------%

portray(Term) :-
	compound(Term),
	Term =.. [F|As],
	write(F),
	write('('),
	portray_args(As),
	write(')').

portray_args([]).
portray_args([X|Xs]) :-
	portray2(X),
	portray_args_2(Xs).

portray_args_2([]).
portray_args_2([X|Xs]) :-
	write(', '),
	portray2(X),
	portray_args_2(Xs).

portray2(Term) :-
	( compound(Term) ->
		functor(Term,F,N),
		format("<~a/~d>", [F,N])
	;
		write(Term)
	).

spyHook(_, Term) :-
	interactive_display(1, Term).

interactive_display(Depth, Term) :-
	flushOutput(user_output),
	flushOutput(user_error),
	( portray(Term) ->
		true
	;
		write('<<'),
		write(Term),
		write('>>')
	),
	nl,
	flushOutput(user_output),
	flushOutput(user_error),
	( nonvar(Term) ->
		write(user_error, Depth),
		write(user_error, '> select arg to display (h for help): '),
		flushOutput(user_error),
		read(user_input, Num),
		( Num = 'a' ->
			write(user_error, Term),
			writeln(user_error, '.')
		; Num = 'e' ->
			fail
		; Num = 'r' ->
			true
		; Num = 0 ->
			true
		; Num = 'h' ->
			write('h = help'), nl,
			write('r = return (1 level)'), nl,
			write('e = exit (all levels)'), nl,
			write('a = display all'), nl,
			write('<number> = display nth argument'), nl,
			nl,
			flushOutput(user_output),
			flushOutput(user_error),
			interactive_display(Depth, Term)
		; arg(Num, Term, Arg) ->
			Depth1 is Depth + 1,
			interactive_display(Depth1, Arg),
			interactive_display(Depth, Term)
		;
			write('Invalid response'), nl,
			flushOutput(user_output),
			flushOutput(user_error),
			interactive_display(Depth, Term)
		)
	;
		true
	).

%-----------------------------------------------------------------------------%
