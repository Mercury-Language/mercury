%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: sp_lib.nl:
% Main author: fjh.
%
% This file is for Sicstus Prolog compatibility.
%
%-----------------------------------------------------------------------------%

?- prompt(_, '').	% turn off that silly '|:' interactive input prompt.

% Translate various NU-Prolog-isms into Sicstus Prolog.

nuprolog :-
	fail.

some(_Vars, Goal) :-
	call(Goal).

not(Goal) :-
	\+ Goal.

all(Vars, Goal) :-
	not some(Vars, not Goal).

(P => Q) :-
	not (P, not Q).

(P <= Q) :-
	Q => P.

(P <=> Q) :-
	(P => Q), (Q => P).

putprop(Atom, Key, Property) :-
	retractall(property(Atom, Key, _)),
	assert(property(Atom, Key, Property)).
getprop(Atom, Key, Property) :-
	property(Atom, Key, Property).
remprop(Atom, Key) :-
	retractall(property(Atom, Key, _Property)).

currentInput(X) :-
	current_input(X).

currentOutput(X) :-
	current_output(X).

flushOutput(X) :-
	flush_output(X).

setInput(X) :-
	set_input(X).

setOutput(X) :-
	set_output(X).

lineCount(X,Y1) :-
	line_count(X,Y),
	Y1 is Y + 1.

eof(end_of_file).

member(Element, List, SubList) :-
	SubList = [Element | _],
	append(_, SubList, List).

system(Command, Status) :-
	atom_chars(Com, Command),
	( sicstus3 ->
		use_module(library(system), []),
		system:system(Com, Status)
	;
		unix(system(Com, Status))
	).

	% test whether we are running version 3 of SICStus or not
sicstus3 :-
	% there is probably a more elegant way of doing this, but the
	% following test seems to do the trick - it fails with SICStus 2.x
	% but succeeds with SICStus 3.x
	prolog_flag(argv, _).

portray(Stream, Term) :-
	currentOutput(S),
	setOutput(Stream),
	( portray(Term) -> true ; print(Term) ),
	setOutput(S).

intToString(I, S) :-
	number_chars(I, S).

string__to_float(String, Float) :-
	number_chars(Float, String).

duplicate(Term, Copy) :-
	copy_term(Term, Copy).

%-----------------------------------------------------------------------------%

% Sheesh - do I really have to define these myself!

member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

append([], X, X).
append([X|Xs], Ys, [X|Zs]) :-
	append(Xs, Ys, Zs).

A \= B :- \+ A = B.

	% define =/3 for DCGs
=(A, A, A).

%-----------------------------------------------------------------------------%

error(Message) :-
	format("Software Error: ~s\n", [Message]),
	atom_chars(Msg, Message),
	raise_exception(software_error(Msg)).

%-----------------------------------------------------------------------------%

% bimap__search depends on reordering

bimap__search(bimap(O, C), K, V) :-
	( nonvar(K) ->
		map__search(O, K, V),
		map__search(C, V, K)
	; nonvar(V) ->
		map__search(C, V, K),
		map__search(O, K, V)
	;
		error("bimap__search")
	).

%-----------------------------------------------------------------------------%

call(Goal0, A) :-
	Goal0 =.. L0,
	append(L0, [A], L),
	Goal =.. L,
	call(Goal).
call(Goal0, A, B) :-
	Goal0 =.. L0,
	append(L0, [A, B], L),
	Goal =.. L,
	call(Goal).
call(Goal0, A, B, C) :-
	Goal0 =.. L0,
	append(L0, [A, B, C], L),
	Goal =.. L,
	call(Goal).
call(Goal0, A, B, C, D) :-
	Goal0 =.. L0,
	append(L0, [A, B, C, D], L),
	Goal =.. L,
	call(Goal).
call(Goal0, A, B, C, D, E) :-
	Goal0 =.. L0,
	append(L0, [A, B, C, D, E], L),
	Goal =.. L,
	call(Goal).
call(Goal0, A, B, C, D, E, F) :-
	Goal0 =.. L0,
	append(L0, [A, B, C, D, E, F], L),
	Goal =.. L,
	call(Goal).
call(Goal0, A, B, C, D, E, F, G) :-
	Goal0 =.. L0,
	append(L0, [A, B, C, D, E, F, G], L),
	Goal =.. L,
	call(Goal).
call(Goal0, A, B, C, D, E, F, G, H) :-
	Goal0 =.. L0,
	append(L0, [A, B, C, D, E, F, G, H], L),
	Goal =.. L,
	call(Goal).
call(Goal0, A, B, C, D, E, F, G, H, I) :-
	Goal0 =.. L0,
	append(L0, [A, B, C, D, E, F, G, H, I], L),
	Goal =.. L,
	call(Goal).
call(Goal0, A, B, C, D, E, F, G, H, I, J) :-
	Goal0 =.. L0,
	append(L0, [A, B, C, D, E, F, G, H, I, J], L),
	Goal =.. L,
	call(Goal).

char__to_int(C, N) :-
	atom_chars(C, [N]).

char__max_char_value(255).

% This predicate is defined in C in lexer.m
lexer__rev_char_list_to_string(RevChars, String) :-
	list__reverse(RevChars, Chars),
	string__from_char_list(Chars, String).

string__contains_char(String, Char) :-
	string__to_char_list(String, CharList),
	member(Char, CharList),
	!.

report_stats :-
	statistics(global_stack, [Heap,_]),
	statistics(program, [Program,_]),
	statistics(memory, [TotalMemory,_]),
	statistics(runtime, [Time,_]),
	TimeInSeconds is Time / 1000.0,
	format(user_error,
		"[Heap ~3dk, Program ~3dk, Total ~3dk, Time ~3f]
",
		[Heap, Program, TotalMemory, TimeInSeconds]),
	fail ; true.

io__gc_call(X) -->
	io__call(X).
	% { garbage_collect }.

%-----------------------------------------------------------------------------%

% Use the Mercury parser rather than the Prolog one.

term_io__read_term(Result) -->
	parser__read_term(Result).

%-----------------------------------------------------------------------------%
