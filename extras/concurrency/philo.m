%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Main author: conway
%
% The classic "Dining Philosophers" problem, to show how to use the basic
% coroutining primitives.
%
%---------------------------------------------------------------------------%
:- module philo.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module spawn, global, semaphore.
:- import_module bool, list, require, string.

:- type forks
	--->	forks(bool, bool, bool, bool, bool).

:- type philosopher
	--->	plato
	;	aristotle
	;	descartes
	;	russell
	;	sartre
	.

main -->
	new(Lock), signal(Lock),
	new(forks(yes, yes, yes, yes, yes), ForkGlob),
	spawn(philosopher(plato, Lock, ForkGlob)),
	spawn(philosopher(aristotle, Lock, ForkGlob)),
	spawn(philosopher(descartes, Lock, ForkGlob)),
	spawn(philosopher(russell, Lock, ForkGlob)),
	philosopher(sartre, Lock, ForkGlob).

:- pred philosopher(philosopher, semaphore, global(forks),
		io__state, io__state).
:- mode philosopher(in, in, in, di, uo) is cc_multi.

philosopher(Who, Lock, ForkGlob) -->
	{ name(Who, Name) },
	io__format("%s is thinking.\n", [s(Name)]),
	wait(Lock),
	get(ForkGlob, Forks0),
	( { forks(Who, Forks0, Forks1) } ->
		set(ForkGlob, Forks1),
		signal(Lock),
		io__format("%s is eating.\n", [s(Name)]),
		wait(Lock),
		get(ForkGlob, Forks2),
		( { forks(Who, Forks3, Forks2) } ->
			set(ForkGlob, Forks3),
			signal(Lock)
		;
			{ error("all forked up") }
		)
	;
		% Our 2 forks were not available
		signal(Lock)
	),
	philosopher(Who, Lock, ForkGlob).

:- pred forks(philosopher, forks, forks).
:- mode forks(in, in, out) is semidet.
:- mode forks(in, out, in) is semidet.

forks(plato,		forks(yes, yes, C, D, E), forks(no, no, C, D, E)).
forks(aristotle,	forks(A, yes, yes, D, E), forks(A, no, no, D, E)).
forks(descartes,	forks(A, B, yes, yes, E), forks(A, B, no, no, E)).
forks(russell,		forks(A, B, C, yes, yes), forks(A, B, C, no, no)).
forks(sartre,		forks(yes, B, C, D, yes), forks(no, B, C, D, no)).

:- pred name(philosopher, string).
:- mode name(in, out) is det.

name(plato	, "Plato").
name(aristotle	, "Aristotle").
name(descartes	, "Descartes").
name(russell	, "Russell").
name(sartre	, "Sartre").

