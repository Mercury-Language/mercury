%---------------------------------------------------------------------------%
% Copyright (C) 2000-2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Main author: petdr (based on code by conway)
%
% The classic "Dining Philosophers" problem, to show how to use mvars
% to do coroutining.
%
%---------------------------------------------------------------------------%
:- module philo2.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module mvar, spawn.
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
	mvar__init(ForkGlob),
	mvar__put(ForkGlob, forks(yes, yes, yes, yes, yes)),
	spawn(philosopher(plato, ForkGlob)),
	spawn(philosopher(aristotle, ForkGlob)),
	spawn(philosopher(descartes, ForkGlob)),
	spawn(philosopher(russell, ForkGlob)),
	philosopher(sartre, ForkGlob).

:- pred philosopher(philosopher, mvar(forks),
		io__state, io__state).
:- mode philosopher(in, in, di, uo) is cc_multi.

philosopher(Who, ForkGlob) -->
	io__flush_output,
	{ name(Who, Name) },
	io__format("%s is thinking.\n", [s(Name)]),
	rand_sleep(5),
	mvar__take(ForkGlob, Forks0),
	io__format("%s is attempting to eat.\n", [s(Name)]),
	( { forks(Who, Forks0, Forks1) } ->
		mvar__put(ForkGlob, Forks1),
		io__format("%s is eating.\n", [s(Name)]),
		rand_sleep(10),
		mvar__take(ForkGlob, Forks2),
		( { forks(Who, Forks3, Forks2) } ->
			mvar__put(ForkGlob, Forks3)
		;
			{ error("all forked up") }
		)
	;
		% Our 2 forks were not available
		mvar__put(ForkGlob, Forks0)
	),
	philosopher(Who, ForkGlob).

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

:- pred rand_sleep(int::in, io__state::di, io__state::uo) is det.
:- pragma c_code(rand_sleep(Int::in, IO0::di, IO::uo),
		[thread_safe, will_not_call_mercury], "{
#ifdef _MSC_VER
	Sleep(1000 * (rand() % Int));
#else
	sleep((rand() % Int));
#endif
	IO =  IO0;
}").
:- pragma foreign_proc("C#", rand_sleep(Int::in, _IO0::di, _IO::uo),
		[thread_safe, will_not_call_mercury, promise_pure], "{
	System.Threading.Thread.Sleep((new System.Random()).Next(Int) * 1000);
}").
