%------------------------------------------------------------------------------%
% philo3.m
% Copyright (C) 2001-2002 Ralph Becket <rbeck@microsoft.com>
% Mon May 14 14:32:29 BST 2001
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%
% RELEASED TO THE MERCURY PROJECT FOR DISTRIBUTION UNDER
% WHATEVER LICENCE IS DEEMED APPROPRIATE BY THE PROJECT
% MANAGEMENT.
%
% The dining philosophers using semaphores.  The philosophers
% acquire forks such that even numbered philosophers pick up
% left then right whereas odd numbered philosophers pick up
% right then left.  This is guaranteed not to lead to deadlock.
%
%------------------------------------------------------------------------------%

:- module philo3.

:- interface.

:- import_module io.



:- pred main(io__state::di, io__state::uo) is cc_multi.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module string, list.
:- import_module semaphore, spawn.

%------------------------------------------------------------------------------%

main -->
    semaphore__new(Fork0), semaphore__signal(Fork0),
    semaphore__new(Fork1), semaphore__signal(Fork1),
    semaphore__new(Fork2), semaphore__signal(Fork2),
    semaphore__new(Fork3), semaphore__signal(Fork3),
    semaphore__new(Fork4), semaphore__signal(Fork4),
    spawn(philosopher("Plato",      0, Fork0, 1, Fork1)),
    spawn(philosopher("Aristotle",  2, Fork2, 1, Fork1)),
    spawn(philosopher("Descartes",  2, Fork2, 3, Fork3)),
    spawn(philosopher("Calvin",     4, Fork4, 3, Fork3)),
          philosopher("Hobbes",     4, Fork4, 0, Fork0).

%------------------------------------------------------------------------------%

:- pred philosopher(string,int,semaphore,int,semaphore,io__state,io__state).
:- mode philosopher(in, in, in, in, in, di, uo) is cc_multi.

philosopher(Name, A, ForkA, B, ForkB) -->

    io__format("%s is thinking\n", [s(Name)]),
    yield,
    rand_sleep(10),

    semaphore__wait(ForkA),
    io__format("%s has acquired fork %d\n", [s(Name), i(A)]),
    semaphore__wait(ForkB),
    io__format("%s has acquired fork %d\n", [s(Name), i(B)]),

    io__format("%s is eating\n", [s(Name)]),
    yield,
    rand_sleep(5),

    io__format("%s relinquishes fork %d\n", [s(Name), i(B)]),
    semaphore__signal(ForkB),
    io__format("%s relinquishes fork %d\n", [s(Name), i(A)]),
    semaphore__signal(ForkA),

    philosopher(Name, A, ForkA, B, ForkB).

%------------------------------------------------------------------------------%

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

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
