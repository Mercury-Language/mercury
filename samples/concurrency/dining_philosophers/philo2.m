%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2003, 2006-2007, 2010 The University of Melbourne.
% Copyright (C) 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: philo2.m.
% Main author: petdr (based on code by conway)
%
% The classic "Dining Philosophers" problem, to show how to use mvars
% to do coroutining.
%
%-----------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module philo2.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma require_feature_set([concurrency]).

:- import_module thread.
:- import_module thread.mvar.

:- import_module bool.
:- import_module list.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- type forks
    --->    forks(bool, bool, bool, bool, bool).

:- type philosopher
    --->    plato
    ;       aristotle
    ;       descartes
    ;       russell
    ;       sartre.

main(!IO) :-
    mvar.init(ForkGlob, !IO),
    mvar.put(ForkGlob, forks(yes, yes, yes, yes, yes), !IO),
    spawn(philosopher(plato, ForkGlob), !IO),
    spawn(philosopher(aristotle, ForkGlob), !IO),
    spawn(philosopher(descartes, ForkGlob), !IO),
    spawn(philosopher(russell, ForkGlob), !IO),
    philosopher(sartre, ForkGlob, !IO).

:- pred philosopher(philosopher::in, mvar(forks)::in, io::di, io::uo)
    is cc_multi.

philosopher(Who, ForkGlob, !IO) :-
    io.flush_output(!IO),
    name(Who, Name),
    io.format("%s is thinking.\n", [s(Name)], !IO),
    rand_sleep(5, !IO),
    mvar.take(ForkGlob, Forks0, !IO),
    io.format("%s is attempting to eat.\n", [s(Name)], !IO),
    ( if forks(Who, Forks0, Forks1) then
        mvar.put(ForkGlob, Forks1, !IO),
        io.format("%s is eating.\n", [s(Name)], !IO),
        rand_sleep(10, !IO),
        mvar.take(ForkGlob, Forks2, !IO),
        ( if forks(Who, Forks3, Forks2) then
            mvar.put(ForkGlob, Forks3, !IO)
        else
            error("all forked up")
        )
    else
        % Our 2 forks were not available
        mvar.put(ForkGlob, Forks0, !IO)
    ),
    philosopher(Who, ForkGlob, !IO).

:- pred forks(philosopher, forks, forks).
:- mode forks(in, in, out) is semidet.
:- mode forks(in, out, in) is semidet.

forks(plato,        forks(yes, yes, C, D, E), forks(no, no, C, D, E)).
forks(aristotle,    forks(A, yes, yes, D, E), forks(A, no, no, D, E)).
forks(descartes,    forks(A, B, yes, yes, E), forks(A, B, no, no, E)).
forks(russell,      forks(A, B, C, yes, yes), forks(A, B, C, no, no)).
forks(sartre,       forks(yes, B, C, D, yes), forks(no, B, C, D, no)).

:- pred name(philosopher::in, string::out) is det.

name(plato,     "Plato").
name(aristotle, "Aristotle").
name(descartes, "Descartes").
name(russell,   "Russell").
name(sartre,    "Sartre").

%---------------------------------------------------------------------------%

:- pragma foreign_code("C#", "
    public static System.Random rng = new System.Random();
").

:- pragma foreign_decl("Java", "

import java.util.Random;

").

:- pragma foreign_code("Java", "
    public static Random rng = new Random();
").

:- pred rand_sleep(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    rand_sleep(Int::in, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, will_not_call_mercury],
"
#if defined(MR_WIN32)
    Sleep(1000 * (rand() % Int));
#else
    sleep((rand() % Int));
#endif
").

:- pragma foreign_proc("C#",
    rand_sleep(Int::in, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    System.Threading.Thread.Sleep(rng.Next(Int) * 1000);
").

:- pragma foreign_proc("Java",
    rand_sleep(Int::in, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    try {
        Thread.sleep(rng.nextInt(Int) * 1000);
    } catch ( InterruptedException e ) {
        /* Just return if we are interrupted.*/
    }
").

%---------------------------------------------------------------------------%
:- end_module philo2.
%---------------------------------------------------------------------------%
