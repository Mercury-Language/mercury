%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2006-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: philo.m.
% Main author: conway.
%
% The classic "Dining Philosophers" problem, to show how to use the basic
% coroutining primitives.
%
%-----------------------------------------------------------------------------%

:- module philo.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module thread.
:- import_module thread.semaphore.

:- import_module bool.
:- import_module list.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

:- mutable(fork_global, forks, forks(yes, yes, yes, yes, yes), ground,
    [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%

:- type forks
    --->    forks(bool, bool, bool, bool, bool).

:- type philosopher
    --->    plato
    ;       aristotle
    ;       descartes
    ;       russell
    ;       sartre.

main(!IO) :-
    semaphore.new(Lock, !IO),
    semaphore.signal(Lock, !IO),
    spawn(philosopher(plato, Lock), !IO),
    spawn(philosopher(aristotle, Lock), !IO),
    spawn(philosopher(descartes, Lock), !IO),
    spawn(philosopher(russell, Lock), !IO),
    philosopher(sartre, Lock, !IO).

:- pred philosopher(philosopher::in, semaphore::in, io::di, io::uo) is cc_multi.

philosopher(Who, Lock, !IO) :-
    name(Who, Name),
    io.format("%s is thinking.\n", [s(Name)], !IO),
    semaphore.wait(Lock, !IO),
    get_fork_global(Forks0, !IO), 
    ( forks(Who, Forks0, Forks1) ->
        set_fork_global(Forks1, !IO),
        semaphore.signal(Lock, !IO),
        io.format("%s is eating.\n", [s(Name)], !IO),
        semaphore.wait(Lock, !IO),
        get_fork_global(Forks2, !IO),
        ( forks(Who, Forks3, Forks2) ->
            set_fork_global(Forks3, !IO),
            semaphore.signal(Lock, !IO)
        ;
            error("all forked up")
        )
    ;
        % Our 2 forks were not available
        signal(Lock, !IO)
    ),
    philosopher(Who, Lock, !IO).

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

%-----------------------------------------------------------------------------%
:- end_module philo.
%-----------------------------------------------------------------------------%
