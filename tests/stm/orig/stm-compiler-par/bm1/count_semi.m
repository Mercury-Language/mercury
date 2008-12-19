%------------------------------------------------------------------------------%
% count_stm.m
% <lmika@csse.unimelb.edu.au>
% Thu Oct 18 00:11:45 EST 2007
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
%------------------------------------------------------------------------------%

:- module count_semi.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module thread.
:- import_module exception.
:- import_module univ.
:- import_module string.
:- import_module list.
:- import_module int.
:- import_module thread.mvar.
:- import_module thread.semaphore.

:- type counter == mvar(int).

%------------------------------------------------------------------------------%

main(!IO) :-
    command_line_arguments(Args, !IO),
    ( Args = [NStr] ->
        N = string.det_to_int(NStr),
        make_new_counter(Semaphore, Counter, !IO),
        spawn_n_threads(N, add_1_atomically(Semaphore, Counter), !IO)
    ;
        io.write_string("Usage: count [threadcnt]\n", !IO)
    ).

%------------------------------------------------------------------------------%
%
:- pred spawn_n_threads(int::in, 
    (pred(io, io))::in(pred(di, uo) is cc_multi), 
    io::di, io::uo) is cc_multi.

spawn_n_threads(N, Closure, !IO) :-
    ( N > 0 ->
        thread.spawn(Closure, !IO),
        N1 = N - 1,
        spawn_n_threads(N1, Closure, !IO)
    ;
        true
    ).

%------------------------------------------------------------------------------%

% The counting operation must be completly atomic.  Therefore, it is protected
% by a single semaphore.

:- pred make_new_counter(semaphore::out, counter::out, io::di, io::uo) is det.

make_new_counter(Semaphore, Counter, !IO) :-
    semaphore.new(Semaphore, !IO),
    semaphore.signal(Semaphore, !IO),
    mvar.init(Counter, !IO),
    mvar.put(Counter, 0, !IO).


:- pred add_1_atomically(semaphore::in, counter::in, io::di, io::uo) 
    is cc_multi.

add_1_atomically(Semaphore, Counter, IO0, IO) :-
    semaphore.wait(Semaphore, IO0, IO1),
    mvar.take(Counter, V0, IO1, IO2),
    V = V0 + 1,
    mvar.put(Counter, V, IO2, IO3),
    semaphore.signal(Semaphore, IO3, IO).

