%------------------------------------------------------------------------------%
% count_stm.m
% <lmika@csse.unimelb.edu.au>
% Thu Oct 18 00:11:45 EST 2007
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
%------------------------------------------------------------------------------%

:- module count_stm.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module thread.
:- import_module stm_builtin.
:- import_module exception.
:- import_module univ.
:- import_module string.
:- import_module list.
:- import_module int.

:- type counter == stm_var(int).

%------------------------------------------------------------------------------%

main(!IO) :-
    command_line_arguments(Args, !IO),
    ( Args = [NStr] ->
        N = string.det_to_int(NStr),
        make_new_counter(Counter, !IO),
        spawn_n_threads(N, add_1_atomically(Counter), !IO)
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

:- pred make_new_counter(counter::out, io::di, io::uo) is det.

make_new_counter(Counter, !IO) :-
    new_stm_var(0, Counter, !IO).


:- pred add_1_atomically(counter::in, io::di, io::uo) is cc_multi.

add_1_atomically(Counter, IO0, IO) :-
    atomic [outer(IO0, IO), inner(STM0, STM)] (
        read_stm_var(Counter, V0, STM0, STM1),
        V = V0 + 1,
        write_stm_var(Counter, V, STM1, STM)
    ).

