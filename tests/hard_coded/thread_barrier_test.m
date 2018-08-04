%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% File: thread_barrier_test.m
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Tue Apr  8 15:54:57 CEST 2014
%---------------------------------------------------------------------------%

:- module thread_barrier_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module thread.
:- import_module thread.barrier.
:- import_module thread.mvar.

:- import_module thread_test_utils.

%---------------------------------------------------------------------------%

:- func fib(integer) = integer.

fib(N) = Fib :-
    ( N < integer(2) -> Fib = integer(1)
    ; Fib = fib(N-integer(1)) + fib(N-integer(2))
    ).

:- pred test_spawn_and_wait(int::in, io::di, io::uo) is cc_multi.

test_spawn_and_wait(ThreadCount, !IO) :-
    init_all_thread_output(AllThreadOutput, !IO),
    init_thread_output(AllThreadOutput, 0, Output, !IO),
    t_write_string(Output, format("-- testing spawning with %d threads",
        [i(ThreadCount)]), !IO),
    barrier.init(ThreadCount + 1, Barrier, !IO),
    list.foldl((pred(Thread::in, !.IO::di, !:IO::uo) is cc_multi :-
        t_write_string(Output, format("spawning thread #%d", [i(Thread)]),
            !IO),
        spawn(test_spawn_and_wait_thread(Thread, AllThreadOutput, Barrier),
            !IO)
    ), 1 `..` ThreadCount, !IO),
    barrier.wait(Barrier, !IO),
    t_write_string(Output, "-- test finished", !IO),
    close_thread_output(Output, !IO),
    write_all_thread_output(AllThreadOutput, !IO).

:- pragma no_determinism_warning(test_spawn_and_wait_thread/5).
:- pred test_spawn_and_wait_thread(int::in, all_threads_output::in,
    barrier::in, io::di, io::uo) is cc_multi.

test_spawn_and_wait_thread(Thread, AllThreadOutput, Barrier, !IO) :-
    init_thread_output(AllThreadOutput, Thread, Output, !IO),
    t_write_string(Output, format("thread %d starting", [i(Thread)]), !IO),
    N = 5 + Thread * 5,
    t_write_string(Output, format("fib(%d) = %s",
        [i(N), s(integer.to_string(fib(integer(N))))]), !IO),
    barrier.wait(Barrier, !IO),
    t_write_string(Output, format("thread %d exiting", [i(Thread)]), !IO),
    close_thread_output(Output, !IO).

    % This state allows us to determine if certain actions have already
    % taken place.  This lets us show that some things happen before/after
    % release is called on the barrier.
    %
:- type state
    --->    state_before_release
    ;       state_after_release.

:- pred test_release(int::in, int::in, io::di, io::uo) is cc_multi.

test_release(AbortAt, ThreadCount, !IO) :-
    init_all_thread_output(AllThreadOutput, !IO),
    init_thread_output(AllThreadOutput, 0, Output, !IO),
    t_write_string(Output, format("-- testing barrier release at %d of %d",
        [i(AbortAt), i(ThreadCount)]), !IO),
    barrier.init(ThreadCount + 1, Barrier, !IO),
    mvar.init(StateMvar, !IO),
    mvar.put(StateMvar, state_before_release, !IO),
    list.foldl((pred(Thread::in, !.IO::di, !:IO::uo) is cc_multi :-
        t_write_string(Output, format("spawning thread #%d", [i(Thread)]),
            !IO),
        spawn(
            release_thread(AllThreadOutput, Thread, AbortAt, Barrier,
                StateMvar),
            !IO)
    ), 1 `..` ThreadCount, !IO),
    % There is no guarantee that we will reach this point before the AbortAt
    % thread releases the barrier, so don't log the state as expected.
    t_write_string(Output, "waiting", !IO),
    barrier.wait(Barrier, !IO),
    log_with_state(Output, StateMvar, "done waiting, test finished", !IO),
    close_thread_output(Output, !IO),
    write_all_thread_output(AllThreadOutput, !IO).

:- pragma no_determinism_warning(release_thread/7).
:- pred release_thread(all_threads_output::in, int::in, int::in, barrier::in,
    mvar(thread_barrier_test.state)::in, io::di, io::uo) is cc_multi.

release_thread(AllOutput, Thread, AbortAt, Barrier, StateMvar, !IO) :-
    init_thread_output(AllOutput, Thread, Output, !IO),
    t_write_string(Output, "thread starting", !IO),
    N = 5 + Thread * 5,
    t_write_string(Output, format("fib(%d) = %s",
        [i(N), s(integer.to_string(fib(integer(N))))]), !IO),
    ( Thread = AbortAt ->
        t_write_string(Output, "releasing barrier", !IO),
        mvar.take(StateMvar, _, !IO),
        barrier.release(Barrier, !IO),
        mvar.put(StateMvar, state_after_release, !IO),
        t_write_string(Output, "released.", !IO)
    ;
        % There is no guarantee whether the AbortAt thread will finish its
        % computation and release the barrier before or after the current
        % thread reaches this point. Logging the state here may lead to
        % spurious test failures -- change it if necessary.
        log_with_state(Output, StateMvar, "waiting", !IO),
        barrier.wait(Barrier, !IO),
        log_with_state(Output, StateMvar, "done waiting", !IO)
    ),
    close_thread_output(Output, !IO).

:- pred log_with_state(thread_output::in,
    mvar(thread_barrier_test.state)::in, string::in, io::di, io::uo) is det.

log_with_state(Output, StateMvar, String, !IO) :-
    mvar.take(StateMvar, State, !IO),
    (
        State = state_before_release,
        StateStr = "before release"
    ;
        State = state_after_release,
        StateStr = "after release"
    ),
    Message = format("%s:\t%s", [s(String), s(StateStr)]),
    t_write_string(Output, Message, !IO),
    mvar.put(StateMvar, State, !IO).

main(!IO) :-
    ( thread.can_spawn ->
        io.write_string("Test spawn and wait\n", !IO),
        test_spawn_and_wait(5, !IO),
        io.write_string("\nTest release\n", !IO),
        test_release(3, 5, !IO)
    ;
        unexpected($file, $pred, $grade ++ " does not support thread spawning")
    ).

%---------------------------------------------------------------------------%
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%---------------------------------------------------------------------------%
