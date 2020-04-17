%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module closeable_channel_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module thread.
:- import_module thread.closeable_channel.
:- import_module thread.semaphore.

%---------------------------------------------------------------------------%

main(!IO) :-
    ( if can_spawn then
        run_test(!IO)
    else
        io.write_string("spawn/3 not supported in this grade\n", !IO)
    ).

:- pred run_test(io::di, io::uo) is cc_multi.

run_test(!IO) :-
    % Output from worker threads is disabled by default as it is
    % non-deterministic.
    io.get_environment_var("VERBOSE", Verbose, !IO),
    (
        Verbose = yes(_),
        LogA = log_on("huey"),
        LogB = log_on("dewey"),
        LogC = log_on("louie")
    ;
        Verbose = no,
        LogA = log_off,
        LogB = log_off,
        LogC = log_off
    ),

    closeable_channel.init(InCh, !IO),
    closeable_channel.init(OutCh, !IO),

    semaphore.init(DoneA, !IO),
    semaphore.init(DoneB, !IO),
    semaphore.init(DoneC, !IO),
    DoneSems = [DoneA, DoneB, DoneC],

    Log = log_on("main"),
    log(Log, "starting worker threads", !IO),

    thread.spawn(thread_proc(LogA, InCh, OutCh, DoneA), !IO),
    thread.spawn(thread_proc(LogB, InCh, OutCh, DoneB), !IO),
    thread.spawn(thread_proc(LogC, InCh, OutCh, DoneC), !IO),

    % Test try_take from open and empty channel.
    trace_try_take(Log, OutCh, _, !IO),

    % Write to open channel.
    Inputs = 1 .. 9,
    list.foldl(trace_put(Log, InCh), Inputs, !IO),

    % Close open channel.
    trace_close(Log, InCh, !IO),

    % Wait for threads to exit.
    log(Log, "waiting for worker threads to exit", !IO),
    list.foldl(semaphore.wait, DoneSems, !IO),
    log(Log, "workers exited", !IO),

    % Write to closed channel.
    trace_put(Log, InCh, 999, !IO),

    % Close output channel.
    trace_close(Log, OutCh, !IO),

    % Read from closed channel.
    channel_to_list(OutCh, OutputsA0, !IO),
    sort(OutputsA0, OutputsA),

    % Closing a channel again has no effect.
    trace_close(Log, OutCh, !IO),

    % Read from empty closed channel does not block.
    channel_to_list(OutCh, OutputsB, !IO),

    io.write_string("outputs (a) = ", !IO),
    io.write(OutputsA, !IO),
    io.nl(!IO),
    io.write_string("outputs (b) = ", !IO),
    io.write(OutputsB, !IO),
    io.nl(!IO).

:- pred thread_proc(log::in, closeable_channel(int)::in,
    closeable_channel(int)::in, semaphore::in, io::di, io::uo) is cc_multi.

thread_proc(Log, InCh, OutCh, DoneSem, !IO) :-
    log(Log, "thread start", !IO),
    thread_loop(Log, InCh, OutCh, !IO),
    log(Log, "thread exit", !IO),
    semaphore.signal(DoneSem, !IO).

:- pred thread_loop(log::in, closeable_channel(int)::in,
    closeable_channel(int)::in, io::di, io::uo) is cc_multi.

thread_loop(Log, InCh, OutCh, !IO) :-
    trace_take(Log, InCh, TakeResult, !IO),
    (
        TakeResult = ok(Input),
        Output = Input * Input,
        trace_put(Log, OutCh, Output, !IO),
        thread_loop(Log, InCh, OutCh, !IO)
    ;
        TakeResult = closed
    ).

:- pred channel_to_list(closeable_channel(int)::in, list(int)::out,
    io::di, io::uo) is det.

channel_to_list(Ch, List, !IO) :-
    closeable_channel.take(Ch, TakeResult, !IO),
    (
        TakeResult = ok(Head),
        channel_to_list(Ch, Tail, !IO),
        List = [Head | Tail]
    ;
        TakeResult = closed,
        List = []
    ).

%---------------------------------------------------------------------------%

:- pred trace_close(log::in, closeable_channel(int)::in, io::di, io::uo)
    is det.

trace_close(Log, Ch, !IO) :-
    log(Log, "closing channel...", !IO),
    closeable_channel.close(Ch, !IO),
    log(Log, "closed channel", !IO).

:- pred trace_put(log::in, closeable_channel(int)::in, int::in,
    io::di, io::uo) is det.

trace_put(Log, Ch, Item, !IO) :-
    logf(Log, "put %d...", [i(Item)], !IO),
    closeable_channel.put(Ch, Item, Success, !IO),
    (
        Success = yes,
        logf(Log, "put %d", [i(Item)], !IO)
    ;
        Success = no,
        logf(Log, "put %d - channel closed", [i(Item)], !IO)
    ).

:- pred trace_take(log::in, closeable_channel(int)::in,
    take_result(int)::out, io::di, io::uo) is det.

trace_take(Log, Ch, TakeResult, !IO) :-
    log(Log, "take...", !IO),
    closeable_channel.take(Ch, TakeResult, !IO),
    (
        TakeResult = ok(Item),
        logf(Log, "take - got %d", [i(Item)], !IO)
    ;
        TakeResult = closed,
        log(Log, "take - channel closed", !IO)
    ).

:- pred trace_try_take(log::in, closeable_channel(int)::in,
    try_take_result(int)::out, io::di, io::uo) is det.

trace_try_take(Log, Ch, TakeResult, !IO) :-
    log(Log, "try_take...", !IO),
    closeable_channel.try_take(Ch, TakeResult, !IO),
    (
        TakeResult = ok(Item),
        logf(Log, "try_take - got %d", [i(Item)], !IO)
    ;
        TakeResult = closed,
        log(Log, "try_take - channel closed", !IO)
    ;
        TakeResult = would_block,
        log(Log, "try_take - would block (possibly empty)", !IO)
    ).

%---------------------------------------------------------------------------%

:- mutable(log_lock, semaphore, semaphore.impure_init(1), ground,
    [untrailed, constant]).

:- type log
    --->    log_on(string)
    ;       log_off.

:- pred log(log::in, string::in, io::di, io::uo) is det.

log(Log, Str, !IO) :-
    (
        Log = log_on(Label),
        write_log(Label, Str, !IO)
    ;
        Log = log_off
    ).

:- pred logf(log::in, string::in, list(poly_type)::in,
    io::di, io::uo) is det.

logf(Log, Format, Args, !IO) :-
    (
        Log = log_on(Label),
        write_log(Label, string.format(Format, Args), !IO)
    ;
        Log = log_off
    ).

:- pred write_log(string::in, string::in, io::di, io::uo) is det.

write_log(Label, Str, !IO) :-
    get_log_lock(Lock),
    semaphore.wait(Lock, !IO),
    io.write_string(Label, !IO),
    io.write_string(": ", !IO),
    io.write_string(Str, !IO),
    io.nl(!IO),
    semaphore.signal(Lock, !IO).
