%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module spawn_native_joinable.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module thread.

:- pragma foreign_decl("C", local, "
    #include <time.h>
").

%---------------------------------------------------------------------------%

main(!IO) :-
    ( if can_spawn_native then
        Options = init_thread_options,
        spawn_native_joinable(thread_proc("a"), Options, SpawnResA, !IO),
        msleep(100, !IO),
        spawn_native_joinable(thread_proc("b"), Options, SpawnResB, !IO),
        (
            SpawnResA = ok(ThreadA),
            (
                SpawnResB = ok(ThreadB),
                join_thread(ThreadA, JoinResA, !IO),
                join_thread(ThreadB, JoinResB, !IO),

                io.write_string("a join result: ", !IO),
                io.print_line(JoinResA, !IO),

                io.write_string("b join result: ", !IO),
                io.print_line(JoinResB, !IO)
            ;
                SpawnResB = error(ErrorB),
                io.print_line(ErrorB, !IO),
                join_thread(ThreadA, _JoinResA, !IO)
            )
        ;
            SpawnResA = error(ErrorA),
            io.print_line(ErrorA, !IO)
        )
    else
        io.write_string("spawn_native_joinable not supported\n", !IO)
    ).

:- type thread_output == {string, int}.

:- pred thread_proc(string::in, joinable_thread(thread_output)::in,
    thread_output::out, io::di, io::uo) is cc_multi.

thread_proc(Id, _Thread, Output, !IO) :-
    io.write_string(Id ++ " start\n", !IO),
    msleep(500, !IO),
    cc_multi_equal({Id, 1234}, Output),
    io.write_string(Id ++ " stop\n", !IO).

%---------------------------------------------------------------------------%

:- pred msleep(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    msleep(Msecs::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef MR_WIN32
    Sleep(Msecs);
#else
{
    struct timespec req;
    int err;

    req.tv_sec = 0;
    req.tv_nsec = Msecs * 1000000;
    do {
        err = nanosleep(&req, &req);
    } while (err == -1 && errno == EINTR);
}
#endif
").

:- pragma foreign_proc("C#",
    msleep(Msecs::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    System.Threading.Thread.Sleep(Msecs);
").

:- pragma foreign_proc("Java",
    msleep(Msecs::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    try {
        Thread.sleep(Msecs);
    } catch (InterruptedException e) {
    }
").
