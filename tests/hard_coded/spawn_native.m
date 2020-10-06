%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module spawn_native.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module thread.

:- pragma foreign_decl("C", local, "
    #include <time.h>
").

%---------------------------------------------------------------------------%

main(!IO) :-
    ( if can_spawn_native then
        init_tl_key(!IO),
        thread.spawn_native(go("a"), _, !IO),
        msleep(100, !IO),
        thread.spawn_native(go("b"), _, !IO),
        msleep(100, !IO),
        thread.spawn_native(go("c"), _, !IO),
        msleep(100, !IO),
        go("d", !IO)
    else
        io.write_string("spawn_native not supported\n", !IO)
    ).

:- pred go(string::in, thread::in, io::di, io::uo) is cc_multi.

go(Id, _, !IO) :-
    go(Id, !IO),
    cc_multi_equal(!IO).

:- pred go(string::in, io::di, io::uo) is det.

go(IdA, !IO) :-
    set_tl(IdA, !IO),
    io.write_string(IdA ++ " start\n", !IO),
    msleep(500, !IO),
    get_tl(IdB, !IO),
    io.write_string(IdB ++ " stop\n", !IO).

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", local, "
#ifdef MR_THREAD_SAFE
    static MercuryThreadKey tl_key;
#endif
").

:- pragma foreign_code("C#", "
    [System.ThreadStatic]
    private static string tl;
").

:- pragma foreign_code("Java", "
    private static ThreadLocal<String> tl = new ThreadLocal<String>();
").

:- pred init_tl_key(io::di, io::uo) is det.

init_tl_key(!IO).

:- pragma foreign_proc("C",
    init_tl_key(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
#ifdef MR_THREAD_SAFE
    pthread_key_create(&tl_key, NULL);
#endif
").

%---------------------------------------------------------------------------%

:- pred set_tl(string::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    set_tl(X::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
#ifdef MR_THREAD_SAFE
    pthread_setspecific(tl_key, X);
#endif
").

:- pragma foreign_proc("C#",
    set_tl(X::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    tl = X;
").

:- pragma foreign_proc("Java",
    set_tl(X::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    tl.set(X);
").

%---------------------------------------------------------------------------%

:- pred get_tl(string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_tl(X::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
#ifdef MR_THREAD_SAFE
    X = pthread_getspecific(tl_key);
#endif
").

:- pragma foreign_proc("C#",
    get_tl(X::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    X = tl;
").

:- pragma foreign_proc("Java",
    get_tl(X::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    X = tl.get();
").

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
