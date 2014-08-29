%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module tabled_typeclass.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- type status
    --->    status_success
    ;       status_no_memory
    ;       status_null_pointer.

:- typeclass surface(S) where [].

:- instance surface(status) where [].

%---------------------------------------------------------------------------%

main(!IO) :-
    test_tc(status_success, !IO),
    test_notc(status_success, !IO),
    test_tc(status_no_memory, !IO),
    test_notc(status_no_memory, !IO),
    test_tc(status_null_pointer, !IO),
    test_notc(status_null_pointer, !IO).

:- pred test_tc(status::in, io::di, io::uo) is det.

test_tc(Status, !IO) :-
    tc_action(Status, N, !IO),
    io.write_int(N, !IO),
    io.nl(!IO).

:- pred test_notc(status::in, io::di, io::uo) is det.

test_notc(Status, !IO) :-
    notc_action(Status, N, !IO),
    io.write_int(N, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C",
"
static int counter = 0;
").

:- pred tc_action(S::in, int::out, io::di, io::uo) is det <= surface(S).

:- pragma foreign_proc("C",
    tc_action(Surface::in, Counter::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    /* Surface */
    Counter = ++counter;
    IO = IO0;
").

:- pred notc_action(S::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    notc_action(Surface::in, Counter::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    /* Surface */
    Counter = ++counter;
    IO = IO0;
").

%---------------------------------------------------------------------------%
