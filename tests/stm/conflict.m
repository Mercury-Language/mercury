%---------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
% Contains a predicate that reads an STM var then holds the transaction open
% while it waits, and another predicate that opens a transaction, waits, and
% then writes to an STM var. Used to ensure that a transaction reads an STM var,
% then a parallel transaction writes to the same var and commits before the read
% transaction can commit.
%---------------------------------------------------------------------------%

:- module conflict.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module thread.
:- import_module stm_builtin.
:- import_module list.
:- import_module int.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    new_stm_var(0, TVar, !IO),
    thread.spawn(read_and_wait(TVar, 2), !IO),
    thread.spawn(wait_and_write(TVar, 1, 28), !IO).

%---------------------------------------------------------------------------%

:- pred read_and_wait(stm_var(int)::in, int::in, io::di, io::uo) is cc_multi.

:- pragma promise_pure(read_and_wait/4).
read_and_wait(TVar, Delay, IO0, IO) :-
    atomic [outer(IO0, IO1), inner(STM0, STM)] (
        trace [io(!IO)] (
            print("entering read transaction\n", !IO)
        ),
        read_stm_var(TVar, X, STM0, STM),
        trace [io(!IO)] (
            format("value read was: %d\n", [i(X)], !IO)
        ),
        impure sleep(Delay)
    ),
    format("read transaction committed: X=%d\n", [i(X)], IO1, IO).

:- pred wait_and_write(stm_var(int)::in, int::in, int::in, io::di, io::uo)
    is cc_multi.

:- pragma promise_pure(wait_and_write/5).
wait_and_write(TVar, Delay, X, IO0, IO) :-
    atomic [outer(IO0, IO1), inner(STM0, STM)] (
        impure sleep(Delay),
        trace [io(!IO)] (
            print("entering write transaction\n", !IO)
        ),
        write_stm_var(TVar, X, STM0, STM),
        trace [io(!IO)] (
            format("wrote value: %d\n", [i(X)], !IO)
        )
    ),
    print("write transaction committed\n", IO1, IO).

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C",
"
    #include <unistd.h>
").

:- impure pred sleep(int::in) is det.

:- pred sleep(int::in, io::di, io::uo) is det.

:- pragma promise_pure(sleep/3).
sleep(Seconds, !IO) :-
    impure sleep(Seconds).

:- pragma foreign_proc("C",
    sleep(Seconds::in),
    [will_not_call_mercury, thread_safe],
"
    sleep(Seconds);
").
