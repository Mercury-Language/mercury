%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test initialisation of mutables by impure functions.

:- module mutable_init_impure.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module thread.
:- import_module thread.semaphore.

:- mutable(sem1, semaphore, init_sem, ground, [untrailed, attach_to_io_state]).
:- mutable(sem2, semaphore, init_sem, ground, [untrailed, constant]).

:- impure func init_sem = semaphore.

init_sem = Sem :-
    impure Sem = semaphore.impure_init(1).

:- mutable(foo, string, init_foo, ground, [untrailed, constant]).

:- semipure func init_foo = string.

init_foo = X :-
    promise_semipure X = "Testing...".

main(!IO) :-
    get_sem1(Sem1, !IO),
    semaphore.wait(Sem1, !IO),

    get_sem2(Sem2),
    semaphore.wait(Sem2, !IO),

    get_foo(Foo),
    io.write_string(Foo, !IO),
    io.nl(!IO),

    io.write_string("Success.\n", !IO).
