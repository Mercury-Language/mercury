% Test semaphore predicates don't crash.

:- module test_semaphore.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module thread.
:- import_module thread.semaphore.

main(!IO) :-
    semaphore.new(S, !IO),
    semaphore.signal(S, !IO),
    semaphore.signal(S, !IO),
    semaphore.wait(S, !IO),
    semaphore.wait(S, !IO),
    semaphore.signal(S, !IO),
    io.write_string("ok\n", !IO).

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
