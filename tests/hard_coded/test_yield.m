% Test thread.yield does not crash.

:- module test_yield.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module thread.

main(!IO) :-
    thread.yield(!IO),
    thread.yield(!IO),
    io.write_string("ok\n", !IO).

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
