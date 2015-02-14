%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test thread.yield does not crash.

:- module test_yield.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module thread.

main(!IO) :-
    thread.yield(!IO),
    thread.yield(!IO),
    io.write_string("ok\n", !IO).
