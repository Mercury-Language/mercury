%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module time_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module maybe.
:- import_module time.
:- import_module float.

main(!IO) :-
    time(Time, !IO),
    localtime(Time, LocalTM, !IO),
    mktime(LocalTM, MkTime, !IO),
    Diff = difftime(Time, MkTime),
    ( if Diff >= 0.0, Diff < 1.0 then
        io.write_string("mktime succeeded\n", !IO)
    else
        io.write_string("mktime failed\n", !IO)
    ),

    % Sunday 2001-01-07 03:02:01
    TM = tm(101, 0, 7, 3, 2, 1, 6, 0, no),
    io.write_string(asctime(TM), !IO),
    io.nl(!IO).
