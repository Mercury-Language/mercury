%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module time.subtime.

:- interface.
:- import_module io.

:- pred submain(io::di, io::uo) is cc_multi.

:- implementation.

submain(!IO) :-
    io.write_string("Hello, pellucidar!\n", !IO).
