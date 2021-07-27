%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ho_unique_error.

:- interface.

:- import_module io.

:- pred call_ho(io::di, io::uo) is multi.

:- implementation.

call_ho(!IO) :-
    ( call(io__write_string, "First\n", !IO)
    ; call(io__write_string, "Second\n", !IO)
    ).
