%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module sub_c.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module sub_c_helper_1.sub1.

main(!IO) :-
    io.write_string("Hello.\n", !IO).
