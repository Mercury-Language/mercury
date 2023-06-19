%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module accessibility_t2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module accessibility_t2_helper_1.sub1.
:- import_module accessibility_t2_helper_1.

main(!IO) :-
    io.write_string("Hello.\n", !IO).
