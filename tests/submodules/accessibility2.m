%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module accessibility2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module sub2_a.sub1.
:- import_module sub2_a.

main(!IO) :-
    io.write_string("Hello.\n", !IO).
