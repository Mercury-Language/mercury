%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module linkage_test.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module linkage_test2.

main(!IO) :-
    io.print(f : int, !IO),
    io.nl(!IO).
